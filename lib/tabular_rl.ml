(** Tabular Q-learning implementation for Sokoban *)

open Sokoban

module StateHash = struct
  type t = string
  let equal = String.equal
  let hash = Hashtbl.hash
end

module QTable = Hashtbl.Make(StateHash)

type q_learning_params = {
  learning_rate: float;
  discount_factor: float;
  epsilon_start: float;
  epsilon_end: float;
  epsilon_decay: float;
  max_steps: int;
}

type tabular_agent = {
  q_table: float QTable.t;
  params: q_learning_params;
  mutable epsilon: float;
  mutable steps: int;
}

let default_params = {
  learning_rate = 0.1;
  discount_factor = 0.95;
  epsilon_start = 1.0;
  epsilon_end = 0.01;
  epsilon_decay = 0.995;
  max_steps = 500;
}

let state_to_key state =
  state_to_string state

let action_to_int = function
  | Move Up -> 0
  | Move Down -> 1
  | Move Left -> 2
  | Move Right -> 3

let int_to_action = function
  | 0 -> Move Up
  | 1 -> Move Down
  | 2 -> Move Left
  | 3 -> Move Right
  | _ -> failwith "Invalid action index"

let get_q_value agent state action =
  let key = state_to_key state ^ "_" ^ string_of_int (action_to_int action) in
  try QTable.find agent.q_table key
  with Not_found -> 0.0

let set_q_value agent state action value =
  let key = state_to_key state ^ "_" ^ string_of_int (action_to_int action) in
  QTable.replace agent.q_table key value

let get_best_action agent state =
  let actions = get_valid_actions state in
  match actions with
  | [] -> None
  | _ ->
    let best_action = List.fold_left (fun (best_a, best_v) action ->
      let q_val = get_q_value agent state action in
      if q_val > best_v then (action, q_val) else (best_a, best_v)
    ) (List.hd actions, get_q_value agent state (List.hd actions)) (List.tl actions) in
    Some (fst best_action)

let epsilon_greedy_action agent state =
  let actions = get_valid_actions state in
  match actions with
  | [] -> None
  | _ ->
    if Random.float 1.0 < agent.epsilon then
      Some (List.nth actions (Random.int (List.length actions)))
    else
      get_best_action agent state

let calculate_reward old_state new_state _action won =
  let step_penalty = -0.01 in
  let box_moved_penalty = -0.02 in
  let win_reward = 10.0 in
  
  let reward = ref step_penalty in
  
  if won then
    reward := !reward +. win_reward
  else begin
    let old_boxes_on_target = ref 0 in
    let new_boxes_on_target = ref 0 in
    
    for y = 0 to old_state.height - 1 do
      for x = 0 to old_state.width - 1 do
        match old_state.grid.(y).(x) with
        | BoxOnTarget -> incr old_boxes_on_target
        | _ -> ()
      done
    done;
    
    for y = 0 to new_state.height - 1 do
      for x = 0 to new_state.width - 1 do
        match new_state.grid.(y).(x) with
        | BoxOnTarget -> incr new_boxes_on_target
        | _ -> ()
      done
    done;
    
    let box_diff = !new_boxes_on_target - !old_boxes_on_target in
    reward := !reward +. (float_of_int box_diff *. 1.0);
    
    let (px, py) = old_state.player_pos in
    let old_cell = old_state.grid.(py).(px) in
    if is_box old_cell then
      reward := !reward +. box_moved_penalty
  end;
  
  !reward

let update_q_value agent state action next_state reward =
  let old_q = get_q_value agent state action in
  let next_actions = get_valid_actions next_state in
  let max_next_q = 
    if List.length next_actions = 0 then 0.0
    else List.fold_left (fun max_q a -> 
      max max_q (get_q_value agent next_state a)
    ) neg_infinity next_actions
  in
  let new_q = old_q +. agent.params.learning_rate *. 
    (reward +. agent.params.discount_factor *. max_next_q -. old_q) in
  set_q_value agent state action new_q

let train_episode agent initial_state =
  let rec loop state steps total_reward =
    if steps >= agent.params.max_steps then
      (false, total_reward, steps)
    else if check_win state then
      (true, total_reward, steps)
    else
      match epsilon_greedy_action agent state with
      | None -> (false, total_reward, steps)
      | Some action ->
        let next_state = apply_action state action in
        let won = check_win next_state in
        let reward = calculate_reward state next_state action won in
        update_q_value agent state action next_state reward;
        
        if won then
          (true, total_reward +. reward, steps + 1)
        else
          loop next_state (steps + 1) (total_reward +. reward)
  in
  let result = loop initial_state 0 0.0 in
  agent.epsilon <- max agent.params.epsilon_end 
    (agent.epsilon *. agent.params.epsilon_decay);
  agent.steps <- agent.steps + 1;
  result

let train agent initial_state episodes =
  let total_rewards = ref 0.0 in
  let wins = ref 0 in
  
  for episode = 1 to episodes do
    let (won, reward, _steps) = train_episode agent initial_state in
    total_rewards := !total_rewards +. reward;
    if won then incr wins;
    
    if episode mod 100 = 0 then
      Printf.printf "Episode %d: Avg Reward = %.2f, Win Rate = %.2f%%, Epsilon = %.3f\n"
        episode (!total_rewards /. float_of_int episode) 
        (float_of_int !wins /. float_of_int episode *. 100.0)
        agent.epsilon;
      flush stdout
  done;
  
  (!total_rewards /. float_of_int episodes, 
   float_of_int !wins /. float_of_int episodes)

let create_agent ?(params=default_params) () = {
  q_table = QTable.create 10000;
  params;
  epsilon = params.epsilon_start;
  steps = 0;
}

let play_episode agent initial_state ?(render=false) () =
  let rec loop state steps history =
    let history' = state :: history in
    
    if render then begin
      Visualization.clear_screen ();
      Visualization.print_state state;
      Unix.sleepf 0.5
    end;
    
    if steps >= agent.params.max_steps then
      (false, List.rev history', steps)
    else if check_win state then
      (true, List.rev history', steps)
    else
      match get_best_action agent state with
      | None -> (false, List.rev history', steps)
      | Some action ->
        let next_state = apply_action state action in
        loop next_state (steps + 1) history'
  in
  loop initial_state 0 []

let evaluate agent initial_state episodes =
  let wins = ref 0 in
  let total_steps = ref 0 in
  
  for _ = 1 to episodes do
    let (won, _, steps) = play_episode agent initial_state () in
    if won then incr wins;
    total_steps := !total_steps + steps
  done;
  
  (float_of_int !wins /. float_of_int episodes,
   float_of_int !total_steps /. float_of_int episodes)