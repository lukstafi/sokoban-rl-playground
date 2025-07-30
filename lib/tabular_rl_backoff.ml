(** Tabular Q-learning with backoff implementation for Sokoban *)

open Sokoban

module StateHash = struct
  type t = string
  let equal = String.equal
  let hash = Hashtbl.hash
end

module QTable = Hashtbl.Make(StateHash)

(** Count table to track how many times we've seen each state *)
module CountTable = Hashtbl.Make(StateHash)

type q_learning_params = {
  learning_rate: float;
  discount_factor: float;
  epsilon_start: float;
  epsilon_end: float;
  epsilon_decay: float;
  max_steps: int;
  window_sizes: int list;  (* List of window sizes, e.g., [3; 5; 7; -1] where -1 means full state *)
}

type q_table_level = {
  q_table: float QTable.t;
  count_table: int CountTable.t;
  window_size: int;  (* -1 for full state *)
}

type backoff_agent = {
  q_levels: q_table_level list;  (* Ordered from smallest to largest window *)
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
  window_sizes = [3; 5; 7; -1];  (* 3x3, 5x5, 7x7 windows, then full state *)
}

(** Extract a window of the state centered on the player *)
let extract_window state window_size =
  if window_size = -1 then
    (* Full state *)
    state
  else
    let (px, py) = state.player_pos in
    let half_window = window_size / 2 in
    let window_state = make_state window_size window_size in
    
    (* Fill window with walls by default (for out-of-bounds) *)
    for y = 0 to window_size - 1 do
      for x = 0 to window_size - 1 do
        window_state.grid.(y).(x) <- Wall
      done
    done;
    
    (* Copy visible portion of the state *)
    for dy = -half_window to half_window do
      for dx = -half_window to half_window do
        let world_x = px + dx in
        let world_y = py + dy in
        let window_x = dx + half_window in
        let window_y = dy + half_window in
        
        if world_x >= 0 && world_x < state.width && 
           world_y >= 0 && world_y < state.height then
          window_state.grid.(window_y).(window_x) <- state.grid.(world_y).(world_x)
        (* else it remains Wall *)
      done
    done;
    
    (* Player is always at center of window *)
    { window_state with player_pos = (half_window, half_window) }

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

(** Get Q-value using backoff strategy *)
let get_q_value_backoff agent state action =
  (* Try each level from most specific (largest window) to least specific *)
  let rec try_levels = function
    | [] -> 0.0  (* Default Q-value *)
    | level :: rest ->
      let window_state = extract_window state level.window_size in
      let key = state_to_key window_state ^ "_" ^ string_of_int (action_to_int action) in
      
      (* Check if we've seen this state before *)
      match CountTable.find_opt level.count_table (state_to_key window_state) with
      | Some count when count > 0 ->
        (* We've seen this state, use its Q-value *)
        (try QTable.find level.q_table key with Not_found -> 0.0)
      | _ ->
        (* Haven't seen this state at this level, try next level *)
        try_levels rest
  in
  (* Start with the most specific (largest window/full state) *)
  try_levels (List.rev agent.q_levels)

(** Update Q-value at all levels *)
let update_q_values agent state action next_state reward =
  let old_q = get_q_value_backoff agent state action in
  
  (* Get best next Q-value using backoff *)
  let next_actions = get_valid_actions next_state in
  let max_next_q = 
    if List.length next_actions = 0 then 0.0
    else List.fold_left (fun max_q a -> 
      max max_q (get_q_value_backoff agent next_state a)
    ) neg_infinity next_actions
  in
  
  let new_q = old_q +. agent.params.learning_rate *. 
    (reward +. agent.params.discount_factor *. max_next_q -. old_q) in
  
  (* Update all levels *)
  List.iter (fun level ->
    let window_state = extract_window state level.window_size in
    let window_next_state = extract_window next_state level.window_size in
    let key = state_to_key window_state ^ "_" ^ string_of_int (action_to_int action) in
    
    (* Update Q-value *)
    QTable.replace level.q_table key new_q;
    
    (* Update counts *)
    let state_key = state_to_key window_state in
    let count = try CountTable.find level.count_table state_key with Not_found -> 0 in
    CountTable.replace level.count_table state_key (count + 1);
    
    (* Also update count for next state *)
    let next_state_key = state_to_key window_next_state in
    let next_count = try CountTable.find level.count_table next_state_key with Not_found -> 0 in
    CountTable.replace level.count_table next_state_key (next_count + 1)
  ) agent.q_levels

(** Get best action using backoff strategy *)
let get_best_action_backoff agent state =
  let actions = get_valid_actions state in
  match actions with
  | [] -> None
  | _ ->
    let best_action = List.fold_left (fun (best_a, best_v) action ->
      let q_val = get_q_value_backoff agent state action in
      if q_val > best_v then (action, q_val) else (best_a, best_v)
    ) (List.hd actions, get_q_value_backoff agent state (List.hd actions)) (List.tl actions) in
    Some (fst best_action)

let epsilon_greedy_action agent state =
  let actions = get_valid_actions state in
  match actions with
  | [] -> None
  | _ ->
    if Random.float 1.0 < agent.epsilon then
      Some (List.nth actions (Random.int (List.length actions)))
    else
      get_best_action_backoff agent state

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
        update_q_values agent state action next_state reward;
        
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
      Printf.printf "Backoff Episode %d: Avg Reward = %.2f, Win Rate = %.2f%%, Epsilon = %.3f\n"
        episode (!total_rewards /. float_of_int episode) 
        (float_of_int !wins /. float_of_int episode *. 100.0)
        agent.epsilon;
      flush stdout
  done;
  
  (!total_rewards /. float_of_int episodes, 
   float_of_int !wins /. float_of_int episodes)

let create_agent ?(params=default_params) () = 
  let q_levels = List.map (fun window_size ->
    {
      q_table = QTable.create 10000;
      count_table = CountTable.create 10000;
      window_size;
    }
  ) params.window_sizes in
  {
    q_levels;
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
      match get_best_action_backoff agent state with
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

(** Debug function to show what the agent sees at different window levels *)
let debug_windows agent state =
  Printf.printf "=== Window views for current state ===\n";
  List.iter (fun level ->
    let window_state = extract_window state level.window_size in
    Printf.printf "\nWindow size %d:\n" level.window_size;
    Visualization.print_state window_state;
    let state_key = state_to_key window_state in
    let count = try CountTable.find level.count_table state_key with Not_found -> 0 in
    Printf.printf "Seen %d times\n" count
  ) agent.q_levels