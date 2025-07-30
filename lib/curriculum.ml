(** Curriculum learning module for generating Sokoban levels of increasing difficulty *)

open Sokoban

type difficulty_level = 
  | Corridor of int  (* length *)
  | Room of int * int  (* width, height *)
  | MultiBox of int  (* number of boxes *)
  | Complex

type curriculum_config = {
  corridor_lengths: int list;
  room_sizes: (int * int) list;
  box_counts: int list;
}

let default_curriculum = {
  corridor_lengths = [3; 4; 5; 6; 7];
  room_sizes = [(4, 4); (5, 5); (6, 5); (6, 6); (7, 7)];
  box_counts = [1; 2; 2; 3; 3];
}

(** Generate a simple corridor level *)
let generate_corridor_level length =
  let width = length + 2 in
  let level = String.concat "\n" [
    String.make width '#';
    "#" ^ String.make (length - 2) ' ' ^ "@.#";
    String.make width '#';
  ] in
  
  let state = parse_level level in
  let box_pos = (length - 1, 1) in
  set_cell state box_pos Box;
  state

(** Generate a room with walls *)
let generate_room_level width height =
  let state = make_state width height in
  
  for x = 0 to width - 1 do
    state.grid.(0).(x) <- Wall;
    state.grid.(height - 1).(x) <- Wall
  done;
  
  for y = 0 to height - 1 do
    state.grid.(y).(0) <- Wall;
    state.grid.(y).(width - 1) <- Wall
  done;
  
  let player_x = 1 + Random.int (width - 2) in
  let player_y = 1 + Random.int (height - 2) in
  state.grid.(player_y).(player_x) <- Player;
  let player_pos = (player_x, player_y) in
  
  let rec place_box_and_target () =
    let box_x = 1 + Random.int (width - 2) in
    let box_y = 1 + Random.int (height - 2) in
    let target_x = 1 + Random.int (width - 2) in
    let target_y = 1 + Random.int (height - 2) in
    
    if (box_x, box_y) <> player_pos && (target_x, target_y) <> player_pos &&
       (box_x, box_y) <> (target_x, target_y) then begin
      state.grid.(box_y).(box_x) <- Box;
      state.grid.(target_y).(target_x) <- Target
    end else
      place_box_and_target ()
  in
  
  place_box_and_target ();
  { state with player_pos }

(** Generate a level with multiple boxes *)
let generate_multibox_level num_boxes =
  let size = max 5 (num_boxes + 3) in
  let state = make_state size size in
  
  for x = 0 to size - 1 do
    state.grid.(0).(x) <- Wall;
    state.grid.(size - 1).(x) <- Wall
  done;
  
  for y = 0 to size - 1 do
    state.grid.(y).(0) <- Wall;
    state.grid.(y).(size - 1) <- Wall
  done;
  
  let used_positions = ref [] in
  
  let rec find_free_position () =
    let x = 1 + Random.int (size - 2) in
    let y = 1 + Random.int (size - 2) in
    if List.mem (x, y) !used_positions then
      find_free_position ()
    else begin
      used_positions := (x, y) :: !used_positions;
      (x, y)
    end
  in
  
  let player_pos = find_free_position () in
  let (px, py) = player_pos in
  state.grid.(py).(px) <- Player;
  
  for _ = 1 to num_boxes do
    let (bx, by) = find_free_position () in
    state.grid.(by).(bx) <- Box;
    let (tx, ty) = find_free_position () in
    state.grid.(ty).(tx) <- Target
  done;
  
  { state with player_pos }

(** Generate a complex predefined level *)
let generate_complex_level () =
  let levels = [
    "########\n\
     #......#\n\
     #.####.#\n\
     #.#  #.#\n\
     #.# $#.#\n\
     #.#$ #.#\n\
     #.#@ #.#\n\
     ########";
    
    "#######\n\
     #.....#\n\
     #.###.#\n\
     #...#.#\n\
     #$#$#.#\n\
     #  @  #\n\
     #######";
    
    "  #####\n\
     ###   #\n\
     #.@$  #\n\
     ### $.#\n\
     #.##$ #\n\
     # # . ##\n\
     #$ *$$.#\n\
     #   .  #\n\
     ########";
  ] in
  parse_level (List.nth levels (Random.int (List.length levels)))

(** Generate next level in curriculum *)
let generate_level = function
  | Corridor length -> generate_corridor_level length
  | Room (width, height) -> generate_room_level width height
  | MultiBox count -> generate_multibox_level count
  | Complex -> generate_complex_level ()

(** Curriculum scheduler *)
type curriculum_scheduler = {
  mutable stage: int;
  mutable substage: int;
  config: curriculum_config;
}

let create_scheduler ?(config=default_curriculum) () = {
  stage = 0;
  substage = 0;
  config;
}

let get_current_difficulty scheduler =
  match scheduler.stage with
  | 0 -> Corridor (List.nth scheduler.config.corridor_lengths scheduler.substage)
  | 1 -> 
    let (w, h) = List.nth scheduler.config.room_sizes scheduler.substage in
    Room (w, h)
  | 2 -> MultiBox (List.nth scheduler.config.box_counts scheduler.substage)
  | _ -> Complex

let advance_curriculum scheduler success_rate threshold =
  if success_rate >= threshold then begin
    scheduler.substage <- scheduler.substage + 1;
    
    let max_substage = match scheduler.stage with
      | 0 -> List.length scheduler.config.corridor_lengths
      | 1 -> List.length scheduler.config.room_sizes
      | 2 -> List.length scheduler.config.box_counts
      | _ -> 1
    in
    
    if scheduler.substage >= max_substage then begin
      scheduler.stage <- min 3 (scheduler.stage + 1);
      scheduler.substage <- 0
    end;
    true
  end else
    false

let reset_curriculum scheduler =
  scheduler.stage <- 0;
  scheduler.substage <- 0

let get_curriculum_info scheduler =
  let stage_name = match scheduler.stage with
    | 0 -> "Corridor"
    | 1 -> "Room"
    | 2 -> "Multi-box"
    | _ -> "Complex"
  in
  Printf.sprintf "Stage: %s, Level: %d" stage_name (scheduler.substage + 1)

(** Training with curriculum *)
let train_with_curriculum agent scheduler episodes_per_stage success_threshold =
  let rec train_stage () =
    let difficulty = get_current_difficulty scheduler in
    let level = generate_level difficulty in
    
    Printf.printf "\n%s\n" (get_curriculum_info scheduler);
    Printf.printf "Training on level:\n%s\n\n" (state_to_string level);
    
    let (avg_reward, win_rate) = Tabular_rl.train agent level episodes_per_stage in
    
    Printf.printf "Stage complete: Avg Reward = %.2f, Win Rate = %.2f%%\n" 
      avg_reward (win_rate *. 100.0);
    
    if advance_curriculum scheduler win_rate success_threshold then begin
      if scheduler.stage < 3 then
        train_stage ()
      else
        Printf.printf "\nCurriculum completed!\n"
    end else begin
      Printf.printf "Repeating current stage...\n";
      train_stage ()
    end
  in
  train_stage ()