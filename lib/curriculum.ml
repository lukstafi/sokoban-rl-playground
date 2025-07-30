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
  room_sizes = [(5, 5); (6, 5); (6, 6); (7, 6); (7, 7)];
  box_counts = [1; 2; 2; 3; 3];
}

(** Generate a simple corridor level with random orientation *)
let generate_corridor_level length =
  (* Randomly choose horizontal or vertical *)
  let horizontal = Random.bool () in
  
  if horizontal then
    (* Horizontal corridor *)
    let width = length + 2 in
    let height = 3 in
    let state = make_state width height in
    
    (* Create walls *)
    for x = 0 to width - 1 do
      set_cell state (x, 0) Wall;
      set_cell state (x, height - 1) Wall
    done;
    for y = 0 to height - 1 do
      set_cell state (0, y) Wall;
      set_cell state (width - 1, y) Wall
    done;
    
    (* Place entities in a line *)
    set_cell state (1, 1) Player;
    set_cell state (length - 1, 1) Box;
    set_cell state (length, 1) Target;
    
    { state with player_pos = (1, 1) }
  else
    (* Vertical corridor *)
    let width = 3 in
    let height = length + 2 in
    let state = make_state width height in
    
    (* Create walls *)
    for x = 0 to width - 1 do
      set_cell state (x, 0) Wall;
      set_cell state (x, height - 1) Wall
    done;
    for y = 0 to height - 1 do
      set_cell state (0, y) Wall;
      set_cell state (width - 1, y) Wall
    done;
    
    (* Place entities in a vertical line *)
    set_cell state (1, 1) Player;
    set_cell state (1, length - 1) Box;
    set_cell state (1, length) Target;
    
    { state with player_pos = (1, 1) }

(** Generate a room with walls using greedy incremental placement *)
let generate_room_level width height =
  let max_attempts = 50 in
  
  let rec attempt n =
    if n >= max_attempts then begin
      (* Fall back to a simple working configuration *)
      let state = make_state width height in
      
      (* Create walls *)
      for x = 0 to width - 1 do
        state.grid.(0).(x) <- Wall;
        state.grid.(height - 1).(x) <- Wall
      done;
      for y = 0 to height - 1 do
        state.grid.(y).(0) <- Wall;
        state.grid.(y).(width - 1) <- Wall
      done;
      
      (* Simple working setup: player at (1,1), box at (2,1), target at (3,1) *)
      state.grid.(1).(1) <- Player;
      state.grid.(1).(2) <- Box;
      state.grid.(1).(3) <- Target;
      { state with player_pos = (1, 1) }
    end else
      let state = make_state width height in
      
      (* Create walls *)
      for x = 0 to width - 1 do
        state.grid.(0).(x) <- Wall;
        state.grid.(height - 1).(x) <- Wall
      done;
      for y = 0 to height - 1 do
        state.grid.(y).(0) <- Wall;
        state.grid.(y).(width - 1) <- Wall
      done;
      
      (* Get all inner positions - for small rooms, include corners *)
      let all_positions = ref [] in
      let include_corners = width <= 5 || height <= 5 in
      for x = 1 to width - 2 do
        for y = 1 to height - 2 do
          if include_corners || not (Sokoban.is_corner (x, y) state) then
            all_positions := (x, y) :: !all_positions
        done
      done;
      
      (* Shuffle positions *)
      let shuffle lst =
        let arr = Array.of_list lst in
        for i = Array.length arr - 1 downto 1 do
          let j = Random.int (i + 1) in
          let tmp = arr.(i) in
          arr.(i) <- arr.(j);
          arr.(j) <- tmp
        done;
        Array.to_list arr
      in
      
      let positions = shuffle !all_positions in
      
      (* Try to place player, box, and target *)
      match positions with
      | [] -> attempt (n + 1)
      | p1 :: rest_positions ->
        (* Place player first *)
        let (px, py) = p1 in
        state.grid.(py).(px) <- Player;
        
        (* Try different combinations for box and target *)
        let try_placement (bx, by) (tx, ty) =
          (* Don't place box and target in same position *)
          if (bx, by) = (tx, ty) then false
          else begin
            (* Temporarily place box *)
            state.grid.(by).(bx) <- Box;
            
            (* For small rooms, allow corner placements if target is there *)
            let corner_ok = 
              if width <= 5 && height <= 5 then
                (* In small rooms, box in corner is OK if target is also in corner *)
                if Sokoban.is_corner (bx, by) state then
                  Sokoban.is_corner (tx, ty) state
                else
                  true
              else
                (* In larger rooms, avoid corners for boxes *)
                not (Sokoban.is_corner (bx, by) state)
            in
            
            if not corner_ok then begin
              state.grid.(by).(bx) <- Empty;
              false
            end else begin
              (* Place target *)
              state.grid.(ty).(tx) <- Target;
              
              (* Check if configuration is potentially solvable *)
              let test_state = { state with player_pos = (px, py) } in
              if Sokoban.is_potentially_solvable test_state then
                true
              else begin
                (* Revert changes *)
                state.grid.(by).(bx) <- Empty;
                state.grid.(ty).(tx) <- Empty;
                false
              end
            end
          end
        in
        
        (* Try multiple placement combinations *)
        let rec try_combinations = function
          | [] -> None
          | (b, t) :: rest ->
            if try_placement b t then
              Some { state with player_pos = (px, py) }
            else begin
              (* Clean up any leftover state *)
              let (bx, by) = b in
              let (tx, ty) = t in
              state.grid.(by).(bx) <- Empty;
              state.grid.(ty).(tx) <- Empty;
              try_combinations rest
            end
        in
        
        (* Generate different position combinations *)
        let combinations = match rest_positions with
          | p2 :: p3 :: p4 :: _ -> 
            [(p2, p3); (p2, p4); (p3, p2); (p3, p4); (p4, p2); (p4, p3)]
          | p2 :: p3 :: [] ->
            [(p2, p3); (p3, p2)]
          | _ -> []
        in
        
        match try_combinations combinations with
        | Some final_state -> final_state
        | None -> attempt (n + 1)
  in
  attempt 0

(** Generate a level with multiple boxes using greedy placement *)
let generate_multibox_level num_boxes =
  let size = max 5 (num_boxes + 3) in
  let max_attempts = 50 in
  
  let rec attempt n =
    if n >= max_attempts then begin
      (* Fall back to simple line configuration *)
      let state = make_state size size in
      
      (* Create walls *)
      for x = 0 to size - 1 do
        state.grid.(0).(x) <- Wall;
        state.grid.(size - 1).(x) <- Wall
      done;
      for y = 0 to size - 1 do
        state.grid.(y).(0) <- Wall;
        state.grid.(y).(size - 1) <- Wall
      done;
      
      (* Simple line setup *)
      state.grid.(1).(1) <- Player;
      for i = 0 to num_boxes - 1 do
        state.grid.(2).(2 + i) <- Box;
        state.grid.(3).(2 + i) <- Target
      done;
      { state with player_pos = (1, 1) }
    end else
      let state = make_state size size in
      
      (* Create walls *)
      for x = 0 to size - 1 do
        state.grid.(0).(x) <- Wall;
        state.grid.(size - 1).(x) <- Wall
      done;
      for y = 0 to size - 1 do
        state.grid.(y).(0) <- Wall;
        state.grid.(y).(size - 1) <- Wall
      done;
      
      (* Get all inner positions and avoid corners *)
      let valid_positions = ref [] in
      for x = 1 to size - 2 do
        for y = 1 to size - 2 do
          if not (Sokoban.is_corner (x, y) state) then
            valid_positions := (x, y) :: !valid_positions
        done
      done;
      
      if List.length !valid_positions < 1 + 2 * num_boxes then
        attempt (n + 1)
      else
        (* Shuffle positions *)
        let shuffle lst =
          let arr = Array.of_list lst in
          for i = Array.length arr - 1 downto 1 do
            let j = Random.int (i + 1) in
            let tmp = arr.(i) in
            arr.(i) <- arr.(j);
            arr.(j) <- tmp
          done;
          Array.to_list arr
        in
        
        let positions = shuffle !valid_positions in
        
        (* Place player *)
        match positions with
        | [] -> attempt (n + 1)
        | player_pos :: rest ->
          let (px, py) = player_pos in
          state.grid.(py).(px) <- Player;
          
          (* Try to place boxes and targets greedily *)
          let rec place_boxes remaining boxes_to_place =
            if boxes_to_place = 0 then
              Some remaining
            else
              match remaining with
              | [] | [_] -> None  (* Need at least 2 positions per box *)
              | pos1 :: pos2 :: rest ->
                let (x1, y1) = pos1 in
                let (x2, y2) = pos2 in
                
                (* Try pos1 as box, pos2 as target *)
                state.grid.(y1).(x1) <- Box;
                if Sokoban.is_box_deadlocked (x1, y1) state then begin
                  state.grid.(y1).(x1) <- Empty;
                  (* Try pos2 as box, pos1 as target *)
                  state.grid.(y2).(x2) <- Box;
                  if Sokoban.is_box_deadlocked (x2, y2) state then begin
                    state.grid.(y2).(x2) <- Empty;
                    None  (* Both positions fail *)
                  end else begin
                    state.grid.(y1).(x1) <- Target;
                    place_boxes rest (boxes_to_place - 1)
                  end
                end else begin
                  state.grid.(y2).(x2) <- Target;
                  place_boxes rest (boxes_to_place - 1)
                end
          in
          
          match place_boxes rest num_boxes with
          | Some _ ->
            let final_state = { state with player_pos } in
            if Sokoban.is_potentially_solvable final_state then
              final_state
            else
              attempt (n + 1)
          | None -> attempt (n + 1)
  in
  attempt 0

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
    | 0 -> "Corridor (mixed H/V)"
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