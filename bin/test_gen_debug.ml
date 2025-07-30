open Sokoban_rl_playground

let generate_room_debug width height =
  Printf.printf "Generating %dx%d room...\n" width height;
  
  let state = Sokoban.make_state width height in
  
  (* Create walls *)
  for x = 0 to width - 1 do
    Sokoban.set_cell state (x, 0) Wall;
    Sokoban.set_cell state (x, height - 1) Wall
  done;
  for y = 0 to height - 1 do
    Sokoban.set_cell state (0, y) Wall;
    Sokoban.set_cell state (width - 1, y) Wall
  done;
  
  (* Get all inner positions *)
  let all_positions = ref [] in
  for x = 1 to width - 2 do
    for y = 1 to height - 2 do
      all_positions := (x, y) :: !all_positions
    done
  done;
  
  Printf.printf "Total positions: %d\n" (List.length !all_positions);
  
  (* Count non-corner positions *)
  let non_corners = List.filter (fun pos -> not (Sokoban.is_corner pos state)) !all_positions in
  Printf.printf "Non-corner positions: %d\n" (List.length non_corners);
  
  (* Simple placement *)
  match !all_positions with
  | p1 :: p2 :: p3 :: _ ->
    let (px, py) = p1 in
    let (bx, by) = p2 in
    let (tx, ty) = p3 in
    Sokoban.set_cell state (px, py) Player;
    Sokoban.set_cell state (bx, by) Box;
    Sokoban.set_cell state (tx, ty) Target;
    { state with player_pos = (px, py) }
  | _ -> failwith "Not enough positions"

let () =
  let state = generate_room_debug 5 5 in
  Visualization.print_state state;
  Printf.printf "Checking solvability...\n";
  Printf.printf "Count boxes: %d\n" (Sokoban.count_boxes state);
  Printf.printf "Count targets: %d\n" (Sokoban.count_targets state);
  Printf.printf "Checking deadlock...\n";
  Printf.printf "Has deadlock: %b\n" (Sokoban.has_deadlock state);
  Printf.printf "Is potentially solvable: %b\n" (Sokoban.is_potentially_solvable state)