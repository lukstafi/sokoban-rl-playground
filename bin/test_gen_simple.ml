open Sokoban_rl_playground

let generate_simple_room width height =
  let state = Sokoban.make_state width height in
  
  (* Add walls *)
  for x = 0 to width - 1 do
    Sokoban.set_cell state (x, 0) Wall;
    Sokoban.set_cell state (x, height - 1) Wall
  done;
  for y = 0 to height - 1 do
    Sokoban.set_cell state (0, y) Wall;
    Sokoban.set_cell state (width - 1, y) Wall
  done;
  
  (* Simple strategy: place items in a line to ensure solvability *)
  let mid_y = height / 2 in
  Sokoban.set_cell state (1, mid_y) Player;
  Sokoban.set_cell state (2, mid_y) Box;
  Sokoban.set_cell state (width - 2, mid_y) Target;
  
  { state with player_pos = (1, mid_y) }

let () =
  Printf.printf "Testing simple generation:\n\n";
  
  let state = generate_simple_room 5 5 in
  Visualization.print_state state;
  Printf.printf "Solvable: %b\n\n" (Sokoban.is_potentially_solvable state);
  
  let state = generate_simple_room 7 7 in
  Visualization.print_state state;
  Printf.printf "Solvable: %b\n" (Sokoban.is_potentially_solvable state)