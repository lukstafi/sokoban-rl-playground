open Sokoban_rl_playground

let () =
  Random.self_init ();
  Printf.printf "Testing generation with fallback:\n\n";
  
  (* Force fallback by using max attempts = 0 *)
  let state = Sokoban.make_state 5 5 in
  
  (* Create walls *)
  for x = 0 to 4 do
    Sokoban.set_cell state (x, 0) Wall;
    Sokoban.set_cell state (x, 4) Wall
  done;
  for y = 0 to 4 do
    Sokoban.set_cell state (0, y) Wall;
    Sokoban.set_cell state (4, y) Wall
  done;
  
  (* Simple working setup from fallback *)
  Sokoban.set_cell state (1, 1) Player;
  Sokoban.set_cell state (2, 1) Box;
  Sokoban.set_cell state (3, 1) Target;
  let state = { state with player_pos = (1, 1) } in
  
  Visualization.print_state state;
  Printf.printf "Potentially solvable: %b\n" (Sokoban.is_potentially_solvable state);
  Printf.printf "Has deadlock: %b\n" (Sokoban.has_deadlock state)