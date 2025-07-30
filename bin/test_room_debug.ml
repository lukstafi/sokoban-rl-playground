open Sokoban_rl_playground

let () =
  Random.self_init ();
  Printf.printf "Testing room generation with debugging:\n\n";
  
  (* Test simple 5x5 room *)
  let state = Sokoban.make_state 5 5 in
  
  (* Add walls *)
  for x = 0 to 4 do
    Sokoban.set_cell state (x, 0) Wall;
    Sokoban.set_cell state (x, 4) Wall;
  done;
  for y = 0 to 4 do
    Sokoban.set_cell state (0, y) Wall;
    Sokoban.set_cell state (4, y) Wall;
  done;
  
  Printf.printf "Empty room:\n";
  Visualization.print_state state;
  
  (* Check corners *)
  Printf.printf "\nCorner checks:\n";
  for x = 1 to 3 do
    for y = 1 to 3 do
      Printf.printf "(%d,%d) is corner: %b\n" x y (Sokoban.is_corner (x, y) state)
    done
  done;
  
  (* Test a simple configuration *)
  Printf.printf "\nTesting simple configuration:\n";
  Sokoban.set_cell state (1, 1) Player;
  Sokoban.set_cell state (2, 2) Box;
  Sokoban.set_cell state (3, 3) Target;
  let state = { state with player_pos = (1, 1) } in
  
  Visualization.print_state state;
  Printf.printf "Is potentially solvable: %b\n" (Sokoban.is_potentially_solvable state);
  Printf.printf "Has deadlock: %b\n" (Sokoban.has_deadlock state);
  Printf.printf "Box deadlocked at (2,2): %b\n" (Sokoban.is_box_deadlocked (2, 2) state)