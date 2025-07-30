open Sokoban_rl_playground

let () =
  Printf.printf "Testing deadlock detection:\n";
  
  (* Create test level *)
  let level = Sokoban.make_state 5 3 in
  
  (* Add walls *)
  for x = 0 to 4 do
    Sokoban.set_cell level (x, 0) Wall;
    Sokoban.set_cell level (x, 2) Wall;
  done;
  for y = 0 to 2 do
    Sokoban.set_cell level (0, y) Wall;
    Sokoban.set_cell level (4, y) Wall;
  done;
  
  (* Place entities *)
  Sokoban.set_cell level (1, 1) Player;
  Sokoban.set_cell level (3, 1) Box;
  
  let level = { level with player_pos = (1, 1) } in
  
  Visualization.print_state level;
  Printf.printf "Box in corner: %b\n" (Sokoban.is_corner (3, 1) level);
  Printf.printf "Has deadlock: %b\n" (Sokoban.has_deadlock level)