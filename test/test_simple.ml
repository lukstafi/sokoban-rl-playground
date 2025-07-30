open Sokoban_rl_playground

let () =
  Random.self_init ();
  
  Printf.printf "Testing improved room generation:\n\n";
  
  (* Generate a 5x5 room 5 times to show they're solvable *)
  for i = 1 to 5 do
    Printf.printf "Room %d:\n" i;
    let state = Curriculum.generate_room_level 5 5 in
    Visualization.print_state state;
    Printf.printf "Potentially solvable: %b\n\n" 
      (Sokoban.is_potentially_solvable state)
  done;
  
  Printf.printf "Testing deadlock detection:\n\n";
  
  (* Box in corner without target - deadlock *)
  let deadlocked = Sokoban.parse_level "#####\n#@$ #\n#####" in
  Visualization.print_state deadlocked;
  Printf.printf "Box in corner (deadlock): %b\n\n" (Sokoban.has_deadlock deadlocked);
  
  (* Box in corner with target - not deadlock *)
  let not_deadlocked = Sokoban.parse_level "#####\n#@*.#\n#####" in
  Visualization.print_state not_deadlocked;
  Printf.printf "Box on target in corner (not deadlock): %b\n" (Sokoban.has_deadlock not_deadlocked)