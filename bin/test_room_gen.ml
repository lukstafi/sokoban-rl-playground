open Sokoban_rl_playground

let () =
  Random.self_init ();
  Printf.printf "Testing room generation (5x5):\n\n";
  
  try
    let state = Curriculum.generate_room_level 5 5 in
    Visualization.print_state state;
    Printf.printf "Successfully generated!\n";
    Printf.printf "Potentially solvable: %b\n" (Sokoban.is_potentially_solvable state)
  with Failure msg ->
    Printf.printf "Failed: %s\n" msg