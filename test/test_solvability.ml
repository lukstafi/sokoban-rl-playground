open Sokoban_rl_playground

let test_room_generation () =
  Printf.printf "Testing room generation with solvability checks:\n\n";
  
  (* Test different room sizes *)
  let room_sizes = [(5, 5); (6, 5); (6, 6); (7, 6); (7, 7)] in
  
  List.iter (fun (w, h) ->
    Printf.printf "Generating %dx%d room:\n" w h;
    let state = Curriculum.generate_room_level w h in
    Visualization.print_state state;
    
    let solvable = Sokoban.is_potentially_solvable state in
    let has_dl = Sokoban.has_deadlock state in
    let num_boxes = Sokoban.count_boxes state in
    let num_targets = Sokoban.count_targets state in
    
    Printf.printf "Solvable: %b, Has deadlock: %b, Boxes: %d, Targets: %d\n\n" 
      solvable has_dl num_boxes num_targets
  ) room_sizes;
  
  (* Test multibox generation *)
  Printf.printf "Testing multibox generation:\n\n";
  
  for n = 1 to 3 do
    Printf.printf "Generating level with %d boxes:\n" n;
    let state = Curriculum.generate_multibox_level n in
    Visualization.print_state state;
    
    let solvable = Sokoban.is_potentially_solvable state in
    let has_dl = Sokoban.has_deadlock state in
    
    Printf.printf "Solvable: %b, Has deadlock: %b\n\n" solvable has_dl
  done

let test_deadlock_detection () =
  Printf.printf "Testing deadlock detection:\n\n";
  
  (* Create a simple deadlocked state *)
  let deadlocked = "
#####
#@$ #
#####" in
  
  let state = Sokoban.parse_level deadlocked in
  Printf.printf "Deadlocked level (box in corner):\n";
  Visualization.print_state state;
  Printf.printf "Has deadlock: %b\n\n" (Sokoban.has_deadlock state);
  
  (* Create a solvable state *)
  let solvable = "
#####
#@ .#
# $ #
#####" in
  
  let state = Sokoban.parse_level solvable in
  Printf.printf "Solvable level:\n";
  Visualization.print_state state;
  Printf.printf "Has deadlock: %b\n\n" (Sokoban.has_deadlock state)

let () =
  Random.self_init ();
  test_deadlock_detection ();
  test_room_generation ()