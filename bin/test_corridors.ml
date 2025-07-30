open Sokoban_rl_playground

let () =
  Random.self_init ();
  Printf.printf "Testing mixed corridor generation:\n\n";
  
  (* Generate several corridors to show both orientations *)
  for i = 1 to 10 do
    Printf.printf "Corridor %d (length %d):\n" i (3 + i / 3);
    let state = Curriculum.generate_corridor_level (3 + i / 3) in
    Visualization.print_state state;
    Printf.printf "\n"
  done;
  
  Printf.printf "Testing curriculum learning with mixed corridors:\n\n";
  
  (* Train a simple agent on corridors *)
  let agent = Tabular_rl.create_agent () in
  let total_reward = ref 0.0 in
  let wins = ref 0 in
  
  for episode = 1 to 100 do
    (* Generate a random corridor *)
    let length = 3 + Random.int 3 in
    let state = Curriculum.generate_corridor_level length in
    let (won, reward, _) = Tabular_rl.train_episode agent state in
    total_reward := !total_reward +. reward;
    if won then incr wins;
    
    if episode mod 20 = 0 then
      Printf.printf "Episode %d: Win rate = %.1f%%\n" 
        episode (float_of_int !wins /. float_of_int episode *. 100.0)
  done;
  
  Printf.printf "\nFinal win rate: %.1f%%\n" 
    (float_of_int !wins /. 100.0 *. 100.0)