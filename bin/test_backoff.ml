open Sokoban_rl_playground

let test_window_extraction () =
  Printf.printf "=== Testing Window Extraction ===\n\n";
  
  (* Create a simple test state *)
  let state = Sokoban.parse_level "
#######
#     #
# $.@ #
#     #
#######" in
  
  Printf.printf "Original state:\n";
  Visualization.print_state state;
  Printf.printf "\n";
  
  (* Test different window sizes *)
  let window_sizes = [3; 5; 7] in
  List.iter (fun size ->
    Printf.printf "Window size %dx%d:\n" size size;
    let window = Tabular_rl_backoff.extract_window state size in
    Visualization.print_state window;
    Printf.printf "\n"
  ) window_sizes

let compare_agents () =
  Printf.printf "\n=== Comparing Standard vs Backoff Q-Learning ===\n\n";
  
  (* Test on a simple level *)
  let test_level = Sokoban.parse_level "
#######
#     #
# $.@ #
#     #
#######" in
  
  (* Train standard agent *)
  Printf.printf "Training standard Q-learning agent:\n";
  let standard_agent = Tabular_rl.create_agent () in
  let (std_reward, std_win_rate) = Tabular_rl.train standard_agent test_level 500 in
  Printf.printf "Final: Avg Reward = %.2f, Win Rate = %.2f%%\n\n" 
    std_reward (std_win_rate *. 100.0);
  
  (* Train backoff agent *)
  Printf.printf "Training backoff Q-learning agent:\n";
  let backoff_agent = Tabular_rl_backoff.create_agent () in
  let (bo_reward, bo_win_rate) = Tabular_rl_backoff.train backoff_agent test_level 500 in
  Printf.printf "Final: Avg Reward = %.2f, Win Rate = %.2f%%\n\n" 
    bo_reward (bo_win_rate *. 100.0);
  
  (* Test generalization on a different level *)
  Printf.printf "Testing generalization on new level:\n";
  let new_level = Sokoban.parse_level "
########
#      #
#  $.@ #
#      #
########" in
  
  Visualization.print_state new_level;
  
  (* Evaluate both agents *)
  let (std_eval_win, std_eval_steps) = 
    Tabular_rl.evaluate standard_agent new_level 100 in
  let (bo_eval_win, bo_eval_steps) = 
    Tabular_rl_backoff.evaluate backoff_agent new_level 100 in
  
  Printf.printf "\nEvaluation results:\n";
  Printf.printf "Standard: Win rate = %.1f%%, Avg steps = %.1f\n"
    (std_eval_win *. 100.0) std_eval_steps;
  Printf.printf "Backoff:  Win rate = %.1f%%, Avg steps = %.1f\n"
    (bo_eval_win *. 100.0) bo_eval_steps

let test_curriculum_with_backoff () =
  Printf.printf "\n=== Testing Curriculum Learning with Backoff ===\n\n";
  
  let _scheduler = Curriculum.create_scheduler () in
  let backoff_agent = Tabular_rl_backoff.create_agent 
    ~params:{
      Tabular_rl_backoff.default_params with
      window_sizes = [3; 5; -1];  (* 3x3, 5x5, and full state *)
    } () in
  
  (* Train on first few corridor levels *)
  for i = 1 to 3 do
    let length = 2 + i in
    Printf.printf "Training on corridor length %d:\n" length;
    let level = Curriculum.generate_corridor_level length in
    Visualization.print_state level;
    
    let (reward, win_rate) = Tabular_rl_backoff.train backoff_agent level 200 in
    Printf.printf "Result: Avg Reward = %.2f, Win Rate = %.2f%%\n\n" 
      reward (win_rate *. 100.0)
  done;
  
  (* Test on a room to see generalization *)
  Printf.printf "Testing generalization on room:\n";
  let room = Curriculum.generate_room_level 5 5 in
  Visualization.print_state room;
  
  let (eval_win, eval_steps) = 
    Tabular_rl_backoff.evaluate backoff_agent room 50 in
  Printf.printf "Room evaluation: Win rate = %.1f%%, Avg steps = %.1f\n"
    (eval_win *. 100.0) eval_steps;
  
  (* Debug: show what the agent sees *)
  Printf.printf "\n";
  Tabular_rl_backoff.debug_windows backoff_agent room

let () =
  Random.self_init ();
  test_window_extraction ();
  compare_agents ();
  test_curriculum_with_backoff ()