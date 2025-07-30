open Sokoban_rl_playground

let simple_level = "
#######
#     #
# $.@ #
#     #
#######"

let medium_level = "
########
#......#
#.####.#
#.#  #.#
#.# $#.#
#.#$ #.#
#.#@ #.#
########"

let () =
  Random.self_init ();
  
  Printf.printf "=== Sokoban RL Playground ===\n\n";
  
  Printf.printf "1. Testing basic game mechanics\n";
  Printf.printf "--------------------------------\n";
  let state = Sokoban.parse_level simple_level in
  Visualization.print_state state;
  Printf.printf "\n";
  
  Printf.printf "2. Training tabular Q-learning agent\n";
  Printf.printf "------------------------------------\n";
  let agent = Tabular_rl.create_agent () in
  let (avg_reward, win_rate) = Tabular_rl.train agent state 1000 in
  Printf.printf "\nTraining complete! Average reward: %.2f, Win rate: %.2f%%\n\n" 
    avg_reward (win_rate *. 100.0);
  
  Printf.printf "3. Watching trained agent play\n";
  Printf.printf "------------------------------\n";
  let (won, _history, steps) = Tabular_rl.play_episode agent state ~render:true () in
  Printf.printf "\nEpisode finished: %s in %d steps\n\n" 
    (if won then "Won" else "Lost") steps;
  
  Printf.printf "4. Training with curriculum learning\n";
  Printf.printf "-----------------------------------\n";
  let curriculum_agent = Tabular_rl.create_agent () in
  let scheduler = Curriculum.create_scheduler () in
  Curriculum.train_with_curriculum curriculum_agent scheduler 500 0.8;
  
  Printf.printf "\n5. Testing on harder level with curriculum-trained agent\n";
  Printf.printf "-------------------------------------------------------\n";
  let hard_state = Sokoban.parse_level medium_level in
  let (eval_win_rate, eval_avg_steps) = 
    Tabular_rl.evaluate curriculum_agent hard_state 100 in
  Printf.printf "Evaluation on hard level: Win rate = %.2f%%, Avg steps = %.1f\n"
    (eval_win_rate *. 100.0) eval_avg_steps;
  
  Printf.printf "\n6. REINFORCE scaffolding demo (not functional)\n";
  Printf.printf "---------------------------------------------\n";
  let reinforce_agent = 
    Reinforce.create_reinforce_agent ~width:7 ~height:5 ~use_baseline:true () in
  Printf.printf "REINFORCE agent created with policy network: ";
  Printf.printf "input=%d, hidden=%d, output=%d\n"
    reinforce_agent.policy_network.input_size
    reinforce_agent.policy_network.hidden_size
    reinforce_agent.policy_network.output_size;
  Printf.printf "Note: REINFORCE requires a deep learning framework to be functional\n"
