(** Simple text-based visualization for Sokoban *)

open Sokoban

let clear_screen () =
  print_string "\027[2J\027[H"

let print_state state =
  print_endline (state_to_string state)

let print_stats episode reward steps =
  Printf.printf "Episode: %d | Reward: %.2f | Steps: %d\n" episode reward steps

let print_action action =
  let action_str = match action with
    | Move Up -> "Up"
    | Move Down -> "Down"
    | Move Left -> "Left"
    | Move Right -> "Right"
  in
  Printf.printf "Action: %s\n" action_str

let print_game_status status =
  match status with
  | Playing -> ()
  | Won -> print_endline "🎉 Level Complete!"
  | Lost -> print_endline "💀 Game Over!"

let animate_game state_history delay =
  List.iter (fun state ->
    clear_screen ();
    print_state state;
    Unix.sleepf delay
  ) state_history

let print_episode_summary episode_num total_reward steps win =
  Printf.printf "Episode %d completed: Reward=%.2f, Steps=%d, %s\n"
    episode_num total_reward steps (if win then "WIN" else "FAIL")

let print_training_summary episodes avg_reward win_rate =
  Printf.printf "\nTraining Summary:\n";
  Printf.printf "Total Episodes: %d\n" episodes;
  Printf.printf "Average Reward: %.2f\n" avg_reward;
  Printf.printf "Win Rate: %.2f%%\n" (win_rate *. 100.)