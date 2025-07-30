(** Core Sokoban game logic and types *)

type cell = 
  | Empty
  | Wall
  | Box
  | Target
  | BoxOnTarget
  | Player
  | PlayerOnTarget

type position = int * int

type direction = Up | Down | Left | Right

type state = {
  grid: cell array array;
  player_pos: position;
  width: int;
  height: int;
}

type action = Move of direction

type game_status = Playing | Won | Lost

let make_state width height =
  {
    grid = Array.make_matrix height width Empty;
    player_pos = (0, 0);
    width;
    height;
  }

let copy_state state =
  {
    grid = Array.map Array.copy state.grid;
    player_pos = state.player_pos;
    width = state.width;
    height = state.height;
  }

let get_cell state (x, y) =
  if x >= 0 && x < state.width && y >= 0 && y < state.height then
    state.grid.(y).(x)
  else
    Wall

let set_cell state (x, y) cell =
  if x >= 0 && x < state.width && y >= 0 && y < state.height then
    state.grid.(y).(x) <- cell

let move_position (x, y) = function
  | Up -> (x, y - 1)
  | Down -> (x, y + 1)
  | Left -> (x - 1, y)
  | Right -> (x + 1, y)

let is_box = function
  | Box | BoxOnTarget -> true
  | _ -> false

let is_target = function
  | Target | BoxOnTarget | PlayerOnTarget -> true
  | _ -> false

let is_empty = function
  | Empty | Target -> true
  | _ -> false

let cell_without_player = function
  | Player -> Empty
  | PlayerOnTarget -> Target
  | c -> c

let cell_with_player = function
  | Empty -> Player
  | Target -> PlayerOnTarget
  | c -> c

let cell_without_box = function
  | Box -> Empty
  | BoxOnTarget -> Target
  | c -> c

let cell_with_box = function
  | Empty -> Box
  | Target -> BoxOnTarget
  | c -> c

let check_win state =
  let has_box = ref false in
  let all_on_target = ref true in
  for y = 0 to state.height - 1 do
    for x = 0 to state.width - 1 do
      match state.grid.(y).(x) with
      | Box -> has_box := true; all_on_target := false
      | BoxOnTarget -> has_box := true
      | _ -> ()
    done
  done;
  !has_box && !all_on_target

let apply_action state action =
  let new_state = copy_state state in
  let new_pos = move_position state.player_pos (match action with Move d -> d) in
  let next_cell = get_cell state new_pos in
  
  match next_cell with
  | Wall -> state
  | Empty | Target ->
    set_cell new_state state.player_pos (cell_without_player (get_cell state state.player_pos));
    set_cell new_state new_pos (cell_with_player next_cell);
    { new_state with player_pos = new_pos }
  | Box | BoxOnTarget ->
    let box_new_pos = move_position new_pos (match action with Move d -> d) in
    let box_next_cell = get_cell state box_new_pos in
    if is_empty box_next_cell then
      begin
        set_cell new_state state.player_pos (cell_without_player (get_cell state state.player_pos));
        set_cell new_state new_pos (cell_with_player (cell_without_box next_cell));
        set_cell new_state box_new_pos (cell_with_box box_next_cell);
        { new_state with player_pos = new_pos }
      end
    else
      state
  | _ -> state

let is_valid_action state action =
  apply_action state action != state

let get_valid_actions state =
  [Move Up; Move Down; Move Left; Move Right]
  |> List.filter (is_valid_action state)

let state_to_string state =
  let buffer = Buffer.create ((state.width + 1) * state.height) in
  for y = 0 to state.height - 1 do
    for x = 0 to state.width - 1 do
      let c = match state.grid.(y).(x) with
        | Empty -> ' '
        | Wall -> '#'
        | Box -> '$'
        | Target -> '.'
        | BoxOnTarget -> '*'
        | Player -> '@'
        | PlayerOnTarget -> '+'
      in
      Buffer.add_char buffer c
    done;
    if y < state.height - 1 then Buffer.add_char buffer '\n'
  done;
  Buffer.contents buffer

let parse_level level_string =
  let lines = String.split_on_char '\n' level_string in
  let height = List.length lines in
  let width = List.fold_left (fun acc line -> max acc (String.length line)) 0 lines in
  let state = make_state width height in
  let player_pos = ref (0, 0) in
  
  List.iteri (fun y line ->
    String.iteri (fun x c ->
      if x < width then
        let cell = match c with
          | ' ' -> Empty
          | '#' -> Wall
          | '$' -> Box
          | '.' -> Target
          | '*' -> BoxOnTarget
          | '@' -> player_pos := (x, y); Player
          | '+' -> player_pos := (x, y); PlayerOnTarget
          | _ -> Empty
        in
        state.grid.(y).(x) <- cell
    ) line
  ) lines;
  
  { state with player_pos = !player_pos }

(** Deadlock detection for unsolvable states *)

let is_corner pos state =
  let (x, y) = pos in
  let is_wall_or_boundary (px, py) =
    px < 0 || px >= state.width || py < 0 || py >= state.height ||
    state.grid.(py).(px) = Wall
  in
  (* Check if position is in a corner (two adjacent walls) *)
  (is_wall_or_boundary (x-1, y) && is_wall_or_boundary (x, y-1)) ||
  (is_wall_or_boundary (x-1, y) && is_wall_or_boundary (x, y+1)) ||
  (is_wall_or_boundary (x+1, y) && is_wall_or_boundary (x, y-1)) ||
  (is_wall_or_boundary (x+1, y) && is_wall_or_boundary (x, y+1))

let is_box_deadlocked (x, y) state =
  let cell = get_cell state (x, y) in
  if not (is_box cell) then false
  else if cell = BoxOnTarget then false  (* Box is already on target *)
  else
    (* Check if box is in a corner without a target *)
    if is_corner (x, y) state && get_cell state (x, y) <> BoxOnTarget then
      true
    else
      (* Check for deadlock patterns along walls *)
      let is_wall_at (dx, dy) = 
        get_cell state (x + dx, y + dy) = Wall 
      in
      let _is_box_at (dx, dy) =
        is_box (get_cell state (x + dx, y + dy))
      in
      (* Horizontal wall deadlock *)
      if (is_wall_at (0, -1) || is_wall_at (0, 1)) then
        (* Box against horizontal wall - check if it can ever reach a target *)
        let rec check_horizontal_line pos dir =
          let nx = pos + dir in
          if nx < 0 || nx >= state.width then true
          else
            let cell = get_cell state (nx, y) in
            if cell = Wall then true
            else if cell = Target || cell = BoxOnTarget || cell = PlayerOnTarget then false
            else if is_box cell && is_corner (nx, y) state then true
            else check_horizontal_line nx dir
        in
        check_horizontal_line x 1 && check_horizontal_line x (-1)
      (* Vertical wall deadlock *)
      else if (is_wall_at (-1, 0) || is_wall_at (1, 0)) then
        let rec check_vertical_line pos dir =
          let ny = pos + dir in
          if ny < 0 || ny >= state.height then true
          else
            let cell = get_cell state (x, ny) in
            if cell = Wall then true
            else if cell = Target || cell = BoxOnTarget || cell = PlayerOnTarget then false
            else if is_box cell && is_corner (x, ny) state then true
            else check_vertical_line ny dir
        in
        check_vertical_line y 1 && check_vertical_line y (-1)
      else
        false

let has_deadlock state =
  try
    for y = 0 to state.height - 1 do
      for x = 0 to state.width - 1 do
        if is_box_deadlocked (x, y) state then
          raise Exit
      done
    done;
    false
  with Exit -> true

let count_targets state =
  let count = ref 0 in
  for y = 0 to state.height - 1 do
    for x = 0 to state.width - 1 do
      match state.grid.(y).(x) with
      | Target | BoxOnTarget | PlayerOnTarget -> incr count
      | _ -> ()
    done
  done;
  !count

let count_boxes state =
  let count = ref 0 in
  for y = 0 to state.height - 1 do
    for x = 0 to state.width - 1 do
      match state.grid.(y).(x) with
      | Box | BoxOnTarget -> incr count
      | _ -> ()
    done
  done;
  !count

let is_potentially_solvable state =
  (* Basic checks *)
  let num_boxes = count_boxes state in
  let num_targets = count_targets state in
  num_boxes = num_targets && num_boxes > 0 && not (has_deadlock state)