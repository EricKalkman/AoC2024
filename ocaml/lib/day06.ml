let parse_input s =
  let g = Grid.from_string s in
  let start = Grid.first_coord '^' g |> Option.get in
  Grid.set g start '.';
  (start, g)

open Point

let try_move g (c, dir) =
  let c2 = Ints2.move dir c in
  if Grid.in_bounds g c2 then
    if Grid.get g c2 == '#' then Some (c, turn_right dir) else Some (c2, dir)
  else None

type 'a stop_kind = Exit of 'a | Loop of 'a
type visited = bool array

let is_visited g arr ((row, col), dir) =
  let w = Grid.width g in
  arr.((row * (w * 4)) + (col * 4) + int_of_direction dir)

let visit g arr ((row, col), dir) =
  let w = Grid.width g in
  arr.((row * (w * 4)) + (col * 4) + int_of_direction dir) <- true

let all_visited g arr =
  let w = Grid.width g in
  let result = ref [] in
  for i = 0 to (Array.length arr - 1) do
    let row = i / (w * 4) in
    let col = i mod (w * 4) / 4 in
    let d = i mod 4 in
    if arr.(i) then result := ((row, col), direction_of_int d) :: !result
  done;
  !result

let move_until_done g node =
  let set = Array.make (Grid.height g * Grid.width g * 4) false in
  let rec loop node =
    match try_move g node with
    | None -> Exit set
    | Some next_node ->
        if is_visited g set next_node then Loop set
        else (
          visit g set next_node;
          loop next_node)
  in
  loop node

let part_1 fname =
  let start, g = fname |> Util.slurp |> parse_input in
  match move_until_done g (start, N) with
  | Exit set ->
      set |> all_visited g |> List.map Util.fst |> Ints2Set.of_list
      |> Ints2Set.cardinal |> string_of_int
  | _ -> failwith "unreachable"

let part_2 fname =
  let start, g = fname |> Util.slurp |> parse_input in
  let coord_set =
    match move_until_done g (start, N) with
    | Exit set ->
        set |> all_visited g |> List.map Util.fst |> Ints2Set.of_list
        |> Ints2Set.remove start
    | _ -> failwith "unreachable"
  in
  coord_set |> Ints2Set.to_seq
  |> Util.count_p (fun c ->
         Grid.set g c '#';
         let res = move_until_done g (start, N) in
         Grid.set g c '.';
         match res with Loop _ -> true | _ -> false)
  |> string_of_int
