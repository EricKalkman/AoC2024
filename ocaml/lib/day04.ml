let part_1 fname =
  let open Point in
  let g = fname |> Util.slurp |> Grid.from_string in
  let spellers = dirs8 |> List.map (Grid.spells_along g "XMAS") in
  g |> Grid.all_coords_of 'X'
  |> List.map (fun c -> Util.count_p_list (fun s -> s c) spellers)
  |> Util.sum_list |> string_of_int

let part_2 fname =
  let open Point in
  let g = fname |> Util.slurp |> Grid.from_string in
  let spellers =
    [ SE; SW; NE; NW ]
    |> List.map (fun dir c ->
           Grid.spells_along g "MAS" dir Ints2.(c @- step dir))
  in
  g |> Grid.all_coords_of 'A'
  |> Util.count_p_list (fun c -> 2 == Util.count_p_list (fun s -> s c) spellers)
  |> string_of_int
