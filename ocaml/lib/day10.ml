open Ops

let part_1 fname =
  let open Point in
  let g = fname |> Util.slurp |> Grid.from_string in
  let trailheads = Grid.all_coords_of '0' g |> List.map Ints2Set.singleton in
  [ '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ]
  |> List.fold_left
       (fun trails elevation ->
         trails
         |> List.map (fun trail ->
                trail |> Ints2Set.to_seq
                |> Seq.concat_map (Ints2.neighbors4 >> List.to_seq)
                |> Seq.filter (fun c ->
                       Grid.(in_bounds g c && get g c == elevation))
                |> Ints2Set.of_seq))
       trailheads
  |> List.map Ints2Set.cardinal |> Util.sum_list |> string_of_int

let part_2 fname =
  let open Point in
  let g = fname |> Util.slurp |> Grid.from_string in
  let trailheads =
    (* seq instead of set because we're not deduping this time *)
    Grid.all_coords_of '0' g |> List.(map (fun x -> [ x ] |> to_seq))
  in
  [ '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ]
  |> List.fold_left
       (fun trails elevation ->
         trails
         |> List.map (fun trail ->
                trail
                |> Seq.concat_map (Ints2.neighbors4 >> List.to_seq)
                |> Seq.filter (fun c ->
                       Grid.(in_bounds g c && get g c == elevation))))
       trailheads
  |> List.map Seq.length |> Util.sum_list |> string_of_int
