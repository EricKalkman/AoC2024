open Ops

let group_frequencies g =
  g
  |> Grid.all_coords_of_p (fun c -> c != '.')
  |> Util.group_list_by (fun c -> Grid.get g c)

let antinodes_1 g set =
  let open Point.Ints2 in
  set |> Util.combinations 2
  |> Seq.concat_map (fun lst ->
         let a, b = Util.coerce_pairs lst in
         let delta = b @- a |> scale 2 in
         [ a @+ delta; b @- delta ] |> List.to_seq)
  |> Seq.filter (Grid.in_bounds g)

let antinodes_2 g set =
  let open Point.Ints2 in
  set |> Util.combinations 2
  |> Seq.concat_map (fun lst ->
         let a, b = Util.coerce_pairs lst in
         let delta_b = b @- a in
         let delta_a = scale (-1) delta_b in
         Seq.append
           (Grid.trace_coords g delta_a a)
           (Grid.trace_coords g delta_b b))

let part antinodes fname =
  let g = fname |> Util.slurp |> Grid.from_string in
  let freqs = group_frequencies g in
  freqs |> Hashtbl.to_seq_values
  |> Seq.concat_map (List.to_seq >> antinodes g)
  |> Util.dedup |> Hashtbl.length |> string_of_int

let part_1 = part antinodes_1
let part_2 = part antinodes_2
