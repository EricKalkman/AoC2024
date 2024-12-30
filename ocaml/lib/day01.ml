open Parsecomb

let part_1 fname =
  fname |> Util.slurp |> parse spaced_ints_table |> Result.get_ok
  |> Util.transpose_list
  |> List.map (List.sort Int.compare)
  |> Util.transpose_list
  |> List.map (function
       | x :: xs -> List.fold_left ( - ) x xs |> Int.abs
       | [] -> failwith "unreachable")
  |> Util.sum_list
  |> string_of_int

let part_2 fname =
  let cols =
    fname |> Util.slurp |> parse spaced_ints_table |> Result.get_ok
    |> Util.transpose_list
  in
  let right_counts = List.nth cols 1 |> Util.count_list in
  List.hd cols
  |> List.fold_left (fun s x -> s + (x * Util.get_default right_counts x 0)) 0
  |> string_of_int
