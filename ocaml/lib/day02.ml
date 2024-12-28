let is_safe lst =
  match lst with
  | [] -> true
  | [ _ ] -> true
  | x :: y :: xs ->
      let dir = y - x in
      x :: y :: xs |> List.to_seq |> Util.adjacent_pairs
      |> Seq.for_all (fun (a, b) ->
             Int.abs (b - a) >= 1 && Int.abs (b - a) <= 3 && (b - a) * dir > 0)

let part_1 fname =
  let open Parsecomb in
  fname |> Util.slurp |> parse spaced_ints_table |> Result.get_ok
  |> Util.count_p_list is_safe |> string_of_int

let drops lst =
  let rec loop hd lst =
    match lst with
    | [] -> fun () -> Seq.Nil
    | x :: xs -> Seq.cons (List.append (List.rev hd) xs) (loop (x :: hd) xs)
  in
  loop [] lst

let part_2 fname =
  let open Parsecomb in
  fname |> Util.slurp |> parse spaced_ints_table |> Result.get_ok
  |> Util.count_p_list (fun x -> is_safe x || Seq.exists is_safe (drops x))
  |> string_of_int
