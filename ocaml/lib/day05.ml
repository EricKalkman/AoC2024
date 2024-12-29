open Ops
open Parsecomb

let ordering =
  parse_int -| chr '|' -| parse_int |> map (fun ((a, _), b) -> (a, b))

let all_inp =
  list_of ordering nl -| str "\n\n" -| ints_csv_table
  |> map (fun ((orderings, _), updates) ->
         ( orderings |> List.to_seq |> Util.pairs_to_directed_graph,
           updates |> List.map Array.of_list ))

let right_order rules (src, dst) =
  let ( >>= ) = Option.bind in
  Hashtbl.find_opt rules src >>= List.find_opt (( == ) dst) |> Option.is_some

let part_1 fname =
  let orderings, updates =
    fname |> Util.slurp |> parse all_inp |> Result.get_ok
  in
  updates |> List.to_seq
  |> Seq.filter
       (Array.to_seq >> Util.partition ~pad:1 ~n:2 >> Seq.map Util.coerce_pairs
       >> Seq.for_all (right_order orderings))
  |> Seq.map (fun a -> a.(Array.length a / 2))
  |> Util.sum
  |> string_of_int

let part_2 fname =
  let orderings, updates =
    fname |> Util.slurp |> parse all_inp |> Result.get_ok
  in
  updates |> List.to_seq
  |> Seq.filter
       (Array.to_seq >> Util.partition ~pad:1 ~n:2 >> Seq.map Util.coerce_pairs
       >> Seq.for_all (right_order orderings)
       >> not)
  |> Seq.map
       ( (fun arr ->
           arr
           |> Array.sort (fun a b ->
                  if right_order orderings (a, b) then -1 else 1);
           arr)
       >> fun a -> a.(Array.length a / 2) )
  |> Util.sum
  |> string_of_int
