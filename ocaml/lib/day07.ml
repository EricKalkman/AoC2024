open Parsecomb

let equation =
  parse_int -| str ": " -| spaced_ints
  |> map (fun ((target, _), terms) -> (target, terms))

let input = list_of equation nl

type operator = Id | Add | Mul | Concat

let deconcat target x =
  (* note: terms are never 0, so x should never be 0 *)
  let divisor =
    Float.(x + 1 |> of_int |> log10 |> ceil |> pow 10.0 |> round |> to_int)
  in
  if target mod divisor == x then Some (target / divisor) else None

let inverse op target x =
  match op with
  | Add -> Some (target - x)
  | Mul -> if target mod x == 0 then Some (target / x) else None
  | Concat -> deconcat target x
  | _ -> failwith "not implemented"

let rec determine_ops ops target = function
  | [ term ] when target == term -> Some [ (Id, term) ]
  | [ _ ] -> None
  | term :: terms ->
      let ( let* ) = Option.bind in
      ops
      |> Seq.map (fun op ->
             let* next_target = inverse op target term in
             let* result = determine_ops ops next_target terms in
             Some ((op, term) :: result))
      |> Seq.find Option.is_some |> Option.value ~default:None
  | _ -> None

let part ops fname =
  fname |> Util.slurp |> parse input |> Result.get_ok
  |> List.filter (fun (target, terms) ->
         determine_ops ops target (List.rev terms) |> Option.is_some)
  |> List.map Util.fst |> Util.sum_list |> string_of_int

let part_1 = part (List.to_seq [ Add; Mul ])
let part_2 = part (List.to_seq [ Add; Mul; Concat ])
