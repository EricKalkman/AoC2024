open Ops

let parse_mul =
  let open Parsecomb in
  chain_ps [ str "mul("; digits; str ","; digits; str ")" ]
  |> map (function
       | [ _; a; _; b; _ ] -> [ a |> int_of_string; b |> int_of_string ]
       | _ -> failwith "unreachable")

let not_parse_mul = Parsecomb.(until parse_mul |> map (fun _ -> []))

let parse_muls =
  let open Parsecomb in
  maybe not_parse_mul -| list_of parse_mul not_parse_mul
  |> map (snd >> List.filter (Util.complement List.is_empty))

let part_1 fname =
  fname |> Util.slurp |> Parsecomb.parse parse_muls |> Result.get_ok
  |> List.map Util.prod_list |> Util.sum_list |> string_of_int

let parse_do = Parsecomb.str "do()"
let parse_dont = Parsecomb.str "don't()"

let split_donts =
  let open Parsecomb in
  list_of (until parse_dont) (parse_dont -| until parse_do)

let part_2 fname =
  let open Parsecomb in
  fname |> Util.slurp |> parse split_donts |> Result.get_ok
  |> List.concat_map (fun s ->
         s |> parse parse_muls
         |> Result.fold ~ok:Util.identity ~error:(fun _ -> []))
  |> List.map Util.prod_list |> Util.sum_list |> string_of_int
