let time_exec f =
  let t1 = Sys.time () in
  let res = f () in
  let t2 = Sys.time () in
  Printf.printf "Execution time (s): %f\n" (t2 -. t1);
  res

let fst (a, _) = a
let snd (_, b) = b

let slurp fname =
  let ch = open_in_bin fname in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let rec transpose = function
  | [] -> []
  | [] :: _ -> []
  | lst ->
      let firsts = List.map List.hd lst in
      let rest = transpose (List.map List.tl lst) in
      firsts :: rest

let sum = Seq.fold_left ( + ) 0
let prod = Seq.fold_left ( * ) 1
let sum_list = List.fold_left ( + ) 0
let prod_list = List.fold_left ( * ) 1

let htadd k v h =
  Hashtbl.add h k v;
  h

let identity x = x
let complement f x = not (f x)
let inc x = x + 1

let count_list lst =
  let res = Hashtbl.create 32 in
  let rec loop = function
    | [] -> res
    | x :: xs ->
        let cur = Hashtbl.find_opt res x |> Option.fold ~none:1 ~some:inc in
        Hashtbl.replace res x cur;
        loop xs
  in
  loop lst

let get_default ht x default =
  match Hashtbl.find_opt ht x with None -> default | Some y -> y

let count seq =
  let res = Hashtbl.create 32 in
  Seq.fold_left
    (fun res x ->
      let cur = Hashtbl.find_opt res x |> Option.fold ~none:0 ~some:inc in
      Hashtbl.replace res x cur;
      res)
    res seq

let count_p_list p = List.fold_left (fun s x -> if p x then s + 1 else s) 0
let count_p p = Seq.fold_left (fun s x -> if p x then s + 1 else s) 0

let rec adjacent_pairs s =
  match s |> Seq.uncons with
  | None -> fun () -> Seq.Nil
  | Some (x, rest) -> (
      match rest |> Seq.uncons with
      | None -> fun () -> Seq.Nil
      | Some (y, _) -> Seq.cons (x, y) (adjacent_pairs rest))

