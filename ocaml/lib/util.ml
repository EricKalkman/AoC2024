open Ops

let time_exec f =
  let t1 = Sys.time () in
  let res = f () in
  let t2 = Sys.time () in
  Printf.printf "Execution time (ms): %f\n" (1000.0 *. (t2 -. t1));
  res

let fst (a, _) = a
let snd (_, b) = b

let slurp fname =
  let ch = open_in_bin fname in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let rec transpose_list = function
  | [] -> []
  | [] :: _ -> []
  | lst ->
      let firsts = List.map List.hd lst in
      let rest = transpose_list (List.map List.tl lst) in
      firsts :: rest

let rec transpose seq =
  match Seq.uncons seq with
  | None -> Seq.empty
  | Some (h, _) ->
      if Seq.is_empty h then Seq.empty
      else fun () ->
        let s2 = Seq.map (Seq.uncons >> Option.get) seq in
        let firsts = Seq.map fst s2 in
        let rest = transpose (Seq.map snd s2) in
        Seq.Cons (firsts, rest)

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

let ht_update ht k f = Hashtbl.replace ht k (f (Hashtbl.find_opt ht k))

let pairs_to_directed_graph pairs =
  let result = Hashtbl.create 32 in
  pairs
  |> Seq.iter (fun (n1, n2) ->
         ht_update result n1 (function None -> [ n2 ] | Some vs -> n2 :: vs);
         (* ensure that n2 shows up as a node in the graph with e.g., a call to keys *)
         ht_update result n2 (function
           | None -> []
           | Some x -> x));
  result

let pairs_to_undirected_graph pairs =
  let result = Hashtbl.create 32 in
  pairs
  |> Seq.iter (fun (n1, n2) ->
         ht_update result n1 (function None -> [ n2 ] | Some vs -> n2 :: vs);
         ht_update result n2 (function None -> [ n1 ] | Some vs -> n1 :: vs));
  result

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

let rec partition ?pad ~n seq =
  let pad = Option.value pad ~default:n in
  let h = seq |> Seq.take n |> List.of_seq in
  if List.length h < n then Seq.empty
  else fun () -> Seq.Cons (h, partition ~pad ~n (Seq.drop pad seq))

let coerce_pairs = function [ a; b ] -> (a, b) | _ -> failwith "expected pair"

let group_by f seq =
  let result = Hashtbl.create 32 in
  Seq.(
    seq
    |> iter (fun v ->
           let k = f v in
           ht_update result k (function None -> [ v ] | Some vs -> v :: vs)));
  result

let group_list_by f lst =
  let result = Hashtbl.create 32 in
  List.(
    lst
    |> iter (fun v ->
           let k = f v in
           ht_update result k (function None -> [ v ] | Some vs -> v :: vs)));
  result

let rec combinations k seq =
  if k == 1 then seq |> Seq.map (fun x -> [ x ])
  else
    match Seq.uncons seq with
    | None -> Seq.empty
    | Some (h, t) ->
        Seq.append
          (Seq.map (List.cons h) (combinations (k - 1) t))
          (combinations k t)

let dedup seq =
  let h = Hashtbl.create 32 in
  seq |> Seq.iter (fun x -> Hashtbl.replace h x 1);
  h
