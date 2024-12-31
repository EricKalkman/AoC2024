module type Queue = sig
  type 'a t

  val empty : 'a t
  val singleton : 'a -> 'a t
  val is_empty : 'a t -> bool
  val push : 'a t -> 'a -> 'a t
  val push_front : 'a t -> 'a -> 'a t
  val peek : 'a t -> 'a option
  val peek_back : 'a t -> 'a option
  val pop : 'a t -> ('a * 'a t) option
  val pop_back : 'a t -> ('a * 'a t) option
  val count : 'a t -> int
  val of_list : 'a list -> 'a t
  val of_seq : 'a Seq.t -> 'a t
end

module FQ : Queue = struct
  type 'a t = 'a list * 'a list * int

  let empty = ([], [], 0)
  let singleton x = ([], [ x ], 1)
  let is_empty (_, _, c) = c == 0
  let push (ins, rev, c) x = (x :: ins, rev, c + 1)
  let push_front (ins, rev, c) x = (ins, x :: rev, c + 1)

  let peek (ins, rev, _) =
    match rev with
    | x :: _ -> Some x
    | [] -> ( match List.rev ins with [] -> None | x :: _ -> Some x)

  let peek_back (ins, rev, _) =
    match ins with
    | x :: _ -> Some x
    | [] -> ( match List.rev rev with [] -> None | x :: _ -> Some x)

  let pop (ins, rev, c) =
    match rev with
    | h :: t -> Some (h, (ins, t, c - 1))
    | [] -> (
        match List.rev ins with [] -> None | h :: t -> Some (h, ([], t, c - 1)))

  let pop_back (ins, rev, c) =
    match ins with
    | h :: t -> Some (h, (t, rev, c - 1))
    | [] -> (
        match List.rev rev with [] -> None | h :: t -> Some (h, (t, [], c - 1)))

  let count (_, _, c) = c
  let of_list lst = List.fold_left push empty lst
  let of_seq seq = Seq.fold_left push empty seq
end

let nil_visit _ _ () = ()
let cons_visit _ = List.cons

let bfs_visit visit initial_state neighfunc start stopcond =
  let rec loop q visit_state prevs =
    match FQ.pop q with
    | None -> (prevs, visit_state, None)
    | Some (u, _) when stopcond u -> (prevs, visit prevs u visit_state, Some u)
    | Some (u, q) ->
        let ns =
          neighfunc u |> List.filter (fun v -> not (Hashtbl.mem prevs v))
        in
        List.iter (fun v -> Hashtbl.replace prevs v u) ns;
        loop (List.fold_left FQ.push q ns) (visit prevs u visit_state) prevs
  in
  let prevs = Hashtbl.create 32 in
  Hashtbl.add prevs start start;
  loop (FQ.singleton start) initial_state prevs

let bfs neighfunc = bfs_visit nil_visit () neighfunc
let bfs_cons neighfunc = bfs_visit cons_visit [] neighfunc
