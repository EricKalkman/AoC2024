module Chunk = struct
  type t = { pos : int; len : int; id : int } [@@deriving eq, ord]
end

module ChunkSet = Set.Make (Chunk)
module ChunkMap = Map.Make (Chunk)

let parse_input s =
  (* this used to use sequence operations... but those are reaaaaally slow in OCaml *)
  let open Chunk in
  let digits = Array.make (String.length s + 1) 0 in
  String.iteri (fun i c -> digits.(i) <- Char.code c - Char.code '0') s;
  let poses = Array.make (Array.length digits) 0 in
  Array.mapi_inplace
    (fun i _ -> if i == 0 then 0 else poses.(i - 1) + digits.(i - 1))
    poses;

  let files, spaces = (ref [], ref []) in
  for i = 0 to Array.length digits - 1 do
    let pos = poses.(i) in
    let d = digits.(i) in
    if Int.logand i 1 == 0 then files := { id = i / 2; pos; len = d } :: !files
    else spaces := { id = i / 2; pos; len = d } :: !spaces
  done;
  (!files, !spaces)

let rec move_files_blockwise rev_files spaces =
  let open Chunk in
  match rev_files with
  | [] -> failwith "there should always be files"
  | file :: files -> (
      if file.len == 0 then move_files_blockwise files spaces
      else
        match spaces with
        | [] -> rev_files
        | space :: spaces ->
            if space.pos > file.pos then rev_files
            else if space.len == 0 then move_files_blockwise rev_files spaces
            else
              let { pos = spos; len = slen; _ } = space in
              let { id = fid; pos = fpos; len = flen } = file in
              let to_consume = Int.min slen flen in
              { id = fid; pos = spos; len = to_consume }
              :: move_files_blockwise
                   ({ id = fid; pos = fpos; len = flen - to_consume } :: files)
                   ({ id = 0; pos = spos + to_consume; len = slen - to_consume }
                   :: spaces))

let triangle_sum first last = (last - first + 1) * (first + last) / 2

let checksum chunks =
  let open Chunk in
  chunks
  |> List.map (fun { id; pos; len } -> id * triangle_sum pos (pos + len - 1))
  |> Util.sum_list

let move_files_leftward files spaces =
  let open Chunk in
  let rec aux files spaces =
    (** uses a sorted Set of Chunk.t's to keep track of the leftmost space
       of a given length and recursively constructs a list of moved files *)
    match files with
    | [] -> []
    | file :: files ->
        let leftmost_space =
          [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ]  (* 9 options for space lengths *)
          (* all space lengths that file fits into *)
          |> List.filter_map (fun len ->
                 if len < file.len || ChunkSet.is_empty spaces.(len) then None
                 else Some spaces.(len))
          (* the leftmost of those space lengths *)
          |> List.fold_left
               (fun m set ->
                 let x = ChunkSet.min_elt set in
                 if x.pos < m.pos then x else m)
               { id = -1; pos = Int.max_int; len = 0 }
        in
        (* if no space large enough was found to house file *)
        if leftmost_space.id == -1 || file.pos < leftmost_space.pos then
          file :: aux files spaces
        else
          let { pos = spos; len = slen; id = sid } = leftmost_space in
          let to_add = { id = file.id; pos = spos; len = file.len } in
          spaces.(slen) <- ChunkSet.remove leftmost_space spaces.(slen);
          (if slen > file.len then
             let new_space =
               { pos = spos + file.len; len = slen - file.len; id = sid }
             in
             spaces.(slen - file.len) <-
               ChunkSet.add new_space spaces.(slen - file.len));
          to_add :: aux files spaces
  in

  let spaces_arr = Array.make 10 ChunkSet.empty in
  spaces
  |> Util.group_list_by (fun { len; _ } -> len)
  |> Hashtbl.iter (fun k vs -> spaces_arr.(k) <- ChunkSet.of_list vs);
  aux files spaces_arr

let part mover fname =
  let rev_files, rev_spaces = fname |> Util.slurp |> String.trim |> parse_input in
  mover rev_files (List.rev rev_spaces) |> checksum |> string_of_int

let part_1 = part move_files_blockwise
let part_2 = part move_files_leftward
