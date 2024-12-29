module StringBuf = struct
  type t = { s : string; pos : int }

  let make s = { s; pos = 0 }
  let advance by { s; pos } = { s; pos = Int.min (String.length s) (pos + by) }

  let peek { s; pos } =
    if pos == String.length s then None else Some (String.get s pos)

  let peek_str n { s; pos } =
    if pos + n > String.length s then None else Some (String.sub s pos n)

  let is_eof { s; pos } = pos == String.length s
  let length { s; pos } = String.length s - pos

  let get buf i =
    if i < 0 || i >= length buf then invalid_arg "StringBuf.get"
    else String.get buf.s (i + buf.pos)

  let get_opt buf i =
    if i < 0 || i >= length buf then None
    else Some (String.get buf.s (i + buf.pos))
end

let parse p s =
  let buf = StringBuf.make s in
  p buf |> Result.map (fun (res, _) -> res)

let chr c buf =
  let open StringBuf in
  match peek buf with
  | None -> Error "eof"
  | Some x when x == c -> Ok (c, advance 1 buf)
  | _ -> Error ("wrong char " ^ String.make 1 c)

let charset s buf =
  let open StringBuf in
  match peek buf with
  | None -> Error "eof"
  | Some c when String.contains s c -> Ok (c, advance 1 buf)
  | _ -> Error ("wrong char of set " ^ s)

let any buf =
  let open StringBuf in
  buf |> peek
  |> Option.to_result ~none:"eof"
  |> Result.map (fun x -> (x, advance 1 buf))

let char_p p buf =
  let open StringBuf in
  match peek buf with
  | None -> Error "eof"
  | Some c when p c -> Ok (c, advance 1 buf)
  | _ -> Error "char did not match predicate"

let str s buf =
  let open StringBuf in
  match peek_str (String.length s) buf with
  | None -> Error "eof"
  | Some t when String.equal s t -> Ok (s, advance (String.length s) buf)
  | _ -> Error ("wrong string " ^ s)

let str_p p buf =
  let open StringBuf in
  let buf_len = length buf in
  let len = ref (-1) in
  let i = ref 0 in
  while !len == -1 && !i < buf_len do
    if not (p (get buf !i)) then len := !i else i := !i + 1
  done;
  if !len == -1 then len := buf_len;
  if !len == 0 then Error "no string matched predicate"
  else Ok (String.sub buf.s buf.pos !len, advance !len buf)

let until p buf =
  let open StringBuf in
  let rec loop n =
    if n >= length buf then
      Ok
        ( peek_str n buf |> Option.get,
          advance n buf )
    else
      match p (advance n buf) with
      | Ok _ -> Ok (peek_str n buf |> Option.get, advance n buf)
      | Error _ -> loop (n+1)
  in
  loop 0

let str_of_set s buf =
  buf
  |> str_p (String.contains s)
  |> Result.map_error (fun _ -> "no string of set " ^ s)

let map f p buf =
  let ( let* ) = Result.bind in
  let* res, buf2 = p buf in
  Ok (f res, buf2)

let chain a b buf =
  let ( let* ) = Result.bind in
  let* a_res, buf1 = a buf in
  let* b_res, buf2 = b buf1 in
  Ok ((a_res, b_res), buf2)

let chain_ps ps buf =
  let rec loop acc buf = function
    | [] -> Ok (List.rev acc, buf)
    | p :: ps ->
        let ( let* ) = Result.bind in
        let* res, buf2 = p buf in
        loop (res :: acc) buf2 ps
  in
  loop [] buf ps

let ( -| ) = chain

let choice a b buf =
  match a buf with
  | Error _ ->
      b buf
      |> Result.map (fun (x, buf) -> (Either.Right x, buf))
      |> Result.map_error (fun _ -> "failed both choices")
  | x -> x |> Result.map (fun (x, buf) -> (Either.Left x, buf))

let ( -|| ) = chain

let maybe p buf =
  p buf
  |> Result.fold
       ~ok:(fun (x, buf) -> Ok (Some x, buf))
       ~error:(fun _ -> Ok (None, buf))

let many p buf =
  let rec loop acc buf =
    match p buf with
    | Error _ -> Ok (List.rev acc, buf)
    | Ok (x, buf) -> loop (x :: acc) buf
  in
  loop [] buf

let list_of p sep =
  p -| maybe (many (sep -| p))
  |> map (fun (a, maybe_bs) ->
         a :: Option.fold ~none:[] ~some:(List.map Util.snd) maybe_bs)

(* relatively useless; I suppose that it can encourage the garbage collector *)
let skip p = p |> map (fun _ -> ())
let collapse_either = function Either.Left a -> a | Either.Right a -> a
let digits = str_of_set "0123456789"

let parse_int =
  maybe (chr '-') -| digits
  |> map (function
       | Some _, digstr -> -int_of_string digstr
       | _, digstr -> int_of_string digstr)

let ws = str_of_set " \n\r\t"
let nl = charset "\n\r"
let ints_csv = list_of parse_int (chr ',' -| maybe (str_of_set " \t"))
let spaced_ints = list_of parse_int (str_p (fun c -> Char.equal c ' '))

let any_ints =
  list_of parse_int (str_p (fun c -> not (String.contains "0123456789-" c)))

let ints_csv_table = list_of ints_csv nl
let spaced_ints_table = list_of spaced_ints nl
