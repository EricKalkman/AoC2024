type 'a t = { data : 'a array; height : int; width : int }

let make height width e =
  { data = Array.make (height * width) e; height; width }

let from_string s =
  let s_trimmed = String.trim s in
  let width = String.index s_trimmed '\n' in
  let height = (String.length s_trimmed + 1) / (width + 1) in
  let arr = Array.make (width * height) ' ' in
  for row = 0 to width - 1 do
    for col = 0 to height - 1 do
      let s_idx = (row * (width + 1)) + col in
      let a_idx = (row * width) + col in
      arr.(a_idx) <- s_trimmed.[s_idx]
    done
  done;
  { data = arr; height; width }

let height g = g.height
let width g = g.width

let in_bounds { height; width; _ } (row, col) =
  row >= 0 && row < height && col >= 0 && col < width

let map f { data; width; height } =
  { data = data |> Array.map f; width; height }

let contains height width row col =
  row < 0 || row >= height || col < 0 || col >= width

let get { data; height; width } (row, col) =
  if contains height width row col then invalid_arg "grid get"
  else data.((row * width) + col)

let set { data; height; width } (row, col) x =
  if contains height width row col then invalid_arg "grid set"
  else data.((row * width) + col) <- x

let to_matrix { data; height; width } =
  let res = Array.make_matrix height width data.(0) in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      res.(i).(j) <- data.((i * width) + j)
    done
  done;
  res

let coords { height; width; _ } =
  Seq.unfold
    (fun (row, col) ->
      if row == height then None
      else if col == width then
        let p = (row + 1, 0) in
        Some (p, p)
      else
        let p = (row, col + 1) in
        Some (p, p))
    (0, -1)

let first_coord_p p { data; width; _ } =
  let i = ref 0 in
  let break = ref false in
  let len = Array.length data in
  while (not !break) && !i < len do
    if p data.(!i) then break := true else i := !i + 1
  done;
  if !i == len then None else Some (!i / width, !i mod width)

let first_coord x = first_coord_p (fun y -> x == y)

let all_coords_of_p p { data; width; _ } =
  let i = ref 0 in
  let res = ref [] in
  let len = Array.length data in
  while !i < len do
    if p data.(!i) then
      let row, col = (!i / width, !i mod width) in
      res := (row, col) :: !res
    else ();
    i := !i + 1
  done;
  !res

let all_coords_of x = all_coords_of_p (fun y -> x == y)

let trace ?n g dir start =
  let open Point in
  let delta = Ints2.step dir in
  let res =
    Seq.unfold
      (fun c ->
        let c2 = Ints2.(c @+ delta) in
        if in_bounds g c2 then Some (get g c2, c2) else None)
      Ints2.(start @- delta)
  in
  match n with None -> res | Some n -> res |> Seq.take n

let trace_string ?n g dir start =
  trace ?n g dir start |> String.of_seq

let spells_along g s dir start =
  let len = String.length s in
  let traced = trace_string ~n:len g dir start in
  String.equal s traced
