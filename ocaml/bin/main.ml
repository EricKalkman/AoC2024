open Aoclib

let day_fname day =
  Printf.sprintf "../inputs/day%02d.inp" day

let run_day n p1 p2 =
  let fname = day_fname n in
  let d1, d2 = (fun () -> p1 fname), (fun () -> p2 fname) in
  Printf.printf "Day %02d\n" n;
  let r1 = Util.time_exec d1 in
  Printf.printf "Part 1: %s\n" r1;
  let r2 = Util.time_exec d2 in
  Printf.printf "Part 2: %s\n\n" r2;
  flush Stdlib.stdout

let () =
  run_day 1 Day01.part_1 Day01.part_2;
  run_day 2 Day02.part_1 Day02.part_2;
  run_day 3 Day03.part_1 Day03.part_2;
  run_day 4 Day04.part_1 Day04.part_2;
  run_day 5 Day05.part_1 Day05.part_2;
  run_day 6 Day06.part_1 Day06.part_2;
  run_day 7 Day07.part_1 Day07.part_2;
  run_day 8 Day08.part_1 Day08.part_2;
