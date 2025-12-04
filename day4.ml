open Printf
open Scanf
open String

let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines

(* AoC 2025 Day 4 *)

(* Helpers *)
let read_rolls s =
  s |> String.to_seq |> Array.of_seq |> Array.map (fun r -> r == '@')

let ( ++ ) a b = Int64.add a b

let solve rows cols mat =
  let mark = Array.init rows (fun i -> Array.init cols (fun j -> false)) in
  let ans = ref 0 in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      mark.(i).(j) <- false;
      (* printf "%d" (Bool.to_int mat.(i).(j)); *)
      let k1 = if i > 0 && j > 0 then Bool.to_int mat.(i - 1).(j - 1) else 0 in
      let k2 = if i > 0 then Bool.to_int mat.(i - 1).(j) else 0 in
      let k3 =
        if i > 0 && j < cols - 1 then Bool.to_int mat.(i - 1).(j + 1) else 0
      in
      let k4 = if j > 0 then Bool.to_int mat.(i).(j - 1) else 0 in
      let k6 = if j < cols - 1 then Bool.to_int mat.(i).(j + 1) else 0 in
      let k7 =
        if i < rows - 1 && j > 0 then Bool.to_int mat.(i + 1).(j - 1) else 0
      in
      let k8 = if i < rows - 1 then Bool.to_int mat.(i + 1).(j) else 0 in
      let k9 =
        if i < rows - 1 && j < cols - 1 then Bool.to_int mat.(i + 1).(j + 1)
        else 0
      in
      let cnt = k1 + k2 + k3 + k4 + k6 + k7 + k8 + k9 in
      (* printf "(%d, %d) %d %b\n" i j cnt mat.(i).(j); *)
      if mat.(i).(j) && cnt < 4 then (
        ans := !ans + 1;
        mark.(i).(j) <- true)
    done
    (* printf "\n"; *)
  done;
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      if mark.(i).(j) then mat.(i).(j) <- false
    done
  done;
  !ans

(* Part 1 *)
let solve_all_part_1 all_lines =
  let rows = all_lines |> List.length in
  let cols = all_lines |> List.hd |> String.length in
  let mat = Array.init rows (fun i -> Array.init cols (fun j -> false)) in
  let rec process cur i =
    match cur with
    | [] -> ()
    | h :: t ->
        let rolls = read_rolls h in
        mat.(i) <- rolls;
        process t (i + 1)
  in
  process all_lines 0;
  solve rows cols mat

(* Part 2 *)
let solve_all_part_2 all_lines =
  let rows = all_lines |> List.length in
  let cols = all_lines |> List.hd |> String.length in
  let mat = Array.make rows (Array.make cols false) in
  let rec process cur i =
    match cur with
    | [] -> ()
    | h :: t ->
        let rolls = read_rolls h in
        mat.(i) <- rolls;
        process t (i + 1)
  in
  process all_lines 0;
  let ans = ref 0 in
  let tot = ref 0 in

  while
    (ans := solve rows cols mat;
     !ans)
    != 0
  do
    (* printf "%d\n" !ans; *)
    tot := !tot + !ans
  done;
  !tot

(* Driver *)
let () =
  let lines = read_lines "day4.in" in
  let ans1 = solve_all_part_1 lines in
  printf "Part 1 = %d\n" ans1;
  let ans2 = solve_all_part_2 lines in
  printf "Part 2 = %d\n" ans2;
  ()
