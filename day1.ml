open Printf
open Scanf

let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines

(* AoC 2025 Day 1 *)

(* Helpers *)
let read_ops s =
  sscanf s "%c%d" (fun c x -> match c with 'L' -> -x | 'R' -> x | _ -> 0)

let modulo x y =
  let r = x mod y in
  if r >= 0 then r else r + y

(* Part 1 *)
let rec solve_all_part_1 lines cur =
  match lines with
  | [] -> 0
  | h :: t ->
      let dx = read_ops h in
      let after = modulo (cur + dx) 100 in
      let cnt = match after with 0 -> 1 | _ -> 0 in
      cnt + solve_all_part_1 t after

(* Part 2 *)
let rec solve_all_part_2 lines cur =
  match lines with
  | [] -> 0
  | h :: t ->
      let dx = read_ops h in
      let dest = cur + dx in
      let after = modulo dest 100 in
      let cnt =
        if dest <= 0 then (-dest / 100) + Bool.to_int (cur > 0)
        else if dest >= 100 then dest / 100
        else 0
      in
      cnt + solve_all_part_2 t after

(* Driver *)
let () =
  let lines = read_lines "day1.in" in
  let ans1 = solve_all_part_1 lines 50 in
  printf "Part 1 = %d\n" ans1;
  let ans2 = solve_all_part_2 lines 50 in
  printf "Part 2 = %d\n" ans2;
  ()
