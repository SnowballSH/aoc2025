open Printf
open Scanf
open String

let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines

(* AoC 2025 Day 2 *)

(* Helpers *)
let read_ids s =
  let ranges = String.split_on_char ',' s in
  List.map (fun r -> sscanf r "%Ld-%Ld" (fun a b -> (a, b))) ranges

(* Part 1 *)
let solve_all_part_1 lines =
  let ranges = read_ids (List.hd lines) in
  let rec solve ranges acc =
    match ranges with
    | [] -> acc
    | (x, y) :: t ->
        let rec count a b cnt =
          if a > b then cnt
          else
            let s = Int64.to_string a in
            let l = String.length s in
            let delta =
              if l mod 2 == 1 then 0L
              else
                let s1 = String.sub s 0 (l / 2) in
                let s2 = String.sub s (l / 2) (l / 2) in
                let eq = String.equal s1 s2 in
                if eq then a else 0L
            in
            count (Int64.add a 1L) b (Int64.add cnt delta)
        in
        let c = count x y 0L in
        solve t (Int64.add acc c)
  in
  solve ranges 0L

(* Part 2 *)
let rec repeat n s = if n = 0 then "" else s ^ repeat (n - 1) s

let solve_all_part_2 lines =
  let ranges = read_ids (List.hd lines) in
  let rec solve ranges acc =
    match ranges with
    | [] -> acc
    | (x, y) :: t ->
        let rec count a b cnt =
          if a > b then cnt
          else
            let s = Int64.to_string a in
            let l = String.length s in
            let delta =
              let rec check k =
                if k * 2 > l then false
                else if
                  l mod k == 0
                  &&
                  let block = String.sub s 0 k in
                  let rep = repeat (l / k) block in
                  String.equal s rep
                then true
                else check (k + 1)
              in
              if check 1 then a else 0L
            in
            count (Int64.add a 1L) b (Int64.add cnt delta)
        in
        let c = count x y 0L in
        solve t (Int64.add acc c)
  in
  solve ranges 0L

(* Driver *)
let () =
  let lines = read_lines "day2.in" in
  let ans1 = solve_all_part_1 lines in
  printf "Part 1 = %Ld\n" ans1;
  let ans2 = solve_all_part_2 lines in
  printf "Part 2 = %Ld\n" ans2;
  ()
