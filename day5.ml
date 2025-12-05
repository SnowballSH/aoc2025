open Printf
open Scanf
open String

let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines

(* AoC 2025 Day 5 *)

(* Helpers *)
let read_range s = sscanf s "%Ld-%Ld" (fun a b -> (a, b))
let read_int s = sscanf s "%Ld" (fun x -> x)
let ( ++ ) a b = Int64.add a b
let ( -- ) a b = Int64.sub a b

(* Part 1 *)

(* Brute force O(nm) *)
(* let solve_all_part_1 all_lines =
  let rec ranges rem acc =
    match rem with
    | [] -> (rem, acc)
    | "" :: t -> (t, acc)
    | h :: t -> ranges t (read_range h :: acc)
  in
  let remaining_lines, rng = ranges all_lines [] in
  let rec count_occ x rem =
    match rem with
    | [] -> false
    | h :: t ->
        let a, b = h in
        if a <= x && x <= b then true else count_occ x t
  in
  let rec solve rem acc =
    match rem with
    | [] -> acc
    | h :: t ->
        let ok = count_occ (read_int h) rng in
        solve t (acc + Bool.to_int ok)
  in
  solve remaining_lines 0 *)

(* Two Pointers, using Part 2 solution *)
(* O(n log n + m log m) *)
let solve_all_part_1 all_lines =
  let rec ranges rem acc =
    match rem with
    | [] -> (rem, acc)
    | "" :: t -> (t, acc)
    | h :: t -> ranges t (read_range h :: acc)
  in
  let remaining_lines, rng = ranges all_lines [] in
  let rec enumerate l r rem acc =
    match rem with
    | [] -> (rem, acc)
    | h :: t ->
        if h < l then enumerate l r t acc
        else if h <= r then enumerate l r t (acc + 1)
        else (rem, acc)
  in
  let rec solve cur rem chk acc =
    let l1, r1 = cur in
    match rem with
    | [] ->
        let other, cnt = enumerate l1 r1 chk 0 in
        acc + cnt
    | nxt :: t ->
        let l2, r2 = nxt in
        if l2 > r1 then
          let other, cnt = enumerate l1 r1 chk 0 in
          solve nxt t other (acc + cnt)
        else if r2 > r1 then
          let other, cnt = enumerate l1 (l2 -- 1L) chk 0 in
          solve nxt t other (acc + cnt)
        else solve cur t chk acc
  in
  let a =
    List.sort
      (fun (a1, b1) (a2, b2) ->
        match Int64.compare a1 a2 with 0 -> Int64.compare b1 b2 | c -> c)
      rng
  in
  match a with
  | [] -> 0
  | h :: t ->
      let b = List.sort Int64.compare (List.map read_int remaining_lines) in
      solve h t b 0

(* Part 2 *)
(* Pretty standard interval covering problem *)
(* O(n log n) *)
let solve_all_part_2 all_lines =
  let rec solve cur rem acc =
    let l1, r1 = cur in
    match rem with
    | [] -> acc ++ (r1 -- l1 ++ 1L)
    | nxt :: t ->
        let l2, r2 = nxt in
        if l2 > r1 then solve nxt t (acc ++ (r1 -- l1 ++ 1L))
        else if r2 > r1 then solve nxt t (acc ++ (l2 -- l1))
        else solve cur t acc
  in
  let rec ranges rem acc =
    match rem with
    | [] -> acc
    | "" :: t -> acc
    | h :: t -> ranges t (read_range h :: acc)
  in
  let rng = ranges all_lines [] in
  let a =
    List.sort
      (fun (a1, b1) (a2, b2) ->
        match Int64.compare a1 a2 with 0 -> Int64.compare b1 b2 | c -> c)
      rng
  in
  match a with [] -> 0L | h :: t -> solve h t 0L

(* Driver *)
let () =
  let lines = read_lines "day5.in" in
  let ans1 = solve_all_part_1 lines in
  printf "Part 1 = %d\n" ans1;
  let ans2 = solve_all_part_2 lines in
  printf "Part 2 = %Ld\n" ans2;
  ()
