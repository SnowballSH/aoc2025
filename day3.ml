open Printf
open Scanf
open String

let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines

(* AoC 2025 Day 3 *)

(* Helpers *)
let read_joltage s =
  String.to_seq s
  |> Seq.map (fun r -> Char.code r - Char.code '0')
  |> List.of_seq

let ( ++ ) a b = Int64.add a b

let lindex x arr =
  let n = List.length arr in
  let rec help index cur =
    match cur with
    | [] -> n
    | h :: t -> if x = h then index else help (index + 1) t
  in
  help 0 arr

let rindex x rarr =
  let n = List.length rarr in
  let arr = List.rev rarr in
  n - 1 - lindex x arr

(* Part 1 *)
let solve_all_part_1 all_lines =
  let rec solve lines acc =
    match lines with
    | [] -> acc
    | line :: tail ->
        let nums = read_joltage line in
        (* solve the problem for nums *)
        (* this solution is O(2n + 100) = O(n), where n = |nums| *)
        let li = Array.init 10 (fun i -> lindex (9 - i) nums) in
        let ri = Array.init 10 (fun i -> rindex (9 - i) nums) in
        let rec help a =
          let l = li.(9 - a) in
          let r = Array.find_index (fun j -> j > l) ri in
          match r with Some j -> (10 * a) + (9 - j) | None -> help (a - 1)
        in
        let ans = help 9 in
        solve tail (acc + ans)
  in
  solve all_lines 0

(* Part 2 *)
let solve_all_part_2 all_lines =
  let rec solve lines acc =
    match lines with
    | [] -> acc
    | line :: tail ->
        let nums = read_joltage line in
        let n = List.length nums in
        let limit = 12 in
        (* solve the problem for nums *)
        (* lexicographically largest subsequence *)
        (* use a monotonic stack *)
        (* this solution is O(n) *)
        let rec lls rem stk quota =
          match rem with
          | [] -> stk |> List.rev |> List.take limit
          | h :: t ->
              (* pop stack while smaller and able to *)
              let rec popstk s cur q =
                match s with
                | [] -> (s, q)
                | sh :: st ->
                    if q = 0 || sh >= cur then (s, q) else popstk st cur (q - 1)
              in
              let ns, nq = popstk stk h quota in
              lls t (h :: ns) nq
        in
        let seq = lls nums [] (n - limit) in
        let rec build digits acc =
          match digits with
          | [] -> acc
          | h :: t ->
              let new_acc = Int64.of_int h ++ Int64.mul 10L acc in
              build t new_acc
        in
        let ans = build seq 0L in
        solve tail (acc ++ ans)
  in
  solve all_lines 0L

(* Driver *)
let () =
  let lines = read_lines "day3.in" in
  let ans1 = solve_all_part_1 lines in
  printf "Part 1 = %d\n" ans1;
  let ans2 = solve_all_part_2 lines in
  printf "Part 2 = %Ld\n" ans2;
  ()
