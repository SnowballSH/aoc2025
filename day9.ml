open Printf
open Scanf
open String

let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines

(* AoC 2025 Day 9 *)

(* There's an O(n log n) solution but it's too hard to code *)

(* Helpers *)
let read_pair s = sscanf s "%Ld,%Ld" (fun x y -> (x, y))

let rec parse_inputs lines cur =
  match lines with [] -> cur | h :: t -> parse_inputs t (read_pair h :: cur)

let ( ++ ) a b = Int64.add a b
let ( -- ) a b = Int64.sub a b
let ( ** ) a b = Int64.mul a b
let ( // ) a b = Int64.div a b
let ( %% ) a b = Int64.rem a b

(* Part 1 *)

let solve_all_part_1 all_lines =
  let pts = parse_inputs all_lines [] in
  let rec helper2 (x1, y1) rem2 acc2 =
    match rem2 with
    | [] -> acc2
    | (x2, y2) :: t ->
        let v = (Int64.abs (x1 -- x2) ++ 1L) ** (Int64.abs (y1 -- y2) ++ 1L) in
        helper2 (x1, y1) t (Int64.max v acc2)
  in
  let rec helper1 rem1 acc1 =
    match rem1 with [] -> acc1 | h :: t -> helper1 t (helper2 h t acc1)
  in
  helper1 pts 0L

(* Part 2 *)
let intlen n = n |> Int64.to_string |> String.length

let group_by_separator sep lst =
  let rec aux current_group acc = function
    | [] -> List.rev (List.rev current_group :: acc)
    | h :: t ->
        if h = sep then aux [] (List.rev current_group :: acc) t
        else aux (h :: current_group) acc t
  in
  match lst with [] -> [] | _ -> aux [] [] lst

let solve_all_part_2 all_lines = 0L

(* Driver *)
let () =
  let lines = read_lines "day9.in" in
  let ans1 = solve_all_part_1 lines in
  printf "Part 1 = %Ld\n" ans1;
  let ans2 = solve_all_part_2 lines in
  printf "Part 2 = %Ld\n" ans2;
  ()
