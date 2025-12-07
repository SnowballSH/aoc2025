open Printf
open Scanf
open String

let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines

(* AoC 2025 Day 6 *)

(* Helpers *)
let read_tokens s =
  s |> String.split_on_char ' ' |> List.filter (fun x -> x <> "")

let ( ++ ) a b = Int64.add a b
let ( -- ) a b = Int64.sub a b
let ( ** ) a b = Int64.mul a b
let ( // ) a b = Int64.div a b
let ( %% ) a b = Int64.rem a b

let rec transpose = function
  | [] :: _ -> []
  | [] -> []
  | rows -> List.map List.hd rows :: transpose (List.map List.tl rows)

let digits_to_int digits =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> if h = -1L then aux acc t else aux ((acc ** 10L) ++ h) t
  in
  aux 0L digits

let rec parse_inputs lines cur =
  match lines with
  | [] -> (cur, [])
  | h :: [] -> (cur, read_tokens h)
  | h :: t ->
      parse_inputs t ((h |> read_tokens |> List.map Int64.of_string) :: cur)

(* Part 1 *)

let rec calc rem_nums rem_ops acc =
  match rem_nums with
  | [] -> acc
  | h :: t -> (
      match rem_ops with
      | [] -> acc
      | op :: op_t -> (
          match op with
          | "+" -> calc t op_t acc ++ (h |> List.fold_left Int64.add 0L)
          | _ -> calc t op_t acc ++ (h |> List.fold_left Int64.mul 1L)))

let solve_all_part_1 all_lines =
  let nums_0, ops = parse_inputs all_lines [] in
  let nums_t = transpose nums_0 in
  calc nums_t ops 0L

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

let solve_all_part_2 all_lines =
  let rev_lines = List.rev all_lines in
  match rev_lines with
  | [] -> 0L
  | op_line :: rev_reg_lines ->
      let ops = read_tokens op_line in
      let nums =
        rev_reg_lines |> List.rev |> List.map String.to_seq
        |> List.map List.of_seq |> transpose
        |> List.map (List.filter (fun x -> x <> ' '))
        |> List.map List.to_seq |> List.map String.of_seq
        |> group_by_separator ""
        |> List.map (List.map Int64.of_string)
      in
      calc nums ops 0L

(* Driver *)
let () =
  let lines = read_lines "day6.in" in
  let ans1 = solve_all_part_1 lines in
  printf "Part 1 = %Ld\n" ans1;
  let ans2 = solve_all_part_2 lines in
  printf "Part 2 = %Ld\n" ans2;
  ()
