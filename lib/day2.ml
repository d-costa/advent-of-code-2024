open! Imports

type monotonicity = Inc | Dec | Unknown

let string_of_mon = function
  | Inc -> "Inc"
  | Dec -> "Dec"
  | Unknown -> "Unknown"

module M = struct
  (* Type to parse the input into *)
  type t = int list list

  (* Parse the input to type t, invoked for both parts *)
  let parse input =
    input
    |> String.split_on_char '\n'
    |> List.filter (fun word -> String.trim word <> "") (* remove empty lines from input *)
    |> List.map (fun line ->
        line
        |> String.split_on_char ' '
        |> List.filter (fun word -> String.trim word <> "") (* remove empty words from line *)
        |> List.map int_of_string
      )

  let calc_mon a b = if a < b then Inc else Dec

  let is_safe report = 
    let rec aux prev_mon = function 
      | x::y::xs -> 
        let mon = calc_mon x y in
        if abs(x - y) > 3 || abs(x - y) < 1 then false
        else if prev_mon <> Unknown && mon <> prev_mon then false
        else aux mon (y::xs)
      | _::[] -> true
      | [] -> true

    in aux Unknown report

  (* takes a list of n elements, and return n lists, where n_i is the original list without the ith element *)
  let gen_permutations lst =
    let rec aux i index_to_rm = function
      | [] -> []
      | x::xs -> 
        if i = index_to_rm then aux (i + 1) index_to_rm xs
        else x::aux (i + 1) index_to_rm xs
    in
    List.mapi (fun i _ -> aux 0 i lst) lst


  (* Run part 1 with parsed inputs *)
  let part1 reports = 
    let rec aux acc = function
      | [] -> acc
      | x::xs -> 
        if is_safe x then aux (acc + 1) xs
        else aux acc xs
    in
    aux 0 reports |> Printf.printf "%d\n"


  (* Run part 2 with parsed inputs *)
  let part2 reports = 
    let rec check_report acc = function (* for each report *)
      | [] -> acc
      | x::xs -> let perms = gen_permutations x in (* generate all permutations of the report *)
        let rec check_perms = function
          | [] -> 0 (* if no permutation is safe, count this report as invalid *)
          | p::_perms -> 
            if is_safe p then 1 (* if any permutation is safe, count this report as valid*)
            else check_perms _perms (* else check the next permutation *)
        in
        check_report (acc + check_perms perms) xs
    in
    check_report 0 reports |> Printf.printf "%d\n"
end

include M
include Day.Make (M)

(* Example input *)
let example = ""

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {| |}]
