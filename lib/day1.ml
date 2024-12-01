open! Imports

module IntMap = Map.Make(Int)

module M = struct
  (* Type to parse the input into *)
  type t = (int * int) list

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
        |> function
        | [a; b] -> (a, b)
        | _ -> failwith "Invalid input format"
      )

  (* take a list of pairs of numbers, and returns two lists *)
  let unzip pairs = List.split pairs

  let sort_lists numbers =
    let left, right = unzip numbers in
    (List.sort compare left, List.sort compare right)

  let count_occurrences value lst =
    List.fold_left (fun acc x -> if x = value then acc + 1 else acc) 0 lst

  (* Run part 1 with parsed inputs *)
  let part1 numbers =
    let rec aux acc = function
      | [], [] -> acc
      | a::l, b::r -> aux (acc + abs (a - b)) (l, r) (* sum absolute differences *)
      | _ -> failwith "Lists are of unequal length"

    in
    let left, right = sort_lists numbers in
    aux 0 (left, right) |> Printf.printf "%d\n"

  (* Run part 2 with parsed inputs *)
  let part2 numbers =
    let memo = ref IntMap.empty in

    let rec aux acc = function
      | [], _ -> acc
      | a::l, r ->
        let count =
          match IntMap.find_opt a !memo with (* check if we have already counted a *)
          | Some count -> count
          | None -> (* not present in memo, calcualte and save it *)
            let count = count_occurrences a r in
            memo := IntMap.add a count !memo;
            count
        in
        aux (acc + count * a) (l, r) (* sum product of count of a in r and a *)
    in
    let left, right = unzip numbers in
    aux 0 (left, right) |> Printf.printf "%d\n"

end

include M
include Day.Make (M)

(* Example input *)
let example = "
3   4
4   3
2   5
1   3
3   9
3   3"

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {|
11
31
|}]
