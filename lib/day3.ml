open! Imports

module M = struct
  (* Type to parse the input into *)

  type alphabet = M | U | L | LPAR | RPAR | NUM of string | COMMA | ERR | DO | DONT

  type t = alphabet list

  let string_from_alphabet_list al = 
    let rec aux = function
      | M::xs -> "M" ^ aux xs
      | U::xs -> "U" ^ aux xs
      | L::xs -> "L" ^ aux xs
      | LPAR::xs -> "(" ^ aux xs
      | RPAR::xs -> ")" ^ aux xs
      | COMMA::xs -> "," ^ aux xs
      | NUM(i)::xs -> i ^ aux xs
      | ERR::xs -> "#" ^ aux xs
      | DO::xs -> "DO" ^ aux xs
      | DONT::xs -> "DONT" ^ aux xs
      | [] -> ""
    in aux al

  let tokenizer input_string = 
    let rec aux = function
      | [] -> []
      | x::xs -> 
        match x with
        | 'm' -> M::aux xs
        | 'u' -> U::aux xs
        | 'l' -> L::aux xs
        | '(' -> LPAR::aux xs
        | ')' -> RPAR::aux xs
        | ',' -> COMMA::aux xs
        | 'd' -> 
          (match xs with
           | 'o'::'('::')'::rest -> DO::aux rest
           | 'o'::'n'::'\''::'t'::'('::')'::rest -> DONT::aux rest
           | _ -> ERR::aux xs)
        | '0'..'9' as c -> 
          let rec collect_digits acc = function
            | d::ds when '0' <= d && d <= '9' -> collect_digits (acc ^ String.make 1 d) ds
            | rest -> (NUM acc, rest)
          in
          let (num_token, rest) = collect_digits (String.make 1 c) xs in
          num_token :: aux rest
        | _ -> ERR::aux xs
    in

    aux (String.to_seq input_string |> List.of_seq)

  (* Parse the input to type t, invoked for both parts *)
  let parse input = tokenizer input

  (* Run part 1 with parsed inputs *)
  let part1 input = 
    let rec aux = function
      | M::U::L::LPAR::NUM(a)::COMMA::NUM(b)::RPAR::xs -> (int_of_string a * int_of_string b) + aux xs
      | _::xs -> aux xs
      | [] -> 0
    in
    aux input |> Printf.printf "%d\n"

  (* Run part 2 with parsed inputs *)
  let part2 input =
    let rec aux_dont = function
      | DO::xs -> aux_do xs
      | _::xs -> aux_dont xs
      | [] -> 0
    and aux_do = function
      | DO::xs -> aux_do xs
      | DONT::xs -> aux_dont xs
      | M::U::L::LPAR::NUM(a)::COMMA::NUM(b)::RPAR::xs -> (int_of_string a * int_of_string b) + aux_do xs
      | _::xs -> aux_do xs
      | [] -> 0
    in
    aux_do input |> Printf.printf "%d\n"
end

include M
include Day.Make (M)

(* Example input *)
let example = ""

(* Expect test for example input *)
let%expect_test _ = run example ; [%expect {| |}]
