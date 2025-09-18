open Base
open Aoc_lib.Helpers

let day = 5
type t = string list

let parse_input (input: string): t = String.split_lines input

let part1 (input: t): string = List.count ~f:(fun s ->
  match String.to_list s with
  | hd :: tl ->
      let pairs = zip (hd :: tl) tl in
      let vowels = List.count ~f:(fun c -> List.mem ~equal:Char.equal ['a'; 'e'; 'i'; 'o'; 'u'] c) (hd :: tl) in
      let double = List.exists pairs ~f:(uncurry Char.equal) in
      let blacks = List.exists pairs ~f:(fun (a, b) ->
          match (a, b) with
          | ('a', 'b') | ('c', 'd') | ('p', 'q') | ('x', 'y') -> true
          | _ -> false
        )
      in Int.(>=) vowels 3 && double && not blacks
  | _ -> false
) input |> Int.to_string

let part2 (input: t): string = List.count ~f:(fun s ->
  match String.to_list s with
  | hd :: hd2 :: tl ->
      let pairs = zipi (hd :: hd2 :: tl) (hd2 :: tl)
      in let hasPair = List.exists (List.cartesian_product pairs pairs)
        ~f:(fun ((i, (a, b)), (j, (c, d))) -> Int.abs (i - j) > 1 && Char.equal a c && Char.equal b d)
      in let triples = zip3 (hd :: hd2 :: tl) (hd2 :: tl) tl in
      hasPair && List.exists triples ~f:(fun (a, b, c) -> Char.equal a c)
  | _ -> false
) input |> Int.to_string
