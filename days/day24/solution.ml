open Base
open Aoc_lib.Helpers

let day = 24
type t = int list

let parse_input (input: string): t = String.split_lines input |> List.map ~f:Int.of_string

let part1 (input: t): string =
  let third = (List.sum (module Int) input ~f:Fn.id) / 3 in
  let rec aux n rem taken =
    if n = 0 then Some taken else
    if n < 0 then None else
        match rem with
        | [] -> None
        | x :: xs -> Option.first_some (aux (n - x) xs (x :: taken)) (aux n xs taken)
    in
    aux third (List.rev input) []
    |> Option.value_exn
    |> List.reduce_exn ~f:( * )
    |> Int.to_string

let part2 (input: t): string =
  let fourth = (List.sum (module Int) input ~f:Fn.id) / 4 in
  let rec aux n rem taken =
    if n = 0 then [taken] else
      if n < 0 then [] else
        match rem with
        | [] -> []
        | x :: xs -> aux (n - x) xs (x :: taken) @ aux n xs taken
    in
    aux fourth (List.rev input) []
    |> List.min_elt ~compare:(fun a b ->
        let l = List.length a - List.length b in
        if l <> 0 then l else
          let p1 = List.reduce_exn a ~f:( * ) in
          let p2 = List.reduce_exn b ~f:( * ) in
          p1 - p2
    )
    |> Option.value_exn
    |> List.reduce_exn ~f:( * )
    |> Int.to_string
