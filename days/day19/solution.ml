open Base
open Aoc_lib.Helpers

let day = 19
type t = ((string * string) list * string)

let parse_input (input: string): t =
  let lines = String.split_lines input in
  let m = List.last_exn lines in
  let ts =
    List.drop_last_exn lines
    |> List.drop_last_exn
    |> List.map ~f:(fun s ->
        match String.split ~on:' ' s with
        | [l; _; r] -> (l, r)
        | _ -> failwith "Parse Error"
    )
  in (ts, m)

let part1 ((ts, m): t): string =
  let len = String.length m in
  let rec aux rest: string list =
    if String.length rest = 0 then [] else
      let skip = aux (String.sub rest ~pos:1 ~len:(String.length rest - 1)) in
      let replace =
        List.filter ~f:(fun (prefix, _) -> String.is_prefix rest ~prefix) ts
        |> List.map ~f:(fun (f, r) ->
          let rs = String.chop_prefix_exn rest ~prefix:f in
          String.sub m ~pos:0 ~len:(len - String.length rest) ^ r ^ rs
        )
      in skip @ replace 
  in aux m |> Set.of_list (module String) |> Set.length |> Int.to_string


let part2 ((ts, m): t): string =
  let rules = List.map ts ~f:(fun (f, r) -> (r, f)) in
  let rec reduce str steps =
    if String.equal str "e" then steps
    else
      let found = ref false in
      let new_str = ref str in
      List.iter rules ~f:(fun (r, f) ->
        match String.substr_index !new_str ~pattern:r with
        | Some i when not !found ->
            new_str := String.prefix !new_str i ^ f ^ String.drop_prefix !new_str (i + String.length r);
            found := true
        | _ -> ()
      );
      if !found then reduce !new_str (steps + 1) else Int.max_value
  in
  reduce m 0 |> Int.to_string
