open Base
open Aoc_lib.Helpers

let day = 23

type instr =
  | HLF of int
  | TPL of int 
  | INC of int
  | JMP of int
  | JIE of int * int
  | JIO of int * int

type t = instr array 

let parse_input (input: string): t =
  let r_to_i = function | "a" | "a," -> 0 | "b" | "b,"-> 1 | _ -> failwith "Invalid register" in
  String.split_lines input |> List.map ~f:(fun line ->
    match String.split ~on:' ' line with
    | ["hlf"; r] -> HLF (r_to_i r)
    | ["tpl"; r] -> TPL (r_to_i r)
    | ["inc"; r] -> INC (r_to_i r)
    | ["jmp"; offset] -> JMP (Int.of_string offset)
    | ["jie"; r; offset] -> JIE (r_to_i r, Int.of_string offset)
    | ["jio"; r; offset] -> JIO (r_to_i r, Int.of_string offset)
    | _ -> failwith "Parse Error"
  )
  |> Array.of_list

let part1 (program: t): string =
  let regs = [|0; 0|] in
  let pc = ref 0 in 
  while !pc < Array.length program do
    match program.(!pc) with
    | HLF r -> regs.(r) <- regs.(r) / 2; pc := !pc + 1
    | TPL r -> regs.(r) <- regs.(r) * 3; pc := !pc + 1
    | INC r -> regs.(r) <- regs.(r) + 1; pc := !pc + 1
    | JMP offset -> pc := !pc + offset
    | JIE (r, offset) -> if regs.(r) % 2 = 0 then pc := !pc + offset else pc := !pc + 1
    | JIO (r, offset) -> if regs.(r) = 1 then pc := !pc + offset else pc := !pc + 1
  done;
  Int.to_string regs.(1)


let part2 (program: t): string =
  let regs = [|1; 0|] in
  let pc = ref 0 in 
  while !pc < Array.length program do
    match program.(!pc) with
    | HLF r -> regs.(r) <- regs.(r) / 2; pc := !pc + 1
    | TPL r -> regs.(r) <- regs.(r) * 3; pc := !pc + 1
    | INC r -> regs.(r) <- regs.(r) + 1; pc := !pc + 1
    | JMP offset -> pc := !pc + offset
    | JIE (r, offset) -> if regs.(r) % 2 = 0 then pc := !pc + offset else pc := !pc + 1
    | JIO (r, offset) -> if regs.(r) = 1 then pc := !pc + offset else pc := !pc + 1
  done;
  Int.to_string regs.(1)
