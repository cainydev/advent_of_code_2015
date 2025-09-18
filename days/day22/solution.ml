open Base
open Aoc_lib.Helpers

let day = 22
type t = (int * int) 

let parse_input (input: string): t =
  match String.split_lines input with
  | [hp; damage] ->
      let hp = String.split ~on:' ' hp |> List.last_exn |> Int.of_string in
      let damage = String.split ~on:' ' damage |> List.last_exn |> Int.of_string in
      (hp, damage)
  | _ -> failwith "Parse Error"

let part1 ((boss_hp, boss_dmg): t): string =
  let best = ref Int.max_value in
  let rec aux effects players_turn hp mana boss_hp boss_dmg spent =
    if spent >= !best then None else

    (* apply effects *)
    let mana, boss_hp, armor = List.fold effects ~init:(mana, boss_hp, 0)
      ~f:(fun (mana, boss_hp, armor) (_, a, d, im) -> (mana + im, boss_hp - d, armor + a))
    in

    
    (* detect boss slain *)
    if boss_hp <= 0 then (best := Int.min !best spent; Some spent) else

    (* decrement / discard effects *)
    let effects = List.filter_map ~f:(fun (turns, a, b, c) ->
      if turns <= 1 then None else Some (turns - 1, a, b, c)
    ) effects in
    
    (* turns *)
    if players_turn then
      List.filter_map ~f:Fn.id [
        if mana >= 53 then aux effects false hp (mana - 53) (boss_hp - 4) boss_dmg (spent + 53) else None;
        if mana >= 73 then aux effects false (hp + 2) (mana - 73) (boss_hp - 2) boss_dmg (spent + 73) else None;
        if mana >= 113 && not (List.exists ~f:(fun (_, a, _, _) -> a = 7) effects)
        then aux ((6, 7, 0, 0) :: effects) false hp (mana - 113) boss_hp boss_dmg (spent + 113) else None;
        if mana >= 173 && not (List.exists ~f:(fun (_, _, d, _) -> d = 3) effects)
        then aux ((6, 0, 3, 0) :: effects) false hp (mana - 173) boss_hp boss_dmg (spent + 173) else None;
        if mana >= 229 && not (List.exists ~f:(fun (_, _, _, im) -> im = 101) effects)
        then aux ((5, 0, 0, 101) :: effects) false hp (mana - 229) boss_hp boss_dmg (spent + 229) else None;
      ] |> List.min_elt ~compare:Int.compare
    else
      let dmg = Int.max (boss_dmg - armor) 1 in
      if (hp - dmg) <= 0 then None
      else aux effects true (hp - dmg) mana boss_hp boss_dmg spent
      
  in aux [] true 50 500 boss_hp boss_dmg 0 |> Option.value_exn |> Int.to_string

let part2 ((boss_hp, boss_dmg): t): string =
  let best = ref Int.max_value in
  let rec aux effects players_turn hp mana boss_hp boss_dmg spent =
    if spent >= !best then None else
    
    (* decrement hp *)
    let hp = if players_turn then hp - 1 else hp in
    if hp <= 0 then None else

    (* apply effects *)
    let mana, boss_hp, armor = List.fold effects ~init:(mana, boss_hp, 0)
      ~f:(fun (mana, boss_hp, armor) (_, a, d, im) -> (mana + im, boss_hp - d, armor + a))
    in

    
    (* detect boss slain *)
    if boss_hp <= 0 then (best := Int.min !best spent; Some spent) else

    (* decrement / discard effects *)
    let effects = List.filter_map ~f:(fun (turns, a, b, c) ->
      if turns <= 1 then None else Some (turns - 1, a, b, c)
    ) effects in
    
    (* turns *)
    if players_turn then
      List.filter_map ~f:Fn.id [
        if mana >= 53 then aux effects false hp (mana - 53) (boss_hp - 4) boss_dmg (spent + 53) else None;
        if mana >= 73 then aux effects false (hp + 2) (mana - 73) (boss_hp - 2) boss_dmg (spent + 73) else None;
        if mana >= 113 && not (List.exists ~f:(fun (_, a, _, _) -> a = 7) effects)
        then aux ((6, 7, 0, 0) :: effects) false hp (mana - 113) boss_hp boss_dmg (spent + 113) else None;
        if mana >= 173 && not (List.exists ~f:(fun (_, _, d, _) -> d = 3) effects)
        then aux ((6, 0, 3, 0) :: effects) false hp (mana - 173) boss_hp boss_dmg (spent + 173) else None;
        if mana >= 229 && not (List.exists ~f:(fun (_, _, _, im) -> im = 101) effects)
        then aux ((5, 0, 0, 101) :: effects) false hp (mana - 229) boss_hp boss_dmg (spent + 229) else None;
      ] |> List.min_elt ~compare:Int.compare
    else
      let dmg = Int.max (boss_dmg - armor) 1 in
      if (hp - dmg) <= 0 then None
      else aux effects true (hp - dmg) mana boss_hp boss_dmg spent
      
  in aux [] true 50 500 boss_hp boss_dmg 0 |> Option.value_exn |> Int.to_string
