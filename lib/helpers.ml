open Base

(** Some useful composition stuff *)
let ( << ) f g x = f (g x)

let ( >> ) f g x = g (f x)

let ( $ ) f x = f x

(** Some useful reexports *)
let const = Fn.const

(** exn helper *)
let exn_opt f x =
  try Some (f x)
  with _ -> None

(** tuples **)
let fst3 (a, _, _) = a
let snd3 (_, b, _) = b
let thrd3 (_, _, c) = c

let fst4 (a, _, _, _) = a
let snd4 (_, b, _, _) = b
let thrd4 (_, _, c, _) = c
let frth4 (_, _, _, d) = d

let positive_ints = Sequence.unfold ~init:0 ~f:(fun n -> Some (n, n + 1))
