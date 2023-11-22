(* See https://github.com/MLton/mlton/issues/535. *)

datatype tree = Leaf of char | Node of tree * tree

fun nextis c cs =
  case TextIO.StreamIO.input1 (TextIO.getInstream cs) of
    NONE => false
  | SOME (c', cs') => c' = c

(* This version works. *)
(*
fun nextis c cs =
  case TextIO.lookahead cs of
    NONE => false
  | SOME c' => c' = c
*)

fun discard c cs =
  let val c' = valOf (TextIO.input1 cs) in
    if c = c'
    then ()
    else raise Fail ("unexpected character: " ^ Char.toString c' ^ ", expecting: " ^ Char.toString c)
  end

fun parse cs =
  case valOf (TextIO.input1 cs) of
   #"(" =>
      let
        fun loop l r =
          if nextis #")" cs
          then (discard #")" cs; Node (l, r))
          else loop (Node (l, r)) (parse cs)
      in
        loop (parse cs) (parse cs)
      end
  | c => Leaf c

val cs = TextIO.openString "(a(bc)d)"

(* with this it works too *)
(* val true = nextis #"(" cs *)

val t = (parse cs)
