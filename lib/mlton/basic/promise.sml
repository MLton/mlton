(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Promise: PROMISE =
struct

datatype 'a t = T of 'a state ref
and 'a state =
   Unevaluated of unit -> 'a
 | Evaluating
 | Evaluated of 'a

fun layout l (T r) =
  let
     open Layout
  in
     case !r of
        Unevaluated _ => str "Unevaluated"
      | Evaluating => str "Evaluating"
      | Evaluated x => seq [str "Evaluated ", l x]
  end

fun delay th = T (ref (Unevaluated th))

fun reset (T r, th) =
   case !r of
      Evaluating => Error.bug "Promise.reset"
    | _ => r := Unevaluated th

exception Force
fun force (T r) =
   case !r of
      Evaluated x => x
    | Unevaluated th =>
         (let
             val _ = r := Evaluating
             val x = th ()
             val _ = r := Evaluated x
          in
             x
          end handle exn => (r := Unevaluated th; raise exn))
    | Evaluating => raise Force

fun lazy th =
   let val p = delay th
   in fn () => force p
   end

fun isUnevaluated (T r) =
   case !r of
      Unevaluated _ => true
    | _ => false

end
