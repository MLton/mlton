(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 2005-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Exn0 =
struct

type t = exn

val history = MLton.Exn.history

val name = General.exnName

val message = General.exnMessage

exception Bind = Bind
exception Match = Match
exception Overflow = Overflow
exception Subscript = Subscript

local
   (* would like to make the declaration of z in a let inside the try function,
    * with 'a as a free type variable.  But SML/NJ doesn't allow it.
    *)
   datatype 'a z = Ok of 'a | Raise of exn
in
   val try: (unit -> 'a) * ('a -> 'b) * (exn -> 'b) -> 'b =
      fn (t, k, h) =>
      case Ok (t ()) handle e => Raise e of
         Ok x => k x
       | Raise e => h e
end

fun finally (thunk, cleanup: unit -> unit) =
   try (thunk, fn a => (cleanup (); a), fn e => (cleanup (); raise e))

fun windFail (f: unit -> 'a, g: unit -> unit): 'a =
   f () handle ex => (g (); raise ex)

fun 'a withEscape (f: ('a -> 'b) -> 'a): 'a =
   let
      exception E of 'a
   in
      f (fn x => raise E x) handle E x => x
   end

end
