(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
(*-------------------------------------------------------------------*)
(*                               Bool                                *)
(*-------------------------------------------------------------------*)

structure Bool: BOOL =
struct

open Pervasive.Bool

type t = bool

val compare =
   let open Relation
   in fn (false, false) => EQUAL
       | (false, true) => LESS
       | (true, false) => GREATER
       | (true, true) => EQUAL
   end

val equals =
   fn (true, true) => true
    | (false, false) => true
    | _ => false

val layout = Layout.str o toString
(*fun output(b, out) = Pervasive.IO.output(out, toString b)*)

end
