(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Option: OPTION =
struct

datatype option = datatype option

exception Option

fun map f =
   fn NONE => NONE
    | SOME a => SOME (f a)

fun app f z = (ignore (map f z); ())

fun compose (f, g) c = map f (g c)

val join =
   fn NONE => NONE
    | SOME v => v

fun mapPartial f = join o (map f)

fun composePartial (f, g) = (mapPartial f) o g

fun filter f a = if f a then SOME a else NONE

fun getOpt (z, a) =
   case z of
      NONE => a
    | SOME v => v

val isSome =
   fn NONE => false
    | SOME _ => true

val valOf =
   fn NONE => raise Option
    | SOME v => v

end

structure OptionGlobal: OPTION_GLOBAL = Option
open OptionGlobal
