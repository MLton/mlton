(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Pointer: POINTER =
struct

datatype 'a t = T of 'a option ref

fun !(T r) =
   case Ref.! r of
      NONE => Error.bug "Pointer.!"
    | SOME v => v

fun (T r) := v = Ref.:=(r, SOME v)

fun clear(T r) = Ref.:=(r, NONE)

fun copy(T r, T r') = Ref.:=(r, Ref.! r')

fun eq(T r, T r') = Ref.equals(r, r')

fun follow(T r) = Ref.! r

fun equals(p, p', equals) =
   case (follow p, follow p') of
      (NONE, NONE) => true
    | (SOME v, SOME v') => equals(v, v')
    | _ => false

fun isNull p = Option.isNone(follow p)

fun make v = T(ref v)

fun new v = make(SOME v)

fun null() = make NONE

fun swap(T p, T p') = Ref.swap(p, p')

end
