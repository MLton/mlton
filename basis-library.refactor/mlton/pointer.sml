(* Copyright (C) 2003-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonPointer: MLTON_POINTER =
struct

open Primitive.Pointer

fun add (p, t) = fromWord (Word.+ (toWord p, t))
fun compare (p, p') = Word.compare (toWord p, toWord p')
fun diff (p, p') = Word.- (toWord p, toWord p')
fun sub (p, t) = fromWord (Word.- (toWord p, t))
   
end
