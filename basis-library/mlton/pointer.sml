(* Copyright (C) 2003-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

structure MLtonPointer: MLTON_POINTER =
struct

open Primitive.Pointer

fun add (p, t) = fromWord (Word.+ (toWord p, t))
fun compare (p, p') = Word.compare (toWord p, toWord p')
fun diff (p, p') = Word.- (toWord p, toWord p')
fun sub (p, t) = fromWord (Word.- (toWord p, t))
   
end
