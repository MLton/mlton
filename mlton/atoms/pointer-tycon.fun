(* Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor PointerTycon (S: POINTER_TYCON_STRUCTS): POINTER_TYCON =
struct

open S

type int = Int.t
   
datatype t = T of {index: int}

local
   fun make f (T r) = f r
in
   val index = make #index
end

fun fromIndex i = T {index = i}
   
fun compare (p, p') = Int.compare (index p, index p')

fun equals (pt, pt') = index pt = index pt'

val op <= = fn (pt, pt') => index pt <= index pt'

fun toString (T {index, ...}) =
   concat ["pt_", Int.toString index]

val layout = Layout.str o toString

val c = Counter.new 0

fun new () = T {index = Counter.next c}

(* These basic pointer tycons are hardwired into the runtime and are
 * prefixed to every user program.  See gc.h for the definitions of
 * {STACK,STRING,THREAD,WEAK_GONE,WORD_VECTOR}_TYPE_INDEX.
 *)
val stack = new ()
val word8Vector = new ()
val thread = new ()
val weakGone = new ()
val wordVector = new ()

end
