(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor Mtype (S: MTYPE_STRUCTS): MTYPE = 
struct

open S

datatype dest =
   Char
 | Double
 | Int
 | Pointer
 | Uint

datatype t = T of {dest: dest}

fun dest (T {dest, ...}) = dest

fun toString t =
   case dest t of
      Char => "Char"
    | Double => "Double"
    | Int => "Int"
    | Pointer => "Pointer"
    | Uint => "Word"

val layout = Layout.str o toString

fun equals (t, t') = dest t = dest t'

val equals =
   Trace.trace2 ("Runtime.Type.equals", layout, layout, Bool.layout) equals

local
   fun new dest = T {dest = dest}
in
   val char = new Char
   val double = new Double
   val int = new Int
   val pointer = new Pointer
   val uint = new Uint
end

val all = [char, double, int, pointer, uint]

fun memo f =
   let val all = List.revMap (all, fn t => (t, f t))
   in fn t => #2 (valOf (List.peek (all, fn (t', _) => equals (t, t'))))
   end

val bool = int
val label = uint
val word = uint
  
fun isPointer t =
   case dest t of
      Pointer => true
    | _ => false
	 
local
   val byte: int = 1
   val word: int = 4
   val double: int = 8
in
   fun size t =
      case dest t of
	 Char => byte
       | Double => double
       | Int => word
       | Pointer => word
       | Uint => word
end

fun name t =
   case dest t of
      Char => "C"
    | Double => "D"
    | Int => "I"
    | Pointer => "P"
    | Uint => "U"

local
   fun align a b =
      let
	 open Word
	 val a = fromInt a - 0w1
      in
	 toInt (andb (notb a, a + fromInt b))
      end
in
   val align4 = align 4
   val align8 = align 8
end

fun align (ty: t, n: int): int =
   case dest ty of
      Char => n
    | Double => align8 n
    | Int => align4 n
    | Pointer => align4 n
    | Uint => align4 n

end
