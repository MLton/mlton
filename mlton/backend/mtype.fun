(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
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

fun equals (t, t') = dest t = dest t'

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
   in fn t => #2 (List.lookup (all, fn (t', _) => equals (t, t')))
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

fun toString t =
   case dest t of
      Char => "uchar"
    | Double => "double"
    | Int => "int"
    | Pointer => "pointer"
    | Uint => "uint"

val layout = Layout.str o toString

fun doubleWordAlign (i: int): int =
   let open Word
   in toInt (andb (notb 0w7, (0w7 + fromInt i)))
   end
   
fun wordAlign (i: int): int =
   let open Word
   in toInt (andb (notb 0w3, (0w3 + fromInt i)))
   end

fun align (ty: t, n: int): int =
   case dest ty of
      Char => n
    | Double => doubleWordAlign n
    | Int => wordAlign n
    | Pointer => wordAlign n
    | Uint => wordAlign n

end
