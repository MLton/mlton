(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor Field (S: FIELD_STRUCTS): FIELD = 
struct

open S

datatype t =
   String of string
 | Int of int
   
val fromString = String
val fromInt = Int

val equals =
   fn (String s, String s') => String.equals (s, s')
    | (Int n, Int n') => Int.equals (n, n')
    | _ => false

val toString =
   fn String s => s
    | Int n => Int.toString (n + 1)
	 
fun layout f =
   case f of
      String s => Layout.str s
    | Int n => Int.layout (n + 1)
	 
val op <= =
   fn (String s, String s') => String.<= (s, s')
    | (Int n, Int n') => Int.<= (n, n')
    | (String _, Int _) => false
    | (Int _, String _) => true

end
