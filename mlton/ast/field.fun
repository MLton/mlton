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
   Int of int
 | Symbol of Symbol.t
   
val equals =
   fn (Int n, Int n') => Int.equals (n, n')
    | (Symbol s, Symbol s') => Symbol.equals (s, s')
    | _ => false

val toString =
   fn Int n => Int.toString (n + 1)
    | Symbol s => Symbol.toString s
	 
val layout = Layout.str o toString
	 
val op <= =
   fn (Int n, Int n') => Int.<= (n, n')
    | (Symbol s, Symbol s') => Symbol.<= (s, s')
    | (Symbol _, Int _) => false
    | (Int _, Symbol _) => true

end
