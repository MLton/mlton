(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor Con (S: CON_STRUCTS): CON = 
struct

open S

structure C = HashId (val noname = "C")
open C

structure P = PrimCons (C)
open P

val all = [cons, falsee, nill, reff, truee, bind, match]

fun stats () =
   let
      open Layout
   in
      align
      (List.map (all, fn c =>
		 seq [layout c, str " size is ",
		      Int.layout (MLton.size c),
		      str " plist length is ",
		      Int.layout (PropertyList.length (plist c))]))
   end
   
fun clearPrimitive () = List.foreach (all, clear)

fun fromBool b = if b then truee else falsee
   
end
