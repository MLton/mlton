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

structure C = HashId (structure AstId = AstId
		     val noname = "C")
open C
structure P = PrimCons (C)
open P

val fromAst = newString o AstId.toString
   
fun fromAsts l = List.map (l, fromAst)

fun toAst id = AstId.fromString (toString id, Region.bogus)
   
fun toAsts l = List.map (l, toAst)

val all = [cons, falsee, nill, reff, truee, bind, match]

fun stats () =
   let open Layout
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
