(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
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
   
val toAst = AstId.fromString o toString
   
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
