(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Tycon (S: TYCON_STRUCTS): TYCON = 
struct

open S

structure Id = HashId (structure AstId = AstId
		       val noname = "t")
open Id

structure P = PrimTycons (Id)
open P

fun stats () =
   let open Layout
   in
      align
      (List.map (prims, fn c =>
		 seq [layout c, str " size is ",
		      Int.layout (MLton.size c),
		      str " plist length is ",
		      Int.layout (PropertyList.length (plist c))]))
   end

val _ =
   Trace.trace2 ("Tycon.equals", layout, layout, Bool.layout) equals

end
