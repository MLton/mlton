(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor Tycon (S: TYCON_STRUCTS): TYCON = 
struct

open S

structure Id = Id (structure Symbol = Symbol
		   val noname = "t")
open Id

structure AdmitsEquality = AdmitsEquality ()
structure Kind = TyconKind ()
   
structure P = PrimTycons (structure AdmitsEquality = AdmitsEquality
			  structure IntSize = IntSize
			  structure Kind = Kind
			  structure RealSize = RealSize
			  structure WordSize = WordSize
			  open Id)
open P

val setPrintName =
   Trace.trace2 ("Tycon.setPrintName", layout, String.layout, Unit.layout)
   setPrintName

fun stats () =
   let
      open Layout
   in
      align
      (List.map (prims, fn (c, _, _) =>
		 seq [layout c, str " size is ",
		      Int.layout (MLton.size c),
		      str " plist length is ",
		      Int.layout (PropertyList.length (plist c))]))
   end

end
