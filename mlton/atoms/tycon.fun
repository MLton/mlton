(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Tycon (S: TYCON_STRUCTS): TYCON = 
struct

open S

structure Id = Id (val noname = "t")
open Id

structure AdmitsEquality = AdmitsEquality ()
structure Kind = TyconKind ()
   
structure P = PrimTycons (structure AdmitsEquality = AdmitsEquality
                          structure CharSize = CharSize
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
