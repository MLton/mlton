(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
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

local
val unsetFnRef : (Layout.t -> Layout.t) ref = ref (fn _ => Error.bug "Tycon.unsetFnRef")
in
val {destroy = resetLayoutPretty: unit -> unit,
     get = layoutPretty: t -> Layout.t,
     set = setLayoutPretty: t * Layout.t -> unit} =
   Property.destGetSet
   (plist, Property.initFun (fn c => !unsetFnRef (layout c)))
val resetLayoutPretty = fn {unset} =>
   (unsetFnRef := unset ; resetLayoutPretty ())
end

structure P = PrimTycons (structure AdmitsEquality = AdmitsEquality
                          structure CharSize = CharSize
                          structure IntSize = IntSize
                          structure Kind = Kind
                          structure RealSize = RealSize
                          structure WordSize = WordSize
                          open Id
                          val layoutPretty = layoutPretty)
open P

fun stats () =
   let
      open Layout
   in
      align
      (List.map (prims, fn {tycon = c, ...} =>
                 seq [layout c, str " size is ",
                      Int.layout (MLton.size c),
                      str " plist length is ",
                      Int.layout (PropertyList.length (plist c))]))
   end
(* quell unused warning *)
val _ = stats

end
