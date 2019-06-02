(* Copyright (C) 2009,2017,2019 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature RETURN_STRUCTS =
   sig
      structure Label: LABEL
      structure Handler: HANDLER
      sharing Label = Handler.Label
   end

signature RETURN =
   sig
      include RETURN_STRUCTS

      datatype t =
         Dead
       | NonTail of {cont: Label.t,
                     handler: Handler.t}
       | Tail

      val compose: t * t -> t
      val equals: t * t -> bool
      val foldLabel: t * 'a * (Label.t * 'a -> 'a) -> 'a
      val foreachHandler: t * (Label.t -> unit) -> unit
      val foreachLabel: t * (Label.t -> unit) -> unit
      val hash: t -> word
      val layout: t -> Layout.t
      val map: t * (Label.t -> Label.t) -> t
   end
