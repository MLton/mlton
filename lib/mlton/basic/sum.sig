(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature SUM_STRUCTS =
   sig
      structure X: T
      structure Y: T
   end

signature SUM =
   sig
      include SUM_STRUCTS

      datatype t =
         X of X.t
       | Y of Y.t

      val equals: t * t -> bool
      val layout: t -> Layout.t
      val map: t * (X.t -> 'a) * (Y.t -> 'a) -> 'a
      val outX: t -> X.t
      val outY: t -> Y.t
   end
