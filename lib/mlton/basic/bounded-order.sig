(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature BOUNDED_ORDER =
   sig
      structure O: ORDER
      include ORDER
      val inject: O.t -> t
      val project: t -> O.t
      val largest: t
      val smallest: t
   end
