(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
