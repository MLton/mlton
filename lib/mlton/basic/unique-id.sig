(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
   
signature UNIQUE_ID =
   sig
      type t

      val equals: t * t -> bool
      val layout: t -> Layout.t
      val new: unit -> t
      val toString: t -> string
   end
