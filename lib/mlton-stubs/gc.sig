(* Copyright (C) 2002-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature MLTON_GC =
   sig
      val collect: unit -> unit
      val pack: unit -> unit
      val setMessages: bool -> unit
      val setSummary: bool -> unit
      val unpack: unit -> unit
   end
