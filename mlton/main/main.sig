(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

type int = Int.t

signature MAIN_STRUCTS =
   sig
   end

signature MAIN =
   sig
      include MAIN_STRUCTS
	 
      val commandLine: string list -> OS.Process.status
      val exportMLton: unit -> unit
      val exportNJ: Dir.t * File.t -> unit

      val doit: unit -> unit
   end
