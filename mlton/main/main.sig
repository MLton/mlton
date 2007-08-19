(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
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
      val exportNJ: File.t -> unit
   end
