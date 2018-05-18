(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MAIN_STRUCTS =
   sig
   end

signature MAIN =
   sig
      include MAIN_STRUCTS

      val main: string * string list -> OS.Process.status
      val mainWrapped: unit -> 'a
   end
