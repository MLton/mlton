(* Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature DEEP_FLATTEN_STRUCTS = 
   sig
      include SHRINK2
   end

signature DEEP_FLATTEN = 
   sig
      include DEEP_FLATTEN_STRUCTS

      val flatten: Program.t -> Program.t
   end
