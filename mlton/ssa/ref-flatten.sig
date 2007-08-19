(* Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature REF_FLATTEN_STRUCTS = 
   sig
      include SHRINK2
   end

signature REF_FLATTEN = 
   sig
      include REF_FLATTEN_STRUCTS

      val flatten: Program.t -> Program.t
   end
