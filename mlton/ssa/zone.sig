(* Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature ZONE_STRUCTS = 
   sig
      include TYPE_CHECK2
   end

signature ZONE = 
   sig
      include ZONE_STRUCTS

      val zone: Program.t -> Program.t
   end
