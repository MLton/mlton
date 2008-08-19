(* Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature REMOVE_UNUSED2_STRUCTS = 
   sig
      include SHRINK2
   end

signature REMOVE_UNUSED2 = 
   sig
      include REMOVE_UNUSED2_STRUCTS

      val remove: Program.t -> Program.t
   end
