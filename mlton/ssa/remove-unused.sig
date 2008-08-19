(* Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature REMOVE_UNUSED_STRUCTS = 
   sig
      include SHRINK
   end

signature REMOVE_UNUSED = 
   sig
      include REMOVE_UNUSED_STRUCTS

      val remove: Program.t -> Program.t
   end
