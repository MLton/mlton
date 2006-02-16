(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature TYPE_CHECK2_STRUCTS = 
   sig
      include ANALYZE2
   end

signature TYPE_CHECK2 = 
   sig
      include TYPE_CHECK2_STRUCTS
      
      val typeCheck: Program.t -> unit
   end
