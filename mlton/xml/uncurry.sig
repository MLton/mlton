(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature UNCURRY_STRUCTS = 
   sig
      include SHRINK
   end

signature UNCURRY = 
   sig
      include UNCURRY_STRUCTS

      val uncurry: Program.t -> Program.t
   end
