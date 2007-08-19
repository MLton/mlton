(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CONSTANT_PROPAGATION_STRUCTS = 
   sig
      include SHRINK
   end

signature CONSTANT_PROPAGATION = 
   sig
      include CONSTANT_PROPAGATION_STRUCTS

      val simplify: Program.t -> Program.t
   end
