(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature REDUNDANT_TESTS_STRUCTS = 
   sig
      include SHRINK
   end

signature REDUNDANT_TESTS = 
   sig
      include REDUNDANT_TESTS_STRUCTS
      
      val simplify: Program.t -> Program.t
   end
