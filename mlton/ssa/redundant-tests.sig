(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
