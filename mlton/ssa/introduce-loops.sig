(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature INTRODUCE_LOOPS_STRUCTS = 
   sig
      include SHRINK
   end

signature INTRODUCE_LOOPS = 
   sig
      include INTRODUCE_LOOPS_STRUCTS
      
      val introduceLoops: Program.t -> Program.t
   end
