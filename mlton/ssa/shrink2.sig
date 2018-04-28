(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature SHRINK2_STRUCTS = 
   sig
      include PREPASSES2
   end

signature SHRINK2 = 
   sig
      include SHRINK2_STRUCTS

      val shrinkFunction:
         {globals: Statement.t vector} -> Function.t -> Function.t
      val shrink: Program.t -> Program.t
   end
