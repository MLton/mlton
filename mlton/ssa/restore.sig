(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature RESTORE_STRUCTS = 
   sig
      include SHRINK
   end

signature RESTORE = 
   sig
      include RESTORE_STRUCTS

      val restoreFunction: 
         {globals: Statement.t vector} -> Function.t -> Function.t
      val restore: Program.t -> Program.t
   end
