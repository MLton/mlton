(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature RESTORE_R_STRUCTS =
   sig
      include RSSA
   end

signature RESTORE_R =
   sig
      include RESTORE_R_STRUCTS

      val restoreFunction:
         {main: Function.t} -> Function.t -> Function.t
      val restore: Program.t -> Program.t
   end

