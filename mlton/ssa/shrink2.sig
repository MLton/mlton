(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature SHRINK2_STRUCTS = 
   sig
      include TYPE_CHECK2
   end

signature SHRINK2 = 
   sig
      include SHRINK2_STRUCTS

      val eliminateDeadBlocksFunction: Function.t -> Function.t
      val eliminateDeadBlocks: Program.t -> Program.t
      val shrinkFunction:
	 {globals: Statement.t vector} -> Function.t -> Function.t
      val shrink: Program.t -> Program.t
   end
