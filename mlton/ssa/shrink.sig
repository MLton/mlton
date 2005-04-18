(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature SHRINK_STRUCTS = 
   sig
      include PREPASSES
   end

signature SHRINK = 
   sig
      include SHRINK_STRUCTS

      val shrinkFunction: 
	 {globals: Statement.t vector} -> Function.t -> Function.t
      val shrink: Program.t -> Program.t
   end
