(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
   
signature SSA_TO_RSSA_STRUCTS =
   sig
      structure Rssa: RSSA
      structure Ssa: SSA
      sharing Rssa.CFunction = Ssa.CFunction
      sharing Rssa.Const = Ssa.Const
      sharing Rssa.Func = Ssa.Func
      sharing Rssa.Label = Ssa.Label
      sharing Rssa.Prim = Ssa.Prim
      sharing Rssa.ProfileExp = Ssa.ProfileExp
      sharing Rssa.SourceInfo = Ssa.SourceInfo
      sharing Rssa.Var = Ssa.Var
   end

signature SSA_TO_RSSA =
   sig
      include SSA_TO_RSSA_STRUCTS
	 
      val convert: Ssa.Program.t -> Rssa.Program.t
   end
