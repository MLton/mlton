(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
   
signature REPRESENTATION_STRUCTS = 
   sig
      structure Rssa: RSSA
      structure Ssa: SSA2
      sharing Rssa.RealSize = Ssa.RealSize
      sharing Rssa.WordSize = Ssa.WordSize
   end

signature REPRESENTATION = 
   sig
      include REPRESENTATION_STRUCTS

      val compute:
	 Ssa.Program.t
	 -> {diagnostic: unit -> unit,
	     genCase: {cases: (Ssa.Con.t * Rssa.Label.t) vector,
		       default: Rssa.Label.t option,
		       test: unit -> Rssa.Operand.t,
		       tycon: Ssa.Tycon.t} -> (Rssa.Statement.t list
					       * Rssa.Transfer.t
					       * Rssa.Block.t list),
	     object: {args: 'a vector,
		      con: Ssa.Con.t option,
		      dst: Rssa.Var.t * Rssa.Type.t,
		      objectTy: Ssa.Type.t,
		      oper: 'a -> Rssa.Operand.t} -> Rssa.Statement.t list,
	     objectTypes: (Rssa.PointerTycon.t * Rssa.ObjectType.t) vector,
	     select: {dst: Rssa.Var.t * Rssa.Type.t,
		      object: Rssa.Operand.t,
		      objectTy: Ssa.Type.t,
		      offset: int} -> Rssa.Statement.t list,
	     toRtype: Ssa.Type.t -> Rssa.Type.t option,
	     update: {object: Rssa.Operand.t,
		      objectTy: Ssa.Type.t,
		      offset: int,
		      value: Rssa.Operand.t} -> Rssa.Statement.t list,
	     vectorSub: {dst: Rssa.Var.t * Rssa.Type.t,
			 index: Rssa.Operand.t,
			 offset: int,
			 vector: Rssa.Operand.t,
			 vectorTy: Ssa.Type.t} -> Rssa.Statement.t list,
	     vectorUpdate: {index: Rssa.Operand.t,
			    offset: int,
			    value: Rssa.Operand.t,
			    vector: Rssa.Operand.t,
			    vectorTy: Ssa.Type.t} -> Rssa.Statement.t list}
   end
