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
      structure Ssa: SSA
      sharing Rssa.IntSize = Ssa.IntSize
      sharing Rssa.RealSize = Ssa.RealSize
      sharing Rssa.WordSize = Ssa.WordSize
   end

signature REPRESENTATION = 
   sig
      include REPRESENTATION_STRUCTS

      structure TupleRep:
	 sig
	    type t

	    val layout: t -> Layout.t
	    val tycon: t -> Rssa.PointerTycon.t
	 end

      val compute:
	 Ssa.Program.t
	 -> {conApp: {args: 'a vector,
		      con: Ssa.Con.t,
		      dst: unit -> Rssa.Var.t,
		      oper: 'a -> Rssa.Operand.t,
		      ty: unit -> Rssa.Type.t} -> Rssa.Statement.t list,
	     genCase: {cases: (Ssa.Con.t * Rssa.Label.t) vector,
		       default: Rssa.Label.t option,
		       test: unit -> Rssa.Operand.t,
		       tycon: Ssa.Tycon.t} -> (Rssa.Statement.t list
					       * Rssa.Transfer.t
					       * Rssa.Block.t list),
	     objectTypes: Rssa.ObjectType.t vector,
	     reff: {arg: unit -> Rssa.Operand.t,
		    dst: Rssa.Var.t,
		    ty: Ssa.Type.t} -> Rssa.Statement.t list,
	     select: {dst: unit -> Rssa.Var.t,
		      offset: int,
		      tuple: unit -> Rssa.Operand.t,
		      tupleTy: Ssa.Type.t} -> Rssa.Statement.t list,
	     toRtype: Ssa.Type.t -> Rssa.Type.t option,
	     tuple: {components: 'a vector,
		     dst: Rssa.Var.t * Ssa.Type.t,
		     oper: 'a -> Rssa.Operand.t} -> Rssa.Statement.t list}
   end
