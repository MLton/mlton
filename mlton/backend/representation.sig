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
	    val select:
	       t * {dst: unit -> Rssa.Var.t,
		    offset: int,
		    tuple: unit -> Rssa.Operand.t} -> Rssa.Statement.t list
	    val tuple:
	       t * {components: 'a vector,
		    dst: Rssa.Var.t,
		    oper: 'a -> Rssa.Operand.t} -> Rssa.Statement.t list
	    val tycon: t -> Rssa.PointerTycon.t
	 end

      (* How a constructor variant of a datatype is represented. *)
      structure ConRep:
	 sig
	    type t

	    val con: t * {args: 'a vector,
			  dst: unit -> Rssa.Var.t,
			  oper: 'a -> Rssa.Operand.t,
			  ty: unit -> Rssa.Type.t} -> Rssa.Statement.t list
	    val layout: t -> Layout.t
	 end

      structure TyconRep:
	 sig
	    type t

	    val genCase:
	       t * {cases: (ConRep.t * Rssa.Label.t) vector,
		    default: Rssa.Label.t option,
		    test: unit -> Rssa.Operand.t}
	       -> (Rssa.Statement.t list
		   * Rssa.Transfer.t
		   * Rssa.Block.t list)
	 end

      val compute:
	 Ssa.Program.t
	 -> {
	     conRep: Ssa.Con.t -> ConRep.t,
	     objectTypes: Rssa.ObjectType.t vector,
	     refRep: Ssa.Type.t -> TupleRep.t,
	     toRtype: Ssa.Type.t -> Rssa.Type.t option,
	     tupleRep: Ssa.Type.t -> TupleRep.t,
	     tyconRep: Ssa.Tycon.t -> TyconRep.t
	    }
   end
