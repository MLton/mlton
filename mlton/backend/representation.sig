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
      structure Ssa: SSA
      structure Rssa: RSSA
   end

signature REPRESENTATION = 
   sig
      include REPRESENTATION_STRUCTS

      structure TyconRep:
	 sig
	    datatype t =
	     (* Datatype has no representation (Void) or contains a single
	      * variant, and hence constructor requires no additional
	      * representation.
	      *) 
	       Direct
	     (* All cons are non-value-carrying and are represented as ints. *)
	     | Enum
	     (* All cons except for one are non-value-carrying and are
	      * represented as ints that are nonzero mod 4.  The value carrying
	      * con is represented transparently, i.e. the value is known to be a
	      * pointer and is left as such.
	      *)
	     | EnumDirect
	     (* All cons except for one are non-value-carrying and are
	      * represented as ints that are nonzero mod 4.  The value carrying
	      * con is represented by boxing its arg.
	      *)
	     | EnumIndirect
	     (* Non-value-carrying and are represented as ints that are nonzero
	      * mod 4.  Value carrying cons are represented by boxing the args
	      * and adding an integer tag.
	      *)
	     | EnumIndirectTag
	     (* All cons are value carrying and are represented by boxing the
	      * args and adding an integer tag.
	      *)
	     | IndirectTag
	     | Void
	 end

      structure TupleRep:
	 sig
	    datatype t = T of {offsets: {offset: int,
					 ty: Rssa.Type.t} option vector,
			       size: int,
			       ty: Rssa.Type.t,
			       tycon: Rssa.PointerTycon.t}

	    val layout: t -> Layout.t
	    val tycon: t -> Rssa.PointerTycon.t
	 end

      (* How a constructor variant of a datatype is represented. *)
      structure ConRep:
	 sig
	    datatype t =
	     (* an integer representing a variant in a datatype *)
	       IntAsTy of {int: int,
			   ty: Rssa.Type.t}
	     (* box the arg(s) and add the integer tag as the first word *)
	     | TagTuple of {rep: TupleRep.t,
			    tag: int}
	     (* just keep the value itself *)
	     | Transparent of Rssa.Type.t
	     (* box the arg(s) *)
	     | Tuple of TupleRep.t
	     (* need no representation *)
	     | Void

	    val layout: t -> Layout.t
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
