(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature AST_STRUCTS =
   sig
      include AST_ATOMS_STRUCTS
   end

signature AST =
   sig
      include AST_CORE

      structure Sigexp:
	 sig
	    type spec

	    type t
	    datatype node =
	       Spec of spec
	     | Var of Sigid.t
             | Where of t * {tyvars: Tyvar.t vector,
			     longtycon: Longtycon.t,
			     ty: Type.t} list

	    include WRAPPED sharing type node' = node
			    sharing type obj = t

            val var: Sigid.t -> t
	    val wheree: t * {tyvars: Tyvar.t vector,
			     longtycon: Longtycon.t,
			     ty: Type.t} list * Region.t -> t
	    val spec: spec -> t

	    val layout: t -> Layout.t
	 end

      structure SigConst:
	 sig
	    datatype t =
	       None
	     | Opaque of Sigexp.t
	     | Transparent of Sigexp.t
	 end

      structure Equation:
	 sig
	    type t
	    datatype node =
	       Structure of Longstrid.t list
	     | Type of Longtycon.t list
	    include WRAPPED sharing type node' = node
			    sharing type obj = t
	 end

      structure Spec:
	 sig
	    type t
	    datatype node =
	       Datatype of DatatypeRhs.t
	     | Eqtype of {tycon: Tycon.t,
			  tyvars: Tyvar.t vector} list
	     | Empty
	     | Exception of (Con.t * Type.t option) list
	     | IncludeSigexp of Sigexp.t
	     | IncludeSigids of Sigid.t list
	     | Seq of t * t
	     | Sharing of {equations: Equation.t list,
			   spec: t}
	     | Structure of (Strid.t * Sigexp.t) list
	     | Type of {tycon: Tycon.t,
			tyvars: Tyvar.t vector} list
	     | TypeDefs of TypBind.t
	     | Val of (Var.t * Type.t) list

	    include WRAPPED sharing type node' = node
			    sharing type obj = t

	    val layout: t -> Layout.t
	 end
      sharing type Spec.t = Sigexp.spec

      structure Strexp:
	 sig
	    type strdec

	    type t
	    datatype node =
	       App of Fctid.t * t
             | Constrained of t * SigConst.t
	     | Let of strdec * t
	     | Struct of strdec
	     | Var of Longstrid.t

	    include WRAPPED sharing type node' = node
			    sharing type obj = t

            val var: Longstrid.t -> t
	    val structt: strdec -> t
	    val constrained: t * SigConst.t -> t
	    val app: Fctid.t * t -> t
	    val lett: strdec * t -> t
	       
	    val layout: t -> Layout.t
	 end

      structure Strdec:
	 sig
	    type t
	    datatype node =
	       Core of Dec.t
	     | Local of t * t
	     | Seq of t list
	     | Structure of {name: Strid.t,
			     def: Strexp.t,
			     constraint: SigConst.t} vector

	    include WRAPPED sharing type node' = node
			    sharing type obj = t

            val coalesce: t -> t
            val core: Dec.t -> t
	    val layout: t -> Layout.t
	    val locall: t * t -> t
	    val openn: Longstrid.t vector -> t
	    val seq: t list -> t
            val structuree: {name: Strid.t,
			     def: Strexp.t,
			     constraint: SigConst.t} vector -> t
	 end
      sharing type Strdec.t = Strexp.strdec

      structure FctArg:
	 sig
	    type t
	    datatype node =
	       Structure of Strid.t * Sigexp.t
	     | Spec of Spec.t
	    include WRAPPED sharing type node' = node
			    sharing type obj = t
	 end
      
      structure Topdec:
	 sig
	    type t
	    datatype node =
	       BasisDone of {ffi: Longstrid.t}
	     | Functor of {arg: FctArg.t,
			   body: Strexp.t,
			   name: Fctid.t,
			   result: SigConst.t} vector
	     | Signature of (Sigid.t * Sigexp.t) vector
	     | Strdec of Strdec.t

	    include WRAPPED sharing type node' = node
			    sharing type obj = t

            val fromExp: Exp.t -> t
	    val functorr: {name: Fctid.t,
			   arg: FctArg.t,
			   result: SigConst.t,
			   body: Strexp.t} vector -> t
	    val layout: t -> Layout.t
	    val signaturee: (Sigid.t * Sigexp.t) vector -> t
            val strdec: Strdec.t -> t
	 end

      structure Program:
	 sig
	    datatype t = T of Topdec.t list list

	    val append: t * t -> t
	    val coalesce: t -> t
	    val empty: t
	    val size: t -> int
	    val layout: t -> Layout.t
	 end
   end
