(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
   
signature PRIM_STRUCTS = 
   sig
      structure CFunction: C_FUNCTION
      structure CType: C_TYPE
      structure Con: CON
      structure Const: CONST
      structure IntSize: INT_SIZE
      structure RealSize: REAL_SIZE
      structure Scheme: SCHEME
      structure Type: TYPE
      structure WordSize: WORD_SIZE
      sharing CFunction.CType = CType
      sharing IntSize = CType.IntSize = Const.IntX.IntSize = Type.Tycon.IntSize
      sharing RealSize = CType.RealSize = Const.RealX.RealSize
	 = Type.Tycon.RealSize
      sharing Type = Scheme.Type
      sharing WordSize = CType.WordSize = Const.WordX.WordSize
	 = Type.Tycon.WordSize
   end

signature PRIM = 
   sig
      include PRIM_STRUCTS

      structure Name:
	 sig
	    datatype t =
	       Array_array (* backend *)
	     | Array_array0Const (* constant propagation *)
	     | Array_length (* ssa to rssa *)
	     | Array_sub (* backend *)
	     | Array_toVector (* backend *)
	     | Array_update (* backend *)
	     | BuildConstant of string (* type inference *)
	     | C_CS_charArrayToWord8Array (* type inference *)
	     | Char_chr (* type inference *)
	     | Char_ge (* type inference *)
	     | Char_gt (* type inference *)
	     | Char_le (* type inference *)
	     | Char_lt (* type inference *)
	     | Char_ord (* type inference *)
	     | Char_toWord8 (* type inference *)
	     | Constant of string (* type inference *)
	     | Cpointer_isNull (* codegen *)
	     | Exn_extra (* implement exceptions *)
	     | Exn_keepHistory (* a compile-time boolean *)
	     | Exn_name (* implement exceptions *)
	     | Exn_setExtendExtra (* implement exceptions *)
	     | Exn_setInitExtra (* implement exceptions *)
	     | Exn_setTopLevelHandler (* implement exceptions *)
	     | FFI of CFunction.t (* ssa to rssa *)
	     | FFI_Symbol of {name: string,
			      ty: CType.t} (* codegen *)
	     | FFI_getPointer (* ssa to rssa *)
	     | FFI_setPointer (* ssa to rssa *)
	     | GC_collect (* ssa to rssa *)
	     | GC_pack (* ssa to rssa *)
	     | GC_unpack (* ssa to rssa *)
	     | Int_add of IntSize.t (* codegen *)
	     | Int_addCheck of IntSize.t (* codegen *)
	     | Int_ge of IntSize.t (* codegen *)
	     | Int_gt of IntSize.t (* codegen *)
	     | Int_le of IntSize.t (* codegen *)
	     | Int_lt of IntSize.t (* codegen *)
	     | Int_mul of IntSize.t (* codegen *)
	     | Int_mulCheck of IntSize.t (* codegen *)
	     | Int_neg of IntSize.t (* codegen *)
	     | Int_negCheck of IntSize.t (* codegen *)
	     | Int_quot of IntSize.t (* codegen *)
	     | Int_rem of IntSize.t (* codegen *)
	     | Int_sub of IntSize.t (* codegen *)
	     | Int_subCheck of IntSize.t (* codegen *)
	     | Int_toInt of IntSize.t * IntSize.t (* codegen *)
	     | Int_toReal of IntSize.t * RealSize.t (* codegen *)
	     | Int_toWord of IntSize.t * WordSize.t (* codegen *)
	     | IntInf_add (* ssa to rssa *)
	     | IntInf_andb (* ssa to rssa *)
	     | IntInf_arshift (* ssa to rssa *)
	     | IntInf_compare (* ssa to rssa *)
	     | IntInf_equal (* ssa to rssa *)
	     | IntInf_gcd (* ssa to rssa *)
	     | IntInf_lshift (* ssa to rssa *)
	     | IntInf_mul (* ssa to rssa *)
	     | IntInf_neg (* ssa to rssa *)
	     | IntInf_notb (* ssa to rssa *)
	     | IntInf_orb (* ssa to rssa *)
	     | IntInf_quot (* ssa to rssa *)
	     | IntInf_rem (* ssa to rssa *)
	     | IntInf_sub (* ssa to rssa *)
	     | IntInf_toString (* ssa to rssa *)
	     | IntInf_toVector (* ssa to rssa *)
	     | IntInf_toWord (* ssa to rssa *)
	     | IntInf_xorb (* ssa to rssa *)
	     | MLton_bogus (* ssa to rssa *)
	     (* of type unit -> 'a.
	      * Makes a bogus value of any type.
	      *)
	     | MLton_bug (* ssa to rssa *)
	     | MLton_deserialize (* unused *)
	     | MLton_eq (* codegen *)
	     | MLton_equal (* polymorphic equality *)
	     | MLton_halt (* ssa to rssa *)
	     (* MLton_handlesSignals and MLton_installSignalHandler work together
	      * to inform the optimizer and basis library whether or not the
	      * program uses signal handlers.
	      *
	      * MLton_installSignalHandler is called by MLton.Signal.setHandler,
	      * and is effectively a noop, but is left in the program until the
	      * end of the backend, so that the optimizer can test whether or
	      * not the program installs signal handlers.
	      *
	      * MLton_handlesSignals is translated by closure conversion into
	      * a boolean, and is true iff MLton_installsSignalHandler is called.
	      *)
	     | MLton_handlesSignals (* closure conversion *)
	     | MLton_installSignalHandler (* backend *)
	     | MLton_serialize (* unused *)
	     | MLton_size (* ssa to rssa *)
	     | MLton_touch (* backend *)
	     | Real_Math_acos of RealSize.t (* codegen *)
	     | Real_Math_asin of RealSize.t (* codegen *)
	     | Real_Math_atan of RealSize.t (* codegen *)
	     | Real_Math_atan2 of RealSize.t (* codegen *)
	     | Real_Math_cos of RealSize.t (* codegen *)
	     | Real_Math_exp of RealSize.t (* codegen *)
	     | Real_Math_ln of RealSize.t (* codegen *)
	     | Real_Math_log10 of RealSize.t  (* codegen *)
	     | Real_Math_sin of RealSize.t (* codegen *)
	     | Real_Math_sqrt of RealSize.t (* codegen *)
	     | Real_Math_tan of RealSize.t (* codegen *)
	     | Real_abs of RealSize.t (* codegen *)
	     | Real_add of RealSize.t (* codegen *)
	     | Real_div of RealSize.t (* codegen *)
	     | Real_equal of RealSize.t (* codegen *)
	     | Real_ge of RealSize.t (* codegen *)
	     | Real_gt of RealSize.t (* codegen *)
	     | Real_ldexp of RealSize.t (* codegen *)
	     | Real_le of RealSize.t (* codegen *)
	     | Real_lt of RealSize.t (* codegen *)
	     | Real_mul of RealSize.t (* codegen *)
	     | Real_muladd of RealSize.t (* codegen *)
	     | Real_mulsub of RealSize.t (* codegen *)
	     | Real_neg of RealSize.t	  (* codegen *)
	     | Real_qequal of RealSize.t (* codegen *)
	     | Real_round of RealSize.t (* codegen *)
	     | Real_sub of RealSize.t (* codegen *)
	     | Real_toInt of RealSize.t (* codegen *)
	     | Ref_assign (* backend *)
	     | Ref_deref (* backend *)
	     | Ref_ref (* backend *)
	     | String_toWord8Vector (* type inference *)
	     | Thread_atomicBegin (* backend *)
	     | Thread_atomicEnd (* backend *)
	     | Thread_canHandle (* backend *)
	     | Thread_copy (* ssa to rssa *)
	     | Thread_copyCurrent (* ssa to rssa *)
	     | Thread_returnToC (* codegen *)
	     (* switchTo has to be a _prim because we have to know that it
	      * enters the runtime -- because everything must be saved
	      * on the stack.
	      *)
	     | Thread_switchTo (* ssa to rssa *)
	     | Vector_length (* ssa to rssa *)
	     | Vector_sub (* backend *)
	     | Weak_canGet (* ssa to rssa *)
	     | Weak_get (* ssa to rssa *)
	     | Weak_new (* ssa to rssa *)
	     | Word_add of WordSize.t (* codegen *)
	     | Word_addCheck of WordSize.t (* codegen *)
	     | Word_andb of WordSize.t (* codegen *)
	     | Word_arshift of WordSize.t (* codegen *)
	     | Word_div of WordSize.t (* codegen *)
	     | Word_ge of WordSize.t (* codegen *)
	     | Word_gt of WordSize.t (* codegen *)
	     | Word_le of WordSize.t (* codegen *)
	     | Word_lshift of WordSize.t (* codegen *)
	     | Word_lt of WordSize.t (* codegen *)
	     | Word_mod of WordSize.t (* codegen *)
	     | Word_mul of WordSize.t (* codegen *)
	     | Word_mulCheck of WordSize.t (* codegen *)
	     | Word_neg of WordSize.t (* codegen *)
	     | Word_notb of WordSize.t (* codegen *)
	     | Word_orb of WordSize.t (* codegen *)
	     | Word_rol of WordSize.t (* codegen *)
	     | Word_ror of WordSize.t (* codegen *)
	     | Word_rshift of WordSize.t (* codegen *)
	     | Word_sub of WordSize.t (* codegen *)
	     | Word_toInt of WordSize.t * IntSize.t (* codegen *)
	     | Word_toIntInf (* ssa to rssa *)
	     | Word_toIntX of WordSize.t * IntSize.t (* codegen *)
	     | Word_toWord of WordSize.t * WordSize.t (* codegen *)
	     | Word_toWordX of WordSize.t * WordSize.t (* codegen *)
	     | Word_xorb of WordSize.t (* codegen *)
	     | WordVector_toIntInf (* ssa to rssa *)
	     | Word8_toChar (* type inference *)
	     | Word8Array_subWord (* ssa to rssa *)
	     | Word8Array_updateWord (* ssa to rssa *)
	     | Word8Vector_subWord (* ssa to rssa *)
	     | Word8Vector_toString (* type inference *)
	     | World_save (* ssa to rssa *)

	    val layout: t -> Layout.t
	    val toString: t -> string
	 end

      type t

      structure ApplyArg:
	 sig
	    datatype 'a t =
	       Con of {con: Con.t, hasArg: bool}
	     | Const of Const.t
	     | Var of 'a

	    val layout: ('a -> Layout.t) -> 'a t -> Layout.t
	 end
      structure ApplyResult:
	 sig
	    type prim
	    datatype 'a t =
	       Apply of prim * 'a list
	     | Bool of bool
	     | Const of Const.t
	     | Overflow
	     | Unknown
	     | Var of 'a

	    val layout: ('a -> Layout.t) -> 'a t -> Layout.t
	 end where type prim = t

      val allocTooLarge: t
      val apply: t * 'a ApplyArg.t list * ('a * 'a -> bool) -> 'a ApplyResult.t
      val array: t
      val assign: t
      val bogus: t
      val bug: t
      val buildConstant: string * Scheme.t -> t
      val checkApp: {
		     prim: t,
		     targs: 'a vector,
		     args: 'a vector,
		     con: Type.Tycon.t * 'a vector -> 'a,
		     equals: 'a * 'a -> bool,
		     dearrowOpt: 'a -> ('a * 'a) option,
		     detupleOpt: 'a -> 'a vector option,
		     isUnit: 'a -> bool
		     } -> 'a option
      val constant: string * Scheme.t -> t
      val deref: t
      val deserialize: t
      val eq: t    (* pointer equality *)
      val equal: t (* polymorphic equality *)
      val equals: t * t -> bool (* equality of names *)
      val extractTargs: {prim: t,
			 args: 'a vector,
			 result: 'a,
			 dearray: 'a -> 'a,
			 dearrow: 'a -> 'a * 'a,
			 deref: 'a -> 'a,
			 devector: 'a -> 'a,
			 deweak: 'a -> 'a} -> 'a vector
      val ffi: CFunction.t * Scheme.t -> t
      val ffiSymbol: {name: string, ty: CType.t} -> t
      val gcCollect: t
      val intInfEqual: t
      val intAdd: IntSize.t -> t
      val intAddCheck: IntSize.t -> t
      val intMul: IntSize.t -> t
      val intMulCheck: IntSize.t -> t
      val intSub: IntSize.t -> t
      val intSubCheck: IntSize.t -> t
      val intToWord: IntSize.t * WordSize.t -> t
      val isCommutative: t -> bool
      (*
       * isFunctional p = true iff p always returns same result when given
       *   same args and has no side effects.
       * isFuntional implies not maySideEffect.
       * examples: Array_length, MLton_equal, Int_add
       * not examples: Array_array, Array_sub, Ref_deref, Ref_ref
       *)
      val isFunctional: t -> bool
      val layout: t -> Layout.t
      val layoutApp: t * 'a vector * ('a -> Layout.t) -> Layout.t
      (* Int_addCheck, Int_mulCheck, Int_subCheck *)
      val mayOverflow: t -> bool
      val mayRaise: t -> bool
      (* examples: Array_update, Ref_assign
       * not examples: Array_array, Array_sub, Ref_deref, Ref_ref
       *)
      val maySideEffect: t -> bool
      val name: t -> Name.t
      val new: string * Scheme.t -> t
      val newNullary: CFunction.t -> t (* new of type unit -> unit *)
      val numArgs: t -> int option
      val reff: t
      val scheme: t -> Scheme.t
      val serialize: t
      val toString: t -> string
      val vectorLength: t
      val vectorSub: t
      val wordAdd: WordSize.t -> t
      val wordAddCheck: WordSize.t -> t
      val wordAndb: WordSize.t -> t
      val wordGe: WordSize.t -> t
      val wordGt: WordSize.t -> t
      val wordLe: WordSize.t -> t
      val wordLt: WordSize.t -> t
      val wordMul: WordSize.t -> t
      val wordMulCheck: WordSize.t -> t
      val wordRshift: WordSize.t -> t
      val wordSub: WordSize.t -> t
      val wordToInt: WordSize.t * IntSize.t -> t
      val wordToIntX: WordSize.t * IntSize.t -> t
   end
