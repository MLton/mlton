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
      structure Con: CON
      structure Const: CONST
      structure Scheme: SCHEME
      structure Type: TYPE
      sharing Type = Scheme.Type
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
	     | Array_update (* backend *)
	     | BuildConstant of string (* type inference *)
	     | Byte_byteToChar (* ssa to rssa *)
	     | Byte_charToByte (* ssa to rssa *)
	     | C_CS_charArrayToWord8Array (* ssa to rssa *)
	     | Char_lt (* codegen *)
	     | Char_le (* codegen *)
	     | Char_gt (* codegen *)
	     | Char_ge (* codegen *)
	     | Char_chr (* codegen *)
	     | Char_ord (* codegen *)
	     | Constant of string (* type inference *)
	     | Cpointer_isNull (* codegen *)
	     | Exn_extra (* implement exceptions *)
	     | Exn_keepHistory (* a compile-time boolean *)
	     | Exn_name (* implement exceptions *)
	     | Exn_setExtendExtra (* implement exceptions *)
	     | Exn_setInitExtra (* implement exceptions *)
	     | Exn_setTopLevelHandler (* implement exceptions *)
	     | FFI of string (* ssa to rssa *)
	     | GC_collect (* ssa to rssa *)
	     | GC_pack (* ssa to rssa *)
	     | GC_unpack (* ssa to rssa *)
             | Int_add (* codegen *)
             | Int_addCheck (* codegen *)
             | Int_ge (* codegen *)
             | Int_geu (* codegen *)
             | Int_gt (* codegen *)
             | Int_gtu (* codegen *)
             | Int_le (* codegen *)
             | Int_lt (* codegen *)
             | Int_mul (* codegen *)
             | Int_mulCheck (* codegen *)
             | Int_neg (* codegen *)
             | Int_negCheck (* codegen *)
             | Int_quot (* codegen *)
             | Int_rem (* codegen *)
             | Int_sub (* codegen *)
             | Int_subCheck (* codegen *)
	     | IntInf_add (* ssa to rssa *)
	     | IntInf_andb (* ssa to rssa *)
	     | IntInf_arshift (* ssa to rssa *)
	     | IntInf_compare (* ssa to rssa *)
	     | IntInf_equal (* ssa to rssa *)
	     | IntInf_fromVector (* ssa to rssa *)
	     | IntInf_fromWord (* ssa to rssa *)
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
	     | Real_Math_acos (* codegen *)
	     | Real_Math_asin (* codegen *)
	     | Real_Math_atan (* codegen *)
	     | Real_Math_atan2 (* codegen *)
	     | Real_Math_cos (* codegen *)
	     | Real_Math_cosh (* codegen *)
	     | Real_Math_exp (* codegen *)
	     | Real_Math_ln (* codegen *)
	     | Real_Math_log10  (* codegen *)
	     | Real_Math_pow (* codegen *)
	     | Real_Math_sin (* codegen *)
	     | Real_Math_sinh (* codegen *)
	     | Real_Math_sqrt (* codegen *)
	     | Real_Math_tan (* codegen *)
	     | Real_Math_tanh (* codegen *)
	     | Real_abs (* codegen *)
	     | Real_add (* codegen *)
	     | Real_copysign (* codegen *)
	     | Real_div (* codegen *)
	     | Real_equal (* codegen *)
	     | Real_frexp (* ssa to rssa *)
	     | Real_fromInt (* codegen *)
	     | Real_ge (* codegen *)
	     | Real_gt (* codegen *)
	     | Real_ldexp (* codegen *)
	     | Real_le (* codegen *)
	     | Real_lt (* codegen *)
	     | Real_modf (* ssa to rssa *)
	     | Real_mul (* codegen *)
	     | Real_muladd (* codegen *)
	     | Real_mulsub (* codegen *)
	     | Real_neg	  (* codegen *)
	     | Real_qequal (* codegen *)
	     | Real_round (* codegen *)
	     | Real_sub (* codegen *)
	     | Real_toInt (* codegen *)
	     | Ref_assign (* backend *)
	     | Ref_deref (* backend *)
	     | Ref_ref (* backend *)
	     | String_fromWord8Vector (* ssa to rssa *)
	     | String_toWord8Vector (* ssa to rssa *)
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
	     | Vector_fromArray (* backend *)
	     | Vector_length (* ssa to rssa *)
	     | Vector_sub (* backend *)
	     | Weak_canGet (* ssa to rssa *)
	     | Weak_get (* ssa to rssa *)
	     | Weak_new (* ssa to rssa *)
	     | Word32_add (* codegen *)
	     | Word32_addCheck (* codegen *)
	     | Word32_andb (* codegen *)
	     | Word32_arshift (* codegen *)
	     | Word32_div (* codegen *)
	     | Word32_fromInt (* ssa to rssa *)
	     | Word32_ge (* codegen *)
	     | Word32_gt (* codegen *)
	     | Word32_le (* codegen *)
	     | Word32_lshift (* codegen *)
	     | Word32_lt (* codegen *)
	     | Word32_mod (* codegen *)
	     | Word32_mul (* codegen *)
	     | Word32_mulCheck (* codegen *)
	     | Word32_neg (* codegen *)
	     | Word32_notb (* codegen *)
	     | Word32_orb (* codegen *)
	     | Word32_rol (* codegen *)
	     | Word32_ror (* codegen *)
	     | Word32_rshift (* codegen *)
	     | Word32_sub (* codegen *)
	     | Word32_toIntX (* ssa to rssa *)
	     | Word32_xorb (* codegen *)
	     | Word8Array_subWord (* codegen *)
	     | Word8Array_updateWord (* codegen *)
	     | Word8Vector_subWord (* codegen *)
	     | Word8_add (* codegen *)
	     | Word8_andb (* codegen *)
	     | Word8_arshift (* codegen *)
	     | Word8_div (* codegen *)
	     | Word8_fromInt (* codegen *)
	     | Word8_fromLargeWord (* codegen *)
	     | Word8_ge (* codegen *)
	     | Word8_gt (* codegen *)
	     | Word8_le (* codegen *)
	     | Word8_lshift (* codegen *)
	     | Word8_lt (* codegen *)
	     | Word8_mod (* codegen *)
	     | Word8_mul (* codegen *)
	     | Word8_neg (* codegen *)
	     | Word8_notb (* codegen *)
	     | Word8_orb (* codegen *)
	     | Word8_rol (* codegen *)
	     | Word8_ror (* codegen *)
	     | Word8_rshift (* codegen *)
	     | Word8_sub (* codegen *)
	     | Word8_toInt (* codegen *)
	     | Word8_toIntX (* codegen *)
	     | Word8_toLargeWord (* codegen *)
	     | Word8_toLargeWordX (* codegen *)
	     | Word8_xorb (* codegen *)
	     | World_save (* ssa to rssa *)

	    val layout: t -> Layout.t
	    val toString: t -> string
	 end

      type t

      structure ApplyArg:
	 sig
	    datatype 'a t =
	       Con of {con: Con.t, hasArg: bool}
	     | Const of Const.Node.t
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
      val ffi: string * Scheme.t -> t
      val gcCollect: t
      val intInfEqual: t
      val intAdd: t
      val intAddCheck: t
      val intMul: t
      val intMulCheck: t
      val intSub: t
      val intSubCheck: t
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
      val newNullary: string -> t (* new of type unit -> unit *)
      val numArgs: t -> int option
      val reff: t
      val scheme: t -> Scheme.t
      val serialize: t
      val toString: t -> string
      val vectorLength: t
      val vectorSub: t
      val word32Add: t
      val word32AddCheck: t
      val word32Andb: t
      val word32FromInt: t
      val word32Gt: t
      val word32Mul: t
      val word32MulCheck: t
      val word32Rshift: t
      val word32Sub: t
      val word32ToIntX: t
   end
