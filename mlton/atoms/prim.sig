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
	       Array_allocate (* created and implemented in backend *)
	     | Array_array (* implemented in backend *)
	     | Array_array0 (* implemented in backend *)
	     | Array_array0Const (* implemented in constant-propagation.fun *)
	     | Array_length
	     | Array_sub (* implemented in backend *)
	     | Array_update (* implemented in backend *)
	     | BuildConstant of string
	     | Byte_byteToChar
	     | Byte_charToByte
	     | C_CS_charArrayToWord8Array
	     | Char_lt
	     | Char_le
	     | Char_gt
	     | Char_ge
	     | Char_chr
	     | Char_ord
	     | Constant of string (* implemented in infer.fun *)
	     | Cpointer_isNull
	     | Exn_extra (* implemented in implement-exceptions.fun *)
	     | Exn_keepHistory (* a compile-time boolean *)
	     | Exn_name (* implemented in implement-exceptions.fun *)
	     | Exn_setExtendExtra (* implemented in implement-exceptions.fun *)
	     | Exn_setInitExtra (* implemented in implement-exceptions.fun *)
	     | Exn_setTopLevelHandler (* implemented in implement-exceptions.fun *)
	     | FFI of string
	     | GC_collect
	     | GC_pack
	     | GC_unpack
	     | Int_mul
	     | Int_mulCheck
	     | Int_add
	     | Int_addCheck
	     | Int_sub
	     | Int_subCheck
	     | Int_lt
	     | Int_le
	     | Int_gt
	     | Int_ge
	     | Int_geu
	     | Int_gtu
	     | Int_quot
	     | Int_rem
	     | Int_neg
	     | Int_negCheck
	     | IntInf_add
	     | IntInf_compare
	     | IntInf_equal
	     | IntInf_fromVector
	     | IntInf_fromWord
	     | IntInf_gcd
	     | IntInf_mul
	     | IntInf_neg
	     | IntInf_quot
	     | IntInf_rem
	     | IntInf_sub
	     | IntInf_toString
	     | IntInf_toVector
	     | IntInf_toWord
	     | MLton_bogus (* of type unit -> 'a.
			    * implemented in backend.
			    * Makes a bogus value of any type.
			    *)
	     | MLton_bug
	     | MLton_deserialize
	     | MLton_eq
	     | MLton_equal (* implemented in ssa/poly-equal.fun *)
	     | MLton_halt
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
	     | MLton_handlesSignals
	     | MLton_installSignalHandler
	     | MLton_serialize
	     | MLton_size
	     | Real_Math_acos
	     | Real_Math_asin
	     | Real_Math_atan
	     | Real_Math_atan2
	     | Real_Math_cos
	     | Real_Math_cosh
	     | Real_Math_exp
	     | Real_Math_ln
	     | Real_Math_log10 
	     | Real_Math_pow
	     | Real_Math_sin
	     | Real_Math_sinh
	     | Real_Math_sqrt
	     | Real_Math_tan
	     | Real_Math_tanh
	     | Real_mul
	     | Real_muladd
	     | Real_mulsub
	     | Real_add
	     | Real_sub
	     | Real_div
	     | Real_lt
	     | Real_le
	     | Real_equal
	     | Real_gt
	     | Real_ge
	     | Real_qequal
	     | Real_abs
	     | Real_copysign
	     | Real_frexp
	     | Real_fromInt
	     | Real_ldexp
	     | Real_modf
	     | Real_toInt
	     | Real_neg	 
	     | Real_round
	     | Ref_assign (* implemented in backend *)
	     | Ref_deref (* implemented in backend *)
	     | Ref_ref (* implemented in backend *)
	     | String_equal
	     | String_fromCharVector
	     | String_fromWord8Vector
	     | String_size
	     | String_sub (* implemented in backend *)
	     | String_toCharVector
	     | String_toWord8Vector
	     | Thread_atomicBegin (* implemented in backend *)
	     | Thread_atomicEnd (* implemented in backend *)
	     | Thread_canHandle (* implemented in backend *)
	     | Thread_copy
	     | Thread_copyCurrent
	     (* switchTo has to be a _prim because we have to know that it
	      * enters the runtime -- because everything must be saved
	      * on the stack.
	      *)
	     | Thread_switchTo
	     | Vector_fromArray (* implemented in backend *)
	     | Vector_length
	     | Vector_sub (* implemented in backend *)
	     | Word32_add
	     | Word32_addCheck
	     | Word32_andb
	     | Word32_arshift
	     | Word32_div
	     | Word32_fromInt
	     | Word32_ge
	     | Word32_gt
	     | Word32_le
	     | Word32_lshift
	     | Word32_lt
	     | Word32_mod
	     | Word32_mul
	     | Word32_mulCheck
	     | Word32_neg
	     | Word32_notb
	     | Word32_orb
	     | Word32_rol
	     | Word32_ror
	     | Word32_rshift
	     | Word32_sub
	     | Word32_toIntX
	     | Word32_xorb
	     | Word8Array_subWord
	     | Word8Array_updateWord
	     | Word8Vector_subWord
	     | Word8_add
	     | Word8_andb
	     | Word8_arshift
	     | Word8_div
	     | Word8_fromInt
	     | Word8_fromLargeWord
	     | Word8_ge
	     | Word8_gt
	     | Word8_le
	     | Word8_lshift
	     | Word8_lt
	     | Word8_mod
	     | Word8_mul
	     | Word8_neg
	     | Word8_notb
	     | Word8_orb
	     | Word8_rol
	     | Word8_ror
	     | Word8_rshift
	     | Word8_sub
	     | Word8_toInt
	     | Word8_toIntX
	     | Word8_toLargeWord
	     | Word8_toLargeWordX
	     | Word8_xorb
	     | World_save

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
      val array0: t
      val arrayAllocate: t
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
			 devector: 'a -> 'a} -> 'a vector
      val ffi: string * Scheme.t -> t
      val gcCollect: t
      val intInfEqual: t
      val intAdd: t
      val intAddCheck: t
      val intMul: t
      val intMulCheck: t
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
      val stringEqual: t
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
