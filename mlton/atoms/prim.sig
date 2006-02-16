(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PRIM_STRUCTS = 
   sig
      structure CFunction: C_FUNCTION
      structure CType: C_TYPE
      structure Con: CON
      structure Const: CONST
      structure RealSize: REAL_SIZE
      structure WordSize: WORD_SIZE
      sharing RealSize = Const.RealX.RealSize
      sharing WordSize = Const.WordX.WordSize
   end

signature PRIM = 
   sig
      include PRIM_STRUCTS

      structure Name:
         sig
            datatype 'a t =
               Array_array (* backend *)
             | Array_array0Const (* constant propagation *)
             | Array_length (* ssa to rssa *)
             | Array_sub (* ssa to ssa2 *)
             | Array_toVector (* backend *)
             | Array_update (* ssa to ssa2 *)
             | Exn_extra (* implement exceptions *)
             | Exn_name (* implement exceptions *)
             | Exn_setExtendExtra (* implement exceptions *)
             | Exn_setInitExtra (* implement exceptions *)
             | FFI of 'a CFunction.t (* ssa to rssa *)
             | FFI_Symbol of {name: string} (* codegen *)
             | GC_collect (* ssa to rssa *)
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
             | MLton_share
             | MLton_size (* ssa to rssa *)
             | MLton_touch (* backend *)
             | Pointer_getPointer (* ssa to rssa *)
             | Pointer_getReal of RealSize.t (* ssa to rssa *)
             | Pointer_getWord of WordSize.t (* ssa to rssa *)
             | Pointer_setPointer (* ssa to rssa *)
             | Pointer_setReal of RealSize.t (* ssa to rssa *)
             | Pointer_setWord of WordSize.t (* ssa to rssa *)
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
             | Real_ldexp of RealSize.t (* codegen *)
             | Real_le of RealSize.t (* codegen *)
             | Real_lt of RealSize.t (* codegen *)
             | Real_mul of RealSize.t (* codegen *)
             | Real_muladd of RealSize.t (* codegen *)
             | Real_mulsub of RealSize.t (* codegen *)
             | Real_neg of RealSize.t     (* codegen *)
             | Real_qequal of RealSize.t (* codegen *)
             | Real_round of RealSize.t (* codegen *)
             | Real_sub of RealSize.t (* codegen *)
             | Real_toWord of RealSize.t * WordSize.t * {signed: bool} (* codegen *)
             | Real_toReal of RealSize.t * RealSize.t (* codegen *)
             | Ref_assign (* ssa to ssa2 *)
             | Ref_deref (* ssa to ssa2 *)
             | Ref_ref (* ssa to ssa2 *)
             | String_toWord8Vector (* defunctorize *)
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
             | TopLevel_setHandler (* implement exceptions *)
             | TopLevel_setSuffix (* implement suffix *)
             | Vector_length (* ssa to ssa2 *)
             | Vector_sub (* ssa to ssa2 *)
             | Weak_canGet (* ssa to rssa *)
             | Weak_get (* ssa to rssa *)
             | Weak_new (* ssa to rssa *)
             | Word_add of WordSize.t (* codegen *)
             | Word_addCheck of WordSize.t * {signed: bool} (* codegen *)
             | Word_andb of WordSize.t (* codegen *)
             | Word_equal of WordSize.t (* codegen *)
             | Word_lshift of WordSize.t (* codegen *)
             | Word_lt of WordSize.t * {signed: bool} (* codegen *)
             | Word_mul of WordSize.t * {signed: bool} (* codegen *)
             | Word_mulCheck of WordSize.t * {signed: bool} (* codegen *)
             | Word_neg of WordSize.t (* codegen *)
             | Word_negCheck of WordSize.t (* codegen *)
             | Word_notb of WordSize.t (* codegen *)
             | Word_orb of WordSize.t (* codegen *)
             | Word_quot of WordSize.t * {signed: bool} (* codegen *)
             | Word_rem of WordSize.t * {signed: bool} (* codegen *)
             | Word_rol of WordSize.t (* codegen *)
             | Word_ror of WordSize.t (* codegen *)
             | Word_rshift of WordSize.t * {signed: bool} (* codegen *)
             | Word_sub of WordSize.t (* codegen *)
             | Word_subCheck of WordSize.t* {signed: bool} (* codegen *)
             | Word_toIntInf (* ssa to rssa *)
             | Word_toReal of WordSize.t * RealSize.t * {signed: bool} (* codegen *)
             | Word_toWord of WordSize.t * WordSize.t * {signed: bool} (* codegen *)
             | Word_xorb of WordSize.t (* codegen *)
             | WordVector_toIntInf (* ssa to rssa *)
             | Word8Array_subWord (* ssa to rssa *)
             | Word8Array_updateWord (* ssa to rssa *)
             | Word8Vector_subWord (* ssa to rssa *)
             | Word8Vector_toString (* defunctorize *)
             | World_save (* ssa to rssa *)

            val layout: 'a t -> Layout.t
            val toString: 'a t -> string
         end

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
            type 'a prim
            datatype ('a, 'b) t =
               Apply of 'a prim * 'b list
             | Bool of bool
             | Const of Const.t
             | Overflow
             | Unknown
             | Var of 'b

            val layout: ('b -> Layout.t) -> ('a, 'b) t -> Layout.t
         end
      
      type 'a t
      sharing type t = ApplyResult.prim
      val apply:
         'a t * 'b ApplyArg.t list * ('b * 'b -> bool) -> ('a, 'b) ApplyResult.t
      val array: 'a t
      val arrayLength: 'a t
      val assign: 'a t
      val bogus: 'a t
      val bug: 'a t
      val deref: 'a t
      val deserialize: 'a t
      val eq: 'a t    (* pointer equality *)
      val equal: 'a t (* polymorphic equality *)
      val equals: 'a t * 'a t -> bool
      val extractTargs: 'a t * {args: 'b vector,
                                deArray: 'b -> 'b,
                                deArrow: 'b -> 'b * 'b,
                                deVector: 'b -> 'b,
                                deWeak: 'b -> 'b,
                                result: 'b} -> 'b vector
      val ffi: 'a CFunction.t -> 'a t
      val ffiSymbol: {name: string} -> 'a t
      val fromString: string -> 'a t option
      val gcCollect: 'a t
      val intInfEqual: 'a t
      val isCommutative: 'a t -> bool
      (*
       * isFunctional p = true iff p always returns same result when given
       *   same args and has no side effects.
       * isFuntional implies not maySideEffect.
       * examples: Array_length, MLton_equal, Int_add
       * not examples: Array_array, Array_sub, Ref_deref, Ref_ref
       *)
      val isFunctional: 'a t -> bool
      val layout: 'a t -> Layout.t
      val layoutApp: 'a t * 'b vector * ('b -> Layout.t) -> Layout.t
      val map: 'a t * ('a -> 'b) -> 'b t
      (* Int_addCheck, Int_mulCheck, Int_subCheck *)
      val mayOverflow: 'a t -> bool
      val mayRaise: 'a t -> bool
      (* examples: Array_update, Ref_assign
       * not examples: Array_array, Array_sub, Ref_deref, Ref_ref
       *)
      val maySideEffect: 'a t -> bool
      val pointerGet: CType.t -> 'a t
      val pointerSet: CType.t -> 'a t
      val name: 'a t -> 'a Name.t
      val reff: 'a t
      val serialize: 'a t
      val toString: 'a t -> string
      val touch: 'a t
      val vectorLength: 'a t
      val vectorSub: 'a t
      val wordAdd: WordSize.t -> 'a t
      val wordAddCheck: WordSize.t * {signed: bool} -> 'a t
      val wordAndb: WordSize.t -> 'a t
      val wordEqual: WordSize.t -> 'a t
      val wordLt: WordSize.t * {signed: bool} -> 'a t
      val wordLshift: WordSize.t -> 'a t
      val wordMul: WordSize.t * {signed: bool} -> 'a t
      val wordMulCheck: WordSize.t * {signed: bool} -> 'a t
      val wordNeg: WordSize.t -> 'a t
      val wordOrb: WordSize.t -> 'a t
      val wordRshift: WordSize.t * {signed: bool} -> 'a t
      val wordSub: WordSize.t -> 'a t
      val wordSubCheck: WordSize.t * {signed: bool} -> 'a t
      val wordToWord: WordSize.t * WordSize.t * {signed: bool} -> 'a t
   end
