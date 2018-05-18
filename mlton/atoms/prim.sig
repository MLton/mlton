(* Copyright (C) 2014,2017 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
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
               Array_alloc of {raw: bool} (* to rssa (as runtime C fn) *)
             | Array_copyArray (* to rssa (as runtime C fn) *)
             | Array_copyVector (* to rssa (as runtime C fn) *)
             | Array_length (* to rssa *)
             | Array_sub (* to ssa2 *)
             | Array_toArray (* to rssa *)
             | Array_toVector (* to rssa *)
             | Array_uninit (* to rssa *)
             | Array_uninitIsNop (* to rssa *)
             | Array_update (* to ssa2 *)
             | CPointer_add (* codegen *)
             | CPointer_diff (* codegen *)
             | CPointer_equal (* codegen *)
             | CPointer_fromWord (* codegen *)
             | CPointer_getCPointer (* to rssa *)
             | CPointer_getObjptr (* to rssa *)
             | CPointer_getReal of RealSize.t (* to rssa *)
             | CPointer_getWord of WordSize.t (* to rssa *)
             | CPointer_lt (* codegen *)
             | CPointer_setCPointer (* to rssa *)
             | CPointer_setObjptr (* to rssa *)
             | CPointer_setReal of RealSize.t (* to rssa *)
             | CPointer_setWord of WordSize.t (* to rssa *)
             | CPointer_sub (* codegen *)
             | CPointer_toWord (* codegen *)
             | Exn_extra (* implement exceptions *)
             | Exn_name (* implement exceptions *)
             | Exn_setExtendExtra (* implement exceptions *)
             | FFI of 'a CFunction.t (* to rssa *)
             | FFI_Symbol of {name: string,
                              cty: CType.t option,
                              symbolScope: CFunction.SymbolScope.t } (* codegen *)
             | GC_collect (* to rssa (as runtime C fn) *)
             | IntInf_add (* to rssa (as runtime C fn) *)
             | IntInf_andb (* to rssa (as runtime C fn) *)
             | IntInf_arshift (* to rssa (as runtime C fn) *)
             | IntInf_compare (* to rssa (as runtime C fn) *)
             | IntInf_gcd (* to rssa (as runtime C fn) *)
             | IntInf_lshift (* to rssa (as runtime C fn) *)
             | IntInf_mul (* to rssa (as runtime C fn) *)
             | IntInf_neg (* to rssa (as runtime C fn) *)
             | IntInf_notb (* to rssa (as runtime C fn) *)
             | IntInf_orb (* to rssa (as runtime C fn) *)
             | IntInf_quot (* to rssa (as runtime C fn) *)
             | IntInf_rem (* to rssa (as runtime C fn) *)
             | IntInf_sub (* to rssa (as runtime C fn) *)
             | IntInf_toString (* to rssa (as runtime C fn) *)
             | IntInf_toVector (* to rssa *)
             | IntInf_toWord (* to rssa *)
             | IntInf_xorb (* to rssa (as runtime C fn) *)
             (* of type unit -> 'a.
              * Makes a bogus value of any type.
              *)
             | MLton_bogus (* to rssa *)
             | MLton_bug (* to rssa (as impure C fn) *)
             | MLton_deserialize (* unused *)
             | MLton_eq (* to rssa (as Word_equal) *)
             | MLton_equal (* polymorphic equality *)
             | MLton_halt (* to rssa (as runtime C fn) *)
             | MLton_hash (* polymorphic hash *)
             (* MLton_handlesSignals and MLton_installSignalHandler work together
              * to inform the optimizer and basis library whether or not the
              * program uses signal handlers.
              *
              * MLton_installSignalHandler is called by MLton.Signal.setHandler,
              * and is effectively a noop, but is left in the program until, so
              * that the optimizer can test whether or not the program installs
              * signal handlers.
              *
              * MLton_handlesSignals is translated by closure conversion into
              * a boolean, and is true iff MLton_installsSignalHandler is called.
              *)
             | MLton_handlesSignals (* closure conversion *)
             | MLton_installSignalHandler (* to rssa (as nop) *)
             | MLton_serialize (* unused *)
             | MLton_share (* to rssa (as nop or runtime C fn) *)
             | MLton_size (* to rssa (as runtime C fn) *)
             | MLton_touch (* to rssa (as nop) or backend (as nop) *)
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
             | Real_castToWord of RealSize.t * WordSize.t (* codegen *)
             | Real_div of RealSize.t (* codegen *)
             | Real_equal of RealSize.t (* codegen *)
             | Real_ldexp of RealSize.t (* codegen *)
             | Real_le of RealSize.t (* codegen *)
             | Real_lt of RealSize.t (* codegen *)
             | Real_mul of RealSize.t (* codegen *)
             | Real_muladd of RealSize.t (* codegen *)
             | Real_mulsub of RealSize.t (* codegen *)
             | Real_neg of RealSize.t (* codegen *)
             | Real_qequal of RealSize.t (* codegen *)
             | Real_rndToReal of RealSize.t * RealSize.t (* codegen *)
             | Real_rndToWord of RealSize.t * WordSize.t * {signed: bool} (* codegen *)
             | Real_round of RealSize.t (* codegen *)
             | Real_sub of RealSize.t (* codegen *)
             | Ref_assign (* to ssa2 *)
             | Ref_deref (* to ssa2 *)
             | Ref_ref (* to ssa2 *)
             | String_toWord8Vector (* defunctorize *)
             | Thread_atomicBegin (* to rssa *)
             | Thread_atomicEnd (* to rssa *)
             | Thread_atomicState (* to rssa *)
             | Thread_copy (* to rssa (as runtime C fn) *)
             | Thread_copyCurrent (* to rssa (as runtime C fn) *)
             | Thread_returnToC (* codegen *)
             (* switchTo has to be a _prim because we have to know that it
              * enters the runtime -- because everything must be saved
              * on the stack.
              *)
             | Thread_switchTo (* to rssa (as runtime C fn) *)
             | TopLevel_getHandler (* implement exceptions *)
             | TopLevel_getSuffix (* implement suffix *)
             | TopLevel_setHandler (* implement exceptions *)
             | TopLevel_setSuffix (* implement suffix *)
             | Vector_length (* to ssa2 *)
             | Vector_sub (* to ssa2 *)
             | Vector_vector (* to ssa2 *)
             | Weak_canGet (* to rssa (as runtime C fn) *)
             | Weak_get (* to rssa (as runtime C fn) *)
             | Weak_new (* to rssa (as runtime C fn) *)
             | Word_add of WordSize.t (* codegen *)
             | Word_addCheck of WordSize.t * {signed: bool} (* codegen *)
             | Word_andb of WordSize.t (* codegen *)
             | Word_castToReal of WordSize.t * RealSize.t (* codegen *)
             | Word_equal of WordSize.t (* codegen *)
             | Word_extdToWord of WordSize.t * WordSize.t * {signed: bool} (* codegen *)
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
             | Word_rndToReal of WordSize.t * RealSize.t * {signed: bool} (* codegen *)
             | Word_rol of WordSize.t (* codegen *)
             | Word_ror of WordSize.t (* codegen *)
             | Word_rshift of WordSize.t * {signed: bool} (* codegen *)
             | Word_sub of WordSize.t (* codegen *)
             | Word_subCheck of WordSize.t * {signed: bool} (* codegen *)
             | Word_toIntInf (* to rssa *)
             | Word_xorb of WordSize.t (* codegen *)
             | WordVector_toIntInf (* to rssa *)
             | WordArray_subWord of {seqSize:WordSize.t, eleSize:WordSize.t} (* to rssa *)
             | WordArray_updateWord of {seqSize: WordSize.t, eleSize: WordSize.t}  (* to rssa *)
             | WordVector_subWord of {seqSize: WordSize.t, eleSize: WordSize.t}  (* to rssa *)
             | Word8Vector_toString (* defunctorize *)
             | World_save (* to rssa (as runtime C fn) *)

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
      val arrayAlloc: {raw: bool} -> 'a t
      val arrayLength: 'a t
      val arrayToVector: 'a t
      val arrayUpdate: 'a t
      val assign: 'a t
      val bogus: 'a t
      val bug: 'a t
      val checkApp: 'a t * {args: 'a vector,
                            result: 'a,
                            targs: 'a vector,
                            typeOps: {array: 'a -> 'a,
                                      arrow: 'a * 'a -> 'a,
                                      bool: 'a,
                                      cpointer: 'a,
                                      equals: 'a * 'a -> bool,
                                      exn: 'a,
                                      intInf: 'a,
                                      real: RealSize.t -> 'a,
                                      reff: 'a -> 'a,
                                      thread: 'a,
                                      unit: 'a,
                                      vector: 'a -> 'a,
                                      weak: 'a -> 'a,
                                      word: WordSize.t -> 'a}} -> bool
      val cpointerAdd: 'a t
      val cpointerDiff: 'a t
      val cpointerEqual: 'a t
      val cpointerGet: CType.t -> 'a t 
      val cpointerLt: 'a t
      val cpointerSet: CType.t -> 'a t 
      val cpointerSub: 'a t
      val cpointerToWord: 'a t
      val deref: 'a t
      val eq: 'a t    (* pointer equality *)
      val equal: 'a t (* polymorphic equality *)
      val equals: 'a t * 'a t -> bool
      val extractTargs: 'a t * {args: 'b vector,
                                result: 'b,
                                typeOps: {deArray: 'b -> 'b,
                                          deArrow: 'b -> 'b * 'b,
                                          deRef: 'b -> 'b,
                                          deVector: 'b -> 'b,
                                          deWeak: 'b -> 'b}} -> 'b vector
      val ffi: 'a CFunction.t -> 'a t
      val ffiSymbol: {name: string, 
                      cty: CType.t option, 
                      symbolScope: CFunction.SymbolScope.t } -> 'a t
      val fromString: string -> 'a t option
      val hash: 'a t (* polymorphic hash *)
      val intInfToWord: 'a t
      val intInfToVector: 'a t
      val isCommutative: 'a t -> bool
      (*
       * isFunctional p = true iff p always returns same result when given
       *   same args and has no side effects.
       * isFuntional implies not maySideEffect.
       * examples: Array_length, MLton_equal, Vector_vector, Word_add
       * not examples: Array_alloc, Array_sub, Ref_deref, Ref_ref
       *)
      val isFunctional: 'a t -> bool
      val layout: 'a t -> Layout.t
      val layoutApp: 'a t * 'b vector * ('b -> Layout.t) -> Layout.t
      val layoutFull: 'a t * ('a -> Layout.t) -> Layout.t
      val map: 'a t * ('a -> 'b) -> 'b t
      (* examples: Word_addCheck, Word_mulCheck, Word_subCheck *)
      val mayOverflow: 'a t -> bool
      (* examples: Array_update, Ref_assign
       * not examples: Array_sub, Array_uninit, Ref_deref, Ref_ref
       *)
      val maySideEffect: 'a t -> bool
      val name: 'a t -> 'a Name.t
      val realCastToWord: RealSize.t * WordSize.t -> 'a t
      val reff: 'a t
      val toString: 'a t -> string
      val touch: 'a t
      val vector: 'a t
      val vectorLength: 'a t
      val vectorSub: 'a t
      val wordAdd: WordSize.t -> 'a t
      val wordAddCheck: WordSize.t * {signed: bool} -> 'a t
      val wordAndb: WordSize.t -> 'a t
      val wordCastToReal : WordSize.t * RealSize.t -> 'a t
      val wordEqual: WordSize.t -> 'a t
      val wordExtdToWord: WordSize.t * WordSize.t * {signed: bool} -> 'a t
      val wordLshift: WordSize.t -> 'a t
      val wordLt: WordSize.t * {signed: bool} -> 'a t
      val wordMul: WordSize.t * {signed: bool} -> 'a t
      val wordNeg: WordSize.t -> 'a t
      val wordOrb: WordSize.t -> 'a t
      val wordQuot: WordSize.t * {signed: bool} -> 'a t
      val wordRshift: WordSize.t * {signed: bool} -> 'a t
      val wordSub: WordSize.t -> 'a t
      val wordXorb: WordSize.t -> 'a t
   end
