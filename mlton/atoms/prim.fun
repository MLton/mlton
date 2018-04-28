(* Copyright (C) 2009-2010,2014,2016-2017 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(*
 * If you add new polymorphic primitives, you must modify extractTargs.
 *)

functor Prim (S: PRIM_STRUCTS): PRIM = 
struct

open S

local
   open Const
in
   structure RealX = RealX
   structure WordX = WordX
   structure WordXVector = WordXVector
end

structure Kind =
   struct
      datatype t =
         DependsOnState
       | Functional
       | Moveable
       | SideEffect
   end

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

fun name p = p

(* The values of these strings are important since they are referred to
 * in the basis library code.  See basis-library/misc/primitive.sml.
 *)
fun toString (n: 'a t): string =
   let
      fun real (s: RealSize.t, str: string): string =
         concat ["Real", RealSize.toString s, "_", str]
      fun sign {signed} = if signed then "WordS" else "WordU"
      fun word (s: WordSize.t, str: string): string =
         concat ["Word", WordSize.toString s, "_", str]
      fun wordSeq (seqSize: WordSize.t, seqKind: string, oper: string, eleSize: WordSize.t): string =
         concat ["Word", WordSize.toString seqSize, seqKind, "_", oper, "Word", WordSize.toString eleSize]
      fun wordS (s: WordSize.t, sg, str: string): string =
         concat [sign sg, WordSize.toString s, "_", str]
      val realC = ("Real", RealSize.toString)
      val wordC = ("Word", WordSize.toString)
      fun wordCS sg = (sign sg, WordSize.toString)
      fun coerce (k, (n, sizeToString), (n', sizeToString'), s, s'): string =
         concat [n, sizeToString s, "_", k ,"To", n', sizeToString' s']
      fun cast (c, c', s, s') = coerce ("cast", c, c', s, s')
      fun extd (c, c', s, s') = coerce ("extd", c, c', s, s')
      fun rnd (c, c', s, s') = coerce ("rnd", c, c', s, s')
      fun cpointerGet (ty, s) = concat ["CPointer_get", ty, s]
      fun cpointerSet (ty, s) = concat ["CPointer_set", ty, s]
   in
      case n of
         Array_alloc {raw} => if raw then "Array_allocRaw" else "Array_alloc"
       | Array_copyArray => "Array_copyArray"
       | Array_copyVector => "Array_copyVector"
       | Array_length => "Array_length"
       | Array_sub => "Array_sub"
       | Array_toArray => "Array_toArray"
       | Array_toVector => "Array_toVector"
       | Array_uninit => "Array_uninit"
       | Array_uninitIsNop => "Array_uninitIsNop"
       | Array_update => "Array_update"
       | CPointer_add => "CPointer_add"
       | CPointer_diff => "CPointer_diff"
       | CPointer_equal => "CPointer_equal"
       | CPointer_fromWord => "CPointer_fromWord"
       | CPointer_getCPointer => "CPointer_getCPointer"
       | CPointer_getObjptr => "CPointer_getObjptr"
       | CPointer_getReal s => cpointerGet ("Real", RealSize.toString s)
       | CPointer_getWord s => cpointerGet ("Word", WordSize.toString s)
       | CPointer_lt => "CPointer_lt"
       | CPointer_setCPointer => "CPointer_setCPointer"
       | CPointer_setObjptr => "CPointer_setObjptr"
       | CPointer_setReal s => cpointerSet ("Real", RealSize.toString s)
       | CPointer_setWord s => cpointerSet ("Word", WordSize.toString s)
       | CPointer_sub => "CPointer_sub"
       | CPointer_toWord => "CPointer_toWord"
       | Exn_extra => "Exn_extra"
       | Exn_name => "Exn_name"
       | Exn_setExtendExtra => "Exn_setExtendExtra"
       | FFI f => (CFunction.Target.toString o CFunction.target) f
       | FFI_Symbol {name, ...} => name
       | GC_collect => "GC_collect"
       | IntInf_add => "IntInf_add"
       | IntInf_andb => "IntInf_andb"
       | IntInf_arshift => "IntInf_arshift"
       | IntInf_compare => "IntInf_compare"
       | IntInf_gcd => "IntInf_gcd"
       | IntInf_lshift => "IntInf_lshift"
       | IntInf_mul => "IntInf_mul"
       | IntInf_neg => "IntInf_neg"
       | IntInf_notb => "IntInf_notb"
       | IntInf_orb => "IntInf_orb"
       | IntInf_quot => "IntInf_quot"
       | IntInf_rem => "IntInf_rem"
       | IntInf_sub => "IntInf_sub"
       | IntInf_toString => "IntInf_toString"
       | IntInf_toVector => "IntInf_toVector"
       | IntInf_toWord => "IntInf_toWord"
       | IntInf_xorb => "IntInf_xorb"
       | MLton_bogus => "MLton_bogus"
       | MLton_bug => "MLton_bug"
       | MLton_deserialize => "MLton_deserialize"
       | MLton_eq => "MLton_eq"
       | MLton_equal => "MLton_equal"
       | MLton_halt => "MLton_halt"
       | MLton_hash => "MLton_hash"
       | MLton_handlesSignals => "MLton_handlesSignals"
       | MLton_installSignalHandler => "MLton_installSignalHandler"
       | MLton_serialize => "MLton_serialize"
       | MLton_share => "MLton_share"
       | MLton_size => "MLton_size"
       | MLton_touch => "MLton_touch"
       | Real_Math_acos s => real (s, "Math_acos")
       | Real_Math_asin s => real (s, "Math_asin")
       | Real_Math_atan s => real (s, "Math_atan")
       | Real_Math_atan2 s => real (s, "Math_atan2")
       | Real_Math_cos s => real (s, "Math_cos")
       | Real_Math_exp s => real (s, "Math_exp")
       | Real_Math_ln s => real (s, "Math_ln")
       | Real_Math_log10 s => real (s, "Math_log10")
       | Real_Math_sin s => real (s, "Math_sin")
       | Real_Math_sqrt s => real (s, "Math_sqrt")
       | Real_Math_tan s => real (s, "Math_tan")
       | Real_abs s => real (s, "abs")
       | Real_add s => real (s, "add")
       | Real_castToWord (s1, s2) => cast (realC, wordC, s1, s2)
       | Real_div s => real (s, "div")
       | Real_equal s => real (s, "equal")
       | Real_ldexp s => real (s, "ldexp")
       | Real_le s => real (s, "le")
       | Real_lt s => real (s, "lt")
       | Real_mul s => real (s, "mul")
       | Real_muladd s => real (s, "muladd")
       | Real_mulsub s => real (s, "mulsub")
       | Real_neg s => real (s, "neg")
       | Real_qequal s => real (s, "qequal")
       | Real_rndToReal (s1, s2) => rnd (realC, realC, s1, s2)
       | Real_rndToWord (s1, s2, sg) => rnd (realC, wordCS sg, s1, s2)
       | Real_round s => real (s, "round")
       | Real_sub s => real (s, "sub")
       | Ref_assign => "Ref_assign"
       | Ref_deref => "Ref_deref"
       | Ref_ref => "Ref_ref"
       | String_toWord8Vector => "String_toWord8Vector"
       | Thread_atomicBegin => "Thread_atomicBegin"
       | Thread_atomicEnd => "Thread_atomicEnd"
       | Thread_atomicState => "Thread_atomicState"
       | Thread_copy => "Thread_copy"
       | Thread_copyCurrent => "Thread_copyCurrent"
       | Thread_returnToC => "Thread_returnToC"
       | Thread_switchTo => "Thread_switchTo"
       | TopLevel_getHandler => "TopLevel_getHandler"
       | TopLevel_getSuffix => "TopLevel_getSuffix"
       | TopLevel_setHandler => "TopLevel_setHandler"
       | TopLevel_setSuffix => "TopLevel_setSuffix"
       | Vector_length => "Vector_length"
       | Vector_sub => "Vector_sub"
       | Vector_vector => "Vector_vector"
       | Weak_canGet => "Weak_canGet"
       | Weak_get => "Weak_get"
       | Weak_new => "Weak_new"
       | WordArray_subWord {seqSize, eleSize} =>
            wordSeq (seqSize, "Array", "sub", eleSize)
       | WordArray_updateWord {seqSize, eleSize} =>
            wordSeq (seqSize, "Array", "update", eleSize)
       | WordVector_subWord {seqSize, eleSize} =>
            wordSeq (seqSize, "Vector", "sub", eleSize)
       | Word8Vector_toString => "Word8Vector_toString"
       | WordVector_toIntInf => "WordVector_toIntInf"
       | Word_add s => word (s, "add")
       | Word_addCheck (s, sg) => wordS (s, sg, "addCheck")
       | Word_andb s => word (s, "andb")
       | Word_castToReal (s1, s2) => cast (wordC, realC, s1, s2)
       | Word_equal s => word (s, "equal")
       | Word_extdToWord (s1, s2, sg) => extd (wordCS sg, wordC, s1, s2)
       | Word_lshift s => word (s, "lshift")
       | Word_lt (s, sg) => wordS (s, sg, "lt")
       | Word_mul (s, sg) => wordS (s, sg, "mul")
       | Word_mulCheck (s, sg) => wordS (s, sg, "mulCheck")
       | Word_neg s => word (s, "neg")
       | Word_negCheck s => word (s, "negCheck")
       | Word_notb s => word (s, "notb")
       | Word_orb s => word (s, "orb")
       | Word_quot (s, sg) => wordS (s, sg, "quot")
       | Word_rem (s, sg) => wordS (s, sg, "rem")
       | Word_rndToReal (s1, s2, sg) => rnd (wordCS sg, realC, s1, s2)
       | Word_rol s => word (s, "rol")
       | Word_ror s => word (s, "ror")
       | Word_rshift (s, sg) => wordS (s, sg, "rshift")
       | Word_sub s => word (s, "sub")
       | Word_subCheck (s, sg) => wordS (s, sg, "subCheck")
       | Word_toIntInf => "Word_toIntInf"
       | Word_xorb s => word (s, "xorb")
       | World_save => "World_save"
   end

fun layout p = Layout.str (toString p)
fun layoutFull (p, layoutX) =
   case p of
      FFI f => Layout.seq [Layout.str "FFI ", CFunction.layout (f, layoutX)]
    | FFI_Symbol {name, cty, symbolScope} =>
         Layout.seq [Layout.str "FFI_Symbol ",
                     Layout.record
                     [("name", Layout.str name),
                      ("cty", Option.layout CType.layout cty),
                      ("symbolScope", CFunction.SymbolScope.layout symbolScope)]]
    | p => layout p

val equals: 'a t * 'a t -> bool =
   fn (Array_alloc {raw = r}, Array_alloc {raw = r'}) => Bool.equals (r, r')
    | (Array_copyArray, Array_copyArray) => true
    | (Array_copyVector, Array_copyVector) => true
    | (Array_length, Array_length) => true
    | (Array_sub, Array_sub) => true
    | (Array_toArray, Array_toArray) => true
    | (Array_toVector, Array_toVector) => true
    | (Array_uninit, Array_uninit) => true
    | (Array_uninitIsNop, Array_uninitIsNop) => true
    | (Array_update, Array_update) => true
    | (CPointer_add, CPointer_add) => true
    | (CPointer_diff, CPointer_diff) => true
    | (CPointer_equal, CPointer_equal) => true
    | (CPointer_fromWord, CPointer_fromWord) => true
    | (CPointer_getCPointer, CPointer_getCPointer) => true
    | (CPointer_getObjptr, CPointer_getObjptr) => true
    | (CPointer_getReal s, CPointer_getReal s') => RealSize.equals (s, s')
    | (CPointer_getWord s, CPointer_getWord s') => WordSize.equals (s, s')
    | (CPointer_lt, CPointer_lt) => true
    | (CPointer_setCPointer, CPointer_setCPointer) => true
    | (CPointer_setObjptr, CPointer_setObjptr) => true
    | (CPointer_setReal s, CPointer_setReal s') => RealSize.equals (s, s')
    | (CPointer_setWord s, CPointer_setWord s') => WordSize.equals (s, s')
    | (CPointer_sub, CPointer_sub) => true
    | (CPointer_toWord, CPointer_toWord) => true
    | (Exn_extra, Exn_extra) => true
    | (Exn_name, Exn_name) => true
    | (Exn_setExtendExtra, Exn_setExtendExtra) => true
    | (FFI f, FFI f') => CFunction.equals (f, f')
    | (FFI_Symbol {name = n, ...}, FFI_Symbol {name = n', ...}) => n = n'
    | (GC_collect, GC_collect) => true
    | (IntInf_add, IntInf_add) => true
    | (IntInf_andb, IntInf_andb) => true
    | (IntInf_arshift, IntInf_arshift) => true
    | (IntInf_compare, IntInf_compare) => true
    | (IntInf_gcd, IntInf_gcd) => true
    | (IntInf_lshift, IntInf_lshift) => true
    | (IntInf_mul, IntInf_mul) => true
    | (IntInf_neg, IntInf_neg) => true
    | (IntInf_notb, IntInf_notb) => true
    | (IntInf_orb, IntInf_orb) => true
    | (IntInf_quot, IntInf_quot) => true
    | (IntInf_rem, IntInf_rem) => true
    | (IntInf_sub, IntInf_sub) => true
    | (IntInf_toString, IntInf_toString) => true
    | (IntInf_toVector, IntInf_toVector) => true
    | (IntInf_toWord, IntInf_toWord) => true
    | (IntInf_xorb, IntInf_xorb) => true
    | (MLton_bogus, MLton_bogus) => true
    | (MLton_bug, MLton_bug) => true
    | (MLton_deserialize, MLton_deserialize) => true
    | (MLton_eq, MLton_eq) => true
    | (MLton_equal, MLton_equal) => true
    | (MLton_halt, MLton_halt) => true
    | (MLton_hash, MLton_hash) => true
    | (MLton_handlesSignals, MLton_handlesSignals) => true
    | (MLton_installSignalHandler, MLton_installSignalHandler) => true
    | (MLton_serialize, MLton_serialize) => true
    | (MLton_share, MLton_share) => true
    | (MLton_size, MLton_size) => true
    | (MLton_touch, MLton_touch) => true
    | (Real_Math_acos s, Real_Math_acos s') => RealSize.equals (s, s')
    | (Real_Math_asin s, Real_Math_asin s') => RealSize.equals (s, s')
    | (Real_Math_atan s, Real_Math_atan s') => RealSize.equals (s, s')
    | (Real_Math_atan2 s, Real_Math_atan2 s') => RealSize.equals (s, s')
    | (Real_Math_cos s, Real_Math_cos s') => RealSize.equals (s, s')
    | (Real_Math_exp s, Real_Math_exp s') => RealSize.equals (s, s')
    | (Real_Math_ln s, Real_Math_ln s') => RealSize.equals (s, s')
    | (Real_Math_log10 s, Real_Math_log10 s') => RealSize.equals (s, s')
    | (Real_Math_sin s, Real_Math_sin s') => RealSize.equals (s, s')
    | (Real_Math_sqrt s, Real_Math_sqrt s') => RealSize.equals (s, s')
    | (Real_Math_tan s, Real_Math_tan s') => RealSize.equals (s, s')
    | (Real_abs s, Real_abs s') => RealSize.equals (s, s')
    | (Real_add s, Real_add s') => RealSize.equals (s, s')
    | (Real_castToWord (s1, s2), Real_castToWord (s1', s2')) =>
         RealSize.equals (s1, s1')
         andalso WordSize.equals (s2, s2')
    | (Real_div s, Real_div s') => RealSize.equals (s, s')
    | (Real_equal s, Real_equal s') => RealSize.equals (s, s')
    | (Real_ldexp s, Real_ldexp s') => RealSize.equals (s, s')
    | (Real_le s, Real_le s') => RealSize.equals (s, s')
    | (Real_lt s, Real_lt s') => RealSize.equals (s, s')
    | (Real_mul s, Real_mul s') => RealSize.equals (s, s')
    | (Real_muladd s, Real_muladd s') => RealSize.equals (s, s')
    | (Real_mulsub s, Real_mulsub s') => RealSize.equals (s, s')
    | (Real_neg s, Real_neg s') => RealSize.equals (s, s')
    | (Real_qequal s, Real_qequal s') => RealSize.equals (s, s')
    | (Real_rndToReal (s1, s2), Real_rndToReal (s1', s2')) =>
         RealSize.equals (s1, s1') andalso RealSize.equals (s2, s2')
    | (Real_rndToWord (s1, s2, sg), Real_rndToWord (s1', s2', sg')) =>
         RealSize.equals (s1, s1')
         andalso WordSize.equals (s2, s2')
         andalso sg = sg'
    | (Real_round s, Real_round s') => RealSize.equals (s, s')
    | (Real_sub s, Real_sub s') => RealSize.equals (s, s')
    | (Ref_assign, Ref_assign) => true
    | (Ref_deref, Ref_deref) => true
    | (Ref_ref, Ref_ref) => true
    | (String_toWord8Vector, String_toWord8Vector) => true
    | (Thread_atomicBegin, Thread_atomicBegin) => true
    | (Thread_atomicEnd, Thread_atomicEnd) => true
    | (Thread_atomicState, Thread_atomicState) => true
    | (Thread_copy, Thread_copy) => true
    | (Thread_copyCurrent, Thread_copyCurrent) => true
    | (Thread_returnToC, Thread_returnToC) => true
    | (Thread_switchTo, Thread_switchTo) => true
    | (TopLevel_getHandler, TopLevel_getHandler) => true
    | (TopLevel_getSuffix, TopLevel_getSuffix) => true
    | (TopLevel_setHandler, TopLevel_setHandler) => true
    | (TopLevel_setSuffix, TopLevel_setSuffix) => true
    | (Vector_length, Vector_length) => true
    | (Vector_sub, Vector_sub) => true
    | (Vector_vector, Vector_vector) => true
    | (Weak_canGet, Weak_canGet) => true
    | (Weak_get, Weak_get) => true
    | (Weak_new, Weak_new) => true
    | (Word_add s, Word_add s') => WordSize.equals (s, s')
    | (Word_addCheck (s, sg), Word_addCheck (s', sg')) =>
         WordSize.equals (s, s') andalso sg = sg'
    | (Word_andb s, Word_andb s') => WordSize.equals (s, s')
    | (Word_castToReal (s1, s2), Word_castToReal (s1', s2')) =>
         WordSize.equals (s1, s1')
         andalso RealSize.equals (s2, s2')
    | (Word_extdToWord (s1, s2, sg), Word_extdToWord (s1', s2', sg')) =>
         WordSize.equals (s1, s1')
         andalso WordSize.equals (s2, s2')
         andalso sg = sg'
    | (Word_equal s, Word_equal s') => WordSize.equals (s, s')
    | (Word_lshift s, Word_lshift s') => WordSize.equals (s, s')
    | (Word_lt (s, sg), Word_lt (s', sg')) =>
         WordSize.equals (s, s') andalso sg = sg'
    | (Word_mul (s, sg), Word_mul (s', sg')) =>
         WordSize.equals (s, s') andalso sg = sg'
    | (Word_mulCheck (s, sg), Word_mulCheck (s', sg')) =>
         WordSize.equals (s, s') andalso sg = sg'
    | (Word_neg s, Word_neg s') => WordSize.equals (s, s')
    | (Word_negCheck s, Word_negCheck s') => WordSize.equals (s, s')
    | (Word_notb s, Word_notb s') => WordSize.equals (s, s')
    | (Word_orb s, Word_orb s') => WordSize.equals (s, s')
    | (Word_quot (s, sg), Word_quot (s', sg')) =>
         WordSize.equals (s, s') andalso sg = sg'
    | (Word_rem (s, sg), Word_rem (s', sg')) =>
         WordSize.equals (s, s') andalso sg = sg'
    | (Word_rndToReal (s1, s2, sg), Word_rndToReal (s1', s2', sg')) =>
         WordSize.equals (s1, s1')
         andalso RealSize.equals (s2, s2')
         andalso sg = sg'
    | (Word_rol s, Word_rol s') => WordSize.equals (s, s')
    | (Word_ror s, Word_ror s') => WordSize.equals (s, s')
    | (Word_rshift (s, sg), Word_rshift (s', sg')) =>
         WordSize.equals (s, s') andalso sg = sg'
    | (Word_sub s, Word_sub s') => WordSize.equals (s, s')
    | (Word_subCheck (s, sg), Word_subCheck (s', sg')) =>
         WordSize.equals (s, s') andalso sg = sg'
    | (Word_toIntInf, Word_toIntInf) => true
    | (Word_xorb s, Word_xorb s') => WordSize.equals (s, s')
    | (WordVector_toIntInf, WordVector_toIntInf) => true
    | (WordArray_subWord {seqSize = seqSize, eleSize = eleSize},
       WordArray_subWord {seqSize = seqSize', eleSize = eleSize'}) =>
         WordSize.equals (seqSize, seqSize')
         andalso WordSize.equals (eleSize, eleSize')
    | (WordArray_updateWord {seqSize = seqSize, eleSize = eleSize},
       WordArray_updateWord {seqSize = seqSize', eleSize = eleSize'}) =>
         WordSize.equals (seqSize, seqSize')
         andalso WordSize.equals (eleSize, eleSize')
    | (WordVector_subWord {seqSize = seqSize, eleSize = eleSize},
       WordVector_subWord {seqSize = seqSize', eleSize = eleSize'}) =>
         WordSize.equals (seqSize, seqSize')
         andalso WordSize.equals (eleSize, eleSize')
    | (Word8Vector_toString, Word8Vector_toString) => true
    | (World_save, World_save) => true
    | _ => false

val map: 'a t * ('a -> 'b) -> 'b t =
   fn (p, f) =>
   case p of
      Array_alloc {raw} => Array_alloc {raw = raw}
    | Array_copyArray => Array_copyArray
    | Array_copyVector => Array_copyVector
    | Array_length => Array_length
    | Array_sub => Array_sub
    | Array_toArray => Array_toArray
    | Array_toVector => Array_toVector
    | Array_uninit => Array_uninit
    | Array_uninitIsNop => Array_uninitIsNop
    | Array_update => Array_update
    | CPointer_add => CPointer_add
    | CPointer_diff => CPointer_diff
    | CPointer_equal => CPointer_equal
    | CPointer_fromWord => CPointer_fromWord
    | CPointer_getCPointer => CPointer_getCPointer
    | CPointer_getObjptr => CPointer_getObjptr
    | CPointer_getReal z => CPointer_getReal z
    | CPointer_getWord z => CPointer_getWord z
    | CPointer_lt => CPointer_lt
    | CPointer_setCPointer => CPointer_setCPointer
    | CPointer_setObjptr => CPointer_setObjptr
    | CPointer_setReal z => CPointer_setReal z
    | CPointer_setWord z => CPointer_setWord z
    | CPointer_sub => CPointer_sub
    | CPointer_toWord => CPointer_toWord
    | Exn_extra => Exn_extra
    | Exn_name => Exn_name
    | Exn_setExtendExtra => Exn_setExtendExtra
    | FFI func => FFI (CFunction.map (func, f))
    | FFI_Symbol {name, cty, symbolScope} => 
        FFI_Symbol {name = name, cty = cty, symbolScope = symbolScope}
    | GC_collect => GC_collect
    | IntInf_add => IntInf_add
    | IntInf_andb => IntInf_andb
    | IntInf_arshift => IntInf_arshift
    | IntInf_compare => IntInf_compare
    | IntInf_gcd => IntInf_gcd
    | IntInf_lshift => IntInf_lshift
    | IntInf_mul => IntInf_mul
    | IntInf_neg => IntInf_neg
    | IntInf_notb => IntInf_notb
    | IntInf_orb => IntInf_orb
    | IntInf_quot => IntInf_quot
    | IntInf_rem => IntInf_rem
    | IntInf_sub => IntInf_sub
    | IntInf_toString => IntInf_toString
    | IntInf_toVector => IntInf_toVector
    | IntInf_toWord => IntInf_toWord
    | IntInf_xorb => IntInf_xorb
    | MLton_bogus => MLton_bogus
    | MLton_bug => MLton_bug
    | MLton_deserialize => MLton_deserialize
    | MLton_eq => MLton_eq
    | MLton_equal => MLton_equal
    | MLton_halt => MLton_halt
    | MLton_hash => MLton_hash
    | MLton_handlesSignals => MLton_handlesSignals
    | MLton_installSignalHandler => MLton_installSignalHandler
    | MLton_serialize => MLton_serialize
    | MLton_share => MLton_share
    | MLton_size => MLton_size
    | MLton_touch => MLton_touch
    | Real_Math_acos z => Real_Math_acos z
    | Real_Math_asin z => Real_Math_asin z
    | Real_Math_atan z => Real_Math_atan z
    | Real_Math_atan2 z => Real_Math_atan2 z
    | Real_Math_cos z => Real_Math_cos z
    | Real_Math_exp z => Real_Math_exp z
    | Real_Math_ln z => Real_Math_ln z
    | Real_Math_log10 z => Real_Math_log10 z
    | Real_Math_sin z => Real_Math_sin z
    | Real_Math_sqrt z => Real_Math_sqrt z
    | Real_Math_tan z => Real_Math_tan z
    | Real_abs z => Real_abs z
    | Real_add z => Real_add z
    | Real_castToWord z => Real_castToWord z
    | Real_div z => Real_div z
    | Real_equal z => Real_equal z
    | Real_ldexp z => Real_ldexp z
    | Real_le z => Real_le z
    | Real_lt z => Real_lt z
    | Real_mul z => Real_mul z
    | Real_muladd z => Real_muladd z
    | Real_mulsub z => Real_mulsub z
    | Real_neg z => Real_neg z
    | Real_qequal z => Real_qequal z
    | Real_rndToReal z => Real_rndToReal z
    | Real_rndToWord z => Real_rndToWord z
    | Real_round z => Real_round z
    | Real_sub z => Real_sub z
    | Ref_assign => Ref_assign
    | Ref_deref => Ref_deref
    | Ref_ref => Ref_ref
    | String_toWord8Vector => String_toWord8Vector
    | Thread_atomicBegin => Thread_atomicBegin
    | Thread_atomicEnd => Thread_atomicEnd
    | Thread_atomicState => Thread_atomicState
    | Thread_copy => Thread_copy
    | Thread_copyCurrent => Thread_copyCurrent
    | Thread_returnToC => Thread_returnToC
    | Thread_switchTo => Thread_switchTo
    | TopLevel_getHandler => TopLevel_getHandler
    | TopLevel_getSuffix => TopLevel_getSuffix
    | TopLevel_setHandler => TopLevel_setHandler
    | TopLevel_setSuffix => TopLevel_setSuffix
    | Vector_length => Vector_length
    | Vector_sub => Vector_sub
    | Vector_vector => Vector_vector
    | Weak_canGet => Weak_canGet
    | Weak_get => Weak_get
    | Weak_new => Weak_new
    | Word_add z => Word_add z
    | Word_addCheck z => Word_addCheck z
    | Word_andb z => Word_andb z
    | Word_castToReal z => Word_castToReal z
    | Word_equal z => Word_equal z
    | Word_extdToWord z => Word_extdToWord z
    | Word_lshift z => Word_lshift z
    | Word_lt z => Word_lt z
    | Word_mul z => Word_mul z
    | Word_mulCheck z => Word_mulCheck z
    | Word_neg z => Word_neg z
    | Word_negCheck z => Word_negCheck z
    | Word_notb z => Word_notb z
    | Word_orb z => Word_orb z
    | Word_quot z => Word_quot z
    | Word_rem z => Word_rem z
    | Word_rndToReal z => Word_rndToReal z
    | Word_rol z => Word_rol z
    | Word_ror z => Word_ror z
    | Word_rshift z => Word_rshift z
    | Word_sub z => Word_sub z
    | Word_subCheck z => Word_subCheck z
    | Word_toIntInf => Word_toIntInf
    | Word_xorb z => Word_xorb z
    | WordVector_toIntInf => WordVector_toIntInf
    | WordArray_subWord z => WordArray_subWord z
    | WordArray_updateWord z => WordArray_updateWord z
    | WordVector_subWord z => WordVector_subWord z
    | Word8Vector_toString => Word8Vector_toString
    | World_save => World_save

val cast: 'a t -> 'b t = fn p => map (p, fn _ => Error.bug "Prim.cast")

val arrayAlloc = fn {raw} => Array_alloc {raw = raw}
val arrayLength = Array_length
val arrayToVector = Array_toVector
val arrayUpdate = Array_update
val assign = Ref_assign
val bogus = MLton_bogus
val bug = MLton_bug
val cpointerAdd = CPointer_add
val cpointerDiff = CPointer_diff
val cpointerEqual = CPointer_equal
fun cpointerGet ctype = 
   let datatype z = datatype CType.t
   in
      case ctype of
         CPointer => CPointer_getCPointer
       | Int8 => CPointer_getWord (WordSize.fromBits (Bits.fromInt 8))
       | Int16 => CPointer_getWord (WordSize.fromBits (Bits.fromInt 16))
       | Int32 => CPointer_getWord (WordSize.fromBits (Bits.fromInt 32))
       | Int64 => CPointer_getWord (WordSize.fromBits (Bits.fromInt 64))
       | Objptr => CPointer_getObjptr
       | Real32 => CPointer_getReal RealSize.R32
       | Real64 => CPointer_getReal RealSize.R64
       | Word8 => CPointer_getWord (WordSize.fromBits (Bits.fromInt 8))
       | Word16 => CPointer_getWord (WordSize.fromBits (Bits.fromInt 16))
       | Word32 => CPointer_getWord (WordSize.fromBits (Bits.fromInt 32))
       | Word64 => CPointer_getWord (WordSize.fromBits (Bits.fromInt 64))
   end
val cpointerLt = CPointer_lt
fun cpointerSet ctype = 
   let datatype z = datatype CType.t
   in
      case ctype of
         CPointer => CPointer_setCPointer
       | Int8 => CPointer_setWord (WordSize.fromBits (Bits.fromInt 8))
       | Int16 => CPointer_setWord (WordSize.fromBits (Bits.fromInt 16))
       | Int32 => CPointer_setWord (WordSize.fromBits (Bits.fromInt 32))
       | Int64 => CPointer_setWord (WordSize.fromBits (Bits.fromInt 64))
       | Objptr => CPointer_setObjptr
       | Real32 => CPointer_setReal RealSize.R32
       | Real64 => CPointer_setReal RealSize.R64
       | Word8 => CPointer_setWord (WordSize.fromBits (Bits.fromInt 8))
       | Word16 => CPointer_setWord (WordSize.fromBits (Bits.fromInt 16))
       | Word32 => CPointer_setWord (WordSize.fromBits (Bits.fromInt 32))
       | Word64 => CPointer_setWord (WordSize.fromBits (Bits.fromInt 64))
   end
val cpointerSub = CPointer_sub
val cpointerToWord = CPointer_toWord
val deref = Ref_deref
val eq = MLton_eq
val equal = MLton_equal
val ffi = FFI
val ffiSymbol = FFI_Symbol
val hash = MLton_hash
val intInfToVector = IntInf_toVector
val intInfToWord = IntInf_toWord
val intInfNeg = IntInf_neg
val intInfNotb = IntInf_notb
val realCastToWord = Real_castToWord
val reff = Ref_ref
val touch = MLton_touch
val vector = Vector_vector
val vectorLength = Vector_length
val vectorSub = Vector_sub
val wordAdd = Word_add
val wordAddCheck = Word_addCheck
val wordAndb = Word_andb
val wordCastToReal = Word_castToReal
val wordEqual = Word_equal
val wordExtdToWord = Word_extdToWord
val wordLshift = Word_lshift
val wordLt = Word_lt
val wordMul = Word_mul
val wordNeg = Word_neg
val wordNegCheck = Word_negCheck
val wordNotb = Word_notb
val wordOrb = Word_orb
val wordQuot = Word_quot
val wordRshift = Word_rshift
val wordSub = Word_sub
val wordXorb = Word_xorb

val isCommutative =
   fn MLton_eq => true
    | MLton_equal => true
    | Real_add _ => true
    | Real_mul _ => true
    | Real_equal _ => true
    | Real_qequal _ => true
    | Word_add _ => true
    | Word_addCheck _ => true
    | Word_andb _ => true
    | Word_equal _ => true
    | Word_mul _ => true
    | Word_mulCheck _ => true
    | Word_orb _ => true
    | Word_xorb _ => true
    | _ => false

val mayOverflow =
   fn Word_addCheck _ => true
    | Word_mulCheck _ => true
    | Word_negCheck _ => true
    | Word_subCheck _ => true
    | _ => false

val kind: 'a t -> Kind.t =
   fn p =>
   let
      datatype z = datatype Kind.t
   in
      case p of
         Array_alloc _ => Moveable
       | Array_copyArray => SideEffect
       | Array_copyVector => SideEffect
       | Array_length => Functional
       | Array_sub => DependsOnState
       | Array_toArray => DependsOnState
       | Array_toVector => DependsOnState
       | Array_uninit => SideEffect
       | Array_uninitIsNop => Functional
       | Array_update => SideEffect
       | CPointer_add => Functional
       | CPointer_diff => Functional
       | CPointer_equal => Functional
       | CPointer_fromWord => Functional
       | CPointer_getCPointer => DependsOnState
       | CPointer_getObjptr => DependsOnState
       | CPointer_getReal _ => DependsOnState
       | CPointer_getWord _ => DependsOnState
       | CPointer_lt => Functional
       | CPointer_setCPointer => SideEffect
       | CPointer_setObjptr => SideEffect
       | CPointer_setReal _ => SideEffect
       | CPointer_setWord _ => SideEffect
       | CPointer_sub => Functional
       | CPointer_toWord => Functional
       | Exn_extra => Functional
       | Exn_name => Functional
       | Exn_setExtendExtra => SideEffect
       | FFI (CFunction.T {kind, ...}) => (case kind of
                                              CFunction.Kind.Impure => SideEffect
                                            | CFunction.Kind.Pure => Functional
                                            | CFunction.Kind.Runtime _ => SideEffect)
       | FFI_Symbol _ => Functional
       | GC_collect => SideEffect
       | IntInf_add => Functional
       | IntInf_andb => Functional
       | IntInf_arshift => Functional
       | IntInf_compare => Functional
       | IntInf_gcd => Functional
       | IntInf_lshift => Functional
       | IntInf_mul => Functional
       | IntInf_neg => Functional
       | IntInf_notb => Functional
       | IntInf_orb => Functional
       | IntInf_quot => Functional
       | IntInf_rem => Functional
       | IntInf_sub => Functional
       | IntInf_toString => Functional
       | IntInf_toVector => Functional
       | IntInf_toWord => Functional
       | IntInf_xorb => Functional
       | MLton_bogus => Functional
       | MLton_bug => SideEffect
       | MLton_deserialize => Moveable
       | MLton_eq => Functional
       | MLton_equal => Functional
       | MLton_halt => SideEffect
       | MLton_hash => Functional
       | MLton_handlesSignals => Functional
       | MLton_installSignalHandler => SideEffect
       | MLton_serialize => DependsOnState
       | MLton_share => SideEffect
       | MLton_size => DependsOnState
       | MLton_touch => SideEffect
       | Real_Math_acos _ => DependsOnState (* depends on rounding mode *)
       | Real_Math_asin _ => DependsOnState (* depends on rounding mode *)
       | Real_Math_atan _ => DependsOnState (* depends on rounding mode *)
       | Real_Math_atan2 _ => DependsOnState (* depends on rounding mode *)
       | Real_Math_cos _ => DependsOnState (* depends on rounding mode *)
       | Real_Math_exp _ => DependsOnState (* depends on rounding mode *)
       | Real_Math_ln _ => DependsOnState (* depends on rounding mode *)
       | Real_Math_log10 _ => DependsOnState (* depends on rounding mode *)
       | Real_Math_sin _ => DependsOnState (* depends on rounding mode *)
       | Real_Math_sqrt _ => DependsOnState (* depends on rounding mode *)
       | Real_Math_tan _ => DependsOnState (* depends on rounding mode *)
       | Real_abs _ => Functional
       | Real_add _ => DependsOnState (* depends on rounding mode *)
       | Real_castToWord _ => Functional
       | Real_div _ => DependsOnState (* depends on rounding mode *)
       | Real_equal _ => Functional
       | Real_ldexp _ => DependsOnState (* depends on rounding mode *)
       | Real_le _ => Functional
       | Real_lt _ => Functional
       | Real_mul _ => DependsOnState (* depends on rounding mode *)
       | Real_muladd _ => DependsOnState (* depends on rounding mode *)
       | Real_mulsub _ => DependsOnState (* depends on rounding mode *)
       | Real_neg _ => Functional
       | Real_qequal _ => Functional
       | Real_rndToReal _ => DependsOnState (* depends on rounding mode *)
       | Real_rndToWord _ => Functional
       | Real_round _ => DependsOnState (* depends on rounding mode *)
       | Real_sub _ => DependsOnState (* depends on rounding mode *)
       | Ref_assign => SideEffect
       | Ref_deref => DependsOnState
       | Ref_ref => Moveable
       | String_toWord8Vector => Functional
       | Thread_atomicBegin => SideEffect
       | Thread_atomicEnd => SideEffect
       | Thread_atomicState => DependsOnState
       | Thread_copy => Moveable
       | Thread_copyCurrent => SideEffect
       | Thread_returnToC => SideEffect
       | Thread_switchTo => SideEffect
       | TopLevel_getHandler => DependsOnState
       | TopLevel_getSuffix => DependsOnState
       | TopLevel_setHandler => SideEffect
       | TopLevel_setSuffix => SideEffect
       | Vector_length => Functional
       | Vector_sub => Functional
       | Vector_vector => Functional
       | Weak_canGet => DependsOnState
       | Weak_get => DependsOnState
       | Weak_new => Moveable
       | WordArray_subWord _ => DependsOnState
       | WordArray_updateWord _ => SideEffect
       | WordVector_subWord _ => Functional
       | Word8Vector_toString => Functional
       | WordVector_toIntInf => Functional
       | Word_add _ => Functional
       | Word_addCheck _ => SideEffect
       | Word_andb _ => Functional
       | Word_castToReal _ => Functional
       | Word_equal _ => Functional
       | Word_extdToWord _ => Functional
       | Word_lshift _ => Functional
       | Word_lt _ => Functional
       | Word_mul _ => Functional
       | Word_mulCheck _ => SideEffect
       | Word_neg _ => Functional
       | Word_negCheck _ => SideEffect
       | Word_notb _ => Functional
       | Word_orb _ => Functional
       | Word_quot _ => Functional
       | Word_rem _ => Functional
       | Word_rndToReal _ => DependsOnState (* depends on rounding mode *)
       | Word_rol _ => Functional
       | Word_ror _ => Functional
       | Word_rshift _ => Functional
       | Word_sub _ => Functional
       | Word_subCheck _ => SideEffect
       | Word_toIntInf => Functional
       | Word_xorb _ => Functional
       | World_save => SideEffect
   end

fun isFunctional p = Kind.Functional = kind p

fun maySideEffect p = Kind.SideEffect = kind p

local
   fun reals (s: RealSize.t) =
      [(Real_Math_acos s),
       (Real_Math_asin s),
       (Real_Math_atan s),
       (Real_Math_atan2 s),
       (Real_Math_cos s),
       (Real_Math_exp s),
       (Real_Math_ln s),
       (Real_Math_log10 s),
       (Real_Math_sin s),
       (Real_Math_sqrt s),
       (Real_Math_tan s),
       (Real_abs s),
       (Real_add s),
       (Real_div s),
       (Real_equal s),
       (Real_ldexp s),
       (Real_le s),
       (Real_lt s),
       (Real_mul s),
       (Real_muladd s),
       (Real_mulsub s),
       (Real_neg s),
       (Real_qequal s),
       (Real_round s),
       (Real_sub s)]

   fun wordSigns (s: WordSize.t, signed: bool) =
      let
         val sg = {signed = signed}
      in
         List.map ([Word_addCheck,
                    Word_lt,
                    Word_mul,
                    Word_mulCheck,
                    Word_quot,
                    Word_rem,
                    Word_rshift,
                    Word_subCheck],
                   fn p => p (s, sg))
      end

   fun words (s: WordSize.t) =
      [(Word_add s),
       (Word_andb s),
       (Word_equal s),
       (Word_lshift s),
       (Word_neg s),
       (Word_negCheck s),
       (Word_notb s),
       (Word_orb s),
       (Word_rol s),
       (Word_ror s),
       (Word_sub s),
       (Word_xorb s)]
      @ wordSigns (s, true)
      @ wordSigns (s, false)
in
   val all: unit t list =
      [Array_alloc {raw = false},
       Array_alloc {raw = true},
       Array_copyArray,
       Array_copyVector,
       Array_length,
       Array_sub,
       Array_toArray,
       Array_toVector,
       Array_uninit,
       Array_uninitIsNop,
       Array_update,
       CPointer_add,
       CPointer_diff,
       CPointer_equal,
       CPointer_fromWord,
       CPointer_getCPointer,
       CPointer_getObjptr,
       CPointer_lt,
       CPointer_setCPointer,
       CPointer_setObjptr,
       CPointer_sub,
       CPointer_toWord,
       Exn_extra,
       Exn_name,
       Exn_setExtendExtra,
       GC_collect,
       IntInf_add,
       IntInf_andb,
       IntInf_arshift,
       IntInf_compare,
       IntInf_gcd,
       IntInf_lshift,
       IntInf_mul,
       IntInf_notb,
       IntInf_neg,
       IntInf_orb,
       IntInf_quot,
       IntInf_rem,
       IntInf_sub,
       IntInf_toString,
       IntInf_toVector,
       IntInf_toWord,
       IntInf_xorb,
       MLton_bogus,
       MLton_bug,
       MLton_deserialize,
       MLton_eq,
       MLton_equal,
       MLton_halt,
       MLton_hash,
       MLton_handlesSignals,
       MLton_installSignalHandler,
       MLton_serialize,
       MLton_share,
       MLton_size,
       MLton_touch,
       Ref_assign,
       Ref_deref,
       Ref_ref,
       String_toWord8Vector,
       Thread_atomicBegin,
       Thread_atomicEnd,
       Thread_atomicState,
       Thread_copy,
       Thread_copyCurrent,
       Thread_returnToC,
       Thread_switchTo,
       TopLevel_getHandler,
       TopLevel_getSuffix,
       TopLevel_setHandler,
       TopLevel_setSuffix,
       Vector_length,
       Vector_sub,
       Vector_vector,
       Weak_canGet,
       Weak_get,
       Weak_new,
       Word_toIntInf,
       WordVector_toIntInf,
       Word8Vector_toString,
       World_save]
      @ List.concat [List.concatMap (RealSize.all, reals),
                     List.concatMap (WordSize.prims, words)]
      @ let
           val real = RealSize.all
           val word = WordSize.prims
           val wordNonPrim =
              List.keepAll
              (WordSize.all, fn s => not (List.contains (word, s, WordSize.equals)))
           fun coerces (name, sizes, sizes', ac) =
              List.fold
              (sizes, ac, fn (s, ac) =>
               List.fold 
               (sizes', ac, fn (s', ac) =>
                name (s, s') :: ac))
           fun coercesS (name, sizes, sizes', ac) =
              List.fold
              ([false, true], ac, fn (signed, ac) =>
               coerces (fn (s, s') => name (s, s', {signed = signed}),
                        sizes, sizes', ac))
           fun casts (name, sizes, ac) =
              List.fold (sizes, ac, fn (s, ac) => name s :: ac)
           fun castsS (name, sizes, ac) =
              List.fold
              ([false, true], ac, fn (signed, ac) =>
               casts (fn s => name (s, {signed = signed}),
                      sizes, ac))
        in
           casts (fn rs => Real_castToWord (rs, WordSize.fromBits (RealSize.bits rs)), real, 
           coerces (Real_rndToReal, real, real,
           coercesS (Real_rndToWord, real, word,
           casts (fn rs => Word_castToReal (WordSize.fromBits (RealSize.bits rs), rs), real,
           coercesS (Word_extdToWord, word, word,
           castsS (fn (s, signed) => Word_extdToWord (s, WordSize.roundUpToPrim s, signed), wordNonPrim,
           castsS (fn (s, signed) => Word_extdToWord (WordSize.roundUpToPrim s, s, signed), wordNonPrim,
           coercesS (Word_rndToReal, word, real, []))))))))
        end
     @ List.concatMap
       (WordSize.prims, fn seqSize =>
        List.concatMap
        (WordSize.prims, fn eleSize =>
         List.map
         ([WordArray_subWord, WordArray_updateWord, WordVector_subWord], fn p =>
          p {seqSize = seqSize, eleSize = eleSize})))
     @ let
          fun doit (all, get, set) =
             List.concatMap (all, fn s => [get s, set s])
       in
          List.concat [doit (RealSize.all, CPointer_getReal, CPointer_setReal),
                       doit (WordSize.prims, CPointer_getWord, CPointer_setWord)]
       end
end

local
   val table: {hash: word,
               prim: unit t,
               string: string} HashSet.t =
      HashSet.new {hash = #hash}
   val () =
      List.foreach (all, fn prim =>
                    let
                       val string = toString prim
                       val hash = String.hash string
                       val _ =
                          HashSet.lookupOrInsert (table, hash,
                                                  fn _ => false,
                                                  fn () => {hash = hash,
                                                            prim = prim,
                                                            string = string})
                    in
                       ()
                    end)
in
   val fromString: string -> 'a t option =
      fn name =>
      Option.map
      (HashSet.peek
       (table, String.hash name, fn {string, ...} => name = string),
       fn {prim, ...} => cast prim)
end

fun 'a checkApp (prim: 'a t,
                 {args: 'a vector,
                  result: 'a,
                  targs: 'a vector,
                  typeOps = {array: 'a -> 'a,
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
                             word: WordSize.t -> 'a}}): bool =
   let
      fun arg i = Vector.sub (args, i)
      fun noArgs () =
         0 = Vector.length args
      fun oneArg arg0' () =
         1 = Vector.length args
         andalso equals (arg0', arg 0)
      fun twoArgs (arg0', arg1') () =
         2 = Vector.length args
         andalso equals (arg0', arg 0)
         andalso equals (arg1', arg 1)
      fun threeArgs (arg0', arg1', arg2') () =
         3 = Vector.length args
         andalso equals (arg0', arg 0)
         andalso equals (arg1', arg 1)
         andalso equals (arg2', arg 2)
      fun fiveArgs (arg0', arg1', arg2', arg3', arg4') () =
         5 = Vector.length args
         andalso equals (arg0', arg 0)
         andalso equals (arg1', arg 1)
         andalso equals (arg2', arg 2)
         andalso equals (arg3', arg 3)
         andalso equals (arg4', arg 4)
      fun nArgs args' () =
         Vector.equals (args', args, equals)
      fun done (args, result') =
         args () andalso equals (result', result)
      fun targ i = Vector.sub (targs, i)
      fun noTargs f =
         0 = Vector.length targs
         andalso done (f ())
      fun oneTarg f =
         1 = Vector.length targs
         andalso done (f (targ 0))
      local
         fun make f s = let val t = f s
                        in noTargs (fn () => (oneArg t, t))
                        end
      in
         val realUnary = make real
         val wordUnary = make word
      end
      local
         fun make f s = let val t = f s
                        in noTargs (fn () => (twoArgs (t, t), t))
                        end
      in
         val realBinary = make real
         val wordBinary = make word
      end
      local
         fun make f s = let val t = f s
                        in noTargs (fn () => (twoArgs (t, t), bool))
                        end
      in
         val realCompare = make real
         val wordCompare = make word
      end
      val cint = word (WordSize.cint ())
      val compareRes = word WordSize.compareRes
      val csize = word (WordSize.csize ())
      val cptrdiff = word (WordSize.cptrdiff ())
      val seqIndex = word (WordSize.seqIndex ())
      val shiftArg = word WordSize.shiftArg
      val bigIntInfWord = word (WordSize.bigIntInfWord ())
      val smallIntInfWord = word (WordSize.smallIntInfWord ())

      val word8 = word WordSize.word8
      val word32 = word WordSize.word32
      fun intInfBinary () =
         noTargs (fn () => (threeArgs (intInf, intInf, csize), intInf))
      fun intInfShift () =
         noTargs (fn () => (threeArgs (intInf, shiftArg, csize), intInf))
      fun intInfUnary () =
         noTargs (fn () => (twoArgs (intInf, csize), intInf))
      fun realTernary s =
         noTargs (fn () => (threeArgs (real s, real s, real s), real s))
      fun wordArray seqSize = array (word seqSize)
      fun wordShift s =
         noTargs (fn () => (twoArgs (word s, shiftArg), word s))
      val word8Vector = vector word8
      fun wordVector seqSize = vector (word seqSize)
      val string = word8Vector
  in
      case prim of
         Array_alloc _ => oneTarg (fn targ => (oneArg seqIndex, array targ))
       | Array_copyArray => oneTarg (fn t => (fiveArgs (array t, seqIndex, array t, seqIndex, seqIndex), unit))
       | Array_copyVector => oneTarg (fn t => (fiveArgs (array t, seqIndex, vector t, seqIndex, seqIndex), unit))
       | Array_length => oneTarg (fn t => (oneArg (array t), seqIndex))
       | Array_sub => oneTarg (fn t => (twoArgs (array t, seqIndex), t))
       | Array_toArray => oneTarg (fn t => (oneArg (array t), array t))
       | Array_toVector => oneTarg (fn t => (oneArg (array t), vector t))
       | Array_uninit =>
            oneTarg (fn t => (twoArgs (array t, seqIndex), unit))
       | Array_uninitIsNop =>
            oneTarg (fn t => (oneArg (array t), bool))
       | Array_update =>
            oneTarg (fn t => (threeArgs (array t, seqIndex, t), unit))
       | CPointer_add =>
            noTargs (fn () => (twoArgs (cpointer, cptrdiff), cpointer))
       | CPointer_diff =>
            noTargs (fn () => (twoArgs (cpointer, cpointer), cptrdiff))
       | CPointer_equal =>
            noTargs (fn () => (twoArgs (cpointer, cpointer), bool))
       | CPointer_fromWord => noTargs (fn () => (oneArg (csize), cpointer))
       | CPointer_getCPointer =>
            noTargs (fn () => (twoArgs (cpointer, cptrdiff), cpointer))
       | CPointer_getObjptr =>
            oneTarg (fn t => (twoArgs (cpointer, cptrdiff), t))
       | CPointer_getReal s =>
            noTargs (fn () => (twoArgs (cpointer, cptrdiff), real s))
       | CPointer_getWord s =>
            noTargs (fn () => (twoArgs (cpointer, cptrdiff), word s))
       | CPointer_lt =>
            noTargs (fn () => (twoArgs (cpointer, cpointer), bool))
       | CPointer_setCPointer =>
            noTargs (fn () => (threeArgs (cpointer, cptrdiff, cpointer),
                               unit))
       | CPointer_setObjptr =>
            oneTarg (fn t => (threeArgs (cpointer, cptrdiff, t), unit))
       | CPointer_setReal s =>
            noTargs (fn () => (threeArgs (cpointer, cptrdiff, real s), unit))
       | CPointer_setWord s =>
            noTargs (fn () => (threeArgs (cpointer, cptrdiff, word s), unit))
       | CPointer_sub =>
            noTargs (fn () => (twoArgs (cpointer, cptrdiff), cpointer))
       | CPointer_toWord => noTargs (fn () => (oneArg cpointer, csize))
       | Exn_extra => oneTarg (fn t => (oneArg exn, t))
       | Exn_name => noTargs (fn () => (oneArg exn, string))
       | Exn_setExtendExtra => oneTarg (fn t => (oneArg (arrow (t, t)), unit))
       | FFI f =>
            noTargs (fn () => (nArgs (CFunction.args f), CFunction.return f))
       | FFI_Symbol _ => noTargs (fn () => (noArgs, cpointer))
       | GC_collect => noTargs (fn () => (noArgs, unit))
       | IntInf_add => intInfBinary ()
       | IntInf_andb => intInfBinary ()
       | IntInf_arshift => intInfShift ()
       | IntInf_compare =>
            noTargs (fn () => (twoArgs (intInf, intInf), compareRes))
       | IntInf_gcd => intInfBinary ()
       | IntInf_lshift => intInfShift ()
       | IntInf_mul => intInfBinary ()
       | IntInf_neg => intInfUnary ()
       | IntInf_notb => intInfUnary ()
       | IntInf_orb => intInfBinary ()
       | IntInf_quot => intInfBinary ()
       | IntInf_rem => intInfBinary ()
       | IntInf_sub => intInfBinary ()
       | IntInf_toString =>
            noTargs (fn () => (threeArgs (intInf, word32, csize), string))
       | IntInf_toVector =>
            noTargs (fn () => (oneArg intInf, vector bigIntInfWord))
       | IntInf_toWord => noTargs (fn () => (oneArg intInf, smallIntInfWord))
       | IntInf_xorb => intInfBinary ()
       | MLton_bogus => oneTarg (fn t => (noArgs, t))
       | MLton_bug => noTargs (fn () => (oneArg string, unit))
       | MLton_deserialize => oneTarg (fn t => (oneArg word8Vector, t))
       | MLton_eq => oneTarg (fn t => (twoArgs (t, t), bool))
       | MLton_equal => oneTarg (fn t => (twoArgs (t, t), bool))
       | MLton_halt => noTargs (fn () => (oneArg cint, unit))
       | MLton_hash => oneTarg (fn t => (oneArg t, word32))
       | MLton_handlesSignals => noTargs (fn () => (noArgs, bool))
       | MLton_installSignalHandler => noTargs (fn () => (noArgs, unit))
       | MLton_serialize => oneTarg (fn t => (oneArg t, word8Vector))
       | MLton_share => oneTarg (fn t => (oneArg t, unit))
       | MLton_size => oneTarg (fn t => (oneArg t, csize))
       | MLton_touch => oneTarg (fn t => (oneArg t, unit))
       | Real_Math_acos s => realUnary s
       | Real_Math_asin s => realUnary s
       | Real_Math_atan s => realUnary s
       | Real_Math_atan2 s => realBinary s
       | Real_Math_cos s => realUnary s
       | Real_Math_exp s => realUnary s
       | Real_Math_ln s => realUnary s
       | Real_Math_log10 s => realUnary s
       | Real_Math_sin s => realUnary s
       | Real_Math_sqrt s => realUnary s
       | Real_Math_tan s => realUnary s
       | Real_abs s => realUnary s
       | Real_add s => realBinary s
       | Real_castToWord (s, s') =>
            noTargs (fn () => (oneArg (real s), word s'))
       | Real_div s => realBinary s
       | Real_equal s => realCompare s
       | Real_ldexp s => noTargs (fn () => (twoArgs (real s, cint), real s))
       | Real_le s => realCompare s
       | Real_lt s => realCompare s
       | Real_mul s => realBinary s
       | Real_muladd s => realTernary s
       | Real_mulsub s => realTernary s
       | Real_neg s => realUnary s
       | Real_qequal s => realCompare s
       | Real_rndToReal (s, s') =>
            noTargs (fn () => (oneArg (real s), real s'))
       | Real_rndToWord (s, s', _) =>
            noTargs (fn () => (oneArg (real s), word s'))
       | Real_round s => realUnary s
       | Real_sub s => realBinary s
       | Ref_assign => oneTarg (fn t => (twoArgs (reff t, t), unit))
       | Ref_deref => oneTarg (fn t => (oneArg (reff t), t))
       | Ref_ref => oneTarg (fn t => (oneArg t, reff t))
       | Thread_atomicBegin => noTargs (fn () => (noArgs, unit))
       | Thread_atomicEnd => noTargs (fn () => (noArgs, unit))
       | Thread_atomicState => noTargs (fn () => (noArgs, word32))
       | Thread_copy => noTargs (fn () => (oneArg thread, thread))
       | Thread_copyCurrent => noTargs (fn () => (noArgs, unit))
       | Thread_returnToC => noTargs (fn () => (noArgs, unit))
       | Thread_switchTo => noTargs (fn () => (oneArg thread, unit))
       | TopLevel_getHandler => noTargs (fn () => (noArgs, arrow (exn, unit)))
       | TopLevel_getSuffix => noTargs (fn () => (noArgs, arrow (unit, unit)))
       | TopLevel_setHandler =>
            noTargs (fn () => (oneArg (arrow (exn, unit)), unit))
       | TopLevel_setSuffix =>
            noTargs (fn () => (oneArg (arrow (unit, unit)), unit))
       | String_toWord8Vector =>
            noTargs (fn () => (oneArg string, word8Vector))
       | Vector_length => oneTarg (fn t => (oneArg (vector t), seqIndex))
       | Vector_sub => oneTarg (fn t => (twoArgs (vector t, seqIndex), t))
       | Vector_vector => oneTarg (fn targ => (nArgs (Vector.map (args, fn _ => targ)), vector targ))
       | Weak_canGet => oneTarg (fn t => (oneArg (weak t), bool))
       | Weak_get => oneTarg (fn t => (oneArg (weak t), t))
       | Weak_new => oneTarg (fn t => (oneArg t, weak t))
       | WordArray_subWord {seqSize, eleSize} =>
            noTargs (fn () => (twoArgs (wordArray seqSize, seqIndex), word eleSize))
       | WordArray_updateWord {seqSize, eleSize} =>
            noTargs (fn () => (threeArgs (wordArray seqSize, seqIndex, word eleSize), unit))
       | WordVector_subWord {seqSize, eleSize} =>
            noTargs (fn () => (twoArgs (wordVector seqSize, seqIndex), word eleSize))
       | Word8Vector_toString =>
            noTargs (fn () => (oneArg (word8Vector), string))
       | WordVector_toIntInf =>
            noTargs (fn () => (oneArg (vector bigIntInfWord), intInf))
       | Word_add s => wordBinary s
       | Word_addCheck (s, _) => wordBinary s
       | Word_andb s => wordBinary s
       | Word_castToReal (s, s') =>
            noTargs (fn () => (oneArg (word s), real s'))
       | Word_equal s => wordCompare s
       | Word_extdToWord (s, s', _) =>
            noTargs (fn () => (oneArg (word s), word s'))
       | Word_lshift s => wordShift s
       | Word_lt (s, _) => wordCompare s
       | Word_mul (s, _) => wordBinary s
       | Word_mulCheck (s, _) => wordBinary s
       | Word_neg s => wordUnary s
       | Word_negCheck s => wordUnary s
       | Word_notb s => wordUnary s
       | Word_orb s => wordBinary s
       | Word_quot (s, _) => wordBinary s
       | Word_rem (s, _) => wordBinary s
       | Word_rndToReal (s, s', _) =>
            noTargs (fn () => (oneArg (word s), real s'))
       | Word_rol s => wordShift s
       | Word_ror s => wordShift s
       | Word_rshift (s, _) => wordShift s
       | Word_sub s => wordBinary s
       | Word_subCheck (s, _) => wordBinary s
       | Word_toIntInf => noTargs (fn () => (oneArg smallIntInfWord, intInf))
       | Word_xorb s => wordBinary s
       | World_save => noTargs (fn () => (oneArg string, unit))
   end

val checkApp =
   fn z =>
   Trace.trace ("Prim.check", layout o #1, Layout.ignore) checkApp z

fun ('a, 'b) extractTargs (prim: 'b t,
                           {args: 'a vector,
                            result: 'a,
                            typeOps = {deArray: 'a -> 'a,
                                       deArrow: 'a -> 'a * 'a,
                                       deRef: 'a -> 'a,
                                       deVector: 'a -> 'a,
                                       deWeak: 'a -> 'a}}) =
   let
      val one = Vector.new1
      fun arg i = Vector.sub (args, i)
      datatype z = datatype t
   in
      case prim of
         Array_alloc _ => one (deArray result)
       | Array_copyArray => one (deArray (arg 0))
       | Array_copyVector => one (deArray (arg 0))
       | Array_length => one (deArray (arg 0))
       | Array_sub => one (deArray (arg 0))
       | Array_toArray => one (deArray (arg 0))
       | Array_toVector => one (deArray (arg 0))
       | Array_uninit => one (deArray (arg 0))
       | Array_uninitIsNop => one (deArray (arg 0))
       | Array_update => one (deArray (arg 0))
       | CPointer_getObjptr => one result
       | CPointer_setObjptr => one (arg 2)
       | Exn_extra => one result
       | Exn_setExtendExtra => one (#2 (deArrow (arg 0)))
       | MLton_bogus => one result
       | MLton_deserialize => one result
       | MLton_eq => one (arg 0)
       | MLton_equal => one (arg 0)
       | MLton_hash => one (arg 0)
       | MLton_serialize => one (arg 0)
       | MLton_share => one (arg 0)
       | MLton_size => one (arg 0)
       | MLton_touch => one (arg 0)
       | Ref_assign => one (deRef (arg 0))
       | Ref_deref => one (deRef (arg 0))
       | Ref_ref => one (deRef result)
       | Vector_length => one (deVector (arg 0))
       | Vector_sub => one (deVector (arg 0))
       | Vector_vector => one (deVector result)
       | Weak_canGet => one (deWeak (arg 0))
       | Weak_get => one result
       | Weak_new => one (arg 0)
       | _ => Vector.new0 ()
   end

val extractTargs =
   fn z =>
   Trace.trace ("Prim.extractTargs", layout o #1, Layout.ignore) extractTargs z

structure IntInfRep = Const.IntInfRep

structure ApplyArg =
   struct
      datatype 'a t =
         Con of {con: Con.t, hasArg: bool}
       | Const of Const.t
       | Var of 'a

      fun layout layoutX =
         fn Con {con, hasArg} =>
              Layout.record [("con", Con.layout con),
                             ("hasArg", Bool.layout hasArg)]
          | Const c => Const.layout c
          | Var x => layoutX x
   end

structure ApplyResult =
   struct
      type 'a prim = 'a t
      val layoutPrim = layout

      datatype ('a, 'b) t =
         Apply of 'a prim * 'b list
       | Bool of bool
       | Const of Const.t
       | Overflow
       | Unknown
       | Var of 'b

      fun layout layoutX ar =
         let
            open Layout
         in
            case ar of
               Apply (p, args) => seq [layoutPrim p, List.layout layoutX args]
             | Bool b => Bool.layout b
             | Const c => Const.layout c
             | Overflow => str "Overflow"
             | Unknown => str "Unknown"
             | Var x => layoutX x
         end
   end

(*
 * In addition to constant folding, here are the algebraic identities currently
 * handled.
 *
 * x * 1 = 1 * x = x
 * x * ~1 = ~1 * x = ~x
 * x * 0 = 0 * x = 0
 * x + 0 = 0 + x = x
 * x mod x = x rem x = 0
 * x mod 1 = x rem 1 = x mod ~1 = x rem ~1 = 0
 * x div x = x quot x = 1
 * x div 1 = x quot 1 = x
 * andb (x, x) = orb (x, x) = x
 * xorb (x, x) = 0
 * x - 0 = x
 * 0 - x = ~x
 * x - x = 0
 * x > x = x < x = false
 * x >= x = x <= x = true
 * x = x --> true
 *
 * Also, simple equality tests on constructors are handled.
 * A = A --> true
 * A = B --> false
 * A x = B y --> false
 *)

fun ('a, 'b) apply (p: 'a t,
                    args: 'b ApplyArg.t list,
                    varEquals: 'b * 'b -> bool): ('a, 'b) ApplyResult.t =
   let
      datatype z = datatype t
      datatype z = datatype Const.t
      val bool = ApplyResult.Bool
      val boolOpt = fn NONE => ApplyResult.Unknown | SOME b => bool b
      val f = bool false
      val t = bool true
      fun seqIndexConst i =
         ApplyResult.Const
         (Const.word (WordX.fromIntInf (i, WordSize.seqIndex ())))
      local
         val maxIntInf = IntInf.<< (1, 0w128)
         val minIntInf = IntInf.~ maxIntInf
      in
         fun intInfTooBig ii =
            IntInf.< (ii, minIntInf)
            orelse IntInf.> (ii, maxIntInf)
      end
      val intInfTooBig =
         Trace.trace 
         ("Prim.intInfTooBig", IntInf.layout, Bool.layout)
         intInfTooBig
      fun intInf (ii:  IntInf.t): ('a, 'b) ApplyResult.t =
         if intInfTooBig ii
            then ApplyResult.Unknown
         else ApplyResult.Const (Const.intInf ii)
      val intInfConst = intInf o IntInf.fromInt
      val null = ApplyResult.Const Const.null
      fun real (r: RealX.t): ('a, 'b) ApplyResult.t =
         ApplyResult.Const (Const.real r)
      val realOpt = fn NONE => ApplyResult.Unknown | SOME r => real r
      fun realNeg (s, x): ('a, 'b) ApplyResult.t =
          ApplyResult.Apply (Real_neg s, [x])
      fun realAdd (s, x, y): ('a, 'b) ApplyResult.t =
          ApplyResult.Apply (Real_add s, [x, y])
      fun word (w: WordX.t): ('a, 'b) ApplyResult.t =
         ApplyResult.Const (Const.word w)
      val wordOpt = fn NONE => ApplyResult.Unknown | SOME w => word w
      fun wordVector (v: WordXVector.t): ('a, 'b) ApplyResult.t =
         ApplyResult.Const (Const.wordVector v)
      fun iio (f, c1, c2) = intInf (f (c1, c2))
      fun wordS (f: WordX.t * WordX.t * {signed: bool} -> WordX.t,
                 (_: WordSize.t, sg),
                 w: WordX.t,
                 w': WordX.t) =
         word (f (w, w', sg))
      fun wordCmp (f: WordX.t * WordX.t * {signed: bool} -> bool,
                   (_: WordSize.t, sg),
                   w: WordX.t,
                   w': WordX.t) =
         bool (f (w, w', sg))
      fun wordOrOverflow (s, sg, w) =
         if WordSize.isInRange (s, w, sg)
            then word (WordX.fromIntInf (w, s))
         else ApplyResult.Overflow
      fun wcheck (f: IntInf.t * IntInf.t -> IntInf.t,
                  (s: WordSize.t, sg as {signed}),
                  w: WordX.t,
                  w': WordX.t) =
         let
            val conv = if signed then WordX.toIntInfX else WordX.toIntInf
         in
            wordOrOverflow (s, sg, f (conv w, conv w'))
         end
      val eq =
         fn (Word w1, Word w2) => bool (WordX.equals (w1, w2))
          | _ => ApplyResult.Unknown
      val equal =
         fn (IntInf ii1, IntInf ii2) => bool (IntInf.equals (ii1, ii2))
          | (Word w1, Word w2) => bool (WordX.equals (w1, w2))
          | (WordVector v1, WordVector v2) => bool (WordXVector.equals (v1, v2))
          | _ => ApplyResult.Unknown
      fun intInfBinary (i1, i2) =
         if intInfTooBig i1 orelse intInfTooBig i2
            then ApplyResult.Unknown
         else 
            case p of
               IntInf_add => iio (IntInf.+, i1, i2)
             | IntInf_andb => iio (IntInf.andb, i1, i2)
             | IntInf_gcd => iio (IntInf.gcd, i1, i2)
             | IntInf_mul => iio (IntInf.*, i1, i2)
             | IntInf_orb => iio (IntInf.orb, i1, i2)
             | IntInf_quot => iio (IntInf.quot, i1, i2)
             | IntInf_rem => iio (IntInf.rem, i1, i2)
             | IntInf_sub => iio (IntInf.-, i1, i2)
             | IntInf_xorb => iio (IntInf.xorb, i1, i2)
             | _ => ApplyResult.Unknown
      fun intInfUnary (i1) =
         if intInfTooBig i1
            then ApplyResult.Unknown
         else 
            case p of
               IntInf_neg => intInf (IntInf.~ i1)
             | IntInf_notb => intInf (IntInf.notb i1)
             | _ => ApplyResult.Unknown
      fun intInfShiftOrToString (i1, w2) =
         if intInfTooBig i1
            then ApplyResult.Unknown
         else 
            case p of
               IntInf_arshift =>
                  intInf (IntInf.~>> (i1, Word.fromIntInf (WordX.toIntInf w2)))
             | IntInf_lshift =>
                  let
                     val maxShift =
                        WordX.fromIntInf (128, WordSize.shiftArg)
                  in
                     if WordX.lt (w2, maxShift, {signed = false})
                        then intInf (IntInf.<< (i1, Word.fromIntInf (WordX.toIntInf w2)))
                     else ApplyResult.Unknown
                  end
             | IntInf_toString =>
                  let
                     val base =
                        case WordX.toInt w2 of
                           2 => StringCvt.BIN
                         | 8 => StringCvt.OCT 
                         | 10 => StringCvt.DEC
                         | 16 => StringCvt.HEX
                         | _ => Error.bug "Prim.apply: strange base for IntInf_toString"
                  in
                     ApplyResult.Const (Const.string (IntInf.format (i1, base)))
                  end
             | _ => ApplyResult.Unknown
      fun allConsts (cs: Const.t list) =
         (case (p, cs) of
             (MLton_eq, [c1, c2]) => eq (c1, c2)
           | (MLton_equal, [c1, c2]) => equal (c1, c2)
           | (CPointer_equal, [Null, Null]) => bool true
           | (CPointer_fromWord, [Word w]) =>
                 if WordX.isZero w
                    then null
                 else ApplyResult.Unknown
           | (CPointer_toWord, [Null]) => word (WordX.zero (WordSize.cpointer ()))
           | (IntInf_compare, [IntInf i1, IntInf i2]) =>
                let
                   val i =
                      case IntInf.compare (i1, i2) of
                         Relation.LESS => ~1
                       | Relation.EQUAL => 0
                       | Relation.GREATER => 1
                in
                   word (WordX.fromIntInf (i, WordSize.compareRes))
                end
           | (IntInf_toWord, [IntInf i]) =>
                (case IntInfRep.fromIntInf i of
                    IntInfRep.Big _ => ApplyResult.Unknown
                  | IntInfRep.Small w => word w)
           | (IntInf_toVector, [IntInf i]) =>
                (case IntInfRep.fromIntInf i of
                    IntInfRep.Big v => wordVector v
                  | IntInfRep.Small _ => ApplyResult.Unknown)
           | (_, [IntInf i1, IntInf i2, _]) => intInfBinary (i1, i2)
           | (_, [IntInf i1, Word w2, _]) => intInfShiftOrToString (i1, w2)
           | (_, [IntInf i1, _]) => intInfUnary (i1)
           | (Vector_length, [WordVector v]) =>
                 seqIndexConst (IntInf.fromInt (WordXVector.length v))
           | (Vector_sub, [WordVector v, Word i]) =>
                 word (WordXVector.sub (v, WordX.toInt i))
           | (Real_neg _, [Real r]) => realOpt (RealX.neg r)
           | (Real_abs _, [Real r]) => realOpt (RealX.abs r)
           | (Real_Math_acos _, [Real r]) => realOpt (RealX.acos r)
           | (Real_Math_asin _, [Real r]) => realOpt (RealX.asin r)
           | (Real_Math_atan _, [Real r]) => realOpt (RealX.atan r)
           | (Real_Math_atan2 _, [Real r1, Real r2]) =>
                realOpt (RealX.atan2 (r1, r2))
           | (Real_Math_cos _, [Real r]) => realOpt (RealX.cos r)
           | (Real_Math_exp _, [Real r]) => realOpt (RealX.exp r)
           | (Real_Math_ln _, [Real r]) => realOpt (RealX.ln r)
           | (Real_Math_log10 _, [Real r]) => realOpt (RealX.log10 r)
           | (Real_Math_sin _, [Real r]) => realOpt (RealX.sin r)
           | (Real_Math_sqrt _, [Real r]) => realOpt (RealX.sqrt r)
           | (Real_Math_tan _, [Real r]) => realOpt (RealX.tan r)
           | (Real_add _, [Real r1, Real r2]) => realOpt (RealX.add (r1, r2))
           | (Real_div _, [Real r1, Real r2]) => realOpt (RealX.div (r1, r2))
           | (Real_mul _, [Real r1, Real r2]) => realOpt (RealX.mul (r1, r2))
           | (Real_sub _, [Real r1, Real r2]) => realOpt (RealX.sub (r1, r2))
           | (Real_muladd _, [Real r1, Real r2, Real r3]) =>
                realOpt (RealX.muladd (r1, r2, r3))
           | (Real_mulsub _, [Real r1, Real r2, Real r3]) =>
                realOpt (RealX.mulsub (r1, r2, r3))
           | (Real_equal _, [Real r1, Real r2]) => boolOpt (RealX.equal (r1, r2))
           | (Real_le _, [Real r1, Real r2]) => boolOpt (RealX.le (r1, r2))
           | (Real_lt _, [Real r1, Real r2]) => boolOpt (RealX.lt (r1, r2))
           | (Real_qequal _, [Real r1, Real r2]) => boolOpt (RealX.qequal (r1, r2))
           | (Real_castToWord _, [Real r]) => wordOpt (RealX.castToWord r)
           | (Vector_vector, (Word w)::_) =>
                (wordVector o WordXVector.fromList)
                ({elementSize = WordX.size w},
                 List.map (cs, Const.deWord))
           | (Word_castToReal _, [Word w]) => realOpt (RealX.castFromWord w)
           | (Word_rndToReal (_, s, {signed}), [Word w]) =>
                realOpt
                (RealX.fromIntInf
                 (if signed then WordX.toIntInfX w else WordX.toIntInf w, s))
           | (Word_add _, [Word w1, Word w2]) => word (WordX.add (w1, w2))
           | (Word_addCheck s, [Word w1, Word w2]) => wcheck (op +, s, w1, w2)
           | (Word_andb _, [Word w1, Word w2]) => word (WordX.andb (w1, w2))
           | (Word_equal _, [Word w1, Word w2]) => bool (WordX.equals (w1, w2))
           | (Word_lshift _, [Word w1, Word w2]) => word (WordX.lshift (w1, w2))
           | (Word_lt s, [Word w1, Word w2]) => wordCmp (WordX.lt, s, w1, w2)
           | (Word_mul s, [Word w1, Word w2]) => wordS (WordX.mul, s, w1, w2)
           | (Word_mulCheck s, [Word w1, Word w2]) => wcheck (op *, s, w1, w2)
           | (Word_neg _, [Word w]) => word (WordX.neg w)
           | (Word_negCheck s, [Word w]) =>
                wordOrOverflow (s, {signed = true}, ~ (WordX.toIntInfX w))
           | (Word_notb _, [Word w]) => word (WordX.notb w)
           | (Word_orb _, [Word w1, Word w2]) => word (WordX.orb (w1, w2))
           | (Word_quot s, [Word w1, Word w2]) =>
                if WordX.isZero w2
                   then ApplyResult.Unknown
                else wordS (WordX.quot, s, w1, w2)
           | (Word_rem s, [Word w1, Word w2]) =>
                if WordX.isZero w2
                   then ApplyResult.Unknown
                else wordS (WordX.rem, s, w1, w2)
           | (Word_rol _, [Word w1, Word w2]) => word (WordX.rol (w1, w2))
           | (Word_ror _, [Word w1, Word w2]) => word (WordX.ror (w1, w2))
           | (Word_rshift s, [Word w1, Word w2]) =>
                wordS (WordX.rshift, s, w1, w2)
           | (Word_sub _, [Word w1, Word w2]) => word (WordX.sub (w1, w2))
           | (Word_subCheck s, [Word w1, Word w2]) => wcheck (op -, s, w1, w2)
           | (Word_toIntInf, [Word w]) =>
                (case IntInfRep.smallToIntInf w of
                    NONE => ApplyResult.Unknown
                  | SOME i => intInf i)
           | (Word_extdToWord (_, s, {signed}), [Word w]) =>
                word (if signed then WordX.resizeX (w, s)
                      else WordX.resize (w, s))
           | (Word_xorb _, [Word w1, Word w2]) => word (WordX.xorb (w1, w2))
           | (WordVector_toIntInf, [WordVector v]) =>
                (case IntInfRep.bigToIntInf v of
                    NONE => ApplyResult.Unknown
                  | SOME i => intInf i)
           | _ => ApplyResult.Unknown)
             handle Chr => ApplyResult.Unknown
                  | Div => ApplyResult.Unknown
                  | Exn.Overflow => ApplyResult.Overflow
                  | Subscript => ApplyResult.Unknown
      fun someVars () =
         let
            datatype z = datatype ApplyResult.t
            fun varIntInf (x, i: IntInf.t, space, inOrder) =
               let
                  fun neg () = Apply (intInfNeg, [x, space])
                  fun notb () = Apply (intInfNotb, [x, space])
                  val i = IntInf.toInt i
               in
                  case p of
                     IntInf_add => if i = 0 then Var x else Unknown
                   | IntInf_andb => if i = 0
                                       then intInfConst 0
                                    else if i = ~1
                                       then Var x
                                    else Unknown
                   | IntInf_arshift => if i = 0
                                          then intInfConst 0
                                       else if i = ~1
                                          then intInfConst ~1
                                       else Unknown
                   | IntInf_gcd => if (i = ~1 orelse i = 1)
                                      then intInfConst 1
                                   else Unknown
                   | IntInf_lshift => if i = 0
                                         then intInfConst 0
                                      else Unknown
                   | IntInf_mul =>
                        (case i of
                            0 => intInfConst 0
                          | 1 => Var x
                          | ~1 => neg ()
                          | _ => Unknown)
                   | IntInf_orb => if i = 0
                                      then Var x
                                   else if i = ~1
                                      then intInfConst ~1
                                   else Unknown
                   | IntInf_quot => if inOrder
                                       then (case i of
                                                1 => Var x
                                              | ~1 => neg ()
                                              | _ => Unknown)
                                    else Unknown
                   | IntInf_rem => if inOrder andalso (i = ~1 orelse i = 1)
                                      then intInfConst 0
                                   else Unknown
                   | IntInf_sub => if i = 0
                                      then if inOrder
                                              then Var x
                                           else neg ()
                                   else Unknown
                   | IntInf_xorb => if i = 0
                                       then Var x
                                    else if i = ~1
                                       then notb ()
                                    else Unknown
                   | _ => Unknown
               end handle Exn.Overflow => Unknown
            fun varReal (x, r, inOrder) =
               let
                  datatype z = datatype RealX.decon
                  datatype z = datatype ApplyResult.t
                  fun negIf (s, signBit) =
                      if signBit then realNeg (s, x) else Var x
                  (* The SML Basis library does not distinguish between
                     different NaN values, so optimizations that may only
                     produce a different NaN value can be considered safe.
                     For example, SNaN*1.0 = SNaN/1.0 = QNaN, so it is
                     safe to optimize x*1.0 and x/1.0 to x. *)
               in
                  case RealX.decon r of
                     NONE => Unknown
                   | SOME d =>
                     case d of
                        ZERO _ => Unknown
                      | ONE {signBit} =>
                        (case p of
                            Real_mul s => negIf (s, signBit)
                          | Real_div s => if inOrder
                                             then negIf (s, signBit)
                                          else Unknown
                          | _ => Unknown)
                      | NAN =>
                        (case p of
                            Real_Math_atan2 _ => real r
                          | Real_add _ => real r
                          | Real_div _ => real r
                          | Real_mul _ => real r
                          | Real_sub _ => real r
                          | Real_equal _ => bool false
                          | Real_qequal _ => bool true
                          | Real_le _ => bool false
                          | Real_lt _ => bool false
                          | _ => Unknown)
                      | POW2 {signBit, exp} =>
                        (case p of
                            Real_mul s =>
                            if not signBit andalso exp = 2
                               then realAdd (s, x, x)
                            else Unknown
                          | Real_div s =>
                            if inOrder andalso not signBit andalso exp = 0
                               then realAdd (s, x, x)
                            else Unknown
                          | _ => Unknown)
                      | INF _ => Unknown
                      | FIN _ => Unknown
               end
            fun varWord (x, w, inOrder) =
               let
                  val zero = word o WordX.zero
                  fun add () = if WordX.isZero w then Var x else Unknown
                  fun mul ((s, {signed}), neg) =
                     if WordX.isZero w
                        then word w
                     else if WordX.isOne w
                             then Var x
                          else if signed andalso WordX.isNegOne w
                                  then Apply (neg s, [x])
                               else Unknown
                  fun sub (s, neg) =
                     if WordX.isZero w
                        then if inOrder
                                then Var x
                             else Apply (neg s, [x])
                     else Unknown
                  fun ro s =
                     if inOrder
                        then
                           if WordX.isZero
                              (WordX.rem
                               (w,
                                WordX.fromIntInf
                                (IntInf.fromInt
                                 (Bits.toInt (WordSize.bits s)),
                                 WordX.size w),
                                {signed = false}))
                              then Var x
                           else Unknown
                     else
                        if WordX.isZero w orelse WordX.isAllOnes w
                           then word w
                        else Unknown
                  fun shift s =
                     if inOrder
                        then if WordX.isZero w
                                then Var x
                             else if (WordX.ge
                                      (w,
                                       WordX.fromIntInf (Bits.toIntInf
                                                         (WordSize.bits s),
                                                         WordSize.shiftArg),
                                       {signed = false}))
                                     then zero s
                                  else Unknown
                     else if WordX.isZero w
                             then zero s
                          else Unknown
               in
                  case p of
                     CPointer_add => 
                        if WordX.isZero w
                           then Var x
                        else Unknown
                   | CPointer_sub =>
                        if WordX.isZero w
                           andalso inOrder
                           then Var x
                        else Unknown
                   | Word_add _ => add ()
                   | Word_addCheck _ => add ()
                   | Word_andb s =>
                        if WordX.isZero w
                           then zero s
                        else if WordX.isAllOnes w
                                then Var x
                             else Unknown
                   | Word_lshift s => shift s
                   | Word_lt (_, sg) =>
                        if inOrder
                           then if WordX.isMin (w, sg) then f else Unknown
                        else if WordX.isMax (w, sg) then f else Unknown
                   | Word_mul s => mul (s, wordNeg)
                   | Word_mulCheck s => mul (s, wordNegCheck)
                   | Word_orb _ =>
                        if WordX.isZero w
                           then Var x
                        else if WordX.isAllOnes w
                                then word w
                             else Unknown
                   | Word_quot (s, {signed}) =>
                        if inOrder
                           then
                              if WordX.isOne w
                                 then Var x
                              else if signed andalso WordX.isNegOne w
                                      then Apply (wordNeg s, [x])
                                   else Unknown
                        else Unknown
                   | Word_rem (s, {signed}) =>
                        if inOrder
                           andalso (WordX.isOne w
                                    orelse signed andalso WordX.isNegOne w)
                           then zero s
                        else Unknown
                   | Word_rol s => ro s
                   | Word_ror s => ro s
                   | Word_rshift (s, {signed}) =>
                        if signed
                           then
                              if WordX.isZero w
                                 then if inOrder then Var x else zero s
                              else if WordX.isAllOnes w andalso not inOrder
                                      then word w
                                   else Unknown
                        else
                           shift s
                   | Word_sub s => sub (s, wordNeg)
                   | Word_subCheck s => sub (s, wordNegCheck o #1)
                   | Word_xorb s =>
                        if WordX.isZero w
                           then Var x
                        else if WordX.isAllOnes w
                                then Apply (wordNotb s, [x])
                             else Unknown
                   | _ => Unknown
               end
            datatype z = datatype ApplyArg.t
         in
            case (p, args) of
               (_, [Con {con = c, hasArg = h}, Con {con = c', ...}]) =>
                  if (case p of
                         MLton_eq => true
                       | MLton_equal => true
                       | _ => false)
                     then if Con.equals (c, c')
                             then if h
                                     then Unknown
                                  else t
                          else f
                  else Unknown
             | (_, [Var x, Const (Real r)]) => varReal (x, r, true)
             | (_, [Const (Real r), Var x]) => varReal (x, r, false)
             | (_, [Var x, Const (Word i)]) => varWord (x, i, true)
             | (_, [Const (Word i), Var x]) => varWord (x, i, false)
             | (_, [Const (IntInf i1), Const (IntInf i2), _]) => 
                  intInfBinary (i1, i2)
             | (_, [Const (IntInf i1), Const (Word w2), _]) => 
                  intInfShiftOrToString (i1, w2)
             | (_, [Const (IntInf i1), _]) => intInfUnary (i1)
             | (_, [Var x, Const (IntInf i), Var space]) =>
                  varIntInf (x, i, space, true)
             | (_, [Const (IntInf i), Var x, Var space]) =>
                  varIntInf (x, i, space, false)
             | (_, [Var x, Const (Word w), _]) =>
                  if WordX.isZero w
                     then
                        let
                           datatype z = datatype ApplyResult.t
                        in
                           case p of
                              IntInf_arshift => Var x
                            | IntInf_lshift => Var x
                            | _ => Unknown
                        end
                  else Unknown
             | (_, [Var x, Var y, _]) =>
                  if varEquals (x, y)
                     then let datatype z = datatype ApplyResult.t
                          in
                             case p of
                                IntInf_andb => Var x
                              | IntInf_orb => Var x
                              | IntInf_quot => intInfConst 1
                              | IntInf_rem => intInfConst 0
                              | IntInf_sub => intInfConst 0
                              | IntInf_xorb => intInfConst 0
                              | _ => Unknown
                          end
                  else Unknown
             | (_, [Var x, Var y]) =>
                  if varEquals (x, y)
                     then let
                             datatype z = datatype ApplyResult.t
                          in
                             case p of
                                CPointer_diff => word (WordX.zero (WordSize.cptrdiff ()))
                              | CPointer_equal => t
                              | CPointer_lt => f
                              | IntInf_compare =>
                                   word (WordX.zero WordSize.compareRes)
                              | MLton_eq => t
                              | MLton_equal => t
                              | Real_lt _ => f
                              | Real_qequal _ => t
                              | Word_andb _ => Var x
                              | Word_equal _ => t
                              | Word_lt _ => f
                              | Word_orb _ => Var x
                              | Word_quot (s, _) => word (WordX.one s)
                              | Word_rem (s, _) => word (WordX.zero s)
                              | Word_sub s => word (WordX.zero s)
                              | Word_subCheck (s, _) => word (WordX.zero s)
                              | Word_xorb s => word (WordX.zero s)
                              | _ => Unknown
                          end
                  else Unknown
             | _ => Unknown
         end
   in
      if List.forall (args, fn ApplyArg.Const _ => true | _ => false)
         then
            allConsts
            (List.map
             (args, fn ApplyArg.Const c => c | _ => Error.bug "Prim.apply"))
      else someVars ()
   end

fun ('a, 'b) layoutApp (p: 'a t,
                        args: 'b vector,
                        layoutArg: 'b -> Layout.t): Layout.t =
   let
      fun arg i = layoutArg (Vector.sub (args, i))
      open Layout
      fun one name = seq [str name, str " ", arg 0]
      fun two name = seq [arg 0, str " ", str name, str " ", arg 1]
   in
      case p of
         Array_length => one "length"
       | Real_Math_acos _ => one "acos"
       | Real_Math_asin _ => one "asin"
       | Real_Math_atan _ => one "atan"
       | Real_Math_cos _ => one "cos"
       | Real_Math_exp _ => one "exp"
       | Real_Math_ln _ => one "ln"
       | Real_Math_log10  _ => one "log10"
       | Real_Math_sin _ => one "sin"
       | Real_Math_sqrt _ => one "sqrt"
       | Real_Math_tan _ => one "tan"
       | Real_add _ => two "+"
       | Real_div _ => two "/"
       | Real_equal _ => two "=="
       | Real_le _ => two "<="
       | Real_lt _ => two "<"
       | Real_mul _ => two "*"
       | Real_neg _ => one "-"
       | Real_qequal _ => two "?="
       | Real_sub _ => two "-"
       | Ref_assign => two ":="
       | Ref_deref => one "!"
       | Ref_ref => one "ref"
       | Vector_length => one "length"
       | Word_add _ => two "+"
       | Word_addCheck _ => two "+"
       | Word_andb _ => two "&"
       | Word_equal _ => two "="
       | Word_lshift _ => two "<<"
       | Word_lt _ => two "<"
       | Word_mul _ => two "*"
       | Word_mulCheck _ => two "*"
       | Word_neg _ => one "-"
       | Word_negCheck _ => one "-"
       | Word_orb _ => two "|"
       | Word_rol _ => two "rol"
       | Word_ror _ => two "ror"
       | Word_rshift (_, {signed}) => two (if signed then "~>>" else ">>")
       | Word_sub _ => two "-"
       | Word_subCheck _ => two "-"
       | Word_xorb _ => two "^"
       | _ => seq [layout p, str " ", Vector.layout layoutArg args]
   end

structure Name =
   struct
      datatype t = datatype t
      val toString = toString
   end

end
