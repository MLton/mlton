
(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(*
 * If you add new polymorphic primitives, you must modify extractTargs.
 *)

functor Prim (S: PRIM_STRUCTS): PRIM = 
struct

open S

type word = Word.t
   
local
   open Const
in
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
   Array_array (* backend *)
 | Array_array0Const (* constant propagation *)
 | Array_length (* ssa to rssa *)
 | Array_sub (* backend *)
 | Array_toVector (* backend *)
 | Array_update (* backend *)
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
 | MLton_share
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
 | Real_neg of RealSize.t         (* codegen *)
 | Real_qequal of RealSize.t (* codegen *)
 | Real_round of RealSize.t (* codegen *)
 | Real_sub of RealSize.t (* codegen *)
 | Real_toReal of RealSize.t * RealSize.t (* codegen *)
 | Real_toWord of RealSize.t * WordSize.t * {signed: bool} (* codegen *)
 | Ref_assign (* backend *)
 | Ref_deref (* backend *)
 | Ref_ref (* backend *)
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
 | Vector_length (* ssa to rssa *)
 | Vector_sub (* ssa to rssa *)
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
      fun wordS (s: WordSize.t, sg, str: string): string =
         concat [sign sg, WordSize.toString s, "_", str]
      val realC = ("Real", RealSize.toString)
      val wordC = ("Word", WordSize.toString)
      fun wordCS sg = (sign sg, WordSize.toString)
      fun coerce ((n, sizeToString), (n', sizeToString'), s, s'): string =
         concat [n, sizeToString s, "_to", n', sizeToString' s']
      fun pointerGet (ty, s) = concat ["Pointer_get", ty, s]
      fun pointerSet (ty, s) = concat ["Pointer_set", ty, s]
   in
      case n of
         Array_array => "Array_array"
       | Array_array0Const => "Array_array0Const"
       | Array_length => "Array_length"
       | Array_sub => "Array_sub"
       | Array_toVector => "Array_toVector"
       | Array_update => "Array_update"
       | Exn_extra => "Exn_extra"
       | Exn_name => "Exn_name"
       | Exn_setExtendExtra => "Exn_setExtendExtra"
       | Exn_setInitExtra => "Exn_setInitExtra"
       | FFI f => (CFunction.Target.toString o CFunction.target) f
       | FFI_Symbol {name, ...} => name
       | GC_collect => "GC_collect"
       | IntInf_add => "IntInf_add"
       | IntInf_andb => "IntInf_andb"
       | IntInf_arshift => "IntInf_arshift"
       | IntInf_compare => "IntInf_compare"
       | IntInf_equal => "IntInf_equal"
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
       | MLton_handlesSignals => "MLton_handlesSignals"
       | MLton_installSignalHandler => "MLton_installSignalHandler"
       | MLton_serialize => "MLton_serialize"
       | MLton_share => "MLton_share"
       | MLton_size => "MLton_size"
       | MLton_touch => "MLton_touch"
       | Pointer_getPointer => "Pointer_getPointer"
       | Pointer_getReal s => pointerGet ("Real", RealSize.toString s)
       | Pointer_getWord s => pointerGet ("Word", WordSize.toString s)
       | Pointer_setPointer => "Pointer_setPointer"
       | Pointer_setReal s => pointerSet ("Real", RealSize.toString s)
       | Pointer_setWord s => pointerSet ("Word", WordSize.toString s)
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
       | Real_round s => real (s, "round")
       | Real_sub s => real (s, "sub")
       | Real_toWord (s1, s2, sg) => coerce (realC, wordCS sg, s1, s2)
       | Real_toReal (s1, s2) => coerce (realC, realC, s1, s2)
       | Ref_assign => "Ref_assign"
       | Ref_deref => "Ref_deref"
       | Ref_ref => "Ref_ref"
       | String_toWord8Vector => "String_toWord8Vector"
       | Thread_atomicBegin => "Thread_atomicBegin"
       | Thread_atomicEnd => "Thread_atomicEnd"
       | Thread_canHandle => "Thread_canHandle"
       | Thread_copy => "Thread_copy"
       | Thread_copyCurrent => "Thread_copyCurrent"
       | Thread_returnToC => "Thread_returnToC"
       | Thread_switchTo => "Thread_switchTo"
       | TopLevel_setHandler => "TopLevel_setHandler"
       | TopLevel_setSuffix => "TopLevel_setSuffix"
       | Vector_length => "Vector_length"
       | Vector_sub => "Vector_sub"
       | Weak_canGet => "Weak_canGet"
       | Weak_get => "Weak_get"
       | Weak_new => "Weak_new"
       | Word8Array_subWord => "Word8Array_subWord"
       | Word8Array_updateWord => "Word8Array_updateWord"
       | Word8Vector_subWord => "Word8Vector_subWord"
       | Word8Vector_toString => "Word8Vector_toString"
       | WordVector_toIntInf => "WordVector_toIntInf"
       | Word_add s => word (s, "add")
       | Word_addCheck (s, sg) => wordS (s, sg, "addCheck")
       | Word_andb s => word (s, "andb")
       | Word_equal s => word (s, "equal")
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
       | Word_rol s => word (s, "rol")
       | Word_ror s => word (s, "ror")
       | Word_rshift (s, sg) => wordS (s, sg, "rshift")
       | Word_sub s => word (s, "sub")
       | Word_subCheck (s, sg) => wordS (s, sg, "subCheck")
       | Word_toIntInf => "Word_toIntInf"
       | Word_toReal (s1, s2, sg) => coerce (wordCS sg, realC, s1, s2)
       | Word_toWord (s1, s2, sg) => coerce (wordCS sg, wordC, s1, s2)
       | Word_xorb s => word (s, "xorb")
       | World_save => "World_save"
   end

fun layout p = Layout.str (toString p)
   
val equals: 'a t * 'a t -> bool =
   fn (Array_array, Array_array) => true
    | (Array_array0Const, Array_array0Const) => true
    | (Array_length, Array_length) => true
    | (Array_sub, Array_sub) => true
    | (Array_toVector, Array_toVector) => true
    | (Array_update, Array_update) => true
    | (Exn_extra, Exn_extra) => true
    | (Exn_name, Exn_name) => true
    | (Exn_setExtendExtra, Exn_setExtendExtra) => true
    | (Exn_setInitExtra, Exn_setInitExtra) => true
    | (FFI f, FFI f') => CFunction.equals (f, f')
    | (FFI_Symbol {name = n, ...}, FFI_Symbol {name = n', ...}) => n = n'
    | (GC_collect, GC_collect) => true
    | (IntInf_add, IntInf_add) => true
    | (IntInf_andb, IntInf_andb) => true
    | (IntInf_arshift, IntInf_arshift) => true
    | (IntInf_compare, IntInf_compare) => true
    | (IntInf_equal, IntInf_equal) => true
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
    | (MLton_handlesSignals, MLton_handlesSignals) => true
    | (MLton_installSignalHandler, MLton_installSignalHandler) => true
    | (MLton_serialize, MLton_serialize) => true
    | (MLton_share, MLton_share) => true
    | (MLton_size, MLton_size) => true
    | (MLton_touch, MLton_touch) => true
    | (Pointer_getPointer, Pointer_getPointer) => true
    | (Pointer_getReal s, Pointer_getReal s') => RealSize.equals (s, s')
    | (Pointer_getWord s, Pointer_getWord s') => WordSize.equals (s, s')
    | (Pointer_setPointer, Pointer_setPointer) => true
    | (Pointer_setReal s, Pointer_setReal s') => RealSize.equals (s, s')
    | (Pointer_setWord s, Pointer_setWord s') => WordSize.equals (s, s')
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
    | (Real_round s, Real_round s') => RealSize.equals (s, s')
    | (Real_sub s, Real_sub s') => RealSize.equals (s, s')
    | (Real_toReal (s1, s2), Real_toReal (s1', s2')) =>
         RealSize.equals (s1, s1') andalso RealSize.equals (s2, s2')
    | (Real_toWord (s1, s2, sg), Real_toWord (s1', s2', sg')) =>
         RealSize.equals (s1, s1')
         andalso WordSize.equals (s2, s2')
         andalso sg = sg'
    | (Ref_assign, Ref_assign) => true
    | (Ref_deref, Ref_deref) => true
    | (Ref_ref, Ref_ref) => true
    | (String_toWord8Vector, String_toWord8Vector) => true
    | (Thread_atomicBegin, Thread_atomicBegin) => true
    | (Thread_atomicEnd, Thread_atomicEnd) => true
    | (Thread_canHandle, Thread_canHandle) => true
    | (Thread_copy, Thread_copy) => true
    | (Thread_copyCurrent, Thread_copyCurrent) => true
    | (Thread_returnToC, Thread_returnToC) => true
    | (Thread_switchTo, Thread_switchTo) => true
    | (TopLevel_setHandler, TopLevel_setHandler) => true
    | (TopLevel_setSuffix, TopLevel_setSuffix) => true
    | (Vector_length, Vector_length) => true
    | (Vector_sub, Vector_sub) => true
    | (Weak_canGet, Weak_canGet) => true
    | (Weak_get, Weak_get) => true
    | (Weak_new, Weak_new) => true
    | (Word_add s, Word_add s') => WordSize.equals (s, s')
    | (Word_addCheck (s, sg), Word_addCheck (s', sg')) =>
         WordSize.equals (s, s') andalso sg = sg'
    | (Word_andb s, Word_andb s') => WordSize.equals (s, s')
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
    | (Word_rol s, Word_rol s') => WordSize.equals (s, s')
    | (Word_ror s, Word_ror s') => WordSize.equals (s, s')
    | (Word_rshift (s, sg), Word_rshift (s', sg')) =>
         WordSize.equals (s, s') andalso sg = sg'
    | (Word_sub s, Word_sub s') => WordSize.equals (s, s')
    | (Word_subCheck (s, sg), Word_subCheck (s', sg')) =>
         WordSize.equals (s, s') andalso sg = sg'
    | (Word_toIntInf, Word_toIntInf) => true
    | (Word_toReal (s1, s2, sg), Word_toReal (s1', s2', sg')) =>
         WordSize.equals (s1, s1')
         andalso RealSize.equals (s2, s2')
         andalso sg = sg'
    | (Word_toWord (s1, s2, sg), Word_toWord (s1', s2', sg')) =>
         WordSize.equals (s1, s1')
         andalso WordSize.equals (s2, s2')
         andalso sg = sg'
    | (Word_xorb s, Word_xorb s') => WordSize.equals (s, s')
    | (WordVector_toIntInf, WordVector_toIntInf) => true
    | (Word8Array_subWord, Word8Array_subWord) => true
    | (Word8Array_updateWord, Word8Array_updateWord) => true
    | (Word8Vector_subWord, Word8Vector_subWord) => true
    | (Word8Vector_toString, Word8Vector_toString) => true
    | (World_save, World_save) => true
    | _ => false

val map: 'a t * ('a -> 'b) -> 'b t =
   fn (p, f) =>
   case p of
      Array_array => Array_array
    | Array_array0Const => Array_array0Const
    | Array_length => Array_length
    | Array_sub => Array_sub
    | Array_toVector => Array_toVector
    | Array_update => Array_update
    | Exn_extra => Exn_extra
    | Exn_name => Exn_name
    | Exn_setExtendExtra => Exn_setExtendExtra
    | Exn_setInitExtra => Exn_setInitExtra
    | FFI func => FFI (CFunction.map (func, f))
    | FFI_Symbol {name} => FFI_Symbol {name = name}
    | GC_collect => GC_collect
    | IntInf_add => IntInf_add
    | IntInf_andb => IntInf_andb
    | IntInf_arshift => IntInf_arshift
    | IntInf_compare => IntInf_compare
    | IntInf_equal => IntInf_equal
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
    | MLton_handlesSignals => MLton_handlesSignals
    | MLton_installSignalHandler => MLton_installSignalHandler
    | MLton_serialize => MLton_serialize
    | MLton_share => MLton_share
    | MLton_size => MLton_size
    | MLton_touch => MLton_touch
    | Pointer_getPointer => Pointer_getPointer
    | Pointer_getReal z => Pointer_getReal z
    | Pointer_getWord z => Pointer_getWord z
    | Pointer_setPointer => Pointer_setPointer
    | Pointer_setReal z => Pointer_setReal z
    | Pointer_setWord z => Pointer_setWord z
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
    | Real_round z => Real_round z
    | Real_sub z => Real_sub z
    | Real_toReal z => Real_toReal z
    | Real_toWord z => Real_toWord z
    | Ref_assign => Ref_assign
    | Ref_deref => Ref_deref
    | Ref_ref => Ref_ref
    | String_toWord8Vector => String_toWord8Vector
    | Thread_atomicBegin => Thread_atomicBegin
    | Thread_atomicEnd => Thread_atomicEnd
    | Thread_canHandle => Thread_canHandle
    | Thread_copy => Thread_copy
    | Thread_copyCurrent => Thread_copyCurrent
    | Thread_returnToC => Thread_returnToC
    | Thread_switchTo => Thread_switchTo
    | TopLevel_setHandler => TopLevel_setHandler
    | TopLevel_setSuffix => TopLevel_setSuffix
    | Vector_length => Vector_length
    | Vector_sub => Vector_sub
    | Weak_canGet => Weak_canGet
    | Weak_get => Weak_get
    | Weak_new => Weak_new
    | Word_add z => Word_add z
    | Word_addCheck z => Word_addCheck z
    | Word_andb z => Word_andb z
    | Word_equal z => Word_equal z
    | Word_lshift z => Word_lshift z
    | Word_lt z => Word_lt z
    | Word_mul z => Word_mul z
    | Word_mulCheck z => Word_mulCheck z
    | Word_neg z => Word_neg z
    | Word_negCheck z => Word_negCheck z
    | Word_notb z => Word_notb z
    | Word_orb z => Word_orb z
    | Word_rol z => Word_rol z
    | Word_quot z => Word_quot z
    | Word_rem z => Word_rem z
    | Word_ror z => Word_ror z
    | Word_rshift z => Word_rshift z
    | Word_sub z => Word_sub z
    | Word_subCheck z => Word_subCheck z
    | Word_toIntInf => Word_toIntInf
    | Word_toReal z => Word_toReal z
    | Word_toWord z => Word_toWord z
    | Word_xorb z => Word_xorb z
    | WordVector_toIntInf => WordVector_toIntInf
    | Word8Array_subWord => Word8Array_subWord
    | Word8Array_updateWord => Word8Array_updateWord
    | Word8Vector_subWord => Word8Vector_subWord
    | Word8Vector_toString => Word8Vector_toString
    | World_save => World_save

val cast: 'a t -> 'b t = fn p => map (p, fn _ => Error.bug "Prim.cast")

val array = Array_array
val arrayLength = Array_length
val assign = Ref_assign
val bogus = MLton_bogus
val bug = MLton_bug
val deref = Ref_deref
val deserialize = MLton_deserialize
val eq = MLton_eq
val equal = MLton_equal
val ffi = FFI
val ffiSymbol = FFI_Symbol
val gcCollect = GC_collect
val intInfEqual = IntInf_equal
val intInfNeg = IntInf_neg
val intInfNotb = IntInf_notb
fun pointerGet ctype =
   let datatype z = datatype CType.t
   in
      case ctype of
         Int8 => Pointer_getWord (WordSize.fromBits (Bits.fromInt 8))
       | Int16 => Pointer_getWord (WordSize.fromBits (Bits.fromInt 16))
       | Int32 => Pointer_getWord (WordSize.fromBits (Bits.fromInt 32))
       | Int64 => Pointer_getWord (WordSize.fromBits (Bits.fromInt 64))
       | Pointer => Pointer_getPointer
       | Real32 => Pointer_getReal RealSize.R32
       | Real64 => Pointer_getReal RealSize.R64
       | Word8 => Pointer_getWord (WordSize.fromBits (Bits.fromInt 8))
       | Word16 => Pointer_getWord (WordSize.fromBits (Bits.fromInt 16))
       | Word32 => Pointer_getWord (WordSize.fromBits (Bits.fromInt 32))
       | Word64 => Pointer_getWord (WordSize.fromBits (Bits.fromInt 64))
   end
fun pointerSet ctype =
   let datatype z = datatype CType.t
   in
      case ctype of
         Int8 => Pointer_setWord (WordSize.fromBits (Bits.fromInt 8))
       | Int16 => Pointer_setWord (WordSize.fromBits (Bits.fromInt 16))
       | Int32 => Pointer_setWord (WordSize.fromBits (Bits.fromInt 32))
       | Int64 => Pointer_setWord (WordSize.fromBits (Bits.fromInt 64))
       | Pointer => Pointer_setPointer
       | Real32 => Pointer_setReal RealSize.R32
       | Real64 => Pointer_setReal RealSize.R64
       | Word8 => Pointer_setWord (WordSize.fromBits (Bits.fromInt 8))
       | Word16 => Pointer_setWord (WordSize.fromBits (Bits.fromInt 16))
       | Word32 => Pointer_setWord (WordSize.fromBits (Bits.fromInt 32))
       | Word64 => Pointer_setWord (WordSize.fromBits (Bits.fromInt 64))
   end

val reff = Ref_ref
val serialize = MLton_serialize
val touch = MLton_touch
val vectorLength = Vector_length
val vectorSub = Vector_sub
val wordAdd = Word_add
val wordAddCheck = Word_addCheck
val wordAndb = Word_andb
val wordEqual = Word_equal
val wordLshift = Word_lshift
val wordLt = Word_lt
val wordMul = Word_mul
val wordMulCheck = Word_mulCheck
val wordNeg = Word_neg
val wordNegCheck = Word_negCheck
val wordNotb = Word_notb
val wordOrb = Word_orb
val wordRshift = Word_rshift
val wordSub = Word_sub
val wordSubCheck = Word_subCheck
val wordToWord = Word_toWord

val isCommutative =
   fn IntInf_equal => true
    | MLton_eq => true
    | MLton_equal => true
    | Real_add _ => true
    | Real_mul _ => true
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

val mayRaise = mayOverflow

val kind: 'a t -> Kind.t =
   fn p =>
   let
      datatype z = datatype Kind.t
   in
      case p of
         Array_array => Moveable
       | Array_array0Const => Moveable
       | Array_length => Functional
       | Array_sub => DependsOnState
       | Array_toVector => DependsOnState
       | Array_update => SideEffect
       | Exn_extra => Functional
       | Exn_name => Functional
       | Exn_setExtendExtra => SideEffect
       | Exn_setInitExtra => SideEffect
       | FFI _ => Kind.SideEffect
       | FFI_Symbol _ => Functional
       | GC_collect => SideEffect
       | IntInf_add => Functional
       | IntInf_andb => Functional
       | IntInf_arshift => Functional
       | IntInf_compare => Functional
       | IntInf_equal => Functional
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
       | MLton_handlesSignals => Functional
       | MLton_installSignalHandler => SideEffect
       | MLton_serialize => DependsOnState
       | MLton_share => SideEffect
       | MLton_size => DependsOnState
       | MLton_touch => SideEffect
       | Pointer_getPointer => DependsOnState
       | Pointer_getReal _ => DependsOnState
       | Pointer_getWord _ => DependsOnState
       | Pointer_setPointer => SideEffect
       | Pointer_setReal _ => SideEffect
       | Pointer_setWord _ => SideEffect
       | Real_Math_acos _ => Functional
       | Real_Math_asin _ => Functional
       | Real_Math_atan _ => Functional
       | Real_Math_atan2 _ => Functional
       | Real_Math_cos _ => Functional
       | Real_Math_exp _ => Functional
       | Real_Math_ln _ => Functional
       | Real_Math_log10 _ => Functional
       | Real_Math_sin _ => Functional
       | Real_Math_sqrt _ => Functional
       | Real_Math_tan _ => Functional
       | Real_abs _ => Functional
       | Real_add _ => Functional
       | Real_div _ => Functional
       | Real_equal _ => Functional
       | Real_ldexp _ => Functional
       | Real_le _ => Functional
       | Real_lt _ => Functional
       | Real_mul _ => Functional
       | Real_muladd _ => Functional
       | Real_mulsub _ => Functional
       | Real_neg _ => Functional
       | Real_qequal _ => Functional
       | Real_round _ => DependsOnState  (* depends on rounding mode *)
       | Real_sub _ => Functional
       | Real_toReal _ => Functional
       | Real_toWord _ => Functional
       | Ref_assign => SideEffect
       | Ref_deref => DependsOnState
       | Ref_ref => Moveable
       | String_toWord8Vector => Functional
       | Thread_atomicBegin => SideEffect
       | Thread_atomicEnd => SideEffect
       | Thread_canHandle => DependsOnState
       | Thread_copy => Moveable
       | Thread_copyCurrent => SideEffect
       | Thread_returnToC => SideEffect
       | Thread_switchTo => SideEffect
       | TopLevel_setHandler => SideEffect
       | TopLevel_setSuffix => SideEffect
       | Vector_length => Functional
       | Vector_sub => Functional
       | Weak_canGet => DependsOnState
       | Weak_get => DependsOnState
       | Weak_new => Moveable
       | Word8Array_subWord => DependsOnState
       | Word8Array_updateWord => SideEffect
       | Word8Vector_subWord => Functional
       | Word8Vector_toString => Functional
       | WordVector_toIntInf => Functional
       | Word_add _ => Functional
       | Word_addCheck _ => SideEffect
       | Word_andb _ => Functional
       | Word_equal _ => Functional
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
       | Word_rol _ => Functional
       | Word_ror _ => Functional
       | Word_rshift _ => Functional
       | Word_sub _ => Functional
       | Word_subCheck _ => SideEffect
       | Word_toIntInf => Functional
       | Word_toReal _ => Functional
       | Word_toWord _ => Functional
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
      [Array_array,
       Array_array0Const,
       Array_length,
       Array_sub,
       Array_toVector,
       Array_update,
       Exn_extra,
       Exn_name,
       Exn_setExtendExtra,
       Exn_setInitExtra,
       GC_collect,
       IntInf_add,
       IntInf_andb,
       IntInf_arshift,
       IntInf_compare,
       IntInf_equal,
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
       MLton_handlesSignals,
       MLton_installSignalHandler,
       MLton_serialize,
       MLton_share,
       MLton_size,
       MLton_touch,
       Pointer_getPointer,
       Pointer_setPointer,
       Ref_assign,
       Ref_deref,
       Ref_ref,
       String_toWord8Vector,
       Thread_atomicBegin,
       Thread_atomicEnd,
       Thread_canHandle,
       Thread_copy,
       Thread_copyCurrent,
       Thread_returnToC,
       Thread_switchTo,
       TopLevel_setHandler,
       TopLevel_setSuffix,
       Vector_length,
       Vector_sub,
       Weak_canGet,
       Weak_get,
       Weak_new,
       Word_toIntInf,
       WordVector_toIntInf,
       Word8Array_subWord,
       Word8Array_updateWord,
       Word8Vector_subWord,
       Word8Vector_toString,
       World_save]
      @ List.concat [List.concatMap (RealSize.all, reals),
                     List.concatMap (WordSize.prims, words)]
      @ let
           val real = RealSize.all
           val word = WordSize.all
           fun coerces (name, sizes, sizes', ac) =
              List.fold
              ([false, true], ac, fn (signed, ac) =>
               List.fold
               (sizes, ac, fn (s, ac) =>
                List.fold (sizes', ac, fn (s', ac) =>
                           name (s, s', {signed = signed}) :: ac)))
        in
           coerces (Real_toWord, real, word,
                    coerces (Word_toReal, word, real,
                             coerces (Word_toWord, word, word,
                                      List.fold
                                      (real, [], fn (s, ac) =>
                                       List.fold
                                       (real, ac, fn (s', ac) =>
                                        Real_toReal (s, s') :: ac)))))
        end
     @ let
          fun doit (all, get, set) =
             List.concatMap (all, fn s => [get s, set s])
       in
          List.concat [doit (RealSize.all, Pointer_getReal, Pointer_setReal),
                       doit (WordSize.prims, Pointer_getWord, Pointer_setWord)]
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

fun ('a, 'b) extractTargs (prim: 'b t,
                           {args: 'a vector,
                            deArray: 'a -> 'a,
                            deArrow: 'a -> 'a * 'a,
                            deVector: 'a -> 'a,
                            deWeak: 'a -> 'a,
                            result: 'a}) =
   let
      val one = Vector.new1
      fun arg i = Vector.sub (args, i)
      datatype z = datatype t
   in
      case prim of
         Array_array => one (deArray result)
       | Array_array0Const => one (deArray result)
       | Array_sub => one (deArray (arg 0))
       | Array_toVector => one (deArray (arg 0))
       | Array_update => one (deArray (arg 0))
       | Array_length => one (deArray (arg 0))
       | Exn_extra => one result
       | Exn_setExtendExtra => one (#2 (deArrow (arg 0)))
       | Exn_setInitExtra => one (arg 0)
       | MLton_bogus => one result
       | MLton_deserialize => one result
       | MLton_eq => one (arg 0)
       | MLton_equal => one (arg 0)
       | MLton_serialize => one (arg 0)
       | MLton_share => one (arg 0)
       | MLton_size => one (arg 0)
       | MLton_touch => one (arg 0)
       | Pointer_getPointer => one result
       | Pointer_setPointer => one (arg 2)
       | Ref_assign => one (arg 1)
       | Ref_deref => one result
       | Ref_ref => one (arg 0)
       | Vector_length => one (deVector (arg 0))
       | Vector_sub => one (deVector (arg 0))
       | Weak_canGet => one (deWeak (arg 0))
       | Weak_get => one result
       | Weak_new => one (arg 0)
       | _ => Vector.new0 ()
   end

val extractTargs =
   fn z =>
   Trace.trace ("Prim.extractTargs", layout o #1, Layout.ignore) extractTargs z

structure SmallIntInf = Const.SmallIntInf

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

      datatype ('a, 'b) t =
         Apply of 'a prim * 'b list
       | Bool of bool
       | Const of Const.t
       | Overflow
       | Unknown
       | Var of 'b

      val truee = Bool true
      val falsee = Bool false

      val layoutPrim = layout

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
      val intInf = ApplyResult.Const o Const.intInf
      val intInfConst = intInf o IntInf.fromInt
      fun word (w: WordX.t): ('a, 'b) ApplyResult.t =
         ApplyResult.Const (Const.word w)
      val f = ApplyResult.falsee
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
         fn (Word w1, Word w2) => bool (WordX.equals (w1, w2))
          | (WordVector v1, WordVector v2) => bool (WordXVector.equals (v1, v2))
          | _ => ApplyResult.Unknown
      fun allConsts (cs: Const.t list) =
         (case (p, cs) of
             (IntInf_compare, [IntInf i1, IntInf i2]) =>
                let
                   val i =
                      case IntInf.compare (i1, i2) of
                         Relation.LESS => ~1
                       | Relation.EQUAL => 0
                       | Relation.GREATER => 1
                in
                   word (WordX.fromIntInf (i, WordSize.default))
                end
           | (IntInf_equal, [IntInf i1, IntInf i2]) => bool (i1 = i2)
           | (IntInf_toWord, [IntInf i]) =>
                (case SmallIntInf.toWord i of
                    NONE => ApplyResult.Unknown
                  | SOME w => word (WordX.fromIntInf (Word.toIntInf w,
                                                      WordSize.default)))
           | (MLton_eq, [c1, c2]) => eq (c1, c2)
           | (MLton_equal, [c1, c2]) => equal (c1, c2)
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
                intInf (SmallIntInf.fromWord
                        (Word.fromIntInf (WordX.toIntInf w)))
           | (Word_toWord (_, s, {signed}), [Word w]) =>
                word (if signed then WordX.resizeX (w, s)
                      else WordX.resize (w, s))
           | (Word_xorb _, [Word w1, Word w2]) => word (WordX.xorb (w1, w2))
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
                   | IntInf_gcd => if (i = ~1 orelse i = 1)
                                      then intInfConst 1
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
                  fun ro () =
                     if inOrder
                        then
                           let
                              val s = WordX.size w
                           in
                              if WordX.isZero
                                 (WordX.rem
                                  (w,
                                   WordX.fromIntInf
                                   (IntInf.fromInt
                                    (Bits.toInt (WordSize.bits s)),
                                    s),
                                   {signed = false}))
                                 then Var x
                              else Unknown
                           end
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
                                                         WordSize.default),
                                       {signed = false}))
                                     then zero s
                                  else Unknown
                     else if WordX.isZero w
                             then zero s
                          else Unknown
               in
                  case p of
                     Word_add _ => add ()
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
                   | Word_rol _ => ro ()
                   | Word_ror _ => ro ()
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
               (IntInf_toString, [Const (IntInf i), Const (Word base), _]) =>
                  let
                     val base =
                        case WordX.toInt base of
                           2 => StringCvt.BIN
                         | 8 => StringCvt.OCT 
                         | 10 => StringCvt.DEC
                         | 16 => StringCvt.HEX
                         | _ => Error.bug "Prim.apply: strange base for IntInf_toString"
                  in
                     ApplyResult.Const (Const.string (IntInf.format (i, base)))
                  end
             | (_, [Con {con = c, hasArg = h}, Con {con = c', ...}]) =>
                  if (case p of
                         MLton_eq => true
                       | MLton_equal => true
                       | _ => false)
                     then if Con.equals (c, c')
                             then if h
                                     then Unknown
                                  else bool true
                          else bool false
                  else Unknown
             | (_, [Var x, Const (Word i)]) => varWord (x, i, true)
             | (_, [Const (Word i), Var x]) => varWord (x, i, false)
             | (_, [Const (IntInf i1), Const (IntInf i2), _]) =>
                  (case p of
                      IntInf_add => iio (IntInf.+, i1, i2)
                    | IntInf_andb => iio (IntInf.andb, i1, i2)
                    | IntInf_gcd => iio (IntInf.gcd, i1, i2)
                    | IntInf_mul => iio (IntInf.*, i1, i2)
                    | IntInf_orb => iio (IntInf.orb, i1, i2)
                    | IntInf_quot => iio (IntInf.quot, i1, i2)
                    | IntInf_rem => iio (IntInf.rem, i1, i2)
                    | IntInf_sub => iio (IntInf.-, i1, i2)
                    | IntInf_xorb => iio (IntInf.xorb, i1, i2)
                    | _ => Unknown)
             | (_, [Const (IntInf i1), Const (Word w2), _]) =>
                  (case p of
                      IntInf_arshift =>
                         intInf (IntInf.~>>
                                 (i1, Word.fromIntInf (WordX.toIntInf w2)))
                    | IntInf_lshift =>
                         intInf (IntInf.<<
                                 (i1, Word.fromIntInf (WordX.toIntInf w2)))
                    | _ => Unknown)
             | (_, [Const (IntInf i1), _]) =>
                  (case p of
                      IntInf_neg => intInf (IntInf.~ i1)
                    | IntInf_notb => intInf (IntInf.notb i1)
                    | _ => Unknown)
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
                             val t = ApplyResult.truee
                             val f = ApplyResult.falsee
                             datatype z = datatype ApplyResult.t
                          in
                             case p of
                                IntInf_compare =>
                                   word (WordX.zero WordSize.default)
                              | IntInf_equal => t
                              | MLton_eq => t
                              | MLton_equal => t
                              | Real_lt _ => f
                              | Real_le _ => t
                              | Real_qequal _ => t
                              | Word_andb _ => Var x
                              | Word_equal _ => t
                              | Word_lt _ => f
                              | Word_orb _ => Var x
                              | Word_quot (s, _) => word (WordX.one s)
                              | Word_rem (s, _) => word (WordX.zero s)
                              | Word_sub s => word (WordX.zero s)
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
         IntInf_equal => two "="
       | MLton_eq => two "="
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
      val layout = layout
      val toString = toString
   end

end
