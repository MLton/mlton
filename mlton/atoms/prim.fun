(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
   structure IntX = IntX
   structure WordX = WordX
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
 | Char_toWord8 (* type inference *)
 | Exn_extra (* implement exceptions *)
 | Exn_keepHistory (* a compile-time boolean *)
 | Exn_name (* implement exceptions *)
 | Exn_setExtendExtra (* implement exceptions *)
 | Exn_setInitExtra (* implement exceptions *)
 | Exn_setTopLevelHandler (* implement exceptions *)
 | FFI of 'a CFunction.t (* ssa to rssa *)
 | FFI_Symbol of {name: string,
		  ty: 'a} (* codegen *)
 | GC_collect (* ssa to rssa *)
 | GC_pack (* ssa to rssa *)
 | GC_unpack (* ssa to rssa *)
 | Int_add of IntSize.t (* codegen *)
 | Int_addCheck of IntSize.t (* codegen *)
 | Int_arshift of IntSize.t (* codegen *)
 | Int_equal of IntSize.t (* ssa to rssa / codegen *)
 | Int_ge of IntSize.t (* codegen *)
 | Int_gt of IntSize.t (* codegen *)
 | Int_le of IntSize.t (* codegen *)
 | Int_lshift of IntSize.t (* codegen *)
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
 | Pointer_getInt of IntSize.t (* ssa to rssa *)
 | Pointer_getPointer (* ssa to rssa *)
 | Pointer_getReal of RealSize.t (* ssa to rssa *)
 | Pointer_getWord of WordSize.t (* ssa to rssa *)
 | Pointer_setInt of IntSize.t (* ssa to rssa *)
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
 | Real_toInt of RealSize.t * IntSize.t (* codegen *)
 | Real_toReal of RealSize.t * RealSize.t (* codegen *)
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
 | Vector_sub (* ssa to rssa *)
 | Weak_canGet (* ssa to rssa *)
 | Weak_get (* ssa to rssa *)
 | Weak_new (* ssa to rssa *)
 | Word_add of WordSize.t (* codegen *)
 | Word_addCheck of WordSize.t (* codegen *)
 | Word_andb of WordSize.t (* codegen *)
 | Word_arshift of WordSize.t (* codegen *)
 | Word_div of WordSize.t (* codegen *)
 | Word_equal of WordSize.t (* codegen *)
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

fun name p = p

(* The values of these strings are important since they are referred to
 * in the basis library code.  See basis-library/misc/primitive.sml.
 *)
fun toString (n: 'a t): string =
   let
      fun int (s: IntSize.t, str: string): string =
	 concat ["Int", IntSize.toString s, "_", str]
      fun real (s: RealSize.t, str: string): string =
	 concat ["Real", RealSize.toString s, "_", str]
      fun word (s: WordSize.t, str: string): string =
	 concat ["Word", WordSize.toString s, "_", str]
      val intC = ("Int", IntSize.toString)
      val realC = ("Real", RealSize.toString)
      val wordC = ("Word", WordSize.toString)
      local
	 fun make (suf, ((n, sizeToString), (n', sizeToString'),
			 s, s')): string =
	    concat [n, sizeToString s, "_to", n', sizeToString' s', suf]
      in
	 fun coerce z = make ("", z)
	 fun coerceX z = make ("X", z)
      end
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
       | Char_toWord8 => "Char_toWord8"
       | Exn_extra => "Exn_extra"
       | Exn_keepHistory => "Exn_keepHistory"
       | Exn_name => "Exn_name"
       | Exn_setExtendExtra => "Exn_setExtendExtra"
       | Exn_setInitExtra => "Exn_setInitExtra"
       | Exn_setTopLevelHandler => "Exn_setTopLevelHandler"
       | FFI f => CFunction.name f
       | FFI_Symbol {name, ...} => name
       | GC_collect => "GC_collect"
       | GC_pack => "GC_pack"
       | GC_unpack => "GC_unpack"
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
       | Int_add s => int (s, "add")
       | Int_addCheck s => int (s, "addCheck")
       | Int_arshift s => int (s, "arshift")
       | Int_equal s => int (s, "equal")
       | Int_ge s => int (s, "ge")
       | Int_gt s => int (s, "gt")
       | Int_le s => int (s, "le")
       | Int_lshift s => int (s, "lshift")
       | Int_lt s => int (s, "lt")
       | Int_mul s => int (s, "mul")
       | Int_mulCheck s => int (s, "mulCheck")
       | Int_neg s => int (s, "neg")
       | Int_negCheck s => int (s, "negCheck")
       | Int_quot s => int (s, "quot")
       | Int_rem s => int (s, "rem")
       | Int_sub s => int (s, "sub")
       | Int_subCheck s => int (s, "subCheck")
       | Int_toInt (s1, s2) => coerce (intC, intC, s1, s2)
       | Int_toReal (s1, s2) => coerce (intC, realC, s1, s2)
       | Int_toWord (s1, s2) => coerce (intC, wordC, s1, s2)
       | MLton_bogus => "MLton_bogus"
       | MLton_bug => "MLton_bug"
       | MLton_deserialize => "MLton_deserialize"
       | MLton_eq => "MLton_eq"
       | MLton_equal => "MLton_equal"
       | MLton_halt => "MLton_halt"
       | MLton_handlesSignals => "MLton_handlesSignals"
       | MLton_installSignalHandler => "MLton_installSignalHandler"
       | MLton_serialize => "MLton_serialize"
       | MLton_size => "MLton_size"
       | MLton_touch => "MLton_touch"
       | Pointer_getInt s => pointerGet ("Int", IntSize.toString s)
       | Pointer_getPointer => "Pointer_getPointer"
       | Pointer_getReal s => pointerGet ("Real", RealSize.toString s)
       | Pointer_getWord s => pointerGet ("Word", WordSize.toString s)
       | Pointer_setInt s => pointerSet ("Int", IntSize.toString s)
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
       | Real_ge s => real (s, "ge")
       | Real_gt s => real (s, "gt")
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
       | Real_toInt (s1, s2) => coerce (realC, intC, s1, s2)
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
       | Vector_length => "Vector_length"
       | Vector_sub => "Vector_sub"
       | Weak_canGet => "Weak_canGet"
       | Weak_get => "Weak_get"
       | Weak_new => "Weak_new"
       | Word8Array_subWord => "Word8Array_subWord"
       | Word8Array_updateWord => "Word8Array_updateWord"
       | Word8Vector_subWord => "Word8Vector_subWord"
       | Word8Vector_toString => "Word8Vector_toString"
       | Word8_toChar => "Word8_toChar"
       | WordVector_toIntInf => "WordVector_toIntInf"
       | Word_add s => word (s, "add")
       | Word_addCheck s => word (s, "addCheck")
       | Word_andb s => word (s, "andb")
       | Word_arshift s => word (s, "arshift")
       | Word_div s => word (s, "div")
       | Word_equal s => word (s, "equal")
       | Word_ge s => word (s, "ge")
       | Word_gt s => word (s, "gt")
       | Word_le s => word (s, "le")
       | Word_lshift s => word (s, "lshift")
       | Word_lt s => word (s, "lt")
       | Word_mod s => word (s, "mod")
       | Word_mul s => word (s, "mul")
       | Word_mulCheck s => word (s, "mulCheck")
       | Word_neg s => word (s, "neg")
       | Word_notb s => word (s, "notb")
       | Word_orb s => word (s, "orb")
       | Word_rol s => word (s, "rol")
       | Word_ror s => word (s, "ror")
       | Word_rshift s => word (s, "rshift")
       | Word_sub s => word (s, "sub")
       | Word_toInt (s1, s2) => coerce (wordC, intC, s1, s2)
       | Word_toIntInf => "Word_toIntInf"
       | Word_toIntX (s1, s2) => coerceX (wordC, intC, s1, s2)
       | Word_toWord (s1, s2) => coerce (wordC, wordC, s1, s2)
       | Word_toWordX (s1, s2) => coerceX (wordC, wordC, s1, s2)
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
    | (Char_toWord8, Char_toWord8) => true
    | (Exn_extra, Exn_extra) => true
    | (Exn_keepHistory, Exn_keepHistory) => true
    | (Exn_name, Exn_name) => true
    | (Exn_setExtendExtra, Exn_setExtendExtra) => true
    | (Exn_setInitExtra, Exn_setInitExtra) => true
    | (Exn_setTopLevelHandler, Exn_setTopLevelHandler) => true
    | (FFI f, FFI f') => CFunction.equals (f, f')
    | (FFI_Symbol {name = n, ...}, FFI_Symbol {name = n', ...}) => n = n'
    | (GC_collect, GC_collect) => true
    | (GC_pack, GC_pack) => true
    | (GC_unpack, GC_unpack) => true
    | (Int_add s, Int_add s') => IntSize.equals (s, s')
    | (Int_addCheck s, Int_addCheck s') => IntSize.equals (s, s')
    | (Int_arshift s, Int_arshift s') => IntSize.equals (s, s')
    | (Int_equal s, Int_equal s') => IntSize.equals (s, s')
    | (Int_ge s, Int_ge s') => IntSize.equals (s, s')
    | (Int_gt s, Int_gt s') => IntSize.equals (s, s')
    | (Int_le s, Int_le s') => IntSize.equals (s, s')
    | (Int_lshift s, Int_lshift s') => IntSize.equals (s, s')
    | (Int_lt s, Int_lt s') => IntSize.equals (s, s')
    | (Int_mul s, Int_mul s') => IntSize.equals (s, s')
    | (Int_mulCheck s, Int_mulCheck s') => IntSize.equals (s, s')
    | (Int_neg s, Int_neg s') => IntSize.equals (s, s')
    | (Int_negCheck s, Int_negCheck s') => IntSize.equals (s, s')
    | (Int_quot s, Int_quot s') => IntSize.equals (s, s')
    | (Int_rem s, Int_rem s') => IntSize.equals (s, s')
    | (Int_sub s, Int_sub s') => IntSize.equals (s, s')
    | (Int_subCheck s, Int_subCheck s') => IntSize.equals (s, s')
    | (Int_toInt (s1, s2), Int_toInt (s1', s2')) =>
	 IntSize.equals (s1, s1') andalso IntSize.equals (s2, s2')
    | (Int_toReal (s1, s2), Int_toReal (s1', s2')) =>
	 IntSize.equals (s1, s1') andalso RealSize.equals (s2, s2')
    | (Int_toWord (s1, s2), Int_toWord (s1', s2')) =>
	 IntSize.equals (s1, s1') andalso WordSize.equals (s2, s2')
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
    | (MLton_size, MLton_size) => true
    | (MLton_touch, MLton_touch) => true
    | (Pointer_getInt s, Pointer_getInt s') => IntSize.equals (s, s')
    | (Pointer_getPointer, Pointer_getPointer) => true
    | (Pointer_getReal s, Pointer_getReal s') => RealSize.equals (s, s')
    | (Pointer_getWord s, Pointer_getWord s') => WordSize.equals (s, s')
    | (Pointer_setInt s, Pointer_setInt s') => IntSize.equals (s, s')
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
    | (Real_ge s, Real_ge s') => RealSize.equals (s, s')
    | (Real_gt s, Real_gt s') => RealSize.equals (s, s')
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
    | (Real_toInt (s1, s2), Real_toInt (s1', s2')) =>
	 RealSize.equals (s1, s1') andalso IntSize.equals (s2, s2')
    | (Real_toReal (s1, s2), Real_toReal (s1', s2')) =>
	 RealSize.equals (s1, s1') andalso RealSize.equals (s2, s2')
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
    | (Vector_length, Vector_length) => true
    | (Vector_sub, Vector_sub) => true
    | (Weak_canGet, Weak_canGet) => true
    | (Weak_get, Weak_get) => true
    | (Weak_new, Weak_new) => true
    | (Word_add s, Word_add s') => WordSize.equals (s, s')
    | (Word_addCheck s, Word_addCheck s') => WordSize.equals (s, s')
    | (Word_andb s, Word_andb s') => WordSize.equals (s, s')
    | (Word_arshift s, Word_arshift s') => WordSize.equals (s, s')
    | (Word_div s, Word_div s') => WordSize.equals (s, s')
    | (Word_equal s, Word_equal s') => WordSize.equals (s, s')
    | (Word_ge s, Word_ge s') => WordSize.equals (s, s')
    | (Word_gt s, Word_gt s') => WordSize.equals (s, s')
    | (Word_le s, Word_le s') => WordSize.equals (s, s')
    | (Word_lshift s, Word_lshift s') => WordSize.equals (s, s')
    | (Word_lt s, Word_lt s') => WordSize.equals (s, s')
    | (Word_mod s, Word_mod s') => WordSize.equals (s, s')
    | (Word_mul s, Word_mul s') => WordSize.equals (s, s')
    | (Word_mulCheck s, Word_mulCheck s') => WordSize.equals (s, s')
    | (Word_neg s, Word_neg s') => WordSize.equals (s, s')
    | (Word_notb s, Word_notb s') => WordSize.equals (s, s')
    | (Word_orb s, Word_orb s') => WordSize.equals (s, s')
    | (Word_rol s, Word_rol s') => WordSize.equals (s, s')
    | (Word_ror s, Word_ror s') => WordSize.equals (s, s')
    | (Word_rshift s, Word_rshift s') => WordSize.equals (s, s')
    | (Word_sub s, Word_sub s') => WordSize.equals (s, s')
    | (Word_toInt (s1, s2), Word_toInt (s1', s2')) =>
	 WordSize.equals (s1, s1') andalso IntSize.equals (s2, s2')
    | (Word_toIntInf, Word_toIntInf) => true
    | (Word_toIntX (s1, s2), Word_toIntX (s1', s2')) =>
	 WordSize.equals (s1, s1') andalso IntSize.equals (s2, s2')
    | (Word_toWord (s1, s2), Word_toWord (s1', s2')) =>
	 WordSize.equals (s1, s1') andalso WordSize.equals (s2, s2')
    | (Word_toWordX (s1, s2), Word_toWordX (s1', s2')) =>
	 WordSize.equals (s1, s1') andalso WordSize.equals (s2, s2')
    | (Word_xorb s, Word_xorb s') => WordSize.equals (s, s')
    | (WordVector_toIntInf, WordVector_toIntInf) => true
    | (Word8_toChar, Word8_toChar) => true
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
    | Char_toWord8 => Char_toWord8
    | Exn_extra => Exn_extra
    | Exn_keepHistory => Exn_keepHistory
    | Exn_name => Exn_name
    | Exn_setExtendExtra => Exn_setExtendExtra
    | Exn_setInitExtra => Exn_setInitExtra
    | Exn_setTopLevelHandler => Exn_setTopLevelHandler
    | FFI func => FFI (CFunction.map (func, f))
    | FFI_Symbol {name, ty} => FFI_Symbol {name = name, ty = f ty}
    | GC_collect => GC_collect
    | GC_pack => GC_pack
    | GC_unpack => GC_unpack
    | Int_add z => Int_add z
    | Int_addCheck z => Int_addCheck z
    | Int_arshift z => Int_arshift z
    | Int_equal z => Int_equal z
    | Int_ge z => Int_ge z
    | Int_gt z => Int_gt z
    | Int_le z => Int_le z
    | Int_lshift z => Int_lshift z
    | Int_lt z => Int_lt z
    | Int_mul z => Int_mul z
    | Int_mulCheck z => Int_mulCheck z
    | Int_neg z => Int_neg z
    | Int_negCheck z => Int_negCheck z
    | Int_quot z => Int_quot z
    | Int_rem z => Int_rem z
    | Int_sub z => Int_sub z
    | Int_subCheck z => Int_subCheck z
    | Int_toInt z => Int_toInt z
    | Int_toReal z => Int_toReal z
    | Int_toWord z => Int_toWord z
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
    | MLton_size => MLton_size
    | MLton_touch => MLton_touch
    | Pointer_getInt z => Pointer_getInt z
    | Pointer_getPointer => Pointer_getPointer
    | Pointer_getReal z => Pointer_getReal z
    | Pointer_getWord z => Pointer_getWord z
    | Pointer_setInt z => Pointer_setInt z
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
    | Real_ge z => Real_ge z
    | Real_gt z => Real_gt z
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
    | Real_toInt z => Real_toInt z
    | Real_toReal z => Real_toReal z
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
    | Vector_length => Vector_length
    | Vector_sub => Vector_sub
    | Weak_canGet => Weak_canGet
    | Weak_get => Weak_get
    | Weak_new => Weak_new
    | Word_add z => Word_add z
    | Word_addCheck z => Word_addCheck z
    | Word_andb z => Word_andb z
    | Word_arshift z => Word_arshift z
    | Word_div z => Word_div z
    | Word_equal z => Word_equal z
    | Word_ge z => Word_ge z
    | Word_gt z => Word_gt z
    | Word_le z => Word_le z
    | Word_lshift z => Word_lshift z
    | Word_lt z => Word_lt z
    | Word_mod z => Word_mod z
    | Word_mul z => Word_mul z
    | Word_mulCheck z => Word_mulCheck z
    | Word_neg z => Word_neg z
    | Word_notb z => Word_notb z
    | Word_orb z => Word_orb z
    | Word_rol z => Word_rol z
    | Word_ror z => Word_ror z
    | Word_rshift z => Word_rshift z
    | Word_sub z => Word_sub z
    | Word_toInt z => Word_toInt z
    | Word_toIntInf => Word_toIntInf
    | Word_toIntX z => Word_toIntX z
    | Word_toWord z => Word_toWord z
    | Word_toWordX z => Word_toWordX z
    | Word_xorb z => Word_xorb z
    | WordVector_toIntInf => WordVector_toIntInf
    | Word8_toChar => Word8_toChar
    | Word8Array_subWord => Word8Array_subWord
    | Word8Array_updateWord => Word8Array_updateWord
    | Word8Vector_subWord => Word8Vector_subWord
    | Word8Vector_toString => Word8Vector_toString
    | World_save => World_save

val cast: 'a t -> 'b t = fn p => map (p, fn _ => Error.bug "Prim.cast")

val array = Array_array
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
val intAdd = Int_add
val intAddCheck = Int_addCheck
val intEqual = Int_equal
val intInfEqual = IntInf_equal
val intInfNeg = IntInf_neg
val intInfNotb = IntInf_notb
val intMul = Int_mul
val intMulCheck = Int_mulCheck
val intNeg = Int_neg
val intNegCheck = Int_negCheck
val intSub = Int_sub
val intSubCheck = Int_subCheck
val intToInt = Int_toInt
val intToWord = Int_toWord
val reff = Ref_ref
val serialize = MLton_serialize
val vectorLength = Vector_length
val vectorSub = Vector_sub
val wordAdd = Word_add
val wordAddCheck = Word_addCheck
val wordAndb = Word_andb
val wordArshift = Word_arshift
val wordEqual = Word_equal
val wordGe = Word_ge
val wordGt = Word_gt
val wordLe = Word_le
val wordLshift = Word_lshift
val wordLt = Word_lt
val wordMul = Word_mul
val wordMulCheck = Word_mulCheck
val wordNeg = Word_neg
val wordNotb = Word_notb
val wordOrb = Word_orb
val wordRshift = Word_rshift
val wordSub = Word_sub
val wordToInt = Word_toInt
val wordToIntX = Word_toIntX
val wordToWord = Word_toWord

val isCommutative =
   fn Int_add _ => true
    | Int_addCheck _ => true
    | Int_equal _ => true
    | Int_mul _ => true
    | Int_mulCheck _ => true
    | IntInf_equal => true
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
   fn Int_addCheck _ => true
    | Int_mulCheck _ => true
    | Int_negCheck _ => true
    | Int_subCheck _ => true
    | Word_addCheck _ => true
    | Word_mulCheck _ => true
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
       | Char_toWord8 => Functional
       | Exn_extra => Functional
       | Exn_keepHistory => Functional
       | Exn_name => Functional
       | Exn_setExtendExtra => SideEffect
       | Exn_setInitExtra => SideEffect
       | Exn_setTopLevelHandler => SideEffect
       | FFI _ => Kind.SideEffect
       | FFI_Symbol _ => Kind.DependsOnState
       | GC_collect => SideEffect
       | GC_pack => SideEffect
       | GC_unpack => SideEffect
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
       | Int_add _ => Functional
       | Int_addCheck _ => SideEffect
       | Int_arshift _ => Functional
       | Int_equal _ => Functional
       | Int_ge _ => Functional
       | Int_gt _ => Functional
       | Int_le _ => Functional
       | Int_lshift _ => Functional
       | Int_lt _ => Functional
       | Int_mul _ => Functional
       | Int_mulCheck _ => SideEffect
       | Int_neg _ => Functional
       | Int_negCheck _ => SideEffect
       | Int_quot _ => Functional
       | Int_rem _ => Functional
       | Int_sub _ => Functional
       | Int_subCheck _ => SideEffect
       | Int_toInt _ => Functional
       | Int_toReal _ => Functional
       | Int_toWord _ => Functional
       | MLton_bogus => Functional
       | MLton_bug => SideEffect
       | MLton_deserialize => Moveable
       | MLton_eq => Functional
       | MLton_equal => Functional
       | MLton_halt => SideEffect
       | MLton_handlesSignals => Functional
       | MLton_installSignalHandler => SideEffect
       | MLton_serialize => DependsOnState
       | MLton_size => DependsOnState
       | MLton_touch => SideEffect
       | Pointer_getInt _ => DependsOnState
       | Pointer_getPointer => DependsOnState
       | Pointer_getReal _ => DependsOnState
       | Pointer_getWord _ => DependsOnState
       | Pointer_setInt _ => SideEffect
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
       | Real_ge _ => Functional
       | Real_gt _ => Functional
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
       | Real_toInt _ => Functional
       | Real_toReal _ => Functional
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
       | Vector_length => Functional
       | Vector_sub => Functional
       | Weak_canGet => DependsOnState
       | Weak_get => DependsOnState
       | Weak_new => Moveable
       | Word8Array_subWord => DependsOnState
       | Word8Array_updateWord => SideEffect
       | Word8Vector_subWord => Functional
       | Word8Vector_toString => Functional
       | Word8_toChar => Functional
       | WordVector_toIntInf => Functional
       | Word_add _ => Functional
       | Word_addCheck _ => SideEffect
       | Word_andb _ => Functional
       | Word_arshift _ => Functional
       | Word_div _ => Functional
       | Word_equal _ => Functional
       | Word_ge _ => Functional
       | Word_gt _ => Functional
       | Word_le _ => Functional
       | Word_lshift _ => Functional
       | Word_lt _ => Functional
       | Word_mod _ => Functional
       | Word_mul _ => Functional
       | Word_mulCheck _ => SideEffect
       | Word_neg _ => Functional
       | Word_notb _ => Functional
       | Word_orb _ => Functional
       | Word_rol _ => Functional
       | Word_ror _ => Functional
       | Word_rshift _ => Functional
       | Word_sub _ => Functional
       | Word_toInt _ => Functional
       | Word_toIntInf => Functional
       | Word_toIntX _ => Functional
       | Word_toWord _ => Functional
       | Word_toWordX _ => Functional
       | Word_xorb _ => Functional
       | World_save => SideEffect
   end

fun isFunctional p = Kind.Functional = kind p

fun maySideEffect p = Kind.SideEffect = kind p

local
   fun ints (s: IntSize.t) =
      [(Int_add s),
       (Int_addCheck s),
       (Int_arshift s),
       (Int_equal s),
       (Int_ge s),
       (Int_gt s),
       (Int_le s),
       (Int_lshift s),
       (Int_lt s),
       (Int_mul s),
       (Int_mulCheck s),
       (Int_neg s),
       (Int_negCheck s),
       (Int_quot s),
       (Int_rem s),
       (Int_sub s),
       (Int_subCheck s)]
 
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
       (Real_ge s),
       (Real_gt s),
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

   fun words (s: WordSize.t) =
      [(Word_add s),
       (Word_addCheck s),
       (Word_andb s),
       (Word_arshift s),
       (Word_div s),
       (Word_equal s),
       (Word_ge s),
       (Word_gt s),
       (Word_le s),
       (Word_lshift s),
       (Word_lt s),
       (Word_mod s),
       (Word_mul s),
       (Word_mulCheck s),
       (Word_neg s),
       (Word_notb s),
       (Word_orb s),
       (Word_rol s),
       (Word_ror s),
       (Word_rshift s),
       (Word_sub s),
       (Word_xorb s)]
in
   val all: unit t list =
      [Array_array,
       Array_array0Const,
       Array_length,
       Array_sub,
       Array_toVector,
       Array_update,
       Char_toWord8,
       Exn_extra,
       Exn_name,
       Exn_setExtendExtra,
       Exn_setInitExtra,
       Exn_setTopLevelHandler,
       Exn_setTopLevelHandler,
       GC_collect,
       GC_pack,
       GC_unpack,
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
       Vector_length,
       Vector_sub,
       Weak_canGet,
       Weak_get,
       Weak_new,
       Word_toIntInf,
       WordVector_toIntInf,
       Word8_toChar,
       Word8Array_subWord,
       Word8Array_updateWord,
       Word8Vector_subWord,
       Word8Vector_toString,
       World_save]
      @ List.concat [List.concatMap (IntSize.prims, ints),
		     List.concatMap (RealSize.all, reals),
		     List.concatMap (WordSize.prims, words)]
      @ let
	   val int = IntSize.all
	   val real = RealSize.all
	   val word = WordSize.all
	   fun coerces (name, sizes, sizes') =
	      List.fold
	      (sizes, [], fn (s, ac) =>
	       List.fold (sizes', ac, fn (s', ac) => name (s, s') :: ac))
	in
	   List.concat [coerces (Int_toInt, int, int),
			coerces (Int_toReal, int, real),
			coerces (Int_toWord, int, word),
			coerces (Real_toInt, real, int),
			coerces (Real_toReal, real, real),
			coerces (Word_toInt, word, int),
			coerces (Word_toIntX, word, int),
			coerces (Word_toWord, word, word),
			coerces (Word_toWordX, word, word)]
	end
     @ let
	  fun doit (all, get, set) =
	     List.concatMap (all, fn s => [get s, set s])
       in
	  List.concat [doit (IntSize.prims, Pointer_getInt, Pointer_setInt),
		       doit (RealSize.all, Pointer_getReal, Pointer_setReal),
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
   val fromString: string -> 'a t =
      fn name =>
      cast
      (#prim
       (HashSet.lookupOrInsert
	(table, String.hash name,
	 fn {string, ...} => name = string,
	 fn () => Error.bug (concat ["unknown primitive: ", name]))))
end

fun 'a extractTargs {args: 'a vector,
		     deArray: 'a -> 'a,
		     deArrow: 'a -> 'a * 'a,
		     deRef: 'a -> 'a,
		     deVector: 'a -> 'a,
		     deWeak: 'a -> 'a,
		     prim: 'a t,
		     result: 'a} =
   let
      val one = Vector.new1
      fun arg i = Vector.sub (args, i)
      datatype z = datatype t
   in
      case prim of
	 Array_array => one (deArray result)
       | Array_array0Const => one (deArray result)
       | Array_sub => one result
       | Array_toVector => one (deArray (arg 0))
       | Array_update => one (arg 2)
       | Array_length => one (deArray (arg 0))
       | Exn_extra => one result
       | Exn_setExtendExtra => one (#2 (deArrow (arg 0)))
       | Exn_setInitExtra => one (arg 0)
       | MLton_bogus => one result
       | MLton_deserialize => one result
       | MLton_eq => one (arg 0)
       | MLton_equal => one (arg 0)
       | MLton_serialize => one (arg 0)
       | MLton_size => one (deRef (arg 0))
       | MLton_touch => one (arg 0)
       | Pointer_getPointer => one result
       | Pointer_setPointer => one (arg 2)
       | Ref_assign => one (arg 1)
       | Ref_deref => one result
       | Ref_ref => one (arg 0)
       | Vector_length => one (deVector (arg 0))
       | Vector_sub => one result
       | Weak_canGet => one (deWeak (arg 0))
       | Weak_get => one result
       | Weak_new => one (arg 0)
       | _ => Vector.new0 ()
   end

val extractTargs =
   fn z =>
   Trace.trace ("extractTargs", layout o #prim, Layout.ignore) extractTargs z

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
      val int = ApplyResult.Const o Const.int
      val intInf = ApplyResult.Const o Const.intInf
      val intInfConst = intInf o IntInf.fromInt
      fun word (w: WordX.t): ('a, 'b) ApplyResult.t =
	 ApplyResult.Const (Const.word w)
      val word8Vector = ApplyResult.Const o Const.word8Vector
      val t = ApplyResult.truee
      val f = ApplyResult.falsee
      fun iio (f, c1, c2) = intInf (f (c1, c2))
      fun io (f: IntX.t * IntX.t -> IntX.t, i, i') =
	 int (f (i, i'))
      fun wcheck (f: IntInf.t * IntInf.t -> IntInf.t,
		  w: WordX.t,
		  w': WordX.t,
		  s: WordSize.t) =
	 let
	    val x = f (WordX.toIntInf w, WordX.toIntInf w')
	 in
	    if x <= WordX.toIntInf (WordX.max s)
	       then word (WordX.fromIntInf (x, s))
	    else ApplyResult.Overflow
	 end
      val eq =
 	 fn (Int i1, Int i2) => bool (IntX.equals (i1, i2))
 	  | (Word w1, Word w2) => bool (WordX.equals (w1, w2))
 	  | _ => ApplyResult.Unknown
      val equal =
	 fn (Int i1, Int i2) => bool (IntX.equals (i1, i2))
	  | (Word w1, Word w2) => bool (WordX.equals (w1, w2))
	  | (Word8Vector v1, Word8Vector v2) => bool (v1 = v2)
	  | _ => ApplyResult.Unknown
      fun allConsts (cs: Const.t list) =
	 (case (p, cs) of
	     (Int_add _, [Int i1, Int i2]) => io (IntX.+, i1, i2)
	   | (Int_addCheck _, [Int i1, Int i2]) => io (IntX.+, i1, i2)
	   | (Int_arshift _, [Int i, Word w]) =>
		int (IntX.~>> (i, WordX.toIntInf w))
           | (Int_equal _, [Int i1, Int i2]) => bool (IntX.equals (i1, i2))
	   | (Int_ge _, [Int i1, Int i2]) => bool (IntX.>= (i1, i2))
	   | (Int_gt _, [Int i1, Int i2]) => bool (IntX.> (i1, i2))
	   | (Int_le _, [Int i1, Int i2]) => bool (IntX.<= (i1, i2))
	   | (Int_lshift _, [Int i, Word w]) =>
		int (IntX.<< (i, WordX.toIntInf w))
	   | (Int_lt _, [Int i1, Int i2]) => bool (IntX.< (i1, i2))
	   | (Int_mul _, [Int i1, Int i2]) => io (IntX.*, i1, i2)
	   | (Int_mulCheck _, [Int i1, Int i2]) => io (IntX.*, i1, i2)
	   | (Int_neg _, [Int i]) => int (IntX.~ i)
	   | (Int_negCheck _, [Int i]) => int (IntX.~ i)
	   | (Int_quot _, [Int i1, Int i2]) => io (IntX.quot, i1, i2)
	   | (Int_rem _, [Int i1, Int i2]) => io (IntX.rem, i1, i2)
	   | (Int_sub _, [Int i1, Int i2]) => io (IntX.-, i1, i2)
	   | (Int_subCheck _, [Int i1, Int i2]) => io (IntX.-, i1, i2)
	   | (Int_toInt (_, s), [Int i]) =>
	        int (IntX.make (IntX.toIntInf i, s))
	   | (Int_toWord (_, s), [Int i]) =>
		word (WordX.fromIntInf (IntX.toIntInf i, s))
	   | (IntInf_compare, [IntInf i1, IntInf i2]) =>
		int (IntX.make (IntInf.fromInt (case IntInf.compare (i1, i2) of
						   Relation.LESS => ~1
						 | Relation.EQUAL => 0
						 | Relation.GREATER => 1),
				IntSize.default))
	   | (IntInf_equal, [IntInf i1, IntInf i2]) => bool (i1 = i2)
	   | (IntInf_toWord, [IntInf i]) =>
		(case SmallIntInf.toWord i of
		    NONE => ApplyResult.Unknown
		  | SOME w => word (WordX.fromIntInf (Word.toIntInf w,
						      WordSize.default)))
	   | (MLton_eq, [c1, c2]) => eq (c1, c2)
	   | (MLton_equal, [c1, c2]) => equal (c1, c2)
	   | (Word_add _, [Word w1, Word w2]) => word (WordX.+ (w1, w2))
	   | (Word_addCheck s, [Word w1, Word w2]) =>
		wcheck (IntInf.+, w1, w2, s)
	   | (Word_andb _, [Word w1, Word w2]) => word (WordX.andb (w1, w2))
	   | (Word_arshift _, [Word w1, Word w2]) => word (WordX.~>> (w1, w2))
	   | (Word_div _, [Word w1, Word w2]) => word (WordX.div (w1, w2))
           | (Word_equal _, [Word w1, Word w2]) => bool (WordX.equals (w1, w2))
	   | (Word_ge _, [Word w1, Word w2]) => bool (WordX.>= (w1, w2))
	   | (Word_gt _, [Word w1, Word w2]) => bool (WordX.> (w1, w2))
	   | (Word_le _, [Word w1, Word w2]) => bool (WordX.<= (w1, w2))
	   | (Word_lshift _, [Word w1, Word w2]) => word (WordX.<< (w1, w2))
	   | (Word_lt _, [Word w1, Word w2]) => bool (WordX.< (w1, w2))
	   | (Word_mod _, [Word w1, Word w2]) => word (WordX.mod (w1, w2))
	   | (Word_mul _, [Word w1, Word w2]) => word (WordX.* (w1, w2))
	   | (Word_mulCheck s, [Word w1, Word w2]) =>
		wcheck (IntInf.*, w1, w2, s)
	   | (Word_notb _, [Word w]) => word (WordX.notb w)
	   | (Word_orb _, [Word w1, Word w2]) => word (WordX.orb (w1, w2))
	   | (Word_rol _, [Word w1, Word w2]) => word (WordX.rol (w1, w2))
	   | (Word_ror _, [Word w1, Word w2]) => word (WordX.ror (w1, w2))
	   | (Word_rshift _, [Word w1, Word w2]) => word (WordX.>> (w1, w2))
	   | (Word_sub _, [Word w1, Word w2]) => word (WordX.- (w1, w2))
	   | (Word_toInt (_, s), [Word w]) =>
		int (IntX.make (WordX.toIntInf w, s))
	   | (Word_toIntInf, [Word w]) =>
		intInf (SmallIntInf.fromWord
			(Word.fromIntInf (WordX.toIntInf w)))
	   | (Word_toIntX (_, s), [Word w]) =>
		int (IntX.make (WordX.toIntInfX w, s))
	   | (Word_toWord (_, s), [Word w]) => word (WordX.resize (w, s))
	   | (Word_toWordX (_, s), [Word w]) => word (WordX.resizeX (w, s))
	   | (Word_xorb _, [Word w1, Word w2]) => word (WordX.xorb (w1, w2))
	   | _ => ApplyResult.Unknown)
	     handle Chr => ApplyResult.Unknown
		  | Div => ApplyResult.Unknown
		  | Exn.Overflow => ApplyResult.Overflow
		  | Subscript => ApplyResult.Unknown
      fun someVars () =
	 let
	    datatype z = datatype ApplyResult.t
	    fun add (x: 'b, i: IntX.t): ('a, 'b) ApplyResult.t =
	       if IntX.isZero i then Var x else Unknown
	    fun mul (x: 'b, i: IntX.t, s: IntSize.t, neg) =
	       (case IntX.toInt i of
		   0 => int (IntX.zero s)
		 | 1 => Var x
		 | ~1 => Apply (neg s, [x])
		 | _ => Unknown) handle Exn.Overflow => Unknown
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
		  fun mul () =
		     if WordX.isZero w
			then word w
		     else if WordX.isOne w
			     then Var x
			  else Unknown
		  fun ro () =
		     if inOrder
			then
			   let
			      val s = WordX.size w
			   in
			      if WordX.isZero
				 (WordX.mod
				  (w,
				   WordX.fromIntInf
				   (IntInf.fromInt
				    (Bits.toInt (WordSize.bits s)),
				    s)))
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
			     else if (WordX.>=
				      (w,
				       WordX.fromIntInf (Bits.toIntInf
							 (WordSize.bits s),
							 WordSize.default)))
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
		   | Word_arshift s =>
			if WordX.isZero w
			   then if inOrder then Var x else zero s
			else if WordX.isAllOnes w
				then if inOrder then Unknown else word w
			     else Unknown
		   | Word_div _ =>
			if inOrder andalso WordX.isOne w then Var x else Unknown
		   | Word_ge _ =>
			if inOrder
			   then if WordX.isZero w then t else Unknown
			else if WordX.isMax w then t else Unknown
		   | Word_gt _ =>
			if inOrder
			   then if WordX.isMax w then f else Unknown
			else if WordX.isZero w then f else Unknown
		   | Word_le _ =>
			if inOrder
			   then if WordX.isMax w then t else Unknown
			else if WordX.isZero w then t else Unknown
		   | Word_lshift s => shift s
		   | Word_lt _ =>
			if inOrder
			   then if WordX.isZero w then f else Unknown
			else if WordX.isMax w then f else Unknown
		   | Word_mod s =>
			if inOrder andalso WordX.isOne w
			   then zero s
			else Unknown
		   | Word_mul _ => mul ()
		   | Word_mulCheck _ => mul ()
		   | Word_orb _ =>
			if WordX.isZero w
			   then Var x
			else if WordX.isAllOnes w
				then word w
			     else Unknown
		   | Word_rol _ => ro ()
		   | Word_ror _ => ro ()
		   | Word_rshift s => shift s
		   | Word_sub s =>
			if WordX.isZero w
			   then
			      if inOrder
				 then Var x
			      else Apply (wordNeg s, [x])
			else Unknown
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
	       (IntInf_toString, [Const (IntInf i), Const (Int base), _]) =>
		  let
		     val base =
			case IntX.toInt base of
			   2 => StringCvt.BIN
			 | 8 => StringCvt.OCT 
			 | 10 => StringCvt.DEC
			 | 16 => StringCvt.HEX
			 | _ => Error.bug "strange base for IntInf_toString"
		  in
		     word8Vector (Word8.stringToVector (IntInf.format (i, base)))
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
	     | (_, [Var x, Const (Int i)]) =>
		  (case p of
		      Int_add _ => add (x, i)
		    | Int_addCheck _ => add (x, i)
		    | Int_ge _ => if IntX.isMin i then t else Unknown
		    | Int_gt _ => if IntX.isMax i then f else Unknown
		    | Int_le _ => if IntX.isMax i then t else Unknown
		    | Int_lt _ => if IntX.isMin i then f else Unknown
		    | Int_mul s => mul (x, i, s, intNeg)
		    | Int_mulCheck s => mul (x, i, s, intNegCheck)
		    | Int_quot s =>
			 if IntX.isNegOne i
			    then Apply (intNeg s, [x])
			 else if IntX.isOne i
				 then ApplyResult.Var x
			      else Unknown
		    | Int_rem s =>
			 if IntX.isNegOne i orelse IntX.isOne i
			    then int (IntX.zero s)
			 else Unknown
		    | Int_sub _ =>
			 if IntX.isZero i
			    then ApplyResult.Var x
			 else Unknown
		    | Int_subCheck _ =>
			 if IntX.isZero i
			    then ApplyResult.Var x
			 else Unknown
		    | _ => Unknown)
	     | (_, [Const (Int i), Var x]) =>
		  (case p of 
		      Int_add _ => add (x, i)
		    | Int_addCheck _ => add (x, i)
		    | Int_ge _ => if IntX.isMax i then t else Unknown
		    | Int_gt _ => if IntX.isMin i then f else Unknown
		    | Int_le _ => if IntX.isMin i then t else Unknown
		    | Int_lt _ => if IntX.isMax i then f else Unknown
		    | Int_mul s => mul (x, i, s, intNeg)
		    | Int_mulCheck s => mul (x, i, s, intNegCheck)
		    | Int_sub s =>
			 if IntX.isZero i
			    then Apply (intNeg s, [x])
			 else Unknown
		    | Int_subCheck s =>
			 if IntX.isZero i
			    then Apply (intNegCheck s, [x])
			 else Unknown
		    | _ => Unknown)
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
                                Int_equal _ => t
			      | Int_ge _ => t
			      | Int_gt _ => f
			      | Int_le _ => t
			      | Int_lt _ => f
			      | Int_quot s => int (IntX.one s)
			      | Int_rem s => int (IntX.zero s)
			      | Int_sub s => int (IntX.zero s)
			      | IntInf_compare =>
				   int (IntX.zero IntSize.default)
			      | IntInf_equal => t
			      | MLton_eq => t
			      | MLton_equal => t
			      | Real_lt _ => f
			      | Real_le _ => t
			      | Real_equal _ => t
			      | Real_gt _ => f
			      | Real_ge _ => t
			      | Real_qequal _ => t
			      | Word_andb _ => Var x
			      | Word_div s => word (WordX.one s)
                              | Word_equal _ => t
			      | Word_ge _ => t
			      | Word_gt _ => f
			      | Word_le _ => t
			      | Word_lt _ => f
			      | Word_mod s => word (WordX.zero s)
			      | Word_orb _ => Var x
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
      datatype z = datatype t
   in
      case p of
	 Int_mul _ => two "*?"
       | Int_mulCheck _ => two "*"
       | Int_add _ => two "+?"
       | Int_addCheck _ => two "+"
       | Int_sub _ => two "-?"
       | Int_subCheck _ => two "-"
       | Int_equal _ => two "="
       | Int_lt _ => two "<"
       | Int_le _ => two "<="
       | Int_gt _ => two ">"
       | Int_ge _ => two ">="
       | Int_neg _ => one "-?"
       | Int_negCheck _ => one "-"
       | IntInf_equal => two "="
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
       | Real_ge _ => two ">="
       | Real_gt _ => two ">"
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
       | Word_addCheck _ => two "+c"
       | Word_andb _ => two "&"
       | Word_arshift _ => two "~>>"
       | Word_equal _ => two "="
       | Word_ge _ => two ">="
       | Word_gt _ => two ">"
       | Word_le _ => two "<="
       | Word_lshift _ => two "<<"
       | Word_lt _ => two "<"
       | Word_mul _ => two "*"
       | Word_mulCheck _ => two "*c"
       | Word_neg _ => one "-"
       | Word_orb _ => two "|"
       | Word_rol _ => two "rol"
       | Word_ror _ => two "ror"
       | Word_rshift _ => two ">>"
       | Word_sub _ => two "-"
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
