(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
(*
 * If you add new primitives, you may need to modify backend/machine.fun.
 * If you add new polymorphic primitives, you should also modify the
 * extractTargs function.
 *)
functor Prim (S: PRIM_STRUCTS): PRIM = 
struct

open S

datatype z = datatype IntSize.t
datatype z = datatype RealSize.t
datatype z = datatype WordSize.t	       

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

structure Name =
   struct
      datatype t =
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
       | FFI of CFunction.t (* ssa to rssa *)
       | FFI_Symbol of {name: string,
			ty: CType.t} (* codegen *)
       | GC_collect (* ssa to rssa *)
       | GC_pack (* ssa to rssa *)
       | GC_unpack (* ssa to rssa *)
       | Int_add of IntSize.t (* codegen *)
       | Int_addCheck of IntSize.t (* codegen *)
       | Int_equal of IntSize.t (* ssa to rssa / codegen *)
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
       | Pointer_getInt of IntSize.t (* backend *)
       | Pointer_getPointer (* backend *)
       | Pointer_getReal of RealSize.t (* backend *)
       | Pointer_getWord of WordSize.t (* backend *)
       | Pointer_setInt of IntSize.t (* backend *)
       | Pointer_setPointer (* backend *)
       | Pointer_setReal of RealSize.t (* backend *)
       | Pointer_setWord of WordSize.t (* backend *)
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
       | Vector_sub (* backend *)
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

      val equals: t * t -> bool = op =

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

      datatype z = datatype Kind.t
      (* The values of these strings are important since they are referred to
       * in the basis library code.  See basis-library/misc/primitive.sml.
       *)
      fun ints (s: IntSize.t) =
	 List.map
	 ([(Int_add, Functional, "add"),
	   (Int_addCheck, SideEffect, "addCheck"),
	   (Int_equal, Functional, "equal"),
	   (Int_ge, Functional, "ge"),
	   (Int_gt, Functional, "gt"),
	   (Int_le, Functional, "le"),
	   (Int_lt, Functional, "lt"),
	   (Int_mul, Functional, "mul"),
	   (Int_mulCheck, SideEffect, "mulCheck"),
	   (Int_neg, Functional, "neg"),
	   (Int_negCheck, SideEffect, "negCheck"),
	   (Int_quot, Functional, "quot"),
	   (Int_rem, Functional, "rem"),
	   (Int_sub, Functional, "sub"),
	   (Int_subCheck, SideEffect, "subCheck")],
	  fn (makeName, kind, str) =>
	  (makeName s, kind, concat ["Int", IntSize.toString s, "_", str]))
 
      fun reals (s: RealSize.t) =
	 List.map
	 ([(Real_Math_acos, Functional, "Math_acos"),
	   (Real_Math_asin, Functional, "Math_asin"),
	   (Real_Math_atan, Functional, "Math_atan"),
	   (Real_Math_atan2, Functional, "Math_atan2"),
	   (Real_Math_cos, Functional, "Math_cos"),
	   (Real_Math_exp, Functional, "Math_exp"),
	   (Real_Math_ln, Functional, "Math_ln"),
	   (Real_Math_log10, Functional, "Math_log10"),
	   (Real_Math_sin, Functional, "Math_sin"),
	   (Real_Math_sqrt, Functional, "Math_sqrt"),
	   (Real_Math_tan, Functional, "Math_tan"),
	   (Real_abs, Functional, "abs"),
	   (Real_add, Functional, "add"),
	   (Real_div, Functional, "div"),
	   (Real_equal, Functional, "equal"),
	   (Real_ge, Functional, "ge"),
	   (Real_gt, Functional, "gt"),
	   (Real_ldexp, Functional, "ldexp"),
	   (Real_le, Functional, "le"),
	   (Real_lt, Functional, "lt"),
	   (Real_mul, Functional, "mul"),
	   (Real_muladd, Functional, "muladd"),
	   (Real_mulsub, Functional, "mulsub"),
	   (Real_neg, Functional, "neg"),
	   (Real_qequal, Functional, "qequal"),
	   (Real_round, Functional, "round"),
	   (Real_sub, Functional, "sub")],
	 fn (makeName, kind, str) =>
	 (makeName s, kind, concat ["Real", RealSize.toString s, "_", str]))

      fun words (s: WordSize.t) =
	 List.map
	 ([(Word_add, Functional, "add"),
	   (Word_addCheck, SideEffect, "addCheck"),
	   (Word_andb, Functional, "andb"),
	   (Word_arshift, Functional, "arshift"),
	   (Word_div, Functional, "div"),
	   (Word_equal, Functional, "equal"),
	   (Word_ge, Functional, "ge"),
	   (Word_gt, Functional, "gt"),
	   (Word_le, Functional, "le"),
	   (Word_lshift, Functional, "lshift"),
	   (Word_lt, Functional, "lt"),
	   (Word_mod, Functional, "mod"),
	   (Word_mul, Functional, "mul"),
	   (Word_mulCheck, SideEffect, "mulCheck"),
	   (Word_neg, Functional, "neg"),
	   (Word_notb, Functional, "notb"),
	   (Word_orb, Functional, "orb"),
	   (Word_rol, Functional, "rol"),
	   (Word_ror, Functional, "ror"),
	   (Word_rshift, Functional, "rshift"),
	   (Word_sub, Functional, "sub"),
	   (Word_xorb, Functional, "xorb")],
	  fn (makeName, kind, str) =>
	  (makeName s, kind, concat ["Word", WordSize.toString s, "_", str]))

      val strings =
	 [
	  (Array_array, Moveable, "Array_array"),
	  (Array_array0Const, Moveable, "Array_array0Const"),
	  (Array_length, Functional, "Array_length"),
	  (Array_sub, DependsOnState, "Array_sub"),
	  (Array_toVector, DependsOnState, "Array_toVector"),
	  (Array_update, SideEffect, "Array_update"),
	  (Char_toWord8, Functional, "Char_toWord8"),
	  (Exn_extra, Functional, "Exn_extra"),
	  (Exn_name, Functional, "Exn_name"),
	  (Exn_setExtendExtra, SideEffect, "Exn_setExtendExtra"),
	  (Exn_setInitExtra, SideEffect, "Exn_setInitExtra"),
	  (Exn_setTopLevelHandler, SideEffect, "Exn_setTopLevelHandler"),
	  (Exn_setTopLevelHandler, SideEffect, "Exn_setTopLevelHandler"),
	  (GC_collect, SideEffect, "GC_collect"),
	  (GC_pack, SideEffect, "GC_pack"),
	  (GC_unpack, SideEffect, "GC_unpack"),
	  (IntInf_add, Functional, "IntInf_add"),
	  (IntInf_andb, Functional, "IntInf_andb"),
	  (IntInf_arshift, Functional, "IntInf_arshift"),
	  (IntInf_compare, Functional, "IntInf_compare"),
	  (IntInf_equal, Functional, "IntInf_equal"),
	  (IntInf_gcd, Functional, "IntInf_gcd"),
	  (IntInf_lshift, Functional, "IntInf_lshift"),
	  (IntInf_mul, Functional, "IntInf_mul"),
	  (IntInf_notb, Functional, "IntInf_notb"),
	  (IntInf_neg, Functional, "IntInf_neg"),
	  (IntInf_orb, Functional, "IntInf_orb"),
	  (IntInf_quot, Functional, "IntInf_quot"),
	  (IntInf_rem, Functional, "IntInf_rem"),
	  (IntInf_sub, Functional, "IntInf_sub"),
	  (IntInf_toString, Functional, "IntInf_toString"),
	  (IntInf_toVector, Functional, "IntInf_toVector"),
	  (IntInf_toWord, Functional, "IntInf_toWord"),
	  (IntInf_xorb, Functional, "IntInf_xorb"),
	  (MLton_bogus, Functional, "MLton_bogus"),
	  (MLton_bug, SideEffect, "MLton_bug"),
	  (MLton_deserialize, Moveable, "MLton_deserialize"),
	  (MLton_eq, Functional, "MLton_eq"),
	  (MLton_equal, Functional, "MLton_equal"),
	  (MLton_halt, SideEffect, "MLton_halt"),
	  (MLton_handlesSignals, Functional, "MLton_handlesSignals"),
	  (MLton_installSignalHandler, SideEffect,
	   "MLton_installSignalHandler"),
	  (MLton_serialize, DependsOnState, "MLton_serialize"),
	  (MLton_size, DependsOnState, "MLton_size"),
	  (MLton_touch, SideEffect, "MLton_touch"),
	  (Ref_assign, SideEffect, "Ref_assign"),
	  (Ref_deref, DependsOnState, "Ref_deref"),
	  (Ref_ref, Moveable, "Ref_ref"),
	  (String_toWord8Vector, Functional, "String_toWord8Vector"),
	  (Thread_atomicBegin, SideEffect, "Thread_atomicBegin"),
	  (Thread_atomicEnd, SideEffect, "Thread_atomicEnd"),
	  (Thread_canHandle, DependsOnState, "Thread_canHandle"),
	  (Thread_copy, Moveable, "Thread_copy"),
	  (Thread_copyCurrent, SideEffect, "Thread_copyCurrent"),
	  (Thread_returnToC, SideEffect, "Thread_returnToC"),
	  (Thread_switchTo, SideEffect, "Thread_switchTo"),
	  (Vector_length, Functional, "Vector_length"),
	  (Vector_sub, Functional, "Vector_sub"),
	  (Weak_canGet, DependsOnState, "Weak_canGet"),
	  (Weak_get, DependsOnState, "Weak_get"),
	  (Weak_new, Moveable, "Weak_new"),
	  (Word_toIntInf, Functional, "Word_toIntInf"),
	  (WordVector_toIntInf, Functional, "WordVector_toIntInf"),
	  (Word8_toChar, Functional, "Word8_toChar"),
	  (Word8Array_subWord, DependsOnState, "Word8Array_subWord"),
	  (Word8Array_updateWord, SideEffect, "Word8Array_updateWord"),
	  (Word8Vector_subWord, Functional, "Word8Vector_subWord"),
	  (Word8Vector_toString, Functional, "Word8Vector_toString"),
	  (World_save, SideEffect, "World_save")]
	 @ List.concat [List.concatMap (IntSize.all, ints),
			List.concatMap (RealSize.all, reals),
			List.concatMap (WordSize.all, words)]
	 @ let
	      val int = ("Int", IntSize.all, IntSize.toString)
	      val real = ("Real", RealSize.all, RealSize.toString)
	      val word = ("Word", WordSize.all, WordSize.toString)
	      local
		 fun coerces' suf (name,
				   (n, sizes, sizeToString),
				   (n', sizes', sizeToString')) =
		    List.fold
		    (sizes, [], fn (s, ac) =>
		     List.fold
		     (sizes', ac, fn (s', ac) =>
		      (name (s, s'), Functional,
		       concat [n, sizeToString s, "_to", n', sizeToString' s',
			       suf])
		      :: ac))
	      in
		 val coerces = fn z => coerces' "" z
		 val coercesX = fn z => coerces' "X" z
	      end
	   in
	      List.concat [coerces (Int_toInt, int, int),
			   coerces (Int_toReal, int, real),
			   coerces (Int_toWord, int, word),
			   coerces (Real_toInt, real, int),
			   coerces (Real_toReal, real, real),
			   coerces (Word_toInt, word, int),
			   coercesX (Word_toIntX, word, int),
			   coerces (Word_toWord, word, word),
			   coercesX (Word_toWordX, word, word)]
	   end
	@ let
	     fun doit (name, all, toString, get, set) =
		List.concatMap
		(all, fn s =>
		 [(get s, DependsOnState,
		   concat ["Pointer_get", name, toString s]),
		  (set s, SideEffect,
		   concat ["Pointer_set", name, toString s])])
	  in
	     List.concat [doit ("Int", IntSize.all, IntSize.toString,
				Pointer_getInt, Pointer_setInt),
			  doit ("Pointer", [()], fn () => "",
				fn () => Pointer_getPointer,
				fn () => Pointer_setPointer),
			  doit ("Real", RealSize.all, RealSize.toString,
				Pointer_getReal, Pointer_setReal),
			  doit ("Word", WordSize.all, WordSize.toString,
				Pointer_getWord, Pointer_setWord)]
	  end
	 
      fun toString n =
	 case n of
	    FFI f => CFunction.name f
	  | FFI_Symbol {name, ...} => name
	  | _ => (case List.peek (strings, fn (n', _, _) => n = n') of
		     NONE => Error.bug "Prim.toString missing name"
		   | SOME (_, _, s) => s)

      val layout = Layout.str o toString
   end

datatype t =
   T of {name: Name.t,
	 nameString: string,
	 kind: Kind.t}

local
   fun make sel (T r) = sel r
in
   val kind = make #kind
   val name = make #name
   val toString = make #nameString
end

val layout = Name.layout o name

local
   fun make k p = k = kind p
in
   val isFunctional = make Kind.Functional
   val maySideEffect = make Kind.SideEffect
end
val isFunctional = Trace.trace ("isFunctional", layout, Bool.layout) isFunctional

val isCommutative = Name.isCommutative o name
val mayOverflow = Name.mayOverflow o name
val mayRaise = Name.mayRaise o name

fun make (n: Name.t, k: Kind.t): t =
   T {kind = k,
      name = n,
      nameString = Name.toString n}

fun equals (p, p') = Name.equals (name p, name p')

val new: Name.t -> t =
   fn n =>
   let
      val k =
	 case n of
	    Name.FFI _ => Kind.SideEffect
	  | Name.FFI_Symbol _ => Kind.DependsOnState
	  | _ => (case List.peek (Name.strings, fn (n', _, _) => n = n') of
		     NONE => Error.bug (concat ["strange name: ",
						Name.toString n])
		   | SOME (_, k, _) => k)
   in
      make (n, k)
   end

val array = new Name.Array_array
val assign = new Name.Ref_assign
val bogus = new Name.MLton_bogus
val bug = new Name.MLton_bug
val deref = new Name.Ref_deref
val deserialize = new Name.MLton_deserialize
val eq = new Name.MLton_eq
val equal = new Name.MLton_equal
val gcCollect = new Name.GC_collect
val intInfEqual = new Name.IntInf_equal
val intInfNeg = new Name.IntInf_neg
val intInfNotb = new Name.IntInf_notb
val reff = new Name.Ref_ref
val serialize = new Name.MLton_serialize
val vectorLength = new Name.Vector_length
val vectorSub = new Name.Vector_sub

local
   fun make n = IntSize.memoize (new o n)
in
   val intAdd = make Name.Int_add
   val intAddCheck = make Name.Int_addCheck
   val intEqual = make Name.Int_equal
   val intNeg = make Name.Int_neg
   val intNegCheck = make Name.Int_negCheck
   val intMul = make Name.Int_mul
   val intMulCheck = make Name.Int_mulCheck
   val intSub = make Name.Int_sub
   val intSubCheck = make Name.Int_subCheck
end

local
   fun make n = WordSize.memoize (new o n)
in
   val wordAdd = make Name.Word_add
   val wordAddCheck = make Name.Word_addCheck
   val wordAndb = make Name.Word_andb
   val wordEqual = make Name.Word_equal
   val wordGe = make Name.Word_ge
   val wordGt = make Name.Word_gt
   val wordLe = make Name.Word_le
   val wordLt = make Name.Word_lt
   val wordMul = make Name.Word_mul
   val wordMulCheck = make Name.Word_mulCheck
   val wordNeg = make Name.Word_neg
   val wordNotb = make Name.Word_notb
   val wordRshift = make Name.Word_rshift
   val wordSub = make Name.Word_sub
end

local
   fun make (name, memo, memo') =
      let
	 val f = memo (fn s => memo' (fn s' => name (s, s')))
      in
	 fn (s, s') => new (f s s')
      end
   val int = IntSize.memoize
   val word = WordSize.memoize
in
   val intToWord = make (Name.Int_toWord, int, word)
   val wordToInt = make (Name.Word_toInt, word, int)
   val wordToIntX = make (Name.Word_toIntX, word, int)
end
      
val ffi = new o Name.FFI
   
fun newNullary f = new (Name.FFI f)
   
val allocTooLarge = newNullary CFunction.allocTooLarge
   
fun ffiSymbol z = new (Name.FFI_Symbol z)

val new: string -> t =
   fn name =>
   let
      val (name, kind) =
	 case List.peek (Name.strings, fn (_, _, s) => s = name) of
	    NONE => Error.bug (concat ["unknown primitive: ", name])
	  | SOME (n, k, _) => (n, k)
   in
      make (name, kind)
   end

val new = Trace.trace ("Prim.new", String.layout, layout) new

fun 'a extractTargs {args: 'a vector,
		     deArray: 'a -> 'a,
		     deArrow: 'a -> 'a * 'a,
		     deRef: 'a -> 'a,
		     deVector: 'a -> 'a,
		     deWeak: 'a -> 'a,
		     prim: t,
		     result: 'a} =
   let
      val one = Vector.new1
      fun arg i = Vector.sub (args, i)
      datatype z = datatype Name.t
   in
      case name prim of
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
      type prim = t
      datatype 'a t =
	 Apply of prim * 'a list
       | Bool of bool
       | Const of Const.t
       | Overflow
       | Unknown
       | Var of 'a

      val truee = Bool true
      val falsee = Bool false

      val layoutPrim = layout
      fun layout layoutX =
	 let open Layout
	 in fn Apply (p, args) => seq [layoutPrim p, List.layout layoutX args]
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
   
fun 'a apply (p, args, varEquals) =
   let
      datatype z = datatype Name.t
      datatype z = datatype Const.t
      val bool = ApplyResult.Bool
      val int = ApplyResult.Const o Const.int
      val intInf = ApplyResult.Const o Const.intInf
      val intInfConst = intInf o IntInf.fromInt
      fun word (w: WordX.t): 'a ApplyResult.t =
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
	    val x' = x mod (Int.toIntInf (WordSize.size s))
	 in
	    if x = x'
	       then word (WordX.fromLargeInt (x, s))
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
	 (case (name p, cs) of
	     (Int_add _, [Int i1, Int i2]) => io (IntX.+, i1, i2)
	   | (Int_addCheck _, [Int i1, Int i2]) => io (IntX.+, i1, i2)
           | (Int_equal _, [Int i1, Int i2]) => bool (IntX.equals (i1, i2))
	   | (Int_ge _, [Int i1, Int i2]) => bool (IntX.>= (i1, i2))
	   | (Int_gt _, [Int i1, Int i2]) => bool (IntX.> (i1, i2))
	   | (Int_le _, [Int i1, Int i2]) => bool (IntX.<= (i1, i2))
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
		word (WordX.fromLargeInt (IntX.toIntInf i, s))
	   | (IntInf_compare, [IntInf i1, IntInf i2]) =>
		int (IntX.make
		     (IntInf.fromInt (case IntInf.compare (i1, i2) of
					 Relation.LESS => ~1
				       | Relation.EQUAL => 0
				       | Relation.GREATER => 1),
		      IntSize.default))
	   | (IntInf_equal, [IntInf i1, IntInf i2]) => bool (i1 = i2)
	   | (IntInf_toWord, [IntInf i]) =>
		(case SmallIntInf.toWord i of
		    NONE => ApplyResult.Unknown
		  | SOME w => word (WordX.make (LargeWord.fromWord w,
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
			(LargeWord.toWord (WordX.toLargeWord w)))
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
	    fun add (x: 'a, i: IntX.t): 'a ApplyResult.t =
	       if IntX.isZero i then Var x else Unknown
	    fun mul (x: 'a, i: IntX.t, s: IntSize.t, neg) =
	       (case IntX.toInt i of
		   0 => int (IntX.zero s)
		 | 1 => Var x
		 | ~1 => Apply (neg s, [x])
		 | _ => Unknown) handle Exn.Overflow => Unknown
	    val name = name p
	    fun varIntInf (x, i: IntInf.t, space, inOrder) =
	       let
		  fun neg () = Apply (intInfNeg, [x, space])
		  fun notb () = Apply (intInfNotb, [x, space])
		  val i = IntInf.toInt i
	       in
		  case name of
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
				   WordX.make
				   (LargeWord.fromInt (WordSize.size s), s)))
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
				      (w, WordX.make (LargeWord.fromInt
						      (WordSize.size s),
						      WordSize.default)))
				     then zero s
				  else Unknown
		     else if WordX.isZero w
			     then zero s
			  else Unknown
	       in
		  case name of
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
	    case (name, args) of
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
		  if name = MLton_equal orelse name = MLton_eq
		     then if Con.equals (c, c')
			     then if h
				     then Unknown
				  else bool true
			  else bool false
		  else Unknown
	     | (_, [Var x, Const (Word i)]) => varWord (x, i, true)
	     | (_, [Const (Word i), Var x]) => varWord (x, i, false)
	     | (_, [Var x, Const (Int i)]) =>
		  (case name of
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
		  (case name of 
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
		  (case name of
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
		  (case name of
		      IntInf_arshift =>
			 intInf (IntInf.~>>
				 (i1, LargeWord.toWord (WordX.toLargeWord w2)))
		    | IntInf_lshift =>
			 intInf (IntInf.<<
				 (i1, LargeWord.toWord (WordX.toLargeWord w2)))
		    | _ => Unknown)
	     | (_, [Const (IntInf i1), _]) =>
		  (case name of
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
			   case name of
			      IntInf_arshift => Var x
			    | IntInf_lshift => Var x
			    | _ => Unknown
			end
		  else Unknown
             | (_, [Var x, Var y, _]) =>
		  if varEquals (x, y)
		     then let datatype z = datatype ApplyResult.t
			  in
			     case name of
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
			     case name of
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

fun layoutApp (p: t, args: 'a vector, layoutArg: 'a -> Layout.t): Layout.t =
   let
      fun arg i = layoutArg (Vector.sub (args, i))
      open Layout
      fun one name = seq [str name, str " ", arg 0]
      fun two name = seq [arg 0, str " ", str name, str " ", arg 1]
      datatype z = datatype Name.t
   in
      case name p of
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

end
