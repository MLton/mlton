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

local open Type
in
   structure Tycon = Tycon
   structure Tyvar = Tyvar
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
	 Array_array
       | Array_array0
       | Array_array0Const
       | Array_length
       | Array_sub
       | Array_update
       | BuildConstant of string
       | Byte_byteToChar
       | Byte_charToByte
       | C_CS_charArrayToWord8Array
       | Char_chr
       | Char_ge
       | Char_gt
       | Char_le
       | Char_lt
       | Char_ord
       | Constant of string
       | Cpointer_isNull
       | Exn_extra
       | Exn_keepHistory
       | Exn_name
       | Exn_setExtendExtra
       | Exn_setInitExtra
       | Exn_setTopLevelHandler
       | FFI of string
       | GC_collect
       | GC_pack
       | GC_unpack
       | Int_add
       | Int_addCheck
       | Int_ge
       | Int_geu
       | Int_gt
       | Int_gtu
       | Int_le
       | Int_lt
       | Int_mul
       | Int_mulCheck
       | Int_neg
       | Int_negCheck
       | Int_quot
       | Int_rem
       | Int_sub
       | Int_subCheck
       | IntInf_add
       | IntInf_andb
       | IntInf_arshift
       | IntInf_compare
       | IntInf_equal
       | IntInf_fromVector
       | IntInf_fromWord
       | IntInf_gcd
       | IntInf_lshift
       | IntInf_mul
       | IntInf_notb
       | IntInf_neg
       | IntInf_orb
       | IntInf_quot
       | IntInf_rem
       | IntInf_sub
       | IntInf_toString
       | IntInf_toVector
       | IntInf_toWord
       | IntInf_xorb
       | MLton_bogus
       | MLton_bug
       | MLton_deserialize
       | MLton_eq
       | MLton_equal
       | MLton_halt
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
       | Real_abs
       | Real_add
       | Real_copysign
       | Real_div
       | Real_equal
       | Real_frexp
       | Real_fromInt
       | Real_ge
       | Real_gt
       | Real_ldexp
       | Real_le
       | Real_lt
       | Real_modf
       | Real_mul
       | Real_muladd
       | Real_mulsub
       | Real_neg
       | Real_qequal
       | Real_round
       | Real_sub
       | Real_toInt
       | Ref_assign
       | Ref_deref
       | Ref_ref
       | String_fromWord8Vector
       | String_toWord8Vector
       | Thread_atomicBegin
       | Thread_atomicEnd
       | Thread_canHandle
       | Thread_copy
       | Thread_copyCurrent
       | Thread_switchTo
       | Vector_fromArray
       | Vector_length
       | Vector_sub
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

      val equals: t * t -> bool = op =

      val isCommutative =
	 fn Int_add => true
	  | Int_addCheck => true
	  | Int_mul => true
	  | Int_mulCheck => true
	  | IntInf_equal => true
	  | MLton_eq => true
	  | MLton_equal => true
	  | Real_add => true
	  | Real_mul => true
	  | Real_qequal => true
	  | Word32_add => true
	  | Word32_addCheck => true
	  | Word32_andb => true
	  | Word32_mul => true
	  | Word32_mulCheck => true
	  | Word32_orb => true
	  | Word32_xorb => true
	  | Word8_add => true
	  | Word8_andb => true
	  | Word8_mul => true
	  | Word8_orb => true
	  | Word8_xorb => true
	  | _ => false

      val mayOverflow =
	 fn Int_addCheck => true
	  | Int_mulCheck => true
	  | Int_negCheck => true
	  | Int_subCheck => true
	  | Word32_addCheck => true
	  | Word32_mulCheck => true
	  | _ => false

      val mayRaise = mayOverflow

      datatype z = datatype Kind.t
	       
      (* The values of these strings are important since they are referred to
       * in the basis library code.  See basis-library/misc/primitive.sml.
       *)
      val strings =
	 [
	  (Array_array, Moveable, "Array_array"),
	  (Array_array0, Moveable, "Array_array0"),
	  (Array_array0Const, Moveable, "Array_array0Const"),
	  (Array_length, Functional, "Array_length"),
	  (Array_sub, DependsOnState, "Array_sub"),
	  (Array_update, SideEffect, "Array_update"),
	  (Byte_byteToChar, Functional, "Byte_byteToChar"),
	  (Byte_charToByte, Functional, "Byte_charToByte"),
	  (C_CS_charArrayToWord8Array, DependsOnState,
	   "C_CS_charArrayToWord8Array"),
	  (Char_chr, Functional, "Char_chr"),
	  (Char_ge, Functional, "Char_ge"),
	  (Char_gt, Functional, "Char_gt"),
	  (Char_le, Functional, "Char_le"),
	  (Char_lt, Functional, "Char_lt"),
	  (Char_ord, Functional, "Char_ord"),
	  (Cpointer_isNull, Functional, "Cpointer_isNull"),
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
	  (IntInf_fromVector, Functional, "IntInf_fromVector"),
	  (IntInf_fromWord, Functional, "IntInf_fromWord"),
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
	  (Int_add, Functional, "Int_add"),
	  (Int_addCheck, SideEffect, "Int_addCheck"),
	  (Int_ge, Functional, "Int_ge"),
	  (Int_geu, Functional, "Int_geu"),
	  (Int_gt, Functional, "Int_gt"),
	  (Int_gtu, Functional, "Int_gtu"),
	  (Int_le, Functional, "Int_le"),
	  (Int_lt, Functional, "Int_lt"),
	  (Int_mul, Functional, "Int_mul"),
	  (Int_mulCheck, SideEffect, "Int_mulCheck"),
	  (Int_neg, Functional, "Int_neg"),
	  (Int_negCheck, SideEffect, "Int_negCheck"),
	  (Int_quot, Functional, "Int_quot"),
	  (Int_rem, Functional, "Int_rem"),
	  (Int_sub, Functional, "Int_sub"),
	  (Int_subCheck, SideEffect, "Int_subCheck"),
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
	  (Real_Math_acos, Functional, "Real_Math_acos"),
	  (Real_Math_asin, Functional, "Real_Math_asin"),
	  (Real_Math_atan, Functional, "Real_Math_atan"),
	  (Real_Math_atan2, Functional, "Real_Math_atan2"),
	  (Real_Math_cos, Functional, "Real_Math_cos"),
	  (Real_Math_cosh, Functional, "Real_Math_cosh"),
	  (Real_Math_exp, Functional, "Real_Math_exp"),
	  (Real_Math_ln, Functional, "Real_Math_ln"),
	  (Real_Math_log10, Functional, "Real_Math_log10"),
	  (Real_Math_pow, Functional, "Real_Math_pow"),
	  (Real_Math_sin, Functional, "Real_Math_sin"),
	  (Real_Math_sinh, Functional, "Real_Math_sinh"),
	  (Real_Math_sqrt, Functional, "Real_Math_sqrt"),
	  (Real_Math_tan, Functional, "Real_Math_tan"),
	  (Real_Math_tanh, Functional, "Real_Math_tanh"),
	  (Real_abs, Functional, "Real_abs"),
	  (Real_add, Functional, "Real_add"),
	  (Real_copysign, Functional, "Real_copysign"),
	  (Real_div, Functional, "Real_div"),
	  (Real_equal, Functional, "Real_equal"),
	  (Real_frexp, SideEffect, "Real_frexp"),
	  (Real_fromInt, Functional, "Real_fromInt"),
	  (Real_ge, Functional, "Real_ge"),
	  (Real_gt, Functional, "Real_gt"),
	  (Real_ldexp, Functional, "Real_ldexp"),
	  (Real_le, Functional, "Real_le"),
	  (Real_lt, Functional, "Real_lt"),
	  (Real_modf, SideEffect, "Real_modf"),
	  (Real_mul, Functional, "Real_mul"),
	  (Real_muladd, Functional, "Real_muladd"),
	  (Real_mulsub, Functional, "Real_mulsub"),
	  (Real_neg, Functional, "Real_neg"),
	  (Real_qequal, Functional, "Real_qequal"),
	  (Real_round, Functional, "Real_round"),
	  (Real_sub, Functional, "Real_sub"),
	  (Real_toInt, Functional, "Real_toInt"),
	  (Ref_assign, SideEffect, "Ref_assign"),
	  (Ref_deref, DependsOnState, "Ref_deref"),
	  (Ref_ref, Moveable, "Ref_ref"),
	  (String_fromWord8Vector, Functional, "String_fromWord8Vector"),
	  (String_toWord8Vector, Functional, "String_toWord8Vector"),
	  (Thread_atomicBegin, SideEffect, "Thread_atomicBegin"),
	  (Thread_atomicEnd, SideEffect, "Thread_atomicEnd"),
	  (Thread_canHandle, DependsOnState, "Thread_canHandle"),
	  (Thread_copy, Moveable, "Thread_copy"),
	  (Thread_copyCurrent, SideEffect, "Thread_copyCurrent"),
	  (Thread_switchTo, SideEffect, "Thread_switchTo"),
	  (Vector_fromArray, DependsOnState, "Vector_fromArray"),
	  (Vector_length, Functional, "Vector_length"),
	  (Vector_sub, Functional, "Vector_sub"),
	  (Word32_add, Functional, "Word32_add"),
	  (Word32_addCheck, SideEffect, "Word32_addCheck"),
	  (Word32_andb, Functional, "Word32_andb"),
	  (Word32_arshift, Functional, "Word32_arshift"),
	  (Word32_div, Functional, "Word32_div"),
	  (Word32_fromInt, Functional, "Word32_fromInt"),
	  (Word32_ge, Functional, "Word32_ge"),
	  (Word32_gt, Functional, "Word32_gt"),
	  (Word32_le, Functional, "Word32_le"),
	  (Word32_lshift, Functional, "Word32_lshift"),
	  (Word32_lt, Functional, "Word32_lt"),
	  (Word32_mod, Functional, "Word32_mod"),
	  (Word32_mul, Functional, "Word32_mul"),
	  (Word32_mulCheck, SideEffect, "Word32_mulCheck"),
	  (Word32_neg, Functional, "Word32_neg"),
	  (Word32_notb, Functional, "Word32_notb"),
	  (Word32_orb, Functional, "Word32_orb"),
	  (Word32_rol, Functional, "Word32_rol"),
	  (Word32_ror, Functional, "Word32_ror"),
	  (Word32_rshift, Functional, "Word32_rshift"),
	  (Word32_sub, Functional, "Word32_sub"),
	  (Word32_toIntX, Functional, "Word32_toIntX"),
	  (Word32_xorb, Functional, "Word32_xorb"),
	  (Word8Array_subWord, DependsOnState, "Word8Array_subWord"),
	  (Word8Array_updateWord, SideEffect, "Word8Array_updateWord"),
	  (Word8Vector_subWord, Functional, "Word8Vector_subWord"),
	  (Word8_add, Functional, "Word8_add"),
	  (Word8_andb, Functional, "Word8_andb"),
	  (Word8_arshift, Functional, "Word8_arshift"),
	  (Word8_div, Functional, "Word8_div"),
	  (Word8_fromInt, Functional, "Word8_fromInt"),
	  (Word8_fromLargeWord, Functional, "Word8_fromLargeWord"),
	  (Word8_ge, Functional, "Word8_ge"),
	  (Word8_gt, Functional, "Word8_gt"),
	  (Word8_le, Functional, "Word8_le"),
	  (Word8_lshift, Functional, "Word8_lshift"),
	  (Word8_lt, Functional, "Word8_lt"),
	  (Word8_mod, Functional, "Word8_mod"),
	  (Word8_mul, Functional, "Word8_mul"),
	  (Word8_neg, Functional, "Word8_neg"),
	  (Word8_notb, Functional, "Word8_notb"),
	  (Word8_orb, Functional, "Word8_orb"),
	  (Word8_rol, Functional, "Word8_rol"),
	  (Word8_ror, Functional, "Word8_ror"),
	  (Word8_rshift, Functional, "Word8_rshift"),
	  (Word8_sub, Functional, "Word8_sub"),
	  (Word8_toInt, Functional, "Word8_toInt"),
	  (Word8_toIntX, Functional, "Word8_toIntX"),
	  (Word8_toLargeWord, Functional, "Word8_toLargeWord"),
	  (Word8_toLargeWordX, Functional, "Word8_toLargeWordX"),
	  (Word8_xorb, Functional, "Word8_xorb"),
	  (World_save, SideEffect, "World_save")]

      fun toString n =
	 case n of
	    BuildConstant s => s
	  | Constant s => s
	  | FFI s => s
	  | _ => (case List.peek (strings, fn (n', _, _) => n = n') of
		     NONE => Error.bug "Prim.toString missing name"
		   | SOME (_, _, s) => s)

      val layout = Layout.str o toString
   end

datatype t =
   T of {name: Name.t,
	 nameString: string,
	 scheme: Scheme.t,
	 kind: Kind.t,
	 numArgs: int option}

local
   fun make sel (T r) = sel r
in
   val kind = make #kind
   val name = make #name
   val numArgs = make #numArgs
   val scheme = make #scheme
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

structure Scheme =
   struct
      open Scheme
	 
      fun numArgs (s: t): int option =
	 case Type.dearrowOpt (ty s) of
	    NONE => NONE
	  | SOME (t, _) => (case Type.detupleOpt t of
			      NONE => SOME 1
			    | SOME ts => SOME (Vector.length ts))
   end

fun new (n: Name.t, k: Kind.t, s: Scheme.t): t =
   T {
      kind = k,
      name = n,
      nameString = Name.toString n,
      numArgs = Scheme.numArgs s,
      scheme = s
      }

local
   fun make f (name: string, s: Scheme.t): t =
      new (f name, Kind.Functional, s)
in
   val buildConstant = make Name.BuildConstant
   val constant = make Name.Constant
end

fun equals (p, p') = Name.equals (name p, name p')

local
   val newPrim = new
   open Type Scheme
   val new = newPrim
   val --> = arrow
   infix -->

   val new =
      fn (n: Name.t, s: Scheme.t) =>
      let
	 val k =
	    case n of
	       Name.FFI _ =>
		  (if isSome (Scheme.numArgs s)
		      then Kind.SideEffect
		   else Kind.DependsOnState)
	     | _ => (case List.peek (Name.strings, fn (n', _, _) => n = n') of
			NONE => Error.bug "strange name"
		      | SOME (_, k, _) => k)
      in new (n, k, s)
      end
   val tuple = tuple o Vector.fromList    
in
   val array0 = new (Name.Array_array0, make1 (fn a => unit --> array a))
   val array = new (Name.Array_array, make1 (fn a => int --> array a))
   val assign = new (Name.Ref_assign, make1 (fn a => tuple [reff a, a] --> unit))
   val bogus = new (Name.MLton_bogus, make1 (fn a => a))
   val bug = new (Name.MLton_bug, make0 (string --> unit))
   val deref = new (Name.Ref_deref, make1 (fn a => reff a --> a))
   val deserialize =
      new (Name.MLton_deserialize, make1 (fn a => vector word8 --> a))
   val eq = new (Name.MLton_eq, makeEqual1 (fn a => tuple [a, a] --> bool))
   val equal = new (Name.MLton_equal, makeEqual1 (fn a => tuple [a, a] --> bool))
   val gcCollect = new (Name.GC_collect, make0 (tuple [word, bool] --> unit))
   val reff = new (Name.Ref_ref, make1 (fn a => a --> reff a))
   val serialize = new (Name.MLton_serialize, make1 (fn a => a --> vector word8))
   val vectorLength = new (Name.Vector_length, make1 (fn a => vector a --> int))
   val vectorSub =
      new (Name.Vector_sub, make1 (fn a => tuple [vector a, int] --> a))

   fun new0 (name, ty) = new (name, make0 ty)

   val intNeg = new0 (Name.Int_neg, int --> int)
   val intNegCheck = new0 (Name.Int_negCheck, int --> int)
   val intInfNeg =
      new0 (Name.IntInf_neg, tuple [intInf, word] --> intInf)
   val intInfEqual = new0 (Name.IntInf_equal, tuple [intInf, intInf] --> bool)
   val word8Neg = new0 (Name.Word8_neg, word8 --> word8)
   val word8Notb = new0 (Name.Word8_notb, word8 --> word8)
   val word32Notb = new0 (Name.Word32_notb, word --> word)
   val word32Neg = new0 (Name.Word32_neg, word --> word)

   local
      fun make n = new0 (n, tuple [int, int] --> int)
   in
      val intAdd = make Name.Int_add
      val intAddCheck = make Name.Int_addCheck
      val intMul = make Name.Int_mul
      val intMulCheck = make Name.Int_mulCheck
      val intSub = make Name.Int_sub
      val intSubCheck = make Name.Int_subCheck
   end

   local
      fun make n = new0 (n, tuple [word, word] --> word)
   in
      val word32Add = make Name.Word32_add
      val word32AddCheck = make Name.Word32_addCheck
      val word32Andb = make Name.Word32_andb
      val word32Mul = make Name.Word32_mul
      val word32MulCheck = make Name.Word32_mulCheck
      val word32Rshift = make Name.Word32_rshift
      val word32Sub = make Name.Word32_sub
   end

   local
      fun make n = new0 (n, tuple [word, word] --> bool)
   in
      val word32Gt = make Name.Word32_gt
   end

   val word32FromInt = new0 (Name.Word32_fromInt, int --> word)
   val word32ToIntX = new0 (Name.Word32_toIntX, word --> int)
      
   fun ffi (name: string, s: Scheme.t) =
      new (Name.FFI name, s)

   fun newNullary (name: string) = new0 (Name.FFI name, unit --> unit)

   val allocTooLarge = newNullary "MLton_allocTooLarge"
end
   
val new: string * Scheme.t -> t =
   fn (name, scheme) =>
   let
      val (name, kind) =
	 case List.peek (Name.strings, fn (_, _, s) => s = name) of
	    NONE => Error.bug (concat ["unknown primitive: ", name])
	  | SOME (n, k, _) => (n, k)
   in
      new (name, kind, scheme)
   end

val new = Trace.trace2 ("Prim.new", String.layout, Scheme.layout, layout) new
   
fun 'a checkApp {prim, targs, args,
		 con, detupleOpt, dearrowOpt, equals, isUnit}
   : 'a option =
   let
      val error = NONE
      val Scheme.T {tyvars, ty} = scheme prim
      fun show s =
	 if true
	    then ()
	 else Out.print s
   in
      if Vector.length targs <> Vector.length tyvars
	 then
	    (show (concat ["primapp error, #targs=",
			   Int.toString (Vector.length targs),
			   ", #tyvars=",
			   Int.toString (Vector.length tyvars), "\n"])
	     ; error)
      else
	 let
	    val env = Vector.zip (tyvars, targs)
	    fun var a =
	       case Vector.peek (env, fn (a', _) => Tyvar.equals (a, a')) of
		  NONE => Error.bug "prim scheme with free tyvar"
		| SOME (_, t) => t
	    val ty = Type.hom {ty = ty, var = var, con = con}
	 in
	    case numArgs prim of
	       NONE => if Vector.isEmpty args
			  then SOME ty
		       else (show "primapp error, no numArgs\n"
			     ; error)
	     | SOME n =>
		  case dearrowOpt ty of
		     NONE => error
		   | SOME (argType, result) =>
			case (n, Vector.length args) of
			   (0, 0) => SOME result
			 | (1, 1) =>
			      if equals (argType, Vector.sub (args, 0))
				 then SOME result
			      else error
			 | _ => 
			      case detupleOpt argType of
				 NONE => error
			       | SOME argTypes =>
				    if Vector.equals (args, argTypes, equals)
				       then SOME result
				    else error
	 end
   end

fun returnsBool p =
   case Type.dearrowOpt (Scheme.ty (scheme p)) of
      SOME (_, Type.Con (tycon, _)) => Tycon.equals (tycon, Tycon.bool)
    | _ => false

fun 'a extractTargs {prim, args, result,
		     dearray,
		     dearrow: 'a -> 'a * 'a,
		     deref, devector} =
   let
      val one = Vector.new1
      fun arg i = Vector.sub (args, i)
      datatype z = datatype Name.t
   in
      case name prim of
	 Array_array => one (dearray result)
       | Array_array0 => one (dearray result)
       | Array_array0Const => one (dearray result)
       | Array_sub => one result
       | Array_update => one (arg 2)
       | Array_length => one (dearray (arg 0))
       | Exn_extra => one result
       | Exn_setExtendExtra => one (#2 (dearrow (arg 0)))
       | Exn_setInitExtra => one (arg 0)
       | MLton_bogus => one result
       | MLton_deserialize => one result
       | MLton_eq => one (arg 0)
       | MLton_equal => one (arg 0)
       | MLton_serialize => one (arg 0)
       | MLton_size => one (deref (arg 0))
       | Ref_assign => one (arg 1)
       | Ref_deref => one result
       | Ref_ref => one (arg 0)
       | Vector_fromArray => one (dearray (arg 0))
       | Vector_length => one (devector (arg 0))
       | Vector_sub => one result
       | _ => Vector.new0 ()
   end

structure SmallIntInf = Const.SmallIntInf

structure ApplyArg =
   struct
      datatype 'a t =
	 Con of {con: Con.t, hasArg: bool}
       | Const of Const.Node.t
       | Var of 'a

      fun layout layoutX =
	 fn Con {con, hasArg} =>
	      Layout.record [("con", Con.layout con),
			     ("hasArg", Bool.layout hasArg)]
	  | Const c => Const.Node.layout c
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
      datatype z = datatype Const.Node.t
      val bool = ApplyResult.Bool
      val char = ApplyResult.Const o Const.fromChar
      val int = ApplyResult.Const o Const.fromInt
      val intInf = ApplyResult.Const o Const.fromIntInf
      val intInfConst = intInf o IntInf.fromInt
      val string = ApplyResult.Const o Const.fromString
      val word = ApplyResult.Const o Const.fromWord
      val word32 = word
      val word8 = ApplyResult.Const o Const.fromWord8
      val t = ApplyResult.truee
      val f = ApplyResult.falsee
      local
	 fun make from (f, c1, c2) = from (f (c1, c2))
      in
	 fun io z = make int z
	 val wo = make word
	 fun pred z = make bool z
	 val iio = make intInf
      end
      fun iu (f, i1, i2) = bool (f (Word.fromInt i1, Word.fromInt i2))
      fun w8w (f, w8: Word.t, w: Word.t) = word8 (f (Word8.fromWord w8, w))
      fun w8p (p, w1, w2) = bool (p (Word8.fromWord w1, Word8.fromWord w2))
      fun w8o (f, w1, w2) = word8 (f (Word8.fromWord w1, Word8.fromWord w2))
      val eq =
 	 fn (Char c1, Char c2) => bool (Char.equals (c1, c2))
 	  | (Int i1, Int i2) => bool (Int.equals (i1, i2))
 	  | (Word w1, Word w2) => bool (Word.equals (w1, w2))
 	  | _ => ApplyResult.Unknown
      val equal =
	 fn (Char c1, Char c2) => bool (Char.equals (c1, c2))
	  | (Int i1, Int i2) => bool (Int.equals (i1, i2))
	  | (String s1, String s2) => bool (String.equals (s1, s2))
	  | (Word w1, Word w2) => bool (Word.equals (w1, w2))
	  | _ => ApplyResult.Unknown
      fun allConsts (cs: Const.Node.t list) =
	 (case (name p, cs) of
	     (Byte_byteToChar, [Word w]) => char (Word.toChar w)
	   | (Byte_charToByte, [Char c]) => word8 (Word8.fromChar c)
	   | (Char_lt, [Char c1, Char c2]) => pred (Char.<, c1, c2)
	   | (Char_le, [Char c1, Char c2]) => pred (Char.<=, c1, c2)
	   | (Char_gt, [Char c1, Char c2]) => pred (Char.>, c1, c2)
	   | (Char_ge, [Char c1, Char c2]) => pred (Char.>=, c1, c2)
	   | (Char_chr, [Int i]) => char (Char.fromInt i)
	   | (Char_ord, [Char c]) => int (Char.toInt c)
	   | (Int_add, [Int i1, Int i2]) => io (Int.+, i1, i2)
	   | (Int_addCheck, [Int i1, Int i2]) => io (Int.+, i1, i2)
	   | (Int_mul, [Int i1, Int i2]) => io (Int.*, i1, i2)
	   | (Int_mulCheck, [Int i1, Int i2]) => io (Int.*, i1, i2)
	   | (Int_sub, [Int i1, Int i2]) => io (Int.-, i1, i2)
	   | (Int_subCheck, [Int i1, Int i2]) => io (Int.-, i1, i2)
	   | (Int_lt, [Int i1, Int i2]) => pred (Int.<, i1, i2)
	   | (Int_le, [Int i1, Int i2]) => pred (Int.<=, i1, i2)
	   | (Int_gt, [Int i1, Int i2]) => pred (Int.>, i1, i2)
	   | (Int_ge, [Int i1, Int i2]) => pred (Int.>=, i1, i2)
	   | (Int_geu, [Int i1, Int i2]) => iu (Word.>=, i1, i2)
	   | (Int_gtu, [Int i1, Int i2]) => iu (Word.>, i1, i2)
	   | (Int_neg, [Int i]) => int (~ i)
	   | (Int_negCheck, [Int i]) => int (~ i)
	   | (Int_quot, [Int i1, Int i2]) => io (Int.quot, i1, i2)
	   | (Int_rem, [Int i1, Int i2]) => io (Int.rem, i1, i2)
	   | (IntInf_compare, [IntInf i1, IntInf i2]) =>
		int (case IntInf.compare (i1, i2) of
			Relation.LESS => ~1
		      | Relation.EQUAL => 0
		      | Relation.GREATER => 1)
	   | (IntInf_equal, [IntInf i1, IntInf i2]) =>
		bool (IntInf.equals (i1, i2))
	   | (IntInf_fromWord, [Word w]) => intInf (SmallIntInf.fromWord w)
	   | (IntInf_toWord, [IntInf i]) =>
		(case SmallIntInf.toWord i of
		    NONE => ApplyResult.Unknown
		  | SOME w => word w)
	   | (MLton_eq, [c1, c2]) => eq (c1, c2)
	   | (MLton_equal, [c1, c2]) => equal (c1, c2)
	   | (Word8_mul, [Word w1, Word w2]) => w8o (Word8.*, w1, w2)
	   | (Word8_add, [Word w1, Word w2]) => w8o (Word8.+, w1, w2)
	   | (Word8_sub, [Word w1, Word w2]) => w8o (Word8.-, w1, w2)
	   | (Word8_lt, [Word w1, Word w2]) => w8p (Word8.<, w1, w2)
	   | (Word8_lshift, [Word w1, Word w2]) => w8w (Word8.<<, w1, w2)
	   | (Word8_le, [Word w1, Word w2]) => w8p (Word8.<=, w1, w2)
	   | (Word8_gt, [Word w1, Word w2]) => w8p (Word8.>, w1, w2)
	   | (Word8_ge, [Word w1, Word w2]) => w8p (Word8.>=, w1, w2)
	   | (Word8_rol, [Word w1, Word w2]) => w8w (Word8.rol, w1, w2)
	   | (Word8_ror, [Word w1, Word w2]) => w8w (Word8.ror, w1, w2)
	   | (Word8_rshift, [Word w1, Word w2]) => w8w (Word8.>>, w1, w2)
	   | (Word8_andb, [Word w1, Word w2]) => w8o (Word8.andb, w1, w2)
	   | (Word8_div, [Word w1, Word w2]) => w8o (Word8.div, w1, w2)
	   | (Word8_fromInt, [Int i]) => word8 (Word8.fromInt i)
	   | (Word8_fromLargeWord, [Word w]) => word8 (Word8.fromWord w)
	   | (Word8_mod, [Word w1, Word w2]) => w8o (Word8.mod, w1, w2)
	   | (Word8_notb, [Word w]) => word8 (Word8.notb (Word8.fromWord w))
	   | (Word8_orb, [Word w1, Word w2]) => w8o (Word8.orb, w1, w2)
	   | (Word8_toInt, [Word w]) => int (Word8.toInt (Word8.fromWord w))
	   | (Word8_toIntX, [Word w]) => int (Word8.toIntX (Word8.fromWord w))
	   | (Word8_toLargeWord, [Word w]) =>
		word (Word8.toWord (Word8.fromWord w))
	   | (Word8_toLargeWordX, [Word w]) => 
		word (Word8.toWordX (Word8.fromWord w))
	   | (Word8_xorb, [Word w1, Word w2]) => w8o (Word8.xorb, w1, w2)
	   | (Word8_arshift, [Word w1, Word w2]) => w8w (Word8.~>>, w1, w2)
	   | (Word32_add, [Word w1, Word w2]) => wo (Word.+, w1, w2)
	   | (Word32_addCheck, [Word w1, Word w2]) =>
		wo (MLton.Word.addCheck, w1, w2)
	   | (Word32_andb, [Word w1, Word w2]) => wo (Word.andb, w1, w2)
	   | (Word32_arshift, [Word w1, Word w2]) => wo (Word.~>>, w1, w2)
	   | (Word32_div, [Word w1, Word w2]) => wo (Word.div, w1, w2)
	   | (Word32_fromInt, [Int i]) => word (Word.fromInt i)
	   | (Word32_ge, [Word w1, Word w2]) => pred (Word.>=, w1, w2)
	   | (Word32_gt, [Word w1, Word w2]) => pred (Word.>, w1, w2)
	   | (Word32_le, [Word w1, Word w2]) => pred (Word.<=, w1, w2)
	   | (Word32_lshift, [Word w1, Word w2]) => wo (Word.<<, w1, w2)
	   | (Word32_lt, [Word w1, Word w2]) => pred (Word.<, w1, w2)
	   | (Word32_mod, [Word w1, Word w2]) => wo (Word.mod, w1, w2)
	   | (Word32_mul, [Word w1, Word w2]) => wo (Word.*, w1, w2)
	   | (Word32_mulCheck, [Word w1, Word w2]) =>
		wo (MLton.Word.mulCheck, w1, w2)
	   | (Word32_notb, [Word w]) => word (Word.notb w)
	   | (Word32_orb, [Word w1, Word w2]) => wo (Word.orb, w1, w2)
	   | (Word32_rol, [Word w1, Word w2]) => wo (Word.rol, w1, w2)
	   | (Word32_ror, [Word w1, Word w2]) => wo (Word.ror, w1, w2)
	   | (Word32_rshift, [Word w1, Word w2]) => wo (Word.>>, w1, w2)
	   | (Word32_sub, [Word w1, Word w2]) => wo (Word.-, w1, w2)
	   | (Word32_toIntX, [Word w]) => int (Word.toIntX w)
	   | (Word32_xorb, [Word w1, Word w2]) => wo (Word.xorb, w1, w2)
	   | _ => ApplyResult.Unknown)
	     handle Chr => ApplyResult.Unknown
		  | Div => ApplyResult.Unknown
		  | Exn.Overflow => ApplyResult.Overflow
		  | Subscript => ApplyResult.Unknown
      fun someVars () =
	 let
	    datatype z = datatype ApplyResult.t
	    fun add (x, i) = if i = 0 then Var x else Unknown
	    fun mul (x, i, neg) =
	       case i of
		  0 => int 0
		| 1 => Var x
		| ~1 => Apply (neg, [x])
		| _ => Unknown
	    val name = name p
	    fun varIntInf (x, i: IntInf.t, space, inOrder) =
	       let
		  fun negate () = Apply (intInfNeg, [x, space])
		  val i = IntInf.toInt i
	       in
		  case name of
		     IntInf_add => if i = 0 then Var x else Unknown
		   | IntInf_mul =>
			(case i of
			    0 => intInfConst 0
			  | 1 => Var x
			  | ~1 => negate ()
			  | _ => Unknown)
		   | IntInf_quot => if inOrder
				       then (case i of
						1 => Var x
					      | ~1 => negate ()
					      | _ => Unknown)
				    else Unknown
		   | IntInf_rem => if inOrder andalso (i = ~1 orelse i = 1)
				      then intInfConst 0
				   else Unknown
		   | IntInf_sub => if i = 0
				      then if inOrder
					      then Var x
					   else negate ()
				   else Unknown
		   | _ => Unknown
	       end handle Exn.Overflow => Unknown
	    fun varWord (x, w, inOrder) =
	       let
		  fun allOnes isWord8 = if isWord8 then 0wxFF else 0wxFFFFFFFF
		  val max = allOnes
		  fun zero isWord8 = if isWord8 then word8 0wx0 else word32 0wx0
		  fun maxRes isWord8 =
		     if isWord8 then word8 0wxFF else word32 0wxFFFFFFFF
		  fun add () = if w = 0w0 then Var x else Unknown
		  fun andb isWord8 =
		     if w = 0w0
		        then zero isWord8
		     else if w = allOnes isWord8
			     then Var x
			  else Unknown
		  fun arshift isWord8 =
		     if w = 0w0 then if inOrder then Var x else zero isWord8
		     else if w = max isWord8
			     then if inOrder then Unknown else maxRes isWord8
			  else Unknown
		  nonfix div
		  fun div () = if inOrder andalso w = 0w1 then Var x else Unknown
		  fun ge isWord8 =
		     if inOrder
			then if w = 0w0 then t else Unknown
		     else if w = max isWord8 then t else Unknown
		  fun gt isWord8 =
		     if inOrder
			then if w = max isWord8 then f else Unknown
		     else if w = 0w0 then f else Unknown
		  fun le isWord8 =
		     if inOrder
			then if w = max isWord8 then t else Unknown
		     else if w = 0w0 then t else Unknown
		  fun lt isWord8 =
		     if inOrder
			then if w = 0w0 then f else Unknown
		     else if w = max isWord8 then f else Unknown
		  nonfix mod
		  fun mod isWord8 =
		     if inOrder andalso w = 0w1 then zero isWord8 else Unknown
		  fun mul isWord8 =
		     case w of
			0w0 => zero isWord8
		      | 0w1 => Var x
		      | _ => Unknown
		  fun orb isWord8 =
		     if w = 0w0
			then Var x
		     else if w = allOnes isWord8
			     then maxRes isWord8
			  else Unknown
		  fun ro isWord8 =
		     if inOrder
			then
			   if 0w0 = Word.mod (w, if isWord8 then 0w8 else 0w32)
			      then Var x
			   else Unknown
		     else
			if w = 0w0
			   then zero isWord8
			else if w = allOnes isWord8
				then maxRes isWord8
			     else Unknown
		  fun shift isWord8 =
		     if inOrder
			then if w = 0w0
				then Var x
			     else if w >= (if isWord8 then 0w8 else 0w32)
				     then zero isWord8
				  else Unknown
		     else if w = 0w0
			     then zero isWord8
			  else Unknown
		  fun sub isWord8 =
		     if w = 0w0
			then
			   if inOrder
			      then Var x
			   else Apply (if isWord8
					  then word8Neg
				       else word32Neg,
					  [x])
		     else Unknown
		  fun xorb isWord8 =
		     if w = 0w0
			then Var x
		     else if w = allOnes isWord8
			     then Apply (if isWord8 then word8Notb
					 else word32Notb,
					 [x])
			  else Unknown
	       in
		  case name of
		     Word8_add => add ()
		   | Word32_add => add ()
		   | Word32_addCheck => add ()
		   | Word8_andb => andb true
		   | Word32_andb => andb false
		   | Word8_arshift => arshift true
		   | Word32_arshift => arshift false
		   | Word8_div => div ()
		   | Word32_div => div ()
		   | Word8_ge => ge true
		   | Word32_ge => ge false
		   | Word8_gt => gt true
		   | Word32_gt => gt false
		   | Word8_le => le true
		   | Word32_le => le false
		   | Word8_lshift => shift true
		   | Word32_lshift => shift false
		   | Word8_lt => lt true
		   | Word32_lt => lt false
		   | Word8_mod => mod true
		   | Word32_mod => mod false
		   | Word8_mul => mul true
		   | Word32_mul => mul false
		   | Word32_mulCheck => mul false
		   | Word8_orb => orb true
		   | Word32_orb => orb false
		   | Word8_rol => ro true
		   | Word32_rol => ro false
		   | Word8_ror => ro true
		   | Word32_ror => ro false
		   | Word8_rshift => shift true
		   | Word32_rshift => shift false
		   | Word8_sub => sub true
		   | Word32_sub => sub false
		   | Word8_xorb => xorb true
		   | Word32_xorb => xorb false
		   | _ => Unknown
	       end
	    fun varInt (x, i, inOrder) =
	       case name of 
		  Int_add => add (x, i)
		| Int_addCheck => add (x, i)
		| Int_mul => mul (x, i, intNeg)
		| Int_mulCheck => mul (x, i, intNegCheck)
		| Int_quot => if inOrder
				 then (case i of
					  1 => Var x
					| ~1 => Apply (intNeg, [x])
					| _ => Unknown)
			      else Unknown
		| Int_rem => if inOrder andalso (i = ~1 orelse i = 1)
				 then int 0
			      else Unknown
		| Int_sub =>
		     if i = 0
			then if inOrder
				then Var x
			     else Apply (intNeg, [x])
		     else Unknown
		| Int_subCheck =>
		     if i = 0
			then if inOrder
				then Var x
			     else Apply (intNegCheck, [x])
		     else Unknown
		| _ => Unknown
	    datatype z = datatype ApplyArg.t
	 in
	    case (name, args) of
	       (IntInf_neg, [Const (IntInf i), _]) => intInf (IntInf.~ i)
	     | (IntInf_toString, [Const (IntInf i), Const (Int base), _]) =>
		  let
		     val base =
			case base of
			   2 => StringCvt.BIN
			 | 8 => StringCvt.OCT 
			 | 10 => StringCvt.DEC
			 | 16 => StringCvt.HEX
			 | _ => Error.bug "strange base for IntInf_toString"
		  in
		     string (IntInf.format (i, base))
		  end
	     | (_, [Con {con = c, hasArg = h}, Con {con = c', hasArg = h'}]) =>
		  if name = MLton_equal orelse name = MLton_eq
		     then if Con.equals (c, c')
			     then if h
				     then bool true
				  else Unknown
			  else bool false
		  else Unknown
	     | (_, [Var x, Const (Word i)]) => varWord (x, i, true)
	     | (_, [Const (Word i), Var x]) => varWord (x, i, false)
	     | (_, [Var x, Const (Int i)]) => varInt (x, i, true)
	     | (_, [Const (Int i), Var x]) => varInt (x, i, false)
	     | (_, [Const (IntInf i1), Const (IntInf i2), _]) =>
		  (case name of
		      IntInf_add => iio (IntInf.+, i1, i2)
		    | IntInf_mul => iio (IntInf.*, i1, i2)
		    | IntInf_quot => iio (IntInf.quot, i1, i2)
		    | IntInf_rem => iio (IntInf.rem, i1, i2)
		    | IntInf_sub => iio (IntInf.-, i1, i2)
		    | _ => Unknown)
	     | (_, [Var x, Const (IntInf i), Var space]) =>
		  varIntInf (x, i, space, true)
	     | (_, [Const (IntInf i), Var x, Var space]) =>
		  varIntInf (x, i, space, false)
	     | (_, [Var x, Var y, _]) =>
		  if varEquals (x, y)
		     then
			(case name of
			    IntInf_quot => intInfConst 1
			  | IntInf_rem => intInfConst 0
			  | IntInf_sub => intInfConst 0
			  | _ => Unknown)
		  else Unknown
			  | (_, [Var x, Var y]) =>
			       if varEquals (x, y)
				  then let
					  val t = ApplyResult.truee
					  val f = ApplyResult.falsee
					  datatype z = datatype ApplyResult.t
				       in case name of
					  Char_lt => f
					| Char_le => t
					| Char_gt => f
					| Char_ge => t
					| Int_ge => t
					| Int_geu => t
					| Int_gt => f
					| Int_gtu => f
					| Int_le => t
					| Int_lt => f
					| Int_quot => int 1
					| Int_rem => int 0
					| Int_sub => int 0
					| IntInf_compare => int 0
					| IntInf_equal => t
					| MLton_eq => t
					| MLton_equal => t
					| Real_lt => f
					| Real_le => t
					| Real_equal => t
					| Real_gt => f
					| Real_ge => t
					| Real_qequal => t
					| Word8_andb => Var x
					| Word8_div => word8 0w1
					| Word8_ge => t
					| Word8_gt => f
					| Word8_le => t
					| Word8_lt => f
					| Word8_mod => word8 0w0
					| Word8_orb => Var x
					| Word8_sub => word8 0w0
					| Word8_xorb => word8 0w0
					| Word32_andb => Var x
					| Word32_div => word 0w1
					| Word32_ge => t
					| Word32_gt => f
					| Word32_le => t
					| Word32_lt => f
					| Word32_mod => word 0w0
					| Word32_orb => Var x
					| Word32_sub => word 0w0
					| Word32_xorb => word 0w0
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
	 Char_lt => two "<"
       | Char_le => two "<="
       | Char_gt => two ">"
       | Char_ge => two ">="
       | Char_chr => one "chr"
       | Char_ord => one "ord"
       | Int_mul => two "*?"
       | Int_mulCheck => two "*"
       | Int_add => two "+?"
       | Int_addCheck => two "+"
       | Int_sub => two "-?"
       | Int_subCheck => two "-"
       | Int_lt => two "<"
       | Int_le => two "<="
       | Int_gt => two ">"
       | Int_ge => two ">="
       | Int_geu => two ">=u"
       | Int_gtu => two ">u"
       | Int_neg => one "-?"
       | Int_negCheck => one "-"
       | IntInf_equal => two "="
       | MLton_eq => two "="
       | Real_Math_acos => one "acos"
       | Real_Math_asin => one "asin"
       | Real_Math_atan => one "atan"
       | Real_Math_cos => one "cos"
       | Real_Math_cosh => one "cosh"
       | Real_Math_exp => one "exp"
       | Real_Math_ln => one "ln"
       | Real_Math_log10  => one "log10"
       | Real_Math_pow => two "^"
       | Real_Math_sin => one "sin"
       | Real_Math_sinh => one "sinh"
       | Real_Math_sqrt => one "sqrt"
       | Real_Math_tan => one "tan"
       | Real_Math_tanh => one "tanh"
       | Real_mul => two "*"
       | Real_add => two "+"
       | Real_sub => two "-"
       | Real_div => two "/"
       | Real_lt => two "<"
       | Real_le => two "<="
       | Real_equal => two "=="
       | Real_gt => two ">"
       | Real_ge => two ">="
       | Real_qequal => two "?="
       | Real_neg => one "-"
       | Ref_assign => two ":="
       | Ref_deref => one "!"
       | Ref_ref => one "ref"
       | Vector_length => one "length"
       | Word32_add => two "+"
       | Word32_addCheck => two "+c"
       | Word32_andb => two "&"
       | Word32_arshift => two "~>>"
       | Word32_ge => two ">="
       | Word32_gt => two ">"
       | Word32_le => two "<="
       | Word32_lshift => two "<<"
       | Word32_lt => two "<"
       | Word32_mul => two "*"
       | Word32_mulCheck => two "*c"
       | Word32_neg => one "-"
       | Word32_orb => two "|"
       | Word32_rol => two "rol"
       | Word32_ror => two "ror"
       | Word32_rshift => two ">>"
       | Word32_sub => two "-"
       | Word32_xorb => two "^"
       | Word8_add => two "+"
       | Word8_andb => two "&"
       | Word8_arshift => two "~>>"
       | Word8_ge => two ">="
       | Word8_gt => two ">"
       | Word8_le => two "<="
       | Word8_lshift => two "<<"
       | Word8_lt => two "<"
       | Word8_mul => two "*"
       | Word8_neg => one "-"
       | Word8_orb => two "|"
       | Word8_rol => two "rol"
       | Word8_ror => two "ror"
       | Word8_rshift => two ">>"
       | Word8_sub => two "-"
       | Word8_xorb => two "^"
       | _ => seq [layout p, str " ", Vector.layout layoutArg args]
   end

end
