(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor SsaToRssa (S: SSA_TO_RSSA_STRUCTS): SSA_TO_RSSA =
struct

open S
open Rssa

datatype z = datatype IntSize.prim
datatype z = datatype WordSize.prim

structure S = Ssa
local
   open Ssa
in
   structure Con = Con
end
local
   open Runtime
in
   structure GCField = GCField
end

structure Prim =
   struct
      open Prim

      type t = Type.t Prim.t
   end

structure CFunction =
   struct
      open CFunction
      open Type.BuiltInCFunction

      type t = Type.t CFunction.t

      local
	 open Type
      in
	 val gcState = gcState
	 val Int32 = int (IntSize.I (Bits.fromInt 32))
	 val Word32 = word (Bits.fromInt 32)
	 val unit = unit
      end

      datatype z = datatype Convention.t
	 
      val copyCurrentThread =
	 T {args = Vector.new1 gcState,
	    bytesNeeded = NONE,
	    convention = Cdecl,
	    ensuresBytesFree = false,
	    mayGC = true,
	    maySwitchThreads = false,
	    modifiesFrontier = true,
	    modifiesStackTop = true,
	    name = "GC_copyCurrentThread",
	    return = unit}

      val copyThread =
	 T {args = Vector.new2 (gcState, Type.thread),
	    bytesNeeded = NONE,
	    convention = Cdecl,
	    ensuresBytesFree = false,
	    mayGC = true,
	    maySwitchThreads = false,
	    modifiesFrontier = true,
	    modifiesStackTop = true,
	    name = "GC_copyThread",
	    return = Type.thread}

      val exit =
	 T {args = Vector.new1 Int32,
	    bytesNeeded = NONE,
	    convention = Cdecl,
	    ensuresBytesFree = false,
	    mayGC = false,
	    maySwitchThreads = false,
	    modifiesFrontier = true,
	    modifiesStackTop = true,
	    name = "MLton_exit",
	    return = unit}

      fun gcArrayAllocate {return} =
	 T {args = Vector.new4 (gcState, Word32, Int32, Word32),
	    bytesNeeded = NONE,
	    convention = Cdecl,
	    ensuresBytesFree = true,
	    mayGC = true,
	    maySwitchThreads = false,
	    modifiesFrontier = true,
	    modifiesStackTop = true,
	    name = "GC_arrayAllocate",
	    return = return}

      local
	 fun make name =
	    T {args = Vector.new1 gcState,
	       bytesNeeded = NONE,
	       convention = Cdecl,
	       ensuresBytesFree = false,
	       mayGC = true,
	       maySwitchThreads = false,
	       modifiesFrontier = true,
	       modifiesStackTop = true,
	       name = name,
	       return = unit}
      in
	 val pack = make "GC_pack"
	 val unpack = make "GC_unpack"
      end

      val returnToC =
	 T {args = Vector.new0 (),
	    bytesNeeded = NONE,
	    convention = Cdecl,
	    ensuresBytesFree = false,
	    modifiesFrontier = true,
	    modifiesStackTop = true,
	    mayGC = true,
	    maySwitchThreads = true,
	    name = "Thread_returnToC",
	    return = unit}

      val threadSwitchTo =
	 T {args = Vector.new2 (Type.thread, Word32),
	    bytesNeeded = NONE,
	    convention = Cdecl,
	    ensuresBytesFree = true,
	    mayGC = true,
	    maySwitchThreads = true,
	    modifiesFrontier = true,
	    modifiesStackTop = true,
	    name = "Thread_switchTo",
	    return = unit}

      fun weakCanGet t =
	 vanilla {args = Vector.new1 t,
		  name = "GC_weakCanGet",
		  return = Type.bool}
	 
      fun weakGet {arg, return} =
	 vanilla {args = Vector.new1 arg,
		  name = "GC_weakGet",
		  return = return}
		  
      fun weakNew {arg, return} =
	 T {args = Vector.new3 (gcState, Word32, arg),
	    bytesNeeded = NONE,
	    convention = Cdecl,
	    ensuresBytesFree = false,
	    mayGC = true,
	    maySwitchThreads = false,
	    modifiesFrontier = true,
	    modifiesStackTop = true,
	    name = "GC_weakNew",
	    return = return}

      val worldSave =
	 T {args = Vector.new2 (gcState, Word32),
	    bytesNeeded = NONE,
	    convention = Cdecl,
	    ensuresBytesFree = false,
	    mayGC = true,
	    maySwitchThreads = false,
	    modifiesFrontier = true,
	    modifiesStackTop = true,
	    name = "GC_saveWorld",
	    return = unit}

      fun size t =
	 vanilla {args = Vector.new1 t,
		  name = "MLton_size",
		  return = Int32}
   end

structure Name =
   struct
      open Prim.Name

      type t = Type.t t

      fun cFunctionRaise (n: t): CFunction.t =
	 let
	    datatype z = datatype CFunction.Convention.t
	    val word = Type.word o WordSize.bits
	    val vanilla = CFunction.vanilla
	    val intC = ("Int", Type.int, IntSize.toString)
	    val realC = ("Real", Type.real, RealSize.toString)
	    val wordC = ("Word", word, WordSize.toString)
	    fun coerce (s1, (fromName, fromType, fromString),
			s2, (toName, toType, toString)) =
	       vanilla {args = Vector.new1 (fromType s1),
			name = concat [fromName, fromString s1,
				       "_to", toName, toString s2],
			return = toType s2}
	    fun coerceX (s1, (fromName, fromType, fromString),
			 s2, (toName, toType, toString)) =
	       vanilla {args = Vector.new1 (fromType s1),
			name = concat [fromName, fromString s1,
				       "_to", toName, toString s2, "X"],
			return = toType s2}
	    fun intBinary (s, name) =
	       let
		  val t = Type.int s
	       in
		  vanilla {args = Vector.new2 (t, t),
			   name = concat ["Int", IntSize.toString s, "_", name],
			   return = t}
	       end
	    fun intCompare (s, name) =
	       vanilla {args = Vector.new2 (Type.int s, Type.int s),
			name = concat ["Int", IntSize.toString s, "_", name],
			return = Type.bool}
	    fun intInfBinary name =
	       CFunction.T {args = Vector.new3 (Type.intInf, Type.intInf,
						Type.defaultWord),
			    bytesNeeded = SOME 2,
			    convention = Cdecl,
			    ensuresBytesFree = false,
			    mayGC = false,
			    maySwitchThreads = false,
			    modifiesFrontier = true,
			    modifiesStackTop = false,
			    name = concat ["IntInf_", name],
			    return = Type.intInf}
	    fun intInfShift name =
	       CFunction.T {args = Vector.new3 (Type.intInf,
						Type.defaultWord,
						Type.defaultWord),
			    bytesNeeded = SOME 2,
			    convention = Cdecl,
			    ensuresBytesFree = false,
			    mayGC = false,
			    maySwitchThreads = false,
			    modifiesFrontier = true,
			    modifiesStackTop = false,
			    name = concat ["IntInf_", name],
			    return = Type.intInf}
	    val intInfToString =
	       CFunction.T {args = Vector.new3 (Type.intInf,
						Type.defaultInt,
						Type.defaultWord),
			    bytesNeeded = SOME 2,
			    convention = Cdecl,
			    ensuresBytesFree = false,
			    mayGC = false,
			    maySwitchThreads = false,
			    modifiesFrontier = true,
			    modifiesStackTop = false,
			    name = "IntInf_toString",
			    return = Type.string}
	    fun intInfUnary name =
	       CFunction.T {args = Vector.new2 (Type.intInf, Type.defaultWord),
			    bytesNeeded = SOME 1,
			    convention = Cdecl,
			    ensuresBytesFree = false,
			    mayGC = false,
			    maySwitchThreads = false,
			    modifiesFrontier = true,
			    modifiesStackTop = false,
			    name = concat ["IntInf_", name],
			    return = Type.intInf}
	    fun wordBinary (s, name) =
	       let
		  val t = word s
	       in
		  vanilla {args = Vector.new2 (t, t),
			   name = concat ["Word", WordSize.toString s,
					  "_", name],
			   return = t}
	       end
	    fun wordCompare (s, name) =
	       vanilla {args = Vector.new2 (word s, word s),
			name = concat ["Word", WordSize.toString s, "_", name],
			return = Type.bool}
	    fun wordShift (s, name) =
	       vanilla {args = Vector.new2 (word s, Type.defaultWord),
			name = concat ["Word", WordSize.toString s, "_", name],
			return = word s}
	    fun wordUnary (s, name) =
	       vanilla {args = Vector.new1 (word s),
			name = concat ["Word", WordSize.toString s, "_", name],
			return = word s}
	 in
	    case n of
	       Int_add s => intBinary (s, "add")
	     | Int_equal s =>
		  let
		     val s = IntSize.roundUpToPrim s
		  in
		     vanilla {args = Vector.new2 (Type.int s, Type.int s),
			      name = concat ["Int", IntSize.toString s,
					     "_equal"],
			      return = Type.bool}
		  end
	     | Int_ge s => intCompare (s, "ge")
	     | Int_gt s => intCompare (s, "gt")
	     | Int_le s => intCompare (s, "le")
	     | Int_lt s => intCompare (s, "lt")
	     | Int_mul s => intBinary (s, "mul")
	     | Int_quot s => intBinary (s, "quot")
	     | Int_rem s => intBinary (s, "rem")
	     | Int_toInt (s1, s2) => coerce (s1, intC, s2, intC)
	     | Int_toReal (s1, s2) => coerce (s1, intC, s2, realC)
	     | Int_toWord (s1, s2) => coerce (s1, intC, s2, wordC)
	     | IntInf_add => intInfBinary "add"
	     | IntInf_andb => intInfBinary "andb"
	     | IntInf_arshift => intInfShift "arshift"
	     | IntInf_compare => 
		  vanilla {args = Vector.new2 (Type.intInf, Type.intInf),
			   name = "IntInf_compare",
			   return = Type.defaultInt}
	     | IntInf_equal =>
		  vanilla {args = Vector.new2 (Type.intInf, Type.intInf),
			   name = "IntInf_equal",
			   return = Type.bool}
	     | IntInf_gcd => intInfBinary "gcd"
	     | IntInf_lshift => intInfShift "lshift"
	     | IntInf_mul => intInfBinary "mul"
	     | IntInf_neg => intInfUnary "neg"
	     | IntInf_notb => intInfUnary "notb"
	     | IntInf_orb => intInfBinary "orb"
	     | IntInf_quot => intInfBinary "quot"
	     | IntInf_rem => intInfBinary "rem"
	     | IntInf_sub => intInfBinary "sub"
	     | IntInf_toString => intInfToString
	     | IntInf_xorb => intInfBinary "xorb"
	     | MLton_bug => CFunction.bug
	     | Thread_returnToC => CFunction.returnToC
	     | Word_add s => wordBinary (s, "add")
	     | Word_andb s => wordBinary (s, "andb")
	     | Word_arshift s => wordShift (s, "arshift")
	     | Word_div s => wordBinary (s, "div")
	     | Word_equal s => wordCompare (s, "equal")
	     | Word_ge s => wordCompare (s, "ge")
	     | Word_gt s => wordCompare (s, "gt")
	     | Word_le s => wordCompare (s, "le")
	     | Word_lshift s => wordShift (s, "lshift")
	     | Word_lt s => wordCompare (s, "lt")
	     | Word_mod s => wordBinary (s, "mod")
	     | Word_mul s => wordBinary (s, "mul")
	     | Word_neg s => wordUnary (s, "neg")
	     | Word_notb s => wordUnary (s, "notb")
	     | Word_orb s => wordBinary (s, "orb")
	     | Word_rol s => wordShift (s, "rol")
	     | Word_ror s => wordShift (s, "ror")
	     | Word_rshift s => wordShift (s, "rshift")
	     | Word_sub s => wordBinary (s, "sub")
	     | Word_toInt (s1, s2) => coerce (s1, wordC, s2, intC)
	     | Word_toIntX (s1, s2) => coerceX (s1, wordC, s2, intC)
	     | Word_toWord (s1, s2) => coerce (s1, wordC, s2, wordC)
	     | Word_toWordX (s1, s2) => coerceX (s1, wordC, s2, wordC)
	     | Word_xorb s => wordBinary (s, "xorb")
	     | _ => raise Fail "cFunctionRaise"
	 end

      fun cFunction n = SOME (cFunctionRaise n) handle _ => NONE

      fun cCodegenImplements n =
	 let
	    datatype z = datatype RealSize.t
	 in
	    case n of
	       FFI_Symbol _ => true
	     | Int_add _ => true
	     | Int_equal _ => true
	     | Int_ge _ => true
	     | Int_gt _ => true
	     | Int_le _ => true
	     | Int_lt _ => true
	     | Int_mul _ => true
	     | Int_neg _ => true
	     | Int_sub _ => true
	     | Int_toInt _ => true
	     | Int_toReal _ => true
	     | Int_toWord _ => true
	     | MLton_eq => true
	     | Real_Math_acos _ => true
	     | Real_Math_asin _ => true
	     | Real_Math_atan _ => true
	     | Real_Math_atan2 _ => true
	     | Real_Math_cos _ => true
	     | Real_Math_exp _ => true
	     | Real_Math_ln _ => true
	     | Real_Math_log10 _ => true
	     | Real_Math_sin _ => true
	     | Real_Math_sqrt _ => true
	     | Real_Math_tan _ => true
	     | Real_add _ => true
	     | Real_div _ => true
	     | Real_equal _ => true
	     | Real_ge _ => true
	     | Real_gt _ => true
	     | Real_ldexp _ => true
	     | Real_le _ => true
	     | Real_lt _ => true
	     | Real_mul _ => true
	     | Real_muladd _ => true
	     | Real_mulsub _ => true
	     | Real_neg _ => true
	     | Real_round _ => true
	     | Real_sub _ => true
	     | Real_toInt _ => true
	     | Real_toReal _ => true
	     | Thread_returnToC => true
	     | Word_add _ => true
	     | Word_andb _ => true
	     | Word_arshift _ => true
	     | Word_div _ => true
	     | Word_equal _ => true
	     | Word_ge _ => true
	     | Word_gt _ => true
	     | Word_le _ => true
	     | Word_lshift _ => true
	     | Word_lt _ => true
	     | Word_mod _ => true
	     | Word_mul _ => true
	     | Word_neg _ => true
	     | Word_notb _ => true
	     | Word_orb _ => true
	     | Word_rol _ => true
	     | Word_ror _ => true
	     | Word_rshift _ => true
	     | Word_sub _ => true
	     | Word_toInt _ => true
	     | Word_toIntX _ => true
	     | Word_toWord _ => true
	     | Word_toWordX _ => true
	     | Word_xorb _ => true
	     | _ => false
	 end

      fun x86CodegenImplements n =
	 let
	    datatype z = datatype IntSize.prim
	    datatype z = datatype RealSize.t
	    datatype z = datatype WordSize.prim
	    fun i32168 s =
	       case IntSize.prim s of
		  I8 => true
		| I16 => true
		| I32 => true
		| I64 => false
	    fun w32168 s =
	       case WordSize.prim s of
		  W8 => true
		| W16 => true
		| W32 => true
		| W64 => false
	 in
	    case n of
	       FFI_Symbol _ => true
	     | Int_add _ => true
	     | Int_addCheck _ => true
	     | Int_equal s => i32168 s
	     | Int_ge s => i32168 s
	     | Int_gt s => i32168 s
	     | Int_le s => i32168 s
	     | Int_lt s => i32168 s
	     | Int_mul s => i32168 s
	     | Int_mulCheck s => i32168 s
	     | Int_neg _ => true
	     | Int_negCheck _ => true
	     | Int_quot s => i32168 s
	     | Int_rem s => i32168 s
	     | Int_sub _ => true
	     | Int_subCheck _ => true
	     | Int_toInt (s1, s2) =>
		  (case (IntSize.prim s1, IntSize.prim s2) of
		      (I32, I32) => true
		    | (I32, I16) => true
		    | (I32, I8) => true
		    | (I16, I32) => true
		    | (I16, I16) => true
		    | (I16, I8) => true
		    | (I8, I32) => true
		    | (I8, I16) => true
		    | _ => false)
	     | Int_toReal (s1, s2) =>
		  (case (IntSize.prim s1, s2) of
		      (I32, R64) => true
		    | (I32, R32) => true
		    | (I16, R64) => true
		    | (I16, R32) => true
		    | (I8, R64) => true
		    | (I8, R32) => true
		    | _ => false)
	      | Int_toWord (s1, s2) =>
		   (case (IntSize.prim s1, WordSize.prim s2) of
		       (I32, W32) => true
		     | (I32, W16) => true
		     | (I32, W8) => true
		     | (I16, W32) => true
		     | (I16, W16) => true
		     | (I16, W8) => true
		     | (I8, W32) => true
		     | (I8, W16) => true
		     | (I8, W8) => true
		     | _ => false)
	      | MLton_eq => true
	      | Real_Math_acos _ => true
	      | Real_Math_asin _ => true
	      | Real_Math_atan _ => true
	      | Real_Math_atan2 _ => true
	      | Real_Math_cos _ => true
	      | Real_Math_exp _ => true
	      | Real_Math_ln _ => true
	      | Real_Math_log10 _ => true
	      | Real_Math_sin _ => true
	      | Real_Math_sqrt _ => true
	      | Real_Math_tan _ => true
	      | Real_abs _ => true
	      | Real_add _ => true
	      | Real_div _ => true
	      | Real_equal _ => true
	      | Real_ge _ => true
	      | Real_gt _ => true
	      | Real_ldexp _ => true
	      | Real_le _ => true
	      | Real_lt _ => true
	      | Real_mul _ => true
	      | Real_muladd _ => true
	      | Real_mulsub _ => true
	      | Real_neg _ => true
	      | Real_qequal _ => true
	      | Real_round _ => true
	      | Real_sub _ => true
	      | Real_toInt (s1, s2) =>
		   (case (s1, IntSize.prim s2) of
		       (R64, I32) => true
		     | (R64, I16) => true
		     | (R64, I8) => true
		     | (R32, I32) => true
		     | (R32, I16) => true
		     | (R32, I8) => true
		     | _ => false)
	      | Real_toReal _ => true
	      | Word_add _ => true
	      | Word_addCheck _ => true
	      | Word_andb _ => true
	      | Word_arshift s => w32168 s
	      | Word_div s => w32168 s
	      | Word_equal s => w32168 s
	      | Word_ge s => w32168 s
	      | Word_gt s => w32168 s
	      | Word_le s => w32168 s
	      | Word_lshift s => w32168 s
	      | Word_lt s => w32168 s
	      | Word_mod s => w32168 s
	      | Word_mul s => w32168 s
	      | Word_mulCheck s => w32168 s
	      | Word_neg _ => true
	      | Word_notb _ => true
	      | Word_orb _ => true
	      | Word_rol s => w32168 s
	      | Word_ror s => w32168 s
	      | Word_rshift s => w32168 s
	      | Word_sub _ => true
	      | Word_toInt (s1, s2) =>
		   (case (WordSize.prim s1, IntSize.prim s2) of
		       (W32, I32) => true
		     | (W32, I16) => true
		     | (W32, I8) => true
		     | (W16, I32) => true
		     | (W16, I16) => true
		     | (W16, I8) => true
		     | (W8, I32) => true
		     | (W8, I16) => true
		     | (W8, I8) => true
		     | _ => false)
	      | Word_toIntX (s1, s2) =>
		   (case (WordSize.prim s1, IntSize.prim s2) of
		       (W32, I32) => true
		     | (W32, I16) => true
		     | (W32, I8) => true
		     | (W16, I32) => true
		     | (W16, I16) => true
		     | (W16, I8) => true
		     | (W8, I32) => true
		     | (W8, I16) => true
		     | (W8, I8) => true
		     | _ => false)
	      | Word_toWord (s1, s2) =>
		   (case (WordSize.prim s1, WordSize.prim s2) of
		       (W32, W32) => true
		     | (W32, W16) => true
		     | (W32, W8) => true
		     | (W16, W32) => true
		     | (W16, W16) => true
		     | (W16, W8) => true
		     | (W8, W32) => true
		     | (W8, W16) => true
		     | (W8, W8) => true
		     | _ => false)
	      | Word_toWordX (s1, s2) =>
		   (case (WordSize.prim s1, WordSize.prim s2) of
		       (W32, W32) => true
		     | (W32, W16) => true
		     | (W32, W8) => true
		     | (W16, W32) => true
		     | (W16, W16) => true
		     | (W16, W8) => true
		     | (W8, W32) => true
		     | (W8, W16) => true
		     | (W8, W8) => true
		     | _ => false)
	      | Word_xorb _ => true
	      | _ => false
	 end

      val x86CodegenImplements: t -> bool =
	 Trace.trace ("x86CodegenImplements", layout, Bool.layout)
	 x86CodegenImplements
   end

datatype z = datatype Operand.t
datatype z = datatype Statement.t
datatype z = datatype Transfer.t

structure Representation = Representation (structure Rssa = Rssa
					   structure Ssa = Ssa)

fun updateCard (addr: Operand.t): Statement.t list =
   let
      val index = Var.newNoname ()
      val indexTy = Type.defaultWord
   in
      [PrimApp {args = (Vector.new2
			(addr,
			 Operand.word
			 (WordX.fromIntInf (IntInf.fromInt
					    (!Control.cardSizeLog2),
					    WordSize.default)))),
		dst = SOME (index, indexTy),
		prim = Prim.wordRshift WordSize.default},
       Move {dst = (Operand.ArrayOffset
		    {base = Operand.Runtime GCField.CardMap,
		     index = (Operand.Cast
			      (Operand.Var {ty = indexTy, var = index},
			       Type.defaultInt)),
		     ty = Type.word Bits.inByte}),
	     src = Operand.word (WordX.one (WordSize.fromBits Bits.inByte))}]
   end

fun arrayUpdate {array, index, elt, ty}: Statement.t list =
   if not (!Control.markCards) orelse not (Type.isPointer ty)
      then
	 [Move {dst = ArrayOffset {base = array, index = index, ty = ty},
		src = elt}]
   else
      let
	 val bytes = Bytes.toIntInf (Type.bytes ty)
	 val shift = IntInf.log2 bytes
	 val _ =
	    if bytes = IntInf.pow (2, shift)
	       then ()
	    else Error.bug "can't handle shift"
	 val shift = Bits.fromInt shift
	 val addr = Var.newNoname ()
	 val addrTy = Type.address ty
	 val addrOp = Operand.Var {ty = addrTy, var = addr}
	 val temp = Var.newNoname ()
	 val tempTy =
	    Type.seq
	    (Vector.new2 (Type.constant (WordX.zero (WordSize.fromBits shift)),
			  Type.word (Bits.- (Bits.inWord, shift))))
	 val tempOp = Operand.Var {ty = tempTy, var = temp}
      in
	 [PrimApp {args = Vector.new2 (Operand.cast (index, Type.defaultWord),
				       Operand.word (WordX.fromIntInf
						     (Bits.toIntInf shift,
						      WordSize.default))),
		   dst = SOME (temp, tempTy),
		   prim = Prim.wordLshift WordSize.default},
	  PrimApp {args = Vector.new2 (Cast (array, addrTy), tempOp),
		   dst = SOME (addr, addrTy),
		   prim = Prim.wordAdd WordSize.default}]
	 @ updateCard addrOp
	 @ [Move {dst = Operand.Offset {base = addrOp,
					offset = Bytes.zero,
					ty = ty},
		  src = elt}]
      end

val word = Type.word o WordSize.bits

fun convert (program as S.Program.T {functions, globals, main, ...})
   : Rssa.Program.t =
   let
      val {conApp, genCase, objectTypes, reff, select, toRtype, tuple} =
	 Representation.compute program
      fun tyconTy (pt: PointerTycon.t): ObjectType.t =
	 Vector.sub (objectTypes, PointerTycon.index pt)
      val {get = varInfo: Var.t -> {ty: S.Type.t},
	   set = setVarInfo, ...} =
	 Property.getSetOnce (Var.plist,
			      Property.initRaise ("varInfo", Var.layout))
      val setVarInfo =
	 Trace.trace2 ("SsaToRssa.setVarInfo",
		       Var.layout, S.Type.layout o #ty, Unit.layout)
	 setVarInfo
      val varType = #ty o varInfo
      fun varOp (x: Var.t): Operand.t =
	 Var {var = x, ty = valOf (toRtype (varType x))}
      val varOp =
	 Trace.trace ("SsaToRssa.varOp", Var.layout, Operand.layout) varOp
      fun varOps xs = Vector.map (xs, varOp)
      val extraBlocks = ref []
      fun newBlock {args, kind,
		    statements: Statement.t vector,
		    transfer: Transfer.t}: Label.t =
	 let
	    val l = Label.newNoname ()
	    val _ = List.push (extraBlocks,
			       Block.T {args = args,
					kind = kind,
					label = l,
					statements = statements,
					transfer = transfer})
	 in
	    l
	 end
      val tagOffset = 0
      fun translateCase ({test: Var.t,
			  cases: S.Cases.t,
			  default: Label.t option})
	 : Statement.t list * Transfer.t =
	 let
	    fun id x = x
	    fun simple (s, cs, cast) =
	       ([],
		Switch
		(Switch.T
		 {cases = (QuickSort.sortVector
			   (cs, fn ((w, _), (w', _)) => WordX.<= (w, w'))),
		  default = default,
		  size = s,
		  test = cast (varOp test)}))
	 in
	    case cases of
	       S.Cases.Con cases =>
		  (case (Vector.length cases, default) of
		      (0, NONE) => ([], Transfer.bug)
		    | _ => 
			 let
			    val (tycon, tys) = S.Type.tyconArgs (varType test)
			 in
			    if Vector.isEmpty tys
			       then
				  let
				     val test = fn () => varOp test
				     val (ss, t, blocks) =
					genCase {cases = cases,
						 default = default,
						 test = test,
						 tycon = tycon}
				     val () =
					extraBlocks := blocks @ !extraBlocks
				  in
				     (ss, t)
				  end
			    else Error.bug "strange type in case"
			 end)
	     | S.Cases.Int (s, cs) =>
		  let
		     val s = WordSize.fromBits (IntSize.bits s)
		     val cs = Vector.map (cs, fn (i, l) =>
					  (WordX.fromIntInf (IntX.toIntInf i, s),
					   l))
		     val t = word s
		  in
		     simple (s, cs, fn z => Operand.Cast (z, t))
		  end
	     | S.Cases.Word (s, cs) =>
		  simple (s, cs, fn z => z)
	 end
      val {get = labelInfo: (Label.t ->
			     {args: (Var.t * S.Type.t) vector,
			      cont: (Handler.t * Label.t) list ref,
			      handler: Label.t option ref}),
	   set = setLabelInfo, ...} =
	 Property.getSetOnce (Label.plist,
			      Property.initRaise ("label info", Label.layout))
      fun eta (l: Label.t, kind: Kind.t): Label.t =
	 let
	    val {args, ...} = labelInfo l
	    val args = Vector.keepAllMap (args, fn (x, t) =>
					  Option.map (toRtype t, fn t =>
						      (Var.new x, t)))
	    val l' = Label.new l
	    val _ = 
	       List.push
	       (extraBlocks,
		Block.T {args = args,
			 kind = kind,
			 label = l',
			 statements = Vector.new0 (),
			 transfer = (Transfer.Goto
				     {dst = l,
				      args = Vector.map (args, fn (var, ty) =>
							 Var {var = var,
							      ty = ty})})})
	 in
	    l'
	 end
      fun labelHandler (l: Label.t): Label.t =
	 let
	    val {handler, ...} = labelInfo l
	 in
	    case !handler of
	       NONE =>
		  let
		     val l' = eta (l, Kind.Handler)
		     val _ = handler := SOME l'
		  in
		     l'
		  end
	     | SOME l => l
	 end
      fun labelCont (l: Label.t, h: Handler.t): Label.t =
	 let
	    val {cont, ...} = labelInfo l
	    datatype z = datatype Handler.t
	 in
	    case List.peek (!cont, fn (h', _) => Handler.equals (h, h')) of
	       SOME (_, l) => l
	     | NONE =>
		  let
		     val l' = eta (l, Kind.Cont {handler = h})
		     val _ = List.push (cont, (h, l'))
		  in
		     l'
		  end
	 end
      val labelCont =
	 Trace.trace2 ("SsaToRssa.labelCont",
		       Label.layout, Handler.layout, Label.layout)
	 labelCont
      fun vos (xs: Var.t vector) =
	 Vector.keepAllMap (xs, fn x =>
			    Option.map (toRtype (varType x), fn _ =>
					varOp x))
      fun translatePrim p =
	 Prim.map (p, fn t =>
		   case toRtype t of
		      NONE => Type.unit
		    | SOME t => t)
      fun translateTransfer (t: S.Transfer.t): Statement.t list * Transfer.t =
	 case t of
	    S.Transfer.Arith {args, overflow, prim, success, ty} =>
	       let
		  val ty = valOf (toRtype ty)
		  val temp = Var.newNoname ()
		  val noOverflow =
		     newBlock
		     {args = Vector.new0 (),
		      kind = Kind.Jump,
		      statements = Vector.new0 (),
		      transfer = (Transfer.Goto
				  {dst = success,
				   args = (Vector.new1
					   (Operand.Var {var = temp,
							 ty = ty}))})}
	       in
		  ([], Transfer.Arith {dst = temp,
				       args = vos args,
				       overflow = overflow,
				       prim = translatePrim prim,
				       success = noOverflow,
				       ty = ty})
	       end
	  | S.Transfer.Bug => ([], Transfer.bug)
	  | S.Transfer.Call {func, args, return} =>
	       let
		  datatype z = datatype S.Return.t
		  val return =
		     case return of
			Dead => Return.Dead
		      | NonTail {cont, handler} =>
			   let
			      datatype z = datatype S.Handler.t
			      val handler =
				 case handler of
				    Caller => Handler.Caller
				  | Dead => Handler.Dead
				  | Handle l => Handler.Handle (labelHandler l)
			   in
			      Return.NonTail {cont = labelCont (cont, handler),
					      handler = handler}
			   end
		      | Tail => Return.Tail
	       in
		  ([], Transfer.Call {func = func,
				      args = vos args,
				      return = return})
	       end
	  | S.Transfer.Case r => translateCase r
	  | S.Transfer.Goto {dst, args} =>
	       ([], Transfer.Goto {dst = dst, args = vos args})
	  | S.Transfer.Raise xs => ([], Transfer.Raise (vos xs))
	  | S.Transfer.Return xs => ([], Transfer.Return (vos xs))
	  | S.Transfer.Runtime {args, prim, return} =>
	       let
		  datatype z = datatype Prim.Name.t
	       in
		  case Prim.name prim of
		     MLton_halt =>
			([], Transfer.CCall {args = vos args,
					     func = CFunction.exit,
					     return = NONE})
		   | Thread_copyCurrent =>
			let
			   val func = CFunction.copyCurrentThread
			   val l =
			      newBlock {args = Vector.new0 (),
					kind = Kind.CReturn {func = func},
					statements = Vector.new0 (),
					transfer =
					(Goto {args = Vector.new0 (),
					       dst = return})}
			in
			   ([],
			    Transfer.CCall
			    {args = (Vector.concat
				     [Vector.new1 Operand.GCState, vos args]),
			     func = func,
			     return = SOME l})
			end
		   | _ => Error.bug (concat
				     ["strange prim in SSA Runtime transfer ",
				      Prim.toString prim])
	       end
      fun translateFormals v =
	 Vector.keepAllMap (v, fn (x, t) =>
			    Option.map (toRtype t, fn t => (x, t)))
      fun bogus (t: Type.t): Operand.t =
	 let
	    val c = Operand.Const
	    datatype z = datatype Type.dest
	 in
	    case Type.dest t of
	       Constant w => c (Const.word w)
	     | Int s => c (Const.int (IntX.zero s))
	     | Pointer _ =>
		  Operand.Cast (Operand.int (IntX.one IntSize.default), t)
	     | Real s => c (Const.real (RealX.zero s))
	     | Sum ts => bogus (Vector.sub (ts, 0))
	     | Word s => c (Const.word (WordX.zero (WordSize.fromBits s)))
	     | _ => Error.bug (concat ["no bogus value of type ",
				       Layout.toString (Type.layout t)])
	 end
      val handlesSignals = 
	 S.Program.hasPrim 
	 (program, fn p => 
	  case Prim.name p of
	     Prim.Name.MLton_installSignalHandler => true
	   | _ => false)
      fun translateStatementsTransfer (statements, ss, transfer) =
	 let
	    fun loop (i, ss, t): Statement.t vector * Transfer.t =
	       if i < 0
		  then (Vector.fromList ss, t)
	       else
		  let
		     val S.Statement.T {exp, ty, var} =
			Vector.sub (statements, i)
		     fun none () = loop (i - 1, ss, t)
		     fun add s = loop (i - 1, s :: ss, t)
		     fun adds ss' = loop (i - 1, ss' @ ss, t)
		     fun split (args, kind,
				ss: Statement.t list,
				make: Label.t -> Statement.t list * Transfer.t) =
			let
			   val l = newBlock {args = args,
					     kind = kind,
					     statements = Vector.fromList ss,
					     transfer = t}
			   val (ss, t) = make l
			in
			   loop (i - 1, ss, t)
			end
		     fun move (oper: Operand.t) =
			add (Bind {isMutable = false,
				   oper = oper,
				   var = valOf var})
		  in
		     case exp of
			S.Exp.ConApp {con, args} =>
			   adds (conApp
				 {args = args,
				  con = con,
				  dst = fn () => valOf var,
				  oper = varOp,
				  ty = fn () => valOf (toRtype ty)})
		      | S.Exp.Const c =>
			   let
			      datatype z = datatype Const.t
			      val c =
				 case c of
				    Int i =>
				       Int (IntX.make (IntX.toIntInf i,
						       IntSize.roundUpToPrim
						       (IntX.size i)))
				  | Word w =>
				       Word (WordX.fromIntInf
					     (WordX.toIntInf w,
					      WordSize.roundUpToPrim
					      (WordX.size w)))
				  | _ => c
			   in
			      move (Operand.Const c)
			   end
		      | S.Exp.PrimApp {prim, targs, args, ...} =>
			   let
			      val prim = translatePrim prim
			      fun a i = Vector.sub (args, i)
			      fun cast () =
				 move (Operand.cast (varOp (a 0),
						     valOf (toRtype ty)))
			      fun targ () = toRtype (Vector.sub (targs, 0))
			      fun ifTargIsPointer (yes, no) =
				 case targ () of
				    NONE => no ()
				  | SOME t =>
				       if Type.isPointer t
					  then yes t
				       else no ()
			      fun arrayOrVectorLength () =
				 move (Operand.Offset
				       {base = varOp (a 0),
					offset = Runtime.arrayLengthOffset,
					ty = Type.defaultInt})
			      fun arrayOffset (ty: Type.t): Operand.t =
				 ArrayOffset {base = varOp (a 0),
					      index = varOp (a 1),
					      ty = ty}
			      fun sub (ty: Type.t) = move (arrayOffset ty)
			      fun dst () =
				 case var of
				    SOME x =>
				       Option.map (toRtype (varType x), fn t =>
						   (x, t))
				  | NONE => NONE
			      fun primApp prim =
				 add (PrimApp {dst = dst (),
					       prim = prim,
					       args = varOps args})
			      datatype z = datatype Prim.Name.t
			      fun bumpCanHandle n =
				 let
				    val canHandle =
				       Operand.Runtime GCField.CanHandle
				    val res = Var.newNoname ()
				    val resTy = Operand.ty canHandle
				 in
				    [Statement.PrimApp
				     {args = (Vector.new2
					      (canHandle,
					       (Operand.word
						(WordX.fromIntInf
						 (IntInf.fromInt n,
						  WordSize.default))))),
				      dst = SOME (res, resTy),
				      prim = Prim.wordAdd WordSize.default},
				     Statement.Move
				     {dst = canHandle,
				      src = Operand.Var {var = res,
							 ty = resTy}}]
				 end
			      fun ccallGen
				 {args: Operand.t vector,
				  func: CFunction.t,
				  prefix: Transfer.t -> (Statement.t list
							 * Transfer.t)} =
				 let
				    val formals =
				       case dst () of
					  NONE => Vector.new0 ()
					| SOME (x, t) => Vector.new1 (x, t)
				 in
				    split
				    (formals, Kind.CReturn {func = func}, ss,
				     fn l =>
				     let
					val t =
					   Transfer.CCall {args = args,
							   func = func,
							   return = SOME l}
					fun isolate () =
					   (* Put the CCall in its own block
					    * so that limit check insertion
					    * can put a limit check just before
					    * it.
					    *)
					   let
					      val l =
						 newBlock
						 {args = Vector.new0 (),
						  kind = Kind.Jump,
						  statements = Vector.new0 (),
						  transfer = t}
					   in
					      prefix
					      (Transfer.Goto
					       {args = Vector.new0 (),
						dst = l})
					   end
				     in
					case CFunction.bytesNeeded func of
					   NONE => prefix t
					 | SOME i =>
					      Operand.caseBytes
					      (Vector.sub (args, i),
					       {big = fn _ => isolate (),
						small = fn _ => prefix t})
				     end)
				 end
			      fun ccall {args, func} =
				 ccallGen {args = args,
					   func = func,
					   prefix = fn t => ([], t)}
			      fun simpleCCall (f: CFunction.t) =
				 ccall {args = vos args,
					func = f}
			      fun array (numElts: Operand.t) =
				 let
				    val result = valOf (toRtype ty)
				    val pt =
				       case Type.dest result of
					  Type.Pointer pt => PointerTycon pt
					| _ => Error.bug "strange array"
				    val args =
				       Vector.new4 (Operand.GCState,
						    Operand.EnsuresBytesFree,
						    numElts,
						    pt)
				    val func =
				       CFunction.gcArrayAllocate
				       {return = result}
				 in
				    ccall {args = args, func = func}
				 end
		     fun pointerGet ty =
			move (ArrayOffset {base = varOp (a 0),
					   index = varOp (a 1),
					   ty = ty})
		     fun pointerSet ty =
			add (Move {dst = ArrayOffset {base = varOp (a 0),
						      index = varOp (a 1),
						      ty = ty},
				   src = varOp (a 2)})
		     fun refAssign (ty, src) =
		        let
			   val addr = varOp (a 0)
			   val ss =
			      Move {dst = Operand.Offset {base = addr,
							  offset = Bytes.zero,
							  ty = ty},
				    src = src}
			      :: ss
			   val ss =
			      if !Control.markCards andalso Type.isPointer ty
				 then updateCard addr @ ss
			      else ss
			in
			   loop (i - 1, ss, t)
			end
		     fun nativeOrC (p: Prim.t) =
			let
			   val n = Prim.name p
			in
			   if if !Control.Native.native
				 then Name.x86CodegenImplements n
			      else Name.cCodegenImplements n
			      then primApp p
			   else (case Name.cFunction n of
				    NONE =>
				       Error.bug (concat ["unimplemented prim:",
							  Name.toString n])
				  | SOME f => simpleCCall f)
			end
		     val arrayUpdate =
			fn ty =>
			loop (i - 1,
			      arrayUpdate {array = varOp (a 0),
					   index = varOp (a 1),
					   elt = varOp (a 2),
					   ty = ty}
			      @ ss, t)
		     datatype z = datatype Prim.Name.t
			   in
			      case Prim.name prim of
				 Array_array => array (varOp (a 0))
			       | Array_length => arrayOrVectorLength ()
			       | Array_sub =>
				    (case targ () of
					NONE => none ()
				      | SOME t => sub t)
			       | Array_toVector =>
				    let
				       val array = varOp (a 0)
				       val vecTy = valOf (toRtype ty)
				       val pt =
					  case Type.dest vecTy of
					     Type.Pointer pt => pt
					   | _ => Error.bug "strange Array_toVector"
				    in
				       loop
				       (i - 1,
					Move
					{dst = (Offset
						{base = array,
						 offset = Runtime.headerOffset,
						 ty = Type.defaultWord}),
					 src = PointerTycon pt}
					:: Bind {isMutable = false,
						 oper = (Operand.Cast
							 (array, vecTy)),
						 var = valOf var}
					:: ss,
					t)
				    end
			       | Array_update =>
				    (case targ () of
					NONE => none ()
				      | SOME ty => arrayUpdate ty)
			       | FFI f => simpleCCall f
			       | GC_collect =>
				    ccall
				    {args = (Vector.new5
					     (Operand.GCState,
					      Operand.int (IntX.zero
							   IntSize.default),
					      Operand.bool true,
					      Operand.File,
					      Operand.Line)),
				     func = (CFunction.gc
					     {maySwitchThreads = false})}
			       | GC_pack =>
				    ccall {args = Vector.new1 Operand.GCState,
					   func = CFunction.pack}
			       | GC_unpack =>
				    ccall {args = Vector.new1 Operand.GCState,
					   func = CFunction.unpack}
			       | Int_equal s =>
				    nativeOrC (Prim.intEqual
					       (IntSize.roundUpToPrim s))
			       | Int_toInt (s1, s2) =>
				    let
				       val s1 = IntSize.roundUpToPrim s1
				       val s2 = IntSize.roundUpToPrim s2
				    in
				       if IntSize.equals (s1, s2)
					  then cast ()
				       else nativeOrC (Prim.intToInt (s1, s2))
				    end
			       | IntInf_toVector => cast ()
			       | IntInf_toWord => cast ()
			       | MLton_bogus =>
				    (case toRtype ty of
					NONE => none ()
				      | SOME t => move (bogus t))
			       | MLton_eq =>
				    (case targ () of
					NONE => move (Operand.bool true)
				      | SOME _ => primApp prim)
			       | MLton_installSignalHandler => none ()
			       | MLton_size =>
				    simpleCCall
				    (CFunction.size (Operand.ty (varOp (a 0))))
			       | MLton_touch => none ()
			       | Pointer_getInt s => pointerGet (Type.int s)
			       | Pointer_getPointer =>
				    (case targ () of
					NONE => Error.bug "getPointer"
				      | SOME t => pointerGet t)
			       | Pointer_getReal s => pointerGet (Type.real s)
			       | Pointer_getWord s => pointerGet (word s)
			       | Pointer_setInt s => pointerSet (Type.int s)
			       | Pointer_setPointer =>
				    (case targ () of
					NONE => Error.bug "setPointer"
				      | SOME t => pointerSet t)
			       | Pointer_setReal s => pointerSet (Type.real s)
			       | Pointer_setWord s => pointerSet (word s)
			       | Ref_assign =>
				    (case targ () of
					NONE => none ()
				      | SOME ty => refAssign (ty, varOp (a 1)))
			       | Ref_deref =>
				    (case targ () of
					NONE => none ()
				      | SOME ty =>
					   move (Offset {base = varOp (a 0),
							 offset = Bytes.zero,
							 ty = ty}))
			       | Ref_ref =>
				    adds (reff {arg = fn () => varOp (a 0),
						dst = valOf var,
						ty = Vector.sub (targs, 0)})
			       | Thread_atomicBegin =>
				    (* gcState.canHandle++;
				     * if (gcState.signalIsPending)
				     *   gcState.limit = gcState.limitPlusSlop - LIMIT_SLOP;
				     *)
				    split
				    (Vector.new0 (), Kind.Jump, ss,
				     fn continue =>
				     let
					datatype z = datatype GCField.t
					val tmp = Var.newNoname ()
					val size = WordSize.pointer ()
					val ty = Type.cPointer ()
					val statements =
					   Vector.new2
					   (Statement.PrimApp
					    {args = (Vector.new2
						     (Operand.Runtime LimitPlusSlop,
						      Operand.word
						      (WordX.fromIntInf
						       (IntInf.fromInt
							(Bytes.toInt Runtime.limitSlop),
							size)))),
					     dst = SOME (tmp, ty),
					     prim = Prim.wordSub size},
					    Statement.Move
					    {dst = Operand.Runtime Limit,
					     src = Operand.Var {var = tmp,
								ty = ty}})
					val signalIsPending =
					   newBlock
					   {args = Vector.new0 (),
					    kind = Kind.Jump,
					    statements = statements,
					    transfer = (Transfer.Goto
							{args = Vector.new0 (),
							 dst = continue})}
				     in
					(bumpCanHandle 1,
					 if handlesSignals 
					    then
					       Transfer.ifBool
					       (Operand.Runtime SignalIsPending,
						{falsee = continue,
						 truee = signalIsPending})
					 else 
					    Transfer.Goto {args = Vector.new0 (),
							   dst = continue})
				     end)
			       | Thread_atomicEnd =>
				    (* gcState.canHandle--;
				     * if (gcState.signalIsPending
				     *     and 0 == gcState.canHandle)
				     *   gc;
				     *)
				    split
				    (Vector.new0 (), Kind.Jump, ss,
				     fn continue =>
				     let
					datatype z = datatype GCField.t
					val func =
					   CFunction.gc {maySwitchThreads = true}
					val returnFromHandler = 
					   newBlock
					   {args = Vector.new0 (),
					    kind = Kind.CReturn {func = func},
					    statements = Vector.new0 (),
					    transfer =
					    Goto {args = Vector.new0 (),
						  dst = continue}}
					val args = 
					   Vector.new5
					   (Operand.GCState,
					    Operand.int (IntX.zero
							 IntSize.default),
					    Operand.bool false,
					    Operand.File,
					    Operand.Line)
					val switchToHandler =
					   newBlock
					   {args = Vector.new0 (),
					    kind = Kind.Jump,
					    statements = Vector.new0 (),
					    transfer =
					    Transfer.CCall
					    {args = args,
					     func = func,
					     return = SOME returnFromHandler}}
					val testCanHandle =
					   newBlock
					   {args = Vector.new0 (),
					    kind = Kind.Jump,
					    statements = Vector.new0 (),
					    transfer =
					    Transfer.ifZero
					    (Operand.Runtime CanHandle,
					     {falsee = continue,
					      truee = switchToHandler})}
				     in
					(bumpCanHandle ~1,
					 if handlesSignals 
					    then 
					       Transfer.ifBool
					       (Operand.Runtime SignalIsPending,
						{falsee = continue,
						 truee = testCanHandle})
					 else 
					    Transfer.Goto {args = Vector.new0 (),
							   dst = continue})
				     end)
			       | Thread_canHandle =>
				    move (Operand.Cast
					  (Operand.Runtime GCField.CanHandle,
					   Type.defaultInt))
			       | Thread_copy =>
				    ccall {args = (Vector.concat
						   [Vector.new1 Operand.GCState,
						    vos args]),
					   func = CFunction.copyThread}
			       | Thread_switchTo =>
				    ccall {args = (Vector.new2
						   (varOp (a 0),
						    Operand.EnsuresBytesFree)),
					   func = CFunction.threadSwitchTo}
			       | Vector_length => arrayOrVectorLength ()
			       | Vector_sub =>
				    (case targ () of
					NONE => none ()
				      | SOME t => sub t)
			       | Weak_canGet =>
				    ifTargIsPointer
				    (fn _ => (simpleCCall
					       (CFunction.weakCanGet
						(Operand.ty (varOp (a 0))))),
				     fn () => move (Operand.bool false))
			       | Weak_get =>
				    ifTargIsPointer
				    (fn t => (simpleCCall
					      (CFunction.weakGet
					       {arg = Operand.ty (varOp (a 0)),
						return = t})),
				     none)
			       | Weak_new =>
				    ifTargIsPointer
				    (fn t =>
				     let
					val result = valOf (toRtype ty)
					val header =
					   Operand.PointerTycon
					   (case Type.dest result of
					       Type.Pointer pt => pt
					     | _ => Error.bug "Weak_new")
					val func =
					   CFunction.weakNew {arg = t,
							      return = result}
				     in
					ccall {args = (Vector.concat
						       [Vector.new2
							(Operand.GCState,
							 header),
							vos args]),
					       func = func}
				     end,
				     none)
			       | Word_equal s =>
				    nativeOrC (Prim.wordEqual
					       (WordSize.roundUpToPrim s))
			       | Word_toIntInf => cast ()
			       | Word_toWord (s1, s2) =>
				    let
				       val s1 = WordSize.roundUpToPrim s1
				       val s2 = WordSize.roundUpToPrim s2
				    in
				       if WordSize.equals (s1, s2)
					  then cast ()
				       else nativeOrC (Prim.wordToWord (s1, s2))
				    end
			       | WordVector_toIntInf => cast ()
			       | Word8Array_subWord => sub Type.defaultWord
			       | Word8Array_updateWord =>
				    arrayUpdate Type.defaultWord
			       | Word8Vector_subWord => sub Type.defaultWord
			       | World_save =>
				    ccall {args = (Vector.new2
						   (Operand.GCState,
						    Vector.sub (vos args, 0))),
					   func = CFunction.worldSave}
			       | _ => nativeOrC prim
			   end
		      | S.Exp.Profile e => add (Statement.Profile e)
		      | S.Exp.Select {tuple, offset} =>
			   adds (select {dst = fn () => valOf var,
					 offset = offset,
					 tuple = fn () => varOp tuple,
					 tupleTy = varType tuple})
		      | S.Exp.Tuple ys =>
			   if 0 = Vector.length ys
			      then none ()
			   else adds (tuple {components = ys,
					     dst = (valOf var, ty),
					     oper = varOp})
		      | S.Exp.Var y =>
			   (case toRtype ty of
			       NONE => none ()
			     | SOME _ => move (varOp y))
		  end
	 in
	    loop (Vector.length statements - 1, ss, transfer)
	 end
      fun translateBlock (S.Block.T {label, args, statements, transfer}) = 
	 let
	    val (ss, t) = translateTransfer transfer
	    val (ss, t) = translateStatementsTransfer (statements, ss, t)
	 in
	    Block.T {args = translateFormals args,
		     kind = Kind.Jump,
		     label = label,
		     statements = ss,
		     transfer = t}
	 end
      fun translateFunction (f: S.Function.t): Function.t =
	 let
	    val _ =
	       S.Function.foreachVar (f, fn (x, t) => setVarInfo (x, {ty = t}))
	    val {args, blocks, name, raises, returns, start, ...} =
	       S.Function.dest f
	    val _ =
	       Vector.foreach
	       (blocks, fn S.Block.T {label, args, ...} =>
		setLabelInfo (label, {args = args,
				      cont = ref [],
				      handler = ref NONE}))
	    val blocks = Vector.map (blocks, translateBlock)
	    val blocks = Vector.concat [Vector.fromList (!extraBlocks), blocks]
	    val _ = extraBlocks := []
	    fun transTypes (ts : S.Type.t vector option)
	       : Type.t vector option =
	       Option.map (ts, fn ts => Vector.keepAllMap (ts, toRtype))
	 in
	    Function.new {args = translateFormals args,
			  blocks = blocks,
			  name = name,
			  raises = transTypes raises,
			  returns = transTypes returns,
			  start = start}
	 end
      val main =
	  let
	     val start = Label.newNoname ()
	     val bug = Label.newNoname ()
	  in
	     translateFunction
	     (S.Function.profile
	      (S.Function.new
	       {args = Vector.new0 (),
		blocks = (Vector.new2
			  (S.Block.T
			   {label = start,
			    args = Vector.new0 (),
			    statements = globals,
			    transfer = (S.Transfer.Call
					{args = Vector.new0 (),
					 func = main,
					 return =
					 S.Return.NonTail
					 {cont = bug,
					  handler = S.Handler.Dead}})},
			   S.Block.T
			   {label = bug,
			    args = Vector.new0 (),
			    statements = Vector.new0 (),
			    transfer = S.Transfer.Bug})),
		name = Func.newNoname (),
		raises = NONE,
		returns = NONE,
		start = start},
	       S.SourceInfo.main))
	  end
      val functions = List.revMap (functions, translateFunction)
      val p = Program.T {functions = functions,
			 handlesSignals = handlesSignals,
			 main = main,
			 objectTypes = objectTypes}
      val _ = Program.clear p
   in
      p
   end

end
