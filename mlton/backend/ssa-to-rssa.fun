(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
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
   structure Base = Base
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
	    name = "GC_copyCurrentThread",
	    readsStackTop = true,
	    return = unit,
	    writesStackTop = true}

      val copyThread =
	 T {args = Vector.new2 (gcState, Type.thread),
	    bytesNeeded = NONE,
	    convention = Cdecl,
	    ensuresBytesFree = false,
	    mayGC = true,
	    maySwitchThreads = false,
	    modifiesFrontier = true,
	    name = "GC_copyThread",
	    readsStackTop = true,
	    return = Type.thread,
	    writesStackTop = true}

      val exit =
	 T {args = Vector.new1 Word32,
	    bytesNeeded = NONE,
	    convention = Cdecl,
	    ensuresBytesFree = false,
	    mayGC = false,
	    maySwitchThreads = false,
	    modifiesFrontier = true,
	    name = "MLton_exit",
	    readsStackTop = true,
	    return = unit,
	    writesStackTop = true}

      fun gcArrayAllocate {return} =
	 T {args = Vector.new4 (gcState, Word32, Word32, Word32),
	    bytesNeeded = NONE,
	    convention = Cdecl,
	    ensuresBytesFree = true,
	    mayGC = true,
	    maySwitchThreads = false,
	    modifiesFrontier = true,
	    name = "GC_arrayAllocate",
	    readsStackTop = true,
	    return = return,
	    writesStackTop = true}

      local
	 fun make name =
	    T {args = Vector.new1 gcState,
	       bytesNeeded = NONE,
	       convention = Cdecl,
	       ensuresBytesFree = false,
	       mayGC = true,
	       maySwitchThreads = false,
	       modifiesFrontier = true,
	       name = name,
	       readsStackTop = true,
	       return = unit,
	       writesStackTop = true}
      in
	 val pack = make "GC_pack"
	 val unpack = make "GC_unpack"
      end

      val returnToC =
	 T {args = Vector.new0 (),
	    bytesNeeded = NONE,
	    convention = Cdecl,
	    ensuresBytesFree = false,
	    mayGC = true,
	    maySwitchThreads = true,
	    modifiesFrontier = true,
	    name = "Thread_returnToC",
	    readsStackTop = true,
	    return = unit,
	    writesStackTop = true}

      val threadSwitchTo =
	 T {args = Vector.new2 (Type.thread, Word32),
	    bytesNeeded = NONE,
	    convention = Cdecl,
	    ensuresBytesFree = true,
	    mayGC = true,
	    maySwitchThreads = true,
	    modifiesFrontier = true,
	    name = "Thread_switchTo",
	    readsStackTop = true,
	    return = unit,
	    writesStackTop = true}

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
	    name = "GC_weakNew",
	    readsStackTop = true,
	    return = return,
	    writesStackTop = true}

      val worldSave =
	 T {args = Vector.new2 (gcState, Word32),
	    bytesNeeded = NONE,
	    convention = Cdecl,
	    ensuresBytesFree = false,
	    mayGC = true,
	    maySwitchThreads = false,
	    modifiesFrontier = true,
	    name = "GC_saveWorld",
	    readsStackTop = true,
	    return = unit,
	    writesStackTop = true}

      fun share t =
	 vanilla {args = Vector.new1 t,
		  name = "MLton_share",
		  return = unit}

      fun size t =
	 vanilla {args = Vector.new1 t,
		  name = "MLton_size",
		  return = Word32}
   end

structure Name =
   struct
      open Prim.Name

      type t = Type.t t

      fun cFunctionRaise (n: t): CFunction.t =
	 let
	    datatype z = datatype CFunction.Convention.t
	    val name = toString n
	    val word = Type.word o WordSize.bits
	    val vanilla = CFunction.vanilla
	    fun coerce (t1, t2) =
	       vanilla {args = Vector.new1 t1,
			name = name,
			return = t2}
	    fun intInfBinary () =
	       CFunction.T {args = Vector.new3 (Type.intInf, Type.intInf,
						Type.defaultWord),
			    bytesNeeded = SOME 2,
			    convention = Cdecl,
			    ensuresBytesFree = false,
			    mayGC = false,
			    maySwitchThreads = false,
			    modifiesFrontier = true,
			    name = name,
			    readsStackTop = false,
			    return = Type.intInf,
			    writesStackTop = false}
	    fun intInfShift () =
	       CFunction.T {args = Vector.new3 (Type.intInf,
						Type.defaultWord,
						Type.defaultWord),
			    bytesNeeded = SOME 2,
			    convention = Cdecl,
			    ensuresBytesFree = false,
			    mayGC = false,
			    maySwitchThreads = false,
			    modifiesFrontier = true,
			    name = name,
			    readsStackTop = false,
			    return = Type.intInf,
			    writesStackTop = false}
	    fun intInfToString () =
	       CFunction.T {args = Vector.new3 (Type.intInf,
						Type.defaultWord,
						Type.defaultWord),
			    bytesNeeded = SOME 2,
			    convention = Cdecl,
			    ensuresBytesFree = false,
			    mayGC = false,
			    maySwitchThreads = false,
			    modifiesFrontier = true,
			    name = name,
			    readsStackTop = false,
			    return = Type.string,
			    writesStackTop = false}
	    fun intInfUnary () =
	       CFunction.T {args = Vector.new2 (Type.intInf, Type.defaultWord),
			    bytesNeeded = SOME 1,
			    convention = Cdecl,
			    ensuresBytesFree = false,
			    mayGC = false,
			    maySwitchThreads = false,
			    modifiesFrontier = true,
			    name = name,
			    readsStackTop = false,
			    return = Type.intInf,
			    writesStackTop = false}
	    fun wordBinary s =
	       let
		  val t = word s
	       in
		  vanilla {args = Vector.new2 (t, t),
			   name = name,
			   return = t}
	       end
	    fun wordCompare s =
	       vanilla {args = Vector.new2 (word s, word s),
			name = name,
			return = Type.bool}
	    fun wordShift s =
	       vanilla {args = Vector.new2 (word s, Type.defaultWord),
			name = name,
			return = word s}
	    fun wordUnary s =
	       vanilla {args = Vector.new1 (word s),
			name = name,
			return = word s}
	 in
	    case n of
	       IntInf_add => intInfBinary ()
	     | IntInf_andb => intInfBinary ()
	     | IntInf_arshift => intInfShift ()
	     | IntInf_compare => 
		  vanilla {args = Vector.new2 (Type.intInf, Type.intInf),
			   name = name,
			   return = Type.defaultWord}
	     | IntInf_equal =>
		  vanilla {args = Vector.new2 (Type.intInf, Type.intInf),
			   name = name,
			   return = Type.bool}
	     | IntInf_gcd => intInfBinary ()
	     | IntInf_lshift => intInfShift ()
	     | IntInf_mul => intInfBinary ()
	     | IntInf_neg => intInfUnary ()
	     | IntInf_notb => intInfUnary ()
	     | IntInf_orb => intInfBinary ()
	     | IntInf_quot => intInfBinary ()
	     | IntInf_rem => intInfBinary ()
	     | IntInf_sub => intInfBinary ()
	     | IntInf_toString => intInfToString ()
	     | IntInf_xorb => intInfBinary ()
	     | MLton_bug => CFunction.bug
	     | Thread_returnToC => CFunction.returnToC
	     | Word_add s => wordBinary s
	     | Word_andb s => wordBinary s
	     | Word_equal s => wordCompare s
	     | Word_ge (s, _) => wordCompare s
	     | Word_gt (s, _) => wordCompare s
	     | Word_le (s, _) => wordCompare s
	     | Word_lshift s => wordShift s
	     | Word_lt (s, _) => wordCompare s
	     | Word_mul (s, _) => wordBinary s
	     | Word_neg s => wordUnary s
	     | Word_notb s => wordUnary s
	     | Word_orb s => wordBinary s
	     | Word_quot (s, _) => wordBinary s
	     | Word_rem (s, _) => wordBinary s
	     | Word_rol s => wordShift s
	     | Word_ror s => wordShift s
	     | Word_rshift (s, _) => wordShift s
	     | Word_sub s => wordBinary s
	     | Word_toReal (s1, s2, _) =>
		  coerce (Type.word (WordSize.bits s1), Type.real s2)
	     | Word_toWord (s1, s2, _) =>
		  coerce (Type.word (WordSize.bits s1),
			  Type.word (WordSize.bits s2))
	     | Word_xorb s => wordBinary s
	     | _ => raise Fail "cFunctionRaise"
	 end

      fun cFunction n = SOME (cFunctionRaise n) handle _ => NONE
   end

datatype z = datatype Operand.t
datatype z = datatype Statement.t
datatype z = datatype Transfer.t

structure PackedRepresentation = PackedRepresentation (structure Rssa = Rssa
						       structure Ssa = Ssa)

structure Type =
   struct
      open Type
	 
      fun scale (ty: t): Scale.t =
	 case Scale.fromInt (Bytes.toInt (bytes ty)) of
	    NONE => Error.bug "Type.scale"
	  | SOME s => s
   end

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
		prim = Prim.wordRshift (WordSize.default,
					{signed = false})},
       Move {dst = (ArrayOffset
		    {base = Runtime GCField.CardMap,
		     index = Var {ty = indexTy, var = index},
		     offset = Bytes.zero,
		     scale = Scale.One,
		     ty = Type.word Bits.inByte}),
	     src = Operand.word (WordX.one (WordSize.fromBits Bits.inByte))}]
   end

fun convertConst (c: Const.t): Const.t =
   let
      datatype z = datatype Const.t
   in
      case c of
	 Word w => Word (WordX.resize (w, WordSize.roundUpToPrim (WordX.size w)))
       | _ => c
   end

fun convert (program as S.Program.T {functions, globals, main, ...},
	     {codegenImplementsPrim}): Rssa.Program.t =
   let
      val {diagnostic, genCase, object, objectTypes, select, toRtype, update} =
	 PackedRepresentation.compute program
      val objectTypes = Vector.concat [ObjectType.basic, objectTypes]
      val () =
	 Vector.foreachi
	 (objectTypes, fn (i, (pt, _)) => PointerTycon.setIndex (pt, i))
      val objectTypes = Vector.map (objectTypes, #2)
      val () = diagnostic ()
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
      fun translateCase ({test: Var.t,
			  cases: S.Cases.t,
			  default: Label.t option})
	 : Statement.t list * Transfer.t =
	 case cases of
	    S.Cases.Con cases =>
	       (case (Vector.length cases, default) of
		   (0, NONE) => ([], Transfer.bug)
		 | _ =>
		      (case S.Type.dest (varType test) of
			  S.Type.Datatype tycon =>
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
			| _ => Error.bug "strange type in case"))
	  | S.Cases.Word (s, cs) =>
	       ([],
		Switch
		(Switch.T
		 {cases = (QuickSort.sortVector
			   (cs, fn ((w, _), (w', _)) =>
			    WordX.le (w, w', {signed = false}))),
		  default = default,
		  size = s,
		  test = varOp test}))
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
					   (Var {var = temp, ty = ty}))})}
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
			    {args = Vector.concat [Vector.new1 GCState,
						   vos args],
			     func = func,
			     return = SOME l})
			end
		   | _ => Error.bug (concat
				     ["strange prim in SSA Runtime transfer ",
				      Prim.toString prim])
	       end
      val translateTransfer =
	 Trace.trace ("SsaToRssa.translateTransfer",
		      S.Transfer.layout,
		      Layout.tuple2 (List.layout Statement.layout,
				     Transfer.layout))
	 translateTransfer
      fun translateFormals v =
	 Vector.keepAllMap (v, fn (x, t) =>
			    Option.map (toRtype t, fn t => (x, t)))
      fun bogus (t: Type.t): Operand.t =
	 case Type.deReal t of
	    NONE => Operand.cast (Operand.word (Type.bogusWord t), t)
	  | SOME s => Operand.Const (Const.real (RealX.zero s))
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
		     fun none () = loop (i - 1, ss, t)
		     fun add s = loop (i - 1, s :: ss, t)
		     fun adds ss' = loop (i - 1, ss' @ ss, t)
		     val s = Vector.sub (statements, i)
		  in
		     case s of
			S.Statement.Profile e => add (Statement.Profile e)
		      | S.Statement.Updates (base, us) =>
			   let
			      val baseOp = Base.map (base, varOp)
			      val baseTy = varType (Base.object base)
			      val ({isPointerUpdate}, ss) =
				 Vector.foldr
				 (us, ({isPointerUpdate = false}, []),
				  fn ({offset, value},
				      z as ({isPointerUpdate}, ac)) =>
				  case toRtype (varType value) of
				     NONE => z
				   | SOME t => 
					({isPointerUpdate =
					  (isPointerUpdate
					   orelse Type.isPointer t)},
					 update {base = baseOp,
						 baseTy = baseTy,
						 offset = offset,
						 value = varOp value} @ ac))
			      val ss =
				 if !Control.markCards
				    andalso isPointerUpdate
				    then updateCard (Base.object baseOp) @ ss
				 else ss
			   in
			      adds ss
			   end
		      | S.Statement.Bind {exp, ty, var} =>
	          let
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
		     fun maybeMove (f: Type.t -> Operand.t) =
			case toRtype ty of
			   NONE => none ()
			 | SOME ty =>
			      add (Bind {dst = (valOf var, ty),
					 isMutable = false,
					 src = f ty})
		     fun move (src: Operand.t) = maybeMove (fn _ => src)
		  in
		     case exp of
			S.Exp.Const c => move (Const (convertConst c))
		      | S.Exp.Inject {variant, ...} =>
			   if isSome (toRtype ty)
			      then move (varOp variant)
			   else none ()
		      | S.Exp.Object {args, con} =>
			   (case toRtype ty of
			       NONE => none ()
			     | SOME dstTy => 
				  adds (object {args = args,
						con = con,
						dst = (valOf var, dstTy),
						objectTy = ty,
						oper = varOp}))
		      | S.Exp.PrimApp {args, prim} =>
			   let
			      val prim = translatePrim prim
			      fun arg i = Vector.sub (args, i)
			      fun a i = varOp (arg i)
			      fun cast () =
				 move (Operand.cast (a 0, valOf (toRtype ty)))
			      fun ifIsWeakPointer (ty: S.Type.t, yes, no) =
				 case S.Type.dest ty of
				    S.Type.Weak ty =>
				       (case toRtype ty of
					   NONE => no ()
					 | SOME t =>
					      if Type.isPointer t
						 then yes t
					      else no ())
				  | _ => Error.bug "ifIsWeakPointer"
			      fun arrayOrVectorLength () =
				 move (Offset
				       {base = a 0,
					offset = Runtime.arrayLengthOffset,
					ty = Type.defaultWord})
			      fun subWord () =
				 move (ArrayOffset {base = a 0,
						    index = a 1,
						    offset = Bytes.zero,
						    scale = Type.scale Type.defaultWord,
						    ty = Type.defaultWord})
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
				    val canHandle = Runtime GCField.CanHandle
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
				      src = Var {ty = resTy, var = res}}]
				 end
			      fun ccall {args: Operand.t vector,
					 func: CFunction.t} =
				 let
				    val formals =
				       case dst () of
					  NONE => Vector.new0 ()
					| SOME (x, t) => Vector.new1 (x, t)
				 in
				    split
				    (formals, Kind.CReturn {func = func}, ss,
				     fn l =>
				     ([],
				      Transfer.CCall {args = args,
						      func = func,
						      return = SOME l}))
				 end
			      fun simpleCCall (f: CFunction.t) =
				 ccall {args = vos args,
					func = f}
			      fun array (numElts: Operand.t) =
				 let
				    val result = valOf (toRtype ty)
				    val pt =
				       case Type.dePointer result of
					  NONE => Error.bug "strange array"
					| SOME pt => PointerTycon pt
				    val args =
				       Vector.new4 (GCState,
						    EnsuresBytesFree,
						    numElts,
						    pt)
				    val func =
				       CFunction.gcArrayAllocate
				       {return = result}
				 in
				    ccall {args = args, func = func}
				 end
		     fun pointerGet () =
			maybeMove (fn ty =>
				   ArrayOffset {base = a 0,
						index = a 1,
						offset = Bytes.zero,
						scale = Type.scale ty,
						ty = ty})
		     fun pointerSet () =
			let
			   val src = a 2
			   val ty = Operand.ty src
			in
			   add (Move {dst = ArrayOffset {base = a 0,
							 index = a 1,
							 offset = Bytes.zero,
							 scale = Type.scale ty,
							 ty = ty},
				      src = a 2})
			end
		     fun codegenOrC (p: Prim.t) =
			let
			   val n = Prim.name p
			in
			   if codegenImplementsPrim p
			      then primApp p
			   else (case Name.cFunction n of
				    NONE =>
				       Error.bug (concat ["unimplemented prim:",
							  Name.toString n])
				  | SOME f => simpleCCall f)
			end
		     datatype z = datatype Prim.Name.t
			   in
			      case Prim.name prim of
				 Array_array => array (a 0)
			       | Array_length => arrayOrVectorLength ()
			       | Array_toVector =>
				    let
				       val array = a 0
				       val vecTy = valOf (toRtype ty)
				       val pt =
					  case Type.dePointer vecTy of
					     NONE => Error.bug "strange Array_toVector"
					   | SOME pt => pt
				    in
				       loop
				       (i - 1,
					Move
					{dst = (Offset
						{base = array,
						 offset = Runtime.headerOffset,
						 ty = Type.defaultWord}),
					 src = PointerTycon pt}
					:: Bind {dst = (valOf var, vecTy),
						 isMutable = false,
						 src = Cast (array, vecTy)}
					:: ss,
					t)
				    end
			       | FFI f => simpleCCall f
			       | GC_collect =>
				    ccall
				    {args = (Vector.new5
					     (GCState,
					      Operand.zero WordSize.default,
					      Operand.bool true,
					      File,
					      Line)),
				     func = (CFunction.gc
					     {maySwitchThreads = handlesSignals})}
			       | GC_pack =>
				    ccall {args = Vector.new1 GCState,
					   func = CFunction.pack}
			       | GC_unpack =>
				    ccall {args = Vector.new1 GCState,
					   func = CFunction.unpack}
			       | IntInf_toVector => cast ()
			       | IntInf_toWord => cast ()
			       | MLton_bogus =>
				    (case toRtype ty of
					NONE => none ()
				      | SOME t => move (bogus t))
			       | MLton_eq =>
				    (case toRtype (varType (arg 0)) of
					NONE => move (Operand.bool true)
				      | SOME t =>
					   codegenOrC
					   (Prim.wordEqual
					    (WordSize.fromBits (Type.width t))))
			       | MLton_installSignalHandler => none ()
			       | MLton_share =>
				    (case toRtype (varType (arg 0)) of
					NONE => none ()
				      | SOME t =>
					   if not (Type.isPointer t)
					      then none ()
					   else
					      simpleCCall (CFunction.share
							   (Operand.ty (a 0))))
			       | MLton_size =>
				    simpleCCall
				    (CFunction.size (Operand.ty (a 0)))
			       | MLton_touch => none ()
			       | Pointer_getPointer => pointerGet ()
			       | Pointer_getReal _ => pointerGet ()
			       | Pointer_getWord _ => pointerGet ()
			       | Pointer_setPointer => pointerSet ()
			       | Pointer_setReal _ => pointerSet ()
			       | Pointer_setWord _ => pointerSet ()
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
						     (Runtime LimitPlusSlop,
						      Operand.word
						      (WordX.fromIntInf
						       (IntInf.fromInt
							(Bytes.toInt Runtime.limitSlop),
							size)))),
					     dst = SOME (tmp, ty),
					     prim = Prim.wordSub size},
					    Statement.Move
					    {dst = Runtime Limit,
					     src = Var {ty = ty, var = tmp}})
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
					       (Runtime SignalIsPending,
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
					   (GCState,
					    Operand.zero WordSize.default,
					    Operand.bool false,
					    File,
					    Line)
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
					    (Runtime CanHandle,
					     {falsee = continue,
					      truee = switchToHandler})}
				     in
					(bumpCanHandle ~1,
					 if handlesSignals 
					    then 
					       Transfer.ifBool
					       (Runtime SignalIsPending,
						{falsee = continue,
						 truee = testCanHandle})
					 else 
					    Transfer.Goto {args = Vector.new0 (),
							   dst = continue})
				     end)
			       | Thread_canHandle =>
				    move (Runtime GCField.CanHandle)
			       | Thread_copy =>
				    ccall {args = (Vector.concat
						   [Vector.new1 GCState,
						    vos args]),
					   func = CFunction.copyThread}
			       | Thread_switchTo =>
				    ccall {args = (Vector.new2
						   (a 0, EnsuresBytesFree)),
					   func = CFunction.threadSwitchTo}
			       | Vector_length => arrayOrVectorLength ()
			       | Weak_canGet =>
				    ifIsWeakPointer
				    (varType (arg 0),
				     fn _ => simpleCCall (CFunction.weakCanGet
							  (Operand.ty (a 0))),
				     fn () => move (Operand.bool false))
			       | Weak_get =>
				    ifIsWeakPointer
				    (varType (arg 0),
				     fn t => (simpleCCall
					      (CFunction.weakGet
					       {arg = Operand.ty (a 0),
						return = t})),
				     none)
			       | Weak_new =>
				    ifIsWeakPointer
				    (ty,
				     fn t =>
				     let
					val result = valOf (toRtype ty)
					val header =
					   PointerTycon
					   (case Type.dePointer result of
					       NONE => Error.bug "Weak_new"
					     | SOME pt => pt)
					val func =
					   CFunction.weakNew {arg = t,
							      return = result}
				     in
					ccall {args = (Vector.concat
						       [Vector.new2
							(GCState, header),
							vos args]),
					       func = func}
				     end,
				     none)
			       | Word_equal s =>
				    codegenOrC (Prim.wordEqual
					       (WordSize.roundUpToPrim s))
			       | Word_toIntInf => cast ()
			       | Word_toWord (s1, s2, {signed}) =>
				    if WordSize.equals (s1, s2)
				       then move (a 0)
				    else
				       let
					  val signed =
					     signed
					     andalso Bits.< (WordSize.bits s1,
							     WordSize.bits s2)
					  val s1 = WordSize.roundUpToPrim s1
					  val s2 = WordSize.roundUpToPrim s2
				       in
					  if WordSize.equals (s1, s2)
					     then cast ()
					  else
					     codegenOrC
					     (Prim.wordToWord
					      (s1, s2, {signed = signed}))
				       end
			       | WordVector_toIntInf => move (a 0)
			       | Word8Array_subWord => subWord ()
			       | Word8Array_updateWord =>
				    add (Move {dst = (ArrayOffset
						      {base = a 0,
						       index = a 1,
						       offset = Bytes.zero,
						       scale = Type.scale Type.defaultWord,
						       ty = Type.defaultWord}),
					       src = a 2})
			       | Word8Vector_subWord => subWord ()
			       | World_save =>
				    ccall {args = (Vector.new2
						   (GCState,
						    Vector.sub (vos args, 0))),
					   func = CFunction.worldSave}
			       | _ => codegenOrC prim
			   end
		      | S.Exp.Select {base, offset} =>
			   (case var of
			       NONE => none ()
			     | SOME var =>
				  (case toRtype ty of
				      NONE => none ()
				    | SOME ty => 
					 adds
					 (select
					  {base = Base.map (base, varOp),
					   baseTy = varType (Base.object base),
					   dst = (var, ty),
					   offset = offset})))
		      | S.Exp.Var y =>
			   (case toRtype ty of
			       NONE => none ()
			     | SOME _ => move (varOp y))
		  end
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
		mayInline = false, (* doesn't matter *)
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
