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

datatype z = datatype IntSize.t
datatype z = datatype WordSize.t

structure CFunction =
   struct
      open CFunction 

      local
	 open CType
      in
	 val Int32 = Int I32
	 val Int64 = Int I64
	 val Word32 = Word W32
	 val Word64 = Word W64
      end

      datatype z = datatype CType.t
      datatype z = datatype Convention.t

      local
	 fun make (name, i) =
	    CFunction.T {args = Vector.new3 (Pointer, Pointer, Word32),
			 bytesNeeded = SOME i,
			 convention = Cdecl,
			 ensuresBytesFree = false,
			 mayGC = false,
			 maySwitchThreads = false,
			 modifiesFrontier = true,
			 modifiesStackTop = false,
			 name = name,
			 return = SOME CType.pointer}
      in
	 val intInfAdd = make ("IntInf_do_add", 2)
	 val intInfAndb = make ("IntInf_do_andb", 2)
	 val intInfGcd = make ("IntInf_do_gcd", 2)
	 val intInfMul = make ("IntInf_do_mul", 2)
	 val intInfOrb = make ("IntInf_do_orb", 2)
	 val intInfQuot = make ("IntInf_do_quot", 2)
	 val intInfRem = make ("IntInf_do_rem", 2)
	 val intInfSub = make ("IntInf_do_sub", 2)
	 val intInfXorb = make ("IntInf_do_xorb", 2)
      end

      local
	 fun make (name, i) =
	    CFunction.T {args = Vector.new3 (Pointer, Word32, Word32),
			 bytesNeeded = SOME i,
			 convention = Cdecl,
			 ensuresBytesFree = false,
			 mayGC = false,
			 maySwitchThreads = false,
			 modifiesFrontier = true,
			 modifiesStackTop = false,
			 name = name,
			 return = SOME CType.pointer}
      in
	 val intInfArshift = make ("IntInf_do_arshift", 2)
	 val intInfLshift = make ("IntInf_do_lshift", 2)
      end

      local
	 fun make (name, i) =
	    CFunction.T {args = Vector.new2 (Pointer, Word32),
			 bytesNeeded = SOME i,
			 convention = Cdecl,
			 ensuresBytesFree = false,
			 mayGC = false,
			 maySwitchThreads = false,
			 modifiesFrontier = true,
			 modifiesStackTop = false,
			 name = name,
			 return = SOME CType.pointer}
      in
	 val intInfNeg = make ("IntInf_do_neg", 1)
	 val intInfNotb = make ("IntInf_do_notb", 1)
      end

      val intInfToString =
	 CFunction.T {args = Vector.new3 (Pointer, Int32, Word32),
		      bytesNeeded = SOME 2,
		      convention = Cdecl,
		      ensuresBytesFree = false,
		      mayGC = false,
		      maySwitchThreads = false,
		      modifiesFrontier = true,
		      modifiesStackTop = false,
		      name = "IntInf_do_toString",
		      return = SOME Pointer}

      local
	 fun make name = vanilla {args = Vector.new2 (Pointer, Pointer),
				  name = name,
				  return = SOME CType.defaultInt}
      in
	 val intInfCompare = make "IntInf_compare"
	 val intInfEqual = make "IntInf_equal"
      end

      local
	 fun make name = vanilla {args = Vector.new2 (Int64, Int64),
				  name = name,
				  return = SOME CType.defaultInt}
      in
	 val int64Equal = make "Int64_equal"
      end

      local
	 fun make name =
	    IntSize.memoize
	    (fn s =>
	     vanilla {args = Vector.new2 (CType.Int s, CType.Int s),
		      name = concat ["Int", IntSize.toString s, "_", name],
		      return = SOME CType.bool})
      in
	 val intGe = make "ge"
	 val intGt = make "gt"
	 val intLe = make "le"
	 val intLt = make "lt"
      end

      local
	 val int = ("Int", CType.Int, IntSize.memoize, IntSize.toString)
	 val word = ("Word", CType.Word, WordSize.memoize, WordSize.toString)
	 fun make ((fromName, fromType, fromMemo, fromString),
		   (toName, toType, toMemo, toString)) =
	    let
	       val f =
		  fromMemo
		  (fn s1 =>
		   toMemo
		   (fn s2 =>
		    vanilla {args = Vector.new1 (fromType s1),
			     name = concat [fromName, fromString s1,
					    "_to", toName, toString s2],
			     return = SOME (toType s2)}))
	    in
	       fn (s1, s2) => f s1 s2
	    end
      in
	 val intToInt = make (int, int)
	 val intToWord = make (int, word)
	 val wordToInt = make (word, int)
      end
   
      local
	 fun make name =
	    IntSize.memoize
	    (fn s =>
	     vanilla {args = Vector.new2 (CType.Int s, CType.Int s),
		      name = concat ["Int", IntSize.toString s, "_", name],
		      return = SOME (CType.Int s)})
      in
	 val intMul = make "mul"
	 val intQuot = make "quot"
	 val intRem = make "rem"
      end

      val word64Equal = vanilla {args = Vector.new2 (Word64, Word64),
				 name = "Word64_equal",
				 return = SOME CType.defaultInt}

      val copyCurrentThread =
	 T {args = Vector.new1 Pointer,
	    bytesNeeded = NONE,
	    convention = Cdecl,
	    ensuresBytesFree = false,
	    mayGC = true,
	    maySwitchThreads = false,
	    modifiesFrontier = true,
	    modifiesStackTop = true,
	    name = "GC_copyCurrentThread",
	    return = NONE}

      val copyThread =
	 T {args = Vector.new2 (Pointer, Pointer),
	    bytesNeeded = NONE,
	    convention = Cdecl,
	    ensuresBytesFree = false,
	    mayGC = true,
	    maySwitchThreads = false,
	    modifiesFrontier = true,
	    modifiesStackTop = true,
	    name = "GC_copyThread",
	    return = SOME Pointer}

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
	    return = NONE}

      val gcArrayAllocate =
	 T {args = Vector.new4 (Pointer, Word32, Word32, Word32),
	    bytesNeeded = NONE,
	    convention = Cdecl,
	    ensuresBytesFree = true,
	    mayGC = true,
	    maySwitchThreads = false,
	    modifiesFrontier = true,
	    modifiesStackTop = true,
	    name = "GC_arrayAllocate",
	    return = SOME Pointer}

      local
	 fun make name =
	    T {args = Vector.new1 Pointer,
	       bytesNeeded = NONE,
	       convention = Cdecl,
	       ensuresBytesFree = false,
	       mayGC = true,
	       maySwitchThreads = false,
	       modifiesFrontier = true,
	       modifiesStackTop = true,
	       name = name,
	       return = NONE}
      in
	 val pack = make "GC_pack"
	 val unpack = make "GC_unpack"
      end

      val threadSwitchTo =
	 T {args = Vector.new2 (Pointer, Word32),
	    bytesNeeded = NONE,
	    convention = Cdecl,
	    ensuresBytesFree = true,
	    mayGC = true,
	    maySwitchThreads = true,
	    modifiesFrontier = true,
	    modifiesStackTop = true,
	    name = "Thread_switchTo",
	    return = NONE}

      val weakCanGet =
	 vanilla {args = Vector.new1 Pointer,
		  name = "GC_weakCanGet",
		  return = SOME CType.bool}
	 
      val weakGet =
	 vanilla {args = Vector.new1 Pointer,
		  name = "GC_weakGet",
		  return = SOME Pointer}
		  
      val weakNew =
	 T {args = Vector.new3 (Pointer, Word32, Pointer),
	    bytesNeeded = NONE,
	    convention = Cdecl,
	    ensuresBytesFree = false,
	    mayGC = true,
	    maySwitchThreads = false,
	    modifiesFrontier = true,
	    modifiesStackTop = true,
	    name = "GC_weakNew",
	    return = SOME Pointer}

      val worldSave =
	 T {args = Vector.new2 (Pointer, Int32),
	    bytesNeeded = NONE,
	    convention = Cdecl,
	    ensuresBytesFree = false,
	    mayGC = true,
	    maySwitchThreads = false,
	    modifiesFrontier = true,
	    modifiesStackTop = true,
	    name = "GC_saveWorld",
	    return = NONE}
   end

datatype z = datatype Operand.t
datatype z = datatype Statement.t
datatype z = datatype Transfer.t

structure Representation = Representation (structure Rssa = Rssa
					   structure Ssa = Ssa)
local
   open Representation
in
   structure ConRep = ConRep
   structure TupleRep = TupleRep
   structure TyconRep = TyconRep
end

fun convert (program as S.Program.T {functions, globals, main, ...})
   : Rssa.Program.t =
   let
      val callsFromC =
	 S.Program.hasPrim (program, fn p =>
			    case Prim.name p of
			       Prim.Name.Thread_returnToC => true
			     | _ => false)
      val {conRep, objectTypes, refRep, toRtype, tupleRep, tyconRep} =
	 Representation.compute program
      val conRep =
	 Trace.trace ("conRep", Con.layout, ConRep.layout) conRep
      fun tyconTy (pt: PointerTycon.t): ObjectType.t =
	 Vector.sub (objectTypes, PointerTycon.index pt)
      (* varInt is set for variables that are constant integers.  It is used
       * so that we can precompute array numBytes when numElts is known.
       *)
      val {get = varInt: Var.t -> IntX.t option,
	   set = setVarInt, ...} =
	 Property.getSetOnce (Var.plist, Property.initConst NONE)
      val _ =
	 Vector.foreach (globals, fn S.Statement.T {var, exp, ...} =>
			 case exp of
			    S.Exp.Const c =>
			       (case c of
				   Const.Int n =>
				      Option.app (var, fn x =>
						  setVarInt (x, SOME n))
				 | _ => ())
			  | _ => ())
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
      fun toRtypes ts = Vector.map (ts, toRtype)
      fun conSelects {rep = TupleRep.T {offsets, ...},
		      variant: Operand.t}: Operand.t vector =
	 Vector.keepAllMap
	 (offsets, fn off =>
	  Option.map (off, fn {offset, ty} =>
		      Offset {base = variant,
			      offset = offset,
			      ty = ty}))
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
      fun genCase {cases: (Con.t * Label.t) vector,
		   default: Label.t option,
		   test: Var.t,
		   testRep: TyconRep.t}: Statement.t list * Transfer.t =
	 let
	    fun enum (test: Operand.t): Transfer.t =
	       let
		  val cases =
		     Vector.keepAllMap
		     (cases, fn (c, j) =>
		      case conRep c of
			 ConRep.IntAsTy {int, ...} => SOME (int, j)
		       | _ => NONE)
		  val numEnum =
		     case Operand.ty test of
			Type.EnumPointers {enum, ...} => Vector.length enum
		      | _ => Error.bug "strage enum"
		  val default =
		     if numEnum = Vector.length cases
			then NONE
		     else default
	       in
		  if 0 = Vector.length cases
		     then
			(case default of
			    NONE => Error.bug "no targets"
			  | SOME l => Goto {dst = l,
					    args = Vector.new0 ()})
		  else
		     let
			val l = #2 (Vector.sub (cases, 0))
		     in
			if Vector.forall (cases, fn (_, l') =>
					  Label.equals (l, l'))
			   andalso (case default of
				       NONE => true
				     | SOME l' => Label.equals (l, l'))
			   then Goto {dst = l,
				      args = Vector.new0 ()}
			else
			   let
			      val cases =
				 QuickSort.sortVector
				 (cases, fn ((i, _), (i', _)) => i <= i')
			      val cases =
				 Vector.map (cases, fn (i, l) =>
					     (IntX.make (IntInf.fromInt i,
							 IntSize.default),
					      l))
			   in
			      Switch (Switch.Int {cases = cases,
						  default = default,
						  size = IntSize.default,
						  test = test})
			   end
		     end
	       end
	    fun transferToLabel (transfer: Transfer.t): Label.t =
	       case transfer of
		  Transfer.Goto {args, dst, ...} =>
		     (Assert.assert ("transferToLabel", fn () =>
				     0 = Vector.length args)
		      ; dst)
		| _ => newBlock {args = Vector.new0 (),
				 kind = Kind.Jump,
				 statements = Vector.new0 (),
				 transfer = transfer}
	    fun switchEP
	       (makePointersTransfer: Operand.t -> Statement.t list * Transfer.t)
	       : Transfer.t =
	       let
		  val test = varOp test
		  val {enum = e, pointers = p} =
		     case Operand.ty test of
			Type.EnumPointers ep => ep
		      | _ => Error.bug "strange switchEP"
		  val enumTy = Type.EnumPointers {enum = e,
						  pointers = Vector.new0 ()}
		  val enumVar = Var.newNoname ()
		  val enumOp = Operand.Var {var = enumVar,
					    ty = enumTy}
		  val pointersTy = Type.EnumPointers {enum = Vector.new0 (),
						      pointers = p}
		  val pointersVar = Var.newNoname ()
		  val pointersOp = Operand.Var {ty = pointersTy,
						var = pointersVar}
		  fun block (var, ty, statements, transfer) =
		     newBlock {args = Vector.new0 (),
			       kind = Kind.Jump,
			       statements = (Vector.fromList
					     (Statement.Bind
					      {isMutable = false,
					       oper = Operand.Cast (test, ty),
					       var = var}
					      :: statements)),
			       transfer = transfer}
		  val (s, t) = makePointersTransfer pointersOp
		  val pointers = block (pointersVar, pointersTy, s, t)
		  val enum = block (enumVar, enumTy, [], enum enumOp)
	       in
		  Switch (Switch.EnumPointers
			  {enum = enum,
			   pointers = pointers,
			   test = test})
	       end
	    fun tail (l: Label.t, args: Operand.t vector): Label.t =
	       if 0 = Vector.length args
		  then l
	       else
		  let
		     val xs = Vector.map (args, fn _ => Var.newNoname ())
		  in
		     newBlock {args = Vector.new0 (),
			       kind = Kind.Jump,
			       statements = Vector.new0 (),
			       transfer = Goto {dst = l, args = args}}
		  end
	    fun enumAndOne (): Transfer.t =
	       let
		  fun make (pointersOp: Operand.t)
		     : Statement.t list * Transfer.t =
		     let
			val (dst, args: Operand.t vector) =
			   case Vector.peekMap
			      (cases, fn (c, j) =>
			       case conRep c of
				  ConRep.Transparent _ =>
				     SOME (j, Vector.new1 pointersOp)
				| ConRep.Tuple r =>
				     SOME (j, conSelects {rep = r,
							  variant = pointersOp})
				| _ => NONE) of
			      NONE =>
				 (case default of
				     NONE => Error.bug "enumAndOne: no default"
				   | SOME j => (j, Vector.new0 ()))
			    | SOME z => z
		     in
			([], Transfer.Goto {args = args,
					    dst = dst})
		     end
	       in
		  switchEP make
	       end
	    fun indirectTag (test: Operand.t): Statement.t list * Transfer.t =
	       let
		  val cases =
		     Vector.keepAllMap
		     (cases, fn (c, l) =>
		      case conRep c of
			 ConRep.TagTuple {rep, tag} =>
			    let
			       val tycon = TupleRep.tycon rep
			       val tag =
				  if !Control.variant = Control.FirstWord
				     then tag
				  else PointerTycon.index tycon
			       val pointerVar = Var.newNoname ()
			       val pointerTy = Type.pointer tycon
			       val pointerOp =
				  Operand.Var {ty = pointerTy,
					       var = pointerVar}
			       val statements =
				  Vector.new1
				  (Statement.Bind
				   {isMutable = false,
				    oper = Operand.Cast (test, pointerTy),
				    var = pointerVar})
			       val dst =
				  newBlock
				  {args = Vector.new0 (),
				   kind = Kind.Jump,
				   statements = statements,
				   transfer =
				   Goto {args = conSelects {rep = rep,
							    variant = pointerOp},
					 dst = l}}
			    in
			       SOME {dst = dst,
				     tag = tag,
				     tycon = tycon}
			    end
		       | _ => NONE)
		  val numTag =
		     case Operand.ty test of
			Type.EnumPointers {pointers, ...} =>
			   Vector.length pointers
		      | _ => Error.bug "strange indirecTag"
		  val default =
		     if numTag = Vector.length cases
			then NONE
		     else default
		  val cases =
		     QuickSort.sortVector
		     (cases, fn ({tycon = t, ...}, {tycon = t', ...}) =>
		      PointerTycon.<= (t, t'))
		  val (ss, tag) =
		     case !Control.variant of
			Control.FirstWord =>
			   ([], Offset {base = test,
					offset = tagOffset,
					ty = Type.defaultInt})
		      | Control.Header =>
			   let
			      val headerOffset = ~4
			      val tagVar = Var.newNoname ()
			      val s =
				 PrimApp {args = (Vector.new2
						  (Offset {base = test,
							   offset = headerOffset,
							   ty = Type.defaultWord},
						   Operand.word (WordX.one WordSize.default))),
					  dst = SOME (tagVar, Type.defaultWord),
					  prim = Prim.wordRshift WordSize.default}
			   in
			      ([s], Cast (Var {ty = Type.defaultWord,
					       var = tagVar},
					  Type.defaultInt))
			   end
		      | HeaderIndirect =>
			   Error.bug "HeaderIndirect unimplemented"
	       in
		  (ss,
		   Switch (Switch.Pointer {cases = cases,
					   default = default,
					   tag = tag,
					   test = test}))
	       end
	    fun prim () =
	       case (Vector.length cases, default) of
		  (1, _) =>
		     (* We use _ instead of NONE for the default becuase
		      * there may be an unreachable default case.
		      *)
		     let
			val (c, l) = Vector.sub (cases, 0)
		     in
			case conRep c of
			   ConRep.Void =>
			      Goto {dst = l,
				    args = Vector.new0 ()}
			 | ConRep.Transparent _ =>
			      Goto {dst = l,
				    args = Vector.new1 (varOp test)}
			 | ConRep.Tuple r =>
			      Goto {dst = l,
				    args = conSelects {rep = r,
						       variant = (varOp test)}}
			 | _ => Error.bug "strange conRep for Prim"
		     end
		| (0, SOME l) => Goto {dst = l, args = Vector.new0 ()}
		| _ => Error.bug "prim datatype with more than one case"
	 in
	    case testRep of
	       TyconRep.Direct => ([], prim ())
	     | TyconRep.Enum => ([], enum (varOp test))
	     | TyconRep.EnumDirect => ([], enumAndOne ())
	     | TyconRep.EnumIndirect => ([], enumAndOne ())
	     | TyconRep.EnumIndirectTag => ([], switchEP indirectTag)
	     | TyconRep.IndirectTag => indirectTag (varOp test)
	     | TyconRep.Void => ([], prim ())
	 end
      fun translateCase ({test: Var.t,
			  cases: S.Cases.t,
			  default: Label.t option})
	 : Statement.t list * Transfer.t =
	 let
	    fun id x = x
	    fun simple (s, cs, make, branch, le) =
	       ([],
		Switch
		(make {cases = (QuickSort.sortVector
				(Vector.map (cs, fn (i, j) => (branch i, j)),
				 fn ((i, _), (i', _)) => le (i, i'))),
		       default = default,
		       size = s,
		       test = varOp test}))
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
			       then genCase {cases = cases,
					     default = default,
					     test = test,
					     testRep = tyconRep tycon}
			    else Error.bug "strange type in case"
			 end)
	     | S.Cases.Int (s, cs) => simple (s, cs, Switch.Int, id, IntX.<=)
	     | S.Cases.Word (s, cs) => simple (s, cs, Switch.Word, id, WordX.<=)
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
	    val info as {handler, ...} = labelInfo l
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
	    val info as {cont, ...} = labelInfo l
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
				       prim = prim,
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
	 in
	    case t of
	       Type.EnumPointers (ep as {enum, ...})  =>
		  Operand.Cast (Operand.int (IntX.one IntSize.default), t)
	     | Type.ExnStack => Error.bug "bogus ExnStack"
	     | Type.Int s => c (Const.int (IntX.zero s))
	     | Type.IntInf => SmallIntInf 0wx1
	     | Type.Label _ => Error.bug "bogus Label"
	     | Type.MemChunk _ => Error.bug "bogus MemChunk"
	     | Type.Real s => c (Const.real (RealX.zero s))
	     | Type.Word s => c (Const.word (WordX.zero s))
	 end
      fun translateStatementsTransfer (statements, ss, transfer) =
	 let
	    fun loop (i, ss, t): Statement.t vector * Transfer.t =
	       if i < 0
		  then (Vector.fromList ss, t)
	       else
		  let
		     val s as S.Statement.T {var, ty, exp} =
			Vector.sub (statements, i)
		     fun none () = loop (i - 1, ss, t)
		     fun add s = loop (i - 1, s :: ss, t)
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
		     fun makeStores (ys: Var.t vector, offsets) =
			QuickSort.sortVector
			(Vector.keepAllMap2
			 (ys, offsets, fn (y, offset) =>
			  Option.map (offset, fn {offset, ty} =>
				      {offset = offset,
				       value = varOp y})),
			 fn ({offset = i, ...}, {offset = i', ...}) => i <= i')
		     fun allocate (ys: Var.t vector,
				   TupleRep.T {size, offsets, ty, tycon, ...}) =
			add (Object {dst = valOf var,
				     size = size + Runtime.normalHeaderSize,
				     stores = makeStores (ys, offsets),
				     ty = ty,
				     tycon = tycon})
		     val allocate =
			Trace.trace2
			("allocate",
			 Vector.layout Var.layout,
			 TupleRep.layout,
			 Layout.ignore)
			allocate
		     fun allocateTagged (n: int,
					 ys: Var.t vector,
					 TupleRep.T {size, offsets, ty, tycon}) =
			add (Object
			     {dst = valOf var,
			      size = size + Runtime.normalHeaderSize,
			      stores = (Vector.concat
					[Vector.new1
					 {offset = tagOffset,
					  value = (Operand.int
						   (IntX.make
						    (IntInf.fromInt n,
						     IntSize.default)))},
					 makeStores (ys, offsets)]),
			      ty = ty,
			      tycon = tycon})
		     fun move (oper: Operand.t) =
			add (Bind {isMutable = false,
				   oper = oper,
				   var = valOf var})
		  in
		     case exp of
			S.Exp.ConApp {con, args} =>
			   (case conRep con of
			       ConRep.Void => none ()
			     | ConRep.IntAsTy {int, ty} =>
				  move (Operand.Cast
					(Operand.int
					 (IntX.make (IntInf.fromInt int,
						     IntSize.default)),
					 ty))
			     | ConRep.TagTuple {rep, tag} =>
				  if !Control.variant = Control.FirstWord
				     then allocateTagged (tag, args, rep)
				  else allocate (args, rep)
			     | ConRep.Transparent _ =>
				  move (Operand.cast
					(varOp (Vector.sub (args, 0)),
					 valOf (toRtype ty)))
			     | ConRep.Tuple rep =>
				  allocate (args, rep))
		      | S.Exp.Const c => move (Operand.Const c)
		      | S.Exp.PrimApp {prim, targs, args, ...} =>
			   let
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
					  then yes ()
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
			      fun normal () =
				 add (PrimApp {dst = dst (),
					       prim = prim,
					       args = varOps args})
			      datatype z = datatype Prim.Name.t
			      fun bumpCanHandle n =
				 let
				    val canHandle =
				       Operand.Runtime GCField.CanHandle
				    val res = Var.newNoname ()
				 in
				    [Statement.PrimApp
				     {args = (Vector.new2
					      (canHandle,
					       (Operand.int
						(IntX.make
						 (IntInf.fromInt n,
						  IntSize.default))))),
				      dst = SOME (res, Type.defaultInt),
				      prim = Prim.intAdd IntSize.default},
				     Statement.Move
				     {dst = canHandle,
				      src = Operand.Var {var = res,
							 ty = Type.defaultInt}}]
				 end
			      fun ccallGen
				 {args: Operand.t vector,
				  func: CFunction.t,
				  prefix: Transfer.t -> (Statement.t list
							 * Transfer.t)} =
				 let
				    val (formals, return) =
				       case dst () of
					  NONE => (Vector.new0 (), NONE)
					| SOME (x, t) =>
					     (Vector.new1 (x, t), SOME t)
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
				    val pt =
				       case (Type.dePointer
					     (valOf (toRtype ty))) of
					  NONE => Error.bug "strange array"
					| SOME pt => PointerTycon pt
				    val args =
				       Vector.new4 (Operand.GCState,
						    Operand.EnsuresBytesFree,
						    numElts,
						    pt)
				 in
				    ccall {args = args,
					   func = CFunction.gcArrayAllocate}
				 end
		     fun updateCard (addr: Operand.t, prefix, assign) =
		        let
			   val index = Var.newNoname ()
			   val ss = 
			      (PrimApp
			       {args = (Vector.new2
					(Operand.Cast (addr, Type.defaultWord),
					 Operand.word
					 (WordX.make
					  (LargeWord.fromInt
					   (!Control.cardSizeLog2),
					   WordSize.default)))),
				dst = SOME (index, Type.defaultInt),
				prim = Prim.wordRshift WordSize.default})
			      :: (Move
				  {dst = (Operand.ArrayOffset
					  {base = (Operand.Runtime
						   GCField.CardMap),
					   index = (Operand.Var
						    {ty = Type.defaultInt,
						     var = index}),
					   ty = Type.word W8}),
				   src = Operand.word (WordX.one W8)})
			      :: assign
			      :: ss
			in
			  loop (i - 1, prefix ss, t)
			end
		     fun arrayUpdate (ty: Type.t) =
		        if !Control.markCards andalso Type.isPointer ty
			   then let
				   val src = varOp (a 2)
				   val arrayOp = varOp (a 0)
				   val temp = Var.newNoname ()
				   val tempOp =
				      Operand.Var {var = temp,
						   ty = Type.defaultWord}
				   val addr = Var.newNoname ()
				   val mc =
				      case Type.dePointer (Operand.ty arrayOp) of
					 NONE => Error.bug "strange array"
				       | SOME p => 
					    case tyconTy p of
					       ObjectType.Array mc => mc
					     | _ => Error.bug "strange array"
				   val addrOp =
				      Operand.Var {var = addr,
						   ty = Type.MemChunk mc}
				   fun prefix ss =
				      (PrimApp
				       {args = Vector.new2
					       (Operand.Cast (varOp (a 1),
							      Type.defaultWord),
					        Operand.word
						(WordX.make
						 (LargeWord.fromInt
						  (Type.size ty),
						  WordSize.default))),
				        dst = SOME (temp, Type.defaultWord),
				        prim = Prim.wordMul WordSize.default})
				      :: (PrimApp
					  {args = (Vector.new2
						   (Operand.Cast (arrayOp,
								  Type.defaultWord),
						    tempOp)),
					   dst = SOME (addr, Type.MemChunk mc),
					   prim = Prim.wordAdd WordSize.default})
				      :: ss
				   val assign =
				      Move {dst = (Operand.Offset
						   {base = addrOp,
						    offset = 0,
						    ty = ty}),
					    src = varOp (a 2)}
				in
				   updateCard (addrOp, prefix, assign)
				end
			else add (Move {dst = arrayOffset ty,
					src = varOp (a 2)})
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
			   val assign = Move {dst = Operand.Offset {base = addr,
								    offset = 0,
								    ty = ty},
					      src = src}
			in
			   if !Control.markCards andalso Type.isPointer ty
			      then updateCard (addr, fn ss => ss, assign)
			   else loop (i - 1, assign::ss, t)
			end
			      datatype z = datatype Prim.Name.t
			   in
			      case Prim.name prim of
				 Array_array =>
				    array (Operand.Var {var = a 0,
							ty = Type.defaultInt})
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
				    if s = IntSize.I64 andalso !Control.Native.native 
				       then simpleCCall CFunction.int64Equal
				       else normal ()
			       | Int_ge s =>
				    if s = IntSize.I64 andalso !Control.Native.native
				       then simpleCCall (CFunction.intGe s)
				    else normal ()
			       | Int_gt s =>
				    if s = IntSize.I64 andalso !Control.Native.native
				       then simpleCCall (CFunction.intGt s)
				    else normal ()
			       | Int_le s =>
				    if s = IntSize.I64 andalso !Control.Native.native
				       then simpleCCall (CFunction.intLe s)
				    else normal ()
			       | Int_lt s =>
				    if s = IntSize.I64 andalso !Control.Native.native
				       then simpleCCall (CFunction.intLt s)
				    else normal ()
			       | Int_mul s =>
				    if s = IntSize.I64 andalso !Control.Native.native
				       then simpleCCall (CFunction.intMul s)
				    else normal ()
			       | Int_quot s =>
				    if s = IntSize.I64
				       orelse not (!Control.Native.native)
				       then simpleCCall (CFunction.intQuot s)
				    else normal ()
			       | Int_rem s =>
				    if s = IntSize.I64
				       orelse not (!Control.Native.native)
				       then simpleCCall (CFunction.intRem s)
				    else normal ()
			       | Int_toInt (s1, s2) =>
				    let
				       datatype z = datatype IntSize.t
				    in
				       if (case (s1, s2) of
					      (I32, I64) => true
					    | (I64, I32) => true
					    | _ => false)
					  andalso !Control.Native.native
					  then simpleCCall (CFunction.intToInt (s1, s2))
				       else normal ()
				    end
			       | Int_toWord (s1, s2) =>
				    let
				       datatype z = datatype IntSize.t
				       datatype z = datatype WordSize.t
				    in
				       if (case (s1, s2) of
					      (I64, W32) => true
					    | _ => false)
					  andalso !Control.Native.native
					  then simpleCCall (CFunction.intToWord (s1, s2))
				       else normal ()
				    end
			       | IntInf_add => simpleCCall CFunction.intInfAdd
			       | IntInf_andb => simpleCCall CFunction.intInfAndb
			       | IntInf_arshift =>
				    simpleCCall CFunction.intInfArshift
			       | IntInf_compare =>
				    simpleCCall CFunction.intInfCompare
			       | IntInf_equal =>
				    simpleCCall CFunction.intInfEqual
			       | IntInf_gcd => simpleCCall CFunction.intInfGcd
			       | IntInf_lshift =>
				    simpleCCall CFunction.intInfLshift
			       | IntInf_mul => simpleCCall CFunction.intInfMul
			       | IntInf_neg => simpleCCall CFunction.intInfNeg
			       | IntInf_notb => simpleCCall CFunction.intInfNotb
			       | IntInf_orb => simpleCCall CFunction.intInfOrb
			       | IntInf_quot => simpleCCall CFunction.intInfQuot
			       | IntInf_rem => simpleCCall CFunction.intInfRem
			       | IntInf_sub => simpleCCall CFunction.intInfSub
			       | IntInf_toString =>
				    simpleCCall CFunction.intInfToString
			       | IntInf_toVector => cast ()
			       | IntInf_toWord => cast ()
			       | IntInf_xorb => simpleCCall CFunction.intInfXorb
			       | MLton_bogus =>
				    (case toRtype ty of
					NONE => none ()
				      | SOME t => move (bogus t))
			       | MLton_bug => simpleCCall CFunction.bug
			       | MLton_eq =>
				    (case targ () of
					NONE => move (Operand.bool true)
				      | SOME _ => normal ())
			       | MLton_size => simpleCCall CFunction.size
			       | Pointer_getInt s => pointerGet (Type.Int s)
			       | Pointer_getPointer =>
				    (case targ () of
					NONE => Error.bug "getPointer"
				      | SOME t => pointerGet t)
			       | Pointer_getReal s => pointerGet (Type.Real s)
			       | Pointer_getWord s => pointerGet (Type.Word s)
			       | Pointer_setInt s => pointerSet (Type.Int s)
			       | Pointer_setPointer =>
				    (case targ () of
					NONE => Error.bug "setPointer"
				      | SOME t => pointerSet t)
			       | Pointer_setReal s => pointerSet (Type.Real s)
			       | Pointer_setWord s => pointerSet (Type.Word s)
			       | Ref_assign =>
				    (case targ () of
					NONE => none ()
				      | SOME ty => refAssign (ty, varOp (a 1)))
			       | Ref_deref =>
				    (case targ () of
					NONE => none ()
				      | SOME ty =>
					   move (Offset {base = varOp (a 0),
							 offset = 0,
							 ty = ty}))
			       | Ref_ref =>
				    allocate
				    (Vector.new1 (a 0),
				     refRep (Vector.sub (targs, 0)))
			       | Thread_atomicBegin =>
				    (* assert (s->canHandle >= 0);
				     * s->canHandle++;
				     * if (s->signalIsPending)
				     *         s->limit = s->limitPlusSlop
				     *                    - LIMIT_SLOP;
				     *)
				    split
				    (Vector.new0 (), Kind.Jump, ss, fn l =>
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
						      (WordX.make
						       (LargeWord.fromInt
							Runtime.limitSlop,
							size)))),
					     dst = SOME (tmp, ty),
					     prim = Prim.wordSub size},
					    Statement.Move
					    {dst = Operand.Runtime Limit,
					     src = Operand.Var {var = tmp,
								ty = ty}})
					val l' =
					   newBlock
					   {args = Vector.new0 (),
					    kind = Kind.Jump,
					    statements = statements,
					    transfer = (Transfer.Goto
							{args = Vector.new0 (),
							 dst = l})}
				     in
					(bumpCanHandle 1,
					 Transfer.ifInt
					 (Operand.Runtime SignalIsPending,
					  {falsee = l,
					   truee = l'}))
				     end)
			       | Thread_atomicEnd =>
				    (* gcState.canHandle--;
				     * assert(gcState.canHandle >= 0);
				     * if (gcState.signalIsPending
				     *     and 0 == gcState.canHandle)
				     *         gcState.limit = 0;
				     *)
				    split
				    (Vector.new0 (), Kind.Jump, ss, fn l =>
				     let
					datatype z = datatype GCField.t
					val statements =
					   Vector.new1
					   (Statement.Move
					    {dst = Operand.Runtime Limit,
					     src =
					     Operand.word
					     (WordX.zero (WordSize.pointer ()))})
					val l'' =
					   newBlock
					   {args = Vector.new0 (),
					    kind = Kind.Jump,
					    statements = statements,
					    transfer =
					    Transfer.Goto
					    {args = Vector.new0 (),
					     dst = l}}
					val l' =
					   newBlock
					   {args = Vector.new0 (),
					    kind = Kind.Jump,
					    statements = Vector.new0 (),
					    transfer =
					    Transfer.ifInt
					    (Operand.Runtime CanHandle,
					     {falsee = l'',
					      truee = l})}
				     in
					(bumpCanHandle ~1,
					 Transfer.ifInt
					 (Operand.Runtime SignalIsPending,
					  {falsee = l,
					   truee = l'}))
				     end)
			       | Thread_canHandle =>
				    move (Operand.Runtime GCField.CanHandle)
			       | Thread_copy =>
				    ccall {args = (Vector.concat
						   [Vector.new1 Operand.GCState,
						    vos args]),
					   func = CFunction.copyThread}
			       | Thread_returnToC =>
				    ccall {args = vos args,
					   func = CFunction.returnToC}
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
				    (fn () => simpleCCall CFunction.weakCanGet,
				     fn () => move (Operand.bool false))
			       | Weak_get =>
				    ifTargIsPointer
				    (fn () => simpleCCall CFunction.weakGet,
				     none)
			       | Weak_new =>
				    ifTargIsPointer
				    (fn () =>
				     let
					val header =
					   Operand.PointerTycon
					   (valOf
					    (Type.dePointer
					     (valOf (toRtype ty))))
				     in
					ccall {args = (Vector.concat
						       [Vector.new2
							(Operand.GCState,
							 header),
							vos args]),
					       func = CFunction.weakNew}
				     end,
				     none)
			       | Word_equal s =>
				    if s = WordSize.W64
				       then simpleCCall CFunction.word64Equal
				    else normal ()
			       | Word_toInt (s1, s2) =>
				    let
				       datatype z = datatype IntSize.t
				       datatype z = datatype WordSize.t
				    in
				       if (case (s1, s2) of
					      (W32, I64) => true
					    | _ => false)
					  andalso !Control.Native.native
					  then simpleCCall (CFunction.wordToInt (s1, s2))
				       else normal ()
				    end
			       | Word_toIntInf => cast ()
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
			       | _ => normal ()
			   end
		      | S.Exp.Profile e => add (Statement.Profile e)
		      | S.Exp.Select {tuple, offset} =>
			   let
			      val TupleRep.T {offsets, ...} =
				 tupleRep (varType tuple)
			   in
			      case Vector.sub (offsets, offset) of
				 NONE => none ()
			       | SOME {offset, ty} =>
				    move (Offset {base = varOp tuple,
						  offset = offset,
						  ty = ty})
			   end
		      | S.Exp.Tuple ys =>
			   if 0 = Vector.length ys
			      then none ()
			   else allocate (ys, tupleRep ty)
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
			 main = main,
			 objectTypes = objectTypes}
      val _ = Program.clear p
   in
      p
   end

end
