functor SsaToRssa (S: SSA_TO_RSSA_STRUCTS): SSA_TO_RSSA =
struct

open S

structure S = Ssa

open Rssa
datatype z = datatype Operand.t
datatype z = datatype Statement.t
datatype z = datatype Transfer.t

structure ImplementHandlers = ImplementHandlers (structure Ssa = Ssa)
structure Representation = Representation (structure Ssa = Ssa
					   structure Mtype = Type)

local open Representation
in
   structure TyconRep = TyconRep
   structure ConRep = ConRep
end

fun convert (p: S.Program.t): Rssa.Program.t =
   let
      val program as S.Program.T {datatypes, globals, functions, main} =
	 ImplementHandlers.doit p
      val {tyconRep, conRep, toMtype = toType} = Representation.compute program
      (* varInt is set for variables that are constant integers.  It is used
       * so that we can precompute array numBytes when numElts is known.
       *)
      val {get = varInt: Var.t -> int option,
	   set = setVarInt, ...} =
	 Property.getSetOnce (Var.plist, Property.initConst NONE)
      val _ =
	 Vector.foreach (globals, fn S.Statement.T {var, exp, ...} =>
			 case exp of
			    S.Exp.Const c =>
			       (case Const.node c of
				   Const.Node.Int n =>
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
	 Var {var = x, ty = valOf (toType (varType x))}
      fun varOps xs = Vector.map (xs, varOp)
      val _ =
	 Control.diagnostics
	 (fn display =>
	  (display (Layout.str "Representations:")
	   ; (Vector.foreach
	      (datatypes, fn S.Datatype.T {tycon, cons} =>
	       let open Layout
	       in display (seq [Tycon.layout tycon,
				str " ",
				TyconRep.layout (tyconRep tycon)])
		  ; display (indent
			     (Vector.layout (fn {con, ...} =>
					     seq [Con.layout con,
						  str " ",
						  ConRep.layout (conRep con)])
			      cons,
			      2))
	       end))))
      fun toTypes ts = Vector.map (ts, toType)
      val labelSize = Type.size Type.label
      val tagOffset = 0
      fun sortTypes (initialOffset: int,
		     tys: Type.t option vector)
	 : {numPointers: int,
	    numWordsNonPointers: int,
	    offsets: {offset: int, ty: Type.t} option vector,
	    size: int} =
	 let
	    val bytes = ref []
	    val doubleWords = ref []
	    val words = ref []
	    val pointers = ref []
	    val numPointers = ref 0
	    val _ =
	       Vector.foreachi
	       (tys, fn (i, t) =>
		case t of
		   NONE => ()
		 | SOME t =>
		      let
			 val r =
			    if Type.isPointer t
			       then (Int.inc numPointers
				     ; pointers)
			    else (case Type.size t of
				     1 => bytes
				   | 4 => words
				   | 8 => doubleWords
				   | _ => Error.bug "strange size")
		      in
			 List.push (r, (i, t))
		      end)
	    fun build (r, size, accum) =
	       List.fold (!r, accum, fn ((index, ty), (res, offset)) =>
			  ({index = index, offset = offset, ty = ty} :: res,
			   offset + size))
	    val (accum, offset: int) =
	       build (bytes, 1,
		      build (words, 4,
			     build (doubleWords, 8, ([], initialOffset))))
	    val offset = Type.align (Type.pointer, offset)
	    val numWordsNonPointers =
	       (offset - initialOffset) div Runtime.wordSize
	    val (components, size) = build (pointers, 4, (accum, offset))
	    val offsets =
	       Vector.mapi
	       (tys, fn (i, ty) =>
		Option.map
		(ty, fn ty =>
		 let
		    val {offset, ty, ...} =
		       List.lookup (components, fn {index, ...} => i = index)
		 in
		    {offset = offset, ty = ty}
		 end))
	 in
	    {numPointers = !numPointers,
	     numWordsNonPointers = numWordsNonPointers,
	     offsets = offsets,
	     size = size}
	 end
      (* Compute layout for each con and associate it with the con. *)
      local
	 val {get, set, ...} =
	    Property.getSetOnce (Con.plist,
				 Property.initRaise ("con info", Con.layout))
      in
	 val _ =
	    Vector.foreach
	    (datatypes, fn S.Datatype.T {cons, ...} =>
	     Vector.foreach (cons, fn {con, args} =>
			     let
				fun doit n =
				   let
				      val mtypes = toTypes args
				      val info = sortTypes (n, mtypes)
				   in
				      set (con, {info = info,
						 mtypes = mtypes})
				   end
			     in
				case conRep con of
				   ConRep.Tuple => doit 0
				 | ConRep.TagTuple _ => doit 4
				 | _ => ()
			     end))
	 val conInfo = get
      end
      (* Compute layout for each tuple type. *)
      val {get = tupleInfo, ...} =
	 Property.get (S.Type.plist,
		       Property.initFun
		       (fn t => sortTypes (0, toTypes (S.Type.detuple t))))
      fun conSelects (variant: Var.t, con: Con.t): Operand.t vector =
	 let
	    val _ = Assert.assert ("conSelects", fn () =>
				   case conRep con of
				      ConRep.TagTuple _ => true
				    | ConRep.Tuple => true
				    | _ => false)
	    val {info = {offsets, ...}, ...} = conInfo con
	 in
	    Vector.keepAllMap (offsets, fn off =>
			       Option.map (off, fn {offset, ty} =>
					   Offset {base = variant,
						   bytes = offset,
						   ty = ty}))
	 end
      val extraBlocks = ref []
      val (resetAllocTooLarge, allocTooLarge) = Block.allocTooLarge extraBlocks
      fun newBlock {args, kind, profileInfo,
		    statements: Statement.t vector,
		    transfer: Transfer.t}: Label.t =
	 let
	    val l = Label.newNoname ()
	    val _ = List.push (extraBlocks,
			       Block.T {args = args,
					kind = kind,
					label = l,
					profileInfo = profileInfo,
					statements = statements,
					transfer = transfer})
	 in
	    l
	 end
      fun genCase {cases: (Con.t * Label.t) vector,
		   default: Label.t option,
		   profileInfo,
		   test: Var.t,
		   testRep: TyconRep.t}: Transfer.t =
	 let
	    fun switch {cases: Cases.t,
			default: Label.t option,
			numLeft: int,
			test: Operand.t}: Transfer.t =
	       let
		  datatype z = None | One of Label.t | Many
		  val default = if numLeft = 0 then NONE else default
		  val targets =
		     Cases.fold
		     (cases,
		      case default of
			 SOME l => One l
		       | NONE => None,
		      fn (l, Many) => Many
		       | (l, One l') => if Label.equals (l, l')
					   then One l'
					else Many
		       | (l, None) => One l)
	       in		
		  case targets of
		     None => Error.bug "no targets"
		   | One l => Goto {dst = l,
				    args = Vector.new0 ()}
		   | Many => Switch {test = test,
				     cases = cases,
				     default = default}
	       end
	    fun enum (test: Operand.t, numEnum: int): Transfer.t =
	       let
		  val (cases, numLeft) =
		     Vector.fold
		     (cases, ([], numEnum),
		      fn ((c, j), (cases, numLeft)) =>
		      let
			 fun keep n = ((n, j) :: cases, numLeft - 1)
		      in
			 case conRep c of
			    ConRep.Int n => keep n
			  | ConRep.IntCast n => keep n
			  | _ => (cases, numLeft)
		      end)
	       in switch {test = test,
			  cases = Cases.Int cases,
			  default = default,
			  numLeft = numLeft}
	       end
	    fun transferToLabel (transfer: Transfer.t): Label.t =
	       case transfer of
		  Transfer.Goto {args, dst, ...} =>
		     (Assert.assert ("transferToLabel", fn () =>
				     0 = Vector.length args)
		      ; dst)
		| _ => newBlock {args = Vector.new0 (),
				 kind = Kind.Jump,
				 profileInfo = profileInfo,
				 statements = Vector.new0 (),
				 transfer = transfer}
	    fun switchIP (numEnum, pointer: Label.t): Transfer.t =
	       Transfer.SwitchIP
	       {test = varOp test,
		int = transferToLabel (enum (CastInt test, numEnum)),
		pointer = pointer}
	    fun tail (l: Label.t, args: Operand.t vector): Label.t =
	       if 0 = Vector.length args
		  then l
	       else
		  let
		     val xs = Vector.map (args, fn _ => Var.newNoname ())
		  in
		     newBlock {args = Vector.new0 (),
			       kind = Kind.Jump,
			       profileInfo = profileInfo,
			       statements = Vector.new0 (),
			       transfer = Goto {dst = l, args = args}}
		  end
	    fun enumAndOne (numEnum: int): Transfer.t =
	       let
		  val (l, args: Operand.t vector) =
		     Vector.loop
		     (cases, fn (c, j) =>
		      case conRep c of
			 ConRep.Transparent _ =>
			    SOME (j, Vector.new1 (varOp test))
		       | ConRep.Tuple => SOME (j, conSelects (test, c))
		       | _ => NONE,
			    fn () =>
			    case default of
			       NONE =>
				  Error.bug "enumAndOne: no default"
			     | SOME j => (j, Vector.new0 ()))
	       in switchIP (numEnum, tail (l, args))
	       end
	    fun indirectTag (numTag: int): Transfer.t =
	       let
		  val (cases, numLeft) =
		     Vector.fold
		     (cases, ([], numTag),
		      fn ((c, j), (cases, numLeft)) =>
		      case conRep c of
			 ConRep.TagTuple n =>
			    ((n, tail (j, conSelects (test, c))) :: cases,
			     numLeft - 1)
		       | _ => (cases, numLeft))
	       in switch {test = Offset {base = test,
					 bytes = tagOffset,
					 ty = Type.int},
			  cases = Cases.Int cases,
			  default = default,
			  numLeft = numLeft}
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
			 | ConRep.Tuple =>
			      Goto {dst = l,
				    args = conSelects (test, c)}
			 | _ => Error.bug "strange conRep for Prim"
		     end
		| (0, SOME l) => Goto {dst = l, args = Vector.new0 ()}
		| _ => Error.bug "prim datatype with more than one case"
	 in
	    case testRep of
	       TyconRep.Prim mtype => prim ()
	     | TyconRep.Enum {numEnum} => enum (varOp test, numEnum)
	     | TyconRep.EnumDirect {numEnum} => enumAndOne numEnum
	     | TyconRep.EnumIndirect {numEnum} => enumAndOne numEnum
	     | TyconRep.EnumIndirectTag {numEnum, numTag} =>
		  switchIP (numEnum, transferToLabel (indirectTag numTag))
	     | TyconRep.IndirectTag {numTag} => indirectTag numTag
	     | TyconRep.Void => prim ()
	 end
      fun translateCase (profileInfo,
			 {test: Var.t,
			  cases: Label.t S.Cases.t,
			  default: Label.t option}): Transfer.t =
	 let
	    fun id x = x
	    fun doit (l, f, branch) =
	       Switch {test = varOp test,
		       cases = f (Vector.toListMap
				  (l, fn (i, j) => (branch i, j))),
		       default = default}
	 in
	    case cases of
	       S.Cases.Char l => doit (l, Cases.Char, id)
	     | S.Cases.Int l => doit (l, Cases.Int, id)
	     | S.Cases.Word l => doit (l, Cases.Word, id)
	     | S.Cases.Word8 l => doit (l, Cases.Char, Word8.toChar)
	     | S.Cases.Con cases =>
		  (case (Vector.length cases, default) of
		      (0, NONE) => Bug
		    | _ => 
			 let
			    val (tycon, tys) = S.Type.tyconArgs (varType test)
			 in
			    if Vector.isEmpty tys
			       then genCase {cases = cases,
					     default = default,
					     profileInfo = profileInfo,
					     test = test,
					     testRep = tyconRep tycon}
			    else Error.bug "strange type in case"
			 end)
	 end
      val {get = labelInfo: (Label.t ->
			     {args: (Var.t * S.Type.t) vector,
			      cont: (Label.t option * Label.t) list ref,
			      handler: Label.t option ref}),
	   set = setLabelInfo, ...} =
	 Property.getSetOnce (Label.plist,
			      Property.initRaise ("label info", Label.layout))
      fun eta (profileInfo, l: Label.t, kind: Kind.t): Label.t =
	 let
	    val {args, ...} = labelInfo l
	    val args = Vector.keepAllMap (args, fn (x, t) =>
					  Option.map (toType t, fn t =>
						      (Var.new x, t)))
	    val l' = Label.new l
	    val _ = 
	       List.push
	       (extraBlocks,
		Block.T {args = args,
			 kind = kind,
			 label = l',
			 profileInfo = profileInfo,
			 statements = Vector.new0 (),
			 transfer = (Transfer.Goto
				     {dst = l,
				      args = Vector.map (args, fn (var, ty) =>
							 Var {var = var,
							      ty = ty})})})
	 in
	    l'
	 end
      fun labelHandler (profileInfo, l: Label.t): Label.t =
	 let
	    val info as {handler, ...} = labelInfo l
	 in
	    case !handler of
	       NONE =>
		  let
		     val l' = eta (profileInfo, l, Kind.Handler)
		     val _ = handler := SOME l'
		  in
		     l'
		  end
	     | SOME l => l
	 end
      fun labelCont (profileInfo, l: Label.t, h: Handler.t): Label.t =
	 let
	    val info as {cont, ...} = labelInfo l
	    datatype z = datatype Handler.t
	 in
	    case List.peek (!cont, fn (h', _) =>
			    case (h, h') of
			       (CallerHandler, NONE) => true
			     | (None, NONE) => true
			     | (Handle l, SOME l') => Label.equals (l, l')
			     | _ => false) of
	       SOME (_, l) => l
	     | NONE =>
		  let
		     val handler =
			case h of
			   CallerHandler => NONE
			 | None => NONE
			 | Handle l => SOME l
		     val l' = eta (profileInfo, l, Kind.Cont {handler = handler})
		     val _ = List.push (cont, (handler, l'))
		  in
		     l'
		  end
	 end
      val labelCont =
	 Trace.trace3 ("SsaToRssa.labelCont",
		       Layout.ignore, Label.layout, Handler.layout, Label.layout)
	 labelCont
      fun vos (xs: Var.t vector) =
	 Vector.keepAllMap (xs, fn x =>
			    Option.map (toType (varType x), fn _ =>
					varOp x))
      fun translateTransfer (profileInfo, t: S.Transfer.t): Transfer.t =
	 case t of
	    S.Transfer.Arith {args, overflow, prim, success, ty} =>
	       let
		  val ty = valOf (toType ty)
		  val temp = Var.newNoname ()
		  val noOverflow =
		     newBlock
		     {args = Vector.new0 (),
		      kind = Kind.Jump,
		      profileInfo = profileInfo,
		      statements = Vector.new0 (),
		      transfer = (Transfer.Goto
				  {dst = success,
				   args = (Vector.new1
					   (Operand.Var {var = temp,
							 ty = ty}))})}
	       in
		  Transfer.Arith {dst = temp,
				  args = vos args,
				  overflow = overflow,
				  prim = prim,
				  success = noOverflow,
				  ty = ty}
	       end
	  | S.Transfer.Bug => Transfer.Bug
	  | S.Transfer.Call {func, args, return} =>
	       let
		  datatype z = datatype Return.t
		  datatype z = datatype Handler.t
		  val return =
		     case return of
			NonTail {cont, handler} =>
			   let
			      val handler = Handler.map 
				            (handler, fn handler =>
					     labelHandler (profileInfo, handler))
			   in
			      NonTail {cont = labelCont (profileInfo, cont, handler),
				       handler = handler}
			   end
		      | _ => return
	       in
		  Transfer.Call {func = func,
				 args = vos args,
				 return = return}
	       end
	  | S.Transfer.Case r => translateCase (profileInfo, r)
	  | S.Transfer.Goto {dst, args} =>
	       Transfer.Goto {dst = dst, args = vos args}
	  | S.Transfer.Raise xs => Transfer.Raise (vos xs)
	  | S.Transfer.Return xs => Transfer.Return (vos xs)
	  | S.Transfer.Runtime {args, prim, return} =>
	       Transfer.Runtime {args = vos args,
				 prim = prim,
				 return = eta (profileInfo,
					       return,
					       Kind.Runtime {prim = prim})}
      fun translateFormals v =
	 Vector.keepAllMap (v, fn (x, t) =>
			    Option.map (toType t, fn t => (x, t)))
      fun translateStatementsTransfer (profileInfo, statements, transfer) =
	 let
	    fun loop (i, ss, t): Statement.t vector * Transfer.t =
	       if i < 0
		  then (Vector.fromList ss, t)
	       else
		  let
		     val S.Statement.T {var, ty, exp} =
			Vector.sub (statements, i)
		     fun none () = loop (i - 1, ss, t)
		     fun add s = loop (i - 1, s :: ss, t)
		     fun split (args, kind,
				ss: Statement.t list,
				make: Label.t -> Statement.t list * Transfer.t) =
			let
			   val l = newBlock {args = args,
					     kind = kind,
					     profileInfo = profileInfo,
					     statements = Vector.fromList ss,
					     transfer = t}
			   val (ss, t) = make l
			in
			   loop (i - 1, ss, t)
			end
		     fun makeStores (ys: Var.t vector, offsets) =
			Vector.keepAllMap2
			(ys, offsets, fn (y, offset) =>
			 Option.map (offset, fn {offset, ty} =>
				     {offset = offset,
				      value = varOp y}))
		     fun allocate (ys: Var.t vector,
				   {size, offsets, numPointers,
				    numWordsNonPointers}) =
			let
			   val (p, np) =
			      if 0 = numPointers
				 andalso 0 = numWordsNonPointers
				 then (0, 1)
			      else (numPointers, numWordsNonPointers)
			in
			   add (Object {dst = valOf var,
					numPointers = p,
					numWordsNonPointers = np,
					stores = makeStores (ys, offsets)})
			end
		     fun allocateTagged (n: int,
					 ys: Var.t vector,
					 {size, offsets,
					  numPointers, numWordsNonPointers}) =
			add (Object
			     {dst = valOf var,
			      numPointers = numPointers,
			      numWordsNonPointers =
			      (* for the tag *) 1 + numWordsNonPointers,
			      stores = (Vector.concat
					[Vector.new1 {offset = tagOffset,
						      value = Operand.int n},
					 makeStores (ys, offsets)])})
		     fun move (oper: Operand.t) =
			add (Bind {isMutable = false,
				   oper = oper,
				   var = valOf var})
		  in
		     case exp of
			S.Exp.ConApp {con, args} =>
			   (case conRep con of
			       ConRep.Void => none ()
			     | ConRep.Int n => move (Operand.int n)
			     | ConRep.IntCast n => move (Operand.Pointer n)
			     | ConRep.TagTuple n =>
				  allocateTagged (n, args, #info (conInfo con))
			     | ConRep.Transparent _ =>
				  move (varOp (Vector.sub (args, 0)))
			     | ConRep.Tuple =>
				  allocate (args, #info (conInfo con)))
		      | S.Exp.Const c => move (Operand.Const c)
		      | S.Exp.PrimApp {prim, targs, args, ...} =>
			   let
			      fun a i = Vector.sub (args, i)
			      fun targ () = toType (Vector.sub (targs, 0))
			      fun arrayOffset (ty: Type.t): Operand.t =
				 ArrayOffset {base = a 0,
					      index = a 1,
					      ty = ty}
			      fun sub (ty: Type.t) = move (arrayOffset ty)
			      fun dst () =
				 case var of
				    SOME x =>
				       Option.map (toType (varType x), fn t =>
						   (x, t))
				  | NONE => NONE
			      fun normal () =
				 add (PrimApp {dst = dst (),
					       prim = prim,
					       args = varOps args})
			      fun array0 () =
				 add (Array0 {dst = valOf var})
			      datatype z = datatype Prim.Name.t
			      fun bumpCanHandle n =
				 let
				    val canHandle =
				       Operand.Runtime RuntimeOperand.CanHandle
				    val res = Var.newNoname ()
				 in
				    [Statement.PrimApp
				     {args = Vector.new2 (canHandle,
							  Operand.int n),
				      dst = SOME (res, Type.int),
				      prim = Prim.intAdd},
				     Statement.Move
				     {dst = canHandle,
				      src = Operand.Var {var = res,
							 ty = Type.int}}]
				 end
			   in
			      if Prim.impCall prim
				 then
				    let
				       val (formals, returnTy) =
					    case dst () of
					       NONE => (Vector.new0 (), NONE)
					     | SOME (x, t) =>
						  (Vector.new1 (x, t), SOME t)
				    in
				       split
				       (formals,
					Kind.CReturn {prim = prim},
					ss,
					fn l =>
					([],
					 Transfer.CCall {args = vos args,
							 prim = prim,
							 return = l,
							 returnTy = returnTy}))
				    end
			      else if Prim.entersRuntime prim
				 then
				    split
				    (Vector.new0 (),
				     Kind.Runtime {prim = prim},
				     ss,
				     fn l =>
				     ([], Transfer.Runtime {args = vos args,
							    prim = prim,
							    return = l}))
			      else
				 case Prim.name prim of
				    Array_array =>
  (case targ () of
      NONE => array0 ()
    | SOME t =>
	 let
	    val (nbnp, np, bytesPerElt) =
	       if Type.isPointer t
		  then (0, 1, Runtime.pointerSize)
	       else
		  let val n = Type.size t
		  in (n, 0, n)
		  end
	 in
	    if 0 = np andalso 0 = nbnp
	       then array0 ()
	    else
	       let
		  val numElts = a 0
		  val (numBytes, numElts, continue) =
		     case varInt numElts of
			SOME n =>
			   (* Compute the number of bytes in the array now, since
			    * the number of elements is a known constant.
			    *)
			   let
			      val numBytes =
				 Runtime.wordAlign
				 (MLton.Word.mulCheck (Word.fromInt n,
						       Word.fromInt bytesPerElt))
				 handle Overflow => Runtime.allocTooLarge
			   in
			      (Operand.word numBytes,
			       Operand.int n,
			       fn alloc =>
			       ([], Goto {args = Vector.new0 (),
					  dst = alloc}))
			   end 
		      | NONE =>
			   let
			      val numEltsOp =
				 Operand.Var {var = numElts, ty = Type.int}
			      val numBytes = Var.newNoname ()
			      val numEltsWord = Var.newNoname ()
			      val numEltsWordOp =
				 Operand.Var {var = numEltsWord, ty = Type.word}
			      val conv =
				 PrimApp {args = Vector.new1 numEltsOp,
					  dst = SOME (numEltsWord, Type.word),
					  prim = Prim.word32FromInt}
			   in
			      (Operand.Var {var = numBytes, ty = Type.word},
			       numEltsOp,
			       fn alloc =>
			       if 1 = nbnp
				  then
				     let
					val numBytesWord = Var.newNoname ()
					val numEltsP3 = Var.newNoname ()
				     in
					([conv,
					  PrimApp
					  {args = (Vector.new2 (Operand.word 0w3,
								numEltsWordOp)),
					   dst = SOME (numEltsP3, Type.word),
					   prim = Prim.word32Add},
					  PrimApp
					  {args = (Vector.new2
						   (Operand.word (Word.notb 0w3),
						    Operand.Var {var = numEltsP3,
								 ty = Type.word})),
					   dst = SOME (numBytes, Type.word),
					   prim = Prim.word32Andb}],
					 Goto {args = Vector.new0 (),
					       dst = alloc})
				     end
			       else
				  ([conv],
				   Transfer.Arith
				   {args = Vector.new2 (Operand.word
							(Word.fromInt bytesPerElt),
							numEltsWordOp),
				    dst = numBytes,
				    overflow = allocTooLarge (),
				    prim = Prim.word32MulCheck,
				    success = alloc,
				    ty = Type.word}))
			   end
	       in
		  split (Vector.new0 (), Kind.Jump,
			 Array {dst = valOf var,
				numBytes = numBytes,
				numBytesNonPointers = nbnp,
				numElts = numElts,
				numPointers = np}
			 :: ss,
			 continue)
	       end
	 end)
				  | Array_array0 => array0 ()
				  | Array_sub =>
				       (case targ () of
					   NONE => none ()
					 | SOME t => sub t)
				  | Array_update =>
				       (case targ () of
					   NONE => none ()
					 | SOME t =>
					      add (Move {dst = arrayOffset t,
							 src = varOp (a 2)}))
				  | MLton_bogus =>
				       (case toType ty of
					   NONE => none ()
					 | SOME t =>
					      let
						 val c = Operand.Const
					      in
						 move
						 (case Type.dest t of
						     Type.Char =>
							c (Const.fromChar #"\000")
						   | Type.Double =>
							c (Const.fromReal "0.0")
						   | Type.Int =>
							c (Const.fromInt 0)
						   | Type.Pointer =>
							Operand.Pointer 1
						   | Type.Uint =>
							c (Const.fromWord 0w0))
					      end)
				  | MLton_eq =>
				       (case targ () of
					   NONE => move (Operand.int 1)
					 | SOME _ => normal ())
				  | Ref_assign =>
				       (case targ () of
					   NONE => none ()
					 | SOME ty =>
					      add
					      (Move {dst = Offset {base = a 0,
								   bytes = 0,
								   ty = ty},
						     src = varOp (a 1)}))
				  | Ref_deref =>
				       (case targ () of
					   NONE => none ()
					 | SOME ty =>
					      move (Offset {base = a 0,
							    bytes = 0,
							    ty = ty}))
				  | Ref_ref =>
				       let
					  val (ys, ts) =
					     case targ () of
						NONE => (Vector.new0 (),
							 Vector.new0 ())
					      | SOME t => (Vector.new1 (a 0),
							   Vector.new1 (SOME t))
				       in allocate (ys, sortTypes (0, ts))
				       end
				  | String_sub => sub Type.char
				  | Thread_atomicBegin =>
				       (* assert(gcState.canHandle >= 0);
					* gcState.canHandle++;
					* if (gcState.signalIsPending)
					*         setLimit(&gcState);
					*)
				       split
				       (Vector.new0 (), Kind.Jump, ss, fn l =>
					let
					   fun doit (dst, prim, a, b) =
					      let
						 val tmp = Var.newNoname ()
					      in
						 Vector.new2
						 (Statement.PrimApp
						  {args = Vector.new2 (a, b),
						   dst = SOME (tmp, Type.word),
						   prim = prim},
						  Statement.Move
						  {dst = Operand.Runtime dst,
						   src = (Operand.Var
							  {var = tmp,
							   ty = Type.word})})
					      end
					   datatype z = datatype RuntimeOperand.t
					   val statements =
					      Vector.concat
					      [doit (LimitPlusSlop,
						     Prim.word32Add,
						     Operand.Runtime Base,
						     Operand.Runtime FromSize),
					       doit (Limit,
						     Prim.word32Sub,
						     Operand.Runtime LimitPlusSlop,
						     Operand.word
						     (Word.fromInt
						      Runtime.limitSlop))]
					   val l' =
					      newBlock
					      {args = Vector.new0 (),
					       kind = Kind.Jump,
					       profileInfo = profileInfo,
					       statements = statements,
					       transfer = (Transfer.Goto
							   {args = Vector.new0 (),
							    dst = l})}
					in
					   (bumpCanHandle 1,
					    Transfer.iff
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
					   datatype z = datatype RuntimeOperand.t
					   val statements =
					      Vector.new1
					      (Statement.Move
					       {dst = Operand.Runtime Limit,
						src = Operand.word 0w0})
					   val l'' =
					      newBlock
					      {args = Vector.new0 (),
					       kind = Kind.Jump,
					       profileInfo = profileInfo,
					       statements = statements,
					       transfer =
					       Transfer.Goto
					       {args = Vector.new0 (),
						dst = l}}
					   val l' =
					      newBlock
					      {args = Vector.new0 (),
					       kind = Kind.Jump,
					       profileInfo = profileInfo,
					       statements = Vector.new0 (),
					       transfer =
					       Transfer.iff
					       (Operand.Runtime CanHandle,
						{truee = l,
						 falsee = l''})}
					in
					   (bumpCanHandle ~1,
					    Transfer.iff
					    (Operand.Runtime SignalIsPending,
					     {falsee = l,
					      truee = l'}))
					end)
				  | Thread_canHandle =>
				       move (Operand.Runtime
					     RuntimeOperand.CanHandle)
				  | Vector_fromArray => move (varOp (a 0))
				  | Vector_sub =>
				       (case targ () of
					   NONE => none ()
					 | SOME t => sub t)
				  | _ => normal ()
			   end
		      | S.Exp.Select {tuple, offset} =>
			   (case Vector.sub (#offsets (tupleInfo (varType tuple)),
					     offset) of
			       NONE => none ()
			     | SOME {offset, ty} =>
				  move (Offset {base = tuple,
						bytes = offset,
						ty = ty}))
		      | S.Exp.SetExnStackLocal => add SetExnStackLocal
		      | S.Exp.SetExnStackSlot => add SetExnStackSlot
		      | S.Exp.SetHandler h => 
			   add (SetHandler (labelHandler (profileInfo, h)))
		      | S.Exp.SetSlotExnStack => add SetSlotExnStack
		      | S.Exp.Tuple ys => allocate (ys, tupleInfo ty)
		      | S.Exp.Var y => move (varOp y)
		      | _ => Error.bug "translateStatement saw strange PrimExp"
		  end
	 in
	    loop (Vector.length statements - 1, [], transfer)
	 end
      fun translateBlock (profileInfo,
			  S.Block.T {label, args, statements, transfer}) =
	 let
	    val profileInfo = profileInfo label
	    val (ss, t) =
	       translateStatementsTransfer (profileInfo,
					    statements,
					    translateTransfer (profileInfo, transfer))
	 in
	    Block.T {args = translateFormals args,
		     kind = Kind.Jump,
		     label = label,
		     profileInfo = profileInfo,
		     statements = ss,
		     transfer = t}
	 end
      fun translateFunction (f: S.Function.t): Function.t =
	 let
	    val _ = resetAllocTooLarge ()
	    val _ =
	       S.Function.foreachVar (f, fn (x, t) => setVarInfo (x, {ty = t}))
	    val {args, blocks, name, start, ...} = S.Function.dest f
	    val _ =
	       Vector.foreach
	       (blocks, fn S.Block.T {label, args, ...} =>
		setLabelInfo (label, {args = args,
				      cont = ref [],
				      handler = ref NONE}))
	    val profileInfoFunc = Func.toString name
	    fun profileInfo label =
	       {ssa = {func = profileInfoFunc,
		       label = Label.toString label}}
	    val blocks = Vector.map (blocks, fn block =>
				     translateBlock (profileInfo, block))
	    val blocks = Vector.concat [Vector.fromList (!extraBlocks), blocks]
	    val _ = extraBlocks := []
	 in
	    Function.new {args = translateFormals args,
			  blocks = blocks,
			  name = name,
			  start = start}
	 end
      val main =
	  let
	     val start = Label.newNoname ()
	  in
	     translateFunction
	     (S.Function.new
	      {args = Vector.new0 (),
	       blocks = (Vector.new1
			 (S.Block.T
			  {label = start,
			   args = Vector.new0 (),
			   statements = globals,
			   transfer = S.Transfer.Call {func = main,
						       args = Vector.new0 (),
						       return = Return.Dead}})),
	       name = Func.newNoname (),
	       raises = NONE,
	       returns = NONE,
	       start = start})
	  end
      val functions = List.revMap (functions, translateFunction)
      val p = Program.T {functions = functions,
			 main = main}
      val _ = Program.clear p
   in
      p
   end

end
