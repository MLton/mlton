(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor Backend (S: BACKEND_STRUCTS): BACKEND =
struct

open S

structure M = Machine
local
   open Machine
in
   structure Chunk = Chunk
   structure Global = Global
   structure Label = Label
   structure MemChunk = MemChunk
   structure ObjectType = ObjectType
   structure PointerTycon = PointerTycon
   structure Register = Register
   structure Runtime = Runtime
   structure Type = Type
end
local
   open Runtime
in
   structure CFunction = CFunction
   structure GCField = GCField
end
val wordSize = Runtime.wordSize

structure Rssa = Rssa (open Ssa Machine)
structure R = Rssa
local
   open Rssa
in
   structure Const = Const
   structure Func = Func
   structure Function = Function
   structure Prim = Prim
   structure Type = Type
   structure Var = Var
end 

structure ProfileAlloc = ProfileAlloc (structure Rssa = Rssa)
structure AllocateRegisters = AllocateRegisters (structure Machine = Machine
						 structure Rssa = Rssa)
structure Chunkify = Chunkify (Rssa)
structure LimitCheck = LimitCheck (structure Rssa = Rssa)
structure ParallelMove = ParallelMove ()
structure SignalCheck = SignalCheck(structure Rssa = Rssa)
structure SsaToRssa = SsaToRssa (structure Rssa = Rssa
				 structure Ssa = Ssa)

nonfix ^
fun ^ r = valOf (!r)

structure VarOperand =
   struct
      datatype t =
	 Allocate of {operand: M.Operand.t option ref}
       | Const of M.Operand.t

      fun layout i =
	 let
	    open Layout
	 in
	    case i of
	       Allocate {operand, ...} =>
		  seq [str "Allocate ",
		       record [("operand",
				Option.layout M.Operand.layout (!operand))]]
	     | Const oper => seq [str "Const ", M.Operand.layout oper]
	 end

      val operand: t -> M.Operand.t =
	 fn Allocate {operand, ...} => ^operand
	  | Const oper => oper
   end

structure IntSet = UniqueSet (val cacheSize: int = 1
			      val bits: int = 14
			      structure Element =
				 struct
				    open Int
				    fun hash n = Word.fromInt n
				 end)

structure Chunk =
   struct
      datatype t = T of {blocks: M.Block.t list ref,
			 chunkLabel: M.ChunkLabel.t}

      fun label (T {chunkLabel, ...}) = chunkLabel
	 
      fun equals (T {blocks = r, ...}, T {blocks = r', ...}) = r = r'
	 
      fun new (): t =
	 T {blocks = ref [],
	    chunkLabel = M.ChunkLabel.new ()}
	 
      fun newBlock (T {blocks, ...}, z) =
	 List.push (blocks, M.Block.T z)
   end

val traceGenBlock =
   Trace.trace ("Backend.genBlock",
		Label.layout o R.Block.label,
		Unit.layout)

fun eliminateDeadCode (f: R.Function.t): R.Function.t =
   let
      val {args, blocks, name, returns, raises, start} = R.Function.dest f
      val {get, set, ...} =
	 Property.getSetOnce (Label.plist, Property.initConst false)
      val get = Trace.trace ("Backend.labelIsReachable",
			     Label.layout,
			     Bool.layout) get
      val _ =
	 R.Function.dfs (f, fn R.Block.T {label, ...} =>
			 (set (label, true)
			  ; fn () => ()))
      val blocks =
	 Vector.keepAll (blocks, fn R.Block.T {label, ...} => get label)
   in
      R.Function.new {args = args,
		      blocks = blocks,
		      name = name,
		      returns = returns,
		      raises = raises,
		      start = start}
   end

fun toMachine (program: Ssa.Program.t) =
   let
      fun pass (name, doit, program) =
	 Control.passTypeCheck {display = Control.Layouts Rssa.Program.layouts,
				name = name,
				style = Control.No,
				suffix = "rssa",
				thunk = fn () => doit program,
				typeCheck = R.Program.typeCheck}
      val program = pass ("ssaToRssa", SsaToRssa.convert, program)
      val program = pass ("insertLimitChecks", LimitCheck.insert, program)
      val program = pass ("insertSignalChecks", SignalCheck.insert, program)
      val program =
	 if !Control.profile = Control.ProfileAlloc
	    then pass ("profileAlloc", ProfileAlloc.doit, program)
	 else program
      val _ =
	 let
	    open Control
	 in
	    if !keepRSSA
	       then saveToFile ({suffix = "rssa"},
				No,
				program,
				Layouts Rssa.Program.layouts)
	    else ()
	 end
      val program as R.Program.T {functions, main, objectTypes,
				  profileAllocLabels} = program
      val handlesSignals = Rssa.Program.handlesSignals program
      (* Chunk information *)
      val {get = labelChunk, set = setLabelChunk, ...} =
	 Property.getSetOnce (Label.plist,
			      Property.initRaise ("labelChunk", Label.layout))
      val {get = funcChunk: Func.t -> Chunk.t, set = setFuncChunk, ...} =
	 Property.getSetOnce (Func.plist,
			      Property.initRaise ("funcChunk", Func.layout))
      val funcChunkLabel = Chunk.label o funcChunk
      val chunks = ref []
      fun newChunk () =
	 let
	    val c = Chunk.new ()
	    val _ = List.push (chunks, c)
	 in
	    c
	 end
      val handlers = ref []
      val frames: {chunkLabel: M.ChunkLabel.t,
		   offsets: int list,
		   return: Label.t,
		   size: int} list ref = ref []
      (* Set funcChunk and labelChunk. *)
      val _ =
	 Vector.foreach
	 (Chunkify.chunkify program, fn {funcs, labels} =>
	  let 
	     val c = newChunk ()
	     val _ = Vector.foreach (funcs, fn f => setFuncChunk (f, c))
	     val _ = Vector.foreach (labels, fn l => setLabelChunk (l, c))
	  in
	     ()
	  end)
      (* The global raise operands. *)
      local
	 val table: (Type.t vector * M.Operand.t vector) list ref = ref []
      in
	 fun raiseOperands (ts: Type.t vector): M.Operand.t vector =
	    case List.peek (!table, fn (ts', _) =>
			    Vector.equals (ts, ts', Type.equals)) of
	       NONE =>
		  let
		     val gs =
			Vector.map (ts, fn ty =>
				    M.Operand.Global
				    (Global.new {isRoot = false,
						 ty = ty}))
		     val _ = List.push (table, (ts, gs))
		  in
		     gs
		  end
	     | SOME (_, gs) => gs
      end
      val {get = varInfo: Var.t -> {operand: VarOperand.t,
				    ty: Type.t},
	   set = setVarInfo, ...} =
	 Property.getSetOnce (Var.plist,
			      Property.initRaise ("Backend.info", Var.layout))
      val setVarInfo =
	 Trace.trace2 ("Backend.setVarInfo",
		       Var.layout, VarOperand.layout o #operand, Unit.layout)
	 setVarInfo
      val varInfo =
	 Trace.trace ("Backend.varInfo",
		      Var.layout,
		      fn {operand, ...} =>
		      Layout.record [("operand", VarOperand.layout operand)])
	 varInfo
      val varOperand: Var.t -> M.Operand.t =
	 VarOperand.operand o #operand o varInfo
      fun varOperands xs = Vector.map (xs, varOperand)
      (* Hash tables for uniquifying globals. *)
      local
	 fun 'a make (ty: Type.t, toString: 'a -> string) =
	    let
	       val set: {global: M.Global.t,
			 hash: word,
			 string: string} HashSet.t = HashSet.new {hash = #hash}
	       fun get (a: 'a): M.Operand.t =
		  let
		     val s = toString a
		     val hash = String.hash s
		  in
		     M.Operand.Global
		     (#global
		      (HashSet.lookupOrInsert
		       (set, hash, fn {string, ...} => s = string,
			fn () => {hash = hash,
				  global = M.Global.new {isRoot = true,
							 ty = ty},
				  string = s})))
		  end
	       fun all () =
		  HashSet.fold
		  (set, [], fn ({global, string, ...}, ac) =>
		   (global, string) :: ac)
	    in
	       (all, get)
	    end
      in
	 val (allIntInfs, globalIntInf) =
	    make (Type.intInf, fn i => IntInf.format (i, StringCvt.DEC))
	 val (allReals, globalReal) = make (Type.real, fn s => s)
	 val (allStrings, globalString) = make (Type.string, fn s => s)
	 fun constOperand (c: Const.t): M.Operand.t =
	    let
	       datatype z = datatype Const.Node.t
	    in
	       case Const.node c of
		  Char n => M.Operand.Char n
		| Int n => M.Operand.Int n
		| IntInf i =>
		     (case Const.SmallIntInf.toWord i of
			 NONE => globalIntInf i
		       | SOME w => M.Operand.SmallIntInf w)
		| Real f =>
		     if !Control.Native.native
			then globalReal f
		     else M.Operand.Real f
		| String s => globalString s
		| Word w =>
		     let val ty = Const.ty c
		     in if Const.Type.equals (ty, Const.Type.word)
			   then M.Operand.Word w
			else if Const.Type.equals (ty, Const.Type.word8)
				then M.Operand.Char (Char.chr (Word.toInt w))
			     else Error.bug "strange word"
		     end
	    end
      end
      fun parallelMove {chunk,
			dsts: M.Operand.t vector,
			srcs: M.Operand.t vector}: M.Statement.t vector =
	 let
	    val moves =
	       Vector.fold2 (srcs, dsts, [],
			     fn (src, dst, ac) => {src = src, dst = dst} :: ac)
	    fun temp r =
	       M.Operand.Register (Register.new (M.Operand.ty r, NONE))
	 in
	    Vector.fromList
	    (ParallelMove.move {
				equals = M.Operand.equals,
				move = M.Statement.move,
				moves = moves,
				interfere = M.Operand.interfere,
				temp = temp
				})
	 end
      fun translateOperand (oper: R.Operand.t): M.Operand.t =
	 let
	    datatype z = datatype R.Operand.t
	 in
	    case oper of
	       ArrayOffset {base, index, ty} =>
		  M.Operand.ArrayOffset {base = translateOperand base,
					 index = translateOperand index,
					 ty = ty}
	     | Cast (z, t) => M.Operand.Cast (translateOperand z, t)
	     | Const c => constOperand c
	     | EnsuresBytesFree =>
		  Error.bug "backend translateOperand saw EnsuresBytesFree"
	     | File => M.Operand.File
	     | GCState => M.Operand.GCState
	     | Line => M.Operand.Line
	     | Offset {base, offset, ty} =>
		  M.Operand.Offset {base = translateOperand base,
				    offset = offset,
				    ty = ty}
	     | PointerTycon pt =>
		  M.Operand.Word (Runtime.typeIndexToHeader
				  (PointerTycon.index pt))
	     | Runtime r => M.Operand.Runtime r
	     | SmallIntInf w => M.Operand.SmallIntInf w
	     | Var {var, ...} => varOperand var
	 end
      fun translateOperands ops = Vector.map (ops, translateOperand)
      fun genStatement (s: R.Statement.t,
			handlerLinkOffset: {handler: int,
					    link: int} option)
	 : M.Statement.t vector =
	 let
	    fun handlerOffset () = #handler (valOf handlerLinkOffset)
	    fun linkOffset () = #link (valOf handlerLinkOffset)
	    datatype z = datatype R.Statement.t
	 in
	    case s of
               Bind {isMutable, oper, var} =>
		  if isMutable
		     orelse (case #operand (varInfo var) of
				VarOperand.Const _ => false
			      | _ => true)
		     then (Vector.new1
			   (M.Statement.move {dst = varOperand var,
					      src = translateOperand oper}))
		  else Vector.new0 ()
	     | Move {dst, src} =>
		  Vector.new1
		  (M.Statement.move {dst = translateOperand dst,
				     src = translateOperand src})
	     | Object {dst, size, stores, tycon, ...} =>
		  Vector.new1
		  (M.Statement.Object
		   {dst = varOperand dst,
		    header = (Runtime.typeIndexToHeader
			      (PointerTycon.index tycon)),
		    size = size,
		    stores = Vector.map (stores, fn {offset, value} =>
					 {offset = offset,
					  value = translateOperand value})})
	     | PrimApp {dst, prim, args} =>
		  let
		     datatype z = datatype Prim.Name.t
		  in
		     case Prim.name prim of
			MLton_installSignalHandler => Vector.new0 ()
		      | _ => 
			   Vector.new1
			   (M.Statement.PrimApp
			    {args = translateOperands args,
			     dst = Option.map (dst, varOperand o #1),
			     prim = prim})
		  end
	     | SetExnStackLocal =>
		  Vector.new1
		  (M.Statement.SetExnStackLocal {offset = handlerOffset ()})
	     | SetExnStackSlot =>
		  Vector.new1
		  (M.Statement.SetExnStackSlot {offset = linkOffset ()})
	     | SetHandler h =>
		  Vector.new1
		  (M.Statement.move
		   {dst = M.Operand.StackOffset {offset = handlerOffset (),
						 ty = Type.label},
		    src = M.Operand.Label h})
	     | SetSlotExnStack =>
		  Vector.new1
		  (M.Statement.SetSlotExnStack {offset = linkOffset ()})
	 end
      val genStatement =
	 Trace.trace ("Backend.genStatement",
		      R.Statement.layout o #1, Vector.layout M.Statement.layout)
	 genStatement
      val bugTransfer =
	 M.Transfer.CCall
	 {args = (Vector.new1
		  (globalString "backend thought control shouldn't reach here")),
	  frameInfo = NONE,
	  func = CFunction.bug,
	  return = NONE}
      val {get = labelInfo: Label.t -> {args: (Var.t * Type.t) vector},
	   set = setLabelInfo, ...} =
	 Property.getSetOnce
	 (Label.plist, Property.initRaise ("labelInfo", Label.layout))
      val setLabelInfo =
	 Trace.trace2 ("Backend.setLabelInfo",
		       Label.layout, Layout.ignore, Unit.layout)
	 setLabelInfo
      fun callReturnOperands (xs: 'a vector,
			      ty: 'a -> Type.t,
			      shift: int): M.Operand.t vector =
	 #1 (Vector.mapAndFold
	     (xs, 0,
	      fn (x, offset) =>
	      let
		 val ty = ty x
		 val offset = Type.align (ty, offset)
	      in
		 (M.Operand.StackOffset {offset = shift + offset, 
					 ty = ty},
		  offset + Type.size ty)
	      end))
      fun genFunc (f: Function.t, isMain: bool): unit =
	 let
	    val f = eliminateDeadCode f
	    val {args, blocks, name, raises, returns, start, ...} =
	       Function.dest f
	    val raises = Option.map (raises, fn ts => raiseOperands ts)
	    val returns =
	       Option.map (returns, fn ts =>
			   callReturnOperands (ts, fn t => t, 0))
	    val chunk = funcChunk name
	    fun labelArgOperands (l: R.Label.t): M.Operand.t vector =
	       Vector.map (#args (labelInfo l), varOperand o #1)
	    fun newVarInfo (x, ty: Type.t) =
	       let
		  val operand =
		     if isMain
			then VarOperand.Const (M.Operand.Global
					       (M.Global.new {isRoot = true,
							      ty = ty}))
		     else VarOperand.Allocate {operand = ref NONE}
	       in
		  setVarInfo (x, {operand = operand,
				  ty = ty})
	       end
	    fun newVarInfos xts = Vector.foreach (xts, newVarInfo)
	    (* Set the constant operands, labelInfo, and varInfo. *)
	    val _ = newVarInfos args
	    val _ =
	       Rssa.Function.dfs
	       (f, fn R.Block.T {args, label, statements, transfer, ...} =>
		let
		   val _ = setLabelInfo (label, {args = args})
		   val _ = newVarInfos args
		   val _ =
		      Vector.foreach
		      (statements, fn s =>
		       let
			  fun normal () = R.Statement.foreachDef (s, newVarInfo)
		       in
			  case s of
			     R.Statement.Bind {isMutable, oper, var} =>
				if isMutable
				   then normal ()
				else
				   let
				      fun set (z: M.Operand.t,
					       casts: Type.t list) =
					 let
					    val z =
					       List.fold
					       (casts, z, fn (t, z) =>
						M.Operand.Cast (z, t))
					 in
					    setVarInfo
					    (var, {operand = VarOperand.Const z,
						   ty = M.Operand.ty z})
					 end
				      fun loop (z: R.Operand.t, casts) =
					 case z of
					    R.Operand.Cast (z, t) =>
					       loop (z, t :: casts)
					  | R.Operand.Const c =>
					       set (constOperand c, casts)
					  | R.Operand.Var {var = var', ...} =>
					       (case #operand (varInfo var') of
						   VarOperand.Const z =>
						      set (z, casts)
						 | VarOperand.Allocate _ =>
						      normal ())
					  | _ => normal ()
				   in
				      loop (oper, [])
				   end
			   | _ => normal ()
		       end)
		   val _ = R.Transfer.foreachDef (transfer, newVarInfo)
		in
		   fn () => ()
		end)
	    (* Allocate stack slots. *)
	    local
	       val varInfo =
		  fn x =>
		  let
		     val {operand, ty, ...} = varInfo x
		  in
		     {operand = (case operand of
				    VarOperand.Allocate {operand, ...} =>
				       SOME operand
				  | _ => NONE),
		      ty = ty}
		  end
	    in
	       val {handlerLinkOffset, labelInfo = labelRegInfo, ...} =
		  AllocateRegisters.allocate
		  {argOperands = callReturnOperands (args, #2, 0),
		   function = f,
		   varInfo = varInfo}
	    end
	    val profileInfoFunc = Func.toString name
	    (* ------------------------------------------------- *)
	    (*                    genTransfer                    *)
	    (* ------------------------------------------------- *)
	    fun genTransfer (t: R.Transfer.t,
			     chunk: Chunk.t,
			     label: Label.t)
	       : M.Statement.t vector * M.Transfer.t =
	       let
		  fun simple t = (Vector.new0 (), t)
	       in
		  case t of
		     R.Transfer.Arith {args, dst, overflow, prim, success, ty} =>
			simple
			(M.Transfer.Arith {args = translateOperands args,
					   dst = varOperand dst,
					   overflow = overflow,
					   prim = prim,
					   success = success,
					   ty = ty})
		   | R.Transfer.CCall {args, func, return} =>
			simple (M.Transfer.CCall
				{args = translateOperands args,
				 frameInfo = if CFunction.mayGC func
						then SOME M.FrameInfo.bogus
					     else NONE,
				 func = func,
				 return = return})
		   | R.Transfer.Call {func, args, return} =>
			let
			   val (contLive, frameSize, return) =
			      case return of
				 R.Return.Dead =>
				    (Vector.new0 (), 0, NONE)
			       | R.Return.Tail =>
				    (Vector.new0 (), 0, NONE)
			       | R.Return.HandleOnly =>
				    (Vector.new0 (), 0, NONE)
			       | R.Return.NonTail {cont, handler} =>
				    let
				       val {liveNoFormals, size, ...} =
					  labelRegInfo cont
				       val handler =
					  case handler of
					     R.Handler.CallerHandler => NONE
					   | R.Handler.None => NONE
					   | R.Handler.Handle h => SOME h
				    in
				       (liveNoFormals,
					size, 
					SOME {return = cont,
					      handler = handler,
					      size = size})
				    end
			   val dsts =
			      callReturnOperands
			      (args, R.Operand.ty, frameSize)
			   val setupArgs =
			      parallelMove
			      {chunk = chunk,
			       dsts = dsts,
			       srcs = translateOperands args}
			   val chunk' = funcChunk func
			   val transfer =
			      M.Transfer.Call
			      {label = funcToLabel func,
			       live = Vector.concat [contLive, dsts],
			       return = return}
			in
			   (setupArgs, transfer)
			end
		   | R.Transfer.Goto {dst, args} =>
			(parallelMove {srcs = translateOperands args,
				       dsts = labelArgOperands dst,
				       chunk = labelChunk dst},
			 M.Transfer.Goto dst)
		   | R.Transfer.Raise srcs =>
			(M.Statement.moves
			 {dsts = (raiseOperands
				  (Vector.map (srcs, R.Operand.ty))),
			  srcs = translateOperands srcs},
			 M.Transfer.Raise)
		   | R.Transfer.Return xs =>
			let
			   val dsts =
			      callReturnOperands (xs, R.Operand.ty, 0)
			in
			   (parallelMove
			    {chunk = chunk,
			     dsts = dsts,
			     srcs = translateOperands xs},
			    M.Transfer.Return)
			end
		   | R.Transfer.Switch switch =>
			let
			   fun doit ({cases: ('a * Label.t) vector,
				      default: Label.t option,
				      test: R.Operand.t},
				     make: {cases: ('a * Label.t) vector,
					    default: Label.t option,
					    test: M.Operand.t} -> M.Switch.t) =
			      simple
			      (case (Vector.length cases, default) of
				  (0, NONE) => bugTransfer
				| (1, NONE) =>
				     M.Transfer.Goto (#2 (Vector.sub (cases, 0)))
				| (0, SOME dst) => M.Transfer.Goto dst
				| _ =>
				     M.Transfer.Switch
				     (make {cases = cases,
					    default = default,
					    test = translateOperand test}))
			in
			   case switch of
			      R.Switch.Char z => doit (z, M.Switch.Char)
			    | R.Switch.EnumPointers {enum, pointers, test} =>
			         simple
			         (M.Transfer.Switch
				  (M.Switch.EnumPointers
				   {enum = enum,
				    pointers = pointers,
				    test = translateOperand test}))
			    | R.Switch.Int z => doit (z, M.Switch.Int)
			    | R.Switch.Pointer {cases, default, tag, test} =>
				 simple
				 (M.Transfer.Switch
				  (M.Switch.Pointer
				   {cases = (Vector.map
					     (cases, fn {dst, tag, tycon} =>
					      {dst = dst,
					       tag = tag,
					       tycon = tycon})),
				    default = default,
				    tag = translateOperand tag,
				    test = translateOperand test}))
			    | R.Switch.Word z => doit (z, M.Switch.Word)
			end
	       end
	    val genTransfer =
	       Trace.trace ("Backend.genTransfer",
			    R.Transfer.layout o #1,
			    Layout.tuple2 (Vector.layout M.Statement.layout,
					   M.Transfer.layout))
	       genTransfer
	    fun genBlock (R.Block.T {args, kind, label, profileInfo,
				     statements, transfer,
				     ...}) : unit =
	       let
		  val _ =
		     if Label.equals (label, start)
			then let
				val live = #live (labelRegInfo start)
				val args =
				   Vector.map
				   (live, fn z =>
				    case z of
				       M.Operand.StackOffset so => so
				     | _ => Error.bug "function with strange live")
			     in
				Chunk.newBlock
				(chunk, 
				 {label = funcToLabel name,
				  kind = M.Kind.Func,
				  live = live,
				  profileInfo = 
				  {ssa = #ssa profileInfo,
				   rssa = {func = profileInfoFunc,
					   label = Label.toString label}},
				  raises = raises,
				  returns = returns,
				  statements = Vector.new0 (),
				  transfer = M.Transfer.Goto start})
			     end
		     else ()
		  val {adjustSize, live, liveNoFormals, size, ...} =
		     labelRegInfo label
		  val chunk = labelChunk label
		  val statements =
		     Vector.concatV
		     (Vector.map (statements, fn s =>
				  genStatement (s, handlerLinkOffset)))
		  val (preTransfer, transfer) =
		     genTransfer (transfer, chunk, label)
		  fun frame () =
		     let
			val offsets =
			   Vector.fold
			   (liveNoFormals, [], fn (oper, ac) =>
			    case oper of
			       M.Operand.StackOffset {offset, ty} =>
				  if Type.isPointer ty
				     then offset :: ac
				  else ac
			     | _ => ac)
		     in
			List.push (frames, {chunkLabel = Chunk.label chunk,
					    offsets = offsets,
					    return = label,
					    size = size})
		     end
		  val (kind, live, pre) =
		     case kind of
			R.Kind.Cont _ =>
			   let
			      val _ = frame ()
			      val srcs = callReturnOperands (args, #2, size)
			   in
			      (M.Kind.Cont {args = srcs,
					    frameInfo = M.FrameInfo.bogus},
			       liveNoFormals,
			       parallelMove
			       {chunk = chunk,
				dsts = Vector.map (args, varOperand o #1),
				srcs = srcs})
			   end
		      | R.Kind.CReturn {func as CFunction.T {mayGC, ...}} =>
			   let
			      val dst =
				 case Vector.length args of
				    0 => NONE
				  | 1 => SOME (varOperand
					       (#1 (Vector.sub (args, 0))))
				  | _ => Error.bug "strange CReturn"
			      val frameInfo =
				 if mayGC
				    then
				       let
					  val _ = frame ()
				       in
					  SOME M.FrameInfo.bogus
				       end
				 else NONE
			   in
			      (M.Kind.CReturn {dst = dst,
					      frameInfo = frameInfo,
					      func = func},
			       liveNoFormals,
			       Vector.new0 ())
			   end
		      | R.Kind.Handler =>
			   let
			      val _ =
				 List.push
				 (handlers, {chunkLabel = Chunk.label chunk,
					     label = label})
			      val {handler = offset, ...} =
				 valOf handlerLinkOffset
			      val dsts = Vector.map (args, varOperand o #1)
			      val handles =
				 raiseOperands (Vector.map (dsts, M.Operand.ty))
			   in
			      (M.Kind.Handler {handles = handles,
					       offset = offset},
			       liveNoFormals,
			       M.Statement.moves {dsts = dsts,
						  srcs = handles})
			   end
		      | R.Kind.Jump => (M.Kind.Jump, live, Vector.new0 ())
		  val statements = Vector.concat [pre, statements, preTransfer]
	       in
		  Chunk.newBlock (chunk,
				  {kind = kind,
				   label = label,
				   live = live,
				   profileInfo = 
				   {ssa = #ssa profileInfo,
				    rssa = {func = profileInfoFunc,
					    label = Label.toString label}},
				   raises = raises,
				   returns = returns,
				   statements = statements,
				   transfer = transfer})
	       end
	    val genBlock = traceGenBlock genBlock
	    val _ = Vector.foreach (blocks, genBlock)
	    val _ =
	       if isMain
		  then ()
	       else Vector.foreach (blocks, R.Block.clear)
	 in
	    ()
	 end
      val genFunc =
	 Trace.trace2 ("Backend.genFunc",
		       Func.layout o Function.name, Bool.layout, Unit.layout)
	 genFunc
      (* Generate the main function first.
       * Need to do this in order to set globals.
       *)
      val _ = genFunc (main, true)
      val _ = List.foreach (functions, fn f => genFunc (f, false))
      val chunks = !chunks
      val _ = IntSet.reset ()
      val c = Counter.new 0
      val frameOffsets = ref []
      val {get: IntSet.t -> int, ...} =
	 Property.get
	 (IntSet.plist,
	  Property.initFun
	  (fn offsets =>
	   let val index = Counter.next c
	   in
	      List.push (frameOffsets, IntSet.toList offsets)
	      ; index
	   end))
      val {get = frameInfo: Label.t -> M.FrameInfo.t, set = setFrameInfo, ...} = 
	 Property.getSetOnce (Label.plist,
			      Property.initRaise ("frameInfo", Label.layout))
      val setFrameInfo =
	 Trace.trace2 ("Backend.setFrameInfo",
		       Label.layout, M.FrameInfo.layout, Unit.layout)
	 setFrameInfo
      val _ =
	 List.foreach
	 (!frames, fn {return, size, offsets, ...} =>
	  setFrameInfo
	  (return,
	   M.FrameInfo.T {size = size,
			  frameOffsetsIndex = get (IntSet.fromList offsets)}))
      (* Reverse the list of frameOffsets because offsetIndex 
       * is from back of list.
       *)
      val frameOffsets =
	 Vector.rev (Vector.fromListMap (!frameOffsets, Vector.fromList))
      fun blockToMachine (M.Block.T {kind, label, live, profileInfo,
				     raises, returns, statements, transfer}) =
	 let
	    datatype z = datatype M.Kind.t
	    val kind =
	       case kind of
		  Cont {args, ...} => Cont {args = args,
					    frameInfo = frameInfo label}
		| CReturn {dst, frameInfo = f, func} =>
		     CReturn {dst = dst,
			      frameInfo = Option.map (f, fn _ =>
						      frameInfo label),
			      func = func}
		| _ => kind
	    val transfer =
	       case transfer of
		  M.Transfer.CCall {args, frameInfo = f, func, return} =>
		     M.Transfer.CCall
		     {args = args,
		      frameInfo = Option.map (f, fn _ =>
					      frameInfo (valOf return)),
		      func = func,
		      return = return}
		| _ => transfer
	 in
	    M.Block.T {kind = kind,
		       label = label,
		       live = live,
		       profileInfo = profileInfo,
		       raises = raises,
		       returns = returns,
		       statements = statements,
		       transfer = transfer}
	 end
      fun chunkToMachine (Chunk.T {chunkLabel, blocks}) =
	 let
	    val blocks = Vector.fromListMap (!blocks, blockToMachine)
	    val regMax = Runtime.Type.memo (fn _ => ref ~1)
	    val regsNeedingIndex =
	       Vector.fold
	       (blocks, [], fn (b, ac) =>
		M.Block.foldDefs
		(b, ac, fn (z, ac) =>
		 case z of
		    M.Operand.Register r =>
		       (case Register.indexOpt r of
			   NONE => r :: ac
			 | SOME i =>
			      let
				 val z = regMax (Type.toRuntime (Register.ty r))
				 val _ =
				    if i > !z
				       then z := i
				    else ()
			      in
				 ac
			      end)
		  | _ => ac))
	    val _ =
	       List.foreach
	       (regsNeedingIndex, fn r =>
		let
		   val z = regMax (Type.toRuntime (Register.ty r))
		   val i = 1 + !z
		   val _ = z := i
		   val _ = Register.setIndex (r, i)
		in
		   ()
		end)
	 in
	    Machine.Chunk.T {chunkLabel = chunkLabel,
			     blocks = blocks,
			     regMax = ! o regMax}
	 end
      val mainName = R.Function.name main
      val main = {chunkLabel = Chunk.label (funcChunk mainName),
		  label = funcToLabel mainName}
      val chunks = List.revMap (chunks, chunkToMachine)
      (* The clear is necessary because properties have been attached to Funcs
       * and Labels, and they appear as labels in the resulting program.
       *)
      val _ = List.foreach (chunks, fn M.Chunk.T {blocks, ...} =>
			    Vector.foreach (blocks, Label.clear o M.Block.label))
      val maxFrameSize =
	 List.fold
	 (chunks, 0, fn (M.Chunk.T {blocks, ...}, max) =>
	  Vector.fold
	  (blocks, max, fn (M.Block.T {kind, statements, transfer, ...}, max) =>
	   let
	      fun doOperand (z: M.Operand.t, max) =
		 let
		    datatype z = datatype M.Operand.t
		 in
		    case z of
		       ArrayOffset {base, index, ...} =>
			  doOperand (base, doOperand (index, max))
		     | Cast (z, _) => doOperand (z, max)
		     | Contents {oper, ...} => doOperand (oper, max)
		     | Offset {base, ...} => doOperand (base, max)
		     | StackOffset {offset, ty} =>
			  Int.max (offset + Type.size ty, max)
		     | _ => max
		 end
	      val max =
		 case M.Kind.frameInfoOpt kind of
		    NONE => max
		  | SOME (M.FrameInfo.T {size, ...}) => Int.max (max, size)
	      val max =
		 Vector.fold
		 (statements, max, fn (s, max) =>
		  M.Statement.foldOperands (s, max, doOperand))
	      val max =
		 M.Transfer.foldOperands (transfer, max, doOperand)
	   in
	      max
	   end))
      val maxFrameSize = Runtime.wordAlignInt maxFrameSize
   in
      Machine.Program.T 
      {chunks = chunks,
       frameOffsets = frameOffsets, 
       handlesSignals = handlesSignals,
       intInfs = allIntInfs (), 
       main = main,
       maxFrameSize = maxFrameSize,
       objectTypes = objectTypes,
       profileAllocLabels = profileAllocLabels,
       reals = allReals (),
       strings = allStrings ()}
   end

end
   
