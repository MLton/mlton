(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Backend (S: BACKEND_STRUCTS): BACKEND =
struct

open S

structure M = Machine
local
   open Machine
in
   structure Chunk = Chunk
end

structure Rssa = Rssa (open Ssa
		       structure Cases = Machine.Cases
		       structure Type = Machine.Type)
structure R = Rssa
local
   open Rssa
in
   structure Cases = Cases
   structure Con = Con
   structure Const = Const
   structure Func = Func
   structure Function = Function
   structure Label = Label
   structure Prim = Prim
   structure Tycon = Tycon
   structure Type = Type
   structure Var = Var
end 

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
val wordSize: int = 4
val labelSize = Type.size Type.label
   
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
			 chunkLabel: M.ChunkLabel.t,
			 regMax: Type.t -> int ref}

      fun label (T {chunkLabel, ...}) = chunkLabel
	 
      fun equals (T {blocks = r, ...}, T {blocks = r', ...}) = r = r'
	 
      fun new (): t =
	 T {blocks = ref [],
	    chunkLabel = M.ChunkLabel.new (),
	    regMax = Type.memo (fn _ => ref 0)}
	 
      fun register (T {regMax, ...}, n, ty) =
	 let
	    val r = regMax ty
	    val _ = r := Int.max (!r, n + 1)
	 in
	    M.Register.T {index = n, ty = ty}
	 end
      
      fun tempRegister (c as T {regMax, ...}, ty) =
	 register (c, !(regMax ty), ty)
	 
      fun newBlock (T {blocks, ...}, z) =
	 List.push (blocks, M.Block.T z)
   end

val traceGenBlock =
   Trace.trace ("Backend.genBlock",
		Label.layout o R.Block.label,
		Unit.layout)

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
      val R.Program.T {functions, main} = program
      (* Chunk information *)
      val {get = labelChunk, set = setLabelChunk, ...} =
	 Property.getSetOnce (Label.plist,
			      Property.initRaise ("labelChunk", Label.layout))
      val {get = funcChunk: Func.t -> Chunk.t, set = setFuncChunk, ...} =
	 Property.getSetOnce (Func.plist,
			      Property.initRaise ("funcChunk", Func.layout))
      val funcChunkLabel = Chunk.label o funcChunk
      val globalCounter = Type.memo (fn _ => Counter.new 0)
      fun newGlobal ty =
	 M.Global.T {index = Counter.next (globalCounter ty),
		     ty = ty}
      val globalPointerNonRootCounter = Counter.new 0
      val constantCounter = Type.memo (fn _ => Counter.new 0)
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
		     val opers =
			Vector.map
			(ts, fn t =>
			 if Type.isPointer t
			    then
			       M.Operand.GlobalPointerNonRoot
			       (Counter.next globalPointerNonRootCounter)
			 else M.Operand.Global (newGlobal t))
		     val _ = List.push (table, (ts, opers))
		  in
		     opers
		  end
	     | SOME (_, os) => os
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
				  global = newGlobal ty,
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
	    make (Type.pointer, fn i => IntInf.format (i, StringCvt.DEC))
	 val (allFloats, globalFloat) = make (Type.double, fn s => s)
	 val (allStrings, globalString) = make (Type.pointer, fn s => s)
	 fun constOperand (c: Const.t): M.Operand.t =
	    let
	       datatype z = datatype Const.Node.t
	    in
	       case Const.node c of
		  Char n => M.Operand.Char n
		| Int n => M.Operand.Int n
		| IntInf i =>
		     if Const.SmallIntInf.isSmall i
			then M.Operand.IntInf (Const.SmallIntInf.toWord i)
		     else globalIntInf i
		| Real f =>
		     if !Control.Native.native
			then globalFloat f
		     else M.Operand.Float f
		| String s => globalString s
		| Word w =>
		     let val t = Const.tycon c
		     in if Tycon.equals (t, Tycon.word)
			   then M.Operand.Uint w
			else if Tycon.equals (t, Tycon.word8)
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
	       M.Operand.Register (Chunk.tempRegister (chunk, M.Operand.ty r))
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
		  M.Operand.ArrayOffset {base = varOperand base,
					 index = varOperand index,
					 ty = ty}
	     | CastInt x => M.Operand.CastInt (varOperand x)
	     | Const c => constOperand c
	     | Offset {base, bytes, ty} =>
		  M.Operand.Offset {base = varOperand base,
				    offset = bytes,
				    ty = ty}
	     | Pointer n => M.Operand.Pointer n
	     | Var {var, ...} => varOperand var
	 end
      fun translateOperands ops = Vector.map (ops, translateOperand)
      fun genStatement (s: R.Statement.t,
			handlerLinkOffset: {handler: int,
					    link: int} option): M.Statement.t =
	 let
	    fun handlerOffset () = #handler (valOf handlerLinkOffset)
	    fun linkOffset () = #link (valOf handlerLinkOffset)
	    datatype z = datatype R.Statement.t
	 in
	    case s of
	       Array {dst, numBytesNonPointers, numElts, numPointers} =>
		  M.Statement.Array {dst = varOperand dst,
				     numBytesNonPointers = numBytesNonPointers,
				     numElts = varOperand numElts,
				     numPointers = numPointers}
	     | Move {dst, src} =>
		  if (case dst of
			 R.Operand.Var {var, ...} =>
			    (case #operand (varInfo var) of
				VarOperand.Const _ => true
			      | _ => false)
		       | _ => false)
		     then M.Statement.Noop
		  else M.Statement.move {dst = translateOperand dst,
					 src = translateOperand src}
	     | Object {dst, numPointers, numWordsNonPointers, stores} =>
		  M.Statement.Object
		  {dst = varOperand dst,
		   numPointers = numPointers,
		   numWordsNonPointers = numWordsNonPointers,
		   stores = Vector.map (stores, fn {offset, value} =>
					{offset = offset,
					 value = translateOperand value})}
	     | PrimApp {dst, prim, args} =>
		  M.Statement.PrimApp {args = Vector.map (args, varOperand),
				       dst = Option.map (dst, varOperand o #1),
				       prim = prim}
	     | SetExnStackLocal =>
		  M.Statement.SetExnStackLocal {offset = handlerOffset ()}
	     | SetExnStackSlot =>
		  M.Statement.SetExnStackSlot {offset = linkOffset ()}
	     | SetHandler h =>
		  M.Statement.move
		  {dst = M.Operand.StackOffset {offset = handlerOffset (),
						ty = Type.label},
		   src = M.Operand.Label h}
	     | SetSlotExnStack =>
		  M.Statement.SetSlotExnStack {offset = linkOffset ()}
	 end
      val genStatement =
	 Trace.trace ("Backend.genStatement",
		      R.Statement.layout o #1, M.Statement.layout)
	 genStatement
      val {get = labelInfo: Label.t -> {args: (Var.t * Type.t) vector},
	   set = setLabelInfo, ...} =
	 Property.getSetOnce
	 (Label.plist, Property.initRaise ("labelInfo", Label.layout))
      val setLabelInfo =
	 Trace.trace2 ("Backend.setLabelInfo",
		       Label.layout, Layout.ignore, Unit.layout)
	 setLabelInfo
      fun genFunc (f: Function.t, isMain: bool): unit =
	 let
	    val {args, blocks, name, start, ...} = Function.dest f
	    val chunk = funcChunk name
	    fun labelArgOperands (l: R.Label.t): M.Operand.t vector =
	       Vector.map (#args (labelInfo l), varOperand o #1)
	    fun newVarInfo (x, ty) =
	       setVarInfo
	       (x, {operand = if isMain
				 then
				    VarOperand.Const (M.Operand.Global
						      (newGlobal ty))
			      else VarOperand.Allocate {operand = ref NONE},
                    ty = ty})
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
			     R.Statement.Move {dst = R.Operand.Var {var, ty}, src} =>
				let
				   fun set oper =
				      setVarInfo
				      (var, {operand = VarOperand.Const oper,
					     ty = ty})
				in
				   case src of
				      R.Operand.Const c => set (constOperand c)
				    | R.Operand.Pointer n =>
					 set (M.Operand.Pointer n)
				    | R.Operand.Var {var = var', ...} =>
					 (case #operand (varInfo var') of
					     VarOperand.Const oper => set oper
					   | VarOperand.Allocate _ => normal ())
				    | _ => normal ()
				end
			   | _ => normal ()
		       end)
		   val _ = R.Transfer.foreachDef (transfer, newVarInfo)
		in
		   fn () => ()
		end)
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
	    (* Allocate stack slots. *)
	    local
	       val varInfo =
		  fn x =>
		  let
		     val {operand, ty, ...} = varInfo x
		  in
		     {operand = (case operand of
				    VarOperand.Allocate {operand, ...} => SOME operand
				  | _ => NONE),
		      ty = ty}
		  end
	       fun newRegister (l, n, ty) =
		  let
		     val chunk =
			case l of
			   NONE => chunk
			 | SOME l => labelChunk l
		  in
		     Chunk.register (chunk, n, ty)
		  end
	    in
	       val {handlerLinkOffset, labelInfo = labelRegInfo, ...} =
		  AllocateRegisters.allocate
		  {argOperands = callReturnOperands (args, #2, 0),
		   function = f,
		   newRegister = newRegister,
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
		     R.Transfer.Arith {args, dst, overflow, prim, success} =>
			simple
			(M.Transfer.Arith {args = varOperands args,
					   dst = varOperand dst,
					   overflow = overflow,
					   prim = prim,
					   success = success})
		   | R.Transfer.Bug => simple M.Transfer.Bug
		   | R.Transfer.CCall {args, prim, return, returnTy} =>
			simple (M.Transfer.CCall {args = translateOperands args,
						  prim = prim,
						  return = return,
						  returnTy = returnTy})
		   | R.Transfer.Call {func, args, return} =>
			let
			   val (frameSize, return, handlerLive) =
			      case return of
				 R.Return.Dead => (0, NONE, Vector.new0 ())
			       | R.Return.Tail => (0, NONE, Vector.new0 ())
			       | R.Return.HandleOnly => (0, NONE, Vector.new0 ())
			       | R.Return.NonTail {cont, handler} =>
				    let
				       val {size, adjustSize, ...} =
					  labelRegInfo cont
				       val (handler, handlerLive) =
					  case handler of
					     R.Handler.CallerHandler =>
						(NONE, Vector.new0 ())
					   | R.Handler.None =>
						(NONE, Vector.new0 ())
					   | R.Handler.Handle h =>
						let
						   val {size = size', ...} =
						      labelRegInfo h
						   val {handler, link} =
						      valOf handlerLinkOffset
						in
						   (SOME h,
						    Vector.new2
						    (M.Operand.StackOffset 
						     {offset = handler,
						      ty = Type.label},
						     M.Operand.StackOffset 
						     {offset = link,
						      ty = Type.uint}))
						end
				       val size = 
					  if !Control.newReturn
					     then #size (adjustSize size)
					  else size
				    in
				       (size, 
					SOME {return = cont,
					      handler = handler,
					      size = size},
					handlerLive)
				    end
			   val dsts =
			      callReturnOperands (args, R.Operand.ty, frameSize)
			   val setupArgs =
			      parallelMove {chunk = chunk,
					    dsts = dsts,
					    srcs = translateOperands args}
			   val chunk' = funcChunk func
			   val transfer =
			      M.Transfer.Call
			      {label = funcToLabel func,
			       live = Vector.concat [handlerLive, dsts],
			       return = return}
			in (setupArgs, transfer)
			end
		   | R.Transfer.Goto {dst, args} =>
			(parallelMove {srcs = translateOperands args,
				       dsts = labelArgOperands dst,
				       chunk = labelChunk dst},
			 M.Transfer.Goto dst)
		   | R.Transfer.LimitCheck {failure, kind, success} =>
			let
			   datatype z = datatype R.LimitCheck.t
			   val kind =
			      case kind of
				 Array {bytesPerElt, extraBytes, numElts,
					stackToo} =>
				    M.LimitCheck.Array
				    {bytesPerElt = bytesPerElt,
				     extraBytes = extraBytes,
				     numElts = varOperand numElts,
				     stackToo = stackToo}
			       | Heap z => M.LimitCheck.Heap z
			       | Signal => M.LimitCheck.Signal
			       | Stack => M.LimitCheck.Stack
			   (* It doesn't matter whether we use live or
			    * liveNoFormals, since the return is nullary.
			    *)
			in
			   simple
			   (M.Transfer.LimitCheck {failure = failure,
						   kind = kind,
						   success = success})
			end
		   | R.Transfer.Raise srcs =>
			(M.Statement.moves
			 {dsts = raiseOperands (Vector.map
						(srcs, R.Operand.ty)),
			  srcs = translateOperands srcs},
			 M.Transfer.Raise)
		   | R.Transfer.Return xs =>
			let
			   val dsts = callReturnOperands (xs, R.Operand.ty, 0)
			in
			   (parallelMove {chunk = chunk,
					  srcs = translateOperands xs,
					  dsts = dsts},
			    M.Transfer.Return {live = dsts})
			end
		   | R.Transfer.Runtime {prim, args, return} => 
			simple
			(M.Transfer.Runtime
			 {args = Vector.map (args, translateOperand),
			  prim = prim,
			  return = return})
		   | R.Transfer.Switch {cases, default, test} =>
			let
			   fun doit l =
			      simple
			      (case (l, default) of
				  ([], NONE) => M.Transfer.Bug
				| ([(_, dst)], NONE) => M.Transfer.Goto dst
				| ([], SOME dst) => M.Transfer.Goto dst
				| _ =>
				     M.Transfer.Switch
				     {cases = cases,
				      default = default,
				      test = translateOperand test})
			in
			   case cases of
			      Cases.Char l => doit l
			    | Cases.Int l => doit l
			    | Cases.Word l => doit l
			end
		   | R.Transfer.SwitchIP {int, pointer, test} =>
			simple (M.Transfer.SwitchIP
				{int = int,
				 pointer = pointer,
				 test = translateOperand test})
	       end
	    val genTransfer =
	       Trace.trace ("Backend.genTransfer",
			    R.Transfer.layout o #1,
			    Layout.tuple2 (Vector.layout M.Statement.layout,
					   M.Transfer.layout))
	       genTransfer
	    val _ =
	       let
		  val live = #live (labelRegInfo start)
	       in
		  Chunk.newBlock
		  (chunk, {label = funcToLabel name,
			   kind = M.Kind.Func {args = live},
			   live = live,
			   profileInfo = {func = profileInfoFunc,
					  label = profileInfoFunc},
			   statements = Vector.new0 (),
			   transfer = M.Transfer.Goto start})
	       end
	    fun genBlock (R.Block.T {args, kind, label, statements, transfer,
				     ...}) : unit =
	       let
		  val {adjustSize, live, liveNoFormals, size, ...} =
		     labelRegInfo label
		  val chunk = labelChunk label
		  val statements =
		     Vector.map (statements, fn s =>
				 genStatement (s, handlerLinkOffset))
		  val (preTransfer, transfer) =
		     genTransfer (transfer, chunk, label)
		  fun frame () =
		     let
			val offsets =
			   Vector.fold
			   (liveNoFormals, [], fn (oper, ac) =>
			    case oper of
			       M.Operand.StackOffset {offset, ty} =>
				  (case Type.dest ty of
				      Type.Pointer => offset :: ac
				    | _ => ac)
			     | _ => ac)
		     in
			List.push (frames, {chunkLabel = Chunk.label chunk,
					    offsets = offsets,
					    return = label,
					    size = size})
		     end
		  val (kind, pre) =
		     case kind of
			R.Kind.Cont {handler} =>
			   let
			      val _ = frame ()
			      val srcs = callReturnOperands (args, #2, size)
			   in
			      (M.Kind.Cont {args = srcs,
					    frameInfo = M.FrameInfo.bogus},
			       parallelMove
			       {chunk = chunk,
				dsts = Vector.map (args, varOperand o #1),
				srcs = srcs})
			   end
		      | R.Kind.CReturn {prim} =>
			   let
			      val dst =
				 if 0 < Vector.length args
				    then SOME (varOperand
					       (#1 (Vector.sub (args, 0))))
				 else NONE
			   in
			      (M.Kind.CReturn {dst = dst,
					       prim = prim},
			       Vector.new0 ())
			   end
		      | R.Kind.Handler =>
			   let
			      val _ =
				 List.push
				 (handlers, {chunkLabel = Chunk.label chunk,
					     label = label})
			      val {handler, ...} = valOf handlerLinkOffset
			      val dsts = Vector.map (args, varOperand o #1)
			   in
			      (M.Kind.Handler {offset = handler},
			       M.Statement.moves
			       {dsts = dsts,
				srcs = (raiseOperands
					(Vector.map (dsts, M.Operand.ty)))})
			   end
		      | R.Kind.Jump => (M.Kind.Jump, Vector.new0 ())
		      | R.Kind.Runtime {prim} =>
			   let
			      val _ = frame ()
			   in
			      (M.Kind.Runtime {frameInfo = M.FrameInfo.bogus,
					       prim = prim},
			       Vector.new0 ())
			   end
		  val statements = Vector.concat [pre, statements, preTransfer]
	       in
		  Chunk.newBlock (chunk,
				  {kind = kind,
				   label = label,
				   live = live,
				   profileInfo = {func = profileInfoFunc,
						  label = Label.toString label},
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
				     statements, transfer}) =
	 let
	    datatype z = datatype M.Kind.t
	    val kind =
	       case kind of
		  Cont {args, ...} => Cont {args = args,
					    frameInfo = frameInfo label}
		| Runtime {prim, ...} => Runtime {frameInfo = frameInfo label,
						  prim = prim}
		| _ => kind
	 in
	    M.Block.T {kind = kind,
		       label = label,
		       live = live,
		       profileInfo = profileInfo,
		       statements = statements,
		       transfer = transfer}
	 end
      fun chunkToMachine (Chunk.T {chunkLabel, blocks, regMax}) =
	 Machine.Chunk.T {chunkLabel = chunkLabel,
			  blocks = Vector.fromListMap (!blocks, blockToMachine),
			  regMax = ! o regMax}
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
	      fun doFrameInfo (M.FrameInfo.T {size, ...}, max) =
		 Int.max (max, size)
	      fun doOperand (z: M.Operand.t, max) =
		 let
		    datatype z = datatype M.Operand.t
		 in
		    case z of
		       ArrayOffset {base, index, ...} =>
			  doOperand (base, doOperand (index, max))
		     | CastInt z => doOperand (z, max)
		     | Contents {oper, ...} => doOperand (oper, max)
		     | Offset {base, ...} => doOperand (base, max)
		     | StackOffset {offset, ty} =>
			  Int.max (offset + Type.size ty, max)
		     | _ => max
		 end
	      val max =
		 case kind of
		    M.Kind.Cont {frameInfo, ...} =>
		       doFrameInfo (frameInfo, max)
		  | M.Kind.Runtime {frameInfo, ...} =>
		       doFrameInfo (frameInfo, max)
		  | _ => max
	      val max =
		 Vector.fold
		 (statements, max, fn (s, max) =>
		  M.Statement.foldOperands (s, max, doOperand))
	      val max =
		 M.Transfer.foldOperands (transfer, max, doOperand)
	   in
	      max
	   end))
      val maxFrameSize = Type.wordAlign maxFrameSize
   in
      Machine.Program.T 
      {chunks = chunks,
       floats = allFloats (),
       frameOffsets = frameOffsets, 
       globals = Counter.value o globalCounter,
       globalsNonRoot = Counter.value globalPointerNonRootCounter,
       intInfs = allIntInfs (), 
       main = main,
       maxFrameSize = maxFrameSize,
       strings = allStrings ()}
   end

end
   
