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
structure ParallelMove = ParallelMove ()
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
      datatype t = T of {chunkLabel: M.ChunkLabel.t,
			 (* where to start *)
			 entries: Label.t list ref,
			 gcReturns: Label.t list ref,
			 blocks: M.Block.t list ref,
			 (* for each type, gives the max # registers used *)
			 regMax: Type.t -> int ref}

      fun addEntry (T {entries, ...}, l) = List.push (entries, l)

      fun numRegsOfType (T {regMax, ...}, ty: Type.t): int = !(regMax ty)
	 
      fun numPointers (c) = numRegsOfType (c, Type.pointer)
      
      fun label (T {chunkLabel, ...}) = chunkLabel
	 
      fun equals (T {blocks = r, ...}, T {blocks = r', ...}) = r = r'
	 
      fun new (): t =
	 T {chunkLabel = M.ChunkLabel.new (),
	    entries = ref [],
	    blocks = ref [],
	    regMax = Type.memo (fn _ => ref 0),
	    gcReturns = ref []}
	 
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

fun toMachine (program: Ssa.Program.t) =
   let
      val program = SsaToRssa.convert program
      (* NEED TO INSERT LIMIT CHECKS*)
      val program as R.Program.T {functions, main, ...} = program
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
      val maxStackOffset: int ref = ref 0
      fun stackOffset {offset, ty} =
	 let
	    val n = offset + Type.size ty
	    val _ = if n > !maxStackOffset then maxStackOffset := n else ()
	 in M.Operand.StackOffset {offset = offset, ty = ty}
	 end
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
      fun newFrame (f as {chunkLabel, live, return, size}) = 
	 let
	    val maxFrameSize = Int.^ (2, 16)
	    val _ =
	       if size >= maxFrameSize
		  then (Error.bug
			(concat ["MLton cannot handle stack frames larger than ",
				 Int.toString maxFrameSize,
				 " bytes."]))
	       else ()
	    val offsets =
	       List.fold
	       (live, [], fn (oper, liveOffsets) =>
		case M.Operand.deStackOffset oper of
		   NONE => liveOffsets
		 | SOME {offset, ty} =>
		      (case Type.dest ty
			  of Type.Pointer => offset::liveOffsets
			| _ => liveOffsets))
	 in
	    List.push (frames, {chunkLabel = chunkLabel,
				offsets = offsets,
				return = return,
				size = size})
	 end
      (* Set funcChunk and labelChunk. *)
      val _ =
	 Vector.foreach
	 (Chunkify.chunkify program, fn {funcs, labels} =>
	  let 
	     val c = newChunk ()
	     val _ = Vector.foreach (funcs, fn f =>
				     (Chunk.addEntry (c, funcToLabel f)
				      ; setFuncChunk (f, c)))
	     val _ = Vector.foreach (labels, fn l => setLabelChunk (l, c))
	  in
	     ()
	  end)
      (* Add chunk entries for all conts and handlers. *)
      val _ =
	 List.foreach
	 (functions, fn f =>
	  let
	     val {blocks, ...} = R.Function.dest f
	  in
	     Vector.foreach
	     (blocks, fn R.Block.T {kind, label, ...} =>
	      if let
		    datatype z = datatype R.Kind.t
		 in
		    (case kind of
			Cont _ => true
		      | CReturn => false
		      | Handler => true
		      | Normal => false
		      | Runtime => true)
		 end
		 then Chunk.addEntry (labelChunk label, label)
	      else ())
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
	       CastInt x => M.Operand.CastInt (varOperand x)
	     | Const c => constOperand c
	     | Offset {base, bytes, ty} =>
		  M.Operand.Offset {base = varOperand base,
				    offset = bytes,
				    ty = ty}
	     | OffsetScale {base, index, ty} =>
		  M.Operand.ArrayOffset {base = varOperand base,
					 offset = varOperand index,
					 ty = ty}
	     | Var {var, ...} => varOperand var
	 end
      fun translateOperands ops = Vector.map (ops, translateOperand)
      fun genStatement (s: R.Statement.t, handlerOffset): M.Statement.t =
	 let
	    datatype z = datatype R.Statement.t
	 in
	    case s of
	       Array {dst, numBytesNonPointers, numElts, numPointers} =>
		  M.Statement.Array {dst = varOperand dst,
				     numBytesNonPointers = numBytesNonPointers,
				     numElts = varOperand numElts,
				     numPointers = numPointers}
	     | Move {dst, src} =>
		  M.Statement.move {dst = translateOperand dst,
				    src = translateOperand src}
	     | Object {dst, numPointers, numWordsNonPointers, stores} =>
		  M.Statement.Allocate
		  {dst = varOperand dst,
		   size = wordSize * (numPointers + numWordsNonPointers),
		   numPointers = numPointers,
		   numWordsNonPointers = numWordsNonPointers,
		   stores = Vector.map (stores, fn {offset, value} =>
					{offset = offset,
					 value = translateOperand value})}
	     | PrimApp {dst, prim, args} =>
		  M.Statement.Assign {dst = Option.map (dst, fn (x, _) =>
							varOperand x),
				      prim = prim,
				      args = Vector.map (args, varOperand)}
	     | SetExnStackLocal =>
		  M.Statement.SetExnStackLocal {offset = valOf handlerOffset}
	     | SetExnStackSlot =>
		  M.Statement.SetExnStackSlot {offset = valOf handlerOffset}
	     | SetHandler h =>
		  M.Statement.move
		  {dst = stackOffset {offset = valOf handlerOffset,
						ty = Type.label},
		   src = M.Operand.Label h}
	     | SetSlotExnStack =>
		  M.Statement.SetSlotExnStack {offset = valOf handlerOffset}
	 end
      val genStatement =
	 Trace.trace ("Backend.genStatement",
		      R.Statement.layout o #1, M.Statement.layout)
	 genStatement
      val {get = labelInfo: Label.t -> {args: (Var.t * Type.t) vector},
	   set = setLabelInfo, ...} =
	 Property.getSetOnce
	 (Label.plist, Property.initRaise ("labelInfo", Label.layout))
      fun genFunc (f: Function.t, isMain: bool): unit =
	 let
	    val {args, blocks, name, start, ...} = Function.dest f
	    val chunk = funcChunk name
	    fun labelArgOperands (l: R.Label.t): M.Operand.t vector =
	       Vector.map (#args (labelInfo l), varOperand o #1)
	    fun newVarInfo (x, ty) =
	       setVarInfo
	       (x, {operand = if isMain
				 then VarOperand.Allocate {operand = ref NONE}
			      else VarOperand.Const (M.Operand.Global
						     (newGlobal ty)),
                    ty = ty})
	    fun newVarInfos xts = Vector.foreach (xts, newVarInfo)
	    (* Set the constant operands, labelInfo, and varInfo. *)
	    val _ = newVarInfos args
	    val _ =
	       Vector.foreach
	       (blocks, fn R.Block.T {label, args, statements, ...} =>
		let
		   val _ = setLabelInfo (label, {args = args})
		   val _ = newVarInfos args
		   val _ =
		      Vector.foreach
		      (statements, fn s =>
		       let
			  fun normal () =
			     R.Statement.forDef (s, fn {var, ty} =>
						 newVarInfo (var, ty))
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
				    | R.Operand.Var {var = var', ...} =>
					 (case #operand (varInfo var') of
					     VarOperand.Const oper => set oper
					   | VarOperand.Allocate _ => normal ())
				    | _ => normal ()
				end
			   | _ => normal ()
		       end)
		in
		   ()
		end)
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
	       val {handlerOffset, labelInfo = labelRegInfo, ...} =
		  AllocateRegisters.allocate {function = f,
					      newRegister = newRegister,
					      varInfo = varInfo}
	    end
	    val profileInfoFunc = Func.toString name
	    (* ------------------------------------------------- *)
	    (*                    genTransfer                    *)
	    (* ------------------------------------------------- *)
	    fun callReturnOperands (xs: 'a vector,
				    ty: 'a -> Type.t,
				    shift: int): M.Operand.t vector =
	       #1 (Vector.mapAndFold
		   (xs, labelSize, (* labelSize is for return address *)
		    fn (x, offset) =>
		    let
		       val ty = ty x
		       val offset = Type.align (ty, offset)
		    in
		       (stackOffset {offset = shift + offset, 
					       ty = ty},
			offset + Type.size ty)
		    end))
	    fun genTransfer (t: R.Transfer.t,
			     chunk: Chunk.t,
			     label: Label.t,
			     handlerOffset: int option)
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
						   val handlerOffset =
						      valOf handlerOffset
						in
						   (SOME h,
						    Vector.new2
						    (stackOffset 
						     {offset = handlerOffset,
						      ty = Type.label},
						     stackOffset 
						     {offset = (handlerOffset
								+ labelSize),
						      ty = Type.uint}))
						end
				       val size = 
					  if !Control.newReturn
					     then #size (adjustSize size)
					  else Type.wordAlign size
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
			      if !Control.Native.native
				 orelse (not (Chunk.equals (chunk, chunk')))
				 then
				    M.Transfer.FarJump
				    {chunkLabel = Chunk.label chunk',
				     label = funcToLabel func,
				     live = (Vector.toList
					     (Vector.concat [handlerLive, dsts])),
				     return = return}
			      else M.Transfer.NearJump {label = funcToLabel func,
							return = return}
			in (setupArgs, transfer)
			end
		   | R.Transfer.Goto {dst, args} =>
			(parallelMove {srcs = translateOperands args,
				       dsts = labelArgOperands dst,
				       chunk = labelChunk dst},
			 M.Transfer.NearJump {label = dst,
					      return = NONE})
		   | R.Transfer.LimitCheck {failure, kind, success} =>
			let
			   datatype z = datatype R.LimitCheck.t
			   val kind =
			      case kind of
				 Array {bytesPerElt, extraBytes, numElts, stackToo} =>
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
			    M.Transfer.Return {live = Vector.toList dsts})
			end
		   | R.Transfer.Runtime {prim, args, return} => 
			simple
			(M.Transfer.Runtime
			 {args = Vector.map (args, translateOperand),
			  prim = prim,
			  return = return})
		   | R.Transfer.Switch {cases, default, test} =>
			simple (M.Transfer.Switch {cases = cases,
						   default = default,
						   test = translateOperand test})
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
			   transfer = (M.Transfer.NearJump
				       {label = start,
					return = NONE})})
	       end
	    fun genBlock (R.Block.T {args, kind, label, statements, transfer,
				     ...}) : unit =
	       let
		  val {adjustSize, live, liveNoFormals, size, ...} =
		     labelRegInfo label
		  val chunk = labelChunk label
		  val statements =
		     Vector.map (statements, fn s =>
				 genStatement (s, handlerOffset))
		  val (preTransfer, transfer) =
		     genTransfer (transfer, chunk, label, handlerOffset)
		  val (kind, pre) =
		     case kind of
			R.Kind.Cont {handler} =>
			   let
			      val size' =
				 case handler of
				    NONE => size
				  | SOME h =>
				       let
					  val {size = size', ...} =
					     labelRegInfo h
				       in Int.max (size, size')
				       end
			      val size = Type.wordAlign size'
			      val _ =
				 newFrame {return = label,
					   chunkLabel = Chunk.label chunk,
					   size = size,
					   live = liveNoFormals}
			      val srcs = callReturnOperands (args, #2, 0)
			   in
			      (M.Kind.Cont {args = Vector.toList srcs,
					    frameInfo = M.FrameInfo.bogus},
			       parallelMove
			       {chunk = chunk,
				dsts = Vector.map (args, varOperand o #1),
				srcs = srcs})
			   end
		      | R.Kind.CReturn =>
			   let
			      val ret =
				 if 0 < Vector.length args
				    then
				       let
					  val (x, t) = Vector.sub (args, 0)
				       in
					  SOME {arg = varOperand x, ty = t}
				       end
				 else NONE
			   in
			      (M.Kind.CReturn ret, Vector.new0 ())
			   end
		      | R.Kind.Handler =>
			   let
			      val _ =
				 List.push
				 (handlers, {chunkLabel = Chunk.label chunk,
					     label = label})
			      val offset = valOf handlerOffset
			      val dsts = Vector.map (args, varOperand o #1)
			   in (M.Kind.Handler {offset = offset},
			       M.Statement.moves
			       {dsts = dsts,
				srcs = (raiseOperands
					(Vector.map (dsts, M.Operand.ty)))})
			   end
		      | R.Kind.Normal => (M.Kind.Jump, Vector.new0 ())
		      | R.Kind.Runtime =>
			   (M.Kind.Runtime {frameInfo = M.FrameInfo.bogus},
			    Vector.new0 ())
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
	    val _ = Vector.foreach (blocks, genBlock)
	    val _ = Vector.foreach (blocks, R.Block.clear)
	 in
	    ()
	 end
      (* Generate the main function first.
       * Need to do this in order to set globals.
       *)
      val _ =
	 case List.peek (functions, fn f =>
			 Func.equals (main, R.Function.name f)) of
	    NONE => Error.bug "missing main function"
	  | SOME f => genFunc (f, true)
      val _ = List.foreach (functions, fn f => genFunc (f, false))
      val chunks = !chunks
      (* The clear is necessary because properties have been attached to Funcs
       * and Labels, and they appear as labels in the resulting program
       *)
      val _ = List.foreach (chunks, fn Chunk.T {blocks, ...} =>
			    List.foreach (!blocks, Label.clear o M.Block.label))
      val _ = IntSet.reset ()
      val c = Counter.new 0
      val frameOffsets = ref []
      val {get: IntSet.t -> int, ...} =
	 Property.get
	 (IntSet.plist,
	  Property.initFun
	  (fn offsets =>
	   let val index = Counter.next c
	   in List.push (frameOffsets, IntSet.toList offsets)
	      ; index
	   end))
      val getFrameLayoutOffsetIndex = get o IntSet.fromList
      val {get = frameInfo: Label.t -> M.FrameInfo.t,
	   set = setFrameInfo, ...} = 
	 Property.getSetOnce (Label.plist,
			      Property.initRaise ("frameInfo", Label.layout))
      val _ =
	 List.foreach
	 (!frames, fn {return, size, offsets, ...} =>
	  setFrameInfo
	  (return,
	   M.FrameInfo.T {size = size,
			  offsetIndex = getFrameLayoutOffsetIndex offsets}))
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
		| Runtime _ => Runtime {frameInfo = frameInfo label}
		| _ => kind
	 in
	    M.Block.T {kind = kind,
		       label = label,
		       live = live,
		       profileInfo = profileInfo,
		       statements = statements,
		       transfer = transfer}
	 end
      fun chunkToMachine (Chunk.T
			  {chunkLabel, entries, gcReturns, blocks, regMax, ...})
	 =
	 Machine.Chunk.T {chunkLabel = chunkLabel,
			  blocks = Vector.fromListMap (!blocks, blockToMachine),
			  regMax = ! o regMax}

   in
      Machine.Program.T 
      {chunks = List.revMap (chunks, chunkToMachine),
       floats = allFloats (),
       frameOffsets = frameOffsets, 
       globals = Counter.value o globalCounter,
       globalsNonRoot = Counter.value globalPointerNonRootCounter,
       intInfs = allIntInfs (), 
       main = {chunkLabel = Chunk.label (funcChunk main),
	       label = funcToLabel main},
       maxFrameSize = Type.wordAlign (!maxStackOffset),
       strings = allStrings ()}
   end

end
   
