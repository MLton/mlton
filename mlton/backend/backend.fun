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

structure AllocateRegisters = AllocateRegisters (structure Rssa = Rssa
						 structure Machine = Machine)
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
      val mprogram = M.Program.new ()
      (* Create the mprogram chunks. *)
      val chunks = Chunkify.chunkify program
      val _ =
	 Vector.foreach
	 (chunks, fn {funcs, labels} =>
	  let 
	     val c = M.Program.newChunk mprogram
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
		    datatype z = datatype R.Block.Kind.t
		 in
		    (case kind of
			Cont _ => true
		      | CReturn => false
		      | Handler => true
		      | Normal => false)
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
			    then M.Program.newGlobalPointerNonRoot mprogram
			 else M.Program.newGlobal (mprogram, t))
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
	 fun 'a make (new: M.Program.t * string -> M.Operand.t,
		      toString: 'a -> string): 'a -> M.Operand.t =
	    let
	       val set = HashSet.new {hash = #hash}
	       fun get (a: 'a): M.Operand.t =
		  let
		     val s = toString a
		     val hash = String.hash s
		  in
		     #operand
		     (HashSet.lookupOrInsert
		      (set, hash, fn {string, ...} => s = string,
		       fn () => {hash = hash,
				 operand = new (mprogram, s),
				 string = s}))
		  end
	    in
	       get     
	    end
	 val globalIntInf = make (M.Program.newIntInf,
				  fn i => IntInf.format (i, StringCvt.DEC))
	 val globalFloat = make (M.Program.newFloat, fn s => s)
	 val globalString = make (M.Program.newString, fn s => s)
      in
	 fun constOperand (c: Const.t): M.Operand.t =
	    let
	       datatype z = datatype Const.Node.t
	    in
	       case Const.node c of
		  Char n => M.Operand.char n
		| Int n => M.Operand.int n
		| IntInf i =>
		     if Const.SmallIntInf.isSmall i
			then M.Operand.intInf (Const.SmallIntInf.toWord i)
		     else globalIntInf i
		| Real f =>
		     if !Control.Native.native
			then globalFloat f
		     else M.Operand.float f
		| String s => globalString s
		| Word w =>
		     let val t = Const.tycon c
		     in if Tycon.equals (t, Tycon.word)
			   then M.Operand.uint w
			else if Tycon.equals (t, Tycon.word8)
				then M.Operand.char (Char.chr (Word.toInt w))
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
	       M.Operand.register (Chunk.tempRegister (chunk, M.Operand.ty r))
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
	       CastInt x => M.Operand.castInt (varOperand x)
	     | Const c => constOperand c
	     | Offset {base, bytes, ty} =>
		  M.Operand.offset {base = varOperand base,
				    offset = bytes,
				    ty = ty}
	     | OffsetScale {base, index, ty} =>
		  M.Operand.arrayOffset {base = varOperand base,
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
		  M.Statement.array {dst = varOperand dst,
				     numBytesNonPointers = numBytesNonPointers,
				     numElts = varOperand numElts,
				     numPointers = numPointers}
	     | Move {dst, src} =>
		  M.Statement.move {dst = translateOperand dst,
				    src = translateOperand src}
	     | Object {dst, numPointers, numWordsNonPointers, stores} =>
		  M.Statement.allocate
		  {dst = varOperand dst,
		   size = wordSize * (numPointers + numWordsNonPointers),
		   numPointers = numPointers,
		   numWordsNonPointers = numWordsNonPointers,
		   stores = Vector.map (stores, fn {offset, value} =>
					{offset = offset,
					 value = translateOperand value})}
	     | PrimApp {dst, prim, args} =>
		  M.Statement.assign {dst = Option.map (dst, fn (x, _) =>
							varOperand x),
				      prim = prim,
				      args = Vector.map (args, varOperand)}
	     | SetExnStackLocal =>
		  M.Statement.setExnStackLocal {offset = valOf handlerOffset}
	     | SetExnStackSlot =>
		  M.Statement.setExnStackSlot {offset = valOf handlerOffset}
	     | SetHandler h =>
		  M.Statement.move
		  {dst = M.Operand.stackOffset {offset = valOf handlerOffset,
						ty = Type.label},
		   src = M.Operand.label h}
	     | SetSlotExnStack =>
		  M.Statement.setSlotExnStack {offset = valOf handlerOffset}
	 end
      val genStatement =
	 Trace.trace ("Backend.genStatement",
		      R.Statement.layout o #1, M.Statement.layout)
	 genStatement
      val _ =
	 M.Program.setMain
	 (mprogram, {chunkLabel = Chunk.label (funcChunk main),
		     label = funcToLabel main})
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
			      else VarOperand.Const (M.Program.newGlobal
						     (mprogram, ty)),
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
	    in
	       val {handlerOffset, labelInfo = labelRegInfo, ...} =
		  AllocateRegisters.allocate {chunk = chunk,
					      function = f,
					      labelChunk = labelChunk,
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
		       (M.Operand.stackOffset {offset = shift + offset, 
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
			(M.Transfer.arith {args = varOperands args,
					   dst = varOperand dst,
					   overflow = overflow,
					   prim = prim,
					   success = success})
		   | R.Transfer.Bug => simple M.Transfer.bug
		   | R.Transfer.CCall {args, prim, return, returnTy} =>
			simple (M.Transfer.ccall {args = translateOperands args,
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
						    (M.Operand.stackOffset 
						     {offset = handlerOffset,
						      ty = Type.label},
						     M.Operand.stackOffset 
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
				    M.Transfer.farJump
				    {chunkLabel = Chunk.label chunk',
				     label = funcToLabel func,
				     live = (Vector.toList
					     (Vector.concat [handlerLive, dsts])),
				     return = return}
			      else M.Transfer.nearJump {label = funcToLabel func,
							return = return}
			in (setupArgs, transfer)
			end
		   | R.Transfer.Goto {dst, args} =>
			(parallelMove {srcs = translateOperands args,
				       dsts = labelArgOperands dst,
				       chunk = labelChunk dst},
			 M.Transfer.nearJump {label = dst,
					      return = NONE})
		   | R.Transfer.LimitCheck {kind, return} =>
			let
			   datatype z = datatype R.LimitCheck.t
			   val kind =
			      case kind of
				 Array {numElts, bytesPerElt, extraBytes} =>
				    M.LimitCheck.Array
				    {numElts = varOperand numElts,
				     bytesPerElt = bytesPerElt,
				     extraBytes = extraBytes}
			       | Heap z => M.LimitCheck.Heap z
			       | Signal => M.LimitCheck.Signal
			       | Stack => M.LimitCheck.Stack
			   (* It doesn't matter whether we use live or
			    * liveNoFormals, since the return is nullary.
			    *)
			   val {live, size, ...} = labelRegInfo return
			in
			   simple
			   (M.Transfer.limitCheck {frameSize = size,
						   kind = kind,
						   live = live,
						   return = return})
			end
		   | R.Transfer.Raise srcs =>
			(M.Statement.moves
			 {dsts = raiseOperands (Vector.map
						(srcs, R.Operand.ty)),
			  srcs = translateOperands srcs},
			 M.Transfer.raisee)
		   | R.Transfer.Return xs =>
			let
			   val dsts = callReturnOperands (xs, R.Operand.ty, 0)
			in
			   (parallelMove {chunk = chunk,
					  srcs = translateOperands xs,
					  dsts = dsts},
			    M.Transfer.return {live = Vector.toList dsts})
			end
		   | R.Transfer.Runtime {prim, args, return} => 
			simple
			(M.Transfer.runtime
			 {args = Vector.map (args, translateOperand),
			  frameSize = #size (labelRegInfo return),
			  prim = prim,
			  return = return})
		   | R.Transfer.Switch {cases, default, test} =>
			simple (M.Transfer.switch {cases = cases,
						   default = default,
						   test = translateOperand test})
		   | R.Transfer.SwitchIP {int, pointer, test} =>
			simple (M.Transfer.switchIP
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
			   kind = M.Block.Kind.func {args = live},
			   live = live,
			   profileInfo = {func = profileInfoFunc,
					  label = profileInfoFunc},
			   statements = Vector.new0 (),
			   transfer = (M.Transfer.nearJump
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
			R.Block.Kind.Cont {handler} =>
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
				 M.Program.newFrame
				 (mprogram, {return = label,
					     chunkLabel = Chunk.label chunk,
					     size = size,
					     live = liveNoFormals})
			      val srcs = callReturnOperands (args, #2, 0)
			   in
			      (M.Block.Kind.cont {args = Vector.toList srcs,
						  size = size},
			       parallelMove
			       {chunk = chunk,
				dsts = Vector.map (args, varOperand o #1),
				srcs = srcs})
			   end
		      | R.Block.Kind.CReturn =>
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
			      (M.Block.Kind.creturn ret, Vector.new0 ())
			   end
		      | R.Block.Kind.Handler =>
			   let
			      val _ =
				 M.Program.newHandler
				 (mprogram, {chunkLabel = Chunk.label chunk,
					     label = label})
			      val offset = valOf handlerOffset
			      val dsts = Vector.map (args, varOperand o #1)
			   in (M.Block.Kind.handler {offset = offset},
			       M.Statement.moves
			       {dsts = dsts,
				srcs = raiseOperands (Vector.map (dsts,
								  M.Operand.ty))})
			   end
		      | R.Block.Kind.Normal =>
			   (M.Block.Kind.jump, Vector.new0 ())
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
      (* The M.Program.clear is necessary because Funcs and Labels are turned
       * into Labels in the resulting mprogram, and properties have been
       * attached to them by the backend.
       *)
      val _ = M.Program.clear mprogram
   in
      mprogram
   end

end
   
