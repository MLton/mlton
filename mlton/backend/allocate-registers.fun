(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor AllocateRegisters (S: ALLOCATE_REGISTERS_STRUCTS): ALLOCATE_REGISTERS = 
struct

open S

local open Ssa
in
   structure Sblock = Block
   structure Exp = Exp
   structure Func = Func
   structure Function = Function
   structure Handler = Handler
   structure Slabel = Label
   structure Prim = Prim
   structure Program = Program
   structure Return = Return
   structure Sstatement = Statement
   structure Stransfer = Transfer
   structure Stype = Type
   structure Var = Var
end

local open Machine
in
   structure Chunk = Chunk
   structure GCInfo = GCInfo
   structure Operand = Operand
   structure MlimitCheck = LimitCheck
   structure MprimInfo = PrimInfo   
   structure Mtype = Type
   structure Register = Register
   structure Statement = Statement
end

val traceAllocateBlock =
   Trace.trace ("Allocate.block",
		fn (Tree.T (Sblock.T {label, ...}, _),
		    _: Chunk.t) => Slabel.layout label,
		Unit.layout)

val traceAllocateStatement =
   Trace.trace ("Allocate.statement",
		fn (s, _: Chunk.t) => Sstatement.layout s,
		Unit.layout)

val traceForceStack =
   Trace.trace ("Allocate.forceStack", Var.layout, Unit.layout)
   
(* If a handler is stored in a stack frame, then we need both a uint for
 * the old handler and space for the handler itself
 *)
local
   open Mtype
in
   val labelSize = size label
   val handlerSize = labelSize + size uint
end

structure Live = Live (open Ssa)
structure LimitCheck = LimitCheck (open Ssa)

structure Info =
   struct
      datatype t =
	 T of {
	       limitCheck: Machine.LimitCheck.t,
	       live: Operand.t list,
	       liveNoFormals: Operand.t list,
	       liveFrame: (Handler.t * Operand.t list) list,
	       size: int,
	       adjustSize: int -> {size: int, shift: int}
	       }

      local
	 fun make f (T r) = f r
      in
	 val live = make #live
	 val size = make #size
      end

      fun layout (T {limitCheck, 
		     live, liveNoFormals, liveFrame,
		     size, adjustSize}) =
	 Layout.record
	 [("limitCheck", Machine.LimitCheck.layout limitCheck),
	  ("live", List.layout Operand.layout live),
	  ("liveNoFormals", List.layout Operand.layout liveNoFormals),
	  ("liveFrame", 
	   List.layout
	   (Layout.tuple2 (Handler.layout, List.layout Operand.layout)) 
	   liveFrame),
	  ("size", Int.layout size)]
   end

nonfix ^
fun ^ r = valOf (!r)

(* ------------------------------------------------- *)
(*                     allocate                      *)
(* ------------------------------------------------- *)

fun allocate {program = program as Program.T {globals, ...},
	      funcChunk,
	      isCont,
	      isHandler,
	      labelChunk,
	      labelToLabel,
	      varInfo: Var.t -> {operand: Machine.Operand.t option ref option,
				 primInfo: Machine.PrimInfo.t ref,
				 ty: Machine.Type.t}} =
   let
      val limitCheck = LimitCheck.limitCheck program
      val _ =
	 Vector.foreach
	 (globals, fn Sstatement.T {var, exp, ...} =>
	  case exp of
	     Exp.PrimApp {prim, ...} =>
	       let
		 val {primInfo, ...} = varInfo (valOf var)
	       in 
		 if Prim.entersRuntime prim
		   then primInfo :=
		        MprimInfo.runtime (GCInfo.make {frameSize = labelSize,
							live = []})
		   else primInfo :=  MprimInfo.normal []
	       end
	   | _ => ())
      fun diagnostics f =
	 Control.diagnostics
	 (fn display =>
	  let
	     open Layout
	     fun diagVar x =
		display (seq
			 [Var.layout x, str " ",
			  Option.layout
			  (fn r => Option.layout Operand.layout (!r))
			  (#operand (varInfo x))])
	     fun diagStatement (Sstatement.T {var, ...}) =
		Option.app (var, diagVar)
	  in
	     f (display, diagVar, diagStatement)
	  end)
      val _ = diagnostics (fn (display, _, diagStatement) =>
			   (display (Layout.str "Global allocs:")
			    ; Vector.foreach (globals, diagStatement)))
   in
      fn f =>
      let
	 val _ =
	    Control.diagnostic (fn () =>
				let open Layout
				in seq [str "Function allocs for ",
					Func.layout (Function.name f)]
				end)
	 val limitCheck = limitCheck f
	 val {get = labelInfo: Slabel.t -> Info.t,
	      set = setSlabelInfo, ...} =
	    Property.getSetOnce
	    (Slabel.plist,
	     Property.initRaise ("label info", Slabel.layout))
	 val setSlabelInfo =
	    Trace.trace2
	    ("Allocate.setSlabelInfo", Slabel.layout, Info.layout, Unit.layout)
	    setSlabelInfo
	 val {get = funcInfo: Func.t -> {
					 info: Info.t,
					 handlerOffset: int option
					 }, 
	      set = setFuncInfo, ...} =
	    Property.getSetOnce (Func.plist,
				 Property.initRaise ("func info", Func.layout))
	 val {labelLive, primLive} =
	    Live.live (f, {shouldConsider = isSome o #operand o varInfo})
	 val liveBegin = #begin o labelLive
	 val liveBeginNoFormals = #beginNoFormals o labelLive
	 val livePrim = #vars o primLive
	 fun liveBegin' l =
	    let val i = labelLive l
	    in (#begin i, #handlerSlots i)
	    end
	 fun liveBeginNoFormals' l =
	    let val i = labelLive l
	    in (#beginNoFormals i, #handlerSlots i)
	    end
	 fun liveBeginFrame' l =
	    let val i = labelLive l
	    in (#frame i, #handlerSlots i)
	    end
	 fun livePrim' x =
	    let val i = primLive x
	    in (#vars i, #handlerSlots i)
	    end
	 val {args, blocks, name, start, ...} = Function.dest f
	 (*
	  * Decide which variables will live in stack slots and which
	  * will live in registers.
	  * Initially,
	  *   - all formals are put in stack slots
	  *   - everything else is put everything in a register.
	  * Variables get moved to the stack if they are
	  *   - live at the beginning of a basic block (i.e. Fun dec)
	  *   - live at a primitive that enters the runtime system
	  *)
	 datatype place = Stack | Register
	 val {get = place: Var.t -> place, set = setPlace, ...} =
	    (* FIXME: could use destGetSetOnce? *)
	    Property.getSet (Var.plist, Property.initConst Register)
	 (* !hasHandler = true iff handlers are installed in this function. *)
	 val hasHandler: bool ref = ref false
	 fun forceStack (x: Var.t): unit = setPlace (x, Stack)
	 val forceStack = traceForceStack forceStack
	 fun forceStacks (xs: Var.t list): unit = List.foreach (xs, forceStack)
	 val _ = Vector.foreach (args, forceStack o #1)
	 val _ =
	    Vector.foreach
	    (blocks, fn Sblock.T {label, args, statements, transfer, ...} =>
	     let
	        val _ = if isCont label andalso !Control.stackCont
			  then Vector.foreach(args, forceStack o #1)
			else ()
		val _ =
		   case limitCheck label of
		      LimitCheck.No => ()
		    | _ => forceStacks (liveBegin label)
		val _ =
		   Vector.foreach
		   (statements, fn Sstatement.T {var, exp, ...} =>
		    case exp of
		       Exp.PrimApp {prim, args, ...} =>
			  (* Array_array is treated specially because a
			   * limit check is inserted before the allocation,
			   * which refers to the size.  Therefore the size
			   * is live at the limit check.
			   *)
			  if Prim.name prim = Prim.Name.Array_array
			     then (forceStacks
				   (Vector.sub (args, 0)
				    :: livePrim (valOf var)))
			  else if Prim.entersRuntime prim
				  then forceStacks (livePrim (valOf var))
			       else ()
		     | Exp.SetHandler h => hasHandler := true
		     | Exp.SetExnStackLocal => hasHandler := true
		     | Exp.SetExnStackSlot => hasHandler := true
		     | Exp.SetSlotExnStack => hasHandler := true
		     | _ => ())
		val _ =
		   case transfer of
		      Stransfer.Call {return = Return.NonTail {cont, handler, ...}, ...} =>
			 (forceStacks (liveBeginNoFormals cont)
			  ; (Handler.foreachLabel
			     (handler, fn l =>
			      forceStacks (liveBeginNoFormals l))))
			 
		    | _ => ()
	     in
		()
	     end)
	 (* The next available offset on the stack.
	  * It is labelSize initially because the stack pointer always points
	  * at the return label in the previous frame.
	  *)
	 val nextOffset = ref labelSize
	 fun getNextOffset (ty: Mtype.t, size) =
	    let
	       val offset = Mtype.align (ty, !nextOffset)
	       val _ = nextOffset := offset + size
	    in
	       offset
	    end
	 (* The next available register number for each type. *)
	 val nextReg = Mtype.memo (fn _ => ref 0)
	 fun nextRegister (ty: Mtype.t, c: Chunk.t): Register.t =
	    let
	       val r = nextReg ty
	       val reg = Chunk.register (c, !r, ty)
	       val _ = Int.inc r
	    in
	       reg
	    end
	 fun allocateVarInfo (x: Var.t, 
			      {operand, ty, primInfo},
			      c: Chunk.t, 
			      force: bool): unit =
	    if force orelse isSome operand
	       then let
		       val oper =
			  case place x of
			     Stack =>
				Operand.stackOffset
				{ty = ty,
				 offset = getNextOffset (ty, Mtype.size ty)}
			   | Register =>
				Operand.register (nextRegister (ty, c))
		    in case operand of
		       NONE => ()
		     | SOME r => r := SOME oper
		    end
	    else ()
	 val allocateVarInfo =
	    Trace.trace4
	    ("Allocate.allocateVarInfo",
	     Var.layout,
	     fn {operand, ...} =>
	     Option.layout (Ref.layout (Option.layout Operand.layout)) operand,
	     Layout.ignore, Bool.layout, Unit.layout)
	    allocateVarInfo
	 fun allocateVar (x, c, f) = allocateVarInfo (x, varInfo x, c, f)
	 val chunk = funcChunk name
	 (* Get the stack slots for the formals.  This has to happen first
	  * So that the calling convention is followed.
	  *)
	 val _ = Vector.foreach (args, fn (x, _) =>
				 allocateVar (x, chunk, true))
	 val handlerOffset =
	    if !hasHandler
	       then (SOME (getNextOffset (Mtype.label, handlerSize)))
	    else NONE
	 local
	    fun getOperands ((xs: Var.t list,
			      (code, link): bool * bool),
			     force: bool) =
	       let
		  val ops = []
		  val ops =
		     if code
			then let
				val handlerOffset = valOf handlerOffset
			     in
				(Operand.stackOffset {offset = handlerOffset,
						      ty = Mtype.uint})
				:: ops
			     end
		     else ops
		  val ops =
		     if link
			then let
				val handlerOffset = valOf handlerOffset
			     in
				(Operand.stackOffset
				 {offset = handlerOffset + labelSize,
				  ty = Mtype.uint})
				:: ops
			     end
		     else ops
	       in
		  List.fold
		  (xs, ops, fn (x, operands) =>
		   let
		      val {operand, ty, ...} = varInfo x
		   in
		      case operand of
			 NONE => operands
		       | SOME r =>
			    if force
			       then (case place x of
					Register =>
					   Error.bug 
					   (concat
					    ["live register ",
					     Layout.toString (Var.layout x)])
				      | Stack =>
					   case Operand.deStackOffset (^r) of
					      NONE => Error.bug "live slot"
					    | SOME _ => (^r)::operands)
			    else (^r)::operands
		   end)
	       end
	 in
	    val getOperands =
	       Trace.trace ("Allocate.getOperands",
			    fn ((xs, (code, link)), force) =>
			    Layout.tuple [List.layout Var.layout xs,
					  Bool.layout code,
					  Bool.layout link,
					  Bool.layout force],
			    List.layout Operand.layout)
	       getOperands
	    fun getLiveOperands (j, force) =
	       let
		  val {begin, beginNoFormals, frame, handlerSlots = hs} =
		     labelLive j
	       in
		  {live = getOperands ((begin, hs), false),
		   liveNoFormals = getOperands ((beginNoFormals, hs), force),
		   liveFrame = List.map(frame, fn (h, frame) => 
					(h, getOperands ((frame, hs), force)))}
	       end
	    val getLiveOperands =
	       Trace.trace ("Allocate.getLiveOperands",
			    fn (j, force) =>
			    Layout.tuple [Slabel.layout j,
					  Bool.layout force],
			    fn {live, liveNoFormals, liveFrame} =>
			    Layout.record [("live", 
					    List.layout Operand.layout live),
					   ("liveNoFormals",
					    List.layout Operand.layout liveNoFormals),
					   ("liveFrame", 
					    List.layout 
					    (Layout.tuple2
					     (Handler.layout,
					      List.layout Operand.layout))
					    liveFrame)])
	                   getLiveOperands
	    fun getLivePrimOperands x = getOperands (livePrim' x, false)
	    fun getLivePrimRuntimeOperands x = getOperands (livePrim' x, true)
	 end
	 local
	    fun getGCInfo' live = GCInfo.make {frameSize = !nextOffset,
					       live = live}
	 in
	    val getGCInfo' = getGCInfo'
	    fun getGCInfo j = getGCInfo' (getOperands (liveBegin' j, true))
	    val getGCInfo =
	       Trace.trace ("Allocate.getGCInfo",
			    Slabel.layout,
			    GCInfo.layout)
                           getGCInfo
	    fun getGCInfoPrimRuntime x =
	       getGCInfo' (getLivePrimRuntimeOperands x)
	 end
	 fun allocateStatement (Sstatement.T {var, exp, ...},
				c: Chunk.t) =
	    case var of
	       NONE => ()
	     | SOME var =>
		  let
		     val info as {primInfo, ...} = varInfo var
		     val _ = allocateVarInfo (var, info, c, false)
		  in
		     case exp of
			Exp.PrimApp {prim, ...} =>
			   if Prim.entersRuntime prim
			      then
				 primInfo :=
				 MprimInfo.runtime
				 (getGCInfoPrimRuntime var)
			   else if Prim.impCall prim
				   then
				      primInfo :=
				      (MprimInfo.normal
				       (getLivePrimOperands var))
				else ()
		      | _ => ()
		  end
	 val allocateStatement = traceAllocateStatement allocateStatement
	 val todo : (unit -> unit) list ref = ref []
	 (* Descend the dominator tree. *)
	 fun loop arg : unit =
	    traceAllocateBlock
	    (fn (Tree.T (Sblock.T {args, label, statements, ...}, children),
		 c: Chunk.t) =>
	    let	
	       val isCont = isCont label
	       val isHandler = isHandler label
	       val c' = labelChunk label
	       val saveReg = Mtype.memo (! o nextReg)
	       val saveOffset = !nextOffset
	       val _ = List.foreach (Mtype.all, ignore o saveReg)
	       val _ =
		  if Chunk.equals (c, c')
		     then ()
		  else (* We can reset all of the register counters
			* because we know that no registers live
			* across a chunk boundary.
			*)
		     List.foreach (Mtype.all, fn t => nextReg t := 0)
	       fun adjustSize size
		 = let
		     val (offset,offset') =
		        Vector.fold
			(args, (0,size), fn ((x, _), (offset, offset')) =>
			 let val ty = #ty (varInfo x)
			 in (Mtype.align (ty, offset) + Mtype.size ty,
			     Mtype.align (ty, offset') + Mtype.size ty)
			 end)
		     val (offset,offset') = (Mtype.wordAlign offset,
					     Mtype.wordAlign offset')
		     val shift = Int.abs((offset' - size) - offset)
		   in
		     {size = size + shift + offset,
		      shift = shift}
		   end
	       val _ = if isCont andalso !Control.newReturn
			 then let
				val {shift, ...} = adjustSize saveOffset
			      in
				nextOffset := saveOffset + shift
			      end
		       else ()
	       val _ = Vector.foreach (args, fn (x, _) =>
				       allocateVar (x, c', false))
	       (* This must occur after allocating slots for the
		* args, since it must have the correct stack frame
		* size.
		*)
	       val limitCheck = 
		  let
		     fun doit make = make (getGCInfo label) 
		  in
		     case limitCheck label of
			LimitCheck.No => MlimitCheck.No
		      | LimitCheck.Maybe => doit MlimitCheck.Maybe
		      | LimitCheck.Yes => doit MlimitCheck.Yes
		  end
	       val _ = Vector.foreach (statements, fn s =>
				       allocateStatement (s, c'))
	       val bodyOffset = !nextOffset
	       val _ = Vector.foreach (children, fn n => loop (n, c'))
	       val _ = List.foreach (Mtype.all, fn t => nextReg t := saveReg t)
	       val _ = nextOffset := saveOffset
	       val size = saveOffset
	       val th = fn () => let
				   val {live, liveNoFormals, liveFrame} =
				      getLiveOperands (label, 
						       isCont orelse isHandler)
				 in 
				    setSlabelInfo
				    (label, Info.T {limitCheck = limitCheck,
						    live = live,
						    liveNoFormals = liveNoFormals,
						    liveFrame = liveFrame,
						    size = size,
						    adjustSize = adjustSize})
				 end
	       val _ = if isCont
			  then (* If this is a cont, then some handlers paired
				* with it may not have yet been allocated;
				* this can cause getLiveOperands to fail when
				* looping through the liveFrames, which include
				* variables live down the handlers.
				* So, delay it for now and do it after all allocation
				* has taken place.
				*)
			       List.push(todo, th)
		       else th ()
	    in ()
	    end) arg
	 val _ = loop (Function.dominatorTree f, chunk)
	 val _ = List.foreach(!todo, fn th => th ())
	 val Info.T {live, ...} = labelInfo start
	 val limitCheck = MlimitCheck.Stack (getGCInfo' live)
	 val _ =
	    diagnostics
	    (fn (display, diagVar, diagStatement) =>
	     let
		open Layout
		val _ =
		   display (seq [str "function ", Func.layout name,
				 str " handlerOffset ",
				 Option.layout Int.layout handlerOffset])
		val _ = Vector.foreach (args, diagVar o #1)
		val _ =
		   Vector.foreach
		   (blocks, fn Sblock.T {label, args, statements, ...} =>
		    (display (Slabel.layout label)
		     ; Vector.foreach (args, diagVar o #1)
		     ; Vector.foreach (statements, diagStatement)))
	     in ()
	     end)
      in
	 {handlerOffset = handlerOffset,
	  labelInfo = labelInfo,
	  limitCheck = limitCheck}
      end
   end
end
