(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor AllocateRegisters (S: ALLOCATE_REGISTERS_STRUCTS): ALLOCATE_REGISTERS = 
struct

open S

local open Cps
in
   structure Dec = Dec
   structure Exp = Exp
   structure Func = Func
   structure Function = Function
   structure Jump = Jump
   structure Prim = Prim
   structure PrimExp = PrimExp
   structure Program = Program
   structure Ctransfer = Transfer
   structure Var = Var
end

local open Machine
in
   structure Chunk = Chunk
   structure GCInfo = GCInfo
   structure Operand = Operand
   structure MlimitCheck = LimitCheck
   structure Mtype = Type
   structure Register = Register
   structure Statement = Statement
end

(* If a handler is stored in a stack frame, then we need both a uint for
 * the old handler and space for the handler itself
 *)
local
   open Mtype
in
   val labelSize = size label
   val handlerSize = labelSize + size uint
end

structure Live = Live (open Cps)
structure LimitCheck = LimitCheck (open Cps)

structure Info =
   struct
      datatype t =
	 T of {
	       limitCheck: Machine.LimitCheck.t,
	       live: Register.t list,
	       liveNoFormals: Register.t list
	       }

      fun layout (T {limitCheck, live, liveNoFormals}) =
	 Layout.record
	 [("limitCheck", Machine.LimitCheck.layout limitCheck),
	  ("live", List.layout Register.layout live),
	  ("liveNoFormals", List.layout Register.layout liveNoFormals)]
   end 

nonfix ^
fun ^ r = valOf (!r)

(* ------------------------------------------------- *)
(*                     allocate                      *)
(* ------------------------------------------------- *)

fun allocate {program = program as Program.T {globals, functions, ...},
	      funcChunk, jumpChunk, jumpHandlers,
	      varInfo: Var.t -> {operand: Machine.Operand.t option ref option,
				 primInfo: Machine.GCInfo.t option ref,
				 ty: Machine.Type.t}} =
   let
      val shouldAllocate = isSome o #operand o varInfo
      val {destroy = destroyLimitCheck, limitCheck} =
	 LimitCheck.limitCheck program
      val {get = jumpInfo: Jump.t -> Info.t,
	   set = setJumpInfo} =
	 Property.getSetOnce (Jump.plist,
			      Property.initRaise ("jump info", Jump.layout))
      val setJumpInfo =
	 Trace.trace2 ("setJumpInfo", Jump.layout, Info.layout, Unit.layout)
	 setJumpInfo
      val {get = contInfo: Jump.t ->  {
				       size: int,
				       liveOffsets: int list
				       },
	   set = setContInfo} =
	 Property.getSetOnce (Jump.plist,
			      Property.initRaise ("cont info", Jump.layout))
      val setContInfo =
	 Trace.trace2 ("setContInfo",
		       Jump.layout,
		       fn {size, liveOffsets} =>
		       Layout.record
		       [("size", Int.layout size),
			("liveOffsets", List.layout Int.layout liveOffsets)],
		       Unit.layout)
	 setContInfo
      val {get = handlerInfo: Jump.t ->  {
					  size: int,
					  liveOffsets: int list
					  },
	   set = setHandlerInfo} =
	 Property.getSetOnce (Jump.plist,
			      Property.initRaise ("handler info", Jump.layout))
      val {get = funcInfo, set = setFuncInfo} =
	 Property.getSetOnce (Func.plist,
			      Property.initRaise ("func info", Func.layout))
      fun allocateFunc (name: Func.t,
			args: (Var.t * Cps.Type.t) vector,
			body: Exp.t): unit = 
	 let
	    val {liveBegin, liveBeginNoFormals, livePrim,
		 destroy = destroyLive} =
	       Live.live {formals = args,
			  exp = body,
			  shouldConsider = shouldAllocate,
			  jumpHandlers = jumpHandlers}
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
	    val {get = place: Var.t -> place,
		 set = setPlace, destroy = destroyPlace} =
	       (* FIXME: could use destGetSetOnce? *)
	       Property.destGetSet (Var.plist, Property.initConst Register)
	    val {get = isCont: Jump.t -> bool,
		 set = setIsCont, destroy = destroyIsCont} =
	       (* Can't be setOnce because conts could appear in multiple
		* nontail calls.
		*)
	       Property.destGetSet (Jump.plist, Property.initConst false)
	    val setIsCont =
	       Trace.trace2
	       ("setIsCont", Jump.layout, Bool.layout, Unit.layout)
	       setIsCont
	    val {get = isHandler: Jump.t -> bool,
		 set = setIsHandler, destroy = destroyIsHandler} =
	       (* Can't be setOnce because handler could appear in multiple
		* handler pushes.
		*)
	       Property.destGetSet (Jump.plist, Property.initConst false)
	    val setIsHandler =
	       Trace.trace2
	       ("setIsHandler", Jump.layout, Bool.layout, Unit.layout)
	       setIsHandler
	    (* !hasHandler = true iff handlers are installed in this function. *)
	    val hasHandler: bool ref = ref false
	    fun forceStack (x: Var.t): unit = setPlace (x, Stack)
	    fun forceStacks (xs: Var.t list): unit =
	       List.foreach (xs, forceStack)
	    val _ = Vector.foreach (args, forceStack o #1)
	    fun loopExp (e: Exp.t): unit =
	       let val {decs, transfer} = Exp.dest e
	       in List.foreach (decs, loopDec)
		  ; (case transfer of
			Ctransfer.Call {cont = SOME c, ...} =>
			   (setIsCont (c, true)
			    ; forceStacks (liveBeginNoFormals c))
		      | _ => ())
	       end
	    and loopDec (d: Dec.t) =
	       case d of
		  Dec.Fun {name, body, ...} =>
		     ((case limitCheck name of
			  LimitCheck.No => ()
			| _ => forceStacks (liveBegin name))
			  ; loopExp body)
		| Dec.HandlerPush h => (setIsHandler (h, true)
					; hasHandler := true
					; forceStacks (liveBeginNoFormals h))
		| Dec.Bind {var, exp = PrimExp.PrimApp {prim, args, ...}, ...} =>
		     let
			datatype z = datatype Prim.Name.t
		     in case Prim.name prim of
			Array_array =>
			   (* Array_array is treated specially because a limit
			    * check is inserted before the allocation, which
			    * refers to the size.  Therefore the size is live
			    * at the limit check.
			    *)
			   forceStacks (Vector.sub (args, 0) :: livePrim var)
		      | _ => if Prim.entersRuntime prim
				then forceStacks (livePrim var)
			     else ()
		     end
		| _ => ()
	    val _ = loopExp body
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
	       let val r = nextReg ty
		  val reg = Chunk.register (c, !r, ty)
	       in Int.inc r
		  ; reg
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
	    fun allocateVar (x, c, f) = allocateVarInfo (x, varInfo x, c, f)
  	    val allocateVar =
  	       Trace.trace3
	       ("allocateVar", Var.layout, Layout.ignore, Bool.layout,
		Unit.layout)
  	       allocateVar
	    val chunk = funcChunk name
	    (* Get the stack slots for the formals.  This has to happen first
	     * So that the calling convention is followed.
	     *)
	    val _ = Vector.foreach (args, fn (x, _) =>
				    allocateVar (x, chunk, true))
	    val handlerOffset =
	       if !hasHandler
		  then SOME (getNextOffset (Mtype.label, handlerSize))
	       else NONE
	    fun getMask (xs: Var.t list): int list =
	       List.fold
	       (xs, [], fn (x, offsets) =>
		let
		   val {operand, ty, ...} = varInfo x
		in
		   case (operand, Mtype.isPointer ty) of
		      (SOME r, true) => 		 
			 (case place x of
			     Register =>
				Error.bug "can't have live pointer register"
			   | Stack =>
				case Operand.deStackOffset (^r) of
				   NONE => Error.bug "must be a slot"
				 | SOME  {offset, ...} => offset :: offsets)
		    | _ => offsets
		end)
	    val getMask =
	       Trace.trace ("getMask", List.layout Var.layout,
			    List.layout Int.layout) getMask
	    fun getGCInfo (live: Var.t list) =
	       GCInfo.make {frameSize = !nextOffset,
			    offsets = getMask live}
	    val traceAllocate =
	       Trace.trace ("allocate", Exp.layout o #1, Unit.layout)
	    fun allocate arg =
	       traceAllocate
	       (fn (e: Exp.t, c: Chunk.t) =>
		List.foreach
		(Exp.decs e,
		 fn Dec.Bind {var, exp, ...} =>
		 let
		    val info as {primInfo, ...} = varInfo var
		    val _ = allocateVarInfo (var, info, c, false)
		    val _ =
		       case exp of
			  PrimExp.PrimApp {prim, ...} =>
			     if Prim.entersRuntime prim
				then (primInfo :=
				      SOME (getGCInfo (livePrim var)))
			     else ()
			| _ => ()
		 in ()
		 end
		  | Dec.Fun {name, args, body, ...} =>
		       let
			  val saveReg = Mtype.memo (! o nextReg)
			  val saveOffset = !nextOffset
			  val _ = List.foreach (Mtype.all, ignore o saveReg)
			  val c' = jumpChunk name
			  val _ =
			     if Chunk.equals (c, c')
				then ()
			     else (* We can reset all of the register counters
				   * because we know that no registers live
				   * across a chunk boundary.
				   *)
				List.foreach (Mtype.all, fn t => nextReg t := 0)
			  val _ = Vector.foreach (args, fn (x, _) =>
						  allocateVar (x, c', false))
			  (* This must occur after allocating slots for the
			   * args, since it must have the correct stack frame
			   * size.
			   *)
			  val limitCheck = 
			     let
				fun doit make =
				   make (getGCInfo (liveBegin name))
			     in case limitCheck name of
				LimitCheck.No => MlimitCheck.No
			      | LimitCheck.Maybe => doit MlimitCheck.Maybe
			      | LimitCheck.Yes => doit MlimitCheck.Yes
			     end
			  val _ = allocate (body, c')
			  val _ = List.foreach (Mtype.all, fn t =>
						nextReg t := saveReg t)
			  val _ = nextOffset := saveOffset
			  fun getLiveOffsets () =
			     getMask (liveBeginNoFormals name)
			  val _ =
			     if isCont name
				then
				   setContInfo
				   (name, {size = Mtype.wordAlign saveOffset,
					   liveOffsets = getLiveOffsets ()})
			     else ()
			  val _ =
			     if isHandler name
				then
				   setHandlerInfo
				   (name, {size = valOf handlerOffset,
					   liveOffsets = getLiveOffsets ()})
			     else ()
			  fun getRegs (xs: Var.t list): Register.t list =
			     List.fold
			     (xs, [], fn (x, rs) =>
			      let val {operand, ...} = varInfo x
			      in case operand of
				 NONE => rs
			       | SOME oper => 
				    (case Operand.deRegister (^oper) of
					NONE => rs
				      | SOME r => r :: rs)
			      end)
			  val _ =
			     setJumpInfo
			     (name,
			      Info.T
			      {limitCheck = limitCheck,
			       live = getRegs (liveBegin name),
			       liveNoFormals = getRegs (liveBeginNoFormals name)})
		       in ()
		       end
		  | _ => ())) arg
	    val _ = allocate (body, chunk)
	    val limitCheck =
	       MlimitCheck.Stack (getGCInfo (Vector.toListMap (args, #1)))
	 in destroyPlace ()
	    ; destroyIsCont ()
	    ; destroyIsHandler ()
	    ; destroyLive ()
	    ; setFuncInfo (name,
			   {info = Info.T {limitCheck = limitCheck,
					   live = [],
					   liveNoFormals = []},
			    handlerOffset = handlerOffset})
	    ; ()
	 end
      val _ =
	 Vector.foreach
	 (globals, fn {var, exp, ...} =>
	  case exp of
	     PrimExp.PrimApp {prim, ...} =>
		if Prim.entersRuntime prim
		   then let
			   val {primInfo, ...} = varInfo var
			in
			   primInfo := SOME (GCInfo.make
					     {frameSize = labelSize,
					      offsets = []})
			end
		else ()
	   | _ => ())
      val _ =
	 Vector.foreach (functions, fn Function.T {name, args, body, ...} =>
			 allocateFunc (name, args, body))
      val _ =
	 Control.diagnostics
	 (fn display =>
	  let
	     open Layout
	     fun layoutVar x =
		display (seq
			 [Var.layout x, str " ",
			  Option.layout
			  (fn r => Option.layout Operand.layout (!r))
			  (#operand (varInfo x))])
	     fun loopBind {var, ty, exp} = layoutVar var
	     fun loop (args, body) =
		(Vector.foreach (args, layoutVar o #1)
		 ; List.foreach (#decs (Exp.dest body),
				 fn Dec.Bind b => loopBind b
				  | Dec.Fun {args, body, ...} =>
				       loop (args, body)
				  | _ => ()))
	     val _ = Vector.foreach (globals, loopBind)
	     val _ =
		Vector.foreach
		(functions, fn Function.T {name, args, body, ...} =>
		 let val {handlerOffset, ...} = funcInfo name
		 in display (seq [str "function ", Func.layout name,
				  str " handlerOffset ",
				  Option.layout Int.layout handlerOffset])
		    ; loop (args, body)
		 end)
	  in ()
	  end)
      val _ = destroyLimitCheck ()
   in
      {contInfo = contInfo,
       funcInfo = funcInfo,
       handlerInfo = handlerInfo,
       jumpInfo = jumpInfo}
   end

val allocate =
   Trace.trace
   ("allocate",
    fn {program, ...} => Cps.Program.layout program,
    Layout.ignore)
   allocate
   
end
