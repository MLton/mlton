(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor AllocateRegisters (S: ALLOCATE_REGISTERS_STRUCTS): ALLOCATE_REGISTERS = 
struct

open S
structure R = Rssa
structure M = Machine

local
   open Rssa
in
   structure Block = Block
   structure Func = Func
   structure Function = Function
   structure Kind = Kind
   structure Label = Label
   structure Var = Var
   structure Type = Type
end

local
   open Machine
in
   structure Operand = Operand
   structure Register = Register
end

val traceForceStack =
   Trace.trace ("Allocate.forceStack", Var.layout, Unit.layout)
   
(* If a handler is stored in a stack frame, then we need both a uint for
 * the old handler and space for the handler itself
 *)
local
   open Type
in
   val labelSize = size label
   val handlerSize = labelSize + size uint
end

structure Live = Live (open Rssa)

structure Stack:
   sig
      type t

      val empty: t
      val get: t * Type.t -> t * {offset: int}
(*      val getAt: t * {offset: int, size: int} -> t option *)
      val layout: t -> Layout.t
      (* new takes a list of already allocated nonoverlapping spots *)
      val new: {offset: int, ty: Type.t} list -> t
      val size: t -> int
   end =
   struct
      (* Keep a list of allocated slots sorted in increasing order of offset.
       *)
      datatype t = T of {offset: int, size: int} list

      fun layout (T alloc) =
	 List.layout (fn {offset, size} =>
		      Layout.record [("offset", Int.layout offset),
				     ("size", Int.layout size)])
	 alloc
	 
      val empty = T []

      fun size (T alloc) =
	 case alloc of
	    [] => 0
	  | _ => let
		    val {offset, size} = List.last alloc
		 in
		    offset + size
		 end

      fun new (alloc): t =
	 let
	    val a = Array.fromListMap (alloc, fn {offset, ty} =>
				       {offset = offset,
					size = Type.size ty})
	    val _ = QuickSort.sort (a, fn (r, r') => #offset r <= #offset r')
	 in
	    T (Array.toList a)
	 end

      fun get (T alloc, ty) =
	 let
	    val slotSize = Type.size ty
	 in
	    case alloc of
	       [] => (T [{offset = 0, size = slotSize}],
		      {offset = 0})
	     | a :: alloc =>
		  let
		     fun loop (alloc, a as {offset, size}, ac) =
			let
			   val prevEnd = offset + size
			   val begin = Type.align (ty, prevEnd)
			   fun coalesce () =
			      if prevEnd = begin
				 then ({offset = offset,
					size = size + slotSize},
				       ac)
			      else ({offset = begin, size = slotSize},
				    {offset = offset, size = size} :: ac)
			in
			   case alloc of
			      [] =>
				 let
				    val (a, ac) = coalesce ()
				 in
				    (T (rev (a :: ac)), {offset = begin})
				 end
			    | (a' as {offset, size}) :: alloc =>
				 if begin + slotSize > offset
				    then loop (alloc, a', a :: ac)
				 else
				     let
					val (a'' as {offset = o', size = s'}, ac)
					   = coalesce ()
					val alloc =
					   List.appendRev
					   (ac,
					    if o' + s' = offset
					       then
						  {offset = o', size = size + s'}
						  :: alloc
					    else a'' :: a' :: alloc)
				     in
					(T alloc, {offset = begin})
				     end
			end
		  in
		     loop (alloc, a, [])
		  end
	 end
   end

structure Info =
   struct
      type t = {
		live: Operand.t list,
		liveNoFormals: Operand.t list,
		size: int,
		adjustSize: int -> {size: int, shift: int}
		}

      fun layout ({live, liveNoFormals, size, ...}: t) =
	 Layout.record
	 [("live", List.layout Operand.layout live),
	  ("liveNoFormals", List.layout Operand.layout liveNoFormals),
	  ("size", Int.layout size)]
   end

nonfix ^
fun ^ r = valOf (!r)

(* ------------------------------------------------- *)
(*                     allocate                      *)
(* ------------------------------------------------- *)

fun allocate {function = f: Rssa.Function.t,
	      newRegister,
	      varInfo: Var.t -> {operand: Machine.Operand.t option ref option,
				 ty: Machine.Type.t}} =
   let
      fun diagnostics f =
	 Control.diagnostics
	 (fn display =>
	  let
	     open Layout
	     fun diagVar (x: Var.t): unit =
		display (seq
			 [Var.layout x, str " ",
			  Option.layout
			  (fn r => Option.layout Operand.layout (!r))
			  (#operand (varInfo x))])
	     fun diagStatement (s: R.Statement.t): unit =
		R.Statement.forDef (s, diagVar o #var)
	  in
	     f (display, diagVar, diagStatement)
	  end)
      val _ =
	 Control.diagnostic (fn () =>
			     let open Layout
			     in seq [str "Function allocs for ",
				     Func.layout (Function.name f)]
			     end)
      val {get = labelInfo: R.Label.t -> Info.t, set = setLabelInfo, ...} =
	 Property.getSetOnce (R.Label.plist,
			      Property.initRaise ("label info", R.Label.layout))
      val setLabelInfo =
	 Trace.trace2
	 ("Allocate.setLabelInfo", R.Label.layout, Info.layout, Unit.layout)
	 setLabelInfo
      val {get = funcInfo: Func.t -> {
				      info: Info.t,
				      handlerOffset: int option
				      }, 
	   set = setFuncInfo, ...} =
	 Property.getSetOnce (Func.plist,
			      Property.initRaise ("func info", Func.layout))
      val labelLive =
	 Live.live (f, {shouldConsider = isSome o #operand o varInfo})
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
	 val _ = Vector.foreach (args, forceStack o #1)
	 val _ =
	    Vector.foreach
	    (blocks,
	     fn R.Block.T {args, kind, label, statements, transfer, ...} =>
	     let
		val {begin, beginNoFormals, ...} = labelLive label
	        val _ =
		   case kind of
		      Kind.Cont _ =>
			 (Vector.foreach (args, forceStack o #1)
			  ; List.foreach (beginNoFormals, forceStack))
		    | Kind.Handler =>
			 List.foreach (beginNoFormals, forceStack)
		    | Kind.Runtime =>
			 List.foreach (beginNoFormals, forceStack)
		    | _ => ()
		val _ =
		   Vector.foreach
		   (statements, fn s =>
		    let
		       datatype z = datatype R.Statement.t
		    in
		       case s of
			  SetHandler h => hasHandler := true
			| SetExnStackLocal => hasHandler := true
			| SetExnStackSlot => hasHandler := true
			| SetSlotExnStack => hasHandler := true
			| _ => ()
		    end)
	     in
		()
	     end)
	 (* The next available register number for each type. *)
	 val nextReg = Type.memo (fn _ => ref 0)
	 fun allocateVarInfo (x: Var.t,
			      {operand, ty},
			      l: Label.t option, 
			      force: bool,
			      s: Stack.t): Stack.t =
	    if force orelse isSome operand
	       then let
		       val (s, oper) =
			  case place x of
			     Stack =>
				let
				   val (s, {offset}) = Stack.get (s, ty)
				in
				   (s, Operand.StackOffset {ty = ty,
							    offset = offset})
				end
			   | Register =>
				let
				   val r = nextReg ty
				   val reg = newRegister (l, !r, ty)
				   val _ = Int.inc r
				in
				   (s, Operand.Register reg)
				end
		       val _ = 
			  case operand of
			     NONE => ()
			   | SOME r => r := SOME oper
		    in
		       s
		    end
	    else s
	 val allocateVarInfo =
	    Trace.trace5
	    ("Allocate.allocateVarInfo",
	     Var.layout,
	     fn {operand, ...} =>
	     Option.layout (Ref.layout (Option.layout Operand.layout)) operand,
	     Layout.ignore, Bool.layout, Stack.layout,
	     Stack.layout)
	    allocateVarInfo
	 fun allocateVar (x, c, f, s) = allocateVarInfo (x, varInfo x, c, f, s)
	 (* The initial stack has one element, a label, because the stack
	  * pointer always points at the return label in the previous frame.
	  *)
	 val (stack, _) = Stack.get (Stack.empty, Type.label)
	 (* Get the stack slots for the formals.  These are done in the
	  * initial stack so that the calling convention is followed.
	  *)
	 val stack =
	    Vector.fold (args, stack, fn ((x, _), s) =>
			 allocateVar (x, NONE, true, s))
	 (* Allocate slots for the link and handler, if necessary.
	  * This code relies on Stack.get putting them together, since for now
	  * the machine IL assumes they are and only has the ability to refer
	  * to one of them.
	  *)
	 val (stack, handlerOffset) =
	    if !hasHandler
	       then
		  let
		     val (s, {offset}) = Stack.get (stack, Type.label)
		     val (s, {...}) = Stack.get (s, Type.uint)
		  in
		     (s, SOME offset)
		  end
	    else (stack, NONE)
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
				(Operand.StackOffset {offset = handlerOffset,
						      ty = Type.uint})
				:: ops
			     end
		     else ops
		  val ops =
		     if link
			then let
				val handlerOffset = valOf handlerOffset
			     in
				(Operand.StackOffset
				 {offset = handlerOffset + labelSize,
				  ty = Type.uint})
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
	 end
	 (* Do a DFS of the control-flow graph. *)
	 val _ =
	    Function.dfs
	    (f, fn R.Block.T {args, label, kind, statements, ...} =>
	     let
		val {begin, beginNoFormals, handlerSlots = hs} = labelLive label
		val live = getOperands ((begin, hs), false)
		val liveNoFormals =
		   getOperands ((beginNoFormals, hs),
				case kind of
				   Kind.Cont _ => true
				 | Kind.CReturn => false
				 | Kind.Handler => true
				 | Kind.Normal => false
				 | Kind.Runtime => true)
		val stack =
		   Stack.new (List.fold (liveNoFormals, [], fn (oper, ac) =>
					 case Operand.deStackOffset oper of
					    NONE => ac
					  | SOME a => a :: ac))
		fun adjustSize size =
		   let
		      val (offset, offset') =
			 Vector.fold
			 (args, (0, size), fn ((x, _), (offset, offset')) =>
			  let val ty = #ty (varInfo x)
			  in (Type.align (ty, offset) + Type.size ty,
			      Type.align (ty, offset') + Type.size ty)
			  end)
		      val offset = Type.wordAlign offset
		      val offset' = Type.wordAlign offset'
		      val shift = Int.abs ((offset' - size) - offset)
		   in
		      {size = size + shift + offset,
		       shift = shift}
		   end
		val _ =
		   if !Control.newReturn
		      andalso (case kind of
				  Kind.Cont _ => true
				| _ => false)
		      then Error.bug "newReturn not implemented"
		   else ()
		val s =
		   Vector.fold (args, stack, fn ((x, _), s) =>
				allocateVar (x, SOME label, false, s))
		val _ =
		   Vector.fold
		   (statements, s, fn (s, stack) =>
		    R.Statement.foldDef
		    (s, stack, fn ({var, ...}, stack) =>
		     allocateVar (var, SOME label, false, stack)))
		val _ = setLabelInfo (label, {live = live,
					       liveNoFormals = liveNoFormals,
					       size = Stack.size stack,
					       adjustSize = adjustSize})
	     in
		fn () => ()
	     end)
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
		   (blocks, fn R.Block.T {label, args, statements, ...} =>
		    (display (R.Label.layout label)
		     ; Vector.foreach (args, diagVar o #1)
		     ; Vector.foreach (statements, diagStatement)))
	     in ()
	     end)
      in
	 {handlerOffset = handlerOffset,
	  labelInfo = labelInfo}
      end

val allocate = 
   Trace.trace
   ("AllocateRegisters.allocate",
    fn {function, ...} => Func.layout (Function.name function),
    Layout.ignore)
   allocate
   
end
