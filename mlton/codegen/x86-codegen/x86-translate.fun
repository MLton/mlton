(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor x86Translate(S: X86_TRANSLATE_STRUCTS): X86_TRANSLATE =
struct

  open S

  val tracer = x86.tracer
  val tracerTop = x86.tracerTop

  val wordBytes = x86MLton.wordBytes
  val pointerBytes = x86MLton.pointerBytes
  val normalHeaderBytes = x86MLton.normalHeaderBytes
  val arrayHeaderBytes = x86MLton.arrayHeaderBytes
  val intInfOverheadBytes = x86MLton.intInfOverheadBytes

  fun argsToString(ss: string list): string
    = "(" ^ (concat (List.separate(ss, ", "))) ^ ")"

  structure Machine = x86MLton.Machine

  local
     open Machine
  in
     structure Label = Label
     structure Prim = Prim
     structure Register = Register
     structure Runtime = Runtime
     structure Type = Type
  end
  
  structure Global =
     struct
	open Machine.Global

	fun toX86MemLoc (g: t) =
	   let
	      val ty = Machine.Type.toRuntime (ty g)
	      val base =
		 x86.Immediate.label
		 (if isRoot g
		     then x86MLton.global_base ty
		  else x86MLton.globalPointerNonRoot_base)
	   in
	      x86.MemLoc.imm {base = base,
			      index = x86.Immediate.const_int (index g),
			      scale = x86MLton.toX86Scale ty,
			      size = x86MLton.toX86Size ty,
			      class = x86MLton.Classes.Globals}
	   end

	val toString = Layout.toString o layout
     end

  structure Operand =
    struct
      open Machine.Operand

      val toX86Size = x86MLton.toX86Size o Type.toRuntime o ty

      val rec toX86Operand =
	 fn ArrayOffset {base, index, ty} =>
	       let
		  val base = toX86Operand base
		  val index = toX86Operand index
		  val ty = Type.toRuntime ty
		  val memloc =
		     case (x86.Operand.deMemloc base,
			   x86.Operand.deImmediate index,
			   x86.Operand.deMemloc index) of
			(SOME base, SOME index, _) =>
			   x86.MemLoc.simple 
			   {base = base,
			    index = index,
			    scale = x86MLton.toX86Scale ty,
			    size = x86MLton.toX86Size ty,
			    class = x86MLton.Classes.Heap}
		      | (SOME base, _, SOME index) =>
			   x86.MemLoc.complex 
			   {base = base,
			    index = index,
			    scale = x86MLton.toX86Scale ty,
			    size = x86MLton.toX86Size ty,
			    class = x86MLton.Classes.Heap}
		      | _ => Error.bug (concat ["toX86Operand: strange Offset:",
						" base: ",
						x86.Operand.toString base,
						" index: ",
						x86.Operand.toString index])
	       in
		  x86.Operand.memloc memloc
	       end
	  | Cast (z, _) => toX86Operand z
	  | Char c => x86.Operand.immediate_const_char c
	  | Contents {oper, ty} =>
	       let
		  val ty = Type.toRuntime ty
		  val base = toX86Operand oper
		  val offset = x86.Immediate.const_int 0
		  val size = x86MLton.toX86Size ty
		  val memloc =
		     case x86.Operand.deMemloc base of
			SOME base =>
			   x86.MemLoc.simple 
			   {base = base,
			    index = x86.Immediate.const_int 0,
			    scale = x86.Scale.One,
			    size = x86MLton.toX86Size ty,
			    class = x86MLton.Classes.Heap}
		      | _ => Error.bug (concat
					["toX86Operand: strange Contents",
					 " base: ",
					 x86.Operand.toString base])
	       in
		  x86.Operand.memloc memloc
	       end
	  | File => x86MLton.fileName
	  | GCState => x86.Operand.label x86MLton.gcState_label
	  | Global g => x86.Operand.memloc (Global.toX86MemLoc g)
	  | Int i => x86.Operand.immediate_const_int i
	  | Label l => x86.Operand.immediate_label l
	  | Line => x86MLton.fileLine ()
	  | Offset {base, offset, ty} =>
	       let
		  val base = toX86Operand base
		  val ty = Type.toRuntime ty
		  val memloc =
		     case x86.Operand.deMemloc base of
			SOME base =>
			   x86.MemLoc.simple 
			   {base = base,
			    index = x86.Immediate.const_int offset,
			    scale = x86.Scale.One,
			    size = x86MLton.toX86Size ty,
			    class = x86MLton.Classes.Heap}
		      | _ => Error.bug (concat ["toX86Operand: strange Offset:",
						" base: ",
						x86.Operand.toString base])
	       in
		  x86.Operand.memloc memloc
	       end
	  | Real _ => Error.bug "toX86Operand: Real unimplemented"
	  | Register r =>
	       let
		  val ty = Machine.Type.toRuntime (Register.ty r)
		  val base = x86.Immediate.label (x86MLton.local_base ty)
	       in
		  x86.Operand.memloc
		  (x86.MemLoc.imm {base = base,
				   index = (x86.Immediate.const_int
					    (Register.index r)),
				   scale = x86MLton.toX86Scale ty,
				   size = x86MLton.toX86Size ty,
				   class = x86MLton.Classes.Locals})
	       end
	  | Runtime oper =>
		let
		   datatype z = datatype Machine.Runtime.GCField.t
		   open x86MLton
		in
		   case oper of
		      CanHandle => gcState_canHandleContentsOperand ()
		    | CardMap => gcState_cardMapContentsOperand ()
		    | CurrentThread => gcState_currentThreadContentsOperand ()
		    | ExnStack =>
			 gcState_currentThread_exnStackContentsOperand ()
		    | Frontier => gcState_frontierContentsOperand ()
		    | Limit => gcState_limitContentsOperand ()
		    | LimitPlusSlop => gcState_limitPlusSlopContentsOperand ()
		    | MaxFrameSize => gcState_maxFrameSizeContentsOperand ()
		    | SignalIsPending =>
			 gcState_signalIsPendingContentsOperand ()
		    | StackBottom => gcState_stackBottomContentsOperand ()
		    | StackLimit => gcState_stackLimitContentsOperand ()
		    | StackTop => gcState_stackTopContentsOperand ()
		end
	  | SmallIntInf ii => x86.Operand.immediate_const_word ii
	  | StackOffset {offset, ty} =>
	       let
		  val ty = Type.toRuntime ty
		  val memloc =
		     x86.MemLoc.simple 
		     {base = x86MLton.gcState_stackTopContents (), 
		      index = x86.Immediate.const_int offset,
		      scale = x86.Scale.One,
		      size = x86MLton.toX86Size ty,
		      class = x86MLton.Classes.Stack}
	       in
		  x86.Operand.memloc memloc
	       end
	  | Word w => x86.Operand.immediate_const_word w
	       
      val toX86Operand =
	 fn operand =>
	 toX86Operand operand
	 handle exn => Error.reraise (exn, "x86Translate.Operand.toX86Operand")

      fun convert x = (toX86Operand x, toX86Size x)
    end

  type transInfo = x86MLton.transInfo

  structure Entry =
    struct
      structure Kind = Machine.Kind
	 
      fun toX86Blocks {label, kind, 
		       transInfo as {frameInfoToX86, live, liveInfo,
				     ...}: transInfo}
	= (
	   x86Liveness.LiveInfo.setLiveOperands
	   (liveInfo, label, live label);
	   case kind
	     of Kind.Jump
	      => let
		 in
		   AppendList.single
		   (x86.Block.mkBlock'
		    {entry = SOME (x86.Entry.jump {label = label}),
		     statements = [],
		     transfer = NONE})
		 end
	      | Kind.Func
	      => let
		   val args
		     = List.fold
		       (live label,
			x86.MemLocSet.empty,
			fn (operand, args)
			 => case x86.Operand.deMemloc operand
			      of SOME memloc => x86.MemLocSet.add(args, memloc)
			       | NONE => args)
		 in
		   AppendList.single
		   (x86.Block.mkBlock'
		    {entry = SOME (x86.Entry.func {label = label,
						   live = args}),
		     statements = [],
		     transfer = NONE})
		 end
	      | Kind.Cont {args, frameInfo, ...}
	      => let
		    val frameInfo = frameInfoToX86 frameInfo
		   val args
		     = Vector.fold
		       (args,
			x86.MemLocSet.empty,
			fn (operand, args)
			 => case x86.Operand.deMemloc
			         (Operand.toX86Operand operand)
			      of SOME memloc => x86.MemLocSet.add(args, memloc)
			       | NONE => args)
		 in
		   AppendList.single
		   (x86.Block.mkBlock'
		    {entry = SOME (x86.Entry.cont {label = label,
						   live = args,
						   frameInfo = frameInfo}),
		     statements = [],
		     transfer = NONE})
		 end
	      | Kind.Handler {frameInfo, ...}
	      => let
		 in 
		   AppendList.single
		   (x86.Block.mkBlock'
		    {entry = SOME (x86.Entry.handler
				   {frameInfo = frameInfoToX86 frameInfo,
				    label = label,
				    live = x86.MemLocSet.empty}),
		     statements = [],
		     transfer = NONE})
		 end
	      | Kind.CReturn {dst, frameInfo, func}
	      => let
		   val dst = Option.map (dst, Operand.convert)
		 in
		   x86MLton.creturn
		   {dst = dst,
		    frameInfo = Option.map (frameInfo, frameInfoToX86),
		    func = func,
		    label = label,
		    transInfo = transInfo}
		 end)
    end

  structure Statement =
    struct
      open Machine.Statement

      fun comments statement
	= if !Control.Native.commented > 0
	    then let
		   val comment = (Layout.toString o layout) statement
		 in
		   (AppendList.single
		    (x86.Block.mkBlock'
		     {entry = NONE,
		      statements = [x86.Assembly.comment
				    (concat ["begin: ",
					     comment])],
		      transfer = NONE}),
		    AppendList.single
		    (x86.Block.mkBlock'
		     {entry = NONE,
		      statements = [x86.Assembly.comment
				    (concat ["end: ",
					     comment])],
		      transfer = NONE}))
		 end
	    else (AppendList.empty,AppendList.empty)

      fun toX86Blocks {statement,
		       transInfo as {liveInfo, ...} : transInfo}
	= (case statement
	     of Noop
	      => AppendList.empty
	      | Move {src, dst}
	      => let
		   val (comment_begin,
			comment_end) = comments statement
		     
		   val dstsize = Operand.toX86Size dst
		   val dst = Operand.toX86Operand dst
		     
		   val srcsize = Operand.toX86Size src
		   val src = Operand.toX86Operand src 
		     
		   val _ 
		     = Assert.assert
		       ("toX86Blocks: Move",
			fn () => srcsize = dstsize)
		 in
		   AppendList.appends
		   [comment_begin,
		    AppendList.single
		    (x86.Block.mkBlock'
		     {entry = NONE,
		      statements
		      = [(* dst = src *)
			 case x86.Size.class srcsize
			   of x86.Size.INT => x86.Assembly.instruction_mov 
			                      {dst = dst,
					       src = src,
					       size = srcsize}
			    | x86.Size.FLT => x86.Assembly.instruction_pfmov
					      {dst = dst,
					       src = src,
					       size = srcsize}
			    | _ => Error.bug "toX86Blocks: Move"],
		      transfer = NONE}),
		    comment_end]
		 end 
	      | PrimApp {dst, prim, args}
   	      => let
		   val (comment_begin, comment_end) = comments statement
		   val args = Vector.map (args, Operand.convert)
		   val dst = Option.map (dst, Operand.convert)
		 in
		   AppendList.appends
		   [comment_begin,
		    (x86MLton.prim {prim = prim,
				    args = args,
				    dst = dst,
				    transInfo = transInfo}),
		    comment_end]
		 end
	      | ProfileLabel l =>
		   AppendList.single
		   (x86.Block.mkProfileBlock'
		    {profileLabel = l})
 	      | SetSlotExnStack {offset}
	      => let
		   val (comment_begin, comment_end) = comments statement
		   val exnStack
		     = x86MLton.gcState_currentThread_exnStackContentsOperand ()
		   val stackTop = x86MLton.gcState_stackTopContentsOperand ()
		   val stackBottom =
		      x86MLton.gcState_stackBottomContentsOperand ()
		   val tempP 
		     = let
			 val index = x86.Immediate.const_int offset
			 val memloc
			   = x86.MemLoc.simple 
			     {base = x86MLton.gcState_stackTopContents (), 
			      index = index,
			      scale = x86.Scale.One,
			      size = x86MLton.pointerSize,
			      class = x86MLton.Classes.Stack}
		       in
			 x86.Operand.memloc memloc
		       end
		 in
		   AppendList.appends
		   [comment_begin,
		    AppendList.single
		    (x86.Block.mkBlock'
		     {entry = NONE,
		      statements =
		      [(* *(stackTop + offset) = exnStack *)
		       x86.Assembly.instruction_mov 
		       {dst = tempP,
			src = exnStack,
			size = x86MLton.pointerSize}],
		      transfer = NONE}),
		    comment_end]
		 end
	      | SetExnStackLocal {offset}
	      => let
		   val (comment_begin,
			comment_end) = comments statement
		   val exnStack
		     = x86MLton.gcState_currentThread_exnStackContentsOperand ()
		   val stackTop = x86MLton.gcState_stackTopContentsOperand ()
		   val stackBottom =
		      x86MLton.gcState_stackBottomContentsOperand ()
		 in
		   AppendList.appends
		   [comment_begin,
		    AppendList.single
		    (x86.Block.mkBlock'
		     {entry = NONE,
		      statements
		      = [(* exnStack = (stackTop + offset) - stackBottom *)
			 x86.Assembly.instruction_mov 
			 {dst = exnStack,
			  src = stackTop,
			  size = x86MLton.pointerSize},
			 x86.Assembly.instruction_binal 
			 {oper = x86.Instruction.ADD,
			  dst = exnStack,
			  src = x86.Operand.immediate_const_int offset,
			  size = x86MLton.pointerSize},
			 x86.Assembly.instruction_binal 
			 {oper = x86.Instruction.SUB,
			  dst = exnStack,
			  src = stackBottom,
			  size = x86MLton.pointerSize}],
		      transfer = NONE}),
		    comment_end]
		 end
	      | SetExnStackSlot {offset}
	      => let
		   val (comment_begin,
			comment_end) = comments statement
		     
		   val exnStack =
		      x86.Operand.memloc 
		      (x86MLton.gcState_currentThread_exnStackContents ())
		     
		   val tempP 
		     = let
			 val index = x86.Immediate.const_int offset
			 val memloc 
			   = x86.MemLoc.simple 
			     {base = x86MLton.gcState_stackTopContents (), 
			      index = index,
			      scale = x86.Scale.One,
			      size = x86MLton.pointerSize,
			      class = x86MLton.Classes.Stack}
		       in
			 x86.Operand.memloc memloc
		       end
		 in
		   AppendList.appends
		   [comment_begin,
		    AppendList.single
		    (x86.Block.mkBlock'
		     {entry = NONE,
		      statements 
		      = [(* exnStack = *(stackTop + offset) *)
			 x86.Assembly.instruction_mov 
			 {dst = exnStack,
			  src = tempP,
			  size = x86MLton.pointerSize}],
		      transfer = NONE}),
		    comment_end]
		 end
	      | Object {dst, header, size, stores}
	      => let
		   val (comment_begin,
			comment_end) = comments statement
		     
		   val dstsize = Operand.toX86Size dst
		   val dst = Operand.toX86Operand dst
		   val dst' = case x86.Operand.deMemloc dst
				of SOME dst' => dst'
				 | NONE => Error.bug "Allocate: strange dst"
		   val _ 
		     = Assert.assert
		       ("toX86Assembly: Allocate, dstsize",
			fn () => dstsize = x86MLton.pointerSize)
		       
		   val frontier = x86MLton.gcState_frontierContentsOperand ()
		   val frontierDeref = x86MLton.gcState_frontierDerefOperand ()
		   val frontierPlusOHW
		     = (x86.Operand.memloc o x86.MemLoc.simple)
		       {base = x86MLton.gcState_frontierContents (), 
			index = x86.Immediate.const_int normalHeaderBytes,
			scale = x86.Scale.One,
			size = x86MLton.pointerSize,
			class = x86MLton.Classes.Heap}
		       
		   fun stores_toX86Assembly ({offset, value}, l)
		     = let
			 val size =
			    x86MLton.toX86Size
			    (Type.toRuntime (Operand.ty value))
			 val value = Operand.toX86Operand value
			 val dst
			   = let
			       val index = x86.Immediate.const_int offset
			       val memloc
				 = x86.MemLoc.simple
				   {base = dst',
				    index = index,
				    scale = x86.Scale.One,
				    size = size,
				    class = x86MLton.Classes.Heap}
			     in
			       x86.Operand.memloc memloc
			     end
		       in
			 (case x86.Size.class size
			    of x86.Size.INT 
			     => x86.Assembly.instruction_mov 
			        {dst = dst,
				 src = value,
				 size = size}
			     | x86.Size.FLT 
			     => x86.Assembly.instruction_pfmov
				{dst = dst,
				 src = value,
				size = size}
			     | _ => Error.bug "toX86Blocks: Allocate")::l
		       end
		 in
		   AppendList.appends
		   [comment_begin,
		    AppendList.single
		    (x86.Block.mkBlock'
		     {entry = NONE,
		      statements
		      = ((* *(frontier) = header *)
			 x86.Assembly.instruction_mov 
			 {dst = frontierDeref,
			  src = x86.Operand.immediate_const_word header,
			  size = x86MLton.pointerSize})::
		        ((* dst = frontier + objectHeaderSize *)
			 x86.Assembly.instruction_lea
			 {dst = dst,
			  src = frontierPlusOHW,
			  size = x86MLton.pointerSize})::
			(Vector.foldr(stores,
				      [(* frontier += size *)
				       x86.Assembly.instruction_binal
				       {oper = x86.Instruction.ADD,
					dst = frontier,
					src = x86.Operand.immediate_const_int size,
					size = x86MLton.pointerSize}],
				      stores_toX86Assembly)),
		      transfer = NONE}),
		    comment_end]
		 end)
	  handle exn
	   => Error.reraise (exn, concat ["x86Translate.Statement.toX86Blocks::",
					  Layout.toString (layout statement)])
    end

  structure Transfer =
    struct
      open Machine.Transfer

      fun goto l
	= AppendList.single
	  (x86.Block.mkBlock'
	   {entry = NONE,
	    statements = [],
	    transfer = SOME (x86.Transfer.goto
			     {target = l})})
 
      fun iff (test, a, b)
	= let
	    val size = Operand.toX86Size test
	    val test = Operand.toX86Operand test
	  in
	    if Label.equals(a, b)
	      then AppendList.single
		   (x86.Block.mkBlock'
		    {entry = NONE,
		     statements = [],
		     transfer = SOME (x86.Transfer.goto {target = a})})
	      else AppendList.single
		   ((* if (test) goto a
		     * goto b
		     *)
		    x86.Block.mkBlock'
		    {entry = NONE,
		     statements 
		     = [x86.Assembly.instruction_test
			{src1 = test,
			 src2 = test,
			 size = size}],
		     transfer
		     = SOME (x86.Transfer.iff
			     {condition = x86.Instruction.NZ,
			      truee = a,
			      falsee = b})})
	  end

      fun cmp (test, k, a, b)
	= let
	    val size = Operand.toX86Size test
	    val test = Operand.toX86Operand test
	  in
	    if Label.equals(a, b)
	      then AppendList.single
		   (x86.Block.mkBlock'
		    {entry = NONE,
		     statements = [],
		     transfer = SOME (x86.Transfer.goto {target = a})})
	      else AppendList.single
		   ((* if (test = k) goto a
		     * goto b
		     *)
		    x86.Block.mkBlock'
		    {entry = NONE,
		     statements 
		     = [x86.Assembly.instruction_cmp
			{src1 = test,
			 src2 = x86.Operand.immediate k,
			 size = size}],
		     transfer
		     = SOME (x86.Transfer.iff
			     {condition = x86.Instruction.E,
			      truee = a,
			      falsee = b})})
	  end

      fun switch(test, cases, default)
	= let
	    val test = Operand.toX86Operand test
	  in
	    AppendList.single
	    (x86.Block.mkBlock'
	     {entry = NONE,
	      statements = [],
	      transfer = SOME (x86.Transfer.switch
			       {test = test,
				cases = cases,
				default = default})})
	  end

       fun doSwitchChar (test, cases, default)
	= (case (cases, default)
	     of ([],            NONE)
	      => Error.bug "toX86Blocks: doSwitchChar"
	      | ([(_,l)],       NONE) => goto l
	      | ([],            SOME l) => goto l
	      | ([(#"\000",f),(#"\001",t)], NONE) => iff(test,t,f)
	      | ([(#"\001",t),(#"\000",f)], NONE) => iff(test,t,f)
	      | ([(_,l),(k',l')],NONE) 
	      => cmp(test,x86.Immediate.const_char k',l',l)
	      | ([(k',l')],      SOME l)
	      => cmp(test,x86.Immediate.const_char k',l',l)
	      | ((_,l)::cases,  NONE) 
	      => switch(test, x86.Transfer.Cases.char cases, l)
	      | (cases,         SOME l) 
	      => switch(test, x86.Transfer.Cases.char cases, l))

      fun doSwitchInt (test, cases, default)
	= (case (cases, default)
	     of ([],             NONE)
	      => Error.bug "toX86Blocks: doSwitchInt"
	      | ([(_,l)],        NONE) => goto l
	      | ([],             SOME l) => goto l
	      | ([(0,f),(1,t)],  NONE) => iff(test,t,f)
	      | ([(1,t),(0,f)],  NONE) => iff(test,t,f)
	      | ([(_,l),(k',l')],NONE) 
	      => cmp(test,x86.Immediate.const_int k',l',l)
	      | ([(k',l')],      SOME l)
	      => cmp(test,x86.Immediate.const_int k',l',l)
	      | ((_,l)::cases,   NONE) 
	      => switch(test, x86.Transfer.Cases.int cases, l)
	      | (cases,          SOME l) 
	      => switch(test, x86.Transfer.Cases.int cases, l))

      fun doSwitchWord (test, cases, default)
	= (case (cases, default)
	     of ([],            NONE)
	      => Error.bug "toX86Blocks: doSwitchWord"
	      | ([(_,l)],       NONE) => goto l
	      | ([],            SOME l) => goto l
	      | ([(0wx0,f),(0wx1,t)], NONE) => iff(test,t,f)
	      | ([(0wx1,t),(0wx0,f)], NONE) => iff(test,t,f)
	      | ([(_,l),(k',l')],NONE) 
	      => cmp(test,x86.Immediate.const_word k',l',l)
	      | ([(k',l')],      SOME l)
	      => cmp(test,x86.Immediate.const_word k',l',l)
	      | ((_,l)::cases,  NONE) 
	      => switch(test, x86.Transfer.Cases.word cases, l)
	      | (cases,         SOME l) 
	      => switch(test, x86.Transfer.Cases.word cases, l))

      fun comments transfer
	= if !Control.Native.commented > 0
	    then let
		   val comment = (Layout.toString o layout) transfer
		 in
		   AppendList.single
		   (x86.Block.mkBlock'
		    {entry = NONE,
		     statements = [x86.Assembly.comment comment],
		      transfer = NONE})
		 end
	    else AppendList.empty

	 
      fun toX86Blocks {returns, transfer,
		       transInfo as {frameInfoToX86, ...}: transInfo}
	= (case transfer
	     of Arith {prim, args, dst, overflow, success, ty}
	      => let
		   val args = Vector.map (args, Operand.convert)
		   val dst = Operand.convert dst
		 in
		   AppendList.append
		   (comments transfer,
		    x86MLton.arith {prim = prim,
				    args = args,
				    dst = dst,
				    overflow = overflow,
				    success = success,
				    transInfo = transInfo})
		 end
	      | CCall {args, frameInfo, func, return}
	      => let
		   val args = Vector.map (args, Operand.convert)
		 in
		   AppendList.append
		   (comments transfer,	
		    x86MLton.ccall {args = args,
				    frameInfo = (Option.map
						 (frameInfo, frameInfoToX86)),
				    func = func,
				    return = return,
				    transInfo = transInfo})
		 end
	      | Return
	      => AppendList.append
	         (comments transfer,
		  AppendList.single
		  (x86.Block.mkBlock'
		   {entry = NONE,
		    statements = [],
		    transfer 
		    = SOME (x86.Transfer.return 
			    {live 
			     = Vector.fold
			       ((case returns of
				    NONE => Error.bug "strange Return"
				  | SOME zs => zs),
				x86.MemLocSet.empty,
				fn (operand, live)
				 => case x86.Operand.deMemloc
				         (Operand.toX86Operand operand)
				      of SOME memloc => x86.MemLocSet.add(live, memloc)
				       | NONE => live)})}))
  	      | Raise
	      => AppendList.append
	         (comments transfer,
		  AppendList.single
		  (x86.Block.mkBlock'
		   {entry = NONE,
		    statements = [],
		    transfer 
		    = SOME (x86.Transfer.raisee 
			    {live 
			     = x86.MemLocSet.add
			       (x86.MemLocSet.add
				(x86.MemLocSet.empty,
				 x86MLton.gcState_stackBottomContents ()),
				x86MLton.gcState_currentThread_exnStackContents ())})}))
	      | Switch switch
              => let
		    datatype z = datatype Machine.Switch.t
		    fun simple ({cases, default, test}, doSwitch) =
		       AppendList.append
		       (comments transfer,
			doSwitch (test, Vector.toList cases, default))
			
		 in
		    case switch of
		       Char z => simple (z, doSwitchChar)
		     | EnumPointers {enum, pointers, test} =>
			  let
			     val size = Operand.toX86Size test
			     val test = Operand.toX86Operand test
			  in
			     AppendList.append
			     (comments transfer,
			      AppendList.single
			      ((* if (test & 0x3) goto int 
				* goto pointer
				*)
			       x86.Block.mkBlock'
			       {entry = NONE,
				statements 
				= [x86.Assembly.instruction_test
				   {src1 = test,
				    src2 = x86.Operand.immediate_const_word 0wx3,
				    size = size}],
				transfer 
				= SOME (x86.Transfer.iff
					{condition = x86.Instruction.NZ,
					 truee = enum,
					 falsee = pointers})}))
			  end
		     | Int z => simple (z, doSwitchInt)
		     | Pointer {cases, default, tag, ...} =>
			  simple ({cases = (Vector.map
					    (cases, fn {dst, tag, ...} =>
					     (tag, dst))),
				   default = default,
				   test = tag},
				  doSwitchInt)
		     | Word z => simple (z, doSwitchWord)
		 end
	      | Goto label
	      => (AppendList.append
		  (comments transfer,
		   AppendList.single
		   ((* goto label *)
		    x86.Block.mkBlock'
		    {entry = NONE,
		     statements = [],
		     transfer = SOME (x86.Transfer.goto {target = label})})))
	      | Call {label, live, return, ...}
	      =>
		 let
		    val live =
		       Vector.fold
		       (live, x86.MemLocSet.empty, fn (operand, live) =>
			case (x86.Operand.deMemloc
			      (Operand.toX86Operand operand)) of
			   NONE => live
			 | SOME memloc => x86.MemLocSet.add (live, memloc))
		    val com = comments transfer
		    val transfer =
		       case return of
			  NONE => x86.Transfer.tail {target = label,
						     live = live}
			| SOME {return, handler, size} =>
			     x86.Transfer.nontail {target = label,
						   live = live,
						   return = return,
						   handler = handler,
						   size = size}
		 in
		    AppendList.append
		    (com,
		     AppendList.single
		     (x86.Block.mkBlock' {entry = NONE,
				    statements = [],
				    transfer = SOME transfer}))
		 end)
	  handle exn
	   => Error.reraise (exn, "x86Translate.Transfer.toX86Blocks")
    end

  structure Block =
    struct
      open Machine.Block

      fun toX86Blocks {block as T {label, 
				   live, 
				   kind, 
				   raises,
				   returns,
				   statements, 
				   transfer,
				   ...},
		       transInfo as {...} : transInfo}
	= let
	    val pseudo_blocks
	      = AppendList.append
	        (AppendList.snoc
		 (Entry.toX86Blocks {label = label,
				     kind = kind,
				     transInfo = transInfo},
		  x86.Block.mkBlock'
		  {entry = NONE,
		   statements 
		   = if !Control.Native.commented > 0
		       then let
			      val comment
				= "Live: " ^
				  (argsToString
				   (Vector.toListMap
				    (live, fn l => Operand.toString l)))
			    in
			      [x86.Assembly.comment comment]
			    end
		       else [],
		    transfer = NONE}),
		 Vector.foldr(statements,
			      (Transfer.toX86Blocks {returns = returns,
						     transfer = transfer,
						     transInfo = transInfo}),
			      fn (statement,l)
			       => AppendList.append
			          (Statement.toX86Blocks 
				   {statement = statement,
				    transInfo = transInfo}, l)))

	    val pseudo_blocks = AppendList.toList pseudo_blocks

	    val blocks = x86.Block.compress pseudo_blocks
	  in
	    blocks
	  end
	  handle exn
	   => Error.reraise (exn, "x86Translate.Block.toX86Blocks")
    end

  structure Chunk =
    struct
      open Machine.Chunk

      fun toX86Chunk {chunk as T {blocks, ...}, 
		      frameInfoToX86,
		      liveInfo}
	= let
	    val data = ref []
	    val addData = fn l => List.push (data, l)
	    val _ = addData [x86.Assembly.pseudoop_data ()]
	    val {get = live : Label.t -> x86.Operand.t list,
		 set = setLive, 
		 rem = remLive, ...}
	      = Property.getSetOnce
	        (Label.plist, Property.initRaise ("live", Label.layout))
	    val _ = Vector.foreach
	            (blocks, fn Block.T {label, live, ...} =>
		     setLive (label,
			      Vector.toListMap (live, Operand.toX86Operand)))
	    val transInfo = {addData = addData,
			     frameInfoToX86 = frameInfoToX86,
			     live = live,
			     liveInfo = liveInfo}
	    val x86Blocks 
	      = List.concat (Vector.toListMap
			     (blocks, 
				fn block
				 => Block.toX86Blocks 
				    {block = block,
				     transInfo = transInfo}))
	    val _ = Vector.foreach (blocks, fn Block.T {label, ...} =>
				    remLive label)
	    val _ = addData [x86.Assembly.pseudoop_text ()]
	    val data = List.concatRev (!data)
	  in
	    x86.Chunk.T {data = data, blocks = x86Blocks}
	  end
	  handle exn
	   => Error.reraise (exn, "x86Translate.Chunk.toX86Chunk")
    end

  fun translateChunk {chunk: x86MLton.Machine.Chunk.t,
		      frameInfoToX86,
		      liveInfo: x86Liveness.LiveInfo.t}:
                     {chunk: x86.Chunk.t}
    = {chunk = Chunk.toX86Chunk {chunk = chunk,
				 frameInfoToX86 = frameInfoToX86,
				 liveInfo = liveInfo}}

  val (translateChunk, translateChunk_msg)
    = tracerTop
      "translateChunk"
      translateChunk

  fun translateChunk_totals ()
    = (translateChunk_msg ();
       Control.indent ();
       Control.unindent ())

end
