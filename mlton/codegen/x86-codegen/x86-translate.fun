(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor x86Translate(S: X86_TRANSLATE_STRUCTS): X86_TRANSLATE =
struct

  open S

  val tracer = x86.tracer
  val tracerTop = x86.tracerTop

  val wordBytes = x86MLton.wordBytes
  val pointerBytes = x86MLton.pointerBytes
  val objectHeaderBytes = x86MLton.objectHeaderBytes
  val arrayHeaderBytes = x86MLton.arrayHeaderBytes
  val intInfOverheadBytes = x86MLton.intInfOverheadBytes

  fun argsToString(ss: string list): string
    = "(" ^ (concat (List.separate(ss, ", "))) ^ ")"

  structure Machine = x86MLton.Machine

  structure Label = Machine.Label
  structure Prim = Machine.Prim
    
  structure Type =
    struct
      open Machine.Type
      fun name t = case dest t 
		     of Char => "C"
		      | Double => "D"
		      | Int => "I"
		      | Pointer => "P"
		      | Uint => "U"
    end
    
  structure Local =
    struct
      open Machine.Register

      fun toX86MemLoc (T{index, ty})
	= let
	    val base
	      = x86.Immediate.label (x86MLton.local_base ty)
	  in
	    x86.MemLoc.imm {base = base,
			    index = x86.Immediate.const_int index,
			    scale = x86MLton.toX86Scale ty,
			    size = x86MLton.toX86Size ty,
			    class = x86MLton.Classes.Locals}
	  end

      fun eq(T{index = index1, ty = ty1},T{index = index2, ty = ty2})
	= Type.equals(ty1, ty2) 
	  andalso index1 = index2

      val toString = Layout.toString o layout
    end
  
  structure Global =
    struct
      open Machine.Global

      fun toX86MemLoc (T{index, ty})
	= let
	    val base
	      = x86.Immediate.label (x86MLton.global_base ty)
	  in
	    x86.MemLoc.imm {base = base,
			    index = x86.Immediate.const_int index,
			    scale = x86MLton.toX86Scale ty,
			    size = x86MLton.toX86Size ty,
			    class = x86MLton.Classes.Globals}
	  end

      val toString = Layout.toString o layout
    end

  structure Operand =
    struct
      open Machine.Operand

      val toX86Size = x86MLton.toX86Size o ty

      val rec toX86Operand
	= fn Char c 
	   => x86.Operand.immediate_const_char c
	   | Int i 
	   => x86.Operand.immediate_const_int i
	   | Uint w
	   => x86.Operand.immediate_const_word w
	   | IntInf ii
	   => x86.Operand.immediate_const_word ii
	   | Float f
	   => Error.bug "toX86Operand: Float, unimplemented"
	   | Pointer i
	   => x86.Operand.immediate_const_int i
	   | Label l
	   => x86.Operand.immediate_label l
	   | CastInt p
	   => toX86Operand p
	   | Register l
	   => x86.Operand.memloc (Local.toX86MemLoc l)
	   | Global g
	   => x86.Operand.memloc (Global.toX86MemLoc g)
	   | GlobalPointerNonRoot i
           => let
		val base
		  = x86.Immediate.label (x86MLton.globalPointerNonRoot_base)
		val memloc 
		  = x86.MemLoc.imm 
		    {base = base,
		     index = x86.Immediate.const_int i,
		     scale = x86MLton.pointerScale,
		     size = x86MLton.pointerSize,
		     class = x86MLton.Classes.Globals}
	      in
		x86.Operand.memloc memloc
	      end
	   | StackOffset {offset, ty}
	   => let
		val memloc 
		  = x86.MemLoc.simple 
		    {base = x86MLton.gcState_stackTopContents, 
		     index = x86.Immediate.const_int offset,
		     scale = x86.Scale.One,
		     size = x86MLton.toX86Size ty,
		     class = x86MLton.Classes.Stack}
	      in
		x86.Operand.memloc memloc
	      end
	   | Offset {base, offset, ty}
	   => let
		val base = toX86Operand base
		val memloc
		  =  case x86.Operand.deMemloc base
		       of SOME base
			=> x86.MemLoc.simple 
			   {base = base,
			    index = x86.Immediate.const_int offset,
			    scale = x86.Scale.One,
			    size = x86MLton.toX86Size ty,
			    class = x86MLton.Classes.Heap}
		        | _
			=> Error.bug ("toX86Operand: strange Offset:" ^
				      " base: " ^
				      (x86.Operand.toString base))
	      in
		x86.Operand.memloc memloc
	      end
	   | ArrayOffset {base, index, ty}
	   => let
		val base = toX86Operand base
		val index = toX86Operand index

		val memloc
		  = case (x86.Operand.deMemloc base,
			  x86.Operand.deImmediate index,
			  x86.Operand.deMemloc index)
		       of (SOME base, SOME index, _)
		        => x86.MemLoc.simple 
			   {base = base,
			    index = index,
			    scale = x86MLton.toX86Scale ty,
			    size = x86MLton.toX86Size ty,
			    class = x86MLton.Classes.Heap}
			| (SOME base, _, SOME index)
		        => x86.MemLoc.complex 
			   {base = base,
			    index = index,
			    scale = x86MLton.toX86Scale ty,
			    size = x86MLton.toX86Size ty,
			    class = x86MLton.Classes.Heap}
			| _
			=> Error.bug ("toX86Operand: strange Offset:" ^
				      " base: " ^
				      (x86.Operand.toString base) ^
				      " index: " ^
				      (x86.Operand.toString index))
	      in
		x86.Operand.memloc memloc
	      end
	   | Contents {oper, ty}
	   => let
		val base = toX86Operand oper
		val offset = x86.Immediate.const_int 0
		val size = x86MLton.toX86Size ty

		val memloc
		  = case x86.Operand.deMemloc base
		      of SOME base
		       => x86.MemLoc.simple 
			  {base = base,
			   index = x86.Immediate.const_int 0,
			   scale = x86.Scale.One,
			   size = x86MLton.toX86Size ty,
			   class = x86MLton.Classes.Heap}
		       | _
		       => Error.bug ("toX86Operand: strange Contents" ^
				     " base: " ^
				      (x86.Operand.toString base))
	      in
		x86.Operand.memloc memloc
	      end
      val toX86Operand 
	= fn operand => (toX86Operand operand)
	                handle exn
			 => Error.bug ("x86Translate.Operand.toX86Operand::" ^ 
				       (case exn
					  of Fail s => s
					   | _ => "?"))
    end

  type transInfo = x86MLton.transInfo

  structure Entry =
    struct
      structure Kind = Machine.Kind

      structure FrameInfo =
	struct
	  fun toX86FrameInfo {label,
			      frameInfo = Machine.FrameInfo.T {size = size', ...},
			      transInfo as {frameLayouts, ...} : transInfo}
	    = case frameLayouts label
		of NONE => Error.bug "toX86FrameInfo: label"
		 | SOME {size, frameLayoutsIndex}
		 => let
		      val _ = Assert.assert
			      ("toX86FrameInfo: size",
			       fn () => size = size')
		    in
		      x86.Entry.FrameInfo.frameInfo
		      {size = size,
		       frameLayoutsIndex = frameLayoutsIndex}
		    end
	end
	 
      fun toX86Blocks {label, kind, 
		       transInfo as {frameLayouts, live, liveInfo, ...} : transInfo}
	= (
	   x86Liveness.LiveInfo.setLiveOperands
	   (liveInfo, label, live label);
	   case kind
	     of Kind.Jump
	      => let
		 in
		   AppendList.single
		   (x86.Block.T'
		    {entry = SOME (x86.Entry.jump {label = label}),
		     profileInfo = x86.ProfileInfo.none,
		     statements = [],
		     transfer = NONE})
		 end
	      | Kind.Func {args}
	      => let
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
		   (x86.Block.T'
		    {entry = SOME (x86.Entry.func {label = label,
						   live = args}),
		     profileInfo = x86.ProfileInfo.none,
		     statements = [],
		     transfer = NONE})
		 end
	      | Kind.Cont {args, frameInfo}
	      => let
	           val frameInfo = FrameInfo.toX86FrameInfo {label = label,
							     frameInfo = frameInfo,
							     transInfo = transInfo}
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
		   (x86.Block.T'
		    {entry = SOME (x86.Entry.cont {label = label,
						   live = args,
						   frameInfo = frameInfo}),
		     profileInfo = x86.ProfileInfo.none,
		     statements = [],
		     transfer = NONE})
		 end
	      | Kind.Handler {offset}
	      => let
		 in 
		   AppendList.single
		   (x86.Block.T'
		    {entry = SOME (x86.Entry.handler {label = label,
						      live = x86.MemLocSet.empty,
						      offset = offset}),
		     profileInfo = x86.ProfileInfo.none,
		     statements = [],
		     transfer = NONE})
		 end
	      | Kind.CReturn {prim, dst}
	      => let
		   fun convert x
		     = (Operand.toX86Operand x,
			x86MLton.toX86Size (Operand.ty x))
		   val dst = Option.map (dst, convert)
		 in
		   x86MLton.creturn
		   {prim = prim,
		    label = label,
		    dst = dst,
		    transInfo = transInfo}
		 end
	      | Kind.Runtime {frameInfo, prim}
	      => let
	           val frameInfo = FrameInfo.toX86FrameInfo {label = label,
							     frameInfo = frameInfo,
							     transInfo = transInfo}
		 in
		   x86MLton.runtimereturn
		   {prim = prim,
		    label = label,
		    frameInfo = frameInfo,
		    transInfo = transInfo}
		 end)
    end

  structure LimitCheck =
    struct
      val limitCheckTemp = x86MLton.limitCheckTempContentsOperand

      val stackTop = x86MLton.gcState_stackTopContentsOperand
      val stackLimit = x86MLton.gcState_stackLimitContentsOperand

      val frontier = x86MLton.gcState_frontierContentsOperand
      val limit = x86MLton.gcState_limitContentsOperand

      datatype t = datatype Machine.LimitCheck.t

      fun limitCheck {kind: t,
		      failure: Label.t,
		      success: Label.t,
		      transInfo as {addData, frameLayouts, 
				    live, liveInfo, ...} : transInfo}
	= let
	    val live = live failure
	    val frameSize
	      = case frameLayouts failure
		  of NONE => Error.bug "limitCheck: frameSize"
		   | SOME {size, ...} => size

	    val checkForce = Label.newString "checkForce"
	    val checkStack = Label.newString "checkStack"
	    val checkFrontier = Label.newString "checkFrontier"
	    val doGC = Label.newString "doGC"

	    val noStackCheck
	      = ([], SOME (x86.Transfer.goto {target = checkFrontier}))
	    val doStackCheck
	      = ([x86.Assembly.instruction_cmp
		  {src1 = stackTop,
		   src2 = stackLimit,
		   size = x86MLton.pointerSize}],			   
		 SOME (x86.Transfer.iff 
		       {condition = x86.Instruction.A,
			truee = doGC,
			falsee = checkFrontier}))
	    val noFrontierCheck
	      = ([], SOME (x86.Transfer.goto {target = success}))
	    fun doFrontierCheck statements
	      = (statements,
		 SOME (x86.Transfer.iff 
		       {condition = x86.Instruction.BE,
			truee = success,
			falsee = doGC}))

	    val (computeBytes,stackCheck,frontierCheck,bytes)
	      = case kind
		  of Array {numElts, bytesPerElt as 0, 
			    extraBytes as 0, stackToo}
		   => let
			val bytes = 0
		      in
			([],
			 if stackToo then doStackCheck else noStackCheck,
			 doFrontierCheck
			 [x86.Assembly.instruction_cmp
			  {src1 = frontier,
			   src2 = limit,
			   size = x86MLton.pointerSize}],
			 x86.Operand.immediate_const_int bytes)
		      end
		   | Array {numElts, bytesPerElt as 0, 
			    extraBytes, stackToo}
		   => let
			val bytes = extraBytes
			val frontier_offset
			  = (x86.Operand.memloc o x86.MemLoc.simple) 
			    {base = x86MLton.gcState_frontierContents,
			     index = x86.Immediate.const_int bytes,
			     scale = x86.Scale.One,
			     size = x86MLton.pointerSize,
			     class = x86MLton.Classes.Heap}
		      in
			([], 
			 if stackToo then doStackCheck else noStackCheck,
			 doFrontierCheck
			 [x86.Assembly.instruction_lea
			  {dst = limitCheckTemp,
			   src = frontier_offset,
			   size = x86MLton.pointerSize},
			  x86.Assembly.instruction_cmp
			  {src1 = limitCheckTemp,
			   src2 = limit,
			   size = x86MLton.pointerSize}],
			 x86.Operand.immediate_const_int bytes)
		      end
		   | Array {numElts as Operand.Int 0, 
			    bytesPerElt, 
			    extraBytes as 0,
			    stackToo}
		   => let
			val bytes = 0
		      in
			([],
			 if stackToo then doStackCheck else noStackCheck,
			 doFrontierCheck
			 [x86.Assembly.instruction_cmp
			  {src1 = frontier,
			   src2 = limit,
			   size = x86MLton.pointerSize}],
			 x86.Operand.immediate_const_int bytes)
		      end
		   | Array {numElts as Operand.Int 0, 
			    bytesPerElt, 
			    extraBytes,
			    stackToo}
		   => let
			val bytes = extraBytes
			val frontier_offset
			  = (x86.Operand.memloc o x86.MemLoc.simple) 
			    {base = x86MLton.gcState_frontierContents,
			     index = x86.Immediate.const_int bytes,
			     scale = x86.Scale.One,
			     size = x86MLton.pointerSize,
			     class = x86MLton.Classes.Heap}
		      in
			([],
			 if stackToo then doStackCheck else noStackCheck,
			 doFrontierCheck
			 [x86.Assembly.instruction_lea
			  {dst = limitCheckTemp,
			   src = frontier_offset,
			   size = x86MLton.pointerSize},
			  x86.Assembly.instruction_cmp
			  {src1 = limitCheckTemp,
			   src2 = limit,
			   size = x86MLton.pointerSize}],
			 x86.Operand.immediate_const_int bytes)
		      end
		   | Array {numElts as Operand.Int numElts', 
			    bytesPerElt, 
			    extraBytes,
			    stackToo}
		   => let
			val bytes = numElts' * bytesPerElt + extraBytes
			val frontier_offset
			  = (x86.Operand.memloc o x86.MemLoc.simple) 
			    {base = x86MLton.gcState_frontierContents,
			     index = x86.Immediate.const_int bytes,
			     scale = x86.Scale.One,
			     size = x86MLton.pointerSize,
			     class = x86MLton.Classes.Heap}
		      in
			([],
			 if stackToo then doStackCheck else noStackCheck,
			 doFrontierCheck
			 [x86.Assembly.instruction_lea
			  {dst = limitCheckTemp,
			   src = frontier_offset,
			   size = x86MLton.pointerSize},
			  x86.Assembly.instruction_cmp
			  {src1 = limitCheckTemp,
			   src2 = limit,
			   size = x86MLton.pointerSize}],
			 x86.Operand.immediate_const_int bytes)
		      end
		   | Array {numElts, bytesPerElt, extraBytes, stackToo}
		   => let
			val numEltsSize = Operand.toX86Size numElts
			val numElts = Operand.toX86Operand numElts
			val _
			  = Assert.assert
			    ("LimitCheck: Array, numEltsSize",
			     fn () => numEltsSize = x86MLton.wordSize)
			val arrayAllocateTemp' 
			  = x86MLton.arrayAllocateTempContents
			val arrayAllocateTemp 
			  = x86MLton.arrayAllocateTempContentsOperand
			val frontier_offset
			  = (x86.Operand.memloc o x86.MemLoc.complex) 
			    {base = x86MLton.gcState_frontierContents,
			     index = arrayAllocateTemp',
			     scale = x86.Scale.One,
			     size = x86MLton.pointerSize,
			     class = x86MLton.Classes.Heap}
		      in
			(List.concat
			 [[(* arrayAllocateTemp 
			    *    = numElts * bytesPerElt + bytesAllocated 
			    *)
			   x86.Assembly.instruction_mov
			   {dst = arrayAllocateTemp,
			    src = numElts,
			    size = x86MLton.wordSize},
			   x86.Assembly.instruction_pmd
			   {oper = x86.Instruction.MUL,
			    dst = arrayAllocateTemp,
			    src = x86.Operand.immediate_const_int bytesPerElt,
			    size = x86MLton.wordSize}],
			  if extraBytes = 0
			    then []
			    else [x86.Assembly.instruction_binal
				  {oper = x86.Instruction.ADD,
				   dst = arrayAllocateTemp,
				   src = x86.Operand.immediate_const_int 
				         extraBytes,
				   size = x86MLton.wordSize}]],
			 if stackToo then doStackCheck else noStackCheck,
			 doFrontierCheck
			 [x86.Assembly.instruction_lea
			  {dst = limitCheckTemp,
			   src = frontier_offset,
			   size = x86MLton.pointerSize},
			  x86.Assembly.instruction_cmp
			  {src1 = limitCheckTemp,
			   src2 = limit,
			   size = x86MLton.pointerSize}],
			 arrayAllocateTemp)
		      end
		   | Heap {bytes as 0, stackToo}
		   => let
		      in
			([],
			 if stackToo then doStackCheck else noStackCheck,
			 doFrontierCheck
			 [x86.Assembly.instruction_cmp
			  {src1 = frontier,
			   src2 = limit,
			   size = x86MLton.pointerSize}],
			 x86.Operand.immediate_const_int bytes)
		      end
		   | Heap {bytes, stackToo}
		   => let
			val frontier_offset
			  = (x86.Operand.memloc o x86.MemLoc.simple) 
			    {base = x86MLton.gcState_frontierContents,
			     index = x86.Immediate.const_int bytes,
			     scale = x86.Scale.One,
			     size = x86MLton.pointerSize,
			     class = x86MLton.Classes.Heap}
		      in
			([],
			 if stackToo then doStackCheck else noStackCheck,
			 doFrontierCheck
			 [x86.Assembly.instruction_lea
			  {dst = limitCheckTemp,
			   src = frontier_offset,
			   size = x86MLton.pointerSize},
			  x86.Assembly.instruction_cmp
			  {src1 = limitCheckTemp,
			   src2 = limit,
			   size = x86MLton.pointerSize}],
			 x86.Operand.immediate_const_int bytes)
		      end
		   | Signal
		   => ([],
		       noStackCheck,
		       doFrontierCheck
		       [x86.Assembly.instruction_cmp
			{src1 = frontier,
			 src2 = limit,
			 size = x86MLton.pointerSize}],
		       x86.Operand.immediate_const_int 0)
		   | Stack
		   => ([],
		       doStackCheck,
		       noFrontierCheck,
		       x86.Operand.immediate_const_int 0)

	    val liveDoGC = bytes::live
	    val _ = x86Liveness.LiveInfo.setLiveOperands(liveInfo,
							 checkForce,
							 liveDoGC)
	    val _ = x86Liveness.LiveInfo.setLiveOperands(liveInfo,
							 checkStack, 
							 liveDoGC)
	    val _ = x86Liveness.LiveInfo.setLiveOperands(liveInfo,
							 checkFrontier, 
							 liveDoGC)
	    val _ = x86Liveness.LiveInfo.setLiveOperands(liveInfo, 
							 doGC, 
							 liveDoGC)

	    val gcFirstAux = x86MLton.gcFirstAuxTempContentsOperand
	    val gcFirst
	      = case !Control.gcCheck
		  of Control.Limit => NONE
		   | Control.First 
		   => let
			val gcFirst = Label.newString "gcFirst"
			val _ = addData [x86.Assembly.pseudoop_p2align 
					 (x86.Immediate.const_int 2, NONE, NONE),
					 x86.Assembly.label gcFirst,
					 x86.Assembly.pseudoop_long 
					 [x86.Immediate.const_int 1]]
		      in
			SOME ((x86.Operand.memloc o x86.MemLoc.imm) 
			      {base = x86.Immediate.label gcFirst,
			       index = x86.Immediate.const_int 0,
			       scale = x86MLton.wordScale,
			       size = x86MLton.wordSize,
			       class = x86MLton.Classes.StaticNonTemp})
		      end
		   | Control.Every => NONE
	  in
	    AppendList.fromList
	    [x86.Block.T'
	     {entry = NONE,
	      profileInfo = x86.ProfileInfo.none,
	      statements = 
	      if !Control.limitCheckCounts
		then [x86.Assembly.instruction_binal
		      {oper = x86.Instruction.ADD,
		       src = x86.Operand.immediate_const_int 1,
		       dst = x86MLton.gcState_numLCsLowContentsOperand,
		       size = x86.Size.LONG},
		      x86.Assembly.instruction_binal
		      {oper = x86.Instruction.ADC,
		       src = x86.Operand.immediate_const_int 0,
		       dst = x86MLton.gcState_numLCsHighContentsOperand,
		       size = x86.Size.LONG}]
		else [],
	      transfer = NONE},
	     x86.Block.T'
	     {entry = NONE,
	      profileInfo = x86.ProfileInfo.none,
	      statements = computeBytes,
	      transfer = SOME (x86.Transfer.goto {target = checkForce})},
	     let
	       val (statements, transfer)
		 = case !Control.gcCheck
		     of Control.Limit 
		      => ([], SOME (x86.Transfer.goto {target = checkStack}))
		      | Control.First
		      => let
			   val gcFirst = valOf gcFirst
			 in
			   ([x86.Assembly.instruction_mov
			     {src = gcFirst,
			      dst = gcFirstAux,
			      size = x86MLton.wordSize},
			     x86.Assembly.instruction_mov
			     {src = x86.Operand.immediate_const_int 0,
			      dst = gcFirst,
			      size = x86MLton.wordSize},
			     x86.Assembly.instruction_test
			     {src1 = gcFirstAux,
			      src2 = gcFirstAux,
			      size = x86MLton.wordSize}],
			    SOME (x86.Transfer.iff
				  {condition = x86.Instruction.NZ,
				   truee = doGC,
				   falsee = checkStack}))
			 end
		      | Control.Every
		      => ([], SOME (x86.Transfer.goto {target = doGC}))
	     in
	       x86.Block.T'
	       {entry = SOME (x86.Entry.jump {label = checkForce}),
		profileInfo = x86.ProfileInfo.none,
		statements = statements,
		transfer = transfer}
	     end,
             (* if (stackTop > stackLimit) goto doGC *)
	     let
	       val (statements, transfer) = stackCheck
	     in
	       x86.Block.T'
	       {entry = SOME (x86.Entry.jump {label = checkStack}),
		profileInfo = x86.ProfileInfo.none,
		statements = statements,
		transfer = transfer}
	     end,
	     (* if (frontier + bytes <= limit) goto success *)
	     let 
	       val (statements, transfer) = frontierCheck
	     in
	       x86.Block.T'
	       {entry = SOME (x86.Entry.jump {label = checkFrontier}),
		profileInfo = x86.ProfileInfo.none,
		statements = statements,
		transfer = transfer}
	     end,
	     (* doGC: *)
	     x86.Block.T'
	     {entry = SOME (x86.Entry.jump {label = doGC}),
	      profileInfo = x86.ProfileInfo.none,
	      statements = [],
	      transfer = SOME (x86.Transfer.runtime
			       {prim = Prim.gcCollect,
				args = [(x86.Operand.immediate_label x86MLton.gcState, 
					 x86MLton.pointerSize),
					(bytes, x86MLton.wordSize),
					(case !Control.gcCheck
					   of Control.Limit 
					    => x86.Operand.immediate_const_int 0
					    | Control.First => gcFirstAux
					    | Control.Every 
				            => x86.Operand.immediate_const_int 1,
					 x86MLton.wordSize),
				        (x86MLton.fileName, x86MLton.pointerSize),
				        (x86MLton.fileLine (), x86MLton.wordSize)],
				return = failure,
				size = frameSize})}]
	  end
	  handle exn
	   => Error.bug ("x86Translate.LimitCheck.limitCheck::" ^ 
			 (case exn
			    of Fail s => s
			     | _ => "?"))
    end

  structure Statement =
    struct
      open Machine.Statement

      val toX86Blocks_AllocateArray_init
	= fn {dst,numElts,numBytesNonPointers,numPointers}
	   => let
		val dstsize = Operand.toX86Size dst
		val dst = Operand.toX86Operand dst
		val _ 
		  = Assert.assert
		    ("toX86Blocks: AllocateArray, dstsize",
		     fn () => dstsize = x86MLton.pointerSize)
		  
		val numEltsSize = Operand.toX86Size numElts
		val numElts = Operand.toX86Operand numElts
		val _
		  = Assert.assert
		    ("toX86Blocks: AllocateArray, numEltsSize",
		     fn () => numEltsSize = x86MLton.wordSize)

		val frontier = x86MLton.gcState_frontierContentsOperand
		val frontierDeref = x86MLton.gcState_frontierDerefOperand
		val frontierOffset
		  = let
		      val memloc 
			= x86.MemLoc.simple 
			  {base = x86MLton.gcState_frontierContents, 
			   index = x86.Immediate.const_int 1,
			   scale = x86MLton.wordScale,
			   size = x86MLton.pointerSize,
			   class = x86MLton.Classes.Heap}
		    in
		      x86.Operand.memloc memloc
		    end
		val frontierPlusAHW
		  = (x86.Operand.memloc o x86.MemLoc.simple)
		    {base = x86MLton.gcState_frontierContents, 
		     index = x86.Immediate.const_int arrayHeaderBytes,
		     scale = x86.Scale.One,
		     size = x86MLton.pointerSize,
		     class = x86MLton.Classes.Heap}

		val gcArrayHeaderWord 
		  = (x86.Operand.immediate o x86MLton.gcArrayHeader)
		    {nonPointers = numBytesNonPointers,
		     pointers = numPointers}
	      in
		AppendList.single
		(x86.Block.T'
		 {entry = NONE,
		  profileInfo = x86.ProfileInfo.none,
		  statements
		  = [(* *(frontier) = numElts *)
		     x86.Assembly.instruction_mov
		     {dst = frontierDeref,
		      src = numElts,
		      size = x86MLton.wordSize},
		     (* *(frontier + wordSize) 
		      *    = gcArrayHeader(numBytesNonPointers, numPointers) 
		      *)
		     x86.Assembly.instruction_mov
		     {dst = frontierOffset,
		      src = gcArrayHeaderWord,
		      size = x86MLton.wordSize},
		     (* dst = frontier + arrayHeaderSize *)
		     x86.Assembly.instruction_lea
		     {dst = dst,
		      src = frontierPlusAHW,
		      size = x86MLton.pointerSize},
		     (* frontier = dst *)
		     x86.Assembly.instruction_mov
		     {dst = frontier,
		      src = dst,
		      size = x86MLton.pointerSize}],
		  transfer = NONE})
	     end

      val toX86Blocks_AllocateArray_arrayNoPointers
	= fn {dst, 
	      numElts as Operand.Int numElts', 
	      numBytesNonPointers,
	      live,
	      liveInfo}
	   => let
		val frontier = x86MLton.gcState_frontierContentsOperand
	      in
		if numBytesNonPointers = 0 orelse numElts' = 0
		  then AppendList.single
		       (x86.Block.T'
			{entry = NONE,
			 profileInfo = x86.ProfileInfo.none,
			 statements
			 = [(* frontier += pointerSize *)
			    x86.Assembly.instruction_binal
			    {oper = x86.Instruction.ADD,
			     dst = frontier,
			     src = x86.Operand.immediate_const_int pointerBytes,
			     size = x86MLton.pointerSize}],
			  transfer = NONE})
		  else AppendList.single
		       (x86.Block.T'
			{entry = NONE,
			 profileInfo = x86.ProfileInfo.none,
			 statements 
			 = [(* frontier 
			     *    += wordAlign(numElts * 
			     *                 numBytesNonPointers) 
			     *)
			    x86.Assembly.instruction_binal
			    {oper = x86.Instruction.ADD,
			     dst = frontier,
			     src = if numElts' < 0
				     then x86.Operand.immediate_const_int 0
				     else x86.Operand.immediate_const_int
				          (x86MLton.wordAlign(numElts' * 
							      numBytesNonPointers)),
			     size = x86MLton.wordSize}],
			 transfer = NONE})
	      end
	   | {dst, 
	      numElts, 
	      numBytesNonPointers,
	      live,
	      liveInfo}
	   => let
		val liveIn = (Operand.toX86Operand dst)::live
		  
		val numEltsSize = Operand.toX86Size numElts
		val numElts = Operand.toX86Operand numElts
		val _ 
		  = Assert.assert
		    ("toX86Blocks: AllocateArray, numEltsSize",
		     fn () => numEltsSize = x86MLton.wordSize)

		val arrayAllocateTemp = x86MLton.arrayAllocateTempContentsOperand
		val frontier = x86MLton.gcState_frontierContentsOperand
       	      in
		if numBytesNonPointers = 0
		  then AppendList.single
		       (x86.Block.T'
			{entry = NONE,
			 profileInfo = x86.ProfileInfo.none,
			 statements 
			 = [(* frontier += pointerSize *)
			    x86.Assembly.instruction_binal
			    {oper = x86.Instruction.ADD,
			     dst = frontier,
			     src = x86.Operand.immediate_const_int
			           pointerBytes,
			     size = x86MLton.pointerSize}],
			 transfer = NONE})
		  else let
			 val arrayNoPointersZ 
			   = Label.newString "arrayNoPointersZ"
			 val _ = x86Liveness.LiveInfo.setLiveOperands
			         (liveInfo,
				  arrayNoPointersZ, 
				  liveIn)
			 val arrayNoPointersNZ
			   = Label.newString "arrayNoPointersNZ"
			 val _ = x86Liveness.LiveInfo.setLiveOperands
			         (liveInfo,
				  arrayNoPointersNZ, 
				  numElts::liveIn)
			 val arrayNoPointersZJoin
			   = Label.newString "arrayNoPointersZJoin"
			 val _ = x86Liveness.LiveInfo.setLiveOperands
			         (liveInfo,
				  arrayNoPointersZJoin, 
				  liveIn) 
		       in
			 AppendList.fromList
			 [(* if (numElts == 0) goto arrayNoPointersZ *)
			  x86.Block.T'
			  {entry = NONE,
			   profileInfo = x86.ProfileInfo.none,
			   statements 
			   = [x86.Assembly.instruction_test
			      {src1 = numElts,
			       src2 = numElts,
			       size = numEltsSize}],
			   transfer
			   = SOME (x86.Transfer.iff
				   {condition = x86.Instruction.Z,
				    truee = arrayNoPointersZ,
				    falsee = arrayNoPointersNZ})},
			  (* arrayNoPointersNZ: *)
			  x86.Block.T'
			  {entry = SOME (x86.Entry.jump {label = arrayNoPointersNZ}),
			   profileInfo = x86.ProfileInfo.none,
			   statements 
			   = [(* frontier 
			       *    += wordAlign(numElts * 
			       *                 numBytesNonPointers) 
			       *) 
			      x86.Assembly.instruction_mov
			      {dst = arrayAllocateTemp,
			       src = numElts,
			       size = x86MLton.wordSize},
			      x86.Assembly.instruction_pmd
			      {oper = x86.Instruction.MUL,
			       dst = arrayAllocateTemp,
			       src = x86.Operand.immediate_const_int 
			             numBytesNonPointers,
			       size = x86MLton.wordSize},
			      x86.Assembly.instruction_binal
			      {oper = x86.Instruction.ADD,
			       dst = arrayAllocateTemp,
			       src = x86.Operand.immediate_const_int 3,
			       size = x86MLton.wordSize},
			      x86.Assembly.instruction_binal
			      {oper = x86.Instruction.AND,
			       dst = arrayAllocateTemp,
			       src = x86.Operand.immediate_const_int ~4,
			       size = x86MLton.wordSize},
			      x86.Assembly.instruction_binal
			      {oper = x86.Instruction.ADD,
			       dst = frontier,
			       src = arrayAllocateTemp,
			       size = x86MLton.pointerSize}],
			   (* goto arrayNoPointersZJoin *)
			   transfer 
			   = SOME (x86.Transfer.goto
				   {target = arrayNoPointersZJoin})},
			  (* arrayNoPointersZ: *)
			  x86.Block.T'
			  {entry = SOME (x86.Entry.jump {label = arrayNoPointersZ}),
			   profileInfo = x86.ProfileInfo.none,
			   statements
			   = [(* frontier += pointerSize *)
			      x86.Assembly.instruction_binal
			      {oper = x86.Instruction.ADD,
			       dst = frontier,
			       src = x86.Operand.immediate_const_int pointerBytes,
			       size = x86MLton.pointerSize}],
			   (* goto arrayNoPointersZJoin *)
			   transfer 
			   = SOME (x86.Transfer.goto
				   {target = arrayNoPointersZJoin})},
			  (* arrayNoPointersZJoin: *)
			  x86.Block.T'
			  {entry = SOME (x86.Entry.jump {label = arrayNoPointersZJoin}),
			   profileInfo = x86.ProfileInfo.none,
			   statements = [],
			   transfer = NONE}]
		       end
	      end

      val toX86Blocks_AllocateArray_arrayPointers
	= fn {dst, 
	      numElts as Operand.Int numElts', 
	      numPointers,
	      live,
	      liveInfo}
	   => let
		val frontier = x86MLton.gcState_frontierContentsOperand
	      in
		AppendList.single
		(x86.Block.T'
		 {entry = NONE,
		  profileInfo = x86.ProfileInfo.none,
		  statements 
		  = [if numElts' = 0
		       then (* frontier += pointerSize *)
			    x86.Assembly.instruction_binal
			    {oper = x86.Instruction.ADD,
			     dst = frontier,
			     src = x86.Operand.immediate_const_int pointerBytes,
			     size = x86MLton.pointerSize}
		       else (* frontier 
			     *    += numElts * numPointers * pointerSize 
			     *)
			    x86.Assembly.instruction_binal
			    {oper = x86.Instruction.ADD,
			     dst = frontier,
			     src = x86.Operand.immediate_const_int
			           (numElts' * numPointers * pointerBytes),
			     size = x86MLton.pointerSize}],
		  transfer = NONE})
	      end
	   | {dst, 
	      numElts,
	      numPointers,
	      live,
	      liveInfo} 
	   => let
		val liveIn = (Operand.toX86Operand dst)::live  
		val numEltsSize = Operand.toX86Size numElts
		val numElts = Operand.toX86Operand numElts
		val _ 
		  = Assert.assert
		    ("toX86Blocks: AllocateArray, numEltsSize",
		     fn () => numEltsSize = x86MLton.wordSize)

		val arrayAllocateTemp = x86MLton.arrayAllocateTempContentsOperand
		val frontier = x86MLton.gcState_frontierContentsOperand

		val arrayPointersZ
		  = Label.newString "arrayPointersZ"
		val _ = x86Liveness.LiveInfo.setLiveOperands
		        (liveInfo,
			 arrayPointersZ, 
			 liveIn)
		val arrayPointersNZ
		  = Label.newString "arrayPointersNZ"
		val _ = x86Liveness.LiveInfo.setLiveOperands
		        (liveInfo,
			 arrayPointersNZ, 
			 numElts::liveIn)
		val arrayPointersZJoin
		  = Label.newString "arrayPointersZJoin"
		val _ = x86Liveness.LiveInfo.setLiveOperands
		        (liveInfo,
			 arrayPointersZJoin, 
			 liveIn)
	      in
		AppendList.fromList
		[(* if (numElts == 0) goto arrayPointersZ *)
		 x86.Block.T'
		 {entry = NONE,
		  profileInfo = x86.ProfileInfo.none,
		  statements
		  = [x86.Assembly.instruction_test
		     {src1 = numElts,
		      src2 = numElts,
		      size = numEltsSize}],
		  transfer 
		  = SOME (x86.Transfer.iff
			  {condition = x86.Instruction.Z,
			   truee = arrayPointersZ,
			   falsee = arrayPointersNZ})},
		 (* arrayPointersNZ: *)
		 x86.Block.T'
		 {entry = SOME (x86.Entry.jump {label = arrayPointersNZ}),
		  profileInfo = x86.ProfileInfo.none,
		  statements
		  = [(* frontier 
		      *    += numElts * numPointers * pointerSize 
		      *)
		     x86.Assembly.instruction_mov
		     {dst = arrayAllocateTemp,
		      src = numElts,
		      size = x86MLton.wordSize},
		     x86.Assembly.instruction_pmd
		     {oper = x86.Instruction.MUL,
		      dst = arrayAllocateTemp,
		      src = x86.Operand.immediate_const_int 
		            (numPointers * pointerBytes),
		      size = x86MLton.wordSize},
		     x86.Assembly.instruction_binal
		     {oper = x86.Instruction.ADD,
		      dst = frontier,
		      src = arrayAllocateTemp,
		      size = x86MLton.pointerSize}],
		  (* goto arrayPointersZJoin *)
		  transfer 
		  = SOME (x86.Transfer.goto
			  {target = arrayPointersZJoin})},
		 (* arrayPointersZ: *)
		 x86.Block.T'
		 {entry = SOME (x86.Entry.jump {label = arrayPointersZ}),
		  profileInfo = x86.ProfileInfo.none,
		  statements 
		  = [(* frontier += pointerSize *)
		     x86.Assembly.instruction_binal
		     {oper = x86.Instruction.ADD,
		      dst = frontier,
		      src = x86.Operand.immediate_const_int pointerBytes,
		      size = x86MLton.pointerSize}],
		  (* goto arrayPointersZJoin *)
		  transfer 
		  = SOME (x86.Transfer.goto
			  {target = arrayPointersZJoin})},
		 (* arrayPointersZJoin: *)
		 x86.Block.T'
		 {entry = SOME (x86.Entry.jump {label = arrayPointersZJoin}),
		  profileInfo = x86.ProfileInfo.none,
		  statements = [],
		  transfer = NONE}]
	      end

      fun comments statement
	= if !Control.Native.commented > 0
	    then let
		   val comment = (Layout.toString o layout) statement
		 in
		   (AppendList.single
		    (x86.Block.T'
		     {entry = NONE,
		      profileInfo = x86.ProfileInfo.none,
		      statements = [x86.Assembly.comment
				    (concat ["begin: ",
					     comment])],
		      transfer = NONE}),
		    AppendList.single
		    (x86.Block.T'
		     {entry = NONE,
		      profileInfo = x86.ProfileInfo.none,
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
		    (x86.Block.T'
		     {entry = NONE,
		      profileInfo = x86.ProfileInfo.none,
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
		   val (comment_begin,
			comment_end) = comments statement
		   fun convert x
		     = (Operand.toX86Operand x,
			x86MLton.toX86Size (Operand.ty x))

		   val args = Vector.map(args, convert)
		     
		   val dst = Option.map(dst, convert)
		 in
		   AppendList.appends
		   [comment_begin,
		    (x86MLton.prim {prim = prim,
				    args = args,
				    dst = dst,
				    transInfo = transInfo}),
		    comment_end]
		 end
 	      | SetSlotExnStack {offset}
	      => let
		   val (comment_begin, comment_end) = comments statement
		   val exnStack
		     = x86MLton.gcState_currentThread_exnStackContentsOperand
		   val stackTop = x86MLton.gcState_stackTopContentsOperand
		   val stackBottom = x86MLton.gcState_stackBottomContentsOperand
		   val tempP 
		     = let
			 val index = x86.Immediate.const_int offset
			 val memloc
			   = x86.MemLoc.simple 
			     {base = x86MLton.gcState_stackTopContents, 
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
		    (x86.Block.T'
		     {entry = NONE,
		      profileInfo = x86.ProfileInfo.none,
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
		     = x86MLton.gcState_currentThread_exnStackContentsOperand
		   val stackTop = x86MLton.gcState_stackTopContentsOperand
		   val stackBottom = x86MLton.gcState_stackBottomContentsOperand
		 in
		   AppendList.appends
		   [comment_begin,
		    AppendList.single
		    (x86.Block.T'
		     {entry = NONE,
		      profileInfo = x86.ProfileInfo.none,
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
		     
		   val exnStack 
		     = x86.Operand.memloc 
		       x86MLton.gcState_currentThread_exnStackContents
		     
		   val tempP 
		     = let
			 val index = x86.Immediate.const_int offset
			 val memloc 
			   = x86.MemLoc.simple 
			     {base = x86MLton.gcState_stackTopContents, 
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
		    (x86.Block.T'
		     {entry = NONE,
		      profileInfo = x86.ProfileInfo.none,
		      statements 
		      = [(* exnStack = *(stackTop + offset) *)
			 x86.Assembly.instruction_mov 
			 {dst = exnStack,
			  src = tempP,
			  size = x86MLton.pointerSize}],
		      transfer = NONE}),
		    comment_end]
		 end
	      | Object {dst, stores, numPointers, numWordsNonPointers}
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
		       
		   val frontier = x86MLton.gcState_frontierContentsOperand
		   val frontierDeref = x86MLton.gcState_frontierDerefOperand
		   val frontierPlusOHW
		     = (x86.Operand.memloc o x86.MemLoc.simple)
		       {base = x86MLton.gcState_frontierContents, 
			index = x86.Immediate.const_int objectHeaderBytes,
			scale = x86.Scale.One,
			size = x86MLton.pointerSize,
			class = x86MLton.Classes.Heap}
		       
		   val gcObjectHeaderWord 
		     = (x86.Operand.immediate o x86MLton.gcObjectHeader)
		       {nonPointers = numWordsNonPointers,
			pointers = numPointers}
		       
		   fun stores_toX86Assembly ({offset, value}, l)
		     = let
			 val size = x86MLton.toX86Size (Operand.ty value)
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
		    (x86.Block.T'
		     {entry = NONE,
		      profileInfo = x86.ProfileInfo.none,
		      statements
		      = ((* *(frontier) 
			  *    = gcObjectHeader(numWordsNonPointers, 
			  *                     numPointers)
			  *)
			 x86.Assembly.instruction_mov 
			 {dst = frontierDeref,
			  src = gcObjectHeaderWord,
			  size = x86MLton.pointerSize})::
		        ((* dst = frontier + objectHeaderSize *)
			 x86.Assembly.instruction_lea
			 {dst = dst,
			  src = frontierPlusOHW,
			  size = x86MLton.pointerSize})::
			(Vector.foldr(stores,
				      [(* frontier += objectHeaderSize + size *)
				       x86.Assembly.instruction_binal
				       {oper = x86.Instruction.ADD,
					dst = frontier,
					src = x86.Operand.immediate_const_int 
					      (objectHeaderBytes
					       + (Runtime.objectSize
						  {numPointers = numPointers,
						   numWordsNonPointers =
						   numWordsNonPointers})),
					size = x86MLton.pointerSize}],
				      stores_toX86Assembly)),
(*
		      = List.concat
		        [[(* *(frontier) 
			   *    = gcObjectHeader(numWordsNonPointers, 
			   *                     numPointers)
			   *)
			  x86.Assembly.instruction_mov 
			  {dst = frontierDeref,
			   src = gcObjectHeaderWord,
			   size = x86MLton.pointerSize},
			  (* dst = frontier + objectHeaderSize *)
			  x86.Assembly.instruction_lea
			  {dst = dst,
			   src = frontierPlusOHW,
			   size = x86MLton.pointerSize}],
			 (Vector.foldr(stores,
				       [],
				       stores_toX86Assembly)),
			 [(* frontier += objectHeaderSize + size *)
			  x86.Assembly.instruction_binal
			  {oper = x86.Instruction.ADD,
			   dst = frontier,
			   src = x86.Operand.immediate_const_int 
			         (objectHeaderSize + size),
			   size = x86MLton.pointerSize}]],
*)
		      transfer = NONE}),
		    comment_end]
		 end
	      | Array {dst,
		       numElts,
		       numPointers,
		       numBytesNonPointers}
	      => let
		   val (comment_begin,
			comment_end) = comments statement
		     
		   val live = []
		 in
		   AppendList.appends
		   [comment_begin,
		    (toX86Blocks_AllocateArray_init
		     {dst = dst,
		      numElts = numElts,
		      numBytesNonPointers = numBytesNonPointers,
		      numPointers = numPointers}),
		    (if numPointers = 0
		       then toX86Blocks_AllocateArray_arrayNoPointers
			    {dst = dst,
			     numElts = numElts,
			     numBytesNonPointers = numBytesNonPointers,
			     live = live,
			     liveInfo = liveInfo}
		     else if numBytesNonPointers = 0
		       then toX86Blocks_AllocateArray_arrayPointers
		            {dst = dst,
			     numElts = numElts,
			     numPointers = numPointers,
			     live = live,
			     liveInfo = liveInfo}
		     else Error.bug "toX86Blocks: AllocateArray"),
		    comment_end]
		 end)
	  handle exn
	   => Error.bug ("x86Translate.Statement.toX86Blocks::" ^ 
			 (Layout.toString (layout statement)) ^ "::" ^
			 (case exn
			    of Fail s => s
			     | _ => "?"))

    end

  structure Transfer =
    struct
      open Machine.Transfer

      fun goto l
	= AppendList.single
	  (x86.Block.T'
	   {entry = NONE,
	    profileInfo = x86.ProfileInfo.none,
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
		   (x86.Block.T'
		    {entry = NONE,
		     profileInfo = x86.ProfileInfo.none,
		     statements = [],
		     transfer = SOME (x86.Transfer.goto {target = a})})
	      else AppendList.single
		   ((* if (test) goto a
		     * goto b
		     *)
		    x86.Block.T'
		    {entry = NONE,
		     profileInfo = x86.ProfileInfo.none,
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
		   (x86.Block.T'
		    {entry = NONE,
		     profileInfo = x86.ProfileInfo.none,
		     statements = [],
		     transfer = SOME (x86.Transfer.goto {target = a})})
	      else AppendList.single
		   ((* if (test = k) goto a
		     * goto b
		     *)
		    x86.Block.T'
		    {entry = NONE,
		     profileInfo = x86.ProfileInfo.none,
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
	    (x86.Block.T'
	     {entry = NONE,
	      profileInfo = x86.ProfileInfo.none,
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
		   (x86.Block.T'
		    {entry = NONE,
		     profileInfo = x86.ProfileInfo.none,
		     statements = [x86.Assembly.comment comment],
		      transfer = NONE})
		 end
	    else AppendList.empty

      fun toX86Blocks {transfer, transInfo as {...} : transInfo}
	= (case transfer
	     of Arith {prim, args, dst, overflow, success}
	      => let
		   fun convert x
		     = (Operand.toX86Operand x,
			x86MLton.toX86Size (Operand.ty x))
		   val args = Vector.map(args, convert)
		   val dst = convert dst
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
	      | Bug 
	      => AppendList.append
	         (comments transfer,
		  x86MLton.bug {transInfo = transInfo})
	      | CCall {args, prim, return, returnTy}
	      => let
		   fun convert x
		     = (Operand.toX86Operand x,
			x86MLton.toX86Size (Operand.ty x))
		   val args = Vector.map(args, convert)
		   val dstsize = Option.map (returnTy, x86MLton.toX86Size)
		 in
		   AppendList.append
		   (comments transfer,	
		    x86MLton.ccall
		    {prim = prim,
		     args = args,
		     return = return,
		     dstsize = dstsize,
		     transInfo = transInfo})
		 end
	      | LimitCheck {kind, failure, success}
	      => AppendList.append
	         (comments transfer,
		  LimitCheck.limitCheck
		  {kind = kind,
		   failure = failure,
		   success = success,
		   transInfo = transInfo})
	      | Runtime {args, prim, return}
	      => let
		   fun convert x
		     = (Operand.toX86Operand x,
			x86MLton.toX86Size (Operand.ty x))
		   val args = Vector.map(args, convert)
		 in
		   AppendList.append
		   (comments transfer,
		    x86MLton.runtimecall
		    {prim = prim,
		     args = args,
		     return = return,
		     transInfo = transInfo})
		 end
	      | Return {live}
	      => AppendList.append
	         (comments transfer,
		  AppendList.single
		  (x86.Block.T'
		   {entry = NONE,
		    profileInfo = x86.ProfileInfo.none,
		    statements = [],
		    transfer 
		    = SOME (x86.Transfer.return 
			    {live 
			     = Vector.fold
			       (live,
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
		  (x86.Block.T'
		   {entry = NONE,
		    profileInfo = x86.ProfileInfo.none,
		    statements = [],
		    transfer 
		    = SOME (x86.Transfer.raisee 
			    {live 
			     = x86.MemLocSet.add
			       (x86.MemLocSet.add
				(x86.MemLocSet.empty,
				 x86MLton.gcState_stackBottomContents),
				x86MLton.gcState_currentThread_exnStackContents)})}))
	      | Switch {test, cases, default}
	      => AppendList.append
	         (comments transfer,
		  (case cases 
		     of Machine.Cases.Char cases 
		      => doSwitchChar (test,cases,default)
		      | Machine.Cases.Int cases 
		      => doSwitchInt (test,cases,default)
	              | Machine.Cases.Word cases 
	              => doSwitchWord (test,cases,default)))
	      | SwitchIP {test, int, pointer}
	      => let
		   val size = Operand.toX86Size test
		   val test = Operand.toX86Operand test
		 in
		   AppendList.append
		   (comments transfer,
		    AppendList.single
		    ((* if (test & 0x3) goto int 
		      * goto pointer
		      *)
		     x86.Block.T'
		     {entry = NONE,
		      profileInfo = x86.ProfileInfo.none,
		      statements 
		      = [x86.Assembly.instruction_test
			 {src1 = test,
			  src2 = x86.Operand.immediate_const_word 0wx3,
			  size = size}],
		      transfer 
		      = SOME (x86.Transfer.iff
			      {condition = x86.Instruction.NZ,
			       truee = int,
			       falsee = pointer})}))
		 end
	      | Goto label
	      => (AppendList.append
		  (comments transfer,
		   AppendList.single
		   ((* goto label *)
		    x86.Block.T'
		    {entry = NONE,
		     profileInfo = x86.ProfileInfo.none,
		     statements = [],
		     transfer = SOME (x86.Transfer.goto {target = label})})))
	      | Call {label, live, return, ...}
	      => (case return
		    of SOME {return, handler, size}
		     => AppendList.append
		        (comments transfer,
			 AppendList.single
			 (x86.Block.T'
			  {entry = NONE,
			   profileInfo = x86.ProfileInfo.none,
			   statements = [],
			   transfer 
			   = SOME (x86.Transfer.nontail 
				   {target = label,
				    live 
				    = Vector.fold
				      (live,
				       x86.MemLocSet.empty,
				       fn (operand,live)
				        => case x86.Operand.deMemloc
				                (Operand.toX86Operand operand)
					     of SOME memloc 
					      => x86.MemLocSet.add(live, memloc)
					      | NONE => live),
				    return = return,
				    handler = handler,
				    size = size})}))
		     | NONE
	             => AppendList.append
			(comments transfer,
			 AppendList.single
			 ((* goto label *)
			  x86.Block.T'
			  {entry = NONE,
			   profileInfo = x86.ProfileInfo.none,
			   statements = [],
			   transfer 
			   = SOME (x86.Transfer.tail 
				   {target = label,
				    live 
				    = Vector.fold
				      (live,
				       x86.MemLocSet.empty,
				       fn (operand,live)
				        => case x86.Operand.deMemloc
				                (Operand.toX86Operand operand)
					     of SOME memloc 
					      => x86.MemLocSet.add(live, memloc)
					      | NONE => live)})}))))
	  handle exn
	   => Error.bug ("x86Translate.Transfer.toX86Blocks::" ^ 
			 (case exn
			    of Fail s => s
			     | _ => "?"))
    end

  structure Block =
    struct
      open Machine.Block

      fun toX86Blocks {block as T {label, 
				   live, 
				   kind, 
				   profileInfo as 
				   {ssa as {func = funcProfileInfo,
					    label = labelProfileInfo}, ...},
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
		  x86.Block.T'
		  {entry = NONE,
		   profileInfo = x86.ProfileInfo.none,
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
			      (Transfer.toX86Blocks {transfer = transfer,
						     transInfo = transInfo}),
			      fn (statement,l)
			       => AppendList.append
			          (Statement.toX86Blocks 
				   {statement = statement,
				    transInfo = transInfo}, l)))

	    val pseudo_blocks = AppendList.toList pseudo_blocks
		 
	    val blocks = x86.Block.compress pseudo_blocks

	    fun addProfileInfo profileInfo
	      = let
		  val profileInfo
		    = x86.ProfileInfo.add
		      (profileInfo,
		       {profileLevel = 0,
			profileName = funcProfileInfo})
		  val profileInfo
		    = x86.ProfileInfo.add
		      (profileInfo,
		       {profileLevel = 1,
			profileName = labelProfileInfo})
		in
		  profileInfo
		end

	    val blocks
	      = if !Control.profile
		  then List.map
		       (blocks,
			fn (x86.Block.T {entry, profileInfo, 
					 statements, transfer})
			 => let
			      val label = x86.Entry.label entry
			      val profileInfo
				= x86.ProfileInfo.add
				  (addProfileInfo profileInfo,
				   {profileLevel = 2,
				    profileName = Label.toString label})
			    in
			      x86.Block.T {entry = entry,
					   profileInfo = profileInfo,
					   statements = statements,
					   transfer = transfer}
			    end)
		  else blocks
	  in
	    blocks
	  end
	  handle exn
	   => Error.bug ("x86Translate.Block.toX86Blocks::" ^ 
			 (case exn
			    of Fail s => s
			     | _ => "?"))
    end

  structure Chunk =
    struct
      open Machine.Chunk

      fun toX86Chunk {chunk as T {blocks, ...}, 
		      frameLayouts, 
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
			     frameLayouts = frameLayouts,
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
	   => Error.bug ("x86Translate.Chunk.toX86Chunk::" ^ 
			 (case exn
			    of Fail s => s
			     | _ => "?"))
    end

  structure Program =
    struct
      open Machine.Program

      fun toX86Chunks {program as T {chunks,...},
		       frameLayouts,
		       liveInfo} 
	= let
	    val chunks
	      = List.map(chunks,
			 fn chunk
			  => Chunk.toX86Chunk {chunk = chunk,
					       frameLayouts = frameLayouts,
					       liveInfo = liveInfo})
	  in 
	    chunks
	  end
    end

  fun translateChunk {chunk: x86MLton.Machine.Chunk.t,
		      frameLayouts: x86MLton.Machine.Label.t ->
		                    {size: int, frameLayoutsIndex: int} option,
		      liveInfo: x86Liveness.LiveInfo.t} :
                     {chunk: x86.Chunk.t}
		      
    = {chunk = Chunk.toX86Chunk {chunk = chunk,
				 frameLayouts = frameLayouts,
				 liveInfo = liveInfo}}

  val (translateChunk, translateChunk_msg)
    = tracerTop
      "translateChunk"
      translateChunk

  fun translateChunk_totals ()
    = (translateChunk_msg ();
       Control.indent ();
       Control.unindent ())


  fun translateProgram {program: x86MLton.Machine.Program.t,
			frameLayouts: x86MLton.Machine.Label.t ->
			              {size: int, frameLayoutsIndex: int} option,
			liveInfo: x86Liveness.LiveInfo.t} :
                       {chunks: x86.Chunk.t list}
    = {chunks = Program.toX86Chunks {program = program,
				     frameLayouts = frameLayouts,
				     liveInfo = liveInfo}}

  val (translateProgram, translateProgram_msg)
    = tracerTop
      "translateProgram"
      translateProgram

  fun translateProgram_totals ()
    = (translateProgram_msg ();
       Control.indent ();
       Control.unindent ())
end
