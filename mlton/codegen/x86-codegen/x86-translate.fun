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

  structure MachineOutput = x86MLton.MachineOutput

  structure Label = MachineOutput.Label
  structure Prim = MachineOutput.Prim
    
  structure Type =
    struct
      open MachineOutput.Type
      fun name t = case dest t 
		     of Char => "C"
		      | Double => "D"
		      | Int => "I"
		      | Pointer => "P"
		      | Uint => "U"
		      | Void => "V"
    end
    
  structure Local =
    struct
      open MachineOutput.Register

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
      open MachineOutput.Global

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
      open MachineOutput.Operand

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
	   | ArrayOffset {base, offset, ty}
	   => let
		val base = toX86Operand base
		val index = toX86Operand offset

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
    end

  structure PrimInfo =
    struct
      open MachineOutput.PrimInfo

      val toX86PrimInfo
	= fn None => x86MLton.PrimInfo.None
	   | Overflow (label, live)
	   => x86MLton.PrimInfo.Overflow (label, List.map(live, Operand.toX86Operand))
	   | Runtime (MachineOutput.GCInfo.T {frameSize, live, return})
           => x86MLton.PrimInfo.Runtime {frameSize = frameSize,
					 live = List.map(live, Operand.toX86Operand),
					 return = return}
	   | Normal live
	   => x86MLton.PrimInfo.Normal (List.map(live, Operand.toX86Operand))
    end

  structure LimitCheck =
    struct
      val limitCheckTemp = x86MLton.limitCheckTempContentsOperand

      val stackTop = x86MLton.gcState_stackTopContentsOperand
      val stackLimit = x86MLton.gcState_stackLimitContentsOperand

      val frontier = x86MLton.gcState_frontierContentsOperand
      val limit = x86MLton.gcState_limitContentsOperand

      datatype kind = Const of int
	            | Variable of x86.MemLoc.t

      fun limitCheck {info = {frameSize, live, return},
		      bytes: kind, 
		      stackCheck: bool,
		      loopGC: x86.Label.t option,
		      frameLayouts: x86MLton.MachineOutput.Label.t ->
			            {size: int, frameLayoutsIndex: int} option,
		      liveInfo: x86Liveness.LiveInfo.t}
	= let
	    val liveDoGC 
	      = case bytes
		  of Const _ => live
		   | Variable bytes => (x86.Operand.memloc bytes)::live

	    val doGC = Label.newString "doGC"
	    val _ = x86Liveness.LiveInfo.setLiveOperands(liveInfo, doGC, liveDoGC)
	    val skipGC = Label.newString "skipGC"
	    val _ = x86Liveness.LiveInfo.setLiveOperands(liveInfo, skipGC, live)
	    val _ = case loopGC
		      of NONE => ()
		       | SOME l => x86Liveness.LiveInfo.setLiveOperands
			           (liveInfo, l, live)

	    val info = {frameSize = frameSize,
			live = live,
			return = return}
	  in
	    AppendList.appends
	    [(if stackCheck
		then let
		       val checkFrontier = Label.newString "checkFrontier"
		       val _ = x86Liveness.LiveInfo.setLiveOperands(liveInfo, 
								    checkFrontier, 
								    liveDoGC)
		     in AppendList.fromList
		        [(* if (stackTop >= stackLimit) goto doGC *)
			 x86.Block.T'
			 {entry = NONE,
			  profileInfo = x86.ProfileInfo.none,
			  statements
			  = [x86.Assembly.instruction_cmp
			     {src1 = stackTop,
			      src2 = stackLimit,
			      size = x86MLton.pointerSize}],
			  transfer 
			  = SOME (x86.Transfer.iff 
				  {condition = x86.Instruction.AE,
				   truee = doGC,
				   falsee = checkFrontier})},
			 (* checkFrontier: *)
			 x86.Block.T'
			 {entry = SOME (x86.Entry.jump {label = checkFrontier}),
			  profileInfo = x86.ProfileInfo.none,
			  statements = [],
			  transfer = NONE}]
		     end
		else AppendList.empty),
	     AppendList.fromList
	     [(* if (frontier + bytes <= limit) goto skipGC *)
	      x86.Block.T'
	      {entry = NONE,
	       profileInfo = x86.ProfileInfo.none,
	       statements
	       = case bytes
		   of Const 0
		    => [x86.Assembly.instruction_cmp
			{src1 = frontier,
			 src2 = limit,
			 size = x86MLton.pointerSize}]
		    | Const bytes
		    => let
			 val frontier_offset
			   = (x86.Operand.memloc o x86.MemLoc.simple) 
			     {base = x86MLton.gcState_frontierContents,
			      index = x86.Immediate.const_int bytes,
			      scale = x86.Scale.One,
			      size = x86MLton.pointerSize,
			      class = x86MLton.Classes.Heap}
		       in
			 [x86.Assembly.instruction_lea
			  {dst = limitCheckTemp,
			   src = frontier_offset,
			   size = x86MLton.pointerSize},
			  x86.Assembly.instruction_cmp
			  {src1 = limitCheckTemp,
			   src2 = limit,
			   size = x86MLton.pointerSize}]
		       end
		    | Variable bytes
		    => let
			 val frontier_offset
			   = (x86.Operand.memloc o x86.MemLoc.complex) 
			     {base = x86MLton.gcState_frontierContents,
			      index = bytes,
			      scale = x86.Scale.One,
			      size = x86MLton.pointerSize,
			      class = x86MLton.Classes.Heap}
		       in
			 [x86.Assembly.instruction_lea
			  {dst = limitCheckTemp,
			   src = frontier_offset,
			   size = x86MLton.pointerSize},
			  x86.Assembly.instruction_cmp
			  {src1 = limitCheckTemp,
			   src2 = limit,
			   size = x86MLton.pointerSize}]
		       end,
	       transfer 
	       = SOME (x86.Transfer.iff {condition = x86.Instruction.BE,
					 truee = skipGC,
					 falsee = doGC})},
	      (* doGC: *)
	      x86.Block.T'
	      {entry = SOME (x86.Entry.jump {label = doGC}),
	       profileInfo = x86.ProfileInfo.none,
	       statements = [],
	       transfer = NONE}],
	     (x86MLton.invokeRuntime
	      {target = Label.fromString "GC_gc",
	       args = [(x86.Operand.immediate_label x86MLton.gcState, 
			x86MLton.pointerSize),
		       (case bytes
			  of Const bytes => x86.Operand.immediate_const_int bytes
			   | Variable bytes => x86.Operand.memloc bytes, 
			x86MLton.wordSize),
		       (x86.Operand.immediate_const_int 0, x86MLton.wordSize),
		       (x86MLton.fileName, x86MLton.pointerSize),
		       (x86MLton.fileLine (), x86MLton.wordSize)],
	       info = info,
	       frameLayouts = frameLayouts,
	       liveInfo = liveInfo}),
	     AppendList.fromList
	     [(* goto loopGC *)
	      x86.Block.T'
	      {entry = NONE,
	       profileInfo = x86.ProfileInfo.none,
	       statements = [],
	       transfer 
	       = SOME (x86.Transfer.goto {target = case loopGC
						     of SOME loopGC => loopGC
						      | NONE => skipGC})},
	      (* skipGC: *)
	      x86.Block.T'
	      {entry = SOME (x86.Entry.jump {label = skipGC}),
	       profileInfo = x86.ProfileInfo.none,
	       statements = [],
	       transfer = NONE}]]
	  end
    end

  structure Statement =
    struct
      open MachineOutput.Statement

      val toX86Blocks_AllocateArray_limitCheck 
	= fn {numElts,
	      limitCheck = NONE, 
	      frameLayouts,
	      liveInfo}
	   => AppendList.empty
	   | {numElts,
	      limitCheck as SOME {gcInfo, bytesPerElt = 0, bytesAllocated},
	      frameLayouts,
	      liveInfo}
	   => let
		val MachineOutput.GCInfo.T {frameSize, live, return} = gcInfo
		val info = {frameSize = frameSize,
			    live = List.map(numElts::live, Operand.toX86Operand),
			    return = return}
		val arrayLimitCheckLoop = Label.newString "arrayLimitCheckLoopA"
	      in 
		AppendList.appends
	        [AppendList.fromList
		 [x86.Block.T'
		  {entry = NONE,
		   profileInfo = x86.ProfileInfo.none,
		   statements = [],
		   transfer = SOME (x86.Transfer.goto
				    {target = arrayLimitCheckLoop})},
		  x86.Block.T'
		  {entry = SOME (x86.Entry.jump {label = arrayLimitCheckLoop}),
		   profileInfo = x86.ProfileInfo.none,
		   statements = [],
		   transfer = NONE}],
		 (LimitCheck.limitCheck 
		  {info = info,
		   bytes = LimitCheck.Const bytesAllocated,
		   stackCheck = false,
		   loopGC = SOME arrayLimitCheckLoop,
		   frameLayouts = frameLayouts,
		   liveInfo = liveInfo})]
	      end
	   | {numElts as Operand.Int numElts',
	      limitCheck as SOME {gcInfo, bytesPerElt, bytesAllocated},
	      frameLayouts,
	      liveInfo} 
	   => let
		val MachineOutput.GCInfo.T {frameSize, live, return} = gcInfo
		val info = {frameSize = frameSize,
			    live = List.map(numElts::live, Operand.toX86Operand),
			    return = return}
		val arrayLimitCheckLoop = Label.newString "arrayLimitCheckLoopB"
	      in 
		AppendList.appends
		[AppendList.fromList
		 [x86.Block.T'
		  {entry = NONE,
		   profileInfo = x86.ProfileInfo.none,
		   statements = [],
		   transfer = SOME (x86.Transfer.goto
				    {target = arrayLimitCheckLoop})},
		  x86.Block.T'
		  {entry = SOME (x86.Entry.jump {label = arrayLimitCheckLoop}),
		   profileInfo = x86.ProfileInfo.none,
		   statements = [],
		   transfer = NONE}],
		 (LimitCheck.limitCheck 
		  {info = info,
		   bytes = LimitCheck.Const
		           (numElts' * bytesPerElt + bytesAllocated),
		   stackCheck = false,
		   loopGC = SOME arrayLimitCheckLoop,
		   frameLayouts = frameLayouts,
		   liveInfo = liveInfo})]
	      end
	   | {numElts,
	      limitCheck as SOME {gcInfo, bytesPerElt, bytesAllocated},
	      frameLayouts,
	      liveInfo}
	   => let
		val MachineOutput.GCInfo.T {frameSize, live, return} = gcInfo
		val info = {frameSize = frameSize,
			    live = List.map(numElts::live, Operand.toX86Operand),
			    return = return}
		val arrayLimitCheckLoop = Label.newString "arrayLimitCheckLoopC"

		val numEltsSize = Operand.toX86Size numElts
		val numElts = Operand.toX86Operand numElts
		val _
		  = Assert.assert
		    ("toX86Blocks: AllocateArray, numEltsSize",
		     fn () => numEltsSize = x86MLton.wordSize)
		val arrayAllocateTemp' = x86MLton.arrayAllocateTempContents
		val arrayAllocateTemp = x86MLton.arrayAllocateTempContentsOperand
	      in 
		AppendList.appends
		[AppendList.fromList
		 [x86.Block.T'
		  {entry = NONE,
		   profileInfo = x86.ProfileInfo.none,
		   statements = [],
		   transfer = SOME (x86.Transfer.goto
				    {target = arrayLimitCheckLoop})},
		  x86.Block.T'
		  {entry = SOME (x86.Entry.jump {label = arrayLimitCheckLoop}),
		   profileInfo = x86.ProfileInfo.none,
		   statements 
		   = [(* arrayAllocateTemp 
		       *    = numElts * bytesPerElt + bytesAllocated 
		       *)
		      x86.Assembly.instruction_mov
		      {dst = arrayAllocateTemp,
		       src = numElts,
		       size = x86MLton.wordSize},
		      x86.Assembly.instruction_pmd
		      {oper = x86.Instruction.MUL,
		       dst = arrayAllocateTemp,
		       src = x86.Operand.immediate_const_int 
		             bytesPerElt,
		       size = x86MLton.wordSize},
		      x86.Assembly.instruction_binal
		      {oper = x86.Instruction.ADD,
		       dst = arrayAllocateTemp,
		       src = x86.Operand.immediate_const_int 
		             bytesAllocated,
		       size = x86MLton.wordSize}],
		   transfer = NONE}],
		(LimitCheck.limitCheck 
		 {info = info,
		  bytes = LimitCheck.Variable arrayAllocateTemp',
		  stackCheck = false,	
		  loopGC = SOME arrayLimitCheckLoop,
		  frameLayouts = frameLayouts,
		  liveInfo = liveInfo})]
	      end

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
			     src = x86.Operand.immediate_const_int
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

      val toX86Blocks_AllocateArray_arrayPointers_loop
	= fn {dst,
	      live,
	      liveInfo}
	   => let
		val dstsize = Operand.toX86Size dst
		val dst = Operand.toX86Operand dst
		val _ 
		  = Assert.assert
		    ("toX86Blocks: AllocateArray, dstsize",
		     fn () => dstsize = x86MLton.pointerSize)

		val liveIn = dst::live

		val arrayAllocateLoopTemp 
		  = x86MLton.arrayAllocateLoopTempContentsOperand
		val arrayAllocateLoopTempDeref 
		  = x86MLton.arrayAllocateLoopTempDerefOperand
		val frontier 
		  = x86MLton.gcState_frontierContentsOperand

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
			 liveIn)
		val arrayPointersZJoin
		  = Label.newString "arrayPointersZJoin"
		val _ = x86Liveness.LiveInfo.setLiveOperands
		        (liveInfo,
			 arrayPointersZJoin, 
			 liveIn)
		val arrayPointersLoop
		  = Label.newString "arrayPointersLoop"
		val _ = x86Liveness.LiveInfo.setLiveOperands
		        (liveInfo,
			 arrayPointersLoop, 
			 x86MLton.arrayAllocateLoopTempContentsOperand::liveIn)
		val arrayPointersLoopJoin
		  = Label.newString "arrayPointersLoopJoin"
		val _ = x86Liveness.LiveInfo.setLiveOperands
		        (liveInfo,
			 arrayPointersLoopJoin, 
			 liveIn)
	      in
		AppendList.fromList
		[(* for (arrayAllocateLoopTemp = dst; 
		  *      arrayAllocateLoopTemp < frontier; 
		  *      arrayAllocateLoopTemp++)
		  *    *arrayAllocateLoopTemp = 0x1 
		  *)
		 x86.Block.T'
		 {entry = NONE,
		  profileInfo = x86.ProfileInfo.none,
		  statements 
		  = [(* arrayAllocateLoopTemp = dst *)
		     x86.Assembly.instruction_mov
		     {dst = arrayAllocateLoopTemp,
		      src = dst,
		      size = x86MLton.pointerSize}],
		  transfer = NONE},
		 (* if arrayAllocateLoopTemp >= frontier 
		  *    goto arrayPointersLoopJoin
		  *)
		 x86.Block.T'
		 {entry = NONE,
		  profileInfo = x86.ProfileInfo.none,
		  statements
		  = [x86.Assembly.instruction_cmp
		     {src1 = arrayAllocateLoopTemp,
		      src2 = frontier,
		      size = x86MLton.pointerSize}],
		  transfer 
		  = SOME (x86.Transfer.iff
			  {condition = x86.Instruction.AE,
			   truee = arrayPointersLoopJoin,
			   falsee = arrayPointersLoop})},
		 (* arrayPointersLoop: *)
		 x86.Block.T'
		 {entry = SOME (x86.Entry.jump {label = arrayPointersLoop}),
		  profileInfo = x86.ProfileInfo.none,
		  statements
		  = [x86.Assembly.instruction_mov
		     {dst = arrayAllocateLoopTempDeref,
		      src = x86.Operand.immediate_const_word 0wx1,
		      size = x86MLton.pointerSize},
		     x86.Assembly.instruction_binal
		     {oper = x86.Instruction.ADD,
		      dst = arrayAllocateLoopTemp,
		      src = x86.Operand.immediate_const_int pointerBytes,
		      size = x86MLton.pointerSize},
		     x86.Assembly.instruction_cmp
		     {src1 = arrayAllocateLoopTemp,
		      src2 = frontier,
		      size = x86MLton.pointerSize}],
		  transfer 
		  = SOME (x86.Transfer.iff
			  {condition = x86.Instruction.B,
			   truee = arrayPointersLoop,
			   falsee = arrayPointersLoopJoin})},
		 (* arrayPointersLoopJoin: *)
		 x86.Block.T'
		 {entry = SOME (x86.Entry.jump {label = arrayPointersLoopJoin}),
		  profileInfo = x86.ProfileInfo.none,
		  statements = [],
		  transfer = NONE}]
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
		AppendList.cons
		((x86.Block.T'
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
		   transfer = NONE}),
		 (toX86Blocks_AllocateArray_arrayPointers_loop 
		  {dst = dst,
		   live = live,
		   liveInfo = liveInfo}))
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
		val arrayPointersLoop
		  = Label.newString "arrayPointersLoop"
		val _ = x86Liveness.LiveInfo.setLiveOperands
		        (liveInfo,
			 arrayPointersLoop, 
			 liveIn)
		val arrayPointersLoopJoin
		  = Label.newString "arrayPointersLoopJoin"
		val _ = x86Liveness.LiveInfo.setLiveOperands
		        (liveInfo,
			 arrayPointersLoopJoin, 
			 liveIn)		  
	      in
		AppendList.append
		(AppendList.fromList
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
		    transfer = NONE}],
		(toX86Blocks_AllocateArray_arrayPointers_loop 
		 {dst = dst,
		  live = live,
		  liveInfo = liveInfo}))
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
		       frameLayouts,
		       liveInfo}
	= case statement
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
	     | Push bytes => Error.bug "toX86Blocks: Push"
	     | Assign {dst, oper, args, pinfo}
   	     => let
		  val (comment_begin,
		       comment_end) = comments statement

		  val args 
		    = List.map(args,
			       fn arg 
			        => (Operand.toX86Operand arg,
				    x86MLton.toX86Size (Operand.ty arg)))
		    
		  val pinfo
		    = PrimInfo.toX86PrimInfo pinfo

		  val dst 
		    = Option.map(dst, 
				 fn dst 
				  => (Operand.toX86Operand dst, 
				      x86MLton.toX86Size (Operand.ty dst)))
		in
		  AppendList.appends
		  [comment_begin,
		   (x86MLton.applyPrim {oper = oper,
					args = args,
					dst = dst,
					pinfo = pinfo,
					frameLayouts = frameLayouts,
					liveInfo = liveInfo}),
		   comment_end]
		end
	     | LimitCheck {info, bytes, stackCheck}
             => let
		  val (comment_begin,
		       comment_end) = comments statement

		  val MachineOutput.GCInfo.T {frameSize, live, return} = info
		  val info = {frameSize = frameSize,
			      live = List.map(live, Operand.toX86Operand),
			      return = return}
		  val statementLimitCheckLoop 
		    = Label.newString "statementLimitCheckLoop"
		in 
		  AppendList.appends
		  [comment_begin,
		   AppendList.fromList
		   [x86.Block.T'
		    {entry = NONE,
		     profileInfo = x86.ProfileInfo.none,
		     statements = [],
		     transfer 
		     = SOME (x86.Transfer.goto 
			     {target = statementLimitCheckLoop})},
		    x86.Block.T'
		    {entry = SOME (x86.Entry.jump {label = statementLimitCheckLoop}),
		     profileInfo = x86.ProfileInfo.none,
		     statements = [],
		     transfer = NONE}],
		   (LimitCheck.limitCheck
		    {info = info,
		     bytes = LimitCheck.Const bytes,
		     stackCheck = stackCheck,
		     loopGC = SOME statementLimitCheckLoop,
		     frameLayouts = frameLayouts,
		     liveInfo = liveInfo}),
		   comment_end]
		end
	     | SaveExnStack {offset}
	     => let
		  val (comment_begin,
		       comment_end) = comments statement

		  val exnStack = x86MLton.gcState_currentThread_exnStackContentsOperand
		  val stackTop = x86MLton.gcState_stackTopContentsOperand
		  val stackBottom = x86MLton.gcState_stackBottomContentsOperand
		    
		  val tempP 
		    = let
			val index 
			  = x86.Immediate.const_int (offset + wordBytes)
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
		     = [(* *(stackTop + offset + wordSize) = exnStack *)
			x86.Assembly.instruction_mov 
			{dst = tempP,
			 src = exnStack,
			 size = x86MLton.pointerSize},
			(* exnStack = (stackTop + offset) - stackBottom *)
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
	     | RestoreExnStack {offset}
	     => let
		  val (comment_begin,
		       comment_end) = comments statement
		    
		  val exnStack 
		    = x86.Operand.memloc 
		      x86MLton.gcState_currentThread_exnStackContents
		      
		  val tempP 
		    = let
			val index 
			  = x86.Immediate.const_int (offset + wordBytes)
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
		     = [(* exnStack = *(stackTop + offset + wordSize) *)
			x86.Assembly.instruction_mov 
			{dst = exnStack,
			 src = tempP,
			 size = x86MLton.pointerSize}],
		     transfer = NONE}),
		   comment_end]
		end
	     | Allocate {dst, size, stores, numPointers, numWordsNonPointers}
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
(*
			      val index 
				= x86.Immediate.const_int (objectHeaderBytes + offset)
			      val memloc 
				= x86.MemLoc.simple 
			          {base = x86MLton.gcState_frontierContents, 
				   index = index,
				   scale = x86.Scale.One,
				   size = size,
				   class = x86MLton.Classes.Heap}
*)
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
			    | _ 
			    => Error.bug "toX86Blocks: Allocate")::l
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
		       (List.foldr(stores,
				   [(* frontier += objectHeaderSize + size *)
				    x86.Assembly.instruction_binal
				    {oper = x86.Instruction.ADD,
				     dst = frontier,
				     src = x86.Operand.immediate_const_int 
				           (objectHeaderBytes + size),
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
			(List.foldr(stores,
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
	     | AllocateArray {dst,
			      numElts,
			      numPointers,
			      numBytesNonPointers,
			      live,
			      limitCheck}
	     => let
		  val (comment_begin,
		       comment_end) = comments statement

		  val live = List.map(live, Operand.toX86Operand)
		in
		  AppendList.appends
		  [comment_begin,
		   (toX86Blocks_AllocateArray_limitCheck
		    {numElts = numElts,
		     limitCheck = limitCheck,
		     frameLayouts = frameLayouts,
		     liveInfo = liveInfo}),
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
		end
    end

  structure Transfer =
    struct
      open MachineOutput.Transfer

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
(*
	    if Label.equals(a, b)
	      then AppendList.single
		   (x86.Block.T'
		    {entry = NONE,
		     profileInfo = x86.ProfileInfo.none,
		     statements = [],
		     transfer = SOME (x86.Transfer.goto {target = a})})
	      else 
*)
                   AppendList.single
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
(*
	    if Label.equals(a, b)
	      then AppendList.single
		   (x86.Block.T'
		    {entry = NONE,
		     profileInfo = x86.ProfileInfo.none,
		     statements = [],
		     transfer = SOME (x86.Transfer.goto {target = a})})
	      else 
*)
                   AppendList.single
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

      fun toX86Blocks {transfer, liveInfo}
	= (case transfer
	     of Bug 
	      => AppendList.append
	         (comments transfer,
		  x86MLton.bug {liveInfo = liveInfo})
	      | Return {live}
	      => AppendList.append
	         (comments transfer,
		  AppendList.single
		  (x86.Block.T'
		   {entry = NONE,
		    profileInfo = x86.ProfileInfo.none,
		    statements = [],
		    transfer = SOME (x86.Transfer.return 
				     {live = List.keepAllMap
				             (live,
					      x86.Operand.deMemloc o
					      Operand.toX86Operand)})}))
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
			    {live = [x86MLton.gcState_stackBottomContents,
				     x86MLton.gcState_currentThread_exnStackContents]})}))
	      | Switch {test, cases, default}
	      => AppendList.append
	         (comments transfer,
		  (case cases 
		     of MachineOutput.Cases.Char cases 
		      => doSwitchChar (test,cases,default)
		      | MachineOutput.Cases.Int cases 
		      => doSwitchInt (test,cases,default)
	              | MachineOutput.Cases.Word cases 
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
	      | NearJump {label, return}
	      => (case return
		    of SOME {return, handler, size}
		     => Error.bug (concat ["toX86Blocks: NearJump, return ",
					   Label.toString return])
		     | NONE
		     => AppendList.append
		        (comments transfer,
			 AppendList.single
			 ((* goto label *)
			  x86.Block.T'
			  {entry = NONE,
			   profileInfo = x86.ProfileInfo.none,
			   statements = [],
			   transfer = SOME (x86.Transfer.goto {target = label})})))
	      | FarJump {label, live, return, ...}
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
				    live = List.keepAllMap
				           (live,
					    x86.Operand.deMemloc o 
					    Operand.toX86Operand),
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
				    live = List.keepAllMap
				           (live,
					    x86.Operand.deMemloc o 
					    Operand.toX86Operand)})}))))
    end

  structure Block =
    struct
      open MachineOutput.Block

      fun toX86Blocks {block as T {label, 
				   kind, 
				   live,
				   profileName,
				   statements, 
				   transfer},
		       frameLayouts,
		       liveInfo}
	= let
	    val live' = live
	    val live = List.map(live, Operand.toX86Operand)

	    fun frameInfo frameSize
	      = case frameLayouts label
		  of NONE => Error.bug ("toX86Blocks: frameInfo")
		   | SOME {size, frameLayoutsIndex}
		   => let
			val _
			  = Assert.assert
			    ("toX86Blocks: frame size ",
			     fn () => size = frameSize)
		      in 
			x86.Entry.FrameInfo.frameInfo
			{size = size,
			 frameLayoutsIndex = frameLayoutsIndex}
		      end
	    val entry
	      = case kind
		  of Kind.Jump 
		   => let
			val _ = x86Liveness.LiveInfo.setLiveOperands
			        (liveInfo, label, live)
		      in
			x86.Entry.jump {label = label}
		      end
		   | Kind.Func {args}
		   => let
			val args = List.map(args, Operand.toX86Operand)

			val live = List.keepAll
			           (live,
				    fn operand 
				     => not (List.contains
					     (args, operand, x86.Operand.eq)))

			val _ = x86Liveness.LiveInfo.setLiveOperands
			        (liveInfo, label, live)

			val args = List.keepAllMap(args, x86.Operand.deMemloc)
		      in 
			x86.Entry.func {label = label, 
					live = args}
		      end
		   | Kind.Cont {args, size}
		   => let
			val args = List.map(args, Operand.toX86Operand)

			val live = List.keepAll
			           (live,
				    fn operand 
				     => not (List.contains
					     (args, operand, x86.Operand.eq)))

			val _ = x86Liveness.LiveInfo.setLiveOperands
			        (liveInfo, label, live)

			val args = List.keepAllMap(args, x86.Operand.deMemloc)
		      in 
			x86.Entry.cont {label = label, 
					live = args,
					frameInfo = frameInfo size}
		      end
		   | Kind.Handler {size}
		   => let
			val _ = x86Liveness.LiveInfo.setLiveOperands
			        (liveInfo, label, live)		      
		      in 
			x86.Entry.handler 
			{label = label,
			 live = [],
			 frameInfo = x86.Entry.FrameInfo.frameInfo
			             {size = size,
				      frameLayoutsIndex = ~1}}
		      end
		   
	    val pseudo_blocks
	      = AppendList.cons 
	        (x86.Block.T'
		 {entry = SOME entry,
		  profileInfo = x86.ProfileInfo.none,
		  statements 
		  = if !Control.Native.commented > 0
		      then let
			     val comment
			       = "Live: " ^
			         (argsToString
				  (List.map(live', fn l => Operand.toString l)))
			   in
			     [x86.Assembly.comment comment]
			   end
		      else [],
		  transfer = NONE},
		 Array.foldr(statements,
			     (Transfer.toX86Blocks {transfer = transfer,
						    liveInfo = liveInfo}),
			     fn (statement,l)
			      => AppendList.append
			         (Statement.toX86Blocks 
				  {statement = statement,
				   frameLayouts = frameLayouts,
				   liveInfo = liveInfo}, l)))

	    val pseudo_blocks = AppendList.toList pseudo_blocks
		 
	    val blocks = x86.Block.compress pseudo_blocks

	    fun addProfileInfo profileInfo
	      = let
		  val profileInfo
		    = x86.ProfileInfo.add
		      (profileInfo,
		       {profileLevel = 0,
			profileName = profileName})
		  val profileInfo
		    = x86.ProfileInfo.add
		      (profileInfo,
		       {profileLevel = 1,
			profileName = Label.toString label})
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
    end

  structure Chunk =
    struct
      open MachineOutput.Chunk

      fun toX86Chunk {chunk as T {blocks, ...}, 
		      frameLayouts, 
		      liveInfo}
	= let
	    val blocks 
	      = List.concatMap(blocks, 
				fn block
				 => Block.toX86Blocks 
				    {block = block,
				     frameLayouts = frameLayouts,
				     liveInfo = liveInfo})
	  in
	    x86.Chunk.T {blocks = blocks}
			 
	  end
    end

  structure Program =
    struct
      open MachineOutput.Program

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

  fun translateChunk {chunk: x86MLton.MachineOutput.Chunk.t,
		      frameLayouts: x86MLton.MachineOutput.Label.t ->
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


  fun translateProgram {program: x86MLton.MachineOutput.Program.t,
			frameLayouts: x86MLton.MachineOutput.Label.t ->
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
