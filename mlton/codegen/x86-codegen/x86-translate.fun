(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor x86Translate(S: X86_TRANSLATE_STRUCTS): X86_TRANSLATE =
struct

  open S

  val tracer
    = Control.traceBatch 
(*
    = fn s => fn f => (Control.trace (Control.Detail, s) f, fn () => ())
*)

  val wordSize: int = 4
  val pointerSize = wordSize
  val objectHeaderSize = wordSize
  val arrayHeaderSize = 2 * wordSize
  val intInfOverhead = arrayHeaderSize + wordSize (* for the sign *)

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
			    commit = x86.MemLoc.Commit.commit 
			             {isTemp = true,
				      onFlush = false},
			    class = x86MLton.Runtime}
	  end

      fun eq(T{index = index1, ty = ty1},T{index = index2, ty = ty2})
	= Type.equals(ty1, ty2) 
	  andalso index1 = index2

      fun toString(T{index, ty}) =
         concat["R", Type.name ty, "(", Int.toString index, ")"]
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
			    commit = x86.MemLoc.Commit.commit
			             {isTemp = false,
				      onFlush = true},
			    class = x86MLton.Runtime}
	  end

      fun toString(T{index, ty}) =
         concat["G", Type.name ty, "(", Int.toString index, ")"]
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
		     commit = x86.MemLoc.Commit.commit
		              {isTemp = false,
			       onFlush = true},
		     class = x86MLton.Runtime}
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
		     commit = x86.MemLoc.Commit.commit
		              {isTemp = false,
			       onFlush = true},
		     class = x86MLton.Stack}
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
			    commit = x86.MemLoc.Commit.commit
			             {isTemp = false,
				      onFlush = true},
			    class = x86MLton.Heap}
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
			    commit = x86.MemLoc.Commit.commit
			             {isTemp = false,
				      onFlush = true},
			    class = x86MLton.Heap}
			| (SOME base, _, SOME index)
		        => x86.MemLoc.complex 
			   {base = base,
			    index = index,
			    scale = x86MLton.toX86Scale ty,
			    size = x86MLton.toX86Size ty,
			    commit = x86.MemLoc.Commit.commit
			             {isTemp = false,
				      onFlush = true},
			    class = x86MLton.Heap}
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
			   commit = x86.MemLoc.Commit.commit 
			            {isTemp = false,
				     onFlush = true},
			   class = x86MLton.Heap}
		       | _
		       => Error.bug ("toX86Operand: strange Contents" ^
				     " base: " ^
				      (x86.Operand.toString base))
	      in
		x86.Operand.memloc memloc
	      end
    end

  structure LimitCheck =
    struct
      val limitCheckTemp = x86.Operand.memloc x86MLton.limitCheckTempContents

      val stackTop = x86.Operand.memloc x86MLton.gcState_stackTopContents
      val stackLimit = x86.Operand.memloc x86MLton.gcState_stackLimitContents

      val frontier = x86.Operand.memloc x86MLton.gcState_frontierContents
      val limit = x86.Operand.memloc x86MLton.gcState_limitContents

      val line = Label.fromString "__LINE__"

      fun limitCheck {info = MachineOutput.GCInfo.T {frameSize, return},
		      bytes: x86.Operand.t, 
		      stackCheck: bool,
		      loopGC: x86.Label.t option,
		      liveInfo as {get : x86.Label.t 
				         -> x86.MemLoc.t list,
				   set}}
	= let
	    val (comment_begin,
		 comment_end)
	      = if !Control.Native.commented > 0
		  then ([x86.Block.T'
			 {label = NONE,
			  profileInfo = x86.ProfileInfo.none,
			  statements 
			  = [x86.Assembly.comment "begin limitCheck"],
			  transfer = NONE}],
			[x86.Block.T'
			 {label = NONE,
			  profileInfo = x86.ProfileInfo.none,
			  statements 
			  = [x86.Assembly.comment "end limitCheck"],
			  transfer = NONE}])
		  else ([],[])		 

	    val live = case x86.Operand.deMemloc bytes
			 of SOME memloc => if x86.MemLoc.isTemp memloc
					     then [memloc]
					     else []
			  | NONE => []

	    val _ = set(return, [])
	    val doGC = Label.newString "doGC"
	    val _ = set(doGC, live)
	    val skipGC = Label.newString "skipGC"
	    val _ = set(skipGC, [])
	  in
	    List.concat
	    [comment_begin,
	     (if stackCheck
		then let
		       val checkFrontier = Label.newString "checkFrontier"
		       val _ = set(checkFrontier, live)
		     in [(* if (stackTop >= stackLimit) goto doGC *)
			 x86.Block.T'
			 {label = NONE,
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
			 {label = SOME checkFrontier,
			  profileInfo = x86.ProfileInfo.none,
			  statements = [],
			  transfer = NONE}]
		     end
		else []),
	     ((* if (frontier + bytes <= limit) goto skipGC *)
	      x86.Block.T'
	      {label = NONE,
	       profileInfo = x86.ProfileInfo.none,
	       statements
	       = case (x86.Operand.deImmediate bytes,
		       x86.Operand.deMemloc bytes)
		   of (SOME bytes,_)
		    => (case x86.Immediate.deConst bytes
			  of SOME (x86.Immediate.Int 0)
			   => [x86.Assembly.instruction_cmp
			       {src1 = frontier,
				src2 = limit,
				size = x86MLton.pointerSize}]
			   | _ 
			   => let
				val frontier_offset
				  = (x86.Operand.memloc o x86.MemLoc.simple) 
				    {base = x86MLton.gcState_frontierContents,
				     index = bytes,
				     scale = x86.Scale.One,
				     size = x86MLton.pointerSize,
				     commit = x86.MemLoc.Commit.commit
				              {isTemp = false,
					       onFlush = true},
				     class = x86MLton.Heap}
			      in
				[x86.Assembly.instruction_lea
				 {dst = limitCheckTemp,
				  src = frontier_offset,
				  size = x86MLton.pointerSize},
				 x86.Assembly.instruction_cmp
				 {src1 = limitCheckTemp,
				  src2 = limit,
				  size = x86MLton.pointerSize}]
			      end)
		    | (_, SOME bytes)
		    => let
			 val frontier_offset
			   = (x86.Operand.memloc o x86.MemLoc.complex) 
			     {base = x86MLton.gcState_frontierContents,
			      index = bytes,
			      scale = x86.Scale.One,
			      size = x86MLton.pointerSize,
			      commit = x86.MemLoc.Commit.commit 
			               {isTemp = false,
					onFlush = true},
			      class = x86MLton.Heap}
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
		    | (NONE, NONE)
		    => Error.bug "limitCheck",
	       transfer 
	       = SOME (x86.Transfer.iff {condition = x86.Instruction.BE,
					 truee = skipGC,
					 falsee = doGC})})::
	     (* doGC: *)
	     (x86.Block.T'
	      {label = SOME doGC,
	       profileInfo = x86.ProfileInfo.none,
	       statements = [],
	       transfer = NONE})::
	     (x86MLton.invokeRuntime
	      {label = Label.fromString "GC_gc",
	       args = [(x86.Operand.immediate_label x86MLton.gcState,
			x86MLton.pointerSize),
		       (bytes, x86MLton.wordSize),
		       (x86.Operand.immediate_const_int 0, x86MLton.wordSize),
		       (x86MLton.fileName, x86MLton.pointerSize),
		       (x86MLton.fileLine (), x86MLton.wordSize)],
	       frameSize = frameSize, 
	       return = return,
	       liveInfo = liveInfo}),
	     [(* goto loopGC *)
	      x86.Block.T'
	      {label = NONE,
	       profileInfo = x86.ProfileInfo.none,
	       statements = [],
	       transfer 
	       = SOME (x86.Transfer.goto {target = case loopGC
						     of SOME loopGC => loopGC
						      | NONE => skipGC})},
	      (* skipGC: *)
	      x86.Block.T'
	      {label = SOME skipGC,
	       profileInfo = x86.ProfileInfo.none,
	       statements = [],
	       transfer = NONE}],
	     comment_end]
	  end
    end

  structure Statement =
    struct
      open MachineOutput.Statement

      val toX86Blocks_AllocateArray_limitCheck 
	= fn {numElts,
	      limitCheck = NONE, 
	      liveInfo}
	   => []
	   | {numElts,
	      limitCheck as SOME {gcInfo, bytesPerElt = 0, bytesAllocated},
	      liveInfo as {get,set}}
	   => let
		val arrayLimitCheckLoop = Label.newString "arrayLimitCheckLoop"
		val _ = set(arrayLimitCheckLoop, [])
	      in 
	        (x86.Block.T'
		 {label = NONE,
		  profileInfo = x86.ProfileInfo.none,
		  statements = [],
		  transfer = SOME (x86.Transfer.goto
				   {target = arrayLimitCheckLoop})})::
		(x86.Block.T'
		 {label = SOME arrayLimitCheckLoop,
		  profileInfo = x86.ProfileInfo.none,
		  statements = [],
		  transfer = NONE})::
		(LimitCheck.limitCheck 
		 {info = gcInfo,
		  bytes = x86.Operand.immediate_const_int bytesAllocated,
		  stackCheck = false,
		  loopGC = SOME arrayLimitCheckLoop,
		  liveInfo = liveInfo})
	      end
	   | {numElts as Operand.Int numElts',
	      limitCheck as SOME {gcInfo, bytesPerElt, bytesAllocated},
	      liveInfo as {get,set}}	      
	   => let
		val arrayLimitCheckLoop = Label.newString "arrayLimitCheckLoop"
		val _ = set(arrayLimitCheckLoop, [])
	      in 
		(x86.Block.T'
		 {label = NONE,
		  profileInfo = x86.ProfileInfo.none,
		  statements = [],
		  transfer = SOME (x86.Transfer.goto
				   {target = arrayLimitCheckLoop})})::
		(x86.Block.T'
		 {label = SOME arrayLimitCheckLoop,
		  profileInfo = x86.ProfileInfo.none,
		  statements = [],
		  transfer = NONE})::
		(LimitCheck.limitCheck 
		 {info = gcInfo,
		  bytes = x86.Operand.immediate_const_int 
		          (numElts' * bytesPerElt + bytesAllocated),
		  stackCheck = false,
		  loopGC = SOME arrayLimitCheckLoop,
		  liveInfo = liveInfo})
	      end
	   | {numElts,
	      limitCheck as SOME {gcInfo, bytesPerElt, bytesAllocated},
	      liveInfo as {get,set}}
	   => let
		val numEltsSize = Operand.toX86Size numElts
		val numElts = Operand.toX86Operand numElts
		val _
		  = Assert.assert
		    ("toX86Blocks: AllocateArray, numEltsSize",
		     fn () => numEltsSize = x86MLton.wordSize)

		val arrayAllocateTemp 
		  = x86.Operand.memloc x86MLton.arrayAllocateTempContents

		val arrayLimitCheckLoop = Label.newString "arrayLimitCheckLoop"
		val _ = set(arrayLimitCheckLoop, [])
	      in 
		(x86.Block.T'
		 {label = NONE,
		  profileInfo = x86.ProfileInfo.none,
		  statements = [],
		  transfer = SOME (x86.Transfer.goto
				   {target = arrayLimitCheckLoop})})::
		(x86.Block.T'
		 {label = SOME arrayLimitCheckLoop,
		  profileInfo = x86.ProfileInfo.none,
		  statements
		  = [(* arrayAllocateTemp 
		      *    = numElts * bytesPerElt + bytesAllcoated 
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
		  transfer = NONE})::
		(LimitCheck.limitCheck {info = gcInfo,
					bytes = arrayAllocateTemp,
					stackCheck = false,	
					loopGC = SOME arrayLimitCheckLoop,
					liveInfo = liveInfo})
	      end

      val toX86Blocks_AllocateArray_init
	= fn {dst,numElts,numBytesNonPointers,numPointers,liveInfo}
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

		val frontier 
		  = x86.Operand.memloc x86MLton.gcState_frontierContents
		val frontierDeref
		  = x86.Operand.memloc x86MLton.gcState_frontierDeref
		val frontierOffset
		  = let
		      val memloc 
			= x86.MemLoc.simple 
			  {base = x86MLton.gcState_frontierContents, 
			   index = x86.Immediate.const_int 1,
			   scale = x86MLton.wordScale,
			   size = x86MLton.pointerSize,
			   commit = x86.MemLoc.Commit.commit {isTemp = false,
							      onFlush = true},
			   class = x86MLton.Heap}
		    in
		      x86.Operand.memloc memloc
		    end
		val frontierPlusAHW
		  = (x86.Operand.memloc o x86.MemLoc.simple)
		    {base = x86MLton.gcState_frontierContents, 
		     index = x86.Immediate.const_int arrayHeaderSize,
		     scale = x86.Scale.One,
		     size = x86MLton.pointerSize,
		     commit = x86.MemLoc.Commit.commit {isTemp = false,
							onFlush = true},
		     class = x86MLton.Heap}

		val gcArrayHeaderWord 
		  = (x86.Operand.immediate o x86MLton.gcArrayHeader)
		    {nonPointers = numBytesNonPointers,
		     pointers = numPointers}
	      in
		[x86.Block.T'
		 {label = NONE,
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
		  transfer = NONE}]
	     end

      val toX86Blocks_AllocateArray_arrayNoPointers
	= fn {dst, 
	      numElts as Operand.Int numElts', 
	      numBytesNonPointers,
	      liveInfo as {get,set}}
	   => let
		val frontier 
		  = x86.Operand.memloc x86MLton.gcState_frontierContents
	      in
		if numBytesNonPointers = 0 orelse numElts' = 0
		  then [x86.Block.T'
			{label = NONE,
			 profileInfo = x86.ProfileInfo.none,
			 statements
			 = [(* frontier += pointerSize *)
			    x86.Assembly.instruction_binal
			    {oper = x86.Instruction.ADD,
			     dst = frontier,
			     src = x86.Operand.immediate_const_int 
			           pointerSize,
			     size = x86MLton.pointerSize}],
			  transfer = NONE}]
		  else [x86.Block.T'
			{label = NONE,
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
			 transfer = NONE}]
	      end
	   | {dst, 
	      numElts, 
	      numBytesNonPointers,
	      liveInfo as {get,set}}
	   => let
		val liveIn 
		  = case x86.Operand.deMemloc (Operand.toX86Operand dst)
			       of SOME memloc => if x86.MemLoc.isTemp memloc
						   then [memloc]
						   else []
				| NONE => []
		  
		val numEltsSize = Operand.toX86Size numElts
		val numElts = Operand.toX86Operand numElts
		val _ 
		  = Assert.assert
		    ("toX86Blocks: AllocateArray, numEltsSize",
		     fn () => numEltsSize = x86MLton.wordSize)

		val arrayAllocateTemp 
		  = x86.Operand.memloc x86MLton.arrayAllocateTempContents

		val frontier 
		  = x86.Operand.memloc x86MLton.gcState_frontierContents
       	      in
		if numBytesNonPointers = 0
		  then [x86.Block.T'
			{label = NONE,
			 profileInfo = x86.ProfileInfo.none,
			 statements 
			 = [(* frontier += pointerSize *)
			    x86.Assembly.instruction_binal
			    {oper = x86.Instruction.ADD,
			     dst = frontier,
			     src = x86.Operand.immediate_const_int
			           pointerSize,
			     size = x86MLton.pointerSize}],
			 transfer = NONE}]
		  else let
			 val arrayNoPointersZ 
			   = Label.newString "arrayNoPointersZ"
			 val _ = set(arrayNoPointersZ, liveIn)
			 val arrayNoPointersNZ
			   = Label.newString "arrayNoPointersNZ"
			 val _ = set(arrayNoPointersNZ, liveIn)
			 val arrayNoPointersZJoin
			   = Label.newString "arrayNoPointersZJoin"
			 val _ = set(arrayNoPointersZJoin, liveIn) 
		       in
			 [(* if (numElts == 0) goto arrayNoPointersZ *)
			  x86.Block.T'
			  {label = NONE,
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
			  {label = SOME arrayNoPointersNZ,
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
			      x86.Assembly.instruction_unal
			      {oper = x86.Instruction.DEC,
			       dst = arrayAllocateTemp,
			       size = x86MLton.wordSize},
			      x86.Assembly.instruction_binal
			      {oper = x86.Instruction.AND,
			       dst = arrayAllocateTemp,
			       src = x86.Operand.immediate_const_word 
			             0wxFFFFFFFc,
			       size = x86MLton.wordSize},
			      x86.Assembly.instruction_binal
			      {oper = x86.Instruction.ADD,
			       dst = arrayAllocateTemp,
			       src = x86.Operand.immediate_const_int 4,
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
			  {label = SOME arrayNoPointersZ,
			   profileInfo = x86.ProfileInfo.none,
			   statements
			   = [(* frontier += pointerSize *)
			      x86.Assembly.instruction_binal
			      {oper = x86.Instruction.ADD,
			       dst = frontier,
			       src = x86.Operand.immediate_const_int
			             pointerSize,
			       size = x86MLton.pointerSize}],
			   (* goto arrayNoPointersZJoin *)
			   transfer 
			   = SOME (x86.Transfer.goto
				   {target = arrayNoPointersZJoin})},
			  (* arrayNoPointersZJoin: *)
			  x86.Block.T'
			  {label = SOME arrayNoPointersZJoin,
			   profileInfo = x86.ProfileInfo.none,
			   statements = [],
			   transfer = NONE}]
		       end
	      end

      val toX86Blocks_AllocateArray_arrayPointers_loop
	= fn {dst,
	      liveInfo as {get,set}}
	   => let
		val dstsize = Operand.toX86Size dst
		val dst = Operand.toX86Operand dst
		val _ 
		  = Assert.assert
		    ("toX86Blocks: AllocateArray, dstsize",
		     fn () => dstsize = x86MLton.pointerSize)

		val liveIn 
		  = case x86.Operand.deMemloc dst
			       of SOME memloc => if x86.MemLoc.isTemp memloc
						   then [memloc]
						   else []
				| NONE => []

		val arrayAllocateLoopTemp 
		  = x86.Operand.memloc x86MLton.arrayAllocateLoopTempContents
		val arrayAllocateLoopTempDeref
		  = x86.Operand.memloc x86MLton.arrayAllocateLoopTempDeref

		val frontier 
		  = x86.Operand.memloc x86MLton.gcState_frontierContents

		val arrayPointersZ
		  = Label.newString "arrayPointersZ"
		val _ = set(arrayPointersZ, liveIn)
		val arrayPointersNZ
		  = Label.newString "arrayPointersNZ"
		val _ = set(arrayPointersNZ, liveIn)
		val arrayPointersZJoin
		  = Label.newString "arrayPointersZJoin"
		val _ = set(arrayPointersZJoin, liveIn)
		val arrayPointersLoop
		  = Label.newString "arrayPointersLoop"
		val _ = set(arrayPointersLoop, 
			    x86MLton.arrayAllocateLoopTempContents::liveIn)
		val arrayPointersLoopJoin
		  = Label.newString "arrayPointersLoopJoin"
		val _ = set(arrayPointersLoopJoin, liveIn)
	      in
		[(* for (arrayAllocateLoopTemp = dst; 
		  *      arrayAllocateLoopTemp < frontier; 
		  *      arrayAllocateLoopTemp++)
		  *    *arrayAllocateLoopTemp = 0x1 
		  *)
		 x86.Block.T'
		 {label = NONE,
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
		 {label = NONE,
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
		 {label = SOME arrayPointersLoop,
		  profileInfo = x86.ProfileInfo.none,
		  statements
		  = [x86.Assembly.instruction_mov
		     {dst = arrayAllocateLoopTempDeref,
		      src = x86.Operand.immediate_const_word 0wx1,
		      size = x86MLton.pointerSize},
		     x86.Assembly.instruction_binal
		     {oper = x86.Instruction.ADD,
		      dst = arrayAllocateLoopTemp,
		      src = x86.Operand.immediate_const_int 
		            pointerSize,
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
		 {label = SOME arrayPointersLoopJoin,
		  profileInfo = x86.ProfileInfo.none,
		  statements = [],
		  transfer = NONE}]
	      end

      val toX86Blocks_AllocateArray_arrayPointers
	= fn {dst, 
	      numElts as Operand.Int numElts', 
	      numPointers,
	      liveInfo as {get,set}}
	   => let
		val frontier 
		  = x86.Operand.memloc x86MLton.gcState_frontierContents
	      in
		(x86.Block.T'
		 {label = NONE,
		  profileInfo = x86.ProfileInfo.none,
		  statements 
		  = [if numElts' = 0
		       then (* frontier += pointerSize *)
			    x86.Assembly.instruction_binal
			    {oper = x86.Instruction.ADD,
			     dst = frontier,
			     src = x86.Operand.immediate_const_int
			           pointerSize,
			     size = x86MLton.pointerSize}
		       else (* frontier 
			     *    += numElts * numPointers * pointerSize 
			     *)
			    x86.Assembly.instruction_binal
			    {oper = x86.Instruction.ADD,
			     dst = frontier,
			     src = x86.Operand.immediate_const_int
			           (numElts' * numPointers * pointerSize),
			     size = x86MLton.pointerSize}],
		  transfer = NONE})::
		(toX86Blocks_AllocateArray_arrayPointers_loop 
		 {dst = dst,
		  liveInfo = liveInfo})
	      end
	   | {dst, 
	      numElts,
	      numPointers,
	      liveInfo as {get,set}} 
	   => let
		val liveIn 
		  = case x86.Operand.deMemloc (Operand.toX86Operand dst)
			       of SOME memloc => if x86.MemLoc.isTemp memloc
						   then [memloc]
						   else []
				| NONE => []
		  
		val numEltsSize = Operand.toX86Size numElts
		val numElts = Operand.toX86Operand numElts
		val _ 
		  = Assert.assert
		    ("toX86Blocks: AllocateArray, numEltsSize",
		     fn () => numEltsSize = x86MLton.wordSize)

		val arrayAllocateTemp 
		  = x86.Operand.memloc x86MLton.arrayAllocateTempContents

		val frontier 
		  = x86.Operand.memloc x86MLton.gcState_frontierContents

		val arrayPointersZ
		  = Label.newString "arrayPointersZ"
		val _ = set(arrayPointersZ, liveIn)
		val arrayPointersNZ
		  = Label.newString "arrayPointersNZ"
		val _ = set(arrayPointersNZ, liveIn)
		val arrayPointersZJoin
		  = Label.newString "arrayPointersZJoin"
		val _ = set(arrayPointersZJoin, liveIn)
		val arrayPointersLoop
		  = Label.newString "arrayPointersLoop"
		val _ = set(arrayPointersLoop, liveIn)
		val arrayPointersLoopJoin
		  = Label.newString "arrayPointersLoopJoin"
		val _ = set(arrayPointersLoopJoin, liveIn)		  
	      in
		((* if (numElts == 0) goto arrayPointersZ *)
		 x86.Block.T'
		 {label = NONE,
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
			   falsee = arrayPointersNZ})})::
		((* arrayPointersNZ: *)
		 x86.Block.T'
		 {label = SOME arrayPointersNZ,
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
		            (numPointers * pointerSize),
		      size = x86MLton.wordSize},
		     x86.Assembly.instruction_binal
		     {oper = x86.Instruction.ADD,
		      dst = frontier,
		      src = arrayAllocateTemp,
		      size = x86MLton.pointerSize}],
		  (* goto arrayPointersZJoin *)
		  transfer 
		  = SOME (x86.Transfer.goto
			  {target = arrayPointersZJoin})})::
		((* arrayPointersZ: *)
		 x86.Block.T'
		 {label = SOME arrayPointersZ,
		  profileInfo = x86.ProfileInfo.none,
		  statements 
		  = [(* frontier += pointerSize *)
		     x86.Assembly.instruction_binal
		     {oper = x86.Instruction.ADD,
		      dst = frontier,
		      src = x86.Operand.immediate_const_int
		      pointerSize,
		      size = x86MLton.pointerSize}],
		  (* goto arrayPointersZJoin *)
		  transfer 
		  = SOME (x86.Transfer.goto
			  {target = arrayPointersZJoin})})::
		((* arrayPointersZJoin: *)
		 x86.Block.T'
		 {label = SOME arrayPointersZJoin,
		  profileInfo = x86.ProfileInfo.none,
		  statements = [],
		  transfer = NONE})::
		(toX86Blocks_AllocateArray_arrayPointers_loop 
		 {dst = dst,
		  liveInfo = liveInfo})
	      end

      fun comments (statement, extra)
	= if !Control.Native.commented > 0
	    then let
		   val comment = extra ()
		 in
		   ([x86.Block.T'
		     {label = NONE,
		      profileInfo = x86.ProfileInfo.none,
		      statements = [x86.Assembly.comment
				    (concat ["begin Statement.",
					     statement,
					     ": ",
					     comment])],
		      transfer = NONE}],
		    [x86.Block.T'
		     {label = NONE,
		      profileInfo = x86.ProfileInfo.none,
		      statements = [x86.Assembly.comment
				    (concat ["end Statement.",
					     statement,
					     ": ",
					     comment])],
		      transfer = NONE}])
		 end
	    else ([],[])

      fun toX86Blocks {statement,
		       liveInfo as {get,set}}
	= case statement
	    of Noop
	     => []
	     | Move {src, dst}
	     => let
		  val (comment_begin,
		       comment_end)
		    = comments 
		      ("Move",
		       fn () => concat[Operand.toString dst,
				       " = ",
				       Operand.toString src])

		  val dstsize = Operand.toX86Size dst
		  val dst = Operand.toX86Operand dst
		    
		  val srcsize = Operand.toX86Size src
		  val src = Operand.toX86Operand src 
		    
		  val _ 
		    = Assert.assert
		      ("toX86Blocks: Move",
		       fn () => srcsize = dstsize)
		in
		  List.concat
		  [comment_begin,
		   [x86.Block.T'
		    {label = NONE,
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
		     transfer = NONE}],
		   comment_end]
		end 
	     | Push bytes
	     => let
		  val (comment_begin,
		       comment_end)
		    = comments 
		      ("Push", 
		       fn () => Int.toString bytes)

		  val stackTop 
		    = x86.Operand.memloc x86MLton.gcState_stackTopContents
		  val bytes = x86.Operand.immediate_const_int bytes
		in 
		  List.concat
		  [comment_begin,
		   [x86.Block.T'
		    {label = NONE,
		     profileInfo = x86.ProfileInfo.none,
		     statements
		     = [(* stackTop += bytes *)
			x86.Assembly.instruction_binal 
			{oper = x86.Instruction.ADD,
			 dst = stackTop,
			 src = bytes, 
			 size = x86MLton.pointerSize}],
		     transfer = NONE}],
		   comment_end]
		end
	     | Assign {dst, oper, args, info, pinfo}
   	     => let
		  val (comment_begin,
		       comment_end)
		    = comments 
		      ("Assign",
		       fn () => concat
		                [case dst
				   of NONE => ""
				    | SOME dst 
				    => (Operand.toString dst) ^ " = ",
				 Prim.toString oper,
				 argsToString 
				 (List.map(args, Operand.toString))])

		  val args 
		    = List.map(args,
			       fn arg 
			        => (Operand.toX86Operand arg,
				    x86MLton.toX86Size (Operand.ty arg)))
		    
		  val info 
		    = Option.map(info, fn MachineOutput.GCInfo.T i => i)
		  val dst 
		    = Option.map(dst, 
				 fn dst 
				  => (Operand.toX86Operand dst, 
				      x86MLton.toX86Size (Operand.ty dst)))
		in
		  List.concat
		  [comment_begin,
		   (x86MLton.applyPrim {oper = oper,
					args = args,
					dst = dst,
					info = info,
					pinfo = pinfo,
					liveInfo = liveInfo}),
		   comment_end]
		end
	     | LimitCheck {info, bytes, stackCheck}
             => let
		  val (comment_begin,
		       comment_end)
		    = comments 
		      ("LimitCheck",
		       fn () => "")

		  val statementLimitCheckLoop
		    = Label.newString "statementLimitCheckLoop"
		  val _ = set(statementLimitCheckLoop, [])
		in 
		  List.concat
		  [comment_begin,
		   (x86.Block.T'
		    {label = NONE,
		     profileInfo = x86.ProfileInfo.none,
		     statements = [],
		     transfer 
		     = SOME (x86.Transfer.goto 
			     {target = statementLimitCheckLoop})})::
		   (x86.Block.T'
		    {label = SOME statementLimitCheckLoop,
		     profileInfo = x86.ProfileInfo.none,
		     statements = [],
		     transfer = NONE})::
		   (LimitCheck.limitCheck
		    {info = info,
		     bytes = x86.Operand.immediate_const_int bytes,
		     stackCheck = stackCheck,
		     loopGC = SOME statementLimitCheckLoop,
		     liveInfo = liveInfo}),
		   comment_end]
		end
	     | SaveExnStack {offset}
	     => let
		  val (comment_begin,
		       comment_end)
		    = comments ("SaveExnStack",
				fn () => Int.toString offset)

		  val exnStack 
		    = x86.Operand.memloc 
		      x86MLton.gcState_currentThread_exnStackContents
		  val stackTop 
		    = x86.Operand.memloc x86MLton.gcState_stackTopContents
		  val stackBottom 
		    = x86.Operand.memloc x86MLton.gcState_stackBottomContents
		    
		  val tempP 
		    = let
			val index 
			  = x86.Immediate.const_int (offset + wordSize)
			val memloc 
			  = x86.MemLoc.simple 
			    {base = x86MLton.gcState_stackTopContents, 
			     index = index,
			     scale = x86.Scale.One,
			     size = x86MLton.pointerSize,
			     commit = x86.MemLoc.Commit.commit 
			              {isTemp = false,
				       onFlush = true},
			     class = x86MLton.Stack}
		      in
			x86.Operand.memloc memloc
		      end
		in
		  List.concat
		  [comment_begin,
		   [x86.Block.T'
		    {label = NONE,
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
		     transfer = NONE}],
		   comment_end]
		end
	     | RestoreExnStack {offset}
	     => let
		  val (comment_begin,
		       comment_end)
		    = comments ("RestoreExnStack",
				fn () => Int.toString offset)
		    
		  val exnStack 
		    = x86.Operand.memloc 
		      x86MLton.gcState_currentThread_exnStackContents
		      
		  val tempP 
		    = let
			val index 
			  = x86.Immediate.const_int (offset + wordSize)
			val memloc 
			  = x86.MemLoc.simple 
			    {base = x86MLton.gcState_stackTopContents, 
			     index = index,
			     scale = x86.Scale.One,
			     size = x86MLton.pointerSize,
			     commit = x86.MemLoc.Commit.commit  
			              {isTemp = false,
				       onFlush = true},
			     class = x86MLton.Stack}
		      in
			x86.Operand.memloc memloc
		      end
		in
		  List.concat
		  [comment_begin,
		   [x86.Block.T'
		    {label = NONE,
		     profileInfo = x86.ProfileInfo.none,
		     statements 
		     = [(* exnStack = *(stackTop + offset + wordSize) *)
			x86.Assembly.instruction_mov 
			{dst = exnStack,
			 src = tempP,
			 size = x86MLton.pointerSize}],
		     transfer = NONE}],
		   comment_end]
		end
	     | Allocate {dst, size, stores, numPointers, numWordsNonPointers}
	     => let
		  val (comment_begin,
		       comment_end)
		    = comments 
		      ("Allocate",
		       fn () => concat[Operand.toString dst,
				       " = ", 
				       argsToString
				       (List.map(stores,
						 fn {offset,value}
						  => concat
						     ["[",
						      Int.toString offset, 
						      " <- ", 
						      Operand.toString value,
						      "]"]))])

		  val dstsize = Operand.toX86Size dst
		  val dst = Operand.toX86Operand dst
		  val _ 
		    = Assert.assert
		      ("toX86Assembly: Allocate, dstsize",
		       fn () => dstsize = x86MLton.pointerSize)

		  val frontier 
		    = x86.Operand.memloc x86MLton.gcState_frontierContents
		  val frontierDeref
		    = x86.Operand.memloc x86MLton.gcState_frontierDeref
		  val frontierPlusOHW
		    = (x86.Operand.memloc o x86.MemLoc.simple)
		      {base = x86MLton.gcState_frontierContents, 
		       index = x86.Immediate.const_int objectHeaderSize,
		       scale = x86.Scale.One,
		       size = x86MLton.pointerSize,
		       commit = x86.MemLoc.Commit.commit {isTemp = false,
							  onFlush = true},
		       class = x86MLton.Heap}

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
			      val index 
				= x86.Immediate.const_int 
			          (objectHeaderSize + offset)
			      val memloc 
				= x86.MemLoc.simple 
			          {base = x86MLton.gcState_frontierContents, 
				   index = index,
				   scale = x86.Scale.One,
				   size = size,
				   commit = x86.MemLoc.Commit.commit
				            {isTemp = false,
					     onFlush = true},
				   class = x86MLton.Heap}
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
		  List.concat
		  [comment_begin,
		   [x86.Block.T'
		    {label = NONE,
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
				           (objectHeaderSize + size),
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
		     transfer = NONE}],
		   comment_end]
		end
	     | AllocateArray {dst,
			      numElts,
			      numPointers,
			      numBytesNonPointers,
			      limitCheck}
	     => let
		  val (comment_begin,
		       comment_end)
		    = comments
		      ("AllocateArray",
		       fn () => concat[Operand.toString dst,
				       " = X[",
				       Operand.toString numElts,
				       "]"])
		in
		  List.concat
		  [comment_begin,
		   (toX86Blocks_AllocateArray_limitCheck
		    {numElts = numElts,
		     limitCheck = limitCheck,
		     liveInfo = liveInfo}),
		   (toX86Blocks_AllocateArray_init
		    {dst = dst,
		     numElts = numElts,
		     numBytesNonPointers = numBytesNonPointers,
		     numPointers = numPointers,
		     liveInfo = liveInfo}),
		   (if numPointers = 0
		      then toX86Blocks_AllocateArray_arrayNoPointers
			   {dst = dst,
			    numElts = numElts,
			    numBytesNonPointers = numBytesNonPointers,
			    liveInfo = liveInfo}
		    else if numBytesNonPointers = 0
		      then toX86Blocks_AllocateArray_arrayPointers
		           {dst = dst,
			    numElts = numElts,
			    numPointers = numPointers,
			    liveInfo = liveInfo}
		    else Error.bug "toX86Blocks: AllocateArray"),
		   comment_end]
		end
    end

  structure Transfer =
    struct
      open MachineOutput.Transfer

      fun goto l
	= [x86.Block.T'
	   {label = NONE,
	    profileInfo = x86.ProfileInfo.none,
	    statements = [],
	    transfer = SOME (x86.Transfer.goto
			     {target = l})}]
 
      fun iff (test, a, b)
	= let
	    val size = Operand.toX86Size test
	    val test = Operand.toX86Operand test
	  in
	    if Label.equals(a, b)
	      then ([x86.Block.T'
		     {label = NONE,
		      profileInfo = x86.ProfileInfo.none,
		      statements = [],
		      transfer = SOME (x86.Transfer.goto {target = a})}])
	      else [(* if (test) goto a
		     * goto b
		     *)
		    x86.Block.T'
		    {label = NONE,
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
			      falsee = b})}]
	  end

      fun cmp (test, k, a, b)
	= let
	    val size = Operand.toX86Size test
	    val test = Operand.toX86Operand test
	  in
	    if Label.equals(a, b)
	      then ([x86.Block.T'
		     {label = NONE,
		      profileInfo = x86.ProfileInfo.none,
		      statements = [],
		      transfer = SOME (x86.Transfer.goto {target = a})}])
	      else [(* if (test = k) goto a
		     * goto b
		     *)
		    x86.Block.T'
		    {label = NONE,
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
			      falsee = b})}]
	  end

      fun switch(test, cases, default)
	= let
	    val test = Operand.toX86Operand test
	  in
	    [x86.Block.T'
	     {label = NONE,
	      profileInfo = x86.ProfileInfo.none,
	      statements = [],
	      transfer = SOME (x86.Transfer.switch
			       {test = test,
				cases = cases,
				default = default})}]
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

      fun comments (statement, extra)
	= if !Control.Native.commented > 0
	    then let
		   val comment = extra ()
		 in
		   [x86.Block.T'
		    {label = NONE,
		     profileInfo = x86.ProfileInfo.none,
		     statements = [x86.Assembly.comment
				   (concat ["Transfer.",
					    statement,
					    ": ",
					    comment])],
		      transfer = NONE}]
		 end
	    else []

      val toX86Blocks
	= fn Bug 
	   => List.concat
	      [(comments ("Bug", fn () => "")),
	       [x86.Block.T'
		{label = NONE,
		 profileInfo = x86.ProfileInfo.none,
		 statements = [],
		 transfer = NONE}],
	       x86MLton.bug]
	   | Return
	   => List.concat
	      [(comments ("Return", fn () => "")),
	       [x86.Block.T'
		{label = NONE,
		 profileInfo = x86.ProfileInfo.none,
		 statements = [],
		 (* jump *(gcState_stackTop) *)
		 transfer
		 = SOME (x86.Transfer.assembly
			 [x86.Assembly.instruction_jmp
			  {target = x86.Operand.memloc 
			            x86MLton.gcState_stackTopDeref,
			   absolute = true}])}]]
	   | Raise
	   => let
		val exnStack 
		  = x86.Operand.memloc
		    x86MLton.gcState_currentThread_exnStackContents
		val stackTop 
		  = x86.Operand.memloc x86MLton.gcState_stackTopContents
		val stackBottom 
		  = x86.Operand.memloc x86MLton.gcState_stackBottomContents
	      in
		List.concat
		[(comments ("Raise", fn () => "")),
		 [x86.Block.T'
		  {label = NONE,
		   profileInfo = x86.ProfileInfo.none,
		   statements
		   = [(* stackTop = stackBottom + exnStack *)
		      x86.Assembly.instruction_mov
		      {dst = stackTop,
		       src = stackBottom,
		       size = x86MLton.pointerSize},
		      x86.Assembly.instruction_binal
		      {oper = x86.Instruction.ADD,
		       dst = stackTop,
		       src = exnStack,
		       size = x86MLton.pointerSize}],
		   (* goto *(stackTop) *) 
		   transfer 
		   = SOME (x86.Transfer.assembly
			   [x86.Assembly.instruction_jmp
			    {target = x86.Operand.memloc 
			              x86MLton.gcState_stackTopDeref,
			    absolute = true}])}]]
	      end
	   | Switch {test, cases, default}
	   => List.concat
	      [(comments ("Switch", fn () => "")) @
	       (case cases 
		  of MachineOutput.Cases.Char cases 
		   => doSwitchChar (test,cases,default)
		   | MachineOutput.Cases.Int cases 
		   => doSwitchInt (test,cases,default)
	           | MachineOutput.Cases.Word cases 
	           => doSwitchWord (test,cases,default))]
	   | SwitchIP {test, int, pointer}
	   => let
		val size = Operand.toX86Size test
		val test = Operand.toX86Operand test
	      in
		List.concat
		[(comments ("SwitchIP", fn () => "")),
		 [(* if (test & 0x3) goto int 
		   * goto pointer
		   *)
		  x86.Block.T'
		  {label = NONE,
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
			    falsee = pointer})}]]
	      end
	   | NearJump {label}
	   => List.concat
	      [(comments ("NearJump", fn () => "")),
	       [(* goto label *)
		x86.Block.T'
		{label = NONE,
		 profileInfo = x86.ProfileInfo.none,
		 statements = [],
		 transfer
		 = SOME (x86.Transfer.goto {target = label})}]]
	   | FarJump {label,...}
	   => List.concat
	      [(comments ("FarJump", fn () => "")),
	       [x86.Block.T'
		{label = NONE,
		 profileInfo = x86.ProfileInfo.none,
		 statements = [],
		 transfer 
		 = SOME (x86.Transfer.goto {target = label})}]]
    end

  structure Block =
    struct
      open MachineOutput.Block

      fun toX86Blocks {block as T {label, 
				   live,
				   profileName,
				   statements, 
				   transfer},
		       liveInfo as {get,set}}
	= let
	    val live = List.removeDuplicates(live, Local.eq)
	    val _ = set(label, List.map(live, Local.toX86MemLoc))

	    val pseudo_blocks
	      = (x86.Block.T'
		 {label = SOME label,
		  profileInfo = x86.ProfileInfo.none,
		  statements 
		  = if !Control.Native.commented > 0
		      then let
			     val comment
			       = "Live: " ^
			         (argsToString
				  (List.map(live, fn l => Local.toString l)))
			   in
			     [x86.Assembly.comment comment]
			   end
		      else [],
		  transfer = NONE})::
	        (Array.foldr(statements,
			     (Transfer.toX86Blocks transfer),
			     fn (statement,l)
			      => List.concat
			         [(Statement.toX86Blocks 
				   {statement = statement,
				    liveInfo = liveInfo}),
				  l]))
		 
	    val blocks
	      = x86.Block.compress pseudo_blocks

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
			fn (x86.Block.T {label, profileInfo, 
					 statements, transfer})
			 => let
			      val profileInfo
				= x86.ProfileInfo.add
				  (addProfileInfo profileInfo,
				   {profileLevel = 2,
				    profileName = Label.toString label})
			    in
			      x86.Block.T {label = label,
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

      fun toX86Chunk {chunk as T {blocks, entries, gcReturns, ...}, 
		      liveInfo}
	= let
	    val exports = List.rev(List.removeDuplicates(List.concat [entries, gcReturns],
							 Label.equals))
	    val blocks 
	      = List.concatMap(blocks, 
				fn block
				 => Block.toX86Blocks {block = block,
						       liveInfo = liveInfo})
	  in
	    x86.Chunk.T {exports = exports,
			 blocks = blocks}
			 
	  end
    end

  structure Program =
    struct
      open MachineOutput.Program

      fun toX86Chunks {program as T {chunks,...},
		       liveInfo} 
	= let
	    val chunks
	      = List.map(chunks,
			 fn chunk
			  => Chunk.toX86Chunk {chunk = chunk,
					       liveInfo = liveInfo})
	  in 
	    chunks
	  end
    end

  fun translateChunk {chunk: x86MLton.MachineOutput.Chunk.t} :
                     {chunk: x86.Chunk.t,
		      liveInfo: {get: x86.Label.t 
			              -> x86.MemLoc.t list,
				 set: x86.Label.t * x86.MemLoc.t list
				      -> unit}}
    = let
	val liveInfo as {get : x86.Label.t -> x86.MemLoc.t list,
			 set}
	  = Property.getSet(Label.plist, Property.initConst [])
      in
	{chunk = Chunk.toX86Chunk {chunk = chunk,
				   liveInfo = liveInfo},
	 liveInfo = liveInfo}
      end

  val (translateChunk, translateChunk_msg)
    = tracer
      "translateChunk"
      translateChunk

  fun translateChunk_totals ()
    = (translateChunk_msg ();
       Control.indent ();
       Control.unindent ())


  fun translateProgram {program: x86MLton.MachineOutput.Program.t} :
                       {chunks: x86.Chunk.t list,
			liveInfo: {get: x86.Label.t 
			                -> x86.MemLoc.t list,
				   set: x86.Label.t * x86.MemLoc.t list
				        -> unit}}
    = let
	val liveInfo as {get : x86.Label.t -> x86.MemLoc.t list,
			 set}
	  = Property.getSet(Label.plist, Property.initConst [])
      in
	{chunks = Program.toX86Chunks {program = program,
				       liveInfo = liveInfo},
	 liveInfo = liveInfo}
      end

  val (translateProgram, translateProgram_msg)
    = tracer
      "translateProgram"
      translateProgram

  fun translateProgram_totals ()
    = (translateProgram_msg ();
       Control.indent ();
       Control.unindent ())
end
