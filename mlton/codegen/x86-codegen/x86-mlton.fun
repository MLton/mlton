(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor x86MLton(S: X86_MLTON_STRUCTS): X86_MLTON =
struct

  open S
  open x86MLtonBasic
  open x86
  local
     open Machine
  in
     structure CFunction = CFunction
     structure Prim = Prim
     structure Runtime = Runtime
  end

  type transInfo = {addData : x86.Assembly.t list -> unit,
		    frameLayouts: x86.Label.t ->
		                  {size: int,
				   frameLayoutsIndex: int} option,
		    live: x86.Label.t -> x86.Operand.t list,
		    liveInfo: x86Liveness.LiveInfo.t}

  fun prim {prim : Prim.t,
	    args : (Operand.t * Size.t) vector,
	    dst : (Operand.t * Size.t) option,
	    transInfo as {addData, frameLayouts, live, liveInfo} : transInfo}
    = let
	val primName = Prim.toString prim
	datatype z = datatype Prim.Name.t

	fun getDst ()
	  = case dst
	      of SOME dst => dst
	       | NONE => Error.bug "applyPrim: getDst"
	fun getSrc1 ()
	  = Vector.sub (args, 0)
	    handle _ => Error.bug "applyPrim: getSrc1"
	fun getSrc2 ()
	  = (Vector.sub (args, 0), Vector.sub (args, 1))
	    handle _ => Error.bug "applyPrim: getSrc2"
	fun getSrc3 ()
	  = (Vector.sub (args, 0), Vector.sub (args, 1), Vector.sub (args, 2))
	    handle _ => Error.bug "applyPrim: getSrc3"

	fun unimplemented s
	  = AppendList.fromList
	    [Block.T'
	     {entry = NONE,
	      profileInfo = ProfileInfo.none,
	      statements = [Assembly.comment ("UNIMPLEMENTED PRIM: " ^ s)],
	      transfer = NONE}]
		
	fun lengthArrayVectorString ()
	  = let
	      val (dst,dstsize) = getDst ();
	      val _ 
		= Assert.assert
		  ("applyPrim: lengthArrayVectorString, dstsize",
		   fn () => dstsize = wordSize)
	      val (src,srcsize) = getSrc1 ();
	      val _ 
		= Assert.assert
		  ("applyPrim: lengthArrayVectorString, srcsize",
		   fn () => srcsize = wordSize)

	      val memloc
		= case (Operand.deMemloc src)
		    of SOME base
		     => MemLoc.simple 
		        {base = base,
			 index = Immediate.const_int ~2,
			 scale = wordScale,
			 size = wordSize,
			 class = Classes.Heap}
		     | NONE => Error.bug 
                               "applyPrim: lengthArrayVectorString, src"
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements 
		= [Assembly.instruction_mov
		   {dst = dst,
		    src = Operand.memloc memloc,
		    size = wordSize}],
		transfer = NONE}]
	    end

	fun subWord8ArrayVector ()
	  = let
	      val (dst,dstsize) = getDst ()
	      val _ 
		= Assert.assert
		  ("applyPrim: subWord8ArrayVector, dstsize", 
		   fn () => dstsize = Size.LONG)
	      val ((src1,src1size),
		   (src2,src2size)) = getSrc2 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: subWord8ArrayVector, src1size",
		   fn () => src1size = pointerSize)
	      val _ 
		= Assert.assert
		  ("applyPrim: subWord8ArrayVector, src2size",
		   fn () => src2size = pointerSize)

	      val base 
		= case (Operand.deMemloc src1)
		    of SOME base => base
		     | NONE => Error.bug "applyPrim: subWord8ArrayVector, src1"
	      val memloc
		= case (Operand.deImmediate src2,
			Operand.deMemloc src2)
		    of (SOME index, _)
		     => MemLoc.simple 
		        {base = base,
			 index = index,
			 scale = Scale.Four,
			 size = Size.LONG,
			 class = Classes.Heap}
		     | (_, SOME index)
		     => MemLoc.complex 
		        {base = base,
			 index = index,
			 scale = Scale.Four,
			 size = Size.LONG,
			 class = Classes.Heap}
		     | _ => Error.bug "applyPrim: subWord8ArrayVector, src2"
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements 
		= [Assembly.instruction_mov
		   {dst = dst,
		    src = Operand.memloc memloc,
		    size = dstsize}],
		transfer = NONE}]
	    end	  

	fun updateWord8Array ()
	  = let
	      val ((src1,src1size),
		   (src2,src2size),
		   (src3,src3size)) = getSrc3 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: updateWord8Array, src1size", 
		   fn () => src1size = pointerSize)
	      val _ 
		= Assert.assert
		  ("applyPrim: updateWord8Array, src2size", 
		   fn () => src2size = wordSize)
	      val _ 
		= Assert.assert
		  ("applyPrim: updateWord8Array, src3size", 
		   fn () => src3size = wordSize)

	      val base 
		= case (Operand.deMemloc src1)
		    of SOME base => base
		     | NONE => Error.bug "applyPrim: updateWord8Array, src1"
	      val memloc
		= case (Operand.deImmediate src2,
			Operand.deMemloc src2)
		    of (SOME index, _)
		     => MemLoc.simple 
		        {base = base,
			 index = index,
			 scale = Scale.Four,
			 size = Size.LONG,
			 class = Classes.Heap}
		     | (_, SOME index)
		     => MemLoc.complex 
			{base = base,
			 index = index,
			 scale = Scale.Four,
			 size = Size.LONG,
			 class = Classes.Heap}
		     | _ => Error.bug "applyPrim: updateWord8Array, src2"
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements 
		= [Assembly.instruction_mov
		   {dst = Operand.memloc memloc,
		    src = src3,
		    size = src3size}],
		transfer = NONE}]
	    end	  

	fun mov ()
	  = let
	      val (dst,dstsize) = getDst ()
	      val (src,srcsize) = getSrc1 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: mov, dstsize/srcsize",
		   fn () => srcsize = dstsize)
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements
		= [Assembly.instruction_mov
		   {dst = dst,
		    src = src,
		    size = srcsize}],
		transfer = NONE}]
	    end
	  
	fun movx oper
	  = let
	      val (dst,dstsize) = getDst ()
	      val (src,srcsize) = getSrc1 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: movx, dstsize/srcsize",
		   fn () => Size.lt(srcsize,dstsize))
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements
		= [Assembly.instruction_movx
		   {oper = oper,
		    dst = dst,
		    src = src,
		    dstsize = dstsize,
		    srcsize = srcsize}],
		transfer = NONE}]
	    end

	fun xvom ()
	  = let
	      val (dst,dstsize) = getDst ()
	      val (src,srcsize) = getSrc1 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: xvom, dstsize/srcsize",
		   fn () => Size.lt(dstsize,srcsize))
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements
		= [Assembly.instruction_xvom
		   {dst = dst,
		    src = src,
		    dstsize = dstsize,
		    srcsize = srcsize}],
		transfer = NONE}]
	    end

	fun binal oper
	  = let
	      val ((src1,src1size),
		   (src2,src2size)) = getSrc2 ()
	      val (dst,dstsize) = getDst ()
	      val _ 
		= Assert.assert
		  ("applyPrim: binal, dstsize/src1size/src2size",
		   fn () => src1size = dstsize andalso
		            src2size = dstsize)

	      (* Reverse src1/src2 when src1 and src2 are temporaries
	       * and the oper is commutative. 
	       *)
	      val (src1,src2)
		= if (oper = Instruction.ADD)
		     orelse
		     (oper = Instruction.ADC)
		     orelse
		     (oper = Instruction.AND)
		     orelse
		     (oper = Instruction.OR)
		     orelse
		     (oper = Instruction.XOR)
		    then case (Operand.deMemloc src1, Operand.deMemloc src2)
			   of (SOME memloc_src1, SOME memloc_src2)
			    => if x86Liveness.track memloc_src1
			          andalso
				  x86Liveness.track memloc_src2
				 then (src2,src1)
				 else (src1,src2)
			    | _ => (src1,src2)
		    else (src1,src2)
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements
		= [Assembly.instruction_mov
		   {dst = dst,
		    src = src1,
		    size = src1size},
		   Assembly.instruction_binal
		   {oper = oper,
		    dst = dst,
		    src = src2,
		    size = dstsize}],
		transfer = NONE}]
	    end

	fun pmd oper
	  = let
	      val ((src1,src1size),
		   (src2,src2size)) = getSrc2 ()
	      val (dst,dstsize) = getDst ()
	      val _ 
		= Assert.assert
		  ("applyPrim: pmd, dstsize/src1size/src2size",
		   fn () => src1size = dstsize andalso
		            src2size = dstsize)

	      (* Reverse src1/src2 when src1 and src2 are temporaries
	       * and the oper is commutative. 
	       *)
	      val (src1,src2)
		= if (oper = Instruction.IMUL)
		     orelse
		     (oper = Instruction.MUL)
		    then case (Operand.deMemloc src1, Operand.deMemloc src2)
			   of (SOME memloc_src1, SOME memloc_src2)
			    => if x86Liveness.track memloc_src1
			          andalso
				  x86Liveness.track memloc_src2
				 then (src2,src1)
				 else (src1,src2)
			    | _ => (src1,src2)
		    else (src1,src2)
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements
		= [Assembly.instruction_mov
		   {dst = dst,
		    src = src1,
		    size = src1size},
		   Assembly.instruction_pmd
		   {oper = oper,
		    dst = dst,
		    src = src2,
		    size = dstsize}],
		transfer = NONE}]
	    end

	fun imul2 ()
	  = let
	      val ((src1,src1size),
		   (src2,src2size)) = getSrc2 ()
	      val (dst,dstsize) = getDst ()
	      val _ 
		= Assert.assert
		  ("applyPrim: pmd, dstsize/src1size/src2size",
		   fn () => src1size = dstsize andalso
		            src2size = dstsize)

	      (* Reverse src1/src2 when src1 and src2 are temporaries
	       * and the oper is commutative. 
	       *)
	      val (src1,src2)
		= case (Operand.deMemloc src1, Operand.deMemloc src2)
		    of (SOME memloc_src1, SOME memloc_src2)
		     => if x86Liveness.track memloc_src1
		           andalso
			   x86Liveness.track memloc_src2
			  then (src2,src1)
			  else (src1,src2)
		     | _ => (src1,src2)
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements
		= [Assembly.instruction_mov
		   {dst = dst,
		    src = src1,
		    size = src1size},
		   Assembly.instruction_imul2
		   {dst = dst,
		    src = src2,
		    size = dstsize}],
		transfer = NONE}]
	    end

	fun unal oper
	  = let
	      val (src,srcsize) = getSrc1 ()
	      val (dst,dstsize) = getDst ()
	      val _ 
		= Assert.assert
		  ("applyPrim: unal, dstsize/srcsize",
		   fn () => srcsize = dstsize)
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements 
		= [Assembly.instruction_mov
		   {dst = dst,
		    src = src,
		    size = srcsize},
		   Assembly.instruction_unal
		   {oper = oper,
		    dst = dst,
		    size = dstsize}],
		transfer = NONE}]
	    end

	fun sral oper
	  = let
	      val (dst,dstsize) = getDst ()
	      val ((src1,src1size),
		   (src2,src2size)) = getSrc2 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: sral, dstsize/src1size",
		   fn () => src1size = dstsize)
	      val _ 
		= Assert.assert
		  ("applyPrim: sral, src2size",
		   fn () => src2size = wordSize)
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements
		= [Assembly.instruction_mov
		   {dst = dst,
		    src = src1,
		    size = dstsize},
		   Assembly.instruction_sral
		   {oper = oper,
		    dst = dst,
		    count = src2,
		    size = dstsize}],
		transfer = NONE}]
	    end

	fun cmp condition
	  = let
	      val (dst,dstsize) = getDst ()
	      val ((src1,src1size),
		   (src2,src2size)) = getSrc2 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: cmp, src1size/src2size",
		   fn () => src1size = src2size)
	    in
	      (* Can't have an immediate in src1 position,
	       * so reverse the srcs and reverse the condition.
	       *
	       * This won't fix an immediate in both positions.
	       * Either constant folding eliminated it
	       * or the register allocator will raise an error.
	       *)
	      case Operand.deImmediate src1
		of SOME _ => AppendList.fromList
		             [Block.T'
			      {entry = NONE,
			       profileInfo = ProfileInfo.none,
			       statements
			       = [Assembly.instruction_cmp
				  {src1 = src2,
				   src2 = src1,
				   size = src1size},
				  Assembly.instruction_setcc
				  {condition 
				   = Instruction.condition_reverse condition,
				   dst = dst,
				   size = dstsize}],
			       transfer = NONE}]
		 | NONE => AppendList.fromList
			   [Block.T'
			    {entry = NONE,	
			     profileInfo = ProfileInfo.none,
			     statements
			     = [Assembly.instruction_cmp
				{src1 = src1,
				 src2 = src2,
				 size = src1size},
				Assembly.instruction_setcc
				{condition = condition,
				 dst = dst,
				 size = dstsize}],
			     transfer = NONE}]
	    end

	fun test condition
	  = let
	      val (dst,dstsize) = getDst ()
	      val ((src1,src1size),
		   (src2,src2size)) = getSrc2 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: cmp, src1size/src2size",
		   fn () => src1size = src2size)
	    in
	      (* Can't have an immediate in src1 position,
	       * so reverse the srcs and reverse the condition.
	       *
	       * This won't fix an immediate in both positions.
	       * Either constant folding eliminated it
	       * or the register allocator will raise an error.
	       *)
	      case Operand.deImmediate src1
		of SOME _ => AppendList.fromList
		             [Block.T'
			      {entry = NONE,
			       profileInfo = ProfileInfo.none,
			       statements
			       = [Assembly.instruction_test
				  {src1 = src2,
				   src2 = src1,
				   size = src1size},
				  Assembly.instruction_setcc
				  {condition 
				   = Instruction.condition_reverse condition,
				   dst = dst,
				   size = dstsize}],
			       transfer = NONE}]
		 | NONE => AppendList.fromList
			   [Block.T'
			    {entry = NONE,
			     profileInfo = ProfileInfo.none,
			     statements
			     = [Assembly.instruction_test
				{src1 = src1,
				 src2 = src2,
				 size = src1size},
				Assembly.instruction_setcc
				{condition = condition,
				 dst = dst,
				 size = dstsize}],
			     transfer = NONE}]
	    end
	  
	fun fbina oper
	  = let
	      val (dst,dstsize) = getDst ()
	      val ((src1,src1size),
		   (src2,src2size)) = getSrc2 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: fbina, dstsize/src1size/src2size",
		   fn () => src1size = dstsize andalso
		            src2size = dstsize)

	      (* Reverse src1/src2 when src1 and src2 are temporaries.
	       *)
	      val (oper,src1,src2)
		= case (Operand.deMemloc src1, Operand.deMemloc src2)
		    of (SOME memloc_src1, SOME memloc_src2) 
                     => if x86Liveness.track memloc_src1
		           andalso
			   x86Liveness.track memloc_src2
			  then (Instruction.fbina_reverse oper,src2,src1)
			  else (oper,src1,src2)
		     | _ => (oper,src1,src2)
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements
		= [Assembly.instruction_pfmov
		   {dst = dst,
		    src = src1,
		    size = src1size},
		   Assembly.instruction_pfbina
		   {oper = oper,
		    dst = dst,
		    src = src2,
		    size = dstsize}],
		transfer = NONE}]
	    end

	fun fbina_fmul oper
	  = let
	      val (dst,dstsize) = getDst ()
	      val ((src1,src1size),
		   (src2,src2size),
		   (src3,src3size)) = getSrc3 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: fbina_fmul, dstsize/src1size/src2size/src3size",
		   fn () => src1size = dstsize andalso
		            src2size = dstsize andalso
			    src3size = dstsize)
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements
		= [Assembly.instruction_pfmov
		   {dst = dst,
		    src = src1,
		    size = src1size},
		   Assembly.instruction_pfbina
		   {oper = Instruction.FMUL,
		    dst = dst,
		    src = src2,
		    size = dstsize},
		   Assembly.instruction_pfbina
		   {oper = oper,
		    dst = dst,
		    src = src3,
		    size = dstsize}],
		transfer = NONE}]
	    end

	fun funa oper
	  = let
	      val (dst,dstsize) = getDst ()
	      val (src,srcsize) = getSrc1 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: funa, dstsize/srcsize",
		   fn () => srcsize = dstsize)
	    in
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements 
		= [Assembly.instruction_pfmov
		   {dst = dst,
		    src = src,
		    size = srcsize},
		   Assembly.instruction_pfuna
		   {oper = oper,
		    dst = dst,
		    size = dstsize}],
		transfer = NONE}]
	    end

	fun flogarithm oper
	  = let
	      val (dst,dstsize) = getDst ()
	      val (src,srcsize) = getSrc1 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: logarithm, dstsize/srcsize",
		   fn () => srcsize = dstsize)
	    in	
	      AppendList.fromList
	      [Block.T'
	       {entry = NONE,
		profileInfo = ProfileInfo.none,
		statements 
		= [Assembly.instruction_pfldc
		   {oper = oper,
		    dst = dst,
		    size = dstsize},
		   Assembly.instruction_pfbinasp
		   {oper = Instruction.FYL2X,
		    src = src,
		    dst = dst,
		    size = dstsize}],
		transfer = NONE}]
	    end

	val (comment_begin,
	     comment_end)
	  = if !Control.Native.commented > 0
	      then let
		     val comment = primName
		   in 
		     (AppendList.single
		      (x86.Block.T'
		       {entry = NONE,
			profileInfo = x86.ProfileInfo.none,
			statements 
			= [x86.Assembly.comment 
			   ("begin prim: " ^ comment)],
			transfer = NONE}),
		      AppendList.single
		      (x86.Block.T'
		       {entry = NONE,
			profileInfo = x86.ProfileInfo.none,
			statements 
			= [x86.Assembly.comment 
			   ("end prim: " ^ comment)],
			transfer = NONE}))
		   end
	      else (AppendList.empty,AppendList.empty)
      in
	AppendList.appends
	[comment_begin,
	 (case Prim.name prim of
	       Array_length => lengthArrayVectorString ()
	     | Byte_byteToChar => mov ()
	     | Byte_charToByte => mov ()
	     | C_CS_charArrayToWord8Array => mov ()
	     | Char_lt => cmp Instruction.B
	     | Char_le => cmp Instruction.BE
	     | Char_gt => cmp Instruction.A
	     | Char_ge => cmp Instruction.AE
	     | Char_chr => xvom ()
	     | Char_ord => movx Instruction.MOVZX
	     | Cpointer_isNull 
	     => let
		  val (dst,dstsize) = getDst ()
		  val (src,srcsize) = getSrc1 ()
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements 
		    = [Assembly.instruction_cmp
		       {src1 = src,
			src2 = Operand.immediate_const_int 0,
			size = srcsize},
		       Assembly.instruction_setcc
		       {condition = Instruction.E,
			dst = dst,
			size = dstsize}],
		    transfer = NONE}]
		end
	     | FFI s 
	     => (case Prim.numArgs prim
		   of NONE 
		    => let
			 val (dst,dstsize) = getDst ()

			 val memloc
			   = x86.MemLoc.makeContents 
			     {base = Immediate.label (Label.fromString s),
			      size = dstsize,
			      class = Classes.CStatic}
		       in
			 AppendList.fromList
			 [Block.T'
			  {entry = NONE,
			   profileInfo = ProfileInfo.none,
			   statements
			   = [case Size.class dstsize
				of Size.INT 
				 => Assembly.instruction_mov 
				    {dst = dst,
				     src = Operand.memloc memloc,
				     size = dstsize}
				 | Size.FLT 
				 => Assembly.instruction_pfmov
				    {dst = dst,
				     src = Operand.memloc memloc,
				     size = dstsize}
				 | _ => Error.bug "prim: FFI"],
			   transfer = NONE}]
		       end
 	            | SOME _ => Error.bug "prim: FFI")
             | Int_add => binal Instruction.ADD
	     | Int_sub => binal Instruction.SUB
	     | Int_mul => imul2 () 
	     | Int_quot => pmd Instruction.IDIV
	     | Int_rem => pmd Instruction.IMOD
	     | Int_neg => unal Instruction.NEG 
	     | Int_lt => cmp Instruction.L
	     | Int_le => cmp Instruction.LE
	     | Int_gt => cmp Instruction.G
	     | Int_ge => cmp Instruction.GE
	     | Int_gtu => cmp Instruction.A
	     | Int_geu => cmp Instruction.AE
	     | IntInf_fromVector => mov ()
	     | IntInf_toVector => mov ()
	     | IntInf_fromWord => mov ()
	     | IntInf_toWord => mov ()
	     | MLton_eq => cmp Instruction.E
	     | MLton_serialize => unimplemented primName
	     | MLton_deserialize => unimplemented primName
	     | Real_Math_acos 
	     => let
		  val (dst,dstsize) = getDst ()
		  val (src,srcsize) = getSrc1 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_Math_acos, dstsize/srcsize",
		       fn () => srcsize = dstsize)
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements
		    = [Assembly.instruction_pfmov
		       {dst = realTemp1ContentsOperand,
			src = src,
			size = srcsize},
		       Assembly.instruction_pfmov
		       {dst = realTemp2ContentsOperand,
			src = realTemp1ContentsOperand,
			size = srcsize},
		       Assembly.instruction_pfbina
		       {oper = Instruction.FMUL,
			dst = realTemp2ContentsOperand,
			src = realTemp2ContentsOperand,
			size = srcsize},
		       Assembly.instruction_pfldc
		       {oper = Instruction.ONE,
			dst = realTemp3ContentsOperand,
			size = srcsize},
		       Assembly.instruction_pfbina
		       {oper = Instruction.FSUB,
			dst = realTemp3ContentsOperand,
			src = realTemp2ContentsOperand,
			size = srcsize},
		       Assembly.instruction_pfuna
		       {oper = Instruction.FSQRT,
			dst = realTemp3ContentsOperand,
			size = srcsize},
		       Assembly.instruction_pfmov
		       {dst = dst,
			src = realTemp3ContentsOperand,
			size = dstsize},
		       Assembly.instruction_pfbinasp
		       {oper = Instruction.FPATAN,
			src = realTemp1ContentsOperand,
			dst = dst,
			size = dstsize}],
		    transfer = NONE}]
		end
	     | Real_Math_asin
	     => let
		  val (dst,dstsize) = getDst ()
		  val (src,srcsize) = getSrc1 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_Math_asin, dstsize/srcsize",
		       fn () => srcsize = dstsize)
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements
		    = [Assembly.instruction_pfmov
		       {dst = dst,
			src = src,
			size = srcsize},
		       Assembly.instruction_pfmov
		       {dst = realTemp1ContentsOperand,
			src = dst,
			size = srcsize},
		       Assembly.instruction_pfbina
		       {oper = Instruction.FMUL,
			dst = realTemp1ContentsOperand,
			src = realTemp1ContentsOperand,
			size = srcsize},
		       Assembly.instruction_pfldc
		       {oper = Instruction.ONE,
			dst = realTemp2ContentsOperand,
			size = srcsize},
		       Assembly.instruction_pfbina
		       {oper = Instruction.FSUB,
			dst = realTemp2ContentsOperand,
			src = realTemp1ContentsOperand,
			size = srcsize},
		       Assembly.instruction_pfuna
		       {oper = Instruction.FSQRT,
			dst = realTemp2ContentsOperand,
			size = srcsize},
		       Assembly.instruction_pfbinasp
		       {oper = Instruction.FPATAN,
			src = realTemp2ContentsOperand,
			dst = dst,
			size = dstsize}],
		    transfer = NONE}]
		end
	     | Real_Math_atan 
	     => let
		  val (dst,dstsize) = getDst ()
		  val (src,srcsize) = getSrc1 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_Math_atan, dstsize/srcsize",
		       fn () => srcsize = dstsize)
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements 
		    = [Assembly.instruction_pfmov
		       {dst = dst,
			src = src,
			size = srcsize},
		       Assembly.instruction_pfldc
		       {oper = Instruction.ONE,
			dst = realTemp1ContentsOperand,
			size = dstsize},
		       Assembly.instruction_pfbinasp
		       {oper = Instruction.FPATAN,
			src = realTemp1ContentsOperand,
			dst = dst,
			size = dstsize}],
		    transfer = NONE}]
		end
	     | Real_Math_atan2 
	     => let
		  val (dst,dstsize) = getDst ()
		  val ((src1,src1size),
		       (src2,src2size))= getSrc2 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_Math_atan2, dstsize/src1size/src2size",
		       fn () => src1size = dstsize andalso
		                src2size = dstsize)
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements 
		    = [Assembly.instruction_pfmov
		       {dst = dst,
			src = src1,
			size = src1size},
		       Assembly.instruction_pfbinasp
		       {oper = Instruction.FPATAN,
			src = src2,
			dst = dst,
			size = dstsize}],
		    transfer = NONE}]
		end
	     | Real_Math_cos => funa Instruction.FCOS
	     | Real_Math_exp 
	     => let
		  val (dst,dstsize) = getDst ()
		  val (src,srcsize) = getSrc1 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_Math_exp, dstsize/srcsize",
		       fn () => srcsize = dstsize)
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements 
		    = [Assembly.instruction_pfldc
		       {oper = Instruction.L2E,
			dst = dst,
			size = dstsize},
		       Assembly.instruction_pfbina
		       {oper = Instruction.FMUL,
			src = src,
			dst = dst,
			size = dstsize},
		       Assembly.instruction_pfmov
		       {src = dst,
			dst = realTemp1ContentsOperand,
			size = dstsize},
		       Assembly.instruction_pfuna
		       {oper = Instruction.FRNDINT,
			dst = realTemp1ContentsOperand,
			size = dstsize},
		       Assembly.instruction_pfbina
		       {oper = Instruction.FSUB,
			src = realTemp1ContentsOperand,
			dst = dst,
			size = dstsize},
		       Assembly.instruction_pfuna
		       {oper = Instruction.F2XM1,
			dst = dst,
			size = dstsize},
		       Assembly.instruction_pfldc
		       {oper = Instruction.ONE,
			dst = realTemp2ContentsOperand,
			size = dstsize},
		       Assembly.instruction_pfbina
		       {oper = Instruction.FADD,
			src = realTemp2ContentsOperand,
			dst = dst,
			size = dstsize},
		       Assembly.instruction_pfbinas
		       {oper = Instruction.FSCALE,
			src = realTemp1ContentsOperand,
			dst = dst,
			size = dstsize}],
		    transfer = NONE}]
		end
 	     | Real_Math_ln => flogarithm Instruction.LN2
	     | Real_Math_log10 => flogarithm Instruction.LG2
	     | Real_Math_sin => funa Instruction.FSIN
	     | Real_Math_sqrt => funa Instruction.FSQRT
	     | Real_Math_tan
	     => let
		  val (dst,dstsize) = getDst ()
		  val (src,srcsize) = getSrc1 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_Math_tan, dstsize/srcsize",
		       fn () => srcsize = dstsize)
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements 
		    = [Assembly.instruction_pfmov
		       {src = src,
			dst = dst,
			size = dstsize},
		       Assembly.instruction_pfptan
		       {dst = dst,
			size = dstsize}],
		    transfer = NONE}]
		end
	     | Real_mul => fbina Instruction.FMUL
	     | Real_muladd => fbina_fmul Instruction.FADD
	     | Real_mulsub => fbina_fmul Instruction.FSUB
	     | Real_add => fbina Instruction.FADD
	     | Real_sub => fbina Instruction.FSUB
	     | Real_div => fbina Instruction.FDIV
	     | Real_lt 
	     => let
		  val (dst,dstsize) = getDst ()
		  val ((src1,src1size),
		       (src2,src2size))= getSrc2 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_lt, src1size/src2size",
		       fn () => src1size = src2size)
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements
		    = [Assembly.instruction_pfcom
		       {src1 = src2,
			src2 = src1,
			size = src1size},
		       Assembly.instruction_fstsw
		       {dst = fpswTempContentsOperand,
			check = false},
		       Assembly.instruction_test
		       {src1 = fpswTempContentsOperand,
			src2 = Operand.immediate_const_word 0wx4500,
			size = Size.WORD},
		       Assembly.instruction_setcc
		       {condition = Instruction.Z,
			dst = dst,
			size = dstsize}],
		    transfer = NONE}]
		end
	     | Real_le
	     => let
		  val (dst,dstsize) = getDst ()
		  val ((src1,src1size),
		       (src2,src2size))= getSrc2 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_le, src1size/src2size",
		       fn () => src1size = src2size)
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements
		    = [Assembly.instruction_pfcom
		       {src1 = src2,
			src2 = src1,
			size = src1size},
		       Assembly.instruction_fstsw
		       {dst = fpswTempContentsOperand,
			check = false},
		       Assembly.instruction_test
		       {src1 = fpswTempContentsOperand,
			src2 = Operand.immediate_const_word 0wx500,
			size = Size.WORD},
		       Assembly.instruction_setcc
		       {condition = Instruction.Z,
			dst = dst,
			size = dstsize}],
		    transfer = NONE}]
		end
	     | Real_equal
	     => let
		  val (dst,dstsize) = getDst ()
		  val ((src1,src1size),
		       (src2,src2size))= getSrc2 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_equal, src1size/src2size",
		       fn () => src1size = src2size)
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements
		    = [Assembly.instruction_pfucom
		       {src1 = src2,
			src2 = src1,
			size = src1size},
		       Assembly.instruction_fstsw
		       {dst = fpswTempContentsOperand,
			check = false},
		       Assembly.instruction_binal
		       {oper = Instruction.AND,
			dst = fpswTempContentsOperand,
			src = Operand.immediate_const_word 0wx4500,
			size = Size.WORD},
		       Assembly.instruction_cmp
		       {src1 = fpswTempContentsOperand,
			src2 = Operand.immediate_const_word 0wx4000,
			size = Size.WORD},
		       Assembly.instruction_setcc
		       {condition = Instruction.E,
			dst = dst,
			size = dstsize}],
		    transfer = NONE}]
		end
	     | Real_gt
	     => let
		  val (dst,dstsize) = getDst ()
		  val ((src1,src1size),
		       (src2,src2size))= getSrc2 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_gt, src1size/src2size",
		       fn () => src1size = src2size)
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements
		    = [Assembly.instruction_pfcom
		       {src1 = src1,
			src2 = src2,
			size = src1size},
		       Assembly.instruction_fstsw
		       {dst = fpswTempContentsOperand,
			check = false},
		       Assembly.instruction_test
		       {src1 = fpswTempContentsOperand,
			src2 = Operand.immediate_const_word 0wx4500,
			size = Size.WORD},
		       Assembly.instruction_setcc
		       {condition = Instruction.Z,
			dst = dst,
			size = dstsize}],
		    transfer = NONE}]
		end
	     | Real_ge
	     => let
		  val (dst,dstsize) = getDst ()
		  val ((src1,src1size),
		       (src2,src2size))= getSrc2 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_ge, src1size/src2size",
		       fn () => src1size = src2size)
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements
		    = [Assembly.instruction_pfcom
		       {src1 = src1,
			src2 = src2,
			size = src1size},
		       Assembly.instruction_fstsw
		       {dst = fpswTempContentsOperand,
			check = false},
		       Assembly.instruction_test
		       {src1 = fpswTempContentsOperand,
			src2 = Operand.immediate_const_word 0wx500,
			size = Size.WORD},
		       Assembly.instruction_setcc
		       {condition = Instruction.Z,
			dst = dst,
			size = dstsize}],
		    transfer = NONE}]
		end
	     | Real_qequal
	     => let
		  val (dst,dstsize) = getDst ()
		  val ((src1,src1size),
		       (src2,src2size))= getSrc2 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_qequal, src1size/src2size",
		       fn () => src1size = src2size)
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements
		    = [Assembly.instruction_pfucom
		       {src1 = src2,
			src2 = src1,
			size = src1size},
		       Assembly.instruction_fstsw
		       {dst = fpswTempContentsOperand,
			check = false},
		       Assembly.instruction_test
		       {src1 = fpswTempContentsOperand,
			src2 = Operand.immediate_const_word 0wx4400,
			size = Size.WORD},
		       Assembly.instruction_setcc
		       {condition = Instruction.NE,
			dst = dst,
			size = dstsize}],
		    transfer = NONE}]
		end
	     | Real_abs => funa Instruction.FABS
	     | Real_fromInt 
	     => let
		  val (dst,dstsize) = getDst ()
		  val (src,srcsize) = getSrc1 ()
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements 
		    = [Assembly.instruction_pfmovfi
		       {dst = dst,
			src = src,
			srcsize = srcsize,
			dstsize = dstsize}],
		    transfer = NONE}]
		end 
	     | Real_toInt
	     => let
		  val (dst,dstsize) = getDst ()
		  val (src,srcsize) = getSrc1 ()
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements 
		    = [Assembly.instruction_pfmovti
		       {dst = dst,
			src = src,
			srcsize = srcsize,
			dstsize = dstsize}],
		    transfer = NONE}]
		end 
	     | Real_ldexp 
	     => let
		  val (dst,dstsize) = getDst ()
		  val ((src1,src1size),
		       (src2,src2size)) = getSrc2 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_ldexp, dstsize/src1size",
		       fn () => src1size = dstsize)
		  val _
		    = Assert.assert
		      ("applyPrim: Real_qequal, src2size",
		       fn () => src2size = Size.LONG)
		in
		  AppendList.fromList
		  [Block.T'
		   {entry = NONE,
		    profileInfo = ProfileInfo.none,
		    statements 
		    = [Assembly.instruction_pfmovfi
		       {dst = realTemp1ContentsOperand,
			src = src2,
			srcsize = src2size,
			dstsize = dstsize},
		       Assembly.instruction_pfmov
		       {dst = dst,
			src = src1,
			size = dstsize},
		       Assembly.instruction_pfbinas
		       {oper = Instruction.FSCALE,
			dst = dst,
			src = realTemp1ContentsOperand,
			size = dstsize}],
		    transfer = NONE}]
		end
	     | Real_neg => funa Instruction.FCHS
	     | Real_round => funa Instruction.FRNDINT
	     | String_fromWord8Vector => mov ()
	     | String_toWord8Vector => mov ()
	     | Vector_length => lengthArrayVectorString ()
	     | Word8_toInt => movx Instruction.MOVZX
	     | Word8_toIntX => movx Instruction.MOVSX
	     | Word8_fromInt => xvom ()
	     | Word8_toLargeWord => movx Instruction.MOVZX
	     | Word8_toLargeWordX => movx Instruction.MOVSX
	     | Word8_fromLargeWord => xvom ()
	     | Word8_add => binal Instruction.ADD
	     | Word8_sub => binal Instruction.SUB
	     | Word8_andb => binal Instruction.AND
	     | Word8_orb => binal Instruction.OR
	     | Word8_xorb => binal Instruction.XOR
	     | Word8_mul => pmd Instruction.MUL
	     | Word8_div => pmd Instruction.DIV
	     | Word8_mod => pmd Instruction.MOD
	     | Word8_neg => unal Instruction.NEG
	     | Word8_notb => unal Instruction.NOT
	     | Word8_lt => cmp Instruction.B
	     | Word8_le => cmp Instruction.BE
	     | Word8_gt => cmp Instruction.A
	     | Word8_ge => cmp Instruction.AE
	     | Word8_rol => sral Instruction.ROL
	     | Word8_ror => sral Instruction.ROR
	     | Word8_lshift => sral Instruction.SHL
	     | Word8_rshift => sral Instruction.SHR
	     | Word8_arshift => sral Instruction.SAR
	     | Word8Array_subWord => subWord8ArrayVector ()
	     | Word8Array_updateWord => updateWord8Array ()
	     | Word8Vector_subWord => subWord8ArrayVector ()
	     | Word32_toIntX => mov ()
	     | Word32_fromInt => mov ()
	     | Word32_add => binal Instruction.ADD
	     | Word32_sub => binal Instruction.SUB
	     | Word32_andb => binal Instruction.AND
	     | Word32_orb => binal Instruction.OR
	     | Word32_xorb => binal Instruction.XOR
(*
	     | Word32_mul => pmd Instruction.MUL
*)
	     | Word32_mul => imul2 ()
	     | Word32_div => pmd Instruction.DIV
	     | Word32_mod => pmd Instruction.MOD
	     | Word32_neg => unal Instruction.NEG
	     | Word32_notb => unal Instruction.NOT
	     | Word32_lt => cmp Instruction.B
	     | Word32_le => cmp Instruction.BE
	     | Word32_gt => cmp Instruction.A
	     | Word32_ge => cmp Instruction.AE
	     | Word32_rol => sral Instruction.ROL
	     | Word32_ror => sral Instruction.ROR
	     | Word32_lshift => sral Instruction.SHL
	     | Word32_rshift => sral Instruction.SHR
	     | Word32_arshift => sral Instruction.SAR
	     | _ => Error.bug ("prim: strange Prim.Name.t: " ^ primName)),
	 comment_end]
      end

  fun ccall {args: (x86.Operand.t * x86.Size.t) vector,
	     frameInfo,
	     func as CFunction.T {name, returnTy, ...},
	     return: x86.Label.t option,
	     transInfo: transInfo}
    = let
	val dstsize = Option.map (returnTy, toX86Size)
	val comment_begin
	  = if !Control.Native.commented > 0
	      then AppendList.single (x86.Block.T'
				      {entry = NONE,
				       profileInfo = x86.ProfileInfo.none,
				       statements 
				       = [x86.Assembly.comment
					  ("begin ccall: " ^ name)],
				       transfer = NONE})
	    else AppendList.empty
      in
	AppendList.appends
	[comment_begin,
	 AppendList.single
	 (Block.T'
	  {entry = NONE,
	   profileInfo = ProfileInfo.none,
	   statements = [],
	   transfer = SOME (Transfer.ccall 
			    {args = Vector.toList args,
			     dstsize = dstsize,
			     frameInfo = frameInfo,
			     func = func,
			     return = return,
			     target = Label.fromString name})})]
      end

  fun creturn {dst: (x86.Operand.t * x86.Size.t) option,
	       frameInfo: x86.FrameInfo.t option,
	       func: CFunction.t,
	       label: x86.Label.t, 
	       transInfo as {live, liveInfo, ...}: transInfo}
    = let
	val name = CFunction.name func
	fun getDst ()
	  = case dst
	      of SOME dst => dst
	       | NONE => Error.bug "creturn: getDst"
	fun default ()
	  = let
	      val _ = x86Liveness.LiveInfo.setLiveOperands
		      (liveInfo, label, live label)
	    in 
	      AppendList.single
	      (x86.Block.T'
	       {entry = SOME (Entry.creturn {dst = dst,
					     frameInfo = frameInfo,
					     func = func,
					     label = label}),
		profileInfo = ProfileInfo.none,
		statements = [],
		transfer = NONE})
	    end
	val comment_end
	  = if !Control.Native.commented > 0
	      then (AppendList.single
		    (x86.Block.T' {entry = NONE,
				   profileInfo = x86.ProfileInfo.none,
				   statements = [x86.Assembly.comment 
						 ("end creturn: " ^ name)],
				   transfer = NONE}))
	      else AppendList.empty
      in
	AppendList.appends [default (), comment_end]
      end

  fun arith {prim : Prim.t,
	     args : (Operand.t * Size.t) vector,
	     dst : (Operand.t * Size.t),
	     overflow : Label.t,
	     success : Label.t,
	     transInfo as {addData, frameLayouts, live, liveInfo, ...} : transInfo}
    = let
	val primName = Prim.toString prim
	datatype z = datatype Prim.Name.t

	fun arg i = Vector.sub (args, i)
	  
	val (src1, src1size) = arg 0
	val (dst, dstsize) = dst
	val _ = Assert.assert
	        ("arith: dstsize/srcsize",
		 fn () => src1size = dstsize)
	fun check (src, statement, condition)
	  = AppendList.single
	    (x86.Block.T'
	     {entry = NONE,	
	      profileInfo = x86.ProfileInfo.none,
	      statements = [x86.Assembly.instruction_mov
			    {dst = dst,
			     src = src,
			     size = src1size},
			    statement],
	      transfer = SOME (x86.Transfer.iff
			       {condition = condition,
				truee = overflow,
				falsee = success})})
	fun binal (oper: x86.Instruction.binal, condition)
	  = let
	      val (src2, src2size) = arg 1
	      val _ = Assert.assert
		      ("arith: binal, dstsize/src2size",
		       fn () => src2size = dstsize)
	      (* Reverse src1/src2 when src1 and src2 are
	       * temporaries and the oper is commutative. 
	       *)
	      val (src1,src2)
		= if (oper = x86.Instruction.ADD)
		    then case (x86.Operand.deMemloc src1,
			       x86.Operand.deMemloc src2)
			   of (SOME memloc_src1, SOME memloc_src2)
			    => if x86Liveness.track memloc_src1
			          andalso
				  x86Liveness.track memloc_src2
				 then (src2,src1)
				 else (src1,src2)
			    | _ => (src1,src2)
		    else (src1,src2)
	    in
	      check (src1,
		     x86.Assembly.instruction_binal
		     {oper = oper,
		      dst = dst,
		      src = src2,
		      size = dstsize},
		     condition)
	    end
 	fun pmd (oper: x86.Instruction.md, condition)
  	  = let
 	      val (src2, src2size) = arg 1
 	      val _ = Assert.assert
 		      ("arith: pmd, dstsize/src2size",
 		       fn () => src2size = dstsize)
 	      (* Reverse src1/src2 when src1 and src2 are
 	       * temporaries and the oper is commutative. 
 	       *)
 	      val (src1, src2)
 		= if oper = x86.Instruction.MUL
 		    then case (x86.Operand.deMemloc src1,
 			       x86.Operand.deMemloc src2)
 			   of (SOME memloc_src1, SOME memloc_src2)
 			    => if x86Liveness.track memloc_src1
 			          andalso
 				  x86Liveness.track memloc_src2
 				 then (src2,src1)
 				 else (src1,src2)
 			    | _ => (src1,src2)
 		    else (src1,src2)
 	    in
 	      check (src1,
		     x86.Assembly.instruction_pmd
 		     {oper = oper,
 		      dst = dst,
 		      src = src2,
 		      size = dstsize},
		     condition)
 	    end
	fun unal (oper: x86.Instruction.unal, condition)
	  = let
	    in
	      check (src1,
		     x86.Assembly.instruction_unal 
		     {oper = oper,
		      dst = dst,
		      size = dstsize},
		     condition)
	    end
	fun imul2_check condition
	  = let
	      val (src2, src2size) = arg 1
	      val _ = Assert.assert
		      ("arith: imul2_check, dstsizesrc2size",
		       fn () => src2size = dstsize)
	      (* Reverse src1/src2 when src1 and src2 are
	       * temporaries and the oper is commutative. 
	       *)
	      val (src1, src2)
		= case (x86.Operand.deMemloc src1,
			x86.Operand.deMemloc src2)
		    of (SOME memloc_src1, SOME memloc_src2)
		     => if x86Liveness.track memloc_src1
		           andalso
			   x86Liveness.track memloc_src2
			  then (src2,src1)
			  else (src1,src2)
		     | _ => (src1,src2)
	    in
	      check (src1,
		     x86.Assembly.instruction_imul2
		     {dst = dst,
		      src = src2,
		      size = dstsize},
		     condition)
	    end
	  
	val (comment_begin,
	     comment_end)
	  = if !Control.Native.commented > 0
	      then let
		     val comment = primName
		   in 
		     (AppendList.single
		      (x86.Block.T'
		       {entry = NONE,
			profileInfo = x86.ProfileInfo.none,
			statements 
			= [x86.Assembly.comment 
			   ("begin arith: " ^ comment)],
			transfer = NONE}),
		      AppendList.single
		      (x86.Block.T'
		       {entry = NONE,
			profileInfo = x86.ProfileInfo.none,
			statements 
			= [x86.Assembly.comment 
			   ("end arith: " ^ comment)],
			transfer = NONE}))
		   end
	      else (AppendList.empty,AppendList.empty)
      in
	AppendList.appends
	[comment_begin,
	 (case Prim.name prim of
	     Int_addCheck => binal (x86.Instruction.ADD, x86.Instruction.O)
	   | Int_subCheck => binal (x86.Instruction.SUB, x86.Instruction.O)
	   | Int_mulCheck => imul2_check x86.Instruction.O
	   | Int_negCheck => unal (x86.Instruction.NEG, x86.Instruction.O)
	   | Word32_addCheck => binal (x86.Instruction.ADD, x86.Instruction.C)
	   | Word32_mulCheck => pmd (x86.Instruction.MUL, x86.Instruction.C)
	   | _ => Error.bug ("arith: strange Prim.Name.t: " ^ primName))]
      end

end
