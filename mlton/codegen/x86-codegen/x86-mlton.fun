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
     structure IntSize = IntSize
     structure Prim = Prim
     structure WordSize = WordSize
     datatype z = datatype IntSize.prim
     datatype z = datatype RealSize.t
     datatype z = datatype WordSize.prim
  end

  type transInfo = {addData : x86.Assembly.t list -> unit,
		    frameInfoToX86: (x86MLtonBasic.Machine.FrameInfo.t
				     -> x86.FrameInfo.t),
		    live: x86.Label.t -> x86.Operand.t list,
		    liveInfo: x86Liveness.LiveInfo.t}

  fun prim {prim : Prim.t,
	    args : (Operand.t * Size.t) vector,
	    dsts : (Operand.t * Size.t) vector,
	    transInfo = {...} : transInfo}
    = let
	val primName = Prim.toString prim
	datatype z = datatype Prim.Name.t

	fun getDst1 ()
	  = Vector.sub (dsts, 0)
	    handle _ => Error.bug "applyPrim: getDst1"
	fun getDst2 ()
	  = (Vector.sub (dsts, 0), Vector.sub (dsts, 1))
	    handle _ => Error.bug "applyPrim: getDst2"
	fun getSrc1 ()
	  = Vector.sub (args, 0)
	    handle _ => Error.bug "applyPrim: getSrc1"
	fun getSrc2 ()
	  = (Vector.sub (args, 0), Vector.sub (args, 1))
	    handle _ => Error.bug "applyPrim: getSrc2"
	fun getSrc3 ()
	  = (Vector.sub (args, 0), Vector.sub (args, 1), Vector.sub (args, 2))
	    handle _ => Error.bug "applyPrim: getSrc3"
	fun getSrc4 ()
	  = (Vector.sub (args, 0), Vector.sub (args, 1), 
	     Vector.sub (args, 2), Vector.sub (args, 3))
	    handle _ => Error.bug "applyPrim: getSrc4"

	fun mov ()
	  = let
	      val (dst,dstsize) = getDst1 ()
	      val (src,srcsize) = getSrc1 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: mov, dstsize/srcsize",
		   fn () => srcsize = dstsize)
	    in
	      AppendList.fromList
	      [Block.mkBlock'
	       {entry = NONE,
		statements
		= [Assembly.instruction_mov
		   {dst = dst,
		    src = src,
		    size = srcsize}],
		transfer = NONE}]
	    end
	  
	fun movx oper
	  = let
	      val (dst,dstsize) = getDst1 ()
	      val (src,srcsize) = getSrc1 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: movx, dstsize/srcsize",
		   fn () => Size.lt(srcsize,dstsize))
	    in
	      AppendList.fromList
	      [Block.mkBlock'
	       {entry = NONE,
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
	      val (dst,dstsize) = getDst1 ()
	      val (src,srcsize) = getSrc1 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: xvom, dstsize/srcsize",
		   fn () => Size.lt(dstsize,srcsize))
	    in
	      AppendList.fromList
	      [Block.mkBlock'
	       {entry = NONE,
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
	      val (dst,dstsize) = getDst1 ()
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
	      [Block.mkBlock'
	       {entry = NONE,
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

	fun binal64 (oper1, oper2)
	  = let
	      val ((src1,src1size),
		   (src2,src2size),
		   (src3,src3size),
		   (src4,src4size)) = getSrc4 ()
	      val ((dst1,dst1size),
		   (dst2,dst2size)) = getDst2 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: binal64, dst1size/dst2size/src1size/src2size/src3size/src4size",
		   fn () => src1size = dst1size andalso
		            src3size = dst1size andalso
			    src2size = dst2size andalso
		            src4size = dst2size andalso
		            dst1size = dst2size)
	    in
	      AppendList.fromList
	      [Block.mkBlock'
	       {entry = NONE,
		statements
		= [Assembly.instruction_mov
		   {dst = dst1,
		    src = src1,
		    size = src1size},
		   Assembly.instruction_mov
		   {dst = dst2,
		    src = src2,
		    size = src2size},
		   Assembly.instruction_binal
		   {oper = oper1,
		    dst = dst1,
		    src = src3,
		    size = dst1size},
		   Assembly.instruction_binal
		   {oper = oper2,
		    dst = dst2,
		    src = src4,
		    size = dst2size}],
		transfer = NONE}]
	    end

	fun pmd oper
	  = let
	      val ((src1,src1size),
		   (src2,src2size)) = getSrc2 ()
	      val (dst,dstsize) = getDst1 ()
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
	      [Block.mkBlock'
	       {entry = NONE,
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
	      val (dst,dstsize) = getDst1 ()
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
	      [Block.mkBlock'
	       {entry = NONE,
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
	      val (dst,dstsize) = getDst1 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: unal, dstsize/srcsize",
		   fn () => srcsize = dstsize)
	    in
	      AppendList.fromList
	      [Block.mkBlock'
	       {entry = NONE,
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

	fun unal64 (oper, mk)
	  = let
	      val ((src1,src1size),(src2,src2size)) = getSrc2 ()
	      val ((dst1,dst1size),(dst2,dst2size)) = getDst2 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: unal, dst1size/dst2size/src1size/src2size",
		   fn () => src1size = dst1size andalso
                            src2size = dst2size andalso
                            dst1size = dst2size)
	    in
	      AppendList.fromList
	      [Block.mkBlock'
	       {entry = NONE,
		statements 
		= [Assembly.instruction_mov
		   {dst = dst1,
		    src = src1,
		    size = src1size},
		   Assembly.instruction_mov
		   {dst = dst2,
		    src = src2,
		    size = src2size},
		   Assembly.instruction_unal
		   {oper = oper,
		    dst = dst1,
		    size = dst1size}] @
		  (mk (dst2,dst2size)) @
		  [Assembly.instruction_unal
		   {oper = oper,
		    dst = dst2,
		    size = dst2size}],
		transfer = NONE}]
	    end

	fun sral oper
	  = let
	      val (dst,dstsize) = getDst1 ()
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
	      [Block.mkBlock'
	       {entry = NONE,
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
	      val (dst,dstsize) = getDst1 ()
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
		             [Block.mkBlock'
			      {entry = NONE,
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
			   [Block.mkBlock'
			    {entry = NONE,	
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
	  
	fun fbina oper
	  = let
	      val (dst,dstsize) = getDst1 ()
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
	      [Block.mkBlock'
	       {entry = NONE,
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
	      val (dst,dstsize) = getDst1 ()
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
	      [Block.mkBlock'
	       {entry = NONE,
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
	      val (dst,dstsize) = getDst1 ()
	      val (src,srcsize) = getSrc1 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: funa, dstsize/srcsize",
		   fn () => srcsize = dstsize)
	    in
	      AppendList.fromList
	      [Block.mkBlock'
	       {entry = NONE,
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
	      val (dst,dstsize) = getDst1 ()
	      val (src,srcsize) = getSrc1 ()
	      val _ 
		= Assert.assert
		  ("applyPrim: logarithm, dstsize/srcsize",
		   fn () => srcsize = dstsize)
	    in	
	      AppendList.fromList
	      [Block.mkBlock'
	       {entry = NONE,
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
		      (x86.Block.mkBlock'
		       {entry = NONE,
			statements 
			= [x86.Assembly.comment 
			   ("begin prim: " ^ comment)],
			transfer = NONE}),
		      AppendList.single
		      (x86.Block.mkBlock'
		       {entry = NONE,
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
	     FFI_Symbol {name, ...}
	     => let
		   val (dst,dstsize) = getDst1 ()
		   val memloc
		      = x86.MemLoc.makeContents 
		      {base = Immediate.label (Label.fromString name),
		       size = dstsize,
		       class = Classes.CStatic}
		in
		   AppendList.fromList
		   [Block.mkBlock'
		    {entry = NONE,
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
             | Int_add s => 
		(case IntSize.prim s of
		    I8 => binal Instruction.ADD
		  | I16 => binal Instruction.ADD
		  | I32 => binal Instruction.ADD
		  | I64 => binal64 (Instruction.ADD, Instruction.ADC))
	     | Int_equal s => 	
		(case IntSize.prim s of
		    I8 => cmp Instruction.E
		  | I16 => cmp Instruction.E
		  | I32 => cmp Instruction.E
		  | I64 => Error.bug "FIXME")
	     | Int_ge s => 	
		(case IntSize.prim s of
		    I8 => cmp Instruction.GE
		  | I16 => cmp Instruction.GE
		  | I32 => cmp Instruction.GE
		  | I64 => Error.bug "FIXME")
	     | Int_gt s => 
		(case IntSize.prim s of
		    I8 => cmp Instruction.G
		  | I16 => cmp Instruction.G
		  | I32 => cmp Instruction.G
		  | I64 => Error.bug "FIXME")
	     | Int_le s => 
		(case IntSize.prim s of
		    I8 => cmp Instruction.LE
		  | I16 => cmp Instruction.LE
		  | I32 => cmp Instruction.LE
		  | I64 => Error.bug "FIXME")
	     | Int_lt s =>
		(case IntSize.prim s of
		    I8 => cmp Instruction.L
		  | I16 => cmp Instruction.L
		  | I32 => cmp Instruction.L
		  | I64 => Error.bug "FIXME")
	     | Int_mul s =>
		(case IntSize.prim s of
		    I8 => pmd Instruction.IMUL
		  | I16 => imul2 () 
		  | I32 => imul2 ()
		  | I64 => Error.bug "FIXME")
	     | Int_neg s => 
		(case IntSize.prim s of
		    I8 => unal Instruction.NEG 
		  | I16 => unal Instruction.NEG 
		  | I32 => unal Instruction.NEG 
		  | I64 => unal64 (Instruction.NEG, 
				   fn (dst,dstsize) => [Assembly.instruction_binal
							{dst = dst,
							 oper = Instruction.ADC,
							 src = Operand.immediate_const_int 0,
							 size = dstsize}]))
	     | Int_quot s => 
		(case IntSize.prim s of
		    I8 => pmd Instruction.IDIV
		  | I16 => pmd Instruction.IDIV
		  | I32 => pmd Instruction.IDIV
		  | I64 => Error.bug "FIXME")
	     | Int_rem s => 
		(case IntSize.prim s of
		    I8 => pmd Instruction.IMOD
		  | I16 => pmd Instruction.IMOD
		  | I32 => pmd Instruction.IMOD
		  | I64 => Error.bug "FIXME")
	     | Int_sub s => 
		(case IntSize.prim s of
		    I8 => binal Instruction.SUB
		  | I16 => binal Instruction.SUB
		  | I32 => binal Instruction.SUB
		  | I64 => binal64 (Instruction.SUB, Instruction.SBB))
	     | Int_toInt (s, s') =>
		(case (IntSize.prim s, IntSize.prim s') of
		    (I64, I64) => Error.bug "FIXME"
		  | (I64, I32) => Error.bug "FIXME"
		  | (I64, I16) => Error.bug "FIXME"
		  | (I64, I8) => Error.bug "FIXME"
		  | (I32, I64) => Error.bug "FIXME"
		  | (I32, I32) => mov ()
		  | (I32, I16) => xvom ()
		  | (I32, I8) => xvom ()
		  | (I16, I64) => Error.bug "FIXME"
		  | (I16, I32) => movx Instruction.MOVSX
		  | (I16, I16) => mov ()
		  | (I16, I8) => xvom ()
		  | (I8, I64) => Error.bug "FIXME"
		  | (I8, I32) => movx Instruction.MOVSX
		  | (I8, I16) => movx Instruction.MOVSX
		  | (I8, I8) => mov ())
	     | Int_toReal (s, s')
	     => let
		  fun default () =
		    let
		      val (dst,dstsize) = getDst1 ()
		      val (src,srcsize) = getSrc1 ()
		    in
		      AppendList.fromList
		      [Block.mkBlock'
		       {entry = NONE,
			statements 
			= [Assembly.instruction_pfmovfi
			   {src = src,
			    dst = dst,
			    srcsize = srcsize,
			    dstsize = dstsize}],
			transfer = NONE}]
		    end 
		  fun default' () =
		    let
		      val (dst,dstsize) = getDst1 ()
		      val (src,srcsize) = getSrc1 ()
		      val (tmp,tmpsize) =
			 (fildTempContentsOperand, Size.WORD)
		    in
		      AppendList.fromList
		      [Block.mkBlock'
		       {entry = NONE,
			statements 
			= [Assembly.instruction_movx
			   {oper = Instruction.MOVSX,
			    src = src,
			    dst = tmp,
			    dstsize = tmpsize,
			    srcsize = srcsize},
			   Assembly.instruction_pfmovfi
			   {src = tmp,
			    dst = dst,
			    srcsize = tmpsize,
			    dstsize = dstsize}],
			transfer = NONE}]
		    end 
		in
		   case (IntSize.prim s, s') of
		      (I64, R64) => Error.bug "FIXME"
		    | (I64, R32) => Error.bug "FIXME"
		    | (I32, R64) => default ()
		    | (I32, R32) => default ()
		    | (I16, R64) => default ()
		    | (I16, R32) => default ()
		    | (I8, R64) => default' ()
		    | (I8, R32) => default' ()
		end
	     | Int_toWord (s, s') =>
		(case (IntSize.prim s, WordSize.prim s') of
		    (I64, W64) => Error.bug "FIXME"
		  | (I64, W32) => Error.bug "FIXME"
		  | (I64, W16) => Error.bug "FIXME"
		  | (I64, W8) => Error.bug "FIXME"
		  | (I32, W64) => Error.bug "FIXME"
		  | (I32, W32) => mov ()
		  | (I32, W16) => xvom ()
		  | (I32, W8) => xvom ()
		  | (I16, W64) => Error.bug "FIXME"
		  | (I16, W32) => movx Instruction.MOVSX
		  | (I16, W16) => mov ()
		  | (I16, W8) => xvom ()
		  | (I8, W64) => Error.bug "FIXME"
		  | (I8, W32) => movx Instruction.MOVSX
		  | (I8, W16) => movx Instruction.MOVSX
		  | (I8, W8) => mov ())
	     | MLton_eq => cmp Instruction.E
	     | Real_Math_acos _
	     => let
		  val (dst,dstsize) = getDst1 ()
		  val (src,srcsize) = getSrc1 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_Math_acos, dstsize/srcsize",
		       fn () => srcsize = dstsize)
		  val realTemp1ContentsOperand = realTemp1ContentsOperand srcsize
		  val realTemp2ContentsOperand = realTemp2ContentsOperand srcsize
		  val realTemp3ContentsOperand = realTemp3ContentsOperand srcsize
		in
		  AppendList.fromList
		  [Block.mkBlock'
		   {entry = NONE,
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
	     | Real_Math_asin _
	     => let
		  val (dst,dstsize) = getDst1 ()
		  val (src,srcsize) = getSrc1 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_Math_asin, dstsize/srcsize",
		       fn () => srcsize = dstsize)
		  val realTemp1ContentsOperand = realTemp1ContentsOperand srcsize
		  val realTemp2ContentsOperand = realTemp2ContentsOperand srcsize
		in
		  AppendList.fromList
		  [Block.mkBlock'
		   {entry = NONE,
		    statements
		    = [Assembly.instruction_pfmov
		       {dst = dst,
			src = src,
			size = srcsize},
		       Assembly.instruction_pfmov
		       {dst = realTemp1ContentsOperand,
			src = dst,
			size = dstsize},
		       Assembly.instruction_pfbina
		       {oper = Instruction.FMUL,
			dst = realTemp1ContentsOperand,
			src = realTemp1ContentsOperand,
			size = dstsize},
		       Assembly.instruction_pfldc
		       {oper = Instruction.ONE,
			dst = realTemp2ContentsOperand,
			size = dstsize},
		       Assembly.instruction_pfbina
		       {oper = Instruction.FSUB,
			dst = realTemp2ContentsOperand,
			src = realTemp1ContentsOperand,
			size = dstsize},
		       Assembly.instruction_pfuna
		       {oper = Instruction.FSQRT,
			dst = realTemp2ContentsOperand,
			size = dstsize},
		       Assembly.instruction_pfbinasp
		       {oper = Instruction.FPATAN,
			src = realTemp2ContentsOperand,
			dst = dst,
			size = dstsize}],
		    transfer = NONE}]
		end
	     | Real_Math_atan _
	     => let
		  val (dst,dstsize) = getDst1 ()
		  val (src,srcsize) = getSrc1 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_Math_atan, dstsize/srcsize",
		       fn () => srcsize = dstsize)
		  val realTemp1ContentsOperand = realTemp1ContentsOperand srcsize
		in
		  AppendList.fromList
		  [Block.mkBlock'
		   {entry = NONE,
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
	     | Real_Math_atan2 _
	     => let
		  val (dst,dstsize) = getDst1 ()
		  val ((src1,src1size),
		       (src2,src2size))= getSrc2 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_Math_atan2, dstsize/src1size/src2size",
		       fn () => src1size = dstsize andalso
		                src2size = dstsize)
		in
		  AppendList.fromList
		  [Block.mkBlock'
		   {entry = NONE,
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
	     | Real_Math_cos _ => funa Instruction.FCOS
	     | Real_Math_exp _
	     => let
		  val (dst,dstsize) = getDst1 ()
		  val (src,srcsize) = getSrc1 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_Math_exp, dstsize/srcsize",
		       fn () => srcsize = dstsize)
		  val realTemp1ContentsOperand = realTemp1ContentsOperand srcsize
		  val realTemp2ContentsOperand = realTemp2ContentsOperand srcsize
		in
		  AppendList.fromList
		  [Block.mkBlock'
		   {entry = NONE,
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
 	     | Real_Math_ln _ => flogarithm Instruction.LN2
	     | Real_Math_log10 _ => flogarithm Instruction.LG2
	     | Real_Math_sin _ => funa Instruction.FSIN
	     | Real_Math_sqrt _ => funa Instruction.FSQRT
	     | Real_Math_tan _
	     => let
		  val (dst,dstsize) = getDst1 ()
		  val (src,srcsize) = getSrc1 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_Math_tan, dstsize/srcsize",
		       fn () => srcsize = dstsize)
		in
		  AppendList.fromList
		  [Block.mkBlock'
		   {entry = NONE,
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
	     | Real_mul _ => fbina Instruction.FMUL
	     | Real_muladd _ => fbina_fmul Instruction.FADD
	     | Real_mulsub _ => fbina_fmul Instruction.FSUB
	     | Real_add _ => fbina Instruction.FADD
	     | Real_sub _ => fbina Instruction.FSUB
	     | Real_div _ => fbina Instruction.FDIV
	     | Real_lt _
	     => let
		  val (dst,dstsize) = getDst1 ()
		  val ((src1,src1size),
		       (src2,src2size))= getSrc2 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_lt, src1size/src2size",
		       fn () => src1size = src2size)
		in
		  AppendList.fromList
		  [Block.mkBlock'
		   {entry = NONE,
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
	     | Real_le _
	     => let
		  val (dst,dstsize) = getDst1 ()
		  val ((src1,src1size),
		       (src2,src2size))= getSrc2 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_le, src1size/src2size",
		       fn () => src1size = src2size)
		in
		  AppendList.fromList
		  [Block.mkBlock'
		   {entry = NONE,
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
	     | Real_equal _
	     => let
		  val (dst,dstsize) = getDst1 ()
		  val ((src1,src1size),
		       (src2,src2size))= getSrc2 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_equal, src1size/src2size",
		       fn () => src1size = src2size)
		in
		  AppendList.fromList
		  [Block.mkBlock'
		   {entry = NONE,
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
	     | Real_gt _
	     => let
		  val (dst,dstsize) = getDst1 ()
		  val ((src1,src1size),
		       (src2,src2size))= getSrc2 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_gt, src1size/src2size",
		       fn () => src1size = src2size)
		in
		  AppendList.fromList
		  [Block.mkBlock'
		   {entry = NONE,
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
	     | Real_ge _
	     => let
		  val (dst,dstsize) = getDst1 ()
		  val ((src1,src1size),
		       (src2,src2size))= getSrc2 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_ge, src1size/src2size",
		       fn () => src1size = src2size)
		in
		  AppendList.fromList
		  [Block.mkBlock'
		   {entry = NONE,
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
	     | Real_qequal _
	     => let
		  val (dst,dstsize) = getDst1 ()
		  val ((src1,src1size),
		       (src2,src2size))= getSrc2 ()
		  val _
		    = Assert.assert
		      ("applyPrim: Real_qequal, src1size/src2size",
		       fn () => src1size = src2size)
		in
		  AppendList.fromList
		  [Block.mkBlock'
		   {entry = NONE,
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
	     | Real_abs _ => funa Instruction.FABS
	     | Real_toInt (s, s')
	     => let
		  fun default () =
		    let
		      val (dst,dstsize) = getDst1 ()
		      val (src,srcsize) = getSrc1 ()
		    in
		      AppendList.fromList
		      [Block.mkBlock'
		       {entry = NONE,
			statements 
			= [Assembly.instruction_pfmovti
			   {dst = dst,
			    src = src,
			    srcsize = srcsize,
			    dstsize = dstsize}],
			transfer = NONE}]
		    end 
		  fun default' () =
		    let
		      val (dst,dstsize) = getDst1 ()
		      val (src,srcsize) = getSrc1 ()
		      val (tmp,tmpsize) =
			 (fildTempContentsOperand, Size.WORD)
		    in
		      AppendList.fromList
		      [Block.mkBlock'
		       {entry = NONE,
			statements 
			= [Assembly.instruction_pfmovti
			   {dst = dst,
			    src = src,
			    srcsize = srcsize,
			    dstsize = dstsize},
			   Assembly.instruction_xvom
			   {src = tmp,
			    dst = dst,
			    dstsize = dstsize,
			    srcsize = tmpsize}],
			transfer = NONE}]
		    end 
		in
		   case (s, IntSize.prim s') of
		      (R64, I64) => Error.bug "FIXME"
		    | (R64, I32) => default ()
		    | (R64, I16) => default ()
		    | (R64, I8) => default' ()
		    | (R32, I64) => Error.bug "FIXME"
		    | (R32, I32) => default ()
		    | (R32, I16) => default ()
		    | (R32, I8) => default' ()
		end
             | Real_toReal (s, s')
	     => let
		  val (dst,dstsize) = getDst1 ()
		  val (src,srcsize) = getSrc1 ()
		  fun mov () =
		     AppendList.fromList
		     [Block.mkBlock'
		      {entry = NONE,
		       statements 
		       = [Assembly.instruction_pfmov
			  {dst = dst,
			   src = src,
			   size = srcsize}],
		       transfer = NONE}]
		  fun movx () =
		     AppendList.fromList
		     [Block.mkBlock'
		      {entry = NONE,
		       statements 
		       = [Assembly.instruction_pfmovx
			  {dst = dst,
			   src = src,
			   srcsize = srcsize,
			   dstsize = dstsize}],
		       transfer = NONE}]
		  fun xvom () =
		     AppendList.fromList
		     [Block.mkBlock'
		      {entry = NONE,
		       statements 
		       = [Assembly.instruction_pfxvom
			  {dst = dst,
			   src = src,
			   srcsize = srcsize,
			   dstsize = dstsize}],
		       transfer = NONE}]
		in	
		   case (s, s') of
		      (R64, R64) => mov ()
		    | (R64, R32) => xvom ()
		    | (R32, R64) => movx ()
		    | (R32, R32) => mov ()
		end 
	     | Real_ldexp _ 
	     => let
		  val (dst,dstsize) = getDst1 ()
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
		  val realTemp1ContentsOperand = realTemp1ContentsOperand src1size
		in
		  AppendList.fromList
		  [Block.mkBlock'
		   {entry = NONE,
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
	     | Real_neg _ => funa Instruction.FCHS
	     | Real_round _ => funa Instruction.FRNDINT
	     | Word_add s => 
		(case WordSize.prim s of
		    W8 => binal Instruction.ADD
		  | W16 => binal Instruction.ADD
		  | W32 => binal Instruction.ADD
		  | W64 => binal64 (Instruction.ADD, Instruction.ADC))
	     | Word_andb s => 
		(case WordSize.prim s of
		    W8 => binal Instruction.AND
		  | W16 => binal Instruction.AND
		  | W32 => binal Instruction.AND
		  | W64 => binal64 (Instruction.AND, Instruction.AND))
	     | Word_arshift s => 
		(case WordSize.prim s of
		    W8 => sral Instruction.SAR
		  | W16 => sral Instruction.SAR
		  | W32 => sral Instruction.SAR
		  | W64 => Error.bug "FIXME")
	     | Word_div s => 
		(case WordSize.prim s of
		    W8 => pmd Instruction.DIV
		  | W16 => pmd Instruction.DIV
		  | W32 => pmd Instruction.DIV
		  | W64 => Error.bug "FIXME")
	     | Word_equal s => 
		(case WordSize.prim s of
		    W8 => cmp Instruction.E
		  | W16 => cmp Instruction.E
		  | W32 => cmp Instruction.E
		  | W64 => Error.bug "FIXME")
	     | Word_ge s => 
		(case WordSize.prim s of
		    W8 => cmp Instruction.AE
		  | W16 => cmp Instruction.AE
		  | W32 => cmp Instruction.AE
		  | W64 => Error.bug "FIXME")
	     | Word_gt s => 
		(case WordSize.prim s of
		    W8 => cmp Instruction.A
		  | W16 => cmp Instruction.A
		  | W32 => cmp Instruction.A
		  | W64 => Error.bug "FIXME")
	     | Word_le s => 
		(case WordSize.prim s of
		    W8 => cmp Instruction.BE
		  | W16 => cmp Instruction.BE
		  | W32 => cmp Instruction.BE
		  | W64 => Error.bug "FIXME")
	     | Word_lshift s => 
		(case WordSize.prim s of
		    W8 => sral Instruction.SHL
		  | W16 => sral Instruction.SHL
		  | W32 => sral Instruction.SHL
		  | W64 => Error.bug "FIXME")
	     | Word_lt s => 
		(case WordSize.prim s of
		    W8 => cmp Instruction.B
		  | W16 => cmp Instruction.B
		  | W32 => cmp Instruction.B
		  | W64 => Error.bug "FIXME")
	     | Word_mod s => 
		(case WordSize.prim s of
		    W8 => pmd Instruction.MOD
		  | W16 => pmd Instruction.MOD
		  | W32 => pmd Instruction.MOD
		  | W64 => Error.bug "FIXME")
	     | Word_mul s =>
		(case WordSize.prim s of
		    W8 => pmd Instruction.MUL
		  | W16 => imul2 ()
		  | W32 => imul2 ()
		  | W64 => Error.bug "FIXME")
	     | Word_neg s => 
		(case WordSize.prim s of
		    W8 => unal Instruction.NEG
		  | W16 => unal Instruction.NEG
		  | W32 => unal Instruction.NEG
		  | W64 => unal64 (Instruction.NEG, 
				   fn (dst,dstsize) => [Assembly.instruction_binal
							{dst = dst,
							 oper = Instruction.ADC,
							 src = Operand.immediate_const_int 0,
							 size = dstsize}]))
	     | Word_notb s => 
		(case WordSize.prim s of
		    W8 => unal Instruction.NOT
		  | W16 => unal Instruction.NOT
		  | W32 => unal Instruction.NOT
		  | W64 => unal64 (Instruction.NOT, fn _ => []))
	     | Word_orb s => 
		(case WordSize.prim s of
		    W8 => binal Instruction.OR
		  | W16 => binal Instruction.OR
		  | W32 => binal Instruction.OR
		  | W64 => binal64 (Instruction.OR, Instruction.OR))
	     | Word_rol s => 
		(case WordSize.prim s of
		    W8 => sral Instruction.ROL
		  | W16 => sral Instruction.ROL
		  | W32 => sral Instruction.ROL
		  | W64 => Error.bug "FIXME")
	     | Word_ror s => 
		(case WordSize.prim s of
		    W8 => sral Instruction.ROR
		  | W16 => sral Instruction.ROR
		  | W32 => sral Instruction.ROR
		  | W64 => Error.bug "FIXME")
	     | Word_rshift s => 
		(case WordSize.prim s of
		    W8 => sral Instruction.SHR
		  | W16 => sral Instruction.SHR
		  | W32 => sral Instruction.SHR
		  | W64 => Error.bug "FIXME")
	     | Word_sub s => 
		(case WordSize.prim s of
		    W8 => binal Instruction.SUB
		  | W16 => binal Instruction.SUB
		  | W32 => binal Instruction.SUB
		  | W64 => binal64 (Instruction.SUB, Instruction.SBB))
	     | Word_toInt (s, s') =>
		(case (WordSize.prim s, IntSize.prim s') of
		   (W64, I64) => Error.bug "FIXME"
		 | (W64, I32) => Error.bug "FIXME"
		 | (W64, I16) => Error.bug "FIXME"
		 | (W64, I8) => Error.bug "FIXME"
		 | (W32, I64) => Error.bug "FIXME"
		 | (W32, I32) => mov ()
		 | (W32, I16) => xvom ()
		 | (W32, I8) => xvom ()
		 | (W16, I64) => Error.bug "FIXME"
		 | (W16, I32) => movx Instruction.MOVZX
		 | (W16, I16) => mov ()
		 | (W16, I8) => xvom ()
		 | (W8, I64) => Error.bug "FIXME"
		 | (W8, I32) => movx Instruction.MOVZX
		 | (W8, I16) => movx Instruction.MOVZX
		 | (W8, I8) => mov ())
	     | Word_toIntX (s, s') =>
		(case (WordSize.prim s, IntSize.prim s') of
		   (W64, I64) => Error.bug "FIXME"
		 | (W64, I32) => Error.bug "FIXME"
		 | (W64, I16) => Error.bug "FIXME"
		 | (W64, I8) => Error.bug "FIXME"
		 | (W32, I64) => Error.bug "FIXME"
		 | (W32, I32) => mov ()
		 | (W32, I16) => xvom ()
		 | (W32, I8) => xvom ()
		 | (W16, I64) => Error.bug "FIXME"
		 | (W16, I32) => movx Instruction.MOVSX
		 | (W16, I16) => mov ()
		 | (W16, I8) => xvom ()
		 | (W8, I64) => Error.bug "FIXME"
		 | (W8, I32) => movx Instruction.MOVSX
		 | (W8, I16) => movx Instruction.MOVSX
		 | (W8, I8) => mov ())
	     | Word_toWord (s, s') =>
	        (case (WordSize.prim s, WordSize.prim s') of
		    (W64, W64) => Error.bug "FIXME"
		  | (W64, W32) => Error.bug "FIXME"
		  | (W64, W16) => Error.bug "FIXME"
		  | (W64, W8) => Error.bug "FIXME"
		  | (W32, W64) => Error.bug "FIXME"
		  | (W32, W32) => mov ()
		  | (W32, W16) => xvom ()
		  | (W32, W8) => xvom ()
		  | (W16, W64) => Error.bug "FIXME"
		  | (W16, W32) => movx Instruction.MOVZX
		  | (W16, W16) => mov ()
		  | (W16, W8) => xvom ()
		  | (W8, W64) => Error.bug "FIXME"
		  | (W8, W32) => movx Instruction.MOVZX
		  | (W8, W16) => movx Instruction.MOVZX
		  | (W8, W8) => mov ())
	     | Word_toWordX (s, s') =>
		(case (WordSize.prim s, WordSize.prim s') of
		    (W64, W64) => Error.bug "FIXME"
		  | (W64, W32) => Error.bug "FIXME"
		  | (W64, W16) => Error.bug "FIXME"
		  | (W64, W8) => Error.bug "FIXME"
		  | (W32, W64) => Error.bug "FIXME"
		  | (W32, W32) => mov ()
		  | (W32, W16) => xvom ()
		  | (W32, W8) => xvom ()
		  | (W16, W64) => Error.bug "FIXME"
		  | (W16, W32) => movx Instruction.MOVSX
		  | (W16, W16) => mov ()
		  | (W16, W8) => xvom ()
		  | (W8, W64) => Error.bug "FIXME"
		  | (W8, W32) => movx Instruction.MOVSX
		  | (W8, W16) => movx Instruction.MOVSX
		  | (W8, W8) => mov ())
	     | Word_xorb s => 
		(case WordSize.prim s of
		    W8 => binal Instruction.XOR
		  | W16 => binal Instruction.XOR
		  | W32 => binal Instruction.XOR
		  | W64 => binal64 (Instruction.XOR, Instruction.XOR))
	     | _ => Error.bug ("prim: strange Prim.Name.t: " ^ primName)),
	 comment_end]
      end

  fun ccall {args: (x86.Operand.t * x86.Size.t) vector,
	     frameInfo,
	     func,
	     return: x86.Label.t option,
	     transInfo = {...}: transInfo}
    = let
	val CFunction.T {convention, name, ...} = func
	val name =
	   if convention = CFunction.Convention.Stdcall
	      then
		 let
		    val argsSize =
		       Vector.fold (args, 0, fn ((_, s), ac) =>
				    ac + x86.Size.toBytes s)
		 in
		    concat [name, "@", Int.toString argsSize]
		 end
	   else name
	val comment_begin
	  = if !Control.Native.commented > 0
	      then AppendList.single (x86.Block.mkBlock'
				      {entry = NONE,
				       statements 
				       = [x86.Assembly.comment
					  ("begin ccall: " ^ name)],
				       transfer = NONE})
	    else AppendList.empty
      in
	AppendList.appends
	[comment_begin,
	 AppendList.single
	 (Block.mkBlock'
	  {entry = NONE,
	   statements = [],
	   transfer = SOME (Transfer.ccall 
			    {args = Vector.toList args,
			     frameInfo = frameInfo,
			     func = func,
			     return = return,
			     target = Label.fromString name})})]
      end

  fun creturn {dsts: (x86.Operand.t * x86.Size.t) vector,
	       frameInfo: x86.FrameInfo.t option,
	       func: CFunction.t,
	       label: x86.Label.t, 
	       transInfo = {live, liveInfo, ...}: transInfo}
    = let
	val name = CFunction.name func
	fun default ()
	  = let
	      val _ = x86Liveness.LiveInfo.setLiveOperands
		      (liveInfo, label, live label)
	    in 
	      AppendList.single
	      (x86.Block.mkBlock'
	       {entry = SOME (Entry.creturn {dsts = dsts,
					     frameInfo = frameInfo,
					     func = func,
					     label = label}),
		statements = [],
		transfer = NONE})
	    end
	val comment_end
	  = if !Control.Native.commented > 0
	      then (AppendList.single
		    (x86.Block.mkBlock' {entry = NONE,
				   statements = [x86.Assembly.comment 
						 ("end creturn: " ^ name)],
				   transfer = NONE}))
	      else AppendList.empty
      in
	AppendList.appends [default (), comment_end]
      end

  fun arith {prim : Prim.t,
	     args : (Operand.t * Size.t) vector,
	     dsts : (Operand.t * Size.t) vector,
	     overflow : Label.t,
	     success : Label.t,
	     transInfo = {live, liveInfo, ...} : transInfo}
    = let
	val primName = Prim.toString prim
	datatype z = datatype Prim.Name.t

	fun getDst1 ()
	  = Vector.sub (dsts, 0)
	    handle _ => Error.bug "arith: getDst1"
	fun getDst2 ()
	  = (Vector.sub (dsts, 0), Vector.sub (dsts, 1))
	    handle _ => Error.bug "arith: getDst2"
	fun getSrc1 ()
	  = Vector.sub (args, 0)
	    handle _ => Error.bug "arith: getSrc1"
	fun getSrc2 ()
	  = (Vector.sub (args, 0), Vector.sub (args, 1))
	    handle _ => Error.bug "arith: getSrc2"
	fun getSrc4 ()
	  = (Vector.sub (args, 0), Vector.sub (args, 1), 
	     Vector.sub (args, 2), Vector.sub (args, 3))
	    handle _ => Error.bug "arith: getSrc4"

	fun check (statements, condition)
	  = AppendList.single
	    (x86.Block.mkBlock'
	     {entry = NONE,	
	      statements = statements,
	      transfer = SOME (x86.Transfer.iff
			       {condition = condition,
				truee = overflow,
				falsee = success})})
	fun binal (oper: x86.Instruction.binal, condition)
	  = let
	      val (dst, dstsize) = getDst1 ()
	      val ((src1, src1size), (src2, src2size)) = getSrc2 ()
	      val _ = Assert.assert
		      ("arith: binal, dstsize/src1size/src2size",
		       fn () => src1size = dstsize andalso src2size = dstsize)
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
	      check ([Assembly.instruction_mov
		      {dst = dst,
		       src = src1,
		       size = dstsize},
		      Assembly.instruction_binal
		      {oper = oper,
		       dst = dst,
		       src = src2,
		       size = dstsize}],
		     condition)
	    end
	fun binal64 (oper1: x86.Instruction.binal, 
		     oper2: x86.Instruction.binal, 
		     condition)
	  = let
	      val ((dst1, dst1size), (dst2, dst2size)) = getDst2 ()
	      val ((src1, src1size), (src2, src2size),
		   (src3, src3size), (src4, src4size)) = getSrc4 ()
	      val _ = Assert.assert
		      ("arith: binal64, dst1size/dst2size/src1size/src2size/src3size/src4size",
		       fn () => src1size = dst1size andalso src3size = dst1size andalso
                                src2size = dst2size andalso src4size = dst2size andalso
                                dst1size = dst2size)
	    in
	      check ([Assembly.instruction_mov
		      {dst = dst1,
		       src = src1,
		       size = dst1size},
		      Assembly.instruction_mov
		      {dst = dst2,
		       src = src2,
		       size = dst2size},
		      Assembly.instruction_binal
		      {oper = oper1,
		       dst = dst1,
		       src = src3,
		       size = dst1size},
		      Assembly.instruction_binal
		      {oper = oper2,
		       dst = dst2,
		       src = src4,
		       size = dst2size}],
		     condition)
	    end
 	fun pmd (oper: x86.Instruction.md, condition)
  	  = let
	      val (dst, dstsize) = getDst1 ()
	      val ((src1, src1size), (src2, src2size)) = getSrc2 ()
	      val _ = Assert.assert
		      ("arith: pmd, dstsize/src1size/src2size",
		       fn () => src1size = dstsize andalso src2size = dstsize)
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
 	      check ([Assembly.instruction_mov
		      {dst = dst,
		       src = src1,
		       size = dstsize},
		      Assembly.instruction_pmd
		      {oper = oper,
		       dst = dst,
		       src = src2,
		       size = dstsize}],
		     condition)
 	    end
	fun unal (oper: x86.Instruction.unal, condition)
	  = let
	      val (dst, dstsize) = getDst1 ()
	      val (src1, src1size) = getSrc1 ()
	      val _ = Assert.assert
		      ("arith: unal, dstsize/src1size",
		       fn () => src1size = dstsize)
	    in
	      check ([Assembly.instruction_mov
		      {dst = dst,
		       src = src1,
		       size = dstsize},
		      Assembly.instruction_unal 
		      {oper = oper,
		       dst = dst,
		       size = dstsize}],
		     condition)
	    end

	fun neg64 ()
	  = let
	      val ((dst1, dst1size), (dst2, dst2size)) = getDst2 ()
	      val ((src1, src1size), (src2, src2size)) = getSrc2 ()
	      val _ = Assert.assert
		      ("arith: neg64, dst1size/dst2size/src1size/src2size",
		       fn () => src1size = dst1size andalso
		                src2size = dst2size andalso
				dst1size = dst2size)
	      val loZ = Label.newString "loZ"
	      val _ = x86Liveness.LiveInfo.setLiveOperands
		      (liveInfo, loZ, dst2::((live success) @ (live overflow)))
	      val loNZ = Label.newString "loNZ"
	      val _ = x86Liveness.LiveInfo.setLiveOperands
		      (liveInfo, loNZ, dst2::(live success))
	    in
	       AppendList.fromList
	       [x86.Block.mkBlock'
		{entry = NONE,
		 statements = [Assembly.instruction_mov
			       {dst = dst1,
				src = src1,
				size = dst1size},
			       Assembly.instruction_mov
			       {dst = dst2,
				src = src2,
				size = dst2size},
			       Assembly.instruction_unal 
			       {oper = x86.Instruction.NEG,
				dst = dst1,
				size = dst1size}],
                 transfer = SOME (x86.Transfer.iff
                                  {condition = x86.Instruction.Z,
				   truee = loZ,
				   falsee = loNZ})},
		x86.Block.mkBlock'
		{entry = SOME (x86.Entry.jump {label = loNZ}),
		 statements = [Assembly.instruction_unal
			       {dst = dst2,
				oper = Instruction.INC,
				size = dst2size},
			       Assembly.instruction_unal 
			       {oper = x86.Instruction.NEG,
				dst = dst2,
				size = dst2size}],
		 transfer = SOME (x86.Transfer.goto {target = success})},
		x86.Block.mkBlock'
		{entry = SOME (x86.Entry.jump {label = loZ}),
		 statements = [Assembly.instruction_unal 
			       {oper = x86.Instruction.NEG,
				dst = dst2,
				size = dst2size}],
		 transfer = SOME (x86.Transfer.iff
				  {condition = x86.Instruction.O,
				   truee = overflow,
				   falsee = success})}]
	    end

	fun imul2 condition
	  = let
	      val (dst, dstsize) = getDst1 ()
	      val ((src1, src1size), (src2, src2size)) = getSrc2 ()
	      val _ = Assert.assert
		      ("arith: imul2, dstsize/src1size/src2size",
		       fn () => src1size = dstsize andalso src2size = dstsize)
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
	      check ([Assembly.instruction_mov
		      {dst = dst,
		       src = src1,
		       size = dstsize},
		      Assembly.instruction_imul2
		      {dst = dst,
		       src = src2,
		       size = dstsize}],
		     condition)
	    end

	val (comment_begin,_)
	  = if !Control.Native.commented > 0
	      then let
		     val comment = primName
		   in 
		     (AppendList.single
		      (x86.Block.mkBlock'
		       {entry = NONE,
			statements 
			= [x86.Assembly.comment 
			   ("begin arith: " ^ comment)],
			transfer = NONE}),
		      AppendList.single
		      (x86.Block.mkBlock'
		       {entry = NONE,
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
	     Int_addCheck s => 
	       (case IntSize.prim s of
		  I8 => binal (x86.Instruction.ADD, x86.Instruction.O)
		| I16 => binal (x86.Instruction.ADD, x86.Instruction.O)
		| I32 => binal (x86.Instruction.ADD, x86.Instruction.O)
		| I64 => binal64 (x86.Instruction.ADD, x86.Instruction.ADC, x86.Instruction.O))
	   | Int_subCheck s => 
	       (case IntSize.prim s of
		  I8 => binal (x86.Instruction.SUB, x86.Instruction.O)
		| I16 => binal (x86.Instruction.SUB, x86.Instruction.O)
		| I32 => binal (x86.Instruction.SUB, x86.Instruction.O)
		| I64 => binal64 (x86.Instruction.SUB, x86.Instruction.SBB, x86.Instruction.O))
	   | Int_mulCheck s => 	
	       (case IntSize.prim s of
		  I8 => pmd (x86.Instruction.IMUL, x86.Instruction.O)
		| I16 => imul2 x86.Instruction.O
		| I32 => imul2 x86.Instruction.O
		| I64 => Error.bug "FIXME")
	   | Int_negCheck s => 
	       (case IntSize.prim s of
		  I8 => unal (x86.Instruction.NEG, x86.Instruction.O)
		| I16 => unal (x86.Instruction.NEG, x86.Instruction.O)
		| I32 => unal (x86.Instruction.NEG, x86.Instruction.O)
		| I64 => neg64 ())
	   | Word_addCheck s => 
	       (case WordSize.prim s of
		   W8 => binal (x86.Instruction.ADD, x86.Instruction.C)
		 | W16 => binal (x86.Instruction.ADD, x86.Instruction.C)
		 | W32 => binal (x86.Instruction.ADD, x86.Instruction.C)
		 | W64 => binal64 (x86.Instruction.ADD, x86.Instruction.ADC, x86.Instruction.C))
	   | Word_mulCheck s => 
	       (case WordSize.prim s of
		  W8 => pmd (x86.Instruction.MUL, x86.Instruction.C)
		| W16 => pmd (x86.Instruction.MUL, x86.Instruction.C)
		| W32 => pmd (x86.Instruction.MUL, x86.Instruction.C)
		| W64 => Error.bug "FIXME")
	   | _ => Error.bug ("arith: strange Prim.Name.t: " ^ primName))]
      end

end
