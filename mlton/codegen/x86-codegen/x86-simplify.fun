(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor x86Simplify(S: X86_SIMPLIFY_STRUCTS): X86_SIMPLIFY =
struct

  open S
  open x86

  val rec partition
    = fn ([],p) => ([],[])
       | (h::t,p) => let
		       val (truee,falsee) = partition(t, p)
		     in
		       if p h
			 then (h::truee,falsee)
			 else (truee,h::falsee)
		     end

  val rec ones : int -> word
    = fn 0 => 0wx0
       | n => Word.orb(Word.<<(ones (n-1), 0wx1),0wx1)

  val tracer
    = Control.traceBatch
(*
    = fn s => fn f => (Control.trace (Control.Detail, s) f, fn () => ())
*)

  structure JumpInfo =
    struct
      datatype status = Must | Maybe of int

      val lt
	= fn (Maybe i1, Must) => true
	   | (Maybe i1, Maybe i2) => i1 < i2
	   | _ => false
      val eq
	= fn (Must, Must) => true
           | (Maybe i1, Maybe i2) => i1 = i2
	   | _ => false

      val toString
        = fn Must => "Must"
	   | Maybe i => concat ["Maybe ", Int.toString i]

      local
	fun doit (status_ref, maybe_fn)
	  = case !status_ref
	      of Must => ()
	       | Maybe i => status_ref := (maybe_fn i)
      in
	fun inc {label, jumpInfo as {get}}
	  = doit (get label, fn i => Maybe (i+1))
	fun dec {label, jumpInfo as {get}}
	  = doit (get label, fn i => Maybe (i-1))
	fun force {label, jumpInfo as {get}}
	  = doit (get label, fn i => Must)
      end

      fun computeJumpInfo {blocks: Block.t list, 
			   exports: Label.t list,
			   block_pre: Label.t -> Assembly.t list option}
	= let
	    val jumpInfo as {get: Label.t -> status ref}
	      = Property.get
	        (Label.plist, 
		 Property.initFun 
		 (fn label
		   => case block_pre label 
			of NONE => if List.contains(exports,
						    label,
						    Label.equals)
				     then ref Must
				     else ref (Maybe 0) 
			 | SOME assembly => ref Must))

	    val _ 
	      = List.foreach
	        (blocks,
		 fn Block.T {transfer,...}
		  => List.foreach
		     (Transfer.targets transfer,
		      fn label 
		       => inc {label = label,
			       jumpInfo = jumpInfo}))
	  in
	    jumpInfo
	  end

      val (computeJumpInfo, computeJumpInfo_msg)
	= tracer
          "computeJumpInfo"
	  computeJumpInfo

      fun verifyJumpInfo {blocks: Block.t list, 
			  exports: Label.t list,
			  block_pre: Label.t -> Assembly.t list option,
			  jumpInfo: {get: Label.t -> status ref}}
	= let
	    val jumpInfo' = computeJumpInfo {blocks = blocks,
					     exports = exports,
					     block_pre = block_pre}

	    val verified 
	      = List.forall
	        (blocks,
		 fn Block.T {label,...}
		  => if !((#get jumpInfo') label) = !((#get jumpInfo) label)
		       then true
		       else false)
	  in
	    verified
	  end

      val (verifyJumpInfo, verifyJumpInfo_msg)
	= tracer
          "verifyJumpInfo"
	  verifyJumpInfo
    end

  structure PeepholeBlock =
    struct
      structure Peephole
	= Peephole(type label_type = Label.t
		   type profileInfo_type = ProfileInfo.t
		   type statement_type = Assembly.t
		   type transfer_type = Transfer.t
		   datatype block = datatype Block.t)
      open Peephole

      fun make_callback_msg name
	= let
	    val count = ref 0
	    val total = ref 0
	    val callback = fn true => (Int.inc count; Int.inc total)
	                    | false => Int.inc total
	    val msg = fn () => Control.messageStr 
	                       (Control.Detail,
				concat [name, 
					": ", Int.toString (!count),
					" / ", Int.toString (!total)])
	  in
	    (callback,msg)
	  end

      val isComment : statement_type -> bool
	= fn Assembly.Comment _
	   => true
	   | _ => false

      local
	val isInstructionMOV : statement_type -> bool
	  = fn Assembly.Instruction (Instruction.MOV
				     {...})
	     => true
	     | _ => false

	val isInstructionBinALMD : statement_type -> bool
	  = fn Assembly.Instruction (Instruction.BinAL {...})
	     => true
	     | Assembly.Instruction (Instruction.pMD {...})
	     => true
	     | _ => false

	val template : template
	  = {start = EmptyOrNonEmpty,
	     statements = [One isInstructionMOV,
			   All isComment,
			   One isInstructionBinALMD],
	     finish = EmptyOrNonEmpty,
	     transfer = fn _ => true}

	val rewriter : rewriter
	  = fn {label,
		profileInfo,
		start, 
		statements as
		[[Assembly.Instruction (Instruction.MOV
					{src = src1,
					 dst = dst1, 
					 size = size1})],
		 comments,
		 [Assembly.Instruction (Instruction.BinAL
					{oper = oper2,
					 src = src2,
					 dst = dst2,
					 size = size2})]],
		finish, 
		transfer}
	     => if Size.eq(size1, size2) andalso
	           Operand.eq(dst1, dst2) andalso
		   Operand.eq(src1, src2)
		  then let
			 val statements
			   = (Assembly.instruction_mov
			      {src = src1,
			       dst = dst1,
			       size = size1})::
			     (Assembly.instruction_binal
			      {oper = oper2,
			       src = dst1,
			       dst = dst2,
			       size = size1})::
			     finish

			 val statements
			   = List.fold(start,
				       List.concat [comments, 
						    statements],
				       op ::)
		       in
			 SOME (Block.T
			       {label = label,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		  else NONE
	     | {label,
		profileInfo,
		start, 
		statements as
		[[Assembly.Instruction (Instruction.MOV
					{src = src1,
					 dst = dst1, 
					 size = size1})],
		 comments,
		 [Assembly.Instruction (Instruction.pMD
					{oper = oper2,
					 src = src2,
					 dst = dst2,
					 size = size2})]],
		finish, 
		transfer}
	     => if Size.eq(size1, size2) andalso
	           Operand.eq(dst1, dst2) andalso
		   Operand.eq(src1, src2)
		  then let
			 val statements
			   = (Assembly.instruction_mov
			      {src = src1,
			       dst = dst1,
			       size = size1})::
			     (Assembly.instruction_pmd
			      {oper = oper2,
			       src = dst1,
			       dst = dst2,
			       size = size1})::
			     finish

			 val statements
			   = List.fold(start,
				       List.concat [comments, 
						    statements],
				       op ::)
		       in
			 SOME (Block.T
			       {label = label,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		  else NONE
	     | _ => Error.bug "Peephole: elimBinALMDDouble"

	val (callback,elimBinALMDDouble_msg) 
	  = make_callback_msg "elimBinALMDDouble"
      in
	val elimBinALMDDouble : optimization
	  = {template = template,
	     rewriter = rewriter,
	     callback = callback}
	val elimBinALMDDouble_msg = elimBinALMDDouble_msg
      end

      local
	val isInstructionFMOV : statement_type -> bool
	  = fn Assembly.Instruction (Instruction.pFMOV
				     {...})
	     => true
	     | _ => false

	val isInstructionFBinA : statement_type -> bool
	  = fn Assembly.Instruction (Instruction.pFBinA {...})
	     => true
	     | Assembly.Instruction (Instruction.pFBinAS {...})
	     => true
	     | Assembly.Instruction (Instruction.pFBinASP {...})
	     => true
	     | _ => false

	val template : template
	  = {start = EmptyOrNonEmpty,
	     statements = [One isInstructionFMOV,
			   All isComment,
			   One isInstructionFBinA],
	     finish = EmptyOrNonEmpty,
	     transfer = fn _ => true}

	val rewriter : rewriter
	  = fn {label,
		profileInfo,
		start, 
		statements as
		[[Assembly.Instruction (Instruction.pFMOV
					{src = src1,
					 dst = dst1, 
					 size = size1})],
		 comments,
		 [Assembly.Instruction (Instruction.pFBinA
					{oper = oper2,
					 src = src2,
					 dst = dst2,
					 size = size2})]],
		finish, 
		transfer}
	     => if Size.eq(size1, size2) andalso
	           Operand.eq(dst1, dst2) andalso
		   Operand.eq(src1, src2)
		  then let
			 val statements
			   = (Assembly.instruction_pfmov
			      {src = src1,
			       dst = dst1,
			       size = size1})::
			     (Assembly.instruction_pfbina
			      {oper = oper2,
			       src = dst1,
			       dst = dst2,
			       size = size1})::
			     finish

			 val statements
			   = List.fold(start,
				       List.concat [comments, 
						    statements],
				       op ::)
		       in
			 SOME (Block.T
			       {label = label,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		  else NONE
	     | {label,
		profileInfo, 
		start, 
		statements as
		[[Assembly.Instruction (Instruction.pFMOV
					{src = src1,
					 dst = dst1, 
					 size = size1})],
		 comments,
		 [Assembly.Instruction (Instruction.pFBinAS
					{oper = oper2,
					 src = src2,
					 dst = dst2,
					 size = size2})]],
		finish, 
		transfer}
	     => if Size.eq(size1, size2) andalso
	           Operand.eq(dst1, dst2) andalso
		   Operand.eq(src1, src2)
		  then let
			 val statements
			   = (Assembly.instruction_pfmov
			      {src = src1,
			       dst = dst1,
			       size = size1})::
			     (Assembly.instruction_pfbinas
			      {oper = oper2,
			       src = dst1,
			       dst = dst2,
			       size = size1})::
			     finish

			 val statements
			   = List.fold(start,
				       List.concat [comments,
						    statements],
				       op ::)
		       in
			 SOME (Block.T
			       {label = label,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		  else NONE
	     | {label,
	        profileInfo,
		start, 
		statements as
		[[Assembly.Instruction (Instruction.pFMOV
					{src = src1,
					 dst = dst1, 
					 size = size1})],
		 comments,
		 [Assembly.Instruction (Instruction.pFBinASP
					{oper = oper2,
					 src = src2,
					 dst = dst2,
					 size = size2})]],
		finish, 
		transfer}
	     => if Size.eq(size1, size2) andalso
	           Operand.eq(dst1, dst2) andalso
		   Operand.eq(src1, src2)
		  then let
			 val statements
			   = (Assembly.instruction_pfmov
			      {src = src1,
			       dst = dst1,
			       size = size1})::
			     (Assembly.instruction_pfbinasp
			      {oper = oper2,
			       src = dst1,
			       dst = dst2,
			       size = size1})::
			     finish

			 val statements
			   = List.fold(start,
				       List.concat [comments,
						    statements],
				       op ::)
		       in
			 SOME (Block.T
			       {label = label,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		  else NONE
	     | _ => Error.bug "Peephole: elimFltBinADouble"

	val (callback,elimFltBinADouble_msg) 
	  = make_callback_msg "elimFltBinADouble"
      in
	val elimFltBinADouble : optimization
	  = {template = template,
	     rewriter = rewriter,
	     callback = callback}
	val elimFltBinADouble_msg = elimFltBinADouble_msg
      end

      local
	val isInstructionMOV_srcImmediate : statement_type -> bool
	  = fn Assembly.Instruction (Instruction.MOV 
				     {src = Operand.Immediate immediate,
				      ...})
	     => true
	     | _ => false				     

	val isInstructionBinALMD_operCommute : statement_type -> bool
	  = fn Assembly.Instruction (Instruction.BinAL
				     {oper,
				      ...})
	     => ((oper = Instruction.ADD)
		 orelse
		 (oper = Instruction.ADC)
		 orelse
		 (oper = Instruction.AND)
		 orelse
		 (oper = Instruction.OR)
		 orelse
		 (oper = Instruction.XOR))
	        andalso
		(case (Operand.deMemloc src,
		       Operand.deMemloc dst)
		   of (SOME src, SOME dst)
		    => not (List.exists
			    (src::(MemLoc.utilized src),
			     fn memloc => MemLoc.mayAlias(memloc, dst)))
	            | _ => true)
	     | Assembly.Instruction (Instruction.pMD
				     {oper,
				      ...})
	     => ((oper = Instruction.IMUL)
		 orelse
		 (oper = Instruction.MUL))
		andalso
		(case (Operand.deMemloc src,
		       Operand.deMemloc dst)
		   of (SOME src, SOME dst)
		    => not (MemLoc.mayAlias(src, dst))
		       andalso
		       not (List.contains(MemLoc.utilized src, dst, MemLoc.eq))
	            | _ => true)
	     | _ => false

	val template : template
	  = {start = EmptyOrNonEmpty,
	     statements = [One isInstructionMOV_srcImmediate,
			   All isComment,
			   One isInstructionBinALMD_operCommute],
	     finish = EmptyOrNonEmpty,
	     transfer = fn _ => true}

	val rewriter : rewriter
	  = fn {label,
		profileInfo,
		start, 
		statements as
		[[Assembly.Instruction (Instruction.MOV
					{src = src1, 
					 dst = dst1, 
					 size = size1})],
		 comments,
		 [Assembly.Instruction (Instruction.BinAL
					{oper = oper2,
					 src = src2,
					 dst = dst2,
					 size = size2})]],
		finish, 
		transfer}
	     => if Size.eq(size1, size2) andalso
	           Operand.eq(dst1, dst2)
		  then case (src1, src2)
			 of (Operand.Immediate _, Operand.Immediate _)
			  => NONE
			  | (Operand.Immediate _, _)
			  => let
			       val statements
				 = (Assembly.instruction_mov
				    {src = src2,
				     dst = dst1,
				     size = size1})::
				   (Assembly.instruction_binal
				    {oper = oper2,
				     src = src1,
				     dst = dst2,
				     size = size2})::
				   finish

			       val statements
				 = List.fold(start,
					     List.concat [comments,
							  statements],
					     op ::)
			     in
			       SOME (Block.T
				     {label = label,
				      profileInfo = profileInfo,
				      statements = statements,
				      transfer = transfer})
			     end
			  | _ => NONE
		  else NONE
	     | {label,
		profileInfo,
		start, 
		statements as
		[[Assembly.Instruction (Instruction.MOV
					{src = src1, 
					 dst = dst1, 
					 size = size1})],
		 comments,
		 [Assembly.Instruction (Instruction.pMD
					{oper = oper2,
					 src = src2,
					 dst = dst2,
					 size = size2})]],
		finish, 
		transfer}
	     => if Size.eq(size1, size2) andalso
	           Operand.eq(dst1, dst2)
		  then case (src1, src2)
			 of (Operand.Immediate _, Operand.Immediate _)
			  => NONE
			  | (Operand.Immediate _, _)
			  => let
			       val statements
				 = (Assembly.instruction_mov
				    {src = src2,
				     dst = dst1,
				     size = size1})::
				   (Assembly.instruction_pmd
				    {oper = oper2,
				     src = src1,
				     dst = dst2,
				     size = size2})::
				   finish

			       val statements
				 = List.fold(start,
					     List.concat [comments,
							  statements],
					     op ::)
			     in
			       SOME (Block.T
				     {label = label,
				      profileInfo = profileInfo,
				      statements = statements,
				      transfer = transfer})
			     end
			  | _ => NONE
		  else NONE
	     | _ => Error.bug "Peephole: commuteBinALMD"

	val (callback,commuteBinALMD_msg) 
	  = make_callback_msg "commuteBinALMD"
      in
	val commuteBinALMD : optimization
	  = {template = template,
	     rewriter = rewriter,
	     callback = callback}
	val commuteBinALMD_msg = commuteBinALMD_msg
      end

      local
	val isImmediate0
	  = fn Immediate.Const (Immediate.Char #"\000") => true
	     | Immediate.Const (Immediate.Int 0) => true
	     | Immediate.Const (Immediate.Word 0wx0) => true
	     | _ => false

	val isInstructionMOV_srcImmediate0 : statement_type -> bool
	  = fn Assembly.Instruction (Instruction.MOV
				     {src = Operand.Immediate immediate,
				      ...})
	     => isImmediate0 immediate
	     | _ => false

	val isInstructionBinAL : statement_type -> bool
	  = fn Assembly.Instruction (Instruction.BinAL
				     {oper,
				      ...})
       	     => (case oper
		   of Instruction.ADC => false
		    | Instruction.SBB => false
		    | _ => true)
	     | _ => false

	val template : template
	  = {start = EmptyOrNonEmpty,
	     statements = [One isInstructionMOV_srcImmediate0,
			   All isComment,
			   One isInstructionBinAL],
	     finish = EmptyOrNonEmpty,
	     transfer = fn _ => true}

	val rewriter : rewriter
	  = fn {label,
		profileInfo,
		start,
		statements as
		[[Assembly.Instruction (Instruction.MOV
					{src = src1 as Operand.Immediate immediate, 
					 dst = dst1, 
					 size = size1})],
		 comments,
		 [Assembly.Instruction (Instruction.BinAL
					{oper = oper2,
					 src = src2,
					 dst = dst2,
					 size = size2})]],
		finish as [], 
		transfer as Transfer.Iff {condition = Instruction.O,
					  truee,
					  falsee}}
	     => if Size.eq(size1, size2) andalso
	           Operand.eq(dst1, dst2)
		  then let
			 val statements
			   = (fn l
			       => case oper2
				    of Instruction.ADD
				     => (Assembly.instruction_mov
					 {src = src2,
					  dst = dst1,
					  size = size1})::
				        l
			             | Instruction.SUB
				     => (Assembly.instruction_mov
					 {src = src2,
					  dst = dst1,
					  size = size1})::
					(Assembly.instruction_unal
					 {oper = Instruction.NEG,
					  dst = dst1,
					  size = size1})::
					l
			             | Instruction.AND
				     => (Assembly.instruction_mov
					 {src = src1,
					  dst = dst1,
					  size = size1})::
					l
			             | Instruction.OR
				     => (Assembly.instruction_mov
					 {src = src2,
					  dst = dst1,
					  size = size1})::
					l
			             | Instruction.XOR
				     => (Assembly.instruction_mov
					 {src = src2,
					  dst = dst1,
					  size = size1})::
					l
				     | _ => Error.bug "elimBinAL0L")
			     finish

			 val statements
			   = List.fold(start,
				       List.concat [comments, 
						    statements],
				       op ::)

			 val transfer
			   = case oper2
			       of Instruction.SUB => transfer
				| _ => Transfer.Goto {target = falsee}
		       in
			 SOME (Block.T
			       {label = label,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		  else NONE
	     | {label,
		profileInfo,
		start,
		statements as
		[[Assembly.Instruction (Instruction.MOV
					{src = src1 as Operand.Immediate immediate, 
					 dst = dst1, 
					 size = size1})],
		 comments,
		 [Assembly.Instruction (Instruction.BinAL
					{oper = oper2,
					 src = src2,
					 dst = dst2,
					 size = size2})]],
		finish as [], 
		transfer as Transfer.Iff {condition = Instruction.NO,
					  truee,
					  falsee}}
	     => if Size.eq(size1, size2) andalso
	           Operand.eq(dst1, dst2)
		  then let
			 val statements
			   = (fn l 
			       => case oper2
				    of Instruction.ADD
				     => (Assembly.instruction_mov
				         {src = src2,
					  dst = dst1,
					  size = size1})::
				        l
			             | Instruction.SUB
				     => (Assembly.instruction_mov
					 {src = src2,
					  dst = dst1,
					  size = size1})::
					(Assembly.instruction_unal
					 {oper = Instruction.NEG,
					  dst = dst2,
					  size = size2})::
					l
			             | Instruction.AND
				     => (Assembly.instruction_mov
					 {src = src1,
					  dst = dst1,
					  size = size1})::
					l
			             | Instruction.OR
				     => (Assembly.instruction_mov
					 {src = src2,
					  dst = dst1,
					  size = size1})::
					l
			             | Instruction.XOR
				     => (Assembly.instruction_mov
					 {src = src2,
					  dst = dst1,
					  size = size1})::
					l
				     | _ => Error.bug "elimBinAL0L")
                             finish

			 val statements
			   = List.fold(start,
				       List.concat [comments,
						    statements],
				       op ::)
			     
			 val transfer
			   = case oper2
			       of Instruction.SUB => transfer
				| _ => Transfer.Goto {target = truee}
		       in
			 SOME (Block.T
			       {label = label,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		  else NONE
	     | {label,
		profileInfo,
		start, 
		statements as
		[[Assembly.Instruction (Instruction.MOV
					{src = src1 as Operand.Immediate immediate, 
					 dst = dst1, 
					 size = size1})],
		 comments,
		 [Assembly.Instruction (Instruction.BinAL
					{oper = oper2,
					 src = src2,
					 dst = dst2,
					 size = size2})]],
		finish, 
		transfer}
	     => if Size.eq(size1, size2) andalso
	           Operand.eq(dst1, dst2)
		  then let
			 val statements
			   = (fn l 
			       => case oper2
				    of Instruction.ADD
				     => (Assembly.instruction_mov
					 {src = src2,
					  dst = dst1,
					  size = size1})::
				        l
			             | Instruction.SUB
				     => (Assembly.instruction_mov
					 {src = src2,
					  dst = dst1,
					  size = size1})::
					(Assembly.instruction_unal
					 {oper = Instruction.NEG,
					  dst = dst1,
					  size = size1})::
					l
			             | Instruction.AND
				     => (Assembly.instruction_mov
					 {src = src1,
					  dst = dst1,
					  size = size1})::
					l
			             | Instruction.OR
				     => (Assembly.instruction_mov
					 {src = src2,
					  dst = dst1,
					  size = size1})::
					l
			             | Instruction.XOR
				     => (Assembly.instruction_mov
					 {src = src2,
					  dst = dst1,
					  size = size1})::
					l
				     | _ => Error.bug "elimBinAL0L")
                             finish

			 val statements
			   = List.fold(start,
				       List.concat [comments,
						    statements],
				       op ::)
		       in
			 SOME (Block.T
			       {label = label,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		  else NONE
	     | _ => Error.bug "Peephole: elimBinAL0L"

	val (callback,elimBinAL0L_msg) 
	  = make_callback_msg "elimBinAL0L"
      in
	val elimBinAL0L : optimization
	  = {template = template,
	     rewriter = rewriter,
	     callback = callback}
	val elimBinAL0L_msg = elimBinAL0L_msg
      end

      local
	val isImmediate0
	  = fn Immediate.Const (Immediate.Char #"\000") => true
	     | Immediate.Const (Immediate.Int 0) => true
	     | Immediate.Const (Immediate.Word 0wx0) => true
	     | _ => false

	val isInstructionMOV : statement_type -> bool
	  = fn Assembly.Instruction (Instruction.MOV
				     {...})
	     => true
	     | _ => false

	val isInstructionBinAL_srcImmediate0 : statement_type -> bool
	  = fn Assembly.Instruction (Instruction.BinAL
				     {oper,
				      src = Operand.Immediate immediate,
				      ...})
       	     => (case oper
		   of Instruction.ADC => false
		    | Instruction.SBB => false
		    | _ => true)
                andalso
		isImmediate0 immediate
	     | _ => false

	val template : template
	  = {start = EmptyOrNonEmpty,
	     statements = [One isInstructionMOV,
			   All isComment,
			   One isInstructionBinAL_srcImmediate0],
	     finish = EmptyOrNonEmpty,
	     transfer = fn _ => true}

	val rewriter : rewriter
	  = fn {label,
		profileInfo,
		start, 
		statements as
		[[Assembly.Instruction (Instruction.MOV
					{src = src1,
					 dst = dst1, 
					 size = size1})],
		 comments,
		 [Assembly.Instruction (Instruction.BinAL
					{oper = oper2,
					 src = src2 as Operand.Immediate immediate,
					 dst = dst2,
					 size = size2})]],
		finish as [], 
		transfer as Transfer.Iff {condition = Instruction.O,
					  truee,
					  falsee}}
	     => if Size.eq(size1, size2) andalso
	           Operand.eq(dst1, dst2)
		  then let
			 val statements
			   = (fn l 
			       => case oper2
				    of Instruction.ADD
				     => (Assembly.instruction_mov
					 {src = src1,
					  dst = dst1,
					  size = size1})::
				        l
			             | Instruction.SUB
				     => (Assembly.instruction_mov
					 {src = src1,
					  dst = dst1,
					  size = size1})::
					l
			             | Instruction.AND
				     => (Assembly.instruction_mov
					 {src = src2,
					  dst = dst1,
					  size = size1})::
					l
			             | Instruction.OR
				     => (Assembly.instruction_mov
					 {src = src1,
					  dst = dst1,
					  size = size1})::
					l
			             | Instruction.XOR
				     => (Assembly.instruction_mov
					 {src = src1,
					  dst = dst1,
					  size = size1})::
					l
				     | _ => Error.bug "elimBinAL0R")
                             finish

			 val statements
			   = List.fold(start,
				       List.concat [comments,
						    statements],
				       op ::)

			 val transfer = Transfer.Goto {target = falsee}
		       in
			 SOME (Block.T
			       {label = label,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		  else NONE
	     | {label,
		profileInfo,
		start, 
		statements as
		[[Assembly.Instruction (Instruction.MOV
					{src = src1,
					 dst = dst1, 
					 size = size1})],
		 comments,
		 [Assembly.Instruction (Instruction.BinAL
					{oper = oper2,
					 src = src2 as Operand.Immediate immediate,
					 dst = dst2,
					 size = size2})]],
		finish as [], 
		transfer as Transfer.Iff {condition = Instruction.NO,
					  truee,
					  falsee}}
	     => if Size.eq(size1, size2) andalso
	           Operand.eq(dst1, dst2)
		  then let
			 val statements
			   = (fn l
			       => case oper2
				    of Instruction.ADD
				     => (Assembly.instruction_mov
				         {src = src1,
					  dst = dst1,
					  size = size1})::
				        l
			             | Instruction.SUB
				     => (Assembly.instruction_mov
					 {src = src1,
					  dst = dst1,
					  size = size1})::
					l
			             | Instruction.AND
				     => (Assembly.instruction_mov
					 {src = src2,
					  dst = dst1,
					  size = size1})::
					l
			             | Instruction.OR
				     => (Assembly.instruction_mov
					 {src = src1,
					  dst = dst1,
					  size = size1})::
					l
			             | Instruction.XOR
				     => (Assembly.instruction_mov
					 {src = src1,
					  dst = dst1,
					  size = size1})::
					l
				     | _ => Error.bug "elimBinAL0R")
                             finish

			 val statements
			   = List.fold(start,
				       List.concat [comments,
						    statements],
				       op ::)

			 val transfer = Transfer.Goto {target = truee}
		       in
			 SOME (Block.T
			       {label = label,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		  else NONE
	     | {label,
		profileInfo,
		start, 
		statements as
		[[Assembly.Instruction (Instruction.MOV
					{src = src1,
					 dst = dst1, 
					 size = size1})],
		 comments,
		 [Assembly.Instruction (Instruction.BinAL
					{oper = oper2,
					 src = src2 as Operand.Immediate immediate,
					 dst = dst2,
					 size = size2})]],
		finish, 
		transfer}
	     => if Size.eq(size1, size2) andalso
	           Operand.eq(dst1, dst2)
		  then let
			 val statements
			   = (fn l
			       => case oper2
				    of Instruction.ADD
				     => (Assembly.instruction_mov
					 {src = src1,
					  dst = dst1,
					  size = size1})::
				        l
			             | Instruction.SUB
				     => (Assembly.instruction_mov
					 {src = src1,
					  dst = dst1,
					  size = size1})::
					l
			             | Instruction.AND
				     => (Assembly.instruction_mov
					 {src = src2,
					  dst = dst1,
					  size = size1})::
					l
			             | Instruction.OR
				     => (Assembly.instruction_mov
					 {src = src1,
					  dst = dst1,
					  size = size1})::
					l
			             | Instruction.XOR
				     => (Assembly.instruction_mov
					 {src = src1,
					  dst = dst1,
					  size = size1})::
					l
				     | _ => Error.bug "elimBinAL0R")
                             finish

			 val statements
			   = List.fold(start,
				       List.concat [comments,
						    statements],
				       op ::)
		       in
			 SOME (Block.T
			       {label = label,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		  else NONE
	     | _ => Error.bug "Peephole: elimBinAL0R"

	val (callback,elimBinAL0R_msg) 
	  = make_callback_msg "elimBinAL0R"
      in
	val elimBinAL0R : optimization
	  = {template = template,
	     rewriter = rewriter,
	     callback = callback}
	val elimBinAL0R_msg = elimBinAL0R_msg
      end

      local
	val isImmediate1
	  = fn Immediate.Const (Immediate.Char #"\001") => true
	     | Immediate.Const (Immediate.Int 1) => true
	     | Immediate.Const (Immediate.Int ~1) => true
	     | Immediate.Const (Immediate.Word 0wx1) => true
	     | Immediate.Const (Immediate.Word 0wxFFFFFFFF) => true
	     | _ => false

	val getImmediate1
	  = fn Immediate.Const (Immediate.Char #"\001") => SOME false
	     | Immediate.Const (Immediate.Int 1) => SOME false
	     | Immediate.Const (Immediate.Int ~1) => SOME true
	     | Immediate.Const (Immediate.Word 0wx1) => SOME false
	     | Immediate.Const (Immediate.Word 0wxFFFFFFFF) => SOME true
	     | _ => NONE

	val isInstructionADDorSUB_srcImmediate1 : statement_type -> bool
	= fn Assembly.Instruction (Instruction.BinAL 
				   {oper,
				    src = Operand.Immediate immediate,
				    ...})
	   => (case oper
		 of Instruction.ADD => true
		  | Instruction.SUB => true
		  | _ => false)
	      andalso
	      isSome (getImmediate1 immediate)
	   | _ => false

	val template : template 
	  = {start = EmptyOrNonEmpty,
	     statements = [One isInstructionADDorSUB_srcImmediate1],
	     finish = EmptyOrNonEmpty,
	     transfer = fn _ => true}

	val rewriter : rewriter
	  = fn {label,
		profileInfo,
		start, 
		statements as
		[[Assembly.Instruction (Instruction.BinAL 
					{oper,
					 src = Operand.Immediate immediate, 
					 dst, 
					 size})]],
		finish, 
		transfer}
	     => let
		  val oper
		    = case (oper, getImmediate1 immediate)
		        of (Instruction.ADD, SOME false) => Instruction.INC
		         | (Instruction.ADD, SOME true ) => Instruction.DEC
		         | (Instruction.SUB, SOME false) => Instruction.DEC
		         | (Instruction.SUB, SOME true ) => Instruction.INC
			 | _ => Error.bug "elimAddSub1"

		  val statements
		    = (Assembly.instruction_unal
		       {oper = oper,
			dst = dst,
			size = size})::
		      finish

		  val statements
		    = List.fold(start,
				statements,
				op ::)
		in 
		  SOME (Block.T
			{label = label,
			 profileInfo = profileInfo,
			 statements = statements,
			 transfer = transfer})
		end
	     | _ => Error.bug "Peephole: elimAddSub1"
 
	val (callback,elimAddSub1_msg) 
	  = make_callback_msg "elimAddSub1"
      in
	val elimAddSub1: optimization
	  = {template = template,
	     rewriter = rewriter,
	     callback = callback}
	val elimAddSub1_msg = elimAddSub1_msg
      end

      local
	val rec log2'
	  = fn (0wx0, _) => NONE
	     | (w : Word32.word, i : int)
	     => if 0wx1 = Word32.andb(w, 0wx1)
		  then case w
			 of 0wx00000001 => SOME (i, false)
			  | 0wxFFFFFFFF => SOME (i, true)
			  | _ => NONE
		  else log2' (Word32.~>>(w, 0wx1), i + 1)
	fun log2 w = log2' (w, 0 : int)

	val isImmediatePow2
	  = fn Immediate.Const (Immediate.Char c) 
	     => isSome (log2 (Word.fromChar c))
	     | Immediate.Const (Immediate.Int i) 
	     => isSome (log2 (Word.fromInt i))
	     | Immediate.Const (Immediate.Word w) 
	     => isSome (log2 w)
	     | _ => false

	val getImmediateLog2
	  = fn Immediate.Const (Immediate.Char c) 
	     => log2 (Word.fromChar c)
	     | Immediate.Const (Immediate.Int i) 
	     => log2 (Word.fromInt i)
	     | Immediate.Const (Immediate.Word w)
	     => log2 w
	     | _ => NONE

	val isInstructionMULorDIV_srcImmediatePow2 : statement_type -> bool
	= fn Assembly.Instruction (Instruction.pMD 
				   {oper,
				    src = Operand.Immediate immediate,
				    ...})
	   => (case oper
		 of Instruction.IMUL => true
		  | Instruction.MUL => true
		  | Instruction.IDIV => true
		  | Instruction.DIV => true
		  | _ => false)
	      andalso
	      isImmediatePow2 immediate
	   | _ => false

	val template : template 
	  = {start = EmptyOrNonEmpty,
	     statements 
	     = [One isInstructionMULorDIV_srcImmediatePow2],
	     finish = EmptyOrNonEmpty,
	     transfer = fn _ => true}

	val rewriter : rewriter
	  = fn {label,
		profileInfo,
		start, 
		statements as
		[[Assembly.Instruction (Instruction.pMD 
					{oper = Instruction.IMUL, 
					 src = Operand.Immediate immediate, 
					 dst, 
					 size})]],
		finish as [], 
		transfer as Transfer.Iff {condition = Instruction.O,
					  truee,
					  falsee}}
	     => NONE
	     | {label,
		profileInfo,
		start, 
		statements as
		[[Assembly.Instruction (Instruction.pMD 
					{oper = Instruction.IMUL, 
					 src = Operand.Immediate immediate, 
					 dst, 
					 size})]],
		finish as [], 
		transfer as Transfer.Iff {condition = Instruction.NO,
					  truee,
					  falsee}}
	     => NONE
	     | {label,
		profileInfo,
		start, 
		statements as
		[[Assembly.Instruction (Instruction.pMD 
					{oper = Instruction.IMUL, 
					 src = Operand.Immediate immediate, 
					 dst, 
					 size})]],
		finish, 
		transfer}
	     => (case getImmediateLog2 immediate
		   of NONE => Error.bug "Peephole: elimMDPow2"
		    | SOME (0,false) 
		    => SOME (Block.T
			     {label = label,
			      profileInfo = profileInfo,
			      statements = List.fold(start,
						     finish,
						     op ::),
			      transfer = transfer})
		    | SOME (0,true)
		    => let
			 val statements
			   = (Assembly.instruction_unal
			      {oper = Instruction.NEG,
			       dst = dst,
			       size = size})::
			     finish

			 val statements
			   = List.fold(start, 
				       statements,
				       op ::)
		       in
			 SOME (Block.T
			       {label = label,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		    | SOME (i,b)
		    => if i < (8 * Size.toBytes size)
			 then let
				val statements
				  = (fn l
				      => (Assembly.instruction_sral
					  {oper = Instruction.SAL,
					   count = Operand.immediate_const_int i,
					   dst = dst,
					   size = size})::
				         (if b
					    then (Assembly.instruction_unal
						  {oper = Instruction.NEG,
						   dst = dst,
						   size = size})::
					         l
					    else l))
                                    finish

				val statements
				  = List.fold(start, 
					      statements,
					      op ::)
			      in
				SOME (Block.T
				      {label = label,
				       profileInfo = profileInfo,
				       statements = statements,
				       transfer = transfer})
			      end
			 else NONE)
	     | {label,
		profileInfo,
		start, 
		statements as
		[[Assembly.Instruction (Instruction.pMD 
					 {oper = Instruction.MUL, 
					  src = Operand.Immediate immediate, 
					  dst, 
					  size})]],
		finish, 
		transfer}
	     => (case getImmediateLog2 immediate
		   of NONE => Error.bug "Peephole: elimMDPow2"
		    | SOME (0,false) 
		    => SOME (Block.T
			     {label = label,
			      profileInfo = profileInfo,
			      statements = List.fold(start,
						     finish,
						     op ::),
			      transfer = transfer})
		    | SOME (i,false)
		    => if i < (8 * Size.toBytes size)
			 then let
				val statements
				  = (Assembly.instruction_sral
				     {oper = Instruction.SAL,
				      count = Operand.immediate_const_int i,
				      dst = dst,
				      size = size})::
				    finish

				val statements
				  = List.fold(start, 
					      statements,
					      op ::)
			      in 
				SOME (Block.T
				      {label = label,
				       profileInfo = profileInfo,
				       statements = statements,
				       transfer = transfer})
			      end
			 else NONE
		    | SOME (i,true)
		    => NONE)
	     | {label,
		profileInfo,
		start, 
		statements as
		[[Assembly.Instruction (Instruction.pMD
					{oper = Instruction.IDIV,
					 src = Operand.Immediate immediate, 
					 dst, 
					 size})]],
		finish, 
		transfer}
	     => (case getImmediateLog2 immediate
		   of NONE => Error.bug "Peephole: elimMDPow2"
		    | SOME (0,false) 
		    => SOME (Block.T
			     {label = label,
			      profileInfo = profileInfo,
			      statements = List.fold(start,
						     finish,
						     op ::),
			      transfer = transfer})
		    | SOME (0,true)
		    => let
			 val statements
			   = (Assembly.instruction_unal
			      {oper = Instruction.NEG,
			       dst = dst,
			       size = size})::
			     finish

			 val statements
			   = List.fold(start, 
				       statements,
				       op ::)
		       in
			 SOME (Block.T
			       {label = label,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		    | SOME (i,b)
		    => if i < (8 * Size.toBytes size)
			 then let
				val temp
				  = (Operand.memloc o MemLoc.imm)
				    {base = Immediate.const_int 0,
				     index = Immediate.const_int 0,
				     scale = Scale.Four,
				     size = Size.LONG,
				     commit = MemLoc.Commit.commit
				              {isTemp = true,
					       onFlush = false},
				     class = MemLoc.Class.new "Temp"}

				val width = 8 * Size.toBytes size

				val statements
				  = ((fn l
				       => (Assembly.instruction_mov
					   {src = dst,
					    dst = temp,
					    size = size})::
				          l) o
				     (fn l
				       => if i > 0
					    then (Assembly.instruction_sral
						  {oper = Instruction.SAR,
						   dst = temp,
						   count 
						   = Operand.immediate_const_int (i - 1),
						   size = size})::
					         l
					    else l) o
				     (fn l
				       => if i < width
					    then (Assembly.instruction_sral
						  {oper = Instruction.SHR,
						   dst = temp,
						   count 
						   = Operand.immediate_const_int (width - i),
						   size = size})::
					         l
					    else l) o
				     (fn l
				       => (Assembly.instruction_binal
					   {oper = Instruction.ADD,
					    src = temp,
					    dst = dst,
					    size = size})::
					  (Assembly.instruction_sral
					   {oper = Instruction.SAR,
					    count = Operand.immediate_const_int i,
					    dst = dst,
					    size = size})::
					  l) o
				     (fn l
				       => if b
					    then (Assembly.instruction_unal
						  {oper = Instruction.NEG,
						   dst = dst,
						   size = size})::
					          l
					     else l))
				    finish

				val statements
				  = List.fold(start,
					      statements,
					      op ::)
			      in 
				SOME (Block.T
				      {label = label,
				       profileInfo = profileInfo,
				       statements = statements,
				       transfer = transfer})
			      end
			 else NONE)
	     | {label,
		profileInfo,
		start, 
		statements as
		[[Assembly.Instruction (Instruction.pMD
					{oper = Instruction.DIV,
					 src = Operand.Immediate immediate, 
					 dst, 
					 size})]],
		finish, 
		transfer}
	     => (case getImmediateLog2 immediate
		   of NONE => Error.bug "Peephole: elimMDPow2"
		    | SOME (0,false) 
		    => SOME (Block.T
			     {label = label,
			      profileInfo = profileInfo,
			      statements = List.fold(start,
						     finish,
						     op ::),
			      transfer = transfer})
		    | SOME (i,false)
		    => if i < (8 * Size.toBytes size)
			 then let
				val statements
				  = (Assembly.instruction_sral
				     {oper = Instruction.SHR,
				      count = Operand.immediate_const_int i,
				      dst = dst,
				      size = size})::
				    finish

				val statements
				  = List.fold(start,
					      statements,
					      op ::)
			      in 
				SOME (Block.T
				      {label = label,
				       profileInfo = profileInfo,
				       statements = statements,
				       transfer = transfer})
			      end
			 else NONE
	            | SOME (i,true) => NONE)
 	     | _ => Error.bug "Peephole: elimMDPow2"

	val (callback,elimMDPow2_msg) 
	  = make_callback_msg "elimMDPow2"
      in
	val elimMDPow2 : optimization
	  = {template = template,
	     rewriter = rewriter,
	     callback = callback}
	val elimMDPow2_msg = elimMDPow2_msg
      end  

      local
	val isInstructionCMPorTEST
	  = fn Assembly.Instruction (Instruction.CMP _)
	     => true
	     | Assembly.Instruction (Instruction.TEST _)
	     => true
	     | _ => false

	val isTransfer_Iff
	  = fn Transfer.Iff {truee, falsee, ...}
	     => true
	     | _ => false

	val template 
	  = {start = EmptyOrNonEmpty,
	     statements = [One isInstructionCMPorTEST,
			   All isComment],
	     finish = Empty,
	     transfer = not o isTransfer_Iff}

	val rewriter 
	  = fn {label,
		profileInfo,
		start, 
		statements as
		[[Assembly.Instruction instruction],
		 comments],
		finish as [],
		transfer}
	     => let
		  val statements 
		    = List.fold(start, 
				comments,
				op ::)
		in 
		  SOME (Block.T {label = label,
				 profileInfo = profileInfo,
				 statements = statements,
				 transfer = transfer})
		end
	     | _ => Error.bug "elimCMPTST"

	val (callback,elimCMPTST_msg) 
	  = make_callback_msg "elimCMPTST"
      in
	val elimCMPTST : optimization
	  = {template = template,
	     rewriter = rewriter,
	     callback = callback}
	val elimCMPTST_msg = elimCMPTST_msg
      end

      local
	val optimizations_pre
	  = [commuteBinALMD,
(*
 	     elimBinAL0L,
	     elimBinAL0R,
*)
	     elimAddSub1,
	     elimMDPow2]
	val optimizations_pre_msg
	  = [commuteBinALMD_msg,
(*
	     elimBinAL0L_msg,
	     elimBinAL0R_msg,
*)
	     elimAddSub1_msg,
	     elimMDPow2_msg]

	val optimizations_post
	  = [elimBinALMDDouble,
	     elimFltBinADouble,
	     elimCMPTST]
	val optimizations_post_msg
	  = [elimBinALMDDouble_msg,
	     elimFltBinADouble_msg,
	     elimCMPTST_msg]
      in
	val (peepholeBlock_pre, peepholeBlock_pre_msg)
	  = tracer
	    "peepholeBlock_pre"
	    (fn block => peepholeBlock {optimizations = optimizations_pre,
					block = block})
	val peepholeBlock_pre_msg
	  = fn () => (peepholeBlock_pre_msg ();
		      Control.indent ();
		      List.foreach(optimizations_pre_msg, fn msg => msg ());
		      Control.unindent ())

	val (peepholeBlock_post, peepholeBlock_post_msg)
	  = tracer
	    "peepholeBlock_post"
	    (fn block => peepholeBlock {optimizations = optimizations_post,
					block = block})

	val peepholeBlock_post_msg
	  = fn () => (peepholeBlock_post_msg ();
		      Control.indent ();
		      List.foreach(optimizations_post_msg, fn msg => msg ());
		      Control.unindent ())
      end

      open JumpInfo

      val (callback_elimIff,elimIff_msg)
	= make_callback_msg "elimIff"
      fun makeElimIff {jumpInfo : {get: Label.t -> status ref}} :
	              optimization
	= let
	    val isComment
	      = fn Assembly.Comment _ => true
	         | _ => false

	    val isInstructionCMPorTEST
	      = fn Assembly.Instruction (Instruction.CMP _)
	         => true
	         | Assembly.Instruction (Instruction.TEST _)
	         => true
	         | _ => false

	    val isTransferIff_eqTargets
	      = fn Transfer.Iff {truee, falsee, ...}
	         => Label.equals(truee, falsee)
	         | _ => false

	    val template 
	      = {start = EmptyOrNonEmpty,
		 statements = [One isInstructionCMPorTEST,
			       All isComment],
		 finish = Empty,
		 transfer = isTransferIff_eqTargets}

	    val rewriter 
	      = fn {label,
		    profileInfo,
		    start, 
		    statements as
		    [statement,
		     comments],
		    finish as [],
		    transfer as Transfer.Iff {condition, truee, falsee}}
	         => let
		      val _ = dec {label = falsee,
				   jumpInfo = jumpInfo}
			
		      val statements 
			= List.fold(start, 
				    comments, 
				    op ::)
			
		      val transfer = Transfer.goto {target = truee}
		    in 
		      SOME (Block.T {label = label,
				     profileInfo = profileInfo,
				     statements = statements,
				     transfer = transfer})
		    end
	         | _ => Error.bug "elimIff"
	  in
	    {template = template,
	     rewriter = rewriter,
	     callback = callback_elimIff}
	  end

      val (callback_elimSwitchTest,elimSwitchTest_msg)
	= make_callback_msg "elimSwitchTest"
      fun makeElimSwitchTest {jumpInfo : {get: Label.t -> status ref}} :
	                     optimization
	= let
	    val isComment
	      = fn Assembly.Comment _ => true
	         | _ => false

	    val isTransferSwitch_testImmediateEval
	      = fn Transfer.Switch {test as Operand.Immediate immediate, ...}
	         => isSome (Immediate.eval immediate)
                 | _ => false

	    val template 
	      = {start = Empty,
		 statements = [All (fn _ => true)],
		 finish = Empty,
		 transfer = isTransferSwitch_testImmediateEval}

	    val rewriter 
	      = fn {label,
		    profileInfo,
		    start as [], 
		    statements as [statements'],
		    finish as [],
		    transfer as Transfer.Switch {test as Operand.Immediate immediate,
						 cases, 
						 default}}
	         => let
		      val statements = statements'
		      val test = valOf (Immediate.eval immediate)
		      val cases
			= Transfer.Cases.keepAll'
			  (cases,
			   fn k => k = test,
			   fn (c,target) 
			    => (dec {label = target,
				     jumpInfo = jumpInfo};
				(Word.fromInt o Char.ord) c),
			   fn (i,target) 
			    => (dec {label = target,
				     jumpInfo = jumpInfo};
				Word.fromInt i),
			   fn (w,target) 
			    => (dec {label = label,
				     jumpInfo = jumpInfo};
				w))
			  
		      val transfer
			= if Transfer.Cases.isEmpty cases
			    then Transfer.goto {target = default}
			  else if Transfer.Cases.isSingle cases
			    then let
				   val _ = dec {label = default,
						jumpInfo = jumpInfo}
				   val target
				     = Transfer.Cases.extract
				       (cases, 
					fn target => target)
				   val _ = inc {label = target,
						jumpInfo = jumpInfo}
				 in
				   Transfer.goto {target = target}
				 end
			  else Error.bug "elimSwitchTest"
		    in
		      SOME (Block.T {label = label,
				     profileInfo = profileInfo,
				     statements = statements,
				     transfer = transfer})
		    end
	         | _ => Error.bug "elimSwitchTest"
	  in
	    {template = template,
	     rewriter = rewriter,
	     callback = callback_elimSwitchTest}
	  end

      val (callback_elimSwitchCases,elimSwitchCases_msg)
	= make_callback_msg "elimSwitchCases"
      fun makeElimSwitchCases {jumpInfo : {get: Label.t -> status ref}} :
	                       optimization
	= let
	    val isComment
	      = fn Assembly.Comment _ => true
	         | _ => false

	    val isTransferSwitch_casesDefault
	      = fn Transfer.Switch {cases, default, ...}
	         => let
		      val n = Transfer.Cases.count
			      (cases, 
			       fn target => Label.equals(target, default))
		    in 
		      n > 0
		    end
		 | _ => false

	    val template 
	      = {start = Empty,
		 statements = [All (fn _ => true)],
		 finish = Empty,
		 transfer = isTransferSwitch_casesDefault}

	    val rewriter 
	      = fn {label,
		    profileInfo,
		    start as [], 
		    statements as [statements'],
		    finish as [],
		    transfer as Transfer.Switch {test, cases, default}}
	         => let
		      val statements = statements'
		      val cases
			= Transfer.Cases.keepAll
			  (cases,
			   fn target => if Label.equals(target, default)
					  then (dec {label = target,
						     jumpInfo = jumpInfo};
						false)
					  else true)

		      val (statements, transfer)
			= if Transfer.Cases.isEmpty cases
			    then (statements,
				  Transfer.goto {target = default})
			  else if Transfer.Cases.isSingle cases
			    then let
				   val (k,target)
				     = Transfer.Cases.extract'
				       (cases,
					fn (k,target) => (k,target),
					fn (c,target) 
					 => (Immediate.const_char c, target),
					fn (i,target)
					 => (Immediate.const_int i, target),
					fn (w,target)
					 => (Immediate.const_word w, target))
				   val size
				     = case Operand.size test
					 of SOME size => size
					  | NONE => Size.LONG
				 in 
				   (List.concat
				    [statements,
				     [Assembly.instruction_cmp
				      {src1 = test,
				       src2 = Operand.immediate k,
				       size = size}]],
				    Transfer.iff {condition = Instruction.E,
						  truee = target,
						  falsee = default})
				 end				    
			  else (statements,
				Transfer.switch {test = test,
						 cases = cases,
						 default = default})
		    in 
		      SOME (Block.T {label = label,	
				     profileInfo = profileInfo,
				     statements = statements,
				     transfer = transfer})
		    end
	         | _ => Error.bug "elimSwitchCases"
	  in
	    {template = template,
	     rewriter = rewriter,
	     callback = callback_elimSwitchCases}
	  end
    end

  structure ElimGoto =
    struct
      open JumpInfo

      fun elimSimpleGoto {blocks : Block.t list,
			  jumpInfo : {get: Label.t -> status ref}} 
	= let
	    val gotoInfo as {get: Label.t -> Label.t option,
			     set: Label.t * Label.t option -> unit}
	      = Property.getSet(Label.plist, Property.initConst NONE)
	    val changed = ref false
	      
	    val labels
	      = List.keepAllMap
	        (blocks,
		 fn block as Block.T {label, 
				      profileInfo,
				      statements, 
				      transfer as Transfer.Goto {target}}
		  => if List.forall(statements,
				    fn Assembly.Comment _ => true
				     | _ => false)
		        andalso
			not (Label.equals(label, target))
			andalso
			!((#get jumpInfo) label) <> Must
		       then (set(label, SOME target); SOME label)
		       else NONE
		  | _ => NONE)
		
	    fun loop ()
	      = if List.fold(labels,
			     false,
			     fn (label,b)
			      => case get label
				   of NONE => b
			            | SOME target
				    => (case get target
					  of NONE => b
					   | SOME target'
					   => (set(label, SOME target');
					       true)))
		  then loop ()
		  else ()
		    
	    val _ = loop ()

	    fun update target
	      = case get target
		  of SOME target'
		   => (changed := true;
		       dec {label = target,
			    jumpInfo = jumpInfo};
		       inc {label = target',
			    jumpInfo = jumpInfo};
		       target')
		   | NONE => target

	    val elimSimpleGoto'
	      = fn Transfer.Goto {target} 
	         => Transfer.Goto {target = update target}
		 | Transfer.Iff {condition, truee, falsee}
	         => Transfer.Iff {condition = condition,
				  truee = update truee,
				  falsee = update falsee}
	         | Transfer.Switch {test, cases, default}
	         => Transfer.Switch {test = test,
				     cases = Transfer.Cases.map
				             (cases,
					      fn target => update target),
			             default = update default}
	         | transfer => transfer

	    val blocks
	      = List.map
	        (blocks,
		 fn Block.T {label, profileInfo, statements, transfer}
		  => Block.T {label = label,
			      profileInfo = profileInfo,
			      statements = statements,
			      transfer = elimSimpleGoto' transfer})

	    val blocks
	      = List.removeAll
	        (blocks,
		 fn Block.T {label,...}
		  => (case get label
			of SOME label' => (changed := true;
					   dec {label = label',
						jumpInfo = jumpInfo};
					   true)
			 | NONE => false))
	  in
	    {blocks = blocks,
	     changed = !changed}
	  end

      val (elimSimpleGoto,elimSimpleGoto_msg)
	= tracer
	  "elimSimpleGoto"
	  elimSimpleGoto

      fun elimComplexGoto {blocks : Block.t list,
			   jumpInfo : {get: Label.t -> status ref}}
	= let
	    val gotoInfo as {get: Label.t -> Block.t option,
			     set: Label.t * Block.t option -> unit}
	      = Property.getSet(Label.plist, Property.initConst NONE)

	    val labels
	      = List.keepAllMap
	        (blocks,
		 fn block as Block.T {label,...}
		  => if !((#get jumpInfo) label) = Maybe 1
		       then (set(label, SOME block); SOME label)
		       else NONE)

	    fun loop ()
	      = if List.fold
	           (labels,
		    false,
		    fn (label,b)
		     => case get label
			  of SOME (Block.T 
				   {label,
				    profileInfo,
				    statements,
				    transfer as Transfer.Goto {target}})
			   => (if Label.equals(label,target)
				 then b
				 else (case get target
					 of NONE => b
					  | SOME (Block.T
						  {label = label',
						   profileInfo 
						   = profileInfo',
						   statements = statements',
						   transfer = transfer'})
					  => (set(label,
						  SOME (Block.T
							{label = label,
							 profileInfo
							 = ProfileInfo.combine
							   (profileInfo,
							    profileInfo'),
							 statements
							 = List.concat
							   [statements, 
							    statements'],
						         transfer 
						         = transfer'}));
					      true)))
		           | _ => b)
		  then loop ()
		  else ()

	    val _ = loop ()

	    val elimComplexGoto'
	      = fn block as Block.T {label, 
				     profileInfo,
				     statements, 
				     transfer as Transfer.Goto {target}}
		 => if Label.equals(label,target)
		      then block
		      else (case get target
			      of NONE => block
			       | SOME (Block.T {label = label',
						profileInfo = profileInfo',
						statements = statements',
						transfer = transfer'})
			       => let
				    val _ = dec {label = label',
						 jumpInfo = jumpInfo}
				    val _ = List.foreach
				            (Transfer.targets transfer',
					     fn target 
					      => inc {label = target, 
						      jumpInfo = jumpInfo})

				    val block
				      = Block.T {label = label,
						 profileInfo 
						 = ProfileInfo.combine
						   (profileInfo,
						    profileInfo'),
						 statements 
						 = List.concat
						   [statements, 
						    statements'],
						 transfer = transfer'}
				  in
				    block
				  end)
	         | block => block

	    val blocks
	      = List.map(blocks, elimComplexGoto')

	    fun loop {blocks, changed}
	      = let
		  val {blocks,changed'}
		    = List.foldr
		      (blocks,
		       {blocks = [], changed' = false},
		       fn (block as Block.T {label,transfer,...},
			   {blocks, changed'})
		        => if !((#get jumpInfo label)) = Maybe 0
			     then let
				    val _ = List.foreach
				            (Transfer.targets transfer,
					     fn target 
					      => dec {label = target,
						      jumpInfo = jumpInfo})
				  in
				    {blocks = blocks,
				     changed' = true}
				  end
			     else {blocks = block::blocks,
				   changed' = changed'})
		in
		  if changed'
		    then loop {blocks = blocks, changed = true}
		    else {blocks = blocks, changed = changed}
		end

	    val {blocks, changed} = loop {blocks = blocks, changed = false}
	  in
	    {blocks = blocks,
	     changed = changed}
	  end

      val (elimComplexGoto, elimComplexGoto_msg)
	= tracer
	  "elimComplexGoto"
	  elimComplexGoto

      fun elimGoto {blocks : Block.t list,
		    jumpInfo : {get: Label.t -> status ref}}
	= let
	    val elimIff 
	      = PeepholeBlock.makeElimIff {jumpInfo = jumpInfo}
	    val elimSwitchTest
	      = PeepholeBlock.makeElimSwitchTest {jumpInfo = jumpInfo}
	    val elimSwitchCases 
	      = PeepholeBlock.makeElimSwitchCases {jumpInfo = jumpInfo}

	    fun loop {blocks,changed}
	      = let
		  val {blocks,
		       changed = changed_elimSimpleGoto}
		    = elimSimpleGoto {blocks = blocks,
				      jumpInfo = jumpInfo}

		  val {blocks,
		       changed = changed_peepholeBlocks}
		    = PeepholeBlock.peepholeBlocks
		      {blocks = blocks,
		       optimizations = [elimIff,
					elimSwitchTest,
					elimSwitchCases]}
		in
		  if changed_elimSimpleGoto orelse changed_peepholeBlocks
		    then loop {blocks = blocks, changed = true}
		    else {blocks = blocks, changed = changed}
		end

	    val {blocks, 
		 changed = changed_loop} 
	      = loop {blocks = blocks, changed = false}

	    val {blocks,
		 changed = changed_elimComplexGoto} 
	      = elimComplexGoto {blocks = blocks,
				 jumpInfo = jumpInfo}
	  in
	    {blocks = blocks,
	     changed = changed_loop orelse changed_elimComplexGoto}
	  end

      val (elimGoto, elimGoto_msg)
	= tracer
	  "elimGoto"
	  elimGoto

      val elimGoto_msg 
	= fn () => (elimGoto_msg ();
		    Control.indent ();
		    PeepholeBlock.elimIff_msg ();
		    PeepholeBlock.elimSwitchTest_msg ();
		    PeepholeBlock.elimSwitchCases_msg ();
		    elimSimpleGoto_msg ();
		    elimComplexGoto_msg ();
		    Control.unindent ())
    end

  structure Liveness =
    struct

      type t = {liveOut: MemLoc.t list,
		dead: MemLoc.t list}

      fun toString {liveOut, dead}
	= let
	    fun doit (name, l, toString, s)
	      = List.fold(l, s,
			  fn (x, s)
			   => concat [name, toString x, "\n", s])
	  in
	    doit("liveOut: ", liveOut, MemLoc.toString,
	    doit("dead: ", dead, MemLoc.toString,
		 ""))
	  end
	
      fun eq({liveOut = liveOut1,
	      dead = dead1},
	     {liveOut = liveOut2,
	      dead = dead2})
	= List.equalsAsSet(liveOut1, liveOut2, MemLoc.eq)
	  andalso
	  List.equalsAsSet(dead1, dead2, MemLoc.eq)

      fun temp_uses_defs {uses : Operand.t list,
			  defs : Operand.t list}
	= let
	    val baseUses
	      = List.fold
	        (uses,
		 [],
		 fn (operand, baseUses)
		  => case Operand.deMemloc operand
		       of SOME memloc => if MemLoc.isTemp memloc
			                    andalso 
					    not (List.contains(baseUses,
							       memloc,
							       MemLoc.eq))
					   then memloc::baseUses
					   else baseUses
		        | NONE => baseUses)
		   
	    val tempUses
	      = let
		  fun doit (operands, tempUses)
		    = List.fold
		      (operands,
		       tempUses,
		       fn (operand, tempUses)
		        => case Operand.deMemloc operand
			     of SOME memloc
			      => List.fold(MemLoc.utilized memloc,
					   tempUses,
					   fn (memloc, tempUses)
					    => if MemLoc.isTemp memloc
					          andalso
						  not (List.contains(tempUses,
								     memloc,
								     MemLoc.eq))
						 then memloc::tempUses
						 else tempUses)
                              | NONE => tempUses)
		in
		  doit(defs, 
		  doit(uses, 
		       baseUses))
		end

	    val baseDefs
	      = List.fold
	        (defs,
		 [],
		 fn (operand, baseDefs)
		  => case Operand.deMemloc operand
		       of SOME memloc => if MemLoc.isTemp memloc
			                    andalso 
					    not (List.contains(baseDefs,
							       memloc,
							       MemLoc.eq))
					   then memloc::baseDefs
					   else baseDefs
			| NONE => baseDefs)
	    val tempDefs = baseDefs
	  in
	    {uses = tempUses,
	     defs = tempDefs}
	  end

      fun liveness {uses : MemLoc.t list,
		    defs : MemLoc.t list,
		    live : MemLoc.t list} : 
	           {info : t,
		    live : MemLoc.t list}
	= let
	    val liveOut = live

	    val liveIn
	      = let
		  fun doit (memlocs, liveIn)
		    = List.fold(memlocs,
				liveIn,
				fn (memloc,liveIn)
				 => if List.contains(defs,
						     memloc,
						     MemLoc.eq)
  				       orelse
				       List.contains(liveIn,
						     memloc,
						     MemLoc.eq)
				      then liveIn
				      else memloc::liveIn)
		in
		  doit(live,
		       uses)
		end

	    val dead
	      = let
		  fun doit (memlocs, dead)
		    = List.fold(memlocs,
				dead,
				fn (memloc,dead)
				 => if List.contains(liveOut,
						     memloc,
						     MemLoc.eq)
				       orelse
				       List.contains(dead,
						     memloc,
						     MemLoc.eq)
				      then dead
				      else memloc::dead)
		in
		  doit(liveIn,
		  doit(defs,
		       []))
		end
	  in
	    {info = {liveOut = liveOut,
		     dead = dead},
	     live = liveIn}
	  end

      fun livenessAssembly {assembly: Assembly.t,
			    live: MemLoc.t list} :
	                   {info: t,
			    live: MemLoc.t list}
	= let
	    val {uses,defs,...} = Assembly.uses_defs_kills assembly
	    val {uses,defs} = temp_uses_defs {uses = uses, defs = defs}
	  in 
	    liveness {uses = uses,
		      defs = defs,
		      live = live}
	  end 

      fun livenessTransfer' {transfer: Transfer.t,
			     live: MemLoc.t list} :
	                    {info: t,
			     live: MemLoc.t list}
	= let
	    val {uses,defs,...} = Transfer.uses_defs_kills transfer
	    val {uses,defs} = temp_uses_defs {uses = uses, defs = defs}
	  in 
	    liveness {uses = uses,
		      defs = defs,
		      live = live}
	  end

      fun livenessTransfer {transfer: Transfer.t,
			    liveInfo 
			    as {get: Label.t -> MemLoc.t list,
				set: Label.t * MemLoc.t list -> unit}} :
                           {info: t,
			    live: MemLoc.t list}
	= let
	    val targets = Transfer.targets transfer
	    val live = List.concatMap(targets, get)
	  in
	    livenessTransfer' {transfer = transfer,
			       live = live}
	  end

(* Memoize the transfer function.

      fun livenessBlockFn {block as Block.T {label, profileInfo,
					     statements, transfer}}
	= let
	    val {uses, defs, ...} = Transfer.uses_defs_kills transfer
	    val {uses, defs} = temp_uses_defs {uses = uses, defs = defs}

	    val {uses_block, defs_block}
	      = List.foldr
	        (statements,
		 {uses_block = uses,
		  defs_block = defs},
		 fn (asm,{uses_block,defs_block})
		  => let
		       val {uses,defs,...} 
			 = Assembly.uses_defs_kills asm
		       val {uses,defs} 
			 = temp_uses_defs {uses = uses, defs = defs}

		       val uses_block
			 = List.fold(uses_block,
				     uses,
				     fn (memloc,uses_block)
				      => if List.contains(defs,
							  memloc,
							  MemLoc.eq)
				            orelse
					    List.contains(uses_block,
							  memloc,
							  MemLoc.eq)
					   then uses_block
					   else memloc::uses_block)

		       val defs_block
			 = List.fold(defs,
				     defs_block,
				     fn (memloc,defs_block)
				      => if List.contains(defs_block,
							  memloc,
							  MemLoc.eq)
					   then defs_block
					   else memloc::defs_block)
							 
		     in
		       {uses_block = uses_block,
			defs_block = defs_block}
		     end)
	  in
	    fn live => liveness {uses = uses_block,
				 defs = defs_block,
				 live = live}
	  end

      fun verifyLiveInfo {blocks,
			  liveInfo
			  as {get: Label.t -> MemLoc.t list,
			      set: Label.t * MemLoc.t list -> unit}}
	= let 
	    val error = fn _ => Error.bug "getBlockInfo: livenessFn"
	    val blockInfo
	      as {get = getBlockInfo : 
		        Label.t -> 
			{pred: Label.t list ref,
			 succ: Label.t list ref,
			 topo: int ref,
			 livenessFn: (MemLoc.t list -> 
				      {info: t, live: MemLoc.t list}) ref}}
	      = Property.get
	        (Label.plist,
	         Property.initFun (fn label => {pred = ref [],
						succ = ref [],
						topo = ref ~1,
						livenessFn = ref error}))
	    val get_pred = (#pred o getBlockInfo)
	    val get_succ = (#succ o getBlockInfo)
	    val get_topo = (#topo o getBlockInfo)
	    val get_livenessFn = (#livenessFn o getBlockInfo)
	    val get_pred' = (! o #pred o getBlockInfo)
	    val get_succ' = (! o #succ o getBlockInfo)
	    val get_topo' = (! o #topo o getBlockInfo)
	    val get_livenessFn' = (! o #livenessFn o getBlockInfo)

	    val labels
	      = List.map
	        (blocks,
		 fn block' as Block.T {label,transfer,...}
		  => let
		       val {succ,topo,livenessFn,...} = getBlockInfo label
		       val targets = Transfer.targets transfer
		     in 		
		       succ := targets;
		       topo := 0;
		       livenessFn := livenessBlockFn {block = block'};
		       List.foreach
		       (targets,
			fn target => List.push(get_pred target, label));
		       label
		     end)

	    local
	      val todo = ref []
	      fun topo_order(x,y) = Int.compare(get_topo' x, get_topo' y)
	    in
	      fun add_todo x = todo := List.insert(!todo, x, topo_order)
	      fun push_todo x = todo := x::(!todo)
	      fun rev_todo () = todo := List.rev (!todo)
	      fun get_todo () 
		= (case !todo
		     of [] => NONE
		      | (x::todo') => (todo := todo';
				       SOME x))
	    end

	    local
	      val num = ref 1
	    in
	      fun topo_sort label
		= let
		    val {topo, pred, ...} = getBlockInfo label
		  in
		    if !topo = 0
		      then (topo := !num;
			    Int.inc num;
			    push_todo label;
			    List.foreach(!pred, topo_sort))
		      else ()
		  end
	      fun topo_root label
		= (get_topo label := !num;
		   Int.inc num;
		   push_todo label)
	    end

	    fun loop (labels, n)
	      = if List.isEmpty labels
		  then ()
		  else let
			 val (exits,labels)
			   = partition
			     (labels,
			      fn label
			       => let
				    val succ = get_succ' label
				    val succ' 
				      = List.removeAll
				        (succ,
					 fn target 
					  => get_topo' target = ~1)
				  in
				    List.length succ' = n
				  end)
			 val exits
			   = List.removeAll
			     (exits,
			      fn label => get_topo' label <> 0)
			 val _ 
			   = (List.foreach
			      (exits, 
			       fn label => topo_root label);
			      List.foreach
			      (exits,
			       fn label 
			        => List.foreach(get_pred' label, topo_sort)))
		       in
			 loop(labels, n + 1)
		       end
	    val _ = loop(labels, 0)
	    val _ = rev_todo ()

	    val changed = ref false
	    fun doit ()
	      = (case get_todo ()
		   of NONE => ()
		    | SOME label
		    => let
			 val {pred, succ, livenessFn, ...} = getBlockInfo label

			 val live = List.concatMap
			            (!succ,
				     fn label => case get label
						    of SOME live => live
						     | NONE => [])
				    
			 val {live,...} = (!livenessFn) live

			 val live' = case get label
				       of SOME live' => live'
					| NONE => []
		       in
			 if List.equalsAsSet(live,live',MemLoc.eq)
			   then ()
			   else (set(label, SOME live);
				 List.foreach(!pred, add_todo);
(*
				 print "Liveness: verifyLiveInfo: ";
				 print (Label.toString label);
			         print "\n";
			         print "backend: ";
			         List.foreach
			         (live',
			          fn m 
			           => (print (MemLoc.toString m);
			         print " "));
			         print "\n";
			         print "calculated: ";
				 List.foreach
			         (live,
			          fn m 
			           => (print (MemLoc.toString m);
			       	 print " "));
			         print "\n";
*)
				 changed := true);
			 doit ()
		       end)
	    val _ = doit ()
	  in
	    {changed = !changed}
	  end
*)

      fun verifyLiveInfo {blocks,
			  liveInfo
			  as {get: Label.t -> MemLoc.t list,
			      set: Label.t * MemLoc.t list -> unit}}
	= let 
	    val blockInfo
	      as {get = getBlockInfo : 
		        Label.t -> {pred: Label.t list ref,
				    block: Block.t option ref,
				    topo: int ref}}
	      = Property.get
	        (Label.plist,
	         Property.initFun (fn label => {pred = ref [],
						block = ref NONE,
						topo = ref ~1}))
	    val get_pred = (#pred o getBlockInfo)
	    val get_block = (#block o getBlockInfo)
	    val get_topo = (#topo o getBlockInfo)
	    val get_pred' = (! o #pred o getBlockInfo)
	    val get_block' = (! o #block o getBlockInfo)
	    val get_topo' = (! o #topo o getBlockInfo)

	    val labels
	      = List.map
	        (blocks,
		 fn block' as Block.T {label,transfer,...}
		  => let
		       val {block,topo,...} = getBlockInfo label
		       val targets = Transfer.targets transfer
		     in 
		       block := SOME block';
		       topo := 0;
		       List.foreach
		       (targets,
			fn target => List.push(get_pred target, label));
		       label
		     end)

	    local
	      val todo = ref []
	      fun topo_order(x,y) = Int.<=(get_topo' x, get_topo' y)
	    in
	      fun add_todo x = todo := List.insert(!todo, x, topo_order)
	      fun push_todo x = todo := x::(!todo)
	      fun rev_todo () = todo := List.rev (!todo)
	      fun get_todo () 
		= (case !todo
		     of [] => NONE
		      | (x::todo') => (todo := todo';
				       SOME x))
	    end

	    local
	      val num = Counter.new 1
	    in
	      fun topo_sort label
		= let
		    val {topo, pred, ...} = getBlockInfo label
		  in
		    if !topo = 0
		      then (topo := Counter.next num;
			    push_todo label;
			    List.foreach(!pred, topo_sort))
		      else ()
		  end
	      fun topo_root label
		= (get_topo label := Counter.next num;
		   push_todo label)
	    end

	    fun loop (labels, n)
	      = if List.isEmpty labels
		  then ()
		  else let
			 val (exits,labels)
			   = partition
			     (labels,
			      fn label
			       => let
				    val Block.T {transfer,...} 
				      = valOf (get_block' label)
				    val targets = Transfer.targets transfer

				    val targets'
				      = List.fold(targets,
						  0,
						  fn (target,targets')
						   => if get_topo' target = ~1
							then targets'
							else targets' + 1)
				  in
				    targets' = n
				  end)
			 val exits
			   = List.removeAll
			     (exits,
			      fn label => get_topo' label <> 0)
			 val _ 
			   = (List.foreach
			      (exits, 
			       fn label => topo_root label);
			      List.foreach
			      (exits,
			       fn label 
			        => List.foreach(get_pred' label, topo_sort)))
		       in
			 loop(labels, n + 1)
		       end
	    val _ = loop(labels, 0)
	    val _ = rev_todo ()

	    val changed = ref false
	    fun doit ()
	      = (case get_todo ()
		   of NONE => ()
		    | SOME label
		    => let
			 val {pred, block, ...} = getBlockInfo label
			 val (Block.T {statements, transfer,...}) 
			   = valOf (!block)

			 val {live,...}
			   = livenessTransfer {transfer = transfer,
					       liveInfo = liveInfo}
			   
			 val live
			   = List.foldr
			     (statements,
			      live,
			      fn (asm,live)
			       => let
				    val {live,...}
				      = livenessAssembly 
				        {assembly = asm,
					 live = live}
				  in
				    live
				  end)
			     
			 val live' = get label
		       in
			 if List.equalsAsSet(live,live',MemLoc.eq)
			   then ()
			   else (set(label, live);
				 List.foreach(!pred, add_todo);
(*
				 print "Liveness: verifyLiveInfo: ";
				 print (Label.toString label);
			         print "\n";
			         print "backend: ";
			         List.foreach
			         (live',
			          fn m 
			           => (print (MemLoc.toString m);
			         print " "));
			         print "\n";
			         print "calculated: ";
				 List.foreach
			         (live,
			          fn m 
			           => (print (MemLoc.toString m);
			         print " "));
			         print "\n";
*)
				 changed := true);
			 doit ()
		       end)
	    val _ = doit ()
	  in
	    {changed = !changed}
	  end

      val (verifyLiveInfo :
	   {blocks: Block.t list, 
	    liveInfo: {get: Label.t -> MemLoc.t list,
		       set: Label.t * MemLoc.t list -> unit}} ->
	   {changed: bool},
	   verifyLiveInfo_msg)
	= tracer
          "verifyLiveInfo"
	  verifyLiveInfo
    end

  structure LivenessBlock =
    struct

      datatype t = T of {label: Label.t,
			 profileInfo: ProfileInfo.t,
			 statements: (Assembly.t * Liveness.t) list,
			 transfer: Transfer.t * Liveness.t}

      fun toString (T {label, profileInfo, statements, transfer})
	= concat [Label.toString label,
		  ":\n",
		  List.fold
		  (statements,
		   "",
		   fn ((asm,info),s)
		   => concat [s,
			      Assembly.toString asm,
			      "\n",
			      Liveness.toString info]),
		  let
		    val (trans,info) = transfer
		  in
		    concat[Transfer.toString trans,
			   "\n",
			   Liveness.toString info]
		  end]

      fun print_block (T {label, profileInfo, statements, transfer})
	= (print (Label.toString label);
	   print ":\n";
	   List.foreach
	   (statements,
	    fn (asm,info)
	     => (print (Assembly.toString asm);
		 print "\n";
		 print (Liveness.toString info)));
	   let
	     val (trans,info) = transfer
	   in
	     print (Transfer.toString trans);
	     print "\n";
	     print (Liveness.toString info)
	   end)

      fun toLivenessStatements {statements,
				live}
	= let
	    val {statements,live}
	      = List.foldr(statements,
			   {statements = [], live = live},
			   fn (asm,{statements,live})
			    => let
				 val {info, live}
				   = Liveness.livenessAssembly 
				     {assembly = asm,
				      live = live}
			       in
				 {statements = (asm, info)::statements, 
				  live = live}
			       end)
	  in
	    {statements = statements,
	     live = live}
	  end

      fun reLivenessStatements {statements: (Assembly.t * Liveness.t) list,
				live}
	= let
	    val {statements,live,...}
	      = List.foldr(statements,
			   {statements = [], 
			    live = live, 
			    continue = false},
			   fn ((asm,info),{statements,live,continue})
			   => if continue
				then {statements = (asm,info)::statements,
				      live = [],
				      continue = continue}
				else let
				       val {info = info', live = live'}
					 = Liveness.livenessAssembly 
					   {assembly = asm,
					    live = live}
				     in
				       {statements = (asm, info')::statements, 
					live = live',
					continue = Liveness.eq(info,info')}
				     end)
	  in
	    {statements = statements}
	  end

      fun toLivenessTransfer {transfer,
			      liveInfo}
	= let
	    val {info, live}
	      = Liveness.livenessTransfer {transfer = transfer,
					   liveInfo = liveInfo}
	  in
	    {transfer = (transfer,info),
	     live = live}
	  end

      fun reLivenessTransfer {transfer: Transfer.t * Liveness.t}
	= let
	    val (transfer,{liveOut,...}) = transfer
	    val {info, live} = Liveness.livenessTransfer' {transfer = transfer,
							   live = liveOut}
	  in
	    {transfer = (transfer, info),
	     live = live}
	  end

      fun toLivenessBlock {block as Block.T {label, profileInfo,
					     statements, transfer},
			   liveInfo : {get: Label.t -> MemLoc.t list,
				       set: Label.t * MemLoc.t list -> unit}}

	= let
	    val {transfer, live}
	      = toLivenessTransfer {transfer = transfer,
				    liveInfo = liveInfo}

	    val {statements, live}
	      = toLivenessStatements {statements =statements,
				      live = live}

	    val liveness_block
	      = T {label = label,
		   profileInfo = profileInfo,
		   statements = statements,
		   transfer = transfer}
	  in 
	    liveness_block
	  end

      val (toLivenessBlock:
	   {block: Block.t,
	    liveInfo: {get: Label.t -> MemLoc.t list,
	               set: Label.t * MemLoc.t list -> unit}} ->
	   t,
	   toLivenessBlock_msg)
	= tracer
	  "toLivenessBlock"
          toLivenessBlock

      fun verifyLivenessStatements {statements,
				    live}
	= List.foldr(statements,
		     {verified = true, live = live},
		     fn ((asm,info),{verified, live})
		      => let
			   val {info = info', live}
			     = Liveness.livenessAssembly 
			       {assembly = asm,
				live = live}
			 in
			   {verified = verified andalso 
			               Liveness.eq(info, info'), 
			    live = live}
			 end)

      fun verifyLivenessTransfer {transfer = (transfer,info),
				  liveInfo}
	= let
	    val {info = info', live}
	      = Liveness.livenessTransfer {transfer = transfer,
					   liveInfo = liveInfo}
	  in
	    {verified = Liveness.eq(info, info'),
	     live = live}
	  end

      fun verifyLivenessBlock {block as T {label, profileInfo,
					   statements, transfer},
			       liveInfo 
			       as {get: Label.t -> MemLoc.t list,
				   set: Label.t * MemLoc.t list -> unit}}
	= let
	    val {verified = verified_transfer,
		 live}
	      = verifyLivenessTransfer {transfer = transfer,
					liveInfo = liveInfo}

	    val {verified = verified_statements,
		 live}
	      = verifyLivenessStatements {statements =statements,
					  live = live}

(* FIXME -- the live-in set changed
	    val live' = get label

	    val verified_live = List.equalsAsSet(live, live', MemLoc.eq)
*)
	    val verified_live = true
	  in 
	    verified_transfer andalso 
	    verified_statements andalso
	    verified_live
	  end

      val (verifyLivenessBlock:
	   {block: t,
	    liveInfo: {get: Label.t -> MemLoc.t list,
	               set: Label.t * MemLoc.t list -> unit}} ->
	   bool,
	   verifyLivenessBlock_msg)
	= tracer
	  "verifyLivenessBlock"
          verifyLivenessBlock

      fun toBlock (T {label, profileInfo, statements, transfer})
	= let
	    val statements = List.map(statements, fn (asm,info) => asm)
	    val (transfer,info) = transfer
	  in 
	    Block.T {label = label,
		     profileInfo = profileInfo,
		     statements = statements,
		     transfer = transfer}
	  end

      val (toBlock: t -> Block.t, 
	   toBlock_msg)
	= tracer
	  "toBlock"
          toBlock
    end

  structure MoveHoistLivenessBlock = 
    struct
      fun moveHoist {block as LivenessBlock.T 
		              {label, profileInfo, statements, transfer}}
	= let
	    val {transfer,live} 
	      = LivenessBlock.reLivenessTransfer {transfer = transfer}

	    val {statements, changed, moves, live}
	      = List.foldr
	        (statements,
		 {statements = [],
		  changed = false,
		  moves = [],
		  live = live},
		 fn ((asm,info as {dead,...}),
		     {statements,changed,moves,live})
		  => let
		       fun default ()
			 = let
			     val {uses,defs,...} = Assembly.uses_defs_kills asm

			     val baseUses
			       = List.fold
			         (uses,
				  [],
				  fn (operand,baseUses)
				   => case Operand.deMemloc operand
					of SOME memloc
					 => if List.contains
					       (baseUses,
						memloc,
						MemLoc.eq)
					      then baseUses
					      else memloc::baseUses
				         | NONE => baseUses)
			     val baseDefs
			       = List.fold
			         (defs,
				  [],
				  fn (operand,baseDefs)
				   => case Operand.deMemloc operand
					of SOME memloc
					 => if List.contains
					       (baseDefs,
						memloc,
						MemLoc.eq)
					      then baseDefs
					      else memloc::baseDefs
				         | NONE => baseDefs)

			     val allUses
			       = let
				   fun doit(memlocs,allUses)
				     = List.fold
				       (memlocs,
					allUses,
					fn (memloc,allUses)
					 => List.fold
					    (MemLoc.utilized memloc,
					     allUses,
					     fn (memloc,allUses)
					      => if List.contains
					            (allUses,
						     memloc,
						     MemLoc.eq)
						   then allUses
						   else memloc::allUses))
				 in
				   doit(baseDefs,
				   doit(baseUses,
					baseUses))
				 end
			     val allDefs = baseDefs

			     val {forces,
				  moves,
				  ...}
			       = List.fold
			         (moves,
				  {forces = [],
				   moves = [],
				   allUses = allUses,
				   allDefs = allDefs},
				  fn (move as {src,dst,...},
				      {forces,
				       moves,
				       allUses,
				       allDefs})
				   => let
					val utilized_src
					  = MemLoc.utilized src
					val utilized_dst
					  = MemLoc.utilized dst
				      in 
					if List.exists
				           (allDefs,
					    fn memloc'
					     => List.exists
					        (src::utilized_src,
						 fn memloc'' 
						  => MemLoc.mayAlias
						     (memloc', memloc'')))
					   orelse
					   List.exists
					   (allDefs,
					    fn memloc'
					     => List.exists
					        (dst::utilized_dst,
						 fn memloc'' 
						  => MemLoc.mayAlias
						     (memloc', memloc'')))
					   orelse
					   List.exists
					   (allUses,
					    fn memloc'
					     => MemLoc.mayAlias
					        (memloc',dst) 
						orelse
						MemLoc.mayAlias
						(memloc',src))
					  then {forces = move::forces,
						moves = moves,
						allUses
						= src::(List.concat
							[utilized_src,
							 utilized_dst,
							 allUses]),
						allDefs 
						= dst::allDefs}
					  else {forces = forces,
						moves = move::moves,
						allUses = allUses,
						allDefs = allDefs}
				      end)

			     val moves 
			       = List.revMap
			         (moves,
				  fn move as {src,dst,size,age}
				   => {src = src,
				       dst = dst,
				       size = size,
				       age = age + 1})
			       
			     val statements_forces
			       = List.revMap
			         (forces,
				  fn force as {src,dst,size,...}
				   => (case Size.class size
					 of Size.INT
					  => Assembly.instruction_mov
					     {src = Operand.memloc src,
					      dst = Operand.memloc dst,
					      size = size}
					  | _
					  => Assembly.instruction_pfmov
					     {src = Operand.memloc src,
					      dst = Operand.memloc dst,
					      size = size}))

			     val {statements = statements_asm_forces,
				  live}
			       = LivenessBlock.toLivenessStatements
			         {statements = asm::statements_forces,
				  live = live}
			   in
			     {statements 
			      = List.concat
			        [statements_asm_forces,
				 statements],
			      changed 
			      = changed 
			        orelse
				List.exists(forces,
					    fn force as {age,...}
					     => age <> 0),
			      moves = moves,
			      live = live}
			   end

		       fun force ()
			 = let
			     val forces = moves
			     val statements_forces
			       = List.map
			         (forces,
				  fn force as {src,dst,size,...}
				   => (case Size.class size
					 of Size.INT
					  => Assembly.instruction_mov
					     {src = Operand.memloc src,
					      dst = Operand.memloc dst,
					      size = size}
					  | _
					  => Assembly.instruction_pfmov
					     {src = Operand.memloc src,
					      dst = Operand.memloc dst,
					      size = size}))
				 
			     val {statements = statements_asm_forces,
				  live}
			       = LivenessBlock.toLivenessStatements
			         {statements = asm::statements_forces,
				  live = live}
			   in
			     {statements 
			      = List.concat
			        [statements_asm_forces,
				 statements],
			      changed = changed
			                orelse
					List.exists(forces,
						    fn force as {age,...}
						     => age <> 0),
			      moves = [],
			      live = live}
			   end
		     in
		       case asm
			 of Assembly.Instruction 
			    (Instruction.MOV
			     {src as Operand.MemLoc memloc_src,
			      dst as Operand.MemLoc memloc_dst,
			      size})
			  => if List.contains(dead,
					      memloc_src,
					      MemLoc.eq)
			        orelse
				List.exists(moves,
					    fn {src,...}
					     => MemLoc.eq(memloc_src,src))
			       then {statements = statements,
				     changed = changed,
				     moves = {src = memloc_src,
					      dst = memloc_dst,
					      size = size,
					      age = 0}::moves,
				     live = live}
			       else default ()
			  | Assembly.Instruction 
			    (Instruction.pFMOV
			     {src as Operand.MemLoc memloc_src,
			      dst as Operand.MemLoc memloc_dst,
			      size})
			  => if List.contains(dead,
					      memloc_src,
					      MemLoc.eq)
			        orelse
				List.exists(moves,
					    fn {src,...}
					     => MemLoc.eq(memloc_src,src))
			       then {statements = statements,
				     changed = changed,
				     moves = {src = memloc_src,
					      dst = memloc_dst,
					      size = size,
					      age = 0}::moves,
				     live = live}
			       else default ()
			  | _ => default ()
		     end)

	    val forces = moves
	    val statements_forces
	      = List.map
	        (forces,
		 fn move as {src,dst,size,...}
		  => (case Size.class size
			of Size.INT
			 => Assembly.instruction_mov
			    {src = Operand.memloc src,
			     dst = Operand.memloc dst,
			     size = size}
			 | _
			 => Assembly.instruction_pfmov
			    {src = Operand.memloc src,
			     dst = Operand.memloc dst,
			     size = size}))
	    val {statements = statements_forces,
		 live}
	      = LivenessBlock.toLivenessStatements
	        {statements = statements_forces,
		 live = live}
	    val statements = List.concat [statements_forces, 
					  statements]
	    val changed = changed
	                  orelse
			  List.exists(forces,
				      fn force as {age,...}
				       => age <> 0)
	    val block = LivenessBlock.T {label = label,
					 profileInfo = profileInfo,
					 statements = statements,
					 transfer = transfer}
	  in
	    {block = block,
	     changed = changed}
	  end

      val (moveHoist: 
	   {block: LivenessBlock.t} -> 
	   {block: LivenessBlock.t, 
	    changed: bool},
	   moveHoist_msg)
	= tracer
	  "moveHoist"
	  moveHoist
    end

  structure CopyPropagateLivenessBlock =
    struct
      fun copyPropagate' {src,
			  dst as Operand.MemLoc memloc_dst,
			  block as LivenessBlock.T {label,
						    profileInfo,
						    statements,
						    transfer},
			  liveInfo}
	= let
	    val changed = ref 0
	    val (all,replacer)
	      = case src
		  of Operand.MemLoc memloc_src
		   => let
			val all
			  = let
			      fun doit (memlocs, all)
				= List.fold
				  (memlocs,
				   all,
				   fn (memloc,all)
				    => if List.contains(all,
							memloc,
							MemLoc.eq)
					 then all
					 else memloc::all)
			    in
			      doit(memloc_dst::(MemLoc.utilized memloc_dst),
			      doit(memloc_src::(MemLoc.utilized memloc_src),
				   []))
			    end

			fun replacer' memloc
			  = if MemLoc.eq(memloc,memloc_dst)
			      then (changed := !changed + 1; 
				    memloc_src)
			      else memloc
				
			val replacer
			  = fn {use,def} => fn operand
			     => if use andalso not def
				  then case operand
					 of Operand.MemLoc memloc
					  => Operand.memloc 
					     (MemLoc.replace replacer' 
					                     memloc)
					  | _ => operand
				else case operand
				       of Operand.MemLoc memloc
				        => if MemLoc.eq(memloc,
							memloc_dst)
					     then operand
					     else Operand.memloc
					          (MemLoc.replace replacer' 
						                  memloc)
				        | _ => operand
		      in
			(all, replacer)
		      end
		   | _
		   => let
			val all
			  = let
			      fun doit (memlocs, all)
				= List.fold
				  (memlocs,
				   all,
				   fn (memloc,all)
				    => if List.contains(all,
							memloc,
							MemLoc.eq)
					 then all
					 else memloc::all)
			    in
			      doit(memloc_dst::(MemLoc.utilized memloc_dst),
				   [])
			    end
				
			val replacer
			  = fn {use,def} 
			     => fn operand
			         => if use andalso not def
				      then if Operand.eq(operand,dst)
					     then (changed := !changed + 1; 
						   src)
					     else operand
				      else operand
		      in
			(all, replacer)
		      end

	    val (transfer,_) = transfer

	    fun doit ([] : (Assembly.t * Liveness.t) list) 
	      = SOME (Block.T {label = label,
			       profileInfo = profileInfo,
			       statements = [],
			       transfer = Transfer.replace
			                  replacer
					  transfer})
	      | doit ((asm,{dead,...})::statements)
	      = let
		  val asm = Assembly.replace replacer asm

		  val {uses,defs,...} = Assembly.uses_defs_kills asm

		  local
		    fun doit operands
		      = List.fold
		        (operands,
			 [],
			 fn (operand,memlocs)
			  => case Operand.deMemloc operand
			       of SOME memloc
				=> if List.contains(memlocs,
						    memloc,
						    MemLoc.eq)
				     then memlocs
				     else memloc::memlocs
				| NONE => memlocs)
		  in
		    val uses = doit uses
		    val defs = doit defs
		  end

		  val uses'
		    = let
			fun doit(memlocs,uses')
			  = List.fold
			    (memlocs,
			     uses',
			     fn (memloc,uses')
			      => if List.contains(uses',
						  memloc,
						  MemLoc.eq)
				   then uses'
				   else memloc::uses')
			fun doit'(memlocs,uses')
			  = List.fold
			    (memlocs,
			     uses',
			     fn (memloc,uses')
			      => doit(MemLoc.utilized memloc, uses'))
		      in
			doit'(defs,
		        doit'(uses,
			doit(uses,
			     [])))
		      end
		in
		  if not (List.contains(uses',
					memloc_dst,
					MemLoc.eq))
		    then if List.contains(dead,memloc_dst,MemLoc.eq)
			   then let
				  val statements 
				    = List.map(statements, 
					       fn (asm,{...}) => asm)
				in 
				  SOME (Block.T 
					{label = label,
					 profileInfo = profileInfo,
					 statements = asm::statements,
					 transfer = transfer})
				end
			 else if List.forall
			         (all,
				  fn memloc
				   => List.forall
				      (defs,
				       fn memloc'
			                => not (MemLoc.mayAlias(memloc, 
								memloc'))))
			   then case doit statements
				  of NONE => NONE
				   | SOME (Block.T {label,
						    profileInfo,
						    statements,
						    transfer})
				   => SOME (Block.T 
					    {label = label,
					     profileInfo = profileInfo,
					     statements = asm::statements,
					     transfer = transfer})
		         else NONE
		    else NONE
		end
	  in
	    case doit statements
	      of NONE => NONE
	       | SOME block => SOME {block = LivenessBlock.toLivenessBlock 
				             {block = block,
					      liveInfo = liveInfo},
				     changed = !changed > 0}
	  end
	| copyPropagate' _ = Error.bug "copyPropagate'"

      fun copyPropagate {block as LivenessBlock.T 
			          {label, profileInfo, statements, transfer},
			 liveInfo}
	= let
	    val (_,{liveOut,...}) = transfer

	    val {block,changed,liveOut}
	      = List.foldr
	        (statements,
		 {block = LivenessBlock.T {label = label,
					   profileInfo = profileInfo,
					   statements = [],
					   transfer = transfer},
		  changed = false,
		  liveOut = liveOut},
		 fn ((asm as Assembly.Instruction 
		             (Instruction.MOV
			      {src,
			       dst as Operand.MemLoc memloc_dst,
			       size}),
		      info as {dead,...}),
		     {block as LivenessBlock.T {label,profileInfo,
						statements,transfer},
		      changed,
		      liveOut})
		  => let
		       val block'
			 = LivenessBlock.T 
			   {label = label,
			    profileInfo = profileInfo,
			    statements = (asm,info)::statements,
			    transfer = transfer}	
		       val liveOut'
			 = List.removeAll(liveOut,
					  fn memloc 
					   => List.contains
					      (dead,
					       memloc,
					       MemLoc.eq))
		     in
		       if MemLoc.isTemp memloc_dst 
			  andalso
			  not (List.contains(liveOut,
					     memloc_dst,
					     MemLoc.eq))
			 then case copyPropagate' {src = src,
						   dst = dst,
						   block = block,
						   liveInfo = liveInfo}
				of NONE => {block = block',
					    changed = changed,
					    liveOut = liveOut'}
				 | SOME {block = block',
					 changed = changed'}
				  => {block = block',
				      changed = changed orelse changed',
				      liveOut = liveOut'}
			 else {block = block',
			       changed = changed,
			       liveOut = liveOut'}
		     end
		  | ((asm as Assembly.Instruction 
		             (Instruction.pFMOV
			      {src,
			       dst as Operand.MemLoc memloc_dst,
			       size}),
		      info as {dead,...}),
		     {block as LivenessBlock.T {label,profileInfo,
						statements,transfer},
		      changed,
		      liveOut})
		  => let
		       val block'
			 = LivenessBlock.T 
			   {label = label,
			    profileInfo = profileInfo,
			    statements = (asm,info)::statements,
			    transfer = transfer}	
		       val liveOut'
			 = List.removeAll(liveOut,
					  fn memloc 
					   => List.contains
					      (dead,
					       memloc,
					       MemLoc.eq))
		     in 
		       if MemLoc.isTemp memloc_dst 
		          andalso
			  not (List.contains(liveOut,
					     memloc_dst,
					     MemLoc.eq))
			 then case copyPropagate' {src = src,
						   dst = dst,
						   block = block,
						   liveInfo = liveInfo}
				of NONE 
				 => {block = block',
				     changed = changed,
				     liveOut = liveOut'}
				 | SOME {block = block',
					 changed = changed'}
				 => {block = block',
				     changed = changed orelse changed',
				     liveOut = liveOut'}
			 else {block = block',
			       changed = changed,
			       liveOut = liveOut'}
		     end
		  | ((asm,info as {dead,...}),
		     {block as LivenessBlock.T {label,profileInfo,
						statements,transfer},
		      changed,
		      liveOut})
		  => let
		       val block'
			 = LivenessBlock.T 
			   {label = label,
			    profileInfo = profileInfo,
			    statements = (asm,info)::statements,
			    transfer = transfer}
		       val liveOut'
			 = List.removeAll(liveOut,
					  fn memloc 
					   => List.contains
					      (dead,
					       memloc,
					       MemLoc.eq))
		     in
		       {block = block',
			changed = changed,
			liveOut = liveOut'}
		     end)
	  in
	    {block = block,
	     changed = changed}
	  end

      val (copyPropagate : 
	   {block: LivenessBlock.t, 
	    liveInfo: {get: Label.t -> MemLoc.t list,
		       set: Label.t * MemLoc.t list -> unit}} ->
	   {block: LivenessBlock.t,
	    changed: bool},
	   copyPropagate_msg)
	= tracer
	  "copyPropagate"
	  copyPropagate
    end

  structure PeepholeLivenessBlock =
    struct
      structure Peephole 
	= Peephole(type label_type = Label.t
		   type profileInfo_type = ProfileInfo.t
		   type statement_type = Assembly.t * Liveness.t
		   type transfer_type = Transfer.t * Liveness.t
		   datatype block = datatype LivenessBlock.t)
      open Peephole

      fun make_callback_msg name
	= let
	    val count = ref 0
	    val total = ref 0
	    val callback = fn true => (Int.inc count; Int.inc total)
	                    | false => Int.inc total
	    val msg = fn () => Control.messageStr 
	                       (Control.Detail,
				concat [name, 
					": ", Int.toString (!count),
					" / ", Int.toString (!total)])
	  in
	    (callback,msg)
	  end

      val isComment : statement_type -> bool
	= fn (Assembly.Comment _, 
	      {...}) => true
	   | _ => false

      local
	val isInstruction_dstsTemp_dstsDead : statement_type -> bool
	  = fn (Assembly.Instruction instruction,
		{dead,...})
	     => let
		  val {dsts,...} = Instruction.srcs_dsts instruction
		in 
		  case dsts
		    of NONE => false
		     | SOME dsts => List.forall
		                    (dsts,
				     fn Operand.MemLoc memloc
				      => MemLoc.isTemp memloc
				         andalso
					 List.contains(dead,memloc,MemLoc.eq)
				      | _ => false)
		end 
	     | _ => false

	val template : template 
	  = {start = EmptyOrNonEmpty,
	     statements = [One isInstruction_dstsTemp_dstsDead],
	     finish = EmptyOrNonEmpty,
	     transfer = fn _ => true}

	val rewriter : rewriter
	  = fn {label,
		profileInfo,
		start, 
		statements as
		[[(Assembly.Instruction instruction,
		   info as {...})]],
		finish as [], 
		transfer as (Transfer.Iff {condition = Instruction.O,
					   truee,
					   falsee},
			     {...})}
	     => NONE
	     | {label,
		profileInfo,
		start, 
		statements as
		[[(Assembly.Instruction instruction,
		   info as {...})]],
		finish as [], 
		transfer as (Transfer.Iff {condition = Instruction.NO,
					   truee,
					   falsee},
			     {...})}
	     => NONE
	     | {label,
		profileInfo,
		start, 
		statements as
		[[(Assembly.Instruction instruction,
		   info as {liveOut,...})]],
		finish, 
		transfer}
	     => let
		  val {statements}
		    = LivenessBlock.reLivenessStatements
		      {statements = List.rev start,
		       live = liveOut}

		  val statements
		    = List.concat [statements, finish]
		in
		  SOME (LivenessBlock.T
			{label = label,
			 profileInfo = profileInfo,
			 statements = statements,
			 transfer = transfer})
		end
             | _ => Error.bug "Peephole: elimDeadDsts"

	val (callback,elimDeadDsts_msg)
	  = make_callback_msg "elimDeadDsts"
      in
	val elimDeadDsts : optimization
	  = {template = template,
	     rewriter = rewriter,
	     callback = callback}
	val elimDeadDsts_msg = elimDeadDsts_msg
      end

      local
	val isInstructionMOV_dstTemp : statement_type -> bool
	  = fn (Assembly.Instruction (Instruction.MOV 
				      {dst = Operand.MemLoc memloc,...}), 
		{...})
	     => MemLoc.isTemp memloc
	     | _ => false

	val isInstructionAL_dstTemp : statement_type -> bool
	  = fn (Assembly.Instruction (Instruction.BinAL
				      {dst = Operand.MemLoc memloc,...}),
		{...})
	     => MemLoc.isTemp memloc
	     | (Assembly.Instruction (Instruction.pMD
				      {dst = Operand.MemLoc memloc,...}),
		
		{...})
	     => MemLoc.isTemp memloc
	     | (Assembly.Instruction (Instruction.UnAL
				      {dst = Operand.MemLoc memloc,...}),
		
		{...})
	     => MemLoc.isTemp memloc
	     | (Assembly.Instruction (Instruction.SRAL
				      {dst = Operand.MemLoc memloc,...}),
		
		{...})
	     => MemLoc.isTemp memloc
	     | _ => false

	val isInstructionMOV_srcTemp_srcDead : statement_type -> bool
	  = fn (Assembly.Instruction (Instruction.MOV 
				      {src = Operand.MemLoc memloc,...}),
		{dead,...})
	     => MemLoc.isTemp memloc
	        andalso
		List.contains(dead, memloc, MemLoc.eq)
	     | _ => false

	val template : template 
	  = {start = EmptyOrNonEmpty,
	     statements = [One isInstructionMOV_dstTemp,
			   All (fn asm 
				 => (isComment asm) 
				    orelse
				    (isInstructionAL_dstTemp asm)),
			   One isInstructionMOV_srcTemp_srcDead],
	     finish = EmptyOrNonEmpty,
	     transfer = fn _ => true}

	val rewriter : rewriter
	  = fn {label,
		profileInfo,
		start, 
		statements as
		[[(Assembly.Instruction (Instruction.MOV 
					 {src = src1,
					  dst = dst1 as Operand.MemLoc memloc1,
					  size = size1}),
		   {...})],
		 statements',
		 [(Assembly.Instruction (Instruction.MOV 
					 {src = src2 as Operand.MemLoc memloc2,
					  dst = dst2,
					  size = size2}),
		   {liveOut = liveOut2,...})]],
		finish, 
		transfer}
	     => if Size.eq(size1,size2) andalso
	           MemLoc.eq(memloc1,memloc2) andalso
		   List.forall
		   (statements',
		    fn (Assembly.Comment _, liveInfo) => true
		     | (Assembly.Instruction (Instruction.BinAL
					      {oper, 
					       src, 
					       dst as Operand.MemLoc memloc, 
					       size}),
			liveInfo)
		     => Size.eq(size1,size) andalso
                        MemLoc.eq(memloc1,memloc) andalso
			(case (src,dst2)
			   of (Operand.MemLoc memloc_src,
			       Operand.MemLoc memloc_dst2)
			    => List.forall
			       (memloc_src::(MemLoc.utilized memloc_src),
				fn memloc' 
				 => not (MemLoc.mayAlias(memloc_dst2,memloc')))
			    | (Operand.Immediate _, _) => true
			    | _ => false)
		     | (Assembly.Instruction (Instruction.pMD
					      {oper, 
					       src, 
					       dst as Operand.MemLoc memloc, 
					       size}),
			liveInfo)
		     => Size.eq(size1,size) andalso
                        MemLoc.eq(memloc1,memloc) andalso
			(case (src,dst2)
			   of (Operand.MemLoc memloc_src,
			       Operand.MemLoc memloc_dst2)
			    => List.forall
			       (memloc_src::(MemLoc.utilized memloc_src),
				fn memloc' 
				 => not (MemLoc.mayAlias(memloc_dst2,memloc')))
			    | (Operand.Immediate _, _) => true
			    | _ => false)
		     | (Assembly.Instruction (Instruction.UnAL
					      {oper, 
					       dst as Operand.MemLoc memloc, 
					       size}),
			liveInfo)
		     => Size.eq(size1,size) andalso
                        MemLoc.eq(memloc1,memloc) 
		     | (Assembly.Instruction (Instruction.SRAL
					      {oper, 
					       count,
					       dst as Operand.MemLoc memloc, 
					       size}),
			liveInfo)
		     => Size.eq(size1,size) andalso
                        MemLoc.eq(memloc1,memloc) andalso
			(case (count,dst2)
			   of (Operand.MemLoc memloc_count,
			       Operand.MemLoc memloc_dst2)
			    => List.forall
			       (memloc_count::(MemLoc.utilized memloc_count),
				fn memloc' 
				 => not (MemLoc.mayAlias(memloc_dst2,memloc')))
			    | (Operand.Immediate _, _) => true
			    | _ => false)
		     | _ => Error.bug "Peephole: elimALCopy")
		  then let
			 val statements
			   = List.map
			     (statements',
			      fn (asm,info)
			       => Assembly.replace
			          (fn {use, def} 
				    => fn operand 
				        => if Operand.eq(operand,dst1)
					     then dst2
					     else operand)
                                  asm)

			 val {statements, ...}
			   = LivenessBlock.toLivenessStatements
			     {statements 
			      = (Assembly.instruction_mov
				 {src = src1,
				  dst = dst2,
				  size = size1})::statements,
			      live = liveOut2}

			 val statements
			   = List.fold(start,
				       List.concat [statements,
						    finish],
				       op ::)
		       in
			 SOME (LivenessBlock.T
			       {label = label,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})		 
		       end
		  else NONE
             | _ => Error.bug "Peephole: elimALCopy"

	val (callback,elimALCopy_msg)
	  = make_callback_msg "elimALCopy"
      in
	val elimALCopy : optimization
	  = {template = template,
	     rewriter = rewriter,
	     callback = callback}
	val elimALCopy_msg = elimALCopy_msg
      end

      local
	val isInstructionMOV_eqSrcDst : statement_type -> bool
	= fn (Assembly.Instruction (Instruction.MOV 
				    {dst = Operand.MemLoc memloc1,
				     src = Operand.MemLoc memloc2,...}),
	      {...}) 
	   => MemLoc.eq(memloc1,memloc2) 
	   | _ => false

	val template : template 
	  = {start = EmptyOrNonEmpty,
	     statements = [One isInstructionMOV_eqSrcDst],
	     finish = EmptyOrNonEmpty,
	     transfer = fn _ => true}

	val rewriter : rewriter
	  = fn {label,
		profileInfo,
		start, 
		statements as
		[[(Assembly.Instruction (Instruction.MOV 
					 {src = src1, 
					  dst = dst1, 
					  size = size1}),
		   {...})]],
		finish, 
		transfer}
	     => let
		  val statements
		    = List.fold(start,
				finish,
				op ::)
		in 
		  SOME (LivenessBlock.T
			{label = label,
			 profileInfo = profileInfo,
			 statements = statements,
			 transfer = transfer})
		end
	     | _ => Error.bug "Peephole: elimSelfMove"

	val (callback,elimSelfMove_msg)
	  = make_callback_msg "elimSelfMove"
      in
	val elimSelfMove : optimization
	  = {template = template,
	     rewriter = rewriter,
	     callback = callback}
	val elimSelfMove_msg = elimSelfMove_msg
      end

      local
	val isInstructionMOV_dstMemloc : statement_type -> bool
	  = fn (Assembly.Instruction (Instruction.MOV 
				      {dst = Operand.MemLoc memloc,...}),
		{...})
	     => true
	     | _ => false

	val isInstructionBinALMD_dstMemloc_operCommute : statement_type -> bool
	  = fn (Assembly.Instruction (Instruction.BinAL
				      {oper,
				       dst = Operand.MemLoc memloc,...}),
		{...})
	     => (oper = Instruction.ADD)
	        orelse
		(oper = Instruction.ADC)
		orelse
		(oper = Instruction.AND)
		orelse
		(oper = Instruction.OR)
		orelse
		(oper = Instruction.XOR)
	     | (Assembly.Instruction (Instruction.pMD
				      {oper,
				       dst = Operand.MemLoc memloc,...}),
		{...})
	     => (oper = Instruction.IMUL)
	        orelse
		(oper = Instruction.MUL)
	   | _ => false

	val template : template 
	  = {start = EmptyOrNonEmpty,
	     statements = [One isInstructionMOV_dstMemloc,
			   All isComment,
			   One isInstructionBinALMD_dstMemloc_operCommute],
	     finish = EmptyOrNonEmpty,
	     transfer = fn _ => true}

	val rewriter : rewriter
	  = fn {label,
		profileInfo,
		start, 
		statements as
		[[(Assembly.Instruction (Instruction.MOV 
					 {src = src1,
					  dst 
					  = dst1 as Operand.MemLoc memloc_dst1,
					  size = size1}),
		   {dead = dead1,...})],
		 comments,
		 [(Assembly.Instruction (Instruction.BinAL 
					 {oper = oper2,
					  src = src2,
					  dst 
					  = dst2 as Operand.MemLoc memloc_dst2,
					  size = size2}),
		   {dead = dead2,
		    liveOut = liveOut2,...})]],
		finish, 
		transfer}
	     => if Size.eq(size1,size2) andalso
	           Operand.eq(dst1,dst2) andalso
		   not (Operand.eq(src1,src2)) andalso
		   (case (src1,src2)
		      of (Operand.MemLoc memloc_src1,
			  Operand.MemLoc memloc_src2)
		       => List.contains(dead2,
					memloc_src2,
					MemLoc.eq)
			  andalso
			  not (List.contains(dead1,
					     memloc_src1,
					     MemLoc.eq))
		       | (_, Operand.MemLoc memloc_src2)
		       => List.contains(dead2,
					memloc_src2,
					MemLoc.eq)
		       | _ => false) andalso
		   (case src1
		      of Operand.MemLoc memloc_src1
		       => not (List.exists
			       (memloc_src1::(MemLoc.utilized memloc_src1),
				fn memloc'
				 => MemLoc.mayAlias(memloc',memloc_dst1)))
		       | _ => true) andalso
		   (case src2
		      of Operand.MemLoc memloc_src2
		       => not (List.exists
			       (memloc_src2::(MemLoc.utilized memloc_src2),
				fn memloc'
				 => MemLoc.mayAlias(memloc',memloc_dst1)))
		       | _ => true)
		  then let
			 val statements
			   = (Assembly.instruction_mov
			      {src = src2,
			       dst = dst1,
			       size = size1})::
			     (List.concat
			      [List.map(comments, #1),
			       [Assembly.instruction_binal
				{oper = oper2,
				 src = src1,
				 dst = dst2,
				 size = size2}]])

			 val {statements, ...}
			   = LivenessBlock.toLivenessStatements
			     {statements = statements,
			      live = liveOut2}

			 val statements
			   = List.fold(start,
				       List.concat [statements, 
						    finish],
				       op ::)
		       in
			 SOME (LivenessBlock.T
			       {label = label,	
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})		 
		       end
		  else NONE
	     | {label,
		profileInfo,
		start, 
		statements as
		[[(Assembly.Instruction (Instruction.MOV 
					 {src = src1,
					  dst 
					  = dst1 as Operand.MemLoc memloc_dst1,
					  size = size1}),
		   {dead = dead1,...})],
		 comments,
		 [(Assembly.Instruction (Instruction.pMD 
					 {oper = oper2,
					  src = src2,
					  dst 
					  = dst2 as Operand.MemLoc memloc_dst2,
					  size = size2}),
		   {dead = dead2,
		    liveOut = liveOut2,...})]],
		finish, 
		transfer}
	     => if Size.eq(size1,size2) andalso
	           Operand.eq(dst1,dst2) andalso
		   not (Operand.eq(src1,src2)) andalso
		   (case (src1,src2)
		      of (Operand.MemLoc memloc_src1,
			  Operand.MemLoc memloc_src2)
		       => List.contains(dead2,
					memloc_src2,
					MemLoc.eq)
			  andalso
			  not (List.contains(dead1,
					     memloc_src1,
					     MemLoc.eq))
		       | (_, Operand.MemLoc memloc_src2)
		       => List.contains(dead2,
					memloc_src2,
					MemLoc.eq)
		       | _ => false) andalso
		   (case src1
		      of Operand.MemLoc memloc_src1
		       => not (List.exists
			       (memloc_src1::(MemLoc.utilized memloc_src1),
				fn memloc'
				 => MemLoc.mayAlias(memloc',memloc_dst1)))
		       | _ => true) andalso
		   (case src2
		      of Operand.MemLoc memloc_src2
		       => not (List.exists
			       (memloc_src2::(MemLoc.utilized memloc_src2),
				fn memloc'
				 => MemLoc.mayAlias(memloc',memloc_dst1)))
		       | _ => true)
		  then let
			 val statements
			   = (Assembly.instruction_mov
			      {src = src2,
			       dst = dst1,
			       size = size1})::
			     (List.concat
			      [List.map(comments, #1),
			       [Assembly.instruction_pmd
				{oper = oper2,
				 src = src1,
				 dst = dst2,
				 size = size2}]])

			 val {statements, ...}
			   = LivenessBlock.toLivenessStatements
			     {statements = statements,
			      live = liveOut2}

			 val statements
			   = List.fold(start,
				       List.concat [statements,
						    finish],
				       op ::)
		       in
			 SOME (LivenessBlock.T
			       {label = label,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})		 
		       end
		  else NONE
             | _ => Error.bug "Peephole: commuteBinALMD"

	val (callback,commuteBinALMD_msg)
	  = make_callback_msg "commuteBinALMD"
      in
	val commuteBinALMD : optimization
	  = {template = template,
	     rewriter = rewriter,
	     callback = callback}
	val commuteBinALMD_msg = commuteBinALMD_msg
      end

      local
	val isInstructionFMOV_dstTemp : statement_type -> bool
	  = fn (Assembly.Instruction (Instruction.pFMOV 
				      {dst = Operand.MemLoc memloc,...}), 
		{...})
	     => MemLoc.isTemp memloc
	     | _ => false

	val isInstructionFltA_dstTemp : statement_type -> bool
	  = fn (Assembly.Instruction (Instruction.pFBinA
				      {dst = Operand.MemLoc memloc,...}),
		{...})
	     => MemLoc.isTemp memloc
	     | (Assembly.Instruction (Instruction.pFUnA
				      {dst = Operand.MemLoc memloc,...}),
		
		{...})
	     => MemLoc.isTemp memloc
	     | (Assembly.Instruction (Instruction.pFPTAN
				      {dst = Operand.MemLoc memloc,...}),
		
		{...})
	     => MemLoc.isTemp memloc
	     | (Assembly.Instruction (Instruction.pFBinAS
				      {dst = Operand.MemLoc memloc,...}),
		{...})
	     => MemLoc.isTemp memloc
	     | (Assembly.Instruction (Instruction.pFBinASP
				      {dst = Operand.MemLoc memloc,...}),
		{...})
	     => MemLoc.isTemp memloc
	     | _ => false

	val isInstructionFMOV_srcTemp_srcDead : statement_type -> bool
	  = fn (Assembly.Instruction (Instruction.pFMOV 
				      {src = Operand.MemLoc memloc,...}),
		{dead,...})
	     => MemLoc.isTemp memloc
	        andalso
		List.contains(dead, memloc, MemLoc.eq)
	     | _ => false

	val template : template 
	  = {start = EmptyOrNonEmpty,
	     statements = [One isInstructionFMOV_dstTemp,
			   All (fn asm 
				 => (isComment asm) 
				    orelse
				    (isInstructionFltA_dstTemp asm)),
			   One isInstructionFMOV_srcTemp_srcDead],
	     finish = EmptyOrNonEmpty,
	     transfer = fn _ => true}

	val rewriter : rewriter
	  = fn {label,
		profileInfo,
		start, 
		statements as
		[[(Assembly.Instruction (Instruction.pFMOV 
					 {src = src1,
					  dst = dst1 as Operand.MemLoc memloc1,
					  size = size1}),
		   {...})],
		 statements',
		 [(Assembly.Instruction (Instruction.pFMOV 
					 {src = src2 as Operand.MemLoc memloc2,
					  dst = dst2,
					  size = size2}),
		   {liveOut = liveOut2,...})]],
		finish, 
		transfer}
	     => if Size.eq(size1,size2) andalso
	           MemLoc.eq(memloc1,memloc2) andalso
		   List.forall
		   (statements',
		    fn (Assembly.Comment _, liveInfo) => true
		     | (Assembly.Instruction (Instruction.pFBinA
					      {oper, 
					       src, 
					       dst as Operand.MemLoc memloc, 
					       size}),
			liveInfo)
		     => Size.eq(size1,size) andalso
                        MemLoc.eq(memloc1,memloc) andalso
			(case (src,dst2)
			   of (Operand.MemLoc memloc_src,
			       Operand.MemLoc memloc_dst2)
			    => List.forall
			       (memloc_src::(MemLoc.utilized memloc_src),
				fn memloc' 
				 => not (MemLoc.mayAlias(memloc_dst2,memloc')))
			    | (Operand.Immediate _, _) => true
			    | _ => false)
		     | (Assembly.Instruction (Instruction.pFUnA
					      {oper, 
					       dst as Operand.MemLoc memloc, 
					       size}),
			liveInfo)
		     => Size.eq(size1,size) andalso
                        MemLoc.eq(memloc1,memloc) 
		     | (Assembly.Instruction (Instruction.pFPTAN
					      {dst as Operand.MemLoc memloc, 
					       size}),
			liveInfo)
		     => Size.eq(size1,size) andalso
                        MemLoc.eq(memloc1,memloc) 
		     | (Assembly.Instruction (Instruction.pFBinAS
					      {oper, 
					       src, 
					       dst as Operand.MemLoc memloc, 
					       size}),
			liveInfo)
		     => Size.eq(size1,size) andalso
                        MemLoc.eq(memloc1,memloc) andalso
			(case (src,dst2)
			   of (Operand.MemLoc memloc_src,
			       Operand.MemLoc memloc_dst2)
			    => List.forall
			       (memloc_src::(MemLoc.utilized memloc_src),
				fn memloc' 
				 => not (MemLoc.mayAlias(memloc_dst2,memloc')))
			    | (Operand.Immediate _, _) => true
			    | _ => false)
		     | (Assembly.Instruction (Instruction.pFBinASP
					      {oper, 
					       src, 
					       dst as Operand.MemLoc memloc, 
					       size}),
			liveInfo)
		     => Size.eq(size1,size) andalso
                        MemLoc.eq(memloc1,memloc) andalso
			(case (src,dst2)
			   of (Operand.MemLoc memloc_src,
			       Operand.MemLoc memloc_dst2)
			    => List.forall
			       (memloc_src::(MemLoc.utilized memloc_src),
				fn memloc' 
				 => not (MemLoc.mayAlias(memloc_dst2,memloc')))
			    | (Operand.Immediate _, _) => true
			    | _ => false)
		     | _ => Error.bug "Peephole: elimFltACopy")
		  then let
			 val statements
			   = List.map
			     (statements',
			      fn (asm,info)
			       => Assembly.replace
			          (fn {use, def} 
				    => fn operand 
				        => if Operand.eq(operand,dst1)
					     then dst2
					     else operand)
                                  asm)

			 val {statements, ...}
			   = LivenessBlock.toLivenessStatements
			     {statements 
			      = (Assembly.instruction_pfmov
				 {src = src1,
				  dst = dst2,
				  size = size1})::statements,
			      live = liveOut2}

			 val statements
			   = List.fold(start,
				       List.concat [statements, 
						    finish],
				       op ::)
		       in
			 SOME (LivenessBlock.T
			       {label = label,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})		 
		       end
		  else NONE
             | _ => Error.bug "Peephole: elimFltACopy"

	val (callback,elimFltACopy_msg)
	  = make_callback_msg "elimFltACopy"
      in
	val elimFltACopy : optimization
	  = {template = template,
	     rewriter = rewriter,
	     callback = callback}
	val elimFltACopy_msg = elimFltACopy_msg
      end

      local
	val isInstructionFMOV_eqSrcDst : statement_type -> bool
	= fn (Assembly.Instruction (Instruction.pFMOV 
				    {dst = Operand.MemLoc memloc1,
				     src = Operand.MemLoc memloc2,...}),
	      {...}) 
	   => MemLoc.eq(memloc1,memloc2) 
	   | _ => false

	val template : template 
	  = {start = EmptyOrNonEmpty,
	     statements = [One isInstructionFMOV_eqSrcDst],
	     finish = EmptyOrNonEmpty,
	     transfer = fn _ => true}

	val rewriter : rewriter
	  = fn {label,
		profileInfo,
		start, 
		statements as
		[[(Assembly.Instruction (Instruction.pFMOV 
					 {src = src1, 
					  dst = dst1, 
					  size = size1}),
		   {...})]],
		finish, 
		transfer}
	     => let
		  val statements 
		    = List.fold
		      (start, 
		       finish, 
		       op ::)
		in 
		  SOME (LivenessBlock.T
			{label = label,
			 profileInfo = profileInfo,
			 statements = statements,
			 transfer = transfer})
		end
	     | _ => Error.bug "Peephole: elimFltSelfMove"
 
	val (callback,elimFltSelfMove_msg)
	  = make_callback_msg "elimFltSelfMove"
      in
	val elimFltSelfMove : optimization
	  = {template = template,
	     rewriter = rewriter,
	     callback = callback}
	val elimFltSelfMove_msg = elimFltSelfMove_msg
      end

      local
	val isInstructionFMOV_dstMemloc : statement_type -> bool
	  = fn (Assembly.Instruction (Instruction.pFMOV 
				      {dst = Operand.MemLoc memloc,...}),
		{...})
	     => true
	     | _ => false

	val isInstructionFltBinA_dstMemloc : statement_type -> bool
	  = fn (Assembly.Instruction (Instruction.pFBinA
				      {oper,
				       dst = Operand.MemLoc memloc,...}),
		{...})
	     => true
	     | _ => false

	val template : template 
	  = {start = EmptyOrNonEmpty,
	     statements = [One isInstructionFMOV_dstMemloc,
			   All isComment,
			   One isInstructionFltBinA_dstMemloc],
	     finish = EmptyOrNonEmpty,
	     transfer = fn _ => true}

	val rewriter : rewriter
	  = fn {label,
		profileInfo,
		start, 
		statements as
		[[(Assembly.Instruction (Instruction.pFMOV 
					 {src = src1,
					  dst 
					  = dst1 as Operand.MemLoc memloc_dst1,
					  size = size1}),
		   {dead = dead1,...})],
		 comments,
		 [(Assembly.Instruction (Instruction.pFBinA 
					 {oper = oper2,
					  src = src2,
					  dst 
					  = dst2 as Operand.MemLoc memloc_dst2,
					  size = size2}),
		   {dead = dead2,
		    liveOut = liveOut2,...})]],
		finish, 
		transfer}
	     => if Size.eq(size1,size2) andalso
	           Operand.eq(dst1,dst2) andalso
		   not (Operand.eq(src1, src2)) andalso
		   (case (src1,src2)
		      of (Operand.MemLoc memloc_src1,
			  Operand.MemLoc memloc_src2)
		       => List.contains(dead2,
					memloc_src2,
					MemLoc.eq)
			  andalso
			  not (List.contains(dead1,
					     memloc_src1,
					     MemLoc.eq))
		       | (_, Operand.MemLoc memloc_src2)
		       => List.contains(dead2,
					memloc_src2,
					MemLoc.eq)
		       | _ => false) andalso
		   (case src1
		      of Operand.MemLoc memloc_src1
		       => not (List.exists
			       (memloc_src1::(MemLoc.utilized memloc_src1),
				fn memloc'
				 => MemLoc.mayAlias(memloc',memloc_dst1)))
		       | _ => true) andalso
		   (case src2
		      of Operand.MemLoc memloc_src2
		       => not (List.exists
			       (memloc_src2::(MemLoc.utilized memloc_src2),
				fn memloc'
				 => MemLoc.mayAlias(memloc',memloc_dst1)))
		       | _ => true)
		  then let
			 val statements
			   = (Assembly.instruction_pfmov
			      {src = src2,
			       dst = dst1,
			       size = size1})::
			     (List.concat
			      [List.map(comments, #1),
			       [Assembly.instruction_pfbina
				{oper = Instruction.fbina_reverse oper2,
				 src = src1,
				 dst = dst2,
				 size = size2}]])

			 val {statements, ...}
			   = LivenessBlock.toLivenessStatements
			     {statements = statements,
			      live = liveOut2}

			 val statements 
			   = List.fold(start, 
				       List.concat [statements,
						    finish], 
				       op ::)
		       in
			 SOME (LivenessBlock.T
			       {label = label,	
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})		 
		       end
		  else NONE
             | _ => Error.bug "Peephole: commuteFltBinA"

	val (callback,commuteFltBinA_msg)
	  = make_callback_msg "commuteFltBinA"
      in
	val commuteFltBinA : optimization
	  = {template = template,
	     rewriter = rewriter,
	     callback = callback}
	val commuteFltBinA_msg = commuteFltBinA_msg
      end

      local
	val isInstructionSETcc_dstTemp : statement_type -> bool
	  = fn (Assembly.Instruction (Instruction.SETcc 
				      {dst = Operand.MemLoc memloc,...}),
		{...}) 
	     => MemLoc.isTemp memloc
	     | _ => false

	val isInstructionTEST_eqSrcs_srcsTemp_srcsDead : statement_type -> bool
	  = fn (Assembly.Instruction (Instruction.TEST 
				      {src1 = Operand.MemLoc memloc1,
				       src2 = Operand.MemLoc memloc2,...}),
		{dead,...})
	     => MemLoc.eq(memloc1,memloc2) andalso
	        MemLoc.isTemp memloc1 andalso
		List.contains(dead, memloc1, MemLoc.eq)
	     | _ => false

	val isIff_conditionZorNZ : transfer_type -> bool
	  = fn (Transfer.Iff {condition,...},
		{...})
	     => (case condition
		   of Instruction.Z => true
		    | Instruction.NZ => true
		    | _ => false)
	     | _ => false

	val template : template
	  = {start = EmptyOrNonEmpty,
	     statements = [One isInstructionSETcc_dstTemp,
			   All isComment,
			   One isInstructionTEST_eqSrcs_srcsTemp_srcsDead,
			   All isComment],
	     finish = Empty,
	     transfer = isIff_conditionZorNZ}

	val rewriter : rewriter
	  = fn {label,
		profileInfo,
		start, 
		statements as
		[[(Assembly.Instruction (Instruction.SETcc
					 {condition = condition1,
					  dst 
					  = dst1 as Operand.MemLoc memloc1,
					  size = size1}),
		   {...})],
		 comments1,
		 [(Assembly.Instruction (Instruction.TEST
					 {src1 
					  = src12 as Operand.MemLoc memloc12,
					  src2 
					  = src22 as Operand.MemLoc memloc22,
					  size = size2}),
		   {...})],
		 comments2],
		finish as [],
		transfer as
		(Transfer.Iff {condition, truee, falsee},
		 infoT as {...})}
	     => if MemLoc.eq(memloc1,memloc12)
		  then let
			 val condition 
			   = case condition
			       of Instruction.Z 
				=> Instruction.condition_negate condition1
			        | Instruction.NZ => condition1
			        | _ => Error.bug "Peephole: conditionalJump"

			 val transfer 
			   = (Transfer.iff {condition = condition,
					    truee = truee,
					    falsee = falsee},
			      infoT)

			 val {transfer,live}
			   = LivenessBlock.reLivenessTransfer 
			     {transfer = transfer}

			 val statements
			   = List.concat
			     [List.map(comments1, #1),
			      List.map(comments2, #1)]

			 val {statements, ...}
			   = LivenessBlock.toLivenessStatements
			     {statements = statements,
			      live = live}

			 val statements 
			   = List.fold(start, 
				       statements, 
				       op ::)
		       in
			 SOME (LivenessBlock.T
			       {label = label,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		  else NONE
	     | _ => Error.bug "Peephole: conditionalJump"

	val (callback,conditionalJump_msg)
	  = make_callback_msg "conditionalJump"
      in
	val conditionalJump : optimization
	  = {template = template,
	     rewriter = rewriter,
	     callback = callback}
	val conditionalJump_msg = conditionalJump_msg
      end

      local
	val {template, rewriter, ...} = elimDeadDsts
	val (callback,elimDeadDsts_minor_msg)
	  = make_callback_msg "elimDeadDsts_minor"
      in
	val elimDeadDsts_minor : optimization
	  = {template = template,
	     rewriter = rewriter,
	     callback = callback}
	val elimDeadDsts_minor_msg = elimDeadDsts_minor_msg
      end

      local
	val {template, rewriter, ...} = elimSelfMove
	val (callback,elimSelfMove_minor_msg)
	  = make_callback_msg "elimSelfMove_minor"
      in
	val elimSelfMove_minor : optimization
	  = {template = template,
	     rewriter = rewriter,
	     callback = callback}
	val elimSelfMove_minor_msg = elimSelfMove_minor_msg
      end

      local
	val {template, rewriter, ...} = elimFltSelfMove
	val (callback,elimFltSelfMove_minor_msg)
	  = make_callback_msg "elimFltSelfMove_minor"
      in
	val elimFltSelfMove_minor : optimization
	  = {template = template,
	     rewriter = rewriter,
	     callback = callback}
	val elimFltSelfMove_minor_msg = elimFltSelfMove_minor_msg
      end

      local
	val optimizations 
	  = [elimALCopy, 
	     elimFltACopy,
	     elimDeadDsts,
	     elimSelfMove,
	     elimFltSelfMove,
	     commuteBinALMD,
	     commuteFltBinA,
	     conditionalJump]
	val optimizations_msg
	  = [elimALCopy_msg, 
	     elimFltACopy_msg,
	     elimDeadDsts_msg,
	     elimSelfMove_msg,
	     elimFltSelfMove_msg,
	     commuteBinALMD_msg,
	     commuteFltBinA_msg,
	     conditionalJump_msg]

	val optimizations_minor
	  = [elimDeadDsts_minor,
	     elimSelfMove_minor,
	     elimFltSelfMove_minor]
	val optimizations_minor_msg
	  = [elimDeadDsts_minor_msg,
	     elimSelfMove_minor_msg,
	     elimFltSelfMove_minor_msg]
      in
	val (peepholeLivenessBlock, peepholeLivenessBlock_msg)
	  = tracer
            "peepholeLivenessBlock"
	    (fn block => peepholeBlock {optimizations = optimizations,
					block = block})

	val peepholeLivenessBlock_msg
	  = fn () => (peepholeLivenessBlock_msg ();
		      Control.indent ();
		      List.foreach(optimizations_msg, fn msg => msg ());
		      Control.unindent ())

	val (peepholeLivenessBlock_minor, peepholeLivenessBlock_minor_msg)
	  = tracer
            "peepholeLivenessBlock_minor"
	    (fn block => peepholeBlock {optimizations = optimizations_minor,
					block = block})

	val peepholeLivenessBlock_minor_msg
	  = fn () => (peepholeLivenessBlock_minor_msg ();
		      Control.indent ();
		      List.foreach(optimizations_minor_msg, fn msg => msg ());
		      Control.unindent ())
      end
    end

  structure LiveTransferInfo =
    struct
      open JumpInfo

      fun temp_uses_defs {uses : Operand.t list,
			  defs : Operand.t list}
	= let
	    val baseUses
	      = List.fold
	        (uses,
		 [],
		 fn (operand, baseUses)
		  => case Operand.deMemloc operand
		       of SOME memloc => if MemLoc.isTemp memloc
			                    andalso 
					    not (List.contains(baseUses,
							       memloc,
							       MemLoc.eq))
					   then memloc::baseUses
					   else baseUses
		        | NONE => baseUses)
		   
	    val tempUses
	      = let
		  fun doit (operands, tempUses)
		    = List.fold
		      (operands,
		       tempUses,
		       fn (operand, tempUses)
		        => case Operand.deMemloc operand
			     of SOME memloc
			      => List.fold(MemLoc.utilized memloc,
					   tempUses,
					   fn (memloc, tempUses)
					    => if MemLoc.isTemp memloc
					          andalso
						  not (List.contains
						       (tempUses,
							memloc,
							MemLoc.eq))
						 then memloc::tempUses
						 else tempUses)
                              | NONE => tempUses)
		in
		  doit(defs, 
		  doit(uses, 
		       baseUses))
		end

	    val baseDefs
	      = List.fold
	        (defs,
		 [],
		 fn (operand, baseDefs)
		  => case Operand.deMemloc operand
		       of SOME memloc => if MemLoc.isTemp memloc
			                    andalso 
					    not (List.contains(baseDefs,
							       memloc,
							       MemLoc.eq))
					   then memloc::baseDefs
					   else baseDefs
			| NONE => baseDefs)
	    val tempDefs = baseDefs
	  in
	    {uses = tempUses,
	     defs = tempDefs}
	  end

      fun computeLiveTransferInfo {blocks : Block.t list,
				   transferRegs : Register.t list,
				   liveInfo : 
				   {get : Label.t -> MemLoc.t list,
				    set : Label.t * MemLoc.t list -> unit},
				   jumpInfo : {get: Label.t -> status ref}}
	= let
	    datatype liveTransferType
	      = Normal
	      | Default of Label.t list
	      | Case of Label.t
	      | Top

	    val join
	      = fn (Normal, k) => k
		 | (k, Normal) => k
		 | (Default l1, Default l2) 
	         => if List.equalsAsSet(l1, l2, Label.equals)
		      then Default l1
		      else Top
		 | (Case l1, Case l2) 
		 => if Label.equals(l1,l2)
		      then Case l1
		      else Top
		 | _ => Top

	    val blockInfo
	     as {get = getBlockInfo :
		       Label.t -> {pred: Label.t list ref,
				   block: Block.t option ref,
				   succ: Label.t list ref,
				   distancesF: 
				   (MemLoc.t * (int * int) ref) list ref,
				   distancesB: 
				   (MemLoc.t * (int * int) ref) list ref,
				   liveTransferType: liveTransferType ref}}
	      = Property.get
	        (Label.plist,
		 Property.initFun (fn label 
				    => {pred = ref [],
					block = ref NONE,
					succ = ref [],
					distancesF = ref [],
					distancesB = ref [],
					liveTransferType = ref Normal}))

	    val get_pred = (#pred o getBlockInfo)
	    val get_block = (#block o getBlockInfo)
	    val get_succ = (#succ o getBlockInfo)
	    val get_liveTransferType = (#liveTransferType o getBlockInfo)
	    fun join_liveTransferType (label, liveTransferType')
	      = let
		  val liveTransferType = get_liveTransferType label
		in
		  liveTransferType := join(liveTransferType', 
					   !liveTransferType)
		end

	    fun get_distancesF' {temp : MemLoc.t,
				 label : Label.t}
	      = let
		  val {block, succ, distancesF, ...} = getBlockInfo label
		in
		  case List.peek(!distancesF,
				 fn (temp',_) => MemLoc.eq(temp,temp'))
		    of SOME (_,distances) => !distances
		     | NONE 
		     => let
			  val temp_distancesF = ref (0,1)
			  val _ = List.push(distancesF, 
					    (temp,temp_distancesF));

			  val Block.T {statements, transfer, ...} 
			    = valOf (!block)

			  datatype t = Pos of int | Length of int
			  fun posF ([],n) 
			    = let
				val {uses,defs,...} 
				  = Transfer.uses_defs_kills transfer
				val {uses,defs} 
				  = temp_uses_defs {uses = uses,
						    defs = defs}
			      in
				if List.contains(uses,
						 temp,
						 MemLoc.eq)
				  then Pos n
				  else Length (n + 1)
			      end
			    | posF (asm::assembly,n)
			    = let
				val {uses,defs,...} 
				  = Assembly.uses_defs_kills asm
				val {uses,defs} 
				  = temp_uses_defs {uses = uses,
						    defs = defs}
			      in
				if List.contains(uses,
						 temp,
						 MemLoc.eq)
				  then Pos n
				  else posF (assembly, n + 1)
			      end

			  val distances
			    = case posF(statements, 1)
				of Pos n => (n, 1)
				 | Length n
				 => List.fold
                                    (!succ,
				     (0,0),
				     fn (label, (tot, num))
				      => if List.contains
				            ((#get liveInfo) label,
					     temp,
					     MemLoc.eq)
					   then let
						  val (tot',num') 
						    = get_distancesF' 
						      {temp = temp, 
						       label = label}
						in
						  (tot + n * num' + tot',
						   num + num')
						end 
					   else (tot, num))
			in
			  temp_distancesF := distances;
			  distances
			end
		end

	    fun get_distancesF {temp : MemLoc.t,
				label : Label.t}
	      = let
		  val distances 
		    = get_distancesF' {temp = temp,
				       label = label}
		in
		  distances
		end

	    fun get_distancesB' {temp : MemLoc.t,
				 label : Label.t}
	      = let
		  val {pred, block, distancesB, ...} = getBlockInfo label
		in
		  case List.peek(!distancesB,
				 fn (temp',_) => MemLoc.eq(temp,temp'))
		    of SOME (_,distances) => !distances
		     | NONE 
		     => let
			  val temp_distancesB = ref (0,1)
			  val _ = List.push(distancesB, 
					    (temp,temp_distancesB));

			  val Block.T {statements, transfer, ...} 
			    = valOf (!block)

			  datatype t = Pos of int | Length of int
			  fun posR []
			    = let
				val {uses,defs,...} 
				  = Transfer.uses_defs_kills transfer
				val {uses,defs} 
				  = temp_uses_defs {uses = uses,
						    defs = defs}
			      in
				if List.contains(uses,
						 temp,
						 MemLoc.eq)
				   orelse
				   List.contains(defs,
						 temp,
						 MemLoc.eq)
				  then Pos 1
				  else Length 2
			      end
			    | posR (asm::assembly)
			    = (case posR assembly
				 of Pos n => Pos n
				  | Length n
				  => let
				       val {uses,defs,...} 
					 = Assembly.uses_defs_kills asm
				       val {uses,defs} 
					 = temp_uses_defs {uses = uses,
							   defs = defs}
				     in
				       if List.contains(uses,
							temp,
							MemLoc.eq)
					  orelse
					  List.contains(defs,
							temp,
							MemLoc.eq)
					 then Pos n
					 else Length (n + 1)
				     end)

			  val distances
			    = case posR statements
				of Pos n => (n,1)
				 | Length n
				 => List.fold
                                    (!pred,
				     (0,0),
				     fn (label, (tot, num))
				      => let
					   val (tot',num') 
					     = get_distancesB' 
					       {temp = temp, 
						label = label}
					 in
					   (tot + n * num' + tot',
					    num + num')
					 end)
			in
			  temp_distancesB := distances;
			  distances
			end
		end

	    fun get_distancesB {temp : MemLoc.t,
				label : Label.t}
	      = let
		  val {pred, ...} = getBlockInfo label

		  val distances
		    = List.fold
		      (!pred,
		       (0,0),
		       fn (label, (tot, num))
		        => let
			     val (tot', num')
			       = get_distancesB' {temp = temp,
						  label = label}
			   in
			     (tot + tot', num + num')
			   end)
		in
		  distances
		end
	      
	    val liveTransferInfo
	     as {get = getLiveTransferInfo :
		       Label.t -> (MemLoc.t * Register.t) list,
		 set = setLiveTransferInfo}
	     = Property.getSet
	       (Label.plist,
		Property.initFun (fn label => []))

	    val labels
	      = List.map
	        (blocks,
		 fn block' as Block.T {label, transfer, ...}
		  => let
		       val {block,succ,liveTransferType,...} 
			 = getBlockInfo label
		       val targets = Transfer.targets transfer
		     in 
		       block := SOME block';
		       succ := targets;
		       (if !((#get jumpInfo) label) = Must
			  then liveTransferType := Top
			  else ());
		       List.foreach
		       (targets,
			fn target => List.push(get_pred target, label));
		       (case transfer
			  of Transfer.Switch {default, cases, ...}
			   => let
				val cases
				  = Transfer.Cases.map'
				    (cases,
				     fn target => target,
				     fn (k, target) => target,
				     fn (k, target) => target,
				     fn (k, target) => target)
			      in
				(join_liveTransferType (default, 
							Default cases);
				 List.foreach
				 (cases,
				  fn target 
				   => join_liveTransferType (target, 
							     Case default)))
			      end
			   | _ => ());
		       label
		     end)

	    fun doit label
	      = let
		  val liveIn_distance
		    = List.keepAllMap
		      ((#get liveInfo) label,
		       fn temp
		        => if List.exists
		              (transferRegs,
			       fn register
			        => MemLoc.size temp = Register.size register)
			     then let
				    val (totF, numF)
				      = get_distancesF {temp = temp,
							label = label}

				    val (totB, numB)
				      = get_distancesB {temp = temp,
							label = label}

				    val (tot, num)
				      = (totF * numB + totB * numF,
					 numF * numB)

				    val d = tot div num
				  in
				    SOME (temp, d)
				  end
				  handle Overflow => SOME (temp, Int.maxInt div 2)
			     else NONE)

		  val liveIn_distance_sorted
		    = List.insertionSort
		      (liveIn_distance,
		       fn ((_,distance1),(_,distance2)) 
		        => distance1 < distance2)

		  val liveIn = List.map(liveIn_distance_sorted, #1)

		  val (_,liveTransfer)
		    = List.fold
		      (liveIn,
		       (transferRegs,[]),
		       fn (temp,(transferRegs,liveTransfer))
		        => case List.peek
		                (transferRegs,
				 fn register 
				  => MemLoc.size temp = Register.size register)
			     of SOME register
			      => (List.removeAll
				  (transferRegs,
				   fn register'
				    => Register.coincide(register,
							 register')),
				  (temp,register)::liveTransfer)
			      | NONE => (transferRegs,liveTransfer))
		in
		  setLiveTransferInfo(label, liveTransfer)
		end

	    fun doit' labels
	      = let
		  val liveIn_distance''
		    = List.concatMap
		      (labels,
		       fn label 
		        => List.keepAllMap
		           ((#get liveInfo) label,
			    fn temp
			     => if List.exists
			           (transferRegs,
				    fn register
				     => MemLoc.size temp = Register.size register)
				  then let
					 val (totF, numF)
					   = get_distancesF {temp = temp,
							     label = label}
					 val (totB, numB)
					   = get_distancesB {temp = temp,
							     label = label}

					 val (tot, num)
					   = (totF * numB + totB * numF,
					      numF * numB)

					 val d = tot div num
				       in
					 SOME (temp, d)
				       end
				  else NONE))

		  val liveIn_distance'
		    = List.fold
		      (liveIn_distance'',
		       [],
		       fn ((temp,distance),liveIn_distance')
		        => let
			     val rec add
			       = fn [] => [(temp,distance,1)]
			          | (temp',distance',num')::liveIn_distance'
			          => if MemLoc.eq(temp, temp')
				       then (temp',
					     distance' + distance,
					     num' + 1)::
					    liveIn_distance'
				       else (temp',
					     distance',
					     num')::
					    (add liveIn_distance')
			   in
			     add liveIn_distance'
			   end)

		  val liveIn_distance
		    = List.map
		      (liveIn_distance',
		       fn (temp,distance,num) => (temp, distance div num))

		  val liveIn_distance_sorted
		    = List.insertionSort
		      (liveIn_distance,
		       fn ((_,distance1),(_,distance2)) 
		        => distance1 < distance2)

		  val liveIn = List.map(liveIn_distance_sorted, #1)

		  val (_,liveTransfer)
		    = List.fold
		      (liveIn,
		       (transferRegs,[]),
		       fn (temp,(transferRegs,liveTransfer))
		        => case List.peek
		                (transferRegs,
				 fn register 
				  => MemLoc.size temp = Register.size register)
			     of SOME register
			      => (List.removeAll
				  (transferRegs,
				   fn register'
				    => Register.coincide(register,
							 register')),
				  (temp,register)::liveTransfer)
			      | NONE => (transferRegs,liveTransfer))
		in
		  List.foreach
		  (labels,
		   fn label
		    => let
			 val liveIn = (#get liveInfo) label

			 val liveTransfer
			   = List.keepAll
			     (liveTransfer,
			      fn (temp,register)
			       => List.contains(liveIn,
						temp,
						MemLoc.eq))
		       in
			 setLiveTransferInfo(label, liveTransfer)
		       end)
		end

	    val _
	      = List.foreach
	        (labels,
		 fn label
		  => case !(get_liveTransferType label)
		       of Normal => doit label
			| Default labels
			=> if List.forall
			      (labels,
			       fn label' 
			        => case !(get_liveTransferType label')
				     of Case label'' 
				      => Label.equals(label,label'')
				      | _ => false)
			     then doit' (label::labels)
			     else setLiveTransferInfo(label, [])
			| Case label' => ()
			| Top => setLiveTransferInfo(label, []))
	  in
	    liveTransferInfo
	  end

      val (computeLiveTransferInfo, computeLiveTransferInfo_msg)
	= tracer
	  "computeLiveTransferInfo"
	  computeLiveTransferInfo

      fun computeNoLiveTransferInfo ()
	= let
	    val liveTransferInfo
	     as {get = getLiveTransferInfo :
		       Label.t -> (MemLoc.t * Register.t) list,
		 set = setLiveTransferInfo}
	     = Property.getSet
	       (Label.plist,
		Property.initFun (fn label => []))
	  in
	    liveTransferInfo
	  end
    end

  structure GenerateTransfers =
    struct
      open Transfer
      open JumpInfo

      datatype gef = GEF of {generate : gef -> 
			                {label : Label.t,
					 begin : bool,
					 align : bool} -> 
					Assembly.t list,
			     effect : gef -> 
			              {transfer : Transfer.t,
				       profile_end : Assembly.t list} ->
                                      Assembly.t list,
			     fall : gef ->
			            {profile_end : Assembly.t list,
				     label : Label.t,
				     live : MemLoc.t list} ->
                                    Assembly.t list}

      fun generateTransfers {blocks : Block.t list,
			     exports : Label.t list,
			     optimize: int,
			     block_pre : Label.t -> Assembly.t list option,
			     block_begin : Assembly.t list,
			     block_end : Assembly.t list,
			     block_fall : Assembly.t list,
			     transferRegs : Register.t list,
			     liveInfo : {get : Label.t -> MemLoc.t list,
					 set : Label.t * MemLoc.t list 
					       -> unit},
			     jumpInfo : {get: Label.t -> status ref}}
	= let
	    val liveTransferInfo
	      = if !Control.Native.liveTransfer
		  then LiveTransferInfo.computeLiveTransferInfo
		       {blocks = blocks,
			transferRegs = transferRegs,
			liveInfo = liveInfo,
			jumpInfo = jumpInfo}
		  else LiveTransferInfo.computeNoLiveTransferInfo ()

	    val layoutInfo as {get : Label.t -> Block.t option,
			       set}
	      = Property.getSet(Label.plist, Property.initConst NONE)

	    val _ 
	      = List.foreach
	        (blocks,
		 fn block as Block.T {label,...}
		  => set(label, SOME block))

	    local
	      val queue = ref (Queue.empty ())
	    in
	      fun enque x = queue := Queue.enque(!queue, x)
	      fun deque () = case Queue.deque(!queue)
			       of NONE => NONE
				| SOME(x, queue') => (queue := queue';
						      SOME x)
	    end

	    fun generateAll (gef as GEF {generate,effect,fall})
	                    {label, begin, align} : Assembly.t list
	      = (case get label
		   of NONE => []
		    | SOME (Block.T {label, profileInfo,
				     statements, transfer})
		    => let
			 val _ = set(label, NONE)

			 val (profile_begin,profile_end)
			   = ProfileInfo.profile_begin_end profileInfo

			 val pre 
			   = if begin
			       then ((fn l
				       => if align
					    then (Assembly.pseudoop_p2align 2)::
					         l
					    else l) o
				     (fn l
				       => if List.contains(exports,
							   label,
							   Label.equals)
					    then (Assembly.pseudoop_global label)::
					         l
					    else l) o
				     (fn l
				       => case block_pre label
					    of NONE => l
					     | SOME assembly => List.concat
					                        [assembly, l]))
				    ((Assembly.label label)::
				     (List.fold
				      ((#get liveTransferInfo) label,
				       profile_begin @ block_begin,
				       fn ((temp,register),block_begin)
				        => (x86.Assembly.directive_assume
					    {register = register,
					     memloc = temp,
					     weight = 1024,
					     sync = false,
					     reserve = false})::
				           block_begin)))
			       else profile_begin

			 val statements = statements

			 val post 
			   = List.concat [block_end, 
					  [Assembly.directive_flush ()]]

			 val transfer = effect gef {transfer = transfer,
						    profile_end = profile_end}
		       in
			 List.concat [pre,
				      statements,
				      post,
				      transfer]
		       end)

	    fun effectDefault (gef as GEF {generate,effect,fall})
	                      {transfer, profile_end} : Assembly.t list
	      = (case transfer
		   of Assembly assembly 
		    => List.concat [assembly, profile_end]
	            | Goto {target}
		    => fall gef
		            {profile_end = profile_end,
			     label = target,
			     live = (#get liveInfo) target}
		    | Iff {condition, truee, falsee}
		    => let
			 val condition_neg 
			   = Instruction.condition_negate condition
			   
			 val truee_live = (#get liveInfo) truee
			 val truee_liveTransfer 
			   = (#get liveTransferInfo) truee
			 val truee_live
			   = List.removeAll
			     (truee_live,
			      fn temp => List.exists
			                 (truee_liveTransfer,
					  fn (temp',_) 
					   => MemLoc.eq(temp, temp')))

			 val falsee_live = (#get liveInfo) falsee
			 val falsee_liveTransfer
			   = (#get liveTransferInfo) falsee
			 val falsee_live
			   = List.removeAll
			     (falsee_live,
			      fn temp => List.exists
			                 (falsee_liveTransfer,
					  fn (temp',_) 
					   => MemLoc.eq(temp, temp')))

			 val common_live
			   = List.keepAll(truee_live,
					  fn temp
					   => List.contains(falsee_live,
							    temp,
							    MemLoc.eq))

			 val (truee_live,truee_live_length)
			   = List.fold
			     (truee_live,
			      ([],0),
			      fn (temp,(truee_live,truee_live_length))
			       => if List.contains(common_live,
						   temp,
						   MemLoc.eq)
				    then (truee_live,truee_live_length)
				    else (temp::truee_live,
					  1 + truee_live_length))
			 val truee_live_length
			   = truee_live_length + 
			     (List.length truee_liveTransfer)
			   

			 val (falsee_live,falsee_live_length)
			   = List.fold
			     (falsee_live,
			      ([],0),
			      fn (temp,(falsee_live,falsee_live_length))
			       => if List.contains(common_live,
						   temp,
						   MemLoc.eq)
				    then (falsee_live,falsee_live_length)
				    else (temp::falsee_live,
					  1 + falsee_live_length))
			 val falsee_live_length
			   = falsee_live_length + 
			     (List.length falsee_liveTransfer)

			 fun fall_truee ()
			   = (enque falsee;
			      List.concat
			      [(Assembly.directive_commit 
				{memlocs = List.concat [common_live, 
							falsee_live]})::
			       (List.map
				(falsee_liveTransfer,
				 fn (temp,register)
				  => Assembly.directive_cache
				     {register = register,
				      memloc = temp,
				      reserve = false})),
			       (Assembly.instruction_jcc
				{condition = condition_neg,
				 target = Operand.label falsee})::
			       (fall gef 
 			             {profile_end = profile_end,
				      label = truee,
				      live = truee_live})])

			 fun fall_falsee ()
			   = (enque truee;
			      List.concat
			      [(Assembly.directive_commit 
				{memlocs = List.concat [common_live,
							truee_live]})::
			       (List.map
				(truee_liveTransfer,
				 fn (temp,register)
				  => Assembly.directive_cache
				     {register = register,
				      memloc = temp,
				      reserve = false})),
			       (Assembly.instruction_jcc
				{condition = condition,
				 target = Operand.label truee})::
			       (fall gef
			             {profile_end = profile_end,
				      label = falsee,
				      live = falsee_live})])
		       in 
			 case (!((#get jumpInfo) truee),
			       !((#get jumpInfo) falsee))
			   of (Maybe 1, Maybe 1)
			    => if truee_live_length <= falsee_live_length
				 then fall_falsee ()
				 else fall_truee ()
			    | (Maybe 1, _)
			    => fall_truee ()
			    | (_, Maybe 1)
			    => fall_falsee ()
			    | _
			    => if truee_live_length <= falsee_live_length
				 then fall_falsee ()
				 else fall_truee ()
		       end
	            | Switch {test, cases, default}
		    => let
			 val size 
			   = case Operand.size test
			       of SOME size => size
				| NONE => Size.LONG

			 val default_live = (#get liveInfo) default
			 val default_liveTransfer
			   = (#get liveTransferInfo) default
			 val default_live
			   = List.removeAll
			     (default_live,
			      fn temp 
			       => List.exists
			          (default_liveTransfer,
				   fn (temp',_) 
				    => MemLoc.eq(temp, temp')))

			 val cases_cases_live_cases_liveTransfer
			   = Transfer.Cases.map'
			     (cases,
			      fn (k, target) 
			       => let
				    val target_live = (#get liveInfo) target
				    val target_liveTransfer
				      = (#get liveTransferInfo) target
				    val target_live
				      = List.removeAll
				        (target_live,
					 fn temp 
					  => List.exists
					     (target_liveTransfer,
					      fn (temp',_) 
					       => MemLoc.eq(temp, temp')))
				  in
				    ((k,target),
				     target_live,
				     target_liveTransfer)
				  end,
			      fn (c, target) => (Immediate.Char c, target),
			      fn (i, target) => (Immediate.Int i, target),
			      fn (w, target) => (Immediate.Word w, target))

			 val (cases, carry_live, carry_liveTransfer)
			   = List.fold
			     (cases_cases_live_cases_liveTransfer,
			      ([], [], []),
			      fn (((k,target), 
				   target_live, 
				   target_liveTransfer),
				  (cases, 
				   carry_live, 
				   carry_liveTransfer))
			       => let
				    val (target_live,
					 carry_live)
				      = List.fold
				        (target_live,
					 ([],carry_live),
					 fn (temp,
					     (target_live',
					      carry_live'))
					  => if List.contains
					        (carry_live,
						 temp,
						 MemLoc.eq)
					       then (target_live',
						     carry_live')
					       else (temp::target_live',
						     temp::carry_live'))

				    val (target_liveTransfer,
					 carry_liveTransfer)
				      = List.fold
				        (target_liveTransfer,
					 ([],carry_liveTransfer),
					 fn ((temp,register),
					     (target_liveTransfer',
					      carry_liveTransfer'))
					  => if List.contains
					        (carry_liveTransfer,
						 (temp,register),
						 fn ((t1,r1),(t2,r2))
						  => MemLoc.eq(t1,t2)
						     andalso
						     Register.eq(r1,r2))
					       then (target_liveTransfer',
						     carry_liveTransfer')
					       else ((temp,register)::
						     target_liveTransfer',
						     (temp,register)::
						     carry_liveTransfer'))
				  in
				    enque target;
				    (List.concat
				     [(Assembly.instruction_jcc
				       {condition = Instruction.E,
					target = Operand.label target})::
				      (Assembly.instruction_cmp
				       {src1 = test,
					src2 = Operand.immediate_const k,
					size = size})::	
				      (List.map
				       (target_liveTransfer,
					fn (temp,register)
					 => Assembly.directive_cache
					    {register = register,
					     memloc = temp,
					     reserve = true})),
				      (Assembly.directive_commit 
				       {memlocs = target_live})::
				      cases],
				     carry_live,
				     carry_liveTransfer)
				  end)

			 val default_live
			   = List.removeAll
			     (default_live,
			      fn temp 
			       => List.contains
			          (carry_live,
				   temp,
				   MemLoc.eq))

			 val cases
			   = List.fold
			     (carry_liveTransfer,
			      cases,
			      fn ((temp,register),cases)
			       => (Assembly.directive_unreserve
				   {register = register})::
			          cases)

			 val test = valOf (Operand.deMemloc test)
		       in 
			 (Assembly.directive_cache
			  {memloc = test,
			   register = Register.return (MemLoc.size test),
			   reserve = true})::
			 List.fold(cases,
				   (Assembly.directive_unreserve
				    {register = Register.return (MemLoc.size test)})::
			           (fall gef
			                 {profile_end = profile_end,
					  label = default,
					  live = default_live}),
				   op ::)
		      end)

	    fun effectJumpTable (gef as GEF {generate,effect,fall})
	                         {transfer, profile_end} : Assembly.t list
	      = case transfer
		  of Switch {test, cases, default}
		   => let
			fun reduce(cases,
				   ops as {zero,
					   even,
					   incFn, decFn, halfFn,
					   ltFn, gtFn,
					   min, minFn,
					   max, maxFn,
					   range})
			  = let
			      fun reduce' cases
				= let
				    val (minK,maxK,length,
					 allEven,allOdd)
				      = List.fold
				        (cases,
					 (max, min, 0,
					  true, true),
					 fn ((k,target),
					     (minK,maxK,length,
					      allEven,allOdd))
					  => let
					       val isEven = even k
					     in
					       (minFn(k,minK),
						maxFn(k,maxK),
						length + 1,
						allEven andalso isEven,
						allOdd andalso not isEven)
					     end)
				  in
				    if length > 1 andalso
				       (allEven orelse allOdd)
				      then let
					     val f = if allOdd
						       then halfFn o decFn
						       else halfFn
					     val cases' 
					       = List.map
					         (cases,
						  fn (k,target)
						   => (f k, target))

					     val (cases'', 
						  minK'', maxK'', length'',
						  shift'', mask'')
					       = reduce' cases'

					     val shift' = 1 + shift''
					     val mask' 
					       = Word.orb
					         (Word.<<(mask'', 0wx1),
						  if allOdd
						    then 0wx1
						    else 0wx0)
					   in
					     (cases'', 
					      minK'', maxK'', length'',
					      shift', mask')
					   end
				      else (cases, 
					    minK, maxK, length,
					    0, 0wx0)
				  end
			    in 
			      reduce' cases
			    end

			fun doitTable(cases,
				      ops as {zero,
					      even,
					      incFn, decFn, halfFn,
					      ltFn, gtFn,
					      min, minFn,
					      max, maxFn,
					      range},
				      minK, maxK, rangeK, shift, mask,
				      constFn)
			  = let
			      val jump_table_label
				= Label.newString "jumpTable"

			      val rec filler 
				= fn ([],_) => []
				   | (cases as (i,target)::cases',j)
				   => if i = j
					then (enque target;
					      (Immediate.label target)::
					      (filler(cases', incFn j)))
					else (Immediate.label default)::
					     (filler(cases, incFn j))

			      val _ = enque default
			      val jump_table = filler (cases, minK)

			      val index
				= MemLoc.imm
				  {base = Immediate.const_int 0,
				   index = Immediate.const_int 0,
				   scale = Scale.Four,
				   size = Size.LONG,
				   commit = MemLoc.Commit.commit
				            {isTemp = true,
					     onFlush = false},
				   class = MemLoc.Class.new "Temp"}
			      val check
				= MemLoc.imm
				  {base = Immediate.const_int 0,
				   index = Immediate.const_int 0,
				   scale = Scale.Four,
				   size = Size.LONG,
				   commit = MemLoc.Commit.commit
				            {isTemp = true,
					     onFlush = false},
				   class = MemLoc.Class.new "Temp"}

			      val address
				= MemLoc.basic
				  {base = Immediate.label jump_table_label,
				   index = index,
				   scale = Scale.Four,
				   size = Size.LONG,
				   commit = MemLoc.Commit.commit
				            {isTemp = true,
					     onFlush = false},
				   class = MemLoc.Class.Code}

			      val size 
				= case Operand.size test
				    of SOME size => size
				     | NONE => Size.LONG
			      val index = Operand.memloc index
			      val check = Operand.memloc check
			      val address = Operand.memloc address
			      val default = Operand.label default
			    in
			      ((fn l
				 => if Size.lt(size, Size.LONG)
				      then (Assembly.instruction_movx
					    {oper = Instruction.MOVZX,
					     src = test,
					     srcsize = size,
					     dst = index,
					     dstsize = Size.LONG})::
					   l
				      else (Assembly.instruction_mov
					    {src = test,
					     dst = index,
					     size = Size.LONG})::
					   l) o
			       (fn l
				 => if shift > 0
				      then ((fn l
					      => (Assembly.instruction_mov
						  {src = index,
						   dst = check,
						   size = Size.LONG})::
					         (Assembly.instruction_binal
						  {oper = Instruction.AND,
						   src = Operand.immediate_const_word (ones shift),
						   dst = check,
						   size = Size.LONG})::
						 l) o
					    (fn l
					      => if mask = 0wx0
						   then l
						   else (Assembly.instruction_binal
							 {oper = Instruction.SUB,
							  src = Operand.immediate_const_word mask,
							  dst = check,
							  size = Size.LONG})::
						        l) o
					    (fn l
					      => (Assembly.instruction_jcc
						  {condition = Instruction.NZ,
						   target = default})::
					         (Assembly.instruction_sral
						  {oper = Instruction.SAR,
						   count = Operand.immediate_const_int shift,
						   dst = index,
						   size = Size.LONG})::
						 l)) 
					   l
				      else l) o
			       (fn l
				 => if minK = zero
				      then l
				      else (Assembly.instruction_binal
					    {oper = Instruction.SUB,
					     src = Operand.immediate 
					           (constFn minK),
					     dst = index,
					     size = Size.LONG})::
					   l) o
			       (fn l
				 => (Assembly.instruction_cmp
				     {src1 = index,
				      src2 = Operand.immediate_const_int rangeK,
				      size = size})::
				    (Assembly.instruction_jcc
				     {condition = Instruction.AE,
				      target = default})::
				    (Assembly.instruction_jmp
				     {target = address,
				      absolute = true})::
				    l))
			      (List.concat
			       [profile_end,
				[Assembly.pseudoop_data (),
				 Assembly.pseudoop_p2align 2,
				 Assembly.label jump_table_label,
				 Assembly.pseudoop_long jump_table,
				 Assembly.pseudoop_text ()]])
			    end

			fun doit(cases,
				 ops as {zero,
					 even,
					 incFn, decFn, halfFn,
					 ltFn, gtFn,
					 min, minFn,
					 max, maxFn,
					 range},
				 constFn)
			  = let
			      val (cases, 
				   minK, maxK, length,
				   shift, mask) 
				= reduce(cases, ops)

			      val rangeK 
				= SOME (range(minK,maxK))
                                  handle Overflow => NONE
			    in
			      if length >= 8 
				 andalso
				 (isSome rangeK
				  andalso
				  valOf rangeK <= 2 * length)
				then let
				       val rangeK = valOf rangeK

				       val default_live 
					 = (#get liveInfo) default
				       val default_liveTransfer
					 = (#get liveTransferInfo) default
				       val default_live
					 = List.removeAll
					   (default_live,
					    fn temp 
					     => List.exists
					        (default_liveTransfer,
						 fn (temp',_) 
						  => MemLoc.eq(temp, temp')))

				       val cases 
					 = List.insertionSort
					   (cases, 
					    fn ((k,target),(k',target')) 
					     => ltFn(k,k'))
				     
				       val cases_live_cases_liveTransfer
					 = List.map
					   (cases,
					    fn (k, target)
					     => let
						  val target_live 
						    = (#get liveInfo) target
						  val target_liveTransfer
						    = (#get liveTransferInfo) 
						      target
						  val target_live
						    = List.removeAll
						      (target_live,
						       fn temp 
						        => List.exists
						           (target_liveTransfer,
							    fn (temp',_) 
							     => MemLoc.eq(temp, temp')))
						in 
						  (target_live,
						   target_liveTransfer)
						end)

				       val (live,liveTransfer)
					 = List.fold
					   (cases_live_cases_liveTransfer,
					    (default_live,
					     default_liveTransfer),
					    fn ((target_live,
						 target_liveTransfer),
						(live,
						 liveTransfer))
					     => let
						  val live
						    = List.fold
						      (target_live,
						       live,
						       fn (temp,
							   live)
						        => if List.contains
						              (live,
							       temp,
							       MemLoc.eq)
							     then live
							     else temp::live)

						  val liveTransfer
						    = List.fold
						      (target_liveTransfer,
						       liveTransfer,
						       fn ((temp,register),
							   liveTransfer)
						        => if List.contains
						              (liveTransfer,
							       (temp,register),
							       fn ((t1,r1),(t2,r2))
							        => MemLoc.eq(t1,t2)
							           andalso
								   Register.eq(r1,r2))
							     then liveTransfer
							     else (temp,register)::
							          liveTransfer)
						in 
						  (live,liveTransfer)
						end)
					   

				       val test = valOf (Operand.deMemloc test)
				     in 
				       List.concat
				       [(Assembly.directive_commit
					 {memlocs = live})::
					(Assembly.directive_cache
					 {memloc = test,
					  register 
					  = Register.return (MemLoc.size test),
					  reserve = true})::
					(List.map
					 (liveTransfer,
					  fn (temp,register)
					   => Assembly.directive_cache
					      {register = register,
					       memloc = temp,
					       reserve = true})),
					(doitTable(cases, 
						   ops,
						   minK, maxK, rangeK,
						   shift, mask,
						   constFn))]
				     end
				else effectDefault gef
				                   {transfer = transfer,
						    profile_end = profile_end}
			    end
		      in
			case cases
			  of Transfer.Cases.Char cases
			   => doit
			      (cases,
			       {zero = #"\000",
				even = fn c => (Char.ord c) mod 2 = 0,
				incFn = Char.succ,
				decFn = Char.pred,
				halfFn = fn c => Char.chr((Char.ord c) div 2),
				ltFn = Char.<,
				gtFn = Char.>,
				min = Char.minChar,
				minFn = Char.min,
				max = Char.maxChar,
				maxFn = Char.max,
				range = fn (min,max) => ((Char.ord max) - 
							 (Char.ord min)) + 1},
			       Immediate.const_char)
			   | Transfer.Cases.Int cases
			   => doit
			      (cases,
			       {zero = 0,
				even = fn i => i mod 2 = 0,
				incFn = fn i => i + 1,
				decFn = fn i => i - 1,
				halfFn = fn i => i div 2,
				ltFn = Int.<,
				gtFn = Int.>,
				min = Int.minInt,
				minFn = Int.min,
				max = Int.maxInt,
				maxFn = Int.max,
				range = fn (min,max) => max - min + 1},
			       Immediate.const_int)
			   | Transfer.Cases.Word cases
			   => doit
			      (cases,
			       {zero = 0wx0,
				even = fn w => Word.mod(w,0wx2) = 0wx0,
				incFn = fn x => Word.+(x,0wx1),
				decFn = fn x => Word.-(x,0wx1),
				halfFn = fn x => Word.div(x,0wx2),
				ltFn = Word.<,
				gtFn = Word.>,
				min = 0wx0,
				minFn = Word.min,
				max = 0wxFFFFFFFF,
				maxFn = Word.max,
				range = fn (min,max) => ((Word.toInt max) -
							 (Word.toInt min) + 
							 1)},
			       Immediate.const_word)
		      end
		   | _ => effectDefault gef 
		                        {transfer = transfer,
					 profile_end = profile_end}

	    fun fallNone (gef as GEF {generate,effect,fall})
	                 {profile_end, label, live} : Assembly.t list
	      = let
		  val liveTransfer = (#get liveTransferInfo) label

		  val live 
		    = List.removeAll
		      (live,
		       fn temp 
		        => List.exists
		           (liveTransfer,
			    fn (temp',_) 
			     => MemLoc.eq(temp, temp')))

		  fun default ()
		    = List.concat
		      [(Assembly.directive_commit
			{memlocs = live})::
		       (List.map
			(liveTransfer,
			 fn (temp,register)
			  => Assembly.directive_cache
			     {register = register,
			      memloc = temp,
			      reserve = false})),
		       (Assembly.instruction_jmp
			{target = Operand.label label,
			 absolute = false})::
		       profile_end]
		in
		  case get label
		    of NONE
		     => default ()
		     | SOME (block as Block.T {label,...})
		     => (enque label;
			 default ())
		end

	    fun fallDefault (gef as GEF {generate,effect,fall})
	                    {profile_end, label, live} : Assembly.t list
	      = let
		  val liveTransfer = (#get liveTransferInfo) label

		  val live 
		    = List.removeAll
		      (live,
		       fn temp 
		        => List.exists
		           (liveTransfer,
			    fn (temp',_) 
			     => MemLoc.eq(temp, temp')))

		  fun default ()
		    = List.concat
		      [(Assembly.directive_commit
			{memlocs = live})::
		       (List.map
			(liveTransfer,
			 fn (temp,register)
			  => Assembly.directive_cache
			     {register = register,
			      memloc = temp,
			      reserve = false})),
		       (Assembly.instruction_jmp
			{target = Operand.label label,
			 absolute = false})::
		       profile_end]
		in
		  case get label
		    of NONE 
		     => default ()
		     | SOME (block as Block.T {label,...})
		     => (case block_pre label
			   of SOME _ 
			    => (enque label;
				default ())
			    | NONE 
			    => (case !((#get jumpInfo) label)
				  of Maybe 1 
				   => List.concat
				      [block_fall,
				       profile_end,
				       generate gef
				                {label = label,
					 	 begin = false,
						 align = false}]
				   | _ => List.concat
				          [(Assembly.directive_commit 
					    {memlocs = live})::
					   (List.map
					    (liveTransfer,
					     fn (temp,register)
					      => Assembly.directive_cache
					         {register = register,
						  memloc = temp,
						  reserve = false})),
					   (Assembly.directive_reset ())::
					   (List.concat
					    [profile_end,
					     (generate gef
					               {label = label,
							begin = true,
							align = false})])]))
		end

	    fun make {generate, effect, fall}
	      = generate (GEF {generate = generate,
			       effect = effect,
			       fall = fall})
			 
	    val generate
	      = case optimize 
		  of 0 => make {generate = generateAll,
				effect = effectDefault,
				fall = fallNone}
		   | _ => make {generate = generateAll,
				effect = effectJumpTable,
				fall = fallDefault}

	    val _ = List.foreach(exports, fn label => enque label)
	    fun doit () : Assembly.t list list
	      = (case deque ()
		   of NONE => []
		    | SOME label
		    => (case generate {label = label,
				       begin = true,
				       align = true}
			  of [] => doit ()
			   | block => block::(doit ())))
	    val assembly = doit ()
       	  in
	    assembly
	  end

      val (generateTransfers, generateTransfers_msg)
	= tracer
          "generateTransfers"
	  generateTransfers
    end

  fun simplify {chunk as Chunk.T {exports, blocks}: Chunk.t,
		optimize : int,
		block_pre : Label.t -> Assembly.t list option,
		block_begin : Assembly.t list,
		block_end : Assembly.t list,
		block_fall : Assembly.t list,
		transferRegs : Register.t list,
		liveInfo : {get : Label.t -> MemLoc.t list,
			    set : Label.t * MemLoc.t list -> unit}} :
               Assembly.t list list
    = let
	val labels
	  = List.map(blocks, fn Block.T {label,...} => label)

	fun changed_msg(changed,msg)
(*
	  = Control.messageStr ("completed " ^ msg)
*)
(*
	  = if changed
	      then Control.messageStr (msg ^ " changed")
	      else ()
*)
	  = ()

	(*********************************************************************)
	(* verifyLiveInfo                                                    *)
	(*********************************************************************)
	val {changed} = Liveness.verifyLiveInfo {blocks = blocks,
						 liveInfo = liveInfo}
	val _ = changed_msg(changed, "verifyLiveInfo")
	  
	(*********************************************************************)
	(* computeJumpInfo                                                   *)
	(*********************************************************************)
	val jumpInfo
	  = JumpInfo.computeJumpInfo {blocks = blocks,
				      exports = exports,
				      block_pre = block_pre}


	(*********************************************************************)
	(* optimizer                                                         *)
	(*********************************************************************)
	fun optimizer blocks
	  = let
	       val blocks = blocks
	       val changed = false

	       (**************************************************************)
	       (* elimGoto                                                   *)
	       (**************************************************************)
	       val {blocks = blocks',
		    changed = changed'}
		 = ElimGoto.elimGoto {blocks = blocks,
				      jumpInfo = jumpInfo}

	       val _
		 = Assert.assert
		   ("verifyJumpInfo",
		    fn () => JumpInfo.verifyJumpInfo {blocks = blocks',
						      exports = exports,
						      block_pre = block_pre,
						      jumpInfo = jumpInfo})

	       val blocks = blocks'
	       val _ 
		 = changed_msg
		   (changed', 
		    "elimGoto")
	       val changed = changed orelse changed'		 

	       (**************************************************************)
	       (* peepholeBlock/moveHoist/peepholeLivenessBlock/copyPropagate*)
	       (**************************************************************)
	       val {blocks = blocks',
		    changed = changed'}
		 = List.fold
		   (blocks,
		    {blocks = [], changed = false},
		    fn (block, {blocks, changed})
		     => let
			  val block = block
			  val changed = changed

			  (***************************************************)
			  (* peepholeBlock_pre                               *)
			  (***************************************************)
			  val {block = block',
			       changed = changed'}
			    = PeepholeBlock.peepholeBlock_pre
                              block

			  val block = block'
			  val _ 
			    = changed_msg
			      (changed', 
			       "peepholeBlock_pre")
			  val changed = changed orelse changed'

			  (***************************************************)
			  (* toLivenessBlock                                 *)
			  (***************************************************)
			  val block'
			    = LivenessBlock.toLivenessBlock 
			      {block = block,
			       liveInfo = liveInfo}
			  val changed' = false

			  val block = block'
			  val block_toLivenessBlock = block
			  val _ 
			    = changed_msg
			      (changed', 
			       "toLivenessBlock")
			  val changed = changed orelse changed'

			  (***************************************************)
			  (* moveHoist                                       *)
			  (***************************************************)
			  val {block = block', 
			       changed = changed'}
			    = if !Control.Native.moveHoist
				then MoveHoistLivenessBlock.moveHoist
				     {block = block}
				else {block = block,
				      changed = false}

			  val _
			    = Assert.assert
			      ("verifyLivenessBlock: moveHoist",
			       fn () => LivenessBlock.verifyLivenessBlock
			                {block = block',
					 liveInfo = liveInfo})

			  val block = block'
			  val block_moveHoist = block
			  val _ 
			    = changed_msg
			      (changed', 
			       "moveHoistLivenessBlock")
			  val changed = changed orelse changed'

			  (***************************************************)
			  (* peepholeLivenessBlock                           *)
			  (***************************************************)
			  val {block = block',
			       changed = changed'}
			    = PeepholeLivenessBlock.peepholeLivenessBlock
			      block

			  val _
			    = Assert.assert
			      ("verifyLivenessBlock: peepholeLivenessBlock",
			       fn () => LivenessBlock.verifyLivenessBlock
			                {block = block',
					 liveInfo = liveInfo})

			  val block = block'
			  val block_peepholeLivenessBlock = block
			  val _ 
			    = changed_msg
			      (changed', 
			       "peepholeLivenessBlock")
			  val changed = changed orelse changed'

			  (***************************************************)
			  (* copyPropagate                                   *)
			  (***************************************************)
			  val {block = block', 
			       changed = changed'}
			    = if !Control.Native.copyProp
				then CopyPropagateLivenessBlock.copyPropagate
				     {block = block,
				      liveInfo = liveInfo}
				else {block = block,
				      changed = false}

			  val _
			    = Assert.assert
			      ("verifyLivenessBlock: copyPropagate",
			       fn () => LivenessBlock.verifyLivenessBlock
			                {block = block',
					 liveInfo = liveInfo})

			  val block = block'
			  val block_copyPropagate = block
			  val _ 
			    = changed_msg
			      (changed', 
			       "copyPropagate")
			  val changed = changed orelse changed'

			  (***************************************************)
			  (* peepholeLivenessBlock_minor                     *)
			  (***************************************************)
			  val {block = block',
			       changed = changed'}
			    = PeepholeLivenessBlock.peepholeLivenessBlock_minor
			      block

			  val _
			    = Assert.assert
			      ("verifyLivenessBlock: peepholeLivenessBlock_minor",
			       fn () => LivenessBlock.verifyLivenessBlock
			                {block = block',
					 liveInfo = liveInfo})

			  val block = block'
			  val block_peepholeLivenessBlock_minor = block
			  val _ 
			    = changed_msg
			      (changed', 
			       "peepholeLivenessBlock_minor")
			  val changed = changed orelse changed'

			  (***************************************************)
			  (* toBlock                                         *)
			  (***************************************************)
			  val block'
			    = LivenessBlock.toBlock block
			  val changed' = false

			  val block = block'
			  val block_toBlock = block
			  val _ = changed_msg(changed', "toBlock")
			  val changed = changed orelse changed'

			  (***************************************************)
			  (* peepholeBlock_post                              *)
			  (***************************************************)
			  val {block = block',
			       changed = changed'}
			    = PeepholeBlock.peepholeBlock_post
                              block

			  val block = block'
			  val block_peepholeBlock_post = block
			  val _ = changed_msg(changed', "peepholeBlock_post")
			  val changed = changed orelse changed'
			in
			  {blocks = block::blocks,
			   changed = changed}
			end)

	       val blocks = blocks'
	       val _ = changed_msg(changed', "peepholeBlock/moveHoist/peepholeLivenessBlock/copyPropagate")
	       val changed = changed orelse changed'

	       (**************************************************************)
	       (* verifyLiveInfo                                             *)
	       (**************************************************************)
	       val {changed = changed'} 
		 = Liveness.verifyLiveInfo {blocks = blocks,
					    liveInfo = liveInfo}
	       val _ = changed_msg(changed, "verifyLiveInfo")
	       val changed = changed orelse changed'
	    in
	      {blocks = blocks,
	       changed = changed}
	    end


	(*********************************************************************)
	(* optimizer_loop                                                    *)
	(*********************************************************************)
	fun optimizer_loop blocks
	  = let
	      fun loop (blocks,changed)
		= let
		    val {blocks,
			 changed = changed'}
		      = optimizer blocks
		  in
		    if changed'
		      then loop (blocks, true)
		      else (blocks,changed)
		  end

	      val (blocks,changed) = loop (blocks,false)
	    in
	      {blocks = blocks,
	       changed = changed}
	    end


	(*********************************************************************)
	(* blocks                                                            *)
	(*********************************************************************)
	val {blocks,...}
	  = case optimize
	      of 0 => {blocks = blocks, changed = false}
	       | 1 => optimizer blocks
	       | _ => optimizer_loop blocks

	(*********************************************************************)
	(* assembly                                                          *)
	(*********************************************************************)
	val assembly
	  = GenerateTransfers.generateTransfers
	    {blocks = blocks,
	     exports = exports,
	     optimize = optimize,
	     block_pre = block_pre,
	     block_begin = block_begin,
	     block_end = block_end,
	     block_fall = block_fall,
	     transferRegs = transferRegs,
	     liveInfo = liveInfo,
	     jumpInfo = jumpInfo}

	val _ = List.foreach(labels, Label.clear)
      in
	assembly
      end

  val (simplify, simplify_msg)
    = tracer
      "simplify"
      simplify

  fun simplify_totals ()
    = (simplify_msg ();
       Control.indent ();
       Liveness.verifyLiveInfo_msg ();
       JumpInfo.computeJumpInfo_msg ();
       ElimGoto.elimGoto_msg ();
       JumpInfo.verifyJumpInfo_msg ();
       PeepholeBlock.peepholeBlock_pre_msg ();
       LivenessBlock.toLivenessBlock_msg ();
       MoveHoistLivenessBlock.moveHoist_msg ();
       PeepholeLivenessBlock.peepholeLivenessBlock_msg ();
       CopyPropagateLivenessBlock.copyPropagate_msg ();
       PeepholeLivenessBlock.peepholeLivenessBlock_minor_msg ();
       LivenessBlock.verifyLivenessBlock_msg ();
       LivenessBlock.toBlock_msg ();
       PeepholeBlock.peepholeBlock_post_msg ();
       GenerateTransfers.generateTransfers_msg ();
       Control.indent ();
       LiveTransferInfo.computeLiveTransferInfo_msg ();
       Control.unindent ();
       Control.unindent ())
end
