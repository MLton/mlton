(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor x86Simplify(S: X86_SIMPLIFY_STRUCTS): X86_SIMPLIFY =
struct

  open S
  open x86

  val rec ones : int -> word
    = fn 0 => 0wx0
       | n => Word.orb(Word.<<(ones (n-1), 0wx1),0wx1)

  val tracer = x86.tracer
  val tracerTop = x86.tracerTop

  structure PeepholeBlock =
    struct
      structure Peephole
	= Peephole(type entry_type = Entry.t
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
	  = fn Assembly.Instruction (Instruction.MOV {...})
	     => true
	     | _ => false

	val isInstructionBinALMD : statement_type -> bool
	  = fn Assembly.Instruction (Instruction.BinAL {...})
	     => true
	     | Assembly.Instruction (Instruction.pMD {...})
	     => true
	     | Assembly.Instruction (Instruction.IMUL2 {...})
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
	  = fn {entry,
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
			       {entry = entry,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		  else NONE
	     | {entry,
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
			       {entry = entry,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		  else NONE
	     | {entry,
		profileInfo,
		start, 
		statements as
		[[Assembly.Instruction (Instruction.MOV
					{src = src1,
					 dst = dst1, 
					 size = size1})],
		 comments,
		 [Assembly.Instruction (Instruction.IMUL2
					{src = src2,
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
			     (Assembly.instruction_imul2
			      {src = dst1,
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
			       {entry = entry,
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
	  = fn Assembly.Instruction (Instruction.pFMOV {...})
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
	  = fn {entry,
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
			       {entry = entry,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		  else NONE
	     | {entry,
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
			       {entry = entry,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		  else NONE
	     | {entry,
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
			       {entry = entry,
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
				     {oper, src, dst, ...})
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
				     {oper, src, dst, ...})
	     => ((oper = Instruction.IMUL)
		 orelse
		 (oper = Instruction.MUL))
	        andalso
		(case (Operand.deMemloc src,
		       Operand.deMemloc dst)
		   of (SOME src, SOME dst)
		    => not (List.exists
			    (src::(MemLoc.utilized src),
			     fn memloc => MemLoc.mayAlias(memloc, dst)))
	            | _ => true)
	     | Assembly.Instruction (Instruction.IMUL2
				     {src, dst, ...})
	     => (case (Operand.deMemloc src,
		       Operand.deMemloc dst)
		   of (SOME src, SOME dst)
		    => not (List.exists
			    (src::(MemLoc.utilized src),
			     fn memloc => MemLoc.mayAlias(memloc, dst)))
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
	  = fn {entry,
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
				     {entry = entry,
				      profileInfo = profileInfo,
				      statements = statements,
				      transfer = transfer})
			     end
			  | _ => NONE
		  else NONE
	     | {entry,
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
				     {entry = entry,
				      profileInfo = profileInfo,
				      statements = statements,
				      transfer = transfer})
			     end
			  | _ => NONE
		  else NONE
	     | {entry,
		profileInfo,
		start, 
		statements as
		[[Assembly.Instruction (Instruction.MOV
					{src = src1, 
					 dst = dst1, 
					 size = size1})],
		 comments,
		 [Assembly.Instruction (Instruction.IMUL2
					{src = src2,
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
				   (Assembly.instruction_imul2
				    {src = src1,
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
				     {entry = entry,
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
	val isInstructionMOV_srcImmediate0 : statement_type -> bool
	  = fn Assembly.Instruction (Instruction.MOV
				     {src = Operand.Immediate immediate,
				      ...})
	     => Immediate.zero immediate
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
	  = fn {entry,
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
			       {entry = entry,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		  else NONE
	     | {entry,
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
			       {entry = entry,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		  else NONE
	     | {entry,
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
			       {entry = entry,
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
	val isInstructionMOV : statement_type -> bool
	  = fn Assembly.Instruction (Instruction.MOV {...})
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
		Immediate.zero immediate
	     | _ => false

	val template : template
	  = {start = EmptyOrNonEmpty,
	     statements = [One isInstructionMOV,
			   All isComment,
			   One isInstructionBinAL_srcImmediate0],
	     finish = EmptyOrNonEmpty,
	     transfer = fn _ => true}

	val rewriter : rewriter
	  = fn {entry,
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
			       {entry = entry,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		  else NONE
	     | {entry,
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
			       {entry = entry,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		  else NONE
	     | {entry,
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
			       {entry = entry,
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
	      isSome (getImmediate1 (Immediate.destruct immediate))
	   | _ => false

	val template : template 
	  = {start = EmptyOrNonEmpty,
	     statements = [One isInstructionADDorSUB_srcImmediate1],
	     finish = EmptyOrNonEmpty,
	     transfer = fn _ => true}

	val rewriter : rewriter
	  = fn {entry,
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
		    = case (oper, getImmediate1 (Immediate.destruct immediate))
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
			{entry = entry,
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
	fun divTemp size
	  = MemLoc.imm {base = Immediate.label (Label.fromString "divTemp"),
			index = Immediate.const_int 0,
			scale = Scale.Four,
			size = size,
			class = MemLoc.Class.Temp}

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
	       isImmediatePow2 (Immediate.destruct immediate)
 	    | Assembly.Instruction (Instruction.IMUL2
				    {src = Operand.Immediate immediate,
				     ...})
	    => isImmediatePow2 (Immediate.destruct immediate)
	    | _ => false

	val template : template 
	  = {start = EmptyOrNonEmpty,
	     statements 
	     = [One isInstructionMULorDIV_srcImmediatePow2,
		All isComment],
	     finish = EmptyOrNonEmpty,
	     transfer = fn _ => true}

	val rewriter : rewriter
	  = fn {entry,
		profileInfo,
		start, 
		statements as
		[[Assembly.Instruction (Instruction.pMD 
					{oper = Instruction.IMUL, 
					 src = Operand.Immediate immediate, 
					 dst, 
					 size})],
		 comments],
		finish as [], 
		transfer as Transfer.Iff {condition,
					  truee,
					  falsee}}
	     => (case getImmediateLog2 (Immediate.destruct immediate)
		   of NONE => Error.bug "Peephole: elimMDPow2"
		    | SOME (0,false)
		    => let
			 val transfer
			   = case condition
			       of Instruction.O 
				=> Transfer.Goto {target = falsee}
				| Instruction.NO 
				=> Transfer.Goto {target = truee}
				| _ => Error.bug "Peephole: elimMDPow2"
				 
			 val statements
			   = List.fold(start,
				       comments,
				       op ::)
		       in
			 SOME (Block.T
			       {entry = entry,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		    | SOME (0,true)
		    => let
			 val statements
			   = List.fold
			     (start,
			      (Assembly.instruction_unal
			       {oper = Instruction.NEG,
				dst = dst,
				size = size})::
			      comments,
			      op ::)
		       in 
			 SOME (Block.T
			       {entry = entry,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		    | SOME (1,b)
		    => let
			 val statements
			   = List.fold
			     (start,
			      (fn l
			        => if b
				     then (Assembly.instruction_unal
					   {oper = Instruction.NEG,
					    dst = dst,
					    size = size})::
				          l
				     else l)
			      ((Assembly.instruction_binal
				{oper = Instruction.ADD,
				 src = dst,
				 dst = dst,
				 size = size})::
			       comments),
			      op ::)
		       in
			 SOME (Block.T
			       {entry = entry,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		    | _ => NONE)
	     | {entry,
		profileInfo,
		start, 
		statements as
		[[Assembly.Instruction (Instruction.pMD 
					{oper = Instruction.IMUL, 
					 src = Operand.Immediate immediate, 
					 dst, 
					 size})],
		 comments],
		finish, 
		transfer}
	     => (case getImmediateLog2 (Immediate.destruct immediate)
		   of NONE => Error.bug "Peephole: elimMDPow2"
		    | SOME (0,false) 
		    => SOME (Block.T
			     {entry = entry,
			      profileInfo = profileInfo,
			      statements = List.fold(start,
						     List.concat [comments, finish],
						     op ::),
			      transfer = transfer})
		    | SOME (0,true)
		    => let
			 val statements
			   = (Assembly.instruction_unal
			      {oper = Instruction.NEG,
			       dst = dst,
			       size = size})::
			     (List.concat [comments, finish])

			 val statements
			   = List.fold(start, 
				       statements,
				       op ::)
		       in
			 SOME (Block.T
			       {entry = entry,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		    | SOME (1,b)
		    => let
			 val statements
			   = List.fold
			     (start,
			      (fn l
			        => if b
				     then (Assembly.instruction_unal
					   {oper = Instruction.NEG,
					    dst = dst,
					    size = size})::
				          l
				     else l)
			      ((Assembly.instruction_binal
				{oper = Instruction.ADD,
				 src = dst,
				 dst = dst,
				 size = size})::
			       (List.concat [comments, finish])),
			      op ::)
		       in
			 SOME (Block.T
			       {entry = entry,
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
                                    (List.concat [comments, finish])

				val statements
				  = List.fold(start,
					      statements,
					      op ::)
			      in
				SOME (Block.T
				      {entry = entry,
				       profileInfo = profileInfo,
				       statements = statements,
				       transfer = transfer})
			      end
			 else NONE)
	     | {entry,
		profileInfo,
		start, 
		statements as
		[[Assembly.Instruction (Instruction.pMD 
					 {oper = Instruction.MUL, 
					  src = Operand.Immediate immediate, 
					  dst, 
					  size})],
		 comments],
		finish, 
		transfer}
	     => (case getImmediateLog2 (Immediate.destruct immediate)
		   of NONE => Error.bug "Peephole: elimMDPow2"
		    | SOME (0,false) 
		    => SOME (Block.T
			     {entry = entry,
			      profileInfo = profileInfo,
			      statements = List.fold(start,
						     List.concat [comments, finish],
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
				    (List.concat [comments, finish])

				val statements
				  = List.fold(start, 
					      statements,
					      op ::)
			      in 
				SOME (Block.T
				      {entry = entry,
				       profileInfo = profileInfo,
				       statements = statements,
				       transfer = transfer})
			      end
			 else NONE
		    | SOME (i,true)
		    => NONE)
	     | {entry,
		profileInfo,
		start, 
		statements as
		[[Assembly.Instruction (Instruction.pMD
					{oper = Instruction.IDIV,
					 src = Operand.Immediate immediate, 
					 dst, 
					 size})],
		 comments],
		finish, 
		transfer}
	     => (case getImmediateLog2 (Immediate.destruct immediate)
		   of NONE => Error.bug "Peephole: elimMDPow2"
		    | SOME (0,false) 
		    => SOME (Block.T
			     {entry = entry,
			      profileInfo = profileInfo,
			      statements = List.fold(start,
						     List.concat [comments, finish],
						     op ::),
			      transfer = transfer})
		    | SOME (0,true)
		    => let
			 val statements
			   = (Assembly.instruction_unal
			      {oper = Instruction.NEG,
			       dst = dst,
			       size = size})::
			     (List.concat [comments, finish])

			 val statements
			   = List.fold(start, 
				       statements,
				       op ::)
		       in
			 SOME (Block.T
			       {entry = entry,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		    | SOME (i,b)
		    => if i < (8 * Size.toBytes size)
			 then let
				val divTemp = Operand.MemLoc (divTemp size)
				val width = 8 * Size.toBytes size

				val statements
				  = ((fn l
				       => (Assembly.instruction_mov
					   {src = dst,
					    dst = divTemp,
					    size = size})::
				          l) o
				     (fn l
				       => if i > 0
					    then (Assembly.instruction_sral
						  {oper = Instruction.SAR,
						   dst = divTemp,
						   count 
						   = Operand.immediate_const_int 
						     (i - 1),
						   size = size})::
					         l
					    else l) o
				     (fn l
				       => if i < width
					    then (Assembly.instruction_sral
						  {oper = Instruction.SHR,
						   dst = divTemp,
						   count 
						   = Operand.immediate_const_int 
						     (width - i),
						   size = size})::
					         l
					    else l) o
				     (fn l
				       => (Assembly.instruction_binal
					   {oper = Instruction.ADD,
					    src = divTemp,
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
				    (List.concat [comments, finish])

				val statements
				  = List.fold(start,
					      statements,
					      op ::)
			      in 
				SOME (Block.T
				      {entry = entry,
				       profileInfo = profileInfo,
				       statements = statements,
				       transfer = transfer})
			      end
			 else NONE)
	     | {entry,
		profileInfo,
		start, 
		statements as
		[[Assembly.Instruction (Instruction.pMD
					{oper = Instruction.DIV,
					 src = Operand.Immediate immediate, 
					 dst, 
					 size})],
		 comments],
		finish, 
		transfer}
	     => (case getImmediateLog2 (Immediate.destruct immediate)
		   of NONE => Error.bug "Peephole: elimMDPow2"
		    | SOME (0,false) 
		    => SOME (Block.T
			     {entry = entry,
			      profileInfo = profileInfo,
			      statements = List.fold(start,
						     List.concat [comments, finish],
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
				    (List.concat [comments, finish])

				val statements
				  = List.fold(start,
					      statements,
					      op ::)
			      in 
				SOME (Block.T
				      {entry = entry,
				       profileInfo = profileInfo,
				       statements = statements,
				       transfer = transfer})
			      end
			 else NONE
	            | SOME (i,true) => NONE)
	     | {entry,
		profileInfo,
		start, 
		statements as
		[[Assembly.Instruction (Instruction.IMUL2
					{src = Operand.Immediate immediate, 
					 dst, 
					 size})],
		 comments],
		finish as [], 
		transfer as Transfer.Iff {condition,
					  truee,
					  falsee}}
	     => (case getImmediateLog2 (Immediate.destruct immediate)
		   of NONE => Error.bug "Peephole: elimMDPow2"
		    | SOME (0,false)
		    => let
			 val transfer
			   = case condition
			       of Instruction.O 
				=> Transfer.Goto {target = falsee}
				| Instruction.NO 
				=> Transfer.Goto {target = truee}
				| _ => Error.bug "Peephole: elimMDPow2"
				 
			 val statements
			   = List.fold(start,
				       comments,
				       op ::)
		       in
			 SOME (Block.T
			       {entry = entry,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		    | SOME (0,true)
		    => let
			 val statements
			   = List.fold
			     (start,
			      (Assembly.instruction_unal
			       {oper = Instruction.NEG,
				dst = dst,
				size = size})::
			      comments,
			      op ::)
		       in 
			 SOME (Block.T
			       {entry = entry,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		    | SOME (1,b)
		    => let
			 val statements
			   = List.fold
			     (start,
			      (fn l
			        => if b
				     then (Assembly.instruction_unal
					   {oper = Instruction.NEG,
					    dst = dst,
					    size = size})::
				          l
				     else l)
			      ((Assembly.instruction_binal
				{oper = Instruction.ADD,
				 src = dst,
				 dst = dst,
				 size = size})::
			       comments),
			      op ::)
		       in
			 SOME (Block.T
			       {entry = entry,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		    | _ => NONE)
	     | {entry,
		profileInfo,
		start, 
		statements as
		[[Assembly.Instruction (Instruction.IMUL2
					{src = Operand.Immediate immediate, 
					 dst, 
					 size})],
		 comments],
		finish, 
		transfer}
	     => (case getImmediateLog2 (Immediate.destruct immediate)
		   of NONE => Error.bug "Peephole: elimMDPow2"
		    | SOME (0,false) 
		    => SOME (Block.T
			     {entry = entry,
			      profileInfo = profileInfo,
			      statements = List.fold(start,
						     List.concat [comments, finish],
						     op ::),
			      transfer = transfer})
		    | SOME (0,true)
		    => let
			 val statements
			   = (Assembly.instruction_unal
			      {oper = Instruction.NEG,
			       dst = dst,
			       size = size})::
			     (List.concat [comments, finish])

			 val statements
			   = List.fold(start, 
				       statements,
				       op ::)
		       in
			 SOME (Block.T
			       {entry = entry,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		       end
		    | SOME (1,b)
		    => let
			 val statements
			   = List.fold
			     (start,
			      (fn l
			        => if b
				     then (Assembly.instruction_unal
					   {oper = Instruction.NEG,
					    dst = dst,
					    size = size})::
				          l
				     else l)
			      ((Assembly.instruction_binal
				{oper = Instruction.ADD,
				 src = dst,
				 dst = dst,
				 size = size})::
			       (List.concat [comments, finish])),
			      op ::)
		       in
			 SOME (Block.T
			       {entry = entry,
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
                                    (List.concat [comments, finish])

				val statements
				  = List.fold(start,
					      statements,
					      op ::)
			      in
				SOME (Block.T
				      {entry = entry,
				       profileInfo = profileInfo,
				       statements = statements,
				       transfer = transfer})
			      end
			 else NONE)
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
	val isInstructionCMPorTEST : statement_type -> bool
	  = fn Assembly.Instruction (Instruction.CMP _)
	     => true
	     | Assembly.Instruction (Instruction.TEST _)
	     => true
	     | _ => false

	val isInstructionMOV : statement_type -> bool
	  = fn Assembly.Instruction (Instruction.MOV {...})
	     => true
	     | _ => false

	val isInstructionSETcc : statement_type -> bool
	  = fn Assembly.Instruction (Instruction.SETcc {...})
	     => true
	     | _ => false

	val isInstruction : statement_type -> bool
	  = fn Assembly.Instruction _
	     => true
	     | _ => false

	val isTransfer_Iff : transfer_type -> bool
	  = fn Transfer.Iff {...}
	     => true
	     | _ => false

	val template 
	  = {start = EmptyOrNonEmpty,
	     statements = [One isInstructionCMPorTEST,
			   All isComment],
	     finish = EmptyOrNonEmpty,
	     transfer = fn _ => true}

	val rewriter 
	  = fn {entry,
		profileInfo,
		start, 
		statements as
		[[Assembly.Instruction instruction],
		 comments],
		finish,
		transfer}
	     => let
		  val rec scan 
		    = fn [] => not (isTransfer_Iff transfer)
		       | asm::statements
		       => if isComment asm
		             orelse 
			     isInstructionMOV asm
			    then scan statements
			  else if isInstructionSETcc asm
			    then false
			  else if isInstruction asm
			    then true
			  else false
		in
		  if scan finish
		    then let
			  val statements 
			    = List.fold(start, 
					List.concat [comments, finish],
					op ::)
			 in
			   SOME (Block.T {entry = entry,
					  profileInfo = profileInfo,
					  statements = statements,
					  transfer = transfer})
			 end
		    else NONE
		end
	     | _ => Error.bug "elimCMPTEST"

	val (callback,elimCMPTEST_msg) 
	  = make_callback_msg "elimCMPTEST"
      in
	val elimCMPTEST : optimization
	  = {template = template,
	     rewriter = rewriter,
	     callback = callback}
	val elimCMPTEST_msg = elimCMPTEST_msg
      end

      local
	val isInstructionCMP_srcImmediate0
	  = fn Assembly.Instruction (Instruction.CMP
				     {src1 = Operand.Immediate immediate,
				      ...})
	     => Immediate.zero immediate
	     | Assembly.Instruction (Instruction.CMP
				     {src2 = Operand.Immediate immediate,
				      ...})
	     => Immediate.zero immediate
	     | _ => false

	val isTransfer_Iff_E_NE
	  = fn Transfer.Iff {condition, ...}
	     => condition = Instruction.E
	        orelse
		condition = Instruction.NE
	     | _ => false

	val template 
	  = {start = EmptyOrNonEmpty,
	     statements = [One isInstructionCMP_srcImmediate0,
			   All isComment],
	     finish = Empty,
	     transfer = isTransfer_Iff_E_NE}

	val rewriter 
	  = fn {entry,
		profileInfo,
		start, 
		statements as
		[[Assembly.Instruction
		  (Instruction.CMP {src1, src2, size})],
		 comments],
		finish as [],
		transfer as Transfer.Iff {condition, truee, falsee}}
	     => let
		  val condition
		    = case condition
			of Instruction.E => Instruction.Z
			 | Instruction.NE => Instruction.NZ
			 | _ => Error.bug "Peephole: elimCMP0"

		  val src
		    = case (Operand.deImmediate src1, 
			    Operand.deImmediate src2)
			of (SOME _, NONE) => src2
			 | (NONE, SOME _) => src1
			 | (SOME immediate1, 
			    SOME immediate2)
			 => if Immediate.zero immediate1
			      then src2
			      else src1
			 | _ => Error.bug "Peephole: elimCMP0"

		  val statements 
		    = List.fold(start, 
				(Assembly.instruction_test
				 {src1 = src,
				  src2 = src,
				  size = size})::
				comments,
				op ::)

		  val transfer
		    = Transfer.Iff {condition = condition,
				    truee = truee,
				    falsee = falsee}
		in 
		  SOME (Block.T {entry = entry,
				 profileInfo = profileInfo,
				 statements = statements,
				 transfer = transfer})
		end
	     | _ => Error.bug "elimCMP0"

	val (callback,elimCMP0_msg) 
	  = make_callback_msg "elimCMP0"
      in
	val elimCMP0 : optimization
	  = {template = template,
	     rewriter = rewriter,
	     callback = callback}
	val elimCMP0_msg = elimCMP0_msg
      end

      local
	val isInstructionAL
	  = fn Assembly.Instruction (Instruction.BinAL {...})
	     => true
	     | Assembly.Instruction (Instruction.UnAL {...})
		
	     => true
	     | Assembly.Instruction (Instruction.SRAL {...})
		
	     => true
	     | _ => false

	val isInstructionTEST_eqSrcs
	  = fn Assembly.Instruction (Instruction.TEST {src1, src2, ...})
	     => Operand.eq(src1, src2)
	     | _ => false

	val isTransfer_Iff_Z_NZ
	  = fn Transfer.Iff {condition, ...}
	     => condition = Instruction.Z
	        orelse
		condition = Instruction.NZ
	     | _ => false

	val template 
	  = {start = EmptyOrNonEmpty,
	     statements = [One isInstructionAL,
			   All isComment,
			   One isInstructionTEST_eqSrcs,
			   All isComment],
	     finish = Empty,
	     transfer = isTransfer_Iff_Z_NZ}

	val rewriter 
	  = fn {entry,
		profileInfo,
		start, 
		statements as
		[[Assembly.Instruction instruction],
		 comments1,
		 [Assembly.Instruction
		  (Instruction.TEST {src1, src2, size})],
		 comments2],
		finish as [],
		transfer as Transfer.Iff {condition, truee, falsee}}
	     => let
		  val dst
		    = case instruction
			of Instruction.BinAL {dst, ...} => dst
			 | Instruction.UnAL {dst, ...} => dst
			 | Instruction.SRAL {dst, ...} => dst
			 | _ => Error.bug "elimALTEST"
		in
		  if Operand.eq(dst,src1)
		    then let
			   val statements
			     = List.fold
			       (start,
				(Assembly.instruction instruction)::
				(List.concat [comments1, comments2]),
				op ::)
			 in
			   SOME (Block.T {entry = entry,
					  profileInfo = profileInfo,
					  statements = statements,
					  transfer = transfer})
			 end
		    else NONE
		end
	     | _ => Error.bug "elimALTEST"

	val (callback,elimALTEST_msg) 
	  = make_callback_msg "elimALTEST"
      in
	val elimALTEST : optimization
	  = {template = template,
	     rewriter = rewriter,
	     callback = callback}
	val elimALTEST_msg = elimALTEST_msg
      end

      local
	val optimizations_pre
	  = commuteBinALMD::
(*	    elimBinAL0L:: *)
(*	    elimBinAL0R:: *)
	    elimAddSub1::
	    elimMDPow2::
	    elimCMPTEST::
	    nil
	val optimizations_pre_msg
	  = commuteBinALMD_msg::
(*	    elimBinAL0L_msg:: *)
(*	    elimBinAL0R_msg:: *)
	    elimAddSub1_msg::
	    elimMDPow2_msg::
	    nil

	val optimizations_post
	  = elimBinALMDDouble::
	    elimFltBinADouble::
	    elimCMPTEST::
	    elimCMP0::
	    elimALTEST::
	    nil
	val optimizations_post_msg
	  = elimBinALMDDouble_msg::
	    elimFltBinADouble_msg::
	    elimCMPTEST_msg::
	    elimCMP0_msg::
	    elimALTEST_msg::
	    nil
      in
	val peepholeBlock_pre
	  = fn block => (peepholeBlock {optimizations = optimizations_pre,
				       block = block}
			 handle exn
			  => (print "\n\n***** raising in peepholeBlock_pre\n";
			      Block.printBlock block;
			      raise exn))
	val (peepholeBlock_pre, peepholeBlock_pre_msg)
	  = tracer
	    "peepholeBlock_pre"
	    peepholeBlock_pre

	val peepholeBlock_pre_msg
	  = fn () => (peepholeBlock_pre_msg ();
		      Control.indent ();
		      List.foreach(optimizations_pre_msg, fn msg => msg ());
		      Control.unindent ())

	val peepholeBlock_post
	  = fn block => (peepholeBlock {optimizations = optimizations_post,
				       block = block}
			 handle exn
			 => (print "\n\n***** raising in peepholeBlock_post\n";
			     Block.printBlock block;
			     raise exn))
	val (peepholeBlock_post, peepholeBlock_post_msg)
	  = tracer
	    "peepholeBlock_post"
	    peepholeBlock_post

	val peepholeBlock_post_msg
	  = fn () => (peepholeBlock_post_msg ();
		      Control.indent ();
		      List.foreach(optimizations_post_msg, fn msg => msg ());
		      Control.unindent ())
      end

      val (callback_elimIff,elimIff_msg)
	= make_callback_msg "elimIff"
      fun makeElimIff {jumpInfo : x86JumpInfo.t} :
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
		 statements = [],
		 finish = Empty,
		 transfer = isTransferIff_eqTargets}

	    val rewriter 
	      = fn {entry,
		    profileInfo,
		    start, 
		    statements as [],
		    finish as [],
		    transfer as Transfer.Iff {condition, truee, falsee}}
	         => let
		      val _ = x86JumpInfo.decNear(jumpInfo, falsee)
			
		      val statements 
			= List.fold(start, 
				    [],
				    op ::)
			
		      val transfer = Transfer.goto {target = truee}
		    in 
		      SOME (Block.T {entry = entry,
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
      fun makeElimSwitchTest {jumpInfo : x86JumpInfo.t} :
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
	      = fn {entry,
		    profileInfo,
		    start as [], 
		    statements as [statements'],
		    finish as [],
		    transfer as 
		    Transfer.Switch {test as Operand.Immediate immediate,
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
			    => (x86JumpInfo.decNear(jumpInfo, target);
				(Word.fromInt o Char.ord) c),
			   fn (i,target) 
			    => (x86JumpInfo.decNear(jumpInfo, target);
				Word.fromInt i),
			   fn (w,target) 
			    => (x86JumpInfo.decNear(jumpInfo, target);
				w))
			  
		      val transfer
			= if Transfer.Cases.isEmpty cases
			    then Transfer.goto {target = default}
			  else if Transfer.Cases.isSingle cases
			    then let
				   val _ = x86JumpInfo.decNear
				           (jumpInfo, default)

				   val target
				     = Transfer.Cases.extract
				       (cases, 
					fn target => target)
				   val _ = x86JumpInfo.incNear
				           (jumpInfo, target)
				 in
				   Transfer.goto {target = target}
				 end
			  else Error.bug "elimSwitchTest"
		    in
		      SOME (Block.T {entry = entry,
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
      fun makeElimSwitchCases {jumpInfo : x86JumpInfo.t} :
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
	      = fn {entry,
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
					  then (x86JumpInfo.decNear
						(jumpInfo, target);
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
		      SOME (Block.T {entry = entry,	
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
      fun elimSimpleGoto {chunk as Chunk.T {blocks, ...},
			  jumpInfo : x86JumpInfo.t} 
	= let
	    val gotoInfo as {get: Label.t -> Label.t option,
			     set: Label.t * Label.t option -> unit,
			     destroy}
	      = Property.destGetSet(Label.plist, Property.initConst NONE)
	    val changed = ref false
	      
	    val labels
	      = List.keepAllMap
	        (blocks,
		 fn block as Block.T {entry as Entry.Jump {label}, 
				      profileInfo,
				      statements, 
				      transfer as Transfer.Goto {target}}
		  => if List.forall(statements,
				    fn Assembly.Comment _ => true
				     | _ => false)
		        andalso
			not (Label.equals(label, target))
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
		       x86JumpInfo.decNear(jumpInfo, target); 
		       x86JumpInfo.incNear(jumpInfo, target'); 
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
	         | Transfer.CCall {target, args, dst, live, return}
	         => Transfer.CCall {target = target,
				    args = args,
				    dst = dst,
				    live = live,
				    return = update return}
	         | transfer => transfer

	    val blocks
	      = List.map
	        (blocks,
		 fn Block.T {entry, profileInfo, statements, transfer}
		  => Block.T {entry = entry,
			      profileInfo = profileInfo,
			      statements = statements,
			      transfer = elimSimpleGoto' transfer})

	    val blocks
	      = List.removeAll
	        (blocks,
		 fn Block.T {entry,...}
		  => (case get (Entry.label entry)
			of SOME label' => (changed := true;
					   x86JumpInfo.decNear(jumpInfo, 
							       label');
					   true)
			 | NONE => false))

	    val _ = destroy ()
	  in
	    {chunk = Chunk.T {blocks = blocks},
	     changed = !changed}
	  end

      val (elimSimpleGoto,elimSimpleGoto_msg)
	= tracer
	  "elimSimpleGoto"
	  elimSimpleGoto

      fun elimComplexGoto {chunk as Chunk.T {blocks, ...},
			   jumpInfo : x86JumpInfo.t}
	= let
	    datatype z = datatype x86JumpInfo.status

	    val gotoInfo as {get: Label.t -> Block.t option,
			     set: Label.t * Block.t option -> unit,
			     destroy}
	      = Property.destGetSet(Label.plist, Property.initConst NONE)

	    val labels
	      = List.keepAllMap
	        (blocks,
		 fn block as Block.T {entry as Entry.Jump {label},...}
		  => if x86JumpInfo.getNear(jumpInfo, label) = Count 1
			 then (set(label, SOME block); SOME label)
			 else NONE
		  | _ => NONE)

	    fun loop ()
	      = if List.fold
	           (labels,
		    false,
		    fn (label,b)
		     => case get label
			  of SOME (Block.T 
				   {entry,
				    profileInfo,
				    statements,
				    transfer as Transfer.Goto {target}})
			   => (if Label.equals(label,target)
				 then b
				 else (case get target
					 of NONE => b
					  | SOME (Block.T
						  {entry = entry',
						   profileInfo = profileInfo',
						   statements = statements',
						   transfer = transfer'})
					  => (set(label,
						  SOME (Block.T
							{entry = entry,
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
	      = fn block as Block.T {entry, 
				     profileInfo,
				     statements, 
				     transfer as Transfer.Goto {target}}
		 => if Label.equals(Entry.label entry,target)
		      then block
		      else (case get target
			      of NONE => block
			       | SOME (Block.T {entry = entry',
						profileInfo = profileInfo',
						statements = statements',
						transfer = transfer'})
			       => let
				    val _ = x86JumpInfo.decNear
				            (jumpInfo, 
					     Entry.label entry')
				    val _ = List.foreach
				            (Transfer.nearTargets transfer',
					     fn target 
					      => x86JumpInfo.incNear
					         (jumpInfo, target))

				    val block
				      = Block.T {entry = entry,
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
		       fn (block as Block.T {entry,transfer,...},
			   {blocks, changed'})
		        => if x86JumpInfo.getNear(jumpInfo, Entry.label entry) = Count 0 
			     then let
				    val _ = List.foreach
				            (Transfer.nearTargets transfer,
					     fn target 
					      => x86JumpInfo.decNear
					         (jumpInfo, target))
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

	    val _ = destroy ()
	  in
	    {chunk = Chunk.T {blocks = blocks},
	     changed = changed}
	  end

      val (elimComplexGoto, elimComplexGoto_msg)
	= tracer
	  "elimComplexGoto"
	  elimComplexGoto

      fun elimGoto {chunk : Chunk.t,
		    jumpInfo : x86JumpInfo.t}
	= let
	    val elimIff 
	      = PeepholeBlock.makeElimIff {jumpInfo = jumpInfo}
	    val elimSwitchTest
	      = PeepholeBlock.makeElimSwitchTest {jumpInfo = jumpInfo}
	    val elimSwitchCases 
	      = PeepholeBlock.makeElimSwitchCases {jumpInfo = jumpInfo}

	    fun loop {chunk, changed}
	      = let
		  val {chunk,
		       changed = changed_elimSimpleGoto}
		    = elimSimpleGoto {chunk = chunk,
				      jumpInfo = jumpInfo}

		  val Chunk.T {blocks} = chunk

		  val {blocks,
		       changed = changed_peepholeBlocks}
		    = PeepholeBlock.peepholeBlocks
		      {blocks = blocks,
		       optimizations = [elimIff,
					elimSwitchTest,
					elimSwitchCases]}

		  val chunk = Chunk.T {blocks = blocks}
		in
		  if changed_elimSimpleGoto orelse changed_peepholeBlocks
		    then loop {chunk = chunk, changed = true}
		    else {chunk = chunk, changed = changed}
		end

	    val {chunk, 
		 changed = changed_loop} 
	      = loop {chunk = chunk, changed = false}

	    val {chunk,
		 changed = changed_elimComplexGoto} 
	      = elimComplexGoto {chunk = chunk,
				 jumpInfo = jumpInfo}
	  in
	    {chunk = chunk,
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

  structure MoveHoistLivenessBlock = 
    struct
      structure LiveSet = x86Liveness.LiveSet
      structure LiveInfo = x86Liveness.LiveInfo
      structure Liveness = x86Liveness.Liveness
      structure LivenessBlock = x86Liveness.LivenessBlock

      fun moveHoist {block as LivenessBlock.T 
		              {entry, profileInfo, statements, transfer}}
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
		 fn ((asm: Assembly.t,info as {dead,...}: Liveness.t),
		     {statements: (Assembly.t * Liveness.t) list,
		      changed : bool,
		      moves,
		      live: x86Liveness.LiveSet.t})
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
			  => if LiveSet.contains(dead,
						 memloc_src)
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
			  => if LiveSet.contains(dead,
						 memloc_src)
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
	    val block = LivenessBlock.T {entry = entry,
					 profileInfo = profileInfo,
					 statements = statements,
					 transfer = transfer}
	  in
	    {block = block,
	     changed = changed}
	  end

      val moveHoist
	= fn {block} => (moveHoist {block = block}
			 handle exn
			 => (print "\n\n***** raising in moveHoist\n";
			     LivenessBlock.printBlock block;
			     raise exn))

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
      structure LiveSet = x86Liveness.LiveSet
      structure LiveInfo = x86Liveness.LiveInfo
      structure Liveness = x86Liveness.Liveness
      structure LivenessBlock = x86Liveness.LivenessBlock

      fun copyPropagate' {src,
			  dst as Operand.MemLoc memloc_dst,
			  pblock as {statements, transfer},
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
			     => case Operand.deMemloc operand
				  of SOME memloc
				   => if (use andalso not def)
				         orelse
					 (not (MemLoc.eq(memloc, memloc_dst)))
					then Operand.memloc
					     (MemLoc.replace replacer' memloc)
					else operand
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

	    fun doit (statements : (Assembly.t * Liveness.t) list)
	      = let
		  fun uses_defs {uses, defs}
		    = let
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

			  fun doit'(memlocs,uses)
			    = List.fold
			      (memlocs,
			       uses,
			       fn (memloc,uses)
			        => if List.contains(uses,
						    memloc,
						    MemLoc.eq)
				     then uses
				     else memloc::uses)
			  fun doit''(memlocs,uses)
			    = List.fold
			      (memlocs,
			       uses,
			       fn (memloc,uses)
			        => doit'(MemLoc.utilized memloc, uses))
			in
			  val uses = doit uses
			  val defs = doit defs
			  val uses = doit''(defs,
				     doit''(uses,
					    uses))
			end
		      in
			{uses = uses, defs = defs}
		      end
		in
		  case statements
		    of []
		     => let
			  val transfer = Transfer.replace replacer transfer
			  val {uses,defs,...} = Transfer.uses_defs_kills transfer

			  val {uses, defs} = uses_defs {uses = uses, defs = defs}
			in
			  if not (List.contains(uses,
						memloc_dst,
						MemLoc.eq))
			     andalso
			     not (MemLocSet.contains(Transfer.live transfer,
						     memloc_dst))
			    then if List.forall
			            (all,
				     fn memloc
				      => List.forall
				         (defs,
					  fn memloc'
					   => not (MemLoc.mayAlias(memloc, 
								   memloc'))))
				   then SOME {statements = [],
					      transfer = transfer}
				 else NONE
			    else NONE
			end
		     | (asm,{dead,...})::statements
		     => let
			  val asm = Assembly.replace replacer asm
			  val {uses,defs,...} = Assembly.uses_defs_kills asm

			  val {uses, defs} = uses_defs {uses = uses, defs = defs}
			in
			  if not (List.contains(uses,
						memloc_dst,
						MemLoc.eq))
			    then if LiveSet.contains(dead,memloc_dst)
				   then let
					  val statements 
					    = List.map(statements, 
						       fn (asm,{...}) => asm)
					in 
					  SOME {statements = asm::statements,
						transfer = transfer}
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
					   | SOME {statements,
						   transfer}
				           => SOME {statements = asm::statements,
						    transfer = transfer}
				 else NONE
			    else NONE
			end
		end
	  in
	    case doit statements
	      of NONE => NONE
	       | SOME {statements, transfer}
	       => let
		    val {transfer, live} 
		      = LivenessBlock.toLivenessTransfer 
		        {transfer = transfer,
			 liveInfo = liveInfo}
		    val {statements, live} 
		      = LivenessBlock.toLivenessStatements
		        {statements = statements,
			 live = live}
		  in
		    SOME {pblock = {statements = statements,
				    transfer = transfer},
			  changed = !changed > 0}
		  end
	  end
	| copyPropagate' _ = Error.bug "copyPropagate'"


      fun copyPropagate {block as LivenessBlock.T 
			          {entry, profileInfo, statements, transfer},
			 liveInfo}
	= let
	    val {pblock as {statements,transfer},changed}
	      = List.foldr
	        (statements,
		 {pblock = {statements = [],
			    transfer = transfer},
		  changed = false},
		 fn ((asm as Assembly.Instruction
		             (Instruction.MOV
			      {src,
			       dst as Operand.MemLoc memloc_dst,
			       size}),
		      info),
		     {pblock as {statements, transfer},
		      changed})
		   => let
			val pblock' = {statements = (asm,info)::statements,
				       transfer = transfer}
		      in
			if x86Liveness.track memloc_dst
			   andalso
			   (List.fold
			    (statements,
			     false,
			     fn ((_,{dead,...}),b)
			      => b orelse LiveSet.contains(dead,memloc_dst))
			    orelse
			    LiveSet.contains(#dead(#2(transfer)),memloc_dst))
			  then case copyPropagate' {src = src,
						    dst = dst,
						    pblock = pblock,
						    liveInfo = liveInfo}
				of NONE => {pblock = pblock',
					    changed = changed}
				 | SOME {pblock,
					 changed = changed'}
				  => {pblock = pblock,
				      changed = changed orelse changed'}
			 else {pblock = pblock',
			       changed = changed}
		      end
		   | ((asm as Assembly.Instruction
		             (Instruction.pFMOV
			      {src,
			       dst as Operand.MemLoc memloc_dst,
			       size}),
		      info),
		     {pblock as {statements, transfer},
		      changed})
		   => let
			val pblock' = {statements = (asm,info)::statements,
				       transfer = transfer}
		      in
			if x86Liveness.track memloc_dst
			   andalso
			   (List.fold
			    (statements,
			     false,
			     fn ((_,{dead,...}),b)
			      => b orelse LiveSet.contains(dead,memloc_dst))
			    orelse
			    LiveSet.contains(#dead(#2(transfer)),memloc_dst))
			  then case copyPropagate' {src = src,
						    dst = dst,
						    pblock = pblock,
						    liveInfo = liveInfo}
				of NONE => {pblock = pblock',
					    changed = changed}
				 | SOME {pblock,
					 changed = changed'}
				  => {pblock = pblock,
				      changed = changed orelse changed'}
			 else {pblock = pblock',
			       changed = changed}
		      end
		   | ((asm,info),
		      {pblock as {statements, transfer},
		       changed})
		   => {pblock = {statements = (asm,info)::statements,
				 transfer = transfer},
		       changed = changed})
	  in
	    {block = LivenessBlock.T {entry = entry,
				      profileInfo = profileInfo,
				      statements = statements,
				      transfer = transfer},
	     changed = changed}
	  end

      val copyPropagate
	= fn {block, liveInfo}
	   => (copyPropagate {block = block, liveInfo = liveInfo}
	       handle exn
	        => (print "\n\n***** raising in copyPropagate\n";
		    LivenessBlock.printBlock block;
		    raise exn))

      val (copyPropagate : 
	   {block: LivenessBlock.t, 
	    liveInfo: LiveInfo.t} -> 
	   {block: LivenessBlock.t,
	    changed: bool},
	   copyPropagate_msg)
	= tracer
	  "copyPropagate"
	  copyPropagate
    end

  structure PeepholeLivenessBlock =
    struct
      structure LiveSet = x86Liveness.LiveSet
      structure Liveness = x86Liveness.Liveness
      structure LivenessBlock = x86Liveness.LivenessBlock

      structure Peephole 
	= Peephole(type entry_type = Entry.t * Liveness.t
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
				      => x86Liveness.track memloc
				         andalso
					 LiveSet.contains(dead,memloc)
				      | _ => false)
		end 
	     | _ => false

	val template : template 
	  = {start = EmptyOrNonEmpty,
	     statements = [One isInstruction_dstsTemp_dstsDead],
	     finish = EmptyOrNonEmpty,
	     transfer = fn _ => true}

	val rewriter : rewriter
	  = fn {entry,
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
	     | {entry,
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
	     | {entry,
		profileInfo,
		start, 
		statements as
		[[(Assembly.Instruction instruction,
		   info as {liveOut,...})]],
		finish, 
		transfer}
	     => let
(*
		  val label = let
				val (entry,_) = entry
			      in 
				Entry.label entry
			      end
		  val {dsts, ...} = Instruction.srcs_dsts instruction
		  val _ = print (Label.toString label)
		  val _ = print ": "
		  val _ = Option.app
		          (dsts,
			   fn dsts 
			    => List.foreach
			       (dsts,
				fn operand 
				 => (print (Operand.toString operand);
				     print " ")))
		  val _ = print "\n"
*)

		  val {statements, live}
		    = LivenessBlock.reLivenessStatements
		      {statements = List.rev start,
		       live = liveOut}

		  val {entry, ...}
		    = LivenessBlock.reLivenessEntry
		      {entry = entry,
		       live = live}

		  val statements
		    = List.concat [statements, finish]
		in
		  SOME (LivenessBlock.T
			{entry = entry,
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
	     => x86Liveness.track memloc
	     | _ => false

	val isInstructionAL_dstTemp : statement_type -> bool
	  = fn (Assembly.Instruction (Instruction.BinAL
				      {dst = Operand.MemLoc memloc,...}),
		{...})
	     => x86Liveness.track memloc
	     | (Assembly.Instruction (Instruction.pMD
				      {dst = Operand.MemLoc memloc,...}),
		
		{...})
	     => x86Liveness.track memloc
	     | (Assembly.Instruction (Instruction.IMUL2
				      {dst = Operand.MemLoc memloc,...}),
		
		{...})
	     => x86Liveness.track memloc
	     | (Assembly.Instruction (Instruction.UnAL
				      {dst = Operand.MemLoc memloc,...}),
		
		{...})
	     => x86Liveness.track memloc
	     | (Assembly.Instruction (Instruction.SRAL
				      {dst = Operand.MemLoc memloc,...}),
		
		{...})
	     => x86Liveness.track memloc
	     | _ => false

	val isInstructionMOV_srcTemp_srcDead : statement_type -> bool
	  = fn (Assembly.Instruction (Instruction.MOV 
				      {src = Operand.MemLoc memloc,...}),
		{dead,...})
	     => x86Liveness.track memloc
	        andalso
		LiveSet.contains(dead, memloc)
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
	  = fn {entry,
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
		     | (Assembly.Instruction (Instruction.IMUL2
					      {src, 
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
			       {entry = entry,
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
	  = fn {entry,
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
			{entry = entry,
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
	     | (Assembly.Instruction (Instruction.IMUL2
				      {dst = Operand.MemLoc memloc,...}),
		{...})
	     => true 
	     | _ => false

	val template : template 
	  = {start = EmptyOrNonEmpty,
	     statements = [One isInstructionMOV_dstMemloc,
			   All isComment,
			   One isInstructionBinALMD_dstMemloc_operCommute],
	     finish = EmptyOrNonEmpty,
	     transfer = fn _ => true}

	val rewriter : rewriter
	  = fn {entry,
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
		       => LiveSet.contains(dead2,
					   memloc_src2)
			  andalso
			  not (LiveSet.contains(dead1,
						memloc_src1))
		       | (_, Operand.MemLoc memloc_src2)
		       => LiveSet.contains(dead2,
					   memloc_src2)
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
			       {entry = entry,	
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})		 
		       end
		  else NONE
	     | {entry,
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
		       => LiveSet.contains(dead2,
					   memloc_src2)
			  andalso
			  not (LiveSet.contains(dead1,
						memloc_src1))
		       | (_, Operand.MemLoc memloc_src2)
		       => LiveSet.contains(dead2,
					   memloc_src2)
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
			       {entry = entry,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})		 
		       end
		  else NONE
	     | {entry,
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
		 [(Assembly.Instruction (Instruction.IMUL2
					 {src = src2,
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
		       => LiveSet.contains(dead2,
					   memloc_src2)
			  andalso
			  not (LiveSet.contains(dead1,
						memloc_src1))
		       | (_, Operand.MemLoc memloc_src2)
		       => LiveSet.contains(dead2,
					   memloc_src2)
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
			       [Assembly.instruction_imul2
				{src = src1,
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
			       {entry = entry,
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
	     => x86Liveness.track memloc
	     | _ => false

	val isInstructionFltA_dstTemp : statement_type -> bool
	  = fn (Assembly.Instruction (Instruction.pFBinA
				      {dst = Operand.MemLoc memloc,...}),
		{...})
	     => x86Liveness.track memloc
	     | (Assembly.Instruction (Instruction.pFUnA
				      {dst = Operand.MemLoc memloc,...}),
		
		{...})
	     => x86Liveness.track memloc
	     | (Assembly.Instruction (Instruction.pFPTAN
				      {dst = Operand.MemLoc memloc,...}),
		
		{...})
	     => x86Liveness.track memloc
	     | (Assembly.Instruction (Instruction.pFBinAS
				      {dst = Operand.MemLoc memloc,...}),
		{...})
	     => x86Liveness.track memloc
	     | (Assembly.Instruction (Instruction.pFBinASP
				      {dst = Operand.MemLoc memloc,...}),
		{...})
	     => x86Liveness.track memloc
	     | _ => false

	val isInstructionFMOV_srcTemp_srcDead : statement_type -> bool
	  = fn (Assembly.Instruction (Instruction.pFMOV 
				      {src = Operand.MemLoc memloc,...}),
		{dead,...})
	     => x86Liveness.track memloc
	        andalso
		LiveSet.contains(dead, memloc)
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
	  = fn {entry,
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
			       {entry = entry,
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
	  = fn {entry,
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
			{entry = entry,
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
	  = fn {entry,
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
		       => LiveSet.contains(dead2,
					   memloc_src2)
			  andalso
			  not (LiveSet.contains(dead1,
						memloc_src1))
		       | (_, Operand.MemLoc memloc_src2)
		       => LiveSet.contains(dead2,
					   memloc_src2)
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
			       {entry = entry,	
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
	     => x86Liveness.track memloc
	     | _ => false

	val isInstructionTEST_eqSrcs_srcsTemp_srcsDead : statement_type -> bool
	  = fn (Assembly.Instruction (Instruction.TEST 
				      {src1 = Operand.MemLoc memloc1,
				       src2 = Operand.MemLoc memloc2,...}),
		{dead,...})
	     => MemLoc.eq(memloc1, memloc2) andalso
	        x86Liveness.track memloc1 andalso
		LiveSet.contains(dead, memloc1)
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
	  = fn {entry,
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

			 val {statements, live}
			   = LivenessBlock.toLivenessStatements
			     {statements = statements,
			      live = live}

			 val statements 
			   = List.fold(start, 
				       statements, 
				       op ::)

			 val live
			   = case statements
			       of (_,{liveIn,...})::_ => liveIn
				| _ => Error.bug "Peephole: conditionalJump"

			 val {entry, ...}
			   = LivenessBlock.reLivenessEntry
			     {entry = entry,
			      live = live}
		       in
			 SOME (LivenessBlock.T
			       {entry = entry,
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
	  = elimALCopy::
	    elimFltACopy::
	    elimDeadDsts::
	    elimSelfMove::
	    elimFltSelfMove::
	    commuteBinALMD::
	    commuteFltBinA::
	    conditionalJump::
	    nil
	val optimizations_msg
	  = elimALCopy_msg:: 
	    elimFltACopy_msg::
	    elimDeadDsts_msg::
	    elimSelfMove_msg::
	    elimFltSelfMove_msg::
	    commuteBinALMD_msg::
	    commuteFltBinA_msg::
	    conditionalJump_msg::
	    nil

	val optimizations_minor
	  = elimDeadDsts_minor::
	    elimSelfMove_minor::
	    elimFltSelfMove_minor::
	    nil
	val optimizations_minor_msg
	  = elimDeadDsts_minor_msg::
	    elimSelfMove_minor_msg::
	    elimFltSelfMove_minor_msg::
	    nil
      in
	val peepholeLivenessBlock
	  = fn block => (peepholeBlock {optimizations = optimizations,
					block = block}
	                 handle exn
			  => (print "\n\n***** raising in peepholeLivenessBlock\n";
			      LivenessBlock.printBlock block;
			      raise exn))

	val (peepholeLivenessBlock, peepholeLivenessBlock_msg)
	  = tracer
            "peepholeLivenessBlock"
	    peepholeLivenessBlock

	val peepholeLivenessBlock_msg
	  = fn () => (peepholeLivenessBlock_msg ();
		      Control.indent ();
		      List.foreach(optimizations_msg, fn msg => msg ());
		      Control.unindent ())

	val peepholeLivenessBlock_minor
	  = fn block => (peepholeBlock {optimizations = optimizations_minor,
					block = block}
	                 handle exn
			  => (print "\n\n***** raising in peepholeLivenessBlock_minor\n";
			      LivenessBlock.printBlock block;
			      raise exn))

	val (peepholeLivenessBlock_minor, peepholeLivenessBlock_minor_msg)
	  = tracer
            "peepholeLivenessBlock_minor"
	    peepholeLivenessBlock_minor

	val peepholeLivenessBlock_minor_msg
	  = fn () => (peepholeLivenessBlock_minor_msg ();
		      Control.indent ();
		      List.foreach(optimizations_minor_msg, fn msg => msg ());
		      Control.unindent ())
      end
    end

  fun simplify {chunk as Chunk.T {blocks}: Chunk.t,
		optimize : int,
		liveInfo : x86Liveness.LiveInfo.t,
		jumpInfo : x86JumpInfo.t} :
               Chunk.t
    = let
	val labels
	  = List.map(blocks, fn Block.T {entry,...} => Entry.label entry)

	fun changedChunk_msg 
            {chunk as Chunk.T {blocks, ...}, changed, msg}
	  = ()
	fun changedBlock_msg 
	    {block as Block.T {entry, ...}, changed, msg}
	  = ()
	fun changedLivenessBlock_msg 
	    {block as x86Liveness.LivenessBlock.T {entry, ...}, changed, msg}
	  = ()

(*
	fun changedChunk_msg 
            {chunk as Chunk.T {blocks, ...}, changed, msg}
	  = (print ("finished " ^ msg ^ "\n"))
	fun changedBlock_msg 
	    {block as Block.T {entry, ...}, changed, msg}
	  = (print ("finished " ^ msg ^ "\n"))
	fun changedLivenessBlock_msg 
	    {block as x86Liveness.LivenessBlock.T {entry, ...}, changed, msg}
	  = if changed then (print ("finished " ^ msg ^ "\n")) else ()
*)

(*
	fun changedChunk_msg 
            {chunk as Chunk.T {blocks, ...}, changed, msg}
	  = (print (String.make (60, #"*"));
	     print "\n";
	     print msg;
	     print "\n";
	     List.foreach(blocks, 
			  fn b as Block.T {entry, ...}
			   => (print (concat
				      ["liveIn: ",
				       (concat o List.separate)
				       (List.map
					(x86Liveness.LiveSet.toList
					 (x86Liveness.LiveInfo.getLive
					  (liveInfo, Entry.label entry)),
					 fn memloc => MemLoc.toString memloc),
					"\n        "),
				       "\n"]);
			       x86.Block.printBlock b)))

	fun changedBlock_msg 
	    {block as Block.T {entry, ...}, changed, msg}
	  = (print (String.make (60, #"*"));
	     print "\n";
	     print msg;
	     print "\n";
	     (print (concat
		     ["liveIn: ",
		      (concat o List.separate)
		      (List.map
		       (x86Liveness.LiveSet.toList
			(x86Liveness.LiveInfo.getLive
			 (liveInfo, Entry.label entry)),
			fn memloc => MemLoc.toString memloc),
		       "\n        "),
		      "\n"]);
	      x86.Block.printBlock block))

	fun changedLivenessBlock_msg 
	    {block as x86Liveness.LivenessBlock.T {entry, ...}, changed, msg}
	  = (print (String.make (60, #"*"));
	     print "\n";
	     print msg;
	     print "\n";
	     (print (concat
		     ["liveIn: ",
		      (concat o List.separate)
		      (List.map
		       (x86Liveness.LiveSet.toList
			(x86Liveness.LiveInfo.getLive
			 (liveInfo, Entry.label (#1 entry))),
			fn memloc => MemLoc.toString memloc),
		       "\n        "),
		      "\n"]);
	      x86Liveness.LivenessBlock.printBlock block))
*)

	fun checkLivenessBlock
	    {block, block', msg}
	  = Assert.assert
	    ("verifyLivenessBlock: " ^ msg,
	     fn () => if x86Liveness.LivenessBlock.verifyLivenessBlock
	                 {block = block,
			  liveInfo = liveInfo}
			 handle exn
			  => Error.bug 
			     ("x86Liveness.LivenessBlock.verifyLivenessBlock::" ^
			      (case exn
				 of Fail s => s
				  | _ => "?"))
			then true
			else (print ("pre: " ^ msg);
			      x86Liveness.LivenessBlock.printBlock block;
			      print (String.make(60, #"*"));
			      print ("post: " ^ msg);
			      x86Liveness.LivenessBlock.printBlock block';
			      print (String.make(60, #"*"));
			      false))

	(*********************************************************************)
	(* simplify                                                          *)
	(*********************************************************************)

	val _ = changedChunk_msg 
	        {chunk = chunk,
		 changed = false,
		 msg = "simplify:"}

	(*********************************************************************)
	(* completeLiveInfo                                                  *)
	(*********************************************************************)
	val _ = x86Liveness.LiveInfo.completeLiveInfo 
	        {chunk = chunk,
		 liveInfo = liveInfo,
		 pass = "pre"}
		handle exn
		 => Error.bug 
		    ("x86Liveness.LiveInfo.completeLiveInfo (pre)::" ^
		     (case exn
			of Fail s => s
			 | _ => "?"))

	val _ = changedChunk_msg 
	        {chunk = chunk,
		 changed = false,
		 msg = "completeLiveInfo (pre):"}

	(*********************************************************************)
	(* completeJumpInfo                                                  *)
	(*********************************************************************)
	val _ = x86JumpInfo.completeJumpInfo 
	        {chunk = chunk,
		 jumpInfo = jumpInfo}
		handle exn
		 => Error.bug 
		    ("x86JumpInfo.completeJumpInfo::" ^
		     (case exn
			of Fail s => s
			 | _ => "?"))

	val _
	  = Assert.assert
	    ("verifyEntryTransfer",
	     fn () => x86EntryTransfer.verifyEntryTransfer
	              {chunk = chunk}
		      handle exn
		       => Error.bug 
		          ("x86JumpInfo.verifyEntryTransfer::" ^
			   (case exn
			      of Fail s => s
			       | _ => "?")))

	(*********************************************************************)
	(* optimizer                                                         *)
	(*********************************************************************)
	fun optimizer chunk
	  = let
	       val chunk = chunk
	       val changed = false

	       (**************************************************************)
	       (* elimGoto                                                   *)
	       (**************************************************************)
	       val {chunk = chunk',
		    changed = changed'}
		 = ElimGoto.elimGoto {chunk = chunk,
				      jumpInfo = jumpInfo}
		   handle exn
		    => Error.bug 
		       ("ElimGoto.elimGoto::" ^
			(case exn
			   of Fail s => s
			    | _ => "?"))

	       val _
		 = Assert.assert
		   ("verifyJumpInfo",
		    fn () => x86JumpInfo.verifyJumpInfo 
		             {chunk = chunk',
			      jumpInfo = jumpInfo}
		             handle exn
			      => Error.bug 
			         ("x86JumpInfo.verifyJumpInfo::" ^
				  (case exn
				     of Fail s => s
				      | _ => "?")))

	       val _
		 = Assert.assert
		   ("verifyEntryTransfer",
		    fn () => x86EntryTransfer.verifyEntryTransfer
		             {chunk = chunk'}
			     handle exn
			      => Error.bug 
			         ("x86JumpInfo.verifyEntryTransfer::" ^
				  (case exn
				     of Fail s => s
				      | _ => "?")))

	       val _ = changedChunk_msg 
		       {chunk = chunk,
			changed = changed',
			msg = "ElimGoto.elimGoto:"}
	       val chunk = chunk'
	       val changed = changed orelse changed'		 

	       (**************************************************************)
	       (* peepholeBlock/moveHoist/peepholeLivenessBlock/copyPropagate*)
	       (**************************************************************)
	       val Chunk.T {blocks} = chunk
	       val {blocks = blocks',
		    changed = changed'}
		 = List.fold
		   (blocks,
		    {blocks = [], changed = false},
		    fn (block, {blocks, changed})
		     => let
			  val _ = changedBlock_msg 
			          {block = block,
				   changed = false,
				   msg = "peepholeBlock/moveHoist/peepholeLivenessBlock/copyPropagate"}
			  (***************************************************)
			  (* peepholeBlock_pre                               *)
			  (***************************************************)
			  val {block = block',
			       changed = changed'}
			    = PeepholeBlock.peepholeBlock_pre block
			      handle exn
			       => Error.bug 
			          ("PeepholeBlock.peepholeBlock_pre::" ^
				   (case exn
				      of Fail s => s
				       | _ => "?"))

			  val _ = changedBlock_msg 
			          {block = block',
				   changed = changed',
				   msg = "PeepholeBlock.peepholeBlock_pre"}
			  val block = block'
			  val changed = changed orelse changed'

			  (***************************************************)
			  (* toLivenessBlock                                 *)
			  (***************************************************)
			  val block'
			    = x86Liveness.LivenessBlock.toLivenessBlock 
			      {block = block,
			       liveInfo = liveInfo}
			      handle exn
			       => Error.bug 
			          ("x86Liveness.LivenessBlock.toLivenessBlock::" ^
				   (case exn
				      of Fail s => s
				       | _ => "?"))

			  val block = block'
			  val _ = changedLivenessBlock_msg 
			          {block = block',
				   changed = false,
				   msg = "x86Liveness.LivenessBlock.toLivenessBlock"}

			  (***************************************************)
			  (* moveHoist                                       *)
			  (***************************************************)
			  val {block = block', 
			       changed = changed'}
			    = if !Control.Native.moveHoist
				then MoveHoistLivenessBlock.moveHoist
				     {block = block}
				     handle exn
				      => Error.bug 
				         ("MoveHoistLivenessBlock.moveHoist::" ^
					  (case exn
					     of Fail s => s
					      | _ => "?"))
				else {block = block,
				      changed = false}

			  val _ = checkLivenessBlock 
                                  {block = block,
				   block' = block',
				   msg = "MoveHoistLivenessBlock.moveHoist"}

			  val _ = changedLivenessBlock_msg 
			          {block = block',
				   changed = changed',
				   msg = "MoveHoistLivenessBlock.moveHoist"}
			  val block = block'
			  val changed = changed orelse changed'

			  (***************************************************)
			  (* peepholeLivenessBlock                           *)
			  (***************************************************)
			  val {block = block',
			       changed = changed'}
			    = PeepholeLivenessBlock.peepholeLivenessBlock
			      block
			      handle exn
			       => Error.bug 
			          ("PeepholeLivenessBlock.peepholeLivenessBlock::" ^
				   (case exn
				      of Fail s => s
				       | _ => "?"))

			  val _ = checkLivenessBlock 
                                  {block = block,
				   block' = block',
				   msg = "PeepholeLivenessBlock.peepholeLivenessBlock"}

			  val _ = changedLivenessBlock_msg 
			          {block = block',
				   changed = changed',
				   msg = "PeepholeLivenessBlock.peepholeLivenessBlock"}
			  val block = block'
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
				     handle exn
				      => Error.bug 
				         ("CopyPropagateLivenessBlock.copyPropagate::" ^ 
					  (case exn
					     of Fail s => s
					      | _ => "?"))
				else {block = block,
				      changed = false}

			  val _ = checkLivenessBlock 
                                  {block = block,
				   block' = block',
				   msg = "CopyPropagateLivenessBlock.copyPropagate"}

			  val _ = changedLivenessBlock_msg 
			          {block = block',
				   changed = changed',
				   msg = "CopyPropagateLivenessBlock.copyPropagate"}
			  val block = block'
			  val changed = changed orelse changed'

			  (***************************************************)
			  (* peepholeLivenessBlock_minor                     *)
			  (***************************************************)
			  val {block = block',
			       changed = changed'}
			    = PeepholeLivenessBlock.peepholeLivenessBlock_minor
			      block
			      handle exn
			       => Error.bug 
			          ("PeepholeLivenessBlock.peepholeLivenessBlock_minor::" ^ 
				   (case exn
				      of Fail s => s
				       | _ => "?"))

			  val _ = checkLivenessBlock 
                                  {block = block,
				   block' = block',
				   msg = "PeepholeLivenessBlock.peepholeLivenessBlock_minor"}

			  val _ = changedLivenessBlock_msg 
			          {block = block',
				   changed = changed',
				   msg = "PeepholeLivenessBlock.peepholeLivenessBlock_minor"}
			  val block = block'
			  val changed = changed orelse changed'

			  (***************************************************)
			  (* toBlock                                         *)
			  (***************************************************)
			  val block'
			    = x86Liveness.LivenessBlock.toBlock {block = block}
			      handle exn
			       => Error.bug 
			          ("x86Liveness.LivenessBlock.toBlock::" ^ 
				   (case exn
				      of Fail s => s
				       | _ => "?"))

			  val _ = changedBlock_msg 
			          {block = block',
				   changed = false,
				   msg = "x86Liveness.LivenessBlock.toBlock"}
			  val block = block'

			  (***************************************************)
			  (* peepholeBlock_post                              *)
			  (***************************************************)
			  val {block = block',
			       changed = changed'}
			    = PeepholeBlock.peepholeBlock_post block
			      handle exn
			       => Error.bug 
			          ("PeepholeBlock.peepholeBlock_post::" ^ 
				   (case exn
				      of Fail s => s
				       | _ => "?"))

			  val _ = changedBlock_msg 
			          {block = block',
				   changed = changed',
				   msg = "PeepholeBlock.peepholeBlock_post"}
			  val block = block'
			  val changed = changed orelse changed'
			in
			  {blocks = block::blocks,
			   changed = changed}
			end)
	       val chunk' = Chunk.T {blocks = blocks'}

	       val _ = changedChunk_msg 
		       {chunk = chunk',
			changed = changed',
			msg = "peepholeBlock/moveHoist/peepholeLivenessBlock/copyPropagate"}
	       val chunk = chunk'
	       val changed = changed orelse changed'

	       (**************************************************************)
	       (* completeLiveInfo                                           *)
	       (**************************************************************)
	       val _
		 = x86Liveness.LiveInfo.completeLiveInfo 
		   {chunk = chunk,
		    liveInfo = liveInfo,
		    pass = "post"}
		   handle exn
		    => Error.bug 
		       ("x86Liveness.LiveInfo.completeLiveInfo (post)::" ^ 
			(case exn
			   of Fail s => s
			    | _ => "?"))

	       val _ = changedChunk_msg 
		       {chunk = chunk,
			changed = false,
			msg = "completeLiveInfo (post):"}
	    in
	      {chunk = chunk,
	       changed = changed}
	    end

	(*********************************************************************)
	(* optimizer_loop                                                    *)
	(*********************************************************************)
	fun optimizer_loop chunk
	  = let
	      fun loop {chunk, changed}
		= let
		    val {chunk, changed = changed'}
		      = optimizer chunk
		  in
		    if changed'
		      then loop {chunk = chunk, 
				 changed = true}
		      else {chunk = chunk,
			    changed = changed}
		  end

	      val {chunk, changed} 
		= loop {chunk = chunk, changed = false}
	    in
	      {chunk = chunk,
	       changed = changed}
	    end


	(*********************************************************************)
	(* chunk                                                            *)
	(*********************************************************************)
	val {chunk, changed}
	  = case optimize
	      of 0 => {chunk = chunk, changed = false}
	       | 1 => optimizer chunk
	       | _ => optimizer_loop chunk
      in
	chunk
      end

  val (simplify, simplify_msg)
    = tracerTop
      "simplify"
      simplify

  fun simplify_totals ()
    = (simplify_msg ();
       Control.indent ();
       x86Liveness.LiveInfo.completeLiveInfo_msg ();
       x86JumpInfo.completeJumpInfo_msg ();
       ElimGoto.elimGoto_msg ();
       x86JumpInfo.verifyJumpInfo_msg ();
       x86EntryTransfer.verifyEntryTransfer_msg ();
       PeepholeBlock.peepholeBlock_pre_msg ();
       x86Liveness.LivenessBlock.toLivenessBlock_msg ();
       MoveHoistLivenessBlock.moveHoist_msg ();
       PeepholeLivenessBlock.peepholeLivenessBlock_msg ();
       CopyPropagateLivenessBlock.copyPropagate_msg ();
       PeepholeLivenessBlock.peepholeLivenessBlock_minor_msg ();
       x86Liveness.LivenessBlock.verifyLivenessBlock_msg ();
       x86Liveness.LivenessBlock.toBlock_msg ();
       PeepholeBlock.peepholeBlock_post_msg ();
       Control.unindent ())
end
