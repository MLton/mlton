(* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor amd64Simplify(S: AMD64_SIMPLIFY_STRUCTS): AMD64_SIMPLIFY =
struct

  open S
  open amd64

  val tracer = amd64.tracer
  val tracerTop = amd64.tracerTop

  structure PeepholeBlock =
    struct
      structure Peephole
        = Peephole(type entry_type = Entry.t
                   type profileLabel_type = ProfileLabel.t option
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
          = fn Assembly.Instruction (Instruction.MOV _)
             => true
             | _ => false

        val isInstructionBinALMD : statement_type -> bool
          = fn Assembly.Instruction (Instruction.BinAL _)
             => true
             | Assembly.Instruction (Instruction.pMD _)
             => true
             | Assembly.Instruction (Instruction.IMUL2 _)
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
                profileLabel,
                start, 
                statements =
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
                                profileLabel = profileLabel,
                                statements = statements,
                                transfer = transfer})
                       end
                  else NONE
             | {entry,
                profileLabel,
                start, 
                statements =
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
                                profileLabel = profileLabel,
                                statements = statements,
                                transfer = transfer})
                       end
                  else NONE
             | {entry,
                profileLabel,
                start, 
                statements =
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
                                profileLabel = profileLabel,
                                statements = statements,
                                transfer = transfer})
                       end
                  else NONE
             | _ => Error.bug "amd64Simplify.PeepholeBlock: elimBinALMDDouble"

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
        val isInstructionSSEMOVS : statement_type -> bool
          = fn Assembly.Instruction (Instruction.SSE_MOVS _)
             => true
             | _ => false

        val isInstructionSSEBinAS : statement_type -> bool
          = fn Assembly.Instruction (Instruction.SSE_BinAS _)
             => true
             | _ => false

        val template : template
          = {start = EmptyOrNonEmpty,
             statements = [One isInstructionSSEMOVS,
                           All isComment,
                           One isInstructionSSEBinAS],
             finish = EmptyOrNonEmpty,
             transfer = fn _ => true}

        val rewriter : rewriter
          = fn {entry,
                profileLabel,
                start, 
                statements =
                [[Assembly.Instruction (Instruction.SSE_MOVS
                                        {src = src1,
                                         dst = dst1, 
                                         size = size1})],
                 comments,
                 [Assembly.Instruction (Instruction.SSE_BinAS
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
                           = (Assembly.instruction_sse_movs
                              {src = src1,
                               dst = dst1,
                               size = size1})::
                             (Assembly.instruction_sse_binas
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
                                profileLabel = profileLabel,
                                statements = statements,
                                transfer = transfer})
                       end
                  else NONE
             | _ => Error.bug "amd64Simplify.PeepholeBlock: elimSSEBinASDouble"

        val (callback,elimSSEBinASDouble_msg) 
          = make_callback_msg "elimSSEBinASDouble"
      in
        val elimSSEBinASDouble : optimization
          = {template = template,
             rewriter = rewriter,
             callback = callback}
        val elimSSEBinASDouble_msg = elimSSEBinASDouble_msg
      end

      local
        val isInstructionMOV_srcImmediate : statement_type -> bool
          = fn Assembly.Instruction (Instruction.MOV 
                                     {src = Operand.Immediate _,
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
                profileLabel,
                start, 
                statements =
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
                                      profileLabel = profileLabel,
                                      statements = statements,
                                      transfer = transfer})
                             end
                          | _ => NONE
                  else NONE
             | {entry,
                profileLabel,
                start, 
                statements =
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
                                      profileLabel = profileLabel,
                                      statements = statements,
                                      transfer = transfer})
                             end
                          | _ => NONE
                  else NONE
             | {entry,
                profileLabel,
                start, 
                statements =
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
                                      profileLabel = profileLabel,
                                      statements = statements,
                                      transfer = transfer})
                             end
                          | _ => NONE
                  else NONE
             | _ => Error.bug "amd64Simplify.PeepholeBlock: commuteBinALMD"

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
        val getImmediate1
          = fn Immediate.Word w => if WordX.isOne w
                                      then SOME false
                                   else if WordX.isNegOne w
                                      then SOME true
                                   else NONE
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
                profileLabel,
                start, 
                statements =
                [[Assembly.Instruction (Instruction.BinAL 
                                        {oper,
                                         src = Operand.Immediate immediate, 
                                         dst, 
                                         size})]],
                finish, 
                transfer}
             => if (case List.fold
                         (finish, (false, false), fn (asm, (b, b')) =>
                          case asm
                            of Assembly.Comment _ => (b, b')
                             | Assembly.Instruction
                               (Instruction.BinAL
                                {oper = Instruction.ADC, ...})
                             => (true, if b then b' else true)
                             | Assembly.Instruction
                               (Instruction.BinAL
                                {oper = Instruction.SBB, ...})
                             => (true, if b then b' else true)
                             | Assembly.Instruction
                               (Instruction.SETcc
                                {condition = Instruction.C, ...})
                             => (true, if b then b' else true)
                             | Assembly.Instruction
                               (Instruction.SETcc
                                {condition = Instruction.NC, ...})
                             => (true, if b then b' else true)
                             | _ => (true, b'))
                      of (_, true) => true
                       | (false, _) => (case transfer
                                          of Transfer.Iff
                                             {condition = Instruction.C, ...} => true
                                           | Transfer.Iff
                                             {condition = Instruction.NC, ...} => true
                                           | _ => false)
                       | _ => false)
                  then NONE
                  else let
                         val oper
                           = case (oper, getImmediate1 (Immediate.destruct immediate))
                               of (Instruction.ADD, SOME false) => Instruction.INC
                                | (Instruction.ADD, SOME true ) => Instruction.DEC
                                | (Instruction.SUB, SOME false) => Instruction.DEC
                                | (Instruction.SUB, SOME true ) => Instruction.INC
                                | _ => Error.bug "amd64Simplify.PeeholeBlock: elimAddSub1:oper"

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
                                profileLabel = profileLabel,
                                statements = statements,
                                transfer = transfer})
                       end
             | _ => Error.bug "amd64Simplify.PeeholeBlock: elimAddSub1"

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
          = fn (w : WordX.t, i : int) =>
            if WordX.isZero w then NONE
            else if WordX.isOne (WordX.andb (w, WordX.one (WordX.size w)))
               then if WordX.isOne w
                       then SOME (i, false)
                    else if WordX.isNegOne w
                       then SOME (i, true)
                    else NONE
               else log2' (WordX.rshift (w, WordX.one (WordX.size w), {signed = true}), i + 1)
        fun log2 w = log2' (w, 0 : int)
        fun divTemp size
          = MemLoc.imm {base = Immediate.label (Label.fromString "divTemp"),
                        index = Immediate.zero,
                        scale = Scale.Four,
                        size = size,
                        class = MemLoc.Class.Temp}

        val isImmediatePow2
          = fn Immediate.Word w => isSome (log2 w)
             | _ => false

        val getImmediateLog2
          = fn Immediate.Word w => log2 w 
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
                profileLabel,
                start, 
                statements =
                [[Assembly.Instruction (Instruction.pMD 
                                        {oper = Instruction.IMUL, 
                                         src = Operand.Immediate immediate, 
                                         dst, 
                                         size})],
                 comments],
                finish = [], 
                transfer as Transfer.Iff {condition,
                                          truee,
                                          falsee}}
             => (case getImmediateLog2 (Immediate.destruct immediate)
                   of NONE => Error.bug "amd64Simplify.PeeholeBlock: elimMDPow2:getImmediateLog2"
                    | SOME (0,false)
                    => let
                         val transfer
                           = case condition
                               of Instruction.O 
                                => Transfer.Goto {target = falsee}
                                | Instruction.NO 
                                => Transfer.Goto {target = truee}
                                | _ => Error.bug "amd64Simplify.PeeholeBlock: elimMDPow2:transfer"

                         val statements
                           = List.fold(start,
                                       comments,
                                       op ::)
                       in
                         SOME (Block.T
                               {entry = entry,
                                profileLabel = profileLabel,
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
                                profileLabel = profileLabel,
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
                                profileLabel = profileLabel,
                                statements = statements,
                                transfer = transfer})
                       end
                    | _ => NONE)
             | {entry,
                profileLabel,
                start, 
                statements =
                [[Assembly.Instruction (Instruction.pMD 
                                        {oper = Instruction.IMUL, 
                                         src = Operand.Immediate immediate, 
                                         dst, 
                                         size})],
                 comments],
                finish, 
                transfer}
             => (case getImmediateLog2 (Immediate.destruct immediate)
                   of NONE => Error.bug "amd64Simplify.PeeholeBlock: elimMDPow2:getImmediateLog2"
                    | SOME (0,false) 
                    => SOME (Block.T
                             {entry = entry,
                              profileLabel = profileLabel,
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
                                profileLabel = profileLabel,
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
                                profileLabel = profileLabel,
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
                                           count = Operand.immediate_int i,
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
                                       profileLabel = profileLabel,
                                       statements = statements,
                                       transfer = transfer})
                              end
                         else NONE)
             | {entry,
                profileLabel,
                start, 
                statements =
                [[Assembly.Instruction (Instruction.pMD 
                                         {oper = Instruction.MUL, 
                                          src = Operand.Immediate immediate, 
                                          dst, 
                                          size})],
                 comments],
                finish, 
                transfer}
             => (case getImmediateLog2 (Immediate.destruct immediate)
                   of NONE => Error.bug "amd64Simplify.PeeholeBlock: elimMDPow2:getImmediateLog2"
                    | SOME (0,false) 
                    => SOME (Block.T
                             {entry = entry,
                              profileLabel = profileLabel,
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
                                      count = Operand.immediate_int i,
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
                                       profileLabel = profileLabel,
                                       statements = statements,
                                       transfer = transfer})
                              end
                         else NONE
                    | SOME (_,true)
                    => NONE)
             | {entry,
                profileLabel,
                start, 
                statements =
                [[Assembly.Instruction (Instruction.pMD
                                        {oper = Instruction.IDIV,
                                         src = Operand.Immediate immediate, 
                                         dst, 
                                         size})],
                 comments],
                finish, 
                transfer}
             => (case getImmediateLog2 (Immediate.destruct immediate)
                   of NONE => Error.bug "amd64Simplify.PeeholeBlock: elimMDPow2:getImmediateLog2"
                    | SOME (0,false) 
                    => SOME (Block.T
                             {entry = entry,
                              profileLabel = profileLabel,
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
                                profileLabel = profileLabel,
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
                                       => if i > 1
                                            then (Assembly.instruction_sral
                                                  {oper = Instruction.SAR,
                                                   dst = divTemp,
                                                   count 
                                                   = Operand.immediate_int 
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
                                                   = Operand.immediate_int 
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
                                            count = Operand.immediate_int i,
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
                                       profileLabel = profileLabel,
                                       statements = statements,
                                       transfer = transfer})
                              end
                         else NONE)
             | {entry,
                profileLabel,
                start, 
                statements =
                [[Assembly.Instruction (Instruction.pMD
                                        {oper = Instruction.DIV,
                                         src = Operand.Immediate immediate, 
                                         dst, 
                                         size})],
                 comments],
                finish, 
                transfer}
             => (case getImmediateLog2 (Immediate.destruct immediate)
                   of NONE => Error.bug "amd64Simplify.PeeholeBlock: elimMDPow2:getImmediateLog2"
                    | SOME (0,false) 
                    => SOME (Block.T
                             {entry = entry,
                              profileLabel = profileLabel,
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
                                      count = Operand.immediate_int i,
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
                                       profileLabel = profileLabel,
                                       statements = statements,
                                       transfer = transfer})
                              end
                         else NONE
                    | SOME (_,true) => NONE)
             | {entry,
                profileLabel,
                start, 
                statements =
                [[Assembly.Instruction (Instruction.IMUL2
                                        {src = Operand.Immediate immediate, 
                                         dst, 
                                         size})],
                 comments],
                finish = [], 
                transfer as Transfer.Iff {condition,
                                          truee,
                                          falsee}}
             => (case getImmediateLog2 (Immediate.destruct immediate)
                   of NONE => Error.bug "amd64Simplify.PeeholeBlock: elimMDPow2:getImmediateLog2"
                    | SOME (0,false)
                    => let
                         val transfer
                           = case condition
                               of Instruction.O 
                                => Transfer.Goto {target = falsee}
                                | Instruction.NO 
                                => Transfer.Goto {target = truee}
                                | _ => Error.bug "amd64Simplify.PeeholeBlock: elimMDPow2:transfer"

                         val statements
                           = List.fold(start,
                                       comments,
                                       op ::)
                       in
                         SOME (Block.T
                               {entry = entry,
                                profileLabel = profileLabel,
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
                                profileLabel = profileLabel,
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
                                profileLabel = profileLabel,
                                statements = statements,
                                transfer = transfer})
                       end
                    | _ => NONE)
             | {entry,
                profileLabel,
                start, 
                statements =
                [[Assembly.Instruction (Instruction.IMUL2
                                        {src = Operand.Immediate immediate, 
                                         dst, 
                                         size})],
                 comments],
                finish, 
                transfer}
             => (case getImmediateLog2 (Immediate.destruct immediate)
                   of NONE => Error.bug "amd64Simplify.PeeholeBlock: elimMDPow2:getImmediateLog2"
                    | SOME (0,false) 
                    => SOME (Block.T
                             {entry = entry,
                              profileLabel = profileLabel,
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
                                profileLabel = profileLabel,
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
                                profileLabel = profileLabel,
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
                                           count = Operand.immediate_int i,
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
                                       profileLabel = profileLabel,
                                       statements = statements,
                                       transfer = transfer})
                              end
                         else NONE)
             | _ => Error.bug "amd64Simplify.PeeholeBlock: elimMDPow2"

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
          = fn Assembly.Instruction (Instruction.MOV _)
             => true
             | _ => false

        val isInstructionSETcc : statement_type -> bool
          = fn Assembly.Instruction (Instruction.SETcc _)
             => true
             | _ => false

        val isInstruction : statement_type -> bool
          = fn Assembly.Instruction _
             => true
             | _ => false

        val isTransfer_Iff : transfer_type -> bool
          = fn Transfer.Iff _
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
                profileLabel,
                start, 
                statements =
                [[Assembly.Instruction _],
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
                                          profileLabel = profileLabel,
                                          statements = statements,
                                          transfer = transfer})
                         end
                    else NONE
                end
             | _ => Error.bug "amd64Simplify.PeeholeBlock: elimCMPTEST"

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
             => Immediate.isZero immediate
             | Assembly.Instruction (Instruction.CMP
                                     {src2 = Operand.Immediate immediate,
                                      ...})
             => Immediate.isZero immediate
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
                profileLabel,
                start, 
                statements =
                [[Assembly.Instruction
                  (Instruction.CMP {src1, src2, size})],
                 comments],
                finish = [],
                transfer = Transfer.Iff {condition, truee, falsee}}
             => let
                  val condition
                    = case condition
                        of Instruction.E => Instruction.Z
                         | Instruction.NE => Instruction.NZ
                         | _ => Error.bug "amd64Simplify.PeeholeBlock: elimCMP0:condition"

                  val src
                    = case (Operand.deImmediate src1, 
                            Operand.deImmediate src2)
                        of (SOME _, NONE) => src2
                         | (NONE, SOME _) => src1
                         | (SOME immediate1, SOME _)
                         => if Immediate.isZero immediate1
                              then src2
                              else src1
                         | _ => Error.bug "amd64Simplify.PeeholeBlock: elimCMP0:src"

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
                                 profileLabel = profileLabel,
                                 statements = statements,
                                 transfer = transfer})
                end
             | _ => Error.bug "amd64Simplify.PeeholeBlock: elimCMP0"

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
        val isInstructionAL_setZF
          = fn Assembly.Instruction (Instruction.BinAL _)
             => true
             | Assembly.Instruction (Instruction.UnAL {oper, ...})
             => (case oper
                   of Instruction.NOT => false
                    | _ => true)
             | Assembly.Instruction (Instruction.SRAL {oper, ...})
             => (case oper
                   of Instruction.ROL => false
                    | Instruction.RCL => false
                    | Instruction.ROR => false
                    | Instruction.RCR => false
                    | _ => true)
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
             statements = [One isInstructionAL_setZF,
                           All isComment,
                           One isInstructionTEST_eqSrcs,
                           All isComment],
             finish = Empty,
             transfer = isTransfer_Iff_Z_NZ}

        val rewriter 
          = fn {entry,
                profileLabel,
                start, 
                statements =
                [[Assembly.Instruction instruction],
                 comments1,
                 [Assembly.Instruction
                  (Instruction.TEST {src1, ...})],
                 comments2],
                finish = [],
                transfer as Transfer.Iff {...}}
             => let
                  val dst
                    = case instruction
                        of Instruction.BinAL {dst, ...} => dst
                         | Instruction.UnAL {dst, ...} => dst
                         | Instruction.SRAL {dst, ...} => dst
                         | _ => Error.bug "amd64Simplify.PeeholeBlock: elimALTEST:dst"
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
                                          profileLabel = profileLabel,
                                          statements = statements,
                                          transfer = transfer})
                         end
                    else NONE
                end
             | _ => Error.bug "amd64Simplify.PeeholeBlock: elimALTEST"

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
(*          elimBinAL0L:: *)
(*          elimBinAL0R:: *)
            elimAddSub1::
            elimMDPow2::
            elimCMPTEST::
            nil
        val optimizations_pre_msg
          = commuteBinALMD_msg::
(*          elimBinAL0L_msg:: *)
(*          elimBinAL0R_msg:: *)
            elimAddSub1_msg::
            elimMDPow2_msg::
            nil

        val optimizations_post
          = elimBinALMDDouble::
            elimSSEBinASDouble::
            elimCMPTEST::
            elimCMP0::
            elimALTEST::
            nil
        val optimizations_post_msg
          = elimBinALMDDouble_msg::
            elimSSEBinASDouble_msg::
            elimCMPTEST_msg::
            elimCMP0_msg::
            elimALTEST_msg::
            nil
      in
        val peepholeBlock_pre
          = fn block => (peepholeBlock {optimizations = optimizations_pre,
                                       block = block})
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
                                       block = block})
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
      fun makeElimIff {jumpInfo : amd64JumpInfo.t} :
                      optimization
        = let
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
                    profileLabel,
                    start, 
                    statements = [],
                    finish = [],
                    transfer = Transfer.Iff {truee, falsee, ...}}
                 => let
                      val _ = amd64JumpInfo.decNear(jumpInfo, falsee)

                      val statements 
                        = List.fold(start, 
                                    [],
                                    op ::)

                      val transfer = Transfer.goto {target = truee}
                    in 
                      SOME (Block.T {entry = entry,
                                     profileLabel = profileLabel,
                                     statements = statements,
                                     transfer = transfer})
                    end
                 | _ => Error.bug "amd64Simplify.PeeholeBlock: elimIff"
          in
            {template = template,
             rewriter = rewriter,
             callback = callback_elimIff}
          end

      val (callback_elimSwitchTest,elimSwitchTest_msg)
        = make_callback_msg "elimSwitchTest"
      fun makeElimSwitchTest {jumpInfo : amd64JumpInfo.t} :
                             optimization
        = let
            val isTransferSwitch_testImmediateEval
              = fn Transfer.Switch {test = Operand.Immediate immediate, ...}
                 => isSome (Immediate.eval immediate)
                 | _ => false

            val template 
              = {start = Empty,
                 statements = [All (fn _ => true)],
                 finish = Empty,
                 transfer = isTransferSwitch_testImmediateEval}

            val rewriter 
              = fn {entry,
                    profileLabel,
                    start = [], 
                    statements = [statements'],
                    finish = [],
                    transfer = 
                    Transfer.Switch {test = Operand.Immediate immediate,
                                     cases, 
                                     default}}
                 => let
                      val statements = statements'
                      val test = valOf (Immediate.eval immediate)
                      val cases
                        = Transfer.Cases.keepAll
                          (cases,
                           fn (w,target) 
                            => (amd64JumpInfo.decNear(jumpInfo, target);
                                WordX.equals (w, test)))

                      val transfer
                        = if Transfer.Cases.isEmpty cases
                            then Transfer.goto {target = default}
                          else if Transfer.Cases.isSingle cases
                            then let
                                   val _ = amd64JumpInfo.decNear
                                           (jumpInfo, default)

                                   val target
                                     = Transfer.Cases.extract
                                       (cases, #2)
                                   val _ = amd64JumpInfo.incNear
                                           (jumpInfo, target)
                                 in
                                   Transfer.goto {target = target}
                                 end
                          else Error.bug "amd64Simplify.PeeholeBlock: elimSwitchTest:transfer"
                    in
                      SOME (Block.T {entry = entry,
                                     profileLabel = profileLabel,
                                     statements = statements,
                                     transfer = transfer})
                    end
                 | _ => Error.bug "amd64Simplify.PeeholeBlock: elimSwitchTest"
          in
            {template = template,
             rewriter = rewriter,
             callback = callback_elimSwitchTest}
          end

      val (callback_elimSwitchCases,elimSwitchCases_msg)
        = make_callback_msg "elimSwitchCases"
      fun makeElimSwitchCases {jumpInfo : amd64JumpInfo.t} :
                              optimization
        = let
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
                    profileLabel,
                    start = [], 
                    statements = [statements'],
                    finish = [],
                    transfer = Transfer.Switch {test, cases, default}}
                 => let
                      val statements = statements'
                      val cases
                        = Transfer.Cases.keepAll
                          (cases,
                           fn (_,target) => if Label.equals(target, default)
                                               then (amd64JumpInfo.decNear
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
                                     = Transfer.Cases.extract
                                       (cases,
                                        fn (w,target) =>
                                        (Immediate.word w, target))
                                   val size
                                     = case Operand.size test
                                         of SOME size => size
                                          | NONE => Size.QUAD
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
                                     profileLabel = profileLabel,
                                     statements = statements,
                                     transfer = transfer})
                    end
                 | _ => Error.bug "amd64Simplify.PeeholeBlock: elimSwitchCases"
          in
            {template = template,
             rewriter = rewriter,
             callback = callback_elimSwitchCases}
          end
    end

  structure ElimGoto =
    struct
      fun elimSimpleGoto {chunk = Chunk.T {data, blocks, ...},
                          delProfileLabel : amd64.ProfileLabel.t -> unit,
                          jumpInfo : amd64JumpInfo.t} 
        = let
            val {get: Label.t -> Label.t option,
                 set: Label.t * Label.t option -> unit,
                 destroy}
              = Property.destGetSet(Label.plist, Property.initConst NONE)
            val changed = ref false

            val labels
              = List.keepAllMap
                (blocks,
                 fn Block.T {entry = Entry.Jump {label}, 
                             profileLabel,
                             statements, 
                             transfer = Transfer.Goto {target}}
                  => if List.forall(statements,
                                    fn Assembly.Comment _ => true
                                     | _ => false)
(*
                        andalso
                        not (Label.equals(label, target))
*)
                       then (Option.app(profileLabel, delProfileLabel);
                             set(label, SOME target); 
                             SOME label)
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
                                           => if Label.equals(label, target')
                                                then (set(label, NONE);
                                                      b)
                                                else (set(label, SOME target');
                                                      true)))
                  then loop ()
                  else ()

            val _ = loop ()

            fun update target
              = case get target
                  of SOME target'
                   => (changed := true;
                       amd64JumpInfo.decNear(jumpInfo, target); 
                       amd64JumpInfo.incNear(jumpInfo, target'); 
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
                                             (cases, update o #2),
                                     default = update default}
                 | transfer => transfer

            val blocks
              = List.map
                (blocks,
                 fn Block.T {entry, profileLabel, statements, transfer}
                  => Block.T {entry = entry,
                              profileLabel = profileLabel,
                              statements = statements,
                              transfer = elimSimpleGoto' transfer})

            val blocks
              = List.removeAll
                (blocks,
                 fn Block.T {entry,...}
                  => (case get (Entry.label entry)
                        of SOME label' => (changed := true;
                                           amd64JumpInfo.decNear(jumpInfo, 
                                                               label');
                                           true)
                         | NONE => false))

            val _ = destroy ()
          in
            {chunk = Chunk.T {data = data, blocks = blocks},
             changed = !changed}
          end

      val (elimSimpleGoto,elimSimpleGoto_msg)
        = tracer
          "elimSimpleGoto"
          elimSimpleGoto

      fun elimComplexGoto {chunk = Chunk.T {data, blocks, ...},
                           jumpInfo : amd64JumpInfo.t}
        = let
            datatype z = datatype amd64JumpInfo.status

            val {get: Label.t -> Block.t option,
                 set: Label.t * Block.t option -> unit,
                 destroy}
              = Property.destGetSet(Label.plist, Property.initConst NONE)

            val labels
              = List.keepAllMap
                (blocks,
                 fn block as Block.T {entry = Entry.Jump {label},...}
                  => if amd64JumpInfo.getNear(jumpInfo, label) = Count 1
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
                                    profileLabel,
                                    statements,
                                    transfer = Transfer.Goto {target}})
                           => (if Label.equals(label,target)
                                 then b
                                 else (case get target
                                         of NONE => b
                                          | SOME (Block.T
                                                  {entry = entry',
                                                   profileLabel = profileLabel',
                                                   statements = statements',
                                                   transfer = transfer'})
                                          => (set(label,
                                                  SOME (Block.T
                                                        {entry = entry,
                                                         profileLabel = profileLabel,
                                                         statements
                                                         = List.concat
                                                           [statements,
                                                            [Assembly.Label
                                                             (Entry.label entry')],
                                                            ProfileLabel.toAssemblyOpt
                                                            profileLabel',
                                                            statements'],
                                                         transfer 
                                                         = transfer'}));
                                              true)))
                           | _ => b)
                  then loop ()
                  else ()

            val _ = loop ()

            val changed = ref false
            val elimComplexGoto'
              = fn block as Block.T {entry, 
                                     profileLabel,
                                     statements, 
                                     transfer = Transfer.Goto {target}}
                 => if Label.equals(Entry.label entry,target)
                      then block
                      else (case get target
                              of NONE => block
                               | SOME (Block.T {entry = entry',
                                                profileLabel = profileLabel',
                                                statements = statements',
                                                transfer = transfer'})
                               => let
                                    val _ = changed := true
                                    val _ = amd64JumpInfo.decNear
                                            (jumpInfo, 
                                             Entry.label entry')
                                    val _ = List.foreach
                                            (Transfer.nearTargets transfer',
                                             fn target 
                                              => amd64JumpInfo.incNear
                                                 (jumpInfo, target))

                                    val block
                                      = Block.T {entry = entry,
                                                 profileLabel = profileLabel,
                                                 statements 
                                                 = List.concat
                                                   [statements,
                                                    [Assembly.label
                                                     (Entry.label entry')],
                                                    ProfileLabel.toAssemblyOpt
                                                    profileLabel',
                                                    statements'],
                                                 transfer = transfer'}
                                  in
                                    block
                                  end)
                 | block => block

            val blocks
              = List.map(blocks, elimComplexGoto')

            val _ = destroy ()
          in
            {chunk = Chunk.T {data = data, blocks = blocks},
             changed = !changed}
          end

      val (elimComplexGoto, elimComplexGoto_msg)
        = tracer
          "elimComplexGoto"
          elimComplexGoto

      fun elimBlocks {chunk = Chunk.T {data, blocks, ...},
                      jumpInfo : amd64JumpInfo.t}
        = let
            val {get = getIsBlock,
                 set = setIsBlock,
                 destroy = destroyIsBlock}
              = Property.destGetSetOnce
                (Label.plist, Property.initConst false)

            val {get: Label.t -> {block: Block.t,
                                  reach: bool ref},
                 set, 
                 destroy}
              = Property.destGetSetOnce
                (Label.plist, Property.initRaise ("gotoInfo", Label.layout))

            val (labels, funcs)
              = List.fold
                (blocks, ([], []),
                 fn (block as Block.T {entry, ...}, (labels, funcs))
                  => let
                       val label = Entry.label entry
                     in
                       setIsBlock(label, true);
                       set(label, {block = block,
                                   reach = ref false}) ;
                       case entry
                         of Entry.Func _ => (label::labels, label::funcs)
                          | _ => (label::labels, funcs)
                     end)

            fun loop label
              = let
                  val {block = Block.T {transfer, ...}, reach} = get label
                in
                  if !reach
                    then ()
                    else (reach := true ;
                          List.foreach (Transfer.nearTargets transfer, loop))
                end
            val _ = List.foreach (funcs, loop)

            fun check oper
              = case (Operand.deImmediate oper, Operand.deLabel oper)
                  of (SOME immediate, _) 
                   => (case Immediate.deLabel immediate
                         of SOME label => if getIsBlock label
                                             then ! (#reach (get label))
                                          else true
                          | NONE => true)
                   | (_, SOME label) => if getIsBlock label
                                           then ! (#reach (get label))
                                        else true
                   | _ => true

            val changed = ref false
            val blocks
              = List.keepAllMap
                (labels,
                 fn label 
                  => let
                       val {block = Block.T {entry, 
                                             profileLabel, 
                                             statements, 
                                             transfer}, 
                            reach} = get label
                     in
                       if !reach
                         then SOME 
                              (Block.T 
                               {entry = entry,
                                profileLabel = profileLabel,
                                statements
                                = List.keepAll
                                  (statements,
                                   fn Assembly.Instruction i 
                                    => (case #srcs (Instruction.srcs_dsts i)
                                          of NONE => true
                                           | SOME srcs
                                           => List.forall(srcs, check))
                                    | _ => true),
                                transfer = transfer})
                         else (changed := true ;
                               List.foreach 
                               (Transfer.nearTargets transfer,
                                fn label => amd64JumpInfo.decNear (jumpInfo, label));
                               NONE)
                     end)

            val _ = destroy ()
            val _ = destroyIsBlock ()
          in
            {chunk = Chunk.T {data = data, blocks = blocks},
             changed = !changed}
          end

      val (elimBlocks, elimBlocks_msg)
        = tracer
          "elimBlocks"
          elimBlocks

      fun elimGoto {chunk : Chunk.t,
                    delProfileLabel: amd64.ProfileLabel.t -> unit,
                    jumpInfo : amd64JumpInfo.t}
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
                                      delProfileLabel = delProfileLabel,
                                      jumpInfo = jumpInfo}

                  val Chunk.T {data, blocks, ...} = chunk

                  val {blocks,
                       changed = changed_peepholeBlocks}
                    = PeepholeBlock.peepholeBlocks
                      {blocks = blocks,
                       optimizations = [elimIff,
                                        elimSwitchTest,
                                        elimSwitchCases]}

                  val chunk = Chunk.T {data = data, blocks = blocks}
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

            val {chunk,
                 changed = changed_elimBlocks}
              = elimBlocks {chunk = chunk,
                            jumpInfo = jumpInfo}
          in
            {chunk = chunk,
             changed = changed_loop 
                       orelse changed_elimComplexGoto 
                       orelse changed_elimBlocks}
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
                    elimBlocks_msg ();
                    Control.unindent ())
    end

  structure MoveHoistLivenessBlock = 
    struct
      structure LiveSet = amd64Liveness.LiveSet
      structure Liveness = amd64Liveness.Liveness
      structure LivenessBlock = amd64Liveness.LivenessBlock

      fun moveHoist {block = LivenessBlock.T 
                             {entry, profileLabel, statements, transfer}}
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
                 fn ((asm: Assembly.t, Liveness.T {dead,...}),
                     {statements: (Assembly.t * Liveness.t) list,
                      changed : bool,
                      moves,
                      live: amd64Liveness.LiveSet.t})
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
                                  fn {src,dst,size,age}
                                   => {src = src,
                                       dst = dst,
                                       size = size,
                                       age = age + 1})

                             val statements_forces
                               = List.revMap
                                 (forces,
                                  fn {src,dst,size,...}
                                   => (case Size.class size
                                         of Size.INT
                                          => Assembly.instruction_mov
                                             {src = Operand.memloc src,
                                              dst = Operand.memloc dst,
                                              size = size}
                                          | Size.FLT
                                          => Assembly.instruction_sse_movs
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
                                            fn {age,...}
                                             => age <> 0),
                              moves = moves,
                              live = live}
                           end
                     in
                       case asm
                         of Assembly.Instruction 
                            (Instruction.MOV
                             {src = Operand.MemLoc memloc_src,
                              dst = Operand.MemLoc memloc_dst,
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
                            (Instruction.SSE_MOVS
                             {src = Operand.MemLoc memloc_src,
                              dst = Operand.MemLoc memloc_dst,
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
                 fn {src,dst,size,...}
                  => (case Size.class size
                        of Size.INT
                         => Assembly.instruction_mov
                            {src = Operand.memloc src,
                             dst = Operand.memloc dst,
                             size = size}
                         | Size.FLT
                         => Assembly.instruction_sse_movs
                            {src = Operand.memloc src,
                             dst = Operand.memloc dst,
                             size = size}))
            val {statements = statements_forces,
                 ...}
              = LivenessBlock.toLivenessStatements
                {statements = statements_forces,
                 live = live}
            val statements = List.concat [statements_forces, 
                                          statements]
            val changed = changed
                          orelse
                          List.exists(forces,
                                      fn {age,...}
                                       => age <> 0)
            val block = LivenessBlock.T {entry = entry,
                                         profileLabel = profileLabel,
                                         statements = statements,
                                         transfer = transfer}
          in
            {block = block,
             changed = changed}
          end

      val moveHoist
        = fn {block} => (moveHoist {block = block})

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
      structure LiveSet = amd64Liveness.LiveSet
      structure LiveInfo = amd64Liveness.LiveInfo
      structure Liveness = amd64Liveness.Liveness
      structure LivenessBlock = amd64Liveness.LivenessBlock

      fun copyPropagate' {src,
                          dst as Operand.MemLoc memloc_dst,
                          pblock = {statements, transfer},
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
                     | (asm, Liveness.T {dead, ...}) :: statements
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
                                            = List.map (statements, #1)
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
                    val {statements, ...} 
                      = LivenessBlock.toLivenessStatements
                        {statements = statements,
                         live = live}
                  in
                    SOME {pblock = {statements = statements,
                                    transfer = transfer},
                          changed = !changed > 0}
                  end
          end
        | copyPropagate' _ = Error.bug "amd64Simplify.PeeholeBlock: copyPropagate'"


      fun copyPropagate {block = LivenessBlock.T 
                                 {entry, profileLabel, statements, transfer},
                         liveInfo}
        = let
            val {pblock = {statements,transfer},changed}
              = List.foldr
                (statements,
                 {pblock = {statements = [],
                            transfer = transfer},
                  changed = false},
                 fn ((asm as Assembly.Instruction
                             (Instruction.MOV
                              {src,
                               dst as Operand.MemLoc memloc_dst,
                               ...}),
                      info: Liveness.t),
                     {pblock as {statements, transfer},
                      changed})
                   => let
                        val pblock' = {statements = (asm,info)::statements,
                                       transfer = transfer}
                      in
                        if amd64Liveness.track memloc_dst
                           andalso
                           (List.fold
                            (statements,
                             false,
                             fn ((_, Liveness.T {dead,...}),b)
                              => b orelse LiveSet.contains(dead,memloc_dst))
                            orelse
                            LiveSet.contains(Liveness.dead(#2(transfer)),memloc_dst))
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
                             (Instruction.SSE_MOVS
                              {src,
                               dst as Operand.MemLoc memloc_dst,
                               ...}),
                      info),
                     {pblock as {statements, transfer},
                      changed})
                   => let
                        val pblock' = {statements = (asm,info)::statements,
                                       transfer = transfer}
                      in
                        if amd64Liveness.track memloc_dst
                           andalso
                           (List.fold
                            (statements,
                             false,
                             fn ((_, Liveness.T {dead,...}),b)
                              => b orelse LiveSet.contains(dead,memloc_dst))
                            orelse
                            LiveSet.contains(Liveness.dead (#2 transfer),
                                             memloc_dst))
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
                      {pblock = {statements, transfer},
                       changed})
                   => {pblock = {statements = (asm,info)::statements,
                                 transfer = transfer},
                       changed = changed})
          in
            {block = LivenessBlock.T {entry = entry,
                                      profileLabel = profileLabel,
                                      statements = statements,
                                      transfer = transfer},
             changed = changed}
          end

      val copyPropagate
        = fn {block, liveInfo}
           => (copyPropagate {block = block, liveInfo = liveInfo})

      val (copyPropagate : 
           {block: LivenessBlock.t, 
            liveInfo: LiveInfo.t} -> 
           {block: LivenessBlock.t,
            changed: bool},
           copyPropagate_msg)
        = tracer
          "copyPropagate"
          copyPropagate

      val copyPropagate =
         fn arg as {block as LivenessBlock.T {statements, ...}, ...} =>
         if List.length statements <= !Control.Native.copyPropCutoff
            then copyPropagate arg
         else {block = block, changed = false}
    end

  structure PeepholeLivenessBlock =
    struct
      structure LiveSet = amd64Liveness.LiveSet
      structure Liveness = amd64Liveness.Liveness
      structure LivenessBlock = amd64Liveness.LivenessBlock

      structure Peephole 
        = Peephole(type entry_type = Entry.t * Liveness.t
                   type profileLabel_type = ProfileLabel.t option
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
        = fn (Assembly.Comment _, _) => true
           | _ => false

      local
        val isInstruction_dstsTemp_dstsDead : statement_type -> bool
          = fn (Assembly.Instruction instruction,
                Liveness.T {dead,...})
             => let
                  val {dsts,...} = Instruction.srcs_dsts instruction
                in 
                  case dsts
                    of NONE => false
                     | SOME dsts => List.forall
                                    (dsts,
                                     fn Operand.MemLoc memloc
                                      => amd64Liveness.track memloc
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
                profileLabel,
                start, 
                statements =
                [[(Assembly.Instruction _,
                   Liveness.T {liveOut,...})]],
                finish, 
                transfer}
             => if (case List.fold
                         (finish, (false, false), fn ((asm, _), (b, b')) =>
                          case asm
                            of Assembly.Comment _ => (b, b')
                             | Assembly.Instruction
                               (Instruction.SETcc _) 
                             => (true, if b then b' else true)
                             | _ => (true, b'))
                      of (_, true) => true
                       | (false, _) => (case #1 transfer
                                          of Transfer.Iff _ => true
                                           | _ => false)
                       | _ => false)
                  then NONE
                  else let
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
                                profileLabel = profileLabel,
                                statements = statements,
                                transfer = transfer})
                       end
             | _ => Error.bug "amd64Simplify.PeeholeBlock: elimDeadDsts"

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
                _)
             => amd64Liveness.track memloc
             | _ => false

        val isInstructionAL_dstTemp : statement_type -> bool
          = fn (Assembly.Instruction (Instruction.BinAL
                                      {dst = Operand.MemLoc memloc,...}),
                _)
             => amd64Liveness.track memloc
             | (Assembly.Instruction (Instruction.pMD
                                      {dst = Operand.MemLoc memloc,...}),

                _)
             => amd64Liveness.track memloc
             | (Assembly.Instruction (Instruction.IMUL2
                                      {dst = Operand.MemLoc memloc,...}),

                _)
             => amd64Liveness.track memloc
             | (Assembly.Instruction (Instruction.UnAL
                                      {dst = Operand.MemLoc memloc,...}),

                _)
             => amd64Liveness.track memloc
             | (Assembly.Instruction (Instruction.SRAL
                                      {dst = Operand.MemLoc memloc,...}),

                _)
             => amd64Liveness.track memloc
             | _ => false

        val isInstructionMOV_srcTemp_srcDead : statement_type -> bool
          = fn (Assembly.Instruction (Instruction.MOV 
                                      {src = Operand.MemLoc memloc,...}),
                Liveness.T {dead,...})
             => amd64Liveness.track memloc
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
                profileLabel,
                start, 
                statements =
                [[(Assembly.Instruction (Instruction.MOV 
                                         {src = src1,
                                          dst = dst1 as Operand.MemLoc memloc1,
                                          size = size1}),
                   _)],
                 statements',
                 [(Assembly.Instruction (Instruction.MOV 
                                         {src = Operand.MemLoc memloc2,
                                          dst = dst2,
                                          size = size2}),
                   Liveness.T {liveOut = liveOut2,...})]],
                finish, 
                transfer}
             => if Size.eq(size1,size2) andalso
                   MemLoc.eq(memloc1,memloc2) andalso
                   List.forall
                   (statements',
                    fn (Assembly.Comment _, _) => true
                     | (Assembly.Instruction (Instruction.BinAL
                                              {src, 
                                               dst = Operand.MemLoc memloc, 
                                               size,
                                               ...}),
                        _)
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
                                              {src, 
                                               dst = Operand.MemLoc memloc, 
                                               size,
                                               ...}),
                        _)
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
                                               dst = Operand.MemLoc memloc, 
                                               size}),
                        _)
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
                                              {dst = Operand.MemLoc memloc, 
                                               size,
                                               ...}),
                        _)
                     => Size.eq(size1,size) andalso
                        MemLoc.eq(memloc1,memloc) 
                     | (Assembly.Instruction (Instruction.SRAL
                                              {count,
                                               dst = Operand.MemLoc memloc, 
                                               size, 
                                               ...}),
                        _)
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
                     | _ => Error.bug "amd64Simplify.PeeholeBlock: elimALCopy")
                  then let
                         val statements
                           = List.map
                             (statements',
                              fn (asm,_)
                               => Assembly.replace
                                  (fn {...} 
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
                                profileLabel = profileLabel,
                                statements = statements,
                                transfer = transfer})            
                       end
                  else NONE
             | _ => Error.bug "amd64Simplify.PeeholeBlock: elimALCopy"

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
        val isInstructionSSEMOVS_dstTemp : statement_type -> bool
          = fn (Assembly.Instruction (Instruction.SSE_MOVS
                                      {dst = Operand.MemLoc memloc,...}), 
                _)
             => amd64Liveness.track memloc
             | _ => false

        val isInstructionSSEAS_dstTemp : statement_type -> bool
          = fn (Assembly.Instruction (Instruction.SSE_BinAS
                                      {dst = Operand.MemLoc memloc,...}),
                _)
             => amd64Liveness.track memloc
             | (Assembly.Instruction (Instruction.SSE_UnAS
                                      {dst = Operand.MemLoc memloc,...}),

                _)
             => amd64Liveness.track memloc
             | _ => false

        val isInstructionSSEMOVS_srcTemp_srcDead : statement_type -> bool
          = fn (Assembly.Instruction (Instruction.SSE_MOVS
                                      {src = Operand.MemLoc memloc,...}),
                Liveness.T {dead,...})
             => amd64Liveness.track memloc
                andalso
                LiveSet.contains(dead, memloc)
             | _ => false

        val template : template 
          = {start = EmptyOrNonEmpty,
             statements = [One isInstructionSSEMOVS_dstTemp,
                           All (fn asm 
                                 => (isComment asm) 
                                    orelse
                                    (isInstructionSSEAS_dstTemp asm)),
                           One isInstructionSSEMOVS_srcTemp_srcDead],
             finish = EmptyOrNonEmpty,
             transfer = fn _ => true}

        val rewriter : rewriter
          = fn {entry,
                profileLabel,
                start, 
                statements =
                [[(Assembly.Instruction (Instruction.SSE_MOVS
                                         {src = src1,
                                          dst = dst1 as Operand.MemLoc memloc1,
                                          size = size1}),
                   _)],
                 statements',
                 [(Assembly.Instruction (Instruction.SSE_MOVS
                                         {src = Operand.MemLoc memloc2,
                                          dst = dst2,
                                          size = size2}),
                   Liveness.T {liveOut = liveOut2,...})]],
                finish, 
                transfer}
             => if Size.eq(size1,size2) andalso
                   MemLoc.eq(memloc1,memloc2) andalso
                   List.forall
                   (statements',
                    fn (Assembly.Comment _, _) => true
                     | (Assembly.Instruction (Instruction.SSE_BinAS
                                              {src, 
                                               dst = Operand.MemLoc memloc, 
                                               size,
                                               ...}),
                        _)
                     => Size.eq(size1,size) andalso
                        MemLoc.eq(memloc1,memloc) andalso
                        (case (src,dst2)
                           of (Operand.MemLoc memloc_src,
                               Operand.MemLoc memloc_dst2)
                            => List.forall
                               (memloc_src::(MemLoc.utilized memloc_src),
                                fn memloc' 
                                 => not (MemLoc.mayAlias(memloc_dst2,memloc')))
                            | _ => false)
                     | (Assembly.Instruction (Instruction.SSE_UnAS
                                              {dst = Operand.MemLoc memloc, 
                                               size,
                                               ...}),
                        _)
                     => Size.eq(size1,size) andalso
                        MemLoc.eq(memloc1,memloc) 
                     | _ => Error.bug "amd64Simplify.PeeholeBlock: elimSSEASCopy")
                  then let
                         val statements
                           = List.map
                             (statements',
                              fn (asm,_)
                               => Assembly.replace
                                  (fn {...} 
                                    => fn operand 
                                        => if Operand.eq(operand,dst1)
                                             then dst2
                                             else operand)
                                  asm)

                         val {statements, ...}
                           = LivenessBlock.toLivenessStatements
                             {statements 
                              = (Assembly.instruction_sse_movs
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
                                profileLabel = profileLabel,
                                statements = statements,
                                transfer = transfer})            
                       end
                  else NONE
             | _ => Error.bug "amd64Simplify.PeeholeBlock: elimSSEASCopy"

        val (callback,elimSSEASCopy_msg)
          = make_callback_msg "elimSSEASCopy"
      in
        val elimSSEASCopy : optimization
          = {template = template,
             rewriter = rewriter,
             callback = callback}
        val elimSSEASCopy_msg = elimSSEASCopy_msg
      end

      local
        val isInstructionMOV_eqSrcDst : statement_type -> bool
        = fn (Assembly.Instruction (Instruction.MOV 
                                    {dst = Operand.MemLoc memloc1,
                                     src = Operand.MemLoc memloc2,...}),
              _) 
           => MemLoc.eq(memloc1,memloc2) 
           | _ => false

        val template : template 
          = {start = EmptyOrNonEmpty,
             statements = [One isInstructionMOV_eqSrcDst],
             finish = EmptyOrNonEmpty,
             transfer = fn _ => true}

        val rewriter : rewriter
          = fn {entry,
                profileLabel,
                start, 
                statements =
                [[(Assembly.Instruction (Instruction.MOV 
                                         {src = Operand.MemLoc memloc, ...}),
                   Liveness.T {liveOut,...})]],
                finish, 
                transfer}
             => if List.exists (MemLoc.utilized memloc, amd64Liveness.track)
                   then let
                           val {statements, live} =
                              LivenessBlock.reLivenessStatements
                              {statements = List.rev start,
                               live = liveOut}
                           val {entry, ...} =
                              LivenessBlock.reLivenessEntry
                              {entry = entry,
                               live = live}
                           val statements =
                              List.concat [statements, finish]
                        in 
                           SOME (LivenessBlock.T
                                 {entry = entry,
                                  profileLabel = profileLabel,
                                  statements = statements,
                                  transfer = transfer})
                        end
                   else let
                           val statements =
                              List.fold(start, finish, op ::)
                        in
                           SOME (LivenessBlock.T
                                 {entry = entry,
                                  profileLabel = profileLabel,
                                  statements = statements,
                                  transfer = transfer})
                        end
             | _ => Error.bug "amd64Simplify.PeeholeBlock: elimSelfMove"

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
        val isInstructionSSEMOVS_eqSrcDst : statement_type -> bool
        = fn (Assembly.Instruction (Instruction.SSE_MOVS
                                    {dst = Operand.MemLoc memloc1,
                                     src = Operand.MemLoc memloc2,...}),
              _) 
           => MemLoc.eq(memloc1,memloc2) 
           | _ => false

        val template : template 
          = {start = EmptyOrNonEmpty,
             statements = [One isInstructionSSEMOVS_eqSrcDst],
             finish = EmptyOrNonEmpty,
             transfer = fn _ => true}

        val rewriter : rewriter
          = fn {entry,
                profileLabel,
                start, 
                statements =
                [[(Assembly.Instruction (Instruction.SSE_MOVS
                                         {src = Operand.MemLoc memloc, ...}),
                   Liveness.T {liveOut,...})]],
                finish, 
                transfer}
             => if List.exists (MemLoc.utilized memloc, amd64Liveness.track)
                   then let
                           val {statements, live} =
                              LivenessBlock.reLivenessStatements
                              {statements = List.rev start,
                               live = liveOut}
                           val {entry, ...} =
                              LivenessBlock.reLivenessEntry
                              {entry = entry,
                               live = live}
                           val statements =
                              List.concat [statements, finish]
                        in 
                           SOME (LivenessBlock.T
                                 {entry = entry,
                                  profileLabel = profileLabel,
                                  statements = statements,
                                  transfer = transfer})
                        end
                   else let
                           val statements =
                              List.fold(start, finish, op ::)
                        in
                           SOME (LivenessBlock.T
                                 {entry = entry,
                                  profileLabel = profileLabel,
                                  statements = statements,
                                  transfer = transfer})
                        end
             | _ => Error.bug "amd64Simplify.PeeholeBlock: elimSSESSelfMove"

        val (callback,elimSSESSelfMove_msg)
          = make_callback_msg "elimSSESSelfMove"
      in
        val elimSSESSelfMove : optimization
          = {template = template,
             rewriter = rewriter,
             callback = callback}
        val elimSSESSelfMove_msg = elimSSESSelfMove_msg
      end

      local
        val isInstructionMOV_dstMemloc : statement_type -> bool
          = fn (Assembly.Instruction (Instruction.MOV 
                                      {dst = Operand.MemLoc _,...}),
                _)
             => true
             | _ => false

        val isInstructionBinALMD_dstMemloc_operCommute : statement_type -> bool
          = fn (Assembly.Instruction (Instruction.BinAL
                                      {oper,
                                       dst = Operand.MemLoc _,...}),
                _)
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
                                       dst = Operand.MemLoc _,...}),
                _)
             => (oper = Instruction.IMUL)
                orelse
                (oper = Instruction.MUL)
             | (Assembly.Instruction (Instruction.IMUL2
                                      {dst = Operand.MemLoc _,...}),
                _)
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
                profileLabel,
                start, 
                statements =
                [[(Assembly.Instruction (Instruction.MOV 
                                         {src = src1,
                                          dst 
                                          = dst1 as Operand.MemLoc memloc_dst1,
                                          size = size1}),
                   Liveness.T {dead = dead1,...})],
                 comments,
                 [(Assembly.Instruction (Instruction.BinAL 
                                         {oper = oper2,
                                          src = src2,
                                          dst 
                                          = dst2 as Operand.MemLoc _,
                                          size = size2}),
                   Liveness.T {dead = dead2,
                               liveOut = liveOut2, ...})]],
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
                                profileLabel = profileLabel,
                                statements = statements,
                                transfer = transfer})            
                       end
                  else NONE
             | {entry,
                profileLabel,
                start, 
                statements =
                [[(Assembly.Instruction (Instruction.MOV 
                                         {src = src1,
                                          dst 
                                          = dst1 as Operand.MemLoc memloc_dst1,
                                          size = size1}),
                   Liveness.T {dead = dead1,...})],
                 comments,
                 [(Assembly.Instruction (Instruction.pMD 
                                         {oper = oper2,
                                          src = src2,
                                          dst 
                                          = dst2 as Operand.MemLoc _,
                                          size = size2}),
                   Liveness.T {dead = dead2,
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
                                profileLabel = profileLabel,
                                statements = statements,
                                transfer = transfer})            
                       end
                  else NONE
             | {entry,
                profileLabel,
                start, 
                statements =
                [[(Assembly.Instruction (Instruction.MOV 
                                         {src = src1,
                                          dst 
                                          = dst1 as Operand.MemLoc memloc_dst1,
                                          size = size1}),
                   Liveness.T {dead = dead1,...})],
                 comments,
                 [(Assembly.Instruction (Instruction.IMUL2
                                         {src = src2,
                                          dst 
                                          = dst2 as Operand.MemLoc _,
                                          size = size2}),
                   Liveness.T {dead = dead2,
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
                                profileLabel = profileLabel,
                                statements = statements,
                                transfer = transfer})            
                       end
                  else NONE
             | _ => Error.bug "amd64Simplify.PeeholeBlock: commuteBinALMD"

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
        val isInstructionSSEMOVS_dstMemloc : statement_type -> bool
          = fn (Assembly.Instruction (Instruction.SSE_MOVS
                                      {dst = Operand.MemLoc _,...}),
                _)
             => true
             | _ => false

        val isInstructionSSEBinAS_dstMemloc_operCommute : statement_type -> bool
          = fn (Assembly.Instruction (Instruction.SSE_BinAS
                                      {oper,
                                       dst = Operand.MemLoc _,...}),
                _)
             => (oper = Instruction.SSE_ADDS)
                orelse
                (oper = Instruction.SSE_MULS)
             | _ => false

        val template : template 
          = {start = EmptyOrNonEmpty,
             statements = [One isInstructionSSEMOVS_dstMemloc,
                           All isComment,
                           One isInstructionSSEBinAS_dstMemloc_operCommute],
             finish = EmptyOrNonEmpty,
             transfer = fn _ => true}

        val rewriter : rewriter
          = fn {entry,
                profileLabel,
                start, 
                statements =
                [[(Assembly.Instruction (Instruction.SSE_MOVS
                                         {src = src1,
                                          dst 
                                          = dst1 as Operand.MemLoc memloc_dst1,
                                          size = size1}),
                   Liveness.T {dead = dead1,...})],
                 comments,
                 [(Assembly.Instruction (Instruction.SSE_BinAS
                                         {oper = oper2,
                                          src = src2,
                                          dst 
                                          = dst2 as Operand.MemLoc _,
                                          size = size2}),
                   Liveness.T {dead = dead2,
                               liveOut = liveOut2, ...})]],
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
                           = (Assembly.instruction_sse_movs
                              {src = src2,
                               dst = dst1,
                               size = size1})::
                             (List.concat
                              [List.map(comments, #1),
                               [Assembly.instruction_sse_binas
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
                                profileLabel = profileLabel,
                                statements = statements,
                                transfer = transfer})            
                       end
                  else NONE
             | _ => Error.bug "amd64Simplify.PeeholeBlock: commuteSSEBinAS"

        val (callback,commuteSSEBinAS_msg)
          = make_callback_msg "commuteSSEBinAS"
      in
        val commuteSSEBinAS : optimization
          = {template = template,
             rewriter = rewriter,
             callback = callback}
        val commuteSSEBinAS_msg = commuteSSEBinAS_msg
      end

      local
        val isInstructionSETcc : statement_type -> bool
          = fn (Assembly.Instruction (Instruction.SETcc 
                                      {...}),
                _) 
             => true
             | _ => false

        val isInstructionTEST_eqSrcs : statement_type -> bool
          = fn (Assembly.Instruction (Instruction.TEST 
                                      {src1 = Operand.MemLoc memloc1,
                                       src2 = Operand.MemLoc memloc2,...}),
                Liveness.T {...})
             => MemLoc.eq(memloc1, memloc2) 
             | _ => false

        val isIff_conditionZorNZ : transfer_type -> bool
          = fn (Transfer.Iff {condition,...},
                _)
             => (case condition
                   of Instruction.Z => true
                    | Instruction.NZ => true
                    | _ => false)
             | _ => false

        val template : template
          = {start = EmptyOrNonEmpty,
             statements = [One isInstructionSETcc,
                           All isComment,
                           One isInstructionTEST_eqSrcs,
                           All isComment],
             finish = Empty,
             transfer = isIff_conditionZorNZ}

        val rewriter : rewriter
          = fn {entry,
                profileLabel,
                start, 
                statements =
                [[(statement as 
                   Assembly.Instruction (Instruction.SETcc
                                         {condition = condition1,
                                          dst 
                                          = Operand.MemLoc memloc1,
                                          ...}),
                   _)],
                 comments1,
                 [(Assembly.Instruction (Instruction.TEST
                                         {src1 
                                          = Operand.MemLoc memloc12,
                                          ...}),
                   Liveness.T {dead, ...})],
                 comments2],
                finish = [],
                transfer =
                (Transfer.Iff {condition, truee, falsee},
                 infoT as _)}
             => if MemLoc.eq(memloc1,memloc12)
                  then let
                         val condition 
                           = case condition
                               of Instruction.Z 
                                => Instruction.condition_negate condition1
                                | Instruction.NZ => condition1
                                | _ => Error.bug "amd64Simplify.PeeholeBlock: conditionalJump:condition"

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
                         val statements 
                           = if amd64Liveness.track memloc1 andalso
                                LiveSet.contains(dead, memloc1)
                               then statements
                               else statement::statements

                         val {statements, ...}
                           = LivenessBlock.toLivenessStatements
                             {statements = statements,
                              live = live}

                         val statements 
                           = List.fold(start, 
                                       statements, 
                                       op ::)

                         val live
                           = case statements
                               of (_, Liveness.T {liveIn,...})::_ => liveIn
                                | _ => Error.bug "amd64Simplify.PeeholeBlock: conditionalJump:live"

                         val {entry, ...}
                           = LivenessBlock.reLivenessEntry
                             {entry = entry,
                              live = live}
                       in
                         SOME (LivenessBlock.T
                               {entry = entry,
                                profileLabel = profileLabel,
                                statements = statements,
                                transfer = transfer})
                       end
                  else NONE
             | _ => Error.bug "amd64Simplify.PeeholeBlock: conditionalJump"

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
        val {template, rewriter, ...} = elimSSESSelfMove
        val (callback,elimSSESSelfMove_minor_msg)
          = make_callback_msg "elimSSESSelfMove_minor"
      in
        val elimSSESSelfMove_minor : optimization
          = {template = template,
             rewriter = rewriter,
             callback = callback}
        val elimSSESSelfMove_minor_msg = elimSSESSelfMove_minor_msg
      end

      local
        val optimizations 
          = elimALCopy::
            elimSSEASCopy::
            elimDeadDsts::
            elimSelfMove::
            elimSSESSelfMove::
            commuteBinALMD::
            commuteSSEBinAS::
            conditionalJump::
            nil
        val optimizations_msg
          = elimALCopy_msg:: 
            elimSSEASCopy_msg:: 
            elimDeadDsts_msg::
            elimSelfMove_msg::
            elimSSESSelfMove_msg::
            commuteBinALMD_msg::
            commuteSSEBinAS_msg::
            conditionalJump_msg::
            nil

        val optimizations_minor
          = elimDeadDsts_minor::
            elimSelfMove_minor::
            elimSSESSelfMove_minor::
            nil
        val optimizations_minor_msg
          = elimDeadDsts_minor_msg::
            elimSelfMove_minor_msg::
            elimSSESSelfMove_minor_msg::
            nil
      in
        val peepholeLivenessBlock
          = fn block => (peepholeBlock {optimizations = optimizations,
                                        block = block})

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
                                        block = block})

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

  fun simplify {chunk : Chunk.t,
                optimize : int,
                delProfileLabel : amd64.ProfileLabel.t -> unit,
                liveInfo : amd64Liveness.LiveInfo.t,
                jumpInfo : amd64JumpInfo.t} :
               Chunk.t
    = let
(*
        fun changedChunk_msg 
            {chunk as Chunk.T {blocks, ...}, changed, msg}
          = (print ("finished " ^ msg ^ "\n"))
        fun changedBlock_msg 
            {block as Block.T {entry, ...}, changed, msg}
          = (print ("finished " ^ msg ^ "\n"))
        fun changedLivenessBlock_msg 
            {block as amd64Liveness.LivenessBlock.T {entry, ...}, changed, msg}
          = if changed then (print ("finished " ^ msg ^ "\n")) else ()
*)

        fun changedChunk_msg 
            {chunk = Chunk.T {blocks, ...}, changed, msg}
          = if not changed then () else
            (print (String.make (60, #"*"));
             print "\n";
             print msg;
             print "\n";
             List.foreach(blocks, 
                          fn b as Block.T {entry, ...}
                           => (print (concat
                                      ["liveIn: ",
                                       (concat o List.separate)
                                       (List.map
                                        (amd64Liveness.LiveSet.toList
                                         (amd64Liveness.LiveInfo.getLive
                                          (liveInfo, Entry.label entry)),
                                         fn memloc => MemLoc.toString memloc),
                                        "\n        "),
                                       "\n"]);
                               amd64.Block.printBlock b)))

        fun changedBlock_msg 
            {block as Block.T {entry, ...}, changed, msg}
          = if not changed then () else
            (print (String.make (60, #"*"));
             print "\n";
             print msg;
             print "\n";
             (print (concat
                     ["liveIn: ",
                      (concat o List.separate)
                      (List.map
                       (amd64Liveness.LiveSet.toList
                        (amd64Liveness.LiveInfo.getLive
                         (liveInfo, Entry.label entry)),
                        fn memloc => MemLoc.toString memloc),
                       "\n        "),
                      "\n"]);
              amd64.Block.printBlock block))

        fun changedLivenessBlock_msg 
            {block as amd64Liveness.LivenessBlock.T {entry, ...}, changed, msg}
          = if not changed then () else 
            (print (String.make (60, #"*"));
             print "\n";
             print msg;
             print "\n";
             (print (concat
                     ["liveIn: ",
                      (concat o List.separate)
                      (List.map
                       (amd64Liveness.LiveSet.toList
                        (amd64Liveness.LiveInfo.getLive
                         (liveInfo, Entry.label (#1 entry))),
                        fn memloc => MemLoc.toString memloc),
                       "\n        "),
                      "\n"]);
              amd64Liveness.LivenessBlock.printBlock block))

        val debug = false
        val changedChunk_msg : {chunk : Chunk.t, changed: bool, msg: string} -> unit =
           if debug then changedChunk_msg else (fn _ => ())
        val changedBlock_msg : {block : Block.t, changed: bool, msg: string} -> unit =
           if debug then changedBlock_msg else (fn _ => ())
        val changedLivenessBlock_msg : {block : amd64Liveness.LivenessBlock.t, changed: bool, msg: string} -> unit =
           if debug then changedLivenessBlock_msg else (fn _ => ())

        fun checkLivenessBlock
            {block, block', msg}
          = Assert.assert
            ("amd64Simplify.checkLivenessBlock: " ^ msg,
             fn () => if amd64Liveness.LivenessBlock.verifyLivenessBlock
                         {block = block,
                          liveInfo = liveInfo}
                        then true
                        else (print ("pre: " ^ msg);
                              amd64Liveness.LivenessBlock.printBlock block;
                              print (String.make(60, #"*"));
                              print ("\n");
                              print ("post: " ^ msg);
                              amd64Liveness.LivenessBlock.printBlock block';
                              print (String.make(60, #"*"));
                              print ("\n");
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
        val _ = amd64Liveness.LiveInfo.completeLiveInfo 
                {chunk = chunk,
                 liveInfo = liveInfo,
                 pass = "pre"}

        val _ = changedChunk_msg 
                {chunk = chunk,
                 changed = false,
                 msg = "completeLiveInfo (pre):"}

        (*********************************************************************)
        (* completeJumpInfo                                                  *)
        (*********************************************************************)
        val _ = amd64JumpInfo.completeJumpInfo 
                {chunk = chunk,
                 jumpInfo = jumpInfo}

        val _
          = Assert.assert
            ("amd64Simplify.verifyEntryTransfer",
             fn () => amd64EntryTransfer.verifyEntryTransfer
                      {chunk = chunk})

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
                                      delProfileLabel = delProfileLabel,
                                      jumpInfo = jumpInfo}

               val _
                 = Assert.assert
                   ("amd64Simplify.verifyJumpInfo",
                    fn () => amd64JumpInfo.verifyJumpInfo 
                             {chunk = chunk',
                              jumpInfo = jumpInfo})

               val _
                 = Assert.assert
                   ("amd64Simplify.verifyEntryTransfer",
                    fn () => amd64EntryTransfer.verifyEntryTransfer
                             {chunk = chunk'})

               val _ = changedChunk_msg 
                       {chunk = chunk,
                        changed = changed',
                        msg = "ElimGoto.elimGoto:"}
               val chunk = chunk'
               val changed = changed orelse changed'             

               (**************************************************************)
               (* peepholeBlock/moveHoist/peepholeLivenessBlock/copyPropagate*)
               (**************************************************************)
               val Chunk.T {data, blocks} = chunk
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
                            = amd64Liveness.LivenessBlock.toLivenessBlock 
                              {block = block,
                               liveInfo = liveInfo}

                          val block = block'
                          val _ = changedLivenessBlock_msg 
                                  {block = block',
                                   changed = false,
                                   msg = "amd64Liveness.LivenessBlock.toLivenessBlock"}

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
                            = PeepholeLivenessBlock.peepholeLivenessBlock block

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
                            = PeepholeLivenessBlock.peepholeLivenessBlock_minor block

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
                            = amd64Liveness.LivenessBlock.toBlock {block = block}

                          val _ = changedBlock_msg 
                                  {block = block',
                                   changed = false,
                                   msg = "amd64Liveness.LivenessBlock.toBlock"}
                          val block = block'

                          (***************************************************)
                          (* peepholeBlock_post                              *)
                          (***************************************************)
                          val {block = block',
                               changed = changed'}
                            = PeepholeBlock.peepholeBlock_post block

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
               val chunk' = Chunk.T {data = data, blocks = blocks'}

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
                 = amd64Liveness.LiveInfo.completeLiveInfo 
                   {chunk = chunk,
                    liveInfo = liveInfo,
                    pass = "post"}

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
        val {chunk, ...}
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
       amd64Liveness.LiveInfo.completeLiveInfo_msg ();
       amd64JumpInfo.completeJumpInfo_msg ();
       ElimGoto.elimGoto_msg ();
       amd64JumpInfo.verifyJumpInfo_msg ();
       amd64EntryTransfer.verifyEntryTransfer_msg ();
       PeepholeBlock.peepholeBlock_pre_msg ();
       amd64Liveness.LivenessBlock.toLivenessBlock_msg ();
       MoveHoistLivenessBlock.moveHoist_msg ();
       PeepholeLivenessBlock.peepholeLivenessBlock_msg ();
       CopyPropagateLivenessBlock.copyPropagate_msg ();
       PeepholeLivenessBlock.peepholeLivenessBlock_minor_msg ();
       amd64Liveness.LivenessBlock.verifyLivenessBlock_msg ();
       amd64Liveness.LivenessBlock.toBlock_msg ();
       PeepholeBlock.peepholeBlock_post_msg ();
       Control.unindent ())
end
