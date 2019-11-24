(* Copyright (C) 2019 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor x86MLton (S: X86_MLTON_STRUCTS): X86_MLTON =
struct

  open S
  open x86MLtonBasic
  open x86
  local
     open Machine
  in
     structure CFunction = CFunction
     structure RealSize = RealSize
     structure Prim = Prim
     structure WordSize = WordSize
     datatype z = datatype RealSize.t
     datatype z = datatype WordSize.prim
  end

  type transInfo = {addData : x86.Assembly.t list -> unit,
                    live: x86.Label.t -> x86.Operand.t list,
                    liveInfo: x86Liveness.LiveInfo.t}

  fun implementsPrim (p: 'a Prim.t) =
     let
        datatype z = datatype RealSize.t
        datatype z = datatype WordSize.prim 
        fun w32168 s =
           case WordSize.prim s of
              W8 => true
            | W16 => true
            | W32 => true
            | W64 => false
        datatype z = datatype Prim.Name.t
     in
        case Prim.name p of
           CPointer_add => true
         | CPointer_diff => true
         | CPointer_equal => true
         | CPointer_fromWord => true
         | CPointer_lt => true
         | CPointer_sub => true
         | CPointer_toWord => true
         | Real_Math_acos _ => true
         | Real_Math_asin _ => true
         | Real_Math_atan _ => true
         | Real_Math_atan2 _ => true
         | Real_Math_cos _ => true
         | Real_Math_exp _ => true
         | Real_Math_ln _ => true
         | Real_Math_log10 _ => true
         | Real_Math_sin _ => true
         | Real_Math_sqrt _ => true
         | Real_Math_tan _ => true
         | Real_abs _ => true
         | Real_add _ => true
         | Real_castToWord _ => false (* !! *)
         | Real_div _ => true
         | Real_equal _ => true
         | Real_ldexp _ => true
         | Real_le _ => true
         | Real_lt _ => true
         | Real_mul _ => true
         | Real_neg _ => true
         | Real_qequal _ => true
         | Real_rndToReal _ => true
         | Real_rndToWord (_, s2, {signed}) => signed andalso w32168 s2
         | Real_round _ => true
         | Real_sub _ => true
         | Thread_returnToC => false
         | Word_add _ => true
         | Word_addCheckP _ => true
         | Word_andb _ => true
         | Word_castToReal _ => false (* !! *)
         | Word_equal s => w32168 s
         | Word_extdToWord (s1, s2, _) => w32168 s1 andalso w32168 s2
         | Word_lshift s => w32168 s
         | Word_lt (s, _) => w32168 s
         | Word_mul (s, _) => w32168 s
         | Word_mulCheckP (s, _) => w32168 s
         | Word_neg _ => true
         | Word_negCheckP _ => true
         | Word_notb _ => true
         | Word_orb _ => true
         | Word_quot (s, _) => w32168 s
         | Word_rem (s, _) => w32168 s
         | Word_rndToReal (s1, _, {signed}) => signed andalso w32168 s1
         | Word_rol s => w32168 s
         | Word_ror s => w32168 s
         | Word_rshift (s, _) => w32168 s
         | Word_sub _ => true
         | Word_subCheckP _ => true
         | Word_xorb _ => true
         | _ => false
     end

  val implementsPrim: Machine.Type.t Prim.t -> bool =
     Trace.trace 
     ("x86MLton.implementsPrim", Prim.layout, Bool.layout) 
     implementsPrim

  fun prim {prim : RepType.t Prim.t,
            args : (Operand.t * Size.t) vector,
            dsts : (Operand.t * Size.t) vector,
            transInfo = {...} : transInfo}
    = let
        val primName = Prim.toString prim
        datatype z = datatype Prim.Name.t

        fun getDst1 ()
          = Vector.sub (dsts, 0)
            handle _ => Error.bug "x86MLton.prim: getDst1"
        fun getDst2 ()
          = (Vector.sub (dsts, 0), Vector.sub (dsts, 1))
            handle _ => Error.bug "x86MLton.prim: getDst2"
        fun getSrc1 ()
          = Vector.sub (args, 0)
            handle _ => Error.bug "x86MLton.prim: getSrc1"
        fun getSrc2 ()
          = (Vector.sub (args, 0), Vector.sub (args, 1))
            handle _ => Error.bug "x86MLton.prim: getSrc2"
        fun getSrc4 ()
          = (Vector.sub (args, 0), Vector.sub (args, 1), 
             Vector.sub (args, 2), Vector.sub (args, 3))
            handle _ => Error.bug "x86MLton.prim: getSrc4"

        fun mov ()
          = let
              val (dst,dstsize) = getDst1 ()
              val (src,srcsize) = getSrc1 ()
              val _ 
                = Assert.assert
                  ("x86MLton.prim: mov, dstsize/srcsize",
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
                  ("x86MLton.prim: movx, dstsize/srcsize",
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
                  ("x86MLton.prim: xvom, dstsize/srcsize",
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
                  ("x86MLton.prim: binal, dstsize/src1size/src2size",
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

        fun binalcc (oper, condition)
          = let
              val ((src1,src1size),
                   (src2,src2size)) = getSrc2 ()
              val (dst,dstsize) = getDst1 ()
              val tmp = overflowCheckTempContentsOperand src1size
              val _ 
                = Assert.assert
                  ("x86MLton.prim: binal, src1size/src2size",
                   fn () => src1size = src2size)

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
                   {dst = tmp,
                    src = src1,
                    size = src1size},
                   Assembly.instruction_binal
                   {oper = oper,
                    dst = tmp,
                    src = src2,
                    size = src1size},
                   Assembly.instruction_setcc
                   {condition = condition,
                    dst = dst,
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
                  ("x86MLton.prim: binal64, dst1size/dst2size/src1size/src2size/src3size/src4size",
                   fn () => src1size = dst1size andalso
                            src3size = dst1size andalso
                            src2size = dst2size andalso
                            src4size = dst2size andalso
                            dst1size = dst2size)
              val tdst1 =
                 if List.exists ([src2,src3,src4], fn src =>
                                 Operand.mayAlias (dst1, src))
                    then wordTemp1ContentsOperand dst1size
                    else dst1
              val tdst2 =
                 if List.exists ([src3,src4], fn src =>
                                 Operand.mayAlias (dst2, src))
                    then wordTemp1ContentsOperand dst2size
                    else dst2
            in
              AppendList.fromList
              [Block.mkBlock'
               {entry = NONE,
                statements
                = [Assembly.instruction_mov
                   {dst = tdst1,
                    src = src1,
                    size = src1size},
                   Assembly.instruction_mov
                   {dst = tdst2,
                    src = src2,
                    size = src2size},
                   Assembly.instruction_binal
                   {oper = oper1,
                    dst = tdst1,
                    src = src3,
                    size = dst1size},
                   Assembly.instruction_binal
                   {oper = oper2,
                    dst = tdst2,
                    src = src4,
                    size = dst2size},
                   Assembly.instruction_mov
                   {dst = dst1,
                    src = tdst1,
                    size = dst1size},
                   Assembly.instruction_mov
                   {dst = dst2,
                    src = tdst2,
                    size = dst2size}],
                transfer = NONE}]
            end

        fun binal64cc (oper1, oper2, condition)
          = let
              val ((src1,src1size),
                   (src2,src2size),
                   (src3,src3size),
                   (src4,src4size)) = getSrc4 ()
              val (dst,dstsize) = getDst1 ()
              val tmp1 = overflowCheckTempContentsOperand src1size
              val tmp2 = wordTemp1ContentsOperand src2size
              val _ 
                = Assert.assert
                  ("x86MLton.prim: binal64cc, src1size/src2size/src3size/src4size",
                   fn () => src1size = src3size andalso
                            src2size = src4size)
            in
              AppendList.fromList
              [Block.mkBlock'
               {entry = NONE,
                statements
                = [Assembly.instruction_mov
                   {dst = tmp1,
                    src = src1,
                    size = src1size},
                   Assembly.instruction_mov
                   {dst = tmp2,
                    src = src2,
                    size = src2size},
                   Assembly.instruction_binal
                   {oper = oper1,
                    dst = tmp1,
                    src = src3,
                    size = src1size},
                   Assembly.instruction_binal
                   {oper = oper2,
                    dst = tmp2,
                    src = src4,
                    size = src2size},
                   Assembly.instruction_setcc
                   {condition = condition,
                    dst = dst,
                    size = dstsize}],
                transfer = NONE}]
            end

        fun pmd oper
          = let
              val ((src1,src1size),
                   (src2,src2size)) = getSrc2 ()
              val (dst,dstsize) = getDst1 ()
              val _ 
                = Assert.assert
                  ("x86MLton.prim: pmd, dstsize/src1size/src2size",
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

        fun pmdcc (oper, condition)
          = let
              val ((src1,src1size),
                   (src2,src2size)) = getSrc2 ()
              val (dst,dstsize) = getDst1 ()
              val tmp = overflowCheckTempContentsOperand src1size
              val _ 
                = Assert.assert
                  ("x86MLton.prim: pmdcc, src1size/src2size",
                   fn () => src1size = src2size)

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
                   {dst = tmp,
                    src = src1,
                    size = src1size},
                   Assembly.instruction_pmd
                   {oper = oper,
                    dst = tmp,
                    src = src2,
                    size = src1size},
                   Assembly.instruction_setcc
                   {condition = condition,
                    dst = dst,
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
                  ("x86MLton.prim: imul2, dstsize/src1size/src2size",
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

        fun imul2cc condition
          = let
              val ((src1,src1size),
                   (src2,src2size)) = getSrc2 ()
              val (dst,dstsize) = getDst1 ()
              val tmp = overflowCheckTempContentsOperand src1size
              val _ 
                = Assert.assert
                  ("x86MLton.prim: imul2, src1size/src2size",
                   fn () => src1size = src2size)

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
                   {dst = tmp,
                    src = src1,
                    size = src1size},
                   Assembly.instruction_imul2
                   {dst = tmp,
                    src = src2,
                    size = src1size},
                   Assembly.instruction_setcc
                   {condition = condition,
                    dst = dst,
                    size = dstsize}],
                transfer = NONE}]
            end

        fun neg64cc cond
          = let
              val ((src1,src1size),
                   (src2,src2size)) = getSrc2 ()
              val (dst,dstsize) = getDst1 ()
              val _ 
                = Assert.assert
                  ("x86MLton.prim: neg64cc, src1size/src2size",
                   fn () => src1size = src2size)
              val tmp1 = overflowCheckTempContentsOperand src1size
              val tmp2 = wordTemp1ContentsOperand src2size
            in
              AppendList.fromList
              [Block.mkBlock'
               {entry = NONE,
                statements
                = [Assembly.instruction_mov
                   {dst = tmp1,
                    src = Operand.immediate_zero,
                    size = src1size},
                   Assembly.instruction_mov
                   {dst = tmp2,
                    src = Operand.immediate_zero,
                    size = src2size},
                   Assembly.instruction_binal
                   {oper = Instruction.SUB,
                    dst = tmp1,
                    src = src1,
                    size = src1size},
                   Assembly.instruction_binal
                   {oper = Instruction.SBB,
                    dst = tmp2,
                    src = src2,
                    size = src2size},
                   Assembly.instruction_setcc
                   {condition = cond,
                    dst = dst,
                    size = dstsize}],
                transfer = NONE}]
            end

        fun unal oper
          = let
              val (src,srcsize) = getSrc1 ()
              val (dst,dstsize) = getDst1 ()
              val _ 
                = Assert.assert
                  ("x86MLton.prim: unal, dstsize/srcsize",
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

        fun unalcc (oper, condition)
          = let
              val (src,srcsize) = getSrc1 ()
              val (dst,dstsize) = getDst1 ()
              val tmp = overflowCheckTempContentsOperand srcsize
            in
              AppendList.fromList
              [Block.mkBlock'
               {entry = NONE,
                statements
                = [Assembly.instruction_mov
                   {dst = tmp,
                    src = src,
                    size = srcsize},
                   Assembly.instruction_unal
                   {oper = oper,
                    dst = tmp,
                    size = srcsize},
                   Assembly.instruction_setcc
                   {condition = condition,
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
                  ("x86MLton.prim: unal64, dst1size/dst2size/src1size/src2size",
                   fn () => src1size = dst1size andalso
                            src2size = dst2size andalso
                            dst1size = dst2size)
              val tdst1 =
                 if List.exists ([src2], fn src =>
                                 Operand.mayAlias (dst1, src))
                    then wordTemp1ContentsOperand dst1size
                    else dst1
            in
              AppendList.fromList
              [Block.mkBlock'
               {entry = NONE,
                statements 
                = [Assembly.instruction_mov
                   {dst = tdst1,
                    src = src1,
                    size = src1size},
                   Assembly.instruction_mov
                   {dst = dst2,
                    src = src2,
                    size = src2size},
                   Assembly.instruction_mov
                   {dst = dst1,
                    src = tdst1,
                    size = dst1size},
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
                  ("x86MLton.prim: sral, dstsize/src1size",
                   fn () => src1size = dstsize)
              val _ 
                = Assert.assert
                  ("x86MLton.prim: sral, src2size",
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
                  ("x86MLton.prim: cmp, src1size/src2size",
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
                                  {condition = Instruction.condition_reverse condition,
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
                  ("x86MLton.prim: fbina, dstsize/src1size/src2size",
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

        fun funa oper
          = let
              val (dst,dstsize) = getDst1 ()
              val (src,srcsize) = getSrc1 ()
              val _ 
                = Assert.assert
                  ("x86MLton.prim: funa, dstsize/srcsize",
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
                  ("x86MLton.prim: flogarithm, dstsize/srcsize",
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
          = if !Control.codegenComments > 0
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
        fun bitop (size, i) =
           case WordSize.prim size of
              W8 => binal i
            | W16 => binal i
            | W32 => binal i
            | W64 => binal64 (i, i)
        fun compare (size, {signed}, s, u) =
           let
              val f = if signed then s else u
           in
              case WordSize.prim size of
                 W8 => cmp f
               | W16 => cmp f
               | W32 => cmp f
               | W64 => Error.bug "x86MLton.prim: compare, W64"
           end
        fun shift (size, i) =
           case WordSize.prim size of
              W8 => sral i
            | W16 => sral i
            | W32 => sral i
            | W64 => Error.bug "x86MLton.prim: shift, W64"

        fun flag {signed} =
          if signed then x86.Instruction.O else x86.Instruction.C
      in
        AppendList.appends
        [comment_begin,
         (case Prim.name prim of
               CPointer_add => binal Instruction.ADD
             | CPointer_diff => binal Instruction.SUB
             | CPointer_equal => cmp Instruction.E
             | CPointer_fromWord => mov ()
             | CPointer_lt => cmp Instruction.B
             | CPointer_sub => binal Instruction.SUB
             | CPointer_toWord => mov ()
             | Real_Math_acos _
             => let
                  val (dst,dstsize) = getDst1 ()
                  val (src,srcsize) = getSrc1 ()
                  val _
                    = Assert.assert
                      ("x86MLton.prim: Real_Math_acos, dstsize/srcsize",
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
                      ("x86MLton.prim: Real_Math_asin, dstsize/srcsize",
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
                      ("x86MLton.prim: Real_Math_atan, dstsize/srcsize",
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
                      ("x86MLton.prim: Real_Math_atan2, dstsize/src1size/src2size",
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
                      ("x86MLton.prim: Real_Math_exp, dstsize/srcsize",
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
                      ("x86MLton.prim: Real_Math_tan, dstsize/srcsize",
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
                      ("x86MLton.prim: Real_lt, src1size/src2size",
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
                        src2 = Operand.immediate_int' (0x4500, WordSize.word16),
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
                      ("x86MLton.prim: Real_le, src1size/src2size",
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
                        src2 = Operand.immediate_int' (0x500, WordSize.word16),
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
                      ("x86MLton.prim: Real_equal, src1size/src2size",
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
                        src = Operand.immediate_int' (0x4500, WordSize.word16),
                        size = Size.WORD},
                       Assembly.instruction_cmp
                       {src1 = fpswTempContentsOperand,
                        src2 = Operand.immediate_int' (0x4000, WordSize.word16),
                        size = Size.WORD},
                       Assembly.instruction_setcc
                       {condition = Instruction.E,
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
                      ("x86MLton.prim: Real_qequal, src1size/src2size",
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
                        src2 = Operand.immediate_int' (0x4400, WordSize.word16),
                        size = Size.WORD},
                       Assembly.instruction_setcc
                       {condition = Instruction.NE,
                        dst = dst,
                        size = dstsize}],
                    transfer = NONE}]
                end
             | Real_abs _ => funa Instruction.FABS
             | Real_rndToReal (s, s')
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
             | Real_rndToWord (s, s', _)
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
                           {dst = tmp,
                            src = src,
                            srcsize = srcsize,
                            dstsize = tmpsize},
                           Assembly.instruction_xvom
                           {src = tmp,
                            dst = dst,
                            dstsize = dstsize,
                            srcsize = tmpsize}],
                        transfer = NONE}]
                    end 
                in
                   case (s, WordSize.prim s') of
                      (R64, W64) => Error.bug "x86MLton.prim: Real_toWord, W64"
                    | (R64, W32) => default ()
                    | (R64, W16) => default ()
                    | (R64, W8) => default' ()
                    | (R32, W64) => Error.bug "x86MLton.prim: Real_toWord, W64"
                    | (R32, W32) => default ()
                    | (R32, W16) => default ()
                    | (R32, W8) => default' ()
                end
             | Real_ldexp _ 
             => let
                  val (dst,dstsize) = getDst1 ()
                  val ((src1,src1size),
                       (src2,src2size)) = getSrc2 ()
                  val _
                    = Assert.assert
                      ("x86MLton.prim: Real_ldexp, dstsize/src1size",
                       fn () => src1size = dstsize)
                  val _
                    = Assert.assert
                      ("x86MLton.prim: Real_ldexp, src2size",
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
             | Word_addCheckP (s, sg) =>
                 let
                   val cond = flag sg
                 in
                   case WordSize.prim s of
                      W8 => binalcc (Instruction.ADD, cond)
                    | W16 => binalcc (Instruction.ADD, cond)
                    | W32 => binalcc (Instruction.ADD, cond)
                    | W64 => binal64cc (Instruction.ADD, Instruction.ADC, cond)
                 end
             | Word_andb s => bitop (s, Instruction.AND)
             | Word_equal _ => cmp Instruction.E
             | Word_lshift s => shift (s, Instruction.SHL)
             | Word_lt (s, sg) => compare (s, sg, Instruction.L, Instruction.B)
             | Word_mul (s, {signed}) =>
                (case WordSize.prim s of
                    W8 => pmd (if signed
                                  then Instruction.IMUL
                               else Instruction.MUL)
                  | W16 => imul2 ()
                  | W32 => imul2 ()
                  | W64 => Error.bug "x86MLton.prim: Word_mul, W64")
             | Word_mulCheckP (s, {signed}) =>
                 if signed
                    then
                       (case WordSize.prim s of
                           W8 => pmdcc (x86.Instruction.IMUL, x86.Instruction.O)
                         | W16 => imul2cc x86.Instruction.O
                         | W32 => imul2cc x86.Instruction.O
                         | W64 => Error.bug "x86MLton.arith: Word_mulCheckP, W64")
                 else
                    (case WordSize.prim s of
                        W8 => pmdcc (x86.Instruction.MUL, x86.Instruction.C)
                      | W16 => pmdcc (x86.Instruction.MUL, x86.Instruction.C)
                      | W32 => pmdcc (x86.Instruction.MUL, x86.Instruction.C)
                      | W64 => Error.bug "x86MLton.arith: Word_mulCheckP, W64")
             | Word_neg s => 
                (case WordSize.prim s of
                    W8 => unal Instruction.NEG
                  | W16 => unal Instruction.NEG
                  | W32 => unal Instruction.NEG
                  | W64 => unal64 (Instruction.NEG, 
                                   fn (dst,dstsize) => [Assembly.instruction_binal
                                                        {dst = dst,
                                                         oper = Instruction.ADC,
                                                         src = Operand.immediate_zero,
                                                         size = dstsize}]))
             | Word_negCheckP (s, sg) =>
                 let
                    val cond = flag sg
                 in
                    case WordSize.prim s of
                       W8 => unalcc (Instruction.NEG, cond)
                     | W16 => unalcc (Instruction.NEG, cond)
                     | W32 => unalcc (Instruction.NEG, cond)
                     | W64 => neg64cc cond
                 end
             | Word_notb s => 
                (case WordSize.prim s of
                    W8 => unal Instruction.NOT
                  | W16 => unal Instruction.NOT
                  | W32 => unal Instruction.NOT
                  | W64 => unal64 (Instruction.NOT, fn _ => []))
             | Word_orb s => bitop (s, Instruction.OR)
             | Word_quot (_, {signed}) =>
                  pmd (if signed then Instruction.IDIV else Instruction.DIV)
             | Word_rem (_, {signed}) =>
                  pmd (if signed then Instruction.IMOD else Instruction.MOD)
             | Word_rol s => shift (s, Instruction.ROL)
             | Word_ror s => shift (s, Instruction.ROR)
             | Word_rshift (s, {signed}) =>
                  shift (s, if signed then Instruction.SAR else Instruction.SHR)
             | Word_sub s => 
                (case WordSize.prim s of
                    W8 => binal Instruction.SUB
                  | W16 => binal Instruction.SUB
                  | W32 => binal Instruction.SUB
                  | W64 => binal64 (Instruction.SUB, Instruction.SBB))
             | Word_subCheckP (s, sg) =>
                 let
                   val cond = flag sg
                 in
                   case WordSize.prim s of
                      W8 => binalcc (Instruction.SUB, cond)
                    | W16 => binalcc (Instruction.SUB, cond)
                    | W32 => binalcc (Instruction.SUB, cond)
                    | W64 => binal64cc (Instruction.SUB, Instruction.SBB, cond)
                 end
             | Word_rndToReal (s, s', _)
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
                   case (WordSize.prim s, s') of
                      (W32, R64) => default ()
                    | (W32, R32) => default ()
                    | (W16, R64) => default ()
                    | (W16, R32) => default ()
                    | (W8, R64) => default' ()
                    | (W8, R32) => default' ()
                    | _ => Error.bug "x86MLton.prim: Word_toReal, W64"
                end
             | Word_extdToWord (s, s', {signed}) =>
                  let
                     val b = WordSize.bits s
                     val b' = WordSize.bits s'
                  in
                     if Bits.< (b, b')
                        then movx (if signed
                                      then Instruction.MOVSX
                                   else Instruction.MOVZX)
                     else if Bits.equals (b, b')
                             then mov ()
                          else xvom ()
                  end
             | Word_xorb s => bitop (s, Instruction.XOR)
             | _ => Error.bug ("x86MLton.prim: strange Prim.Name.t: " ^ primName)),
         comment_end]
      end

  fun ccall {args: (x86.Operand.t * x86.Size.t) vector,
             func,
             return: {return: x86.Label.t, size: int option} option,
             transInfo = {...}: transInfo}
    = let
        val CFunction.T {convention, target, ...} = func
        val comment_begin
          = if !Control.codegenComments > 0
              then AppendList.single 
                   (x86.Block.mkBlock'
                    {entry = NONE,
                     statements = 
                     [x86.Assembly.comment
                      (concat 
                       ["begin ccall: ",
                        CFunction.Convention.toString convention,
                        " ",
                        CFunction.Target.toString target])],
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
                             func = func,
                             return = return})})]
      end

  fun creturn {dsts: (x86.Operand.t * x86.Size.t) vector,
               frameInfo: x86.FrameInfo.t option,
               func: RepType.t CFunction.t,
               label: x86.Label.t, 
               transInfo = {live, liveInfo, ...}: transInfo}
    = let
        val CFunction.T {convention, target, ...} = func
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
          = if !Control.codegenComments > 0
              then AppendList.single 
                   (x86.Block.mkBlock'
                    {entry = NONE,
                     statements = 
                     [x86.Assembly.comment
                      (concat 
                       ["begin creturn: ",
                        CFunction.Convention.toString convention,
                        " ",
                        CFunction.Target.toString target])],
                     transfer = NONE})
              else AppendList.empty
      in
        AppendList.appends [default (), comment_end]
      end

end
