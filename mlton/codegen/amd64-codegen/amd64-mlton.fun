(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor amd64MLton (S: AMD64_MLTON_STRUCTS): AMD64_MLTON =
struct

  open S
  open amd64MLtonBasic
  open amd64
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

  type transInfo = {addData : amd64.Assembly.t list -> unit,
                    frameInfoToAMD64: (amd64MLtonBasic.Machine.FrameInfo.t
                                     -> amd64.FrameInfo.t),
                    live: amd64.Label.t -> amd64.Operand.t list,
                    liveInfo: amd64Liveness.LiveInfo.t}

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
         | FFI_Symbol _ => true
         | Real_Math_acos _ => false
         | Real_Math_asin _ => false
         | Real_Math_atan _ => false
         | Real_Math_atan2 _ => false
         | Real_Math_cos _ => false
         | Real_Math_exp _ => false
         | Real_Math_ln _ => false
         | Real_Math_log10 _ => false
         | Real_Math_sin _ => false
         | Real_Math_sqrt _ => true
         | Real_Math_tan _ => false
         | Real_abs _ => true
         | Real_add _ => true
         | Real_castToWord _ => true
         | Real_div _ => true
         | Real_equal _ => true
         | Real_ldexp _ => false
         | Real_le _ => true
         | Real_lt _ => true
         | Real_mul _ => true
         | Real_muladd _ => true
         | Real_mulsub _ => true
         | Real_neg _ => true
         | Real_qequal _ => true
         | Real_rndToReal _ => true
         | Real_rndToWord (_, s2, {signed}) => signed orelse w32168 s2
         | Real_round _ => false
         | Real_sub _ => true
         | Thread_returnToC => false
         | Word_add _ => true
         | Word_addCheck _ => true
         | Word_andb _ => true
         | Word_castToReal _ => true
         | Word_equal _ => true
         | Word_extdToWord _ => true
         | Word_lshift _ => true
         | Word_lt _ => true
         | Word_mul _ => true
         | Word_mulCheck _ => true
         | Word_neg _ => true
         | Word_negCheck _ => true
         | Word_notb _ => true
         | Word_orb _ => true
         | Word_quot _ => true
         | Word_rem _ => true
         | Word_rndToReal (s1, _, {signed}) => signed orelse w32168 s1
         | Word_rol _ => true
         | Word_ror _ => true
         | Word_rshift _ => true
         | Word_sub _ => true
         | Word_subCheck _ => true
         | Word_xorb _ => true
         | _ => false
     end

  val implementsPrim: Machine.Type.t Prim.t -> bool =
     Trace.trace 
     ("amd64MLton.implementsPrim", Prim.layout, Bool.layout) 
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
            handle _ => Error.bug "amd64MLton.prim: getDst1"
        fun getSrc1 ()
          = Vector.sub (args, 0)
            handle _ => Error.bug "amd64MLton.prim: getSrc1"
        fun getSrc2 ()
          = (Vector.sub (args, 0), Vector.sub (args, 1))
            handle _ => Error.bug "amd64MLton.prim: getSrc2"
        fun getSrc3 ()
          = (Vector.sub (args, 0), Vector.sub (args, 1), Vector.sub (args, 2))
            handle _ => Error.bug "amd64MLton.prim: getSrc3"

        fun mov ()
          = let
              val (dst,dstsize) = getDst1 ()
              val (src,srcsize) = getSrc1 ()
              val _ 
                = Assert.assert
                  ("amd64MLton.prim: mov, dstsize/srcsize",
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
                  ("amd64MLton.prim: movx, dstsize/srcsize",
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
                  ("amd64MLton.prim: xvom, dstsize/srcsize",
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
                  ("amd64MLton.prim: binal, dstsize/src1size/src2size",
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
                            => if amd64Liveness.track memloc_src1
                                  andalso
                                  amd64Liveness.track memloc_src2
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

        fun pmd oper
          = let
              val ((src1,src1size),
                   (src2,src2size)) = getSrc2 ()
              val (dst,dstsize) = getDst1 ()
              val _ 
                = Assert.assert
                  ("amd64MLton.prim: pmd, dstsize/src1size/src2size",
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
                            => if amd64Liveness.track memloc_src1
                                  andalso
                                  amd64Liveness.track memloc_src2
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
                  ("amd64MLton.prim: imul2, dstsize/src1size/src2size",
                   fn () => src1size = dstsize andalso
                            src2size = dstsize)

              (* Reverse src1/src2 when src1 and src2 are temporaries
               * and the oper is commutative. 
               *)
              val (src1,src2)
                = case (Operand.deMemloc src1, Operand.deMemloc src2)
                    of (SOME memloc_src1, SOME memloc_src2)
                     => if amd64Liveness.track memloc_src1
                           andalso
                           amd64Liveness.track memloc_src2
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
                  ("amd64MLton.prim: unal, dstsize/srcsize",
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

        fun sral oper
          = let
              val (dst,dstsize) = getDst1 ()
              val ((src1,src1size),
                   (src2,src2size)) = getSrc2 ()
              val _ 
                = Assert.assert
                  ("amd64MLton.prim: sral, dstsize/src1size",
                   fn () => src1size = dstsize)
              val _ 
                = Assert.assert
                  ("amd64MLton.prim: sral, src2size",
                   fn () => src2size = Size.LONG)
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
                  ("amd64MLton.prim: cmp, src1size/src2size",
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

        fun compare ({signed}, s, u) =
           let
              val f = if signed then s else u
           in
              cmp f
           end


        fun sse_movs ()
          = let
              val (dst,dstsize) = getDst1 ()
              val (src,srcsize) = getSrc1 ()
              val _ 
                = Assert.assert
                  ("amd64MLton.prim: sse_movs, dstsize/srcsize",
                   fn () => srcsize = dstsize)
            in
              AppendList.fromList
              [Block.mkBlock'
               {entry = NONE,
                statements
                = [Assembly.instruction_sse_movs
                   {dst = dst,
                    src = src,
                    size = srcsize}],
                transfer = NONE}]
            end

        (*
        fun sse_ucomis condition
          = let
              val (dst,dstsize) = getDst1 ()
              val ((src1,src1size),
                   (src2,src2size)) = getSrc2 ()
              val _ 
                = Assert.assert
                  ("amd64MLton.prim: cmp, src1size/src2size",
                   fn () => src1size = src2size)
            in
              AppendList.fromList
              [Block.mkBlock'
               {entry = NONE,      
                statements
                = [Assembly.instruction_sse_ucomis
                   {src1 = src1,
                    src2 = src2,
                    size = src1size},
                   Assembly.instruction_setcc
                   {condition = condition,
                    dst = dst,
                    size = dstsize}],
                transfer = NONE}]
            end
        *)

        fun sse_cvtsfp2sfp ()
          = let
              val (dst,dstsize) = getDst1 ()
              val (src,srcsize) = getSrc1 ()
              val _ 
                = Assert.assert
                  ("amd64MLton.prim: sse_cvtsfp2sfp, dstsize/srcsize",
                   fn () => srcsize <> dstsize)
            in
              AppendList.fromList
              [Block.mkBlock'
               {entry = NONE,
                statements
                = [Assembly.instruction_sse_cvtsfp2sfp
                   {dst = dst,
                    dstsize = dstsize,
                    src = src,
                    srcsize = srcsize}],
                transfer = NONE}]
            end

        fun sse_movd ()
          = let
              val (dst,dstsize) = getDst1 ()
              val (src,srcsize) = getSrc1 ()
              val _ 
                = Assert.assert
                  ("amd64MLton.prim: sse_movd, dstsize/srcsize",
                   fn () => srcsize <> dstsize)
            in
              AppendList.fromList
              [Block.mkBlock'
               {entry = NONE,
                statements
                = [Assembly.instruction_sse_movd
                   {dst = dst,
                    dstsize = dstsize,
                    src = src,
                    srcsize = srcsize}],
                transfer = NONE}]
            end

        fun sse_binas oper
          = let
              val ((src1,src1size),
                   (src2,src2size)) = getSrc2 ()
              val (dst,dstsize) = getDst1 ()
              val _ 
                = Assert.assert
                  ("amd64MLton.prim: binal, dstsize/src1size/src2size",
                   fn () => src1size = dstsize andalso
                            src2size = dstsize)

              (* Reverse src1/src2 when src1 and src2 are temporaries
               * and the oper is commutative. 
               *)
              val (src1,src2)
                = if (oper = Instruction.SSE_ADDS)
                     orelse
                     (oper = Instruction.SSE_MULS)
                     orelse
                     (oper = Instruction.SSE_MAXS)
                     orelse
                     (oper = Instruction.SSE_MINS)
                    then case (Operand.deMemloc src1, Operand.deMemloc src2)
                           of (SOME memloc_src1, SOME memloc_src2)
                            => if amd64Liveness.track memloc_src1
                                  andalso
                                  amd64Liveness.track memloc_src2
                                 then (src2,src1)
                                 else (src1,src2)
                            | _ => (src1,src2)
                    else (src1,src2)
            in
              AppendList.fromList
              [Block.mkBlock'
               {entry = NONE,
                statements
                = [Assembly.instruction_sse_movs
                   {dst = dst,
                    src = src1,
                    size = src1size},
                   Assembly.instruction_sse_binas
                   {oper = oper,
                    dst = dst,
                    src = src2,
                    size = dstsize}],
                transfer = NONE}]
            end

        fun sse_binas_mul oper
          = let
              val ((src1,src1size),
                   (src2,src2size),
                   (src3,src3size)) = getSrc3 ()
              val (dst,dstsize) = getDst1 ()
              val _ 
                = Assert.assert
                  ("amd64MLton.prim: binal, dstsize/src1size/src2size",
                   fn () => src1size = dstsize andalso
                            src2size = dstsize andalso
                            src3size = dstsize)
            in
              AppendList.fromList
              [Block.mkBlock'
               {entry = NONE,
                statements
                = [Assembly.instruction_sse_movs
                   {dst = dst,
                    src = src1,
                    size = src1size},
                   Assembly.instruction_sse_binas
                   {oper = Instruction.SSE_MULS,
                    dst = dst,
                    src = src2,
                    size = dstsize},
                   Assembly.instruction_sse_binas
                   {oper = oper,
                    dst = dst,
                    src = src3,
                    size = dstsize}],
                transfer = NONE}]
            end

        fun sse_unas oper
          = let
              val (src,srcsize) = getSrc1 ()
              val (dst,dstsize) = getDst1 ()
              val _ 
                = Assert.assert
                  ("amd64MLton.prim: unal, dstsize/srcsize",
                   fn () => srcsize = dstsize)
            in
              AppendList.fromList
              [Block.mkBlock'
               {entry = NONE,
                statements 
                = [Assembly.instruction_sse_unas
                   {oper = oper,
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
                      (amd64.Block.mkBlock'
                       {entry = NONE,
                        statements 
                        = [amd64.Assembly.comment 
                           ("begin prim: " ^ comment)],
                        transfer = NONE}),
                      AppendList.single
                      (amd64.Block.mkBlock'
                       {entry = NONE,
                        statements 
                        = [amd64.Assembly.comment 
                           ("end prim: " ^ comment)],
                        transfer = NONE}))
                   end
              else (AppendList.empty,AppendList.empty)
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
             | FFI_Symbol {name, symbolScope, ...}
             => let
                   datatype z = datatype CFunction.SymbolScope.t
                   datatype z = datatype Control.Format.t
                   datatype z = datatype MLton.Platform.OS.t

                   val (dst, dstsize) = getDst1 ()
                   val label = fn () => Label.fromString name
                   
                   (* how to access an imported label's address *)
                   (* windows coff will add another leading _ to label *)
                   val coff = fn () => Label.fromString ("_imp__" ^ name)
                   val macho = fn () => Label.fromString (name ^ "@GOTPCREL")
                   val elf = fn () => Label.fromString (name ^ "@GOTPCREL")
                   
                   val importLabel = fn () =>
                      case !Control.Target.os of
                         Cygwin => coff ()
                       | Darwin => macho ()
                       | MinGW => coff ()
                       | _ => elf ()
                   
                   val direct = fn () =>
                      AppendList.fromList
                      [Block.mkBlock'
                       {entry = NONE,
                        statements =
                        [Assembly.instruction_lea
                         {dst = dst,
                          src = Operand.memloc_label (label ()),
                          size = dstsize}],
                        transfer = NONE}]
                   
                   val indirect = fn () =>
                      AppendList.fromList
                      [Block.mkBlock'
                       {entry = NONE,
                        statements =
                        [Assembly.instruction_mov
                         {dst = dst,
                          src = Operand.memloc_label (importLabel ()),
                          size = dstsize}],
                        transfer = NONE}]
                in
                   case (symbolScope, 
                         !Control.Target.os, 
                         !Control.positionIndependent) of
                    (* As long as the symbol is private (this means it is not
                     * exported to code outside this text segment), then 
                     * RIP-relative addressing works on every OS/format. 
                     *)
                      (Private, _, _) => direct ()
                    (* When linking an executable, ELF and darwin-x86_64 use 
                     * a special trick to "simplify" the code. All exported
                     * functions and symbols have pointers that correspond to
                     * to the executable. Function pointers point to the 
                     * automatically created PLT entry in the executable.
                     * Variables are copied/relocated into the executable bss.
                     * This means that direct access is fine for executable
                     * and archive formats. (It also means direct access is
                     * NOT fine for a library, even if it defines the symbol)
                     * 
                     * On ELF&darwin, a public symbol must be accessed via
                     * the GOT. This is because the final value may not be
                     * in this text segment. If the executable uses it, then
                     * the unique C address resides in the executable's
                     * text segment. The loader does this by creating a PLT
                     * proxy or copying values to the executable text segment.
                     *)
                    | (Public, _, true) => indirect ()
                    | (Public, _, false) => direct ()
                    (* On windows, the address is the point of definition. So
                     * we must use an indirect lookup even in executables.
                     *)
                    | (External, MinGW, _) => indirect ()
                    | (External, Cygwin, _) => indirect ()
                    (* When compiling to a library, we need to access external
                     * symbols via some address that is updated by the loader.
                     * That address resides within our data segment, and can
                     * be easily referenced using RIP-relative addressing.
                     * This trick is used on every platform MLton supports.
                     * Windows rewrites __imp__name symbols in our segment.
                     * ELF and darwin-x86_64 rewrite name@GOTPCREL.
                     *)
                    | (External, _, true) => indirect ()
                    | (External, _, false) => direct ()
                end
             | Real_Math_sqrt _ => sse_unas Instruction.SSE_SQRTS
             | Real_abs s =>
                let
                   val (dst,dstsize) = getDst1 ()
                   val (src,srcsize) = getSrc1 ()
                   val _ 
                     = Assert.assert
                       ("amd64MLton.prim: Real_abs, dstsize/srcsize",
                        fn () => srcsize = dstsize)
                   fun mkConst wordSize
                     = WordX.rshift
                       (WordX.allOnes wordSize,
                        WordX.one wordSize,
                        {signed = false})

                   val (const,constsize) 
                     = case s of
                         R32 => (mkConst WordSize.word32, Size.LONG)
                       | R64 => (mkConst WordSize.word64, Size.QUAD)
                in
                   AppendList.fromList
                   [Block.mkBlock'
                    {entry = NONE,
                     statements 
                     = [Assembly.instruction_sse_movd
                        {dst = dst,
                         dstsize = dstsize,
                         src = Operand.immediate_word const,
                         srcsize = constsize},
                        Assembly.instruction_sse_binlp
                        {oper = Instruction.SSE_ANDP,
                         src = src,
                         dst = dst,
                         size = dstsize}],
                     transfer = NONE}]

                end
             | Real_add _ => sse_binas Instruction.SSE_ADDS
             | Real_castToWord _ => sse_movd ()
             | Real_div _ => sse_binas Instruction.SSE_DIVS
             | Real_equal _ =>
                let
                   val (dst,dstsize) = getDst1 ()
                   val ((src1,src1size),
                        (src2,src2size)) = getSrc2 ()
                   val tmp =
                      fpeqTempContentsOperand dstsize

                   val _ 
                      = Assert.assert
                      ("amd64MLton.prim: Real_equal, src1size/src2size",
                       fn () => src1size = src2size)
                in
                   AppendList.fromList
                   [Block.mkBlock'
                    {entry = NONE,      
                     statements
                     = [Assembly.instruction_sse_ucomis
                        {src1 = src1,
                         src2 = src2,
                         size = src1size},
                        Assembly.instruction_setcc
                        {condition = Instruction.NP,
                         dst = tmp,
                         size = dstsize},
                        Assembly.instruction_setcc
                        {condition = Instruction.Z,
                         dst = dst,
                         size = dstsize},
                        Assembly.instruction_binal
                        {oper = Instruction.AND,
                         src = tmp,
                         dst = dst,
                         size = dstsize}],
                     transfer = NONE}]
                end
             | Real_lt _ => (* sse_ucomis Instruction.A *)
                let
                   val (dst,dstsize) = getDst1 ()
                   val ((src1,src1size),
                        (src2,src2size)) = getSrc2 ()
                   val tmp =
                      fpeqTempContentsOperand dstsize

                   val _ 
                      = Assert.assert
                      ("amd64MLton.prim: Real_equal, src1size/src2size",
                       fn () => src1size = src2size)
                in
                   AppendList.fromList
                   [Block.mkBlock'
                    {entry = NONE,      
                     statements
                     = [Assembly.instruction_sse_ucomis
                        {src1 = src2,
                         src2 = src1,
                         size = src1size},
                        Assembly.instruction_setcc
                        {condition = Instruction.NP,
                         dst = tmp,
                         size = dstsize},
                        Assembly.instruction_setcc
                        {condition = Instruction.C,
                         dst = dst,
                         size = dstsize},
                        Assembly.instruction_binal
                        {oper = Instruction.AND,
                         src = tmp,
                         dst = dst,
                         size = dstsize}],
                     transfer = NONE}]
                end
             | Real_le _ => (* sse_ucomis Instruction.AE *)
                let
                   val (dst,dstsize) = getDst1 ()
                   val ((src1,src1size),
                        (src2,src2size)) = getSrc2 ()
                   val tmp =
                      fpeqTempContentsOperand dstsize

                   val _ 
                      = Assert.assert
                      ("amd64MLton.prim: Real_equal, src1size/src2size",
                       fn () => src1size = src2size)
                in
                   AppendList.fromList
                   [Block.mkBlock'
                    {entry = NONE,      
                     statements
                     = [Assembly.instruction_sse_ucomis
                        {src1 = src2,
                         src2 = src1,
                         size = src1size},
                        Assembly.instruction_setcc
                        {condition = Instruction.NP,
                         dst = tmp,
                         size = dstsize},
                        Assembly.instruction_setcc
                        {condition = Instruction.NA,
                         dst = dst,
                         size = dstsize},
                        Assembly.instruction_binal
                        {oper = Instruction.AND,
                         src = tmp,
                         dst = dst,
                         size = dstsize}],
                     transfer = NONE}]
                end
             | Real_mul _ => sse_binas Instruction.SSE_MULS
             | Real_muladd _ => sse_binas_mul Instruction.SSE_ADDS
             | Real_mulsub _ => sse_binas_mul Instruction.SSE_SUBS
             | Real_neg s =>
                let
                   val (dst,dstsize) = getDst1 ()
                   val (src,srcsize) = getSrc1 ()
                   val _ 
                     = Assert.assert
                       ("amd64MLton.prim: Real_neg, dstsize/srcsize",
                        fn () => srcsize = dstsize)
                   fun mkConst wordSize
                     = (WordX.notb o WordX.rshift)
                       (WordX.allOnes wordSize,
                        WordX.one wordSize,
                        {signed = false})

                   val (const,constsize) 
                     = case s of
                         R32 => (mkConst WordSize.word32, Size.LONG)
                       | R64 => (mkConst WordSize.word64, Size.QUAD)
                in
                   AppendList.fromList
                   [Block.mkBlock'
                    {entry = NONE,
                     statements 
                     = [Assembly.instruction_sse_movd
                        {dst = dst,
                         dstsize = dstsize,
                         src = Operand.immediate_word const,
                         srcsize = constsize},
                        Assembly.instruction_sse_binlp
                        {oper = Instruction.SSE_XORP,
                         src = src,
                         dst = dst,
                         size = dstsize}],
                     transfer = NONE}]

                end
             | Real_qequal _ =>
                let
                   val (dst,dstsize) = getDst1 ()
                   val ((src1,src1size),
                        (src2,src2size)) = getSrc2 ()
                   val tmp =
                      fpeqTempContentsOperand dstsize

                   val _ 
                      = Assert.assert
                      ("amd64MLton.prim: Real_qequal, src1size/src2size",
                       fn () => src1size = src2size)
                in
                   AppendList.fromList
                   [Block.mkBlock'
                    {entry = NONE,      
                     statements
                     = [Assembly.instruction_sse_ucomis
                        {src1 = src1,
                         src2 = src2,
                         size = src1size},
                        Assembly.instruction_setcc
                        {condition = Instruction.P,
                         dst = tmp,
                         size = dstsize},
                        Assembly.instruction_setcc
                        {condition = Instruction.E,
                         dst = dst,
                         size = dstsize},
                        Assembly.instruction_binal
                        {oper = Instruction.OR,
                         src = tmp,
                         dst = dst,
                         size = dstsize}],
                     transfer = NONE}]
                end
             | Real_rndToReal (s, s') =>
                  let
                     val b = RealSize.bits s
                     val b' = RealSize.bits s'
                  in
                     if Bits.equals (b, b')
                        then sse_movs ()
                     else sse_cvtsfp2sfp ()
                  end
             | Real_rndToWord (_, s', {signed}) =>
                  let
                     fun default () =
                        let
                           val (dst,dstsize) = getDst1 ()
                           val (src,srcsize) = getSrc1 ()
                        in
                           AppendList.fromList
                           [Block.mkBlock'
                            {entry = NONE,
                             statements 
                             = [Assembly.instruction_sse_cvtsfp2si
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
                              (fpcvtTempContentsOperand, Size.QUAD)
                        in
                           AppendList.fromList
                           [Block.mkBlock'
                            {entry = NONE,
                             statements 
                             = [Assembly.instruction_sse_cvtsfp2si
                                {src = src,
                                 dst = tmp,
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
                     case (WordSize.prim s', signed) of
                        (W8, _) => default' ()
                      | (W16, _) => default' ()
                      | (W32, false) => default' ()
                      | (W32, true) => default ()
                      | (W64, true) => default ()
                      | _ => Error.bug "amd64MLton.prim: Real_rndToWord, W64, false"
                  end
             | Real_sub _ => sse_binas Instruction.SSE_SUBS
             | Word_add _ => binal Instruction.ADD
             | Word_andb _ => binal Instruction.AND
             | Word_castToReal _ => sse_movd ()
             | Word_equal _ => cmp Instruction.E
             | Word_lshift _ => sral Instruction.SHL
             | Word_lt (_, sg) => compare (sg, Instruction.L, Instruction.B)
             | Word_mul (s, {signed}) =>
                (case WordSize.prim s of
                    W8 => pmd (if signed
                                  then Instruction.IMUL
                               else Instruction.MUL)
                  | W16 => imul2 ()
                  | W32 => imul2 ()
                  | W64 => imul2 ())
             | Word_neg _ => unal Instruction.NEG
             | Word_notb _ => unal Instruction.NOT
             | Word_orb _ => binal Instruction.OR
             | Word_quot (_, {signed}) =>
                  pmd (if signed then Instruction.IDIV else Instruction.DIV)
             | Word_rem (_, {signed}) =>
                  pmd (if signed then Instruction.IMOD else Instruction.MOD)
             | Word_rndToReal (s, _, {signed}) => 
                  let
                     fun default () =
                        let
                           val (dst,dstsize) = getDst1 ()
                           val (src,srcsize) = getSrc1 ()
                        in
                           AppendList.fromList
                           [Block.mkBlock'
                            {entry = NONE,
                             statements 
                             = [Assembly.instruction_sse_cvtsi2sfp
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
                              (fpcvtTempContentsOperand, Size.QUAD)
                        in
                           AppendList.fromList
                           [Block.mkBlock'
                            {entry = NONE,
                             statements 
                             = [Assembly.instruction_movx
                                {oper = if signed
                                           then Instruction.MOVSX
                                        else Instruction.MOVZX,
                                 src = src,
                                 dst = tmp,
                                 dstsize = tmpsize,
                                 srcsize = srcsize},
                                Assembly.instruction_sse_cvtsi2sfp
                                {src = tmp,
                                 dst = dst,
                                 srcsize = tmpsize,
                                 dstsize = dstsize}],
                             transfer = NONE}]
                        end
                  in
                     case (WordSize.prim s, signed) of
                        (W8, _) => default' ()
                      | (W16, _) => default' ()
                      | (W32, false) => default' ()
                      | (W32, true) => default ()
                      | (W64, true) => default ()
                      | _ => Error.bug "amd64MLton.prim: Word_rndToReal, W64, false"
                  end
             | Word_rol _ => sral Instruction.ROL
             | Word_ror _ => sral Instruction.ROR
             | Word_rshift (_, {signed}) =>
                  sral (if signed then Instruction.SAR else Instruction.SHR)
             | Word_sub _ => binal Instruction.SUB
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
             | Word_xorb _ => binal Instruction.XOR
             | _ => Error.bug ("amd64MLton.prim: strange Prim.Name.t: " ^ primName)),
         comment_end]
      end

  fun ccall {args: (amd64.Operand.t * amd64.Size.t) vector,
             frameInfo,
             func,
             return: amd64.Label.t option,
             transInfo = {...}: transInfo}
    = let
        val CFunction.T {convention, target, ...} = func
        val comment_begin
          = if !Control.Native.commented > 0
              then AppendList.single 
                   (amd64.Block.mkBlock'
                    {entry = NONE,
                     statements = 
                     [amd64.Assembly.comment
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
                             frameInfo = frameInfo,
                             func = func,
                             return = return})})]
      end

  fun creturn {dsts: (amd64.Operand.t * amd64.Size.t) vector,
               frameInfo: amd64.FrameInfo.t option,
               func: RepType.t CFunction.t,
               label: amd64.Label.t, 
               transInfo = {live, liveInfo, ...}: transInfo}
    = let
        val CFunction.T {convention, target, ...} = func
        fun default ()
          = let
              val _ = amd64Liveness.LiveInfo.setLiveOperands
                      (liveInfo, label, live label)
            in 
              AppendList.single
              (amd64.Block.mkBlock'
               {entry = SOME (Entry.creturn {dsts = dsts,
                                             frameInfo = frameInfo,
                                             func = func,
                                             label = label}),
                statements = [],
                transfer = NONE})
            end
        val comment_end
          = if !Control.Native.commented > 0
              then AppendList.single 
                   (amd64.Block.mkBlock'
                    {entry = NONE,
                     statements = 
                     [amd64.Assembly.comment
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

  fun arith {prim : RepType.t Prim.t,
             args : (Operand.t * Size.t) vector,
             dsts : (Operand.t * Size.t) vector,
             overflow : Label.t,
             success : Label.t,
             transInfo = {...} : transInfo}
    = let
        val primName = Prim.toString prim
        datatype z = datatype Prim.Name.t

        fun getDst1 ()
          = Vector.sub (dsts, 0)
            handle _ => Error.bug "amd64MLton.arith: getDst1"
        fun getSrc1 ()
          = Vector.sub (args, 0)
            handle _ => Error.bug "amd64MLton.arith: getSrc1"
        fun getSrc2 ()
          = (Vector.sub (args, 0), Vector.sub (args, 1))
            handle _ => Error.bug "amd64MLton.arith: getSrc2"

        fun check (statements, condition)
          = AppendList.single
            (amd64.Block.mkBlock'
             {entry = NONE,     
              statements = statements,
              transfer = SOME (amd64.Transfer.iff
                               {condition = condition,
                                truee = overflow,
                                falsee = success})})
        fun binal (oper: amd64.Instruction.binal, condition)
          = let
              val (dst, dstsize) = getDst1 ()
              val ((src1, src1size), (src2, src2size)) = getSrc2 ()
              val _ = Assert.assert
                      ("amd64MLton.arith: binal, dstsize/src1size/src2size",
                       fn () => src1size = dstsize andalso src2size = dstsize)
              (* Reverse src1/src2 when src1 and src2 are
               * temporaries and the oper is commutative. 
               *)
              val (src1,src2)
                = if (oper = amd64.Instruction.ADD)
                    then case (amd64.Operand.deMemloc src1,
                               amd64.Operand.deMemloc src2)
                           of (SOME memloc_src1, SOME memloc_src2)
                            => if amd64Liveness.track memloc_src1
                                  andalso
                                  amd64Liveness.track memloc_src2
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
        fun pmd (oper: amd64.Instruction.md, condition)
          = let
              val (dst, dstsize) = getDst1 ()
              val ((src1, src1size), (src2, src2size)) = getSrc2 ()
              val _ = Assert.assert
                      ("amd64MLton.arith: pmd, dstsize/src1size/src2size",
                       fn () => src1size = dstsize andalso src2size = dstsize)
              (* Reverse src1/src2 when src1 and src2 are
               * temporaries and the oper is commutative. 
               *)
              val (src1, src2)
                = if oper = amd64.Instruction.MUL
                    then case (amd64.Operand.deMemloc src1,
                               amd64.Operand.deMemloc src2)
                           of (SOME memloc_src1, SOME memloc_src2)
                            => if amd64Liveness.track memloc_src1
                                  andalso
                                  amd64Liveness.track memloc_src2
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
        fun unal (oper: amd64.Instruction.unal, condition)
          = let
              val (dst, dstsize) = getDst1 ()
              val (src1, src1size) = getSrc1 ()
              val _ = Assert.assert
                      ("amd64MLton.arith: unal, dstsize/src1size",
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

        fun imul2 condition
          = let
              val (dst, dstsize) = getDst1 ()
              val ((src1, src1size), (src2, src2size)) = getSrc2 ()
              val _ = Assert.assert
                      ("amd64MLton.arith: imul2, dstsize/src1size/src2size",
                       fn () => src1size = dstsize andalso src2size = dstsize)
              (* Reverse src1/src2 when src1 and src2 are
               * temporaries and the oper is commutative. 
               *)
              val (src1, src2)
                = case (amd64.Operand.deMemloc src1,
                        amd64.Operand.deMemloc src2)
                    of (SOME memloc_src1, SOME memloc_src2)
                     => if amd64Liveness.track memloc_src1
                           andalso
                           amd64Liveness.track memloc_src2
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
                      (amd64.Block.mkBlock'
                       {entry = NONE,
                        statements 
                        = [amd64.Assembly.comment 
                           ("begin arith: " ^ comment)],
                        transfer = NONE}),
                      AppendList.single
                      (amd64.Block.mkBlock'
                       {entry = NONE,
                        statements 
                        = [amd64.Assembly.comment 
                           ("end arith: " ^ comment)],
                        transfer = NONE}))
                   end
              else (AppendList.empty,AppendList.empty)
        fun flag {signed} =
           if signed then amd64.Instruction.O else amd64.Instruction.C
      in
        AppendList.appends
        [comment_begin,
         (case Prim.name prim of
             Word_addCheck (_, sg) =>
                binal (amd64.Instruction.ADD, flag sg)
           | Word_mulCheck (s, {signed}) =>
                let
                in
                   if signed
                      then
                         (case WordSize.prim s of
                             W8 => pmd (amd64.Instruction.IMUL, amd64.Instruction.O)
                           | W16 => imul2 amd64.Instruction.O
                           | W32 => imul2 amd64.Instruction.O
                           | W64 => imul2 amd64.Instruction.O)
                   else
                      pmd (amd64.Instruction.MUL, amd64.Instruction.C)
                end
           | Word_negCheck _ => 
               unal (amd64.Instruction.NEG, amd64.Instruction.O)
           | Word_subCheck (_, sg) =>
               binal (amd64.Instruction.SUB, flag sg)
           | _ => Error.bug ("amd64MLton.arith: strange Prim.Name.t: " ^ primName))]
      end

end
