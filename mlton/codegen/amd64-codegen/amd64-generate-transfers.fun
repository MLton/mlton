(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor amd64GenerateTransfers(S: AMD64_GENERATE_TRANSFERS_STRUCTS): AMD64_GENERATE_TRANSFERS =
struct

  open S
  open amd64
  open amd64JumpInfo
  open amd64LoopInfo
  open amd64Liveness
  open LiveInfo
  open Liveness

  local
     open Runtime
  in
     structure CFunction = CFunction
  end

  val ones : int * WordSize.t -> WordX.t
    = fn (i, ws) => (WordX.notb o WordX.lshift) 
                    (WordX.allOnes ws,
                     WordX.fromIntInf (IntInf.fromInt i, ws))

  val tracerTop = amd64.tracerTop

  structure amd64LiveTransfers 
    = amd64LiveTransfers(structure amd64 = amd64
                       structure amd64Liveness = amd64Liveness
                       structure amd64JumpInfo = amd64JumpInfo
                       structure amd64LoopInfo = amd64LoopInfo)

  val pointerSize = amd64MLton.pointerSize
  val wordSize = amd64MLton.wordSize

  val normalRegs =
    let
      val transferRegs
        =
          (*
          Register.rax::
          Register.eax::
          Register.al::
          *)
          Register.rbx::
          Register.ebx::
          Register.bl::
          Register.rcx::
          Register.ecx::
          Register.cl::
          Register.rdx:: 
          Register.edx:: 
          Register.dl::
          Register.rdi::
          Register.rsi::
          (*
          Register.rsp::
          Register.rbp::
          *)
          Register.r8::
          Register.r8w::
          Register.r9::
          Register.r9w::
          Register.r10::
          Register.r10w::
          Register.r11::
          Register.r11w::
          (*
          Register.r12::
          Register.r12w::
          *)
          Register.r13::
          Register.r13w::
          Register.r14::
          Register.r14w::
          Register.r15::
          Register.r15w::
          nil

      val transferXmmRegs =
          XmmRegister.xmm1D::
          XmmRegister.xmm1S::
          XmmRegister.xmm2D::
          XmmRegister.xmm2S::
          XmmRegister.xmm3D::
          XmmRegister.xmm3S::
          XmmRegister.xmm4D::
          XmmRegister.xmm4S::
          XmmRegister.xmm5D::
          XmmRegister.xmm5S::
          XmmRegister.xmm6D::
          XmmRegister.xmm6S::
          XmmRegister.xmm7D::
          XmmRegister.xmm7S::
          nil
     in
        {frontierReg = Register.r12,
         stackTopReg = Register.rbp,
         transferRegs = fn Entry.Jump _ => transferRegs
                         | Entry.CReturn _ => Register.rax::Register.eax::Register.al::transferRegs
                         | _ => [],
         transferXmmRegs = fn Entry.Jump _ => transferXmmRegs
                             | Entry.CReturn _ => XmmRegister.xmm0D::XmmRegister.xmm0S::transferXmmRegs
                             | _ => []}
     end

  val reserveRspRegs =
    let
      val transferRegs
        =
          (*
          Register.rax::
          Register.eax::
          Register.al::
          *)
          Register.rbx::
          Register.ebx::
          Register.bl::
          Register.rcx::
          Register.ecx::
          Register.cl::
          Register.rdx:: 
          Register.edx:: 
          Register.dl::
          Register.rdi::
          Register.rsi::
          (*
          Register.rsp::
          Register.rbp::
          *)
          Register.r8::
          Register.r8w::
          Register.r9::
          Register.r9w::
          Register.r10::
          Register.r10w::
          Register.r11::
          Register.r11w::
          (*
          Register.r12::
          Register.r12w::
          *)
          Register.r13::
          Register.r13w::
          Register.r14::
          Register.r14w::
          Register.r15::
          Register.r15w::
          nil

      val transferXmmRegs =
          XmmRegister.xmm8D::
          XmmRegister.xmm8S::
          XmmRegister.xmm9D::
          XmmRegister.xmm9S::
          XmmRegister.xmm10D::
          XmmRegister.xmm10S::
          XmmRegister.xmm11D::
          XmmRegister.xmm11S::
          XmmRegister.xmm12D::
          XmmRegister.xmm12S::
          XmmRegister.xmm13D::
          XmmRegister.xmm13S::
          XmmRegister.xmm14D::
          XmmRegister.xmm14S::
          nil
     in
        {frontierReg = Register.r12,
         stackTopReg = Register.rbp,
         transferRegs = fn Entry.Jump _ => transferRegs
                         | Entry.CReturn _ => Register.rax::Register.eax::Register.al::transferRegs
                         | _ => [],
         transferXmmRegs = fn Entry.Jump _ => transferXmmRegs
                             | Entry.CReturn _ => XmmRegister.xmm0D::XmmRegister.xmm0S::transferXmmRegs
                             | _ => []}
     end

  val indexReg = amd64.Register.rax

  val stackTop = amd64MLton.gcState_stackTopContents
  val frontier = amd64MLton.gcState_frontierContents

  datatype gef = GEF of {generate : gef -> 
                                    {label : Label.t,
                                     falling : bool,
                                     unique : bool} -> 
                                    Assembly.t AppendList.t,
                         effect : gef -> 
                                  {label : Label.t,
                                   transfer : Transfer.t} ->
                                  Assembly.t AppendList.t,
                         fall : gef ->
                                {label : Label.t,
                                 live : LiveSet.t} ->
                                Assembly.t AppendList.t}

  fun generateTransfers {chunk as Chunk.T {data, blocks, ...},
                         optimize: int,
                         newProfileLabel: amd64.ProfileLabel.t -> amd64.ProfileLabel.t,
                         liveInfo : amd64Liveness.LiveInfo.t,
                         jumpInfo : amd64JumpInfo.t,
                         reserveRsp: bool}
    = let
         val {frontierReg, stackTopReg, transferRegs, transferXmmRegs} =
            if reserveRsp
               then reserveRspRegs
            else normalRegs
        val allClasses = !amd64MLton.Classes.allClasses
        val livenessClasses = !amd64MLton.Classes.livenessClasses
        val livenessClasses = ClassSet.+(livenessClasses, 
                                         ClassSet.fromList
                                         [amd64MLton.Classes.StaticNonTemp,
                                          amd64MLton.Classes.CArg])
        val nonlivenessClasses = ClassSet.-(allClasses, livenessClasses)
        val holdClasses = !amd64MLton.Classes.holdClasses
        val farflushClasses = ClassSet.-(nonlivenessClasses, holdClasses)
        val nearflushClasses = ClassSet.-(nonlivenessClasses, holdClasses)
        val runtimeClasses = !amd64MLton.Classes.runtimeClasses
        val cstaticClasses = !amd64MLton.Classes.cstaticClasses
        val heapClasses = !amd64MLton.Classes.heapClasses
        val ccallflushClasses = ClassSet.+(cstaticClasses, heapClasses)

        fun removeHoldMemLocs memlocs
          = MemLocSet.subset
            (memlocs, 
             fn m => not (ClassSet.contains(holdClasses, MemLoc.class m)))

        val stackAssume = {register = stackTopReg,
                           memloc = stackTop (),
                           weight = 1024,
                           sync = false,
                           reserve = false}
        val frontierAssume = {register = frontierReg,
                              memloc = frontier (),
                              weight = 2048,
                              sync = false,
                              reserve = false}
        val cStackAssume = {register = Register.rsp,
                            memloc = amd64MLton.c_stackPContents,
                            weight = 2048, (* ??? *)
                            sync = false,
                            reserve = true}

        fun blockAssumes l =
           let
              val l = frontierAssume :: stackAssume :: l
           in
              Assembly.directive_assume {assumes = if reserveRsp
                                                      then cStackAssume :: l
                                                   else l}
           end

        fun runtimeTransfer live setup trans
          = AppendList.appends
            [AppendList.single
             (Assembly.directive_force
              {commit_memlocs = removeHoldMemLocs live,
               commit_classes = ClassSet.empty,
               remove_memlocs = MemLocSet.empty,
               remove_classes = ClassSet.empty,
               dead_memlocs = MemLocSet.empty,
               dead_classes = ClassSet.empty}),
             setup,
             AppendList.fromList
             [(Assembly.directive_force
               {commit_memlocs = MemLocSet.empty,
                commit_classes = farflushClasses,
                remove_memlocs = MemLocSet.empty,
                remove_classes = ClassSet.empty,
                dead_memlocs = MemLocSet.empty,
                dead_classes = ClassSet.empty})],
             trans]

        fun farEntry l = AppendList.cons (blockAssumes [], l)

        fun farTransfer live setup trans
          = AppendList.appends
            [AppendList.single
             (Assembly.directive_force
              {commit_memlocs = removeHoldMemLocs live,
               commit_classes = ClassSet.empty,
               remove_memlocs = MemLocSet.empty,
               remove_classes = ClassSet.empty,
               dead_memlocs = MemLocSet.empty,
               dead_classes = ClassSet.empty}),
             setup,
             AppendList.fromList
             [(Assembly.directive_cache
               {caches = [{register = stackTopReg,
                           memloc = stackTop (),
                           reserve = true},
                          {register = frontierReg,
                           memloc = frontier (),
                           reserve = true}]}),
              (Assembly.directive_xmmcache
               {caches = []}),
              (Assembly.directive_force
               {commit_memlocs = MemLocSet.empty,
                commit_classes = farflushClasses,
                remove_memlocs = MemLocSet.empty,
                remove_classes = ClassSet.empty,
                dead_memlocs = MemLocSet.empty,
                dead_classes = ClassSet.empty})],
             trans]

        val profileStackTopCommit' = 
          amd64.Assembly.directive_force
          {commit_memlocs = MemLocSet.singleton (stackTop ()),
           commit_classes = ClassSet.empty,
           remove_memlocs = MemLocSet.empty,
           remove_classes = ClassSet.empty,
           dead_memlocs = MemLocSet.empty,
           dead_classes = ClassSet.empty}
        val profileStackTopCommit =
          if !Control.profile <> Control.ProfileNone
            then AppendList.single profileStackTopCommit'
            else AppendList.empty

        val _
          = Assert.assert
            ("amd64GenerateTransfers.verifyLiveInfo",
             fn () => amd64Liveness.LiveInfo.verifyLiveInfo {chunk = chunk,
                                                           liveInfo = liveInfo})
        val _
          = Assert.assert
            ("amd64GenerateTransfers.verifyJumpInfo", 
             fn () => amd64JumpInfo.verifyJumpInfo {chunk = chunk,
                                                  jumpInfo = jumpInfo})

        val _
          = Assert.assert
            ("amd64GenerateTransfers.verifyEntryTransfer", 
             fn () => amd64EntryTransfer.verifyEntryTransfer {chunk = chunk})

        local
          val {get: Label.t -> {block:Block.t},
               set,
               destroy}
            = Property.destGetSetOnce
              (Label.plist, Property.initRaise ("gotoInfo", Label.layout))

          val labels
            = List.fold
              (blocks, [],
               fn (block as Block.T {entry, ...}, labels)
                => let
                     val label = Entry.label entry
                   in
                     set(label, {block = block}) ;
                     label::labels
                   end)

          fun loop labels
            = let
                val (labels, b)
                  = List.fold
                    (labels, ([], false),
                     fn (label, (labels, b))
                      => case amd64JumpInfo.getNear (jumpInfo, label)
                           of amd64JumpInfo.Count 0 
                            => let
                                 val {block = Block.T {transfer, ...}}
                                   = get label
                               in
                                 List.foreach 
                                 (Transfer.nearTargets transfer,
                                  fn label 
                                   => amd64JumpInfo.decNear (jumpInfo, label));
                                 (labels, true)
                               end
                            | _ => (label::labels, b))
              in
                if b
                  then loop labels
                  else List.map (labels, #block o get)
              end
          val blocks = loop labels

          val _ = destroy ()
        in
          val chunk = Chunk.T {data = data, blocks = blocks}
        end

        val loopInfo
          = amd64LoopInfo.createLoopInfo {chunk = chunk, farLoops = false}
        val isLoopHeader
          = fn label => isLoopHeader(loopInfo, label)
                        handle _ => false

        val liveTransfers
          = amd64LiveTransfers.computeLiveTransfers
            {chunk = chunk,
             transferRegs = transferRegs,
             transferXmmRegs = transferXmmRegs,
             liveInfo = liveInfo,
             jumpInfo = jumpInfo,
             loopInfo = loopInfo}

        val getLiveRegsTransfers
          = #1 o amd64LiveTransfers.getLiveTransfers
        val getLiveXmmRegsTransfers
          = #2 o amd64LiveTransfers.getLiveTransfers

        val {get = getLayoutInfo : Label.t -> Block.t option,
             set = setLayoutInfo,
             destroy = destLayoutInfo}
          = Property.destGetSet(Label.plist, 
                                Property.initRaise ("layoutInfo", Label.layout))
        val _ 
          = List.foreach
            (blocks,
             fn block as Block.T {entry, ...}
              => let
                   val label = Entry.label entry
                 in 
                   setLayoutInfo(label, SOME block)
                 end)

        val {get = getProfileLabel : Label.t -> ProfileLabel.t option,
             set = setProfileLabel,
             destroy = destProfileLabel}
          = Property.destGetSetOnce
            (Label.plist, 
             Property.initRaise ("profileLabel", Label.layout))
        val _ 
          = List.foreach
            (blocks,
             fn Block.T {entry, profileLabel, ...}
              => let
                   val label = Entry.label entry
                 in 
                   setProfileLabel(label, profileLabel)
                 end)

        local   
          val stack = ref []
          val queue = ref (Queue.empty ())
        in
          fun enque x = queue := Queue.enque(!queue, x)
          fun push x = stack := x::(!stack)

          fun deque () = (case (!stack)
                            of [] => (case Queue.deque(!queue)
                                        of NONE => NONE
                                         | SOME(queue', x) => (queue := queue';
                                                               SOME x))
                             | x::stack' => (stack := stack';
                                             SOME x))
        end

        fun pushCompensationBlock {label, id}
          = let
              val label' = Label.new label
              val live = getLive(liveInfo, label)
              val profileLabel = getProfileLabel label
              val profileLabel' = Option.map (profileLabel, newProfileLabel)
              val block
                = Block.T {entry = Entry.jump {label = label'},
                           profileLabel = profileLabel',
                           statements 
                           = (Assembly.directive_restoreregalloc
                              {live = MemLocSet.add
                                      (MemLocSet.add
                                       (LiveSet.toMemLocSet live,
                                        stackTop ()),
                                      frontier ()),
                               id = id})::
                             nil,
                           transfer = Transfer.goto {target = label}}
            in
              setLive(liveInfo, label', live);
              setProfileLabel(label', profileLabel');
              incNear(jumpInfo, label');
              Assert.assert("amd64GenerateTransfers.pushCompensationBlock",
                            fn () => getNear(jumpInfo, label') = Count 1);
              amd64LiveTransfers.setLiveTransfersEmpty(liveTransfers, label');
              setLayoutInfo(label', SOME block);
              push label';
              label'
            end

        val c_stackP = amd64MLton.c_stackPContentsOperand

        fun cacheRsp () =
           if reserveRsp
              then AppendList.empty
           else
              AppendList.single
              ((* explicit cache in case there are no args *)
               Assembly.directive_cache 
               {caches = [{register = Register.rsp,
                           memloc = valOf (Operand.deMemloc c_stackP),
                           reserve = true}]})

        fun unreserveRsp () =
           if reserveRsp
              then AppendList.empty
           else AppendList.single (Assembly.directive_unreserve 
                                   {registers = [Register.rsp]})
        
        datatype z = datatype Entry.t
        datatype z = datatype Transfer.t
        fun generateAll (gef as GEF {effect,...})
                        {label, falling, unique} : 
                        Assembly.t AppendList.t
          = (case getLayoutInfo label
               of NONE => AppendList.empty
                | SOME (Block.T {entry, profileLabel, statements, transfer})
                => let
                     val _ = setLayoutInfo(label, NONE)
(*
                     val isLoopHeader = fn _ => false
*)
                     fun near label =
                        let
                           val align =
                              if isLoopHeader label handle _ => false
                                 then
                                    AppendList.single
                                    (Assembly.pseudoop_p2align 
                                     (Immediate.int 4,
                                      NONE,
                                      SOME (Immediate.int 7)))
                              else if falling
                                      then AppendList.empty
                                   else
                                      AppendList.single
                                      (Assembly.pseudoop_p2align
                                       (Immediate.int 4, 
                                        NONE, 
                                        NONE))
                           val assumes =
                              if falling andalso unique
                                 then AppendList.empty
                              else
                                 (* near entry & live transfer assumptions *)
                                 AppendList.fromList
                                 [(blockAssumes
                                   (List.map
                                    (getLiveRegsTransfers
                                     (liveTransfers, label),
                                     fn (memloc,register,sync)
                                     => {register = register,
                                         memloc = memloc,
                                         sync = sync, 
                                         weight = 1024,
                                         reserve = false}))),
                                  (Assembly.directive_xmmassume
                                   {assumes
                                    = (List.map
                                       (getLiveXmmRegsTransfers
                                        (liveTransfers, label),
                                        fn (memloc,register,sync)
                                        => {register = register,
                                            memloc = memloc,
                                            sync = sync,
                                            weight = 1024,
                                            reserve = false}))})]
                        in
                           AppendList.appends
                           [align,
                            AppendList.single 
                            (Assembly.label label),
                            AppendList.fromList 
                            (ProfileLabel.toAssemblyOpt profileLabel),
                            assumes]
                        end
                     val pre
                       = case entry
                           of Jump {label}
                            => near label
                            | CReturn {dsts, frameInfo, func, label}
                            => let
                                 fun getReturn () =
                                    if Vector.length dsts = 0
                                       then AppendList.empty
                                       else let
                                               val srcs =
                                                  Vector.fromList
                                                  (List.map
                                                   (Operand.cReturnTemps
                                                    (CFunction.return func),
                                                    #dst))
                                            in
                                               (AppendList.fromList o Vector.fold2)
                                               (dsts, srcs, [], fn ((dst,dstsize),src,stmts) =>
                                                case Size.class dstsize of
                                                   Size.INT =>
                                                      (amd64.Assembly.instruction_mov
                                                       {dst = dst,
                                                        src = Operand.memloc src,
                                                        size = dstsize})::stmts
                                                 | Size.FLT =>
                                                      (amd64.Assembly.instruction_sse_movs
                                                       {dst = dst,
                                                        src = Operand.memloc src,
                                                        size = dstsize})::stmts)
                                            end
                               in
                                 case frameInfo of
                                   SOME fi =>
                                      let
                                          val FrameInfo.T {size, frameLayoutsIndex}
                                            = fi
                                          val finish
                                            = AppendList.appends
                                              [let      
                                                 val stackTop 
                                                   = amd64MLton.gcState_stackTopContentsOperand ()
                                                 val bytes 
                                                   = amd64.Operand.immediate_int (~ size)
                                               in
                                                 AppendList.cons
                                                 ((* stackTop += bytes *)
                                                  amd64.Assembly.instruction_binal 
                                                  {oper = amd64.Instruction.ADD,
                                                   dst = stackTop,
                                                   src = bytes, 
                                                   size = pointerSize},
                                                 profileStackTopCommit)
                                               end,
                                               (* assignTo dst *)
                                               getReturn ()]
                                        in
                                          AppendList.appends
                                          [AppendList.fromList
                                           [Assembly.pseudoop_p2align 
                                            (Immediate.int 4, NONE, NONE),
                                            Assembly.pseudoop_long 
                                            [Immediate.int frameLayoutsIndex],
                                            Assembly.label label],
                                           AppendList.fromList
                                           (ProfileLabel.toAssemblyOpt profileLabel),
                                           if CFunction.maySwitchThreads func
                                             then (* entry from far assumptions *)
                                                  farEntry finish
                                             else (* near entry & live transfer assumptions *)
                                                  AppendList.append
                                                  (AppendList.fromList
                                                   [(blockAssumes
                                                     (List.map
                                                      (getLiveRegsTransfers
                                                       (liveTransfers, label),
                                                       fn (memloc,register,sync)
                                                       => {register = register,
                                                           memloc = memloc,
                                                           sync = sync, 
                                                           weight = 1024,
                                                           reserve = false}))),
                                                    (Assembly.directive_xmmassume
                                                     {assumes
                                                      = (List.map
                                                         (getLiveXmmRegsTransfers
                                                          (liveTransfers, label),
                                                          fn (memloc,register,sync)
                                                          => {register = register,
                                                              memloc = memloc,
                                                              sync = sync,
                                                              weight = 1024,
                                                              reserve = false}))})],
                                                   finish)]
                                        end
                                 | NONE => 
                                      AppendList.append (near label, getReturn ())
                               end
                            | Func {label,...}
                            => AppendList.appends
                               [AppendList.fromList
                                [Assembly.pseudoop_p2align 
                                 (Immediate.int 4, NONE, NONE),
                                 Assembly.pseudoop_global label,
                                 Assembly.pseudoop_hidden label,
                                 Assembly.label label],
                                AppendList.fromList
                                (ProfileLabel.toAssemblyOpt profileLabel),
                                (* entry from far assumptions *)
                                (farEntry AppendList.empty)]
                            | Cont {label, 
                                    frameInfo = FrameInfo.T {size,
                                                             frameLayoutsIndex},
                                    ...}
                            =>
                               AppendList.appends
                               [AppendList.fromList
                                [Assembly.pseudoop_p2align
                                 (Immediate.int 4, NONE, NONE),
                                 Assembly.pseudoop_long
                                 [Immediate.int frameLayoutsIndex],
                                 Assembly.label label],
                                AppendList.fromList
                                (ProfileLabel.toAssemblyOpt profileLabel),
                                (* entry from far assumptions *)
                                (farEntry
                                 (let
                                     val stackTop 
                                        = amd64MLton.gcState_stackTopContentsOperand ()
                                     val bytes 
                                        = amd64.Operand.immediate_int (~ size)
                                  in
                                     AppendList.cons
                                     ((* stackTop += bytes *)
                                      amd64.Assembly.instruction_binal 
                                      {oper = amd64.Instruction.ADD,
                                       dst = stackTop,
                                       src = bytes, 
                                       size = pointerSize},
                                      profileStackTopCommit)
                                  end))]
                            | Handler {frameInfo = (FrameInfo.T
                                                    {frameLayoutsIndex, size}),
                                       label,
                                       ...}
                            => AppendList.appends
                               [AppendList.fromList
                                [Assembly.pseudoop_p2align 
                                 (Immediate.int 4, NONE, NONE),
                                 Assembly.pseudoop_long
                                 [Immediate.int frameLayoutsIndex],
                                 Assembly.label label],
                                AppendList.fromList
                                (ProfileLabel.toAssemblyOpt profileLabel),
                                (* entry from far assumptions *)
                                (farEntry
                                 (let
                                     val stackTop 
                                        = amd64MLton.gcState_stackTopContentsOperand ()
                                     val bytes 
                                        = amd64.Operand.immediate_int (~ size)
                                  in
                                     AppendList.cons
                                     ((* stackTop += bytes *)
                                      amd64.Assembly.instruction_binal 
                                      {oper = amd64.Instruction.ADD,
                                       dst = stackTop,
                                       src = bytes, 
                                       size = pointerSize},
                                      profileStackTopCommit)
                                  end))]
                     val pre
                       = AppendList.appends
                         [if !Control.Native.commented > 1
                            then AppendList.single
                                 (Assembly.comment (Entry.toString entry))
                            else AppendList.empty,
                          if !Control.Native.commented > 2
                            then AppendList.single
                                 (Assembly.comment 
                                  (LiveSet.fold
                                   (getLive(liveInfo, label),
                                    "",
                                    fn (memloc, s)
                                     => concat [s, 
                                                MemLoc.toString memloc, 
                                                " "])))
                            else AppendList.empty,
                          pre]

                     val (statements,_)
                       = List.foldr
                         (statements,
                          ([], 
                           Liveness.liveIn
                           (livenessTransfer {transfer = transfer, 
                                              liveInfo = liveInfo})),
                          fn (assembly,(statements,live))
                           => let
                                val Liveness.T {liveIn,dead, ...}
                                  = livenessAssembly {assembly = assembly,
                                                      live = live}
                              in
                                (if LiveSet.isEmpty dead
                                   then assembly::statements
                                   else assembly::
                                        (Assembly.directive_force
                                         {commit_memlocs = MemLocSet.empty,
                                          commit_classes = ClassSet.empty,
                                          remove_memlocs = MemLocSet.empty,
                                          remove_classes = ClassSet.empty,
                                          dead_memlocs = LiveSet.toMemLocSet dead,
                                          dead_classes = ClassSet.empty})::
                                        statements,
                                 liveIn)
                              end)

                     val statements = AppendList.fromList statements

                     val transfer = effect gef {label = label, 
                                                transfer = transfer}
                   in
                     AppendList.appends 
                     [pre,
                      statements,
                      transfer]
                   end)

        and effectDefault (gef as GEF {fall,...})
                          {label, transfer} : Assembly.t AppendList.t
          = AppendList.append
            (if !Control.Native.commented > 1
               then AppendList.single
                    (Assembly.comment
                     (Transfer.toString transfer))
               else AppendList.empty,
             case transfer
               of Goto {target}
                => fall gef
                        {label = target,
                         live = getLive(liveInfo, target)}
                | Iff {condition, truee, falsee}
                => let
                     val condition_neg
                       = Instruction.condition_negate condition

                     val truee_live
                       = getLive(liveInfo, truee)
                     val truee_live_length
                       = LiveSet.size truee_live

                     val falsee_live
                       = getLive(liveInfo, falsee)
                     val falsee_live_length
                       = LiveSet.size falsee_live

                     fun fall_truee ()
                       = let
                           val id = Directive.Id.new ()
                           val falsee'
                             = pushCompensationBlock {label = falsee,
                                                      id = id};
                         in
                           AppendList.append
                           (AppendList.fromList
                            [Assembly.directive_force
                             {commit_memlocs = MemLocSet.empty,
                              commit_classes = nearflushClasses,
                              remove_memlocs = MemLocSet.empty,
                              remove_classes = ClassSet.empty,
                              dead_memlocs = MemLocSet.empty,
                              dead_classes = ClassSet.empty},
                             Assembly.instruction_jcc
                             {condition = condition_neg,
                              target = Operand.label falsee'},
                             Assembly.directive_saveregalloc
                             {live = MemLocSet.add
                                     (MemLocSet.add
                                      (LiveSet.toMemLocSet falsee_live,
                                       stackTop ()),
                                      frontier ()),
                              id = id}],
                            (fall gef 
                                  {label = truee,
                                   live = truee_live}))
                         end

                     fun fall_falsee ()
                       = let
                           val id = Directive.Id.new ()
                           val truee' = pushCompensationBlock {label = truee,
                                                               id = id};
                         in
                           AppendList.append
                           (AppendList.fromList
                            [Assembly.directive_force
                             {commit_memlocs = MemLocSet.empty,
                              commit_classes = nearflushClasses,
                              remove_memlocs = MemLocSet.empty,
                              remove_classes = ClassSet.empty,
                              dead_memlocs = MemLocSet.empty,
                              dead_classes = ClassSet.empty},
                             Assembly.instruction_jcc
                             {condition = condition,
                              target = Operand.label truee'},
                             Assembly.directive_saveregalloc
                             {live = MemLocSet.add
                                     (MemLocSet.add
                                      (LiveSet.toMemLocSet truee_live,
                                       stackTop ()),
                                      frontier ()),
                              id = id}],
                            (fall gef 
                                  {label = falsee,
                                   live = falsee_live}))
                         end
                   in
                     case (getLayoutInfo truee,
                           getLayoutInfo falsee)
                       of (NONE, SOME _) => fall_falsee ()
                        | (SOME _, NONE) => fall_truee ()
                        | _ 
                        => let
                             fun default' ()
                               = if truee_live_length <= falsee_live_length
                                   then fall_falsee ()
                                   else fall_truee ()

                             fun default ()
                               = case (getNear(jumpInfo, truee),
                                       getNear(jumpInfo, falsee))
                                   of (Count 1, Count 1) => default' ()
                                    | (Count 1, _) => fall_truee ()
                                    | (_, Count 1) => fall_falsee ()
                                    | _ => default' ()
                           in 
                             case (getLoopDistance(loopInfo, label, truee),
                                   getLoopDistance(loopInfo, label, falsee))
                               of (NONE, NONE) => default ()
                                | (SOME _, NONE) => fall_truee ()
                                | (NONE, SOME _) => fall_falsee ()
                                | (SOME dtruee, SOME dfalsee)
                                => (case Int.compare(dtruee, dfalsee)
                                      of EQUAL => default ()
                                       | LESS => fall_falsee ()
                                       | GREATER => fall_truee ())
                           end
                   end
                | Switch {test, cases, default}
                => let
                     val Liveness.T {dead, ...}
                       = livenessTransfer {transfer = transfer,
                                           liveInfo = liveInfo}

                     val size
                       = case Operand.size test
                           of SOME size => size
                            | NONE => Size.QUAD

                     val default_live
                       = getLive(liveInfo, default)

                     val cases
                       = Transfer.Cases.mapToList
                         (cases,
                          fn (k, target)
                           => let
                                val target_live
                                  = getLive(liveInfo, target)
                                val id = Directive.Id.new ()
                                val target' = pushCompensationBlock 
                                              {label = target,
                                               id = id}
                              in
                                AppendList.fromList
                                [Assembly.instruction_cmp
                                 {src1 = test,
                                  src2 = Operand.immediate_word k,
                                  size = size},
                                 Assembly.instruction_jcc
                                 {condition = Instruction.E,
                                  target = Operand.label target'},
                                 Assembly.directive_saveregalloc
                                 {live = MemLocSet.add
                                         (MemLocSet.add
                                          (LiveSet.toMemLocSet target_live,
                                           stackTop ()),
                                          frontier ()),
                                  id = id}]
                              end)
                   in
                     AppendList.appends
                     [AppendList.single
                      (Assembly.directive_force
                       {commit_memlocs = MemLocSet.empty,
                        commit_classes = nearflushClasses,
                        remove_memlocs = MemLocSet.empty,
                        remove_classes = ClassSet.empty,
                        dead_memlocs = MemLocSet.empty,
                        dead_classes = ClassSet.empty}),
                      AppendList.appends cases,
                      if LiveSet.isEmpty dead
                        then AppendList.empty
                        else AppendList.single
                             (Assembly.directive_force
                              {commit_memlocs = MemLocSet.empty,
                               commit_classes = ClassSet.empty,
                               remove_memlocs = MemLocSet.empty,
                               remove_classes = ClassSet.empty,
                               dead_memlocs = LiveSet.toMemLocSet dead,
                               dead_classes = ClassSet.empty}),
                      (fall gef
                            {label = default,
                             live = default_live})]
                   end
                | Tail {target, live}
                => (* flushing at far transfer *)
                   (farTransfer live
                    AppendList.empty
                    (AppendList.single
                     (Assembly.instruction_jmp
                      {target = Operand.label target,
                       absolute = false})))
                | NonTail {target, live, return, handler, size}
                => let
                     val _ = enque return
                     val _ = case handler
                               of SOME handler => enque handler
                                | NONE => ()

                     val stackTopTemp
                       = amd64MLton.stackTopTempContentsOperand ()
                     val stackTopTempMinusWordDeref'
                       = amd64MLton.stackTopTempMinusWordDeref ()
                     val stackTopTempMinusWordDeref
                       = amd64MLton.stackTopTempMinusWordDerefOperand ()
                     val stackTop 
                       = amd64MLton.gcState_stackTopContentsOperand ()
                     val stackTopMinusWordDeref'
                       = amd64MLton.gcState_stackTopMinusWordDeref ()
                     val stackTopMinusWordDeref
                       = amd64MLton.gcState_stackTopMinusWordDerefOperand ()
                     val bytes 
                       = amd64.Operand.immediate_int size

                     val liveReturn = amd64Liveness.LiveInfo.getLive(liveInfo, return)
                     val liveHandler 
                       = case handler
                           of SOME handler
                            => amd64Liveness.LiveInfo.getLive(liveInfo, handler)
                            | _ => LiveSet.empty
                     val live = MemLocSet.unions [live,
                                                  LiveSet.toMemLocSet liveReturn,
                                                  LiveSet.toMemLocSet liveHandler]
                   in
                     (* flushing at far transfer *)
                     (farTransfer live
                      (if !Control.profile <> Control.ProfileNone
                         then (AppendList.fromList
                               [(* stackTopTemp = stackTop + bytes *)
                                amd64.Assembly.instruction_mov
                                {dst = stackTopTemp,
                                 src = stackTop,
                                 size = pointerSize},
                                amd64.Assembly.instruction_binal
                                {oper = amd64.Instruction.ADD,
                                 dst = stackTopTemp,
                                 src = bytes,
                                 size = pointerSize},
                                (* *(stackTopTemp - WORD_SIZE) = return *)
                                amd64.Assembly.instruction_lea
                                {dst = stackTopTempMinusWordDeref,
                                 src = Operand.memloc_label return,
                                 size = pointerSize},
                                amd64.Assembly.directive_force
                                {commit_memlocs = MemLocSet.singleton stackTopTempMinusWordDeref',
                                 commit_classes = ClassSet.empty,
                                 remove_memlocs = MemLocSet.empty,
                                 remove_classes = ClassSet.empty,
                                 dead_memlocs = MemLocSet.empty,
                                 dead_classes = ClassSet.empty},
                                (* stackTop = stackTopTemp *)
                                amd64.Assembly.instruction_mov
                                {dst = stackTop,
                                 src = stackTopTemp,
                                 size = pointerSize},
                                profileStackTopCommit'])
                         else (AppendList.fromList
                               [(* stackTop += bytes *)
                                amd64.Assembly.instruction_binal 
                                {oper = amd64.Instruction.ADD,
                                 dst = stackTop,
                                 src = bytes, 
                                 size = pointerSize},
                                (* *(stackTop - WORD_SIZE) = return *)
                                amd64.Assembly.instruction_lea
                                {dst = stackTopMinusWordDeref,
                                 src = Operand.memloc_label return,
                                 size = pointerSize},
                                amd64.Assembly.directive_force
                                {commit_memlocs = MemLocSet.singleton stackTopMinusWordDeref',
                                 commit_classes = ClassSet.empty,
                                 remove_memlocs = MemLocSet.empty,
                                 remove_classes = ClassSet.empty,
                                 dead_memlocs = MemLocSet.empty,
                                 dead_classes = ClassSet.empty}]))
                      (AppendList.single
                       (Assembly.instruction_jmp
                        {target = Operand.label target,
                         absolute = false})))
                   end
                | Return {live}
                => let
                     val stackTopMinusWordDeref
                       = amd64MLton.gcState_stackTopMinusWordDerefOperand ()
                   in
                     (* flushing at far transfer *)
                     (farTransfer live
                      AppendList.empty
                      (AppendList.single
                       (* jmp *(stackTop - WORD_SIZE) *)
                       (amd64.Assembly.instruction_jmp
                        {target = stackTopMinusWordDeref,
                         absolute = true})))
                   end
                | Raise {live}
                => let
                     val exnStack 
                       = amd64MLton.gcState_exnStackContentsOperand ()
                     val stackTopTemp
                       = amd64MLton.stackTopTempContentsOperand ()
                     val stackTop 
                       = amd64MLton.gcState_stackTopContentsOperand ()
                     val stackBottom 
                       = amd64MLton.gcState_stackBottomContentsOperand ()
                    in
                      (* flushing at far transfer *)
                      (farTransfer live
                       (if !Control.profile <> Control.ProfileNone
                          then (AppendList.fromList
                                [(* stackTopTemp = stackBottom + exnStack *)
                                 amd64.Assembly.instruction_mov
                                 {dst = stackTopTemp,
                                  src = stackBottom,
                                  size = pointerSize},
                                 amd64.Assembly.instruction_binal
                                 {oper = amd64.Instruction.ADD,
                                  dst = stackTopTemp,
                                  src = exnStack,
                                  size = pointerSize},
                                 (* stackTop = stackTopTemp *)
                                 amd64.Assembly.instruction_mov
                                 {dst = stackTop,
                                  src = stackTopTemp,
                                  size = pointerSize},
                                 profileStackTopCommit'])
                          else (AppendList.fromList
                                [(* stackTop = stackBottom + exnStack *)
                                 amd64.Assembly.instruction_mov
                                 {dst = stackTop,
                                  src = stackBottom,
                                  size = pointerSize},
                                 amd64.Assembly.instruction_binal
                                 {oper = amd64.Instruction.ADD,
                                  dst = stackTop,
                                  src = exnStack,
                                  size = pointerSize}]))
                       (AppendList.single
                        (* jmp *(stackTop - WORD_SIZE) *)
                        (amd64.Assembly.instruction_jmp
                         {target = amd64MLton.gcState_stackTopMinusWordDerefOperand (),
                          absolute = true})))
                    end
                | CCall {args, frameInfo, func, return}
                => let
                     datatype z = datatype CFunction.Convention.t
                     datatype z = datatype CFunction.SymbolScope.t
                     datatype z = datatype CFunction.Target.t
                     val CFunction.T {convention=_,
                                      return = returnTy,
                                      symbolScope,
                                      target, ...} = func
                     val stackTopMinusWordDeref
                       = amd64MLton.gcState_stackTopMinusWordDerefOperand ()
                     val Liveness.T {dead, ...}
                       = livenessTransfer {transfer = transfer,
                                           liveInfo = liveInfo}
                     val c_stackP = amd64MLton.c_stackPContentsOperand
                     val c_stackPDerefWord = amd64MLton.c_stackPDerefWordOperand
                     val c_stackPDerefFloat = amd64MLton.c_stackPDerefFloatOperand
                     val c_stackPDerefDouble = amd64MLton.c_stackPDerefDoubleOperand
                     val applyFFTempFun = amd64MLton.applyFFTempFunContentsOperand
                     val applyFFTempRegArg = amd64MLton.applyFFTempRegArgContents
                     val applyFFTempXmmRegArg = amd64MLton.applyFFTempXmmRegArgContents
                     val (fptrArg, args) =
                        case target of 
                           Direct _ => (AppendList.empty, args)
                         | Indirect => 
                              let
                                 val (fptrArg, args) =
                                    case args of
                                       fptrArg::args => (fptrArg, args)
                                     | _ => Error.bug "amd64GenerateTransfers.generateAll: CCall"
                              in
                                 (AppendList.single
                                  (Assembly.instruction_mov
                                   {src = #1 fptrArg,
                                    dst = applyFFTempFun,
                                    size = #2 fptrArg}),
                                  args)
                              end
                     val win64 = case !Control.Target.os of
                                    MLton.Platform.OS.Cygwin => true
                                  | MLton.Platform.OS.MinGW => true
                                  | _ => false
                     val (setup_args,
                          (reg_args, xmmreg_args),
                          size_stack_args, _)
                       = List.fold
                         (args, (AppendList.empty,
                                 ([],[]),0,
                                 (if win64
                                  then [Register.rcx,Register.rdx,
                                        Register.r8,Register.r9]
                                  else [Register.rdi,Register.rsi,Register.rdx,
                                        Register.rcx,Register.r8,Register.r9],
                                  if win64
                                  then [(XmmRegister.xmm0D,XmmRegister.xmm0S),
                                        (XmmRegister.xmm1D,XmmRegister.xmm1S),
                                        (XmmRegister.xmm2D,XmmRegister.xmm2S),
                                        (XmmRegister.xmm3D,XmmRegister.xmm3S)]
                                  else [(XmmRegister.xmm0D,XmmRegister.xmm0S),
                                        (XmmRegister.xmm1D,XmmRegister.xmm1S),
                                        (XmmRegister.xmm2D,XmmRegister.xmm2S),
                                        (XmmRegister.xmm3D,XmmRegister.xmm3S),
                                        (XmmRegister.xmm4D,XmmRegister.xmm4S),
                                        (XmmRegister.xmm5D,XmmRegister.xmm5S),
                                        (XmmRegister.xmm6D,XmmRegister.xmm6S),
                                        (XmmRegister.xmm7D,XmmRegister.xmm7S)])),
                          fn ((arg, size), 
                              (setup_args,
                               (reg_args, xmmreg_args),
                               size_stack_args,
                               (regs, xmmregs))) =>
                          let
                             fun prune [] = []
                               | prune (x::r) = if win64 then r else (x::r)
                             val (setup_arg,
                                  (reg_args, xmmreg_args),
                                  size_stack_arg,
                                  (regs, xmmregs)) =
                                if Size.eq (size, Size.DBLE)
                                   orelse Size.eq (size, Size.SNGL)
                                    then (case xmmregs of
                                             xmmreg::xmmregs => 
                                                let
                                                   val i = List.length xmmregs
                                                   val mem = applyFFTempXmmRegArg (size, i)
                                                   val xmmreg =
                                                      if Size.eq (size, Size.DBLE)
                                                         then #1 xmmreg
                                                      else #2 xmmreg
                                                in
                                                   (AppendList.fromList
                                                    [Assembly.instruction_sse_movs
                                                     {src = arg,
                                                      dst = Operand.memloc mem,
                                                      size = size},
                                                     Assembly.directive_xmmcache
                                                     {caches = [{register = xmmreg,
                                                                 memloc = mem,
                                                                 reserve = true}]}],
                                                    (reg_args,
                                                     (mem, xmmreg)::xmmreg_args),
                                                    0,
                                                    (prune regs, xmmregs))
                                                end
                                           | [] =>
                                                (AppendList.fromList
                                                 [Assembly.instruction_binal
                                                  {oper = Instruction.SUB,
                                                   dst = c_stackP,
                                                   src = Operand.immediate_int 8,
                                                   size = pointerSize},
                                                  Assembly.instruction_sse_movs
                                                  {src = arg,
                                                   dst = if Size.eq (size, Size.DBLE)
                                                            then c_stackPDerefDouble
                                                         else c_stackPDerefFloat,
                                                   size = size}],
                                                 (reg_args, xmmreg_args),
                                                 8,
                                                 (regs, xmmregs)))
                                else if Size.eq (size, Size.BYTE) 
                                        orelse Size.eq (size, Size.WORD)
                                        orelse Size.eq (size, Size.LONG)
                                        orelse Size.eq (size, Size.QUAD)
                                    then (case regs of
                                             reg::regs => 
                                                let
                                                   val i = List.length regs
                                                   val mem = applyFFTempRegArg i
                                                in
                                                   (AppendList.fromList
                                                    [if Size.lt (size, Size.QUAD)
                                                        then Assembly.instruction_movx
                                                             {oper = Instruction.MOVZX,
                                                              src = arg,
                                                              dst = Operand.memloc mem,
                                                              srcsize = size,
                                                              dstsize = wordSize}
                                                     else Assembly.instruction_mov
                                                          {src = arg,
                                                           dst = Operand.memloc mem,
                                                           size = size},
                                                     Assembly.directive_cache
                                                     {caches = [{register = reg,
                                                                 memloc = mem,
                                                                 reserve = true}]}],
                                                    ((mem,reg)::reg_args,
                                                     xmmreg_args),
                                                    0,
                                                    (regs, prune xmmregs))
                                                end
                                           | [] =>
                                                (AppendList.fromList
                                                 [Assembly.instruction_binal
                                                  {oper = Instruction.SUB,
                                                   dst = c_stackP,
                                                   src = Operand.immediate_int 8,
                                                   size = pointerSize},
                                                  if Size.lt (size, Size.QUAD)
                                                     then Assembly.instruction_movx
                                                          {oper = Instruction.MOVZX,
                                                           src = arg,
                                                           dst = c_stackPDerefWord,
                                                           srcsize = size,
                                                           dstsize = wordSize}
                                                  else Assembly.instruction_mov
                                                       {src = arg,
                                                        dst = c_stackPDerefWord,
                                                        size = size}],
                                                 (reg_args, xmmreg_args),
                                                 8,
                                                 (regs, xmmregs)))
                                else Error.bug "amd64GenerateTransfers.generateAll: CCall"
                          in
                             (AppendList.append (setup_arg, setup_args),
                              (reg_args, xmmreg_args),
                              size_stack_arg + size_stack_args,
                              (regs, xmmregs))
                          end)
                     val (setup_args, size_stack_args) =
                        let
                           val space = 16 - (size_stack_args mod 16)
                        in
                           if space = 16
                              then (setup_args, size_stack_args)
                              else (AppendList.append
                                    (AppendList.single
                                     (Assembly.instruction_binal
                                      {oper = Instruction.SUB,
                                       dst = c_stackP,
                                       src = Operand.immediate_int space,
                                       size = pointerSize}),
                                     setup_args),
                                    size_stack_args + space)
                        end
                     (* Allocate shadow space *)
                     val (setup_args, size_stack_args) =
                        if win64
                           then (AppendList.append
                                 (setup_args,
                                  AppendList.single
                                  (Assembly.instruction_binal
                                   {oper = Instruction.SUB,
                                    dst = c_stackP,
                                    src = Operand.immediate_int 32,
                                    size = pointerSize})),
                                 size_stack_args + 32)
                           else (setup_args, size_stack_args)
                     (* SysV ABI AMD64 requires %rax set to the number
                      * of xmms registers passed for varags functions;
                      * since %rax is caller-save, we conservatively 
                      * set %rax for all functions (not just varargs).
                      *)
                     val (reg_args, setup_args) =
                        if not win64
                           then let
                                   val mem = applyFFTempRegArg 8
                                   val reg = Register.rax
                                in
                                   ((mem,reg) :: reg_args,
                                    AppendList.append
                                    (setup_args,
                                     AppendList.fromList
                                     [Assembly.instruction_mov
                                      {src = Operand.immediate_int (List.length xmmreg_args),
                                       dst = Operand.memloc mem,
                                       size = Size.QUAD},
                                      Assembly.directive_cache
                                      {caches = [{register = reg,
                                                  memloc = mem,
                                                  reserve = true}]}]))
                                end
                        else (reg_args, setup_args)
                     (*
                     val reserve_args =
                        AppendList.fromList
                        [amd64.Assembly.directive_xmmcache
                         {caches = List.map
                                   (xmmreg_args, fn (mem,reg) =>
                                    {register = reg, memloc = mem, 
                                     reserve = true})},
                         amd64.Assembly.directive_cache
                         {caches = List.map
                                   (reg_args, fn (mem,reg) =>
                                    {register = reg, memloc = mem, 
                                     reserve = true})}]
                     *)
                     val flush =
                        case frameInfo of
                           SOME (FrameInfo.T {size, ...}) =>
                                (* Entering runtime *)
                                let
                                  val return = valOf return
                                  val _ = enque return

                                  val stackTopTemp
                                    = amd64MLton.stackTopTempContentsOperand ()
                                  val stackTopTempMinusWordDeref'
                                    = amd64MLton.stackTopTempMinusWordDeref ()
                                  val stackTopTempMinusWordDeref
                                    = amd64MLton.stackTopTempMinusWordDerefOperand ()
                                  val stackTop 
                                    = amd64MLton.gcState_stackTopContentsOperand ()
                                  val stackTopMinusWordDeref'
                                    = amd64MLton.gcState_stackTopMinusWordDeref ()
                                  val stackTopMinusWordDeref
                                    = amd64MLton.gcState_stackTopMinusWordDerefOperand ()
                                  val bytes = amd64.Operand.immediate_int size

                                  val live =
                                    amd64Liveness.LiveInfo.getLive(liveInfo, return)
                                  val {defs, ...} = Transfer.uses_defs_kills transfer
                                  val live = 
                                    List.fold
                                    (defs,
                                     live,
                                     fn (oper,live) =>
                                     case Operand.deMemloc oper of
                                       SOME memloc =>  LiveSet.remove (live, memloc)
                                     | NONE => live)
                                in
                                  (runtimeTransfer (LiveSet.toMemLocSet live)
                                   (if !Control.profile <> Control.ProfileNone
                                      then (AppendList.fromList
                                            [(* stackTopTemp = stackTop + bytes *)
                                             amd64.Assembly.instruction_mov
                                             {dst = stackTopTemp,
                                              src = stackTop,
                                              size = pointerSize},
                                             amd64.Assembly.instruction_binal
                                             {oper = amd64.Instruction.ADD,
                                              dst = stackTopTemp,
                                              src = bytes,
                                              size = pointerSize},
                                             (* *(stackTopTemp - WORD_SIZE) = return *)
                                             amd64.Assembly.instruction_lea
                                             {dst = stackTopTempMinusWordDeref,
                                              src = Operand.memloc_label return,
                                              size = pointerSize},
                                             amd64.Assembly.directive_force
                                             {commit_memlocs = MemLocSet.singleton stackTopTempMinusWordDeref',
                                              commit_classes = ClassSet.empty,
                                              remove_memlocs = MemLocSet.empty,
                                              remove_classes = ClassSet.empty,
                                              dead_memlocs = MemLocSet.empty,
                                              dead_classes = ClassSet.empty},
                                             (* stackTop = stackTopTemp *)
                                             amd64.Assembly.instruction_mov
                                             {dst = stackTop,
                                              src = stackTopTemp,
                                              size = pointerSize},
                                             profileStackTopCommit'])
                                      else (AppendList.fromList
                                            [(* stackTop += bytes *)
                                             amd64.Assembly.instruction_binal 
                                             {oper = amd64.Instruction.ADD,
                                              dst = stackTop,
                                              src = bytes, 
                                              size = pointerSize},
                                             (* *(stackTop - WORD_SIZE) = return *)
                                             amd64.Assembly.instruction_lea
                                             {dst = stackTopMinusWordDeref,
                                              src = Operand.memloc_label return,
                                              size = pointerSize},
                                             amd64.Assembly.directive_force
                                             {commit_memlocs = MemLocSet.singleton stackTopMinusWordDeref',
                                              commit_classes = ClassSet.empty,
                                              remove_memlocs = MemLocSet.empty,
                                              remove_classes = ClassSet.empty,
                                              dead_memlocs = MemLocSet.empty,
                                              dead_classes = ClassSet.empty}]))
                                   (AppendList.single
                                    (Assembly.directive_force
                                     {commit_memlocs = LiveSet.toMemLocSet live,
                                      commit_classes = runtimeClasses,
                                      remove_memlocs = MemLocSet.empty,
                                      remove_classes = ClassSet.empty,
                                      dead_memlocs = MemLocSet.empty,
                                      dead_classes = ClassSet.empty})))
                                end
                         | NONE => 
                                AppendList.single
                                (Assembly.directive_force
                                 {commit_memlocs = let
                                                     val s = MemLocSet.empty
                                                     val s = if CFunction.modifiesFrontier func
                                                               then MemLocSet.add
                                                                    (s, frontier ())
                                                               else s
                                                     val s = if CFunction.readsStackTop func
                                                               then MemLocSet.add
                                                                    (s, stackTop ())
                                                               else s
                                                   in
                                                     s
                                                   end,
                                  commit_classes = ccallflushClasses,
                                  remove_memlocs = MemLocSet.empty,
                                  remove_classes = ClassSet.empty,
                                  dead_memlocs = LiveSet.toMemLocSet dead,
                                  dead_classes = ClassSet.empty})
                     val call =
                        case target of
                           Direct name =>
                              let
                                 datatype z = datatype MLton.Platform.OS.t
                                 datatype z = datatype Control.Format.t
                                 
                                 val label = fn () => Label.fromString name
                                 
                                 (* how to access imported functions: *)
                                 (* Windows rewrites the symbol __imp__name *)
                                 val coff = fn () => Label.fromString ("_imp__" ^ name)
                                 val macho = fn () => label () (* @PLT is implicit *)
                                 val elf = fn () => Label.fromString (name ^ "@PLT")
                                 
                                 val importLabel = fn () =>
                                    case !Control.Target.os of
                                       Cygwin => coff ()
                                     | Darwin => macho ()
                                     | MinGW => coff ()
                                     |  _ => elf ()
                                 
                                 val direct = fn () =>
                                   AppendList.fromList
                                   [Assembly.directive_ccall (),
                                    Assembly.instruction_call
                                    {target = Operand.label (label ()),
                                     absolute = false}]
                                     
                                 val plt = fn () =>
                                   AppendList.fromList
                                   [Assembly.directive_ccall (),
                                    Assembly.instruction_call
                                    {target = Operand.label (importLabel ()),
                                     absolute = false}]
                                
                                 val indirect = fn () =>
                                   AppendList.fromList
                                   [Assembly.directive_ccall (),
                                    Assembly.instruction_call
                                    {target = Operand.memloc_label (importLabel ()),
                                     absolute = true}]
                              in
                                case (symbolScope,
                                      !Control.Target.os, 
                                      !Control.positionIndependent) of
                                   (* Private functions can be easily reached
                                    * with a direct (rip-relative) call.
                                    *)
                                   (Private, _, _) => direct ()
                                   (* Call at the point of definition. *)
                                 | (Public, MinGW, _) => direct ()
                                 | (Public, Cygwin, _) => direct ()
                                 | (Public, Darwin, _) => direct ()
                                   (* ELF requires PLT even for public fns. *)
                                 | (Public, _, true) => plt ()
                                 | (Public, _, false) => direct ()
                                   (* Windows always does indirect calls to
                                    * imported functions. The importLabel has
                                    * the function address written to it.
                                    *)
                                 | (External, MinGW, _) => indirect ()
                                 | (External, Cygwin, _) => indirect ()
                                 | (External, Darwin, _) => plt ()
                                   (* ELF systems (and darwin too) create
                                    * procedure lookup tables (PLT) which 
                                    * proxy the call to libraries. The PLT
                                    * does not contain an address, but instead
                                    * a stub function. Often the PLT is auto-
                                    * matically created. This applies to all
                                    * darwin-x86_64 function calls and calls
                                    * made from an ELF executable.
                                    *)
                                 | (External, _, true) => plt ()
                                 | (External, _, false) => direct ()
                              end
                         | Indirect =>
                              AppendList.fromList
                              [Assembly.directive_ccall (),
                               Assembly.instruction_call
                               {target = applyFFTempFun,
                                absolute = true}]
                     val unreserve_args =
                        AppendList.fromList
                        [amd64.Assembly.directive_xmmunreserve
                         {registers = List.map (xmmreg_args, #2)},
                         amd64.Assembly.directive_unreserve
                         {registers = List.map (reg_args, #2)}]
                     val kill
                       = if isSome frameInfo
                           then AppendList.single
                                (Assembly.directive_force
                                 {commit_memlocs = MemLocSet.empty,
                                  commit_classes = ClassSet.empty,
                                  remove_memlocs = MemLocSet.empty,
                                  remove_classes = ClassSet.empty,
                                  dead_memlocs = MemLocSet.empty,
                                  dead_classes = runtimeClasses})
                           else AppendList.single
                                (Assembly.directive_force
                                 {commit_memlocs = MemLocSet.empty,
                                  commit_classes = ClassSet.empty,
                                  remove_memlocs = MemLocSet.empty,
                                  remove_classes = ClassSet.empty,
                                  dead_memlocs = let
                                                   val s = MemLocSet.empty
                                                   val s = if CFunction.modifiesFrontier func
                                                             then MemLocSet.add
                                                                  (s, frontier ())
                                                             else s
                                                   val s = if CFunction.writesStackTop func
                                                             then MemLocSet.add
                                                                  (s, stackTop ())
                                                             else s
                                                 in
                                                   s
                                                 end,
                                  dead_classes = ccallflushClasses})
                     val getResult =
                        AppendList.single
                        (Assembly.directive_return
                         {returns = Operand.cReturnTemps returnTy})
                     val fixCStack =
                        if size_stack_args > 0
                           then (AppendList.single
                                 (Assembly.instruction_binal
                                  {oper = Instruction.ADD,
                                   dst = c_stackP,
                                   src = Operand.immediate_int size_stack_args,
                                   size = pointerSize}))
                           else AppendList.empty
                     val continue
                       = if CFunction.maySwitchThreads func
                           then (* Returning from runtime *)
                                (farTransfer MemLocSet.empty
                                 AppendList.empty
                                 (AppendList.single
                                  (* jmp *(stackTop - WORD_SIZE) *)
                                  (amd64.Assembly.instruction_jmp
                                   {target = stackTopMinusWordDeref,
                                    absolute = true})))
                         else case return
                                of NONE => AppendList.empty
                                 | SOME l => (if isSome frameInfo
                                                then (* Don't need to trampoline,
                                                      * since didn't switch threads,
                                                      * but can't fall because
                                                      * frame layout data is prefixed
                                                      * to l's code; use fallNone
                                                      * to force a jmp with near
                                                      * jump assumptions.
                                                      *)
                                                     fallNone
                                                else fall)
                                             gef 
                                             {label = l,
                                              live = getLive (liveInfo, l)}
                   in
                     AppendList.appends
                     [cacheRsp (),
                      fptrArg,
                      setup_args,
                      (*reserve_args,*)
                      flush,
                      call,
                      unreserve_args,
                      kill,
                      getResult,
                      fixCStack,
                      unreserveRsp (),
                      continue]
                   end)

        and effectJumpTable (gef as GEF {...})
                             {label, transfer} : Assembly.t AppendList.t
          = case transfer
              of Switch {test, cases, default}
               => let
                     val ws =
                        case Operand.size test of
                           SOME Size.BYTE => WordSize.word8
                         | SOME Size.WORD => WordSize.word16
                         | SOME Size.LONG => WordSize.word32
                         | SOME Size.QUAD => WordSize.word64
                         | _ => Error.bug "amd64GenerateTransfers.effectJumpTable: Switch"

                     val zero = WordX.zero ws
                     val one = WordX.one ws
                     val two = WordX.add (one, one)
                     fun even w = WordX.isZero (WordX.mod (w, two, {signed = false}))
                     fun incFn w = WordX.add (w, one)
                     fun decFn w = WordX.sub (w, one)
                     fun halfFn w = WordX.div (w, two, {signed = false})
                     fun ltFn (w1, w2) = WordX.lt (w1, w2, {signed = false})
                     val min = WordX.min (ws, {signed = false})
                     fun minFn (w1, w2) = if WordX.lt (w1, w2, {signed = false}) 
                                             then w1
                                          else w2
                     val max = WordX.max (ws, {signed = false})
                     fun maxFn (w1, w2) = if WordX.gt (w1, w2, {signed = false}) 
                                             then w1
                                          else w2
                     fun range (w1, w2) = WordX.sub (w2, w1)

                    val Liveness.T {dead, ...}
                      = livenessTransfer {transfer = transfer,
                                          liveInfo = liveInfo}

                    fun reduce(cases)
                      = let
                          fun reduce' cases
                            = let
                                val (minK,maxK,length,
                                     allEven,allOdd)
                                  = List.fold
                                    (cases,
                                     (max, min, 0,
                                      true, true),
                                     fn ((k,_),
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
                                           = WordX.orb
                                             (WordX.lshift(mask'', WordX.one WordSize.word64),
                                              if allOdd
                                                then WordX.one WordSize.word64
                                                else WordX.zero WordSize.word64)
                                       in
                                         (cases'', 
                                          minK'', maxK'', length'',
                                          shift', mask')
                                       end
                                  else (cases, 
                                        minK, maxK, length,
                                        0, WordX.zero WordSize.word64)
                              end
                        in 
                          reduce' cases
                        end

                    fun doitTable(cases,
                                  minK, _, rangeK, shift, mask)
                      = let
                          val jump_table_label
                            = Label.newString "jumpTable"

                          val idT = Directive.Id.new ()
                          val defaultT = 
                             Promise.delay
                             (fn () =>
                              let
                                 val _ = incNear(jumpInfo, default)
                              in 
                                 pushCompensationBlock
                                 {label = default,
                                  id = idT}
                              end)

                          val rec filler 
                            = fn ([],_) => []
                               | (cases as (i,target)::cases',j)
                               => if WordX.equals (i, j)
                                    then let
                                           val target'
                                             = pushCompensationBlock
                                               {label = target,
                                                id = idT}
                                         in 
                                           (Immediate.label target')::
                                           (filler(cases', incFn j))
                                         end 
                                    else (Immediate.label 
                                          (Promise.force defaultT))::
                                         (filler(cases, incFn j))

                          val jump_table = filler (cases, minK)

                          val idD = Directive.Id.new ()
                          val defaultD = pushCompensationBlock
                                         {label = default,
                                          id = idD}

                          val default_live = getLive(liveInfo, default)
                          val live
                            = List.fold
                              (cases,
                               default_live,
                               fn ((_,target), live)
                                => LiveSet.+(live, getLive(liveInfo, target)))

                          val indexTemp
                            = MemLoc.imm 
                              {base = Immediate.label (Label.fromString "indexTemp"),
                               index = Immediate.zero,
                               scale = Scale.Eight,
                               size = Size.QUAD,
                               class = MemLoc.Class.Temp}
                          val checkTemp
                            = MemLoc.imm 
                              {base = Immediate.label (Label.fromString "checkTemp"),
                               index = Immediate.zero,
                               scale = Scale.Eight,
                               size = Size.QUAD,
                               class = MemLoc.Class.Temp}
                          val address
                            = MemLoc.basic
                              {base = Immediate.label jump_table_label,
                               index = indexTemp,
                               scale = Scale.Eight,
                               size = Size.QUAD,
                               class = MemLoc.Class.Code}

                          val size 
                            = case Operand.size test
                                of SOME size => size
                                 | NONE => Size.QUAD
                          val indexTemp' = indexTemp
                          val indexTemp = Operand.memloc indexTemp
                          val checkTemp' = checkTemp
                          val checkTemp = Operand.memloc checkTemp
                          val address = Operand.memloc address
                        in
                          AppendList.appends
                          [if Size.lt(size, Size.QUAD)
                             then AppendList.single
                                  (Assembly.instruction_movx
                                   {oper = Instruction.MOVZX,
                                    src = test,
                                    srcsize = size,
                                    dst = indexTemp,
                                    dstsize = Size.QUAD})
                             else AppendList.single
                                  (Assembly.instruction_mov
                                   {src = test,
                                    dst = indexTemp,
                                    size = Size.QUAD}),
                           if LiveSet.isEmpty dead
                             then AppendList.empty
                             else AppendList.single
                                  (Assembly.directive_force
                                   {commit_memlocs = MemLocSet.empty,
                                    commit_classes = ClassSet.empty,
                                    remove_memlocs = MemLocSet.empty,
                                    remove_classes = ClassSet.empty,
                                    dead_memlocs = LiveSet.toMemLocSet dead,
                                    dead_classes = ClassSet.empty}),
                           if shift > 0
                             then let
                                    val idC = Directive.Id.new ()
                                    val defaultC = pushCompensationBlock
                                                   {label = default,
                                                    id = idC}
                                    val _ = incNear(jumpInfo, default)
                                  in
                                    AppendList.appends
                                    [AppendList.fromList
                                     [Assembly.instruction_mov
                                      {src = indexTemp,
                                       dst = checkTemp,
                                       size = Size.QUAD},
                                      Assembly.instruction_binal
                                      {oper = Instruction.AND,
                                       src = Operand.immediate_word 
                                             (ones (shift, WordSize.word64)),
                                       dst = checkTemp,
                                       size = Size.QUAD}],
                                     if WordX.isZero mask
                                       then AppendList.empty
                                       else AppendList.single
                                            (Assembly.instruction_binal
                                             {oper = Instruction.SUB,
                                              src = Operand.immediate_word mask,
                                              dst = checkTemp,
                                              size = Size.QUAD}),
                                     AppendList.fromList
                                     [Assembly.directive_force
                                      {commit_memlocs = MemLocSet.empty,
                                       commit_classes = nearflushClasses,
                                       remove_memlocs = MemLocSet.empty,
                                       remove_classes = ClassSet.empty,
                                       dead_memlocs = MemLocSet.singleton checkTemp',
                                       dead_classes = ClassSet.empty},
                                      Assembly.instruction_jcc
                                      {condition = Instruction.NZ,
                                       target = Operand.label defaultC},
                                      Assembly.directive_saveregalloc
                                      {id = idC,
                                       live = MemLocSet.add
                                              (MemLocSet.add
                                               (LiveSet.toMemLocSet default_live,
                                                stackTop ()),
                                               frontier ())},
                                      Assembly.instruction_sral
                                      {oper = Instruction.SAR,
                                       count = Operand.immediate_int shift,
                                       dst = indexTemp,
                                       size = Size.QUAD}]]
                                  end
                             else AppendList.empty,
                           if WordX.equals (minK, zero)
                             then AppendList.empty
                             else AppendList.single
                                  (Assembly.instruction_binal
                                   {oper = Instruction.SUB,
                                    src = Operand.immediate_word minK,
                                    dst = indexTemp,
                                    size = Size.QUAD}),
                          AppendList.fromList
                          [Assembly.directive_force
                           {commit_memlocs = MemLocSet.empty,
                            commit_classes = nearflushClasses,
                            remove_memlocs = MemLocSet.empty,
                            remove_classes = ClassSet.empty,
                            dead_memlocs = MemLocSet.empty,
                            dead_classes = ClassSet.empty},
                           Assembly.directive_cache
                           {caches = [{register = indexReg,
                                       memloc = indexTemp',
                                       reserve = false}]},
                           Assembly.instruction_cmp
                           {src1 = indexTemp,
                            src2 = Operand.immediate_word rangeK,
                            size = Size.QUAD},
                           Assembly.instruction_jcc
                           {condition = Instruction.A,
                            target = Operand.label defaultD},
                           Assembly.directive_saveregalloc
                           {id = idD,
                            live = MemLocSet.add
                                   (MemLocSet.add
                                    (LiveSet.toMemLocSet default_live,
                                     stackTop ()),
                                    frontier ())},
                           Assembly.instruction_jmp
                           {target = address,
                            absolute = true},
                           Assembly.directive_saveregalloc
                           {id = idT,
                            live = MemLocSet.add
                                   (MemLocSet.add
                                    (LiveSet.toMemLocSet live,
                                     stackTop ()),
                                    frontier ())},
                           Assembly.directive_force
                           {commit_memlocs = MemLocSet.empty,
                            commit_classes = ClassSet.empty,
                            remove_memlocs = MemLocSet.empty,
                            remove_classes = ClassSet.empty,
                            dead_memlocs = MemLocSet.singleton indexTemp',
                            dead_classes = ClassSet.empty}],
                          AppendList.fromList
                          [Assembly.pseudoop_data (),
                           Assembly.pseudoop_p2align 
                           (Immediate.int 4, NONE, NONE),
                           Assembly.label jump_table_label,
                           Assembly.pseudoop_quad jump_table,
                           Assembly.pseudoop_text ()]]
                        end

                    fun doit(cases)
                      = let
                          val (cases, 
                               minK, maxK, length,
                               shift, mask) 
                            = reduce(cases)

                          val rangeK 
                            = range(minK,maxK)
                        in
                          if length >= 8 
                             andalso
                             WordX.lt (WordX.div(rangeK,two,{signed=false}),
                                       WordX.fromIntInf (IntInf.fromInt length, ws),
                                       {signed = false})
                            then let
                                   val cases 
                                     = List.insertionSort
                                       (cases, 
                                        fn ((k,_),(k',_)) 
                                         => ltFn(k,k'))
                                 in 
                                   doitTable(cases, 
                                             minK, maxK, rangeK,
                                             shift, mask)
                                 end
                            else effectDefault gef
                                               {label = label, 
                                                transfer = transfer}
                        end
                  in
                    case cases
                      of Transfer.Cases.Word cases
                       => doit cases
                  end
               | _ => effectDefault gef 
                                    {label = label,
                                     transfer = transfer}

        and fallNone (GEF {...})
                     {label, live} : Assembly.t AppendList.t
          = let
              val liveRegsTransfer = getLiveRegsTransfers
                                     (liveTransfers, label)
              val liveXmmRegsTransfer = getLiveXmmRegsTransfers
                                         (liveTransfers, label)

              val live
                = List.fold
                  (liveRegsTransfer,
                   live,
                   fn ((memloc,_,_),live)
                    => LiveSet.remove(live,memloc))
              val live
                = List.fold
                  (liveXmmRegsTransfer,
                   live,
                   fn ((memloc,_,_),live)
                    => LiveSet.remove(live,memloc))

              fun default ()
                = AppendList.fromList
                  ((* flushing at near transfer *)
                   (Assembly.directive_cache
                    {caches = [{register = stackTopReg,
                                memloc = stackTop (),
                                reserve = true},
                               {register = frontierReg,
                                memloc = frontier (),
                                reserve = true}]})::
                   (Assembly.directive_xmmcache
                    {caches
                     = List.map
                       (liveXmmRegsTransfer,
                        fn (temp,register,_)
                         => {register = register,
                             memloc = temp,
                             reserve = true})})::
                   (Assembly.directive_cache
                    {caches
                     = List.map
                       (liveRegsTransfer,
                        fn (temp,register,_)
                         => {register = register,
                             memloc = temp,
                             reserve = true})})::
                   (Assembly.directive_force
                    {commit_memlocs = LiveSet.toMemLocSet live,
                     commit_classes = nearflushClasses,
                     remove_memlocs = MemLocSet.empty,
                     remove_classes = ClassSet.empty,
                     dead_memlocs = MemLocSet.empty,
                     dead_classes = ClassSet.empty})::
                   (Assembly.instruction_jmp
                    {target = Operand.label label,
                     absolute = false})::
                   (Assembly.directive_unreserve
                    {registers
                     = (stackTopReg)::
                       (frontierReg)::
                       (List.map
                        (liveRegsTransfer,
                         fn (_,register,_)
                          => register))})::
                   (Assembly.directive_xmmunreserve
                    {registers
                     = (List.map
                        (liveXmmRegsTransfer,
                         fn (_,register,_)
                          => register))})::
                   nil)
            in
              case getLayoutInfo label
                of NONE
                 => default ()
                 | SOME (Block.T {...})
                 => (push label;
                     default ())
            end

        and fallDefault (gef as GEF {generate,...})
                        {label, live} : Assembly.t AppendList.t
          = let 
              datatype z = datatype amd64JumpInfo.status
              val liveRegsTransfer = getLiveRegsTransfers
                                     (liveTransfers, label)
              val liveXmmRegsTransfer = getLiveXmmRegsTransfers
                                         (liveTransfers, label)

              val live
                = List.fold
                  (liveRegsTransfer,
                   live,
                   fn ((memloc,_,_),live)
                    => LiveSet.remove(live,memloc))
              val live
                = List.fold
                  (liveXmmRegsTransfer,
                   live,
                   fn ((memloc,_,_),live)
                    => LiveSet.remove(live,memloc))

              fun default jmp
                = AppendList.appends
                  [AppendList.fromList
                   [(* flushing at near transfer *)
                    (Assembly.directive_cache
                     {caches = [{register = stackTopReg,
                                 memloc = stackTop (),
                                 reserve = true},
                                {register = frontierReg,
                                 memloc = frontier (),
                                 reserve = true}]}),
                    (Assembly.directive_cache
                     {caches
                      = List.map
                        (liveRegsTransfer,
                         fn (temp,register,_)
                          => {register = register,
                              memloc = temp,
                              reserve = true})}),
                    (Assembly.directive_xmmcache
                     {caches
                      = List.map
                        (liveXmmRegsTransfer,
                         fn (temp,register,_)
                          => {register = register,
                              memloc = temp,
                              reserve = true})}),
                    (Assembly.directive_force
                     {commit_memlocs = LiveSet.toMemLocSet live,
                      commit_classes = nearflushClasses,
                      remove_memlocs = MemLocSet.empty,
                      remove_classes = ClassSet.empty,
                      dead_memlocs = MemLocSet.empty,
                      dead_classes = ClassSet.empty})],
                   if jmp
                     then AppendList.single
                          (Assembly.instruction_jmp
                           {target = Operand.label label,
                            absolute = false})
                     else AppendList.empty,
                   AppendList.fromList
                   [(Assembly.directive_unreserve
                     {registers
                      = (stackTopReg)::
                        (frontierReg)::
                        (List.map
                         (liveRegsTransfer,
                          fn (_,register,_)
                           => register))}),
                    (Assembly.directive_xmmunreserve
                     {registers
                      = (List.map
                         (liveXmmRegsTransfer,
                          fn (_,register,_)
                           => register))})]]
            in
              case getLayoutInfo label
                of NONE 
                 => default true
                 | SOME (Block.T {...})
                 => (case getNear(jumpInfo, label)
                       of Count 1 
                        => generate gef
                                    {label = label,
                                     falling = true,
                                     unique = true}
                        | _ => AppendList.append
                               (default false,
                                AppendList.cons
                                (Assembly.directive_reset (),
                                 (generate gef
                                           {label = label,
                                            falling = true,
                                            unique = false}))))
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

        val _ = List.foreach
                (blocks,
                 fn Block.T {entry, ...}
                  => (case entry
                        of Func {label, ...} => enque label
                         | _ => ()))
        fun doit () : Assembly.t list list
          = (case deque ()
               of NONE => []
                | SOME label
                => (case AppendList.toList (generate {label = label,
                                                      falling = false,
                                                      unique = false})
                      of [] => doit ()
                       | block => block::(doit ())))
        val assembly = doit ()
        val _ = destLayoutInfo ()
        val _ = destProfileLabel ()

        val assembly = [Assembly.pseudoop_text ()]::assembly
        val assembly =
           if List.isEmpty data
              then assembly
              else data::assembly
      in
         assembly
      end

  val (generateTransfers, generateTransfers_msg)
    = tracerTop
      "generateTransfers"
      generateTransfers

  fun generateTransfers_totals ()
    = (generateTransfers_msg ();
       Control.indent ();
       amd64Liveness.LiveInfo.verifyLiveInfo_msg ();
       amd64JumpInfo.verifyJumpInfo_msg ();
       amd64EntryTransfer.verifyEntryTransfer_msg ();
       amd64LoopInfo.createLoopInfo_msg ();
       amd64LiveTransfers.computeLiveTransfers_totals ();
       Control.unindent ())
end
