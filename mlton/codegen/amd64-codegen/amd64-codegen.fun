(* Copyright (C) 2009-2010,2014 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor amd64Codegen (S: AMD64_CODEGEN_STRUCTS): AMD64_CODEGEN =
struct
  open S

  structure amd64 = amd64 (open Machine
                       structure RepType = Type)
  structure amd64Pseudo = amd64PseudoCheck (structure S = amd64)

  structure amd64MLtonBasic
    = amd64MLtonBasic (structure amd64 = amd64Pseudo
                     structure Machine = Machine)

  structure amd64Liveness
    = amd64Liveness (structure amd64 = amd64
                   structure amd64MLtonBasic = amd64MLtonBasic)

  structure amd64JumpInfo
    = amd64JumpInfo (structure amd64 = amd64)

  structure amd64LoopInfo
    = amd64LoopInfo (structure amd64 = amd64)

  structure amd64EntryTransfer
    = amd64EntryTransfer (structure amd64 = amd64)

  structure amd64MLton 
    = amd64MLton (structure amd64MLtonBasic = amd64MLtonBasic
                structure amd64Liveness = amd64Liveness)

  val implementsPrim = amd64MLton.implementsPrim

  structure amd64Translate 
    = amd64Translate (structure amd64 = amd64
                    structure amd64MLton = amd64MLton
                    structure amd64Liveness = amd64Liveness)

  structure amd64Simplify
    = amd64Simplify (structure amd64 = amd64
                   structure amd64Liveness = amd64Liveness
                   structure amd64JumpInfo = amd64JumpInfo
                   structure amd64EntryTransfer = amd64EntryTransfer)

  structure amd64GenerateTransfers
    = amd64GenerateTransfers (structure amd64 = amd64
                            structure amd64MLton = amd64MLton
                            structure amd64Liveness = amd64Liveness
                            structure amd64JumpInfo = amd64JumpInfo
                            structure amd64LoopInfo = amd64LoopInfo
                            structure amd64EntryTransfer = amd64EntryTransfer)

  structure amd64AllocateRegisters
    = amd64AllocateRegisters (structure amd64 = amd64
                            structure amd64MLton = amd64MLton)

  open amd64
  fun output {program as Machine.Program.T {chunks, frameLayouts, handlesSignals,
                                            main, ...},
              outputC: unit -> {file: File.t,
                                print: string -> unit,
                                done: unit -> unit},
              outputS: unit -> {file: File.t,
                                print: string -> unit,
                                done: unit -> unit}}: unit
    = let
         val reserveRsp =
            (* There is no sigaltstack on cygwin, we need to reserve %rsp to
             * hold the C stack pointer.  We only need to do this in programs
             * that handle signals.
             *)
            handlesSignals andalso let open Control.Target in !os = Cygwin end

        val makeC = outputC
        val makeS = outputS

        val Machine.Program.T {profileInfo, ...} = program
        val profileInfo =
           case profileInfo of
              NONE => Machine.ProfileInfo.empty
            | SOME pi => pi
        val {newProfileLabel, delProfileLabel, getProfileInfo} = 
          Machine.ProfileInfo.modify profileInfo

        (* C specific *)
        fun outputC ()
          = let
              local
                val Machine.Program.T 
                    {chunks, 
                     frameLayouts, 
                     frameOffsets, 
                     handlesSignals, 
                     main, 
                     maxFrameSize, 
                     objectTypes, 
                     reals, 
                     vectors, ...} =
                  program
              in
                val program =
                  Machine.Program.T 
                  {chunks = chunks, 
                   frameLayouts = frameLayouts, 
                   frameOffsets = frameOffsets, 
                   handlesSignals = handlesSignals, 
                   main = main, 
                   maxFrameSize = maxFrameSize, 
                   objectTypes = objectTypes, 
                   profileInfo = SOME (getProfileInfo ()),
                   reals = reals, 
                   vectors = vectors}
              end
              val {print, done, ...} = makeC ()
              val additionalMainArgs =
                 let
                    val mainLabel = Label.toString (#label main)
                    (* Drop the leading _, because gcc will add it. *)
                    val mainLabel =
                       if !Control.labelsHaveExtra_
                          then String.dropPrefix (mainLabel, 1)
                       else mainLabel
                 in
                    [mainLabel]
                 end
              fun declareLocals () =
                 List.foreach
                 (CType.all,
                  fn t =>
                  let
                     val m =
                        List.fold
                        (chunks, ~1, fn (Machine.Chunk.T {regMax, ...}, max) =>
                         Int.max (max, regMax t))
                     val m = m + 1
                  in
                     print (concat ["PRIVATE ", CType.toString t, 
                                    " local", CType.toString t,
                                    "[", Int.toString m, "];\n"])
                  end)
              fun rest () =
                 declareLocals ()
            in
              CCodegen.outputDeclarations
              {additionalMainArgs = additionalMainArgs,
               includes = ["amd64-main.h"],
               print = print,
               program = program,
               rest = rest}
              ; done ()
            end 

        val outputC = Control.trace (Control.Pass, "outputC") outputC

        (* Assembly specific *)

        val _ = amd64MLtonBasic.init ()

        fun outputJumpToSML print =
           let
              val win64 = case !Control.Target.os of
                             MLton.Platform.OS.Cygwin => true
                           | MLton.Platform.OS.MinGW => true
                           | _ => false
              val jumpToSML = amd64.Label.fromString "MLton_jumpToSML"
              val returnToC = amd64.Label.fromString "Thread_returnToC"
              val {frontierReg, stackTopReg} =
                 {frontierReg = amd64.Register.r12,
                  stackTopReg = amd64.Register.rbp}
              val asm =
                 [
                  amd64.Assembly.pseudoop_text (),
                  amd64.Assembly.pseudoop_p2align 
                  (amd64.Immediate.int 4, NONE, NONE),
                  amd64.Assembly.pseudoop_global jumpToSML,
                  amd64.Assembly.pseudoop_hidden jumpToSML,
                  amd64.Assembly.label jumpToSML,
                  amd64.Assembly.instruction_binal
                  {oper = amd64.Instruction.SUB,
                   src = amd64.Operand.immediate_int 72,
                   dst = amd64.Operand.register amd64.Register.rsp,
                   size = amd64.Size.QUAD},
                  amd64.Assembly.instruction_mov
                  {src = amd64.Operand.register amd64.Register.rbp,
                   dst = (amd64.Operand.address o amd64.Address.T)
                         {disp = SOME (amd64.Immediate.int 64),
                          base = SOME amd64.Register.rsp,
                          index= NONE, scale = NONE},
                   size = amd64.Size.QUAD},
                  amd64.Assembly.instruction_mov
                  {src = amd64.Operand.register amd64.Register.rbx,
                   dst = (amd64.Operand.address o amd64.Address.T)
                         {disp = SOME (amd64.Immediate.int 56),
                          base = SOME amd64.Register.rsp,
                          index= NONE, scale = NONE},
                   size = amd64.Size.QUAD},
                  amd64.Assembly.instruction_mov
                  {src = amd64.Operand.register amd64.Register.r12,
                   dst = (amd64.Operand.address o amd64.Address.T)
                         {disp = SOME (amd64.Immediate.int 48),
                          base = SOME amd64.Register.rsp,
                          index= NONE, scale = NONE},
                   size = amd64.Size.QUAD},
                  amd64.Assembly.instruction_mov
                  {src = amd64.Operand.register amd64.Register.r13,
                   dst = (amd64.Operand.address o amd64.Address.T)
                         {disp = SOME (amd64.Immediate.int 40),
                          base = SOME amd64.Register.rsp,
                          index= NONE, scale = NONE},
                   size = amd64.Size.QUAD},
                  amd64.Assembly.instruction_mov
                  {src = amd64.Operand.register amd64.Register.r14,
                   dst = (amd64.Operand.address o amd64.Address.T)
                         {disp = SOME (amd64.Immediate.int 32),
                          base = SOME amd64.Register.rsp,
                          index= NONE, scale = NONE},
                   size = amd64.Size.QUAD},
                  amd64.Assembly.instruction_mov
                  {src = amd64.Operand.register amd64.Register.r15,
                   dst = (amd64.Operand.address o amd64.Address.T)
                         {disp = SOME (amd64.Immediate.int 24),
                          base = SOME amd64.Register.rsp,
                          index= NONE, scale = NONE},
                   size = amd64.Size.QUAD},
                  amd64.Assembly.instruction_mov
                  {src = (amd64.Operand.address o amd64.Address.T)
                         {disp = SOME (amd64.Immediate.label amd64MLton.c_stackP),
                          base = SOME amd64.Register.rip, index = NONE, scale = NONE},
                   dst = amd64.Operand.register amd64.Register.rbx,
                   size = amd64.Size.QUAD},
                  amd64.Assembly.instruction_mov
                  {src = amd64.Operand.register amd64.Register.rbx,
                   dst = (amd64.Operand.address o amd64.Address.T)
                         {disp = SOME (amd64.Immediate.int 16),
                          base = SOME amd64.Register.rsp,
                          index = NONE, scale = NONE},
                   size = amd64.Size.QUAD},
                  amd64.Assembly.instruction_mov
                  {src = amd64.Operand.register amd64.Register.rsp,
                   dst = (amd64.Operand.address o amd64.Address.T)
                         {disp = SOME (amd64.Immediate.label amd64MLton.c_stackP),
                          base = SOME amd64.Register.rip, index = NONE, scale = NONE},
                   size = amd64.Size.QUAD},
                  amd64.Assembly.instruction_mov
                  {src = (amd64.Operand.address o amd64.Address.T)
                         {disp = (SOME o amd64.Immediate.labelPlusInt)
                                 (amd64MLton.gcState_label,
                                  Bytes.toInt 
                                  (Machine.Runtime.GCField.offset
                                   Machine.Runtime.GCField.StackTop)),
                          base = SOME amd64.Register.rip, index = NONE, scale = NONE},
                   dst = amd64.Operand.register stackTopReg,
                   size = amd64.Size.QUAD},
                  amd64.Assembly.instruction_mov
                  {src = (amd64.Operand.address o amd64.Address.T)
                         {disp = (SOME o amd64.Immediate.labelPlusInt)
                                 (amd64MLton.gcState_label,
                                  Bytes.toInt 
                                  (Machine.Runtime.GCField.offset
                                   Machine.Runtime.GCField.Frontier)),
                          base = SOME amd64.Register.rip, index = NONE, scale = NONE},
                   dst = amd64.Operand.register frontierReg,
                   size = amd64.Size.QUAD},
                  amd64.Assembly.instruction_jmp
                  {target = amd64.Operand.register 
                            (if win64 then amd64.Register.rcx 
                                      else amd64.Register.rdi),
                   absolute = true},
                  amd64.Assembly.pseudoop_p2align 
                  (amd64.Immediate.int 4, NONE, NONE),
                  amd64.Assembly.pseudoop_global returnToC,
                  amd64.Assembly.pseudoop_hidden returnToC,
                  amd64.Assembly.label returnToC,
                  amd64.Assembly.instruction_mov
                  {src = (amd64.Operand.address o amd64.Address.T)
                         {disp = SOME (amd64.Immediate.label amd64MLton.c_stackP),
                          base = SOME amd64.Register.rip, index = NONE, scale = NONE},
                   dst = amd64.Operand.register amd64.Register.rsp,
                   size = amd64.Size.QUAD},
                  amd64.Assembly.instruction_mov
                  {src = (amd64.Operand.address o amd64.Address.T)
                         {disp = SOME (amd64.Immediate.int 16),
                          base = SOME amd64.Register.rsp,
                          index = NONE, scale = NONE},
                   dst = amd64.Operand.register amd64.Register.rbx,
                   size = amd64.Size.QUAD},
                  amd64.Assembly.instruction_mov
                  {src = amd64.Operand.register amd64.Register.rbx,
                   dst = (amd64.Operand.address o amd64.Address.T)
                         {disp = SOME (amd64.Immediate.label amd64MLton.c_stackP),
                          base = SOME amd64.Register.rip, index = NONE, scale = NONE},
                   size = amd64.Size.QUAD},
                  amd64.Assembly.instruction_mov
                  {src = (amd64.Operand.address o amd64.Address.T)
                         {disp = SOME (amd64.Immediate.int 24),
                          base = SOME amd64.Register.rsp,
                          index = NONE, scale = NONE},
                   dst = amd64.Operand.register amd64.Register.r15,
                   size = amd64.Size.QUAD},
                  amd64.Assembly.instruction_mov
                  {src = (amd64.Operand.address o amd64.Address.T)
                         {disp = SOME (amd64.Immediate.int 32),
                          base = SOME amd64.Register.rsp,
                          index = NONE, scale = NONE},
                   dst = amd64.Operand.register amd64.Register.r14,
                   size = amd64.Size.QUAD},
                  amd64.Assembly.instruction_mov
                  {src = (amd64.Operand.address o amd64.Address.T)
                         {disp = SOME (amd64.Immediate.int 40),
                          base = SOME amd64.Register.rsp,
                          index = NONE, scale = NONE},
                   dst = amd64.Operand.register amd64.Register.r13,
                   size = amd64.Size.QUAD},
                  amd64.Assembly.instruction_mov
                  {src = (amd64.Operand.address o amd64.Address.T)
                         {disp = SOME (amd64.Immediate.int 48),
                          base = SOME amd64.Register.rsp,
                          index = NONE, scale = NONE},
                   dst = amd64.Operand.register amd64.Register.r12,
                   size = amd64.Size.QUAD},
                  amd64.Assembly.instruction_mov
                  {src = (amd64.Operand.address o amd64.Address.T)
                         {disp = SOME (amd64.Immediate.int 56),
                          base = SOME amd64.Register.rsp,
                          index = NONE, scale = NONE},
                   dst = amd64.Operand.register amd64.Register.rbx,
                   size = amd64.Size.QUAD},
                  amd64.Assembly.instruction_mov
                  {src = (amd64.Operand.address o amd64.Address.T)
                         {disp = SOME (amd64.Immediate.int 64),
                          base = SOME amd64.Register.rsp,
                          index = NONE, scale = NONE},
                   dst = amd64.Operand.register amd64.Register.rbp,
                   size = amd64.Size.QUAD},
                  amd64.Assembly.instruction_binal
                  {oper = amd64.Instruction.ADD,
                   src = amd64.Operand.immediate_int 72,
                   dst = amd64.Operand.register amd64.Register.rsp,
                   size = amd64.Size.QUAD},
                  amd64.Assembly.instruction_ret {src = NONE}
                  ]
           in
              List.foreach
              (asm,
               fn asm => (Layout.print(Assembly.layout asm, print);
                          print "\n"))
           end

        val liveInfo = amd64Liveness.LiveInfo.newLiveInfo ()
        val jumpInfo = amd64JumpInfo.newJumpInfo ()

        fun frameInfoToAMD64 (Machine.FrameInfo.T {frameLayoutsIndex, ...}) =
           amd64.FrameInfo.T
           {frameLayoutsIndex = frameLayoutsIndex,
            size = Bytes.toInt (#size (Vector.sub (frameLayouts,
                                                   frameLayoutsIndex)))}

        fun outputChunk (chunk as Machine.Chunk.T {blocks, chunkLabel, ...},
                         print)
          = let
              val isMain 
                = Machine.ChunkLabel.equals(#chunkLabel main, chunkLabel)

              val () 
                = if isMain
                     then outputJumpToSML print
                     else ()

              val {chunk}
                = amd64Translate.translateChunk 
                  {chunk = chunk,
                   frameInfoToAMD64 = frameInfoToAMD64,
                   liveInfo = liveInfo}

              val chunk : amd64.Chunk.t
                = amd64Simplify.simplify 
                  {chunk = chunk,
                   (* don't perform optimizations on
                    * the main function (initGlobals)
                    *)
                   optimize = if isMain
                                then 0
                                else !Control.Native.optimize,
                   delProfileLabel = delProfileLabel,
                   liveInfo = liveInfo,
                   jumpInfo = jumpInfo}

              val unallocated_assembly : amd64.Assembly.t list list
                = (amd64GenerateTransfers.generateTransfers
                   {chunk = chunk,
                    optimize = !Control.Native.optimize,
                    newProfileLabel = newProfileLabel,
                    liveInfo = liveInfo,
                    jumpInfo = jumpInfo,
                    reserveRsp = reserveRsp})

              val allocated_assembly : Assembly.t list list
                = amd64AllocateRegisters.allocateRegisters 
                  {assembly = unallocated_assembly,
                   (* don't calculate liveness info
                    * on the main function (initGlobals)
                    *)
                   liveness = not isMain}

              val _ = Vector.foreach (blocks, Label.clear o Machine.Block.label)
              val _ = amd64.Immediate.clearAll ()
              val _ = amd64.MemLoc.clearAll ()
            in
              List.fold
              (allocated_assembly,
               if isMain then 30 else 0,
               fn (block, n)
                => List.fold
                   (block,
                    n,
                    fn (asm, n)
                     => (Layout.print (Assembly.layout asm, print);
                         print "\n";
                         n + 1)))
            end

        fun outputAssembly ()
          = let
              val split = !Control.Native.split
              fun loop chunks
                = let
                    val {print, done, ...} = makeS()
                    fun loop' (chunks, size) 
                      = case chunks
                          of [] => done ()
                           | chunk::chunks
                           => if (case split
                                    of NONE => false
                                     | SOME maxSize => size > maxSize)
                                then (done (); loop (chunk::chunks))
                                else loop'(chunks, 
                                           size + outputChunk (chunk, print))
                  in 
                    loop' (chunks, 0)
                  end
            in 
              loop chunks
              ; amd64Translate.translateChunk_totals ()
              ; amd64Simplify.simplify_totals ()
              ; amd64GenerateTransfers.generateTransfers_totals ()
              ; amd64AllocateRegisters.allocateRegisters_totals ()
            end

        val outputAssembly =
           Control.trace (Control.Pass, "outputAssembly") outputAssembly
      in
        outputAssembly()
        ; outputC()
      end 
end
