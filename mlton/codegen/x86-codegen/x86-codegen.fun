(* Copyright (C) 2009-2010,2014 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor x86Codegen (S: X86_CODEGEN_STRUCTS): X86_CODEGEN =
struct
  open S

  structure x86 = x86 (open Machine
                       structure RepType = Type)
  structure x86Pseudo = x86PseudoCheck (structure S = x86)

  structure x86MLtonBasic
    = x86MLtonBasic (structure x86 = x86Pseudo
                     structure Machine = Machine)

  structure x86Liveness
    = x86Liveness (structure x86 = x86
                   structure x86MLtonBasic = x86MLtonBasic)

  structure x86JumpInfo
    = x86JumpInfo (structure x86 = x86)

  structure x86LoopInfo
    = x86LoopInfo (structure x86 = x86)

  structure x86EntryTransfer
    = x86EntryTransfer (structure x86 = x86)

  structure x86MLton 
    = x86MLton (structure x86MLtonBasic = x86MLtonBasic
                structure x86Liveness = x86Liveness)

  val implementsPrim = x86MLton.implementsPrim

  structure x86Translate 
    = x86Translate (structure x86 = x86
                    structure x86MLton = x86MLton
                    structure x86Liveness = x86Liveness)

  structure x86Simplify
    = x86Simplify (structure x86 = x86
                   structure x86Liveness = x86Liveness
                   structure x86JumpInfo = x86JumpInfo
                   structure x86EntryTransfer = x86EntryTransfer)

  structure x86GenerateTransfers
    = x86GenerateTransfers (structure x86 = x86
                            structure x86MLton = x86MLton
                            structure x86Liveness = x86Liveness
                            structure x86JumpInfo = x86JumpInfo
                            structure x86LoopInfo = x86LoopInfo
                            structure x86EntryTransfer = x86EntryTransfer)

  structure x86AllocateRegisters
    = x86AllocateRegisters (structure x86 = x86
                            structure x86MLton = x86MLton)

  open x86
  fun output {program as Machine.Program.T {chunks, frameLayouts, handlesSignals,
                                            main, ...},
              outputC: unit -> {file: File.t,
                                print: string -> unit,
                                done: unit -> unit},
              outputS: unit -> {file: File.t,
                                print: string -> unit,
                                done: unit -> unit}}: unit
    = let
        val reserveEsp =
            (* There is no sigaltstack on cygwin, we need to reserve %esp to
             * hold the C stack pointer.  We only need to do this in programs
             * that handle signals.
             *)
            handlesSignals andalso let open Control.Target in !os = Cygwin end
        
        val (picMungeLabel, picBase) = x86AllocateRegisters.picRelative ()

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
               includes = ["x86-main.h"],
               print = print,
               program = program,
               rest = rest}
              ; done ()
            end 

        val outputC = Control.trace (Control.Pass, "outputC") outputC

        (* Assembly specific *)

        val _ = x86MLtonBasic.init ()

        fun outputJumpToSML print =
           let
              val jumpToSML = x86.Label.fromString "MLton_jumpToSML"
              val findEIP   = x86.Label.fromString "MLton_findEIP"
              val returnToC = x86.Label.fromString "Thread_returnToC"
              val c_stackP  = picMungeLabel x86MLton.c_stackP
              val gcState   = picMungeLabel x86MLton.gcState_label
              val {frontierReg, stackTopReg} =
                 if reserveEsp
                    then {frontierReg = x86.Register.edi,
                          stackTopReg = x86.Register.ebp}
                    else {frontierReg = x86.Register.esp,
                          stackTopReg = x86.Register.ebp}
              val prefixJumpToSML = [
                  x86.Assembly.pseudoop_text (),
                  x86.Assembly.pseudoop_p2align 
                  (x86.Immediate.int 4, NONE, NONE),
                  x86.Assembly.pseudoop_global jumpToSML,
                  x86.Assembly.pseudoop_hidden jumpToSML,
                  x86.Assembly.label jumpToSML,
                  x86.Assembly.instruction_binal
                  {oper = x86.Instruction.SUB,
                   src = x86.Operand.immediate_int 28,
                   dst = x86.Operand.register x86.Register.esp,
                   size = x86.Size.LONG},
                  x86.Assembly.instruction_mov
                  {src = (x86.Operand.address o x86.Address.T)
                         {disp = SOME (x86.Immediate.int 32),
                          base = SOME x86.Register.esp,
                          index= NONE, scale = NONE},
                   dst = x86.Operand.register x86.Register.eax,
                   size = x86.Size.LONG},
                  x86.Assembly.instruction_mov
                  {src = x86.Operand.register x86.Register.ebp,
                   dst = (x86.Operand.address o x86.Address.T)
                         {disp = SOME (x86.Immediate.int 24),
                          base = SOME x86.Register.esp,
                          index= NONE, scale = NONE},
                   size = x86.Size.LONG},
                  x86.Assembly.instruction_mov
                  {src = x86.Operand.register x86.Register.ebx,
                   dst = (x86.Operand.address o x86.Address.T)
                         {disp = SOME (x86.Immediate.int 20),
                          base = SOME x86.Register.esp,
                          index= NONE, scale = NONE},
                   size = x86.Size.LONG},
                  x86.Assembly.instruction_mov
                  {src = x86.Operand.register x86.Register.edi,
                   dst = (x86.Operand.address o x86.Address.T)
                         {disp = SOME (x86.Immediate.int 16),
                          base = SOME x86.Register.esp,
                          index= NONE, scale = NONE},
                   size = x86.Size.LONG},
                  x86.Assembly.instruction_mov
                  {src = x86.Operand.register x86.Register.esi,
                   dst = (x86.Operand.address o x86.Address.T)
                         {disp = SOME (x86.Immediate.int 12),
                          base = SOME x86.Register.esp,
                          index = NONE, scale = NONE},
                   size = x86.Size.LONG}
                  ]
              (* This is only included if PIC *)
              val loadGOT = [
                  x86.Assembly.instruction_call
                  {target = x86.Operand.label findEIP,
                   absolute = false},
                  x86.Assembly.instruction_binal
                  {oper = x86.Instruction.ADD,
                   src = x86.Operand.immediate_label x86MLton.globalOffsetTable,
                   dst = x86.Operand.register x86.Register.ebx,
                   size = x86.Size.LONG}
                  ]
              val suffixJumpToSML = [
                  x86.Assembly.instruction_mov
                  {src = (x86.Operand.address o x86.Address.T)
                         {disp = SOME (x86.Immediate.label c_stackP),
                          base = picBase, index = NONE, scale = NONE},
                   dst = x86.Operand.register x86.Register.ebp,
                   size = x86.Size.LONG},
                  x86.Assembly.instruction_mov
                  {src = x86.Operand.register x86.Register.ebp,
                   dst = (x86.Operand.address o x86.Address.T)
                         {disp = SOME (x86.Immediate.int 8),
                          base = SOME x86.Register.esp,
                          index = NONE, scale = NONE},
                   size = x86.Size.LONG},
                  x86.Assembly.instruction_mov
                  {src = x86.Operand.register x86.Register.esp,
                   dst = (x86.Operand.address o x86.Address.T)
                         {disp = SOME (x86.Immediate.label c_stackP),
                          base = picBase, index = NONE, scale = NONE},
                   size = x86.Size.LONG},
                  x86.Assembly.instruction_mov
                  {src = (x86.Operand.address o x86.Address.T)
                         {disp = (SOME o x86.Immediate.labelPlusInt)
                                 (gcState,
                                  Bytes.toInt 
                                  (Machine.Runtime.GCField.offset
                                   Machine.Runtime.GCField.StackTop)),
                          base = picBase, index = NONE, scale = NONE},
                   dst = x86.Operand.register stackTopReg,
                   size = x86.Size.LONG},
                  x86.Assembly.instruction_mov
                  {src = (x86.Operand.address o x86.Address.T)
                         {disp = (SOME o x86.Immediate.labelPlusInt)
                                 (gcState,
                                  Bytes.toInt 
                                  (Machine.Runtime.GCField.offset
                                   Machine.Runtime.GCField.Frontier)),
                          base = picBase, index = NONE, scale = NONE},
                   dst = x86.Operand.register frontierReg,
                   size = x86.Size.LONG},
                  x86.Assembly.instruction_jmp
                  {target = x86.Operand.register x86.Register.eax,
                   absolute = true}
                  ]
              val bodyReturnToC = [
                  x86.Assembly.pseudoop_p2align 
                  (x86.Immediate.int 4, NONE, NONE),
                  x86.Assembly.pseudoop_global returnToC,
                  x86.Assembly.pseudoop_hidden returnToC,
                  x86.Assembly.label returnToC,
                  x86.Assembly.instruction_mov
                  {src = (x86.Operand.address o x86.Address.T)
                         {disp = SOME (x86.Immediate.label c_stackP),
                          base = picBase, index = NONE, scale = NONE},
                   dst = x86.Operand.register x86.Register.esp,
                   size = x86.Size.LONG},
                  x86.Assembly.instruction_mov
                  {src = (x86.Operand.address o x86.Address.T)
                         {disp = SOME (x86.Immediate.int 8),
                          base = SOME x86.Register.esp,
                          index = NONE, scale = NONE},
                   dst = x86.Operand.register x86.Register.ebp,
                   size = x86.Size.LONG},
                  x86.Assembly.instruction_mov
                  {src = x86.Operand.register x86.Register.ebp,
                   dst = (x86.Operand.address o x86.Address.T)
                         {disp = SOME (x86.Immediate.label c_stackP),
                          base = picBase, index = NONE, scale = NONE},
                   size = x86.Size.LONG},
                  x86.Assembly.instruction_mov
                  {src = (x86.Operand.address o x86.Address.T)
                         {disp = SOME (x86.Immediate.int 12),
                          base = SOME x86.Register.esp,
                          index = NONE, scale = NONE},
                   dst = x86.Operand.register x86.Register.esi,
                   size = x86.Size.LONG},
                  x86.Assembly.instruction_mov
                  {src = (x86.Operand.address o x86.Address.T)
                         {disp = SOME (x86.Immediate.int 16),
                          base = SOME x86.Register.esp,
                          index = NONE, scale = NONE},
                   dst = x86.Operand.register x86.Register.edi,
                   size = x86.Size.LONG},
                  x86.Assembly.instruction_mov
                  {src = (x86.Operand.address o x86.Address.T)
                         {disp = SOME (x86.Immediate.int 20),
                          base = SOME x86.Register.esp,
                          index = NONE, scale = NONE},
                   dst = x86.Operand.register x86.Register.ebx,
                   size = x86.Size.LONG},
                  x86.Assembly.instruction_mov
                  {src = (x86.Operand.address o x86.Address.T)
                         {disp = SOME (x86.Immediate.int 24),
                          base = SOME x86.Register.esp,
                          index = NONE, scale = NONE},
                   dst = x86.Operand.register x86.Register.ebp,
                   size = x86.Size.LONG},
                  x86.Assembly.instruction_binal
                  {oper = x86.Instruction.ADD,
                   src = x86.Operand.immediate_int 28,
                   dst = x86.Operand.register x86.Register.esp,
                   size = x86.Size.LONG},
                  x86.Assembly.instruction_ret {src = NONE}
                  ]
              (* This is only included if PIC *)
              val bodyFindEIP = [
                  x86.Assembly.pseudoop_p2align 
                  (x86.Immediate.int 4, NONE, NONE),
                  x86.Assembly.pseudoop_global findEIP,
                  x86.Assembly.pseudoop_hidden findEIP,
                  x86.Assembly.label findEIP,
                  x86.Assembly.instruction_mov
                  {src = (x86.Operand.address o x86.Address.T)
                         {base = SOME x86.Register.esp,
                          disp = NONE, index = NONE, scale = NONE},
                   dst = x86.Operand.register x86.Register.ebx,
                   size = x86.Size.LONG},
                  x86.Assembly.instruction_ret {src = NONE}
                  ]
              
              val asm = 
                 List.concat
                 (if picBase <> NONE
                  then [prefixJumpToSML, loadGOT, suffixJumpToSML,
                        bodyReturnToC, bodyFindEIP]
                  else [prefixJumpToSML, suffixJumpToSML, 
                        bodyReturnToC])
           in
              List.foreach
              (asm,
               fn asm => (Layout.print(Assembly.layout asm, print);
                          print "\n"))
           end

        val liveInfo = x86Liveness.LiveInfo.newLiveInfo ()
        val jumpInfo = x86JumpInfo.newJumpInfo ()

        fun frameInfoToX86 (Machine.FrameInfo.T {frameLayoutsIndex, ...}) =
           x86.FrameInfo.T
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
                = x86Translate.translateChunk 
                  {chunk = chunk,
                   frameInfoToX86 = frameInfoToX86,
                   liveInfo = liveInfo}

              val chunk : x86.Chunk.t
                = x86Simplify.simplify 
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

              val unallocated_assembly : x86.Assembly.t list list
                = (x86GenerateTransfers.generateTransfers
                   {chunk = chunk,
                    optimize = !Control.Native.optimize,
                    newProfileLabel = newProfileLabel,
                    liveInfo = liveInfo,
                    jumpInfo = jumpInfo,
                    reserveEsp = reserveEsp,
                    picUsesEbx = picBase <> NONE})

              val allocated_assembly : Assembly.t list list
                = x86AllocateRegisters.allocateRegisters 
                  {assembly = unallocated_assembly,
                   (* don't calculate liveness info
                    * on the main function (initGlobals)
                    *)
                   liveness = not isMain}

              val _ = Vector.foreach (blocks, Label.clear o Machine.Block.label)
              val _ = x86.Immediate.clearAll ()
              val _ = x86.MemLoc.clearAll ()
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
              ; x86Translate.translateChunk_totals ()
              ; x86Simplify.simplify_totals ()
              ; x86GenerateTransfers.generateTransfers_totals ()
              ; x86AllocateRegisters.allocateRegisters_totals ()
            end

        val outputAssembly =
           Control.trace (Control.Pass, "outputAssembly") outputAssembly
      in
        outputAssembly()
        ; outputC()
      end 
end
