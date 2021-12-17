(* Copyright (C) 2009,2019-2021 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor x86Translate(S: X86_TRANSLATE_STRUCTS): X86_TRANSLATE =
struct

  open S

  val tracerTop = x86.tracerTop

  fun argsToString(ss: string list): string
    = "(" ^ (concat (List.separate(ss, ", "))) ^ ")"

  structure Machine = x86MLton.Machine

  local
     open Machine
  in
     structure CSymbol = CSymbol
     structure CSymbolScope = CSymbolScope
     structure Const = Const
     structure Label = Label
     structure Live = Live
     structure Scale = Scale
     structure StackOffset = StackOffset
     structure Temporary = Temporary
     structure Type = Type
     structure WordSize = WordSize
     structure WordX = WordX
  end

  datatype z = datatype WordSize.prim

  structure Global =
     struct
        open Machine.Global

        fun toX86Operand (g: t) : (x86.Operand.t * x86.Size.t) vector =
           let
              val ty = Machine.Type.toCType (ty g)
              val index = index g
              val base =
                 x86.Immediate.label (x86MLton.global_base ty)
              val origin =
                 x86.MemLoc.imm
                 {base = base,
                  index = x86.Immediate.int index,
                  scale = x86.Scale.fromCType ty,
                  size = x86.Size.BYTE,
                  class = x86MLton.Classes.Globals}
              val sizes = x86.Size.fromCType ty
           in
              (#1 o Vector.mapAndFold)
              (sizes, 0, fn (size,offset) =>
               (((x86.Operand.memloc o x86.MemLoc.shift)
                 {origin = origin,
                  disp = x86.Immediate.int offset,
                  scale = x86.Scale.One,
                  size = size}, size), offset + x86.Size.toBytes size))
           end
     end

  type transInfo = x86MLton.transInfo

  structure Operand =
    struct
      open Machine.Operand

      fun get (f: ('a * 'b) -> 'c) (i: int) (v: ('a * 'b) vector) =
         f (Vector.sub (v, i))
      fun getOp0 v =
         get #1 0 v

      fun toX86Operand {operand, transInfo = {addData, ...}: transInfo} =
      let
      local
         fun fromSizes (sizes, origin) =
            (#1 o Vector.mapAndFold)
            (sizes, 0, fn (size,offset) =>
             (((x86.Operand.memloc o x86.MemLoc.shift)
               {origin = origin,
                disp = x86.Immediate.int offset,
                scale = x86.Scale.One,
                size = size}, size), offset + x86.Size.toBytes size))
      in
      val rec toX86Operand : t -> (x86.Operand.t * x86.Size.t) vector =
         fn SequenceOffset {base, index, offset, scale, ty}
            => let
                  val base = toX86Operand base
                  val _ = Assert.assert("x86Translate.Operand.toX86Operand: SequenceOffset/base",
                                        fn () => Vector.length base = 1)
                  val base = getOp0 base
                  val index = toX86Operand index
                  val _ = Assert.assert("x86Translate.Operand.toX86Operand: SequenceOffset/index",
                                       fn () => Vector.length index = 1)
                  val index = getOp0 index
                  val scale =
                     case scale of
                        Scale.One => x86.Scale.One
                      | Scale.Two => x86.Scale.Two
                      | Scale.Four => x86.Scale.Four
                      | Scale.Eight => x86.Scale.Eight
                  val ty = Type.toCType ty
                  val origin =
                     case (x86.Operand.deImmediate base,
                           x86.Operand.deMemloc base,
                           x86.Operand.deImmediate index,
                           x86.Operand.deMemloc index) of
                        (SOME base, _, SOME index, _) =>
                           x86.MemLoc.imm
                           {base = base,
                            index = index,
                            scale = scale,
                            size = x86.Size.BYTE,
                            class = x86MLton.Classes.Heap}
                      | (SOME base, _, _, SOME index) =>
                           x86.MemLoc.basic
                           {base = base,
                            index = index,
                            scale = scale,
                            size = x86.Size.BYTE,
                            class = x86MLton.Classes.Heap}
                      | (_, SOME base, SOME index, _) =>
                           x86.MemLoc.simple
                           {base = base,
                            index = index,
                            scale = scale,
                            size = x86.Size.BYTE,
                            class = x86MLton.Classes.Heap}
                      | (_, SOME base, _, SOME index) =>
                           x86.MemLoc.complex
                           {base = base,
                            index = index,
                            scale = scale,
                            size = x86.Size.BYTE,
                            class = x86MLton.Classes.Heap}
                      | _ => Error.bug (concat ["x86Translate.Operand.toX86Operand: ",
                                                "strange SequenceOffset: base: ",
                                                x86.Operand.toString base,
                                                " index: ",
                                                x86.Operand.toString index])
                  val origin =
                     if Bytes.isZero offset
                        then origin
                        else x86.MemLoc.shift
                             {origin = origin,
                              disp = x86.Immediate.int (Bytes.toInt offset),
                              scale = x86.Scale.One,
                              size = x86.Size.BYTE}
                  val sizes = x86.Size.fromCType ty
               in
                 fromSizes (sizes, origin)
               end
          | Cast (z, _) => toX86Operand z
          | Const (Const.CSymbol (CSymbol.T {name, symbolScope, ...})) =>
               let
                  datatype z = datatype CSymbolScope.t
                  datatype z = datatype Control.Format.t
                  datatype z = datatype MLton.Platform.OS.t

                  val label = fn () => Label.fromString name

                  (* how to access an imported label's address *)
                  (* windows coff will add another leading _ to label *)
                  val coff = fn () => Label.fromString ("_imp__" ^ name)
                  val macho = fn () =>
                     let
                        val label =
                           Label.newString (concat ["L_", name, "_non_lazy_ptr"])
                        val () =
                           addData
                           [x86.Assembly.pseudoop_non_lazy_symbol_pointer (),
                            x86.Assembly.label label,
                            x86.Assembly.pseudoop_indirect_symbol (Label.fromString name),
                            x86.Assembly.pseudoop_long [x86.Immediate.zero]]
                     in
                        label
                     end
                  val elf = fn () => Label.fromString (name ^ "@GOT")

                  val importLabel = fn () =>
                     case !Control.Target.os of
                        Cygwin => coff ()
                      | Darwin => macho ()
                      | MinGW => coff ()
                      | _ => elf ()

                  val direct = fn () =>
                     Vector.new1
                     (x86.Operand.immediate_label (label ()),
                      x86.Size.LONG)
                  val indirect = fn () =>
                     Vector.new1
                     (x86.Operand.memloc_label (importLabel ()),
                      x86.Size.LONG)
               in
                  case (symbolScope,
                        !Control.Target.os,
                        !Control.Native.pic) of
                   (* Even private PIC symbols on darwin need indirection. *)
                     (Private, Darwin, true) => indirect ()
                   (* As long as the symbol is private (thus it is not
                    * exported to code outside this text segment), then
                    * use normal addressing. If PIC is needed, then the
                    * memloc_label is updated to relative access in the
                    * allocate-registers pass.
                    *)
                   | (Private, _, _) => direct ()
                   (* On darwin, even executables use the defintion address.
                    * Therefore we don't need to do indirection.
                    *)
                   | (Public, Darwin, _) => direct ()
                   (* On ELF, a public symbol must be accessed via
                    * the GOT. This is because the final value may not be
                    * in this text segment. If the executable uses it, then
                    * the unique C address resides in the executable's
                    * text segment. The loader does this by creating a PLT
                    * proxy or copying values to the executable text segment.
                    * When linking an executable, ELF uses a special trick
                    * to "simplify" the code. All exported functions and
                    * symbols have pointers that correspond  to the
                    * executable. Function pointers point to the
                    * automatically created PLT entry in the executable.
                    * Variables are copied/relocated into the executable bss.
                    *
                    * This means that direct access is fine for executable
                    * and archive formats. (It also means direct access is
                    * NOT fine for a library, even if it defines the symbol.)
                    *
                    *)
                   | (Public, _, true) => indirect ()
                   | (Public, _, false) => direct ()
                   (* On darwin, the address is the point of definition. So
                    * indirection is needed. We also need to make a stub!
                    *)
                   | (External, Darwin, _) => indirect ()
                   (* On windows, the address is the point of definition. So
                    * we must always use an indirect lookup to the symbols
                    * windows rewrites (__imp__name) in our segment.
                    *)
                   | (External, MinGW, _) => indirect ()
                   | (External, Cygwin, _) => indirect ()
                   (* When compiling ELF to a library, we access external
                    * symbols via some address that is updated by the loader.
                    * That address resides within our data segment, and can
                    * be easily referenced using RBX-relative addressing.
                    * This trick is used on every platform MLton supports.
                    * ELF rewrites symbols of form name@GOT.
                    *)
                   | (External, _, true) => indirect ()
                   | (External, _, false) => direct ()
               end
          | Const Const.Null =>
               Vector.new1 (x86.Operand.immediate_zero, x86MLton.wordSize)
          | Const (Const.Word w) =>
               let
                  fun single size =
                     Vector.new1 (x86.Operand.immediate_word w, size)
               in
                  case WordSize.prim (WordX.size w) of
                     W8 => single x86.Size.BYTE
                   | W16 => single x86.Size.WORD
                   | W32 => single x86.Size.LONG
                   | W64 =>
                        let
                           val lo = WordX.resize (w, WordSize.word32)
                           val w = WordX.rshift (w,
                                                 WordX.fromIntInf (32, WordSize.word64),
                                                 {signed = true})
                           val hi = WordX.resize (w, WordSize.word32)
                        in
                           Vector.new2
                           ((x86.Operand.immediate_word lo, x86.Size.LONG),
                            (x86.Operand.immediate_word hi, x86.Size.LONG))
                        end
               end
          | Const _ => Error.bug "x86Translate.Operand.toX86Operand: Const"
          | Frontier => 
               let 
                  val frontier = x86MLton.gcState_frontierContentsOperand ()
               in
                  Vector.new1 (frontier, valOf (x86.Operand.size frontier))
               end
          | GCState => 
               Vector.new1 (x86.Operand.immediate_label x86MLton.gcState_label,
                            x86MLton.pointerSize)
          | Global g => Global.toX86Operand g
          | Label l => 
               Vector.new1 (x86.Operand.immediate_label l, x86MLton.pointerSize)
          | Offset {base = GCState, offset, ty, volatile = _} =>
               let
                  val offset = Bytes.toInt offset
                  val ty = Type.toCType ty
                  val offset = x86MLton.gcState_offset {offset = offset, ty = ty}
               in
                  Vector.new1 (offset, valOf (x86.Operand.size offset))
               end
          | Offset {base, offset, ty, volatile = _} =>
               let
                 val offset = Bytes.toInt offset
                 val ty = Type.toCType ty
                 val base = toX86Operand base
                 val _ = Assert.assert("x86Translate.Operand.toX86Operand: Offset/base",
                                       fn () => Vector.length base = 1)
                 val base = getOp0 base
                 val origin =
                    case (x86.Operand.deImmediate base,
                          x86.Operand.deMemloc base) of
                       (SOME base, _) =>
                          x86.MemLoc.imm
                          {base = base,
                           index = x86.Immediate.int offset,
                           scale = x86.Scale.One,
                           size = x86.Size.BYTE,
                           class = x86MLton.Classes.Heap}
                     | (_, SOME base) =>
                          x86.MemLoc.simple
                          {base = base,
                           index = x86.Immediate.int offset,
                           scale = x86.Scale.One,
                           size = x86.Size.BYTE,
                           class = x86MLton.Classes.Heap}
                     | _ => Error.bug (concat ["x86Translate.Operand.toX86Operand: ",
                                               "strange Offset: base: ",
                                               x86.Operand.toString base])
                 val sizes = x86.Size.fromCType ty
               in
                  fromSizes (sizes, origin)
               end
          | StackOffset (StackOffset.T {offset, ty}) =>
               let
                  val offset = Bytes.toInt offset
                  val ty = Type.toCType ty
                  val origin =
                     x86.MemLoc.simple 
                     {base = x86MLton.gcState_stackTopContents (), 
                      index = x86.Immediate.int offset,
                      scale = x86.Scale.One,
                      size = x86.Size.BYTE,
                      class = x86MLton.Classes.Stack}
                  val sizes = x86.Size.fromCType ty
               in
                  fromSizes (sizes, origin)
               end
           | StaticHeapRef (Machine.StaticHeap.Ref.T {kind, offset, ...}) =>
               let
                  val offset = Bytes.toInt offset
                  val base =
                     x86.Immediate.labelPlusInt
                     (Machine.StaticHeap.Kind.label kind, offset)
               in
                  Vector.new1 (x86.Operand.immediate base, x86MLton.pointerSize)
               end
          | StackTop => 
               let 
                  val stackTop = x86MLton.gcState_stackTopContentsOperand ()
               in
                  Vector.new1 (stackTop, valOf (x86.Operand.size stackTop))
               end
          | Temporary t =>
               let
                  val ty = Machine.Type.toCType (Temporary.ty t)
                  val index = Machine.Temporary.index t
                  val base = x86.Immediate.label (x86MLton.local_base ty)
                  val origin =
                     x86.MemLoc.imm
                     {base = base,
                      index = x86.Immediate.int index,
                      scale = x86.Scale.fromCType ty,
                      size = x86.Size.BYTE,
                      class = x86MLton.Classes.Locals}
                  val sizes = x86.Size.fromCType ty
               in
                  fromSizes (sizes, origin)
               end
      end
      in
         toX86Operand operand
      end
    end

  structure Entry =
    struct
      structure Kind = Machine.Kind

      fun frameInfoToX86 fi =
         x86.FrameInfo.T
         {frameInfosIndex = Machine.FrameInfo.index fi,
          size = Bytes.toInt (Machine.FrameInfo.size fi)}

      fun toX86Blocks {label, kind, 
                       transInfo as {live, liveInfo, ...}: transInfo}
        = (
           x86Liveness.LiveInfo.setLiveOperands
           (liveInfo, label, live label);
           case kind
             of Kind.Jump
              => let
                 in
                   AppendList.single
                   (x86.Block.mkBlock'
                    {entry = SOME (x86.Entry.jump {label = label}),
                     statements = [],
                     transfer = NONE})
                 end
              | Kind.Func _
              => let
                   val args
                     = List.fold
                       (live label,
                        x86.MemLocSet.empty,
                        fn (operand, args)
                         => case x86.Operand.deMemloc operand
                              of SOME memloc => x86.MemLocSet.add(args, memloc)
                               | NONE => args)
                 in
                   AppendList.single
                   (x86.Block.mkBlock'
                    {entry = SOME (x86.Entry.func {label = label,
                                                   live = args}),
                     statements = [],
                     transfer = NONE})
                 end
              | Kind.Cont {args, frameInfo, ...}
              => let
                    val frameInfo = frameInfoToX86 frameInfo
                    val args =
                       Vector.fold
                       (args, x86.MemLocSet.empty,
                        fn (operand,args) =>
                        Vector.fold
                        (Operand.toX86Operand {operand = Live.toOperand operand,
                                               transInfo = transInfo},
                         args,
                         fn ((operand,_),args) =>
                         case x86.Operand.deMemloc operand of
                            SOME memloc => x86.MemLocSet.add(args, memloc)
                          | NONE => args))
                 in
                   AppendList.single
                   (x86.Block.mkBlock'
                    {entry = SOME (x86.Entry.cont {frameInfo = frameInfo,
                                                   label = label,
                                                   live = args}),
                     statements = [],
                     transfer = NONE})
                 end
              | Kind.Handler {args, frameInfo, ...}
              => let
                    val frameInfo = frameInfoToX86 frameInfo
                    val args =
                       Vector.fold
                       (args, x86.MemLocSet.empty,
                        fn (operand,args) =>
                        Vector.fold
                        (Operand.toX86Operand {operand = Live.toOperand operand,
                                               transInfo = transInfo},
                         args,
                         fn ((operand,_),args) =>
                         case x86.Operand.deMemloc operand of
                            SOME memloc => x86.MemLocSet.add(args, memloc)
                          | NONE => args))
                 in 
                   AppendList.single
                   (x86.Block.mkBlock'
                    {entry = SOME (x86.Entry.handler {frameInfo = frameInfo,
                                                      label = label,
                                                      live = args}),
                     statements = [],
                     transfer = NONE})
                 end
              | Kind.CReturn {dst, frameInfo, func}
              => let
                   val dsts =
                      case dst of
                         NONE => Vector.new0 ()
                       | SOME dst => Operand.toX86Operand {operand = Live.toOperand dst,
                                                           transInfo = transInfo}
                 in
                   x86MLton.creturn
                   {dsts = dsts,
                    frameInfo = Option.map (frameInfo, frameInfoToX86),
                    func = func,
                    label = label,
                    transInfo = transInfo}
                 end)
    end

  structure Statement =
    struct
      open Machine.Statement

      fun comments statement
        = if !Control.codegenComments > 0
            then let
                   val comment = (Layout.toString o layout) statement
                 in
                   (AppendList.single
                    (x86.Block.mkBlock'
                     {entry = NONE,
                      statements = [x86.Assembly.comment
                                    (concat ["begin: ",
                                             comment])],
                      transfer = NONE}),
                    AppendList.single
                    (x86.Block.mkBlock'
                     {entry = NONE,
                      statements = [x86.Assembly.comment
                                    (concat ["end: ",
                                             comment])],
                      transfer = NONE}))
                 end
            else (AppendList.empty,AppendList.empty)

      fun toX86Blocks {statement,
                       transInfo as {...} : transInfo}
        = (case statement
             of Move {src, dst}
              => let
                   val (comment_begin,
                        comment_end) = comments statement

                   val dsts = Operand.toX86Operand {operand = dst, transInfo = transInfo}
                   val srcs = Operand.toX86Operand {operand = src, transInfo = transInfo}
                   (* Operand.toX86Operand returns multi-word 
                    * operands in and they will be moved in order,
                    * so it suffices to check for aliasing between 
                    * the first dst and second src.
                    *)
                   val (dsts,srcs) =
                      if Vector.length srcs > 1
                         andalso x86.Operand.mayAlias
                                 (#1 (Vector.sub (dsts, 0)), 
                                  #1 (Vector.sub (srcs, 1)))
                         then (Vector.rev dsts, Vector.rev srcs)
                         else (dsts,srcs)
                 in
                   AppendList.appends
                   [comment_begin,
                    AppendList.single
                    (x86.Block.mkBlock'
                     {entry = NONE,
                      statements
                      = (Vector.toList o Vector.map2)
                        (dsts,srcs,fn ((dst,_),(src,srcsize)) =>
                         (* dst = src *)
                         case x86.Size.class srcsize
                            of x86.Size.INT => x86.Assembly.instruction_mov 
                                               {dst = dst,
                                                src = src,
                                                size = srcsize}
                          | x86.Size.FLT => x86.Assembly.instruction_pfmov
                                            {dst = dst,
                                             src = src,
                                             size = srcsize}
                          | _ => Error.bug "x86Translate.Statement.toX86Blocks: Move"),
                      transfer = NONE}),
                    comment_end]
                 end 
              | PrimApp {dst, prim, args}
              => let
                   val (comment_begin, comment_end) = comments statement
                   val args = (Vector.concatV o Vector.map)
                              (args, fn operand =>
                               Operand.toX86Operand {operand = operand,
                                                     transInfo = transInfo})
                   val dsts = 
                      case dst of
                         NONE => Vector.new0 ()
                       | SOME dst => Operand.toX86Operand {operand = dst, transInfo = transInfo}
                 in
                   AppendList.appends
                   [comment_begin,
                    (x86MLton.prim {prim = prim,
                                    args = args,
                                    dsts = dsts,
                                    transInfo = transInfo}),
                    comment_end]
                 end)
    end

  structure Transfer =
    struct
      open Machine.Transfer

      fun goto l
        = AppendList.single
          (x86.Block.mkBlock'
           {entry = NONE,
            statements = [],
            transfer = SOME (x86.Transfer.goto
                             {target = l})})

      fun iff (test, a, b, transInfo)
        = let
            val (test,testsize) =
               Vector.sub (Operand.toX86Operand {operand = test, transInfo = transInfo}, 0)
          in
            if Label.equals(a, b)
              then AppendList.single
                   (x86.Block.mkBlock'
                    {entry = NONE,
                     statements = [],
                     transfer = SOME (x86.Transfer.goto {target = a})})
              else AppendList.single
                   ((* if (test) goto a
                     * goto b
                     *)
                    x86.Block.mkBlock'
                    {entry = NONE,
                     statements 
                     = [x86.Assembly.instruction_test
                        {src1 = test,
                         src2 = test,
                         size = testsize}],
                     transfer
                     = SOME (x86.Transfer.iff
                             {condition = x86.Instruction.NZ,
                              truee = a,
                              falsee = b})})
          end

      fun cmp (test, k, a, b, transInfo)
        = let
            val (test,testsize) =
               Vector.sub (Operand.toX86Operand {operand = test, transInfo = transInfo}, 0)
          in
            if Label.equals(a, b)
              then AppendList.single
                   (x86.Block.mkBlock'
                    {entry = NONE,
                     statements = [],
                     transfer = SOME (x86.Transfer.goto {target = a})})
              else AppendList.single
                   ((* if (test = k) goto a
                     * goto b
                     *)
                    x86.Block.mkBlock'
                    {entry = NONE,
                     statements 
                     = [x86.Assembly.instruction_cmp
                        {src1 = test,
                         src2 = x86.Operand.immediate k,
                         size = testsize}],
                     transfer
                     = SOME (x86.Transfer.iff
                             {condition = x86.Instruction.E,
                              truee = a,
                              falsee = b})})
          end

      fun switch(test, cases, default, transInfo)
        = let
            val test = Operand.toX86Operand {operand = test,
                                             transInfo = transInfo}
            val (test,_) = Vector.sub(test, 0)
          in
            AppendList.single
            (x86.Block.mkBlock'
             {entry = NONE,
              statements = [],
              transfer = SOME (x86.Transfer.switch
                               {test = test,
                                cases = cases,
                                default = default})})
          end

      fun doSwitchWord (test, cases, default, transInfo)
        = (case (cases, default)
             of ([],            NONE)
              => Error.bug "x86Translate.Transfer.doSwitchWord"
              | ([(_,l)],       NONE) => goto l
              | ([],            SOME l) => goto l
              | ([(w1,l1),(w2,l2)], NONE) => 
                if WordX.isZero w1 andalso WordX.isOne w2
                   then iff(test,l2,l1,transInfo)
                else if WordX.isZero w2 andalso WordX.isOne w1
                   then iff(test,l1,l2,transInfo)
                else cmp(test,x86.Immediate.word w1,l1,l2,transInfo)
              | ([(k',l')],      SOME l)
              => cmp(test,x86.Immediate.word k',l',l,transInfo)
              | ((_,l)::cases,  NONE) 
              => switch(test, x86.Transfer.Cases.word cases, l, transInfo)
              | (cases,         SOME l) 
              => switch(test, x86.Transfer.Cases.word cases, l, transInfo))

      fun comments transfer
        = if !Control.codegenComments > 0
            then let
                   val comment = (Layout.toString o layout) transfer
                 in
                   AppendList.single
                   (x86.Block.mkBlock'
                    {entry = NONE,
                     statements = [x86.Assembly.comment comment],
                      transfer = NONE})
                 end
            else AppendList.empty


      fun toX86Blocks {returns, transfer, transInfo: transInfo}
        = (case transfer
             of CCall {args, func, return}
              => let
                   val args = (Vector.concatV o Vector.map)
                              (args, fn operand =>
                               Operand.toX86Operand {operand = operand,
                                                     transInfo = transInfo})
                 in
                   AppendList.append
                   (comments transfer,  
                    x86MLton.ccall {args = args,
                                    func = func,
                                    return = Option.map (return, fn {return, size} =>
                                                         {return = return,
                                                          size = Option.map (size, Bytes.toInt)}),
                                    transInfo = transInfo})
                 end
              | Return _
              => AppendList.append
                 (comments transfer,
                  AppendList.single
                  (x86.Block.mkBlock'
                   {entry = NONE,
                    statements = [],
                    transfer 
                    = SOME (x86.Transfer.return 
                            {live 
                             = Vector.fold
                               ((case returns of
                                    NONE => Error.bug "x86Translate.Transfer.toX86Blocsk: Return"
                                  | SOME zs => zs),
                                x86.MemLocSet.empty,
                                fn (operand, live) =>
                                Vector.fold
                                (Operand.toX86Operand {operand = operand,
                                                       transInfo = transInfo},
                                 live,
                                 fn ((operand,_),live) =>
                                 case x86.Operand.deMemloc operand of
                                    SOME memloc => x86.MemLocSet.add(live, memloc)
                                  | NONE => live))})}))
              | Raise _
              => AppendList.append
                 (comments transfer,
                  AppendList.single
                  (x86.Block.mkBlock'
                   {entry = NONE,
                    statements = [],
                    transfer 
                    = SOME (x86.Transfer.raisee 
                            {live 
                             = x86.MemLocSet.add
                               (x86.MemLocSet.add
                                (x86.MemLocSet.empty,
                                 x86MLton.gcState_stackBottomContents ()),
                                x86MLton.gcState_exnStackContents ())})}))
              | Switch (Machine.Switch.T {cases, default, test, ...})
              => AppendList.append
                 (comments transfer,
                  doSwitchWord (test, Vector.toList cases, default, transInfo))
              | Goto label
              => (AppendList.append
                  (comments transfer,
                   AppendList.single
                   ((* goto label *)
                    x86.Block.mkBlock'
                    {entry = NONE,
                     statements = [],
                     transfer = SOME (x86.Transfer.goto {target = label})})))
              | Call {label, live, return, ...}
              => let
                    val live =
                       Vector.fold
                       (live, x86.MemLocSet.empty, fn (operand, live) =>
                        Vector.fold
                        (Operand.toX86Operand {operand = Live.toOperand operand,
                                               transInfo = transInfo},
                         live,
                         fn ((operand, _), live) =>
                         case x86.Operand.deMemloc operand of
                            NONE => live
                          | SOME memloc => x86.MemLocSet.add (live, memloc)))
                    val com = comments transfer
                    val transfer =
                       case return of
                          NONE => x86.Transfer.tail {target = label,
                                                     live = live}
                        | SOME {return, handler, size} =>
                             x86.Transfer.nontail {target = label,
                                                   live = live,
                                                   return = return,
                                                   handler = handler,
                                                   size = Bytes.toInt size}
                 in
                    AppendList.append
                    (com,
                     AppendList.single
                     (x86.Block.mkBlock' {entry = NONE,
                                    statements = [],
                                    transfer = SOME transfer}))
                 end)
    end

  structure Block =
    struct
      open Machine.Block

      fun toX86Blocks {block = T {label, 
                                  live, 
                                  kind, 
                                  returns,
                                  statements, 
                                  transfer,
                                  ...},
                       transInfo as {...} : transInfo}
        = let
            val pseudo_blocks
              = AppendList.append
                (AppendList.snoc
                 (Entry.toX86Blocks {label = label,
                                     kind = kind,
                                     transInfo = transInfo},
                  x86.Block.mkBlock'
                  {entry = NONE,
                   statements 
                   = if !Control.codegenComments > 0
                       then let
                              val comment =
                                 concat ["Live: ",
                                         argsToString
                                         (Vector.toListMap
                                          (live, fn l =>
                                           Operand.toString (Live.toOperand l)))]
                            in
                              [x86.Assembly.comment comment]
                            end
                       else [],
                    transfer = NONE}),
                 Vector.foldr(statements,
                              (Transfer.toX86Blocks
                               {returns = (Option.map
                                           (returns, fn v =>
                                            Vector.map (v, Live.toOperand))),
                                transfer = transfer,
                                transInfo = transInfo}),
                              fn (statement,l)
                               => AppendList.append
                                  (Statement.toX86Blocks 
                                   {statement = statement,
                                    transInfo = transInfo}, l)))

            val pseudo_blocks = AppendList.toList pseudo_blocks

            val blocks = x86.Block.compress pseudo_blocks
          in
            blocks
          end
    end

  structure Chunk =
    struct
      open Machine.Chunk

      fun toX86Chunk {chunk = T {blocks, ...}, 
                      liveInfo}
        = let
            val data = ref []
            val addData = fn l => List.push (data, l)
            val {get = live : Label.t -> x86.Operand.t list,
                 set = setLive, 
                 rem = remLive, ...}
              = Property.getSetOnce
                (Label.plist, Property.initRaise ("live", Label.layout))
            val transInfo = {addData = addData,
                             live = live,
                             liveInfo = liveInfo}
            val _ = Vector.foreach
                    (blocks, fn Block.T {label, live, ...} =>
                     setLive (label,
                              (Vector.toList o #1 o Vector.unzip o 
                               Vector.concatV o Vector.map)
                              (live, fn operand =>
                               Operand.toX86Operand {operand = Live.toOperand operand,
                                                     transInfo = transInfo})))
            val x86Blocks 
              = List.concat (Vector.toListMap
                             (blocks, 
                                fn block
                                 => Block.toX86Blocks 
                                    {block = block,
                                     transInfo = transInfo}))
            val _ = Vector.foreach (blocks, fn Block.T {label, ...} =>
                                    remLive label)
            val data = List.concatRev (!data)
            val data =
               if List.isEmpty data
                  then []
                  else (x86.Assembly.pseudoop_data())::data
          in
            x86.Chunk.T {data = data, blocks = x86Blocks}
          end
    end

  fun translateChunk {chunk: x86MLton.Machine.Chunk.t,
                      liveInfo: x86Liveness.LiveInfo.t}:
                     {chunk: x86.Chunk.t}
    = {chunk = Chunk.toX86Chunk {chunk = chunk,
                                 liveInfo = liveInfo}}

  val (translateChunk, translateChunk_msg)
    = tracerTop
      "translateChunk"
      translateChunk

  fun translateChunk_totals ()
    = (translateChunk_msg ();
       Control.indent ();
       Control.unindent ())

end
