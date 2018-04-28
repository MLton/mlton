(* Copyright (C) 2009 Matthew Fluet.
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
     structure Label = Label
     structure Live = Live
     structure Register = Register
     structure Scale = Scale
     structure StackOffset = StackOffset
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
                 x86.Immediate.label
                 (if isRoot g
                     then x86MLton.global_base ty
                  else x86MLton.globalObjptrNonRoot_base)
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

  structure Operand =
    struct
      open Machine.Operand

      fun get (f: ('a * 'b) -> 'c) (i: int) (v: ('a * 'b) vector) =
         f (Vector.sub (v, i))
      fun getOp0 v =
         get #1 0 v

      val rec toX86Operand : t -> (x86.Operand.t * x86.Size.t) vector =
         fn ArrayOffset {base, index, offset, scale, ty}
            => let
                  val base = toX86Operand base
                  val _ = Assert.assert("x86Translate.Operand.toX86Operand: Array/base",
                                        fn () => Vector.length base = 1)
                  val base = getOp0 base
                  val index = toX86Operand index
                  val _ = Assert.assert("x86Translate.Operand.toX86Operand: Array/index",
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
                     case (x86.Operand.deMemloc base,
                           x86.Operand.deImmediate index,
                           x86.Operand.deMemloc index) of
                        (SOME base, SOME index, _) =>
                           x86.MemLoc.simple 
                           {base = base,
                            index = index,
                            scale = scale,
                            size = x86.Size.BYTE,
                            class = x86MLton.Classes.Heap}
                      | (SOME base, _, SOME index) =>
                           x86.MemLoc.complex 
                           {base = base,
                            index = index,
                            scale = scale,
                            size = x86.Size.BYTE,
                            class = x86MLton.Classes.Heap}
                      | _ => Error.bug (concat ["x86Translate.Operand.toX86Operand: ",
                                                "strange Offset: base: ",
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
                  (#1 o Vector.mapAndFold)
                  (sizes, 0, fn (size,offset) =>
                   (((x86.Operand.memloc o x86.MemLoc.shift)
                     {origin = origin,
                      disp = x86.Immediate.int offset,
                      scale = x86.Scale.One,
                      size = size}, size), offset + x86.Size.toBytes size))
               end
          | Cast (z, _) => toX86Operand z
          | Contents {oper, ty} =>
               let
                  val ty = Type.toCType ty
                  val base = toX86Operand oper
                  val _ = Assert.assert("x86Translate.Operand.toX86Operand: Contents/base",
                                        fn () => Vector.length base = 1)
                  val base = getOp0 base
                  val origin =
                     case x86.Operand.deMemloc base of
                        SOME base =>
                           x86.MemLoc.simple 
                           {base = base,
                            index = x86.Immediate.zero,
                            scale = x86.Scale.One,
                            size = x86.Size.BYTE,
                            class = x86MLton.Classes.Heap}
                      | _ => Error.bug (concat
                                        ["x86Translate.Operand.toX86Operand: ",
                                         "strange Contents: base: ",
                                         x86.Operand.toString base])    
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
          | Null => 
               Vector.new1 (x86.Operand.immediate_zero, x86MLton.wordSize)
          | Offset {base = GCState, offset, ty} =>
               let
                  val offset = Bytes.toInt offset
                  val ty = Type.toCType ty
                  val offset = x86MLton.gcState_offset {offset = offset, ty = ty}
               in
                  Vector.new1 (offset, valOf (x86.Operand.size offset))
               end
          | Offset {base, offset, ty} =>
               let
                  val offset = Bytes.toInt offset
                 val ty = Type.toCType ty
                 val base = toX86Operand base
                 val _ = Assert.assert("x86Translate.Operand.toX86Operand: Offset/base",
                                       fn () => Vector.length base = 1)
                 val base = getOp0 base
                 val origin =
                   case x86.Operand.deMemloc base of
                     SOME base =>
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
                  (#1 o Vector.mapAndFold)
                  (sizes, 0, fn (size,offset) =>
                   (((x86.Operand.memloc o x86.MemLoc.shift)
                     {origin = origin,
                      disp = x86.Immediate.int offset,
                      scale = x86.Scale.One,
                      size = size}, size), offset + x86.Size.toBytes size))
               end
          | Real _ => Error.bug "x86Translate.Operand.toX86Operand: Real unimplemented"
          | Register r =>
               let
                  val ty = Machine.Type.toCType (Register.ty r)
                  val index = Machine.Register.index r
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
                  (#1 o Vector.mapAndFold)
                  (sizes, 0, fn (size,offset) =>
                   (((x86.Operand.memloc o x86.MemLoc.shift)
                     {origin = origin,
                      disp = x86.Immediate.int offset,
                      scale = x86.Scale.One,
                      size = size}, size), offset + x86.Size.toBytes size))
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
                  (#1 o Vector.mapAndFold)
                  (sizes, 0, fn (size,offset) =>
                   (((x86.Operand.memloc o x86.MemLoc.shift)
                     {origin = origin,
                      disp = x86.Immediate.int offset,
                      scale = x86.Scale.One,
                      size = size}, size), offset + x86.Size.toBytes size))
               end
          | StackTop => 
               let 
                  val stackTop = x86MLton.gcState_stackTopContentsOperand ()
               in
                  Vector.new1 (stackTop, valOf (x86.Operand.size stackTop))
               end
          | Word w =>
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
    end

  type transInfo = x86MLton.transInfo

  structure Entry =
    struct
      structure Kind = Machine.Kind

      fun toX86Blocks {label, kind, 
                       transInfo as {frameInfoToX86, live, liveInfo,
                                     ...}: transInfo}
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
              | Kind.Func
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
                        (Operand.toX86Operand (Live.toOperand operand), args,
                         fn ((operand,_),args) =>
                         case x86.Operand.deMemloc operand of
                            SOME memloc => x86.MemLocSet.add(args, memloc)
                          | NONE => args))
                 in
                   AppendList.single
                   (x86.Block.mkBlock'
                    {entry = SOME (x86.Entry.cont {label = label,
                                                   live = args,
                                                   frameInfo = frameInfo}),
                     statements = [],
                     transfer = NONE})
                 end
              | Kind.Handler {frameInfo, ...}
              => let
                 in 
                   AppendList.single
                   (x86.Block.mkBlock'
                    {entry = SOME (x86.Entry.handler
                                   {frameInfo = frameInfoToX86 frameInfo,
                                    label = label,
                                    live = x86.MemLocSet.empty}),
                     statements = [],
                     transfer = NONE})
                 end
              | Kind.CReturn {dst, frameInfo, func}
              => let
                   val dsts =
                      case dst of
                         NONE => Vector.new0 ()
                       | SOME dst => Operand.toX86Operand (Live.toOperand dst)
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
        = if !Control.Native.commented > 0
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
             of Noop
              => AppendList.empty
              | Move {src, dst}
              => let
                   val (comment_begin,
                        comment_end) = comments statement

                   val dsts = Operand.toX86Operand dst
                   val srcs = Operand.toX86Operand src
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
                              (args, Operand.toX86Operand)
                   val dsts = 
                      case dst of
                         NONE => Vector.new0 ()
                       | SOME dst => Operand.toX86Operand dst
                 in
                   AppendList.appends
                   [comment_begin,
                    (x86MLton.prim {prim = prim,
                                    args = args,
                                    dsts = dsts,
                                    transInfo = transInfo}),
                    comment_end]
                 end
              | ProfileLabel l =>
                   AppendList.single
                   (x86.Block.mkProfileBlock'
                    {profileLabel = l}))
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

      fun iff (test, a, b)
        = let
            val (test,testsize) =
               Vector.sub (Operand.toX86Operand test, 0)
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

      fun cmp (test, k, a, b)
        = let
            val (test,testsize) =
               Vector.sub (Operand.toX86Operand test, 0)
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

      fun switch(test, cases, default)
        = let
            val test = Operand.toX86Operand test
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

      fun doSwitchWord (test, cases, default)
        = (case (cases, default)
             of ([],            NONE)
              => Error.bug "x86Translate.Transfer.doSwitchWord"
              | ([(_,l)],       NONE) => goto l
              | ([],            SOME l) => goto l
              | ([(w1,l1),(w2,l2)], NONE) => 
                if WordX.isZero w1 andalso WordX.isOne w2
                   then iff(test,l2,l1)
                else if WordX.isZero w2 andalso WordX.isOne w1
                   then iff(test,l1,l2)
                else cmp(test,x86.Immediate.word w1,l1,l2)
              | ([(k',l')],      SOME l)
              => cmp(test,x86.Immediate.word k',l',l)
              | ((_,l)::cases,  NONE) 
              => switch(test, x86.Transfer.Cases.word cases, l)
              | (cases,         SOME l) 
              => switch(test, x86.Transfer.Cases.word cases, l))

      fun comments transfer
        = if !Control.Native.commented > 0
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


      fun toX86Blocks {returns, transfer,
                       transInfo as {frameInfoToX86, ...}: transInfo}
        = (case transfer
             of Arith {prim, args, dst, overflow, success, ...}
              => let
                   val args = (Vector.concatV o Vector.map)
                              (args, Operand.toX86Operand)
                   val dsts = Operand.toX86Operand dst
                 in
                   AppendList.append
                   (comments transfer,
                    x86MLton.arith {prim = prim,
                                    args = args,
                                    dsts = dsts,
                                    overflow = overflow,
                                    success = success,
                                    transInfo = transInfo})
                 end
              | CCall {args, frameInfo, func, return}
              => let
                   val args = (Vector.concatV o Vector.map)
                              (args, Operand.toX86Operand)
                 in
                   AppendList.append
                   (comments transfer,  
                    x86MLton.ccall {args = args,
                                    frameInfo = (Option.map
                                                 (frameInfo, frameInfoToX86)),
                                    func = func,
                                    return = return,
                                    transInfo = transInfo})
                 end
              | Return
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
                                (Operand.toX86Operand operand, live,
                                 fn ((operand,_),live) =>
                                 case x86.Operand.deMemloc operand of
                                    SOME memloc => x86.MemLocSet.add(live, memloc)
                                  | NONE => live))})}))
              | Raise
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
                  doSwitchWord (test, Vector.toList cases, default))
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
                        (Operand.toX86Operand (Live.toOperand operand), live,
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
                   = if !Control.Native.commented > 0
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
                      frameInfoToX86,
                      liveInfo}
        = let
            val data = ref []
            val addData = fn l => List.push (data, l)
            val {get = live : Label.t -> x86.Operand.t list,
                 set = setLive, 
                 rem = remLive, ...}
              = Property.getSetOnce
                (Label.plist, Property.initRaise ("live", Label.layout))
            val _ = Vector.foreach
                    (blocks, fn Block.T {label, live, ...} =>
                     setLive (label,
                              (Vector.toList o #1 o Vector.unzip o 
                               Vector.concatV o Vector.map)
                              (live, Operand.toX86Operand o Live.toOperand)))
            val transInfo = {addData = addData,
                             frameInfoToX86 = frameInfoToX86,
                             live = live,
                             liveInfo = liveInfo}
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
                      frameInfoToX86,
                      liveInfo: x86Liveness.LiveInfo.t}:
                     {chunk: x86.Chunk.t}
    = {chunk = Chunk.toX86Chunk {chunk = chunk,
                                 frameInfoToX86 = frameInfoToX86,
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
