(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor amd64Translate(S: AMD64_TRANSLATE_STRUCTS): AMD64_TRANSLATE =
struct

  open S

  val tracerTop = amd64.tracerTop

  fun argsToString(ss: string list): string
    = "(" ^ (concat (List.separate(ss, ", "))) ^ ")"

  structure Machine = amd64MLton.Machine

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

        fun toAMD64Operand (g: t) : (amd64.Operand.t * amd64.Size.t) vector =
           let
              val ty = Machine.Type.toCType (ty g)
              val index = index g
              val base =
                 amd64.Immediate.label
                 (if isRoot g
                     then amd64MLton.global_base ty
                  else amd64MLton.globalObjptrNonRoot_base)
              val origin =
                 amd64.MemLoc.imm
                 {base = base,
                  index = amd64.Immediate.int index,
                  scale = amd64.Scale.fromCType ty,
                  size = amd64.Size.BYTE,
                  class = amd64MLton.Classes.Globals}
              val sizes = amd64.Size.fromCType ty
           in
              (#1 o Vector.mapAndFold)
              (sizes, 0, fn (size,offset) =>
               (((amd64.Operand.memloc o amd64.MemLoc.shift)
                 {origin = origin,
                  disp = amd64.Immediate.int offset,
                  scale = amd64.Scale.One,
                  size = size}, size), offset + amd64.Size.toBytes size))
           end
     end

  structure Operand =
    struct
      open Machine.Operand

      fun get (f: ('a * 'b) -> 'c) (i: int) (v: ('a * 'b) vector) =
         f (Vector.sub (v, i))
      fun getOp0 v =
         get #1 0 v

      val rec toAMD64Operand : t -> (amd64.Operand.t * amd64.Size.t) vector =
         fn ArrayOffset {base, index, offset, scale, ty}
            => let
                  val base = toAMD64Operand base
                  val _ = Assert.assert("amd64Translate.Operand.toAMD64Operand: Array/base",
                                        fn () => Vector.length base = 1)
                  val base = getOp0 base
                  val index = toAMD64Operand index
                  val _ = Assert.assert("amd64Translate.Operand.toAMD64Operand: Array/index",
                                       fn () => Vector.length index = 1)
                  val index = getOp0 index
                  val scale =
                     case scale of
                        Scale.One => amd64.Scale.One
                      | Scale.Two => amd64.Scale.Two
                      | Scale.Four => amd64.Scale.Four
                      | Scale.Eight => amd64.Scale.Eight
                  val ty = Type.toCType ty
                  val origin =
                     case (amd64.Operand.deMemloc base,
                           amd64.Operand.deImmediate index,
                           amd64.Operand.deMemloc index) of
                        (SOME base, SOME index, _) =>
                           amd64.MemLoc.simple 
                           {base = base,
                            index = index,
                            scale = scale,
                            size = amd64.Size.BYTE,
                            class = amd64MLton.Classes.Heap}
                      | (SOME base, _, SOME index) =>
                           amd64.MemLoc.complex 
                           {base = base,
                            index = index,
                            scale = scale,
                            size = amd64.Size.BYTE,
                            class = amd64MLton.Classes.Heap}
                      | _ => Error.bug (concat ["amd64Translate.Operand.toAMD64Operand: ",
                                                "strange Offset: base: ",
                                                amd64.Operand.toString base,
                                                " index: ",
                                                amd64.Operand.toString index])
                  val origin =
                     if Bytes.isZero offset
                        then origin
                        else amd64.MemLoc.shift
                             {origin = origin,
                              disp = amd64.Immediate.int (Bytes.toInt offset),
                              scale = amd64.Scale.One,
                              size = amd64.Size.BYTE}
                  val sizes = amd64.Size.fromCType ty
               in
                  (#1 o Vector.mapAndFold)
                  (sizes, 0, fn (size,offset) =>
                   (((amd64.Operand.memloc o amd64.MemLoc.shift)
                     {origin = origin,
                      disp = amd64.Immediate.int offset,
                      scale = amd64.Scale.One,
                      size = size}, size), offset + amd64.Size.toBytes size))
               end
          | Cast (z, _) => toAMD64Operand z
          | Contents {oper, ty} =>
               let
                  val ty = Type.toCType ty
                  val base = toAMD64Operand oper
                  val _ = Assert.assert("amd64Translate.Operand.toAMD64Operand: Contents/base",
                                        fn () => Vector.length base = 1)
                  val base = getOp0 base
                  val origin =
                     case amd64.Operand.deMemloc base of
                        SOME base =>
                           amd64.MemLoc.simple 
                           {base = base,
                            index = amd64.Immediate.zero,
                            scale = amd64.Scale.One,
                            size = amd64.Size.BYTE,
                            class = amd64MLton.Classes.Heap}
                      | _ => Error.bug (concat
                                        ["amd64Translate.Operand.toAMD64Operand: ",
                                         "strange Contents: base: ",
                                         amd64.Operand.toString base])    
                  val sizes = amd64.Size.fromCType ty
               in
                  (#1 o Vector.mapAndFold)
                  (sizes, 0, fn (size,offset) =>
                   (((amd64.Operand.memloc o amd64.MemLoc.shift)
                     {origin = origin,
                      disp = amd64.Immediate.int offset,
                      scale = amd64.Scale.One,
                      size = size}, size), offset + amd64.Size.toBytes size))
               end
          | Frontier => 
               let 
                  val frontier = amd64MLton.gcState_frontierContentsOperand ()
               in
                  Vector.new1 (frontier, valOf (amd64.Operand.size frontier))
               end
          | GCState => 
               Vector.new1 (amd64.Operand.label amd64MLton.gcState_label,
                            amd64MLton.pointerSize)
          | Global g => Global.toAMD64Operand g
          | Label l => 
               Vector.new1 (amd64.Operand.immediate_label l, amd64MLton.pointerSize)
          | Null => 
               Vector.new1 (amd64.Operand.immediate_zero, amd64MLton.wordSize)
          | Offset {base = GCState, offset, ty} =>
               let
                  val offset = Bytes.toInt offset
                  val ty = Type.toCType ty
                  val offset = amd64MLton.gcState_offset {offset = offset, ty = ty}
               in
                  Vector.new1 (offset, valOf (amd64.Operand.size offset))
               end
          | Offset {base, offset, ty} =>
               let
                  val offset = Bytes.toInt offset
                 val ty = Type.toCType ty
                 val base = toAMD64Operand base
                 val _ = Assert.assert("amd64Translate.Operand.toAMD64Operand: Offset/base",
                                       fn () => Vector.length base = 1)
                 val base = getOp0 base
                 val origin =
                   case amd64.Operand.deMemloc base of
                     SOME base =>
                       amd64.MemLoc.simple 
                       {base = base,
                        index = amd64.Immediate.int offset,
                        scale = amd64.Scale.One,
                        size = amd64.Size.BYTE,
                        class = amd64MLton.Classes.Heap}
                   | _ => Error.bug (concat ["amd64Translate.Operand.toAMD64Operand: ",
                                             "strange Offset: base: ",
                                             amd64.Operand.toString base])
                  val sizes = amd64.Size.fromCType ty
               in
                  (#1 o Vector.mapAndFold)
                  (sizes, 0, fn (size,offset) =>
                   (((amd64.Operand.memloc o amd64.MemLoc.shift)
                     {origin = origin,
                      disp = amd64.Immediate.int offset,
                      scale = amd64.Scale.One,
                      size = size}, size), offset + amd64.Size.toBytes size))
               end
          | Real _ => Error.bug "amd64Translate.Operand.toAMD64Operand: Real unimplemented"
          | Register r =>
               let
                  val ty = Machine.Type.toCType (Register.ty r)
                  val index = Machine.Register.index r
                  val base = amd64.Immediate.label (amd64MLton.local_base ty)
                  val origin =
                     amd64.MemLoc.imm
                     {base = base,
                      index = amd64.Immediate.int index,
                      scale = amd64.Scale.fromCType ty,
                      size = amd64.Size.BYTE,
                      class = amd64MLton.Classes.Locals}
                  val sizes = amd64.Size.fromCType ty
               in
                  (#1 o Vector.mapAndFold)
                  (sizes, 0, fn (size,offset) =>
                   (((amd64.Operand.memloc o amd64.MemLoc.shift)
                     {origin = origin,
                      disp = amd64.Immediate.int offset,
                      scale = amd64.Scale.One,
                      size = size}, size), offset + amd64.Size.toBytes size))
               end
          | StackOffset (StackOffset.T {offset, ty}) =>
               let
                  val offset = Bytes.toInt offset
                  val ty = Type.toCType ty
                  val origin =
                     amd64.MemLoc.simple 
                     {base = amd64MLton.gcState_stackTopContents (), 
                      index = amd64.Immediate.int offset,
                      scale = amd64.Scale.One,
                      size = amd64.Size.BYTE,
                      class = amd64MLton.Classes.Stack}
                  val sizes = amd64.Size.fromCType ty
               in
                  (#1 o Vector.mapAndFold)
                  (sizes, 0, fn (size,offset) =>
                   (((amd64.Operand.memloc o amd64.MemLoc.shift)
                     {origin = origin,
                      disp = amd64.Immediate.int offset,
                      scale = amd64.Scale.One,
                      size = size}, size), offset + amd64.Size.toBytes size))
               end
          | StackTop => 
               let 
                  val stackTop = amd64MLton.gcState_stackTopContentsOperand ()
               in
                  Vector.new1 (stackTop, valOf (amd64.Operand.size stackTop))
               end
          | Word w =>
               let
                  fun single size =
                     Vector.new1 (amd64.Operand.immediate_word w, size)
               in
                  case WordSize.prim (WordX.size w) of
                     W8 => single amd64.Size.BYTE
                   | W16 => single amd64.Size.WORD
                   | W32 => single amd64.Size.LONG
                   | W64 => single amd64.Size.QUAD
               end
    end

  type transInfo = amd64MLton.transInfo

  structure Entry =
    struct
      structure Kind = Machine.Kind

      fun toAMD64Blocks {label, kind, 
                       transInfo as {frameInfoToAMD64, live, liveInfo,
                                     ...}: transInfo}
        = (
           amd64Liveness.LiveInfo.setLiveOperands
           (liveInfo, label, live label);
           case kind
             of Kind.Jump
              => let
                 in
                   AppendList.single
                   (amd64.Block.mkBlock'
                    {entry = SOME (amd64.Entry.jump {label = label}),
                     statements = [],
                     transfer = NONE})
                 end
              | Kind.Func
              => let
                   val args
                     = List.fold
                       (live label,
                        amd64.MemLocSet.empty,
                        fn (operand, args)
                         => case amd64.Operand.deMemloc operand
                              of SOME memloc => amd64.MemLocSet.add(args, memloc)
                               | NONE => args)
                 in
                   AppendList.single
                   (amd64.Block.mkBlock'
                    {entry = SOME (amd64.Entry.func {label = label,
                                                   live = args}),
                     statements = [],
                     transfer = NONE})
                 end
              | Kind.Cont {args, frameInfo, ...}
              => let
                    val frameInfo = frameInfoToAMD64 frameInfo
                    val args =
                       Vector.fold
                       (args, amd64.MemLocSet.empty,
                        fn (operand,args) =>
                        Vector.fold
                        (Operand.toAMD64Operand (Live.toOperand operand), args,
                         fn ((operand,_),args) =>
                         case amd64.Operand.deMemloc operand of
                            SOME memloc => amd64.MemLocSet.add(args, memloc)
                          | NONE => args))
                 in
                   AppendList.single
                   (amd64.Block.mkBlock'
                    {entry = SOME (amd64.Entry.cont {label = label,
                                                   live = args,
                                                   frameInfo = frameInfo}),
                     statements = [],
                     transfer = NONE})
                 end
              | Kind.Handler {frameInfo, ...}
              => let
                 in 
                   AppendList.single
                   (amd64.Block.mkBlock'
                    {entry = SOME (amd64.Entry.handler
                                   {frameInfo = frameInfoToAMD64 frameInfo,
                                    label = label,
                                    live = amd64.MemLocSet.empty}),
                     statements = [],
                     transfer = NONE})
                 end
              | Kind.CReturn {dst, frameInfo, func}
              => let
                   val dsts =
                      case dst of
                         NONE => Vector.new0 ()
                       | SOME dst => Operand.toAMD64Operand (Live.toOperand dst)
                 in
                   amd64MLton.creturn
                   {dsts = dsts,
                    frameInfo = Option.map (frameInfo, frameInfoToAMD64),
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
                    (amd64.Block.mkBlock'
                     {entry = NONE,
                      statements = [amd64.Assembly.comment
                                    (concat ["begin: ",
                                             comment])],
                      transfer = NONE}),
                    AppendList.single
                    (amd64.Block.mkBlock'
                     {entry = NONE,
                      statements = [amd64.Assembly.comment
                                    (concat ["end: ",
                                             comment])],
                      transfer = NONE}))
                 end
            else (AppendList.empty,AppendList.empty)

      fun toAMD64Blocks {statement,
                       transInfo as {...} : transInfo}
        = (case statement
             of Noop
              => AppendList.empty
              | Move {src, dst}
              => let
                   val (comment_begin,
                        comment_end) = comments statement

                   val dsts = Operand.toAMD64Operand dst
                   val srcs = Operand.toAMD64Operand src
                   (* Operand.toAMD64Operand returns multi-word 
                    * operands in and they will be moved in order,
                    * so it suffices to check for aliasing between 
                    * the first dst and second src.
                    *)
                   val (dsts,srcs) =
                      if Vector.length srcs > 1
                         andalso amd64.Operand.mayAlias
                                 (#1 (Vector.sub (dsts, 0)), 
                                  #1 (Vector.sub (srcs, 1)))
                         then (Vector.rev dsts, Vector.rev srcs)
                         else (dsts,srcs)
                 in
                   AppendList.appends
                   [comment_begin,
                    AppendList.single
                    (amd64.Block.mkBlock'
                     {entry = NONE,
                      statements
                      = (Vector.toList o Vector.map2)
                        (dsts,srcs,fn ((dst,_),(src,srcsize)) =>
                         (* dst = src *)
                         case amd64.Size.class srcsize
                            of amd64.Size.INT => amd64.Assembly.instruction_mov 
                                                 {dst = dst,
                                                  src = src,
                                                  size = srcsize}
                          | amd64.Size.FLT => amd64.Assembly.instruction_sse_movs
                                              {dst = dst,
                                               src = src,
                                               size = srcsize}),
                      transfer = NONE}),
                    comment_end]
                 end 
              | PrimApp {dst, prim, args}
              => let
                   val (comment_begin, comment_end) = comments statement
                   val args = (Vector.concatV o Vector.map)
                              (args, Operand.toAMD64Operand)
                   val dsts = 
                      case dst of
                         NONE => Vector.new0 ()
                       | SOME dst => Operand.toAMD64Operand dst
                 in
                   AppendList.appends
                   [comment_begin,
                    (amd64MLton.prim {prim = prim,
                                    args = args,
                                    dsts = dsts,
                                    transInfo = transInfo}),
                    comment_end]
                 end
              | ProfileLabel l =>
                   AppendList.single
                   (amd64.Block.mkProfileBlock'
                    {profileLabel = l}))
    end

  structure Transfer =
    struct
      open Machine.Transfer

      fun goto l
        = AppendList.single
          (amd64.Block.mkBlock'
           {entry = NONE,
            statements = [],
            transfer = SOME (amd64.Transfer.goto
                             {target = l})})

      fun iff (test, a, b)
        = let
            val (test,testsize) =
               Vector.sub (Operand.toAMD64Operand test, 0)
          in
            if Label.equals(a, b)
              then AppendList.single
                   (amd64.Block.mkBlock'
                    {entry = NONE,
                     statements = [],
                     transfer = SOME (amd64.Transfer.goto {target = a})})
              else AppendList.single
                   ((* if (test) goto a
                     * goto b
                     *)
                    amd64.Block.mkBlock'
                    {entry = NONE,
                     statements 
                     = [amd64.Assembly.instruction_test
                        {src1 = test,
                         src2 = test,
                         size = testsize}],
                     transfer
                     = SOME (amd64.Transfer.iff
                             {condition = amd64.Instruction.NZ,
                              truee = a,
                              falsee = b})})
          end

      fun cmp (test, k, a, b)
        = let
            val (test,testsize) =
               Vector.sub (Operand.toAMD64Operand test, 0)
          in
            if Label.equals(a, b)
              then AppendList.single
                   (amd64.Block.mkBlock'
                    {entry = NONE,
                     statements = [],
                     transfer = SOME (amd64.Transfer.goto {target = a})})
              else AppendList.single
                   ((* if (test = k) goto a
                     * goto b
                     *)
                    amd64.Block.mkBlock'
                    {entry = NONE,
                     statements 
                     = [amd64.Assembly.instruction_cmp
                        {src1 = test,
                         src2 = amd64.Operand.immediate k,
                         size = testsize}],
                     transfer
                     = SOME (amd64.Transfer.iff
                             {condition = amd64.Instruction.E,
                              truee = a,
                              falsee = b})})
          end

      fun switch(test, cases, default)
        = let
            val test = Operand.toAMD64Operand test
            val (test,_) = Vector.sub(test, 0)
          in
            AppendList.single
            (amd64.Block.mkBlock'
             {entry = NONE,
              statements = [],
              transfer = SOME (amd64.Transfer.switch
                               {test = test,
                                cases = cases,
                                default = default})})
          end

      fun doSwitchWord (test, cases, default)
        = (case (cases, default)
             of ([],            NONE)
              => Error.bug "amd64Translate.Transfer.doSwitchWord"
              | ([(_,l)],       NONE) => goto l
              | ([],            SOME l) => goto l
              | ([(w1,l1),(w2,l2)], NONE) => 
                if WordX.isZero w1 andalso WordX.isOne w2
                   then iff(test,l2,l1)
                else if WordX.isZero w2 andalso WordX.isOne w1
                   then iff(test,l1,l2)
                else cmp(test,amd64.Immediate.word w1,l1,l2)
              | ([(k',l')],      SOME l)
              => cmp(test,amd64.Immediate.word k',l',l)
              | ((_,l)::cases,  NONE) 
              => switch(test, amd64.Transfer.Cases.word cases, l)
              | (cases,         SOME l) 
              => switch(test, amd64.Transfer.Cases.word cases, l))

      fun comments transfer
        = if !Control.Native.commented > 0
            then let
                   val comment = (Layout.toString o layout) transfer
                 in
                   AppendList.single
                   (amd64.Block.mkBlock'
                    {entry = NONE,
                     statements = [amd64.Assembly.comment comment],
                      transfer = NONE})
                 end
            else AppendList.empty


      fun toAMD64Blocks {returns, transfer,
                       transInfo as {frameInfoToAMD64, ...}: transInfo}
        = (case transfer
             of Arith {prim, args, dst, overflow, success, ...}
              => let
                   val args = (Vector.concatV o Vector.map)
                              (args, Operand.toAMD64Operand)
                   val dsts = Operand.toAMD64Operand dst
                 in
                   AppendList.append
                   (comments transfer,
                    amd64MLton.arith {prim = prim,
                                    args = args,
                                    dsts = dsts,
                                    overflow = overflow,
                                    success = success,
                                    transInfo = transInfo})
                 end
              | CCall {args, frameInfo, func, return}
              => let
                   val args = (Vector.concatV o Vector.map)
                              (args, Operand.toAMD64Operand)
                 in
                   AppendList.append
                   (comments transfer,  
                    amd64MLton.ccall {args = args,
                                    frameInfo = (Option.map
                                                 (frameInfo, frameInfoToAMD64)),
                                    func = func,
                                    return = return,
                                    transInfo = transInfo})
                 end
              | Return
              => AppendList.append
                 (comments transfer,
                  AppendList.single
                  (amd64.Block.mkBlock'
                   {entry = NONE,
                    statements = [],
                    transfer 
                    = SOME (amd64.Transfer.return 
                            {live 
                             = Vector.fold
                               ((case returns of
                                    NONE => Error.bug "amd64Translate.Transfer.toAMD64Blocsk: Return"
                                  | SOME zs => zs),
                                amd64.MemLocSet.empty,
                                fn (operand, live) =>
                                Vector.fold
                                (Operand.toAMD64Operand operand, live,
                                 fn ((operand,_),live) =>
                                 case amd64.Operand.deMemloc operand of
                                    SOME memloc => amd64.MemLocSet.add(live, memloc)
                                  | NONE => live))})}))
              | Raise
              => AppendList.append
                 (comments transfer,
                  AppendList.single
                  (amd64.Block.mkBlock'
                   {entry = NONE,
                    statements = [],
                    transfer 
                    = SOME (amd64.Transfer.raisee 
                            {live 
                             = amd64.MemLocSet.add
                               (amd64.MemLocSet.add
                                (amd64.MemLocSet.empty,
                                 amd64MLton.gcState_stackBottomContents ()),
                                amd64MLton.gcState_exnStackContents ())})}))
              | Switch (Machine.Switch.T {cases, default, test, ...})
              => AppendList.append
                 (comments transfer,
                  doSwitchWord (test, Vector.toList cases, default))
              | Goto label
              => (AppendList.append
                  (comments transfer,
                   AppendList.single
                   ((* goto label *)
                    amd64.Block.mkBlock'
                    {entry = NONE,
                     statements = [],
                     transfer = SOME (amd64.Transfer.goto {target = label})})))
              | Call {label, live, return, ...}
              => let
                    val live =
                       Vector.fold
                       (live, amd64.MemLocSet.empty, fn (operand, live) =>
                        Vector.fold
                        (Operand.toAMD64Operand (Live.toOperand operand), live,
                         fn ((operand, _), live) =>
                         case amd64.Operand.deMemloc operand of
                            NONE => live
                          | SOME memloc => amd64.MemLocSet.add (live, memloc)))
                    val com = comments transfer
                    val transfer =
                       case return of
                          NONE => amd64.Transfer.tail {target = label,
                                                     live = live}
                        | SOME {return, handler, size} =>
                             amd64.Transfer.nontail {target = label,
                                                   live = live,
                                                   return = return,
                                                   handler = handler,
                                                   size = Bytes.toInt size}
                 in
                    AppendList.append
                    (com,
                     AppendList.single
                     (amd64.Block.mkBlock' {entry = NONE,
                                    statements = [],
                                    transfer = SOME transfer}))
                 end)
    end

  structure Block =
    struct
      open Machine.Block

      fun toAMD64Blocks {block = T {label, 
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
                 (Entry.toAMD64Blocks {label = label,
                                     kind = kind,
                                     transInfo = transInfo},
                  amd64.Block.mkBlock'
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
                              [amd64.Assembly.comment comment]
                            end
                       else [],
                    transfer = NONE}),
                 Vector.foldr(statements,
                              (Transfer.toAMD64Blocks
                               {returns = (Option.map
                                           (returns, fn v =>
                                            Vector.map (v, Live.toOperand))),
                                transfer = transfer,
                                transInfo = transInfo}),
                              fn (statement,l)
                               => AppendList.append
                                  (Statement.toAMD64Blocks 
                                   {statement = statement,
                                    transInfo = transInfo}, l)))

            val pseudo_blocks = AppendList.toList pseudo_blocks

            val blocks = amd64.Block.compress pseudo_blocks
          in
            blocks
          end
    end

  structure Chunk =
    struct
      open Machine.Chunk

      fun toAMD64Chunk {chunk = T {blocks, ...}, 
                      frameInfoToAMD64,
                      liveInfo}
        = let
            val data = ref []
            val addData = fn l => List.push (data, l)
            val {get = live : Label.t -> amd64.Operand.t list,
                 set = setLive, 
                 rem = remLive, ...}
              = Property.getSetOnce
                (Label.plist, Property.initRaise ("live", Label.layout))
            val _ = Vector.foreach
                    (blocks, fn Block.T {label, live, ...} =>
                     setLive (label,
                              (Vector.toList o #1 o Vector.unzip o 
                               Vector.concatV o Vector.map)
                              (live, Operand.toAMD64Operand o Live.toOperand)))
            val transInfo = {addData = addData,
                             frameInfoToAMD64 = frameInfoToAMD64,
                             live = live,
                             liveInfo = liveInfo}
            val amd64Blocks 
              = List.concat (Vector.toListMap
                             (blocks, 
                                fn block
                                 => Block.toAMD64Blocks 
                                    {block = block,
                                     transInfo = transInfo}))
            val _ = Vector.foreach (blocks, fn Block.T {label, ...} =>
                                    remLive label)
            val data = List.concatRev (!data)
            val data =
               if List.isEmpty data
                  then []
                  else (amd64.Assembly.pseudoop_data())::data
          in
            amd64.Chunk.T {data = data, blocks = amd64Blocks}
          end
    end

  fun translateChunk {chunk: amd64MLton.Machine.Chunk.t,
                      frameInfoToAMD64,
                      liveInfo: amd64Liveness.LiveInfo.t}:
                     {chunk: amd64.Chunk.t}
    = {chunk = Chunk.toAMD64Chunk {chunk = chunk,
                                 frameInfoToAMD64 = frameInfoToAMD64,
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
