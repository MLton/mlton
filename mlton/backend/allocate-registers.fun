(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor AllocateRegisters (S: ALLOCATE_REGISTERS_STRUCTS): ALLOCATE_REGISTERS = 
struct

open S

structure R = Rssa

local
   open Rssa
in
   structure Func = Func
   structure Function = Function
   structure Kind = Kind
   structure Label = Label
   structure Type = Type
   structure Var = Var
end

local
   open Machine
in
   structure CType = CType
   structure Operand = Operand
   structure Register = Register
   structure Runtime = Runtime
   structure StackOffset = StackOffset
end

structure Live = Live (Rssa)

structure Allocation:
   sig
      structure Stack:
         sig
            type t

            val get: t * Type.t -> t * {offset: Bytes.t}
            val layout: t -> Layout.t
            val new: StackOffset.t list -> t
            val size: t -> Bytes.t
         end

      type t

      val getRegister: t * Type.t -> Register.t
      val getStack: t * Type.t -> {offset: Bytes.t}
      val layout: t -> Layout.t
      val new: StackOffset.t list * Register.t list -> t
      val stack: t -> Stack.t
      val stackSize: t -> Bytes.t
   end =
   struct
       structure Stack =
       struct
          (* Keep a list of allocated slots sorted in increasing order of offset.
           *)
          datatype t = T of {offset: Bytes.t, size: Bytes.t} list

          fun layout (T alloc) =
             List.layout (fn {offset, size} =>
                          Layout.record [("offset", Bytes.layout offset),
                                         ("size", Bytes.layout size)])
                         alloc

          fun size (T alloc) =
             case alloc of
                [] => Bytes.zero
              | _ => let
                        val {offset, size} = List.last alloc
                     in
                        Bytes.+ (offset, size)
                     end

          fun new (alloc): t =
             let
                val a =
                   Array.fromListMap (alloc, fn StackOffset.T {offset, ty} =>
                                      {offset = offset,
                                       size = Type.bytes ty})
                val () =
                   QuickSort.sortArray
                   (a, fn (r, r') => Bytes.<= (#offset r, #offset r'))
                fun loop (alloc, ac) =
                   case alloc of
                      [] => List.rev ac
                    | [a] => List.rev (a::ac)
                    | (a1 as {offset = offset1, size = size1})::(a2 as {offset = offset2, size = size2})::alloc =>
                         if Bytes.equals (Bytes.+ (offset1, size1), offset2)
                            then loop ({offset = offset1, size = Bytes.+ (size1, size2)}::alloc, ac)
                            else loop (a2::alloc, a1::ac)
             in
                T (loop (Array.toList a, []))
             end

          fun get (T alloc, ty) =
             let
                val slotSize = Type.bytes ty
                fun loop (alloc, a as {offset, size}, ac) =
                   let
                      val prevEnd = Bytes.+ (offset, size)
                      val begin = Type.align (ty, prevEnd)
                      fun coalesce () =
                         if Bytes.equals (prevEnd, begin)
                            then ({offset = offset, size = Bytes.+ (size, slotSize)}, ac)
                            else ({offset = begin, size = slotSize}, a :: ac)
                   in
                      case alloc of
                         [] =>
                            let
                               val (a, ac) = coalesce ()
                            in
                               (T (rev (a :: ac)), {offset = begin})
                            end
                       | (a' as {offset, size}) :: alloc =>
                            if Bytes.> (Bytes.+ (begin, slotSize), offset)
                               then loop (alloc, a',
                                          if Bytes.isZero offset andalso Bytes.isZero size
                                             then ac
                                             else a :: ac)
                               else let
                                       val (a'' as {offset = o', size = s'}, ac) =
                                          coalesce ()
                                       val alloc =
                                          List.appendRev
                                          (ac,
                                           if Bytes.equals (Bytes.+ (o', s'), offset)
                                              then {offset = o', size = Bytes.+ (size, s')} :: alloc
                                              else a'' :: a' :: alloc)
                                    in
                                       (T alloc, {offset = begin})
                                    end
                   end
             in
                loop (alloc, {offset = Bytes.zero, size = Bytes.zero}, [])
             end
          val get =
             Trace.trace2
             ("AllocateRegisters.Allocation.Stack.get",
              layout, Type.layout,
              Layout.tuple2 (layout, fn {offset} =>
                             Layout.record [("offset", Bytes.layout offset)]))
             get
       end
       structure Registers =
       struct
          (* A register allocation keeps track of the registers that have
           * already been allocated, for each runtime type.  The reason that
           * we associate them with runtime types rather than Rssa types is
           * that the register indices that the codegens use are based on
           * runtime types.
           *)
          datatype t = T of CType.t -> {alloc: Register.t list,
                                        next: int} ref

          fun layout (T f) =
             List.layout
             (fn t =>
              let
                 val {alloc, next} = ! (f t)
              in
                 Layout.record [("ty", CType.layout t),
                                ("next", Int.layout next),
                                ("alloc", List.layout Register.layout alloc)]
              end)
             CType.all

          fun compress {next, alloc} =
             let
                fun loop (next, alloc) =
                   let
                      fun done () = {alloc = alloc,
                                     next = next}
                   in
                      case alloc of
                         [] => done ()
                       | r :: alloc =>
                            if next = Register.index r
                               then loop (next + 1, alloc)
                            else done ()
                   end
             in
                loop (next, alloc)
             end

          fun new (rs: Register.t list): t =
             let
                fun sameType (r, r') =
                   CType.equals
                   (Type.toCType (Register.ty r),
                    Type.toCType (Register.ty r'))
                val rss = List.equivalence (rs, sameType)
             in
                T (CType.memo
                   (fn t =>
                    case List.peek (rss, fn rs =>
                                    case rs of
                                       [] => false
                                     | r :: _ => 
                                          CType.equals
                                          (t, Type.toCType (Register.ty r))) of
                       NONE => ref {alloc = [], next = 0}
                     | SOME rs =>
                          ref
                          (compress
                           {next = 0,
                            alloc =
                            QuickSort.sortList
                            (rs, fn (r, r') =>
                             Register.index r <= Register.index r')})))
             end

          fun get (T f, ty: Type.t) =
             let
                val t = Type.toCType ty
                val r = f t
                val {alloc, next} = !r
                val reg = Register.new (ty, SOME next)
                val _ =
                   r := compress {alloc = alloc,
                                  next = next + 1}
             in
                reg
             end
       end

       datatype t = T of {registers: Registers.t,
                          stack: Stack.t ref}

       local
          fun make s (T x) = s x
       in
          val stack = ! o (make #stack)
          val stackSize = Stack.size o stack
       end

       fun layout (T {registers, stack}) =
          Layout.record
          [("stack", Stack.layout (!stack)),
           ("registers", Registers.layout registers)]

       fun getStack (T {stack, ...}, ty) =
          let
             val (s, offset) = Stack.get (!stack, ty)
             val _ = stack := s
          in
             offset
          end

       fun getRegister (T {registers, ...}, ty) =
          Registers.get (registers, ty)

       fun new (stack, registers) = 
          T {registers = Registers.new registers,
             stack = ref (Stack.new stack)}
   end

structure Info =
   struct
      type t = {live: Operand.t vector,
                liveNoFormals: Operand.t vector,
                size: Bytes.t}

      fun layout ({live, liveNoFormals, size, ...}: t) =
         Layout.record
         [("live", Vector.layout Operand.layout live),
          ("liveNoFormals", Vector.layout Operand.layout liveNoFormals),
          ("size", Bytes.layout size)]
   end

(* ------------------------------------------------- *)
(*                     allocate                      *)
(* ------------------------------------------------- *)

fun allocate {formalsStackOffsets,
              function = f: Rssa.Function.t,
              varInfo: Var.t -> {operand: Machine.Operand.t option ref option,
                                 ty: Type.t}} =
   let
      fun diagnostics f =
         Control.diagnostics
         (fn display =>
          let
             open Layout
             fun diagVar (x: Var.t): unit =
                display (seq
                         [Var.layout x, str " ",
                          Option.layout
                          (fn r => Option.layout Operand.layout (!r))
                          (#operand (varInfo x))])
             fun diagStatement (s: R.Statement.t): unit =
                R.Statement.foreachDef (s, diagVar o #1)
          in
             f (display, diagVar, diagStatement)
          end)
      val _ =
         Control.diagnostic (fn () =>
                             let open Layout
                             in seq [str "Function allocs for ",
                                     Func.layout (Function.name f)]
                             end)
      val {labelLive, remLabelLive} =
         Live.live (f, {shouldConsider = isSome o #operand o varInfo})
      val {args, blocks, name, ...} = Function.dest f
      (*
       * Decide which variables will live in stack slots and which
       * will live in registers.
       * Initially,
       *   - all formals are put in stack slots
       *   - everything else is put in a register.
       * Variables get moved to the stack if they are
       *   - live at the beginning of a Cont block; such variables are
       *     live while the frame is suspended during a non-tail call
       *     and must be stack allocated to be traced during a GC
       *   - live at the beginning of a CReturn block that mayGC; such
       *     variables are live while the frame is suspended during a
       *     C call and must be stack allocated to be traced during
       *     the potential GC
       * Both of the above are indiced by Kind.frameStyle kind =
       * Kind.OffsetsAndSize
       *)
      datatype place = Stack | Register
      val {get = place: Var.t -> place ref, rem = removePlace, ...} =
         Property.get (Var.plist, Property.initFun (fn _ => ref Register))
      (* !hasHandler = true iff handlers are installed in this function. *)
      val hasHandler: bool ref = ref false
      fun forceStack (x: Var.t): unit = place x := Stack
      val _ = Vector.foreach (args, forceStack o #1)
      val _ =
         Vector.foreach
         (blocks,
          fn R.Block.T {kind, label, statements, ...} =>
          let
             val {beginNoFormals, ...} = labelLive label
             val _ =
                case Kind.frameStyle kind of
                   Kind.None => ()
                 | Kind.OffsetsAndSize =>
                      Vector.foreach (beginNoFormals, forceStack)
                 | Kind.SizeOnly => ()
             val _ =
                if not (!hasHandler)
                   andalso (Vector.exists
                            (statements, fn s =>
                             let
                                datatype z = datatype R.Statement.t
                             in
                                case s of
                                   SetHandler _ => true
                                 | SetExnStackLocal => true
                                 | SetExnStackSlot => true
                                 | SetSlotExnStack => true
                                 | _ => false
                             end))
                   then hasHandler := true
                else ()
          in
             ()
          end)
      fun allocateVar (x: Var.t, a: Allocation.t): unit = 
         let
            val {operand, ty} = varInfo x
         in
            if isSome operand
               then let
                       val oper =
                          case ! (place x) of
                             Stack =>
                                let
                                   val {offset} = Allocation.getStack (a, ty)
                                in
                                   Operand.StackOffset
                                   (StackOffset.T {offset = offset, ty = ty})
                                end
                           | Register =>
                                Operand.Register
                                (Allocation.getRegister (a, ty))
                       val () = removePlace x
                       val _ = 
                          case operand of
                             NONE => ()
                           | SOME r => r := SOME oper
                    in
                       ()
                    end
            else ()
         end
      val allocateVar =
         Trace.trace2
         ("AllocateRegisters.allocateVar", Var.layout, Allocation.layout, Unit.layout)
         allocateVar
      (* Set the stack slots for the formals.
       * Also, create a stack allocation that includes all formals; if
       * link and handler stack slots are required, then they will be
       * allocated against this stack.
       *)
      val stack =
         Allocation.Stack.new
         (Vector.foldr2
          (args, formalsStackOffsets args, [],
           fn ((x, _), so, stack) =>
           (valOf (#operand (varInfo x)) := SOME (Operand.StackOffset so)
            ; so :: stack)))
      (* Allocate stack slots for the link and handler, if necessary. *)
      val handlerLinkOffset =
         if !hasHandler
            then
               let
                  (* Choose fixed and permanently allocated stack
                   * slots that do not conflict with formals.
                   *)
                  val (stack, {offset = handler, ...}) =
                     Allocation.Stack.get (stack, Type.label (Label.newNoname ()))
                  val (_, {offset = link, ...}) = 
                     Allocation.Stack.get (stack, Type.exnStack ())
               in
                  SOME {handler = handler, link = link}
               end
         else NONE
      fun getOperands (xs: Var.t vector): Operand.t vector =
         Vector.map (xs, fn x => valOf (! (valOf (#operand (varInfo x)))))
      val getOperands =
         Trace.trace 
         ("AllocateRegisters.getOperands",
          Vector.layout Var.layout, Vector.layout Operand.layout)
         getOperands
      val {get = labelInfo: R.Label.t -> Info.t, set = setLabelInfo, ...} =
         Property.getSetOnce (R.Label.plist,
                              Property.initRaise ("labelInfo", R.Label.layout))
      val setLabelInfo =
         Trace.trace2
         ("AllocateRegisters.setLabelInfo", 
          R.Label.layout, Info.layout, Unit.layout)
         setLabelInfo
      (* Do a DFS of the control-flow graph. *)
      val () =
         Function.dfs
         (f, fn R.Block.T {args, label, kind, statements, transfer, ...} =>
          let
             val {begin, beginNoFormals, handler = handlerLive,
                  link = linkLive} = labelLive label
             val () = remLabelLive label
             fun addHS (ops: Operand.t vector): Operand.t vector =
                case handlerLinkOffset of
                   NONE => ops
                 | SOME {handler, link} =>
                      let
                         val extra = []
                         val extra =
                            case handlerLive of
                               NONE => extra
                             | SOME h => 
                                  Operand.stackOffset {offset = handler,
                                                       ty = Type.label h}
                                  :: extra
                         val extra =
                            if linkLive
                               then
                                  Operand.stackOffset {offset = link,
                                                       ty = Type.exnStack ()}
                                  :: extra
                            else extra
                      in
                         Vector.concat [Vector.fromList extra, ops]
                      end
             val liveNoFormals = getOperands beginNoFormals
             val (stackInit, registersInit) =
                Vector.fold
                (liveNoFormals, ([],[]), fn (oper, (stack, registers)) =>
                 case oper of
                    Operand.StackOffset s => (s::stack, registers)
                  | Operand.Register r => (stack, r::registers)
                  | _ => (stack, registers))
             val stackInit =
                case handlerLinkOffset of
                   NONE => stackInit
                 | SOME {handler, link} =>
                      StackOffset.T {offset = handler,
                                     ty = Type.label (Label.newNoname ())}
                      :: StackOffset.T {offset = link, 
                                        ty = Type.exnStack ()}
                      :: stackInit
             val a = Allocation.new (stackInit, registersInit)
             val size =
                case kind of
                   Kind.Handler =>
                      (case handlerLinkOffset of
                          NONE => Error.bug "AllocateRegisters.allocate: Handler with no handler offset"
                        | SOME {handler, ...} =>
                             Bytes.+ (Runtime.labelSize (), handler))
                 | _ =>
                      let
                         val size =
                            Bytes.+
                            (Runtime.labelSize (),
                             Bytes.alignWord32 (Allocation.stackSize a))
                      in
                         case !Control.align of
                            Control.Align4 => size
                          | Control.Align8 => Bytes.alignWord64 size
                      end
             val _ =
                if Bytes.isWord32Aligned size
                   then ()
                else Error.bug (concat ["AllocateRegisters.allocate: ",
                                        "bad size ",
                                        Bytes.toString size,
                                        " in ", Label.toString label])
             val _ = Vector.foreach (args, fn (x, _) => allocateVar (x, a))
             (* Must compute live after allocateVar'ing the args, since that
              * sets the operands for the args.
              *)
             val live = getOperands begin
             fun one (var, _) = allocateVar (var, a)
             val _ =
                Vector.foreach (statements, fn statement =>
                                R.Statement.foreachDef (statement, one))
             val _ = R.Transfer.foreachDef (transfer, one)
             val _ =
                setLabelInfo (label, {live = addHS live,
                                      liveNoFormals = addHS liveNoFormals,
                                      size = size})
          in
             fn () => ()
          end)
      val () =
         diagnostics
         (fn (display, diagVar, diagStatement) =>
          let
             open Layout
             val _ =
                display (seq [str "function ", Func.layout name,
                              str " handlerLinkOffset ",
                              Option.layout
                              (fn {handler, link} =>
                               record [("handler", Bytes.layout handler),
                                       ("link", Bytes.layout link)])
                              handlerLinkOffset])
             val _ = Vector.foreach (args, diagVar o #1)
             val _ =
                Vector.foreach
                (blocks, fn R.Block.T {label, args, statements, ...} =>
                 let
                    val {live, ...} = labelInfo label
                    val () = display (R.Label.layout label)
                    val () =
                       display
                       (seq [str "live: ", Vector.layout Operand.layout live])
                    val () = Vector.foreach (args, diagVar o #1)
                    val () = Vector.foreach (statements, diagStatement)
                 in
                    ()
                 end)
          in ()
          end)
   in
      {handlerLinkOffset = handlerLinkOffset,
       labelInfo = labelInfo}
   end

val allocate = 
   Trace.trace
   ("AllocateRegisters.allocate",
    fn {function, ...} => Func.layout (Function.name function),
    Layout.ignore)
   allocate

end
