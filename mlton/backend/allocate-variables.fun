(* Copyright (C) 2017,2019,2022 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor AllocateVariables (S: ALLOCATE_VARIABLES_STRUCTS): ALLOCATE_VARIABLES = 
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
   structure Live = Live
   structure Type = Type
   structure Var = Var
end

local
   open Machine
in
   structure CType = CType
   structure Operand = Operand
   structure Runtime = Runtime
   structure StackOffset = StackOffset
   structure Temporary = Temporary
end

structure Allocation:
   sig
      structure Temporaries:
         sig
            type t

            val get: t * Type.t -> Temporary.t
            val empty: unit -> t
         end
      structure Stack:
         sig
            type t

            val get: t * Type.t -> t * {offset: Bytes.t}
            val layout: t -> Layout.t
            val new: StackOffset.t list -> t
            val size: t -> Bytes.t
         end

      type t

      val getTemporary: t * Type.t -> Temporary.t
      val getStack: t * Type.t -> {offset: Bytes.t}
      val layout: t -> Layout.t
      val new: StackOffset.t list * Temporary.t list -> t
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
                   Array.fromListMap (alloc, fn StackOffset.T {offset, ty, ...} =>
                                      {offset = offset,
                                       size = Type.bytes ty})
                val () =
                   QuickSort.sortArray
                   (a, fn (t, t') => Bytes.<= (#offset t, #offset t'))
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
             ("AllocateVariables.Allocation.Stack.get",
              layout, Type.layout,
              Layout.tuple2 (layout, fn {offset} =>
                             Layout.record [("offset", Bytes.layout offset)]))
             get
       end
       structure Temporaries =
       struct
          (* A temporary allocation keeps track of the temporaries that have
           * already been allocated, for each runtime type.  The reason that
           * we associate them with runtime types rather than Rssa types is
           * that the temporary indices that the codegens use are based on
           * runtime types.
           *)
          datatype t = T of CType.t -> {alloc: Temporary.t list,
                                        next: int} ref

          fun layout (T f) =
             List.layout
             (fn t =>
              let
                 val {alloc, next} = ! (f t)
              in
                 Layout.record [("ty", CType.layout t),
                                ("next", Int.layout next),
                                ("alloc", List.layout Temporary.layout alloc)]
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
                       | t :: alloc =>
                            if next = Temporary.index t
                               then loop (next + 1, alloc)
                            else done ()
                   end
             in
                loop (next, alloc)
             end

          fun new (ts: Temporary.t list): t =
             let
                fun sameType (t, t') =
                   CType.equals
                   (Type.toCType (Temporary.ty t),
                    Type.toCType (Temporary.ty t'))
                val tss = List.equivalence (ts, sameType)
             in
                T (CType.memo
                   (fn ty =>
                    case List.peek (tss, fn ts =>
                                    case ts of
                                       [] => false
                                     | t :: _ =>
                                          CType.equals
                                          (ty, Type.toCType (Temporary.ty t))) of
                       NONE => ref {alloc = [], next = 0}
                     | SOME ts =>
                          ref
                          (compress
                           {next = 0,
                            alloc =
                            QuickSort.sortList
                            (ts, fn (t, t') =>
                             Temporary.index t <= Temporary.index t')})))
             end

          fun empty () = new []

          fun get (T f, ty: Type.t) =
             let
                val t = Type.toCType ty
                val r = f t
                val {alloc, next} = !r
                val temp = Temporary.new (ty, SOME next)
                val _ =
                   r := compress {alloc = alloc,
                                  next = next + 1}
             in
                temp
             end
       end

       datatype t = T of {temporaries: Temporaries.t,
                          stack: Stack.t ref}

       local
          fun make s (T x) = s x
       in
          val stack = ! o (make #stack)
          val stackSize = Stack.size o stack
       end

       fun layout (T {temporaries, stack}) =
          Layout.record
          [("stack", Stack.layout (!stack)),
           ("temporaries", Temporaries.layout temporaries)]

       fun getStack (T {stack, ...}, ty) =
          let
             val (s, offset) = Stack.get (!stack, ty)
             val _ = stack := s
          in
             offset
          end

       fun getTemporary (T {temporaries, ...}, ty) =
          Temporaries.get (temporaries, ty)

       fun new (stack, temporaries) = 
          T {temporaries = Temporaries.new temporaries,
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

fun allocate {function = f: Rssa.Function.t,
              paramOffsets,
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
       * will live in temporaries.
       * Initially,
       *   - all variables are put in a temporary.
       * Variables get moved to the stack if they are
       *   - live at the beginning of a Cont block; such variables are
       *     live while the frame is suspended during a non-tail call
       *     and must be stack allocated to be traced during a GC
       *   - live at the beginning of a CReturn block that mayGC; such
       *     variables are live while the frame is suspended during a
       *     C call and must be stack allocated to be traced during
       *     the potential GC
       * Both of the above are indiced by
       * Kind.frameStyle kind = Kind.OffsetsAndSize
       *)
      datatype place = Stack | Temporary
      val {get = place: Var.t -> place ref, rem = removePlace, ...} =
         Property.get (Var.plist, Property.initFun (fn _ => ref Temporary))
      (* The arguments for each Handler block in the function. *)
      val handlersArgs: (Var.t * Type.t) vector list ref = ref []
      fun forceStack (x: Var.t): unit = place x := Stack
      val _ =
         Vector.foreach
         (blocks,
          fn R.Block.T {args, kind, label, ...} =>
          let
             val {beginNoFormals, ...} = labelLive label
             val _ =
                case Kind.frameStyle kind of
                   Kind.None => ()
                 | Kind.OffsetsAndSize =>
                      Vector.foreach (beginNoFormals, forceStack)
                 | Kind.SizeOnly => ()
             val _ =
                case kind of
                   Kind.Handler => List.push (handlersArgs, args)
                 | _ => ()
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
                                   (StackOffset.T {offset = offset, ty = ty, volatile = false})
                                end
                           | Temporary =>
                                Operand.Temporary
                                (Allocation.getTemporary (a, ty))
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
         ("AllocateVariables.allocateVar", Var.layout, Allocation.layout, Unit.layout)
         allocateVar
      fun getOperand (x: Var.t): Operand.t =
         case #operand (varInfo x) of
            NONE => Error.bug (concat ["AllocateVariables.getOperand: ",
                                       "#operand (varInfo ",
                                       Var.toString x, ") = NONE"])
          | SOME r =>
               (case !r of
                   NONE => Error.bug (concat ["AllocateVariables.getOperand: ",
                                              "! (valOf (#operand (varInfo ",
                                              Var.toString x, "))) = NONE"])
                 | SOME oper => oper)
      val getOperand =
         Trace.trace
         ("AllocateVariables.getOperand", Var.layout, Operand.layout)
         getOperand
      fun getOperands (xs: Var.t vector): Operand.t vector =
         Vector.map (xs, getOperand)
      val getOperands =
         Trace.trace
         ("AllocateVariables.getOperands",
          Vector.layout Var.layout, Vector.layout Operand.layout)
         getOperands
      val {get = labelInfo: R.Label.t -> Info.t, set = setLabelInfo, ...} =
         Property.getSetOnce (R.Label.plist,
                              Property.initRaise ("labelInfo", R.Label.layout))
      val setLabelInfo =
         Trace.trace2
         ("AllocateVariables.setLabelInfo",
          R.Label.layout, Info.layout, Unit.layout)
         setLabelInfo

      (* Allocate stacks slots and/or temporaries for the formals.
       * Don't use `allocateVar`, because a stack formal
       * should use the stack slot of the incoming actual.
       *)
      val () =
         let
            val temps = Allocation.Temporaries.empty ()
         in
            Vector.foreach2
            (args, paramOffsets args, fn ((x, ty), so) =>
             let
                val oper =
                   case ! (place x) of
                      Stack => Operand.StackOffset (StackOffset.T so)
                    | Temporary => Operand.Temporary (Allocation.Temporaries.get (temps, ty))
                val () = removePlace x
                val () = valOf (#operand (varInfo x)) := SOME oper
             in
                ()
             end)
         end
      (* Also, create a stack allocation that includes all incoming actuals;
       * if link, handler label, and handler args stack slots are required,
       * then they will be allocated against this stack.
       *)
      val stack =
         Allocation.Stack.new (Vector.toListMap (paramOffsets args, StackOffset.T))
      val handlersInfo =
         case !handlersArgs of
            [] => NONE
          | handlersArgs =>
               let
                  (* Choose fixed and permanently allocated stack slots
                   * that do not conflict with incoming actuals.
                   *)
                  val (stack, {offset = linkOffset, ...}) =
                     Allocation.Stack.get (stack, Type.exnStack ())
                  val (_, {offset = handlerOffset, ...}) =
                     Allocation.Stack.get (stack, Type.label (Label.newNoname ()))
                  val handlerArgsOffset =
                     Bytes.align
                     (Bytes.+ (handlerOffset, Runtime.labelSize ()),
                      {alignment = (case !Control.align of
                                       Control.Align4 => Bytes.inWord32
                                     | Control.Align8 => Bytes.inWord64)})
                  val handlerArgsSize =
                     List.fold
                     (handlersArgs, Bytes.zero, fn (args, maxSize) =>
                      Vector.fold
                      (paramOffsets args, maxSize, fn ({offset, ty, ...}, maxSize) =>
                       Bytes.max (maxSize, Bytes.+ (offset, Type.bytes ty))))
                  val handlerOffset = Bytes.- (handlerArgsOffset, Runtime.labelSize ())
               in
                  SOME {handlerArgsOffset = handlerArgsOffset,
                        handlerArgsSize = handlerArgsSize,
                        handlerOffset = handlerOffset,
                        linkOffset = linkOffset}
               end

      (* Do a DFS of the control-flow graph. *)
      val () =
         Function.dfs
         (f, fn R.Block.T {args, label, kind, statements, ...} =>
          let
             val {begin, beginNoFormals,
                  handler = handlerLive,
                  link = linkLive} = labelLive label
             val () = remLabelLive label
             fun addHS (ops: Operand.t vector): Operand.t vector =
                case handlersInfo of
                   NONE => ops
                 | SOME {handlerOffset, linkOffset, ...} =>
                      let
                         val extra = []
                         val extra =
                            case handlerLive of
                               NONE => extra
                             | SOME h => 
                                  Operand.stackOffset {offset = handlerOffset,
                                                       ty = Type.label h,
                                                       volatile = false}
                                  :: extra
                         val extra =
                            if linkLive
                               then
                                  Operand.stackOffset {offset = linkOffset,
                                                       ty = Type.exnStack (),
                                                       volatile = false}
                                  :: extra
                            else extra
                      in
                         Vector.concat [Vector.fromList extra, ops]
                      end
             val liveNoFormals = getOperands beginNoFormals
             val (stackInit, temporariesInit) =
                Vector.fold
                (liveNoFormals, ([],[]), fn (oper, (stack, temporaries)) =>
                 case oper of
                    Operand.StackOffset s => (s::stack, temporaries)
                  | Operand.Temporary t => (stack, t::temporaries)
                  | _ => (stack, temporaries))
             val stackInit =
                case handlersInfo of
                   NONE => stackInit
                 | SOME {handlerArgsOffset, handlerArgsSize, handlerOffset, linkOffset, ...} =>
                      StackOffset.T {offset = linkOffset,
                                     ty = Type.exnStack (),
                                     volatile = false}
                      :: StackOffset.T {offset = handlerOffset,
                                        ty = Type.label (Label.newNoname ()),
                                        volatile = false}
                      :: (if (Bytes.> (handlerArgsSize, Bytes.zero))
                             then StackOffset.T {offset = handlerArgsOffset,
                                                 ty = Type.bits (Bytes.toBits handlerArgsSize),
                                                 volatile = false}
                                  :: stackInit
                             else stackInit)
             val a = Allocation.new (stackInit, temporariesInit)
             val size =
                case kind of
                   Kind.Handler =>
                      (case handlersInfo of
                          NONE => Error.bug "AllocateVariables.allocate: Handler with no handler offset"
                        | SOME {handlerOffset, ...} =>
                             Bytes.+ (handlerOffset, Runtime.labelSize ()))
                 | _ =>
                      Bytes.align
                      (Bytes.+ (Allocation.stackSize a, Runtime.labelSize ()),
                       {alignment = (case !Control.align of
                                        Control.Align4 => Bytes.inWord32
                                      | Control.Align8 => Bytes.inWord64)})
             val _ =
                if Bytes.isAligned (size, {alignment = (case !Control.align of
                                                           Control.Align4 => Bytes.inWord32
                                                         | Control.Align8 => Bytes.inWord64)})
                   then ()
                else Error.bug (concat ["AllocateVariables.allocate: ",
                                        "bad size ",
                                        Bytes.toString size,
                                        " in ", Label.toString label])
             val _ = Vector.foreach (args, fn (x, _) =>
                                     if Vector.exists (begin, fn y =>
                                                       Var.equals (x, y))
                                        then allocateVar (x, a)
                                        else ())
             (* Must compute live after allocateVar'ing the args, since that
              * sets the operands for the args.
              *)
             val live = getOperands begin
             fun one (var, _) = allocateVar (var, a)
             val _ =
                Vector.foreach (statements, fn statement =>
                                R.Statement.foreachDef (statement, one))
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
                              str " handlersInfo ",
                              Option.layout
                              (fn {handlerArgsOffset, handlerArgsSize,
                                   handlerOffset, linkOffset, ...} =>
                               record [("handlerArgsOffset", Bytes.layout handlerArgsOffset),
                                       ("handlerArgsSize", Bytes.layout handlerArgsSize),
                                       ("handlerOffset", Bytes.layout handlerOffset),
                                       ("linkOffset", Bytes.layout linkOffset)])
                              handlersInfo])
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
      {handlersInfo = Option.map (handlersInfo, fn {handlerOffset, linkOffset, ...} =>
                                  {handlerOffset = handlerOffset,
                                   linkOffset = linkOffset}),
       labelInfo = labelInfo}
   end

val allocate = 
   Trace.trace
   ("AllocateVariables.allocate",
    fn {function, ...} => Func.layout (Function.name function),
    Layout.ignore)
   allocate

end
