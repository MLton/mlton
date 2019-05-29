(* Copyright (C) 2019 Matthew Fluet.
 * Copyright (C) 2002-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ImplementProfiling (S: RSSA_TRANSFORM_STRUCTS): RSSA_TRANSFORM =
struct

open S
open Rssa

structure CFunction =
   struct
      open CFunction

      structure CType =
         struct
            open CType
            val gcState = cpointer
         end

      local
         fun make {args, name, prototype} =
            T {args = args,
               convention = Convention.Cdecl,
               kind = Kind.Runtime {bytesNeeded = NONE,
                                    ensuresBytesFree = NONE,
                                    mayGC = false,
                                    maySwitchThreadsFrom = false,
                                    maySwitchThreadsTo = false,
                                    modifiesFrontier = false,
                                    readsStackTop = true,
                                    writesStackTop = false},
               prototype = (prototype, NONE),
               return = Type.unit,
               symbolScope = SymbolScope.Private,
               target = Target.Direct name}
      in
         val profileEnter = fn () =>
            make {args = Vector.new1 (Type.gcState ()),
                  name = "GC_profileEnter",
                  prototype = Vector.new1 CType.gcState}
         val profileInc = fn () =>
            make {args = Vector.new2 (Type.gcState (), Type.csize ()),
                  name = "GC_profileInc",
                  prototype = Vector.new2 (CType.gcState, CType.csize ())}
         val profileLeave = fn () =>
            make {args = Vector.new1 (Type.gcState ()),
                  name = "GC_profileLeave",
                  prototype = Vector.new1 CType.gcState}
      end
   end

type sourceSeq = {sourceIndex: int} list

structure InfoNode =
   struct
      datatype t = T of {info: SourceInfo.t,
                         sourceNameIndex: int,
                         sourceIndex: int,
                         successors: t list ref}

      local
         fun make f (T r) = f r
      in
         val info = make #info
         val sourceIndex = make #sourceIndex
      end

      fun layout (T {info, ...}) =
         Layout.record [("info", SourceInfo.layout info)]

      fun equals (n: t, n': t): bool = SourceInfo.equals (info n, info n')

      fun call {from = T {successors, ...},
                to as T {info = i', ...}} =
         if let
               open SourceInfo
            in
               equals (i', gc)
               orelse equals (i', main)
               orelse equals (i', unknown)
            end orelse List.exists (!successors, fn n => equals (n, to))
            then ()
         else List.push (successors, to)

      val call =
         Trace.trace ("Profile.InfoNode.call",
                      fn {from, to} =>
                      Layout.record [("from", layout from),
                                     ("to", layout to)],
                      Unit.layout)
         call
   end

structure FuncInfo =
   struct
      datatype t = T of {callers: InfoNode.t list ref,
                         enters: InfoNode.t list ref,
                         seen: bool ref,
                         tailCalls: t list ref}

      fun new () = T {callers = ref [],
                      enters = ref [],
                      seen = ref false,
                      tailCalls = ref []}
   end

structure Push =
   struct
      datatype t =
         Enter of InfoNode.t
       | Skip of SourceInfo.t

      fun layout z =
         let
            open Layout
         in
            case z of
               Enter n => seq [str "Enter ", InfoNode.layout n]
             | Skip i => seq [str "Skip ", SourceInfo.layout i]
         end

      fun toSourceSeq (ps: t list): sourceSeq =
         List.fold (rev ps, [], fn (p, ac) =>
                    case p of
                       Enter (InfoNode.T {sourceIndex, ...}) =>
                          {sourceIndex = sourceIndex} :: ac
                     | Skip _ => ac)
   end

val traceEnter =
   Trace.trace2 ("Profile.enter",
                 List.layout Push.layout,
                 SourceInfo.layout,
                 Layout.tuple2 (List.layout Push.layout, Bool.layout))

fun transform program =
   if !Control.profile = Control.ProfileNone
      then program
   else
   let
      val Program.T {functions, handlesSignals, main, objectTypes, ...} = program
      val debug = false
      datatype z = datatype Control.profile
      val profile = !Control.profile
      val profileStack: bool = !Control.profileStack
      val needProfileLabels: bool =
         profile = ProfileTimeLabel orelse profile = ProfileLabel
      val needCodeCoverage: bool =
         needProfileLabels orelse (profile = ProfileTimeField)
      val infoNodes: InfoNode.t list ref = ref []
      val sourceNames: string list ref = ref []
      local
         val sourceNameCounter = Counter.new 0
         val sep =
            if profile = ProfileCallStack
               then " "
            else "\t"
         val {get = sourceNameIndex, ...} =
            Property.get (SourceInfo.plist,
                          Property.initFun
                          (fn si =>
                           (List.push (sourceNames, SourceInfo.toString' (si, sep))
                            ; Counter.next sourceNameCounter)))
         val sourceCounter = Counter.new 0
      in         
         fun sourceInfoNode (si: SourceInfo.t) =
            let
               val infoNode =
                  InfoNode.T {info = si,
                              sourceNameIndex = sourceNameIndex si,
                              sourceIndex = Counter.next sourceCounter,
                              successors = ref []}
               val _ = List.push (infoNodes, infoNode)
            in
               infoNode
            end
      end
      fun firstEnter (ps: Push.t list): InfoNode.t option =
         List.peekMap (ps, fn p =>
                       case p of
                          Push.Enter n => SOME n
                        | _ => NONE)
      (* unknown must be 0, which == SOURCES_INDEX_UNKNOWN from sources.h *)
      val unknownInfoNode = sourceInfoNode SourceInfo.unknown
      (* gc must be 1 which == SOURCES_INDEX_GC from sources.h *)
      val gcInfoNode = sourceInfoNode SourceInfo.gc
      val mainInfoNode = sourceInfoNode SourceInfo.main
      fun wantedSource (si: SourceInfo.t): bool =
         if SourceInfo.isC si
            then List.length (!Control.profileC) > 0
            else (case SourceInfo.file si of
                     NONE => true
                   | SOME file =>
                        List.foldr
                        (!Control.profileInclExcl, true, 
                         fn ((re, keep), b) =>
                         if Regexp.Compiled.matchesAll (re, file)
                            then keep
                            else b))
      val wantedSource =
         Trace.trace ("Profile.wantedSource", SourceInfo.layout, Bool.layout)
         wantedSource
      fun wantedCSource (si: SourceInfo.t): bool =
         wantedSource si
         andalso
         if SourceInfo.isC si
            then false
            else (case SourceInfo.file si of
                     NONE => false
                   | SOME file => 
                        List.foldr
                        (!Control.profileC, false,
                         fn (re, b) =>
                         if Regexp.Compiled.matchesAll (re, file)
                            then true
                            else b))
      val wantedCSource =
         Trace.trace ("Profile.wantedCSource", SourceInfo.layout, Bool.layout)
         wantedCSource
      fun keepSource (si: SourceInfo.t): bool =
         profile <> ProfileCount
         orelse wantedSource si
      val keepSource =
         Trace.trace ("Profile.keepSource", SourceInfo.layout, Bool.layout)
         keepSource
      (* With -profile count, we want to get zero counts for all functions,
       * whether or not they made it into the final executable.
       *)
      val () =
         case profile of
            ProfileCount =>
               List.foreach (SourceInfo.all (), fn si =>
                             if wantedSource si
                                then ignore (sourceInfoNode si)
                             else ())
          | _ => ()
      val sourceInfoNode =
         fn si =>
         let
            open SourceInfo
         in
            if equals (si, unknown)
               then unknownInfoNode
            else if equals (si, gc)
               then gcInfoNode
            else if equals (si, main)
               then mainInfoNode
            else sourceInfoNode si
         end
      val sourceInfoNode =
         Trace.trace ("Profile.sourceInfoNode", SourceInfo.layout, InfoNode.layout)
         sourceInfoNode
      val sourceSeqs: {sourceIndex: int} vector list ref = ref []
      local
         val table: ({sourceIndex: int} vector, int) HashTable.t =
            let
               fun equals (sourceSeq1, sourceSeq2) =
                  Vector.equals (sourceSeq1, sourceSeq2,
                                 fn ({sourceIndex = si1}, {sourceIndex = si2}) =>
                                 si1 = si2)
               fun hash sourceSeq =
                  Hash.vectorMap (sourceSeq, fn {sourceIndex} =>
                                  Word.fromInt sourceIndex)
            in
               HashTable.new
               {equals = equals,
                hash = hash}
            end
         val c = Counter.new 0
      in
         fun sourceSeqIndex (s: sourceSeq): int =
            let
               val s = Vector.fromListRev s
            in
               HashTable.lookupOrInsert
               (table, s, fn () =>
                (List.push (sourceSeqs, s)
                 ; Counter.next c))
            end
      end
      (* Ensure that [SourceInfo.unknown] is index 0. *)
      val _ = sourceSeqIndex [{sourceIndex = InfoNode.sourceIndex unknownInfoNode}]
      (* Ensure that [SourceInfo.gc] is index 1. *)
      val _ = sourceSeqIndex [{sourceIndex = InfoNode.sourceIndex gcInfoNode}]
      local
         (* Cannot use Label.plist, because RSSA Labels are cleared
          * between ImplementProfiling and conversion to Machine.
          *)
         val frameSourceSeqIndex: (Label.t, int) HashTable.t =
            HashTable.new {equals = Label.equals, hash = Label.hash}
      in
         fun addFrameSourceSeqIndex (label: Label.t, sourceSeqIndex: int): unit =
            (ignore o HashTable.insertIfNew)
            (frameSourceSeqIndex, label, fn () => sourceSeqIndex, fn _ =>
             Error.bug ("ImplementProfiling.addFrameSourceSeqIndex: " ^ Label.toString label))
         fun getFrameSourceSeqIndex (label: Label.t) : int =
            case HashTable.peek (frameSourceSeqIndex, label) of
               NONE => Error.bug "ImplementProfiling.getFrameSourceSeqIndex"
             | SOME sourceSeqIndex => sourceSeqIndex
      end
      fun addFramePushes (label: Label.t, pushes: Push.t list): unit =
         addFrameSourceSeqIndex (label, sourceSeqIndex (Push.toSourceSeq pushes))
      val {get = labelInfo: Label.t -> {block: Block.t,
                                        visited1: bool ref,
                                        visited2: bool ref},
           set = setLabelInfo, ...} =
         Property.getSetOnce
         (Label.plist, Property.initRaise ("info", Label.layout))
      val profileLabelInfos = ref []
      fun profileLabelFromIndex (sourceSeqIndex: int): Statement.t =
         let
            val pl = ProfileLabel.new ()
            val _ = List.push (profileLabelInfos,
                               {profileLabel = pl,
                                sourceSeqIndex = sourceSeqIndex})
         in
            Statement.ProfileLabel pl
         end
      fun setCurSourceSeqIndexFromIndex (sourceSeqIndex: int): Statement.t =
         let
            val curSourceSeqIndex =
               Operand.Runtime Runtime.GCField.CurSourceSeqIndex
         in
            Statement.Move
            {dst = curSourceSeqIndex,
             src = Operand.word (WordX.fromIntInf 
                                 (IntInf.fromInt sourceSeqIndex,
                                  WordSize.word32))}
         end
      fun codeCoverageStatementFromSourceSeqIndex (sourceSeqIndex: int): Statement.t =
         if needProfileLabels
            then profileLabelFromIndex sourceSeqIndex
         else if profile = ProfileTimeField
            then setCurSourceSeqIndexFromIndex sourceSeqIndex
         else Error.bug "Profile.codeCoverageStatement"
      fun codeCoverageStatement (sourceSeq: sourceSeq): Statement.t =
         codeCoverageStatementFromSourceSeqIndex (sourceSeqIndex sourceSeq)
      local
         val {get: Func.t -> FuncInfo.t, ...} =
            Property.get (Func.plist, Property.initFun (fn _ => FuncInfo.new ()))
      in
         val funcInfo = get
         fun addFuncEdges () =
            (* Don't need to add edges for main because no one calls it. *)
            List.foreach
            (functions, fn f =>
             let
                val allSeen: bool ref list ref = ref []
                val func = Function.name f
                val fi as FuncInfo.T {callers, ...} = get func
                (* Add edges from all the callers to the enters in f and all
                 * functions that f tail calls.
                 *)
                fun call (FuncInfo.T {enters, seen, tailCalls, ...}): unit =
                   if !seen
                      then ()
                   else
                      let
                         val _ = seen := true
                         val _ = List.push (allSeen, seen)
                         val _ = 
                            List.foreach
                            (!callers, fn from =>
                             List.foreach
                             (!enters, fn to =>
                              InfoNode.call {from = from, to = to}))
                      in
                         List.foreach (!tailCalls, call)
                      end
                val _ = call fi
                val _ = List.foreach (!allSeen, fn r => r := false)
             in
                ()
             end)
      end
      fun doFunction (f: Function.t): Function.t =
         let
            val {args, blocks, name, raises, returns, start} = Function.dest f
            val _ =
               if not debug
                  then ()
               else print (concat ["doFunction ", Func.toString name, "\n"])
            val FuncInfo.T {enters, tailCalls, ...} = funcInfo name
            fun enter (ps: Push.t list, si: SourceInfo.t): Push.t list * bool =
               let
                  val node = Promise.lazy (fn () => sourceInfoNode si)
                  fun yes () = (Push.Enter (node ()) :: ps, true)
                  fun no () = (Push.Skip si :: ps, false)
               in
                  if SourceInfo.equals (si, SourceInfo.unknown)
                     then no ()
                  else
                     case firstEnter ps of
                        NONE =>
                           if keepSource si
                              then (List.push (enters, node ())
                                    ; yes ())
                           else no ()
                      | SOME (node' as InfoNode.T {info = si', ...}) =>
                           (* 
                            * si  : callee
                            * si' : caller
                            *)
                           if keepSource si
                              andalso
                              let
                                 open SourceInfo
                              in
                                 equals (si', unknown)
                                 orelse
                                 (wantedSource si
                                  andalso
                                  not (equals (si, gcSequenceAllocate))
                                  andalso
                                  (not (isC si)
                                   orelse
                                   (wantedCSource si'
                                    andalso not (equals (si', main)))))
                              end 
                              then (InfoNode.call {from = node', to = node ()}
                                    ; yes ())
                           else no ()
               end
            val enter = traceEnter enter
            val _ =
               Vector.foreach
               (blocks, fn block as Block.T {label, ...} =>
                setLabelInfo (label, {block = block,
                                      visited1 = ref false,
                                      visited2 = ref false}))
            (* Find the first Enter statement and (conceptually) move it to the
             * front of the function.
             *)
            local
               exception Yes of Label.t * Statement.t
               fun goto l =
                  let
                     val {block, visited1, ...} = labelInfo l
                  in
                     if !visited1
                        then ()
                     else
                        let
                           val () = visited1 := true
                           val Block.T {statements, transfer, ...} = block
                           val () =
                              Vector.foreach
                              (statements, fn s =>
                               case s of
                                  Statement.Profile (ProfileExp.Enter _) =>
                                     raise Yes (l, s)
                                | _ => ())
                           val () = Transfer.foreachLabel (transfer, goto)
                        in
                           ()
                        end
                  end
            in
               val first = (goto start; NONE) handle Yes z => SOME z
            end
            val blocks = ref []
            datatype z = datatype Statement.t
            datatype z = datatype ProfileExp.t
            fun backward {args,
                          kind,
                          label,
                          leaves,
                          sourceSeq: sourceSeq,
                          statements: Statement.t list,
                          transfer: Transfer.t}: unit =
               let
                  val (_, ncc, sourceSeq, statements) =
                     List.fold
                     (statements,
                      (leaves, true, sourceSeq, []),
                      fn (s, (leaves, ncc, sourceSeq, ss)) =>
                      case s of
                         Object _ => (leaves, true, sourceSeq, s :: ss)
                       | Profile ps =>
                            let
                               val (ncc, ss) =
                                  if needCodeCoverage
                                     then
                                        if ncc
                                           andalso not (List.isEmpty sourceSeq)
                                           then (false,
                                                 codeCoverageStatement sourceSeq :: ss)
                                        else (true, ss)
                                  else (false, ss)
                               val (leaves, sourceSeq) = 
                                  case ps of
                                     Enter _ =>
                                        (case sourceSeq of
                                            [] => Error.bug 
                                                  "Profile.backward: unmatched Enter"
                                          | _ :: sis => (leaves, sis))
                                   | Leave _ =>
                                        (case leaves of
                                            [] => Error.bug 
                                                  "Profile.backward: missing Leave"
                                          | infoNode :: leaves =>
                                               (leaves,
                                                {sourceIndex = InfoNode.sourceIndex infoNode}
                                                :: sourceSeq))
                            in
                               (leaves, ncc, sourceSeq, ss)
                            end
                       | _ => (leaves, true, sourceSeq, s :: ss))
                  val statements =
                     if needCodeCoverage
                        andalso ncc
                        then codeCoverageStatement sourceSeq :: statements
                     else statements
                  val {args, kind, label} =
                     if profileStack andalso (case kind of
                                                 Kind.Cont _ => true
                                               | Kind.Handler => true
                                               | _ => false)
                        then
                           let
                              val func = CFunction.profileLeave ()
                              val newLabel = Label.newNoname ()
                              val _ =
                                 addFrameSourceSeqIndex
                                 (newLabel, sourceSeqIndex sourceSeq)
                              val statements =
                                 if needCodeCoverage
                                    then (Vector.new1
                                          (codeCoverageStatement sourceSeq))
                                 else Vector.new0 ()
                              val _ =
                                 List.push
                                 (blocks,
                                  Block.T
                                  {args = args,
                                   kind = kind,
                                   label = label,
                                   statements = statements,
                                   transfer = 
                                   Transfer.CCall
                                   {args = Vector.new1 Operand.GCState,
                                    func = func,
                                    return = SOME newLabel}})
                           in
                              {args = Vector.new0 (),
                               kind = Kind.CReturn {func = func},
                               label = newLabel}
                           end
                     else
                        {args = args,
                         kind = kind,
                         label = label}
               in                      
                  List.push (blocks,
                             Block.T {args = args,
                                      kind = kind,
                                      label = label,
                                      statements = Vector.fromList statements,
                                      transfer = transfer})
               end
            val backward =
               Trace.trace
               ("Profile.backward",
                fn {leaves, statements, sourceSeq, ...} =>
                let
                   open Layout
                in
                   record [("leaves", List.layout InfoNode.layout leaves),
                           ("sourceSeq",
                            List.layout (fn {sourceIndex} =>
                                         record [("sourceIndex", Int.layout sourceIndex)])
                                        sourceSeq),
                           ("statements",
                            List.layout Statement.layout statements)]
                end,
                Unit.layout)
               backward
            fun profileEnter (pushes: Push.t list,
                              transfer: Transfer.t): Transfer.t =
               let
                  val func = CFunction.profileEnter ()
                  val newLabel = Label.newNoname ()
                  val sourceSeqIndex = sourceSeqIndex (Push.toSourceSeq pushes)
                  val _ = addFrameSourceSeqIndex (newLabel, sourceSeqIndex)
                  val statements =
                     if needCodeCoverage
                        then Vector.new1 (codeCoverageStatementFromSourceSeqIndex sourceSeqIndex)
                     else Vector.new0 ()
                  val _ =
                     List.push
                     (blocks,
                      Block.T {args = Vector.new0 (),
                               kind = Kind.CReturn {func = func},
                               label = newLabel,
                               statements = statements,
                               transfer = transfer})
               in
                  Transfer.CCall {args = Vector.new1 Operand.GCState,
                                  func = func,
                                  return = SOME newLabel}
               end
            fun goto (l: Label.t, pushes: Push.t list): unit =
               let
                  val _ =
                     if not debug
                        then ()
                     else
                     let
                        open Layout
                     in
                        outputl (seq [str "goto (",
                                      Label.layout l,
                                      str ", ",
                                      List.layout Push.layout pushes,
                                      str ")"],
                                 Out.error)
                     end
                  val {block, visited2, ...} = labelInfo l
               in
                  if !visited2
                     then ()
                  else
                     let
                        val _ = visited2 := true
                        val Block.T {args, kind, label, statements, transfer,
                                     ...} = block
                        val statements =
                           case first of
                              NONE => statements
                            | SOME (firstLabel, firstEnter) =>
                                 if Label.equals (label, firstLabel)
                                    then
                                       Vector.removeFirst
                                       (statements, fn s =>
                                        case s of
                                           Profile (Enter _) => true
                                         | _ => false)
                                 else if Label.equals (label, start)
                                         then
                                            Vector.concat
                                            [Vector.new1 firstEnter,
                                             statements]
                                      else statements
                        val _ =
                           let
                              fun add pushes = addFramePushes (label, pushes)
                              datatype z = datatype Kind.t
                           in
                              case kind of
                                 Cont _ => add pushes
                               | CReturn {func, ...} =>
                                    let
                                       datatype z = datatype CFunction.Target.t
                                       val target = CFunction.target func
                                       fun doit si =
                                          add (#1 (enter (pushes, si)))
                                    in
                                       case target of
                                          Direct "GC_collect" => doit SourceInfo.gc
                                        | Direct "GC_sequenceAllocate" =>
                                             doit SourceInfo.gcSequenceAllocate
                                        | Direct "MLton_bug" => add pushes
                                        | Direct name => doit (SourceInfo.fromC name)
                                        | Indirect => doit (SourceInfo.fromC "<indirect>")
                                    end
                               | Handler => add pushes
                               | Jump => ()
                           end
                        fun maybeSplit {args,
                                        bytesAllocated: Bytes.t,
                                        kind,
                                        label,
                                        leaves,
                                        pushes: Push.t list,
                                        shouldSplit: bool,
                                        statements} =
                           if not shouldSplit
                              then {args = args,
                                    bytesAllocated = Bytes.zero,
                                    kind = kind,
                                    label = label,
                                    leaves = leaves,
                                    statements = statements}
                           else
                              let
                                 val newLabel = Label.newNoname ()
                                 val _ = addFramePushes (newLabel, pushes)
                                 val func = CFunction.profileInc ()
                                 val amount =
                                    case profile of
                                       ProfileAlloc => Bytes.toInt bytesAllocated
                                     | ProfileCount => 1
                                     | _ => Error.bug "Profile.maybeSplit: amount"
                                 val transfer =
                                    Transfer.CCall
                                    {args = (Vector.new2
                                             (Operand.GCState,
                                              Operand.word
                                              (WordX.fromIntInf
                                               (IntInf.fromInt amount,
                                                WordSize.csize ())))),
                                     func = func,
                                     return = SOME newLabel}
                                 val sourceSeq = Push.toSourceSeq pushes
                                 val _ =
                                    backward {args = args,
                                              kind = kind,
                                              label = label,
                                              leaves = leaves,
                                              sourceSeq = sourceSeq,
                                              statements = statements,
                                              transfer = transfer}
                              in
                                 {args = Vector.new0 (),
                                  bytesAllocated = Bytes.zero,
                                  kind = Kind.CReturn {func = func},
                                  label = newLabel,
                                  leaves = [],
                                  statements = []}
                              end
                        val {args, bytesAllocated, kind, label, leaves, pushes,
                             statements} =
                           Vector.fold
                           (statements,
                            {args = args,
                             bytesAllocated = Bytes.zero,
                             kind = kind,
                             label = label,
                             leaves = [],
                             pushes = pushes,
                             statements = []},
                            fn (s, {args, bytesAllocated, kind, label,
                                    leaves,
                                    pushes: Push.t list,
                                    statements}) =>
                            (if not debug
                                then ()
                             else
                                let
                                   open Layout
                                in
                                   outputl
                                   (seq [List.layout Push.layout pushes,
                                         str " ",
                                         Statement.layout s],
                                    Out.error)
                                end
                             ;
                            case s of
                               Object {size, ...} =>
                                  {args = args,
                                   bytesAllocated = Bytes.+ (bytesAllocated, size),
                                   kind = kind,
                                   label = label,
                                   leaves = leaves,
                                   pushes = pushes,
                                   statements = s :: statements}
                             | Profile ps =>
                                  let
                                     val shouldSplit =
                                        profile = ProfileAlloc
                                        andalso Bytes.> (bytesAllocated,
                                                         Bytes.zero)
                                     val {args, bytesAllocated, kind, label,
                                          leaves, statements} =
                                        maybeSplit
                                        {args = args,
                                         bytesAllocated = bytesAllocated,
                                         kind = kind,
                                         label = label,
                                         leaves = leaves,
                                         pushes = pushes,
                                         shouldSplit = shouldSplit,
                                         statements = statements}
                                     datatype z = datatype ProfileExp.t
                                     val (pushes, keep, leaves) =
                                        case ps of
                                           Enter si =>
                                              let
                                                 val (pushes, keep) =
                                                    enter (pushes, si)
                                              in
                                                 (pushes, keep, leaves)
                                              end
                                         | Leave si =>
                                              (case pushes of
                                                  [] => Error.bug 
                                                        "Profile.goto: unmatched Leave"
                                                | p :: pushes =>
                                                     let
                                                        val (keep, si', leaves) =
                                                           case p of
                                                              Push.Enter
                                                              (infoNode as
                                                               InfoNode.T
                                                               {info, ...}) =>
                                                                 (true, info,
                                                                  infoNode :: leaves)
                                                            | Push.Skip si' =>
                                                                 (false, si',
                                                                  leaves)
                                                     in
                                                        if SourceInfo.equals (si, si')
                                                           then (pushes,
                                                                 keep,
                                                                 leaves)
                                                        else Error.bug 
                                                             "Profile.goto: mismatched Leave"
                                                     end)
                                     val shouldSplit =
                                        profile = ProfileCount
                                        andalso (case ps of
                                                    Enter _ => keep
                                                  | _ => false)
                                     val {args, bytesAllocated, kind, label,
                                          leaves, statements} =
                                        maybeSplit
                                        {args = args,
                                         bytesAllocated = bytesAllocated,
                                         kind = kind,
                                         label = label,
                                         leaves = leaves,
                                         pushes = pushes,
                                         shouldSplit = shouldSplit,
                                         statements = statements}
                                     val statements =
                                        if keep
                                           then s :: statements
                                        else statements
                                  in
                                     {args = args,
                                      bytesAllocated = bytesAllocated,
                                      kind = kind,
                                      label = label,
                                      leaves = leaves,
                                      pushes = pushes,
                                      statements = statements}
                                  end
                             | _ =>
                                  {args = args,
                                   bytesAllocated = bytesAllocated,
                                   kind = kind,
                                   label = label,
                                   leaves = leaves,
                                   pushes = pushes,
                                   statements = s :: statements})
                            )
                        val shouldSplit =
                           profile = ProfileAlloc
                           andalso Bytes.> (bytesAllocated, Bytes.zero)
                        val {args, kind, label, leaves, statements, ...} =
                           maybeSplit {args = args,
                                       bytesAllocated = bytesAllocated,
                                       kind = kind,
                                       label = label,
                                       leaves = leaves,
                                       pushes = pushes,
                                       shouldSplit = shouldSplit,
                                       statements = statements}
                        val _ =
                           Transfer.foreachLabel
                           (transfer, fn l => goto (l, pushes))
                        val transfer =
                           case transfer of
                              Transfer.Call {func, return, ...} =>
                                 let
                                    val fi as FuncInfo.T {callers, ...} =
                                       funcInfo func
                                 in
                                    case return of
                                       Return.NonTail _ =>
                                          let
                                             val _ =
                                                case firstEnter pushes of
                                                   NONE =>
                                                      List.push (tailCalls, fi)
                                                 | SOME n => 
                                                      List.push (callers, n)
                                          in
                                             if profileStack
                                                then profileEnter (pushes,
                                                                   transfer)
                                             else transfer
                                          end
                                     | _ =>
                                          (List.push (tailCalls, fi)
                                           ; transfer)
                                 end
                            | _ => transfer
                     in
                        backward {args = args,
                                  kind = kind,
                                  label = label,
                                  leaves = leaves,
                                  sourceSeq = Push.toSourceSeq pushes,
                                  statements = statements,
                                  transfer = transfer}
                     end
               end
            val _ = goto (start, [])
            val blocks = Vector.fromList (!blocks)
         in
            Function.new {args = args,
                          blocks = blocks,
                          name = name,
                          raises = raises,
                          returns = returns,
                          start = start}
         end
      val (main, functions) = (doFunction main, List.map (functions, doFunction))
      val _ = addFuncEdges ()
      val profileLabelInfos = Vector.fromList (!profileLabelInfos)
      val sourceNames = Vector.fromListRev (!sourceNames)
      val sources =
         Vector.map
         (Vector.fromListRev (!infoNodes),
          fn InfoNode.T {sourceNameIndex, successors, ...} =>
          {sourceNameIndex = sourceNameIndex,
           successorSourceSeqIndex = (sourceSeqIndex
                                      (List.revMap (!successors, fn infoNode =>
                                                    {sourceIndex = InfoNode.sourceIndex infoNode})))})
      (* Making sourceSeqs must happen after making sources,
       * since making sources creates new sourceSeqs.
       *)
      val sourceSeqs = Vector.fromListRev (!sourceSeqs)
      val profileInfo =
         ProfileInfo.T {profileLabelInfos = profileLabelInfos,
                        sourceNames = sourceNames,
                        sourceSeqs = sourceSeqs,
                        sources = sources}
   in
      Program.T {functions = functions,
                 handlesSignals = handlesSignals,
                 main = main,
                 objectTypes = objectTypes,
                 profileInfo = SOME (profileInfo, getFrameSourceSeqIndex)}
   end

end
