(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Main : sig val main : unit -> unit end =
struct

type int = Int.t
type word = Word.t

val debug = false

val callGraphFile: File.t option ref = ref NONE
val gray: bool ref = ref false
val longName: bool ref = ref true
val mlmonFiles: string list ref = ref []
val raw = ref false
val showLine = ref false
val splitReg: Regexp.t ref = ref Regexp.none
val title: string option ref = ref NONE
val tolerant: bool ref = ref false

structure Source =
   struct
      datatype t =
         NamePos of {name: string,
                     pos: string}
       | Simple of string

      fun toString n =
         case n of
            NamePos {name, pos} => concat [name, "  ", pos]
          | Simple s => s

      fun toStringMaybeLine n =
         case n of
            NamePos {name, pos} =>
               if !showLine
                  then concat [name, "  ", pos]
               else name
          | Simple s => s

      val layout = Layout.str o toString

      fun fromString s =
         case String.tokens (s, fn c => Char.equals (c, #"\t")) of
            [s] => Simple s
          | [name, pos] =>
               let
                  val name =
                     if !longName
                        then name
                     else
                        List.last
                        (String.tokens (name, fn c => Char.equals (c, #".")))
               in
                  NamePos {name = name, pos = pos}
               end
          | _ => Error.bug "strange source"

      fun toDotLabel s =
         case s of
            NamePos {name, pos} =>
               if !showLine
                  then [(name, Dot.Center),
                        (pos, Dot.Center)]
               else [(name, Dot.Center)]
          | Simple s =>
               [(s, Dot.Center)]
   end

structure Graph = DirectedGraph
local
   open Graph
in
   structure Edge = Edge
   structure Node = Node
end
local
   open Dot
in
   structure EdgeOption = EdgeOption
   structure NodeOption = NodeOption
end

structure AFile =
   struct
      datatype t = T of {callGraph: unit Graph.t,
                         magic: word,
                         master: {isSplit: bool,
                                  source: Source.t} vector,
                         name: string,
                         split: {masterIndex: int,
                                 node: unit Node.t} vector}

      fun layout (T {magic, name, master, ...}) =
         Layout.record
         [("name", String.layout name),
          ("magic", Word.layout magic),
          ("master",
           Vector.layout (fn {isSplit, source} =>
                          Layout.record [("isSplit", Bool.layout isSplit),
                                         ("source", Source.layout source)])
           master)]

      fun new {afile: File.t}: t =
         let
            fun userBug m =
               Error.bug (concat ["Error: executable '", afile, "' ", m, "."])
         in
         if not (File.doesExist afile) then
            userBug "does not exist"
         else if not (File.canRun afile) then
            userBug "does not run"
         else
            Process.callWithIn
            (OS.Path.mkAbsolute {path = afile,
                                 relativeTo = OS.FileSys.getDir ()},
             ["@MLton", "show-sources"],
             fn ins =>
             let
                fun line () =
                   case In.inputLine ins of
                      NONE => Error.bug "unexpected end of show-sources data"
                    | SOME l => l
                val magic = 
                   case Word.fromString (line ()) of
                      NONE => Error.bug "expected magic"
                    | SOME w => w
                fun vector (f: string -> 'a): 'a vector =
                   Vector.tabulate (valOf (Int.fromString (line ())),
                                    fn _ => f (line ()))
                val rc = Regexp.compileNFA (!splitReg)
                val master =
                   vector
                   (fn s =>
                    let
                       val source = Source.fromString (String.dropSuffix (s, 1))
                       val isSplit =
                          Regexp.Compiled.matchesPrefix
                          (rc, Source.toString source)
                    in
                       {isSplit = isSplit,
                        source = source}
                    end)
                val _ =
                   if 0 = Vector.length master then
                      userBug "is not compiled for profiling"
                   else ()
                val sources =
                   vector
                   (fn s =>
                    case String.tokens (s, Char.isSpace) of
                       [masterIndex, successorsIndex] =>
                          {masterIndex = valOf (Int.fromString masterIndex),
                           successorsIndex = valOf (Int.fromString
                                                    successorsIndex)}
                     | _ => Error.bug "AFile.new")
                val sourceSeqs =
                   vector
                   (fn s =>
                    Vector.fromListMap
                    (String.tokens (s, Char.isSpace), fn s =>
                     valOf (Int.fromString s)))
                val graph = Graph.new ()
                val split =
                   Vector.map
                   (sources, fn {masterIndex, ...} =>
                    let
                       val n = Graph.newNode graph
                    in
                       {masterIndex = masterIndex,
                        node = n}
                    end)
                val _ =
                   Vector.foreach2
                   (sources, split,
                    fn ({successorsIndex, ...}, {node = from, ...}) =>
                    Vector.foreach
                    (Vector.sub (sourceSeqs, successorsIndex),
                     fn to =>
                     (ignore o Graph.addEdge)
                     (graph, {from = from,
                              to = #node (Vector.sub (split, to))})))
                val _ =
                   case In.inputLine ins of
                      NONE => ()
                    | SOME _ => Error.bug "expected end of file"
             in
                T {callGraph = graph,
                   magic = magic,
                   master = master,
                   name = afile,
                   split = split}
             end)
         end
   end

structure Kind =
   struct
      datatype t = Alloc | Count | Empty | Time

      val toString =
         fn Alloc => "Alloc"
          | Count => "Count"
          | Empty => "Empty"
          | Time => "Time"

      val layout = Layout.str o toString

      val merge: t * t -> t =
         fn (k, k') =>
         case (k, k') of
            (Alloc, Alloc) => Alloc
          | (Count, Count) => Count
          | (_, Empty) => k
          | (Empty, _) => k'
          | (Time, Time) => Time
          | _ => Error.bug "Kind.merge"
   end

structure Style =
   struct
      datatype t = Current | Stack

      (* val toString = fn Current => "Current" | Stack => "Stack" *)

      (* val layout = Layout.str o toString *)
   end

structure Counts =
   struct
      datatype t =
         Current of {master: IntInf.t vector,
                     split: IntInf.t vector}
       | Empty
       | Stack of {master: {current: IntInf.t,
                            stack: IntInf.t,
                            stackGC: IntInf.t} vector,
                   split: {current: IntInf.t,
                           stack: IntInf.t,
                           stackGC: IntInf.t} vector}

      val layout =
         fn Current {master, split} =>
              Layout.record [("master", Vector.layout IntInf.layout master),
                             ("split", Vector.layout IntInf.layout split)]
          | Empty => Layout.str "empty"
          | Stack {master, split} =>
               let
                  fun lay v =
                     Vector.layout
                     (fn {current, stack, stackGC} =>
                      Layout.record [("current", IntInf.layout current),
                                     ("stack", IntInf.layout stack),
                                     ("stackGC", IntInf.layout stackGC)])
                     v
               in
                  Layout.record [("master", lay master),
                                 ("split", lay split)]
               end

      fun merge (c: t, c': t): t =
         case (c, c') of
            (Current {master = m, split = s},
             Current {master = m', split = s'}) =>
               let
                  fun merge (v, v') = Vector.map2 (v, v', op +)
               in
                  Current {master = merge (m, m'),
                           split = merge (s, s')}
               end
          | (Empty, _) => c'
          | (_, Empty) => c
          | (Stack {master = m, split = s}, Stack {master = m', split = s'}) =>
               let
                  fun merge (v, v') =
                     Vector.map2
                     (v, v', fn ({current = c, stack = s, stackGC = g},
                                 {current = c', stack = s', stackGC = g'}) =>
                      {current = c + c',
                       stack = s + s',
                       stackGC = g + g'})
               in
                  Stack {master = merge (m, m'),
                         split = merge (s, s')}
               end
          | _ =>
               Error.bug
               "cannot merge -profile-stack false with -profile-stack true"
   end

structure ProfFile =
   struct
      datatype t = T of {counts: Counts.t,
                         kind: Kind.t,
                         magic: word,
                         total: IntInf.t,
                         totalGC: IntInf.t}

      fun empty (AFile.T {magic, ...}) =
         T {counts = Counts.Empty,
            kind = Kind.Empty,
            magic = magic,
            total = 0,
            totalGC = 0}

      fun layout (T {counts, kind, magic, total, totalGC}) =
         Layout.record [("kind", Kind.layout kind),
                        ("magic", Word.layout magic),
                        ("total", IntInf.layout total),
                        ("totalGC", IntInf.layout totalGC),
                        ("counts", Counts.layout counts)]

      fun new {mlmonfile: File.t}: t =
         File.withIn
         (mlmonfile, fn ins =>
          let
             fun line () =
                case In.inputLine ins of
                   NONE => Error.bug "unexpected end of mlmon file"
                 | SOME s => String.dropSuffix (s, 1)
             val _ =
                if "MLton prof" = line ()
                   then ()
                else Error.bug "bad header"
             val kind =
                case line () of
                   "alloc" => Kind.Alloc
                 | "count" => Kind.Count
                 | "time" => Kind.Time
                 | _ => Error.bug "invalid profile kind"
             val style =
                case line () of
                   "current" => Style.Current
                 | "stack" => Style.Stack
                 | _ => Error.bug "invalid profile style"
             val magic =
                case Word.fromString (line ()) of
                   NONE => Error.bug "invalid magic"
                 | SOME w => w
             fun s2i s =
                case IntInf.fromString s of
                   NONE => Error.bug "invalid count"
                 | SOME i => i
             val (total, totalGC) =
                case String.tokens (line (), Char.isSpace) of
                   [total, totalGC] => (s2i total, s2i totalGC)
                 | _ => Error.bug "invalid totals"
             fun getCounts (f: string -> 'a): {master: 'a vector,
                                               split: 'a vector} =
                let
                   fun vector () =
                      Vector.tabulate (valOf (Int.fromString (line ())),
                                       fn _ => f (line ()))
                   val split = vector ()
                   val master = vector ()
                in
                   {master = master, split = split}
                end
             val counts =
                case style of
                   Style.Current => Counts.Current (getCounts s2i)
                 | Style.Stack =>
                      Counts.Stack
                      (getCounts
                       (fn s =>
                        case String.tokens (s, Char.isSpace) of
                           [c, s, sGC] =>
                              {current = s2i c,
                               stack = s2i s,
                               stackGC = s2i sGC}
                         | _ =>
                              Error.bug
                              (concat ["strange line: ",
                                       String.dropSuffix (s, 1)])))
          in
             T {counts = counts,
                kind = kind,
                magic = magic,
                total = total,
                totalGC = totalGC}
          end)

      fun merge (T {counts = c, kind = k, magic = m, total = t, totalGC = g},
                 T {counts = c', kind = k', magic = m', total = t',
                    totalGC = g'}): t =
         if m <> m'
            then Error.bug "wrong magic number"
         else
            T {counts = Counts.merge (c, c'),
               kind = Kind.merge (k, k'),
               magic = m,
               total = t + t',
               totalGC = g + g'}
   end

structure Atomic =
   struct
      datatype t =
         Name of string * Regexp.Compiled.t
       | Thresh of real
       | ThreshGC of real
       | ThreshStack of real

      val toSexp: t -> Sexp.t =
         fn a =>
         let
            datatype z = datatype Sexp.t
         in
            case a of
               Name (s, _) => String s
             | Thresh x => List [Atom "thresh", Atom (Real.toString x)]
             | ThreshGC x => List [Atom "thresh-gc", Atom (Real.toString x)]
             | ThreshStack x =>
                  List [Atom "thresh-stack", Atom (Real.toString x)]
         end
   end

structure NodePred =
   struct
      datatype t =
         All
       | And of t vector
       | Atomic of Atomic.t
       | Not of t
       | Or of t vector
       | PathFrom of t
       | PathTo of t
       | Pred of t
       | Succ of t

      val rec toSexp: t -> Sexp.t =
         fn p =>
         let
            datatype z = datatype Sexp.t
            fun nAry (name, ps) =
               List (Atom name :: Vector.toListMap (ps, toSexp))
            fun unary (name, p) =
               List [Atom name, toSexp p]
         in
            case p of
               All => Sexp.Atom "all"
             | And ps => nAry ("and", ps)
             | Atomic a => Atomic.toSexp a
             | Not p => unary ("not", p)
             | Or ps => nAry ("or", ps)
             | PathFrom p => unary ("from", p)
             | PathTo p => unary ("to", p)
             | Pred p => unary ("pred", p)
             | Succ p => unary ("succ", p)
         end

      (* val layout = Sexp.layout o toSexp *)

      val fromString: string -> t =
         fn s =>
         case Sexp.fromString s of
            Sexp.Eof => Error.bug "empty"
          | Sexp.Error s => Error.bug s
          | Sexp.Sexp s =>
               let
                  fun parse (s: Sexp.t): t =
                     let
                        fun err () = Error.bug (Sexp.toString s)
                     in                    
                        case s of
                           Sexp.Atom s =>
                              (case s of
                                  "all" => All
                                | _ => err ())
                         | Sexp.List ss =>
                              (case ss of
                                  [] => err ()
                                | s :: ss =>
                                     let
                                        fun nAry f =
                                           f (Vector.fromListMap (ss, parse))
                                        fun unary f =
                                           case ss of
                                              [s] => f (parse s)
                                            | _ => err ()
                                        fun thresh f =
                                           case ss of
                                              [Sexp.Atom x] =>
                                                 (case Real.fromString x of
                                                     NONE => err ()
                                                   | SOME x =>
                                                        if 0.0 <= x
                                                           andalso x <= 100.0
                                                           then Atomic (f x)
                                                        else err ())
                                            | _ => err ()
                                        datatype z = datatype Atomic.t
                                     in
                                        case s of
                                           Sexp.Atom s =>
                                              (case s of
                                                  "and" => nAry And
                                                | "from" => unary PathFrom
                                                | "not" => unary Not
                                                | "or" => nAry Or
                                                | "pred" => unary Pred
                                                | "succ" => unary Succ
                                                | "thresh" => thresh Thresh
                                                | "thresh-gc" => thresh ThreshGC
                                                | "thresh-stack" =>
                                                     thresh ThreshStack
                                                | "to" => unary PathTo
                                                | _ => err ())
                                         | _ => err ()
                                     end)
                         | Sexp.String s =>
                              (case Regexp.fromString s of
                                  NONE => err ()
                                | SOME (r, _) =>
                                     Atomic
                                     (Atomic.Name (s, Regexp.compileNFA r)))
                     end
               in
                  parse s
               end

      fun nodes (p: t, g: 'a Graph.t,
                 atomic: 'a Node.t * Atomic.t -> bool): 'a Node.t vector =
         let
            val {get = nodeIndex: 'a Node.t -> int,
                 set = setNodeIndex, ...} =
               Property.getSet (Node.plist,
                                Property.initRaise ("index", Node.layout))
            val nodes = Vector.fromList (Graph.nodes g)
            val numNodes = Vector.length nodes
            val _ = Vector.foreachi (nodes, fn (i, n) => setNodeIndex (n, i))
            val transpose =
               Promise.lazy
               (fn () =>
                let
                   val {get = nodeIndex': 'a Graph.u Node.t -> int,
                        set = setNodeIndex, ...} =
                      Property.getSet (Node.plist,
                                       Property.initRaise ("index", Node.layout))
                   val (transpose, {newNode, ...}) = Graph.transpose g
                   val _ =
                      Graph.foreachNode
                      (g, fn n => setNodeIndex (newNode n, nodeIndex n))
                in
                   (transpose, newNode, nodeIndex')
                end)
            fun vectorToNodes (v: bool vector): 'a Node.t vector =
               Vector.keepAllMapi
               (v, fn (i, b) =>
                if b
                   then SOME (Vector.sub (nodes, i))
                else NONE)
            val all = Promise.lazy (fn () =>
                                    Vector.tabulate (numNodes, fn _ => true))
            val none = Promise.lazy (fn () =>
                                     Vector.tabulate (numNodes, fn _ => false))
            fun path (v: bool vector,
                      (g: 'b Graph.t,
                       getNode: 'a Node.t -> 'b Node.t,
                       nodeIndex: 'b Node.t -> int)): bool vector =
               let
                  val roots = vectorToNodes v
                  val a = Array.array (numNodes, false)
                  val _ =
                     Graph.dfsNodes
                     (g,
                      Vector.toListMap (roots, getNode),
                      Graph.DfsParam.startNode (fn n =>
                                                Array.update
                                                (a, nodeIndex n, true)))
               in
                  Vector.fromArray a
               end
            fun loop (p: t): bool vector =
               case p of
                  All => all ()
                | And ps =>
                     Vector.fold (ps, all (), fn (p, v) =>
                                  Vector.map2 (v, loop p, fn (b, b') =>
                                               b andalso b'))
                | Atomic a => Vector.map (nodes, fn n => atomic (n, a))
                | Not p => Vector.map (loop p, not)
                | Or ps =>
                     Vector.fold (ps, none (), fn (p, v) =>
                                  Vector.map2 (v, loop p, fn (b, b') =>
                                               b orelse b'))
                | PathFrom p => path (loop p, (g, fn n => n, nodeIndex))
                | PathTo p => path (loop p, transpose ())
                | Pred p =>
                     let
                        val ns = vectorToNodes (loop p)
                        val {destroy, get, set, ...}  =
                           Property.destGetSetOnce
                           (Node.plist, Property.initConst false)
                        val _ = Vector.foreach (ns, fn n => set (n, true))
                        val v =
                           Vector.map
                           (nodes, fn n =>
                            get n orelse
                            List.exists (Node.successors n, get o Edge.to))
                        val _ = destroy ()
                     in
                        v
                     end
                | Succ p =>
                     let
                        val a = Array.array (numNodes, false)
                        fun yes n = Array.update (a, nodeIndex n, true)
                        val _ =
                           Vector.foreach
                           (vectorToNodes (loop p), fn n =>
                            (yes n
                             ; List.foreach (Node.successors n, yes o Edge.to)))
                     in
                        Vector.fromArray a
                     end
            val v = loop p
         in
            vectorToNodes v
         end
   end

val keep: NodePred.t ref = ref NodePred.All

val ticksPerSecond = 100.0

fun display (AFile.T {callGraph, master, name = aname, split, ...},
             ProfFile.T {counts, kind, total, totalGC, ...}): unit =
   let
      val {get = nodeInfo: (unit Node.t
                            -> {index: int,
                                keep: bool ref,
                                mayKeep: (Atomic.t -> bool) ref}),
           set = setNodeInfo, ...} =
         Property.getSetOnce (Node.plist,
                              Property.initRaise ("info", Node.layout))
      val _ =
         Vector.foreachi (split, fn (i, {node, ...}) =>
                          setNodeInfo (node,
                                       {index = i,
                                        keep = ref false,
                                        mayKeep = ref (fn _ => false)}))
      val profileStack =
         case counts of
            Counts.Current _ => false
          | Counts.Empty => false
          | Counts.Stack _ => true
      val totalReal = Real.fromIntInf (total + totalGC)
      val per: IntInf.t -> real =
         if Real.equals (0.0, totalReal)
            then fn _ => 0.0
         else
            fn ticks => 100.0 * Real.fromIntInf ticks / totalReal
      fun doit ({master = masterCount: 'a vector,
                 split = splitCount: 'a vector},
                f: 'a -> {current: IntInf.t,
                          stack: IntInf.t,
                          stackGC: IntInf.t}) =
         let
            val _ =
               Vector.foreachi
               (split, fn (i, {masterIndex, node, ...}) =>
                let
                   val {mayKeep, ...} = nodeInfo node
                   val {isSplit, source, ...} = Vector.sub (master, masterIndex)
                   val name = Source.toString source
                in
                   mayKeep :=
                   (fn a =>
                    let
                       fun thresh (x: real, sel) =
                          let
                             val (v, i) =
                                if isSplit
                                   then (splitCount, i)
                                else (masterCount, masterIndex)
                          in
                             per (sel (f (Vector.sub (v, i)))) >= x
                          end
                       datatype z = datatype Atomic.t
                    in
                       case a of
                          Name (_, rc) =>
                             Regexp.Compiled.matchesPrefix (rc, name)
                        | Thresh x => thresh (x, #current)
                        | ThreshGC x => thresh (x, #stackGC)
                        | ThreshStack x => thresh (x, #stack)
                    end)
                end)
            fun row (ticks: IntInf.t): string list =
               (concat [Real.format (per ticks, Real.Format.fix (SOME 1)), "%"])
               :: (if !raw
                      then
                         [concat
                          (case kind of
                              Kind.Alloc =>
                                 ["(", IntInf.toCommaString ticks, ")"]
                            | Kind.Count =>
                                 ["(", IntInf.toCommaString ticks, ")"]
                            | Kind.Empty => []
                            | Kind.Time =>
                                 ["(",
                                  Real.format
                                  (Real.fromIntInf ticks / ticksPerSecond,
                                   Real.Format.fix (SOME 2)),
                                  "s)"])]
                   else [])
            fun info (source: Source.t, a: 'a) =
               let
                  val {current, stack, stackGC} = f a
                  val row =
                     row current
                     @ (if profileStack
                           then row stack @ row stackGC
                        else [])
                  val pc = per current
                  val isNonZero = current > 0 orelse stack > 0 orelse stackGC > 0
                  val tableInfo = 
                     if isNonZero orelse (kind = Kind.Count
                                          andalso (case source of
                                                      Source.NamePos _ => true
                                                    | _ => false))
                        then SOME {per = pc,
                                   row = Source.toStringMaybeLine source :: row}
                     else NONE
                  val nodeOptions =
                     [Dot.NodeOption.Shape Dot.Box,
                      Dot.NodeOption.Label
                      (Source.toDotLabel source
                       @ (if isNonZero
                             then [(concat (List.separate (row, " ")),
                                    Dot.Center)]
                          else [])),
                      Dot.NodeOption.Color
                      (if !gray
                          then DotColor.gray (100 - Real.round (per stack))
                       else DotColor.Black)]
               in
                  {nodeOptions = nodeOptions,
                   tableInfo = tableInfo}
               end
            val masterOptions =
               Vector.map2
               (master, masterCount, fn ({source, ...}, a) =>
                info (source, a))
            val splitOptions =
               Vector.map2
               (split, splitCount, fn ({masterIndex, ...}, a) =>
                info (#source (Vector.sub (master, masterIndex)), a))
         in
            (masterOptions, splitOptions)
         end
      val (masterInfo, splitInfo) =
         case counts of
            Counts.Current ms =>
               doit (ms, fn z => {current = z,
                                  stack = 0,
                                  stackGC = 0})
          | Counts.Empty =>
               doit ({master = Vector.new (Vector.length master, ()),
                      split = Vector.new (Vector.length split, ())},
                     fn () => {current = 0,
                               stack = 0,
                               stackGC = 0})
          | Counts.Stack ms =>
               doit (ms, fn z => z)
      val keep = !keep
      val keepNodes =
         NodePred.nodes
         (keep, callGraph, fn (n, a) => (! (#mayKeep (nodeInfo n))) a)
      val _ = Vector.foreach (keepNodes, fn n =>
                              #keep (nodeInfo n) := true)
      (* keep a master node if it is not split and some copy of it is kept. *)
      val keepMaster = Array.new (Vector.length master, false)
      val _ =
         Vector.foreach
         (split, fn {masterIndex, node, ...} =>
          let
             val {keep, ...} = nodeInfo node
             val {isSplit, ...} = Vector.sub (master, masterIndex)
          in
             if !keep andalso not isSplit
                then Array.update (keepMaster, masterIndex, true)
             else ()
          end)
      datatype keep = T
      val keepGraph: keep Graph.t = Graph.new ()
      val {get = nodeOptions: keep Node.t -> NodeOption.t list,
           set = setNodeOptions, ...} =
         Property.getSetOnce (Node.plist,
                              Property.initRaise ("options", Node.layout))
      val tableInfos = ref []
      fun newNode {nodeOptions: NodeOption.t list,
                   tableInfo} =
         let
            val _ = Option.app (tableInfo, fn z => List.push (tableInfos, z))
            val n = Graph.newNode keepGraph
            val _ = setNodeOptions (n, nodeOptions)
         in
            n
         end
      val masterNodes =
         Vector.tabulate
         (Vector.length master, fn i =>
          if Array.sub (keepMaster, i)
             then SOME (newNode (Vector.sub (masterInfo, i)))
          else NONE)
      val splitNodes =
         Vector.mapi
         (split, fn (i, {masterIndex, node, ...}) =>
          let
             val {keep, ...} = nodeInfo node
             val {isSplit, ...} = Vector.sub (master, masterIndex)
          in
             if isSplit
                then
                   if !keep
                      then SOME (newNode (Vector.sub (splitInfo, i)))
                   else NONE
             else Vector.sub (masterNodes, masterIndex)
          end)
      val _ =
         Graph.foreachEdge
         (callGraph, fn (from, e) =>
          let
             val to = Edge.to e
             fun f n = Vector.sub (splitNodes, #index (nodeInfo n))
          in
             case (f from, f to) of
                (SOME from, SOME to) =>
                   (ignore o Graph.addEdge) 
                   (keepGraph, {from = from, to = to})
              | _ => ()
          end)
      val {get = edgeOptions: keep Edge.t -> EdgeOption.t list ref, ...} =
         Property.get (Edge.plist, Property.initFun (fn _ => ref []))
      (* Add a dashed edge from A to B if there is path from A to B of length
       * >= 2 going through only ignored nodes.
       *)
      fun newNode (n: unit Node.t): keep Node.t option =
         Vector.sub (splitNodes, #index (nodeInfo n))
      fun reach (root: unit Node.t, f: keep Node.t -> unit): unit =
         let
            val {get = isKept: keep Node.t -> bool ref, ...} =
               Property.get (Node.plist, Property.initFun (fn _ => ref false))
            val {get = isSeen: unit Node.t -> bool ref, ...} =
               Property.get (Node.plist, Property.initFun (fn _ => ref false))
            fun loop n =
               List.foreach
               (Node.successors n, fn e =>
                let
                   val n = Edge.to e
                   val s = isSeen n
                in
                   if !s
                      then ()
                   else
                      let
                         val _ = s := true
                      in
                         case newNode n of
                            NONE => loop n
                          | SOME keepN => 
                               let
                                  val r = isKept keepN
                               in
                                  if !r
                                     then ()
                                  else (r := true; f keepN)
                               end
                      end
                end)
            val _ =
               List.foreach (Node.successors root, fn e =>
                             let
                                val n = Edge.to e
                             in
                                if Option.isNone (newNode n)
                                   then loop n
                                else ()
                             end)
         in
            ()
         end
      val _ =
         Vector.foreach2
         (split, splitNodes, fn ({node = from, ...}, z) =>
          Option.app
          (z, fn from' =>
           (reach (from, fn to =>
                   let
                      val e = Graph.addEdge (keepGraph, {from = from', to = to})
                      val _ = List.push (edgeOptions e,
                                         EdgeOption.Style Dot.Dashed)
                   in
                      ()
                   end))))
      val _ = Graph.removeDuplicateEdges keepGraph
      val title =
         case !title of
            NONE => concat [aname, " call-stack graph"]
          | SOME s => s
      val _ =
         Option.app
         (!callGraphFile, fn f =>
          File.withOut
          (f, fn out =>
           Layout.output
           (Graph.layoutDot (keepGraph,
                             fn _ => {edgeOptions = ! o edgeOptions,
                                      nodeOptions = nodeOptions,
                                      options = [],
                                      title = title}),
            out)))
      (* Display the table. *)
      val tableRows =
         QuickSort.sortVector
         (Vector.fromList (!tableInfos), fn (z, z') => #per z >= #per z')
      val _ = 
         print
         (concat
          (case kind of
              Kind.Alloc =>
                 [IntInf.toCommaString total, " bytes allocated (",
                  IntInf.toCommaString totalGC, " bytes by GC)\n"]
            | Kind.Count =>
                 [IntInf.toCommaString total, " ticks\n"]
            | Kind.Empty => []
            | Kind.Time =>
                 let
                    fun t2s i = 
                       Real.format (Real.fromIntInf i / ticksPerSecond, 
                                    Real.Format.fix (SOME 2))
                 in
                    [t2s total, " seconds of CPU time (",
                     t2s totalGC, " seconds GC)\n"]
                 end))
      val columnHeads =
         "function"
         :: let
               val pers =
                  if profileStack
                     then ["cur", "stack", "GC"]
                  else ["cur"]
            in
               if !raw
                  then List.concatMap (pers, fn p => [p, "raw"])
               else pers
            end
      val cols =
         (if profileStack then 3 else 1) * (if !raw then 2 else 1)
      val _ =
         let
            open Justify
         in
            outputTable
            (table {columnHeads = SOME columnHeads,
                    justs = Left :: List.duplicate (cols, fn () => Right),
                    rows = Vector.toListMap (tableRows, #row)},
             Out.standard)
         end
   in
      ()
   end

fun makeOptions {usage} =
   let
      open Popt
   in
      List.map
      ([(Normal, "call-graph", " <file>", "write call graph to dot file",
         SpaceString (fn s => callGraphFile := SOME s)),
        (Normal, "graph-title", " <string>", "set call-graph title",
         SpaceString (fn s => title := SOME s)),
        (Normal, "gray", " {false|true}", "gray nodes according to stack %",
         boolRef gray),
        (Normal, "keep", " <exp>", "which functions to display",
         SpaceString (fn s =>
                      keep := NodePred.fromString s
                      handle e => usage (concat ["invalid -keep arg: ",
                                                 Exn.toString e]))),
        (Expert, "long-name", " {true|false}",
         " show long names of functions",
         boolRef longName),
        (Normal, "mlmon", " <file>", "process mlmon files listed in <file>",
         SpaceString (fn s =>
                      mlmonFiles :=
                      List.concat [String.tokens (File.contents s, Char.isSpace),
                                   !mlmonFiles])),
        (Normal, "raw", " {false|true}", "show raw counts",
         boolRef raw),
        (Normal, "show-line", " {false|true}", "show line numbers",
         boolRef showLine),
        (Normal, "split", " <regexp>", "split matching functions",
         SpaceString (fn s =>
                      case Regexp.fromString s of
                         NONE => usage (concat ["invalid -split regexp: ", s])
                       | SOME (r, _) => splitReg := Regexp.or [r, !splitReg])),
        (Normal, "thresh", " [0.0,100.0]", "-keep (thresh x)",
         Real (fn x => if x < 0.0 orelse x > 100.0
                         then usage "invalid -thresh"
                      else keep := NodePred.Atomic (Atomic.Thresh x))),
        (Normal, "tolerant", " {false|true}", "ignore broken mlmon files",
         boolRef tolerant)],
       fn (style, name, arg, desc, opt) =>
       {arg = arg, desc = desc, name = name, opt = opt, style = style})
   end

val mainUsage = "mlprof [option ...] a.out [mlmon.out ...]"
val {parse, usage} =
   Popt.makeUsage {mainUsage = mainUsage,
                   makeOptions = makeOptions,
                   showExpert = fn () => false}

val die = Process.fail

fun commandLine args =
   let
      val rest = parse args
    in
       case rest of
          Result.No msg => usage msg
        | Result.Yes (afile :: files) =>
             let
                val mlmonFiles = files @ !mlmonFiles 
                val aInfo = AFile.new {afile = afile}
                val _ =
                   if debug
                      then
                         (print "AFile:\n"
                          ; Layout.outputl (AFile.layout aInfo, Out.standard))
                   else ()
                val profFile =
                   List.fold
                   (mlmonFiles, ProfFile.empty aInfo,
                    fn (mlmonfile, profFile) =>
                    ProfFile.merge
                    (profFile, ProfFile.new {mlmonfile = mlmonfile})
                    handle e =>
                       let
                          val msg =
                             concat ["Error loading mlmon file '", mlmonfile,
                                     "': ", Exn.toString e]
                       in
                          if !tolerant
                             then
                                (Out.outputl (Out.error, msg)
                                 ; profFile)
                          else die msg
                       end)
                val _ =
                   if debug
                      then
                         (print "ProfFile:\n"
                          ; Layout.outputl (ProfFile.layout profFile,
                                            Out.standard))
                   else ()
                val _ = display (aInfo, profFile)
             in
                ()
             end
        | Result.Yes _ => usage "wrong number of args"
   end

val main = Process.makeMain commandLine

end
