(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor amd64LoopInfo(S: AMD64_LOOP_INFO_STRUCTS) : AMD64_LOOP_INFO =
struct
  open S
  open amd64

  structure Graph = DirectedGraph
  structure Node = Graph.Node
  structure LoopForest = Graph.LoopForest

  val tracer = amd64.tracer

  datatype t = T of {getLoopInfo : Label.t ->
                                   {loopHeader: bool,
                                    loopLabels: Label.t list,
                                    loopPath: int list}}

  fun createLoopInfo {chunk = Chunk.T {blocks, ...}, farLoops}
    = let
        val G = Graph.new ()

        val {get = getNodeInfo : unit Node.t -> Label.t,
             set = setNodeInfo, ...}
          = Property.getSetOnce
            (Node.plist,
             Property.initRaise ("amd64LoopInfo:getNodeInfo", Node.layout))

        val {get = getInfo : Label.t -> unit Node.t,
             destroy = destInfo}
          = Property.destGet
            (Label.plist,
             Property.initFun (fn l => let
                                         val n = Graph.newNode G
                                         val _ = setNodeInfo(n, l)
                                       in
                                         n
                                       end))

        val {get = getLoopInfo : 
                   Label.t -> 
                   {loopHeader: bool,
                    loopLabels: Label.t list,
                    loopPath: int list},
             set = setLoopInfo, ...}
          = Property.getSetOnce
            (Label.plist,
             Property.initRaise ("amd64LoopInfo:getLoopInfo", Label.layout))

        val rootLabel = Label.newString "root"
        val root = getInfo rootLabel

        fun addEdge edge
          = ignore (Graph.addEdge (G, edge))

        val _
          = List.foreach
            (blocks,
             fn Block.T {entry, transfer, ...}
              => let
                   val label = Entry.label entry
                   val node = getInfo label

                   fun doit' target
                     = let
                         val node' = getInfo target
                       in
                         addEdge {from = node, to = node'}
                       end
                   fun doit'' target
                     = let
                         val node' = getInfo target
                       in
                         if farLoops
                           then addEdge {from = node, to = node'}
                           else addEdge {from = root, to = node'}
                       end

                   datatype z = datatype Transfer.t
                 in
                   if Entry.isFunc entry
                     then addEdge {from = root, to = node}
                     else () ;
                   case transfer
                     of Goto {target, ...} 
                      => doit' target
                      | Iff {truee, falsee, ...} 
                      => (doit' truee; 
                          doit' falsee)
                      | Switch {cases, default, ...}
                      => (doit' default;
                          Transfer.Cases.foreach(cases, doit' o #2))
                      | Tail {...}
                      => ()
                      | NonTail {return, handler, ...}
                      => (doit'' return;
                          case handler 
                            of SOME handler => doit'' handler
                             | NONE => ())
                      | Return {...}
                      => ()
                      | Raise {...}
                      => ()
                      | CCall {return, func, ...}
                      => Option.app (return, if CFunction.mayGC func
                                               then doit''
                                               else doit')
                 end)
        val _ = destInfo ()

        val lf = Graph.loopForestSteensgaard (G, {root = root})

        fun doit (f: unit LoopForest.t,
                  headers,
                  path)
          = let
              val {loops, notInLoop} = LoopForest.dest f
              val notInLoop = Vector.toListMap (notInLoop, getNodeInfo)
              val path' = List.rev path
            in
              List.foreach
              (notInLoop, fn l =>
               setLoopInfo 
               (l, {loopHeader = Vector.contains (headers, l, Label.equals),
                    loopLabels = notInLoop,
                    loopPath = path'})) ;
              Vector.foreachi
              (loops, fn (i,{headers, child}) =>
               doit (child, 
                     Vector.map (headers, getNodeInfo),
                     i::path))
            end
        val _ = doit (lf, Vector.new0 (), [])
      in
        T {getLoopInfo = getLoopInfo}
      end

  val (createLoopInfo, createLoopInfo_msg)
    = tracer
      "createLoopInfo"
      createLoopInfo

  fun getLoopDistance (T {getLoopInfo, ...}, from, to)
    = (case (#loopPath (getLoopInfo from), #loopPath (getLoopInfo to))
         of ([], _) => NONE
          | (_, []) => NONE
          | (pfrom, pto)
          => let
               val rec check
                 = fn ([], pto) => SOME (List.length pto)
                    | (pfrom, []) => SOME (~(List.length pfrom))
                    | (f::pfrom,t::pto)
                    => if f = t
                         then check (pfrom, pto)
                         else NONE
             in
               check (pfrom, pto)
             end)
  fun getLoopLabels (T {getLoopInfo, ...}, label) = #loopLabels (getLoopInfo label)
  fun isLoopHeader (T {getLoopInfo, ...}, l) = #loopHeader (getLoopInfo l)
end
