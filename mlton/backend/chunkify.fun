(* Copyright (C) 2019 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Chunkify (S: CHUNKIFY_STRUCTS): CHUNKIFY = 
struct

open S
datatype z = datatype Transfer.t

(* A simple chunkifier that puts all code in the same chunk. *)
fun one (Program.T {functions, main, ...}) =
   let
      val functions = main :: functions
   in
      Vector.new1
      {funcs = Vector.fromListMap (functions, Function.name),
       labels = Vector.concatV (Vector.fromListMap
                                (functions, fn f =>
                                 Vector.map (Function.blocks f, Block.label)))}
   end

(* A chunkifier that puts each function in its own chunk. *)
fun perFunc (Program.T {functions, main, ...}) =
   Vector.fromListMap
   (main :: functions, fn f =>
    let
       val {name, blocks, ...} = Function.dest f
    in
       {funcs = Vector.new1 name,
        labels = Vector.map (blocks, Block.label)}
    end)


fun blockSize (Block.T {statements, transfer, ...}): int =
   let
      val transferSize =
         case transfer of
            Switch (Switch.T {cases, ...}) => 1 + Vector.length cases
          | _ => 1
      val statementsSize =
         if !Control.profile = Control.ProfileNone
            then Vector.length statements
         else Vector.fold (statements, 0, fn (s, ac) =>
                           case s of
                              Statement.ProfileLabel _ => ac
                            | _ => 1 + ac)
   in
      statementsSize + transferSize
   end

structure Graph = EquivalenceGraph
structure Class = Graph.Class
fun coalesce (program as Program.T {functions, main, ...}, limit) =
   let
      val functions = main :: functions
      val graph = Graph.new ()
      val {get = funcClass: Func.t -> Class.t, set = setFuncClass,
           rem = remFuncClass, ...} =
         Property.getSetOnce (Func.plist,
                              Property.initRaise ("class", Func.layout))
      val {get = labelClass: Label.t -> Class.t, set = setLabelClass,
           rem = remLabelClass, ...} =
         Property.getSetOnce (Label.plist,
                              Property.initRaise ("class", Label.layout))
      (* Build the initial partition.
       * Ensure that all Ssa labels that jump to one another are in the same
       * equivalence class.
       *)
      val _ =
         List.foreach
         (functions, fn f =>
          let
             val {name, blocks, start, ...} = Function.dest f
             val _ =
                Vector.foreach
                (blocks, fn b as Block.T {label, ...} =>
                 setLabelClass (label,
                                Graph.newClass (graph, {size = blockSize b})))
             val _ = setFuncClass (name, labelClass start)
             val _ =
                Vector.foreach
                (blocks, fn Block.T {label, transfer, ...} =>
                 let
                    val c = labelClass label
                    fun same (j: Label.t): unit =
                       Graph.== (graph, c, labelClass j)
                 in
                    case transfer of
                       CCall {return, ...} => Option.app (return, same)
                     | Goto {dst, ...} => same dst
                     | Switch s => Switch.foreachLabel (s, same)
                     | _ => ()
                 end)
          in
             ()
          end)
      val rflow = Program.rflow program
      val returnsTo = #returnsTo o rflow
      (* Add edges, and then coalesce the graph. *)
      val _ =
         List.foreach
         (functions, fn f =>
          let
             val {name, blocks, ...} = Function.dest f
             val returnsTo = List.revMap (returnsTo name, labelClass)
             val _ =
                Vector.foreach
                (blocks, fn Block.T {label, transfer, ...} =>
                 case transfer of
                    Call {func, ...} =>
                       Graph.addEdge (graph, labelClass label,
                                      funcClass func)
                  | Return _ =>
                       let
                          val from = labelClass label
                       in
                          List.foreach
                          (returnsTo, fn c =>
                           Graph.addEdge (graph, from, c))
                       end
                  | _ => ())
          in
              ()
          end)
      val _ =
         if limit = 0
            then ()
         else Graph.coarsen (graph, {maxClassSize = limit})
      type chunk = {funcs: Func.t list ref,
                    labels: Label.t list ref}
      val chunks: chunk list ref = ref []
      val {get = classChunk: Class.t -> chunk, ...} =
         Property.get
         (Class.plist,
          Property.initFun (fn _ =>
                            let
                               val c = {funcs = ref [],
                                        labels = ref []}
                               val _ = List.push (chunks, c)
                            in
                               c
                            end))
      val _ =
         let
            fun 'a new (l: 'a,
                        get: 'a -> Class.t,
                        sel: chunk -> 'a list ref): unit =
               List.push (sel (classChunk (get l)), l)
            val _ =
               List.foreach
               (functions, fn f =>
                let
                   val {name, blocks, ...} = Function.dest f
                   val _ = new (name, funcClass, #funcs)
                   val _ =
                      Vector.foreach
                      (blocks, fn Block.T {label, ...} =>
                       new (label, labelClass, #labels))
                in ()
                end)
         in ()
         end
      val _ =
         List.foreach
         (functions, fn f =>
          let
             val {blocks, name, ...} = Function.dest f
             val _ = remFuncClass name
             val _ = Vector.foreach (blocks, remLabelClass o Block.label)
          in
             ()
          end)
   in 
      Vector.fromListMap (!chunks, fn {funcs, labels} =>
                          {funcs = Vector.fromList (!funcs),
                           labels = Vector.fromList (!labels)})
   end

structure Class =
   struct
      type t = PropertyList.t DisjointSet.t
      val new = DisjointSet.singleton o PropertyList.new
      val plist = DisjointSet.!
      val == = DisjointSet.union
   end
structure Graph = DirectedGraph
structure Node = Graph.Node
fun simple (program as Program.T {functions, main, ...}) =
   let
      val functions = main :: functions
      val {get = funcInfo: Func.t -> {class: Class.t,
                                      function: Function.t,
                                      node: unit Node.t},
           set = setFuncInfo,
           rem = remFuncInfo, ...} =
         Property.getSetOnce (Func.plist,
                              Property.initRaise ("Chunkify.simple.funcInfo", Func.layout))
      val funcClass = #class o funcInfo
      val funcFunction = #function o funcInfo
      val funcNode = #node o funcInfo
      val {get = labelInfo: Label.t -> {class: Class.t,
                                        func: Func.t},
           set = setLabelInfo,
           rem = remLabelInfo, ...} =
         Property.getSetOnce (Label.plist,
                              Property.initRaise ("Chunkify.simple.labelInfo", Label.layout))
      val labelClass = #class o labelInfo
      val labelFunc = #func o labelInfo
      val {get = nodeInfo: unit Node.t -> {func: Func.t},
           set = setNodeInfo, ...} =
         Property.getSetOnce (Node.plist,
                              Property.initRaise ("Chunkify.simple.nodeInfo", Node.layout))
      val nodeFunc = #func o nodeInfo
      val cgraph = Graph.new ()
      val _ =
         List.foreach
         (functions, fn f =>
          let
             val {name, blocks, start, ...} = Function.dest f
             val _ =
                Vector.foreach
                (blocks, fn Block.T {label, ...} =>
                 setLabelInfo (label, {class = Class.new (),
                                       func = name}))
             val node = Graph.newNode cgraph
             val _ = setNodeInfo (node, {func = name})
             val _ = setFuncInfo (name, {class = labelClass start,
                                         function = f,
                                         node = node})
          in
             ()
         end)
      (* Place src and dst blocks of intraprocedural transfers in same chunks. *)
      val _ =
         List.foreach
         (functions, fn f =>
          Vector.foreach
          (Function.blocks f, fn Block.T {label, transfer, ...} =>
           let
              val c = labelClass label
              fun same (j: Label.t): unit =
                 Class.== (c, labelClass j)
           in
              case transfer of
                 CCall {return, ...} => Option.app (return, same)
               | Goto {dst, ...} => same dst
               | Switch s => Switch.foreachLabel (s, same)
               | _ => ()
           end))
      (* Build interprocedural call graph. *)
      val _ =
         List.foreach
         (functions, fn f =>
          let
             val {name, blocks, ...} = Function.dest f
             val node = funcNode name
          in
             Vector.foreach
             (blocks, fn Block.T {transfer, ...} =>
              case transfer of
                 Call {func, ...} =>
                    (ignore o Graph.addEdge)
                    (cgraph, {from = node, to = funcNode func})
               | _ => ())
          end)
      (* Compute rflow. *)
      val rflow = Program.rflow program
      val returnsTo = #returnsTo o rflow
      val raisesTo = #raisesTo o rflow
      (* Place src and dst blocks of SCC calls/raises/returns in same chunks. *)
      val _ =
         List.foreach
         (Graph.stronglyConnectedComponents cgraph, fn nodes =>
          let
             val funcs = List.map (nodes, nodeFunc)
             fun funcInSCC f =
                List.exists (funcs, fn f' => Func.equals (f, f'))
             fun labelInSCC l =
                funcInSCC (labelFunc l)
          in
             List.foreach
             (funcs, fn f =>
              let
                 val {name, blocks, ...} = Function.dest (funcFunction f)
                 fun mkRTo rTo = List.revKeepAllMap (rTo name, fn l =>
                                                     if labelInSCC l
                                                        then SOME (labelClass l)
                                                        else NONE)
                 val returnsTo = mkRTo returnsTo
                 val raisesTo = mkRTo raisesTo
                 fun eqRTo (l, rTo) =
                    let val lc = labelClass l
                    in List.foreach (rTo, fn rlc => Class.== (lc, rlc))
                    end
              in
                 Vector.foreach
                 (blocks, fn Block.T {label, transfer, ...} =>
                  case transfer of
                     Call {func, ...} =>
                        if funcInSCC func
                           then Class.== (labelClass label, funcClass func)
                           else ()
                   | Raise _ => eqRTo (label, raisesTo)
                   | Return _ => eqRTo (label, returnsTo)
                   | _ => ())
              end)
          end)
      type chunk = {funcs: Func.t list ref,
                    labels: Label.t list ref}
      val chunks: chunk list ref = ref []
      val {get = classChunk: Class.t -> chunk, ...} =
         Property.get
         (Class.plist,
          Property.initFun (fn _ =>
                            let
                               val c = {funcs = ref [],
                                        labels = ref []}
                               val _ = List.push (chunks, c)
                            in
                               c
                            end))
      val _ =
         let
            fun 'a add (l: 'a,
                        get: 'a -> Class.t,
                        sel: chunk -> 'a list ref): unit =
               List.push (sel (classChunk (get l)), l)
            val _ =
               List.foreach
               (functions, fn f =>
                let
                   val {name, blocks, ...} = Function.dest f
                   val _ = add (name, funcClass, #funcs)
                   val _ = remFuncInfo name
                   val _ =
                      Vector.foreach
                      (blocks, fn Block.T {label, ...} =>
                       (add (label, labelClass, #labels)
                        ; remLabelInfo label))
                in ()
                end)
         in ()
         end
   in
      Vector.fromListMap (!chunks, fn {funcs, labels} =>
                          {funcs = Vector.fromList (!funcs),
                           labels = Vector.fromList (!labels)})
   end

fun chunkify p =
   case !Control.chunkify of
      Control.Chunkify.Coalesce {limit} => coalesce (p, limit)
    | Control.Chunkify.One => one p
    | Control.Chunkify.PerFunc => perFunc p
    | Control.Chunkify.Simple => simple p

val chunkify =
   fn p =>
   let
      val chunks = chunkify p
      val _ = 
         Control.diagnostics
         (fn display =>
          let
             open Layout
             val _ = display (str "Chunkification:")
             val _ =
                Vector.foreach
                (chunks, fn {funcs, labels} =>
                 display
                 (record ([("funcs", Vector.layout Func.layout funcs),
                           ("labels", Vector.layout Label.layout labels)])))
          in
             ()
          end)
   in
      chunks
   end

end
