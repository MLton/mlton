(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
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

(* A chunkifier that puts each function in its own chunk. *)
fun chunkPerFunc (Program.T {functions, main, ...}) =
   Vector.fromListMap
   (main :: functions, fn f =>
    let
       val {name, blocks, ...} = Function.dest f
    in
       {funcs = Vector.new1 name,
        labels = Vector.map (blocks, Block.label)}
    end)

(* A simple chunkifier that puts all code in the same chunk.
 *)
fun oneChunk (Program.T {functions, main, ...}) =
   let
      val functions = main :: functions
   in
      Vector.new1
      {funcs = Vector.fromListMap (functions, Function.name),
       labels = Vector.concatV (Vector.fromListMap
                                (functions, fn f =>
                                 Vector.map (Function.blocks f, Block.label)))}
   end

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

(* Compute the list of functions that each function returns to *)
fun returnsTo (Program.T {functions, main, ...}) =
   let
      val functions = main :: functions
      val {get: Func.t -> {returnsTo: Label.t list ref,
                           tailCalls: Func.t list ref},
           rem, ...} =
         Property.get (Func.plist,
                       Property.initFun (fn _ =>
                                         {returnsTo = ref [],
                                          tailCalls = ref []}))
      fun returnTo (f: Func.t, j: Label.t): unit =
         let
            val {returnsTo, tailCalls} = get f
         in
            if List.exists (!returnsTo, fn j' => Label.equals (j, j'))
               then ()
            else (List.push (returnsTo, j)
                  ; List.foreach (!tailCalls, fn f => returnTo (f, j)))
         end
      fun tailCall (from: Func.t, to: Func.t): unit =
         let
            val {returnsTo, tailCalls} = get from
         in
            if List.exists (!tailCalls, fn f => Func.equals (to, f))
               then ()
            else (List.push (tailCalls, to)
                  ; List.foreach (!returnsTo, fn j => returnTo (to, j)))
         end
      val _ =
         List.foreach
         (functions, fn f =>
          let
             val {name, blocks, ...} = Function.dest f
          in
             Vector.foreach
             (blocks, fn Block.T {transfer, ...} =>
              case transfer of
                 Call {func, return, ...} => (case return of
                                                 Return.NonTail {cont, ...} =>
                                                    returnTo (func, cont)
                                               | _ => tailCall (name, func))

               | _ => ())
          end)
   in
      {rem = rem,
       returnsTo = ! o #returnsTo o get}
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
                       Arith {overflow, success, ...} =>
                          (same overflow; same success)
                     | CCall {return, ...} => Option.app (return, same)
                     | Goto {dst, ...} => same dst
                     | Switch s => Switch.foreachLabel (s, same)
                     | _ => ()
                 end)
          in
             ()
          end)
      val {returnsTo, rem = remReturnsTo} = returnsTo program
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
             val _ = remReturnsTo name
             val _ = Vector.foreach (blocks, remLabelClass o Block.label)
          in
             ()
          end)
   in 
      Vector.fromListMap (!chunks, fn {funcs, labels} =>
                          {funcs = Vector.fromList (!funcs),
                           labels = Vector.fromList (!labels)})
   end

fun chunkify p =
   case !Control.chunk of
      Control.ChunkPerFunc => chunkPerFunc p
    | Control.OneChunk => oneChunk p
    | Control.Coalesce {limit} => coalesce (p, limit)

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
                           ("jumps", Vector.layout Label.layout labels)])))
          in
             ()
          end)
   in
      chunks
   end

end
