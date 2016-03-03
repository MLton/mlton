(* Copyright (C) 1999-2005, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Moves a conditional statement outside a loop by duplicating the loops body
 * under each branch of the conditional.
 *)
functor LoopUnswitch(S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM = 
struct

open S
open Exp Transfer

structure Graph = DirectedGraph
local
   open Graph
in
   structure Node = Node
   structure Forest = LoopForest
end

fun logli (l: Layout.t, i: int): unit =
   Control.diagnostics
   (fn display =>
      display(Layout.indent(l, i)))

fun logl(l: Layout.t): unit =
   logli(l, 0)

fun logsi (s: string, i: int): unit =
   logli((Layout.str s), i)

fun logs (s: string): unit =
   logsi(s, 0)

fun tf ({loops, notInLoop}, labelNode, nodeBlock, depth) =
  let
    val () = logsi ("Not in loop:", depth)
    val _ = Vector.foreach (notInLoop, fn n =>
      let
        val block = nodeBlock n
        val blockName = Label.layout (Block.label block)
      in
        logli (blockName, depth)
      end)
  in
    Vector.foreach(loops, fn l => tl(l, labelNode, nodeBlock, depth))
  end
   
and tl ({headers, child}, labelNode, nodeBlock, depth) =
   let
      val () = logsi ("Loop with headers:", depth)
      val _ = Vector.foreach (headers, fn h =>
         let
            val block = nodeBlock h
            val blockName = Label.layout (Block.label block)
         in
           logli (blockName, depth)
         end)
   in
      tf ((Forest.dest child), labelNode, nodeBlock, depth + 1)
   end

fun traverseForest ({loops, notInLoop}, labelNode, nodeBlock) =
  let
    val () = logs (concat[(Int.toString (Vector.length loops)), " total loops"])
  in
    Vector.foreach(loops, fn l => tl(l, labelNode, nodeBlock, 1))
  end

fun optimizeFunction(function: Function.t): Function.t =
   let
      val {graph, labelNode, nodeBlock} = Function.controlFlow function
      val {name, start, ...} = Function.dest function
      val () = logl (Func.layout name)
      val root = labelNode start
      val forest = Graph.loopForestSteensgaard(graph, {root = root})
      val _ = traverseForest((Forest.dest forest), labelNode, nodeBlock)
   in
      function
   end

fun transform (Program.T {datatypes, globals, functions, main}) =
   let
      val () = logs "Unswitching loops"
      val optimizedFunctions = List.map (functions, optimizeFunction)
   in
      Program.T {datatypes = datatypes,
                 globals = globals,
                 functions = optimizedFunctions,
                 main = main}
   end

end
