(* Copyright (C) 2019 Jason Carr
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *
 * This pass copies some variables into new locations
 * so that the relatively naïve AllocateVars can give one location
 * to each variable.
 *
 * Although this is pretty tightly coupled to the AllocateVars, it saves
 * us a lot of complexity in both, since the latter does not need to do any
 * contortions to try to return multiple variables
 *
 * Mechanically, what we do is find the loops in the program, and try our best
 * to split up variables so that no variable that would have to be on the
 * stack appears in any loop. So while we might end up with some extra copies,
 * by construction we don't really do so in any loop where we wouldn't have to
 * already do something similar in practice.
 *)

functor SeparateVars(S: RSSA_TRANSFORM_STRUCTS): RSSA_TRANSFORM = 
struct

open S
open Rssa
structure Live = Live (Rssa)
structure Restore = RestoreR (Rssa)

fun processLoop ({labelLive, remLabelLive}, setLabelBlock) tree =
   let
      (* in each processLoop call we need to loop twice
       * first we find register-blocking points, and record live vars
       * then for each var live over a blocking point, check if active
       * in loop. If it's active we rewrite the appropriate blocks with a new assignment *)

      val dummyBlock = Vector.first (#headers tree)
      val blocking = Buffer.new {dummy=dummyBlock}

      fun goLoop {headers, child} =
         case DirectedGraph.LoopForest.dest child of
              {loops, notInLoop} =>
              let
              in
                 Vector.foreach (loops, goLoop)
              end
      val _ = goLoop tree
   in
      ()
   end

fun transformFunc func =
   let
      val {args, blocks, name, raises, returns, start} = Function.dest func
      val liveness = Live.live (func, {shouldConsider = fn _ => true})
      val {loops, ...} = DirectedGraph.LoopForest.dest
         (Function.loopForest (func, fn (Block.T {kind, ...}, _) => true))

      val remapTable = HashTable.new {hash=Label.hash, equals=Label.equals}
      fun remap (label, newBlock) = (ignore o HashTable.insertIfNew)
         (remapTable, label, fn () => newBlock, fn _ => ())

      val _ = Vector.foreach (loops, processLoop (liveness, remap))

      val newBlocks = Vector.map (blocks, fn block as Block.T {label, ...} =>
         case HashTable.peek (remapTable, label) of
              SOME newBlock => newBlock
            | NONE => block)
   in
      Function.new
         {args=args, blocks=newBlocks,
          name=name, raises=raises,
          returns=returns, start=start}
   end


fun transform p =
   let
      val Program.T {functions, handlesSignals, main, objectTypes} = p
      val restore = Restore.restoreFunction ()
      val newFunctions = List.map(functions, restore o transformFunc)
   in
      Program.T {functions=newFunctions, handlesSignals=handlesSignals,
                 main=main, objectTypes=objectTypes}
   end

end
