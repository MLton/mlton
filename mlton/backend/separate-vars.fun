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

fun transformFunc f =
   let
      val {args, blocks, name, ...} = Function.dest f
      val forest = Function.loopForest (f,
         fn (_, Block.T {kind, ...}) => not (Kind.frameStyle kind = Kind.OffsetsAndSize))
      val {get = loopInfo: Label.t -> {headers: R.Block.t vector} option,
           set = setLoopInfo,
           rem = removeLoopInfo} =
         Property.getSetOnce (Label.plist, Property.initRaise ("loopInfo", Label.layout))
      val {loops, notInLoop} = DirectedGraph.LoopForest.dest
         (Function.loopForest (f, fn (R.Block.T {kind, ...}, _) =>
            not (R.Kind.frameStyle kind = R.Kind.OffsetsAndSize)))
      val _ = Vector.foreach (notInLoop, fn b => setLoopInfo (R.Block.label b, NONE))
      val _ = Vector.foreach (loops,
         fn t as {headers, ...} =>
            let
               fun setHeaders b =
                  setLoopInfo (R.Block.label b, SOME {headers=headers})
               fun goLoop {headers, child} =
                  case DirectedGraph.LoopForest.dest child of
                     {loops, notInLoop} =>
                        let
                           val _ = Vector.foreach (headers, setHeaders)
                           val _ = Vector.foreach (notInLoop, setHeaders)
                        in
                           Vector.foreach (loops, goLoop)
                        end
            in
               goLoop t
            end)
<<<<<<< HEAD
=======
      val isLoop = isSome o loopInfo o Block.label
      val usedVar = fn {var, block} => if isLoop block then Liveness.Warm else Liveness.Cold

      (* calculate variable liveness within a loop, if it's active in a loop,
       * then the variable will be Warm, else it will be Cold (even if it's live
       * during a loop, so long as it's not used in it) *)
      val {labelLive, remLabelLive} = Live.live (f,
         { shouldConsider=fn _ => true,
           flowBack=fn {earlier, flowed, ...} =>
               if flowed = Liveness.Warm andalso isLoop earlier
                  then Liveness.Warm
                  else Liveness.Cold,
           usedVar=usedVar})

      (* by definition, the loop is strongly connected, so if a variable is
       * live in one header but not defined there, it's live in all of them *)
      val _ = let
            fun doFilter vars = Vector.keepAllMap (vars,
               fn p => case p of
                        (x, Liveness.Warm) => SOME x
                      | (_, Liveness.Cold) => NONE)
         in
            Vector.map (loops, fn {headers, ...} =>
               let
                  val headerVars = (Vector.concatV o Vector.map)
                     (headers, fn header =>
                        let
                           val {beginNoFormals, ...} = labelLive (Block.label header)
                        in
                           doFilter beginNoFormals
                        end)
               in
                  ()
               end)
              end
>>>>>>> 55449d4d9... Cleanup unused variables and disable separate-vars
   in
      f
   end

fun transform p =
   let
      val Program.T {functions, handlesSignals, main, objectTypes} = p
<<<<<<< HEAD
      val newFunctions = Vector.map(functions, transformFunc)
=======
      (*val newFunctions = List.map(functions, transformFunc)*)
      val newFunctions = functions
>>>>>>> 55449d4d9... Cleanup unused variables and disable separate-vars
   in
      Program.T {functions=newFunctions, handlesSignals=handlesSignals,
                 main=main, objectTypes=objectTypes}
   end

end
