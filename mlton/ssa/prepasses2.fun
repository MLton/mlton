(* Copyright (C) 2005-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor PrePasses2 (S: PREPASSES2_STRUCTS): PREPASSES2 = 
struct

open S

structure DeadBlocks =
struct

fun eliminateFunction f =
   let
      val {args, blocks, mayInline, name, raises, returns, start} =
         Function.dest f
      val {get = isLive, set = setLive, rem} =
         Property.getSetOnce (Label.plist, Property.initConst false)
      val _ = Function.dfs (f, fn Block.T {label, ...} =>
                            (setLive (label, true)
                             ; fn () => ()))
      val f =
         if Vector.forall (blocks, isLive o Block.label)
            then f
         else
            let 
               val blocks =
                  Vector.keepAll
                  (blocks, isLive o Block.label)
            in
               Function.new {args = args,
                             blocks = blocks,
                             mayInline = mayInline,
                             name = name,
                             raises = raises,
                             returns = returns,
                             start = start}
            end
       val _ = Vector.foreach (blocks, rem o Block.label)
   in
     f
   end

fun eliminate (Program.T {datatypes, globals, functions, main}) =
   Program.T {datatypes = datatypes,
              globals = globals,
              functions = List.revMap (functions, eliminateFunction),
              main = main}
end

val eliminateDeadBlocksFunction = DeadBlocks.eliminateFunction
(* quell unused warning *)
val _ = eliminateDeadBlocksFunction
val eliminateDeadBlocks = DeadBlocks.eliminate
(* quell unused warning *)
val _ = eliminateDeadBlocks

structure Reverse =
struct

fun reverseFunctions (Program.T {globals, datatypes, functions, main}) =
   Program.T {datatypes = datatypes,
              globals = globals,
              functions = List.rev functions,
              main = main}
end

val reverseFunctions = Reverse.reverseFunctions


structure DropProfile =
struct

fun dropFunction f =
   let
      val {args, blocks, mayInline, name, raises, returns, start} =
         Function.dest f
      val blocks =
         Vector.map
         (blocks, fn Block.T {args, label, statements, transfer} =>
          Block.T {args = args,
                   label = label,
                   statements = Vector.keepAll
                                (statements, 
                                 fn Statement.Profile _ => false
                                  | _ => true),
                   transfer = transfer})
   in
      Function.new {args = args,
                    blocks = blocks,
                    mayInline = mayInline,
                    name = name,
                    raises = raises,
                    returns = returns,
                    start = start}
   end

fun drop (Program.T {datatypes, globals, functions, main}) =
   (Control.profile := Control.ProfileNone
    ; Program.T {datatypes = datatypes,
                 globals = globals,
                 functions = List.revMap (functions, dropFunction),
                 main = main})
end

val dropProfile = DropProfile.drop

end
