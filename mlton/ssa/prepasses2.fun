(* Copyright (C) 2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
   let
      val functions = List.revMap (functions, eliminateFunction)
   in
      Program.T {datatypes = datatypes,
		 globals = globals,
		 functions = functions,
		 main = main}
   end
end

val eliminateDeadBlocksFunction = DeadBlocks.eliminateFunction
val eliminateDeadBlocks = DeadBlocks.eliminate

structure Reverse =
struct

fun reverseFunctions (program as Program.T {globals, datatypes, functions, main}) =
   let
      val program =
	 Program.T {datatypes = datatypes,
		    globals = globals,
		    functions = List.rev functions,
		    main = main}
   in
      program
   end

end

val reverseFunctions = Reverse.reverseFunctions

end
