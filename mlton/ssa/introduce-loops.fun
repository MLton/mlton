(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
(* Change any toplevel function that only calls itself in tail position
 * into one with a local loop and no self calls.
 *)
functor IntroduceLoops (S: INTRODUCE_LOOPS_STRUCTS): INTRODUCE_LOOPS = 
struct

open S
open Exp Transfer

fun introduceLoops (Program.T {datatypes, globals, functions, main}) =
   let
      val functions =
	 List.map
	 (functions, fn f =>
	  let
	     val {name, args, start, blocks, returns, raises} = Function.dest f
	     val tailCallsItself = ref false
	     val noChange = (args, start, blocks)
	     val (args, start, blocks) =
	        (Vector.foreach
		 (blocks, fn Block.T {transfer, ...} =>
		  case transfer of
		     Call {func, return, ...} =>
		        if Func.equals (name, func)
			   andalso not (Return.isNonTail return)
			   then tailCallsItself := true
			else ()
		   | _ => ()) ;
		 if !tailCallsItself
		    then
		       let
			  val _ = Control.diagnostics
			          (fn display =>
				   let open Layout
				   in
				      display (Func.layout name)
				   end)

			  val newArgs =
			     Vector.map (args, fn (x, t) => (Var.new x, t))
			  val loopName = Label.newString "loop"
			  val loopSName = Label.newString "loopS"
			  val blocks = 
			     Vector.toListMap
			     (blocks, fn Block.T {label, args, statements, transfer} =>
			      let
				 val transfer =
				    case transfer of
				       Call {func, args, return} =>
					  if Func.equals (name, func)
					     andalso not (Return.isNonTail return)
					     then Goto {dst = loopName, 
						        args = args}
					  else transfer
				     | _ => transfer
			      in
				Block.T {label = label,
					 args = args,
					 statements = statements,
					 transfer = transfer}
			      end)
			  val blocks = 
			     Vector.fromList
			     (Block.T 
			      {label = loopSName,
			       args = Vector.new0 (),
			       statements = Vector.new0 (),
			       transfer = Goto {dst = loopName,
						args = Vector.map (newArgs, #1)}} ::
			      Block.T 
			      {label = loopName,
			       args = args,
			       statements = Vector.new0 (),
			       transfer = Goto {dst = start,
						args = Vector.new0 ()}} ::
			      blocks)
		       in
			 (newArgs,
			  loopSName,
			  blocks)
		       end
		 else noChange)
	  in Function.new {name = name,
			   args = args,
			   start = start,
			   blocks = blocks,
			   returns = returns,
			   raises = raises}
	  end)
   in
      Program.T {datatypes = datatypes,
		 globals = globals,
		 functions = functions,
		 main = main}
   end

end
