functor ImplementHandlers (S: IMPLEMENT_HANDLERS_STRUCTS): IMPLEMENT_HANDLERS = 
struct

open S
open Ssa
datatype z = datatype Exp.t
datatype z = datatype Transfer.t

structure LabelInfo =
   struct
      type t = {block: Block.t,
		handlerStack: Label.t list option ref,
		replacement: Statement.t vector option ref,
		visited: bool ref}

      fun layout ({handlerStack, visited, ...}: t) =
	 Layout.record
	 [("handlerStack",
	   Option.layout (List.layout Label.layout) (!handlerStack)),
	  ("visited", Bool.layout (!visited))]
   end

fun doit (Program.T {datatypes, globals, functions, main}) =
   let
      fun implementFunction (f: Function.t): Function.t =
	 let
	    val {args, blocks, name, raises, returns, start} = Function.dest f
	    val {get = labelInfo: Label.t -> LabelInfo.t,
		 set = setLabelInfo, ...} =
	       Property.getSetOnce
	       (Label.plist, Property.initRaise ("info", Label.layout))
	    val _ =
	       Vector.foreach
	       (blocks, fn b as Block.T {label, ...} =>
		setLabelInfo (label,
			      {block = b,
			       handlerStack = ref NONE,
			       replacement = ref NONE,
			       visited = ref false}))
	    (* Do a dfs from the start, figuring out the handler stack at
	     * each label.
	     *)
	    fun visit (l: Label.t, hs: Label.t list): unit =
	       let
		  val {block, handlerStack, replacement, visited} = labelInfo l
		  val Block.T {statements, transfer, ...} = block
	       in
		  if !visited
		     then ()
		  else
		     let
			val _ = visited := true
			fun bug msg =
			   (Vector.layout
			    (fn Block.T {label, ...} =>
			     let open Layout
			     in seq [Label.layout label,
				     str " ",
				     LabelInfo.layout (labelInfo label)]
			     end)
			    ; Error.bug (concat
					 [msg, ": ", Label.toString l]))
			val _ =
			   case !handlerStack of
			      NONE => handlerStack := SOME hs
			    | SOME hs' =>
				 if List.equals (hs, hs', Label.equals)
				    then ()
				 else bug "handler stack mismatch"
			val hs =
			   if not (Vector.exists
				   (statements, fn Statement.T {var, exp, ...} =>
				    case exp of
				       HandlerPop _ => true
				     | HandlerPush _ => true
				     | _ => false))
			      (* An optimization to avoid recopying blocks
			       * with no handlers.
			       *)
			      then (replacement := SOME statements
				    ; hs)
			   else
			      let
				 val (hs, ac) =
				    Vector.fold
				    (statements, (hs, []), fn (s, (hs, ac)) =>
				     let
					val Statement.T {var, ty, exp, ...} = s
				     in
					case Statement.exp s of
					   HandlerPop _ =>
					      (case hs of
						  [] => bug "pop of empty handler stack"
						| _ :: hs =>
						     let
							val s =
							   case hs of
							      [] =>
								 Statement.setExnStackSlot
							    | h :: _ =>
								 Statement.setHandler h
						     in (hs, s :: ac)
						     end)
					 | HandlerPush h =>
					      let
						 val ac =
						    Statement.setHandler h :: ac
						 val ac =
						    case hs of
						       [] =>
							  Statement.setExnStackLocal
							  :: Statement.setSlotExnStack
							  :: ac
						     | _ => ac
					      in
						 (h :: hs, ac)
					      end
					 | _ => (hs, s :: ac)
				     end)
				 val _ =
				    replacement := SOME (Vector.fromListRev ac)
			      in
				 hs
			      end
			in
			   Transfer.foreachLabel (transfer, fn l =>
						  visit (l, hs))
			end
	       end
	    val _ = visit (start, [])
	    val blocks =
	       Vector.map
	       (blocks, fn b as Block.T {label, args, transfer, ...} =>
		let
		   val {replacement, visited, ...} = labelInfo label
		in
		   if !visited
		      then Block.T {label = label,
				    args = args,
				    transfer = transfer,
				    statements = valOf (! replacement)}
		   else b
		end)
	 in
	    Function.new {args = args,
			  blocks = blocks,
			  name = name,
			  raises = raises,
			  returns = returns,
			  start = start}
	 end
   in
      Program.T {datatypes = datatypes,
		 globals = globals,
		 functions = List.revMap (functions, implementFunction),
		 main = main}
   end

end
