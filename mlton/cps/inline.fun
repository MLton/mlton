(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Inline (S: INLINE_STRUCTS): INLINE = 
struct

open S
open Dec PrimExp Transfer
type int = Int.t

fun expSize e =
   let 
      val r = ref 0
      fun incSize n = r:= !r + n
      val primExpSize =
	 fn Const _ => 0
	  | Var _ => 0
	  | Select _ => 1
	  | Tuple xs => Vector.length xs
	  | ConApp {args, ...} => 1 + Vector.length args
	  | PrimApp {args, ...} => 1 + Vector.length args
      val transferSize =
	 fn Bug => 1
	  | Call {args, ...} => 1 + Vector.length args
	  | Case {cases, ...} => 1 + Cases.length cases
	  | Jump {args, ...} => 1 + Vector.length args
	  | Raise xs => 1 + Vector.length xs
	  | Return xs => 1 + Vector.length xs
      val decSize =
	 fn Fun _ => 0
	  | Bind {exp, ...} => primExpSize exp
	  | _ => 1
   in Exp.foreach' (e, {handleDec = incSize o decSize,
			handleTransfer = incSize o transferSize})
      ; !r
   end


(* Returns true if the size of the expression (measured somewhat arbitrarily)
 * is greater than SOME integer.  Returns true on NONE.
 *)
fun sizeGreaterThan (exp: Exp.t, size: int option): bool =
   case size of
      NONE => false
    | SOME size =>
	 DynamicWind.withEscape
	 (fn escape =>
	  let 
	     val r = ref 0
	     fun incSize n =
		let val new = !r + n
		in if new > size
		      then escape true
		   else r := new
		end
	     val primExpSize =
		fn Const _ => 0
		 | Var _ => 0
		 | Select _ => 1
		 | Tuple xs => Vector.length xs
		 | ConApp {args, ...} => 1 + Vector.length args
		 | PrimApp {args, ...} => 1 + Vector.length args
	     val transferSize =
		fn Bug => 1
		 | Call {args, ...} => 1 + Vector.length args
		 | Case {cases, ...} => 1 + Cases.length cases
		 | Jump {args, ...} => 1 + Vector.length args
		 | Raise xs => 1 + Vector.length xs
		 | Return xs => 1 + Vector.length xs
	     val decSize =
		fn Fun _ => 0
		 | Bind {exp, ...} => primExpSize exp
		 | _ => 1
	  in Exp.foreach' (exp, {handleDec = incSize o decSize,
				 handleTransfer = incSize o transferSize})
	     ; false
	  end)

local
   fun 'a make (dontInlineBody: Exp.t * 'a -> bool)
      (Program.T {functions, ...}, a: 'a): Func.t -> bool =
      let
	 val {get = shouldInline, set = setShouldInline, ...} =
	    Property.getSetOnce (Func.plist, Property.initConst false)
      in
	 Vector.foreach (functions, fn Function.T {name, body, ...} =>
			if dontInlineBody (body, a)
			   then ()
			else setShouldInline (name, true))
	 ; shouldInline
      end
   fun containsCall (exp: Exp.t): bool =
      DynamicWind.withEscape
      (fn escape =>
       (Exp.foreachTransfer (exp, fn Call _ => escape true | _ => ())
	; false))

   fun containsLoop (exp: Exp.t): bool =
      let
	 val {get, set, destroy} =
	    Property.destGetSet (Jump.plist, Property.initConst false)
	 val res =
	    DynamicWind.withEscape
	    (fn escape =>
	     let
		val loopTransfer =
		   fn Jump {dst, ...} => if get dst then escape true else ()
		    | _ => ()
		fun loopExp (e: Exp.t): unit =
		   let val {decs, transfer} = Exp.dest e
		   in List.foreach (decs, loopDec)
		      ; loopTransfer transfer
		   end
		and loopDec d =
		   case d of
		      Fun {name, body, ...} =>
			 (set (name, true); loopExp body; set (name, false))
		    | _ => ()
	     in loopExp exp; false
	     end)
	 val _ = destroy ()
      in res
      end
in
   val leaf = make (fn (body, {size}) =>
		    sizeGreaterThan (body, size)
		    orelse containsCall body)

   val leafNoLoop = make (fn (body, {size}) =>
			  sizeGreaterThan (body, size)
			  orelse containsCall body
			  orelse containsLoop body)
end

structure Graph = DirectedGraph
structure Node = Graph.Node

fun nonRecursive (program as Program.T {functions, ...}, {size: int option}) =
   let
      val {get = funcInfo: Func.t -> {shouldInline: bool ref,
				      isBig: bool,
				      node: Node.t,
				      numCalls: int ref},
	   set = setFuncInfo, ...} =
	 Property.getSetOnce
	 (Func.plist, Property.initRaise ("func info", Func.layout))
      val {set = setNodeFunc, get = nodeFunc, ...} =
	 Property.getSetOnce (Node.plist,
			      Property.initRaise ("func", Node.layout))
      val graph = Graph.new ()
      (* initialize the info for each func *)
      val _ = 
	 Vector.foreach
	 (functions, fn Function.T {name, body, ...} =>
	  let val n = Graph.newNode graph
	  in setNodeFunc (n, name)
	     ; setFuncInfo (name, {node = n,
				   isBig = sizeGreaterThan (body, size),
				   numCalls = ref 0,
				   shouldInline = ref true})
	  end)
      (* Update call counts. *)
      val _ =
	 Vector.foreach
	 (functions, fn Function.T {name, body, ...} =>
	  Exp.foreachTransfer
	  (body,
	   fn Call {func, ...} =>
	   let val {numCalls, shouldInline, isBig, ...} = funcInfo func
	      val n = !numCalls
	   in numCalls := n + 1;
	      if ((n = 1 andalso isBig)
		  orelse Func.equals (name, func))
		 then shouldInline := false
	      else ()
	   end
	    | _ => ()))
      (* Build the call graph of potentially inlinable functions.
       * Don't add edges for functions that are not inlined, since they
       * can't cause infinite unrolling.
       *)
      val _ = 
	 Vector.foreach
	 (functions, fn Function.T {name, body, ...} =>
	  let val {node = caller, shouldInline = si, ...} = funcInfo name
	  in Exp.foreachTransfer
	     (body,
	      fn Call {func, ...} =>
	      let
		 val {numCalls, shouldInline = si', node = callee, ...} =
		    funcInfo func
	      in if !si andalso !si'
		    then (Graph.addEdge (graph, {from = caller, to = callee})
			  ; ())
		 else ()
	      end
	       | _ => ())
	  end)
      (* Compute strongly-connected components and set any function that is
       * in a nontrivial scc to be not inlined.
       *)
      val _ = 
	 List.foreach
	 (Graph.stronglyConnectedComponents graph,
	  fn [] => ()
	   | [_] => ()
	   | ns => List.foreach (ns, fn n =>
				 #shouldInline (funcInfo (nodeFunc n)) := false))
      val _ =
	 Control.diagnostics
	 (fn disp =>
	  let open Layout
	  in Vector.foreach
	     (functions, fn Function.T {name, body, ...} =>
	      let val {shouldInline, numCalls, ...} = funcInfo name
	      in disp (seq [Func.layout name, str " ",
			    Int.layout (expSize body), str " ",
			    Int.layout (!numCalls), str " ",
			    Int.layout (expSize body * !numCalls), str " ",
			    Bool.layout (!shouldInline)])
	      end)
	     ; Program.layouts (program, disp)
	  end)
   in
      ! o #shouldInline o funcInfo
   end

fun product (program as Program.T {functions, ...},
	     {small: int, product: int}) =
   let
      type info = {body: Exp.t,
		   doesCallSelf: bool ref,
		   node: Node.t,
		   numCalls: int ref,
		   shouldInline: bool ref,
		   size: int ref}
      val {get = funcInfo: Func.t -> info, set = setFuncInfo, ...} =
	 Property.getSetOnce (Func.plist,
			      Property.initRaise ("func info", Func.layout))
      val {set = setNodeFunc, get = nodeFunc, ...} =
	 Property.getSetOnce (Node.plist,
			      Property.initRaise ("func", Node.layout))
      (* expSize returns the size of an expression, taking into account the sizes
       * of inlined callees.
       *)
      fun expSize e =
	 let 
	    val r = ref 0
	    val primExpSize =
	       fn Const _ => 0
		| Var _ => 0
		| Select _ => 1
		| Tuple xs => 3 + Vector.length xs
		| ConApp {args, ...} => 4 + Vector.length args
		| PrimApp {prim, args, ...} =>
		     if (case Prim.name prim of
			    Prim.Name.FFI _ => true
			  | _ => false)
			then 5 + Vector.length args
		     else 3
	    val transferSize =
	       fn Bug => 1
		| Call {func, args, ...} =>
		     let val {shouldInline, size, ...} = funcInfo func
		     in if !shouldInline
			   then !size
			else 4 + Vector.length args
		     end
		| Case {cases, ...} => 1 + 2 * Cases.length cases
		| Jump {args, ...} => 1 + Vector.length args
		| Raise xs => 1 + Vector.length xs
		| Return xs => 1 + Vector.length xs
	    val decSize =
	       fn Bind {exp, ...} => primExpSize exp
		| Fun _ => 0
		| HandlerPush _ => 2
		| HandlerPop => 2
	    fun incSize n = r:= !r + n
	 in Exp.foreach' (e, {handleDec = incSize o decSize,
			      handleTransfer = incSize o transferSize})
	    ; !r
	 end
      (* initialize the info for each func *)
      val graph = Graph.new ()
      val _ = 
	 Vector.foreach
	 (functions, fn Function.T {name, body, ...} =>
	  let val n = Graph.newNode graph
	  in setNodeFunc (n, name)
	     ; setFuncInfo (name, {body = body,
				   doesCallSelf = ref false,
				   node = n,
				   numCalls = ref 0,
				   shouldInline = ref false,
				   size = ref 0})
	  end)
      (* Set numCalls and doesCallSelf. *)
      val _ = 
	 Vector.foreach
	 (functions, fn Function.T {name, body, ...} =>
	  let val {body, node = caller, doesCallSelf, ...} = funcInfo name
	  in Exp.foreachTransfer
	     (body,
	      fn Call {func, ...} =>
	      let val {node = callee, numCalls, ...} = funcInfo func
	      in if Node.equals (caller, callee)
		    then doesCallSelf := true
		 else Int.inc numCalls
	      end
	       | _ => ())
	  end)
      fun mayInline ({body, doesCallSelf, numCalls, size, ...}: info): bool =
	 not (!doesCallSelf)
	 andalso let val n = expSize body
		     val _ = size := n
		 in (!numCalls - 1) * (n - small) <= product
		 end
      (* Build the call graph.  Do not include functions that we already know
       * will not be inlined.
       *)
      val _ =
	 Vector.foreach
	 (functions, fn Function.T {name, body, ...} =>
	  let val info as {node = caller, doesCallSelf, ...} = funcInfo name
	  in if mayInline info
		then
		   (Exp.foreachTransfer
		    (body,
		     fn Call {func, ...} =>
		     let val {node = callee, numCalls, ...} = funcInfo func
		     in if Node.equals (caller, callee)
			   then ()
			else (Graph.addEdge (graph, {from = caller, to = callee})
			      ; ())
		     end
		      | _ => ()))
	     else ()
	  end)
      (* Compute strongly-connected components.
       * Then start at the leaves of the call graph and work up.
       *)
      val _ = 
	 List.foreach
	 (rev (Graph.stronglyConnectedComponents graph),
	  fn [n] => let val info as {shouldInline, ...} = funcInfo (nodeFunc n)
		    in shouldInline := mayInline info
		    end
	   | _ => ())

      val _ =
	 Control.diagnostics
	 (fn display =>
	  let open Layout
	  in Vector.foreach
	     (functions, fn Function.T {name, body, ...} =>
	      let val {numCalls, shouldInline, size, ...} = funcInfo name
	      in display (seq [Func.layout name, str " ",
			       Int.layout (!numCalls), str " ",
			       Int.layout (!size), str " ",
			       Bool.layout (!shouldInline)])
	      end)
	     ; Program.layouts (program, display)
	  end)
   in ! o #shouldInline o funcInfo
   end

fun inline (p as Program.T {datatypes, globals, functions, main}) =
   let
      val shouldInline: Func.t -> bool =
	 let open Control
	 in case !inline of
	    NonRecursive r => product (p, r)
	  | Leaf r => leaf (p, r)
	  | LeafNoLoop r => leafNoLoop (p, r)
	 end
      val {get = funInfo: Func.t -> {args: Var.t vector,
				     body: Exp.t},
	   set = setFunInfo, ...} =
	 Property.getSetOnce (Func.plist,
			      Property.initRaise ("Inline.info", Func.layout))
      val _ = Vector.foreach (functions, fn Function.T {name, args, body, ...} =>
			     setFunInfo (name, {args = Vector.map (args, #1),
						body = body}))
      fun inlineExp (exp: Exp.t, outerCont: Jump.t option): Exp.t =
	 let
	    fun loopExp e =
	       let val {decs, transfer} = Exp.dest e
		  val decs =
		     List.map
		     (decs,
		      fn Fun {name, args, body} =>
		      Fun {name = name, args = args, body = loopExp body}
		       | d => d)
		  fun simple t = Exp.make {decs = decs, transfer = t}
	       in case transfer of
		  Return xs =>
		     simple (case outerCont of
				NONE => Return xs
			      | SOME j => Jump {dst = j, args = xs})
		| Call {func, args, cont} =>
		     let
			val cont =
			   case cont of
			      NONE => outerCont
			    | SOME c => SOME c
			val {args = formals, body} = funInfo func
		     in if shouldInline func
			   then
			      let
				 val body =
				    Exp.alphaRename
				    {exp = body,
				     handleJump = fn _ => (),
				     substitution =
				     Vector.fold2
				     (formals, args, [], fn (f, a, ac) =>
				      {formal = f, actual = a} :: ac)}
			      in Exp.prefixs (inlineExp (body, cont), decs)
			      end
			else simple (Call {func = func, args = args,
					   cont = cont})
		     end
		| _ => simple transfer
	       end

	 in loopExp exp
	 end
      val shrinkExp = shrinkExp globals
      val functions =
	 Vector.keepAllMap
	 (functions, fn Function.T {name, args, body, returns} =>
	  if not (Func.equals (name, main))
	     andalso shouldInline name
	     then NONE
	  else SOME (Function.T {name = name, args = args, returns = returns,
				 body = shrinkExp (inlineExp (body, NONE))}))
      val program =
	 Program.T {datatypes = datatypes,
		    globals = globals,
		    functions = functions,
		    main = main}
      val _ = Program.clear program
   in
      program
   end

end
