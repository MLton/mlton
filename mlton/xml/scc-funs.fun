(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor SccFuns (S: SCC_FUNS_STRUCTS): SCC_FUNS = 
struct

open S
open Dec PrimExp

structure Graph = DirectedGraph
structure Node = Graph.Node

fun sccFuns (Program.T {datatypes, body, overflow}) =
   let
      (* For each function appearing in a fun dec record its node, which will
       * have edges to the nodes of other functions declared in the same dec
       * if they appear in it's body.
       *)
      val {get = funInfo: Var.t -> {
				    node: Node.t,
				    visit: (unit -> unit) ref
				    } option,
	   set = setFunInfo} =
	 Property.getSetOnce (Var.plist, Property.initConst NONE)
      val {get = nodeLambda, set = setNodeLambda} =
	 Property.getSetOnce (Node.plist,
			      Property.initRaise ("lambda", Node.layout))
      fun loopVar x =
	 case funInfo x of
	    NONE => ()
	  | SOME {visit, ...} => !visit ()
      val loopVarExp = loopVar o VarExp.var
      fun loopVarExps xs = Vector.foreach (xs, loopVarExp)
      fun loopLambda (l: Lambda.t): Lambda.t =
	 let val {arg, argType, body} = Lambda.dest l
	 in Lambda.new {arg = arg, argType = argType, body = loopExp body}
	 end
      and loopPrimExp (e: PrimExp.t): PrimExp.t =
	 case e of
	    Const _ => e
	  | Var x => (loopVarExp x; e)
	  | Tuple xs => (loopVarExps xs; e)
	  | Select {tuple, ...} => (loopVarExp tuple; e)
	  | Lambda l => Lambda (loopLambda l)
	  | ConApp {arg, ...} => (Option.app (arg, loopVarExp); e)
	  | PrimApp {args, ...} => (loopVarExps args; e)
	  | App {func, arg} => (loopVarExp func; loopVarExp arg; e)
	  | Raise {exn, ...} => (loopVarExp exn; e)
	  | Case {test, cases, default} =>
	       (loopVarExp test;
		Case {test = test,
		      cases = Cases.map (cases, loopExp),
		      default = Option.map (default, loopExp)})
	  | Handle {try, catch, handler} =>
	       Handle {try = loopExp try,
		       catch = catch,
		       handler = loopExp handler}
      and loopExp (e: Exp.t): Exp.t =
	 let val {decs, result} = Exp.dest e
	    val decs =
	       List.rev
	       (List.fold
		(decs, [], fn (dec, decs) =>
		 case dec of
		    MonoVal {var, ty, exp} =>
		       MonoVal {var = var, ty = ty,
				exp = loopPrimExp exp} :: decs
		  | PolyVal {var, tyvars, ty, exp} =>
		       PolyVal {var = var, tyvars = tyvars, ty = ty,
				exp = loopExp exp} :: decs
		  | Exception _ => dec :: decs
		  | Fun {tyvars, decs = lambdas} =>
		       let val g = Graph.new ()
			  val _ =
			     Vector.foreach
			     (lambdas, fn {var, ty, lambda} =>
			      setFunInfo (var, SOME {node = Graph.newNode g,
						     visit = ref ignore}))
			  val _ = 
			     Vector.foreach
			     (lambdas, fn {var, ty, lambda} =>
			      let val {node = from, ...} = valOf (funInfo var)
			      in Vector.foreach
				 (lambdas, fn {var = x, ...} =>
				  let val {visit, node = to} = valOf (funInfo x)
				  in visit := (fn () =>
					       (Graph.addEdge (g, {from = from,
								   to = to})
						; visit := ignore))
				  end)
				 ; (setNodeLambda
				    (from, {var = var,
					    ty = ty,
					    lambda = loopLambda lambda}))
				 ; (Vector.foreach
				    (lambdas, fn {var, ...} =>
				     let val {visit, ...} = valOf (funInfo var)
				     in visit := ignore
				     end))
			      end)
		       in List.map
			  (Graph.stronglyConnectedComponents g, fn nodes =>
			   Fun {tyvars = tyvars,
				decs = Vector.fromListMap (nodes, nodeLambda)})
			  @ decs
		       end))
	    val _ = loopVarExp result
	 in
	    Exp.new {decs = decs, result = result}
	 end
   in Program.T {datatypes = datatypes,
		 body = loopExp body,
		 overflow = overflow}
   end
end
