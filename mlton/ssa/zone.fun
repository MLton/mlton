(* Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor Zone (S: ZONE_STRUCTS): ZONE = 
struct

open S

type int = Int.t

structure Set = DisjointSet

structure Graph = DirectedGraph
local
   open Graph
in
   structure LoopForest = LoopForest
   structure Node = Node
end

structure Scope = UniqueId ()

fun zoneFunction f =
   let
      val {args, mayInline, name, raises, returns, start, ...} = Function.dest f
      datatype z = datatype Exp.t
      val {get = labelInfo: Label.t -> {isCut: bool ref}, ...} =
	 Property.get (Label.plist,
		       Property.initFun (fn _ => {isCut = ref false}))
      val dominatorTree = Function.dominatorTree f
      (* Decide which labels to cut at. *)
      val cutDepth = !Control.zoneCutDepth
      fun addCuts (Tree.T (b, ts), depth: int) =
	 let
	    val depth =
	       if depth = 0
		  then
		     let
			val Block.T {label, ...} = b
			val {isCut, ...} = labelInfo label
			val () = isCut := true
		     in
			cutDepth
		     end
	       else depth - 1
	 in
	    Vector.foreach (ts, fn t => addCuts (t, depth))
	 end
      val () = addCuts (dominatorTree, cutDepth)
      (* Build a tuple of lives at each cut node. *)
      type info = {componentsRev: Var.t list ref,
		   numComponents: int ref,
		   scope: Scope.t,
		   tuple: Var.t}
      fun newInfo () =
	 {componentsRev = ref [],
	  numComponents = ref 0,
	  scope = Scope.new (),
	  tuple = Var.newNoname ()}
      datatype varInfo =
	 Global
	| Local of {blockCache: Var.t option ref,
		    defScope: Scope.t,
		    ty: Type.t,
		    uses: {exp: Exp.t,
			   scope: Scope.t} list ref}
      val {get = varInfo: Var.t -> varInfo,
	   set = setVarInfo, ...} =
	 Property.getSetOnce (Var.plist,
			      Property.initFun (fn _ => Global))
      val blockSelects: {blockCache: Var.t option ref,
			 statement: Statement.t} list ref = ref []
      fun addBlockSelects (ss: Statement.t vector): Statement.t vector =
	 let
	    val blockSelectsV = Vector.fromList (!blockSelects)
	    val () = Vector.foreach (blockSelectsV, fn {blockCache, ...} =>
				     blockCache := NONE)
	    val () = blockSelects := []
	 in
	    Vector.concat [Vector.map (blockSelectsV, #statement), ss]
	 end
      fun define (x: Var.t, ty: Type.t, info: info): unit =
	 setVarInfo (x, Local {blockCache = ref NONE,
			       defScope = #scope info,
			       ty = ty,
			       uses = ref []})
      fun replaceVar (x: Var.t,
		      {componentsRev, numComponents, scope, tuple}: info)
	 : Var.t =
	 case varInfo x of
	    Global => x
	  | Local {blockCache, defScope, ty, uses, ...} =>
	       case !blockCache of
		  SOME y => y
		| _ => 
		     if Scope.equals (defScope, scope)
			then x
		     else
			let
			   fun new () =
			      let
				 val offset = !numComponents
				 val () = List.push (componentsRev, x)
				 val () = numComponents := 1 + offset
				 val exp = Select {object = tuple,
						   offset = offset}
				 val () = List.push (uses, {exp = exp,
							    scope = scope})
			      in
				 exp
			      end
			   val exp =
			      case !uses of
				 [] => new ()
			       | {exp, scope = scope'} :: _ =>
				    if Scope.equals (scope, scope')
				       then exp
				    else new ()
			   val y = Var.new x
			   val () = blockCache := SOME y
			   val () =
			      List.push
			      (blockSelects,
			       {blockCache = blockCache,
				statement = Statement.T {exp = exp,
							 ty = ty,
							 var = SOME y}})
			in
			   y
			end
      val blocks = ref []
      fun loop (Tree.T (b, ts), info: info) =
	 let
	    val Block.T {args, label, statements, transfer} = b
	    val {isCut = ref isCut, ...} = labelInfo label
	    val info' = 
	       if isCut
		  then newInfo ()
	       else info
	    val define = fn (x, t) => define (x, t, info')
	    val () = Vector.foreach (args, define)
	    val statements =
	       Vector.map
	       (statements, fn Statement.T {exp, ty, var} =>
		let
		   val exp = Exp.replaceVar (exp, fn x => replaceVar (x, info'))
		   val () = Option.app (var, fn x => define (x, ty))
		in
		   Statement.T {exp = exp, ty = ty, var = var}
		end)
	    val transfer =
		Transfer.replaceVar (transfer, fn x => replaceVar (x, info'))
	    val statements = addBlockSelects statements
	    val () = Vector.foreach (ts, fn t => loop (t, info'))
	    val statements =
	       if not isCut
		  then statements
	       else
		  let
		     val {componentsRev, tuple, ...} = info'
		     val components = Vector.fromListRev (!componentsRev)
		  in
		     if 0 = Vector.length components
			then statements
		     else
			let
			   val componentTys =
			      Vector.map
			      (components, fn x =>
			       case varInfo x of
				  Global => Error.bug "global component"
				| Local {ty, uses, ...} =>
				     (ignore (List.pop uses)
				      ; {elt = ty,
					 isMutable = false}))
			   val components =
			      Vector.map (components, fn x =>
					  replaceVar (x, info))
			   val s =
			      Statement.T
			      {exp = Object {args = components, con = NONE},
			       ty = Type.tuple componentTys,
			       var = SOME tuple}
			in
			   addBlockSelects (Vector.concat [Vector.new1 s,
							   statements])
			end
		  end
	    val () = List.push (blocks,
				Block.T {args = args,
					 label = label,
					 statements = statements,
					 transfer = transfer})
	 in
	    ()
	 end
      val () = loop (dominatorTree, newInfo ())
      val blocks = Vector.fromList (!blocks)
   in
      Function.new {args = args,
		    blocks = blocks,
		    mayInline = mayInline,
		    name = name,
		    raises = raises,
		    returns = returns,
		    start = start}
   end

fun maybeZoneFunction (f, ac) =
   let
      val {args, blocks, mayInline, name, raises, returns, start} =
	 Function.dest f
   in
      if Vector.length blocks <= !Control.maxFunctionSize
	 then f :: ac
      else zoneFunction f :: ac
   end
   
fun zone (Program.T {datatypes, globals, functions, main}) =
   Program.T {datatypes = datatypes,
	      globals = globals,
	      functions = List.fold (functions, [], maybeZoneFunction),
	      main = main}

end


