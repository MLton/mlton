(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Chunkify (S: CHUNKIFY_STRUCTS): CHUNKIFY = 
struct

open S
open Dec PrimExp Transfer
type int = Int.t

(* A chunkifier that puts each function in its own chunk. *)
fun chunkPerFunc (Program.T {functions, ...}) =
   Vector.toListMap
   (functions, fn Function.T {name, body, ...} =>
    let val jumps = ref []
    in Exp.foreachDec (body,
		       fn Fun {name, ...}=> List.push (jumps, name)
			| _ => ())
       ; {funcs = [name],
	  jumps = !jumps}
    end)

(* A simple chunkifier that puts all code in the same chunk.
 *)
fun oneChunk (Program.T {functions, main, ...}) =
   let
      val funcs = ref []
      val jumps = ref []
      val _ =
	 Vector.foreach (functions, fn Function.T {name, body, ...} =>
			(List.push (funcs, name)
			 ; (Exp.foreachDec
			    (body,
			     fn Fun {name, ...} => List.push (jumps, name)
			      | _ => ()))))
   in
      [{funcs = !funcs, jumps = !jumps}]
   end

structure Label = Jump
structure Set = DisjointSet

(* The size of an expression, not including nested functions. *)

fun size (e: Exp.t): int =
   let
      val {decs, transfer} = Exp.dest e
      val decsSize =
	 List.fold (decs, 0, fn (d, n) => case d of Fun _ => n | _ => n + 1)
      val transferSize =
	 case transfer of
	    Case {cases, ...} => 1 + Cases.length cases
	  | _ => 1
   in decsSize + transferSize
   end

(* Compute the list of functions that each function returns to *)
fun returnsTo (Program.T {functions, ...}) =
   let
      val {get: Func.t -> {returnsTo: Jump.t list ref,
			   tailCalls: Func.t list ref},
	   destroy, ...} =
	 Property.destGet (Func.plist,
			   Property.initFun (fn _ =>
					     {returnsTo = ref [],
					      tailCalls = ref []}))
      fun returnTo (f: Func.t, j: Jump.t): unit =
	 let val {returnsTo, tailCalls} = get f
	 in if List.exists (!returnsTo, fn j' => Jump.equals (j, j'))
	       then ()
	    else (List.push (returnsTo, j)
		  ; List.foreach (!tailCalls, fn f => returnTo (f, j)))
	 end
      fun tailCall (from: Func.t, to: Func.t): unit =
	 let val {returnsTo, tailCalls} = get from
	 in if List.exists (!tailCalls, fn f => Func.equals (to, f))
	       then ()
	    else (List.push (tailCalls, to)
		  ; List.foreach (!returnsTo, fn j => returnTo (to, j)))
	 end
      val _ =
	 Vector.foreach
	 (functions, fn Function.T {name, body, ...} =>
	  Exp.foreachTransfer
	  (body,
	   fn Call {func, cont, ...} => (case cont of
					    NONE => tailCall (name, func)
					  | SOME j => returnTo (func, j))
	    | _ => ()))
	 
   in {returnsTo = ! o #returnsTo o get,
       destroy = fn () => ()}
   end

structure Graph = EquivalenceGraph
structure Class = Graph.Class
fun coalesce (program as Program.T {functions, ...}, jumpHandlers, limit) =
   let
      val graph = Graph.new ()
      val {get = funcClass: Func.t -> Class.t, set = setFuncClass,
	   destroy = destroyFuncClass} =
	 Property.destGetSetOnce (Func.plist,
				  Property.initRaise ("class", Func.layout))
      val {get = jumpClass: Jump.t -> Class.t, set = setJumpClass,
	   destroy = destroyJumpClass} =
	 Property.destGetSetOnce (Jump.plist,
				  Property.initRaise ("class", Jump.layout))
      fun 'a newClass (name: 'a, exp: Exp.t, setClass: 'a * Class.t -> unit)
	 : Class.t =
	 let val class = Graph.newClass (graph, size exp)
	 in setClass (name, class)
	    ; class
	 end

      (* Build the initial partition.
       * Ensure that all Cps jumps are in the same equivalence class.
       *)
      fun loopExp (e: Exp.t, n: Class.t, hs: Jump.t list): unit =
	 let val {decs, transfer} = Exp.dest e
	    fun same (j: Jump.t): unit = Graph.==(graph, n, jumpClass j)
	 in List.foreach
	    (decs,
	     fn Fun {name, body, ...} =>
	     loopExp (body, newClass (name, body, setJumpClass),
		      jumpHandlers name)
	      | Bind {exp = PrimApp {info, ...}, ...} =>
		   PrimInfo.foreachJump (info, same)
	      | _ => ())
	    ; (case transfer of
		  Jump {dst, ...} => same dst
		| Case {cases, default, ...} =>
		     (Cases.foreach (cases, same)
		      ; Option.app (default, same))
		| Raise _ => (case List.fold (decs, hs, deltaHandlers) of
				 [] => ()
			       | h :: _ => same h)
		| _ => ())
	 end
      val _ =
	 Vector.foreach (functions, fn Function.T {name, body, ...} =>
			loopExp (body, newClass (name, body, setFuncClass), []))
      val {returnsTo, destroy = destroyReturnsTo} = returnsTo program
      (* Add edges, and then coalesce the graph. *)
      val _ =
	 Vector.foreach
	 (functions, fn Function.T {name, body, ...} =>
	  let
	     val returnsTo = List.revMap (returnsTo name, jumpClass)
	     fun loopExp (e: Exp.t, class: Class.t): unit =
		let val {decs, transfer} = Exp.dest e
		in List.foreach
		   (decs,
		    fn Fun {name, body, ...} => loopExp (body, jumpClass name)
		     | _ => ())
		   ; (case transfer of
			 Call {func, cont, ...} =>
			    Graph.addEdge (graph, {from = class,
						   to = funcClass func})
		       | Return _ =>
			    List.foreach (returnsTo, fn c =>
					  Graph.addEdge (graph, {from = class,
								 to = c}))
		       | _ => ())
		end
	  in loopExp (body, funcClass name)
	  end)
	 
      val _ =
	 if limit = 0
	    then ()
	 else Graph.greedy {graph = graph, maxClassSize = limit}
      type chunk = {funcs: Func.t list ref,
		    jumps: Jump.t list ref}
      val chunks: chunk list ref = ref []
      val {get = classChunk: Class.t -> chunk,
	   destroy = destroyClassChunk, ...} =
	 Property.destGet
	 (Class.plist,
	  Property.initFun (fn _ =>
			    let val c = {funcs = ref [],
					 jumps = ref []}
			    in List.push (chunks, c)
			       ; c
			    end))
      val _ =
	 let
	    fun 'a new (l: 'a,
			get: 'a -> Class.t,
			sel: chunk -> 'a list ref): unit =
	       List.push (sel (classChunk (get l)), l)
	    val _ =
	       Vector.foreach
	       (functions, fn Function.T {name, body, ...} =>
		let
		   val _ = new (name, funcClass, #funcs)
		   val _ =
		      Exp.foreachDec
		      (body,
		       fn Fun {name, ...} => new (name, jumpClass, #jumps)
			| _ => ())
		in ()
		end)
	 in ()
	 end
   in destroyFuncClass ()
      ; destroyJumpClass ()
      ; destroyClassChunk ()
      ; destroyReturnsTo ()
      ; List.revMap (!chunks, fn {funcs, jumps} =>
		     {funcs = !funcs,
		      jumps = !jumps})
   end

fun chunkify {program, jumpHandlers} =
   case !Control.chunk of
      Control.ChunkPerFunc => chunkPerFunc program
    | Control.OneChunk => oneChunk program
    | Control.Coalesce {limit} => coalesce (program, jumpHandlers, limit)
	 
end
