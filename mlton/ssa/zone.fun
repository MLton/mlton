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
   
fun zoneFunction (f, ac) =
   let
      val {args, blocks, mayInline, name, raises, returns, start} =
	 Function.dest f
   in
      if Vector.length blocks <= !Control.maxFunctionSize
	 then f :: ac
      else
	 let
	    datatype count = Many | None | One of Label.t
	    fun inc (r: count ref, l: Label.t): unit =
	       case !r of
		  Many => ()
		| None => r := One l
		| One _ => r := Many
	    val {get = labelInfo: Label.t -> {class: {displayed: bool ref,
						      size: int} Set.t,
					      preds: count ref,
					      succs: count ref},
		 ...} =
	       Property.get
	       (Label.plist,
		Property.initFun
		(fn _ => {class = Set.singleton {displayed = ref false,
						 size = 1},
			  preds = ref None,
			  succs = ref None}))
	    (* Count predecessors and successors. *)
	    val () =
	       Vector.foreach
	       (blocks, fn Block.T {label, transfer, ...} =>
		let
		   val {succs, ...} = labelInfo label
		in
		   Transfer.foreachLabel
		   (transfer, fn l =>
		    (inc (succs, l)
		     ; inc (#preds (labelInfo l), label)))
		end)
	    (* Put in classes. *)
	    val () =
	       Vector.foreach
	       (blocks, fn Block.T {label, transfer, ...} =>
		let
		   val {class = c, succs, ...} = labelInfo label
		in
		   case !succs of
		      One l =>
			 let
			    val {class = c', preds, ...} = labelInfo l
			 in
			    case !preds of
			       One _ =>
				  let
				     val {displayed = d, size = s} = Set.value c
				     val {size = s', ...} = Set.value c'
				     val () = Set.union (c, c')
				     val () =
					Set.setValue
					(c, {displayed = d, size = s + s'})
				  in
				     ()
				  end
			     | _ => ()
			 end
		    | _ => ()
		end)
	    (* Display classes. *)
	    val () =
	       Control.diagnostics
	       (fn display =>
		(display (Func.layout name)
		 ; (Vector.foreach
		    (blocks, fn Block.T {label, ...} =>
		     let
			val {displayed, size} =
			   Set.value (#class (labelInfo label))
		     in
			if size > 1 andalso !displayed
			   then ()
			else (displayed := true; display (Int.layout size))
		     end))))
	 (* Cut into zones based on size. *)
	    (* Compute live variables at each cut node. *)
	    (* Build a procedure for each zone, passing in the lives that
	     * it needs.  Make the outer procedure into a trampoline.
	     *)
	 in
	    f :: ac
	 end
   end
   
fun zone (Program.T {datatypes, globals, functions, main}) =
   Program.T {datatypes = datatypes,
	      globals = globals,
	      functions = List.fold (functions, [], zoneFunction),
	      main = main}

end

