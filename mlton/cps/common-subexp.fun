functor CommonSubexp (S: COMMON_SUBEXP_STRUCTS): COMMON_SUBEXP = 
struct

open S
open Dec PrimExp Transfer

type word = Word.t

fun eliminate (program as Program.T {globals, datatypes, functions, main}) =
   if not (!Control.commonSubexp)
      then program
   else
   let
      val _ =
	 Ref.fluidLet
	 (Control.aux, true, fn () =>
	  Control.displays
	  ("pre-cse", fn display =>
	   Program.layouts (program, display)))
      (* Keep track of the replacements of variables. *)
      val {get = replace: Var.t -> Var.t option, set = setReplace} =
	 Property.getSetOnce (Var.plist, Property.initConst NONE)
      (* Keep track of the variable that holds the length of arrays (and
       * vectors and strings).
       *) 
      val {get = getLength: Var.t -> Var.t option, set = setLength} =
	 Property.getSetOnce (Var.plist, Property.initConst NONE)
      fun canonVar x =
	 case replace x of
	    NONE => x
	  | SOME y => y
      fun canonVars xs = Vector.map (xs, canonVar)
      (* Canonicalize a PrimExp.
       * Replace vars with their replacements.
       * Put commutative arguments in canonical order.
       *)
      fun canon (e: PrimExp.t): PrimExp.t =
	 case e of
	    ConApp {con, args} =>
	       ConApp {con = con, args = canonVars args}
	  | Const _ => e
	  | PrimApp {prim, info, targs, args} =>
	       let
		  fun doit args = 
		     PrimApp {prim = prim, info = info, targs = targs,
			      args = args}
		  val args = canonVars args
		  fun arg i = Vector.sub (args, i)
		  fun canon2 () =
		     let
			val a0 = arg 0
			val a1 = arg 1
		     in
			(* What we really want is a total orderning on
			 * variables.  Since we don't have one, we just use
			 * the total ordering on hashes, which means that
			 * we may miss a few cse's but we won't be wrong.
			 *)
			if Var.hash a0 <= Var.hash a1
			   then (a0, a1)
			else (a1, a0)
		     end
		  datatype z = datatype Prim.Name.t
	       in
		  if Prim.isCommutative prim
		     then doit (Vector.new2 (canon2 ()))
		  else
		     if (case Prim.name prim of
			    IntInf_add => true
			  | IntInf_mul => true
			  | _ => false)
			then
			   let
			      val (a0, a1) = canon2 ()
			   in doit (Vector.new3 (a0, a1, arg 2))
			   end
		     else doit args
	       end
	  | Select {tuple, offset} => Select {tuple = canonVar tuple,
					      offset = offset}
	  | Tuple xs => Tuple (canonVars xs)
	  | Var x => Var (canonVar x)

      (* Keep a hash table of canonicalized PrimExps that are in scope. *)
      val table: {hash: word, exp: PrimExp.t, var: Var.t} HashSet.t =
	 HashSet.new {hash = #hash}
      fun loopExp (e: Exp.t): Exp.t =
	 let
	    val {decs, transfer} = Exp.dest e
	    fun loopDecs decs =
	       case decs of
		  [] => []
		| d :: decs => loopDec (d, decs)
	    and loopDec (d, decs) =
	       case d of
		  Bind {var, ty, exp} =>
		     let
			val exp = canon exp
			fun keep () =
			   Bind {var = var, ty = ty, exp = exp}
			   :: loopDecs decs
			fun lookup (exp: PrimExp.t) =
			   let
			      val hash = PrimExp.hash exp
			      val {var = var', ...} =
				 HashSet.lookupOrInsert
				 (table, hash, fn {exp = exp', ...} =>
				  PrimExp.equals (exp, exp'),
				  fn () => {exp = exp,
					    hash = hash,
					    var = var})
			   in
			      if Var.equals (var, var')
				 then
				    let
				       val decs = loopDecs decs
				       val _ = 
					  HashSet.remove
					  (table, hash, fn {var = var', ...} =>
					   Var.equals (var, var'))
				    in
				       Bind {var = var, ty = ty, exp = exp}
				       :: decs
				    end
			      else
				 (setReplace (var, SOME var')
				  ; loopDecs decs)
			   end
		     in
			case exp of
			   PrimApp (pa as {prim, args, ...}) =>
			      let
				 fun arg () = Vector.sub (args, 0)
				 fun knownLength x =
				    let
				       val _ = setLength (var, SOME x)
				    in
				       keep ()
				    end
				 fun conv () =
				    case getLength (arg ()) of
				       NONE => keep ()
				     | SOME x => knownLength x
				 fun length () =
				    case getLength (arg ()) of
				       NONE =>
					  (* We don't know the length, but it
					   * still might be a common subexp.
					   *)
					  lookup exp
				     | SOME x =>
					  (setReplace (var, SOME x)
					   ; loopDecs decs)
				 datatype z = datatype Prim.Name.t
			      in
				 case Prim.name prim of
				    Array_array => knownLength (arg ())
				  | Array_length => length ()
				  | Vector_fromArray => conv ()
				  | String_fromCharVector => conv ()
				  | String_fromWord8Vector => conv ()
				  | String_toCharVector => conv ()
				  | String_toWord8Vector => conv ()
				  | String_size => length ()
				  | Vector_length => length ()
				  | _ =>
				       if Prim.isFunctional prim
					  orelse Prim.mayOverflow prim
					  then lookup exp
				       else keep ()
			      end
			 | _ => lookup exp
		     end
		| Fun {name, args, body} => 
		     Fun {name = name,
			  args = args,
			  body = loopExp body} :: loopDecs decs
		| _ => d :: loopDecs decs
	 in
	    Exp.make {decs = loopDecs decs,
		      transfer = Transfer.replaceVar (transfer, canonVar)}
	 end
      val shrink = shrinkExp globals
      val functions =
	 Vector.map
	 (functions, fn {name, args, body, returns} =>
	  let
	     val body = shrink (loopExp body)
	     val _ = Exp.clear body
	  in
	     {name = name,
	      args = args,
	      body = body,
	      returns = returns}
	  end)
      val program = 
	 Program.T {datatypes = datatypes,
		    globals = globals,
		    functions = functions,
		    main = main}
   in
      program
   end


end
