functor LocalFlatten (S: LOCAL_FLATTEN_STRUCTS): LOCAL_FLATTEN = 
struct

open S
open Dec PrimExp Transfer

(* Flatten a jump arg as long as it is only flows to selects and there some
 * tuple constructed in this function that flows to it.
 *)
   
structure ArgInfo =
   struct
      datatype t = T of {fromTuple: bool ref,
			 fromForce: t list ref,
			 toSelect: bool ref,
			 toForce: t list ref}

      fun isFlat (T {fromTuple, toSelect, ...}) =
	 !fromTuple andalso !toSelect

      val isTupled = not o isFlat

      fun layout (i: t): Layout.t =
	 Layout.str (if isFlat i then "flat" else "tupled")

      fun new () = T {fromTuple = ref false,
		      fromForce = ref [],
		      toSelect = ref true,
		      toForce = ref []}

      fun tuple (T {fromTuple = f, fromForce, ...}) =
	 if !f
	    then ()
	 else (f := true; List.foreach (!fromForce, tuple))

      fun nonSelect (T {toSelect = t, toForce, ...}) =
	 if !t
	    then (t := false; List.foreach (!toForce, nonSelect))
	 else ()
	    
      val op <= =
	 fn (lhs as T {fromTuple = f, fromForce, ...},
	     rhs as T {toSelect = t, toForce, ...}) =>
	 let
	    val _ =
	       if !f
		  then tuple rhs
	       else List.push (fromForce, rhs)
	    val _ =
	       if !t
		  then List.push (toForce, lhs)
	       else nonSelect lhs
	 in
	    ()
	 end
   end

structure VarInfo =
   struct
      datatype t =
	 None
       | Arg of ArgInfo.t
       | Tuple
   end

fun flatten (program as Program.T {globals, datatypes, functions, main}) =
   let
      val {get = varInfo: Var.t -> VarInfo.t,
	   set = setVarInfo, ...} =
	 Property.getSetOnce (Var.plist, Property.initConst VarInfo.None)
      type argsInfo = (ArgInfo.t * Type.t) option vector
      val {get = jumpArgs: Jump.t -> argsInfo,
	   set = setJumpArgs, ...} =
	 Property.getSetOnce (Jump.plist,
			      Property.initRaise ("args", Jump.layout))
      val shrinkExp = shrinkExp globals
      val functions =
	 Vector.map
	 (functions, fn Function.T {name, args, body, returns} =>
	  let
	     fun loop (e: Exp.t) =
		let
		   val {decs, transfer} = Exp.dest e
		   fun force (x: Var.t): unit =
		      case varInfo x of
			 VarInfo.Arg i => ArgInfo.nonSelect i
		       | _ => ()
		   fun forces (xs: Var.t vector): unit =
		      Vector.foreach (xs, force)
		   fun forceArgs (j: Jump.t): unit =
		      Vector.foreach (jumpArgs j,
				      fn NONE => ()
				       | SOME (i, _) => ArgInfo.nonSelect i)
		   val _ =
		      List.foreach
		      (decs,
		       fn Bind {var, exp, ...} =>
		             (case exp of
				 ConApp {args, ...} => forces args
			       | PrimApp {args, ...} => forces args
			       | Tuple args =>
				    (setVarInfo (var, VarInfo.Tuple)
				     ; forces args)
			       | Var x => force x
			       | _ => ())
			| Fun {name, args, body, ...} =>
			     (setJumpArgs
			      (name, (Vector.map
				      (args, fn (x, t) =>
				       if Type.isTuple t
					  then
					     let
						val i = ArgInfo.new ()
						val _ =
						   setVarInfo (x, VarInfo.Arg i)
					     in
						SOME (i, t)
					     end
				       else NONE)))
			      ; loop body)
			| HandlerPush j => forceArgs j
			| HandlerPop => ())
		   val _ =
		      case transfer of
			 Bug => ()
		       | Call {args, cont, ...} =>
			    (forces args
			     ; Option.app (cont, forceArgs))
		       | Case {cases, default, ...} =>
			    (Cases.foreach (cases, forceArgs)
			     ; Option.app (default, forceArgs))
		       | Jump {dst, args} =>
			    Vector.foreach2
			    (args, jumpArgs dst,
			     fn (_, NONE) => ()
			      | (x, SOME (i, _)) =>
				   (case varInfo x of
				       VarInfo.Arg i' => ArgInfo.<= (i', i)
				     | VarInfo.None => ()
				     | VarInfo.Tuple => ArgInfo.tuple i))
		       | Raise xs => forces xs
		       | Return xs => forces xs
		in
		   ()
		end
	     val _ = loop body
	     val _ =
		Control.diagnostics
		(fn display =>
		 Exp.foreachVar (body, fn (x, _) =>
				 case varInfo x of
				    VarInfo.Arg i =>
				       display (let open Layout
						in seq [Var.layout x,
						     str " ",
							ArgInfo.layout i]
						end)
				  | _ => ()))
	     fun makeTuple (formals: (Var.t * Type.t) vector,
			    reps: argsInfo)
		: (Var.t * Type.t) vector * Dec.t list =
		let
		   val (argss, decs) =
		      Vector.map2AndFold
		      (formals, reps, [], fn ((x, t), rep, decs) =>
		       case rep of
			  NONE => (Vector.new1 (x, t), decs)
			| SOME (i, t) =>
			     if ArgInfo.isTupled i
				then (Vector.new1 (x, t), decs)
			     else
				let
				   val vars =
				      Vector.map
				      (Type.detuple t, fn t =>
				       (Var.newNoname (), t))
				in
				   (vars,
				    Bind {var = x,
					  ty = t,
					  exp = Tuple (Vector.map (vars, #1))}
				    :: decs)
				end)
		in (Vector.concatV argss, decs)
		end
	     fun makeSelects (args: Var.t vector,
			      formals: argsInfo): Var.t vector * Dec.t list =
		let
		   val (argss, decs) =
		      Vector.map2AndFold
		      (args, formals, [], fn (arg, formal, ac) =>
		       case formal of
			  NONE => (Vector.new1 arg, ac)
			| SOME (i, t) =>
			     if ArgInfo.isTupled i
				then (Vector.new1 arg, ac)
			     else
				let
				   val (vars, decs) =
				      Vector.foldi
				      (Type.detuple t, ([], ac),
				       fn (i, ty, (vars, decs)) =>
				       let val var = Var.newNoname ()
				       in (var :: vars,
					   Bind {var = var,
						 ty = ty,
						 exp = Select {tuple = arg,
							       offset = i}}
					   :: decs)
				       end)
				in (Vector.fromListRev vars, decs)
				end)
		in (Vector.concatV argss, decs)
		end
	     fun anyFlat (v: argsInfo): bool =
		Vector.exists (v,
			       fn NONE => false
				| SOME (i, _) => ArgInfo.isFlat i)
	     fun loop (e: Exp.t): Exp.t =
		let
		   val {decs, transfer} = Exp.dest e
		   val (rest, transfer) =
		      case transfer of
			 Jump {dst, args} =>
			    let
			       val formals = jumpArgs dst
			    in
			       if anyFlat formals
				  then
				     let
					val (args, decs) =
					   makeSelects (args, formals)
				     in
					(decs, Jump {dst = dst, args = args})
				     end
			       else ([], transfer)
			    end
		       | _ => ([], transfer)
		   val decs =
		      List.fold
		      (rev decs, rest, fn (d, ac) =>
		       let
			  val d =
			     case d of
				Fun {name, args, body} =>
				   let
				      val formals = jumpArgs name
				      val body = loop body
				      val (args, body) =
					 if anyFlat formals
					    then
					       let
						  val (args, decs) =
						     makeTuple (args, formals)
					       in
						  (args,
						   Exp.prefixs (body, decs))
					       end
					 else (args, body) 
				   in
				      Fun {name = name,
					   args = args,
					   body = body}
				   end
			      | _ => d
		       in
			  d :: ac
		       end)
		in
		   Exp.make {decs = decs,
			     transfer = transfer}
		end
	     val body = loop body
	     val _ =
		Control.diagnostic
		(fn () =>
		 let
		    open Layout
		 in
		    align [str "before",
			   Exp.layout body]
		 end)
	     val body = shrinkExp body
	     val _ =
		Control.diagnostic
		(fn () =>
		 let
		    open Layout
		 in
		    align [str "after",
			   Exp.layout body]
		 end)
	     val _ = Exp.clear body
	  in
	     Function.T {name = name,
			 args = args,
			 body = body,
			 returns = returns}
	  end)
      val program' = 
	 Program.T {datatypes = datatypes,
		    globals = globals,
		    functions = functions,
		    main = main}
   in
      program'
   end


end
