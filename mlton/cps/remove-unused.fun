functor RemoveUnused (S: REMOVE_UNUSED_STRUCTS): REMOVE_UNUSED = 
struct

open S
open Dec PrimExp Transfer

structure VarInfo =
   struct
      datatype t = NotGlobal | Unused of PrimExp.t | Used
   end

fun remove (Program.T {datatypes, globals, functions, main}) =
   let
      val {get = varInfo: Var.t -> VarInfo.t, set = setVarInfo, ...} =
	 Property.getSet (Var.plist, Property.initConst VarInfo.NotGlobal)
      val _ = Vector.foreach (globals, fn {var, exp, ...} =>
			     setVarInfo (var, VarInfo.Unused exp))
      val {get = tyInfo: Type.t -> {haveDeconed: bool ref}, 
	   set = setTyInfo, ...} =
	 Property.getSetOnce (Type.plist, 
			      Property.initFun 
			      (fn _ => {haveDeconed = ref false}))
      val {get = tyconCons, set = setTyconCons, ...} =
	 Property.getSetOnce (Tycon.plist,
			      Property.initRaise ("cons", Tycon.layout))
      val {get = conInfo: Con.t -> {caseTargets: Jump.t list ref,
				    dummy: PrimExp.t option ref,
				    inCase: bool ref,
				    isCon: bool ref,
				    isDecon: bool ref,
				    numCons: int ref,
				    tycon: Tycon.t},
	   set = setConInfo, ...} =
	 Property.getSetOnce
	 (Con.plist, Property.initRaise ("RemoveUnused.info", Con.layout))
      val conInfo =
	 Trace.trace ("RemoveUnused.conInfo",
		      Con.layout,
		      fn {inCase, isCon, isDecon, numCons, ...} =>
		      Layout.record [("isCon", Bool.layout (!isCon)),
				     ("isDecon", Bool.layout (!isDecon)),
				     ("numCons", Int.layout (!numCons))])
	 conInfo
      fun newConInfo (con, tycon) =
	 setConInfo (con, {caseTargets = ref [],
			   dummy = ref NONE,
			   inCase = ref false,
			   isCon = ref false,
			   isDecon = ref false,
			   numCons = ref ~1,
			   tycon = tycon})
      val _ =
	 Vector.foreach
	 (datatypes, fn {tycon, cons} =>
	  (setTyconCons (tycon, cons)
	   ; Vector.foreach (cons, fn {con, ...} => newConInfo (con, tycon))))
      val {get = funcInfo: Func.t -> {body: Exp.t,
				      visited: bool ref},
	   set = setFuncInfo, ...} =
	 Property.getSetOnce
	 (Func.plist, Property.initRaise ("RemoveUnused.info", Func.layout))
      val _ =
	 Vector.foreach (functions, fn Function.T {name, body, ...} =>
			 setFuncInfo (name, {body = body, visited = ref false}))
      val {get = jumpInfo: Jump.t -> {body: Exp.t,
				      visited: bool ref},
	   set = setJumpInfo, ...} =
	 Property.getSetOnce
	 (Jump.plist, Property.initRaise ("RemoveUnused.info", Jump.layout))
      val todo: Exp.t list ref = ref []
      local
	 fun 'a make info (a: 'a) =
	    let val {visited, body} = info a
	    in if !visited
		  then ()
	       else (visited := true
		     ; List.push (todo, body))
	    end
      in
	 val visitFunc = make funcInfo
	 val visitJump = make jumpInfo
      end
      fun setIsCon c =
	 let val {isCon, caseTargets, ...} = conInfo c
	 in isCon := true
	    ; List.foreach (!caseTargets, visitJump)
	 end
      fun setIsDecon c = #isDecon (conInfo c) := true
      fun visitPrimExp (e: PrimExp.t): unit =
	 case e of
	    ConApp {con, args} => (setIsCon con; visitVars args)
	  | Const _ => ()
	  | PrimApp {prim, info, targs, args, ...} =>
	       let
		  val _ = PrimInfo.foreachJump (info, visitJump)
		  val _ = visitVars args
		  datatype z = datatype Type.dest
		  fun decon t
		    = let
			val haveDeconed = #haveDeconed (tyInfo t)
		      in
			if !haveDeconed
			  then ()
			  else (haveDeconed := true;
				case Type.dest t 
				  of Datatype t
				   => Vector.foreach
				      (tyconCons t,
				       fn {con, args}
				        => (setIsDecon con;
					    Vector.foreach (args, decon)))
				   | Tuple ts => Vector.foreach (ts, decon)
				   | Vector t => decon t
				   | _ => ())
		      end
	       in case (Prim.name prim, Vector.length targs) of
                  (Prim.Name.MLton_eq, 1) =>
                     (* MLton_eq may be used on datatypes used as enums. *)
                     decon (Vector.sub (targs, 0))
                | (Prim.Name.MLton_equal, 1) =>
                     (* MLton_equal will be expanded by poly-equal into uses
                      * of constructors as patterns.
                      *)
                     decon (Vector.sub (targs, 0))
(*		| (Prim.Name.MLton_size, 1) => decon (Vector.sub (targs, 0)) *)
		| _ => ()
	       end
	  | Select {tuple, ...} => visitVar tuple
	  | Tuple xs => visitVars xs
	  | Var x => visitVar x
      and visitVar x =
	 case varInfo x of
	    VarInfo.Unused exp =>
	       (visitPrimExp exp
		; setVarInfo (x, VarInfo.Used))
	  | _ => ()
      and visitVars xs = Vector.foreach (xs, visitVar)
      fun visitDec (d: Dec.t) =
	 case d of
	    Bind {exp, ...} => visitPrimExp exp
	  | Fun {name, body, ...} =>
	       setJumpInfo (name, {body = body, visited = ref false})
	  | HandlerPush j => visitJump j
	  | HandlerPop => ()
      fun visitTransfer (t: Transfer.t) =
	 case t of
	    Bug => ()
	  | Call {func = f, args, cont, ...} =>
	       (visitFunc f
		; visitVars args
		; Option.app (cont, visitJump))
	  | Case {test, cases, default, ...} =>
	       let
		  val _ = visitVar test
		  fun doit l =
		     (Vector.foreach (l, fn (_, j) => visitJump j)
		      ; Option.app (default, visitJump))
		  datatype z = datatype Cases.t
	       in case cases of
		  Cases.Char l => doit l
		| Cases.Int l => doit l
		| Cases.Word l => doit l
		| Cases.Word8 l => doit l
		| Cases.Con cases =>
		     if 0 = Vector.length cases
			then Option.app (default, visitJump)
		     else
			let
			   val (c, _) = Vector.sub (cases, 0)
			   val _ =
			      Vector.foreach
			      (cases, fn (c, j) =>
			       let
				  val {caseTargets, inCase, isCon, isDecon,
				       ...} = conInfo c
			       in if isSome default then inCase := true else ()
				  ; isDecon := true
				  ; if !isCon
				       then visitJump j
				    else List.push (caseTargets, j)
			       end)
			   val _ =
			      case default of
				 NONE => ()
			       | SOME def => 
				    Vector.foreach
				    (tyconCons (#tycon (conInfo c)),
				     fn {con, ...} =>
				     let
					val {caseTargets, inCase, isCon, ...} =
					   conInfo con
				     in if !inCase
					   then inCase := false
					else if !isCon
						then visitJump def
					     else List.push (caseTargets, def)
				     end)
			in ()
			end
	       end
	  | Jump {dst, args} => (visitJump dst; visitVars args)
	  | Raise xs => visitVars xs
	  | Return xs => visitVars xs
      (* Visit all reachable expressions. *)
      val _ =
	 let
	    fun doit c =
	       let val {isCon, isDecon, ...} = conInfo c
	       in isCon := true; isDecon := true
	       end
	 in doit Con.truee; doit Con.falsee
	 end
      val _ = visitFunc main
      val _ =
	 let
	    fun loop () =
	       case !todo of
		  [] => ()
		| e :: es =>
		     let
			val _ = todo := es
			val {decs, transfer} = Exp.dest e
			val _ = List.foreach (decs, visitDec)
			val _ = visitTransfer transfer
		     in loop ()
		     end
	 in loop ()
	 end
      (* Analysis is done.  Now build the resulting program. *)
      val datatypes =
	 Vector.map
	 (datatypes, fn {tycon, cons} =>
	  let
	     val r: PrimExp.t option ref = ref NONE
	     val cons =
		Vector.keepAllMap
		(cons, fn ca as {con, ...} =>
		 let
		    val {isCon, isDecon, dummy, ...} = conInfo con
		 in
		    case (!isCon, !isDecon) of
		       (false, _) => NONE
		     | (true, true) => SOME ca
		     | (true, false) => 
			  let
			     val (e, res) =
				case !r of
				   NONE => let val c = Con.newString "dummy"
					       val e =
						  ConApp {con = c,
							  args = Vector.new0 ()}
					   in r := SOME e
					      ; newConInfo (c, tycon)
					      ; (e, SOME {con = c,
							  args = Vector.new0 ()})
					   end
				 | SOME e => (e, NONE)
			     val _ = dummy := SOME e
			  in res
			  end
		 end)
	     val numCons = Vector.length cons
	     val _ = Vector.foreach (cons, fn {con, ...} =>
				     #numCons (conInfo con) := numCons)
	  in {tycon = tycon, cons = cons}
	  end)
      fun simplifyTransfer (t: Transfer.t): Transfer.t =
	 case t of
	    Case {cause, test, cases = Cases.Con cases, default} =>
	       let
		  val cases = Vector.keepAll (cases, fn (c, _) =>
					      ! (#isCon (conInfo c)))
		  fun keep default = Case {cause = cause,
					   test = test,
					   cases = Cases.Con cases,
					   default = default}
		  fun none () = keep NONE
	       in case default of
		  NONE => none ()
		| SOME j =>
		     if 0 = Vector.length cases
			then
			   if ! (#visited (jumpInfo j))
			      then Jump {dst = j, args = Vector.new0 ()}
			   else Bug
		     else
			let
			   val (c, _) = Vector.sub (cases, 0)
			in
			   if ! (#numCons (conInfo c)) = Vector.length cases
			      then none ()
			   else keep (SOME j)
			end
	       end
	  | _ => t
      val simplifyTransfer =
	 Trace.trace ("RemoveUnused.simplifyTransfer",
		      Transfer.layout,
		      Transfer.layout)
	 simplifyTransfer
      fun simplifyPrimExp (e: PrimExp.t): PrimExp.t =
	 case e of
	    ConApp {con, ...} =>
	       let val {isDecon, dummy, ...} = conInfo con
	       in if !isDecon
		     then e
		  else valOf (!dummy)
	       end
	  | _ => e
      val simplifyPrimExp =
	 Trace.trace ("RemoveUnused.simplifyPrimExp",
		      PrimExp.layout,
		      PrimExp.layout)
	 simplifyPrimExp
      fun simplifyExp (e: Exp.t): Exp.t =
	 let
	    val {decs, transfer} = Exp.dest e
	    val decs =
	       List.fold
	       (List.rev decs, [], fn (d, ds) =>
		case d of
		   Fun {name, args, body} =>
		      let val {visited, ...} = jumpInfo name
		      in if !visited
			    then Fun {name = name,
				      args = args,
				      body = simplifyExp body} :: ds
			 else ds
		      end
		 | Bind {var, ty, exp} =>
		      Bind {var = var, ty = ty, exp = simplifyPrimExp exp} :: ds
		 | HandlerPop => d :: ds
		 | HandlerPush _ => d :: ds)
	 in Exp.make {decs = decs,
		      transfer = simplifyTransfer transfer}
	 end
      val globals = 
	 Vector.keepAllMap
	 (globals, fn {var, ty, exp} =>
	  case varInfo var of
	     VarInfo.Used =>
		SOME {var = var, ty = ty, exp = simplifyPrimExp exp}
	   | _ => NONE)
      val shrinkExp = shrinkExp globals
      val functions =
	 Vector.keepAllMap
	 (functions, fn Function.T {name, args, body, returns} =>
	  let val {visited, ...} = funcInfo name
	  in if !visited
		then SOME (Function.T {name = name, args = args,
				       returns = returns,
				       body = shrinkExp (simplifyExp body)})
	     else NONE
	  end)
      val p =
	 Program.T {datatypes = datatypes,
		    globals = globals,
		    functions = functions,
		    main = main}
      val _ = Program.clear p
   in
      p
   end

end
