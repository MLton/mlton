functor RemoveUnused (S: REMOVE_UNUSED_STRUCTS): REMOVE_UNUSED = 
struct

open S
open Exp Transfer

structure VarInfo =
  struct
    datatype t = NotGlobal | Unused of Exp.t | Used
  end

fun remove (program as Program.T {datatypes, globals, functions, main})
  = let
      val {get = varInfo: Var.t -> VarInfo.t, set = setVarInfo}
	= Property.getSet 
	  (Var.plist,
	   Property.initConst VarInfo.NotGlobal)
      val _ = Vector.foreach
	      (globals, 
	       fn Statement.T {var = SOME var, exp, ...} 
	        => setVarInfo(var, VarInfo.Unused exp)
	        | _ => ())
      val {get = tyInfo: Type.t -> {haveDeconed: bool ref}, set = setTyInfo}
	= Property.getSetOnce 
	  (Type.plist,
	   Property.initFun (fn _ => {haveDeconed = ref false}))
      local
	fun make s = (s o tyInfo, ! o s o tyInfo)
      in
	val (haveDeconed, haveDeconed') = make #haveDeconed
      end
      val {get = tyconCons: Tycon.t -> {con: Con.t, args: Type.t vector} vector, 
	   set = setTyconCons}
	= Property.getSetOnce 
	  (Tycon.plist, 
	   Property.initRaise ("RemovedUnused.tyconCons", Tycon.layout))
      val {get = conInfo: Con.t -> {caseTargets: Label.t list ref,
				    dummy: Exp.t option ref,
				    inCase: bool ref,
				    isCon: bool ref,
				    isDecon: bool ref,
				    numCons: int ref,
				    tycon: Tycon.t},
	   set = setConInfo} 
	= Property.getSetOnce 
	  (Con.plist, 
	   Property.initRaise ("RemoveUnused.conInfo", Con.layout))
      local
	fun make s = (s o conInfo, ! o s o conInfo)
      in
	val (caseTargets, caseTargets') = make #caseTargets
	val (dummy, dummy') = make #dummy
	val (inCase, inCase') = make #inCase
	val (isCon, isCon') = make #isCon
	val (isDecon, isDecon') = make #isDecon
	val (numCons, numCons') = make #numCons
	val tycon = #tycon o conInfo
      end
      val conInfo 
	= Trace.trace ("RemoveUnused.conInfo",
		       Con.layout,
		       fn {inCase, isCon, isDecon, numCons, ...} 
		        => Layout.record [("isCon", Bool.layout (!isCon)),
					  ("isDecon", Bool.layout (!isDecon)),
					  ("numCons", Int.layout (!numCons))])
	              conInfo
      fun newConInfo (con, tycon)
	= setConInfo (con, {caseTargets = ref [],
			    dummy = ref NONE,
			    inCase = ref false,
			    isCon = ref false,
			    isDecon = ref false,
			    numCons = ref ~1,
			    tycon = tycon})
      val _ = Vector.foreach
	      (datatypes,
	       fn Datatype.T {tycon, cons}
	        => (setTyconCons (tycon, cons);
		    Vector.foreach (cons, fn {con, ...} => newConInfo (con, tycon))))
      val {get = funcInfo: Func.t -> {start: Label.t},
	   set = setFuncInfo}
	= Property.getSetOnce
	  (Func.plist,
	   Property.initRaise ("RemoveUnused.funcInfo", Func.layout))
      local
	fun make s = (s o conInfo, ! o s o conInfo)
      in
	val start = #start o funcInfo
      end
      val {get = labelInfo: Label.t -> {block: Block.t,
					visited: bool ref},
	   set = setLabelInfo}
	= Property.getSetOnce
	  (Label.plist,
	   Property.initRaise ("RemoveUnused.labelInfo", Label.layout))
      local
	fun make s = (s o labelInfo, ! o s o labelInfo)
      in
	val block = #block o labelInfo
	val (visited, visited') = make #visited
      end
      val _ = List.foreach
	      (functions, 
	       fn function 
	        => let
		     val {blocks, name, start, ...} = Function.dest function
		   in 
		     setFuncInfo(name, {start = start}) ;
		     Vector.foreach
		     (blocks,
		      fn block as Block.T {label, ...}
		       => setLabelInfo(label, {block = block,
					       visited = ref false}))
		   end)
      val todo: Block.t list ref = ref []
      fun visitLabel label
	= let val {visited, block} = labelInfo label
	  in
	    if !visited
	      then ()
	      else (visited := true;
		    List.push (todo, block))
	  end
      val visitFunc = visitLabel o #start o funcInfo
      fun setIsCon c
	= let val {isCon, caseTargets, ...} = conInfo c
	  in
	    isCon := true;
	    List.foreach (!caseTargets, visitLabel)
	  end
      fun setIsDecon c 
	= let val {isDecon, ...} = conInfo c
	  in
	    isDecon := true
	  end
      fun visitExp (e: Exp.t)
	= case e
	    of ConApp {con, args} => (setIsCon con; visitVars args)
	     | Const _ => ()
	     | PrimApp {prim, targs, args}
	     => let
		  val _ = visitVars args
		  datatype z = datatype Type.dest
		  fun decon t
		    = let
			val {haveDeconed, ...} = tyInfo t
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
		in
		  case (Prim.name prim, Vector.length targs)
		    of (Prim.Name.MLton_eq, 1)
		     (* MLton_eq may be used on datatypes used as enums. *)
		     => decon (Vector.sub (targs, 0))
		     | (Prim.Name.MLton_equal, 1)
		     (* MLton_equal will be expanded by poly-equal into uses
		      * of constructors as patterns.
		      *)
		     => decon (Vector.sub (targs, 0))
(*		     | (Prim.Name.MLton_size, 1) => decon (Vector.sub (targs, 0)) *)
		     | _ => ()
		end
	     | Select {tuple, ...} => visitVar tuple
	     | Tuple xs => visitVars xs
	     | Var x => visitVar x
	     | _ => ()
      and visitVar (x: Var.t)
	= case varInfo x
	    of VarInfo.Unused exp => (visitExp exp;
				      setVarInfo (x, VarInfo.Used))
	     | _ => ()
      and visitVars (xs: Var.t Vector.t) = Vector.foreach (xs, visitVar)
      fun visitStatement (s: Statement.t)
	= visitExp (Statement.exp s)
      fun visitStatements (ss: Statement.t Vector.t)
	= Vector.foreach(ss, visitStatement)
      fun visitTransfer (t: Transfer.t) 
	= case t
	    of Bug => ()
	     | Call {func, args, return}
	     => (visitFunc func;
		 visitVars args;
		 Option.app (return,
			     fn {cont, handler} 
			      => (visitLabel cont;
				  Option.app (handler, visitLabel))))
	     | Case {test, cases, default}
	     => let
		  val _ = visitVar test
		  fun doit l = (Vector.foreach (l, fn (_, l) => visitLabel l);
				Option.app (default, visitLabel))
		in
		  case cases 
		    of Cases.Char l => doit l
		     | Cases.Int l => doit l
		     | Cases.Word l => doit l
		     | Cases.Word8 l => doit l
		     | Cases.Con cases
		     => if Vector.length cases = 0
			  then Option.app (default, visitLabel)
			  else let
				 val (c, _) = Vector.sub (cases, 0)
				 val _ = Vector.foreach
				         (cases,
					  fn (c, l)
					   => let
						val {caseTargets, inCase, 
						     isCon, isDecon, ...}
						  = conInfo c
					      in
						if isSome default
						  then inCase := true
						  else ();
						isDecon := true;
						if !isCon
						  then visitLabel l
						  else List.push (caseTargets, l)
					      end)
				 val _ = case default
					   of NONE => ()
					    | SOME l 
					    => Vector.foreach
					       (tyconCons (tycon c),
						fn {con, ...}
						 => let
						      val {caseTargets, 
							   inCase, isCon, ...}
							= conInfo con
						    in
						      if !inCase
							then inCase := false
							else if !isCon
							       then visitLabel l
							       else List.push
								    (caseTargets, l)
						    end)
			       in 
				 ()
			       end
		end
	     | Goto {dst, args} 
	     => (visitLabel dst; 
		 visitVars args)
	     | Prim {args, failure, success, ...} 
	     => (visitLabel failure;
		 visitLabel success;
		 visitVars args)
	     | Raise xs => visitVars xs
	     | Return xs => visitVars xs

      (* Visit all reachable expressions. *)
      val _ = let
		fun doit c = let
			       val {isCon, isDecon, ...} = conInfo c
			     in
			       isCon := true ; isDecon := true
			     end
	      in
		doit Con.truee ; doit Con.falsee
	      end
      fun loop ()
	= case !todo
	    of [] => ()
	     | b::bs => let
			  val _ = todo := bs
			  val Block.T {statements, transfer, ...} = b
			in
			  visitStatements statements;
			  visitTransfer transfer;
			  loop ()
			end
      val _ = (visitFunc main;
	       loop ())
      (* Analysis is done,  Now build the resulting program. *)
      val datatypes
	= Vector.map
	  (datatypes,
	   fn Datatype.T {tycon, cons} 
	    => let
		 val r: Exp.t option ref = ref NONE
		 val cons 
		   = Vector.keepAllMap
		     (cons,
		      fn c as {con, ...}
		       => let
			    val {isCon, isDecon, dummy, ...} = conInfo con
			  in
			    case (!isCon,!isDecon)
			      of (false, _) => NONE
			       | (true, true) => SOME c
			       | (true, false)
			       => let
				    val (e, res)
				      = case !r
					  of NONE 
					   => let
						val c = Con.newString "dummy"
						val e = ConApp {con = c,
								args = Vector.new0 ()}
					      in
						r := SOME e ;
						newConInfo (c, tycon) ;
						(e, SOME {con = c, 
							  args = Vector.new0 ()})
					      end
					   | SOME e => (e, NONE)
				    val _ = dummy := SOME e
				  in
				    res
				  end
			  end)
		 val num = Vector.length cons
		 val _ = Vector.foreach (cons, fn {con, ...} => numCons con := num)
	       in
		 Datatype.T {tycon = tycon, cons = cons}
	       end)
      fun simplifyExp (e: Exp.t): Exp.t
	= case e
	    of ConApp {con, ...} 
	     => let
		  val {isDecon, dummy, ...} = conInfo con
		in
		  if !isDecon
		    then e
		    else valOf (!dummy)
		end
	     | _ => e
      val simplifyExp
	= Trace.trace ("RemoveUnused.simplifyExp",
		       Exp.layout,
		       Exp.layout)
                      simplifyExp
      fun simplifyStatement (s: Statement.t 
			     as Statement.T {var, ty, exp}): Statement.t
	= Statement.T {var = var, ty = ty, exp = simplifyExp exp}
      fun simplifyStatements (ss: Statement.t Vector.t): Statement.t Vector.t
	= Vector.map (ss, simplifyStatement)
      fun simplifyTransfer (t: Transfer.t): Transfer.t
	= case t
	    of Case {test, cases = Cases.Con cases, default}
	     => let
		  val cases = Vector.keepAll(cases, fn (c, _) => !(#isCon (conInfo c)))
		  fun keep default = Case {test = test,
					   cases = Cases.Con cases,
					   default = default}
		  fun none () = keep NONE
		in
		  case default
		    of NONE => none ()
		     | SOME l => if Vector.length cases = 0
				   then if visited' l
					  then Goto {dst = l, args = Vector.new0 ()}
					  else Bug
				   else let
					  val (c, _) = Vector.sub (cases, 0)
					in 
					  if Vector.length cases = numCons' c
					    then none ()
					    else keep (SOME l)
					end
		end
	     | _ => t
      val simplifyTransfer
	= Trace.trace ("RemoveUnused.simplifyTransfer",
		       Transfer.layout,
		       Transfer.layout)
	              simplifyTransfer
      fun simplifyBlock (b: Block.t
			 as Block.T {label, args, statements, transfer}): Block.t
	= Block.T {label = label,
		   args = args,
		   statements = simplifyStatements statements,
		   transfer = simplifyTransfer transfer}
      fun simplifyBlocks (bs: Block.t Vector.t): Block.t Vector.t
	= Vector.keepAllMap
	  (bs,
	   fn b as Block.T {label, args, statements, transfer}
	    => if visited' label
		 then SOME (simplifyBlock b)
		 else NONE)
      val globals
	= Vector.keepAllMap
	  (globals,
	   fn s as Statement.T {var, ty, exp}
	    => case var
		 of NONE => SOME (simplifyStatement s)
		  | SOME var => case varInfo var
				  of VarInfo.Used => SOME (simplifyStatement s)
				   | _ => NONE)

(*
      val shrinkBlock = shrinkBlock globals
*)
      val functions
	= List.keepAllMap
	  (functions,
	   fn function
	    => let
		 val {args, blocks, name, returns, start} = Function.dest function
	       in
		 if visited' start
		   then SOME (Function.new
			      {args = args,
			       blocks = (* shrinkBlocks *) simplifyBlocks blocks,
			       name = name,
			       returns = returns,
			       start = start})
		   else NONE
	       end)

      val program = Program.T {datatypes = datatypes,
			       globals = globals,
			       functions = functions,
			       main = main}
    in
      program
    end
end
