(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor MatchCompile (S: MATCH_COMPILE_STRUCTS): MATCH_COMPILE =
struct

open S

structure Env = MonoEnv (structure Domain = Var
			 structure Range = Var)
   
structure FlatPat =
   struct
      datatype t =
	 Any
       | Const of Const.t
       | Con of {con: Con.t,
		 targs: Type.t vector,
		 arg: NestedPat.t option}
       | Tuple of NestedPat.t vector

      fun layout p =
	 let
	    open Layout
	 in
	    case p of
	       Any => str "Any"
	     | Const c => Const.layout c
	     | Con {con, arg, ...} => seq [Con.layout con, str " ",
					   Option.layout NestedPat.layout arg]
	     | Tuple v => Vector.layout NestedPat.layout v
	 end

      val isRefutable =
	 fn Any => false
	  | Const _ => true
	  | Con _ => true
	  | Tuple ps => Vector.exists (ps, NestedPat.isRefutable)

      val isAny =
	 fn Any => true
	  | _ => false

      (* get rid of Wild, Var, Layered - also remove unary tuples *)
      fun flatten (var: Var.t, pat: NestedPat.t, env: Env.t): t * Env.t =
	 let
	    fun extend x = Env.extend (env, x, var)
	 in case NestedPat.node pat of
	    NestedPat.Wild => (Any, env)
	  | NestedPat.Var x => (Any, extend x)
	  | NestedPat.Layered (x, p) => flatten (var, p, extend x)
	  | NestedPat.Const c => (Const c, env)
	  | NestedPat.Con x => (Con x, env)
	  | NestedPat.Tuple ps =>
	       if 1 = Vector.length ps
		  then flatten (var, Vector.sub (ps, 0), env)
	       else (Tuple ps, env)
	 end

      fun flattens (vars: Var.t vector,
		    pats: NestedPat.t vector,
		    env: Env.t): t vector * Env.t =
	 Vector.map2AndFold (vars, pats, env, flatten)
   end   

structure Continue =
   struct
      datatype t =
	 Finish of (Var.t -> Var.t) -> Exp.t
       | Matches of FlatPat.t vector option * t

      fun layout c =
	 let
	    open Layout
	 in
	    case c of
	       Finish _ => str "Finish"
	     | Matches (opt, c) =>
		  seq [str "Matches",
		       tuple [Option.layout (Vector.layout FlatPat.layout) opt,
			      layout c]]
	 end
   end
datatype z = datatype Continue.t

structure Info =
   struct
      datatype t = T of {accum: Env.t,
			 continue: Continue.t}

      fun layout (T {accum, continue}) =
	 Layout.record [("accum", Env.layout accum),
			("continue", Continue.layout continue)]
   end

structure Rule =
   struct
      datatype t = T of {info: Info.t,
			 pat: NestedPat.t}

      fun layout (T {info, pat}) =
	 Layout.record [("info", Info.layout info),
			("pat", NestedPat.layout pat)]
   end

structure FlatRule =
   struct
      datatype t = T of {info: Info.t,
			 pat: FlatPat.t}

      local
	 fun make f (T r) = f r
      in
	 val info = make #info
      end

      fun layout (T {info, pat}) =
	 Layout.record [("info", Info.layout info),
			("pat", FlatPat.layout pat)]
   end

structure Finish =
   struct
      type t = Info.t vector -> Exp.t
	 
      fun layout (_: t) = Layout.str "<finish>"
   end

local
   fun make (name, layout) = 
      Trace.trace4
      (concat ["MatchCompile.", name],
       layout, Type.layout, Vector.layout FlatRule.layout, Finish.layout,
       Exp.layout)
in
   val traceMatchFlat = make ("matchFlat", Var.layout)
   val traceTuple = make ("tuple", Exp.layout)
   val traceConst = make ("const", Exp.layout)
end

local
   fun make (inj, get) (cases, finish) =
      inj (Vector.map
	   (cases, fn {const, infos: Info.t list} =>
	    (get (Const.node const), finish (Vector.fromList infos))))
in
   val directCases = 
      [(Type.char,
	make (Cases.char,
	      fn Const.Node.Char c => c
	       | _ => Error.bug "caseChar type error")),
       (Type.int,
	make (Cases.int,
	      fn Const.Node.Int i => i
	       | _ => Error.bug "caseInt type error")),
       (Type.word,
	make (Cases.word,
	      fn Const.Node.Word w => w
	       | _ => Error.bug "caseWord type error")),
       (Type.word8,
	make (Cases.word8,
	      fn Const.Node.Word w => Word8.fromWord w
	       | _ => Error.bug "caseWord8 type error"))]
end

(*---------------------------------------------------*)
(*                   matchCompile                    *)
(*---------------------------------------------------*)

fun matchCompile {test: Var.t,
		  conTycon: Con.t -> Tycon.t,
		  tyconCons: Tycon.t -> Con.t vector,
		  testType: Type.t,
		  caseType: Type.t,
		  cases: (NestedPat.t * ((Var.t -> Var.t) -> Exp.t)) vector}
   : Exp.t =
   let
      fun match (var: Var.t,
		 ty: Type.t,
		 rules: Rule.t vector,
		 finish: Finish.t): Exp.t =
	 let
	    val rules =
	       Vector.map
	       (rules, fn Rule.T {pat, info as Info.T {accum, continue}} =>
		let
		   val (pat, accum) = FlatPat.flatten (var, pat, accum)
		in
		   FlatRule.T {pat = pat,
			       info = Info.T {accum = accum,
					      continue = continue}}
		end)
	 in matchFlat (var, ty, rules, finish)
	 end
      and matchFlat arg: Exp.t =
	 traceMatchFlat
	 (fn (var: Var.t,
	      ty: Type.t,
	      rules: FlatRule.t vector,
	      finish: Finish.t) =>
	  let
	     val test = Exp.var (var, ty)
	  in
	     case Vector.peek (rules, fn FlatRule.T {pat, ...} =>
			       case pat of
				  FlatPat.Any => false
				| _ => true) of
		NONE => finish (Vector.map (rules, FlatRule.info))
	      | SOME (FlatRule.T {pat, info}) =>
		   case pat of
		      FlatPat.Any => Error.bug "matchFlat"
		    | FlatPat.Const _ => const (test, ty, rules, finish)
		    | FlatPat.Con _ => sum (test, rules, finish)
		    | FlatPat.Tuple ps => tuple (test, ty, rules, finish)
	  end) arg
      and matches (vars: (Var.t * Type.t) vector,
		   rules: {pats: NestedPat.t vector option, info: Info.t} vector,
		   finish: Finish.t): Exp.t =
	 let
	    val rules =
	       Vector.map
	       (rules, fn {pats, info as Info.T {accum, continue}} =>
		case pats of
		   NONE => {pats = NONE, info = info}
		 | SOME pats =>
		      let
			 val (pats, accum) =
			    FlatPat.flattens (Vector.map (vars, #1),
					      pats,
					      accum)
		      in {pats = SOME pats,
			  info = Info.T {accum = accum, continue = continue}}
		      end)
	 in matchesFlat (0, vars, rules, finish)
	 end
      and matchesFlat (i: int,
		       vars: (Var.t * Type.t) vector,
		       rules: {pats: FlatPat.t vector option,
			       info: Info.t} vector,
		       finish: Finish.t): Exp.t =
	 if i = Vector.length vars
	    then finish (Vector.map (rules, #info))
	 else
	    let
	       val (var, ty) = Vector.sub (vars, i)
	       val rules =
		  Vector.map
		  (rules, fn {pats, info as Info.T {accum, continue}} =>
		   case pats of
		      NONE =>
			 FlatRule.T
			 {pat = FlatPat.Any,
			  info = Info.T {accum = accum,
					 continue =
					 Matches (NONE, continue)}}
		    | SOME pats =>
			 FlatRule.T
			 {pat = Vector.sub (pats, i),
			  info =
			  Info.T {accum = accum,
				  continue = Matches (SOME pats, continue)}})
	    in matchFlat
	       (var, ty, rules, fn infos =>
		matchesFlat
		(i + 1, vars,
		 Vector.map (infos, fn Info.T {accum, continue} =>
			     case continue of
				Matches (pats, continue) =>
				   {pats = pats,
				    info = Info.T {accum = accum,
						   continue = continue}}
			      | _ => Error.bug "matchesFlat:"),
		 finish))
	    end
      (*------------------------------------*)
      (*               tuple                *)
      (*------------------------------------*)
      and tuple arg =
	 traceTuple
	 (fn (test: Exp.t,
	      ty: Type.t,
	      rules: FlatRule.t vector,
	      finish: Finish.t) =>
	  let
	     val rules =
		Vector.map
		(rules, fn FlatRule.T {pat, info} =>
		 case pat of
		    FlatPat.Any => {pats = NONE, info = info}
		  | FlatPat.Tuple pats => {pats = SOME pats, info = info}
		  | _ => Error.bug "expected tuple pattern")
	  in Exp.detuple
	     {tuple = test,
	      body = fn vars => matches (vars, rules, finish)}
	  end) arg
      (*------------------------------------*)
      (*                sum                 *)
      (*------------------------------------*)
      and sum (test, rules: FlatRule.t vector, finish: Finish.t) =
	 let
	    datatype arg = 
	       NoArg of Info.t list
	     | Arg of {var: Var.t,
		       ty: Type.t,
		       rules: Rule.t list}
	    val (cases, defaults) =
	       Vector.foldr
	       (rules, ([], []),
		fn (FlatRule.T {pat, info}, (cases, defaults)) =>
		case pat of
		   FlatPat.Any =>
		      (List.map
		       (cases, fn {con, tys, arg} =>
			{con = con, tys = tys,
			 arg = (case arg of
				   NoArg infos => NoArg (info :: infos)
				 | Arg {var, ty, rules} =>
				      Arg {var = var,
					   ty = ty,
					   rules = Rule.T {pat = NestedPat.wild ty,
							   info = info} :: rules})}),
		       info :: defaults)
		 | FlatPat.Con {con=c, targs=tys, arg} => 
		      let
			 fun insert cases =
			    case cases of
			       [] =>
				  [{con = c, tys = tys,
				    arg =
				    (case arg of
					NONE => NoArg (info :: defaults)
				      | SOME p =>
					   let val ty = NestedPat.ty p
					   in Arg {var = Var.newNoname (),
						   ty = ty,
						   rules =
						   Rule.T {pat = p, info = info}
						   :: (List.map
						       (defaults, fn info =>
							Rule.T
							{pat = NestedPat.wild ty,
							 info = info}))}
					   end)}]
			     | (cas as {con, tys, arg=a}) :: cases =>
				  if Con.equals (c, con)
				     then {con = con, tys = tys,
					   arg = (case (a, arg) of
						     (NoArg infos, NONE) =>
							NoArg (info :: infos)
						      | (Arg {var, ty, rules},
							 SOME p) =>
							Arg {var = var,
							     ty = ty,
							     rules = 
							     Rule.T {pat = p,
								     info = info}
							     :: rules}
						       | _ => Error.bug "use of constructor with and without arg in pattern match")}
					:: cases
				  else cas :: (insert cases)
		      in (insert cases, defaults)
		      end
		 | _ => Error.bug "expected constructor pat")
	    val cases = Vector.fromList cases
	    val defaults = Vector.fromList defaults
	    val default =
	       if Vector.isEmpty cases
		  then
		     SOME (finish defaults)
	       else
		  let
		     val {con, ...} = Vector.sub (cases, 0)
		     val tycon = conTycon con
		  in if Tycon.equals (tycon, Tycon.exn)
		     orelse Vector.length cases <> (Vector.length
						    (tyconCons tycon))
			then SOME (finish defaults)
		     else NONE
		  end
	    fun normal () =
	       Exp.casee
	       {test = test, default = default,
		ty = caseType,
		cases =
		Cases.con (Vector.map
			   (cases, fn {con, tys, arg} =>
			    let
			       val (arg, rhs) =
				  case arg of
				     NoArg infos =>
					(NONE, finish (Vector.fromList infos))
				   | Arg {var, ty, rules} =>
					(SOME (var, ty),
					 match (var, ty,
						Vector.fromList rules,
						finish))
			    in {con = con,
				targs = tys,
				arg = arg,
				rhs = rhs}
			    end))}
	 in
	    if 1 = Vector.length cases
	       then
		  let
		     val {con, arg, ...} = Vector.sub (cases, 0)
		  in
		     case arg of
			Arg {var, ty, rules} =>
			   if Con.equals (con, Con.reff)
			      then (Exp.lett
				    {var = var,
				     exp = Exp.deref test,
				     body = match (var, ty,
						   Vector.fromList rules,
						   finish)})
			   else normal ()
		      | _ => normal ()
		  end
	    else normal ()
	 end
      (*------------------------------------*)
      (*               const                *)
      (*------------------------------------*)
      and const arg =
	 traceConst
	 (fn (test: Exp.t,
	      ty: Type.t,
	      rules: FlatRule.t vector,
	      finish: Finish.t) =>
	 let
	    val (cases, defaults) =
	       Vector.foldr
	       (rules, ([], []),
		fn (FlatRule.T {pat, info}, (cases, defaults)) =>
		case pat of
		   FlatPat.Any =>
		      (List.map (cases, fn {const, infos} =>
				 {const = const, infos = info :: infos}),
		       info :: defaults)
		 | FlatPat.Const c =>
		      let
			 fun insert (cases, ac) =
			    case cases of
			       [] => Error.bug "match-compile insert"
			     | (casee as {const, infos}) :: cases =>
				  if Const.equals (c, const)
				     then
					{const = c,
					 infos = info :: infos}
					:: List.appendRev (ac, cases)
				  else insert (cases, casee :: ac)
			 val cases =
			    if List.exists (cases, fn {const, ...} =>
					    Const.equals (c, const))
			       then insert (cases, [])
			    else {const = c, infos = info :: defaults} :: cases
		      in (cases, defaults)
		      end
		 | _ => Error.bug "expected Const pat")
	    val default = finish (Vector.fromList defaults)
	    fun loop ds =
	       case ds of
		  [] =>
		     List.fold
		     (cases, default, fn ({const, infos}, rest) =>
		      Exp.iff {test = Exp.equal (test, Exp.const const),
			       thenn = finish (Vector.fromList infos),
			       elsee = rest,
			       ty = caseType})
		| (ty', make) :: ds  =>
		     if Type.equals (ty, ty')
			then Exp.casee {test = test,
					default = SOME default,
					ty = caseType,
					cases = make (Vector.fromList cases,
						      finish)}
		     else loop ds
	 in loop directCases
	 end) arg
   (*------------------------------------*)
   (*    main code for match compile     *)
   (*------------------------------------*)
   in match (test, testType,
	     Vector.map (cases, fn (p, f) =>
			 Rule.T {pat = p,
				 info = Info.T {accum = Env.empty,
						continue = Finish f}}),
	     fn infos =>
	     if Vector.isEmpty infos
		then Error.bug "matchRules: no default"
	     else
		let val Info.T {accum = env, continue} = Vector.sub (infos, 0)
		in
		   case continue of
		      Finish f => f (fn x => Env.lookup (env, x))
		    | _ => Error.bug "matchRules: expecting Finish"
		end)
   end

val matchCompile =
   Trace.trace
   ("matchCompile",
    fn {cases, ...} => Vector.layout (NestedPat.layout o #1) cases,
    Exp.layout)
   matchCompile
   
end
