(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
       | Const of {const: Const.t, isChar: bool}
       | Con of {arg: NestedPat.t option,
		 con: Con.t,
		 targs: Type.t vector}
       | Tuple of NestedPat.t vector

      fun layout p =
	 let
	    open Layout
	 in
	    case p of
	       Any => str "Any"
	     | Const {const = c, ...} => Const.layout c
	     | Con {con, arg, ...} =>
		  seq [Con.layout con, str " ",
		       Option.layout NestedPat.layout arg]
	     | Tuple v => Vector.layout NestedPat.layout v
	 end

      (* get rid of Wild, Var, Layered - also remove unary tuples *)
      fun flatten (var: Var.t, pat: NestedPat.t, env: Env.t): t * Env.t =
	 let
	    fun extend x = Env.extend (env, x, var)
	 in
	    case NestedPat.node pat of
	       NestedPat.Con x => (Con x, env)
	     | NestedPat.Const c => (Const c, env)
	     | NestedPat.Layered (x, p) => flatten (var, p, extend x)
	     | NestedPat.Tuple ps =>
		  if 1 = Vector.length ps
		     then flatten (var, Vector.sub (ps, 0), env)
		  else (Tuple ps, env)
	     | NestedPat.Var x => (Any, extend x)
	     | NestedPat.Wild => (Any, env)
	 end

      fun flattens (vars: Var.t vector,
		    pats: NestedPat.t vector,
		    env: Env.t): t vector * Env.t =
	 Vector.map2AndFold (vars, pats, env, flatten)
   end   

structure Continue =
   struct
      datatype t =
	 Finish of (NestedPat.t * (Var.t -> Var.t)) -> Exp.t
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
      type t = NestedPat.t * Info.t vector -> Exp.t
	 
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
   fun make (all, cardinality, ty, inj, get) =
      List.map (all, fn s =>
		(ty s,
		 cardinality s,
		 fn (cases, finish) =>
		 inj (s,
		      Vector.map
		      (cases, fn {const, infos: Info.t list} =>
		       (get const, finish (NestedPat.make
					   (NestedPat.Const {const = const,
							     isChar = false},
					    ty s),
					   Vector.fromList infos))))))
in
   val directCases = 
      make (List.remove (IntSize.all, fn s => IntSize.I64 = s),
	    IntSize.cardinality, Type.int, Cases.int,
	    fn Const.Int i => i
	     | _ => Error.bug "caseInt type error")
      @ make (List.remove (WordSize.all, fn s => WordSize.W64 = s),
	      WordSize.cardinality, Type.word, Cases.word,
	      fn Const.Word w => w
	       | _ => Error.bug "caseWord type error")
end

(* unhandledConst cs returns a constant (of the appropriate type) not in cs. *)
fun unhandledConst (cs: Const.t vector): Const.t =
   let
      fun search (start: 'a, next: 'a -> 'a, make: 'a -> Const.t): Const.t =
	 let
	    fun loop (a: 'a): Const.t =
	       let
		  val c = make a
	       in
		  if Vector.exists (cs, fn c' => Const.equals (c, c'))
		     then loop (next a)
		  else c
	       end
	 in
	    loop start
	 end
      fun search {<= : 'a * 'a -> bool,
		  equals: 'a * 'a -> bool,
		  extract: Const.t -> 'a,
		  isMin: 'a -> bool,
		  make: 'a -> Const.t,
		  next: 'a -> 'a,
		  prev: 'a -> 'a} =
	 let
	    val cs = QuickSort.sortVector (Vector.map (cs, extract), op <=)
	    val c = Vector.sub (cs, 0)
	 in
	    if not (isMin c)
	       then make (prev c)
	    else
	       let
		  val n = Vector.length cs
		  fun loop (i, c) =
		     if i = n orelse not (equals (c, Vector.sub (cs, i)))
			then make c
		     else loop (i + 1, next c)
	       in
		  loop (0, c)
	       end
	 end
      val c = Vector.sub (cs, 0)
      datatype z = datatype Const.t
   in
      case c of
	 Int i =>
	    let
	       val s = IntX.size i
	       val min = IntX.toIntInf (IntX.min s)
	       fun extract c =
		  case c of
		     Int i => IntX.toIntInf i
		   | _ => Error.bug "expected Int"
	    in
	       search {<= = op <=,
		       equals = op =,
		       extract = extract,
		       isMin = fn i => i = min,
		       make = fn i => Const.int (IntX.make (i, s)),
		       next = fn i => i + 1,
		       prev = fn i => i - 1}

	    end
       | IntInf _ =>
	    let
	       fun extract c =
		  case c of
		     IntInf i => i
		   | _ => Error.bug "expected IntInf"
	    in
	       search {<= = op <=,
		       equals = op =,
		       extract = extract,
		       isMin = fn _ => false,
		       make = Const.IntInf,
		       next = fn i => i + 1,
		       prev = fn i => i - 1}
	    end
       | Real _ => Error.bug "match on real is not allowed"
       | Word w =>
	    let
	       val s = WordX.size w
	       fun extract c =
		  case c of
		     Word w => WordX.toLargeWord w
		   | _ => Error.bug "expected Word"
	    in
	       search {<= = op <=,
		       equals = op =,
		       extract = extract,
		       isMin = fn w => w = 0w0,
		       make = fn w => Const.word (WordX.make (w, s)),
		       next = fn w => w + 0w1,
		       prev = fn w => w - 0w1}
	    end
       | Word8Vector _ =>
	    let
	       val max =
		  Vector.fold
		  (cs, ~1, fn (c, max) =>
		   case c of
		      Word8Vector v => Int.max (max, Vector.length v)
		    | _ => Error.bug "expected Word8Vector")
	       val w = Word8.fromChar #"a"
	    in
	       Const.Word8Vector (Vector.tabulate (max + 1, fn _ => w))
	    end
   end

(*---------------------------------------------------*)
(*                   matchCompile                    *)
(*---------------------------------------------------*)

fun matchCompile {caseType: Type.t,
		  cases: (NestedPat.t
			  * (NestedPat.t * (Var.t -> Var.t) -> Exp.t)) vector,
		  conTycon: Con.t -> Tycon.t,
		  region: Region.t,
		  test: Var.t,
		  testType: Type.t,
		  tyconCons: Tycon.t -> {con: Con.t,
					 hasArg: bool} vector}: Exp.t =
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
	 in
	    matchFlat (var, ty, rules, finish)
	 end
      and matchFlat arg: Exp.t =
	 traceMatchFlat
	 (fn (var: Var.t,
	      ty: Type.t,
	      rules: FlatRule.t vector,
	      finish: Finish.t) =>
	  case Vector.peek (rules, fn FlatRule.T {pat, ...} =>
			    case pat of
			       FlatPat.Any => false
			     | _ => true) of
	     NONE => finish (NestedPat.wild ty,
			     Vector.map (rules, FlatRule.info))
	   | SOME (FlatRule.T {pat, info}) =>
		let
		   val test = Exp.var (var, ty)
		in
		   case pat of
		      FlatPat.Any => Error.bug "matchFlat"
		    | FlatPat.Const _ => const (test, ty, rules, finish)
		    | FlatPat.Con _ => sum (test, ty, rules, finish)
		    | FlatPat.Tuple _ => tuple (test, ty, rules, finish)
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
	 in
	    matchesFlat (0, vars, [], rules, finish)
	 end
      and matchesFlat (i: int,
		       vars: (Var.t * Type.t) vector,
		       pats: NestedPat.t list,
		       rules: {pats: FlatPat.t vector option,
			       info: Info.t} vector,
		       finish: Finish.t): Exp.t =
	 if i = Vector.length vars
	    then finish (NestedPat.tuple (Vector.fromListRev pats),
			 Vector.map (rules, #info))
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
					 continue = Matches (NONE, continue)}}
		    | SOME pats =>
			 FlatRule.T
			 {pat = Vector.sub (pats, i),
			  info =
			  Info.T {accum = accum,
				  continue = Matches (SOME pats, continue)}})
	    in
	       matchFlat
	       (var, ty, rules, fn (pat, infos) =>
		matchesFlat
		(i + 1, vars, pat :: pats,
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
	  in
	     Exp.detuple {tuple = test,
			  body = fn vars => matches (vars, rules, finish)}
	  end) arg
      (*------------------------------------*)
      (*                sum                 *)
      (*------------------------------------*)
      and sum (test, ty: Type.t, rules: FlatRule.t vector, finish: Finish.t) =
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
		     SOME (finish (NestedPat.wild ty, defaults),
			   region)
	       else
		  let
		     val {con, ...} = Vector.sub (cases, 0)
		     val tycon = conTycon con
		     fun done defaultPat =
			SOME (finish (defaultPat, defaults), region)
		  in
		     if Tycon.equals (tycon, Tycon.exn)
			then done (NestedPat.make
				   (NestedPat.Var (Var.fromString "e"),
				    ty))
		     else
			let
			   val cons = tyconCons tycon
			in
			   if Vector.length cases = Vector.length cons
			      then NONE
			   else
			      done
			      (case (Vector.peek
				     (cons, fn {con, ...} =>
				      not (Vector.exists
					   (cases, fn {con = con', ...} =>
					    Con.equals (con, con'))))) of
				  NONE =>
				     Error.bug "unable to find default example"
				| SOME {con, hasArg} =>
				     let
					val arg =
					   if hasArg
					      then SOME (NestedPat.wild
							 Type.unit)
					   else NONE
				     in
					NestedPat.make
					(NestedPat.Con
					 {arg = arg,
					  con = con,
					  targs = Vector.new0 ()},
					 ty)
				     end)
			end
		  end
	    fun normal () =
	       Exp.casee
	       {test = test, default = default,
		ty = caseType,
		cases =
		Cases.con (Vector.map
			   (cases, fn {con, tys, arg} =>
			    let
			       fun conPat arg =
				  NestedPat.make
				  (NestedPat.Con {arg = arg,
						  con = con,
						  targs = tys},
				   ty)
			       val (arg, rhs) =
				  case arg of
				     NoArg infos =>
					(NONE,
					 finish (conPat NONE,
						 Vector.fromList infos))
				   | Arg {var, ty, rules} =>
					(SOME (var, ty),
					 match (var, ty,
						Vector.fromList rules,
						fn (p, e) =>
						finish (conPat (SOME p), e)))
			    in {con = con,
				targs = tys,
				arg = arg,
				rhs = rhs}
			    end))}
	 in
	    if 1 = Vector.length cases
	       then
		  let
		     val {arg, con, tys, ...} = Vector.sub (cases, 0)
		  in
		     case arg of
			Arg {var, ty, rules} =>
			   if Con.equals (con, Con.reff)
			      then
				 Exp.lett
				 {var = var,
				  exp = Exp.deref test,
				  body = match (var, ty,
						Vector.fromList rules,
						fn (p, e) =>
						finish (NestedPat.make
							(NestedPat.Con
							 {arg = SOME p,
							  con = Con.reff,
							  targs = tys},
							 ty),
							e))}
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
	    val isChar =
	       case Vector.peekMap (rules, fn FlatRule.T {pat, ...} =>
				    case pat of
				       FlatPat.Const {isChar, ...} => SOME isChar
				     | _ => NONE) of
		  NONE => false
		| SOME isChar => isChar
	    val (cases, defaults) =
	       Vector.foldr
	       (rules, ([], []),
		fn (FlatRule.T {pat, info}, (cases, defaults)) =>
		case pat of
		   FlatPat.Any =>
		      (List.map (cases, fn {const, infos} =>
				 {const = const, infos = info :: infos}),
		       info :: defaults)
		 | FlatPat.Const {const = c, ...} =>
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
		      in
			 (cases, defaults)
		      end
		 | _ => Error.bug "expected Const pat")
	    fun default () =
	       let
		  val cs = Vector.fromListMap (cases, #const)
		  val unhandled = 
		     if 0 = Vector.length cs
			then NestedPat.Wild
		     else NestedPat.Const {const = unhandledConst cs,
					   isChar = isChar}
	       in
		  finish (NestedPat.make (unhandled, ty),
			  Vector.fromList defaults)
	       end
	 in
	    case List.peek (directCases, fn (ty', _, _) =>
			    Type.equals (ty, ty')) of
	       NONE =>
		  List.fold
		  (cases, default (), fn ({const, infos}, rest) =>
		   Exp.iff {test = Exp.equal (test, Exp.const const),
			    thenn = finish (NestedPat.make
					    (NestedPat.Const {const = const,
							      isChar = false},
					     ty),
					    Vector.fromList infos),
			    elsee = rest,
			    ty = caseType})
	     | SOME (_, cardinality, make) =>
		  let
		     val cases = Vector.fromList cases
		     val default =
			if cardinality = IntInf.fromInt (Vector.length cases)
			   then NONE
			else SOME (default (), region)
		  in
		     Exp.casee {cases = make (cases, finish),
				default = default,
				test = test,
				ty = caseType}
		  end
	 end) arg
   (*------------------------------------*)
   (*    main code for match compile     *)
   (*------------------------------------*)
   in
      match (test, testType,
	     Vector.map (cases, fn (p, f) =>
			 Rule.T {pat = p,
				 info = Info.T {accum = Env.empty,
						continue = Finish f}}),
	     fn (pat, infos) =>
	     if Vector.isEmpty infos
		then Error.bug "matchRules: no default"
	     else
		let
		   val Info.T {accum = env, continue} = Vector.sub (infos, 0)
		in
		   case continue of
		      Finish f => f (pat, fn x => Env.lookup (env, x))
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
