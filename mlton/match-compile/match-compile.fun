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
	 Finish of Layout.t list ref * ((Var.t -> Var.t) -> Exp.t)
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
      type t = Layout.t * Info.t vector -> Exp.t
	 
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
		       (get const, finish (Const.layout const,
					   Vector.fromList infos))))))
in
   val directCases = 
      make (List.remove (IntSize.all, fn s =>
			 IntSize.equals (s, IntSize.I (Bits.fromInt 64))),
	    IntSize.cardinality, Type.int, Cases.int,
	    fn Const.Int i => i
	     | _ => Error.bug "caseInt type error")
      @ make (List.remove (WordSize.all, fn s =>
			   WordSize.equals
			   (s, WordSize.fromBits (Bits.fromInt 64))),
	      WordSize.cardinality, Type.word, Cases.word,
	      fn Const.Word w => w
	       | _ => Error.bug "caseWord type error")
end

(* unhandledConst cs returns a constant (of the appropriate type) not in cs. *)
fun unhandledConst (cs: Const.t vector): Const.t =
   let
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
		     Word w => WordX.toIntInf w
		   | _ => Error.bug "expected Word"
	    in
	       search {<= = op <=,
		       equals = op =,
		       extract = extract,
		       isMin = fn w => w = 0,
		       make = fn w => Const.word (WordX.fromIntInf (w, s)),
		       next = fn w => w + 1,
		       prev = fn w => w - 1}
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

local
   open Layout
in
   val wild = str "_"

   fun conApp (c, p) = paren (seq [Con.layout c, str " ", p])
end

(*---------------------------------------------------*)
(*                   matchCompile                    *)
(*---------------------------------------------------*)

fun matchCompile {caseType: Type.t,
		  cases: (NestedPat.t * ((Var.t -> Var.t) -> Exp.t)) vector,
		  conTycon: Con.t -> Tycon.t,
		  region: Region.t,
		  test: Var.t,
		  testType: Type.t,
		  tyconCons: Tycon.t -> {con: Con.t,
					 hasArg: bool} vector} =
   let
      fun match (var: Var.t,
		 ty: Type.t,
		 rules: Rule.t vector,
		 finish: Finish.t): Exp.t =
	 let
	    val rules =
	       Vector.map
	       (rules, fn Rule.T {pat, info = Info.T {accum, continue}} =>
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
	     NONE => finish (wild, Vector.map (rules, FlatRule.info))
	   | SOME (FlatRule.T {pat, ...}) =>
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
		       pats: Layout.t list,
		       rules: {pats: FlatPat.t vector option,
			       info: Info.t} vector,
		       finish: Finish.t): Exp.t =
	 if i = Vector.length vars
	    then finish (Layout.tuple (rev pats), Vector.map (rules, #info))
	 else
	    let
	       val (var, ty) = Vector.sub (vars, i)
	       val rules =
		  Vector.map
		  (rules, fn {pats, info = Info.T {accum, continue}} =>
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
	      _,
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
      and sum (test, _: Type.t, rules: FlatRule.t vector, finish: Finish.t) =
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
		  then SOME (finish (wild, defaults), region)
	       else
		  let
		     val {con, ...} = Vector.sub (cases, 0)
		     val tycon = conTycon con
		     fun done (defaultPat: Layout.t) =
			SOME (finish (defaultPat, defaults), region)
		  in
		     if Tycon.equals (tycon, Tycon.exn)
			then done (Layout.str  "e")
		     else
			let
			   val cons = tyconCons tycon
			in
			   if Vector.length cases = Vector.length cons
			      then NONE
			   else
			      let
				 val unhandled =
				    Vector.keepAllMap
				    (cons, fn {con, hasArg, ...} =>
				     if Vector.exists
					(cases, fn {con = con', ...} =>
					 Con.equals (con, con'))
					then NONE
				     else SOME (if hasArg
						   then conApp (con, wild)
						else Con.layout con))
				 open Layout
			      in
				 done (seq (separate (Vector.toList unhandled,
						      " | ")))
			      end
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
			       val (arg, rhs) =
				  case arg of
				     NoArg infos =>
					(NONE,
					 finish (Con.layout con,
						 Vector.fromList infos))
				   | Arg {var, ty, rules} =>
					(SOME (var, ty),
					 match (var, ty,
						Vector.fromList rules,
						fn (p, e) =>
						finish (conApp (con, p), e)))
			    in
			       {con = con,
				targs = tys,
				arg = arg,
				rhs = rhs}
			    end))}
	 in
	    if 1 = Vector.length cases
	       then
		  let
		     val {arg, con, ...} = Vector.sub (cases, 0)
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
						finish (conApp (con, p), e))}
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
			then wild
		     else
			let
			   val c = unhandledConst cs
			in
			   if isChar
			      then (case c of
				       Const.Word w =>
					  let
					     open Layout
					  in
					     seq [str "#\"",
						  Char.layout (WordX.toChar w),
						  str "\""]
					  end
				     | _ => Error.bug "strange char")
			   else Const.layout c
			end
	       in
		  finish (unhandled, Vector.fromList defaults)
	       end
	 in
	    case List.peek (directCases, fn (ty', _, _) =>
			    Type.equals (ty, ty')) of
	       NONE =>
		  List.fold
		  (cases, default (), fn ({const, infos}, rest) =>
		   Exp.iff {test = Exp.equal (test, Exp.const const),
			    thenn = finish (Const.layout const,
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
      val examples = Vector.tabulate (Vector.length cases, fn _ => ref [])
      val res =
	 match (test, testType,
		Vector.map2 (cases, examples, fn ((p, f), r) =>
			     Rule.T {pat = p,
				     info = Info.T {accum = Env.empty,
						    continue = Finish (r, f)}}),
		fn (pat, infos) =>
		if Vector.isEmpty infos
		   then Error.bug "matchRules: no default"
		else
		   let
		      val Info.T {accum = env, continue} = Vector.sub (infos, 0)
		   in
		      case continue of
			 Finish (r, f) =>
			    (List.push (r, pat)
			     ; f (fn x => Env.lookup (env, x)))
		       | _ => Error.bug "matchRules: expecting Finish"
		   end)
   in
      (res,
       fn () =>
       Vector.map (examples, fn r => Layout.seq (Layout.separate (! r, " | "))))
   end

val matchCompile =
   Trace.trace
   ("matchCompile",
    fn {cases, ...} => Vector.layout (NestedPat.layout o #1) cases,
    Exp.layout o #1)
   matchCompile
   
end
