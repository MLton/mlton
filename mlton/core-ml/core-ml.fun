(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor CoreML (S: CORE_ML_STRUCTS): CORE_ML = 
struct

open S

local open Ast
in
   structure Adec = Dec
   structure Apat = Pat
end

structure Wrap = Region.Wrap

fun makeList (con, app, tuple) (l, r) =
   List.foldr (l, Wrap.makeRegion (con Con.nill, r), fn (x1, x2) =>
	       Wrap.makeRegion (app (Con.cons, tuple (Vector.new2 (x1, x2), r)),
				r))

structure Pat =
   struct
      open Wrap
	 
      datatype node =
	 Wild
       | Var of Var.t
       | Const of Ast.Const.t
       | Con of {con: Con.t, arg: t option}
       | Record of {flexible: bool, record: t Record.t}
       | Constraint of t * Type.t
       | Layered of Var.t * t
      withtype t = node Wrap.t
      type node' = node
      type obj = t

      local
	 structure Pat = Ast.Pat
      in
	 fun toAst p =
	    case node p of
	       Wild => Pat.wild
	     | Var x => Pat.var (Var.toAst x)
	     | Const c => Pat.const c
	     | Record {record, flexible} =>
		  (case (flexible, Record.detupleOpt record) of
		      (false, SOME ps) => Pat.tuple (Vector.map (ps, toAst))
		    | _ =>
			 Pat.makeRegion
			 (Pat.Record
			  {flexible = flexible,
			   items = (Vector.map
				    (Record.toVector record, fn (field, p) =>
				     Pat.Item.Field (field, toAst p)))},
			  Region.bogus))
	     | Con {con, arg} =>
		  let
		     val con = Con.toAst con
		  in
		     case arg of
			NONE => Pat.con con
		      | SOME p => Pat.app (con, toAst p)
		  end
	     | Constraint (p, t) => Pat.constraint (toAst p,  Type.toAst t)
	     | Layered (x, p) =>
		  Pat.layered {fixop = Ast.Fixop.None,
			       var = Var.toAst x,
			       constraint = NONE,
			       pat = toAst p}

	 val layout = Pat.layout o toAst
      end

      fun isWild p =
	 case node p of
	    Wild => true
	  | _ => false
	 
      fun isRefutable p =
	 case node p of
	    Wild => false
	  | Var _ => false
	  | Const _ => true
	  | Con _ => true
	  | Record {record, ...} => Record.exists (record, isRefutable)
	  | Constraint (p, _) => isRefutable p
	  | Layered (_, p) => isRefutable p

      fun vars'(p, l) =
	 case node p of
	    Wild => l
	  | Var x => x :: l
	  | Const _ => l
	  | Con {arg, ...} => (case arg of
				  NONE => l
				| SOME p => vars'(p, l))
	  | Record {record, ...} => Record.fold (record, l, vars')
	  | Constraint (p, _) => vars'(p, l)
	  | Layered (x, p) => vars'(p, x :: l)

      fun vars p = vars'(p, [])

      fun removeVarsPred (p: t, pred: Var.t -> bool): t =
	 let
	    fun loop p =
	       let
		  fun doit n = makeRegion (n, region p)
	       in
		  case node p of
		     Wild => p
		   | Const _ => p
		   | Var x => if pred x
				 then doit Wild
			      else p
		   | Record {flexible, record} =>
			doit (Record {flexible = flexible,
				      record = Record.map (record, loop)})
		   | Con {con, arg} =>
			doit (Con {con = con,
				   arg = (case arg of
					     NONE => NONE
					   | SOME p => SOME (loop p))})
		   | Constraint (p, t) => doit (Constraint (loop p, t))
		   | Layered (_, p) => loop p
	       end
	 in loop p
	 end

      fun removeVars p = removeVarsPred (p, fn _ => true)

      fun removeOthersReplace (p, x, y) =
	 let
	    fun loop p =
	       let
		  fun doit n = makeRegion (n, region p)
	       in
		  case node p of
		     Wild => doit Wild
		   | Const _ => p
		   | Var x' =>
			doit (if Var.equals (x, x') then Var y else Wild)
		   | Record {record, flexible} =>
			doit (Record {flexible = flexible,
				      record = Record.map (record, loop)})
		   | Con {con, arg} =>
			doit (Con {con = con,
				   arg = (case arg of
					     NONE => NONE
					   | SOME p => SOME (loop p))})
		   | Constraint (p, _) => loop p
		   | Layered (x', p) =>
			if Var.equals (x, x')
			   then doit (Var y)
			else loop p
	       end
	 in
	    loop p
	 end

      val removeOthersReplace =
	 Trace.trace3 ("Pat.removeOthersReplace",
		       layout, Var.layout, Var.layout, layout)
	 removeOthersReplace

      fun tuple (ps, region)  =
	 if 1 = Vector.length ps
	    then Vector.sub (ps, 0)
	 else makeRegion (Record {flexible = false, record = Record.tuple ps},
			  region)

      fun unit r = tuple (Vector.new0 (), r)
	 
      val list =
	 makeList (fn c => Con {con = c, arg = NONE},
		   fn (c, p) => Con {con = c, arg = SOME p},
		   tuple)

      fun var (x, r) = makeRegion (Var x, r)
	 
      fun record {flexible, record, region} =
	 makeRegion (Record {flexible = flexible, record = record},
		     region)

      local
	 fun make c r = makeRegion (Con {con = c, arg = NONE}, r)
      in
	 val truee = make Con.truee
	 val falsee = make Con.falsee
      end

      fun foreachVar (p, f) =
	 let
	    fun loop p =
	       case node p of
		  Var x => f x
		| Con {arg = SOME p, ...} => loop p
		| Record {record, ...} => Record.foreach (record, loop)
		| Constraint (p, _) => loop p
		| Layered (x, p) => (f x; loop p)
		| _ => ()
	 in loop p
	 end
   end

datatype dec =
   Val of {exp: exp,
	   filePos: string,
	   pat: Pat.t,
	   tyvars: Tyvar.t vector}
  | Fun of {tyvars: Tyvar.t vector,
	    decs: {var: Var.t,
		   types: Type.t vector,
		   match: match} vector}
  | Datatype of {
		 tyvars: Tyvar.t vector,
		 tycon: Tycon.t,
		 cons: {
			con: Con.t,
			arg: Type.t option
			} vector
		 } vector
  | Exception of {
		  con: Con.t,
		  arg: Type.t option
		  }
  | Overload of {var: Var.t,
		 scheme: Scheme.t,
		 ovlds: Var.t vector}
and expNode =
   Var of Var.t
  | Prim of Prim.t
  | Const of Ast.Const.t
  | Con of Con.t
  | Record of exp Record.t
  | Fn of match
  | App of exp * exp
  | Let of dec vector * exp
  | Constraint of exp * Type.t
  | Handle of exp * match
  | Raise of {exn: exp, filePos: string}
and match = Match of {filePos: string,
		      rules: (Pat.t * exp) vector}
withtype exp = expNode Wrap.t

structure Match =
   struct
      type t = match

      local
	 fun make f m =
	    let
	       val Match r = m
	    in
	       f r
	    end
      in
	 val filePos = make #filePos
	 val rules = make #rules
      end

      fun region m =
	 Wrap.region (#1 (Vector.sub (rules m, 0)))
		     

      fun new {filePos, rules} =
	 Match {filePos = filePos,
		rules = rules}
   end

local
   local
      open Ast
   in
      structure Dec = Dec
      structure Exp = Exp
      structure Longvar = Longvar
   end
in
   fun astDatatype ds =
      Dec.datatypee
      (Vector.map
       (ds, fn {tyvars, tycon, cons} =>
	{tyvars = tyvars,
	 tycon = Tycon.toAst tycon,
	 cons = Vector.map (cons, fn {con, arg} =>
			    (Con.toAst con, Type.optionToAst arg))}))
      
   fun decToAst d =
      let
	 fun doit n = Dec.makeRegion (n, Region.bogus)
      in
	 case d of
	    Val {pat, filePos, tyvars, exp} =>
	       doit (Dec.Val {tyvars = tyvars,
			      vbs = Vector.new1 {pat = Pat.toAst pat,
						 exp = expToAst exp,
						 filePos = filePos},
			      rvbs = Vector.new0 ()})
	  | Fun {tyvars, decs} =>
	       doit (Dec.Val
		     {tyvars = tyvars,
		      vbs = Vector.new0 (),
		      rvbs = (Vector.map
			      (decs, fn {var, types, match} =>
			       {pat = (Vector.fold
				       (types, Apat.var (Var.toAst var),
					fn (t, p) =>
					Apat.constraint (p, Type.toAst t))),
				match = matchToAst match}))})
	  | Datatype ds => astDatatype ds
	  | Exception {con, arg} =>
	       Dec.exceptionn (Con.toAst con, Type.optionToAst arg)
	  | Overload {var, scheme, ovlds} =>
	       doit (Dec.Overload
		     (Var.toAst var,
		      Type.toAst (Scheme.ty scheme),
		      Vector.map (ovlds, fn x =>
				  Longvar.short (Var.toAst x))))
      end
   and expToAst e =
      case Wrap.node e of
	 Var x => Exp.var (Var.toAst x)
       | Prim p => Exp.longvid (Ast.Longvid.short
				(Ast.Longvid.Id.fromString (Prim.toString p)))
       | Const c => Exp.const c
       | Con c => Exp.con (Con.toAst c)
       | Record r => Exp.record (Record.map (r, expToAst))
       | Fn m => Exp.fnn (matchToAst m)
       | App (e1, e2) => Exp.app (expToAst e1, expToAst e2)
       | Let (ds, e) => Exp.lett (Vector.map (ds, decToAst), expToAst e)
       | Constraint (e, t) => Exp.constraint (expToAst e, Type.toAst t)
       | Handle (try, match) =>
	    Exp.handlee (expToAst try, matchToAst match)
       | Raise {exn, filePos} => Exp.raisee {exn = expToAst exn,
					     filePos = filePos}

   and matchToAst m =
      let
	 val Match {rules, filePos} = m
      in
	 Ast.Match.T
	 {filePos = filePos,
	  rules = Vector.map (rules, fn (p, e) => (Pat.toAst p, expToAst e))}
      end
end

fun makeForeachVar f =
   let
      fun exp e =
	 case Wrap.node e of
	    Var x => f x
	  | Record r => Record.foreach (r, exp)
	  | Fn m => match m
	  | App (e1, e2) => (exp e1; exp e2)
	  | Let (ds, e) => (Vector.foreach (ds, dec); exp e)
	  | Constraint (e, _) => exp e
	  | Handle (e, m) => (exp e; match m)
	  | Raise {exn, ...} => exp exn
	  | _ => ()
      and match m = Vector.foreach (Match.rules m, exp o #2)
      and dec d =
	 case d of
	    Val {exp = e, ...} => exp e
	  | Fun {decs, ...} => Vector.foreach (decs, match o #match)
	  | Overload {ovlds, ...} => Vector.foreach (ovlds, f)
	  | _ => ()
   in
      {exp = exp, dec = dec}
   end

structure Exp =
   struct
      open Wrap
      type dec = dec
      type match = match
      datatype node = datatype expNode
      type t = exp
      type node' = node
      type obj = t

      val toAst = expToAst

      fun foreachVar (e, f) = #exp (makeForeachVar f) e

      fun fnn (m, r) = makeRegion (Fn m, r)

      fun fn1 (p, e, r) =
	 fnn (Match.new {filePos = "",
			 rules = Vector.new1 (p, e)},
	      r)

      fun isExpansive e =
	 case node e of
	    Var _ => false
	  | Const _ => false
	  | Con _ => false
	  | Fn _ => false
	  | Prim _ => false
	  | Record r => Record.exists (r, isExpansive)
	  | Constraint (e, _) => isExpansive e
	  | App (e1, e2) =>
	       (case node e1 of
		   Con c => Con.equals (c, Con.reff) orelse isExpansive e2
		 | _ => true)
	  | _ => true

      fun record (record, r) = makeRegion (Record record, r)
	 
      fun lambda (x, e, r) = fn1 (makeRegion (Pat.Var x, r), e, r)

      fun delay (e, r) = fn1 (Pat.unit r, e, r)

      fun casee (test, rules, r) =
	 makeRegion (App (makeRegion (Fn rules, r),
			  test),
		     r)

      fun tuple (es, r) =
	 if 1 = Vector.length es
	    then Vector.sub (es, 0)
	 else record (Record.tuple es, r)

      fun unit r = tuple (Vector.new0 (), r)

      fun seq (es, r) =
	 if 1 = Vector.length es
	    then Vector.sub (es, 0)
	 else
	    let
	       val (es, e) = Vector.splitLast es
	    in
	       makeRegion
	       (Let (Vector.map (es, fn e =>
				 Val {pat = makeRegion (Pat.Wild, r),
				      tyvars = Vector.new0 (),
				      exp = e,
				      filePos = ""}),
		     e),
		r)
	    end

      fun force (e, r) = makeRegion (App (e, unit r), r)
	 
      fun list (l, r) =
	 makeList (Con, fn (c, e) => App (makeRegion (Con c, r), e), tuple)
	 (l, r)

      fun var (x, r) = makeRegion (Var x, r)
	 
      fun selector (f, r) =
	 let
	    val x = Var.newNoname ()
	 in
	    fn1 (Pat.record {flexible = true,
			     record = Record.fromVector (Vector.new1
							 (f, Pat.var (x, r))),
			     region = r},
		 var (x, r),
		 r)
	 end

      fun iff (test, thenCase, elseCase, r) =
	 casee (test,
		Match.new {filePos = "",
			   rules = Vector.new2 ((Pat.truee r, thenCase),
						(Pat.falsee r, elseCase))},
		r)

      fun con (c, r) = makeRegion (Con c, r)
      fun truee r = con (Con.truee, r)
      fun falsee r = con (Con.falsee, r)
	 
      fun andAlso (e1, e2, r) = iff (e1, e2, falsee r, r)
      fun orElse (e1, e2, r) = iff (e1, truee r, e2, r)

      fun whilee {test, expr, region = r} =
	 let
	    val loop = Var.newNoname ()
	    val call = makeRegion (App (var (loop, r), unit r), r)
	 in
	    makeRegion
	    (Let (Vector.new1
		  (Fun {tyvars = Vector.new0 (),
			decs = (Vector.new1
				{var = loop,
				 types = Vector.new0 (),
				 match = (Match.new
					  {filePos = "",
					   rules =
					   Vector.new1
					   (Pat.tuple (Vector.new0 (), r),
					    iff (test,
						 seq (Vector.new2 (expr, call),
						      r),
						 unit r,
						 r))})})}),
		  call),
	     r)
	 end

      val layout = Ast.Exp.layout o toAst
   end

structure Dec =
   struct
      datatype t = datatype dec

      val isExpansive =
	 fn Val {exp, ...} => Exp.isExpansive exp
	  | _ => false

      val toAst = decToAst

      val layout = Adec.layout o toAst
   end

structure Program =
   struct
      datatype t = T of {decs: Dec.t vector}

      fun toAst (T {decs, ...}) =
	 Adec.makeRegion
	 (Adec.Local
	  (Adec.makeRegion (Adec.SeqDec (Vector.map (decs, Dec.toAst)),
			    Region.bogus),
	   Adec.empty),
	  Region.bogus)

      val layout = Adec.layout o toAst

      fun size (T {decs = ds, ...}): int =
	 let
	    val n = ref 0
	    fun inc () = n := 1 + !n
	    fun exp e =
	       (inc ()
		; (case Exp.node e of
		      Fn m => match m
		    | Record r => Record.foreach (r, exp)
		    | App (e, e') => (exp e; exp e')
		    | Let (ds, e) => (Vector.foreach (ds, dec); exp e)
		    | Constraint (e, _) => exp e
		    | Handle (e, m) => (exp e; match m)
		    | Raise {exn, ...} => exp exn
		    | _ => ()))
	    and match m = Vector.foreach (Match.rules m, exp o #2)
	    and dec d =
	       case d of
		  Val {exp = e, ...} => exp e
		| Fun {decs, ...} => Vector.foreach (decs, match o #match)
		| Exception _ => inc ()
		| _ => ()
	    val _ = Vector.foreach (ds, dec)
	 in
	    !n
	 end
      
      fun layoutStats p =
	 let open Layout
	 in seq [str "size = ", Int.layout (size p)]
	 end
   end

end
