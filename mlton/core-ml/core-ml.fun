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

fun makeList (con, app, tuple) l =
   List.foldr (l, con Con.nill, fn (x1, x2) =>
	       app (Con.cons, tuple (Vector.new2 (x1, x2))))

structure Pat =
   struct
      datatype t =
	 Wild
       | Var of Var.t
       | Const of Ast.Const.t
       | Con of {con: Con.t, arg: t option}
       | Record of {flexible: bool, record: t Record.t}
       | Constraint of t * Type.t
       | Layered of Var.t * t

      local
	 structure Pat = Ast.Pat
      in
	 fun toAst p =
	    case p of
	       Wild => Pat.wild
	     | Var x => Pat.var (Var.toAst x)
	     | Const c => Pat.const c
	     | Record {record, flexible} =>
		  (case (flexible, Record.detupleOpt record) of
		      (false, SOME ps) => Pat.tuple (Vector.map (ps, toAst))
		    | _ =>
			 Pat.make
			 (Pat.Record
			  {flexible = flexible,
			   items = (Vector.map
				    (Record.toVector record, fn (field, p) =>
				     Pat.Item.Field (field, toAst p)))}))
	     | Con {con, arg} =>
		  let val con = Con.toAst con
		  in case arg of
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
   
      fun isRefutable p =
	 case p of
	    Wild => false
	  | Var _ => false
	  | Const _ => true
	  | Con _ => true
	  | Record {record, ...} => Record.exists (record, isRefutable)
	  | Constraint (p, _) => isRefutable p
	  | Layered (_, p) => isRefutable p

      fun vars'(p, l) =
	 case p of
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
	       case p of
		  Wild => Wild
		| Const _ => p
		| Var x => if pred x then Wild else p
		| Record {flexible, record} =>
		     Record {flexible = flexible,
			     record = Record.map (record, loop)}
		| Con {con, arg} => Con {con = con,
					 arg = (case arg of
						   NONE => NONE
						 | SOME p => SOME (loop p))}
		| Constraint (p, t) => Constraint (loop p, t)
		| Layered (_, p) => loop p
	 in loop p
	 end

      fun removeVars p = removeVarsPred (p, fn _ => true)

      fun removeOthersReplace (p, x, y) =
	 let
	    fun loop p =
	       case p of
		  Wild => Wild
		| Const _ => p
		| Var x' => if Var.equals (x, x') then Var y else Wild
		| Record {record, flexible} =>
		     Record {flexible = flexible,
			     record = Record.map (record, loop)}
		| Con {con, arg} => Con {con = con,
					 arg = (case arg of
						   NONE => NONE
						 | SOME p => SOME (loop p))}
		| Constraint (p, t) => loop p (*Constraint (loop p, t)*)
		| Layered (x', p) =>
		     if Var.equals (x, x') then Var y else loop p
	 in loop p
	 end

      val removeOthersReplace =
	 Trace.trace3 ("Pat.removeOthersReplace",
		       layout, Var.layout, Var.layout, layout)
	 removeOthersReplace

      fun tuple ps =
	 if 1 = Vector.length ps
	    then Vector.sub (ps, 0)
	 else Record {flexible = false, record = Record.tuple ps}

      val unit = tuple (Vector.new0 ())
	 
      val list = makeList (fn c => Con {con = c, arg = NONE},
			   fn (c, p) => Con {con = c, arg = SOME p},
			   tuple)

      val record = Record

      local
	 fun make c = Con {con = c, arg = NONE}
      in
	 val truee = make Con.truee
	 val falsee = make Con.falsee
      end

      fun foreachVar (p, f) =
	 let
	    fun loop p =
	       case p of
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
and exp =
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

and match = T of {filePos: string,
		  rules: (Pat.t * exp) vector}

structure Match =
   struct
      datatype t = datatype match

      local
	 fun make f (T r) = f r
      in
	 val filePos = make #filePos
      end
   
      fun new rs = T {rules = rs,
		      filePos = ""}
   end

local
   local open Ast
   in structure Dec = Dec
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
      case d of
	 Val {pat, filePos, tyvars, exp} =>
	    Dec.make (Dec.Val {tyvars = tyvars,
			       vbs = Vector.new1 {pat = Pat.toAst pat,
						  exp = expToAst exp,
						  filePos = filePos},
			       rvbs = Vector.new0 ()})
       | Fun {tyvars, decs} =>
	    Dec.make (Dec.Val
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
	    Dec.make (Dec.Overload
		      (Var.toAst var,
		       Type.toAst (Scheme.ty scheme),
		       Vector.map (ovlds, fn x =>
				   Longvar.short (Var.toAst x))))

   and expToAst e =
      case e of
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
	    
   and matchToAst (Match.T {rules, filePos}) =
      Ast.Match.T
      {filePos = filePos,
       rules = Vector.map (rules, fn (p, e) => (Pat.toAst p, expToAst e))}
end

fun makeForeachVar f =
   let
      fun exp e =
	 case e of
	    Var x => f x
	  | Record r => Record.foreach (r, exp)
	  | Fn m => match m
	  | App (e1, e2) => (exp e1; exp e2)
	  | Let (ds, e) => (Vector.foreach (ds, dec); exp e)
	  | Constraint (e, _) => exp e
	  | Handle (e, m) => (exp e; match m)
	  | Raise {exn, ...} => exp exn
	  | _ => ()
      and match (Match.T {rules, ...}) = Vector.foreach (rules, exp o #2)
      and dec d =
	 case d of
	    Val {exp = e, ...} => exp e
	  | Fun {decs, ...} => Vector.foreach (decs, match o #match)
	  | Overload {ovlds, ...} => Vector.foreach (ovlds, f)
	  | _ => ()
   in {exp = exp, dec = dec}
   end

structure Exp =
   struct
      type dec = dec
      type match = match
      datatype t = datatype exp

      val toAst = expToAst

      fun foreachVar (e, f) = #exp (makeForeachVar f) e

      val fnn = Fn

      fun fn1 r = fnn (Match.new (Vector.new1 r))

      fun compose () =
	 let val f = Var.newNoname ()
	    val g = Var.newNoname ()
	    val x = Var.newNoname ()
	 in fn1 (Pat.tuple (Vector.new2 (Pat.Var f, Pat.Var g)),
		 fn1 (Pat.Var x, App (Var f, App (Var g, Var x))))
	 end
      
      val rec isExpansive =
	 fn Var _ => false
	  | Const _ => false
	  | Con _ => false
	  | Fn _ => false
	  | Prim _ => false
	  | Record r => Record.exists (r, isExpansive)
	  | Constraint (e, _) => isExpansive e
	  | App (e1, e2) =>
	       (case e1 of
		   Con c => Con.equals (c, Con.reff) orelse isExpansive e2
		 | _ => true)
	  | _ => true

      val record = Record
	 
      fun lambda (x, e) = fn1 (Pat.Var x, e)

      fun delay e = fn1 (Pat.unit, e)

      fun casee (test, rules) = App (Fn rules, test)

      fun tuple es =
	 if 1 = Vector.length es
	    then Vector.sub (es, 0)
	 else record (Record.tuple es)

      val unit = tuple (Vector.new0 ())

      fun seq es =
	 if 1 = Vector.length es
	    then Vector.sub (es, 0)
	 else
	    let
	       val (es, e) = Vector.splitLast es
	    in
	       Let (Vector.map (es, fn e =>
				Val {pat = Pat.Wild,
				     tyvars = Vector.new0 (),
				     exp = e,
				     filePos = ""}),
		    e)
	    end

      fun force e = App (e, unit)
	 
      val list = makeList (Con, fn (c, e) => App (Con c, e), tuple)

      fun selector f =
	 let val x = Var.newNoname ()
	 in fn1 (Pat.Record {flexible = true,
			     record = Record.fromVector (Vector.new1
							 (f, Pat.Var x))},
		 Var x)
	 end

      fun iff (test, thenCase, elseCase) =
	 casee (test, Match.new (Vector.new2 ((Pat.truee, thenCase),
					      (Pat.falsee, elseCase))))

      val truee = Con Con.truee
      val falsee = Con Con.falsee
	 
      fun andAlso (e1, e2) = iff (e1, e2, falsee)
      fun orElse (e1, e2) = iff (e1, truee, e2)

      fun whilee {test, expr} =
	 let
	    val loop = Var.newNoname ()
	 in
	    Let (Vector.new1
		 (Fun {tyvars = Vector.new0 (),
		       decs = (Vector.new1
			       {var = loop,
				types = Vector.new0 (),
				match = (Match.new
					 (Vector.new1
					  (Pat.tuple (Vector.new0 ()),
					   iff (test,
						seq (Vector.new2
						     (expr,
						      App (Var loop, unit))),
						unit))))})}),
		 App (Var loop, unit))
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
      datatype t = T of {decs: Dec.t vector
(*			 constructors: Constructors.t *)}

      (*      val empty = T [] *)

      (*      fun append (T ds, T ds') = T (List.append (ds, ds')) *)

      fun toAst (T {decs, ...}) =
	 Adec.make
	 (Adec.Local (Adec.make (Adec.SeqDec (Vector.map (decs, Dec.toAst))),
		      Adec.empty))

      val layout = Adec.layout o toAst

      fun size (T {decs = ds, ...}): int =
	 let
	    val n = ref 0
	    fun inc () = n := 1 + !n
	    fun exp e =
	       (inc ()
		; (case e of
		      Fn m => match m
		    | Record r => Record.foreach (r, exp)
		    | App (e, e') => (exp e; exp e')
		    | Let (ds, e) => (Vector.foreach (ds, dec); exp e)
		    | Constraint (e, _) => exp e
		    | Handle (e, m) => (exp e; match m)
		    | Raise {exn, ...} => exp exn
		    | _ => ()))
	    and match (Match.T {rules, ...}) = Vector.foreach (rules, exp o #2)
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
