(* Heavily modified from the SML/NJ sources by sweeks@research.nj.nec.com. *)

(* Copyright 1996 by AT&T Bell Laboratories *)
(* precedence.sml *)
 
functor PrecedenceParse (S: PRECEDENCE_PARSE_STRUCTS): PRECEDENCE_PARSE =
struct

open S
   
local open Ast
in structure Exp = Exp
   structure Fixity = Fixity
   structure Fixop = Fixop
   structure Longvid = Longvid
   structure Pat = Pat
   structure Vid = Vid
end
   
structure Fixval =
   struct
      datatype t = Nonfix | Infix of int * int

      fun eval (f: Fixity.t): t =
	 case f of
	    Fixity.Infix NONE => Infix (0, 1)
	  | Fixity.Infix (SOME n) => Infix (n+n, n+n+1)
	  | Fixity.Infixr NONE => Infix (1, 0)
	  | Fixity.Infixr (SOME n) => Infix (n+n+1, n+n)
	  | Fixity.Nonfix => Nonfix

      fun make ({name: Longvid.t, fixop: Fixop.t}, E: Env.t): t =
	 case (fixop, Longvid.split name) of
	    (Fixop.None, ([], vid)) =>
	       (case Env.peekFix (E, vid) of
		   NONE => Nonfix
		 | SOME f => eval f)
	  | _ => Nonfix

      fun makePat (p: Pat.t, E: Env.t): t =
	 case Pat.node p of
	    Pat.Var r => make (r, E)
	  | _ => Nonfix

      fun makeExp (e: Exp.t, E: Env.t): t =
	 case Exp.node e of
	    Exp.Var r => make (r, E)
	  | _ => Nonfix
   end

(*---------------------------------------------------*)
(*           from elaborate/precedence.sml           *)
(*---------------------------------------------------*)

datatype 'a precStack =
   INf of int * 'a * 'a precStack
 | NONf of 'a * 'a precStack
 | NILf
   
fun 'a parse {name: string,
	      region: 'a -> Region.t,
	      items: 'a vector,
	      fixval: 'a -> Fixval.t,
	      apply: 'a * 'a -> 'a,
	      tuple: 'a vector -> 'a}: 'a =
   let
      fun error msg =
	 Control.errorStr (Region.list (Vector.toList items, region), msg)
      fun ensureNONf ((e, f), p) =
	 let
	    val _ =
	       case f of
		  Fixval.Nonfix => ()
		| _ =>
		     (Control.errorStr
		      (region e,
		       concat [name, " begins with infix identifier"])
		      ; raise Fail "bug") (* FIXME *)
	 in
	    NONf (e, p)
	 end
      fun start token = ensureNONf (token, NILf)
      (* parse an expression *)
      fun parse (stack: 'a precStack, (item: 'a, fixval: Fixval.t)) =
	 case (stack, (item, fixval)) of
	    (NONf (e, r), (e', Fixval.Nonfix)) => NONf (apply (e, e'), r)
	  | (p as INf _, token) => ensureNONf (token, p)
	  | (p as NONf (e1, INf (bp, e2, NONf (e3, r))),
	     (e4, f as Fixval.Infix (lbp, rbp))) =>
	    if lbp > bp then INf (rbp, e4, p)
	    else (if lbp = bp
		     then error "mixed left- and right-associative \
		      \operators of same precedence"
		  else ();
		  parse (NONf (apply (e2, tuple (Vector.new2 (e3, e1))),
			       r),
			 (e4, f)))
	   | (p as NONf _, (e', Fixval.Infix (_, rbp))) => INf (rbp, e', p)
	   | _ => Error.bug "Precedence.parse"
      (* clean up the stack *)
      fun finish stack =
	 case stack of
	    NONf (e1, INf (_, e2, NONf (e3, r))) =>
	       finish (NONf (apply (e2, tuple (Vector.new2 (e3, e1))),
			     r))
	  | NONf (e1, NILf) => e1
	  | INf (_, e1, NONf (e2, p)) =>
	       (error (concat [name, " ends with infix identifier"]) ;
		finish (NONf (apply (e2, e1), p)))
	  | NILf => Error.bug "Corelang.finish NILf"
	  | _ => Error.bug "Corelang.finish"
      fun getfix x = (x, fixval x)
   in
      if Vector.isEmpty items
	 then Error.bug "parse"
      else
	 let
	    val item = Vector.sub (items, 0)
	 in
	    finish (Vector.foldFrom
		    (items, 1, start (getfix item), fn (item, state) =>
		     parse (state, getfix item)))
	 end
   end

fun parsePat (ps, E) =
   let
      fun apply (p1, p2) =
	 case Pat.node p1 of
	    Pat.Var {name, ...} =>
	       Pat.makeRegion (Pat.App (Longvid.toLongcon name, p2),
			       Region.append (Pat.region p1,
					      Pat.region p2))
	  | _ =>
	       Error.bug "PrecedenceParse: parsePat: apply got non-constructor"
   in
      parse {name = "pattern",
	     region = Pat.region,
	     items = ps,
	     fixval = fn p => Fixval.makePat (p, E),
	     tuple = Pat.tuple,
	     apply = apply}
   end

val parsePat =
   Trace.trace ("parsePat",
		fn (ps, _) => Vector.layout Pat.layout ps,
		Ast.Pat.layout)
   parsePat

fun parseExp (es, E) =
   parse {name = "expression",
	  region = Exp.region,
	  items = es,
	  fixval = fn e => Fixval.makeExp (e, E),
	  apply = Exp.app,
	  tuple = Exp.tuple}

val parseExp =
   Trace.trace ("parseExp",
		fn (es, _) => Vector.layout Exp.layout es,
		Ast.Exp.layout)
   parseExp

(*---------------------------------------------------*)
(*                    parseClause                    *)
(*---------------------------------------------------*)

fun parseClause (pats: Pat.t vector, E: Env.t) =
   let
      val pats = Vector.toList pats
      fun empty () = Error.bug "parseClause: empty clause"
      fun error msg =
	 (Control.errorStr (Region.list (pats, Pat.region), msg)
	  ; {func = Ast.Var.bogus,
	     args = Vector.new0 ()})
      fun illegal () = error "illegal function symbol in clause"
      fun done (func: Pat.t, args: Pat.t list) =
	 case Pat.node func of
	    Pat.Var {name, ...} =>
	       (case Longvid.split name of
		   ([], x) => {func = Vid.toVar x,
			       args = Vector.fromList args}
		 | _ => illegal ())
	  | _ => illegal ()
      val tuple = Pat.tuple o Vector.new2
      fun parse (ps : Pat.t list) =
	 case ps of
	    p :: rest =>
	       let
		  fun continue () =
		     case rest of
			[] => error "can't find function arguments in clause"
		      | _ => done (p, rest)
	       in
		  case Pat.node p of
		     Pat.FlatApp ps =>
			if 3 = Vector.length ps
			   then
			      let
				 fun p i = Vector.sub (ps, i)
			      in done (p 1, tuple (p 0, p 2) :: rest)
			      end
			else continue ()
		   | _ => continue ()
	       end
	  | _ => Error.bug "empty clause"
   in
      case pats of
	 [a, b, c] => (case Fixval.makePat (b, E) of
			  Fixval.Nonfix => parse pats
			| _ => done (b, [tuple (a, c)]))
       | _ => parse pats
   end
    
end
