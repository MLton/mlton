(* Copyright (C) 1999-2003 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
(*
 * renameDec walks down the tree renaming all explicitly bound tyvars, and on the
 * way back up, tries to bind implicitly scoped tyvars at each possible point.
 *
 * removeDec walks down and binds a tyvar as soon as it sees it, removing all
 * lower binding occurrences of the tyvar. 
 * 
 * removeDec also renames all lower free occurrences of the tyvar to be the
 * "same" as the binding occurrence (so that they can share info).
 *)

functor Scope (S: SCOPE_STRUCTS): SCOPE =
struct

open S
open Ast

structure Tyvars = UnorderedSet (UseName (Tyvar))
structure Env =
   struct
      structure Env = MonoEnv (structure Domain = UseName (Tyvar)
			       structure Range = Tyvar)
      open Env

      (* rename (env, tyvars) extends env by mapping each tyvar to
       * a new tyvar (with the same equality property).  It returns
       * the extended environment and the list of new tyvars.
       *)
      fun rename (env: t, tyvars: Tyvar.t vector): t * Tyvar.t vector =
	 let
	    val (tyvars, env) =
	       Vector.mapAndFold
	       (tyvars, env, fn (tyv, env) =>
		let
		   val tyv' =
		      Tyvar.newNoname {equality = Tyvar.isEquality tyv}
		in (tyv', extend (env, tyv, tyv'))
		end)
	 in (env, tyvars)
	 end
   end

fun ('down, 'up)
   processDec (d: Dec.t,
	       {
		bind: 'down * Tyvar.t vector -> ('down
						 * Tyvar.t vector
						 * ('up -> 'up)),
		bind': 'down * Tyvar.t vector -> ('down
						  * ('up -> (Tyvar.t vector
							     * 'up))),
		combineUp: 'up * 'up -> 'up,
		initDown: 'down,
		initUp: 'up,
		tyvar: Tyvar.t * 'down -> Tyvar.t * 'up
		}): Dec.t * 'up =
   let
      fun loops (xs: 'a vector, loopX: 'a -> 'a * 'up): 'a vector * 'up =
	 Vector.mapAndFold (xs, initUp, fn (x, u) =>
			    let
			       val (x, u') = loopX x
			    in
			       (x, combineUp (u, u'))
			    end)
      fun loopTy (t: Type.t, d: 'down): Type.t * 'up =
	 let
	    fun loop (t: Type.t): Type.t * 'up =
	       let
		  datatype z = datatype Type.node
		  val (n, u) =
		     case Type.node t of
			Con (c, ts) =>
			   let
			      val (ts, u) = loops (ts, loop)
			   in
			      (Con (c, ts), u)
			   end
		      | Record r =>
			   let
			      val (r, u) = SortedRecord.change (r, fn ts =>
								loops (ts, loop))
			   in
			      (Record r, u)
			   end
		      | Var a =>
			   let
			      val (a, u) = tyvar (a, d)
			   in
			      (Var a, u)
			   end
	       in
		  (Type.makeRegion (n, Type.region t), u)
	       end
	 in
	    loop t
	 end
      fun loopTyOpt (to: Type.t option, d: 'down): Type.t option * 'up =
	 case to of
	    NONE => (NONE, initUp)
	  | SOME t =>
	       let
		  val (t, u) = loopTy (t, d)
	       in
		  (SOME t, u)
	       end
      fun loopTypBind (tb: TypBind.t, d: 'down): TypBind.t * 'up =
	 let
	    val TypBind.T tbs = TypBind.node tb
	    val (tbs, u) =
	       loops (Vector.fromList tbs, fn {def, tycon, tyvars} =>
		      let
			 val (d, tyvars, finish) = bind (d, tyvars)
			 val (def, u) = loopTy (def, d)
		      in
			 ({def = def,
			   tycon = tycon,
			   tyvars = tyvars},
			  finish u)
		      end)
	 in
	    (TypBind.makeRegion (TypBind.T (Vector.toList tbs),
				 TypBind.region tb),
	     u)
	 end
      fun loopDatBind (db: DatBind.t, d: 'down): DatBind.t * 'up =
	 let
	    val DatBind.T {datatypes, withtypes} = DatBind.node db
	    val (datatypes, u) =
	       loops
	       (datatypes, fn {cons, tycon, tyvars} =>
		let
		   val (d, tyvars, up) = bind (d, tyvars)
		   val (cons, u) =
		      loops (cons, fn (con, arg) =>
			     let
				val (arg, u) = loopTyOpt (arg, d)
			     in
				((con, arg), u)
			     end)
		in
		   ({cons = cons, tycon = tycon, tyvars = tyvars}, up u)
		end)
	    val (withtypes, u') = loopTypBind (withtypes, d)
	 in
	    (DatBind.makeRegion (DatBind.T {datatypes = datatypes,
					    withtypes = withtypes},
				 DatBind.region db),
	     combineUp (u, u'))
	 end
      fun loopPat (p: Pat.t, d: 'down): Pat.t * 'up =
	 let
	    fun loop (p: Pat.t): Pat.t * 'up =
	       let
		  fun doit n = Pat.makeRegion (n, Pat.region p)
		  fun do1 ((a, u), f) = (doit (f a), u)
		  fun do2 ((a1, u1), (a2, u2), f) =
		     (doit (f (a1, a2)), combineUp (u1, u2))
		  datatype z = datatype Pat.node
	       in
		  case Pat.node p of
		     App (c, p) => do1 (loop p, fn p => App (c, p))
		   | Const _ => (p, initUp)
		   | Constraint (p, t) =>
			do2 (loop p, loopTy (t, d), Constraint)
		   | FlatApp ps => do1 (loops (ps, loop), FlatApp)
		   | Layered {constraint, fixop, pat, var} =>
			do2 (loopTyOpt (constraint, d), loop pat,
			     fn (constraint, pat) =>
			     Layered {constraint = constraint,
				      fixop = fixop,
				      pat = pat,
				      var = var})
		   | List ps => do1 (loops (Vector.fromList ps, loop),
				     fn ps => List (Vector.toList ps))
		   | Record {flexible, items} =>
			let
			   val (items, u) =
			      Vector.mapAndFold
			      (items, initUp, fn (i, u) =>
			       let
				  datatype z = datatype Pat.Item.t
				  val (i, u') =
				     case i of
					Field (f, p) =>
					   let
					      val (p, u) = loop p
					   in
					      (Field (f, p), u)
					   end
				      | Vid (v, to, po) =>
					   let
					      val (to, u) = loopTyOpt (to, d)
					      val (po, u') = loopOpt po
					   in
					      (Vid (v, to, po),
					       combineUp (u, u'))
					   end
			       in
				  (i, combineUp (u, u'))
			       end)
			in
			   (doit (Record {items = items,
					  flexible = flexible}),
			    u)
			end
		   | Tuple ps => do1 (loops (ps, loop), Tuple)
		   | Var _ => (p, initUp)
		   | Wild => (p, initUp)

	       end
	    and loopOpt opt =
	       case opt of
		  NONE =>
		     (NONE, initUp)
		| SOME p =>
		     let
			val (p, u) = loop p
		     in
			(SOME p, u)
		     end
	 in
	    loop p
	 end
      fun loopDec (d: Dec.t, down: 'down): Dec.t * 'up =
	 let
	    fun doit n = Dec.makeRegion (n, Dec.region d)
	    fun do1 ((a, u), f) = (doit (f a), u)
	    fun do2 ((a1, u1), (a2, u2), f) =
	       (doit (f (a1, a2)), combineUp (u1, u2))
	    fun doVec (ds: Dec.t vector, f: Dec.t vector -> Dec.node)
	       : Dec.t * 'up =
	       let
		  val (ds, u) = loops (ds, fn d => loopDec (d, down))
	       in
		  (doit (f ds), u)
	       end
	    fun empty () = (d, initUp)
	    datatype z = datatype Dec.node
	 in
	    case Dec.node d of
	       Abstype {body, datBind} =>
		  let
		     val (body, u) = loopDec (body, down)
		     val (db, u') = loopDatBind (datBind, down)
		  in
		     (doit (Abstype {body = body, datBind = db}),
		      combineUp (u, u'))
		  end
	     | Datatype rhs =>
		  let
		     datatype z = datatype DatatypeRhs.node
		     val (rhs, u) =
			case DatatypeRhs.node rhs of
			   DatBind db =>
			      let
				 val (db, u) = loopDatBind (db, down)
			      in
				 (DatatypeRhs.makeRegion
				  (DatBind db, DatatypeRhs.region rhs),
				  u)
			      end
			 | Repl _ => (rhs, initUp)
		  in
		     (doit (Datatype rhs), u)
		  end
	     | Exception ebs =>
		  let
		     val (ebs, u) =
			loops (ebs, fn (c, rhs) =>
			       let
				  datatype z = datatype EbRhs.node
				  val (rhs, u) =
				     case EbRhs.node rhs of
					Def _ => (rhs, initUp)
				      | Gen to =>
					   let
					      val (to, u) = loopTyOpt (to, down)
					   in
					      (EbRhs.makeRegion
					       (Gen to, EbRhs.region rhs),
					       u)
					   end
			       in
				  ((c, rhs), u)
			       end)
		  in
		     (doit (Exception ebs), u)
		  end
	     | Fix _ => (d, initUp)
	     | Fun (tyvars, decs) =>
		  let
		     val (down, finish) = bind' (down, tyvars)
		     val (decs, u) =
			loops (decs, fn {clauses, filePos} =>
			       let
				  val (clauses, u) =
				     loops
				     (clauses, fn {body, pats, resultType} =>
				      let
					 val (body, u) = loopExp (body, down)
					 val (pats, u') =
					    loops (pats, fn p =>
						   loopPat (p, down))
					 val (resultType, u'') =
					    loopTyOpt (resultType, down)
				      in
					 ({body = body,
					   pats = pats,
					   resultType = resultType},
					  combineUp (u, combineUp (u', u'')))
				      end)
				 in
				    ({clauses = clauses,
				      filePos = filePos},
				     u)
				 end)
		     val (tyvars, u) = finish u
		  in
		     (doit (Fun (tyvars, decs)), u)
		  end
	     | Local (d, d') =>
		  do2 (loopDec (d, down), loopDec (d', down), Local)
	     | Open _ => empty ()
	     | Overload _ => empty ()
	     | SeqDec ds => doVec (ds, SeqDec)
	     | Type tb => do1 (loopTypBind (tb, down), Type)
	     | Val {rvbs, tyvars, vbs} =>
		  let
		     val (down, finish) = bind' (down, tyvars)
		     val (rvbs, u) =
			loops (rvbs, fn {match, pat} =>
			       let
				  val (match, u) = loopMatch (match, down)
				  val (pat, u') = loopPat (pat, down)
			       in
				  ({match = match,
				    pat = pat},
				   combineUp (u, u'))
			       end)
		     val (vbs, u') =
			loops (vbs, fn {exp, filePos, pat} =>
			       let
				  val (exp, u) = loopExp (exp, down)
				  val (pat, u') = loopPat (pat, down)
			       in
				  ({exp = exp,
				    filePos = filePos,
				    pat = pat},
				   combineUp (u, u'))
			       end)
		     val (tyvars, u) = finish (combineUp (u, u'))
		  in
		     (doit (Val {rvbs = rvbs,
				 tyvars = tyvars,
				 vbs = vbs}),
		      u)
		  end
	 end
      and loopDecs (ds, down) = loops (ds, fn d => loopDec (d, down))
      and loopExp (e: Exp.t, d: 'down): Exp.t * 'up =
	 let
	    val loopMatch = fn m => loopMatch (m, d)
	    fun loop (e: Exp.t): Exp.t * 'up =
	       let
		  fun empty () = (e, initUp)
		  val region = Exp.region e
		  fun doit n = Exp.makeRegion (n, region)
		  datatype z = datatype Exp.node
		  fun do1 ((a, u), f) = (doit (f a), u)
		  fun do2 ((a1, u1), (a2, u2), f) =
		     (doit (f (a1, a2)), combineUp (u1, u2))
		  fun do3 ((a1, u1), (a2, u2), (a3, u3), f) =
		     (doit (f (a1, a2, a3)), combineUp (u1, combineUp (u2, u3)))
		  fun doVec (es: Exp.t vector, f: Exp.t vector -> Exp.node)
		     : Exp.t * 'up =
		     let
			val (es, u) = loops (es, loop)
		     in
			(doit (f es), u)
		     end
		  fun doList (es: Exp.t list, f: Exp.t list -> Exp.node)
		     : Exp.t * 'up =
		     let
			val (es, u) = loops (Vector.fromList es, loop)
		     in
			(doit (f (Vector.toList es)), u)
		     end
	       in
		  case Exp.node e of
		     Andalso (e1, e2) => do2 (loop e1, loop e2, Andalso)
		   | App (e1, e2) => do2 (loop e1, loop e2, App)
		   | Case (e, m) => do2 (loop e, loopMatch m, Case)
		   | Const _ => empty ()
		   | Constraint (e, t) => do2 (loop e, loopTy (t, d), Constraint)
		   | FlatApp es => doVec (es, FlatApp)
		   | Fn m => do1 (loopMatch m, Fn)
		   | Handle (e, m) => do2 (loop e, loopMatch m, Handle)
		   | If (e1, e2, e3) => do3 (loop e1, loop e2, loop e3, If)
		   | Let (dec, e) => do2 (loopDec (dec, d), loop e, Let)
		   | List ts => doList (ts, List)
		   | Orelse (e1, e2) => do2 (loop e1, loop e2, Orelse)
		   | Prim {kind, name, ty} =>
			do1 (loopTy (ty, d), fn ty =>
			     Prim {kind = kind,
				   name = name,
				   ty = ty})
		   | Raise {exn, filePos} =>
			do1 (loop exn,
			     fn exn => Raise {exn = exn, filePos = filePos})
		   | Record r =>
			let
			   val (r, u) = Record.change (r, fn es =>
						       loops (es, loop))
			in
			   (doit (Record r), u)
			end
		   | Selector _ => empty ()
		   | Seq es => doVec (es, Seq)
		   | Var _ => empty ()
		   | While {expr, test} =>
			do2 (loop expr, loop test, fn (expr, test) =>
			     While {expr = expr, test = test})
	       end
	 in
	    loop e
	 end
      and loopMatch (Match.T {filePos, rules}, d) =
	 let
	    val (rules, u) =
	       loops (rules, fn (p, e) =>
		      let
			 val (p, u) = loopPat (p, d)
			 val (e, u') = loopExp (e, d)
		      in
			 ((p, e), combineUp (u, u'))
		      end)
	 in
	    (Match.T {filePos = filePos, rules = rules}, u)
	 end
   in
      loopDec (d, initDown)
   end
    
fun scope (dec: Dec.t): Dec.t =
   let
      fun bind (env, tyvars) =
	 let
	    val (env, tyvars) = Env.rename (env, tyvars)
	    fun finish u = Tyvars.- (u, Tyvars.fromList (Vector.toList tyvars))
	 in
	    (env, tyvars, finish)
	 end
      fun bind' (env, tyvars) =
	 let
	    val (env, tyvars) = Env.rename (env, tyvars)
	    fun finish u =
	       (Vector.fromList
		(Tyvars.toList
		 (Tyvars.+ (u, Tyvars.fromList (Vector.toList tyvars)))),
		Tyvars.empty)
	 in
	    (env, finish)
	 end
      fun tyvar (a, env) =
	 let
	    val a =
	       case Env.peek (env, a) of
		  NONE => a
		| SOME a => a
	 in
	    (a, Tyvars.singleton a)
	 end
      val (dec, unguarded) =
	 processDec (dec, {bind = bind,
			   bind' = bind',
			   combineUp = Tyvars.+,
			   initDown = Env.empty,
			   initUp = Tyvars.empty,
			   tyvar = tyvar})
   in
      if Tyvars.isEmpty unguarded
	 then
	    let
	       fun bind (env, tyvars) =
		  let
		     val (env, tyvars) = Env.rename (env, tyvars)
		  in
		     (env, tyvars, fn () => ())
		  end
	       fun bind' (env, tyvars) =
		  let
		     val (env, tyvars) =
			Env.rename
			(env,
			 Vector.fromList
			 (Tyvars.toList
			  (Tyvars.- (Tyvars.fromList (Vector.toList tyvars),
				     Tyvars.fromList (Env.domain env)))))
		  in
		     (env, fn () => (tyvars, ()))
		  end
	       fun tyvar (a, env) =  (Env.lookup (env, a), ())
	       val (dec, ()) =
		  processDec (dec, {bind = bind,
				    bind' = bind',
				    combineUp = fn ((), ()) => (),
				    initDown = Env.empty,
				    initUp = (),
				    tyvar = tyvar})
	    in
	       dec
	    end
      else
	 let
	    open Layout
	    val _ = 
	       Control.error (Dec.region dec,
			      seq [str "free type variables: ",
				   List.layout Tyvar.layout
				   (Tyvars.toList unguarded)],
			      empty)
	 in
	    dec
	 end
   end

val scope = Trace.trace ("scope", Dec.layout, Dec.layout) scope

end
