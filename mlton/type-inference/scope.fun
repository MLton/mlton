(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
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
open CoreML

structure Env = TyvarEnv
structure Tyvars = UnorderedSet (UseName (Tyvar))

fun renames (xs, renameX) =
   let
      val (xs, unguarded) =
	 Vector.mapAndFold (xs, Tyvars.empty, fn (x, tyvars) =>
			    let val (x, unguarded) = renameX x
			    in (x, Tyvars.+ (tyvars, unguarded))
			    end)
   in (xs, unguarded)
   end

fun renameTy (t, env) =
   let
      open Type
      fun loop t =
	 case t of
	    Var a =>
	       let val a = (case Env.peek (env, a) of
			       NONE => a
			     | SOME a => a)
	       in (Var a, Tyvars.singleton a)
	       end
	  | Con (c, ts) => let val (ts, unguarded) = renames (ts, loop)
			   in (Con (c, ts), unguarded)
			   end
	  | Record r => let val (r, u) = Record.change (r, fn ts =>
							renames (ts, loop))
			in (Record r, u)
			end
   in loop t
   end

fun renameTyOpt (to, env) =
   case to of
      NONE => (NONE, Tyvars.empty)
    | SOME t => let val (t, unguarded) = renameTy (t, env)
		in (SOME t, unguarded)
		end

fun renamePat (p, env) =
   let open Pat
      fun loopOpt opt =
	 case opt of
	    NONE => (NONE, Tyvars.empty)
	  | SOME p => let val (p, u) = loop p
		      in (SOME p, u)
		      end
      and loop p =
	 case p of
	    Wild => (p, Tyvars.empty)
	  | Var _ => (p, Tyvars.empty)
	  | Const _ => (p, Tyvars.empty)
	  | Con {con, arg} => let val (arg, unguarded) = loopOpt arg
			    in (Con {con = con, arg = arg}, unguarded)
			    end
	  | Record {flexible, record} =>
	       let val (r, u) = Record.change (record, loops)
	       in (Record {flexible = flexible,
			  record = r},
		   u)
	       end
	  | Constraint (p, t) =>
	       let val (p, unguarded) = loop p
		  val (t, unguarded') = renameTy (t, env)
	       in (Constraint (p, t),
		   Tyvars.+ (unguarded, unguarded'))
	       end
	  | Layered (x, p) =>
	       let val (p, unguarded) = loop p
	       in (Layered (x, p), unguarded)
	       end
      and loops ps = renames (ps, loop)
   in loop p
   end

open Dec Exp
   
fun renameDec (d, env) =
   case d of
      Val {tyvars, pat, exp, filePos} =>
	 let
	    val (env, tyvars) = TyvarEnv.rename (env, tyvars)
	    val (pat, u1) = renamePat (pat, env)
	    val (exp, u2) = renameExp (exp, env)
	 in (Val
	     {tyvars = (Vector.fromList
			(Tyvars.toList
			 (Tyvars.unions [u1, u2,
					 Tyvars.fromList
					 (Vector.toList tyvars)]))),
	      pat = pat, exp = exp, filePos = filePos},
	     Tyvars.empty)
	 end
    | Fun {tyvars, decs} =>
	 let
	    val (env, tyvars) = TyvarEnv.rename (env, tyvars)
	    val (decs, unguarded) =
	       renames (decs, fn {var, ty, match} =>
			 let val (ty, u1) = renameTyOpt (ty, env)
			    val (match, u2) = renameMatch (match, env)
			 in ({var = var, ty = ty, match = match},
			     Tyvars.+ (u1, u2))
			 end)
	 in (Fun {tyvars = (Vector.fromList
			    (Tyvars.toList
			     (Tyvars.+
			      (unguarded,
			       Tyvars.fromList (Vector.toList tyvars))))),
		 decs = decs},
	     Tyvars.empty)
	 end
    | Datatype dbs =>
	 let
	    val (dbs, unguarded) =
	       renames
	       (dbs, fn {tyvars, tycon, cons} =>
		let
		   val (env, tyvars) = TyvarEnv.rename (env, tyvars)
		   val (cons, unguarded) =
		      renames (cons, fn {con, arg} =>
				let val (arg, unguarded) = renameTyOpt (arg, env)
				in ({con = con, arg = arg}, unguarded)
				end)
		in ({tyvars = tyvars, tycon = tycon, cons = cons},
		    Tyvars.- (unguarded, Tyvars.fromList (Vector.toList tyvars)))
		end)
	 in (Datatype dbs, unguarded)
	 end
    | Exception {con, arg} =>
	 let val (arg, unguarded) = renameTyOpt (arg, env)
	 in (Exception {con = con, arg = arg}, unguarded)
	 end
    | Overload _ => (d, Tyvars.empty)
and renameDecs (ds, env) = renames (ds, fn d => renameDec (d, env))
and renameExp (e, env) =
   let val empty = (e, Tyvars.empty)
   in case e of
      Var _ => empty
    | Prim _ => empty
    | Const _ => empty
    | Con _ => empty
    | Record r =>
	 let val (r, u) = Record.change (r, fn es => renameExps (es, env))
	 in (Record r, u)
	 end
    | Fn m =>
	 let val (m, unguarded) = renameMatch (m, env)
	 in (Fn m, unguarded)
	 end
    | App (e1, e2) =>
	 let
	    val (e1, u1) = renameExp (e1, env)
	    val (e2, u2) = renameExp (e2, env)
	 in (App (e1, e2), Tyvars.+ (u1, u2))
	 end
    | Let (ds, e) =>
	 let
	    val (ds, u1) = renameDecs (ds, env)
	    val (e, u2) = renameExp (e, env)
	 in (Let (ds, e), Tyvars.+ (u1, u2))
	 end
    | Constraint (e, t) =>
	 let
	    val (e, u1) = renameExp (e, env)
	    val (t, u2) = renameTy (t, env)
	 in (Constraint (e, t), Tyvars.+ (u1, u2))
	 end
    | Handle (e, m) =>
	 let
	    val (e, u1) = renameExp (e, env)
	    val (m, u2) = renameMatch (m, env)
	 in (Handle (e, m), Tyvars.+ (u1, u2))
	 end
    | Raise {exn, filePos} =>
	 let
	    val (exn, unguarded) = renameExp (exn, env)
	 in
	    (Raise {exn = exn, filePos = filePos}, unguarded)
	 end
   end
and renameExps (es, env) = renames (es, fn e => renameExp (e, env))
and renameMatch (Match.T {rules, filePos}, env) =
   let
      val (rs, unguarded) = renames (rules, fn r => renameRule (r, env))
   in (Match.T {rules = rs, filePos = filePos}, unguarded)
   end
and renameRule ((p, e), env) =
   let val (p, u1) = renamePat (p, env)
      val (e, u2) = renameExp (e, env)
   in ((p, e), Tyvars.+ (u1, u2))
   end      

fun bindNew (bound: Env.t, tyvars: Tyvar.t vector): Env.t * Tyvar.t vector =
   TyvarEnv.rename
   (bound,
    Vector.fromList
    (Tyvars.toList (Tyvars.- (Tyvars.fromList (Vector.toList tyvars),
			      Tyvars.fromList (Env.domain bound)))))

fun removeTy (ty: Type.t, env: Env.t): Type.t =
   let
      open Type
      fun loop ty =
	 case ty of
	    Var a => Var (Env.lookup (env, a))
	  | Con (c, tys) => Con (c, Vector.map (tys, loop))
	  | Record r => Record (Record.map (r, loop))
   in loop ty
   end

fun removeTyOpt (ty: Type.t option, env: Env.t): Type.t option =
   case ty of
      NONE => NONE
    | SOME ty => SOME (removeTy (ty, env))

fun removePat (p: Pat.t, env: Env.t): Pat.t =
   let open Pat
      fun loopOpt opt =
	 case opt of
	    NONE => NONE
	  | SOME p => SOME (loop p)
      and loops ps = List.map (ps, loop)
      and loop p =
	 case p of
	    Wild => p
	  | Var _ => p
	  | Const _ => p
	  | Con {con, arg} => Con {con = con, arg = loopOpt arg}
	  | Record {flexible, record} => Record {flexible = flexible,
					      record = Record.map (record, loop)}
	  | Constraint (p, t) => Constraint (loop p, removeTy (t, env))
	  | Layered (x, p) => Layered (x, loop p)
   in loop p
   end
      
fun removes (xs, scope, removeX) =
   Vector.map (xs, fn x => removeX (x, scope))

fun removeDec (d: Dec.t, scope: Env.t): Dec.t =
   case d of
      Val {tyvars, pat, exp, filePos} =>
	 let val (scope, tyvars) = bindNew (scope, tyvars)
	 in Val {tyvars = tyvars,
		 pat = removePat (pat, scope),
		 exp = removeExp (exp, scope),
		 filePos = filePos}
	 end
    | Fun {tyvars, decs} =>
	 let val (scope, tyvars) = bindNew (scope, tyvars)
	 in Fun {tyvars = tyvars,
		 decs = Vector.map (decs, fn {var, ty, match} =>
				    {var = var,
				     ty = removeTyOpt (ty, scope),
				     match = removeMatch (match, scope)})}
	 end
    | Exception {con, arg} => Exception {con = con,
				       arg = removeTyOpt (arg, scope)}
    | Datatype dbs =>
	 Datatype
	 (Vector.map (dbs, fn {tyvars, tycon, cons} =>
		      let val (scope, tyvars) = TyvarEnv.rename (scope, tyvars)
		      in {tyvars = tyvars,
			  tycon = tycon,
			  cons = Vector.map (cons, fn {con, arg} =>
					     {con = con,
					      arg = removeTyOpt (arg, scope)})}
		      end))
    | Overload _ => d

and removeExp (e: Exp.t, scope: Env.t): Exp.t =
   case e of
      Var _ => e
    | Prim _ => e
    | Const _ => e
    | Con _ => e
    | Record r => Record (Record.map (r, fn e => removeExp (e, scope)))
    | Fn m => Fn (removeMatch (m, scope))
    | App (e1, e2) => App (removeExp (e1, scope), removeExp (e2, scope))
    | Let (ds, e) => Let (removes (ds, scope, removeDec),
			  removeExp (e, scope))
    | Constraint (e, t) => Constraint (removeExp (e, scope), removeTy (t, scope))
    | Handle (e, m) => Handle (removeExp (e, scope), removeMatch (m, scope))
    | Raise {exn, filePos} =>
	 Raise {exn = removeExp (exn, scope),
		filePos = filePos}
and removeMatch (Match.T {rules, filePos}, scope) =
   Match.T {rules = removes (rules, scope, removeRule),
	    filePos = filePos}
and removeRule ((p, e), scope) = (removePat (p, scope), removeExp (e, scope))
    
fun scopeDec d =
   let
      val (d, unguarded) = renameDec (d, Env.empty)
   in
      if Tyvars.isEmpty unguarded
	 then removeDec (d, Env.empty)
      else Error.bug "scope: free type variable"
   end

val scopeDec = Trace.trace ("scopeDec", Dec.layout, Dec.layout) scopeDec

fun scope (Program.T {decs}) = Program.T {decs = Vector.map (decs, scopeDec)}

val scope = Trace.trace ("scope", Program.layout, Program.layout) scope
   
end
