(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
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
   let
      open Pat
      fun loopOpt opt =
	 case opt of
	    NONE => (NONE, Tyvars.empty)
	  | SOME p => let val (p, u) = loop p
		      in (SOME p, u)
		      end
      and loop p =
	 let
	    fun doit n = Pat.makeRegion (n, region p)
	 in
	    case node p of
	       Wild => (p, Tyvars.empty)
	     | Var _ => (p, Tyvars.empty)
	     | Const _ => (p, Tyvars.empty)
	     | Con {con, arg} =>
		  let
		     val (arg, unguarded) = loopOpt arg
		  in
		     (doit (Con {con = con, arg = arg}),
		      unguarded)
		  end
	     | Record {flexible, record} =>
		  let
		     val (r, u) = Record.change (record, loops)
		  in
		     (doit (Record {flexible = flexible,
				    record = r}),
		      u)
		  end
	     | Constraint (p, t) =>
		  let
		     val (p, unguarded) = loop p
		     val (t, unguarded') = renameTy (t, env)
		  in
		     (doit (Constraint (p, t)),
		      Tyvars.+ (unguarded, unguarded'))
		  end
	     | Layered (x, p) =>
		  let
		     val (p, unguarded) = loop p
		  in
		     (doit (Layered (x, p)), unguarded)
		  end
	 end
      and loops ps = renames (ps, loop)
   in
      loop p
   end

datatype z = datatype Dec.node
datatype z = datatype Exp.node
   
fun renameDec (d, env) =
   let
      fun doit n = Dec.makeRegion (n, Dec.region d)
   in
      case Dec.node d of
	 Val {tyvars, pat, exp, filePos} =>
	    let
	       val (env, tyvars) = TyvarEnv.rename (env, tyvars)
	       val (pat, u1) = renamePat (pat, env)
	       val (exp, u2) = renameExp (exp, env)
	    in (doit (Val
		      {tyvars = (Vector.fromList
				 (Tyvars.toList
				  (Tyvars.unions [u1, u2,
						  Tyvars.fromList
						  (Vector.toList tyvars)]))),
		       pat = pat, exp = exp, filePos = filePos}),
		Tyvars.empty)
	    end
       | Fun {tyvars, decs} =>
	    let
	       val (env, tyvars) = TyvarEnv.rename (env, tyvars)
	       val (decs, unguarded) =
		  renames (decs, fn {match, profile, types, var} =>
			   let
			      val (types, u1) = renames (types, fn t =>
							 renameTy (t, env))
			      val (match, u2) = renameMatch (match, env)
			   in
			      ({match = match,
				profile = profile,
				types = types,
				var = var},
			       Tyvars.+ (u1, u2))
			   end)
	    in (doit (Fun {tyvars = (Vector.fromList
				     (Tyvars.toList
				      (Tyvars.+
				       (unguarded,
					Tyvars.fromList
					(Vector.toList tyvars))))),
			   decs = decs}),
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
				  let
				     val (arg, unguarded) =
					renameTyOpt (arg, env)
				  in ({con = con, arg = arg}, unguarded)
				  end)
		   in ({tyvars = tyvars, tycon = tycon, cons = cons},
		       Tyvars.- (unguarded,
				 Tyvars.fromList (Vector.toList tyvars)))
		   end)
	    in
	       (doit (Datatype dbs), unguarded)
	    end
       | Exception {con, arg} =>
	    let
	       val (arg, unguarded) = renameTyOpt (arg, env)
	    in
	       (doit (Exception {con = con, arg = arg}), unguarded)
	    end
       | Overload _ => (d, Tyvars.empty)
   end
and renameDecs (ds, env) = renames (ds, fn d => renameDec (d, env))
and renameExp (e, env) =
   let
      val empty = (e, Tyvars.empty)
      val region = Exp.region e
      fun doit n = Exp.makeRegion (n, region)
   in
      case Exp.node e of
	 App (e1, e2) =>
	    let
	       val (e1, u1) = renameExp (e1, env)
	       val (e2, u2) = renameExp (e2, env)
	    in
	       (doit (App (e1, e2)), Tyvars.+ (u1, u2))
	    end
       | Con _ => empty
       | Const _ => empty
       | Constraint (e, t) =>
	    let
	       val (e, u1) = renameExp (e, env)
	       val (t, u2) = renameTy (t, env)
	    in
	       (doit (Constraint (e, t)), Tyvars.+ (u1, u2))
	    end
       | Fn {match = m, profile} =>
	    let
	       val (m, unguarded) = renameMatch (m, env)
	    in
	       (doit (Fn {match = m, profile = profile}),
		unguarded)
	    end
       | Handle (e, m) =>
	    let
	       val (e, u1) = renameExp (e, env)
	       val (m, u2) = renameMatch (m, env)
	    in
	       (doit (Handle (e, m)), Tyvars.+ (u1, u2))
	    end
       | Let (ds, e) =>
	    let
	       val (ds, u1) = renameDecs (ds, env)
	       val (e, u2) = renameExp (e, env)
	    in
	       (doit (Let (ds, e)), Tyvars.+ (u1, u2))
	    end
       | Prim _ => empty
       | Raise {exn, filePos} =>
	    let
	       val (exn, unguarded) = renameExp (exn, env)
	    in
	       (doit (Raise {exn = exn, filePos = filePos}), unguarded)
	    end
       | Record r =>
	    let val (r, u) = Record.change (r, fn es => renameExps (es, env))
	    in (doit (Record r), u)
	    end
       | Var _ => empty
   end
and renameExps (es, env) = renames (es, fn e => renameExp (e, env))
and renameMatch (m, env) =
   let
      val (rs, unguarded) = renames (Match.rules m, fn r => renameRule (r, env))
   in
      (Match.new {rules = rs, filePos = Match.filePos m},
       unguarded)
   end
and renameRule ((p, e), env) =
   let
      val (p, u1) = renamePat (p, env)
      val (e, u2) = renameExp (e, env)
   in
      ((p, e), Tyvars.+ (u1, u2))
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
   let
      open Pat
      fun loopOpt opt =
	 case opt of
	    NONE => NONE
	  | SOME p => SOME (loop p)
      and loops ps = List.map (ps, loop)
      and loop p =
	 let
	    fun doit n = makeRegion (n, region p)
	 in
	    case node p of
	       Wild => p
	     | Var _ => p
	     | Const _ => p
	     | Con {con, arg} =>
		  doit (Con {con = con, arg = loopOpt arg})
	     | Record {flexible, record} =>
		  doit (Record {flexible = flexible,
				record = Record.map (record, loop)})
	     | Constraint (p, t) =>
		  doit (Constraint (loop p, removeTy (t, env)))
	     | Layered (x, p) => doit (Layered (x, loop p))
	 end
   in
      loop p
   end
      
fun removes (xs, scope, removeX) =
   Vector.map (xs, fn x => removeX (x, scope))

fun removeDec (d: Dec.t, scope: Env.t): Dec.t =
   let
      fun doit n = Dec.makeRegion (n, Dec.region d)
   in
      case Dec.node d of
	 Val {tyvars, pat, exp, filePos} =>
	    let
	       val (scope, tyvars) = bindNew (scope, tyvars)
	    in
	       doit (Val {tyvars = tyvars,
			  pat = removePat (pat, scope),
			  exp = removeExp (exp, scope),
			  filePos = filePos})
	    end
       | Fun {tyvars, decs} =>
	    let
	       val (scope, tyvars) = bindNew (scope, tyvars)
	    in
	       doit
	       (Fun {tyvars = tyvars,
		     decs = (Vector.map
			     (decs, fn {match, profile, types, var} =>
			      {match = removeMatch (match, scope),
			       profile = profile,
			       types = Vector.map (types, fn t =>
						   removeTy (t, scope)),
			       var = var}))})
	    end
       | Exception {con, arg} =>
	    doit (Exception {con = con,
			     arg = removeTyOpt (arg, scope)})
       | Datatype dbs =>
	    doit
	    (Datatype
	     (Vector.map
	      (dbs, fn {tyvars, tycon, cons} =>
	       let val (scope, tyvars) = TyvarEnv.rename (scope, tyvars)
	       in {tyvars = tyvars,
		   tycon = tycon,
		   cons = Vector.map (cons, fn {con, arg} =>
				      {con = con,
				       arg = removeTyOpt (arg, scope)})}
	       end)))
       | Overload _ => d
   end
and removeExp (e: Exp.t, scope: Env.t): Exp.t =
   let
      fun doit n = Exp.makeRegion (n, Exp.region e)
   in
      case Exp.node e of
	 App (e1, e2) =>
	    doit (App (removeExp (e1, scope), removeExp (e2, scope)))
       | Con _ => e
       | Const _ => e
       | Constraint (e, t) =>
	    doit (Constraint (removeExp (e, scope), removeTy (t, scope)))
       | Fn {match = m, profile} =>
	    doit (Fn {match = removeMatch (m, scope),
		      profile = profile})
       | Handle (e, m) =>
	    doit (Handle (removeExp (e, scope), removeMatch (m, scope)))
       | Let (ds, e) => doit (Let (removes (ds, scope, removeDec),
				   removeExp (e, scope)))
       | Prim _ => e
       | Record r =>
	    doit (Record (Record.map (r, fn e => removeExp (e, scope))))
       | Raise {exn, filePos} =>
	    doit (Raise {exn = removeExp (exn, scope),
			 filePos = filePos})
       | Var _ => e
   end
and removeMatch (m, scope) =
   Match.new {rules = removes (Match.rules m, scope, removeRule),
	      filePos = Match.filePos m}
and removeRule ((p, e), scope) = (removePat (p, scope), removeExp (e, scope))
    
fun scopeDec d =
   let
      val (d, unguarded) = renameDec (d, Env.empty)
   in
      if Tyvars.isEmpty unguarded
	 then removeDec (d, Env.empty)
      else
	 let
	    open Layout
	    val _ = 
	       Control.error (Dec.region d,
			      seq [str "free type variables: ",
				   List.layout Tyvar.layout
				   (Tyvars.toList unguarded)],
			      empty)
	 in
	    d
	 end
   end

val scopeDec = Trace.trace ("scopeDec", Dec.layout, Dec.layout) scopeDec

fun scope (Program.T {decs}) = Program.T {decs = Vector.map (decs, scopeDec)}

val scope = Trace.trace ("scope", Program.layout, Program.layout) scope
   
end
