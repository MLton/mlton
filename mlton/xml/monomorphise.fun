(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor Monomorphise (S: MONOMORPHISE_STRUCTS): MONOMORPHISE =
struct

open S
open Xml.Atoms
local open Xml
in
   structure Xcases = Cases
   structure Xpat = Pat
   structure Xdec = Dec
   structure Xexp = Exp
   structure Xlambda = Lambda
   structure XprimExp = PrimExp
   structure Xprogram = Program
   structure Xtype = Type
   structure XvarExp = VarExp
end
local open Sxml
in
   structure Scases = Cases
   structure Spat = Pat
   structure Sdec = Dec
   structure Sexp = Exp
   structure Slambda = Lambda
   structure SprimExp = PrimExp
   structure Sprogram = Program
   structure Stype = Type
   structure SvarExp = VarExp
end

structure Cache:
   sig
      type 'a t
      val new: unit -> 'a t
      val getOrAdd: 'a t * Stype.t vector * (unit -> 'a) -> 'a
      val toList: 'a t -> (Stype.t vector * 'a) list
   end =
   struct
      structure Stype =
	 struct
	    open Stype
	       
	    val layout =
	       fn t =>
	       let open Layout
	       in record [("hash", str (Word.toString (hash t))),
			  ("type", layout t)]
	       end
	 end

      structure Stypes = MonoVector (Stype)

      structure C1 =
 	 struct
 	    structure P = PolyCache
 	       
 	    type 'a t = (Stypes.t, 'a) P.t
 	    fun new () = P.new {equal = Stypes.equals}
 	    val getOrAdd = P.getOrAdd
	       
 	    val toList = P.toList
 	 end
      (*open C1*)

      structure C2 =
	 (* use a splay tree based on lexicographic ordering of vectors of hash
	  * values of types.  Use an alist (i.e. polycache) within each bucket
	  * of the splay tree
	  *)
	 struct
	    structure Cache = PolyCache
	       
	    structure S =
	       SplayMapFn
	       (type ord_key = Stype.t vector
		val compare =
		   fn (ts, ts') =>
		   Vector.compare (ts, ts',
				   fn (t, t') =>
				   Word.compare (Stype.hash t,
						 Stype.hash t')))

	    type 'a t = (Stype.t vector, 'a) Cache.t S.map ref

	    fun new () = ref S.empty

	    local
	       fun equal (v, v') =
		  Vector.equals (v, v', Stype.equals)
	    in
	       fun getOrAdd (m, k, th) =
		  case S.find (!m, k) of
		     NONE => let
				val x = th ()
				val cache =
				   Cache.fromList {equal = equal, 
						   elements = [(k, x)]}
			     in m := S.insert (!m, k, cache); x
			     end
		   | SOME cache => Cache.getOrAdd (cache, k, th)
	    end
	 
	    fun toList c =
	       List.fold (S.listItems (! c), [], fn (cache, items) =>
			  Cache.toList cache @ items)
	 end
      open C2

      (* This structure compares C1 and C2 to make sure they are behaving
       * the same way.  Useful to find bugs in C2.
       * In order to use it, uncomment the open following.
       *)
      structure C1C2 =
	 struct
	    type 'a t = 'a C1.t * 'a C2.t
	       
	    fun new () = (C1.new (), C2.new ())
	       
	    fun layout (c1, c2) =
	       let
		  fun layout c =
		     Layout.list (List.map (c, fn (ts, _) => Stypes.layout ts))
	       in Layout.align [layout (C1.toList c1), layout (C2.toList c2)]
	       end
	    
	    fun getOrAdd (c as (c1, c2), ts, th) =
	       let val th = Promise.lazy th
		  val b1 = ref false
		  val b2 = ref false
		  fun check (g, c, b) = g (c, ts, fn () => (b := true; th ()))
		  val x = check (C1.getOrAdd, c1, b1)
	       in check (C2.getOrAdd, c2, b2);
		  if !b1 = !b2
		     then x
		  else (Control.diagnostic
			(fn () =>
			 (let open Layout
			  in seq [str "getOrAdd",
				  tuple [layout c, Stypes.layout ts]]
			  end))
			; Error.bug "getOrAdd")
	       end
	    
	    fun toList (c as (c1, c2)) =
	       let val l1 = C1.toList c1
		  val l2 = C2.toList c2
		  fun sub (l1, l2) =
		     List.forall (l1, fn (ts1, _) =>
				  List.exists (l2, fn (ts2, _) =>
					       Stypes.equals (ts1, ts2)))
	       in if (List.length l1 = List.length l2
		      andalso sub (l1, l2) andalso sub (l2, l1))
		     then l1
		  else (Control.diagnostic (fn () =>
					    let open Layout
					    in seq [str "toList ", layout c]
					    end)
			; Error.bug "toList")
	       end
	 end
      (* open C1C2 *)
   end

fun monomorphise (Xprogram.T {datatypes, body, ...}): Sprogram.t =
   let
      val {get = getVar: Var.t -> Stype.t vector -> SvarExp.t,
	   set = setVar, ...} =
	 Property.getSet (Var.plist, Property.initRaise ("var", Var.layout))
      val setVar =
	 Trace.trace2 ("setVar", Var.layout, Layout.ignore, Unit.layout) setVar
      val getVar =
	 Trace.trace ("getVar", Var.layout, Layout.ignore) getVar
      val {get = getCon: Con.t -> (Stype.t vector -> Con.t),
	   set = setCon, destroy = destroyCon} =
	 Property.destGetSet (Con.plist, Property.initRaise ("mono", Con.layout))
      val {get = getTycon: Tycon.t -> Stype.t vector -> Stype.t,
	   set = setTycon, destroy = destroyTycon} =
	 Property.destGetSet (Tycon.plist,
			      Property.initRaise ("mono", Tycon.layout))
      val _ =
	 List.foreach (Tycon.prims, fn t =>
		       setTycon (t, fn ts => Stype.con (t, ts)))
      val {set = setTyvar, get = getTyvar: Tyvar.t -> Stype.t, ...} =
	 Property.getSet (Tyvar.plist,
			  Property.initRaise ("tyvar", Tyvar.layout))
      val getTyvar =
	 Trace.trace ("getTyvar", Tyvar.layout, Stype.layout) getTyvar
      val setTyvar =
	 Trace.trace2 ("setTyvar", Tyvar.layout, Stype.layout, Unit.layout)
	 setTyvar
      fun setTyvars (tyvs, tys) = Vector.foreach2 (tyvs, tys, setTyvar)
      fun monoType (t: Xtype.t): Stype.t =
	 Xtype.hom {ty = t,
		    var = getTyvar,
		    con = fn (c, ts) => getTycon c ts}
      val monoType =
	 Trace.trace ("monoType", Xtype.layout, Stype.layout) monoType
      fun monoTypeOpt (to: Xtype.t option): Stype.t option =
	 case to of
	    NONE => NONE
	  | SOME t => SOME (monoType t)
      fun monoTypes ts = Vector.map (ts, monoType)
      fun monoCon (c: Con.t, ts: Xtype.t vector): Con.t = getCon c (monoTypes ts)
      val monoCon =
	 Trace.trace2 ("monoCon", Con.layout, Vector.layout Xtype.layout,
		       Con.layout)
	 monoCon
      (* It is necessary to create new variables for monomorphic variables
       * because they still may have type variables in their type.
       *)
      fun renameMono (x, t) =
	 let
	    val x' = Var.new x
	    val ve = SvarExp.mono x'
	    fun inst ts =
	       if 0 = Vector.length ts
		  then ve
	       else Error.bug "monomorphise: expected monomorphic instance"
	    val _ = setVar (x, inst)
	 in
	    (x', monoType t)
	 end
      val renameMono =
      	 Trace.trace2 ("renameMono", Var.layout, Xtype.layout,
		       Layout.tuple2 (Var.layout, Stype.layout)) renameMono
      fun monoPat (Xpat.T {con, targs, arg}): Spat.t =
	 let
	    val con = monoCon (con, targs)
	 in
	    Spat.T {con = con, targs = Vector.new0 (),
		    arg = (case arg of
			      NONE => NONE
			    | SOME x => SOME (renameMono x))}
	 end
      val monoPat = Trace.trace ("monoPat", Xpat.layout, Spat.layout) monoPat
      fun finishDecs decs = decs []
      val traceMonoExp =
	 Trace.trace ("monoExp", Xexp.layout, Sexp.layout)
      val traceMonoDec =
	 Trace.trace ("monoDec", Xdec.layout,
		      fn (_: unit -> Sdec.t list) => Layout.empty)
      (*------------------------------------*)
      (*             datatypes              *)
      (*------------------------------------*)
      val newDbs: {tyvars: Tyvar.t vector,
		   types: Stype.t vector,
		   tycon: Tycon.t,
		   ty: Stype.t,
		   cons: {con: Con.t,
			  typ: Xtype.t option,
			  used: bool} ref vector} list ref = ref []
      val _ =
	 Vector.foreach
	 (datatypes, fn {tyvars, tycon, cons} =>
	  let
	     val cache = Cache.new ()
	     fun instantiate ts =
		Cache.getOrAdd
		(cache, ts, fn () =>
		 let
		    val (tycon, cons) =
		       if Tycon.equals (tycon, Tycon.bool)
			  then (tycon,
				Vector.map (cons, fn {con, ...} =>
					    ref {con = con, typ = NONE,
						 used = true}))
		       else 
			  (Tycon.new tycon,
			   Vector.map (cons, fn {con, arg} =>
				       ref {con = con, typ = arg,
					    used = false}))
		    val db =
		       {tyvars = tyvars,
			types = ts,
			tycon = tycon,
			ty = Stype.con (tycon, Vector.new0 ()),
			cons = cons}
		    val _ = List.push (newDbs, db)
		 in
		    db
		 end)
	     val _ = setTycon (tycon, #ty o instantiate)
	     val _ =
		Vector.foreachi
		(cons, fn (n, {con, ...}) =>
		 setCon (con, fn ts =>
			 let
			    val r as ref {con, typ, used} =
			       Vector.sub (#cons (instantiate ts), n)
			 in if used then con
			    else let val con = Con.new con
				 in r := {con = con, typ = typ,
					  used = true}
				    ; con
				 end
			 end))
	  in ()
	  end)
      val _ = (monoCon (Con.truee, Vector.new0 ())
	       ; monoCon (Con.falsee, Vector.new0 ()))
      fun finishDbs ac =
	 let
	    val dbs = !newDbs
	    val _ = newDbs := []
	 in case dbs of
	    [] => ac
	  | _ =>
	       finishDbs
	       (List.fold
		(dbs, ac,
		 fn ({tyvars, types, tycon, cons, ...}, ac) =>
		 let
		    val cons =
		       Vector.keepAllMap
		       (cons, fn ref {con, typ, used} =>
			if used
			   then (setTyvars (tyvars, types)
				 ; SOME {con = con,
					 arg = monoTypeOpt typ})
			else NONE)
		    val cons =
		       if Vector.isEmpty cons
			  then Vector.new1 {con = Con.newNoname (), arg = NONE}
		       else cons
		 in {tycon = tycon, tyvars = Vector.new0 (), cons = cons}
		    :: ac
		 end))
	 end
      (*------------------------------------*)
      (*              monoExp               *)
      (*------------------------------------*)
      fun monoVarExp (XvarExp.T {var, targs}) =
	 getVar var (monoTypes targs)
      val monoVarExp =
       	 Trace.trace ("monoVarExp", XvarExp.layout, SvarExp.layout) monoVarExp
      fun monoVarExps xs = Vector.map (xs, monoVarExp)
      fun monoExp (arg: Xexp.t): Sexp.t =
	 traceMonoExp
	 (fn (e: Xexp.t) =>
	  let val {decs, result} = Xexp.dest e
	     val thunks = List.fold (decs, [], fn (d, thunks) =>
				     monoDec d :: thunks)
	     val result = monoVarExp result
	     val decs =
		List.fold (thunks, [], fn (thunk, decs) => thunk () @ decs)
	  in Sexp.new {decs = decs,
		       result = result}
	  end) arg
      and monoPrimExp (e: XprimExp.t): SprimExp.t =
	 case e of
	    XprimExp.Const c => SprimExp.Const c
	  | XprimExp.Var x => SprimExp.Var (monoVarExp x)
	  | XprimExp.Tuple xs => SprimExp.Tuple (monoVarExps xs)
	  | XprimExp.Select {tuple, offset} =>
	       SprimExp.Select {tuple = monoVarExp tuple, offset = offset}
	  | XprimExp.Lambda l => SprimExp.Lambda (monoLambda l)
	  | XprimExp.ConApp {con, targs, arg} =>
	       let val con = monoCon (con, targs)
	       in SprimExp.ConApp {con = con, targs = Vector.new0 (),
				   arg = Option.map (arg, monoVarExp)}
	       end
	  | XprimExp.PrimApp {prim, targs, args} =>
	       SprimExp.PrimApp {prim = prim,
				 targs = monoTypes targs,
				 args = monoVarExps args}
	  | XprimExp.App {func, arg} =>
	       SprimExp.App {func = monoVarExp func, arg = monoVarExp arg}
	  | XprimExp.Raise {exn, filePos} =>
	       SprimExp.Raise {exn = monoVarExp exn,
			       filePos = filePos}
	  | XprimExp.Handle {try, catch, handler} =>
	       SprimExp.Handle {try = monoExp try,
				catch = renameMono catch,
				handler = monoExp handler}
	  | XprimExp.Case {test, cases, default} =>
	       let
		  fun doit cases =
		     Vector.map (cases, fn (c, e) => (c, monoExp e))
		  val cases =
		     case cases of
			Xcases.Char l => Scases.Char (doit l)
		      | Xcases.Con cases => 
			   Scases.Con (Vector.map (cases, fn (pat, exp) =>
						   (monoPat pat, monoExp exp)))
		      | Xcases.Int l => Scases.Int (doit l)
		      | Xcases.Word l => Scases.Word (doit l)
		      | Xcases.Word8 l => Scases.Word8 (doit l)
	       in
		  SprimExp.Case
		  {test = monoVarExp test,
		   cases = cases,
		   default = Option.map (default, fn (e, r) =>
					 (monoExp e, r))}
	       end
      and monoLambda l: Slambda.t =
	 let
	    val {arg, argType, body, region} = Xlambda.dest l
	    val (arg, argType) = renameMono (arg, argType)
	 in
	    Slambda.new {arg = arg,
			 argType = argType,
			 body = monoExp body,
			 region = region}
	 end
      (*------------------------------------*)
      (*              monoDec               *)
      (*------------------------------------*)
      and monoDec arg: unit -> Sdec.t list =
	 traceMonoDec
	 (fn Xdec.MonoVal {var, ty, exp} =>
	  let
	     val (var, _) = renameMono (var, ty)
	  in fn () => [Sdec.MonoVal {var = var,
				     ty = monoType ty,
				     exp = monoPrimExp exp}]
	  end
       | Xdec.PolyVal {var, tyvars, ty, exp} =>
	    let
	       val cache = Cache.new ()
	       val _ =
		  setVar (var, fn ts =>
			  (setTyvars (tyvars, ts)
			   ; Cache.getOrAdd (cache, ts, fn () =>
					     SvarExp.mono (Var.new var))))
	    in
	       fn () =>
	       List.fold
	       (Cache.toList cache, [], fn ((ts, ve), decs) =>
		(setTyvars (tyvars, ts)
		 ; let val {decs = decs', result} = Sexp.dest (monoExp exp)
		   in decs'
		      @ (Sdec.MonoVal {var = SvarExp.var ve,
				       ty = monoType ty,
				       exp = SprimExp.Var result} :: decs)
		   end))
	    end
       | dec as Xdec.Fun {tyvars, decs} =>
	    let
	       val cache = Cache.new ()
	       val _ =
		  Vector.foreachi
		  (decs, fn (n, {var, ...}) =>
		   setVar
		   (var, fn ts =>
		    (setTyvars (tyvars, ts)
		     ; Vector.sub (Cache.getOrAdd
				   (cache, ts, fn () =>
				    Vector.map (decs,
						SvarExp.mono o Var.new o #var)),
				   n))))
	    in fn () =>
	       List.revMap
	       (Cache.toList cache, fn (ts, xs) =>
		(setTyvars (tyvars, ts)
		 ; Vector.foreach2 (decs, xs, fn ({var, ...}, var') =>
				    setVar (var, fn _ => var'))
		 ; (Sdec.Fun
		    {tyvars = Vector.new0 (),
		     decs = (Vector.map2
			     (decs, xs, fn ({ty, lambda, ...}, ve) =>
			      {var = SvarExp.var ve,
			       ty = monoType ty,
			       lambda = monoLambda lambda}))})))
	    end
       | Xdec.Exception {con, arg} =>
	    let
	       val con' =
		  if Con.equals (con, Con.overflow)
		     then
			(* We avoid renaming Overflow because the closure
			 * converter needs to recognize it.  This is not
			 * safe in general, but is OK in this case because
			 * we know there is only one Overflow excon.
			 *)
			con
		  else Con.new con
	       val _ = setCon (con, fn _ => con')
	    in
	       fn () => [Sdec.Exception {con = con',
					 arg = monoTypeOpt arg}]
	    end) arg
      (*------------------------------------*)
      (*     main code for monomorphise     *)
      (*------------------------------------*)
      val body = monoExp body
      val datatypes = finishDbs []
      val program =
	 Sprogram.T {datatypes = Vector.fromList datatypes,
		     body = body,
		     overflow = NONE}
      val _ = Sprogram.clear program
      val _ = destroyCon ()
      val _ = destroyTycon ()
   in
      program
   end

end
