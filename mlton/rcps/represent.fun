
functor Represent(S: REPRESENT_STRUCTS): REPRESENT =
struct
  open S

  structure Const = Rcps.Const
  structure Var = Rcps.Var
  structure Tycon = Rcps.Tycon
  structure Con = Cps.Con
  structure Type = Cps.Type
  structure Rtype = Rcps.Rtype

  local 
    open Cps
  in

  end

  local 
    open Rcps
  in

  end

  structure XType =
    struct
      structure Access = Rtype.Access
      structure Basic = Rtype.Basic
      datatype z = datatype Rtype.t

      fun one x = Vector.fromList [x]
      fun aggOne ta = Aggregate (one ta)
      fun aggOneX t = aggOne (t, Access.x)
      fun aggOneR t = aggOne (t, Access.r)
      fun aggOneRW t = aggOne (t, Access.rw)
	
      fun enum''' c e = Basic (c (Vector.fromList [e]))
      fun enum' c = Basic (c NONE)
      fun enum c = aggOneR (enum' c)

      val float' = Basic (Basic.Float)
      val float = aggOneR (float')

      fun pointer' rtype = Pointer rtype
      fun pointer rtype = aggOneR (pointer' rtype)

      fun abstract' tycon = pointer' (aggOneX (Tycon tycon))
      fun abstract tycon = aggOneR (abstract' tycon)
	
      fun array' rtype = Array (Access.rw, rtype)
      fun array rtype = aggOneR (array' rtype)
	
      fun vector' rtype = Array (Access.r, rtype)
      fun vector rtype = aggOneR (vector' rtype)
	
      fun ref' rtype = pointer' (aggOneRW rtype)
      fun ref rtype = aggOneR (ref' rtype)

      fun tuple' rtypes 
	= Pointer (Aggregate (Vector.map(rtypes, 
					 fn rtype => (rtype, Access.r))))
      fun tuple ts = aggOneR (tuple' ts)
	
      fun data' tycon = pointer' (Tycon tycon)
      fun data tycon = aggOneR (data' tycon)

      fun default (ty: Type.t): Rtype.t
	= case Type.dest ty
	    of Type.Char => enum' Basic.Char
	     | Type.Word8 => enum' Basic.Byte
	     | Type.Int => enum' Basic.Int
	     | Type.Word => enum' Basic.Word
	     | Type.Real => float'
	     | Type.IntInf => abstract' Tycon.intInf
	     | Type.Thread => abstract' Tycon.thread
	     | Type.Pointer => abstract' Tycon.pointer
	     | Type.String => vector' (enum' Basic.Char)
	     | Type.Array t => array' (default t)
	     | Type.Vector t => vector' (default t)
	     | Type.Ref t => ref' (default t)
	     | Type.Tuple t => tuple' (Vector.map(t, default))
	     | Type.Datatype tycon => data' tycon
    end

  structure XPrimExp =
    struct
      structure CPE = Cps.PrimExp
      structure RPE = Rcps.PrimExp
      structure RS = Rcps.Stmt

      fun default (pe: CPE.t, tyconRep, conRep): 
	          ({var: Var.t, ty: Rtype.t, exp: RPE.t} list * RPE.t)
	= case pe
	    of CPE.Const c => ([], RPE.Const c)
	     | CPE.Tuple vs => ([], RPE.Allocate vs)
	     | CPE.ConApp {con, args}
	     => let
		  val conRep' = conRep con
		  val e
		    = case conRep'
			of Rtype.Aggregate ts
			 => let
			      val (t, _) = Vector.sub(ts, 0)
			    in
			      case t
				of Rtype.Basic (Rtype.Basic.Int (SOME es))
				 => Vector.sub(es, 0)
				 | _ => Error.bug "XPrimExp.default"
			    end
			 | _ => Error.bug "XPrimExp.default"
		  val tag = Var.newNoname ()
		in
		  ([{var = tag,
		     ty = Rtype.Basic (Rtype.Basic.Int (SOME (Vector.new1 e))),
		     exp = RPE.Const (Const.fromInt e)}],
		   RPE.Allocate (Vector.fromList (tag::(Vector.toList args))))
		end
	     | CPE.Select {tuple, offset} 
	     => ([], RPE.Loc (Rcps.Loc.Select {var = tuple, offset = offset}))
	     | CPE.Var v => ([], RPE.Var v)
	     | CPE.PrimApp _ => ([], RPE.Var (Var.newNoname ()))
    end

  fun represent (program as Cps.Program.T {datatypes,
					   globals,
					   functions,
					   main}) : Rcps.Program.t
    = let
	val {get = tyconRep, set = setTyconRep} 
	  = Property.getSetOnce (Tycon.plist,
				 Property.initRaise ("tyconRep", Tycon.layout))
	val {get = conRep, set = setConRep}
	  = Property.getSetOnce (Con.plist,
				 Property.initRaise ("conRep", Con.layout))

(*
	val datatypes
	  = Vector.map
	    (datatypes,
	     fn {tycon, cons}
	      => let
		   val cons
		     = Vector.mapi
		       (cons,
			fn (i, {con, args})
			 => let
			      val tag = SOME (Vector.fromList [i])
			      val tag = Rtype.Basic.Int tag
			      val tag = Rtype.Basic tag
			      val tag = Rtype.T {rep = tag,
						 access = {read = true,
							   write = false}}
			      val args = Vector.toListMap(args, XType.default)
			      val obj = Vector.fromList (tag::args)

			      val conRep
				= Rtype.T {rep = Rtype.Aggregate obj,
					   access = {read = true,
						     write = false}}

			      val _ = setConRep(con, conRep)
			    in
			      conRep
			    end)

		   val tyconRep = Rtype.T {rep = Rtype.Sum cons,
					   access  = {read = true, 
						      write = false}}

		   val _ = setTyconRep(tycon, tyconRep)
		 in
		   {tycon = tycon,
		    rep = tyconRep}
		 end)
*)

	val datatypes
	  = Vector.map
	    (datatypes,
	     fn {tycon, cons}
	      => let
		   val cons
		     = Vector.mapi
		       (cons,
			fn (i, {con, args})
			 => let
			      val tag = SOME (Vector.fromList [i])
			      val tag = Rtype.Basic.Int tag
			      val tag = Rtype.Basic tag
			      val args = Vector.toListMap(args, XType.default)
			      val obj = Vector.fromList(tag::args)

			      val conRep' 
				= Vector.map(obj, fn t => (t, Rtype.Access.r))
			      val conRep = Rtype.Aggregate conRep'

			      val _ = setConRep(con, conRep)
			    in
			      conRep
			    end)

		   val tyconRep =Rtype.Sum cons

		   val _ = setTyconRep(tycon, tyconRep)
		 in
		   {tycon = tycon,
		    rep = tyconRep}
		 end)

	fun xPrimExp exp = XPrimExp.default (exp, tyconRep, conRep)

	val globals
	  = Vector.fold
	    (globals,
	     [],
	     fn ({var, ty, exp}, globals)
	      => let
		   val ty = XType.default ty
		   val (binds, exp) = xPrimExp exp
		 in
		   (binds @ [{var = var, ty = ty, exp = exp}])::globals
		 end)
	val globals = List.concatRev globals
	val globals = Vector.fromList globals

      in
	Rcps.Program.T {datatypes = datatypes,
			inits = {strings = [], intInfs = []},
			globals = globals,
			functions = Vector.fromList [],
			main = main}
      end

end
