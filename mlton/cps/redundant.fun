(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Redundant (S: REDUNDANT_STRUCTS): REDUNDANT = 
struct

open S
type int = Int.t

structure Eqrel:>
   sig
      type t
	 
      val areEquivalent: t * int * int -> bool
      val classes: t -> int list list
      val equals: t * t -> bool
      val fixedPoint: unit -> unit
      val fromTypes: Type.t vector -> t
      val layout: t -> Layout.t
      val refine: t * (int * int -> bool) -> unit
      val unify: t * t -> unit
   end =
   struct
      structure R = EquivalenceRelation
      structure Set = DisjointSet

      datatype t = T of {me: R.t Set.t,
			 refinements: (int * int -> bool) list ref}

      val all: t list ref = ref []

      fun fromTypes ts =
	 let val er = T {me = Set.singleton (R.make (Vector.toList ts,
						     Type.equals)),
			 refinements = ref []}
	 in List.push (all, er); er
	 end

      fun me (T {me, ...}) = Set.value me

      fun areEquivalent (er, i, j) = R.areEquivalent (me er, i, j)

      val classes = R.classes o me

      fun equals (T {me, ...}, T {me = me', ...}) = Set.equals (me, me')

      val layout = R.layout o me

      fun refine (T {refinements, ...}, f) = List.push (refinements, f)

      (* Relies on the fact that all unifications happen before the fixed point.
       *)
      fun unify (T {me, ...}, T {me = me', ...}) = Set.union (me, me')

      fun fixedPoint () =
	 (FixedPoint.fix'
	  (fn continue =>
	   List.foreach (!all, fn T {me, refinements} =>
			 let val r = Set.value me
			    val r' = List.fold (!refinements, r,
						fn (refinement, r) =>
						R.refine (r, refinement))
			 in if R.equals (r, r')
			       then ()
			    else (continue (); Set.setValue (me, r'))
			 end))
	  ; all := [])
   end


open Dec PrimExp Transfer

fun redundant (Program.T {datatypes, globals, functions, main}) =
   let
      val {get = funcInfo: Func.t -> {
				      arg: Eqrel.t,
				      return: Eqrel.t
				      },
	   set = setFuncInfo} =
	 Property.getSetOnce
	 (Func.plist, Property.initRaise ("Redundant.info", Func.layout))
      val {get = jumpInfo: Jump.t -> Eqrel.t,
	   set = setJumpInfo} =
	 Property.getSetOnce
	 (Jump.plist, Property.initRaise ("Redundant.info", Jump.layout))
      val {get = varInfo : Var.t -> {
				     index: int,
				     arg: Eqrel.t
				     } option,
	   set = setVarInfo} =
	 Property.getSetOnce (Var.plist, Property.initConst NONE)
      fun varEquiv (x, y) =
	 Var.equals (x, y)
	 orelse (case (varInfo x, varInfo y) of
		    (SOME {index = i, arg = r},
		     SOME {index = i', arg = r'}) =>
		    Eqrel.equals (r, r') andalso Eqrel.areEquivalent (r, i, i')
		   | _ => false)
      (* compute the fixed point *)
      val _ =
	 let
	    fun makeFormalsRel (xs: (Var.t * Type.t) vector): Eqrel.t =
	       let
		  val eqrel = Eqrel.fromTypes (Vector.map (xs, #2))
	       in Vector.foreachi (xs, fn (i, (x, _)) =>
				  setVarInfo (x, SOME {index = i, arg = eqrel}))
		  ; eqrel
	       end
	    (* initialize all varInfo and funcInfo *)
	    val _ =
	       Vector.foreach
	       (functions, fn Function.T {name, args, returns, ...} =>
		setFuncInfo (name, {
				    arg = makeFormalsRel args,
				    return = Eqrel.fromTypes returns
				    }))
	    (* Add the calls to all the funcInfos *)
	    val _ =
	       Vector.foreach
	       (functions, fn Function.T {name, body,...} =>
		let val {return, ...} = funcInfo name
		   val handleDec =
		      fn Fun {name, args, ...} =>
		      setJumpInfo (name, (makeFormalsRel args))
		       | _ => ()
		   val varEquiv =
		      fn vars => fn (i, j) => varEquiv (Vector.sub (vars, i),
							Vector.sub (vars, j))
		   val handleTransfer =
		      fn Call {func, args, cont} =>
		      let val {arg, return = return'} = funcInfo func
		      in Eqrel.refine (arg, varEquiv args)
			 ; Eqrel.unify (return',
					case cont of
					   NONE => return
					 | SOME c => jumpInfo c)
		      end
		       | Case {cases = Cases.Con cases, ...} =>
			    (* For now, assume that constructor arguments
			     * are never redundant.  Thus all case branches
			     * need to have trivial equivalence relations.
			     *)
			    Vector.foreach (cases, fn (_, j) =>
					    Eqrel.refine (jumpInfo j,
							  fn _ => false))
		       | Jump {dst, args, ...} =>
			    Eqrel.refine (jumpInfo dst, varEquiv args)
		       | Return xs => Eqrel.refine (return, varEquiv xs)
		       | _ => ()
		in Exp.foreach'
		   (body, {handleDec = handleDec,
			   handleTransfer = handleTransfer})
		end)
	    val _ = Eqrel.fixedPoint ()
	 in ()
	 end
      val _ = 
	 Control.diagnostics
	 (fn display =>
	  Vector.foreach
	  (functions, fn Function.T {name, ...} =>
	   display
	   (let val {arg, return} = funcInfo name
		open Layout
	    in seq [Func.layout name,
		    str "  ",
		    Eqrel.layout arg,
		    Eqrel.layout return]
	    end)))
      val {get = replacement : Var.t -> Var.t option, set = setReplacement} =
	 Property.getSetOnce (Var.plist, Property.initConst NONE)
      datatype red =
	 Useful
	| Redundant of int (* the index it is the same as *)

      (* Turn an equivalence relation on 0 ... n -1 into a list of length n,
       * by choosing a representative of each class.
       *)
      fun 'a makeReds (xs: 'a vector, r: Eqrel.t): red vector =
	 let
	    val classes = Eqrel.classes r
	    fun getRed i =
	       let
		  val rec loop =
		     fn [] => Useful
		      | class :: classes =>
			   case class of
			      [] => Error.bug "empty class"
			    | [_] => Error.bug "trivial class"
			    | j :: js =>
				 if i = j
				    then Useful
				 else if List.exists (js, fn j => i = j)
					 then Redundant j
				      else loop classes
	       in loop classes
	       end
	 in Vector.tabulate (Vector.length xs, getRed)
	 end
      fun redundantFormals (r: Eqrel.t, xs: (Var.t * Type.t) vector)
	 : red vector * (Var.t * Type.t) vector =
	 let
	    val reds = makeReds (xs, r)
	    val xs =
	       Vector.keepAllMap2
	       (xs, reds, fn (x, red) =>
		case red of
		   Useful => SOME x
		 | Redundant i =>
		      (setReplacement (#1 x, SOME (#1 (Vector.sub (xs, i))))
		       ; NONE))
	 in (reds, xs)
	 end
      fun keepUseful (reds: red vector, xs: 'a vector): 'a vector =
	 Vector.keepAllMap2 (reds, xs, fn (r, x) =>
			     case r of
				Useful => SOME x
			      | _ => NONE)
      val {get = funcReds : Func.t -> {args: red vector,
				       returns: red vector},
	   set = setFuncReds} =
	 Property.getSetOnce (Func.plist,
			      Property.initRaise ("reds", Func.layout))
      val functions =
	 Vector.map
	 (functions, fn Function.T {name, args, body, returns} =>
	  let
	     val {arg, return} = funcInfo name
	     val return = makeReds (returns, return)
	     val (arg, args) = redundantFormals (arg, args)
	     val _ = setFuncReds (name, {args = arg, returns = return})
	  in
	     Function.T {name = name, body = body,
			 args = args,
			 returns = keepUseful (return, returns)}
	  end)
      val {get = jumpReds : Jump.t -> red vector, set = setJumpReds} =
	 Property.getSetOnce (Jump.plist,
			      Property.initRaise ("red", Jump.layout))
      fun loopVar x =
	 case replacement x of
	    NONE => x
	  | SOME y => y
      fun loopVars xs = Vector.map (xs, loopVar)
      val functions =
	 Vector.map
	 (functions, fn Function.T {name, args, body, returns} =>
	  let
	     val {returns = returnReds, ...} = funcReds name
	     fun loopTransfer t =
		case t of
		   Bug => Bug
		 | Call {func, args, cont} =>
		      Call {func = func, cont = cont,
			    args = loopVars (keepUseful (#args (funcReds func),
							 args))}
		 | Case {cause, test, cases, default} =>
		      Case {cause = cause, test = loopVar test, cases = cases,
			    default = default}
		 | Jump {dst, args} =>
		      Jump {dst = dst,
			    args = loopVars (keepUseful (jumpReds dst, args))}
		 | Raise xs => Raise (loopVars xs)
		 | Return xs => Return (loopVars (keepUseful (returnReds, xs)))

	     fun loopExp e =
		let val {decs, transfer} = Exp.dest e
		in Exp.make {decs = List.map (decs, loopDec),
			     transfer = loopTransfer transfer}
		end
	     
	     and loopDec d =
		case d of
		   Bind {var, ty, exp} =>
		      Bind {var = var, ty = ty,
			    exp = PrimExp.replaceVar (exp, loopVar)}
		 | Fun {name, args, body} =>
		      let
			 val (reds, args) =
			    redundantFormals (jumpInfo name, args)
		      in setJumpReds (name, reds)
			 ; Fun {name = name, args = args, body = loopExp body}
		      end
		 | _ => d
	  in Function.T {name = name,
			 args = args,
			 returns = returns,
			 body = loopExp body}
	  end)
      val p = Program.T {datatypes = datatypes,
			 globals = globals,
			 functions = functions,
			 main = main}
      val _ = Program.clear p
   in
      p
   end

end
