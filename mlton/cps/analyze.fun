(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Analyze (S: ANALYZE_STRUCTS): ANALYZE = 
struct

open S
open Dec PrimExp Transfer

fun 'a analyze
   {coerce, conApp, const, copy,
    filter, filterChar, filterInt, filterWord, filterWord8,
    fromType, layout, primApp,
    program = Program.T {main, datatypes, globals, functions},
    select, tuple, useFromTypeOnBinds} =
   let
      fun coerces (from, to) =
	 Vector.foreach2 (from, to, fn (from, to) =>
			 coerce {from = from, to = to})
      val {get = value: Var.t -> 'a, set = setValue} =
	 Property.getSetOnce
	 (Var.plist,
	  Property.initRaise ("analyze var value", Var.layout))
      val value = Trace.trace ("Analyze.value", Var.layout, layout) value
      fun values xs = Vector.map (xs, value)
      val {get = func, set = setFunc} =
	 Property.getSetOnce
	 (Func.plist, Property.initRaise ("analyze func name", Func.layout))
      val {get = jump, set = setJump} =
	 Property.getSetOnce
	 (Jump.plist, Property.initRaise ("analyze jump", Jump.layout))
      fun loopArgs args =
	 Vector.map (args, fn (x, t) =>
		    let val v = fromType t
		    in setValue (x, v)
		       ; v
		    end)
      val _ =
	 Vector.foreach
	 (functions, fn {name, args, returns, ...} =>
	  setFunc (name, {args = loopArgs args,
			  returns = Vector.map (returns, fromType)}))
      val exnVals: 'a vector option ref = ref NONE
      fun getExnVals vs =
	 case !exnVals of
	    NONE => let val vs = Vector.map (vs, copy)
		    in exnVals := SOME vs; vs
		    end
	  | SOME vs => vs
      fun loopTransfer (t, shouldReturns): unit =
	 case t of
	    Bug => ()
	  | Call {func = f, args, cont = c} =>
	       let
		  val {args = formals, returns} = func f
		  val shouldReturns =
		     case c of
			NONE => shouldReturns
		      | SOME c => jump c
	       in coerces (values args, formals)
		  ; coerces (returns, shouldReturns)
	       end
	  | Case {test, cases, default, ...} =>
	       let val test = value test
		  fun ensureNullary j =
		     if 0 = Vector.length (jump j)
			then ()
		     else Error.bug (concat [Jump.toString j,
					     " must be nullary"])
		  fun doit (l, filter) =
		     (filter test
		      ; Vector.foreach (l, fn (_, j) => ensureNullary j))
		  datatype z = datatype Cases.t
		  val _ =
		     case cases of
			Char l => doit (l, filterChar)
		      | Con cases =>
			   Vector.foreach (cases, fn (c, j) =>
					   filter (test, c, jump j))
		      | Int l => doit (l, filterInt)
		      | Word l => doit (l, filterWord)
		      | Word8 l => doit (l, filterWord8)
		  val _ = Option.app (default, ensureNullary)
	       in ()
	       end
	  | Jump {dst, args} => coerces (values args, jump dst)
	  | Raise xs => let val vs = values xs
			in coerces (vs, getExnVals vs)
			end
	  | Return xs => coerces (values xs, shouldReturns)
      val loopTransfer =
	 Trace.trace2
	 ("Analyze.loopTransfer",
	  Transfer.layout, Layout.ignore, Layout.ignore) loopTransfer
      fun loopPrimExp {var: Var.t, ty: Type.t, exp: PrimExp.t}: 'a =
	 case exp of
	    Const c => const c
	  | Var x => value x
	  | Tuple xs =>
	       if 1 = Vector.length xs
		  then Error.bug "unary tuple"
	       else tuple (values xs)
	  | Select {tuple, offset} =>
	       select {tuple = value tuple,
		       offset = offset,
		       resultType = ty}
	  | ConApp {con, args} => conApp {con = con, args = values args}
	  | PrimApp {prim, info, targs, args} =>
	       let
		  datatype z = datatype PrimInfo.t
		  val _ =
		     case info of
			None => ()
		      | Overflow j => coerces (Vector.new0 (), jump j)
	       in
		  primApp {prim = prim,
			   targs = targs,
			   args = values args,
			   resultVar = var,
			   resultType = ty}
	       end
      val loopPrimExp =
	 Trace.trace ("Analyze.loopPrimExp",
		      fn {var, ty, exp, ...} =>
		      Layout.record [("var", Var.layout var),
				     ("ty", Type.layout ty),
				     ("exp", PrimExp.layout exp)],
		      Layout.ignore)
	 loopPrimExp
      fun loopBind {var, exp, ty}: unit =
	 setValue (var,
		   let val v = loopPrimExp {var = var, ty = ty, exp = exp}
		   in if useFromTypeOnBinds
			 then let val v' = fromType ty
			      in coerce {from = v, to = v'}
				 ; v'
			      end
		      else v
		   end)
      fun loopExp (e: Exp.t, returns): unit =
	 let val {decs, transfer, ...} = Exp.dest e
	 in loopDecs (decs, returns)
	    ; loopTransfer (transfer, returns)
	 end
      and loopDecs (ds, returns): unit =
	 List.foreach
	 (ds,
	  Trace.trace ("Analyze.loopDec", Dec.layout, Unit.layout)
	  (fn Bind b => loopBind b
	| Fun {name, args, body} =>
	     (setJump (name, loopArgs args)
	      ; loopExp (body, returns))
	| HandlerPush h => let val vs = jump h
			   in coerces (getExnVals vs, vs)
			   end
	| _ => ())
	  )
      val _ = coerces (Vector.new0 (), #args (func main))
      val _ = Vector.foreach (globals, loopBind)
      val _ = Vector.foreach (functions, fn {name, body, ...} =>
			      loopExp (body, #returns (func name)))
   in {
       value = value,
       func = func,
       jump = jump,
       exnVals = !exnVals
       }
   end

end
