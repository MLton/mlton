(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Analyze (S: ANALYZE_STRUCTS): ANALYZE = 
struct

open S
datatype z = datatype Exp.t
datatype z = datatype Transfer.t
   
fun 'a analyze
   {coerce, conApp, const, copy,
    filter, filterChar, filterInt, filterWord, filterWord8,
    fromType, layout, primApp,
    program = Program.T {main, datatypes, globals, functions},
    select, tuple, useFromTypeOnBinds} =
   let
      val unit = fromType Type.unit
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
      val {get = label, set = setLabel} =
	 Property.getSetOnce
	 (Label.plist, Property.initRaise ("analyze label", Label.layout))
      fun loopArgs args =
	 Vector.map (args, fn (x, t) =>
		    let val v = fromType t
		    in setValue (x, v)
		       ; v
		    end)
      val _ =
	 Vector.foreach
	 (functions, fn Function.T {name, args, returns, ...} =>
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
	  | Call {func = f, args, return} =>
	       let
		  val {args = formals, returns} = func f
		  val shouldReturns =
		     case return of
			NONE => shouldReturns
		      | SOME l => label l
	       in coerces (values args, formals)
		  ; coerces (returns, shouldReturns)
	       end
	  | Case {test, cases, default, ...} =>
	       let val test = value test
		  fun ensureNullary j =
		     if 0 = Vector.length (label j)
			then ()
		     else Error.bug (concat [Label.toString j,
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
					   filter (test, c, label j))
		      | Int l => doit (l, filterInt)
		      | Word l => doit (l, filterWord)
		      | Word8 l => doit (l, filterWord8)
		  val _ = Option.app (default, ensureNullary)
	       in ()
	       end
	  | Goto {dst, args} => coerces (values args, label dst)
	  | Prim {prim, args, failure, success} =>
	       (coerces (Vector.new0 (), label failure)
		; coerce {from = primApp {prim = prim,
					  targs = Vector.new0 (),
					  args = values args,
					  resultType = Type.int},
			  to = Vector.sub (label success, 0)})
	  | Raise xs => let val vs = values xs
			in coerces (vs, getExnVals vs)
			end
	  | Return xs => coerces (values xs, shouldReturns)
      val loopTransfer =
	 Trace.trace2
	 ("Analyze.loopTransfer",
	  Transfer.layout, Layout.ignore, Layout.ignore) loopTransfer
      fun loopStatement (Statement.T {var, exp, ty}): unit =
	 let
	    val v =
	       case exp of
		  ConApp {con, args} => conApp {con = con, args = values args}
		| Const c => const c
		| PrimApp {prim, targs, args, ...} =>
		     primApp {prim = prim,
			      targs = targs,
			      args = values args,
			      resultType = ty}
		| RestoreExnStack => unit
		| SaveExnStack => unit
		| Select {tuple, offset} =>
		     select {tuple = value tuple,
			     offset = offset,
			     resultType = ty}
		| SetHandler h =>
		     let
			val vs = label h
			val _ = coerces (getExnVals vs, vs)
		     in
			unit
		     end
		| Tuple xs =>
		     if 1 = Vector.length xs
			then Error.bug "unary tuple"
		     else tuple (values xs)
		| Var x => value x
	 in
	    Option.app
	    (var, fn var =>
	     if useFromTypeOnBinds
		then let
			val v' = fromType ty
			val _ = coerce {from = v, to = v'}
			val _ = setValue (var, v')
		     in
			()
		     end
	     else ())
	 end
      val _ = coerces (Vector.new0 (), #args (func main))
      val _ = Vector.foreach (globals, loopStatement)
      val _ =
	 Vector.foreach
	 (functions, fn Function.T {name, blocks, ...} =>
	  let
	     val _ =
		Vector.foreach
		(blocks, fn Block.T {label, args, ...} =>
		 setLabel (label, loopArgs args))
	     val returns = #returns (func name)
	     val _ =
		Vector.foreach
		(blocks, fn Block.T {label, args, statements, transfer} =>
		 (Vector.foreach (statements, loopStatement)
		  ; loopTransfer (transfer, returns)))
	  in
	     ()
	  end)
   in {
       value = value,
       func = func,
       label = label,
       exnVals = !exnVals
       }
   end

end
