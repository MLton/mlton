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
      val {get = value: Var.t -> 'a, set = setValue, ...} =
	 Property.getSetOnce
	 (Var.plist,
	  Property.initRaise ("analyze var value", Var.layout))
      val value = Trace.trace ("Analyze.value", Var.layout, layout) value
      fun values xs = Vector.map (xs, value)
      val {get = func, set = setFunc, ...} =
	 Property.getSetOnce
	 (Func.plist, Property.initRaise ("analyze func name", Func.layout))
      val {get = labelInfo, set = setLabelInfo, ...} =
	 Property.getSetOnce
	 (Label.plist, Property.initRaise ("analyze label", Label.layout))
      val labelArgs = #args o labelInfo
      fun loopArgs args =
	 Vector.map (args, fn (x, t) =>
		     let val v = fromType t
		     in setValue (x, v)
			; v
		     end)
      val _ =
	 List.foreach
	 (functions, fn f =>
	  let
	     val {name, args, returns, ...} = Function.dest f
	  in
	     setFunc (name, {args = loopArgs args,
			     returns = Vector.map (returns, fromType)})
	  end)
      val exnVals: 'a vector option ref = ref NONE
      fun getExnVals vs =
	 case !exnVals of
	    NONE => let val vs = Vector.map (vs, copy)
		    in exnVals := SOME vs; vs
		    end
	  | SOME vs => vs
      fun loopTransfer (t, shouldReturns): unit =
	(case t of
	    Bug => ()
	  | Call {func = f, args, return} =>
	       let
		  val {args = formals, returns} = func f
		  val shouldReturns =
		     case return of
			NONE => shouldReturns
		      | SOME {cont, ...} => labelArgs cont
	       in coerces (values args, formals)
		  ; coerces (returns, shouldReturns)
	       end
	  | Case {test, cases, default, ...} =>
	       let val test = value test
		  fun ensureNullary j =
		     if 0 = Vector.length (labelArgs j)
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
					   filter (test, c, labelArgs j))
		      | Int l => doit (l, filterInt)
		      | Word l => doit (l, filterWord)
		      | Word8 l => doit (l, filterWord8)
		  val _ = Option.app (default, ensureNullary)
	       in ()
	       end
	  | Goto {dst, args} => coerces (values args, labelArgs dst)
	  | Prim {prim, args, failure, success} =>
	       (coerces (Vector.new0 (), labelArgs failure)
		; coerce {from = primApp {prim = prim,
					  targs = Vector.new0 (),
					  args = values args,
					  resultType = Type.int},
			  to = Vector.sub (labelArgs success, 0)})
	  | Raise xs => let val vs = values xs
			in coerces (vs, getExnVals vs)
			end
	  | Return xs => coerces (values xs, shouldReturns))
	   handle exn => Error.bug ("loopTransfer:" ^ 
				    (Layout.toString (Transfer.layout t)) ^ ":" ^
				    (case exn
				       of Fail msg => msg
					| _ => ""))
      val loopTransfer =
	 Trace.trace2
	 ("Analyze.loopTransfer",
	  Transfer.layout, Layout.ignore, Layout.ignore) loopTransfer
      fun loopStatement (s as Statement.T {var, exp, ty}): unit =
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
		| Select {tuple, offset} =>
		     select {tuple = value tuple,
			     offset = offset,
			     resultType = ty}
		| SetExnStackLocal => unit
		| SetExnStackSlot => unit
		| SetSlotExnStack => unit
		| SetHandler h =>
		     let
			val vs = labelArgs h
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
	     else setValue (var, v))
	 end
         handle exn => Error.bug ("loopStatement:" ^ 
				  (Layout.toString (Statement.layout s)) ^ ":" ^
				  (case exn
				     of Fail msg => msg
				      | _ => ""))
      val _ = coerces (Vector.new0 (), #args (func main))
      val _ = Vector.foreach (globals, loopStatement)
      val _ =
	 List.foreach
	 (functions, fn f =>
	  let
	     val {name, blocks, start, ...} = Function.dest f
	     val _ =
		Vector.foreach
		(blocks, fn b as Block.T {label, args, ...} =>
		 setLabelInfo (label, {args = loopArgs args,
				       block = b,
				       visited = ref false}))
	     val returns = #returns (func name)
	     fun visit (l: Label.t) =
		let
		   val {block, visited, ...} = labelInfo l
		in
		   if !visited
		      then ()
		   else
		      let
			 val _ = visited := true
			 val Block.T {statements, transfer, ...} = block
		      in
			 Vector.foreach (statements, loopStatement)
			 ; loopTransfer (transfer, returns)
			 ; Transfer.foreachLabel (transfer, visit)
		      end
		end
	     val _ = visit start
	  in
	     ()
	  end)
   in {
       value = value,
       func = func,
       label = labelArgs,
       exnVals = !exnVals
       }
   end

end
