(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor TypeCheck (S: TYPE_CHECK_STRUCTS): TYPE_CHECK = 
struct

open S
open Dec Exp PrimExp Transfer
type int = Int.t

fun equalss (ts, ts') = List.equals (ts, ts', Type.equals)

fun checkScopes (program as
		 Program.T {datatypes, globals, functions, main}): unit =
   let
      datatype status =
	 Undefined
       | InScope
       | Defined

      fun make (layout, plist) =
	 let
	    val {get, set} =
	       Property.getSet (plist, Property.initConst Undefined)
	    fun bind x =
	       case get x of
		  Undefined => set (x, InScope)
		| _ => Error.bug ("duplicate definition of "
				  ^ (Layout.toString (layout x)))
	    fun reference x =
	       case get x of
		  InScope => ()
		| _ => Error.bug (concat
				  ["reference to ",
				   Layout.toString (layout x),
				   " not in scope"])

	    fun unbind x = set (x, Defined)
	 in (bind, reference, unbind)
	 end

      val (bindCon, getCon, _) = make (Con.layout, Con.plist)
      val (bindVar, getVar, unbindVar) = make (Var.layout, Var.plist)
      val (bindFunc, getFunc, _) = make (Func.layout, Func.plist)
      val (bindJump, getJump, unbindJump) = make (Jump.layout, Jump.plist)
      fun getVars xs = Vector.foreach (xs, getVar)
      val jumpHandlers = inferHandlers program
      val loopPrimExp =
	 fn Const _ => ()
	  | Var x => getVar x
	  | Tuple xs => Vector.foreach (xs, getVar)
	  | Select {tuple, ...} => getVar tuple
	  | ConApp {con, args, ...} => (getCon con
					; Vector.foreach (args, getVar))
	  | PrimApp {args, ...} => Vector.foreach (args, getVar)
      val loopTransfer =
	 fn Bug => ()
	  | Call {func, args, cont} => (getFunc func
					; getVars args
					; (case cont of
					      NONE => ()
					    | SOME c => getJump c))
	  | Case {test, cases, default, ...} =>
	       (getVar test
		; Cases.foreach' (cases, getJump, getCon)
		; Option.app (default, getJump))
	  | Jump {dst, args} => (getJump dst; getVars args)
	  | Raise xs => getVars xs
	  | Return xs => getVars xs
      fun loopBind {var, ty, exp} = (loopPrimExp exp; bindVar var)
      fun loopFunc (args: (Var.t * Type.t) vector, body) =
	 (Vector.foreach (args, bindVar o #1)
	  ; loopExp body
	  ; Vector.foreach (args, unbindVar o #1))
      and loopExp e =
	 let val {decs, transfer} = Exp.dest e
	 in List.foreach (decs, 
			  fn Bind b => loopBind b
			   | Fun {name, args, body} =>
				((* The bindJump must be before the List.foreach
				  * because a handler has itself on the top of
				  * the handler stack at its entry.
				  *)
				 bindJump name
				 (* But, we're not checking that handlers are
				  * defined before their use -- the only
				  * reason was to propery compute liveness
				  * in live.fun.  That is no being handled differently.
				  *)
(*				 ; List.foreach (jumpHandlers name, getJump) *)
				 ; loopFunc (args, body))
			   | HandlerPush h => getJump h
			   | HandlerPop => ())
	    ; loopTransfer transfer
	    ; List.foreach (decs, 
			    fn Bind {var, ...} => unbindVar var
			     | Fun {name, ...} => unbindJump name
			     | HandlerPush _ => ()
			     | HandlerPop => ())
	 end
      val _ =
	 Vector.foreach (datatypes, fn {tycon, cons} =>
			 Vector.foreach (cons, bindCon o #con))
      val _ = Vector.foreach (globals, loopBind)
      val _ = Vector.foreach (functions, fn Function.T {name, ...} =>
			      bindFunc name)
      val _ = Vector.foreach (functions, fn Function.T {args, body, ...} =>
			      loopFunc (args, body))
      val _ = getFunc main
      val _ = Program.clear program
   in ()
   end

val checkScopes = Control.trace (Control.Pass, "checkScopes") checkScopes
   
fun typeCheck (program as Program.T {datatypes, functions, ...}): unit =
   let
      val _ = checkScopes program
      val out = Out.error
      val print = Out.outputc out
      exception TypeError
      fun error (msg, lay) =
	 (print ("Type error: " ^ msg ^ "\n")
	  ; Layout.output (lay, out)
	  ; print "\n"
	  ; raise TypeError)
      fun coerce {from: Type.t, to: Type.t}: unit =
	 if Type.equals (from, to)
	    then ()
	 else error ("Type.equals",
		     Layout.record [("from", Type.layout from),
				    ("to", Type.layout to)])
      fun coerces (from, to) =
	 Vector.foreach2 (from, to, fn (from, to) =>
			 coerce {from = from, to = to})
      val error = fn s => error (s, Layout.empty)
      val coerce =
	 Trace.trace ("TypeCheck.coerce",
		      fn {from, to} => let open Layout
				       in record [("from", Type.layout from),
						  ("to", Type.layout to)]
				       end,
				    Unit.layout) coerce
      fun select {tuple: Type.t, offset: int, resultType}: Type.t =
	 case Type.detupleOpt tuple of
	    NONE => error "select of non tuple"
	  | SOME ts => Vector.sub (ts, offset)
      val {get = conInfo: Con.t -> {args: Type.t vector,
				    result: Type.t},
	   set = setConInfo, destroy = destroyCon} =
	 Property.destGetSetOnce
	 (Con.plist,
	  Property.initRaise ("TypeCheck.info", Con.layout))
      val _ =
	 Vector.foreach
	 (datatypes, fn {tycon, cons} =>
	  let val result = Type.con (tycon, Vector.new0 ())
	  in Vector.foreach
	     (cons, fn {con, args} =>
	      setConInfo (con, {args = args,
				result = result}))
	  end)
      fun conApp {con, args} =
	 let
	    val {args = args', result, ...} = conInfo con
	    val _ = coerces (args', args)
	 in
	    result
	 end
      fun filter (test, con, args) =
	 let
	    val {result, args = args'} = conInfo con
	    val _ = coerce {from = test, to = result}
	    val _ = coerces (args', args)
	 in ()
	 end
      fun filterGround to (t: Type.t): unit = coerce {from = t, to = to}
      fun primApp {prim, targs, args, resultVar, resultType} =
	 case Prim.checkApp {prim = prim,
			     targs = targs,
			     args = args,
			     con = Type.con,
			     equals = Type.equals,
			     dearrowOpt = Type.dearrowOpt,
			     detupleOpt = Type.detupleOpt,
			     isUnit = Type.isUnit
			     } of
	    NONE => error "bad primapp"
	  | SOME t => t
      val primApp =
	 Trace.trace ("checkPrimApp",
		      fn {prim, targs, args, ...} =>
		      let open Layout
		      in record [("prim", Prim.layout prim),
				 ("targs", Vector.layout Type.layout targs),
				 ("args", Vector.layout Type.layout args)]
		      end,
		      Type.layout) primApp
      val {value = varType, ...} =
	 analyze {
		  coerce = coerce,
		  conApp = conApp,
		  const = Type.ofConst,
		  copy = fn x => x,
		  filter = filter,
		  filterChar = filterGround Type.char,
		  filterInt = filterGround Type.int,
		  filterWord = filterGround Type.word,
		  filterWord8 = filterGround Type.word8,
		  fromType = fn x => x,
		  layout = Type.layout,
		  primApp = primApp,
		  program = program,
		  select = select,
		  tuple = Type.tuple,
		  useFromTypeOnBinds = true
		  }
	 handle _ => error "analyze raised an exception"
      val _ = destroyCon ()
   in
      ()
   end

val typeCheck =
   Trace.trace ("Cps.typeCheck", Program.layout, Layout.ignore)
   typeCheck

end
