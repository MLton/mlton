(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor SsaTree (S: SSA_TREE_STRUCTS): SSA_TREE = 
struct

open S

structure SourceInfo =
   struct
      datatype t =
	 Bogus
       | Main
       | PolyEqual
       | Region of Region.t

      val bogus = Bogus
      val fromRegion = Region
      val main = Main
      val polyEqual = PolyEqual

      val toString =
	 fn Bogus => "<unknown>"
	  | Main => "<main>"
	  | PolyEqual => "<poly equal>"
	  | Region r =>
	       (case Region.left r of
		   NONE => "<unknown>"
		 | SOME (SourcePos.T {file, line, ...}) =>
		      concat [file, ":", Int.toString line])

      val layout = Layout.str o toString
   end

structure Type =
   struct
      local structure T = HashType (S)
      in open  T
      end

      fun tyconArgs t =
	 case Dest.dest t of
	    Dest.Con x => x
	  | _ => Error.bug "FirstOrderType.tyconArgs"
	       
      datatype dest =
	  Array of t
	| Char
	| Datatype of Tycon.t
	| Int
	| IntInf
	| Pointer
	| PreThread
	| Real
	| Ref of t
	| Thread
	| Tuple of t vector
	| Vector of t
	| Word
	| Word8

      local
	 val {get, set, ...} =
	    Property.getSetOnce (Tycon.plist, Property.initConst NONE)

	 fun nullary c v =
	    if Vector.isEmpty v
	       then c
	    else Error.bug "bogus application of nullary tycon"

	 fun unary make v =
	    if 1 = Vector.length v
	       then make (Vector.sub (v, 0))
	    else Error.bug "bogus application of unary tycon"

	 val tycons =
	    [(Tycon.tuple, Tuple),
	     (Tycon.char, nullary Char),
	     (Tycon.int, nullary Int),
	     (Tycon.intInf, nullary IntInf),
	     (Tycon.pointer, nullary Pointer),
	     (Tycon.preThread, nullary PreThread),
	     (Tycon.real, nullary Real),
	     (Tycon.thread, nullary Thread),
	     (Tycon.word8, nullary Word8),
	     (Tycon.word, nullary Word),
	     (Tycon.array, unary Array),
	     (Tycon.vector, unary Vector),
	     (Tycon.reff, unary Ref)]
      in
	 val _ = List.foreach (tycons, fn (tycon, f) => set (tycon, SOME f))

	 fun dest t =
	    case Dest.dest t of
	       Dest.Con (tycon, ts) =>
		  (case get tycon of
		      NONE => Datatype tycon
		    | SOME f => f ts)
	     | _ => Error.bug "dest"
      end

      local
	 open Layout
      in
	 val {get = layout, ...} =
	    Property.get
	    (plist,
	     Property.initRec
	     (fn (t, layout) =>
	      case dest t of
		 Array t => seq [layout t, str " array"]
	       | Char => str "char"
	       | Datatype t => Tycon.layout t
	       | Int => str "int"
	       | IntInf => str "IntInf.int"
	       | Pointer => str "pointer"
	       | PreThread => str "preThread"
	       | Real => str "real"
	       | Ref t => seq [layout t, str " ref"]
	       | Thread => str "thread"
	       | Tuple ts =>
		    if Vector.isEmpty ts
		       then str "unit"
		    else paren (seq (separate (Vector.toListMap (ts, layout),
					       " * ")))
	       | Vector t => seq [layout t, str " vector"]
	       | Word => str "word"
	       | Word8 => str "word8"))
      end
   end

structure Func =
   struct
      open Var (* Id (structure AstId = Ast.Var) *)

      fun newNoname () = newString "F"
   end

structure Label =
   struct
      open Func
      fun newNoname () = newString "L"
   end

structure Cases = Cases (type con = Con.t
			 val conEquals = Con.equals)

local open Layout
in
   fun layoutTuple xs = Vector.layout Var.layout xs
end

structure Var =
   struct
      open Var

      fun pretty (x, global) =
	 case global x of
	    NONE => toString x
	  | SOME s => s

      fun prettys (xs: Var.t vector, global: Var.t -> string option) =
	 Layout.toString (Vector.layout
			  (fn x =>
			   case global x of
			      NONE => layout x
			    | SOME s => Layout.str s)
			  xs)
   end

structure Exp =
   struct
      datatype t =
	 ConApp of {con: Con.t,
		    args: Var.t vector}
       | Const of Const.t
       | HandlerPop of Label.t
       | HandlerPush of Label.t
       | PrimApp of {prim: Prim.t,
		     targs: Type.t vector,
		     args: Var.t vector}
       | Select of {tuple: Var.t,
		    offset: int}
       | SetExnStackLocal
       | SetExnStackSlot
       | SetSlotExnStack
       | SetHandler of Label.t
       | Tuple of Var.t vector
       | Var of Var.t

      val unit = Tuple (Vector.new0 ())
	 
      fun foreachLabelVar (e, j, v) =
	 let
	    fun vs xs = Vector.foreach (xs, v)
	 in
	    case e of
	       ConApp {args, ...} => vs args
	     | Const _ => ()
	     | HandlerPop l => j l
	     | HandlerPush l => j l
	     | PrimApp {args, ...} => vs args
	     | Select {tuple, ...} => v tuple
	     | SetExnStackLocal => ()
	     | SetExnStackSlot => ()
	     | SetSlotExnStack => ()
	     | SetHandler h => j h
	     | Tuple xs => vs xs
	     | Var x => v x
	 end

      fun foreachLabel (e, j) = foreachLabelVar (e, j, fn _ => ())
      fun foreachVar (e, v) = foreachLabelVar (e, fn _ => (), v)

      fun replaceLabelVar (e, fl, fx) =
	 let
	    fun fxs xs = Vector.map (xs, fx)
	 in
	    case e of
	       ConApp {con, args} => ConApp {con = con, args = fxs args}
	     | Const _ => e
	     | HandlerPop l => HandlerPop (fl l)
	     | HandlerPush l => HandlerPush (fl l)
	     | PrimApp {prim, targs, args} =>
		  PrimApp {prim = prim, targs = targs, args = fxs args}
	     | Select {tuple, offset} =>
		  Select {tuple = fx tuple, offset = offset}
	     | SetExnStackLocal => e
	     | SetExnStackSlot => e
	     | SetHandler h => SetHandler (fl h)
	     | SetSlotExnStack => e
	     | Tuple xs => Tuple (fxs xs)
	     | Var x => Var (fx x)
	 end

      fun replaceVar (e, f) = replaceLabelVar (e, fn l => l, f)
      fun replaceLabel (e, f) = replaceLabelVar (e, f, fn x => x)

      fun layout e =
	 let
	    open Layout
	 in
	    case e of
	       ConApp {con, args} =>
		  seq [Con.layout con, str " ", layoutTuple args]
	     | Const c => Const.layout c
	     | HandlerPop l => seq [str "HandlerPop ", Label.layout l]
	     | HandlerPush l => seq [str "HandlerPush ", Label.layout l]
	     | PrimApp {prim, targs, args} =>
		  seq [Prim.layout prim,
		       if !Control.showTypes
			  then if 0 = Vector.length targs
				  then empty
			       else Vector.layout Type.layout targs
		       else empty,
		       if isSome (Prim.numArgs prim)
			  then seq [str " ", layoutTuple args]
		       else empty]
	     | Select {tuple, offset} =>
		  seq [str "#", Int.layout (offset + 1), str " ",
		       Var.layout tuple]
	     | SetExnStackLocal => str "SetExnStackLocal"
	     | SetExnStackSlot => str "SetExnStackSlot"
	     | SetHandler h => seq [str "SetHandler ", Label.layout h]
	     | SetSlotExnStack => str "SetSlotExnStack"
	     | Tuple xs => layoutTuple xs
	     | Var x => Var.layout x
	 end

      val isFunctional =
	 fn ConApp _ => true
	  | Const _ => true
	  | HandlerPop _ => false
	  | HandlerPush _ => false
	  | PrimApp {prim, ...} => Prim.isFunctional prim
	  | Select _ => true
	  | SetExnStackLocal => false
	  | SetExnStackSlot => false
	  | SetHandler _ => false
	  | SetSlotExnStack => false
	  | Tuple _ => true
	  | Var _ => true
	       
      fun maySideEffect (e: t): bool =
	 case e of
	    ConApp _ => false
	  | Const _ => false
	  | HandlerPop _ => true
	  | HandlerPush _ => true
	  | PrimApp {prim,...} => Prim.maySideEffect prim
	  | Select _ => false
	  | SetExnStackLocal => true
	  | SetExnStackSlot => true
	  | SetHandler _ => true
	  | SetSlotExnStack => true
	  | Tuple _ => false
	  | Var _ => false

      fun varsEquals (xs, xs') = Vector.equals (xs, xs', Var.equals)

      fun equals (e: t, e': t): bool =
	 case (e, e') of
	    (ConApp {con, args}, ConApp {con = con', args = args'}) =>
	       Con.equals (con, con') andalso varsEquals (args, args')
	  | (Const c, Const c') => Const.equals (c, c')
	  | (HandlerPop l, HandlerPop l') => Label.equals (l, l')
	  | (HandlerPush l, HandlerPush l') => Label.equals (l, l')
	  | (PrimApp {prim, args, ...}, PrimApp {prim = prim', args = args', ...}) =>
	       Prim.equals (prim, prim') andalso varsEquals (args, args')
	  | (Select {tuple, offset}, Select {tuple = tuple', offset = offset'}) =>
	       Var.equals (tuple, tuple') andalso offset = offset'
	  | (SetExnStackLocal, SetExnStackLocal) => true
	  | (SetExnStackSlot, SetExnStackslot) => true
	  | (SetHandler l, SetHandler l') => Label.equals (l, l')
	  | (SetSlotExnStack, SetSlotExnStack) => true
	  | (Tuple xs, Tuple xs') => varsEquals (xs, xs')
	  | (Var x, Var x') => Var.equals (x, x')
	  | _ => false

      local
	 val newHash = Random.word
	 val conApp = newHash ()
	 val handlerPop = newHash ()
	 val handlerPush = newHash ()
	 val primApp = newHash ()
	 val select = newHash ()
	 val setExnStackLocal = newHash ()
	 val setExnStackSlot = newHash ()
	 val setHandler = newHash ()
	 val setSlotExnStack = newHash ()
	 val tuple = newHash ()
	 fun hashVars (xs: Var.t vector, w: Word.t): Word.t =
	    Vector.fold (xs, w, fn (x, w) => Word.xorb (w, Var.hash x))
      in
	 val hash: t -> Word.t =
	    fn ConApp {con, args, ...} => hashVars (args, Con.hash con)
	     | Const c => Const.hash c
	     | HandlerPop l => Word.xorb (handlerPop, Label.hash l)
	     | HandlerPush l => Word.xorb (handlerPush, Label.hash l)
	     | PrimApp {args, ...} => hashVars (args, primApp)
	     | Select {tuple, offset} =>
		  Word.xorb (select, Var.hash tuple + Word.fromInt offset)
	     | SetExnStackLocal => setExnStackLocal
	     | SetExnStackSlot => setExnStackSlot
	     | SetHandler h => Word.xorb (Label.hash h, setHandler)
	     | SetSlotExnStack => setSlotExnStack
	     | Tuple xs => hashVars (xs, tuple)
	     | Var x => Var.hash x
      end

      val hash = Trace.trace ("Exp.hash", layout, Word.layout) hash

      val toString = Layout.toString o layout

      fun toPretty (e: t, global: Var.t -> string option): string =
	 case e of
	    ConApp {con, args} =>
	       concat [Con.toString con, " ", Var.prettys (args, global)]
	  | Const c => Const.toString c
	  | HandlerPop l => concat ["HandlerPop ", Label.toString l]
	  | HandlerPush l => concat ["HandlerPush ", Label.toString l]
	  | PrimApp {prim, args, ...} =>
	       Layout.toString
	       (Prim.layoutApp (prim, args, fn x =>
				case global x of
				   NONE => Var.layout x
				 | SOME s => Layout.str s))
	  | SetExnStackLocal => "SetExnStackLocal"
	  | SetExnStackSlot => "SetExnStackSlot"
	  | SetSlotExnStack => "SetSlotExnStack"
	  | Select {tuple, offset} =>
	       concat ["#", Int.toString (offset + 1), " ", Var.toString tuple]
	  | SetHandler h => concat ["SetHandler ", Label.toString h]
	  | Tuple xs => Var.prettys (xs, global)
	  | Var x => Var.toString x
   end
datatype z = datatype Exp.t

structure Statement =
   struct
      datatype t = T of {var: Var.t option,
			 ty: Type.t,
			 exp: Exp.t}

      local
	 fun make f (T r) = f r
      in
	 val var = make #var
	 val ty = make #ty
	 val exp = make #exp
      end

      fun layout (T {var, ty, exp}) =
	 let
	    open Layout
	 in
	    seq [seq [case var of
			 NONE => empty
		       | SOME var =>
			    seq [Var.layout var,
				 if !Control.showTypes
				    then seq [str ": ", Type.layout ty]
				 else empty,
				 str " = "]],
                 Exp.layout exp]
	 end

      local
	 fun make (e: Exp.t) =
	    T {var = NONE,
	       ty = Type.unit,
	       exp = e}
      in
	 val setExnStackLocal = make Exp.SetExnStackLocal
	 val setExnStackSlot = make Exp.SetExnStackSlot
	 val setSlotExnStack = make Exp.SetSlotExnStack
	 fun setHandler h = make (Exp.SetHandler h)
	 fun handlerPop h = make (Exp.HandlerPop h)
	 fun handlerPush h = make (Exp.HandlerPush h)
      end

      fun clear s = Option.app (var s, Var.clear)

      fun prettifyGlobals (v: t vector): Var.t -> string option =
	 let
	    val {get = global: Var.t -> string option, set = setGlobal, ...} =
	       Property.getSet (Var.plist, Property.initConst NONE)
	    val _ = 
	       Vector.foreach
	       (v, fn T {var, exp, ...} =>
		Option.app
		(var, fn var =>
		 let
		    fun set s =
		       let
			  val maxSize = 10
			  val s = 
			     if String.size s > maxSize
				then concat [String.prefix (s, maxSize), "..."]
			     else s
		       in
			  setGlobal (var, SOME s)
		       end
		 in
		    case exp of
		       Const c => set (Layout.toString (Const.layout c))
		     | ConApp {con, args, ...} =>
			  if Vector.isEmpty args
			     then set (Con.toString con)
			  else set (concat [Con.toString con, "(...)"])
		     | _ => ()
		 end))
	 in
	    global
	 end
   end

structure Handler =
   struct
      datatype t =
	 CallerHandler
       | Handle of Label.t
       | None

      fun layout h =
	 let
	    open Layout
	 in
	    case h of
	       CallerHandler => str "CallerHandler"
	     | Handle l => seq [str "Handle ", Label.layout l]
	     | None => str "None"
	 end

      val equals =
	 fn (CallerHandler, CallerHandler) => true
	  | (None, None) => true
	  | (Handle l, Handle l') => Label.equals (l, l')
	  | _ => false

      local
	 val newHash = Random.word
	 val callerHandler = newHash ()
	 val handlee = newHash ()
	 val none = newHash ()
      in
	 val hash: t -> Word.t =
	    fn CallerHandler => callerHandler
	     | Handle l => Label.hash l
	     | None => none
      end

      fun foldLabel (h, a, f) =
	 case h of
	    Handle l => f (l, a)
	  | _ => a

      fun foreachLabel (h, f) = foldLabel (h, (), f o #1)

      fun map (h, f) =
	 case h of
	    Handle l => Handle (f l)
	  | _ => h
   end

structure Return =
   struct
      datatype t =
	 Dead
       | HandleOnly
       | NonTail of {cont: Label.t,
		     handler: Handler.t}
       | Tail

      val layout =
	 let
	    open Layout
	 in
	    fn Dead => str "Dead"
	     | HandleOnly => str "HandleOnly"
	     | NonTail {cont, handler} =>
		  seq [str "NonTail ",
		       record [("cont", Label.layout cont),
			       ("handler", Handler.layout handler)]]
	     | Tail => str "Tail"
	 end

      val isNonTail = fn NonTail _ => true | _ => false
	 
      val equals =
	 fn (Dead, Dead) => true
	  | (HandleOnly, HandleOnly) => true
	  | (NonTail {cont, handler}, 
	     NonTail {cont = cont', handler = handler'}) =>
	       Label.equals (cont, cont') andalso 
	       Handler.equals (handler, handler')
	  | (Tail, Tail) => true
	  | _ => false

      local
	 val newHash = Random.word
	 val dead = newHash ()
	 val handleOnly = newHash ()
	 val nonTail = newHash ()
	 val tail = newHash ()
	 fun hash2 (w1: Word.t, w2: Word.t) = Word.xorb (w1, w2)
      in
	 val hash: t -> Word.t =
	    fn Dead => dead
	     | HandleOnly => handleOnly
	     | NonTail {cont, handler} =>
	          hash2 (Label.hash cont, Handler.hash handler)
	     | Tail => tail
      end

      fun foreachHandler (r, f) =
	 case r of
	    NonTail {handler, ...} => Handler.foreachLabel (handler, f)
	  | _ => ()

      fun foldLabel (r, a, f) =
	 case r of
	    NonTail {cont, handler} =>
	       f (cont, Handler.foldLabel (handler, a, f))
	  | _ => a

      fun foreachLabel (r, f) = foldLabel (r, (), f o #1)

      fun map (r, f) =
	 case r of
	    NonTail {cont, handler} =>
	       NonTail {cont = f cont,
			handler = Handler.map (handler, f)}
	  | _ => r

      fun compose (c: t, r: t): t =
	 case r of
	    Dead => Dead
	  | HandleOnly =>
	       (case c of
		   Dead => Dead
		 | HandleOnly => HandleOnly
		 | NonTail _ => c
		 | Tail => HandleOnly)
	  | NonTail {cont, handler, ...} =>
	       (case (handler, c) of
		   (Handler.CallerHandler, NonTail {handler = h1, ...}) =>
		      NonTail {cont = cont, handler = h1}
		 | _ => r)
	  | Tail => c
   end

structure Transfer =
   struct
      datatype t =
         Arith of {prim: Prim.t,
		   args: Var.t vector,
		   overflow: Label.t, (* Must be nullary. *)
		   success: Label.t, (* Must be unary. *)
		   ty: Type.t}
       | Bug (* MLton thought control couldn't reach here. *)
       | Call of {func: Func.t,
		  args: Var.t vector,
		  return: Return.t}
       | Case of {test: Var.t,
		  cases: Label.t Cases.t,
		  default: Label.t option} (* Must be nullary. *)
       | Goto of {dst: Label.t,
		  args: Var.t vector}
       | Raise of Var.t vector
       | Return of Var.t vector
       | Runtime of {prim: Prim.t,
		     args: Var.t vector,
		     return: Label.t} (* Must be nullary. *)

      fun iff (test: Var.t, {truee, falsee}) =
	 Case
	 {cases = Cases.Int (Vector.new2 ((0, falsee), (1, truee))),
	  default = NONE,
	  test = test}
	 
      fun foreachFuncLabelVar (t, func, label: Label.t -> unit, var) =
	 let
	    fun vars xs = Vector.foreach (xs, var)
	 in
	    case t of
	       Arith {args, overflow, success, ...} =>
		  (vars args
		   ; label overflow 
		   ; label success)
	     | Bug => ()
	     | Call {func = f, args, return, ...} =>
		  (func f
		   ; Return.foreachLabel (return, label)
		   ; vars args)
	     | Case {test, cases, default, ...} =>
		  (var test
		   ; Cases.foreach (cases, label)
		   ; Option.app (default, label))
	     | Goto {dst, args, ...} => (vars args; label dst)
	     | Raise xs => vars xs
	     | Return xs => vars xs
	     | Runtime {args, return, ...} =>
		  (vars args
		   ; label return)
	 end

      fun foreachFunc (t, func) =
	 foreachFuncLabelVar (t, func, fn _ => (), fn _ => ())

      fun foreachLabelVar (t, label, var) =
	 foreachFuncLabelVar (t, fn _ => (), label, var)
	 
      fun foreachLabel (t, j) = foreachLabelVar (t, j, fn _ => ())
      fun foreachVar (t, v) = foreachLabelVar (t, fn _ => (), v)

      fun replaceLabelVar (t, fl, fx) =
	 let
	    fun fxs xs = Vector.map (xs, fx)
	 in
	    case t of
	       Arith {prim, args, overflow, success, ty} =>
		  Arith {prim = prim,
			 args = fxs args,
			 overflow = fl overflow,
			 success = fl success,
			 ty = ty}
	     | Bug => Bug
	     | Call {func, args, return} =>
		  Call {func = func, 
			args = fxs args,
			return = Return.map (return, fl)}
	     | Case {test, cases, default} =>
		  Case {test = fx test, 
			cases = Cases.map(cases, fl),
			default = Option.map(default, fl)}
	     | Goto {dst, args} => 
		  Goto {dst = fl dst, 
			args = fxs args}
	     | Raise xs => Raise (fxs xs)
	     | Return xs => Return (fxs xs)
	     | Runtime {prim, args, return} =>
		  Runtime {prim = prim,
			   args = fxs args,
			   return = fl return}
	 end

      fun replaceLabel (t, f) = replaceLabelVar (t, f, fn x => x)
      fun replaceVar (t, f) = replaceLabelVar (t, fn l => l, f)

      local open Layout
      in
	 fun layoutCase {test, cases, default} =
	    let
	       fun doit (l, layout) =
		  Vector.toListMap
		  (l, fn (i, l) =>
		   seq [layout i, str " => ", Label.layout l])
	       datatype z = datatype Cases.t
	       val cases =
		  case cases of
		     Char l => doit (l, Char.layout)
		   | Con l => doit (l, Con.layout)
		   | Int l => doit (l, Int.layout)
		   | Word l => doit (l, Word.layout)
		   | Word8 l => doit (l, Word8.layout)
	       val cases =
		  case default of
		     NONE => cases
		   | SOME j =>
			cases @ [seq [str "_ => ", Label.layout j]]
	    in align [seq [str "case ", Var.layout test, str " of"],
		      indent (alignPrefix (cases, "| "), 2)]
	    end

	 val layout =
	    fn Arith {prim, args, overflow, success, ty} =>
		  seq [Label.layout success,
		       tuple [Prim.layoutApp (prim, args, Var.layout)],
		       str " Overflow => ",
		       Label.layout overflow, str " ()"]
	     | Bug => str "Bug"
	     | Call {func, args, return} =>
		  let
		     val call = seq [Func.layout func, str " ", layoutTuple args]
		     val call =
			case return of
			   Return.Dead => seq [str "Dead ", call]
			 | Return.HandleOnly => seq [str "HandleOnly ", call]
			 | Return.Tail => call
			 | Return.NonTail {cont, handler} => 
			      let
				 val call =
				    seq [Label.layout cont, str " ", paren call]
			      in
				 case handler of
				    Handler.CallerHandler => call
				  | Handler.Handle l =>
				       seq [call, str " handle ", Label.layout l]
				  | Handler.None => seq [call, str " None"]
			      end
		  in
		     call
		  end
	     | Case arg => layoutCase arg
	     | Goto {dst, args} =>
		  seq [Label.layout dst, str " ", layoutTuple args]
	     | Raise xs => seq [str "raise ", layoutTuple xs]
	     | Return xs =>
		  seq [str "return ",
		       if 1 = Vector.length xs
			  then Var.layout (Vector.sub (xs, 0))
		       else layoutTuple xs]
	     | Runtime {prim, args, return} =>
		  seq [Label.layout return, str " ", 
		       tuple [Prim.layoutApp (prim, args, Var.layout)]]
      end

      fun varsEquals (xs, xs') = Vector.equals (xs, xs', Var.equals)

      fun equals (e: t, e': t): bool =
	 case (e, e') of
	    (Arith {prim, args, overflow, success, ...},
	     Arith {prim = prim', args = args', 
		    overflow = overflow', success = success', ...}) =>
	       Prim.equals (prim, prim') andalso
	       varsEquals (args, args') andalso
	       Label.equals (overflow, overflow') andalso
	       Label.equals (success, success')
	  | (Bug, Bug) => true
	  | (Call {func, args, return}, 
	     Call {func = func', args = args', return = return'}) =>
	       Func.equals (func, func') andalso
	       varsEquals (args, args') andalso
	       Return.equals (return, return')
	  | (Case {test, cases, default},
	     Case {test = test', cases = cases', default = default'}) =>
	       Var.equals (test, test') andalso
	       Cases.equals (cases, cases', Label.equals) andalso
	       Option.equals (default, default', Label.equals)
	  | (Goto {dst, args}, Goto {dst = dst', args = args'}) =>
	       Label.equals (dst, dst') andalso
	       varsEquals (args, args')
	  | (Raise xs, Raise xs') => varsEquals (xs, xs')
	  | (Return xs, Return xs') => varsEquals (xs, xs')
	  | (Runtime {prim, args, return},
	     Runtime {prim = prim', args = args', return = return'}) =>
	       Prim.equals (prim, prim') andalso
	       varsEquals (args, args') andalso
	       Label.equals (return, return')
	  | _ => false

      local
	 val newHash = Random.word
	 val arith = newHash ()
	 val bug = newHash ()
	 val call = newHash ()
	 val casee = newHash ()
	 val goto = newHash ()
	 val raisee = newHash ()
	 val return = newHash ()
	 val runtime = newHash ()
	 fun hashVars (xs: Var.t vector, w: Word.t): Word.t =
	    Vector.fold (xs, w, fn (x, w) => Word.xorb (w, Var.hash x))
	 fun hash2 (w1: Word.t, w2: Word.t) = Word.xorb (w1, w2)
	 fun hash3 (w1: Word.t, w2: Word.t, w3: Word.t) 
	   = Word.xorb (hash2 (w1, w2), w3)
      in
	 val hash: t -> Word.t =
	    fn Arith {args, overflow, success, ...} =>
	          hashVars (args, hash2 (Label.hash overflow,
					 Label.hash success))
	     | Bug => bug
	     | Call {func, args, return} =>
		  hashVars (args, hash2 (Func.hash func, Return.hash return))
	     | Case {test, cases, default} =>
		  hash2 (Var.hash test, 
			 Cases.fold
			 (cases, 
			  Option.fold
			  (default, 0wx55555555, 
			   fn (l, w) => 
			   hash2 (Label.hash l, w)),
			  fn (l, w) => 
			  hash2 (Label.hash l, w)))
	     | Goto {dst, args} =>
		  hashVars (args, Label.hash dst)
	     | Raise xs => hashVars (xs, raisee)
	     | Return xs => hashVars (xs, return)
	     | Runtime {prim, args, return} =>
		  hashVars (args, Label.hash return)
      end

      val hash = Trace.trace ("Transfer.hash", layout, Word.layout) hash

   end
datatype z = datatype Transfer.t

local
   open Layout
in
   fun layoutFormals (xts: (Var.t * Type.t) vector) =
      Vector.layout (fn (x, t) =>
		    seq [Var.layout x,
			 if !Control.showTypes
			    then seq [str ": ", Type.layout t]
			 else empty])
      xts
end

structure Block =
   struct
      datatype t =
	 T of {args: (Var.t * Type.t) vector,
	       label: Label.t,
	       statements: Statement.t vector,
	       transfer: Transfer.t}
	 
      local
	 fun make f (T r) = f r
      in
	 val args = make #args
	 val label = make #label
	 val statements = make #statements
	 val transfer = make #transfer
      end
   
      fun layout (T {label, args, statements, transfer}) =
	 let
	    open Layout
	 in
	    align [seq [Label.layout label, str " ",
			Vector.layout (fn (x, t) =>
				       if !Control.showTypes
					  then seq [Var.layout x, str ": ",
						    Type.layout t]
				       else Var.layout x) args],
		   indent (align
			   [align
			    (Vector.toListMap (statements, Statement.layout)),
			    Transfer.layout transfer],
			   2)]
	 end

      fun clear (T {label, args, statements, ...}) =
	 (Label.clear label
	  ; Vector.foreach (args, Var.clear o #1)
	  ; Vector.foreach (statements, Statement.clear))
   end

structure ExnStack =
   struct
      structure ZPoint =
	 struct
	    datatype t = Caller | Me

	    val equals: t * t -> bool = op =
	       
	    val toString =
	       fn Caller => "Caller"
		| Me => "Me"

	    val layout = Layout.str o toString
	 end

      structure L = FlatLattice (structure Point = ZPoint)
      open L
      structure Point = ZPoint
	 
      val me = point Point.Me
      val caller = point Point.Caller
   end

structure HandlerLat = FlatLattice (structure Point = Label)

structure HandlerInfo =
   struct
      datatype t = T of {block: Block.t,
			 global: ExnStack.t,
			 handler: HandlerLat.t,
			 slot: ExnStack.t,
			 visited: bool ref}

      fun new (b: Block.t): t =
	 T {block = b,
	    global = ExnStack.new (),
	    handler = HandlerLat.new (),
	    slot = ExnStack.new (),
	    visited = ref false}

      fun layout (T {global, handler, slot, ...}) =
	 Layout.record [("global", ExnStack.layout global),
			("slot", ExnStack.layout slot),
			("handler", HandlerLat.layout handler)]
   end

structure Datatype =
   struct
      datatype t =
	 T of {
	       tycon: Tycon.t,
	       cons: {con: Con.t,
		      args: Type.t vector} vector
	       }

      fun layout (T {tycon, cons}) =
	 let
	    open Layout
	 in
	    seq [Tycon.layout tycon,
		 str " = ",
		 alignPrefix
		 (Vector.toListMap
		  (cons, fn {con, args} =>
		   seq [Con.layout con,
			if Vector.isEmpty args
			   then empty
			else seq [str " of ",
				  Vector.layout Type.layout args]]),
		  "| ")]
	 end

      fun clear (T {tycon, cons}) =
	 (Tycon.clear tycon
	  ; Vector.foreach (cons, Con.clear o #con))
   end

structure Function =
   struct
      structure CPromise = ClearablePromise
	 
      type dest = {args: (Var.t * Type.t) vector,
		   blocks: Block.t vector,
		   name: Func.t,
		   raises: Type.t vector option,
		   returns: Type.t vector option,
		   sourceInfo: SourceInfo.t,
		   start: Label.t}

      (* There is a messy interaction between the laziness used in controlFlow
       * and the property lists on labels because the former stores
       * stuff on the property lists.  So, if you force the laziness, then
       * clear the property lists, then try to use the lazy stuff, you will
       * get screwed with undefined properties.  The right thing to do is reset
       * the laziness when the properties are cleared.
       *)
      datatype t =
	 T of {controlFlow:
	       {dfsTree: unit -> Block.t Tree.t,
		dominatorTree: unit -> Block.t Tree.t,
		graph: DirectedGraph.t,
		labelNode: Label.t -> DirectedGraph.Node.t,
		nodeBlock: DirectedGraph.Node.t -> Block.t} CPromise.t,
	       dest: dest}

      local
	 fun make f (T {dest, ...}) = f dest
      in
	 val blocks = make #blocks
	 val dest = make (fn d => d)
	 val name = make #name
	 val raises = make #raises
	 val returns = make #returns
	 val start = make #start
      end

      fun foreachVar (f: t, fx: Var.t * Type.t -> unit): unit =
	 let
	    val {args, blocks, ...} = dest f
	    val _ = Vector.foreach (args, fx)
	    val _ =
	       Vector.foreach
	       (blocks, fn Block.T {args, statements, ...} =>
		(Vector.foreach (args, fx)
		 ; Vector.foreach (statements, fn Statement.T {var, ty, ...} => 
				   Option.app (var, fn x => fx (x, ty)))))
	 in
	    ()
	 end

      fun controlFlow (T {controlFlow, ...}) =
	 let
	    val {graph, labelNode, nodeBlock, ...} = CPromise.force controlFlow
	 in
	    {graph = graph, labelNode = labelNode, nodeBlock = nodeBlock}
	 end

      local
	 fun make sel =
	    fn T {controlFlow, ...} => sel (CPromise.force controlFlow) ()
      in
	 val dfsTree = make #dfsTree
	 val dominatorTree = make #dominatorTree
      end

      fun dfs (f, v) =
	 let
	    val {blocks, name, start, ...} = dest f
	    val numBlocks = Vector.length blocks
	    val {get = labelIndex, set = setLabelIndex, rem, ...} =
	       Property.getSetOnce (Label.plist,
				    Property.initRaise ("index", Label.layout))
	    val _ = Vector.foreachi (blocks, fn (i, Block.T {label, ...}) =>
				     setLabelIndex (label, i))
	    val visited = Array.array (numBlocks, false)
	    fun visit (l: Label.t): unit =
	       let
		  val i = labelIndex l
	       in
		  if Array.sub (visited, i)
		     then ()
		  else
		     let
			val _ = Array.update (visited, i, true)
			val b as Block.T {transfer, ...} =
			   Vector.sub (blocks, i)
			val v' = v b
			val _ = Transfer.foreachLabel (transfer, visit)
			val _ = v' ()
		     in
			()
		     end
	       end
	    val _ = visit start
	    val _ = Vector.foreach (blocks, rem o Block.label)
	 in
	    ()
	 end
      
      fun inferHandlers (f: t): Label.t list option array =
	 let
	    val {blocks, name, start, ...} = dest f
	    val {get = labelIndex: Label.t -> int, set = setLabelIndex, ...} =
	       Property.getSetOnce (Label.plist,
				    Property.initRaise ("index", Label.layout))
	    val _ =
	       Vector.foreachi
	       (blocks, fn (i, Block.T {label, ...}) =>
		setLabelIndex (label, i))
	    val numBlocks = Vector.length blocks
	    val handlerStack = Array.array (numBlocks, NONE)
	    val visited = Array.array (numBlocks, false)
	    (* Do a dfs from the start, figuring out the handler stack at
	     * each label.
	     *)
	    fun visit (l: Label.t, hs: Label.t list): unit =
	       let
		  val i = labelIndex l
		  val Block.T {statements, transfer, ...} =
		     Vector.sub (blocks, i)
	       in
		  if Array.sub (visited, i)
		     then ()
		  else
		     let
			val _ = Array.update (visited, i, true)
			fun bug msg =
			   (Layout.outputl
			    (Vector.layout
			     (fn Block.T {label, ...} =>
			      let open Layout
			      in seq [Label.layout label,
				      str " ",
				      Option.layout (List.layout Label.layout)
				      (Array.sub (handlerStack,
						  labelIndex label))]
			      end)
			     blocks,
			     Out.error)
			    ; (Error.bug
			       (concat
				["inferHandlers bug found in ", Label.toString l,
				 ": ", msg])))
			val _ =
			   case Array.sub (handlerStack, i) of
			      NONE => Array.update (handlerStack, i, SOME hs)
			    | SOME hs' =>
				 if List.equals (hs, hs', Label.equals)
				    then ()
				 else bug "handler stack mismatch"
			val hs =
			   Vector.fold
			   (statements, hs, fn (s, hs) =>
			    let
			       val Statement.T {var, ty, exp, ...} = s
			    in
			       case Statement.exp s of
				  HandlerPop _ =>
				     (case hs of
					 [] => bug "pop of empty handler stack"
				       | _ :: hs => hs)
				| HandlerPush h => h :: hs
				| _ => hs
			    end)
			fun empty s =
			   if List.isEmpty hs
			      then ()
			   else bug (concat ["nonempty stack ", s])
			fun top l =
			   case hs of
			      l' :: _ =>
				 if Label.equals (l, l')
				    then ()
				 else bug "wrong handler on top"
			    | _ => bug "empty stack"
			val _ =
			   case transfer of
			      Call {return, ...} =>
				 (case return of
				     Return.Dead => ()
				   | Return.HandleOnly => empty "HandleOnly"
				   | Return.NonTail {handler, ...} =>
					(case handler of
					    Handler.CallerHandler =>
					       empty "CallerHandler"
					  | Handler.Handle l => top l
					  | Handler.None => ())
				   | Return.Tail => empty "tail")
			    | Raise _ => empty "raise"
			    | Return _ => empty "return"
			    | _ => ()
			val _ = 
			   Transfer.foreachLabel (transfer, fn l =>
						  visit (l, hs))
		     in
			()
		     end
	       end
	    val _ = visit (start, [])
	 in
	    handlerStack
	 end

      fun checkHandlers (f: t): unit =
	 let
	    val {name, start, blocks, ...} = dest f
	    val {get = labelInfo: Label.t -> HandlerInfo.t,
		 rem = remLabelInfo, 
		 set = setLabelInfo} =
	       Property.getSetOnce
	       (Label.plist, Property.initRaise ("info", Label.layout))
	    val _ =
	       Vector.foreach
	       (blocks, fn b => setLabelInfo (Block.label b, HandlerInfo.new b))
	    (* Do a DFS of the control-flow graph. *)
	    fun visitLabel l = visitInfo (labelInfo l)
	    and visitInfo
	       (hi as HandlerInfo.T {block, global, handler, slot, visited, ...})
	       : unit =
	       if !visited
		  then ()
	       else
	       let
		  val _ = visited := true
		  val Block.T {label, statements, transfer, ...} = block
		  datatype z = datatype ExnStack.t
		  val {global, handler, slot} =
		     Vector.fold
		     (statements,
		      {global = global, handler = handler, slot = slot},
		      fn (Statement.T {exp, ...}, {global, handler, slot}) =>
		      case exp of
			 SetExnStackLocal => {global = ExnStack.me,
					      handler = handler,
					      slot = slot}
		       | SetExnStackSlot => {global = slot,
					     handler = handler,
					     slot = slot}
		       | SetSlotExnStack => {global = global,
					     handler = handler,
					     slot = slot}
		       | SetHandler l => {global = global,
					  handler = HandlerLat.point l,
					  slot = slot}
		       | _ => {global = global, handler = handler, slot = slot})
		  fun fail msg =
		     (Control.message
		      (Control.Silent, fn () =>
		       let open Layout
		       in align
			  [str "before: ", HandlerInfo.layout hi,
			   str "block: ", Block.layout block,
			   seq [str "after: ",
				Layout.record
				[("global", ExnStack.layout global),
				 ("slot", ExnStack.layout slot),
				 ("handler", HandlerLat.layout handler)]],
			   Vector.layout
			   (fn Block.T {label, ...} =>
			    seq [Label.layout label,
				 str " ",
				 HandlerInfo.layout (labelInfo label)])
			   blocks]
		       end)
		      ; Error.bug (concat ["handler mismatch at ", msg]))
		  fun assert (msg, f) =
		     if f
			then ()
		     else fail msg
		  fun goto (l: Label.t): unit =
		     let
			val HandlerInfo.T {global = g, handler = h,
					   slot = s, ...} =
			   labelInfo l
			val _ =
			   assert ("goto",
				   ExnStack.<= (global, g)
				   andalso ExnStack.<= (slot, s)
				   andalso HandlerLat.<= (handler, h))
		     in
			visitLabel l
		     end
		  fun tail name =
		     assert (name,
			     ExnStack.forcePoint
			     (global, ExnStack.Point.Caller))
		  fun caller () =
		     ExnStack.forcePoint (global, ExnStack.Point.Caller)
		in
		   case transfer of
		      Arith {overflow, success, ...} =>
			(goto overflow; goto success)
		    | Bug => ()
		    | Call {return, ...} =>
			 assert
			 ("return",
			  case return of
			     Return.Dead => true
			   | Return.HandleOnly => caller ()
			   | Return.NonTail {cont, handler = h, ...} =>
				(goto cont
				 ; (case h of
				       Handler.CallerHandler => caller ()
				     | Handler.Handle l =>
					  let
					     val res =
						ExnStack.forcePoint
						(global, ExnStack.Point.Me)
						andalso (HandlerLat.forcePoint
							 (handler, l))
					     val _ = goto l
					  in
					     res
					  end
				     | Handler.None => true))
			   | Return.Tail => caller ())
		    | Case {cases, default, ...} =>
			 (Cases.foreach (cases, goto)
			  ; Option.app (default, goto))
		    | Goto {dst, ...} => goto dst
		    | Raise _ => tail "raise"
		    | Return _ => tail "return"
		    | Runtime {return, ...} => goto return
		end
	    val info as HandlerInfo.T {global, ...} = labelInfo start
	    val _ = ExnStack.forcePoint (global, ExnStack.Point.Caller)
	    val _ = visitInfo info
	    val _ =
	       Control.diagnostics
	       (fn display =>
		let
		   open Layout
		   val _ = 
		      display (seq [str "checkHandlers ",
				    Func.layout name])
		   val _ =
		      Vector.foreach
		      (blocks, fn Block.T {label, ...} =>
		       display (seq [Label.layout label,
				     str " ",
				     HandlerInfo.layout (labelInfo label)]))
		in
		   ()
		end)
	    val _ = Vector.foreach (blocks, fn b => remLabelInfo (Block.label b))
	 in
	    ()
	 end
			    
      local
	 structure Graph = DirectedGraph
	 structure Node = Graph.Node
	 structure Edge = Graph.Edge
      in
	 fun determineControlFlow ({args, blocks, name, start, ...}: dest) =
	    let
    	       open Dot
	       val g = Graph.new ()
	       fun newNode () = Graph.newNode g
	       val {get = labelNode, ...} =
		  Property.get
		  (Label.plist, Property.initFun (fn _ => newNode ()))
	       val {get = nodeInfo: Node.t -> {block: Block.t},
		    set = setNodeInfo, ...} =
		  Property.getSetOnce
		  (Node.plist, Property.initRaise ("info", Node.layout))
	       val _ =
		  Vector.foreach
		  (blocks, fn b as Block.T {label, transfer, ...} =>
		   let
		      val from = labelNode label
		      val _ = setNodeInfo (from, {block = b})
		      val _ =
			 Transfer.foreachLabel
			 (transfer, fn to =>
			  (Graph.addEdge (g, {from = from, to = labelNode to})
			   ; ()))
		   in
		      ()
		   end)
	       val root = labelNode start
	       val dfsTree =
		  Promise.lazy
		  (fn () =>
		   Graph.dfsTree (g, {root = root,
				      nodeValue = #block o nodeInfo})
		   handle exn => Error.bug (concat ["dfsTree: ",
						    Func.toString name,
						    ":",
						    case exn
						      of Fail s => s
						       | _ => "???"]))
	       val dominatorTree =
		  Promise.lazy
		  (fn () =>
		   Graph.dominatorTree (g, {root = root,
					    nodeValue = #block o nodeInfo})
		   handle exn => Error.bug (concat ["dominatorTree: ",
						    Func.toString name,
						    ":",
						    case exn
						       of Fail s => s
						     | _ => "???"]))
	    in
	       {dfsTree = dfsTree,
		dominatorTree = dominatorTree,
		graph = g,
		labelNode = labelNode,
		nodeBlock = #block o nodeInfo}
	    end

	 fun layoutDot (f, global: Var.t -> string option) =
	    let
	       val {name, args, start, blocks, returns, raises, ...} = dest f
	       fun makeName (name: string,
			     formals: (Var.t * Type.t) vector): string =
		  concat [name, " ",
			  let
			     open Layout
			  in
			     toString
			     (vector
			      (Vector.map
			       (formals, fn (var, ty) =>
				if !Control.showTypes
				   then seq [Var.layout var,
					     str ": ",
					     Type.layout ty]
				else Var.layout var)))
			  end]
	       fun makeName' (name: string,
			      formals: (Var.t * Type.t) vector,
			      returns: Type.t vector option,
			      raises: Type.t vector option): string =
		  concat [makeName (name, formals),
			  if !Control.showTypes
			     then let
				     open Layout
				  in
				     toString
				     (seq [str ": ",
					   Option.layout
					   (Vector.layout Type.layout) returns,
					   str " (",
					   Option.layout
					   (Vector.layout Type.layout) raises,
					   str ")"])
				  end
			  else ""]
	       open Dot
	       val graph = Graph.new ()
	       val {get = nodeOptions, ...} =
		  Property.get (Node.plist, Property.initFun (fn _ => ref []))
	       fun setNodeText (n: Node.t, l): unit =
		  List.push (nodeOptions n, NodeOption.Label l)
	       fun newNode () = Graph.newNode graph
	       val {destroy, get = labelNode} =
		  Property.destGet (Label.plist,
				    Property.initFun (fn _ => newNode ()))
	       val {get = edgeOptions, set = setEdgeOptions, ...} =
		  Property.getSetOnce (Edge.plist, Property.initConst [])
	       val _ =
		  Vector.foreach
		  (blocks, fn Block.T {label, args, statements, transfer} =>
		   let
		      val from = labelNode label
		      fun edge (to: Label.t,
				label: string,
				style: style): unit =
			 let
			    val e = Graph.addEdge (graph, {from = from,
							   to = labelNode to})
			    val _ = setEdgeOptions (e, [EdgeOption.label label,
							EdgeOption.Style style])
			 in
			    ()
			 end
		      val rest =
			 case transfer of
			    Arith {prim, args, overflow, success, ...} =>
			       (edge (success, "", Solid)
				; edge (overflow, "Overflow", Dashed)
				; [Layout.toString
				   (Prim.layoutApp (prim, args, fn x =>
						    Layout.str
						    (Var.pretty (x, global))))])
			  | Bug => ["bug"]
			  | Call {func, args, return, ...} =>
			       let
				  val f = Func.toString func
				  val args = Var.prettys (args, global)
				  val call = [f, " ", args]
			       in
				  case return of
				     Return.Dead => "Dead " :: call
				   | Return.HandleOnly =>
					"HandleOnly " :: call
				   | Return.NonTail {cont, handler} =>
					(edge (cont, "", Dotted)
					 ; (case handler of
					       Handler.CallerHandler => call
					     | Handler.Handle l =>
						  (edge (l, "", Dashed)
						   ; call)
					     | Handler.None => call @ [" None"]))
				   | Return.Tail => call
			       end
			  | Case {test, cases, default, ...} =>
			       let
				  fun doit (v, toString) =
				     Vector.foreach
				     (v, fn (x, j) =>
				      edge (j, toString x, Solid))
				  val _ =
				     case cases of
					Cases.Char v => doit (v, Char.toString)
				      | Cases.Con v => doit (v, Con.toString)
				      | Cases.Int v => doit (v, Int.toString)
				      | Cases.Word v => doit (v, Word.toString)
				      | Cases.Word8 v => doit (v, Word8.toString)
				  val _ = 
				     case default of
					NONE => ()
				      | SOME j =>
					   edge (j, "default", Solid)
			       in
				  ["case ", Var.toString test]
			       end
			  | Goto {dst, args} =>
			       (edge (dst, "", Solid)
				; [Label.toString dst, " ",
				   Var.prettys (args, global)])
			  | Raise xs => ["raise ", Var.prettys (xs, global)]
			  | Return xs => ["return ", Var.prettys (xs, global)]
			  | Runtime {prim, args, return} =>
			       (edge (return, "", Solid)
				; [Layout.toString
				   (Prim.layoutApp (prim, args, fn x =>
						    Layout.str
						    (Var.pretty (x, global))))])
		      val lab =
			 Vector.foldr
			 (statements, [(concat rest, Left)],
			  fn (Statement.T {var, ty, exp, ...}, ac) =>
			  let
			     val exp = Exp.toPretty (exp, global)
			     val s =
				if Type.isUnit ty
				   then exp
				else
				   case var of
				      NONE => exp
				    | SOME var =>
					 concat [Var.toString var,
						 if !Control.showTypes
						    then concat [": ",
								 Layout.toString
								 (Type.layout ty)]
						 else "",
						    " = ", exp]
			  in
			     (s, Left) :: ac
			  end)
		      val name = makeName (Label.toString label, args)
		      val _ = setNodeText (from, (name, Left) :: lab)
		   in
		      ()
		   end)
	       val root = labelNode start
	       val graphLayout =
		  Graph.layoutDot
		  (graph, fn {nodeName} => 
		   {title = concat [Func.toString name, " control-flow graph"],
		    options = [GraphOption.Rank (Min, [{nodeName = nodeName root}])],
		    edgeOptions = edgeOptions,
		    nodeOptions =
		    fn n => let
			       val l = ! (nodeOptions n)
			       open NodeOption
			    in FontColor Black :: Shape Box :: l
			    end})
	       fun treeLayout () =
		  let
		     val _ =
			Vector.foreach
			(blocks, fn Block.T {label, ...} =>
			 nodeOptions (labelNode label)
			 := [NodeOption.label (Label.toString label)])
		     val treeLayout =
			Tree.layoutDot
			(Graph.dominatorTree (graph,
					      {root = root,
					       nodeValue = ! o nodeOptions}),
			 {title = concat [Func.toString name, " dominator tree"],
			  options = [],
			  nodeOptions = fn z => z})
		     val _ = destroy ()
		  in
		     treeLayout
		  end
	    in
	       {graph = graphLayout,
		tree = treeLayout}
	    end
      end

      fun new (dest: dest) =
	 let
	    val controlFlow = CPromise.delay (fn () => determineControlFlow dest)
	 in
	    T {controlFlow = controlFlow,
	       dest = dest}
	 end

      fun clear (T {controlFlow, dest, ...}) =
	 let
	    val {name, args, blocks, ...} = dest
	    val _ = (Vector.foreach (args, Var.clear o #1)
		     ; Vector.foreach (blocks, Block.clear))
      	    val _ = CPromise.clear controlFlow
	 in
	    ()
	 end

      fun layoutHeader (f: t): Layout.t =
	 let
	    val {args, name, raises, returns, start, ...} = dest f
	    open Layout
	 in
	    seq [str "fun ",
		 Func.layout name,
		 str " ",
		 layoutFormals args,
		 if !Control.showTypes
		    then seq [str ": ",
			      record [("raises",
				       Option.layout
				       (Vector.layout Type.layout) raises),
				      ("returns",
				       Option.layout
				       (Vector.layout Type.layout) returns)]]
		 else empty,
		    str " = ", Label.layout start, str " ()"]
	 end

      fun layout (f: t) =
	 let
	    val {blocks, ...} = dest f
	    open Layout
	 in
	    align [layoutHeader f,
		   indent (align (Vector.toListMap (blocks, Block.layout)), 2)]
	 end
      
      fun layouts (f: t, global, output: Layout.t -> unit): unit =
	 let
	    val {blocks, name, ...} = dest f
	    val _ = output (layoutHeader f)
	    val _ = Vector.foreach (blocks, fn b =>
				    output (Layout.indent (Block.layout b, 2)))
	    val _ =
	       if not (!Control.keepDot)
		  then ()
	       else
		  let
		     val {graph, tree} = layoutDot (f, global)
		     val name = Func.toString name
		     fun doit (s, g) =
			let
			   open Control
			in
			   saveToFile
			   ({suffix = concat [name, ".", s, ".dot"]},
			    Dot, (), Layout (fn () => g))
			end
		     val _ = doit ("cfg", graph)
			handle _ => Error.warning "couldn't layout cfg"
		     val _ = doit ("dom", tree ())
			handle _ => Error.warning "couldn't layout dom"
		  in
		     ()
		  end
	 in
	    ()
	 end

      fun alphaRename f =
	 let
	    local
	       fun make (new, plist, layout) =
		  let
		     val {get, set, destroy, ...} = 
		        Property.destGetSetOnce (plist, Property.initConst NONE)
		     fun bind x =
			let
			   val x' = new x
			   val _ = set (x, SOME x')
			in
			   x'
			end
		     fun lookup x =
		        case get x of
			   NONE => x
			 | SOME y => y
		  in (bind, lookup, destroy)
		  end
	    in
	       val (bindVar, lookupVar, destroyVar) =
		  make (Var.new, Var.plist, Var.layout)
	       val (bindLabel, lookupLabel, destroyLabel) =
		  make (Label.new, Label.plist, Label.layout)
	    end
	    fun lookupVars xs = Vector.map (xs, lookupVar)
	    val {args, blocks, name, raises, returns, sourceInfo, start, ...} =
	       dest f
	    val args = Vector.map (args, fn (x, ty) => (bindVar x, ty))
	    val bindLabel = ignore o bindLabel
	    val bindVar = ignore o bindVar
	    val _ = 
	       Vector.foreach
	       (blocks, fn Block.T {label, args, statements, ...} => 
		(bindLabel label
		 ; Vector.foreach (args, fn (x, _) => bindVar x)
		 ; Vector.foreach (statements, 
				   fn Statement.T {var, ...} => 
				   Option.app (var, bindVar))))
	    val blocks = 
	       Vector.map
	       (blocks, fn Block.T {label, args, statements, transfer} =>
		Block.T {label = lookupLabel label,
			 args = Vector.map (args, fn (x, ty) =>
					    (lookupVar x, ty)),
			 statements = Vector.map
			              (statements, 
				       fn Statement.T {var, ty, exp} =>
				       Statement.T 
				       {var = Option.map (var, lookupVar),
					ty = ty,
					exp = Exp.replaceLabelVar
					      (exp, lookupLabel, lookupVar)}),
			 transfer = Transfer.replaceLabelVar
			            (transfer, lookupLabel, lookupVar)})
	    val start = lookupLabel start
	    val _ = destroyVar ()
	    val _ = destroyLabel ()
	 in
	    new {args = args,
		 blocks = blocks,
		 name = name,
		 raises = raises,
		 returns = returns,
		 sourceInfo = sourceInfo,
		 start = start}
	 end
   end

structure Program =
   struct
      datatype t =
	 T of {
	       datatypes: Datatype.t vector,
	       globals: Statement.t vector,
	       functions: Function.t list,
	       main: Func.t
	       }
   end

structure Program =
   struct
      open Program

      fun checkHandlers (T {functions, ...}) =
	 List.foreach (functions, Function.checkHandlers)
	 
      local
	 structure Graph = DirectedGraph
	 structure Node = Graph.Node
	 structure Edge = Graph.Edge
      in
	 fun layoutCallGraph (T {functions, main, ...},
			      title: string): Layout.t =
	    let
	       open Dot
	       val graph = Graph.new ()
	       val {get = nodeOptions, set = setNodeOptions, ...} =
		  Property.getSetOnce
		  (Node.plist, Property.initRaise ("options", Node.layout))
	       val {get = funcNode, destroy} =
		  Property.destGet
		  (Func.plist, Property.initFun
		   (fn f =>
		    let
		       val n = Graph.newNode graph
		       val _ =
			  setNodeOptions
			  (n,
			   let open NodeOption
			   in [FontColor Black, label (Func.toString f)]
			   end)
		    in
		       n
		    end))
	       val {get = edgeOptions, set = setEdgeOptions, ...} =
		  Property.getSetOnce (Edge.plist, Property.initConst [])
	       val _ =
		  List.foreach
		  (functions, fn f =>
		   let
		      val {name, blocks, ...} = Function.dest f
		      val from = funcNode name
		      val {get, destroy} =
			 Property.destGet
			 (Node.plist,
			  Property.initFun (fn _ => {nontail = ref false,
						     tail = ref false}))
		      val _ = 
			 Vector.foreach
			 (blocks, fn Block.T {transfer, ...} =>
			  case transfer of
			     Call {func, return, ...} =>
				let
				   val to = funcNode func
				   val {tail, nontail} = get to
				   val is =
				      (case return of
					  Return.NonTail _ => true
					| _ => false)
				   val r = if is
					      then nontail
					   else tail
				in
				   if !r
				      then ()
				   else (r := true
					 ; (setEdgeOptions
					    (Graph.addEdge
					     (graph, {from = from, to = to}),
					     if is
						then []
					     else [EdgeOption.Style Dotted])))
				end
			   | _ => ())
		      val _ = destroy ()
		   in
		      ()
		   end)
	       val root = funcNode main
	       val l =
		  Graph.layoutDot
		  (graph, fn {nodeName} =>
		   {title = title,
		    options = [GraphOption.Rank (Min, [{nodeName = nodeName root}])],
		    edgeOptions = edgeOptions,
		    nodeOptions = nodeOptions})
	       val _ = destroy ()
	    in
	       l
	    end
      end
	 
      fun layouts (p as T {datatypes, globals, functions, main},
		   output': Layout.t -> unit) =
	 let
	    val global = Statement.prettifyGlobals globals
	    open Layout
	    (* Layout includes an output function, so we need to rebind output
	     * to the one above.
	     *)
	    val output = output' 
	 in
	    output (str "\n\nDatatypes:")
	    ; Vector.foreach (datatypes, output o Datatype.layout)
	    ; output (str "\n\nGlobals:")
	    ; Vector.foreach (globals, output o Statement.layout)
	    ; output (seq [str "\n\nMain: ", Func.layout main])
	    ; output (str "\n\nFunctions:")
	    ; List.foreach (functions, fn f =>
			    Function.layouts (f, global, output))
	    ; if not (!Control.keepDot)
		 then ()
	      else
		 let
		    open Control
		 in
		    saveToFile
		    ({suffix = "call-graph.dot"},
		     Dot, (), Layout (fn () =>
				      layoutCallGraph (p, !Control.inputFile)))
		 end
	 end

      fun layoutStats (T {datatypes, globals, functions, ...}) =
	 let
	    val numTypes = ref 0
	    fun inc _ = Int.inc numTypes
	    val {hom = countType, destroy} =
	       Type.makeHom
	       {var = fn _ => Error.bug "ssa-tree saw var",
		con = inc}
	    val numStatements = ref (Vector.length globals)
	    val numBlocks = ref 0
	    val _ =
	       List.foreach
	       (functions, fn f =>
		let
		   val {args, blocks, ...} = Function.dest f
		in
		   Vector.foreach (args, countType o #2)
		   ; (Vector.foreach
		      (blocks, fn Block.T {statements, ...} =>
		       (Int.inc numBlocks
			; (Vector.foreach
			   (statements, fn Statement.T {ty, ...} =>
			    (countType ty
			     ; Int.inc numStatements))))))
		end)
	    val numFunctions = List.length functions
	    open Layout
	    val _ = destroy ()
	 in
	    align
	    (List.map
	     ([("num functions", Int.layout numFunctions),
	       ("num blocks", Int.layout (!numBlocks)),
	       ("num statements", Int.layout (!numStatements))],
	      fn (name, value) => seq [str (name ^ " "), value]))
	 end

      (* clear all property lists reachable from program *)
      fun clear (T {datatypes, globals, functions, ...}) =
	 ((* Can't do Type.clear because it clears out the info needed for
	   * Type.dest.
	   *)
	  Vector.foreach (datatypes, Datatype.clear)
	  ; Vector.foreach (globals, Statement.clear)
	  ; List.foreach (functions, Function.clear))

      fun clearGlobals (T {globals, ...}) =
	 Vector.foreach (globals, Statement.clear)

      fun clearTop (p as T {datatypes, functions, ...}) =
	 (Vector.foreach (datatypes, Datatype.clear)
	  ; List.foreach (functions, Func.clear o Function.name)
	  ; clearGlobals p)

      fun foreachVar (T {globals, functions, ...}, f) =
	 (Vector.foreach (globals, fn Statement.T {var, ty, ...} =>
			  f (valOf var, ty))
	  ; List.foreach (functions, fn g => Function.foreachVar (g, f)))

      fun hasPrim (T {globals, functions, ...},  f) =
	 DynamicWind.withEscape
	 (fn escape =>
	  let
	     fun loopStatement (Statement.T {exp, ...}) =
		case exp of
		   PrimApp {prim, ...} =>
		      if f prim
			 then escape true
		      else ()
		 | _ => ()
	     fun loopTransfer t =
	        case t of
		   Arith {prim, ...} =>
		      if f prim
			 then escape true
		      else ()
		 | Runtime {prim, ...} =>
		      if f prim
			 then escape true
		      else ()
		 | _ => ()
	     val _ = Vector.foreach (globals, loopStatement)
	     val _ =
		List.foreach
		(functions, fn f =>
		 Vector.foreach
		 (Function.blocks f, fn Block.T {statements, transfer, ...} =>
		  (Vector.foreach (statements, loopStatement);
		   loopTransfer transfer)))
	  in
	     false
	  end)

      (* Print information about the number of arithmetic operations that check
       * for overflow and how many have one constant argument.
       *)
      fun printArithStats (T {functions, globals, ...}): unit =
	 let
	    val {get = isConst, set = setIsConst, ...} =
	       Property.getSetOnce (Var.plist, Property.initConst false)
	    val _ =
	       Vector.foreach
	       (globals, fn Statement.T {var, exp, ...} =>
		case (var, exp) of
		   (SOME x, Exp.Const _) => setIsConst (x, true)
		 | _ => ())
	    fun newStats () =
	       {one = ref 0,
		two = ref 0,
		zero = ref 0}
	    fun printStats (name, {one, two, zero}) =
	       let
		  val zero = !zero
		  val one = !one
		  val two = !two
		  val total = Real.fromInt (zero + one + two)
		  fun per i =
		     if Real.equals (0.0, total)
			then "0"
		     else Real.format (100.0 * Real.fromInt i / total,
				       Real.Format.fix (SOME 1))
		  fun line i = concat [Int.toString i, " (", per i, "%)  "]
	       in
		  print (concat [line zero, line one])
	       end
	    val add = newStats ()
	    val mul = newStats ()
	    val sub = newStats ()
	    val _ =
	       List.foreach
	       (functions, fn f =>
		let
		   val {blocks, ...} = Function.dest f
		in
		   Vector.foreach
		   (blocks, fn Block.T {transfer, ...} =>
		    case transfer of
		       Transfer.Arith {prim, args, ...} =>
			  let
			     fun doit {one, two, zero} =
				let
				   val x = Vector.sub (args, 0)
				   val y = Vector.sub (args, 1)
				in
				   Int.inc
				   (if isConst x
				       then if isConst y then two else one
				    else if isConst y then one else zero)
				end
			     datatype z = datatype Prim.Name.t
			  in
			     case Prim.name prim of
				Int_addCheck => doit add
			      | Int_subCheck => doit sub
			      | Int_mulCheck => doit mul
			      | _ => ()
			  end
		     | _ => ())
		end)
	    val _ = (printStats ("add", add)
		     ; printStats ("sub", sub)
		     ; printStats ("mul", mul))
	 in
	    ()
	 end
   end

end
