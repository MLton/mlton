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
       | PrimApp of {prim: Prim.t,
		     targs: Type.t vector,
		     args: Var.t vector}
       | Profile of ProfileExp.t
       | Select of {tuple: Var.t,
		    offset: int}
       | Tuple of Var.t vector
       | Var of Var.t

      val unit = Tuple (Vector.new0 ())
	 
      fun foreachVar (e, v) =
	 let
	    fun vs xs = Vector.foreach (xs, v)
	 in
	    case e of
	       ConApp {args, ...} => vs args
	     | Const _ => ()
	     | PrimApp {args, ...} => vs args
	     | Profile _ => ()
	     | Select {tuple, ...} => v tuple
	     | Tuple xs => vs xs
	     | Var x => v x
	 end

      fun replaceVar (e, fx) =
	 let
	    fun fxs xs = Vector.map (xs, fx)
	 in
	    case e of
	       ConApp {con, args} => ConApp {con = con, args = fxs args}
	     | Const _ => e
	     | PrimApp {prim, targs, args} =>
		  PrimApp {prim = prim, targs = targs, args = fxs args}
	     | Profile _ => e
	     | Select {tuple, offset} =>
		  Select {tuple = fx tuple, offset = offset}
	     | Tuple xs => Tuple (fxs xs)
	     | Var x => Var (fx x)
	 end

      fun layout e =
	 let
	    open Layout
	 in
	    case e of
	       ConApp {con, args} =>
		  seq [Con.layout con, str " ", layoutTuple args]
	     | Const c => Const.layout c
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
	     | Profile p => ProfileExp.layout p
	     | Select {tuple, offset} =>
		  seq [str "#", Int.layout (offset + 1), str " ",
		       Var.layout tuple]
	     | Tuple xs => layoutTuple xs
	     | Var x => Var.layout x
	 end

      val isFunctional =
	 fn ConApp _ => true
	  | Const _ => true
	  | PrimApp {prim, ...} => Prim.isFunctional prim
	  | Profile _ =>
	       Error.bug "doesn't make sense to ask isFunctional Profile"
	  | Select _ => true
	  | Tuple _ => true
	  | Var _ => true
	       
      fun maySideEffect (e: t): bool =
	 case e of
	    ConApp _ => false
	  | Const _ => false
	  | PrimApp {prim,...} => Prim.maySideEffect prim
	  | Profile _ => false
	  | Select _ => false
	  | Tuple _ => false
	  | Var _ => false

      fun varsEquals (xs, xs') = Vector.equals (xs, xs', Var.equals)

      fun equals (e: t, e': t): bool =
	 case (e, e') of
	    (ConApp {con, args}, ConApp {con = con', args = args'}) =>
	       Con.equals (con, con') andalso varsEquals (args, args')
	  | (Const c, Const c') => Const.equals (c, c')
	  | (PrimApp {prim, args, ...},
	     PrimApp {prim = prim', args = args', ...}) =>
	       Prim.equals (prim, prim') andalso varsEquals (args, args')
	  | (Profile p, Profile p') => ProfileExp.equals (p, p')
	  | (Select {tuple = t, offset = i}, Select {tuple = t', offset = i'}) =>
	       Var.equals (t, t') andalso i = i'
	  | (Tuple xs, Tuple xs') => varsEquals (xs, xs')
	  | (Var x, Var x') => Var.equals (x, x')
	  | _ => false

      local
	 val newHash = Random.word
	 val conApp = newHash ()
	 val primApp = newHash ()
	 val profile = newHash ()
	 val select = newHash ()
	 val tuple = newHash ()
	 fun hashVars (xs: Var.t vector, w: Word.t): Word.t =
	    Vector.fold (xs, w, fn (x, w) => Word.xorb (w, Var.hash x))
      in
	 val hash: t -> Word.t =
	    fn ConApp {con, args, ...} => hashVars (args, Con.hash con)
	     | Const c => Const.hash c
	     | PrimApp {args, ...} => hashVars (args, primApp)
	     | Profile p => Word.xorb (profile, ProfileExp.hash p)
	     | Select {tuple, offset} =>
		  Word.xorb (select, Var.hash tuple + Word.fromInt offset)
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
	  | PrimApp {prim, args, ...} =>
	       Layout.toString
	       (Prim.layoutApp (prim, args, fn x =>
				case global x of
				   NONE => Var.layout x
				 | SOME s => Layout.str s))
	  | Profile p => ProfileExp.toString p
	  | Select {tuple, offset} =>
	       concat ["#", Int.toString (offset + 1), " ", Var.toString tuple]
	  | Tuple xs => Var.prettys (xs, global)
	  | Var x => Var.toString x

      val isProfile =
	 fn Profile _ => true
	  | _ => false
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

      val toString = Layout.toString o layout

      fun equals (T {exp = e, ty = t, var = v},
		  T {exp = e', ty = t', var = v'}): bool =
	 Option.equals (v, v', Var.equals)
	 andalso Type.equals (t, t')
	 andalso Exp.equals (e, e')

      local
	 fun make f x =
	    T {var = NONE,
	       ty = Type.unit,
	       exp = f x}
      in
	 val profile = make Exp.Profile
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
      structure Label = Label

      datatype t =
	 Caller
       | Dead
       | Handle of Label.t

      fun layout (h: t): Layout.t =
	 let
	    open Layout
	 in
	    case h of
	       Caller => str "Caller"
	     | Dead => str "Dead"
	     | Handle l => seq [str "Handle ", Label.layout l]
	 end

      val equals =
	 fn (Caller, Caller) => true
	  | (Dead, Dead) => true
	  | (Handle l, Handle l') => Label.equals (l, l')
	  | _ => false

      fun foldLabel (h: t, a: 'a, f: Label.t * 'a -> 'a): 'a =
	 case h of
	    Caller => a
	  | Dead => a
	  | Handle l => f (l, a)

      fun foreachLabel (h, f) = foldLabel (h, (), f o #1)

      fun map (h, f) =
	 case h of
	    Caller => Caller
	  | Dead => Dead
	  | Handle l => Handle (f l)

      local
	 val newHash = Random.word
	 val caller = newHash ()
	 val dead = newHash ()
	 val handlee = newHash ()
      in
	 fun hash (h: t): word =
	    case h of
	       Caller => caller
	     | Dead => dead
	     | Handle l => Word.xorb (handlee, Label.hash l)
      end
   end

structure Return =
   struct
      structure Label = Label
      structure Handler = Handler

      datatype t =
	 Dead
       | NonTail of {cont: Label.t,
		     handler: Handler.t}
       | Tail

      fun layout r =
	 let
	    open Layout
	 in
	    case r of
	       Dead => str "Dead"
	     | NonTail {cont, handler} =>
		  seq [str "NonTail ",
		       Layout.record
		       [("cont", Label.layout cont),
			("handler", Handler.layout handler)]]
	     | Tail => str "Tail"
	 end

      fun equals (r, r'): bool =
	 case (r, r') of
	    (Dead, Dead) => true
	  | (NonTail {cont = c, handler = h},
	     NonTail {cont = c', handler = h'}) =>
	       Label.equals (c, c') andalso Handler.equals (h, h')
	   | (Tail, Tail) => true
	   | _ => false

      fun foldLabel (r: t, a, f) =
	 case r of
	    Dead => a
	  | NonTail {cont, handler} =>
	       Handler.foldLabel (handler, f (cont, a), f)
	  | Tail => a

      fun foreachLabel (r, f) = foldLabel (r, (), f o #1)

      fun foreachHandler (r, f) =
	 case r of
	    Dead => ()
	  | NonTail {handler, ...} => Handler.foreachLabel (handler, f)
	  | Tail => ()

      fun map (r, f) =
	 case r of
	    Dead => Dead
	  | NonTail {cont, handler} =>
	       NonTail {cont = f cont,
			handler = Handler.map (handler, f)}
	  | Tail => Tail

      fun compose (r, r') =
	 case r' of
	    Dead => Dead
	  | NonTail {cont, handler} =>
	       NonTail
	       {cont = cont,
		handler = (case handler of
			      Handler.Caller =>
				 (case r of
				     Dead => Handler.Caller
				   | NonTail {handler, ...} => handler
				   | Tail => Handler.Caller)
			    | Handler.Dead => handler
			    | Handler.Handle _ => handler)}
	  | Tail => r

      local
	 val newHash = Random.word
	 val dead = newHash ()
	 val nonTail = newHash ()
	 val tail = newHash ()
      in
	 fun hash r =
	    case r of
	       Dead => dead
	     | NonTail {cont, handler} =>
		  Word.xorb (Word.xorb (nonTail, Label.hash cont),
			     Handler.hash handler)
	     | Tail => tail
      end
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
       | Call of {args: Var.t vector,
		  func: Func.t,
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
		  seq [Func.layout func, str " ", layoutTuple args,
		       str " ", Return.layout return]
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
			  | Call {func, args, return} =>
			       let
				  val f = Func.toString func
				  val args = Var.prettys (args, global)
				  val _ =
				     case return of
					Return.Dead => ()
				      | Return.NonTail {cont, handler} =>
					   (edge (cont, "", Dotted)
					    ; (Handler.foreachLabel
					       (handler, fn l =>
						edge (l, "", Dashed))))
				      | Return.Tail => ()
			       in
				  [f, " ", args]
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
	    val {args, blocks, name, raises, returns, start, ...} =
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
					exp = Exp.replaceVar
					      (exp, lookupVar)}),
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
		 start = start}
	 end

      fun profile (f: t, sourceInfo): t =
	 if !Control.profile = Control.ProfileNone
	    orelse !Control.profileIL <> Control.ProfileSource
	    then f
	 else 
	 let
	    val _ = Control.diagnostic (fn () => layout f)
	    val {args, blocks, name, raises, returns, start} = dest f
	    val extraBlocks = ref []
	    val {get = labelBlock, set = setLabelBlock, rem} =
	       Property.getSetOnce
	       (Label.plist, Property.initRaise ("block", Label.layout))
	    val _ =
	       Vector.foreach
	       (blocks, fn block as Block.T {label, ...} =>
		setLabelBlock (label, block))
	    val blocks =
	       Vector.map
	       (blocks, fn Block.T {args, label, statements, transfer} =>
		let
		   fun make (exp: Exp.t): Statement.t =
		      Statement.T {exp = exp,
				   ty = Type.unit,
				   var = NONE}
		   val statements =
		      if Label.equals (label, start)
			 then (Vector.concat
			       [Vector.new1
				(make (Exp.Profile
				       (ProfileExp.Enter sourceInfo))),
				statements])
		      else statements
		   fun leave () =
		      make (Exp.Profile (ProfileExp.Leave sourceInfo))
		   fun prefix (l: Label.t,
			       statements: Statement.t vector): Label.t =
		      let
			 val Block.T {args, ...} = labelBlock l
			 val c = Label.newNoname ()
			 val xs = Vector.map (args, fn (x, _) => Var.new x)
			 val _ =
			    List.push
			    (extraBlocks,
			     Block.T
			     {args = Vector.map2 (xs, args, fn (x, (_, t)) =>
						  (x, t)),
			      label = c,
			      statements = statements,
			      transfer = Goto {args = xs,
					       dst = l}})
		      in
			 c
		      end
		   fun genHandler (cont: Label.t)
		      : Statement.t vector * Label.t * Handler.t =
		      case raises of
			 NONE => (statements, cont, Handler.Caller)
		       | SOME ts => 
			    let
			       val xs = Vector.map (ts, fn _ => Var.newNoname ())
			       val l = Label.newNoname ()
			       val _ =
				  List.push
				  (extraBlocks,
				   Block.T
				   {args = Vector.zip (xs, ts),
				    label = l,
				    statements = Vector.new1 (leave ()),
				    transfer = Transfer.Raise xs})
			    in
			       (statements,
				prefix (cont, Vector.new0 ()),
				Handler.Handle l)
			    end
		   fun addLeave () =
		      (Vector.concat [statements,
				      Vector.new1 (leave ())],
		       transfer)
		   val (statements, transfer) =
		      case transfer of
			 Call {args, func, return} =>
			    let
			       datatype z = datatype Return.t
			    in
			       case return of
				  Dead => (statements, transfer)
				| NonTail {cont, handler} =>
				     (case handler of
					 Handler.Dead => (statements, transfer)
				       | Handler.Caller =>
					    let
					       val (statements, cont, handler) =
						  genHandler cont
					       val return =
						  Return.NonTail
						  {cont = cont,
						   handler = handler}
					    in
					       (statements,
						Call {args = args,
						      func = func,
						      return = return})
					    end
				       | Handler.Handle l =>
					    (statements, transfer))
				| Tail => addLeave ()
			    end
		       | Raise _ => addLeave ()
		       | Return _ => addLeave ()
		       | _ => (statements, transfer)
		in
		   Block.T {args = args,
			    label = label,
			    statements = statements,
			    transfer = transfer}
		end)
	    val _ = Vector.foreach (blocks, rem o Block.label)
	    val blocks = Vector.concat [Vector.fromList (!extraBlocks), blocks]
	    val f = 
	       new {args = args,
		    blocks = blocks,
		    name = name,
		    raises = raises,
		    returns = returns,
		    start = start}
	    val _ = Control.diagnostic (fn () => layout f)
	 in
	    f
	 end

      val profile =
	 Trace.trace2 ("Ssa.Function.profile", layout, SourceInfo.layout, layout)
	 profile
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
				   datatype z = datatype Return.t
				   val is =
				      case return of
					 Dead => false
				       | NonTail _ => true
				       | Tail => false
				   val r = if is then nontail else tail
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
