(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor CpsTree (S: CPS_TREE_STRUCTS): CPS_TREE = 
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
	 Char
	| Int
	| IntInf
	| Pointer
	| Word
	| Word8
	| Real
	| Thread
	| String 
	| Array of t
	| Vector of t
	| Ref of t
	| Datatype of Tycon.t
	| Tuple of t vector

      local
	 val {get, set} =
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
	     (Tycon.real, nullary Real),
	     (Tycon.string, nullary String),
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

      local open Layout
      in
	 fun layout t =
	    case dest t of
	       Char => str "char"
	     | Int => str "int"
	     | IntInf => str "IntInf.int"
	     | Pointer => str "pointer"
	     | Word => str "word"
	     | Word8 => str "word8"
	     | Real => str "real"
	     | String => str "string"
	     | Thread => str "thread"
	     | Array t => seq [layout t, str " array"]
	     | Vector t => seq [layout t, str " vector"]
	     | Ref t => seq [layout t, str " ref"]
	     | Datatype t => Tycon.layout t
	     | Tuple ts =>
		  if Vector.isEmpty ts
		     then str "unit"
		  else paren (seq (separate (Vector.toListMap (ts, layout),
					     " * ")))
      end
   end

structure Func =
   struct
      open Var (* Id (structure AstId = Ast.Var) *)

      fun newNoname () = newString "F"
   end

structure Jump =
   struct
      open Func
      fun newNoname () = newString "L"
   end

local open Layout
in
   fun layoutTuple xs = Vector.layout Var.layout xs
end

structure PrimInfo =
   struct
      datatype t =
	 None
       | Overflow of Jump.t

      fun foreachJump (i: t, f) =
	 case i of
	    None => ()
	  | Overflow j => f j

      fun replaceJump (i: t, f) =
	 case i of
	    None => None
	  | Overflow j => Overflow (f j)
   end

structure PrimExp =
   struct
      datatype t =
	 Const of Const.t
       | Var of Var.t
       | Tuple of Var.t vector
       | Select of {tuple: Var.t,
		    offset: int}
       | ConApp of {con: Con.t,
		    args: Var.t vector}
       | PrimApp of {prim: Prim.t,
		     info: PrimInfo.t,
		     targs: Type.t vector,
		     args: Var.t vector}

      val unit = Tuple (Vector.new0 ())
	 
      fun foreachJumpVar (e, j, v) =
	 let fun vs xs = Vector.foreach (xs, v)
	 in case e of
	    Const _ => ()
	  | Var x => v x
	  | Tuple xs => vs xs
	  | Select {tuple, ...} => v tuple
	  | ConApp {args, ...} => vs args
	  | PrimApp {args, info, ...} => (vs args
					  ; PrimInfo.foreachJump (info, j))
	 end

      fun foreachJump (e, j) = foreachJumpVar (e, j, fn _ => ())
      fun foreachVar (e, v) = foreachJumpVar (e, fn _ => (), v)

      fun replaceJumpVar (e, fj, f) =
	 let fun fs xs = Vector.map (xs, f)
	 in case e of
	    Const _ => e
	  | Var x => Var (f x)
	  | Tuple xs => Tuple (fs xs)
	  | Select {tuple, offset} => Select {tuple = f tuple, offset = offset}
	  | ConApp {con, args} => ConApp {con = con, args = fs args}
	  | PrimApp {prim, info, targs, args} =>
	       PrimApp {prim = prim, info = PrimInfo.replaceJump (info, fj),
			targs = targs, args = fs args}
	 end

      fun replaceVar (e, f) = replaceJumpVar (e, fn j => j, f)

      local open Layout
      in
	 val layout =
	    fn Const c => Const.layout c
	     | Var x => Var.layout x
	     | Tuple xs => layoutTuple xs
	     | Select {tuple, offset} =>
		  seq [str "#", Int.layout (offset + 1), str " ",
		       Var.layout tuple]
	     | ConApp {con, args} =>
		  seq [Con.layout con, str " ", layoutTuple args]
	     | PrimApp {prim, info, targs, args} =>
		  seq [Prim.layout prim,
		       if !Control.showTypes
			  then if 0 = Vector.length targs
				  then empty
			       else Vector.layout Type.layout targs
		       else empty,
			  if isSome (Prim.numArgs prim)
			     then seq [str " ", layoutTuple args]
			  else empty,
			     case info of
				PrimInfo.None => empty
			      | PrimInfo.Overflow j => seq [str " Overflow ",
							    Jump.layout j]]
      end

      fun maySideEffect (e: t): bool =
	 case e of
	    PrimApp {prim,...} => Prim.maySideEffect prim
	  | _ => false
   end

structure Bind =
   struct
      type t = {
		var: Var.t,
		ty: Type.t,
		exp: PrimExp.t
		}

      local
	 open Layout
      in
	 fun layout {var, ty, exp} =
	    seq [str "val ", Var.layout var,
		 if !Control.showTypes
		    then seq [str ": ", Type.layout ty]
		 else empty,
		    str " = ", PrimExp.layout exp]
      end

      fun clear ({var, ...}: t): unit = Var.clear var
   end

structure Cause =
   struct
      datatype t = User | Dispatch | PolyEqual | Coerce
   end

structure Cases = Cases (type con = Con.t)

structure Transfer =
   struct
      datatype t =
	 Bug
       | Case of {cause: Cause.t,
		  test: Var.t,
		  cases: Jump.t Cases.t,
		  default: Jump.t option}
       | Call of {func: Func.t,
		  args: Var.t vector,
		  cont: Jump.t option}
       | Jump of {dst: Jump.t,
		  args: Var.t vector}
       | Raise of Var.t vector
       | Return of Var.t vector
	 
      fun foreachFuncJumpVar (t, func, jump, var) =
	 let
	    fun vars xs = Vector.foreach (xs, var)
	 in case t of
	    Bug => ()
	  | Call {func = f, args, cont, ...} =>
	       (func f
		; Option.app (cont, jump)
		; vars args)
	  | Case {test, cases, default, ...} =>
	       (var test
		; Cases.foreach (cases, jump)
		; Option.app (default, jump))
	  | Jump {dst, args, ...} => (vars args; jump dst)
	  | Raise xs => vars xs
	  | Return xs => vars xs
	 end

      fun foreachJumpVar (t, jump, var) =
	 foreachFuncJumpVar (t, fn _ => (), jump, var)
	 
      fun foreachJump (t, j) = foreachJumpVar (t, j, fn _ => ())
      fun foreachVar (t, v) = foreachJumpVar (t, fn _ => (), v)

      fun replaceVar (t, f) =
	 let fun fs xs = Vector.map (xs, f)
	 in case t of
	    Bug => Bug
	  | Case {cause, test, cases, default} =>
	       Case {cause = cause,
		     test = f test, cases = cases, default = default}
	  | Call {func, args, cont} =>
	       Call {func = func, args = fs args, cont = cont}
	  | Jump {dst, args} => Jump {dst = dst, args = fs args}
	  | Raise xs => Raise (fs xs)
	  | Return xs => Return (fs xs)
	 end

      local open Layout
      in
	 fun layoutCase {test, cases, default, cause}=
	    let
	       fun doit (l, layout) =
		  Vector.toListMap
		  (l, fn (i, l) =>
		   seq [layout i, str " => ", Jump.layout l])
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
			cases @ [seq [str "_ => ", Jump.layout j]]
	    in align [seq [str "case ", Var.layout test, str " of"],
		      indent (alignPrefix (cases, "| "), 2)]
	    end

	 val layout =
	    fn Bug => str "Bug"
	     | Call {func, args, cont} =>
		  let
		     val call = seq [Func.layout func, str " ", layoutTuple args]
		  in
		     case cont of
			NONE => call
		      | SOME j => seq [Jump.layout j, str " ", paren call]
		  end
	     | Case arg => layoutCase arg
	     | Jump {dst, args} =>
		  seq [Jump.layout dst, str " ", layoutTuple args]
	     | Raise xs => seq [str "raise ", layoutTuple xs]
	     | Return xs => if 1 = Vector.length xs
			       then Var.layout (Vector.sub (xs, 0))
			    else layoutTuple xs
      end
   
   end

local open Layout
in
   fun layoutFormals (xts: (Var.t * Type.t) vector) =
      Vector.layout (fn (x, t) =>
		    seq [Var.layout x,
			 if !Control.showTypes
			    then seq [str ": ", Type.layout t]
			 else empty])
      xts
end

structure DecExp =
   struct
      datatype dec =
	 Bind of Bind.t
       | Fun of {
		 name: Jump.t,
		 args: (Var.t * Type.t) vector,
		 body: exp
		 }
       | HandlerPush of Jump.t
       | HandlerPop
      and exp = Exp of {decs: dec list,
			transfer: Transfer.t}

      local
	 open Layout
      in
	 val rec layoutDec =
	    fn Bind b => Bind.layout b
	     | Fun {name, args, body} =>
		  align [seq [str "fun ",
			      align [seq [Jump.layout name,
					  str " ",
					  layoutFormals args,
					  str " = "]]],
			 indent (layoutExp body, !Control.indentation)]
	     | HandlerPush h => seq [str "val _ = HandlerPush ", Jump.layout h]
	     | HandlerPop => str "val _ = HandlerPop"
	 and layoutExp =
	    fn Exp {decs, transfer, ...} =>
	    let
	       val transfer = Transfer.layout transfer
	    in
	       case decs of
		  [] => transfer
		| _ =>
		     align [str "let",
			    indent (align (List.map (decs, layoutDec)),
				    !Control.indentation),
			    str "in",
			    indent (transfer, !Control.indentation),
			    str "end"]
	    end
      end
   end

structure Dec =
   struct
      datatype t = datatype DecExp.dec

      type exp = DecExp.exp

      val layout = DecExp.layoutDec
      val layoutBind = Bind.layout
   end

structure Exp =
   struct
      datatype t = datatype DecExp.exp
	 
      fun dest (Exp r) = r
      val make = Exp

      val decs = #decs o dest
      val transfer = #transfer o dest

      val layout = DecExp.layoutExp
	 
      local
	 fun make f (Exp {decs, transfer}, x): t =
	    Exp {decs = f (x, decs), transfer = transfer}
      in val prefix = make (op ::)
	 val prefixs = make (op @)
      end

      fun fromTransfer (t: Transfer.t): t = Exp {decs = [], transfer = t}

      val bug = fromTransfer Transfer.Bug

      fun foreach (e, {handleDec, handleTransfer}) =
	 let
	    fun exp (Exp {decs, transfer, ...}) =
	       (List.foreach (decs, dec)
		; handleTransfer transfer)
	    and dec d =
	       let val after = handleDec d
	       in (case d of
		      Dec.Fun {body, ...} => exp body
		    | _ => ())
		  ; after ()
	       end
	 in exp e
	 end

      fun ignore _ = ()
      fun ignore' _ = ignore

      fun foreach' (e, {handleDec, handleTransfer}) =
	 foreach (e, {handleTransfer = handleTransfer,
		      handleDec = fn d => (handleDec d; ignore)})

      fun foreachDec (e, handleDec) =
	 foreach' (e, {handleTransfer = ignore,
		       handleDec = handleDec})

      fun foreachVar (e, f) =
	 foreach'
	 (e, {handleTransfer = ignore,
	      handleDec = fn Dec.Bind {var, ty, ...} => f (var, ty)
	    | Dec.Fun {args, ...} => Vector.foreach (args, f)
	    | _ => ()})

      fun foreachBind (e, f) =
	 let
	    fun handleDec d =
	       ((case d of
		    Dec.Bind r => f r
		  | _ => ())
		    ; ignore)
	 in foreach (e, {handleTransfer = ignore,
			 handleDec = handleDec})
	 end

      fun foreachTransfer (e, f) =
	 foreach (e, {handleDec = ignore', handleTransfer = f})

      fun foreachCall (e, f) =
	 foreachTransfer (e, fn Transfer.Call r => f r | _ => ())

      fun layoutFlat (e: t, jumpHandlers: Jump.t -> Jump.t list): Layout.t =
	 let
	    open Layout
	    val funs = ref []
	    val _ = foreachDec (e, fn d =>
				case d of
				   Dec.Fun f => List.push (funs, f)
				 | _ => ())
	    fun layoutExp (Exp {decs, transfer, ...}) =
	       let
		  val transfer = Transfer.layout transfer
	       in
		  case decs of
		     [] => transfer
		   | _ =>
			let
			   val decs =
			      List.fold
			      (rev decs, [], fn (d, ac) =>
			       case d of
				  Dec.Bind b => Bind.layout b :: ac
				| Dec.Fun {name, ...} => 
				     seq [str "fun ",
					  Jump.layout name,
					  str " = ..."] ::
				     ac
				| Dec.HandlerPush h =>
				     seq [str "val _ = HandlerPush ",
					  Jump.layout h]
				     :: ac
				| Dec.HandlerPop =>
				     str "val _ = HandlerPop" :: ac)
			in
			   align [str "let",
				  indent (align decs, 3),
				  str "in",
				  indent (transfer, 3),
				  str "end"]
			end
	       end
	 in
	    align
	    [layoutExp e,
	     align (str "where" ::
		    List.map
		    (!funs, fn {name, args, body} =>
		     align [seq [str "fun ",
				 align [seq [Jump.layout name,
					     str " ",
					     layoutFormals args,
					     str " ",
					     List.layout Jump.layout
					     (jumpHandlers name),
					     str " = "
					     ]]],
			    indent (layoutExp body, !Control.indentation)]))]
	 end

      fun clear (e: t): unit =
	 let open DecExp
	 in foreach'
	    (e, {handleTransfer = fn _ => (),
		 handleDec =
		 fn Bind b => Bind.clear b
		  | Fun {name, args, ...} =>
		       (Jump.clear name
			; Vector.foreach (args, Var.clear o #1))
		  | _ => ()})
	 end

      fun alphaRename {exp: t,
		       substitution: {formal: Var.t, actual: Var.t} list,
		       handleJump: {old: Jump.t, new: Jump.t} -> unit} =
	 let
	    local
	       fun make (new, plist, layout) =
		  let
		     val {get, set, destroy} =
			Property.destGetSetOnce (plist, Property.initConst NONE)
		     fun bind x = let val x' = new x
				  in set (x, SOME x'); x'
				  end
		     fun lookup x =
			case get x of
			   NONE => x
			 | SOME y => y
		  in (set, bind, lookup, destroy)
		  end
	    in
	       val (setVar, bindVar, lookupVar, destroyVar) =
		  make (Var.new, Var.plist, Var.layout)
	       val (_, bindJump, lookupJump, destroyJump) =
		  make (Jump.new, Jump.plist, Jump.layout)
	    end

	    val bindJump =
	       fn j => let val j' = bindJump j
		       in handleJump {old = j, new = j'}
			  ; j'
		       end

	    fun lookupVars xs = Vector.map (xs, lookupVar)
	    datatype dec = datatype Dec.t
	    datatype transfer = datatype Transfer.t
	    fun loopTransfer (t: Transfer.t): Transfer.t =
	       case t of
		  Bug => Bug
		| Raise xs => Raise (lookupVars xs)
		| Case {cause = cause, test, cases, default} =>
		     Case {cause = cause,
			   test = lookupVar test,
			   cases = Cases.map (cases, lookupJump),
			   default = (case default of
					 NONE => NONE
				       | SOME j => SOME (lookupJump j))}
		| Return xs => Return (lookupVars xs)
		| Jump {dst, args} => Jump {dst = lookupJump dst,
					    args = lookupVars args}
		| Call {func, args, cont} =>
		     Call {func = func,
			   args = lookupVars args,
			   cont = Option.map (cont, lookupJump)}

	    fun loopExp e =
	       let val {decs, transfer} = dest e
		  val decs = List.map (decs, loopDec)
	       in make {decs = decs,
			transfer = loopTransfer transfer}
	       end
	    and loopDec d =
	       case d of
		  HandlerPush h => HandlerPush (lookupJump h)
		| HandlerPop => HandlerPop
		| Fun {name, args, body} =>
		     Fun {name = bindJump name,
			  args = Vector.map (args, fn (x, t) => (bindVar x, t)),
			  body = loopExp body}
		| Bind {var, ty, exp} => 
		     Bind {exp = PrimExp.replaceJumpVar (exp, lookupJump,
							 lookupVar),
			   var = bindVar var,
			   ty = ty}
	    val _ =
	       List.foreach (substitution, fn {formal, actual} =>
			     setVar (formal, SOME actual))
	    val exp = loopExp exp
	    val _ = destroyVar ()
	    val _ = destroyJump ()
	 in exp
	 end

      val alphaRename =
	 Trace.trace ("alphaRename", layout o #exp, layout) alphaRename
   end

structure DirectExp =
   struct
      open Dec PrimExp Transfer
      structure Cont =
	 struct
	    type t = PrimExp.t * Type.t -> Exp.t

	    val layout = Layout.ignore

	    fun nameGen (k: Var.t * Type.t -> Exp.t): t =
	       fn (e, t) =>
	       case e of
		  Var x => k (x, t)
		| _ => let val x = Var.newNoname ()
		       in Exp.prefix (k (x, t), Bind {var = x, ty = t, exp = e})
		       end
		    
	    fun name (k: Var.t -> Exp.t): t = nameGen (k o #1)

	    fun return (k: t, xt) = k xt

	    fun jump (j: Jump.t): t =
	       name (fn x =>
		     Exp.make {decs = [],
			       transfer = Jump {dst = j,
						args = Vector.new1 x}})

	    val id: t =
	       name (fn x => Exp.make {decs = [],
				       transfer = Return (Vector.new1 x)})

	    fun prefix (k: t, d: Dec.t): t =
	       fn z => Exp.prefix (return (k, z), d)
	 end
      
      type t = Cont.t -> Exp.t

      fun send (e: t, k: Cont.t): Exp.t = e k

      fun sendName (e: t, k) = send (e, Cont.name k)

      fun toExp e = send (e, Cont.id)

      val layout = Exp.layout o toExp

      val send =
	 Trace.trace2 ("DirectExp.send", layout, Cont.layout, Exp.layout) send

      fun const c = fn k => Cont.return (k, (Const c, Type.ofConst c))

      fun var (x, t) = fn k => Cont.return (k, (Var x, t))

      fun convertsGen (es: t vector, k: Var.t vector -> Exp.t): Exp.t =
	 let
	    val n = Vector.length es
	    fun loop (i, xs) =
	       if i = n
		  then k (Vector.fromListRev xs)
	       else
		  sendName (Vector.sub (es, i), fn x => loop (i + 1, x :: xs))
	 in loop (0, [])
	 end

      fun converts (es: t vector, make: Var.t vector -> PrimExp.t * Type.t): t =
	 fn k => convertsGen (es, k o make)

      fun convert (e: t, make: Var.t -> PrimExp.t * Type.t): t =
	 fn k => send (e, Cont.name (k o make))

      fun name2 (e1: t, e2: t, make: Var.t * Var.t -> t): t =
	 fn k =>
	 sendName (e1, fn x1 => sendName (e2, fn x2 => send (make (x1, x2), k)))

      fun seq (e1: t, e2: t): t = fn k => sendName (e1, fn _ => send (e2, k))

      fun tuple {exps: t vector, ty: Type.t}: t =
	 if 1 = Vector.length exps
	    then Vector.sub (exps, 0)
	 else converts (exps, fn xs => (PrimExp.Tuple xs, ty))

      fun select {tuple, offset, ty} =
	 convert (tuple, fn tuple =>
		  (Select {tuple = tuple, offset = offset}, ty))

      fun conApp {con, args, ty} =
	 converts (args, fn args => (ConApp {con = con,
					     args = args}, ty))

      fun reify (ty: Type.t, z: Jump.t option -> Exp.t): t =
	 fn k =>
	 let val result = Var.newNoname ()
	    val rest = Cont.return (k, (Var result, ty))
	    fun nontail () =
	       let val j = Jump.newNoname ()
	       in Exp.prefix (z (SOME j),
			      Fun {name = j,
				   args = Vector.new1 (result, ty),
				   body = rest})
	       end
	 in
	    case Exp.dest rest of
	       {decs = [], transfer = Return xs} =>
		  if 1 = Vector.length xs
		     then
			(* should also do something for returning unit *)
			if Var.equals (result, Vector.sub (xs, 0))
			   then z NONE
			else nontail ()
		  else nontail ()
	     | _ => nontail ()
	 end

      fun call {func, args, ty} =
	 reify
	 (ty, fn jo =>
	  convertsGen
	  (args, fn args =>
	   Exp.make {decs = [],
		     transfer = Call {func = func, args = args, cont = jo}}))

      fun raisee e =
	 fn _ => sendName (e, fn x =>
			   Exp.make {decs = [],
				     transfer = Raise (Vector.new1 x)})

      fun lett {decs, body} =
	 fn k =>
	 let
	    val rec loop =
	       fn [] => body k
		| {var, ty, exp} :: ds =>
		     send (exp, fn (e, _) =>
			   Exp.prefix
			   (loop ds, Bind {var = var, ty = ty, exp = e}))
	 in loop decs
	 end

      fun joinPoint (ty: Type.t, e: t): t =
	 reify (ty, fn jo => send (e, (case jo of
					  NONE => Cont.id
					| SOME j => Cont.jump j)))

      fun primApp {prim, targs, args, ty} =
	 let
	    fun app args =
	       PrimApp {prim = prim, info = PrimInfo.None,
			targs = targs, args = args}
	 in case Prim.name prim of
	    Prim.Name.MLton_halt =>
	       (fn k =>
		convertsGen (args, fn args =>
			     Exp.make {decs = [Dec.Bind {var = Var.newNoname (),
							 ty = Type.unit,
							 exp = app args}],
				       transfer = Transfer.Bug}))
	  | _ => converts (args, fn args => (app args, ty))
	 end

      fun primApp' {prim, overflow, targs, args, ty} k =
	 convertsGen
	 (args, fn args =>
	  let
	     val (info, k) =
		case overflow of
		   NONE => (PrimInfo.None, k)
		 | SOME e => 
		      let val j = Jump.newNoname ()
		      in (PrimInfo.Overflow j,
			  Cont.prefix (k, Dec.Fun {name = j,
						   args = Vector.new0 (),
						   body = toExp e}))
		      end
	     val primExp =
		PrimApp {prim = prim, info = info,
			 targs = targs, args = args}
	  in Cont.return (k, (primExp , ty))
	  end)

      datatype cases =
	 Char of (char * t) vector
	| Con of {con: Con.t,
		  args: (Var.t * Type.t) vector,
		  body: t} vector
	| Int of (int * t) vector
	| Word of (word * t) vector
	| Word8 of (Word8.t * t) vector

      fun casee {cause, test, ty, cases: cases, default} =
	 joinPoint
	 (ty, fn k =>
	  test
	  (Cont.nameGen
	   (fn (test, testTy) =>
	    let
	       val tycon = Type.detycon testTy
	       fun doCases (cases, finish, make) =
		  let
		     val (cases, decs) =
			Vector.mapAndFold
			(cases, [], fn (c, decs) =>
			 let
			    val (test, args, body) = make c
			    val e = send (body, k)
			    val j = Jump.newNoname ()
			 in ((test, j),
			     Fun {name = j, args = args, body = e} :: decs)
			 end)
		  in (finish cases, decs)
		  end
	       fun doit (l, f) =
		  doCases (l, f, fn (i, e) => (i, Vector.new0 (), e))
	       val (cases, decs) =
		  case cases of
		     Char l => doit (l, Cases.Char)
		   | Con cases =>
			doCases (cases, Cases.Con, fn {con, args, body} =>
				 (con, args, body))
		   | Int l => doit (l, Cases.Int)
		   | Word l => doit (l, Cases.Word)
		   | Word8 l => doit (l, Cases.Word8)
	       val (default, decs) =
		  case default of
		     NONE => (NONE, decs)
		   | SOME e =>
			let val j = Jump.newNoname ()
			in (SOME j,
			    Fun {name = j, args = Vector.new0 (),
				 body = send (e, k)}
			    :: decs)
			end
	    in Exp.make {decs = decs,
			 transfer = Case {cause = cause,
					  test = test,
					  cases = cases,
					  default = default}}
	    end)))
	 
      fun handlee {try, ty, catch, handler} =
	 joinPoint
	 (ty, fn k =>
	  let val h = Jump.newNoname ()
	  in Exp.prefixs
	     (send (try, Cont.prefix (k, HandlerPop)),
	      [Fun {name = h,
		    args = Vector.new1 catch,
		    body = Exp.prefix (send (handler, k), HandlerPop)},
	       HandlerPush h])
	  end)

      fun detupleGen (e: PrimExp.t, t: Type.t,
		      components: Var.t vector,
		      body: Exp.t): Exp.t =
	 Exp.prefixs
	 (body,
	  case Vector.length components of
	     0 => []
	   | 1 => [Bind {var = Vector.sub (components, 0), ty = t, exp = e}]
	   | _ =>
		let
		   val ts = Type.detuple t
		   val tupleVar = Var.newNoname ()
		in Bind {var = tupleVar, ty = t, exp = e}
		   ::
		   #2 (Vector.fold2 (components, ts,
				     (0, []), fn (x, t, (i, ac)) =>
				     (i + 1,
				      Bind {var = x, ty = t,
					    exp = Select {tuple = tupleVar,
							  offset = i}}
				      :: ac)))
		end)
	 
      fun detupleBind {tuple, components, body} =
	 fn k => send (tuple, fn (e, t) => detupleGen (e, t, components, body k))

      fun detuple {tuple, body}: t =
	 fn k =>
	 tuple
	 (fn (e, t) =>
	  case e of
	     Tuple xs => send (body xs, k)
	   | _ => let
		     val components =
			Vector.map (Type.detuple t, fn _ => Var.newNoname ())
		  in detupleGen (e, t, components, send (body components, k))
		  end)

      local
	 fun make c = conApp {con = c, args = Vector.new0 (), ty = Type.bool}
      in val truee = make Con.truee
	 val falsee = make Con.falsee
      end

      val int = const o Const.fromInt

      fun e1 + e2 =
	 primApp {prim = Prim.intPlus,
		  targs = Vector.new0 (),
		  args = Vector.new2 (e1, e2),
		  ty = Type.int}

      fun eq (e1, e2, ty) =
	 primApp {prim = Prim.eq,
		  targs = Vector.new1 ty,
		  args = Vector.new2 (e1, e2),
		  ty = Type.bool}
   end

structure Datatype =
   struct
      type t = {tycon: Tycon.t,
		cons: {con: Con.t,
		       args: Type.t vector} vector}

      fun layout (ds: t vector) =
	 let open Layout
	 in align (str "Datatypes:"
		   ::
		   Vector.toListMap
		   (ds, fn {tycon, cons} =>
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
			  "| ")]))
	 end
   end

structure Set = DisjointSet
   
structure Handler':
   sig
      type t

      val dest: t -> Jump.t
      val equals: t * t -> bool
      val layout: t -> Layout.t
      val new: Jump.t -> t
      val unknown: unit -> t
   end =
   struct
      datatype t = T of Jump.t option Set.t

      fun layout (T s) = Option.layout Jump.layout (Set.value s)
      val new = T o Set.singleton o SOME
      fun unknown () = T (Set.singleton NONE)

      fun equals (T s, T s'): bool =
	 Set.canUnion
	 (s, s',
	  fn (NONE, h) => SOME h
	   | (h, NONE) => SOME h
	   | (SOME h, SOME h') =>
		if Jump.equals (h, h') then SOME (SOME h) else NONE)

      val equals =
	 Trace.trace2 ("Handler'.equals", layout, layout, Bool.layout)
	 equals

      fun dest (T s) =
	 case Set.value s of
	    NONE => Error.bug "unknown handler"
	  | SOME h => h
   end

structure Handlers:
   sig
      type t

      val equals: t * t -> bool
      val empty: t
      val layout: t -> Layout.t
      val pop: t -> t
      val push: t * Handler'.t -> t
      val toList: t -> Jump.t list option
      val unknown: unit -> t
   end =
   struct
      datatype t = T of handlers Set.t
      and handlers =
	 Unknown
	| Empty
	| Push of {top: Handler'.t,
		   rest: t}

      fun layout (T s) =
	 let open Layout
	 in case Set.value s of
	    Unknown => str "Unknown"
	  | Empty => str "Empty"
	  | Push {top, rest} => seq [Handler'.layout top,
				     str " :: ",
				     layout rest]
	 end

      val new = T o Set.singleton
      val empty = new Empty
      fun unknown () = new Unknown
      fun push (rest: t, top: Handler'.t) = new (Push {top = top,
						       rest = rest})
      val push =
	 Trace.trace2 ("Handlers.push", layout, Handler'.layout, layout)
	 push

      fun pop (T s) =
	 case Set.value s of
	    Empty => Error.bug "pop of empty handler stack"
	  | Push {rest, ...} => rest
	  | Unknown =>
	       let val rest = unknown ()
	       in Set.setValue (s, Push {top = Handler'.unknown (),
					 rest = rest})
		  ; rest
	       end

      fun isEmpty (T s) =
	 case Set.value s of
	    Empty => true
	  | _ => false

      fun toList (hs: t): Jump.t list option =
	 let
	    fun loop (T s, ac) =
	       case Set.value s of
		  Unknown => NONE
		| Empty => SOME (rev ac)
		| Push {top, rest} => loop (rest, Handler'.dest top :: ac)
	 in loop (hs, [])
	 end

      fun equals (T s, T s'): bool =
	 Set.canUnion
	 (s, s',
	  fn (Unknown, hs) => SOME hs
	   | (hs, Unknown) => SOME hs
	   | (Empty, Empty) => SOME Empty
	   | (hs as Push {top = t, rest = r}, Push {top = t', rest = r'}) =>
		if Handler'.equals (t, t') andalso equals (r, r')
		   then SOME hs
		else NONE
	   | _ => NONE)	     
      val equals =
	 Trace.trace2 ("Handlers.equals", layout, layout, Bool.layout)
	 equals
   end

open Dec PrimExp Transfer

structure Function =
   struct
      type t = {name: Func.t,
		args: (Var.t * Type.t) vector,
		body: Exp.t,
		returns: Type.t vector}

      local
	 structure Graph = DirectedGraph
	 structure Node = Graph.Node
	 structure Edge = Graph.Edge
      in
	 fun layoutDot ({name, args, body, returns}: t, jumpHandlers) =
	    let
	       open Graph.LayoutDot
	       val {destroy, get = jumpNode, set = setJumpNode} =
		  Property.destGetSet
		  (Jump.plist,
		   Property.initRaise ("node", Jump.layout))
	       val {get = edgeOptions, set = setEdgeOptions} =
		  Property.getSetOnce
		  (Edge.plist, Property.initConst [])
	       val {get = nodeOptions, ...} =
		  Property.get
		  (Node.plist, Property.initFun (fn _ => ref []))
	       val g = Graph.new ()
	       fun addEdge (from, to, opts) =
		  let
		     val e = Graph.addEdge (g, {from = from,
						to = jumpNode to})
		     val _ = setEdgeOptions (e, opts)
		  in
		     ()
		  end
	       fun nodeOption (n, opt) =
		  List.push (nodeOptions n, opt)
	       val main = Graph.newNode g
	       fun loop (e: Exp.t, from: Node.t, name: string) =
		  let
		     val {decs, transfer} = Exp.dest e
		     fun edge (j: Jump.t,
			       label: string,
			       style: style): unit =
			addEdge (from, j,
				 [EdgeOption.Label label,
				  EdgeOption.Style style])
		     val _ =
			List.foreach
			(decs,
			 fn Bind {var, exp, ...} =>
			 (case exp of
			     PrimApp {info, ...} =>
				PrimInfo.foreachJump
				(info, fn j =>
				 edge
				 (j, "Overflow", Dashed))
			   | _ => ())
			  | Fun {name, body, ...} =>
			       let
				  val n = Graph.newNode g
				  val _ = setJumpNode (name, n)
			       in
				  loop (body, n, Jump.toString name)
			       end
			  | _ => ())
		     val rest =
			case transfer of
			   Bug => "\nbug"
			 | Call {func, cont, ...} =>
			      let
				 val f = Func.toString func
			      in
				 case cont of
				    NONE => concat ["\ntail ", f]
				  | SOME j =>
				       (edge (j, (concat
						  ["nontail ", f]),
					      Dotted)
					; (case jumpHandlers j of
					      h :: _ =>
						 edge
						 (h, "", Dotted)
					    | _ => ())
					; "")
			      end
			 | Case {cases, default, ...} =>
			      (nodeOption
			       (from, NodeOption.Shape Diamond)
			       ; let
				    fun doit (v, toString) =
				       Vector.foreach
				       (v, fn (x, j) =>
					edge (j, toString x, Solid))
				 in case cases of
				    Cases.Char v =>
				       doit (v, Char.toString)
				  | Cases.Con v =>
				       doit (v, Con.toString)
				  | Cases.Int v =>
				       doit (v, Int.toString)
				  | Cases.Word v =>
				       doit (v, Word.toString)
				  | Cases.Word8 v =>
				       doit (v, Word8.toString)
				 end
			       ; (case default of
				     NONE => ()
				   | SOME j =>
					edge (j, "default", Solid))
				 ; "")
			 | Jump {dst, ...} =>
			      (edge (dst, "", Solid)
			       ; "")
			 | Raise _ => "\nraise"
			 | Return _ => "\nreturn"
		     val _ = 
			nodeOption
			(from,
			 NodeOption.Label (concat [name, rest]))
		  in
		     ()
		  end
	       val _ = loop (body, main, Func.toString name)
	       val l =
		  Graph.LayoutDot.layout
		  {graph = g,
		   title = Func.toString name,
		   options = [],
		   edgeOptions = edgeOptions,
		   nodeOptions = ! o nodeOptions}
	       val _ = destroy ()
	    in
	       l
	    end
      end
   
      fun layout (func as {name, args, body, returns}, jumpHandlers) =
	 let
	    val _ =
	       if !Control.keepDot
		  then
		     File.withOut
		     (concat [!Control.inputFile, ".",
			      Func.toString name, ".dot"],
		      fn out =>
		      Layout.outputl (layoutDot (func, jumpHandlers), out))
	       else ()
	    open Layout
	 in align [seq [str "fun ",
			Func.layout name,
			str " ",
			layoutFormals args,
			if !Control.showTypes
			   then seq [str ": ",
				     Vector.layout Type.layout returns]
			else empty,
			   str " = "],
		   let
		      val body =
			 if false
			    then Exp.layout body
			 else Exp.layoutFlat (body, jumpHandlers)
		   in indent (body, !Control.indentation)
		   end]
	 end
      
      fun layouts (fs, jumpHandlers, output: Layout.t -> unit): unit =
	 Vector.foreach (fs, fn f => output (layout (f, jumpHandlers)))
   end

structure Program =
   struct
      datatype t =
	 T of {
	       datatypes: Datatype.t vector,
	       globals: {
			 var: Var.t,
			 ty: Type.t,
			 exp: PrimExp.t
			 } vector,
	       functions: Function.t vector,
	       main: Func.t
	       }
   end

val traceJump = Trace.trace ("InferHandlers.jump", Jump.layout, Handlers.layout)
val traceSetJump =
   Trace.trace2
   ("InferHandlers.setJump", Jump.layout, Handlers.layout, Unit.layout)
   
fun inferHandlers (Program.T {functions, ...}) =
   let
      val {get = jump, set = setJump} =
	 Property.getSetOnce (Jump.plist,
			      Property.initRaise ("handlers", Jump.layout))
      val jump = traceJump jump
      val setJump = traceSetJump setJump
      val _ =
	 Vector.foreach
	 (functions, fn {body, ...} =>
	  let
	     fun loop (e: Exp.t, hs: Handlers.t): unit =
		let
		   fun check (msg, hs, hs') =
		      let
			 fun disp () =
			    Control.message
			    (Control.Silent, fn () =>
			     let open Layout
			     in align [seq [str "exp: ", Exp.layout e],
				       seq [str "hs: ", Handlers.layout hs],
				       seq [str "hs': ", Handlers.layout hs']]
			     end)
		      in if Handlers.equals (hs, hs')
			    then ()
			 else (disp ()
			       ; Error.bug (concat
					    ["handler stack mismatch at ", msg]))
		      end
		   val {decs, transfer} = Exp.dest e
		   val after =
		      List.fold
		      (decs, hs, fn (d, hs) =>
		       case d of
			  Bind {exp = PrimApp {info = PrimInfo.Overflow j, ...},
				...} =>
			  (check ("PrimApp", hs, jump j)
			   ; hs)
			      | Fun {name, body, ...} =>
				   (let val hs = Handlers.unknown ()
				    in setJump (name, hs)
				    end
				       ; loop (body, jump name)
				       ; hs)
			      | HandlerPush h =>
				   let
				      val hs = Handlers.push (hs, Handler'.new h)
				   in check ("push", hs, jump h)
				      ; hs
				   end
			      | HandlerPop => Handlers.pop hs
			      | _ => hs)
		   fun checkJump j = check ("jump", after, jump j)
		   fun empty msg = check (msg, after, Handlers.empty)
		in case transfer of
		   Bug => ()
		 | Call {cont = c, ...} =>
		      (case c of
			  NONE => empty "tail call"
			| SOME c => check ("nontail call", after, jump c))
		 | Case {cases, default, ...} =>
		      (Cases.foreach (cases, checkJump)
		       ; Option.app (default, checkJump))
		 | Jump {dst, ...} => checkJump dst
		 | Raise _ => ()
		 | Return _ => empty "return"
		end
	  in loop (body, Handlers.empty)
	  end)
   in
      fn j => (case Handlers.toList (jump j) of
		  NONE => Error.bug (concat ["toList (jump ",
					     Jump.toString j,
					     ") of unknown handler stack"])
		| SOME l => l)
   end

val inferHandlers = Control.trace (Control.Pass, "inferHandlers") inferHandlers
   
fun deltaHandlers (d, hs) =
   case d of
      HandlerPush h => h :: hs
    | HandlerPop => (case hs of
			_ :: hs => hs
		      | _ => Error.bug "deltaHandlers")
    | _ => hs

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
	       open Graph.LayoutDot
	       val g = Graph.new ()
	       val {get = nodeOptions, set = setNodeOptions, ...} =
		  Property.getSetOnce
		  (Node.plist, Property.initRaise ("options", Node.layout))
	       val {get = funcNode, destroy} =
		  Property.destGet
		  (Func.plist, Property.initFun
		   (fn f =>
		    let
		       val n = Graph.newNode g
		       val _ = setNodeOptions (n, [NodeOption.Label
						   (Func.toString f)])
		    in
		       n
		    end))
	       val {get = edgeOptions, set = setEdgeOptions} =
		  Property.getSetOnce (Edge.plist, Property.initConst [])
	       val _ =
		  Vector.foreach
		  (functions, fn {name, body, ...} =>
		   let
		      val from = funcNode name
		      val {get, destroy} =
			 Property.destGet
			 (Node.plist,
			  Property.initFun (fn _ => {nontail = ref false,
						     tail = ref false}))
		      val _ = 
			 Exp.foreachCall
			 (body, fn {func, cont, ...} =>
			  let
			     val to = funcNode func
			     val {tail, nontail} = get to
			     val r = if isSome cont then nontail else tail
			  in
			     if !r
				then ()
			     else (r := true
				   ; (setEdgeOptions
				      (Graph.addEdge (g, {from = from, to = to}),
				       if isSome cont
					  then []
				       else [EdgeOption.Style Dotted])))
			  end)
		      val _ = destroy ()
		   in
		      ()
		   end)
	       val l =
		  Graph.LayoutDot.layout
		  {graph = g,
		   title = title,
		   options = [],
		   edgeOptions = edgeOptions,
		   nodeOptions = nodeOptions}
	       val _ = destroy ()
	    in
	       l
	    end
      end

      fun foreachVar (T {globals, functions, ...}, f) =
	 (Vector.foreach (globals, fn {var, ty, ...} => f (var, ty))
	  ; Vector.foreach (functions, fn {args, body, ...} =>
			   (Vector.foreach (args, f)
			    ; Exp.foreachVar (body, f))))
	 
      fun layouts (p as T {datatypes, globals, functions, main},
		   output': Layout.t -> unit) =
	 let
	    val jumpHandlers = inferHandlers p
	    open Layout
	    val output = output'
	 in output (Datatype.layout datatypes)
	    ; output (str "\n\nGlobals:")
	    ; Vector.foreach (globals, output o Bind.layout)
	    ; output (str "\n\nFunctions:")
	    ; Function.layouts (functions, jumpHandlers, output)
	    ; output (seq [str "\n\nMain: ", Func.layout main])
	    ; if not (!Control.keepDot)
	       then ()
	      else
		 File.withOut
		 (concat [!Control.inputFile, ".dot"],
		  fn out =>
		  Layout.outputl (layoutCallGraph (p, !Control.inputFile),
				  out))
	 end

      fun layout (p as T {datatypes, globals, functions, main}) =
	 let
	    val jumpHandlers = inferHandlers p
	    open Layout
	 in align [Datatype.layout datatypes,
		   str "Globals:",
		   align (Vector.toListMap (globals, Bind.layout)),
		   str "Functions:",
		   align (Vector.toListMap (functions, fn f =>
					    Function.layout (f, jumpHandlers))),
		   seq [str "Main: ", Func.layout main]]
	 end

      fun layoutStats (T {datatypes, globals, functions, ...}) =
	 let
	    val numTypes = ref 0
	    fun inc _ = Int.inc numTypes
	    val {hom = countType, destroy} =
	       Type.makeHom
	       {var = fn _ => Error.bug "cps-tree saw var",
		con = inc}
	    val numPrimExps = ref (Vector.length globals)
	    val numLocalFunctions = ref 0
	    val functionSizes = ref 0
	    val _ =
	       Vector.foreach
	       (functions, fn {body, ...} =>
		Exp.foreach'
		(body,
		 {handleDec = let open Dec
			      in fn Bind {ty, ...} => (countType ty
						       ; Int.inc numPrimExps)
			    | Fun {args, ...} =>
				 (Int.inc numLocalFunctions
				  ; Vector.foreach (args, countType o #2))
			    | _ => ()
			      end,
			   handleTransfer = fn _ => ()}))
	    val numFunctions = Vector.length functions
	    open Layout
	    val _ = destroy ()
	 in
	    align
	    (List.map
	     ([("num functions", Int.layout numFunctions),
	       ("num local functions", Int.layout (!numLocalFunctions)),
	       ("num primExps", Int.layout (!numPrimExps))],
	      fn (name, value) => seq [str (name ^ " "), value]))
	 end

      (* clear all property lists reachable from program *)
      fun clear (T {datatypes, globals, functions, main}) =
	 ((* Can't do Type.clear because it clears out the info needed for
	   * dest.
	   *)
	  (* Type.clear () *)
	  Vector.foreach (datatypes, fn {tycon, cons} =>
			 (Tycon.clear tycon
			  ; Vector.foreach (cons, Con.clear o #con)))
	  ; Vector.foreach (globals, Bind.clear)
	  ; Vector.foreach (functions, fn {name, args, body, returns} =>
			    (Func.clear name
			     ; Vector.foreach (args, Var.clear o #1)
			     ; Exp.clear body)))

      fun hasPrim (T {globals, functions, ...},  f) =
	 DynamicWind.withEscape
	 (fn escape =>
	  let
	     val loopBind: Bind.t -> unit =
		fn {exp = PrimExp.PrimApp {prim, ...}, ...} =>
		if f prim
		   then escape true
		else ()
		 | _ => ()
	     val _ = Vector.foreach (globals, loopBind)
	     val _ = Vector.foreach (functions, fn {body, ...} =>
				    Exp.foreachBind (body, loopBind))
	  in
	     false
	  end)
   end

val emptyVars: Var.t vector = Vector.new0 ()
   
end
