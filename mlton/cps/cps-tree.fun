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

      fun layoutFlat (e: t): Layout.t =
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
				| Dec.Fun _ => ac
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
					     str " = "]]],
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

structure Function =
   struct
      type t = {name: Func.t,
		args: (Var.t * Type.t) vector,
		body: Exp.t,
		returns: Type.t vector}

      structure Graph = DirectedGraph
      structure Node = Graph.Node
	 
      fun layout {name, args, body, returns} =
	 let open Layout
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
			 else Exp.layoutFlat body
		   in indent (body, !Control.indentation)
		   end,
		   if true
		      then
			 let
			    open Graph.LayoutDot
			    val {destroy, get = jumpNode, set = setJumpNode} =
			       Property.destGetSet
			       (Jump.plist,
				Property.initRaise ("node", Jump.layout))
			    val {get = nodeOptions, ...} =
			       Property.get
			       (Node.plist,
				Property.initFun (fn _ => ref []))
			    val g = Graph.new ()
			    fun addLabel (n, l) =
			       List.push (nodeOptions n, NodeOption.Label l)
			    val main = Graph.newNode g
			    val _ = addLabel (main, Func.toString name)
			    fun loop (e, from) =
			       let
				  val {decs, transfer} = Exp.dest e
				  val _ =
				     List.foreach
				     (decs,
				      fn Dec.Fun {name, body, ...} =>
				           let
					      val n = Graph.newNode g
					      val _ = setJumpNode (name, n)
					      val _ =
						 addLabel (n, Jump.toString name)
					   in
					      loop (body, n)
					   end
				       | _ => ())
				  val _ =
				     Transfer.foreachJump
				     (transfer, fn j =>
				      (Graph.addEdge (g, {from = from,
							  to = jumpNode j})
				       ; ()))
			       in
				  ()
			       end
			    val _ = loop (body, main)
			    val l =
			       Graph.LayoutDot.layout
			       {graph = g,
				title = Func.toString name,
				options = [],
				edgeOptions = fn _ => [],
				nodeOptions = ! o nodeOptions}
			    val _ = destroy ()
			 in
			    l
			 end
		   else empty]
	 end
      
      fun layouts (fs, output) =
	 Vector.foreach (fs, output o layout)
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

      fun foreachVar (T {globals, functions, ...}, f) =
	 (Vector.foreach (globals, fn {var, ty, ...} => f (var, ty))
	  ; Vector.foreach (functions, fn {args, body, ...} =>
			   (Vector.foreach (args, f)
			    ; Exp.foreachVar (body, f))))
	 
      fun layouts (T {datatypes, globals, functions, main},
		   output': Layout.t -> unit) =
	 let open Layout
	    val output = output'
	 in output (Datatype.layout datatypes)
	    ; output (str "\n\nGlobals:")
	    ; Vector.foreach (globals, output o Bind.layout)
	    ; output (str "\n\nFunctions:")
	    ; Function.layouts (functions, output)
	    ; output (seq [str "\n\nMain: ", Func.layout main])
	 end

      fun layout (T {datatypes, globals, functions, main}) =
	 let open Layout
	 in align [Datatype.layout datatypes,
		   str "Globals:",
		   align (Vector.toListMap (globals, Bind.layout)),
		   str "Functions:",
		   align (Vector.toListMap (functions, Function.layout)),
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
