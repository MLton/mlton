(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Shrink (S: SHRINK_STRUCTS): SHRINK = 
struct

open S
open Dec PrimExp Transfer
type int = Int.t

structure Position =
   struct
      datatype t =
	 Formal of int
       | Free of Var.t

      fun layout (p: t) =
	 case p of
	    Formal i => Int.layout i
	  | Free x => Var.layout x

      val equals =
	 fn (Formal i, Formal i') => i = i'
	  | (Free x, Free x') => Var.equals (x, x')
	  | _ => false
   end

structure Positions =
   struct
      local
	 structure A = MonoVector (Position)
      in
	 open A
      end

      fun usesFormal (ps: t): bool =
	 exists (ps, fn Position.Formal _ => true | _ => false)
   end

structure JumpInfo =
   struct
      (* IsLiftableCase iff f is of the form
       * fun f (x) =
       *   let fun L1
       *       ...
       *       fun Ln
       *   in case x of ...
       *   end
       * where x only occurs once.
       * In this case, f will be rewritten as
       *   fun L1
       *   ...
       *   fun Ln
       *   fun f (x) = case x of ...
       * The Li's will be called directly at places where the constructor is
       * known.
       * Where the constructor is not known, f will be called.
       *)
      (* !numOccurrences is the number of calls to the jump from outside its
       * body.
       *)
      datatype t = T of {meaning: meaning,
			 name: Jump.t,
			 args: (Var.t * Type.t) vector,
			 numOccurrences: int ref}
      and meaning =
	 Code of {body: Exp.t,
		  formals: (Var.t * Type.t) vector,
		  isLiftableCase: {cases: t Cases.t,
				   default: t option} option ref,
		  isRecursive: bool ref}
	| Jump of {dst: t,
		   args: Positions.t}
	| Raise of Positions.t
	| Return of Positions.t

      fun layout (T {meaning, name, numOccurrences, ...}) = 
	 let open Layout
	 in record [("name", Jump.layout name),
		    ("numOccurrences", Int.layout (!numOccurrences)),
		    ("meaning",
		     case meaning of
			Code {isLiftableCase, isRecursive, ...} =>
			   seq [str "Code ",
				record [("isRecursive",
					 Bool.layout (!isRecursive)),
					("isLiftableCase",
					 Option.layout Layout.ignore
					 (!isLiftableCase))]]
		      | Jump {dst, args} =>
			   seq [str "Jump",
				tuple [layout dst, Positions.layout args]]
		      | Raise ps => seq [str "Raise ", Positions.layout ps]
		      | Return ps => seq [str "Return ", Positions.layout ps])]
	 end

      fun usesFormal (T {meaning, ...}) =
	 case meaning of
	    Code {formals, ...} => not (Vector.isEmpty formals)
	  | Jump {args, ...} => Positions.usesFormal args
	  | Raise xs => Positions.usesFormal xs
	  | Return xs => Positions.usesFormal xs

      val usesFormal =
	 Trace.trace ("JumpInfo.usesFormal", layout, Bool.layout) usesFormal

      local fun make s (T r) = s r
      in
	 val name = make #name
      end

      fun inc (T {numOccurrences = r, ...}, n: int): unit = r := n + !r
	 
      fun equals (T {meaning = m, ...}, T {meaning = m', ...}) =
	 case (m, m') of
	    (Code {isRecursive = i, ...}, Code {isRecursive = i', ...}) =>
	       i = i'
	| (Code {isRecursive = i, ...},
	   Jump {dst = T {meaning = Code {isRecursive = i', ...}, ...},
		 args, ...}) => Vector.isEmpty args andalso i = i'
	| (Jump {dst = T {meaning = Code {isRecursive = i', ...}, ...},
		 args, ...},
	   Code {isRecursive = i, ...}) => Vector.isEmpty args andalso i = i'
	 | (Jump {dst = d, args = a}, Jump {dst = d', args = a'}) =>
	      equals (d, d') andalso Positions.equals (a, a')
	 | (Raise ps, Raise ps') => Positions.equals (ps, ps')
	 | (Return ps, Return ps') => Positions.equals (ps, ps')
	 | _ => false

      fun jump (j as T {meaning, ...}, ps: Positions.t): meaning =
	 let
	    fun extract (ps': Positions.t): Positions.t =
	       Vector.map (ps', let open Position
			       in fn p =>
				  case p of
				     Free x => Free x
				   | Formal i => Vector.sub (ps, i)
			       end)
	 in case meaning of
	    Code _ => Jump {dst = j, args = ps}
	  | Jump {dst, args} => Jump {dst = dst, args = extract args}
	  | Raise ps => Raise (extract ps)
	  | Return ps => Return (extract ps)
	 end

      fun isTail (T {args, meaning, ...}): bool =
	 case meaning of
	    Return ps =>
	       Vector.length args = Vector.length ps
	       andalso Vector.foralli (ps,
				      fn (i, Position.Formal i') => i = i'
				       | _ => false)
	  | _ => false
   end

structure VarInfo =
   struct
      datatype t = T of {var: Var.t,
			 numOccurrences: int ref,
			 value: value option ref}
      and value =
	 Con of {con: Con.t,
		 args: t vector}
	| Const of Const.t
	| Tuple of t vector

      fun equals (T {var = x, ...}, T {var = y, ...}) = Var.equals (x, y)
	 
      fun layout (T {var, numOccurrences, value}) =
	 let open Layout
	 in record [("var", Var.layout var),
		    ("numOccurrences", Int.layout (!numOccurrences)),
		    ("value", Option.layout layoutValue (!value))]
	 end
      and layoutValue v =
	 let open Layout
	 in case v of
	    Con {con, args} => seq [Con.layout con,
				    Vector.layout layout args]
	  | Const c => Const.layout c
	  | Tuple vis => Vector.layout layout vis
	 end

      fun new (x: Var.t) = T {var = x,
			      numOccurrences = ref 0,
			      value = ref NONE}

      fun var (T {var, ...}): Var.t = var
      fun numOccurrences (T {numOccurrences = r, ...}) = r
      fun value (T {value, ...}): value option = !value
   end

structure Value =
   struct
      datatype t = datatype VarInfo.value

      val layout = VarInfo.layoutValue

      fun fromBool (b: bool): t =
	 Con {con = if b then Con.truee else Con.falsee,
	      args = Vector.new0 ()}

      fun toPrimExp (v: t): PrimExp.t =
	 case v of
	    Con {con, args} =>
	       PrimExp.ConApp {con = con,
			       args = Vector.map (args, VarInfo.var)}
	  | Const c => PrimExp.Const c
	  | Tuple xs => PrimExp.Tuple (Vector.map (xs, VarInfo.var))
   end

val traceApply =
   Trace.trace ("Prim.apply",
		fn (p, args, _: VarInfo.t * VarInfo.t -> bool) =>
		let open Layout
		in seq [Prim.layout p,
			List.layout (Prim.ApplyArg.layout
				     (Var.layout o VarInfo.var)) args]
		end,
		Prim.ApplyResult.layout (Var.layout o VarInfo.var))
val traceSimplifyBind =
   Trace.trace2 ("Shrink.simplifyBind", layoutBind,
		 Layout.ignore: (unit -> Exp.t) -> Layout.t,
		 Layout.ignore: Exp.t -> Layout.t)
val traceSimplifyExp = Trace.trace ("simplifyExp", Exp.layout, Exp.layout)
val traceSimplifyTransfer =
   Trace.trace ("Shrink.simplifyTransfer", Transfer.layout, Exp.layout)
val traceJump =
   Trace.trace ("Shrink.jump",
		fn (dst, args) =>
		Layout.tuple [JumpInfo.layout dst,
			      Vector.layout VarInfo.layout args],
		Exp.layout)
val traceDeleteExp = Trace.trace ("Shrink.deleteExp", Exp.layout, Unit.layout)
val deleteJumpInfo' = Trace.info "deleteJumpInfo"
val traceSimplifyCase =
   Trace.trace ("Cps.Shrink.simplifyCase",
		Transfer.layoutCase, Exp.layout)
   
fun shrinkExp globals =
   let
      (* varInfo can't be getSetOnce because of setReplacement. *)
      val {get = varInfo: Var.t -> VarInfo.t, set = setVarInfo} =
	 Property.getSet (Var.plist, Property.initFun VarInfo.new)
      val varInfo =
	 Trace.trace ("Shrink.varInfo", Var.layout, VarInfo.layout) varInfo
      val setVarInfo =
	 Trace.trace2 ("setVarInfo", Var.layout, VarInfo.layout, Unit.layout)
	 setVarInfo
      fun varInfos xs = Vector.map (xs, varInfo)
      val {get = jumpInfo: Jump.t -> JumpInfo.t, set = setJumpInfo} =
	 Property.getSetOnce (Jump.plist,
			      Property.initRaise ("Shrink.info", Jump.layout))
      val jumpInfo =
	 Trace.trace ("Shrink.jumpInfo", Jump.layout, JumpInfo.layout) jumpInfo
      fun simplifyVar (x: Var.t) = VarInfo.var (varInfo x)
      val simplifyVar =
	 Trace.trace ("simplifyVar", Var.layout, Var.layout) simplifyVar
      fun simplifyVars xs = Vector.map (xs, simplifyVar)
      fun incNumOccurrences (r: int ref, n: int): unit =
	 let val new = n + !r
	    val _ = Assert.assert ("incNumOccurrences", fn () => new >= 0)
	 in r := new
	 end
      fun incVarInfo (x: VarInfo.t, n: int): unit =
	 incNumOccurrences (VarInfo.numOccurrences x, n)
      val incVarInfo =
	 Trace.trace2 ("incVarInfo", VarInfo.layout, Int.layout, Unit.layout)
	 incVarInfo
      fun incVar (x: Var.t, n: int): unit = incVarInfo (varInfo x, n)
      val incVar =
	 Trace.trace2 ("incVar", Var.layout, Int.layout, Unit.layout) incVar
      fun deleteVarInfo i = incVarInfo (i, ~1)
      fun deleteVarInfos is = Vector.foreach (is, deleteVarInfo)
      fun deleteVar x = incVar (x, ~1)
      val deleteVar =
	 Trace.trace ("deleteVar", Var.layout, Unit.layout) deleteVar
      fun deletePosition (p: Position.t): unit =
	 case p of
	    Position.Free x => deleteVar x
	  | _ => ()
      fun numVarOccurrences (x: Var.t): int =
	 ! (VarInfo.numOccurrences (varInfo x))
      fun setReplacement (x: Var.t, i: VarInfo.t): unit =
	 let val VarInfo.T {numOccurrences = r, ...} = varInfo x
	 in incVarInfo (i, !r)
	    ; setVarInfo (x, i)
	 end
      val setReplacement =
	 Trace.trace2 ("setReplacement", Var.layout, VarInfo.layout, Unit.layout)
	 setReplacement
      fun addVar (x: Var.t): unit = incVar (x, 1)
      val addVar = Trace.trace ("addVar", Var.layout, Unit.layout) addVar
      fun addVarInfo (x: VarInfo.t): unit = incVarInfo (x, 1)
      fun addPositions (ps: Positions.t): unit =
	 Vector.foreach (ps,
			fn Position.Free x => addVar x
			 | _ => ())
      fun addJumpInfo j = JumpInfo.inc (j, 1)
      fun addJump j = addJumpInfo (jumpInfo j)
      fun addJumpMeaning (m: JumpInfo.meaning) =
	 let datatype z = datatype JumpInfo.meaning
	 in case m of
	    Code _ => ()
	  | Jump {dst, args} => (addJumpInfo dst
				 ; addPositions args)
	  | Raise ps => addPositions ps
	  | Return ps => addPositions ps
	 end
      val _ =
	 Vector.foreach
	 (globals, fn {var, exp, ty} =>
	  let
	     fun construct v =
		setVarInfo (var, VarInfo.T {var = var,
					    numOccurrences = ref 0,
					    value = ref (SOME v)})
	  in case exp of
	     Var y => setVarInfo (var, varInfo y)
	   | Const c => construct (Value.Const c)
	   | Tuple xs => construct (Value.Tuple (Vector.map (xs, varInfo)))
	   | ConApp {con, args} =>
		construct (Value.Con {con = con,
				      args = Vector.map (args, varInfo)})
	   | _ => ()
	  end)
   in fn (exp: Exp.t, mayDelete: bool) =>
      let
	 (* Compute occurrence counts for both variables and jumps. *)
	 val _ =
	    let
	       fun loopExp e =
		  let val {decs, transfer} = Exp.dest e
		  in List.foreach (decs, loopDec)
		     ; Transfer.foreachJumpVar (transfer, addJump, addVar)
		  end
	       and loopDec d =
		  case d of
		     Bind {exp, ...} =>
			PrimExp.foreachJumpVar (exp, addJump, addVar)
		   | Fun {name, args, body} =>
			let
			   val {decs, transfer} = Exp.dest body
			   val numOccurrences = ref 0
			   fun set (m: JumpInfo.meaning) =
			      (addJumpMeaning m
			       ; (setJumpInfo
				  (name,
				   JumpInfo.T {name = name,
					       args = args,
					       numOccurrences = numOccurrences,
					       meaning = m})))
			   fun normal () =
			      let
				 val isLiftableCase = ref NONE
				 val isRecursive = ref false
				 val _ =
				    set (JumpInfo.Code
					 {body = body,
					  formals = args,
					  isLiftableCase = isLiftableCase,
					  isRecursive = isRecursive})
				 val _ = loopExp body
				 val _ =
				    if !numOccurrences > 0
				       then isRecursive := true
				   else
				      case (Vector.length args, transfer) of
					 (1, Case {test, cases, default, ...}) =>
					    let val (x, _) = Vector.sub (args, 0)
					    in if Var.equals (x, test)
					       andalso
					       List.forall (decs,
							    fn Fun _ => true
							     | _ => false)
					       andalso 1 = numVarOccurrences x
						  then
						     (isLiftableCase :=
						      SOME {cases =
							    Cases.map (cases,
								       jumpInfo),
							    default =
							    Option.map (default,
									jumpInfo)})
					       else ()
					    end
				       | _ => ()
				 val _ = numOccurrences := 0
			      in ()
			      end
			   fun extract (actuals: Var.t vector): Positions.t =
			      let
				 val {get: Var.t -> Position.t, set, destroy} =
				    Property.destGetSetOnce
				    (Var.plist, Property.initFun Position.Free)
				 val _ =
				    Vector.foreachi (args, fn (i, (x, _)) =>
						    set (x, Position.Formal i))
				 val ps = Vector.map (actuals, get)
				 val _ = destroy ()
			      in ps
			      end
			   fun sameAsArgs args' =
			      Vector.equals (args, args', fn ((x, _), x') =>
					    Var.equals (x, x'))
			in case (decs, transfer) of
			   ([], Jump {dst, args}) =>
			      if Jump.equals (dst, name)
				 then normal ()
			      else
				 if sameAsArgs args
				    then setJumpInfo (name, jumpInfo dst)
				 else
				    set (JumpInfo.jump (jumpInfo dst,
							extract args))
			 | ([], Raise xs) => set (JumpInfo.Raise (extract xs))
			 | ([], Return xs) => set (JumpInfo.Return (extract xs))
			 | _ => normal ()
			end
		   | HandlerPop => ()
		   | HandlerPush h => addJump h
	    in loopExp exp
	    end
	 local
	    val {get: Jump.t -> bool ref} =
	       Property.get (Jump.plist, Property.initFun (fn _ => ref false))
	 in
	    val amInJump = ! o get
	    fun withinJump (j: Jump.t, f: unit -> 'a): 'a =
	       let val r = get j
	       in r := true
		  ; f () before r := false
	       end
	 end
	 fun makeBody (body, isLiftableCase) =
	    if isSome (!isLiftableCase)
	       then Exp.make {decs = [],
			      transfer = Exp.transfer body}
	    else body
	 fun deletePositions (ps: Positions.t): unit =
	    Vector.foreach (ps,
			   fn Position.Free x => deleteVar x
			    | _ => ())
	 (* Deleting jumps and expressions. *)
	 fun deleteJumpMeaning (m: JumpInfo.meaning, j: Jump.t): unit =
	    let datatype z = datatype JumpInfo.meaning
	    in case m of
	       Code {body, isLiftableCase, ...} =>
		  withinJump (j, fn () =>
			      deleteExp (makeBody (body, isLiftableCase)))
	     | Jump {dst, args} => (deleteJumpInfo dst; deletePositions args)
	     | Raise ps => deletePositions ps
	     | Return ps => deletePositions ps
	    end
	 and deleteJump (j: Jump.t): unit = deleteJumpInfo (jumpInfo j)
	 and deleteJumpInfo arg: unit =
	    Trace.traceInfo (deleteJumpInfo',
			     JumpInfo.layout,
			     Unit.layout,
			     Trace.assertTrue)
	    (fn (JumpInfo.T {meaning, name, numOccurrences, ...}) =>
	    if amInJump name
	       then ()
	    else
	       let
		  val new = !numOccurrences - 1
		  val _ = Assert.assert ("deleteJumpInfo", fn () => new >= 0)
		  val _ = numOccurrences := new
	       in if new = 0
		     then deleteJumpMeaning (meaning, name)
		  else ()
	       end) arg
	 and deleteExp arg : unit =
	    traceDeleteExp
	    (fn (exp: Exp.t) =>
	     let val {decs, transfer} = Exp.dest exp
	     in List.foreach (decs, deleteDec)
		; Transfer.foreachJumpVar (transfer, deleteJump, deleteVar)
	     end) arg
	 and deleteDec d =
	    case d of
	       Bind {exp, ...} =>
		  PrimExp.foreachJumpVar (exp, deleteJump, deleteVar)
	     | Fun {name, body, ...} =>
		  let
		     val JumpInfo.T {meaning, name = n, numOccurrences, ...} =
			jumpInfo name
		  in if 0 = !numOccurrences andalso Jump.equals (name, n)
			then deleteJumpMeaning (meaning, name)
		     else ()
		  end
	     | HandlerPush h => deleteJump h
	     | _ => ()
	 (* Pre: the args counts are correct. *)
	 fun jump arg: Exp.t =
	    traceJump
	    (fn (info as JumpInfo.T {meaning, name, numOccurrences, ...},
		 args: VarInfo.t vector) =>
	     let
		fun extract (ps: Positions.t): VarInfo.t vector =
		   Vector.map (ps, fn p =>
			      let
				 val i =
				    case p of
				       Position.Formal i => Vector.sub (args, i)
				     | Position.Free x => varInfo x
				 val _ = addVarInfo i
			      in i
			      end)
		fun rr (f: Var.t vector -> Transfer.t, ps: Positions.t): Exp.t =
		   (deleteJumpInfo info
		    ; Exp.fromTransfer (f (Vector.map (extract ps, VarInfo.var))))
	     in case meaning of
		JumpInfo.Code {body, formals, isLiftableCase,
			       isRecursive, ...} =>
		   if 1 = !numOccurrences andalso not (!isRecursive)
		      then (numOccurrences := 0
			    ; Vector.foreach2 (formals, args, fn ((x, _), i) =>
					      setReplacement (x, i))
			    ; deleteVarInfos args
			    ; simplifyExp (makeBody (body, isLiftableCase)))
		   else
		      let
			 fun jump (j: Jump.t, args: VarInfo.t vector): Exp.t =
			    Exp.fromTransfer
			    (Jump {dst = j,
				   args = Vector.map (args, VarInfo.var)})
		      in
			 case (!isLiftableCase, Vector.length args) of
			    (SOME {cases, default}, 1) =>
			       let
				  val VarInfo.T {numOccurrences, value, ...} =
				     Vector.sub (args, 0)
				  fun doit (cases, is, args) =
				     let
					val jump =
					   fn (j, args) =>
					   (deleteJumpInfo info
					    ; JumpInfo.inc (j, 1)
					    ; jump (JumpInfo.name j, args))
				     in case Vector.peek (cases, fn (i, _) =>
							  is i) of
					NONE =>
					   (case default of
					       NONE => Exp.bug
					     | SOME j =>
						  jump (j, Vector.new0 ()))
				      | SOME (_, j) =>
					   (incNumOccurrences (numOccurrences,
							       ~1)
					    ; Vector.foreach (args, addVarInfo)
					    ; jump (j, args))
				     end
				  fun doCases (cases, v) =
				     case (cases, v) of
					(Cases.Con cases, Value.Con {con, args}) => 
					   doit (cases, fn c =>
						 Con.equals (c, con), args)
				      | (_, Value.Const c) =>
					   let
					      val doit = fn (l, x) =>
						 doit (l, fn x' => x = x',
						       Vector.new0 ())
					   in case (cases, Const.node c) of
					      (Cases.Char l, Const.Node.Char c) =>
						 doit (l, c)
					    | (Cases.Int l, Const.Node.Int i) =>
						 doit (l, i)
					    | (Cases.Word l, Const.Node.Word w) =>
						 doit (l, w)
					    | (Cases.Word8 l, Const.Node.Word w) =>
						 doit (l, Word8.fromWord w)
					    | _ =>
						 Error.bug "strange constant for Cases.Int"
					   end
				      | _ => Error.bug "strange Case with constant test"
			       in
				  case !value of
				     NONE => jump (name, args)
				   | SOME v => doCases (cases, v)
			       end
			  | _ => jump (name, args)
		      end
			  | JumpInfo.Jump {dst, args} =>
			       (addJumpInfo dst
				; deleteJumpInfo info
				; jump (dst, extract args))
			  | JumpInfo.Raise ps => rr (Raise, ps)
			  | JumpInfo.Return ps => rr (Return, ps)
	     end) arg
	 and simplifyExp arg : Exp.t =
	    traceSimplifyExp
	    (fn (e: Exp.t) => 
	     let val {decs, transfer} = Exp.dest e
	     in simplifyDecs (decs, fn () => simplifyTransfer transfer)
	     end) arg
	 and simplifyDecs (decs: Dec.t list, rest): Exp.t =
	    let
	       val rec loop =
		  fn [] => rest ()
		   | d :: ds => simplifyDec (d, fn () => loop ds)
	    in loop decs
	    end
	 and simplifyDec (dec: Dec.t, rest: unit -> Exp.t): Exp.t =
	    case dec of
	       Bind r => simplifyBind (r, rest)
	     | Fun r => simplifyFun (r, rest)
	     | HandlerPop => Exp.prefix (rest (), dec)
	     | HandlerPush h =>
		  let
		     val info as JumpInfo.T {name, ...} = jumpInfo h
		  in
		     Exp.prefix (rest (), HandlerPush name)
		  end
	 and simplifyBind arg: Exp.t =
	    traceSimplifyBind
	    (fn ({var, ty, exp}, rest: unit -> Exp.t) =>
	     let
		val VarInfo.T {numOccurrences, value, ...} = varInfo var
		fun finish (exp: PrimExp.t, decs: Exp.t): Exp.t =
		   Exp.prefix (decs, Bind {var = var, ty = ty, exp = exp})
		fun nonExpansive (p: PrimExp.t): Exp.t =
		   let
		      fun isUseless (): bool =
			 mayDelete andalso 0 = !numOccurrences
		      fun delete () = PrimExp.foreachVar (p, deleteVar)
		   in if isUseless ()
			 then (delete (); rest ())
		      else let val rest = rest ()
			   in if isUseless ()
				 then (delete (); rest)
			      else finish (p, rest)
			   end
		   end
		fun construct (v: Value.t) =
		   (value := SOME v; nonExpansive (Value.toPrimExp v))
		fun bindVar (x: VarInfo.t) =
		   (setReplacement (var, x)
		    ; deleteVarInfo x
		    ; rest ())
		fun primApp {prim, info, targs, args} =
		   let
		      val info =
			 let open PrimInfo
			 in case info of
			    None => None
			  | Overflow j => Overflow (JumpInfo.name (jumpInfo j))
			 end
		      val e =
			 PrimApp {prim = prim,
				  info = info,
				  targs = targs,
				  args = Vector.map (args, VarInfo.var)}
		   in if Prim.maySideEffect prim
			 then finish (e, rest ())
		      else nonExpansive e
		   end
	     in case exp of
		Const c => construct (Value.Const c)
	      | ConApp {con, args} =>
		   construct (Value.Con {con = con,
					 args = Vector.map (args, varInfo)})
	      | PrimApp {prim, info, targs, args} =>
		   let
		      fun deleteInfo () =
			 let
			    datatype z = datatype PrimInfo.t
			 in case info of
			    None => ()
			  | Overflow j => deleteJump j
			 end
		      val args = Vector.map (args, varInfo)
		      fun normal () =
			 primApp {prim = prim,
				  info = info,
				  targs = targs,
				  args = args}
		   in case Prim.name prim of
		      Prim.Name.FFI _ => normal ()
		    | _ =>
			 let
			    val args' =
			       Vector.map
			       (args, fn vi =>
				case vi of
				   VarInfo.T
				   {value = ref (SOME v), ...} =>
				      (case v of
					  Value.Con {con, args} =>
					     if Vector.isEmpty args
						then
						   Prim.ApplyArg.Con
						   {con = con,
						    hasArg =
						    not (Vector.isEmpty args)}
					     else Prim.ApplyArg.Var vi
					| Value.Const c =>
					     Prim.ApplyArg.Const (Const.node c)
					| _ => Prim.ApplyArg.Var vi)
				 | _ => Prim.ApplyArg.Var vi)
			    val res =
			       traceApply Prim.apply
			       (prim, Vector.toList args', VarInfo.equals)
			    datatype z = datatype Prim.ApplyResult.t
			 in case res of
			    Apply (p, args) =>
			       primApp {prim = p, info = info,
					targs = Vector.new0 (),
					args = Vector.fromList args}
			  | Bool b => (deleteInfo ()
				       ; construct (Value.fromBool b))
			  | Const c => (deleteInfo ()
					; construct (Value.Const c))
			  | Unknown => normal ()
			  | Var x => (deleteInfo ()
				      ; bindVar x)
			 end
		   end
	      | Select {tuple, offset} =>
		   let
		      val VarInfo.T {var = tuple, numOccurrences, value} =
			 varInfo tuple
		   in case !value of
		      NONE => nonExpansive (Select {tuple = tuple,
						    offset = offset})
		    | SOME (Value.Tuple vs) =>
			 (incNumOccurrences (numOccurrences, ~1)
			  ; setReplacement (var, Vector.sub (vs, offset))
			  ; rest ())
		    | _ => Error.bug "select of non-tuple"
		   end
	      | Tuple xs => construct (Value.Tuple (Vector.map (xs, varInfo)))
	      | Var x => bindVar (varInfo x)
	     end) arg
	 and simplifyFun ({name, args, body}, rest: unit -> Exp.t): Exp.t =
	    let
	       val JumpInfo.T {meaning, name = n, numOccurrences, ...} =
		  jumpInfo name
	       fun doit () =
		  let
		     fun isUseless () = 0 = !numOccurrences
		  in if isUseless ()
			then (deleteJumpMeaning (meaning, n); rest ())
		     else
			let val rest = rest ()
			in if isUseless ()
			      then rest
			   else
			      let
				 fun getVars (ps: Positions.t): VarInfo.t vector =
				    Vector.map
				    (ps, fn p =>
				     case p of
					Position.Formal i =>
					   let
					      val i =
						 varInfo
						 (#1 (Vector.sub (args, i)))
					      val _ = incVarInfo (i, 1)
					   in
					      i
					   end
				      | Position.Free x => varInfo x)
				 fun rr (f: Var.t vector -> Transfer.t, ps)
				    : Exp.t =
				    Exp.fromTransfer
				    (f (Vector.map (getVars ps, VarInfo.var)))
				 val body =
				    case meaning of
				       JumpInfo.Code _ =>
					  withinJump (name, fn () =>
						      simplifyExp body)
				     | JumpInfo.Jump {dst, args} =>
					  jump (dst, getVars args)
				     | JumpInfo.Raise ps => rr (Raise, ps)
				     | JumpInfo.Return ps => rr (Return, ps)
			      in Exp.prefix (rest,
					     Fun {name = name, args = args,
						  body = body})
			      end
			end
		  end
	    in case (Jump.equals (name, n), meaning) of
	       (true, JumpInfo.Code {isLiftableCase, ...}) =>
		  let val {decs, transfer} = Exp.dest body
		  in case (decs, !isLiftableCase) of
		     (_ :: _, SOME _) => 
			simplifyDecs
			(decs @ [Fun {name = name, args = args,
				      body = Exp.make {decs = [],
						       transfer = transfer}}],
			 rest)
		   | _ => doit ()
		  end
	     | (true, _) => doit ()
	     | _ => rest ()
	    end
	 and simplifyTransfer arg : Exp.t =
	    traceSimplifyTransfer
	    (fn (t: Transfer.t) =>
	     case t of
		Bug => Exp.bug
	      | Call {func, args, cont} =>
		   let
		      val cont =
			 case cont of
			    NONE => NONE
			  | SOME j =>
			       let val info = jumpInfo j
			       in if JumpInfo.isTail info
				     then NONE
				  else SOME (JumpInfo.name info)
			       end
		   in Exp.fromTransfer
		      (Call {func = func, args = simplifyVars args,
			     cont = cont})
		   end
	      | Case r => simplifyCase r
	      | Jump {dst, args} => jump (jumpInfo dst, varInfos args)
	      | Raise xs => Exp.fromTransfer (Raise (simplifyVars xs))
	      | Return xs => Exp.fromTransfer (Return (simplifyVars xs))
		   ) arg
	 and simplifyCase arg =
	    traceSimplifyCase
	    (fn (c as {cause, test, cases, default}) =>
	    let
	       val test = varInfo test
	       val cases = Cases.map (cases, jumpInfo)
	       val default = Option.map (default, jumpInfo)
	    in
	       if Cases.isEmpty cases
		  then (case default of
			   NONE => (deleteVarInfo test; Exp.bug)
			 | SOME j =>
			      (deleteVarInfo test; jump (j, Vector.new0 ())))
	       else
		  let
		     fun findCase (cases, is, args) =
			let
			   val _ = deleteVarInfo test
			   val n = Vector.length cases
			   val rec loop =
			      fn k =>
			      if k = n
				 then
				    (case default of
					NONE => Exp.bug
				      | SOME j => jump (j, Vector.new0 ()))
			      else
				 let
				    val (i, j) = Vector.sub (cases, k)
				 in
				    if is i
				       then (Int.for (k + 1, n, fn k =>
						      deleteJumpInfo
						      (#2 (Vector.sub
							   (cases, k))))
					     ; Option.app (default,
							   deleteJumpInfo)
					     ; Vector.foreach (args, addVarInfo)
					     ; jump (j, args))
				    else (deleteJumpInfo j; loop (k + 1))
				 end
			in loop 0
			end
		     fun normal () =
			Exp.fromTransfer
			(Case {cause = cause,
			       test = VarInfo.var test,
			       cases = Cases.map (cases, JumpInfo.name),
			       default = Option.map (default, JumpInfo.name)})
		  in case (VarInfo.value test, cases) of
		     (SOME (Value.Const c), _) =>
			let
			   fun doit (l, z) =
			      findCase (l, fn z' => z = z', Vector.new0 ())
			in case (cases, Const.node c) of
			   (Cases.Char l, Const.Node.Char c) => doit (l, c)
			 | (Cases.Int l, Const.Node.Int i) => doit (l, i)
			 | (Cases.Word l, Const.Node.Word w) => doit (l, w)
			 | (Cases.Word8 l, Const.Node.Word w) =>
			      doit (l, Word8.fromWord w)
			 | _ => Error.bug "strange constant for cases"
			end
		   | (SOME (Value.Con {con, args}), Cases.Con cases) =>
			findCase (cases, fn c => Con.equals (con, c), args)
		   | (SOME v, _) =>
			Error.bug (concat ["strange bind for case test: ",
					   Layout.toString (Value.layout v)])
		   | (NONE, _) =>
			(* If all cases are the same, eliminate the case. *)
			let
			   val info as JumpInfo.T {meaning, ...} = Cases.hd cases
			   fun isOk (i: JumpInfo.t): bool =
			      not (JumpInfo.usesFormal i)
			      andalso JumpInfo.equals (info, i)
			   val isOk =
			      Trace.trace ("isOk", JumpInfo.layout, Bool.layout)
			      isOk
			in if (not (JumpInfo.usesFormal info)
			       andalso Cases.forall (cases, isOk)
			       andalso Option.fold (default, true, isOk o #1))
			      then
				 (addJumpInfo info
				  ; Cases.foreach (cases, deleteJumpInfo)
				  ; Option.app (default, deleteJumpInfo)
				  ; jump (info, Vector.new0 ()))
			   else normal ()
			end
		  end
	    end) arg
	 val exp = simplifyExp exp
      in Exp.clear exp
	 ; exp
      end
   end

val shrinkExpNoDelete = fn e => shrinkExp (Vector.new0 ()) (e, false)

val traceShrinkExp =
   Trace.trace ("Cps.shrinkExp",
		Exp.layout,
		Exp.layout)

val shrinkExp = fn globals => let val shrinkExp = shrinkExp globals
			      in traceShrinkExp (fn e => shrinkExp (e, true))
			      end
			   
fun simplifyProgram simplifyExp
   (Program.T {datatypes, globals, functions, main}) =
   let
      val shrinkExp = shrinkExp globals
      val functions =
	 Vector.map
	 (functions, fn {name, args, body, returns} =>
	  {name = name, args = args,
	   body = shrinkExp (simplifyExp body),
	   returns = returns})
   in Program.T {datatypes = datatypes,
		 globals = globals,
		 functions = functions,
		 main = main}
   end

fun shrink p = simplifyProgram (fn x => x) p

end
