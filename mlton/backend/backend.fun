(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Backend (S: BACKEND_STRUCTS): BACKEND = 
struct

open S

local open Cps
in
   structure Cases = Cases
   structure Con = Con
   structure Const = Const
   structure Cdec = Dec
   structure Cexp = Exp
   structure Func = Func
   structure Function = Function
   structure Cprogram = Program
   structure CPrimInfo = PrimInfo
   structure Ctype = Type
   structure Jump = Jump
   structure Prim = Prim
   structure PrimExp = PrimExp
   structure Tycon = Tycon
   structure Ctransfer = Transfer
   structure Var = Var
end 

local open Machine
in
   structure Mcases = Cases
   structure Chunk = Chunk
   structure GCInfo = GCInfo
   structure Kind = Block.Kind
   structure Label = Label
   structure MlimitCheck = LimitCheck
   structure Mtype = Type
   structure Mprogram = Program
   structure Operand = Operand
   structure MPrimInfo = PrimInfo
   structure Register = Register
   structure Statement = Statement
   structure Mtransfer = Transfer
   structure MOtransfer = MachineOutput.Transfer
end

nonfix ^
fun ^ r = valOf (!r)

fun id x = x
   
structure Chunkify = Chunkify (open Cps)

structure ParallelMove = ParallelMove ()

structure Representation = Representation (structure Cps = Cps
					   structure Mtype = Mtype)

structure AllocateRegisters = AllocateRegisters (structure Cps = Cps
						 structure Machine = Machine)
local open AllocateRegisters
in structure Info = Info
end

local open Representation
in structure TyconRep = TyconRep
   structure ConRep = ConRep
end

structure VarOperand =
   struct
      datatype t =
	 Allocate of {isUsed: bool ref,
		      operand: Operand.t option ref}
       | Const of Operand.t
       | Global of Operand.t
       | Void

      val operandOpt =
	 fn Allocate {operand, ...} => SOME (^operand)
	  | Const oper => SOME oper
	  | Global oper => SOME oper
	  | Void => NONE

      val operand = valOf o operandOpt

      fun layout i =
	 let open Layout
	 in case i of
	    Allocate {isUsed, operand, ...} =>
	       seq [str "Allocate ",
		    record [("isUsed", Bool.layout (!isUsed)),
			    ("operand", Option.layout Operand.layout (!operand))
			    ]]
	  | Const oper => seq [str "Const ", Operand.layout oper]
	  | Global oper => seq [str "Global ", Operand.layout oper]
	  | Void => str "Void "
	 end

      val use =
	 fn Allocate {isUsed, ...} => isUsed := true
	  | _ => ()
   end

fun generate (program as Cprogram.T {datatypes, globals, functions, main})
   : Mprogram.t =
   let
      val {tyconRep, conRep, toMtype} = Representation.compute program
      val _ =
	 Control.diagnostic
	 (fn display =>
	  (display (Layout.str "Representations:") ;
	   Vector.foreach
	   (datatypes, fn {tycon, cons} =>
	    let open Layout
	    in display (seq [Tycon.layout tycon,
			     str " ",
			     TyconRep.layout (tyconRep tycon)])
	       ; display (indent
			  (Vector.layout (fn {con, ...} =>
					  seq [Con.layout con,
					       str " ",
					       ConRep.layout (conRep con)])
			   cons,
			   2))
	    end)))
      fun toMtypes ts = Vector.map (ts, toMtype)
      val wordSize = 4
      val labelSize = Mtype.size Mtype.label
      val tagOffset = 0
      val tagType = Mtype.int
      val jumpHandlers = Cps.inferHandlers program
      val chunks = Chunkify.chunkify {program = program,
				      jumpHandlers = jumpHandlers}
      val _ = 
	 Control.diagnostic
	 (fn display =>
	  (display (Layout.str "Chunkification:");
	   List.foreach
	   (chunks, fn {funcs, jumps} =>
	    let open Layout
	    in display
	      (record ([("funcs", List.layout Func.layout funcs),
			("jumps", List.layout Jump.layout jumps)]))
	    end)))
      val {get = jumpInfo: Jump.t -> {args: (Var.t * Ctype.t) vector,
				      chunk: Chunk.t option ref,
				      cont: Label.t option ref,
				      handler: Label.t option ref},
	   set = setJumpInfo} =
	 Property.getSetOnce (Jump.plist,
			      Property.initRaise ("jump info", Jump.layout))
      val jumpArgs = #args o jumpInfo
      val jumpChunk = ^ o #chunk o jumpInfo
      val jumpCont = ^  o #cont o jumpInfo
      val jumpHandler = ^ o #handler o jumpInfo
      val jumpHandler =
	 Trace.trace ("jumpHandler", Jump.layout, Label.layout) jumpHandler
      val {get = funcInfo: Func.t -> {chunk: Chunk.t,
				      nearEntry: Label.t},
	   set = setFuncInfo} =
	 Property.getSetOnce (Func.plist,
			      Property.initRaise ("func info", Func.layout))
      val funcChunk = #chunk o funcInfo
      val funcNearEntry = #nearEntry o funcInfo
      val funcChunkLabel = Chunk.label o funcChunk
      val mprogram = Mprogram.new ()
      val raiseGlobal: Operand.t option ref = ref NONE
      fun raiseOperand (): Operand.t =
	 case !raiseGlobal of
	    NONE => Error.bug "raiseGlobal not defined"
	  | SOME z => z
      (* Create info for jumps used as conts. *)
      val _ =
	 let
	    fun new (j: Jump.t, sel) =
	       sel (jumpInfo j) := SOME (Label.new (jumpToLabel j))
	    fun loopExp (e: Cexp.t): unit =
	       let val {decs, transfer} = Cexp.dest e
	       in List.foreach (decs, loopDec)
		  ; (case transfer of
			Ctransfer.Call {cont, ...} =>
			   Option.app (cont, fn c => new (c, #cont))
		      | _ => ())
	       end
	    and loopDec (d: Cdec.t): unit =
	       case d of
		  Cdec.Fun {name, args, body} =>
		     (setJumpInfo (name, {args = args,
					  chunk = ref NONE,
					  cont = ref NONE,
					  handler = ref NONE})
		      ; loopExp body)
		| Cdec.HandlerPush h =>
		     let
			val _ =
			   case !raiseGlobal of
			      SOME _ => ()
			    | NONE =>
				 let
				    val a = jumpArgs h
				 in
				    if 1 = Vector.length a
				       then
					  let
					     val (_, t) = Vector.sub (a, 0)
					     val t = toMtype t
					     val oper =
						if Mtype.isPointer t
						   then
						      Mprogram.newGlobalPointerNonRoot
						      mprogram
						else
						   Mprogram.newGlobal (mprogram, t)
					  in raiseGlobal := SOME oper
					  end
				    else Error.bug "handler with <> 1 arg"
				 end
		     in new (h, #handler)
		     end
		| _ => ()
	    val _ = Vector.foreach (functions, 
				    fn Function.T {body, ...} => loopExp body)
	 in ()
	 end
      val machineChunks = ref []
      (* Create the mprogram chunks. *)
      val _ =
	 List.foreach
	 (chunks, fn {funcs, jumps} =>
	  let 
	     val conts =
		List.fold (jumps, [], fn (j, cs) =>
			   let
			      val {handler, cont, ...} = jumpInfo j
			      fun add (r, cs) =
				 case !r of
				    NONE => cs
				  | SOME l => l :: cs
			   in add (handler, add (cont, cs))
			   end)
	     val c =
		Mprogram.newChunk
		{program = mprogram,
		 entries = List.fold (funcs, conts, fn (f, ac) =>
				      funcToLabel f :: ac)}
	  in List.push (machineChunks, c)
	     ; List.foreach (funcs, fn f => setFuncInfo (f, {chunk = c,
							     nearEntry 
							     = Label.new 
							       (funcToLabel f)}))
	     ; List.foreach (jumps, fn j => #chunk (jumpInfo j) := SOME c)
	  end)
      (* primInfo is defined for primitives that enter the runtime system. *)
      val {get = varInfo: Var.t -> {operand: VarOperand.t,
				    primInfo: MPrimInfo.t ref,
				    ty: Ctype.t},
	   set = setVarInfo} =
	 Property.getSetOnce (Var.plist,
			      Property.initRaise ("Backend.info", Var.layout))
      val varInfo =
	 Trace.trace ("Backend.varInfo",
		      Var.layout,
		      fn {operand, primInfo, ...} =>
		      Layout.record [("operand", VarOperand.layout operand),
				     ("primInfo", MPrimInfo.layout (!primInfo))])
	 varInfo
      fun newVarInfo (x, oper, ty) =
	 setVarInfo (x, {operand = oper,
			 primInfo = ref MPrimInfo.none,
			 ty = ty})
      val varOperand = #operand o varInfo
      fun varOperands xs = List.map (xs, varOperand)
      val varOperandOpt = VarOperand.operandOpt o varOperand
      val vo = valOf o varOperandOpt
      fun sortTypes (initialOffset: int,
		     tys: Mtype.t vector): {size: int,
					    offsets: int vector,
					    numWordsNonPointers: int,
					    numPointers: int} =
	 let
	    val voids = ref []
	    val bytes = ref []
	    val doubleWords = ref []
	    val words = ref []
	    val pointers = ref []
	    val numPointers = ref 0
	    val _ = Vector.foreachi (tys, fn (i, t) =>
				     List.push
				     (if Mtype.isPointer t
					 then (Int.inc numPointers; pointers)
				      else (case Mtype.size t of
					       0 => voids
					     | 1 => bytes
					     | 4 => words
					     | 8 => doubleWords
					     | _ => Error.bug "strange size"),
					 (i, t)))
	    fun build (r, size, accum) =
	       List.fold (!r, accum, fn ((index, ty), (res, offset)) =>
			  ({index = index, offset = offset, ty = ty} :: res,
			   offset + size))
	    val (accum, offset) =
	       build
	       (voids, 0,
		build (bytes, 1,
		       build (words, 4,
			      build (doubleWords, 8, ([], initialOffset)))))
	    val offset = Mtype.align (Mtype.pointer, offset)
	    val numWordsNonPointers = (offset - initialOffset) div wordSize
	    val (components, size) = build (pointers, 4, (accum, offset))
	    val offsets =
	       Vector.tabulate
	       (Vector.length tys,
		fn i => #offset (List.lookup (components, fn {index, ...} =>
					      i = index)))
	 in {size = size,
	     offsets = offsets,
	     numWordsNonPointers = numWordsNonPointers,
	     numPointers = !numPointers}
	 end
      (* Compute layout for each con and associate it with the con. *)
      local
	 val {get, set} =
	    Property.getSetOnce (Con.plist,
				 Property.initRaise ("con info", Con.layout))
      in
	 val _ =
	    Vector.foreach
	    (datatypes, fn {cons, ...} =>
	     Vector.foreach (cons, fn {con, args} =>
			     let
				fun doit n =
				   let
				      val mtypes = toMtypes args
				      val info = sortTypes (n, mtypes)
				   in set (con, {info = info,
						 mtypes = mtypes})
				   end
			     in case conRep con of
				ConRep.Tuple => doit 0
			      | ConRep.TagTuple _ => doit 4
			      | _ => ()
			     end))
	 val conInfo = get
      end
      (* Compute layout for each tuple type. *)
      local
	 val {get} =
	    Property.get (Ctype.plist,
			  Property.initFun
			  (fn t => sortTypes (0, toMtypes (Ctype.detuple t))))
      in
	 val tupleInfo = get
	 fun tupleOffset (t: Ctype.t, n: int): int =
	    Vector.sub (#offsets (get t), n)
      end
      (* Set the varInfo for all variables.
       * Set the operand for all constants and globals.
       *)
      val _ =
	 let
	    (* genConstBind returns true iff var is set to a constant operand.
	     *)
	    fun genConstBind {var, ty, exp}: bool =
	       let
		  fun set' (oper: VarOperand.t): bool =
		     (newVarInfo (var, oper, ty)
		      ; true)
		  fun set oper = set' (VarOperand.Const oper)
		  fun global (new, s) = set (new (mprogram, s))
		  fun bogus () = set' VarOperand.Void
	       in case exp of
		  PrimExp.ConApp {con, args} =>
		     (case conRep con of
			 ConRep.Void => bogus ()
		       | ConRep.Int n => set (Operand.int n)
		       | ConRep.IntCast n => set (Operand.pointer n)
		       | _ => false)
		| PrimExp.Const c =>
		     let
			datatype z = datatype Const.Node.t
		     in case Const.node c of
			Char c => set (Operand.char c)
		      | Int n =>
			   (Assert.assert
			    ("genConstBind Const", fn () =>
			     Tycon.equals (Const.tycon c, Tycon.int))
			    ; set (Operand.int n))
		      | IntInf i =>
			   if Const.SmallIntInf.isSmall i
			      then
				 set (Operand.intInf
				      (Const.SmallIntInf.toWord i))
			   else global (Mprogram.newIntInf,
					IntInf.format (i, StringCvt.DEC))
		      | Real f => if !Control.Native.native
				     then global (Mprogram.newFloat, f)
				  else set (Operand.float f)
		      | String s => global (Mprogram.newString, s)
		      | Word w =>
			   set
			   (let val t = Const.tycon c
			    in if Tycon.equals (t, Tycon.word)
				  then Operand.uint w
			       else if Tycon.equals (t, Tycon.word8)
				       then (Operand.char
					     (Char.chr (Word.toInt w)))
				    else Error.bug "strange word"
			    end)
		     end
		| PrimExp.PrimApp {prim, ...} =>
		     (case Prim.name prim of
			 Prim.Name.MLton_bogus =>
			    set (case Mtype.dest (toMtype ty) of
				    Mtype.Char => Operand.char #"\000"
(*				  | Mtype.Double => Operand.float "0.0" *)
				  | Mtype.Int => Operand.int 0

				  | Mtype.Uint => Operand.uint 0w0
				  | Mtype.Pointer => Operand.pointer 1
				  | _ => Error.bug "bogus not implemented for type")
		       | _ => false)
		| PrimExp.Select _ =>
		     Mtype.isVoid (toMtype ty) andalso bogus ()
		| PrimExp.Tuple xs =>
		     if 0 = Vector.length xs
			then bogus ()
		     else false
		| PrimExp.Var x =>
		     (case #operand (varInfo x) of
			 VarOperand.Const oper => set oper
		       | _ => false)
	       end
	    val genConstBind =
	       Trace.trace ("genConstBind", Var.layout o #var, Bool.layout)
	       genConstBind
	    fun set (x, ty) =
	       newVarInfo
	       (x, if Mtype.isVoid (toMtype ty)
		      then VarOperand.Void
		   else VarOperand.Allocate {isUsed = ref false,
					     operand = ref NONE}, ty)
	    val use = VarOperand.use o #operand o varInfo
	    fun sets xts = Vector.foreach (xts, set)
	    val _ =
	       Vector.foreach
	       (globals, fn z as {var, ty, exp} =>
		(PrimExp.foreachVar (exp, use)
		 ; if genConstBind z
		      then ()
		   else
		      case exp of
			 PrimExp.Var x => setVarInfo (var, varInfo x)
		       | _ =>
			    newVarInfo
			    (var,
			     let val m = toMtype ty
			     in if Mtype.isVoid m
				   then VarOperand.Void
				else VarOperand.Global (Mprogram.newGlobal
							(mprogram, m))
			     end,
			     ty)))
	    fun loopExp e =
	       let
		  val {decs, transfer} = Cexp.dest e
	       in List.foreach (decs, loopDec)
		  ; Ctransfer.foreachVar (transfer, use)
	       end
	    and loopDec d =
	       case d of
		  Cdec.Bind (z as {var, ty, exp}) =>
		     (PrimExp.foreachVar (exp, use)
		      ; if genConstBind z
			   then ()
			else set (var, ty))
		| Cdec.Fun {args, body, ...} => (sets args; loopExp body)
		| _ => ()
	    val _ = Vector.foreach (functions, fn Function.T {args, body, ...} =>
				   (sets args; loopExp body))
	 in ()
	 end
      local
	 val varInfo =
	    fn x =>
	    let val {operand, primInfo, ty, ...} = varInfo x
	    in {operand = (case operand of
			      VarOperand.Allocate {isUsed, operand, ...} =>
				 if !isUsed
				    then SOME operand
				 else NONE
			    | _ => NONE),
		primInfo = primInfo,
		ty = toMtype ty}
	    end
      in
	 val {funcInfo = funcAllocateInfo,
	      jumpInfo = jumpAllocateInfo} =
	    Control.trace (Control.Pass, "allocate registers")
	    AllocateRegisters.allocate {funcChunk = funcChunk,
					jumpChunk = jumpChunk,
					jumpToLabel = jumpToLabel,
					jumpHandlers = jumpHandlers,
					program = program,
					varInfo = varInfo}
      end
   
      local
	 fun make sel (j: Jump.t) =
	    let val Info.T r = jumpAllocateInfo j
	    in sel r
	    end
      in
	 val jumpLive = make #live
	 val jumpLiveNoFormals = make #liveNoFormals
	 val jumpLiveFrame = make #liveFrame
      end

      fun parallelMove {srcs, dsts, chunk} =
	 let
	    val moves =
	       List.fold2 (srcs, dsts, [],
			   fn (src, dst, ac) => {src = src, dst = dst} :: ac)
	    fun temp r =
	       Operand.register (Chunk.tempRegister (chunk, Operand.ty r))
	 (* 	     val temp =
	  * 		Trace.trace ("temp", Operand.layout, Operand.layout) temp
	  *)
	 in
	    (* 	     Trace.trace
	     * 	     ("parallelMove",
	     * 	      fn {moves, ...} =>
	     * 	      List.layout (fn {src, dst} =>
	     * 			   Layout.tuple
	     * 			   [Operand.layout src, Operand.layout dst])
	     * 	      moves,
	     * 	      Layout.ignore)
	     *)
	    ParallelMove.move {
			       equals = Operand.equals,
			       move = Statement.move,
			       moves = moves,
			       interfere = Operand.interfere,
			       temp = temp
			       }
	 end
      fun tail' (to: Jump.t, srcs: 'a vector, srcOp: 'a -> Operand.t)
	 : Statement.t list * Mtransfer.t * bool =
	 let
	    val t = Mtransfer.nearJump {label = jumpToLabel to,
					return = NONE}
	 in
	    if Vector.isEmpty srcs
	       then ([], t, false)
	    else
	       let
		  val {args, chunk, ...} = jumpInfo to
		  val (srcs, dsts) =
		     Vector.fold2
		     (srcs, args, ([], []),
		      fn (src, (x, _), ac as (srcs, dsts)) =>
		      let
			 val {operand, ...} = varInfo x
		      in
			 case operand of
			    VarOperand.Allocate
			    {isUsed = ref true, operand, ...} =>
			       (srcOp src :: srcs, ^operand :: dsts)
			  | _ => ac
		      end)
	       in
		  (parallelMove {srcs = srcs,
				 dsts = dsts,
				 chunk = ^chunk},
		   t,
		   length srcs > 0)
	       end
	 end
      val tail =
	 Trace.trace ("tail",
		      Jump.layout o #1,
		      fn (s, t, _) =>
		      Layout.tuple [List.layout Statement.layout s,
				    Mtransfer.layout t])
	 tail'
      fun tail (to: Jump.t, srcs: 'a vector, srcOp: 'a -> Operand.t) =
	 let val (s, t, _) = tail' (to, srcs, srcOp)
	 in (s, t)
	 end
      fun conSelects (variant: Operand.t, con: Con.t): Operand.t vector =
	 let
	    val _ = Assert.assert ("conSelects", fn () =>
				   case conRep con of
				      ConRep.TagTuple _ => true
				    | ConRep.Tuple => true
				    | _ => false)
	    val {info = {offsets, ...}, mtypes} = conInfo con
	 in Vector.map2 (offsets, mtypes, fn (i, t) =>
			 Operand.offset {base = variant, offset = i, ty = t})
	 end
      val conSelects =
	 Trace.trace2 ("conSelects",
		       Operand.layout, Con.layout,
		       Vector.layout Operand.layout)
	 conSelects
      (* ------------------------------------------------- *)
      (*                      genCase                      *)
      (* ------------------------------------------------- *)
      fun genCase {chunk: Chunk.t,
		   profileName: string,
		   test: Var.t,
		   testRep: TyconRep.t,
		   cases: (Con.t * Jump.t) vector,
		   default: Jump.t option} =
	 let
	    fun addTest (os: Operand.t list): Operand.t list =
	       case varOperand test of
		  VarOperand.Allocate {operand, ...} => (^operand) :: os
		| _ => os
	    (* Creating this new block without limit checks is OK because all
	     * it does is a few moves and then a transfer.  I.E. it does no
	     * allocations and can not trigger a GC.
	     *)
	    fun newBlock (live, statements, transfer): Label.t =
	       let val l = Label.newNoname ()
	       in Chunk.newBlock (chunk,
				  {label = l,
				   kind = Kind.jump,
				   profileName = profileName,
				   live = live,
				   statements = statements,
				   transfer = transfer})
		  ; l
	       end
	    fun switch {test = test', cases, default, live, numLeft}
	       : {live: Operand.t list, transfer: Mtransfer.t} =
	       let
		  datatype z = None | One of Label.t | Many

		  val (live, default)
		    = if numLeft = 0
			then (live, NONE)
			else case default
			       of NONE => (live, NONE)
			        | SOME j
				=> (jumpLive j @ live, SOME (jumpToLabel j))
		    
		  val targets
		    = Mcases.fold
		      (cases,
		       case default
			 of SOME l => One l
			  | NONE => None,
		       fn (l, Many) => Many
		        | (l, One l') => if Label.equals(l, l')
					   then One l'
					   else Many
			| (l, None) => One l)

		  val (live, transfer)
		    = case targets
			of None 
			 => Error.bug "no targets"
			 | One l 
			 => (live,
			     Mtransfer.nearJump {label = l,
						 return = NONE})
			 | Many 
			 => (addTest live,
			     Mtransfer.switch {test = test',
					       cases = cases,
					       default = default})
	       in {live = live, 
		   transfer = transfer}
	       end
	    fun enum (test: Operand.t, numEnum: int) =
	       let
		  val (live, cases, numLeft) =
		     Vector.fold
		     (cases, ([], [], numEnum),
		      fn ((c, j), (os, cases, numLeft)) =>
		      let
			 fun keep n =
			    (jumpLiveNoFormals j @ os,
			     (n, jumpToLabel j) :: cases,
			     numLeft - 1)
		      in case conRep c of
			 ConRep.Int n => keep n
		       | ConRep.IntCast n => keep n
		       | _ => (os, cases, numLeft)
		      end)
	       in switch {test = test,
			  cases = Mcases.Int cases, default = default,
			  live = live, numLeft = numLeft}
	       end
	    fun transferToLabel {live, transfer}: Label.t =
	       case Mtransfer.toMOut transfer of
		  MOtransfer.NearJump {label, ...} => label
		| _ => newBlock (live, [], transfer)
	    fun switchIP (numEnum, pointer: Label.t): Mtransfer.t =
	       let
		  val test = vo test
		  val int =
		     transferToLabel (enum (Operand.castInt test, numEnum))
	       in Mtransfer.switchIP {test = test,
				      int = int,
				      pointer = pointer}
	       end
	    fun doTail (j: Jump.t, args: Operand.t vector)
	       : Operand.t list * Label.t =
	       let
		  val (s, t, testIsUsed) = tail' (j, args, fn a => a)
	       in
		  case (s, Mtransfer.toMOut t) of
		     ([], MOtransfer.NearJump {label, ...}) => (jumpLive j, label)
		   | _ => let
			     val live = jumpLiveNoFormals j
			     val live = if testIsUsed
					  then addTest live
					  else live
			  in (live, newBlock (live, s, t))
			  end
	       end
	    fun enumAndOne (numEnum: int): Mtransfer.t =
	       let
		  val test = vo test
		  val z =
		     Vector.loop
		     (cases,
		      fn (c, j) =>
		      case conRep c of
			 ConRep.Transparent _ => SOME (j, Vector.new1 test)
		       | ConRep.Tuple => SOME (j, conSelects (test, c))
		       | _ => NONE,
		      fn () =>
		      case default of
			 NONE => Error.bug "enumAndOne: no default"
		       | SOME j => (j, Vector.new0 ()))
	       in switchIP (numEnum, #2 (doTail z))
	       end
	    fun indirectTag (numTag: int) =
	       let
		  val test = vo test
		  val (live, cases, numLeft) =
		     Vector.fold
		     (cases, ([], [], numTag),
		      fn ((c, j), (live, cases, numLeft)) =>
		      case conRep c of
			 ConRep.TagTuple n =>
			    let
			       val (live', l) =
				  doTail (j, conSelects (test, c))
			    in (live' @ live, (n, l) :: cases, numLeft - 1)
			    end
		       | _ => (live, cases, numLeft))
	       in switch {test = Operand.offset {base = test,
						 offset = tagOffset,
						 ty = tagType},
			  cases = Mcases.Int cases, default = default,
			  live = live, numLeft = numLeft}
	       end
	 in case testRep of
	    TyconRep.Prim mtype =>
	       (case (Vector.length cases, default) of
		   (1, _) =>
		      (* We use _ instead of NONE for the default becuase
		       * there may be an unreachable default case.
		       *)
		      let
			 val (c, l) = Vector.sub (cases, 0)
		      in case conRep c of
			 ConRep.Void => tail (l, Vector.new0 (), id)
		       | ConRep.Transparent _ => tail (l, Vector.new1 test, vo)
		       | ConRep.Tuple => tail (l, conSelects (vo test, c), id)
		       | _ => Error.bug "strange conRep for Prim"
		      end
		 | (0, SOME j) => tail (j, Vector.new0 (), id)
		 | _ => Error.bug "prim datatype with more than one case")
	  | TyconRep.Enum {numEnum} =>
	       ([], #transfer (enum (vo test, numEnum)))
	  | TyconRep.EnumDirect {numEnum} => ([], enumAndOne numEnum)
	  | TyconRep.EnumIndirect {numEnum} => ([], enumAndOne numEnum)
	  | TyconRep.EnumIndirectTag {numEnum, numTag} =>
	       ([], switchIP (numEnum, transferToLabel (indirectTag numTag)))
	  | TyconRep.IndirectTag {numTag} =>
	       ([], #transfer (indirectTag numTag))
	 end
      
      (* ------------------------------------------------- *)
      (*                    genPrimExp                     *)
      (* ------------------------------------------------- *)

      fun genPrimExp (x: Var.t, ty: Ctype.t, e: PrimExp.t,
		      chunk: Chunk.t): Statement.t list =
	 let
	    val {operand, primInfo, ty, ...} = varInfo x
	    fun sideEffectFree () = not (PrimExp.maySideEffect e)
	 in if (case operand of
		   VarOperand.Allocate {isUsed, ...} =>
		      not (!isUsed) andalso sideEffectFree ()
		 | VarOperand.Const _ => true
		 | VarOperand.Global _ => false
		 | VarOperand.Void => sideEffectFree ())
	       then []
	    else
	       let
		  fun xop () =
		     case operand of
			VarOperand.Allocate {operand, ...} => ^operand
		      | VarOperand.Global z => z
		      | _ => Error.bug "xop"
		  fun move src = [Statement.move {dst = xop (), src = src}]
		  fun makeStores (ys: Var.t vector, offsets) =
		     Vector.fold2 (ys, offsets, [], fn (y, offset, stores) =>
				   case varOperandOpt y of
				      NONE => stores
				    | SOME value => 
					 {offset = offset, value = value}
					 :: stores)
		  fun allocate (ys: Var.t vector,
				{size, offsets,
				 numPointers, numWordsNonPointers}) =
		     [Statement.allocate
		      {dst = xop (),
		       size = size,
		       numPointers = numPointers,
		       numWordsNonPointers = numWordsNonPointers,
		       stores = makeStores (ys, offsets)}]
		  fun allocateTagged (n: int,
				      ys: Var.t vector,
				      {size, offsets,
				       numPointers, numWordsNonPointers}) =
		     [Statement.allocate
		      {dst = xop (),
		       size = size,
		       numPointers = numPointers,
		       numWordsNonPointers =
		       (* for the tag *) 1 + numWordsNonPointers,
		       stores = ({offset = tagOffset, value = Operand.int n}
				 :: makeStores (ys, offsets))}]

	       in case e of
		  PrimExp.ConApp {con, args} =>
		     let 
			fun tuple () = allocate (args, #info (conInfo con))
		     in case conRep con of
			ConRep.Transparent _ =>
			   move (vo (Vector.sub (args, 0)))
		      | ConRep.Tuple => tuple ()
		      | ConRep.TagTuple n =>
			   allocateTagged (n, args, #info (conInfo con))
		      | _ => Error.bug "strange ConApp"
		     end
		| PrimExp.PrimApp {prim, targs, args, ...} =>
		     let
			fun a i = Vector.sub (args, i)
			fun offset (a, i, ty) =
			   Operand.arrayOffset {base = a,
						offset = i,
						ty = ty}
			fun unsafeSub (ty: Mtype.t) =
			   move (offset (vo (a 0), vo (a 1), ty))
			fun array (n: Operand.t, t: Mtype.t): Statement.t list =
			   let
			      val (nbnp, np) =
				 if Mtype.isPointer t
				    then (0, 1)
				 else (Mtype.size t, 0)
			      val gcInfo = MPrimInfo.deRuntime (!primInfo)
			   in [Statement.allocateArray
			       {dst = xop (),
				numElts = n,
				numBytesNonPointers = nbnp,
				numPointers = np,
				gcInfo = gcInfo}]
			   end
			fun normal () =
			   let
			      val pinfo = !primInfo
			      val dst =
				 let datatype z = datatype VarOperand.t
				 in case operand of
				    Allocate {isUsed, operand, ...} =>
				       if !isUsed
					  then SOME (^operand)
				       else NONE
				  | Const oper => SOME oper
				  | Global oper => SOME oper
				  | Void => NONE
				 end
			   in
			      [Statement.assign
			       {dst = dst,
				oper = prim,
				args = Vector.map (args, vo),
				pinfo = pinfo}]
			   end
			fun targ () = toMtype (Vector.sub (targs, 0))
			datatype z = datatype Prim.Name.t
		     in case Prim.name prim of
			Array_array => array (vo (a 0), targ ())
		      | Array_sub => unsafeSub (targ ())
		      | Array_update =>
			   let
			      val t = targ ()
			   in case Mtype.dest t of
			      Mtype.Void => []
			    | _ => [Statement.move
				    {dst = offset (vo (a 0), vo (a 1), t),
				     src = vo (a 2)}]
			   end
		      | MLton_eq =>
			   if Mtype.isVoid (targ ())
			      then [Statement.move {dst = xop (),
						    src = Operand.int 1}]
			   else normal ()
		      | Ref_assign =>
			   let
			      val t = targ ()
			   in case Mtype.dest t of
			      Mtype.Void => []
			    | _ => [Statement.move
				    {dst = Operand.contents (vo (a 0), t),
				     src = vo (a 1)}]
			   end
		      | Ref_deref =>
			   let
			      val t = targ ()
			   in case Mtype.dest t of
			      Mtype.Void => []
			    | _ => move (Operand.contents (vo (a 0), t))
			   end
		      | Ref_ref =>
			   let
			      val t = targ ()
			      val (ys, ts) = if Mtype.isVoid t
						then (Vector.new0 (),
						      Vector.new0 ())
					     else (Vector.new1 (a 0),
						   Vector.new1 t)
			   in allocate (ys, sortTypes (0, ts))
			   end
		      | String_sub => unsafeSub Mtype.char
		      | Vector_fromArray => move (vo (a 0))
		      | Vector_sub => unsafeSub (targ ())
		      | _ => normal ()
		     end
		| PrimExp.Select {tuple, offset} =>
		     let val {operand, ty = ty', ...} = varInfo tuple
		     in move (Operand.offset
			      {base = VarOperand.operand operand,
			       offset = tupleOffset (ty', offset),
			       ty = toMtype ty})
		     end
		| PrimExp.Tuple ys => allocate (ys, tupleInfo ty)
		| PrimExp.Var y => move (vo y)
		| _ => Error.bug "genPrimExp saw strange primExp"
	       end
	 end

      val genPrimExp =
	 Trace.trace ("genPrimExp",
		      fn (x, t, e, _) => Layout.tuple [Var.layout x,
						       Ctype.layout t,
						       PrimExp.layout e],
		      List.layout Statement.layout)
	 genPrimExp

      fun varsRegs (xs: Var.t list): Register.t list =
	 List.fold (xs, [], fn (x, rs) =>
		    case varOperandOpt x of
		       NONE => rs
		     | SOME oper => 
			  case Operand.deRegister oper of
			     NONE => rs
			   | SOME r => r :: rs)
      (* ------------------------------------------------- *)
      (*                      genCont                      *)
      (* ------------------------------------------------- *)
      fun genCont (c: Chunk.t,
		   l: Label.t,
		   j: Jump.t,
		   args: (Var.t * Ctype.t) vector,
		   profileName: string): unit =
	 let
	    val Info.T {liveFrame, liveNoFormals, cont, ...} = jumpAllocateInfo j
	    val size
	      = case cont
		  of SOME {size, ...} => size
		   | NONE => Error.bug "no cont"

	    (* Need liveFrame
	     * because some vars might be live down a handler
	     * that handles raises from the function returning here.
	     *)
	    val _ = Mprogram.newFrame (mprogram,
				       {return = l,
					chunkLabel = Chunk.label c,
					size = size,
					live = liveFrame})

	    val (args, (argsl, offset)) =
	       Vector.mapAndFold
	       (args, ([], 4),
		fn ((var, ty), (argsl, offset)) =>
		let
		   val ty = toMtype ty
		   val offset = Mtype.align (ty, offset)
		   val calleeOffset = offset + size
		   val arg = Operand.stackOffset {offset = calleeOffset,
						  ty = ty}
		   val isUsed
		     = case varInfo var
			 of {operand = VarOperand.Allocate {isUsed, ...}, ...} 
			  => !isUsed
			  | _ => false
		in (arg,
		    (if isUsed
		       then arg::argsl
		       else argsl,
		     offset + Mtype.size ty))
		end)
	    val (statements, transfer) = tail (j, args, id)

	    val limitCheck =
	       MlimitCheck.Maybe (GCInfo.make 
				  {frameSize = size + offset,
				   live = argsl @ liveNoFormals})
	    val statements =
	          Statement.limitCheck limitCheck
	       :: statements
	    val chunk = jumpChunk j
	    val _ =
	       Chunk.newBlock
	       (chunk, {label = l,
			kind = Kind.cont {args = argsl,
					  size = size},
			live = liveNoFormals,
			profileName = profileName,
			statements = statements,
			transfer = transfer})
	 in ()
	 end
      (* ------------------------------------------------- *)
      (*                    genHandler                     *)
      (* ------------------------------------------------- *)
      fun genHandler (c: Chunk.t,
		      l: Label.t,
		      j: Jump.t,
		      profileName: string): unit =
	 let
	    val _ = Mprogram.newHandler (mprogram, 
					 {chunkLabel = Chunk.label c,
					  label = l})
	    val Info.T {liveNoFormals, handler, ...} = jumpAllocateInfo j
	    val size
	      = case handler
		  of SOME {size, ...} => size
		   | NONE => Error.bug "no handler"
	    val args = Vector.new1 (raiseOperand ())
	    val (statements, transfer) = tail (j, args, id)
	 in Chunk.newBlock (jumpChunk j,
			    {label = l,
			     kind = Kind.handler {size = size},
			     live = liveNoFormals,
			     profileName = profileName,
			     statements = statements,
			     transfer = transfer})
	 end
      (* ------------------------------------------------- *)
      (*                    genTransfer                    *)
      (* ------------------------------------------------- *)
      fun genTransfer (t: Ctransfer.t,
		       chunk: Chunk.t,
		       profileName: string,
		       handlerOffset: int option,
		       handlers: Jump.t list): Statement.t list * Mtransfer.t =
	 case t of
	    Ctransfer.Bug => ([], Mtransfer.bug)
	  | Ctransfer.Call {func, args, cont} =>
	       let
		  val args = Vector.toList args
		  val offsets =
		     rev (#2 (List.fold
			      (args, (4, []), (* 4 is for return address *)
			       fn (arg, (offset, offsets)) =>
			       case varOperandOpt arg of
				  NONE => (offset, offset :: offsets)
				| SOME oper =>
				     let val ty = Operand.ty oper
					val offset = Mtype.align (ty, offset)
				     in (offset + Mtype.size ty,
					 offset :: offsets)
				     end)))

		  val (frameSize, return, handlerLive)
		    = case cont
			of NONE => (0, NONE, [])
			 | SOME c 
			 => let
			      val Info.T {cont, ...} = jumpAllocateInfo c
			      val size
				= case cont
				    of SOME {size, ...} => size
				     | NONE => Error.bug "no cont"

			      val return = jumpCont c

			      val (handler, handlerLive)
				= case jumpHandlers c
				    of h::_ 
				     => let
					  val handlerOffset 
					    = valOf handlerOffset
					in
					  (SOME (jumpHandler h),
					   (Operand.stackOffset 
					    {offset = handlerOffset,
					     ty = Mtype.uint})::
					   (Operand.stackOffset 
					    {offset = handlerOffset + 
					              labelSize,
					     ty = Mtype.uint})::
					   nil)
					end
				     | _ => (NONE, [])
			    in
			      (size, 
			       SOME {return = return,
				     handler = handler,
				     size = size},
			       handlerLive)
			    end

		  val (live, setupArgs) =
		     let
			val (live,moves) =
			   List.fold2
			   (args, offsets, (handlerLive,[]), 
			    fn (arg, offset, (live, ac)) =>
			    case varOperandOpt arg of
			       NONE => (live, ac)
			     | SOME oper =>
				  let
				    val so = Operand.stackOffset
				             {offset = frameSize + offset,
					      ty = Operand.ty oper}
				  in
				    (so::live,
				     {src = oper,
				      dst = so}::ac)
				  end)
			fun temp r =
			   Operand.register
			   (Chunk.tempRegister (chunk, Operand.ty r))
		     in
		       (live,
(* 			 Trace.trace
 * 			 ("parallelMove",
 * 			  fn {moves, ...} =>
 * 			  List.layout (fn {src, dst} =>
 * 				       Layout.tuple
 * 				       [Operand.layout src, Operand.layout dst])
 * 			  moves,
 * 			  fn ss => (List.foreach (ss, fn s =>
 * 						 (Statement.output (s, print)
 * 						  ; print "\n"))
 * 				    ; Layout.empty))
 *)
			ParallelMove.move {equals = Operand.equals,
					   move = Statement.move,
					   moves = moves,
					   interfere = Operand.interfere,
					   temp = temp})
		     end

		  val chunk' = funcChunk func
		  val transfer
		    = if !Control.Native.native 
			then if (* all non-tail calls *)
			        (isSome return)
			        orelse
				(* all tail calls to other cps functions *)
				not (Chunk.equals(chunk, chunk'))
			       then Mtransfer.farJump
				    {chunkLabel = Chunk.label chunk',
				     label = funcToLabel func,
				     live = live,
				     return = return}
			       else Mtransfer.nearJump
				    {label = funcNearEntry func,
				     return = NONE}
			else if Chunk.equals (chunk, chunk')
			       then Mtransfer.nearJump 
				    {label = funcNearEntry func,
				     return = return}
			       else Mtransfer.farJump
			            {chunkLabel = Chunk.label chunk',
				     label = funcToLabel func,
				     live = live,
				     return = return}
	       in (setupArgs, transfer)
	       end
	  | Ctransfer.Case {test, cases, default, ...} =>
	       let
		  fun id x = x
		  fun doit (l, f, branch) =
		     ([],
		      Mtransfer.switch
		      {test = vo test,
		       cases = f (Vector.toListMap
				  (l, fn (i, j) => (branch i, jumpToLabel j))),
		       default = Option.map (default, jumpToLabel)})
	       in case cases of
		  Cases.Char l => doit (l, Mcases.Char, id)
		| Cases.Int l => doit (l, Mcases.Int, id)
		| Cases.Word l => doit (l, Mcases.Word, id)
		| Cases.Word8 l => doit (l, Mcases.Char, Word8.toChar)
		| Cases.Con cases =>
		     (case (Vector.length cases, default) of
			 (0, NONE) => ([], Mtransfer.bug)
		       | _ => 
			    let
			       val (tycon, tys) =
				  Ctype.tyconArgs (#ty (varInfo test))
			    in
			       if Vector.isEmpty tys
				  then genCase {cases = cases,
						chunk = chunk,
						default = default,
						profileName = profileName,
						test = test,
						testRep = tyconRep tycon}
			       else Error.bug "strange type in case"
			    end)
	       end
	  | Ctransfer.Jump {dst, args} => tail (dst, args, vo)
	  | Ctransfer.Raise xs =>
	       (case handlers of
		   [] => (Statement.moves {dsts = [raiseOperand ()],
					   srcs = Vector.toListMap (xs, vo)},
			  Mtransfer.raisee)
		 | h :: _ => tail (h, xs, vo))
	  | Ctransfer.Return xs =>
	       let
		  val xs = Vector.toList xs
		  val rets = varOperands xs
		  val (_, live, moves) =
		     List.fold
		     (xs, (4, [], []), fn (x, (offset, live, moves)) =>
		      case varOperandOpt x of
			 NONE => (offset, live, moves)
		       | SOME x =>
			    let 
			       val ty = Operand.ty x
			       val offset = Mtype.align (ty, offset)
			       val so = Operand.stackOffset {offset = offset, 
							     ty = ty}
			    in 
			       (offset + Mtype.size ty,
				so::live,
				{src = x,
				 dst = so}
				:: moves)
			    end)
		  fun temp r =
		     Operand.register
		     (Chunk.tempRegister (chunk, Operand.ty r))
	       in
		  (ParallelMove.move {equals = Operand.equals,
				      move = Statement.move,
				      moves = moves,
				      interfere = Operand.interfere,
				      temp = temp},
		   Mtransfer.return {live = live})
	       end
      val genTransfer =
	 Trace.trace ("genTransfer",
		      Ctransfer.layout o #1,
		      Layout.tuple2 (List.layout Statement.layout,
				     Mtransfer.layout))
	 genTransfer
      (*------------------------------------*)
      (*               genExp               *)
      (*------------------------------------*)
      fun genExp {exp = e: Cexp.t,
		  profileName: string,
		  label: Label.t,
		  kind: Kind.t,
		  chunk: Chunk.t,
		  info = Info.T {limitCheck, live, ...},
		  handlerOffset: int option,
		  handlers: Jump.t list}: unit =
	 let
	    val {decs, transfer} = Cexp.dest e
	    val (decs, handlers) =
	       genDecs (decs, chunk, profileName, handlerOffset, handlers)
	    val (preTransfer, transfer) =
	       genTransfer (transfer, chunk, profileName, handlerOffset, handlers)
	    val statements =
	       Statement.limitCheck limitCheck :: (decs @ preTransfer)
	 in
	    Chunk.newBlock
	    (chunk, {label = label,
		     kind = kind,
		     live = live,
		     profileName = profileName,
		     statements = statements,
		     transfer = transfer})
	 end

      and genDecs (ds: Cdec.t list,
		   chunk: Chunk.t,
		   profileName: string,
		   handlerOffset,
		   handlers: Jump.t list): Statement.t list * Jump.t list =
	 let
	    val (statements, handlers) =
	       List.fold
	       (ds, ([], handlers), fn (d , (statements, handlers)) =>
		(case d of
		    Cdec.Bind {var, ty, exp} =>
		       genPrimExp (var, ty, exp, chunk) :: statements
		  | Cdec.Fun {name, args, body} =>
		       let
			  val chunk' = chunk
			  val {chunk, cont, handler, ...} = jumpInfo name
			  val chunk = ^chunk
			  val _ =
			     case !cont of
				NONE => ()
			      | SOME l =>
				   genCont (chunk, l, name, args, profileName)
			  val _ =
			     case !handler of
				NONE => ()
			      | SOME l =>
				   genHandler (chunk, l, name, profileName)
			  val _ =
			     genExp {exp = body,
				     profileName = profileName,
				     label = jumpToLabel name,
				     kind = if Chunk.equals(chunk, chunk')
					      then Kind.jump
					      else Kind.func {args = jumpLive name},
				     chunk = chunk,
				     info = jumpAllocateInfo name,
				     handlerOffset = handlerOffset,
				     handlers = jumpHandlers name}
		       in statements
		       end
		  | Cdec.HandlerPush h =>
		       let
			  val offset = valOf handlerOffset
			  val statements =
			     [Statement.move
			      {dst = Operand.stackOffset {offset = offset,
							  ty = Mtype.label},
			       src = Operand.label (jumpHandler h)}]
			     :: statements
		       in case handlers of
			  [] => ([Statement.saveExnStack {offset = offset}]
				 :: statements)
			| _ => statements
		       end
		  | Cdec.HandlerPop =>
		       let val offset = valOf handlerOffset
		       in case handlers of
			  [] => Error.bug "pop of empty handler stack"
			| _ :: handlers => 
			     (case handlers of
				 [] =>
				    [Statement.restoreExnStack {offset = offset}]
			       | h :: _ =>
				    [Statement.move
				     {dst =
				      Operand.stackOffset {offset = offset,
							   ty = Mtype.label},
				      src = Operand.label (jumpHandler h)}])
				 :: statements
		       end,
		    Cps.deltaHandlers (d, handlers)))
	 in (List.fold (statements, [], op @), handlers)
	 end
      (* Build the initGlobals chunk. *)
      val initGlobals = Label.newString "initGlobals"
      val initGlobalsNear = Label.new initGlobals
      val chunk = Mprogram.newChunk {program = mprogram,
				     entries = [initGlobals]}
      val initGlobalsStatements =
	 Statement.limitCheck
	 (MlimitCheck.Maybe
	  (GCInfo.make {live = [],
			frameSize = Mtype.size Mtype.label}))
	 ::
	 List.fold
	 (Vector.fold (globals, [], fn ({var, ty, exp}, statements) =>
		      (genPrimExp (var, ty, exp, chunk) :: statements)),
	  [], op @)
      val _ =
	 Mprogram.setMain (mprogram, {chunkLabel = Chunk.label chunk, 
				      label = initGlobals})
      val _ =
	 Chunk.newBlock
	 (chunk, {label = initGlobals,
		  kind = Kind.func {args = []},
		  live = [],
		  profileName = "initGlobals",
		  statements = initGlobalsStatements,
		  transfer = Mtransfer.farJump {chunkLabel = funcChunkLabel main,
					        label = funcToLabel main,
						live = [],
						return = NONE}})
      val _ =
	 Control.trace (Control.Pass, "generate")
	 Vector.foreach
	 (functions, fn Function.T {name, body, ...} =>
	  let val {info as Info.T {live, ...}, 
		   handlerOffset, ...} = funcAllocateInfo name
	  in Chunk.newBlock
	     (funcChunk name, 
	      {label = funcToLabel name,
	       kind = Kind.func {args = live},
	       live = live,
	       profileName = Func.toString name,
	       statements = [],
	       transfer = Mtransfer.nearJump {label = funcNearEntry name,
					      return = NONE}});
	     genExp {exp = body,
		     profileName = Func.toString name,
		     label = funcNearEntry name,
		     kind = Kind.jump,
		     chunk = funcChunk name,
		     info = info,
		     handlerOffset = handlerOffset,
		     handlers = []}
	  end)
      (* The Mprogram.clear is necessary because Funcs and Jumps are turned into
       * Labels in the resulting mprogram, and properties have been attached to
       * them by the backend.
       *)
      val _ = Mprogram.clear mprogram
   in
      mprogram
   end

end
