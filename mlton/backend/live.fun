(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *
 * This pass is based on the liveness algorithm described in section 4.13,
 * page 132, of Morgan's "Building and Optimizing Compiler".  BTW, the Dragon
 * book and Muchnick's book provided no help at all on speeding up liveness.
 * They suggest using bit-vectors, which is infeasible for MLton due to the
 * large size of and number of variables in CPS functions.
 *
 * Here is a description of the algorithm.
 *
 * Walk over the whole program and
 * 1. Build the predecessor graph of basic blocks.  Each basic block records the
 *    set of its predecessors and the set of variables live at the beginning of
 *    the block.
 * 2. For each variable record the block in which is defined and the list of
 *    blocks where it is used.
 *
 * Now, for each variable, propagate the liveness information backwards from uses
 * along basic blocks until the definition block is reached.
 *
 * That's it.  The reason why it's so fast is that it processes one variable at a
 * time, and hence the operation to determine if that variable is in the live
 * list for a particular block is constant time -- the variable is either at the
 * head of the list or it's not there.
 *)
functor Live (S: LIVE_STRUCTS): LIVE = 
struct

open S
open Dec PrimExp Transfer

structure Block =
   struct
      datatype t = T of {live: Var.t list ref,
			 liveHS: bool ref * bool ref,
			 preds: t list ref}

      fun new () = T {live = ref [],
		      liveHS = (ref false, ref false),
		      preds = ref []}

      fun live (T {live = r, ...}) = !r
      fun liveHS (T {liveHS = (c, l), ...}) = (!c, !l)

      fun equals (T {live = r, ...}, T {live = r', ...}) = r = r'

      fun addEdge (b, T {preds, ...}) =
	 if List.exists (!preds, fn b' => equals (b, b'))
	    then ()
	 else List.push (preds, b)
   end


fun live {exp, formals: (Var.t * Type.t) vector, jumpHandlers, shouldConsider} =
   let
      val {get = isCont: Jump.t -> bool,
	   set = setCont, destroy = destroyCont} =
	 Property.destGetSet (Jump.plist, Property.initConst false)
      val {get = jumpInfo: Jump.t -> {frameBlock: Block.t,
				      argBlock: Block.t,
				      bodyBlock: Block.t,
				      formals: (Var.t * Type.t) vector},
	   set = setJumpInfo,
	   destroy = destroyJumpInfo} =
	 Property.destGetSetOnce (Jump.plist,
				  Property.initRaise ("live info", Jump.layout))
      val {get = varInfo: Var.t -> {defined: Block.t,
				    used: Block.t list ref},
	   set = setVarInfo,
	   destroy = destroyVarInfo} =
	 Property.destGetSetOnce (Var.plist,
				  Property.initRaise ("live info", Var.layout))
      datatype u = Def of Block.t | Use of Block.t
      val handlerSlotInfo = ({defuse = ref [] : u list ref}, 
			     {defuse = ref [] : u list ref})
      val allPrims: (Var.t * Block.t) list ref = ref []
      val allJumps: Jump.t list ref = ref []
      val allVars: Var.t list ref = ref []
      fun newVarInfo (x: Var.t, {defined}) =
	 if shouldConsider x
	    then (List.push (allVars, x)
		  ; setVarInfo (x, {defined = defined,
				    used = ref []}))
	 else ()

      val _ =
	 Exp.foreach''
	 (exp,
	  {handleDec 
	   = fn Fun {name, args, ...}
	      => (fn () => let 
			     val _ = List.push (allJumps, name)
			     val (frameBlock, argBlock, bodyBlock) =
			       case (Vector.length args, isCont name)
				 of (0, false) => let val b = Block.new ()
						  in (b, b, b)
						  end
				  | (_, false) => let val b = Block.new ()
						      val b' = Block.new ()
						      val _ = Block.addEdge (b, b')
						  in (b, b, b')
						  end
				  | (0, true) => let val b = Block.new ()
						     val b' = Block.new ()
						     val _ = Block.addEdge (b, b')
						 in (b, b', b')
						 end
				  | _ => let val b = Block.new ()
					     val b' = Block.new ()
					     val b'' = Block.new ()
					     val _ = Block.addEdge (b, b')
					     val _ = Block.addEdge (b', b'')
					 in (b, b', b'')
					 end
			     val _ = Vector.foreach
			             (args, fn (x, _) =>
				      newVarInfo (x, {defined = argBlock}))
			   in
			     setJumpInfo (name, 
					  {frameBlock = frameBlock,
					   argBlock = argBlock,
					   bodyBlock = bodyBlock,
					   formals = args})
			   end)
	      | _ => fn () => (),
	   handleTransfer
	   = fn Call {cont = SOME j, ...} => setCont (j, true)
	      | _ => ()})

      val head = Block.new ()
      val _ = Vector.foreach (formals, fn (x, _) =>
			      newVarInfo (x, {defined = head}))
      fun use (b, x) =
	 if shouldConsider x
	    then
	       let val {used, ...} = varInfo x
	       in if (case !used of
			 [] => false
		       | b' :: _ => Block.equals (b, b'))
		     then ()
		  else List.push (used, b)
	       end
	 else ()
      fun uses (b: Block.t, xs: Var.t vector) =
	 Vector.foreach (xs, fn x => use (b, x))
      fun loopExp (e: Exp.t, b: Block.t, handlers: Jump.t list): unit =
	 let
	    val {decs, transfer} = Exp.dest e
	    val (b,handlers) =
	       List.fold
	       (decs, (b,handlers), fn (d, (b,handlers)) =>
		(case d of
		   Bind {var, exp, ...} =>
		      let
			 val b =
			    case exp of
			       ConApp {args, ...} => (uses (b, args); b)
			     | Const _ => b
			     | PrimApp {prim, info, args, ...} =>
				  (uses (b, args)
				   ; (PrimInfo.foreachJump
				      (info, fn j =>
				       Block.addEdge
				       (b, #argBlock (jumpInfo j))))
				   ; if Prim.entersRuntime prim
				        orelse
					Prim.mayOverflow prim
					orelse
					Prim.impCall prim
					then 
					   let val b' = Block.new ()
					   in Block.addEdge (b, b')
					      ; List.push (allPrims, (var, b'))
					      ; b'
					   end
				     else b)
			     | Select {tuple, ...} => (use (b, tuple); b)
			     | Tuple xs => (uses (b, xs); b)
			     | Var y => (use (b, y); b)
			 val _ = newVarInfo (var, {defined = b})
		      in b
		      end
		 | Fun {name, args, body} =>
		      let
			 val {frameBlock, argBlock, bodyBlock, ...} = jumpInfo name
			 val _ =
			    (* In case there is a raise to h. *)
			    if isCont name
			       then
				  case jumpHandlers name of
				     h :: _ =>
					Block.addEdge (frameBlock,
						       #argBlock (jumpInfo h))
				   | _ => ()
			    else ()
			 val _ = loopExp (body, bodyBlock, jumpHandlers name)
		      in b
		      end
		 | HandlerPush _ =>
		      let
			val ({defuse = code_defuse, ...},
			     {defuse = link_defuse, ...})
			  = handlerSlotInfo
			  
			val _ = List.push(code_defuse, Def b)
			val _ = case handlers
				  of [] => List.push(link_defuse, Def b)
				   | _ => ()
		      in
			b
		      end
		 | HandlerPop => 
		      let
			val ({defuse = code_defuse, ...},
			     {defuse = link_defuse, ...})
			  = handlerSlotInfo

			val _ = case handlers
				  of [] => Error.bug "pop of empty handler stack"
				   | _ :: [] => List.push(link_defuse, Use b)
				   | _ => List.push(code_defuse, Def b)
		      in
			b
		      end,
		 deltaHandlers (d, handlers)))
	    fun jump j = Block.addEdge (b, #argBlock (jumpInfo j))
	    fun call j = Block.addEdge (b, #frameBlock (jumpInfo j))
	    val _ =
	       case transfer of
		  Bug => ()
		| Call {args, cont, ...} =>
		     (uses (b, args)
		      ; case handlers
			  of [] => ()
			   | h::_ => let
				       val ({defuse = code_defuse, ...},
					    {defuse = link_defuse, ...})
					 = handlerSlotInfo
				     in
				       List.push(code_defuse, Use b);
				       List.push(link_defuse, Use b)
				     end
		      ; Option.app (cont, call))
		| Case {test, cases, default, ...} =>
		     (use (b, test)
		      ; Option.app (default, jump)
		      ; Cases.foreach (cases, jump))
		| Jump {dst, args, ...} =>
		     let val {formals, ...} = jumpInfo dst
		     in Vector.foreach2 (formals, args, fn ((f, _), a) =>
					if shouldConsider f
					   then use (b, a)
					else ())
			; jump dst
		     end
		| Return xs => uses (b, xs)
		| Raise xs =>
		     (uses (b, xs)
		      ; (case handlers of
			    h :: _ => jump h
			  | _ => ()))
	 in ()
	 end
      val b = Block.new ()
      val _ = Block.addEdge (head, b)
      val _ = loopExp (exp, b, [])
      val _ = destroyCont () (* must happen after loopExp *)
      val _ =
	 List.foreach
	 (!allVars, fn x =>
	  let
	     val {defined, used, ...} = varInfo x
	     val todo: Block.t list ref = ref []
	     fun consider (b as Block.T {live, ...}) =
		if Block.equals (b, defined)
		   orelse (case !live of
			      [] => false
			    | x' :: _ => Var.equals (x, x'))
		   then ()
		else (List.push (live, x)
		      ; List.push (todo, b))
	     val _ = List.foreach (!used, consider)
	     fun loop () =
		case !todo of
		   [] => ()
		 | Block.T {preds, ...} :: bs =>
		      (todo := bs
		       ; List.foreach (!preds, consider)
		       ; loop ())
	     val _ = loop ()
	  in ()
	  end)

      (* handler code and link slots are harder; in particular, they don't
       * satisfy the SSA invariant -- there are multiple definitions;
       * furthermore, a def and use in a block does not mean that the def 
       * occurs before the use.  But, a back propagated use will always
       * come after a def in the same block
       *)
      val _ =
	 List.foreach
	 ([(#1, #1), (#2, #2)], fn (sel, sel') =>
	  let
	    val {defuse,...} = sel handlerSlotInfo
	    val todo: Block.t list ref = ref []
	      
	    val defs
	      = List.foldr
	        (!defuse,
		 [],
		 fn (Def b, defs)
		  => b::defs
		  | (Use (b as Block.T {liveHS, ...}), defs)
		  => if List.exists(defs, fn b' => Block.equals(b, b'))
		       then defs
		       else (sel' liveHS := true
			     ; List.push(todo, b)
			     ; defs))

	    fun consider (b as Block.T {liveHS, ...})
	      = if List.exists(defs, fn b' => Block.equals(b, b'))
	           orelse !(sel' liveHS)
		 then ()
		 else (sel' liveHS := true
		       ; List.push (todo, b))

	    fun loop () =
	       case !todo of
		  [] => ()
		| Block.T {preds, ...} :: bs =>
		     (todo := bs
		      ; List.foreach (!preds, consider)
		      ; loop ())
	    val _ = loop ()
	 in
	   ()
	 end)

      val {get = getJump: Jump.t -> 
                          {liveBegin: Var.t list,
			   liveBeginNoFormals: Var.t list,
			   liveBeginFrame: Var.t list,
			   liveBeginHS: (bool * bool)},
	   set, destroy = destroyJump} =
	 Property.destGetSetOnce (Jump.plist,
				  Property.initRaise ("live", Jump.layout))
      val set =
	 Trace.trace2
	 ("setJumpLive",
	  Jump.layout,
	  fn {liveBegin, liveBeginNoFormals, liveBeginFrame, liveBeginHS, ...} =>
	  Layout.record
	  [("liveBegin", List.layout Var.layout liveBegin),
	   ("liveBeginNoFormals", List.layout Var.layout liveBeginNoFormals),
	   ("liveBeginFrame", List.layout Var.layout liveBeginFrame),
	   ("liveBeginHS", Layout.tuple2 (Bool.layout, Bool.layout) liveBeginHS)],
	  Unit.layout)
	 set
      val _ =
	 List.foreach (!allJumps, fn j =>
		       let val {frameBlock, argBlock, bodyBlock, ...} = jumpInfo j
		       in set (j, {liveBegin = Block.live bodyBlock,
				   liveBeginNoFormals = Block.live argBlock,
				   liveBeginFrame = Block.live frameBlock,
				   liveBeginHS = Block.liveHS frameBlock})
		       end)

      val _ = destroyJumpInfo ()

      val {get = getPrim: Var.t -> {livePrim: Var.t list,
				    livePrimHS: (bool * bool)},
	   set, destroy = destroyPrim} =
	 Property.destGetSetOnce (Var.plist,
				  Property.initRaise ("livePrim", Var.layout))
      val set =
	 Trace.trace2
	 ("setPrimLive",
	  Var.layout,
	  fn {livePrim, livePrimHS, ...} =>
	  Layout.record
	  [("livePrim", List.layout Var.layout livePrim),
	   ("livePrimHS", Layout.tuple2 (Bool.layout, Bool.layout) livePrimHS)],
	  Unit.layout)
	 set
      val _ =
	 List.foreach (!allPrims, fn (x, b) =>
		       set (x, {livePrim = Block.live b, 
				livePrimHS = Block.liveHS b}))
      val _ = destroyVarInfo ()
   in {
       liveBegin = #liveBegin o getJump,
       liveBeginNoFormals = #liveBeginNoFormals o getJump,
       liveBeginFrame = #liveBeginFrame o getJump,
       liveBeginHS = #liveBeginHS o getJump,
       livePrim = #livePrim o getPrim,
       livePrimHS = #livePrimHS o getPrim,
       destroy = fn () => (destroyJump ()
			   ; destroyPrim ())
       }
   end

val live =
   Trace.trace ("Live.live", fn {exp, ...} => Exp.layout exp, Layout.ignore)
   live
   
end
