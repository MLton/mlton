(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *
 * This pass is based on the liveness algorithm described in section 4.13,
 * page 132, of Morgan's "Building and Optimizing Compiler".  BTW, the Dragon
 * book and Muchnick's book provided no help at all on speeding up liveness.
 * They suggest using bit-vectors, which is infeasible for MLton due to the
 * large size of and number of variables in SSA functions.
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
datatype z = datatype Exp.t
datatype z = datatype Transfer.t

structure LiveInfo =
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

fun live (function, {isCont: Label.t -> bool,
		     shouldConsider: Var.t -> bool}) =
   let
      val _ =
	 Control.diagnostic
	 (fn () =>
	  let open Layout
	  in seq [str "Live info for ",
		  Func.layout (Function.name function)]
	  end)
      val {args, blocks, start, ...} = Function.dest function
      val {get = labelInfo: Label.t -> {
					argInfo: LiveInfo.t,
					block: Block.t,
					bodyInfo: LiveInfo.t,
					frameInfo: LiveInfo.t,
					isContSet: bool ref
					},
	   set = setLabelInfo, ...} =
	 Property.getSetOnce (Label.plist,
			      Property.initRaise ("live info", Label.layout))
      val {get = varInfo: Var.t -> {defined: LiveInfo.t,
				    used: LiveInfo.t list ref},
	   set = setVarInfo,
	   destroy = destroyVarInfo} =
	 Property.destGetSetOnce (Var.plist,
				  Property.initRaise ("live info", Var.layout))
      datatype u = Def of LiveInfo.t | Use of LiveInfo.t
      val handlerSlotInfo = ({defuse = ref [] : u list ref}, 
			     {defuse = ref [] : u list ref})
      val allPrims: (Var.t * LiveInfo.t) list ref = ref []
      val allVars: Var.t list ref = ref []
      fun newVarInfo (x: Var.t, {defined}): unit =
	 if shouldConsider x
	    then (List.push (allVars, x)
		  ; setVarInfo (x, {defined = defined,
				    used = ref []}))
	 else ()
      val _ =
	 Trace.trace2 ("Live.newVarInfo", Var.layout, Layout.ignore, Unit.layout)
	 newVarInfo
      val _ =
	 Vector.foreach
	 (blocks, fn block as Block.T {label, args, ...} =>
	  let 
	     val (frameInfo, argInfo, bodyInfo) =
		case (Vector.length args, isCont label) of
		   (0, false) => let val b = LiveInfo.new ()
				 in (b, b, b)
				 end
		 | (_, false) => let val b = LiveInfo.new ()
				     val b' = LiveInfo.new ()
				     val _ = LiveInfo.addEdge (b, b')
				 in (b, b, b')
				 end
		 | (0, true) => let val b = LiveInfo.new ()
				    val b' = LiveInfo.new ()
				    val _ = LiveInfo.addEdge (b, b')
				in (b, b', b')
				end
		 | _ => let val b = LiveInfo.new ()
			    val b' = LiveInfo.new ()
			    val b'' = LiveInfo.new ()
			    val _ = LiveInfo.addEdge (b, b')
			    val _ = LiveInfo.addEdge (b', b'')
			in (b, b', b'')
			end
	     val _ =
		Vector.foreach (args, fn (x, _) =>
				newVarInfo (x, {defined = argInfo}))
	  in
	     setLabelInfo (label, {argInfo = argInfo,
				   block = block,
				   bodyInfo = bodyInfo,
				   frameInfo = frameInfo,
				   isContSet = ref false
				   })
	  end)
      val _ = 
	 Vector.foreach
	 (blocks, fn Block.T {label, transfer, ...} =>
	  case transfer of
	     Call {return = SOME {cont, handler}, ...} =>
		let
		   val {frameInfo, isContSet, ...} = labelInfo cont
		in
		   if !isContSet
		      then ()
		   else
		      let
			 val _ = isContSet := true
		      in
			 Option.app
			 (handler, fn h =>
			  (* In case there is a raise to h. *)
			  LiveInfo.addEdge (frameInfo, #argInfo (labelInfo h)))
		      end
		end
	   | _ => ())
      fun use (b, x) =
	 if shouldConsider x
	    then
	       let val {used, ...} = varInfo x
	       in if (case !used of
			 [] => false
		       | b' :: _ => LiveInfo.equals (b, b'))
		     then ()
		  else List.push (used, b)
	       end
	 else ()
      fun uses (b: LiveInfo.t, xs: Var.t vector) =
	 Vector.foreach (xs, fn x => use (b, x))
      fun addEdgesForBlock (Block.T {label, statements, transfer, ...}): unit =
	 let
	    val {bodyInfo, ...} = labelInfo label
	    val b =
	       Vector.fold
	       (statements, bodyInfo,
		fn (Statement.T {var, exp, ...}, b) =>
		let
		   val b =
		      case exp of
			 ConApp {args, ...} => (uses (b, args); b)
		       | Const _ => b
		       | PrimApp {prim, args, ...} =>
			    (uses (b, args)
			     ; if (Prim.entersRuntime prim
				   orelse Prim.impCall prim)
				  then 
				     let
					val b' = LiveInfo.new ()
					val var =
					   case var of
					      NONE =>
						 Error.bug "Live.live needs var"
					    | SOME var => var
				     in
					LiveInfo.addEdge (b, b')
					; List.push (allPrims, (var, b'))
					; b'
				     end
			       else b)
		       | Select {tuple, ...} => (use (b, tuple); b)
		       | SetExnStackLocal => b
		       | SetExnStackSlot =>
			    let
			       val (_, {defuse}) = handlerSlotInfo
			       val _ = List.push (defuse, Use b)
			    in
			       b
			    end
		       | SetHandler _ =>
			    let
			       val ({defuse, ...}, _) = handlerSlotInfo
			       val _ = List.push (defuse, Def b)
			    in
			       b
			    end
		       | SetSlotExnStack =>
			    let
			       val (_, {defuse}) = handlerSlotInfo
			       val _ = List.push (defuse, Def b)
			    in
			       b
			    end
		       | Tuple xs => (uses (b, xs); b)
		       | Var y => (use (b, y); b)
		   val _ =
		      Option.app (var, fn x =>
				  newVarInfo (x, {defined = b}))
		in
		   b
		end)
	    fun goto l = LiveInfo.addEdge (b, #argInfo (labelInfo l))
	    val _ =
	       case transfer of
		  Bug => ()
		| Call {args, return, ...} =>
		     (uses (b, args)
		      ; (Option.app
			 (return, fn {cont, handler} =>
			  let
			     val _ =
				LiveInfo.addEdge (b, #frameInfo (labelInfo cont))
			  in
			     if isSome handler
				then
				   let val ({defuse = code_defuse, ...},
					    {defuse = link_defuse, ...}) =
				      handlerSlotInfo
				   in
				      List.push (code_defuse, Use b)
				      ; List.push (link_defuse, Use b)
				   end
			     else ()
			  end)))
		| Case {test, cases, default, ...} =>
		     (use (b, test)
		      ; Option.app (default, goto)
		      ; Cases.foreach (cases, goto))
		| Goto {dst, args, ...} =>
		     let
			val {block = Block.T {args = formals, ...}, ...} =
			   labelInfo dst
		     in
			Vector.foreach2 (formals, args, fn ((f, _), a) =>
					 if shouldConsider f
					    then use (b, a)
					 else ())
			; goto dst
		     end
		| Prim {args, failure, success, ...} =>
		     (uses (b, args)
		      ; goto failure
		      ; goto success)
		| Return xs => uses (b, xs)
		| Raise xs => uses (b, xs)
	 in ()
	 end
      val addEdgesForBlock =
	 Trace.trace ("Live.addEdgesForBlock",
		      Label.layout o Block.label,
		      Unit.layout)
	 addEdgesForBlock
      val head = LiveInfo.new ()
      val _ = Vector.foreach (args, fn (x, _) =>
			      newVarInfo (x, {defined = head}))
      val _ = Tree.foreachPre (Function.dominatorTree function,
			       addEdgesForBlock)
      fun processVar (x: Var.t): unit =
	 if not (shouldConsider x)
	    then ()
	 else
	    let
	       val {defined, used, ...} = varInfo x
	       val todo: LiveInfo.t list ref = ref []
	       fun consider (b as LiveInfo.T {live, ...}) =
		  if LiveInfo.equals (b, defined)
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
		   | LiveInfo.T {preds, ...} :: bs =>
			(todo := bs
			 ; List.foreach (!preds, consider)
			 ; loop ())
	       val _ = loop ()
	    in ()
	    end
      val _ = Vector.foreach (args, processVar o #1)
      val _ =
	 Vector.foreach
	 (blocks, fn Block.T {args, statements, ...} =>
	  (Vector.foreach (args, processVar o #1)
	   ; Vector.foreach (statements, fn Statement.T {var, ...} =>
			     Option.app (var, processVar))))
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
	    val todo: LiveInfo.t list ref = ref []
	    val defs =
	       List.foldr
	       (!defuse, [],
		fn (Def b, defs) => b::defs
		 | (Use (b as LiveInfo.T {liveHS, ...}), defs) =>
		      if List.exists (defs, fn b' => LiveInfo.equals (b, b'))
			 then defs
		      else (sel' liveHS := true
			    ; List.push(todo, b)
			    ; defs))
	    fun consider (b as LiveInfo.T {liveHS, ...}) =
	       if List.exists (defs, fn b' => LiveInfo.equals (b, b'))
		  orelse !(sel' liveHS)
		  then ()
	       else (sel' liveHS := true
		     ; List.push (todo, b))
	    fun loop () =
	       case !todo of
		  [] => ()
		| LiveInfo.T {preds, ...} :: bs =>
		     (todo := bs
		      ; List.foreach (!preds, consider)
		      ; loop ())
	    val _ = loop ()
	 in
	   ()
	 end)
      val {get = primLive: Var.t -> {vars: Var.t list,
				     handlerSlots: bool * bool},
	   set, ...} =
	 Property.getSetOnce (Var.plist,
			      Property.initRaise ("primLive", Var.layout))
      val _ =
	 List.foreach (!allPrims, fn (x, b) =>
		       set (x, {vars = LiveInfo.live b, 
				handlerSlots = LiveInfo.liveHS b}))
      fun labelLive (l: Label.t) =
	 let
	    val {bodyInfo, argInfo, frameInfo, ...} = labelInfo l
	 in
	    {begin = LiveInfo.live bodyInfo,
	     beginNoFormals = LiveInfo.live argInfo,
	     frame = LiveInfo.live frameInfo,
	     handlerSlots = LiveInfo.liveHS frameInfo}
	 end
      val _ =
	 Control.diagnostics
	 (fn display =>
	  let open Layout
	  in
	     Vector.foreach
	     (blocks, fn b =>
	      let
		 val l = Block.label b		 
		 val {begin, ...} = labelLive l
	      in
		 display (seq [Label.layout l,
			       str ":",
			       List.layout Var.layout begin])
	      end)
	  end)
   in {
       labelLive = labelLive,
       primLive = primLive
       }
   end

val live =
   Trace.trace2 ("Live.live", Func.layout o Function.name, Layout.ignore,
		 Layout.ignore)
   live

end
