
functor x86GenerateTransfersOld(S: X86_GENERATE_TRANSFERS_STRUCTS): X86_GENERATE_TRANSFERS =
struct

  open S
  open x86
  open x86JumpInfo
  open x86LoopInfo
  open x86Liveness.LiveInfo
  open x86Liveness.Liveness

  val rec ones : int -> word
    = fn 0 => 0wx0
       | n => Word.orb(Word.<<(ones (n-1), 0wx1),0wx1)

  val tracer = x86.tracer
  val tracerTop = x86.tracerTop

  structure x86LiveTransfers 
    = x86LiveTransfers(structure x86 = x86
		       structure x86MLton = x86MLton
		       structure x86Liveness = x86Liveness
		       structure x86JumpInfo = x86JumpInfo
		       structure x86LoopInfo = x86LoopInfo)

  val pointerSize = x86MLton.pointerSize
  val wordSize = x86MLton.wordSize

  val transferRegs
    =
(*
      x86.Register.eax::
      x86.Register.al::
*)
      x86.Register.ebx::
      x86.Register.bl::
      x86.Register.ecx::
      x86.Register.cl::
      x86.Register.edx:: 
      x86.Register.dl::
      x86.Register.edi::
      x86.Register.esi::
(*
      x86.Register.esp::
      x86.Register.ebp::
*)
      nil

  val transferFltRegs : Int.t = 6

  val indexReg = x86.Register.eax

  val stackTopReg = Register.ebp
  val frontierReg = Register.esp
  val stackTop = x86MLton.gcState_stackTopContents
  val frontier = x86MLton.gcState_frontierContents

  datatype gef = GEF of {generate : gef -> 
			            {label : Label.t,
				     falling : bool,
				     unique : bool} -> 
				    Assembly.t AppendList.t,
			 effect : gef -> 
			          {label : Label.t,
				   transfer : Transfer.t} ->
				  Assembly.t AppendList.t,
			 fall : gef ->
			        {label : Label.t,
				 live : MemLocSet.t} ->
				Assembly.t AppendList.t}

  fun generateTransfers {chunk as Chunk.T {data, blocks, ...},
			 optimize: int,
			 liveInfo : x86Liveness.LiveInfo.t,
			 jumpInfo : x86JumpInfo.t}
    = let
	val allClasses = !x86MLton.Classes.allClasses
	val livenessClasses = !x86MLton.Classes.livenessClasses
	val livenessClasses = ClassSet.add(livenessClasses, 
					   x86MLton.Classes.StaticNonTemp)
	val nonlivenessClasses = ClassSet.-(allClasses, livenessClasses)
	val holdClasses = !x86MLton.Classes.holdClasses
	val nonholdClasses = ClassSet.-(allClasses, holdClasses)
	val farflushClasses = ClassSet.-(nonlivenessClasses, holdClasses)
	val nearflushClasses = ClassSet.-(nonlivenessClasses, holdClasses)
	val runtimeClasses = !x86MLton.Classes.runtimeClasses
	val nonruntimeClasses = ClassSet.-(allClasses, runtimeClasses)
	val threadflushClasses = ClassSet.-(runtimeClasses, holdClasses)
	val cstaticClasses = !x86MLton.Classes.cstaticClasses
	val heapClasses = !x86MLton.Classes.heapClasses
	val ccallflushClasses = ClassSet.+(cstaticClasses, heapClasses)
	  
	fun removeHoldMemLocs memlocs
	  = MemLocSet.subset
	    (memlocs, 
	     fn m => not (ClassSet.contains(holdClasses, MemLoc.class m)))

	fun runtimeEntry l
	  = AppendList.cons
	    (Assembly.directive_assume
	     {assumes
	      = [{register = stackTopReg,
		  memloc = stackTop,
		  weight = 1024,
		  sync = false,
		  reserve = false},
		 {register = frontierReg,
		  memloc = frontier,
		  weight = 2048,
		  sync = false,
		  reserve = false}]},
	     l)

	fun farEntry l
	  = AppendList.cons
	    (Assembly.directive_assume
	     {assumes
	      = [{register = stackTopReg,
		  memloc = stackTop,
		  weight = 1024,
		  sync = false,
		  reserve = false},
		 {register = frontierReg,
		  memloc = frontier,
		  weight = 2048,
		  sync = false,
		  reserve = false}]},
	     l)

	fun farTransfer live setup trans
	  = AppendList.appends
	    [AppendList.single
	     (Assembly.directive_force
	      {commit_memlocs = removeHoldMemLocs live,
	       commit_classes = ClassSet.empty,
	       remove_memlocs = MemLocSet.empty,
	       remove_classes = ClassSet.empty,
	       dead_memlocs = MemLocSet.empty,
	       dead_classes = ClassSet.empty}),
	     setup,
	     AppendList.fromList
	     [(Assembly.directive_cache
	       {caches = [{register = stackTopReg,
			   memloc = stackTop,
			   reserve = true},
			  {register = frontierReg,
			   memloc = frontier,
			   reserve = true}]}),
	      (Assembly.directive_clearflt ()),
	      (Assembly.directive_force
	       {commit_memlocs = MemLocSet.empty,
		commit_classes = farflushClasses,
		remove_memlocs = MemLocSet.empty,
		remove_classes = ClassSet.empty,
		dead_memlocs = MemLocSet.empty,
		dead_classes = ClassSet.empty})],
	     trans]
	    
	val _
	  = Assert.assert
	    ("verifyLiveInfo",
	     fn () => x86Liveness.LiveInfo.verifyLiveInfo {chunk = chunk,
							   liveInfo = liveInfo})
	val _
	  = Assert.assert
	    ("verifyJumpInfo", 
	     fn () => x86JumpInfo.verifyJumpInfo {chunk = chunk,
						  jumpInfo = jumpInfo})

	val _
	  = Assert.assert
	    ("verifyEntryTransfer", 
	     fn () => x86EntryTransfer.verifyEntryTransfer {chunk = chunk})

	local
	  val gotoInfo as {get: Label.t -> {block:Block.t},
			   set,
			   destroy}
	    = Property.destGetSetOnce
	      (Label.plist, Property.initRaise ("gotoInfo", Label.layout))

	  val labels
	    = List.fold
	      (blocks, [],
	       fn (block as Block.T {entry, ...}, labels)
	        => let
		     val label = Entry.label entry
		   in
		     set(label, {block = block}) ;
		     label::labels
		   end)
	      
	  fun loop labels
	    = let
		val (labels, b)
		  = List.fold
		    (labels, ([], false),
		     fn (label, (labels, b))
		      => case x86JumpInfo.getNear (jumpInfo, label)
			   of x86JumpInfo.Count 0 
			    => let
				 val {block as Block.T {transfer, ...}}
				   = get label
			       in
				 List.foreach 
				 (Transfer.nearTargets transfer,
				  fn label 
				   => x86JumpInfo.decNear (jumpInfo, label));
				 (labels, true)
			       end
			    | _ => (label::labels, b))
	      in
		if b
		  then loop labels
		  else List.map (labels, #block o get)
	      end
	  val blocks = loop labels
	    
	  val _ = destroy ()
	in
	  val chunk = Chunk.T {data = data, blocks = blocks}
	end

	val loopInfo
	  = x86LoopInfo.createLoopInfo {chunk = chunk, farLoops = false}

	val liveTransfers
	  = x86LiveTransfers.computeLiveTransfers
	    {chunk = chunk,
	     transferRegs = transferRegs,
	     transferFltRegs = transferFltRegs,
	     liveInfo = liveInfo,
	     jumpInfo = jumpInfo,
	     loopInfo = loopInfo}
	    handle exn
	     => Error.bug ("x86LiveTransfers.computeLiveTransfers::" ^
			   (case exn
			      of Fail s => s
			       | _ => "?"))

	val getLiveRegsTransfers
	  = #1 o x86LiveTransfers.getLiveTransfers
	val getLiveFltRegsTransfers
	  = #2 o x86LiveTransfers.getLiveTransfers

	val layoutInfo as {get = getLayoutInfo : Label.t -> Block.t option,
			   set = setLayoutInfo,
			   destroy = destLayoutInfo}
	  = Property.destGetSet(Label.plist, 
				Property.initRaise ("layoutInfo", Label.layout))
	val profileInfo as {get = getProfileInfo : Label.t -> ProfileInfo.t,
			    set = setProfileInfo,
			    destroy = destProfileInfo}
	  = Property.destGetSet(Label.plist, 
				Property.initRaise ("profileInfo", Label.layout))

	val _ 
	  = List.foreach
	    (blocks,
	     fn block as Block.T {entry,profileInfo,...}
	      => let
		   val label = Entry.label entry
		 in 
		   setLayoutInfo(label, SOME block);
		   setProfileInfo(label, profileInfo)
		 end)

	local	
	  val stack = ref []
	  val queue = ref (Queue.empty ())
	in
	  fun enque x = queue := Queue.enque(!queue, x)
	  fun push x = stack := x::(!stack)

	  fun deque () = (case (!stack)
			    of [] => (case Queue.deque(!queue)
					of NONE => NONE
					 | SOME(x, queue') => (queue := queue';
							       SOME x))
			     | x::stack' => (stack := stack';
					     SOME x))
	end

	fun pushCompensationBlock {label, id}
	  = let
	      val label' = Label.new label
	      val profileInfo
		= ProfileInfo.add
		  (getProfileInfo label,
		   {profileLevel = 2,
		    profileName = Label.toString label'})

	      val live = getLive(liveInfo, label)
	      val block
		= Block.T {entry = Entry.jump {label = label'},
			   profileInfo = profileInfo,
			   statements 
			   = (Assembly.directive_restoreregalloc
			      {live = MemLocSet.add
			              (MemLocSet.add
				       (live,
					stackTop),
			              frontier),
			       id = id})::
			     nil,
			   transfer = Transfer.goto {target = label}}
	    in
	      setLive(liveInfo, label', live);
	      incNear(jumpInfo, label');
	      Assert.assert("pushCompensationBlock",
			    fn () => getNear(jumpInfo, label') = Count 1);
	      x86LiveTransfers.setLiveTransfersEmpty(liveTransfers, label');
	      setLayoutInfo(label', SOME block);
	      setProfileInfo(label', profileInfo);
	      push label';
	      label'
	    end

	datatype z = datatype Entry.t
	fun generateAll (gef as GEF {generate,effect,fall})
	                {label, falling, unique} : 
			Assembly.t AppendList.t
	  = (case getLayoutInfo label
	       of NONE => AppendList.empty
	        | SOME (Block.T {entry, profileInfo,
				 statements, transfer})
		=> let
		     val _ = setLayoutInfo(label, NONE)

		     val profile_assembly
		       = ProfileInfo.profile_assembly profileInfo
		     val profile_assembly
		       = AppendList.fromList profile_assembly

		     val isLoopHeader = fn _ => false

		     fun near label
		       = if falling
			   then if unique
				  then AppendList.appends
				       [AppendList.fromList
					(if isLoopHeader(loopInfo, label)
					    handle _ => false
					   then [Assembly.pseudoop_p2align 
						 (Immediate.const_int 4,
						  NONE,
						  SOME (Immediate.const_int 7)),
						 Assembly.label label]
					   else [Assembly.label label]),
					profile_assembly]
				  else AppendList.appends
				       [AppendList.fromList
					(if isLoopHeader(loopInfo, label)
					    handle _ => false
					   then [Assembly.pseudoop_p2align 
						 (Immediate.const_int 4,
						  NONE,
						  SOME (Immediate.const_int 7)),
						 Assembly.label label]
					   else [Assembly.label label]),
					AppendList.fromList
					[(* near entry & 
					  * live transfer assumptions *)
					 (Assembly.directive_assume
					  {assumes
					   = ({register = stackTopReg,
					       memloc = stackTop,
					       weight = 1024,
					       sync = false,
					       reserve = false})::
					   ({register = frontierReg,
					     memloc = frontier,
					     weight = 2048,
					     sync = false,
					     reserve = false})::
					   (List.map
					    (getLiveRegsTransfers
					     (liveTransfers, label),
					     fn (memloc,register,sync)
					      => {register = register,
						  memloc = memloc,
						  sync = sync, 
						  weight = 1024,
						  reserve = false}))}),
					 (Assembly.directive_fltassume
					  {assumes
					   = (List.map
					      (getLiveFltRegsTransfers
					       (liveTransfers, label),
					       fn (memloc,sync)
					        => {memloc = memloc,
						    sync = sync,
						    weight = 1024}))})],
					profile_assembly]
			   else AppendList.appends
			        [AppendList.fromList
				 (if isLoopHeader(loopInfo, label)
				     handle _ => false
				    then [Assembly.pseudoop_p2align 
					  (Immediate.const_int 4,
					   NONE,
					   SOME (Immediate.const_int 7)),
					  Assembly.label label]
				    else [Assembly.pseudoop_p2align
					  (Immediate.const_int 4, 
					   NONE, 
					   NONE),
					  Assembly.label label]),
				 AppendList.fromList
				 [(* near entry & 
				   * live transfer assumptions *)
				  (Assembly.directive_assume
				   {assumes
				    = ({register = stackTopReg,
					memloc = stackTop,
					weight = 1024,
					sync = false,
					reserve = false})::
				    ({register = frontierReg,
				      memloc = frontier,
				      weight = 2048,
				      sync = false,
				      reserve = false})::
				    (List.map
				     (getLiveRegsTransfers
				      (liveTransfers, label),
				      fn (memloc,register,sync)
				       => {register = register,
					   memloc = memloc,
					   sync = sync, 
					   weight = 1024,
					   reserve = false}))}),
				  (Assembly.directive_fltassume
				   {assumes
				    = (List.map
				       (getLiveFltRegsTransfers
					(liveTransfers, label),
					fn (memloc,sync)
					 => {memloc = memloc,
					     sync = sync,
					     weight = 1024}))})],
				 profile_assembly]

		     val pre
		       = case entry
			   of Jump {label}
			    => near label
			    | CReturn {label, dst}
			    => AppendList.append
			       (near label,
				case dst
				  of NONE => AppendList.empty
				   | SOME (dst, dstsize)
				   => (case Size.class dstsize
					 of Size.INT
					  => AppendList.single
					     (x86.Assembly.instruction_mov
					      {dst = dst,
					       src = x86MLton.cReturnTempContentsOperand dstsize,
					       size = dstsize})
					  | Size.FLT
					  => AppendList.single
					     (x86.Assembly.instruction_pfmov
					      {dst = dst,
					       src = x86MLton.cReturnTempContentsOperand dstsize,
					       size = dstsize})
					  | _ => Error.bug "CReturn"))
			    | Func {label,...}
			    => AppendList.append
			       (AppendList.fromList
				[Assembly.pseudoop_p2align 
				 (Immediate.const_int 4, NONE, NONE),
				 Assembly.pseudoop_global label,
				 Assembly.label label],
				(* entry from far assumptions *)
				(farEntry profile_assembly))
			    | Cont {label, 
				    frameInfo as Entry.FrameInfo.T 
				                 {size,
						  frameLayoutsIndex},
				    ...}
			    => AppendList.append
			       (AppendList.fromList
				[Assembly.pseudoop_p2align
				 (Immediate.const_int 4, NONE, NONE),
				 Assembly.pseudoop_long
				 [Immediate.const_int frameLayoutsIndex],
				 Assembly.label label],
				(* entry from far assumptions *)
				(farEntry
				 (AppendList.snoc
				  (profile_assembly,
				   let
				     val stackTop 
				       = x86MLton.gcState_stackTopContentsOperand
				     val bytes 
				       = x86.Operand.immediate_const_int (~ size)
				   in
				     (* stackTop += bytes *)
				     x86.Assembly.instruction_binal 
				     {oper = x86.Instruction.ADD,
				      dst = stackTop,
				      src = bytes, 
				      size = pointerSize}
				   end))))
			    | Handler {label,
				       offset, 
				       ...}
			    => AppendList.append
			       (AppendList.fromList
				[Assembly.pseudoop_p2align 
				 (Immediate.const_int 4, NONE, NONE),
				 Assembly.label label],
				(* entry from far assumptions *)
				(farEntry
				 (AppendList.snoc
				  (profile_assembly,
				   let
				     val stackTop 
				       = x86MLton.gcState_stackTopContentsOperand
				     val bytes 
				       = x86.Operand.immediate_const_int (~ offset)
				   in
				     (* stackTop += bytes *)
				     x86.Assembly.instruction_binal 
				     {oper = x86.Instruction.ADD,
				      dst = stackTop,
				      src = bytes, 
				      size = pointerSize}
				   end))))
			    | Runtime {label,
				       frameInfo as Entry.FrameInfo.T 
				                    {size,
						     frameLayoutsIndex}}
			    => AppendList.append
			       (AppendList.fromList
				[Assembly.pseudoop_p2align 
				 (Immediate.const_int 4, NONE, NONE),
				 Assembly.pseudoop_long 
				 [Immediate.const_int frameLayoutsIndex],
				 Assembly.label label],
				(* entry from far assumptions *)
				(farEntry
				 (AppendList.snoc
				  (profile_assembly,
				   let
				     val stackTop 
				       = x86MLton.gcState_stackTopContentsOperand
				     val bytes 
				       = x86.Operand.immediate_const_int (~ size)
				   in
				     (* stackTop += bytes *)
				     x86.Assembly.instruction_binal 
				     {oper = x86.Instruction.ADD,
				      dst = stackTop,
				      src = bytes, 
				      size = pointerSize}
				   end))))
			       
		     val pre
		       = AppendList.appends
		         [if !Control.Native.commented > 1
			    then AppendList.single
			         (Assembly.comment (Entry.toString entry))
			    else AppendList.empty,
			  if !Control.Native.commented > 2
			    then AppendList.single
			         (Assembly.comment 
				  (MemLocSet.fold
				   (getLive(liveInfo, label),
				    "",
				    fn (memloc, s)
				     => concat [s, 
						MemLoc.toString memloc, 
						" "])))
			    else AppendList.empty,
			  pre]

		     val (statements,live)
		       = List.foldr
		         (statements,
			  ([], 
			   #liveIn (livenessTransfer {transfer = transfer, 
						      liveInfo = liveInfo})),
			  fn (assembly,(statements,live))
			   => let
				val live as {liveIn,dead, ...}
				  = livenessAssembly {assembly = assembly,
						      live = live}
			      in
				(if MemLocSet.isEmpty dead
				   then assembly::statements
				   else assembly::
				        (Assembly.directive_force
					 {commit_memlocs = MemLocSet.empty,
					  commit_classes = ClassSet.empty,
					  remove_memlocs = MemLocSet.empty,
					  remove_classes = ClassSet.empty,
					  dead_memlocs = dead,
					  dead_classes = ClassSet.empty})::
					statements,
				 liveIn)
			      end)

		     val statements = AppendList.fromList statements

		     val transfer = effect gef {label = label, 
						transfer = transfer}
		   in
		     AppendList.appends 
		     [pre,
		      statements,
		      transfer]
		   end)

	datatype z = datatype Transfer.t
	fun effectDefault (gef as GEF {generate,effect,fall})
	                  {label, transfer} : Assembly.t AppendList.t
	  = AppendList.append
	    (if !Control.Native.commented > 1
	       then AppendList.single
		    (Assembly.comment
		     (Transfer.toString transfer))
	       else AppendList.empty,
	     case transfer
	       of Goto {target}
		=> fall gef
		        {label = target,
			 live = getLive(liveInfo, target)}
		| Iff {condition, truee, falsee}
		=> let
		     val condition_neg
		       = Instruction.condition_negate condition

		     val truee_live
		       = getLive(liveInfo, truee)
		     val truee_live_length
		       = MemLocSet.size truee_live

		     val falsee_live
		       = getLive(liveInfo, falsee)
		     val falsee_live_length
		       = MemLocSet.size falsee_live

		     fun fall_truee ()
		       = let
			   val id = Directive.Id.new ()
			   val falsee'
			     = pushCompensationBlock {label = falsee,
						      id = id};
			 in
			   AppendList.append
			   (AppendList.fromList
			    [Assembly.directive_force
			     {commit_memlocs = MemLocSet.empty,
			      commit_classes = nearflushClasses,
			      remove_memlocs = MemLocSet.empty,
			      remove_classes = ClassSet.empty,
			      dead_memlocs = MemLocSet.empty,
			      dead_classes = ClassSet.empty},
			     Assembly.directive_saveregalloc
			     {live = MemLocSet.add
			             (MemLocSet.add
				      (falsee_live,
				       stackTop),
				      frontier),
			      id = id},
			     Assembly.instruction_jcc
			     {condition = condition_neg,
			      target = Operand.label falsee'}],
			    (fall gef 
			          {label = truee,
				   live = truee_live}))
			 end

		     fun fall_falsee ()
		       = let
			   val id = Directive.Id.new ()
			   val truee' = pushCompensationBlock {label = truee,
							       id = id};
			 in
			   AppendList.append
			   (AppendList.fromList
			    [Assembly.directive_force
			     {commit_memlocs = MemLocSet.empty,
			      commit_classes = nearflushClasses,
			      remove_memlocs = MemLocSet.empty,
			      remove_classes = ClassSet.empty,
			      dead_memlocs = MemLocSet.empty,
			      dead_classes = ClassSet.empty},
			     Assembly.directive_saveregalloc
			     {live = MemLocSet.add
			             (MemLocSet.add
				      (truee_live,
				       stackTop),
				      frontier),
			      id = id},
			     Assembly.instruction_jcc
			     {condition = condition,
			      target = Operand.label truee'}],
			    (fall gef 
			          {label = falsee,
				   live = falsee_live}))
			 end
		   in
		     case (getLayoutInfo truee,
			   getLayoutInfo falsee)
		       of (NONE, SOME _) => fall_falsee ()
			| (SOME _, NONE) => fall_truee ()
			| _ 
			=> let
			     fun default' ()
			       = if truee_live_length <= falsee_live_length
				   then fall_falsee ()
				   else fall_truee ()

			     fun default ()
			       = case (getNear(jumpInfo, truee),
				       getNear(jumpInfo, falsee))
				   of (Count 1, Count 1) => default' ()
				    | (Count 1, _) => fall_truee ()
				    | (_, Count 1) => fall_falsee ()
				    | _ => default' ()
			   in 
			     case (getLoopDistance(loopInfo, label, truee),
				   getLoopDistance(loopInfo, label, falsee))
			       of (NONE, NONE) => default ()
				| (SOME _, NONE) => fall_truee ()
				| (NONE, SOME _) => fall_falsee ()
			        | (SOME dtruee, SOME dfalsee)
				=> (case Int.compare(dtruee, dfalsee)
				      of EQUAL => default ()
				       | LESS => fall_falsee ()
				       | MORE => fall_truee ())
			   end
		   end
		| Switch {test, cases, default}
		=> let
		     val {dead, ...}
		       = livenessTransfer {transfer = transfer,
					   liveInfo = liveInfo}

		     val size
		       = case Operand.size test
			   of SOME size => size
			    | NONE => Size.LONG

		     val default_live
		       = getLive(liveInfo, default)

		     val cases
		       = Transfer.Cases.map'
		         (cases,
			  fn (k, target)
			   => let
				val target_live
				  = getLive(liveInfo, target)
				val id = Directive.Id.new ()
				val target' = pushCompensationBlock 
				              {label = target,
					       id = id}
			      in
				AppendList.fromList
				[Assembly.instruction_cmp
				 {src1 = test,
				  src2 = Operand.immediate_const k,
				  size = size},
				 Assembly.directive_saveregalloc
				 {live = MemLocSet.add
				         (MemLocSet.add
					  (target_live,
					   stackTop),
					  frontier),
				  id = id},
				 Assembly.instruction_jcc
				 {condition = Instruction.E,
				  target = Operand.label target'}]
			      end,
			  fn (c, target) => (Immediate.Char c, target),
			  fn (i, target) => (Immediate.Int i, target),
			  fn (w, target) => (Immediate.Word w, target))
		   in
		     AppendList.appends
		     [AppendList.single
		      (Assembly.directive_force
		       {commit_memlocs = MemLocSet.empty,
			commit_classes = nearflushClasses,
			remove_memlocs = MemLocSet.empty,
			remove_classes = ClassSet.empty,
			dead_memlocs = MemLocSet.empty,
			dead_classes = ClassSet.empty}),
		      AppendList.appends cases,
		      if MemLocSet.isEmpty dead
			then AppendList.empty
			else AppendList.single
			     (Assembly.directive_force
			      {commit_memlocs = MemLocSet.empty,
			       commit_classes = ClassSet.empty,
			       remove_memlocs = MemLocSet.empty,
			       remove_classes = ClassSet.empty,
			       dead_memlocs = dead,
			       dead_classes = ClassSet.empty}),
		      (fall gef
		            {label = default,
			     live = default_live})]
		   end
                | Tail {target, live}
		=> (* flushing at far transfer *)
		   (farTransfer live
		    AppendList.empty
		    (AppendList.single
		     (Assembly.instruction_jmp
		      {target = Operand.label target,
		       absolute = false})))
		| NonTail {target, live, return, handler, size}
		=> let
		     val _ = enque return
		     val _ = case handler
			       of SOME handler => enque handler
				| NONE => ()

		     val stackTop 
		       = x86MLton.gcState_stackTopContentsOperand
		     val stackTopMinusWordDeref'
		       = x86MLton.gcState_stackTopMinusWordDeref
		     val stackTopMinusWordDeref
		       = x86MLton.gcState_stackTopMinusWordDerefOperand
		     val bytes 
		       = x86.Operand.immediate_const_int size

		     val liveReturn = x86Liveness.LiveInfo.getLive(liveInfo, return)
		     val liveHandler 
		       = case handler
			   of SOME handler
			    => x86Liveness.LiveInfo.getLive(liveInfo, handler)
			    | _ => MemLocSet.empty
		     val live = MemLocSet.unions [live,
						  liveReturn,
						  liveHandler]
		   in
		     (* flushing at far transfer *)
		     (farTransfer live
		      (AppendList.fromList
		       [(* stackTop += bytes *)
			x86.Assembly.instruction_binal 
			{oper = x86.Instruction.ADD,
			 dst = stackTop,
			 src = bytes, 
			 size = pointerSize},
			(* *(stackTop - WORD_SIZE) = return *)
			x86.Assembly.instruction_mov
			{dst = stackTopMinusWordDeref,
			 src = Operand.immediate_label return,
			 size = pointerSize},
			x86.Assembly.directive_force
			{commit_memlocs = MemLocSet.singleton stackTopMinusWordDeref',
			 commit_classes = ClassSet.empty,
			 remove_memlocs = MemLocSet.empty,
			 remove_classes = ClassSet.empty,
			 dead_memlocs = MemLocSet.empty,
			 dead_classes = ClassSet.empty}])
		      (AppendList.single
		       (Assembly.instruction_jmp
			{target = Operand.label target,
			 absolute = false})))
		   end
		| Return {live}
		=> let
		     val stackTopMinusWordDeref
		       = x86MLton.gcState_stackTopMinusWordDerefOperand
		   in
		     (* flushing at far transfer *)
		     (farTransfer live
		      AppendList.empty
		      (AppendList.single
		       (* jmp *(stackTop - WORD_SIZE) *)
		       (x86.Assembly.instruction_jmp
			{target = stackTopMinusWordDeref,
			 absolute = true})))
		   end
		| Raise {live}
		=> let
		     val exnStack 
		       = x86MLton.gcState_currentThread_exnStackContentsOperand
		     val stackTop 
		       = x86MLton.gcState_stackTopContentsOperand
		     val stackTopMinusWordDeref
		       = x86MLton.gcState_stackTopMinusWordDerefOperand
		     val stackBottom 
		       = x86MLton.gcState_stackBottomContentsOperand
		    in
		      (* flushing at far transfer *)
		      (farTransfer live
		       (AppendList.fromList
			[(* stackTop = stackBottom + exnStack *)
			 x86.Assembly.instruction_mov
			 {dst = stackTop,
			  src = stackBottom,
			  size = pointerSize},
			 x86.Assembly.instruction_binal
			 {oper = x86.Instruction.ADD,
			  dst = stackTop,
			  src = exnStack,
			  size = pointerSize}])
		       (AppendList.single
			(* jmp *(stackTop - WORD_SIZE) *)
			(x86.Assembly.instruction_jmp
			 {target = stackTopMinusWordDeref,
			  absolute = true})))
		    end
		| Runtime {prim, args, return, size}
		=> let
		     val _ = enque return
		     
		     val {dead, ...}
		       = livenessTransfer {transfer = transfer,
					   liveInfo = liveInfo}

		     val stackTop'
		       = x86MLton.gcState_stackTopContents
		     val stackTop 
		       = x86MLton.gcState_stackTopContentsOperand
		     val bytes 
		       = x86.Operand.immediate_const_int size
		     val stackTopMinusWordDeref
		       = x86MLton.gcState_stackTopMinusWordDerefOperand

		     val live = x86Liveness.LiveInfo.getLive(liveInfo, return)

		     fun default f
		       = let
			   val target = Label.fromString f

			   val c_stackP
			     = x86MLton.c_stackPContentsOperand
			   val c_stackPDerefDouble
			     = x86MLton.c_stackPDerefDoubleOperand
			   val applyFFTemp
			     = x86MLton.applyFFTempContentsOperand

			   val (assembly_args,size_args)
			     = List.fold
			       (args,(AppendList.empty,0),
				fn ((arg,size),
				    (assembly_args,size_args)) 
				 => (AppendList.append
				     (if Size.eq(size,Size.DBLE)
					then AppendList.fromList
					     [Assembly.instruction_binal
					      {oper = Instruction.SUB,
					       dst = c_stackP,
					       src = Operand.immediate_const_int 8,
					       size = pointerSize},
					      Assembly.instruction_pfmov
					      {src = arg,
					       dst = c_stackPDerefDouble,
					       size = size}]
					else if Size.eq(size,Size.BYTE)
					       then AppendList.fromList
						    [Assembly.instruction_movx
						     {oper = Instruction.MOVZX,
						      dst = applyFFTemp,
						      src = arg,
						      dstsize = wordSize,
						      srcsize = size},
						     Assembly.instruction_ppush
						     {src = applyFFTemp,
						      base = c_stackP,
						      size = wordSize}]
					       else AppendList.single
						    (Assembly.instruction_ppush
						     {src = arg,
						      base = c_stackP,
						      size = size}),
						    assembly_args),
				     (Size.toBytes size) + size_args))
			 in
			   AppendList.appends
			   [AppendList.single
			    ((* explicit cache in case there are no args *)
			     Assembly.directive_cache 
			     {caches = [{register = Register.esp,
					 memloc = valOf (Operand.deMemloc c_stackP),
					 reserve = true}]}),
			    assembly_args,
			    AppendList.fromList
			    [x86.Assembly.directive_force
			     {commit_memlocs = MemLocSet.empty,
			      commit_classes = ClassSet.empty,
			      remove_memlocs = MemLocSet.empty,
			      remove_classes = ClassSet.empty,
			      dead_memlocs = dead,
			      dead_classes = ClassSet.empty},
			     (* stackTop += bytes *)
			     x86.Assembly.instruction_binal 
			     {oper = x86.Instruction.ADD,
			      dst = stackTop,
			      src = bytes, 
			      size = pointerSize},
			     (* *(stackTop - WORD_SIZE) = return *)
			     x86.Assembly.instruction_mov
			     {dst = stackTopMinusWordDeref,
			      src = Operand.immediate_label return,
			      size = pointerSize},
			     (* flushing at Runtime *)
			     Assembly.directive_force
			     {commit_memlocs = live,
			      commit_classes = runtimeClasses,
			      remove_memlocs = MemLocSet.empty,
			      remove_classes = ClassSet.empty,
			      dead_memlocs = MemLocSet.empty,
			      dead_classes = ClassSet.empty},
			     Assembly.directive_ccall (),
			     Assembly.instruction_call 
			     {target = Operand.label target,
			      absolute = false},
			     Assembly.directive_force
			     {commit_memlocs = MemLocSet.empty,
			      commit_classes = ClassSet.empty,
			      remove_memlocs = MemLocSet.empty,
			      remove_classes = ClassSet.empty,
			      dead_memlocs = MemLocSet.empty,
			      dead_classes = runtimeClasses}],
			    (if size_args > 0
			       then AppendList.single
				    (Assembly.instruction_binal
				     {oper = Instruction.ADD,
				      dst = c_stackP,
				      src = Operand.immediate_const_int size_args,
				      size = pointerSize})
			       else AppendList.empty),
			    AppendList.single
			    (Assembly.directive_unreserve 
			     {registers = [Register.esp]}),
			    (* flushing at far transfer *)
			    (farTransfer MemLocSet.empty
			     AppendList.empty
			     (AppendList.single
			      (* jmp *(stackTop - WORD_SIZE) *)
			      (x86.Assembly.instruction_jmp
			       {target = stackTopMinusWordDeref,
				absolute = true})))]
			 end

		     fun thread ()
		       = let
			   val (thread,threadsize)
			     = case args
				 of [_, (thread,threadsize)] => (thread,threadsize)
				  | _ => Error.bug "x86GenerateTransfers::Runtime: args"

			   val threadTemp
			     = x86MLton.threadTempContentsOperand

			   val currentThread
			     = x86MLton.gcState_currentThreadContentsOperand	
			   val stack
			     = x86MLton.gcState_currentThread_stackContentsOperand
			   val stack_used
			     = x86MLton.gcState_currentThread_stack_usedContentsOperand
			   val stack_reserved
			     = x86MLton.gcState_currentThread_stack_reservedContentsOperand
			   val stackBottom
			     = x86MLton.gcState_stackBottomContentsOperand
			   val stackLimit
			     = x86MLton.gcState_stackLimitContentsOperand
			   val maxFrameSize
			     = x86MLton.gcState_maxFrameSizeContentsOperand
			   val canHandle
			     = x86MLton.gcState_canHandleContentsOperand
			   val signalIsPending
			     = x86MLton.gcState_signalIsPendingContentsOperand
			   val limit
			     = x86MLton.gcState_limitContentsOperand
			   val base
			     = x86MLton.gcState_baseContentsOperand

			   val resetJoin = Label.newString "resetJoin"
			 in
			   AppendList.append
			   (AppendList.fromList
			    [(* threadTemp = thread *)
			     Assembly.instruction_mov
			     {dst = threadTemp,
			      src = thread,
			      size = pointerSize},
			     (* stackTop += bytes *)
			     x86.Assembly.instruction_binal 
			     {oper = x86.Instruction.ADD,
			      dst = stackTop,
			      src = bytes, 
			      size = pointerSize},
			     (* *(stackTop - WORD_SIZE) = return *)
			     x86.Assembly.instruction_mov
			     {dst = stackTopMinusWordDeref,
			      src = Operand.immediate_label return,
			      size = pointerSize},
			     (* flushing at Runtime *)
			     Assembly.directive_force
			     {commit_memlocs = live,
			      commit_classes = threadflushClasses,
			      remove_memlocs = MemLocSet.empty,
			      remove_classes = ClassSet.empty,
			      dead_memlocs = MemLocSet.empty,
			      dead_classes = ClassSet.empty},
			     Assembly.directive_force
			     {commit_memlocs = MemLocSet.empty,
			      commit_classes = ClassSet.empty,
			      remove_memlocs = MemLocSet.empty,
			      remove_classes = ClassSet.empty,
			      dead_memlocs = MemLocSet.empty,
			      dead_classes = threadflushClasses},
			     (* currentThread->stack->used
			      *   = stackTop + wordSize - stackBottom
			      *)
			     Assembly.instruction_mov
			     {dst = stack_used,
			      src = stackTop,
			      size = pointerSize},
			     Assembly.instruction_binal
			     {oper = Instruction.ADD,
			      dst = stack_used,
			      src = Operand.immediate_const_int x86MLton.wordBytes,
			      size = pointerSize},
			     Assembly.instruction_binal
			     {oper = Instruction.SUB,
			      dst = stack_used,
			      src = stackBottom,
			      size = pointerSize},
			     (* currentThread = threadTemp *)
			     Assembly.instruction_mov
			     {src = threadTemp,
			      dst = currentThread,
			      size = pointerSize},
			     (* stackBottom = currentThread->stack + 8 *)
			     Assembly.instruction_mov
			     {dst = stackBottom,
			      src = stack,
			      size = pointerSize},
			     Assembly.instruction_binal
			     {oper = Instruction.ADD,
			      dst = stackBottom,
			      src = Operand.immediate_const_int 8,
			      size = pointerSize},
			     (* stackTop
			      *   = stackBottom + currentThread->stack->used - wordSize
			      *)
			     Assembly.instruction_mov
			     {dst = stackTop,
			      src = stackBottom,
			      size = pointerSize},
			     Assembly.instruction_binal
			     {oper = Instruction.ADD,
			      dst = stackTop,
			      src = stack_used,
			      size = pointerSize},
			     Assembly.instruction_binal
			     {oper = Instruction.SUB,
			      dst = stackTop,
			      src = Operand.immediate_const_int x86MLton.wordBytes,
			      size = pointerSize},
			     (* stackLimit
			      *   = stackBottom + currentThread->stack->reserved
			      *                 - 2 * maxFrameSize
			      *)
			     Assembly.instruction_mov
			     {dst = stackLimit,
			      src = stackBottom,
			      size = pointerSize},
			     Assembly.instruction_binal
			     {oper = Instruction.ADD,
			      dst = stackLimit,
			      src = stack_reserved,
			      size = pointerSize},
			     Assembly.instruction_binal
			     {oper = Instruction.SUB,
			      dst = stackLimit,
			      src = maxFrameSize,
			      size = pointerSize},
			     Assembly.instruction_binal
			     {oper = Instruction.SUB,
			      dst = stackLimit,
			      src = maxFrameSize,
			      size = pointerSize},
			     (* gcState.canHandle-- *)
			     Assembly.instruction_unal
			     {oper = Instruction.DEC,
			      dst = canHandle,
			      size = wordSize},
			     (* if (0 == canHandle && signalIsPending)
			      *   limit = 0
			      *)
			     Assembly.directive_cache
			     {caches = [{register = stackTopReg,
					 memloc = stackTop',
					 reserve = true},
					{register = frontierReg,
					 memloc = frontier,
					 reserve = true}]},
			     Assembly.directive_force
			     {commit_memlocs = MemLocSet.empty,
			      commit_classes = farflushClasses,
			      remove_memlocs = MemLocSet.empty,
			      remove_classes = ClassSet.empty,
			      dead_memlocs = MemLocSet.empty,
			      dead_classes = ClassSet.empty},
			     Assembly.instruction_jcc
			     {condition = Instruction.NZ,
			      target = Operand.label resetJoin},
			     Assembly.instruction_cmp
			     {src1 = signalIsPending,
			      src2 = Operand.immediate_const_int 0,
			      size = wordSize},
			     Assembly.directive_cache
			     {caches = [{register = stackTopReg,
					 memloc = stackTop',
					 reserve = true},
					{register = frontierReg,
					 memloc = frontier,
					 reserve = true}]},
			     Assembly.directive_force
			     {commit_memlocs = MemLocSet.empty,
			      commit_classes = farflushClasses,
			      remove_memlocs = MemLocSet.empty,
			      remove_classes = ClassSet.empty,
			      dead_memlocs = MemLocSet.empty,
			      dead_classes = ClassSet.empty},
			     Assembly.instruction_jcc
			     {condition = Instruction.E,
			      target = Operand.label resetJoin},
			     Assembly.instruction_mov
			     {dst = limit,
			      src = Operand.immediate_const_int 0,
			      size = pointerSize},
			     Assembly.directive_cache
			     {caches = [{register = stackTopReg,
					 memloc = stackTop',
					 reserve = true},
					{register = frontierReg,
					 memloc = frontier,
					 reserve = true}]},
			     Assembly.directive_force
			     {commit_memlocs = MemLocSet.empty,
			      commit_classes = farflushClasses,
			      remove_memlocs = MemLocSet.empty,
			      remove_classes = ClassSet.empty,
			      dead_memlocs = MemLocSet.empty,
			      dead_classes = ClassSet.empty},
			     Assembly.directive_reset (),
			     Assembly.label resetJoin,
			     Assembly.directive_assume
			     {assumes
			      = [{register = stackTopReg,
				  memloc = stackTop',
				  weight = 1024,
				  sync = false,
				  reserve = false},
				 {register = frontierReg,
				  memloc = frontier,
				  weight = 2048,
				  sync = false,
				  reserve = false}]}],
			    (* flushing at far transfer *)
			    (farTransfer MemLocSet.empty
			     AppendList.empty
			     (AppendList.single
			      (* jmp *(stackTop - WORD_SIZE) *)
			      (x86.Assembly.instruction_jmp
			       {target = stackTopMinusWordDeref,
				absolute = true}))))
			 end
		       
		     datatype z = datatype Prim.Name.t
		   in
		     case Prim.name prim
		       of GC_collect => default "GC_gc"
			| MLton_halt => default "MLton_exit"
			| Thread_copy => default "GC_copyThread"
			| Thread_copyCurrent => default "GC_copyCurrentThread"
			| Thread_finishHandler => default "GC_finishHandler"
			| Thread_switchTo => thread ()
			| World_save => default "GC_saveWorld"
			| _ => Error.bug "x86GenerateTransfers::Runtime: prim"
		   end
		| CCall {target, args, return, dstsize}
		=> let
		     val {dead, ...}
		       = livenessTransfer {transfer = transfer,
					   liveInfo = liveInfo}

		     val c_stackP
		       = x86MLton.c_stackPContentsOperand
		     val c_stackPDerefDouble
		       = x86MLton.c_stackPDerefDoubleOperand
		     val applyFFTemp
		       = x86MLton.applyFFTempContentsOperand

		     val (assembly_args,size_args)
		       = List.fold
		         (args,(AppendList.empty,0),
			  fn ((arg,size),
			      (assembly_args,size_args)) 
			   => (AppendList.append
			       ((if Size.eq(size,Size.DBLE)
				   then AppendList.fromList
				        [Assembly.instruction_binal
					 {oper = Instruction.SUB,
					  dst = c_stackP,
					  src = Operand.immediate_const_int 8,
					  size = pointerSize},
					 Assembly.instruction_pfmov
					 {src = arg,
					  dst = c_stackPDerefDouble,
					  size = size}]
				   else if Size.eq(size,Size.BYTE)
					  then AppendList.fromList
					       [Assembly.instruction_movx
						{oper = Instruction.MOVZX,
						 dst = applyFFTemp,
						 src = arg,
						 dstsize = wordSize,
						 srcsize = size},
						Assembly.instruction_ppush
						{src = applyFFTemp,
						 base = c_stackP,
						 size = wordSize}]
					  else AppendList.single
					       (Assembly.instruction_ppush
						{src = arg,
						 base = c_stackP,
						 size = size})),
				assembly_args),
			       (Size.toBytes size) + size_args))
		   in
		     AppendList.appends
		     [AppendList.single
		      ((* explicit cache in case there are no args *)
		       Assembly.directive_cache 
		       {caches = [{register = Register.esp,
				   memloc = valOf (Operand.deMemloc c_stackP),
				   reserve = true}]}),
		      assembly_args,
		      AppendList.fromList
		      [(* flushing at Ccall *)
		       Assembly.directive_force
		       {commit_memlocs = MemLocSet.empty,
			commit_classes = ccallflushClasses,
			remove_memlocs = MemLocSet.empty,
			remove_classes = ClassSet.empty,
			dead_memlocs = dead,
			dead_classes = ClassSet.empty},
		       Assembly.directive_ccall (),
		       Assembly.instruction_call 
		       {target = Operand.label target,
			absolute = false},
		       Assembly.directive_force
		       {commit_memlocs = MemLocSet.empty,
			commit_classes = ClassSet.empty,
			remove_memlocs = MemLocSet.empty,
			remove_classes = ClassSet.empty,
			dead_memlocs = MemLocSet.empty,
			dead_classes = ccallflushClasses}],
		      (case dstsize
			 of NONE => AppendList.empty
			  | SOME dstsize
			  => (case Size.class dstsize
				of Size.INT
				 => AppendList.single
				    (Assembly.directive_return
				     {memloc = x86MLton.cReturnTempContents dstsize})
				 | Size.FLT
				 => AppendList.single
				    (Assembly.directive_fltreturn
				     {memloc = x86MLton.cReturnTempContents dstsize})
			         | _ => Error.bug "CCall")),
		      (if size_args > 0
			 then AppendList.single
			      (Assembly.instruction_binal
			       {oper = Instruction.ADD,
				dst = c_stackP,
				src = Operand.immediate_const_int size_args,
				size = pointerSize})
			 else AppendList.empty),
		      AppendList.single
		      (Assembly.directive_unreserve 
		       {registers = [Register.esp]}),
		      fall gef
		           {label = return,
			    live = getLive(liveInfo, return)}]
		   end)

        fun effectJumpTable (gef as GEF {generate,effect,fall})
	                     {label, transfer} : Assembly.t AppendList.t
	  = case transfer
	      of Switch {test, cases, default}
	       => let
		    val {dead, ...}
		      = livenessTransfer {transfer = transfer,
					  liveInfo = liveInfo}

		    fun reduce(cases,
			       ops as {zero,
				       even,
				       incFn, decFn, halfFn,
				       ltFn, gtFn,
				       min, minFn,
				       max, maxFn,
				       range})
		      = let
			  fun reduce' cases
			    = let
				val (minK,maxK,length,
				     allEven,allOdd)
				  = List.fold
				    (cases,
				     (max, min, 0,
				      true, true),
				     fn ((k,target),
					 (minK,maxK,length,
					  allEven,allOdd))
				      => let
					   val isEven = even k
					 in
					   (minFn(k,minK),
					    maxFn(k,maxK),
					    length + 1,
					    allEven andalso isEven,
					    allOdd andalso not isEven)
					 end)
			      in
				if length > 1 andalso
				   (allEven orelse allOdd)
				  then let
					 val f = if allOdd
						   then halfFn o decFn
						   else halfFn
					 val cases' 
					   = List.map
					     (cases,
					      fn (k,target)
					       => (f k, target))
					     
					 val (cases'', 
					      minK'', maxK'', length'',
					      shift'', mask'')
					   = reduce' cases'

					 val shift' = 1 + shift''
					 val mask' 
					   = Word.orb
					     (Word.<<(mask'', 0wx1),
					      if allOdd
						then 0wx1
						else 0wx0)
				       in
					 (cases'', 
					  minK'', maxK'', length'',
					  shift', mask')
				       end
				  else (cases, 
					minK, maxK, length,
					0, 0wx0)
			      end
			in 
			  reduce' cases
			end
		      
		    fun doitTable(cases,
				  ops as {zero,
					  even,
					  incFn, decFn, halfFn,
					  ltFn, gtFn,
					  min, minFn,
					  max, maxFn,
					  range},
				  minK, maxK, rangeK, shift, mask,
				  constFn)
		      = let
			  val jump_table_label
			    = Label.newString "jumpTable"

			  val idT = Directive.Id.new ()
			  val defaultT = pushCompensationBlock
			                 {label = default,
					  id = idT}

			  val rec filler 
			    = fn ([],_) => []
			       | (cases as (i,target)::cases',j)
			       => if i = j
				    then let
					   val target'
					     = pushCompensationBlock
					       {label = target,
						id = idT}
					 in 
					   (Immediate.label target')::
					   (filler(cases', incFn j))
					 end 
				    else (Immediate.label defaultT)::
				         (filler(cases, incFn j))

			  val jump_table = filler (cases, minK)

			  val default_live = getLive(liveInfo, default)
			  val live
			    = List.fold
			      (cases,
			       default_live,
			       fn ((i,target), live)
			        => MemLocSet.+(live, getLive(liveInfo, target)))

			  val indexTemp
			    = MemLoc.imm 
			      {base = Immediate.label (Label.fromString "indexTemp"),
			       index = Immediate.const_int 0,
			       scale = Scale.Four,
			       size = Size.LONG,
			       class = MemLoc.Class.Temp}
			  val checkTemp
			    = MemLoc.imm 
			      {base = Immediate.label (Label.fromString "checkTemp"),
			       index = Immediate.const_int 0,
			       scale = Scale.Four,
			       size = Size.LONG,
			       class = MemLoc.Class.Temp}
			  val address
			    = MemLoc.basic
			      {base = Immediate.label jump_table_label,
			       index = indexTemp,
			       scale = Scale.Four,
			       size = Size.LONG,
			       class = MemLoc.Class.Code}
			      
			  val size 
			    = case Operand.size test
				of SOME size => size
				 | NONE => Size.LONG
			  val indexTemp' = indexTemp
			  val indexTemp = Operand.memloc indexTemp
			  val checkTemp' = checkTemp
			  val checkTemp = Operand.memloc checkTemp
			  val address = Operand.memloc address
			in
			  AppendList.appends
			  [if Size.lt(size, Size.LONG)
			     then AppendList.single
			          (Assembly.instruction_movx
				   {oper = Instruction.MOVZX,
				    src = test,
				    srcsize = size,
				    dst = indexTemp,
				    dstsize = Size.LONG})
			     else AppendList.single
			          (Assembly.instruction_mov
				   {src = test,
				    dst = indexTemp,
				    size = Size.LONG}),
			   if MemLocSet.isEmpty dead
			     then AppendList.empty
			     else AppendList.single
			          (Assembly.directive_force
				   {commit_memlocs = MemLocSet.empty,
				    commit_classes = ClassSet.empty,
				    remove_memlocs = MemLocSet.empty,
				    remove_classes = ClassSet.empty,
				    dead_memlocs = dead,
				    dead_classes = ClassSet.empty}),
			   if shift > 0
			     then let
				    val idC = Directive.Id.new ()
				    val defaultC = pushCompensationBlock
				                   {label = default,
						    id = idC}
				    val _ = incNear(jumpInfo, default)
				  in
				    AppendList.appends
				    [AppendList.fromList
				     [Assembly.instruction_mov
				      {src = indexTemp,
				       dst = checkTemp,
				       size = Size.LONG},
				      Assembly.instruction_binal
				      {oper = Instruction.AND,
				       src = Operand.immediate_const_word 
				             (ones shift),
				       dst = checkTemp,
				       size = Size.LONG}],
				     if mask = 0wx0
				       then AppendList.empty
				       else AppendList.single
					    (Assembly.instruction_binal
					     {oper = Instruction.SUB,
					      src = Operand.immediate_const_word mask,
					      dst = checkTemp,
					      size = Size.LONG}),
				     AppendList.fromList
				     [Assembly.directive_force
				      {commit_memlocs = MemLocSet.empty,
				       commit_classes = nearflushClasses,
				       remove_memlocs = MemLocSet.empty,
				       remove_classes = ClassSet.empty,
				       dead_memlocs = MemLocSet.singleton checkTemp',
				       dead_classes = ClassSet.empty},
				      Assembly.directive_saveregalloc
				      {id = idC,
				       live = MemLocSet.add
				              (MemLocSet.add
					       (default_live,
						stackTop),
					       frontier)},
				      Assembly.instruction_jcc
				      {condition = Instruction.NZ,
				       target = Operand.label defaultC},
				      Assembly.instruction_sral
				      {oper = Instruction.SAR,
				       count = Operand.immediate_const_int shift,
				       dst = indexTemp,
				       size = Size.LONG}]]
				  end
			     else AppendList.empty,
			   if minK = zero
			     then AppendList.empty
			     else AppendList.single
			          (Assembly.instruction_binal
				   {oper = Instruction.SUB,
				    src = Operand.immediate (constFn minK),
				    dst = indexTemp,
				    size = Size.LONG}),
			  AppendList.fromList
			  [Assembly.directive_force
			   {commit_memlocs = MemLocSet.empty,
			    commit_classes = nearflushClasses,
			    remove_memlocs = MemLocSet.empty,
			    remove_classes = ClassSet.empty,
			    dead_memlocs = MemLocSet.empty,
			    dead_classes = ClassSet.empty},
			   Assembly.directive_cache
			   {caches = [{register = indexReg,
				       memloc = indexTemp',
				       reserve = false}]},
			   Assembly.instruction_cmp
			   {src1 = indexTemp,
			    src2 = Operand.immediate_const_int rangeK,
			    size = size},
			   Assembly.directive_saveregalloc
			   {id = idT,
			    live = MemLocSet.add
				   (MemLocSet.add
				    (live,
				     stackTop),
				    frontier)},
			   Assembly.instruction_jcc
			   {condition = Instruction.AE,
			    target = Operand.label defaultT},
			   Assembly.instruction_jmp
			   {target = address,
			    absolute = true},
			   Assembly.directive_force
			   {commit_memlocs = MemLocSet.empty,
			    commit_classes = ClassSet.empty,
			    remove_memlocs = MemLocSet.empty,
			    remove_classes = ClassSet.empty,
			    dead_memlocs = MemLocSet.singleton indexTemp',
			    dead_classes = ClassSet.empty}],
			  AppendList.fromList
			  [Assembly.pseudoop_data (),
			   Assembly.pseudoop_p2align 
			   (Immediate.const_int 4, NONE, NONE),
			   Assembly.label jump_table_label,
			   Assembly.pseudoop_long jump_table,
			   Assembly.pseudoop_text ()]]
			end

		    fun doit(cases,
			     ops as {zero,
				     even,
				     incFn, decFn, halfFn,
				     ltFn, gtFn,
				     min, minFn,
				     max, maxFn,
				     range},
			     constFn)
		      = let
			  val (cases, 
			       minK, maxK, length,
			       shift, mask) 
			    = reduce(cases, ops)
			    
			  val rangeK 
			    = SOME (range(minK,maxK))
			      handle Overflow
			       => NONE
			in
			  if length >= 8 
			     andalso
			     (isSome rangeK
			      andalso
			      valOf rangeK <= 2 * length)
			    then let
				   val rangeK = valOf rangeK

				   val cases 
				     = List.insertionSort
				       (cases, 
					fn ((k,target),(k',target')) 
					 => ltFn(k,k'))
				 in 
				   doitTable(cases, 
					     ops,
					     minK, maxK, rangeK,
					     shift, mask,
					     constFn)
				 end
			    else effectDefault gef
			                       {label = label, 
						transfer = transfer}
			end
		  in
		    case cases
		      of Transfer.Cases.Char cases
		       => doit
			  (cases,
			   {zero = #"\000",
			    even = fn c => (Char.ord c) mod 2 = 0,
			    incFn = Char.succ,
			    decFn = Char.pred,
			    halfFn = fn c => Char.chr((Char.ord c) div 2),
			    ltFn = Char.<,
			    gtFn = Char.>,
			    min = Char.minChar,
			    minFn = Char.min,
			    max = Char.maxChar,
			    maxFn = Char.max,
			    range = fn (min,max) => ((Char.ord max) - 
						     (Char.ord min)) + 1},
			   Immediate.const_char)
		       | Transfer.Cases.Int cases
		       => doit
			  (cases,
			   {zero = 0,
			    even = fn i => i mod 2 = 0,
			    incFn = fn i => i + 1,
			    decFn = fn i => i - 1,
			    halfFn = fn i => i div 2,
			    ltFn = Int.<,
			    gtFn = Int.>,
			    min = Int.minInt,
			    minFn = Int.min,
			    max = Int.maxInt,
			    maxFn = Int.max,
			    range = fn (min,max) => max - min + 1},
			   Immediate.const_int)
		       | Transfer.Cases.Word cases
		       => doit
			  (cases,
			   {zero = 0wx0,
			    even = fn w => Word.mod(w,0wx2) = 0wx0,
			    incFn = fn x => Word.+(x,0wx1),
			    decFn = fn x => Word.-(x,0wx1),
			    halfFn = fn x => Word.div(x,0wx2),
			    ltFn = Word.<,
			    gtFn = Word.>,
			    min = 0wx0,
			    minFn = Word.min,
			    max = 0wxFFFFFFFF,
			    maxFn = Word.max,
			    range = fn (min,max) => ((Word.toInt max) -
						     (Word.toInt min) + 
						     1)},
			   Immediate.const_word)
		  end
	       | _ => effectDefault gef 
		                    {label = label,
				     transfer = transfer}

	fun fallNone (gef as GEF {generate,effect,fall})
	             {label, live} : Assembly.t AppendList.t
	  = let
	      val liveRegsTransfer = getLiveRegsTransfers
		                     (liveTransfers, label)
	      val liveFltRegsTransfer = getLiveFltRegsTransfers
		                        (liveTransfers, label)

	      val live
		= List.fold
		  (liveRegsTransfer,
		   live,
		   fn ((memloc,_,_),live)
		    => MemLocSet.remove(live,memloc))
	      val live
		= List.fold
		  (liveFltRegsTransfer,
		   live,
		   fn ((memloc,_),live)
		    => MemLocSet.remove(live,memloc))

	      fun default ()
		= AppendList.fromList
		  ((* flushing at near transfer *)
		   (Assembly.directive_cache
		    {caches = [{register = stackTopReg,
				memloc = stackTop,
				reserve = true},
			       {register = frontierReg,
				memloc = frontier,
				reserve = true}]})::
		   (Assembly.directive_fltcache
		    {caches 
		     = List.map
		       (liveFltRegsTransfer,
			fn (memloc,sync) 
			 => {memloc = memloc})})::
		   (Assembly.directive_cache
		    {caches
		     = List.map
		       (liveRegsTransfer,
			fn (temp,register,sync)
			 => {register = register,
			     memloc = temp,
			     reserve = true})})::
		   (Assembly.directive_force
		    {commit_memlocs = live,
		     commit_classes = nearflushClasses,
		     remove_memlocs = MemLocSet.empty,
		     remove_classes = ClassSet.empty,
		     dead_memlocs = MemLocSet.empty,
		     dead_classes = ClassSet.empty})::
		   (Assembly.instruction_jmp
		    {target = Operand.label label,
		     absolute = false})::
		   (Assembly.directive_unreserve
		    {registers
		     = (stackTopReg)::
		       (frontierReg)::
		       (List.map
			(liveRegsTransfer,
			 fn (temp,register,sync)
			  => register))})::
		   nil)
	    in
	      case getLayoutInfo label
		of NONE
		 => default ()
		 | SOME (block as Block.T {entry,...})
		 => (push label;
		     default ())
	    end

	datatype z = datatype x86JumpInfo.status
	fun fallDefault (gef as GEF {generate,effect,fall})
	                {label, live} : Assembly.t AppendList.t
	  = let
	      val liveRegsTransfer = getLiveRegsTransfers
		                     (liveTransfers, label)
	      val liveFltRegsTransfer = getLiveFltRegsTransfers
		                        (liveTransfers, label)

	      val live
		= List.fold
		  (liveRegsTransfer,
		   live,
		   fn ((memloc,_,_),live)
		    => MemLocSet.remove(live,memloc))
	      val live
		= List.fold
		  (liveFltRegsTransfer,
		   live,
		   fn ((memloc,_),live)
		    => MemLocSet.remove(live,memloc))

	      fun default jmp
		= AppendList.appends
		  [AppendList.fromList
		   [(* flushing at near transfer *)
		    (Assembly.directive_cache
		     {caches = [{register = stackTopReg,
				 memloc = stackTop,
				 reserve = true},
				{register = frontierReg,
				 memloc = frontier,
				 reserve = true}]}),
		    (Assembly.directive_fltcache
		     {caches 
		      = List.map
		        (liveFltRegsTransfer,
			 fn (memloc,sync) 
			  => {memloc = memloc})}),
		    (Assembly.directive_cache
		     {caches
		      = List.map
		        (liveRegsTransfer,
			 fn (temp,register,sync)
			  => {register = register,
			      memloc = temp,
			      reserve = true})}),
		    (Assembly.directive_force
		     {commit_memlocs = live,
		      commit_classes = nearflushClasses,
		      remove_memlocs = MemLocSet.empty,
		      remove_classes = ClassSet.empty,
		      dead_memlocs = MemLocSet.empty,
		      dead_classes = ClassSet.empty})],
		   if jmp
		     then AppendList.single
		          (Assembly.instruction_jmp
			   {target = Operand.label label,
			    absolute = false})
		     else AppendList.empty,
		   AppendList.single
		   (Assembly.directive_unreserve
		    {registers
		     = (stackTopReg)::
		       (frontierReg)::
		       (List.map
			(liveRegsTransfer,
			 fn (temp,register,sync)
			  => register))})]
	    in
	      case getLayoutInfo label
		of NONE 
		 => default true
		 | SOME (block as Block.T {entry,...})
		 => (case getNear(jumpInfo, label)
		       of Count 1 
			=> generate gef
			            {label = label,
				     falling = true,
				     unique = true}
(*
			=> AppendList.cons
			   (Assembly.directive_force
			    {commit_memlocs = live,
			     commit_classes = ClassSet.empty,
			     remove_memlocs = MemLocSet.empty,
			     remove_classes = ClassSet.empty,
			     dead_memlocs = MemLocSet.empty,
			     dead_classes = ClassSet.empty},
			    generate gef
			             {label = label,
			              falling = true,
			              unique = true})
*)
		        | _ => AppendList.append
			       (default false,
				AppendList.cons
				(Assembly.directive_reset (),
				 (generate gef
				           {label = label,
					    falling = true,
					    unique = false}))))
	    end
	  
	fun make {generate, effect, fall}
	  = generate (GEF {generate = generate,
			   effect = effect,
			   fall = fall})

	val generate
	  = case optimize 
	      of 0 => make {generate = generateAll,
			    effect = effectDefault,
			    fall = fallNone}
	       | _ => make {generate = generateAll,
			    effect = effectJumpTable,
			    fall = fallDefault}

	val _ = List.foreach
	        (blocks,
		 fn Block.T {entry, ...}
		  => (case entry
			of Func {label, ...} => enque label
			 | _ => ()))
	fun doit () : Assembly.t list list
	  = (case deque ()
	       of NONE => []
	        | SOME label
		=> (case AppendList.toList (generate {label = label,
						      falling = false,
						      unique = false})
		      of [] => doit ()
		       | block => block::(doit ())))
	val assembly = doit ()
	val _ = destLayoutInfo ()
	val _ = destProfileInfo ()
      in
	data::assembly
      end

  val (generateTransfers, generateTransfers_msg)
    = tracerTop
      "generateTransfers"
      generateTransfers

  fun generateTransfers_totals ()
    = (generateTransfers_msg ();
       Control.indent ();
       x86Liveness.LiveInfo.verifyLiveInfo_msg ();
       x86JumpInfo.verifyJumpInfo_msg ();
       x86EntryTransfer.verifyEntryTransfer_msg ();
       x86LoopInfo.createLoopInfo_msg ();
       x86LiveTransfers.computeLiveTransfers_totals ();
       Control.unindent ())
end


functor x86GenerateTransfersNew(S: X86_GENERATE_TRANSFERS_STRUCTS): X86_GENERATE_TRANSFERS =
struct

  val print = ignore

  open S
  open x86
  open x86JumpInfo
  open x86LoopInfo
  open x86Liveness.LiveInfo
  open x86Liveness.Liveness

  val rec ones : int -> word
    = fn 0 => 0wx0
       | n => Word.orb(Word.<<(ones (n-1), 0wx1),0wx1)

  val tracer = x86.tracer
  val tracerTop = x86.tracerTop

  structure x86LiveTransfers 
    = x86LiveTransfers(structure x86 = x86
		       structure x86MLton = x86MLton
		       structure x86Liveness = x86Liveness
		       structure x86JumpInfo = x86JumpInfo
		       structure x86LoopInfo = x86LoopInfo)

  val pointerSize = x86MLton.pointerSize
  val wordSize = x86MLton.wordSize

  val transferRegs
    =
(*
      x86.Register.eax::
      x86.Register.al::
*)
      x86.Register.ebx::
      x86.Register.bl::
      x86.Register.ecx::
      x86.Register.cl::
      x86.Register.edx:: 
      x86.Register.dl::
      x86.Register.edi::
      x86.Register.esi::
(*
      x86.Register.esp::
      x86.Register.ebp::
*)
      nil

  val transferFltRegs : Int.t = 6

  val indexReg = x86.Register.eax

  val stackTopReg = Register.ebp
  val frontierReg = Register.esp
  val stackTop = x86MLton.gcState_stackTopContents
  val frontier = x86MLton.gcState_frontierContents

  datatype gef = GEF of {generate : gef -> 
			            {label : Label.t,
				     falling : bool,
				     unique : bool} -> 
				    Assembly.t AppendList.t,
			 effect : gef -> 
			          {label : Label.t,
				   transfer : Transfer.t} ->
				  Assembly.t AppendList.t,
			 fall : gef ->
			        {label : Label.t,
				 target : Label.t,
				 live : MemLocSet.t} ->
				Assembly.t AppendList.t}

  fun generateTransfers {chunk as Chunk.T {data, blocks, ...},
			 optimize: int,
			 liveInfo : x86Liveness.LiveInfo.t,
			 jumpInfo : x86JumpInfo.t}
    = let
	val allClasses = !x86MLton.Classes.allClasses
	val livenessClasses = !x86MLton.Classes.livenessClasses
	val livenessClasses = ClassSet.add(livenessClasses, 
					   x86MLton.Classes.StaticNonTemp)
	val nonlivenessClasses = ClassSet.-(allClasses, livenessClasses)
	val holdClasses = !x86MLton.Classes.holdClasses
	val nonholdClasses = ClassSet.-(allClasses, holdClasses)
	val farflushClasses = ClassSet.-(nonlivenessClasses, holdClasses)
	val nearflushClasses = ClassSet.-(nonlivenessClasses, holdClasses)
	val runtimeClasses = !x86MLton.Classes.runtimeClasses
	val nonruntimeClasses = ClassSet.-(allClasses, runtimeClasses)
	val threadflushClasses = ClassSet.-(runtimeClasses, holdClasses)
	val cstaticClasses = !x86MLton.Classes.cstaticClasses
	val heapClasses = !x86MLton.Classes.heapClasses
	val ccallflushClasses = ClassSet.+(cstaticClasses, heapClasses)
	  
	fun removeHoldMemLocs memlocs
	  = MemLocSet.subset
	    (memlocs, 
	     fn m => not (ClassSet.contains(holdClasses, MemLoc.class m)))

	fun runtimeEntry l
	  = AppendList.cons
	    (Assembly.directive_assume
	     {assumes
	      = [{register = stackTopReg,
		  memloc = stackTop,
		  weight = 1024,
		  sync = false,
		  reserve = false},
		 {register = frontierReg,
		  memloc = frontier,
		  weight = 2048,
		  sync = false,
		  reserve = false}]},
	     l)

	fun farEntry l
	  = AppendList.cons
	    (Assembly.directive_assume
	     {assumes
	      = [{register = stackTopReg,
		  memloc = stackTop,
		  weight = 1024,
		  sync = false,
		  reserve = false},
		 {register = frontierReg,
		  memloc = frontier,
		  weight = 2048,
		  sync = false,
		  reserve = false}]},
	     l)

	fun farTransfer live setup trans
	  = AppendList.appends
	    [AppendList.single
	     (Assembly.directive_force
	      {commit_memlocs = removeHoldMemLocs live,
	       commit_classes = ClassSet.empty,
	       remove_memlocs = MemLocSet.empty,
	       remove_classes = ClassSet.empty,
	       dead_memlocs = MemLocSet.empty,
	       dead_classes = ClassSet.empty}),
	     setup,
	     AppendList.fromList
	     [(Assembly.directive_cache
	       {caches = [{register = stackTopReg,
			   memloc = stackTop,
			   reserve = true},
			  {register = frontierReg,
			   memloc = frontier,
			   reserve = true}]}),
	      (Assembly.directive_clearflt ()),
	      (Assembly.directive_force
	       {commit_memlocs = MemLocSet.empty,
		commit_classes = farflushClasses,
		remove_memlocs = MemLocSet.empty,
		remove_classes = ClassSet.empty,
		dead_memlocs = MemLocSet.empty,
		dead_classes = ClassSet.empty})],
	     trans]
	    
	val _
	  = Assert.assert
	    ("verifyLiveInfo",
	     fn () => x86Liveness.LiveInfo.verifyLiveInfo {chunk = chunk,
							   liveInfo = liveInfo})
	val _
	  = Assert.assert
	    ("verifyJumpInfo", 
	     fn () => x86JumpInfo.verifyJumpInfo {chunk = chunk,
						  jumpInfo = jumpInfo})

	val _
	  = Assert.assert
	    ("verifyEntryTransfer", 
	     fn () => x86EntryTransfer.verifyEntryTransfer {chunk = chunk})

	local
	  val gotoInfo as {get: Label.t -> {block:Block.t},
			   set,
			   destroy}
	    = Property.destGetSetOnce
	      (Label.plist, Property.initRaise ("gotoInfo", Label.layout))

	  val labels
	    = List.fold
	      (blocks, [],
	       fn (block as Block.T {entry, ...}, labels)
	        => let
		     val label = Entry.label entry
		   in
		     set(label, {block = block}) ;
		     label::labels
		   end)
	      
	  fun loop labels
	    = let
		val (labels, b)
		  = List.fold
		    (labels, ([], false),
		     fn (label, (labels, b))
		      => case x86JumpInfo.getNear (jumpInfo, label)
			   of x86JumpInfo.Count 0 
			    => let
				 val {block as Block.T {transfer, ...}}
				   = get label
			       in
				 List.foreach 
				 (Transfer.nearTargets transfer,
				  fn label 
				   => x86JumpInfo.decNear (jumpInfo, label));
				 (labels, true)
			       end
			    | _ => (label::labels, b))
	      in
		if b
		  then loop labels
		  else List.map (labels, #block o get)
	      end
	  val blocks = loop labels
	    
	  val _ = destroy ()
	in
	  val chunk = Chunk.T {data = data, blocks = blocks}
	end

	val loopInfo
	  = x86LoopInfo.createLoopInfo {chunk = chunk, farLoops = true}
	val isLoopHeader
	  = fn label => isLoopHeader(loopInfo, label)
	                handle _ => false

	val liveTransfers
	  = x86LiveTransfers.computeLiveTransfers
	    {chunk = chunk,
	     transferRegs = transferRegs,
	     transferFltRegs = transferFltRegs,
	     liveInfo = liveInfo,
	     jumpInfo = jumpInfo,
	     loopInfo = loopInfo}
	    handle exn
	     => Error.bug ("x86LiveTransfers.computeLiveTransfers::" ^
			   (case exn
			      of Fail s => s
			       | _ => "?"))

	val getLiveRegsTransfers
	  = #1 o x86LiveTransfers.getLiveTransfers
	val getLiveFltRegsTransfers
	  = #2 o x86LiveTransfers.getLiveTransfers

	type li = {block: Block.t, 
		   layedOut: bool ref, 
		   allPreds: Label.t list ref,
		   preds: Label.t list ref}
	val labelInfo as {get = getLabelInfo : Label.t -> li,
			  set = setLabelInfo,
			  destroy = destLabelInfo}
	  = Property.destGetSetOnce
	    (Label.plist, Property.initRaise ("labelInfo", Label.layout))

	val isRare'
	  = fn li : li => let
			    val {block = Block.T {transfer, ...}, ...} = li
			    datatype z = datatype Prim.Name.t
			  in
			    case transfer
			      of Transfer.Runtime {prim, ...}
			       => (case Prim.name prim
				     of GC_collect => true
				      | MLton_halt => true
				      | _ => false)
			       | Transfer.Raise _ => true
			       | _ => false
			  end
	val isRare = isRare' o getLabelInfo
(*
	val isReady'
	  = fn li : li => let
			    val {preds, ...} = li
			  in
			    List.forall(!preds, ! o #layedOut o getLabelInfo)
			  end
	val isReady' 
	  = if !Control.Native.ready
	      then isReady'
	      else fn _ => true 
	val isReady = isReady' o getLabelInfo
*)

	val _ 
	  = (List.foreach
	     (blocks,
	      fn block as Block.T {entry, profileInfo,...}
	       => let
		    val label = Entry.label entry
		  in 
		    setLabelInfo(label, {block = block,
					 layedOut = ref false,
					 allPreds = ref [],
					 preds = ref []})
		  end) ;
	     List.foreach
	     (blocks, 
	      fn block as Block.T {entry, transfer, ...}
	       => let
		    val label = Entry.label entry
		  in
		    List.foreach
		    (Transfer.nearTargets transfer,
		     fn l => List.push (#allPreds(getLabelInfo l), label))
		  end) ;
	     List.foreach
	     (blocks,
	      fn block as Block.T {entry, ...}
	       => let
		    val label = Entry.label entry
		    val {allPreds, preds, ...} = getLabelInfo label
		    val loopLabels = getLoopLabels (loopInfo, label)
		  in
			    preds := List.removeAll
			             (!allPreds,
				      fn l => List.exists
				              (!(#allPreds(getLabelInfo l)), isRare)
					      orelse
					      List.contains
					      (loopLabels, label, Label.equals))
		  end))
(*
		    val loopTreeAt = getLoopTreeAt (loopInfo, label)
		  in
		    case loopTreeAt
		      of NONE 
		       => preds := List.removeAll
			           (!allPreds,
				    fn l => List.exists
				            (!(#allPreds(getLabelInfo l)), isRare))
		       | SOME {down, ...}
		       => let
			    val loopLabels
			      = List.concat (Tree.foldPre(down, [], (op ::)))
			  in
			    preds := List.removeAll
			             (!allPreds,
				      fn l => List.exists
				              (!(#allPreds(getLabelInfo l)), isRare)
					      orelse
					      List.contains
					      (loopLabels, label, Label.equals))
			  end
		  end))
*)

	local	
	  type u = {source: Label.t, target: Label.t}	
	  val stack : u list ref = ref []
	  val queue : u Queue.t ref = ref (Queue.empty ())
	in
	  fun enque (x as {source, target})
	    = let
		val li as {block as Block.T {entry, ...}, ...} = getLabelInfo target
	      in
		if isRare' li
		  then queue := Queue.enque(!queue, x)
		  else stack := x::(!stack)
	      end

	  fun deque () = (case (!stack)
			    of [] => (case Queue.deque(!queue)
					of NONE => NONE
					 | SOME(x, queue') => (queue := queue';
							       SOME (#target x)))
			     | x::stack' => (stack := stack';
					     SOME (#target x)))
	end

	fun getCompensationBlock {label, id}
	  = let
	      val label' = Label.new label

	      val {block as Block.T {profileInfo, ...}, allPreds, preds, ...} 
		= getLabelInfo label
	      val profileInfo
		= ProfileInfo.add
		  (profileInfo,
		   {profileLevel = 2,
		    profileName = Label.toString label'})

	      val live = getLive(liveInfo, label)
	      val block
		= Block.T {entry = Entry.jump {label = label'},
			   profileInfo = profileInfo,
			   statements 
			   = (Assembly.directive_restoreregalloc
			      {live = MemLocSet.add
			              (MemLocSet.add
				       (live, 
					stackTop),
			              frontier),
			       id = id})::
			     nil,
			   transfer = Transfer.goto {target = label}}
	    in
	      setLive(liveInfo, label', live);
	      incNear(jumpInfo, label');
	      Assert.assert("getCompensationBlock",
			    fn () => getNear(jumpInfo, label') = Count 1);
	      x86LiveTransfers.setLiveTransfersEmpty(liveTransfers, label');
	      setLabelInfo(label', {block = block, 
				    layedOut = ref false,
				    allPreds = ref (!allPreds),
				    preds = ref (!preds)});
	      allPreds := [label'];
	      preds := [label'];
	      label'
	    end

	datatype z = datatype Entry.t
	fun generateAll (gef as GEF {generate,effect,fall})
	                {label, falling, unique} : 
			Assembly.t AppendList.t
	  = (case getLabelInfo label
	       of {layedOut = ref true, ...} => AppendList.empty
	        | {block as Block.T {entry, profileInfo,
				     statements, transfer},
		   layedOut, ...}
		=> let
		     val _ = print (concat ["generating: ",
					    Label.toString label,
					    "\n"])
		     val _ = layedOut := true

		     val profile_assembly
		       = ProfileInfo.profile_assembly profileInfo
		     val profile_assembly
		       = AppendList.fromList profile_assembly

		     fun near label
		       = if falling
			   then if unique
				  then AppendList.appends
				       [AppendList.fromList
					(if isLoopHeader label
					   then [Assembly.pseudoop_p2align 
						 (Immediate.const_int 4,
						  NONE,
						  SOME (Immediate.const_int 7)),
						 Assembly.pseudoop_p2align 
						 (Immediate.const_int 3,
						  NONE,
						  SOME (Immediate.const_int 7)),
						 Assembly.label label]
					   else [Assembly.label label]),
					profile_assembly]
				  else AppendList.appends
				       [AppendList.fromList
					(if isLoopHeader label
					   then [Assembly.pseudoop_p2align 
						 (Immediate.const_int 4,
						  NONE,
						  SOME (Immediate.const_int 7)),
						 Assembly.pseudoop_p2align 
						 (Immediate.const_int 3,
						  NONE,
						  SOME (Immediate.const_int 7)),
						 Assembly.label label]
					   else [Assembly.label label]),
					AppendList.fromList
					[(* near entry & 
					  * live transfer assumptions *)
					 (Assembly.directive_assume
					  {assumes
					   = ({register = stackTopReg,
					       memloc = stackTop,
					       weight = 1024,
					       sync = false,
					       reserve = false})::
					   ({register = frontierReg,
					     memloc = frontier,
					     weight = 2048,
					     sync = false,
					     reserve = false})::
					   (List.map
					    (getLiveRegsTransfers
					     (liveTransfers, label),
					     fn (memloc,register,sync)
					      => {register = register,
						  memloc = memloc,
						  sync = sync, 
						  weight = 1024,
						  reserve = false}))}),
					 (Assembly.directive_fltassume
					  {assumes
					   = (List.map
					      (getLiveFltRegsTransfers
					       (liveTransfers, label),
					       fn (memloc,sync)
					        => {memloc = memloc,
						    sync = sync,
						    weight = 1024}))})],
					profile_assembly]
			   else AppendList.appends
			        [AppendList.fromList
				 (if isLoopHeader label
				    then [Assembly.pseudoop_p2align 
					  (Immediate.const_int 4,
					   NONE,
					   SOME (Immediate.const_int 7)),	
					  Assembly.pseudoop_p2align 
					  (Immediate.const_int 3,
					   NONE,
					   SOME (Immediate.const_int 7)),
					  Assembly.label label]
				    else [Assembly.pseudoop_p2align
					  (Immediate.const_int 4, 
					   NONE, 
					   NONE),
					  Assembly.label label]),
				 AppendList.fromList
				 [(* near entry & 
				   * live transfer assumptions *)
				  (Assembly.directive_assume
				   {assumes
				    = ({register = stackTopReg,
					memloc = stackTop,
					weight = 1024,
					sync = false,
					reserve = false})::
				    ({register = frontierReg,
				      memloc = frontier,
				      weight = 2048,
				      sync = false,
				      reserve = false})::
				    (List.map
				     (getLiveRegsTransfers
				      (liveTransfers, label),
				      fn (memloc,register,sync)
				       => {register = register,
					   memloc = memloc,
					   sync = sync, 
					   weight = 1024,
					   reserve = false}))}),
				  (Assembly.directive_fltassume
				   {assumes
				    = (List.map
				       (getLiveFltRegsTransfers
					(liveTransfers, label),
					fn (memloc,sync)
					 => {memloc = memloc,
					     sync = sync,
					     weight = 1024}))})],
				 profile_assembly]

		     val pre
		       = case entry
			   of Jump {label}
			    => near label
			    | CReturn {label, dst}
			    => AppendList.append
			       (near label,
				case dst
				  of NONE => AppendList.empty
				   | SOME (dst, dstsize)
				   => (case Size.class dstsize
					 of Size.INT
					  => AppendList.single
					     (x86.Assembly.instruction_mov
					      {dst = dst,
					       src = x86MLton.cReturnTempContentsOperand dstsize,
					       size = dstsize})
					  | Size.FLT
					  => AppendList.single
					     (x86.Assembly.instruction_pfmov
					      {dst = dst,
					       src = x86MLton.cReturnTempContentsOperand dstsize,
					       size = dstsize})
					  | _ => Error.bug "CReturn"))
			    | Func {label,...}
			    => AppendList.append
			       (AppendList.fromList
				[Assembly.pseudoop_p2align 
				 (Immediate.const_int 4, NONE, NONE),
				 Assembly.pseudoop_global label,
				 Assembly.label label],
				(* entry from far assumptions *)
				(farEntry profile_assembly))
			    | Cont {label, 
				    frameInfo as Entry.FrameInfo.T 
				                 {size,
						  frameLayoutsIndex},
				    ...}
			    => AppendList.append
			       (AppendList.fromList
				[Assembly.pseudoop_p2align
				 (Immediate.const_int 4, NONE, NONE),
				 Assembly.pseudoop_long
				 [Immediate.const_int frameLayoutsIndex],
				 Assembly.label label],
				(* entry from far assumptions *)
				(farEntry
				 (AppendList.snoc
				  (profile_assembly,
				   let
				     val stackTop 
				       = x86MLton.gcState_stackTopContentsOperand
				     val bytes 
				       = x86.Operand.immediate_const_int (~ size)
				   in
				     (* stackTop += bytes *)
				     x86.Assembly.instruction_binal 
				     {oper = x86.Instruction.ADD,
				      dst = stackTop,
				      src = bytes, 
				      size = pointerSize}
				   end))))
			    | Handler {label,
				       offset, 
				       ...}
			    => AppendList.append
			       (AppendList.fromList
				[Assembly.pseudoop_p2align 
				 (Immediate.const_int 4, NONE, NONE),
				 Assembly.label label],
				(* entry from far assumptions *)
				(farEntry
				 (AppendList.snoc
				  (profile_assembly,
				   let
				     val stackTop 
				       = x86MLton.gcState_stackTopContentsOperand
				     val bytes 
				       = x86.Operand.immediate_const_int (~ offset)
				   in
				     (* stackTop += bytes *)
				     x86.Assembly.instruction_binal 
				     {oper = x86.Instruction.ADD,
				      dst = stackTop,
				      src = bytes, 
				      size = pointerSize}
				   end))))
			    | Runtime {label,
				       frameInfo as Entry.FrameInfo.T 
				                    {size,
						     frameLayoutsIndex}}
			    => AppendList.append
			       (AppendList.fromList
				[Assembly.pseudoop_p2align 
				 (Immediate.const_int 4, NONE, NONE),
				 Assembly.pseudoop_long 
				 [Immediate.const_int frameLayoutsIndex],
				 Assembly.label label],
				(* entry from far assumptions *)
				(farEntry
				 (AppendList.snoc
				  (profile_assembly,
				   let
				     val stackTop 
				       = x86MLton.gcState_stackTopContentsOperand
				     val bytes 
				       = x86.Operand.immediate_const_int (~ size)
				   in
				     (* stackTop += bytes *)
				     x86.Assembly.instruction_binal 
				     {oper = x86.Instruction.ADD,
				      dst = stackTop,
				      src = bytes, 
				      size = pointerSize}
				   end))))
			       
		     val pre
		       = AppendList.appends
		         [if !Control.Native.commented > 1
			    then AppendList.single
			         (Assembly.comment (Entry.toString entry))
			    else AppendList.empty,
			  if !Control.Native.commented > 2
			    then AppendList.single
			         (Assembly.comment 
				  (MemLocSet.fold
				   (getLive(liveInfo, label),
				    "",
				    fn (memloc, s)
				     => concat [s, 
						MemLoc.toString memloc, 
						" "])))
			    else AppendList.empty,
			  pre]

		     val (statements,live)
		       = List.foldr
		         (statements,
			  ([], 
			   #liveIn (livenessTransfer {transfer = transfer, 
						      liveInfo = liveInfo})),
			  fn (assembly,(statements,live))
			   => let
				val live as {liveIn,dead, ...}
				  = livenessAssembly {assembly = assembly,
						      live = live}
			      in
				(if MemLocSet.isEmpty dead
				   then assembly::statements
				   else assembly::
				        (Assembly.directive_force
					 {commit_memlocs = MemLocSet.empty,
					  commit_classes = ClassSet.empty,
					  remove_memlocs = MemLocSet.empty,
					  remove_classes = ClassSet.empty,
					  dead_memlocs = dead,
					  dead_classes = ClassSet.empty})::
					statements,
				 liveIn)
			      end)

		     val statements = AppendList.fromList statements

		     val transfer = effect gef {label = label, 
						transfer = transfer}
		   in
		     AppendList.appends 
		     [pre,
		      statements,
		      transfer]
		   end)

	fun commit {label, live} : {pre: Assembly.t AppendList.t,
				    post: Assembly.t AppendList.t}
	  = let
	      val liveRegsTransfer = getLiveRegsTransfers
		                     (liveTransfers, label)
	      val liveFltRegsTransfer = getLiveFltRegsTransfers
		                        (liveTransfers, label)

	      val live
		= List.fold
		  (liveRegsTransfer,
		   live,
		   fn ((memloc,_,_),live)
		    => MemLocSet.remove(live,memloc))
	      val live
		= List.fold
		  (liveFltRegsTransfer,
		   live,
		   fn ((memloc,_),live)
		    => MemLocSet.remove(live,memloc))
	    in
	      {pre = AppendList.fromList
		     [(* flushing at near transfer *)
		      (Assembly.directive_cache
		       {caches = [{register = stackTopReg,
				   memloc = stackTop,
				   reserve = true},
				  {register = frontierReg,
				   memloc = frontier,
				   reserve = true}]}),
		      (Assembly.directive_fltcache
		       {caches 
			= List.map
		          (liveFltRegsTransfer,
			   fn (memloc,sync) 
			    => {memloc = memloc})}),
		      (Assembly.directive_cache
		       {caches
			= List.map
		          (liveRegsTransfer,
			   fn (temp,register,sync)
			    => {register = register,
				memloc = temp,
				reserve = true})}),
		      (Assembly.directive_force
		       {commit_memlocs = live,
			commit_classes = nearflushClasses,
			remove_memlocs = MemLocSet.empty,
			remove_classes = ClassSet.empty,
			dead_memlocs = MemLocSet.empty,
			dead_classes = ClassSet.empty})],
	       post = AppendList.single
	              (Assembly.directive_unreserve
		       {registers
			= (stackTopReg)::
			  (frontierReg)::
			  (List.map
			   (liveRegsTransfer,
			    fn (temp,register,sync)
			     => register))})}
	    end

	datatype z = datatype Transfer.t
	fun effectDefault (gef as GEF {generate,effect,fall})
	                  {label, transfer} : Assembly.t AppendList.t
	  = AppendList.append
	    (if !Control.Native.commented > 1
	       then AppendList.single
		    (Assembly.comment
		     (Transfer.toString transfer))
	       else AppendList.empty,
	     case transfer
	       of Goto {target}
		=> fall gef
		        {label = label,
			 target = target,
			 live = getLive(liveInfo, target)}
		| Iff {condition, truee, falsee}
		=> let
		     val condition_neg
		       = Instruction.condition_negate condition
			     
		     val truee_live
		       = getLive(liveInfo, truee)
		     val truee_live_length
		       = MemLocSet.size truee_live

		     val falsee_live
		       = getLive(liveInfo, falsee)
		     val falsee_live_length
		       = MemLocSet.size falsee_live

		     fun fall_truee' ()
		       = let
			   val id = Directive.Id.new ()
			   val falsee'
			     = getCompensationBlock {label = falsee,
						     id = id}
			   val _ = enque {source = label, target = falsee'}
			 in
			   AppendList.append
			   (AppendList.fromList
			    [Assembly.directive_force
			     {commit_memlocs = MemLocSet.empty,
			      commit_classes = nearflushClasses,
			      remove_memlocs = MemLocSet.empty,
			      remove_classes = ClassSet.empty,
			      dead_memlocs = MemLocSet.empty,
			      dead_classes = ClassSet.empty},
			     Assembly.directive_saveregalloc
			     {live = MemLocSet.add
			             (MemLocSet.add
				      (falsee_live,
				       stackTop),
				      frontier),
			      id = id},
			     Assembly.instruction_jcc
			     {condition = condition_neg,
			      target = Operand.label falsee'}],
			    (fall gef 
			          {label = label,
				   target = truee,
				   live = truee_live}))
			 end

		     fun fall_truee ()
		       = case getNear(jumpInfo, falsee)
			   of Count 1 => fall_truee' ()
			    | _ => let
				     val {pre, post} = commit {label = falsee,
							       live = falsee_live}
				     val _ = enque {source = label,
						    target = falsee}
				   in
				     AppendList.appends
				     [pre,
				      AppendList.single
				      (Assembly.instruction_jcc
				       {condition = condition_neg,
					target = Operand.label falsee}),
				      post,
				      (fall gef 
				            {label = label,
					     target = truee,
					     live = truee_live})]
				   end

		     fun fall_falsee' ()
		       = let
			   val id = Directive.Id.new ()
			   val truee' 
			     = getCompensationBlock {label = truee,
						     id = id}
			   val _ = enque {source = label, target = truee'}
			 in
			   AppendList.append
			   (AppendList.fromList
			    [Assembly.directive_force
			     {commit_memlocs = MemLocSet.empty,
			      commit_classes = nearflushClasses,
			      remove_memlocs = MemLocSet.empty,
			      remove_classes = ClassSet.empty,
			      dead_memlocs = MemLocSet.empty,
			      dead_classes = ClassSet.empty},
			     Assembly.directive_saveregalloc
			     {live = MemLocSet.add
			             (MemLocSet.add
				      (truee_live,
				       stackTop),
				      frontier),
			      id = id},
			     Assembly.instruction_jcc
			     {condition = condition,
			      target = Operand.label truee'}],
			    (fall gef 
			          {label = label,
				   target = falsee,
				   live = falsee_live}))
			 end

		     fun fall_falsee ()
		       = case getNear(jumpInfo, truee)
			   of Count 1 => fall_falsee' ()
			    | _ => let
				     val {pre, post} = commit {label = truee,
							       live = truee_live}
				     val _ = enque {source = label,
						    target = truee}
				   in
				     AppendList.appends
				     [pre,
				      AppendList.single
				      (Assembly.instruction_jcc
				       {condition = condition,
					target = Operand.label truee}),
				      post,
				      (fall gef 
				            {label = label,
					     target = falsee,
					     live = falsee_live})]
				   end
		   in
		     case (getLabelInfo truee,
			   getLabelInfo falsee)
		       of ({layedOut = ref true, ...},
			   {layedOut = ref false, ...}) => fall_falsee ()
			| ({layedOut = ref false, ...},
			   {layedOut = ref true, ...}) => fall_truee ()
			| (truee_li, falsee_li) 
			=> let
			     fun default'' ()
			       = if truee_live_length <= falsee_live_length
				   then fall_falsee ()
				   else fall_truee ()

			     fun default' ()
			       = case (getNear(jumpInfo, truee),
				       getNear(jumpInfo, falsee))
				   of (Count 1, Count 1) => default'' ()
				    | (Count 1, _) => fall_truee ()
				    | (_, Count 1) => fall_falsee ()
				    | _ => default'' ()

			     fun default ()
			       = case (isRare' truee_li, isRare' falsee_li)
				   of (true, false) => fall_falsee ()
				    | (false, true) => fall_truee ()
				    | _ => default' ()
			     val _ = print
			             (concat [Label.toString label,
					      " ",
					      Label.toString truee,
					      " ",
					      case getLoopDistance(loopInfo, 
								   label, 
								   truee)
						of NONE => "NONE"
						 | SOME i => Int.toString i,
					      "\n",
					      Label.toString label,
					      " ",
					      Label.toString falsee,
					      " ",
					      case getLoopDistance(loopInfo, 
								   label, 
								   falsee)
						of NONE => "NONE"
						 | SOME i => Int.toString i,
					      "\n"])
			   in 
			     case (getLoopDistance(loopInfo, label, truee),
				   getLoopDistance(loopInfo, label, falsee))
			       of (NONE, NONE) => default ()
				| (SOME _, NONE) => fall_truee ()
				| (NONE, SOME _) => fall_falsee ()
			        | (SOME dtruee, SOME dfalsee)
				=> (case Int.compare(dtruee, dfalsee)
				      of EQUAL => default ()
				       | LESS => fall_falsee ()
				       | MORE => fall_truee ())
			   end
		   end
		| Switch {test, cases, default}
		=> let
		     val {dead, ...}
		       = livenessTransfer {transfer = transfer,
					   liveInfo = liveInfo}

		     val size
		       = case Operand.size test
			   of SOME size => size
			    | NONE => Size.LONG

		     val default_live
		       = getLive(liveInfo, default)

		     val cases
		       = Transfer.Cases.map'
		         (cases,
			  fn (k, target)
			   => let
				val target_live
				  = getLive(liveInfo, target)
			      in
				case getNear(jumpInfo, target)
				  of Count 1 => let
						  val id = Directive.Id.new ()
						  val target' = getCompensationBlock 
						                {label = target,
								 id = id}
						  val _ = enque {source = label,
								 target = target'}
						in
						  AppendList.fromList
						  [Assembly.instruction_cmp
						   {src1 = test,
						    src2 = Operand.immediate_const k,
						    size = size},
						   Assembly.directive_saveregalloc
						   {live = MemLocSet.add
						    (MemLocSet.add
						     (target_live,
						      stackTop),
						     frontier),
						    id = id},
						   Assembly.instruction_jcc
						   {condition = Instruction.E,
						    target = Operand.label target'}]
						end
				   | _ => let
					    val {pre, post} = commit {label = target,
								      live = target_live}
					    val _ = enque {source = label,
							   target = target}
					  in
					    AppendList.appends
					    [AppendList.single
					     (Assembly.instruction_cmp
					      {src1 = test,
					       src2 = Operand.immediate_const k,
					       size = size}),
					     pre,
					     AppendList.single
					     (Assembly.instruction_jcc
					      {condition = Instruction.E,
					       target = Operand.label target}),
					     post]
					  end
			      end,
			  fn (c, target) => (Immediate.Char c, target),
			  fn (i, target) => (Immediate.Int i, target),
			  fn (w, target) => (Immediate.Word w, target))
		   in
		     AppendList.appends
		     [AppendList.single
		      (Assembly.directive_force
		       {commit_memlocs = MemLocSet.empty,
			commit_classes = nearflushClasses,
			remove_memlocs = MemLocSet.empty,
			remove_classes = ClassSet.empty,
			dead_memlocs = MemLocSet.empty,
			dead_classes = ClassSet.empty}),
		      AppendList.appends cases,
		      if MemLocSet.isEmpty dead
			then AppendList.empty
			else AppendList.single
			     (Assembly.directive_force
			      {commit_memlocs = MemLocSet.empty,
			       commit_classes = ClassSet.empty,
			       remove_memlocs = MemLocSet.empty,
			       remove_classes = ClassSet.empty,
			       dead_memlocs = dead,
			       dead_classes = ClassSet.empty}),
		      (fall gef
		            {label = label,
			     target = default,
			     live = default_live})]
		   end
                | Tail {target, live}
		=> (* flushing at far transfer *)
		   (farTransfer live
		    AppendList.empty
		    (AppendList.single
		     (Assembly.instruction_jmp
		      {target = Operand.label target,
		       absolute = false})))
		| NonTail {target, live, return, handler, size}
		=> let
		     val _ = enque {source = label,
				    target = return}
		     val _ = case handler
			       of SOME handler => enque {source = label,
							 target = handler}
				| NONE => ()

		     val stackTop 
		       = x86MLton.gcState_stackTopContentsOperand
		     val stackTopMinusWordDeref'
		       = x86MLton.gcState_stackTopMinusWordDeref
		     val stackTopMinusWordDeref
		       = x86MLton.gcState_stackTopMinusWordDerefOperand
		     val bytes 
		       = x86.Operand.immediate_const_int size

		     val liveReturn = x86Liveness.LiveInfo.getLive(liveInfo, return)
		     val liveHandler 
		       = case handler
			   of SOME handler
			    => x86Liveness.LiveInfo.getLive(liveInfo, handler)
			    | _ => MemLocSet.empty
		     val live = MemLocSet.unions [live,
						  liveReturn,
						  liveHandler]
		   in
		     (* flushing at far transfer *)
		     (farTransfer live
		      (AppendList.fromList
		       [(* stackTop += bytes *)
			x86.Assembly.instruction_binal 
			{oper = x86.Instruction.ADD,
			 dst = stackTop,
			 src = bytes, 
			 size = pointerSize},
			(* *(stackTop - WORD_SIZE) = return *)
			x86.Assembly.instruction_mov
			{dst = stackTopMinusWordDeref,
			 src = Operand.immediate_label return,
			 size = pointerSize},
			x86.Assembly.directive_force
			{commit_memlocs = MemLocSet.singleton stackTopMinusWordDeref',
			 commit_classes = ClassSet.empty,
			 remove_memlocs = MemLocSet.empty,
			 remove_classes = ClassSet.empty,
			 dead_memlocs = MemLocSet.empty,
			 dead_classes = ClassSet.empty}])
		      (AppendList.single
		       (Assembly.instruction_jmp
			{target = Operand.label target,
			 absolute = false})))
		   end
		| Return {live}
		=> let
		     val stackTopMinusWordDeref
		       = x86MLton.gcState_stackTopMinusWordDerefOperand
		   in
		     (* flushing at far transfer *)
		     (farTransfer live
		      AppendList.empty
		      (AppendList.single
		       (* jmp *(stackTop - WORD_SIZE) *)
		       (x86.Assembly.instruction_jmp
			{target = stackTopMinusWordDeref,
			 absolute = true})))
		   end
		| Raise {live}
		=> let
		     val exnStack 
		       = x86MLton.gcState_currentThread_exnStackContentsOperand
		     val stackTop 
		       = x86MLton.gcState_stackTopContentsOperand
		     val stackTopMinusWordDeref
		       = x86MLton.gcState_stackTopMinusWordDerefOperand
		     val stackBottom 
		       = x86MLton.gcState_stackBottomContentsOperand
		    in
		      (* flushing at far transfer *)
		      (farTransfer live
		       (AppendList.fromList
			[(* stackTop = stackBottom + exnStack *)
			 x86.Assembly.instruction_mov
			 {dst = stackTop,
			  src = stackBottom,
			  size = pointerSize},
			 x86.Assembly.instruction_binal
			 {oper = x86.Instruction.ADD,
			  dst = stackTop,
			  src = exnStack,
			  size = pointerSize}])
		       (AppendList.single
			(* jmp *(stackTop - WORD_SIZE) *)
			(x86.Assembly.instruction_jmp
			 {target = stackTopMinusWordDeref,
			  absolute = true})))
		    end
		| Runtime {prim, args, return, size}
		=> let
		     val _ = enque {source = label, 
				    target = return}
		     
		     val {dead, ...}
		       = livenessTransfer {transfer = transfer,
					   liveInfo = liveInfo}

		     val stackTop'
		       = x86MLton.gcState_stackTopContents
		     val stackTop 
		       = x86MLton.gcState_stackTopContentsOperand
		     val bytes 
		       = x86.Operand.immediate_const_int size
		     val stackTopMinusWordDeref
		       = x86MLton.gcState_stackTopMinusWordDerefOperand

		     val live = x86Liveness.LiveInfo.getLive(liveInfo, return)

		     fun default f
		       = let
			   val target = Label.fromString f

			   val c_stackP
			     = x86MLton.c_stackPContentsOperand
			   val c_stackPDerefDouble
			     = x86MLton.c_stackPDerefDoubleOperand
			   val applyFFTemp
			     = x86MLton.applyFFTempContentsOperand

			   val (assembly_args,size_args)
			     = List.fold
			       (args,(AppendList.empty,0),
				fn ((arg,size),
				    (assembly_args,size_args)) 
				 => (AppendList.append
				     (if Size.eq(size,Size.DBLE)
					then AppendList.fromList
					     [Assembly.instruction_binal
					      {oper = Instruction.SUB,
					       dst = c_stackP,
					       src = Operand.immediate_const_int 8,
					       size = pointerSize},
					      Assembly.instruction_pfmov
					      {src = arg,
					       dst = c_stackPDerefDouble,
					       size = size}]
					else if Size.eq(size,Size.BYTE)
					       then AppendList.fromList
						    [Assembly.instruction_movx
						     {oper = Instruction.MOVZX,
						      dst = applyFFTemp,
						      src = arg,
						      dstsize = wordSize,
						      srcsize = size},
						     Assembly.instruction_ppush
						     {src = applyFFTemp,
						      base = c_stackP,
						      size = wordSize}]
					       else AppendList.single
						    (Assembly.instruction_ppush
						     {src = arg,
						      base = c_stackP,
						      size = size}),
						    assembly_args),
				     (Size.toBytes size) + size_args))
			 in
			   AppendList.appends
			   [AppendList.single
			    ((* explicit cache in case there are no args *)
			     Assembly.directive_cache 
			     {caches = [{register = Register.esp,
					 memloc = valOf (Operand.deMemloc c_stackP),
					 reserve = true}]}),
			    assembly_args,
			    AppendList.fromList
			    [x86.Assembly.directive_force
			     {commit_memlocs = MemLocSet.empty,
			      commit_classes = ClassSet.empty,
			      remove_memlocs = MemLocSet.empty,
			      remove_classes = ClassSet.empty,
			      dead_memlocs = dead,
			      dead_classes = ClassSet.empty},
			     (* stackTop += bytes *)
			     x86.Assembly.instruction_binal 
			     {oper = x86.Instruction.ADD,
			      dst = stackTop,
			      src = bytes, 
			      size = pointerSize},
			     (* *(stackTop - WORD_SIZE) = return *)
			     x86.Assembly.instruction_mov
			     {dst = stackTopMinusWordDeref,
			      src = Operand.immediate_label return,
			      size = pointerSize},
			     (* flushing at Runtime *)
			     Assembly.directive_force
			     {commit_memlocs = live,
			      commit_classes = runtimeClasses,
			      remove_memlocs = MemLocSet.empty,
			      remove_classes = ClassSet.empty,
			      dead_memlocs = MemLocSet.empty,
			      dead_classes = ClassSet.empty},
			     Assembly.directive_ccall (),
			     Assembly.instruction_call 
			     {target = Operand.label target,
			      absolute = false},
			     Assembly.directive_force
			     {commit_memlocs = MemLocSet.empty,
			      commit_classes = ClassSet.empty,
			      remove_memlocs = MemLocSet.empty,
			      remove_classes = ClassSet.empty,
			      dead_memlocs = MemLocSet.empty,
			      dead_classes = runtimeClasses}],
			    (if size_args > 0
			       then AppendList.single
				    (Assembly.instruction_binal
				     {oper = Instruction.ADD,
				      dst = c_stackP,
				      src = Operand.immediate_const_int size_args,
				      size = pointerSize})
			       else AppendList.empty),
			    AppendList.single
			    (Assembly.directive_unreserve 
			     {registers = [Register.esp]}),
			    (* flushing at far transfer *)
			    (farTransfer MemLocSet.empty
			     AppendList.empty
			     (AppendList.single
			      (* jmp *(stackTop - WORD_SIZE) *)
			      (x86.Assembly.instruction_jmp
			       {target = stackTopMinusWordDeref,
				absolute = true})))]
			 end

		     fun thread ()
		       = let
			   val (thread,threadsize)
			     = case args
				 of [_, (thread,threadsize)] => (thread,threadsize)
				  | _ => Error.bug "x86GenerateTransfers::Runtime: args"

			   val threadTemp
			     = x86MLton.threadTempContentsOperand

			   val currentThread
			     = x86MLton.gcState_currentThreadContentsOperand	
			   val stack
			     = x86MLton.gcState_currentThread_stackContentsOperand
			   val stack_used
			     = x86MLton.gcState_currentThread_stack_usedContentsOperand
			   val stack_reserved
			     = x86MLton.gcState_currentThread_stack_reservedContentsOperand
			   val stackBottom
			     = x86MLton.gcState_stackBottomContentsOperand
			   val stackLimit
			     = x86MLton.gcState_stackLimitContentsOperand
			   val maxFrameSize
			     = x86MLton.gcState_maxFrameSizeContentsOperand
			   val canHandle
			     = x86MLton.gcState_canHandleContentsOperand
			   val signalIsPending
			     = x86MLton.gcState_signalIsPendingContentsOperand
			   val limit
			     = x86MLton.gcState_limitContentsOperand
			   val base
			     = x86MLton.gcState_baseContentsOperand

			   val resetJoin = Label.newString "resetJoin"
			 in
			   AppendList.append
			   (AppendList.fromList
			    [(* threadTemp = thread *)
			     Assembly.instruction_mov
			     {dst = threadTemp,
			      src = thread,
			      size = pointerSize},
			     (* stackTop += bytes *)
			     x86.Assembly.instruction_binal 
			     {oper = x86.Instruction.ADD,
			      dst = stackTop,
			      src = bytes, 
			      size = pointerSize},
			     (* *(stackTop - WORD_SIZE) = return *)
			     x86.Assembly.instruction_mov
			     {dst = stackTopMinusWordDeref,
			      src = Operand.immediate_label return,
			      size = pointerSize},
			     (* flushing at Runtime *)
			     Assembly.directive_force
			     {commit_memlocs = live,
			      commit_classes = threadflushClasses,
			      remove_memlocs = MemLocSet.empty,
			      remove_classes = ClassSet.empty,
			      dead_memlocs = MemLocSet.empty,
			      dead_classes = ClassSet.empty},
			     Assembly.directive_force
			     {commit_memlocs = MemLocSet.empty,
			      commit_classes = ClassSet.empty,
			      remove_memlocs = MemLocSet.empty,
			      remove_classes = ClassSet.empty,
			      dead_memlocs = MemLocSet.empty,
			      dead_classes = threadflushClasses},
			     (* currentThread->stack->used
			      *   = stackTop + wordSize - stackBottom
			      *)
			     Assembly.instruction_mov
			     {dst = stack_used,
			      src = stackTop,
			      size = pointerSize},
			     Assembly.instruction_binal
			     {oper = Instruction.ADD,
			      dst = stack_used,
			      src = Operand.immediate_const_int x86MLton.wordBytes,
			      size = pointerSize},
			     Assembly.instruction_binal
			     {oper = Instruction.SUB,
			      dst = stack_used,
			      src = stackBottom,
			      size = pointerSize},
			     (* currentThread = threadTemp *)
			     Assembly.instruction_mov
			     {src = threadTemp,
			      dst = currentThread,
			      size = pointerSize},
			     (* stackBottom = currentThread->stack + 8 *)
			     Assembly.instruction_mov
			     {dst = stackBottom,
			      src = stack,
			      size = pointerSize},
			     Assembly.instruction_binal
			     {oper = Instruction.ADD,
			      dst = stackBottom,
			      src = Operand.immediate_const_int 8,
			      size = pointerSize},
			     (* stackTop
			      *   = stackBottom + currentThread->stack->used - wordSize
			      *)
			     Assembly.instruction_mov
			     {dst = stackTop,
			      src = stackBottom,
			      size = pointerSize},
			     Assembly.instruction_binal
			     {oper = Instruction.ADD,
			      dst = stackTop,
			      src = stack_used,
			      size = pointerSize},
			     Assembly.instruction_binal
			     {oper = Instruction.SUB,
			      dst = stackTop,
			      src = Operand.immediate_const_int x86MLton.wordBytes,
			      size = pointerSize},
			     (* stackLimit
			      *   = stackBottom + currentThread->stack->reserved
			      *                 - 2 * maxFrameSize
			      *)
			     Assembly.instruction_mov
			     {dst = stackLimit,
			      src = stackBottom,
			      size = pointerSize},
			     Assembly.instruction_binal
			     {oper = Instruction.ADD,
			      dst = stackLimit,
			      src = stack_reserved,
			      size = pointerSize},
			     Assembly.instruction_binal
			     {oper = Instruction.SUB,
			      dst = stackLimit,
			      src = maxFrameSize,
			      size = pointerSize},
			     Assembly.instruction_binal
			     {oper = Instruction.SUB,
			      dst = stackLimit,
			      src = maxFrameSize,
			      size = pointerSize},
			     (* gcState.canHandle-- *)
			     Assembly.instruction_unal
			     {oper = Instruction.DEC,
			      dst = canHandle,
			      size = wordSize},
			     (* if (0 == canHandle && signalIsPending)
			      *   limit = 0
			      *)
			     Assembly.directive_cache
			     {caches = [{register = stackTopReg,
					 memloc = stackTop',
					 reserve = true},
					{register = frontierReg,
					 memloc = frontier,
					 reserve = true}]},
			     Assembly.directive_force
			     {commit_memlocs = MemLocSet.empty,
			      commit_classes = farflushClasses,
			      remove_memlocs = MemLocSet.empty,
			      remove_classes = ClassSet.empty,
			      dead_memlocs = MemLocSet.empty,
			      dead_classes = ClassSet.empty},
			     Assembly.instruction_jcc
			     {condition = Instruction.NZ,
			      target = Operand.label resetJoin},
			     Assembly.instruction_cmp
			     {src1 = signalIsPending,
			      src2 = Operand.immediate_const_int 0,
			      size = wordSize},
			     Assembly.directive_cache
			     {caches = [{register = stackTopReg,
					 memloc = stackTop',
					 reserve = true},
					{register = frontierReg,
					 memloc = frontier,
					 reserve = true}]},
			     Assembly.directive_force
			     {commit_memlocs = MemLocSet.empty,
			      commit_classes = farflushClasses,
			      remove_memlocs = MemLocSet.empty,
			      remove_classes = ClassSet.empty,
			      dead_memlocs = MemLocSet.empty,
			      dead_classes = ClassSet.empty},
			     Assembly.instruction_jcc
			     {condition = Instruction.E,
			      target = Operand.label resetJoin},
			     Assembly.instruction_mov
			     {dst = limit,
			      src = Operand.immediate_const_int 0,
			      size = pointerSize},
			     Assembly.directive_cache
			     {caches = [{register = stackTopReg,
					 memloc = stackTop',
					 reserve = true},
					{register = frontierReg,
					 memloc = frontier,
					 reserve = true}]},
			     Assembly.directive_force
			     {commit_memlocs = MemLocSet.empty,
			      commit_classes = farflushClasses,
			      remove_memlocs = MemLocSet.empty,
			      remove_classes = ClassSet.empty,
			      dead_memlocs = MemLocSet.empty,
			      dead_classes = ClassSet.empty},
			     Assembly.directive_reset (),
			     Assembly.label resetJoin,
			     Assembly.directive_assume
			     {assumes
			      = [{register = stackTopReg,
				  memloc = stackTop',
				  weight = 1024,
				  sync = false,
				  reserve = false},
				 {register = frontierReg,
				  memloc = frontier,
				  weight = 2048,
				  sync = false,
				  reserve = false}]}],
			    (* flushing at far transfer *)
			    (farTransfer MemLocSet.empty
			     AppendList.empty
			     (AppendList.single
			      (* jmp *(stackTop - WORD_SIZE) *)
			      (x86.Assembly.instruction_jmp
			       {target = stackTopMinusWordDeref,
				absolute = true}))))
			 end
		       
		     datatype z = datatype Prim.Name.t
		   in
		     case Prim.name prim
		       of GC_collect => default "GC_gc"
			| MLton_halt => default "MLton_exit"
			| Thread_copy => default "GC_copyThread"
			| Thread_copyCurrent => default "GC_copyCurrentThread"
			| Thread_finishHandler => default "GC_finishHandler"
			| Thread_switchTo => thread ()
			| World_save => default "GC_saveWorld"
			| _ => Error.bug "x86GenerateTransfers::Runtime: prim"
		   end
		| CCall {target, args, return, dstsize}
		=> let
		     val {dead, ...}
		       = livenessTransfer {transfer = transfer,
					   liveInfo = liveInfo}

		     val c_stackP
		       = x86MLton.c_stackPContentsOperand
		     val c_stackPDerefDouble
		       = x86MLton.c_stackPDerefDoubleOperand
		     val applyFFTemp
		       = x86MLton.applyFFTempContentsOperand

		     val (assembly_args,size_args)
		       = List.fold
		         (args,(AppendList.empty,0),
			  fn ((arg,size),
			      (assembly_args,size_args)) 
			   => (AppendList.append
			       ((if Size.eq(size,Size.DBLE)
				   then AppendList.fromList
				        [Assembly.instruction_binal
					 {oper = Instruction.SUB,
					  dst = c_stackP,
					  src = Operand.immediate_const_int 8,
					  size = pointerSize},
					 Assembly.instruction_pfmov
					 {src = arg,
					  dst = c_stackPDerefDouble,
					  size = size}]
				   else if Size.eq(size,Size.BYTE)
					  then AppendList.fromList
					       [Assembly.instruction_movx
						{oper = Instruction.MOVZX,
						 dst = applyFFTemp,
						 src = arg,
						 dstsize = wordSize,
						 srcsize = size},
						Assembly.instruction_ppush
						{src = applyFFTemp,
						 base = c_stackP,
						 size = wordSize}]
					  else AppendList.single
					       (Assembly.instruction_ppush
						{src = arg,
						 base = c_stackP,
						 size = size})),
				assembly_args),
			       (Size.toBytes size) + size_args))
		   in
		     AppendList.appends
		     [AppendList.single
		      ((* explicit cache in case there are no args *)
		       Assembly.directive_cache 
		       {caches = [{register = Register.esp,
				   memloc = valOf (Operand.deMemloc c_stackP),
				   reserve = true}]}),
		      assembly_args,
		      AppendList.fromList
		      [(* flushing at Ccall *)
		       Assembly.directive_force
		       {commit_memlocs = MemLocSet.empty,
			commit_classes = ccallflushClasses,
			remove_memlocs = MemLocSet.empty,
			remove_classes = ClassSet.empty,
			dead_memlocs = dead,
			dead_classes = ClassSet.empty},
		       Assembly.directive_ccall (),
		       Assembly.instruction_call 
		       {target = Operand.label target,
			absolute = false},
		       Assembly.directive_force
		       {commit_memlocs = MemLocSet.empty,
			commit_classes = ClassSet.empty,
			remove_memlocs = MemLocSet.empty,
			remove_classes = ClassSet.empty,
			dead_memlocs = MemLocSet.empty,
			dead_classes = ccallflushClasses}],
		      (case dstsize
			 of NONE => AppendList.empty
			  | SOME dstsize
			  => (case Size.class dstsize
				of Size.INT
				 => AppendList.single
				    (Assembly.directive_return
				     {memloc = x86MLton.cReturnTempContents dstsize})
				 | Size.FLT
				 => AppendList.single
				    (Assembly.directive_fltreturn
				     {memloc = x86MLton.cReturnTempContents dstsize})
			         | _ => Error.bug "CCall")),
		      (if size_args > 0
			 then AppendList.single
			      (Assembly.instruction_binal
			       {oper = Instruction.ADD,
				dst = c_stackP,
				src = Operand.immediate_const_int size_args,
				size = pointerSize})
			 else AppendList.empty),
		      AppendList.single
		      (Assembly.directive_unreserve 
		       {registers = [Register.esp]}),
		      fall gef
		           {label = label,
			    target = return,
			    live = getLive(liveInfo, return)}]
		   end)

        fun effectJumpTable (gef as GEF {generate,effect,fall})
	                     {label, transfer} : Assembly.t AppendList.t
	  = case transfer
	      of Switch {test, cases, default}
	       => let
		    val {dead, ...}
		      = livenessTransfer {transfer = transfer,
					  liveInfo = liveInfo}

		    fun reduce(cases,
			       ops as {zero,
				       even,
				       incFn, decFn, halfFn,
				       ltFn, gtFn,
				       min, minFn,
				       max, maxFn,
				       range})
		      = let
			  fun reduce' cases
			    = let
				val (minK,maxK,length,
				     allEven,allOdd)
				  = List.fold
				    (cases,
				     (max, min, 0,
				      true, true),
				     fn ((k,target),
					 (minK,maxK,length,
					  allEven,allOdd))
				      => let
					   val isEven = even k
					 in
					   (minFn(k,minK),
					    maxFn(k,maxK),
					    length + 1,
					    allEven andalso isEven,
					    allOdd andalso not isEven)
					 end)
			      in
				if length > 1 andalso
				   (allEven orelse allOdd)
				  then let
					 val f = if allOdd
						   then halfFn o decFn
						   else halfFn
					 val cases' 
					   = List.map
					     (cases,
					      fn (k,target)
					       => (f k, target))
					     
					 val (cases'', 
					      minK'', maxK'', length'',
					      shift'', mask'')
					   = reduce' cases'

					 val shift' = 1 + shift''
					 val mask' 
					   = Word.orb
					     (Word.<<(mask'', 0wx1),
					      if allOdd
						then 0wx1
						else 0wx0)
				       in
					 (cases'', 
					  minK'', maxK'', length'',
					  shift', mask')
				       end
				  else (cases, 
					minK, maxK, length,
					0, 0wx0)
			      end
			in 
			  reduce' cases
			end
		      
		    fun doitTable(cases,
				  ops as {zero,
					  even,
					  incFn, decFn, halfFn,
					  ltFn, gtFn,
					  min, minFn,
					  max, maxFn,
					  range},
				  minK, maxK, rangeK, shift, mask,
				  constFn)
		      = let
			  val jump_table_label
			    = Label.newString "jumpTable"

			  val idT = Directive.Id.new ()
			  val defaultT = getCompensationBlock
			                 {label = default,
					  id = idT}
			  val _ = enque {source = label,
					 target = defaultT}

			  val rec filler 
			    = fn ([],_) => []
			       | (cases as (i,target)::cases',j)
			       => if i = j
				    then let
					   val target'
					     = getCompensationBlock
					       {label = target,
						id = idT}
					   val _ = enque {source = label,
							  target = target'}
					 in 
					   (Immediate.label target')::
					   (filler(cases', incFn j))
					 end 
				    else (Immediate.label defaultT)::
				         (filler(cases, incFn j))

			  val jump_table = filler (cases, minK)

			  val default_live = getLive(liveInfo, default)
			  val live
			    = List.fold
			      (cases,
			       default_live,
			       fn ((i,target), live)
			        => MemLocSet.+(live, getLive(liveInfo, target)))

			  val indexTemp
			    = MemLoc.imm 
			      {base = Immediate.label (Label.fromString "indexTemp"),
			       index = Immediate.const_int 0,
			       scale = Scale.Four,
			       size = Size.LONG,
			       class = MemLoc.Class.Temp}
			  val checkTemp
			    = MemLoc.imm 
			      {base = Immediate.label (Label.fromString "checkTemp"),
			       index = Immediate.const_int 0,
			       scale = Scale.Four,
			       size = Size.LONG,
			       class = MemLoc.Class.Temp}
			  val address
			    = MemLoc.basic
			      {base = Immediate.label jump_table_label,
			       index = indexTemp,
			       scale = Scale.Four,
			       size = Size.LONG,
			       class = MemLoc.Class.Code}
			      
			  val size 
			    = case Operand.size test
				of SOME size => size
				 | NONE => Size.LONG
			  val indexTemp' = indexTemp
			  val indexTemp = Operand.memloc indexTemp
			  val checkTemp' = checkTemp
			  val checkTemp = Operand.memloc checkTemp
			  val address = Operand.memloc address
			in
			  AppendList.appends
			  [if Size.lt(size, Size.LONG)
			     then AppendList.single
			          (Assembly.instruction_movx
				   {oper = Instruction.MOVZX,
				    src = test,
				    srcsize = size,
				    dst = indexTemp,
				    dstsize = Size.LONG})
			     else AppendList.single
			          (Assembly.instruction_mov
				   {src = test,
				    dst = indexTemp,
				    size = Size.LONG}),
			   if MemLocSet.isEmpty dead
			     then AppendList.empty
			     else AppendList.single
			          (Assembly.directive_force
				   {commit_memlocs = MemLocSet.empty,
				    commit_classes = ClassSet.empty,
				    remove_memlocs = MemLocSet.empty,
				    remove_classes = ClassSet.empty,
				    dead_memlocs = dead,
				    dead_classes = ClassSet.empty}),
			   if shift > 0
			     then let
				    val idC = Directive.Id.new ()
				    val defaultC = getCompensationBlock
				                   {label = default,
						    id = idC}
				    val _ = enque {source = label,
						   target = defaultC}
				    val _ = incNear(jumpInfo, default)
				  in
				    AppendList.appends
				    [AppendList.fromList
				     [Assembly.instruction_mov
				      {src = indexTemp,
				       dst = checkTemp,
				       size = Size.LONG},
				      Assembly.instruction_binal
				      {oper = Instruction.AND,
				       src = Operand.immediate_const_word 
				             (ones shift),
				       dst = checkTemp,
				       size = Size.LONG}],
				     if mask = 0wx0
				       then AppendList.empty
				       else AppendList.single
					    (Assembly.instruction_binal
					     {oper = Instruction.SUB,
					      src = Operand.immediate_const_word mask,
					      dst = checkTemp,
					      size = Size.LONG}),
				     AppendList.fromList
				     [Assembly.directive_force
				      {commit_memlocs = MemLocSet.empty,
				       commit_classes = nearflushClasses,
				       remove_memlocs = MemLocSet.empty,
				       remove_classes = ClassSet.empty,
				       dead_memlocs = MemLocSet.singleton checkTemp',
				       dead_classes = ClassSet.empty},
				      Assembly.directive_saveregalloc
				      {id = idC,
				       live = MemLocSet.add
				              (MemLocSet.add
					       (default_live,
						stackTop),
					       frontier)},
				      Assembly.instruction_jcc
				      {condition = Instruction.NZ,
				       target = Operand.label defaultC},
				      Assembly.instruction_sral
				      {oper = Instruction.SAR,
				       count = Operand.immediate_const_int shift,
				       dst = indexTemp,
				       size = Size.LONG}]]
				  end
			     else AppendList.empty,
			   if minK = zero
			     then AppendList.empty
			     else AppendList.single
			          (Assembly.instruction_binal
				   {oper = Instruction.SUB,
				    src = Operand.immediate (constFn minK),
				    dst = indexTemp,
				    size = Size.LONG}),
			  AppendList.fromList
			  [Assembly.directive_force
			   {commit_memlocs = MemLocSet.empty,
			    commit_classes = nearflushClasses,
			    remove_memlocs = MemLocSet.empty,
			    remove_classes = ClassSet.empty,
			    dead_memlocs = MemLocSet.empty,
			    dead_classes = ClassSet.empty},
			   Assembly.directive_cache
			   {caches = [{register = indexReg,
				       memloc = indexTemp',
				       reserve = false}]},
			   Assembly.instruction_cmp
			   {src1 = indexTemp,
			    src2 = Operand.immediate_const_int rangeK,
			    size = size},
			   Assembly.directive_saveregalloc
			   {id = idT,
			    live = MemLocSet.add
				   (MemLocSet.add
				    (live,
				     stackTop),
				    frontier)},
			   Assembly.instruction_jcc
			   {condition = Instruction.AE,
			    target = Operand.label defaultT},
			   Assembly.instruction_jmp
			   {target = address,
			    absolute = true},
			   Assembly.directive_force
			   {commit_memlocs = MemLocSet.empty,
			    commit_classes = ClassSet.empty,
			    remove_memlocs = MemLocSet.empty,
			    remove_classes = ClassSet.empty,
			    dead_memlocs = MemLocSet.singleton indexTemp',
			    dead_classes = ClassSet.empty}],
			  AppendList.fromList
			  [Assembly.pseudoop_data (),
			   Assembly.pseudoop_p2align 
			   (Immediate.const_int 4, NONE, NONE),
			   Assembly.label jump_table_label,
			   Assembly.pseudoop_long jump_table,
			   Assembly.pseudoop_text ()]]
			end

		    fun doit(cases,
			     ops as {zero,
				     even,
				     incFn, decFn, halfFn,
				     ltFn, gtFn,
				     min, minFn,
				     max, maxFn,
				     range},
			     constFn)
		      = let
			  val (cases, 
			       minK, maxK, length,
			       shift, mask) 
			    = reduce(cases, ops)
			    
			  val rangeK 
			    = SOME (range(minK,maxK))
			      handle Overflow
			       => NONE
			in
			  if length >= 8 
			     andalso
			     (isSome rangeK
			      andalso
			      valOf rangeK <= 2 * length)
			    then let
				   val rangeK = valOf rangeK

				   val cases 
				     = List.insertionSort
				       (cases, 
					fn ((k,target),(k',target')) 
					 => ltFn(k,k'))
				 in 
				   doitTable(cases, 
					     ops,
					     minK, maxK, rangeK,
					     shift, mask,
					     constFn)
				 end
			    else effectDefault gef
			                       {label = label, 
						transfer = transfer}
			end
		  in
		    case cases
		      of Transfer.Cases.Char cases
		       => doit
			  (cases,
			   {zero = #"\000",
			    even = fn c => (Char.ord c) mod 2 = 0,
			    incFn = Char.succ,
			    decFn = Char.pred,
			    halfFn = fn c => Char.chr((Char.ord c) div 2),
			    ltFn = Char.<,
			    gtFn = Char.>,
			    min = Char.minChar,
			    minFn = Char.min,
			    max = Char.maxChar,
			    maxFn = Char.max,
			    range = fn (min,max) => ((Char.ord max) - 
						     (Char.ord min)) + 1},
			   Immediate.const_char)
		       | Transfer.Cases.Int cases
		       => doit
			  (cases,
			   {zero = 0,
			    even = fn i => i mod 2 = 0,
			    incFn = fn i => i + 1,
			    decFn = fn i => i - 1,
			    halfFn = fn i => i div 2,
			    ltFn = Int.<,
			    gtFn = Int.>,
			    min = Int.minInt,
			    minFn = Int.min,
			    max = Int.maxInt,
			    maxFn = Int.max,
			    range = fn (min,max) => max - min + 1},
			   Immediate.const_int)
		       | Transfer.Cases.Word cases
		       => doit
			  (cases,
			   {zero = 0wx0,
			    even = fn w => Word.mod(w,0wx2) = 0wx0,
			    incFn = fn x => Word.+(x,0wx1),
			    decFn = fn x => Word.-(x,0wx1),
			    halfFn = fn x => Word.div(x,0wx2),
			    ltFn = Word.<,
			    gtFn = Word.>,
			    min = 0wx0,
			    minFn = Word.min,
			    max = 0wxFFFFFFFF,
			    maxFn = Word.max,
			    range = fn (min,max) => ((Word.toInt max) -
						     (Word.toInt min) + 
						     1)},
			   Immediate.const_word)
		  end
	       | _ => effectDefault gef 
		                    {label = label,
				     transfer = transfer}

	fun fallNone (gef as GEF {generate,effect,fall})
	             {label, target, live} : Assembly.t AppendList.t
	  = let
	      fun default ()
		= let
		    val {pre, post} = commit {label = target, live = live}
		  in
		    AppendList.appends
		    [pre,
		     AppendList.single
		     (Assembly.instruction_jmp
		      {target = Operand.label target,
		       absolute = false}),
		     post]
		  end
	    in
	      case getLabelInfo target
		of {layedOut = ref true, ...}
		 => default ()
		 | _
		 => (enque {source = label, target = target};
		     default ())
	    end

	datatype z = datatype x86JumpInfo.status
	fun fallDefault (gef as GEF {generate,effect,fall})
	                {label, target, live} : Assembly.t AppendList.t
	  = let
	      fun default jmp
		= let
		    val {pre, post} = commit {label = target, live = live}
		  in
		    AppendList.appends
		    [pre,
		     if jmp
		       then AppendList.single
			    (Assembly.instruction_jmp
			     {target = Operand.label target,
			      absolute = false})
		       else AppendList.empty,
		     post]
		  end
	    in
	      case getLabelInfo target
		of {layedOut = ref true, ...}
		 => default true
		 | li as {block as Block.T {entry, ...}, preds, ...}
		 => (case getNear(jumpInfo, target)
		       of Count 1 => generate gef
			                      {label = target,
					       falling = true,
					       unique = true}
		        | _ => AppendList.appends
			       [default false,
				AppendList.single
				(Assembly.directive_reset ()),
				generate gef
				         {label = target,
					  falling = true,
					  unique = false}])
	    end
	  
	fun make {generate, effect, fall}
	  = generate (GEF {generate = generate,
			   effect = effect,
			   fall = fall})

	val generate
	  = case optimize 
	      of 0 => make {generate = generateAll,
			    effect = effectDefault,
			    fall = fallNone}
	       | _ => make {generate = generateAll,
			    effect = effectJumpTable,
			    fall = fallDefault}

	val _ = List.foreach
	        (blocks,
		 fn Block.T {entry, ...}
		  => (case entry
			of Func {label, ...} => enque {source = label, target = label}
			 | _ => ()))
	fun doit () : Assembly.t list list
	  = (case deque ()
	       of NONE => []
	        | SOME label
		=> (case AppendList.toList (generate {label = label,
						      falling = false,
						      unique = false})
		      of [] => doit ()
		       | block => block::(doit ())))
	val assembly = doit ()
	val _ = destLabelInfo ()
      in
	data::assembly
      end

  val (generateTransfers, generateTransfers_msg)
    = tracerTop
      "generateTransfers"
      generateTransfers

  fun generateTransfers_totals ()
    = (generateTransfers_msg ();
       Control.indent ();
       x86Liveness.LiveInfo.verifyLiveInfo_msg ();
       x86JumpInfo.verifyJumpInfo_msg ();
       x86EntryTransfer.verifyEntryTransfer_msg ();
       x86LoopInfo.createLoopInfo_msg ();
       x86LiveTransfers.computeLiveTransfers_totals ();
       Control.unindent ())
end
