
functor x86GenerateTransfers(S: X86_GENERATE_TRANSFERS_STRUCTS): X86_GENERATE_TRANSFERS =
struct

  open S
  open x86
  open x86JumpInfo
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
		       structure x86JumpInfo = x86JumpInfo)

  val pointerSize = x86MLton.pointerSize
  val wordSize = x86MLton.wordSize

  val transferRegs
    = [x86.Register.ebx,
       x86.Register.bl,
       x86.Register.ecx,
       x86.Register.cl,
       x86.Register.edx,
       x86.Register.dl,
(*
       x86.Register.edi,
       x86.Register.esi,
*)
       x86.Register.esp,
       x86.Register.ebp]

  val transferFltRegs : Int.t = 6

  val stackTopReg = Register.edi
  val frontierReg = Register.esi
  val stackTop = x86MLton.gcState_stackTopContents
  val frontier = x86MLton.gcState_frontierContents

  datatype gef = GEF of {generate : gef -> 
			            {label : Label.t,
				     falling : bool,
				     unique : bool} -> 
				    Assembly.t AppendList.t,
			 effect : gef -> 
			          {transfer : Transfer.t} ->
				  Assembly.t AppendList.t,
			 fall : gef ->
			        {label : Label.t,
				 live : MemLocSet.t} ->
				Assembly.t AppendList.t}

  fun generateTransfers {chunk as Chunk.T {blocks},
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
	val cstaticClasses = !x86MLton.Classes.cstaticClasses
	val heapClasses = !x86MLton.Classes.heapClasses
	val ccallflushClasses = ClassSet.+(cstaticClasses, heapClasses)
	  
	fun removeHoldMemLocs memlocs
	  = List.removeAll
	    (memlocs, 
	     fn m => ClassSet.contains(holdClasses, MemLoc.class m))

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
	       commit_classes = [],
	       remove_memlocs = [],
	       remove_classes = [],
	       dead_memlocs = [],
	       dead_classes = []}),
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
	       {commit_memlocs = [],
		commit_classes = ClassSet.toList farflushClasses,
		remove_memlocs = [],
		remove_classes = [],
		dead_memlocs = [],
		dead_classes = []})],
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

	val liveTransfers
	  = x86LiveTransfers.computeLiveTransfers
	    {chunk = chunk,
	     transferRegs = transferRegs,
	     transferFltRegs = transferFltRegs,
	     liveInfo = liveInfo,
	     jumpInfo = jumpInfo}
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
			   set = setLayoutInfo}
	  = Property.getSet(Label.plist, 
			    Property.initRaise ("layoutInfo", Label.layout))
	val profileInfo as {get = getProfileInfo : Label.t -> ProfileInfo.t,
			    set = setProfileInfo}
	  = Property.getSet(Label.plist, 
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
	  val queue = ref (Queue.empty ())
	in
	  fun enque x = queue := Queue.enque(!queue, x)
	  fun deque () = case Queue.deque(!queue)
			   of NONE => NONE
			    | SOME(x, queue') => (queue := queue';
						  SOME x)
	end

	fun enqueCompensationBlock {label, id}
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
			      {live = stackTop::
			              frontier::
				      (MemLocSet.toList live),
			       id = id})::
			     nil,
			   transfer = Transfer.goto {target = label}}
	    in
	      setLive(liveInfo, label', live);
	      incNear(jumpInfo, label');
	      Assert.assert("enqueCompensationBlock",
			    fn () => getNear(jumpInfo, label') = Count 1);
	      x86LiveTransfers.setLiveTransfersEmpty(liveTransfers, label');
	      setLayoutInfo(label', SOME block);
	      setProfileInfo(label', profileInfo);
	      enque label';
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

		     val pre
		       = case entry
			   of Jump {label}
			    => if falling
				 then if unique
					then AppendList.cons
					     (Assembly.label label,
					      profile_assembly)
					else AppendList.append
					     (AppendList.fromList
					      [Assembly.label label,
					       (* near entry & 
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
					      profile_assembly)
				 else AppendList.append
				      (AppendList.fromList
				       [Assembly.pseudoop_p2align 2,
					Assembly.label label,
					(* near entry & 
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
				       profile_assembly)
			    | Func {label,...}
			    => AppendList.append
			       (AppendList.fromList
				[Assembly.pseudoop_p2align 2,
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
				[Assembly.pseudoop_p2align 2,
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
				      size = x86MLton.pointerSize}
				   end))))
			    | Handler {label, 
				       frameInfo as Entry.FrameInfo.T 
				                    {size,
						     frameLayoutsIndex},
				       ...}
			    => AppendList.append
			       (AppendList.fromList
				[Assembly.pseudoop_p2align 2,
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
				      size = x86MLton.pointerSize}
				   end))))
			    | Runtime {label,
				       frameInfo as Entry.FrameInfo.T 
				                    {size,
						     frameLayoutsIndex}}
			    => AppendList.append
			       (AppendList.fromList
				[Assembly.pseudoop_p2align 2,
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
				      size = x86MLton.pointerSize}
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
					 {commit_memlocs = [],
					  commit_classes =[],
					  remove_memlocs = [],
					  remove_classes = [],
					  dead_memlocs = MemLocSet.toList dead,
					  dead_classes = []})::
					statements,
				 liveIn)
			      end)

		     val statements = AppendList.fromList statements

		     val transfer = effect gef {transfer = transfer}
		   in
		     AppendList.appends 
		     [pre,
		      statements,
		      transfer]
		   end)

	datatype z = datatype Transfer.t
	fun effectDefault (gef as GEF {generate,effect,fall})
	                  {transfer} : Assembly.t AppendList.t
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
			     = enqueCompensationBlock {label = falsee,
						       id = id};
			 in
			   AppendList.append
			   (AppendList.fromList
			    [Assembly.directive_force
			     {commit_memlocs = [],
			      commit_classes = ClassSet.toList nearflushClasses,
			      remove_memlocs = [],
			      remove_classes = [],
			      dead_memlocs = [],
			      dead_classes = []},
			     Assembly.directive_saveregalloc
			     {live = stackTop::
			             frontier::
				     (MemLocSet.toList falsee_live),
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
			   val truee' = enqueCompensationBlock {label = truee,
								id = id};
			 in
			   AppendList.append
			   (AppendList.fromList
			    [Assembly.directive_force
			     {commit_memlocs = [],
			      commit_classes = ClassSet.toList nearflushClasses,
			      remove_memlocs = [],
			      remove_classes = [],
			      dead_memlocs = [],
			      dead_classes = []},
			     Assembly.directive_saveregalloc
			     {live = stackTop::
			             frontier::
				     (MemLocSet.toList truee_live),
			      id = id},
			     Assembly.instruction_jcc
			     {condition = condition,
			      target = Operand.label truee'}],
			    (fall gef 
			          {label = falsee,
				   live = falsee_live}))
			 end
		   in
		     case (getNear(jumpInfo, truee),
			   getNear(jumpInfo, falsee))
		       of (Count 1, Count 1)
			=> if truee_live_length <= falsee_live_length
			     then fall_falsee ()
			     else fall_truee ()
			| (Count 1, _)
		        => fall_truee ()
		        | (_, Count 1)
		        => fall_falsee ()
		        | _
		        => if truee_live_length <= falsee_live_length
		             then fall_falsee ()
		             else fall_truee ()
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
				val target' = enqueCompensationBlock 
				              {label = target,
					       id = id}
			      in
				AppendList.fromList
				[Assembly.instruction_cmp
				 {src1 = test,
				  src2 = Operand.immediate_const k,
				  size = size},
				 Assembly.directive_saveregalloc
				 {live = stackTop::
				         frontier::
					 (MemLocSet.toList target_live),
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
		       {commit_memlocs = [],
			commit_classes = ClassSet.toList nearflushClasses,
			remove_memlocs = [],
			remove_classes = [],
			dead_memlocs = [],
			dead_classes = []}),
		      AppendList.appends cases,
		      if MemLocSet.isEmpty dead
			then AppendList.empty
			else AppendList.single
			     (Assembly.directive_force
			      {commit_memlocs = [],
			       commit_classes = [],
			       remove_memlocs = [],
			       remove_classes = [],
			       dead_memlocs = MemLocSet.toList dead,
			       dead_classes = []}),
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
		     val stackTopDeref'
		       = x86MLton.gcState_stackTopDeref
		     val stackTopDeref
		       = x86MLton.gcState_stackTopDerefOperand
		     val bytes 
		       = x86.Operand.immediate_const_int size

		     val liveReturn = x86Liveness.LiveInfo.getLive(liveInfo, return)
		     val liveHandler 
		       = case handler
			   of SOME handler
			    => x86Liveness.LiveInfo.getLive(liveInfo, handler)
			    | _ => MemLocSet.empty
		     val live = MemLocSet.unions [MemLocSet.fromList live,
						  liveReturn,
						  liveHandler]
		     val live = MemLocSet.toList live
		   in
		     (* flushing at far transfer *)
		     (farTransfer live
		      (AppendList.fromList
		       [(* stackTop += bytes *)
			x86.Assembly.instruction_binal 
			{oper = x86.Instruction.ADD,
			 dst = stackTop,
			 src = bytes, 
			 size = x86MLton.pointerSize},
			(* *(stackTop) = return *)
			x86.Assembly.instruction_mov
			{dst = stackTopDeref,
			 src = Operand.immediate_label return,
			 size = pointerSize},
			x86.Assembly.directive_force
			{commit_memlocs = [stackTopDeref'],
			 commit_classes = [],
			 remove_memlocs = [],
			 remove_classes = [],
			 dead_memlocs = [],
			 dead_classes = []}])
		      (AppendList.single
		       (Assembly.instruction_jmp
			{target = Operand.label target,
			 absolute = false})))
		   end
		| Return {live}
		=> let
		     val stackTopDeref
		       = x86MLton.gcState_stackTopDerefOperand
		   in
		     (* flushing at far transfer *)
		     (farTransfer live
		      AppendList.empty
		      (AppendList.single
		       (* jmp *(stackTop) *)
		       (x86.Assembly.instruction_jmp
			{target = stackTopDeref,
			 absolute = true})))
		   end
		| Raise {live}
		=> let
		     val exnStack 
		       = x86MLton.gcState_currentThread_exnStackContentsOperand
		     val stackTop 
		       = x86MLton.gcState_stackTopContentsOperand
		     val stackTopDeref
		       = x86MLton.gcState_stackTopDerefOperand
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
			  size = x86MLton.pointerSize},
			 x86.Assembly.instruction_binal
			 {oper = x86.Instruction.ADD,
			  dst = stackTop,
			  src = exnStack,
			  size = x86MLton.pointerSize}])
		       (AppendList.single
			(* jmp *(stackTop) *)
			(x86.Assembly.instruction_jmp
			 {target = stackTopDeref,
			  absolute = true})))
		    end
		| Runtime {target, args, live, return, size}
		=> let
		     val {dead, ...}
		       = livenessTransfer {transfer = transfer,
					   liveInfo = liveInfo}

		     val _ = enque return

		     val stackTop 
		       = x86MLton.gcState_stackTopContentsOperand
		     val bytes 
		       = x86.Operand.immediate_const_int size
		     val stackTopDeref
		       = x86MLton.gcState_stackTopDerefOperand

		     val liveReturn = x86Liveness.LiveInfo.getLive(liveInfo, return)
		     val live = MemLocSet.unions [MemLocSet.fromList live,
						  liveReturn]
		     val live = MemLocSet.toList live

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
		       {commit_memlocs = [],
			commit_classes = [],
			remove_memlocs = [],
			remove_classes = [],
			dead_memlocs = MemLocSet.toList dead,
			dead_classes = []},
		       (* stackTop += bytes *)
		       x86.Assembly.instruction_binal 
		       {oper = x86.Instruction.ADD,
			dst = stackTop,
			src = bytes, 
			size = x86MLton.pointerSize},
		       (* *(stackTop) = return *)
		       x86.Assembly.instruction_mov
		       {dst = stackTopDeref,
			src = Operand.immediate_label return,
			size = pointerSize},
		       (* flushing at Runtime *)
		       Assembly.directive_force
		       {commit_memlocs = live,
			commit_classes = ClassSet.toList runtimeClasses,
			remove_memlocs = [],
			remove_classes = [],
			dead_memlocs = [],
			dead_classes = []},
		       Assembly.directive_ccall (),
		       Assembly.instruction_call 
		       {target = Operand.label target,
			absolute = false},
		       Assembly.directive_force
		       {commit_memlocs = [],
			commit_classes = [],
			remove_memlocs = [],
			remove_classes = [],
			dead_memlocs = [],
			dead_classes = ClassSet.toList runtimeClasses}],
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
		      (farTransfer []
		       AppendList.empty
		       (AppendList.single
			(* jmp *(stackTop) *)
			(x86.Assembly.instruction_jmp
			 {target = stackTopDeref,
			  absolute = true})))]
		   end
		| CCall {target, args, dst, live, return}
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
		       {commit_memlocs = live,
			commit_classes = ClassSet.toList ccallflushClasses,
			remove_memlocs = [],
			remove_classes = [],
			dead_memlocs = MemLocSet.toList dead,
			dead_classes = []},
		       Assembly.directive_ccall (),
		       Assembly.instruction_call 
		       {target = Operand.label target,
			absolute = false},
		       Assembly.directive_force
		       {commit_memlocs = [],
			commit_classes = [],
			remove_memlocs = [],
			remove_classes = [],
			dead_memlocs = [],
			dead_classes = ClassSet.toList ccallflushClasses}],
		      (case dst
			 of NONE => AppendList.empty
			  | SOME (dst,dstsize) 
			  => (case Operand.deMemloc dst
				of NONE => Error.bug "applyFF: dst"
				 | SOME dst
				 => (case Size.class dstsize
				       of Size.INT
					=> AppendList.single
					   (Assembly.directive_return
					    {memloc = dst})
				        | Size.FLT
					=> AppendList.single
					   (Assembly.directive_fltreturn
					    {memloc = dst})
				        | _ => Error.bug "applyFF: dstsize"))),
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
	                     {transfer} : Assembly.t AppendList.t
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
			  val defaultT = enqueCompensationBlock
			                 {label = default,
					  id = idT}

			  val rec filler 
			    = fn ([],_) => []
			       | (cases as (i,target)::cases',j)
			       => if i = j
				    then let
					   val target'
					     = enqueCompensationBlock
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
				   {commit_memlocs = [],
				    commit_classes = [],
				    remove_memlocs = [],
				    remove_classes = [],
				    dead_memlocs = MemLocSet.toList dead,
				    dead_classes = []}),
			   if shift > 0
			     then let
				    val idC = Directive.Id.new ()
				    val defaultC = enqueCompensationBlock
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
				      {commit_memlocs = [],
				       commit_classes = ClassSet.toList nearflushClasses,
				       remove_memlocs = [],
				       remove_classes = [],
				       dead_memlocs = [checkTemp'],
				       dead_classes = []},
				      Assembly.directive_saveregalloc
				      {id = idC,
				       live = stackTop::
				              frontier::
					      (MemLocSet.toList default_live)},
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
			   {commit_memlocs = [],
			    commit_classes = ClassSet.toList nearflushClasses,
			    remove_memlocs = [],
			    remove_classes = [],
			    dead_memlocs = [],
			    dead_classes = []},
			   Assembly.directive_cache
			   {caches = [{register = Register.eax,
				       memloc = indexTemp',
				       reserve = false}]},
			   Assembly.instruction_cmp
			   {src1 = indexTemp,
			    src2 = Operand.immediate_const_int rangeK,
			    size = size},
			   Assembly.directive_saveregalloc
			   {id = idT,
			    live = stackTop::
			           frontier::
				   (MemLocSet.toList live)},
			   Assembly.instruction_jcc
			   {condition = Instruction.AE,
			    target = Operand.label defaultT},
			   Assembly.instruction_jmp
			   {target = address,
			    absolute = true},
			   Assembly.directive_force
			   {commit_memlocs = [],
			    commit_classes = [],
			    remove_memlocs = [],
			    remove_classes = [],
			    dead_memlocs = [indexTemp'],
			    dead_classes = []}],
			  AppendList.fromList
			  [Assembly.pseudoop_data (),
			   Assembly.pseudoop_p2align 2,
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
			                       {transfer = transfer}
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
		                    {transfer = transfer}

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
		    {commit_memlocs = MemLocSet.toList live,
		     commit_classes = ClassSet.toList nearflushClasses,
		     remove_memlocs = [],
		     remove_classes = [],
		     dead_memlocs = [],
		     dead_classes = []})::
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
		 => (enque label;
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
		     {commit_memlocs = MemLocSet.toList live,
		      commit_classes = ClassSet.toList nearflushClasses,
		      remove_memlocs = [],
		      remove_classes = [],
		      dead_memlocs = [],
		      dead_classes = []})],
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
      in
	assembly
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
       x86LiveTransfers.computeLiveTransfers_totals ();
       Control.unindent ())
end