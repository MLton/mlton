(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor x86AllocateRegisters(S: X86_ALLOCATE_REGISTERS_STRUCTS) : X86_ALLOCATE_REGISTERS =
struct
  
  open S
  open x86

  val tracer
    = Control.traceBatch
(*
    = fn s => fn f => (Control.trace (Control.Detail, s) f, fn () => ())
*)

  fun partition(l, p)
    = let
	val rec partition' 
	  = fn ([],PS) => PS
	     | (h::t,PS) => let
			      val rec partition''
				= fn [] => [[h]]
				   | P::PS => if List.exists(P,fn x => p(h, x))
						then (h::P)::PS
						else P::(partition'' PS)
			    in
			      partition'(t,partition'' PS)
			    end
      in
	partition'(l,[])
      end

  fun totalOrder (l, plt)
    = let
	val rec totalOrder'
	  = fn ([],l) => l
	     | (h::t,l) => let
			     val rec split
			       = fn (lt,t) 
			          => case List.splitPrefix
			                  (t, fn x => plt(x,h))
				       of (nil,t) => lt@[h]@t
					| (lt',t) => split(lt@lt',t)
			   in
			     totalOrder'(t,split([],l))
			   end
      in
	totalOrder'(l,[])
      end

  val bool_lt
    = fn (false, true) => true
       | _ => false

  val bool_gt
    = fn (true, false) => true
       | _ => false

  fun option_lt lt
    = fn (SOME x, SOME y) => lt (x,y)
       | (NONE, SOME _) => true
       | _ => false

  structure Liveness =
    struct
      datatype future = USE of MemLoc.t
	              | DEF of MemLoc.t
	              | USEDEF of MemLoc.t

      val future_toString
	= fn USE memloc => concat ["USE ", MemLoc.toString memloc]
	   | DEF memloc => concat ["DEF ", MemLoc.toString memloc]
	   | USEDEF memloc => concat ["USEDEF ", MemLoc.toString memloc]

      val future_eq
	= fn (USE memloc1, USE memloc2) => MemLoc.eq(memloc1,memloc2)
	   | (USEDEF memloc1, USEDEF memloc2) => MemLoc.eq(memloc1,memloc2)
	   | (DEF memloc1, DEF memloc2) => MemLoc.eq(memloc1,memloc2)
	   | _ => false

      type hint = MemLoc.t * Register.t

      val hint_toString
	= fn (memloc, register) => concat [MemLoc.toString memloc,
					   " -> ",
					   Register.toString register]

      val hint_eq
	= fn ((memloc1,register1),(memloc2,register2))
	   => MemLoc.eq(memloc1, memloc2)
	      andalso
	      Register.eq(register1, register2)

      type t = {dead: MemLoc.t list,
		commit: MemLoc.t list,
		remove: MemLoc.t list,
		future: future list,
		hint: hint list}

      fun toString {dead, commit, remove, future, hint}
	= let
	    fun doit (name, l, toString, s)
	      = List.fold(l, s,
			  fn (x, s)
			   => concat [name, toString x, "\n", s])
	  in
	    doit("dead: ", dead, MemLoc.toString,
	    doit("commit: ", commit, MemLoc.toString,
	    doit("remove: ", remove, MemLoc.toString,
	    doit("future: ", future, future_toString, 
	    doit("hint: ", hint, hint_toString, "")))))
	  end

      fun toComments {dead, commit, remove, future, hint} 
	= let
	    fun doit (name, l, toString, ac)
	      = List.fold(l, ac, 
			  fn (x, ac) 
			   => (Assembly.comment (concat [name, toString x]))::
			      ac)
	  in
	    doit("dead: ", dead, MemLoc.toString,
	    doit("commit: ", commit, MemLoc.toString,
	    doit("remove: ", remove, MemLoc.toString,
	    doit("future: ", future, future_toString, 
	    doit("hint: ", hint, hint_toString, [])))))
	  end

      datatype commit = NO | COMMIT | REMOVE
      datatype scan = CONTINUE of commit | RETURN of commit
      fun scan(future, default, f)
	= case future
	    of [] => default
	     | h::future 
	     => (case f(default, h)
		   of CONTINUE default => scan(future, default, f)
		    | RETURN commit => commit)

      val rec split
	= fn ([],p) => ([],[],[])
           | (h::t,p) => let
			   val (no,commit,remove) = split(t, p)
			 in
			   case p h
			     of NO => (h::no,commit,remove)
			      | COMMIT => (no,h::commit,remove)
			      | REMOVE => (no,commit,h::remove)
			 end

      fun liveness {uses: MemLoc.t list,
		    defs: MemLoc.t list,
		    future: future list,
		    hint: hint list} :
	           {info: t,
		    future: future list,
		    hint: hint list}
	= let
	    local
	      fun doit(memlocs, all)
		= List.fold
		  (memlocs,
		   all,
		   fn (memloc,all)
		    => if List.contains(all,
					memloc,
					MemLoc.eq)
			 then all
			 else memloc::all)

	      fun doit'(memlocs, all)
		= List.fold
		  (memlocs,
		   all,
		   fn (memloc,all)
		    => doit(MemLoc.utilized memloc, all))
	    in
	      val allUses 
		= doit'(defs,
		  doit'(uses,
		  doit(uses,
		       [])))
	      val allDefs
		= doit(defs,
		       [])
	    end

	    val current_usedef 
	      = List.keepAll(allDefs,
			     fn memloc => List.contains(allUses,
							memloc,
							MemLoc.eq))
	    val current_use
	      = List.keepAll(allUses,
			     fn memloc => not (List.contains(current_usedef,
							     memloc,
							     MemLoc.eq)))

	    val current_def
	      = List.keepAll(allDefs,
			     fn memloc => not (List.contains(current_usedef,
							     memloc,
							     MemLoc.eq)))

	    local
	      fun doit (USE memloc)
		= if not (List.contains(allDefs,
					memloc,
					MemLoc.eq))
		     andalso
		     List.exists
		     (memloc::(MemLoc.utilized memloc),
		      fn memloc'
		       => List.exists
		          (allDefs,
			   fn memloc''
			    => MemLoc.mayAlias(memloc',
					       memloc'')))
		    then NONE
		    else SOME (USE memloc)
		| doit (USEDEF memloc)
		= if not (List.contains(allDefs,
					memloc,
					MemLoc.eq))
		     andalso
		     List.exists
		     (memloc::(MemLoc.utilized memloc),
		      fn memloc'
		       => List.exists
		          (allDefs,
			   fn memloc''
			    => MemLoc.mayAlias(memloc',
					       memloc'')))
		    then NONE 
		  else if not (List.contains(allUses,
					     memloc,
					     MemLoc.eq))
		          andalso 
			  List.exists
			  (allUses,
			   fn memloc'
			    => MemLoc.mayAlias(memloc,
					       memloc'))
			 then SOME (USE memloc)
		  else SOME (USEDEF memloc)
		| doit (DEF memloc)
		= if (not (List.contains(allDefs,
					 memloc,
					 MemLoc.eq))
		      andalso
		      List.exists
		      (memloc::(MemLoc.utilized memloc),
		       fn memloc'
		        => List.exists
		           (allDefs,
			    fn memloc''
			     => MemLoc.mayAlias(memloc',
						memloc''))))
		     orelse
		     (not (List.contains(allUses,
					 memloc,
					 MemLoc.eq))
		      andalso 
		      List.exists
		      (allUses,
		       fn memloc'
		        => MemLoc.mayAlias(memloc,
					   memloc')))
		    then NONE
		    else SOME (DEF memloc)
	    in
	      val future = List.keepAllMap(future, doit)
	    end

	    val (no_use,commit_use,remove_use)
	      = split(current_use,
		      fn memloc
		       => scan
		          (future,
			   REMOVE,
			   fn (default, USE memloc') 
			    => if MemLoc.eq(memloc,memloc')
				 then RETURN NO
				 else if MemLoc.mayAlias(memloc,memloc')
					then CONTINUE default
					else CONTINUE default
			    | (default, USEDEF memloc')
			    => if MemLoc.eq(memloc,memloc')
				 then RETURN NO
				 else if MemLoc.mayAlias(memloc,memloc')
					then RETURN REMOVE
					else CONTINUE default
			    | (default, DEF memloc')
			    => if MemLoc.eq(memloc,memloc')
				 then RETURN REMOVE
				 else if MemLoc.mayAlias(memloc,memloc')
					then RETURN REMOVE
					else CONTINUE default))

	    val (no_usedef,commit_usedef,remove_usedef)
	      = split(current_usedef,
		      fn memloc
		       => scan
		          (future,
			   REMOVE,
			   fn (default, USE memloc')
			    => if MemLoc.eq(memloc,memloc')
				 then CONTINUE COMMIT
				 else if MemLoc.mayAlias(memloc,memloc')
					then RETURN COMMIT
					else CONTINUE default
			    | (default, USEDEF memloc')
			    => if MemLoc.eq(memloc,memloc')
				 then RETURN NO
				 else if MemLoc.mayAlias(memloc,memloc')
					then RETURN REMOVE
					else CONTINUE default
			    | (default, DEF memloc')
			    => if MemLoc.eq(memloc,memloc')
				 then RETURN NO
				 else if MemLoc.mayAlias(memloc,memloc')
					then RETURN REMOVE
					else CONTINUE default))

	    val (no_def,commit_def,remove_def)
	      = split(current_def,
		      fn memloc
		       => scan
		          (future,
			   REMOVE,
			   fn (default, USE memloc')
			    => if MemLoc.eq(memloc,memloc')
				 then CONTINUE COMMIT
				 else if MemLoc.mayAlias(memloc,memloc')
					then RETURN COMMIT
					else CONTINUE default
			    | (default, USEDEF memloc')
			    => if MemLoc.eq(memloc,memloc')
				 then RETURN NO
				 else if MemLoc.mayAlias(memloc,memloc')
					then RETURN REMOVE
					else CONTINUE default
			    | (default, DEF memloc')
			    => if MemLoc.eq(memloc,memloc')
				 then RETURN NO
				 else if MemLoc.mayAlias(memloc,memloc')
					then RETURN REMOVE
					else CONTINUE default))

	    val no = List.concat [no_use,no_usedef,no_def]
	    val commit = List.concat [commit_use,commit_usedef,commit_def]
	    val {yes = _, no = commit} = List.partition(commit, MemLoc.isTemp)
	    val remove = List.concat [remove_use,remove_usedef,remove_def]
	    val {yes = dead, no = remove} = List.partition(remove, MemLoc.isTemp)

	    val future
	      = let
		  val future
		    = List.removeAll
		      (future,
		       fn USE memloc => List.contains(current_use,
						      memloc,
						      MemLoc.eq)
		        | USEDEF memloc => List.contains(current_usedef,
							 memloc,
							 MemLoc.eq)
		        | DEF memloc => List.contains(current_def,
						      memloc,
						      MemLoc.eq))

		  fun doit(memlocs, constructor, future)
		    = List.fold
		      (memlocs,
		       future,
		       fn (memloc,future) 
		        => (constructor memloc)::future)
		in
		  doit(current_use, USE,
		  doit(current_usedef, USEDEF,
		  doit(current_def, DEF,
		       future)))
		end

	    val future
	      = let
		  val rec cut
		    = fn ([],_) => []
		       | (future,0) 
		       => List.keepAll
		          (future,
			   MemLoc.isTemp o (fn USE memloc => memloc
			                     | USEDEF memloc => memloc
					     | DEF memloc => memloc))
		       | (h::future,n) => h::(cut(future,n-1))
		in 
		  cut(future, !Control.Native.future)
		end

	    val info 
	      = {dead = dead,
		 commit = commit,
		 remove = remove,
		 future = future,
		 hint = hint}
	  in
	    {info = info,
	     future = future,
	     hint = hint}
	  end

      fun livenessAssembly {assembly: Assembly.t,
			    future: future list,
			    hint: hint list} :
	                   {info: t,
			    future: future list,
			    hint: hint list}
	= case assembly
	    of Assembly.Directive Directive.Reset 
	     => liveness {uses = [],
			  defs = [],
			  future = [],
			  hint = []}
	     | Assembly.Directive Directive.Flush
	     => liveness {uses = [],
			  defs = [],
			  future
			  = List.keepAllMap
			    (future,
			     fn USE memloc 
			      => if MemLoc.onFlush memloc
				   then (case Size.class (MemLoc.size memloc)
					   of Size.INT => SOME (USE memloc)
					    | _ => NONE)
				   else SOME (USE memloc)
			      | USEDEF memloc 
			      => if MemLoc.onFlush memloc
				   then (case Size.class (MemLoc.size memloc)
					   of Size.INT => SOME (USE memloc)
					    | _ => NONE)
				   else SOME (USEDEF memloc)
			      | DEF memloc
			      => if MemLoc.onFlush memloc
				   then NONE
				   else SOME (DEF memloc)),
			  hint = hint}
	     | Assembly.Directive Directive.Clear
	     => liveness {uses = [],
			  defs = [],
			  future
			  = List.keepAllMap
			    (future,
			     fn USE memloc 
			      => if MemLoc.onFlush memloc
				   then (case Size.class (MemLoc.size memloc)
					   of Size.INT => SOME (USE memloc)
					    | _ => NONE)
				   else SOME (USE memloc)
			      | USEDEF memloc 
			      => if MemLoc.onFlush memloc
				   then (case Size.class (MemLoc.size memloc)
					   of Size.INT => SOME (USE memloc)
					    | _ => NONE)
				   else SOME (USEDEF memloc)
			      | DEF memloc
			      => if MemLoc.onFlush memloc
				   then NONE
				   else SOME (DEF memloc)),
			  hint = hint}
	     | Assembly.Directive (Directive.Cache {memloc, register, reserve})
	     => let
		  val {uses,defs,...} = Assembly.uses_defs_kills assembly
		  val hint
		    = (memloc,register)::
		      (List.foldr
		       (hint,
			[],
			fn ((hint_memloc,hint_register),hint)
		         => if MemLoc.eq(hint_memloc,
					 memloc)
			      then hint
			      else (hint_memloc,hint_register)::hint))

		in
		  liveness {uses = List.keepAllMap(uses,Operand.deMemloc),
			    defs = List.keepAllMap(defs,Operand.deMemloc),
			    future = future,
			    hint = hint}
		end
	     | _ 
	     => let
		  val {uses,defs,...} = Assembly.uses_defs_kills assembly
		in
		  liveness {uses = List.keepAllMap(uses,Operand.deMemloc),
			    defs = List.keepAllMap(defs,Operand.deMemloc),
			    future = future,
			    hint = hint}
		end

      fun toLiveness (assembly: Assembly.t list) : ((Assembly.t * t) list)
	= let
	    val {assembly,...}
	      = List.foldr
	        (assembly,
		 {assembly = [], future = [], hint = []},
		 fn (asm, {assembly,future,hint})
		  => let
		       val {info,future,hint}
			 = livenessAssembly {assembly = asm,
					     future = future,
					     hint = hint}
		     in
		       {assembly = (asm,info)::assembly,
			future = future,
			hint = hint}
		     end)
	  in
	    assembly
	  end

      val (toLiveness,toLiveness_msg)
	= tracer
	  "toLiveness"
	  toLiveness

      fun toNoLiveness (assembly: Assembly.t list) : ((Assembly.t * t) list)
	= List.map(assembly, fn asm => (asm,{dead = [],
					     commit = [],
					     remove = [],
					     future = [],
					     hint = []})) 

      val (toNoLiveness,toNoLiveness_msg)
	= tracer
	  "toNoLiveness"
	  toNoLiveness
   end

  structure RegisterAllocation =
    struct
      exception Spill
      val spillClass = MemLoc.Class.new "Spill"
      val spill : Int.t ref = ref 0
      val spillLabel = Label.fromString "spill"
      val commitDepth : Int.t ref = ref 0

      datatype commit 
	= NO 
	| COMMIT of int 
	| REMOVE of int
	| TRYCOMMIT of int 
	| TRYREMOVE of int

      val commit_toString
	= fn NO => "NO"
           | COMMIT i => "COMMIT " ^ (Int.toString i)
	   | REMOVE i => "REMOVE " ^ (Int.toString i)
           | TRYCOMMIT i => "TRYCOMMIT " ^ (Int.toString i)
	   | TRYREMOVE i => "TRYREMOVE " ^ (Int.toString i)

      type value = {register: Register.t,
		    memloc: MemLoc.t,
		    weight: int,
		    sync: bool,
		    commit: commit}

      fun value_toString {register, memloc, weight, sync, commit}
	= concat [Register.toString register, " ",
		  MemLoc.toString memloc, " ",
		  Int.toString weight, " ",
		  Bool.toString sync, " ",
		  commit_toString commit]

      fun value_eq (value1 as {register = register1,
			       memloc = memloc1,
			       weight = weight1,
			       sync = sync1,
			       commit = commit1},
		    value2 as {register = register2,
			       memloc = memloc2,
			       weight = weight2,
			       sync = sync2,
			       commit = commit2})
	= Register.eq(register1, register2)
	  andalso
	  MemLoc.eq(memloc1, memloc2)
	  andalso
	  weight1 = weight2
	  andalso
	  sync1 = sync2
	  andalso
	  commit1 = commit2

      type fltvalue = {fltregister: FltRegister.t,
		       memloc: MemLoc.t,
		       weight: int,
		       sync: bool,
		       commit: commit}

      fun fltvalue_toString {fltregister, memloc, weight, sync, commit}
	= concat [FltRegister.toString fltregister, " ",
		  MemLoc.toString memloc, " ",
		  Int.toString weight, " ",
		  Bool.toString sync, " ",
		  commit_toString commit]

      fun fltvalue_eq (value1 as {fltregister = fltregister1,
				  memloc = memloc1,
				  weight = weight1,
				  sync = sync1,
				  commit = commit1},
		       value2 as {fltregister = fltregister2,
				  memloc = memloc2,
				  weight = weight2,
				  sync = sync2,
				  commit = commit2})
	= FltRegister.eq(fltregister1, fltregister2)
	  andalso
	  MemLoc.eq(memloc1, memloc2)
	  andalso
	  weight1 = weight2
	  andalso
	  sync1 = sync2
	  andalso
	  commit1 = commit2
		      
      type t = {entries: value list,
		reserved: Register.t list,
		fltstack: fltvalue list}

      fun toString (registerAllocation as {entries, reserved, fltstack}: t) 
	= let
	    fun doit (name, l, toString, ac)
	      = (name ^ "\n") ^
	        (List.fold(l, ac,
			   fn (x, ac)
			    => (toString x) ^ "\n" ^ ac))
	  in
	    doit("entries:", entries, value_toString,
	    doit("reserved:", reserved, Register.toString,
	    doit("fltstack:", fltstack, fltvalue_toString, 
		 "")))
	  end

      fun toComments (registerAllocation as {entries, reserved, fltstack}: t)
	= let
	    fun doit (name, l, toString, ac)
	      = (Assembly.comment name)::
	        (List.fold(l, ac,
			   fn (x, ac)
			    => (Assembly.comment (toString x))::
			       ac))
	  in
	    doit("entries:", entries, value_toString,
	    doit("reserved:", reserved, Register.toString,
	    doit("fltstack:", fltstack, fltvalue_toString,
		 [])))
	  end

      fun eq (registerAllocation1 as {entries = entries1, 
				      reserved = reserved1,
				      fltstack = fltstack1},
	      registerAllocation2 as {entries = entries2, 
				      reserved = reserved2,
				      fltstack = fltstack2})
	= List.equalsAsSet(entries1, 
			   entries2,
			   value_eq)
	  andalso
	  List.equalsAsSet(reserved1,
			   reserved2,
			   Register.eq)
	  andalso
	  List.equals(fltstack1,
		      fltstack2,
		      fltvalue_eq)

      fun empty () : t
	= {entries = [],
	   reserved = [],
	   fltstack = []}

      fun valueMap {map, 
		    registerAllocation as {entries,
					   reserved, 
					   fltstack}: t}
	= {entries = List.map(entries, map),
	   reserved = reserved,
	   fltstack = fltstack}

      fun valueFilter {filter,
		       registerAllocation as {entries, 
					      reserved, 
					      fltstack}: t}
	= List.keepAll(entries, filter)

      fun valuesRegister {register as Register.T {reg, part}, 
			  registerAllocation as {entries, 
						 reserved, 
						 fltstack}: t}
	= List.keepAll(entries, 
		       fn {register 
			   = register' as Register.T {reg = reg',
						      part = part'},...}
		        => reg = reg')

      fun fltvalueMap {map,
		       registerAllocation as {entries, 
					      reserved, 
					      fltstack}: t}
	= {entries = entries,
	   reserved = reserved,
	   fltstack = List.map(fltstack, map)}

      fun fltvalueFilter {filter,
			  registerAllocation as {entries, 
						 reserved, 
						 fltstack} :t}
	= List.keepAll(fltstack, filter)

      fun valuesFltRegister {fltregister as FltRegister.T i,
			     registerAllocation as {entries,
						    reserved,
						    fltstack} : t}
	= List.keepAll(fltstack,
		       fn {fltregister 
			   = fltregister' as FltRegister.T i', ...}
		        => i = i')

      fun reserve {register: Register.t,
		   registerAllocation = {entries, reserved, fltstack}: t}
	= {assembly = [],
	   registerAllocation = {entries = entries,
				 reserved = register::reserved,
				 fltstack = fltstack}}

      fun unreserve {register: Register.t,
		     registerAllocation = {entries, reserved, fltstack}: t}
	= {assembly = [],
	   registerAllocation = {entries = entries,
				 reserved = List.removeAll
				            (reserved,
					     fn register' 
					     => Register.eq(register, 
							    register')),
				 fltstack = fltstack}}

      fun update {value as {register,...},
		  registerAllocation as {entries, reserved, fltstack}: t}
	= {entries = let
		       val entries 
			 = List.removeAll(entries,
					  fn {register = register',...} 
					   => Register.eq(register,register'))
		     in
		       value::entries
		     end,
	   reserved = reserved,
	   fltstack = fltstack}
      fun updates {values, registerAllocation: t}
	= List.fold(values,
		    registerAllocation,
		    fn (value, registerAllocation)
		     => update {value = value,
				registerAllocation = registerAllocation})

      fun fltupdate {value as {fltregister, ...},
		     registerAllocation as {entries, reserved, fltstack}: t}
	= {entries = entries,
	   reserved = reserved,
	   fltstack = let
			val rec fltupdate'
			  = fn [] => Error.bug "fltupdate"
			     | (value' as {fltregister = fltregister', ...})::l
			     => if FltRegister.eq(fltregister, fltregister')
				  then value::l
				  else value'::(fltupdate' l)
		      in
			fltupdate' fltstack
		      end}

      fun fltupdates {values, registerAllocation: t}
	= List.fold(values,
		    registerAllocation,
		    fn (value, registerAllocation)
		     => fltupdate {value = value,
				   registerAllocation = registerAllocation})

      fun delete {register,
		  registerAllocation as {entries, reserved, fltstack}: t}
	= {entries = List.removeAll(entries,
				    fn {register = register',...}
				     => Register.eq(register, register')),
	   reserved = reserved,
	   fltstack = fltstack}
      fun deletes {registers, registerAllocation: t}
	= List.fold(registers,
		    registerAllocation,
		    fn (register, registerAllocation)
		     => delete {register = register,
				registerAllocation = registerAllocation})

      fun fltpush {value,
		   registerAllocation as {entries, reserved, fltstack}: t}
	= {rename = FltRegister.push,
	   registerAllocation
	   = {entries = entries,
	      reserved = reserved,
	      fltstack = case #fltregister value
			   of FltRegister.T 0
			    => value::(List.map(fltstack,
						fn {fltregister 
						    as FltRegister.T i,
						    memloc,
						    weight,
						    sync,
						    commit}
					         => {fltregister =
						     FltRegister.T (i + 1),
						     memloc = memloc,
						     weight = weight,
						     sync = sync,
						     commit = commit}))
			    | _ => Error.bug "fltpush"}}

      fun fltpop {registerAllocation as {entries, reserved, fltstack}: t}
	= {rename = FltRegister.pop,
	   registerAllocation
	   = {entries = entries,
	      reserved = reserved,
	      fltstack = case fltstack
			   of [] => Error.bug "fltpop"
			    | value::fltstack
			    => List.map(fltstack,
					fn {fltregister as FltRegister.T i,
					    memloc,
					    weight,
					    sync,
					    commit}
					 => {fltregister 
					     = FltRegister.T (i - 1),
					     memloc = memloc,
					     weight = weight,
					     sync = sync,
					     commit = commit})}}

      fun fltxch' {fltregister: FltRegister.t,
		   registerAllocation as {entries, reserved, fltstack}: t}
	= let
	    val rec split
	      = fn (fltstack : fltvalue list, []) => Error.bug "fltxch': split"
	         | (fltstack_pre,value::fltstack_post)
	         => if FltRegister.eq(fltregister, #fltregister value)
		      then (List.rev fltstack_pre, value, fltstack_post)
		      else split (value::fltstack_pre, fltstack_post)

	    val (fltstack_pre,
		 {fltregister = fltregister', 
		  memloc = memloc', 
		  weight = weight', 
		  sync = sync', 
		  commit = commit'}, 
		 fltstack_post) = split ([], fltstack)
	  in 
	    {rename = fn fltregister 
	               => if FltRegister.eq(fltregister,
					    fltregister')
			    then FltRegister.top
			  else if FltRegister.eq(fltregister,
						 FltRegister.top)
			    then fltregister'
			  else fltregister,
	     registerAllocation 
	     = {entries = entries,
		reserved = reserved,
		fltstack = case fltstack_pre
			     of [] => Error.bug "fltxch'"
			      | (value as {fltregister,
					   memloc,
					   weight,
					   sync,
					   commit})::fltstack_pre
			      => ({fltregister = fltregister,
				   memloc = memloc',
				   weight = weight',
				   sync = sync',
				   commit = commit'})::
			         (List.concat
				  [fltstack_pre,
				   ({fltregister = fltregister',
				     memloc = memloc,
				     weight = weight,
				     sync = sync,
				     commit = commit})::
				   fltstack_post])}}
	  end

      fun fltxch {value: fltvalue, registerAllocation: t}
	= fltxch' {fltregister = #fltregister value,
		   registerAllocation = registerAllocation}

      fun fltxch1 {registerAllocation: t}
	= fltxch' {fltregister = FltRegister.one,
		   registerAllocation = registerAllocation}

      fun allocated {memloc, 
		     registerAllocation: t}
	= case valueFilter {filter = fn {memloc = memloc',...}
			              => MemLoc.eq(memloc,memloc'),
			    registerAllocation = registerAllocation}
	    of [] => NONE
	     | [value] => SOME value
	     | _ => Error.bug "allocated"

      fun fltallocated {memloc,
			registerAllocation: t}
	= case fltvalueFilter {filter = fn {memloc = memloc',...}
			                 => MemLoc.eq(memloc,memloc'),
			       registerAllocation = registerAllocation}
	    of [] => NONE
	     | [value] => SOME value
	     | _ => Error.bug "fltallocated"

      fun remove {memloc,
		  registerAllocation: t}
	= case allocated {memloc = memloc,
			  registerAllocation = registerAllocation}
	    of SOME {register, ...}
	     => delete {register = register,
			registerAllocation = registerAllocation}
	     | NONE => registerAllocation
      fun removes {memlocs,
		   registerAllocation: t}
	= List.fold(memlocs,
		    registerAllocation,
		    fn (memloc,registerAllocation)
		     => remove {memloc = memloc,
				registerAllocation = registerAllocation})

      local
	val commitPush' 
	  = fn NO => NO
	     | COMMIT i => COMMIT (i + 1)
	     | REMOVE i => REMOVE (i + 1)
	     | TRYCOMMIT i => TRYCOMMIT (i + 1)
	     | TRYREMOVE i => TRYREMOVE (i + 1)

	val commitPop' 
	  = fn NO => NO
	     | COMMIT i => COMMIT (i - 1)
	     | REMOVE i => REMOVE (i - 1)
	     | TRYCOMMIT i => TRYCOMMIT (i - 1)
	     | TRYREMOVE i => TRYREMOVE (i - 1)
      in
	fun commitPush {registerAllocation: t}
	  = valueMap {map = fn {register,memloc,weight,sync,commit}
		             => {register = register,
				 memloc = memloc,
				 weight = weight,
				 sync = sync,
				 commit = commitPush' commit},
		      registerAllocation = registerAllocation}

	fun commitPop {registerAllocation: t}
	  = valueMap {map = fn {register,memloc,weight,sync,commit}
		             => {register = register,
				 memloc = memloc,
				 weight = weight,
				 sync = sync,
				 commit = commitPop' commit},
		      registerAllocation = registerAllocation}
      end

      fun savedRegisters {saves: Operand.t list,
			  registerAllocation: t} :
                         Register.t list
	= List.concatMap
	  (saves,
	   fn Operand.MemLoc m
	    => (case allocated {memloc = m,
				registerAllocation = registerAllocation}
		  of SOME {register, ...} => [register]
		   | NONE => [])
	    | Operand.Register r => [r]
	    | Operand.Address (Address.T {base, index, ...})
	    => (case (base, index)
		  of (NONE,    NONE   ) => []
		   | (SOME rb, NONE   ) => [rb] 
		   | (NONE,    SOME ro) => [ro]
		   | (SOME rb, SOME ro) => [rb,ro])
	   | _ => [])

      fun supportedRegisters {supports: Operand.t list,
			      registerAllocation: t} :
	                     Register.t list
	= let
	    fun supportedRegisters' memloc
	      = case (allocated {memloc = memloc,
				 registerAllocation = registerAllocation},
		      fltallocated {memloc = memloc,
				    registerAllocation = registerAllocation})
		  of (SOME {register, ...}, _) => [register]
		   | (_, SOME _) => []
		   | (NONE, NONE) => List.concatMap(MemLoc.utilized memloc,
						    supportedRegisters')
	  in
	    List.concatMap
	    (supports,
	     fn Operand.MemLoc m => supportedRegisters' m
	      | _ => [])
	  end

      fun supportedMemLocs {supports: Operand.t list,
			    registerAllocation: t} :
	                   MemLoc.t list
	= let
	    fun supportedMemLocs' memloc
	      = case (allocated {memloc = memloc,
				 registerAllocation = registerAllocation},
		      fltallocated {memloc = memloc,
				    registerAllocation = registerAllocation})
		  of (SOME _, _) => [memloc]
		   | (_, SOME _) => [memloc]
		   | (NONE, NONE) => List.concatMap(MemLoc.utilized memloc,
						    supportedMemLocs')
	  in
	    List.concatMap
	    (supports,
	     fn Operand.MemLoc m => supportedMemLocs' m
	      | _ => [])
	  end

      fun fltsavedMemLocs {saves: Operand.t list,
			   registerAllocation: t} :
                          MemLoc.t list
	= List.keepAllMap
	  (saves,
	   fn Operand.MemLoc m
	    => (case fltallocated {memloc = m,
				   registerAllocation = registerAllocation}
		  of SOME _ => SOME m
		   | NONE => NONE)
	    | _ => NONE)

      fun fltsupportedMemLocs {supports: Operand.t list,
			       registerAllocation: t} :
	                      MemLoc.t list
	= List.keepAllMap
	  (supports,
	   fn Operand.MemLoc m
	    => (case fltallocated {memloc = m,
				   registerAllocation = registerAllocation}
		  of SOME _ => SOME m
		   | NONE => NONE)
	    | _ => NONE)

      fun potentialRegisters {size: Size.t,
			      saves: Operand.t list,
			      force: Register.t list,
			      registerAllocation: t} :
                             Register.t list
	= case force
	    of [] => Register.registers size
	     | registers => List.keepAll(Register.registers size,
					 fn register 
					  => List.contains(registers, 
							   register, 
							   Register.eq))

      fun chooseRegister {info as {future,hint,...}: Liveness.t,
			  memloc: MemLoc.t option,
			  size: Size.t,
			  supports: Operand.t list,
			  saves: Operand.t list,
			  force: Register.t list,
			  registerAllocation as {reserved,...}: t} :
	                 {register: Register.t,
			  coincide_values: value list}
        = let
	    val registers = potentialRegisters {size = size,
						saves = saves,
						force = force,
						registerAllocation
						= registerAllocation}

	    val saved
	      = savedRegisters {saves = saves,
				registerAllocation = registerAllocation}

	    val preserved
	      = let
		  fun doit(registers, preserved)
		    = List.fold
		      (registers,
		       preserved,
		       fn (register,preserved) 
		        => if List.contains(preserved,
					    register,
					    Register.eq)
			     then preserved
			     else register::preserved)
		in
		  doit(saved,
		  doit(reserved,
		       []))
		end

	    val registers 
	      = List.removeAll
	        (registers,
		 fn register'
		  => List.exists
		     (preserved,
		      fn register''
		       => Register.coincide(register',register'')))

	    val supported = supportedRegisters {supports = supports,
						registerAllocation
						= registerAllocation}

	    val values = valueFilter {filter = fn _ => true,
				      registerAllocation = registerAllocation}
	    val memlocs = List.map(values, #memloc)

	    val registers_costs
	      = List.map
	        (registers,
		 fn register'
		  => let
		       val hint_cost
			 = List.fold
			   (hint,
			    0,
			    fn ((hint_memloc,hint_register),hint_cost)
			     => if Register.eq(register',
					       hint_register)
				  then case memloc
					 of SOME memloc
					  => if MemLoc.eq(memloc,hint_memloc)
					       then hint_cost + 5
					       else hint_cost - 5
					  | NONE => hint_cost - 5
				else if Register.coincide(register',
							  hint_register)
				  then hint_cost - 5
				else hint_cost)

		       val values = valuesRegister {register = register',
						    registerAllocation 
						    = registerAllocation}
		       val (support_cost,
			    commit_cost,
			    future_cost,
			    utilized_cost,
			    sync_cost,
			    weight_cost)
			 = List.fold
			   (values,
			    (false,false,NONE,0,true,0),
			    fn ({register,memloc,weight,sync,commit,...},
				cost as (support_cost,
					 commit_cost,
					 future_cost,
					 utilized_cost,
					 sync_cost,
					 weight_cost))
			     => if Register.coincide(register,register')
				  then let
					 val support_cost'
					   = List.contains(supported,
							   register,
							   Register.eq)

					 val commit_cost'
					   = case commit
					       of TRYREMOVE _ => false
						| REMOVE _ => false
						| _ => true

					 val future_cost'
					   = List.index
					     (future,
					      fn Liveness.USE memloc'
					       => MemLoc.eq(memloc,memloc')
					       | Liveness.USEDEF memloc'
					       => MemLoc.eq(memloc,memloc')
					       | _ => false)

					 val utilized_cost'
					   = List.fold
					     (memlocs,
					      0,
					      fn (memloc',uc')
					       => List.fold
					          (MemLoc.utilized memloc',
						   0,
						   fn (memloc'',uc'')
						    => if MemLoc.eq
						          (memloc,
							   memloc'')
							 then uc'' + 1
							 else uc'') + uc')

					 val sync_cost' = sync

					 val weight_cost' = weight
				       in
					 (support_cost orelse support_cost',
					  commit_cost orelse support_cost',
					  case (future_cost,future_cost')
					    of (_, NONE) => future_cost
					     | (NONE, _) => future_cost'
					     | (SOME f,SOME f') 
					     => SOME (Int.min(f,f')),
					  utilized_cost + utilized_cost',
					  sync_cost andalso sync_cost',
					  weight_cost + weight_cost')
				       end
				  else cost)
		     in
		       (register',
			(support_cost,
			 hint_cost,
			 commit_cost,
			 future_cost,
			 utilized_cost,
			 sync_cost,
			 weight_cost))
		     end)

	    val registers_costs_sorted
	      = List.insertionSort
	        (registers_costs,
		 fn ((_,(support_c1,
			 hint_c1,
			 commit_c1,
			 future_c1,
			 utilized_c1,
			 sync_c1,
			 weight_c1)),
		     (_,(support_c2,
			 hint_c2,
			 commit_c2,
			 future_c2,
			 utilized_c2,
			 sync_c2,
			 weight_c2))) 
		  => bool_lt(support_c1,support_c2) orelse
		     (support_c1 = support_c2 andalso
		      (hint_c1 > hint_c2 orelse
		       (hint_c1 = hint_c2 andalso
			(bool_lt(commit_c1,commit_c2) orelse
			 (commit_c1 = commit_c2 andalso
			  (option_lt (op >) (future_c1, future_c2) orelse
			   (future_c1 = future_c2 andalso
			    (utilized_c1 < utilized_c2 orelse
			     (utilized_c1 = utilized_c2 andalso
			      (bool_gt(sync_c1,sync_c2) orelse
			       (sync_c1 = sync_c2 andalso
				weight_c1 < weight_c2))))))))))))

	    val registers
	      = List.map(registers_costs_sorted, #1)

	    val register
	      = case registers
		  of []
		   => raise Spill
(*
		   => let
			fun listToString(ss: string list): string
			  = "[" ^ (concat(List.separate(ss, ", "))) ^ "]"

			val size = Size.toString size
			val supports
			  = listToString(List.map(supports,Operand.toString))
			val saves 
			  = listToString(List.map(saves,Operand.toString)) 
			val force
			  = listToString(List.map(force,Register.toString)) 
			val reserved
			  = listToString(List.map(reserved,Register.toString)) 

			val msg = concat["\n",
					 "chooseRegister:\n",
					 (toString registerAllocation),
					 "size = ", size, "\n",
					 "saves = ", saves, "\n",
					 "force = ", force, "\n"]

			val _ = print msg
		      in
(*			Error.bug "chooseRegister, registers:[]" *)
			raise Spill
		      end
*)
		   | register::_ => register
			
	    val values = valuesRegister {register = register,
					 registerAllocation 
					 = registerAllocation}
	    val coincide_values
	      = List.keepAll(values,
			     fn value as {register = register',...}
			      => Register.coincide(register',register))
	  in
	    {register = register,
	     coincide_values = coincide_values}
	  end

      fun freeRegister {info: Liveness.t,
			memloc: MemLoc.t option,
			size: Size.t,
			supports: Operand.t list,
			saves: Operand.t list,
			force: Register.t list, 
			registerAllocation: t} :
                       {register: Register.t,
			assembly: Assembly.t list,
			registerAllocation: t}
	= let
	    val {register = final_register,
		 coincide_values}
	      = chooseRegister {info = info,
				memloc = memloc,
				size = size,
				supports = supports,
				saves = saves,
				force = force,
				registerAllocation = registerAllocation}

	    val supported = supportedMemLocs {supports = supports,
					      registerAllocation
					      = registerAllocation}

	    fun supportRemove memloc
	      = let
		  fun supportRemove' memlocs
		    = List.concatMap
		      (memlocs,
		       fn memloc' 
		        => if MemLoc.eq(memloc,memloc')
			     then []
			     else supportRemove' (MemLoc.utilized memloc'))
		in
		  List.fold
		  (supports,
		   [],
		   fn (Operand.MemLoc memloc', supports)
		    => List.concat [(supportRemove' [memloc']), supports]
		    | (_, supports) => supports)
		end

	    val {assembly = assembly_support,
		 registerAllocation}
	      = List.fold
	        (coincide_values,
		 {assembly = [],
		  registerAllocation = registerAllocation},
		 fn (value as {memloc,...},
		     {assembly,
		      registerAllocation})
		  => if List.contains(supported,
				      memloc,
				      MemLoc.eq)
		       then let
			      val supports = supportRemove memloc

			      val force
				= List.removeAll
				  (Register.registers (MemLoc.size memloc),
				   fn register'
				    => Register.coincide(final_register,
							 register'))
				  
			      val {register,
				   assembly = assembly_register,
				   registerAllocation}
				= toRegisterMemLoc
				  {memloc = memloc,
				   info = info,
				   size = MemLoc.size memloc,
				   move = true,
				   supports = supports,
				   saves = (Operand.register 
					    final_register)::saves,
				   force = force,
				   registerAllocation = registerAllocation}
			    in
			      {assembly = List.concat [assembly, assembly_register],
			       registerAllocation = registerAllocation}
			    end
		       else {assembly = assembly,
			     registerAllocation = registerAllocation})

	    val registerAllocation
	      = valueMap
	        {map = fn value as {register,
				    memloc,
				    weight,
				    sync,
				    commit}
		        => if Register.coincide(register,
						final_register)
			     then {register = register,
				   memloc = memloc,
				   weight = weight,
				   sync = sync,				   
				   commit = REMOVE 0}
			     else value,
		 registerAllocation = registerAllocation}

	    val {assembly = assembly_commit,
		 registerAllocation}
	      = commitRegisters {info = info,
				 supports = supports,
				 saves = saves,
				 registerAllocation = registerAllocation}
	  in
	    {register = final_register,
	     assembly = List.concat [assembly_support, assembly_commit],
	     registerAllocation = registerAllocation}
	  end

      and freeFltRegister {info as {future,...}: Liveness.t,
			   size: Size.t,
			   supports: Operand.t list,
			   saves: Operand.t list,
			   registerAllocation: t} :
	                  {assembly: Assembly.t list,
			   rename: FltRegister.t -> FltRegister.t,
			   registerAllocation: t}
	= let
	    val values
	      = fltvalueFilter {filter = fn _ => true,
				registerAllocation = registerAllocation}
	  in
	    if List.length values >= FltRegister.total
	      then let
		     val saved = fltsavedMemLocs {saves = saves,
						  registerAllocation 
						  = registerAllocation}

		     val supported = fltsupportedMemLocs {supports = supports,
							  registerAllocation
							  = registerAllocation}

		     val values
		       = List.removeAll(values,
					fn {memloc,...}
					 => List.contains(saved,
							  memloc,
							  MemLoc.eq))

		     val values_costs
		       = List.map
		         (values,
			  fn value as {memloc,weight,sync,commit,...}
			   => let
				val support_cost
				  = List.contains(supported,
						  memloc,
						  MemLoc.eq)

				val commit_cost
				  = case commit
				      of TRYREMOVE _ => false
				       | REMOVE _ => false
				       | _ => true

				val future_cost
				  = List.index(future,
					       fn Liveness.USE memloc'
					        => MemLoc.eq(memloc,memloc')
					        | Liveness.USEDEF memloc'
					        => MemLoc.eq(memloc,memloc')
					        | _ => false)

				val sync_cost = sync

				val weight_cost = weight
			      in
				(value,
				 (support_cost,
				  commit_cost,
				  future_cost,
				  sync_cost,
				  weight_cost))
			      end)

		     val values_costs_sorted
		       = List.insertionSort
		         (values_costs,
			  fn ((_,(support_c1,
				  commit_c1,
				  future_c1,
				  sync_c1,
				  weight_c1)),
			      (_,(support_c2,
				  commit_c2,
				  future_c2,
				  sync_c2,
				  weight_c2))) 
			   => bool_lt(support_c1,support_c2) orelse
			      (support_c1 = support_c2 andalso
			       (bool_lt(commit_c1,commit_c2) orelse
				(commit_c1 = commit_c2 andalso
				 (option_lt (op >) 
				            (future_c1, future_c2) orelse
				  (future_c1 = future_c2 andalso
				   (bool_gt(sync_c1,sync_c2) orelse
				    (sync_c1 = sync_c2 andalso
				     weight_c1 < weight_c2))))))))

		     val values = List.map(values_costs_sorted, #1)
		   in
		     case values
		       of [] => Error.bug "freeFltRegister"
			| {fltregister,
			   memloc,
			   weight,
			   sync,
			   commit}::_
			=> let
			     val registerAllocation
			       = fltupdate {value = {fltregister = fltregister,
						     memloc = memloc,
						     weight = weight,
						     sync = sync,
						     commit = REMOVE 0},
					    registerAllocation 
					    = registerAllocation}

			     val {assembly = assembly_commit,
				  rename = rename_commit,
				  registerAllocation}
			       = commitFltRegisters {info = info,
						     supports = supports,
						     saves = saves,
						     registerAllocation
						     = registerAllocation}
			   in
			     {assembly = assembly_commit,
			      rename = rename_commit,
			      registerAllocation = registerAllocation}
			   end
		   end
	      else {assembly = [],
		    rename = FltRegister.id,
		    registerAllocation = registerAllocation}
	  end

      and commitRegisters {info: Liveness.t,
			   supports: Operand.t list,
			   saves: Operand.t list,
			   registerAllocation as {reserved,...}: t} :
	                  {assembly: Assembly.t list,
			   registerAllocation: t}
	= let
	    val _ = Int.inc(commitDepth)
            val commit_values
	      = valueFilter {filter = fn {commit = COMMIT 0, ...} => true
			               | {commit = REMOVE 0, ...} => true
			               | {commit = TRYCOMMIT 0, ...} => true
			               | {commit = TRYREMOVE 0, ...} => true
			               | _ => false, 
			     registerAllocation = registerAllocation}

	    val commit_memlocs = List.map(commit_values, #memloc)

	    val commit_memlocs
	      = List.insertionSort
	        (commit_memlocs,
		 fn (memloc1,memloc2)
		  => List.contains(MemLoc.utilized memloc1,
				   memloc2,
				   MemLoc.eq))

	    val {assembly = assembly_commit,
		 registerAllocation}
	      = List.fold
	        (commit_memlocs,
		 {assembly = [],
		  registerAllocation = registerAllocation},
		 fn (memloc,
		     {assembly,
		      registerAllocation})
		  => (case allocated {memloc = memloc,
				      registerAllocation 
				      = registerAllocation}
			of NONE => {assembly = assembly,
				    registerAllocation = registerAllocation}
			 | SOME (value as {register,
					   memloc,
					   weight,
					   sync,
					   commit})
			 => let
			      fun doCommitFalse ()
				= let
				    val registerAllocation
				      = update {value = {register = register,
							 memloc = memloc,
							 weight = weight,
							 sync = true,
							 commit = NO},
						registerAllocation 
						= registerAllocation}

				    val registerAllocation
				      = commitPush {registerAllocation 
						    = registerAllocation}

				    val commit_saves 
				      = List.removeDuplicates
				        ((Operand.register register)::saves,
					 Operand.eq)

				    val size = Register.size register
				    val {address,
					 assembly = assembly_address,
					 registerAllocation}
				      = toAddressMemLoc {memloc = memloc,
							 info = info,
							 size = size,
							 supports = supports,
							 saves = commit_saves,
							 registerAllocation 
							 = registerAllocation}

				    val registerAllocation
				      = commitPop {registerAllocation 
						   = registerAllocation}
				  in
				    {assembly 
				     = List.concat 
				       [assembly,
					assembly_address,
					[Assembly.instruction_mov
					 {dst = Operand.Address address,
					  src = Operand.Register register,
					  size = size}]],
				     registerAllocation = registerAllocation}
				  end

			      fun doCommitTrue ()
				= let
				    val registerAllocation
				      = update {value = {register = register,
							 memloc = memloc,
							 weight = weight,
							 sync = true,
							 commit = NO},
						registerAllocation 
						= registerAllocation}
				  in
				    {assembly = assembly,
				     registerAllocation = registerAllocation}
				  end

			      fun doRemoveFalse ()
				= let
				    val registerAllocation
				      = update {value = {register = register,
							 memloc = memloc,
							 weight = weight,
							 sync = true,
							 commit = NO},
						registerAllocation 
						= registerAllocation}
				
				    val registerAllocation
				      = commitPush {registerAllocation 
						    = registerAllocation}
				      
				    val commit_saves 
				      = List.removeDuplicates
				        ((Operand.register register)::saves,
					 Operand.eq)

				    val size = Register.size register
				    val {address,
					 assembly = assembly_address,
					 registerAllocation}
				      = toAddressMemLoc {memloc = memloc,
							 info = info,
							 size = size,
							 supports = supports,
							 saves = commit_saves,
							 registerAllocation 
							 = registerAllocation}
				  
				    val registerAllocation
				      = commitPop {registerAllocation 
						   = registerAllocation}
				      
				    val registerAllocation
				      = if List.contains
				           (reserved,
					    register,
					    Register.eq)
					  then registerAllocation
					  else remove {memloc = memloc,
						       registerAllocation 
						       = registerAllocation}
				  in
				    {assembly 
				     = List.concat 
				       [assembly,
					assembly_address,
					[Assembly.instruction_mov
					 {dst = Operand.Address address,
					  src = Operand.Register register,
					  size = size}]],
				     registerAllocation = registerAllocation}
				  end

			      fun doRemoveTrue ()
				= let
				    val registerAllocation
				      = update {value = {register = register,
							 memloc = memloc,
							 weight = weight,
							 sync = true,
							 commit = NO},
						registerAllocation 
						= registerAllocation}
				      
				    val registerAllocation
				      = if List.contains
				           (reserved,
					    register,
					    Register.eq)
					  then registerAllocation
					  else remove {memloc = memloc,
						       registerAllocation 
						       = registerAllocation}
				  in
				    {assembly = assembly,
				     registerAllocation = registerAllocation}
				  end
			    in
			      case (commit,sync)
				of (COMMIT 0, false) => doCommitFalse ()
				 | (COMMIT 0, true) => doCommitTrue ()
				 | (REMOVE 0, false) => doRemoveFalse ()
				 | (REMOVE 0, true) => doRemoveTrue ()
				 | (TRYCOMMIT 0, false) => doCommitFalse ()
				 | (TRYCOMMIT 0, true) => doCommitTrue ()
				 | (TRYREMOVE 0, false) => doRemoveFalse ()
				 | (TRYREMOVE 0, true) => doRemoveTrue ()
				 | _ 
				 => Error.bug "commitRegisters"
			    end))
	    val _ = Int.dec(commitDepth)
	  in
	    {assembly = assembly_commit,
	     registerAllocation = registerAllocation}
	  end
	  handle Spill => (Int.dec(commitDepth);
			   spillRegisters {info = info,
					  supports = supports,
					  saves = saves,
					  registerAllocation = registerAllocation})

      and spillRegisters {info: Liveness.t,
			  supports: Operand.t list,
			  saves: Operand.t list,
			  registerAllocation} :
	                 {assembly: Assembly.t list,
			  registerAllocation: t}
	= if !commitDepth = 0
	    then let
	    val spillStart = !spill

	    val assembly_registerAllocationIn
	      = toComments registerAllocation

	    val {reserved, ...} = registerAllocation

	    val saved = savedRegisters {saves = saves,
					registerAllocation = registerAllocation}

	    val saved = List.fold
	                (reserved,
			 saved,
			 fn (register,saved)
			  => if List.contains(saved,register,Register.eq)
			       then saved
			       else register::saved)

	    val saves = valueFilter
	                {filter = fn {register, ...}
			           => List.contains(saved,
						    register,
						    Register.eq),
			 registerAllocation = registerAllocation}
			
	    val all = valueFilter 
	              {filter = fn _ => true,
		       registerAllocation = registerAllocation}

	    (* partition the values in the register file 
	     * by their base register.
	     *)
	    val groups = partition (all,
				    fn ({register = Register.T {reg = reg1, ...},...},
					{register = Register.T {reg = reg2, ...},...})
				     => reg1 = reg2)

	    (* order the groups by number of registers used
	     *)
	    val groups
	      = List.insertionSort
	        (groups, 
		 fn (g1,g2) => (List.length g1) < (List.length g2))

	    (* choose four registers to spill
	     *)
	    val spills
	      = case groups
		  of g1::g2::g3::g4::_ => List.concat [g1,g2,g3,g4]
		   | _ => Error.bug "spillRegisters"

	    (* totally order the spills by utilization
	     *)
	    val spills
	      = totalOrder
	        (spills,
		 fn ({memloc = memloc1, ...},
		     {memloc = memloc2, ...})
		  => List.contains(MemLoc.utilized memloc2,
				   memloc1,
				   MemLoc.eq))

	    fun mkReplacer (spillMap : (value * MemLoc.t) list)
	      = fn memloc'
	         => case List.peek(spillMap, fn ({memloc,...},_) 
				              => MemLoc.eq(memloc,memloc'))
		      of SOME (_,spillMemloc) => spillMemloc
		       | NONE => memloc'

	    (* associate each spilled value with a spill slot
	     *)
	    val (spillMap, spillEnd)
	      = List.fold
	        (spills,
		 ([], spillStart),
		 fn (value as {memloc, ...},
		     (spillMap, spillEnd))
		  => let	
		       val spillMemLoc
			 = MemLoc.imm {base = Immediate.label spillLabel,
				       index = Immediate.const_int spillEnd,
				       scale = Scale.Four,
				       size = MemLoc.size memloc,
				       commit = MemLoc.Commit.commit {isTemp = true,
								      onFlush = false},
				       class = spillClass}
		     in
		       ((value,spillMemLoc)::spillMap, 
			spillEnd + 1)
		     end)

	    val replacer = mkReplacer spillMap

	    (* commit everything in the register file;
	     * also replace all memlocs that are spilled with their spill slot
	     *)
	    val registerAllocation
	      = valueMap {map = fn {register, memloc, weight, sync, commit}
			         => if List.exists
			               (spillMap,
					fn ({memloc = memloc',...},_)
					 => MemLoc.eq(memloc,memloc'))
				      then {register = register,
					    memloc = replacer memloc,
					    weight = weight,
					    sync = false,
					    commit = NO}
				      else {register = register,
					    memloc = replacer memloc,
					    weight = weight,
					    sync = sync,
					    commit = case commit
						       of NO => COMMIT 0
							| COMMIT _ => COMMIT 0
							| TRYCOMMIT _ => COMMIT 0
							| REMOVE _ => REMOVE 0
							| TRYREMOVE _ => REMOVE 0},
			  registerAllocation = registerAllocation}

	    val assembly_registerAllocationSetup
	      = toComments registerAllocation

	    (* update next available spill slot for cascading spills *)
	    val _ = spill := spillEnd
	    (* commit everything;
	     * since the spilt memlocs look like they are spill slots,
	     * they can all be committed to memory without any additional
	     * registers.
	     *)
	    val {assembly = assembly_commit1,
		 registerAllocation = registerAllocation}
	      = commitRegisters
	        {info = info,
		 supports = [],
		 saves = [],
		 registerAllocation = registerAllocation}

	    val assembly_registerAllocationCommit1
	      = toComments registerAllocation

	    (* unspill; as we pull values in, we update the memloc to what it 
	     * looks under the pending unspills, and then replace any occurences 
	     * of the spill slot with the updated memloc;
	     * by the time we are done, everything should be mapped back to 
	     * its original form.
	     *)
	    val {assembly = assembly_unspill,
		 registerAllocation = registerAllocation}
	      = let
		  val rec doit
		    = fn ([],{assembly,registerAllocation}) 
		       => {assembly = assembly,
			   registerAllocation = registerAllocation}
		       | ((value as {memloc, weight, sync, commit, ...},
			   spillMemLoc)::spillMap,
			  {assembly, registerAllocation})
		       => let
			    val replacer = mkReplacer spillMap
			    val memloc' = replacer memloc
			      
			    val {register,
				 assembly = assembly_unspill,
				 registerAllocation}
			      = toRegisterMemLoc
			        {memloc = spillMemLoc,
				 info = info,
				 size = MemLoc.size memloc,
				 move = true,
				 supports = [],
				 saves = [],
				 force = [],
				 registerAllocation = registerAllocation}
			    val registerAllocation
			      = update {value = {register = register,
						 memloc = memloc',
						 weight = weight,
						 sync = sync,
						 commit 
						 = case commit
						     of NO => COMMIT 0
						      | COMMIT _ => COMMIT 0
						      | TRYCOMMIT _ => COMMIT 0
						      | REMOVE _ => REMOVE 0
						      | TRYREMOVE _ => REMOVE 0},
					registerAllocation = registerAllocation}

			    val registerAllocation
			      = valueMap
			        {map = fn {register,
					   memloc,
					   weight,
					   sync,
					   commit}
				        => {register = register,
					    memloc = MemLoc.replace
                                                     (fn memloc'' => if MemLoc.eq
						                        (memloc'',
									 spillMemLoc)
								       then memloc'
								       else memloc'')
                                                     memloc,
					    weight = weight,
					    sync = sync,
					    commit = commit},
				 registerAllocation = registerAllocation}

			  in
			    doit(spillMap,
				 {assembly = List.concat[assembly,
							 assembly_unspill],
				  registerAllocation = registerAllocation})
			  end
		in
		  doit(spillMap,
		       {assembly = [], 
			registerAllocation = registerAllocation})
		end
	    (* everything is unspilled *)
	    val _ = spill := spillStart

	    val assembly_registerAllocationUnspill
	      = toComments registerAllocation

	    (* commit all the memlocs that got spilt.
	     *)
	    val {assembly = assembly_commit2,
		 registerAllocation = registerAllocation}
	      = commitRegisters
	        {info = info,
		 supports = [],
		 saves = [],
		 registerAllocation = registerAllocation}
	    val _ = spill := spillStart

	    val assembly_registerAllocationCommit2
	      = toComments registerAllocation

	    (* restore the saved operands to their previous locations.
	     *)
	    val {assembly = assembly_restore,
		 registerAllocation}
	      = List.fold
	        (saves,
		 {assembly = [],
		  registerAllocation = registerAllocation},
		 fn (value as {register, memloc, weight, commit, ...},
		     {assembly, registerAllocation})
		  => let
		       val {assembly = assembly_register,
			    registerAllocation,
			    ...}
			 = toRegisterMemLoc
			   {memloc = memloc,
			    info = info,
			    size = Register.size register,
			    move = true,
			    supports = supports,
			    saves = [],
			    force = [register],
			    registerAllocation = registerAllocation}
		       val registerAllocation
			 = update {value = {register = register,
					    memloc = memloc,
					    weight = weight,
					    sync = true,
					    commit = commit},
				   registerAllocation = registerAllocation}
		       val {assembly = assembly_reserve,
			    registerAllocation}
			 = reserve {register = register,
				    registerAllocation = registerAllocation}
		     in
		       {assembly = List.concat [assembly,
						assembly_register,
						assembly_reserve],
			registerAllocation = registerAllocation}
		     end)
	    val {assembly = assembly_unreserve,
		 registerAllocation}
	      = List.fold
	        (saved,
		 {assembly = [],
		  registerAllocation = registerAllocation},
		 fn (register, 
		     {assembly, registerAllocation})
		  => let
		       val {assembly = assembly_unreserve,
			    registerAllocation}
			 = unreserve
			   {register = register,
			    registerAllocation = registerAllocation}
		     in
		       {assembly = assembly @ assembly_unreserve,
			registerAllocation = registerAllocation}
		     end)
	    val {assembly = assembly_reserve,
		 registerAllocation}
	      = List.fold
	        (reserved,
		 {assembly = [],
		  registerAllocation = registerAllocation},
		 fn (register, 
		     {assembly, registerAllocation})
		  => let
		       val {assembly = assembly_reserve,
			    registerAllocation}
			 = reserve
			   {register = register,
			    registerAllocation = registerAllocation}
		     in
		       {assembly = assembly @ assembly_reserve,
			registerAllocation = registerAllocation}
		     end)

	    val assembly_registerAllocationOut
	      = toComments registerAllocation
	  in
	    {assembly = List.concat [assembly_commit1,
				     assembly_unspill,
				     assembly_commit2,
				     assembly_restore,
				     assembly_unreserve,
				     assembly_reserve],
	     registerAllocation = registerAllocation}
	  end
	    else raise Spill

      and commitFltRegisters {info: Liveness.t,
			      supports: Operand.t list,
			      saves: Operand.t list,
			      registerAllocation: t} :
	                     {assembly: Assembly.t list,
			      rename: FltRegister.t -> FltRegister.t,
			      registerAllocation: t}
	= let
	    val commit_values
	      = fltvalueFilter {filter
				= fn {commit = COMMIT 0, ...} => true
			           | {commit = REMOVE 0, ...} => true
			           | {commit = TRYCOMMIT 0, ...} => true
			           | {commit = TRYREMOVE 0, ...} => true
				   | _ => false,
				registerAllocation = registerAllocation}

	    val {assembly = assembly_commit,
		 rename = rename_commit,
		 registerAllocation}
	      = List.fold
	        (commit_values,
		 {assembly = [],
		  rename = FltRegister.id,
		  registerAllocation = registerAllocation},
		 fn (value as {fltregister,
			       memloc,
			       weight,
			       sync,
			       commit},
		     {assembly, rename, registerAllocation})
		  => let
		       fun doCommitFalse ()
			 = let
			     val fltregister = rename fltregister
			     val {assembly = assembly_xch,
				  rename = rename_xch,
				  registerAllocation}
			       = if FltRegister.eq(fltregister, 
						   FltRegister.top)
				   then {assembly = [],
					 rename = FltRegister.id,
					 registerAllocation 
					 = registerAllocation}
				   else let
					  val {rename = rename_xch,
					       registerAllocation}
					    = fltxch' 
					      {fltregister = fltregister,
					       registerAllocation 
					       = registerAllocation}
					in
					  {assembly
					   = [Assembly.instruction_fxch
					      {src = Operand.fltregister 
					             fltregister}],
					   rename = rename_xch,
					   registerAllocation 
					   = registerAllocation}
					end

			     val size = MemLoc.size memloc
			       
			     val {address,
				  assembly = assembly_address,
				  registerAllocation}
			       = toAddressMemLoc {memloc = memloc,
						  info = info,
						  size = size,
						  supports = supports,
						  saves = saves,
						  registerAllocation 
						  = registerAllocation}
			       
			     val registerAllocation
			       = fltupdate {value 
					    = {fltregister = FltRegister.top,
					       memloc = memloc,
					       weight = weight,
					       sync = true,
					       commit = NO},
					    registerAllocation 
					    = registerAllocation}
			   in
			     {assembly 
			      = List.concat
			        [assembly,
				 assembly_xch,
				 assembly_address,
				 case Size.class size
				   of Size.FLT 
				    => [Assembly.instruction_fst
					{dst = Operand.Address address,
					 size = size,
					 pop = false}]
				    | Size.FPI 
				    => [Assembly.instruction_fist
					{dst = Operand.Address address,
					 size = size,
					 pop = false}]
				    | _ => Error.bug "commitFltRegisters"],
			      rename 
			      = rename_xch o rename,
			      registerAllocation  = registerAllocation}
			   end

		       fun doCommitTrue ()
			 = let
			     val fltregister = rename fltregister
			     val registerAllocation
			       = fltupdate 
			         {value = {fltregister = fltregister,
					   memloc = memloc,
					   weight = weight,
					   sync = true,
					   commit = NO},
				  registerAllocation = registerAllocation}
			   in
			     {assembly = assembly,
			      rename = rename,
			      registerAllocation = registerAllocation}
			   end

		       fun doRemoveFalse ()
			 = let
			     val fltregister = rename fltregister
			     val {assembly = assembly_xch,
				  rename = rename_xch,
				  registerAllocation}
			       = if FltRegister.eq(fltregister, 
						   FltRegister.top)
				   then {assembly = [],
					 rename = FltRegister.id,
					 registerAllocation 
					 = registerAllocation}
				   else let
					  val {rename = rename_xch,
					       registerAllocation}
					    = fltxch' 
					      {fltregister = fltregister,
					       registerAllocation 
					       = registerAllocation}
					in
					  {assembly
					   = [Assembly.instruction_fxch
					      {src = Operand.fltregister 
					             fltregister}],
					   rename = rename_xch,
					   registerAllocation 
					   = registerAllocation}
					end

			     val size = MemLoc.size memloc
			       
			     val {address,
				  assembly = assembly_address,
				  registerAllocation}
			       = toAddressMemLoc {memloc = memloc,
						  info = info,
						  size = size,
						  supports = supports,
						  saves = saves,
						  registerAllocation 
						  = registerAllocation}

			     val {rename = rename_pop,
				  registerAllocation}
			       = fltpop 
			         {registerAllocation = registerAllocation}
			   in
			     {assembly 
			      = List.concat
			        [assembly,
				 assembly_xch,
				 assembly_address,
				 case Size.class size
				   of Size.FLT 
				    => [Assembly.instruction_fst
					{dst = Operand.Address address,
					 size = size,
					 pop = true}]
				    | Size.FPI 
				    => [Assembly.instruction_fist
					{dst = Operand.Address address,
					 size = size,
					 pop = true}]
				    | _ => Error.bug "commitFltRegisters"],
			      rename 
			      = rename_pop o rename_xch o rename,
			      registerAllocation  = registerAllocation}
			   end

		       fun doRemoveTrue ()
			 = let
			     val fltregister = rename fltregister
			     val {assembly = assembly_xch,
				  rename = rename_xch,
				  registerAllocation}
			       = if FltRegister.eq(fltregister, 
						   FltRegister.top)
				   then {assembly = [],
					 rename = FltRegister.id,
					 registerAllocation 
					 = registerAllocation}
				   else let
					  val {rename = rename_xch,
					       registerAllocation}
					    = fltxch' 
					      {fltregister = fltregister,
					       registerAllocation 
					       = registerAllocation}
					in
					  {assembly 
					   = [Assembly.instruction_fxch
					      {src = Operand.fltregister 
					       fltregister}],
					   rename = rename_xch,
					   registerAllocation 
					   = registerAllocation}
					end
		     
			     val {rename = rename_pop,
				  registerAllocation}
			       = fltpop {registerAllocation 
					 = registerAllocation}

			     val size = MemLoc.size memloc
			   in
			     {assembly 
			      = List.concat
			        [assembly,
				 assembly_xch,
				 case Size.class size
				   of Size.FLT 
				    => [Assembly.instruction_fst
					{dst = Operand.fltregister 
					       FltRegister.top,
					 size = size,
					 pop = true}]
				  | Size.FPI 
				  => [Assembly.instruction_fst
				      {dst = Operand.fltregister
				             FltRegister.top,
				       size = Size.DBLE,
				       pop = true}]
				  | _ => Error.bug "commitFltRegisters"],
			      rename = rename_pop o rename_xch o rename,
			      registerAllocation  = registerAllocation}
			   end

		       fun doNothing ()
			 = {assembly = assembly,
			    rename = rename,
			    registerAllocation = registerAllocation}
		     in
		       case (commit,sync)
			 of (COMMIT 0, false) => doCommitFalse ()
			  | (COMMIT 0, true) => doCommitTrue ()
			  | (REMOVE 0, false) => doRemoveFalse ()
			  | (REMOVE 0, true) => doRemoveTrue ()
			  | (TRYCOMMIT 0, false)
			  => if FltRegister.eq(rename fltregister, 
					       FltRegister.top)
			       then doCommitFalse ()
			       else doNothing ()
			  | (TRYCOMMIT 0, true)
			  => if FltRegister.eq(rename fltregister, 
					       FltRegister.top)
			       then doCommitTrue ()
			       else doNothing ()
			  | (TRYREMOVE 0, false)
			  => if FltRegister.eq(rename fltregister, 
					       FltRegister.top)
			       then doRemoveFalse ()
			       else doNothing ()
			  | (TRYREMOVE 0, true)
			  => if FltRegister.eq(rename fltregister, 
					       FltRegister.top)
			       then doRemoveTrue ()
			       else doNothing ()
			  | _ => Error.bug "commitFltRegisters"
		     end)
	  in
	    {assembly = assembly_commit,
	     rename = rename_commit,
	     registerAllocation = registerAllocation}
	  end

      and toRegisterMemLoc {memloc: MemLoc.t, 
			    info: Liveness.t,
			    size: Size.t, 
			    move: bool,
			    supports: Operand.t list,
			    saves: Operand.t list, 
			    force: Register.t list, 
			    registerAllocation: t} :
	                   {register: Register.t,
			    assembly: Assembly.t list,
			    registerAllocation: t}
	= (case allocated {memloc = memloc,
			   registerAllocation = registerAllocation}
	     of SOME (value as {register,memloc,weight,sync,commit})
	      => let
		   val registers
		     = potentialRegisters {size = size,
					   saves = saves,
					   force = force,
					   registerAllocation
					   = registerAllocation}
		 in
		   if List.contains(registers, register, Register.eq)
		     then {register = register,
			   assembly = [],
			   registerAllocation = registerAllocation}
		     else let
			    val {register = final_register,
				 coincide_values}
			      = chooseRegister 
				{info = info,
				 memloc = SOME memloc,
				 size = size,
				 supports = supports,
				 saves = (Operand.register register)::saves,
				 force = force,
				 registerAllocation = registerAllocation}
			    val registerAllocation
			      = delete {register = register,
					registerAllocation
					= registerAllocation}
			  in
			    case coincide_values
			      of [] 
			       => if move
				    then let
					   val registerAllocation
					     = update {value 
						       = {register 
							  = final_register,
							  memloc = memloc,
							  weight = weight,
							  sync = sync,
							  commit = commit},
						       registerAllocation
						       = registerAllocation}
					 in
					   {register = final_register,
					    assembly
					    = [Assembly.instruction_mov
					       {src = Operand.register register,
						dst = Operand.register final_register,
						size = size}],
					    registerAllocation 
					    = registerAllocation}
					 end
				    else let
					   val registerAllocation
					     = update {value 
						       = {register 
							  = final_register,
							  memloc = memloc,
							  weight = weight,
							  sync = true,
							  commit = commit},
						       registerAllocation
						       = registerAllocation}
					 in
					   {register = final_register,
					    assembly = [],
					    registerAllocation
					    = registerAllocation}
					 end
			       | [value' as {register = register',
					     memloc = memloc',
					     weight = weight',
					     sync = sync',
					     commit = commit'}] 
			       => if Register.eq(register',final_register)
				    then let
					   val registerAllocation
					     = delete {register
						       = register',
						       registerAllocation
						       = registerAllocation}
					   val registerAllocation
					     = update {value
						       = {register 
							  = register,
							  memloc = memloc',
							  weight = weight',
							  sync = sync',
							  commit = commit'},
						       registerAllocation
						       = registerAllocation}
					 in
					   if move
					     then let
						    val registerAllocation
						      = update
							{value
							 = {register 
							    = final_register,
							    memloc = memloc,
							    weight = weight,
							    sync = sync,
							    commit = commit},
							 registerAllocation
							 = registerAllocation}
						  in
						    {register = final_register,
						     assembly 
						     = [Assembly.instruction_xchg
							{src = Operand.register register,
							 dst = Operand.register final_register,
							 size = size}],
						     registerAllocation
						     = registerAllocation}
						  end
					     else let
						    val registerAllocation
						      = update
							{value
							 = {register 
							    = final_register,
							    memloc = memloc,
							    weight = weight,
							    sync = true,
							    commit = commit},
							 registerAllocation
							 = registerAllocation}
						  in
						    {register = final_register,
						     assembly 
						     = [Assembly.instruction_mov
							{src = Operand.register final_register,
							 dst = Operand.register register,
							 size = size}],
						     registerAllocation
						     = registerAllocation}
						  end
					 end
				    else let
					   val {register = final_register,
						assembly = assembly_register,
						registerAllocation}
					     = freeRegister
					       {info = info,
						memloc = SOME memloc,
						size = size,
						supports = supports,
						saves = (Operand.register
							 register)::saves,
						force = force,
						registerAllocation
						= registerAllocation}
					   val registerAllocation
					     = remove 
					       {memloc = memloc,
						registerAllocation
						= registerAllocation}
					 in
					   if move
					     then let
						    val registerAllocation
						      = update {value
								= {register
								   = final_register,
								   memloc = memloc,
								   weight = weight,
								   sync = sync,
								   commit = commit},
								registerAllocation
								= registerAllocation}
						  in
						    {register = final_register,
						     assembly 
						     = List.concat
						       [assembly_register,
							[Assembly.instruction_mov
							 {src = Operand.register register,
							  dst = Operand.register final_register,
							  size = size}]],
						     registerAllocation
						     = registerAllocation}
						  end
					     else let
						    val registerAllocation
						      = update {value
								= {register
								   = final_register,
								   memloc = memloc,
								   weight = weight,
								   sync = true,
								   commit = commit},
								registerAllocation
								= registerAllocation}
						  in
						    {register = final_register,
						     assembly 
						     = assembly_register,
						     registerAllocation
						     = registerAllocation}
						  end
					 end
			       | coincide_values 
			       => let
				    val {register = final_register,
					 assembly = assembly_register,
					 registerAllocation}
				      = freeRegister {info = info,
						      memloc = SOME memloc,
						      size = size,
						      supports = supports,
						      saves = (Operand.register
							       register)::saves,
						      force = force,
						      registerAllocation
						      = registerAllocation}
				    val registerAllocation
				      = remove {memloc = memloc,
						registerAllocation
						= registerAllocation}
				  in
				    if move
				      then let
					     val registerAllocation
					       = update {value
							 = {register
							    = final_register,
							    memloc = memloc,
							    weight = weight,
							    sync = sync,
							    commit = commit},
							 registerAllocation
							 = registerAllocation}
					   in
					     {register = final_register,
					      assembly 
					      = List.concat
						[assembly_register,
						 [Assembly.instruction_mov
						  {src = Operand.register register,
						   dst = Operand.register final_register,
						   size = size}]],
					      registerAllocation
					      = registerAllocation}
					   end
				      else let
					     val registerAllocation
					       = update {value
							 = {register
							    = final_register,
							    memloc = memloc,
							    weight = weight,
							    sync = true,
							    commit = commit},
							 registerAllocation
							 = registerAllocation}
					   in
					     {register = final_register,
					      assembly 
					      = assembly_register,
					      registerAllocation
					      = registerAllocation}
					   end
				  end
			  end

		 end
	      | NONE 
	      => if move
		   then let
			  val {register, 
			       assembly = assembly_register,
			       registerAllocation}
			    = freeRegister {info = info,
					    memloc = SOME memloc,
					    size = size,
					    supports = (Operand.memloc memloc)::
						       supports,
					    saves = saves,
					    force = force,
					    registerAllocation 
					    = registerAllocation}

			  val {address, 
			       assembly = assembly_address,
			       registerAllocation}
			    = toAddressMemLoc {memloc = memloc,
					       info = info,
					       size = size,
					       supports = supports,
					       saves = (Operand.register register)::
						       saves,
					       registerAllocation 
					       = registerAllocation}

			  val registerAllocation
			    = remove {memloc = memloc,
				      registerAllocation = registerAllocation}

			  val registerAllocation
			    = update {value = {register = register,
					       memloc = memloc,
					       weight = 1024,
					       sync = true,
					       commit = NO},
				      registerAllocation = registerAllocation}
			in
			  {register = register,
			   assembly 
			   = List.concat [assembly_register,
					  assembly_address,
					  [Assembly.instruction_mov
					   {dst = Operand.register register,
					    src = Operand.address address,
					    size = size}]],
			   registerAllocation = registerAllocation}
			end
		   else let
			  val {register, 
			       assembly = assembly_register,
			       registerAllocation}
			    = freeRegister {info = info,
					    memloc = SOME memloc,
					    size = size,
					    supports = supports,
					    saves = saves,
					    force = force,
					    registerAllocation 
					    = registerAllocation}
			  val registerAllocation
			    = remove {memloc = memloc,
				      registerAllocation = registerAllocation}

			  val registerAllocation
			    = update {value = {register = register,
					       memloc = memloc,
					       weight = 1024,
					       sync = true,
					       commit = NO},
				      registerAllocation = registerAllocation}
			in
			  {register = register,
			   assembly = assembly_register,
			   registerAllocation = registerAllocation}
			end) 

      and toFltRegisterMemLoc {memloc: MemLoc.t,
			       info: Liveness.t,
			       size: Size.t,
			       move: bool,
			       supports: Operand.t list,
			       saves: Operand.t list, 
			       top: bool option,
			       registerAllocation: t} :
	                      {fltregister: FltRegister.t,
			       assembly: Assembly.t list,
			       rename : FltRegister.t -> FltRegister.t,
			       registerAllocation: t}
	= case fltallocated {memloc = memloc,
			     registerAllocation = registerAllocation}
	    of SOME (value as {fltregister,memloc,weight,sync,commit})
	     => (case (FltRegister.eq(fltregister, FltRegister.top),
		       top)
		   of (true, NONE)
		    => let
			 val {rename = rename_pop,
			      registerAllocation}
			   = fltpop {registerAllocation
				     = registerAllocation}
			 val assembly_pop
			   = [Assembly.instruction_fst
			      {dst = Operand.fltregister FltRegister.top,
			       size = size,
			       pop = true}]

			 val {rename = rename_push,
			      registerAllocation}
			   = fltpush {value = {fltregister = FltRegister.top,
					       memloc = memloc,
					       weight = weight,
					       sync = sync,
					       commit = commit},
				      registerAllocation = registerAllocation}
		       in
			 {fltregister = FltRegister.top,
			  assembly = assembly_pop,
			  rename = rename_pop,
			  registerAllocation = registerAllocation}
		       end
		    | (false, NONE)
		    => let
			 val {rename = rename_xch,
			      registerAllocation}
			   = fltxch {value = value,
				     registerAllocation 
				     = registerAllocation}
			 val assembly_xch
			   = [Assembly.instruction_fxch
			      {src = Operand.fltregister fltregister}]

			 val {rename = rename_pop,
			      registerAllocation}
			   = fltpop {registerAllocation
				     = registerAllocation}
			 val assembly_pop
			   = [Assembly.instruction_fst
			      {dst = Operand.fltregister FltRegister.top,
			       size = size,
			       pop = true}]

			 val {rename = rename_push,
			      registerAllocation}
			   = fltpush {value = {fltregister = FltRegister.top,
					       memloc = memloc,
					       weight = weight,
					       sync = sync,
					       commit = commit},
				      registerAllocation = registerAllocation}
		       in
			 {fltregister = FltRegister.top,
			  assembly = List.concat [assembly_xch, assembly_pop],
			  rename = rename_pop o rename_xch,
			  registerAllocation = registerAllocation}
		       end
		    | (false, SOME true)
		    => let
			 val {rename = rename_xch,
			      registerAllocation}
			   = fltxch {value = value,
				     registerAllocation 
				     = registerAllocation}
			 val assembly_xch
			   = [Assembly.instruction_fxch
			      {src = Operand.fltregister fltregister}]
		       in
			 {fltregister = FltRegister.top,
			  assembly = assembly_xch,
			  rename = rename_xch,
			  registerAllocation = registerAllocation}
		       end
		    | (_, SOME _)
		    => {fltregister = fltregister,
			assembly = [],
			rename = FltRegister.id,
			registerAllocation = registerAllocation})
	     | NONE
	     => (case (top, move)
		   of (NONE, _)
		    => let
			 val {assembly = assembly_free,
			      rename = rename_free,
			      registerAllocation
			      = registerAllocation}
			   = freeFltRegister {info = info,
					      size = size,
					      supports = supports,
					      saves = saves,
					      registerAllocation
					      = registerAllocation}

			 val {rename = rename_push,
			      registerAllocation}
			   = fltpush {value = {fltregister = FltRegister.top,
					       memloc = memloc,
					       weight = 1024,
					       sync = true,
					       commit = NO},
				      registerAllocation = registerAllocation}
		       in
			 {fltregister = FltRegister.top,
			  assembly = assembly_free,
			  rename = rename_free,
			  registerAllocation = registerAllocation}
		       end
		    | (SOME _, true)
		    => let
			 val {assembly = assembly_free,
			      rename = rename_free,
			      registerAllocation
			      = registerAllocation}
			   = freeFltRegister {info = info,
					      size = size,
					      supports = supports,
					      saves = saves,
					      registerAllocation
					      = registerAllocation}

			 val {address,
			      assembly = assembly_address,
			      registerAllocation}
			   = toAddressMemLoc {memloc = memloc,
					      info = info,
					      size = size,
					      supports = supports,
					      saves = saves,
					      registerAllocation 
					      = registerAllocation}

			 val {rename = rename_push,
			      registerAllocation}
			   = fltpush {value = {fltregister = FltRegister.top,
					       memloc = memloc,
					       weight = 1024,
					       sync = true,
					       commit = NO},
				      registerAllocation = registerAllocation}

			 val assembly_load
			   = case Size.class size
			       of Size.FLT
				=> Assembly.instruction_fld
				   {src = Operand.address address,
				    size = size}
				| Size.FPI
				=> Assembly.instruction_fild
				   {src = Operand.address address,
				    size = size}
				| _ 
				=> Error.bug "toFltRegisterMemLoc, size"
		       in
			 {fltregister = FltRegister.top,
			  assembly = List.concat
			             [assembly_free,
				      assembly_address,
				      [assembly_load]],
			  rename = rename_push o rename_free,
			  registerAllocation = registerAllocation}
		       end
		    | (SOME _, false)
		    => Error.bug "toFltRegisterMemLoc: (top, move)")

      and toAddressMemLoc {memloc: MemLoc.t, 
			   info: Liveness.t,
			   size: Size.t, 
			   supports: Operand.t list,
			   saves: Operand.t list, 
			   registerAllocation: t} :
	                  {address: Address.t,
			   assembly: Assembly.t list,
			   registerAllocation: t}
        = (case memloc
	     of MemLoc.T {base = MemLoc.Imm base, index = MemLoc.Imm index,
			  scale, size, ...}
	      => let
		   val disp' 
		     = if Immediate.eq(index, Immediate.const_int 0)
			 then NONE
			 else SOME (Immediate.binexp 
				    {oper = Immediate.Multiplication, 
				     exp1 = index, 
				     exp2 = Scale.toImmediate scale})
		   val disp
		     = case disp'
			 of NONE => SOME base
			  | SOME disp' => SOME (Immediate.binexp 
						{oper = Immediate.Addition,
						 exp1 = base, 
						 exp2 = disp'})
		 in
		   {address = Address.T {disp = disp,
					 base = NONE,
					 index = NONE,
					 scale = NONE},
		    assembly = [],
		    registerAllocation = registerAllocation}
		 end
	      | MemLoc.T {base = MemLoc.Imm base, index = MemLoc.Mem index,
			  scale, size, ...}
	      => let
		   val disp = SOME base

		   val {register = register_index,
			assembly = assembly_index,
			registerAllocation}
		     = toRegisterMemLoc {memloc = index,
					 info = info,
					 size = MemLoc.size index,
					 move = true,
					 supports = supports,
					 saves = saves,
					 force = Register.indexRegisters,
					 registerAllocation 
					 = registerAllocation}
		 in
		   {address = Address.T {disp = disp,
					 base = NONE,
					 index = SOME register_index,
					 scale = SOME scale},
		    assembly = assembly_index,
		    registerAllocation = registerAllocation}
		 end
	      | MemLoc.T {base = MemLoc.Mem base, index = MemLoc.Imm index,
			  scale, size, ...}
	      => let
		   val disp
		     = if Immediate.eq(index, Immediate.const_int 0)
			 then NONE
			 else SOME (Immediate.binexp 
				    {oper = Immediate.Multiplication,
				     exp1 = index, 
				     exp2 = Scale.toImmediate scale})

		   val {register = register_base,
			assembly = assembly_base,
			registerAllocation}
		     = toRegisterMemLoc {memloc = base,
					 info = info,
					 size = MemLoc.size base,
					 move = true,
					 supports = supports,
					 saves = saves,
					 force = Register.baseRegisters,
					 registerAllocation 
					 = registerAllocation}
		 in
		   {address = Address.T {disp = disp,
					 base = SOME register_base,
					 index = NONE,
					 scale = NONE},
		    assembly = assembly_base,
		    registerAllocation = registerAllocation}
		 end
	      | MemLoc.T {base = MemLoc.Mem base, index = MemLoc.Mem index,
			  scale, size, ...}
	      => let
		   val {register = register_base,
			assembly = assembly_base,
			registerAllocation}
		     = toRegisterMemLoc {memloc = base,
					 info = info,
					 size = MemLoc.size base,
					 move = true,
					 supports 
					 = (Operand.memloc index)::supports,
					 saves = saves,
					 force = Register.baseRegisters,
					 registerAllocation 
					 = registerAllocation}

		   val {register = register_index,
			assembly = assembly_index,
			registerAllocation}
		     = toRegisterMemLoc {memloc = index,
					 info = info,
					 size = MemLoc.size index,
					 move = true,
					 supports = supports,
					 saves = (Operand.memloc base)::
						 (Operand.register 
						  register_base)::
						 saves,
					 force = Register.indexRegisters,
					 registerAllocation 
					 = registerAllocation}
		 in
		   {address = Address.T {disp = NONE,
					 base = SOME register_base,
					 index = SOME register_index,
					 scale = SOME scale},
		    assembly = List.concat [assembly_base, assembly_index],
		    registerAllocation = registerAllocation}
		 end) 

      fun toRegisterImmediate {immediate: Immediate.t,
			       info: Liveness.t, 
			       size: Size.t,
			       supports: Operand.t list,
			       saves: Operand.t list,
			       force: Register.t list,
			       registerAllocation: t} :
	                      {register: Register.t,
			       assembly: Assembly.t list,
			       registerAllocation: t}
	= let
	    val {register = final_register, assembly, registerAllocation}
	      = freeRegister {info = info,
			      memloc = NONE,
			      size = size,
			      supports = supports,
			      saves = saves,
			      force = force,
			      registerAllocation = registerAllocation}
	  in
	    {register = final_register,
	     assembly = List.concat
	                [assembly,
			 [Assembly.instruction_mov
			  {dst = Operand.Register final_register,
			   src = Operand.Immediate immediate,
			   size = size}]],
	     registerAllocation = registerAllocation}
	  end

      fun pre {uses: Operand.t list,
	       defs: Operand.t list,
	       kills: Operand.t list,
	       info as {dead,
			commit,
			remove,
			...}: Liveness.t,
	       registerAllocation: t} :
	      {assembly: Assembly.t list,
	       registerAllocation: t}
	= let
	    val (allUses, allDefs, allKills)
	      = let
		  fun doit(operands, all)
		    = List.fold
		      (operands,
		       all,
		       fn (operand,all)
		        => case Operand.deMemloc operand
			     of SOME memloc 
			      => if List.contains(all,
						  memloc,
						  MemLoc.eq)
				   then all
				   else memloc::all
			      | NONE => all)
		  val uses = doit(uses, [])
		  val defs = doit(defs, [])
		  val kills = doit(kills, [])

		  val allUses
		    = let
			fun doit(memlocs, all)
			  = List.fold
			    (memlocs,
			     all,
			     fn (memloc,all)
			      => if List.contains(all,
						  memloc,
						  MemLoc.eq)
				   then all
				   else memloc::all)
			fun doit'(memlocs, all)
			  = List.fold
			    (memlocs,
			     all,
			     fn (memloc,all)
			      => doit(MemLoc.utilized memloc, all))
		      in
			doit'(uses,
			doit'(defs,
			      uses))
		      end
		  val allDefs = defs
		  val allKills = kills
		in
		  (allUses, allDefs, allKills)
		end

	    val registerAllocation
	      = fltvalueMap 
	        {map = fn value as {fltregister,
				    memloc,
				    weight,
				    sync,
				    commit}
		        => if List.contains
		              (allKills,
			       memloc,
			       MemLoc.eq)
			     then if List.contains
			             (dead,
				      memloc,
				      MemLoc.eq)
				    then {fltregister = fltregister,
					  memloc = memloc,
					  weight = weight,
					  sync = true,
					  commit = NO}
				    else {fltregister = fltregister,
					  memloc = memloc,
					  weight = weight,
					  sync = sync,
					  commit = COMMIT 0}
			     else if not (List.contains(allDefs,
							memloc,
							MemLoc.eq))
			             andalso
				     List.exists
				     (memloc::(MemLoc.utilized memloc),
				      fn memloc'
				       => List.exists
				          (allDefs,
					   fn memloc''
					    => MemLoc.mayAlias(memloc',
							       memloc'')))
				    then {fltregister = fltregister,
					  memloc = memloc,
					  weight = weight,
					  sync = sync,
					  commit = REMOVE 0}
			     else if not (List.contains(allUses,
							memloc,
							MemLoc.eq))
			             andalso 
				     List.exists
				     (allUses,
				      fn memloc'
				       => MemLoc.mayAlias(memloc,
							  memloc'))
				    then {fltregister = fltregister,
					  memloc = memloc,
					  weight = weight,
					  sync = sync,
					  commit = case commit
						     of TRYREMOVE _ => REMOVE 0
						      | REMOVE _ => REMOVE 0
						      | _ => COMMIT 0}
				    else value,
		 registerAllocation = registerAllocation}

	    val {assembly = assembly_commit_fltregisters,
		 rename = rename_commit_fltregisters,
		 registerAllocation}
	      = commitFltRegisters {info = info,
				    supports = [],
				    saves = [],
				    registerAllocation = registerAllocation}

	    val registerAllocation
	      = valueMap 
	        {map = fn value as {register,
				    memloc,
				    weight,
				    sync,
				    commit}
		        => if List.contains
		              (allKills,
			       memloc,
			       MemLoc.eq)
			     then if List.contains
			             (dead,
				      memloc,
				      MemLoc.eq)
				    then {register = register,
					  memloc = memloc,
					  weight = weight,
					  sync = true,
					  commit = NO}
				    else {register = register,
					  memloc = memloc,
					  weight = weight,
					  sync = sync,
					  commit = COMMIT 0}
			     else if (not (List.contains(allUses,
							 memloc,
							 MemLoc.eq))
				      andalso 
				      List.exists
				      (allUses,
				       fn memloc'
				        => MemLoc.mayAlias(memloc,
							   memloc')))
			             orelse
				     (not (List.contains(allDefs,
							 memloc,
							 MemLoc.eq))
				      andalso
				      List.exists
				      (memloc::(MemLoc.utilized memloc),
				       fn memloc'
				        => List.exists
				           (allDefs,
					    fn memloc''
					     => MemLoc.mayAlias(memloc',
								memloc''))))
				    then {register = register,
					  memloc = memloc,
					  weight = weight,
					  sync = sync,
					  commit = COMMIT 0}
				    else value,
		 registerAllocation = registerAllocation}

	    val {assembly = assembly_commit_registers,
		 registerAllocation}
	      = commitRegisters {info = info,
				 supports = [],
				 saves = [],
				 registerAllocation = registerAllocation}
	  in
(*
	    {assembly = if not (List.isEmpty assembly_commit_fltregisters)
	                   orelse
			   not (List.isEmpty assembly_commit_registers)
			  then (print "pre non-empty\n";
				[Assembly.comment "pre begin:"] @
				assembly_commit_fltregisters @
				assembly_commit_registers @
				[Assembly.comment "pre end:"])
			  else assembly_commit_fltregisters @
			       assembly_commit_registers,
*)
	    {assembly = List.concat
	                [assembly_commit_fltregisters,
			 assembly_commit_registers],
	     registerAllocation = registerAllocation}
	  end

      val (pre, pre_msg)
	= tracer
	  "pre"
	  pre

      fun post {uses: Operand.t list,
		final_uses: Operand.t list,
		defs: Operand.t list,
		final_defs: Operand.t list,
		info as {dead,
			 commit,
			 remove,
			 ...}: Liveness.t,
		registerAllocation: t} :
	       {assembly: Assembly.t list,
		registerAllocation: t}
	= let 
	    val (final_uses_registers,
		 final_defs_registers,
		 final_uses_fltregisters,
		 final_defs_fltregisters)
	      = let
		  fun doit(operands, (final_registers, final_fltregisters))
		    = List.fold
		      (operands,
		       (final_registers, final_fltregisters),
		       fn (operand, (final_registers, final_fltregisters))
		        => case (Operand.deRegister operand,
				 Operand.deFltregister operand)
			     of (SOME register, _) 
			      => if List.contains(final_registers,
						  register,
						  Register.eq)
				   then (final_registers,
					 final_fltregisters)
				   else (register::final_registers,
					 final_fltregisters)
			      | (_, SOME fltregister)
			      => if List.contains(final_fltregisters,
						  fltregister,
						  FltRegister.eq)
				   then (final_registers,
					 final_fltregisters)
				   else (final_registers,
					 fltregister::final_fltregisters)
			      | _ => (final_registers, final_fltregisters))
		  val (final_uses_registers, final_uses_fltregisters)
		    = doit(final_uses, ([], []))
		  val (final_defs_registers, final_defs_fltregisters)
		    = doit(final_defs, ([], []))
		in
		  (final_uses_registers,
		   final_defs_registers,
		   final_uses_fltregisters,
		   final_defs_fltregisters)
		end

	    val (allUses, allDefs)
	      = let
		  fun doit(operands, all)
		    = List.fold
		      (operands,
		       all,
		       fn (operand,all)
		        => case Operand.deMemloc operand
			     of SOME memloc 
			      => if List.contains(all,
						  memloc,
						  MemLoc.eq)
				   then all
				   else memloc::all
			      | NONE => all)
		  val uses = doit(uses, [])
		  val defs = doit(defs, [])

		  val allUses
		    = let
			fun doit(memlocs, all)
			  = List.fold
			    (memlocs,
			     all,
			     fn (memloc,all)
			      => if List.contains(all,
						  memloc,
						  MemLoc.eq)
				   then all
				   else memloc::all)
			fun doit'(memlocs, all)
			  = List.fold
			    (memlocs,
			     all,
			     fn (memloc,all)
			      => doit(MemLoc.utilized memloc, all))
		      in
			doit'(uses,
			doit'(defs,
			      uses))
		      end
		  val allDefs = defs
		in
		  (allUses, allDefs)
		end

	    val commit_memlocs 
	      = List.keepAll(commit, fn memloc 
			              => MemLoc.onFlush memloc
			                 orelse
					 MemLoc.isTemp memloc)
	    val remove_memlocs 
	      = List.keepAll(remove, fn memloc 
			              => MemLoc.onFlush memloc
			                 orelse
					 MemLoc.isTemp memloc)

	    val registerAllocation
	      = fltvalueMap 
	        {map = fn value as {fltregister,
				    memloc,
				    weight,
				    sync,
				    commit}
		        => if List.contains(dead,
					    memloc,
					    MemLoc.eq)
			     then {fltregister = fltregister,
				   memloc = memloc,
				   sync = true,
				   weight = weight - 500,
				   commit = TRYREMOVE 0}
			     else let
				    val isSrc
				      = List.contains
				        (final_uses_fltregisters,
					 fltregister,
					 FltRegister.eq)

				    val isDst
				      = List.contains
				        (final_defs_fltregisters,
					 fltregister,
					 FltRegister.eq)
			       
				    val isDef = isDst
				  in
				    {fltregister = fltregister,
				     memloc = memloc,
				     weight = weight - 5
				              + (if isSrc
						   then 5
						   else 0)
				              + (if isDst
						   then 10
						   else 0),
				     sync = sync andalso (not isDef),
				     commit = if !Control.Native.IEEEFP
				                 andalso
						 not (sync andalso (not isDef))
						then REMOVE 0
						else if List.contains
				                        (remove_memlocs,
							 memloc,
							 MemLoc.eq)
						       then TRYREMOVE 0
						     else if List.contains
					                     (commit_memlocs,
							      memloc,
							      MemLoc.eq)
						       then TRYCOMMIT 0
						     else commit}
				  end,
		  registerAllocation = registerAllocation}

	    val {assembly = assembly_commit_fltregisters,
		 rename = rename_commit_fltregisters,
		 registerAllocation}
	      = commitFltRegisters {info = info,
				    supports = [],
				    saves = [],
				    registerAllocation = registerAllocation}

	    val registerAllocation
	      = valueMap 
	        {map = fn value as {register,
				    memloc,
				    weight,
				    sync,
				    commit}
		        => if not (List.contains(allDefs,
						 memloc,
						 MemLoc.eq))
		              andalso
			      List.exists
			      (memloc::(MemLoc.utilized memloc),
			       fn memloc'
			        => List.exists
			           (allDefs,
				    fn memloc''
				     => MemLoc.mayAlias(memloc',
							memloc'')))
			     then {register = register,
				   memloc = memloc,
				   weight = weight,
				   sync = sync,
				   commit = REMOVE 0}
			   else if List.contains(dead,
						 memloc,
						 MemLoc.eq)
			     then value
			   else let
				  val isSrc
				    = List.exists
				      (final_uses_registers,
				       fn register'
				        => Register.coincide(register,
							     register'))

				  val isDst
				    = List.exists
				      (final_defs_registers,
				       fn register'
				        => Register.coincide(register,
							     register'))
			       
				  val isDef = isDst
				in
				  {register = register,
				   memloc = memloc,
				   weight = weight - 5
				            + (if isSrc
						 then 5
						 else 0)
				            + (if isDst
						 then 10
						 else 0),
				   sync = sync andalso (not isDef),
				   commit = if List.contains
				               (remove_memlocs,
						memloc,
						MemLoc.eq)
					      then TRYREMOVE 0
					    else if List.contains
					            (commit_memlocs,
						     memloc,
						     MemLoc.eq)
					      then TRYCOMMIT 0
					    else commit}
				end,
		  registerAllocation = registerAllocation}

	    val {assembly = assembly_commit_registers,
		 registerAllocation}
	      = commitRegisters {info = info,
				 supports = [],
				 saves = [],
				 registerAllocation = registerAllocation}

	    val registerAllocation
	      = valueMap 
	        {map = fn value as {register,
				    memloc,
				    weight,
				    sync,
				    commit}
		        => if List.contains(dead,
					    memloc,
					    MemLoc.eq)
			     then {register = register,
				   memloc = memloc,
				   sync = true,
				   weight = weight,
				   commit = REMOVE 0}
			     else value,
		 registerAllocation = registerAllocation}

	    val {assembly = assembly_dead_registers,
		 registerAllocation}
	      = commitRegisters {info = info,
				 supports = [],
				 saves = [],
				 registerAllocation = registerAllocation}
	  in
	    {assembly = List.concat
	                [assembly_commit_fltregisters,
			 assembly_commit_registers,
			 assembly_dead_registers],
	     registerAllocation = registerAllocation}
	  end

      val (post, post_msg)
	= tracer
	  "post"
	  post

      fun allocateOperand {operand: Operand.t,
			   options = {register: bool,
				      immediate: bool,
				      label: bool,
				      address: bool},
			   info as {dead, 
				    commit,
				    remove,
				    ...}: Liveness.t,
			   size: Size.t,
			   move: bool,
			   supports: Operand.t list,
			   saves: Operand.t list,
			   force: Register.t list,
			   registerAllocation: t} :
	                  {operand: Operand.t,
			   assembly: Assembly.t list,
			   registerAllocation: t}
	= case operand
	    of Operand.Immediate i
	     => if immediate
		  then {operand = operand,
			assembly = [],
			registerAllocation = registerAllocation}
		else if register
		  then let
			 val {register, 
			      assembly, 
			      registerAllocation}
			   = toRegisterImmediate {immediate = i,
						  info = info,
						  size = size,
						  supports = supports,
						  saves = saves,
						  force = force,
						  registerAllocation
						  = registerAllocation}
		       in
			 {operand = Operand.register register,
			  assembly = assembly,
			  registerAllocation = registerAllocation}
		       end
		else if address
		  then let
			 val address 
			   = Address.T 
			     {disp = SOME (Immediate.label (Label.fromString "raTemp")),
			      base = NONE,
			      index = NONE,
			      scale = NONE}
		       in 
			 {operand = Operand.address address,
			  assembly = [Assembly.instruction_mov
				      {src = Operand.immediate i,
				       dst = Operand.address address,
				       size = size}],
			  registerAllocation = registerAllocation}
		       end 
	       else Error.bug "allocateOperand: operand:Immediate"
	     | Operand.Label l
	     => if label
		  then {operand = operand,
			assembly = [],
			registerAllocation = registerAllocation}
		else if immediate
		  then {operand = Operand.immediate_label l,
			assembly = [],
			registerAllocation = registerAllocation}
		else if register
		  then let
			 val {register, 
			      assembly, 
			      registerAllocation}
			   = toRegisterImmediate {immediate 
						  = Immediate.label l,
						  info = info,
						  size = size,
						  supports = supports,
						  saves = saves,
						  force = force,
						  registerAllocation
						  = registerAllocation}
		       in
			 {operand = Operand.register register,
			  assembly = assembly,
			  registerAllocation = registerAllocation}
		       end
		else Error.bug "allocateOperand: operand:Label"
	     | Operand.MemLoc m
	     => let
		  fun toRegisterMemLoc' ()
		    = let
			val {register, 
			     assembly, 
			     registerAllocation}
			  = toRegisterMemLoc 
			    {memloc = m,
			     info = info,
			     size = size,
			     move = move,
			     supports = supports,
			     saves = saves,
			     force = force,
			     registerAllocation = registerAllocation}
		      in
			{operand = Operand.Register register,
			 assembly = assembly,
			 registerAllocation = registerAllocation}
		      end
		  fun toAddressMemLoc' ()
		    = let
			val {address, 
			     assembly, 
			     registerAllocation}
			  = toAddressMemLoc 
			    {memloc = m,
			     info = info,
			     size = size,
			     supports = supports,
			     saves = saves,
			     registerAllocation
			     = registerAllocation}
		      in
			{operand = Operand.Address address,
			 assembly = assembly,
			 registerAllocation = registerAllocation}
		      end
		  fun toAddressMemLocRemove' ()
		    = let
			val registerAllocation
			  = valueMap {map 
				      = fn value as {register,
						     memloc,
						     weight,
						     sync,
						     commit}
				         => if MemLoc.eq(memloc, m)
					      then {register = register, 
						    memloc = memloc, 
						    weight = weight, 
						    sync = sync, 
						    commit = REMOVE 0}
					      else value,
				      registerAllocation = registerAllocation}
		     
			val {assembly = assembly_commit,
			     registerAllocation}
			  = commitRegisters {info = info,
					     supports = supports,
					     saves = saves,
					     registerAllocation 
					     = registerAllocation}
			  
			val {address, assembly, registerAllocation}
			  = toAddressMemLoc {memloc = m,
					     info = info,
					     size = size,
					     supports = supports,
					     saves = saves,
					     registerAllocation
					     = registerAllocation}
		      in
			{operand = Operand.Address address,
			 assembly = assembly_commit @ assembly,
			 registerAllocation = registerAllocation}
		      end
		in 
		  if register andalso address
		    then case allocated {memloc = m,
					 registerAllocation 
					 = registerAllocation}
			   of NONE 
			    => if List.contains(dead,
						m,
						MemLoc.eq)
			          orelse
				  List.contains(remove,
						m,
						MemLoc.eq)
				 then toAddressMemLoc' ()
				 else toRegisterMemLoc' ()
		            | SOME _
			    => toRegisterMemLoc' ()
		  else if register
		     then toRegisterMemLoc' ()
		  else if address
		     then toAddressMemLocRemove' ()
		  else Error.bug "allocateOperand: operand:MemLoc"
		end
	     | _ => Error.bug "allocateOperand: operand"

      val (allocateOperand, allocateOperand_msg)
	= tracer
	  "allocateOperand"
	  allocateOperand

      fun allocateFltOperand {operand: Operand.t,
			      options = {fltregister: bool,
					 address: bool},
			      info as {dead, 
				       commit,
				       remove,
				       ...}: Liveness.t,
			      size: Size.t,
			      move: bool,
			      supports: Operand.t list,
			      saves: Operand.t list,
			      top: bool option,
			      registerAllocation: t} :
	                     {operand: Operand.t,
			      assembly: Assembly.t list,
			      rename: FltRegister.t -> FltRegister.t,
			      registerAllocation: t}
	= case operand
            of Operand.MemLoc m 
	     => if fltregister andalso address
		  then case fltallocated {memloc = m,
					  registerAllocation 
					  = registerAllocation}
			 of NONE 
			  => if List.contains(dead,
					      m,
					      MemLoc.eq)
			        orelse 
				List.contains(remove,
					      m,
					      MemLoc.eq)
			       then let
				      val {address, 
					   assembly, 
					   registerAllocation}
					= toAddressMemLoc 
					  {memloc = m,
					   info = info,
					   size = size,
					   supports = supports,
					   saves = saves,
					   registerAllocation
					   = registerAllocation}
				    in
				      {operand = Operand.Address address,
				       assembly = assembly,
				       rename = FltRegister.id,
				       registerAllocation = registerAllocation}
				    end
			       else let
				      val {fltregister, 
					   assembly, 
					   rename,
					   registerAllocation}
					= toFltRegisterMemLoc 
					  {memloc = m,
					   info = info,
					   size = size,
					   move = move,
					   supports = supports,
					   saves = saves,
					   top = top,
					   registerAllocation
					   = registerAllocation}
				    in
				      {operand 
				       = Operand.FltRegister fltregister,
				       assembly = assembly,
				       rename = rename,
				       registerAllocation = registerAllocation}
				    end
	                  | SOME _
			  => let
			       val {fltregister, 
				    assembly, 
				    rename,
				    registerAllocation}
				 = toFltRegisterMemLoc {memloc = m,
							info = info,
							size = size,
							move = move,
							supports = supports,
							saves = saves,
							top = top,
							registerAllocation
							= registerAllocation}
			     in
			       {operand = Operand.FltRegister fltregister,
				assembly = assembly,
				rename = rename,
				registerAllocation = registerAllocation}
			     end
		else if fltregister
		  then let
			 val {fltregister, 
			      assembly, 
			      rename, 
			      registerAllocation}
			   = toFltRegisterMemLoc {memloc = m,
						  info = info,
						  size = size,
						  move = move,
						  supports = supports,
						  saves = saves,
						  top = top,
						  registerAllocation
						  = registerAllocation}
		       in
			 {operand = Operand.FltRegister fltregister,
			  assembly = assembly,
			  rename = rename,
			  registerAllocation = registerAllocation}
		       end
		else if address
		  then let
			 val registerAllocation
			   = fltvalueMap {map 
					  = fn value as {fltregister,
							 memloc,
							 weight,
							 sync,
							 commit}
				             => if MemLoc.eq(memloc, m)
						  then {fltregister 
							= fltregister, 
							memloc = memloc, 
							weight = weight, 
							sync = sync, 
							commit = REMOVE 0}
						  else value,
				          registerAllocation 
					  = registerAllocation}
		     
			 val {assembly = assembly_commit,
			      rename = rename_commit,
			      registerAllocation}
			   = commitFltRegisters {info = info,
						 supports = supports,
						 saves = saves,
						 registerAllocation 
						 = registerAllocation}

			 val {address, assembly, registerAllocation}
			   = toAddressMemLoc {memloc = m,
					      info = info,
					      size = size,
					      supports = supports,
					      saves = saves,
					      registerAllocation
					      = registerAllocation}
		       in
			 {operand = Operand.Address address,
			  assembly = assembly,
			  rename = rename_commit,
			  registerAllocation = registerAllocation}
		       end
		else Error.bug "allocateFltOperand: operand:MemLoc"
	     | _ => Error.bug "allocateFltOperand: operand"

      val (allocateFltOperand, allocateFltOperand_msg)
	= tracer
	  "allocateFltOperand"
	  allocateFltOperand

      local 
	fun allocateFltStackOperands' {fltregister_top: FltRegister.t,
				       fltregister_one: FltRegister.t,
				       registerAllocation: t} :
	                              {assembly: Assembly.t list,
				       rename: FltRegister.t -> FltRegister.t,
				       registerAllocation: t}
	  = case (fltregister_top, fltregister_one)
	      of (FltRegister.T 0, FltRegister.T 1)
	       => {assembly = [],
		   rename = FltRegister.id,
		   registerAllocation = registerAllocation}
	       | (FltRegister.T 1, FltRegister.T 0)
	       => let
		    val {rename = rename,
			 registerAllocation}
		      = fltxch1 {registerAllocation = registerAllocation}
		  in
		    {assembly = [Assembly.instruction_fxch 
				 {src = Operand.fltregister 
				        (FltRegister.T 1)}],
		     rename = rename,
		     registerAllocation = registerAllocation}
		  end
	       | (FltRegister.T 0, FltRegister.T j)
	       => let
		    val {rename = rename,
			 registerAllocation}
		      = fltxch1 {registerAllocation = registerAllocation}

		    val {rename = rename',
			 registerAllocation}
		      = fltxch' {fltregister = FltRegister.T j,
				 registerAllocation = registerAllocation}

		    val {rename = rename'',
			 registerAllocation}
		      = fltxch1 {registerAllocation = registerAllocation}
		  in
		    {assembly = [Assembly.instruction_fxch 
				 {src = Operand.fltregister 
				        (FltRegister.T 1)},
				 Assembly.instruction_fxch 
				 {src = Operand.fltregister 
				        (FltRegister.T j)},
				 Assembly.instruction_fxch 
				 {src = Operand.fltregister 
				        (FltRegister.T 1)}],
		     rename = rename'' o rename' o rename,
		     registerAllocation = registerAllocation}
		  end
	       | (FltRegister.T 1, FltRegister.T j)
	       => let
		    val {rename = rename,
			 registerAllocation}
		      = fltxch' {fltregister = FltRegister.T j,
				 registerAllocation = registerAllocation}

		    val {rename = rename',
			 registerAllocation}
		      = fltxch1 {registerAllocation = registerAllocation}
		  in
		    {assembly = [Assembly.instruction_fxch 
				 {src = Operand.fltregister 
				        (FltRegister.T j)},
				 Assembly.instruction_fxch 
				 {src = Operand.fltregister 
				        (FltRegister.T 1)}],
		     rename = rename' o rename,
		     registerAllocation = registerAllocation}
		  end
	       | (FltRegister.T i, FltRegister.T 1)
	       => let
		    val {rename = rename,
			 registerAllocation}
		      = fltxch' {fltregister = FltRegister.T i,
				 registerAllocation = registerAllocation}
		  in
		    {assembly = [Assembly.instruction_fxch 
				 {src = Operand.fltregister 
				        (FltRegister.T i)}],
		     rename = rename,
		     registerAllocation = registerAllocation}
		  end
	       | (FltRegister.T i, FltRegister.T 0)
	       => let
		    val {rename = rename,
			 registerAllocation}
		      = fltxch1 {registerAllocation = registerAllocation}

		    val {rename = rename',
			 registerAllocation}
		      = fltxch' {fltregister = FltRegister.T i,
				 registerAllocation = registerAllocation}
		  in
		    {assembly = [Assembly.instruction_fxch 
				 {src = Operand.fltregister 
				        (FltRegister.T 1)},
				 Assembly.instruction_fxch 
				 {src = Operand.fltregister 
				        (FltRegister.T i)}],
		     rename = rename' o rename,
		     registerAllocation = registerAllocation}
		  end
	       | (FltRegister.T i, FltRegister.T j)
	       => let
		    val {rename = rename,
			 registerAllocation}
		      = fltxch' {fltregister = FltRegister.T j,
				 registerAllocation = registerAllocation}

		    val {rename = rename',
			 registerAllocation}
		      = fltxch1 {registerAllocation = registerAllocation}
		      
		    val {rename = rename'',
			 registerAllocation}
		      = fltxch' {fltregister = FltRegister.T i,
				 registerAllocation = registerAllocation}
		  in
		    {assembly = [Assembly.instruction_fxch 
				 {src = Operand.fltregister 
				        (FltRegister.T j)},
				 Assembly.instruction_fxch 
				 {src = Operand.fltregister 
				        (FltRegister.T 1)},
				 Assembly.instruction_fxch 
				 {src = Operand.fltregister 
				        (FltRegister.T i)}],
		     rename = rename'' o rename' o rename,
		     registerAllocation = registerAllocation}
		  end
      in
	fun allocateFltStackOperands {operand_top: Operand.t,
				      size_top: Size.t,
				      move_top: bool,
				      operand_one: Operand.t,
				      move_one: bool,
				      size_one: Size.t,
				      info: Liveness.t,
				      supports: Operand.t list,
				      saves: Operand.t list,
				      registerAllocation: t} :
	                             {operand_top: Operand.t,
				      operand_one: Operand.t,
				      assembly: Assembly.t list,
				      rename: FltRegister.t -> FltRegister.t,
				      registerAllocation: t}
	  = if Operand.eq(operand_top, operand_one)
	      then let
		     val {assembly = assembly_free,
			  rename = rename_free,
			  registerAllocation}
		       = freeFltRegister {info = info,
					  size = size_top,
					  supports = operand_top::supports,
					  saves = saves,
					  registerAllocation
					  = registerAllocation}

		     val {operand = operand_allocate_top_one,
			  assembly = assembly_allocate_top_one,
			  rename = rename_allocate_top_one,
			  registerAllocation}
		       = allocateFltOperand 
		         {operand = operand_top,
			  options = {fltregister = true,
				     address = false},
			  info = info,
			  size = size_top,
			  move = move_top,
			  supports = supports,
			  saves = saves,
			  top = SOME true,
			  registerAllocation 
			  = registerAllocation}

		     val temp
		       = MemLoc.imm
		         {base = Immediate.const_int 0,
			  index = Immediate.const_int 0,
			  scale = Scale.One,
			  size = Size.DBLE,
			  commit = MemLoc.Commit.commit {isTemp = true,
							 onFlush = false},
			  class = MemLoc.Class.new "Temp"}

		     val {rename = rename_push,
			  registerAllocation}
		       = fltpush {value = {fltregister = FltRegister.top,
					   memloc = temp,
					   weight = 0,
					   sync = true,
					   commit = NO},
				  registerAllocation = registerAllocation}
		   in
		     {operand_top = Operand.FltRegister FltRegister.top,
		      operand_one = Operand.FltRegister FltRegister.one,
		      assembly = List.concat
		                 [assembly_free,
				  assembly_allocate_top_one,
				  [Assembly.instruction_fld
				   {src = Operand.FltRegister FltRegister.top,
				    size = size_top}]],
		      rename = rename_push o
		               rename_allocate_top_one o
                               rename_free,
		      registerAllocation = registerAllocation}
		   end
	      else let
		     val {operand = operand_allocate_one,
			  assembly = assembly_allocate_one,
			  rename = rename_allocate_one,
			  registerAllocation}
		       = case operand_one
			   of (Operand.MemLoc memloc_one)
			    => (case fltallocated {memloc = memloc_one,
						   registerAllocation 
						   = registerAllocation}
				  of SOME value_one
				    => {operand = Operand.FltRegister 
					          (#fltregister value_one),
					assembly = [],
					rename = FltRegister.id,
					registerAllocation 
					= registerAllocation}
				    | NONE
				    => allocateFltOperand 
				       {operand = operand_one,
					options = {fltregister = true,
						   address = false},
					info = info,
					size = size_one,
					move = move_one,
					supports = supports,
					saves = operand_top::saves,
					top = SOME true,
					registerAllocation 
					= registerAllocation})
			    | _ => allocateFltOperand 
		                   {operand = operand_one,
				    options = {fltregister = true,
					       address = false},
				    info = info,
				    size = size_one,
				    move = move_one,
				    supports = supports,
				    saves = operand_top::saves,
				    top = SOME true,
				    registerAllocation = registerAllocation}

		     val {operand = operand_allocate_top,
			  assembly = assembly_allocate_top,
			  rename = rename_allocate_top,
			  registerAllocation}
		       = case operand_top
			   of (Operand.MemLoc memloc_top)
			    => (case fltallocated {memloc = memloc_top,
						   registerAllocation 
						   = registerAllocation}
				  of SOME value_top
				    => {operand = Operand.FltRegister 
					          (#fltregister value_top),
					assembly = [],
					rename = FltRegister.id,
					registerAllocation 
					= registerAllocation}
				    | NONE
			            => allocateFltOperand 
				       {operand = operand_top,
					options = {fltregister = true,
						   address = false},
					info = info,
					size = size_top,
					move = move_top,
					supports = supports,
					saves = operand_top::saves,
					top = SOME true,
					registerAllocation 
					= registerAllocation})
			    | _ => allocateFltOperand 
			           {operand = operand_top,
				    options = {fltregister = true,
					       address = false},
				    info = info,
				    size = size_top,
				    move = move_top,
				    supports = supports,
				    saves = operand_top::saves,
				    top = SOME true,
				    registerAllocation = registerAllocation}

		     val fltregister_one
		       = case operand_allocate_one
			   of Operand.FltRegister f => f
			    | _ => Error.bug "allocateFltStackOperand"
		     val fltregister_one = rename_allocate_top fltregister_one

		     val fltregister_top
		       = case operand_allocate_top
			   of Operand.FltRegister f => f
			    | _ => Error.bug "allocateFltStackOperand"

		     val {assembly,
			  rename,
			  registerAllocation}
		       = allocateFltStackOperands'
  		         {fltregister_top = fltregister_top,
			  fltregister_one = fltregister_one,
			  registerAllocation = registerAllocation}
		   in
		     {operand_top = Operand.FltRegister FltRegister.top,
		      operand_one = Operand.FltRegister FltRegister.one,
		      assembly = List.concat
		                 [assembly_allocate_one,
				  assembly_allocate_top,
				  assembly],
		      rename = rename o
		               rename_allocate_top o
			       rename_allocate_one,
		      registerAllocation = registerAllocation}
		   end
      end

      val (allocateFltStackOperands, allocateFltStackOperands_msg)
	= tracer
	  "allocateFltStackOperands"
	  allocateFltStackOperands

      fun renameLift rename 
	= fn Operand.FltRegister f
	   => Operand.FltRegister (rename f)
	   | operand => operand

      fun reset {registerAllocation: t}
	= {assembly = [],
	   registerAllocation = empty ()}

      fun cache {register: Register.t, 
		 memloc: MemLoc.t, 
		 reserve: bool, 
		 info: Liveness.t,
		 registerAllocation: t}
	= let
	    val {register,
		 assembly = assembly_register,
		 registerAllocation as {entries, reserved, fltstack}}
	      = toRegisterMemLoc 
	        {memloc = memloc,
		 info = info,
		 size = Register.size register, 
		 move = true,
		 supports = [],
		 saves = [], 
		 force = [register], 
		 registerAllocation = registerAllocation}

	    val reserved = if reserve
			     then register::reserved
			     else reserved
	  in
	    {assembly = assembly_register,
	     registerAllocation = {entries = entries,
				   reserved = reserved,
				   fltstack = fltstack}}
	  end

      fun assume {register: Register.t, 
		  memloc: MemLoc.t, 
		  weight: int,
		  sync: bool,
		  reserve: bool, 
		  registerAllocation: t}
	= let
	    val registerAllocation as {entries, reserved, fltstack}
	      = update
                {value = {register = register,
			  memloc = memloc,
			  weight = weight,
			  sync = sync,
			  commit = NO},
		 registerAllocation = registerAllocation}

	    val reserved
	      = if reserve
		  then register::reserved
		  else reserved
	  in
	    {assembly = [],
	     registerAllocation = {entries = entries,
				   reserved = reserved,
				   fltstack = fltstack}}
	  end

      fun flush {info: Liveness.t,
		 registerAllocation: t}
	= let
	    val registerAllocation
	      = valueMap {map = fn value as {register,
					     memloc,
					     weight,
					     sync,
					     commit}
				 => if MemLoc.onFlush memloc
				      then {register = register, 
					    memloc = memloc, 
					    weight = weight, 
					    sync = sync, 
					    commit = COMMIT 0}
				      else value,
			  registerAllocation = registerAllocation}

	    val registerAllocation
	      = fltvalueMap {map = fn value as {fltregister,
						memloc,
						weight,
						sync,
						commit}
				    => {fltregister = fltregister, 
					memloc = memloc, 
					weight = weight, 
					sync = sync, 
					commit = REMOVE 0},
			  registerAllocation = registerAllocation}

	    val {assembly = assembly_commit_fltregisters,
		 rename = rename_commit_fltregisters,
		 registerAllocation}
	      = commitFltRegisters {info = info,
				    supports = [],
				    saves = [],
				    registerAllocation = registerAllocation}

	    val {assembly = assembly_commit_registers,
		 registerAllocation}
	      = commitRegisters {info = info,
				 supports = [],
				 saves = [],
				 registerAllocation = registerAllocation}
	  in
	    {assembly = List.concat
	                [assembly_commit_registers,
			 assembly_commit_fltregisters],
	     registerAllocation = registerAllocation}
	  end

      fun clear {info: Liveness.t,
		 registerAllocation: t}
	= let
	    val registerAllocation
	      = valueMap {map = fn value as {register,
					     memloc,
					     weight,
					     sync,
					     commit}
				 => if List.contains
			               (Register.callerSaveRegisters,
					register,
					Register.eq)
				      then {register = register, 
					    memloc = memloc, 
					    weight = weight, 
					    sync = sync, 
					    commit = REMOVE 0}
				      else value,
			  registerAllocation = registerAllocation}

	    val registerAllocation
	      = fltvalueMap {map = fn value as {fltregister,
						memloc,
						weight,
						sync,
						commit}
			            => {fltregister = fltregister, 
					memloc = memloc, 
					weight = weight, 
					sync = sync, 
					commit = REMOVE 0},
			     registerAllocation = registerAllocation}

	    val {assembly = assembly_commit_fltregisters,
		 rename = rename_commit_fltregisters,
		 registerAllocation}
	      = commitFltRegisters {info = info,
				    supports = [],
				    saves = [],
				    registerAllocation = registerAllocation}

	    val {assembly = assembly_commit_registers,
		 registerAllocation}
	      = commitRegisters {info = info,
				 supports = [],
				 saves = [],
				 registerAllocation = registerAllocation}
	  in
	    {assembly = List.concat
	                [assembly_commit_registers,
			 assembly_commit_fltregisters],
	     registerAllocation = registerAllocation}
	  end

      fun eject {memlocs: MemLoc.t list,
		 info: Liveness.t,
		 registerAllocation: t}
	= (case memlocs
	     of [] => {assembly = [],
		       registerAllocation = registerAllocation}
	      | _
	      => let
		   val registerAllocation
		     = valueMap {map 
				 = fn value as {register,
						memloc,
						weight,
						sync,
						commit}
			            => if List.contains(memlocs,
							memloc,
							MemLoc.eq)
					 then {register = register, 
					       memloc = memloc, 
					       weight = weight, 
					       sync = true, 
					       commit = REMOVE 0}
					 else value,
				 registerAllocation 
				 = registerAllocation}

		   val registerAllocation
		     = fltvalueMap {map 
				    = fn value as {fltregister,
						   memloc,
						   weight,
						   sync,
						   commit}
			               => if List.contains(memlocs,
							   memloc,
							   MemLoc.eq)
					    then {fltregister = fltregister, 
						  memloc = memloc, 
						  weight = weight, 
						  sync = true, 
						  commit = REMOVE 0}
					    else value,
				    registerAllocation 
				    = registerAllocation}

		   val {assembly = assembly_commit_fltregisters,
			rename = rename_commit_fltregisters,
			registerAllocation}
		     = commitFltRegisters {info = info,
					   supports = [],
					   saves = [],
					   registerAllocation 
					   = registerAllocation}

		   val {assembly = assembly_commit_registers,
			registerAllocation}
		     = commitRegisters {info = info,
					supports = [],
					saves = [],
					registerAllocation 
					= registerAllocation}
		 in
		   {assembly = List.concat
		               [assembly_commit_registers,
				assembly_commit_fltregisters],
		    registerAllocation = registerAllocation}
		 end)

      fun commit {memlocs: MemLoc.t list,
		  info: Liveness.t,
		  registerAllocation: t}
	= (case memlocs
	     of [] => {assembly = [],
		       registerAllocation = registerAllocation}
	      | _
	      => let
		   val registerAllocation
		     = valueMap {map 
				 = fn value as {register,
						memloc,
						weight,
						sync,
						commit}
			            => if List.contains(memlocs,
							memloc,
							MemLoc.eq)
					 then {register = register, 
					       memloc = memloc, 
					       weight = weight, 
					       sync = sync, 
					       commit = COMMIT 0}
					 else value,
				 registerAllocation 
				 = registerAllocation}

		   val registerAllocation
		     = fltvalueMap {map 
				    = fn value as {fltregister,
						   memloc,
						   weight,
						   sync,
						   commit}
			               => if List.contains(memlocs,
							   memloc,
							   MemLoc.eq)
					    then {fltregister = fltregister, 
						  memloc = memloc, 
						  weight = weight, 
						  sync = sync, 
						  commit = COMMIT 0}
					    else value,
				    registerAllocation 
				    = registerAllocation}

		   val {assembly = assembly_commit_fltregisters,
			rename = rename_commit_fltregisters,
			registerAllocation}
		     = commitFltRegisters {info = info,
					   supports = [],
					   saves = [],
					   registerAllocation 
					   = registerAllocation}

		   val {assembly = assembly_commit_registers,
			registerAllocation}
		     = commitRegisters {info = info,
					supports = [],
					saves = [],
					registerAllocation 
					= registerAllocation}

		 in
		   {assembly = List.concat
		               [assembly_commit_registers,
                                assembly_commit_fltregisters],
		    registerAllocation = registerAllocation}
		 end)

      fun return {memloc = return_memloc,
		  info: Liveness.t,
		  registerAllocation: t}
	= let
	    val killed_values
	      = valueFilter {filter = fn value as {memloc,...}
			               => List.exists
			                  (MemLoc.utilized memloc,
					   fn memloc'
					    => MemLoc.eq(memloc',
							 return_memloc))
					  orelse
					  MemLoc.mayAlias(return_memloc,
							  memloc),
			     registerAllocation = registerAllocation}
	    val killed_memlocs = List.map(killed_values, #memloc)

	    val registerAllocation
	      = removes {memlocs = killed_memlocs,
			 registerAllocation = registerAllocation}

	    val return_register = Register.return (MemLoc.size return_memloc)
	    val registerAllocation
	      = update
                {value = {register = return_register,
			  memloc = return_memloc,
			  weight = 1024,
			  sync = false,
			  commit = NO},
		 registerAllocation = registerAllocation}

	    val {assembly = assembly_post,
		 registerAllocation}
	      = post {uses = [],
		      final_uses = [],
		      defs = [Operand.memloc return_memloc],
		      final_defs = [Operand.register return_register],
		      info = info,
		      registerAllocation = registerAllocation}
	  in
	    {assembly = assembly_post,
	     registerAllocation = registerAllocation}
	  end

      fun fltreturn {memloc = return_memloc,
		     info: Liveness.t,
		     registerAllocation: t}
	= let
	    val return_register = FltRegister.return

	    val {rename = rename_push,
		 registerAllocation}
	      = fltpush
                {value = {fltregister = return_register,
			  memloc = return_memloc,
			  weight = 1024,
			  sync = false,
			  commit = NO},
		 registerAllocation = registerAllocation}

	    val {assembly = assembly_post,
		 registerAllocation}
	      = post {uses = [],
		      final_uses = [],
		      defs = [Operand.memloc return_memloc],
		      final_defs = [Operand.fltregister return_register],
		      info = info,
		      registerAllocation = registerAllocation}

	  in
	    {assembly = assembly_post,
	     registerAllocation = registerAllocation}
	  end
    end

  structure Instruction =
    struct
      structure RA = RegisterAllocation
      open Instruction

      (*
       * Require src/dst operands as follows:
       *
       *              dst
       *          reg imm lab add 
       *      reg  X           X
       *  src imm  X           X
       *      lab
       *      add  X
       *)
      fun allocateSrcDst {src: Operand.t,
			  dst: Operand.t,
			  move_dst: bool,
			  size: Size.t,
			  info as {dead, commit, remove, ...}: Liveness.t,
			  registerAllocation: RegisterAllocation.t}
	= if Operand.eq(src, dst)
	    then let
		   val {operand = final_src_dst, 
			assembly = assembly_src_dst,
			registerAllocation}
		     = RA.allocateOperand 
		       {operand = src,
			options = {register = true,
				   immediate = false,
				   label = false,
				   address = false},
			info = info,
			size = size,
			move = true,
			supports = [],
			saves = [],
			force = [],
			registerAllocation 
			= registerAllocation}
		 in
		   {final_src = final_src_dst,
		    final_dst = final_src_dst,
		    assembly_src_dst = assembly_src_dst,
		    registerAllocation = registerAllocation}
		 end
	    else case (src, dst)
		   of (Operand.MemLoc memloc_src,
		       Operand.MemLoc memloc_dst)
		    => if List.contains(dead,
					memloc_dst,
					MemLoc.eq)
		          orelse
			  List.contains(remove,
					memloc_dst,
					MemLoc.eq)
			 then let
				val {operand = final_dst,
				     assembly = assembly_dst,
				     registerAllocation}
				  = RA.allocateOperand 
				    {operand = dst,
				     options = {register = true,
						immediate = false,
						label = false,
						address = true},
				     info = info,
				     size = size,
				     move = move_dst,
				     supports = [src],
				     saves = [],
				     force = [],
				     registerAllocation 
				     = registerAllocation}

				val options_src
				  = case final_dst
				      of Operand.Register _
				       => {register = true,
					   immediate = true,
					   label = false,
					   address = true}
				       | _ 
				       => {register = true,
					   immediate = true,
					   label = false,
					   address = false}
 
				val {operand = final_src, 
				     assembly = assembly_src,
				     registerAllocation}
				  = RA.allocateOperand 
				    {operand = src,
				     options = options_src,
				     info = info,
				     size = size,
				     move = true,
				     supports = [],
				     saves = [dst,final_dst],
				     force = [],
				     registerAllocation 
				     = registerAllocation}
			      in
				{final_src = final_src,
				 final_dst = final_dst,
				 assembly_src_dst 
				 = List.concat [assembly_dst, assembly_src],
				 registerAllocation = registerAllocation}
			      end
 			 else let
				val {operand = final_src, 
				     assembly = assembly_src,
				     registerAllocation}
				  = RA.allocateOperand 
				    {operand = src,
				     options = {register = true,
						immediate = true,
						label = false,
						address = true},
				     info = info,
				     size = size,
				     move = true,
				     supports = [dst],
				     saves = [],
				     force = [],
				     registerAllocation 
				     = registerAllocation}

				val {operand = final_dst,
				     assembly = assembly_dst,
				     registerAllocation}
				  = RA.allocateOperand 
				    {operand = dst,
				     options = {register = true,
						immediate = false,
						label = false,
						address = false},
				     info = info,
				     size = size,
				     move = move_dst,
				     supports = [],
				     saves = [src,final_src],
				     force = [],
				     registerAllocation 
				     = registerAllocation}
			      in
				{final_src = final_src,
				 final_dst = final_dst,
				 assembly_src_dst 
				 = List.concat [assembly_src, assembly_dst],
				 registerAllocation = registerAllocation}
			      end
		    | _
		    => let
			 val {operand = final_src, 
			      assembly = assembly_src,
			      registerAllocation}
			   = RA.allocateOperand 
			     {operand = src,
			      options = {register = true,
					 immediate = true,
					 label = false,
					 address = false},
			      info = info,
			      size = size,
			      move = true,
			      supports = [dst],
			      saves = [],
			      force = [],
			      registerAllocation 
			      = registerAllocation}

			 val {operand = final_dst,
			      assembly = assembly_dst,
			      registerAllocation}
			   = RA.allocateOperand 
			     {operand = dst,
			      options = {register = true,
					 immediate = false,
					 label = false,
					 address = true},
			      info = info,
			      size = size,
			      move = move_dst,
			      supports = [],
			      saves = [src,final_src],
			      force = [],
			      registerAllocation 
			      = registerAllocation}
		       in
			 {final_src = final_src,
			  final_dst = final_dst,
			  assembly_src_dst 
			  = List.concat [assembly_src, assembly_dst],
			  registerAllocation = registerAllocation}
		       end

      (* 
       * Require src1/src2 operands as follows:
       *
       *               src2
       *           reg imm lab add 
       *       reg  X   X       X
       *  src1 imm
       *       lab
       *       add  X   X
       *)
      fun allocateSrc1Src2 {src1: Operand.t,
			    src2: Operand.t,
			    size: Size.t,
			    info as {dead, commit, remove, ...}: Liveness.t,
			    registerAllocation: RegisterAllocation.t}
	= if Operand.eq(src1, src2)
	    then let
		   val {operand = final_src1_src2, 
			assembly = assembly_src1_src2,
			registerAllocation}
		     = RA.allocateOperand 
		       {operand = src1,
			options = {register = true,
				   immediate = false,
				   label = false,
				   address = false},
			info = info,
			size = size,
			move = true,
			supports = [],
			saves = [],
			force = [],
			registerAllocation 
			= registerAllocation}
		 in
		   {final_src1 = final_src1_src2,
		    final_src2 = final_src1_src2,
		    assembly_src1_src2 = assembly_src1_src2,
		    registerAllocation = registerAllocation}
		 end
	    else let
		   val {operand = final_src1,
			assembly = assembly_src1,
			registerAllocation}
		     = RA.allocateOperand 
		       {operand = src1,
			options = {register = true,
				   immediate = false,
				   label = false,
				   address = true},
			info = info,
			size = size,
			move = true,
			supports = [src2],
			saves = [],
			force = [],
			registerAllocation 
			= registerAllocation}

		   val options_src2
		     = case final_src1
			 of Operand.Register _
			  => {register = true,
			      immediate = true,
			      label = false,
			      address = true}
			  | _ 
			  => {register = true,
			      immediate = true,
			      label = false,
			      address = false}
 
		   val {operand = final_src2, 
			assembly = assembly_src2,
			registerAllocation}
		     = RA.allocateOperand 
		       {operand = src2,
			options = options_src2,
			info = info,
			size = size,
			move = true,
			supports = [],
			saves = [src1,final_src1],
			force = [],
			registerAllocation 
			= registerAllocation}
		 in
		   {final_src1 = final_src1,
		    final_src2 = final_src2,
		    assembly_src1_src2 
		    = List.concat [assembly_src1, assembly_src2],
		    registerAllocation = registerAllocation}
		 end

      fun removable {memloc,
		     info as {dead, commit, remove, ...}: Liveness.t,
		     registerAllocation}
	= List.contains(dead,
			memloc,
			MemLoc.eq)
	  orelse
	  (List.contains(remove,
			 memloc,
			 MemLoc.eq)
	   andalso
	   (case RA.fltallocated {memloc = memloc,
				  registerAllocation = registerAllocation}
	      of SOME {sync,...} => sync
	       | NONE => true))

      fun allocateRegisters {instruction: t,
			     info as {dead, commit, remove, ...}: Liveness.t,
			     registerAllocation: RegisterAllocation.t}
	= case instruction
	    of BinAL {oper, src, dst, size}
	       (* Integer binary arithmetic(w/o mult & div)/logic instructions.
		* Require src/dst operands as follows:
		*
		*              dst
		*          reg imm lab add 
		*      reg  X           X
		*  src imm  X           X
		*      lab
		*      add  X
		*)
	     => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val {final_src,
		       final_dst,
		       assembly_src_dst,
		       registerAllocation}
		    = allocateSrcDst {src = src,
				      dst = dst,
				      move_dst = true,
				      size = size,
				      info = info,
				      registerAllocation = registerAllocation}

		  val instruction 
		    = Instruction.BinAL
		      {oper = oper,
		       src = final_src,
		       dst = final_dst,
		       size = size}

		  val {uses = final_uses,
		       defs = final_defs,
		       ...}
		    = Instruction.uses_defs_kills instruction

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}
		in
		  {assembly
		   = List.concat [assembly_pre,
				  assembly_src_dst,
				  (Assembly.instruction instruction)::
				  assembly_post],
		   registerAllocation = registerAllocation}
		end
	     | pMD {oper, dst, src, size}
	       (* Integer multiplication and division.
		* Require src operand as follows:
		*
		*               src
		*           reg imm lab add
		*            X           X
		*)
	     => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val (hi,lo) 
		    = case size
			of Size.BYTE 
			=> (Register.T {reg = Register.EAX, part = Register.H},
			    Register.T {reg = Register.EAX, part = Register.L})
			| Size.WORD 
			=> (Register.T {reg = Register.EDX, part = Register.X},
			    Register.T {reg = Register.EAX, part = Register.X})
			| Size.LONG
			=> (Register.T {reg = Register.EDX, part = Register.E},
			    Register.T {reg = Register.EAX, part = Register.E})
			| _ => Error.bug "allocateRegisters: pMD, size"

		  val {register,
		       assembly = assembly_clear,
		       registerAllocation}
		    = RA.freeRegister 
		      {info = info,
		       memloc = NONE,
		       size = size,
		       supports = [src,dst],
		       saves = [], 
		       force = [hi],
		       registerAllocation = registerAllocation}

		  val registerAllocation
		    = RA.delete {register = hi,
				 registerAllocation = registerAllocation}

		  val {final_src,
		       final_dst,
		       assembly_src_dst,
		       registerAllocation}
		    = if Operand.eq(src, dst)
			then let
			       val {operand = final_src_dst,
				    assembly = assembly_src_dst,
				    registerAllocation = registerAllocation}
				 = RA.allocateOperand 
				   {operand = dst,
				    options = {register = true,
					       immediate = false,
					       label = false,
					       address = false},
				    info = info,
				    size = size,
				    move = true,
				    supports = [],
				    saves = [Operand.register hi],
				    force = [lo],
				    registerAllocation 
				    = registerAllocation}
			     in
			       {final_src = final_src_dst,
				final_dst = final_src_dst,
				assembly_src_dst = assembly_src_dst,
				registerAllocation = registerAllocation}
			     end
			else let
			       val {operand = final_dst,
				    assembly = assembly_dst,
				    registerAllocation = registerAllocation}
				 = RA.allocateOperand 
				   {operand = dst,
				    options = {register = true,
					       immediate = false,
					       label = false,
					       address = false},
				    info = info,
				    size = size,
				    move = true,
				    supports = [src],
				    saves = [Operand.register hi],
				    force = [lo],
				    registerAllocation 
				    = registerAllocation}

			       val force_src 
				 = List.keepAll
				   (Register.registers size,
				    fn r => not (Register.eq(r, hi) orelse 
						 Register.eq(r, lo)))

			       val {operand = final_src, 
				    assembly = assembly_src,
				    registerAllocation}
				 = RA.allocateOperand 
				   {operand = src,
				    options = {register = true,
					       immediate = false,
					       label = false,
					       address = true}, 
				    info = info,
				    size = size,
				    move = true,
				    supports = [],
				    saves = [Operand.register hi,
					     dst,final_dst],
				    force = force_src,
				    registerAllocation 
				    = registerAllocation}
			     in
			       {final_src = final_src,
				final_dst = final_dst,
				assembly_src_dst = List.concat [assembly_dst, assembly_src],
				registerAllocation = registerAllocation}
			     end

		  val oper' 
		    = case oper
			of Instruction.IMUL => Instruction.IMUL
			 | Instruction.MUL => Instruction.MUL
			 | Instruction.IDIV => Instruction.IDIV
			 | Instruction.DIV => Instruction.DIV
			 | Instruction.IMOD => Instruction.IDIV
			 | Instruction.MOD => Instruction.DIV

		  val registerAllocation
		    = if oper = Instruction.IMOD orelse
		         oper = Instruction.MOD
			then case RA.valuesRegister {register = lo,
						     registerAllocation
						     = registerAllocation}
			       of [value as {register,
					     memloc,
					     weight,
					     sync,
					     commit}]
				=> let
				     val registerAllocation
				       = RA.delete {register = lo,
						    registerAllocation
						    = registerAllocation}

				     val registerAllocation
				       = RA.update {value = {register = hi,
							     memloc = memloc,
							     weight = weight,
							     sync = sync,
							     commit = commit},
						    registerAllocation 
						    = registerAllocation}
				   in
				     registerAllocation
				   end
				| _ => Error.bug "allocateRegisters: pMD, lo"
			else registerAllocation

		  val instruction
		    = Instruction.MD
		      {oper = oper',
		       src = final_src,
		       size = size}

		  val {uses = final_uses,
		       defs = final_defs,  
		       ...}
		    = Instruction.uses_defs_kills instruction

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}
		in
		  {assembly 
		   = List.concat [assembly_pre,
				  assembly_clear,
				  assembly_src_dst,
				  (if oper = Instruction.IMUL orelse
				     oper = Instruction.IDIV orelse
				     oper = Instruction.IMOD
				     then Assembly.instruction_cx
				          {size = size}
				     else Assembly.instruction_binal
				          {oper = Instruction.XOR,
					   dst = Operand.register hi,
					   src = Operand.register hi,
					   size = size})::
				  (Assembly.instruction instruction)::
				  assembly_post],
                   registerAllocation = registerAllocation}
		end
	     | UnAL {oper, dst, size}
	       (* Integer unary arithmetic/logic instructions.
		* Require dst operand as follows:
		*
		*               dst
		*           reg imm lab add
		*            X           X
		*)
	     => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val {operand = final_dst,
		       assembly = assembly_dst,
		       registerAllocation = registerAllocation}
		    = RA.allocateOperand {operand = dst,
					  options = {register = true,
						     immediate = false,
						     label = false,
						     address = true},
					  info = info,
					  size = size,
					  move = true,
					  supports = [],
					  saves = [],
					  force = [],
					  registerAllocation 
					  = registerAllocation}

		  val instruction
		    = Instruction.UnAL
		      {oper = oper,
		       dst = final_dst,
		       size = size}

		  val {uses = final_uses,
		       defs = final_defs,  
		       ...}
		    = Instruction.uses_defs_kills instruction

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}
		in
		  {assembly 
		   = List.concat [assembly_pre,
				  assembly_dst,
				  (Assembly.instruction instruction)::
				  assembly_post],
		   registerAllocation = registerAllocation}
		end
	     | SRAL {oper, count, dst, size}
	       (* Integer shift/rotate arithmetic/logic instructions.
		* Require count operand as follows:
		*
		*              count
		*           reg imm lab add
		*            *   X
		*  * only register %cl
		*
		* Require dst operand as follows:
		*
		*               dst
		*           reg imm lab add
		*            X           X
		*)
	     => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val {final_count,
		       assembly_count,
		       final_dst,
		       assembly_dst,
		       registerAllocation}
		    = if Operand.eq(count,dst)
			then let
			       val {operand = final_count, 
				    assembly = assembly_count,
				    registerAllocation}
				 = RA.allocateOperand 
				   {operand = count,
				    options = {register = true,
					       immediate = false,
					       label = false,
					       address = false}, 
				    info = info,
				    size = size,
				    move = true,
				    supports = [],
				    saves = [],
				    force 
				    = [Register.T {reg = Register.ECX,
						   part = Register.L},
				       Register.T {reg = Register.ECX,
						   part = Register.X},
				       Register.T {reg = Register.ECX,
						   part = Register.E}],
				    registerAllocation 
				    = registerAllocation}

			       val final_dst = final_count
			       val assembly_dst = []
			     in
			       {final_count = final_count,
				assembly_count = assembly_count,
				final_dst = final_dst,
				assembly_dst = assembly_dst,
				registerAllocation = registerAllocation}
			     end
			else let
			       val count_size = case Operand.size count
						  of NONE => Size.BYTE
						   | SOME size => size

			       val {operand = final_count, 
				    assembly = assembly_count,
				    registerAllocation}
				 = RA.allocateOperand 
				   {operand = count,
				    options = {register = true,
					       immediate = true,
					       label = false,
					       address = false}, 
				    info = info,
				    size = count_size,
				    move = true,
				    supports = [dst],
				    saves = [],
				    force 
				    = [Register.T {reg = Register.ECX,
						   part = Register.L},
				       Register.T {reg = Register.ECX,
						   part = Register.X},
				       Register.T {reg = Register.ECX,
						   part = Register.E}],
				    registerAllocation 
				    = registerAllocation}

			       val {operand = final_dst,
				    assembly = assembly_dst,
				    registerAllocation = registerAllocation}
				 = RA.allocateOperand 
				   {operand = dst,
				    options = {register = true,
					       immediate = false,
					       label = false,
					       address = true},
				    info = info,
				    size = size,
				    move = true,
				    supports = [],
				    saves = [count,final_count],
				    force = [],
				    registerAllocation 
				    = registerAllocation}
			     in
			       {final_count = final_count,
				assembly_count = assembly_count,
				final_dst = final_dst,
				assembly_dst = assembly_dst,
				registerAllocation = registerAllocation}
			     end

		  val final_count
		    = case final_count
			of Operand.Register _ 
			 => Operand.register 
			    (Register.T {reg = Register.ECX,
					 part = Register.L})
		         | _ => final_count

		  val instruction 
		    = Instruction.SRAL
		      {oper = oper,
		       count = final_count,
		       dst = final_dst,
		       size = size}

		  val {uses = final_uses,
		       defs = final_defs,  
		       ...}
		    = Instruction.uses_defs_kills instruction

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}
		in
		  {assembly 
		   = List.concat [assembly_pre,
				  assembly_count,
				  assembly_dst,
				  (Assembly.instruction instruction)::
				  assembly_post],
		   registerAllocation = registerAllocation}
		end
             | CMP {src2, src1, size}
	       (* Arithmetic compare;  p. 116
		* Require src1/src2 operands as follows:
		*
		*               src2
		*           reg imm lab add 
		*       reg  X   X       X
		*  src1 imm
		*       lab
		*       add  X   X
		*)
	     => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val {final_src1,
		       final_src2,
		       assembly_src1_src2,
		       registerAllocation}
		    = allocateSrc1Src2 
		      {src1 = src1,
		       src2 = src2,
		       size = size,
		       info = info,
		       registerAllocation = registerAllocation}

		  val instruction
		    = Instruction.CMP 
		      {src1 = final_src1,
		       src2 = final_src2,
		       size = size}

		  val {uses = final_uses,
		       defs = final_defs,  
		       ...}
		    = Instruction.uses_defs_kills instruction

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}
		in
		  {assembly 
		   = List.concat [assembly_pre,
				  assembly_src1_src2,
				  (Assembly.instruction instruction)::
				  assembly_post],
		   registerAllocation = registerAllocation}
		end
             | TEST {src2, src1, size}
	       (* Logical compare; p. 728
		* Require src1/src2 operands as follows:
		*
		*               src2
		*           reg imm lab add 
		*       reg  X   X       X
		*  src1 imm  
		*       lab
		*       add  X   X    
		*)
             => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val {final_src1,
		       final_src2,
		       assembly_src1_src2,
		       registerAllocation}
		    = allocateSrc1Src2 
		      {src1 = src1,
		       src2 = src2,
		       size = size,
		       info = info,
		       registerAllocation = registerAllocation}

		  val instruction
		    = Instruction.TEST
		      {src1 = final_src1,
		       src2 = final_src2,
		       size = size}

		  val {uses = final_uses,
		       defs = final_defs,  
		       ...}
		    = Instruction.uses_defs_kills instruction

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}
		in
		  {assembly 
		   = List.concat [assembly_pre,
				  assembly_src1_src2,
				  (Assembly.instruction instruction)::
				  assembly_post],
		   registerAllocation = registerAllocation}
		end
	     | SETcc {condition, dst, size}
	       (* Set byte on condition; p. 672
		* Require dst operand as follows:
		*
		*                dst
		*            reg imm lab add
		*             *           X
		*  * only byte registers
		*)
	     => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val {operand = final_dst,
		       assembly = assembly_dst,
		       registerAllocation = registerAllocation}
		    = RA.allocateOperand {operand = dst,
					  options = {register = true,
						     immediate = false,
						     label = false,
						     address = false},
					  info = info,
					  size = size,
					  move = false,
					  supports = [],
					  saves = [],
					  force 
					  = Register.withLowPart (size, 
								  Size.BYTE),
					  registerAllocation 
					  = registerAllocation}

		  val {uses = final_uses,
		       defs = final_defs,  
		       ...}
		    = Instruction.uses_defs_kills
		      (Instruction.SETcc {condition = condition,
					  dst = final_dst,
					  size = size})

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}

		  val temp_reg
		    = case final_dst
			of Operand.Register r 
			 => Register.lowPartOf (r, Size.BYTE)
			 | _ 
			 => Error.bug "allocateRegisters: SETcc, temp_reg"
		in
		  {assembly 
		   = List.concat [assembly_pre,
				   assembly_dst,
				   [Assembly.instruction_setcc
				    {condition = condition,
				     dst = Operand.register temp_reg,
				     size = Size.BYTE}],
				   if size = Size.BYTE
				     then []
				     else [Assembly.instruction_movx
					   {oper = Instruction.MOVZX,
					    dst = final_dst,
					    src = Operand.register temp_reg,
					    dstsize = size,
					    srcsize = Size.BYTE}],
				   assembly_post],
		   registerAllocation = registerAllocation}
		end
             | JMP {target, absolute}
	       (* Jump; p. 373
		* Require target operand as follows:
		*
		*               target
		*            reg imm lab add
		*             X   X   X   X
		*)
	     => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val {operand = final_target,
		       assembly = assembly_target,
		       registerAllocation = registerAllocation}
		    = RA.allocateOperand {operand = target,
					  options = {register = true,
						     immediate = true,
						     label = true,
						     address = true},
					  info = info,
					  size = Size.LONG,
					  move = true,
					  supports = [],
					  saves = [],
					  force = [],
					  registerAllocation 
					  = registerAllocation}

		  val instruction
		    = Instruction.JMP
		      {target = final_target,
		       absolute = absolute}

		  val {uses = final_uses,
		       defs = final_defs,  
		       ...}
		    = Instruction.uses_defs_kills instruction

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}
		in
		  {assembly 
		   = List.concat [assembly_pre,
				   assembly_target,
				   [Assembly.instruction instruction],
				   assembly_post],
		   registerAllocation = registerAllocation}
		end
	     | Jcc {condition, target}
	       (* Jump if condition is met; p. 369
		* Require target operand as follows:
		*
		*               target
		*            reg imm lab add
		*                 X   X
		*)
	     => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val {operand = final_target,
		       assembly = assembly_target,
		       registerAllocation = registerAllocation}
		    = RA.allocateOperand {operand = target,
					  options = {register = false,
						     immediate = true,
						     label = true,
						     address = false},
					  info = info,
					  size = Size.LONG,
					  move = true,
					  supports = [],
					  saves = [],
					  force = [],
					  registerAllocation 
					  = registerAllocation}

		  val instruction
		    = Instruction.Jcc
		      {condition = condition,
		       target = final_target}

		  val {uses = final_uses,
		       defs = final_defs,  
		       ...}
		    = Instruction.uses_defs_kills instruction

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}
		in
		  {assembly 
		   = List.concat [assembly_pre,
				   assembly_target,
				   [Assembly.instruction instruction],
				   assembly_post],
		   registerAllocation = registerAllocation}
		end
             | CALL {target, absolute}
	       (* Call procedure; p. 93 
		* Require target operand as follows:
		*
		*               target
		*            reg imm lab add
		*             X   X   X   X
		*)
	     => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val {operand = final_target,
		       assembly = assembly_target,
		       registerAllocation = registerAllocation}
		    = RA.allocateOperand {operand = target,
					  options = {register = true,
						     immediate = true,
						     label = true,
						     address = true},
					  info = info,
					  size = Size.LONG,
					  move = true,
					  supports = [],
					  saves = [],
					  force = [],
					  registerAllocation 
					  = registerAllocation}

		  val instruction
		    = Instruction.CALL
		      {target = final_target,
		       absolute = absolute}

		  val {uses = final_uses,
		       defs = final_defs,  
		       ...}
		    = Instruction.uses_defs_kills instruction

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}
		in
		  {assembly 
		   = List.concat [assembly_pre,
				   assembly_target,
				   [Assembly.instruction instruction],
				   assembly_post],
		   registerAllocation = registerAllocation}
		end
             | RET {src = SOME src}
	       (* Return from procedure; p. 648 
		* Require optional src operand as follows:
		*
		*                src
		*            reg imm lab add
		*                 X   
		*)
             => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val {operand = final_src,
		       assembly = assembly_src,
		       registerAllocation = registerAllocation}
		    = RA.allocateOperand {operand = src,
					  options = {register = false,
						     immediate = true,
						     label = false,
						     address = false},
					  info = info,
					  size = Size.LONG,
					  move = true,
					  supports = [],
					  saves = [],
					  force = [],
					  registerAllocation 
					  = registerAllocation}

		  val instruction
		    = Instruction.RET
		      {src = SOME final_src}

		  val {uses = final_uses,
		       defs = final_defs,  
		       ...}
		    = Instruction.uses_defs_kills instruction

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}
		in
		  {assembly 
		   = List.concat [assembly_pre,
				   [Assembly.instruction instruction],
				   assembly_post],
		   registerAllocation = registerAllocation}
		end
             | RET {src = NONE}
	     => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val instruction
		    = Instruction.RET
		      {src = NONE}

		  val {uses = final_uses,
		       defs = final_defs,  
		       ...}
		    = Instruction.uses_defs_kills instruction

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}
		in
		  {assembly 
		   = List.concat [assembly_pre,
				   [Assembly.instruction instruction],
				   assembly_post],
		   registerAllocation = registerAllocation}
		end
	     | MOV {src, dst, size}
	       (* Move; p. 442 
		* Require src/dst operands as follows:
		*
		*              dst
		*          reg imm lab add 
		*      reg  X           X
		*  src imm  X           X
		*      lab
		*      add  X
		*)
	     => let
		  fun default ()
		    = let
			val {uses,defs,kills} 
			  = Instruction.uses_defs_kills instruction
			val {assembly = assembly_pre,
			     registerAllocation}
			  = RA.pre {uses = uses,
				    defs = defs,
				    kills = kills,
				    info = info,
				    registerAllocation = registerAllocation}
			  
			val {final_src,
			     final_dst,
			     assembly_src_dst,
			     registerAllocation}
			  = allocateSrcDst
			    {src = src,
			     dst = dst,
			     move_dst = false,
			     size = size,
			     info = info,
			     registerAllocation = registerAllocation}
	
			val isConst0
			  = fn Immediate.Char #"\000" => true
			     | Immediate.Int 0 => true
			     | Immediate.Word 0wx0 => true
			     | _ => false

			(* special case moving 0 to a register
			 *)
			val instruction
			  = case (final_src, final_dst)
			      of (Operand.Immediate (Immediate.Const c),
				  Operand.Register _)
			       => if isConst0 c
				    then Instruction.BinAL
				         {oper = XOR,
					  src = final_dst,
					  dst = final_dst,
					  size = size}
				    else Instruction.MOV
				         {src = final_src,
					  dst = final_dst,
					  size = size}
			       | _ => Instruction.MOV
				      {src = final_src,
				       dst = final_dst,
				       size = size}

			val {uses = final_uses,
			     defs = final_defs,  
			     ...}
			  = Instruction.uses_defs_kills instruction

			val {assembly = assembly_post,
			     registerAllocation}
			  = RA.post {uses = uses,
				     final_uses = final_uses,
				     defs = defs,
				     final_defs = final_defs,
				     info = info,
				     registerAllocation = registerAllocation}
		      in
			{assembly 
			 = List.concat [assembly_pre,
					 assembly_src_dst,
					 [Assembly.instruction instruction],
					 assembly_post],
			 registerAllocation = registerAllocation}
		      end
		in
		  case (src,dst)
		    of (Operand.MemLoc memloc_src, Operand.MemLoc memloc_dst)
		     => (case RA.allocated
			      {memloc = memloc_src,
			       registerAllocation = registerAllocation}
			   of SOME {register = register_src,
				    sync = sync_src,
				    commit = commit_src,
				    ...}
			    => if List.contains(dead,memloc_src,MemLoc.eq)
			          orelse
				  (List.contains(remove,memloc_src,MemLoc.eq)
				   andalso
				   sync_src)
				 then let
					val {uses,defs,kills} 
					  = Instruction.uses_defs_kills 
					    instruction
					val {assembly = assembly_pre,
					     registerAllocation}
					  = RA.pre {uses = uses,
						    defs = defs,
						    kills = kills,
						    info = info,
						    registerAllocation 
						    = registerAllocation}

					val registerAllocation
					  = RA.remove
					    {memloc = memloc_dst,
					     registerAllocation
					     = registerAllocation}
			  
					val registerAllocation
					  = RA.update
					    {value = {register = register_src,
						      memloc = memloc_dst,
						      weight = 1024,
						      sync = false,
						      commit = commit_src},
					     registerAllocation 
					     = registerAllocation}

					val final_uses = []
					val final_defs 
					  = [Operand.register register_src]

					val {assembly = assembly_post,
					     registerAllocation}
					  = RA.post {uses = uses,
						     final_uses = final_uses,
						     defs = defs,
						     final_defs = final_defs,
						     info = info,
						     registerAllocation 
						     = registerAllocation}
				      in
					{assembly 
					 = List.concat [assembly_pre,
							assembly_post],
					 registerAllocation 
					 = registerAllocation}
				      end
				 else default ()
			    | _ => default ())
		     | _ => default ()
		end
             | CMOVcc {condition, src, dst, size}
               (* Conditional move; p. 112
		* Require src/dst operands as follows:
		*
		*              dst
		*          reg imm lab add 
		*      reg  X
		*  src imm           
		*      lab
		*      add  X
		*)
	     => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val {operand = final_src, 
		       assembly = assembly_src,
		       registerAllocation}
		    = RA.allocateOperand {operand = src,
					  options = {register = true,
						     immediate = false,
						     label = false,
						     address = true}, 
					  info = info,
					  size = size,
					  move = true,
					  supports = [dst],
					  saves = [],
					  force = [],
					  registerAllocation 
					  = registerAllocation}

		  val {operand = final_dst,
		       assembly = assembly_dst,
		       registerAllocation = registerAllocation}
		    = RA.allocateOperand {operand = dst,
					  options = {register = true,
						     immediate = false,
						     label = false,
						     address = false},
					  info = info,
					  size = size,
					  move = false,
					  supports = [],
					  saves = [src,final_src],
					  force = [],
					  registerAllocation 
					  = registerAllocation}

		  val instruction
		    = Instruction.CMOVcc 
		      {condition = condition,
		       src = final_src,
		       dst = final_dst,
		       size = size}

		  val {uses = final_uses,
		       defs = final_defs,  
		       ...}
		    = Instruction.uses_defs_kills instruction

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}
		in
		  {assembly 
		   = List.concat [assembly_pre,
				   assembly_src,
				   assembly_dst,
				   [Assembly.instruction instruction],
				   assembly_post],
		   registerAllocation = registerAllocation}
		end
             | XCHG {src, dst, size}
       	       (* Exchange register/memory with register; p. 754
		* Require src/dst operands as follows:
		*
		*              dst
		*          reg imm lab add 
		*      reg  X           X
		*  src imm           
		*      lab
		*      add  X
		*)
	     => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val {final_src,
		       final_dst,
		       assembly_src_dst,
		       registerAllocation}
		    = allocateSrcDst {src = src,
				      dst = dst,
				      move_dst = true,
				      size = size,
				      info = info,
				      registerAllocation = registerAllocation}

		  val instruction
		    = Instruction.XCHG
		      {src = final_src,
		       dst = final_dst,
		       size = size}

		  val {uses = final_uses,
		       defs = final_defs,  
		       ...}
		    = Instruction.uses_defs_kills instruction

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}
		in
		  {assembly 
		   = List.concat [assembly_pre,
				   assembly_src_dst,
				   [Assembly.instruction instruction],
				   assembly_post],
		   registerAllocation = registerAllocation}
		end
             | pPUSH {src, base, size}
  	       (* Pseudo push a value onto the stack; p. 621
		* Require src operand as follows:
		*
		*               src
		*           reg imm lab add
		*            *   X       X
		*   * only word or long registers
		*
		*               base
		*           reg imm lab add
		*            *
		*   * only %esp
		*)
             => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val {operand = final_base,
		       assembly = assembly_base,
		       registerAllocation}
		    = RA.allocateOperand {operand = base,
					  options = {register = true,
						     immediate = false,
						     label = false,
						     address = false},
					  info = info,
					  size = Size.LONG,
					  move = true,
					  supports = [src],
					  saves = [],
					  force = [Register.esp],
					  registerAllocation 
					  = registerAllocation}

		  val options
		    = case size
			of Size.WORD
			 => {register = true,
			     immediate = true,
			     label = false,
			     address = true}
			 | Size.LONG
			 => {register = true,
			     immediate = true,
			     label = false,
			     address = true}
			 | _
			 => {register = false,
			     immediate = true,
			     label = false,
			     address = true}

		  val {operand = final_src, 
		       assembly = assembly_src,
		       registerAllocation}
		    = RA.allocateOperand {operand = src,
					  options = options,
					  info = info,
					  size = size,
					  move = true,
					  supports = [],
					  saves = [],
					  force = [],
					  registerAllocation 
					  = registerAllocation}

		  val instruction
		    = Instruction.PUSH
		      {src = final_src,
		       size = size}

		  val {uses = final_uses,
		       defs = final_defs,  
		       ...}
		    = Instruction.uses_defs_kills instruction

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}
		in
		  {assembly 
		   = List.concat [assembly_pre,
				   assembly_base,
				   assembly_src,
				   [Assembly.instruction instruction],
				   assembly_post],
		   registerAllocation = registerAllocation}
		end
             | pPOP {dst, base, size}
	       (* Pseudo pop a value from the stack; p. 571
		* Require dst operand as follows:
		*
		*               dst
		*           reg imm lab add
		*            *           X
		*   * only word or long registers
		*               base
		*           reg imm lab add
		*            *
		*   * only %esp
		*)
             => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val {operand = final_base,
		       assembly = assembly_base,
		       registerAllocation}
		    = RA.allocateOperand {operand = base,
					  options = {register = true,
						     immediate = false,
						     label = false,
						     address = false},
					  info = info,
					  size = Size.LONG,
					  move = true,
					  supports = [dst],
					  saves = [],
					  force = [Register.esp],
					  registerAllocation 
					  = registerAllocation}

		  val options
		    = case size
			of Size.WORD
			 => {register = true,
			     immediate = false,
			     label = false,
			     address = true}
			 | Size.LONG
			 => {register = true,
			     immediate = false,
			     label = false,
			     address = true}
			 | _
			 => {register = false,
			     immediate = false,
			     label = false,
			     address = true}

		  val {operand = final_dst, 
		       assembly = assembly_dst,
		       registerAllocation}
		    = RA.allocateOperand {operand = dst,
					  options = options,
					  info = info,
					  size = size,
					  move = false,
					  supports = [],
					  saves = [],
					  force = [],
					  registerAllocation 
					  = registerAllocation}

		  val instruction
		    = Instruction.POP
		      {dst = final_dst,
		       size = size}

		  val {uses = final_uses,
		       defs = final_defs,  
		       ...}
		    = Instruction.uses_defs_kills instruction

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}
		in
		  {assembly 
		   = List.concat [assembly_pre,
				   assembly_base,
				   assembly_dst,
				   [Assembly.instruction instruction],
				   assembly_post],
		   registerAllocation = registerAllocation}
		end
             | MOVX {oper, src, dst, srcsize, dstsize}
	       (* Move with extention.
		* Require src/dst operands as follows:
		*
		*              dst
		*          reg imm lab add 
		*      reg  X
		*  src imm
		*      lab
		*      add  X
		*)
	     => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val {operand = final_src, 
		       assembly = assembly_src,
		       registerAllocation}
		    = RA.allocateOperand {operand = src,
					  options = {register = true,
						     immediate = false,
						     label = false,
						     address = true}, 
					  info = info,
					  size = srcsize,
					  move = true,
					  supports = [dst],
					  saves = [],
					  force = [],
					  registerAllocation 
					  = registerAllocation}

		  val {operand = final_dst,
		       assembly = assembly_dst,
		       registerAllocation = registerAllocation}
		    = RA.allocateOperand {operand = dst,
					  options = {register = true,
						     immediate = false,
						     label = false,
						     address = false},
					  info = info,
					  size = dstsize,
					  move = false,
					  supports = [],
					  saves = [src,final_src],
					  force = [],
					  registerAllocation 
					  = registerAllocation}

		  val instruction
		    = Instruction.MOVX
		      {oper = oper,
		       src = final_src,
		       dst = final_dst,
		       srcsize = srcsize,
		       dstsize = dstsize}

		  val {uses = final_uses,
		       defs = final_defs,  
		       ...}
		    = Instruction.uses_defs_kills instruction

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}
		in
		  {assembly 
		   = List.concat [assembly_pre,
				   assembly_src,
				   assembly_dst,
				   [Assembly.instruction instruction],
				   assembly_post],
		   registerAllocation = registerAllocation}
		end
             | XVOM {src, dst, srcsize, dstsize}
	       (* Move with contraction.
		* Require src/dst operands as follows:
		*
		*              dst
		*          reg imm lab add 
		*      reg  X           X
		*  src imm
		*      lab
		*      add
		*)
	     => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val {operand = final_src, 
		       assembly = assembly_src,
		       registerAllocation}
		    = RA.allocateOperand {operand = src,
					  options = {register = true,
						     immediate = false,
						     label = false,
						     address = false}, 
					  info = info,
					  size = srcsize,
					  move = true,
					  supports = [dst],
					  saves = [],
					  force 
					  = Register.withLowPart (srcsize,
								  dstsize),
					  registerAllocation 
					  = registerAllocation}

		  val {operand = final_dst,
		       assembly = assembly_dst,
		       registerAllocation = registerAllocation}
		    = RA.allocateOperand {operand = dst,
					  options = {register = true,
						     immediate = false,
						     label = false,
						     address = true},
					  info = info,
					  size = dstsize,
					  move = false,
					  supports = [],
					  saves = [src,final_src],
					  force = [],
					  registerAllocation 
					  = registerAllocation}

		  val {uses = final_uses,
		       defs = final_defs,  
		       ...}
		    = Instruction.uses_defs_kills
		      (Instruction.XVOM
		       {src = final_src,
			dst = final_dst,
			srcsize = srcsize,
			dstsize = dstsize})

		  val temp_reg
		    = case final_src
			of Operand.Register r 
			 => Register.lowPartOf (r, Size.BYTE)
			 | _ 
			 => Error.bug "allocateRegisters: XVOM, temp_reg"

		  val instruction
		     = Instruction.MOV
		       {src = Operand.register temp_reg,
			dst = final_dst,
			size = dstsize}

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}
		in
		  {assembly 
		   = List.concat [assembly_pre,
				   assembly_src,
				   assembly_dst,
				   [Assembly.instruction instruction],
				   assembly_post],
		   registerAllocation = registerAllocation}
		end
             | LEA {src, dst, size}
	       (* Load effective address; p. 393
		* Require src/dst operands as follows:
		*
		*              dst
		*          reg imm lab add 
		*      reg
		*  src imm
		*      lab
		*      add  X
		*)
	     => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val {operand = final_src, 
		       assembly = assembly_src,
		       registerAllocation}
		    = RA.allocateOperand {operand = src,
					  options = {register = false,
						     immediate = false,
						     label = false,
						     address = true}, 
					  info = info,
					  size = size,
					  move = true,
					  supports = [dst],
					  saves = [],
					  force = [],
					  registerAllocation 
					  = registerAllocation}

		  val {operand = final_dst,
		       assembly = assembly_dst,
		       registerAllocation = registerAllocation}
		    = RA.allocateOperand {operand = dst,
					  options = {register = true,
						     immediate = false,
						     label = false,
						     address = false},
					  info = info,
					  size = size,
					  move = false,	
					  supports = [],
					  saves = [src,final_src],
					  force = [],
					  registerAllocation 
					  = registerAllocation}

		  val instruction
		    = Instruction.LEA
		      {src = final_src,
		       dst = final_dst,
		       size = size}

		  val {uses = final_uses,
		       defs = final_defs,  
		       ...}
		    = Instruction.uses_defs_kills instruction

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}
		in
		  {assembly 
		   = List.concat [assembly_pre,
				   assembly_src,
				   assembly_dst,
				   [Assembly.instruction instruction],
				   assembly_post],
		   registerAllocation = registerAllocation}
		end
	     | pFMOV {src, dst, size}
	       (* Pseudo floating-point move.
		*)
	     => (let
		  fun default ()
		    = let
			val {uses,defs,kills} 
			  = Instruction.uses_defs_kills instruction
			val {assembly = assembly_pre,
			     registerAllocation}
			  = RA.pre {uses = uses,
				    defs = defs,
				    kills = kills,
				    info = info,
				    registerAllocation = registerAllocation}

			val {operand = final_src,
			     assembly = assembly_src,
			     rename = rename_src,
			     registerAllocation}
			  = RA.allocateFltOperand 
			    {operand = src,
			     options = {fltregister = true,
					address = true},
			     info = info,
			     size = size,
			     move = true,
			     supports = [dst],
			     saves = [],
			     top = SOME false,
			     registerAllocation
			     = registerAllocation}

			val {operand = final_dst,
			     assembly = assembly_dst,
			     rename = rename_dst,
			     registerAllocation}
			  = RA.allocateFltOperand 
			    {operand = dst,
			     options = {fltregister = true,
					address = false},
			     info = info,
			     size = size,
			     move = false,
			     supports = [],
			     saves = [src,final_src],
			     top = NONE,
			     registerAllocation
			     = registerAllocation}

			val final_src = (RA.renameLift rename_dst) final_src

			val instruction
			  = Instruction.FLD
			    {src = final_src,
			     size = size}

                        val {uses = final_uses,
			     defs = final_defs,  
			     ...}
			  = Instruction.uses_defs_kills instruction

			val {assembly = assembly_post,
			     registerAllocation}
			  = RA.post {uses = uses,
				     final_uses = final_uses,
				     defs = defs,
				     final_defs = final_defs,
				     info = info,
				     registerAllocation = registerAllocation}
		      in
			{assembly = List.concat
			            [assembly_pre,
				     assembly_src,	
				     assembly_dst,
				     [Assembly.instruction instruction],
				     assembly_post],
			 registerAllocation = registerAllocation}
		      end

		  fun default' ()
		    = let
			val {uses,defs,kills} 
			  = Instruction.uses_defs_kills instruction
			val {assembly = assembly_pre,
			     registerAllocation}
			  = RA.pre {uses = uses,
				    defs = defs,
				    kills = kills,
				    info = info,
				    registerAllocation = registerAllocation}

			val {operand = final_src,
			     assembly = assembly_src,
			     rename = rename_src,
			     registerAllocation}
			  = RA.allocateFltOperand 
			    {operand = src,
			     options = {fltregister = true,
					address = false},
			     info = info,
			     size = size,
			     move = true,
			     supports = [dst],
			     saves = [],
			     top = SOME true,
			     registerAllocation = registerAllocation}

			val {operand = final_dst,
			     assembly = assembly_dst,
			     rename = rename_dst,
			     registerAllocation}
			  = RA.allocateFltOperand 
			    {operand = dst,
			     options = {fltregister = false,
					address = true},
			     info = info,
			     size = size,
			     move = false,
			     supports = [],
			     saves = [src,final_src],
			     top = SOME false,
			     registerAllocation = registerAllocation}
			    
			val final_src = (RA.renameLift rename_dst) final_src

			val instruction
			  = Instruction.FST
			    {dst = final_dst,
			     size = size,
			     pop = true}
			    
			val {rename = rename_pop,
			     registerAllocation}
			  = RA.fltpop {registerAllocation = registerAllocation}
			    
			val {uses = final_uses,
			     defs = final_defs,
			     ...}
			  = Instruction.uses_defs_kills instruction
		    
			val final_uses
			  = List.map(final_uses, RA.renameLift rename_pop)
			val final_defs
			  = List.map(final_defs, RA.renameLift rename_pop)

			val {assembly = assembly_post,
			     registerAllocation}
			  = RA.post {uses = uses,
				     final_uses = final_uses,
				     defs = defs,
				     final_defs = final_defs,
				     info = info,
				     registerAllocation = registerAllocation}
		      in
			{assembly = List.concat 
			            [assembly_pre,
				     assembly_src,
				     assembly_dst,
				     [Assembly.instruction instruction],
				     assembly_post],
			 registerAllocation = registerAllocation}
		      end
		in
		  case (src,dst)
		    of (Operand.MemLoc memloc_src,
			Operand.MemLoc memloc_dst)
		     => (case (RA.fltallocated {memloc = memloc_src,
						registerAllocation 
						= registerAllocation},
			       RA.fltallocated {memloc = memloc_dst,
						registerAllocation 
						= registerAllocation})
			   of (SOME {fltregister = fltregister_src, 
				     sync = sync_src,
				     commit = commit_src, 
				     ...},
			       NONE)
			    => if List.contains(dead,memloc_src,MemLoc.eq)
			          orelse
				  (List.contains(remove,memloc_src,MemLoc.eq)
				   andalso
				   sync_src)
				 then if List.contains(remove,
						       memloc_dst,
						       MemLoc.eq)
					then default' ()
					else let
					       val registerAllocation
						 = RA.fltupdate 
						   {value = {fltregister 
							     = fltregister_src,
							     memloc 
							     = memloc_dst,
							     weight = 1024,
							     sync = false,
							     commit 
							     = commit_src},
						    registerAllocation 
						    = registerAllocation}

					       val {uses,defs,kills} 
						 = Instruction.uses_defs_kills
						   instruction
					       val {assembly = assembly_pre,
						    registerAllocation}
						 = RA.pre 
						   {uses = uses,
						    defs = defs,
						    kills = kills,
						    info = info,
						    registerAllocation 
						    = registerAllocation}

					       val final_uses = []
					       val final_defs 
						 = [Operand.fltregister 
						    fltregister_src]

					       val {assembly = assembly_post,
						    registerAllocation}
						 = RA.post 
						   {uses = uses,
						    final_uses = final_uses,
						    defs = defs,
						    final_defs = final_defs,
						    info = info,
						    registerAllocation 
						    = registerAllocation}
					     in
					       {assembly = List.concat 
						           [assembly_pre,
							    assembly_post],
						registerAllocation 
						= registerAllocation}
					     end
				 else default ()
			    | _ => default ())
                     | _ => default ()
		end)
             | pFLDC {oper, dst, size}
             (* Pseudo floating-point load constant.
	      *)
             => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val {operand = final_dst,
		       assembly = assembly_dst,
		       rename = rename_dst,
		       registerAllocation}
		    = RA.allocateFltOperand {operand = dst,
					     options = {fltregister = true,
							address = false},
					     info = info,
					     size = size,
					     move = false,
					     supports = [],
					     saves = [],
					     top = NONE,
					     registerAllocation
					     = registerAllocation}

		  val instruction
		    = Instruction.FLDC
		      {oper = oper}

		  val {uses = final_uses,
		       defs = final_defs,  
		       ...}
		    = Instruction.uses_defs_kills instruction

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}
		in
		  {assembly = List.concat
		              [assembly_pre,
			       assembly_dst,
			       [Assembly.instruction instruction],
			       assembly_post],
		   registerAllocation = registerAllocation}
		end
	     | pFMOVFI {src, dst, srcsize, dstsize}
	     (* Pseudo floating-point from integer.
	      *)
	     => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val {operand = final_src,
		       assembly = assembly_src,
		       registerAllocation}
		    = RA.allocateOperand {operand = src,
					  options = {register = false,
						     immediate = false,
						     label = false,
						     address = true},
					  info = info,
					  size = srcsize,
					  move = true,	
					  supports = [dst],
					  saves = [],
					  force = [],
					  registerAllocation
					  = registerAllocation}

		  val {operand = final_dst,
		       assembly = assembly_dst,
		       rename = rename_dst,
		       registerAllocation}
		    = RA.allocateFltOperand {operand = dst,
					     options = {fltregister = true,
							address = false},
					     info = info,
					     size = dstsize,
					     move = false,	
					     supports = [],
					     saves = [src,final_src],
					     top = NONE,
					     registerAllocation
					     = registerAllocation}

		  val instruction
		    = Instruction.FILD
                      {src = final_src,
		       size = Size.toFPI srcsize}

		  val {uses = final_uses,
		       defs = final_defs,  
		       ...}
		    = Instruction.uses_defs_kills instruction

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}
		in
		  {assembly = List.concat
		              [assembly_pre,
                               assembly_src,
			       assembly_dst,
			       [Assembly.instruction instruction],
			       assembly_post],
		   registerAllocation = registerAllocation}
		end
	     | pFMOVTI {src, dst, srcsize, dstsize}
	     (* Pseudo floating-point to integer.
	      *)
	     => let
		  fun default ()
		    = let
			val {uses,defs,kills} 
			  = Instruction.uses_defs_kills instruction
			val {assembly = assembly_pre,
			     registerAllocation}
			  = RA.pre {uses = uses,
				    defs = defs,
				    kills = kills,
				    info = info,
				    registerAllocation = registerAllocation}

			val {operand = final_src,
			     assembly = assembly_src,
			     rename = rename_src,
			     registerAllocation}
			  = RA.allocateFltOperand 
			    {operand = src,
			     options = {fltregister = true,
					address = false},
			     info = info,
			     size = srcsize,
			     move = true,
			     supports = [dst],
			     saves = [],
			     top = SOME true,
			     registerAllocation = registerAllocation}

			val {operand = final_dst,
			     assembly = assembly_dst,
			     registerAllocation}
			  = RA.allocateOperand 
			    {operand = dst,
			     options = {register = false,
					immediate = false,
					label = false,
					address = true},
			     info = info,
			     size = dstsize,
			     move = false,
			     supports = [],
			     saves = [src,final_src],
			     force = [],
			     registerAllocation = registerAllocation}

			val instruction
			  = Instruction.FIST
			    {dst = final_dst,
			     size = Size.toFPI dstsize,
			     pop = false}

			val {uses = final_uses,
			     defs = final_defs,  
			     ...}
			  = Instruction.uses_defs_kills instruction

			val {assembly = assembly_post,
			     registerAllocation}
			  = RA.post {uses = uses,
				     final_uses = final_uses,
				     defs = defs,
				     final_defs = final_defs,
				     info = info,
				     registerAllocation = registerAllocation}
		      in
			{assembly = List.concat
			            [assembly_pre,
				     assembly_src,
				     assembly_dst,
				     [Assembly.instruction instruction],
				     assembly_post],
			 registerAllocation = registerAllocation}
		      end

		  fun default' ()
		    = let
			val {uses,defs,kills} 
			  = Instruction.uses_defs_kills instruction
			val {assembly = assembly_pre,
			     registerAllocation}
			  = RA.pre {uses = uses,
				    defs = defs,
				    kills = kills,
				    info = info,
				    registerAllocation = registerAllocation}

			val {operand = final_src,
			     assembly = assembly_src,
			     rename = rename_src,
			     registerAllocation}
			  = RA.allocateFltOperand 
			    {operand = src,
			     options = {fltregister = true,
					address = false},
			     info = info,
			     size = srcsize,
			     move = true,
			     supports = [dst],
			     saves = [],
			     top = SOME true,
			     registerAllocation = registerAllocation}

			val {operand = final_dst,
			     assembly = assembly_dst,
			     rename = rename_dst,
			     registerAllocation}
			  = RA.allocateFltOperand 
			    {operand = dst,
			     options = {fltregister = false,
					address = true},
			     info = info,
			     size = dstsize,
			     move = false,
			     supports = [],
			     saves = [src,final_src],
			     top = SOME false,
			     registerAllocation = registerAllocation}

			val final_src 
			  = (RA.renameLift rename_dst) final_src

			val instruction
			  = Instruction.FIST
			    {dst = final_dst,
			     size = Size.toFPI dstsize,
			     pop = true}

			val {rename = rename_pop,
			     registerAllocation}
			  = RA.fltpop {registerAllocation = registerAllocation}

			val {uses = final_uses,
			     defs = final_defs,
			     ...}
			  = Instruction.uses_defs_kills instruction
 
			val final_uses
			  = List.map(final_uses, RA.renameLift rename_pop)
			val final_defs
			  = List.map(final_defs, RA.renameLift rename_pop)

			val {assembly = assembly_post,
			     registerAllocation}
			  = RA.post {uses = uses,
				     final_uses = final_uses,
				     defs = defs,
				     final_defs = final_defs,
				     info = info,
				     registerAllocation 
				     = registerAllocation}
		      in
			{assembly = List.concat 
			            [assembly_pre,
				     assembly_src,
				     assembly_dst,
				     [Assembly.instruction instruction],
				     assembly_post],
			 registerAllocation = registerAllocation}
		      end
		in
		  case src
		    of Operand.MemLoc memloc_src
		     => if removable {memloc = memloc_src,
				      info = info,
				      registerAllocation
				      = registerAllocation}
			  then default' ()
			  else default ()
		     | _ => default ()
		end
	     | pFCOM {src1, src2, size}
	       (* Floating-point compare real; p. 220
	        * Require src operand as follows:
		*
		*               src
		*           fltreg add 
		*             *     X
		*   * only st(1) if pop and pop'
	        *
		* Require size modifier class as follows: FLT(SNGL,DBLE)
		*)
	     => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}
		    
		  val {final_src1,
		       final_src2,
		       assembly_src1_src2,
		       rename_src1_src2,
		       pop,
		       pop',
		       registerAllocation}
		    = if Operand.eq(src1,src2)
			then let
			       fun default b
				 = let
				     val {operand = final_src1_src2,
					  assembly = assembly_src1_src2,
					  rename = rename_src1_src2,
					  registerAllocation}
				       = RA.allocateFltOperand 
				         {operand = src1,
					  options = {fltregister = true,
						     address = false},
					  info = info,
					  size = size,
					  move = true,
					  supports = [],
					  saves = [],
					  top = SOME true,
					  registerAllocation
					  = registerAllocation}
				   in
				     {final_src1 = final_src1_src2,
				      final_src2 = final_src1_src2,
				      assembly_src1_src2 = assembly_src1_src2,
				      rename_src1_src2 = rename_src1_src2,
				      pop = b,
				      pop' = false,
				      registerAllocation = registerAllocation}
				   end
			     in
			       case src1
				 of Operand.MemLoc memloc_src1
				  => if removable {memloc = memloc_src1,
						   info = info,
						   registerAllocation
						   = registerAllocation}
				       then default true
				       else default false
				  | _ => default false
			     end
			else let
			       fun default b
				 = let
				     val {operand = final_src2,
					  assembly = assembly_src2,
					  rename = rename_src2,
					  registerAllocation}
				       = RA.allocateFltOperand 
				         {operand = src2,
					  options = {fltregister = true,
						     address = true},
					  info = info,
					  size = size,
					  move = true,
					  supports = [src1],
					  saves = [],
					  top = SOME false,
					  registerAllocation
					  = registerAllocation}
					 
				     val {operand = final_src1,
					  assembly = assembly_src1,
					  rename = rename_src1,
					  registerAllocation}
				       = RA.allocateFltOperand 
				       {operand = src1,
					options = {fltregister = true,
						   address = false},
					info = info,
					size = size,
					move = true,	
					supports = [],
					saves = [src2,final_src2],
					top = SOME true,
					registerAllocation
					= registerAllocation}
		    
				     val final_src2 
				       = (RA.renameLift rename_src1) final_src2
				   in
				     {final_src1 = final_src1,
				      final_src2 = final_src2,
				      assembly_src1_src2 = List.concat 
				                           [assembly_src2,
							    assembly_src1],
				      rename_src1_src2 = rename_src1 o 
				                         rename_src2,
				      pop = b,
				      pop' = false,
				      registerAllocation = registerAllocation}
				   end

			       fun default' ()
				 = let
				     val {operand_top = final_src1,
					  operand_one = final_src2,
					  assembly = assembly_src1_src2,
					  rename = rename_src1_src2,
					  registerAllocation}
				       = RA.allocateFltStackOperands
				         {operand_top = src1,
					  move_top = true,
					  size_top = size,
					  operand_one = src2,
					  move_one = true,
					  size_one = size,
					  info = info,
					  supports = [],
					  saves = [],
					  registerAllocation 
					  = registerAllocation}
				   in
				     {final_src1 = final_src1,
				      final_src2 = final_src2,
				      assembly_src1_src2 = assembly_src1_src2,
				      rename_src1_src2 = rename_src1_src2,
				      pop = true,
				      pop' = true,
				      registerAllocation = registerAllocation}
				   end
			     in
			       case (src1,src2)
				 of (Operand.MemLoc memloc_src1,
				     Operand.MemLoc memloc_src2)
				  => if removable {memloc = memloc_src1,
						   info = info,
						   registerAllocation
						   = registerAllocation}
				       then case RA.fltallocated
					         {memloc = memloc_src2,
						  registerAllocation
						  = registerAllocation}
					      of SOME {sync,...}
					       => if List.contains
						     (dead,
						      memloc_src2,
						      MemLoc.eq)
						     orelse
						     (List.contains
						      (remove,
						       memloc_src2,
						       MemLoc.eq)
						      andalso
						      sync)
						    then default' ()
						    else default true
					       | NONE
					       => default true
				       else default false
				  | (Operand.MemLoc memloc_src1, _)
				  => if removable {memloc = memloc_src1,
						   info = info,
						   registerAllocation
						   = registerAllocation}
				       then default true
				       else default false
				  | _ => default false
			     end
 
		  val instruction
		    = Instruction.FCOM
		      {src = final_src2,
		       size = size,
		       pop = pop,
		       pop' = pop'}
			    
		  val {rename = rename_pop,
		       registerAllocation}
		    = if pop
			then if pop'
			       then let
				      val {rename = rename_pop,
					   registerAllocation}
					= RA.fltpop {registerAllocation 
						     = registerAllocation}
				      val {rename = rename_pop',
					   registerAllocation}
					= RA.fltpop {registerAllocation 
						     = registerAllocation}
				    in
				      {rename = rename_pop' o rename_pop,
				       registerAllocation= registerAllocation}
				    end
			       else let
				      val {rename = rename_pop,
					   registerAllocation}
					= RA.fltpop {registerAllocation 
						     = registerAllocation}
				    in
				      {rename = rename_pop,
				       registerAllocation = registerAllocation}
				    end
			else {rename = FltRegister.id,
			      registerAllocation = registerAllocation}

		  val {uses = final_uses,
		       defs = final_defs,
		       ...}
		    = Instruction.uses_defs_kills instruction
		    
		  val final_uses
		    = List.map(final_uses, RA.renameLift rename_pop)
		  val final_defs
		    = List.map(final_defs, RA.renameLift rename_pop)

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}
		in
		  {assembly = List.concat [assembly_pre,
					    assembly_src1_src2,
					    [Assembly.instruction instruction],
					    assembly_post],
		   registerAllocation = registerAllocation}
		end
	     | pFUCOM {src1, src2, size}
	       (* Floating-point unordered compare real; p. 307
	        * Require src operand as follows:
		*
		*               src
		*           fltreg add 
		*             *
		*   * only st(1) if pop and pop'
		*)
	     => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val {final_src1,
		       final_src2,
		       assembly_src1_src2,
		       rename_src1_src2,
		       pop,
		       pop',
		       registerAllocation}
		    = if Operand.eq(src1,src2)
			then let
			       fun default b
				 = let
				     val {operand = final_src1_src2,
					  assembly = assembly_src1_src2,
					  rename = rename_src1_src2,
					  registerAllocation}
				       = RA.allocateFltOperand 
				         {operand = src1,
					  options = {fltregister = true,
						     address = false},
					  info = info,
					  size = size,
					  move = true,
					  supports = [],
					  saves = [],
					  top = SOME true,
					  registerAllocation
					  = registerAllocation}
				   in
				     {final_src1 = final_src1_src2,
				      final_src2 = final_src1_src2,
				      assembly_src1_src2 = assembly_src1_src2,
				      rename_src1_src2 = rename_src1_src2,
				      pop = b,
				      pop' = false,
				      registerAllocation = registerAllocation}
				   end
			     in
			       case src1
				 of Operand.MemLoc memloc_src1
				  => if removable {memloc = memloc_src1,
						   info = info,
						   registerAllocation
						   = registerAllocation}
				       then default true
				       else default false
				  | _ => default false
			     end
			else let
			       fun default b
				 = let
				     val {operand = final_src2,
					  assembly = assembly_src2,
					  rename = rename_src2,
					  registerAllocation}
				       = RA.allocateFltOperand 
				         {operand = src2,
					  options = {fltregister = true,
						     address = false},
					  info = info,
					  size = size,
					  move = true,
					  supports = [src1],
					  saves = [],
					  top = SOME false,
					  registerAllocation
					  = registerAllocation}
					 
				     val {operand = final_src1,
					  assembly = assembly_src1,
					  rename = rename_src1,
					  registerAllocation}
				       = RA.allocateFltOperand 
				       {operand = src1,
					options = {fltregister = true,
						   address = false},
					info = info,
					size = size,
					move = true,	
					supports = [],
					saves = [src2,final_src2],
					top = SOME true,
					registerAllocation
					= registerAllocation}
		    
				     val final_src2 
				       = (RA.renameLift rename_src1) final_src2
				   in
				     {final_src1 = final_src1,
				      final_src2 = final_src2,
				      assembly_src1_src2 = List.concat
				                           [assembly_src2,
							    assembly_src1],
				      rename_src1_src2 = rename_src1 o 
				                         rename_src2,
				      pop = b,
				      pop' = false,
				      registerAllocation = registerAllocation}
				   end
			     in
			       case (src1,src2)
				 of (Operand.MemLoc memloc_src1,
				     Operand.MemLoc memloc_src2)
				  => let
				       fun default' ()
					 = case RA.fltallocated 
					        {memloc = memloc_src2,
						 registerAllocation
						 = registerAllocation}
					     of SOME _
					      => let
						   val {operand_top 
							= final_src1,
							operand_one 
							= final_src2,
							assembly 
							= assembly_src1_src2,
							rename 
							= rename_src1_src2,
							registerAllocation}
						     = RA.allocateFltStackOperands
						       {operand_top = src1,
							move_top = true,
							size_top = size,
							operand_one = src2,
							move_one = true,
							size_one = size,
							info = info,
							supports = [],
							saves = [],
							registerAllocation 
							= registerAllocation}
						 in
						   {final_src1 = final_src1,
						    final_src2 = final_src2,
						    assembly_src1_src2 
						    = assembly_src1_src2,
						    rename_src1_src2 
						    = rename_src1_src2,
						    pop = true,
						    pop' = true,
						    registerAllocation 
						    = registerAllocation}
						 end
					      | NONE
					      => default true
				     in 
				       if removable 
					  {memloc = memloc_src1,
					   info = info,
					   registerAllocation
					   = registerAllocation}
					 then if removable 
					         {memloc = memloc_src2,
						  info = info,
						  registerAllocation
						  = registerAllocation}
						then default' ()
						else default true
					 else default true
				     end
				  | (Operand.MemLoc memloc_src1, _)
				  => if removable {memloc = memloc_src1,
						   info = info,
						   registerAllocation
						   = registerAllocation}
				       then default true
				       else default false
				  | _ => default false
			     end
 
		  val instruction
		    = Instruction.FUCOM
		      {src = final_src2,
		       pop = pop,
		       pop' = pop'}

		  val {rename = rename_pop,
		       registerAllocation}
		    = if pop
			then if pop'
			       then let
				      val {rename = rename_pop,
					   registerAllocation}
					= RA.fltpop {registerAllocation 
						     = registerAllocation}
				      val {rename = rename_pop',
					   registerAllocation}
					= RA.fltpop {registerAllocation 
						     = registerAllocation}
				    in
				      {rename = rename_pop' o rename_pop,
				       registerAllocation= registerAllocation}
				    end
			       else let
				      val {rename = rename_pop,
					   registerAllocation}
					= RA.fltpop {registerAllocation 
						     = registerAllocation}
				    in
				      {rename = rename_pop,
				       registerAllocation = registerAllocation}
				    end
			else {rename = FltRegister.id,
			      registerAllocation = registerAllocation}

		  val {uses = final_uses,
		       defs = final_defs,
		       ...}
		    = Instruction.uses_defs_kills instruction
		    
		  val final_uses
		    = List.map(final_uses, RA.renameLift rename_pop)
		  val final_defs
		    = List.map(final_defs, RA.renameLift rename_pop)

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}
		in
		  {assembly = List.concat [assembly_pre,
					    assembly_src1_src2,
					    [Assembly.instruction instruction],
					    assembly_post],
		   registerAllocation = registerAllocation}
		end
	     | pFBinA {oper, src, dst, size}
	       (* Floating-point binary arithmetic instructions.
		* Require src operand as follows:
		*
		*               src
		*           fltreg add 
		*             *     X
		*   * only st(0) if pop
		*
		* Require dst operand as follows:
		*
		*               dst
		*           fltreg add 
		*            *
		*   * only st(0) if src add
		*
		*   * one of src,dst must be st(0)
		*
		* Require size modifier class as follows: FLT
		*)		
	     => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val {final_src,
		       final_dst,
		       assembly_src_dst,
		       rename_src_dst,
		       oper,
		       pop,
		       registerAllocation}
		    = if Operand.eq(src,dst)
			then let
			       val {operand = final_src_dst,
				    assembly = assembly_src_dst,
				    rename = rename_src_dst,
				    registerAllocation}
				 = RA.allocateFltOperand 
				   {operand = dst,
				    options = {fltregister = true,
					       address = false},
				    info = info,
				    size = size,
				    move = true,	
				    supports = [],
				    saves = [],
				    top = SOME true,
				    registerAllocation 
				    = registerAllocation}
			     in
			       {final_src = final_src_dst,
				final_dst = final_src_dst,
				assembly_src_dst = assembly_src_dst,
				rename_src_dst = rename_src_dst,
				oper = oper,
				pop = false,
				registerAllocation = registerAllocation}
			     end
			else let
			       fun default ()
				 = let
				     val {operand = final_src,
					  assembly = assembly_src,
					  rename = rename_src,
					  registerAllocation}
				       = RA.allocateFltOperand 
				         {operand = src,
					  options = {fltregister = true,
						     address = true},
					  info = info,
					  size = size,
					  move = true,	
					  supports = [dst],
					  saves = [],
					  top = SOME false,
					  registerAllocation
					  = registerAllocation}

				     val {operand = final_dst,
					  assembly = assembly_dst,
					  rename = rename_dst,
					  registerAllocation}
				       = case final_src
					   of Operand.Address _
					    => RA.allocateFltOperand 
					       {operand = dst,
						options = {fltregister = true,
							   address = false},
						info = info,
						size = size,
						move = true,
						supports = [],
						saves = [src,final_src],
						top = SOME true,
						registerAllocation 
						= registerAllocation}
				            | Operand.FltRegister f
				            => if FltRegister.eq
					          (f, FltRegister.top)
						 then RA.allocateFltOperand 
					              {operand = dst,
						       options 
						       = {fltregister = true,
							  address = false},
						       info = info,
						       size = size,
						       move = true,
						       supports = [],
						       saves = [src,final_src],
						       top = SOME false,
						       registerAllocation 
						       = registerAllocation}
						 else RA.allocateFltOperand 
						      {operand = dst,
						       options 
						       = {fltregister = true,
							  address = false},
						       info = info,
						       size = size,
						       move = true,	
						       supports = [],
						       saves = [src,final_src],
						       top = SOME true,
						       registerAllocation 
						       = registerAllocation}
					    | _
				            => Error.bug 
					       "allocateRegisters: pFBinA, final_src"

				     val final_src 
				       = (RA.renameLift rename_dst) final_src
				   in
				     {final_src = final_src,
				      final_dst = final_dst,
				      assembly_src_dst = List.concat
				                         [assembly_src,
							  assembly_dst],
				      rename_src_dst = rename_dst o
				                       rename_src,
				      oper = oper,
				      pop = false,
				      registerAllocation = registerAllocation}
				   end

			       fun default' ()
				 = let
				     val {operand = final_dst,
					  assembly = assembly_dst,
					  rename = rename_dst,
					  registerAllocation}
				       = RA.allocateFltOperand 
				         {operand = dst,
					  options = {fltregister = true,
						     address = false},
					  info = info,
					  size = size,
					  move = true,
					  supports = [src],
					  saves = [],
					  top = SOME false,
					  registerAllocation
					  = registerAllocation}
					 
				     val {operand = final_src,
					  assembly = assembly_src,
					  rename = rename_src,
					  registerAllocation}
				       = RA.allocateFltOperand 
				         {operand = src,
					  options = {fltregister = true,
						     address = false},
					  info = info,
					  size = size,
					  move = true,	
					  supports = [],
					  saves = [dst,final_dst],
					  top = SOME true,
					  registerAllocation
					  = registerAllocation}
		    
				     val final_dst 
				       = (RA.renameLift rename_src) final_dst
				   in
				     {final_src = final_src,
				      final_dst = final_dst,
				      assembly_src_dst = List.concat
				                         [assembly_dst,
							  assembly_src],
				      rename_src_dst = rename_src o 
				                       rename_dst,
				      oper = oper,
				      pop = true,
				      registerAllocation = registerAllocation}
				   end

			       fun default'' value_dst
				 = let
				     val {operand = final_dst,
					  assembly = assembly_dst,
					  rename = rename_dst,
					  registerAllocation}
				       = RA.allocateFltOperand 
				         {operand = dst,
					  options = {fltregister = true,
						     address = false},
					  info = info,
					  size = size,
					  move = true,
					  supports = [src],
					  saves = [],
					  top = SOME true,
					  registerAllocation
					  = registerAllocation}
					 
				     val {operand = final_src,
					  assembly = assembly_src,
					  rename = rename_src,
					  registerAllocation}
				       = RA.allocateFltOperand 
				         {operand = src,
					  options = {fltregister = true,
						     address = false},
					  info = info,
					  size = size,
					  move = true,	
					  supports = [],
					  saves = [dst,final_dst],
					  top = SOME false,
					  registerAllocation
					  = registerAllocation}

				     val final_dst 
				       = (RA.renameLift rename_src) final_dst

				     val {fltregister = fltregister_dst,
					  memloc = memloc_dst,
					  weight = weight_dst,
					  sync = sync_dst,
					  commit = commit_dst}
				       = value_dst

				     val fltregister_src
				       = case Operand.deFltregister final_src
					   of SOME fltregister => fltregister
					    | NONE 
					    => Error.bug "allocateRegisters: pFBinA, final_src"

				     val registerAllocation
				       = RA.fltupdate
				         {value
					  = {fltregister = fltregister_src,
					     memloc = memloc_dst,
					     weight = weight_dst,
					     sync = sync_dst,
					     commit = commit_dst},
					  registerAllocation
					  = registerAllocation}
				   in
				     {final_src = final_dst,
				      final_dst = final_src,
				      assembly_src_dst = List.concat
				                         [assembly_dst,
							  assembly_src],
				      rename_src_dst = rename_src o 
				                       rename_dst,
				      oper = Instruction.fbina_reverse oper,
				      pop = true,
				      registerAllocation = registerAllocation}
				   end

			       fun default''' memloc_dst
				 = let
				     val {operand = final_dst,
					  assembly = assembly_dst,
					  rename = rename_dst,
					  registerAllocation}
				       = RA.allocateFltOperand 
				         {operand = dst,
					  options = {fltregister = false,
						     address = true},
					  info = info,
					  size = size,
					  move = true,
					  supports = [src],
					  saves = [],
					  top = SOME false,
					  registerAllocation
					  = registerAllocation}
					 
				     val {operand = final_src,
					  assembly = assembly_src,
					  rename = rename_src,
					  registerAllocation}
				       = RA.allocateFltOperand 
				         {operand = src,
					  options = {fltregister = true,
						     address = false},
					  info = info,
					  size = size,
					  move = true,	
					  supports = [],
					  saves = [dst,final_dst],
					  top = SOME true,
					  registerAllocation
					  = registerAllocation}

				     val final_dst 
				       = (RA.renameLift rename_src) final_dst

				     val {rename = rename_pop,
					  registerAllocation}
				       = RA.fltpop 
				         {registerAllocation
					  = registerAllocation}

				     val {rename = rename_push,
					  registerAllocation}
				       = RA.fltpush
				         {value
					  = {fltregister = FltRegister.top,
					     memloc = memloc_dst,
					     weight = 1024,
					     sync = false,
					     commit = RA.NO},
					  registerAllocation
					  = registerAllocation}
				   in
				     {final_src = final_dst,
				      final_dst = final_src,
				      assembly_src_dst = List.concat
				                         [assembly_dst,
							  assembly_src],
				      rename_src_dst = rename_push o
				                       rename_pop o
						       rename_src o 
				                       rename_dst,
				      oper = Instruction.fbina_reverse oper,
				      pop = false,
				      registerAllocation = registerAllocation}
				   end
			     in
			       case (src,dst)
				 of (Operand.MemLoc memloc_src,
				     Operand.MemLoc memloc_dst)
				  => (case (RA.fltallocated 
					    {memloc = memloc_src,
					     registerAllocation 
					     = registerAllocation},
					    RA.fltallocated
					    {memloc = memloc_dst,
					     registerAllocation
					     = registerAllocation})
					of (SOME (value_src as
						  {fltregister
						   = fltregister_src,
						   sync = sync_src,
						   ...}),
					    SOME (value_dst as
						  {fltregister 
						   = fltregister_dst,
						   ...}))
					 => if List.contains(dead,
							     memloc_src,
							     MemLoc.eq)
					       orelse
					       (List.contains(remove,
							      memloc_src,
							      MemLoc.eq)
						andalso
						sync_src)
					      then if FltRegister.eq
						      (fltregister_dst,
						       FltRegister.top)
						     then default'' value_dst
						     else default' ()
					      else default ()
					 | (SOME {sync = sync_src,...},
					    NONE)
					 => if List.contains(dead,
							     memloc_src,
							     MemLoc.eq)
					       orelse
					       (List.contains(remove,
							      memloc_src,
							      MemLoc.eq)
						andalso
						sync_src)
					      then default''' memloc_dst
					      else default ()
                                         | _ => default ())
				  | (Operand.MemLoc memloc_src, _)
				  => (case RA.fltallocated 
					   {memloc = memloc_src,
					    registerAllocation 
					    = registerAllocation}
					of SOME {sync = sync_src,...}
					 => if List.contains(dead,
							     memloc_src,
							     MemLoc.eq)
					       orelse
					       (List.contains(remove,
							      memloc_src,
							      MemLoc.eq)
						andalso
						sync_src)
					      then default' ()
					      else default ()
                                         | _ => default ())
                                  | _ => default ()
			     end

		  val oper
		    = if Operand.eq(final_src,
				    Operand.fltregister FltRegister.top)
		         andalso isSome (Operand.deFltregister final_dst)
			then fbina_reverse oper
			else oper

		  val instruction
		    = Instruction.FBinA
		      {oper = oper,
		       src = final_src,
		       dst = final_dst,
		       size = size,
		       pop = pop}

		  val {rename = rename_pop,
		       registerAllocation}
		    = if pop
			then let
			       val {rename = rename_pop,
				    registerAllocation}
				 = RA.fltpop {registerAllocation 
					      = registerAllocation}
			     in
			       {rename = rename_pop,
				registerAllocation = registerAllocation}
			     end
			else {rename = FltRegister.id,
			      registerAllocation = registerAllocation}

		  val {uses = final_uses,
		       defs = final_defs,
		       ...}
		    = Instruction.uses_defs_kills instruction
		    
		  val final_uses
		    = List.map(final_uses, RA.renameLift rename_pop)
		  val final_defs
		    = List.map(final_defs, RA.renameLift rename_pop)

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}

		in
		  {assembly = List.concat
		              [assembly_pre,
			       assembly_src_dst,
			       [Assembly.instruction instruction],
			       assembly_post],
		   registerAllocation = registerAllocation}
		end
	     | pFUnA {oper, dst, size}
	       (* Floating-point unary arithmetic instructions.
		* Require src operand as follows:
		*
		*               src
		*           fltreg add 
		*            *
		*   * only st(0)
		*
		* Require size modifier class as follows: FLT
		*)		
	     => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val {operand = final_dst,
		       assembly = assembly_dst,
		       rename = rename_dst,
		       registerAllocation}
		    = RA.allocateFltOperand {operand = dst,
					     options = {fltregister = true,
							address = false},
					     info = info,
					     size = size,
					     move = true,
					     supports = [],
					     saves = [],
					     top = SOME true,
					     registerAllocation
					     = registerAllocation}

		  val instruction
		    = Instruction.FUnA
		      {oper = oper}

		  val {uses = final_uses,
		       defs = final_defs,  
		       ...}
		    = Instruction.uses_defs_kills instruction

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}
		in
		  {assembly = List.concat
		              [assembly_pre,
			       assembly_dst,
			       [Assembly.instruction instruction],
			       assembly_post],
		   registerAllocation = registerAllocation}
		end
	     | pFPTAN {dst, size}
	       (* Floating-point partial tangent instruction.
		* Require src operand as follows:
		*
		*               src
		*           fltreg add 
		*            *
		*   * only st(0)
		*
		* Require size modifier class as follows: FLT
		* Automatically pushes 1.0 onto stack.
		*)		
	     => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val {assembly = assembly_free,
		       rename = rename_free,
		       registerAllocation}
		    = RA.freeFltRegister
		      {info = info,
		       size = Size.DBLE,
		       supports = [dst],
		       saves = [],
		       registerAllocation = registerAllocation}

		  val {operand = final_dst,
		       assembly = assembly_dst,
		       rename = rename_dst,
		       registerAllocation}
		    = RA.allocateFltOperand {operand = dst,
					     options = {fltregister = true,
							address = false},
					     info = info,
					     size = size,
					     move = true,
					     supports = [],
					     saves = [],
					     top = SOME true,
					     registerAllocation
					     = registerAllocation}

		  val instruction
		    = Instruction.FPTAN

		  val {uses = final_uses,
		       defs = final_defs,  
		       ...}
		    = Instruction.uses_defs_kills instruction

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}
		in
		  {assembly = List.concat
		              [assembly_pre,
			       assembly_dst,
			       [Assembly.instruction instruction],
			       [Assembly.instruction_fst
				{dst = Operand.fltregister FltRegister.top,
				 size = Size.DBLE,
				 pop = true}],
			       assembly_post],
		   registerAllocation = registerAllocation}
		end
	     | pFBinAS {oper, src, dst, size}
	       (* Floating-point binary arithmetic stack instructions.
		* Require src operand as follows:
		*
		*               src
		*           fltreg add 
		*             *
		*   * only st(1)
		*
		* Require dst operand as follows:
		*
		*               dst
		*           fltreg add 
		*            *
		*   * only st(0)
		*
		* Require size modifier class as follows: FLT
		*)		
	     => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val {operand_top = final_dst,
		       operand_one = final_src,
		       assembly = assembly_dst_src,
		       rename = rename_dst_src,
		       registerAllocation}
		    = RA.allocateFltStackOperands
		      {operand_top = dst,
		       move_top = true,
		       size_top = size,
		       operand_one = src,
		       move_one = true,
		       size_one = size,
		       info = info,
		       supports = [],
		       saves = [],
		       registerAllocation = registerAllocation}

		  val instruction
		    = Instruction.FBinAS
		      {oper = oper}

		  val {uses = final_uses,
		       defs = final_defs,
		       ...}
		    = Instruction.uses_defs_kills instruction

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}
		in
		  {assembly = List.concat
		              [assembly_pre,
			       assembly_dst_src,
			       [Assembly.instruction instruction],
			       assembly_post],
		   registerAllocation = registerAllocation}
		end
	     | pFBinASP {oper, src, dst, size}
	       (* Floating-point binary arithmetic stack pop instructions.
		* Require src operand as follows:
		*
		*               src
		*           fltreg add 
		*             *
		*   * only st(0)
		*
		* Require dst operand as follows:
		*
		*               dst
		*           fltreg add 
		*            *
		*   * only st(1)
		*
		* Require size modifier class as follows: FLT
		*)		
	     => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val {operand_top = final_src,
		       operand_one = final_dst,
		       assembly = assembly_src_dst,
		       rename = rename_src_dst,
		       registerAllocation}
		    = RA.allocateFltStackOperands
		      {operand_top = src,
		       move_top = true,
		       size_top = size,
		       operand_one = dst,
		       move_one = true,
		       size_one = size,
		       info = info,	
		       supports = [],
		       saves = [],
		       registerAllocation = registerAllocation}

		  val instruction
		    = Instruction.FBinASP
		      {oper = oper}

		  val {rename = rename_pop,
		       registerAllocation}
		    = RA.fltpop {registerAllocation = registerAllocation}

		  val {uses = final_uses,
		       defs = final_defs,
		       ...}
		    = Instruction.uses_defs_kills instruction
		    
		  val final_uses
		    = List.map(final_uses, RA.renameLift rename_pop)
		  val final_defs
		    = List.map(final_defs, RA.renameLift rename_pop)

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}
		in
		  {assembly = List.concat
		              [assembly_pre,
			       assembly_src_dst,
			       [Assembly.instruction instruction],
			       assembly_post],
		   registerAllocation = registerAllocation}
		end
	     | FLDCW {src}
	       (* Floating-point load control word; p. 252
		* Require src operand as follows:
		*
		*              dst
		*          reg imm lab add 
		*                       X
		*)
	     => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val {operand = final_src, 
		       assembly = assembly_src,
		       registerAllocation}
		    = RA.allocateOperand {operand = src,
					  options = {register = false,
						     immediate = false,
						     label = false,
						     address = true}, 
					  info = info,
					  size = Size.WORD,
					  move = false,
					  supports = [],
					  saves = [],
					  force = [],
					  registerAllocation 
					  = registerAllocation}

		  val instruction
		    = Instruction.FLDCW
		      {src = final_src}

		  val {uses = final_uses,
		       defs = final_defs,  
		       ...}
		    = Instruction.uses_defs_kills instruction

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}
		in
		  {assembly 
		   = List.concat [assembly_pre,
				   assembly_src,
				   [Assembly.instruction instruction],
				   assembly_post],
		   registerAllocation = registerAllocation}
		end
	     | FSTCW {dst, check}
	       (* Floating-point store control word; p. 289
		* Require dst operand as follows:
		*
		*              dst
		*          reg imm lab add 
		*                       X
		*)
	     => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val {operand = final_dst, 
		       assembly = assembly_dst,
		       registerAllocation}
		    = RA.allocateOperand {operand = dst,
					  options = {register = false,
						     immediate = false,
						     label = false,
						     address = true}, 
					  info = info,
					  size = Size.WORD,
					  move = false,
					  supports = [],
					  saves = [],
					  force = [],
					  registerAllocation 
					  = registerAllocation}

		  val instruction
		    = Instruction.FSTCW
		      {dst = final_dst,
		       check = check}

		  val {uses = final_uses,
		       defs = final_defs,  
		       ...}
		    = Instruction.uses_defs_kills instruction

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}
		in
		  {assembly 
		   = List.concat [assembly_pre,
				   assembly_dst,
				   [Assembly.instruction instruction],
				   assembly_post],
		   registerAllocation = registerAllocation}
		end
	     | FSTSW {dst, check}
	       (* Floating-point store status word; p. 294
		* Require dst operand as follows:
		*
		*              dst
		*          reg imm lab add 
		*           *           X
		*   * only register %ax
		*)
	     => let
		  val {uses,defs,kills} 
		    = Instruction.uses_defs_kills instruction
		  val {assembly = assembly_pre,
		       registerAllocation}
		    = RA.pre {uses = uses,
			      defs = defs,
			      kills = kills,
			      info = info,
			      registerAllocation = registerAllocation}

		  val {operand = final_dst, 
		       assembly = assembly_dst,
		       registerAllocation}
		    = RA.allocateOperand {operand = dst,
					  options = {register = true,
						     immediate = false,
						     label = false,
						     address = false}, 
					  info = info,
					  size = Size.WORD,
					  move = false,
					  supports = [],
					  saves = [],
					  force = [Register.T 
						   {reg = Register.EAX, 
						    part = Register.X}],
					  registerAllocation 
					  = registerAllocation}

		  val instruction
		    = Instruction.FSTSW
		      {dst = final_dst,
		       check = check}

		  val {uses = final_uses,
		       defs = final_defs,  
		       ...}
		    = Instruction.uses_defs_kills instruction

		  val {assembly = assembly_post,
		       registerAllocation}
		    = RA.post {uses = uses,
			       final_uses = final_uses,
			       defs = defs,
			       final_defs = final_defs,
			       info = info,
			       registerAllocation = registerAllocation}
		in
		  {assembly 
		   = List.concat [assembly_pre,
				  assembly_dst,
				  [Assembly.instruction instruction],
				  assembly_post],
		   registerAllocation = registerAllocation}
		end
	     | _ => Error.bug "allocateRegisters: unimplemented"

      val (allocateRegisters, allocateRegisters_msg)
	= tracer
	  "Instruction.allocateRegisters"
	  allocateRegisters
     end

  structure Directive =
    struct
      open Directive

      fun allocateRegisters {directive, info, registerAllocation}
	= let 
	    val {assembly, registerAllocation}
	      = case directive
		  of Reset
		   => RegisterAllocation.reset 
		      {registerAllocation = registerAllocation}
		   | Reserve {register}
		   => RegisterAllocation.reserve 
		      {register = register,
		       registerAllocation = registerAllocation}
		   | Unreserve {register}
		   => RegisterAllocation.unreserve 
	              {register = register,
		       registerAllocation = registerAllocation}
	           | Cache {register, memloc, reserve}
	           => RegisterAllocation.cache 
		      {register = register,
		       memloc = memloc,
		       reserve = reserve,
		       info = info,
		       registerAllocation = registerAllocation}
		   | Assume {register, memloc, weight, sync, reserve}
                   => RegisterAllocation.assume 
		      {register = register,
		       memloc = memloc,
		       weight = weight,
		       sync = sync,
		       reserve = reserve,
		       registerAllocation = registerAllocation}
		   | Eject {memlocs}
		   => RegisterAllocation.eject 
		      {memlocs = memlocs,
		       info = info,
		       registerAllocation = registerAllocation}
		   | Commit {memlocs}
		   => RegisterAllocation.commit
		      {memlocs = memlocs,
		       info = info,
		       registerAllocation = registerAllocation}
	           | Flush
		   => RegisterAllocation.flush
		      {info = info,
		       registerAllocation = registerAllocation}
	           | Clear
		   => RegisterAllocation.clear
		      {info = info,
		       registerAllocation = registerAllocation}
		   | Return {memloc}
		   => RegisterAllocation.return
		      {memloc = memloc,
		       info = info,
		       registerAllocation = registerAllocation}
		   | FltReturn {memloc}
		   => RegisterAllocation.fltreturn
		      {memloc = memloc,
		       info = info,
		       registerAllocation = registerAllocation}		      
		      
	    val s = Directive.toString directive
	  in
	    {assembly = if !Control.Native.commented > 1
			  then List.concat
			       [(Assembly.comment ("begin directive: " ^ s))::
				assembly,
				[Assembly.comment ("end directive: " ^ s)]]
			  else assembly,
	     registerAllocation = registerAllocation}
	  end

      val (allocateRegisters, allocateRegisters_msg)
	= tracer
	  "Directive.allocateRegisters"
	  allocateRegisters
    end

  structure Assembly =
    struct
      open Assembly

      fun allocateRegisters {assembly: (t * Liveness.t) list,
			     registerAllocation: RegisterAllocation.t}
 	= let
	    val {assembly, registerAllocation}
	      = List.fold
	        (assembly,
		 {assembly = [],
		  registerAllocation = registerAllocation},
		 fn ((Comment s,info), {assembly, registerAllocation})
		  => {assembly = ([Comment s])::assembly,
		      registerAllocation = registerAllocation}
		   | ((Directive d,info), {assembly, registerAllocation})
		   => let
			val {assembly = assembly',
			     registerAllocation}
			  = Directive.allocateRegisters 
			    {directive = d,
			     info = info,
			     registerAllocation = registerAllocation}
(*
			    handle exn
			    => (print (toString (Directive d));
				print "\n";
				print (RegisterAllocation.toString 
				       registerAllocation);
				print "\n";
				Error.bug (case exn
					     of Fail msg => msg
					      | Spill => "spill"))
*)

			val assembly''
			  = ((fn l 
			       => if !Control.Native.commented > 2
				    then (Assembly.comment
					  (Directive.toString d))::
				         l
				    else l) o
			     (fn l
			       => if !Control.Native.commented > 3
				    then (Liveness.toComments info) @ l
				    else l))
			    (if !Control.Native.commented > 4
			       then List.concat
				    [assembly',
				     RegisterAllocation.toComments 
				     registerAllocation]
			       else assembly')
		      in
			{assembly = assembly''::assembly,
			 registerAllocation = registerAllocation}
		      end
		   | ((PseudoOp p,info), {assembly, registerAllocation})
		   => {assembly = ([PseudoOp p])::assembly,
		       registerAllocation = registerAllocation}
		   | ((Label l,info), {assembly, registerAllocation})
		   => {assembly = ([Label l])::assembly,
		       registerAllocation = registerAllocation}
		   | ((Instruction i,info), {assembly, registerAllocation})
		   => let
			val {assembly = assembly',
			     registerAllocation}
			  = Instruction.allocateRegisters 
			    {instruction = i,
			     info = info,
			     registerAllocation = registerAllocation}
(*
			    handle exn
			    => (print (toString (Instruction i));
				print "\n";
				print (RegisterAllocation.toString 
				       registerAllocation);
				print "\n";
				Error.bug (case exn
					     of Fail msg => msg
					      | Spill => "spill"))
*)

			val assembly''
			  = ((fn l 
			       => if !Control.Native.commented > 2
				    then (Assembly.comment
					  (Instruction.toString i))::
				         l
				    else l) o
			     (fn l
			       => if !Control.Native.commented > 3
				    then (Liveness.toComments info) @ l
				    else l))
			    (if !Control.Native.commented > 4
			       then List.concat
				    [assembly',
				     RegisterAllocation.toComments 
				     registerAllocation]
			       else assembly')
		      in
			{assembly = assembly''::assembly,
			 registerAllocation = registerAllocation}
		      end)
	  in
	    {assembly = List.concatRev assembly,
	     registerAllocation = registerAllocation}
	  end

      val (allocateRegisters, allocateRegisters_msg)
	= tracer
	  "Assembly.allocateRegisters"
	  allocateRegisters
    end

  fun allocateRegisters {assembly : Assembly.t list list,
			 liveness : bool} :
                        Assembly.t list list
    = List.map(assembly,
	       fn assembly 
	        => let
		     val assembly
		       = if liveness
			   then Liveness.toLiveness assembly
			   else Liveness.toNoLiveness assembly

		     val {assembly, registerAllocation}
		       = Assembly.allocateRegisters 
		         {assembly = assembly,
			  registerAllocation 
			  = RegisterAllocation.empty ()}
(*
			 handle Fail msg
			 => (List.foreach(assembly,
					  fn (asm,info)
					   => (print (Assembly.toString asm);
					       print "\n";
					       print (Liveness.toString info);
					       print "\n"));
			     Error.bug msg)
*)
		   in
		     assembly
		   end)

  val (allocateRegisters, allocateRegisters_msg)
    = tracer
      "allocateRegisters"
      allocateRegisters

  fun allocateRegisters_totals ()
    = (allocateRegisters_msg ();
       Control.indent ();
       Liveness.toLiveness_msg ();
       Liveness.toNoLiveness_msg ();
       Assembly.allocateRegisters_msg ();
       Control.indent ();
       Instruction.allocateRegisters_msg ();
       Control.indent ();
       RegisterAllocation.pre_msg ();
       RegisterAllocation.post_msg ();
       RegisterAllocation.allocateOperand_msg ();
       RegisterAllocation.allocateFltOperand_msg ();
       RegisterAllocation.allocateFltStackOperands_msg ();
       Control.unindent ();
       Directive.allocateRegisters_msg ();
       Control.unindent ();
       Control.unindent())
end
