(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor UnusedArgs (S: UNUSED_ARGS_STRUCTS): UNUSED_ARGS = 
struct

open S
open Dec Transfer

structure Used =
  struct
    structure L = TwoPointLattice (val bottom = "unused"
				   val top = "used")
    open L
    val makeUsed = makeTop
    val isUsed = isTop
  end

structure NeedsArgs =
  struct
    structure L = TwoPointLattice (val bottom = "no"
				   val top = "yes")
    open L
    val forceArgs = makeTop
    val needsArgs = isTop
  end

fun unusedArgs (program as Program.T {datatypes, globals, functions, main})
  = let
      val {get = varInfo : Var.t -> {used: Used.t}}
	= Property.get (Var.plist,
			Property.initFun (fn _ => {used = Used.new ()}))
      fun used x = #used (varInfo x)
      fun isUsed x = Used.isUsed (used x)
      fun use x = Used.makeUsed (used x)
      fun uses xs = Vector.foreach(xs, use)
      fun flow (x, y) = Used.<=(used x, used y)

      val {get = jumpInfo : Jump.t -> {args: Var.t vector,
				       wrapper: Jump.t option ref,
				       needArgs: bool ref,
				       cedeArgs: bool ref},
	   set = setJumpInfo}
	= Property.getSetOnce
	  (Jump.plist,
	   Property.initRaise ("jumpInfo", Jump.layout))
      fun newJumpInfo (j, args)
	= setJumpInfo (j, {args = Vector.map(args, fn (x,_) => x),
			   wrapper = ref NONE,
			   needArgs = ref false,
			   cedeArgs = ref false})
      fun getArgs j = #args (jumpInfo j)
      fun forceNeed j = #needArgs (jumpInfo j) := true
      fun needArgs j = !(#needArgs (jumpInfo j))
      fun forceCede j = #cedeArgs (jumpInfo j) := true
      fun cedeArgs j = !(#cedeArgs (jumpInfo j))
      fun wrapper j = valOf(!(#wrapper (jumpInfo j)))
      fun getWrapper j = case !(#wrapper (jumpInfo j))
			   of SOME j' => j'
			    | NONE => j
      fun setWrapper (j, j') = #wrapper (jumpInfo j) := SOME j'

      fun wrap j = let
		     val {needArgs, cedeArgs, args, ...} = jumpInfo j
		   in 
		     !needArgs andalso 
		     !cedeArgs andalso 
		     Vector.exists(args, not o isUsed)
		   end

      fun analyzeExp (e: Exp.t) : unit
	= let
	    val {decs, transfer} = Exp.dest e
	  in
	    List.foreach
	    (decs,
	     fn Bind {var, ty, exp}
	      => if PrimExp.maySideEffect exp
		   then PrimExp.foreachVar(exp, use)
		   else PrimExp.foreachVar(exp, fn x => flow (var, x))
	      | Fun {name, args, body}
	      => (newJumpInfo (name, args)
		  ; analyzeExp body)
	      | HandlerPush h
	      => forceNeed h
	      | _ => ())
	    ; analyzeTransfer transfer
	  end
      and analyzeTransfer (t: Transfer.t) : unit
	= (case t
	     of Bug => ()
	      | Call {args, cont, ...}
	      => (uses args
		  ; Option.app(cont, forceNeed))
	      | Case {test, cases, default, ...} 
	      => (use test
		  ; Option.app(default, forceCede)
		  ; case cases
		      of Cases.Con cjs 
		       => Vector.foreach(cjs, fn (_,j) => forceNeed j)
		       | _ => Cases.foreach(cases, fn j => forceCede j))
	      | Raise args => uses args
	      | Return args => uses args
	      | Jump {dst, args}
	      => (Vector.foreach2 (getArgs dst, args, fn (x,y) => flow (x,y))
		  ; forceCede dst))

      fun loopExp (e: Exp.t) : Exp.t
	= let
	    val {decs, transfer} = Exp.dest e
	    val decs
	      = List.fold
	        (decs,
		 [],
		 fn (d,decs)
		  => (case d
			of Bind {var, ty, exp}
			 => if isUsed var orelse
			       PrimExp.maySideEffect exp
			      then d::decs
			      else decs
			 | Fun {name, args, body}
			 => if wrap name
			      then let
				     val name = name
				     val name' = Jump.new name
				     val name'' = Jump.new name

				     val (args, args', args'', acts', acts'')
				       = Vector.fold
				         (args,
					  ([],[],[], [], []),
					  fn ((x,ty),
					      (args,args',args'',acts',acts''))
					   => let
						val x' = Var.new x
						val x'' = Var.new x
					      in 
						if isUsed x
						  then ((x,ty)::args,
							(x',ty)::args',
							(x'',ty)::args'',
							x'::acts',
							x''::acts'')
						  else (args,
							(x',ty)::args',
							(x'',ty)::args'',
							acts',
							acts'')
					      end)
				     val args = Vector.fromListRev args
				     val args' = Vector.fromListRev args'
				     val args'' = Vector.fromListRev args''
				     val acts' = Vector.fromListRev acts'
				     val acts'' = Vector.fromListRev acts''
				       
				     val body' 
				       = Exp.make
				         {decs = [],
					  transfer
					  = Jump {dst = name,
						  args = acts'}}
				     val body'' 
				       = Exp.make
				         {decs = [],
					  transfer
					  = Jump {dst = name,
						  args = acts''}}

				     val inner
				       = Fun {name = name',
					      args = args',
					      body = body'}
				     val outer
				       = Fun {name = name'',
					      args = args'',
					      body = body''}

				     val _ = setWrapper(name, name')
				     val body = Exp.prefix(loopExp body, inner)
				     val _ = setWrapper(name, name'')
				   in
				     outer ::
				     Fun {name = name,
					  args = args,
					  body = body} ::
				     decs
				   end
			      else if needArgs name
				then Fun {name = name,
					  args = args,
					  body = loopExp body} ::
				     decs
			      else let
				     val name = name
				     val args = Vector.keepAll
				                (args, fn (x,_) => isUsed x)
				     val body = loopExp body
				   in
				     Fun {name = name,
					  args = args,
					  body = body} ::
				     decs
				   end
			 | HandlerPush h 
			 => HandlerPush (getWrapper h)::
			    decs
			 | HandlerPop
			 => d::decs))
	    val decs = List.rev decs
	    val transfer = loopTransfer transfer
	  in
	    Exp.make {decs = decs,
		      transfer = transfer}
	  end
      and loopTransfer (t: Transfer.t) : Transfer.t
	= (case t
	     of Call {func, args, cont}
	      => Call {func = func,
		       args = args,
		       cont = Option.map(cont, getWrapper)}
	      | Case {cause, test, cases, default}
	      => let
		   val cases
		     = case cases
			 of Cases.Con cjs 
			  => Cases.Con (Vector.map(cjs, fn (c,j) => (c, getWrapper j)))
			  | _ => cases
		 in 
		   Case {cause = cause,
			 test = test,
			 cases = cases,
			 default = default}
		 end 
	      | Jump {dst, args}
	      => let
		   val args 
		     = Vector.keepAllMap2
		       (getArgs dst, args,
			fn (y, x) => if isUsed y
				       then SOME x
				       else NONE)
		 in 
		   Jump {dst = dst,
			 args = args}
		 end
	      | _ => t)

      val shrinkExp = shrinkExp globals
      val functions 
	= Vector.map 
	  (functions, 
	   fn Function.T {name, args, body, returns}
	    => Function.T {name = name,
			   args = args,
			   body = (analyzeExp body ; 
				   shrinkExp (loopExp body)),
			   returns = returns})

      val program'
	= Program.T {datatypes = datatypes,
		     globals = globals,
		     functions = functions,
		     main = main}
     in 
      Program.clear program'
      ; program'
    end

end
