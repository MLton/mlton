functor CommonBlock (S: COMMON_BLOCK_STRUCTS): COMMON_BLOCK = 
struct

open S
open Dec PrimExp Transfer

fun eliminate (program as Program.T {globals, datatypes, functions, main})
  = let
      val shrink = shrinkExp globals
      val jumpHandlers = inferHandlers program

      fun makeRaise var
	= Exp.make {decs = [],
		    transfer = Raise (Vector.new1 var)}
      fun makeReturn var
	= Exp.make {decs = [],
		    transfer = Return (Vector.new1 var)}
      fun makeJump (dst, var)
	= Exp.make {decs = [],
		    transfer = Jump {dst = dst, args = Vector.new1 var}}
      fun makeNullaryJump dst
	= Exp.make {decs = [],
		    transfer = Jump {dst = dst, args = Vector.new0 ()}}

      val {get = varInfo: Var.t -> 
	                  {returner: (Func.t * Jump.t) option ref,
			   raiser: (Func.t * Jump.t) option ref,
			   jumpers: (Func.t * {inside: (Jump.t * Jump.t) list ref,
					       outside: (Jump.t * Jump.t) list ref}) option ref} 
			  option,
	   set = setVarInfo}
	= Property.getSetOnce
          (Var.plist, Property.initConst NONE)

      val {get = jumpInfo: Jump.t -> {nest: bool ref,
				      inside: Dec.t list ref,
				      outside: Dec.t list ref},
	   set = setJumpInfo}
	= Property.getSetOnce
	  (Jump.plist, Property.initFun (fn _ => {nest = ref false,
						  inside = ref [],
						  outside = ref []}))

      val _
	= Vector.foreach
	  (globals,
	   fn {var, ...}
	    => setVarInfo(var, SOME {returner = ref NONE, 
				     raiser = ref NONE,
				     jumpers = ref NONE}))


      fun eliminateFunction (f as Function.T {name, args, body, returns})
	= let

	    val newDecs = ref []

	    fun commonReturner var
	      = case varInfo var
		  of NONE => NONE
	           | SOME {returner, ...}
		   => let
			fun install ()
			  = let
			      val j = Jump.newNoname ()
			      val dec = Fun {name = j,
					     args = Vector.new0 (),
					     body = makeReturn var}
			    in
			      List.push(newDecs, dec) ;
			      returner := SOME (name, j) ;
			      SOME j
			    end
		      in
			case !returner
			  of NONE => install ()
			   | SOME (name', j')
			   => if Func.equals(name, name')
				then SOME j'
				else install ()
		      end
	    fun commonRaiser var
	      = case varInfo var
		  of NONE => NONE
		   | SOME {raiser, ...}
		   => let
			fun install ()
			  = let
			      val j = Jump.newNoname ()
			      val dec = Fun {name = j,
					     args = Vector.new0 (),
					     body = makeRaise var}
			    in
			      List.push(newDecs, dec) ;
			      raiser := SOME (name, j) ;
			      SOME j
			    end
		      in
			case !raiser
			  of NONE => install ()
			   | SOME (name', j')
			   => if Func.equals(name, name')
				then SOME j'
				else install ()
		      end
	    fun commonJumpers (k, var)
	      = case varInfo var
		  of NONE => NONE
		   | SOME {jumpers, ...}
		   => let
			val info as {nest, ...} = jumpInfo k
			val io = if !nest then #inside else #outside
			val io' = if !nest then #inside else #outside

			fun install kjs
			  = let
			      val j = Jump.newNoname ()
			      val dec = Fun {name = j,
					     args = Vector.new0 (),
					     body = makeJump (k, var)}
			      val {nest, inside, outside} = jumpInfo k
			    in
			      List.push (io info, dec) ;
			      List.push (kjs, (k, j)) ;
			      SOME j
			    end
			fun install' ()
			  = let
			      val info = {inside = ref [], 
					  outside = ref []}
			    in
			      jumpers := SOME (name, info) ;
			      install (io' info)
			    end
		      in
			case !jumpers
			  of NONE => install' ()
			   | SOME (name', info')
			   => if Func.equals(name, name')
				then let
				       val kjs' = io' info'
				     in
				       case List.peek
				            (!kjs',
					     fn (k', _) => Jump.equals(k, k'))
					 of NONE => install kjs'
					  | SOME (_, j') => SOME j'
				     end
				else install' ()
		      end

	    fun loopExp (e : Exp.t) : Exp.t
	      = let
		  val {decs, transfer} = Exp.dest e
		  val decs
		    = List.fold
		      (List.rev decs,
		       [],
		       fn (Fun {name, args, body}, decs)
		        => let
			     val {nest, inside, outside} = jumpInfo name
			     val _ = nest := true

			     fun finish body
			       = let
				   val _ = nest := false
				 in
				   (Fun {name = name,
					 args = args,
					 body = Exp.prefixs(body, !inside)})::
				   (List.appendRev(!outside, decs))
				 end

			     fun default ()
			       = finish (loopExp body)
			       
			     fun default' dst
			       = finish (makeNullaryJump dst)

			     val {decs, transfer} = Exp.dest body
			   in
			      case (decs, transfer)
				of ([], Return vars)
				 => if 0 = Vector.length args
				       andalso 1 = Vector.length vars
				      then let
					     val var = Vector.sub(vars, 0)
					   in
					     case commonReturner var
					       of SOME j => default' j
						| NONE => default ()
					   end
				      else default ()
				 | ([], Raise vars) 
				 => if 0 = Vector.length args
				       andalso 1 = Vector.length vars
				       andalso List.isEmpty (jumpHandlers name)
				      then let
					     val var = Vector.sub(vars, 0)
					   in
					     case commonRaiser var
					       of SOME j => default' j
						| NONE => default ()
					   end
				      else default ()
                                 | ([], Jump {dst, args = vars})
				 => if 0 = Vector.length args
				       andalso 1 = Vector.length vars
				      then let
					     val var = Vector.sub(vars, 0)
					   in
					     case commonJumpers (dst, var)
					       of SOME j => default' j
						| NONE => default ()
					   end
				      else default ()
				 | _ => default ()
			   end
			| (dec, decs) => dec::decs)
		in
		  Exp.make {decs = decs, transfer = transfer}
		end

	    val {decs, transfer} = Exp.dest (loopExp body)
	    val decs = List.appendRev(!newDecs, decs)
	    val body = Exp.make {decs = decs, transfer = transfer}
	  in
	    Function.T {name = name,
			args = args,
			body = shrink body,
			returns = returns}
	  end

      val program 
	= Program.T {datatypes = datatypes,
		     globals = globals,
		     functions = Vector.map(functions, eliminateFunction),
		     main = main}
      val _ = Program.clear program
    in
      program
    end
end
