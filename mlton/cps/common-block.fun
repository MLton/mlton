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
			   jumpers: (Func.t * (Jump.t * Jump.t) list) option ref} 
			  option,
	   set = setVarInfo}
	= Property.getSetOnce
          (Var.plist, Property.initConst NONE)

      val {get = jumpInfo: Jump.t -> Dec.t list ref,
	   set = setJumpInfo}
	= Property.getSetOnce
	  (Jump.plist, Property.initFun (fn _ => ref []))

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
			fun install kjs
			  = let
			      val j = Jump.newNoname ()
			      val dec = Fun {name = j,
					     args = Vector.new0 (),
					     body = makeJump (k, var)}
			    in
			      List.push(jumpInfo k, dec) ;
			      jumpers := SOME (name, (k, j)::kjs) ;
			      SOME j
			    end
		      in
			case !jumpers
			  of NONE => install []
			   | SOME (name', kjs')
			   => if Func.equals(name, name')
				then case List.peek
				          (kjs', fn (k', _) => Jump.equals(k, k'))
				       of NONE => install kjs'
					| SOME (_, j') => SOME j'
				else install []
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
			     fun doit dec
			       = dec::(List.appendRev(!(jumpInfo name), decs))

			     fun default ()
			       = doit (Fun {name = name,
					    args = args,
					    body = loopExp body})
			       
			     fun default' dst
			       = doit (Fun {name = name,
					    args = args,
					    body = makeNullaryJump dst})

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
