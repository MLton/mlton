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
      fun makeNullaryJump dst
	= Exp.make {decs = [],
		    transfer = Jump {dst = dst, args = Vector.new0 ()}}

      fun eliminateFunction (f as Function.T {name, args, body, returns})
	= let
	    val {get = varInfo: Var.t -> Jump.t option ref option,
		 set = setVarInfo}
	      = Property.getSetOnce
	        (Var.plist,
		 Property.initConst NONE)

	    val _ 
	      = Vector.foreach
	        (globals, fn {var, ...} => setVarInfo (var, SOME (ref NONE)))

	    val newDecs = ref []
	    fun maybeInstall var
	      = case varInfo var
		  of NONE => NONE
		   | SOME r
		   => (case !r
			 of SOME j => SOME j
			  | NONE 
			  => let
			       val j = Jump.newNoname ()
			       val dec
				 = Fun 
				   {name = j,
				    args = Vector.new0 (),
				    body = makeRaise var}
			     in
			       List.push
			       (newDecs, dec) ;
			       r := SOME j ;
			       SOME j
			     end)

	    fun loopExp (e : Exp.t) : Exp.t
	      = let
		  val {decs, transfer} = Exp.dest e
		  val decs
		    = List.fold
		      (decs,
		       [],
		       fn (Fun {name, args, body}, decs)
		        => let
			     fun default ()
			       = (Fun 
				  {name = name, 
				   args = args, 
				   body = loopExp body})::decs
			     fun default' dst
			       = (Fun 
				  {name = name,
				   args = args,
				   body = makeNullaryJump dst})::decs
			     val {decs, transfer} = Exp.dest body
			   in
			     case (jumpHandlers name, 
				   Vector.length args,
				   decs, 
				   transfer)
			       of ([], 0, [], Raise vars)
				=> if Vector.length vars = 1
				     then let
					    val var = Vector.sub(vars, 0)
					  in
					    case maybeInstall var
					      of SOME j => default' j
					       | NONE => default ()
					  end
				     else default ()
				| _ => default ()
			   end
			| (dec, decs) => dec::decs)
		  val decs = List.rev decs
		in
		  Exp.make {decs = decs, transfer = transfer}
		end

	    val {decs, transfer} = Exp.dest (loopExp body)
	    val decs = (!newDecs) @ decs
	    val body = Exp.make {decs = decs, transfer = transfer}
	  in
	    Function.T {name = name,
			args = args,
			body = shrink body,
			returns = returns}
	  end

      val functions
	= Vector.map
	  (functions, eliminateFunction)

      val program 
	= Program.T {datatypes = datatypes,
		     globals = globals,
		     functions = functions,
		     main = main}
      val _ = Program.clear program
    in
      program
    end
end
