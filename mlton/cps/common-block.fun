functor CommonBlock (S: COMMON_BLOCK_STRUCTS): COMMON_BLOCK = 
struct

open S
open Dec PrimExp Transfer

fun eliminate (program as Program.T {globals, datatypes, functions, main})
  = let
      val exnGlobals
	= Vector.keepAll
	  (globals, 
	   fn {ty, ...} => case Type.dest ty
			     of Type.Datatype tc => Tycon.equals(tc, Tycon.exn)
			      | _ => false)

      val shrink = shrinkExp globals
      val jumpHandlers = inferHandlers program

      fun makeRaise var
	= Exp.make {decs = [],
		    transfer = Raise (Vector.new1 var)}
      fun makeNullaryJump dst
	= Exp.make {decs = [],
		    transfer = Jump {dst = dst, args = Vector.new0 ()}}

      val {get = varInfo: Var.t -> Jump.t option ref option,
	   set = setVarInfo}
	= Property.getSet(Var.plist, Property.initConst NONE)

      fun resetExnGlobals ()
	= Vector.foreach
	  (exnGlobals, fn {var, ...} => setVarInfo (var, SOME (ref NONE)))

      fun eliminateFunction (f as Function.T {name, args, body, returns})
	= let
	    val _ = resetExnGlobals ()

	    val newDecs = ref []
	    fun commonBlock var
	      = case varInfo var
		  of NONE => NONE
		   | SOME r
		   => (case !r
			 of SOME j => SOME j
			  | NONE 
			  => let
			       val j = Jump.newNoname ()
			       val dec = Fun {name = j,
					      args = Vector.new0 (),
					      body = makeRaise var}
			     in
			       List.push(newDecs, dec) ;
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
			      case (decs, transfer)
				of ([], Raise vars) 
				 => if 0 = Vector.length args
				       andalso 1 = Vector.length vars
				       andalso List.isEmpty (jumpHandlers name)
				      then let
					     val var = Vector.sub(vars, 0)
					   in
					     case commonBlock var
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
