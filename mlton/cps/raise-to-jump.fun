(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor RaiseToJump (S: RAISE_TO_JUMP_STRUCTS): RAISE_TO_JUMP = 
struct

open S
open Dec Transfer

structure CanRaise =
   struct
      structure L = TwoPointLattice (val bottom = "no"
				     val top = "yes")
      open L
      val makeYes = makeTop
      val canRaise = isTop
   end

fun raiseToJump (program as Program.T {datatypes, globals, functions, main}) =
   let
      val jumpHandlers = inferHandlers program

      (* For each function, decide whether or not it can raise an exception to
       * its caller.  This can happen if either
       *     1. It raises when the handler stack is empty
       *  or 2. It calls a function that can raise while the handler stack is
       *        empty.
       *)
      val {get = funcInfo: Func.t -> {canRaise: CanRaise.t}, ...} =
	 Property.get (Func.plist,
		       Property.initFun (fn _ => {canRaise = CanRaise.new ()}))
      val {get = keepHandler: Jump.t -> bool ref, ...} =
	 Property.get (Jump.plist,
		       Property.initFun (fn _ => ref false))
      val funcCanRaise = #canRaise o funcInfo
      val _ =
	 Vector.foreach
	 (functions, fn Function.T {name, body, ...} =>
	  let
	     val {canRaise, ...} = funcInfo name
	     fun loopExp (e: Exp.t, hs: Jump.t list): unit =
		let val {decs, transfer} = Exp.dest e
		   val _ = loopDecs decs
		   fun handlers () = List.fold (decs, hs, deltaHandlers)
		in case transfer of
		   Call {func, ...} =>
		      let val canRaise' = funcCanRaise func
		      in case handlers () of
			 [] => CanRaise.<=(canRaise', canRaise)
		       | h :: _ =>
			    let val k = keepHandler h
			    in if !k
				  then ()
			       else CanRaise.addHandler (canRaise', fn () =>
							 k := true)
			    end
		      end
		 | Raise args =>
		      (case handlers () of
			  [] => CanRaise.makeYes canRaise
			| _ :: _ => ())
		 | _ => ()
		end
	     and loopDecs ds =
		List.foreach
		(ds,
		 fn Fun {name, args, body, ...} =>
		 loopExp (body, jumpHandlers name)
		  | _ => ())
	  in loopExp (body, [])
	  end)
      val _ =
	 Control.diagnostics
	 (fn display =>
	  let open Layout
	  in Vector.foreach (functions, fn Function.T {name, ...} =>
			     display (seq
				      [Func.layout name,
				       str " ",
				       CanRaise.layout (funcCanRaise name)]))
	  end)
      (* Translate. *)
      fun loopExp (e: Exp.t, hs: Jump.t list): Exp.t =
	 let val {decs, transfer} = Exp.dest e
	    val (hs, decs) =
	       List.fold
	       (decs, (hs, []), fn (d, (hs, decs)) =>
		let
		   val decs =
		      case d of
			 Fun {name, args, body} =>
			    Fun {name = name, args = args,
				 body = loopExp (body, jumpHandlers name)}
			    :: decs
		       | HandlerPop =>
			    if !(keepHandler (hd hs))
			       then d :: decs
			    else decs
		       | HandlerPush h =>
			    if !(keepHandler h)
			       then d :: decs
			    else decs
		       | _ => d :: decs
		in (deltaHandlers (d, hs), decs)
		end)
	    val decs = rev decs
	    val transfer =
	       case transfer of
		  Raise args =>
		     (case hs of
			 [] => transfer
		       | h :: _ => Jump {dst = h, args = args})
		| _ => transfer
	 in Exp.make {decs = decs,
		      transfer = transfer}
	 end
      and loopDec d =
	 case d of
	    Fun {name, args, body} =>
	       Fun {name = name, args = args,
		    body = loopExp (body, jumpHandlers name)}
	  | _ => d
      val shrinkExp = shrinkExp globals
      val functions =
	 Vector.map (functions, fn Function.T {name, args, body, returns} =>
		     Function.T {name = name,
				 args = args,
				 body = shrinkExp (loopExp (body, [])),
				 returns = returns})
      val program =
	 Program.T {datatypes = datatypes,
		    globals = globals,
		    functions = functions,
		    main = main}
      val _ = Program.clear program
   in
      program
   end

end
