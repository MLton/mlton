(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor InferHandlers (S: INFER_HANDLERS_STRUCTS): INFER_HANDLERS = 
struct

open S

structure Set = DisjointSet
   
structure Handler':
   sig
      type t

      val dest: t -> Jump.t
      val equals: t * t -> bool
      val layout: t -> Layout.t
      val new: Jump.t -> t
      val unknown: unit -> t
   end =
   struct
      datatype t = T of Jump.t option Set.t

      fun layout (T s) = Option.layout Jump.layout (Set.value s)
      val new = T o Set.singleton o SOME
      fun unknown () = T (Set.singleton NONE)

      fun equals (T s, T s'): bool =
	 Set.canUnion
	 (s, s',
	  fn (NONE, h) => SOME h
	   | (h, NONE) => SOME h
	   | (SOME h, SOME h') =>
		if Jump.equals (h, h') then SOME (SOME h) else NONE)

      val equals =
	 Trace.trace2 ("Handler'.equals", layout, layout, Bool.layout)
	 equals

      fun dest (T s) =
	 case Set.value s of
	    NONE => Error.bug "unknown handler"
	  | SOME h => h
   end

structure Handlers:
   sig
      type t

      val equals: t * t -> bool
      val empty: t
      val layout: t -> Layout.t
      val pop: t -> t
      val push: t * Handler'.t -> t
      val toList: t -> Jump.t list option
      val unknown: unit -> t
   end =
   struct
      datatype t = T of handlers Set.t
      and handlers =
	 Unknown
	| Empty
	| Push of {top: Handler'.t,
		   rest: t}

      fun layout (T s) =
	 let open Layout
	 in case Set.value s of
	    Unknown => str "Unknown"
	  | Empty => str "Empty"
	  | Push {top, rest} => seq [Handler'.layout top,
				     str " :: ",
				     layout rest]
	 end

      val new = T o Set.singleton
      val empty = new Empty
      fun unknown () = new Unknown
      fun push (rest: t, top: Handler'.t) = new (Push {top = top,
						       rest = rest})
      val push =
	 Trace.trace2 ("Handlers.push", layout, Handler'.layout, layout)
	 push

      fun pop (T s) =
	 case Set.value s of
	    Empty => Error.bug "pop of empty handler stack"
	  | Push {rest, ...} => rest
	  | Unknown =>
	       let val rest = unknown ()
	       in Set.setValue (s, Push {top = Handler'.unknown (),
					 rest = rest})
		  ; rest
	       end

      fun isEmpty (T s) =
	 case Set.value s of
	    Empty => true
	  | _ => false

      fun toList (hs: t): Jump.t list option =
	 let
	    fun loop (T s, ac) =
	       case Set.value s of
		  Unknown => NONE
		| Empty => SOME (rev ac)
		| Push {top, rest} => loop (rest, Handler'.dest top :: ac)
	 in loop (hs, [])
	 end

      fun equals (T s, T s'): bool =
	 Set.canUnion
	 (s, s',
	  fn (Unknown, hs) => SOME hs
	   | (hs, Unknown) => SOME hs
	   | (Empty, Empty) => SOME Empty
	   | (hs as Push {top = t, rest = r}, Push {top = t', rest = r'}) =>
		if Handler'.equals (t, t') andalso equals (r, r')
		   then SOME hs
		else NONE
	   | _ => NONE)	     
      val equals =
	 Trace.trace2 ("Handlers.equals", layout, layout, Bool.layout)
	 equals
   end

open Dec PrimExp Transfer

val traceJump = Trace.trace ("InferHandlers.jump", Jump.layout, Handlers.layout)
val traceSetJump =
   Trace.trace2
   ("InferHandlers.setJump", Jump.layout, Handlers.layout, Unit.layout)
   
fun inferHandlers (Program.T {functions, ...}) =
   let
      val {get = jump, set = setJump} =
	 Property.getSetOnce (Jump.plist,
			      Property.initRaise ("handlers", Jump.layout))
      val jump = traceJump jump
      val setJump = traceSetJump setJump
      val _ =
	 Vector.foreach
	 (functions, fn {body, ...} =>
	  let
	     fun loop (e: Exp.t, hs: Handlers.t): unit =
		let
		   fun check (msg, hs, hs') =
		      let
			 fun disp () =
			    Control.message
			    (Control.Silent, fn () =>
			     let open Layout
			     in align [seq [str "exp: ", Exp.layout e],
				       seq [str "hs: ", Handlers.layout hs],
				       seq [str "hs': ", Handlers.layout hs']]
			     end)
		      in if Handlers.equals (hs, hs')
			    then ()
			 else (disp ()
			       ; Error.bug (concat
					    ["handler stack mismatch at ", msg]))
		      end
		   val {decs, transfer} = Exp.dest e
		   val after =
		      List.fold
		      (decs, hs, fn (d, hs) =>
		       case d of
			  Bind {exp = PrimApp {info = PrimInfo.Overflow j, ...},
				...} =>
			  (check ("PrimApp", hs, jump j)
			   ; hs)
			      | Fun {name, body, ...} =>
				   (let val hs = Handlers.unknown ()
				    in setJump (name, hs)
				    end
				       ; loop (body, jump name)
				       ; hs)
			      | HandlerPush h =>
				   let
				      val hs = Handlers.push (hs, Handler'.new h)
				   in check ("push", hs, jump h)
				      ; hs
				   end
			      | HandlerPop => Handlers.pop hs
			      | _ => hs)
		   fun checkJump j = check ("jump", after, jump j)
		   fun empty msg = check (msg, after, Handlers.empty)
		in case transfer of
		   Bug => ()
		 | Call {cont = c, ...} =>
		      (case c of
			  NONE => empty "tail call"
			| SOME c => check ("nontail call", after, jump c))
		 | Case {cases, default, ...} =>
		      (Cases.foreach (cases, checkJump)
		       ; Option.app (default, checkJump))
		 | Jump {dst, ...} => checkJump dst
		 | Raise _ => ()
		 | Return _ => empty "return"
		end
	  in loop (body, Handlers.empty)
	  end)
   in
      fn j => (case Handlers.toList (jump j) of
		  NONE => Error.bug (concat ["toList (jump ",
					     Jump.toString j,
					     ") of unknown handler stack"])
		| SOME l => l)
   end

val inferHandlers = Control.trace (Control.Pass, "inferHandlers") inferHandlers
   
fun deltaHandlers (d, hs) =
   case d of
      HandlerPush h => h :: hs
    | HandlerPop => (case hs of
			_ :: hs => hs
		      | _ => Error.bug "deltaHandlers")
    | _ => hs

end
