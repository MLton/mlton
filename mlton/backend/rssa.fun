functor Rssa (S: RSSA_STRUCTS): RSSA =
struct

open S

structure Operand =
   struct
      datatype t =
	 CastInt of Var.t
       | Const of Const.t
       | Offset of {base: Var.t,
		    bytes: int,
		    ty: Type.t}
       | OffsetScale of {base: Var.t,
			 index: Var.t,
			 ty: Type.t}
       | Var of {var: Var.t,
		 ty: Type.t}

      val int = Const o Const.fromInt

      val toString =
	 fn CastInt x => concat [ "CastInt ", Var.toString x]
	     | Const c => Const.toString c
	     | Offset {base, bytes, ty} =>
		  concat ["O", Type.name ty,
			  "(", Var.toString base, ",", Int.toString bytes, ")"]
	     | OffsetScale {base, index, ty} =>
		  concat ["X", Type.name ty, 
			  "(", Var.toString base, ",", Var.toString index, ")"]
	     | Var {var, ...} => Var.toString var

      val layout: t -> Layout.t = Layout.str o toString

      val ty =
	 fn CastInt _ => Type.int
	  | Const c =>
	       let
		  datatype z = datatype Const.Node.t
	       in
		  case Const.node c of
		     Char _ => Type.char
		   | Int _ => Type.int
		   | IntInf _ => Type.pointer
		   | Real _ => Type.double
		   | String _ => Type.pointer
		   | Word _ =>
			let
			   val t = Const.tycon c
			in
			   if Tycon.equals (t, Tycon.word)
			      then Type.uint
			   else if Tycon.equals (t, Tycon.word8)
				   then Type.char
				else Error.bug "strange word"
			end
	       end
	  | Offset {ty, ...} => ty
	  | OffsetScale {ty, ...} => ty
	  | Var {ty, ...} => ty
   end

structure Statement =
   struct
      datatype t =
	 Array of {dst: Var.t,
		   numBytesNonPointers: int,
		   numElts: Var.t,
		   numPointers: int}
       | Move of {dst: Operand.t, (* If the dst is var, then it is the
				   * SSA defining occurrence.
				   *)
		  src: Operand.t}
       | Object of {dst: Var.t,
		    numPointers: int,
		    numWordsNonPointers: int,
		    stores: {offset: int, (* bytes *)
			     value: Operand.t} vector}
       | PrimApp of {dst: (Var.t * Type.t) option,
		     prim: Prim.t,
		     args: Var.t vector}
       | SetExnStackLocal
       | SetExnStackSlot
       | SetHandler of Label.t (* label must be of Handler kind. *)
       | SetSlotExnStack

      fun foldDef (s, a, f) =
	 case s of
	    Array {dst, ...} => f ({var = dst, ty = Type.pointer}, a)
	  | Move {dst, ...} =>
	       (case dst of
		   Operand.Var x => f (x, a)
		 | _ => a)
	  | Object {dst, ...} => f ({var = dst, ty = Type.pointer}, a)
	  | PrimApp {dst, ...} =>
	       Option.fold (dst, a, fn ((x, t), a) =>
			    f ({var = x, ty = t}, a))
	  | _ => a

      fun forDef (s, f) = foldDef (s, (), fn (x, ()) => f x)

      val layout =
	 let
	    open Layout
	 in
	    fn Array {dst, ...} => seq [Var.layout dst, str " = Array"]
	     | Move {dst, src} =>
		  seq [Operand.layout dst, str " = ", Operand.layout src]
	     | Object {dst, ...} => seq [Var.layout dst, str " = Allocate"]
	     | PrimApp {dst, prim, args, ...} =>
		  seq [(case dst of
			   NONE => empty
			 | SOME (x, _) => seq [Var.layout x, str " = "]),
		       Prim.layout prim, str " ",
		       Vector.layout Var.layout args]
	     | SetExnStackLocal => str "SetExnStackLocal"
	     | SetExnStackSlot => str "SetExnStackSlot "
	     | SetHandler l => seq [str "SetHandler ", Label.layout l]
	     | SetSlotExnStack => str "SetSlotExnStack "
	 end
   end

structure LimitCheck =
   struct
      datatype t =
	 Array of {bytesPerElt: int,
		   extraBytes: int,
		   numElts: Var.t,
		   stackToo: bool}

       | Heap of {bytes: int,
		  stackToo: bool}
       | Signal
       | Stack

      fun forVar (l, f) =
	 case l of
	    Array {numElts, ...} => f numElts
	  | _ => ()

      fun layout (l: t): Layout.t =
	 let
	    open Layout
	 in
	    case l of
	       Array {bytesPerElt, extraBytes, numElts, stackToo} =>
		  seq [str "Array ",
		       record [("bytesPerElt", Int.layout bytesPerElt),
			       ("extraBytes", Int.layout extraBytes),
			       ("numElts", Var.layout numElts),
			       ("stackToo", Bool.layout stackToo)]]
	     | Heap {bytes, stackToo} =>
		  seq [str "Heap ",
		       record [("bytes", Int.layout bytes),
			       ("stackToo", Bool.layout stackToo)]]
	     | Signal => str "Signal"
	     | Stack => str "Stack"
	 end
   end

structure Transfer =
   struct
      datatype t =
	 Arith of {dst: Var.t,
		   prim: Prim.t,
		   args: Var.t vector,
		   overflow: Label.t,
		   success: Label.t
		   }
       | Bug
       | CCall of {args: Operand.t vector,
		   prim: Prim.t,
		   return: Label.t,
		   returnTy: Type.t option}
       | Call of {func: Func.t,
		  args: Operand.t vector,
		  return: Return.t}
       | Goto of {dst: Label.t,
		  args: Operand.t vector}
       | LimitCheck of {failure: Label.t,
			kind: LimitCheck.t,
			success: Label.t}
       | Raise of Operand.t vector
       | Return of Operand.t vector
       | Runtime of {args: Operand.t vector,
		     prim: Prim.t,
		     return: Label.t}
       | Switch of {cases: Cases.t,
		    default: Label.t option,
		    test: Operand.t}
       | SwitchIP of {int: Label.t,
		      pointer: Label.t,
		      test: Operand.t}

      fun layout t =
	 let
	    open Layout
	 in
	    case t of
	       Arith {prim, args, dst, overflow, success} =>
		  seq [str "Arith ",
		       record [("prim", Prim.layout prim),
			       ("args", Vector.layout Var.layout args),
			       ("dst", Var.layout dst),
			       ("overflow", Label.layout overflow),
			       ("success", Label.layout success)]]
	     | Bug => str "Bug"
	     | CCall {args, prim, return, returnTy} =>
		  seq [str "CCall",
		       record [("args", Vector.layout Operand.layout args),
			       ("prim", Prim.layout prim),
			       ("return", Label.layout return),
			       ("returnTy", Option.layout Type.layout returnTy)]]
	     | Call {args, func, return, ...} =>
		  let
		     val call = seq [Func.layout func, str " ",
				     Vector.layout Operand.layout args]
		     val call =
			case return of
			   Return.Dead => seq [str "Dead ", call]
			 | Return.HandleOnly => seq [str "HandleOnly ", call]
			 | Return.Tail => call
			 | Return.NonTail {cont, handler} => 
			      let
				 val call =
				    seq [Label.layout cont, str " ", paren call]
			      in
				 case handler of
				    Handler.CallerHandler => call
				  | Handler.Handle l =>
				       seq [call, str " handle ", Label.layout l]
				  | Handler.None => seq [call, str " None"]
			      end
		  in
		     call
		  end
	     | Goto {dst, args} =>
		  seq [Label.layout dst, str " ",
		       Vector.layout Operand.layout args]
	     | LimitCheck {failure, kind, success} =>
		  seq [str "LimitCheck ",
		       record [("failure", Label.layout failure),
			       ("kind", LimitCheck.layout kind),
			       ("success", Label.layout success)]]
	     | Raise xs => seq [str "Raise", Vector.layout Operand.layout xs]
	     | Return xs => 
		  seq [str "Return ",
		       seq [str "Raise", Vector.layout Operand.layout xs]]
	     | Runtime {args, prim, return} =>
		  seq [str "Runtime ",
		       record [("args", Vector.layout Operand.layout args),
			       ("prim", Prim.layout prim),
			       ("return", Label.layout return)]]
	     | Switch {test, cases, default} =>
		  seq [str "Switch ",
		       tuple [Operand.layout test,
			      Cases.layout cases,
			      Option.layout Label.layout default]]
	     | SwitchIP {test, int, pointer} =>
		  seq [str "SwitchIP ", tuple [Operand.layout test,
					       Label.layout int,
					       Label.layout pointer]]
	 end

      fun foreachLabel (t, f) =
	 case t of
	    Arith {overflow, success, ...} =>
	       (f overflow; f success)
	  | Bug => ()
	  | CCall {return, ...} => f return
	  | Call {return, ...} => Return.foreachLabel (return, f)
	  | Goto {dst, ...} => f dst
	  | LimitCheck {failure, success, ...} => (f failure; f success)
	  | Raise _ => ()
	  | Return _ => ()
	  | Runtime {return, ...} => f return
	  | Switch {cases, default, ...} =>
	       (Option.app (default, f)
		; Cases.foreach (cases, f))
	  | SwitchIP {int, pointer, ...} =>
	       (f int; f pointer)
   end

structure Kind =
   struct
      datatype t =
	 Cont of {handler: Label.t option}
       | CReturn of {prim: Prim.t}
       | Handler
       | Normal
       | Runtime of {prim: Prim.t}

      val isOnStack =
	 fn Cont _ => true
	  | CReturn _ => false
	  | Handler => true
	  | Normal => false
	  | Runtime _ => true
   end

structure Block =
   struct
      datatype t =
	 T of {
	       args: (Var.t * Type.t) vector,
	       kind: Kind.t,
	       label: Label.t,
	       statements: Statement.t vector,
	       transfer: Transfer.t
	       }

      local
	 fun make f (T r) = f r
      in
	 val args = make #args
	 val kind = make #kind
	 val label = make #label
	 val statements = make #statements
	 val transfer = make #transfer
      end
   
      val clear = Label.clear o label
   end

structure Function =
   struct
      datatype t = T of {args: (Var.t * Type.t) vector,
			 blocks: Block.t vector,
			 name: Func.t,
			 start: Label.t}

      local
	 fun make f (T r) = f r
      in
	 val args = make #args
	 val blocks = make #blocks
	 val name = make #name
	 val start = make #start
      end

      fun dest (T r) = r
      val new = T

      fun dfs (T {blocks, start, ...}, v) =
	 let
	    val numBlocks = Vector.length blocks
	    val {get = labelIndex, set = setLabelIndex, rem, ...} =
	       Property.getSetOnce (Label.plist,
				    Property.initRaise ("index", Label.layout))
	    val _ = Vector.foreachi (blocks, fn (i, Block.T {label, ...}) =>
				     setLabelIndex (label, i))
	    val visited = Array.array (numBlocks, false)
	    fun visit (l: Label.t): unit =
	       let
		  val i = labelIndex l
	       in
		  if Array.sub (visited, i)
		     then ()
		  else
		     let
			val _ = Array.update (visited, i, true)
			val b as Block.T {transfer, ...} =
			   Vector.sub (blocks, i)
			val v' = v b
			val _ = Transfer.foreachLabel (transfer, visit)
			val _ = v' ()
		     in
			()
		     end
	       end
	    val _ = visit start
	    val _ = Vector.foreach (blocks, rem o Block.label)
	 in
	    ()
	 end

   end

structure Program =
   struct
      datatype t = T of {functions: Function.t list,
			 main: Func.t}
   end

end
