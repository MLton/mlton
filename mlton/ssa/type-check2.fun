(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor TypeCheck2 (S: TYPE_CHECK2_STRUCTS): TYPE_CHECK2 = 
struct

open S

type int = Int.t
type word = Word.t
   
datatype z = datatype Exp.t
datatype z = datatype Transfer.t

fun checkScopes (program as
		 Program.T {datatypes, globals, functions, main}): unit =
   let
      datatype 'a status =
	 Undefined
       | InScope of 'a
       | Defined

      fun make' (layout, plist) =
	 let
	    val {get, set, ...} =
	       Property.getSet (plist, Property.initConst Undefined)
	    fun bind (x, v) =
	       case get x of
		  Undefined => set (x, InScope v)
		| _ => Error.bug (concat ["duplicate definition of ",
					  Layout.toString (layout x)])
	    fun reference x =
	       case get x of
		  InScope v => v
		| _ => Error.bug (concat ["reference to ",
					  Layout.toString (layout x),
					  " not in scope"])

	    fun unbind x = set (x, Defined)
	 in (bind, ignore o reference, reference, unbind)
	 end
      fun make (layout, plist) =
	 let val (bind, reference, _, unbind) = make' (layout, plist)
	 in (fn x => bind (x, ()), reference, unbind)
	 end
      val (bindTycon, _, getTycon', _) = make' (Tycon.layout, Tycon.plist)
      val (bindCon, getCon, getCon', _) = make' (Con.layout, Con.plist)
      val (bindVar, getVar, getVar', unbindVar) = make' (Var.layout, Var.plist)
      fun getVars xs = Vector.foreach (xs, getVar)
      val (bindFunc, getFunc, _) = make (Func.layout, Func.plist)
      val (bindLabel, getLabel, unbindLabel) = make (Label.layout, Label.plist)
      fun loopStatement (Statement.T {var, ty, exp, ...}) =
	 let
	    val () = Exp.foreachVar (exp, getVar)
	    val () =
	       case exp of
		  Object {con, ...} => Option.app (con, getCon)
		| _ => ()
	    val () = Option.app (var, fn x => bindVar (x, ty))
	 in
	    ()
	 end
      val loopTransfer =
	 fn Arith {args, ...} => getVars args
	  | Bug => ()
	  | Call {func, args, ...} => (getFunc func; getVars args)
	  | Case {test, cases, default, ...} =>
	       let
		  fun doit (cases: ('a * 'b) vector,
			    equals: 'a * 'a -> bool,
			    toWord: 'a -> word): unit =
		     let
		        val table = HashSet.new {hash = toWord}
			val _ =
			   Vector.foreach
			   (cases, fn (x, _) =>
			    let
			       val _ = 
				  HashSet.insertIfNew
				  (table, toWord x, fn y => equals (x, y),
				   fn () => x, 
				   fn _ => Error.bug "redundant branch in case")
			    in
			       ()
			    end)
		     in
			if isSome default
			   then ()
			else Error.bug "case has no default"
		     end
		  fun doitCon cases =
		     let
		        val numCons = 
			   case Type.dest (getVar' test) of
			      Type.Datatype t => getTycon' t
			    | _ => Error.bug "case test is not a datatype"
			val cons = Array.array (numCons, false)
			val _ =
			   Vector.foreach
			   (cases, fn (con, _) =>
			    let
			       val i = getCon' con
			    in
			       if Array.sub (cons, i)
				  then Error.bug "redundant branch in case"
			       else Array.update (cons, i, true)
			    end)
		     in
			case (Array.forall (cons, fn b => b), isSome default) of
			   (true, true) =>
			      Error.bug "exhaustive case has default"
			 | (false, false) =>
			      Error.bug "non-exhaustive case has no default"
			 | _ => ()
		     end
		  val _ = getVar test
	       in
		  case cases of
		     Cases.Con cs => doitCon cs 
		   | Cases.Word (_, cs) =>
			doit (cs, WordX.equals, Word.fromIntInf o WordX.toIntInf)
	       end
	  | Goto {args, ...} => getVars args
	  | Raise xs => getVars xs
	  | Return xs => getVars xs
	  | Runtime {args, ...} => getVars args
      fun loopFunc (f: Function.t) =
	 let
	    val {args, blocks, ...} = Function.dest f
	    (* Descend the dominator tree, verifying that variable definitions
	     * dominate variable uses.
	     *)
	    fun loop (Tree.T (block, children)): unit =
	       let
		  val Block.T {args, statements, transfer, ...} = block
		  val _ = Vector.foreach (args, bindVar)
		  val _ = Vector.foreach (statements, loopStatement)
		  val _ = loopTransfer transfer
		  val _ = Vector.foreach (children, loop)
		  val _ =
		     Vector.foreach (statements, fn s =>
				     Option.app (Statement.var s, unbindVar))
		  val _ = Vector.foreach (args, unbindVar o #1)
	       in
		  ()
	       end
	    val _ = Vector.foreach (args, bindVar)
	    val _ = Vector.foreach (blocks, bindLabel o Block.label)
	    val _ =
	       Vector.foreach
	       (blocks, fn Block.T {transfer, ...} =>
		Transfer.foreachLabel (transfer, getLabel))
	    val _ = loop (Function.dominatorTree f)
	    val _ = Vector.foreach (blocks, unbindLabel o Block.label)
	    val _ = Vector.foreach (args, unbindVar o #1)
	    val _ = Function.clear f
	 in
	     ()
	 end
      val _ = Vector.foreach
	      (datatypes, fn Datatype.T {tycon, cons} =>
	       (bindTycon (tycon, Vector.length cons) ;
		Vector.foreachi (cons, fn (i, {con, ...}) => bindCon (con, i))))
      val _ = Vector.foreach (globals, loopStatement)
      val _ = List.foreach (functions, bindFunc o Function.name)
      val _ = List.foreach (functions, loopFunc)
      val _ = getFunc main
      val _ = Program.clearTop program
   in
      ()
   end

val checkScopes = Control.trace (Control.Pass, "checkScopes") checkScopes

structure Function =
   struct
      open Function
	 
      fun checkProf (f: t): unit =
	 let
	    val debug = false
	    val {blocks, start, ...} = dest f
	    val {get = labelInfo, rem, set = setLabelInfo, ...} =
	       Property.getSetOnce
	       (Label.plist,
		Property.initRaise ("info", Label.layout))
	    val _ = Vector.foreach (blocks, fn b as Block.T {label, ...} =>
				    setLabelInfo (label,
						  {block = b,
						   sources = ref NONE}))
	    fun goto (l: Label.t, sources: SourceInfo.t list) =
	       let
		  fun bug (msg: string): 'a =
		     let
			val _ = 
			   Vector.foreach
			   (blocks, fn Block.T {label, ...} =>
			    let
			       val {sources, ...} = labelInfo label
			       open Layout
			    in
			       outputl
			       (seq [Label.layout label,
				     str " ",
				     Option.layout
				     (List.layout SourceInfo.layout)
				     (!sources)],
				Out.error)
			    end)
		     in
			Error.bug
			(concat ["checkProf bug found in ", Label.toString l,
				 ": ", msg])
		     end
		  val _ =
		     if not debug
			then ()
		     else
		     let
			open Layout
		     in
			outputl (seq [str "goto (",
				      Label.layout l,
				      str ", ",
				      List.layout SourceInfo.layout sources,
				      str ")"],
				 Out.error)
		     end
		  val {block, sources = r} = labelInfo l
	       in
		  case !r of
		     NONE =>
			let
			   val _ = r := SOME sources
			   val Block.T {statements, transfer, ...} = block
			   datatype z = datatype Statement.t
			   datatype z = datatype ProfileExp.t
			   val sources =
			      Vector.fold
			      (statements, sources,
			       fn (Statement.T {exp, ...}, sources) =>
			       case exp of
				  Profile pe =>
				     (case pe of
					 Enter s => s :: sources
				       | Leave s =>
					    (case sources of
						[] => bug "unmatched Leave"
					      | s' :: sources =>
						   if SourceInfo.equals (s, s')
						      then sources
						   else bug "mismatched Leave"))
				| _ => sources)
			   val _ =
			      if not debug
				 then ()
			      else
			      let
				 open Layout
			      in
				 outputl (List.layout SourceInfo.layout sources,
					  Out.error)
			      end
			   val _ = 
			      if (case transfer of
				     Call {return, ...} =>
					let
					   datatype z = datatype Return.t
					in
					   case return of
					      Dead => false
					    | NonTail _ => false
					    | Tail => true
					end
				   | Raise _ => true
				   | Return _ => true
				   | _ => false)
				 then (case sources of
					  [] => ()
					| _ => bug "nonempty sources when leaving function")
			      else ()
			in
			   Transfer.foreachLabel
			   (transfer, fn l => goto (l, sources))
			end
		   | SOME sources' =>
			if List.equals (sources, sources', SourceInfo.equals)
			   then ()
			else bug "mismatched block"
	       end
	    val _ = goto (start, [])
	    val _ = Vector.foreach (blocks, fn Block.T {label, ...} => rem label)
	 in
	    ()
	 end
   end

fun checkProf (Program.T {functions, ...}): unit =
   List.foreach (functions, fn f => Function.checkProf f)

val checkProf = Control.trace (Control.Pass, "checkProf") checkProf

fun typeCheck (program as Program.T {datatypes, ...}): unit =
   let
      val _ = checkScopes program
      val _ =
	 if !Control.profile <> Control.ProfileNone
	    then checkProf program
	 else ()
      val out = Out.error
      val print = Out.outputc out
      exception TypeError
      fun error (msg, lay) =
	 (print (concat ["Type error: ", msg, "\n"])
	  ; Layout.output (lay, out)
	  ; print "\n"
	  ; raise TypeError)
      val {get = conInfo: Con.t -> {result: Type.t,
				    ty: Type.t,
				    tycon: Tycon.t},
	   set = setConInfo, ...} =
	 Property.getSetOnce
	 (Con.plist, Property.initRaise ("TypeCheck.info", Con.layout))
      val conTycon = #tycon o conInfo
      val _ =
	 Vector.foreach
	 (datatypes, fn Datatype.T {tycon, cons} =>
	  let
	     val result = Type.datatypee tycon
	  in
	     Vector.foreach (cons, fn {con, args} =>
			     setConInfo (con, {result = result,
					       ty = Type.conApp (con, args),
					       tycon = tycon}))
	  end)
      fun isSubtype (t1: Type.t, t2: Type.t): bool =
	 Type.equals (t1, t2)
	 orelse (case (Type.dest t1, Type.dest t2) of
		    (Type.Object {con, ...}, Type.Datatype tyc) =>
		       (case con of
			   NONE => false
			 | SOME c => Tycon.equals (conTycon c, tyc))
		  | _ => false)
      fun coerce {from: Type.t, to: Type.t}: unit =
	 if isSubtype (from, to)
	    then ()
	 else error ("TypeCheck.coerce",
		     Layout.record [("from", Type.layout from),
				    ("to", Type.layout to)])
      val coerce =
	 Trace.trace ("TypeCheck.coerce",
		      fn {from, to} => let open Layout
				       in record [("from", Type.layout from),
						  ("to", Type.layout to)]
				       end,
				    Unit.layout) coerce
      fun coerces (from, to) =
	 Vector.foreach2 (from, to, fn (from, to) =>
			  coerce {from = from, to = to})
      fun object {args, con, resultType} =
	 let
	    fun err () = error ("bad object", Layout.empty)
	 in
	    case Type.dest resultType of
	       Type.Object {args = args', con = con'} =>
		  (if Option.equals (con, con', Con.equals)
		      andalso (Vector.foreach2
			       (args, args', fn (t, {elt = t', ...}) =>
				coerce {from = t, to = t'})
			       ; true)
		      then resultType
		   else err ())
	     | _ => err ()
	 end
      fun select {object: Type.t, offset: int, resultType = _}: Type.t =
	 case Type.dest object of
	    Type.Object {args, ...} => #elt (Vector.sub (args, offset))
	  | _ => error ("select of non object", Layout.empty)
      fun update {object, offset, value} =
	 case Type.dest object of
	    Type.Object {args, ...} =>
	       let
		  val {elt, isMutable} = Vector.sub (args, offset)
		  val () = coerce {from = value, to = elt}
		  val () =
		     if isMutable
			then ()
		     else error ("update of non-mutable field", Layout.empty)
	       in
		  ()
	       end
	  | _ => error ("update of non object", Layout.empty)
      fun filter {con, test, variant} =
	 let
	    val {result, ty, ...} = conInfo con
	    val () = coerce {from = test, to = result}
	    val () = Option.app (variant, fn to => coerce {from = ty, to = to})
	 in
	    ()
	 end
      fun filterWord (from, s) = coerce {from = from, to = Type.word s}
      fun primApp {args, prim, resultType, resultVar = _, targs} =
	 let
	    datatype z = datatype Prim.Name.t
	    val () =
	       if Type.checkPrimApp {args = args,
				     isSubtype = isSubtype,
				     prim = prim,
				     result = resultType,
				     targs = targs}
		  then ()
	       else error ("bad primapp",
			   let
			      open Layout
			   in
			      seq [Prim.layout prim,
				   tuple (Vector.toListMap (args, Type.layout))]
			   end)
	 in
	    resultType
	 end
      val _ =
	 analyze {coerce = coerce,
		  const = Type.ofConst,
		  filter = filter,
		  filterWord = filterWord,
		  fromType = fn x => x,
		  layout = Type.layout,
		  object = object,
		  primApp = primApp,
		  program = program,
		  select = select,
		  update = update,
		  useFromTypeOnBinds = true}
	 handle e => error (concat ["analyze raised exception ",
				    Layout.toString (Exn.layout e)],
			    Layout.empty)
      val _ = Program.clear program
   in
      ()
   end

end
