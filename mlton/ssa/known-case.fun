functor KnownCase (S: KNOWN_CASE_STRUCTS): KNOWN_CASE =
struct

open S
open Exp Transfer

type int = Int.t
type word = Word.t

fun mkPost ()
  = let
      val post = ref []
    in
      {addPost = fn th => List.push (post, th),
       post = fn () => List.foreach(!post, fn th => th ())}
    end

structure TyconInfo =
  struct
    datatype t = T of {numCons: int}

    local 
      fun make f (T r) = f r
      fun make' f = (make f, ! o (make f))
    in
      val numCons = make #numCons
    end

    fun layout (T {numCons, ...}) 
      = Layout.record [("numCons", Int.layout numCons)]
  end

structure ConInfo =
  struct
    datatype t = T of {index: int}

    local 
      fun make f (T r) = f r
      fun make' f = (make f, ! o (make f))
    in
      val index = make #index
    end

    fun layout (T {index, ...}) 
      = Layout.record [("index", Int.layout index)]
  end

structure ConValue =
  struct
    type u = Var.t vector option
    type t = u option
    fun equals (x, y)
      = Option.equals
        (x, y, fn (x', y') =>
	 Option.equals
	 (x', y', fn (x'', y'') =>
	  Vector.equals (x'', y'', Var.equals)))
    val layout = Option.layout (Option.layout (Vector.layout Var.layout))

    val join' : u * u -> u
      = fn (SOME x'', SOME y'') 
         => if Vector.equals (x'', y'', Var.equals)
	      then SOME x''
	      else NONE
	 | (NONE, _) => NONE
	 | (_, NONE) => NONE
    val join : t * t -> t
      = fn (SOME x', SOME y') => SOME (join' (x', y'))
         | (NONE, y) => y
         | (x, NONE) => x

    fun newKnown args = SOME (SOME args)
    fun newUnknown () = SOME NONE
    fun new () = NONE

    fun isTop (x : t) = isSome x
    val isBot = not o isTop
  end

structure TyconValue =
  struct
    type t = ConValue.t vector

    val layout = Vector.layout ConValue.layout

    val join : t * t -> t
      = fn (x, y) => Vector.map2 (x, y, ConValue.join)

    fun newKnown (numCons, i, args)
      = Vector.tabulate
        (numCons, fn j => 
	 if i = j 
	   then ConValue.newKnown args 
	   else ConValue.new ())
    fun newUnknown numCons
      = Vector.new (numCons, ConValue.newUnknown ())
    fun new numCons
      = Vector.new (numCons, ConValue.new ())
  end

structure VarInfo =
  struct
    datatype t = T of {active: bool ref, 
		       tyconValues: TyconValue.t list ref}

    local 
      fun make f (T r) = f r
      fun make' f = (make f, ! o (make f))
    in
      val (active, active') = make' #active
      val (tyconValues, tyconValues') = make' #tyconValues
    end

    fun layout (T {tyconValues, ...}) 
      = Layout.record [("tyconValues", 
			List.layout TyconValue.layout (!tyconValues))]

    fun deactivate (T {active, ...}) = active := false
    fun activate (T {active, ...}) = active := true
    fun activate' (vi, addPost)
      = (addPost (fn () => deactivate vi);
	 activate vi)
    val active = active'

    fun popTyconValue (T {tyconValues, ...}) = ignore (List.pop tyconValues)
    fun pushTyconValue (T {tyconValues, ...}, tcv) = List.push (tyconValues, tcv)
    fun pushTyconValue' (vi, tcv, addPost)
      = (addPost (fn () => popTyconValue vi);
	 pushTyconValue (vi, tcv))
    fun tyconValue (T {tyconValues, ...}) 
      = case !tyconValues of h::_ => SOME h | _ => NONE

    fun pushActiveTyconValue (vi, tcv, addPost, addPost')
      = if active vi
	  then let val tcv' = valOf (tyconValue vi)
	       in 
		 popTyconValue vi;
		 pushTyconValue (vi, TyconValue.join (tcv, tcv'))
	       end
	  else (activate' (vi, addPost');
		pushTyconValue' (vi, tcv, addPost))
  end

structure LabelInfo =
  struct
    datatype t = T of {activations: (VarInfo.t * TyconValue.t) list ref,
		       block: Block.t, 
		       depth: int ref,
		       pred: Label.t option option ref}

    local 
      fun make f (T r) = f r
      fun make' f = (make f, ! o (make f))
    in
      val (activations, activations') = make' #activations
      val block = make #block
      val (depth, depth') = make' #depth
      val (pred, pred') = make' #pred
    end

    fun layout (T {activations, pred, ...}) 
      = Layout.record 
        [("pred", Option.layout (Option.layout Label.layout) (!pred))]

    fun new block = T {activations = ref [],
		       block = block,
		       depth = ref 0,
		       pred = ref NONE}

    fun addPred (T {pred, ...}, l)
      = case !pred
	  of NONE => pred := SOME (SOME l)
	   | SOME NONE => ()
	   | SOME (SOME l') => if Label.equals (l, l')
				 then ()
				 else pred := SOME NONE
    fun onePred (T {pred, ...})
      = case !pred
	  of SOME (SOME _) => true
	   | _ => false

    fun addActivation (T {activations, ...}, activation)
      = List.push (activations, activation)
    fun activate (T {activations, ...}, addPost)
      = let
	  val {addPost = addPost', post = post'} = mkPost ()
	in
	  List.foreach
	  (!activations, fn (vi, tcv) =>
	   VarInfo.pushActiveTyconValue (vi, tcv, addPost, addPost'));
	  post' ()
	end
    val activate : t * ((unit -> unit) -> unit) -> unit
      = Trace.trace
        ("KnownCase.activate",
	 fn (T {activations, block as Block.T {label, ...}, ...}, _) =>
	 let open Layout
	 in
	   seq [Label.layout label,
		str " ",
		(List.layout (tuple2 (Layout.ignore, 
				      TyconValue.layout))
		             (!activations))]
	 end,
	 Layout.ignore)
	activate

  end

fun simplify (program as Program.T {globals, datatypes, functions, main})
  = let
      (* restore and shrink *)
      val restore = restoreFunction globals
      val shrink = shrinkFunction globals

      (* tyconInfo and conInfo *)
      val {get = tyconInfo: Tycon.t -> TyconInfo.t,
	   set = setTyconInfo, ...}
	= Property.getSetOnce
	  (Tycon.plist, Property.initRaise ("knownCase.tyconInfo", Tycon.layout))
      val {get = conInfo: Con.t -> ConInfo.t,
	   set = setConInfo, ...}
	= Property.getSetOnce
	  (Con.plist, Property.initRaise ("knownCase.conInfo", Con.layout))
      val _ = Vector.foreach
	      (datatypes, fn Datatype.T {tycon, cons} =>
	       (setTyconInfo (tycon, TyconInfo.T {numCons = Vector.length cons});
		Vector.foreachi
		(cons, fn (i, {con, ...}) =>
		 setConInfo (con, ConInfo.T {index = i}))))
      (* Diagnostics *)
      val _ = Control.diagnostics
	      (fn display =>
	       let open Layout
	       in
		 Vector.foreach
		 (datatypes, fn Datatype.T {tycon, cons} =>
		  let val tci = tyconInfo tycon
		  in
		    display (seq [Tycon.layout tycon, str " ",
				  TyconInfo.layout tci,
				  Vector.layout 
				  (fn {con, ...} =>
				   let val ci = conInfo con
				   in
				     seq [Con.layout con, str " ",
					  ConInfo.layout ci]
				   end)
				  cons])
		  end)
	       end)
      fun optimizeTycon tycon = not (Tycon.equals (tycon, Tycon.bool))	      
      fun optimizeType ty = case Type.dest ty
			      of Type.Datatype tycon => optimizeTycon tycon
			       | _ => false

      (* varInfo *)
      val {get = varInfo: Var.t -> VarInfo.t,
	   set = setVarInfo, ...}
	= Property.getSetOnce
	  (Var.plist, Property.initFun (fn _ => VarInfo.T {active = ref false,
							   tyconValues = ref []}))

      fun initVarInfo (x, ty, exp)
	= case Type.dest ty
	    of Type.Datatype tycon
	     => if optimizeTycon tycon
		  then let
			 val numCons = TyconInfo.numCons (tyconInfo tycon)
			 val tyconValue
			   = case exp
			       of SOME (ConApp {con, args})
				=> TyconValue.newKnown 
				   (numCons, ConInfo.index (conInfo con), args)
			        | _ => TyconValue.newUnknown numCons
		       in
			 VarInfo.pushTyconValue
			 (varInfo x, tyconValue)
		       end
		  else ()
	     | _ => ()

      fun initVarInfoArgs args 
	= Vector.foreach 
	  (args, fn (x, ty) =>
	   initVarInfo (x, ty, NONE))
      fun initVarInfoStatement (Statement.T {var, ty, exp})
	= Option.app (var, fn x => initVarInfo (x, ty, SOME exp))
      fun initVarInfoStatements statements
	= Vector.foreach (statements, initVarInfoStatement)

      val _ = initVarInfoStatements globals
      (* Diagnostics *)
      val _ = Control.diagnostics
	      (fn display =>
	       let open Layout
	       in
		 Vector.foreach
		 (globals, fn Statement.T {var, ...} => 
		  Option.app
		  (var, fn x =>
		   let val vi = varInfo x
		   in 
		     display (seq [Var.layout x, str " ",
				   VarInfo.layout vi])
		   end))
	       end)

      (* labelInfo *)
      val {get = labelInfo: Label.t -> LabelInfo.t, 
	   set = setLabelInfo, ...}
	= Property.getSetOnce
	  (Label.plist, Property.initRaise ("knownCase.labelInfo", Label.layout))

      val functions
	= List.revMap
	  (functions, fn f =>
	   let
	     val {name, args, start, blocks, returns, raises} = Function.dest f

	     val _ = initVarInfoArgs args
	     val _ = Vector.foreach
	             (blocks, fn block as Block.T {label, ...} =>
		      setLabelInfo (label, LabelInfo.new block))
	     val _ = Vector.foreach
	             (blocks, fn Block.T {label, args, statements, transfer} =>
		      (initVarInfoArgs args;
		       initVarInfoStatements statements;
		       Transfer.foreachLabel
		       (transfer, fn l =>
			let val li = labelInfo l
			in LabelInfo.addPred (li, label)
			end)))
	     (* Diagnostics *)
	     val _ = Control.diagnostics
	             (fn display =>
		      let open Layout
		      in
			Vector.foreach
			(blocks, fn Block.T {label, ...} =>
			 let val li = labelInfo label
			 in 
			   display (seq [Label.layout label, str " ",
					 LabelInfo.layout li])
			 end)
		      end)

	     val newBlocks = ref []
	     local 
	       val table: {hash: word,
			   transfer: Transfer.t,
			   label: Label.t} HashSet.t
		 = HashSet.new {hash = #hash}
	     in
	       fun newBlock transfer
		 = let
		     val hash = Transfer.hash transfer
		     val {label, ...}
		       = HashSet.lookupOrInsert
		         (table, hash,
			  fn {transfer = transfer', ...} =>
			  Transfer.equals (transfer, transfer'),
			  fn () => 
			  let
			    val label = Label.newNoname ()
			    val block = Block.T
			                {label = label,
					 args = Vector.new0 (),
					 statements = Vector.new0 (),
					 transfer = transfer}
			    val _ = setLabelInfo (label, LabelInfo.new block)
			    val _ = List.push (newBlocks, block)
			  in
			    {hash = hash,
			     label = label,
			     transfer = transfer}
			  end)
		   in
		     label
		   end
	       fun bugBlock () = newBlock Bug
	     end
	     val traceRewriteTransfer 
	       = Trace.traceRec 
	         ("KnownCase.rewriteTransfer",
		  Layout.tuple2 (Bool.layout, Transfer.layout),
		  Option.layout
		  (Layout.tuple2
		   (Vector.layout Statement.layout,
		    Transfer.layout)))
	     fun rewriteTransfer rewriteTransferRec (top, transfer)
	       = DynamicWind.withEscape
	         (fn escape =>
		  case transfer
		    of Goto {dst, args}
		     => let
			  val {addPost, post} = mkPost ()
			  val LabelInfo.T
			      {block = blockDst, 
			       depth = depthDst, ...}
			    = labelInfo dst
			  val Block.T {args = argsDst, 
				       statements = statementsDst, 
				       transfer = transferDst, ...} 
			    = blockDst
			in
			  if Vector.length statementsDst <= 
			     (if top then 0 else 0)
			     andalso
			     !depthDst <= 1
			    then let
				   val _ = addPost (fn () => Int.dec depthDst)
				   val _ = Int.inc depthDst
				   val _ = Vector.foreach2
				           (args, argsDst, fn (x, (y, ty)) =>
					    if optimizeType ty
					      then let
						     val xvi = varInfo x
						     val yvi = varInfo y
						   in
						     VarInfo.pushTyconValue'
						     (yvi, 
						      valOf (VarInfo.tyconValue xvi),
						      addPost)
						   end
					      else ())
				 in
				   (case rewriteTransferRec (false, transferDst)
				     of NONE => NONE
				      | SOME (newStatements, newTransfer)
				      => SOME (Vector.concat
					       [Vector.map2
						(args, argsDst, fn (x, (y, ty)) =>
						 Statement.T {var = SOME y,
							      ty = ty,
							      exp = Var x}),
						statementsDst,
						newStatements],
					       newTransfer))
				   before (post ())
				 end
			    else NONE
			end
	             | Case {test, cases = Cases.Con cases, default}
		     => let
			  val {addPost, post} = mkPost ()

			  val testvi = varInfo test
			  val tyconValue as conValues 
			    = case VarInfo.tyconValue testvi
				of NONE => escape NONE
				 | SOME tyconValue => tyconValue
			  val numCons = Vector.length conValues

			  datatype z = None 
			             | One of (int * ConValue.u)
			             | Many

			  fun doOneSome (i, args)
			    = let
				val transfer
				  = case Vector.peek
			                 (cases, fn (con, _) =>
					  ConInfo.index (conInfo con) = i)
				      of SOME (con, dst)
				       => Goto {dst = dst, args = args}
				       | NONE
				       => Goto {dst = valOf default,
						args = Vector.new0 ()}
			      in
				case rewriteTransferRec (false, transfer)
				  of NONE => SOME (Vector.new0 (), transfer)
				   | sst => sst
			      end

			  fun rewriteDefault conValues'
			    = let
				val dst = valOf default
				val LabelInfo.T
				    {block = blockDst, ...}
				  = labelInfo dst
				val Block.T {transfer = transferDst, ...}
				  = blockDst

				val _ = VarInfo.pushTyconValue'
				        (testvi, conValues', addPost)
			      in
				rewriteTransferRec 
				(false, Goto {dst = dst, args = Vector.new0 ()})
			      end

			  fun doOneNone i
			    = case Vector.peek
			           (cases, fn (con, _) =>
				    ConInfo.index (conInfo con) = i)
				of SOME (con, dst)
				 => SOME (Vector.new0 (),
					  Case 
					  {test = test,
					   cases = Cases.Con (Vector.new1 (con, dst)),
					   default = if numCons = 1
						       then NONE
						       else SOME (bugBlock ())})
			         | NONE => rewriteDefault conValues

fun doMany ()
  = let
      val cons = Array.new (numCons, false)
      val cases = Vector.keepAllMap
	          (cases, fn (con, dst) =>
		   let
		     val conIndex = ConInfo.index (conInfo con)
		     val _ = Array.update (cons, conIndex, true)
		   in
		     if ConValue.isTop (Vector.sub (conValues, conIndex))
		       then SOME (con, dst)
		       else NONE
		   end)
      val (cases, default)
	= case default
	    of NONE => (cases, NONE)
	     | SOME dst
	     => let
		  val conValues' = Vector.tabulate
		                   (numCons, fn i =>
				    if Array.sub (cons, i)
				      then ConValue.new ()
				      else Vector.sub (conValues, i))
		in
		  case rewriteDefault conValues'
		    of SOME (statements, 
			     Case {test = test', 
				   cases = Cases.Con cases',
				   default = default'})
		     => if Vector.length statements = 0
		           andalso
			   Var.equals (test, test')
			  then (Vector.concat [cases, cases'], default')
			  else (cases, SOME dst)
		     | SOME (statements, transfer)
		     => if Vector.length statements = 0
			  then (cases, SOME (newBlock transfer))
			  else (cases, SOME dst)
		     | NONE => (cases, SOME dst)
		end
      val numCases = Vector.length cases
      fun doit (cases, default)
	= SOME (Vector.new0 (),
		Case {test = test,
		      cases = Cases.Con cases,
		      default = default})
    in
      if numCases = numCons
	then doit (cases, NONE)
	else doit (cases,
		   case default
		     of SOME _ => default
		      | NONE => SOME (bugBlock ()))
    end
			       in
				 (if not top
				     andalso
				     Vector.forall
				     (conValues, ConValue.isTop)
				    then NONE
				    else case Vector.foldi
				              (conValues, None, 
					       fn (i, conValue, Many) => Many
						| (i, conValue, One cv)
					        => (case conValue
						      of NONE => One cv
						       | _ => Many)
						| (i, conValue, None)
					        => (case conValue
						      of NONE => None
						       | SOME cv => One (i, cv)))
					   of None 
					    => SOME (Vector.new0 (), Bug)
					    | One (i, SOME args) 
					    => doOneSome (i, args)
					    | One (i, NONE) 
					    => doOneNone i
					    | Many 
					    => doMany ())
				 before (post ())
			       end
		     | _ => NONE)
	     val rewriteTransfer = traceRewriteTransfer rewriteTransfer
	     fun rewriteBlock (Block.T {label, args, statements, transfer})
	       = let
		   val (statements, transfer)
		     = case rewriteTransfer (true, transfer)
			 of NONE => (statements, transfer)
			  | SOME (newStatements, newTransfer)
			  => (Vector.concat [statements,newStatements],
			      newTransfer)
		 in
		    Block.T {label = label,
			     args = args,
			     statements = statements,
			     transfer = transfer}
		 end
(*
	     val blocks = Vector.map (blocks, rewriteBlock)
	     val blocks = Vector.concat [Vector.fromListRev (!newBlocks), blocks]
*)
	     fun activateTransfer transfer
	       = DynamicWind.withEscape
	         (fn escape =>
		  case transfer
		    of Goto {dst, args}
		     => let
			  val liDst as LabelInfo.T 
			               {block = blockDst as Block.T 
					                    {args = argsDst, 
							     ...},
					...}
			    = labelInfo dst
			in
			  if LabelInfo.onePred liDst
			    then Vector.foreach2
			         (args, argsDst, fn (x, (y, ty)) =>
				  if optimizeType ty
				    then let
					   val xvi = varInfo x
					   val yvi = varInfo y
					   val conValues' 
					     = valOf (VarInfo.tyconValue xvi)
					 in
					   LabelInfo.addActivation
					   (liDst, (yvi, conValues'))
					 end
				    else ())
			    else ()
			end
		     | Case {test, cases = Cases.Con cases, default}
		     => let
			  val testvi = varInfo test
			  val tyconValue as conValues
			    = case VarInfo.tyconValue testvi
				of NONE => escape ()
				 | SOME tyconValue => tyconValue
			  val numCons = Vector.length conValues

			  val cons = Array.new (numCons, false)
			in
			  Vector.foreach
			  (cases, fn (con, dst) =>
			   let
			     val conIndex = ConInfo.index (conInfo con)
			     val _ = Array.update (cons, conIndex, true)
			     val liDst as LabelInfo.T
			                  {block = blockDst as Block.T
					                       {args = argsDst,
								...},
					   ...}
			       = labelInfo dst
			     val conValues' 
			       = TyconValue.newKnown 
			         (numCons, conIndex, Vector.map (argsDst, #1))
			   in
			     if LabelInfo.onePred liDst
			       then LabelInfo.addActivation
				    (liDst, (testvi, conValues'))
			       else ()
			   end);
			  Option.app
			  (default, fn dst =>
			   let
			     val liDst as LabelInfo.T
			                  {...}
			       = labelInfo dst
			     val conValues' = Vector.tabulate
			                      (numCons, fn i =>
					       if Array.sub (cons, i)
						 then ConValue.new ()
						 else Vector.sub (conValues, i))
			   in
			     if LabelInfo.onePred liDst
			       then LabelInfo.addActivation
				    (liDst, (testvi, conValues'))
			       else ()
			   end)
			end
		     | _ => ())
	     fun doitTree tree 
	       = let
		   fun loop (Tree.T (block as Block.T {label, transfer, ...}, 
				     children))
		     = let
			 val {addPost, post} = mkPost ()
 			 val _ = LabelInfo.activate (labelInfo label, addPost)
			 val _ = activateTransfer transfer
			 val block = rewriteBlock block
		       in
			 List.push (newBlocks, block) ;
			 Vector.foreach (children, loop) ;
			 post ()
		       end
		   val _ = loop tree
		 in
		   Vector.fromListRev (!newBlocks)
		 end
	     val blocks = doitTree (Function.dominatorTree f)

	     val f = Function.new {name = name,
				   args = args,
				   start = start,
				   blocks = blocks,
				   returns = returns,
				   raises = raises}
	     val f = restore f
	     val f = shrink f
	     val f = shrink f
	     val _ = Function.clear f
	   in
	     f
	   end)
      val program = Program.T {datatypes = datatypes,
			       globals = globals,
			       functions = functions,
			       main = main}
      val _ = Program.clearTop program
    in
      program
    end

end