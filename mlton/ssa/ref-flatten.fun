(* Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor RefFlatten (S: REF_FLATTEN_STRUCTS): REF_FLATTEN = 
struct

open S

type int = Int.t

datatype z = datatype Exp.t
datatype z = datatype Statement.t
datatype z = datatype Transfer.t

structure Set = DisjointSet

structure Finish =
   struct
      datatype t = T of {flat: Type.t Prod.t option, 
			 ty: Type.t}

      val _: t -> Layout.t =
	 fn T {flat, ty} =>
	 let
	    open Layout
	 in
	    record [("flat",
		     Option.layout (fn p => Prod.layout (p, Type.layout)) flat),
		    ("ty", Type.layout ty)]
	 end
   end

structure Value =
   struct
      datatype t =
	 Ground of Type.t
       | Object of object
       | Weak of {arg: t,
		  finalType: Type.t option ref}
      and flat =
	 NotFlat
	| Offset of {object: object,
		     offset: int}
	| Unknown
      and object =
	 Obj of {args: t Prod.t,
		 con: ObjectCon.t,
		 finalComponents: Type.t Prod.t option ref,
		 finalOffsets: int vector option ref,
		 finalType: Type.t option ref,
		 flat: flat ref}

      local
	 open Layout
      in
	 fun layout v: Layout.t =
	    case v of
	       Ground _ => str "Ground"
	     | Object ob => layoutObject ob
	     | Weak _ => str "Weak"
	 and layoutFlat (f: flat): Layout.t =
	    case f of
	       NotFlat => str "NotFlat"
	     | Offset {offset, ...} =>
		  seq [str "Offset ",
		       record [("offset", Int.layout offset)]]
	     | Unknown => str "Unknown"
	 and layoutObject (Obj {args, con, flat, ...}) =
	    seq [str "Object ",
		 record [("args", Prod.layout (args, layout)),
			 ("con", ObjectCon.layout con),
			 ("flat", layoutFlat (! flat))]]
      end
   end

structure Flat =
   struct
      datatype t = datatype Value.flat

      val layout = Value.layoutFlat
   end

structure Object =
   struct
      datatype t = datatype Value.object

      val layout = Value.layoutObject

      fun equals (Obj {flat = f, ...}, Obj {flat = f', ...}) = f = f'
   end

datatype z = datatype Object.t

structure Value =
   struct
      open Value

      fun ground () = Ground

      val deObject: t -> Object.t option =
	 fn v =>
	 case v of
	    Object ob => SOME ob
	  | _ => NONE

      fun deFlat {inner: t, outer: Object.t}: Object.t option =
	 case inner of
	    Object (z as Obj {flat, ...}) =>
	       (case ! flat of
		   Flat.Offset {object, ...} =>
		      if Object.equals (object, outer) then SOME z else NONE
		 | _ => NONE)
	  | _ => NONE
	       
      fun dontFlatten (v: t): unit =
	 case v of
	    Object (Obj {flat, ...}) => flat := NotFlat
	  | _ => ()
   end

structure VarInfo =
   struct
      datatype t =
	 NonObject
       | Object of {components: Var.t vector,
		    flat: Flat.t ref}
	 
      val layout =
	 let
	    open Layout
	 in
	    fn NonObject => str "NonObject"
	     | Object {components, flat} =>
		  seq [str "Object ",
		       record [("components",
				Vector.layout Var.layout components),
			       ("flat", Flat.layout (!flat))]]
	 end
   end

fun flatten (program as Program.T {datatypes, functions, globals, main}) =
   let
      val {get = varData: Var.t -> {info: VarInfo.t ref,
				    ty: Type.t},
	   set = setVarData, ...} =
	 Property.getSetOnce (Var.plist, Property.initRaise ("info", Var.layout))
      val () =
	 Program.foreachVar (program, fn (x, t) =>
			     setVarData (x, {info = ref VarInfo.NonObject,
					     ty = t}))
      val varInfo = #info o varData
      val varType = #ty o varData
      val varInfo =
	 Trace.trace ("RefFlatten.varInfo",
		      Var.layout, Ref.layout VarInfo.layout)
	 varInfo
      val {get = conValue: Con.t -> Value.t option ref, ...} =
	 Property.get (Con.plist, Property.initFun (fn _ => ref NONE))
      val conValue =
	 Trace.trace ("conValue",
		      Con.layout, Ref.layout (Option.layout Value.layout))
	 conValue
      val {get = typeValue: Type.t -> Value.t, ...} =
	 Property.get
	 (Type.plist,
	  Property.initRec
	  (fn (t, typeValue) =>
	   case Type.dest t of
	      Type.Object {args, con} =>
		 let
		    (* Only may flatten objects with mutable fields, and where
		     * the field isn't unit.  Flattening a unit field could lead
		     * to a problem because the containing object might be
		     * otherwise immutable, and hence the unit ref would lose its
		     * identity.  We can fix this once objects have a notion of
		     * identity independent of mutability.
		     *)
		    val flat =
		       ref
		       (if (case con of ObjectCon.Tuple => true | _ => false)
			   andalso (Vector.exists
				    (Prod.dest args, fn {elt, isMutable} =>
				     isMutable andalso not (Type.isUnit elt)))
			   then Flat.Unknown
			else Flat.NotFlat)
		    val v = 
		       Value.Object
		       (Value.Obj {args = Prod.map (args, typeValue),
				   con = con,
				   finalComponents = ref NONE,
				   finalOffsets = ref NONE,
				   finalType = ref NONE,
				   flat = flat})
		    val () =
		       case con of
			  ObjectCon.Con c => conValue c := SOME v
			| _ => ()
		 in
		    v
		 end
	    | Type.Weak t => Value.Weak {arg = typeValue t,
					 finalType = ref NONE}
	    | _ => Value.Ground t))
      val typeValue =
	 Trace.trace ("typeValue", Type.layout, Value.layout) typeValue
      val varValue = typeValue o varType
      val varObject = Value.deObject o varValue
      val () =
	 Program.foreachPrimApp
	 (program, fn {args, prim} =>
	  let
	     fun dontFlatten () =
		Vector.foreach (args, Value.dontFlatten o varValue)
	     datatype z = datatype Prim.Name.t
	  in
	     case Prim.name prim of
		FFI _ =>
		  (* Some imports, like Real64.modf, take ref cells that can not
		   * be flattened.
		   *)
		  dontFlatten ()
	     | MLton_size => dontFlatten ()
	     | _ => ()
	  end)
      (* Mark a variable as flat if it is used only once and that use is in an
       * object allocation.
       *)
      fun use x =
	 case ! (varInfo x) of
	    VarInfo.Object {flat, ...} => flat := Flat.NotFlat
	  | _ => ()
      val use = Trace.trace ("RefFlatten.use", Var.layout, Unit.layout) use
      fun uses xs = Vector.foreach (xs, use)
      fun loopStatement (s: Statement.t): unit =
	 case s of
	    Bind {exp = Exp.Object {args, ...}, var, ...} =>
	       (case var of
		   NONE => uses args
		 | SOME var =>
		      case Value.deObject (varValue var) of
			 NONE => uses args
		       | SOME object =>
			    let
			       val () =
				  varInfo var
				  := VarInfo.Object {components = args,
						     flat = ref Flat.Unknown}
			    in
			       Vector.foreachi
			       (args, fn (offset, x) =>
				case ! (varInfo x) of
				   VarInfo.NonObject => ()
				 | VarInfo.Object {flat, ...} => 
				      let
					 datatype z = datatype Flat.t
				      in
					 case !flat of
					    Unknown =>
					       flat :=
					       Offset {object = object,
						       offset = offset}
					  | _ => flat := NotFlat
				      end)
			    end)
	  | _ => Statement.foreachUse (s, use)
      val loopStatement =
	 Trace.trace ("RefFlatten.loopStatement", Statement.layout, Unit.layout)
	 loopStatement
      fun loopStatements ss = Vector.foreach (ss, loopStatement)
      fun loopTransfer t = Transfer.foreachVar (t, use)
      val () = loopStatements globals
      val () =
	 List.foreach
	 (functions, fn f =>
	  Function.dfs
	  (f, fn Block.T {statements, transfer, ...} =>
	   (loopStatements statements
	    ; loopTransfer transfer
	    ; fn () => ())))
      fun foreachObject (f): unit =
	 let
	    fun loopStatement s =
	       case s of
		  Bind {exp = Exp.Object {args, ...}, var, ...} =>
		     Option.app
		     (var, fn var =>
		      case varValue var of
			 Value.Ground _ => ()
		       | Value.Object obj => f (var, args, obj)
		       | _ => Error.bug "Object with strange value")
		| _ => ()
	    val () = Vector.foreach (globals, loopStatement)
	    val () =
	       List.foreach
	       (functions, fn f =>
		let
		   val {blocks, ...} = Function.dest f
		in
		   Vector.foreach
		   (blocks, fn Block.T {statements, ...} =>
		    Vector.foreach (statements, loopStatement))
		end)
	 in
	    ()
	 end
      (* Try to flatten each ref. *)
      val () =
	 foreachObject
	 (fn (var, _, Obj {flat, ...}) =>
	  let
	     datatype z = datatype Flat.t
	     val flat'Ref as ref flat' =
		case ! (varInfo var) of
		   VarInfo.NonObject => Error.bug "Object with NonObject"
		 | VarInfo.Object {flat, ...} => flat
	     fun notFlat () =
		(flat := NotFlat
		 ; flat'Ref := NotFlat)
	  in
	     case flat' of
		Offset {object = obj, offset = i} =>
		   (case ! flat of
		       NotFlat => notFlat ()
		     | Offset {object = obj', offset = i'} =>
			  if i = i' andalso Object.equals (obj, obj')
			     then ()
			  else notFlat ()
		     | Unknown => flat := flat')
	      | _ => notFlat ()
	  end)
      (* The following two disallows could be improved by disallowing only
       * if the value is potentially big.
       *)
      (* Disallow flattening of globals. *)
      val () =
	 Vector.foreach
	 (globals, fn s =>
	  case s of
	     Bind {var, ...} =>
		Option.app (var, fn x => Value.dontFlatten (varValue x))
	   | _ => ())
      (* Disallow passing flattened refs across function boundaries. *)
      val () =
	 List.foreach
	 (functions, fn f =>
	  let
	     val {args, raises, returns, ...} = Function.dest f
	     val () = Vector.foreach (args, fn (_, t) =>
				      Value.dontFlatten (typeValue t))
	     fun rr vso =
		Option.app (vso, fn vs =>
			    Vector.foreach (vs, Value.dontFlatten o typeValue))
	     val () = rr raises
	     val () = rr returns
	  in
	     ()
	  end)
      (* Disallow flattening into object components that aren't explicitly
       * constructed.
       *)
      val () =
	 foreachObject
	 (fn (_, args, obj) =>
	  Vector.foreach
	  (args, fn arg =>
	   case ! (varInfo arg) of
	      VarInfo.NonObject =>
		 let
		    val v = varValue arg
		 in
		    if isSome (Value.deFlat {inner = v, outer = obj})
		       then Value.dontFlatten v
		    else ()
		 end
	    | VarInfo.Object _ => ()))
      val () =
	 Control.diagnostics
	 (fn display =>
	  let
	     open Layout
	     val () =
		Vector.foreach
		(datatypes, fn Datatype.T {cons, ...} =>
		 Vector.foreach
		 (cons, fn {con, ...} =>
		  display
		  (seq [Con.layout con, str " ",
			Option.layout Value.layout (! (conValue con))])))
	     val () =
		Program.foreachVar
		(program, fn (x, _) =>
		 let
		    val vi =
		       case ! (varInfo x) of
			  VarInfo.NonObject => str "NonObject"
			| VarInfo.Object {flat, ...} =>
			     seq [str "Object ", Flat.layout (!flat)]
		 in
		    display
		    (seq [Var.layout x, str " ",
			  record [("value", Value.layout (varValue x)),
				  ("varInfo", vi)]])
		 end)
	  in
	     ()
	  end)
      (* Conversion from values to types. *)
      datatype z = datatype Finish.t
      val traceValueType = Trace.trace ("valueType", Value.layout, Type.layout)
      fun valueType arg: Type.t =
	 traceValueType
	 (fn (v: Value.t) =>
	 let
	    datatype z = datatype Value.t
	 in
	    case v of
	       Ground t => t
	     | Object z => objectType z
	     | Weak {arg, finalType} =>
		  Ref.memoize (finalType, fn () => Type.weak (valueType arg))
	 end) arg
      and objectFinalComponents (obj as Obj {args, finalComponents, ...}) =
	 Ref.memoize
	 (finalComponents, fn () =>
	  Prod.make
	  (Vector.fromList
	   (Vector.foldr
	    (Prod.dest args, [], fn ({elt, isMutable = i}, ac) =>
	     case Value.deFlat {inner = elt, outer = obj} of
		NONE => {elt = valueType elt, isMutable = i} :: ac
	      | SOME z => 
		   Vector.foldr
		   (Prod.dest (objectFinalComponents z), ac,
		    fn ({elt, isMutable = i'}, ac) =>
		    {elt = elt, isMutable = i orelse i'} :: ac)))))
      and objectFinalOffsets (z as Obj {args, finalOffsets, flat, ...}) =
	 Ref.memoize
	 (finalOffsets, fn () =>
	  let
	     val initial =
		case ! flat of
		   Flat.Offset {object, offset} => objectOffset (object, offset)
		 | _ => 0
	     val (_, offsets) =
		Vector.fold
		(Prod.dest args, (initial, []), fn ({elt, ...}, (offset, ac)) =>
		 let
		    val width =
		       case Value.deFlat {inner = elt, outer = z} of
			  NONE => 1
			| SOME z => Prod.length (objectFinalComponents z)
		 in
		    (offset + width, offset :: ac)
		 end)
	  in
	     Vector.fromListRev offsets
	  end)
      and objectOffset (z: Object.t, offset: int): int =
	 Vector.sub (objectFinalOffsets z, offset)
      and objectType (z as Obj {con, finalType, flat, ...}): Type.t =
	 Ref.memoize
	 (finalType, fn () =>
	  case ! flat of
	     Flat.Offset {object, ...} => objectType object
	   | _ => Type.object {args = objectFinalComponents z,
			       con = con})
      (* Transform the program. *)
      fun transformFormals (xts: (Var.t * Type.t) vector)
	 : (Var.t * Type.t) vector =
	 Vector.map (xts, fn (x, _) => (x, valueType (varValue x)))
      val extraSelects: Statement.t list ref = ref []
      fun flattenValues (object: Var.t,
			 obj as Obj {args, ...},
			 ac: Var.t list): Var.t list =
	 Vector.foldri
	 (Prod.dest args, ac, fn (i, {elt, ...}, ac) =>
	  case Value.deFlat {inner = elt, outer = obj} of
	     NONE => 
		let
		   val var = Var.newNoname ()
		   val () =
		      List.push
		      (extraSelects,
		       Bind
		       {exp = Select {base = Base.Object object,
				      offset = objectOffset (obj, i)},
			ty = valueType elt,
			var = SOME var})
		in
		   var :: ac
		end
	   | SOME obj => flattenValues (object, obj, ac))
      fun flattenArgs (xs: Var.t vector, outer: Object.t, ac): Var.t list =
	 Vector.foldr
	 (xs, ac, fn (x, ac) =>
	  let
	     val v = varValue x
	  in
	     case Value.deFlat {inner = v, outer = outer} of
		NONE => x :: ac
	      | SOME obj =>
		   (case ! (varInfo x) of
		       VarInfo.NonObject => flattenValues (x, obj, ac)
		     | VarInfo.Object {components, ...} =>
			  flattenArgs (components, obj, ac))
	  end)
      val flattenArgs =
	 Trace.trace3 ("flattenArgs",
		       Vector.layout Var.layout,
		       Object.layout,
		       List.layout Var.layout,
		       List.layout Var.layout)
	 flattenArgs
      fun transformBind {exp, ty, var}: Statement.t vector =
	 let
	    fun make e =
	       Vector.new1
	       (Bind {exp = e,
		      ty = (case var of
			       NONE => ty
			     | SOME var => valueType (varValue var)),
		      var = var})
	    fun none () = Vector.new0 ()
	 in
	    case exp of
	       Exp.Object {args, con} =>
		  (case var of
		      NONE => none ()
		    | SOME var =>
			 (case varObject var of
			     NONE => make exp
			   | SOME (z as Obj {flat, ...}) =>
				case ! flat of
				   Flat.Offset _ => none ()
				 | _ =>
				      let
					 val args =
					    Vector.fromList
					    (flattenArgs (args, z, []))
					 val extra = !extraSelects
					 val () = extraSelects := []
				      in
					 Vector.concat
					 [Vector.fromList extra,
					  make (Exp.Object
						{args = args, con = con})]
				      end))
	     | PrimApp {args, prim} =>
		  make (PrimApp {args = args, prim = prim})
	     | Select {base, offset} =>
		  (case var of
		      NONE => none ()
		    | SOME var =>
			 (case base of
			     Base.Object object =>
				(case varObject object of
				    NONE => make exp
				  | SOME obj => 
				       make
				       (if isSome (Value.deFlat
						   {inner = varValue var,
						    outer = obj})
					   then Var object
					else (Select
					      {base = base,
					       offset = (objectOffset
							 (obj, offset))})))
			   | Base.VectorSub _ => make exp))
	     | _ => make exp
	 end
      fun transformStatement (s: Statement.t): Statement.t vector =
	 case s of
	    Bind b => transformBind b
	  | Profile _ => Vector.new1 s
	  | Updates (base, us) =>
	       (case base of
		   Base.Object object =>
		      Vector.new1
		      (case varObject object of
			  NONE => s
			| SOME obj =>
			     Updates
			     (base,
			      Vector.map (us, fn {offset, value} =>
					  {offset = objectOffset (obj, offset),
					   value = value})))
		 | Base.VectorSub _ => Vector.new1 s)
      val transformStatement =
	 Trace.trace ("transformStatement",
		      Statement.layout,
		      Vector.layout Statement.layout)
	 transformStatement
      fun transformStatements ss =
	 Vector.concatV (Vector.map (ss, transformStatement))
      fun transformBlock (Block.T {args, label, statements, transfer}) =
	    Block.T {args = transformFormals args,
		     label = label,
		     statements = transformStatements statements,
		     transfer = transfer}
      val datatypes =
	 Vector.map
	 (datatypes, fn Datatype.T {cons, tycon} =>
	  let
	     val cons =
		Vector.map
		(cons, fn {con, args} =>
		 let
		    val args =
		       case ! (conValue con) of
			  NONE => args
			| SOME v => 
			     case Type.dest (valueType v) of
				Type.Object {args, ...} => args
			      | _ => Error.bug "strange con"
		 in
		    {args = args, con = con}
		 end)
	  in
	     Datatype.T {cons = cons, tycon = tycon}
	  end)
      fun transformFunction (f: Function.t): Function.t =
	  let
	     val {args, blocks, mayInline, name, raises, returns, start, ...} =
		Function.dest f
	     fun rr tso =
		Option.map (tso, fn ts => Vector.map (ts, valueType o typeValue))
	     val raises = rr raises
	     val returns = rr returns
	  in
	     Function.new {args = transformFormals args,
			   blocks = Vector.map (blocks, transformBlock),
			   mayInline = mayInline,
			   name = name,
			   raises = raises,
			   returns = returns,
			   start = start}
	  end
      val program =
	 Program.T {datatypes = datatypes,
		    functions = List.revMap (functions, transformFunction),
		    globals = transformStatements globals,
		    main = main}
      val () = Program.clear program
   in
      shrink program
   end

end

