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
datatype z = datatype Transfer.t

structure Set = DisjointSet

structure Unary =
   struct
      datatype t = Array | Vector | Weak

      val equals: t * t -> bool = op =
	 
      val toString =
	 fn Array => "Array"
	  | Vector => "Vector"
	  | Weak => "Weak"

      val layout = Layout.str o toString
   end

structure Finish =
   struct
      datatype t = T of {flat: {elt: Type.t, isMutable: bool} vector option, 
			 ty: Type.t}

      fun notFlat ty = T {flat = NONE, ty = ty}

      val layout: t -> Layout.t =
	 fn T {flat, ty} =>
	 let
	    open Layout
	 in
	    record [("flat",
		     Option.layout
		     (Vector.layout
		      (fn {elt, isMutable} =>
		       record [("elt", Type.layout elt),
			       ("isMutable", Bool.layout isMutable)]))
		     flat),
		    ("ty", Type.layout ty)]
	 end
   end

structure Value =
   struct
      datatype t =
	 Ground of Type.t
       | Object of object
       | Unary of {arg: t,
		   finalType: Type.t option ref,
		   unary: Unary.t}
      and flat =
	 NotFlat
	| Offset of {object: object,
		     offset: int}
	| Unknown
      withtype object =
	 {args: {elt: t, isMutable: bool} vector,
	  con: Con.t option,
	  finalComponents: {elt: Type.t,
			    isMutable: bool} vector option ref,
	  finalOffsets: int vector option ref,
	  finalType: Type.t option ref,
	  flat: flat Set.t}

      fun value v = v

      local
	 open Layout
      in
	 fun layout v: Layout.t =
	    case value v of
	       Ground t => Type.layout t
	     | Object ob => layoutObject ob
	     | Unary {arg, unary, ...} =>
		  seq [str "Unary ",
		       record [("arg", layout arg),
			       ("unary", Unary.layout unary)]]
	 and layoutFlat (f: flat): Layout.t =
	    case f of
	       NotFlat => str "NotFlat"
	     | Offset {offset, ...} =>
		  seq [str "Offset ",
		       record [("offset", Int.layout offset)]]
	     | Unknown => str "Unknown"
	 and layoutObject {args, con, finalType, flat, ...} =
	    seq [str "Object ",
		 record [("args", Vector.layout (layout o #elt) args),
			 ("con", Option.layout Con.layout con),
			 ("finalType", Option.layout Type.layout (!finalType)),
			 ("flat", layoutFlat (Set.value flat))]]
      end
   end

structure Flat =
   struct
      datatype t = datatype Value.flat

      val layout = Value.layoutFlat
   end

structure Object =
   struct
      type t = Value.object

      val layout = Value.layoutObject

      fun equals ({flat = f, ...}: t, {flat = f', ...}: t) = Set.equals (f, f')
   end

structure Value =
   struct
      open Value

      fun new v = v
	 
      local
	 fun unary u a = new (Unary {arg = a, finalType = ref NONE, unary = u})
	 datatype z = datatype Unary.t
      in
	 val array = unary Array
	 val vector = unary Vector
	 val weak = unary Weak
      end
	 
      local
	 val deUnary: t -> t =
	    fn v =>
	    case value v of
	       Unary {arg, ...} => arg
	     | _ => Error.bug "deUnary"
      in
	 val deArray = deUnary
	 val deVector = deUnary
	 val deWeak = deUnary
      end

      val deObject: t -> Object.t =
	 fn v =>
	 case value v of
	    Object ob => ob
	  | _ => Error.bug "Value.deObject"

      fun deFlat {inner: t, outer: Object.t}: Object.t option =
	 case value inner of
	    Object (z as {flat, ...}) =>
	       (case Set.value flat of
		   Flat.Offset {object, ...} =>
		      if Object.equals (object, outer) then SOME z else NONE
		 | _ => NONE)
	  | _ => NONE
	       
      fun dontFlatten (v: t): unit =
	 case value v of
	    Object {flat, ...} => Set.setValue (flat, NotFlat)
	  | _ => ()

      val ground: Type.t -> t = new o Ground

      fun object (con, args) =
	 new (Object {args = args,
		      con = con,
		      finalComponents = ref NONE,
		      finalOffsets = ref NONE,
		      finalType = ref NONE,
		      flat = Set.singleton Unknown})
	     
      val tuple: {elt: t, isMutable: bool} vector -> t =
	 fn vs => object (NONE, vs)

      val tuple =
	 Trace.trace ("Value.tuple",
		      Vector.layout
		      (fn {elt, isMutable} =>
		       Layout.record [("elt", layout elt),
				      ("isMutable", Bool.layout isMutable)]),
		      layout)
	 tuple

      val unit = tuple (Vector.new0 ())

      fun fromType (t: Type.t) =
	 let
	    datatype z = datatype Type.dest
	 in
	    case Type.dest t of
	       Array t => array (fromType t)
	     | Object {args, con} =>
		  (case con of
		      NONE => tuple (Vector.map (args, fn {elt, isMutable} =>
						 {elt = fromType elt,
						  isMutable = isMutable}))
		    | SOME _ => Error.bug "Value.fromType Object SOME con")
	     | Vector t => vector (fromType t)
	     | Weak t => weak (fromType t)
	     | _ => ground t
	 end

      val fromType = Trace.trace ("Value.fromType", Type.layout, layout) fromType

      val rec unify: t * t -> unit =
	 fn (v, v') =>
	 case (v, v') of
	    (Ground t, Ground t') => ()
	  | (Object {args = a, flat = f, ...},
	     Object {args = a', flat = f', ...}) =>
	       (Set.union (f, f')
		; (Vector.foreach2
		   (a, a', fn ({elt = e, ...}, {elt = e', ...}) =>
		    unify (e, e'))))
	  | (Unary {arg = a, ...}, Unary {arg = a', ...}) => unify (a, a')
	  | _ => Error.bug "strange unify"

      fun coerce {from, to} = unify (from, to)

      val coerce =
	 Trace.trace ("Value.coerce",
		      fn {from, to} =>
		      Layout.record [("from", layout from),
				     ("to", layout to)],
		      Unit.layout)
	 coerce
   end

fun flatten (program as Program.T {datatypes, functions, globals, main}) =
   let
      val {get = conValue: Con.t -> Value.t, set = setConValue, ...} =
	 Property.getSetOnce 
	 (Con.plist, Property.initRaise ("conInfo", Con.layout))
      val () =
	 Vector.foreach
	 (datatypes, fn Datatype.T {cons, tycon} =>
	  Vector.foreach
	  (cons, fn {args, con} =>
	   let
	      val value =
		 Value.object (SOME con,
			       Vector.map (args, fn {elt, isMutable} =>
					   {elt = Value.fromType elt,
					    isMutable = isMutable}))
	      val () = setConValue (con, value)
	      (* Constructors can never be flattened into other objects. *)
	      val () = Value.dontFlatten value
	   in
	      ()
	   end))
      fun typeValue t =
	 case Type.dest t of
	    Type.Object {con = SOME c, ...} => conValue c
	  | _ => Value.fromType t
      val coerce = Value.coerce
      fun inject {sum, variant} = Value.ground (Type.datatypee sum)
      fun object {args, con, resultType} =
	 case con of
	    NONE => Value.tuple args
	  | SOME c => 
	       let
		  val object = conValue c
		  val {args = args', ...} = Value.deObject object
		  val () =
		     Vector.foreach2
		     (args, args', fn ({elt = a, ...}, {elt = a', ...}) =>
		      coerce {from = a, to = a'})
	       in
		  object
	       end
      val object =
	 Trace.trace
	 ("object",
	  fn {args, con, ...} =>
	  Layout.record [("args",
			  Vector.layout
			  (fn {elt, isMutable} =>
			   Layout.record [("elt", Value.layout elt),
					  ("isMutable", Bool.layout isMutable)])
			  args),
			 ("con", Option.layout Con.layout con)],
	  Value.layout)
	 object
      fun primApp {args, prim, resultVar, resultType, targs} =
	 let
	    fun arg i = Vector.sub (args, i)
	    fun result () = typeValue resultType
	    datatype z = datatype Prim.Name.t
	    fun equal () = (Value.unify (arg 0, arg 1)
			    ; result ())
	    fun dontFlatten () =
	       (Vector.foreach (args, Value.dontFlatten)
		; result ())
	 in
	    case Prim.name prim of
	       Array_sub => Value.deArray (arg 0)
	     | Array_toVector => Value.vector (Value.deArray (arg 0))
	     | Array_update =>
		  (coerce {from = arg 2,
			   to = Value.deArray (arg 0)}
		   ; result ())
	     | FFI _ =>
		  (* Some imports, like Real64.modf, take ref cells that can not
		   * be flattened.
		   *)
		  dontFlatten ()
	     | MLton_eq => equal ()
	     | MLton_equal => equal ()
	     | MLton_size => dontFlatten ()
	     | Vector_sub => Value.deVector (arg 0)
	     | Weak_get => Value.deWeak (arg 0)
	     | Weak_new => Value.weak (arg 0)
	     | _ => result ()
	 end
      fun select {object, offset} =
	 let
	    datatype z = datatype Value.t
	    val args =
	       case object of
		  Object {args, ...} => args
		| _ => Error.bug "select"
	 in
	    #elt (Vector.sub (args, offset))
	 end
      fun update {object, offset, value} =
	 coerce {from = value,
		 to = select {object = object, offset = offset}}
      fun const c = Value.fromType (Type.ofConst c)
      val {func, label, value = varValue, ...} =
	 analyze {coerce = coerce,
		  const = const,
		  filter = fn _ => (),
		  filterWord = fn _ => (),
		  fromType = typeValue,
		  inject = inject,
		  layout = Value.layout,
		  object = object,
		  primApp = primApp,
		  program = program,
		  select = fn {object, offset, ...} => select {object = object,
							       offset = offset},
		  update = update,
		  useFromTypeOnBinds = false}
      val varObject = Value.deObject o varValue
      (* Mark a variable as flat if it is used only once and that use is in an
       * object allocation.
       *)
      datatype varInfo =
	 NonTuple
	| Tuple of {components: Var.t vector,
		    flat: Flat.t ref}
      val {get = varInfo: Var.t -> varInfo ref, ...} =
	 Property.get (Var.plist, Property.initFun (fn _ => ref NonTuple))
      fun use x =
	 case ! (varInfo x) of
	    Tuple {flat, ...} => flat := Flat.NotFlat
	  | _ => ()
      fun uses xs = Vector.foreach (xs, use)
      fun loopStatement (Statement.T {exp, ty, var}) =
	 let
	    datatype z = datatype Exp.t
	 in
	    case exp of
	       Object {args, con, ...} =>
		  (case var of
		      NONE => uses args
		    | SOME var => 
			 let
			    val () =
			       case con of
				  NONE =>
				     varInfo var
				     := Tuple {components = args,
					       flat = ref Flat.Unknown}
				| _ => ()
			    val object = Value.deObject (varValue var)
			 in
			    Vector.foreachi
			    (args, fn (offset, x) =>
			     case ! (varInfo x) of
				NonTuple => ()
			      | Tuple {components, flat} => 
				   let
				      datatype z = datatype Flat.t
				   in
				      case !flat of
					 Unknown =>
					    flat := Offset {object = object,
							    offset = offset}
				       | _ => flat := NotFlat
				   end)
			 end)
	     | _ => Exp.foreachVar (exp, use)
	 end
      fun loopStatements ss = Vector.foreach (ss, loopStatement)
      fun loopTransfer t = Transfer.foreachVar (t, use)
      val {get = labelInfo: Label.t -> {args: (Var.t * Type.t) vector},
	   set = setLabelInfo, ...} =
	 Property.getSetOnce (Label.plist,
			      Property.initRaise ("info", Label.layout))
      val labelArgs = #args o labelInfo
      val () = loopStatements globals
      val () =
	 List.foreach
	 (functions, fn f =>
	  let
	     val {args, blocks, ...} = Function.dest f
	  in
	     Vector.foreach
	     (blocks, fn Block.T {args, label, statements, transfer, ...} =>
	      (setLabelInfo (label, {args = args})
	       ; loopStatements statements
	       ; loopTransfer transfer))
	  end)
      (* Walk over the whole program and try to flatten each ref. *)
      fun loopStatement (Statement.T {exp, var, ...}) =
	 case exp of
	    Object {args, con = NONE} =>
	       Option.app
	       (var, fn var =>
		case varValue var of
		   Value.Object {flat, ...} =>
		      let
			 datatype z = datatype Flat.t
			 val flat'Ref as ref flat' =
			    case ! (varInfo var) of
			       NonTuple => Error.bug "tuple with NonTuple"
			     | Tuple {flat, ...} => flat
			 fun notFlat () =
			    (Set.setValue (flat, NotFlat)
			     ; flat'Ref := NotFlat)
		      in
			 case flat' of
			    Offset {object = obj, offset = i} =>
			       (case Set.value flat of
				   NotFlat => notFlat ()
				 | Offset {object = obj', offset = i'} =>
				      if i = i'
					 andalso Object.equals (obj, obj')
					 then ()
				      else notFlat ()
				 | Unknown => Set.setValue (flat, flat'))
			  | _ => notFlat ()
		      end
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
		  display (Value.layout (conValue con))))
	     val () =
		Program.foreachVar
		(program, fn (x, _) =>
		 let
		    val vi =
		       case ! (varInfo x) of
			  NonTuple => str "NonTuple"
			| Tuple {flat, ...} =>
			     seq [str "Tuple ", Flat.layout (!flat)]
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
      fun memoize (r: 'a option ref, f: unit -> 'a): 'a =
	 case !r of
	    NONE =>
	       let
		  val a = f ()
		  val () = r := SOME a
	       in
		  a
	       end
	  | SOME a => a
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
	     | Unary {arg, finalType, unary} =>
		  memoize
		  (finalType, fn () =>
		   let
		      val arg = valueType arg
		      datatype z = datatype Unary.t
		   in
		      case unary of
			 Array => Type.array arg
		       | Vector => Type.vector arg
		       | Weak => Type.weak arg
		   end)
	 end) arg
      and objectFinalComponents (z as {args, finalComponents, ...}) =
	 memoize
	 (finalComponents, fn () =>
	  Vector.fromList
	  (Vector.foldr
	   (args, [], fn ({elt, isMutable = i}, ac) =>
	    case Value.deFlat {inner = elt, outer = z} of
	       NONE => {elt = valueType elt, isMutable = i} :: ac
	     | SOME z => 
		  Vector.foldr
		  (objectFinalComponents z, ac, fn ({elt, isMutable = i'}, ac) =>
		   {elt = elt, isMutable = i orelse i'} :: ac))))
      and objectFinalOffsets (z as {args, finalOffsets, flat, ...}: Object.t) =
	 memoize
	 (finalOffsets, fn () =>
	  let
	     val initial =
		case Set.value flat of
		   Flat.Offset {object, offset} => objectOffset (object, offset)
		 | _ => 0
	     val (_, offsets) =
		Vector.fold
		(args, (initial, []), fn ({elt, ...}, (offset, ac)) =>
		 let
		    val width =
		       case Value.deFlat {inner = elt, outer = z} of
			  NONE => 1
			| SOME z => Vector.length (objectFinalComponents z)
		 in
		    (offset + width, offset :: ac)
		 end)
	  in
	     Vector.fromListRev offsets
	  end)
      and objectOffset (z: Object.t, offset: int): int =
	 Vector.sub (objectFinalOffsets z, offset)
      and objectType (z as {con, finalType, flat, ...}: Object.t): Type.t =
	 memoize
	 (finalType, fn () =>
	  case Set.value flat of
	     Flat.Offset {object, ...} => objectType object
	   | _ => Type.object {args = objectFinalComponents z,
			       con = con})
      and valueOffset (v: Value.t, offset: int): int =
	 objectOffset (Value.deObject v, offset)
      val valueOffset =
	 Trace.trace2 ("valueOffset", Value.layout, Int.layout, Int.layout)
	 valueOffset
      (* Transform the program. *)
      fun transformFormals (xts: (Var.t * Type.t) vector)
	 : (Var.t * Type.t) vector =
	 Vector.map (xts, fn (x, _) => (x, valueType (varValue x)))
      val extraSelects: Statement.t list ref = ref []
      fun flattenValues (object: Var.t,
			 obj as {args, ...}: Object.t,
			 ac: Var.t list): Var.t list =
	 Vector.foldri
	 (args, ac, fn (i, {elt, ...}, ac) =>
	  case Value.deFlat {inner = elt, outer = obj} of
	     NONE => 
		let
		   val var = Var.newNoname ()
		   val () =
		      List.push
		      (extraSelects,
		       Statement.T
		       {exp = Select {object = object,
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
		       NonTuple => flattenValues (x, obj, ac)
		     | Tuple {components, ...} =>
			  flattenArgs (components, obj, ac))
	  end)
      val flattenArgs =
	 Trace.trace3 ("flattenArgs",
		       Vector.layout Var.layout,
		       Object.layout,
		       List.layout Var.layout,
		       List.layout Var.layout)
	 flattenArgs
      fun transformStatement (s as Statement.T {exp, ty, var})
	 : Statement.t vector =
	 let
	    fun make e =
	       Vector.new1
	       (Statement.T {exp = e,
			     ty = (case var of
				      NONE => ty
				    | SOME var => valueType (varValue var)),
			     var = var})
	    fun none () = Vector.new0 ()
	 in
	    case exp of
	       Object {args, con} =>
		  (case var of
		      NONE => none ()
		    | SOME var =>
			 let
			    val z as {flat, ...} = varObject var
			 in
			    case Set.value flat of
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
				      make (Object {args = args, con = con})]
				  end
			 end)
	      | PrimApp {args, prim, targs} =>
		   let
		      val vargs = Vector.map (args, varValue)
		      fun v i = valueType (Vector.sub (vargs, i))
		      fun equal () = Vector.new1 (Type.join (v 0, v 1))
		      datatype z = datatype Prim.Name.t
		      val targs =
			 case Prim.name prim of
			    MLton_eq => equal ()
			  | MLton_equal => equal ()
			  | _ => 
			       Vector.map
			       (Prim.extractTargs
				(prim,
				 {args = vargs,
				  deArray = Value.deArray,
				  deArrow = fn _ => Error.bug "deArrow",
				  deRef = fn _ => Error.bug "deRef",
				  deVector = Value.deVector,
				  deWeak = Value.deWeak,
				  result = (case var of
					       NONE => Value.unit
					     | SOME x => varValue x)}),
				valueType)
		   in
		      make (PrimApp {args = args,
				     prim = prim,
				     targs = targs})
		   end
	     | Select {object, offset} =>
		  (case var of
		      NONE => none ()
		    | SOME var =>
			 let
			    val obj = varObject object
			 in
			    make
			    (if isSome (Value.deFlat {inner = varValue var,
						      outer = obj})
				then Var object
			     else Select {object = object,
					  offset = objectOffset (obj, offset)})
			 end)
	     | Update {object, offset, value} =>
		  make (Update {object = object,
				offset = valueOffset (varValue object, offset),
				value = value})
	     | _ => make exp
	 end
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
      fun valuesTypes vs = Vector.map (vs, valueType)
      val datatypes =
	 Vector.map
	 (datatypes, fn Datatype.T {cons, tycon} =>
	  Datatype.T
	  {cons = Vector.map (cons, fn {con, ...} =>
			      case Type.dest (valueType (conValue con)) of
				 Type.Object {args, ...} =>
				    {args = args, con = con}
			       | _ => Error.bug "strange con"),
	   tycon = tycon})
      fun transformFunction (f: Function.t): Function.t =
	  let
	     val {args, blocks, mayInline, name, start, ...} = Function.dest f
	     val {raises, returns, ...} = func name
	     val raises = Option.map (raises, valuesTypes)
	     val returns = Option.map (returns, valuesTypes)
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
      program
   end

end

