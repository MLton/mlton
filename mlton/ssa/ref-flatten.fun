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

      local
	 open Layout
      in
	 fun layout v: Layout.t =
	    case v of
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

      fun unary (u, a) = Unary {arg = a, finalType = ref NONE, unary = u}

      val deObject: t -> Object.t option =
	 fn Object ob => SOME ob
	  | _ => NONE

      fun deFlat {inner: t, outer: Object.t}: Object.t option =
	 case inner of
	    Object (z as {flat, ...}) =>
	       (case Set.value flat of
		   Flat.Offset {object, ...} =>
		      if Object.equals (object, outer) then SOME z else NONE
		 | _ => NONE)
	  | _ => NONE
	       
      fun dontFlatten (v: t): unit =
	 case v of
	    Object {flat, ...} => Set.setValue (flat, NotFlat)
	  | _ => ()

      val unit = Ground Type.unit

      fun isUnit v =
	 case v of
	    Ground t => Type.isUnit t
	  | _ => false
	       
      fun object {args, con} =
	 let
	    (* Only may flatten objects with mutable fields. *)
	    val flat =
	       Set.singleton
	       (if Vector.exists (args, fn {elt, isMutable} =>
				  isMutable andalso not (isUnit elt))
		   then Unknown
		else NotFlat)
	 in
	    Object {args = args,
		    con = con,
		    finalComponents = ref NONE,
		    finalOffsets = ref NONE,
		    finalType = ref NONE,
		    flat = flat}
	 end
	    
      val tuple: {elt: t, isMutable: bool} vector -> t =
	 fn vs => object {args = vs, con = NONE}

      val tuple =
	 Trace.trace ("Value.tuple",
		      Vector.layout
		      (fn {elt, isMutable} =>
		       Layout.record [("elt", layout elt),
				      ("isMutable", Bool.layout isMutable)]),
		      layout)
	 tuple

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

fun flatten (program as Program.T {datatypes, functions, globals, main}) =
   let
      val {get = conValue: Con.t -> Value.t option ref, ...} =
	 Property.get (Con.plist, Property.initFun (fn _ => ref NONE))
      val conValue =
	 Trace.trace ("conValue",
		      Con.layout, Ref.layout (Option.layout Value.layout))
	 conValue
      datatype 'a make =
	 Const of 'a
       | Make of unit -> 'a
      val {get = makeTypeValue: Type.t -> Value.t make, ...} =
	 Property.get
	 (Type.plist,
	  Property.initRec
	  (fn (t, makeTypeValue) =>
	   let
	      fun const () = Const (Value.Ground t)
	      fun unary (t, u) =
		 case makeTypeValue t of
		    Const _ => const ()
		  | Make f => Make (fn () => Value.unary (u, f ()))
	      datatype z = datatype Type.dest
	   in
	      case Type.dest t of
		 Array t => unary (t, Unary.Array)
	       | Object {args, con} =>
		    let
		       fun doit () =
			  let
			     val args =
				Vector.map
				(args, fn {elt, isMutable} =>
				 {elt = makeTypeValue elt,
				  isMutable = isMutable})
			     val mayFlatten = Vector.exists (args, #isMutable)
			     val needToMake =
				mayFlatten
				orelse
				Vector.exists (args, fn {elt, ...} =>
					       case elt of
						  Const _ => false
						| Make _ => true)
			     fun make () =
				Value.object
				{args = (Vector.map
					 (args, fn {elt, isMutable} =>
					  {elt = (case elt of
						     Const v => v
						   | Make f => f ()),
					   isMutable = isMutable})),
				 con = con}
			  in
			     if needToMake
				then Make make
			     else const ()
			  end
		    in
		       case con of
			  NONE => doit ()
			| SOME c =>
			     let
				val r = conValue c
			     in
				Const
				(memoize
				 (r, fn () =>
				  case doit () of
				     Const v => v
				   | Make f =>
					let
					   val v = f ()
					   (* Constructors can never be
					    * flattened into other objects.
					    *)
					   val () = Value.dontFlatten v
					in
					   v
					end))
			     end
		    end
	       | Vector t => unary (t, Unary.Vector)
	       | Weak t => unary (t, Unary.Weak)
	       | _ => const ()
	   end))
      fun typeValue (t: Type.t): Value.t =
	  case makeTypeValue t of
	     Const v => v
	   | Make f => f ()
      val typeValue =
	 Trace.trace ("typeValue", Type.layout, Value.layout) typeValue
      val coerce = Value.coerce
      fun inject {sum, variant} = typeValue (Type.datatypee sum)
      fun object {args, con, resultType} =
	 let
	    val m = makeTypeValue resultType
	 in
	    case con of
	       NONE =>
		  (case m of
		      Const v => v
		    | Make _ => Value.tuple args)
	  | SOME c =>
	       (case m of
		   Const v =>
		      let
			 val () =
			    case Value.deObject v of
			       NONE => ()
			     | SOME {args = args', ...} =>
				  Vector.foreach2
				  (args, args',
				   fn ({elt = a, ...}, {elt = a', ...}) =>
				   coerce {from = a, to = a'})
		      in
			 v
		      end
		 | _ => Error.bug "strange con value")
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
      local
	 val deUnary: Value.t -> Value.t =
	    fn v =>
	    case v of
	       Value.Ground t =>
		  typeValue (case Type.dest t of
				Type.Array t => t
			      | Type.Vector t => t
			      | Type.Weak t => t
			      | _ => Error.bug "deUnary")
	     | Value.Unary {arg, ...} => arg
	     | _ => Error.bug "deUnary"
      in
	 val deArray = deUnary
	 val deVector = deUnary
	 val deWeak = deUnary
      end
      fun primApp {args, prim, resultVar, resultType, targs} =
	 let
	    local
	       fun make u v =
		  case makeTypeValue resultType of
		     Const v => v
		   | Make _ => Value.unary (u, v)
	       datatype z = datatype Unary.t
	    in
	       val vector = make Vector
	       val weak = make Weak
	    end
	    fun arg i = Vector.sub (args, i)
	    fun result () = typeValue resultType
	    datatype z = datatype Prim.Name.t
	    fun dontFlatten () =
	       (Vector.foreach (args, Value.dontFlatten)
		; result ())
	    fun equal () =
	       (Value.unify (arg 0, arg 1)
		; result ())
	 in
	    case Prim.name prim of
	       Array_sub => deArray (arg 0)
	     | Array_toVector => vector (deArray (arg 0))
	     | Array_update =>
		  (coerce {from = arg 2,
			   to = deArray (arg 0)}
		   ; result ())
	     | FFI _ =>
		  (* Some imports, like Real64.modf, take ref cells that can not
		   * be flattened.
		   *)
		  dontFlatten ()
	     | MLton_eq => equal ()
	     | MLton_equal => equal ()
	     | MLton_size => dontFlatten ()
	     | Vector_sub => deVector (arg 0)
	     | Weak_get => deWeak (arg 0)
	     | Weak_new => weak (arg 0)
	     | _ => result ()
	 end
      fun select {object, offset} =
	 let
	    datatype z = datatype Value.t
	 in
	    case object of
		  Ground t =>
		     (case Type.dest t of
			 Type.Object {args, ...} =>
			    typeValue (#elt (Vector.sub (args, offset)))
		       | _ => Error.bug "select Ground")
		| Object {args, ...} => #elt (Vector.sub (args, offset))
		| _ => Error.bug "select"
	 end
      fun update {object, offset, value} =
	 coerce {from = value,
		 to = select {object = object, offset = offset}}
      fun const c = typeValue (Type.ofConst c)
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
			 case Value.deObject (varValue var) of
			    NONE => uses args
			  | SOME object =>
			       let
				  val () =
				     case con of
					NONE =>
					   varInfo var
					   := Tuple {components = args,
						     flat = ref Flat.Unknown}
				      | _ => ()
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
						  flat :=
						  Offset {object = object,
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
		   Value.Ground _ => ()
		 | Value.Object {flat, ...} =>
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
		  display (Option.layout Value.layout (! (conValue con)))))
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
			 (case varObject var of
			     NONE => make exp
			   | SOME (z as {flat, ...}) =>
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
				      end))
	      | PrimApp {args, prim, targs} =>
		   let
		      val vargs = Vector.map (args, varValue)
		      fun v i = valueType (Vector.sub (vargs, i))
		      datatype z = datatype Prim.Name.t
		      val targs =
			 Vector.map
			 (Prim.extractTargs
			  (prim,
			   {args = vargs,
			    deArray = deArray,
			    deArrow = fn _ => Error.bug "deArrow",
			    deRef = fn _ => Error.bug "deRef",
			    deVector = deVector,
			    deWeak = deWeak,
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
			 (case varObject object of
			     NONE => make exp
			   | SOME obj => 
				make
				(if isSome (Value.deFlat {inner = varValue var,
							  outer = obj})
				    then Var object
				 else (Select
				       {object = object,
					offset = objectOffset (obj, offset)}))))
	     | Update {object, offset, value} =>
		  (case varObject object of
		      NONE => make exp
		    | SOME obj => 
			 make (Update {object = object,
				       offset = objectOffset (obj, offset),
				       value = value}))
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

