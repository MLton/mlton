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
      datatype t =
	 Flat of {elt: Type.t, isMutable: bool} vector
	| NotFlat of Type.t

      val layout: t -> Layout.t =
	 fn f =>
	 let
	    open Layout
	 in
	    case f of
	       Flat es =>
		  seq [str "Flat ",
		       Vector.layout
		       (fn {elt, isMutable} =>
			record [("elt", Type.layout elt),
				("isMutable", Bool.layout isMutable)])
		       es]
	     | NotFlat t => seq [str "NotFlat ", Type.layout t]
	 end
   end

structure Value =
   struct
      datatype t =
	 Ground of Type.t
       | Object of {args: {elt: t, isMutable: bool} vector,
		    con: Con.t option,
		    finalOffsets: int vector option ref,
		    finish: Finish.t option ref,
		    flat: flat Set.t}
       | Unary of {arg: t,
		   finalType: Type.t option ref,
		   unary: Unary.t}
      and flat =
	 NotFlat
	| Offset of {object: t,
		     offset: int}
	| Unknown

      local
	 open Layout
      in
	 fun layout v: Layout.t =
	    case v of
	       Ground t => Type.layout t
	     | Object {args, con, finish, flat, ...} =>
		  seq [str "Object ",
		       record
		       [("args", Vector.layout (layout o #elt) args),
			("con", Option.layout Con.layout con),
			("finish", Option.layout Finish.layout (!finish)),
			("flat", layoutFlat (Set.value flat))]]
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
      end

      local
	 fun unary u a = Unary {arg = a, finalType = ref NONE, unary = u}
	 datatype z = datatype Unary.t
      in
	 val array = unary Array
	 val vector = unary Vector
	 val weak = unary Weak
      end
	 
      local
	 val deUnary: t -> t =
	    fn v =>
	    case v of
	       Unary {arg, ...} => arg
	     | _ => Error.bug "deUnary"
      in
	 val deArray = deUnary
	 val deVector = deUnary
	 val deWeak = deUnary
      end

      fun deFlat (v: t): {elt: t, isMutable: bool} vector option =
	 case v of
	    Object {args, flat, ...} =>
	       (case Set.value flat of
		   Offset _ => SOME args
		 | _ => NONE)
	  | _ => NONE
	       
      val ground: Type.t -> t = Ground

      fun object (con, args) =
	 Object {args = args,
		 con = con,
		 finalOffsets = ref NONE,
		 finish = ref NONE,
		 flat = Set.singleton Unknown}

      val tuple: {elt: t, isMutable: bool} vector -> t =
	 fn vs => object (NONE, vs)

      val sameObject: t * t -> bool =
	 fn (v, v') =>
	 case (v, v') of
	    (Object {flat = f, ...}, Object {flat = f', ...}) =>
	       Set.equals (f, f')
	  | _ => false

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

      fun coerce {from: t, to: t}: unit =
	 case (from, to) of
	    (Ground t, Ground t') =>
	       if Type.equals (t, t')
		  then ()
	       else Error.bug "coerce Ground"
	  | (Object {con = SOME _, ...}, Ground _) => ()
	  | (Object {args = a, flat = f, ...},
	     Object {args = a', flat = f', ...}) =>
	       (Set.union (f, f')
		; (Vector.foreach2
		   (a, a', fn ({elt = e, ...}, {elt = e', ...}) =>
		    coerce {from = e, to = e'})))
	  | (Unary {arg = a, unary = u, ...},
	     Unary {arg = a', unary = u', ...}) =>
	       if Unary.equals (u, u')
		  then coerce {from = a, to = a'}
	       else Error.bug "coerce Unary"
	  | _ => Error.bug "strange coerce"

      val coerce =
	 Trace.trace ("Value.coerce",
		      fn {from, to} =>
		      Layout.record [("from", layout from),
				     ("to", layout to)],
		      Unit.layout)
	 coerce
   end

structure Flat =
   struct
      datatype t = datatype Value.flat

      val layout = Value.layoutFlat
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
	      val () =
		 case value of
		    Value.Object {flat, ...} => Set.setValue (flat, Flat.NotFlat)
		  | _ => Error.bug "constructor not object"
	   in
	      ()
	   end))
      fun typeValue t =
	 case Type.dest t of
	    Type.Object {con = SOME c, ...} => conValue c
	  | _ => Value.fromType t
      val coerce = Value.coerce
      fun object {args, con, resultType} =
	 let
	    val object =
	       case con of
		  NONE => Value.fromType resultType
		| SOME c => conValue c
	    val () =
	       case object of
		  Value.Object {args = args', ...} =>
		     Vector.foreach2
		     (args, args', fn (a, {elt = a', ...}) =>
		      coerce {from = a, to = a'})
		| _ => Error.bug "strange object"
	 in
	    object
	 end
      fun primApp {args, prim, resultVar, resultType, targs = _} =
	 let
	    fun arg i = Vector.sub (args, i)
	    fun result () = typeValue resultType
	    datatype z = datatype Prim.Name.t
	 in
	    case Prim.name prim of
	       Array_sub => Value.deArray (arg 0)
	     | Array_toVector => Value.vector (Value.deArray (arg 0))
	     | Array_update =>
		  (coerce {from = arg 2,
			   to = Value.deArray (arg 0)}
		   ; result ())
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
		  layout = Value.layout,
		  object = object,
		  primApp = primApp,
		  program = program,
		  select = fn {object, offset, ...} => select {object = object,
							       offset = offset},
		  update = update,
		  useFromTypeOnBinds = false}
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
      fun loopStatements ss =
	 Vector.foreach
	 (ss, fn Statement.T {exp, ty, var} =>
	  let
	     datatype z = datatype Exp.t
	  in
	     case exp of
		Const _ => ()
	      | Object {args, con, ...} =>
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
			     val object = varValue var
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
	      | PrimApp {args, ...} => uses args
	      | Profile _ => ()
	      | Select {object, ...} => use object
	      | Update {object, value, ...} => (use object; use value)
	      | Var x => use x
	  end)
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
					 andalso Value.sameObject (obj, obj')
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
      fun valueType (v: Value.t): Type.t =
	 case valueFinish v of
	    Flat _ =>
	       (case v of
		   Value.Object {flat, ...} =>
		      (case Set.value flat of
			  Flat.Offset {object, ...} => valueType object
			| _ => Error.bug "valueType flat")
		 | _ => Error.bug "valueType object")
	  | NotFlat t => t
      and valueFinish (v: Value.t) =
	 let
	    datatype z = datatype Value.t
	 in
	    case v of
	       Ground t => NotFlat t
	     | Object z => objectFinish z
	     | Unary {arg, finalType, unary} =>
		  NotFlat
		  (memoize
		   (finalType, fn () =>
		    case valueFinish arg of
		       NotFlat arg =>
			  let
			     datatype z = datatype Unary.t
			  in
			     case unary of
				Array => Type.array arg
			      | Vector => Type.vector arg
			      | Weak => Type.weak arg
			  end
		     | _ => Error.bug "finishValue Unary"))
	 end
      and valueOffset (object: Value.t, offset: int): int =
	 case object of
	    Value.Object z => Vector.sub (objectFinalOffsets z, offset)
	  | _ => Error.bug "valueOffset of non Object"
      and objectFinalOffsets (z as {args, finalOffsets, flat, ...}) =
	 memoize
	 (finalOffsets, fn () =>
	  let
	     val initial =
		case Set.value flat of
		   Flat.Offset {object, offset} => valueOffset (object, offset)
		 | _ => 0
	     val (_, offsets) =
		Vector.fold
		(args, (initial, []), fn ({elt, ...}, (offset, ac)) =>
		 (offset + (case valueFinish elt of
			       Flat es => Vector.length es
			     | NotFlat _ => 1),
		  offset :: ac))
	  in
	     Vector.fromListRev offsets
	  end)
      and objectFinish {args, con, finish, flat, ...}: Finish.t =
	 memoize
	 (finish, fn () =>
	  let
	     val isFlat =
		case Set.value flat of
		   Flat.Offset _ => true
		 | _ => false
	     val tss =
		Vector.fold
		(args, [], fn ({elt, isMutable = i}, ac) =>
		 let
		    val ts =
		       case valueFinish elt of
			  Flat vs =>
			     Vector.map (vs, fn {elt, isMutable = i'} =>
					 {elt = elt,
					  isMutable = i orelse i'})
			| NotFlat t => Vector.new1 {elt = t, isMutable = i}
		 in
		    ts :: ac
		 end)
	     val ts = Vector.concatV (Vector.fromListRev tss)
	  in
	     if isFlat
		then Flat ts
	     else
		NotFlat (Type.object {args = ts, con = con})
	  end)
      val valueType =
	 Trace.trace ("valueType", Value.layout, Type.layout) valueType
      val getOffset =
	 Trace.trace2 ("valueOffset", Value.layout, Int.layout, Int.layout)
	 valueOffset
      (* Transform the program. *)
      fun transformFormals (xts: (Var.t * Type.t) vector)
	 : (Var.t * Type.t) vector =
	 Vector.map (xts, fn (x, _) => (x, valueType (varValue x)))
      fun flattenArgs (xs: Var.t vector): Var.t vector =
	 let
	    fun loop (xs: Var.t vector, ac: Var.t list): Var.t list =
	       Vector.foldr
	       (xs, ac, fn (x, ac) =>
		case ! (varInfo x) of
		   NonTuple => x :: ac
		 | Tuple {components, flat} => 
		      case !flat of
			 Flat.Offset _ => loop (components, ac)
		       | _ => x :: ac)
	 in
	    Vector.fromList (loop (xs, []))
	 end
      val flattenArgs =
	 Trace.trace ("flattenArgs",
		      Vector.layout Var.layout,
		      Vector.layout Var.layout)
	 flattenArgs
      fun transformStatement (s as Statement.T {exp, ty, var})
	 : Statement.t option =
	 let
	    fun make e =
	       SOME
	       (Statement.T {exp = e,
			     ty = (case var of
				      NONE => ty
				    | SOME var => valueType (varValue var)),
			     var = var})
	 in
	    case exp of
	       Object {args, con} =>
		  let
		     fun simple () =
			make (Object {args = flattenArgs args,
				      con = con})
		  in
		     case var of
			NONE => NONE
		      | SOME var =>
			   case ! (varInfo var) of
			      NonTuple => simple ()
			    | Tuple {flat, ...} => 
				 case !flat of
				    Flat.Offset _ => NONE
				  | _ => simple ()
		  end
	     | Select {object, offset} =>
		  (case var of
		      NONE => NONE
		    | SOME var =>
			 make
			 (if isSome (Value.deFlat (varValue var))
			     then Var object
			  else Select {object = object,
				       offset = valueOffset (varValue object,
							     offset)}))
	     | Update {object, offset, value} =>
		  make (Update {object = object,
				offset = valueOffset (varValue object, offset),
				value = value})
	     | _ => make exp
	 end
      val transformStatement =
	 Trace.trace ("transformStatement",
		      Statement.layout,
		      Option.layout Statement.layout)
	 transformStatement
      fun transformStatements ss = Vector.keepAllMap (ss, transformStatement)
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

