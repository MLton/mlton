(* Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor DeepFlatten (S: DEEP_FLATTEN_STRUCTS): DEEP_FLATTEN = 
struct

open S

type int = Int.t

datatype z = datatype Exp.t
datatype z = datatype Statement.t
datatype z = datatype Transfer.t

structure Tree = Tree (structure Seq = Prod)

structure TypeTree =
   struct
      datatype t = datatype Tree.t
	 
      datatype info =
	 Flat
       | NotFlat of {ty: Type.t,
		     var: Var.t option}

      type t = info Tree.t

      fun layout (t: t): Layout.t =
	 Tree.layout
	 (t,
	  let
	     open Layout
	  in
	     fn Flat => str "Flat"
	      | NotFlat {ty, var} =>
		   seq [str "NotFlat ",
			record [("ty", Type.layout ty),
				("var", Option.layout Var.layout var)]]
	  end)

      val isFlat: t -> bool =
	 fn T (i, _) =>
	 case i of
	    Flat => true
	  | NotFlat _ => false
   end

structure VarTree =
   struct
      open TypeTree

      val labelRoot: t * Var.t -> t =
	 fn (t as T (info, ts), x) =>
	 case info of
	    Flat => t
	  | NotFlat {ty, ...} => T (NotFlat {ty = ty, var = SOME x}, ts)

      val fromTypeTree: TypeTree.t -> t = fn t => t

      val foldRoots: t * 'a * (Var.t * 'a -> 'a) -> 'a =
	 fn (t, a, f) =>
	 let
	    fun loop (T (info, children), a: 'a): 'a =
	       case info of
		  Flat => Prod.fold (children, a, loop)
		| NotFlat {var, ...} =>
		     case var of
			NONE => Error.bug "foldRoots"
		      | SOME x => f (x, a)
	 in
	    loop (t, a)
	 end

      fun foreachRoot (t, f) = foldRoots (t, (), f o #1)

      val rootsOnto: t * Var.t list -> Var.t list =
	 fn (t, ac) =>
	 List.appendRev (foldRoots (t, [], op ::), ac)

      val rec dropVars: t -> t =
	 fn T (info, ts) =>
	 let
	    val info =
	       case info of
		  Flat => Flat
		| NotFlat {ty, ...} => NotFlat {ty = ty, var = NONE}
	 in
	    T (info, Prod.map (ts, dropVars))
	 end
	 
      fun fillInRoots (t: t, {base: Var.t Base.t, offset: int})
	 : t * Statement.t list =
	 let
	    fun loop (t as T (info, ts), offset, ac) =
	       case info of
		  Flat =>
		     let
			val (ts, (offset, ac)) =
			   Vector.mapAndFold
			   (Prod.dest ts, (offset, ac),
			    fn ({elt = t, isMutable}, (offset, ac)) =>
			    let
			       val (t, offset, ac) = loop (t, offset, ac)
			    in
			       ({elt = t, isMutable = isMutable},
				(offset, ac))
			    end)
		     in
			(T (Flat, Prod.make ts), offset, ac)
		     end
		| NotFlat {ty, var} =>
		     let
			val (t, ac) =
			   case var of
			      NONE =>
				 let
				    val var = Var.newNoname ()
				 in
				    (T (NotFlat {ty = ty, var = SOME var}, ts),
				     Bind
				     {exp = Select {base = base,
						    offset = offset},
				      ty = ty,
				      var = SOME var} :: ac)
				 end
			    | SOME _ => (t, ac)
		     in
			(t, offset + 1, ac)
		     end
	    val (t, _, ac) = loop (t, offset, [])
	 in
	    (t, ac)
	 end

      val fillInRoots =
	 Trace.trace2 ("VarTree.fillInRoots",
		       layout,
		       fn {base, offset} =>
		       Layout.record [("base", Base.layout (base, Var.layout)),
				      ("offset", Int.layout offset)],
		       Layout.tuple2 (layout, List.layout Statement.layout))
	 fillInRoots
   end

fun flatten {base: Var.t Base.t option,
	     from: VarTree.t,
	     offset: int,
	     to: TypeTree.t}: {offset: int} * VarTree.t * Statement.t list =
   let
      val Tree.T (from, fs) = from
   in
      case from of
	 VarTree.Flat =>
	    if TypeTree.isFlat to
	       then flattensAt {base = base,
				froms = fs,
				offset = offset,
				tos = Tree.children to}
	    else Error.bug "cannot flatten from Flat to NotFlat"
       | VarTree.NotFlat {ty, var} =>
	    let
	       val (var, ss) =
		  case var of
		     NONE =>
			let
			   val base =
			      case base of
				 NONE => Error.bug "flatten missing base"
			       | SOME base => base
			   val result = Var.newNoname ()
			in
			   (result,
			    [Bind {exp = Select {base = base,
						 offset = offset},
				   ty = ty,
				   var = SOME result}])
			end
		   | SOME var => (var, [])
	       val (r, ss) =
		  if TypeTree.isFlat to
		     then
			let
			   val (_, r, ss') =
			      flattensAt {base = SOME (Base.Object var),
					  froms = fs,
					  offset = 0,
					  tos = Tree.children to}
			in
			   (r, ss @ ss')
			end
		  else (Tree.T (VarTree.NotFlat {ty = ty, var = SOME var},
				fs),
			ss)
	    in
	       ({offset = 1 + offset}, r, ss)
	    end
   end
and flattensAt {base: Var.t Base.t option,
		froms: VarTree.t Prod.t,
		offset: int,
		tos: TypeTree.t Prod.t} =
   let
      val (ts, (off, ss)) =
	 Vector.map2AndFold
	 (Prod.dest froms, Prod.dest tos, ({offset = offset}, []),
	  fn ({elt = f, isMutable}, {elt = t, ...}, ({offset}, ss)) =>
	  let
	     val () =
		if isMutable
		   then Error.bug "flattensAt mutable"
		else ()
	     val ({offset}, t, ss') =
		flatten {base = base,
			 from = f,
			 offset = offset,
			 to = t}
	  in
	     ({elt = t, isMutable = false},
	      ({offset = offset}, ss' @ ss))
	  end)
   in
      (off, Tree.T (VarTree.Flat, Prod.make ts), ss)
   end

fun coerceTree {from: VarTree.t, to: TypeTree.t}: VarTree.t * Statement.t list =
   let
      val (_, r, ss) =
	 flatten {base = NONE,
		  from = from,
		  offset = 0,
		  to = to}
   in
      (r, ss)
   end

val coerceTree =
   let
      open Layout
   in
      Trace.trace ("DeepFlatten.coerceTree",
		   fn {from, to} =>
		   record [("from", VarTree.layout from),
			   ("to", TypeTree.layout to)],
		   fn (vt, ss) =>
		   tuple [VarTree.layout vt,
			  List.layout Statement.layout ss])
      coerceTree
   end

structure Flat =
   struct
      datatype t = Flat | NotFlat

      val toString: t -> string =
	 fn Flat => "Flat"
	  | NotFlat => "NotFlat"

      val layout = Layout.str o toString
   end

datatype z = datatype Flat.t
   
structure Value =
   struct
      datatype t = T of {finalOffsets: int vector option ref,
			 finalTree: TypeTree.t option ref,
			 finalType: Type.t option ref,
			 finalTypes: Type.t Prod.t option ref,
			 value: value} Equatable.t
      and value =
	 Ground of Type.t
       | Object of {args: t Prod.t,
		    coercedFrom: t list ref,
		    con: ObjectCon.t,
		    flat: Flat.t ref}
       | Weak of {arg: t}

      fun new' v = {finalOffsets = ref NONE,
		    finalTree = ref NONE,
		    finalType = ref NONE,
		    finalTypes = ref NONE,
		    value = v}

      fun delay (f: unit -> value): t =
	 T (Equatable.delay (fn () => new' (f ())))
	 
      fun new (v: value) = T (Equatable.new (new' v))

      fun value (T e) = #value (Equatable.value e)

      fun layout v: Layout.t =
	 let
	    open Layout
	 in
	    case value v of
	       Ground t => Type.layout t
	     | Object {args, con, flat, ...} => 
		  seq [str "Object ",
		       record [("args", Prod.layout (args, layout)),
			       ("con", ObjectCon.layout con),
			       ("flat", Flat.layout (! flat))]]
	     | Weak {arg, ...} => seq [str "Weak ", layout arg]
	 end

      val ground = new o Ground

      fun weak (a: t): value = Weak {arg = a}
	       
      val traceCoerce =
	 Trace.trace ("Value.coerce",
		      fn {from, to} =>
		      Layout.record [("from", layout from),
				     ("to", layout to)],
		      Unit.layout)

      val traceUnify =
	 Trace.trace2 ("Value.unify", layout, layout, Unit.layout)

      val rec unify: t * t -> unit =
	 fn arg =>
	 traceUnify
	 (fn (v as T e, T e') =>
	  let
	     val callDont = ref false
	     val () =
		Equatable.equate
		(e, e', fn (z as {value = v, ...}, {value = v', ...}) =>
		 case (v, v') of
		    (Ground _, Ground _) => z
		  | (Object {args = a, coercedFrom = c, flat = f, ...}, Object {args = a', coercedFrom = c', flat = f', ...}) =>
		       let
			  val () = unifyProd (a, a')
		       in
			  case (!f, !f') of
			     (Flat, Flat) =>
				(c := List.fold (!c', !c, op ::); new' v)
			   | (Flat, NotFlat) =>
				(callDont := true; new' v)
			   | (NotFlat, Flat) =>
				(callDont := true; new' v')
			   | (NotFlat, NotFlat) => z
		       end
		  | (Weak {arg = a, ...}, Weak {arg = a', ...}) =>
		       (unify (a, a'); z)
		  | _ => Error.bug "strange unify")
	  in
	     if !callDont
		then dontFlatten v
	     else ()
	  end) arg
      and unifyProd =
	 fn (p, p') =>
	 Vector.foreach2
	 (Prod.dest p, Prod.dest p',
	  fn ({elt = e, ...}, {elt = e', ...}) => unify (e, e'))
      and coerce =
	 fn arg as {from as T e, to as T e'} =>
	 traceCoerce
	 (fn _ =>
	 if Equatable.equals (e, e')
	    then ()
	 else
	    let
	       val {value = v, ...} = Equatable.value e
	       val {value = v', ...} = Equatable.value e'
	    in
	       case (v, v') of
		  (Ground _, Ground _) => ()
		| (Object {args = a, con, ...},
		   Object {args = a', coercedFrom = c', flat = f', ...}) =>
		     (if Prod.isMutable a orelse ObjectCon.isVector con
			 then unify (from, to)
		      else
			 case !f' of
			    Flat => (List.push (c', from)
				     ; coerceProd {from = a, to = a'})
			  | NotFlat => unify (from, to))
		| (Weak _, Weak _) => unify (from, to)
		| _ => Error.bug "strange unify"
	    end) arg
      and coerceProd =
	 fn {from = p, to = p'} =>
	 Vector.foreach2
	 (Prod.dest p, Prod.dest p', fn ({elt = e, ...}, {elt = e', ...}) =>
	  coerce {from = e, to = e'})
      and dontFlatten: t -> unit =
	 fn v =>
	 case value v of
	    Object {coercedFrom, flat, ...} =>
	       (case ! flat of
		   Flat =>
		      let
			 val () = flat := NotFlat
			 val from = !coercedFrom
			 val () = coercedFrom := []
		      in
			 List.foreach (from, fn v' => unify (v, v'))
		      end
		 | NotFlat => ())
	  | _ => ()

      fun object {args, con} =
	 let
	    (* Don't flatten object components that are immutable fields.  Those
	     * have already had a chance to be flattened by other passes.
	     *)
	    val _  =
	       if (case con  of
		      ObjectCon.Con _ => true
		    | ObjectCon.Tuple => true
		    | ObjectCon.Vector => false)
		  then Vector.foreach (Prod.dest args, fn {elt, isMutable} =>
				       if isMutable
					  then ()
				       else dontFlatten elt)
	       else ()
	    (* Don't flatten constructors, since they are part of a sum type.
	     * Don't flatten unit.
	     * Don't flatten vectors (of course their components can be
	     * flattened).
	     * Don't flatten objects with mutable fields, since sharing must be
	     * preserved.
	     *)
	    val dontFlatten =
	       (case con of
		   ObjectCon.Con _ => true
		 | ObjectCon.Tuple => false
		 | ObjectCon.Vector => true)
	       orelse Prod.isEmpty args
	       orelse Prod.isMutable args
	    val flat = if dontFlatten then Flat.NotFlat else Flat.Flat
	 in
	    Object {args = args,
		    coercedFrom = ref [],
		    con = con,
		    flat = ref flat}
	 end
	    
      val tuple: t Prod.t -> t =
	 fn vs => new (object {args = vs, con = ObjectCon.Tuple})

      val tuple =
	 Trace.trace ("Value.tuple", fn p => Prod.layout (p, layout), layout)
	 tuple

      val deObject =
	 fn v =>
	 case value v of
	    Object z => z
	  | _ => Error.bug "Value.deObject"

      fun select {base: t, offset: int}: t =
	 Prod.elt (#args (deObject base), offset)

      val deWeak: t -> t =
	 fn v =>
	 case value v of
	    Weak {arg, ...} => arg
	  | _ => Error.bug "Value.deWeak"

      val traceFinalType = Trace.trace ("Value.finalType", layout, Type.layout)
      val traceFinalTypes =
	 Trace.trace ("Value.finalTypes", layout,
		      fn p => Prod.layout (p, Type.layout))

      fun finalTree (v as T e): TypeTree.t =
	 let
	    val {finalTree = r, value, ...} = Equatable.value e
	 in
	    Ref.memoize
	    (r, fn () =>
	     let
		fun notFlat () = TypeTree.NotFlat {ty = finalType v,
						   var = NONE}
	     in
		case value of
		   Object {args, flat, ...} =>
		      let
			 val info =
			    case !flat of
			       Flat => TypeTree.Flat
			     | NotFlat => notFlat ()
		      in
			 Tree.T (info, Prod.map (args, finalTree))
		      end
		 | _ => Tree.T (notFlat (), Prod.empty ())
	     end)
	 end
      and finalType arg: Type.t =
	 traceFinalType
	 (fn v as T e =>
	  let
	     val {finalType = r, value, ...} = Equatable.value e
	  in
	     Ref.memoize
	     (r, fn () =>
	      case value of
		 Ground t => t
	       | Object _ => Prod.elt (finalTypes v, 0)
	       | Weak {arg, ...} => Type.weak (finalType arg))
	  end) arg
      and finalTypes arg: Type.t Prod.t =
	 traceFinalTypes
	 (fn v as T e =>
	 let
	     val {finalTypes, value, ...} = Equatable.value e
	  in
	     Ref.memoize
	     (finalTypes, fn () =>
	      case value of
		 Object {args, con, flat, ...} =>
		    let
		       val args = prodFinalTypes args
		    in
		       case !flat of
			  Flat => args
			| NotFlat =>
			     Prod.make
			     (Vector.new1
			      {elt = Type.object {args = args, con = con},
			       isMutable = false})
		    end
	       | _ => Prod.make (Vector.new1 {elt = finalType v,
					      isMutable = false}))
	 end) arg
      and prodFinalTypes (p: t Prod.t): Type.t Prod.t =
	 Prod.make
	 (Vector.fromList
	  (Vector.foldr
	   (Prod.dest p, [], fn ({elt, isMutable = i}, ac) =>
	    Vector.foldr
	    (Prod.dest (finalTypes elt), ac, fn ({elt, isMutable = i'}, ac) =>
	     {elt = elt, isMutable = i orelse i'} :: ac))))

      fun finalOffsets (T e): int vector =
	 let
	    val {finalOffsets = r, value, ...} = Equatable.value e
	 in
	    Ref.memoize
	    (r, fn () =>
	     case value of
		Object {args, ...} =>
		   Vector.fromListRev
		   (#2 (Prod.fold
			(args, (0, []), fn (elt, (offset, offsets)) =>
			 (offset + Prod.length (finalTypes elt),
			  offset :: offsets))))
	      | _ => Error.bug "finalOffsets of non object")
	 end

      fun finalOffset (object, offset) =
	 Vector.sub (finalOffsets object, offset)
   end

fun flatten (program as Program.T {datatypes, functions, globals, main}) =
   let
      val {get = conValue: Con.t -> Value.t option ref, ...} =
	 Property.get (Con.plist, Property.initFun (fn _ => ref NONE))
      val conValue =
	 Trace.trace ("conValue",
		      Con.layout, Ref.layout (Option.layout Value.layout))
	 conValue
      val {get = makeTypeValue: Type.t -> unit -> Value.t, ...} =
	 Property.get
	 (Type.plist,
	  Property.initRec
	  (fn (t, makeTypeValue) =>
	   let
	      datatype z = datatype Type.dest
	   in
	      case Type.dest t of
		 Object {args, con} =>
		    let
		       val args = Prod.map (args, makeTypeValue)
		       fun doit () =
			  Value.delay
			  (fn () =>
			   Value.object {args = Prod.map (args, fn f => f ()),
					 con = con})
		       datatype z = datatype ObjectCon.t
		    in
		       case con of
			  Con c =>
			     let
				val v = conValue c
			     in
				fn () => Ref.memoize (v, doit)
			     end
			| Tuple => doit
			| Vector =>  doit
		    end
	       | Weak t =>
		    let
		       val t = makeTypeValue t
		    in
		       fn () => Value.delay (fn () => Value.weak (t ()))
		    end
	       | _ =>
		    let
		       val v = Value.ground t
		    in
		       fn () => v
		    end
	   end))
      fun typeValue (t: Type.t): Value.t = makeTypeValue t ()
      val typeValue =
	 Trace.trace ("typeValue", Type.layout, Value.layout) typeValue
      val coerce = Value.coerce
      fun inject {sum, variant = _} = typeValue (Type.datatypee sum)
      fun object {args, con, resultType} =
	 case con of
	    NONE => Value.tuple args
	  | SOME _ =>
	       let
		  val res = typeValue resultType
		  val () =
		     Value.coerceProd {from = args,
				       to = #args (Value.deObject res)}
	       in
		  res
	       end
      val object =
	 Trace.trace
	 ("object",
	  fn {args, con, ...} =>
	  Layout.record [("args", Prod.layout (args, Value.layout)),
			 ("con", Option.layout Con.layout con)],
	  Value.layout)
	 object
      fun primApp {args, prim, resultVar = _, resultType} =
	 let
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
	       Array_toVector =>
		  let
		     val res = result ()
		     datatype z = datatype Value.value
		     val () =
			case (Value.value (arg 0), Value.value res) of
			   (Ground _, Ground _) => ()
			 | (Object {args = a, ...}, Object {args = a', ...}) =>
			      Vector.foreach2
			      (Prod.dest a, Prod.dest a',
			       fn ({elt = v, ...}, {elt = v', ...}) =>
			       Value.unify (v, v'))
			 | _ => Error.bug "Array_toVector"
		  in
		     res
		  end
	     | FFI _ =>
		  (* Some imports, like Real64.modf, take ref cells that can not
		   * be flattened.
		   *)
		  dontFlatten ()
	     | MLton_eq => equal ()
	     | MLton_equal => equal ()
	     | MLton_size => dontFlatten ()
	     | Weak_get => Value.deWeak (arg 0)
	     | Weak_new => Value.new (Value.weak (arg 0))
	     | _ => result ()
	 end
      fun update {base, offset, value} =
	 coerce {from = value,
		 to = Value.select {base = base, offset = offset}}
      fun const c = typeValue (Type.ofConst c)
      val {func, value = varValue, ...} =
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
		  select = fn {base, offset, ...} => (Value.select
						      {base = base,
						       offset = offset}),
		  update = update,
		  useFromTypeOnBinds = false}
      (* Don't flatten outermost part of formal parameters. *)
      fun dontFlattenFormals (xts: (Var.t * Type.t) vector): unit =
	 Vector.foreach (xts, fn (x, _) => Value.dontFlatten (varValue x))
      val () =
	 List.foreach
	 (functions, fn f =>
	  let
	     val {args, blocks, ...} = Function.dest f
	     val () = dontFlattenFormals args
	     val () = Vector.foreach (blocks, fn Block.T {args, ...} =>
				      dontFlattenFormals args)
	  in
	     ()
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
		 display
		 (seq [Var.layout x, str " ", Value.layout (varValue x)]))
	  in
	     ()
	  end)
      (* Transform the program. *)
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
			     case Type.dest (Value.finalType v) of
				Type.Object {args, ...} => args
			      | _ => Error.bug "strange con"
		 in
		    {args = args, con = con}
		 end)
	  in
	     Datatype.T {cons = cons, tycon = tycon}
	  end)
      val valueType = Value.finalType
      fun valuesTypes vs = Vector.map (vs, Value.finalType)
      val {get = varTree: Var.t -> VarTree.t, set = setVarTree, ...} =
	 Property.getSetOnce (Var.plist,
			      Property.initRaise ("tree", Var.layout))
      val setVarTree =
	 Trace.trace2 ("DeepFlatten.setVarTree",
		       Var.layout, VarTree.layout, Unit.layout)
	 setVarTree
      fun simpleVarTree (x: Var.t): unit =
	 setVarTree
	 (x, VarTree.labelRoot (VarTree.fromTypeTree
				(Value.finalTree (varValue x)),
				x))
      fun transformFormals xts =
	 Vector.map (xts, fn (x, _) =>
		     let
			val () = simpleVarTree x
		     in
			(x, Value.finalType (varValue x))
		     end)
      fun replaceVar (x: Var.t): Var.t =
	 let
	    fun bug () = Error.bug (concat ["replaceVar ", Var.toString x])
	    val Tree.T (info, _) = varTree x
	 in
	    case info of
	       VarTree.Flat => bug ()
	     | VarTree.NotFlat {var, ...} =>
		  case var of
		     NONE => bug ()
		   | SOME y => y
	 end
      fun replaceVars xs = Vector.map (xs, replaceVar)
      fun transformBind {exp, ty, var}: Statement.t list =
	 let
	    fun simpleTree () = Option.app (var, simpleVarTree)
	    fun doit (e: Exp.t) =
	       let
		  val ty =
		     case var of
			NONE => ty
		      | SOME var => valueType (varValue var)
	       in
		  [Bind {exp = e, ty = ty, var = var}]
	       end
	    fun simple () =
	       (simpleTree ()
		; doit (Exp.replaceVar (exp, replaceVar)))
	    fun none () = []
	 in
	    case exp of
	       Const _ => simple ()
	     | Inject _ => simple ()
	     | Object {args, con} =>
		  (case var of
		      NONE => none ()
		    | SOME var =>
			 let
			    val v = varValue var
			 in
			    case Value.value v of
			       Value.Object {args = expects, flat, ...} =>
				  let
				     val z =
					Vector.map2
					(args, Prod.dest expects,
					 fn (arg, {elt, isMutable}) =>
					 let
					    val (vt, ss) =
					       coerceTree
					       {from = varTree arg,
						to = Value.finalTree elt}
					 in
					    ({elt = vt,
					      isMutable = isMutable},
					     ss)
					 end)
				     val vts = Vector.map (z, #1)
				     fun set info =
					setVarTree (var,
						    Tree.T (info,
							    Prod.make vts))
				  in
				     case !flat of
					Flat => (set VarTree.Flat; none ())
				      | NotFlat =>
					   let
					      val ty = Value.finalType v
					      val () =
						 set (VarTree.NotFlat
						      {ty = ty,
						       var = SOME var})
					      val args =
						 Vector.fromList
						 (Vector.foldr
						  (vts, [],
						   fn ({elt = vt, ...}, ac) =>
						   VarTree.rootsOnto (vt, ac)))
					      val obj =
						 Bind
						 {exp = Object {args = args,
								con = con},
						  ty = ty,
						  var = SOME var}
					   in
					      Vector.foldr
					      (z, [obj],
					       fn ((_, ss), ac) => ss @ ac)
					   end
				  end
			  | _ => Error.bug "transformStatement Object"
			 end)
	     | PrimApp {args, prim} =>
		  let
		     val () = simpleTree ()
		  in
		     doit (PrimApp {args = replaceVars args,
				    prim = prim})
		  end
	     | Select {base, offset} =>
		  (case var of
		      NONE => none ()
		    | SOME var =>
			 let
			    val baseVar = Base.object base
			    val Tree.T (info, children) = varTree baseVar
			    val {elt = child, isMutable} =
			       Prod.sub (children, offset)
			    val (child, ss) =
			       case info of
				  VarTree.Flat => (child, [])
				| VarTree.NotFlat _ =>
				     let
					val child =
					   (* Don't simplify a select out
					    * of a mutable field.
					    * Something may have mutated
					    * it.
					    *)
					   if isMutable
					      then VarTree.dropVars child
					   else child
				     in
					VarTree.fillInRoots
					(child,
					 {base = Base.map (base, replaceVar),
					  offset = (Value.finalOffset
						    (varValue baseVar,
						     offset))})
				     end
			    val () = setVarTree (var, child)
			 in
			    ss
			 end)
	     | Var x =>
		  (Option.app (var, fn y => setVarTree (y, varTree x))
		   ; none ())
	 end
      fun transformStatement (s: Statement.t): Statement.t list =
	 let
	    fun simple () = [Statement.replaceUses (s, replaceVar)]
	 in
	    case s of
	       Bind b => transformBind b
	     | Profile _ => simple ()
	     | Updates (base, us) =>
		  let
		     val baseVar =
			case base of
			   Base.Object x => x
			 | Base.VectorSub {vector = x, ...} => x
		     val objectValue = varValue baseVar
		     val ss = ref []
		     val us =
			Vector.foldr
			(us, [], fn ({offset, value}, ac) =>
			 let
			    val child =
			       Value.finalTree
			       (Value.select {base = objectValue,
					      offset = offset})
			    val offset = Value.finalOffset (objectValue, offset)
			 in
			    if not (TypeTree.isFlat child)
			       then {offset = offset,
				     value = replaceVar value} :: ac
			    else
			       let
				  val (vt, ss') =
				     coerceTree {from = varTree value,
						 to = child}
				  val () = ss := ss' @ (!ss)
				  val r = ref offset
				  val us = ref ac
				  val () =
				     VarTree.foreachRoot
				     (vt, fn var =>
				      let
					 val offset = !r
					 val () = r := 1 + !r
				      in
					 List.push (us, {offset = offset,
							 value = var})
				      end)
			       in
				  !us
			       end
			 end)
		  in
		     !ss @ [Updates (Base.map (base, replaceVar),
				     Vector.fromList us)]
		  end
	 end
      val transformStatement =
	 Trace.trace ("DeepFlatten.transformStatement",
		      Statement.layout,
		      List.layout Statement.layout)
	 transformStatement
      fun transformStatements ss =
	 Vector.concatV
	 (Vector.map (ss, Vector.fromList o transformStatement))
      fun transformTransfer t = Transfer.replaceVar (t, replaceVar)
      val transformTransfer =
	 Trace.trace ("DeepFlatten.transformTransfer",
		      Transfer.layout, Transfer.layout)
	 transformTransfer
      fun transformBlock (Block.T {args, label, statements, transfer}) =
	 Block.T {args = transformFormals args,
		  label = label,
		  statements = transformStatements statements,
		  transfer = transformTransfer transfer}
      fun transformFunction (f: Function.t): Function.t =
	  let
	     val {args, mayInline, name, start, ...} = Function.dest f
	     val {raises, returns, ...} = func name
	     val args = transformFormals args
	     val raises = Option.map (raises, valuesTypes)
	     val returns = Option.map (returns, valuesTypes)
	     val blocks = ref []
	     val () =
		Function.dfs (f, fn b =>
			      (List.push (blocks, transformBlock b)
			       ; fn () => ()))
	  in
	     Function.new {args = args,
			   blocks = Vector.fromList (!blocks),
			   mayInline = mayInline,
			   name = name,
			   raises = raises,
			   returns = returns,
			   start = start}
	  end
      val globals = transformStatements globals
      val functions = List.revMap (functions, transformFunction)
      val program =
	 Program.T {datatypes = datatypes,
		    functions = functions,
		    globals = globals,
		    main = main}
      val () = Program.clear program
   in
      shrink program
   end

end

