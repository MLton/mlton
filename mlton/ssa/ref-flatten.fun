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

structure Flat =
   struct
      datatype t =
	 ConOffset of {con: Con.t,
		       offset: int}
       | NotFlat
       | TupleOffset of {offset: int,
			 tuple: Type.t}
       | Unknown

      val layout: t -> Layout.t =
	 fn f =>
	 let
	    open Layout
	 in
	    case f of
	       ConOffset {con, offset} =>
		  seq [str "ConOffset ",
		       record [("con", Con.layout con),
			       ("offset", Int.layout offset)]]
	     | NotFlat => str "NotFlat"
	     | TupleOffset {offset, tuple} =>
		  seq [str "TupleOffset ",
		       record [("offset", Int.layout offset),
			       ("tuple", Type.layout tuple)]]
	     | Unknown => str "Unknown"
	 end
   end

structure Value =
   struct
      datatype t =
	 Ground
       | Ref of {arg: t,
		 flat: Flat.t Set.t}
       | Tuple of t vector
       | Unary of t

      fun layout (v: t): Layout.t =
	 let
	    open Layout
	 in
	    case v of
	       Ground => str "Ground"
	     | Ref {arg, flat} =>
		  seq [str "Ref ",
		       record [("arg", layout arg),
			       ("flat", Flat.layout (Set.value flat))]]
	     | Tuple v => tuple (Vector.toListMap (v, layout))
	     | Unary v => seq [str "Unary ", layout v]
	 end

      val ground = Ground

      val isGround =
	 fn Ground => true
	  | _ => false
	 
      fun unary v =
	 case v of
	    Ground => Ground
	  | _ => Unary v

      val array = unary
      val vector = unary
      val weak = unary
	 
      val reff: t -> t =
	 fn arg => Ref {arg = arg, flat = Set.singleton Flat.Unknown}

      val deUnary: t -> t =
	 fn Ground => Ground
	  | Unary v => v
	  | _ => Error.bug "deUnary"

      val deArray = deUnary
      val deref =
	 fn Ref {arg, ...} => arg
	  | _ => Error.bug "deref"
      val deVector = deUnary
      val deWeak = deUnary

      val tuple: t vector -> t =
	 fn vs =>
	 if Vector.forall (vs, isGround)
	    then ground
	 else Tuple vs

      val select: t * int -> t =
	 fn (v, i) =>
	 case v of
	    Ground => ground
	  | Tuple v => Vector.sub (v, i)
	  | _ => Error.bug "Value.select"
	       
      fun fromType (t: Type.t) =
	 let
	    datatype z = datatype Type.dest
	 in
	    case Type.dest t of
	       Array t => array (fromType t)
	     | Ref t => reff (fromType t)
	     | Tuple ts => tuple (Vector.map (ts, fromType))
	     | Vector t => vector (fromType t)
	     | Weak t => weak (fromType t)
	     | _ => ground
	 end

      val rec unify: t * t -> unit =
	 fn (Ground, Ground) => ()
	  | (Ref {arg = a, flat = f}, Ref {arg = a', flat = f'}) =>
	       (Set.union (f, f'); unify (a, a'))
	  | (Tuple v, Tuple v') => Vector.foreach2 (v, v', unify)
	  | (Unary v, Unary v') => unify (v, v')
	  | _ => Error.bug "Value.unify"
   end

fun flatten (program as Program.T {datatypes, functions, globals, main}) =
   let
      val {get = conInfo: Con.t -> {args: Value.t vector},
	   set = setConInfo, ...} =
	 Property.getSetOnce 
	 (Con.plist, Property.initRaise ("conInfo", Con.layout))
      val conArgs = #args o conInfo
      val () =
	 Vector.foreach
	 (datatypes, fn Datatype.T {cons, tycon} =>
	  Vector.foreach
	  (cons, fn {args, con} =>
	   setConInfo (con, {args = Vector.map (args, Value.fromType)})))
      fun coerce {from, to} = Value.unify (from, to)
      fun conApp {args, con} =
	 (Vector.foreach2 (args, conArgs con, fn (v, v') =>
			   coerce {from = v, to = v'})
	  ; Value.ground)
      fun filter (_, con, args) =
	 Vector.foreach2 (conArgs con, args, fn (v, v') =>
			  coerce {from = v, to = v'})
      fun primApp {args, prim, resultVar, resultType, targs = _} =
	 let
	    fun arg i = Vector.sub (args, i)
	    fun result () = Value.fromType resultType
	    datatype z = datatype Prim.Name.t
	 in
	    case Prim.name prim of
	       Array_sub => Value.deArray (arg 0)
	     | Array_toVector => arg 0
	     | Array_update =>
		  (coerce {from = arg 2,
			   to = Value.deArray (arg 0)}
		   ; result ())
	     | Ref_assign =>
		  (coerce {from = arg 1, to = Value.deref (arg 0)}; result ())
	     | Ref_deref => Value.deref (arg 0)
	     | Ref_ref => Value.reff (arg 0)
	     | Vector_sub => Value.deVector (arg 0)
	     | Weak_get => Value.deWeak (arg 0)
	     | Weak_new => Value.weak (arg 0)
	     | _ => result ()
	 end
      fun select {offset, resultType = _, tuple} =
	 Value.select (tuple, offset)
      val {value, ...} =
	 analyze {coerce = coerce,
		  conApp = conApp,
		  const = fn _ => Value.ground,
		  filter = filter,
		  filterWord = fn _ => (),
		  fromType = Value.fromType,
		  layout = Value.layout,
		  primApp = primApp,
		  program = program,
		  select = select,
		  tuple = Value.tuple,
		  useFromTypeOnBinds = false}
      (* Mark a ref field of a tuple as unflattenable if
       * 1. When the ref is constructed, it is used in some way other
       *    than put in the tuple.
       * 2. When the ref is extracted, it is used in some way other than in
       *    a ! or :=.
       *
       * Count numOccurences of each variable.
       * Flag indicating if the variable is directly the result of a ref.
       * Flag indicating if a variable is used in something other than
       * a ! or :=.
       *)
      val {get = varInfo: Var.t -> {flat: Flat.t ref}, ...} =
	 Property.get (Var.plist,
		       Property.initFun (fn _ => {flat = ref Flat.Unknown}))
      fun use x = #flat (varInfo x) := Flat.NotFlat
      fun uses xs = Vector.foreach (xs, use)
      fun object (xs, make) =
	 Vector.foreachi
	 (xs, fn (i, x) =>
	  let
	     val {flat, ...} = varInfo x
	  in
	     case !flat of
		Flat.Unknown => flat := make {offset = i}
	      | _ => flat := Flat.NotFlat
	  end)
      fun loopStatements ss =
	 Vector.foreach
	 (ss, fn Statement.T {exp, ty, var} =>
	  case exp of
	     ConApp {args, con, ...} =>
		object (args, fn {offset} =>
			Flat.ConOffset {con = con, offset = offset})
	   | Const _ => ()
	   | PrimApp {args, ...} => uses args
	   | Profile _ => ()
	   | Select {tuple, ...} => use tuple
	   | Tuple xs =>
		object (xs, fn {offset} =>
			Flat.TupleOffset {offset = offset, tuple = ty})
	   | Var x => use x)
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
      (* Now, walk over the whole program and try to flatten each ref. *)
      fun loopStatement (Statement.T {exp, var, ...}) =
	 case exp of
	    PrimApp {prim, ...} =>
	       (case Prim.name prim of
		   Prim.Name.Ref_ref =>
		      Option.app
		      (var, fn var =>
		       case value var of
			  Value.Ref {flat, ...} =>
			     let
				datatype z = datatype Flat.t
				val {flat = flat'} = varInfo var
				val flat' = !flat'
				fun notFlat () = Set.setValue (flat, NotFlat)
			     in
				case flat' of
				   ConOffset {con = c, offset = i} =>
				      (case Set.value flat of
					  ConOffset {con = c', offset = i'} =>
					     if Con.equals (c, c') andalso i = i'
						then ()
					     else notFlat ()
					| Unknown => Set.setValue (flat, flat')
					| _ => notFlat ())
				 | NotFlat => notFlat ()
				 | TupleOffset {offset = i, tuple = t} =>
				      (case Set.value flat of
					  TupleOffset {offset = i', tuple = t'} =>
					     if i = i' andalso Type.equals (t, t')
						then ()
					     else notFlat ()
					| Unknown => Set.setValue (flat, flat')
					| _ => notFlat ())
				 | Unkonwn => notFlat ()
			     end
			| _ => Error.bug "Ref_ref with strange value")
		 | _ => ())
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
		  display
		  (seq [Con.layout con, str " ",
			tuple (Vector.toListMap (conArgs con, Value.layout))])))
	     val () =
		Program.foreachVar
		(program, fn (x, _) =>
		 let
		    val {flat} = varInfo x
		 in
		    display
		    (seq [Var.layout x, str " ",
			  record [("flat", Flat.layout (!flat)),
				  ("value", Value.layout (value x))]])
		 end)
	  in
	     ()
	  end)
   in
      program
   end

end
