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
   
structure Value =
   struct
      datatype t =
	 Ground
       | Ref of t
       | Tuple of {elt: t, mayFlatten: bool Set.t} vector
       | Unary of t

      fun layout (v: t): Layout.t =
	 let
	    open Layout
	 in
	    case v of
	       Ground => str "Ground"
	     | Ref v => seq [str "Ref ", layout v]
	     | Tuple v =>
		  tuple (Vector.toListMap
			 (v, fn {elt, mayFlatten} =>
			  record [("elt", layout elt),
				  ("mayFlatten",
				   Bool.layout (Set.value mayFlatten))]))
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
      val reff = Ref
      val vector = unary
      val weak = unary

      val deUnary: t -> t =
	 fn Ground => Ground
	  | Unary v => v
	  | _ => Error.bug "deUnary"

      val deArray = deUnary
      val deref =
	 fn Ref v => v
	  | _ => Error.bug "deref"
      val deVector = deUnary
      val deWeak = deUnary

      val tuple: t vector -> t =
	 fn vs =>
	 if Vector.forall (vs, isGround)
	    then ground
	 else Tuple (Vector.map (vs, fn v =>
				 {elt = v, mayFlatten = Set.singleton true}))

      val select: t * int -> t =
	 fn (v, i) =>
	 case v of
	    Ground => ground
	  | Tuple v => #elt (Vector.sub (v, i))
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
	  | (Ref v, Ref v') => unify (v, v')
	  | (Tuple v, Tuple v') =>
	       Vector.foreach2
	       (v, v', fn ({elt = e, mayFlatten = m},
			   {elt = e', mayFlatten = m'}) =>
		(Set.union (m, m'); unify (e, e')))
	  | (Unary v, Unary v') => unify (v, v')
	  | _ => Error.bug "Value.unify"
   end

fun flatten (program as Program.T {datatypes, functions, globals, main}) =
   let
      val {get = conInfo: Con.t -> {args: {mayFlatten: bool ref,
					   value: Value.t} vector},
	   set = setConInfo, ...} =
	 Property.getSetOnce 
	 (Con.plist, Property.initRaise ("conInfo", Con.layout))
      val conArgs = #args o conInfo
      val () =
	 Vector.foreach
	 (datatypes, fn Datatype.T {cons, tycon} =>
	  Vector.foreach
	  (cons, fn {args, con} =>
	   setConInfo (con, {args = Vector.map (args, fn t =>
						{mayFlatten = ref true,
						 value = Value.fromType t})})))
      fun coerce {from, to} = Value.unify (from, to)
      fun conApp {args, con} =
	 (Vector.foreach2 (args, conArgs con, fn (v, {value = v', ...}) =>
			   coerce {from = v, to = v'})
	  ; Value.ground)
      fun filter (_, con, args) =
	 Vector.foreach2 (conArgs con, args, fn ({value = v, ...}, v') =>
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
      val {get = varInfo: Var.t -> {isDirectRef: bool ref,
				    isUsedOnlyAsRef: bool ref,
				    numOccurrences: int ref}, ...} =
	 Property.get (Var.plist,
		       Property.initFun
		       (fn _ => {isDirectRef = ref false,
				 isUsedOnlyAsRef = ref true,
				 numOccurrences = ref 0}))
      fun use x =
	 let
	    val {isUsedOnlyAsRef, numOccurrences, ...} = varInfo x
	 in
	    Int.inc numOccurrences
	    ; isUsedOnlyAsRef := false
	 end
      fun uses xs = Vector.foreach (xs, use)
      fun loopStatements ss =
	 Vector.foreach
	 (ss, fn Statement.T {exp, var, ...} =>
	  case exp of
	     ConApp {args, ...} => uses args
	   | Const _ => ()
	   | PrimApp {args, prim, ...} =>
		let
		   fun arg i = Vector.sub (args, i)
		   fun asRef () =
		      Int.inc (#numOccurrences (varInfo (arg 0)))
		   datatype z = datatype Prim.Name.t
		in
		   case Prim.name prim of
		      Ref_assign => (asRef (); use (arg 1))
		    | Ref_deref => asRef ()
		    | Ref_ref =>
			 (uses args
			  ;  Option.app (var, fn x =>
					 #isDirectRef (varInfo x) := true))
		    | _ => uses args
		end
	   | Profile _ => ()
	   | Select {tuple, ...} => use tuple
	   | Tuple xs => uses xs
	   | Var x => use x)
      fun loopTransfer t = Transfer.foreachVar (t, use)
      val () = loopStatements globals
      fun loopFormals xts =
	 Vector.foreach (xts, fn (x, _) =>
			 let
			    val {isUsedOnlyAsRef, ...} = varInfo x
			 in
			    isUsedOnlyAsRef := false
			 end)
      val {get = labelInfo: Label.t -> {args: (Var.t * Type.t) vector},
	   set = setLabelInfo, ...} =
	 Property.getSetOnce (Label.plist,
			      Property.initRaise ("info", Label.layout))
      val labelArgs = #args o labelInfo
      val () =
	 List.foreach
	 (functions, fn f =>
	  let
	     val {args, blocks, ...} = Function.dest f
	     val () = loopFormals args
	  in
	     Vector.foreach
	     (blocks, fn Block.T {args, label, statements, transfer, ...} =>
	      (setLabelInfo (label, {args = args})
	       ; loopFormals args
	       ; loopStatements statements
	       ; loopTransfer transfer))
	  end)
      val isLoneRef: Var.t -> bool =
	 fn x =>
	 let
	    val {isDirectRef, numOccurrences, ...} = varInfo x
	 in
	    !isDirectRef andalso 1 = !numOccurrences
	 end
      val () =
	 List.foreach
	 (functions, fn f =>
	  let
	     val {blocks, ...} = Function.dest f
	  in
	     Vector.foreach
	     (blocks, fn Block.T {statements, transfer, ...} =>
	      let
		 val () =
		    Vector.foreach
		    (statements, fn Statement.T {exp, var, ...} =>
		     case exp of
			ConApp {args, con, ...} =>
			   Vector.foreach2
			   (args, conArgs con, fn (x, {mayFlatten, ...}) =>
			    if isLoneRef x
			       then ()
			    else mayFlatten := false)
		      | Select {offset, tuple} =>
			   Option.app
			   (var, fn x =>
			    let
			       val {isUsedOnlyAsRef, ...} = varInfo x
			    in
			       if !isUsedOnlyAsRef
				  then ()
			       else (case value tuple of
					Value.Tuple v =>
					   Set.setValue
					   (#mayFlatten (Vector.sub (v, offset)),
					    false)
				      | _ => ())
			    end)
		      | Tuple xs =>
			   Option.app
			   (var, fn x =>
			    case value x of
			       Value.Tuple v =>
				  Vector.foreach2
				  (xs, v, fn (x, {mayFlatten, ...}) =>
				   if isLoneRef x
				      then ()
				   else Set.setValue (mayFlatten, false))
			     | _ => ())
		      | _ => ())
		 val () =
		    case transfer of
		       Case {cases = Cases.Con v, ...} =>
			  Vector.foreach
			  (v, fn (con, l) =>
			   Vector.foreach2
			   (conArgs con, labelArgs l,
			    fn ({mayFlatten, ...}, (x, _)) =>
			    let
			       val {isUsedOnlyAsRef, ...} = varInfo x
			    in
			       if !isUsedOnlyAsRef
				  then ()
			       else mayFlatten := false
			    end))
		     | _ => ()
	      in
		 ()
	      end)
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
			tuple (Vector.toListMap
			       (conArgs con, fn {mayFlatten, value} =>
				record
				[("mayFlatten", Bool.layout (!mayFlatten)),
				 ("value", Value.layout value)]))])))
	     val () =
		Program.foreachVar
		(program, fn (x, _) =>
		 let
		    val {isDirectRef, isUsedOnlyAsRef, numOccurrences} =
		       varInfo x
		 in
		    display
		    (seq [Var.layout x, str " ",
			  record
			  [("isDirectRef", Bool.layout (!isDirectRef)),
			   ("isUsedOnlyAsRef", Bool.layout (!isUsedOnlyAsRef)),
			   ("numOccurrences", Int.layout (!numOccurrences)),
			   ("value", Value.layout (value x))]])
		 end)
	  in
	     ()
	  end)
   in
      program
   end

end
