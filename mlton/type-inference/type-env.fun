(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor TypeEnv (S: TYPE_ENV_STRUCTS): TYPE_ENV =
struct

open S

local open Ast
in structure Aconst = Const
end

structure Field = Record.Field
structure Srecord = SortedRecord
structure Set = DisjointSet

structure Unknown =
   struct
      datatype t = T of {canGeneralize: bool,
			 equality: bool,
			 id: int}

      fun layout (T {id, ...}) =
	 let
	    open Layout
	 in
	    seq [str "Unknown ", Int.layout id]
	 end
      (* 		 namedRecord ("Unknown",
       * 			      [("equality", Bool.layout equality),
       * 			       ("canGeneralize", Bool.layout canGeneralize)]),
       *)
      local
	 val r: int ref = ref 0
      in
	 fun newId () = (Int.inc r; !r)
      end

      fun new {canGeneralize, equality} =
	 T {canGeneralize = canGeneralize,
	    equality = equality,
	    id = newId ()}

      fun join (T r, T r'): t =
	 T {equality = #equality r orelse #equality r',
	    canGeneralize = #canGeneralize r andalso #canGeneralize r',
	    id = newId ()}
   end

structure Type =
   struct
      structure Set = DisjointSet
    
      (* Tuples of length <> 1 are always represented as records.
       * There will never be tuples of length one.
       *)
      datatype ty =
	 Unknown of Unknown.t
       | Var of Tyvar.t
       | Con of Tycon.t * t vector
       | Word (* an unresolved word type *)
       | Int (* an unresolved int type *)
       | Record of {flexible: bool,
		    record: t Srecord.t}
      withtype t = {ty: ty,
		    plist: PropertyList.t} Set.t

      val toType: t -> ty = #ty o Set.value
      val plist: t -> PropertyList.t = #plist o Set.value

      (* unknown is a bit strange because frees needs the reference to the
       * whole type (Set.t) and not just the unknown info
       *)
      fun makeHom {con, int, record, unknown, var, word} =
	 let
	    val {get, destroy} =
	       Property.destGet
	       (plist,
		Property.initRec
		(fn (t, get) =>
		 case toType t of
		    Word => word
		  | Int => int
		  | Unknown u => unknown (u, t)
		  | Var a => var a
		  | Con (c, ts) => con (c, Vector.map (ts, get))
		  | Record {flexible, record = r} =>
		       record {flexible = flexible,
			       record = Srecord.map (r, get)}))
	 in {hom = get, destroy = destroy}
	 end

      fun hom {ty, unknown, var, con, word, int, record} =
	 let val {hom, destroy} = makeHom {unknown = unknown,
					   var = var,
					   con = con,
					   word = word,
					   int = int,
					   record = record}
	 in hom ty before destroy ()
	 end

      fun deconOpt t =
	 case toType t of
	    Con x => SOME x
	  | _ => NONE

      fun layoutDetailed ty =
	 let
	    open Layout
	 in hom {ty = ty,
		 unknown = Unknown.layout o #1,
		 var = fn a => paren (seq [str "Var ", Tyvar.layout a]),
		 word = str "Word",
		 int = str "Int",
		 con = fn (c, ts) => paren (align
					    [seq [str "Con ", Tycon.layout c],
					     Vector.layout (fn l => l) ts]),
		 record = fn {flexible, record} =>
		 paren (seq ([str "Record ",
			      Srecord.layout
			      {extra = if flexible then "..." else "",
			       layoutElt = fn x => x,
			       layoutTuple = vector,
			       record = record,
			       separator = " ="}]))}
			       
	 end

      local
	 structure T = Ast.Type
      in
	 fun toAst t =
	    case toType t of
	       Unknown _ => T.unit
	     | Var a => T.var a
	     | Word => T.con (Ast.Tycon.word, Vector.new0 ())
	     | Int => T.con (Ast.Tycon.int, Vector.new0 ())
	     | Con (c, ts) => T.con (Tycon.toAst c, Vector.map (ts, toAst))
	     | Record {flexible, record} =>
		  T.record (Srecord.map (record, toAst))
      end

      fun newTy (ty: ty): t =
	 Set.singleton {ty = ty,
			plist = PropertyList.new ()}

      val new = newTy o Unknown o Unknown.new

      val record = newTy o Record

      fun tuple ts =
	 if 1 = Vector.length ts
	    then Vector.sub (ts, 0)
	 else record {flexible = false,
		      record = Srecord.tuple ts}

      val unit = tuple (Vector.new0 ())

      fun isUnit s =
	 case toType s of
	    Record {flexible = false, record} =>
	       (case Srecord.detupleOpt record of
		   SOME ts => Vector.isEmpty ts
		 | NONE => false)
	  | _ => false

      val _ = if isUnit unit then () else Error.bug "unit isn't unit"
	 
      fun con (tycon, ts) =
	 if Tycon.equals (tycon, Tycon.tuple) then tuple ts
	 else newTy (Con (tycon, ts))

      val layout = Layout.switch {detailed = layoutDetailed,
				  normal = Ast.Type.layout o toAst}
      val layout = layoutDetailed
	 
      val equals: t * t -> bool = Set.equals
   end

structure Ops = TypeOps (structure Tycon = Tycon
			 open Type)

structure Type =
   struct
      (* Order is important, since want specialized definitions
       * in Type to override general definitions in Ops.
       *)
      open Ops Type

      fun substitute (t: t, tyvarsAndTypes: (Tyvar.t * Type.t) vector): t =
	 let
	    fun loop t =
	       case toType t of
		  Unknown _ => t
		| Word => t
		| Int => t
		| Var a => (case Vector.peek (tyvarsAndTypes, fn (a', _) =>
					      Tyvar.equals (a, a')) of
			       NONE => t
			     | SOME (_, t) => t)
		| Con (c, ts) => newTy (Con (c, Vector.map (ts, loop)))
		| Record {flexible, record} =>
		     if flexible
			then Error.bug "substituting in flexible record type"
		     else newTy (Record {flexible = false,
					 record = Srecord.map (record, loop)})
	 in loop t
	 end

      (* val substitute =
       *    Trace.trace2 ("substitute", layout, Layout.ignore, layout) substitute		
       *)

      val var = newTy o Var

      (*val con = Trace.trace2 ("con", Tycon.layout,
       List.layout (", ", layout),
       layout) con*)
      fun ofConst c =
	 case c of
	    Aconst.Char _ => char
	  | Aconst.Int _ => newTy Type.Int
	  | Aconst.Real _ => real
	  | Aconst.String _ => string
	  | Aconst.Word _ => newTy Type.Word

      fun derecord t =
	 case toType t of
	    Record r => r
	  | _ => Error.bug "Type.deRecord"

      (* val traceCanUnify = Trace.trace2 ("canUnify", layout, layout, Bool.layout) *)

      fun canUnify arg = 
	 (*   traceCanUnify *)
	 (fn (t, t') =>
	  case (toType t,   toType t') of
	     (Unknown _,  _) => true
	   | (_, Unknown _) => true
	   | (Con (c, ts), Con (c', ts')) => (Tycon.equals (c, c')
					      andalso
					      Vector.forall2 (ts, ts', canUnify))
	   | (Con (c, ts), Word) =>
		0 = Vector.length ts andalso Tycon.isWordX c
	   | (Word, Con (c, ts)) =>
		0 = Vector.length ts andalso Tycon.isWordX c
	   | (Con (c, ts), Int) =>
		0 = Vector.length ts andalso Tycon.isIntX c
	   | (Int, Con (c, ts)) =>
		0 = Vector.length ts andalso Tycon.isIntX c
	   | (Var a, Var a') => Tyvar.equals (a, a')
	   | (Word, Word) => true
	   | (Int, Int) => true
	   | (Record {flexible = f, record = r},
	      Record {flexible = f', record = r'}) =>
	     if f orelse f' then Error.bug "canUnify on flexible record"
	     else let
		     val fs = Srecord.toVector r
		     val fs' = Srecord.toVector r'
		  in Vector.length fs = Vector.length fs'
		     andalso Vector.forall2 (fs, fs', fn ((f, t), (f', t')) =>
					     Field.equals (f, f')
					     andalso canUnify (t, t'))
		  end
	    | _ => false) arg

      val traceUnify = Trace.trace2 ("unify", layout, layout, Unit.layout)

      fun unify arg =
	 traceUnify
	 (fn (s, s') =>
	  if Set.equals (s, s') then ()
	  else
	     let
		val t = toType s
		val t' = toType s'
		val t =
		   case (t, t')           of
		      (Unknown r, Unknown r') => Unknown (Unknown.join (r, r'))
		    | (t, Unknown _) => t
		    | (Unknown _, t) => t
		    | (Var a, Var a') => if Tyvar.equals (a, a')
					    then t
					 else Error.bug "unify Var"
		    | (Con (c, ts), Con (c', ts')) =>
			 if Tycon.equals (c, c')
			    then (unifys (ts, ts'); t)
			 else Error.bug "unify Con"
		    | (Con (c, ts), Word) =>
			 if Tycon.isWordX c andalso Vector.isEmpty ts
			    then t
			 else Error.bug "unify Word"
		    | (Word, Con (c, ts)) =>
			 if Tycon.isWordX c andalso Vector.isEmpty ts
			    then t'
			 else Error.bug "unify Word"
		    | (Con (c, ts), Int) =>
			 if Tycon.isIntX c andalso Vector.isEmpty ts
			    then t
			 else Error.bug "unify Int"
		    | (Int, Con (c, ts)) =>
			 if Tycon.isIntX c andalso Vector.isEmpty ts
			    then t'
			 else Error.bug "unify Int"
		    | (Word, Word) => t
		    | (Int, Int) => t
		       | (Record {flexible = f, record = r},
			  Record {flexible = f', record = r'}) =>
			 let
			    fun oneFlex (flex, nonflex) =
			       let
				  val n = Vector.length flex
			       in
				  Vector.fold'
				  (nonflex, 0, 0,
				   fn (_, (f', t'), i) =>
				   if i = n
				      then Vector.Done ()
				   else let
					   val (f, t) = Vector.sub (flex, i)
					   val i =
					      if Field.equals (f, f')
						 then (unify (t, t')
						       ; i + 1)
					      else i
					in Vector.Continue i
					end,
				     fn i => if i = n
						then ()
					     else
						Error.bug
						"impossible unification of flexible record pattern")
			       end
			    val lay =
			       Vector.layout (Layout.tuple2 (Field.layout, layout))
			    val oneFlex =
			       Trace.trace2 ("oneFlex", lay, lay, Unit.layout)
			       oneFlex
			    val fs = Srecord.toVector r
			    val fs' = Srecord.toVector r'
			    val n = Vector.length fs
			    val n' = Vector.length fs'
			 in if f
			       then
				  if f'
				     then
					let
					   fun loop (fs, fs', ac) =
					      case (fs, fs') of
						 ([], []) => rev ac
					       | (_, []) => List.appendRev (ac, fs)
					       | ([], _) => List.appendRev (ac, fs')
					       | ((f, t) :: fs0, (f', t') :: fs'0) =>
						    if Field.equals (f, f')
						       then (unify (t, t')
							     ; loop (fs0, fs'0,
								     (f, t) :: ac))
						    else if Field.<= (f, f')
							    then
							       loop (fs0, fs',
								     (f, t) :: ac)
							 else
							    loop (fs, fs'0,
								  (f', t') :: ac)
					in Record
					   {flexible = true,
					    record = (Srecord.fromVector
						      (Vector.fromList
						       (loop (Vector.toList fs,
							      Vector.toList fs',
							      []))))}
					end
				  else (oneFlex (fs, fs'); t')
			    else if f' then (oneFlex (fs', fs); t)
				 else if n = n'
					 then (Vector.foreach2
					       (fs, fs', fn ((f, t), (f', t')) =>
						if Field.equals (f, f')
						   then unify (t, t')
						else Error.bug "unify field")
					       ; t)
				      else Error.bug "unify mismatch: different length records"
			 end
			| _ => Error.bug "unify mismatch"
		val _ = Set.union (s, s')
		val _ = Set.setValue (s, {ty = t, plist = PropertyList.new ()})
	     in
		()
	     end) arg

      and unifys (ts: t vector, ts': t vector): unit =
	 if Vector.length ts = Vector.length ts'
	    then Vector.foreach2 (ts, ts', unify)
	 else Error.bug "unify Con: different number of args"

      fun unifys ts =
	 case ts of
	    t :: ts => (List.foreach (ts, fn t' => unify (t, t'))
			; t)
	  | [] => new {equality = false, canGeneralize = true}

      local
	 structure X = XmlType
	 val con = X.con
	 val unknown = con (Tycon.tuple, Vector.new0 ())
	 val {hom, ...} =
	    makeHom {unknown = fn _ => unknown,
		     var = X.var,
		     word = con (Tycon.defaultWord, Vector.new0 ()),
		     int = con (Tycon.defaultInt, Vector.new0 ()),
		     con = con,
		     record = fn {flexible, record} =>
		     if flexible
			then Error.bug "unresolved ... in flexible pattern"
		     else
			let
			   val ts = Srecord.range record
			in
			   if 1 = Vector.length ts
			      then Vector.sub (ts, 0)
			   else con (Tycon.tuple, ts)
			end}
      in
	 val toXml = hom

	 val toXml = Trace.trace ("toXml", layout, XmlType.layout) toXml
      end
   end

structure InferScheme =
   struct
      structure Scheme = GenericScheme (structure Tyvar = Tyvar
					structure Type = Type)
      open Scheme

      fun mayContainUnknown (T {tyvars, ty, ...}): bool =
	 let
	    exception Yes
	    val {hom, destroy} =
	       Type.makeHom
	       {con = fn _ => (),
		int = (),
		record = fn _ => (),
		unknown = (fn (Unknown.T {canGeneralize, ...}, t) =>
			   if canGeneralize
			      then raise Yes
			   else ()),
		var = fn a => if Vector.exists (tyvars, fn a' =>
						Tyvar.equals (a, a'))
				 then ()
			      else raise Yes,
		word = ()}
	    val res = (hom ty; false) handle Yes => true
	    val _ = destroy ()
	 in
	    res
	 end

      fun instantiate {scheme, canGeneralize} =
	 let
	    val ts = Vector.map (tyvars scheme, fn v =>
				 Type.new {equality = Tyvar.isEquality v,
					   canGeneralize = canGeneralize})
	 in {instance = apply (scheme, ts), args = ts}
	 end
   end

structure VarRange =
   struct
      datatype kind =
	 Normal
       | Delayed
       | Recursive of Tyvar.t vector ref
       | Overload of (Var.t * Type.t) vector

      datatype t = T of {scheme: InferScheme.t,
			 kind: kind}

      fun scheme (T {scheme, ...}) = scheme

      fun layout (T {scheme, ...}) = InferScheme.layout scheme
   end

datatype t = T of z ref
and z =
   Empty
 | Cons of InferScheme.t * t

fun toList (T opens) =
   case !opens of
      Empty => []
    | Cons (s, opens) => s :: (toList opens)

local
   open Layout
in
   fun layout e = list (List.map (toList e, InferScheme.layout))
end

val empty: t = T (ref Empty)

(* This property is destroyed after type inference is done by the call to
 * Xml.Program.clear in infer.fun.
 * The property can not be getSetOnce, because it is set twice for fun
 * declarations.
 *)
val {get = getVarRange: Var.t -> VarRange.t, set = setVarRange} =
   Property.getSet (Var.plist, Property.initRaise ("range", Var.layout))

val setVarRange =
   Trace.trace2 ("setVarRange", Var.layout, VarRange.layout, Unit.layout)
   setVarRange
   
fun extendVarRange (e, x, r) =
   let
      val _ = setVarRange (x, r)
      val s = VarRange.scheme r
   in
      if InferScheme.mayContainUnknown s
	 then T (ref (Cons (s, e)))
      else e
   end

fun lookupVarRange (_, x) = getVarRange x

val lookupVarRange =
   Trace.trace2 ("Env.lookupVarRange", layout, Var.layout, VarRange.layout)
   lookupVarRange

fun lookupVar (e, x) = VarRange.scheme (lookupVarRange (e, x))
   
fun extendVar (e, x, s) =
   extendVarRange (e, x, VarRange.T {scheme = s,
				     kind = VarRange.Normal})
val extendVar =
   Trace.trace3 ("Env.extendVar", layout, Var.layout, InferScheme.layout,
		 layout)
   extendVar

structure Free =
   struct
      datatype t =
	 Type of Type.t
       | Tyvar of Tyvar.t

      val equals =
	 fn (Type t, Type t') => Type.equals (t, t')
	  | (Tyvar a, Tyvar a') => Tyvar.equals (a, a')
	  | _ => false
   end

fun close (e, ty: Type.t, ensure: Tyvar.t vector): Tyvar.t list =
   let
      val frees: Free.t list ref = ref []
	 
      fun addFree f =
	 if List.contains (!frees, f, Free.equals)
	    then ()
	 else List.push (frees, f)
      val _ =
	 Type.hom
	 {ty = ty,
	  con = fn _ => (),
	  word = (),
	  int = (),
	  record = fn _ => (),
	  unknown = (fn (Unknown.T {canGeneralize, ...}, t) =>
		     if canGeneralize
			then addFree (Free.Type t)
		     else ()),
	  var = fn a => addFree (Free.Tyvar a)}
      (* Loop through all of the type schemes in the environment and remove
       * any frees.
       *)
      fun loop (T r, T r') =
	 if List.isEmpty (!frees)
	    then ()
	 else
	    case !r' of
	       Empty => r := Empty
	     | pair as Cons (InferScheme.T {tyvars = bound, ty}, r') =>
		  let
		     val keep = ref false
		     fun removeFree f =
			(keep := true
			 ; if List.contains (!frees, f, Free.equals)
			      then frees := List.remove (!frees, fn f' =>
							 Free.equals (f, f'))
			   else ())
		     val _ =
			Type.hom
			{ty = ty,
			 con = fn _ => (),
			 int = (),
			 record = (fn {flexible, ...} =>
				   if flexible
				      then keep := true
				   else ()),
			 unknown = (fn (Unknown.T {canGeneralize, ...}, t) =>
				    if canGeneralize
				       then removeFree (Free.Type t)
				    else ()),
			 var = (fn a =>
				if Vector.contains (ensure, a, Tyvar.equals)
				   then Error.bug "unable to generalize tyvar"
				else 
				   if Vector.exists (bound, fn a' =>
						     Tyvar.equals (a, a'))
				      then ()
				   else removeFree (Free.Tyvar a)),
			 word = ()}
		  in
		     if !keep
			then (r := pair
			      ; loop (r', r'))
		     else loop (T r, r')
		  end
      val _ = loop (e, e)
   in
      List.revMap
      (!frees, fn free =>
       case free of
	  Free.Tyvar a => a
	| Free.Type s => 
	     case Type.toType s of
		Type.Unknown (Unknown.T {equality, ...}) =>
		   let
		      val a = Tyvar.newNoname {equality = equality}
		      val _ = Set.setValue (s, {ty = Type.Var a,
						plist = PropertyList.new ()})
		   in
		      a
		   end
	      | _ => Error.bug "Env.close: not unknown")
   end

val close =
   Trace.trace3
   ("Env.close", layout, Type.layout, Vector.layout Tyvar.layout,
    List.layout Tyvar.layout)
   close
   
end
