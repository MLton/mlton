(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor TypeEnv (S: TYPE_ENV_STRUCTS): TYPE_ENV =
struct

open S

structure Field = Record.Field
structure Srecord = SortedRecord
structure Set = DisjointSet

(*
 * Keep a clock so that when we need to generalize a type we can tell which
 * unknowns were created in the expression being generalized.
 *
 * Keep track of all unknowns and the time allocated. 
 *
 * Unify should always keep the older unknown.
 *
 * If they are unknowns since the clock, they may be generalized.
 *
 * For type variables, keep track of the time that they need to be generalized
 * at.  If they are ever unified with an unknown of an earlier time, then
 * they can't be generalized.
 *)
structure Time:>
   sig
      type t

      val <= : t * t -> bool
      val equals: t * t -> bool
      val min: t * t -> t
      val layout: t -> Layout.t
      val now: unit -> t
      val tick: unit -> t
   end =
   struct
      type t = int

      val equals = op =
	 
      val min = Int.min

      val op <= = Int.<=

      val layout = Int.layout

      val clock: t ref = ref 0

      fun now () = !clock

      fun tick () = (clock := 1 + !clock
		     ; !clock)
   end
   
structure Unknown =
   struct
      datatype t = T of {canGeneralize: bool,
			 equality: bool,
			 id: int,
			 time: Time.t ref}

      local
	 fun make f (T r) = f r
      in
	 val time = ! o (make #time)
      end

      fun layout (T {canGeneralize, id, time, ...}) =
	 let
	    open Layout
	 in
	    seq [str "Unknown ",
		 record [("canGeneralize", Bool.layout canGeneralize),
			 ("id", Int.layout id),
			 ("time", Time.layout (!time))]]
	 end

      fun minTime (u as T {time, ...}, t) =
	 if Time.<= (!time, t)
	    then ()
	 else time := t

      fun layoutPretty (T {id, ...}) =
	 let
	    open Layout
	 in
	    seq [str "'a", Int.layout id]
	 end

      val toString = Layout.toString o layoutPretty
      
      local
	 val r: int ref = ref 0
      in
	 fun newId () = (Int.inc r; !r)
      end

      fun new {canGeneralize, equality} =
	 T {canGeneralize = canGeneralize,
	    equality = equality,
	    id = newId (),
	    time = ref (Time.now ())}

      fun join (T r, T r'): t =
	 T {canGeneralize = #canGeneralize r andalso #canGeneralize r',
	    equality = #equality r andalso #equality r',
	    id = newId (),
	    time = ref (Time.min (! (#time r), ! (#time r')))}
   end

(* Flexible record spine, i.e. a possibly extensible list of fields. *)
structure Spine:
   sig
      type t

      val canAddFields: t -> bool
      val empty: unit -> t
      val equals: t * t -> bool
      val fields: t -> Field.t list
      (* ensureField checks if field is there.  If it is not, then ensureField
       * will add it unless no more fields are allowed in the spine.
       * It returns true iff it succeeds.
       *)
      val ensureField: t * Field.t -> bool
      val foldOverNew: t * (Field.t * 'a) list * 'b * (Field.t * 'b -> 'b) -> 'b
      val layout: t -> Layout.t
      val new: Field.t list -> t
      val noMoreFields: t -> unit
      (* Unify returns the fields that are in each spine but not in the other.
       *)
      val unify: t * t -> unit
   end =
   struct
      datatype t = T of {fields: Field.t list ref,
			 more: bool ref} Set.t

      fun new fields = T (Set.singleton {fields = ref fields,
					 more = ref true})

      fun equals (T s, T s') = Set.equals (s, s')

      fun empty () = new []

      fun layout (T s) =
	 let
	    val {fields, more} = Set.value s
	 in
	    Layout.record [("fields", List.layout Field.layout (!fields)),
			   ("more", Bool.layout (!more))]
	 end

      fun canAddFields (T s) = ! (#more (Set.value s))
      fun fields (T s) = ! (#fields (Set.value s))

      fun ensureFieldValue ({fields, more}, f) =
	 List.contains (!fields, f, Field.equals)
	 orelse (!more andalso (List.push (fields, f); true))

      fun ensureField (T s, f) = ensureFieldValue (Set.value s, f)

      fun noMoreFields (T s) = #more (Set.value s) := false

      fun unify (T s, T s') =
	 let
	    val {fields = fs, more = m} = Set.value s
	    val {more = m', ...} = Set.value s'
	    val _ = Set.union (s, s')
	    val _ = Set.setValue (s, {fields = fs, more = ref (!m andalso !m')})
	 in
	    ()
	 end

      fun foldOverNew (spine: t, fs, ac, g) =
	 List.fold
	 (fields spine, ac, fn (f, ac) =>
	  if List.exists (fs, fn (f', _) => Field.equals (f, f'))
	     then ac
	  else g (f, ac))
   end

val {get = tyvarTime: Tyvar.t -> Time.t ref, ...} =
   Property.get (Tyvar.plist, Property.initFun (fn _ => ref (Time.now ())))

local
   type z = Layout.t * {isChar: bool, needsParen: bool}
   open Layout
in
   fun simple (l: Layout.t): z =
      (l, {isChar = false, needsParen = false})
   val dontCare: z = simple (str "_")
   fun layoutRecord (ds: (Field.t * z) list) =
      simple (case ds of
		 [] => str "{...}"
	       | _ => 
		    seq [str "{",
			 mayAlign
			 (separateRight
			  (List.map
			   (QuickSort.sortList (ds, fn ((f, _), (f', _)) =>
						Field.<= (f, f')),
			    fn (f, (l, _)) => seq [Field.layout f, str ": ", l]),
			   ",")),
			 str ", ...}"])
   fun layoutTuple (zs: z vector): z =
      Tycon.layoutApp (Tycon.tuple, zs)
end

structure Type =
   struct
      (* Tuples of length <> 1 are always represented as records.
       * There will never be tuples of length one.
       *)
      datatype t = T of {ty: ty,
			 plist: PropertyList.t} Set.t
      and ty =
	 Con of Tycon.t * t vector
	| FlexRecord of {fields: fields,
			 spine: Spine.t,
			 time: Time.t ref}
	(* GenFlexRecord only appears in type schemes.
	 * It will never be unified.
	 * The fields that are filled in after generalization are stored in
	 * extra.
	 *)
	| GenFlexRecord of genFlexRecord
	| Int (* an unresolved int type *)
	| Real (* an unresolved real type *)
	| Record of t Srecord.t
	| Unknown of Unknown.t
	| Var of Tyvar.t
	| Word (* an unresolved word type *)
      withtype fields = (Field.t * t) list
      and genFlexRecord =
	 {extra: unit -> {field: Field.t,
			  tyvar: Tyvar.t} list,
	  fields: (Field.t * t) list,
	  spine: Spine.t}
 
      val freeFlexes: t list ref = ref []
      val freeUnknowns: t list ref = ref []

      local
	 fun make f (T s) = f (Set.value s)
      in
	 val toType: t -> ty = make #ty
	 val plist: t -> PropertyList.t = make #plist
      end

      local
	 open Layout
      in
	 fun layoutFields fs =
	    List.layout (Layout.tuple2 (Field.layout, layout)) fs
	 and layout ty =
	    case toType ty of
	       Con (c, ts) =>
		  paren (align [seq [str "Con ", Tycon.layout c],
				Vector.layout layout ts])
	     | FlexRecord {fields, spine, time} =>
		  seq [str "Flex ",
		       record [("fields", layoutFields fields),
			       ("spine", Spine.layout spine),
			       ("time", Time.layout (!time))]]
	     | GenFlexRecord {fields, spine, ...} =>
		  seq [str "GenFlex ",
		       record [("fields", layoutFields fields),
			       ("spine", Spine.layout spine)]]
	     | Int => str "Int"
	     | Real => str "Real"
	     | Record r => Srecord.layout {record = r,
					   separator = ": ",
					   extra = "",
					   layoutTuple = Vector.layout layout,
					   layoutElt = layout}
	     | Unknown u => Unknown.layout u
	     | Var a => paren (seq [str "Var ", Tyvar.layout a])
	     | Word => str "Word"
      end

      val toString = Layout.toString o layout

      fun union (T s, T s') = Set.union (s, s')

      fun set (T s, v) = Set.setValue (s, v)
	 
      fun makeHom {con, flexRecord, genFlexRecord, int, real,
		   record, recursive, unknown, var, word} =
	 let
	    datatype status = Processing | Seen | Unseen
	    val {destroy = destroyStatus, get = status, ...} =
	       Property.destGet (plist, Property.initFun (fn _ => ref Unseen))
	    val {get, destroy = destroyProp} =
	       Property.destGet
	       (plist,
		Property.initRec
		(fn (t, get) =>
		 let
		    val r = status t
		 in
		    case !r of
		       Seen => Error.bug "impossible"
		     | Processing => recursive t
		     | Unseen =>
			  let
			     val _ = r := Processing
			     fun loopFields fields =
				List.revMap (fields, fn (f, t) => (f, get t))
			     val res = 
				case toType t of
				   Con (c, ts) =>
				      con (t, c, Vector.map (ts, get))
				 | Int => int t
				 | FlexRecord {fields, spine, time} =>
				      flexRecord (t, {fields = loopFields fields,
						      spine = spine,
						      time = time})
				 | GenFlexRecord {extra, fields, spine} =>
				      genFlexRecord
				      (t, {extra = extra,
					   fields = loopFields fields,
					   spine = spine})
				 | Real => real t
				 | Record r => record (t, Srecord.map (r, get))
				 | Unknown u => unknown (t, u)
				 | Var a => var (t, a)
				 | Word => word t
			     val _ = r := Seen
			  in
			     res
			  end
		 end))
	    fun destroy () =
	       (destroyStatus ()
		; destroyProp ())
	 in
	    {hom = get, destroy = destroy}
	 end

      fun hom (ty, z) =
	 let
	    val {hom, destroy} = makeHom z
	 in
	    hom ty before destroy ()
	 end

      fun layoutPretty (t: t): Layout.t =
	 let
	    val str = Layout.str
	    fun maybeParen (b, t) = if b then Layout.paren t else t
	    fun con (_, c, ts) = Tycon.layoutApp (c, ts)
	    fun int _ = simple (str "int")
	    fun flexRecord (_, {fields, spine, time}) =
	       layoutRecord
	       (List.fold
		(fields,
		 Spine.foldOverNew (spine, fields, [], fn (f, ac) =>
				    (f, simple (str "unit"))
				    :: ac),
		 fn ((f, t), ac) => (f, t) :: ac))
	    fun genFlexRecord (_, {extra, fields, spine}) =
	       layoutRecord
	       (List.fold
		(fields,
		 List.revMap (extra (), fn {field, tyvar} =>
			      (field, simple (Tyvar.layout tyvar))),
		 fn ((f, t), ac) => (f, t) :: ac))
	    fun real _ = simple (str "real")
	    fun record (_, r) =
	       case Srecord.detupleOpt r of
		  NONE => layoutRecord (Vector.toList (Srecord.toVector r))
		| SOME ts => Tycon.layoutApp (Tycon.tuple, ts)
	    fun recursive _ = simple (str "<recur>")
	    fun unknown (_, u) = simple (str "???")
	    fun var (_, a) = simple (Tyvar.layout a)
	    fun word _ = simple (str "word")
	 in
	    #1 (hom (t, {con = con,
			 flexRecord = flexRecord,
			 genFlexRecord = genFlexRecord,
			 int = int,
			 real = real,
			 record = record,
			 recursive = recursive,
			 unknown = unknown,
			 var = var,
			 word = word}))
	 end

      fun deConOpt t =
	 case toType t of
	    Con x => SOME x
	  | _ => NONE

      fun newTy (ty: ty): t =
	 T (Set.singleton {ty = ty,
			   plist = PropertyList.new ()})

      fun new z =
	 let
	    val t = newTy (Unknown (Unknown.new z))
	    val _ = List.push (freeUnknowns, t)
	 in
	    t
	 end

      fun flexRecord record =
	 let
	    val v = Srecord.toVector record
	    val spine = Spine.new (Vector.toListMap (v, #1))
	    fun isResolved (): bool = not (Spine.canAddFields spine)
	    val t =
	       newTy (FlexRecord {fields = Vector.toList v,
				  spine = spine,
				  time = ref (Time.now ())})
	    val _ = List.push (freeFlexes, t)
	 in
	    (t, isResolved)
	 end
	 
      val record = newTy o Record

      fun tuple ts =
	 if 1 = Vector.length ts
	    then Vector.sub (ts, 0)
	 else newTy (Record (Srecord.tuple ts))

      fun con (tycon, ts) =
	 if Tycon.equals (tycon, Tycon.tuple) then tuple ts
	 else newTy (Con (tycon, ts))

      val char = con (Tycon.char, Vector.new0 ())
      val string = con (Tycon.vector, Vector.new1 char)

      val var = newTy o Var
   end

structure Ops = TypeOps (structure IntSize = IntSize
			 structure Tycon = Tycon
			 structure WordSize = WordSize
			 open Type)

fun layoutTopLevel (t: Type.ty) =
   let
      val str = Layout.str
      datatype z = datatype Type.ty
   in
      case t of
	 Con (c, ts) =>
	    Tycon.layoutApp
	    (c, Vector.map (ts, fn t =>
			    if (case Type.toType t of
				   Con (c, _) => Tycon.equals (c, Tycon.char)
				 | _ => false)
			       then (str "_", {isChar = true,
					       needsParen = false})
			    else dontCare))
       | FlexRecord _ => simple (str "{_}")
       | GenFlexRecord _ => simple (str "{_}")
       | Int => simple (str "int")
       | Real => simple (str "real")
       | Record r =>
	    (case Srecord.detupleOpt r of
		NONE => simple (str "{_}")
	      | SOME ts => layoutTuple (Vector.map (ts, fn _ => dontCare)))
       | Unknown _ => Error.bug "layoutTopLevel Unknown"
       | Var a => simple (Tyvar.layout a)
       | Word => simple (str "word")
   end
   
structure Type =
   struct
      (* Order is important, since want specialized definitions in Type to
       * override general definitions in Ops.
       *)
      open Ops Type

      val char = con (Tycon.char, Vector.new0 ())
	 
      val unit = tuple (Vector.new0 ())

      fun isUnit t =
	 case toType t of
	    Record r =>
	       (case Srecord.detupleOpt r of
		   NONE => false
		 | SOME v => 0 = Vector.length v)
	  | _ => false

      val equals: t * t -> bool = fn (T s, T s') => Set.equals (s, s')

      local
	 fun make ty () = newTy ty
      in
	 val unresolvedInt = make Int
	 val unresolvedReal = make Real
	 val unresolvedWord = make Word
      end
   
      val traceCanUnify =
	 Trace.trace2 ("canUnify", layout, layout, Bool.layout)

      fun canUnify arg = 
	 traceCanUnify
	 (fn (t, t') =>
	  case (toType t, toType t') of
	     (Unknown _,  _) => true
	   | (_, Unknown _) => true
	   | (Con (c, ts), t') => conAnd (c, ts, t')
	   | (t', Con (c, ts)) => conAnd (c, ts, t')
	   | (Int, Int) => true
	   | (Real, Real) => true
	   | (Record r, Record r') =>
		let
		   val fs = Srecord.toVector r
		   val fs' = Srecord.toVector r'
		in Vector.length fs = Vector.length fs'
		   andalso Vector.forall2 (fs, fs', fn ((f, t), (f', t')) =>
					   Field.equals (f, f')
					   andalso canUnify (t, t'))
		end
	   | (Var a, Var a') => Tyvar.equals (a, a')
	   | (Word, Word) => true
	   | _ => false) arg
      and conAnd (c, ts, t') =
	 case t' of
	    Con (c', ts') =>
	       Tycon.equals (c, c')
	       andalso Vector.forall2 (ts, ts', canUnify)
	  | Int => 0 = Vector.length ts andalso Tycon.isIntX c
	  | Real => 0 = Vector.length ts andalso Tycon.isRealX c
	  | Word => 0 = Vector.length ts andalso Tycon.isWordX c
	  | _ => false

      fun minTime (t, time) =
	 let
	    fun doit r = r := Time.min (!r, time)
	    fun var (_, a) = doit (tyvarTime a)
	    val {destroy, hom} =
	       makeHom
	       {con = fn _ => (),
		flexRecord = fn (_, {time = r, ...}) => doit r,
		genFlexRecord = fn _ => (),
		int = fn _ => (),
		real = fn _ => (),
		record = fn _ => (),
		recursive = fn _ => (),
		unknown = fn (_, u) => Unknown.minTime (u, time),
		var = var,
		word = fn _ => ()}
	    val _ = hom t
	    val _ = destroy ()
	 in
	    ()
	 end

      structure Lay =
	 struct
	    type t = Layout.t * {isChar: bool, needsParen: bool}
	 end
      
      structure UnifyResult =
	 struct
	    datatype t =
	       NotUnifiable of Lay.t * Lay.t
	     | Unified

	    val layout =
	       let
		  open Layout
	       in
		  fn NotUnifiable _ => str "NotUnifiable"
		   | Unified => str "Unified"
	       end
	 end

      datatype z = datatype UnifyResult.t

      val traceUnify = Trace.trace2 ("unify", layout, layout, UnifyResult.layout)

      fun unify (t, t'): UnifyResult.t =
	 let
	    fun unify arg =
	       traceUnify
	       (fn (outer as T s, outer' as T s') =>
		if Set.equals (s, s')
		   then Unified
		else
		   let
		      fun notUnifiable (l: Lay.t, l': Lay.t) =
			 (NotUnifiable (l, l'),
			  Unknown (Unknown.new {canGeneralize = true,
						equality = true}))
		      fun oneFlex ({fields, spine, time}, r, outer, swap) =
			 let
			    val _ = minTime (outer, !time)
			    val differences =
			       List.fold
			       (fields, ([], []), fn ((f, t), (ac, ac')) =>
				case Srecord.peek (r, f) of
				   NONE => ((f, dontCare) :: ac, ac')
				 | SOME t' =>
				      case unify (t, t') of
					 NotUnifiable (l, l') =>
					    ((f, l) :: ac, (f, l') :: ac')
				       | Unified => (ac, ac'))
			    val differences =
			       List.fold
			       (Spine.fields spine, differences,
				fn (f, (ac, ac')) =>
				if List.exists (fields, fn (f', _) =>
						Field.equals (f, f'))
				   then (ac, ac')
				else
				   case Srecord.peek (r, f) of
				      NONE => ((f, dontCare) :: ac, ac')
				    | SOME _ => (ac, ac'))
			    val differences =
			       Srecord.foldi
			       (r, differences, fn (f, t, (ac, ac')) =>
				let
				   val ac' =
				      if Spine.ensureField (spine, f)
					 then ac'
				      else (f, dontCare) :: ac'
				in
				   case List.peek (fields, fn (f', _) =>
						   Field.equals (f, f')) of
				      NONE => (ac, ac')
				    | SOME (_, t') =>
					 case unify (t, t') of
					    NotUnifiable (l, l') =>
					       ((f, l') :: ac, (f, l) :: ac')
					  | Unified => (ac, ac')
				end)
			    val _ = Spine.noMoreFields spine
			 in
			    case differences of
			       ([], []) => (Unified, Record r)
			     | (ds, ds') =>
				  let
				     val ds = layoutRecord ds
				     val ds' = layoutRecord ds'
				  in
				     notUnifiable (if swap then (ds', ds)
						   else (ds, ds'))
				  end
			 end
		      fun genFlexError () =
			 Error.bug "GenFlexRecord seen in unify"
		      val {ty = t, plist} = Set.value s
		      val {ty = t', ...} = Set.value s'
		      fun not () =
			 notUnifiable (layoutTopLevel t, layoutTopLevel t')
		      fun conAnd (c, ts, t, t', swap) =
			 let
			    fun lay () = layoutTopLevel (Con (c, ts))
			    val notUnifiable =
			       fn (z, z') =>
			       notUnifiable (if swap then (z', z) else (z, z'))
			 in
			    case t of
			       Con (c', ts') =>
				  if Tycon.equals (c, c')
				     then
					if Vector.length ts <> Vector.length ts'
					   then
					      let
						 fun lay ts =
						    simple
						    (Layout.seq
						     [Layout.str
						      (concat ["<",
							       Int.toString
							       (Vector.length ts),
							       " args> "]),
						      Tycon.layout c])
					      in
						 notUnifiable (lay ts, lay ts')
					      end
					else
					   let
					      val us =
						 Vector.map2 (ts, ts', unify)
					   in
					      if Vector.forall
						 (us,
						  fn Unified => true
						   | _ => false)
						 then (Unified, t)
					      else
						 let
						    val (ls, ls') =
						       Vector.unzip
						       (Vector.map
							(us,
							 fn Unified =>
							    (dontCare,
							     dontCare)
							  | NotUnifiable (l, l') =>
							       (l, l')))
						    fun lay ls =
						       Tycon.layoutApp (c, ls)
						 in
						    notUnifiable (lay ls,
								  lay ls')
						 end
					   end
				  else not ()
			     | Int =>
				  if Tycon.isIntX c andalso Vector.isEmpty ts
				     then (Unified, t')
				  else not ()
			     | Real =>
				  if Tycon.isRealX c andalso Vector.isEmpty ts
				     then (Unified, t')
				  else not ()
			     | Word =>
				  if Tycon.isWordX c andalso Vector.isEmpty ts
				     then (Unified, t')
				  else not ()
			     | _ => not ()
			 end
		      fun oneUnknown (u, t, outer) =
			 let
			    val _ = minTime (outer, Unknown.time u)
			 in
			    (Unified, t)
			 end
		      val (res, t) =
			 case (t, t') of
			    (Unknown r, Unknown r') =>
			       (Unified, Unknown (Unknown.join (r, r')))
			  | (_, Unknown u) => oneUnknown (u, t, outer)
			  | (Unknown u, _) => oneUnknown (u, t', outer')
			  | (Con (c, ts), _) => conAnd (c, ts, t', t, false)
			  | (_, Con (c, ts)) => conAnd (c, ts, t, t', true)
			  | (FlexRecord f, Record r) =>
			       oneFlex (f, r, outer', false)
			  | (Record r, FlexRecord f) =>
			       oneFlex (f, r, outer, true)
			  | (FlexRecord {fields = fields, spine = s, time = t},
			     FlexRecord {fields = fields', spine = s',
					 time = t', ...}) =>
			       let
				  fun subsetSpine (fields, spine, spine') =
				     List.fold
				     (Spine.fields spine, [], fn (f, ac) =>
				      if List.exists (fields, fn (f', _) =>
						      Field.equals (f, f'))
					 orelse Spine.ensureField (spine', f)
					 then ac
				      else (f, dontCare) :: ac)
				  val ac = subsetSpine (fields, s, s')
				  val ac' = subsetSpine (fields', s', s)
				  fun subset (fields, fields', spine', ac, ac') =
				     List.fold
				     (fields, (ac, ac'),
				      fn ((f, t), (ac, ac')) =>
				      case List.peek (fields', fn (f', _) =>
						      Field.equals (f, f')) of
					 NONE =>
					    if Spine.ensureField (spine', f)
					       then (ac, ac')
					    else ((f, dontCare) :: ac, ac')
				       | SOME (_, t') =>
					    case unify (t, t') of
					       NotUnifiable (l, l') =>
						  ((f, l) :: ac, (f, l) :: ac')
					     | Unified => (ac, ac'))
				  val (ac, ac') =
				     subset (fields, fields', s', ac, ac')
				  val (ac, ac') =
				     subset (fields', fields, s, [], [])
				  val _ = Spine.unify (s, s')
				  val fields =
				     List.fold
				     (fields, fields', fn ((f, t), ac) =>
				      if List.exists (fields', fn (f', _) =>
						      Field.equals (f, f'))
					 then ac
				      else (f, t) :: ac)
			       in
				  case (ac, ac') of
				     ([], []) =>
					(Unified,
					 FlexRecord
					 {fields = fields,
					  spine = s,
					  time = ref (Time.min (!t, !t'))})
				   | _ =>
					notUnifiable (layoutRecord ac,
						      layoutRecord ac')
			       end
			  | (GenFlexRecord _, _) => genFlexError ()
			  | (_, GenFlexRecord _) => genFlexError ()
			  | (Int, Int) => (Unified, Int)
			  | (Real, Real) => (Unified, Real)
			  | (Record r, Record r') =>
			       (case (Srecord.detupleOpt r,
				      Srecord.detupleOpt r') of
				   (NONE, NONE) =>
				      let
					 fun diffs (r, r', ac, ac') =
					    Vector.fold
					    (Srecord.toVector r, (ac, ac'),
					     fn ((f, t), (ac, ac')) =>
					     case Srecord.peek (r', f) of
						NONE =>
						   ((f, dontCare) :: ac, ac')
					      | SOME t' =>
						   case unify (t, t') of
						      NotUnifiable (l, l') =>
							 ((f, l) :: ac,
							  (f, l') :: ac')
						    | Unified => (ac, ac'))
					 val (ac, ac') = diffs (r, r', [], [])
					 val (ac', ac) = diffs (r', r, ac', ac)
				      in
					 case (ac, ac') of
					    ([], []) =>
					       (Unified, Record r)
					  | _ =>
					       notUnifiable (layoutRecord ac,
							     layoutRecord ac')
				      end
				 | (SOME ts, SOME ts') =>
				      if Vector.length ts = Vector.length ts'
					 then
					    let
					       val us =
						  Vector.map2 (ts, ts', unify)
					    in
					       if Vector.forall
						  (us,
						   fn Unified => true
						    | _ => false)
						  then (Unified, Record r)
					       else
						  let
						     val (ls, ls') =
							Vector.unzip
							(Vector.map
							 (us,
							  fn Unified =>
							        (dontCare,
								 dontCare)
							   | NotUnifiable (l, l') =>
								(l, l')))
						  in
						     notUnifiable
						     (layoutTuple ls,
						      layoutTuple ls')
						  end
					    end
				      else not ()
				 | _ => not ())
			  | (Var a, Var a') =>
			       if Tyvar.equals (a, a')
				  then (Unified, t)
			       else not ()
			  | (Word, Word) => (Unified, Word)
			  | _ => not ()
		      val _ =
			 case res of
			    NotUnifiable _ => ()
			  | Unified =>
			       (Set.union (s, s')
				;  Set.setValue (s, {ty = t, plist = plist}))
		   in
		      res
		   end) arg
	 in
	    unify (t, t')
	 end

      structure UnifyResult' =
	 struct
	    datatype t =
	       NotUnifiable of Layout.t * Layout.t
	     | Unified

	    val layout =
	       let
		  open Layout
	       in
		  fn NotUnifiable _ => str "NotUnifiable"
		   | Unified => str "Unified"
	       end
	 end

      datatype unifyResult = datatype UnifyResult'.t

      val unify =
	 fn (t, t') =>
	 case unify (t, t') of
	    UnifyResult.NotUnifiable ((l, _), (l', _)) => NotUnifiable (l, l')
	  | UnifyResult.Unified => Unified

      val word8 = word WordSize.W8
	 
      fun 'a simpleHom {con: t * Tycon.t * 'a vector -> 'a,
			record: t * (Field.t * 'a) vector -> 'a,
			var: t * Tyvar.t -> 'a} =
	 let
	    val con =
	       fn (t, c, ts) =>
	       if Tycon.equals (c, Tycon.char)
		  then con (word8, Tycon.word WordSize.W8, Vector.new0 ())
	       else con (t, c, ts)
	    val unit = con (unit, Tycon.tuple, Vector.new0 ())
	    val unknown = unit
	    fun sortFields (fields: (Field.t * 'a) list) =
	       Array.toVector
	       (QuickSort.sortArray
		(Array.fromList fields, fn ((f, _), (f', _)) =>
		 Field.<= (f, f')))
	    fun unsorted (t, fields: (Field.t *  'a) list) =
	       let
		  val v = sortFields fields
	       in
		  record (t, v)
	       end
	    fun genFlexRecord (t, {extra, fields, spine}) =
	       unsorted (t,
			 List.fold
			 (extra (), fields, fn ({field, tyvar}, ac) =>
			  (field, var (Type.var tyvar, tyvar)) :: ac))
	    fun flexRecord (t, {fields, spine, time}) =
	       if Spine.canAddFields spine
		  then Error.bug "Type.hom flexRecord"
	       else unsorted (t,
			      Spine.foldOverNew
			      (spine, fields, fields, fn (f, ac) =>
			       (f, unit) :: ac))
	    fun recursive t = Error.bug "Type.hom recursive"
	    val int =
	       con (int IntSize.default, Tycon.defaultInt, Vector.new0 ())
	    val real =
	       con (real RealSize.default, Tycon.defaultReal, Vector.new0 ())
	    val word =
	       con (word WordSize.default, Tycon.defaultWord, Vector.new0 ())
	    val {hom: t -> 'a, ...} =
	       makeHom {con = con,
			int = fn _ => int,
			flexRecord = flexRecord,
			genFlexRecord = genFlexRecord,
			real = fn _ => real,
			record = fn (t, r) => record (t, Srecord.toVector r),
			recursive = recursive,
			unknown = fn _ => unknown,
			var = var,
			word = fn _ => word}
	 in
	    hom
	 end
   end

structure Scheme =
   struct
      datatype t =
	 General of {bound: unit -> Tyvar.t vector,
		     canGeneralize: bool,
		     flexes: Type.genFlexRecord list,
		     tyvars: Tyvar.t vector,
		     ty: Type.t}
       | Type of Type.t
      
      fun layout s =
	 case s of
	    Type t => Type.layout t
	  | General {canGeneralize, tyvars, ty, ...} =>
	       Layout.record [("canGeneralize", Bool.layout canGeneralize),
			      ("tyvars", Vector.layout Tyvar.layout tyvars),
			      ("ty", Type.layout ty)]

      fun layoutPretty s =
	 case s of
	    Type t => Type.layoutPretty t
	  | General {ty, ...} => Type.layoutPretty ty

      val tyvars =
	 fn General {tyvars, ...} => tyvars
	  | Type _ => Vector.new0 ()
	 
      val bound =
	 fn General {bound, ...} => bound ()
	  | Type _ => Vector.new0 ()

      val bound =
	 Trace.trace ("Scheme.bound", layout, Vector.layout Tyvar.layout)
	 bound

      val ty =
	 fn General {ty, ...} => ty
	  | Type ty => ty

      fun make {canGeneralize, tyvars, ty} =
	 if 0 = Vector.length tyvars
	    then Type ty
	 else General {bound = fn () => tyvars,
		       canGeneralize = canGeneralize,
		       flexes = [],
		       tyvars = tyvars,
		       ty = ty}

      val fromType = Type

      fun instantiate (t: t, subst) =
	 case t of
	    Type ty => {args = fn () => Vector.new0 (),
			instance = ty}
	  | General {canGeneralize, flexes, tyvars, ty, ...} =>
	       let
		  open Type
		  val {destroy = destroyTyvarInst,
		       get = tyvarInst: Tyvar.t -> Type.t option,
		       set = setTyvarInst} =
		     Property.destGetSetOnce (Tyvar.plist,
					      Property.initConst NONE)
		  val types =
		     Vector.mapi
		     (tyvars, fn (i, a) =>
		      let
			 val t = subst {canGeneralize = canGeneralize,
					equality = Tyvar.isEquality a,
					index = i}
			 val _ = setTyvarInst (a, SOME t)
		      in
			 t
		      end)
		  type z = {isNew: bool, ty: Type.t}
		  fun isNew {isNew = b, ty} = b
		  fun keep ty = {isNew = false, ty = ty}
		  fun con (ty, c, zs) =
		     if Vector.exists (zs, isNew)
			then {isNew = true,
			      ty = newTy (Con (c, Vector.map (zs, #ty)))}
		     else keep ty
		  val flexInsts = ref []
		  fun genFlexRecord (t, {extra, fields, spine}) =
		     let
			val fields = List.revMap (fields, fn (f, t: z) =>
						  (f, #ty t))
			val flex = newTy (FlexRecord {fields = fields,
						      spine = spine,
						      time = ref (Time.now ())})
			val _ = List.push (flexInsts, {spine = spine,
						       flex = flex})
		     in
			{isNew = true,
			 ty = flex}
		     end
		  fun record (t, r) =
		     if Srecord.exists (r, isNew)
			then {isNew = true,
			      ty = newTy (Record (Srecord.map (r, #ty)))}
		     else keep t
		  fun recursive t =
		     if true
			then Error.bug "instantiating recursive type"
		     else
			{isNew = true,
			 ty = new {canGeneralize = true,
				   equality = true}}
		  fun var (ty, a) =
		     case tyvarInst a of
			NONE => {isNew = false, ty = ty}
		      | SOME ty => {isNew = true, ty = ty}
		  val {ty: Type.t, ...} =
		     Type.hom (ty, {con = con,
				    int = keep,
				    flexRecord = keep o #1,
				    genFlexRecord = genFlexRecord,
				    real = keep,
				    record = record,
				    recursive = recursive,
				    unknown = keep o #1,
				    var = var,
				    word = keep})
		  val _ = destroyTyvarInst ()
		  val flexInsts = !flexInsts
		  fun args (): Type.t vector =
		     Vector.fromList
		     (List.fold
		      (flexes, Vector.toList types,
		       fn ({fields, spine, ...}, ac) =>
		       let
			  val flex =
			     case List.peek (flexInsts,
					     fn {spine = spine', ...} =>
					     Spine.equals (spine, spine')) of
				NONE => Error.bug "missing flexInst"
			      | SOME {flex, ...} => flex
			  fun peekFields (fields, f) =
			     Option.map
			     (List.peek (fields, fn (f', _) =>
					 Field.equals (f, f')),
			      #2)
			  val peek =
			     case Type.toType flex of
				FlexRecord {fields, ...} =>
				   (fn f => peekFields (fields, f))
			      | GenFlexRecord {extra, fields, ...} =>
				   (fn f =>
				    case peekFields (fields, f) of
				       NONE =>
					  Option.map
					  (List.peek
					   (extra (), fn {field, ...} =>
					    Field.equals (f, field)),
					   Type.var o #tyvar)
				     | SOME t => SOME t)
			      | Record r => (fn f => Srecord.peek (r, f))
			      | _ => Error.bug "strange flexInst"
		       in
			  Spine.foldOverNew
			  (spine, fields, ac, fn (f, ac) =>
			   (case peek f of
			       NONE => Type.unit
			     | SOME t => t) :: ac)
		       end))
	       in
		  {args = args,
		   instance = ty}
	       end

      fun apply (s, ts) =
	 #instance (instantiate (s, fn {index, ...} => Vector.sub (ts, index)))
	    			    
      val instantiate =
	 fn s =>
	 instantiate (s, fn {canGeneralize, equality, ...} =>
		      Type.new {canGeneralize = canGeneralize,
				equality = equality})
				
      val instantiate =
	 Trace.trace ("Scheme.instantiate", layout, Type.layout o #instance)
	 instantiate
	 
      fun haveFrees (v: t vector): bool vector =
	 let
	    exception Yes
	    val {destroy, hom} =
	       Type.makeHom {con = fn _ => (),
			     flexRecord = fn _ => (),
			     genFlexRecord = fn _ => (),
			     int = fn _ => (),
			     real = fn _ => (),
			     record = fn _ => (),
			     recursive = fn _ => (),
			     unknown = fn _ => raise Yes,
			     var = fn _ => (),
			     word = fn _ => ()}
	    val res =
	       Vector.map (v, fn s =>
			   let
			      val _ =
				 case s of
				    General {ty, ...} => hom ty
				  | Type ty => hom ty
			   in
			      false
			   end handle Yes => true)
	    val _ = destroy ()
	 in
	    res
	 end
   end

fun close (ensure: Tyvar.t vector, region)
   : Type.t vector -> {bound: unit -> Tyvar.t vector,
		       schemes: Scheme.t vector} =
   let
      val genTime = Time.tick ()
      val _ = Vector.foreach (ensure, fn a => (tyvarTime a; ()))
   in
      fn tys =>
      let
	 val unable =
	    Vector.keepAll (ensure, fn a =>
			    not (Time.<= (genTime, !(tyvarTime a))))
	 val _ = 
	    if Vector.length unable > 0
	       then
		  let
		     open Layout
		  in
		     Control.error
		     (region,
		      seq [str "unable to generalize ",
			   seq (List.separate (Vector.toListMap (unable,
								 Tyvar.layout),
					       str ", "))],
		      empty)
		  end
	    else ()
	 (* Convert all the unknown types bound at this level into tyvars. *)
	 val (tyvars, ac) =
	    List.fold
	    (!Type.freeUnknowns, (Vector.toList ensure, []),
	     fn (t, (tyvars, ac)) =>
	     case Type.toType t of
		Type.Unknown (Unknown.T {canGeneralize, equality, time, ...}) =>
		   if canGeneralize andalso Time.<= (genTime, !time)
		      then
			 let
			    val a = Tyvar.newNoname {equality = equality}
			    val _ = Type.set (t, {ty = Type.Var a,
						  plist = PropertyList.new ()})
			 in
			    (a :: tyvars, ac)
			 end
		   else (tyvars, t :: ac)
	      | _ => (tyvars, ac))
	 val _ = Type.freeUnknowns := ac
	 (* Convert all the FlexRecords bound at this level into GenFlexRecords.
	  *)
	 val (flexes, ac) =
	    List.fold
	    (!Type.freeFlexes, ([], []), fn (t as Type.T s, (flexes, ac)) =>
	     let
		val {ty, plist} = Set.value s
	     in
		case ty of
		   Type.FlexRecord {fields, spine, time, ...} =>
		      if Time.<= (genTime, !time)
			 then
			    let
			       val extra =
				  Promise.lazy
				  (fn () =>
				   Spine.foldOverNew
				   (spine, fields, [], fn (f, ac) =>
				    {field = f,
				     tyvar = Tyvar.newNoname {equality = false}}
				    :: ac))
			       val gfr = {extra = extra,
					  fields = fields,
					  spine = spine}
			       val _ = 
				  Set.setValue
				  (s, {plist = plist,
				       ty = Type.GenFlexRecord gfr})
			    in
			       (gfr :: flexes, ac)
			    end
		      else (flexes, t :: ac)
                  | _ => (flexes, ac)
	     end)
	 val _ = Type.freeFlexes := ac
	 (* For all fields that were added to the generalized flex records, add
	  * a type variable.
	  *)
	 fun bound () =
	    Vector.fromList
	    (List.fold
	     (flexes, tyvars, fn ({extra, fields, spine}, ac) =>
	      let
		 val extra = extra ()
	      in
		 Spine.foldOverNew
		 (spine, fields, ac, fn (f, ac) =>
		  case List.peek (extra, fn {field, ...} =>
				  Field.equals (f, field)) of
		     NONE => Error.bug "GenFlex missing field"
		   | SOME {tyvar, ...} => tyvar :: ac)
	      end))
	 val schemes =
	    Vector.map
	    (tys, fn ty =>
	     Scheme.General {bound = bound,
			     canGeneralize = true,
			     flexes = flexes,
			     tyvars = Vector.fromList tyvars,
			     ty = ty})
      in
	 {bound = bound,
	  schemes = schemes}
      end
   end

fun closeTop (r: Region.t): unit =
   let
      val _ =
	 List.foreach
	 (!Type.freeUnknowns, fn t =>
	  case Type.toType t of
	     Type.Unknown _ => (Type.unify (t, Type.unit)
				; ())
	   | _ => ())
      val _ = Type.freeUnknowns := []
      val _ = List.foreach (!Type.freeFlexes, fn t =>
			    case Type.toType t of
 			       Type.FlexRecord _ => Error.bug "free flex\n"
			     | _ => ())
      val _ = Type.freeFlexes := []
   in
      ()
   end

structure Type =
   struct
      open Type

      fun homConVar {con, var} =
	 let
	    fun tuple (t, ts) =
	       if 1 = Vector.length ts
		  then Vector.sub (ts, 0)
	       else con (t, Tycon.tuple, ts)
	 in
	    simpleHom {con = con,
		       record = fn (t, fs) => tuple (t, Vector.map (fs, #2)),
		       var = var}
	 end

      fun hom {con, var} =
	 homConVar {con = fn (_, c, ts) => con (c, ts),
		    var = fn (_, a) => var a}
	 
      fun deRecord t =
	 let
	    val hom =
	       simpleHom
	       {con = fn (t, _, _) => (t, NONE),
		record = fn (t, fs) => (t,
					SOME (Vector.map (fs, fn (f, (t, _)) =>
							  (f, t)))),
		var = fn (t, _) => (t, NONE)}
	 in
	    case #2 (hom t) of
	       NONE => Error.bug "Type.deRecord"
	     | SOME fs => fs
	 end

      fun deTupleOpt t =
	 let
	    val hom =
	       homConVar
	       {con = fn (t, c, ts) => (t,
					if Tycon.equals (c, Tycon.tuple)
					   then SOME (Vector.map (ts, #1))
					else NONE),
                var = fn (t, _) => (t, NONE)}
	 in
	    #2 (hom t)
	 end

      val deTupleOpt =
	 Trace.trace ("Type.deTupleOpt", layout,
		      Option.layout (Vector.layout layout))
	 deTupleOpt

      val deTuple = valOf o deTupleOpt
   end
end
