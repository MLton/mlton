(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor Interface (S: INTERFACE_STRUCTS): INTERFACE =
struct

open S

local
   open Ast
in
   structure Longstrid = Longstrid
   structure Longtycon = Longtycon
   structure Record = SortedRecord
   structure Strid = Strid
   structure Symbol = Symbol
   structure Tyvar = Tyvar
   structure Vid = Vid
end

structure Field = Record.Field

structure EtypeStr = EnvTypeStr
local
   open EtypeStr
in
   structure AdmitsEquality = AdmitsEquality
   structure Con = Con
   structure Econs = Cons
   structure Kind = Kind
   structure Escheme = Scheme
   structure Etycon = Tycon
   structure Etype = Type
end

structure Set = DisjointSet

structure Shape =
   struct
      datatype t = T of {plist: PropertyList.t}

      local
	 fun make f (T r) = f r
      in
	 val plist = make #plist
      end

      fun layout (T _) = Layout.str "<shape>"

      fun new () = T {plist = PropertyList.new ()}

      fun equals (s, s') = PropertyList.equals (plist s, plist s')
   end

structure Status:
   sig
      datatype t = Con | Exn | Var
	 
      val layout: t -> Layout.t
      val toString: t -> string
   end =
   struct
      datatype t = Con | Exn | Var

      val toString =
	 fn Con => "Con"
	  | Exn => "Exn"
	  | Var => "Var"

      val layout = Layout.str o toString
   end

(* only needed for debugging *)
structure TyconId = IntUniqueId()

structure Defn =
   struct
      type t = exn
   end

structure Time:>
   sig
      type t

      val < : t * t -> bool
      val current: unit -> t
      val layout: t -> Layout.t
      val min: t * t -> t
      val tick: unit -> t
   end =
   struct
      type t = int

      val op < = Int.<
	 
      val layout = Int.layout

      val min = Int.min

      val currentTime: int ref = ref 0

      fun current () = !currentTime

      fun tick () =
	 let
	    val n = 1 + !currentTime
	    val _ = currentTime := n
	 in
	    n
	 end
   end

structure FlexibleTycon =
   struct
      (* hasCons is true if this tycon occurs in any type structure where the
       * cons are nonempty.  This allows us to do a quick check of the side
       * condition on rule 64 that requires all type structures to be well-formed
       * when implementing "where type". 
       *)
      datatype t = T of {admitsEquality: AdmitsEquality.t ref,
			 copy: copy,
			 creationTime: Time.t,
			 defn: exn ref,
			 hasCons: bool,
			 id: TyconId.t} Set.t
      withtype copy = t option ref

      fun dest (T s) = Set.value s

      local
	 fun make f = f o dest
      in
	 val defn = ! o make #defn
      end

      fun admitsEquality t = #admitsEquality (dest t)

      val equals = fn (T s, T s') => Set.equals (s, s')

      fun layout (T s) =
	 let
	    open Layout
	    val {admitsEquality, creationTime, hasCons, id, ...} = Set.value s
	 in
	    record [("admitsEquality", AdmitsEquality.layout (!admitsEquality)),
		    ("creationTime", Time.layout creationTime),
		    ("hasCons", Bool.layout hasCons),
		    ("id", TyconId.layout id)]
	 end

      fun layoutApp (t, v) = (layout t, {isChar = false, needsParen = false})

      val copies: copy list ref = ref []
	 
      fun new {defn: Defn.t, hasCons: bool}: t =
	 T (Set.singleton {admitsEquality = ref AdmitsEquality.Sometimes,
			   copy = ref NONE,
			   creationTime = Time.current (),
			   defn = ref defn,
			   hasCons = hasCons,
			   id = TyconId.new ()})
   end

structure Tycon =
   struct
      structure AdmitsEquality = AdmitsEquality

      datatype t =
	 Flexible of FlexibleTycon.t
       | Rigid of Etycon.t * Kind.t

      val fromEnv: Etycon.t * Kind.t -> t = Rigid

      fun admitsEquality c =
	 case c of
	    Flexible f => FlexibleTycon.admitsEquality f
	  | Rigid (e, _) => Etycon.admitsEquality e

      val arrow = fromEnv (Etycon.arrow, Kind.Arity 2)

      val equals =
	 fn (Flexible f, Flexible f') => FlexibleTycon.equals (f, f')
	  | (Rigid (c, _), Rigid (c', _)) => Etycon.equals (c, c')
	  | _ => false

      val exn = Rigid (Etycon.exn, Kind.Arity 0)

      val layout =
	 fn Flexible c => FlexibleTycon.layout c
	  | Rigid (c, _) => Etycon.layout c

      fun layoutApp (t: t, v) =
	 case t of
	    Flexible f => FlexibleTycon.layoutApp (f, v)
	  | Rigid (c, _) => Etycon.layoutApp (c, v)

      val tuple = Rigid (Etycon.tuple, Kind.Nary)
   end

structure Type =
   struct
      datatype t =
	 Con of Tycon.t * t vector
       | Record of t Record.t
       | Var of Tyvar.t

      fun arrow (t1, t2) = Con (Tycon.arrow, Vector.new2 (t1, t2))

      val bogus = Con (Tycon.exn, Vector.new0 ())	 

      val con = Con

      fun deArrowOpt (t: t): (t * t) option =
	 case t of
	    Con (c, ts) =>
	       if Tycon.equals (c, Tycon.arrow)
		  then SOME (Vector.sub (ts, 0), Vector.sub (ts, 1))
	       else NONE
	  | _ => NONE

      fun deArrow t =
	 case deArrowOpt t of
	    NONE => Error.bug "Type.deArrow"
	  | SOME z => z

      fun deEta (t: t, tyvars: Tyvar.t vector): Tycon.t option =
	 case t of
	    Con (c, ts) =>
	       if Vector.length ts = Vector.length tyvars
		  andalso Vector.foralli (ts, fn (i, t) =>
					  case t of
					     Var a =>
						Tyvar.equals
						(a, Vector.sub (tyvars, i))
					   | _ => false)
		  then SOME c
	       else NONE
           | _ => NONE

      val exn = Con (Tycon.exn, Vector.new0 ())

      fun hom (t, {con, record, var}) =
	 let
	    val rec loop =
	       fn Con (c, ts) => con (c, Vector.map (ts, loop))
		| Record r => record (Record.map (r, loop))
		| Var a => var a
	 in
	    loop t
	 end
	       
      local
	 open Layout
	 fun simple l = (l, {isChar = false, needsParen = false})
	 fun loop t =
	    case t of
	       Con (c, ts) => Tycon.layoutApp (c, Vector.map (ts, loop))
	     | Record r =>
		  (case Record.detupleOpt r of
		      NONE =>
			 simple
			 (seq
			  [str "{",
			   mayAlign
			   (separateRight
			    (Vector.toListMap
			     (QuickSort.sortVector
			      (Record.toVector r, fn ((f, _), (f', _)) =>
			       Field.<= (f, f')),
			      fn (f, t) =>
			      seq [Field.layout f, str ": ", #1 (loop t)]),
			     ",")),
			   str "}"])
		    | SOME ts => Tycon.layoutApp (Tycon.tuple,
						  Vector.map (ts, loop)))
	     | Var a => simple (Tyvar.layout a)
      in
	 val layout = #1 o loop
      end

      val record = Record

      fun substitute (t: t, sub: (Tyvar.t * t) vector): t =
	 let
	    fun var a =
	       case Vector.peek (sub, fn (a', _) => Tyvar.equals (a, a')) of
		  NONE => Error.bug "substitute"
		| SOME (_, t) => t
	 in
	    hom (t, {con = Con,
		     record = Record,
		     var = var})
	 end

      val var = Var
   end

structure Scheme = GenericScheme (structure Type = Type
				  structure Tyvar = Tyvar)

structure Scheme =
   struct
      open Scheme

      fun admitsEquality _ = raise Fail "Interface.Scheme.admitsEquality"

      fun bogus () = T {ty = Type.bogus, tyvars = Vector.new0 ()}

      fun dest (T {ty, tyvars}) = (tyvars, ty)
	 
      fun make (tyvars, ty) = T {ty = ty, tyvars = tyvars}
   end

structure TypeStr = TypeStr (structure AdmitsEquality = AdmitsEquality
			     structure Con = Con
			     structure Kind = Kind
			     structure Name = Ast.Con
			     structure Record = Record
			     structure Scheme = Scheme
			     structure Tycon = Tycon
			     structure Type = Type
			     structure Tyvar = Tyvar)

structure Cons = TypeStr.Cons
   
structure Defn =
   struct
      open Defn

      datatype dest =
	 Realized of EtypeStr.t
       | TypeStr of TypeStr.t
       | Undefined

      exception U of dest

      val realized = U o Realized
      val typeStr = U o TypeStr
      val undefined = U Undefined

      fun dest (d: t): dest =
	 case d of
	    U u => u
	  | _ => Error.bug "Defn.dest"
   end

(* expandTy expands all type definitions in ty *)
local
   fun con (c, ts) =
      case c of
	 Tycon.Flexible f =>
	    (case Defn.dest (FlexibleTycon.defn f) of
		Defn.Realized _ => Error.bug "expandTy saw Realized"
	      | Defn.TypeStr s => expandTy (TypeStr.apply (s, ts))
	      | Defn.Undefined => Type.Con (c, ts))
       | Tycon.Rigid _ => Type.Con (c, ts)
   and expandTy (ty: Type.t): Type.t =
      Type.hom (ty, {con = con,
		     record = Type.Record,
		     var = Type.Var})
in
   val expandTy = expandTy
end

fun copyCons (Cons.T v): Cons.t =
   Cons.T (Vector.map (v, fn {con, name, scheme} =>
		       {con = con,
			name = name,
			scheme = copyScheme scheme}))
and copyDefn (d: Defn.t): Defn.t =
   let
      open Defn
   in
      case dest d of
	 Realized _ => Error.bug "copyDefn Realized"
       | TypeStr s => Defn.typeStr (copyTypeStr s)
       | Undefined => Defn.undefined
   end
and copyFlexibleTycon (FlexibleTycon.T s): FlexibleTycon.t =
   let
      open FlexibleTycon
      val {admitsEquality = a, copy, defn, hasCons, ...} = Set.value s
   in
      case !copy of
	 NONE => 
	    let
	       val c = new {defn = copyDefn (!defn), hasCons = hasCons}
	       val _ = admitsEquality c := !a
	       val _ = List.push (copies, copy)
	       val _ = copy := SOME c
	    in
	       c
	    end
       | SOME c => c
   end
and copyTycon (t: Tycon.t): Tycon.t =
   let
      open Tycon
   in
      case t of
	 Flexible c => Flexible (copyFlexibleTycon c)
       | Rigid _ => t
   end
and copyType (t: Type.t): Type.t =
   let
      open Type
   in
      hom (t, {con = fn (c, ts) => Con (copyTycon c, ts),
	       record = Record,
	       var = Var})
   end
and copyScheme (Scheme.T {tyvars, ty}): Scheme.t =
   Scheme.T {ty = copyType ty, tyvars = tyvars}
and copyTypeStr (s: TypeStr.t): TypeStr.t =
   let
      open TypeStr
      val kind = kind s
   in
      case node s of
	 Datatype {cons, tycon} => data (copyTycon tycon, kind, copyCons cons)
       | Scheme s => def (copyScheme s, kind)
       | Tycon c => tycon (copyTycon c, kind)
   end

fun flexibleTyconToEnv (c: FlexibleTycon.t): EtypeStr.t =
   let
      open FlexibleTycon
   in
      case Defn.dest (defn c) of
	 Defn.Realized s => s
       | Defn.TypeStr s => typeStrToEnv s
       | _ => Error.bug "FlexiblTycon.toEnv"
   end
and tyconToEnv (t: Tycon.t): EtypeStr.t =
   let
      open Tycon
   in
      case t of
	 Flexible c => flexibleTyconToEnv c
       | Rigid (c, k) => EtypeStr.tycon (c, k)
   end
and typeToEnv (t: Type.t): Etype.t =
   Type.hom (t, {con = fn (c, ts) => EtypeStr.apply (tyconToEnv c, ts),
		 record = Etype.record,
		 var = Etype.var})
and schemeToEnv (Scheme.T {ty, tyvars}): Escheme.t =
   Escheme.make (tyvars, typeToEnv ty)
and consToEnv (Cons.T v): Econs.t =
   Econs.T (Vector.map (v, fn {con, name, scheme} =>
			{con = con,
			 name = name,
			 scheme = schemeToEnv scheme}))
and typeStrToEnv (s: TypeStr.t): EtypeStr.t =
   let
      val k = TypeStr.kind s
      datatype z = datatype TypeStr.node
   in
      case TypeStr.node s of
	 Datatype {cons, tycon} =>
	    let
	       val tycon: Etycon.t =
		  case tycon of
		     Tycon.Flexible c =>
			let
			   val typeStr = flexibleTyconToEnv c
			in
			   case EtypeStr.node typeStr of
			      EtypeStr.Datatype {tycon, ...} => tycon
			    | EtypeStr.Tycon c => c
			    | _ =>
				 let
				    open Layout
				 in
				    Error.bug
				    (toString
				     (seq [str "datatype ",
					   TypeStr.layout s,
					   str " realized with scheme ",
					   EtypeStr.layout typeStr]))
				 end
			end
		   | Tycon.Rigid (c, _) => c
	    in
	       EtypeStr.data (tycon, k, consToEnv cons)
	    end
       | Scheme s => EtypeStr.def (schemeToEnv s, k)
       | Tycon c => EtypeStr.abs (tyconToEnv c)
   end

structure AdmitsEquality =
   struct
      open AdmitsEquality

      fun fromBool b = if b then Sometimes else Never
	 
      fun toBool a = 
	 case a of
		  Always => true
		| Never => false
		| Sometimes => true
   end

fun flexibleTyconAdmitsEquality (FlexibleTycon.T s): AdmitsEquality.t =
   let
      val {admitsEquality, defn, ...} = Set.value s
      datatype z = datatype Defn.dest
   in
      case Defn.dest (!defn) of
	 Realized _ => Error.bug "flexibleTyconAdmitsEquality Realized"
       | TypeStr s => typeStrAdmitsEquality s
       | Undefined => !admitsEquality
   end
and schemeAdmitsEquality (s: Scheme.t): bool =
   let
      fun con (c, bs) =
	 let
	    datatype z = datatype AdmitsEquality.t
	 in
	    case ! (Tycon.admitsEquality c) of
	       Always => true
	     | Never => false
	     | Sometimes => Vector.forall (bs, fn b => b)
	 end
   in
      Type.hom (expandTy (Scheme.ty s),
		{con = con,
		 record = fn r => Record.forall (r, fn b => b),
		 var = fn _ => true})
   end
and tyconAdmitsEquality (t: Tycon.t): AdmitsEquality.t =
   let
      datatype z = datatype Tycon.t
   in
      case t of
	 Flexible c => flexibleTyconAdmitsEquality c
       | Rigid (e, _) => ! (Etycon.admitsEquality e)
   end
and typeStrAdmitsEquality (s: TypeStr.t): AdmitsEquality.t =
   let
      datatype z = datatype TypeStr.node
   in
      case TypeStr.node s of
	 Datatype {tycon = c, ...} => tyconAdmitsEquality c
       | Scheme s => AdmitsEquality.fromBool (schemeAdmitsEquality s)
       | Tycon c => tyconAdmitsEquality c
   end

structure FlexibleTycon =
   struct
      open FlexibleTycon

      fun realize (T s, e: EtypeStr.t): unit =
	 let
 	    val {defn, ...} = Set.value s
	 in
	    defn := Defn.realized e
	 end

      val bogus = new {defn = Defn.undefined, hasCons = false}

      fun share (T s, T s') =
	 let
	    val {admitsEquality = a, creationTime = t, hasCons = h, id, ...} =
	       Set.value s
	    val {admitsEquality = a', creationTime = t', hasCons = h', ...} =
	       Set.value s'
	    val _ = Set.union (s, s')
	    val _ = 
	       Set.setValue
	       (s, {admitsEquality = ref (AdmitsEquality.or (!a, !a')),
		    copy = ref NONE,
		    creationTime = Time.min (t, t'),
		    defn = ref Defn.undefined,
		    hasCons = h orelse h',
		    id = id})
	 in
	    ()
	 end
   end

structure Tycon =
   struct
      open Tycon

      fun make {hasCons} =
	 Flexible (FlexibleTycon.new {defn = Defn.undefined,
				      hasCons = hasCons})

      val exn = fromEnv (Etycon.exn, Kind.Arity 0)
   end

structure Type =
   struct
      open Type

      fun fromEnv (t: Etype.t): t =
	 let
	    fun con (c, ts) =
	       Con (Tycon.fromEnv (c, Kind.Arity (Vector.length ts)), ts)
	 in
	    Etype.hom (t, {con = con,
			   record = Record,
			   var = Var})
	 end
   end

structure Scheme =
   struct
      open Scheme

      val admitsEquality = schemeAdmitsEquality
 
      val copy = copyScheme

      val toEnv = schemeToEnv
	 
      fun fromEnv (s: Escheme.t): t =
	 let
	    val (tyvars, ty) = Escheme.dest s
	 in
	    make (tyvars, Type.fromEnv ty)
	 end
   end

structure Cons =
   struct
      open TypeStr.Cons

      fun fromEnv (Econs.T v): t =
	 T (Vector.map (v, fn {con, name, scheme} =>
			{con = con,
			 name = name,
			 scheme = Scheme.fromEnv scheme}))
   end

val renameTycons = ref (fn () => ())
   
structure TypeStr =
   struct
      structure Cons' = Cons
      structure Scheme' = Scheme
      structure Tycon' = Tycon
      structure Type' = Type
      open TypeStr
      structure Cons = Cons'
      structure Scheme = Scheme'
      structure Tycon = Tycon'
      structure Type = Type'

      val admitsEquality = typeStrAdmitsEquality
	 
      val copy = copyTypeStr

      val toEnv = typeStrToEnv
	 
      fun fromEnv (s: EtypeStr.t) =
	 let
	    val kind = EtypeStr.kind s
	 in
	    case EtypeStr.node s of
	       EtypeStr.Datatype {cons, tycon} =>
		  data (Tycon.fromEnv (tycon, kind),
			kind,
			Cons.fromEnv cons)
	     | EtypeStr.Scheme s => def (Scheme.fromEnv s, kind)
	     | EtypeStr.Tycon c =>
		  tycon (Tycon.fromEnv (c, kind), kind)
	 end

      val fromEnv =
	 Trace.trace ("TypeStr.fromEnv", EtypeStr.layout, layout) fromEnv

      fun getFlex (s: t, time, oper, reg, lay): FlexibleTycon.t option =
	 let
	    fun error what =
	       let
		  open Layout
		  val _ = 
		     Control.error
		     (reg,
		      seq [str "type ", lay (),
			   str (concat [" is ", what, " and cannot be ", oper])],
		      empty)
	       in
		  NONE
	       end
	    fun loop (s: t): FlexibleTycon.t option =
	       case node s of
		  Datatype {tycon, ...} => loopTycon tycon
		| Scheme (Scheme.T {ty, tyvars}) =>
		     (case Type.deEta (expandTy ty, tyvars) of
			 NONE => error "a definition"
		       | SOME c => loopTycon c)
		| Tycon c => loopTycon c
	    and loopTycon (c: Tycon.t): FlexibleTycon.t option =
	       case c of
		  Tycon.Flexible c =>
		     let
			val {creationTime, defn, ...} = FlexibleTycon.dest c
		     in
			case Defn.dest (!defn) of
			   Defn.Realized _ => Error.bug "getFlex of realized"
			 | Defn.TypeStr s => loop s
			 | Defn.Undefined =>
			      if Time.< (creationTime, time)
				 then error "not local"
			      else SOME c
		     end
		| Tycon.Rigid (c, _) =>
		     (! renameTycons ()
		      ; error (concat ["already defined as ",
				       Layout.toString (Etycon.layout c)]))
	 in
	    loop s
	 end

      fun share ((s: t, reg, lay), (s': t, reg', lay'), time: Time.t): unit =
	 let
	    val oper = "shared"
	    val k = kind s
	    val k' = kind s'
	 in
	    if not (Kind.equals (k, k'))
	       then
		  let
		     open Layout
		  in
		     Control.error
		     (reg,
		      seq [str "type ", lay (),
			   str " has arity ", Kind.layout k,
			   str " and type ", lay' (),
			   str " has arity ", Kind.layout k',
			   str " so cannot be shared"],
		      empty)
		  end
	    else
	       case (getFlex (s, time, oper, reg, lay),
		     getFlex (s', time, oper, reg', lay')) of
		  (SOME f, SOME f') => FlexibleTycon.share (f, f')
		| _ => ()
	 end

      val share =
	 Trace.trace
	 ("TypeStr.share",
	  fn ((s, _, _), (s', _, _), t) =>
	  Layout.tuple [layout s, layout s', Time.layout t],
	  Unit.layout)
	 share

      fun wheree (s': t, r: Region.t, lay, time: Time.t, s: t): unit =
	 case getFlex (s', time, "redefined", r, lay) of
	    NONE => ()
	  | SOME flex =>
	       let
		  val k = kind s
		  val k' = kind s'
	       in
		  if not (Kind.equals (k, k'))
		     then
			let
			   open Layout
			in
			   Control.error
			   (r,
			    seq [str "type ", lay (),
				 str " has arity ", Kind.layout k',
				 str " and cannot be defined to have arity ",
				 Kind.layout k],
			    empty)
			end
		  else if (admitsEquality s' = AdmitsEquality.Sometimes
			   andalso admitsEquality s = AdmitsEquality.Never)
			  then
			     let
				open Layout
			     in
				Control.error
				(r,
				 seq [str "eqtype ", lay (),
				      str " cannot be defined as a non-equality type"],
				 empty)
			     end
		       else
			  let
			     val {defn, hasCons, ...} = FlexibleTycon.dest flex
			  in
			     if hasCons
				andalso
				(case node s of
				    Scheme (Scheme.T {ty, tyvars}) =>
				       Option.isNone
				       (Type.deEta (expandTy ty, tyvars))
				  | _ => false)
				then
				   let
				      open Layout
				   in
				      Control.error
				      (r,
				       seq [str "type ", lay (),
					    str " is a datatype and cannot be redefined as a complex type"],
				       empty)
				   end
			     else
				defn := Defn.typeStr s
			  end
	       end

      val wheree =
	 Trace.trace ("TypeStr.wheree",
		      fn (s, _, _, t, s') => Layout.tuple [layout s,
							   Time.layout t,
							   layout s'],
		      Unit.layout)
	 wheree
   end

structure UniqueId = IntUniqueId ()

(*---------------------------------------------------*)
(*                   Main Datatype                   *)
(*---------------------------------------------------*)
(* Invariant: only ever union two envs if they have the same shape. *)
(* The shape of interface is the set of longtycons that are accessible in it. *)

datatype t = T of {copy: copy,
		   plist: PropertyList.t,
		   shape: Shape.t,
		   strs: (Ast.Strid.t * t) array,
		   types: (Ast.Tycon.t * TypeStr.t) array,
		   uniqueId: UniqueId.t,
		   vals: (Ast.Vid.t * (Status.t * Scheme.t)) array} Set.t
withtype copy = t option ref

fun new {strs, types, vals} =
   T (Set.singleton {copy = ref NONE,
		     plist = PropertyList.new (),
		     shape = Shape.new (),
		     strs = strs,
		     types = types,
		     uniqueId = UniqueId.new (),
		     vals = vals})

val empty = new {strs = Array.new0 (),
		 types = Array.new0 (),
		 vals = Array.new0 ()}

local
   fun make f (T s) = f (Set.value s)
in
   val plist = make #plist
   val shape = make #shape
   val strs = make #strs
   val types = make #types
   val uniqueId = make #uniqueId
   val vals = make #vals
end

local
   open Layout
in
   fun layout (T s) =
      let
	 val {shape, strs, types, uniqueId = u, vals, ...} = Set.value s
      in
	 record [("shape", Shape.layout shape),
		 ("uniqueId", UniqueId.layout u),

		 ("strs",
		  Array.layout (Layout.tuple2 (Strid.layout, layout)) strs),
		 ("types",
		  Array.layout (Layout.tuple2 (Ast.Tycon.layout, TypeStr.layout))
		  types),
		 ("vals",
		  Array.layout (Layout.tuple2 (Vid.layout,
					       Layout.tuple2 (Status.layout,
							      Scheme.layout)))
		  vals)]
      end
end

fun dest (T s) =
   let
      val {strs, types, vals, ...} = Set.value s
   in
      {strs = strs,
       types = types,
       vals = vals}
   end

fun equals (T s, T s') = Set.equals (s, s')

val equals =
   Trace.trace2 ("Interface.equals", layout, layout, Bool.layout) equals

fun peekStrid (T s, strid: Ast.Strid.t): t option =
   let
      val {strs, ...} = Set.value s
   in
      Array.peekMap (strs, fn (strid', I) =>
		     if Strid.equals (strid, strid')
			then SOME I
		     else NONE)
   end

datatype 'a peekResult =
   Found of 'a
  | UndefinedStructure of Strid.t list

fun peekStrids (I: t, strids: Ast.Strid.t list): t peekResult =
   let
      fun loop (I, strids, ac) =
	 case strids of
	    [] => Found I
	  | strid :: strids =>
	       case peekStrid (I, strid) of
		  NONE => UndefinedStructure (rev (strid :: ac))
		| SOME I => loop (I, strids, strid :: ac)
   in
      loop (I, strids, [])
   end

fun peekTycon (T s, tycon: Ast.Tycon.t): TypeStr.t option =
   let
      val {types, ...} = Set.value s
   in
      Array.peekMap (types, fn (name, typeStr) =>
		     if Ast.Tycon.equals (tycon, name)
			then SOME typeStr
		     else NONE)
   end

fun unbound (r: Region.t, className, x: Layout.t): unit =
   Control.error
   (r,
    let open Layout
    in seq [str "undefined ", str className, str " ", x]
    end,
    Layout.empty)

fun layoutStrids (ss: Strid.t list): Layout.t =
   Layout.str (concat (List.separate (List.map (ss, Strid.toString), ".")))

fun lookupLongtycon (I: t, long: Longtycon.t, r: Region.t,
		     {prefix: Strid.t list}) =
   let
      val (ss, c) = Longtycon.split long
   in
      case peekStrids (I, ss) of
	 Found I =>
	    (case peekTycon (I, c) of
		NONE => 
		   (unbound (r, "type",
			     Longtycon.layout (Longtycon.long (prefix @ ss, c)))
		    ; NONE)
	      | SOME s => SOME s)
       | UndefinedStructure ss =>
	    (unbound (r, "structure", layoutStrids (prefix @ ss))
	     ; NONE)
   end

fun share (I: t, ls: Longstrid.t, I': t, ls': Longstrid.t, time): unit =
   let
      fun lay (s, ls, strids, name) =
	 (s, Longstrid.region ls,
	  fn () =>
	  let
	     val (ss, s) = Longstrid.split ls
	  in
	     Ast.Longtycon.layout
	     (Ast.Longtycon.long (List.concat [ss, [s], rev strids],
				  name))
	  end)
      fun share (I as T s, I' as T s', strids): unit = 
	 if Shape.equals (shape I, shape I')
	    then
	       let
		  fun loop (T s, T s', strids): unit =
		     let
			val {strs, types, ...} = Set.value s
			val {strs = strs', types = types', ...} = Set.value s'
			val _ = Set.union (s, s')
			val _ =
			   Array.foreach2
			   (types, types', fn ((name, s), (_, s')) =>
			    TypeStr.share (lay (s, ls, strids, name),
					   lay (s', ls', strids, name),
					   time))
			val _ =
			   Array.foreach2
			   (strs, strs', fn ((name, I), (_, I')) =>
			    loop (I, I', name :: strids))
		     in
			()
		     end
	       in
		  loop (I, I', strids)
	       end
	 else (* different shapes -- need to share pointwise *)
	    let
	       val T s = I
	       val {strs, types, ...} = Set.value s
	       val {strs = strs', types = types', ...} = Set.value s'
	       fun walk2 (a, a', compareNames, f) =
		  let
		     val n = Array.length a
		     val n' = Array.length a'
		     fun both (i, i') =
			if i < n andalso i' < n'
			   then compare (i, Array.sub (a, i),
					 i', Array.sub (a', i'))
			else ()
		     and compare (i, (name, z), i', (name', z')) =
			case compareNames (name, name') of
			   GREATER =>
			      let
				 val i' = i' + 1
			      in
				 if i' < n'
				    then compare (i, (name, z),
						  i', Array.sub (a', i'))
				 else ()
			      end
			 | EQUAL => (f (z, z', name)
				     ; both (i + 1, i' + 1))
			 | LESS =>
			      let
				 val i = i + 1
			      in
				 if i < n
				    then compare (i, Array.sub (a, i),
						  i', (name', z'))
				 else ()
			      end
		  in
		     both (0, 0)
		  end
	       val _ =
		  walk2 (strs, strs', Strid.compare,
			 fn (I, I', name) => share (I, I', name :: strids))
	       val _ =
		  walk2 (types, types', Ast.Tycon.compare,
			 fn (s, s', name) =>
			 TypeStr.share (lay (s, ls, strids, name),
					lay (s', ls', strids, name),
					time))
	    in
	       ()
	    end
   in
      share (I, I', [])
   end

val share =
   Trace.trace
   ("Interface.share",
    fn (I, _, I', _, t) =>
    Layout.tuple [layout I, layout I', Time.layout t],
    Unit.layout)
   share

fun 'a copyAndRealize (I: t, {followStrid, init: 'a, realizeTycon}): t =
   let
      (* Keep track of all nodes that have forward pointers to copies, so
       * that we can gc them when done.
       *)
      val copies: copy list ref = ref []
      fun loop (I as T s, a: 'a): t =
	 let
	    val {copy, shape, strs, types, vals, ...} = Set.value s
	 in
	    case !copy of
	       NONE =>
		  let
		     val types =
			Array.map
			(types, fn (name, typeStr) =>
			 let
			    val typeStr = TypeStr.copy typeStr
			    val _ =
			       case realizeTycon of
				  NONE => ()
				| SOME f =>
				     case TypeStr.toTyconOpt typeStr of
					SOME (Tycon.Flexible c) =>
					   let
					      val FlexibleTycon.T s = c
					      val {admitsEquality, defn, hasCons,
						   ...} =
						 Set.value s
					   in
					      case Defn.dest (!defn) of
						 Defn.Realized _ => ()
					       | Defn.TypeStr _ => ()
					       | Defn.Undefined =>
						    FlexibleTycon.realize
						    (c,
						     f (a, name,
							!admitsEquality,
							TypeStr.kind typeStr,
							{hasCons = hasCons}))
					   end
				      | _ => ()
			 in
			    (name, typeStr)
			 end)
		     val vals =
			Array.map
			(vals, fn (name, (status, scheme)) =>
			 (name, (status, Scheme.copy scheme)))
		     val strs =
			Array.map (strs, fn (name, I) =>
				   (name, loop (I, followStrid (a, name))))
		     val I = T (Set.singleton {copy = ref NONE,
					       plist = PropertyList.new (),
					       shape = shape,
					       strs = strs,
					       types = types,
					       uniqueId = UniqueId.new (),
					       vals = vals})
		     val _ = List.push (copies, copy)
		     val _ = copy := SOME I
		  in
		     I
		  end
	     | SOME I => I
	 end
      val I = loop (I, init)
      fun clear copies = List.foreach (!copies, fn copy => copy := NONE)
      val _ = clear copies
      val _ = clear FlexibleTycon.copies
      val _ = FlexibleTycon.copies := []
   in
      I
   end

fun copy I = copyAndRealize (I, {init = (),
				 followStrid = fn _ => (),
				 realizeTycon = NONE})
				 
val copy = Trace.trace ("Interface.copy", layout, layout) copy

val info = Trace.info "Interface.realize"
   
fun realize (I, {init, followStrid, realizeTycon}) =
   Trace.traceInfo' (info ,layout o #1, layout)
   copyAndRealize (I, {init = init,
		       followStrid = followStrid,
		       realizeTycon = SOME realizeTycon})

end
