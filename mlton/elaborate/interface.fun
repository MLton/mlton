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
   structure Tyvar = Tyvar
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

structure ShapeId = UniqueId ()

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
       
structure FlexibleTycon =
   struct
      structure TypeFcn =
	 struct
	    datatype t =
	       Forced of EtypeStr.t
	     | Fun
	     | Tycon

	    fun layout f =
	       let
		  open Layout
	       in
		  case f of
		     Forced f => paren (seq [str "forced ", EtypeStr.layout f])
		   | Fun => str "<flexible def>"
		   | Tycon => str "<flexible tycon>"
	       end

	    fun layoutApp (f: t, v: (Layout.t * {isChar: bool,
						 needsParen: bool}) vector) =
	       let
		  open Layout
	       in
		  (seq [paren (layout f), tuple (Vector.toListMap (v, #1))],
		   {isChar = false, needsParen = true})
	       end

	    val toEnv: t -> EtypeStr.t =
	       fn Forced f => f
		| _ => Error.bug "impossible force of FlexibleTycon"
	 end
      
      datatype t = T of {admitsEquality: AdmitsEquality.t ref,
			 copy: copy,
			 hasCons: bool,
			 id: TyconId.t,
			 typeFcn: TypeFcn.t} Set.t
      withtype copy = t option ref

      val equals = fn (T s, T s') => Set.equals (s, s')

      fun dest (T s) = Set.value s

      fun setValue (T s, r) = Set.setValue (s, r)

      fun admitsEquality t = #admitsEquality (dest t)

      fun isFlexible (T s) =
	 case #typeFcn (Set.value s) of
	    TypeFcn.Tycon => true
	  | _ => false

      fun layout (T s) =
	 let
	    open Layout
	    val {admitsEquality, hasCons, id, typeFcn, ...} = Set.value s
	 in
	    record [("admitsEquality", AdmitsEquality.layout (!admitsEquality)),
		    ("hasCons", Bool.layout hasCons),
		    ("id", TyconId.layout id),
		    ("typeFcn", TypeFcn.layout typeFcn)]
	 end

      fun setTypeStr (T s, e: EtypeStr.t): unit =
	 let
	    val {admitsEquality, copy, id, hasCons, ...} = Set.value s
	 in
	    Set.setValue (s, {admitsEquality = admitsEquality,
			      copy = copy,
			      hasCons = hasCons,
			      id = id,
			      typeFcn = TypeFcn.Forced e})
	 end

      fun new {hasCons: bool, typeFcn: TypeFcn.t}: t =
	 T (Set.singleton {admitsEquality = ref AdmitsEquality.Sometimes,
			   copy = ref NONE,
			   hasCons = hasCons,
			   id = TyconId.new (),
			   typeFcn = typeFcn})

      fun make {hasCons} = new {hasCons = hasCons, typeFcn = TypeFcn.Tycon}

      val bogus = make {hasCons = false}

      fun toTypeFcn (T s) = #typeFcn (Set.value s)

      fun layoutApp (t, v) =
	 TypeFcn.layoutApp (toTypeFcn t, v)
	 
      val copies: copy list ref = ref []
	 
      fun copy (T s): t =
	 let
	    val {admitsEquality = a, copy, hasCons, typeFcn, ...} = Set.value s
	 in
	    case !copy of
	       NONE => 
		  let
		     val c = new {hasCons = hasCons,
				  typeFcn = typeFcn}
		     val _ = admitsEquality c := !a
		     val _ = List.push (copies, copy)
		     val _ = copy := SOME c
		  in
		     c
		  end
	     | SOME c => c
	 end

      fun shareOK (T s, T s') =
	 let
	    val {admitsEquality = a, hasCons = h, id, typeFcn = f, ...} =
	       Set.value s
	    val {admitsEquality = a', hasCons = h', typeFcn = f', ...} =
	       Set.value s'
	    val _ = Set.union (s, s')
	    val _ = 
	       Set.setValue
	       (s, {admitsEquality = ref (AdmitsEquality.or (!a, !a')),
		    copy = ref NONE,
		    id = id,
		    hasCons = h orelse h',
		    typeFcn = TypeFcn.Tycon})
	 in
	    ()
	 end

      fun share (f, z, f', z'): unit =
	 let
	    fun error (reg, lay) =
	       let
		  open Layout
	       in
		  Control.error
		  (reg,
		   seq [str "type ", lay (),
			str " is a definition and cannot be shared"],
		   empty)
	       end
	 in
	    case (toTypeFcn f, toTypeFcn f') of
	       (TypeFcn.Fun, _) => error z
	     | (_, TypeFcn.Fun) => error z'
	     | (TypeFcn.Tycon, TypeFcn.Tycon) => shareOK (f, f')
	     | _ => Error.bug "type sharing on Forced typeFcn"
	 end

      fun toEnv (T s): EtypeStr.t =
	 TypeFcn.toEnv (#typeFcn (Set.value s))
   end

structure Tycon =
   struct
      structure AdmitsEquality = AdmitsEquality

      datatype t =
	 Flexible of FlexibleTycon.t
       | Rigid of Etycon.t * Kind.t

      val tuple = Rigid (Etycon.tuple, Kind.Nary)

      val layout =
	 fn Flexible c => FlexibleTycon.layout c
	  | Rigid (c, _) => Etycon.layout c

      val equals =
	 fn (Flexible f, Flexible f') => FlexibleTycon.equals (f, f')
	  | (Rigid (c, _), Rigid (c', _)) => Etycon.equals (c, c')
	  | _ => false

      val exn = Rigid (Etycon.exn, Kind.Arity 0)

      fun admitsEquality (t: t): AdmitsEquality.t ref =
	 case t of
	    Flexible f => FlexibleTycon.admitsEquality f
	  | Rigid (e, _) => Etycon.admitsEquality e

      val fromEnv: Etycon.t * Kind.t -> t = Rigid

      fun layoutApp (t: t, v) =
	 case t of
	    Flexible f => FlexibleTycon.layoutApp (f, v)
	  | Rigid (c, _) => Etycon.layoutApp (c, v)

      val make = Flexible o FlexibleTycon.make

      fun copy (t: t): t =
	 case t of
	    Flexible c => Flexible(FlexibleTycon.copy c)
	  | Rigid _ => t

      fun toEnv (t: t): EtypeStr.t =
	 case t of
	    Flexible c => FlexibleTycon.toEnv c
	  | Rigid (c, k) => EtypeStr.tycon (c, k)

      val arrow = fromEnv (Etycon.arrow, Kind.Arity 2)

      val exn = fromEnv (Etycon.exn, Kind.Arity 0)

      fun toFlexible (c: t): FlexibleTycon.t option =
	 case c of
	    Flexible c => SOME c
	  | Rigid _ => NONE
   end

structure Type =
   struct
      datatype t =
	 Con of Tycon.t * t vector
       | Record of t Record.t
       | Var of Tyvar.t

      val bogus = Con (Tycon.exn, Vector.new0 ())	 
      val con = Con
      val record = Record
      val var = Var

      val exn = Con (Tycon.exn, Vector.new0 ())

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

      fun toEnv t =
	 hom (t, {con = fn (c, ts) => EtypeStr.apply (Tycon.toEnv c, ts),
		  record = Etype.record,
		  var = Etype.var})

      fun fromEnv (t: Etype.t): t =
	 let
	    fun con (c, ts) =
	       Con (Tycon.fromEnv (c, Kind.Arity (Vector.length ts)), ts)
	 in
	    Etype.hom (t, {con = con,
			   record = Record,
			   var = Var})
	 end
		 
      fun copy (t: t): t =
	 hom (t, {con = fn (c, ts) => Con (Tycon.copy c, ts),
		  record = Record,
		  var = Var})

      fun arrow (t1, t2) = Con (Tycon.arrow, Vector.new2 (t1, t2))

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
   end

structure Scheme = GenericScheme (structure Type = Type
				  structure Tyvar = Tyvar)
				  
structure Scheme =
   struct
      open Scheme

      fun copy (T {tyvars, ty}): t =
	 T {ty = Type.copy ty, tyvars = tyvars}

      fun dest (T {ty, tyvars}) = (tyvars, ty)
	 
      fun make (tyvars, ty) = T {ty = ty, tyvars = tyvars}

      fun bogus () = T {ty = Type.bogus, tyvars = Vector.new0 ()}

      fun toEnv (Scheme.T {ty, tyvars}) =
	 Escheme.make (tyvars, Type.toEnv ty)

      fun admitsEquality (s: t): bool =
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
	    Type.hom (ty s, {con = con,
			     record = fn r => Record.forall (r, fn b => b),
			     var = fn _ => true})
	 end

      fun fromEnv (s: Escheme.t): t =
	 let
	    val (tyvars, ty) = Escheme.dest s
	 in
	    make (tyvars, Type.fromEnv ty)
	 end
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

structure Cons =
   struct
      open TypeStr.Cons

      fun copy (T v): t =
	 T (Vector.map (v, fn {con, name, scheme} =>
			{con = con,
			 name = name,
			 scheme = Scheme.copy scheme}))

      fun toEnv (T v): Econs.t =
	 Econs.T (Vector.map (v, fn {con, name, scheme} =>
			      {con = con,
			       name = name,
			       scheme = Scheme.toEnv scheme}))

      fun fromEnv (Econs.T v): t =
	 T (Vector.map (v, fn {con, name, scheme} =>
			{con = con,
			 name = name,
			 scheme = Scheme.fromEnv scheme}))
   end

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

      fun toFlexible (s: t): FlexibleTycon.t option =
	 case node s of
	    Datatype {tycon, ...} => Tycon.toFlexible tycon
	  | Tycon c => Tycon.toFlexible c
	  | _ => NONE

      fun copy (s: t): t =
	 let
	    val kind = kind s
	 in
	    case node s of
	       Datatype {cons, tycon} => data (Tycon.copy tycon,
					       kind,
					       Cons.copy cons)
	     | Scheme s => def (Scheme.copy s, kind)
	     | Tycon c => tycon (Tycon.copy c, kind)
	 end

      fun toEnv (s: t): EtypeStr.t =
	 let
	    val k = kind s
	 in
	    case node s of
	       Datatype {cons, tycon} =>
		  let
		     val tycon: Etycon.t =
			case tycon of
			   Tycon.Flexible c =>
			      let
				 val typeStr = FlexibleTycon.toEnv c
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
						 layout s,
						 str " realized with scheme ",
						 EtypeStr.layout typeStr]))
				       end
			      end
			 | Tycon.Rigid (c, _) => c
		  in
		     EtypeStr.data (tycon, k, Cons.toEnv cons)
		  end
	     | Scheme s => EtypeStr.def (Scheme.toEnv s, k)
	     | Tycon c => EtypeStr.abs (Tycon.toEnv c)
	 end

      val toEnv = Trace.trace ("TypeStr.toEnv", layout, EtypeStr.layout) toEnv

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

      fun share (s: t, z, s': t, z'): unit =
	 let
	    fun getFlex (s: t, (reg, lay),
			 continue: FlexibleTycon.t -> unit): unit =
	       let
		  fun error what =
		     let
			open Layout
		     in
			Control.error
			(reg,
			 seq [str "type ", lay (),
			      str (concat [" is ", what,
					   " and cannot be shared"])],
			 empty)
		     end
		  fun get c =
		     case c of
			Tycon.Flexible f => continue f
		      | Tycon.Rigid _ => error "a toplevel type"
	       in
		  case node s of
		     Datatype {tycon, ...} => get tycon
		   | Scheme _ => error "a definition"
		   | Tycon c => get  c
	       end
	    val k = kind s
	    val k' = kind s'
	 in
	    if not (Kind.equals (k, k'))
	       then
		  let
		     val (reg, lay) = z
		     val (_, lay') = z'
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
	       getFlex (s, z, fn c =>
			getFlex (s', z', fn c' =>
				 FlexibleTycon.share (c, z, c', z')))
	 end
   end
	   
(*---------------------------------------------------*)
(*                   Main Datatype                   *)
(*---------------------------------------------------*)
(* Invariant: only ever union two envs if they have the same shape. *)
(* The shape of interface is the set of longtycons that are accessible in it. *)

datatype t = T of {copy: copy,
		   elements: element list,
		   plist: PropertyList.t,
		   shapeId: ShapeId.t,
		   wheres: (FlexibleTycon.t * TypeStr.t) list ref} Set.t
and element =
   Str of {interface: t,
	   name: Ast.Strid.t}
  | Type of {name: Ast.Tycon.t,
	     typeStr: TypeStr.t}
  | Val of {name: Ast.Vid.t,
	    scheme: Scheme.t,
	    status: Status.t}
withtype copy = t option ref

fun reportDuplicates (T s, region) =
   let
      val {elements, ...} = Set.value s
      fun make (kind, toString) =
	 let
	    val h = HashSet.new {hash = #hash}
	 in
	    fn n => let
		       val s = toString n
		       val hash = String.hash s
		       val isNew = ref true
		       val _ = 
			  HashSet.lookupOrInsert
			  (h, hash,
			   fn {name, ...} => (s = name
					      andalso (isNew := false
						       ; true)),
			   fn () => {hash = hash,
				     name = s})
		    in
		       if !isNew
			  then ()
		       else Control.error (region,
					   Layout.str
					   (concat ["duplicate ",
						    kind,
						    " specification: ",
						    s]),
					   Layout.empty)
		    end
	 end
      val str = make ("structure", Ast.Strid.toString)
      val ty = make ("type", Ast.Tycon.toString)
      val vid = make ("variable", Ast.Vid.toString)
   in
      List.foreach
      (elements, fn e =>
       case e of
	  Str {name, ...} => str name
	| Type {name, ...} => ty name
	| Val {name, ...} => vid name)
   end
   
type interface = t

local
   fun make f (T s) = f (Set.value s)
in
   val plist = make #plist
end

fun equals (T s, T s') = Set.equals (s, s')

local
   open Layout
in
   fun layout(T s) =
      let
	 val {elements, wheres, ...} = Set.value s
      in
	 record[("elements", list (List.map (elements, layoutElement))),
		("wheres", list (List.map (!wheres, fn (c, f) =>
					   tuple [FlexibleTycon.layout c,
						  TypeStr.layout f])))]
		
      end
   and layoutElement (e: element) =
      let
	 val (lhs, rhs) =
	    case e of
	       Val{name, scheme, status} =>
		  (Ast.Vid.layout name,
		   tuple[Status.layout status,
			 Scheme.layout scheme])
	     | Type{name, typeStr} =>
		  (Ast.Tycon.layout name,
		   TypeStr.layout typeStr)
	     | Str{name, interface} =>
		  (Ast.Strid.layout name, layout interface)
      in seq [lhs, str " -> ", rhs]
      end
end

fun explicit elements: t =
   T (Set.singleton {copy = ref NONE,
		     elements = elements,
		     plist = PropertyList.new (),
		     shapeId = ShapeId.new (),
		     wheres = ref []})

val empty = explicit []

val bogus = empty

fun vals v = explicit (Vector.toListMap (v, Val))
fun strs v = explicit (Vector.toListMap (v, Str))
fun types v = explicit (Vector.toListMap (v, Type))
   
local
   fun make status (Cons.T cs) =
      explicit (Vector.toListMap (cs, fn {name, scheme, ...} =>
				  Val {name = Ast.Vid.fromCon name,
				       scheme = scheme,
				       status = status}))
in
   val cons = make Status.Con
   val excons = make Status.Exn
end

fun elements (T s): element list = #elements (Set.value s)
fun shapeId (T s) = #shapeId (Set.value s)
   
fun extendTycon (I, tycon, typeStr) =
   explicit (elements I @ [Type {name = tycon, typeStr = typeStr}])

fun (T s) + (T s') =
   let
      val {elements = es, wheres = ws, ...} = Set.value s
      val {elements = es', wheres = ws', ...} = Set.value s'
   in
      T (Set.singleton {copy = ref NONE,
			elements = es @ es',
			plist = PropertyList.new (),
			shapeId = ShapeId.new (),
			wheres = ref (!ws @ !ws')})
   end

fun peekTyconElements (elements: element list, tycon): TypeStr.t option =
   case List.peek (elements,
		   fn Type {name, ...} => Ast.Tycon.equals(tycon,name)
		    | _ => false) of
      NONE => NONE
    | SOME (Type {typeStr, ...}) => SOME typeStr
    | _ => Error.bug "peekTyconElements"
	 
fun peekStridElements (elements, strid): t option =
   case List.peek (elements,
		   fn Str  {name, ...} => Strid.equals(strid,name)
		    | _ => false) of
      NONE => NONE
    | SOME (Str {interface, ...}) => SOME interface
    | _ => Error.bug "peekStridElements"

fun peekStrid (I: t, strid: Ast.Strid.t): t option =
   peekStridElements (elements I, strid)

datatype 'a peekResult =
   Found of t
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

fun unbound (r: Region.t, className, x: Layout.t): unit =
   Control.error
   (r,
    let open Layout
    in seq [str "undefined ", str className, str " ", x]
    end,
    Layout.empty)

fun layoutStrids (ss: Strid.t list): Layout.t =
   Layout.str (concat (List.separate (List.map (ss, Strid.toString), ".")))

fun lookupLongstrid (I: t, s: Longstrid.t): t =
   let
      val (strids, strid) = Longstrid.split s
   in
      case peekStrids (I, strids @ [strid]) of
	 Found I => I
       | UndefinedStructure ss =>
	    (unbound (Longstrid.region s, "structure", layoutStrids ss)
	     ; bogus)
   end

structure PeekResult =
   struct
      datatype 'a t =
	 Found of 'a
       | UndefinedStructure of Strid.t list
       | Undefined

      fun layout lay =
	 fn Found z => lay z
	  | UndefinedStructure ss => layoutStrids ss
	  | Undefined => Layout.str "Undefined"

      val toOption: 'a t -> 'a option =
	 fn Found z => SOME z
	  | _ => NONE
   end

fun peekLongtycon (I: t, c: Longtycon.t): TypeStr.t PeekResult.t =
   let
      val (strids, c) = Longtycon.split c
   in
      case peekStrids (I, strids) of
        Found I =>
	   (case peekTyconElements (elements I, c) of
	       NONE => PeekResult.Undefined
	     | SOME s => PeekResult.Found s)
      | UndefinedStructure ss => PeekResult.UndefinedStructure ss
   end

fun lookupLongtycon (I: t, c: Longtycon.t, continue: TypeStr.t -> unit): unit =
   let
      datatype z = datatype PeekResult.t
   in
      case peekLongtycon (I, c) of
	 Found s => continue s
       | UndefinedStructure ss =>
	    unbound (Longtycon.region c, "structure", layoutStrids ss)
       | Undefined => 
	    unbound (Longtycon.region c, "type", Longtycon.layout c)
   end

val peekLongtycon =
   fn z =>
   let
      datatype z = datatype PeekResult.t
   in
      case peekLongtycon z of
	 Found s => SOME s
       | _ => NONE
   end

fun shareType (I: t, c: Longtycon.t, c': Longtycon.t) =
   lookupLongtycon
   (I, c, fn s =>
    lookupLongtycon
    (I, c', fn s' =>
     TypeStr.share (s, (Longtycon.region c, fn () => Longtycon.layout c),
		    s', (Longtycon.region c', fn () => Longtycon.layout c'))))

fun sameShape (m, m') = ShapeId.equals (shapeId m, shapeId m')

fun share (I as T s, reg: Region.t, I' as T s', reg', strids): unit = 
   if Set.equals (s, s')
      then ()
   else
      if sameShape (I, I')
	 then
	    let
	       fun loop (T s, T s', strids): unit =
		  if Set.equals (s, s')
		     then ()
		  else 
		     let
			val {elements = es, ...} = Set.value s
			val {elements = es', ...} = Set.value s'
			val _ = Set.union (s, s')
			val _ =
			   List.foreach2
			   (es, es', fn (e, e') =>
			    case (e, e') of
			       (Str {interface = I, name, ...},
				Str {interface = I', ...}) =>
				  loop (I, I', name :: strids)
			     | (Type {typeStr = s, name, ...},
				Type {typeStr = s', ...}) =>
				  let
				     fun lay () =
					Ast.Longtycon.layout
					(Ast.Longtycon.long (rev strids, name))
				  in
				     TypeStr.share (s, (reg, lay),
						    s', (reg', lay))
				  end
			     | _ => ())
		     in
			()
		     end
	    in
	       loop (I, I', strids)
	    end
      else (* different shapes -- need to share pointwise *)
	 let
	    val es = elements I
	    val es' = elements I'
	 in
	    List.foreach
	    (es, fn e =>
	     case e of
		Str {name, interface = I} =>
		   (case peekStridElements (es', name) of
		       NONE => ()
		     | SOME I' => share (I, reg, I', reg', name :: strids))
	      | Type {name, typeStr = s} =>
	           (case peekTyconElements (es',name) of
		       NONE => ()
		     | SOME s' =>
			  let
			     fun lay () =
				Ast.Longtycon.layout
				(Ast.Longtycon.long (rev strids, name))
			  in
			     TypeStr.share (s, (reg, lay), s', (reg', lay))
			  end)
	      | _ => ())
	 end

val share =
   fn (m, s: Longstrid.t, s': Longstrid.t) =>
   share (lookupLongstrid (m, s),
	  Longstrid.region s,
	  lookupLongstrid (m, s'),
	  Longstrid.region s',
	  [])

structure TypeFcn = FlexibleTycon.TypeFcn

fun wheres (I as T s, v: (Longtycon.t * TypeStr.t) vector): unit =
   let
      val {wheres, ...} = Set.value s
   in
      Vector.foreach
      (v, fn (c, s: TypeStr.t) =>
       let
	  val reg = Longtycon.region c
	  fun noRedefine () =
	     let
		open Layout
	     in
		Control.error (reg,
			       seq [str "type ",
				    Longtycon.layout c,
				    str " cannot be redefined"],
			       empty)
	     end
       in
	  lookupLongtycon
	  (I, c, fn s' =>
	   case TypeStr.toFlexible s' of
	      NONE => noRedefine ()
	    | SOME flex =>
		 let
		    val k = TypeStr.kind s
		    val k' = TypeStr.kind s'
		 in
		    if not (Kind.equals (k, k'))
		       then
			  let
			     open Layout
			  in
			     Control.error
			     (reg,
			      seq [str "type ",
				   Longtycon.layout c,
				   str " has arity ", Kind.layout k',
				   str " and cannot be redefined to have arity ",
				   Kind.layout k],
			      empty)
			  end
		    else if (TypeStr.admitsEquality s' = AdmitsEquality.Sometimes
			     andalso TypeStr.admitsEquality s = AdmitsEquality.Never)
		        then
			   let
			      open Layout
			   in
			      Control.error
			      (reg,
			       seq [str "eqtype ",
				    Longtycon.layout c,
				    str " cannot be redefined as a non-equality type"],
			       empty)
			   end
		    else
		       let
			  val {admitsEquality, copy, hasCons, id, typeFcn} =
			     FlexibleTycon.dest flex
		       in
			  if hasCons andalso (case TypeStr.node s of
						 TypeStr.Scheme _ => true
					       | _ => false)
			     then
				let
				   open Layout
				in
				   Control.error
				   (reg,
				    seq [str "type ",
					 Longtycon.layout c,
					 str " is a datatype and cannot be redefined as a complex type"],
				    empty)
				end
			  else
			     let
				datatype z = datatype TypeFcn.t
			     in
				case typeFcn of
				   Forced _ =>
				      Error.bug "where type on forced flexible tycon"
				 | Fun => noRedefine ()
				 | Tycon =>
				      let
					 fun doWhere () =
					    (List.push (wheres, (flex, s))
					     ;
					     FlexibleTycon.setValue
					     (flex, {admitsEquality = admitsEquality,
						     copy = copy,
						     hasCons = hasCons,
						     id = id,
						     typeFcn = typeFcn}))
					 fun doTycon c =
					    case c of
					       Tycon.Flexible flex' =>
						  FlexibleTycon.shareOK (flex, flex')
					     | Tycon.Rigid (c, _) => doWhere ()
				      in
					 case TypeStr.node s of
					    TypeStr.Datatype {tycon, ...} =>
					       doTycon tycon
					  | TypeStr.Scheme _ => doWhere ()
					  | TypeStr.Tycon c => doTycon c
				      end
			     end
		       end
		 end)
       end)
   end

val wheres =
   Trace.trace2 ("Interface.wheres",
		 layout,
		 Vector.layout (Layout.tuple2 (Longtycon.layout,
					       TypeStr.layout)),
		 Unit.layout)
   wheres

fun copyAndRealize (I: t, getTypeFcnOpt): t =
   let
      (* Keep track of all nodes that have forward pointers to copies, so
       * that we can gc them when done.
       *)
      val copies: copy list ref = ref []
      fun loop (T s, strids: Ast.Strid.t list): t =
	 let
	    val {copy, shapeId, elements, wheres, ...} = Set.value s
	 in
	    case !copy of
	       NONE =>
		  let
		     val wheres =
			List.map
			(!wheres, fn (c, s) =>
			 let
			    val c = FlexibleTycon.copy c
			    val s = TypeStr.copy s
			    val _ =
			       if isSome getTypeFcnOpt
				  then 
				     FlexibleTycon.setTypeStr
				     (c, TypeStr.toEnv s)
			       else ()
			 in
			    (c, s)
			 end)
		     val elements =
			List.map
			(elements, fn e =>
			 case e of
			    Str {name, interface} =>
			       Str {interface = loop (interface,
						      strids @ [name]),
				    name = name}
			  | Type {name, typeStr} =>
			       let
				  val typeStr = TypeStr.copy typeStr
				  val _ =
				     case (TypeStr.toTyconOpt typeStr,
					   getTypeFcnOpt) of
					(SOME (Tycon.Flexible c), SOME f) =>
					   let
					      fun get () =
						 f
						 (Longtycon.long (strids, name),
						  ! (FlexibleTycon.admitsEquality
						     c),
						  TypeStr.kind typeStr)
					      fun doit (s: EtypeStr.t): unit =
						 FlexibleTycon.setTypeStr (c, s)
					   in
					      case FlexibleTycon.toTypeFcn c of
						 TypeFcn.Fun => ()
					       | TypeFcn.Tycon => doit (get ())
					       | TypeFcn.Forced s =>
						    let
						       val s' = get ()
						    in
						       case (EtypeStr.node s,
							     EtypeStr.node s') of
							  (EtypeStr.Tycon c,
							   EtypeStr.Datatype
							   {tycon = c', ...}) =>
							     if Etycon.equals (c, c')
								then doit s'
							     else ()
							 | _ => ()
						    end
					   end
				      | _ => ()
			       in
				  Type {name = name,
					typeStr = typeStr}
			       end
			  | Val {name, scheme, status} =>
			       Val {name = name,
				    scheme = Scheme.copy scheme,
				    status = status})
		     val I = T (Set.singleton {copy = ref NONE,
					       elements = elements,
					       plist = PropertyList.new (),
					       shapeId = shapeId,
					       wheres = ref wheres})
		     val _ = List.push (copies, copy)
		     val _ = copy := SOME I
		  in
		     I
		  end
	     | SOME I => I
	 end
      val I = loop (I, [])
      fun clear copies =
	 (List.foreach (!copies, fn copy => copy := NONE)
	  ; copies := [])
      val _ = clear copies
      val _ = clear FlexibleTycon.copies
   in
      I
   end

fun copy I = copyAndRealize (I, NONE)

fun realize (I, f) = copyAndRealize (I, SOME f)

val realize = Trace.trace2 ("realize", layout, Layout.ignore, layout) realize

fun foreach (T s, {handleStr, handleType, handleVal}) =
   let
      val {elements, ...} = Set.value s
   in
      List.foreach
      (elements, fn elt =>
       case elt of
	  Str r => handleStr r
	| Type {name, typeStr} =>
	     handleType {name = name,
			 typeStr = TypeStr.toEnv typeStr}
	| Val {name, scheme, status} =>
	     handleVal {name = name,
			scheme = Scheme.toEnv scheme,
			status = status})
   end

end
