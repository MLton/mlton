(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor ElaborateEnv (S: ELABORATE_ENV_STRUCTS): ELABORATE_ENV =
struct

open S

local
   open Ast
in
   structure Fctid = Fctid
   structure Strid = Strid
   structure Longvid = Longvid
   structure Longstrid = Longstrid
   structure Longtycon = Longtycon
   structure Priority = Priority
   structure Sigid = Sigid
   structure Strid = Strid
   structure Symbol = Symbol
end

local
   open CoreML
in
   structure Con = Con
   structure Dec = Dec
   structure Exp = Exp
   structure Pat = Pat
   structure Tycon = Tycon
   structure Tyvar = Tyvar
   structure Var = Var
   structure Var = Var
end

local
   open Tycon
in
   structure AdmitsEquality = AdmitsEquality
   structure Kind = Kind
   structure Symbol = Symbol
end

local
   open TypeEnv
in
   structure Scheme = Scheme
   structure Type = Type
end

structure Decs = Decs (structure CoreML = CoreML)

structure Scheme =
   struct
      open Scheme
	 
      fun bogus () = fromType (Type.new ())
   end

val insideFunctor = ref false

structure Scope =
   struct
      structure Unique = UniqueId ()
      datatype t = T of {isTop: bool,
			 unique: Unique.t}

      local
	 fun make f (T r) = f r
      in
	 val isTop = make #isTop
	 val unique = make #unique
      end

      fun new {isTop: bool}: t =
	 T {isTop = isTop,
	    unique = Unique.new ()}

      fun equals (s, s') = Unique.equals (unique s, unique s')
   end

structure Uses:
   sig
      type 'a t

      val add: 'a t * 'a -> unit
      val all: 'a t -> 'a list
      val clear: 'a t -> unit
      val forceUsed: 'a t -> unit
      val hasUse: 'a t -> bool
      val isUsed: 'a t -> bool
      val new: unit -> 'a t
   end =
   struct
      datatype 'a t = T of {direct: 'a list ref,
			    forceUsed: bool ref}
 
      fun new () = T {direct = ref [],
		      forceUsed = ref false}

      fun add (T {direct, ...}, a) = List.push (direct, a)

      fun forceUsed (T {forceUsed = r, ...}) = r := true

      fun clear (T {direct, ...}) = direct := []

      fun all (T {direct, ...}) = !direct

      fun hasUse (T {direct, ...}): bool =
	 not (List.isEmpty (!direct))
	 
      fun isUsed (u as T {forceUsed, ...}): bool =
	 !forceUsed orelse hasUse u
   end

structure Class =
   struct
      datatype t = Con | Exn | Fix | Fct | Sig | Str | Typ | Var

      val toString =
	 fn Con => "constructor"
	  | Exn => "exception"
	  | Fix => "fixity"
	  | Fct => "functor"
	  | Sig => "signature"
	  | Str => "structure"
	  | Typ => "type"
	  | Var => "variable"
   end

structure Vid =
   struct
      datatype t =
	 Con of Con.t
       | Exn of Con.t
       | Overload of Priority.t * (Var.t * Type.t) vector
       | Var of Var.t

      val statusPretty =
	 fn Con _ => "constructor"
	  | Exn _ => "exception"
	  | Overload _ => "overload"
	  | Var _ => "variable"

      val bogus = Var Var.bogus

      fun layout vid =
	 let
	    open Layout
	    val (name, l) =
	       case vid of
		  Con c => ("Con", Con.layout c)
		| Exn c => ("Exn", Con.layout c)
		| Overload (p,xts) =>
		     ("Overload(" ^ (Layout.toString (Priority.layout p)) ^ ")",
		      Vector.layout (Layout.tuple2 (Var.layout, Type.layout))
		      xts)
		| Var v => ("Var", Var.layout v)
	 in
	    paren (seq [str name, str " ", l])
	 end

      val deVar =
	 fn Var v => SOME v
	  | _ => NONE
		
      val deCon =
	 fn Con c => SOME c
	  | Exn c => SOME c
	  | _ => NONE
	  
      val class =
	 fn Con _ => Class.Con
	  | Exn _ => Class.Exn
	  | Overload _ => Class.Var
	  | Var _ => Class.Var
   end

structure TypeStr =
   struct
      structure AdmitsEquality = AdmitsEquality
      structure Kind = Kind

      structure Tycon =
	 struct
	    open Tycon

	    val admitsEquality = TypeEnv.tyconAdmitsEquality
	 end

      structure Cons =
	 struct
	    datatype t = T of {con: Con.t,
			       name: Ast.Con.t,
			       scheme: Scheme.t,
			       uses: Ast.Vid.t Uses.t} vector
	       
	    val empty = T (Vector.new0 ())
	       
	    fun layout (T v) =
	       Vector.layout (fn {name, scheme, ...} =>
			      let
				 open Layout
			      in
				 seq [Ast.Con.layout name,
				      str ": ", Scheme.layout scheme]
			      end)
	       v
	 end

      datatype node =
	 Datatype of {cons: Cons.t,
		      tycon: Tycon.t}
       | Scheme of Scheme.t
       | Tycon of Tycon.t

      datatype t = T of {kind: Kind.t,
			 node: node}

      local
	 fun make f (T r) = f r
      in
	 val kind = make #kind
	 val node = make #node
      end

      fun layout t =
	 let
	    open Layout
	 in
	    case node t of
	       Datatype {tycon, cons} =>
		  seq [str "Datatype ",
		       record [("tycon", Tycon.layout tycon),
			       ("cons", Cons.layout cons)]]
	     | Scheme s => Scheme.layout s
	     | Tycon t => seq [str "Tycon ", Tycon.layout t]
	 end

      fun admitsEquality (s: t): AdmitsEquality.t =
	 case node s of
	    Datatype {tycon = c, ...} => ! (Tycon.admitsEquality c)
	  | Scheme s => if Scheme.admitsEquality s
			   then AdmitsEquality.Sometimes
			else AdmitsEquality.Never
	  | Tycon c =>  ! (Tycon.admitsEquality c)

      fun bogus (k: Kind.t): t =
	 T {kind = k,
	    node = Scheme (Scheme.bogus ())}

      fun abs t =
	 case node t of
	    Datatype {tycon, ...} => T {kind = kind t,
					node = Tycon tycon}
	  | _ => t

      fun apply (t: t, tys: Type.t vector): Type.t =
	 case node t of
	    Datatype {tycon, ...} => Type.con (tycon, tys)
	  | Scheme s => Scheme.apply (s, tys)
	  | Tycon t => Type.con (t, tys)

      fun cons t =
	 case node t of
	    Datatype {cons, ...} => cons
	  | _ => Cons.empty

      fun data (tycon, kind, cons) =
	 T {kind = kind,
	    node = Datatype {tycon = tycon, cons = cons}}
   
      fun def (s: Scheme.t, k: Kind.t) =
	 let
	    val (tyvars, ty) = Scheme.dest s
	 in
	    T {kind = k,
	       node = (case Type.deEta (ty, tyvars) of
			  NONE => Scheme s
			| SOME c => Tycon c)}
	 end

      fun toTyconOpt s =
	 case node s of
	    Datatype {tycon, ...} => SOME tycon
	  | Scheme _ => NONE
	  | Tycon c => SOME c

      fun tycon (c, kind) = T {kind = kind,
			       node = Tycon c}
   end

local
   open TypeStr
in
   structure Cons = Cons
end

structure Interface = Interface (structure Ast = Ast
				 structure EnvTypeStr = TypeStr)

local
   open Interface
in
   structure FlexibleTycon = FlexibleTycon
   structure Shape = Shape
   structure Status = Status
   structure TyconMap = TyconMap
end

structure Interface =
   struct
      structure Econs = Cons
      structure Escheme = Scheme
      structure Etycon = Tycon
      structure Etype = Type
      structure EtypeStr = TypeStr
      open Interface

      fun flexibleTyconToEnv (c: FlexibleTycon.t): EtypeStr.t =
	 let
	    datatype z = datatype FlexibleTycon.realization
	 in
	    case FlexibleTycon.realization c of
	       ETypeStr s => s
	     | TypeStr s => typeStrToEnv s
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
	 Escheme.make {canGeneralize = true,
		       ty = typeToEnv ty,
		       tyvars = tyvars}
      and consToEnv (Cons.T v): Econs.t =
	 Econs.T (Vector.map (v, fn {name, scheme} =>
			      {con = Con.newNoname (),
			       name = name,
			       scheme = schemeToEnv scheme,
			       uses = Uses.new ()}))
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

      structure Tycon =
	 struct
	    open Tycon

	    val fromEnv = Rigid
	 end

      structure Type =
	 struct
	    open Type

	    fun fromEnv (t: Etype.t): t =
	       let
		  fun con (c, ts) =
		     Type.con (Tycon.fromEnv (c, Kind.Arity (Vector.length ts)),
			       ts)
	       in
		  Etype.hom (t, {con = con,
				 expandOpaque = false,
				 record = record,
				 replaceCharWithWord8 = false,
				 var = var})
	       end
	 end
      
      structure Scheme =
	 struct
	    open Scheme

	    val toEnv = schemeToEnv

	    fun fromEnv (s: Escheme.t): t =
	       let
		  val (tyvars, ty) = Escheme.dest s
	       in
		  Scheme.T {ty = Type.fromEnv ty,
			    tyvars = tyvars}
	       end
	 end

      structure Cons =
	 struct
	    open Cons
	       
	    fun fromEnv (Econs.T v): t =
	       T (Vector.map (v, fn {name, scheme, ...} =>
			      {name = name,
			       scheme = Scheme.fromEnv scheme}))
	 end

      structure TypeStr =
	 struct
	    open TypeStr

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
	       Trace.trace ("Interface.TypeStr.fromEnv", EtypeStr.layout, layout)
	       fromEnv
	 end
   end

structure Status =
   struct
      open Status

      val pretty: t -> string =
	 fn Con => "constructor"
	  | Exn => "exception"
	  | Var => "variable"
   end

structure Info =
   struct
      (* The array is sorted by domain element. *)
      datatype ('a, 'b) t = T of {domain: 'a,
				  range: 'b,
				  uses: 'a Uses.t} array
	 
      fun layout (layoutDomain, layoutRange) (T a) =
	 Array.layout (fn {domain, range, ...} =>
		       Layout.tuple [layoutDomain domain, layoutRange range])
	 a

      fun foreach (T a, f) =
	 Array.foreach (a, fn {domain, range, ...} => f (domain, range))

      fun peek (T a, domain: 'a, toSymbol: 'a -> Symbol.t) =
	 Option.map
	 (BinarySearch.search (a, fn {domain = d, ...} =>
			       Symbol.compare (toSymbol domain, toSymbol d)),
	  fn i =>
	  let
	     val v as {uses, ...} =  Array.sub (a, i)
	     val _ = Uses.add (uses, domain)
	  in
	     v
	  end)

      val map: ('a, 'b) t * ('b -> 'b) -> ('a, 'b) t =
	 fn (T a, f) =>
	 T (Array.map (a, fn {domain, range, uses} =>
		       {domain = domain,
			range = f range,
			uses = uses}))

      val map2: ('a, 'b) t * ('a, 'b) t * ('b * 'b -> 'b) -> ('a, 'b) t =
	 fn (T a, T a', f) =>
	 T (Array.map2
	    (a, a', fn ({domain, range = r, uses}, {range = r', ...}) =>
	     {domain = domain,
	      range = f (r, r'),
	      uses = uses}))
   end

val allTycons: Tycon.t list ref = ref (List.map (Tycon.prims, #1))
val newTycons: (Tycon.t * Kind.t) list ref = ref []

val newTycon: string * Kind.t * AdmitsEquality.t -> Tycon.t =
   fn (s, k, a) =>
   let
      val c = Tycon.fromString s
      val _ = TypeEnv.initAdmitsEquality (c, a)
      val _ = List.push (allTycons, c)
      val _ = List.push (newTycons, (c, k))
   in
      c
   end

(* ------------------------------------------------- *)
(*                     Structure                     *)
(* ------------------------------------------------- *)

structure Structure =
   struct
      datatype t = T of {interface: Interface.t option,
			 plist: PropertyList.t,
			 strs: (Ast.Strid.t, t) Info.t,
			 types: (Ast.Tycon.t, TypeStr.t) Info.t,
			 vals: (Ast.Vid.t, Vid.t * Scheme.t) Info.t}

      local
	 fun make f (T r) = f r
      in
	 val interface = make #interface
	 val plist = make #plist
      end

      fun eq (s: t, s': t): bool = PropertyList.equals (plist s, plist s')

      local
	 fun make (field, toSymbol) (T fields, domain) =
	    Info.peek (field fields, domain, toSymbol)
      in
	 val peekStrid' = make (#strs, Ast.Strid.toSymbol)
	 val peekVid' = make (#vals, Ast.Vid.toSymbol)
	 val peekTycon' = make (#types, Ast.Tycon.toSymbol)
      end

      fun peekStrid z = Option.map (peekStrid' z, #range)
      fun peekTycon z = Option.map (peekTycon' z, #range)
      fun peekVid z = Option.map (peekVid' z, #range)

      local
	 fun make (from, de) (S, x) =
	    case peekVid (S, from x) of
	       NONE => NONE
	     | SOME (vid, s) => Option.map (de vid, fn z => (z, s))
      in
	 val peekCon = make (Ast.Vid.fromCon, Vid.deCon)
	 val peekVar = make (Ast.Vid.fromVar, Vid.deVar)
      end

      fun layout (T {strs, vals, types, ...}) =
	 Layout.record
	 [("types", Info.layout (Ast.Tycon.layout, TypeStr.layout) types),
	  ("vals", (Info.layout (Ast.Vid.layout,
				 Layout.tuple2 (Vid.layout, Scheme.layout))
		    vals)),
	  ("strs", Info.layout (Strid.layout, layout) strs)]

      fun hasInterface (S: t, I: Interface.t): bool =
	 case interface S of
	    NONE => false
	  | SOME I' => Shape.equals (Interface.shape I, Interface.shape I')

      val hasInterface =
	 Trace.trace2 ("Structure.hasInterface", layout, Interface.layout,
		       Bool.layout) hasInterface

      local
	 datatype handleUses = Clear | Force
	 fun make handleUses =
	    let
	       fun loop (T f) =
		  let
		     fun doit (sel, forceRange) =
			let
			   val Info.T a = sel f
			in
			   Array.foreach
			   (a, fn {range, uses, ...} =>
			    let
			       val _ =
				  case handleUses of
				     Clear => Uses.clear uses
				   | Force => Uses.forceUsed uses
			       val _ = forceRange range
			    in
			       ()
			    end)
			end
		     val _ = doit (#strs, loop)
		     val _ = doit (#types, ignore)
		     val _ = doit (#vals, ignore)
		  in
		     ()
		  end
	    in
	       loop
	    end
      in
	 val clearUsed = make Clear
	 val forceUsed = make Force
      end

      fun foreach2Sorted (abs: ('a * 'b) array,
			  info: ('a, 'c) Info.t,
			  equals: ('a * 'a -> bool),
			  f: ('a * 'b * 'c option -> unit)): unit =
	 let
	    val Info.T acs = info
	    val _ =
	       Array.fold
	       (abs, 0, fn ((a, b), i) =>
		let
		   fun find j =
		      if j = Array.length acs
			 then (i, NONE)
		      else
			 let
			    val {domain = a', range = c, ...} =
			       Array.sub (acs, j)
			 in
			    if equals (a, a')
			       then (j + 1, SOME c)
			    else find (j + 1)
			 end
		   val (i, co) = find i
		   val () = f (a, b, co)
		in
		   i
		end)
	 in
	    ()
	 end
      
      fun realize (S: t, tm: 'a TyconMap.t,
		   f: (Ast.Tycon.t
		       * 'a
		       * TypeStr.t option
		       * {nest: Strid.t list}) -> unit): unit =
	 let
	    fun allNone (TyconMap.T {strs, types}, nest) =
	       (Array.foreach (strs, fn (name, tm) => allNone (tm, name :: nest))
		; Array.foreach (types, fn (name, flex) =>
				 f (name, flex, NONE, {nest = nest})))
	    fun loop (TyconMap.T {strs, types},
		      S as T {strs = strs', types = types', ...},
		      nest: Strid.t list) =
	       let
		  val () =
		     foreach2Sorted
		     (strs, strs', Ast.Strid.equals,
		      fn (name, tm, S) =>
		      case S of
			 NONE => allNone (tm, nest)
		       | SOME S => loop (tm, S, name :: nest))
		  val () =
		     foreach2Sorted
		     (types, types', Ast.Tycon.equals,
		      fn (name, flex, opt) => f (name, flex, opt, {nest = nest}))
	       in
		   ()
	       end
	 in
	    loop (tm, S, [])
	 end

      local
	 open Layout
      in
	 fun layouts ({showUsed: bool},
		      shapeSigid: Shape.t -> (Sigid.t * Interface.t) option) =
	    let
	       fun layoutTypeSpec (n, s) =
		  layoutTypeSpec' (Ast.Tycon.layout n, s, {isWhere = false})
	       and layoutTypeSpec' (name: Layout.t, s, {isWhere: bool}) =
		  let
		     val {destroy, lay} = Type.makeLayoutPretty ()
		     val lay = #1 o lay
		     val tyvars =
			case TypeStr.kind s of
			   Kind.Arity n =>
			      Vector.tabulate
			      (n, fn _ =>
			       Type.var (Tyvar.newNoname {equality = false}))
			 | Kind.Nary => Vector.new0 ()
		     val args =
			case Vector.length tyvars of
			   0 => empty
			 | 1 => seq [lay (Vector.sub (tyvars, 0)), str " "]
			 | _ =>
			      seq
			      [paren (seq (separateRight
					   (Vector.toList (Vector.map (tyvars, lay)),
					    ", "))),
			       str " "]
		     val t =
			case TypeStr.node s of
			   TypeStr.Datatype _ => "datatype"
			 | _ =>
			      if isWhere
				 then "type"
			      else
				 let
				    datatype z = datatype AdmitsEquality.t
				 in
				    case TypeStr.admitsEquality s of
				       Always => "eqtype"
				     | Never => "type"
				     | Sometimes => "eqtype"   
				 end
                     val def = seq [str t, str " ", args, name, str " = "]
		     val res = 
			case TypeStr.node s of
			   TypeStr.Datatype {cons = Cons.T cs, tycon} =>
			      if isWhere
				 then seq [def, lay (Type.con (tycon, tyvars))]
			      else
				 let
				    val cs =
				       Vector.toListMap
				       (cs, fn {name, scheme, ...} =>
					seq [Ast.Con.layout name,
					     case (Type.deArrowOpt
						   (Scheme.apply (scheme, tyvars))) of
						NONE => empty
					      | SOME (t, _) => seq [str " of ", lay t]])
				 in
				    seq [def, alignPrefix (cs, "| ")]
				 end
			 | TypeStr.Scheme s =>
			      seq [def, lay (Scheme.apply (s, tyvars))]
			 | TypeStr.Tycon c =>
			      seq [def, lay (Type.con (c, tyvars))]
		     val _ = destroy ()
		  in
		     res
		  end
	       fun layoutValSpec (d: Ast.Vid.t, (vid, scheme)) =
		  let
		     fun simple s =
			seq [str s, str " ", Ast.Vid.layout d,
			     if Ast.Vid.isSymbolic d then str " " else empty,
			     str ": ", Scheme.layoutPretty scheme]
		     datatype z = datatype Vid.t
		  in
		     case vid of
			Con _ => NONE
		      | Exn c =>
			   SOME
			   (seq [str "exception ", Con.layout c, 
				 case Type.deArrowOpt (Scheme.ty scheme) of
				    NONE => empty
				  | SOME (t, _) =>
				       seq [str " of ", Type.layoutPretty t]])
		      | Overload  _ => SOME (simple "val")
		      | Var _ => SOME (simple "val")
		  end
	       fun layoutStrSpec (d: Strid.t, r) =
		  let
		     val (l, {messy}) = layoutAbbrev r
		     val bind = seq [str "structure ", Strid.layout d, str ":"]
		  in
		     if messy
			then align [bind, indent (l, 3)]
		     else seq [bind, str " ", l]
		  end
	       and layoutStr (T {strs, vals, types, ...}) =
		  let
		     fun doit (Info.T a, layout) =
			align (Array.foldr
			       (a, [], fn ({domain, range, uses}, ac) =>
				if showUsed andalso not (Uses.hasUse uses)
				   then ac
				else
				   case layout (domain, range) of
				      NONE => ac
				    | SOME l => l :: ac))
		  in
		     align
		     [str "sig",
		      indent (align [doit (types, SOME o layoutTypeSpec),
				     doit (vals, layoutValSpec),
				     doit (strs, SOME o layoutStrSpec)],
			      3),
		      str "end"]
		  end
               and layoutAbbrev (S as T {interface, ...}) =
		  case if showUsed
			  then NONE
		       else (case interface of
				NONE => NONE
			      | SOME I => shapeSigid (Interface.shape I)) of
		     NONE => (layoutStr S, {messy = true})
		   | SOME (s, I) =>
			let
			   val wheres = ref []
			   val () =
			      realize
			      (S, Interface.flexibleTycons I,
			       fn (name, flex, typeStr, {nest}) =>
			       case typeStr of
				  NONE => Error.bug "missing typeStr"
				| SOME typeStr =>
				     List.push
				     (wheres,
				      seq [str "where ",
					   layoutTypeSpec'
					   (Ast.Longtycon.layout
					    (Ast.Longtycon.long (rev nest,
								 name)),
					    typeStr,
					    {isWhere = true})]))
			in
			   (align (Sigid.layout s :: (rev (!wheres))),
			    {messy = false})
			end
	    in
	       {layoutAbbrev = layoutAbbrev,
		layoutStr = layoutStr,
		strSpec = layoutStrSpec,
		typeSpec = layoutTypeSpec,
		valSpec = layoutValSpec}
	    end
      end

      fun layoutPretty S =
	 #layoutStr (layouts ({showUsed = false}, fn _ => NONE)) S

      datatype 'a peekResult =
	 Found of 'a
	| UndefinedStructure of Strid.t list
	  
      fun peekStrids (S, strids) =
	 let
	    fun loop (S, strids, ac) =
	       case strids of
		  [] => Found S
		| strid :: strids =>
		     case peekStrid (S, strid) of
			NONE => UndefinedStructure (rev (strid :: ac))
		      | SOME S => loop (S, strids, strid :: ac)
	 in
	    loop (S, strids, [])
	 end

      fun peekLongtycon (S, t): TypeStr.t option =
	 let
	    val (strids, t) = Longtycon.split t
	 in
	    case peekStrids (S, strids) of
	       Found S => peekTycon (S, t)
	     | UndefinedStructure _ => NONE
	 end

      val ffi: t option ref = ref NONE
   end

structure FunctorClosure =
   struct
      datatype t =
	 T of {apply: Structure.t * string list -> Decs.t * Structure.t option,
	       argInt: Interface.t,
	       formal: Structure.t,
	       result: Structure.t option}

      local
	 fun make f (T r) = f r
      in
	 val argInterface = make #argInt
	 val result = make #result
      end

      fun layout _ = Layout.str "<functor closure>"

      fun apply (T {apply, ...}, S, nest) = apply (S, nest)

      val apply =
	 Trace.trace3 ("FunctorClosure.apply",
		       layout,
		       Structure.layout,
		       List.layout String.layout,
		       (Option.layout Structure.layout) o #2)
	 apply
   end

structure Time:>
   sig
      type t

      val >= : t * t -> bool
      val next: unit -> t
   end =
   struct
      type t = int

      val op >= : t * t -> bool = op >=

      val c = Counter.new 0

      fun next () = Counter.next c
   end

(* ------------------------------------------------- *)
(*                     NameSpace                     *)
(* ------------------------------------------------- *)

structure Values =
   struct
      type ('a, 'b) value = {domain: 'a,
			     range: 'b,
			     scope: Scope.t,
			     time: Time.t,
			     uses: 'a Uses.t}
      (* The domains of all elements in a values list have the same symbol. *)
      datatype ('a, 'b) t = T of ('a, 'b) value list ref

      fun new (): ('a, 'b) t = T (ref [])

      fun ! (T r) = Ref.! r

      fun pop (T r) = List.pop r
   end

structure NameSpace =
   struct
      datatype ('a, 'b) t =
	 T of {class: 'b -> Class.t,
	       current: ('a, 'b) Values.t list ref,
	       defUses: {class: Class.t,
			 def: 'a,
			 uses: 'a Uses.t} list ref,
	       lookup: 'a -> ('a, 'b) Values.t,
	       region: 'a -> Region.t,
	       toSymbol: 'a -> Symbol.t}

      fun values (T {lookup, ...}, a) = lookup a

      fun new {class, lookup, region, toSymbol} =
	 T {class = class,
	    current = ref [],
	    defUses = ref [],
	    lookup = lookup,
	    region = region,
	    toSymbol = toSymbol}

      fun newUses (T {defUses, ...}, class, def) =
	 let
	    val u = Uses.new ()
	    val _ = List.push (defUses, {class = class,
					 def = def,
					 uses = u})
	 in
	    u
	 end

      fun ('a, 'b) peek (ns, a: 'a, {markUse: 'b -> bool})
	 : 'b option =
	 case Values.! (values (ns, a)) of
	    [] => NONE
	  | {range, uses, ...} :: _ => 
	       (if markUse range then Uses.add (uses, a) else ()
		; SOME range)

      fun collect (T {current, toSymbol, ...}: ('a, 'b) t)
	 : unit -> ('a, 'b) Info.t =
	 let
	    val old = !current
	    val _ = current := []
	 in
	    fn () =>
	    let
	       val elts =
		  List.revMap (!current, fn values =>
			       let
				  val {domain, range, uses, ...} =
				     Values.pop values
			       in
				  {domain = domain,
				   range = range,
				   uses = uses}
			       end)
	       val _ = current := old
	       val a =
		  QuickSort.sortArray
		  (Array.fromList elts,
		   fn ({domain = d, ...}, {domain = d', ...}) =>
		   Symbol.<= (toSymbol d, toSymbol d'))
	    in
	       Info.T a
	    end
	 end
   end

(*---------------------------------------------------*)
(*                 Main Env Datatype                 *)
(*---------------------------------------------------*)

structure All =
   struct
      datatype t =
	 Fct of (Fctid.t, FunctorClosure.t) Values.t
       | Fix of (Ast.Vid.t, Ast.Fixity.t) Values.t
       | Sig of (Sigid.t, Interface.t) Values.t
       | Str of (Strid.t, Structure.t) Values.t
       | Tyc of (Ast.Tycon.t, TypeStr.t) Values.t
       | Val of (Ast.Vid.t, Vid.t * Scheme.t) Values.t

      val fctOpt = fn Fct z => SOME z | _ => NONE
      val fixOpt = fn Fix z => SOME z | _ => NONE
      val sigOpt = fn Sig z => SOME z | _ => NONE
      val strOpt = fn Str z => SOME z | _ => NONE
      val tycOpt = fn Tyc z => SOME z | _ => NONE
      val valOpt = fn Val z => SOME z | _ => NONE
   end

datatype t = T of {currentScope: Scope.t ref,
		   fcts: (Fctid.t, FunctorClosure.t) NameSpace.t,
		   fixs: (Ast.Vid.t, Ast.Fixity.t) NameSpace.t,
		   lookup: Symbol.t -> All.t list ref,
		   maybeAddTop: Symbol.t -> unit,
		   sigs: (Sigid.t, Interface.t) NameSpace.t,
		   strs: (Strid.t, Structure.t) NameSpace.t,
		   (* topSymbols is a list of all symbols that are defined at
		    * the top level (in any namespace).
		    *)
		   topSymbols: Symbol.t list ref,
		   types: (Ast.Tycon.t, TypeStr.t) NameSpace.t,
		   vals: (Ast.Vid.t, Vid.t * Scheme.t) NameSpace.t}

fun sizeMessage (E: t): Layout.t =
   let
      val size = MLton.size
      open Layout
   in
      record [("total", Int.layout (size E))]
   end

fun empty () =
   let
      val {get = lookupAll: Symbol.t -> All.t list ref, ...} = 
	 Property.get (Symbol.plist, Property.initFun (fn _ => ref []))
      val topSymbols = ref []
      val {get = maybeAddTop: Symbol.t -> unit, ...} =
	 Property.get (Symbol.plist,
		       Property.initFun (fn s => List.push (topSymbols, s)))
      fun ('a, 'b) make (class: 'b -> Class.t,
			 region: 'a -> Region.t,
			 toSymbol: 'a -> Symbol.t,
			 extract: All.t -> ('a, 'b) Values.t option,
			 make: ('a, 'b) Values.t -> All.t)
	 : ('a, 'b) NameSpace.t  =
	 let
	    fun lookup (a: 'a): ('a, 'b) Values.t =
	       let
		  val r = lookupAll (toSymbol a)
	       in
		  case List.peekMap (!r, extract) of
		     NONE =>
			let
			   val v = Values.new ()
			   val _ = List.push (r, make v)
			in
			   v
			end
		   | SOME v => v
	       end
	 in
	    NameSpace.new {class = class,
			   lookup = lookup,
			   region = region,
			   toSymbol = toSymbol}
	 end
      val fcts = make (fn _ => Class.Fct, Fctid.region, Fctid.toSymbol,
		       All.fctOpt, All.Fct)
      val fixs = make (fn _ => Class.Fix, Ast.Vid.region, Ast.Vid.toSymbol,
		       All.fixOpt, All.Fix)
      val sigs = make (fn _ => Class.Sig, Sigid.region, Sigid.toSymbol,
		       All.sigOpt, All.Sig)
      val strs = make (fn _ => Class.Str, Strid.region, Strid.toSymbol,
		       All.strOpt, All.Str)
      val types = make (fn _ => Class.Typ, Ast.Tycon.region, Ast.Tycon.toSymbol,
			All.tycOpt, All.Tyc)
      val vals = make (Vid.class o #1, Ast.Vid.region, Ast.Vid.toSymbol,
		       All.valOpt, All.Val)
   in
      T {currentScope = ref (Scope.new {isTop = true}),
	 fcts = fcts,
	 fixs = fixs,
	 lookup = lookupAll,
	 maybeAddTop = maybeAddTop,
	 sigs = sigs,
	 strs = strs,
	 topSymbols = topSymbols,
	 types = types,
	 vals = vals}
   end

local
   fun foreach (T {lookup, ...}, s, {fcts, fixs, sigs, strs, types, vals}) =
      List.foreach
      (! (lookup s), fn a =>
       let
	  datatype z = datatype All.t
       in
	  case a of
	     Fct vs => fcts vs
	   | Fix vs => fixs vs
	   | Sig vs => sigs vs
	   | Str vs => strs vs
	   | Tyc vs => types vs
	   | Val vs => vals vs
       end)
in
   fun foreachDefinedSymbol (E, z) =
      Symbol.foreach (fn s => foreach (E, s, z))

   fun foreachTopLevelSymbol (E as T {topSymbols, ...}, z) =
      List.foreach (!topSymbols, fn s => foreach (E, s, z))
end

fun collect (E,
	     keep: {hasUse: bool, scope: Scope.t} -> bool,
	     le: {domain: Symbol.t, time: Time.t}
	         * {domain: Symbol.t, time: Time.t} -> bool) =
   let
      val fcts = ref []
      val sigs = ref []
      val strs = ref []
      val types = ref []
      val vals = ref []
      fun doit ac vs =
	 case Values.! vs of
	    [] => ()
	  | (z as {scope, uses, ...}) :: _ =>
	       if keep {hasUse = Uses.hasUse uses, scope = scope}
		  then List.push (ac, z)
	       else ()
      val _ =
	 foreachDefinedSymbol (E, {fcts = doit fcts,
				   fixs = fn _ => (),
				   sigs = doit sigs,
				   strs = doit strs,
				   types = doit types,
				   vals = doit vals})
      fun ('a, 'b) finish (r, toSymbol: 'a -> Symbol.t) =
	 QuickSort.sortArray
	 (Array.fromList (!r),
	  fn ({domain = d, time = t, ...}: ('a, 'b) Values.value,
	      {domain = d', time = t',...}: ('a, 'b) Values.value) =>
	  le ({domain = toSymbol d, time = t},
	      {domain = toSymbol d', time = t'}))
   in
      {fcts = finish (fcts, Fctid.toSymbol),
       sigs = finish (sigs, Sigid.toSymbol),
       strs = finish (strs, Strid.toSymbol),
       types = finish (types, Ast.Tycon.toSymbol),
       vals = finish (vals, Ast.Vid.toSymbol)}
   end

fun setTyconNames (E: t): unit =
   let
      val {get = shortest: Tycon.t -> int ref, ...} =
	 Property.get (Tycon.plist, Property.initFun (fn _ => ref Int.maxInt))
      fun doType (typeStr: TypeStr.t,
		  name: Ast.Tycon.t,
		  length: int,
		  strids: Strid.t list): unit =
	 case TypeStr.toTyconOpt typeStr of
	    NONE => ()
	  | SOME c => 
	       let
		  val r = shortest c
	       in
		  if length >= !r
		     then ()
		  else
		     let
			val _ = r := length
			val name =
			   Pretty.longid (List.map (strids, Strid.layout),
					  Ast.Tycon.layout name)
		     in
			Tycon.setPrintName (c, Layout.toString name)
		     end
	       end
      val {get = strShortest: Structure.t -> int ref, ...} =
	 Property.get (Structure.plist,
		       Property.initFun (fn _ => ref Int.maxInt))
      fun loopStr (s as Structure.T {strs, types, ...},
		   length: int,
		   strids: Strid.t list)
	 : unit =
	 let
	    val r = strShortest s
	 in
	    if length >= !r
	       then ()
	    else
	       (r := length
		; Info.foreach (types, fn (name, typeStr) =>
				doType (typeStr, name, length, strids))
		; Info.foreach (strs, fn (strid, str) =>
				loopStr (str, 1 + length, strids @ [strid])))
	 end
      (* Sort the declarations in decreasing order of definition time so that
       * later declarations will be processed first, and hence will take
       * precedence.
       *)
      val {strs, types, ...} =
	 collect (E, fn _ => true,
		  fn ({time = t, ...}, {time = t', ...}) => Time.>= (t, t'))
      val _ = Array.foreach (types, fn {domain = name, range = typeStr, ...} =>
			     doType (typeStr, name, 0, []))
      val _ = Array.foreach (strs, fn {domain = strid, range = str, ...} =>
			     loopStr (str, 1, [strid]))
      val _ =
	 List.foreach
	 (!allTycons, fn c =>
	  if ! (shortest c) < Int.maxInt
	     then ()
	  else
	     Tycon.setPrintName (c, concat ["?.", Tycon.originalName c]))
   in
      ()
   end

fun dummyStructure (I: Interface.t, {prefix: string})
   : Structure.t * (Structure.t * (Tycon.t * TypeStr.t -> unit) -> unit) =
   let
      val I = Interface.copy I
      fun realize (TyconMap.T {strs, types}, nest) =
	 let
	    val strs =
	       Array.map (strs, fn (name, tm) =>
			  (name, realize (tm, name :: nest)))
	    val types =
	       Array.map
	       (types, fn (tycon, flex) =>
		let
		   val {admitsEquality = a, kind = k, ...} =
		      FlexibleTycon.dest flex
		   val name =
		      concat (prefix
			      :: (List.fold (nest, [Ast.Tycon.toString tycon],
					     fn (s, ss) =>
					     Strid.toString s :: "." :: ss)))
		   val c = newTycon (name, k, a)
		   val _ = FlexibleTycon.realize (flex, TypeStr.tycon (c, k))
		in
		   (tycon, c)
		end)
	 in
	    TyconMap.T {strs = strs, types = types}
	 end
      val flexible = realize (Interface.flexibleTycons I, [])
      val {get, ...} =
	 Property.get
	 (Interface.plist,
	  Property.initRec
	  (fn (I, get) =>
	   let
	      val {strs, types, vals} = Interface.dest I
	      val strs =
		 Array.map (strs, fn (name, I) =>
			    {domain = name,
			     range = get I,
			     uses = Uses.new ()})
	      val types =
		 Array.map (types, fn (name, s) =>
			    {domain = name,
			     range = Interface.TypeStr.toEnv s,
			     uses = Uses.new ()})
	      val vals =
		 Array.map (vals, fn (name, (status, scheme)) =>
			    let
			       val con = CoreML.Con.fromString o Ast.Vid.toString
			       val var = CoreML.Var.fromString o Ast.Vid.toString
			       val vid =
				  case status of
				     Status.Con => Vid.Con (con name)
				   | Status.Exn => Vid.Exn (con name)
				   | Status.Var => Vid.Var (var name)
			    in
			       {domain = name,
				range = (vid, Interface.Scheme.toEnv scheme),
				uses = Uses.new ()}
			    end)
	   in
	      Structure.T {interface = SOME I,
			   plist = PropertyList.new (),
			   strs = Info.T strs,
			   types = Info.T types,
			   vals = Info.T vals}
	   end))
      val S = get I
      fun instantiate (S, f) =
	 Structure.realize (S, flexible, fn (_, c, so, _) =>
			    case so of
			       NONE => Error.bug "instantiate"
			     | SOME s => f (c, s))
   in
      (S, instantiate)
   end

val dummyStructure =
   Trace.trace ("dummyStructure",
		Interface.layout o #1,
		Structure.layoutPretty o #1)
   dummyStructure

fun layout' (E: t, keep, showUsed): Layout.t =
   let
      val _ = setTyconNames E
      val {fcts, sigs, strs, types, vals} =
	 collect (E, keep,
		  fn ({domain = d, ...}, {domain = d', ...}) =>
		  Symbol.<= (d, d'))
      open Layout
      fun doit (a, layout) = align (Array.toListMap (a, layout))
      val {get = shapeSigid: Shape.t -> (Sigid.t * Interface.t) option,
	   set = setShapeSigid, ...} =
	 Property.getSet (Shape.plist, Property.initConst NONE)
      val _ = Array.foreach (sigs, fn {domain = s, range = I, ...} =>
			     setShapeSigid (Interface.shape I, SOME (s, I)))
      val {strSpec, typeSpec, valSpec, ...} =
	 Structure.layouts (showUsed, shapeSigid)
      val {layoutAbbrev, layoutStr, ...} =
	 Structure.layouts ({showUsed = false}, shapeSigid)
      val sigs =
	 doit (sigs, fn {domain = sigid, range = I, ...} =>
	       let
		  val (S, _) = dummyStructure (I, {prefix = "?."})
	       in
		  align [seq [str "signature ", Sigid.layout sigid, str " = "],
			 indent (layoutStr S, 3)]
	       end)
      val fcts =
	 doit (fcts,
	       fn {domain,
		   range = FunctorClosure.T {formal, result, ...}, ...} =>
	       align [seq [str "functor ", Fctid.layout domain, str " ",
			   paren (seq [str "S: ", #1 (layoutAbbrev formal)])],
		      case result of
			   NONE => empty
			 | SOME S =>
			      indent (seq [str ": ", #1 (layoutAbbrev S)], 3)])
      val vals = align (Array.foldr (vals, [], fn ({domain, range, ...}, ac) =>
				     case valSpec (domain, range) of
					NONE => ac
				      | SOME l => l :: ac))
      val types = doit (types, fn {domain, range, ...} =>
			typeSpec (domain, range))
      val strs = doit (strs, fn {domain, range, ...} => strSpec (domain, range))
   in
      align [types, vals, strs, fcts, sigs]
   end

fun layout E = layout' (E, fn _ => true, {showUsed = false})

fun layoutCurrentScope (E as T {currentScope, ...}) =
   let
      val s = !currentScope
   in
      layout' (E, fn {scope, ...} => Scope.equals (s, scope),
	       {showUsed = false})
   end

fun layoutUsed (E: t): Layout.t = layout' (E, #hasUse, {showUsed = true})

fun clearDefUses (E as T f) =
   let
      fun doit sel =
	 let
	    val NameSpace.T {defUses, ...} = sel f
	 in
	    defUses := []
	 end
      val _ = doit #fcts
      val _ = doit #fixs
      val _ = doit #sigs
      val _ = doit #strs
      val _ = doit #types
      val _ = doit #vals
      fun doit clearRange (Values.T r) =
	 case !r of
	    [] => ()
	  | {range, uses, ...} :: _ =>
	       (Uses.clear uses
		; clearRange range)
      val _ =
	 foreachDefinedSymbol
	 (E, {fcts = doit ignore,
	      fixs = doit ignore,
	      sigs = doit ignore,
	      strs = doit Structure.clearUsed,
	      types = doit ignore,
	      vals = doit ignore})
   in
      ()
   end

(* Force everything that is currently in scope to be marked as used. *)
fun forceUsed E =
   let
      fun doit forceRange (Values.T r) =
	 case !r of
	    [] => ()
	  | {uses, range, ...} :: _ =>
	       (Uses.forceUsed uses
		; forceRange range)
      val _ =
	 foreachDefinedSymbol
	 (E, {fcts = doit (fn f => Option.app (FunctorClosure.result f,
					       Structure.forceUsed)),
	      fixs = doit ignore,
	      sigs = doit ignore,
	      strs = doit Structure.forceUsed,
	      types = doit ignore,
	      vals = doit ignore})
   in
      ()
   end

fun processDefUse (E as T f) =
   let
      val _ = forceUsed E
      val all: {class: Class.t,
		def: Layout.t,
		isUsed: bool,
		region: Region.t,
		uses: Region.t list} list ref = ref []
      fun doit sel =
	 let
	    val NameSpace.T {defUses, region, toSymbol, ...} = sel f
	 in
	    List.foreach
	    (!defUses, fn {class, def, uses, ...} =>
	     List.push
	     (all, {class = class,
		    def = Symbol.layout (toSymbol def),
		    isUsed = Uses.isUsed uses,
		    region = region def,
		    uses = List.fold (Uses.all uses, [], fn (u, ac) =>
				      region u :: ac)}))
	 end
      val _ = doit #fcts
      val _ = doit #sigs
      val _ = doit #strs
      val _ = doit #types
      val _ = doit #vals
      val a = Array.fromList (!all)
      val _ =
	 QuickSort.sortArray (a, fn ({region = r, ...}, {region = r', ...}) =>
			      Region.<= (r, r'))
      val l =
	 Array.foldr
	 (a, [], fn (z as {class, def, isUsed, region, uses}, ac) =>
	  case ac of
	     [] => [z]
	   | {isUsed = i', region = r', uses = u', ...} :: ac' =>
		if Region.equals (region, r')
		   then {class = class,
			 def = def,
			 isUsed = isUsed orelse i',
			 region = region,
			 uses = uses @ u'} :: ac'
		else z :: ac)
      val _ =
	 if not (!Control.warnUnused)
	    then ()
	 else
	    List.foreach
	    (l, fn {class, def, isUsed, region, ...} =>
	     if isUsed orelse Option.isNone (Region.left region)
		then ()
	     else
		let
		   open Layout
		in
		   Control.warning
		   (region,
		    seq [str (concat ["unused ", Class.toString class, ": "]),
			 def],
		    empty)
		end)
      val _ =
	 case !Control.showDefUse of
	    NONE => ()
	  | SOME f =>
	       File.withOut
	       (f, fn out =>
		List.foreach
		(l, fn {class, def, region, uses, ...} =>
		 case Region.left region of
		    NONE => ()
		  | SOME p =>
		       let
			  val uses = Array.fromList uses
			  val _ = QuickSort.sortArray (uses, Region.<=)
			  val uses =
			     Array.foldr
			     (uses, [], fn (r, ac) =>
			      case ac of
				 [] => [r]
			       | r' :: _ =>
				    if Region.equals (r, r')
				       then ac
				    else r :: ac)
			  open Layout
		       in
			  outputl
			  (align [seq [str (Class.toString class),
				       str " ",
				       def,
				       str " ",
				       str (SourcePos.toString p)],
				  indent
				  (align
				   (List.map
				    (uses, fn r =>
				     str (concat [case Region.left r of
						     NONE => "NONE"
						   | SOME p =>
							SourcePos.toString p,
						  " "]))),
				   4)],
			   out)
		       end))
   in
      ()
   end

fun newCons (T {vals, ...}, v) =
   let
      val forceUsed = 1 = Vector.length v
      val v =
	 Vector.map (v, fn {con, name} =>
		     let
			val uses = NameSpace.newUses (vals, Class.Con,
						      Ast.Vid.fromCon name)
			val _ = if forceUsed then Uses.forceUsed uses else ()
		     in
			{con = con,
			 name = name,
			 uses = uses}
		     end)
   in
      fn v' => Cons.T (Vector.map2
		       (v, v', fn ({con, name, uses}, scheme) =>
			{con = con,
			 name = name,
			 scheme = scheme,
			 uses = uses}))
   end

(* ------------------------------------------------- *)
(*                       peek                        *)
(* ------------------------------------------------- *)

local
   fun make sel (T r, a) = NameSpace.peek (sel r, a, {markUse = fn _ => true})
in
   val peekFctid = make #fcts
   val peekFix = make #fixs
   val peekSigid = make #sigs
   val peekStrid = make #strs
   val peekTycon = make #types
   val peekVid = make #vals
   fun peekVar (E, x) =
      case peekVid (E, Ast.Vid.fromVar x) of
	 NONE => NONE
       | SOME (vid, s) => Option.map (Vid.deVar vid, fn x => (x, s))
end

fun peekCon (T {vals, ...}, c: Ast.Con.t): (Con.t * Scheme.t) option =
   case NameSpace.peek (vals, Ast.Vid.fromCon c,
			{markUse = fn (vid, _) => isSome (Vid.deCon vid)}) of
      NONE => NONE
    | SOME (vid, s) => Option.map (Vid.deCon vid, fn c => (c, s))

fun layoutLong (ids: Layout.t list) =
   let
      open Layout
   in
      seq (separate (ids, "."))
   end

fun layoutStrids (ss: Strid.t list): Layout.t =
   layoutLong (List.map (ss, Strid.layout))
   
structure PeekResult =
   struct
      datatype 'a t =
	 Found of 'a
       | UndefinedStructure of Strid.t list
       | Undefined

      fun map (r: 'a t, f: 'a -> 'b): 'b t =
	 case r of
	    Found a => Found (f a)
	  | UndefinedStructure ss => UndefinedStructure ss
	  | Undefined => Undefined
	       
      val toOption: 'a t -> 'a option =
	 fn Found z => SOME z
	  | _ => NONE
   end
    
local
   datatype z = datatype PeekResult.t
   fun make (split: 'a -> Strid.t list * 'b,
	     peek: t * 'b -> 'c option,
	     strPeek: Structure.t * 'b -> 'c option) (E, x) =
      let
	 val (strids, x) = split x
      in
	 case strids of
	    [] => (case peek (E, x) of
		      NONE => Undefined
		    | SOME z => Found z)
	  | strid :: strids =>
	       case peekStrid (E, strid) of
		  NONE => UndefinedStructure [strid]
		| SOME S =>
		     case Structure.peekStrids (S, strids) of
			Structure.Found S =>
			   (case strPeek (S, x) of
			       NONE => Undefined
			     | SOME z => Found z)
		      | Structure.UndefinedStructure ss =>
			   UndefinedStructure (strid :: ss)
      end
in
   val peekLongstrid =
      make (Ast.Longstrid.split, peekStrid, Structure.peekStrid)
   val peekLongtycon =
      make (Longtycon.split, peekTycon, Structure.peekTycon)
   val peekLongvar = make (Ast.Longvar.split, peekVar, Structure.peekVar)
   val peekLongvid = make (Ast.Longvid.split, peekVid, Structure.peekVid)
   val peekLongcon = make (Ast.Longcon.split, peekCon, Structure.peekCon)
end

(* ------------------------------------------------- *)
(*                      lookup                       *)
(* ------------------------------------------------- *)

fun unbound (r: Region.t, className, x: Layout.t): unit =
   Control.error
   (r,
    let open Layout
    in seq [str "undefined ", str className, str " ", x]
    end,
    Layout.empty)

fun lookupFctid (E, x) =
   case peekFctid (E, x) of
      NONE => (unbound (Ast.Fctid.region x, "functor", Ast.Fctid.layout x)
	       ; NONE)
    | SOME f => SOME f

fun lookupSigid (E, x) =
   case peekSigid (E, x) of
      NONE => (unbound (Ast.Sigid.region x, "signature", Ast.Sigid.layout x)
	       ; NONE)
    | SOME I => SOME I

local
   fun make (peek: t * 'a -> 'b PeekResult.t,
	     bogus: unit -> 'b,
	     className: string,
	     region: 'a -> Region.t,
	     layout: 'a -> Layout.t)
      (E: t, x: 'a): 'b =
      let
	 datatype z = datatype PeekResult.t
      in
	 case peek (E, x) of
	    Found z => z
	  | UndefinedStructure ss =>
	       (unbound (region x, "structure", layoutStrids ss); bogus ())
	  | Undefined =>
	       (unbound (region x, className, layout x); bogus ())
      end
in
   val lookupLongcon =
      make (peekLongcon,
	    fn () => (Con.bogus, Scheme.bogus ()),
	    "constructor",
	    Ast.Longcon.region,
	    Ast.Longcon.layout)
   val lookupLongstrid =
      make (fn (E, x) => PeekResult.map (peekLongstrid (E, x), SOME),
	    fn () => NONE,
	    "structure",
	    Ast.Longstrid.region,
	    Ast.Longstrid.layout)
   val lookupLongtycon =
      make (peekLongtycon,
	    fn () => TypeStr.bogus Kind.Nary,
	    "type",
	    Longtycon.region,
	    Longtycon.layout)
   val lookupLongvid =
      make (peekLongvid,
	    fn () => (Vid.bogus, Scheme.bogus ()),
	    "variable",
	    Ast.Longvid.region,
	    Ast.Longvid.layout)
   val lookupLongvar =
      make (peekLongvar,
	    fn () => (Var.bogus, Scheme.bogus ()),
	    "variable",
	    Ast.Longvar.region,
	    Ast.Longvar.layout)
end

val peekLongcon = PeekResult.toOption o peekLongcon
val peekLongtycon = PeekResult.toOption o peekLongtycon
   
(* ------------------------------------------------- *)
(*                      extend                       *)
(* ------------------------------------------------- *)

structure ExtendUses =
   struct
      datatype 'a t =
	 New
       | Old of 'a Uses.t
       | Rebind

      fun fromIsRebind {isRebind} = if isRebind then Rebind else New
   end

val extend:
   t * ('a, 'b) NameSpace.t * {domain: 'a,
			       forceUsed: bool,
			       range: 'b,
			       scope: Scope.t,
			       time: Time.t,
			       uses: 'a ExtendUses.t} -> unit =
   fn (T {maybeAddTop, ...},
       ns as NameSpace.T {class, current, lookup, toSymbol, ...},
       {domain, forceUsed, range, scope, time, uses}) =>
   let
      fun newUses () =
	 let
	    val u = NameSpace.newUses (ns, class range, domain)
	    val _ = if forceUsed then Uses.forceUsed u else ()
	 in
	    u
	 end
      val values as Values.T r = lookup domain
      datatype z = datatype ExtendUses.t
      fun new () =
	 let
	    val _ = List.push (current, values)
	    val uses =
	       case uses of
		  New => newUses ()
		| Old u => u
		| Rebind => Error.bug "rebind new"
	 in
	    {domain = domain,
	     range = range,
	     scope = scope,
	     time = time,
	     uses = uses}
	 end
   in
      case !r of
	 [] =>
	    let
	       val _ =
		  if Scope.isTop scope
		     then maybeAddTop (toSymbol domain)
		  else ()
	    in
	       r := [new ()]
	    end
       | all as ({scope = scope', uses = uses', ...} :: rest) =>
	    if Scope.equals (scope, scope')
	       then
		  let
		     val uses =
			case uses of
			   New => newUses ()
			 | Old u => u
			 | Rebind => uses'
		  in
		     r := {domain = domain,
			   range = range,
			   scope = scope,
			   time = time,
			   uses = uses} :: rest
		  end
	    else r := new () :: all
   end

local
   val extend =
      fn (E as T (fields as {currentScope, ...}), get,
	  domain: 'a,
	  range: 'b,
	  forceUsed: bool,
	  uses: 'a ExtendUses.t) =>
      let
	 val ns = get fields
      in
	 extend (E, ns, {domain = domain,
			 forceUsed = forceUsed,
			 range = range,
			 scope = !currentScope,
			 time = Time.next (),
			 uses = uses})
      end
in
   fun extendFctid (E, d, r) =
      extend (E, #fcts, d, r, false, ExtendUses.New)
   fun extendFix (E, d, r) =
      extend (E, #fixs, d, r, false, ExtendUses.New)
   fun extendSigid (E, d, r) =
      extend (E, #sigs, d, r, false, ExtendUses.New)
   fun extendStrid (E, d, r) =
      extend (E, #strs, d, r, false, ExtendUses.New)
   fun extendVals (E, d, r, eu) = extend (E, #vals, d, r, false, eu)
   fun extendTycon (E, d, s, ir) =
      let
	 val forceTyconUsed =
	    let
	       datatype z = datatype TypeStr.node
	    in
	       case TypeStr.node s of
		  Datatype {cons = Cons.T v , ...} =>
		     let
			val _ = 
			   Vector.foreach
			   (v, fn {con, name, scheme, uses} => 
			    extendVals (E, Ast.Vid.fromCon name,
					(Vid.Con con, scheme),
					ExtendUses.Old uses))
		     in
			true
		     end
		| Scheme _ => false
		| Tycon _ => true
	    end
	 val _ = extend (E, #types, d, s, forceTyconUsed,
			 ExtendUses.fromIsRebind ir)
      in
	 ()
      end
end
       
fun extendExn (E, c, c', s) =
   extendVals (E, Ast.Vid.fromCon c, (Vid.Exn c', s), ExtendUses.New)
	       
fun extendVar (E, x, x', s, ir) =
   extendVals (E, Ast.Vid.fromVar x, (Vid.Var x', s),
	       ExtendUses.fromIsRebind ir)

val extendVar =
   Trace.trace
   ("extendVar",
    fn (_, x, x', s, _) =>
    Layout.tuple [Ast.Var.layout x, Var.layout x', Scheme.layoutPretty s],
    Unit.layout)
   extendVar

fun extendOverload (E, p, x, yts, s) =
   extendVals (E, Ast.Vid.fromVar x, (Vid.Overload (p, yts), s), ExtendUses.New)

(* ------------------------------------------------- *)   
(*                       local                       *)
(* ------------------------------------------------- *)

local
   fun doit (E: t, ns as NameSpace.T {current, ...}, s0) =
      let
	 val old = !current
	 val _ = current := []
      in
	 fn () =>
	 let
	    val c1 = !current
	    val _ = current := []
	 in
	    fn () =>
	    let
	       val c2 = !current
	       val lift = List.revMap (c2, Values.pop)
	       val _ = List.foreach (c1, fn v => (Values.pop v; ()))
	       val _ = current := old
	       val _ =
		  List.foreach (lift, fn {domain, range, time, uses, ...} =>
				extend (E, ns, {domain = domain,
						forceUsed = false,
						range = range,
						scope = s0,
						time = time,
						uses = ExtendUses.Old uses}))
	    in
	       ()
	    end
	 end
      end
in
   fun localTop (E as T {currentScope, fcts, fixs, sigs, strs, types, vals, ...},
		 f) =
      let
	 val s0 = !currentScope
	 val fcts = doit (E, fcts, s0)
	 val fixs = doit (E, fixs, s0)
	 val sigs = doit (E, sigs, s0)
	 val strs = doit (E, strs, s0)
	 val types = doit (E, types, s0)
	 val vals = doit (E, vals, s0)
	 val _ = currentScope := Scope.new {isTop = true}
	 val a = f ()
	 val fcts = fcts ()
	 val fixs = fixs ()
	 val sigs = sigs ()
	 val strs = strs ()
	 val types = types ()
	 val vals = vals ()
	 fun finish g =
	    let
	       val _ = currentScope := Scope.new {isTop = true}
	       val b = g ()
	       val _ = (fcts (); fixs (); sigs (); strs (); types (); vals ())
	       val _ = currentScope := s0
	    in
	       b
	    end
      in
	 (a, finish)
      end

   fun localModule (E as T {currentScope, fixs, strs, types, vals, ...},
		    f1, f2) =
      let
	 val s0 = !currentScope
	 val fixs = doit (E, fixs, s0)
	 val strs = doit (E, strs, s0)
	 val types = doit (E, types, s0)
	 val vals = doit (E, vals, s0)
	 val _ = currentScope := Scope.new {isTop = false}
	 val a1 = f1 ()
	 val fixs = fixs ()
	 val strs = strs ()
	 val types = types ()
	 val vals = vals ()
	 val _ = currentScope := Scope.new {isTop = false}
	 val a2 = f2 a1
	 val _ = (fixs (); strs (); types (); vals ())
	 val _ = currentScope := s0
      in
	 a2
      end

   (* Can't eliminate the use of strs in localCore, because openn still modifies
    * module level constructs.
    *)
   val localCore = localModule
end

fun makeStructure (T {currentScope, fixs, strs, types, vals, ...}, make) =
   let
      val f = NameSpace.collect fixs
      val s = NameSpace.collect strs
      val t = NameSpace.collect types
      val v = NameSpace.collect vals
      val s0 = !currentScope
      val _ = currentScope := Scope.new {isTop = false}
      val res = make ()
      val _ = f ()
      val S = Structure.T {interface = NONE,
			   plist = PropertyList.new (),
			   strs = s (),
			   types = t (),
			   vals = v ()}
      val _ = currentScope := s0
   in
      (res, S)
   end

fun scope (T {currentScope, fixs, strs, types, vals, ...}, th) =
   let
      fun doit (NameSpace.T {current, ...}) =
	 let
	    val old = !current
	    val _ = current := []
	 in fn () => (List.foreach (!current, fn v => (Values.pop v; ()))
		      ; current := old)
	 end
      val s0 = !currentScope
      val _ = currentScope := Scope.new {isTop = false}
      val f = doit fixs 
      val s = doit strs
      val t = doit types
      val v = doit vals
      val res = th ()
      val _ = (f (); s (); t (); v ())
      val _ = currentScope := s0
   in
      res
   end

fun scopeAll (T {currentScope, fcts, fixs, sigs, strs, types, vals, ...}, th) =
   let
      fun doit (NameSpace.T {current, ...}) =
	 let
	    val old = !current
	    val _ = current := []
	 in fn () => (List.foreach (!current, fn v => (Values.pop v; ()))
		      ; current := old)
	 end
      val s0 = !currentScope
      val _ = currentScope := Scope.new {isTop = true}
      val fc = doit fcts
      val f = doit fixs
      val si = doit sigs
      val s = doit strs
      val t = doit types
      val v = doit vals
      val res = th ()
      val _ = (fc (); f (); si (); s (); t (); v ())
      val _ = currentScope := s0
   in
      res
   end
   
fun openStructure (E as T {currentScope, strs, vals, types, ...},
		   Structure.T {strs = strs',
				vals = vals',
				types = types', ...}): unit =
   let
      val scope = !currentScope
      fun doit (ns, Info.T a) =
	 Array.foreach (a, fn {domain, range, uses} =>
			extend (E, ns, {domain = domain,
					forceUsed = false,
					range = range,
					scope = scope,
					time = Time.next (),
					uses = ExtendUses.Old uses}))
      val _ = doit (strs, strs')
      val _ = doit (vals, vals')
      val _ = doit (types, types')
   in
      ()
   end

fun makeOpaque (S: Structure.t, I: Interface.t, {prefix: string}) =
   let
      fun fixCons (Cons.T cs, Cons.T cs') =
	 Cons.T
	 (Vector.map
	  (cs', fn {name, scheme, ...} =>
	   let
	      val (con, uses) =
		 case Vector.peek (cs, fn {name = n, ...} =>
				   Ast.Con.equals (n, name)) of
		    NONE => (Con.bogus, Uses.new ())
		  | SOME {con, uses, ...} => (con, uses)
	   in
	      {con = con, name = name, scheme = scheme, uses = uses}
	   end))
      val (S', instantiate) = dummyStructure (I, {prefix = prefix})
      val _ = instantiate (S, fn (c, s) =>
			   TypeEnv.setOpaqueTyconExpansion
			   (c, fn ts => TypeStr.apply (s, ts)))
      val {destroy,
	   get = replacements: (Structure.t
				-> {formal: Structure.t,
				    new: Structure.t} list ref), ...} =
	 Property.destGet (Structure.plist,
			   Property.initFun (fn _ => ref []))
      fun loop (S, S'): Structure.t =
	 let
	    val rs = replacements S
	 in
	    case List.peek (!rs, fn {formal, ...} =>
			    Structure.eq (S', formal)) of
	       NONE =>
		  let
		     val Structure.T {strs, types, vals, ...} = S
		     val Structure.T {strs = strs',
				      types = types',
				      vals = vals', ...} = S'
		     val strs = Info.map2 (strs, strs', loop)
		     val types =
			Info.map2
			(types, types', fn (s, s') =>
			 let
			    datatype z = datatype TypeStr.node
			 in
			    case TypeStr.node s' of
			       Datatype {cons = cs', tycon} =>
				  (case TypeStr.node s of
				      Datatype {cons = cs, ...} =>
					 TypeStr.data
					 (tycon, TypeStr.kind s',
					  fixCons (cs, cs'))
				    | _ => s')
			     | Scheme _ => s'
			     | Tycon _ => s'
			 end)
		     val vals =
			Info.map2 (vals, vals', fn ((v, _), (_, s)) =>
				   (v, s))
		     val new =
			Structure.T {interface = Structure.interface S',
				     plist = PropertyList.new (),
				     strs = strs,
				     types = types,
				     vals = vals}
		     val _ = List.push (rs, {formal = S', new = new})
		  in
		     new
		  end
	     | SOME {new, ...} => new
	 end
      val S'' = loop (S, S')
      val _ = destroy ()
   in
      S''
   end

fun transparentCut (E: t, S: Structure.t, I: Interface.t, {isFunctor: bool},
		    region: Region.t): Structure.t * Decs.t =
   let
      val sign =
	 if isFunctor
	    then "argument signature"
	 else "signature"
      val preError =
	 Promise.lazy
	 (fn () =>
	  scope (E, fn () =>
		 (openStructure (E, S)
		  ; setTyconNames E)))
      val decs = ref []
      (* pre: arities are equal. *)
      fun equalSchemes (structScheme: Scheme.t,
			sigScheme: Scheme.t,
			name: string,
			thing: string,
			lay: unit -> Layout.t,
			r: Region.t): unit =
	 let
	    fun error (l1, l2) =
	       let
		  open Layout
	       in
		  Control.error
		  (r,
		   seq [str (concat [thing, " in structure disagrees with ",
				     sign])],
		   align [seq [str (concat [name, ": "]), lay ()],
			  seq [str "structure: ", l1],
			  seq [str "signature: ", l2]])
	       end
	    val (tyvars', ty') = Scheme.dest sigScheme
	    val tyvars =
	       Vector.tabulate
	       (Vector.length tyvars', fn _ =>
		Type.var (Tyvar.newNoname {equality = false}))
	 in
	    Type.unify
	    (Scheme.apply (structScheme, tyvars),
	     Scheme.apply (Scheme.make {canGeneralize = true,
					ty = ty',
					tyvars = tyvars'},
			   tyvars),
	     {error = error,
	      preError = preError})
	 end
      val equalSchemes =
	 Trace.trace
	 ("equalSchemes",
	  fn (s, s', _, _, _, _) => Layout.tuple [Scheme.layout s,
						  Scheme.layout s'],
	  Unit.layout)
	 equalSchemes
      fun layout (strids, x) =
	 layoutLong (List.fold (strids, [x], fn (s, ac) => Strid.layout s :: ac))
      fun checkCons (Cons.T v, Cons.T v',
		     strids: Strid.t list,
		     tycon: Ast.Tycon.t): unit =
	 let
	    fun lay (c: Ast.Con.t) = layout (strids, Ast.Con.layout c)
	    val extraStr =
	       Vector.keepAllMap
	       (v, fn {name = n, scheme = s, ...} =>
		case Vector.peek (v', fn {name = n', ...} =>
				  Ast.Con.equals (n, n')) of
		   NONE => SOME n
		 | SOME {scheme = s', ...} =>
		      let
			 val _ =
			    equalSchemes
			    (s, s', "constructor", "constructor type",
			     fn () => lay n, region)
		      in
			 NONE
		      end)
	    fun extras (v, name) =
	       if 0 = Vector.length v
		  then ()
	       else
		  let
		     open Layout
		  in
		     Control.error
		     (region,
		      seq [str "type ",
			   layout (strids, Ast.Tycon.layout tycon),
			   str (concat [" has constructors in ", name,
					" only: "]),
			   seq (List.separate (Vector.toListMap (v, lay),
					       str ", "))],
		      empty)
		  end
	    val _ = extras (extraStr, "structure")
	    val extraSig =
	       Vector.keepAllMap
	       (v', fn {name = n', ...} =>
		if Vector.exists (v, fn {name = n, ...} =>
				  Ast.Con.equals (n, n'))
		   then NONE
		else SOME n')
	    val _ = extras (extraSig, "signature")
	 in
	    ()
	 end
      fun handleType (structStr: TypeStr.t,
		      sigStr: Interface.TypeStr.t,
		      strids: Strid.t list,
		      name: Ast.Tycon.t): TypeStr.t =
	 let
	    val sigStr = Interface.TypeStr.toEnv sigStr
	    val structKind = TypeStr.kind structStr
	    val sigKind = TypeStr.kind sigStr
	    fun tyconScheme (c: Tycon.t): Scheme.t =
	       let
		  val tyvars =
		     case TypeStr.kind structStr of
			Kind.Arity n =>
			   Vector.tabulate
			   (n, fn _ =>
			    Tyvar.newNoname {equality = false})
		      | _ => Error.bug "Nary tycon"
	       in
		  Scheme.make
		  {canGeneralize = true,
		   ty = Type.con (c, Vector.map (tyvars, Type.var)),
		   tyvars = tyvars}
	       end
	    datatype z = datatype TypeStr.node
	    fun checkScheme (sigScheme: Scheme.t) =
	       let
		  val structScheme =
		     case TypeStr.node structStr of
			Datatype {tycon = c, ...} => tyconScheme c
		      | Scheme s => s
		      | Tycon c => tyconScheme c
		  val _ =
		     equalSchemes
		     (structScheme, sigScheme,
		      "type", "type definition",
		      fn () => layout (strids, Ast.Tycon.layout name), region)
	       in
		  sigStr
	       end
	 in
	    if not (Kind.equals (structKind, sigKind))
	       then
		  let
		     open Layout
		     val _ =
			Control.error
			(region,
			 seq [str "type ",
			      layout (strids, Ast.Tycon.layout name),
			      str " has arity ", Kind.layout structKind,
			      str " in structure but arity ",
			      Kind.layout sigKind, str " in ",
			      str sign],
			 empty)
		  in
		     sigStr
		  end
	    else
	       case TypeStr.node sigStr of
		  Datatype {cons = sigCons, ...} =>
		     (case TypeStr.node structStr of
			 Datatype {cons = structCons, ...} =>
			    (checkCons (structCons, sigCons, strids, name)
			     ; structStr)
		       | _ =>
			    let
			       open Layout
			       val _ = 
				  Control.error
				  (region,
				   seq [str "type ",
					layout (strids, Ast.Tycon.layout name),
					str " is a datatype in ", str sign,
					str " but not in structure"],
				   Layout.empty)
			    in
			       sigStr
			    end)
		| Scheme s => checkScheme s
		| Tycon c => checkScheme (tyconScheme c)
	 end
      fun map (structInfo: ('a, 'b) Info.t,
	       sigArray: ('a * 'c) array,
	       strids: Strid.t list,
	       nameSpace: string,
	       namesEqual: 'a * 'a -> bool,
	       layoutName: 'a -> Layout.t,
	       bogus: 'c -> 'd,
	       doit: 'a * 'b * 'c -> 'd): ('a, 'd) Info.t =
	 let
	    val Info.T structArray = structInfo
	    val n = Array.length structArray
	    val r = ref 0
	    val array =
	       Array.map
	       (sigArray, fn (name, c) =>
		let
		   fun find i =
		      if i = n
			 then
			    let
			       open Layout
			       val _ =
				  Control.error
				  (region,
				   seq [str (concat [nameSpace, " "]),
					layout (strids, layoutName name),
					str (concat
					     [" in ", sign,
					      " but not in structure: "])],
				   empty)
			    in
			       {domain = name,
				range = bogus c,
				uses = Uses.new ()}
			    end
		      else
			 let
			    val {domain, range, uses} =
			       Array.sub (structArray, i)
			 in
			    if namesEqual (domain, name)
			       then (r := i + 1
				     ; {domain = domain,
					range = doit (name, range, c),
					uses = uses})
			    else find (i + 1)
			 end
		in
		   find (!r)
		end)
	 in
	    Info.T array
	 end
      fun checkMatch (Structure.T {strs, types, ...}, I, strids): unit =
	 let
	    val {strs = strs', types = types', ...} = Interface.dest I
	    val _ =
	       map (strs, strs', strids,
		    "structure", Strid.equals, Strid.layout,
		    fn _ => (),
		    fn (name, S, I) => checkMatch (S, I, name :: strids))
	    val _ = 
	       map (types, types', strids,
		    "type", Ast.Tycon.equals, Ast.Tycon.layout,
		    fn _ => (),
		    fn (name, s, s') => (handleType (s, s', strids, name); ()))
	 in
	    ()
	 end
      val {destroy, get: Structure.t -> (Interface.t * Structure.t) list ref,
	   ...} =
	 Property.destGet (Structure.plist, Property.initFun (fn _ => ref []))
      fun cut (S, I, strids): Structure.t =
	 if Structure.hasInterface (S, I)
	    then (checkMatch (S, I, strids); S)
	 else
	    let
	       val seen = get S
	    in
	       case List.peek (!seen, fn (I', _) => Interface.equals (I, I')) of
		  NONE =>
		     let
			val S' = reallyCut (S, I, strids)
			val _ = List.push (seen, (I, S'))
		     in
			S'
		     end
		| SOME (_, S) => S
	    end
      and reallyCut (Structure.T {strs = structStrs,
				  types = structTypes,
				  vals = structVals, ...},
		     I, strids) =
	 let
	    val {strs = sigStrs, types = sigTypes, vals = sigVals} =
	       Interface.dest I
	    val strs =
	       map (structStrs, sigStrs, strids,
		    "structure", Strid.equals, Strid.layout,
		    fn I => #1 (dummyStructure (I, {prefix = ""})),
		    fn (name, S, I) => cut (S, I, name :: strids))
	    val types =
	       map (structTypes, sigTypes, strids,
		    "type", Ast.Tycon.equals, Ast.Tycon.layout,
		    Interface.TypeStr.toEnv,
		    fn (name, s, s') => handleType (s, s', strids, name))
	    val vals =
	       map
	       (structVals, sigVals, strids,
		"variable", Ast.Vid.equals, Ast.Vid.layout,
		fn (status, sigScheme) =>
		let
		   val vid =
		      case status of
			 Status.Con => Vid.Con (Con.newNoname ())
		       | Status.Exn => Vid.Exn (Con.newNoname ())
		       | Status.Var => Vid.Var (Var.newNoname ())
		in
		   (vid, Interface.Scheme.toEnv sigScheme)
		end,
		fn (name, (vid, strScheme), (status, sigScheme)) =>
		let
		   val sigScheme = Interface.Scheme.toEnv sigScheme
		   val (tyvars, sigType) = Scheme.dest sigScheme
		   val {args, instance = strType} = Scheme.instantiate strScheme
		   fun error (l, l') =
		      let
			 open Layout
		      in
			 Control.error
			 (region,
			  seq [str "variable type in structure disagrees with ",
			       str sign],
			  align [seq [str "variable: ",
				      Longvid.layout	
				      (Longvid.long (rev strids, name))],
				 seq [str "structure: ", l],
				 seq [str "signature: ", l']])
		      end
		   val _ = Type.unify (strType, sigType,
				       {error = error,
					preError = preError})
		   fun addDec (n: Exp.node): Vid.t =
		      let
			 val x = Var.newNoname ()
			 val e = Exp.make (n, strType)
			 val _ =
			    List.push
			    (decs,
			     Dec.Val {rvbs = Vector.new0 (),
				      tyvars = fn () => tyvars,
				      vbs = (Vector.new1
					     {exp = e,
					      lay = fn _ => Layout.empty,
					      pat = Pat.var (x, strType),
					      patRegion = region})})
		      in
			 Vid.Var x
		      end
		   fun con (c: Con.t): Vid.t =
		      addDec (Exp.Con (c, args ()))
		   val vid =
		      case (vid, status) of
			 (Vid.Con c, Status.Var) => con c
		       | (Vid.Exn c, Status.Var) => con c
		       | (Vid.Var x, Status.Var) =>
			    (if 0 < Vector.length tyvars
				orelse 0 < Vector.length (args ())
				then
				   addDec (Exp.Var (fn () => x, args))
			     else vid)
		       | (Vid.Con _, Status.Con) => vid
		       | (Vid.Exn _, Status.Exn) => vid
		       | _ =>
			    let
			       open Layout
			       val _ =
				  Control.error
				  (region,
				   seq [str (concat
					     [Vid.statusPretty vid,
					      " in structure but ",
					      Status.pretty status, " in ",
					      sign, ": "]),
					layout (strids, Ast.Vid.layout name)],
				   Layout.empty)
			    in
			       vid
			    end
		in
		   (vid, sigScheme)
		end)
	 in
	    Structure.T {interface = SOME I,
			 plist = PropertyList.new (),
			 strs = strs,
			 types = types,
			 vals = vals}
	 end
      fun typeStrIsConsistent (typeStr: TypeStr.t,
			       long,
			       a: AdmitsEquality.t,
			       k: Kind.t,
			       {hasCons: bool}): bool =
	 if not (AdmitsEquality.<= (a, TypeStr.admitsEquality typeStr))
	    then 
	       let
		  open Layout
		  val _ =
		     Control.error
		     (region,
		      seq [str "type ", Longtycon.layout (long ()),
			   str " admits equality in ", str sign,
			   str " but not in structure"],
		      empty)
	       in
		  false
	       end
	 else if hasCons andalso Option.isNone (TypeStr.toTyconOpt typeStr)
            then
	       let
		  open Layout
		  val _ =
		     Control.error
		     (region,
		      seq [str "type ", Longtycon.layout (long ()),
			   str " is a datatype in ", str sign,
			   str " but not in structure"],
		      empty)
	       in
		  false
	       end
	 else
	    let
	       val k' = TypeStr.kind typeStr
	    in
	       if not (Kind.equals (k, k'))
		  then
		     let
			open Layout
			val _ =
			   Control.error
			   (region,
			    seq [str "type ",
				 Longtycon.layout (long ()),
				 str " has arity ",
				 Kind.layout k',
				 str " in structure but arity ",
				 Kind.layout k,
				 str " in ", str sign],
			    empty)
		     in
			false
		     end
	       else true
	    end
      val I = Interface.copy I
      val () =
	 Structure.realize
	 (S, Interface.flexibleTycons I,
	  fn (name, flex, typeStr, {nest}) =>
	  let
	     val {admitsEquality = a, hasCons, kind = k, ...} =
		FlexibleTycon.dest flex
	     fun long () = Longtycon.long (rev nest, name)
	     fun bad () =
		TypeStr.tycon (newTycon (Longtycon.toString (long ()), k, a), k)
	     val typeStr =
		case typeStr of
		   NONE => bad ()
		 | SOME typeStr =>
		      if typeStrIsConsistent (typeStr, long, a, k,
					      {hasCons = hasCons})
			 then typeStr
		      else bad ()
	     val () = FlexibleTycon.realize (flex, typeStr)
	  in
	     ()
	  end)
      val S = cut (S, I, [])
      val () = destroy ()
   in
      (S, Decs.fromList (!decs))
   end

(* section 5.3, 5.5, 5.6 and rules 52, 53 *)
fun cut (E: t, S: Structure.t, I: Interface.t,
	 {isFunctor: bool, opaque: bool, prefix: string}, region)
   : Structure.t * Decs.t =
   let
      val (S, decs) = transparentCut (E, S, I, {isFunctor = isFunctor}, region)
      (* Aoid doing the opaque match if numErrors > 0 because it can lead
       * to internal errors that might be confusing to the user.
       *)
      val S = 
	 if opaque andalso 0 = !Control.numErrors
	    then makeOpaque (S, I, {prefix = prefix})
	 else S
   in
      (S, decs)
   end

val cut =
   Trace.trace ("cut",
		fn (_, S, I, _, _) =>
		Layout.tuple [Structure.layoutPretty S, Interface.layout I],
		Structure.layoutPretty o #1)
   cut

(* ------------------------------------------------- *)
(*                  functorClosure                   *)
(* ------------------------------------------------- *)

fun snapshot (E as T {currentScope, fcts, fixs, sigs, strs, types, vals, ...})
   : (unit -> 'a) -> 'a =
   let
      val add: (Scope.t -> unit) list ref = ref []
      (* Push onto add everything currently in scope. *)
      fun doit (NameSpace.T {current, ...}) (v as Values.T vs) =
	 case ! vs of
	    [] => ()
	  | {domain, range, uses, ...} :: _ =>
	       List.push
	       (add, fn s0 =>
		(List.push (vs, {domain = domain,
				 range = range,
				 scope = s0,
				 time = Time.next (),
				 uses = uses})
		 ; List.push (current, v)))
      val _ =
	 foreachTopLevelSymbol (E, {fcts = doit fcts,
				    fixs = doit fixs,
				    sigs = doit sigs,
				    strs = doit strs,
				    types = doit types,
				    vals = doit vals})
   in
      fn th =>
      let
	 val s0 = Scope.new {isTop = false}
	 val restore: (unit -> unit) list ref = ref []
	 fun doit (NameSpace.T {current, ...}) =
	    let
	       val current0 = !current
	       val _ = current := []
	    in
	       List.push (restore, fn () =>
			  (List.foreach (!current, fn v => (Values.pop v; ()))
			   ; current := current0))
	    end
	 val _ = (doit fcts; doit fixs; doit sigs
		  ; doit strs; doit types; doit vals)
	 val _ = List.foreach (!add, fn f => f s0)
	 (* Clear out any symbols that weren't available in the old scope. *)
	 fun doit (Values.T vs) =
	    let
	       val cur = !vs
	    in
	       case cur of
		  [] => ()
		| {scope, ...} :: _ =>
		     if Scope.equals (s0, scope)
			then ()
		     else (vs := []
			   ; List.push (restore, fn () => vs := cur))
	    end
	 val _ =
	    (* Can't use foreachToplevelSymbol here, because a constructor C may
	     * have been defined in a local scope but may not have been defined
	     * at the snapshot point.  This will make the identifier C, which
	     * originally would have elaborated as a variable instead elaborate
	     * as a constructor.
	     *)
	    foreachDefinedSymbol (E, {fcts = doit,
				      fixs = doit,
				      sigs = doit,
				      strs = doit,
				      types = doit,
				      vals = doit})
	 val s1 = !currentScope
	 val _ = currentScope := s0
	 val res = th ()
	 val _ = currentScope := s1
	 val _ = List.foreach (!restore, fn f => f ())
      in
	 res
      end
   end

fun functorClosure
   (E: t,
    prefix: string,
    argInt: Interface.t,
    makeBody: Structure.t * string list -> Decs.t * Structure.t option) =
   let
      (* Keep track of the first tycon currently at the front of allTycons.
       * Once we are done elaborating the body, we can remove all the dummy
       * tycons created while elaborating the body by removing everything from
       * allTycons up to firstTycon.
       *)
      val firstTycon =
	 case !allTycons of
	    [] => Error.bug "no front of allTycons"
	  | c :: _ => c
      (* Need to tick here so that any tycons created in the dummy structure
       * for the functor formal have a new time, and will therefore report an
       * error if they occur before the functor declaration.
       *)
      val _ = TypeEnv.tick {useBeforeDef = fn _ => Error.bug "functor tick"}
      val (formal, instantiate) = dummyStructure (argInt, {prefix = prefix})
      val _ = insideFunctor := true
      (* Keep track of all tycons created during the instantiation of the
       * functor.  These will later become the generative tycons that will need
       * to be recreated for each functor application.
       * This has two beneficial effects.
       * 1. It keeps allTycons smaller.
       * 2. It keeps the names of these tycons from being set by setTyconNames,
       *    which they always would be because they are now out of scope.
       *)
      val _ = newTycons := []
      val (_, result) = makeBody (formal, [])
      val _ = Option.app (result, Structure.forceUsed)
      val generative = !newTycons
      val _ = allTycons := let
			      fun loop cs =
				 case cs of
				    [] => Error.bug "allTycons missing front"
				  | c :: cs =>
				       if Tycon.equals (c, firstTycon)
					  then cs
				       else loop cs
			   in
			      loop (!allTycons)
			   end
      val _ = newTycons := []
      val _ = insideFunctor := false
      val restore =
	 if !Control.elaborateOnly
	    then fn f => f ()
	 else snapshot E
      fun apply (actual, nest) =
	 if not (!insideFunctor) andalso not (!Control.elaborateOnly)
	    then restore (fn () => makeBody (actual, nest))
	 else
	    let
	       val _ = Structure.forceUsed actual
	       val {destroy = destroy1,
		    get = tyconTypeStr: Tycon.t -> TypeStr.t option,
		    set = setTyconTypeStr, ...} =
		  Property.destGetSet (Tycon.plist, Property.initConst NONE)
	       (* Match the actual against the formal, to set the tycons.
		* Then duplicate the result, replacing tycons.  Want to generate
		* new tycons just like the functor body did.
		*)
	       val _ =
		  instantiate (actual, fn (c, s) => setTyconTypeStr (c, SOME s))
	       val _ =
		  List.foreach
		  (generative, fn (c, k) =>
		   setTyconTypeStr
		   (c, SOME (TypeStr.tycon
			     (newTycon (Tycon.originalName c, k,
					! (TypeEnv.tyconAdmitsEquality c)),
			      k))))
	       fun replaceType (t: Type.t): Type.t =
		  let
		     fun con (c, ts) =
			case tyconTypeStr c of
			   NONE => Type.con (c, ts)
			 | SOME s => TypeStr.apply (s, ts)
		  in
		     Type.hom (t, {con = con,
				   expandOpaque = false,
				   record = Type.record,
				   replaceCharWithWord8 = false,
				   var = Type.var})
		  end
	       fun replaceScheme (s: Scheme.t): Scheme.t =
		  let
		     val (tyvars, ty) = Scheme.dest s
		  in
		     Scheme.make {canGeneralize = true,
				  ty = replaceType ty,
				  tyvars = tyvars}
		  end
	       fun replaceCons (Cons.T v): Cons.t =
		  Cons.T
		  (Vector.map
		   (v, fn {con, name, scheme, uses} =>
		    {con = con,
		     name = name,
		     scheme = replaceScheme scheme,
		     uses = uses}))
	       fun replaceTypeStr (s: TypeStr.t): TypeStr.t =
		  let
		     val k = TypeStr.kind s
		     datatype z = datatype TypeStr.node
		  in
		     case TypeStr.node s of
			Datatype {cons, tycon} =>
			   let
			      val tycon =
				 case tyconTypeStr tycon of
				    NONE => tycon
				  | SOME s =>
				       (case TypeStr.node s of
					   Datatype {tycon, ...} => tycon
					 | Scheme _ =>
					      Error.bug "bad datatype"
					 | Tycon c => c)
			   in
			      TypeStr.data (tycon, k, replaceCons cons)
			   end
		      | Scheme s => TypeStr.def (replaceScheme s, k)
		      | Tycon c =>
			   (case tyconTypeStr c of
			       NONE => s
			     | SOME s' => s')
		  end
	       val {destroy = destroy2,
		    get = replacement: Structure.t -> Structure.t, ...} =
		  Property.destGet
		  (Structure.plist,
		   Property.initRec
		   (fn (Structure.T {interface, strs, types, vals, ... },
			replacement) =>
		    Structure.T
		    {interface = interface,
		     plist = PropertyList.new (),
		     strs = Info.map (strs, replacement),
		     types = Info.map (types, replaceTypeStr),
		     vals = Info.map (vals, fn (v, s) =>
				      (v, replaceScheme s))}))
	       val result = Option.map (result, replacement)
	       val _ = destroy1 ()
	       val _ = destroy2 ()
	    in
	       (Decs.empty, result)
	    end
   in
      FunctorClosure.T {apply = apply,
			argInt = argInt,
			formal = formal,
			result = result}
   end

structure Env =
   struct
      type t = t

      val lookupLongtycon = lookupLongtycon
   end

structure InterfaceEnv =
   struct
      local
	 open Interface
      in
	 structure Scheme = Scheme
	 structure Status = Status
	 structure TypeStr = TypeStr
      end

      type env = Env.t
	 
      datatype t = T of {currentScope: Scope.t ref,
			 env: Env.t,
			 strs: (Strid.t, Interface.t) NameSpace.t,
			 types: (Ast.Tycon.t, TypeStr.t) NameSpace.t,
			 vals: (Ast.Vid.t, Status.t * Scheme.t) NameSpace.t}

      val allowDuplicates = ref false

      fun extend (T (fields as {currentScope, ...}),
		  domain, range, kind: string, ns, region): unit =
	 let
	    val scope = !currentScope
	    val NameSpace.T {current, lookup, toSymbol, ...} = ns fields
	    fun value () = {domain = domain,
			    range = range,
			    scope = scope,
			    time = Time.next (),
			    uses = Uses.new ()}
	    val values as Values.T r = lookup domain
	    fun new () = (List.push (current, values)
			  ; List.push (r, value ()))
	 in
	    case !r of
	       [] => new ()
	     | {scope = scope', ...} :: l =>
		  if Scope.equals (scope, scope')
		     then if !allowDuplicates
			     then r := value () :: l
			  else
			     Control.error
			     (region,
			      Layout.str
			      (concat ["duplicate ",
				       kind,
				       " specification: ",
				       Symbol.toString (toSymbol domain)]),
			      Layout.empty)
		  else new ()
	 end

      fun extendStrid (E, s, I, r) = extend (E, s, I, "structure", #strs, r)

      fun extendTycon (E, c, s, r) = extend (E, c, s, "type", #types, r)

      fun extendVid (E, v, st, s, r) = extend (E, v, (st, s), "value", #vals, r)

      val lookupSigid = fn (T {env, ...}, x) => lookupSigid (env, x)

      local
	 fun make sel (T r, a) =
	    NameSpace.peek (sel r, a, {markUse = fn _ => true})
      in
	 val peekStrid = make #strs
	 val peekTycon = make #types
      end

      fun lookupLongstrid (E: t, s: Longstrid.t): Interface.t option =
	 let
	    fun error l =
	       (unbound (Longstrid.region s, "structure", l)
		; NONE)
	    val (strids, strid) = Longstrid.split s
	 in
	    case strids of
	       [] =>
		  (case peekStrid (E, strid) of
		      NONE => error (Strid.layout strid)
		    | SOME I => SOME I)
	     | s :: ss =>
		  case peekStrid (E, s) of
		     NONE => error (Strid.layout s)
		   | SOME I =>
			let
			   datatype z = datatype Interface.peekResult
			in
			   case Interface.peekStrids (I, ss @ [strid]) of
			      Found I => SOME I
			    | UndefinedStructure ss =>
				 error (layoutStrids (s :: ss))
			end
	 end
      
      fun lookupLongtycon (E as T {env, ...},
			   long: Longtycon.t): TypeStr.t option =
	 let
	    fun doit () =
	       SOME (TypeStr.fromEnv (Env.lookupLongtycon (env, long)))
	    val (strids, c) = Longtycon.split long
	 in
	    case strids of
	       [] =>
		  (case peekTycon (E, c) of
		      NONE => doit ()
		    | SOME s => SOME s)
	     | s :: ss =>
		  case peekStrid (E, s) of
		     NONE => doit ()
		   | SOME I =>
			Interface.lookupLongtycon
			(I, Longtycon.long (ss, c), Longtycon.region long,
			 {prefix = [s]})
	 end

      fun makeInterface (T {currentScope, strs, types, vals, ...}, make) =
	 let
	    val s = NameSpace.collect strs
	    val t = NameSpace.collect types
	    val v = NameSpace.collect vals
	    val s0 = !currentScope
	    val _ = currentScope := Scope.new {isTop = false}
	    val res = make ()
	    val Info.T s = s ()
	    val s = Array.map (s, fn {domain, range, ...} => (domain, range))
	    val Info.T t = t ()
	    val t = Array.map (t, fn {domain, range, ...} => (domain, range))
	    val Info.T v = v ()
	    val v = Array.map (v, fn {domain, range = (status, scheme), ...} =>
			       (domain, (status, scheme)))
	    val I = Interface.new {strs = s, types = t, vals = v}
	    val _ = currentScope := s0
	 in
	    (I, res)
	 end
      
      fun openInterface (E, I, r: Region.t) =
	 let
	    val {strs, vals, types} = Interface.dest I
	    val _ = Array.foreach (strs, fn (s, I) => extendStrid (E, s, I, r))
	    val _ = Array.foreach (types, fn (c, s) => extendTycon (E, c, s, r))
	    val _ = Array.foreach (vals, fn (x, (s, sc)) =>
				   extendVid (E, x, s, sc, r))
	 in
	    ()
	 end

      val extendStrid = fn (E, s, I) => extendStrid (E, s, I, Strid.region s)

      val extendTycon = fn (E, c, s) => extendTycon (E, c, s, Ast.Tycon.region c)

      val extendVid =
	 fn (E, v, st, s) => extendVid (E, v, st, s, Ast.Vid.region v)

      fun extendCon (E, c, s) =
	 extendVid (E, Ast.Vid.fromCon c, Status.Con, s)

      fun extendExn (E, c, s) =
	 extendVid (E, Ast.Vid.fromCon c, Status.Exn, s)
end

fun makeInterfaceEnv (env as T {currentScope, ...}): InterfaceEnv.t =
   let
      val {get =
	   lookupAll: (Symbol.t
		       -> {strs: (Strid.t, Interface.t) Values.t,
			   types: (Ast.Tycon.t, Interface.TypeStr.t) Values.t,
			   vals: (Ast.Vid.t, Status.t * Interface.Scheme.t) Values.t}),
	   ...} =
	 Property.get (Symbol.plist,
		       Property.initFun
		       (fn _ => {strs = Values.new (),
				 types = Values.new (),
				 vals = Values.new ()}))
      fun make (sel, class, region, toSymbol: 'a -> Symbol.t)
	 : ('a, 'b) NameSpace.t =
	 NameSpace.new {class = fn _ => class,
			lookup = sel o lookupAll o toSymbol,
			region = region,
			toSymbol = toSymbol}
   in
      InterfaceEnv.T
      {currentScope = currentScope,
       env = env,
       strs = make (#strs, Class.Str, Strid.region, Strid.toSymbol),
       types = make (#types, Class.Typ, Ast.Tycon.region, Ast.Tycon.toSymbol),
       vals = make (#vals, Class.Var, Ast.Vid.region, Ast.Vid.toSymbol)}
   end
   
end
