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
   structure Fixity = Fixity
   structure Strid = Strid
   structure Longcon = Longcon
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
   structure Prim = Prim
   structure Record = Record
   structure SortedRecord = SortedRecord
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

structure TypeScheme = Scheme

structure Scope = UniqueId ()

structure Vid =
   struct
      datatype t =
	 Con of Con.t
       | Exn of Con.t
       | Overload of Priority.t * (Var.t * Type.t) vector
       | Var of Var.t

      val statusPretty =
	 fn Con _ => "a constructor"
	  | Exn _ => "an exception"
	  | Overload _ => "an overload"
	  | Var _ => "a variable"

      val statusString =
	 fn Con _ => "con"
	  | Exn _ => "exn"
	  | Overload _ => "var"
	  | Var _ => "var"

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
	  
      fun output (r, out) = Layout.output (layout r, out)
   end

fun layoutSize z = Int.layout (MLton.size z)

structure TypeStr = TypeStr (structure AdmitsEquality = AdmitsEquality
			     structure Con = Con
			     structure Kind = Tycon.Kind
			     structure Name = Ast.Con
			     structure Record = SortedRecord
			     structure Scheme =
				struct
				   open Scheme

				   val make =
				      fn (tyvars, ty) =>
				      make {canGeneralize = true,
					    ty = ty,
					    tyvars = tyvars}
				end
			     structure Tycon =
				struct
				   open Tycon

				   val admitsEquality =
				      TypeEnv.tyconAdmitsEquality

				   val make = newNoname
				end
			     structure Type =
				struct
				   open Type

				   val bogus = new ()

				   fun hom (t, {con, record, var}) =
				      Type.hom (t, {con = con,
						    expandOpaque = false,
						    record = record,
						    replaceCharWithWord8 = false,
						    var = var})
				end
			     structure Tyvar = Tyvar)

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
   structure Status = Status
end

structure Status =
   struct
      open Status

      val pretty: t -> string =
	 fn Con => "a constructor"
	  | Exn => "an exception"
	  | Var => "a variable"
   end

structure Info =
   struct
      (* The array is sorted by domain element. *)
      datatype ('a, 'b) t = T of {domain: 'a,
				  isUsed: bool ref,
				  range: 'b} array
	 
      fun bogus () = T (Array.tabulate (0, fn _ => Error.bug "impossible"))

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
	     val v as {isUsed, ...} =  Array.sub (a, i)
	     val _ = isUsed := !Control.showBasisUsed
	  in
	     v
	  end)

      val map: ('a, 'b) t * ('b -> 'b) -> ('a, 'b) t =
	 fn (T a, f) =>
	 T (Array.map (a, fn {domain, range, ...} =>
		       {domain = domain,
			isUsed = ref false,
			range = f range}))

      val map2: ('a, 'b) t * ('a, 'b) t * ('b * 'b -> 'b) -> ('a, 'b) t =
	 fn (T a, T a', f) =>
	 T (Array.map2
	    (a, a', fn ({domain, range = r, ...}, {range = r', ...}) =>
	     {domain = domain,
	      isUsed = ref false,
	      range = f (r, r')}))
   end

val allTycons: Tycon.t list ref = ref []
val newTycons: (Tycon.t * Kind.t) list ref = ref []

val newTycon: string * Kind.t * AdmitsEquality.t -> Tycon.t =
   fn (s, k, a) =>
   let
      val c = Tycon.fromString s
      val _ = TypeEnv.initAdmitsEquality (c, a)
      (* The tick is after initAdmitsEquality so that any unknowns subsequently
       * created will have a later time than the tycon, and hence can be
       * unified with it.
       *)
      val _ = TypeEnv.Time.tick ()
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
      datatype t = T of {plist: PropertyList.t,
			 strs: (Ast.Strid.t, t) Info.t,
			 types: (Ast.Tycon.t, TypeStr.t) Info.t,
			 vals: (Ast.Vid.t, Vid.t * Scheme.t) Info.t}

      local
	 fun make f (T r) = f r
      in
	 val plist = make #plist
      end

      fun eq (s: t, s': t): bool = PropertyList.equals (plist s, plist s')

      fun layoutUsed (T {strs, types, vals, ...}) =
	 let
	    open Layout
	    fun doit (Info.T a, lay): Layout.t =
	       align
	       (Array.foldr (a, [], fn ({domain, isUsed, range}, ac) =>
			     if not (!isUsed)
				then ac
			     else lay (domain, range) :: ac))
	    fun doitn (i, name, lay) =
	       doit (i, fn (d, _) => seq [str name, lay d])
	 in
	    align [doitn (types, "type ", Ast.Tycon.layout),
		   doitn (vals, "val ", Ast.Vid.layout),
		   doit (strs, fn (d, r) =>
			 align [seq [str "structure ", Strid.layout d],
				indent (layoutUsed r, 3)])]
	 end

      fun layout (T {strs, vals, types, ...}) =
	 Layout.record
	 [("types", Info.layout (Ast.Tycon.layout, TypeStr.layout) types),
	  ("vals", (Info.layout (Ast.Vid.layout,
				 Layout.tuple2 (Vid.layout, Scheme.layout))
		    vals)),
	  ("strs", Info.layout (Strid.layout, layout) strs)]

      local
	 open Layout
      in
	 fun layoutTypeSpec (name: Ast.Tycon.t, s) =
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
	       val def = seq [str "type ", args,
			      Ast.Tycon.layout name, str " = "]
	       val res = 
		  case TypeStr.node s of
		     TypeStr.Datatype {cons = Cons.T cs, ...} =>
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
			   seq [str "data", def, alignPrefix (cs, "| ")]
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
		       str ": ", Scheme.layoutPretty scheme]
	       datatype z = datatype Vid.t
	    in
	       case vid of
		  Con _ => simple "con"
		| Exn c =>
		     seq [str "exception ", Con.layout c, 
			  case Type.deArrowOpt (Scheme.ty scheme) of
			     NONE => empty
			   | SOME (t, _) =>
				seq [str " of ", Type.layoutPretty t]]
		| Overload  _ => simple "val"
		| Var _ => simple "val"
	    end
	 fun layoutStrSpec (d: Strid.t, r) =
	    align [seq [str "structure ", Strid.layout d, str ":"],
		   indent (layoutPretty r, 3)]
	 and layoutPretty (T {strs, vals, types, ...}) =
	    let
	       fun doit (Info.T a, layout) =
		  align (Array.foldr (a, [], fn ({domain, range, ...}, ac) =>
				      layout (domain, range) :: ac))
	    in
	       align
	       [str "sig",
		indent (align [doit (types, layoutTypeSpec),
			       doit (vals, layoutValSpec),
			       doit (strs, layoutStrSpec)],
			3),
		str "end"]
	    end
      end

      val bogus = T {plist = PropertyList.new (),
		     strs = Info.bogus (),
		     vals = Info.bogus (),
		     types = Info.bogus ()}

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

      datatype peekResult =
	 Found of t
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
	    val (strids, t) = Ast.Longtycon.split t
	 in
	    case peekStrids (S, strids) of
	       Found S => peekTycon (S, t)
	     | UndefinedStructure _ => NONE
	 end

      fun maker () =
	 let
	    fun make toSymbol =
	       let
		  val r = ref []
		  fun add {domain, range} =
		     List.push (r, {domain = domain,
				    isUsed = ref false,
				    range = range})
		  fun done () = 
		     Info.T
		     (QuickSort.sortArray
		      (Array.fromList (!r),
		       fn ({domain = d, ...}, {domain = d', ...}) =>
		       Symbol.<= (toSymbol d, toSymbol d')))
	       in
		  (add, done)
	       end
	    val (addStr, strs) = make Strid.toSymbol
	    val (addType, types) = make Ast.Tycon.toSymbol
	    val (addVal, vals) = make Ast.Vid.toSymbol
	    fun finish () =
	       T {plist = PropertyList.new (),
		  strs = strs (), 
		  types = types (),
		  vals = vals ()}
	 in
	    {addStr = addStr,
	     addType = addType,
	     addVal = addVal,
	     finish = finish}
	 end

      val ffi: t option ref = ref NONE
   end

structure FunctorClosure =
   struct
      datatype t =
	 T of {apply: (Structure.t * string list * Region.t
		       -> Decs.t * Structure.t),
	       sizeMessage: unit -> Layout.t}

      val bogus = T {apply = fn _ => (Decs.empty, Structure.bogus),
 		     sizeMessage = fn _ => Layout.str "<bogus>"}

      fun apply (T {apply, ...}, s, nest, r) = apply (s, nest, r)

      fun sizeMessage (T {sizeMessage, ...}) = sizeMessage ()
	 
      fun layout _ = Layout.str "<functor closure>"
   end

(* ------------------------------------------------- *)
(*                     NameSpace                     *)
(* ------------------------------------------------- *)

structure Values =
   struct
      (* The domains of all elements in a values list have the same symbol. *)
      datatype ('a, 'b) t = T of {domain: 'a,
				  isUsed: bool ref,
				  scope: Scope.t,
				  range: 'b} list ref

      fun new (): ('a, 'b) t = T (ref [])

      fun pop (T r) = List.pop r

      fun ! (T r) = Ref.! r
   end

structure NameSpace =
   struct
      datatype ('a, 'b) t = T of {current: ('a, 'b) Values.t list ref,
				  lookup: Symbol.t -> ('a, 'b) Values.t,
				  toSymbol: 'a -> Symbol.t}

      fun values (T {lookup, toSymbol, ...}, a) = lookup (toSymbol a)

      fun new {lookup, toSymbol} =
	 T {current = ref [],
	    lookup = lookup,
	    toSymbol = toSymbol}

      fun peek (ns, a) =
	 case Values.! (values (ns, a)) of
	    [] => NONE
	  | {isUsed, range, ...} :: _ => 
	       (isUsed := !Control.showBasisUsed
		; SOME range)

      val update: ('a, 'b) t * Scope.t * {domain: 'a,
					  isUsed: bool ref,
					  range: 'b} -> unit =
	 fn (T {current, lookup, toSymbol, ...},
	     scope,
	     {domain, isUsed, range}) =>
	 let
	    val value = {domain = domain,
			 isUsed = isUsed,
			 range = range,
			 scope = scope}
	    val values as Values.T r = lookup (toSymbol domain)
	    fun new () = (List.push (current, values)
			  ; List.push (r, value))
	 in
	    case !r of
	       [] => new ()
	     | {scope = scope', ...} :: l =>
		  if Scope.equals (scope, scope')
		     then r := value :: l
		  else new ()
	 end

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
				  val {domain, isUsed, range, ...} =
				     Values.pop values
			       in
				  {domain = domain,
				   isUsed = isUsed,
				   range = range}
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
		   sigs: (Sigid.t, Interface.t) NameSpace.t,
		   strs: (Strid.t, Structure.t) NameSpace.t,
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
      fun ('a, 'b) make (toSymbol,
			 extract: All.t -> ('a, 'b) Values.t option,
			 make: ('a, 'b) Values.t -> All.t)
	 : ('a, 'b) NameSpace.t  =
	 let
	    fun lookup (s: Symbol.t): ('a, 'b) Values.t =
	       let
		  val r = lookupAll s
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
	    NameSpace.new {lookup = lookup,
			   toSymbol = toSymbol}
	 end
   in
      T {currentScope = ref (Scope.new ()),
	 fcts = make (Fctid.toSymbol, All.fctOpt, All.Fct),
	 fixs = make (Ast.Vid.toSymbol, All.fixOpt, All.Fix),
	 lookup = lookupAll,
	 sigs = make (Sigid.toSymbol, All.sigOpt, All.Sig),
	 strs = make (Strid.toSymbol, All.strOpt, All.Str),
	 types = make (Ast.Tycon.toSymbol, All.tycOpt, All.Tyc),
	 vals = make (Ast.Vid.toSymbol, All.valOpt, All.Val)}
   end

fun foreach (T {lookup, ...}, {fcts, fixs, sigs, strs, types, vals}) =
   Symbol.foreach
   (fn s =>
    List.foreach (! (lookup s),
		  let
		     datatype z = datatype All.t
		  in
		     fn Fct vs => fcts vs
		      | Fix vs => fixs vs
		      | Sig vs => sigs vs
		      | Str vs => strs vs
		      | Tyc vs => types vs
		      | Val vs => vals vs
		  end))

fun collect (E as T r, f: {isUsed: bool} -> bool) =
   let
      val fcts = ref []
      val sigs = ref []
      val strs = ref []
      val types = ref []
      val vals = ref []
      fun doit ac vs =
	 case Values.! vs of
	    [] => ()
	  | {domain, isUsed, range, ...} :: _ =>
	       if f {isUsed = !isUsed}
			    then List.push (ac, (domain, range))
	       else ()
      val _ =
	 foreach (E, {fcts = doit fcts,
		      fixs = fn _ => (),
		      sigs = doit sigs,
		      strs = doit strs,
		      types = doit types,
		      vals = doit vals})
      fun finish (r, toSymbol) =
	 QuickSort.sortArray
	 (Array.fromList (!r), fn ((d, _), (d', _)) =>
	  Symbol.<= (toSymbol d, toSymbol d'))
   in
      {fcts = finish (fcts, Fctid.toSymbol),
       sigs = finish (sigs, Sigid.toSymbol),
       strs = finish (strs, Strid.toSymbol),
       types = finish (types, Ast.Tycon.toSymbol),
       vals = finish (vals, Ast.Vid.toSymbol)}
   end
   
fun layout (E: t): Layout.t =
   let
      val {fcts, sigs, strs, types, vals} = collect (E, fn _ => true)
      open Layout
      fun doit (a, layout) = align (Array.toListMap (a, layout))
   in
      align [doit (types, Structure.layoutTypeSpec),
	     doit (vals, Structure.layoutValSpec),
	     doit (sigs, fn (s, _) => seq [str "signature ", Sigid.layout s]),
	     doit (fcts, fn (s, _) => seq [str "functor ", Fctid.layout s]),
	     doit (strs, Structure.layoutStrSpec)]
   end

fun layoutUsed (E: t): Layout.t =
   let
      val {fcts, sigs, strs, types, vals} = collect (E, #isUsed)
      open Layout
      fun doit (a, layout) = align (Array.toListMap (a, layout))
      fun doitn (ns, name, lay) =
	 doit (ns, fn (s, _) => seq [str name, str " ", lay s])
   in
      align [doitn (types, "type", Ast.Tycon.layout),
	     doitn (vals, "val", Ast.Vid.layout),
	     doitn (sigs, "signature", Sigid.layout),
	     doitn (fcts, "functor", Fctid.layout),
	     doit (strs, fn (s, r) =>
		   align [seq [str "structure ", Strid.layout s],
			  indent (Structure.layoutUsed r, 3)])]
   end

(* ------------------------------------------------- *)
(*                       peek                        *)
(* ------------------------------------------------- *)

local
   fun make sel (T r, a) = NameSpace.peek (sel r, a)
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

fun peekCon (E: t, c: Ast.Con.t): (Con.t * Scheme.t) option =
   case peekVid (E, Ast.Vid.fromCon c) of
      NONE => NONE
    | SOME (vid, s) => Option.map (Vid.deCon vid, fn c => (c, s))

fun layoutStrids (ss: Strid.t list): Layout.t =
   Layout.str (concat (List.separate (List.map (ss, Strid.toString), ".")))
   
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
      make (Ast.Longtycon.split, peekTycon, Structure.peekTycon)
   val peekLongvar = make (Ast.Longvar.split, peekVar, Structure.peekVar)
   val peekLongvid = make (Ast.Longvid.split, peekVid, Structure.peekVid)
   val peekLongcon = make (Ast.Longcon.split, peekCon, Structure.peekCon)
end

val peekLongcon =
   Trace.trace2 ("peekLongcon", Layout.ignore, Ast.Longcon.layout,
		 PeekResult.layout (Layout.tuple2
				    (CoreML.Con.layout, TypeScheme.layout)))
   peekLongcon
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

local
   fun make (peek: t * 'a -> 'b option,
	     bogus: unit -> 'b,
	     className: string,
	     region: 'a -> Region.t,
	     layout: 'a -> Layout.t)
      (E: t, x: 'a): 'b =
      case peek (E, x) of
	 SOME y => y
       | NONE => (unbound (region x, className, layout x); bogus ())
in
   val lookupFctid =
      make (peekFctid, fn () => FunctorClosure.bogus,
	    "functor", Ast.Fctid.region, Ast.Fctid.layout)
   val lookupSigid =
      make (peekSigid, fn () => Interface.bogus,
	    "signature", Ast.Sigid.region, Ast.Sigid.layout)
end

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
      make (peekLongstrid,
	    fn () => Structure.bogus,
	    "structure",
	    Ast.Longstrid.region,
	    Ast.Longstrid.layout)
   val lookupLongtycon =
      make (peekLongtycon,
	    fn () => TypeStr.bogus Kind.Nary,
	    "type",
	    Ast.Longtycon.region,
	    Ast.Longtycon.layout)
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

local
   fun make get (T (fields as {currentScope, ...}), domain, range) =
      let
	 val ns = get fields
      in
	 NameSpace.update (ns, !currentScope,
			   {domain = domain,
			    isUsed = ref false,
			    range = range})
      end
in
   val extendFctid = make #fcts
   val extendFix = make #fixs
   val extendSigid = make #sigs
   val extendStrid = make #strs
   val extendTycon = make #types
   val extendVals = make #vals
end

fun extendCon (E, c, c', s) =
   extendVals (E, Ast.Vid.fromCon c, (Vid.Con c', s))
	       
fun extendExn (E, c, c', s) =
   extendVals (E, Ast.Vid.fromCon c, (Vid.Exn c', s))
	       
fun extendVar (E, x, x', s) =
   extendVals (E, Ast.Vid.fromVar x, (Vid.Var x', s))

fun extendOverload (E, p, x, yts, s) =
   extendVals (E, Ast.Vid.fromVar x, (Vid.Overload (p, yts), s))

val extendVar =
   Trace.trace4
   ("extendVar", Layout.ignore, Ast.Var.layout, Var.layout, Scheme.layoutPretty,
    Unit.layout)
   extendVar

(* ------------------------------------------------- *)   
(*                       local                       *)
(* ------------------------------------------------- *)

local
   fun doit (info as NameSpace.T {current, ...}, s0) =
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
	       val lift = List.map (c2, Values.pop)
	       val _ = List.foreach (c1, fn v => (Values.pop v; ()))
	       val _ = current := old
	       val _ =
		  List.foreach (lift, fn {domain, isUsed, range, ...} =>
				NameSpace.update
				(info, s0, {domain = domain,
					    isUsed = isUsed,
					    range = range}))
	    in
	       ()
	    end
	 end
      end
in
   fun localTop (T {currentScope, fcts, fixs, sigs, strs, types, vals, ...}, f) =
      let
	 val s0 = !currentScope
	 val fcts = doit (fcts, s0)
	 val fixs = doit (fixs, s0)
	 val sigs = doit (sigs, s0)
	 val strs = doit (strs, s0)
	 val types = doit (types, s0)
	 val vals = doit (vals, s0)
	 val _ = currentScope := Scope.new ()
	 val a = f ()
	 val fcts = fcts ()
	 val fixs = fixs ()
	 val sigs = sigs ()
	 val strs = strs ()
	 val types = types ()
	 val vals = vals ()
	 fun finish g =
	    let
	       val _ = currentScope := Scope.new ()
	       val b = g ()
	       val _ = (fcts (); fixs (); sigs (); strs (); types (); vals ())
	       val _ = currentScope := s0
	    in
	       b
	    end
      in (a, finish)
      end

   fun localModule (T {currentScope, fixs, strs, types, vals, ...},
		    f1, f2) =
      let
	 val s0 = !currentScope
	 val fixs = doit (fixs, s0)
	 val strs = doit (strs, s0)
	 val types = doit (types, s0)
	 val vals = doit (vals, s0)
	 val _ = currentScope := Scope.new ()
	 val a1 = f1 ()
	 val fixs = fixs ()
	 val strs = strs ()
	 val types = types ()
	 val vals = vals ()
	 val _ = currentScope := Scope.new ()
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
      val _ = currentScope := Scope.new ()
      val res = make ()
      val _ = f ()
      val S = Structure.T {plist = PropertyList.new (),
			   strs = s (),
			   types = t (),
			   vals = v ()}
      val _ = currentScope := s0
   in (res, S)
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
      val _ = currentScope := Scope.new ()
      val f = doit fixs 
      val s = doit strs
      val t = doit types
      val v = doit vals
      val res = th ()
      val _ = (f (); s (); t (); v ())
      val _ = currentScope := s0
   in res
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
      val _ = currentScope := Scope.new ()
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

fun openStructure (T {currentScope, strs, vals, types, ...},
		   Structure.T {strs = strs',
				vals = vals',
				types = types', ...}): unit =
   let
      val scope = !currentScope
      fun doit (info, Info.T a) =
	 Array.foreach (a, fn z => NameSpace.update (info, scope, z))
   in doit (strs, strs')
      ; doit (vals, vals')
      ; doit (types, types')
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
      val {strs, types, ...} = collect (E, fn _ => true)
      val _ = Array.foreach (types, fn (name, typeStr) =>
			     doType (typeStr, name, 0, []))
      val _ = Array.foreach (strs, fn (strid, str) => loopStr (str, 1, [strid]))
      val _ =
	 List.foreach
	 (!allTycons, fn c =>
	  if ! (shortest c) < Int.maxInt
	     then ()
	  else Tycon.setPrintName (c, concat ["?.", Tycon.originalName c]))
   in
      ()
   end

fun dummyStructure (T {strs, types, vals, ...}, prefix: string, I: Interface.t)
   : Structure.t * (Structure.t * (Tycon.t * TypeStr.t -> unit) -> unit) =
   let
      val tycons: (Longtycon.t * Tycon.t) list ref = ref []
      val I =
	 Interface.realize
	 (I, fn (c, a, k) =>
	  let
	     val c' = newTycon (concat [prefix, Longtycon.toString c], k, a)
	     val _ = List.push (tycons, (c, c'))
	  in
	     TypeStr.tycon (c', k)
	  end)
      val tycons = !tycons
      val {get, ...} =
	 Property.get
	 (Interface.plist,
	  Property.initRec
	  (fn (I, get) =>
	   let
	      val {addStr, addType, addVal, finish} = Structure.maker ()
	      fun handleStr {name, interface = I} =
		 addStr {domain = name,
			 range = get I}
	      fun handleType {name, typeStr} =
		 addType {domain = name,
			  range = typeStr}
	      fun handleVal {name, scheme, status} =
		 let
		    val con = CoreML.Con.fromString o Ast.Vid.toString
		    val var = CoreML.Var.fromString o Ast.Vid.toString
		    val vid =
		       case status of
			  Status.Con => Vid.Con (con name)
			| Status.Exn => Vid.Exn (con name)
			| Status.Var => Vid.Var (var name)
		 in
		    addVal {domain = name,
			    range = (vid, scheme)}
		 end
	      val _ =
		 Interface.foreach
		 (I, {handleStr = handleStr,
		      handleType = handleType,
		      handleVal = handleVal})
	   in
	      finish ()
	   end))
      val S = get I
      fun instantiate (S', f) =
	 List.foreach (tycons, fn (long, c) =>
		       case Structure.peekLongtycon (S', long) of
			  NONE => Error.bug "structure missing longtycon"
			| SOME s=> f (c, s))
   in
      (S, instantiate)
   end

val dummyStructure =
   Trace.trace ("dummyStructure",
		Interface.layout o #3,
		Structure.layoutPretty o #1)
   dummyStructure
	 
(* section 5.3, 5.5, 5.6 and rules 52, 53 *)
fun cut (E: t, S: Structure.t, I: Interface.t,
	 {isFunctor: bool, opaque: bool, prefix: string}, region)
   : Structure.t * Decs.t =
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
      fun error (name, l) =
	 let
	    open Layout
	 in
	    Control.error
	    (region,
	     seq [str (concat [name, " "]), l,
		  str " in ", str sign, str " but not in structure"],
	     empty)
	 end
      (* pre: arities are equal. *)
      fun equalSchemes (s: Scheme.t, s': Scheme.t,
			name: string,
			thing: string,
			lay: unit -> Layout.t,
			r: Region.t): unit =
	 let
	    val (tyvars', ty') = Scheme.dest s'
	    val tyvars =
	       Vector.tabulate
	       (Vector.length tyvars', fn _ =>
		Type.var (Tyvar.newNoname {equality = false}))
	 in
	    Type.unify
	    (Scheme.apply (s, tyvars),
	     Scheme.apply (Scheme.make {canGeneralize = true,
					ty = ty',
					tyvars = tyvars'},
			   tyvars),
	     preError,
	     fn (l1, l2) =>
	     let
		open Layout
	     in
		(r,
		 seq [str (concat [thing, " in structure disagrees with ",
				   sign])],
		 align [seq [str (concat [name, ": "]), lay ()],
			seq [str "structure: ", l1],
			seq [str "signature: ", l2]])
	     end)
	 end
      val equalSchemes =
	 Trace.trace
	 ("equalSchemes",
	  fn (s, s', _, _, _, _) => Layout.tuple [Scheme.layout s,
						  Scheme.layout s'],
	  Unit.layout)
	 equalSchemes
      fun checkCons (Cons.T v, Cons.T v', strids): unit =
	 let
	    fun lay (c: Ast.Con.t) =
	       Longcon.layout (Longcon.long (rev strids, c))
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
			    (s, s',
			     "constructor",
			     "constructor type",
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
		      seq [str (concat ["constructors in ", name, " only: "]),
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
      val I' =
	 Interface.realize
	 (I, fn (c, a, k) =>
	  case Structure.peekLongtycon (S, c) of
	     NONE => TypeStr.bogus k
	   | SOME typeStr =>
		let
		   val _ =
		      if AdmitsEquality.<= (a, TypeStr.admitsEquality typeStr)
			 then ()
		      else
			 let
			    open Layout
			 in
			    Control.error
			    (region,
			     seq [str "type ", Longtycon.layout c,
				  str " admits equality in ",
				  str sign, str " but not in structure"],
			     empty)
			 end
		   val k' = TypeStr.kind typeStr
		   val typeStr =
		      if Kind.equals (k, k')
			 then typeStr
		      else
			 let
			    open Layout
			    val _ =
			       Control.error
			       (region,
				seq [str "type ", Longtycon.layout c,
				     str " has arity ", Kind.layout k',
				     str " in structure but arity ",
				     Kind.layout k, str " in ", str sign],
				empty)
			 in
			    TypeStr.bogus k
			 end
		in
		   typeStr
		end)
	 
      val {destroy,
	   get: Structure.t -> (Interface.t * Structure.t) list ref,
	   ...} =
	 Property.destGet (Structure.plist,
			   Property.initFun (fn _ => ref []))
      fun cut (S, I, strids): Structure.t =
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
      and reallyCut (S, I, strids) =
	 let
	    val {addStr, addType, addVal, finish} = Structure.maker ()
	    fun handleStr {name, interface = I} =
	       case Structure.peekStrid' (S, name) of
		  NONE =>
		     error ("structure",
			    Longstrid.layout (Longstrid.long (rev strids, name)))
		| SOME {range, ...} =>
		     addStr {domain = name,
			     range = cut (range, I, name :: strids)}
	    fun handleType {name: Ast.Tycon.t,
			    typeStr: TypeStr.t} =
	       let
		  fun layoutName () =
		     Longtycon.layout
		     (Longtycon.long (rev strids, name))
	       in
		  case Structure.peekTycon' (S, name) of
		     NONE => error ("type", layoutName ())
		   | SOME {range = typeStr', ...} =>
			let
			   fun tyconScheme (c: Tycon.t): Scheme.t =
			      let
				 val tyvars =
				    case TypeStr.kind typeStr' of
				       Kind.Arity n =>
					  Vector.tabulate
					  (n, fn _ =>
					   Tyvar.newNoname
					   {equality = false})
				     | _ => Error.bug "Nary tycon"
			      in
				 Scheme.make
				 {canGeneralize = true,
				  ty = Type.con (c, Vector.map (tyvars, Type.var)),
				  tyvars = tyvars}
			      end
			   datatype z = datatype TypeStr.node
			   val k = TypeStr.kind typeStr
			   val k' = TypeStr.kind typeStr'
			   fun typeStrScheme (s: TypeStr.t) =
			      case TypeStr.node s of
				 Datatype {tycon, ...} =>
				    tyconScheme tycon
			       | Scheme s => s
			       | Tycon c' => tyconScheme c'
			   val typeStr =
			      if not (Kind.equals (k, k'))
				 then
				    let
				       open Layout
				    in
				       Control.error
				       (region,
					seq [str "type ", layoutName (),
					     str " has arity ", Kind.layout k',
					     str " in structure but arity ", Kind.layout k,
					     str " in ", str sign],
					empty)
				       ; typeStr
				    end
			      else
				 case TypeStr.node typeStr of
				    Datatype {cons = c, ...} =>
				       (case TypeStr.node typeStr' of
					   Datatype {cons = c', ...} =>
					      (checkCons (c', c,
							  strids)
					       ; typeStr')
					 | _ =>
					      let
						 open Layout
					      in
						 Control.error
						 (region,
						  seq [str "type ",
						       layoutName (),
						       str " is a datatype in ",
						       str sign,
						       str " but not in structure"],
						  Layout.empty)
						 ; typeStr
					      end)
				  | Scheme s =>
				       (equalSchemes
					(typeStrScheme typeStr', s,
					 "type", "type definition",
					 layoutName, region)
					; typeStr)
				  | Tycon c =>
				       (equalSchemes
					(typeStrScheme typeStr',
					 tyconScheme c,
					 "type", "type definition",
					 layoutName, region)
					; typeStr)
			in
			   addType {domain = name,
				    range = typeStr}
			end
	       end
	    fun handleVal {name, scheme = s, status} =
	       case Structure.peekVid' (S, name) of
		  NONE =>
		     error ("variable",
			    Longvid.layout (Longvid.long (rev strids, name)))
		| SOME {range = (vid, s'), ...} =>
		     let
			val (tyvars, t) = Scheme.dest s
			val {args, instance = t'} = Scheme.instantiate s'
			val _ =
			   Type.unify
			   (t, t', preError, fn (l, l') =>
			    let
			       open Layout
			    in
			       (region,
				seq [str "variable type in structure disagrees with ", str sign],
				align [seq [str "variable: ",
					    Longvid.layout	
					    (Longvid.long
					     (rev strids, name))],
				       seq [str "structure: ", l'],
				       seq [str "signature: ", l]])
			    end)
			fun addDec (n: Exp.node): Vid.t =
			   let
			      val x = Var.newNoname ()
			      val e = Exp.make (n, t')
			      val _ =
				 List.push
				 (decs,
				  Dec.Val
				  {rvbs = Vector.new0 (),
				   tyvars = fn () => tyvars,
				   vbs = (Vector.new1
					  {exp = e,
					   lay = fn _ => Layout.empty,
					   pat = Pat.var (x, t'),
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
				 if 0 < Vector.length tyvars
				    orelse 0 < Vector.length (args ())
				    then
				       addDec
				       (Exp.Var (fn () => x, args))
				 else vid
				     | (Vid.Con _, Status.Con) => vid
				     | (Vid.Exn _, Status.Exn) => vid
				     | _ =>
					  (Control.error
					   (region,
					    Layout.str
					    (concat
					     ["identifier ",
					      Longvid.toString
					      (Longvid.long (rev strids,
							     name)),
					      " is ",
					      Vid.statusPretty vid,
					      " in the structure but ",
					      Status.pretty status,
					      " in the ", sign]),
					    Layout.empty)
					   ; vid)
		     in
			addVal {domain = name,
				range = (vid, s)}
		     end
	    val _ = Interface.foreach (I, {handleStr = handleStr,
					   handleType = handleType,
					   handleVal = handleVal})
	 in
	    finish ()
	 end
      val S = cut (S, I', [])
      val _ = destroy ()
      val S =
	 (* We avoid doing the opaque match if numErrors > 0 because it can lead
	  * to internal errors that might be confusing to the user.
	  *)
	 if not opaque orelse !Control.numErrors > 0
	    then S
	 else
	    let
	       fun fixCons (Cons.T cs, Cons.T cs') =
		  Cons.T
		  (Vector.map
		   (cs', fn {con, name, scheme} =>
		    let
		       val con =
			  case Vector.peek (cs, fn {name = n, ...} =>
					    Ast.Con.equals (n, name)) of
			     NONE => Con.bogus
			   | SOME {con, ...} => con
		    in
		       {con = con, name = name, scheme = scheme}
		    end))
	       val (S', instantiate) = dummyStructure (E, prefix, I)
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
				 Structure.T {plist = PropertyList.new (),
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
   in
      (S, Decs.fromList (!decs))
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
	  | {domain, isUsed, range, ...} :: _ =>
	       List.push
	       (add, fn s0 =>
		(List.push (vs, {domain = domain,
				 isUsed = isUsed,
				 range = range,
				 scope = s0})
		 ; List.push (current, v)))
      val _ =
	 foreach (E, {fcts = doit fcts,
		      fixs = doit fixs,
		      sigs = doit sigs,
		      strs = doit strs,
		      types = doit types,
		      vals = doit vals})
   in
      fn th =>
      let
	 val s0 = Scope.new ()
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
	    foreach (E, {fcts = doit,
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

val useFunctorSummary = ref false
		     
fun functorClosure
   (E: t,
    prefix: string,
    argInt: Interface.t,
    makeBody: Structure.t * string list -> Decs.t * Structure.t) =
   let
      val (formal, instantiate) = dummyStructure (E, prefix, argInt)
      val _ = useFunctorSummary := true
      (* Keep track of all tycons created during the instantiation of the
       * functor.  These will later become the generative tycons that will need
       * to be recreated for each functor application.
       *)
      val _ = newTycons := []
      val (_, res) = makeBody (formal, [])
      val generative = !newTycons
      val _ = newTycons := []
      val _ = useFunctorSummary := false
      val restore = snapshot E
      fun apply (arg, nest, region) =
	 let
	    val (actual, decs) =
	       cut (E, arg, argInt,
		    {isFunctor = true, opaque = false, prefix = ""}, region)
	 in
	    if !useFunctorSummary
	       then
		  let
		     val {destroy = destroy1,
			  get = tyconTypeStr: Tycon.t -> TypeStr.t option,
			  set = setTyconTypeStr, ...} =
			Property.destGetSet (Tycon.plist,
					     Property.initConst NONE)
		     (* Match the actual against the formal, to set the tycons.
		      * Then duplicate the res, replacing tycons.
		      * Want to generate new tycons just like the functor body
		      * did.
		      *)
		     val _ =
			instantiate (actual, fn (c, s) =>
				     setTyconTypeStr (c, SOME s))
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
			 (v, fn {con, name, scheme} =>
			  {con = con,
			   name = name,
			   scheme = replaceScheme scheme}))
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
			 (fn (Structure.T {strs, types, vals, ... },
			      replacement) =>
			  Structure.T
			  {plist = PropertyList.new (),
			   strs = Info.map (strs, replacement),
			   types = Info.map (types, replaceTypeStr),
			   vals = Info.map (vals, fn (v, s) =>
					    (v, replaceScheme s))}))
		     val res = replacement res
		     val _ = destroy1 ()
		     val _ = destroy2 ()
		  in
		     (Decs.empty, res)
		  end
	    else
	       let
		  val (decs', str) = restore (fn () => makeBody (actual, nest))
	       in
		  (Decs.append (decs, decs'),
		   str)
	       end
	 end
      val apply =
	 Trace.trace ("functorApply",
		      Structure.layout o #1,
		      Layout.tuple2 (Layout.ignore, Structure.layout))
	 apply
      fun sizeMessage () = layoutSize apply
      val fc =
	 FunctorClosure.T {apply = apply,
			   sizeMessage = sizeMessage}
   in
      fc
   end

end
