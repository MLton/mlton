(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor ElaborateEnv (S: ELABORATE_ENV_STRUCTS): ELABORATE_ENV =
struct

open S

local open Ast
in structure Fixity = Fixity
   structure Strid = Strid
   structure Longcon = Longcon
   structure Longvid = Longvid
   structure Longstrid = Longstrid
   structure Longtycon = Longtycon
end

local open CoreML
in structure Con = Con
   structure Var = Var
   structure Prim = Prim
   structure Record = Record
   structure Scheme = Scheme
   structure Srecord = SortedRecord
   structure Tycon = Tycon
   structure Type = Type
   structure Tyvar = Tyvar
   structure Var = Var
end

structure Scope = UniqueId ()

structure TypeStr =
   struct
      datatype t =
	 Datatype of {cons: {name: Ast.Con.t,
			     con: Con.t} vector,
		      tycon: Tycon.t}
       | Scheme of Scheme.t
       | Tycon of Tycon.t

      val bogus =
	 Scheme (Scheme.T
		 {tyvars = Vector.new0 (),
		  ty = Type.Var (Ast.Tyvar.newNoname {equality = false})})

      fun abs t =
	 case t of
	    Datatype {tycon, ...} => Tycon tycon
	  | _ => t

      fun apply (t, tys) =
	 case t of
	    Datatype {tycon, ...} => Type.con (tycon, tys)
	  | Scheme s => Scheme.apply (s, tys)
	  | Tycon t => Type.con (t, tys)

      fun cons t =
	 case t of
	    Datatype {cons, ...} => cons
	  | _ => Vector.new0 ()

      fun data (tycon, cons) = Datatype {tycon = tycon, cons = cons}

      val def = Scheme

      val tycon = Tycon

      fun layout t =
	 let open Layout
	 in case t of
	    Datatype {tycon, cons} =>
	       seq [str "Datatype ",
		    record [("tycon", Tycon.layout tycon),
			    ("cons", (Vector.layout (fn {name, con} =>
						     tuple [Ast.Con.layout name,
							    Con.layout con])
				      cons))]]
	  | Scheme s => Scheme.layout s
	  | Tycon t => seq [str "Tycon ", Tycon.layout t]
	 end
   end

structure Vid =
   struct
      open CoreML
	 
      datatype t =
	 Var of Var.t
       | Con of Con.t
       | ConAsVar of CoreML.Con.t
       | Exn of Con.t
       | Prim of Prim.t

      val statusString =
	 fn Var _ => "var"
	  | Prim _ => "var"
	  | ConAsVar _ => "var"
	  | Con _ => "con"
	  | Exn _ => "exn"

      val bogus = Var Var.bogus

      fun layout vid =
	 let open Layout
	    val (name, l) =
	       case vid of
		  Var v => ("Var", Var.layout v)
		| Con c => ("Con", Con.layout c)
		| ConAsVar c => ("ConAsVar", Con.layout c)
		| Exn c => ("Exn", Con.layout c)
		| Prim p => ("Prim", Prim.layout p)
	 in if false
	       then l
	    else paren (seq [str name, str " ", l])
	 end

      val deVar =
	 fn Var v => SOME v
	  | _ => NONE
		
      val deCon =
	 fn Con c => SOME c
	  | Exn c => SOME c
	  | _ => NONE

      val dePrim =
	 fn Prim p => SOME p
	  | _ => NONE
	  
      fun output (r, out) = Layout.output (layout r, out)
   end

fun layoutSize z = Int.layout (MLton.size z)
   
structure Values =
   struct
      datatype ('a, 'b) t = T of {domain: 'a,
				  ranges: {scope: Scope.t,
					   value: 'b} list ref}

      fun sizeMessage (vs as T {domain, ranges}, layoutA, layoutB) =
	 let
	    open Layout
	 in
	    seq [layoutA domain, str " ",
		 List.layout (layoutB o #value) (!ranges)]
	 end

      fun layout (layoutDomain, layoutRange) (T {domain, ranges, ...}) =
	 Layout.tuple [layoutDomain domain,
		       List.layout (layoutRange o #value) (!ranges)]

      fun new domain = T {domain = domain,
			  ranges = ref []}

      local
	 fun f g (T r) = g r
      in
	 fun domain z = f #domain z
	 fun ranges z = f #ranges z
      end

      fun isEmpty (T {ranges, ...}) = List.isEmpty (!ranges)

      val pop: ('a, 'b) t -> 'b =
	 fn T {ranges, ...} => #value (List.pop ranges)
   end

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

(* ------------------------------------------------- *)
(*                     Interface                     *)
(* ------------------------------------------------- *)

structure Interface =
   struct
      structure Info =
	 struct
	    (* The array is sorted by domain element. *)
	    datatype ('a, 'b) t = T of {range: 'b,
					values: ('a, 'b) Values.t} array

	    fun bogus () = T (Array.tabulate (0, fn _ => Error.bug "impossible"))

	    fun layout (layoutDomain, layoutRange) (T a) =
	       Array.layout (fn {range, values} =>
			     Layout.tuple [layoutDomain (Values.domain values),
					   layoutRange range])
	       a

	    fun foreach (T a, f) =
	       Array.foreach (a, fn {range, values, ...} =>
			      f (Values.domain values, range))

	    fun peek (T a, compare, domain) =
	       Option.map
	       (BinarySearch.search
		(a, fn {values, ...} => compare (domain, Values.domain values)),
		fn i => Array.sub (a, i))
	 end

      structure TypeStr =
	 struct
	    datatype t =
	       Datatype of {cons: Ast.Con.t vector}
	     | Tycon

	    val cons =
	       fn Datatype {cons, ...} => cons
		| Tycon => Vector.new0 ()

	    fun layout t =
	       let
		  open Layout
	       in
		  case t of
		     Datatype {cons, ...} =>
			seq [str "Datatype ", Vector.layout Ast.Con.layout cons]
		   | Tycon => str "Tycon"
	       end
	 end
      
      datatype t = T of {id: ShapeId.t,
			 strs: (Ast.Strid.t, t) Info.t,
			 vals: (Ast.Vid.t, Status.t) Info.t,
			 types: (Ast.Tycon.t, TypeStr.t) Info.t}

      local
	 fun make (field, compare) (T fields, domain)  =
	    Option.map (Info.peek (field fields, compare, domain), #range)
      in
	 val peekStrid = make (#strs, Ast.Strid.compare)
	 val peekTycon = make (#types, Ast.Tycon.compare)
      end

      fun peekStrids (I: t, strids: Ast.Strid.t list): t option =
	 case strids of
	    [] => SOME I
	  | s :: strids =>
	       case peekStrid (I, s) of
		  NONE => NONE
		| SOME I => peekStrids (I, strids)
   
      val bogus = T {id = ShapeId.new (),
		     strs = Info.bogus (),
		     vals = Info.bogus (),
		     types = Info.bogus ()}

      fun layout (T {strs, vals, types, ...}) =
	 Layout.record
	 [("strs", Info.layout (Ast.Strid.layout, layout) strs),
	  ("vals", Info.layout (Ast.Vid.layout, Status.layout) vals),
	  ("types", Info.layout (Ast.Tycon.layout, TypeStr.layout) types)]

      fun shapeId (T {id, ...}) = id

      fun foreach (T {strs, vals, types, ...},
		   {handleStr, handleType, handleVal}) =
	 (Info.foreach (strs, handleStr)
	  ; Info.foreach (vals, handleVal)
	  ; Info.foreach (types, handleType))
   end

(* ------------------------------------------------- *)
(*                     Structure                     *)
(* ------------------------------------------------- *)

structure Structure =
   struct
      structure Info = Interface.Info

      datatype t = T of {shapeId: ShapeId.t option,
			 strs: (Ast.Strid.t, t) Info.t,
			 vals: (Ast.Vid.t, Vid.t) Info.t,
			 types: (Ast.Tycon.t, TypeStr.t) Info.t}

      fun layout (T {strs, vals, types, ...}) =
	 Layout.record
	 [("types", Info.layout (Ast.Tycon.layout, TypeStr.layout) types),
	  ("vals", Info.layout (Ast.Vid.layout, Vid.layout) vals),
	  ("strs", Info.layout (Ast.Strid.layout, layout) strs)]

      local
	 open Layout
      in
	 fun layoutTypeSpec (d, _) = seq [str "type ", Ast.Tycon.layout d]
	 fun layoutValSpec (d, r) =
	    seq [str (case r of
			 Vid.Var _ => "val"
		       | Vid.Con _ => "con"
		       | Vid.ConAsVar _ => "val"
		       | Vid.Exn _ => "exn"
		       | Vid.Prim _ => "val"),
		 str " ",
		 Ast.Vid.layout d]
	 fun layoutStrSpec (d, r) =
	    seq [str "structure ", Ast.Strid.layout d, str ": ",
		 layoutPretty r]
	 and layoutPretty (T {strs, vals, types, ...}) =
	    let
	       fun doit (Info.T a, layout) =
		  align (Array.foldr (a, [], fn ({range, values}, ac) =>
				      layout (Values.domain values,
					      range)
				      :: ac))
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

      val bogus = T {shapeId = NONE,
		     strs = Info.bogus (),
		     vals = Info.bogus (),
		     types = Info.bogus ()}

      local
	 fun make (field, compare) (T fields, domain) =
	    Info.peek (field fields, compare, domain)
      in
	 val peekStrid' = make (#strs, Ast.Strid.compare)
	 val peekVid' = make (#vals, Ast.Vid.compare)
	 val peekTycon' = make (#types, Ast.Tycon.compare)
      end

      fun peekStrid z = Option.map (peekStrid' z, #range)
      fun peekTycon z = Option.map (peekTycon' z, #range)
      fun peekVid z = Option.map (peekVid' z, #range)

      val peekVid =
	 Trace.trace2 ("peekVid",
		       layout, Ast.Vid.layout, Option.layout Vid.layout)
	 peekVid
	 
      local
	 fun make (from, de) (S, x) =
	    case peekVid (S, from x) of
	       NONE => NONE
	     | SOME vid => de vid
      in
	 val peekCon = make (Ast.Vid.fromCon, Vid.deCon)
	 val peekVar = make (Ast.Vid.fromVar, Vid.deVar)
      end

      fun peekStrids (S, strids) =
	 case strids of
	    [] => SOME S
	  | strid :: strids =>
	       case peekStrid (S, strid) of
		  NONE => NONE
		| SOME S => peekStrids (S, strids)

      fun peekLongtycon (S, t) =
	 let val (strids, t) = Ast.Longtycon.split t
	 in case peekStrids (S, strids) of
	    NONE => NONE
	  | SOME S => peekTycon (S, t)
	 end

      val lookupLongtycon = valOf o peekLongtycon
	 
      (* section 5.3, 5.5, 5.6 and rules 52, 53 *)
      fun cut {str, interface, opaque, region}: t =
	 let
	    fun error (name, l) =
	       Control.error
	       (region, let open Layout
			in seq [str name, str " ", l,
				str " in signature but not in structure"]
			end)
	    fun cut (S as T {shapeId, ...}, I, strids) =
	       let
		  val shapeId' = Interface.shapeId I
		  val cutoff =
		     if opaque then NONE
		     else case shapeId of
			NONE => NONE
		      | SOME shapeId =>
			   if ShapeId.equals (shapeId, shapeId')
			      then SOME S
			   else NONE
	       in case cutoff of
		  SOME S => S
		| NONE =>
		     let
			val strs = ref []
			val vals: {range: Vid.t,
				   values: (Ast.Vid.t, Vid.t) Values.t} list ref
			   = ref []
			val types: {range: TypeStr.t,
				    values: (Ast.Tycon.t, TypeStr.t) Values.t}
			   list ref = ref []
			fun handleStr (name, I) =
			   case peekStrid' (S, name) of
			      NONE =>
				 error
				 (Longstrid.className,
				  Longstrid.layout	
				  (Longstrid.long(rev strids, name)))
			    | SOME {range, values} =>
				 List.push
				 (strs, {range = cut (range, I, name :: strids),
					 values = values})
			fun handleType (name: Ast.Tycon.t,
					typeStr: Interface.TypeStr.t) =
			   case peekTycon' (S, name) of
			      NONE =>
				 error
				 (Longtycon.className,
				  Longtycon.layout
				  (Longtycon.long (rev strids, name)))
			    | SOME {range = typeStr', values} =>
				 let
				    datatype z = datatype TypeStr.t
				    val typeStr'' =
				       case typeStr of
					  Interface.TypeStr.Datatype {cons} =>
					     (case typeStr' of
						 Datatype _ => typeStr'
					       | _ =>
						    (Control.error
						     (region,
						      let open Layout
						      in seq [str "type ",
							      str " is a datatype in signature but not in structure"]
						      end)
						     ; TypeStr.bogus))
					| Interface.TypeStr.Tycon =>
					     let
						datatype z = datatype TypeStr.t
					     in case typeStr' of
						Datatype {tycon, ...} =>
						   Tycon tycon
					      | _ => typeStr'
					     end
				 in List.push (types, {range = typeStr'',
						       values = values})
				 end
			fun handleVal (name, status) =
			   case peekVid' (S, name) of
			      NONE =>
				 error (Longvid.className,
					Longvid.layout (Longvid.long
							(rev strids, name)))
			    | SOME {range = vid, values} =>
				 let
				    val vid =
				       case (vid, status) of
					  (Vid.Con c, Status.Var) =>
					     Vid.ConAsVar c
					| (Vid.Exn c, Status.Var) =>
					     Vid.ConAsVar c
					| (_, Status.Var) => vid
					| (Vid.Con _, Status.Con) => vid
					| (Vid.Exn _, Status.Exn) => vid
					| _ =>
					     (Control.error
					      (region,
					       Layout.str
					       (concat
						[Longvid.className,
						 " ",
						 Longvid.toString
						 (Longvid.long (rev strids,
								name)),
						 " has status ",
						 Vid.statusString vid,
						 " in structure but status ",
						 Status.toString status,
						 " in signature "]))
					      ; vid)
				 in List.push (vals, {range = vid,
						      values = values})
				 end
			val _ =
			   Interface.foreach
			   (I, {handleStr = handleStr,
				handleType = handleType,
				handleVal = handleVal})
			fun doit (elts, less) =
			   Info.T
			   (Array.fromList
			    (MergeSort.sort
			     (!elts,
			      fn ({values = v, ...}, {values = v', ...}) =>
			      less (Values.domain v, Values.domain v'))))
		     in T {shapeId = SOME shapeId',
			   strs = doit (strs, Ast.Strid.<=),
			   vals = doit (vals, Ast.Vid.<=),
			   types = doit (types, Ast.Tycon.<=)}
		     end
	       end
	 in cut (str, interface, [])
	 end

      val cut =
	 Trace.trace ("cut",
		      fn {str, interface, ...} =>
		      Layout.tuple [layout str, Interface.layout interface],
		      layout)
	 cut
   end

structure FunctorClosure =
   struct
      datatype t = T of {apply: Structure.t * Region.t -> Decs.t * Structure.t,
			 sizeMessage: unit -> Layout.t}

      val bogus = T {apply = fn _ => (Decs.empty, Structure.bogus),
 		     sizeMessage = fn _ => Layout.str "<bogus>"}

      fun apply (T {apply, ...}, s, r) = apply (s, r)
      fun sizeMessage (T {sizeMessage, ...}) = sizeMessage ()
	 
      fun layout _ = Layout.str "<functor closure>"
   end

(* ------------------------------------------------- *)
(*                     NameSpace                     *)
(* ------------------------------------------------- *)

structure NameSpace =
   struct
      datatype ('a, 'b) t =
	 T of {current: ('a, 'b) Values.t list ref,
	       equals: 'a * 'a -> bool,
	       hash: 'a -> word,
	       table: ('a, 'b) Values.t HashSet.t}

      fun fold (T {table, ...}, ac, f) =
	 HashSet.fold (table, [], fn (vs, ac) =>
		       if Values.isEmpty vs
			  then ac
		       else f (vs, ac))
	 
      fun domain s = fold (s, [], fn (vs, ac) => Values.domain vs :: ac)

      fun collect (T {current, ...}: ('a, 'b) t,
		   le: 'a * 'a -> bool): unit -> ('a, 'b) Structure.Info.t =
	 let
	    val old = !current
	    val _ = current := []
	 in fn () =>
	    let
	       val elts =
		  List.revMap (!current, fn values =>
			       {range = Values.pop values,
				values = values})
	       val _ = current := old
	       val a =
		  Array.fromList
		  (MergeSort.sort
		   (elts, fn ({values = v, ...}, {values = v', ...}) =>
		    le (Values.domain v, Values.domain v')))
	    in Structure.Info.T a
	    end
	 end

      fun peek (T {equals, hash, table, ...}, a) =
	 case HashSet.peek (table, hash a, fn vs =>
			    equals (a, Values.domain vs)) of
	    SOME (Values.T {ranges = ref ({value, ...} :: _), ...}) => SOME value
	  | _ => NONE

      fun sizeMessage (i as T {table, ...}: ('a, 'b) t,
		       layoutA: 'a -> Layout.t,
		       layoutB: 'b -> Layout.t) =
	 let
	    open Layout
	 in
	    align (seq [str "total ", layoutSize i]
		   :: (HashSet.fold
		       (table, [], fn (v, ac) =>
			Values.sizeMessage (v, layoutA, layoutB) :: ac)))
	 end

      fun new (equals, hash) =
	 T {current = ref [],
	    equals = equals,
	    hash = hash,
	    table = HashSet.new {hash = hash o Values.domain}}

      fun layout (layoutDomain, layoutRange) (T {table, ...}) =
	 HashSet.layout (Values.layout (layoutDomain, layoutRange)) table

      fun values (T {hash, equals, table, ...}, a) =
	 HashSet.lookupOrInsert (table, hash a,
				 fn vs => equals (a, Values.domain vs),
				 fn () => Values.new a)

      val update: ('a, 'b) t * Scope.t * ('a, 'b) Values.t * 'b -> unit =
	 fn (T {current, ...}, scope, values as Values.T {ranges, ...}, value) =>
	 let
	    val value = {scope = scope, value = value}
	    fun new () = (List.push (current, values)
			  ; List.push (ranges, value))
	 in case !ranges of
	    [] => new ()
	  | {scope = scope', ...} :: l =>
	       if Scope.equals (scope, scope')
		  then ranges := value :: l
	       else new ()
	 end
   end

(*---------------------------------------------------*)
(*                 Main Env Datatype                 *)
(*---------------------------------------------------*)

datatype t = T of {currentScope: Scope.t ref,
		   fcts: (Ast.Fctid.t, FunctorClosure.t) NameSpace.t,
		   fixs: (Ast.Vid.t, Ast.Fixity.t) NameSpace.t,
		   sigs: (Ast.Sigid.t, Interface.t) NameSpace.t,
		   strs: (Ast.Strid.t, Structure.t) NameSpace.t,
		   types: (Ast.Tycon.t, TypeStr.t) NameSpace.t,
		   vals: (Ast.Vid.t, Vid.t) NameSpace.t}

fun clean (T {fcts, fixs, sigs, strs, types, vals, ...}): unit =
   let
      fun doit (NameSpace.T {table, ...}) =
	 HashSet.removeAll (table, Values.isEmpty)
   in
      doit fcts; doit fixs; doit sigs
   (* Can't doit to the following because it removes Values.t components that
    * are referred to by structures.  Hence, later opens fail.
    *)
      (* doit strs; doit types; doit vals *)
   end

fun sizeMessage (E as T {fcts, fixs, sigs, strs, types, vals, ...}) =
   let
      val size = MLton.size
      open Layout
   in record [("total", Int.layout (size E)),
	      ("fcts", NameSpace.sizeMessage (fcts, Ast.Fctid.layout,
					 FunctorClosure.sizeMessage)),
	      ("sigs", NameSpace.sizeMessage (sigs, Ast.Sigid.layout, layoutSize)),
	      ("strs", NameSpace.sizeMessage (strs, Ast.Strid.layout, layoutSize))]
   end

fun empty () =
   T {currentScope = ref (Scope.new ()),
      fcts = NameSpace.new let open Ast.Fctid in (equals, hash) end,
      fixs = NameSpace.new let open Ast.Vid in (equals, hash) end,
      sigs = NameSpace.new let open Ast.Sigid in (equals, hash) end,
      strs = NameSpace.new let open Ast.Strid in (equals, hash) end,
      types = NameSpace.new let open Ast.Tycon in (equals, hash) end,
      vals = NameSpace.new let open Ast.Vid in (equals, hash) end}

fun layout (T {strs, types, vals, ...}) =
   Layout.tuple
   [NameSpace.layout (Ast.Tycon.layout, TypeStr.layout) types,
    NameSpace.layout (Ast.Vid.layout, Vid.layout) vals,
    NameSpace.layout (Ast.Strid.layout, Structure.layout) strs]

fun layoutPretty (T {fcts, sigs, strs, types, vals, ...}) =
   let
      open Layout
      fun doit (NameSpace.T {table, ...}, le, layout) =
	 let
	    val l =
	       HashSet.fold
	       (table, [], fn (Values.T {domain, ranges}, ac) =>
		case !ranges of
		   [] => ac
		 | {value, ...} :: _ => (domain, value) :: ac)
	 in align (List.map (MergeSort.sort
			     (l, fn ((d, _), (d', _)) => le (d, d')),
			     layout))
	 end
   in
      align [doit (types, Ast.Tycon.<=, Structure.layoutTypeSpec),
	     doit (vals, Ast.Vid.<=, Structure.layoutValSpec),
	     doit (sigs, Ast.Sigid.<=, fn (d, _) => seq [str "signature ",
							 Ast.Sigid.layout d]),
	     doit (fcts, Ast.Fctid.<=, fn (d, _) => seq [str "functor ",
							 Ast.Fctid.layout d]),
	     doit (strs, Ast.Strid.<=, Structure.layoutStrSpec)]
   end

(* ------------------------------------------------- *)
(*                  functorClosure                   *)
(* ------------------------------------------------- *)

fun snapshot (T {currentScope, fcts, fixs, sigs, strs, types, vals}):
   (unit -> 'a) -> 'a =
   let
      val s0 = Scope.new ()
      fun doit (NameSpace.T {current, table, ...}) =
	 let
	    val all =
	       HashSet.fold (table, [], fn (vs as Values.T {ranges, ...}, ac) =>
			     case !ranges of
				[] => ac
			      | {value, ...} :: _ => (value, vs) :: ac)

	 in fn () =>
	    let
	       val current0 = !current
	       val _ =
		  current :=
		  List.fold
		  (all, [], fn ((v, vs as Values.T {ranges, ...}), ac) =>
		   (List.push (ranges, {scope = s0, value = v})
		    ; vs :: ac))
	       val removed =
		  HashSet.fold
		  (table, [], fn (Values.T {ranges, ...}, ac) =>
		   let
		      val r = !ranges
		   in
		      case r of
			 [] => ac
		       | {scope, ...} :: _ =>
			    if Scope.equals (s0, scope)
			       then ac
			    else (ranges := []
				  ; (ranges, r) :: ac)
		   end)
	    in fn () => (List.foreach (!current, fn v => (Values.pop v; ()))
			 ; current := current0
			 ; List.foreach (removed, fn (ranges, r) =>
					 ranges := r))
	    end
	 end
      val fcts = doit fcts
      val fixs = doit fixs
      val sigs = doit sigs
      val strs = doit strs
      val types = doit types
      val vals = doit vals
   in fn th =>
      let
	 val s1 = !currentScope
	 val fcts = fcts ()
	 val fixs = fixs ()
	 val sigs = sigs ()
	 val strs = strs ()
	 val types = types ()
	 val vals = vals ()
	 val _ = currentScope := s0
	 val res = th ()
	 val _ = currentScope := s1
	 val _ = (fcts (); fixs (); sigs (); strs (); types (); vals ())
      in res
      end
   end
      
fun functorClosure (E: t,
		    argInt: Interface.t,
		    makeBody: Structure.t -> Decs.t * Structure.t) =
   let
      val restore = snapshot E
      fun apply (arg, region) =
	 let
	    val actual = Structure.cut {str = arg,
					interface = argInt,
					opaque = false,
					region = region}
	 in restore (fn () => makeBody actual)
	 end
      val apply =
	 Trace.trace ("functorApply",
		      Structure.layout o #1,
		      Layout.tuple2 (Decs.layout, Structure.layout))
	 apply
      fun sizeMessage () = layoutSize apply
   in
      FunctorClosure.T {apply = apply,
			sizeMessage = sizeMessage}
   end
   

(* ------------------------------------------------- *)
(*                       peek                        *)
(* ------------------------------------------------- *)

local
   fun 'a make field (T fields, a) = NameSpace.peek (field fields, a)
in
   val peekFctid = make #fcts
   val peekFix = make #fixs
   val peekFix =
      Trace.trace
      ("peekFix", Ast.Vid.layout o #2, Option.layout Ast.Fixity.layout)
      peekFix			      
   val peekSigid = make #sigs
   val peekStrid = make #strs
   val peekTycon = make #types
   val peekVid = make #vals
   fun peekVar (E, x) =
      case peekVid (E, Ast.Vid.fromVar x) of
	 NONE => NONE
       | SOME vid => Vid.deVar vid
end

fun peekCon (E: t, c: Ast.Con.t): CoreML.Con.t option =
   case peekVid (E, Ast.Vid.fromCon c) of
      NONE => NONE
    | SOME vid => Vid.deCon vid

local
   fun make (split, peek, strPeek) (E, x) =
      let
	 val (strids, x) = split x
      in case strids of
	 [] => peek (E, x)
       | strid :: strids =>
	    case peekStrid (E, strid) of
	       NONE => NONE
	     | SOME S =>
		  case Structure.peekStrids (S, strids) of
		     NONE => NONE
		   | SOME S => strPeek (S, x)
      end
in
   val peekLongstrid = make (Ast.Longstrid.split, peekStrid, Structure.peekStrid)
   val peekLongtycon = make (Ast.Longtycon.split, peekTycon, Structure.peekTycon)
   val peekLongvar = make (Ast.Longvar.split, peekVar, Structure.peekVar)
   val peekLongvid = make (Ast.Longvid.split, peekVid, Structure.peekVid)
   val peekLongcon = make (Ast.Longcon.split, peekCon, Structure.peekCon)
end

(* ------------------------------------------------- *)
(*                      lookup                       *)
(* ------------------------------------------------- *)

local
   fun make (peek: t * 'a -> 'b option,
	     bogus: 'b,
	     unbound: 'a -> unit) (E: t, x: 'a): 'b =
      case peek (E, x) of
	 SOME y => y
       | NONE => (unbound x; bogus)
in
   val lookupFctid = make (peekFctid, FunctorClosure.bogus, Ast.Fctid.unbound)
   val lookupLongcon = make (peekLongcon, Con.bogus, Ast.Longcon.unbound)
   val lookupLongstrid =
      make (peekLongstrid, Structure.bogus, Ast.Longstrid.unbound)
   val lookupLongtycon =
      make (peekLongtycon, TypeStr.bogus, Ast.Longtycon.unbound)
   val lookupLongvid = make (peekLongvid, Vid.bogus, Ast.Longvid.unbound)
   val lookupLongvar = make (peekLongvar, Var.bogus, Ast.Longvar.unbound)
   val lookupSigid = make (peekSigid, Interface.bogus, Ast.Sigid.unbound)
end

(* ------------------------------------------------- *)
(*                      extend                       *)
(* ------------------------------------------------- *)

local
   fun make get (T (fields as {currentScope, ...}), domain, range) =
      let
	 val ns = get fields
      in NameSpace.update (ns, !currentScope,
			   NameSpace.values (ns, domain),
			   range)
      end
in
   val extendFctid = make #fcts
   val extendFix = make #fixs
   val extendFix =
      Trace.trace ("extendFix",
		   fn (_, x, f) => Layout.tuple [Ast.Vid.layout x,
						 Ast.Fixity.layout f],
		   Unit.layout)
      extendFix
   val extendSigid = make #sigs
   val extendStrid = make #strs
   val extendTycon = make #types
   val extendVals = make #vals
end

val extendTycon =
   Trace.trace3 ("extendTycon", layout, Ast.Tycon.layout, TypeStr.layout,
		 Unit.layout)
   extendTycon

fun extendCon (E, c, c') =
   extendVals (E, Ast.Vid.fromCon c, Vid.Con c')
	       
fun extendExn (E, c, c') =
   extendVals (E, Ast.Vid.fromCon c, Vid.Exn c')
	       
fun extendVar (E, x, x') =
   extendVals (E, Ast.Vid.fromVar x, Vid.Var x')

val extendVar =
   Trace.trace3 ("extendVar", layout, Ast.Var.layout, Var.layout, Unit.layout)
   extendVar

(* ------------------------------------------------- *)   
(*                       local                       *)
(* ------------------------------------------------- *)

local
   fun doit (info as NameSpace.T {current, ...}, s0) =
      let
	 val old = !current
	 val _ = current := []
      in fn () =>
	 let
	    val c1 = !current
	    val _ = current := []
	 in fn () =>
	    let
	       val c2 = !current
	       val lift = List.map (c2, Values.pop)
	       val _ = List.foreach (c1, fn v => (Values.pop v; ()))
	       val _ = current := old
	       val _ = List.foreach2 (lift, c2, fn (v, vs) =>
				      NameSpace.update (info, s0, vs, v))
	    in ()
	    end
	 end
      end
in
   fun localTop (T {currentScope, fcts, fixs, sigs, strs, types, vals, ...},
		 f1, f2) =
      let
	 val s0 = !currentScope
	 val fcts = doit (fcts, s0)
	 val fixs = doit (fixs, s0)
	 val sigs = doit (sigs, s0)
	 val strs = doit (strs, s0)
	 val types = doit (types, s0)
	 val vals = doit (vals, s0)
	 val _ = currentScope := Scope.new ()
	 val a1 = f1 ()
	 val fcts = fcts ()
	 val fixs = fixs ()
	 val sigs = sigs ()
	 val strs = strs ()
	 val types = types ()
	 val vals = vals ()
	 val _ = currentScope := Scope.new ()
	 val a2 = f2 ()
	 val _ = (fcts (); fixs (); sigs (); strs (); types (); vals ())
	 val _ = currentScope := s0
      in (a1, a2)
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
	 val a2 = f2 ()
	 val _ = (fixs (); strs (); types (); vals ())
	 val _ = currentScope := s0
      in (a1, a2)
      end

   (* Can't eliminate the use of strs in localCore, because openn still modifies
    * module level constructs.
    *)
   val localCore = localTop
end

fun makeStructure (T {currentScope, fixs, strs, types, vals, ...}, make) =
   let
      val f = NameSpace.collect (fixs, Ast.Vid.<=)
      val s = NameSpace.collect (strs, Ast.Strid.<=)
      val t = NameSpace.collect (types, Ast.Tycon.<=)
      val v = NameSpace.collect (vals, Ast.Vid.<=)
      val s0 = !currentScope
      val _ = currentScope := Scope.new ()
      val res = make ()
      val _ = f ()
      val S = Structure.T {shapeId = NONE,
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

fun openStructure (T {currentScope, strs, vals, types, ...},
		   Structure.T {strs = strs',
				vals = vals',
				types = types', ...}): unit =
   let
      val scope = !currentScope
      fun doit (info, Structure.Info.T a) =
	 Array.foreach (a, fn {range, values} =>
			NameSpace.update (info, scope, values, range))
   in doit (strs, strs')
      ; doit (vals, vals')
      ; doit (types, types')
   end

(* ------------------------------------------------- *)
(*                  InterfaceMaker                   *)
(* ------------------------------------------------- *)

structure Env =
   struct
      datatype t = datatype t

      val lookupLongtycon = lookupLongtycon
   end

structure InterfaceMaker =
   struct
      structure NameSpace =
	 struct
	    open NameSpace

	    fun update (T {current, ...}, scope, values, value) =
	       let
		  val ranges = Values.ranges values
		  val value = {scope = scope, value = value}
		  fun new () = (List.push (current, values)
				; List.push (ranges, value))
	       in case !ranges of
		  [] => new ()
		| {scope = scope', ...} :: l =>
		     if Scope.equals (scope, scope')
			then Control.error (Region.bogus,
					    Layout.str "duplicate spec")
		     else new ()
	       end
	 end

      datatype t = T of {currentScope: Scope.t ref,
			 env: Env.t,
			 strs: (Ast.Strid.t, Interface.t) NameSpace.t,
			 types: (Ast.Tycon.t, Interface.TypeStr.t) NameSpace.t,
			 vals: (Ast.Vid.t, Status.t) NameSpace.t}

      local
	 fun make sel (T (fields as {currentScope, ...}), d, r) =
	    let
	       val info as NameSpace.T {equals, hash, table, ...} = sel fields
	    in NameSpace.update
	       (info, !currentScope,
		HashSet.lookupOrInsert (table, hash d,
					fn vs => equals (d, Values.domain vs),
					fn () => Values.new d),
		r)
	    end
      in
	 val addStrid = make #strs
	 val addTycon' = make #types
	 val addVid = make #vals
      end

      fun addCon (m, c) = addVid (m, Ast.Vid.fromCon c, Status.Con)
      fun addExcon (m, c) = addVid (m, Ast.Vid.fromCon c, Status.Exn)
      fun addVar (m, x) = addVid (m, Ast.Vid.fromVar x, Status.Var)
      fun addTycon (m as T {env = Env.T {vals, ...}, ...}, tyc, cons) =
	 let
(* 	    val cons =
 * 	       List.revMap
 * 	       (cons, fn c =>
 * 		{con = c,
 * 		 values = NameSpace.values (vals, Ast.Vid.fromCon c)})
 *)
	 in addTycon' (m, tyc,
		       if Vector.isEmpty cons
			  then Interface.TypeStr.Tycon
		       else Interface.TypeStr.Datatype {cons = cons})
	    ; Vector.foreach (cons, fn c => addCon (m, c))
	 end

      fun includeInterface (T {currentScope, strs, types, vals, ...},
			    Interface.T {strs = strs',
					 types = types',
					 vals = vals', ...}): unit =
	 let
	    val scope = !currentScope
	    fun doit (info, Interface.Info.T a) =
	       Array.foreach (a, fn {range, values} =>
			      NameSpace.update (info, scope, values, range))
	 in doit (strs, strs')
	    ; doit (vals, vals')
	    ; doit (types, types')
	 end

      fun lookupLongtycon (T {env, strs, types, ...},
			   x): Ast.Con.t vector =
	 let
	    fun unbound () = (Ast.Longtycon.unbound x; Vector.new0 ())
	    fun lookInEnv () =
	       let
		  val typeStr = Env.lookupLongtycon (env, x)
	       in
		  Vector.map (TypeStr.cons typeStr, #name)
	       end
	    val (strids, tycon) = Ast.Longtycon.split x
	 in
	    case strids of
	       [] => (case NameSpace.peek (types, tycon) of
			 NONE => lookInEnv ()
		       | SOME typeStr => Interface.TypeStr.cons typeStr)
	     | s :: strids =>
		  (case NameSpace.peek (strs, s) of
		      NONE => lookInEnv ()
		    | SOME I =>
			 (case Interface.peekStrids (I, strids) of
			     NONE => unbound ()
			   | SOME I =>
				case Interface.peekTycon (I, tycon) of
				   NONE => unbound ()
				 | SOME typeStr =>
				      Interface.TypeStr.cons typeStr))
	 end

      fun makeInterface (T {currentScope, strs, types, vals, ...}, make) =
	 let
	    val strs = NameSpace.collect (strs, Ast.Strid.<=)
	    val types = NameSpace.collect (types, Ast.Tycon.<=)
	    val vals = NameSpace.collect (vals, Ast.Vid.<=)
	    val s0 = !currentScope
	    val _ = currentScope := Scope.new ()
	    val res = make ()
	    val I = Interface.T {id = ShapeId.new (),
				 strs = strs (),
				 types = types (),
				 vals = vals ()}
	    val _ = currentScope := s0
	 in (res, I)
	 end
   end

fun makeInterfaceMaker E =
   InterfaceMaker.T
   {currentScope = ref (Scope.new ()),
    env = E,
    strs = NameSpace.new let open Ast.Strid in (equals, hash) end,
    types = NameSpace.new let open Ast.Tycon in (equals, hash) end,
    vals = NameSpace.new let open Ast.Vid in (equals, hash) end}
   
fun addEquals E =
   extendVals (E, Ast.Vid.fromString "=", Vid.Prim Prim.equal)

end
