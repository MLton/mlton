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
   structure Fixity = Fixity
   structure Strid = Strid
   structure Longcon = Longcon
   structure Longvid = Longvid
   structure Longstrid = Longstrid
   structure Longtycon = Longtycon
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

structure Kind = Tycon.Kind

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
       | Overload of (Var.t * Type.t) vector
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
		| Overload xts =>
		     ("Overload",
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
   
structure Values =
   struct
      datatype ('a, 'b) t = T of {domain: 'a,
				  ranges: {isUsed: bool ref,
					   scope: Scope.t,
					   value: 'b} list ref}

      fun domain (T {domain, ...}) = domain
	    
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

      val pop: ('a, 'b) t -> {isUsed: bool ref,
			      scope: Scope.t,
			      value: 'b} =
	 fn T {ranges, ...} => List.pop ranges
   end

structure TypeStr = TypeStr (structure Con = Con
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
   structure ShapeId = ShapeId
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
      datatype ('a, 'b) t = T of {isUsed: bool ref,
				  range: 'b,
				  values: ('a, 'b) Values.t} array

      fun bogus () = T (Array.tabulate (0, fn _ => Error.bug "impossible"))

      fun layout (layoutDomain, layoutRange) (T a) =
	 Array.layout (fn {range, values, ...} =>
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
	  fn i =>
	  let
	     val v as {isUsed, ...} =  Array.sub (a, i)
	     val _ = isUsed := !Control.showBasisUsed
	  in
	     v
	  end)
   end

(* pre: arities are equal. *)
fun equalSchemes (s: Scheme.t, s': Scheme.t, name: unit -> Layout.t, r: Region.t)
   : unit =
   let
      val (tyvars, ty) = Scheme.dest s
      val (_, ty') = Scheme.dest s'
      val tyvars =
	 Vector.tabulate (Vector.length tyvars, fn _ =>
			  Type.var (Tyvar.newNoname {equality = false}))
   in
      Type.unify
      (Scheme.apply (s, tyvars), Scheme.apply (s', tyvars), fn (l1, l2) =>
       let
	  open Layout
       in
	  (r,
	   seq [str "type ", name (),
		str " in structure disagrees with signature"],
	   align [seq [str "structure: ", l1],
		  seq [str "signature: ", l2]])
       end)
   end

(* ------------------------------------------------- *)
(*                     Structure                     *)
(* ------------------------------------------------- *)

structure Structure =
   struct
      datatype t = T of {shapeId: ShapeId.t option,
			 strs: (Ast.Strid.t, t) Info.t,
			 types: (Ast.Tycon.t, TypeStr.t) Info.t,
			 vals: (Ast.Vid.t, Vid.t * Scheme.t) Info.t}

      fun layoutUsed (T {strs, types, vals, ...}) =
	 let
	    open Layout
	    fun doit (Info.T a, lay): Layout.t =
	       align
	       (Array.foldr (a, [], fn ({isUsed, range, values}, ac) =>
			     if not (!isUsed)
				then ac
			     else lay (Values.domain values, range) :: ac))
	    fun doitn (i, name, lay) =
	       doit (i, fn (d, _) => seq [str name, lay d])
	 in
	    align [doitn (types, "type ", Ast.Tycon.layout),
		   doitn (vals, "val ", Ast.Vid.layout),
		   doit (strs, fn (d, r) =>
			 align [seq [str "structure ", Ast.Strid.layout d],
				indent (layoutUsed r, 3)])]
	 end

      fun layout (T {strs, vals, types, ...}) =
	 Layout.record
	 [("types", Info.layout (Ast.Tycon.layout, TypeStr.layout) types),
	  ("vals",
	   Info.layout (Ast.Vid.layout,
			Layout.tuple2 (Vid.layout, Scheme.layout))
	   vals),
	  ("strs", Info.layout (Ast.Strid.layout, layout) strs)]

      local
	 open Layout
      in
	 fun layoutTypeSpec (d, _) = seq [str "type ", Ast.Tycon.layout d]
	 fun layoutValSpec (d, (vid, scheme)) =
	    let
	       fun simple s =
		  seq [str s, str " ", Ast.Vid.layout d, str ": ",
		       Scheme.layoutPretty scheme]
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
	 fun layoutStrSpec (d, r) =
	    align [seq [str "structure ", Ast.Strid.layout d, str ":"],
		   indent (layoutPretty r, 3)]
	 and layoutPretty (T {strs, vals, types, ...}) =
	    let
	       fun doit (Info.T a, layout) =
		  align (Array.foldr (a, [], fn ({range, values, ...}, ac) =>
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
	    fun make (op <=) =
	       let
		  val r = ref []
		  fun add {range, values} =
		     List.push (r, {isUsed = ref false,
				    range = range,
				    values = values})
		  fun done () = 
		     Info.T
		     (QuickSort.sortArray
		      (Array.fromList (!r),
		       fn ({values = v, ...}, {values = v', ...}) =>
		       Values.domain v <= Values.domain v'))
	       in
		  (add, done)
	       end
	    val (addStr, strs) = make Ast.Strid.<=
	    val (addType, types) = make Ast.Tycon.<=
	    val (addVal, vals) = make Ast.Vid.<=
	    fun finish shapeId =
	       T {shapeId = shapeId,
		  strs = strs (), 
		  types = types (),
		  vals = vals ()}
	 in
	    {addStr = addStr,
	     addType = addType,
	     addVal = addVal,
	     finish = finish}
	 end
      
      (* section 5.3, 5.5, 5.6 and rules 52, 53 *)
      fun cut (str: t, {interface, opaque: bool, region}): t * Decs.t =
	 let
	    val decs = ref []
	    fun error (name, l) =
	       let
		  open Layout
	       in
		  Control.error
		  (region,
		   seq [str (concat [name, " "]), l,
			str " in signature but not in structure"],
		   empty)
	       end
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
				  (s, s', fn () =>
				   let
				      open Layout
				   in
				      seq [str "of ", lay n]
				   end,
				   region)
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
	    val interface =
	       Interface.realize
	       (interface, fn (c, a, k) =>
		case peekLongtycon (str, c) of
		   NONE => (error ("type", Longtycon.layout c)
			    ; TypeStr.bogus k)
		 | SOME typeStr =>
		      let
			 val k' = TypeStr.kind typeStr
		      in
			 if Kind.equals (k, k')
			    then typeStr
			 else
			    let
			       open Layout
			       val _ =
				  Control.error
				  (Longtycon.region c,
				   seq [str "type ", Longtycon.layout c,
					str "has arity ", Kind.layout k',
					str "in structure but arity ",
					Kind.layout k, str " in signature"],
				   empty)
			    in
			       TypeStr.bogus k
			    end
		      end)
	    fun cut (S as T {shapeId, ...}, I, strids) =
	       let
		  val {addStr, addType, addVal, finish} = maker ()
		  val shapeId' = Interface.shapeId I
		  fun doit () =
		     let
			fun handleStr {name, interface = I} =
			   case peekStrid' (S, name) of
			      NONE =>
				 error
				 ("structure",
				  Longstrid.layout	
				  (Longstrid.long (rev strids, name)))
			    | SOME {range, values, ...} =>
				 addStr {range = cut (range, I, name :: strids),
					 values = values}
			fun handleType {name: Ast.Tycon.t,
					typeStr: TypeStr.t} =
			   let
			      fun layoutName () =
				 Longtycon.layout
				 (Longtycon.long (rev strids, name))
			   in
			      case peekTycon' (S, name) of
				 NONE => error ("type", layoutName ())
			       | SOME {range = typeStr', values, ...} =>
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
							 str " in signature"],
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
								   str " is a datatype in signature but not in structure"],
							      Layout.empty)
							     ; typeStr
							  end)
					      | Scheme s =>
						   (equalSchemes
						    (typeStrScheme typeStr',
						     s, layoutName, region)
						    ; typeStr)
					      | Tycon c =>
						   (equalSchemes
						    (typeStrScheme typeStr',
						     tyconScheme c,
						     layoutName, region)
						    ; typeStr)
				    in
				       addType {range = typeStr,
						values = values}
				    end
			   end
                        fun handleVal {name, scheme = s, status} =
			   case peekVid' (S, name) of
			      NONE =>
				 error ("variable",
					Longvid.layout (Longvid.long
							(rev strids, name)))
			    | SOME {range = (vid, s'), values, ...} =>
				 let
				    val (tyvars, t) = Scheme.dest s
				    val {args, instance = t'} =
				       Scheme.instantiate s'
				    val _ =
				       Type.unify
				       (t, t', fn (l, l') =>
					let
					   open Layout
					in
					   (region,
					    seq [str "type of ",
						 Longvid.layout	
						 (Longvid.long
						  (rev strids, name)),
						 str " in structure disagrees with signature"],
					    align [seq [str "structure: ", l'],
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
						 " in the signature "]),
					       Layout.empty)
					      ; vid)
				 in
				    addVal {range = (vid, s),
					    values = values}
				 end
			val _ =
			   Interface.foreach
			   (I, {handleStr = handleStr,
				handleType = handleType,
				handleVal = handleVal})
		     in
			finish (SOME shapeId')
		     end
	       in
		  case shapeId of
		     NONE => doit ()
		   | SOME shapeId =>
			if ShapeId.equals (shapeId, shapeId')
			   then S
			else doit ()
	       end
	    val str = cut (str, interface, [])
	 in
	    (str, Decs.fromList (!decs))
	 end

      val cut =
	 Trace.trace ("cut",
		      fn (str, {interface, ...}) =>
		      Layout.tuple [layoutPretty str,
				    Interface.layout interface],
		      layout o #1)
	 cut

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
		   le: 'a * 'a -> bool): unit -> ('a, 'b) Info.t =
	 let
	    val old = !current
	    val _ = current := []
	 in
	    fn () =>
	    let
	       val elts =
		  List.revMap (!current, fn values =>
			       let
				  val {isUsed, value, ...} = Values.pop values
			       in
				  {isUsed = isUsed,
				   range = value,
				   values = values}
			       end)
	       val _ = current := old
	       val a =
		  QuickSort.sortArray
		  (Array.fromList elts,
		   fn ({values = v, ...}, {values = v', ...}) =>
		   le (Values.domain v, Values.domain v'))
	    in
	       Info.T a
	    end
	 end

      fun peek (T {equals, hash, table, ...}, a) =
	 case HashSet.peek (table, hash a, fn vs =>
			    equals (a, Values.domain vs)) of
	    SOME (Values.T {ranges = ref ({isUsed, value, ...} :: _), ...}) =>
	       (isUsed := !Control.showBasisUsed
		; SOME value)
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

      val update: ('a, 'b) t * Scope.t * {isUsed: bool ref,
					  range: 'b,
					  values: ('a, 'b) Values.t} -> unit =
	 fn (T {current, ...}, scope, {isUsed,
				       range,
				       values as Values.T {ranges, ...}}) =>
	 let
	    val value = {isUsed = isUsed,
			 scope = scope,
			 value = range}
	    fun new () = (List.push (current, values)
			  ; List.push (ranges, value))
	 in
	    case !ranges of
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
		   vals: (Ast.Vid.t, Vid.t * Scheme.t) NameSpace.t}

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
   in
      record
      [("total", Int.layout (size E)),
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
    NameSpace.layout (Ast.Vid.layout,
		      Layout.tuple2 (Vid.layout, Scheme.layout)) vals,
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
	 in align (List.map (QuickSort.sortList
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

fun layoutUsed (T {fcts, sigs, strs, types, vals, ...}) =
   let
      open Layout
      fun doit (NameSpace.T {table, ...}, le, layout) =
	 let
	    val all =
	       HashSet.fold
	       (table, [], fn (Values.T {domain, ranges}, ac) =>
		case !ranges of
		   [] => ac
		 | {isUsed, value, ...} :: _ =>
		      if !isUsed
			 then (domain, layout (domain, value)) :: ac
		      else ac)
	 in
	    align (List.map
		   (QuickSort.sortList
		    (all, fn ((d, _), (d', _)) => le (d, d')),
		    #2))
	 end
      fun doitn (ns, name, le, lay) =
	 doit (ns, le, fn (d, _) => seq [str name, str " ", lay d])

   in
      align [doitn (types, "type", Ast.Tycon.<=, Ast.Tycon.layout),
	     doitn (vals, "val", Ast.Vid.<=, Ast.Vid.layout),
	     doitn (sigs, "signature", Ast.Sigid.<=, Ast.Sigid.layout),
	     doitn (fcts, "functor", Ast.Fctid.<=, Ast.Fctid.layout),
	     doit (strs, Ast.Strid.<=,
		   fn (d, r) =>
		   align [seq [str "structure ", Ast.Strid.layout d],
			  indent (Structure.layoutUsed r, 3)])]
   end
   
fun dummyStructure (T {strs, types, vals, ...}, I: Interface.t): Structure.t =
   let
      val I =
	 Interface.realize
	 (I, fn (c, a, k) =>
	  let
	     val c = Tycon.fromString (Longtycon.toString c)
	     val _ = TypeEnv.tyconAdmitsEquality c := a
	  in
	     TypeStr.tycon (c, k)
	  end)
      val {get, ...} =
	 Property.get
	 (Interface.plist,
	  Property.initRec
	  (fn (I, get) =>
	   let
	      val {addStr, addType, addVal, finish} = Structure.maker ()
	      fun handleStr {name, interface = I} =
		 addStr {range = get I,
			 values = NameSpace.values (strs, name)}
	      fun handleType {name, typeStr} =
		 addType {range = typeStr,
			  values = NameSpace.values (types, name)}
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
		    addVal {range = (vid, scheme),
			    values = NameSpace.values (vals, name)}
		 end
	      val _ =
		 Interface.foreach
		 (I, {handleStr = handleStr,
		      handleType = handleType,
		      handleVal = handleVal})
	   in
	      finish (SOME (Interface.shapeId I))
	   end))
   in
      get I
   end

val dummyStructure =
   Trace.trace ("dummyStructure",
		Interface.layout o #2,
		Structure.layoutPretty)
   dummyStructure

(* ------------------------------------------------- *)
(*                  functorClosure                   *)
(* ------------------------------------------------- *)

fun snapshot (T {currentScope, fcts, fixs, sigs, strs, types, vals}):
   (unit -> 'a) -> 'a =
   let
      fun m l = Layout.outputl (l, Out.error)
      open Layout
      fun doit (NameSpace.T {current, table, ...}, lay) =
	 let
	    val all =
	       HashSet.fold
	       (table, [], fn (vs as Values.T {ranges, ...}, ac) =>
		case !ranges of
		   [] => ac
		 | z :: _ => (z, vs) :: ac)
	 in
	    fn s0 =>
	    let
	       val current0 = !current
	       val _ =
		  current :=
		  List.fold
		  (all, [], fn (({isUsed, value, ...},
				 vs as Values.T {ranges, ...}), ac) =>
		   (List.push (ranges, {isUsed = isUsed,
					scope = s0,
					value = value})
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
			 ; List.foreach (removed, op :=))
	    end
	 end
      val fcts = doit (fcts, Ast.Fctid.layout)
      val fixs = doit (fixs, Ast.Vid.layout)
      val sigs = doit (sigs, Ast.Sigid.layout)
      val strs = doit (strs, Ast.Strid.layout)
      val types = doit (types, Ast.Tycon.layout)
      val vals = doit (vals, Ast.Vid.layout)
   in
      fn th =>
      let
	 val s0 = Scope.new ()
	 val fcts = fcts s0
	 val fixs = fixs s0
	 val sigs = sigs s0
	 val strs = strs s0
	 val types = types s0
	 val vals = vals s0
	 val s1 = !currentScope
	 val _ = currentScope := s0
	 val res = th ()
	 val _ = currentScope := s1
	 val _ = (fcts (); fixs (); sigs (); strs (); types (); vals ())
      in
	 res
      end
   end
      
fun functorClosure
   (E: t,
    argInt: Interface.t,
    makeBody: Structure.t * string list -> Decs.t * Structure.t) =
   let
      val _ = makeBody (dummyStructure (E, argInt), [])
      val restore = snapshot E
      fun apply (arg, nest, region) =
	 let
	    val (actual, decs) =
	       Structure.cut (arg, {interface = argInt,
				    opaque = false,
				    region = region})
	    val (decs', str) = restore (fn () => makeBody (actual, nest))
	 in
	    (Decs.append (decs, decs'),
	     str)
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
			   {isUsed = ref false,
			    range = range,
			    values = NameSpace.values (ns, domain)})
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

fun extendCon (E, c, c', s) =
   extendVals (E, Ast.Vid.fromCon c, (Vid.Con c', s))
	       
fun extendExn (E, c, c', s) =
   extendVals (E, Ast.Vid.fromCon c, (Vid.Exn c', s))
	       
fun extendVar (E, x, x', s) =
   extendVals (E, Ast.Vid.fromVar x, (Vid.Var x', s))

fun extendOverload (E, x, yts, s) =
   extendVals (E, Ast.Vid.fromVar x, (Vid.Overload yts, s))

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
		  List.foreach2 (lift, c2, fn ({isUsed, value, ...}, values) =>
				 NameSpace.update
				 (info, s0, {isUsed = isUsed,
					     range = value,
					     values = values}))
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

end
