(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
(* This pass must happen before polymorphic equality is implemented becuase
 * 1. it will make polymorphic equality faster because some types are simpler
 * 2. it removes uses of polymorphic equality that must return true
 *
 * This pass computes a "cardinality" of each datatype, which is an
 * abstraction of the number of values of the datatype.
 *   Zero means the datatype has no values (except for bottom).
 *   One means the datatype has one values (except for bottom).
 *   Many means the datatype has many values.
 *
 * This pass removes all datatypes whose cardinality is Zero or One
 * and removes
 *   components of tuples
 *   function args
 *   constructor args
 * which are such datatypes.
 *
 * This pass marks constructors as one of
 *   Useless: it never appears in a ConApp.
 *   Transparent: it is the only variant in its datatype
 *     and its argument type does not contain any uses of
 *       Tycon.array or Tycon.vector.
 *   Useful: otherwise
 * This pass also removes Useless and Transparent constructors.
 *
 * We must keep track of Transparent consturctors whose argument type
 * uses Tycon.array because of datatypes like the following:
 *   datatype t = T of t vector
 * Such a datatype has Cardinality.Many, but we cannot eliminate
 * the datatype and replace the lhs by the rhs, i.e. we must keep the
 * circularity around .
 * Must do similar things for vectors.
 * 
 * Also, to eliminate as many Transparent constructors as possible, for
 * something like the following,
 *   datatype t = T of u array
 *   and u = U of t vector
 * we (arbitrarily) expand one of the datatypes first.
 * The result will be something like
 *   datatype u = U of u array array
 * where all uses of t are replaced by u array.
 *)

functor SimplifyTypes (S: SIMPLIFY_TYPES_STRUCTS): SIMPLIFY_TYPES = 
struct

open S
open Dec PrimExp Transfer
type int = Int.t

structure Cardinality =
   struct
      datatype t = Zero | One | Many

      val isMany: t -> bool =
	 fn Many => true
	  | _ => false
	       
      fun layout c =
	 Layout.str (case c of
			Zero => "zero"
		      | One => "one"
		      | Many => "many")

      val equals: t * t -> bool = op =

      val isZero: t -> bool =
	 fn Zero => true
	  | _ => false
   end

structure ConRep =
   struct
      datatype t =
	 Useless
       | Transparent
       | Useful

      val isUseful =
	 fn Useful => true
	  | _ => false

      val isUseless =
	 fn Useless => true
	  | _ => false

      val toString =
	 fn Useless => "useless"
	  | Transparent => "transparent"
	  | Useful => "useful"

      val layout = Layout.str o toString
   end

structure Result =
   struct
      datatype 'a t =
	 Bugg
       | Delete
       | Keep of 'a

      fun layout layoutX =
	 let open Layout
	 in fn Bugg => str "Bug"
       | Delete => str "Delete"
       | Keep x => seq [str "Keep ", layoutX x]
	 end
   end

fun simplify (Program.T {datatypes, globals, functions, main}) =
   let
      val {get = conInfo: Con.t -> {rep: ConRep.t ref,
				    args: Type.t vector},
	   set = setConInfo} =
	 Property.getSetOnce
	 (Con.plist, Property.initRaise ("SimplifyTypes.info", Con.layout))
      val conInfo =
	 Trace.trace ("conInfo",
		      Con.layout,
		      fn {rep, args} =>
		      Layout.record [("rep", ConRep.layout (!rep)),
				     ("args", Vector.layout Type.layout args)])
	 conInfo
      val conRep = ! o #rep o conInfo
      val conArgs = #args o conInfo
      fun setConRep (con, r) = #rep (conInfo con) := r
      val conIsUseful = ConRep.isUseful o conRep
      val conIsUseless = ConRep.isUseless o conRep
      val conIsUseful =
	 Trace.trace ("conIsUseful", Con.layout, Bool.layout) conIsUseful
      val setConRep =
	 Trace.trace2 ("setConRep", Con.layout, ConRep.layout, Unit.layout)
	 setConRep
      (* Initialize conInfo *)
      val _ =
	 Vector.foreach
	 (datatypes, fn {cons, ...} =>
	  Vector.foreach (cons, fn {con, args} =>
			  setConInfo (con, {rep = ref ConRep.Useless,
					    args = args})))
      val {get = tyconReplacement: Tycon.t -> Type.t option,
	   set = setTyconReplacement} =
	 Property.getSet (Tycon.plist, Property.initConst NONE)
      val setTyconReplacement = fn (c, t) => setTyconReplacement (c, SOME t)
      val {get = tyconInfo: Tycon.t -> {
					cardinality: Cardinality.t ref,
					cons: {
					       con: Con.t,
					       args: Type.t vector
					       } vector ref,
					numCons: int ref,
					(* tycons whose cardinality depends on mine *)
					dependents: Tycon.t list ref,
					isOnWorklist: bool ref
					},
	   set = setTyconInfo} =
	 Property.getSetOnce
	 (Tycon.plist, Property.initRaise ("SimplifyTypes.info", Tycon.layout))

      local
	 fun make sel = (! o sel o tyconInfo,
			 fn (t, x) => sel (tyconInfo t) := x)
      in
	 val (tyconNumCons, setTyconNumCons) = make #numCons
	 val (tyconCardinality, _) = make #cardinality
      end
      val _ =
	 Vector.foreach
	 (datatypes, fn {tycon, cons} =>
	  setTyconInfo (tycon, {
				cardinality = ref Cardinality.Zero,
				numCons = ref 0,
				cons = ref cons,
				dependents = ref [],
				isOnWorklist = ref false
				}))
      (* Tentatively mark all constructors appearing in a ConApp as Useful
       * (some may later be marked as Transparent).
       *)
      val _ =
	 let
	    fun handleBind {var, ty, exp} =
	       case exp of
		  ConApp {con, ...} => setConRep (con, ConRep.Useful)
		| _ => ()
	    fun consUseful e = Exp.foreachBind (e, handleBind)
	    (* Booleans are special because they are generated by primitives. *)
	    val _ =
	       List.foreach ([Con.truee, Con.falsee], fn c =>
			     setConRep (c, ConRep.Useful))
	    val _ = Vector.foreach (globals, handleBind)
	    val _ = Vector.foreach (functions, fn Function.T {body, ...} =>
				    consUseful body)
	 in ()
	 end
      (* Remove useless constructors from datatypes.
       * Remove datatypes which have no cons.
       *)
      val datatypes =
	 Vector.keepAllMap
	 (datatypes, fn {tycon, cons} =>
	  let
	     val cons = Vector.keepAll (cons, conIsUseful o #con)
	  in
	     if 0 = Vector.length cons
		then (setTyconReplacement (tycon, Type.unit)
		      ; NONE)
	     else (#cons (tyconInfo tycon) := cons
		   ; SOME {tycon = tycon, cons = cons})
	  end)
      (* Build the dependents for each tycon. *)
      val _ =
	 let
	    val {get = isDatatype, set = setDatatype, destroy} =
	       Property.destGetSetOnce (Tycon.plist, Property.initConst false)
	    val _ =
	       Vector.foreach (datatypes, fn {tycon, ...} =>
			       setDatatype (tycon, true))
	    val _ =
	       Vector.foreach
	       (datatypes, fn {tycon, cons} =>
		let
		   val {get = isDependent, set = setDependent, destroy} =
		      Property.destGetSet (Tycon.plist, Property.initConst false)
		   fun setTypeDependents t =
		      let val (tycon', ts) = Type.tyconArgs t
		      in if isDatatype tycon'
			    then if isDependent tycon'
				    then ()
				 else (setDependent (tycon', true)
				       ; List.push (#dependents
						    (tyconInfo tycon'),
						    tycon))
			 else Vector.foreach (ts, setTypeDependents)
		      end
		   val _ =
		      Vector.foreach (cons, fn {args, ...} =>
				     Vector.foreach (args, setTypeDependents))
		   val _ = destroy ()
		in ()
		end)
	    val _ = destroy ()
	 in ()
	 end

      (* diagnostic *)
      (*       val _ =
       * 	 List.foreach
       * 	 (datatypes, fn {tycon, ...} =>
       * 	  Control.message
       * 	  (fn () =>
       * 	   let open Layout
       * 	   in seq [str "dependents of ",
       * 		  Tycon.layout tycon,
       * 		  str " = ",
       * 		  List.layout Tycon.layout (!(#dependents (tyconInfo tycon)))]
       * 	   end))
       *)
      local open Type Cardinality
      in
	 fun typeCardinality t =
	    case dest t of
	       Ref t => (case typeCardinality t of
			    Zero => Zero
			  | _ => Many)
	     | Tuple ts => tupleCardinality ts
	     | Datatype tycon => tyconCardinality tycon
	     | _ => Many
	 and tupleCardinality (ts: Type.t vector) =
	    DynamicWind.withEscape
	    (fn escape =>
	     (Vector.foreach (ts, fn t =>
			     let val c = typeCardinality t
			     in case c of
				Many => escape Many
			      | One => ()
			      | Zero => escape Zero
			     end)
	      ; One))
      end
      (*       val typeCardinality =
       * 	 Trace.trace ("typeCardinality", Type.layout, Cardinality.layout)
       * 	 typeCardinality
       *)
      fun conCardinality {con, args} = tupleCardinality args

      (*       val conCardinality =
       * 	 Trace.trace ("conCardinality", Con.layout o #con, Cardinality.layout)
       * 	 conCardinality
       *)
      (* Compute the tycon cardinalitues with a fixed point,
       * initially assuming every datatype tycon cardinality is Zero.
       *)
      val _ =
	 let
	    (* list of datatype tycons whose cardinality has not yet stabilized *)
	    val worklist =
	       ref (Vector.fold (datatypes, [], fn ({tycon, ...}, ac) =>
				tycon :: ac))
	    fun loop () =
	       case !worklist of
		  [] => ()
		| tycon :: tycons =>
		     (worklist := tycons
		      ; let
			   val {cons, cardinality, dependents, isOnWorklist,
				...} = tyconInfo tycon
			   val c =
			      DynamicWind.withEscape
			      (fn escape =>
			       let datatype z = datatype Cardinality.t
			       in Vector.fold
				  (!cons, Zero, fn (c, ac) =>
				   case conCardinality c of
				      Many => escape Many
				    | One => (case ac of
						 Many => Error.bug "Many"
					       | One => escape Many
					       | Zero => One)
				    | Zero => ac)
			       end)
			in isOnWorklist := false
			   ; if Cardinality.equals (c, !cardinality)
				then ()
			     else (cardinality := c
				   ; (List.foreach
				      (!dependents, fn tycon =>
				       let
					  val {isOnWorklist, ...} =
					     tyconInfo tycon
				       in if !isOnWorklist
					     then ()
					  else (isOnWorklist := true
						; List.push (worklist, tycon))
				       end)))
			end
		      ; loop ())
	 in loop ()
	 end
      (* diagnostic *)
      (*       val _ =
       * 	 List.foreach (datatypes, fn {tycon, ...} =>
       * 		      Control.message
       * 		      (fn () =>
       * 		       let open Layout
       * 		       in seq [str "cardinality of ",
       * 			      Tycon.layout tycon,
       * 			      str " = ",
       * 			     Cardinality.layout (tyconCardinality tycon)]
       * 		       end))
       *)
    
      (*       val setTyconReplacement =
       * 	 Trace.trace2 ("setTyconReplacement",
       * 		      Tycon.layout, Type.layout, Layout.ignore)
       * 	 setTyconReplacement
       *)
      fun transparent (tycon, con, args) =
	 (setTyconReplacement (tycon, Type.tuple args)
	  ; setConRep (con, ConRep.Transparent)
	  ; setTyconNumCons (tycon, 1))
      (*       val transparent =
       * 	 Trace.trace3
       * 	 ("transparent",
       * 	  Tycon.layout, Con.layout, List.layout Type.layout, Unit.layout)
       * 	 transparent
       *)
      (* "unary" is datatypes with one constructor whose rhs contains an
       * array (or vector) type.
       * For datatypes with one variant not containing an array type, eliminate
       * the datatype. 
       *)
      val (datatypes, unary) =
	 Vector.fold
	 (datatypes, ([], []), fn ({tycon, cons}, (datatypes, unary)) =>
	  let
	     (* remove all cons with zero cardinality and mark them as useless *)
	     val cons =
		Vector.keepAllMap
		(cons, fn c as {con, ...} =>
		 case conCardinality c of
		    Cardinality.Zero => (setConRep (con, ConRep.Useless)
					 ; NONE)
		  | _ => SOME c)
	  in case Vector.length cons of
	     0 => (setTyconNumCons (tycon, 0)
		    ; setTyconReplacement (tycon, Type.unit)
		    ; (datatypes, unary))
	   | 1 =>
		let
		   val {con, args} = Vector.sub (cons, 0)
		in
		   if Vector.exists (args, fn t =>
				    Type.containsTycon (t, Tycon.array)
				    orelse Type.containsTycon (t, Tycon.vector))
		      then (datatypes,
			    {tycon = tycon, con = con, args = args}
			    :: unary)
		   else (transparent (tycon, con, args)
			 ; (datatypes, unary))
		end
	   | _ => ({tycon = tycon, cons = cons} :: datatypes,
		   unary)
	  end)
      fun containsTycon (ty: Type.t, tyc: Tycon.t): bool =
	 let open Type
	    fun loop t =
	       case dest t of
		  Tuple ts => Vector.exists (ts, loop)
		| Array t => loop t
		| Vector t => loop t
		| Ref t => loop t
		| Datatype tyc' =>
		     (case tyconReplacement tyc' of
			 NONE => Tycon.equals (tyc, tyc')
		       | SOME t => loop t)
		| _ => false
	 in loop ty
	 end
      (* Keep the circular transparent cons, ditch the rest. *)
      val datatypes =
	 List.fold
	 (unary, datatypes, fn ({tycon, con, args}, accum) =>
	  if Vector.exists (args, fn arg => containsTycon (arg, tycon))
	     then {tycon = tycon,
		   cons = Vector.new1 {con = con, args = args}} :: accum
	  else (transparent (tycon, con, args)
		; accum))
      fun makeKeepSimplifyTypes simplifyType ts =
	 Vector.keepAllMap (ts, fn t =>
			    let
			       val t = simplifyType t
			    in
			       if Type.isUnit t
				  then NONE
			       else SOME t
			    end)
      val {get = simplifyType, destroy = destroySimplifyType} =
	 Property.destGet
	 (Type.plist,
	  Property.initRec
	  (fn (t, simplifyType) =>
	   let
	      val keepSimplifyTypes = makeKeepSimplifyTypes simplifyType
	      open Type
	   in case dest t of
	      Tuple ts => Type.tuple (keepSimplifyTypes ts)
	    | Array t => array (simplifyType t)
	    | Vector t => vector (simplifyType t)
	    | Ref t => reff (simplifyType t)
	    | Datatype tycon => 
		 (case tyconReplacement tycon of
		     SOME t =>
			let
			   val t = simplifyType t
			   val _ = setTyconReplacement (tycon, t)
			in
			   t
			end
		   | NONE => t)
	    | _ => t
	   end))
      val simplifyType =
 	 Trace.trace ("simplifyType", Type.layout, Type.layout)
 	 simplifyType
      fun simplifyTypes ts = Vector.map (ts, simplifyType)
      val keepSimplifyTypes = makeKeepSimplifyTypes simplifyType
      (* Simplify constructor argument types. *)
      val datatypes =
	 Vector.fromListMap
	 (datatypes, fn {tycon, cons} =>
	  (setTyconNumCons (tycon, Vector.length cons)
	   ; {tycon = tycon,
	      cons = Vector.map (cons, fn {con, args} =>
				{con = con,
				 args = keepSimplifyTypes args})}))
      val unitVar = Var.newNoname ()
      val {get = varInfo: Var.t -> Type.t, set = setVarInfo} =
	 Property.getSetOnce (Var.plist,
			      Property.initRaise ("varInfo", Var.layout))
      fun simplifyVarType (x: Var.t, t: Type.t): Type.t =
	 (setVarInfo (x, t)
	  ; simplifyType t)
      val oldVarType = varInfo
      val newVarType = simplifyType o oldVarType
      fun simplifyVar (x: Var.t): Var.t =
	 if Type.isUnit (newVarType x)
	    then unitVar
	 else x
      fun simplifyVars xs = List.map (xs, simplifyVar)
      val varIsUseless = Type.isUnit o newVarType
      fun removeUselessVars xs = Vector.keepAll (xs, not o varIsUseless)
      fun tuple xs =
	 let
	    val xs = removeUselessVars xs
	 in if 1 = Vector.length xs
	       then Var (Vector.sub (xs, 0))
	    else Tuple xs
	 end
      fun simplifyFormals xts =
	 Vector.keepAllMap
	 (xts, fn (x, t) =>
	  let val t = simplifyVarType (x, t)
	  in if Type.isUnit t
		then NONE
	     else SOME (x, t)
	  end)
      val typeIsUseful = not o Type.isUnit o simplifyType
      datatype result = datatype Result.t
      fun simplifyPrimExp (e: PrimExp.t): PrimExp.t result =
	 case e of
	    Const _ => Keep e
	  | Var _ => Keep e
	  | Tuple xs => Keep (tuple xs)
	  | Select {tuple, offset} =>
	       let
		  val ts = Type.detuple (oldVarType tuple)
	       in Vector.fold'
		  (ts, 0, (offset, 0), fn (pos, t, (n, offset)) =>
		   if n = 0
		      then (Vector.Done
			    (Keep
			     (if offset = 0
				 andalso not (Vector.existsR
					      (ts, pos + 1, Vector.length ts,
					       typeIsUseful))
				 then Var tuple
			      else Select {tuple = tuple,
					   offset = offset})))
		   else Vector.Continue (n - 1,
					 if typeIsUseful t
					    then offset + 1
					 else offset),
		      fn _ => Error.bug "newOffset")
	       end
	  | ConApp {con, args} =>
	       (case conRep con of
		   ConRep.Transparent => Keep (tuple args)
		 | ConRep.Useful =>
		      Keep (ConApp {con = con,
				    args = removeUselessVars args})
		 | ConRep.Useless => Bugg)
	  | PrimApp {prim, info, targs, args} =>
	       Keep
	       (let 
		   fun normal () =
		      PrimApp {prim = prim, info = info,
			       targs = simplifyTypes targs,
			       args = Vector.map (args, simplifyVar)}
		   fun equal () =
		      if 2 = Vector.length args
			 then
			    if varIsUseless (Vector.sub (args, 0))
			       then ConApp {con = Con.truee,
					    args = Vector.new0 ()}
			    else normal ()
		      else Error.bug "strange eq/equal PrimApp"
		   open Prim.Name
		in case Prim.name prim of
		   MLton_eq => equal ()
		 | MLton_equal => equal ()
		 | _ => normal ()
		end)
      val simplifyPrimExp =
	 Trace.trace ("SimplifyTypes.simplifyPrimExp",
		      PrimExp.layout, Result.layout PrimExp.layout)
	 simplifyPrimExp
      fun simplifyTransfer (t : Transfer.t): Dec.t list * Transfer.t =
	 case t of
	    Bug => ([], Bug)
	  | Call {func, args, cont} =>
	       ([],
		Call {func = func, cont = cont, args = removeUselessVars args})
	  | Case {cause, test, cases = Cases.Con cases, default} =>
	       let
		  val cases =
		     Vector.keepAll (cases, fn (con, _) =>
				     not (ConRep.isUseless (conRep con)))
		  val default =
		     case (Vector.length cases, default) of
			(_,     NONE)    => NONE
		      | (0,     SOME j)  => SOME j
		      | (n,     SOME j)  =>
			   if n = tyconNumCons (Type.tycon (oldVarType test))
			      then NONE
			   else SOME j
		  fun normal () =
		     ([],
		      Case {cause = cause, test = test,
			    cases = Cases.Con cases,
			    default = default})
	       in case (Vector.length cases, default) of
		  (0,         NONE)   => ([], Bug)
		| (0,         SOME j) =>
		     ([], Jump {dst = j, args = Vector.new0 ()})
		| (1, NONE)   =>
		     let
			val (con, j) = Vector.sub (cases, 0)
		     in
			if ConRep.isUseful (conRep con)
			   then
			      (* This case can occur because an array or vector
			       * tycon was kept around.
			       *)
			      normal () 
			else (* The type has become a tuple.  Do the selects. *)
			   let
			      val ts = keepSimplifyTypes (conArgs con)
			      val (args, decs) =
				 if 1 = Vector.length ts
				    then (Vector.new1 test, Vector.new0 ())
				 else
				    Vector.unzip
				    (Vector.mapi
				     (ts, fn (i, t) =>
				      let val x = Var.newNoname ()
				      in (x,
					  Bind {var = x, ty = t,
						exp = Select {tuple = test,
							      offset = i}})
				      end))
			   in (Vector.toList decs, Jump {dst = j, args = args})
			   end
		     end
		| _ => normal ()
	       end
	  | Case r => ([], Case r)
	  | Jump {dst, args} =>
	       ([], Jump {dst = dst, args = removeUselessVars args})
	  | Raise xs => ([], Raise (removeUselessVars xs))
	  | Return xs => ([], Return (removeUselessVars xs))
      val simplifyTransfer =
	 Trace.trace
	 ("SimplifyTypes.simplifyTransfer", Transfer.layout,
	  Layout.tuple2 (List.layout Dec.layout, Transfer.layout))
	 simplifyTransfer
      fun simplifyBind {var, ty, exp} =
	 let val ty = simplifyVarType (var, ty)
	    fun normal () =
	       case simplifyPrimExp exp of
		  Bugg => Bugg
		| Delete => Delete
		| Keep e => Keep {var = var, ty = ty, exp = e}
	 in if Type.isUnit ty
	       then (case exp of
			PrimApp {prim, ...} =>
			   if Prim.maySideEffect prim
			      then normal ()
			   else Delete
		      | _ => Delete)
	    else normal ()
	 end
      (*       val traceSimplifyExp =
       * 	 Trace.trace ("simplifyExp", Exp.layout, Exp.layout)
       *       val traceSimplifyDec =
       * 	 Trace.trace ("simplifyDec", Dec.layout, Result.layout Dec.layout)
       *)
      fun simplifyExp arg =
	 (*	 traceSimplifyExp *)
	 (fn e =>
	  let val {decs, transfer} = Exp.dest e
	     fun loop (decs, accum) =
		let
		   fun done (ds, t) =
		      Exp.make {decs = List.appendRev (accum, ds),
				transfer = t}
		in case decs of
		   [] => done (simplifyTransfer transfer)
		 | d :: decs =>
		      case simplifyDec d of
			 Bugg => done ([], Bug)
		       | Delete => loop (decs, accum)
		       | Keep d => loop (decs, d :: accum)
		end
	  in loop (decs, [])
	  end) arg
      and simplifyDec arg: Dec.t result =
	 (*	 traceSimplifyDec *)
	 (fn (d: Dec.t) =>
	  case d of
	     Bind b => (case simplifyBind b of
			   Bugg => Bugg
			 | Delete => Delete
			 | Keep b => Keep (Bind b))
	   | Fun {name, args, body} =>
		Keep (Fun {name = name,
			   args = simplifyFormals args,
			   body = simplifyExp body})
	   | _ => Keep d) arg
      val globals =
	 Vector.concat
	 [Vector.new1 {var = unitVar,
		      ty = Type.unit,
		      exp = PrimExp.unit},
	  Vector.keepAllMap (globals, fn b =>
			    case simplifyBind b of
			       Bugg => Error.bug "global bind can't fail"
			     | Delete => NONE
			     | Keep b => SOME b)]
      val shrinkExp = shrinkExp globals
      val shrinkExp =
	 Trace.trace ("SimplifyTypes.shrinkExp", Exp.layout, Exp.layout)
	 shrinkExp
      val functions =
	 Vector.map
	 (functions, fn Function.T {name, args, body, returns} =>
	  Function.T {name = name,
		      args = simplifyFormals args,
		      body = shrinkExp (simplifyExp body),
		      returns = keepSimplifyTypes returns})
      val program =
	 Program.T {datatypes = datatypes,
		    globals = globals,
		    functions = functions,
		    main = main}
      val _ = destroySimplifyType ()
      val _ = Program.clear program
   in
      program
   end

end
