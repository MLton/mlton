(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor ElaborateCore (S: ELABORATE_CORE_STRUCTS): ELABORATE_CORE = 
struct

open S

local
   open Ast
in
   structure Acon = Con
   structure Aconst = Const
   structure Adec = Dec
   structure Aexp = Exp
   structure Amatch = Match
   structure Apat = Pat
   structure Atype = Type
   structure Avar = Var
   structure Avid = Vid
   structure DatatypeRhs = DatatypeRhs
   structure DatBind = DatBind
   structure EbRhs = EbRhs
   structure Fixop = Fixop
   structure Longvid = Longvid
   structure Longtycon = Longtycon
   structure PrimKind = PrimKind
   structure Attribute = PrimKind.Attribute
   structure Priority = Priority
   structure Record = Record
   structure SortedRecord = SortedRecord
   structure Symbol = Symbol
   structure TypBind = TypBind
end

local
   open Env
in
   structure TypeEnv = TypeEnv
   structure TypeStr = TypeStr
   structure Vid = Vid
end

local
   open TypeStr
in
   structure Kind = Kind
end

local
   open TypeEnv
in
   structure Scheme = Scheme
   structure Type = Type
end

local
   open CoreML
in
   structure CFunction = CFunction
   structure Convention	 = CFunction.Convention	
   structure Con = Con
   structure Const = Const
   structure Cdec = Dec
   structure Cexp = Exp
   structure Ffi = Ffi
   structure IntSize = IntSize
   structure IntX = IntX
   structure Lambda = Lambda
   structure Cpat = Pat
   structure Prim = Prim
   structure RealSize = RealSize
   structure RealX = RealX
   structure SourceInfo = SourceInfo
   structure Tycon = Tycon
   structure Tyvar = Tyvar
   structure Var = Var
   structure WordSize = WordSize
   structure WordX = WordX
end

structure AdmitsEquality = Tycon.AdmitsEquality

local
   open Record
in
   structure Field = Field
end

structure Parse = PrecedenceParse (structure Ast = Ast
				   structure Env = Env)

structure Scope = Scope (structure Ast = Ast)

structure Apat =
   struct
      open Apat

      fun getName (p: t): string option =
	 case node p of
	    Var {name, ...} => SOME (Longvid.toString name)
	  | Constraint (p, _) => getName p
	  | FlatApp v =>
	       if 1 = Vector.length v
		  then getName (Vector.sub (v, 0))
	       else NONE
	  | Layered {var, ...} => SOME (Avar.toString var)
	  | _ => NONE

      val getName =
	 Trace.trace ("Apat.getName", layout, Option.layout String.layout)
	 getName
   end

structure Lookup =
   struct
      type t = Longtycon.t -> TypeStr.t

      fun fromEnv (E: Env.t) longtycon = Env.lookupLongtycon (E, longtycon)
   end

fun elaborateType (ty: Atype.t, lookup: Lookup.t): Type.t =
   let 
      fun loop (ty: Atype.t): Type.t =
	 case Atype.node ty of
	    Atype.Var a => (* rule 44 *)
	       Type.var a
	  | Atype.Con (c, ts) => (* rules 46, 47 *)
	       let
		  val ts = Vector.map (ts, loop)
		  fun normal () =
		     let
			val s = lookup c
			val kind = TypeStr.kind s
			val numArgs = Vector.length ts
		     in
			if (case kind of
			       Kind.Arity n => n = numArgs
			     | Kind.Nary => true)
			   then TypeStr.apply (s, ts)
			else
			   let
			      open Layout
			      val _ = 
				 Control.error
				 (Atype.region ty,
				  seq [str "type constructor ",
				       Ast.Longtycon.layout c,
				       str " given ",
				       Int.layout numArgs,
				       str (if numArgs = 1
					       then " argument"
					       else " arguments"),
				       str " but wants ",
				       Kind.layout kind],
				  empty)
			   in
			      Type.new ()
			   end
		     end
	       in
		  case (Ast.Longtycon.split c, Vector.length ts) of
		     (([], c), 2) =>
			if Ast.Tycon.equals (c, Ast.Tycon.arrow)
			   then Type.arrow (Vector.sub (ts, 0),
					    Vector.sub (ts, 1))
			else normal ()
		   | _ => normal ()
	       end
	  | Atype.Record r => (* rules 45, 49 *)
	       Type.record (SortedRecord.map (r, loop))
   in
      loop ty
   end

val overloads: (Ast.Priority.t * (unit -> unit)) list ref = ref []

val freeTyvarChecks: (unit -> unit) list ref = ref []

val {hom = typeTycon: Type.t -> Tycon.t option, ...} =
   Type.makeHom {con = fn (c, _) => SOME c,
		 expandOpaque = false,
		 var = fn _ => NONE}

fun 'a elabConst (c: Aconst.t,
		  make: (unit -> Const.t) * Type.t -> 'a,
		  {false = f: 'a, true = t: 'a}): 'a =
   let
      fun error (ty: Type.t): unit =
	 let
	    open Layout
	 in
	    Control.error
	    (Aconst.region c,
	     seq [Type.layoutPretty ty, str " too big: ", Aconst.layout c],
	     empty)
	 end
      fun choose (tycon, all, sizeTycon, make) =
	 case List.peek (all, fn s => Tycon.equals (tycon, sizeTycon s)) of
	    NONE => Const.string "<bogus>"
	  | SOME s => make s
      fun now (c: Const.t, ty: Type.t): 'a = make (fn () => c, ty)
      fun delay (ty: Type.t, resolve: Tycon.t -> Const.t): 'a =
	 let
	    val resolve =
	       Promise.lazy
	       (fn () =>
		let
		   val tycon =
		      case typeTycon ty of
			 NONE => Tycon.bogus
		       | SOME c => c
		in
		   resolve tycon
		end)
	    val _ = List.push (overloads, (Priority.default, ignore o resolve))
	 in
	    make (resolve, ty)
	 end
   in
      case Aconst.node c of
	 Aconst.Bool b => if b then t else f
       | Aconst.Char c =>
	    now (Const.Word (WordX.make (LargeWord.fromChar c, WordSize.W8)),
		 Type.char)
       | Aconst.Int i =>
	    let
	       val ty = Type.unresolvedInt ()
	    in
	       delay
	       (ty, fn tycon =>
		if Tycon.equals (tycon, Tycon.intInf)
		   then Const.IntInf i
		else
		   choose (tycon, IntSize.all, Tycon.int, fn s =>
			   Const.Int
			   (IntX.make (i, s)
			    handle Overflow => (error ty; IntX.zero s))))
	    end
       | Aconst.Real r =>
	    let
	       val ty = Type.unresolvedReal ()
	    in
	       delay
	       (ty, fn tycon =>
		choose (tycon, RealSize.all, Tycon.real, fn s =>
			Const.Real (case RealX.make (r, s) of
				       NONE => (error ty; RealX.zero s)
				     | SOME r => r)))
	    end
       | Aconst.String s => now (Const.string s, Type.string)
       | Aconst.Word w =>
	    let
	       val ty = Type.unresolvedWord ()
	    in
	       delay
	       (ty, fn tycon =>
		choose (tycon, WordSize.all, Tycon.word, fn s =>
			Const.Word
			(if w <= LargeWord.toIntInf (WordSize.max s)
			    then WordX.fromLargeInt (w, s)
			 else (error ty
			       ; WordX.zero s))))
	    end	       
   end

local
   open Layout
in
   val align = align
   val empty = empty
   val seq = seq
   val str = str
end

val unify =
   fn (t, t', preError, error) =>
   Type.unify (t, t', {error = Control.error o error,
		       preError = preError})
   
fun unifyList (trs: (Type.t * Region.t) vector,
	       z,
	       lay: unit -> Layout.t): Type.t =
   if 0 = Vector.length trs
      then Type.list (Type.new ())
   else
      let
	 val (t, _) = Vector.sub (trs, 0)
	 val _ =
	    Vector.foreach
	    (trs, fn (t', r) =>
	     unify (t, t', z, fn (l, l') =>
		    (r,
		     str "list element types disagree",
		     align [seq [str "element:  ", l'],
			    seq [str "previous: ", l],
			    lay ()])))
      in
	 Type.list t
      end

val info = Trace.info "elaboratePat"

structure Var =
   struct
      open Var

      val fromAst = fromString o Avar.toString
   end

val allowRebindEquals = ref true
   
local
   val eq = Avar.fromSymbol (Symbol.equal, Region.bogus)
in
   fun ensureNotEquals x =
      if not (!allowRebindEquals) andalso Avar.equals (x, eq)
	 then
	    let
	       open Layout
	    in
	       Control.error (Avar.region x, str "= can't be redefined", empty)
	    end
      else ()
end

fun approximate (l: Layout.t): Layout.t =
   let
      val s = Layout.toString l
      val n = String.size s
   in
      Layout.str
      (if n <= 60
	  then s
       else concat [String.prefix (s, 35), "  ...  ", String.suffix (s, 25)])
   end

val elaboratePat:
   unit
   -> Apat.t * Env.t * {bind: bool} * (unit -> unit)
   -> Cpat.t * (Avar.t * Var.t * Type.t) vector =
   fn () =>
   let
      val others: (Apat.t * (Avar.t * Var.t * Type.t) vector) list ref = ref []
   in
      fn (p: Apat.t, E: Env.t, {bind}, preError: unit -> unit) =>
      let
	 val xts: (Avar.t * Var.t * Type.t) list ref = ref []
	 fun bindToType (x: Avar.t, t: Type.t): Var.t =
	    let
	       val _ = ensureNotEquals x
	       val x' = Var.fromAst x
	       val _ =
		  if List.exists (!xts, fn (x', _, _) => Avar.equals (x, x'))
		     then
			let
			   open Layout
			in
			   Control.error (Avar.region x,
					  seq [str "variable ",
					       Avar.layout x,
					       str " occurs more than once in pattern"],
					  seq [str "pattern: ",
					       approximate (Apat.layout p)])
			end
		  else ()
	       val _ =
		  case (List.peekMap
			(!others, fn (p, v) =>
			 if Vector.exists (v, fn (x', _, _) =>
					   Avar.equals (x, x'))
			    then SOME p
			 else NONE)) of
		     NONE => ()
		   | SOME p' => 
			let
			   open Layout
			in
			   Control.error
			   (Apat.region p,
			    seq [str "variable ",
				 Avar.layout x,
				 str " occurs in multiple patterns"],
			    align [seq [str "pattern: ",
					approximate (Apat.layout p)],
				   seq [str "pattern: ",
					approximate (Apat.layout p')]])

			end
	       val _ = List.push (xts, (x, x', t))
	       val _ =
		  if bind
		     then Env.extendVar (E, x, x', Scheme.fromType t,
					 {isRebind = false})
		  else ()
	    in
	       x'
	    end
	 fun bind (x: Avar.t): Var.t * Type.t =
	    let
	       val t = Type.new ()
	    in
	       (bindToType (x, t), t)
	    end
	 fun loop arg: Cpat.t =
	    Trace.traceInfo' (info, Apat.layout, Cpat.layout)
	    (fn p: Apat.t =>
	     let
		val region = Apat.region p
		val unify = fn (t, t', f) => unify (t, t', preError, f)
		fun unifyPatternConstraint (p, lay, c) =
		   unify
		   (p, c, fn (l1, l2) =>
		    (region,
		     str "pattern and constraint disagree",
		     align [seq [str "expects: ", l2],
			    seq [str "but got: ", l1],
			    seq [str "in: ", lay ()]]))
		fun lay () = approximate (Apat.layout p)
	     in
		case Apat.node p of
		   Apat.App (c, p) =>
		      let
			 val (con, s) = Env.lookupLongcon (E, c)
			 val {args, instance} = Scheme.instantiate s
			 val args = args ()
			 val p = loop p
			 val argType = Type.new ()
			 val resultType = Type.new ()
			 val _ =
			    unify
			    (instance, Type.arrow (argType, resultType),
			     fn _ =>
			     (region,
			      str "constant constructor applied to argument",
			      seq [str "pattern: ", lay ()]))
			 val _ =
			    unify
			    (Cpat.ty p, argType, fn (l, l') =>
			     (region,
			      str "constructor applied to incorrect argument",
			      align [seq [str "expects: ", l'],
				     seq [str "but got: ", l],
				     seq [str "pattern: ", lay ()]]))
		      in
			 Cpat.make (Cpat.Con {arg = SOME p,
					      con = con,
					      targs = args},
				    resultType)
		      end
		 | Apat.Const c =>
		      elabConst
		      (c,
		       fn (resolve, ty) => Cpat.make (Cpat.Const resolve, ty),
		       {false = Cpat.falsee,
			true = Cpat.truee})
		 | Apat.Constraint (p, t) =>
		      let
			 val p' = loop p
			 val _ =
			    unifyPatternConstraint
			    (Cpat.ty p', fn () => Apat.layout p,
			     elaborateType (t, Lookup.fromEnv E))
		      in
			 p'
		      end
		 | Apat.FlatApp items =>
		      loop (Parse.parsePat
			    (items, E, fn () => seq [str "pattern: ", lay ()]))
		 | Apat.Layered {var = x, constraint, pat, ...} =>
		      let
			 val t =
			    case constraint of
			       NONE => Type.new ()
			     | SOME t => elaborateType (t, Lookup.fromEnv E)
			 val x = bindToType (x, t)
			 val pat' = loop pat
			 val _ =
			    unifyPatternConstraint (Cpat.ty pat',
						    fn () => Apat.layout pat,
						    t)
		      in
			 Cpat.make (Cpat.Layered (x, pat'), t)
		      end
		 | Apat.List ps =>
		      let
			 val ps' = Vector.map (ps, loop)
		      in
			 Cpat.make (Cpat.List ps',
				    unifyList
				    (Vector.map2 (ps, ps', fn (p, p') =>
						  (Cpat.ty p', Apat.region p)),
				     preError,
				     fn () => seq [str "pattern:  ", lay ()]))
		      end
		 | Apat.Record {flexible, items} =>
		      (* rules 36, 38, 39 and Appendix A, p.57 *)
		      let
			 val (fs, ps) =
			    Vector.unzip
			    (Vector.map
			     (items,
			      fn (f, i) =>
			      (f,
			       case i of
				  Apat.Item.Field p => p
				| Apat.Item.Vid (vid, tyo, po) =>
				     let
					val p =
					   case po of
					      NONE =>
						 Apat.longvid (Longvid.short vid)
					    | SOME p =>
						 Apat.layered
						 {fixop = Fixop.None,
						  var = Ast.Vid.toVar vid,
						  constraint = NONE,
						  pat = p}
				     in
					case tyo of
					   NONE => p
					 | SOME ty => Apat.constraint (p, ty)
				     end)))
			 val ps = Vector.map (ps, loop)
			 val r = SortedRecord.zip (fs, Vector.map (ps, Cpat.ty))
			 val ty =
			    if flexible
			       then
				  let
				     val (t, isResolved) = Type.flexRecord r
				     fun resolve () =
					if isResolved ()
					   then ()
					else
					   Control.error
					   (region,
					    str "unresolved ... in record pattern",
					    seq [str "pattern: ", lay ()])
				     val _ = List.push (overloads, (Priority.default, resolve))
				  in
				     t
				  end
			    else
			       Type.record r
		      in
			 Cpat.make
			 (Cpat.Record (Record.fromVector (Vector.zip (fs, ps))),
			  ty)
		      end
		 | Apat.Tuple ps =>
		      let
			 val ps = Vector.map (ps, loop)
		      in
			 Cpat.make (Cpat.Tuple ps,
				    Type.tuple (Vector.map (ps, Cpat.ty)))
		      end
		 | Apat.Var {name, ...} =>
		      let
			 val (strids, x) = Ast.Longvid.split name
			 fun var () =
			    let
			       val (x, t) = bind (Ast.Vid.toVar x)
			    in
			       Cpat.make (Cpat.Var x, t)
			    end
		      in
			 case Env.peekLongcon (E, Ast.Longvid.toLongcon name) of
			    NONE =>
			       if List.isEmpty strids
				  then var ()
			       else
				  let
				     val _ = 
					Control.error
					(region,
					 seq [str "undefined constructor: ",
					      Ast.Longvid.layout name],
					 empty)
				  in
				     Cpat.make (Cpat.Wild, Type.new ())
				  end
			  | SOME (c, s) =>
			       let
				  val {args, instance} = Scheme.instantiate s
			       in
				  Cpat.make
				  (Cpat.Con {arg = NONE, con = c, targs = args ()},
				   instance)
			       end
		      end
		 | Apat.Wild =>
		      Cpat.make (Cpat.Wild, Type.new ())
	     end) arg
	 val p' = loop p
	 val xts = Vector.fromList (!xts)
	 val _ = List.push (others, (p, xts))
      in
	 (p', xts)
      end
   end

(*---------------------------------------------------*)
(*                   Declarations                    *)
(*---------------------------------------------------*)

structure Nest =
   struct
      type t = string list

      val layout = List.layout String.layout
   end

val info = Trace.info "elaborateDec"
val elabExpInfo = Trace.info "elaborateExp"

structure CType =
   struct
      open CoreML.CType

      fun sized (all: 'a list,
		 toString: 'a -> string,
		 prefix: string,
		 make: 'a -> t,
		 makeType: 'a -> 'b) =
	 List.map (all, fn a =>
		   (make a, concat [prefix, toString a], makeType a))

      val nullary: (t * string * Tycon.t) list =
	 [(bool, "Bool", Tycon.bool),
	  (char, "Char", Tycon.char),
	  (pointer, "Pointer", Tycon.pointer),
	  (pointer, "Pointer", Tycon.preThread),
	  (pointer, "Pointer", Tycon.thread)]
	 @ sized (IntSize.all, IntSize.toString, "Int", Int, Tycon.int)
	 @ sized (RealSize.all, RealSize.toString, "Real", Real, Tycon.real)
	 @ sized (WordSize.all, WordSize.toString, "Word", Word, Tycon.word)

      val unary: Tycon.t list =
	 [Tycon.array, Tycon.reff, Tycon.vector]

      fun fromType (t: Type.t): (t * string) option =
	 case Type.deConOpt t of
	    NONE => NONE
	  | SOME (c, ts) =>
	       case List.peek (nullary, fn (_, _, c') => Tycon.equals (c, c')) of
		  NONE =>
		     if List.exists (unary, fn c' => Tycon.equals (c, c'))
			andalso 1 = Vector.length ts
			andalso isSome (fromType (Vector.sub (ts, 0)))
			then SOME (Pointer, "Pointer")
		     else NONE
		| SOME (t, s, _) => SOME (t, s)

      val fromType =
	 Trace.trace ("Ctype.fromType",
		      Type.layoutPretty,
		      Option.layout (Layout.tuple2 (layout, String.layout)))
	 fromType

      fun parse (ty: Type.t)
	 : ((t * string) vector * (t * string) option) option =
	 case Type.deArrowOpt ty of
	    NONE => NONE
	  | SOME (t1, t2) =>
	       let
		  fun finish (ts: (t * string) vector) =
		     case fromType t2 of
			NONE =>
			   if Type.isUnit t2
			      then SOME (ts, NONE)
			   else NONE
		      | SOME t => SOME (ts, SOME t)
	       in
		  case Type.deTupleOpt t1 of 
		     NONE =>
			(case fromType t1 of
			    NONE => NONE
			  | SOME u => finish (Vector.new1 u))
		   | SOME ts =>
			let
			   val us = Vector.map (ts, fromType)
			in
			   if Vector.forall (us, isSome)
			      then finish (Vector.map (us, valOf))
			   else NONE
			end
	       end
   end

fun parseAttributes (attributes: Attribute.t list): Convention.t option =
   case attributes of
      [] => SOME Convention.Cdecl
    | [a] =>
	 SOME (case a of
		  Attribute.Cdecl => Convention.Cdecl
		| Attribute.Stdcall =>
		     if !Control.targetOS = MLton.Platform.OS.Cygwin
			then Convention.Stdcall
		     else Convention.Cdecl)
    | _ => NONE

fun import {attributes: Attribute.t list,
	    name: string,
	    ty: Type.t,
	    region: Region.t}: Prim.t =
   let
      fun error l = Control.error (region, l, Layout.empty)
      fun invalidAttributes () =
	 error (seq [str "invalid attributes for import: ",
		     List.layout Attribute.layout attributes])
   in
      case CType.parse ty of
	 NONE =>
	    (case CType.fromType ty of
		NONE => 
		   let
		      val _ =
			 Control.error
			 (region,
			  str "invalid type for import: ",
			  Type.layoutPretty ty)
		   in
		      Prim.bogus
		   end
	      | SOME (t, _) =>
		   case attributes of
		      [] => Prim.ffiSymbol {name = name, ty = t}
		    | _ => 
			 let
			    val _ = invalidAttributes ()
			 in
			    Prim.bogus
			 end)
       | SOME (args, result) =>
	    let
	       val convention =
		  case parseAttributes attributes of
		     NONE => (invalidAttributes ()
			      ; Convention.Cdecl)
		   | SOME c => c
	       val func =
		  CFunction.T {args = Vector.map (args, #1),
			       bytesNeeded = NONE,
			       convention = convention,
			       ensuresBytesFree = false,
			       modifiesFrontier = true,
			       modifiesStackTop = true,
			       mayGC = true,
			       maySwitchThreads = false,
			       name = name,
			       return = Option.map (result, #1)}
	    in
	       Prim.ffi func
	    end
   end

fun export {attributes, name: string, region: Region.t, ty: Type.t}: Aexp.t =
   let
      fun error l = Control.error (region, l, Layout.empty)
      fun invalidAttributes () =
	 error (seq [str "invalid attributes for export: ",
		     List.layout Attribute.layout attributes])
      val convention =
	 case parseAttributes attributes of
	    NONE => (invalidAttributes ()
		     ; Convention.Cdecl)
	  | SOME c => c
      val (exportId, args, res) =
	 case CType.parse ty of
	    NONE =>
	       (Control.error
		(region,
		 seq [str "invalid type for exported function: ",
		      Type.layoutPretty ty],
		 Layout.empty)
		; (0, Vector.new0 (), NONE))
	  | SOME (us, t) =>
	       let
		  val id = Ffi.addExport {args = Vector.map (us, #1),
					  convention = convention,
					  name = name,
					  res = Option.map (t, #1)}
	       in
		  (id, us, t)
	       end
      open Ast
      fun id (name: string) =
	 Aexp.longvid (Longvid.short
		       (Vid.fromSymbol (Symbol.fromString name,
					region)))
      fun int (i: int): Aexp.t =
	 Aexp.const (Aconst.makeRegion (Aconst.Int (IntInf.fromInt i), region))
      val f = Var.fromSymbol (Symbol.fromString "f", region)
   in
      Exp.fnn
      (Vector.new1
       (Pat.var f,
	Exp.app
	(id "register",
	 Exp.tuple
	 (Vector.new2
	  (int exportId,
	   Exp.fnn
	   (Vector.new1
	    (Pat.tuple (Vector.new0 ()),
	     let
		val map = CType.memo (fn _ => Counter.new 0)
		val varCounter = Counter.new 0
		val (args, decs) =
		   Vector.unzip
		   (Vector.map
		    (args, fn (u, name) =>
		     let
			val x =
			   Var.fromSymbol
			   (Symbol.fromString
			    (concat ["x",
				     Int.toString (Counter.next varCounter)]),
			    region)
			val dec =
			   Dec.vall (Vector.new0 (),
				     x,
				     Exp.app (id (concat ["get", name]),
					      int (Counter.next (map u))))
		     in
			(x, dec)
		     end))
		val resVar = Var.fromSymbol (Symbol.fromString "res", region)
		fun newVar () = Var.fromSymbol (Symbol.fromString "none", region)
	     in
		Exp.lett
		(Vector.concat
		 [decs,
		  Vector.map 
		  (Vector.new4
		   ((newVar (), Exp.app (id "atomicEnd", Exp.unit)),
		    (resVar, Exp.app (Exp.var f,
				      Exp.tuple (Vector.map (args, Exp.var)))),
		    (newVar (), Exp.app (id "atomicBegin", Exp.unit)),
		    (newVar (),
		     (case res of
			 NONE => Exp.constraint (Exp.var resVar, Type.unit)
		       | SOME (_, name) => 
			    Exp.app (id (concat ["set", name]),
				     Exp.var resVar)))),
		   fn (x, e) => Dec.vall (Vector.new0 (), x, e))],
		 Exp.tuple (Vector.new0 ()))
	     end)))))))
   end

val export =
   Trace.trace
   ("export",
    fn {name, ...} => String.layout name,
    Aexp.layout)
   export

structure Aexp =
   struct
      open Aexp

      local
	 val x = Symbol.fromString "x"
      in
	 fun selector (f: Field.t, r: Region.t): t =
	    let
	       val x = Avar.fromSymbol (x, r)
	    in
	       fnn (Vector.new1
		    (Apat.makeRegion
		     (Apat.Record {flexible = true,
				   items = (Vector.new1
					    (f, Apat.Item.Field (Apat.var x)))},
		      r),
		     var x))
	    end
      end
   end

structure Con =
   struct
      open Con

      val fromAst = fromString o Ast.Con.toString
   end

fun elaborateDec (d, {env = E,
		      lookupConstant: string * ConstType.t -> CoreML.Const.t,
		      nest}) =
   let
      val {get = recursiveTargs: Var.t -> (unit -> Type.t vector) option ref,
	   ...} =
	 Property.get (Var.plist, Property.initFun (fn _ => ref NONE))
      fun recursiveFun () =
	 let
	    val boundRef: (unit -> Tyvar.t vector) option ref = ref NONE
	    val targs =
	       Promise.lazy
	       (fn () =>
		case !boundRef of
		   NONE => Error.bug "boundRef not set"
		 | SOME f => Vector.map (f (), Type.var))
	    fun markFunc func = recursiveTargs func := SOME targs
	    fun unmarkFunc func = recursiveTargs func := NONE
	    fun setBound b = boundRef := SOME b
	 in
	    {markFunc = markFunc,
	     setBound = setBound,
	     unmarkFunc = unmarkFunc}
	 end  
      fun elabType (t: Atype.t): Type.t =
	 elaborateType (t, Lookup.fromEnv E)
      fun elabTypBind (typBind: TypBind.t) =
	 let
	    val TypBind.T types = TypBind.node typBind
	    val strs =
	       Vector.map
	       (types, fn {def, tyvars, ...} =>
		TypeStr.def (Scheme.make {canGeneralize = true,
					  ty = elabType def,
					  tyvars = tyvars},
			     Kind.Arity (Vector.length tyvars)))
	 in
	    Vector.foreach2
	    (types, strs, fn ({tycon, ...}, str) =>
	     Env.extendTycon (E, tycon, str, {isRebind = false}))
	 end
      fun elabDatBind (datBind: DatBind.t, nest: string list)
	 : Decs.t * {tycon: Ast.Tycon.t,
		     typeStr: TypeStr.t} vector =
	 (* rules 28, 29, 81, 82 *)
	 let
	    val DatBind.T {datatypes, withtypes} = DatBind.node datBind
	    (* Build enough of an env so that that the withtypes and the
	     * constructor argument types can be elaborated.
	     *)
	    val datatypes =
	       Vector.map
	       (datatypes, fn {cons, tycon = name, tyvars} =>
		let
		   val kind = Kind.Arity (Vector.length tyvars)
		   val tycon =
		      Env.newTycon
		      (concat (List.separate
			       (rev (Ast.Tycon.toString name :: nest),
				".")),
		       kind,
		       AdmitsEquality.Sometimes)
		   val _ = Env.extendTycon (E, name, TypeStr.tycon (tycon, kind),
					    {isRebind = false})
		   val cons =
		      Vector.map
		      (cons, fn (name, arg) =>
		       {con = Con.fromAst name,
			name = name,
			arg = arg})
		   val makeCons =
		      Env.newCons (E, Vector.map (cons, fn {con, name, ...} =>
						  {con = con, name = name}))
		in
		   {cons = cons,
		    makeCons = makeCons,
		    name = name,
		    tycon = tycon,
		    tyvars = tyvars}
		end)
	    val change = ref false
	    fun elabAll () =
	       (elabTypBind withtypes
		; (Vector.map
		   (datatypes,
		    fn {cons, makeCons, name, tycon, tyvars} =>
		    let
		       val resultType: Type.t =
			  Type.con (tycon, Vector.map (tyvars, Type.var))
		       val (schemes, datatypeCons) =
			  Vector.unzip
			  (Vector.map
			   (cons, fn {arg, con, ...} =>
			    let
			       val (arg, ty) =
				  case arg of
				     NONE => (NONE, resultType)
				   | SOME t =>
					let
					   val t = elabType t
					in
					   (SOME t, Type.arrow (t, resultType))
					end
			       val scheme =
				  Scheme.make {canGeneralize = true,
					       ty = ty,
					       tyvars = tyvars}
			    in
			       (scheme, {arg = arg, con = con})
			    end))
		       val _ =
			  let
			     val r = TypeEnv.tyconAdmitsEquality tycon
			     datatype z = datatype AdmitsEquality.t
			  in
			     case !r of
				Always => Error.bug "datatype Always"
			      | Never => ()
			      | Sometimes =>
				   if Vector.forall
				      (datatypeCons, fn {arg, ...} =>
				       case arg of
					  NONE => true
					| SOME ty =>
					     Scheme.admitsEquality
					     (Scheme.make {canGeneralize = true,
							   ty = ty,
							   tyvars = tyvars}))
				      then ()
				   else (r := Never; change := true)
			  end
		    val typeStr =
		       TypeStr.data (tycon,
				     Kind.Arity (Vector.length tyvars),
				     makeCons schemes)
		    val _ = Env.extendTycon (E, name, typeStr, {isRebind = true})
		 in
		    ({cons = datatypeCons,
		      tycon = tycon,
		      tyvars = tyvars},
		     {tycon = name,
		      typeStr = typeStr})
		 end)))
	    (* Maximize equality. *)
	    fun loop () =
	       let
		  val res = elabAll ()
	       in
		  if !change
		     then (change := false; loop ())
		  else res
	       end
	    val (dbs, strs) = Vector.unzip (loop ())
	 in
	    (Decs.single (Cdec.Datatype dbs), strs)
	 end
      fun elabDec arg : Decs.t =
	 Trace.traceInfo
	 (info,
	  Layout.tuple3 (Ast.Dec.layout, Nest.layout, Bool.layout),
	  Layout.ignore, Trace.assertTrue)
	 (fn (d, nest, isTop) =>
	  let
	     val region = Adec.region d
	     fun lay () = seq [str "in: ", approximate (Adec.layout d)]
	     val preError = Promise.lazy (fn () => Env.setTyconNames E)
	     fun useBeforeDef (c: Tycon.t) =
		let
		   val _ = preError ()
		   open Layout
		in
		   Control.error
		   (region,
		    seq [str "type ", Tycon.layout c,
			 str " escapes the scope of its definition"],
		    lay ())
		end
	     val _ = TypeEnv.tick {useBeforeDef = useBeforeDef}
	     val unify = fn (t, t', f) => unify (t, t', preError, f)
	     fun checkSchemes (v: (Var.t * Scheme.t) vector): unit =
		if isTop
		   then
		      List.push
		      (freeTyvarChecks,
		       fn () =>
		       Vector.foreach2
		       (v, Scheme.haveFrees (Vector.map (v, #2)),
			fn ((x, s), b) =>
			if b
			   then
			      let
				 val _ = preError ()
				 open Layout
			      in
				 Control.warning
				 (region,
				  seq [str "unable to locally determine type of variable: ",
				       Var.layout x],
				  align [seq [str "type: ", Scheme.layoutPretty s],
					 lay ()])
			      end
			else ()))
		else ()
	     val elabDec = fn (d, isTop) => elabDec (d, nest, isTop)
	  in
	     case Adec.node d of
		Adec.Abstype {datBind, body} => (* rule 19 and p.57 *)
		   let
		      val ((decs, strs), decs') =
			 Env.localCore
			 (E,
			  fn () => elabDatBind (datBind, nest),
			  fn z => (z, elabDec (body, isTop)))
		      val _ =
			 Vector.foreach
			 (strs, fn {tycon, typeStr} =>
			  Env.extendTycon (E, tycon, TypeStr.abs typeStr,
					   {isRebind = false}))
		   in
		      Decs.append (decs, decs')
		   end
	      | Adec.Datatype rhs =>
		   (case DatatypeRhs.node rhs of
		       DatatypeRhs.DatBind datBind => (* rule 17 *)
			  #1 (elabDatBind (datBind, nest))
		     | DatatypeRhs.Repl {lhs, rhs} => (* rule 18 *)
			  let
			     val s = Env.lookupLongtycon (E, rhs)
			     val _ = Env.extendTycon (E, lhs, s,
						      {isRebind = false})
			  in
			     Decs.empty
			  end)
	      | Adec.Exception ebs =>
		   let
		      val decs =
			 Vector.fold
			 (ebs, Decs.empty, fn ((exn, rhs), decs) =>
			  let
			     val (decs, exn', scheme) =
				case EbRhs.node rhs of
				   EbRhs.Def c =>
				      let
					 val (c, s) = Env.lookupLongcon (E, c)
				      in
					 (decs, c, s)
				      end
				 | EbRhs.Gen arg =>
				      let
					 val exn' = Con.fromAst exn
					 val (arg, ty) =
					    case arg of
					       NONE => (NONE, Type.exn)
					     | SOME t =>
						  let
						     val t = elabType t
						  in
						     (SOME t,
						      Type.arrow (t, Type.exn))
						  end
					 val scheme = Scheme.fromType ty
				      in
					 (Decs.add (decs,
						    Cdec.Exception {arg = arg,
								    con = exn'}),
					  exn',
					  scheme)
				      end
			     val _ = Env.extendExn (E, exn, exn', scheme)
			  in
			     decs
			  end)
		   in
		      decs
		   end
	      | Adec.Fix {ops, fixity} =>
		   (Vector.foreach (ops, fn op' =>
				    Env.extendFix (E, op', fixity))
		    ; Decs.empty)
	      | Adec.Fun (tyvars, fbs) =>
		   let
		      val fbs =
			 Vector.map
			 (fbs, fn clauses =>
			  Vector.map
			  (clauses, fn {body, pats, resultType} =>
			   let
			      fun lay () =
				 approximate
				 (let
				     open Layout
				  in
				     seq [seq (List.separate
					       (Vector.toListMap
						(pats, Apat.layoutDelimit),
						str " ")),
					  str " = ",
					  Aexp.layout body]
				  end)
			      val {args, func} =
				 Parse.parseClause (pats, E, region, lay)
			   in
			      {args = args,
			       body = body,
			       func = func,
			       lay = lay,
			       resultType = resultType}
			   end))
		      val {close, ...} = TypeEnv.close (tyvars, region)
		      val {markFunc, setBound, unmarkFunc} = recursiveFun ()
		      val fbs =
			 Vector.map
			 (fbs, fn clauses =>
			  if Vector.isEmpty clauses
			     then Error.bug "no clauses in fundec"
			  else
			     let
				fun lay () =
				   let
				      open Layout
				   in
				      seq [str "in: ",
					   approximate
					   (seq
					    (separate
					     (Vector.toListMap
					      (clauses, fn {lay, ...} => lay ()),
					      " | ")))]
				   end
				val {args, func, ...} = Vector.sub (clauses, 0)
				val numArgs = Vector.length args
				val _ =
				   Vector.foreach
				   (clauses, fn {args, ...} =>
				    if numArgs = Vector.length args
				       then ()
				    else
				       Control.error
				       (region,
					seq [str "function defined with different numbers of arguments"],
					lay ()))
				val diff =
				   Vector.fold
				   (clauses, [], fn ({func = func', ...}, ac) =>
				    if Avar.equals (func, func')
				       then ac
				    else func' :: ac)
				val _ =
				   case diff of
				      [] => ()
				    | _ =>
					 let
					    val diff =
					       List.removeDuplicates
					       (func :: diff, Avar.equals)
					 in
					    Control.error
					    (region,
					     seq [str "function defined with multiple names: ",
						  seq (Layout.separateRight
						       (List.map (diff,
								  Avar.layout),
							", "))],
					     lay ())
					 end
				val var = Var.fromAst func
				val ty = Type.new ()
				val _ = Env.extendVar (E, func, var,
						       Scheme.fromType ty,
						       {isRebind = false})
				val _ = markFunc var
				val _ =
				   Acon.ensureRedefine
				   (Avid.toCon (Avid.fromVar func))
			     in
				{clauses = clauses,
				 func = func,
				 lay = lay,
				 ty = ty,
				 var = var}
			     end)
		      val _ =
			 Vector.fold
			 (fbs, [], fn ({func = f, ...}, ac) =>
			  if List.exists (ac, fn f' => Avar.equals (f, f'))
			     then
				(Control.error
				 (Avar.region f,
				  seq [str "function ",
				       Avar.layout f,
				       str " defined multiple times: "],
				  lay ())
				 ; ac)
			  else f :: ac)
		      val decs =
			 Vector.map
			 (fbs, fn {clauses,
				   func: Avar.t,
				   lay,
				   ty: Type.t,
				   var: Var.t} =>
			  let
			     val nest = Avar.toString func :: nest
			     val sourceInfo =
				SourceInfo.function {name = nest,
						     region = Avar.region func}
			     val rs =
				Vector.map
				(clauses, fn {args: Apat.t vector,
					      body: Aexp.t,
					      lay: unit -> Layout.t,
					      resultType: Atype.t option, ...} =>
				 Env.scope
				 (E, fn () =>
				  let
				     val elaboratePat = elaboratePat ()
				     val pats =
					Vector.map
					(args, fn p =>
					 {pat = #1 (elaboratePat
						    (p, E, {bind = true},
						     preError)),
					  region = Apat.region p})
				     val bodyRegion = Aexp.region body
				     val body = elabExp (body, nest, NONE)
				     val _ =
					Option.app
					(resultType, fn t =>
					 unify
					 (elabType t, Cexp.ty body,
					  fn (l1, l2) =>
					  (Atype.region t,
					   str "function result type disagrees with expression",
					   align
					   [seq [str "result type: ", l1],
					    seq [str "expression:  ", l2],
					    lay ()])))
				  in
				     {body = body,
				      bodyRegion = bodyRegion,
				      lay = lay,
				      pats = pats}
				  end))
			     val numArgs =
				Vector.length (#pats (Vector.sub (rs, 0)))
			     val argTypes =
				Vector.tabulate
				(numArgs, fn i =>
				 let
				    val t =
				       Cpat.ty
				       (#pat (Vector.sub
					      (#pats (Vector.sub (rs, 0)),
					       i)))
				    val _ =
				       Vector.foreach
				       (rs, fn {pats, ...} =>
					let
					   val {pat, region} =
					      Vector.sub (pats, i)
					in
					   unify
					   (t, Cpat.ty pat, fn (l1, l2) =>
					    (region,
					     str "function with argument of different types",
					     align [seq [str "argument: ", l2],
						    seq [str "previous: ", l1],
						    lay ()]))
					end)
				 in
				    t
				 end)
			     val t = Cexp.ty (#body (Vector.sub (rs, 0)))
			     val _ =
				Vector.foreach
				(rs, fn {body, bodyRegion, ...} =>
				 unify
				 (t, Cexp.ty body, fn (l1, l2) =>
				  (bodyRegion,
				   str "function with result of different types",
				   align [seq [str "result:   ", l2],
					  seq [str "previous: ", l1],
					  lay ()])))
			     val xs =
				Vector.tabulate (numArgs, fn _ =>
						 Var.newNoname ())
			     fun make (i: int): Cexp.t =
				if i = Vector.length xs
				   then
				      let
					 val e =
					    Cexp.casee
					    {kind = "function",
					     lay = lay,
					     noMatch = Cexp.RaiseMatch,
					     region = region,
					     rules =
					     Vector.map
					     (rs, fn {body, lay, pats, ...} =>
					      let
						 val pats =
						    Vector.map (pats, #pat)
					      in
						 {exp = body,
						  lay = SOME lay,
						  pat =
						  (Cpat.make
						   (Cpat.Tuple pats,
						    Type.tuple
						    (Vector.map (pats, Cpat.ty))))}
					      end),
					     test = 
					     Cexp.tuple
					     (Vector.map2
					      (xs, argTypes, Cexp.var))}
				      in
					 Cexp.enterLeave (e, sourceInfo)
				      end
				else
				   let
				      val body = make (i + 1)
				      val argType = Vector.sub (argTypes, i)
				   in
				      Cexp.make
				      (Cexp.Lambda
				       (Lambda.make
					{arg = Vector.sub (xs, i),
					 argType = argType,
					 body = body}),
				       Type.arrow (argType, Cexp.ty body))
				   end
			     val lambda = make 0
			     val _ =
				unify
				(Cexp.ty lambda, ty, fn (l1, l2) =>
				 (Avar.region func,
				  str "function type disagrees with recursive use",
				  align [seq [str "function type: ", l1],
					 seq [str "recursive use: ", l2],
					 lay ()]))
			     val lambda =
				case Cexp.node lambda of
				   Cexp.Lambda l => l
				 | _ => Lambda.bogus
			  in
			     {lambda = lambda,
			      ty = ty,
			      var = var}
			  end)
		      val {bound, schemes} = close (Vector.map (decs, #ty))
		      val _ = checkSchemes (Vector.zip
					    (Vector.map (decs, #var),
					     schemes))
		      val _ = setBound bound
		      val _ =
			 Vector.foreach3
			 (fbs, decs, schemes,
			  fn ({func, ...}, {var, ...}, scheme) =>
			  (Env.extendVar (E, func, var, scheme,
					  {isRebind = true})
			   ; unmarkFunc var))
		      val decs =
			 Vector.map (decs, fn {lambda, var, ...} =>
				     {lambda = lambda, var = var})
		   in
		      Decs.single (Cdec.Fun {decs = decs,
					     tyvars = bound})
		   end
	      | Adec.Local (d, d') =>
		   Env.localCore
		   (E,
		    fn () => elabDec (d, false),
		    fn decs => Decs.append (decs, elabDec (d', isTop)))
	      | Adec.Open paths =>
		   let
		      (* The following code is careful to first lookup all of the
		       * paths in the current environment, and then extend the
		       * environment with all of the results.
		       * See rule 22 of the Definition.
		       *)
		      val _ =
			 Vector.foreach
			 (Vector.map (paths, fn p => Env.lookupLongstrid (E, p)),
			  fn so => Option.app (so, fn s =>
					       Env.openStructure (E, s)))
		   in
		      Decs.empty
		   end
	      | Adec.Overload (p, x, tyvars, ty, xs) =>
		   let
		      (* Lookup the overloads before extending the var in case
		       * x appears in the xs.
		       *)
		      val ovlds =
			 Vector.map (xs, fn x => Env.lookupLongvar (E, x))
		      val _ =
			 Env.extendOverload
			 (E, p, x, 
			  Vector.map (ovlds, fn (x, s) => (x, Scheme.ty s)),
			  Scheme.make {canGeneralize = false,
				       tyvars = tyvars,
				       ty = elabType ty})
		   in
		      Decs.empty
		   end
	      | Adec.SeqDec ds =>
		   Vector.fold (ds, Decs.empty, fn (d, decs) =>
				Decs.append (decs, elabDec (d, isTop)))
	      | Adec.Type typBind =>
		   (elabTypBind typBind
		    ; Decs.empty)
	      | Adec.Val {tyvars, rvbs, vbs} =>
		   let
		      val {close, dontClose} = TypeEnv.close (tyvars, region)
		      (* Must do all the es and rvbs before the ps because of
		       * scoping rules.
		       *)
		      val vbs =
			 Vector.map
			 (vbs, fn {exp, pat, ...} =>
			  let
			     fun lay () =
				let
				   open Layout
				in
				   seq [str "in: ",
					approximate
					(seq [Apat.layout pat,
					      str " = ", Aexp.layout exp])]
				end
			  in
			     {exp = elabExp (exp, nest,
					     SOME (case Apat.getName pat of
						      NONE => "anon"
						    | SOME s => s)),
			      expRegion = Aexp.region exp,
			      lay = lay,
			      pat = pat,
			      patRegion = Apat.region pat}
			  end)
		      val close =
			 case Vector.peek (vbs, Cexp.isExpansive o #exp) of
			    NONE => close
			  | SOME {expRegion, ...} => 
			       let
				  val _ =
				     if Vector.isEmpty tyvars
					then ()
				     else
					Control.error
					(expRegion,
					 seq [str
					      (concat
					       ["can't bind type variable",
						if Vector.length tyvars > 1
						   then "s"
						else "",
						": "]),
					      seq (Layout.separateRight
						   (Vector.toListMap (tyvars, Tyvar.layout),
						    ", "))],
					 lay ())
			       in
				  fn tys =>
				  (dontClose ()
				   ; {bound = fn () => Vector.new0 (),
				      schemes =
				      Vector.map (tys, Scheme.fromType)})
			       end
		      val {markFunc, setBound, unmarkFunc} = recursiveFun ()
		      val elaboratePat = elaboratePat ()
		      val rvbs =
			 Vector.map
			 (rvbs, fn {pat, match} =>
			  let
			     val region = Apat.region pat
			     val (pat, bound) =
				elaboratePat (pat, E, {bind = false}, preError)
			     val (nest, var, ty) =
				if 0 = Vector.length bound
				   then ("anon" :: nest,
					 Var.newNoname (),
					 Type.new ())
				else
				   let
				      val (x, x', t) = Vector.sub (bound, 0)
				   in
				      (Avar.toString x :: nest, x', t)
				   end
			     val _ = markFunc var
			     val scheme = Scheme.fromType ty
			     val bound =
				Vector.map
				(bound, fn (x, _, _) =>
				 (Acon.ensureRedefine (Avid.toCon
						       (Avid.fromVar x))
				  ; ensureNotEquals x
				  ; Env.extendVar (E, x, var, scheme,
						   {isRebind = false})
				  ; (x, var, ty)))
			  in
			     {bound = bound,
			      match = match,
			      nest = nest,
			      pat = pat,
			      region = region,
			      var = var}
			  end)
		      val rvbs =
			 Vector.map
			 (rvbs, fn {bound, match, nest, pat, var, ...} =>
			  let
			     val {argType, region, resultType, rules} =
				elabMatch (match, preError, nest)
			     val _ =
				unify
				(Cpat.ty pat,
				 Type.arrow (argType, resultType),
				 fn (l1, l2) =>
				 (region,
				  str "function type disagrees with recursive uses",
				  align [seq [str "function type:  ", l1],
					 seq [str "recursive uses: ", l2],
					 lay ()]))
			     val arg = Var.newNoname ()
			     val body =
				Cexp.enterLeave
				(Cexp.casee {kind = "function",
					     lay = lay,
					     noMatch = Cexp.RaiseMatch,
					     region = region,
					     rules = rules,
					     test = Cexp.var (arg, argType)},
				 SourceInfo.function {name = nest,
						      region = region})
			     val lambda =
				Lambda.make {arg = arg,
					     argType = argType,
					     body = body}
			  in
			     {bound = bound,
			      lambda = lambda,
			      var = var}
			  end)
		      val boundVars =
			 Vector.map
			 (Vector.concatV (Vector.map (rvbs, #bound)),
			  fn x => (x, {isRebind = true}))
		      val rvbs =
			 Vector.map
			 (rvbs, fn {bound, lambda, var} =>
			  (Vector.foreach (bound, unmarkFunc o #2)
			   ; {lambda = lambda,
			      var = var}))
		      val vbs =
			 Vector.map
			 (vbs,
			  fn {exp = e, expRegion, lay, pat, patRegion, ...} =>
			  let
			     val (p, bound) =
				elaboratePat (pat, E, {bind = false}, preError)
			     val _ =
				unify
				(Cpat.ty p, Cexp.ty e, fn (p, e) =>
				 (Apat.region pat,
				  str "pattern and expression disagree",
				  align [seq [str "pattern:    ", p],
					 seq [str "expression: ", e],
					 lay ()]))
			  in
			     {bound = bound,
			      exp = e,
			      expRegion = expRegion,
			      lay = lay,
			      pat = p,
			      patRegion = patRegion}
			  end)
		      val boundVars =
			 Vector.concat
			 [boundVars,
			  Vector.map
			  (Vector.concatV (Vector.map (vbs, #bound)),
			   fn x => (x, {isRebind = false}))]
		      val {bound, schemes} =
			 close (Vector.map (boundVars, #3 o #1))
		      val _ = checkSchemes (Vector.zip
					    (Vector.map (boundVars, #2 o #1),
					     schemes))
		      val _ = setBound bound
		      val _ =
			 Vector.foreach2
			 (boundVars, schemes, fn (((x, x', _), ir), scheme) =>
			  Env.extendVar (E, x, x', scheme, ir))
		      val vbs =
			 Vector.map (vbs, fn {exp, lay, pat, patRegion, ...} =>
				     {exp = exp,
				      lay = lay,
				      pat = pat,
				      patRegion = patRegion})
		   in
		      (* According to page 28 of the Definition, we should
		       * issue warnings for nonexhaustive valdecs only when it's
		       * not a top level dec.   It seems harmless enough to go
		       * ahead and always issue them.
		       *)
		      Decs.single (Cdec.Val {rvbs = rvbs,
					     tyvars = bound,
					     vbs = vbs})
		   end
	  end) arg
      and elabExp (arg: Aexp.t * Nest.t * string option): Cexp.t =
	 Trace.traceInfo
	 (elabExpInfo,
	  Layout.tuple3 (Aexp.layout, Nest.layout, Layout.ignore),
	  Layout.ignore,
	  Trace.assertTrue)
	 (fn (e: Aexp.t, nest, maybeName) =>
	  let
	     val preError = Promise.lazy (fn () => Env.setTyconNames E)
	     val unify = fn (t, t', f) => unify (t, t', preError, f)
	     fun lay () = seq [str "in: ", approximate (Aexp.layout e)]
	     val unify =
		fn (a, b, f) => unify (a, b, fn z =>
				       let
					  val (r, l, l') = f z
				       in
					  (r, l, align [l', lay ()])
				       end)
	     val region = Aexp.region e
	     fun elab e = elabExp (e, nest, NONE)
	  in
	     case Aexp.node e of
		Aexp.Andalso (e, e') =>
		   let
		      val ce = elab e
		      val ce' = elab e'
		      fun doit (ce, br) =
			 unify
			 (Cexp.ty ce, Type.bool,
			  fn (l, _) =>
			  (Aexp.region e,
			   str (concat
				[br, " branch of andalso not of type bool"]),
			   seq [str " branch: ", l]))
		      val _ = doit (ce, "left")
		      val _ = doit (ce', "right")
		   in
		      Cexp.andAlso (ce, ce')
		   end
	      | Aexp.App (e1, e2) =>
		   let
		      val e1 = elab e1
		      val e2 = elab e2
		      val argType = Type.new ()
		      val resultType = Type.new ()
		      val _ =
			 unify (Cexp.ty e1, Type.arrow (argType, resultType),
				fn (l, _) =>
				(region,
				 str "function not of arrow type",
				 seq [str "function: ", l]))
		      val _ =
			 unify
			 (argType, Cexp.ty e2, fn (l1, l2) =>
			  (region,
			   str "function applied to incorrect argument",
			   align [seq [str "expects: ", l1],
				  seq [str "but got: ", l2]]))
		   in
		      Cexp.make (Cexp.App (e1, e2), resultType)
		   end
	      | Aexp.Case (e, m) =>
		   let
		      val e = elab e
		      val {argType, rules, ...} = elabMatch (m, preError, nest)
		      val _ =
			 unify
			 (Cexp.ty e, argType, fn (l1, l2) =>
			  (region,
			   str "case object and rules disagree",
			   align [seq [str "object type:  ", l1],
				  seq [str "rules expect: ", l2]]))
		   in
		      Cexp.casee {kind = "case",
				  lay = lay,
				  noMatch = Cexp.RaiseMatch,
				  region = region,
				  rules = rules,
				  test = e}
		   end
	      | Aexp.Const c =>
		   elabConst
		   (c,
		    fn (resolve, ty) => Cexp.make (Cexp.Const resolve, ty),
		    {false = Cexp.falsee,
		     true = Cexp.truee})
	      | Aexp.Constraint (e, t') =>
		   let
		      val e = elab e
		      val _ =
			 unify
			 (Cexp.ty e, elabType t', fn (l1, l2) =>
			  (region,
			   str "expression and constraint disagree",
			   align [seq [str "expects: ", l2],
				  seq [str "but got: ", l1]]))
		   in
		      e
		   end
	      | Aexp.FlatApp items => elab (Parse.parseExp (items, E, lay))
	      | Aexp.Fn m =>
		   let
		      val nest =
			 case maybeName of
			    NONE => "anon" :: nest
			  | SOME s => s :: nest
		      val {arg, argType, body} =
			 elabMatchFn (m, preError, nest, "function", lay,
				      Cexp.RaiseMatch)
		      val body =
			 Cexp.enterLeave
			 (body, SourceInfo.function {name = nest,
						     region = region})
		   in
		      Cexp.make (Cexp.Lambda (Lambda.make {arg = arg,
							   argType = argType,
							   body = body}),
				 Type.arrow (argType, Cexp.ty body))
		   end
	      | Aexp.Handle (try, match) =>
		   let
		      val try = elab try
		      val {arg, argType, body} =
			 elabMatchFn (match, preError, nest, "handler", lay,
				      Cexp.RaiseAgain)
		      val _ =
			 unify
			 (Cexp.ty try, Cexp.ty body, fn (l1, l2) =>
			  (region,
			   str "expression and handler disagree",
			   align [seq [str "expression: ", l1],
				  seq [str "handler:    ", l2]]))
		      val _ =
			 unify
			 (argType, Type.exn, fn (l1, _) =>
			  (Amatch.region match,
			   seq [str "handler handles wrong type: ", l1],
			   empty))
		   in
		      Cexp.make (Cexp.Handle {catch = (arg, Type.exn),
					      handler = body, 
					      try = try},
				 Cexp.ty try)
		   end
	      | Aexp.If (a, b, c) =>
		   let
		      val a' = elab a
		      val b' = elab b
		      val c' = elab c
		      val _ =
			 unify
			 (Cexp.ty a', Type.bool, fn (l1, _) =>
			  (Aexp.region a,
			   str "if test not of type bool",
			   seq [str "test type: ", l1]))
		      val _ =
			 unify
			 (Cexp.ty b', Cexp.ty c', fn (l1, l2) =>
			  (region,
			   str "then and else branches disagree",
			   align [seq [str "then: ", l1],
				  seq [str "else: ", l2]]))
		   in
		      Cexp.iff (a', b', c')
		   end
	      | Aexp.Let (d, e) =>
		   Env.scope
		   (E, fn () =>
		    let
		       (* Create a new type (at the current time) before
			* elaborating the decs.  Then, unification with this
			* type makes sure that any datatypes declared in the
			* decs don't escape.
			*)
		       val t = Type.new ()
		       val d = Decs.toVector (elabDec (d, nest, false))
		       val e = elab e
		       val ty = Cexp.ty e
		       val _ =
			  unify
			  (t, ty, fn _ => Error.bug "elabExp Let unify failure")
		    in
		       Cexp.make (Cexp.Let (d, e), ty)
		    end)
	      | Aexp.List es =>
		   let
		      val es' = Vector.map (es, elab)
		   in
		      Cexp.make (Cexp.List es',
				 unifyList
				 (Vector.map2 (es, es', fn (e, e') =>
					       (Cexp.ty e', Aexp.region e)),
				  preError, lay))
		   end
	      | Aexp.Orelse (e, e') =>
		   let
		      val ce = elab e
		      val ce' = elab e'
		      fun doit (ce, br) =
			 unify
			 (Cexp.ty ce, Type.bool,
			  fn (l, _) =>
			  (Aexp.region e,
			   str (concat
				[br, " branch of orelse not of type bool"]),
			   seq [str " branch: ", l]))
		      val _ = doit (ce, "left")
		      val _ = doit (ce', "right")
		   in
		      Cexp.orElse (ce, ce')
		   end
	      | Aexp.Prim {kind, name, ty} =>
		   let
		      val ty = elabType ty
		      val expandedTy =
			 Type.hom
			 (ty, {con = Type.con,
			       expandOpaque = true,
			       record = Type.record,
			       replaceCharWithWord8 = true,
			       var = Type.var})
		      (* We use expandedTy to get the underlying primitive right
		       * but we use wrap in the end to make the result of the
		       * final expression be ty, because that is what the rest
		       * of the code expects to see.
		       *)
		      fun wrap (e, t) = Cexp.make (Cexp.node e, t)
		      fun primApp {args, prim, result: Type.t} =
			 let
			    val targs =
			       Prim.extractTargs
			       {args = Vector.map (args, Cexp.ty),
				deArray = Type.deArray,
				deArrow = Type.deArrow,
				deRef = Type.deRef,
				deVector = Type.deVector,
				deWeak = Type.deWeak,
				prim = prim,
				result = result}
			 in
			    Cexp.make (Cexp.PrimApp {args = args,
						     prim = prim,
						     targs = targs},
				       result)
			 end
		      fun eta (p: Prim.t): Cexp.t =
			 case Type.deArrowOpt expandedTy of
			    NONE =>
			       wrap (primApp {args = Vector.new0 (),
					      prim = p,
					      result = ty},
				     ty)
			  | SOME (argType, bodyType) =>
			       let
				  val arg = Var.newNoname ()
				  fun app args =
				     primApp {args = args,
					      prim = p,
					      result = bodyType}
				  val body =
				     case Type.deTupleOpt argType of
					NONE =>
					   app (Vector.new1
						(Cexp.var (arg, argType)))
				      | SOME ts =>
					   let
					      val vars =
						 Vector.map
						 (ts, fn t =>
						  (Var.newNoname (), t))
					   in
					      Cexp.casee
					      {kind = "",
					       lay = fn _ => Layout.empty,
					       noMatch = Cexp.Impossible,
					       region = Region.bogus,
					       rules =
					       Vector.new1
					       {exp = app (Vector.map
							   (vars, Cexp.var)),
						lay = NONE,
						pat =
						(Cpat.tuple
						 (Vector.map (vars, Cpat.var)))},
					       test = Cexp.var (arg, argType)}
					   end
			       in
				  Cexp.make (Cexp.Lambda
					     (Lambda.make {arg = arg,
							   argType = argType,
							   body = body}),
					     ty)
			       end
		      fun lookConst (name: string) =
			 let
			    fun bug () =
			       let
				  open Layout
				  val _ =
				     Control.error
				     (region,
				      seq [str "strange constant type: ",
					   Type.layout expandedTy],
				      empty)
			       in
				  Error.bug "lookConst bug"
			       end
			 in
			    case Type.deConOpt expandedTy of
			       NONE => bug ()
			     | SOME (c, ts) =>
				  let
				     val ct =
					if Tycon.equals (c, Tycon.bool)
					   then ConstType.Bool
					else if Tycon.isIntX c
						then ConstType.Int
					else if Tycon.isRealX c
						then ConstType.Real
					else if Tycon.isWordX c
						then ConstType.Word
					else if Tycon.equals (c, Tycon.vector)
					   andalso 1 = Vector.length ts
					   andalso
					   (case (Type.deConOpt
						  (Vector.sub (ts, 0))) of
					       NONE => false
					     | SOME (c, _) => 
						  Tycon.equals (c, Tycon.char))
						then ConstType.String
					     else
						bug ()
				  fun finish () = lookupConstant (name, ct)
			       in
				  Cexp.make (Cexp.Const finish, ty)
			       end
			 end
		      datatype z = datatype Ast.PrimKind.t
		   in
		      case kind of
			 BuildConst => lookConst name
		       | Const => lookConst name
		       | Export attributes =>
			    let
			       val e =
				  Env.scope
				  (E, fn () =>
				   (Env.openStructure
				    (E, valOf (!Env.Structure.ffi))
				    ; elabExp (export {attributes = attributes,
						       name = name,
						       region = region,
						       ty = expandedTy},
					       nest,
					       NONE)))
			       val _ =
				  unify
				  (Cexp.ty e,
				   Type.arrow (expandedTy, Type.unit),
				   fn (l1, l2) =>
				   let
				      open Layout
				   in
				      (region,
				       str "export unify bug",
				       align [seq [str "inferred: ", l1],
					      seq [str "expanded: ", l2]])
				   end)
			    in
			       wrap (e, Type.arrow (ty, Type.unit))
			    end
		       | Import attributes =>
			    eta (import {attributes = attributes,
					 name = name,
					 region = region,
					 ty = expandedTy})
		       | Prim => eta (Prim.new name)
		   end
	      | Aexp.Raise exn =>
		   let
		      val region = Aexp.region exn
		      val exn = elab exn
		      val _ =
			 unify
			 (Cexp.ty exn, Type.exn, fn (l1, _) =>
			  (region,
			   str "raise of non exception",
			   seq [str "exp type: ", l1]))
		      val resultType = Type.new ()
		   in
		      Cexp.make (Cexp.Raise {exn = exn, region = region},
				 resultType)
		   end
	      | Aexp.Record r =>
		   let
		      val r = Record.map (r, elab)
		      val ty =
			 Type.record
			 (SortedRecord.fromVector
			  (Record.toVector (Record.map (r, Cexp.ty))))
		   in
		      Cexp.make (Cexp.Record r, ty)
		   end
	      | Aexp.Selector f => elab (Aexp.selector (f, region))
	      | Aexp.Seq es =>
		   let
		      val es' = Vector.map (es, elab)
		      val last = Vector.length es - 1
		      (* Error for expressions before a ; that don't return
		       * unit.
		       *)
		      val _ =
			 if not (!Control.sequenceUnit)
			    then ()
			 else
			    Vector.foreachi
			    (es', fn (i, e') =>
			     if i = last
				then ()
			     else
				let
				   fun error (l, _) =
				      let
					 val e = Vector.sub (es, i)
					 open Layout
				      in
					 Control.error
					 (Aexp.region e,
					  str "sequence expression not of type unit",
					  align [seq [str "type: ", l],
						 seq [str "in: ",
						      approximate (Aexp.layout e)]])
				      end
				in
				   Type.unify (Cexp.ty e', Type.unit,
					       {error = error,
						preError = preError})
				end)
		   in
		      Cexp.make (Cexp.Seq es', Cexp.ty (Vector.sub (es', last)))
		   end
	      | Aexp.Var {name = id, ...} =>
		   let
		      val (vid, scheme) = Env.lookupLongvid (E, id)
		      val {args, instance} = Scheme.instantiate scheme
		      fun con c = Cexp.Con (c, args ())
		      val e =
			 case vid of
			    Vid.Con c => con c
			  | Vid.Exn c => con c
			  | Vid.Overload (p, yts) =>
			       let
				  val resolve =
				     Promise.lazy
				     (fn () =>
				      case (Vector.peek
					    (yts, fn (_, t) =>
					     Type.canUnify (instance, t))) of
					 NONE =>
					    let
					       val _ =
						  Control.error
						  (region,
						   seq [str "impossible use of overloaded var: ",
							str (Longvid.toString id)],
						   Type.layoutPretty instance)
					    in
					       Var.newNoname ()
					    end
				       | SOME (y, t) =>  
					    (unify (instance, t, fn _ =>
						    Error.bug "overload unify")
					     ; y))
				  val _ = 
				     List.push (overloads, (p, ignore o resolve))
			       in
				  Cexp.Var (resolve, fn () => Vector.new0 ())
			       end
			  | Vid.Var x =>
			       Cexp.Var (fn () => x,
					 case ! (recursiveTargs x) of
					    NONE => args
					  | SOME f => f)
		   in
		      Cexp.make (e, instance)
		   end
	      | Aexp.While {expr, test} =>
		   let
		      val test' = elab test
		      val _ =
			 unify
			 (Cexp.ty test', Type.bool, fn (l1, _) =>
			  (Aexp.region test,
			   str "while test not of type bool",
			   seq [str "test type: ", l1]))
		      val expr = elab expr
		      (* Error if expr is not of type unit. *)
		      val _ =
			 if not (!Control.sequenceUnit)
			    then ()
			 else
			    unify (Cexp.ty expr, Type.unit, fn (l, _) =>
				   (region,
				    str "while body not of type unit",
				    seq [str "body type: ", l]))
		   in
		      Cexp.whilee {expr = expr, test = test'}
		   end
	  end) arg
      and elabMatchFn (m: Amatch.t, preError, nest, kind, lay, noMatch) =
	 let
	    val arg = Var.newNoname ()
	    val {argType, region, rules, ...} = elabMatch (m, preError, nest)
	    val body =
	       Cexp.casee {kind = kind,
			   lay = lay,
			   noMatch = noMatch,
			   region = region,
			   rules = rules,
			   test = Cexp.var (arg, argType)}
	 in
	   {arg = arg,
	    argType = argType,
	    body = body}
	 end
      and elabMatch (m: Amatch.t, preError, nest: Nest.t) =
	 let
	    val region = Amatch.region m
	    val Amatch.T rules = Amatch.node m
	    val argType = Type.new ()
	    val resultType = Type.new ()
	    val rules =
	       Vector.map
	       (rules, fn (pat, exp) =>
		Env.scope
		(E, fn () =>
		 let
		    fun lay () =
		       let
			  open Layout
		       in
			  approximate
			  (seq [Apat.layout pat, str " => ", Aexp.layout exp])
		       end
		    val (p, _) =
		       elaboratePat () (pat, E, {bind = true}, preError)
		    val _ =
		       unify
		       (Cpat.ty p, argType, preError, fn (l1, l2) =>
			(Apat.region pat,
			 str "rule patterns disagree",
			 align [seq [str "pattern:  ", l1],
				seq [str "previous: ", l2],
				seq [str "in: ", lay ()]]))
		    val e = elabExp (exp, nest, NONE)
		    val _ =
		       unify
		       (Cexp.ty e, resultType, preError, fn (l1, l2) =>
			(Aexp.region exp,
			 str "rule results disagree",
			 align [seq [str "result:   ", l1],
				seq [str "previous: ", l2],
				seq [str "in: ", lay ()]]))
		 in
		    {exp = e,
		     lay = SOME lay,
		     pat = p}
		 end))
	 in
	    {argType = argType,
	     region = region,
	     resultType = resultType,
	     rules = rules}
	 end
      val ds = elabDec (Scope.scope d, nest, true)
      (* List.insertionSort is anti-stable;
       * hence, it sorts and reverses the overloads. 
       *)
      val _ = List.foreach (List.insertionSort
			    (!overloads, fn ((x,_),(y,_)) =>
			     Priority.<= (y, x)),
			    fn (_,p) => (p (); ()))
      val _ = overloads := []
   in
      ds
   end

fun reportUndeterminedTypes () =
   (List.foreach (rev (!freeTyvarChecks), fn p => p ())
    ; freeTyvarChecks := [])

end
