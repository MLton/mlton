(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor AstCore (S: AST_CORE_STRUCTS): AST_CORE = 
struct

open S Layout
structure Field = Record.Field
structure Wrap = Region.Wrap

structure Fixity =
   struct
      datatype t =
	 Infix of int option
       | Infixr of int option
       | Nonfix

      val bogus = Nonfix

      val isInfix =
	 fn Nonfix => false
	  | _ => true
	  
      val toString =
	 fn Infix NONE => "infix"
	  | Infix (SOME n) => "infix " ^ Int.toString n
	  | Infixr NONE => "infixr"
	  | Infixr (SOME n) => "infixr " ^ Int.toString n
	  | Nonfix => "nonfix"
	       
      val layout = Layout.str o toString
   end

structure Fixop =
   struct
      datatype t = Op | None

      val layout =
	 fn Op => str "op "
	  | None => empty
   end

fun layoutApp (func: 'a,
	      getLongvid: 'a -> Longvid.t option,
	      layoutFunc: 'a -> Layout.t,
	      arg: 'b,
	      getPair: 'b -> ('b * 'b) option,
	      layoutArg: 'b -> Layout.t) =
   mayAlign [layoutFunc func, layoutArg arg]
	 
fun layoutConstraint (t, ty) =
   mayAlign [seq [t, str ":"], Type.layout ty]
   
fun maybeConstrain (e, tyo) =
   case tyo of
      NONE => e
    | SOME ty => layoutConstraint (e, ty)

fun layoutLongvid x =
   str (let val s = Longvid.toString x
	in if s = "*" then " * "
	   else if String.isSuffix {string = s, suffix = "*"}
		   then s ^ " "
		else s
	end)

structure Vector =
   struct
      open Vector

      fun cons (x, v) = concat [new1 x, v]
   end

(*---------------------------------------------------*)
(*                     Patterns                      *)
(*---------------------------------------------------*)

structure Pat =
   struct
      open Wrap
      datatype node =
	 App of Longcon.t * t
       | Const of Const.t
       | Constraint of t * Type.t
       | FlatApp of t vector
       | Layered of {fixop: Fixop.t,
		     var: Var.t,
		     constraint: Type.t option,
		     pat: t}
       | List of t vector
       | Record of {flexible: bool,
		    items: (Record.Field.t * item) vector}
       | Tuple of t vector
       | Var of {fixop: Fixop.t, name: Longvid.t}
       | Wild
      and item =
	 Field of t
	| Vid of Vid.t * Type.t option * t option 
      withtype t = node Wrap.t
      type node' = node
      type obj = t

      structure Item =
	 struct
	    type pat = t
	    datatype t = datatype item
	 end

      fun make n = makeRegion (n, Region.bogus)

      val wild = make Wild
      val const = make o Const
      val record = make o Record
      val constraint = make o Constraint
      val layered = make o Layered

      val emptyList = make (List (Vector.new0 ()))

      fun longvid x = make (Var {name = x, fixop = Fixop.None})
      val var = longvid o Longvid.short o Vid.fromVar

      fun con c =
	 if Con.equals (c, Con.nill) then emptyList
	 else longvid (Longvid.short (Vid.fromCon c))
		     
      fun app (c, p) =
	 let
	    val default = make (App (Longcon.short c, p))
	 in
	    if Con.equals (c, Con.cons)
	       then
		  case node p of
		     Tuple ps =>
			if 2 = Vector.length ps
			   then
			      let
				 val p0 = Vector.sub (ps, 0)
				 val p1 = Vector.sub (ps, 1)
			      in
				 case node p1 of
				    List ps => make (List (Vector.cons (p0, ps)))
				  | _ => default
			      end
			else default
		   | _ => default
	    else default
	 end
      
      fun isWild p =
	 case node p of
	    Wild => true
	  | _ => false

      local
	 fun isLongvid x p =
	    case node p of
	       Var {name=x', ...} => Longvid.equals (x, x')
	     | _ => false
      in
	 val isTrue = isLongvid Longvid.truee
	 val isFalse = isLongvid Longvid.falsee
      end
   
      fun tuple ps =
	 if 1 = Vector.length ps
	    then Vector.sub (ps, 0)
	 else make (Tuple ps)

      fun layout (p, isDelimited) =
	 let
	    fun delimit t = if isDelimited then t else paren t
	 in
	    case node p of
	       App (c, p) => delimit (mayAlign [Longcon.layout c,
						layoutF p])
	     | Const c => Const.layout c
	     | Constraint (p, t) => delimit (layoutConstraint (layoutF p, t))
	     | FlatApp ps => delimit (layoutFlatApp ps)
	     | Layered {fixop, var, constraint, pat} =>
		  delimit
		  (mayAlign [maybeConstrain
			     (seq [Fixop.layout fixop, Var.layout var],
			      constraint),
			     seq [str "as ", layoutT pat]])
	     | List ps => list (Vector.toListMap (ps, layoutT))
	     | Record {items, flexible} =>
		  seq [str "{",
		       mayAlign (separateRight
				 (Vector.toListMap (items, layoutItem), ",")),
		       if flexible
			  then str (if Vector.isEmpty items
				       then "..."
				    else ", ...")
		       else empty,
		       str "}"]
	     | Tuple ps => Vector.layout layoutT ps
	     | Var {name, fixop} => seq [Fixop.layout fixop, layoutLongvid name]
	     | Wild => str "_"
	 end
      and layoutF p = layout (p, false)
      and layoutT p = layout (p, true)
      and layoutFlatApp ps = seq (separate (Vector.toListMap (ps, layoutF), " "))
      and layoutItem (f, i) =
	 seq [Field.layout f,
	      case i of
		 Field p => seq [str " = ", layoutT p]
	       | Vid (_, tyo, po) =>
		    seq [case tyo of
			    NONE => empty
			  | SOME ty => seq [str ": ", Type.layout ty],
                         case po of
			    NONE => empty
			  | SOME p => seq [str " as ", layoutT p]]]

      val layoutDelimit = layoutF
      val layout = layoutT
   
      val unit = tuple (Vector.new0 ())
   end

structure Eb =
   struct
      structure Rhs =
	 struct
	    open Wrap
	    datatype node =
	       Gen of Type.t option
	     | Def of Longcon.t
	    type t = node Wrap.t
	    type node' = node
	    type obj = t
	 
	    fun layout rhs =
	       case node rhs of
		  Gen to => Type.layoutOption to
		| Def c => seq [str " = ", Longcon.layout c]
	 end
      
      type t = Con.t * Rhs.t

      fun gen (c, to) = {exn = c, rhs = Rhs.Gen to}

      fun layout (exn, rhs) =
	 seq [Con.layout exn, Rhs.layout rhs]
   end

structure EbRhs = Eb.Rhs

structure PrimKind =
   struct
      structure Attribute =
	 struct
	    datatype t = Cdecl | Stdcall

	    val toString: t -> string =
	       fn Cdecl => "cdecl"
		| Stdcall => "stdcall"

	    val layout = Layout.str o toString
	 end

      datatype t =
	 BuildConst
       | Const
       | Export of Attribute.t list
       | Import of Attribute.t list
       | Prim
   end

structure Priority =
   struct
      datatype t = T of int option
      val op <= = fn (T x, T y) =>
	 case (x, y) of
	    (NONE, NONE) => true
	  | (NONE, _) => true
	  | (_, NONE) => false
	  | (SOME x, SOME y) => Int.<= (x, y)
      val default = T NONE
      fun layout (T x) =
	 case x of
	    NONE => Layout.empty
	  | SOME x => Int.layout x
   end

datatype expNode =
   Var of {name: Longvid.t, fixop: Fixop.t}
  | Fn of match
  | FlatApp of exp vector
  | App of exp * exp
  | Case of exp * match
  | Let of dec * exp
  | Seq of exp vector
  | Const of Const.t
  | Record of expNode Wrap.t Record.t (* the Kit barfs on exp Record.t *)
  | List of exp vector
  | Selector of Field.t
  | Constraint of exp * Type.t
  | Handle of exp * match
  | Raise of exp
  | If of exp * exp * exp
  | Andalso of exp * exp
  | Orelse of exp * exp
  | While of {test: exp, expr: exp}
  | Prim of {kind: PrimKind.t,
	     name: string,
	     ty: Type.t}
and decNode =
   Abstype of {body: dec,
	       datBind: DatBind.t}
  | Datatype of DatatypeRhs.t
  | Exception of Eb.t vector
  | Fix of {fixity: Fixity.t,
	    ops: Vid.t vector}
  | Fun of Tyvar.t vector * {body: exp,
			     pats: Pat.t vector,
			     resultType: Type.t option} vector vector
  | Local of dec * dec
  | Open of Longstrid.t vector
  | Overload of Priority.t * Var.t * 
                Tyvar.t vector * Type.t * 
                Longvar.t vector
  | SeqDec of dec vector
  | Type of TypBind.t
  | Val of {tyvars: Tyvar.t vector,
	    vbs: {exp: exp,
		  pat: Pat.t} vector,
	    rvbs: {match: match,
		   pat: Pat.t} vector}
and matchNode = T of (Pat.t * exp) vector
withtype
    dec = decNode Wrap.t
and exp = expNode Wrap.t
and match = matchNode Wrap.t

open Wrap

structure Match =
   struct
      open Wrap
      type t = match
      datatype node = datatype matchNode
      type node' = node
      type obj = t
   end

fun layoutAndsTyvars (prefix, (tyvars, xs), layoutX) =
   layoutAnds (prefix,
	       case Vector.toListMap (xs, layoutX) of
		  x :: xs =>
		     (if Vector.isEmpty tyvars
			 then x
		      else seq [Tyvar.layouts tyvars, str " ", x]) :: xs
		| [] => [],
	      fn (prefix, x) => seq [prefix, x])

fun expNodeName e =
   case node e of
      Andalso _ => "Andalso"
    | App _ => "App"
    | Case _ => "Case"
    | Const _ => "Const"
    | Constraint _ => "Constraint"
    | FlatApp _ => "FlatApp"
    | Fn _ => "Fn"
    | Handle _ => "Handle"
    | If _ => "If"
    | Let _ => "Let"
    | List _ => "List"
    | Orelse _ => "Orelse"
    | Prim _ => "Prim"
    | Raise _ => "Raise"
    | Record _ => "Record"
    | Selector _ => "Selector"
    | Seq _ => "Seq"
    | Var _ => "Var"
    | While _ => "While"

val traceLayoutExp =
   Trace.traceInfo' (Trace.info "layoutExp",
		     fn (e, b: bool) => Layout.str (expNodeName e),
		     Layout.ignore: Layout.t -> Layout.t)
   
fun layoutExp arg =
   traceLayoutExp
   (fn (e, isDelimited) =>
   let
      fun delimit t = if isDelimited then t else paren t
   in
      case node e of
	 Andalso (e, e') =>
	    delimit (mayAlign [layoutExpF e,
			       seq [str "andalso ", layoutExpF e']])
       | App (function, argument) =>
	    delimit (mayAlign [layoutExpF function, layoutExpF argument])
       | Case (expr, match) =>
	    delimit (align [seq [str "case ", layoutExpT expr,
				 str " of"],
			    indent (layoutMatch match, 2)])
       | Const c => Const.layout c
       | Constraint (expr, constraint) =>
	    delimit (layoutConstraint (layoutExpF expr, constraint))
       | FlatApp es =>
	    delimit (seq (separate (Vector.toListMap (es, layoutExpF), " ")))
       | Fn m => delimit (seq [str "fn ", layoutMatch m])
       | Handle (try, match) =>
	    delimit (align [layoutExpF try,
			    seq [str "handle ", layoutMatch match]])
       | If (test, thenCase, elseCase) =>
	    delimit (mayAlign [seq [str "if ", layoutExpT test],
			       seq [str "then ", layoutExpT thenCase],
			       seq [str "else ", layoutExpT elseCase]])
       | Let (dec, expr) => Pretty.lett (layoutDec dec, layoutExpT expr)
       | List es => list (Vector.toListMap (es, layoutExpT))
       | Orelse (e, e') =>
	    delimit (mayAlign [layoutExpF e,
			       seq [str "orelse ", layoutExpF e']])
       | Prim {name, ...} => str name
       | Raise exn => delimit (seq [str "raise ", layoutExpF exn])
       | Record r =>
	    let
	       fun layoutTuple es =
		  if 1 = Vector.length es
		     then layoutExp (Vector.sub (es, 0), isDelimited)
		  else tuple (layoutExpsT es)
	    in
	       Record.layout {record = r,
			      separator = " = ",
			      extra = "",
			      layoutTuple = layoutTuple,
			      layoutElt = layoutExpT}
	    end
       | Selector f => seq [str "#", Field.layout f]
       | Seq es => paren (align (separateRight (layoutExpsT es, " ;")))
       | Var {name, fixop} => seq [Fixop.layout fixop, layoutLongvid name]
       | While {test, expr} =>
	    delimit (align [seq [str "while ", layoutExpT test],
			    seq [str "do ", layoutExpT expr]])
   end) arg
and layoutExpsT es = Vector.toListMap (es, layoutExpT)
and layoutExpT e = layoutExp (e, true)
and layoutExpF e = layoutExp (e, false)

and layoutMatch m =
   let
      val Match.T rules = node m
   in
      alignPrefix (Vector.toListMap (rules, layoutRule), "| ")
   end
   
and layoutRule (pat, exp) =
   mayAlign [seq [Pat.layoutF pat, str " =>"],
	     layoutExpF exp]
      
and layoutDec d =
   case node d of
      Abstype {datBind, body} =>
	 align [DatBind.layout ("abstype", datBind),
		seq [str "with ", layoutDec body],
		str "end"]
    | Datatype rhs => DatatypeRhs.layout rhs
    | Exception ebs =>
	 layoutAnds ("exception", Vector.toList ebs,
		     fn (prefix, eb) => seq [prefix, Eb.layout eb])
    | Fix {fixity, ops} =>
	 seq [Fixity.layout fixity, str " ",
	      seq (separate (Vector.toListMap (ops, Vid.layout), " "))]
    | Fun fbs => layoutAndsTyvars ("fun", fbs, layoutFb)
    | Local (d, d') => Pretty.locall (layoutDec d, layoutDec d')
    | Open ss => seq [str "open ",
		      seq (separate (Vector.toListMap (ss, Longstrid.layout),
				     " "))]
    | Overload (p, x, _, t, xs) =>
	 seq [str "_overload ", Priority.layout p, str " ",
	      align [layoutConstraint (Var.layout x, t),
		     layoutAnds ("as", Vector.toList xs, fn (prefix, x) =>
				 seq [prefix, Longvar.layout x])]]
    | SeqDec ds => align (Vector.toListMap (ds, layoutDec))
    | Type typBind => TypBind.layout typBind
    | Val {tyvars, vbs, rvbs} =>
	 align [layoutAndsTyvars ("val", (tyvars, vbs), layoutVb),
		layoutAndsTyvars ("val rec", (tyvars, rvbs), layoutRvb)]

and layoutVb {pat, exp} =
   bind (Pat.layoutT pat, layoutExpT exp)

and layoutRvb {pat, match, ...} =
   bind (Pat.layout pat, seq [str "fn ", layoutMatch match])
   
and layoutFb clauses =
   alignPrefix (Vector.toListMap (clauses, layoutClause), "| ")
   
and layoutClause ({pats, resultType, body}) =
   mayAlign [seq [maybeConstrain (Pat.layoutFlatApp pats,
			    resultType),
	     str " ="],
	 layoutExpF body] (* this has to be layoutExpF in case body
			   is a case expression *)

structure Match =
   struct
      open Match

      val layout = layoutMatch
   end

structure Exp =
   struct
      open Wrap
      type dec = dec
      type match = match
      type t = exp
      datatype node = datatype expNode
      type node' = node
      type obj = t

      fun make n = makeRegion (n, Region.bogus)
      val const = make o Const
      val constraint = make o Constraint
      fun fnn rs =
	 make (Fn (Match.makeRegion (Match.T rs, Region.bogus)))
      val handlee = make o Handle
      val raisee = make o Raise
      val record = make o Record

      fun longvid name = make (Var {name = name, fixop = Fixop.None})
      val var = longvid o Longvid.short o Vid.fromVar

      fun select {tuple, offset}: t =
	 make (App (make (Selector (Field.Int offset)), tuple))

      local
	 fun isLongvid x e =
	    case node e of
	       Var {name=x', ...} => Longvid.equals (x, x')
	     | _ => false
      in
	 val isFalse = isLongvid Longvid.falsee
	 val isTrue = isLongvid Longvid.truee
      end
			    
      fun iff (a: t, b: t, c: t): t =
	 make (if isTrue b then Orelse (a, c)
	      else if isFalse c then Andalso (a, b)
		   else If (a, b, c))
		 
      fun casee (e: t, m: Match.t) =
	 let
	    val Match.T rules = Match.node m
	    val default = make (Case (e, m))
	 in
	    if 2 = Vector.length rules
	       then
		  let
		     val (p0, e0) = Vector.sub (rules, 0)
		     val (p1, e1) = Vector.sub (rules, 1)
		  in
		     if Pat.isTrue p0 andalso Pat.isFalse p1
			then iff (e, e0, e1)
		     else if Pat.isFalse p0 andalso Pat.isTrue p1
			     then iff (e, e1, e0)
			  else default
		  end
	    else default
	 end

      val emptyList: t = make (List (Vector.new0 ()))
	 
      fun con c: t = if Con.equals (c, Con.nill) then emptyList
		      else longvid (Longvid.short (Vid.fromCon c))

      fun app (e1: t, e2: t): t =
	 let
	    val e = makeRegion (App (e1, e2),
				Region.append (region e1, region e2))
	 in
	    case node e1 of
	       Fn m => casee (e2, m)
	     | Var {name = x, ...} =>
		  if Longvid.equals (x, Longvid.cons)
		     then case node e2 of
			Record r =>
			   (case Record.detupleOpt r of
			       SOME v =>
				  if 2 = Vector.length v
				     then
					let
					   val e1 = Vector.sub (v, 0)
					   val es = Vector.sub (v, 1)
					in
					   case node es of
					      List es =>
						 make (List (Vector.cons
							     (e1, es)))
					    | _ => e
					end
				  else e
					    | _ => e)
		      | _ => e
		  else e
		      | _ => e
	 end

      val seq = make o Seq
	 
      fun lett (ds: dec vector, e: t): t =
	 let
	    val es =
	       Vector.keepAllMap
	       (ds, fn d =>
		case node d of
		   Val {tyvars, vbs, rvbs} =>
		      if 1 = Vector.length vbs
			 andalso 0 = Vector.length rvbs
			 then
			    let
			       val {pat, exp, ...} = Vector.sub (vbs, 0)
			    in
			       if Vector.isEmpty tyvars andalso Pat.isWild pat
				  then SOME exp
			       else NONE
			    end
		      else NONE
		 | _ => NONE)
	 in
	    make
	    (if Vector.length ds = Vector.length es
		then Seq (Vector.concat [es, Vector.new1 e])
	     else
		case node e of
		   Let (d, e) => Let (make (SeqDec (Vector.concat
						    [ds, Vector.new1 d])),
				      e)
		 | _ => Let (make (SeqDec ds), e))
	 end

      fun tuple (es: t vector): t =
	 if 1 = Vector.length es
	    then Vector.sub (es, 0)
	 else make (Record (Record.tuple es))

      val unit: t = tuple (Vector.new0 ())

      val layout = layoutExpT
   end

structure Dec =
   struct
      open Wrap
      type t = dec
      datatype node = datatype decNode
      type node' = node
      type obj = t

      fun make n = makeRegion (n, Region.bogus)
	 
      val openn = make o Open

      fun exceptionn (exn: Con.t, to: Type.t option): t =
	 make (Exception (Vector.new1 (exn, make (Eb.Rhs.Gen to))))

      fun datatypee datatypes: t =
	 make
	 (Datatype
	  (DatatypeRhs.makeRegion
	   (DatatypeRhs.DatBind
	    (DatBind.makeRegion (DatBind.T {withtypes = TypBind.empty,
					    datatypes = datatypes},
				 Region.bogus)),
	    Region.bogus)))

      val seq = make o SeqDec
      val empty = seq (Vector.new0 ())

      fun vall (tyvars, var, exp): t =
	 make (Val {tyvars = tyvars,
		    vbs = Vector.new1 {exp = exp, pat = Pat.var var},
		    rvbs = Vector.new0 ()})

      local
	 val it = Var.fromSymbol (Symbol.fromString "it", Region.bogus)
      in
	 fun fromExp (e: Exp.t): t =
	    vall (Vector.new0 (), it, e)
      end
   
      val layout = layoutDec
   end
   
end
