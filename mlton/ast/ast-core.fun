(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
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

fun nest (prefix, x, y) =
   align [seq [str prefix, x],
	      seq [str "in ", y],
	      str "end"]

fun layoutLet (d, e) = nest ("let ", d, e)
fun layoutLocal (d, d') = nest ("local ", d, d')

fun layoutLongvid x =
   str (let val s = Longvid.toString x
	in if s = "*" then " * "
	   else if String.isSuffix {string = s, suffix = "*"}
		   then s ^ " "
		else s
	end)

(*---------------------------------------------------*)
(*                     Patterns                      *)
(*---------------------------------------------------*)

structure Pat =
   struct
      open Wrap
      datatype node =
	 Wild
       | Var of {fixop: Fixop.t, name: Longvid.t}
       | Const of Const.t
       | Tuple of t vector
       | Record of {items: item vector,
		    flexible: bool}
       | List of t list
       | FlatApp of t vector
       | App of Longcon.t * t
       | Constraint of t * Type.t
       | Layered of {fixop: Fixop.t,
		     var: Var.t,
		     constraint: Type.t option,
		     pat: t}
      and item =
	 Field of Record.Field.t * t
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

      val emptyList = make (List [])

      fun longvid x = make (Var {name = x, fixop = Fixop.None})
      val var = longvid o Longvid.short o Vid.fromVar

      fun con c =
	 if Con.equals (c, Con.nill) then emptyList
	 else longvid (Longvid.short (Vid.fromCon c))
		     
      fun app (c, p) =
	 let val default = make (App (Longcon.short c, p))
	 in if Con.equals (c, Con.cons)
	       then (case node p of
			Tuple ps =>
			   if 2 = Vector.length ps
			      then
				 let
				    val p0 = Vector.sub (ps, 0)
				    val p1 = Vector.sub (ps, 1)
				 in case node p1 of
				     List ps => make (List (p0 :: ps))
				   | _ => default
				 end
			   else default
		      | _ => default)
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
	 let fun delimit t = if isDelimited then t else paren t
	 in case node p of
	    Wild => str "_"
	  | Var {name, fixop} => seq [Fixop.layout fixop, layoutLongvid name]
	  | Const c => Const.layout c
	  | Tuple ps => Vector.layout layoutT ps
	  | Record {items, flexible} =>
	       seq [str "{",
		   mayAlign (separateRight (Vector.toListMap (items, layoutItem),
					    ", ")),
		   if flexible
		      then str (if Vector.isEmpty items then "..." else ", ...")
		   else empty,
		   str "}"]
	  | List ps => Layout.list (List.map (ps, layoutT))
	  | FlatApp ps => delimit (layoutFlatApp ps)
	  | App (c, p) => delimit (mayAlign [Longcon.layout c,
					     layoutF p])
	  | Constraint (p, t) => delimit (layoutConstraint (layoutF p, t))
	  | Layered {fixop, var, constraint, pat} =>
	       delimit
	       (mayAlign [maybeConstrain
			  (seq [Fixop.layout fixop, Var.layout var],
			   constraint),
		      seq [str "as ", layoutT pat]])
	 end
      and layoutF p = layout (p, false)
      and layoutT p = layout (p, true)
      and layoutFlatApp ps = seq (separate (Vector.toListMap (ps, layoutF), " "))
      and layoutItem i =
	 case i of
	    Field (f, p) => seq [Field.layout f, str " = ", layoutT p]
	  | Vid (vid, tyo, po) =>
	       seq [Vid.layout vid,
		    case tyo of
		       NONE => empty
		     | SOME ty => seq [str ": ", Type.layout ty],
			  case po of
			     NONE => empty
			   | SOME p => seq [str " as ", layoutT p]]

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
      datatype t = BuildConst | Const | FFI | Prim
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
  | List of exp list
  | Selector of Field.t
  | Constraint of exp * Type.t
  | Handle of exp * match
  | Raise of {exn: exp, filePos: string}
  | If of exp * exp * exp
  | Andalso of exp * exp
  | Orelse of exp * exp
  | While of {test: exp, expr: exp}
  | Prim of {kind: PrimKind.t,
	     name: string,
	     ty: Type.t}
and decNode =
    Val of {tyvars: Tyvar.t vector,
	    vbs: {pat: Pat.t,
		  exp: exp,
		  filePos: string} vector,
	    rvbs: {pat: Pat.t,
		   match: match} vector}
  | Fun of Tyvar.t vector * {clauses: {pats: Pat.t vector,
				       resultType: Type.t option,
				       body: exp} vector,
			     filePos: string} vector
  | Type of TypBind.t
  | Datatype of DatatypeRhs.t
  | Abstype of {datBind: DatBind.t,
		body: dec}
  | Exception of Eb.t vector
  | Local of dec * dec
  | SeqDec of dec vector
  | Open of Longstrid.t vector
  | Overload of Var.t * Type.t * Longvar.t vector
  | Fix of {fixity: Fixity.t,
	    ops: Vid.t vector}
and match = T of {filePos: string,
		  rules: (Pat.t * exp) vector}
withtype
   exp = expNode Wrap.t
and dec = decNode Wrap.t

open Wrap

structure Match =
   struct
      datatype t = datatype match
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

fun layoutExp (e, isDelimited) =
   let
      fun delimit t = if isDelimited then t else paren t
   in
      case node e of
	 Var {name, fixop} => seq [Fixop.layout fixop, layoutLongvid name]
       | Fn m => delimit (seq [str "fn ", layoutMatch m])
       | FlatApp es => seq (separate (Vector.toListMap (es, layoutExpF), " "))
       | App (function, argument) =>
	    delimit (mayAlign [layoutExpF function, layoutExpF argument])
       | Case (expr, match) =>
	    delimit (align [seq [str "case ", layoutExpT expr,
				 str " of"],
			    indent (layoutMatch match, 2)])
       | Let (dec, expr) => layoutLet (layoutDec dec, layoutExpT expr)
       | Seq es => paren (align (separateRight (layoutExpsT es, " ;")))
       | Const c => Const.layout c
       | Record r =>
	    let
	       fun layoutTuple es =
		  if 1 = Vector.length es
		     then layoutExp (Vector.sub (es, 0), isDelimited)
		  else tuple (layoutExpsT es)
	    in
	       Record.layout {record = r,
			      separator = " =",
			      extra = "",
			      layoutTuple = layoutTuple,
			      layoutElt = layoutExpT}
	    end
       | List es => list (List.map (es, layoutExpT))
       | Selector f => seq [str "#", Field.layout f]
       | Constraint (expr, constraint) =>
	    delimit (layoutConstraint (layoutExpF expr, constraint))
       | Handle (try, match) =>
	    delimit (align [layoutExpF try,
			    seq [str "handle ", layoutMatch match]])
       | Raise {exn, ...} => delimit (seq [str "raise ", layoutExpF exn])
       | If (test, thenCase, elseCase) =>
	    delimit (mayAlign [seq [str "if ", layoutExpT test],
			       seq [str "then ", layoutExpT thenCase],
			       seq [str "else ", layoutExpT elseCase]])
       | Andalso (e, e') =>
	    delimit (mayAlign [layoutExpF e,
			       seq [str "andalso ", layoutExpF e']])
       | Orelse (e, e') =>
	    delimit (mayAlign [layoutExpF e,
			       seq [str "orelse ", layoutExpF e']])
       | While {test, expr} =>
	    delimit (align [seq [str "while ", layoutExpT test],
			    seq [str "do ", layoutExpT expr]])
       | Prim {name, ...} => str name
   end
and layoutExpsT es = Vector.toListMap (es, layoutExpT)
and layoutExpT e = layoutExp (e, true)
and layoutExpF e = layoutExp (e, false)

and layoutMatch (Match.T {rules, ...}) =
   alignPrefix (Vector.toListMap (rules, layoutRule), "| ")
   
and layoutRule (pat, exp) =
   mayAlign [seq [Pat.layoutF pat, str " =>"],
	     layoutExpF exp]
      
and layoutDec d =
   case node d of
      Local (d, d') => layoutLocal (layoutDec d, layoutDec d')
    | SeqDec ds => align (Vector.toListMap (ds, layoutDec))
    | Val {tyvars, vbs, rvbs} =>
	 align [layoutAndsTyvars ("val", (tyvars, vbs), layoutVb),
		layoutAndsTyvars ("val rec", (Vector.new0 (), rvbs), layoutRvb)]
    | Fun fbs => layoutAndsTyvars ("fun", fbs, layoutFb)
    | Type typBind => TypBind.layout typBind
    | Datatype rhs => DatatypeRhs.layout rhs
    | Abstype {datBind, body} =>
	 align [DatBind.layout ("abstype", datBind),
		seq [str "with ", layoutDec body],
		str "end"]
    | Exception ebs =>
	 layoutAnds ("exception", Vector.toList ebs,
		     fn (prefix, eb) => seq [prefix, Eb.layout eb])
    | Open ss => seq [str "open ",
		      seq (separate (Vector.toListMap (ss, Longstrid.layout),
				     " "))]
    | Overload (x, t, xs) =>
	 seq [str "_overload ",
	      align [layoutConstraint (Var.layout x, t),
		     layoutAnds ("as", Vector.toList xs, fn (prefix, x) =>
				 seq [prefix, Longvar.layout x])]]
    | Fix {fixity, ops} =>
	 seq [Fixity.layout fixity, str " ",
	      seq (separate (Vector.toListMap (ops, Vid.layout), " "))]

and layoutVb {pat, exp, filePos} =
   bind (Pat.layoutT pat, layoutExpT exp)

and layoutRvb {pat, match, ...} =
   bind (Pat.layout pat, seq [str "fn ", layoutMatch match])
   
and layoutFb {clauses, filePos} =
   alignPrefix (Vector.toListMap (clauses, layoutClause), "| ")
   
and layoutClause ({pats, resultType, body}) =
   mayAlign [seq [maybeConstrain (Pat.layoutFlatApp pats,
			    resultType),
	     str " ="],
	 layoutExpF body] (* this has to be layoutExpF in case body
			   is a case expression *)

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
      val fnn = make o Fn
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
      in val isTrue = isLongvid Longvid.truee
	 val isFalse = isLongvid Longvid.falsee
      end
   
      fun iff (a: t, b: t, c: t): t =
	 make (if isTrue b then Orelse (a, c)
	      else if isFalse c then Andalso (a, b)
		   else If (a, b, c))
		 
      fun casee (e: t, m as Match.T {rules, ...}) =
	 let val default = make (Case (e, m))
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

      val emptyList: t = make (List [])
	 
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
					      List es => make (List (e1 :: es))
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

      fun delay (e: t): t =
	 fnn (Match.T {rules = Vector.new1 (Pat.tuple (Vector.new0 ()), e),
		       filePos = ""})
(*	 
 *      val handleFunc =
 *	 let val e = Var.fromString "e"
 *	    val f = Var.fromString "f"
 *	    val x = Var.fromString "x"
 *	 in fnn (rules [(Pat.tuple [Pat.var e, Pat.var f],
 *		       make (Handle (app (var e, unit),
 *				   rules [(Pat.var x, app (var f, var x))])))])
 *	 end
 *)
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

      fun funn (tyvars, rvbs): t =
	 make
	 (Fun (tyvars,
	       Vector.map
	       (rvbs, fn {var,
			  match = Match.T {rules, filePos},
			  resultTy} =>
		let
		   val vp = Pat.longvid (Longvid.short (Vid.fromVar var))
		in
		   {clauses =
		    Vector.map (rules, fn (pat, exp) =>
				{pats = Vector.new2 (vp, pat),
				 body = exp,
				 resultType = NONE}),
		    filePos = filePos}
		end)))
	     
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
		    vbs = Vector.new1 {pat = Pat.var var, exp = exp,
				       filePos = ""},
		    rvbs = Vector.new0 ()})

      local
	 val it = Var.fromString ("it", Region.bogus)
      in
	 fun fromExp (e: Exp.t): t =
	    vall (Vector.new0 (), it, e)
      end
   
      val layout = layoutDec
   end
   
end
