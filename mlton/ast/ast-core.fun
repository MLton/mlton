(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor AstCore (S: AST_CORE_STRUCTS): AST_CORE = 
struct

open S Layout
structure Field = Record.Field
structure Wrap = Region.Wrap

val isInfix': (Vid.t -> bool) ref = ref (fn _ => false)

fun isInfix id = !isInfix' id

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
   let fun nonfixx () = mayAlign [layoutFunc func, layoutArg arg]
   in case getLongvid func of
      SOME x =>
	 if Longvid.isLong x orelse not (isInfix (Longvid.toId x))
	    then nonfixx ()
	 else (case getPair arg of
		  SOME (e1, e2) => mayAlign [layoutArg e1,
				       seq [Longvid.layout x,
					   str " ",
					   layoutArg e2]]
		| NONE => seq [str "op ", Longvid.layout x,
			      str " ", layoutArg arg])
    | _ => nonfixx ()
   end
	 
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
   seq [if not (Longvid.isLong x) andalso isInfix (Longvid.toId x)
(*	  orelse Longvid.toString x = "=" *)
	  then str "op "
       else empty,
       str (let val s = Longvid.toString x
	   in if s = "*" then " * "
	      else if String.isSuffix {string = s, suffix = "*"}
		      then s ^ " "
		   else s
	   end)]

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
	  | App (c, p) =>
	       delimit
	       (layoutApp (c, SOME o Longvid.fromLongcon, Longcon.layout,
			   p,
			   fn p => (case node p of
				       Tuple ps =>
					  if 2 = Vector.length ps
					     then SOME (Vector.sub (ps, 0),
							Vector.sub (ps, 1))
					  else NONE
				     | _ => NONE),
			   layoutF))
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

datatype expNode =
   Var of {name: Longvid.t, fixop: Fixop.t}
  | Fn of rules
  | FlatApp of exp vector
  | App of exp * exp
  | Case of exp * rules
  | Let of dec * exp
  | Seq of exp vector
  | Const of Const.t
  | Record of expNode Wrap.t Record.t (* the Kit barfs on exp Record.t *)
  | List of exp list
  | Selector of Field.t
  | Constraint of exp * Type.t
  | Handle of exp * rules
  | Raise of exp
  | If of exp * exp * exp
  | Andalso of exp * exp
  | Orelse of exp * exp
  | While of {test: exp, expr: exp}
  | Prim of {name: string, ty: Type.t}
  | FFI of {name: string, ty: Type.t}
and decNode =
    Val of {tyvars: Tyvar.t vector,
	    vbs: (Pat.t * exp) vector,
	    rvbs: {var: Var.t,
		   ty: Type.t option,
		   fixity: Vid.t option,
		   rules: rules} vector}
  | Fun of Tyvar.t vector *  {pats: Pat.t vector,
			      resultType: Type.t option,
			      body: exp} vector vector
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
withtype
   exp = expNode Wrap.t
and dec = decNode Wrap.t
and rules = (Pat.t * expNode Wrap.t) vector

open Wrap

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
   let fun delimit t = if isDelimited then t else paren t
   in case node e of
      Var {name, fixop} => seq [Fixop.layout fixop, layoutLongvid name]
    | Fn rs => delimit (seq [str "fn ", layoutRules rs])
    | FlatApp es => seq (separate (Vector.toListMap (es, layoutExpF), " "))
    | App (function, argument) =>
	 delimit
	 (layoutApp (function,
		    fn e => (case node e of
				Var {name, ...} => SOME name
			      | _ => NONE),
		    layoutExpF,
		    argument,
		    fn e => (case node e of
				Record r =>
				   (case Record.detupleOpt r of
				       SOME v =>
					  if 2 = Vector.length v
					     then SOME (Vector.sub (v, 0),
							Vector.sub (v, 1))
					  else NONE
				     | _ => NONE)
			      | _ => NONE),
		    layoutExpF))
    | Case (expr, rules) =>
	 delimit (align [seq [str "case ", layoutExpT expr,
				str " of"],
			    indent (layoutRules rules, 2)])
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
    | Handle (expr, rules) =>
	 delimit (align [layoutExpF expr,
		       seq [str "handle ", layoutRules rules]])
    | Raise e => delimit (seq [str "raise ", layoutExpF e])
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
    | FFI {name, ...} => str name
   end
and layoutExpsT es = Vector.toListMap (es, layoutExpT)
and layoutExpT e = layoutExp (e, true)
and layoutExpF e = layoutExp (e, false)

and layoutRules (rs: rules) =
   alignPrefix (Vector.toListMap (rs, layoutRule), "| ")
   
and layoutRule (pat, exp) =
   mayAlign [seq [Pat.layoutF pat, str " =>"],
	     layoutExpF exp]
      
and layoutDec d =
   case node d of
      Local (d, d') => layoutLocal (layoutDec d, layoutDec d')
    | SeqDec ds => align (Vector.toListMap (ds, layoutDec))
    | Val {tyvars, vbs, rvbs} =>
	 align [layoutAndsTyvars ("val", (tyvars, vbs), layoutVb),
		layoutAndsTyvars ("rec", (Vector.new0 (), rvbs), layoutRvb)]
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

and layoutVb (pat, exp) =
   bind (Pat.layoutT pat, layoutExpT exp)

and layoutRvb {var, rules, ty, ...} =
   bind (maybeConstrain (Var.layout var, ty),
	 seq [str "fn ", layoutRules rules])
   
and layoutFb clauses =
   alignPrefix (Vector.toListMap (clauses, layoutClause), "| ")
   
and layoutClause ({pats, resultType, body}) =
   mayAlign [seq [maybeConstrain (Pat.layoutFlatApp pats,
			    resultType),
	     str " ="],
	 layoutExpF body] (* this has to be layoutExpF in case body
			   is a case expression *)

structure Rules =
   struct
      type t = (Pat.t * exp) vector
   end

structure Exp =
   struct
      open Wrap
      type dec = dec
      type t = exp
      datatype node = datatype expNode
      type node' = node
      type obj = t

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
		 
      fun casee (e: t, rs: (Pat.t * t) vector) =
	 let val default = make (Case (e, rs))
	 in
	    if 2 = Vector.length rs
	       then
		  let
		     val (p0, e0) = Vector.sub (rs, 0)
		     val (p1, e1) = Vector.sub (rs, 1)
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
	 let val e = make (App (e1, e2))
	 in case node e1 of
	    Fn rs => casee (e2, rs)
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
			       val (p, e) = Vector.sub (vbs, 0)
			    in
			       if Vector.isEmpty tyvars andalso Pat.isWild p
				  then SOME e
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

      fun delay (e: t): t = fnn (Vector.new1 (Pat.tuple (Vector.new0 ()), e))
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

      val openn = make o Open

    
      fun funn (tyvars, rvbs): t =
	 make
	 (if Vector.forall (rvbs, fn (_, _, resultTy) =>
			    case resultTy of
			       NONE => true
			     | _ => false)
	     then Fun (tyvars,
		       Vector.map
		       (rvbs, fn (var, rules, _) =>
			let
			   val vp = Pat.longvid (Longvid.short (Vid.fromVar var))
			in Vector.map
			   (rules, fn (pat, exp) =>
			    {pats = Vector.new2 (vp, pat),
			     body = exp,
			     resultType = NONE})
			end))
	  else Val {tyvars = tyvars,
		    vbs = Vector.new0 (),
		    rvbs = Vector.map (rvbs, fn (var, rules, _) =>
				       {var = var,
					fixity = NONE,
					rules = rules,
					ty = NONE})}) (* can't use the resultty *)
	     
      fun exceptionn (exn: Con.t, to: Type.t option): t =
	 make (Exception (Vector.new1 (exn, make (Eb.Rhs.Gen to))))

      fun datatypee datatypes: t =
	 make
	 (Datatype
	  (DatatypeRhs.make
	   (DatatypeRhs.DatBind
	    (DatBind.make (DatBind.T {withtypes = TypBind.empty,
				      datatypes = datatypes})))))

      val seq = make o SeqDec
      val empty = seq (Vector.new0 ())

      fun vall (tyvars, var, exp): t =
	 make (Val {tyvars = tyvars,
		    vbs = Vector.new1 (Pat.var var, exp),
		    rvbs = Vector.new0 ()})

      local
	 val it = Var.fromString "it"
      in
	 fun fromExp (e: Exp.t): t =
	    vall (Vector.new0 (), it, e)
      end
   
      val layout = layoutDec
   end

val isInfix = isInfix'
   
end
