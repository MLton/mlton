(* Copyright (C) 2009,2012,2015,2017 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
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

fun mkCtxt (x, lay) () =
   seq [str "in: ", lay x]

fun layoutConstraint (t, ty) =
   mayAlign [seq [t, str ":"], Type.layout ty]

fun maybeConstrain (e, tyo) =
   case tyo of
      NONE => e
    | SOME ty => layoutConstraint (e, ty)

fun layoutLongvid x =
   str (let val s = Longvid.toString x
        in if s = "*" then " * "
           else if String.hasSuffix (s, {suffix = "*"})
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
         App of Longcon.t * t
       | Const of Const.t
       | Constraint of t * Type.t
       | FlatApp of t vector
       | Layered of {fixop: Fixop.t,
                     var: Var.t,
                     constraint: Type.t option,
                     pat: t}
       | List of t vector
       | Or of t vector
       | Paren of t
       | Record of {flexible: bool,
                    items: (Record.Field.t * Region.t * item) vector}
       | Tuple of t vector
       | Var of {fixop: Fixop.t, name: Longvid.t}
       | Vector of t vector
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
      val constraint = make o Constraint
      val layered = make o Layered

      fun longvid x = make (Var {name = x, fixop = Fixop.None})
      val var = longvid o Longvid.short o Vid.fromVar

      fun tuple ps =
         if 1 = Vector.length ps
            then Vector.first ps
         else makeRegion (Tuple ps,
                          Region.append
                          (region (Vector.first ps),
                           region (Vector.last ps)))

      fun layout (p, isDelimited) =
         let
            fun delimit t = if isDelimited then t else paren t
         in
            case node p of
               App (c, p) => delimit (mayAlign [Longcon.layout c,
                                                layoutF p])
             | Const c => Const.layout c
             | Constraint (p, t) => delimit (layoutConstraint (layoutF p, t))
             | FlatApp ps =>
                  if Vector.length ps = 1
                     then layout (Vector.first ps, isDelimited)
                  else delimit (layoutFlatApp ps)
             | Layered {fixop, var, constraint, pat} =>
                  delimit
                  (mayAlign [maybeConstrain
                             (seq [Fixop.layout fixop, Var.layout var],
                              constraint),
                             seq [str "as ", layoutT pat]])
             | List ps => list (Vector.toListMap (ps, layoutT))
             | Or ps =>
                  delimit
                  (mayAlign (separateLeft (Vector.toListMap (ps, layoutT), "| ")))
             | Paren p => layout (p, isDelimited)
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
             | Tuple ps => Layout.tuple (Vector.toListMap (ps, layoutT))
             | Var {name, fixop} => seq [Fixop.layout fixop, layoutLongvid name]
             | Vector ps => vector (Vector.map (ps, layoutT))
             | Wild => str "_"
         end
      and layoutF p = layout (p, false)
      and layoutT p = layout (p, true)
      and layoutFlatApp ps = seq (separate (Vector.toListMap (ps, layoutF), " "))
      and layoutItem (f, _, i) =
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

      val layout = layoutT

      fun checkSyntax (p: t): unit =
         let
            val c = checkSyntax
         in
            case node p of
               App (_, p) => c p
             | Const _ => ()
             | Constraint (p, t) => (c p; Type.checkSyntax t)
             | FlatApp ps => Vector.foreach (ps, c)
             | Layered {constraint, pat, ...} =>
                  (c pat; Option.app (constraint, Type.checkSyntax))
             | List ps => Vector.foreach (ps, c)
             | Paren p => c p
             | Or ps => Vector.foreach (ps, c)
             | Record {items, ...} =>
                  (reportDuplicateFields (Vector.map (items, fn (f, r, i) => (f, (r, i))),
                                          {ctxt = mkCtxt (p, layout)})
                   ; Vector.foreach (items, fn (_, _, i) =>
                                     case i of
                                        Item.Field p => c p
                                      | Item.Vid (_, to, po) =>
                                           (Option.app (to, Type.checkSyntax)
                                            ; Option.app (po, c))))
             | Tuple ps => Vector.foreach (ps, c)
             | Var _ => ()
             | Vector ps => Vector.foreach (ps, c)
             | Wild => ()
         end
   end

structure Eb =
   struct
      structure Rhs =
         struct
            open Wrap
            datatype node =
               Def of Longcon.t
             | Gen of Type.t option
            type t = node Wrap.t
            type node' = node
            type obj = t

            fun layout rhs =
               case node rhs of
                  Def c => seq [str " = ", Longcon.layout c]
                | Gen to => Type.layoutOption to

            fun checkSyntax (e: t): unit =
               case node e of
                  Def _ => ()
                | Gen to => Option.app (to, Type.checkSyntax)
         end

      type t = Con.t * Rhs.t

      fun layout (exn, rhs) =
         seq [Con.layout exn, Rhs.layout rhs]
   end

structure EbRhs = Eb.Rhs

structure PrimKind =
   struct
      structure ImportExportAttribute =
         struct
            datatype t = Cdecl | External | Impure | Private | Public | Pure | Reentrant | Runtime | Stdcall

            val toString: t -> string =
               fn Cdecl => "cdecl"
                | External => "external"
                | Impure => "impure"
                | Private => "private"
                | Public => "public"
                | Pure => "pure"
                | Reentrant => "reentrant"
                | Runtime => "runtime"
                | Stdcall => "stdcall"

            val layout = Layout.str o toString
         end

      structure SymbolAttribute =
         struct
            datatype t = Alloc | External | Private | Public

            val toString: t -> string =
               fn Alloc => "alloc"
                | External => "external"
                | Private => "private"
                | Public => "public"

            val layout = Layout.str o toString
         end

      datatype t =
         Address of {attributes: SymbolAttribute.t list,
                     name: string,
                     ty: Type.t}
       | BuildConst of {name: string, 
                        ty: Type.t}
       | CommandLineConst of {name: string, 
                              ty: Type.t,
                              value: Const.t}
       | Const of {name: string, 
                   ty: Type.t}
       | Export of {attributes: ImportExportAttribute.t list, 
                    name: string,
                    ty: Type.t}
       | IImport of {attributes: ImportExportAttribute.t list,
                     ty: Type.t}
       | Import of {attributes: ImportExportAttribute.t list, 
                    name: string,
                    ty: Type.t}
       | ISymbol of {ty: Type.t}
       | Prim of {name: string, 
                  ty: Type.t}
       | Symbol of {attributes: SymbolAttribute.t list, 
                    name: string,
                    ty: Type.t}

      fun name pk =
         case pk of
            Address {name, ...} => name
          | BuildConst {name, ...} => name
          | CommandLineConst {name, ...} => name
          | Const {name, ...} => name
          | Export {name, ...} => name
          | IImport {...} => "<iimport>"
          | Import {name, ...} => name
          | ISymbol {...} => "<isymbol>"
          | Prim {name, ...} => name
          | Symbol {name, ...} => name
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
    Andalso of exp * exp
  | App of exp * exp
  | Case of exp * match
  | Const of Const.t
  | Constraint of exp * Type.t
  | FlatApp of exp vector
  | Fn of match
  | Handle of exp * match
  | If of exp * exp * exp
  | Let of dec * exp
  | List of exp vector
  | Orelse of exp * exp
  | Paren of exp
  | Prim of PrimKind.t
  | Raise of exp
  | Record of (Region.t * exp) Record.t
  | Selector of Field.t
  | Seq of exp vector
  | Var of {name: Longvid.t, fixop: Fixop.t}
  | Vector of exp vector
  | While of {test: exp, expr: exp}
and decNode =
    Abstype of {body: dec,
                datBind: DatBind.t}
  | Datatype of DatatypeRhs.t
  | DoDec of exp
  | Exception of Eb.t vector
  | Fix of {fixity: Fixity.t,
            ops: Vid.t vector}
  | Fun of {tyvars: Tyvar.t vector,
            fbs: {body: exp,
                  pats: Pat.t vector,
                  resultType: Type.t option} vector vector}
  | Local of dec * dec
  | Open of Longstrid.t vector
  | Overload of Priority.t * Var.t * 
                Tyvar.t vector * Type.t * 
                Longvid.t vector
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

fun layoutTyvarsAndsSusp (prefix, (tyvars, xs), layoutX) =
   layoutAndsSusp
   (prefix, xs, fn (first, prefix, x) =>
    if first andalso not (Vector.isEmpty tyvars)
       then seq [prefix,
                 case Vector.length tyvars of
                    1 => Tyvar.layout (Vector.sub (tyvars, 0))
                  | _ => Layout.tuple (Vector.toListMap (tyvars, Tyvar.layout)),
                 str " ",
                 layoutX x]
       else seq [prefix, layoutX x])

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
    | Paren _ => "Paren"
    | Prim _ => "Prim"
    | Raise _ => "Raise"
    | Record _ => "Record"
    | Selector _ => "Selector"
    | Seq _ => "Seq"
    | Var _ => "Var"
    | Vector _ => "Vector"
    | While _ => "While"

val traceLayoutExp =
   Trace.traceInfo' (Trace.info "AstCore.layoutExp",
                     fn (e, _: bool) => Layout.str (expNodeName e),
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
            if Vector.length es = 1
               then layoutExp (Vector.first es, isDelimited)
            else delimit (seq (separate (Vector.toListMap (es, layoutExpF), " ")))
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
       | Paren e => layoutExp (e, isDelimited)
       | Prim kind => str (PrimKind.name kind)
       | Raise exn => delimit (seq [str "raise ", layoutExpF exn])
       | Record r =>
            let
               fun layoutTuple es =
                  if 1 = Vector.length es
                     then layoutExp (Vector.first es, isDelimited)
                  else tuple (layoutExpsT es)
            in
               Record.layout {record = r,
                              separator = " = ",
                              extra = "",
                              layoutTuple = fn res => layoutTuple (Vector.map (res, #2)),
                              layoutElt = layoutExpT o #2}
            end
       | Selector f => seq [str "#", Field.layout f]
       | Seq es => paren (align (separateRight (layoutExpsT es, " ;")))
       | Var {name, fixop} => seq [Fixop.layout fixop, layoutLongvid name]
       | Vector es => vector (Vector.map (es, layoutExpT))
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
   mayAlign [seq [Pat.layoutT pat, str " =>"],
             layoutExpF exp]

and layoutDec d =
   case node d of
      Abstype {datBind, body} =>
         align [DatBind.layout ("abstype", datBind),
                seq [str "with ", layoutDec body],
                str "end"]
    | Datatype rhs => DatatypeRhs.layout rhs
    | DoDec exp => seq [str "do ", layoutExpT exp]
    | Exception ebs =>
         layoutAnds ("exception", ebs,
                     fn (prefix, eb) => seq [prefix, Eb.layout eb])
    | Fix {fixity, ops} =>
         seq [Fixity.layout fixity, str " ",
              seq (separate (Vector.toListMap (ops, Vid.layout), " "))]
    | Fun {tyvars, fbs} =>
         let
            val fbs = layoutFun {tyvars = tyvars, fbs = fbs}
         in
            align (Vector.toListMap (fbs, fn th => th ()))
         end
    | Local (d, d') => Pretty.locall (layoutDec d, layoutDec d')
    | Open ss => seq [str "open ",
                      seq (separate (Vector.toListMap (ss, Longstrid.layout),
                                     " "))]
    | Overload (p, x, _, t, xs) =>
         seq [str "_overload ", Priority.layout p, str " ",
              align [layoutConstraint (Var.layout x, t),
                     layoutAnds ("as", xs, fn (prefix, x) =>
                                 seq [prefix, Longvid.layout x])]]
    | SeqDec ds => align (Vector.toListMap (ds, layoutDec))
    | Type typBind => TypBind.layout typBind
    | Val {tyvars, vbs, rvbs} =>
         let
            val {vbs, rvbs} =
               layoutVal {tyvars = tyvars, vbs = vbs, rvbs = rvbs}
         in
            align [align (Vector.toListMap (vbs, fn th => th ())),
                   align (Vector.toListMap (rvbs, fn th => th ()))]
         end

and layoutFun {tyvars, fbs} =
   layoutTyvarsAndsSusp ("fun", (tyvars, fbs), layoutFb)

and layoutFb clauses =
   alignPrefix (Vector.toListMap (clauses, layoutClause), "| ")

and layoutClause ({pats, resultType, body}) =
   mayAlign [seq [maybeConstrain (Pat.layoutFlatApp pats,
                                  resultType),
                  str " ="],
             layoutExpF body] (* this has to be layoutExpF in case body
                                 is a case expression *)

and layoutVal {tyvars, vbs, rvbs} =
   if Vector.isEmpty rvbs
      then {vbs = layoutTyvarsAndsSusp ("val", (tyvars, vbs), layoutVb),
            rvbs = Vector.new0 ()}
   else if Vector.isEmpty vbs
      then {vbs = Vector.new0 (),
            rvbs = layoutTyvarsAndsSusp ("val rec", (tyvars, rvbs), layoutRvb)}
   else {vbs = layoutTyvarsAndsSusp ("val", (tyvars, vbs), layoutVb),
         rvbs = layoutTyvarsAndsSusp ("and rec", (Vector.new0 (), rvbs), layoutRvb)}

and layoutVb {pat, exp} =
   bind (Pat.layoutT pat, layoutExpT exp)

and layoutRvb {pat, match, ...} =
   bind (Pat.layout pat, seq [str "fn ", layoutMatch match])

fun checkSyntaxExp (e: exp): unit =
   let
      val c = checkSyntaxExp
   in
      case node e of
         Andalso (e1, e2) => (c e1; c e2)
       | App (e1, e2) => (c e1; c e2)
       | Case (e, m) => (c e; checkSyntaxMatch m)
       | Const _ => ()
       | Constraint (e, t) => (c e; Type.checkSyntax t)
       | FlatApp es => Vector.foreach (es, c)
       | Fn m => checkSyntaxMatch m
       | Handle (e, m) => (c e; checkSyntaxMatch m)
       | If (e1, e2, e3) => (c e1; c e2; c e3)
       | Let (d, e) => (checkSyntaxDec d; c e)
       | List es => Vector.foreach (es, c)
       | Orelse (e1, e2) => (c e1; c e2)
       | Paren e => c e
       | Prim _ => ()
       | Raise e => c e
       | Record r =>
            (reportDuplicateFields (Record.toVector r,
                                    {ctxt = mkCtxt (e, layoutExpT)})
             ; Record.foreach (r, c o #2))
       | Selector _ => ()
       | Seq es => Vector.foreach (es, c)
       | Var _ => ()
       | Vector es => Vector.foreach (es, c)
       | While {expr, test} => (c expr; c test)
   end

and checkSyntaxMatch (m: match): unit =
   let
      val T v = node m
   in
      Vector.foreach (v, fn (p, e) => (Pat.checkSyntax p; checkSyntaxExp e))
   end

and checkSyntaxDec (d: dec): unit =
   case node d of
      Abstype {datBind, body} =>
         (DatBind.checkSyntaxDef datBind
          ; checkSyntaxDec body)
    | Datatype rhs => DatatypeRhs.checkSyntaxDef rhs
    | DoDec exp => checkSyntaxExp exp
    | Exception v =>
         (Vector.foreach
          (v, fn (con, ebrhs) =>
           (Vid.checkRedefineSpecial
            (Vid.fromCon con,
             {allowIt = false,
              ctxt = mkCtxt (d, layoutDec),
              keyword = "exception"})
            ; EbRhs.checkSyntax ebrhs))
          ; (reportDuplicates
             (v, {ctxt = mkCtxt (d, layoutDec),
                  equals = fn ((c, _), (c', _)) => Con.equals (c, c'),
                  layout = Con.layout o #1,
                  name = "exception definition",
                  region = Con.region o #1})))
    | Fix _ => () (* The Definition allows, e.g., "infix + +". *)
    | Fun {tyvars, fbs, ...} =>
         (reportDuplicateTyvars (tyvars,
                                 {ctxt = mkCtxt (d, layoutDec)})
          ; Vector.foreach (fbs, fn clauses =>
                            Vector.foreach
                            (clauses, fn {body, pats, resultType} =>
                             (checkSyntaxExp body
                              ; Vector.foreach (pats, Pat.checkSyntax)
                              ; Option.app (resultType, Type.checkSyntax)))))
    | Local (d, d') => (checkSyntaxDec d; checkSyntaxDec d')
    | Open _ => ()
    | Overload (_, _, _, ty, _) => Type.checkSyntax ty
    | SeqDec v => Vector.foreach (v, checkSyntaxDec)
    | Type b => TypBind.checkSyntaxDef b
    | Val {tyvars, rvbs, vbs, ...} =>
         (reportDuplicateTyvars (tyvars,
                                 {ctxt = mkCtxt (d, layoutDec)})
          ; Vector.foreach (rvbs, fn {match, pat} =>
                            (checkSyntaxMatch match
                             ; Pat.checkSyntax pat))
          ; Vector.foreach (vbs, fn {exp, pat} =>
                            (checkSyntaxExp exp
                             ; Pat.checkSyntax pat)))

structure Exp =
   struct
      open Wrap
      type dec = dec
      type match = match
      type t = exp
      datatype node = datatype expNode
      type node' = node
      type obj = t

      fun const c = makeRegion (Const c, Const.region c)

      fun constraint (e, t) = makeRegion (Constraint (e, t), region e)

      fun fnn rs =
         let
            val r =
               if Vector.isEmpty rs
                  then Region.bogus
               else Region.append (Pat.region (#1 (Vector.first rs)),
                                   region (#2 (Vector.last rs)))
         in
            makeRegion (Fn (Match.makeRegion (Match.T rs, r)), r)
         end

      fun longvid name =
         makeRegion (Var {name = name, fixop = Fixop.None},
                     Longvid.region name)

      val var = longvid o Longvid.short o Vid.fromVar

      fun app (e1: t, e2: t): t =
         makeRegion (App (e1, e2),
                     Region.append (region e1, region e2))

      fun lett (ds: dec vector, e: t, r: Region.t): t =
         makeRegion (Let (makeRegion (SeqDec ds, r), e), r)

      fun tuple (es: t vector): t =
         if 1 = Vector.length es
            then Vector.first es
         else
            let
               val r =
                  if Vector.isEmpty es
                     then Region.bogus
                  else Region.append (region (Vector.first es),
                                      region (Vector.last es))
               val res =
                  Vector.map (es, fn e => (Region.bogus, e))
            in
               makeRegion (Record (Record.tuple res), r)
            end

      val unit: t = tuple (Vector.new0 ())

      val layout = layoutExpT
   end

structure Match =
   struct
      open Match
      val layout = layoutMatch
      val layoutRule = layoutRule
   end

structure Dec =
   struct
      open Wrap
      type t = dec
      datatype node = datatype decNode
      type node' = node
      type obj = t

      val checkSyntax = checkSyntaxDec

      fun make n = makeRegion (n, Region.bogus)

      val openn = make o Open

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
      val layoutFun = layoutFun
      val layoutVal = layoutVal
   end

end
