(* Copyright (C) 2009,2012,2015 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
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
      val constraint = make o Constraint
      val layered = make o Layered

      fun longvid x = make (Var {name = x, fixop = Fixop.None})
      val var = longvid o Longvid.short o Vid.fromVar

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
             | FlatApp ps =>
                  if Vector.length ps = 1
                     then layout (Vector.sub (ps, 0), isDelimited)
                  else delimit (layoutFlatApp ps)
             | Layered {fixop, var, constraint, pat} =>
                  delimit
                  (mayAlign [maybeConstrain
                             (seq [Fixop.layout fixop, Var.layout var],
                              constraint),
                             seq [str "as ", layoutT pat]])
             | List ps => list (Vector.toListMap (ps, layoutT))
             | Or ps => seq [str "(",
                             mayAlign (separate 
                                       (Vector.toListMap (ps, layoutF), "|")),
                             str ")"]
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
             | Or ps => Vector.foreach (ps, c)
             | Record {items, ...} =>
                  (Vector.foreach (items, fn (_, i) =>
                                   case i of
                                      Item.Field p => c p
                                    | Item.Vid (_, to, po) =>
                                         (Option.app (to, Type.checkSyntax)
                                          ; Option.app (po, c)))
                   ; reportDuplicateFields (items,
                                            {region = region p,
                                             term = fn () => layout p}))
             | Tuple ps => Vector.foreach (ps, c)
             | Var _ => ()
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
  | Prim of PrimKind.t
and decNode =
   Abstype of {body: dec,
               datBind: DatBind.t}
  | DoDec of exp
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

fun layoutAndsTyvars (prefix, (tyvars, xs), layoutX) =
   layoutAnds (prefix,
               Vector.fromList
               (case Vector.toListMap (xs, layoutX) of
                   [] => []
                 | x :: xs =>
                      (if Vector.isEmpty tyvars
                          then x
                       else seq [Tyvar.layouts tyvars, str " ", x]) :: xs),
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
               then layoutExp (Vector.sub (es, 0), isDelimited)
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
       | Prim kind => str (PrimKind.name kind)
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
    | DoDec exp => layoutExpT exp
    | Exception ebs =>
         layoutAnds ("exception", ebs,
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
                     layoutAnds ("as", xs, fn (prefix, x) =>
                                 seq [prefix, Longvid.layout x])]]
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
       | Prim _ => ()
       | Raise e => c e
       | Record r =>
            (Record.foreach (r, c)
             ; reportDuplicateFields (Record.toVector r,
                                      {region = region e,
                                       term = fn () => layoutExp (e, true)}))
       | Selector _ => ()
       | Seq es => Vector.foreach (es, c)
       | Var _ => ()
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
         (DatBind.checkSyntax datBind
          ; checkSyntaxDec body)
    | Datatype rhs => DatatypeRhs.checkSyntax rhs
    | DoDec exp => checkSyntaxExp exp
    | Exception v =>
         (Vector.foreach (v, fn (_, ebrhs) => EbRhs.checkSyntax ebrhs)
          ; (reportDuplicates
             (v, {equals = fn ((c, _), (c', _)) => Con.equals (c, c'),
                  layout = Con.layout o #1,
                  name = "exception declaration",
                  region = Con.region o #1,
                  term = fn () => layoutDec d})))
    | Fix _ => () (* The Definition allows, e.g., "infix + +". *)
    | Fun (_, fs) =>
         Vector.foreach (fs, fn clauses =>
                         Vector.foreach
                         (clauses, fn {body, pats, resultType} =>
                          (checkSyntaxExp body
                           ; Vector.foreach (pats, Pat.checkSyntax)
                           ; Option.app (resultType, Type.checkSyntax))))
    | Local (d, d') => (checkSyntaxDec d; checkSyntaxDec d')
    | Open _ => ()
    | Overload (_, _, _, ty, _) => Type.checkSyntax ty
    | SeqDec v => Vector.foreach (v, checkSyntaxDec)
    | Type b => TypBind.checkSyntax b
    | Val {rvbs, vbs, ...} =>
         (Vector.foreach (rvbs, fn {match, pat} =>
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
               if 0 = Vector.length rs
                  then Region.bogus
               else Region.append (Pat.region (#1 (Vector.sub (rs, 0))),
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
            then Vector.sub (es, 0)
         else
            let
               val r =
                  if 0 = Vector.length es
                     then Region.bogus
                  else Region.append (region (Vector.sub (es, 0)),
                                      region (Vector.last es))
            in
               makeRegion (Record (Record.tuple es), r)
            end

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
   end

end
