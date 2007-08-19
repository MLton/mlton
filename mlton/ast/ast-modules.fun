(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor AstModules (S: AST_MODULES_STRUCTS): AST_MODULES = 
struct

open S

structure AstCore = AstCore (AstAtoms (S))

open AstCore Layout

val layouts = List.map
structure Wrap = Region.Wrap
val node = Wrap.node

structure Equation =
   struct
      open Wrap
      datatype node =
         Type of Longtycon.t list
       | Structure of Longstrid.t list
      type t = node Wrap.t
      type node' = node
      type obj = t

      fun layout eq =
         case node eq of
            Type longtycons =>
               seq (str "sharing type "
                    :: separate (List.map (longtycons, Longtycon.layout), " = "))
          | Structure longstrids =>
               seq (str "sharing "
                    :: separate (List.map (longstrids, Longstrid.layout), " = "))
   end

type typedescs = {tyvars: Tyvar.t vector,
                  tycon: Tycon.t} vector

datatype sigexpNode =
   Var of Sigid.t
 | Where of sigexp * {tyvars: Tyvar.t vector,
                      longtycon: Longtycon.t,
                      ty: Type.t} vector
 | Spec of spec
and sigConst =
   None
  | Transparent of sigexp
  | Opaque of sigexp
and specNode =
   Datatype of DatatypeRhs.t
  | Empty
  | Eqtype of typedescs
  | Exception of (Con.t * Type.t option) vector
  | IncludeSigexp of sigexp
  | IncludeSigids of Sigid.t vector
  | Seq of spec * spec
  | Sharing of {equations: Equation.t vector,
                spec: spec}
  | Structure of (Strid.t * sigexp) vector
  | Type of typedescs
  | TypeDefs of TypBind.t
  | Val of (Var.t * Type.t) vector
withtype spec = specNode Wrap.t
and sigexp = sigexpNode Wrap.t

fun layoutTypedescs (prefix, typedescs) =
   layoutAnds (prefix, typedescs, fn (prefix, {tyvars, tycon}) =>
               seq [prefix,
                    Type.layoutApp (Tycon.layout tycon, tyvars, Tyvar.layout)])

fun layoutTypedefs (prefix, typBind) =
   let
      val TypBind.T ds = TypBind.node typBind
   in
      layoutAnds (prefix, ds, fn (prefix, {def, tycon, tyvars}) =>
                  seq [prefix,
                       Type.layoutApp (Tycon.layout tycon, tyvars, Tyvar.layout),
                       str " = ", Type.layout def])
   end

fun layoutSigexp (e: sigexp): Layout.t =
   case node e of
      Var s => Sigid.layout s
    | Where (e, ws) =>
         let
            val e = layoutSigexp e
         in
            if 0 = Vector.length ws
               then e
            else
               seq [e, 
                    layoutAndsBind
                    (" where", "=", ws, fn {tyvars, longtycon, ty} =>
                     (OneLine,
                      seq [str "type ",
                           Type.layoutApp
                           (Longtycon.layout longtycon, tyvars,
                            Tyvar.layout)],
                      Type.layout ty))]
         end
    | Spec s => align [str "sig",
                       indent (layoutSpec s, 3),
                       str "end"]

and layoutSigConst sigConst =
   case sigConst of
      None => empty
    | Transparent s => seq [str ": ", layoutSigexp s]
    | Opaque s => seq [str " :> ", layoutSigexp s]

and layoutSpec (s: spec): t =
   case node s of
      Datatype rhs => DatatypeRhs.layout rhs
    | Empty => empty
    | Eqtype typedescs => layoutTypedescs ("eqtype", typedescs)
    | Exception sts =>
         layoutAnds
         ("exception", sts, fn (prefix, (c, to)) => seq [prefix,
                                                         Con.layout c,
                                                         Type.layoutOption to])
    | IncludeSigexp s => seq [str "include ", layoutSigexp s]
    | IncludeSigids sigids =>
         seq (str "include "
              :: separate (Vector.toListMap (sigids, Sigid.layout), " "))
    | Seq (s, s') => align [layoutSpec s, layoutSpec s']
    | Sharing {spec, equations} =>
         align [layoutSpec spec,
                align (Vector.toListMap (equations, Equation.layout))]
    | Structure l =>
         layoutAndsBind ("structure", ":", l, fn (strid, sigexp) =>
                         (case node sigexp of
                             Var _ => OneLine
                           | _ => Split 3,
                                Strid.layout strid,
                                layoutSigexp sigexp))
    | Type typedescs => layoutTypedescs ("type", typedescs)
    | TypeDefs typedefs => layoutTypedefs ("type", typedefs)
    | Val sts =>
         layoutAndsBind
         ("val", ":", sts, fn (x, t) => (OneLine, Var.layout x, Type.layout t))

fun checkSyntaxSigexp (e: sigexp): unit =
   case node e of
      Spec s => checkSyntaxSpec s
    | Var _ => ()
    | Where (e, v) =>
         (checkSyntaxSigexp e
          ; Vector.foreach (v, fn {ty, ...} => Type.checkSyntax ty))

and checkSyntaxSigConst (s: sigConst): unit =
   case s of
      None => ()
    | Opaque e => checkSyntaxSigexp e
    | Transparent e => checkSyntaxSigexp e

and checkSyntaxSpec (s: spec): unit =
   let
      fun term () = layoutSpec s
   in
      case node s of
         Datatype d => DatatypeRhs.checkSyntax d
       | Eqtype v =>
            reportDuplicates
            (v, {equals = (fn ({tycon = c, ...}, {tycon = c', ...}) =>
                           Tycon.equals (c, c')),
                 layout = Tycon.layout o #tycon,
                 name = "type",
                 region = Tycon.region o #tycon,
                 term = term})
       | Empty => ()
       | Exception v =>
            (Vector.foreach (v, fn (_, to) =>
                             Option.app (to, Type.checkSyntax))
             ; (reportDuplicates
                (v, {equals = fn ((c, _), (c', _)) => Con.equals (c, c'),
                     layout = Con.layout o #1,
                     name = "exception",
                     region = Con.region o #1,
                     term = term})))
       | IncludeSigexp e => checkSyntaxSigexp e
       | IncludeSigids _ => ()
       | Seq (s, s') => (checkSyntaxSpec s; checkSyntaxSpec s')
       | Sharing {spec, ...} => checkSyntaxSpec spec
       | Structure v =>
            (Vector.foreach (v, checkSyntaxSigexp o #2)
             ; (reportDuplicates
                (v, {equals = fn ((s, _), (s', _)) => Strid.equals (s, s'),
                     layout = Strid.layout o #1,
                     name = "structure specification",
                     region = Strid.region o #1,
                     term = term})))
       | Type v =>
            reportDuplicates
            (v, {equals = (fn ({tycon = c, ...}, {tycon = c', ...}) =>
                           Tycon.equals (c, c')),
                 layout = Tycon.layout o #tycon,
                 name = "type specification",
                 region = Tycon.region o #tycon,
                 term = term})
       | TypeDefs b => TypBind.checkSyntax b
       | Val v =>
            (Vector.foreach (v, fn (_, t) => Type.checkSyntax t)
             ; (reportDuplicates
                (v, {equals = fn ((x, _), (x', _)) => Var.equals (x, x'),
                     layout = Var.layout o #1,
                     name = "value specification",
                     region = Var.region o #1,
                     term = term})))
   end

structure Sigexp =
   struct
      open Wrap
      type spec = spec
      type t = sigexp
      datatype node = datatype sigexpNode
      type node' = node
      type obj = t

      val checkSyntax = checkSyntaxSigexp

      fun wheree (sigexp: t, wherespecs, region): t =
         if 0 = Vector.length wherespecs
            then sigexp
         else makeRegion (Where (sigexp, wherespecs), region)

      fun make n = makeRegion (n, Region.bogus)

      val spec = make o Spec

      val layout = layoutSigexp
   end

structure SigConst =
   struct
      datatype t = datatype sigConst

      val checkSyntax = checkSyntaxSigConst
      val layout = layoutSigConst
   end

structure Spec =
   struct
      open Wrap
      datatype node = datatype specNode
      type t = spec
      type node' = node
      type obj = t

      val checkSyntax = checkSyntaxSpec
      val layout = layoutSpec
   end

(*---------------------------------------------------*)
(*                Strdecs and Strexps                *)
(*---------------------------------------------------*)

datatype strdecNode =
   Core of Dec.t
  | Local of strdec * strdec
  | Seq of strdec list
  | Structure of {constraint: SigConst.t,
                  def: strexp,
                  name: Strid.t} vector

and strexpNode =
   App of Fctid.t * strexp
  | Constrained of strexp * SigConst.t
  | Let of strdec * strexp
  | Struct of strdec
  | Var of Longstrid.t
withtype strexp = strexpNode Wrap.t
and strdec = strdecNode Wrap.t

fun layoutStrdec d =
   case node d of
      Core d => Dec.layout d
    | Local (d, d') => Pretty.locall (layoutStrdec d, layoutStrdec d')
    | Seq ds => align (layoutStrdecs ds)
    | Structure strbs =>
         layoutAndsBind ("structure", "=", strbs,
                         fn {name, def, constraint} =>
                         (case node def of
                             Var _ => OneLine
                           | _ => Split 3,
                                seq [Strid.layout name, SigConst.layout constraint],
                                layoutStrexp def))

and layoutStrdecs ds = layouts (ds, layoutStrdec)

and layoutStrexp exp =
   case node exp of
      App (f, e) => seq [Fctid.layout f, str " ", paren (layoutStrexp e)]
    | Constrained (e, c) => mayAlign [layoutStrexp e, SigConst.layout c]
    | Let (dec, strexp) => Pretty.lett (layoutStrdec dec, layoutStrexp strexp)
    | Struct d => align [str "struct",
                         indent (layoutStrdec d, 3),
                         str "end"]
    | Var s => Longstrid.layout s

fun checkSyntaxStrdec (d: strdec): unit =
   case node d of
      Core d => Dec.checkSyntax d
    | Local (d, d') => (checkSyntaxStrdec d; checkSyntaxStrdec d')
    | Seq ds => List.foreach (ds, checkSyntaxStrdec)
    | Structure v =>
         (Vector.foreach (v, fn {constraint, def, ...} =>
                          (SigConst.checkSyntax constraint
                           ; checkSyntaxStrexp def))
          ; (reportDuplicates
             (v, {equals = (fn ({name = n, ...}, {name = n', ...}) =>
                            Strid.equals (n, n')),
                  layout = Strid.layout o #name,
                  name = "structure definition",
                  region = Strid.region o #name,
                  term = fn () => layoutStrdec d})))
and checkSyntaxStrexp (e: strexp): unit =
   case node e of
      App (_, e) => checkSyntaxStrexp e
    | Constrained (e, c) => (checkSyntaxStrexp e
                             ; SigConst.checkSyntax c)
    | Let (d, e) => (checkSyntaxStrdec d
                     ; checkSyntaxStrexp e)
    | Struct d => checkSyntaxStrdec d
    | Var _ => ()

structure Strexp =
   struct
      open Wrap
      type strdec = strdec
      type t = strexp
      datatype node = datatype strexpNode
      type node' = node
      type obj = t

      val checkSyntax = checkSyntaxStrexp
      fun make n = makeRegion (n, Region.bogus)
      val constrained = make o Constrained
      val lett = make o Let
      val layout = layoutStrexp
   end

structure Strdec =
   struct
      open Wrap
      type t = strdec
      datatype node = datatype strdecNode
      type node' = node
      type obj = t

      val checkSyntax = checkSyntaxStrdec
      fun make n = makeRegion (n, Region.bogus)

      val core = make o Core

      val openn = core o Dec.openn

      val layout = layoutStrdec

      val fromExp = core o Dec.fromExp

      val trace = Trace.trace ("AstModules.Strdec.coalesce", layout, layout)
      fun coalesce (d: t): t =
         trace
         (fn d =>
         case node d of
            Core _ => d
          | Local (d1, d2) =>
               let
                  val d1 = coalesce d1
                  val d2 = coalesce d2
                  val node = 
                     case (node d1, node d2) of
                        (Core d1', Core d2') =>
                           Core (Dec.makeRegion
                                 (Dec.Local (d1', d2'),
                                  Region.append (region d1, region d2)))
                      | _ => Local (d1, d2)
               in
                  makeRegion (node, region d)
               end
          | Seq ds =>
               let
                  fun finish (ds: Dec.t list, ac: t list): t list =
                     case ds of
                        [] => ac
                      | _ =>
                           let
                              val d =
                                 makeRegion (Core (Dec.makeRegion
                                                   (Dec.SeqDec (Vector.fromListRev ds),
                                                    Region.bogus)),
                                             Region.bogus)
                           in
                              d :: ac
                           end
                  fun loop (ds, cores, ac) =
                     case ds of
                        [] => finish (cores, ac)
                      | d :: ds =>
                           let
                              val d = coalesce d
                           in
                              case node d of
                                 Core d => loop (ds, d :: cores, ac)
                               | Seq ds' => loop (ds' @ ds, cores, ac)
                               | _ => loop (ds, [], d :: finish (cores, ac))
                           end
                  val r = region d
               in
                  case loop (ds, [], []) of
                     [] => makeRegion (Core (Dec.makeRegion
                                             (Dec.SeqDec (Vector.new0 ()), r)),
                                       r)
                   | [d] => d
                   | ds => makeRegion (Seq (rev ds), r)
               end
          | Structure _ => d) d
   end

structure FctArg =
   struct
      open Wrap
      datatype node =
         Structure of Strid.t * Sigexp.t
       | Spec of Spec.t
      type t = node Wrap.t
      type node' = node
      type obj = t

      fun layout a =
         case node a of
            Structure (strid, sigexp) =>
               seq [Strid.layout strid, str ": ", Sigexp.layout sigexp]
          | Spec spec => Spec.layout spec

      fun checkSyntax (fa: t): unit =
         case node fa of
            Structure (_, e) => Sigexp.checkSyntax e
          | Spec s => Spec.checkSyntax s
   end

structure Topdec =
   struct
      open Wrap
      datatype node =
         Functor of {arg: FctArg.t,
                     body: Strexp.t,
                     name: Fctid.t,
                     result: SigConst.t} vector
       | Signature of (Sigid.t * Sigexp.t) vector
       | Strdec of Strdec.t
      type t = node Wrap.t
      type node' = node
      type obj = t

      fun layout d =
         case node d of
            Functor fctbs =>
               layoutAndsBind ("functor", "=", fctbs,
                               fn {name, arg, result, body} =>
                               (Split 0,
                                seq [Fctid.layout name, str " ",
                                     paren (FctArg.layout arg),
                                     layoutSigConst result],
                                layoutStrexp body))
          | Signature sigbs =>
               layoutAndsBind ("signature", "=", sigbs,
                               fn (name, def) =>
                               (case Sigexp.node def of
                                   Sigexp.Var _ => OneLine
                                 | _ => Split 3,
                                      Sigid.layout name,
                                      Sigexp.layout def))
          | Strdec d => Strdec.layout d


      fun make n = makeRegion (n, Region.bogus)
      val fromExp = make o Strdec o Strdec.fromExp

      fun checkSyntax (d: t): unit =
         case node d of
            Functor v =>
               (Vector.foreach
                (v, fn {arg, body, result, ...} =>
                 (FctArg.checkSyntax arg
                  ; Strexp.checkSyntax body
                  ; SigConst.checkSyntax result))
                ; (reportDuplicates
                   (v, {equals = (fn ({name = n, ...}, {name = n', ...}) =>
                                  Fctid.equals (n, n')),
                        layout = Fctid.layout o #name,
                        name = "functor definition",
                        region = Fctid.region o #name,
                        term = fn () => layout d})))
          | Signature bs =>
               (Vector.foreach (bs, Sigexp.checkSyntax o #2)
                ; (reportDuplicates
                   (bs,
                    {equals = fn ((s, _), (s', _)) => Sigid.equals (s, s'),
                     layout = Sigid.layout o #1,
                     name = "signature definition",
                     region = Sigid.region o #1,
                     term = fn () => layout d})))
          | Strdec d => Strdec.checkSyntax d
   end

end
