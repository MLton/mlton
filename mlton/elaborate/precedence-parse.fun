(* Heavily modified from the SML/NJ sources. *)

(* Copyright 1996 by AT&T Bell Laboratories *)
(* precedence.sml *)

functor PrecedenceParse (S: PRECEDENCE_PARSE_STRUCTS): PRECEDENCE_PARSE =
struct

open S

local open Ast
in structure Exp = Exp
   structure Fixity = Fixity
   structure Fixop = Fixop
   structure Longvid = Longvid
   structure Pat = Pat
   structure Vid = Vid
end

structure Exp =
struct
   open Exp
   fun apply {func, arg} = Exp.app (func, arg)
   fun applyInfix {func, argl, argr} =
      let
         val arg = Exp.tuple (Vector.new2 (argl, argr))
      in
         Exp.makeRegion (Exp.App (func, arg),
                         Exp.region arg)
      end
end

structure Pat =
struct
   open Pat
   local
      fun finishApply {func, arg, region, ctxt} =
         case Pat.node func of
            Pat.Var {name, ...} =>
               Pat.makeRegion (Pat.App (Longvid.toLongcon name, arg),
                               region)
          | _ =>
               let
                  val () =
                     Control.error
                     (region,
                      Layout.str "non-constructor applied to argument in pattern",
                      ctxt ())
               in
                  Pat.wild
               end
   in
      fun apply ctxt {func, arg} =
         finishApply {func = func, arg = arg,
                      region = Region.append (Pat.region func, Pat.region arg),
                      ctxt = ctxt}
      fun applyInfix ctxt {func, argl, argr} =
         let
            val arg = Pat.tuple (Vector.new2 (argl, argr))
         in
            finishApply {func = func, arg = arg,
                         region = Pat.region arg,
                         ctxt = ctxt}
         end
   end
end

structure Fixval =
   struct
      datatype t = Nonfix | Infix of int * int

      fun eval (f: Fixity.t): t =
         case f of
            Fixity.Infix NONE => Infix (0, 1)
          | Fixity.Infix (SOME n) => Infix (n+n, n+n+1)
          | Fixity.Infixr NONE => Infix (1, 0)
          | Fixity.Infixr (SOME n) => Infix (n+n+1, n+n)
          | Fixity.Nonfix => Nonfix

      fun make ({name: Longvid.t, fixop: Fixop.t}, E: Env.t): t =
         case (fixop, Longvid.split name) of
            (Fixop.None, ([], vid)) =>
               (case Env.peekFix (E, vid) of
                   NONE => Nonfix
                 | SOME f => eval f)
          | _ => Nonfix

      fun makePat (p: Pat.t, E: Env.t): t =
         case Pat.node p of
            Pat.Var r => make (r, E)
          | _ => Nonfix

      fun makeExp (e: Exp.t, E: Env.t): t =
         case Exp.node e of
            Exp.Var r => make (r, E)
          | _ => Nonfix
   end

(*---------------------------------------------------*)
(*           from elaborate/precedence.sml           *)
(*---------------------------------------------------*)

datatype 'a precStack =
   INf of int * 'a * 'a precStack
 | NONf of 'a * 'a precStack
 | NILf

fun 'a parse {apply: {func: 'a, arg: 'a} -> 'a,
              applyInfix: {func: 'a, argl: 'a, argr: 'a} -> 'a,
              ctxt: unit -> Layout.t,
              fixval: 'a -> Fixval.t,
              items: 'a vector,
              name: string,
              region: 'a -> Region.t,
              toString: 'a -> string}: 'a =
   let
      fun error (r: Region.t, msg: string) =
         Control.error (r, Layout.str msg, ctxt ())
      fun ensureNONf ((e, f), p, start) =
         let
            val _ =
               case f of
                  Fixval.Nonfix => ()
                | _ =>
                     error
                     (region e,
                      concat [if start
                                 then name ^ " starts with infix identifier: "
                                 else "identifier must be used infix: ",
                              toString e])
         in
            NONf (e, p)
         end
      fun start token = ensureNONf (token, NILf, true)
      (* parse an expression *)
      fun parse (stack: 'a precStack, (item: 'a, fixval: Fixval.t)) =
         case (stack, (item, fixval)) of
            (NONf (e, r), (e', Fixval.Nonfix)) => NONf (apply {func = e, arg = e'}, r)
          | (p as INf _, token) => ensureNONf (token, p, false)
          | (p as NONf (e1, INf (bp, e2, NONf (e3, r))),
             (e4, f as Fixval.Infix (lbp, rbp))) =>
            if lbp > bp then INf (rbp, e4, p)
            else (if lbp = bp
                     then error (Region.append (region e2, region e4),
                                 concat ["infix identifiers with equal precedence but mixed associativity: ", toString e2, ", ", toString e4])
                  else ();
                  parse (NONf (applyInfix {func = e2, argl = e3, argr = e1},
                               r),
                         (e4, f)))
           | (p as NONf _, (e', Fixval.Infix (_, rbp))) => INf (rbp, e', p)
           | _ => Error.bug "PrecedenceParse.parse.parse"
      (* clean up the stack *)
      fun finish stack =
         case stack of
            NONf (e1, INf (_, e2, NONf (e3, r))) =>
               finish (NONf (applyInfix {func = e2, argl = e3, argr = e1},
                             r))
          | NONf (e1, NILf) => e1
          | INf (_, e1, NONf (e2, p)) =>
               (error (region e1, concat [name, " ends with infix identifier: ", toString e1])
                ; finish (NONf (apply {func = e2, arg = e1}, p)))
          | NILf => Error.bug "PrecedenceParse.parse.finish: NILf"
          | _ => Error.bug "PrecedenceParse.parse.finish"
      fun getfix x = (x, fixval x)
   in
      if Vector.isEmpty items
         then
            Error.bug "PrecedenceParse.parse"
      else
         let
            val item = Vector.first items
         in
            finish (Vector.foldFrom
                    (items, 1, start (getfix item), fn (item, state) =>
                     parse (state, getfix item)))
         end
   end

fun parsePat (ps, E, ctxt) =
   parse {apply = Pat.apply ctxt,
          applyInfix = Pat.applyInfix ctxt,
          ctxt = ctxt,
          fixval = fn p => Fixval.makePat (p, E),
          items = ps,
          name = "pattern",
          region = Pat.region,
          toString = Layout.toString o Pat.layout}

val parsePat =
   Trace.trace ("PrecedenceParse.parsePat",
                fn (ps, _, _) => Vector.layout Pat.layout ps,
                Ast.Pat.layout)
   parsePat

fun parseExp (es, E, ctxt) =
   parse {apply = Exp.apply,
          applyInfix = Exp.applyInfix,
          ctxt = ctxt,
          fixval = fn e => Fixval.makeExp (e, E),
          items = es,
          name = "expression",
          region = Exp.region,
          toString = Layout.toString o Exp.layout}

val parseExp =
   Trace.trace ("PrecedenceParse.parseExp",
                fn (es, _, _) => Vector.layout Exp.layout es,
                Ast.Exp.layout)
   parseExp

(*---------------------------------------------------*)
(*                    parseClause                    *)
(*---------------------------------------------------*)

structure ClausePat =
   struct
      datatype t =
         Apply of {func: t, arg: t}
       | ApplyInfix of {func: t, argl: t, argr: t}
       | Pat of Pat.t

      fun region p =
         case p of
            Apply {func, arg} =>
               Region.append (region func, region arg)
          | ApplyInfix {argl, argr, ...} =>
               Region.append (region argl, region argr)
          | Pat p => Pat.region p

      local
         fun toPat p =
            case p of
               Apply {func, arg} =>
                  let
                     val func = toPat func
                     val arg = toPat arg
                  in
                     Pat.makeRegion
                     (Pat.FlatApp (Vector.new2 (func, arg)),
                      Region.append (Pat.region func, Pat.region arg))
                  end
             | ApplyInfix {func, argl, argr} =>
                  let
                     val func = toPat func
                     val argl = toPat argl
                     val argr = toPat argr
                  in
                     Pat.makeRegion
                     (Pat.FlatApp (Vector.new3 (argl, func, argr)),
                      Region.append (Pat.region argl, Pat.region argr))
                  end
             | Pat p => p
      in
         val layout = Pat.layout o toPat
      end
   end

fun parseClausePats (ps, E, ctxt) =
   parse {apply = ClausePat.Apply,
          applyInfix = ClausePat.ApplyInfix,
          ctxt = ctxt,
          fixval = fn ClausePat.Pat p => Fixval.makePat (p, E)
                    | _ => Fixval.Nonfix,
          items = Vector.map (ps, ClausePat.Pat),
          name = "function clause",
          region = ClausePat.region,
          toString = Layout.toString o ClausePat.layout}

fun parseClause (pats: Pat.t vector, E: Env.t, ctxt) =
   let
      fun error (region, msg) =
         Control.error (region, msg, ctxt ())
      fun improper region =
         error
         (region, Layout.str "function clause with improper infix pattern")

      fun toPat p=
         case p of
            ClausePat.Pat p => p
          | ClausePat.Apply {func, arg} =>
               Pat.apply ctxt
               {func = toPat func,
                arg = toPat arg}
          | ClausePat.ApplyInfix {func, argl, argr} =>
               Pat.applyInfix ctxt
               {func = toPat func,
                argl = toPat argl,
                argr = toPat argr}
      fun toPatTop p =
         case p of
            ClausePat.Pat p => p
          | _ => (improper (ClausePat.region p)
                  ; toPat p)
      fun toPatList p =
         let
            fun loop (p, args) =
               case p of
                  ClausePat.Apply {func, arg} =>
                     loop (func, (toPatTop arg)::args)
                | _ => (toPatTop p)::args
         in
            loop (p, [])
         end

      fun done (func: Pat.t, args: Pat.t list) =
         let
            fun illegalName () =
               (error (Pat.region func,
                       Layout.seq [Layout.str "function clause with illegal name: ",
                                   Pat.layout func])
                ; Ast.Var.bogus)
            val func =
               case Pat.node func of
                  Pat.Var {name, ...} =>
                     (case Longvid.split name of
                         ([], x) => Vid.toVar x
                       | _ => illegalName ())
                | _ => illegalName ()
            val args = Vector.fromList args
            val _ =
               if Vector.isEmpty args
                  then error (Region.append (Pat.region (Vector.sub (pats, 0)),
                                             Pat.region (Vector.last pats)),
                              Layout.str "function clause with no arguments")
                  else ()
         in
            {func = func, args = args}
         end
      fun doneApplyInfix ({func, argl, argr}, rest) =
         let
            val func = toPatTop func
            val argl = toPatTop argl
            val argr = toPatTop argr
         in
            done (func, (Pat.tuple (Vector.new2 (argl, argr)))::rest)
         end
   in
      case parseClausePats (pats, E, ctxt) of
         ClausePat.ApplyInfix func_argl_argr =>
            doneApplyInfix (func_argl_argr, [])
       | p =>
            (case toPatList p of
                [] => Error.bug "PrecedenceParse.parseClause: empty"
              | p::rest =>
                   let
                      val improper = fn () =>
                         (improper (Pat.region p)
                          ; done (Pat.var Ast.Var.bogus, rest))
                   in
                      case Pat.node p of
                         Pat.Paren p' =>
                            (case Pat.node p' of
                                Pat.FlatApp pats =>
                                   (case parseClausePats (pats, E, ctxt) of
                                       ClausePat.ApplyInfix func_argl_argr =>
                                          doneApplyInfix (func_argl_argr, rest)
                                     | _ => improper ())
                              | _ => improper ())
                       | _ => done (p, rest)
                   end)
   end

end
