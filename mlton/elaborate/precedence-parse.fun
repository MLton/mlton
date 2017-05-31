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
              fixval: 'a -> Fixval.t,
              items: 'a vector,
              lay: unit -> Layout.t,
              name: string,
              region: 'a -> Region.t,
              toString: 'a -> string}: 'a =
   let
      fun error (r: Region.t, msg: string) =
         Control.error (r, Layout.str msg, lay ())
      fun ensureNONf ((e, f), p) =
         let
            val _ =
               case f of
                  Fixval.Nonfix => ()
                | _ =>
                     Control.error
                     (region e,
                      Layout.str (concat ["identifier must be used infix: ",
                                          toString e]),
                      lay ())
         in
            NONf (e, p)
         end
      fun start token = ensureNONf (token, NILf)
      (* parse an expression *)
      fun parse (stack: 'a precStack, (item: 'a, fixval: Fixval.t)) =
         case (stack, (item, fixval)) of
            (NONf (e, r), (e', Fixval.Nonfix)) => NONf (apply {func = e, arg = e'}, r)
          | (p as INf _, token) => ensureNONf (token, p)
          | (p as NONf (e1, INf (bp, e2, NONf (e3, r))),
             (e4, f as Fixval.Infix (lbp, rbp))) =>
            if lbp > bp then INf (rbp, e4, p)
            else (if lbp = bp
                     then error (region e1,
                                 "operators of same precedence with mixed associativity")
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
               (error (region e1, concat [name, " ends with infix identifier"])
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
            val item = Vector.sub (items, 0)
         in
            finish (Vector.foldFrom
                    (items, 1, start (getfix item), fn (item, state) =>
                     parse (state, getfix item)))
         end
   end

fun parsePat (ps, E, lay) =
   let
      fun finishApply {func, arg, region} =
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
                      lay ())
               in
                  Pat.wild
               end
      fun apply {func, arg} =
         finishApply {func = func, arg = arg,
                      region = Region.append (Pat.region func, Pat.region arg)}
      fun applyInfix {func, argl, argr} =
         let
            val arg = Pat.tuple (Vector.new2 (argl, argr))
         in
            finishApply {func = func, arg = arg, region = Pat.region arg}
         end
   in
      parse {apply = apply,
             applyInfix = applyInfix,
             fixval = fn p => Fixval.makePat (p, E),
             items = ps,
             lay = lay,
             name = "pattern",
             region = Pat.region,
             toString = Layout.toString o Pat.layout}
   end

val parsePat =
   Trace.trace ("PrecedenceParse.parsePat",
                fn (ps, _, _) => Vector.layout Pat.layout ps,
                Ast.Pat.layout)
   parsePat

fun parseExp (es, E, lay) =
   let
      fun apply {func, arg} = Exp.app (func, arg)
      fun applyInfix {func, argl, argr} =
         let
            val arg = Exp.tuple (Vector.new2 (argl, argr))
         in
            Exp.makeRegion (Exp.App (func, arg),
                            Exp.region arg)
         end
   in
      parse {apply = apply,
             applyInfix = applyInfix,
             fixval = fn e => Fixval.makeExp (e, E),
             items = es,
             lay = lay,
             name = "expression",
             region = Exp.region,
             toString = Layout.toString o Exp.layout}
   end

val parseExp =
   Trace.trace ("PrecedenceParse.parseExp",
                fn (es, _, _) => Vector.layout Exp.layout es,
                Ast.Exp.layout)
   parseExp

(*---------------------------------------------------*)
(*                    parseClause                    *)
(*---------------------------------------------------*)

fun parseClause (pats: Pat.t vector, E: Env.t, lay) =
   let
      fun error (region, msg) =
         (Control.error (region, msg, lay ())
          ; {func = Ast.Var.bogus,
             args = Vector.new0 ()})
      fun done (func: Pat.t, args: Pat.t list) =
         let
            fun illegal () =
               error (Pat.region func,
                      Layout.seq [Layout.str "function defined with illegal name: ",
                                  Pat.layout func])
         in
            case Pat.node func of
               Pat.Var {name, ...} =>
                  (case Longvid.split name of
                      ([], x) => {func = Vid.toVar x,
                                  args = Vector.fromList args}
                    | _ => illegal ())
             | _ => illegal ()
         end
      val tuple = Pat.tuple o Vector.new2
      fun parse (ps : Pat.t list) =
         case ps of
            p :: rest =>
               let
                  fun continue () =
                     case rest of
                        [] => error (Region.append (Pat.region (Vector.sub (pats, 0)),
                                                    Pat.region (Vector.last pats)),
                                     Layout.str "function defined with no arguments")
                      | _ => done (p, rest)
               in
                  case Pat.node p of
                     Pat.FlatApp ps =>
                        if 3 = Vector.length ps
                           then
                              let
                                 fun p i = Vector.sub (ps, i)
                              in done (p 1, tuple (p 0, p 2) :: rest)
                              end
                        else continue ()
                   | Pat.Paren p => parse (p :: rest)
                   | _ => continue ()
               end
          | _ => Error.bug "PrecedenceParse.parseClause: empty"
      val pats = Vector.toList pats
   in
      case pats of
         [a, b, c] => (case Fixval.makePat (b, E) of
                          Fixval.Nonfix => parse pats
                        | _ => done (b, [tuple (a, c)]))
       | _ => parse pats
   end

end
