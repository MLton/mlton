(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor AstPrograms (S: AST_PROGRAMS_STRUCTS): AST_PROGRAMS = 
struct

open S

structure AstModules = AstModules (S)

open AstModules Layout

structure Program =
   struct
      datatype t = T of Topdec.t list list

      val empty = T []

      fun layout (T dss) =
         Layout.align (List.map (dss, fn ds =>
                                 Layout.paren 
                                 (Layout.align (List.map (ds, Topdec.layout)))))

      fun checkSyntax (T dss) =
         List.foreach (dss, fn ds => List.foreach (ds, Topdec.checkSyntax))

      fun coalesce (T dss): t =
         let
            fun finish (sds, ac) =
               case sds of
                  [] => ac
                | _ =>
                     let
                        val t =
                           Topdec.makeRegion
                           (Topdec.Strdec (Strdec.makeRegion
                                           (Strdec.Seq (rev sds), Region.bogus)),
                            Region.bogus)
                     in
                        t :: ac
                     end
            fun loop (ds, sds, ac) =
               case ds of
                  [] => finish (sds, ac)
                | d :: ds =>
                     case Topdec.node d of
                        Topdec.Strdec d => loop (ds, d :: sds, ac)
                      | _ => loop (ds, [], d :: finish (sds, ac))
         in
            T (List.map (dss, fn ds => rev (loop (ds, [], []))))
         end

      val coalesce =
         Trace.trace ("AstPrograms.Program.coalesce", layout, layout) coalesce

      fun size (T dss): int =
         let
            val n = ref 0
            fun inc () = n := 1 + !n
            fun dec (d: Dec.t): unit =
               let
                  datatype z = datatype Dec.node
               in
                  case Dec.node d of
                     Abstype {body, ...} => dec body
                   | Exception cs => Vector.foreach (cs, fn _ => inc ())
                   | Fun {fbs, ...} =>
                        Vector.foreach (fbs, fn clauses =>
                                        Vector.foreach (clauses, exp o #body))
                   | Local (d, d') => (dec d; dec d')
                   | SeqDec ds => Vector.foreach (ds, dec)
                   | Val {vbs, rvbs, ...} =>
                        (Vector.foreach (vbs, exp o #exp)
                         ; Vector.foreach (rvbs, match o #match))
                   | _ => ()
               end
            and exp (e: Exp.t): unit =
               let
                  val _ = inc ()
                  datatype z = datatype Exp.node
               in
                  case Exp.node e of
                     Andalso (e1, e2) => (exp e1; exp e2)
                   | App (e, e') => (exp e; exp e')
                   | Case (e, m) => (exp e; match m)
                   | Constraint (e, _) => exp e
                   | FlatApp es => exps es
                   | Fn m => match m
                   | Handle (e, m) => (exp e; match m)
                   | If (e1, e2, e3) => (exp e1; exp e2; exp e3)
                   | Let (d, e) => (dec d; exp e)
                   | List es => Vector.foreach (es, exp)
                   | Orelse (e1, e2) => (exp e1; exp e2)
                   | Raise exn => exp exn
                   | Record r => Record.foreach (r, exp o #2)
                   | Seq es => exps es
                   | While {test, expr} => (exp test; exp expr)
                   | _ => ()
               end
            and exps es = Vector.foreach (es, exp)
            and match m =
               let
                  val Match.T rules = Match.node m
               in
                  Vector.foreach (rules, exp o #2)
               end
            fun strdec d =
               let
                  datatype z = datatype Strdec.node
               in
                  case Strdec.node d of
                     Core d => dec d
                   | Local (d, d') => (strdec d; strdec d')
                   | Seq ds => List.foreach (ds, strdec)
                   | ShowBasis _ => ()
                   | Structure ds =>
                        Vector.foreach (ds, fn {def, ...} => strexp def)
               end
            and strexp e =
               let
                  datatype z = datatype Strexp.node
               in
                  case Strexp.node e of
                     Struct d => strdec d
                   | Constrained (e, _) => strexp e
                   | App (_, e) => strexp e
                   | Let (d, e) => (strdec d; strexp e)
                   | _ => ()
               end

            fun topdec d =
               let
                  datatype z = datatype Topdec.node
               in
                  case Topdec.node d of
                     Functor ds =>
                        Vector.foreach (ds, fn {body, ...} => strexp body)
                   | Strdec d => strdec d
                   | _ => ()
               end
            val _ = List.foreach (dss, fn ds => List.foreach (ds, topdec))
         in
            !n
         end
      (* quell unused warning *)
      val _ = size
   end

end
