(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor RedundantTests (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM = 
struct

open S

structure Rel =
   struct
      datatype t =
         EQ
       | LT of {signed: bool}
       | LE of {signed: bool}
       | NE

      val equals: t * t -> bool = op =

      val toString =
         fn EQ => "="
          | LT _ => "<"
          | LE _ => "<="
          | NE => "<>"

      val layout = Layout.str o toString
   end

structure Oper =
   struct
      datatype t =
         Const of Const.t
       | Var of Var.t

      val layout =
         fn Const c => Const.layout c
          | Var x => Var.layout x

      val equals =
         fn (Const c, Const c') => Const.equals (c, c')
          | (Var x, Var x') => Var.equals (x, x')
          | _ => false
   end

structure Fact =
   struct
      datatype t = T of {rel: Rel.t,
                         lhs: Oper.t,
                         rhs: Oper.t}

      fun layout (T {rel, lhs, rhs}) =
         let open Layout
         in seq [Oper.layout lhs, str " ", Rel.layout rel,
                 str " ", Oper.layout rhs]
         end

      fun equals (T {rel, lhs = l, rhs = r},
                  T {rel = rel', lhs = l', rhs = r'}) =
         Rel.equals (rel, rel')
         andalso Oper.equals (l, l')
         andalso Oper.equals (r, r')

      fun negate (T {rel, lhs, rhs}): t =
         let
            datatype z = datatype Rel.t
            val rel =
               case rel of
                  EQ => NE
                | LT s => LE s
                | LE s => LT s
                | NE => EQ
         in
            T {rel = rel, lhs = rhs, rhs = lhs}
         end

      datatype result = False | True | Unknown

      fun determine (facts: t list, f: t): result =
         if List.contains (facts, f, equals)
            then True
         else if List.contains (facts, negate f, equals)
                 then False
              else Unknown
   end

open Exp Transfer

fun transform (Program.T {globals, datatypes, functions, main}) =
   let
      datatype varInfo =
         Const of Const.t
       | Fact of Fact.t
       | None
       | Or of Fact.t * Fact.t
      val {get = varInfo: Var.t -> varInfo, set = setVarInfo, ...} =
         Property.getSetOnce (Var.plist, Property.initConst None)
      val setVarInfo =
         Trace.trace ("RedundantTests.setVarInfo",
                      Var.layout o #1,
                      Unit.layout)
         setVarInfo
      datatype z = datatype Fact.result
      datatype z = datatype Rel.t
      fun makeVarInfo {args, prim, targs = _}: varInfo =
         let
            fun arg i =
               let
                  val x = Vector.sub (args, i)
               in
                  case varInfo x of
                     Const c => Oper.Const c
                   | _ => Oper.Var x
               end
            fun z (r, a, b) =
               Fact (Fact.T {rel = r,
                             lhs = arg a,
                             rhs = arg b})
            fun doit rel = z (rel, 0, 1)
            datatype z = datatype Prim.Name.t
         in
            case Prim.name prim of
               MLton_eq => doit EQ
             | Word_equal _ => doit EQ
             | Word_lt (_, sg) => doit (LT sg)
             | _ => None
         end
      fun setConst (x, c) = setVarInfo (x, Const c)
      val _ =
         Vector.foreach
         (globals, fn Statement.T {var, exp, ...} =>
          case exp of
             Exp.Const c => Option.app (var, fn x => setConst (x, c))
           | _ => ())
      local
         fun make c =
            let
               val x = Var.newNoname ()
            in
               (x,
                Statement.T {var = SOME x, 
                             ty = Type.bool,
                             exp = ConApp {con = c, args = Vector.new0 ()}})
            end
      in
         val (trueVar, t) = make Con.truee
         val (falseVar, f) = make Con.falsee
      end
      local
         val statements = ref []
      in
         val one =
            WordSize.memoize
            (fn s =>
             let
                val one = Var.newNoname ()
                val () =
                   List.push
                   (statements,
                    Statement.T {exp = Exp.Const (Const.word (WordX.one s)),
                                 ty = Type.word s,
                                 var = SOME one})
             in
                one
             end)
         val ones = Vector.fromList (!statements)
      end
      val globals = Vector.concat [Vector.new2 (t, f), ones, globals]
      val shrink = shrinkFunction {globals = globals}
      val numSimplified = ref 0
      fun simplifyFunction f =
          let
             val {args, blocks, mayInline, name, raises, returns, start} =
                Function.dest f
             val _ =
                Control.diagnostic
                (fn () => 
                 let open Layout
                 in seq [str "processing ", Func.layout name]
                 end)
             val {get = labelInfo: Label.t -> {ancestor: Label.t option ref,
                                               facts: Fact.t list ref,
                                               inDeg: int ref},
                  ...} =
                Property.get
                (Label.plist, Property.initFun (fn _ => {ancestor = ref NONE,
                                                         facts = ref [],
                                                         inDeg = ref 0}))
             (* Set up inDeg. *)
             fun inc l = Int.inc (#inDeg (labelInfo l))
             val () = inc start
             val _ =
                Vector.foreach
                (blocks, fn Block.T {transfer, ...} =>
                 Transfer.foreachLabel (transfer, inc))
             (* Perform analysis, set up facts, and set up ancestor. *)
             fun loop (Tree.T (Block.T {label, statements, transfer, ...},
                               children),
                       ancestor') =
                let
                   val _ = 
                      Vector.foreach
                      (statements, fn Statement.T {var, exp, ...} =>
                       case exp of
                          Exp.Const c =>
                             Option.app (var, fn x => setConst (x, c))
                        | Exp.PrimApp pa =>
                             Option.app (var, fn x =>
                                         setVarInfo (x, makeVarInfo pa))
                        | _ => ())
                   val _ = 
                      case transfer of
                         Case {test, cases, default, ...} =>
                            let
                               fun add (l, f) =
                                  let
                                     val {facts, inDeg, ...} = labelInfo l
                                  in
                                     if !inDeg = 1
                                        then List.push (facts, f)
                                     else ()
                                  end           
                               fun falseTrue () =
                                  case cases of
                                     Cases.Con v =>
                                        let
                                           fun ca i = Vector.sub (v, i)
                                        in
                                           case (Vector.length v, default) of
                                              (1, SOME l') =>
                                                 let
                                                    val (c, l) = ca 0
                                                 in
                                                    if Con.equals (c, Con.truee)
                                                       then (l', l)
                                                    else (l, l')
                                                 end
                                            | (2, _) =>
                                                 let
                                                    val (c, l) = ca 0
                                                    val (_, l') = ca 1
                                                 in
                                                    if Con.equals (c, Con.truee)
                                                       then (l', l)
                                                    else (l, l')
                                                 end
                                            | _ => Error.bug "RedundantTests.simplifyFunction: expected two branches"
                                        end
                                   | _ => Error.bug "RedundantTests.simplifyFunction: expected con"
                            in
                               case varInfo test of
                                  Fact f =>
                                     let
                                        val (l, l') = falseTrue ()
                                     in
                                       add (l, Fact.negate f)
                                       ; add (l', f)
                                     end
                                | Or (f, f') =>
                                     let
                                        val (l, _) = falseTrue ()
                                     in
                                        add (l, Fact.negate f) 
                                        ; add (l, Fact.negate f')
                                     end
                                | _ => ()
                            end
                       | _ => ()

                   val {ancestor, facts, ...} = labelInfo label
                   val _ = ancestor := ancestor'
                   val ancestor' = if List.isEmpty (!facts)
                                      then ancestor'
                                   else SOME label
                in
                   Vector.foreach 
                   (children, fn tree => loop (tree, ancestor'))
                end
             val _ = loop (Function.dominatorTree f, NONE)
             (* Diagnostic. *)
             val _ = 
                Control.diagnostics
                (fn display =>
                 Vector.foreach
                 (blocks, fn Block.T {label, ...} =>
                  let open Layout
                  in display (seq [Label.layout label,
                                   str " ",
                                   List.layout Fact.layout
                                   (! (#facts (labelInfo label)))])
                  end))
             (* Transformation. *)
             fun isFact (l: Label.t, p: Fact.t -> bool): bool =
                let
                   fun loop (l: Label.t) =
                      let
                         val {ancestor, facts, ...} = labelInfo l
                      in
                         List.exists (!facts, p)
                         orelse (case !ancestor of
                                    NONE => false
                                  | SOME l => loop l)
                      end
                in
                   loop l
                end
             fun determine (l: Label.t, f: Fact.t) =
                let
                   fun loop {ancestor, facts, ...} =
                      case Fact.determine (!facts, f) of
                         Unknown =>
                            (case !ancestor of
                                NONE => Unknown
                              | SOME l => loop (labelInfo l))
                     | r => r
                in
                   loop (labelInfo l)
                end
             val blocks =
                Vector.map
                (blocks, fn Block.T {label, args, statements, transfer} =>
                 let
                    val statements =
                       Vector.map
                       (statements, fn statement as Statement.T {ty, var, ...} =>
                        let
                           fun doit x =
                              (Int.inc numSimplified
                               ; Control.diagnostic
                                 (fn () =>
                                  let open Layout
                                  in seq [Option.layout Var.layout var,
                                          str " -> ",
                                          Var.layout x]
                                  end)
                               ; Statement.T {var = var, 
                                              ty = ty,
                                              exp = Var x})
                           fun falsee () = doit falseVar
                           fun truee () = doit trueVar
                        in
                           case var of
                              NONE => statement
                            | SOME var =>
                                 (case varInfo var of
                                     Or (f, f') =>
                                        (case determine (label, f) of
                                            False =>
                                               (case determine (label, f') of
                                                   False => falsee ()
                                                 | True => truee ()
                                                 | Unknown => statement)
                                          | True => truee ()
                                          | Unknown => statement)
                                   | Fact f => 
                                        (case determine (label, f) of
                                            False => falsee ()
                                          | True => truee () 
                                          | Unknown => statement)
                                   | _ => statement)
                        end)
                    val noChange = (statements, transfer)
                    fun arith (args: Var.t vector,
                               prim: Type.t Prim.t,
                               success: Label.t)
                       : Statement.t vector * Transfer.t =
                       let
                          fun simplify (prim: Type.t Prim.t,
                                        x: Var.t,
                                        s: WordSize.t) =
                             let
                                val res = Var.newNoname ()
                             in
                                (Vector.concat
                                 [statements,
                                  Vector.new1
                                  (Statement.T
                                   {exp = PrimApp {args = Vector.new2 (x, one s),
                                                   prim = prim,
                                                   targs = Vector.new0 ()},
                                    ty = Type.word s,
                                    var = SOME res})],
                                 Goto {args = Vector.new1 res,
                                       dst = success})
                             end
                          fun add1 (x: Var.t, s: WordSize.t, sg) =
                             if isFact (label, fn Fact.T {lhs, rel, rhs} =>
                                        case (lhs, rel, rhs) of
                                           (Oper.Var x', Rel.LT sg', _) =>
                                              Var.equals (x, x')
                                              andalso sg = sg'
                                         | (Oper.Var x', Rel.LE sg',
                                            Oper.Const c) =>
                                              Var.equals (x, x')
                                              andalso sg = sg'
                                              andalso
                                              (case c of
                                                  Const.Word w =>
                                                     WordX.lt
                                                     (w, WordX.max (s, sg), sg)
                                                | _ => Error.bug "RedundantTests.add1: strange fact")
                                         | _ => false)
                                then simplify (Prim.wordAdd s, x, s)
                             else noChange
                          fun sub1 (x: Var.t, s: WordSize.t, sg) =
                             if isFact (label, fn Fact.T {lhs, rel, rhs} =>
                                        case (lhs, rel, rhs) of
                                           (_, Rel.LT sg', Oper.Var x') =>
                                              Var.equals (x, x')
                                              andalso sg = sg'
                                         | (Oper.Const c, Rel.LE sg',
                                            Oper.Var x') =>
                                              Var.equals (x, x')
                                              andalso sg = sg'
                                              andalso
                                              (case c of
                                                  Const.Word w =>
                                                     WordX.gt
                                                     (w, WordX.min (s, sg), sg)
                                                | _ => Error.bug "RedundantTests.sub1: strange fact")
                                         | _ => false)
                                then simplify (Prim.wordSub s, x, s)
                             else noChange
                          fun add (c: Const.t, x: Var.t, (s, sg as {signed})) =
                             case c of
                                Const.Word i =>
                                   if WordX.isOne i
                                      then add1 (x, s, sg)
                                   else if signed andalso WordX.isNegOne i
                                           then sub1 (x, s, sg)
                                        else noChange
                              | _ => Error.bug "RedundantTests.add: strange const"
                          datatype z = datatype Prim.Name.t
                       in
                          case Prim.name prim of
                             Word_addCheck s =>
                                let
                                   val x1 = Vector.sub (args, 0)
                                   val x2 = Vector.sub (args, 1)
                                in
                                   case varInfo x1 of
                                      Const c => add (c, x2, s)
                                    | _ => (case varInfo x2 of
                                               Const c => add (c, x1, s)
                                             | _ => noChange)
                                end
                           | Word_subCheck (s, sg as {signed}) =>
                                let
                                   val x1 = Vector.sub (args, 0)
                                   val x2 = Vector.sub (args, 1)
                                in
                                   case varInfo x2 of
                                      Const c =>
                                         (case c of
                                             Const.Word w =>
                                                if WordX.isOne w
                                                   then sub1 (x1, s, sg)
                                                else
                                                   if (signed
                                                       andalso WordX.isNegOne w)
                                                      then add1 (x1, s, sg)
                                                   else noChange
                                           | _ =>
                                                Error.bug "RedundantTests.sub: strage const")
                                    | _ => noChange
                                end
                           | _ => noChange
                       end
                    val (statements, transfer) =
                       case transfer of
                          Arith {args, prim, success, ...} =>
                             arith (args, prim, success)
                        | _ => noChange
                 in
                   Block.T {label = label,
                            args = args,
                            statements = statements,
                            transfer = transfer}
                 end)
          in
             shrink (Function.new {args = args,
                                   blocks = blocks,
                                   mayInline = mayInline,
                                   name = name,
                                   raises = raises,
                                   returns = returns,
                                   start = start})
          end
      val _ =
         Control.diagnostic
         (fn () =>
          let open Layout
          in seq [str "numSimplified = ", Int.layout (!numSimplified)]
          end)
      val functions = List.revMap (functions, simplifyFunction)
      val program = 
         Program.T {datatypes = datatypes,
                    globals = globals,
                    functions = functions,
                    main = main}
      val _ = Program.clearTop program
   in
      program
   end
end
