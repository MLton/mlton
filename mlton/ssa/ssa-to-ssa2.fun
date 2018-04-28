(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 2004-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor SsaToSsa2 (S: SSA_TO_SSA2_STRUCTS): SSA_TO_SSA2 = 
struct

open S

structure S = Ssa
structure S2 = Ssa2

local
   open S
in
   structure Con = Con
   structure Label = Label
   structure Prim = Prim
   structure Var = Var
end

local
   open S2
in
   structure Base = Base
   structure Prod = Prod
end

fun convert (S.Program.T {datatypes, functions, globals, main}) =
   let
      val {get = convertType: S.Type.t -> S2.Type.t, ...} =
         Property.get
         (S.Type.plist,
          Property.initRec
          (fn (t, convertType)  =>
           case S.Type.dest t of
              S.Type.Array t => S2.Type.array1 (convertType t)
            | S.Type.CPointer => S2.Type.cpointer
            | S.Type.Datatype tycon => S2.Type.datatypee tycon
            | S.Type.IntInf => S2.Type.intInf
            | S.Type.Real s => S2.Type.real s
            | S.Type.Ref t => S2.Type.reff1 (convertType t)
            | S.Type.Thread => S2.Type.thread
            | S.Type.Tuple ts =>
                 S2.Type.tuple (Prod.make
                                (Vector.map (ts, fn t =>
                                             {elt = convertType t,
                                              isMutable = false})))
            | S.Type.Vector t => S2.Type.vector1 (convertType t)
            | S.Type.Weak t => S2.Type.weak (convertType t)
            | S.Type.Word s => S2.Type.word s))
      fun convertTypes ts = Vector.map (ts, convertType)
      val {get = conType: Con.t -> S2.Type.t, set = setConType, ...} =
         Property.getSetOnce (Con.plist,
                              Property.initRaise ("type", Con.layout))
      val datatypes =
         Vector.map
         (datatypes, fn S.Datatype.T {cons, tycon} =>
          S2.Datatype.T
          {cons = Vector.map (cons, fn {args, con} =>
                              let
                                 val args =
                                    Prod.make
                                    (Vector.map (args, fn t =>
                                                 {elt = convertType t,
                                                  isMutable = false}))
                                 val () =
                                    setConType (con, S2.Type.conApp (con, args))
                              in
                                 {args = args,
                                  con = con}
                              end),
           tycon = tycon})
      fun convertPrim p = S.Prim.map (p, convertType)
      fun convertStatement (S.Statement.T {exp, ty, var})
         : S2.Statement.t vector =
         let
            val ty = convertType ty
            fun simple (exp: S2.Exp.t): S2.Statement.t vector =
               Vector.new1 (S2.Statement.Bind {exp = exp, ty = ty, var = var})
            fun maybeBindUnit (stmt: S2.Statement.t): S2.Statement.t vector =
               case var of
                  NONE => Vector.new1 stmt
                | SOME _ =>
                     Vector.new2
                     (S2.Statement.Bind {var = var,
                                         ty = ty,
                                         exp = S2.Exp.unit},
                      stmt)
         in
            case exp of
               S.Exp.ConApp {args, con} =>
                  let
                     val sum =
                        case S2.Type.dest ty of
                           S2.Type.Datatype tycon => tycon
                         | _ => Error.bug "SsaToSsa2.convertStatement: strange ConApp"
                     val variant = Var.newNoname ()
                  in
                     Vector.new2
                     (S2.Statement.Bind {exp = S2.Exp.Object {args = args,
                                                              con = SOME con},
                                         ty = conType con,
                                         var = SOME variant},
                      S2.Statement.Bind {exp = S2.Exp.Inject {variant = variant,
                                                              sum = sum},
                                         ty = ty,
                                         var = var})
                  end
             | S.Exp.Const c => simple (S2.Exp.Const c)
             | S.Exp.PrimApp {args, prim, ...} =>
                  let
                     fun arg i = Vector.sub (args, i)
                     fun sub () =
                        simple
                        (S2.Exp.Select {base = Base.VectorSub {index = arg 1,
                                                               vector = arg 0},
                                        offset = 0})
                     datatype z = datatype Prim.Name.t
                   in
                      case Prim.name prim of
                         Array_sub => sub ()
                       | Array_update =>
                            maybeBindUnit
                            (S2.Statement.Update
                             {base = Base.VectorSub {index = arg 1,
                                                     vector = arg 0},
                              offset = 0,
                              value = arg 2})
                       | Ref_assign =>
                            maybeBindUnit
                            (S2.Statement.Update
                             {base = Base.Object (arg 0),
                              offset = 0,
                              value = arg 1})
                       | Ref_deref =>
                            simple (S2.Exp.Select {base = Base.Object (arg 0),
                                                   offset = 0})
                       | Ref_ref =>
                            simple (S2.Exp.Object {args = Vector.new1 (arg 0),
                                                   con = NONE})
                       | Vector_length =>
                            simple (S2.Exp.PrimApp {args = args,
                                                    prim = Prim.arrayLength})
                       | Vector_sub => sub ()
                       | Vector_vector =>
                            let
                               val siws = S2.WordSize.seqIndex ()
                               fun mkIStmt (iVar, i) =
                                  S2.Statement.Bind
                                  {exp = (S2.Exp.Const o S2.Const.word o S2.WordX.fromIntInf)
                                         (IntInf.fromInt i, siws),
                                   ty = S2.Type.word siws,
                                   var = SOME iVar}
                               val nVar = Var.newString "n"
                               val aVar = Var.newString "a"
                               val vStmt =
                                  S2.Statement.Bind
                                  {exp = S2.Exp.PrimApp {args = Vector.new1 aVar,
                                                         prim = Prim.arrayToVector},
                                   ty = ty,
                                   var = var}
                               val stmts =
                                  Vector.foldri
                                  (args, [vStmt], fn (i, arg, stmts) =>
                                   let
                                      val iVar = Var.newString "i"
                                      val iStmt = mkIStmt (iVar, i)
                                      val uStmt =
                                         S2.Statement.Update
                                         {base = Base.VectorSub {index = iVar,
                                                                 vector = aVar},
                                          offset = 0,
                                          value = arg}
                                   in
                                      iStmt::uStmt::stmts
                                   end)
                               val nStmt = mkIStmt (nVar, Vector.length args)
                               val aStmt =
                                  S2.Statement.Bind
                                  {exp = S2.Exp.PrimApp {args = Vector.new1 nVar,
                                                         prim = Prim.arrayAlloc
                                                                {raw = false}},
                                   ty = S2.Type.array1 (S2.Type.deVector1 ty),
                                   var = SOME aVar}
                               val stmts = nStmt::aStmt::stmts
                            in
                               Vector.fromList stmts
                            end
                       | _ =>
                            simple (S2.Exp.PrimApp {args = args,
                                                    prim = convertPrim prim})
                   end
             | S.Exp.Profile e => maybeBindUnit (S2.Statement.Profile e)
             | S.Exp.Select {offset, tuple} =>
                  simple (S2.Exp.Select {base = Base.Object tuple,
                                         offset = offset})
             | S.Exp.Tuple v => simple (S2.Exp.Object {args = v, con = NONE})
             | S.Exp.Var x => simple (S2.Exp.Var x)
         end
      val convertStatement =
         Trace.trace ("SsaToSsa2.convertStatement",
                      S.Statement.layout,
                      Vector.layout S2.Statement.layout)
            convertStatement
      fun convertHandler (h: S.Handler.t): S2.Handler.t =
         case h of
            S.Handler.Caller => S2.Handler.Caller
          | S.Handler.Dead => S2.Handler.Dead
          | S.Handler.Handle l => S2.Handler.Handle l
      fun convertReturn (r: S.Return.t): S2.Return.t =
         case r of
            S.Return.Dead => S2.Return.Dead
          | S.Return.NonTail {cont, handler} =>
               S2.Return.NonTail {cont = cont,
                                  handler = convertHandler handler}
          | S.Return.Tail => S2.Return.Tail
      val extraBlocks: S2.Block.t list ref = ref []
      fun convertCases (cs: S.Cases.t): S2.Cases.t =
         case cs of
            S.Cases.Con v =>
               S2.Cases.Con
               (Vector.map
                (v, fn (c, l) =>
                 let
                    val objectTy = conType c
                 in
                    case S2.Type.dest objectTy of
                       S2.Type.Object {args, ...} =>
                          if Prod.isEmpty args
                             then (c, l)
                          else
                             let
                                val l' = Label.newNoname ()
                                val object = Var.newNoname ()
                                val (xs, statements) =
                                   Vector.unzip
                                   (Vector.mapi
                                    (Prod.dest args, fn (i, {elt = ty, ...}) =>
                                     let
                                        val x = Var.newNoname ()
                                        val exp =
                                           S2.Exp.Select
                                           {base = Base.Object object,
                                            offset = i}
                                     in
                                        (x,
                                         S2.Statement.Bind {exp = exp,
                                                            ty = ty,
                                                            var = SOME x})
                                     end))
                                val transfer =
                                   S2.Transfer.Goto {args = xs, dst = l}
                                val args = Vector.new1 (object, objectTy)
                                val () =
                                   List.push
                                   (extraBlocks,
                                    S2.Block.T {args = args,
                                                label = l',
                                                statements = statements,
                                                transfer = transfer})
                             in
                                (c, l')
                             end
                     | _ => Error.bug "SsaToSsa2.convertCases: strange object type"
                 end))
          | S.Cases.Word v => S2.Cases.Word v
      fun convertTransfer (t: S.Transfer.t): S2.Transfer.t =
         case t of
            S.Transfer.Arith {args, overflow, prim, success, ty} =>
               S2.Transfer.Arith {args = args,
                                  overflow = overflow,
                                  prim = convertPrim prim,
                                  success = success,
                                  ty = convertType ty}
          | S.Transfer.Bug => S2.Transfer.Bug
          | S.Transfer.Call {args, func, return} =>
               S2.Transfer.Call {args = args,
                                 func = func,
                                 return = convertReturn return}
          | S.Transfer.Case {cases, default, test} =>
               S2.Transfer.Case {cases = convertCases cases,
                                 default = default,
                                 test = test}
          | S.Transfer.Goto r => S2.Transfer.Goto r
          | S.Transfer.Raise v => S2.Transfer.Raise v
          | S.Transfer.Return v => S2.Transfer.Return v
          | S.Transfer.Runtime {args, prim, return} =>
               S2.Transfer.Runtime {args = args,
                                    prim = convertPrim prim,
                                    return = return}
      fun convertStatements ss =
         Vector.concatV (Vector.map (ss, convertStatement))
      fun convertFormals xts = Vector.map (xts, fn (x, t) => (x, convertType t))
      fun convertBlock (S.Block.T {args, label, statements, transfer}) =
         S2.Block.T {args = convertFormals args,
                     label = label,
                     statements = convertStatements statements,
                     transfer = convertTransfer transfer}
      val functions =
         List.map
         (functions, fn f =>
          let
             val {args, blocks, mayInline, name, raises, returns, start} =
                S.Function.dest f
             fun rr tvo = Option.map (tvo, convertTypes)
             val blocks = Vector.map (blocks, convertBlock)
             val blocks = Vector.concat [blocks, Vector.fromList (!extraBlocks)]
             val () = extraBlocks := []
          in
             S2.Function.new {args = convertFormals args,
                              blocks = blocks,
                              mayInline = mayInline,
                              name = name,
                              raises = rr raises,
                              returns = rr returns,
                              start = start}
          end)
      val globals = convertStatements globals
      val program = 
         S2.Program.T {datatypes = datatypes,
                       functions = functions,
                       globals = globals,
                       main = main}
   in
      S2.shrink program
   end

end
