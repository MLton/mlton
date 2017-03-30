(* Copyright (C) 2017 Matthew Fluet.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ImplementVectors (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM =
struct

open S

(*
 * This pass eliminates Vector expressions.
 *
 * For each occurrence of the vector expression primitive, it generates
 * code that builds an array of the same size, updates the array with the
 * values that will make up the vector and unsafely casts the array to
 * a vector.
 *)

open Exp Transfer

structure Dexp =
   struct
      open DirectExp
   end

fun transform (Program.T {datatypes, globals, functions, main}) =
  let
      val {get = funcInfo: Func.t -> {hasVectorPrim: bool},
           set = setFuncInfo, ...} =
         Property.getSet (Func.plist, Property.initConst {hasVectorPrim = false})
      val {get = labelInfo: Label.t -> {hasVectorPrim: bool},
           set = setLabelInfo, ...} =
          Property.getSet (Label.plist, Property.initConst {hasVectorPrim = false})

      val () =
         List.foreach
         (functions, fn f =>
          let
             val {name, blocks, ...} = Function.dest f
          in
             Vector.foreach
             (blocks, fn Block.T {label, statements, ...} =>
              let
                 fun setHasVectorPrim () =
                    (setFuncInfo (name, {hasVectorPrim = true})
                     ; setLabelInfo (label, {hasVectorPrim = true}))
              in
                 Vector.foreach
                 (statements, fn Statement.T {exp, ...} =>
                  (case exp of
                      PrimApp {prim, ...} =>
                         (case Prim.name prim of
                             Prim.Name.Vector_vector => setHasVectorPrim ()
                           | _ => ())
                    | _ => ()))
              end)
          end)

      val xformGlobals =
          Vector.exists
              (globals,
               fn (Statement.T {exp, ...}) =>
                  (case exp of
                       PrimApp {prim, ...} =>
                       (case Prim.name prim of
                            Prim.Name.Vector_vector => true
                         | _ => false)
                     | _ => false))

      fun makeVector (args, ty) =
          let
              val ety = Type.deVector ty
              val aty = Type.array ety

              fun buildIndexExp len =
                  Dexp.const
                     (Const.word
                           (WordX.fromIntInf
                                (IntInf.fromInt len,
                                 WordSize.seqIndex ())))

              val arrayVar = Var.newString "a"
              val arrayExp = Dexp.primApp
                                 {args = Vector.new1
                                             (buildIndexExp (Vector.length args)),
                                  prim = Prim.arrayUninit,
                                  targs = Vector.new1 ety,
                                  ty = aty}
              val arrayDecs = [{var = arrayVar, exp = arrayExp}]
              val arrayUpdates = Vector.mapi
                                     (args,
                                      (fn (index, arg) =>
                                          (Dexp.primApp
                                               {args = Vector.new3
                                                           (Dexp.var (arrayVar, aty),
                                                            (buildIndexExp index),
                                                            Dexp.var (arg, ety)),
                                                prim = Prim.arrayUpdate,
                                                targs = Vector.new1 ety,
                                                ty = Type.unit})))
              val arrayUpdateDecs = Vector.fold
                                        (arrayUpdates,
                                         arrayDecs,
                                         (fn (xmlExp, decsList) => decsList @
                                                                   [{var = (Var.newNoname ()),
                                                                     exp = xmlExp}]))
          in
              Dexp.lett {decs = arrayUpdateDecs,
                         body = Dexp.primApp {args = Vector.new1 (Dexp.var (arrayVar, aty)),
                                              prim = Prim.arrayToVector,
                                              targs = Vector.new1 ety,
                                              ty = ty}}
          end

      fun transformStatement (stmt as Statement.T {exp, var, ty, ...}) =
        (case exp of
             PrimApp {prim, args, ...} =>
             (case Prim.name prim of
                  Prim.Name.Vector_vector =>
                  let
                      val vect = makeVector (args, ty)
                      val l = Label.newNoname ()
                      val (_, bs) = Dexp.linearizeGoto
                                        (vect,
                                         Handler.Dead,
                                         l)
                      val (vx, vstmts) =
                          case bs of
                              [Block.T {statements = stmts,
                                        transfer = Transfer.Goto
                                                       {args = args, ...}, ...}] => (Vector.sub(args, 0), stmts)
                            | _ => Error.bug "ImplementVectors.Value.vstmts"
                      val vstmtsList =
                          Vector.foldr
                              (vstmts,
                               [],
                               fn (vstmt, vlist) => vstmt::vlist)
                  in
                      vstmtsList @
                      [Statement.T {var = var, ty = ty, exp = (Exp.Var vx)}]
                  end
                | _ => [stmt])
           | _ => [stmt])

      fun transformStatements (stmts) =
        Vector.foldr
            (stmts, [], (fn (stmt, statements) =>
                            (transformStatement stmt) @ statements))

      fun doit blocks =
          let
              val blocks =
               Vector.foldr
                   (blocks, [],
                    fn (block as Block.T {label, args, statements, transfer}, blocks) =>
                       if not (#hasVectorPrim (labelInfo label))
                       then block::blocks
                       else
                           let
                               val block =
                                   Block.T {label = label,
                                            args = args,
                                            statements = Vector.fromList (transformStatements statements),
                                            transfer = transfer}
                           in
                               block::blocks
                           end)
          in
              Vector.fromList blocks
          end

      val globals =
         if xformGlobals
         then
             let
                 val globals = transformStatements globals
             in
                 Vector.fromList globals
             end
         else globals

      val functions =
         List.revMap
         (functions, fn f =>
          let
             val {args, blocks, mayInline, name, raises, returns, start} =
                Function.dest f
             val f =
                 if #hasVectorPrim (funcInfo name)
                    then Function.new {args = args,
                                       blocks = doit blocks,
                                       mayInline = mayInline,
                                       name = name,
                                       raises = raises,
                                       returns = returns,
                                       start = start}
                 else f
             val () = Function.clear f
          in
             f
          end)

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
