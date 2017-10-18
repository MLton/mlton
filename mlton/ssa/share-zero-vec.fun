(* Copyright (C) 2017 Matthew Fluet.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ShareZeroVec (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM =
  struct

    open S
    open Exp

    (* split a vector of statements at the point where the Array_toVector
     * arg var is declared;
     * return (match, pre, post);
     * note that a var may remain unmatched if reassigned between the original
     * Array_uninit statement and its aliased use in the Array_toVector statement.
     *)
    fun split (stmts, arrVars) =
      case Vector.peekMapi
           (stmts,
            fn Statement.T {var, ty, exp} =>
              case exp of
                  PrimApp ({prim, args, targs}) =>
                    (case Prim.name prim of
                        Array_uninit =>
                          if isSome var
                          then
                            if List.contains (arrVars, valOf var, Var.equals)
                            then SOME (valOf var, ty, Vector.first args, targs)
                            else NONE
                          else NONE
                      | _ => NONE)
                | _ => NONE) of
          NONE => NONE
        | SOME (i, (arrVar, arrTy, len, elemTy)) =>
            SOME ((arrVar, arrTy, len, elemTy), (* val arrVar = Array_uninit ([elemTy], len) *)
                  Vector.prefix (stmts, i),
                  Vector.dropPrefix (stmts, i + 1))

    fun transform (Program.T {datatypes, globals, functions, main}) =
      let

        (* initialize a HashSet for new zero-length array globals *)
        val zeroVar = Var.newString "zero"
        val zeroVarStmt =
          Statement.T
          {var = SOME zeroVar,
           ty = Type.word (WordSize.seqIndex ()),
           exp = Exp.Const (Const.word (WordX.zero (WordSize.seqIndex ())))}
        val newGlobals = ref []
        local
          val hs: {eltTy: Type.t, zeroArrVar: Var.t} HashSet.t =
              HashSet.new {hash = fn {eltTy, ...} => Type.hash eltTy}
        in
          fun getZeroArrVar (ty: Type.t): Var.t =
            let
              val {zeroArrVar, ...} =
                HashSet.lookupOrInsert
                (hs, Type.hash ty,
                fn {eltTy, ...} => Type.equals (eltTy, ty),
                fn () =>
                  let
                    val zeroArrVar = Var.newString "zeroArr"
                    val statement =
                      Statement.T
                      {var = SOME zeroArrVar,
                       ty = Type.array ty,
                       exp = PrimApp
                             {args = Vector.new1 zeroVar,
                              prim = Prim.arrayUninit,
                              targs = Vector.new1 ty}}
                    val () = List.push (newGlobals, statement)
                  in
                    {eltTy = ty,
                     zeroArrVar = zeroArrVar}
                  end)
            in
              zeroArrVar
            end
        end

        val functions' =
          List.map
          (functions, fn f =>
            let
              val {args, blocks, mayInline, name, raises, returns, start} =
                  Function.dest f

              (* 1st pass: compile a list of array vars to be frozen to vectors *)
              val arrVars =
                Vector.fold
                (blocks,
                 [],
                 fn (Block.T {statements, ...}, acc) =>
                   Vector.fold
                   (statements,
                    acc,
                    fn (Statement.T {exp, ...}, acc) =>
                      case exp of
                          PrimApp {prim, args, ...} =>
                          (case Prim.name prim of
                              Array_toVector =>
                                (Vector.first args)::acc
                            | _ => acc)
                        | _ => acc))

            in
              if List.isEmpty arrVars
              then f
              else (* 2nd iteration: branch and join on Array_uninit *)
                let
                  val blocks =
                    let
                      val blocks' = Vector.toList blocks
                      fun splitAndMerge
                          ((block as Block.T {label, args, statements, transfer})::bs,
                          acc) =
                            let
                              val match = split (statements, arrVars)
                            in
                              if isSome match
                              then
                                let
                                  val ((arrVar, arrTy, numVar, eltTy), pre, post) =
                                      valOf match
                                  val ifZeroLab = Label.newString "L_zero"
                                  val ifNonZeroLab = Label.newString "L_nonzero"
                                  val joinLab = Label.newString "L_join"

                                  val preBlock =
                                    let
                                      val isZeroVar = Var.newString "isZero"
                                      val newStatements =
                                        Vector.new1
                                        (Statement.T
                                          {var = SOME isZeroVar,
                                           ty = Type.bool,
                                           exp = PrimApp
                                                 {args = Vector.new2 (zeroVar, numVar),
                                                  prim = Prim.wordEqual (WordSize.seqIndex ()),
                                                  targs = Vector.new0 ()}})
                                      val transfer =
                                        Transfer.Case
                                        {cases = Cases.Con
                                                  (Vector.new2
                                                  ((Con.truee, ifZeroLab),
                                                   (Con.falsee, ifNonZeroLab))),
                                         default = NONE,
                                         test = isZeroVar}
                                    in
                                      Block.T {label = label,
                                               args = args,
                                               statements = Vector.concat [pre, newStatements],
                                               transfer = transfer}
                                    end

                                  val ifNonZeroBlock =
                                    let
                                      val arrVar' = Var.new arrVar
                                      val statements =
                                        Vector.new1
                                        (Statement.T
                                          {var = SOME arrVar',
                                           ty = arrTy,
                                           exp = PrimApp
                                                 {args = Vector.new1 numVar,
                                                  prim = Prim.arrayUninit,
                                                  targs = eltTy}})
                                      val transfer = Transfer.Goto
                                                     {args = Vector.new1 arrVar',
                                                      dst = joinLab}
                                    in
                                      Block.T {label = ifNonZeroLab,
                                               args = Vector.new0 (),
                                               statements = statements,
                                               transfer = transfer}
                                    end

                                  val ifZeroBlock =
                                    let
                                      val transfer =
                                        Transfer.Goto
                                        {args = Vector.new1 (getZeroArrVar (Vector.first eltTy)),
                                         dst = joinLab}
                                    in
                                        Block.T {label = ifZeroLab,
                                                 args = Vector.new0 (),
                                                 statements = Vector.new0 (),
                                                 transfer = transfer}
                                    end

                                  val joinBlock =
                                    Block.T {label = joinLab,
                                             args = Vector.new1 (arrVar, arrTy),
                                             statements = post,
                                             transfer = transfer}
                                in
                                  splitAndMerge
                                  (joinBlock::bs,
                                   ifNonZeroBlock::ifZeroBlock::preBlock::acc)
                                end
                              else splitAndMerge (bs, block::acc)
                            end

                        | splitAndMerge ([], acc) = acc

                    in
                      splitAndMerge (blocks', [])
                    end

                  val blocks = Vector.fromListRev blocks
                in
                  Function.new {args = args,
                                blocks = blocks,
                                mayInline = mayInline,
                                name = name,
                                raises = raises,
                                returns = returns,
                                start = start}
                end

            end)

      in
          Program.T {datatypes = datatypes,
                     globals = Vector.concat
                               [globals,
                                Vector.fromList (zeroVarStmt :: !newGlobals)],
                     functions = functions',
                     main = main}
      end

  end
