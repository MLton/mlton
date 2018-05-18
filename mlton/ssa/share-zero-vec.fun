(* Copyright (C) 2017 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ShareZeroVec (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM =
struct

open S
open Exp

fun transform (Program.T {datatypes, globals, functions, main}) =
   let
      val seqIndexSize = WordSize.seqIndex ()
      val seqIndexTy = Type.word seqIndexSize
      val (zeroVar, globals) =
         case Vector.peekMap (globals, fn Statement.T {var, ty, exp} =>
                              case (var, exp) of
                                 (SOME var, Exp.Const (Const.Word w)) =>
                                    if WordX.isZero w
                                       andalso Type.equals (seqIndexTy, ty)
                                       then SOME var
                                       else NONE
                            | _ => NONE) of
            SOME zeroVar => (zeroVar, globals)
          | _ => let
                    val zeroVar = Var.newString "zero"
                    val zeroVarStmt =
                       Statement.T
                       {var = SOME zeroVar,
                        ty = seqIndexTy,
                        exp = Exp.Const (Const.word (WordX.zero seqIndexSize))}
                 in
                    (zeroVar, Vector.concat [globals, Vector.new1 zeroVarStmt])
                 end

      val shrink = shrinkFunction {globals = globals}

      (* initialize a HashSet for new zero-length array globals *)
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
                                 prim = Prim.arrayAlloc {raw = false},
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

      (* splitStmts (stmts, arrVars)
       * returns (preStmts, (arrVar, arrTy, eltTy, lenVar), postStmts)
       * when stmts = ...pre...
       *              val arrVar: arrTy = Array_alloc(eltTy) (lenVar)
       *              ...post...
       * and arrVar in arrVars
       *)
      fun splitStmts (stmts, arrVars) =
         case Vector.peekMapi
              (stmts, fn Statement.T {var, ty, exp} =>
               case exp of
                  PrimApp ({prim, args, targs}) =>
                     (case (var, Prim.name prim) of
                         (SOME var, Prim.Name.Array_alloc {raw = false}) =>
                            if List.contains (arrVars, var, Var.equals)
                               then SOME (var, ty,
                                          Vector.first targs,
                                          Vector.first args)
                               else NONE
                       | _ => NONE)
                | _ => NONE) of
            NONE => NONE
          | SOME (i, (arrVar, arrTy, eltTy, lenVar)) =>
               SOME (Vector.prefix (stmts, i),
                     (* val arrVar: arrTy = Array_alloc(eltTy) (lenVar) *)
                     (arrVar, arrTy, eltTy, lenVar),
                     Vector.dropPrefix (stmts, i + 1))

      fun transformBlock (block, arrVars) =
         case splitStmts (Block.statements block, arrVars) of
            NONE => NONE
          | SOME (preStmts, (arrVar, arrTy, eltTy, lenVar), postStmts) =>
               let
                  val Block.T {label, args, transfer, ...} = block
                  val ifZeroLab = Label.newString "L_zeroLen"
                  val ifNonZeroLab = Label.newString "L_nonZeroLen"
                  val joinLab = Label.newString "L_join"

                  (* new block up to Array_alloc match *)
                  val preBlock =
                     let
                        val isZeroVar = Var.newString "isZero"
                        val newStatements =
                           Vector.new1
                           (Statement.T
                            {var = SOME isZeroVar,
                             ty = Type.bool,
                             exp = PrimApp
                                   {args = Vector.new2 (zeroVar, lenVar),
                                    prim = Prim.wordEqual seqIndexSize,
                                    targs = Vector.new0 ()}})
                        val transfer =
                           Transfer.Case
                           {cases = (Cases.Con o Vector.new2)
                                    ((Con.truee, ifZeroLab),
                                     (Con.falsee, ifNonZeroLab)),
                            default = NONE,
                            test = isZeroVar}
                     in
                        Block.T {label = label,
                                 args = args,
                                 statements = Vector.concat [preStmts,
                                                             newStatements],
                                 transfer = transfer}
                     end

                  (* new block for if zero array *)
                  val ifZeroBlock =
                     let
                        val transfer =
                           Transfer.Goto
                           {args = Vector.new1 (getZeroArrVar eltTy),
                            dst = joinLab}
                     in
                        Block.T {label = ifZeroLab,
                                 args = Vector.new0 (),
                                 statements = Vector.new0 (),
                                 transfer = transfer}
                     end

                  (* new block for if non-zero array *)
                  val ifNonZeroBlock =
                     let
                        val arrVar' = Var.new arrVar
                        val statements =
                           Vector.new1
                           (Statement.T
                            {var = SOME arrVar',
                             ty = arrTy,
                             exp = PrimApp
                                   {args = Vector.new1 lenVar,
                                    prim = Prim.arrayAlloc {raw = false},
                                    targs = Vector.new1 eltTy}})
                        val transfer =
                           Transfer.Goto
                           {args = Vector.new1 arrVar',
                            dst = joinLab}
                     in
                        Block.T {label = ifNonZeroLab,
                                 args = Vector.new0 (),
                                 statements = statements,
                                 transfer = transfer}
                     end

                  (* new block with statements following match *)
                  val joinBlock =
                     Block.T {label = joinLab,
                              args = Vector.new1 (arrVar, arrTy),
                              statements = postStmts,
                              transfer = transfer}
               in
                  SOME (preBlock, ifZeroBlock, ifNonZeroBlock, joinBlock)
               end

      val functions =
         List.revMap
         (functions, fn f =>
          let
             val {args, blocks, mayInline, name, raises, returns, start} =
                Function.dest f

             (* analysis: compile a list of array vars cast to vectors *)
             val arrVars =
                Vector.fold
                (blocks, [], fn (Block.T {statements, ...}, acc) =>
                 Vector.fold
                 (statements, acc, fn (Statement.T {exp, ...}, acc) =>
                  case exp of
                     PrimApp ({prim, args, ...}) =>
                        (case Prim.name prim of
                            Prim.Name.Array_toVector =>
                               (Vector.first args)::acc
                          | _ => acc)
                   | _ => acc))
          in
             if List.isEmpty arrVars
                then f (* no Array_toVector found in the function *)
                else (* transformation: branch and join at Array_alloc *)
                   let
                      fun doBlock (b, acc) =
                         case transformBlock (b, arrVars) of
                            NONE => b::acc
                          | SOME (preBlock,
                                  ifZeroBlock, ifNonZeroBlock,
                                  joinBlock) =>
                               doBlock (joinBlock,
                                        ifNonZeroBlock::ifZeroBlock::preBlock::acc)
                      val blocks = Vector.fold (blocks, [], doBlock)
                      val blocks = Vector.fromListRev blocks
                   in
                      shrink (Function.new {args = args,
                                            blocks = blocks,
                                            mayInline = mayInline,
                                            name = name,
                                            raises = raises,
                                            returns = returns,
                                            start = start})
                   end
          end)
      val globals = Vector.concat [globals, Vector.fromList (!newGlobals)]
   in
      Program.T {datatypes = datatypes,
                 globals = globals,
                 functions = functions,
                 main = main}
   end

end
