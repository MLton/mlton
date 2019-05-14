(* Copyright (C) 2018 Jason Carr
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(*
 * Duplicate each global that appears in the program, once per usage in the program.
 * Globals which refer to other globals are not duplicated recursively.
 *
 * The primary purpose of this pass is to support passes which could possibly use
 * different types for different globals, currently SplitTypes, and so that these
 * passes may be separately enabled.
 *)

functor DuplicateGlobals(S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM =
struct

   open S

   fun transform (Program.T {datatypes, globals, functions, main}): Program.t =
      let
         val globalVars: (Var.t, Var.t list ref) HashTable.t =
            HashTable.new {hash=Var.hash, equals=Var.equals}
         val _ = Vector.foreach (globals, fn st =>
            let
               val exp = Statement.exp st
               val var = Statement.var st
               fun duplicatable var =
                  ignore (HashTable.lookupOrInsert (globalVars, var, fn () => ref []))
               val _ =
                  case (var, exp) of
                     (SOME var, Exp.ConApp _) => duplicatable var
                   | (SOME var, Exp.PrimApp {prim, ...}) =>
                        (case Prim.name prim of
                             (* we might want to duplicate this due to the targ *)
                             Prim.Name.MLton_bogus => duplicatable var
                           | Prim.Name.Vector_vector => duplicatable var
                           | _ => ())
                   | _ => ()
            in
               ()
            end)
         fun freshenIfGlobal (var: Var.t) =
            case HashTable.peek (globalVars, var) of
                 NONE => var
               | SOME vs =>
                    let
                       val newVar = Var.new var
                       val _ = List.push (vs, newVar)
                    in
                       newVar
                    end
         fun freshenVec (vars: Var.t vector) = Vector.map (vars, freshenIfGlobal)

         (* Rewrite all globals to a new version if they're in the table of valid globals *)
         fun loopExp exp =
            case exp of
                 Exp.ConApp {args, con} => Exp.ConApp {con=con, args=freshenVec args}
               | Exp.PrimApp {args, prim, targs} =>
                    Exp.PrimApp {args=freshenVec args, prim=prim, targs=targs}
               | Exp.Select {offset, tuple} =>
                    Exp.Select {offset=offset, tuple=freshenIfGlobal tuple}
               | Exp.Tuple vs => Exp.Tuple (freshenVec vs)
               | Exp.Var v => Exp.Var (freshenIfGlobal v)
               | _ => exp
         fun loopStatement (Statement.T {exp, ty, var}) =
            (* This variable won't be global, since we're only looping over the program *)
            Statement.T {exp=loopExp exp, ty=ty, var=var}

         fun loopTransfer transfer =
            case transfer of
                 Transfer.Bug => Transfer.Bug
               | Transfer.Call {args, func, return} =>
                    Transfer.Call {args=freshenVec args, func=func, return=return}
               | Transfer.Case {cases, default, test} =>
                    Transfer.Case {cases=cases, default=default, test=freshenIfGlobal test}
               | Transfer.Goto {args, dst} =>
                    Transfer.Goto {args=freshenVec args, dst=dst}
               | Transfer.Raise vs =>
                    Transfer.Raise (freshenVec vs)
               | Transfer.Return vs =>
                    Transfer.Return (freshenVec vs)
               | Transfer.Runtime {args, prim, return} =>
                    Transfer.Runtime {args=freshenVec args, prim=prim, return=return}
         fun loopBlock (Block.T {args, label, statements, transfer}) =
            Block.T {args=args, label=label,
               statements=Vector.map (statements, loopStatement),
               transfer=loopTransfer transfer}
         fun loopFunction func =
            let
               val {args, blocks, mayInline, name, raises, returns, start} = Function.dest func
               val newBlocks = Vector.map(blocks, loopBlock)
            in
               Function.new {args=args, blocks=newBlocks, mayInline=mayInline, name=name,
                  raises=raises, returns=returns, start=start}
            end

         val newFunctions = List.map (functions, loopFunction)

         (* We do not recurse on global definintions, so if g1 = C1 (g0) and g2 = C2 (g0), we
          * will not duplicate g0. We expect this shouldn't improve much over proper
          * duplication of single-depth and nullary usages *)
         val newGlobals =
            let
               fun globalToClones (Statement.T {exp, ty, var}) =
                  let
                     val newVars =
                        case var of
                             SOME var =>
                                (case HashTable.peek (globalVars, var) of
                                     SOME vs => !vs
                                   | NONE => [])
                           | NONE => []
                  in
                     (Vector.fromList o List.map) (newVars, fn var =>
                        Statement.T {
                           exp = exp,
                           var = SOME var,
                           ty = ty })
                  end
               (* globals that are used in other globals,
                * we want to avoid duplicating to help reduce churn/improve diagonstic data *)
               val usedGlobals: Var.t HashSet.t =
                  HashSet.new {hash=Var.hash}
               val _ = Vector.map (globals, fn Statement.T {exp, ...} =>
                  Exp.foreachVar (exp, fn var =>
                        ignore (HashSet.lookupOrInsert (usedGlobals, Var.hash var,
                           fn var' => Var.equals (var, var'),
                           fn () => var))))
               fun isUsedInGlobals global =
                  case HashSet.peek (usedGlobals, Var.hash global, fn var' => Var.equals (global, var')) of
                       NONE => false
                     | SOME _ => true
               fun shouldKeepOriginal (Statement.T {var=varOpt, ...}) =
                  case varOpt of
                       SOME var =>
                           (case HashTable.peek (globalVars, var) of
                                SOME _ => isUsedInGlobals var
                              | NONE => true (* variable wasn't duplicated *))
                     | NONE => true (* doesn't have a var *)
            in
               Vector.concat [
                  Vector.keepAll (globals, shouldKeepOriginal),
                  Vector.concatV (Vector.map (globals, globalToClones))]
            end
      in
         Program.T {datatypes=datatypes, globals=newGlobals, functions=newFunctions, main=main}
      end
end
