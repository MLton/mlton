(* Copyright (C) 2018 Jason Carr
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *
 * Duplicate each global that appears in the program, once per usage in the program.
 * Globals which refer to other globals are not duplicated recursively.
 *
 * The primary purpose of this pass is to support passes which could possibly use
 * different types for different globals, currently SplitTypes, and so that these
 * passes may be separately enabled
 *)
functor DuplicateGlobals(S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM =
struct

   open S

   fun transform (Program.T {datatypes, globals, functions, main}): Program.t =
      let
         val globalVars: (Var.t, Var.t list ref) HashTable.t
            = HashTable.new {hash=Var.hash, equals=Var.equals}
         val _ = Vector.foreach (globals, fn st => case Statement.var st of
               SOME var => ignore (HashTable.lookupOrInsert (globalVars, var, fn () => ref []))
             | NONE => ())
         fun freshenIfGlobal (var: Var.t) =
            case HashTable.peek (globalVars, var) of
                 NONE => var
               | SOME vs =>
                    let
                       val newVar = Var.new var
                       val _ = vs := (newVar :: !vs)
                    in
                       newVar
                    end
         fun freshenVec (vars: Var.t vector) = Vector.map (vars, freshenIfGlobal)

         (* so what's going on here is that each *usage* of a global will be duplicated *)
         fun loopExp exp =
            case exp of
                 Exp.ConApp {args, con} => Exp.ConApp {con=con, args= freshenVec args}
               | Exp.PrimApp {args, prim, targs} =>
                    Exp.PrimApp {args= freshenVec args, prim=prim, targs=targs}
               | Exp.Select {offset, tuple} =>
                    Exp.Select {offset=offset, tuple= freshenIfGlobal tuple}
               | Exp.Tuple vs => Exp.Tuple (freshenVec vs)
               | Exp.Var v => Exp.Var (freshenIfGlobal v)
               | _ => exp
         fun loopStatement (Statement.T {exp, ty, var}) =
            (* in the main program tree, the variable won't be a global, but we'll loop over
             * globals since some may depend on each other *)
            Statement.T {exp=loopExp exp, ty=ty, var=Option.map (var, freshenIfGlobal)}

         fun loopTransfer transfer =
            case transfer of
                 Transfer.Arith {args, overflow, prim, success, ty} =>
                    Transfer.Arith {args= freshenVec args, overflow=overflow, prim=prim, success=success, ty=ty}
               | Transfer.Bug => Transfer.Bug
               | Transfer.Call {args, func, return} =>
                    Transfer.Call {args= freshenVec args, func=func, return=return}
               | Transfer.Case {cases, default, test} =>
                    Transfer.Case {cases=cases, default=default, test= freshenIfGlobal test}
               | Transfer.Goto {args, dst} =>
                    Transfer.Goto {args= freshenVec args, dst=dst}
               | Transfer.Raise vs =>
                    Transfer.Raise (freshenVec vs)
               | Transfer.Return vs =>
                    Transfer.Return (freshenVec vs)
               | Transfer.Runtime {args, prim, return} =>
                    Transfer.Runtime {args=freshenVec args, prim=prim, return=return}
         fun loopBlock (Block.T {args, label, statements, transfer}) =
            Block.T {args = args, label=label,
               statements = Vector.map (statements, loopStatement),
               transfer = loopTransfer transfer}
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
          * duplication of single-depth usages *)
         val newGlobals =
            let
               fun globalToClones (Statement.T {exp, ty, var}) =
                  let
                     val realVar = case var of
                        SOME v => v
                      | NONE => Error.bug "SplitTypes.duplicateGlobals.newGlobals: Global variable was NONE"
                     val newVars = case HashTable.peek (globalVars, realVar) of
                        SOME vs => !vs
                      (* it's possible a global is referenced only in globals, not in the program *)
                      | NONE => []
                  in
                     (Vector.fromList o List.map) (newVars, fn var =>
                        Statement.T {
                           exp = exp,
                           var = SOME (Var.new var),
                           ty = ty })
                  end
            in
               Vector.concat [globals, Vector.concatV (Vector.map (globals, globalToClones))]
            end
      in
         Program.T {datatypes=datatypes, globals=newGlobals, functions=newFunctions, main=main}
      end
end
