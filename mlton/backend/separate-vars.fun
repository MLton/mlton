(* Copyright (C) 2019 Jason Carr
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *
 * This pass copies some variables into new locations
 * so that the relatively naïve AllocateVars can give one location
 * to each variable.
 *
 * Although this is pretty tightly coupled to the AllocateVars, it saves
 * us a lot of complexity in both, since the latter does not need to do any
 * contortions to try to return multiple variables
 *
 * Mechanically, what we do is find the loops in the program, and try our best
 * to split up variables so that no variable that would have to be on the
 * stack appears in any loop. So while we might end up with some extra copies,
 * by construction we don't really do so in any loop where we wouldn't have to
 * already do something similar in practice.
 *)

functor SeparateVars(S: RSSA_TRANSFORM_STRUCTS): RSSA_TRANSFORM = 
struct

open S
open Rssa
structure Live = Live (Rssa)
structure Restore = RestoreR (Rssa)

fun foreachBlock f {headers, child} =
   case DirectedGraph.LoopForest.dest child of
        {loops, notInLoop} =>
        let
           val _ = Vector.foreach (headers, f)
           val _ = Vector.foreach (notInLoop, f)
        in
           Vector.foreach (loops, foreachBlock f)
        end

fun processLoop ({labelLive, remLabelLive}, varTy, setLabelBlock) tree =
   let
      (* in each processLoop call we need to loop twice
       * first we find register-blocking points, and record live vars
       * then for each var live over a blocking point, check if active
       * in loop. If it's active we rewrite the appropriate blocks with a new assignment *)

      val blockingLabels = Buffer.new {dummy=Label.new ()}

      datatype VarInfo =
         Skip
       | Consider
       | Rewrite
      val {get=blockedVars, set=setBlockedVar, destroy=destroyBlockedVars} =
         Property.destGetSet (Var.plist, Property.initConst Skip)

      (* Check which blocks block local vars and mark all live vars for consideration *)
      fun markBlock (Block.T {kind, label, ...}) =
         (* note this means that the block is *within* the loop.
          * We don't expect it, but this means that no destination can be referenced in two separate loops *)
         case Kind.frameStyle kind of
            Kind.OffsetsAndSize =>
               ( Buffer.add (blocking, label)
               ; Vector.foreach (#begin o labelLive label, fn v => setBlockedVar (v, Consider)))
          | _ => ()
      val _ = foreachBlock markBlock tree

      (* now for each var in consideration, check if active in loop *)
      fun checkActiveVars block =
         Block.foreachUse (block,
            fn v =>
               case blockedVars v of
                    Consider => setBlockedVar (v, Rewrite)
                  | NONE => ())
      val _ = foreachBlock checkActiveVars tree

      val remappedVars: (Var.t * Label.t, Var.t) HashTable.t = HashTable.new {
         equals=fn ((v, l), (v', l')) => Var.equals (v, v') andalso Label.equals (l, l'),
         hash=fn (v, l) => Hash.combine (Var.hash v, Label.hash l)}
      (* map based on the destination block, on the extremely unlikely chance that
       * the return block is a join point from two GCs *)
      fun getVar {var: Var.t, dest: Label.t} =
         case blockedVars var of
              Rewrite => HashTable.lookupOrInsert (remappedVars, (var, dest), fn () => Var.new var)
            | _ => var

      fun rewriteDestBlock (Block.T {args, kind, label, statements, transfer}) =
         let
         in
            ()
         end

      fun rewriteSourceBlock (Block.T {args, kind, label, statements, transfer}) dest =
         let
            val varsToConsider = #begin o labelLive dest
            fun toOp v = Operand.Var {ty=varTy v, var=v}
            val newStatements = Vector.keepAllMap (varsToConsider,
               fn v =>
                  let
                     val newVar = getVar {var=v, dest=dest}
                  in
                     if Var.equals (v, newVar)
                     then NONE
                     else SOME (Statement.Move {src=toOp v, dest=toOp newVar})
                  end)
            val _ = rewriteDestBlock block
            val newBlock =
               Block.T {args=args, kind=kind, label=label,
                  statements=Vector.concat(statements, newStatements),
                  transfer=transfer}
            val _ = remap (label, newBlock)
         in
            ()
         end

      val _ = destroyBlockedVars ()
   in
      ()
   end

fun transformFunc func =
   let
      val {args, blocks, name, raises, returns, start} = Function.dest func
      val liveness = Live.live (func, {shouldConsider = fn _ => true})
      val {loops, ...} = DirectedGraph.LoopForest.dest
         (Function.loopForest (func, fn (Block.T {kind, ...}, _) => true))
      val {get=varTy, set=setVarTy, ...} = Property.getSetOnce
         (Var.plist, Property.initRaise ("SeparateVars.transformFunc.varTy", Var.layout))
      val _ = Function.foreachDef (func, setVarTy)


      val {set=remap, get=getRemapped, ...} = Property.getSetOnce
         (Label.plist, Property.initConst NONE)

      val _ = Vector.foreach (loops, processLoop (liveness, varTy, fn (l, b) => remap (l, SOME b)))

      val newBlocks = Vector.map (blocks, fn block as Block.T {label, ...} =>
         case getRemapped label of
              SOME newBlock => newBlock
            | NONE => block)
   in
      Function.new
         {args=args, blocks=newBlocks,
          name=name, raises=raises,
          returns=returns, start=start}
   end


fun transform p =
   let
      val Program.T {functions, handlesSignals, main, objectTypes} = p
      val restore = Restore.restoreFunction ()
      val newFunctions = List.map(functions, restore o transformFunc)
   in
      Program.T {functions=newFunctions, handlesSignals=handlesSignals,
                 main=main, objectTypes=objectTypes}
   end

end
