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

fun foreachBlock ({headers, child}, f) =
   case DirectedGraph.LoopForest.dest child of
        {loops, notInLoop} =>
        let
           val _ = Vector.foreach (headers, f)
           val _ = Vector.foreach (notInLoop, f)
        in
           Vector.foreach (loops, fn loop => foreachBlock (loop, f))
        end

fun processLoop ({labelLive: (* smlnj can't figure it out *)
             Label.t -> { begin: Var.t vector, beginNoFormals: Var.t vector,
                        handler: Label.t option, link: bool}, remLabelLive},
      varTy, setLabelBlock, labelBlock) tree =
   let
      (* in each processLoop call we need to loop twice
       * first we find register-blocking points, and record live vars
       * then for each var live over a blocking point, check if active
       * in loop. If it's active we rewrite the appropriate blocks with a new assignment *)

      datatype VarInfo =
         Skip
       | Consider
       | Rewrite
      val {get=blockedVar, set=setBlockedVar, destroy=destroyBlockedVars} =
         Property.destGetSet (Var.plist, Property.initConst Skip)
      val {get=blockingLabel, set=setBlockingLabel, destroy=destroyBlockingLabels} =
         Property.destGetSet (Label.plist, Property.initConst NONE)

      (* Check which blocks block local vars and mark all live vars for consideration *)
      fun markBlock (b as Block.T {kind, label, ...}) =
         (* note this means that the block is *within* the loop.
          * We don't expect it, but this means that no destination can be referenced in two separate loops *)
         case kind of
            Kind.CReturn {func} =>
               if CFunction.mayGC func then
                  ( setBlockingLabel (label, SOME b)
                  ; Vector.foreach (#beginNoFormals (labelLive label), fn v => setBlockedVar (v, Consider)))
               else ()
          | _ => ()
      val _ = foreachBlock (tree, markBlock)

      (* now for each var in consideration, check if active in loop *)
      fun checkActiveVars block =
         Block.foreachUse (block,
            fn v =>
               case blockedVar v of
                    Consider => setBlockedVar (v, Rewrite)
                  | _ => ())
      val _ = foreachBlock (tree, checkActiveVars)

      val remappedVars: (Var.t * Label.t, Var.t) HashTable.t = HashTable.new {
         equals=fn ((v, l), (v', l')) => Var.equals (v, v') andalso Label.equals (l, l'),
         hash=fn (v, l) => Hash.combine (Var.hash v, Label.hash l)}
      (* map based on the destination block, on the extremely unlikely chance that
       * the return block is a join point from two GCs *)
      fun remappedVar {var: Var.t, dest: Label.t} =
         case blockedVar var of
              Rewrite => SOME (HashTable.lookupOrInsert (remappedVars, (var, dest), fn () => Var.new var))
            | _ => NONE

      fun rewriteDestBlock (Block.T {args, kind, label, statements, transfer}, rewrites) =
         let
            val newStatements = Vector.map(rewrites,
               fn (old, new, ty) => Statement.Bind
                     {dst=(old, ty), isMutable=false, src=Operand.Var {ty=ty, var=new}})
            val newBlock =
               Block.T {args=args, kind=kind, label=label,
                  statements=Vector.concat [statements, newStatements],
                  transfer=transfer}
            val _ = setLabelBlock (label, newBlock)
         in
            ()
         end

      fun rewriteSourceBlock (Block.T {args, kind, label, statements, transfer}, destLabel) =
         if isSome (labelBlock destLabel)
         then ()
         else
            let
               val varsToConsider = #beginNoFormals (labelLive destLabel)
               fun toOp v = Operand.Var {ty=varTy v, var=v}
               val newVars = Vector.keepAllMap (varsToConsider,
                  fn v => Option.map (remappedVar {var=v, dest=destLabel}, fn v' => (v,v')))
               val rewrites = Vector.map (newVars,
                  fn (old, new) => (old, new, varTy old))

               val _ = rewriteDestBlock (valOf (blockingLabel destLabel), rewrites)

               val newStatements = Vector.map(rewrites,
                  fn (old, new, ty) => Statement.Bind
                        {dst=(new, ty), isMutable=false, src=Operand.Var {ty=ty, var=old}})
               val newBlock =
                  Block.T {args=args, kind=kind, label=label,
                     statements=Vector.concat [statements, newStatements],
                     transfer=transfer}
               val _ = setLabelBlock (label, newBlock)
            in
               ()
            end

      fun anyLabel (transfer, p) =
         let
            val r = ref false
            val _ = Transfer.foreachLabel (transfer, fn l => if p l then r := true else ())
         in
            !r
         end
      val _ = foreachBlock (tree,
         fn (b as Block.T {transfer, ...}) =>
            if (anyLabel (transfer, isSome o blockingLabel))
            (* assumes only one blocking dest *)
            then Transfer.foreachLabel (transfer,
               let
                  val r = ref true
               in
                  fn l =>
                     if !r andalso isSome (blockingLabel l)
                     then (r := false; rewriteSourceBlock (b, l))
                     else ()
               end)
            else ()
      )

      val _ = destroyBlockingLabels ()
      val _ = destroyBlockedVars ()
   in
      ()
   end

fun transformFunc func =
   let
      val {args, blocks, name, raises, returns, start} = Function.dest func
      val liveness = Live.live (func, {shouldConsider = fn _ => true})
      val {loops, ...} = DirectedGraph.LoopForest.dest
         (Function.loopForest (func, fn (_, _) => true))
      val {get=varTy, set=setVarTy, ...} = Property.getSetOnce
         (Var.plist, Property.initRaise ("SeparateVars.transformFunc.varTy", Var.layout))
      val _ = Function.foreachDef (func, setVarTy)


      val {set=remap, get=getRemapped, ...} = Property.getSetOnce
         (Label.plist, Property.initConst NONE)

      val _ = Vector.foreach (loops, processLoop (liveness, varTy, fn (l, b) => remap (l, SOME b), getRemapped))

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
