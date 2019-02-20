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
 *
 * This must be run before implement-handlers, as it uses restore
 *)

functor SeparateVars(S: RSSA_TRANSFORM_STRUCTS): RSSA_TRANSFORM = 
struct

open S
open Rssa
structure Live = Live (Rssa)
structure Restore = RestoreR (Rssa)

fun isStackBlock (Block.T {kind, ...}) =
   case kind of
        Kind.CReturn {func} =>
        (case !Control.bounceRssaLocations of
              Control.AnyGC => CFunction.mayGC func
            | Control.GCCollect => CFunction.target func = CFunction.Target.Direct "GC_collect")
            | _ => false

datatype VarInfo
   = Ignore
   | Rewrite

fun transformFunc func =
   let
      val {args, blocks, name, raises, returns, start} = Function.dest func
      val liveInfo = Live.live (func, {shouldConsider = fn _ => true})
      fun beginNoFormals label = #beginNoFormals ((#labelLive liveInfo) label)

      val {loops, ...} = DirectedGraph.LoopForest.dest
         (Function.loopForest (func, fn (b, _) =>
            not (isStackBlock b)))

      val {get=varTy, set=setVarTy, ...} = Property.getSetOnce
         (Var.plist, Property.initRaise ("SeparateVars.transformFunc.varTy", Var.layout))
      val _ = Function.foreachDef (func, setVarTy)

      val {get=varInfo, set=setVarInfo, ...} = Property.getSet
         (Var.plist, Property.initConst Ignore)

      val _ = let
         fun foreachBlock ({headers, child}, f) =
            case DirectedGraph.LoopForest.dest child of
                 {loops, notInLoop} =>
                 let
                    val _ = Vector.foreach (headers, f)
                    val _ = Vector.foreach (notInLoop, f)
                 in
                    Vector.foreach (loops, fn loop => foreachBlock (loop, f))
                 end
         (* now for each var in consideration, check if active in loop *)
         fun checkActiveVars block = Block.foreachUse (block, fn v => setVarInfo (v, Rewrite))
         fun processLoop loop = foreachBlock (loop, checkActiveVars)
      in
         Vector.foreach (loops, processLoop)
      end


      val remappedVars: (Var.t * Label.t, Var.t) HashTable.t = HashTable.new {
         equals=fn ((v, l), (v', l')) => Var.equals (v, v') andalso Label.equals (l, l'),
         hash=fn (v, l) => Hash.combine (Var.hash v, Label.hash l)}
      (* map based on the destination block, on the extremely unlikely chance that
       * the return block is a join point from two GCs *)
      fun remappedVar {var: Var.t, dest: Label.t} =
         case varInfo var of
              Rewrite => SOME (HashTable.lookupOrInsert (remappedVars, (var, dest), fn () => Var.new var))
            | _ => NONE
      val {set=remapBlock, get=getRemappedBlock, ...} = Property.getSetOnce
         (Label.plist, Property.initConst NONE)
      val {set=setBlockingLabel, get=blockingLabel, ...} = Property.getSetOnce
         (Label.plist, Property.initConst NONE)
      val _ = Vector.foreach (Function.blocks func,
         fn b as Block.T {label, ...} =>
            if isStackBlock b
               then setBlockingLabel (label, SOME b)
               else ())

      fun rewriteDestBlock (Block.T {args, kind, label, statements, transfer}, rewrites) =
         let
            val newStatements = Vector.map(rewrites,
               fn (old, new, ty) => Statement.Bind
                     (* Unlike below, we're fine letting this get copy-propagated,
                      * since it usually won't make it any farther than a phi argument *)

            val newBlock =
               Block.T {args=args, kind=kind, label=label,
                  statements=Vector.concat [statements, newStatements],
                  transfer=transfer}
            val _ = remapBlock (label, SOME newBlock)
         in
            ()
         end

      fun rewriteSourceBlock (Block.T {args, kind, label, statements, transfer}, destLabel) =
         let
            val varsToConsider = beginNoFormals destLabel
            val newVars = Vector.keepAllMap (varsToConsider,
               fn v => Option.map (remappedVar {var=v, dest=destLabel}, fn v' => (v,v')))
            val rewrites = Vector.map (newVars,
               fn (old, new) => (old, new, varTy old))

            val _ = if isSome (getRemappedBlock destLabel)
               then ()
               else rewriteDestBlock (valOf (blockingLabel destLabel), rewrites)

            val newStatements = Vector.map(rewrites,
               fn (old, new, ty) => Statement.Bind
                     (* we set isMutable = true primarily so that the simplifier
                      * won't copy-propagate; it's easier for us with advance knowleddge
                      * than to add any lifetime analysis to the copy propagator. 
                      * And in some sense, the variables are mutable, just
                      * mutated by the RTS *)
                     {dst=(new, ty), isMutable=true, src=Operand.Var {ty=ty, var=old}})
            val newBlock =
               Block.T {args=args, kind=kind, label=label,
                  statements=Vector.concat [statements, newStatements],
                  transfer=transfer}
            val _ = remapBlock (label, SOME newBlock)
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
      val _ = Vector.foreach (Function.blocks func,
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

      val newBlocks = Vector.map (blocks, fn block as Block.T {label, ...} =>
         case getRemappedBlock label of
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
      val restore = Restore.restoreFunction {main=main}
      val newFunctions = List.map(functions, restore o transformFunc)
   in
      Program.T {functions=newFunctions, handlesSignals=handlesSignals,
                 main=main, objectTypes=objectTypes}
   end

end
