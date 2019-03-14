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

fun shouldBounceAt (Block.T {kind, ...}) =
   case kind of
        Kind.Jump => false
      | _ => true
        (*
        Kind.CReturn {func} => true
        (case !Control.bounceRssaLocations of
              Control.AnyGC => CFunction.mayGC func
            | Control.GCCollect => CFunction.target func = CFunction.Target.Direct "GC_collect"
            | _ => false)
      | Kind.Cont _ => true
      | _ => false*)

structure Weight = struct
   type t = {depth: int, count: int}
   fun inc ({depth, count}, depth') =
      if depth' > depth
         then {depth=depth', count=1}
      else if depth' = depth
         then {depth=depth', count=count + 1}
      else {depth=depth, count=count}
   fun new depth =
      {depth=depth, count=0}
   fun op < ({depth=depth1, count=count1},
             {depth=depth2, count=count2}) =
      if Int.< (depth1, depth2)
         then true
      else if Int.< (count1, count2)
         then true
      else false
end

datatype varinfo
   = Ignore
   | Consider of Weight.t
   | Rewrite

fun loopForeach (depth, {headers, child}, f) =
   case DirectedGraph.LoopForest.dest child of
        {loops, notInLoop} =>
        let
           val _ = Vector.foreach (headers, f depth)
           val _ = Vector.foreach (notInLoop, f depth)
        in
           Vector.foreach (loops, fn loop => loopForeach (depth, loop, f))
        end

fun transformFunc func =
   let
      val {args, blocks, name, raises, returns, start} = Function.dest func
      val liveInfo = Live.live (func, {shouldConsider = fn _ => true})
      fun beginNoFormals label = #beginNoFormals ((#labelLive liveInfo) label)

      val {loops, ...} = DirectedGraph.LoopForest.dest
         (Function.loopForest (func, fn (b, _) =>
            not (shouldBounceAt b)))

      val {get=varTy, set=setVarTy, ...} = Property.getSetOnce
         (Var.plist, Property.initRaise ("SeparateVars.transformFunc.varTy", Var.layout))

      val _ = Function.foreachDef (func, setVarTy)

      val {get=varInfo, set=setVarInfo, ...} = Property.getSet
         (Var.plist, Property.initConst Ignore)

      val _ = Vector.foreach (blocks,
         fn b as Block.T {label, ...} =>
            if shouldBounceAt b
            then Vector.foreach (beginNoFormals label,
               fn v => setVarInfo (v, Consider (Weight.new 0)))
            else ())

      val {get=labelInfo, ...} = Property.get
         (Label.plist, Property.initFun
            (fn _ => {inLoop=ref false, block=ref NONE}))

      val n = !Control.bounceRssaLimit
      val _ = Control.diagnostic (fn () =>
         let
            open Layout
         in
            seq [str "Bounce rssa limit: ", Option.layout Int.layout n]
         end)
      val _ = let
         (* now for each var in consideration, check if active in loop,
          * increment its usage for each depth *)
         fun checkActiveVars depth (block as Block.T {label, ...}) =
           let
              fun incVarInfo v =
                 let
                    val newInfo =
                       case varInfo v of
                            Ignore => Ignore
                          | Consider d => Consider (Weight.inc (d, depth))
                          | Rewrite => Error.bug "Unexpected Rewrite"
                    val _ = setVarInfo (v, newInfo)
                 in
                    ()
                 end
              val _ = Block.foreachUse (block,
                  fn v =>
                     case n of
                          NONE => setVarInfo (v, Rewrite)
                        | SOME _ => incVarInfo v)
              val _ = (#inLoop o labelInfo) label := true
           in
             ()
           end
         fun processLoop loop = loopForeach (1, loop, checkActiveVars)
      in
         Vector.foreach (loops, processLoop)
      end
      (* Process variables in two passes so that
       * the choices are consistent amongst different loops;
       * combined weights are taken into account and every variable
       * is either rewritten or ignored *)
      fun mkLoopPicker n loop =
         let
            (* assume n is small, else we should use a proper heap *)
            val heap = Array.new (n, (NONE, Weight.new 0))
            fun insert (i, x, xw) =
               if i >= n
                  then ()
               else
                  let
                     val (y, yw) = Array.sub (heap, i)
                     val (x, xw) =
                        (* maximize weight *)
                        if (Weight.< (yw, xw))
                        then ( Array.update (heap, i, (x, xw)) ; (y, yw))
                        else (x, xw)
                  in
                     insert (i + 1, x, xw)
                  end
            fun insertVar x =
               case
                  varInfo x of
                    Consider w => insert (0, SOME x, w)
                  | _ => ()
            fun insertVars _ block =
               Block.foreachUse (block, insertVar)
            val _ = loopForeach (0, loop, insertVars)
            val _ = Array.foreach (heap,
               fn (x, _) =>
                  case x of
                       SOME x => setVarInfo (x, Rewrite)
                     | NONE => ())
         in
            ()
         end
      val _ = case n of
           SOME n => Vector.foreach (loops, mkLoopPicker n)
         | NONE => ()

      val _ = Vector.foreach (blocks,
         fn (b as Block.T {label, ...}) =>
            (#block o labelInfo) label := SOME b)

      datatype direction
         = EnterLoop
         | LeaveLoop

      val newBlocks = ref []


      fun insertRewriteBlock (destLabel, direction) =
         let
            val {block, ...} = labelInfo destLabel
            val Block.T {label=destLabel, args=destArgs, ...} = (valOf o !) block

            val args = Vector.map (destArgs, fn (v, ty) => (Var.new v, ty))
            val live = beginNoFormals destLabel

            val rewrites = Vector.keepAll (live, fn v => varInfo v = Rewrite)
            val statements = Vector.map (rewrites,
               fn v =>
                  let
                     val ty = varTy v
                     val (src, dst) =
                        case direction of
                             EnterLoop => (v, v)
                           | LeaveLoop => (v, v)
                     val src = Operand.Var {var=src, ty=ty}
                     val dst = (dst, ty)
                  in
                     Statement.Bind {dst=dst, src=src,
                     (* temporary hack *)
                     isMutable=true}
                  end)
            (* we use our condition here, the kind of a block on the edge
             * of a loop is never anything interesting, since calls are
             * unconditional, there is always an intervening block
             * that would not be in the loop *)
            val kind = Kind.Jump
            val label = Label.new destLabel
            val jumpArgs = Vector.map (args, fn (v, ty) => Operand.Var {var=v, ty=ty})
            val transfer = Transfer.Goto {dst=destLabel, args=jumpArgs}
            val block = Block.T {args=args, kind=kind, label=label, statements=statements, transfer=transfer}
            val _ = List.push (newBlocks, block)
         in
            label
         end

      fun rewriteBlock (b as Block.T {args, kind, label, statements, transfer}) =
         let
            val {inLoop, ...} = labelInfo label
            val inLoop = !inLoop
            val direction = if inLoop then LeaveLoop else EnterLoop
            fun test l =
               let
                  val {inLoop=inLoop', ...} = labelInfo l
                  val inLoop' = !inLoop'
               in
                  not (inLoop = inLoop')
               end
            val r = ref false
            fun rewrite destLabel =
               if test destLabel
               then ( r := true ; insertRewriteBlock (destLabel, direction))
               else destLabel
            val newTransfer = Transfer.replaceLabels(transfer, rewrite)
            val newBlock = Block.T {args=args, kind=kind, label=label, statements=statements, transfer=newTransfer}
         in
            (* save some garbage if we don't need to change *)
            if !r
               then newBlock
               else b
         end

      val _ = Vector.foreach (blocks, fn b => List.push (newBlocks, rewriteBlock b))
      val newBlocks = Vector.fromListRev (!newBlocks)
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
