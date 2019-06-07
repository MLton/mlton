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

functor BounceVars(S: RSSA_TRANSFORM_STRUCTS): RSSA_TRANSFORM = 
struct

open S

fun shouldAvoid (Block.T {kind, ...}) =
   (* this definition is important;
    * we assume that no edge has even
    * one side inside a loop;
    * this is true because calls etc
    * must be unconditional;
    * i.e. not be able to get back
    * into the loop without necessarily
    * going over a bad edge *)
   case kind of
        Kind.Jump => true
      | _ => false

fun shouldBounceAt (Block.T {kind, ...}) =
   (* This is based on the allocate
    * registers choice, of which variables
    * go to the stack, and similarly,
    * cannot be frivolously changed *)
   case Kind.frameStyle kind of
        Kind.OffsetsAndSize => true
      | _ => false

structure Weight = struct

   (* t is a positive rational weight: count/loopSize *)
   datatype t = T of {count: IntInf.t, size: IntInf.t}

   val op + =  fn (T {count=count1, size=size1},
                   T {count=count2, size=size2}) =>
      let
         open IntInf
         val l = lcm (size1, size2)
      in
         T {count= count1 * (l div size2) + count2 * (l div size1),
            size=l}
      end
   fun count (T {count, ...}) = count

   fun inc size t = t + T {count=IntInf.fromInt 1, size=IntInf.fromInt size}
   val new = T {count=IntInf.fromInt 0, size=IntInf.fromInt 1}
   val op < = fn (T {count=count1, size=size1},
                  T {count=count2, size=size2}) =>
      let open IntInf in
         count1 * size2 < count2 * size1
      end

end

datatype varinfo
   = Ignore (* No consideration *)
   | UsedInLoop (* Used in loop, haven't checked bouncing *)
   | Consider of Weight.t (* Should be considered for bouncing *)
   | Rewrite of Weight.t (* Will be bounced *)

fun loopForeach ({headers, child}, f) =
     let
        val _ = Vector.foreach (headers, f)
        val {loops, notInLoop} = DirectedGraph.LoopForest.dest child
        val _ = Vector.foreach (notInLoop, f)
     in
        Vector.foreach (loops, fn loop => loopForeach (loop, f))
     end

fun transformFunc func =
   let
      val {args, blocks, name, raises, returns, start} = Function.dest func
      val liveInfo = Live.live (func, {shouldConsider = fn _ => true})
      fun beginNoFormals label = #beginNoFormals ((#labelLive liveInfo) label)

      val {loops, ...} = DirectedGraph.LoopForest.dest
         (Function.loopForest (func, fn (b, _) =>
            shouldAvoid b))

      val {get=varTy, set=setVarTy, ...} = Property.getSetOnce
         (Var.plist, Property.initRaise ("SeparateVars.transformFunc.varTy", Var.layout))

      val _ = Function.foreachDef (func, setVarTy)

      val {get=varInfo, set=setVarInfo, ...} = Property.getSet
         (Var.plist, Property.initConst Ignore)

      datatype InLoop
        = InLoop of {header: bool}
        | NotInLoop

      val {get=labelInfo, ...} = Property.get
         (Label.plist, Property.initFun
            (fn _ => {inLoop=ref NotInLoop, block=ref NONE}))

      val numRewritten = ref 0
      fun setRewrite (v, weight) =
         (case varInfo v of
              Consider _ =>
                  (if Control.optFuelAvailAndUse ()
                  then ( Int.inc numRewritten ; setVarInfo (v, Rewrite weight))
                  else ())
            | _ => ())

      fun checkLoopSize sizeref =
         case !Control.bounceRssaLoopCutoff of
              SOME n => !sizeref < n
            | NONE => true

      (* For each loop, we'll set used variables in that loop
       * to be considered for bouncing, so that we can eliminate some bounce
       * points based on number of variables considered.
       *
       * We can also set some label info at this point *)
      val _ = let
         fun setConsiderVars (block as Block.T {label, ...}) =
           (let
              val _ = Block.foreachUse (block,
                  fn v => setVarInfo (v, UsedInLoop))
              val _ = (#inLoop o labelInfo) label :=
                InLoop {header=false}
           in
             ()
           end)
         fun setHeader (Block.T {label, ...}) =
            let
               val inLoop = (#inLoop o labelInfo) label
            in
               case !inLoop  of
                    InLoop {...} => inLoop := InLoop {header=true}
                  | _ => ()
            end
         fun processLoop (loop as {headers,...}) =
            let
               fun count reff _ = Int.inc reff
               val size = ref 0
               val _ = loopForeach (loop, count size)
               val _ =
                  if checkLoopSize size
                     then loopForeach (loop, setConsiderVars)
                     else ()
               val _ = Vector.foreach (headers, setHeader)
            in
               ()
            end
      in
         Vector.foreach (loops, processLoop)
      end

      (* Now check each bounce point,
       * if it doesn't make the cutoff, set the vars
       * to actually be considered for rewriting *)
      val cutoff =
         case !Control.bounceRssaLiveCutoff of
              SOME n => n
            | NONE => ~1
      val _ = Vector.foreach (blocks,
         fn b as Block.T {label, ...} =>
            let
               val live = beginNoFormals label
            in
               if shouldBounceAt b andalso
                  (cutoff < 0 orelse
                  (Vector.length
                     (Vector.keepAll
                     (live, fn v => UsedInLoop = varInfo v)))
                  < cutoff)
                  then
                     Vector.foreach (live,
                        fn v => setVarInfo (v, Consider Weight.new))
               else ()
            end)
      (* foreach arg, set Consider, since they may need *)
      val _ = Vector.foreach (args,
         fn (x, _) =>
            case varInfo x of
                 UsedInLoop => setVarInfo (x, Consider Weight.new)
               | _ => ())

      (* Finally, vars with Consider are actually worth checking,
       * so set their weights accurately *)
      val _ = let
         fun setVarWeights size (block as Block.T {label, ...}) =
           let
               fun modVarInfo (v, f) =
                  let
                     val newInfo =
                        case varInfo v of
                             Ignore => Ignore
                           | UsedInLoop => UsedInLoop
                           | Consider w => Consider (f w)
                           | Rewrite w => Rewrite (f w)
                     val _ = setVarInfo (v, newInfo)
                  in
                     ()
                  end
              val _ = Block.foreachDef (block,
                  fn (v, _) => modVarInfo (v, Weight.inc size))

              val _ = Block.foreachUse (block,
                  fn v =>
                     case !Control.bounceRssaLimit of
                          NONE => setRewrite (v, Weight.new)
                        | SOME _ => modVarInfo (v,
                             Weight.inc size))
              val _ = (#inLoop o labelInfo) label :=
                InLoop {header=false}
           in
             ()
           end
         fun processLoop loop =
            let
               val size = ref 0
               val _ = loopForeach (loop, fn Block.T {...} => Int.inc size)
               val _ =
                  (* this bound is a conservative bound
                   * backed up by data showing no improvements at
                   * all over this size, so we'll save the overhead *)
                  if checkLoopSize size
                  then loopForeach (loop, setVarWeights (!size))
                  else ()
            in
               ()
            end
      in
         Vector.foreach (loops, processLoop)
      end

      (* Process the choices for each loop in a separate pass,
       * each loop chooses independently which variables to bounce,
       * then those variables are bounced over all loops they're
       * a part of, so they're not inadvertently stack allocated. *)
      fun chooseBouncedVariables n loop =
         let
            (* assume n is small *)
            val heap = Array.new (n, (NONE, Weight.new))
            fun insert (i, x, xw) =
               if i >= n orelse
                     (case !Control.bounceRssaUsageCutoff of
                           SOME n => IntInf.>= (Weight.count xw, IntInf.fromInt n)
                         | NONE => false)
                     (* Variables with lots of uses are usually worse
                      * candidates than shorter lived variables used once or
                      * twice since they have much longer lifespans,
                      * 15 is a conservative bound backed by some data *)
                  then ()
               else
                  let
                     val (y, yw) = Array.sub (heap, i)
                     val (x, xw) =
                        (* maximize weight *)
                        if (Weight.< (yw, xw))
                        then (Array.update (heap, i, (x, xw)) ; (y, yw))
                        else (x, xw)
                  in
                     insert (i + 1, x, xw)
                  end
            fun insertVar x =
               case
                  varInfo x of
                    Consider w => insert (0, SOME x, w)
                    (* May overlap *)
                  | Rewrite w => insert (0, SOME x, w)
                  | _ => ()
            fun insertVars block =
               Block.foreachUse (block, insertVar)
            val _ = loopForeach (loop, insertVars)
            val _ = Array.foreach (heap,
               fn (x, xw) =>
                  case x of
                       SOME x => setRewrite (x, xw)
                     | NONE => ())
         in
            ()
         end
      val _ = case !Control.bounceRssaLimit of
           SOME n => Vector.foreach (loops, chooseBouncedVariables n)
         | NONE => () (* Already chosen when seen *)

      val _ = Control.diagnostics (fn show =>
         let
            open Layout
         in
            show (seq [str "Function ", Func.layout name]) ;
            show (seq [str "Number of loops: ", (str o Int.toString o Vector.length) loops  ]) ;
            show (seq [str "Number of variables rewritten:: ", (str o Int.toString o !) numRewritten  ])
         end)




      datatype direction
         = EnterLoop
         | LeaveLoop
      val newBlocks = ref []

      val _ = Vector.foreach (blocks,
         fn (b as Block.T {label, ...}) =>
            (#block o labelInfo) label := SOME b)

      fun insertRewriteBlock (destLabel, direction) =
         let
            val {block, ...} = labelInfo destLabel
            val Block.T {label=destLabel, args=destArgs, ...} = (valOf o !) block

            val args = Vector.map (destArgs, fn (v, ty) => (Var.new v, ty))
            val live = beginNoFormals destLabel

            val rewrites = Vector.keepAll (live, fn v =>
               case varInfo v of
                    Rewrite _ => true
                  | _ => false)
            val _ = Control.diagnostics (fn show =>
               let
                  open Layout
                  val _ =
                     show (seq [str "Dest Label ", Label.layout destLabel, str " (",
                                str (case direction of EnterLoop => "entrance" | LeaveLoop => "exit"),
                                str ")"])
               in
                  Vector.foreach (rewrites, fn v => show (seq [str "Rewriting ", Var.layout v]))
               end)
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
                     (* This is a bit of a hack, but makes it clear
                      * to the shrinker that these variables should
                      * block copy-propagation. It is preserved by
                      * RestoreRssa *)
                     isMutable= true}
                  end)
            (* Due to the loop forest construction, (i.e. shouldAvoid)
             * The kind of a block on the edge is always Kind.Jump
             * since every non-Jump must be followed by a case
             * transfer to exit the loop conditionally. *)
            val kind = Kind.Jump
            val label = Label.new destLabel
            val jumpArgs = Vector.map (args, fn (v, ty) => Operand.Var {var=v, ty=ty})
            val transfer = Transfer.Goto {dst=destLabel, args=jumpArgs}
            val block = Block.T {args=args, kind=kind, label=label, statements=statements, transfer=transfer}
            val _ = List.push (newBlocks, block)
         in
            label
         end

      fun handleBlock (b as Block.T {args, kind, label, statements, transfer}) =
         let
            val {inLoop, ...} = labelInfo label
            val inLoop = !inLoop
            fun test l =
               let
                  val {inLoop=inLoop', ...} = labelInfo l
                  val inLoop' = !inLoop'
               in
                  case (inLoop, inLoop') of
                       (NotInLoop, InLoop _) => SOME EnterLoop
                     | (InLoop _, NotInLoop) => SOME LeaveLoop
                     | _ => NONE
               end
            val needsRewrite = ref false
            fun rewrite destLabel =
               case test destLabel of
                    NONE => destLabel
                  | SOME dir =>
                       ( needsRewrite := true ;
                         insertRewriteBlock (destLabel, dir))
            val newTransfer = Transfer.replaceLabels(transfer, rewrite)
         in
            if !needsRewrite
               then Block.T {args=args, kind=kind, label=label, statements=statements, transfer=newTransfer}
               else b
         end
      val _ = Vector.foreach (blocks, fn b => List.push (newBlocks, handleBlock b))
      val newBlocks = Vector.fromListRev (!newBlocks)
   in
      Function.new
         {args=args, blocks=newBlocks,
          name=name, raises=raises,
          returns=returns, start=start}
   end


fun transform p =
   let
      val Program.T {functions, handlesSignals, main, objectTypes, profileInfo} = p
      val restore = restoreFunction {main=main}
      val newFunctions = List.map(functions, restore o transformFunc)
   in
      Program.T {functions=newFunctions, handlesSignals=handlesSignals,
                 main=main, objectTypes=objectTypes, profileInfo=profileInfo}
   end

end
