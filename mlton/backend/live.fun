(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *

 * This pass is based on the liveness algorithm described in section 4.13,
 * page 132, of Morgan's "Building an Optimizing Compiler".  BTW, the Dragon
 * book and Muchnick's book provided no help at all on speeding up liveness.
 * They suggest using bit-vectors, which is infeasible for MLton due to the
 * large size of and number of variables in SSA functions.
 *
 * Here is a description of the algorithm.
 *
 * Walk over the whole program and
 * 1. Build the predecessor graph of basic blocks.  Each basic block records the
 *    set of its predecessors and the set of variables live at the beginning of
 *    the block.
 * 2. For each variable record the block in which is defined and the list of
 *    blocks where it is used.
 *
 * Now, for each variable, propagate the liveness information backwards from uses
 * along basic blocks until the definition block is reached.
 *
 * That's it.  The reason why it's so fast is that it processes one variable at a
 * time, and hence the operation to determine if that variable is in the live
 * list for a particular block is constant time -- the variable is either at the
 * head of the list or it's not there.
 *)
functor Live (S: LIVE_STRUCTS): LIVE = 
struct

open S
datatype z = datatype Statement.t
datatype z = datatype Transfer.t

structure LiveInfo =
   struct
      datatype t = T of {live: Var.t Buffer.t,
                         liveHS: {handler: Label.t option ref,
                                  link: unit option ref},
                         name: string,
                         preds: t list ref}

      fun layout (T {name, ...}) = Layout.str name

      fun new (name: string) =
         T {live = Buffer.new {dummy = Var.bogus},
            liveHS = {handler = ref NONE,
                      link = ref NONE},
            name = name,
            preds = ref []}

      fun live (T {live, ...}) = Buffer.toVector live

      fun liveHS (T {liveHS = {handler, link}, ...}) =
         {handler = !handler,
          link = isSome (!link)}

      fun equals (T {preds = r, ...}, T {preds = r', ...}) = r = r'

      fun addEdge (b, T {preds, ...}) =
         if List.exists (!preds, fn b' => equals (b, b'))
            then ()
         else List.push (preds, b)

      val addEdge =
         Trace.trace2 
         ("Live.LiveInfo.addEdge", layout, layout, Unit.layout) 
         addEdge
   end

val traceConsider = 
   Trace.trace ("Live.consider", LiveInfo.layout, Bool.layout)

fun live (function, {shouldConsider: Var.t -> bool}) =
   let
      val shouldConsider =
         Trace.trace ("Live.shouldConsider", Var.layout, Bool.layout)
         shouldConsider
      val {args, blocks, ...} = Function.dest function
      val _ =
         Control.diagnostic
         (fn () =>
          let
             val numVars = ref 0
             fun loopVar (x, _) =
                if shouldConsider x
                   then Int.inc numVars
                else ()
             fun loopFormals v = Vector.foreach (v, loopVar)
             val () =
                Vector.foreach
                (blocks, fn Block.T {args, statements, transfer, ...} =>
                 (loopFormals args
                  ; Vector.foreach (statements, fn s =>
                                    Statement.foreachDef (s, loopVar))
                  ; Transfer.foreachDef (transfer, loopVar)))
             open Layout
          in
             align [seq [str "Live info for ",
                         Func.layout (Function.name function)],
                    seq [str "  num blocks ", Int.layout (Vector.length blocks)],
                    seq [str "  num vars ", Int.layout (!numVars)]]
          end)
      val {get = labelInfo: Label.t -> {argInfo: LiveInfo.t,
                                        block: Block.t,
                                        bodyInfo: LiveInfo.t},
           rem = removeLabelInfo,
           set = setLabelInfo, ...} =
         Property.getSetOnce (Label.plist,
                              Property.initRaise ("live info", Label.layout))
      val {get = varInfo: Var.t -> {defined: LiveInfo.t option ref,
                                    used: LiveInfo.t list ref},
           rem = removeVarInfo, ...} =
         Property.get (Var.plist,
                       Property.initFun (fn _ => {defined = ref NONE,
                                                  used = ref []}))
      datatype 'a defuse = Def of LiveInfo.t | Use of 'a * LiveInfo.t
      val handlerCodeDefUses: Label.t defuse list ref = ref []
      val handlerLinkDefUses: unit defuse list ref = ref []
      val allVars: Var.t list ref = ref []
      fun setDefined (x: Var.t, defined): unit =
         if shouldConsider x
            then (List.push (allVars, x)
                  ; #defined (varInfo x) := SOME defined)
         else ()
      val setDefined =
         Trace.trace2 ("Live.setDefined",
                       Var.layout, LiveInfo.layout, Unit.layout)
         setDefined
      (* Set the labelInfo for each block. *)
      val _ =
         Vector.foreach
         (blocks, fn block as Block.T {args, label, ...} =>
          let
             val name = Label.toString label
             val (argInfo, bodyInfo) =
                case Vector.length args of
                   0 => let val b = LiveInfo.new (name ^ "a")
                        in (b, b)
                        end
                 | _ => let val b = LiveInfo.new (name ^ "b")
                            val b' = LiveInfo.new (name ^ "c")
                            val _ = LiveInfo.addEdge (b, b')
                        in (b, b')
                        end
          in
             setLabelInfo (label, {argInfo = argInfo,
                                   block = block,
                                   bodyInfo = bodyInfo})
          end)
      (* Add the control-flow edges and set the defines and uses for each
       * variable.
       *)
      val head = LiveInfo.new "main"
      val _ = Vector.foreach (args, fn (x, _) => setDefined (x, head))
      val _ =
         Vector.foreach
         (blocks,
          fn Block.T {args, kind, label, statements, transfer, ...} =>
          let
            val {argInfo, bodyInfo = b, ...} = labelInfo label
            val _ = Vector.foreach (args, fn (x, _) => setDefined (x, argInfo))
            fun goto l = LiveInfo.addEdge (b, #argInfo (labelInfo l))
            (* Make sure that a cont's live vars includes variables live in its
             * handler.
             *)
            val _ =
               case kind of
                  Kind.Cont {handler, ...} =>
                     Handler.foreachLabel (handler, goto)
                | _ => ()
            fun define (x: Var.t): unit = setDefined (x, b)
            fun use (x: Var.t): unit =
               if shouldConsider x
                  then
                     let val {used, ...} = varInfo x
                     in
                        if (case !used of
                               [] => false
                             | b' :: _ => LiveInfo.equals (b, b'))
                           then ()
                        else List.push (used, b)
                     end
               else ()
            val use = Trace.trace ("Live.use", Var.layout, Unit.layout) use
            val _ =
               Vector.foreach
               (statements, fn s =>
                let
                   val _ = Statement.foreachDefUse (s, {def = define o #1,
                                                        use = use})
                   val _ =
                      case s of
                         SetExnStackSlot =>
                            List.push (handlerLinkDefUses, Use ((), b))
                       | SetHandler _ =>
                            List.push (handlerCodeDefUses, Def b)
                       | SetSlotExnStack =>
                            List.push (handlerLinkDefUses, Def b)
                       | _ => ()
                in
                   ()
                end)
            fun label l =
               let
                  val {block = Block.T {kind, ...}, ...} = labelInfo l
               in
                  case kind of
                     Kind.Handler =>
                        List.push (handlerCodeDefUses, Use (l, b))
                   | _ => goto l
               end
            val _ =
               Transfer.foreachDefLabelUse (transfer, {def = define o #1,
                                                       label = label,
                                                       use = use})
          in ()
          end)
      (* Back-propagate every variable from uses to define point. *)
      fun processVar (x: Var.t): unit =
         if not (shouldConsider x)
            then ()
         else
            let
               val {defined, used, ...} = varInfo x
               val defined = valOf (!defined)
               val todo: LiveInfo.t list ref = ref []
               fun consider (b as LiveInfo.T {live, ...}) =
                  if LiveInfo.equals (b, defined)
                     orelse (case Buffer.last live of
                                NONE => false
                              | SOME x' => Var.equals (x, x'))
                     then false
                  else (Buffer.add (live, x)
                        ; List.push (todo, b)
                        ; true)
               val consider = traceConsider consider
               val consider = ignore o consider
               val _ = List.foreach (!used, consider)
               fun loop () =
                  case !todo of
                     [] => ()
                   | LiveInfo.T {preds, ...} :: bs =>
                        (todo := bs
                         ; List.foreach (!preds, consider)
                         ; loop ())
               val _ = loop ()
            in ()
            end
      val processVar =
         Trace.trace ("Live.processVar", Var.layout, Unit.layout) processVar
      val _ = List.foreach (!allVars, processVar)
      val _ = Function.foreachDef (function, fn (x, _) => removeVarInfo x)
      (* handler code and link slots are harder; in particular, they don't
       * satisfy the SSA invariant -- there are multiple definitions;
       * furthermore, a def and use in a block does not mean that the def 
       * occurs before the use.  But, a back propagated use will always
       * come after a def in the same block
       *)
      fun handlerLink (defuse: 'a defuse list ref,
                       sel: {handler: Label.t option ref,
                             link: unit option ref} -> 'a option ref) =
         let
            val todo: ('a * LiveInfo.t) list ref = ref []
            (* The foldr is important because the statements in each block were
             * visited in order, meaning that the earlier statements appear
             * later in !defuse.  Hence, with the foldr, the defs and uses are
             * visited in order for each block.
             *)
            val defs =
               List.foldr
               (!defuse, [], fn (du, defs) =>
                case du of
                   Def b => b::defs
                 | Use (a, b as LiveInfo.T {liveHS, ...}) =>
                      let
                         val _ =
                            if
                               (* Since we are visiting all of the statements
                                * in the block together, in order, we are
                                * guaranteed that if there is a prior definition
                                * then it will be first on defs.
                                *)
                               (case defs of 
                                   [] => false
                                 | b' :: _ => LiveInfo.equals (b, b'))
                               then ()
                            else (sel liveHS := SOME a
                                  ; List.push (todo, (a, b)))
                      in
                         defs
                      end)
            fun consider (b as LiveInfo.T {liveHS, ...}, a: 'a) =
               if List.exists (defs, fn b' => LiveInfo.equals (b, b'))
                  orelse isSome (!(sel liveHS))
                  then ()
               else (sel liveHS := SOME a
                     ; List.push (todo, (a, b)))
            fun loop () =
               case !todo of
                  [] => ()
                | (a, LiveInfo.T {preds, ...}) :: bs =>
                     (todo := bs
                      ; List.foreach (!preds, fn b => consider (b, a))
                      ; loop ())
            val _ = loop ()
         in
            ()
         end
      val _ = handlerLink (handlerCodeDefUses, #handler)
      val _ = handlerLink (handlerLinkDefUses, #link)
      val {get = labelLive, rem = remLabelLive, ...} =
         Property.get
         (Label.plist,
          Property.initFun
          (fn l =>
           let
              val {bodyInfo, argInfo, ...} = labelInfo l
              val () = removeLabelInfo l
              val {handler, link} = LiveInfo.liveHS bodyInfo
           in
              {begin = LiveInfo.live bodyInfo,
               beginNoFormals = LiveInfo.live argInfo,
               handler = handler,
               link = link}
           end))
      val () = Vector.foreach (blocks, fn b =>
                               ignore (labelLive (Block.label b)))
      val _ =
         Control.diagnostics
         (fn display =>
          let open Layout
          in
             Vector.foreach
             (blocks, fn b =>
              let
                 val l = Block.label b           
                 val {begin, beginNoFormals, handler, link} = labelLive l
              in
                 display
                 (seq [Label.layout l,
                       str " ",
                       record [("begin", Vector.layout Var.layout begin),
                               ("beginNoFormals",
                                Vector.layout Var.layout beginNoFormals),
                               ("handler", Option.layout Label.layout handler),
                               ("link", Bool.layout link)]])
              end)
          end)
   in 
      {labelLive = labelLive,
       remLabelLive = remLabelLive}
   end

val live =
   Trace.trace2 ("Live.live", Func.layout o Function.name, Layout.ignore,
                 Layout.ignore)
   live

end
