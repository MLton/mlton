(* Copyright (C) 2009,2011,2017 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Shrink2 (S: SHRINK2_STRUCTS): SHRINK2 =
struct

open S

structure Statement =
   struct
      open Statement

      fun isProfile (s: t): bool =
         case s of
            Profile _ => true
          | _ => false
   end

structure Array =
   struct
      open Array

      fun inc (a: int t, i: int): unit = update (a, i, 1 + sub (a, i))
      fun dec (a: int t, i: int): unit = update (a, i, sub (a, i) - 1)
   end

datatype z = datatype Exp.t
datatype z = datatype Statement.t
datatype z = datatype Transfer.t

structure VarInfo =
   struct
      datatype t = T of {isUsed: bool ref,
                         numOccurrences: int ref,
                         ty: Type.t option,
                         value: value option ref,
                         var: Var.t}
      and value =
         Const of Const.t
       | Inject of {sum: Tycon.t,
                    variant: t}
       | Object of {args: t vector,
                    con: Con.t option}
       | Select of {object: t,
                    offset: int}

      fun equals (T {var = x, ...}, T {var = y, ...}) = Var.equals (x, y)

      fun layout (T {isUsed, numOccurrences, ty, value, var}) =
         let open Layout
         in record [("isUsed", Bool.layout (!isUsed)),
                    ("numOccurrences", Int.layout (!numOccurrences)),
                    ("ty", Option.layout Type.layout ty),
                    ("value", Option.layout layoutValue (!value)),
                    ("var", Var.layout var)]
         end
      and layoutValue v =
         let
            open Layout
         in
            case v of
               Const c => Const.layout c
             | Inject {sum, variant} =>
                  seq [layout variant, str ": ", Tycon.layout sum]
             | Object {args, con} =>
                  let
                     val args = Vector.layout layout args
                  in
                     case con of
                        NONE => args
                      | SOME con => seq [Con.layout con, args]
                  end
             | Select {object, offset} =>
                  seq [str "#", Int.layout (offset + 1),
                       str " ", layout object]
         end

      fun new (x: Var.t, ty: Type.t option) =
         T {isUsed = ref false,
            numOccurrences = ref 0,
            ty = ty,
            value = ref NONE,
            var = x}

      fun setValue (T {value, ...}, v) =
         (Assert.assert ("Ssa2.Shrink2.VarInfo.setValue", fn () => Option.isNone (!value))
          ; value := SOME v)

      fun numOccurrences (T {numOccurrences = r, ...}) = r
      fun ty (T {ty, ...}): Type.t option = ty
      fun value (T {value, ...}): value option = !value
      fun var (T {var, ...}): Var.t = var
   end

structure Value =
   struct
      datatype t = datatype VarInfo.value
   end

structure Position =
   struct
      datatype t =
         Formal of int
       | Free of Var.t

      fun layout (p: t) =
         case p of
            Formal i => Int.layout i
          | Free x => Var.layout x

      val equals =
         fn (Formal i, Formal i') => i = i'
          | (Free x, Free x') => Var.equals (x, x')
          | _ => false
   end

structure Positions = MonoVector (Position)

structure LabelMeaning =
   struct
      datatype t = T of {aux: aux,
                         blockIndex: int, (* The index of the block *)
                         label: Label.t} (* redundant, the label of the block *)

      and aux =
         Block
       | Bug
       | Case of {canMove: Statement.t list,
                  cases: Cases.t,
                  default: Label.t option}
       | Goto of {canMove: Statement.t list,
                  dst: t,
                  args: Positions.t}
       | Raise of {args: Positions.t,
                   canMove: Statement.t list}
       | Return of {args: Positions.t,
                    canMove: Statement.t list}

      local
         fun make f (T r) = f r
      in
         val aux = make #aux
         val blockIndex = make #blockIndex
      end

      fun layout (T {aux, label, ...}) =
         let
            open Layout
         in
            seq [Label.layout label,
                 str " ",
                 case aux of
                    Block => str "Block "
                  | Bug => str "Bug"
                  | Case _ => str "Case"
                  | Goto {dst, args, ...} =>
                       seq [str "Goto ",
                            tuple [layout dst, Positions.layout args]]
                  | Raise {args, ...} =>
                       seq [str "Raise ", Positions.layout args]
                  | Return {args, ...} =>
                       seq [str "Return ", Positions.layout args]]
         end
   end

structure State =
   struct
      datatype state =
         Unvisited
       | Visited of LabelMeaning.t
       | Visiting

      val layout =
         let
            open Layout
         in
            fn Unvisited => str "Unvisited"
             | Visited m => LabelMeaning.layout m
             | Visiting => str "Visiting"
         end
   end

val traceApplyInfo = Trace.info "Ssa2.Shrink2.Prim.apply"

fun shrinkFunction {globals: Statement.t vector} =
   let
      fun use (VarInfo.T {isUsed, var, ...}): Var.t =
         (isUsed := true
          ; var)
      fun uses (vis: VarInfo.t vector): Var.t vector = Vector.map (vis, use)
      (* varInfo can't be getSetOnce because of setReplacement. *)
      val {get = varInfo: Var.t -> VarInfo.t, set = setVarInfo, ...} =
         Property.getSet (Var.plist,
                          Property.initFun (fn x => VarInfo.new (x, NONE)))
      val setVarInfo =
         Trace.trace2 ("Ssa2.Shrink2.setVarInfo",
                       Var.layout, VarInfo.layout, Unit.layout)
         setVarInfo
      fun varInfos xs = Vector.map (xs, varInfo)
      fun simplifyVar (x: Var.t) = use (varInfo x)
      val simplifyVar =
         Trace.trace ("Ssa2.Shrink2.simplifyVar", Var.layout, Var.layout) simplifyVar
      fun simplifyVars xs = Vector.map (xs, simplifyVar)
      fun incVarInfo (x: VarInfo.t): unit =
         Int.inc (VarInfo.numOccurrences x)
      fun incVar (x: Var.t): unit = incVarInfo (varInfo x)
      fun incVars xs = Vector.foreach (xs, incVar)
      fun numVarOccurrences (x: Var.t): int =
         ! (VarInfo.numOccurrences (varInfo x))
      val () =
         Vector.foreach
         (globals, fn s =>
          case s of
             Bind {exp, ty, var} =>
                let
                   val () = Option.app
                      (var, fn x =>
                       setVarInfo (x, VarInfo.new (x, SOME ty)))
                   fun construct v =
                      Option.app (var, fn x => VarInfo.setValue (varInfo x, v))
                in
                   case exp of
                      Const c => construct (Value.Const c)
                    | Object {args, con} =>
                         construct
                         (Value.Object {args = Vector.map (args, varInfo),
                                        con = con})
                    | Select {base, offset} =>
                         (case base of
                             Base.Object x =>
                                construct (Value.Select {object = varInfo x,
                                                         offset = offset})
                           | _ => ())
                    | Var y =>
                         Option.app (var, fn x => setVarInfo (x, varInfo y))
                    | _ => ()
                end
           | _ => ())
   in
      fn f: Function.t =>
      let
         val () = Function.clear f
         val {args, blocks, mayInline, name, raises, returns, start, ...} =
            Function.dest f
         val () = Vector.foreach (args, fn (x, ty) =>
                                  setVarInfo (x, VarInfo.new (x, SOME ty)))
         (* Index the labels by their defining block in blocks. *)
         val {get = labelIndex, set = setLabelIndex, ...} =
            Property.getSetOnce (Label.plist,
                                 Property.initRaise ("index", Label.layout))
         val () = Vector.foreachi (blocks, fn (i, Block.T {label, ...}) =>
                                   setLabelIndex (label, i))
         val numBlocks = Vector.length blocks
         (* Do a DFS to compute occurrence counts and set label meanings *)
         val states = Array.array (numBlocks, State.Unvisited)
         val inDegree = Array.array (numBlocks, 0)
         fun addLabelIndex i = Array.inc (inDegree, i)
         val isHeader = Array.array (numBlocks, false)
         val numHandlerUses = Array.array (numBlocks, 0)
         fun layoutLabel (l: Label.t): Layout.t =
            let
               val i = labelIndex l
            in
               Layout.record [("label", Label.layout l),
                              ("inDegree", Int.layout (Array.sub (inDegree, i)))]
            end
         fun incAux aux =
            case aux of
               LabelMeaning.Goto {dst, ...} =>
                  addLabelIndex (LabelMeaning.blockIndex dst)
             | _ => ()
         fun incLabel (l: Label.t): unit =
            incLabelMeaning (labelMeaning l)
         and incLabelMeaning (LabelMeaning.T {aux, blockIndex, ...}): unit =
            let
               val i = blockIndex
               val n = Array.sub (inDegree, i)
               val () = Array.update (inDegree, i, 1 + n)
            in
               if n = 0
                  then incAux aux
               else ()
            end
         and labelMeaning (l: Label.t): LabelMeaning.t =
            let
               val i = labelIndex l
            in
               case Array.sub (states, i) of
                  State.Visited m => m
                | State.Visiting =>
                     (Array.update (isHeader, i, true)
                      ; (LabelMeaning.T
                         {aux = LabelMeaning.Block,
                          blockIndex = i,
                          label = Block.label (Vector.sub (blocks, i))}))
                | State.Unvisited =>
                     let
                        val () = Array.update (states, i, State.Visiting)
                        val m = computeMeaning i
                        val () = Array.update (states, i, State.Visited m)
                     in
                        m
                     end
            end
         and computeMeaning (i: int): LabelMeaning.t =
            let
               val Block.T {args, statements, transfer, ...} =
                  Vector.sub (blocks, i)
               val () = Vector.foreach (args, fn (x, ty) =>
                                        setVarInfo (x, VarInfo.new (x, SOME ty)))
               val () = Vector.foreach (statements, fn s =>
                                        Statement.foreachUse (s, incVar))
               fun extract (actuals: Var.t vector): Positions.t =
                  let
                     val {get: Var.t -> Position.t, set, destroy} =
                        Property.destGetSetOnce
                        (Var.plist, Property.initFun Position.Free)
                     val () = Vector.foreachi (args, fn (i, (x, _)) =>
                                              set (x, Position.Formal i))
                     val ps = Vector.map (actuals, get)
                     val () = destroy ()
                  in ps
                  end
               fun doit aux =
                  LabelMeaning.T {aux = aux,
                                  blockIndex = i,
                                  label = Block.label (Vector.sub (blocks, i))}
               fun normal () = doit LabelMeaning.Block
               fun canMove () =
                  Vector.toList statements
               fun rr (xs: Var.t vector, make) =
                  let
                     val () = incVars xs
(*
                     val n = Vector.length statements
                     fun loop (i, ac) =
                        if i = n
                           then
                              if 0 = Vector.length xs
                                 orelse 0 < Vector.length args
                                 then doit (make {args = extract xs,
                                                  canMove = rev ac})
                              else normal ()
                        else
                           let
                              val s = Vector.sub (statements, i)
                           in
                              if Statement.isProfile s
                                 then loop (i + 1, s :: ac)
                              else normal ()
                           end
                  in
                     loop (0, [])
                  end
*)
                  in
                     if Vector.forall (statements, Statement.isProfile)
                        andalso (0 = Vector.length xs
                                 orelse 0 < Vector.length args)
                        then doit (make {args = extract xs,
                                         canMove = canMove ()})
                     else normal ()
                  end
            in
               case transfer of
                  Arith {args, overflow, success, ...} =>
                     (incVars args
                      ; incLabel overflow
                      ; incLabel success
                      ; normal ())
                | Bug =>
                     if Vector.isEmpty statements
                        andalso (case returns of
                                    NONE => true
                                  | SOME ts =>
                                       Vector.equals
                                       (ts, args, fn (t, (_, t')) =>
                                        Type.equals (t, t')))
                        then doit LabelMeaning.Bug
                     else normal ()
                | Call {args, return, ...} =>
                     let
                        val () = incVars args
                        val () =
                           Return.foreachHandler
                           (return, fn l =>
                            Array.inc (numHandlerUses, labelIndex l))
                        val () = Return.foreachLabel (return, incLabel)
                     in
                        normal ()
                     end
                | Case {test, cases, default} =>
                     let
                        val () = incVar test
                        val () = Cases.foreach (cases, incLabel)
                        val () = Option.app (default, incLabel)
                     in
                        if Vector.forall(statements, Statement.isProfile)
                           andalso not (Array.sub (isHeader, i))
                           andalso 1 = Vector.length args
                           andalso 1 = numVarOccurrences test
                           andalso Var.equals (test, #1 (Vector.first args))
                           then
                              doit (LabelMeaning.Case {canMove = canMove (),
                                                       cases = cases,
                                                       default = default})
                        else
                           normal ()
                     end
                | Goto {dst, args = actuals} =>
                     let
                        val () = incVars actuals
                        val m = labelMeaning dst
                     in
                        if Vector.exists (statements, not o Statement.isProfile)
                           orelse Array.sub (isHeader, i)
                           then (incLabelMeaning m
                                 ; normal ())
                        else
                           if Vector.isEmpty statements
                              andalso
                              Vector.equals (args, actuals, fn ((x, _), x') =>
                                             Var.equals (x, x')
                                             andalso 1 = numVarOccurrences x)
                              then m (* It's an eta. *)
                           else
                           let
                              val ps = extract actuals
                              val n =
                                 Vector.fold (args, 0, fn ((x, _), n) =>
                                              n + numVarOccurrences x)
                              val n' =
                                 Vector.fold (ps, 0, fn (p, n) =>
                                              case p of
                                                 Position.Formal _ => n + 1
                                               | _ => n)
                              datatype z = datatype LabelMeaning.aux
                           in
                              if n <> n'
                                 then (incLabelMeaning m
                                       ; normal ())
                              else
                                 let
                                    fun extract (ps': Positions.t)
                                       : Positions.t =
                                       Vector.map
                                       (ps', fn p =>
                                        let
                                           datatype z = datatype Position.t
                                        in
                                           case p of
                                              Free x => Free x
                                            | Formal i => Vector.sub (ps, i)
                                        end)
                                    val canMove' = canMove ()
                                    val a =
                                       case LabelMeaning.aux m of
                                          Block =>
                                             Goto {canMove = canMove',
                                                   dst = m,
                                                   args = ps}
                                        | Bug =>
                                             if (case returns of
                                                    NONE => true
                                                  | SOME ts =>
                                                       Vector.equals
                                                       (ts, args, fn (t, (_, t')) =>
                                                        Type.equals (t, t')))
                                                then Bug
                                             else Goto {canMove = canMove',
                                                        dst = m,
                                                        args = ps}
                                       | Case _ =>
                                             Goto {canMove = canMove',
                                                   dst = m,
                                                   args = ps}
                                        | Goto {canMove, dst, args} =>
                                             Goto {canMove = canMove' @ canMove,
                                                   dst = dst,
                                                   args = extract args}
                                        | Raise {args, canMove} =>
                                             Raise {args = extract args,
                                                    canMove = canMove' @ canMove}
                                        | Return {args, canMove} =>
                                             Return {args = extract args,
                                                     canMove = canMove' @ canMove}
                                 in
                                    doit a
                                 end
                           end
                     end
                | Raise xs => rr (xs, LabelMeaning.Raise)
                | Return xs => rr (xs, LabelMeaning.Return)
                | Runtime {args, return, ...} =>
                     (incVars args
                      ; incLabel return
                      ; normal ())
            end
         val () = incLabel start
         fun indexMeaning i =
            case Array.sub (states, i) of
               State.Visited m => m
             | _ => Error.bug "Ssa2.Shrink2.indexMeaning: not computed"
         val indexMeaning =
            Trace.trace ("Ssa2.Shrink2.indexMeaning", Int.layout, LabelMeaning.layout)
            indexMeaning
         val labelMeaning = indexMeaning o labelIndex
         val labelMeaning =
            Trace.trace ("Ssa2.Shrink2.labelMeaning",
                         Label.layout, LabelMeaning.layout)
            labelMeaning
         fun meaningLabel m =
            Block.label (Vector.sub (blocks, LabelMeaning.blockIndex m))
         fun labelArgs l =
            Block.args (Vector.sub (blocks, labelIndex l))
         fun meaningArgs m =
            Block.args (Vector.sub (blocks, LabelMeaning.blockIndex m))
         fun save (f, s) =
            let
               val {destroy, controlFlowGraph, ...} =
                  Function.layoutDot (f, Var.layout)
            in
               File.withOut
               (concat ["/tmp/", Func.toString (Function.name f),
                        ".", s, ".dot"],
                fn out => Layout.outputl (controlFlowGraph, out))
               ; destroy ()
            end
         val () = if true then () else save (f, "pre")
         (* *)
         val () =
            if true
               then ()
            else
               Layout.outputl
               (Vector.layout
                (fn i =>
                 (Layout.record
                  [("label",
                    Label.layout (Block.label (Vector.sub (blocks, i)))),
                   ("inDegree", Int.layout (Array.sub (inDegree, i))),
                   ("state", State.layout (Array.sub (states, i)))]))
                (Vector.tabulate (numBlocks, fn i => i)),
                Out.error)
         val () =
            Assert.assert
            ("Ssa2.Shrink2.labelMeanings", fn () =>
             let
                val inDegree' = Array.array (numBlocks, 0)
                fun bumpIndex i = Array.inc (inDegree', i)
                fun bumpMeaning m = bumpIndex (LabelMeaning.blockIndex m)
                val bumpLabel = bumpMeaning o labelMeaning
                fun doit (LabelMeaning.T {aux, blockIndex, ...}) =
                   let
                      datatype z = datatype LabelMeaning.aux
                   in
                      case aux of
                         Block =>
                            Transfer.foreachLabel
                            (Block.transfer (Vector.sub (blocks, blockIndex)),
                             bumpLabel)
                       | Bug => ()
                       | Case {cases, default, ...} =>
                            (Cases.foreach (cases, bumpLabel)
                             ; Option.app (default, bumpLabel))
                       | Goto {dst, ...} => bumpMeaning dst
                       | Raise _ => ()
                       | Return _ => ()
                   end
                val () =
                   Array.foreachi
                   (states, fn (i, s) =>
                    if Array.sub (inDegree, i) > 0
                       then
                          (case s of
                              State.Visited m => doit m
                            | _ => ())
                    else ())
                val () = bumpMeaning (labelMeaning start)
             in
                Array.equals (inDegree, inDegree', Int.equals)
                orelse
                let
                   val () =
                      Layout.outputl
                      (Vector.layout
                       (fn i =>
                        (Layout.record
                         [("label",
                           Label.layout (Block.label (Vector.sub (blocks, i)))),
                          ("inDegree", Int.layout (Array.sub (inDegree, i))),
                          ("inDegree'", Int.layout (Array.sub (inDegree', i))),
                          ("state", State.layout (Array.sub (states, i)))]))
                       (Vector.tabulate (numBlocks, fn i => i)),
                       Out.error)
                in
                   false
                end
             end)
         val isBlock = Array.array (numBlocks, false)
         (* Functions for maintaining inDegree. *)
         val addLabelIndex =
            fn i =>
            (Assert.assert ("Ssa2.Shrink2.addLabelIndex", fn () =>
                            Array.sub (inDegree, i) > 0)
             ; addLabelIndex i)
         val addLabelMeaning = addLabelIndex o LabelMeaning.blockIndex
         fun layoutLabelMeaning m =
            Layout.record
            [("inDegree", Int.layout (Array.sub
                                      (inDegree, LabelMeaning.blockIndex m))),
             ("meaning", LabelMeaning.layout m)]
         val traceDeleteLabelMeaning =
            Trace.trace ("Ssa2.Shrink2.deleteLabelMeaning",
                         layoutLabelMeaning, Unit.layout)
         fun deleteLabel l = deleteLabelMeaning (labelMeaning l)
         and deleteLabelMeaning arg: unit =
            traceDeleteLabelMeaning
            (fn (m: LabelMeaning.t) =>
            let
               val i = LabelMeaning.blockIndex m
               val n = Array.sub (inDegree, i) - 1
               val () = Array.update (inDegree, i, n)
               val () = Assert.assert ("Ssa2.Shrink2.deleteLabelMeaning", fn () => n >= 0)
            in
               if n = 0 (* andalso not (Array.sub (isBlock, i)) *)
                  then
                     let
                        datatype z = datatype LabelMeaning.aux
                     in
                        case LabelMeaning.aux m of
                           Block =>
                              let
                                 val t = Block.transfer (Vector.sub (blocks, i))
                                 val () = Transfer.foreachLabel (t, deleteLabel)
                                 val () =
                                    case t of
                                       Transfer.Call {return, ...} =>
                                          Return.foreachHandler
                                          (return, fn l =>
                                           Array.dec (numHandlerUses,
                                                      (LabelMeaning.blockIndex
                                                       (labelMeaning l))))
                                     | _ => ()
                              in
                                 ()
                              end
                         | Bug => ()
                         | Case {cases, default, ...} =>
                              (Cases.foreach (cases, deleteLabel)
                               ; Option.app (default, deleteLabel))
                         | Goto {dst, ...} => deleteLabelMeaning dst
                         | Raise _ => ()
                         | Return _ => ()
                     end
               else ()
            end) arg
         fun primApp (prim: Type.t Prim.t, args: VarInfo.t vector)
            : (Type.t, VarInfo.t) Prim.ApplyResult.t =
            let
               val args' =
                  Vector.map
                  (args, fn vi =>
                   case vi of
                      VarInfo.T {value = ref (SOME v), ...} =>
                         (case v of
                             Value.Const c => Prim.ApplyArg.Const c
                           | Value.Object {args, con} =>
                                (case (con, Vector.length args) of
                                    (SOME con, 0) =>
                                       Prim.ApplyArg.Con {con = con,
                                                          hasArg = false}
                                  | _ => Prim.ApplyArg.Var vi)
                           | _ => Prim.ApplyArg.Var vi)
                    | _ => Prim.ApplyArg.Var vi)
            in
               Trace.traceInfo'
               (traceApplyInfo,
                fn (p, args, _) =>
                let
                   open Layout
                in
                   seq [Prim.layout p, str " ",
                        List.layout (Prim.ApplyArg.layout
                                     (Var.layout o VarInfo.var)) args]
                end,
                Prim.ApplyResult.layout (Var.layout o VarInfo.var))
               Prim.apply
               (prim, Vector.toList args', VarInfo.equals)
            end
         (* Another DFS, this time accumulating the new blocks. *)
         val traceForceMeaningBlock =
            Trace.trace ("Ssa2.Shrink2.forceMeaningBlock",
                        layoutLabelMeaning, Unit.layout)
         val traceSimplifyBlock =
            Trace.trace2 ("Ssa2.Shrink2.simplifyBlock",
                          List.layout Statement.layout,
                          layoutLabel o Block.label,
                          Layout.tuple2 (List.layout Statement.layout,
                                         Transfer.layout))
         val traceGotoMeaning =
            Trace.trace3
            ("Ssa2.Shrink2.gotoMeaning",
             List.layout Statement.layout,
             layoutLabelMeaning,
             Vector.layout VarInfo.layout,
             Layout.tuple2 (List.layout Statement.layout, Transfer.layout))
         val traceEvalStatement =
            Trace.trace
            ("Ssa2.Shrink2.evalStatement",
             Statement.layout,
             Layout.ignore: (Statement.t list -> Statement.t list) -> Layout.t)
         val traceSimplifyTransfer =
            Trace.trace ("Ssa2.Shrink2.simplifyTransfer",
                         Transfer.layout,
                         Layout.tuple2 (List.layout Statement.layout,
                                        Transfer.layout))
         val traceSimplifyCase =
            Trace.trace
            ("Ssa2.Shrink2.simplifyCase",
             fn {canMove, cases, default, test, ...} =>
             Layout.record [("canMove", List.layout Statement.layout canMove),
                            ("cantSimplify", Layout.str "fn () => ..."),
                            ("gone", Layout.str "fn () => ..."),
                            ("test", VarInfo.layout test),
                            ("cases/default",
                             (Transfer.layout o Transfer.Case)
                             {cases = cases,
                              default = default,
                              test = VarInfo.var test})],
             Layout.tuple2 (List.layout Statement.layout, Transfer.layout))
         val newBlocks = ref []
         fun simplifyLabel l =
            let
               val m = labelMeaning l
               val () = forceMeaningBlock m
            in
               meaningLabel m
            end
         and forceMeaningBlock arg =
            traceForceMeaningBlock
            (fn (LabelMeaning.T {aux, blockIndex = i, ...}) =>
             if Array.sub (isBlock, i)
                then ()
             else
                let
                   val () = Array.update (isBlock, i, true)
                   val block as Block.T {label, args, ...} =
                      Vector.sub (blocks, i)
                   fun extract (p: Position.t): VarInfo.t =
                      varInfo (case p of
                                  Position.Formal n => #1 (Vector.sub (args, n))
                                | Position.Free x => x)
                   val (statements, transfer) =
                      let
                         fun rr ({args, canMove}, make) =
                            (canMove, make (Vector.map (args, use o extract)))
                         datatype z = datatype LabelMeaning.aux
                      in
                         case aux of
                            Block => simplifyBlock ([], block)
                          | Bug => ([], Transfer.Bug)
                          | Case _ => simplifyBlock ([], block)
                          | Goto {canMove, dst, args} =>
                               gotoMeaning (canMove,
                                            dst,
                                            Vector.map (args, extract))
                          | Raise z => rr (z, Transfer.Raise)
                          | Return z => rr (z, Transfer.Return)
                      end
                   val () =
                      List.push
                      (newBlocks,
                       Block.T {label = label,
                                args = args,
                                statements = Vector.fromList statements,
                                transfer = transfer})
                in
                   ()
                end) arg
         and simplifyBlock arg : Statement.t list * Transfer.t =
            traceSimplifyBlock
            (fn (canMoveIn, Block.T {statements, transfer, ...}) =>
            let
               val f = evalStatements statements
               val (ss, transfer) = simplifyTransfer transfer
            in
               (canMoveIn @ (f ss), transfer)
            end) arg
         and evalStatements (ss: Statement.t vector)
            : Statement.t list -> Statement.t list =
            let
               val fs = Vector.map (ss, evalStatement)
            in
               fn ss => Vector.foldr (fs, ss, fn (f, ss) => f ss)
            end
         and simplifyTransfer arg : Statement.t list * Transfer.t =
            traceSimplifyTransfer
            (fn (t: Transfer.t) =>
            case t of
                Arith {prim, args, overflow, success, ty} =>
                   let
                      val args = varInfos args
                   in
                      case primApp (prim, args) of
                         Prim.ApplyResult.Const c =>
                            let
                               val () = deleteLabel overflow
                               val x = Var.newNoname ()
                               val isUsed = ref false
                               val vi =
                                  VarInfo.T {isUsed = isUsed,
                                             numOccurrences = ref 0,
                                             ty = SOME ty,
                                             value = ref (SOME (Value.Const c)),
                                             var = x}
                               val (ss, t) = goto (success, Vector.new1 vi)
                               val ss =
                                  if !isUsed
                                     then Bind {var = SOME x,
                                                ty = Type.ofConst c,
                                                exp = Exp.Const c} :: ss
                                  else ss
                            in
                               (ss, t)
                            end
                       | Prim.ApplyResult.Var x =>
                            let
                               val () = deleteLabel overflow
                            in
                               goto (success, Vector.new1 x)
                            end
                       | Prim.ApplyResult.Overflow =>
                            let
                               val () = deleteLabel success
                            in
                               goto (overflow, Vector.new0 ())
                            end
                       | Prim.ApplyResult.Apply (prim, args) =>
                            let
                               val args = Vector.fromList args
                            in
                               ([], Arith {prim = prim,
                                           args = uses args,
                                           overflow = simplifyLabel overflow,
                                           success = simplifyLabel success,
                                           ty = ty})
                            end
                       | _ =>
                            ([], Arith {prim = prim,
                                        args = uses args,
                                        overflow = simplifyLabel overflow,
                                        success = simplifyLabel success,
                                        ty = ty})
                   end
             | Bug => ([], Bug)
             | Call {func, args, return} =>
                  let
                     val (statements, return) =
                        case return of
                           Return.NonTail {cont, handler} =>
                              let
                                 fun isEta (m: LabelMeaning.t,
                                            ps: Position.t vector): bool =
                                    Vector.length ps
                                    = Vector.length (meaningArgs m)
                                    andalso
                                    Vector.foralli
                                    (ps,
                                     fn (i, Position.Formal i') => i = i'
                                      | _ => false)
                                 val m = labelMeaning cont
                                 fun nonTail () =
                                    let
                                       val () = forceMeaningBlock m
                                       val handler =
                                          Handler.map
                                          (handler, fn l =>
                                           let
                                              val m = labelMeaning l
                                              val () = forceMeaningBlock m
                                           in
                                              meaningLabel m
                                           end)
                                    in
                                       ([],
                                        Return.NonTail {cont = meaningLabel m,
                                                        handler = handler})
                                    end
                                 fun tail statements =
                                    (deleteLabelMeaning m
                                     ; (statements, Return.Tail))
                                 fun cont handlerEta =
                                    case LabelMeaning.aux m of
                                       LabelMeaning.Bug =>
                                          (case handlerEta of
                                              NONE => nonTail ()
                                            | SOME canMove => tail canMove)
                                     | LabelMeaning.Return {args, canMove} =>
                                          if isEta (m, args)
                                             then tail canMove
                                          else nonTail ()
                                     | _ => nonTail ()

                              in
                                 case handler of
                                    Handler.Caller => cont NONE
                                  | Handler.Dead => cont NONE
                                  | Handler.Handle l =>
                                       let
                                          val m = labelMeaning l
                                       in
                                          case LabelMeaning.aux m of
                                             LabelMeaning.Bug => cont NONE
                                           | LabelMeaning.Raise {args, canMove} =>
                                                if isEta (m, args)
                                                   then cont (SOME canMove)
                                                else nonTail ()
                                           | _ => nonTail ()
                                       end
                              end
                         | _ => ([], return)
                  in
                     (statements,
                      Call {func = func,
                            args = simplifyVars args,
                            return = return})
                  end
              | Case {test, cases, default} =>
                   let
                      val test = varInfo test
                      fun cantSimplify () =
                         ([],
                          Case {test = use test,
                                cases = Cases.map (cases, simplifyLabel),
                                default = Option.map (default, simplifyLabel)})
                   in
                      simplifyCase
                      {canMove = [],
                       cantSimplify = cantSimplify,
                       cases = cases,
                       default = default,
                       gone = fn () => (Cases.foreach (cases, deleteLabel)
                                        ; Option.app (default, deleteLabel)),
                       test = test}
                   end
              | Goto {dst, args} => goto (dst, varInfos args)
              | Raise xs => ([], Raise (simplifyVars xs))
              | Return xs => ([], Return (simplifyVars xs))
              | Runtime {prim, args, return} =>
                   ([], Runtime {prim = prim,
                                 args = simplifyVars args,
                                 return = simplifyLabel return})
                   ) arg
         and simplifyCase arg : Statement.t list * Transfer.t =
            traceSimplifyCase
            (fn {canMove, cantSimplify,
                 cases, default, gone, test: VarInfo.t} =>
            let
               (* tryToEliminate makes sure that the destination meaning
                * hasn't already been simplified.  If it has, then we can't
                * simplify the case.
                *)
               fun tryToEliminate m =
                  let
                     val i = LabelMeaning.blockIndex m
                  in
                     if Array.sub (inDegree, i) = 0
                        then cantSimplify ()
                     else
                        let
                           val () = addLabelIndex i
                           val () = gone ()
                        in
                           gotoMeaning (canMove, m, Vector.new0 ())
                        end
                  end
            in
               if Cases.isEmpty cases
                  then (case default of
                           NONE => ([], Bug)
                         | SOME l => tryToEliminate (labelMeaning l))
               else
                  let
                     val l = Cases.hd cases
                     fun isOk (l': Label.t): bool = Label.equals (l, l')
                  in
                     if Vector.isEmpty (labelArgs l)
                        andalso Cases.forall (cases, isOk)
                        andalso (case default of
                                    NONE => true
                                  | SOME l => isOk l)
                        then
                           (* All cases the same -- eliminate the case. *)
                           tryToEliminate (labelMeaning l)
                     else
                        let
                           fun findCase (cases, isCon, args) =
                              let
                                 val n = Vector.length cases
                                 fun doit (l, args) =
                                    let
                                       val args =
                                          if Vector.isEmpty (labelArgs l)
                                             then Vector.new0 ()
                                          else args
                                       val m = labelMeaning l
                                       val () = addLabelMeaning m
                                       val () = gone ()
                                    in
                                       gotoMeaning (canMove, m, args)
                                    end
                                 fun loop k =
                                    if k = n
                                       then
                                          (case default of
                                              NONE => (gone (); ([], Bug))
                                            | SOME l => doit (l, Vector.new0 ()))
                                    else
                                       let
                                          val (con, l) = Vector.sub (cases, k)
                                       in
                                          if isCon con
                                             then doit (l, args)
                                          else loop (k + 1)
                                       end
                              in
                                 loop 0
                              end
                        in
                           case (VarInfo.value test, cases) of
                              (SOME (Value.Const c), _) =>
                                 (case (cases, c) of
                                     (Cases.Word (_, cs), Const.Word w) =>
                                        findCase (cs,
                                                  fn w' => WordX.equals (w, w'),
                                                  Vector.new0 ())
                                   | _ =>
                                        Error.bug "Ssa2.Shrink2.simplifyCase: strange constant")
                            | (SOME (Value.Inject {variant, ...}),
                               Cases.Con cases) =>
                                 let
                                    val VarInfo.T {value, ...} = variant
                                 in
                                    case !value of
                                       SOME (Value.Object
                                             {con = SOME con, ...}) =>
                                          findCase (cases,
                                                    fn c => Con.equals (con, c),
                                                    Vector.new1 variant)
                                     | _ => cantSimplify ()
                                 end
                            | _ => cantSimplify ()
                        end
                  end
            end) arg
         and goto (dst: Label.t, args: VarInfo.t vector)
            : Statement.t list * Transfer.t =
            gotoMeaning ([], labelMeaning dst, args)
         and gotoMeaning arg : Statement.t list * Transfer.t =
            traceGotoMeaning
            (fn (canMoveIn,
                 m as LabelMeaning.T {aux, blockIndex = i, ...},
                 args: VarInfo.t vector) =>
             let
                val n = Array.sub (inDegree, i)
                val () = Assert.assert ("Ssa2.Shrink2.gotoMeaning", fn () => n >= 1)
                fun normal () =
                   if n = 1
                      then
                         let
                            val () = Array.update (inDegree, i, 0)
                            val b = Vector.sub (blocks, i)
                            val () =
                               Vector.foreach2
                               (Block.args b, args, fn ((x, _), vi) =>
                                setVarInfo (x, vi))
                         in
                            simplifyBlock (canMoveIn, b)
                         end
                   else
                      let
                         val () = forceMeaningBlock m
                      in
                         (canMoveIn,
                          Goto {dst = Block.label (Vector.sub (blocks, i)),
                                args = uses args})
                      end
                fun extract p =
                   case p of
                      Position.Formal n => Vector.sub (args, n)
                    | Position.Free x => varInfo x
                fun rr ({args, canMove}, make) =
                   (canMoveIn @ canMove,
                    make (Vector.map (args, use o extract)))
                datatype z = datatype LabelMeaning.aux
             in
                case aux of
                   Block => normal ()
                 | Bug => ((*canMoveIn*)[], Transfer.Bug)
                 | Case {canMove, cases, default} =>
                      simplifyCase {canMove = canMoveIn @ canMove,
                                    cantSimplify = normal,
                                    cases = cases,
                                    default = default,
                                    gone = fn () => deleteLabelMeaning m,
                                    test = Vector.first args}
                 | Goto {canMove, dst, args} =>
                      if Array.sub (isHeader, i)
                         orelse Array.sub (isBlock, i)
                         then normal ()
                      else
                         let
                            val n' = n - 1
                            val () = Array.update (inDegree, i, n')
                            val () =
                               if n' > 0
                                  then addLabelMeaning dst
                               else ()
                         in
                            gotoMeaning (canMoveIn @ canMove,
                                         dst,
                                         Vector.map (args, extract))
                         end
                 | Raise z => rr (z, Transfer.Raise)
                 | Return z => rr (z, Transfer.Return)
             end) arg
         and evalBind {exp, ty, var} =
            let
               val () =
                  Option.app (var, fn x =>
                              setVarInfo (x, VarInfo.new (x, SOME ty)))
               fun delete ss = ss
               fun doit {makeExp: unit -> Exp.t,
                         sideEffect: bool,
                         value: Value.t option} =
                  let
                     fun make var = Bind {exp = makeExp (), ty = ty, var = var}
                  in
                     case var of
                        NONE =>
                           if sideEffect
                              then (fn ss => make NONE :: ss)
                           else delete
                      | SOME x =>
                           let
                              val VarInfo.T {isUsed, value = r, ...} = varInfo x
                              val () = r := value
                           in
                              fn ss =>
                              if !isUsed
                                 then make (SOME x) :: ss
                              else if sideEffect
                                      then make NONE :: ss
                                   else ss
                           end
                  end
               fun simple {sideEffect} =
                  let
                     fun makeExp () = Exp.replaceVar (exp, use o varInfo)
                  in
                     doit {makeExp = makeExp,
                           sideEffect = sideEffect,
                           value = NONE}
                  end
               fun setVar vi =
                  (Option.app (var, fn x => setVarInfo (x, vi))
                   ; delete)
               fun construct (v: Value.t, makeExp) =
                  doit {makeExp = makeExp,
                        sideEffect = false,
                        value = SOME v}
               fun tuple (xs: VarInfo.t vector) =
                  case (Exn.withEscape
                        (fn escape =>
                         let
                            fun no () = escape NONE
                         in
                            Vector.foldri
                            (xs, NONE,
                             fn (i, VarInfo.T {value, ...}, tuple') =>
                             case !value of
                                SOME (Value.Select {object, offset}) =>
                                   (if i = offset
                                       then
                                          case tuple' of
                                             NONE =>
                                                (case VarInfo.ty object of
                                                    NONE => no ()
                                                  | SOME ty =>
                                                       (case Type.dest ty of
                                                           Type.Object {args, con = ObjectCon.Tuple} =>
                                                              if Prod.length args = Vector.length xs
                                                                 andalso
                                                                 Prod.allAreImmutable args
                                                                 then SOME object
                                                              else no ()
                                                         | _ => no ()))
                                           | SOME tuple'' =>
                                                if VarInfo.equals (tuple'', object)
                                                   then tuple'
                                                else no ()
                                    else no ())
                              | _ => no ())
                         end)) of
                     NONE =>
                        construct (Value.Object {args = xs, con = NONE},
                                   fn () => Object {args = uses xs, con = NONE})
                   | SOME object => setVar object
            in
               case exp of
                  Const c => construct (Value.Const c, fn () => exp)
                | Inject {sum, variant} =>
                     let
                        val variant = varInfo variant
                     in
                        construct (Value.Inject {sum = sum, variant = variant},
                                   fn () => Inject {sum = sum,
                                                    variant = use variant})
                     end
                | Object {args, con} =>
                     let
                        val args = varInfos args
                        val isMutable =
                           case Type.dest ty of
                              Type.Object {args, ...} => Prod.someIsMutable args
                            | _ => Error.bug "strange Object type"
                     in
                        (* It would be nice to improve this code to do
                         * reconstruction when isSome con, not just for
                         * tuples.
                         *)
                        if isMutable orelse isSome con then
                           construct (Value.Object {args = args, con = con},
                                      fn () => Object {args = uses args,
                                                       con = con})
                        else tuple args
                     end
                | PrimApp {args, prim} =>
                     let
                        val args = varInfos args
                        fun apply {prim, args} =
                           doit {makeExp = fn () => PrimApp {args = uses args,
                                                             prim = prim},
                                 sideEffect = Prim.maySideEffect prim,
                                 value = NONE}
                        datatype z = datatype Prim.ApplyResult.t
                     in
                        case primApp (prim, args) of
                           Apply (prim, args) =>
                              apply {prim = prim, args = Vector.fromList args}
                         | Bool b =>
                              let
                                 val variant = Var.newNoname ()
                                 val con = Con.fromBool b
                              in
                                 evalStatements
                                 (Vector.new2
                                  (Bind {exp = Object {args = Vector.new0 (),
                                                       con = SOME con},
                                         ty = Type.object {args = Prod.empty (),
                                                           con = ObjectCon.Con con},
                                         var = SOME variant},
                                   Bind {exp = Inject {sum = Tycon.bool,
                                                       variant = variant},
                                         ty = Type.bool,
                                         var = var}))
                              end
                         | Const c => construct (Value.Const c,
                                                 fn () => Exp.Const c)
                         | Var vi => setVar vi
                         | _ => apply {args = args, prim = prim}
                     end
                | Select {base, offset} =>
                     (case base of
                         Base.Object object =>
                            let
                               val object as VarInfo.T {ty, value, ...} =
                                  varInfo object
                               fun dontChange () =
                                  construct
                                  (Value.Select {object = object,
                                                 offset = offset},
                                   fn () =>
                                   Select {base = Base.Object (use object),
                                           offset = offset})
                            in
                               case (ty, !value) of
                                  (SOME ty, SOME (Value.Object {args, ...})) =>
                                     (case Type.dest ty of
                                         Type.Object {args = targs, ...} =>
                                            (* Can't simplify the select if the
                                             * field is mutable.
                                             *)
                                            if (#isMutable
                                                (Vector.sub
                                                 (Prod.dest targs, offset)))
                                               then dontChange ()
                                            else setVar (Vector.sub
                                                         (args, offset))
                                       | _ => Error.bug "Ssa2.Shrink2.evalBind: Select:non object")
                                | _ => dontChange ()
                            end
                       | Base.VectorSub _ => simple {sideEffect = false})
                | Var x => setVar (varInfo x)
            end
         and evalStatement arg : Statement.t list -> Statement.t list =
            traceEvalStatement
            (fn s =>
             let
                fun simple () =
                   fn ss => Statement.replaceUses (s, use o varInfo) :: ss
             in
                case s of
                   Bind b => evalBind b
                 | Profile _ => simple ()
                 | Update _ => simple ()
             end) arg
         val start = labelMeaning start
         val () = forceMeaningBlock start
         val f =
            Function.new {args = args,
                          blocks = Vector.fromList (!newBlocks),
                          mayInline = mayInline,
                          name = name,
                          raises = raises,
                          returns = returns,
                          start = meaningLabel start}
         val () = if true then () else save (f, "post")
         val () = Function.clear f
      in
         f
      end
   end

fun eliminateUselessProfile (f: Function.t): Function.t =
   if !Control.profile = Control.ProfileNone
      then f
   else
      let
         fun eliminateInBlock (b as Block.T {args, label, statements, transfer})
            : Block.t =
            if not (Vector.exists (statements, Statement.isProfile))
               then b
            else
               let
                  datatype z = datatype Exp.t
                  datatype z = datatype ProfileExp.t
                  val stack =
                     Vector.fold
                     (statements, [], fn (s, stack) =>
                      case s of
                         Profile (Leave si) =>
                            (case stack of
                                Profile (Enter si') :: rest =>
                                   if SourceInfo.equals (si, si')
                                      then rest
                                   else Error.bug "Ssa2.Shrink2.eliminateUselessProfile: mismatched Leave"
                              | _ => s :: stack)
                       | _ => s :: stack)
                  val statements = Vector.fromListRev stack
               in
                  Block.T {args = args,
                           label = label,
                           statements = statements,
                           transfer = transfer}
               end
         val {args, blocks, mayInline, name, raises, returns, start} =
            Function.dest f
         val blocks = Vector.map (blocks, eliminateInBlock)
      in
         Function.new {args = args,
                       blocks = blocks,
                       mayInline = mayInline,
                       name = name,
                       raises = raises,
                       returns = returns,
                       start = start}
      end

val traceShrinkFunction =
   Trace.trace ("Ssa2.Shrink2.shrinkFunction", Function.layout, Function.layout)

val shrinkFunction =
   fn g =>
   let
      val s = shrinkFunction g
   in
      fn f => traceShrinkFunction s (eliminateUselessProfile f)
   end

fun shrink (Program.T {datatypes, globals, functions, main}) =
   let
      val s = shrinkFunction {globals = globals}
      val program =
         Program.T {datatypes = datatypes,
                    globals = globals,
                    functions = List.revMap (functions, s),
                    main = main}
      val () = Program.clear program
   in
      program
   end

fun eliminateDeadBlocksFunction f =
   let
      val {args, blocks, mayInline, name, raises, returns, start} =
         Function.dest f
      val {get = isLive, set = setLive, rem} =
         Property.getSetOnce (Label.plist, Property.initConst false)
      val () = Function.dfs (f, fn Block.T {label, ...} =>
                            (setLive (label, true)
                             ; fn () => ()))
      val f =
         if Vector.forall (blocks, isLive o Block.label)
            then f
         else
            let
               val blocks =
                  Vector.keepAll
                  (blocks, isLive o Block.label)
            in
               Function.new {args = args,
                             blocks = blocks,
                             mayInline = mayInline,
                             name = name,
                             raises = raises,
                             returns = returns,
                             start = start}
            end
       val () = Vector.foreach (blocks, rem o Block.label)
   in
     f
   end

fun eliminateDeadBlocks (Program.T {datatypes, globals, functions, main}) =
   let
      val functions = List.revMap (functions, eliminateDeadBlocksFunction)
   in
      Program.T {datatypes = datatypes,
                 globals = globals,
                 functions = functions,
                 main = main}
   end

end
