(* Copyright (C) 2009,2011,2017 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor TypeCheck2 (S: TYPE_CHECK2_STRUCTS): TYPE_CHECK2 = 
struct

open S

datatype z = datatype Exp.t
datatype z = datatype Statement.t
datatype z = datatype Transfer.t

fun checkScopes (program as
                 Program.T {datatypes, globals, functions, main}): unit =
   let
      datatype 'a status =
         Undefined
       | InScope of 'a
       | Defined

      fun make' (layout, plist) =
         let
            val {get, set, ...} =
               Property.getSet (plist, Property.initConst Undefined)
            fun bind (x, v) =
               case get x of
                  Undefined => set (x, InScope v)
                | _ => Error.bug (concat ["Ssa2.TypeCheck2.checkScopes: duplicate definition of ",
                                          Layout.toString (layout x)])
            fun reference x =
               case get x of
                  InScope v => v
                | _ => Error.bug (concat ["Ssa2.TypeCheck2.checkScopes: reference to ",
                                          Layout.toString (layout x),
                                          " not in scope"])

            fun unbind x = set (x, Defined)
         in (bind, ignore o reference, reference, unbind)
         end
      fun make (layout, plist) =
         let val (bind, reference, _, unbind) = make' (layout, plist)
         in (fn x => bind (x, ()), reference, unbind)
         end

      val (bindTycon, getTycon, getTycon', _) = make' (Tycon.layout, Tycon.plist)
      val (bindCon, getCon, _) = make (Con.layout, Con.plist)
      val (bindVar, getVar, getVar', unbindVar) = make' (Var.layout, Var.plist)
      fun getVars xs = Vector.foreach (xs, getVar)
      val (bindFunc, getFunc, _) = make (Func.layout, Func.plist)
      val (bindLabel, getLabel, unbindLabel) = make (Label.layout, Label.plist)

      fun loopObjectCon oc =
         let
            datatype z = datatype ObjectCon.t
            val _ =
               case oc of
                  Con con => getCon con
                | Tuple => ()
                | Vector => ()
         in
            ()
         end
      val {destroy = destroyLoopType, get = loopType, ...} =
         Property.destGet
         (Type.plist,
          Property.initRec
          (fn (ty, loopType) =>
           let
              datatype z = datatype Type.dest
              val _ =
                 case Type.dest ty of
                    CPointer => ()
                  | Datatype tycon => getTycon tycon
                  | IntInf => ()
                  | Object {args, con, ...} => 
                       let
                          val _ = loopObjectCon con
                          val _ = Prod.foreach (args, loopType)
                       in
                          ()
                       end
                  | Real _ => ()
                  | Thread => ()
                  | Weak ty => loopType ty
                  | Word _ => ()
           in
              ()
           end))
      fun loopTypes tys = Vector.foreach (tys, loopType)
      (* Redefine bindVar to check well-formedness of types. *)
      val bindVar = fn (x, ty) => (loopType ty; bindVar (x, ty))
      fun loopExp exp =
         let
            val _ = 
               case exp of
                  Const _ => ()
                | Inject {sum, variant, ...} => (getTycon sum; getVar variant)
                | Object {args, con, ...} => (Option.app (con, getCon); getVars args)
                | PrimApp {args, ...} => getVars args
                | Select {base, ...} => Base.foreach (base, getVar)
                | Var x => getVar x
         in
            ()
         end
      fun loopStatement (s: Statement.t): unit =
         let
            val _ = 
               case s of 
                  Bind {exp, ty, var, ...} =>
                     let
                        val _ = loopExp exp
                        val _ = loopType ty
                        val _ = Option.app (var, fn x => bindVar (x, ty))
                     in
                        ()
                     end
                | Profile _ => ()
                | Update {base, value, ...} => 
                     (Base.foreach (base, getVar); getVar value)
         in
            ()
         end
         handle exn => Error.reraiseSuffix (exn, concat [" in ", Layout.toString (Statement.layout s)])
      val loopTransfer =
         fn Arith {args, ty, ...} => (getVars args; loopType ty)
          | Bug => ()
          | Call {func, args, ...} => (getFunc func; getVars args)
          | Case {test, cases, default, ...} =>
               let
                  fun doit (cases: ('a * 'b) vector,
                            equals: 'a * 'a -> bool,
                            hash: 'a -> word,
                            numExhaustiveCases: IntInf.t) =
                     let
                        val table = HashSet.new {hash = hash}
                        val _ =
                           Vector.foreach
                           (cases, fn (x, _) =>
                            let
                               val _ =
                                  HashSet.insertIfNew
                                  (table, hash x, fn y => equals (x, y),
                                   fn () => x,
                                   fn _ => Error.bug "Ssa2.TypeCheck2.loopTransfer: redundant branch in case")
                            in
                               ()
                            end)
                        val numCases = Int.toIntInf (Vector.length cases)
                     in
                        case (IntInf.equals (numCases, numExhaustiveCases), isSome default) of
                           (true, true) =>
                              Error.bug "Ssa2.TypeCheck2.loopTransfer: exhaustive case has default"
                         | (false, false) =>
                              Error.bug "Ssa2.TypeCheck2.loopTransfer: non-exhaustive case has no default"
                         | _ => ()
                     end
                  fun doitWord (ws, cases) =
                     doit (cases, WordX.equals, WordX.hash, WordSize.cardinality ws)
                  fun doitCon cases =
                     let
                        val numExhaustiveCases =
                           case Type.dest (getVar' test) of
                              Type.Datatype t => Int.toIntInf (getTycon' t)
                            | _ => Error.bug "Ssa2.TypeCheck2.loopTransfer: case test is not a datatype"
                     in
                        doit (cases, Con.equals, Con.hash, numExhaustiveCases)
                     end
                  val _ = getVar test
                  val _ =
                     case cases of
                        Cases.Con cs => doitCon cs 
                      | Cases.Word (ws, cs) => doitWord (ws, cs)
               in
                  ()
               end
          | Goto {args, ...} => getVars args
          | Raise xs => getVars xs
          | Return xs => getVars xs
          | Runtime {args, ...} => getVars args
      val loopTransfer =
         fn t =>
         (loopTransfer t)
         handle exn => Error.reraiseSuffix (exn, concat [" in ", Layout.toString (Transfer.layout t)])
      fun loopFunc (f: Function.t) =
         let
            val {args, blocks, raises, returns, start, ...} = Function.dest f
            (* Descend the dominator tree, verifying that variable definitions
             * dominate variable uses.
             *)
            fun loop (Tree.T (block, children)): unit =
               let
                  val Block.T {label, args, statements, transfer, ...} = block
                  val _ = (Vector.foreach (args, bindVar)
                           ; Vector.foreach (statements, loopStatement)
                           ; loopTransfer transfer)
                          handle exn => Error.reraiseSuffix (exn, concat [" in ", Label.toString label])
                  val _ = Vector.foreach (children, loop)
                  val _ = Vector.foreach 
                          (statements, fn s =>
                           Statement.foreachDef (s, unbindVar o #1))
                  val _ = Vector.foreach (args, unbindVar o #1)
               in
                  ()
               end
            val _ = Vector.foreach (args, bindVar)
            val _ = Vector.foreach (blocks, bindLabel o Block.label)
            (* Check that 'start' and all transfer labels are in scope.
             * In the case that something is not in scope,
             * "getLabel" gives a more informative error message
             * than the CFG/dominatorTree construction failure.
             *)
            val _ = getLabel start
            val _ = Vector.foreach
                    (blocks, fn Block.T {transfer, ...} =>
                     Transfer.foreachLabel (transfer, getLabel))
            val _ = loop (Function.dominatorTree f)
            val _ = Vector.foreach (blocks, unbindLabel o Block.label)
            val _ = Vector.foreach (args, unbindVar o #1)
            val _ = Option.app (returns, loopTypes)
            val _ = Option.app (raises, loopTypes)
            val _ = Function.clear f
         in
             ()
         end
         handle exn => Error.reraiseSuffix (exn, concat [" in ", Func.toString (Function.name f)])
      val _ = Vector.foreach
              (datatypes, fn Datatype.T {tycon, cons} =>
               (bindTycon (tycon, Vector.length cons)
                ; Vector.foreach (cons, fn {con, ...} =>
                                  bindCon con)))
              handle exn => Error.reraiseSuffix (exn, concat [" in Datatypes"])
      val _ = Vector.foreach
              (datatypes, fn Datatype.T {cons, ...} =>
               Vector.foreach (cons, fn {args, ...} =>
                               Prod.foreach (args, loopType)))
              handle exn => Error.reraiseSuffix (exn, concat [" in Datatypes"])
      val _ = Vector.foreach (globals, loopStatement)
              handle exn => Error.reraiseSuffix (exn, concat [" in Globals"])
      val _ = List.foreach (functions, bindFunc o Function.name)
      val _ = List.foreach (functions, loopFunc)
      val _ = getFunc main
      val _ = Program.clearTop program
      val _ = destroyLoopType ()
   in
      ()
   end

val checkScopes = Control.trace (Control.Detail, "checkScopes") checkScopes

structure Function =
   struct
      open Function

      fun checkProf (f: t): unit =
         let
            val debug = false
            val {blocks, start, ...} = dest f
            val {get = labelInfo, rem, set = setLabelInfo, ...} =
               Property.getSetOnce
               (Label.plist,
                Property.initRaise ("info", Label.layout))
            val _ = Vector.foreach (blocks, fn b as Block.T {label, ...} =>
                                    setLabelInfo (label,
                                                  {block = b,
                                                   sources = ref NONE}))
            fun goto (l: Label.t, sources: SourceInfo.t list) =
               let
                  fun bug (msg: string): 'a =
                     let
                        val _ = 
                           Vector.foreach
                           (blocks, fn Block.T {label, ...} =>
                            let
                               val {sources, ...} = labelInfo label
                               open Layout
                            in
                               outputl
                               (seq [Label.layout label,
                                     str " ",
                                     Option.layout
                                     (List.layout SourceInfo.layout)
                                     (!sources)],
                                Out.error)
                            end)
                     in
                        Error.bug
                        (concat ["Ssa2.TypeCheck2.checkProf: bug found in ", Label.toString l,
                                 ": ", msg])
                     end
                  val _ =
                     if not debug
                        then ()
                     else
                     let
                        open Layout
                     in
                        outputl (seq [str "goto (",
                                      Label.layout l,
                                      str ", ",
                                      List.layout SourceInfo.layout sources,
                                      str ")"],
                                 Out.error)
                     end
                  val {block, sources = r} = labelInfo l
               in
                  case !r of
                     NONE =>
                        let
                           val _ = r := SOME sources
                           val Block.T {statements, transfer, ...} = block
                           datatype z = datatype Statement.t
                           datatype z = datatype ProfileExp.t
                           val sources =
                              Vector.fold
                              (statements, sources, fn (s, sources) =>
                               case s of
                                  Profile pe =>
                                     (case pe of
                                         Enter s => s :: sources
                                       | Leave s =>
                                            (case sources of
                                                [] => bug "unmatched Leave"
                                              | s' :: sources =>
                                                   if SourceInfo.equals (s, s')
                                                      then sources
                                                   else bug "mismatched Leave"))
                                | _ => sources)
                           val _ =
                              if not debug
                                 then ()
                              else
                              let
                                 open Layout
                              in
                                 outputl (List.layout SourceInfo.layout sources,
                                          Out.error)
                              end
                           val _ = 
                              if (case transfer of
                                     Call {return, ...} =>
                                        let
                                           datatype z = datatype Return.t
                                        in
                                           case return of
                                              Dead => false
                                            | NonTail _ => false
                                            | Tail => true
                                        end
                                   | Raise _ => true
                                   | Return _ => true
                                   | _ => false)
                                 then (case sources of
                                          [] => ()
                                        | _ => bug "nonempty sources when leaving function")
                              else ()
                        in
                           Transfer.foreachLabel
                           (transfer, fn l => goto (l, sources))
                        end
                   | SOME sources' =>
                        if List.equals (sources, sources', SourceInfo.equals)
                           then ()
                        else bug "mismatched block"
               end
            val _ = goto (start, [])
            val _ = Vector.foreach (blocks, fn Block.T {label, ...} => rem label)
         in
            ()
         end
   end

fun checkProf (Program.T {functions, ...}): unit =
   List.foreach (functions, fn f => Function.checkProf f)

val checkProf = Control.trace (Control.Detail, "checkProf") checkProf

fun typeCheck (program as Program.T {datatypes, ...}): unit =
   let
      val _ = checkScopes program
      val _ =
         if !Control.profile <> Control.ProfileNone
            then checkProf program
         else ()
      val {get = conInfo: Con.t -> {result: Type.t,
                                    ty: Type.t,
                                    tycon: Tycon.t},
           set = setConInfo, ...} =
         Property.getSetOnce
         (Con.plist, Property.initRaise ("Ssa2.TypeCheck2.conInfo", Con.layout))
      val conTycon = #tycon o conInfo
      val _ =
         Vector.foreach
         (datatypes, fn Datatype.T {tycon, cons} =>
          let
             val result = Type.datatypee tycon
          in
             Vector.foreach (cons, fn {con, args} =>
                             setConInfo (con, {result = result,
                                               ty = Type.conApp (con, args),
                                               tycon = tycon}))
          end)
      fun inject {sum: Tycon.t, variant: Type.t}: Type.t =
         let
            val error = fn msg =>
               Error.bug (concat ["Ssa2.TypeCheck2.inject (",
                                  msg, " ",
                                  (Layout.toString o Layout.record)
                                  [("sum", Tycon.layout sum),
                                   ("variant", Type.layout variant)],
                                  ")"])
         in
            case Type.dest variant of
               Type.Object {con, ...} =>
                  (case con of
                      ObjectCon.Con c =>
                         if Tycon.equals (conTycon c, sum)
                            then Type.datatypee sum
                         else error "wrong sum"
                    | _ => error "no object-con")
             | _ => error "no object"
         end
      fun coerce {from: Type.t, to: Type.t}: unit =
         if Type.equals (from, to)
            then ()
         else Error.bug (concat ["SSa2.TypeCheck2.coerce (",
                                 (Layout.toString o Layout.record)
                                 [("from", Type.layout from),
                                  ("to", Type.layout to)],
                                 ")"])
      val coerce =
         Trace.trace ("Ssa2.TypeCheck2.coerce",
                      fn {from, to} => let open Layout
                                       in record [("from", Type.layout from),
                                                  ("to", Type.layout to)]
                                       end,
                                    Unit.layout) coerce
      fun object {args, con, resultType} =
         let
            fun err () = Error.bug "Ssa2.TypeCheck2.object (bad object)"
         in
            case Type.dest resultType of
               Type.Object {args = args', con = con'} =>
                  (if (case (con, con') of
                          (NONE, ObjectCon.Tuple) => true
                        | (SOME c, ObjectCon.Con c') => Con.equals (c, c')
                        | _ => false)
                      andalso (Vector.foreach2
                               (Prod.dest args, Prod.dest args',
                                fn ({elt = t, isMutable = _}, {elt = t', ...}) =>
                                coerce {from = t, to = t'})
                               ; true)
                      then resultType
                   else err ())
             | _ => err ()
         end
      fun base b =
         case b of
            Base.Object ty => ty
          | Base.VectorSub {index, vector} =>
               if Type.isVector vector 
                  then let
                          val _ =
                             if Type.equals (index, Type.word (WordSize.seqIndex ()))
                                then ()
                             else Error.bug "Ssa2.TypeCheck2.base (vector-sub of non seqIndex)"
                       in
                          vector
                       end
               else Error.bug "Ssa2.TypeCheck2.base (vector-sub of non vector)"
      fun select {base: Type.t, offset: int, resultType = _}: Type.t =
         case Type.dest base of
            Type.Object {args, ...} => Prod.elt (args, offset)
          | _ => Error.bug "Ssa2.TypeCheck2.select (non object)"
      fun update {base, offset, value} =
         case Type.dest base of
            Type.Object {args, ...} =>
               let
                  val {elt, isMutable} = Prod.sub (args, offset)
                  val () = coerce {from = value, to = elt}
                  val () =
                     if isMutable
                        then ()
                     else Error.bug "Ssa2.TypeCheck2.update (non-mutable field)"
               in
                  ()
               end
          | _ => Error.bug "Ssa2.TypeCheck2.update (non object)"
      fun filter {con, test, variant} =
         let
            val {result, ty, ...} = conInfo con
            val () = coerce {from = test, to = result}
            val () = Option.app (variant, fn to => coerce {from = ty, to = to})
         in
            ()
         end
      fun filterWord (from, s) = coerce {from = from, to = Type.word s}
      fun primApp {args, prim, resultType, resultVar = _} =
         let
            datatype z = datatype Prim.Name.t
            val () =
               if Type.checkPrimApp {args = args,
                                     prim = prim,
                                     result = resultType}
                  then ()
               else Error.bug (concat ["Ssa2.TypeCheck2.primApp (",
                                       let
                                          open Layout
                                       in
                                          (toString o seq)
                                          [Prim.layout prim,
                                           str " ",
                                           Vector.layout Type.layout args]
                                       end,
                                       ")"])
         in
            resultType
         end
      val _ =
         analyze {base = base,
                  coerce = coerce,
                  const = Type.ofConst,
                  filter = filter,
                  filterWord = filterWord,
                  fromType = fn x => x,
                  inject = inject,
                  layout = Type.layout,
                  object = object,
                  primApp = primApp,
                  program = program,
                  select = select,
                  update = update,
                  useFromTypeOnBinds = true}
      val _ = Program.clear program
   in
      ()
   end

val typeCheck = fn p =>
   (typeCheck p)
   handle exn => Error.reraisePrefix (exn, "TypeError (SSA2): ")

val typeCheck = Control.trace (Control.Pass, "typeCheck") typeCheck

end
