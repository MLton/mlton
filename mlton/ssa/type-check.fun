(* Copyright (C) 2009,2011,2017 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor TypeCheck (S: TYPE_CHECK_STRUCTS): TYPE_CHECK = 
struct

open S

datatype z = datatype Exp.t
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
                | _ => Error.bug ("Ssa.TypeCheck.checkScopes: duplicate definition of "
                                  ^ (Layout.toString (layout x)))
            fun reference x =
               case get x of
                  InScope v => v
                | _ => Error.bug (concat
                                  ["Ssa.TypeCheck.checkScopes: reference to ",
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
      val (bindLabel, getLabel, unbindLabel) = make (Label.layout, Label.plist)
      val (bindFunc, getFunc, _) = make (Func.layout, Func.plist)

      val {destroy = destroyLoopType, get = loopType, ...} =
         Property.destGet
         (Type.plist, 
          Property.initRec 
          (fn (ty, loopType) =>
           let
              datatype z = datatype Type.dest
              val _ =
                 case Type.dest ty of
                    Array ty => loopType ty
                  | CPointer => ()
                  | Datatype tycon => getTycon tycon
                  | IntInf => ()
                  | Real _ => ()
                  | Ref ty => loopType ty
                  | Thread => ()
                  | Tuple tys => Vector.foreach (tys, loopType)
                  | Vector ty => loopType ty
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
                  ConApp {con, args, ...} => (getCon con ; getVars args)
                | Const _ => ()
                | PrimApp {args, targs, ...} => (loopTypes targs; getVars args)
                | Profile _ => ()
                | Select {tuple, ...} => getVar tuple
                | Tuple xs => getVars xs
                | Var x => getVar x
         in
            ()
         end
      fun loopStatement (s as Statement.T {var, ty, exp, ...}) =
         let
            val _ = loopExp exp
            val _ = loopType ty
            val _ = Option.app (var, fn x => bindVar (x, ty))
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
                                   fn _ => Error.bug "Ssa.TypeCheck.loopTransfer: redundant branch in case")
                            in
                               ()
                            end)
                        val numCases = Int.toIntInf (Vector.length cases)
                     in
                        case (IntInf.equals (numCases, numExhaustiveCases), isSome default) of
                           (true, true) =>
                              Error.bug "Ssa.TypeCheck.loopTransfer: exhaustive case has default"
                         | (false, false) =>
                              Error.bug "Ssa.TypeCheck.loopTransfer: non-exhaustive case has no default"
                         | _ => ()
                     end
                  fun doitWord (ws, cases) =
                     doit (cases, WordX.equals, WordX.hash, WordSize.cardinality ws)
                  fun doitCon cases =
                     let
                        val numExhaustiveCases =
                           case Type.dest (getVar' test) of
                              Type.Datatype t => Int.toIntInf (getTycon' t)
                            | _ => Error.bug "Ssa.TypeCheck.loopTransfer: case test is not a datatype"
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
                           Option.app (Statement.var s, unbindVar))
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
                               Vector.foreach (args, loopType)))
              handle exn => Error.reraiseSuffix (exn, concat [" in Datatypes"])
      val _ = Vector.foreach (globals, loopStatement)
              handle exn => Error.reraiseSuffix (exn, concat [" in Globals"])
      val _ = List.foreach (functions, bindFunc o Function.name)
      val _ = getFunc main
      val _ = List.foreach (functions, loopFunc)
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
                        (concat ["Ssa.TypeCheck.checkProf: bug found in ", Label.toString l,
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
                              (statements, sources,
                               fn (Statement.T {exp, ...}, sources) =>
                               case exp of
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
      fun coerce {from: Type.t, to: Type.t}: unit =
         if Type.equals (from, to)
            then ()
            else Error.bug (concat ["Ssa.TypeCheck.coerce (",
                                    (Layout.toString o Layout.record)
                                    [("from", Type.layout from),
                                     ("to", Type.layout to)],
                                    ")"])
      fun coerces (from, to) =
         Vector.foreach2 (from, to, fn (from, to) =>
                         coerce {from = from, to = to})
      val coerce =
         Trace.trace ("Ssa.TypeCheck.coerce",
                      fn {from, to} => let open Layout
                                       in record [("from", Type.layout from),
                                                  ("to", Type.layout to)]
                                       end,
                                    Unit.layout) coerce
      fun select {tuple: Type.t, offset: int, resultType = _}: Type.t =
         case Type.deTupleOpt tuple of
            NONE => Error.bug ("Ssa.TypeCheck.select (non tuple)")
          | SOME ts => Vector.sub (ts, offset)
      val {get = conInfo: Con.t -> {args: Type.t vector,
                                    result: Type.t},
           set = setConInfo, ...} =
         Property.getSetOnce
         (Con.plist, Property.initRaise ("Ssa.TypeCheck.conInfo", Con.layout))
      val _ =
         Vector.foreach
         (datatypes, fn Datatype.T {tycon, cons} =>
          let val result = Type.datatypee tycon
          in Vector.foreach
             (cons, fn {con, args} =>
              setConInfo (con, {args = args,
                                result = result}))
          end)
      fun conApp {con, args} =
         let
            val {args = args', result, ...} = conInfo con
            val _ = coerces (args', args)
         in
            result
         end
      fun filter (test, con, args) =
         let
            val {result, args = args'} = conInfo con
            val _ = coerce {from = test, to = result}
            val _ = coerces (args', args)
         in ()
         end
      fun primApp {args, prim, resultType, resultVar = _, targs} =
         let
            datatype z = datatype Prim.Name.t
            val () =
               if Type.checkPrimApp {args = args,
                                     prim = prim,
                                     result = resultType,
                                     targs = targs}
                  then ()
               else Error.bug (concat ["Ssa.TypeCheck.primApp (",
                                       let
                                          open Layout
                                       in
                                          (toString o seq)
                                          [Prim.layout prim,
                                           if Vector.isEmpty targs
                                              then empty
                                              else Vector.layout Type.layout targs,
                                           str " ",
                                           Vector.layout Type.layout args]
                                       end,
                                       ")"])
         in
            resultType
         end
      val _ =
         analyze {
                  coerce = coerce,
                  conApp = conApp,
                  const = Type.ofConst,
                  filter = filter,
                  filterWord = fn (from, s) => coerce {from = from,
                                                       to = Type.word s},
                  fromType = fn x => x,
                  layout = Type.layout,
                  primApp = primApp,
                  program = program,
                  select = select,
                  tuple = Type.tuple,
                  useFromTypeOnBinds = true
                  }
      val _ = Program.clear program
   in
      ()
   end

val typeCheck = fn p =>
   (typeCheck p)
   handle exn => Error.reraisePrefix (exn, "TypeError (SSA): ")

val typeCheck = Control.trace (Control.Pass, "typeCheck") typeCheck

end
