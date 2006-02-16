(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor TypeCheck (S: TYPE_CHECK_STRUCTS): TYPE_CHECK = 
struct

open S

type int = Int.t
type word = Word.t
   
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

      val (bindTycon, _, getTycon', _) = make' (Tycon.layout, Tycon.plist)
      val (bindCon, getCon, getCon', _) = make' (Con.layout, Con.plist)
      val (bindVar, getVar, getVar', unbindVar) = make' (Var.layout, Var.plist)
      fun getVars xs = Vector.foreach (xs, getVar)
      val (bindFunc, getFunc, _) = make (Func.layout, Func.plist)
      val (bindLabel, getLabel, unbindLabel) = make (Label.layout, Label.plist)
      fun loopStatement (Statement.T {var, ty, exp, ...}) =
         let
            val _ =
               case exp of
                  ConApp {con, args, ...} => (getCon con
                                              ; Vector.foreach (args, getVar))
                | Const _ => ()
                | PrimApp {args, ...} => Vector.foreach (args, getVar)
                | Profile _ => ()
                | Select {tuple, ...} => getVar tuple
                | Tuple xs => Vector.foreach (xs, getVar)
                | Var x => getVar x
            val _ = Option.app (var, fn x => bindVar (x, ty))
         in
            ()
         end
      val loopTransfer =
         fn Arith {args, ...} => getVars args
          | Bug => ()
          | Call {func, args, ...} => (getFunc func; getVars args)
          | Case {test, cases, default, ...} =>
               let
                  fun doit (cases: ('a * 'b) vector,
                            equals: 'a * 'a -> bool,
                            toWord: 'a -> word): unit =
                     let
                        val table = HashSet.new {hash = toWord}
                        val _ =
                           Vector.foreach
                           (cases, fn (x, _) =>
                            let
                               val _ = 
                                  HashSet.insertIfNew
                                  (table, toWord x, fn y => equals (x, y),
                                   fn () => x, 
                                   fn _ => Error.bug "Ssa.TypeCheck.loopTransfer: redundant branch in case")
                            in
                               ()
                            end)
                     in
                        if isSome default
                           then ()
                        else Error.bug "Ssa.TypeCheck.loopTransfer: case has no default"
                     end
                  fun doitCon cases =
                     let
                        val numCons = 
                           case Type.dest (getVar' test) of
                              Type.Datatype t => getTycon' t
                            | _ => Error.bug "Ssa.TypeCheck.loopTransfer: case test is not a datatype"
                        val cons = Array.array (numCons, false)
                        val _ =
                           Vector.foreach
                           (cases, fn (con, _) =>
                            let
                               val i = getCon' con
                            in
                               if Array.sub (cons, i)
                                  then Error.bug "Ssa.TypeCheck.loopTransfer: redundant branch in case"
                               else Array.update (cons, i, true)
                            end)
                     in
                        case (Array.forall (cons, fn b => b), isSome default) of
                           (true, true) =>
                              Error.bug "Ssa.TypeCheck.loopTransfer: exhaustive case has default"
                         | (false, false) =>
                              Error.bug "Ssa.TypeCheck.loopTransfer: non-exhaustive case has no default"
                         | _ => ()
                     end
                  val _ = getVar test
               in
                  case cases of
                     Cases.Con cs => doitCon cs 
                   | Cases.Word (_, cs) =>
                        doit (cs, WordX.equals, Word.fromIntInf o WordX.toIntInf)
               end
          | Goto {args, ...} => getVars args
          | Raise xs => getVars xs
          | Return xs => getVars xs
          | Runtime {args, ...} => getVars args
      fun loopFunc (f: Function.t) =
         let
            val {args, blocks, ...} = Function.dest f
            (* Descend the dominator tree, verifying that variable definitions
             * dominate variable uses.
             *)
            fun loop (Tree.T (block, children)): unit =
               let
                  val Block.T {args, statements, transfer, ...} = block
                  val _ = Vector.foreach (args, bindVar)
                  val _ = Vector.foreach (statements, loopStatement)
                  val _ = loopTransfer transfer
                  val _ = Vector.foreach (children, loop)
                  val _ =
                     Vector.foreach (statements, fn s =>
                                     Option.app (Statement.var s, unbindVar))
                  val _ = Vector.foreach (args, unbindVar o #1)
               in
                  ()
               end
            val _ = Vector.foreach (args, bindVar)
            val _ = Vector.foreach (blocks, bindLabel o Block.label)
            val _ =
               Vector.foreach
               (blocks, fn Block.T {transfer, ...} =>
                Transfer.foreachLabel (transfer, getLabel))
            val _ = loop (Function.dominatorTree f)
            val _ = Vector.foreach (blocks, unbindLabel o Block.label)
            val _ = Vector.foreach (args, unbindVar o #1)
            val _ = Function.clear f
         in
             ()
         end
      val _ = Vector.foreach
              (datatypes, fn Datatype.T {tycon, cons} =>
               (bindTycon (tycon, Vector.length cons) ;
                Vector.foreachi (cons, fn (i, {con, ...}) => bindCon (con, i))))
      val _ = Vector.foreach (globals, loopStatement)
      val _ = List.foreach (functions, bindFunc o Function.name)
      val _ = List.foreach (functions, loopFunc)
      val _ = getFunc main
      val _ = Program.clearTop program
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
      val out = Out.error
      val print = Out.outputc out
      exception TypeError
      fun error (msg, lay) =
         (print (concat ["Type error: ", msg, "\n"])
          ; Layout.output (lay, out)
          ; print "\n"
          ; raise TypeError)
      fun coerce {from: Type.t, to: Type.t}: unit =
         if Type.equals (from, to)
            then ()
         else error ("Ssa.TypeCheck.coerce",
                     Layout.record [("from", Type.layout from),
                                    ("to", Type.layout to)])
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
            NONE => error ("select of non tuple", Layout.empty)
          | SOME ts => Vector.sub (ts, offset)
      val {get = conInfo: Con.t -> {args: Type.t vector,
                                    result: Type.t},
           set = setConInfo, ...} =
         Property.getSetOnce
         (Con.plist, Property.initRaise ("TypeCheck.info", Con.layout))
      val _ =
         Vector.foreach
         (datatypes, fn Datatype.T {tycon, cons} =>
          let val result = Type.con (tycon, Vector.new0 ())
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
               else error ("bad primapp",
                           let
                              open Layout
                           in
                              seq [Prim.layout prim,
                                   tuple (Vector.toListMap (args, Type.layout))]
                           end)
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
         handle e => error (concat ["analyze raised exception ",
                                    Layout.toString (Exn.layout e)],
                            Layout.empty)
      val _ = Program.clear program
   in
      ()
   end

val typeCheck = Control.trace (Control.Pass, "typeCheck") typeCheck

end
