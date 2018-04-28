(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor KnownCase (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM =
struct

open S
open Exp Transfer

fun mkPost ()
  = let
      val post = ref []
    in
      {addPost = fn th => List.push (post, th),
       post = fn () => List.foreach(!post, fn th => th ())}
    end

structure TyconInfo =
  struct
    datatype t = T of {cons: Con.t vector}

    local 
      fun make f (T r) = f r
    in
      val cons = make #cons
    end

    fun layout (T {cons, ...}) 
      = Layout.record [("cons", Vector.layout Con.layout cons)]
  end

structure ConInfo =
  struct
    datatype t = T of {args: Type.t vector,
                       index: int, 
                       tycon: Tycon.t}

    local 
      fun make f (T r) = f r
    in
      val args = make #args
      val index = make #index
    end

    fun layout (T {index, ...}) 
      = Layout.record [("index", Int.layout index)]
  end

structure ConValue =
  struct
    type w = Var.t ref vector
    type v = w option
    type u = v option
    type t = Con.t * u

    val equalsW : w * w -> bool
      = fn (x, y) => Vector.equals (x, y, fn (x, y) => Var.equals (!x, !y))

    val layoutW = Vector.layout (Var.layout o !)
    val layoutV = Option.layout layoutW
    val layoutU = Option.layout layoutV
    val layout : t -> Layout.t = Layout.tuple2 (Con.layout, layoutU)

    val joinV : v * v -> v
      = fn (SOME x, SOME y) 
         => if equalsW (x, y)
              then SOME x
              else NONE
         | (NONE, _) => NONE
         | (_, NONE) => NONE
    val joinU : u * u -> u
      = fn (SOME x, SOME y) => SOME (joinV (x, y))
         | (NONE, y) => y
         | (x, NONE) => x
    val join : t * t -> t
      = fn ((conx, x), (cony, y)) => 
        if Con.equals (conx, cony)
          then (conx, joinU (x, y))
          else Error.bug "KnownCase.ConValue.join"

    fun newKnown (con, args) : t = (con, SOME (SOME args))
    fun newUnknown con : t = (con, SOME NONE)
    fun new con : t = (con, NONE)

    fun isTop ((_, x) : t) = isSome x

    val con : t -> Con.t = fn (conx, _) => conx
  end

structure TyconValue =
  struct
    type t = ConValue.t vector

    val layout : t -> Layout.t = Vector.layout ConValue.layout

    val join : t * t -> t
      = fn (x, y) => Vector.map2 (x, y, ConValue.join)

    fun newKnown (cons, con, args)
      = Vector.map
        (cons, fn con' =>
         if Con.equals (con, con')
           then ConValue.newKnown (con, args)
           else ConValue.new con')

    fun newUnknown cons = Vector.map (cons, ConValue.newUnknown)

    val cons : t -> Con.t vector
      = fn x => Vector.map (x, ConValue.con)
  end

structure VarInfo =
  struct
    datatype t = T of {active: bool ref,
                       tyconValues: TyconValue.t list ref,
                       var: Var.t}

    local 
      fun make f (T r) = f r
      fun make' f = (make f, ! o (make f))
    in
      val (_, active') = make' #active
    end

    fun layout (T {active, tyconValues, var, ...}) 
      = Layout.record [("active", Bool.layout (!active)),
                       ("tyconValues", List.layout TyconValue.layout (!tyconValues)),
                       ("var", Var.layout var)]

    fun new var = T {active = ref false,
                     tyconValues = ref [],
                     var = var}

    fun deactivate (T {active, ...}) = active := false
    fun activate (T {active, ...}) = active := true
    fun activate' (vi, addPost: (unit -> unit) -> unit)
      = (addPost (fn () => deactivate vi);
         activate vi)
    val active = active'

    fun tyconValue (T {tyconValues, ...})
      = case !tyconValues of h::_ => SOME h | _ => NONE
    fun popTyconValue (T {tyconValues, ...}) = ignore (List.pop tyconValues)
    fun pushTyconValue (T {tyconValues, ...}, tcv) = List.push (tyconValues, tcv)
    fun pushTyconValue' (vi, tcv, addPost)
      = let
          val _ = pushTyconValue (vi, tcv)
          val _ = addPost (fn () => popTyconValue vi)
        in
          ()
        end
    fun joinActiveTyconValue (vi, tcv, addPost, addPost')
      = if active vi
          then let val tcv' = valOf (tyconValue vi)
               in 
                 popTyconValue vi;
                 pushTyconValue (vi, TyconValue.join (tcv, tcv'))
               end
          else (activate' (vi, addPost');
                pushTyconValue' (vi, tcv, addPost))
  end

structure ReplaceInfo =
  struct
    datatype t = T of {replaces: Var.t ref list ref}

    fun new var = T {replaces = ref [ref var]}

    fun replace (T {replaces, ...})
      = case !replaces of h::_ => h | _ => Error.bug "KnownCase.ReplaceInfo.replace"
    fun popReplace (T {replaces, ...}) = ignore (List.pop replaces)
    fun pushReplace (T {replaces, ...}, rep) = List.push (replaces, ref rep)
    fun pushReplace' (vi, rep, addPost)
      = let
          val _ = pushReplace (vi, rep)
          val _ = addPost (fn () => popReplace vi)
        in
          ()
        end
    fun flipReplace (vi, rep) 
      = let val r = replace vi 
        in !r before (r := rep) 
        end
    fun flipReplace' (vi, rep, addPost)
      = let 
          val rep = flipReplace (vi, rep)
          val _ = addPost (fn () => ignore (flipReplace (vi, rep)))
        in 
          rep
        end
    fun nextReplace' (vi, rep, addPost)
      = let 
          val rep = flipReplace' (vi, rep, addPost)
          val _ = pushReplace' (vi, rep, addPost)
        in
          ()
        end
  end

structure LabelInfo =
  struct
    datatype t = T of {activations: (VarInfo.t * TyconValue.t) list ref,
                       block: Block.t, 
                       depth: int ref,
                       pred: Label.t option option ref}

    local 
      fun make f (T r) = f r
      fun make' f = (make f, ! o (make f))
    in
      val block = make #block
      val (_, depth') = make' #depth
    end

    fun layout (T {pred, ...}) 
      = Layout.record 
        [("pred", Option.layout (Option.layout Label.layout) (!pred))]

    fun new block = T {activations = ref [],
                       block = block,
                       depth = ref 0,
                       pred = ref NONE}

    fun popDepth (T {depth, ...}) = Int.dec depth
    fun pushDepth (T {depth, ...}) = Int.inc depth
    fun pushDepth' (li, addPost)
      = let
          val _ = pushDepth li
          val _ = addPost (fn () => popDepth li)
        in
          ()
        end

    fun addPred (T {pred, ...}, l)
      = case !pred
          of NONE => pred := SOME (SOME l)
           | SOME NONE => ()
           | SOME (SOME l') => if Label.equals (l, l')
                                 then ()
                                 else pred := SOME NONE
    fun onePred (T {pred, ...})
      = case !pred
          of SOME (SOME _) => true
           | _ => false

    fun addActivation (T {activations, ...}, activation)
      = List.push (activations, activation)
    fun activate (T {activations, ...}, addPost)
      = let
          val {addPost = addPost', post = post'} = mkPost ()
        in 
          List.foreach
          (!activations, fn (vi, tcv) =>
           VarInfo.joinActiveTyconValue (vi, tcv, addPost, addPost'));
          post' ()
        end
    val activate : t * ((unit -> unit) -> unit) -> unit
      = Trace.trace
        ("KnownCase.LabelInfo.activate",
         fn (T {activations, block = Block.T {label, ...}, ...}, _) =>
         let open Layout
         in
           seq [Label.layout label,
                str " ",
                (List.layout (tuple2 (VarInfo.layout,
                                      TyconValue.layout))
                             (!activations))]
         end,
         Layout.ignore)
        activate
  end

fun transform (Program.T {globals, datatypes, functions, main})
  = let
      (* restore and shrink *)
      val restore = restoreFunction {globals = globals}
      val shrink = shrinkFunction {globals = globals}

      (* tyconInfo and conInfo *)
      val {get = tyconInfo: Tycon.t -> TyconInfo.t,
           set = setTyconInfo, ...}
        = Property.getSetOnce
          (Tycon.plist, Property.initRaise ("knownCase.tyconInfo", Tycon.layout))
      val {get = conInfo: Con.t -> ConInfo.t,
           set = setConInfo, ...}
        = Property.getSetOnce
          (Con.plist, Property.initRaise ("knownCase.conInfo", Con.layout))
      val _ = Vector.foreach
              (datatypes, fn Datatype.T {tycon, cons} =>
               (setTyconInfo (tycon, TyconInfo.T {cons = Vector.map (cons, #con)});
                Vector.foreachi
                (cons, fn (i, {con, args}) =>
                 setConInfo (con, ConInfo.T {args = args,
                                             index = i, 
                                             tycon = tycon}))))
      (* Diagnostics *)
      val _ = Control.diagnostics
              (fn display =>
               let open Layout
               in
                 Vector.foreach
                 (datatypes, fn Datatype.T {tycon, cons} =>
                  let val tci = tyconInfo tycon
                  in
                    display (seq [Tycon.layout tycon, str " ",
                                  TyconInfo.layout tci,
                                  Vector.layout 
                                  (fn {con, ...} =>
                                   let val ci = conInfo con
                                   in
                                     seq [Con.layout con, str " ",
                                          ConInfo.layout ci]
                                   end)
                                  cons])
                  end)
               end)
      fun optimizeTycon _ = true
      fun optimizeType ty = case Type.dest ty
                              of Type.Datatype tycon => optimizeTycon tycon
                               | _ => false

      (* varInfo *)
      val {get = varInfo: Var.t -> VarInfo.t, ...}
        = Property.getSetOnce
          (Var.plist, Property.initFun (fn x => VarInfo.new x))
      (* replaceInfo *)
      val {get = replaceInfo: Var.t -> ReplaceInfo.t, ...}
        = Property.get
          (Var.plist, Property.initFun (fn x => ReplaceInfo.new x))


      fun bindVar' (x, ty, exp, addPost)
        = case Type.dest ty
            of Type.Datatype tycon
             => if optimizeTycon tycon
                  then let
                         val cons = TyconInfo.cons (tyconInfo tycon)
                         val tyconValue
                           = case exp
                               of SOME (ConApp {con, args})
                                => TyconValue.newKnown 
                                   (cons, con, 
                                    Vector.map 
                                    (args, ReplaceInfo.replace o replaceInfo))
                                | _ => TyconValue.newUnknown cons
                       in
                         VarInfo.pushTyconValue'
                         (varInfo x, tyconValue, addPost)
                       end
                  else ()
             | _ => ()

      fun bindVarArgs' (args, addPost)
        = Vector.foreach 
          (args, fn (x, ty) =>
           bindVar' (x, ty, NONE, addPost))
      fun bindVarArgs args = bindVarArgs' (args, ignore)
      fun bindVarStatement' (Statement.T {var, ty, exp}, addPost)
        = Option.app 
          (var, fn x => 
           bindVar' (x, ty, SOME exp, addPost))
      fun bindVarStatements' (statements, addPost)
        = Vector.foreach 
          (statements, fn statement => 
           bindVarStatement' (statement, addPost))
      fun bindVarStatements statements = bindVarStatements' (statements, ignore)

      val _ = bindVarStatements globals
      (* Diagnostics *)
      val _ = Control.diagnostics
              (fn display =>
               let open Layout
               in
                 Vector.foreach
                 (globals, fn Statement.T {var, ...} => 
                  Option.app
                  (var, fn x =>
                   let val vi = varInfo x
                   in 
                     display (seq [Var.layout x, str " ",
                                   VarInfo.layout vi])
                   end))
               end)

      (* labelInfo *)
      val {get = labelInfo: Label.t -> LabelInfo.t, 
           set = setLabelInfo, ...}
        = Property.getSetOnce
          (Label.plist, Property.initRaise ("knownCase.labelInfo", Label.layout))

      val functions
        = List.revMap
          (functions, fn f =>
           let
             val {args, blocks, mayInline, name, raises, returns, start} =
                Function.dest f
             val _ = Vector.foreach
                     (blocks, fn block as Block.T {label, ...} =>
                      setLabelInfo (label, LabelInfo.new block))
             val _ = Vector.foreach
                     (blocks, fn Block.T {label, transfer, ...} =>
                      Transfer.foreachLabel
                      (transfer, fn l =>
                       let val li = labelInfo l
                       in LabelInfo.addPred (li, label)
                       end))
             (* Diagnostics *)
             val _ = Control.diagnostics
                     (fn display =>
                      let open Layout
                      in
                        Vector.foreach
                        (blocks, fn Block.T {label, ...} =>
                         let val li = labelInfo label
                         in 
                           display (seq [Label.layout label, str " ",
                                         LabelInfo.layout li])
                         end)
                      end)

             val newBlocks = ref []
             fun addBlock block = List.push (newBlocks, block)
             fun addNewBlock (block as Block.T {label, ...}) 
               = (setLabelInfo (label, LabelInfo.new block); 
                  addBlock block)
             local 
               val table: {hash: word,
                           transfer: Transfer.t,
                           label: Label.t} HashSet.t
                 = HashSet.new {hash = #hash}
             in
                fun newBlock transfer =
                   let
                      val label = Label.newNoname ()
                      val block = Block.T {label = label,
                                           args = Vector.new0 (),
                                           statements = Vector.new0 (),
                                           transfer = transfer}
                      val _ = addNewBlock block
                   in
                      label
                   end
                (* newBlock' isn't used, because it shares blocks that causes
                 * violation of the requirements for profiling information --
                 * namely that each block correspond to a unique sequence of
                 * source infos at it' start.
                 *
                 * I left the code in case we want to enable it when compiling
                 * without profiling.
                 *)
                fun newBlock' transfer
                 = let
                     val hash = Transfer.hash transfer
                     val {label, ...}
                       = HashSet.lookupOrInsert
                         (table, hash,
                          fn {transfer = transfer', ...} =>
                          Transfer.equals (transfer, transfer'),
                          fn () => {hash = hash,
                                    label = newBlock transfer,
                                    transfer = transfer})
                   in
                     label
                   end
                val _ = newBlock' (* quell unused variable warning *)
               fun bugBlock () = newBlock Bug
             end

             val traceRewriteGoto
               = Trace.trace
                 ("KnownCase.rewriteGoto",
                  fn {dst, args} =>
                  Layout.record
                  [("dst", Label.layout dst),
                   ("args", Vector.layout Var.layout args)],
                  Option.layout
                  (Layout.tuple2
                   (Vector.layout Statement.layout,
                    Transfer.layout)))
             val traceRewriteCase
               = Trace.trace
                 ("KnownCase.rewriteCase",
                  fn {test, cases, default} =>
                  Layout.record
                  [("test", Var.layout test),
                   ("cases", Vector.layout 
                             (Layout.tuple2 (Con.layout, Label.layout))
                             cases),
                   ("default", Option.layout Label.layout default)],
                  Option.layout
                  (Layout.tuple2
                   (Vector.layout Statement.layout,
                    Transfer.layout)))
             val traceRewriteTransfer
               = Trace.trace
                 ("KnownCase.rewriteTransfer",
                  Transfer.layout,
                  Option.layout
                  (Layout.tuple2
                   (Vector.layout Statement.layout,
                    Transfer.layout)))

             fun rewriteGoto' {dst, args} :
                              (Statement.t vector * Transfer.t) option
               = let
                   val li = labelInfo dst
                   val Block.T {args = argsDst,
                                statements = statementsDst,
                                transfer = transferDst, ...}
                     = LabelInfo.block li
                   val depthDst = LabelInfo.depth' li
                 in
                   if depthDst <= 2
                      andalso
                      Vector.fold
                      (statementsDst, 0,
                       fn (Statement.T {exp = Profile _, ...}, i) => i
                        | (_, i) => i + 1) <= 0
                     then let
                            val {addPost, post} = mkPost ()
                            val _ = LabelInfo.pushDepth' (li, addPost)

                            val vars = Vector.map2
                                       (args, argsDst,
                                        fn (x, (z, ty)) =>
                                        (x, Var.newNoname (), 
                                         z, Var.newNoname (), ty))

                            val moves1
                              = if depthDst > 0
                                  then Vector.map
                                       (vars, fn (_, _, z, t, ty) =>
                                        (if optimizeType ty
                                           then let
                                                  val zvi = varInfo z
                                                  val tvi = varInfo t
                                                in
                                                  VarInfo.pushTyconValue'
                                                  (tvi,
                                                   valOf (VarInfo.tyconValue zvi),
                                                   addPost)
                                                end
                                           else ();
                                         ReplaceInfo.nextReplace'
                                         (replaceInfo z, t, addPost);
                                         Statement.T {var = SOME t,
                                                      ty = ty,
                                                      exp = Var z}))
                                  else Vector.new0 ()
                            val moves2
                              = Vector.map
                                (vars, fn (x, t, _, _, ty) =>
                                 (if optimizeType ty
                                    then let
                                           val xvi = varInfo x
                                           val tvi = varInfo t
                                         in
                                           VarInfo.pushTyconValue'
                                           (tvi,
                                            valOf (VarInfo.tyconValue xvi),
                                            addPost)
                                         end
                                    else ();
                                  Statement.T {var = SOME t,
                                               ty = ty,
                                               exp = Var x}))
                            val moves3
                              = Vector.map
                                (vars, fn (_, t, z, _, ty) =>
                                 (if optimizeType ty
                                    then let
                                           val tvi = varInfo t
                                           val zvi = varInfo z
                                         in
                                           VarInfo.pushTyconValue'
                                           (zvi,
                                            valOf (VarInfo.tyconValue tvi),
                                            addPost)
                                         end
                                    else ();
                                  Statement.T {var = SOME z,
                                               ty = ty,
                                               exp = Var t}))
                            val _ = bindVarStatements' (statementsDst, addPost)
                          in
                            (case rewriteTransfer transferDst
                               of NONE => NONE
                                | SOME (newStatements, newTransfer)
                                => SOME (Vector.concat [moves1, moves2, moves3, 
                                                        statementsDst,
                                                        newStatements],
                                         newTransfer))
                            before (post ())
                          end
                     else NONE
                 end
             and rewriteGoto goto = traceRewriteGoto
                                    rewriteGoto'
                                    goto

             and rewriteCase' {test, cases, default} :
                              (Statement.t vector * Transfer.t) option

               = let
                   val {addPost, post} = mkPost ()

                   val testvi = varInfo test
                   val tyconValue as conValues
                     = case VarInfo.tyconValue testvi
                         of SOME tyconValue => tyconValue
                          | _ => Error.bug "KnownCase.rewriteCase: tyconValue"
                    val cons = TyconValue.cons tyconValue
                    val numCons = Vector.length cons

                    datatype z = None
                               | One of (Con.t * ConValue.v)
                               | Many

                    fun doOneSome (con, args)
                      = let
                          val goto
                            = case Vector.peek
                                   (cases, fn (con', _) =>
                                    Con.equals (con, con'))
                                of SOME (_, dst)
                                 => {dst = dst, args = Vector.map (args, !)}
                                 | NONE
                                 => {dst = valOf default,
                                     args = Vector.new0 ()}
                        in
                          case rewriteGoto goto
                            of NONE => SOME (Vector.new0 (), Transfer.Goto goto)
                             | sst => sst
                        end
                    val doOneSome
                      = Trace.trace
                        ("KnownCase.doOneSome",
                         Layout.ignore, Layout.ignore)
                        doOneSome

                    fun rewriteDefault conValues'
                      = let
                          val _ = VarInfo.pushTyconValue'
                                  (testvi, conValues', addPost)
                        in
                          rewriteGoto {dst = valOf default, args = Vector.new0 ()}
                        end
                    val rewriteDefault
                      = Trace.trace
                        ("KnownCase.rewriteCase.rewriteDefault",
                         Layout.ignore, Layout.ignore)
                        rewriteDefault

fun doOneNone con
  = let
      fun doit dst
        = SOME (Vector.new0 (),
                Case
                {test = test,
                 cases = Cases.Con (Vector.new1 (con, dst)),
                 default = if numCons = 1
                             then NONE
                             else SOME (bugBlock ())})
    in
      case Vector.peek
           (cases, fn (con', _) =>
            Con.equals (con, con'))
        of SOME (_, dst) => doit dst
         | NONE
         => let
              val args 
                = Vector.map
                  (ConInfo.args (conInfo con),
                   fn ty => 
                   let
                     val x = Var.newNoname ()
                     val xvi = varInfo x
                     val _ = case Type.dest ty
                               of Type.Datatype tycon
                                => if optimizeTycon tycon
                                     then VarInfo.pushTyconValue'
                                          (xvi,
                                           TyconValue.newUnknown
                                           (TyconInfo.cons (tyconInfo tycon)),
                                           addPost)
                                     else ()
                                | _ => ()
                   in
                     (x, ty)
                   end)
              val (xs, _) = Vector.unzip args
              val conValues' = TyconValue.newKnown 
                               (cons, con,
                                Vector.map 
                                (xs, ReplaceInfo.replace o replaceInfo))
              val label = Label.newNoname ()
              val (statements, transfer)
                = case rewriteDefault conValues'
                    of SOME sst => sst
                     | NONE => (Vector.new0 (),
                                Goto {dst = valOf default,
                                      args = Vector.new0 ()})
              val block = Block.T
                          {label = label,
                           args = args,
                           statements = statements,
                           transfer = transfer}
              val _ = addNewBlock block
            in
              doit label
            end
    end
val doOneNone
  = Trace.trace
    ("KnownCase.rewriteCase.doOneNone",
     Layout.ignore, Layout.ignore)
    doOneNone

fun doMany ()
  = let
      val usedCons = Array.new (numCons, false)
      val cases = Vector.keepAllMap
                  (cases, fn (con, dst) =>
                   let
                     val conIndex = ConInfo.index (conInfo con)
                     val _ = Array.update (usedCons, conIndex, true)
                   in
                     if ConValue.isTop (Vector.sub (conValues, conIndex))
                       then SOME (con, dst)
                       else NONE
                   end)
      val (cases, default)
        = case default
            of NONE => (cases, NONE)
             | SOME dst
             => let
                  val conValues' = Vector.mapi
                                   (cons, fn (i, con) =>
                                    if Array.sub (usedCons, i)
                                      then ConValue.new con
                                      else Vector.sub (conValues, i))

                  fun route (statements, (cases, default))
                    = if Vector.isEmpty statements
                        then (cases, default)
                        else let
                               fun route' dst
                                 = let
                                     val Block.T {args, ...} 
                                       = LabelInfo.block (labelInfo dst)

                                     val label = Label.newNoname ()
                                     val args = Vector.map
                                                (args, fn (_, ty) => 
                                                 (Var.newNoname (), ty))
                                     val xs = Vector.map (args, #1)
                                     val block = Block.T
                                                 {label = label,
                                                  args = args,
                                                  statements = statements,
                                                  transfer = Goto {dst = dst,
                                                                   args = xs}}
                                     val _ = addNewBlock block
                                   in
                                     label
                                   end
                             in
                               (Vector.map (cases, fn (con, dst) => (con, route' dst)),
                                Option.map (default, route'))
                             end

                in
                  case rewriteDefault conValues'
                    of SOME (statements, 
                             Case {test = test', 
                                   cases = Cases.Con cases',
                                   default = default'})
                     => if Option.equals
                           (SOME test,
                            Vector.foldr
                            (statements, SOME test',
                             fn (Statement.T _, NONE) => NONE
                              | (Statement.T {var, exp, ...}, SOME test') =>
                             if Option.equals (var, SOME test', Var.equals)
                               then case exp
                                      of Var test' => SOME test'
                                       | _ => NONE
                               else SOME test'),
                            Var.equals)
                          then let
                                 val (cases', default') 
                                   = route (statements, (cases', default'))
                               in
                                 (Vector.concat [cases, cases'], default')
                               end
                          else (cases, SOME dst)
                     | SOME (statements, transfer)
                     => let
                          val label
                            = if Vector.isEmpty statements
                                then newBlock transfer
                                else let
                                       val label = Label.newNoname ()
                                       val block = Block.T
                                                   {label = label,
                                                    args = Vector.new0 (),
                                                    statements = statements,
                                                    transfer = transfer}
                                       val _ = addNewBlock block
                                     in
                                       label
                                     end
                        in
                          (cases, SOME label)
                        end
                     | NONE => (cases, SOME dst)
                end
      val numCases = Vector.length cases
      fun doit (cases, default)
        = SOME (Vector.new0 (),
                Case {test = test,
                      cases = Cases.Con cases,
                      default = default})
    in
      if numCases = numCons
        then doit (cases, NONE)
        else doit (cases,
                   case default
                     of SOME _ => default
                      | NONE => SOME (bugBlock ()))
    end
val doMany
  = Trace.trace
    ("KnownCase.rewriteCase.doMany",
     Layout.ignore, Layout.ignore)
    doMany

                 in
(*
                   (if Vector.forall
                       (conValues, ConValue.isTop)
*)
                   (if false
                      then NONE
                      else case Vector.foldi
                                (conValues, None, 
                                 fn (_, _, Many) => Many
                                  | (_, conValue, One ccv)
                                  => (case conValue
                                        of (_, NONE) => One ccv
                                         | (_, SOME _) => Many)
                                  | (_, conValue, None)
                                  => (case conValue
                                        of (_, NONE) => None
                                         | (con, SOME cv) => One (con, cv)))
                             of None => SOME (Vector.new0 (), Bug)
                              | One (con, SOME args) => doOneSome (con, args)
                              | One (con, NONE) => doOneNone con
                              | Many => doMany ())
                   before (post ())
                 end
             and rewriteCase casee = traceRewriteCase
                                     rewriteCase'
                                     casee

             and rewriteTransfer' (transfer: Transfer.t) : 
                                  (Statement.t vector * Transfer.t) option
               = case transfer
                   of Goto {dst, args} => rewriteGoto {dst = dst, args = args}
                    | Case {test, cases = Cases.Con cases, default}
                    => rewriteCase {test = test, cases = cases, default = default}
                    | _ => NONE
             and rewriteTransfer transfer = traceRewriteTransfer
                                            rewriteTransfer'
                                            transfer

             fun activateGoto {dst, args}
               = let
                   val liDst = labelInfo dst
                   val Block.T {args = argsDst, ...}
                     = LabelInfo.block liDst
                 in
                   if LabelInfo.onePred liDst
                     then Vector.foreach2
                          (args, argsDst, fn (x, (y, ty)) =>
                           if optimizeType ty
                             then let
                                    val xvi = varInfo x
                                    val yvi = varInfo y
                                    val conValues' 
                                      = valOf (VarInfo.tyconValue xvi)
                                  in
                                    LabelInfo.addActivation
                                    (liDst, (yvi, conValues'))
                                  end
                             else ())
                     else ()
                 end
             fun activateCase {test, cases, default}
               = let
                   val testvi = varInfo test
                   val tyconValue as conValues
                     = case VarInfo.tyconValue testvi
                         of NONE => Error.bug "KnownCase.activateCase: tyconValue"
                          | SOME tyconValue => tyconValue
                   val cons = TyconValue.cons tyconValue
                   val numCons = Vector.length cons

                   val usedCons = Array.new (numCons, false)
                 in
                   Vector.foreach
                   (cases, fn (con, dst) =>
                    let
                      val conIndex = ConInfo.index (conInfo con)
                      val _ = Array.update (usedCons, conIndex, true)
                      val liDst = labelInfo dst
                      val Block.T {args = argsDst, ...}
                        = LabelInfo.block liDst
                      val conValues' 
                        = TyconValue.newKnown 
                          (cons, con, 
                           Vector.map 
                           (argsDst, ReplaceInfo.replace o replaceInfo o #1))
                    in
                      if LabelInfo.onePred liDst
                        then LabelInfo.addActivation
                             (liDst, (testvi, conValues'))
                        else ()
                    end);
                   Option.app
                   (default, fn dst =>
                    let
                      val liDst = labelInfo dst
                      val conValues' = Vector.mapi
                                       (cons, fn (i, con) =>
                                        if Array.sub (usedCons, i)
                                          then ConValue.new con
                                          else Vector.sub (conValues, i))
                    in
                      if LabelInfo.onePred liDst
                        then LabelInfo.addActivation
                             (liDst, (testvi, conValues'))
                        else ()
                    end)
                 end
             fun activateTransfer transfer
               = case transfer
                   of Goto {dst, args}
                    => activateGoto {dst = dst, args = args}
                    | Case {test, cases = Cases.Con cases, default}
                    => activateCase {test = test, cases = cases, default = default}
                    | _ => ()

             fun rewriteBlock (Block.T {label, args, statements, transfer},
                               addPost)
               = let
                   val li = labelInfo label
                   val _ = LabelInfo.pushDepth' (li, addPost)
                   val _ = bindVarArgs' (args, addPost)
                   val _ = LabelInfo.activate (li, addPost)
                   val _ = bindVarStatements' (statements, addPost)
                   val _ = activateTransfer transfer
                   val (statements, transfer)
                     = case rewriteTransfer transfer
                         of NONE => (statements, transfer)
                          | SOME (newStatements, newTransfer)
                          => (Vector.concat [statements,newStatements],
                              newTransfer)
                 in
                    Block.T {label = label,
                             args = args,
                             statements = statements,
                             transfer = transfer}
                 end
             val rewriteBlock
               = Trace.trace
                 ("KnownCase.rewriteBlock",
                  Layout.tuple2 (Block.layout, Layout.ignore),
                  Block.layout)
                 rewriteBlock

             fun doitTree tree
               = let
                   fun loop (Tree.T (block, children))
                     = let
                         val {addPost, post} = mkPost ()
                         val block = rewriteBlock (block, addPost)
                       in
                         addBlock block ;
                         Vector.foreach (children, loop) ;
                         post ()
                       end
                   val _ = loop tree
                 in
                   Vector.fromListRev (!newBlocks)
                 end
             val _ = bindVarArgs args
             val blocks = doitTree (Function.dominatorTree f)

             val f = Function.new {args = args,
                                   blocks = blocks,
                                   mayInline = mayInline,
                                   name = name,
                                   raises = raises,
                                   returns = returns,
                                   start = start}
             val _ = Control.diagnostics
                     (fn display =>
                      display (Function.layout f))
             val f = eliminateDeadBlocksFunction f
             val _ = Control.diagnostics
                     (fn display =>
                      display (Function.layout f))
             val f = restore f
             val _ = Control.diagnostics
                     (fn display =>
                      display (Function.layout f))
             val f = shrink f
             val _ = Control.diagnostics
                     (fn display =>
                      display (Function.layout f))
             val _ = Function.clear f
           in
             f
           end)
      val program = Program.T {datatypes = datatypes,
                               globals = globals,
                               functions = functions,
                               main = main}
      val _ = Program.clearTop program
    in
      program
    end
end
