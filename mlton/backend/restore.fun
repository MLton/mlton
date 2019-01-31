(* Copyright (C) 2019 Jason Carr.
 * Copyright (C) 2009,2017 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Restore RSSA (based on ssa/restore2.fun)
 *
 * Based primarily on Section 19.1 of Appel's "Modern Compiler Implementation in ML",
 * (but see the caveats in the comments below).
 * The main deviation is the calculation of liveness of the violating variables,
 * which is used to predicate the insertion of phi arguments.  This is due to 
 * the algorithm's bias towards imperative languages, for which it makes the 
 * assumption that all variables are defined in the start block and all variables
 * are "used" at exit.  
 * This is "optimized" for restoration of functions with small numbers of violating
 * variables -- use bool vectors to represent sets of violating variables.
 * Also, we use a Promise.t to suspend part of the dominance frontier computation.
 *)

functor RestoreR (S: RESTORE_R_STRUCTS): RESTORE_R =
struct

structure Control =
   struct
      open Control
      fun diagnostics _ = ()
   end

open S
open Transfer

structure LabelInfo =
  struct
    datatype t = T of {args: (Var.t * Type.t) vector ref,
                       preds: Label.t list ref,
                       defs: bool vector ref,
                       uses: bool vector ref,
                       live: bool array ref,
                       dtindex: int ref,
                       df: Label.t vector Promise.t ref,
                       phi: Var.t list ref,
                       phiArgs: Var.t vector ref,
                       kind: Kind.t ref,
                       queued: bool ref}

    fun layout (T {preds, defs, uses, live, dtindex, df, phiArgs, ...})
      = let open Layout
        in record [("preds", List.layout Label.layout (!preds)),
                   ("defs", Vector.layout Bool.layout (!defs)),
                   ("uses", Vector.layout Bool.layout (!uses)),
                   ("live", Array.layout Bool.layout (!live)),
                   ("dtindex", Int.layout (!dtindex)),
                   ("df", Promise.layout (Vector.layout Label.layout) (!df)),
                   ("phiArgs", Vector.layout Var.layout (!phiArgs))]
        end

    local 
      fun make f (T r) = f r
      fun make' f = (make f, ! o (make f))
    in
      val (args, args') = make' #args
      val (preds, preds') = make' #preds
      val (defs, defs') = make' #defs
      val (uses, uses') = make' #uses
      val (live, live') = make' #live
      val (dtindex, dtindex') = make' #dtindex
      val (df, df') = make' #df
      val (phi, _) = make' #phi
      val (phiArgs, phiArgs') = make' #phiArgs
      val (queued, _) = make' #queued
      val (kind, kind') = make' #kind
    end

    fun new (): t = T {args = ref (Vector.new0 ()),
                       preds = ref [],
                       defs = ref (Vector.new0 ()),
                       uses = ref (Vector.new0 ()),
                       live = ref (Array.new0 ()),
                       dtindex = ref ~1,
                       df = ref (Promise.delay (fn () => Vector.new0 ())),
                       phi = ref [],
                       phiArgs = ref (Vector.new0 ()),
                       kind = ref Kind.Jump,
                       queued = ref false}
  end

structure Cardinality =
  struct
    structure L = ThreePointLattice(val bottom = "zero"
                                    val mid = "one"
                                    val top = "many")
    open L

    val isZero = isBottom
    val isOne = isMid
    val makeOne = makeMid
    val isMany = isTop
    val makeMany = makeTop

    val inc: t -> unit
      = fn c => if isZero c
                  then makeOne c
                else if isOne c
                  then makeMany c
                else ()
   end

structure VarInfo =
  struct
    datatype t = T of {defs: Cardinality.t,
                       ty: Type.t ref,
                       index: int ref,
                       defSites: Label.t list ref,
                       useSites: Label.t list ref,
                       vars: Var.t list ref}

    fun layout (T {defs, index, defSites, useSites, vars, ...})
      = let open Layout
        in record [("defs", Cardinality.layout defs),
                   ("index", Int.layout (!index)),
                   ("defSites", List.layout Label.layout (!defSites)),
                   ("useSites", List.layout Label.layout (!useSites)),
                   ("vars", List.layout Var.layout (!vars))]
        end

    local 
      fun make f (T r) = f r
      fun make' f = (make f, ! o (make f))
    in
      val defs = make #defs
      val (index,index') = make' #index
      val (_,defSites') = make' #defSites
      val (_,useSites') = make' #useSites
      val (ty,ty') = make' #ty
    end
    fun addDef (T {defs, ...}) = Cardinality.inc defs
    fun addDefSite (T {defSites, ...}, l) = List.push(defSites, l)
    fun addUseSite (T {useSites, ...}, l) = List.push(useSites, l)
    val violates = Cardinality.isMany o defs

    fun new (): t = T {defs = Cardinality.new (),
                       index = ref ~1,
                       defSites = ref [],
                       useSites = ref [],
                       ty = ref Type.unit,
                       vars = ref []}

    fun pushVar (T {vars, ...}, var) = List.push (vars, var)
    fun popVar (T {vars, ...}) = ignore (List.pop vars)
    fun peekVar (T {vars, ...}) = case !vars
                                    of [] => NONE
                                     | h::_ => SOME h
  end

fun restoreFunction ()
  = let
      exception NoViolations

      val {get = varInfo: Var.t -> VarInfo.t, ...}
        = Property.get
          (Var.plist, Property.initFun (fn _ => VarInfo.new ()))

      val {get = labelInfo: Label.t -> LabelInfo.t, ...}
        = Property.get
          (Label.plist, Property.initFun (fn _ => LabelInfo.new ()))

      fun mkQueue ()
        = let
            val todo = ref []
          in
            {enque = fn (l, li) => let 
                                     val queued = LabelInfo.queued li
                                   in
                                     if !queued
                                       then ()
                                       else (queued := true ; 
                                             List.push (todo, (l,li)))
                                   end,
             deque = fn () => case !todo
                                of [] => NONE
                                 | (l,li)::todo' 
                                 => (todo := todo';
                                     LabelInfo.queued li := false;
                                     SOME (l,li))}
          end

      fun mkPost ()
        = let
            val post = ref []
          in
            {addPost = fn th => List.push (post, th),
             post = fn () => List.foreach(!post, fn th => th ())}
          end
    in
      fn (f: Function.t) =>
      let
        val {args, blocks, name, returns, raises, start} = Function.dest f
        (* check for violations *)
        val violations = ref []
        fun addDef (x, ty)
          = let
              val vi = varInfo x
            in
              if VarInfo.violates vi
                then ()
                else (VarInfo.ty vi := ty ;
                      VarInfo.addDef vi ;
                      if VarInfo.violates vi
                        then List.push (violations, x)
                        else ())
            end
        val _ = Function.foreachDef (f, addDef)

        (* escape early *)
        val _ = if List.isEmpty (!violations)
                  then (Control.diagnostics
                        (fn display =>
                         let
                           open Layout
                         in
                           display (seq [Func.layout name,
                                         str " NoViolations"])
                         end);
                        raise NoViolations)
                  else ()

        (* init violations *)
        val index = ref 0
        val violations
          = Vector.fromListMap
            (!violations, fn x =>
             let
               val vi = varInfo x
               val _ = VarInfo.index vi := (!index)
               val _ = Int.inc index
             in
               x
             end)
        val numViolations = !index

        (* Diagnostics *)
        val _ = Control.diagnostics
                (fn display =>
                 let
                   open Layout
                 in
                   display (seq [Func.layout name,
                                 str " Violations: ",
                                 Vector.layout Var.layout violations])
                 end)

        (* init entryBlock *)
        val entry = Label.newNoname ()
        val entryBlock = Block.T {label = entry,
                                  args = args,
                                  statements = Vector.new0 (),
                                  transfer = Goto {dst = start,
                                                   args = Vector.new0 ()},
                                  kind = Kind.Jump}

        (* compute dominator tree *)
        val dt = Function.dominatorTree f
        val dt' = Tree.T (entryBlock, Vector.new1 dt)

        (* compute df (dominance frontier) *)
        (*  based on section 19.1 of Appel's "Modern Compiler Implementation in ML" *)
        (* also computes defSites and useSites of violating variables *)
        (* also computes preds, defs, and uses *)
        val dtindex = ref 0
        fun doitTree (Tree.T (Block.T {label, args, statements, transfer, kind},
                              children))
          = let
              val li = labelInfo label

              val _ = LabelInfo.args li := args
              val _ = LabelInfo.kind li := kind

              val _ = Transfer.foreachLabel 
                      (transfer, fn l => 
                       List.push (LabelInfo.preds (labelInfo l), label))

              val defs = Array.new (numViolations, false)
              val uses = Array.new (numViolations, false)
              fun addDef x
                = let 
                    val vi = varInfo x
                  in 
                    if VarInfo.violates vi
                      then let
                             val index = VarInfo.index' vi
                           in
                             VarInfo.addDefSite (varInfo x, label);
                             Array.update (defs, index, true);
                             Array.update (uses, index, false)
                           end
                      else ()
                  end   
              fun addUse x
                = let 
                    val vi = varInfo x
                  in 
                    if VarInfo.violates vi
                      then let
                             val index = VarInfo.index' vi
                           in 
                             VarInfo.addUseSite (varInfo x, label);
                             Array.update (uses, index, true)
                           end
                      else ()
                  end
              val _ = Transfer.foreachDefLabelUse (transfer,
                      {def=addDef o #1, label=fn _ => (), use=addUse})
              val _ = Vector.foreachr
                      (statements, fn s => Statement.foreachDefUse
                        (s, {def=addDef o #1, use=addUse}))
              val _ = Vector.foreach (args, addDef o #1)
              val _ = LabelInfo.defs li := Array.toVector defs
              val _ = LabelInfo.uses li := Array.toVector uses
              val _ = LabelInfo.live li := Array.new (numViolations, false)

              val _ = Int.inc dtindex
              val dtindexMin = !dtindex
              val _ = LabelInfo.dtindex li := dtindexMin
              val _ = Vector.foreach(children, doitTree)
              val dtindexMax = !dtindex
              fun dominates l 
                = let val dtindex = LabelInfo.dtindex' (labelInfo l)
                  in dtindexMin < dtindex andalso dtindex <= dtindexMax
                  end

              fun promise ()
                = let
                    val df = ref []
                    fun addDF l
                      = if List.contains(!df, l, Label.equals)
                          then ()
                          else List.push(df,l)
                    val _ = Transfer.foreachLabel
                            (transfer, fn l =>
                             if Vector.exists
                                (children, fn Tree.T (b, _) => 
                                 Label.equals (Block.label b, l))
                               then ()
                               else addDF l)
                    val _ = Vector.foreach
                            (children, fn Tree.T (Block.T {label, ...}, _) =>
                             let
                               val li = labelInfo label
                             in
                               Vector.foreach
                               (Promise.force (LabelInfo.df' li), fn l =>
                                if dominates l
                                  then ()
                                  else addDF l)
                             end)
                  in
                    Vector.fromList (!df)
                  end
              val _ = LabelInfo.df li := Promise.delay promise
            in
              ()
            end
        val _ = doitTree dt'

        (* compute liveness *)
        val _
          = Vector.foreach
            (violations, fn x =>
             let
               val {enque, deque} = mkQueue ()
               val enque = fn l => enque (l, labelInfo l)

               val vi = varInfo x
               val index = VarInfo.index' vi
               val useSites = VarInfo.useSites' vi
               val _ = List.foreach (useSites, enque)

               fun doit (_,li)
                 = let
                     val uses = LabelInfo.uses' li
                     val defs = LabelInfo.defs' li
                     val live = LabelInfo.live' li
                   in
                     if Array.sub (live, index)
                        orelse
                        (Vector.sub(defs, index)
                         andalso
                         not (Vector.sub (uses, index)))
                       then ()
                       else (Array.update(live, index, true) ;
                             List.foreach (LabelInfo.preds' li, enque))
                   end
               fun loop ()
                 = case deque ()
                     of NONE => ()
                      | SOME (l,li) => (doit (l, li); loop ())
             in
               loop ()
             end)

        (* insert phi-functions *)
        (*  based on section 19.1 of Appel's "Modern Compiler Implementation in ML"
         *  (beware: Alg. 19.6 (both in the book and as corrected by the 
         *   errata) has numerous typos; and this implementation computes sets of 
         *   variables that must have phi-functions at a node, which is close to 
         *   the algorithm in the book, but the reverse of the algorithm as 
         *   corrected by the errata, which computes sets of nodes that must have 
         *   a phi-functions for a variable.)
         *)
        val _
          = Vector.foreach
            (violations, fn x =>
             let
               val {enque, deque} = mkQueue ()

               val vi = varInfo x
               val index = VarInfo.index' vi
               val defSites = VarInfo.defSites' vi
               val _ = List.foreach
                       (defSites, fn l =>
                        enque (l, labelInfo l))

               fun doit (_,li) 
                 = Vector.foreach
                   (Promise.force (LabelInfo.df' li), fn l =>
                    let
                      val li = labelInfo l
                      val live = LabelInfo.live' li
                      val phi = LabelInfo.phi li
                    in
                      if Array.sub(live, index) 
                         andalso
                         not (List.contains(!phi, x, Var.equals))
                        then (List.push(phi, x);
                              enque (l, li))
                        else ()
                    end)
               fun loop ()
                 = case deque ()
                     of NONE => ()
                      | SOME (l,li) => (doit (l, li); loop ())
             in
               loop ()
             end)

        (* finalize phi args *)
        fun visitBlock (Block.T {label, ...}) 
          = let
              val li = labelInfo label
              val phi = LabelInfo.phi li
              val phiArgs = LabelInfo.phiArgs li
            in
              phiArgs := Vector.fromList (!phi) ;
              phi := []
            end
        val _ = visitBlock entryBlock
        val _ = Vector.foreach (blocks, visitBlock)

        (* Diagnostics *)
        val _ = Control.diagnostics
                (fn display =>
                 let
                   open Layout
                 in
                   Vector.foreach
                   (violations, fn x =>
                    display (seq [Var.layout x,
                                  str " ",
                                  VarInfo.layout (varInfo x)]));
                   Vector.foreach
                   (blocks, fn Block.T {label, ...} =>
                    display (seq [Label.layout label,
                                  str " ",
                                  LabelInfo.layout (labelInfo label)]))
                 end)

        (* rewrite *)
        val blocks = ref []
        fun rewriteVar (x: Var.t) =
          let
            val vi as VarInfo.T {ty, ...} = varInfo x
            val newVar = case VarInfo.peekVar vi
              of NONE => x
               | SOME x' => x'
          in
            Operand.Var {ty=(!ty), var=newVar}
          end
        fun replaceDstOperand (st, dst as {ty=dstTy, var=dstVar})=
          let
             val tupleDst = (dstVar, dstTy)
          in
           case st of
                Statement.Bind {src, isMutable, ...} =>
                   Statement.Bind {dst=tupleDst, isMutable=isMutable, src=src}
              | Statement.Move {src, ...} =>
                   Statement.Move {dst=Operand.Var dst, src=src}
              | Statement.Object {header, size, ...} =>
                   Statement.Object {dst=tupleDst, header=header, size=size}
              | Statement.PrimApp {args, prim, ...} =>
                   Statement.PrimApp {args=args, dst=SOME tupleDst, prim=prim}
              | _ => st
          end
        fun rewriteStatement addPost st
          = let
               val st = Statement.replaceUses (st, rewriteVar)
            in
               Statement.foldDef (st, st, fn (var, ty, st) =>
                  let
                     val vi = varInfo var
                  in
                     if VarInfo.violates vi
                     then
                        let
                           val var' = Var.new var
                           val _ = addPost (fn _ => VarInfo.popVar vi) ;
                           val _ = VarInfo.pushVar (vi, var');
                        in
                           replaceDstOperand (st, {ty=ty, var=var'})
                        end
                     else st
                  end)
            end
        local
          type t = {dst: Label.t,
                    phiArgs: Var.t vector,
                    route: Label.t,
                    hash: Word.t}
          val routeTable : t HashSet.t = HashSet.new {hash = #hash}
        in
          fun route dst
            = let
                val li = labelInfo dst
                val phiArgs = LabelInfo.phiArgs' li
                val kind = LabelInfo.kind' li
              in
                if Vector.isEmpty phiArgs
                  then dst
                  else let
                         val phiArgs = Vector.map (phiArgs,
                            fn v =>
                              let
                                val vi = varInfo v
                              in
                                (valOf (VarInfo.peekVar vi), VarInfo.ty' vi)
                              end)
                         val hash = Hash.combine (Label.hash dst, Hash.vectorMap (phiArgs, Var.hash o #1))
                         val {route, ...} 
                           = HashSet.lookupOrInsert
                             (routeTable, hash, 
                              fn {dst = dst', phiArgs = phiArgs', ... } =>
                              Label.equals (dst, dst') 
                              andalso
                              Vector.equals (Vector.map(phiArgs, #1),
                                             phiArgs', Var.equals),
                              fn () =>
                              let
                                val route = Label.new dst
                                val args = Vector.map (LabelInfo.args' li,
                                  fn (x,ty) => (Var.new x, ty))
                                val args' = Vector.map
                                  (Vector.concat [args, phiArgs],
                                   fn (x, ty) => Operand.Var {ty=ty, var=x})
                                val block = Block.T
                                            {label = route,
                                             args = args,
                                             statements = Vector.new0 (),
                                             transfer = Goto {dst = dst,
                                                              args = args'},
                                             kind = kind}
                                val _ = List.push (blocks, block)
                              in
                                {dst = dst,
                                 phiArgs = Vector.map (phiArgs, #1),
                                 route = route,
                                 hash = hash}
                              end)
                       in
                         route
                       end
              end
        end
        fun rewriteTransfer (t: Transfer.t) 
          = Transfer.replaceLabels (Transfer.replaceUses (t, rewriteVar), route)
        fun visitBlock' (Block.T {label, args, statements, transfer, kind})
          = let
              val {addPost, post} = mkPost ()
              val li = labelInfo label
              fun doit x = let
                             val vi = varInfo x
                             val ty = VarInfo.ty' vi
                           in
                             if VarInfo.violates vi
                               then let
                                      val x' = Var.new x
                                    in
                                      addPost (fn _ => VarInfo.popVar vi) ;
                                      VarInfo.pushVar (vi, x') ;
                                      (x', ty)
                                    end
                               else (x, ty)
                           end
              val args = Vector.map
                         (args, fn (x, _) => doit x)
              val phiArgs = Vector.map
                            (LabelInfo.phiArgs' li, fn x => doit x)
              val args = Vector.concat [args, phiArgs]
              val statements 
                = if Vector.exists(LabelInfo.defs' li, fn b => b)
                     orelse
                     Vector.exists(LabelInfo.uses' li, fn b => b)
                    then Vector.map (statements, rewriteStatement addPost)
                    else statements
              val transfer = rewriteTransfer transfer
              val kind = if Vector.isEmpty phiArgs then kind else Kind.Jump
              val block = Block.T {label = label,
                                   args = args,
                                   statements = statements,
                                   transfer = transfer,
                                   kind = kind}
            in
              (block, post)
            end
        fun visitBlock block
          = let val (block, post) = visitBlock' block
            in List.push (blocks, block) ; post
            end
        fun rewrite ()
          = let
              local
                val (Block.T {label, args, statements, transfer, kind}, post) 
                  = visitBlock' entryBlock
                val entryBlock = Block.T {label = label,
                                          args = Vector.new0 (),
                                          statements = statements,
                                          transfer = transfer,
                                          kind = kind}
                val _ = List.push (blocks, entryBlock)
              in
                val args = args
                val post = post
              end
              val _ = Tree.traverse (Function.dominatorTree f, visitBlock)
              val _ = post ()
            in
              Function.new {args = args,
                            blocks = Vector.fromList (!blocks),
                            name = name,
                            raises = raises,
                            returns = returns,
                            start = entry}
            end
        val f = rewrite ()
      in
        f
      end
      handle NoViolations => f
    end

val traceRestoreFunction
  = Trace.trace ("RestoreR.restoreFunction",
                 Func.layout o Function.name,
                 Func.layout o Function.name)

val restoreFunction
  = fn () =>
    let
      val r = restoreFunction ()
    in
      fn f => traceRestoreFunction r f
   end

fun restore (Program.T {functions, handlesSignals, main, objectTypes})
  = let
      val r = restoreFunction ()
    in
      Program.T {handlesSignals = handlesSignals,
                 functions = List.map (functions, r),
                 main = main,
                 objectTypes = objectTypes}
    end
end