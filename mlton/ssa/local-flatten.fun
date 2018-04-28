(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor LocalFlatten (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM = 
struct

open S
open Exp Transfer

(* Flatten a jump arg as long as it is only flows to selects and there is 
 * some tuple constructed in this function that flows to it.
 *)

structure ArgInfo =
   struct
      datatype t = T of {fromTuple: bool ref,
                         fromForce: t list ref,
                         toSelect: bool ref,
                         toForce: t list ref}

      fun isFlat (T {fromTuple, toSelect, ...}) =
         !fromTuple andalso !toSelect

      val isTupled = not o isFlat

      fun layout (i: t): Layout.t =
         Layout.str (if isFlat i then "flat" else "tupled")

      fun new () = T {fromTuple = ref false,
                      fromForce = ref [],
                      toSelect = ref true,
                      toForce = ref []}

      fun tuple (T {fromTuple = f, fromForce, ...}) =
         if !f
            then ()
         else (f := true; List.foreach (!fromForce, tuple))

      fun nonSelect (T {toSelect = t, toForce, ...}) =
         if !t
            then (t := false; List.foreach (!toForce, nonSelect))
         else ()

      val op <= =
         fn (lhs as T {fromTuple = f, fromForce, ...},
             rhs as T {toSelect = t, toForce, ...}) =>
         let
            val _ =
               if !f
                  then tuple rhs
               else List.push (fromForce, rhs)
            val _ =
               if !t
                  then List.push (toForce, lhs)
               else nonSelect lhs
         in
            ()
         end
   end

structure VarInfo =
   struct
      datatype t =
         None
       | Arg of ArgInfo.t
       | Tuple
   end

fun transform (Program.T {globals, datatypes, functions, main}) =
   let
      val {get = varInfo: Var.t -> VarInfo.t,
           set = setVarInfo, ...} =
         Property.getSetOnce (Var.plist, Property.initConst VarInfo.None)
      type argsInfo = (ArgInfo.t * Type.t) option vector
      val {get = labelArgs: Label.t -> argsInfo,
           set = setLabelArgs, ...} =
         Property.getSetOnce (Label.plist,
                              Property.initRaise ("args", Label.layout))
      val shrink = shrinkFunction {globals = globals}
      val functions =
         List.revMap
         (functions, fn f =>
          let
             val {args, blocks, mayInline, name, raises, returns, start} =
                Function.dest f
             val _ =
                Vector.foreach
                (blocks, fn Block.T {label, args, ...} =>
                 setLabelArgs (label,
                               Vector.map
                               (args, fn (x, t) =>
                                if Type.isTuple t
                                   then
                                      let
                                         val i = ArgInfo.new ()
                                         val _ = setVarInfo (x, VarInfo.Arg i)
                                      in
                                         SOME (i, t)
                                      end
                                else NONE)))

             fun force (x: Var.t): unit =
                case varInfo x of
                   VarInfo.Arg i => ArgInfo.nonSelect i
                 | _ => ()
             fun forces (xs: Var.t vector): unit =
                Vector.foreach (xs, force)
             fun forceArgs (l: Label.t): unit =
                Vector.foreach (labelArgs l,
                                fn NONE => ()
                                 | SOME (i, _) => ArgInfo.nonSelect i)

             fun visit (Block.T {statements, transfer, ...}): unit -> unit =
                let
                   val _ = 
                      Vector.foreach
                      (statements, fn Statement.T {var, exp, ...} =>
                       case exp of
                          ConApp {args, ...} => forces args
                        | PrimApp {args, ...} => forces args
                        | Tuple args => (setVarInfo (valOf var, VarInfo.Tuple)
                                         ; forces args)
                        | Var x => force x
                        | _ => ())
                   val _ =
                      case transfer of
                         Arith {args, overflow, success, ...} =>
                            (forces args
                             ; forceArgs overflow
                             ; forceArgs success) 
                       | Bug => ()
                       | Call {args, return, ...} =>
                            (forces args
                             ; Return.foreachLabel (return, forceArgs))
                       | Case {cases, default, ...} =>
                            (Cases.foreach (cases, forceArgs)
                             ; Option.app (default, forceArgs))
                       | Goto {dst, args} =>
                            Vector.foreach2
                            (args, labelArgs dst,
                             fn (_, NONE) => ()
                              | (x, SOME (i, _)) =>
                                   (case varInfo x of
                                       VarInfo.Arg i' => ArgInfo.<= (i', i)
                                     | VarInfo.None => ()
                                     | VarInfo.Tuple => ArgInfo.tuple i))
                       | Raise xs => forces xs
                       | Return xs => forces xs
                       | Runtime {args, return, ...} =>
                            (forces args
                             ; forceArgs return)
                in
                   fn () => ()
                end
             val _ = Function.dfs (f, visit)
             val _ = 
                Control.diagnostics
                (fn display =>
                 let
                    fun doit x = 
                       case varInfo x of
                          VarInfo.Arg i => display (let open Layout
                                                    in seq [Var.layout x,
                                                            str " ",
                                                            ArgInfo.layout i]
                                                    end)
                        | _ => ()
                 in
                    Vector.foreach
                    (blocks, fn Block.T {args, statements, ...} =>
                     (Vector.foreach(args, doit o #1);
                      Vector.foreach(statements, fn Statement.T {var, ...} =>
                                     Option.app(var, doit))))
                 end)

             fun makeTuple (formals: (Var.t * Type.t) vector,
                            reps: argsInfo)
               : (Var.t * Type.t) vector * Statement.t list =
               let
                  val (argss, stmts) =
                     Vector.map2AndFold
                     (formals, reps, [], fn ((x, ty), rep, stmts) =>
                      case rep of
                         NONE => (Vector.new1 (x, ty), stmts)
                       | SOME (i, _) =>
                            if ArgInfo.isTupled i
                               then (Vector.new1 (x, ty), stmts)
                            else
                               let
                                  val vars = Vector.map
                                             (Type.deTuple ty, fn ty =>
                                              (Var.newNoname (), ty))
                               in
                                  (vars,
                                   Statement.T 
                                   {var = SOME x,
                                    ty = ty,
                                    exp = Tuple (Vector.map (vars, #1))}
                                   :: stmts)
                               end)
               in (Vector.concatV argss, stmts)
               end
             fun makeSelects (args: Var.t vector,
                              formals: argsInfo)
               : Var.t vector * Statement.t list =
               let
                  val (argss, stmts) =
                     Vector.map2AndFold
                     (args, formals, [], fn (x, formal, stmts) =>
                      case formal of
                         NONE => (Vector.new1 x, stmts)
                       | SOME (i, t) =>
                            if ArgInfo.isTupled i
                               then (Vector.new1 x, stmts)
                            else 
                               let
                                  val (vars, stmts) =
                                     Vector.foldi
                                     (Type.deTuple t, ([], stmts),
                                      fn (i, ty, (vars, stmts)) =>
                                      let val var = Var.newNoname ()
                                      in (var :: vars,
                                          Statement.T
                                          {var = SOME var,
                                           ty = ty,
                                           exp = Select {tuple = x,
                                                         offset = i}}
                                          :: stmts)
                                      end)
                               in (Vector.fromListRev vars, stmts)
                               end)
               in (Vector.concatV argss, stmts)
               end
             fun anyFlat (v: argsInfo): bool =
                Vector.exists (v,
                               fn NONE => false
                                | SOME (i, _) => ArgInfo.isFlat i)
             val blocks =
                Vector.map
                (blocks, fn Block.T {label, args, statements, transfer} =>
                 let
                    val (args, pre) =
                       let
                          val formals = labelArgs label
                       in
                          if anyFlat formals
                             then makeTuple (args, formals)
                             else (args, [])
                       end
                    val (post, transfer) =
                       case transfer of
                          Goto {dst, args} =>
                             let
                                val formals = labelArgs dst
                             in
                                if anyFlat formals
                                   then 
                                      let
                                         val (args, stmts) =
                                            makeSelects (args, formals)
                                      in
                                         (stmts, Goto {dst = dst, args = args})
                                      end
                                else ([], transfer)
                             end
                        | _ => ([], transfer)
                    val statements =
                      Vector.concatV
                      (Vector.new3 (Vector.fromList pre,
                                    statements,
                                    Vector.fromList post))
                 in
                    Block.T {label = label,
                             args = args,
                             statements = statements,
                             transfer = transfer}
                 end)
          in
             shrink (Function.new {args = args,
                                   blocks = blocks,
                                   mayInline = mayInline,
                                   name = name,
                                   raises = raises,
                                   returns = returns,
                                   start = start})
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
