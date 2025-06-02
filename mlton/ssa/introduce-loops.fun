(* Copyright (C) 2025 Matthew Fluet.
 * Copyright (C) 1999-2005, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Change any toplevel function that only calls itself in tail position
 * into one with a local loop and no self calls.
 *)
functor IntroduceLoops (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM = 
struct

open S
datatype z = datatype Exp.t
datatype z = datatype Transfer.t

structure LabelInfo =
   struct
      datatype t =
         Block
       | RaiseEta
       | ReturnEta of {profileStmts: Statement.t vector}
   end

fun transform (Program.T {datatypes, globals, functions, main}) =
   let
      val shrink = shrinkFunction {globals = globals}

      val functions =
         List.revMap
         (functions, fn f =>
          let
             val {args, blocks, mayInline, name, raises, returns, start} =
                Function.dest f
             val {get = labelInfo, set = setLabelInfo, ...} =
                Property.getSetOnce
                (Label.plist, Property.initConst LabelInfo.Block)
             val _ =
                Vector.foreach
                (blocks, fn Block.T {label, args = formals, statements, transfer} =>
                 let
                    fun rr (actuals, make) =
                       if Vector.length formals = Vector.length actuals
                          andalso
                          Vector.forall2
                          (formals, actuals, fn ((formal, _), actual) =>
                           Var.equals (formal, actual))
                          andalso
                          Vector.forall (statements, Statement.isProfile)
                          then setLabelInfo (label, make {profileStmts = statements})
                       else ()
                 in
                    case transfer of
                       Transfer.Raise actuals =>
                          rr (actuals, fn _ => LabelInfo.RaiseEta)
                     | Transfer.Return actuals =>
                          rr (actuals, LabelInfo.ReturnEta)
                     | _ => ()
                 end)

             fun returnIsTail return =
                let
                   fun maybe profileStmts =
                      if Vector.isEmpty profileStmts
                         orelse !Control.profileIntroLoopsOpt
                         then SOME {profileStmts = profileStmts}
                      else NONE
                in
                   case return of
                      Return.Dead => NONE
                    | Return.NonTail {cont, handler} =>
                         (case labelInfo cont of
                             LabelInfo.ReturnEta {profileStmts} =>
                                (case handler of
                                    Handler.Caller => maybe profileStmts
                                  | Handler.Dead => maybe profileStmts
                                  | Handler.Handle handler =>
                                       (case labelInfo handler of
                                           LabelInfo.RaiseEta =>
                                              maybe profileStmts
                                         | _ => NONE))
                           | _ => NONE)
                    | Return.Tail => SOME {profileStmts = Vector.new0 ()}
                end

             val tailCallsItself = ref false
             val _ =
                Vector.foreach
                (blocks, fn Block.T {transfer, ...} =>
                 case transfer of
                    Call {func, return, ...} =>
                       if Func.equals (name, func)
                          andalso Option.isSome (returnIsTail return)
                          then tailCallsItself := true
                       else ()
                  | _ => ())
             val (args, start, blocks) =
                if !tailCallsItself
                   then
                      let
                         val _ = Control.diagnostics
                            (fn display =>
                             let open Layout
                             in
                                display (Func.layout name)
                             end)
                         val newArgs =
                            Vector.map (args, fn (x, t) => (Var.new x, t))
                         val loopName = Label.newString "loop"
                         val loopSName = Label.newString "loopS"
                         val blocks = 
                            Vector.toListMap
                            (blocks,
                             fn Block.T {label, args, statements, transfer} =>
                             let
                                val (statements, transfer) =
                                   case transfer of
                                      Call {func, args, return} =>
                                         (case (Func.equals (name, func),
                                                returnIsTail return) of
                                             (true, SOME {profileStmts}) =>
                                                (if Vector.isEmpty profileStmts
                                                    then statements
                                                 else Vector.concat [statements, profileStmts],
                                                 Goto {dst = loopName,
                                                       args = args})
                                           | _ => (statements, transfer))
                                    | _ => (statements, transfer)
                             in
                                Block.T {label = label,
                                         args = args,
                                         statements = statements,
                                         transfer = transfer}
                             end)
                         val blocks = 
                            Vector.fromList
                            (Block.T 
                             {label = loopSName,
                              args = Vector.new0 (),
                              statements = Vector.new0 (),
                              transfer = Goto {dst = loopName,
                                               args = Vector.map (newArgs, #1)}} ::
                             Block.T 
                             {label = loopName,
                              args = args,
                              statements = Vector.new0 (),
                              transfer = Goto {dst = start,
                                               args = Vector.new0 ()}} ::
                             blocks)
                      in
                         (newArgs,
                          loopSName,
                          blocks)
                      end
                else (args, start, blocks)
          in
             (shrink o Function.new)
             {args = args,
              blocks = blocks,
              mayInline = mayInline,
              name = name,
              raises = raises,
              returns = returns,
              start = start}
          end)
   in
      Program.T {datatypes = datatypes,
                 globals = globals,
                 functions = functions,
                 main = main}
   end

end
