(* Copyright (C) 2009,2016-2017,2019 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor RssaShrink (S: RSSA_SHRINK_STRUCTS): RSSA_SHRINK =
struct

open S

local
   open Prim
in
   structure ApplyArg = ApplyArg
   structure ApplyResult = ApplyResult
end

fun shrinkFunction (f: Function.t): Function.t =
   let
      val {args, blocks, name, raises, returns, start} = Function.dest f
      val {get = labelInfo, rem, set = setLabelInfo, ...} =
         Property.getSetOnce
         (Label.plist, Property.initRaise ("info", Label.layout))
      val () =
         Vector.foreach
         (blocks, fn block as Block.T {label, ...} =>
          setLabelInfo (label, {block = block,
                                inline = ref false,
                                replace = ref NONE,
                                occurrences = ref 0}))
      fun visitLabel l = Int.inc (#occurrences (labelInfo l))
      val () = visitLabel start
      val () =
         Vector.foreach (blocks, fn Block.T {transfer, ...} =>
                         Transfer.foreachLabel (transfer, visitLabel))
      datatype z = datatype Statement.t
      datatype z = datatype Transfer.t
      val () =
         Vector.foreach
         (blocks, fn Block.T {args, kind, label, statements, transfer} =>
          case transfer of
             Goto {args=dstArgs, dst, ...} =>
                let
                   val {replace, ...} = labelInfo label
                   val {inline, occurrences, ...} = labelInfo dst
                in
                   if 1 = !occurrences
                      then inline := true
                   else
                      case (Vector.isEmpty statements, kind) of
                           (true, Kind.Jump) =>
                              if Vector.length args = Vector.length dstArgs
                                 andalso Vector.forall2 (args, dstArgs,
                                    fn ((v, _), oper) =>
                                       case oper of
                                            Operand.Var {var=v', ...} =>
                                                Var.equals (v, v')
                                          | _ => false)
                              then replace := SOME dst
                              else ()
                        | _ => ()

                end
           | _ => ())
      fun getReplace l =
         case (! o #replace o labelInfo) l of
              SOME l' => getReplace l'
            | NONE => l
      fun expand (ss: Statement.t vector list, t: Transfer.t)
         : Statement.t vector * Transfer.t =
         let
            fun replaceTransfer t =
               Transfer.replaceLabels (t, getReplace)
            fun done () = (Vector.concat (rev ss), replaceTransfer t)
         in
            case t of
               Goto {args, dst} =>
                  let
                     val {block, inline, ...} = labelInfo dst
                  in
                     if not (!inline)
                        then done ()
                     else
                        let
                           val Block.T {args = formals, statements,
                                        transfer, ...} =
                              block
                           val binds =
                              Vector.map2
                              (formals, args, fn (dst, src) =>
                               Bind {dst = dst,
                                     pinned = false,
                                     src = src})
                        in
                           expand (statements :: binds :: ss, replaceTransfer transfer)
                        end
                  end
             | _ => done ()
         end
      val blocks =
         Vector.fromList
         (Vector.fold
          (blocks, [],
           fn (Block.T {args, kind, label, statements, transfer}, ac) =>
           let
              val {inline, occurrences, replace, ...} = labelInfo label
           in
              if !inline orelse 0 = !occurrences orelse isSome (!replace)
                 then ac
              else
                 let
                    val (statements, transfer) =
                       expand ([statements], transfer)
                 in
                    Block.T {args = args,
                             kind = kind,
                             label = label,
                             statements = statements,
                             transfer = transfer} :: ac
                 end
           end))
      val start = getReplace start
      val () = Vector.foreach (blocks, rem o Block.label)
   in
      Function.new {args = args,
                    blocks = blocks,
                    name = name,
                    raises = raises,
                    returns = returns,
                    start = start}
   end

fun copyProp (Program.T {functions, handlesSignals, main, objectTypes, profileInfo, ...}): Program.t =
   let
      val tracePrimApply =
         Trace.trace3
         ("RssaShrink.copyProp.primApply",
          Prim.layout,
          List.layout (ApplyArg.layout (Var.layout o #var)),
          Layout.ignore,
          ApplyResult.layout (Var.layout o #var))
      val {get = replaceVar: Var.t -> Operand.t,
           set = setReplaceVar, ...} =
         Property.getSetOnce
         (Var.plist, Property.initRaise ("RssaShrink.copyProp.replaceVar", Var.layout))
      fun dontReplace (x: Var.t, t: Type.t): unit =
         setReplaceVar (x, Operand.Var {var = x, ty = t})
      val setReplaceVar = fn (x: Var.t, t: Type.t, z: Operand.t) =>
         let
            val z =
               if Type.equals (Operand.ty z, t)
                  then z
                  else Operand.Cast (z, t)
         in
            setReplaceVar (x, z)
         end
      fun loopStatement (s: Statement.t): Statement.t option =
         let
            datatype z = datatype Statement.t
            val s = Statement.replaceUses (s, replaceVar)
            fun keep () =
               (Statement.foreachDef (s, dontReplace)
                ; SOME s)
         in
            case s of
               Bind {dst = (dst, dstTy), pinned, src} =>
                  if pinned
                     then keep ()
                  else
                     let
                        datatype z = datatype Operand.t
                        fun getSrc src =
                           case src of
                              Cast (src, _) => getSrc src
                            | Const _ => SOME src
                            | Var _ => SOME src
                            | _ => NONE
                     in
                        case getSrc src of
                           NONE => keep ()
                         | SOME src =>
                              (setReplaceVar (dst, dstTy, src)
                               ; NONE)
                     end
             | PrimApp {args, dst, prim} =>
                  let
                     fun replace (z: Operand.t): Statement.t option =
                        (Option.app (dst, fn (x, t) =>
                                     setReplaceVar (x, t, z))
                         ; NONE)
                     datatype z = datatype Operand.t
                     fun getArg arg =
                        case arg of
                           Cast (arg, _) => getArg arg
                         | Const c => SOME (ApplyArg.Const c)
                         | Var x => SOME (ApplyArg.Var x)
                         | _ => NONE
                     val applyArgs = Vector.toListKeepAllMap (args, getArg)
                     datatype z = datatype ApplyResult.t
                  in
                     if Vector.length args <> List.length applyArgs
                        then keep ()
                     else
                        case (tracePrimApply
                              Prim.apply
                              (prim, applyArgs,
                               fn ({var = x, ...}, {var = y, ...}) =>
                               Var.equals (x, y))) of
                           Apply (prim, args) =>
                              let
                                 val args =
                                    Vector.fromListMap (args, Operand.Var)
                                 val () = Option.app (dst, dontReplace)
                              in
                                 SOME (PrimApp {args = args,
                                                dst = dst,
                                                prim = prim})
                              end
                         | Bool b => replace (Operand.bool b)
                         | Const c => replace (Operand.Const c)
                         | Unknown => keep ()
                         | Var x => replace (Operand.Var x)
                  end
          | _ => keep ()
         end
      fun loopTransfer t =
         Transfer.replaceUses (t, replaceVar)
      fun loopFormals args = Vector.foreach (args, dontReplace)
      fun loopFunction (f: Function.t): Function.t =
         let
            val {args, name, raises, returns, start, ...} =
               Function.dest f
            val () = loopFormals args
            val blocks = ref []
            val () =
               Function.dfs
               (f, fn Block.T {args, kind, label, statements, transfer} =>
                let
                   val () = loopFormals args
                   val statements =
                      Vector.keepAllMap (statements, loopStatement)
                   val transfer = loopTransfer transfer
                   val () =
                      List.push
                      (blocks, Block.T {args = args,
                                        kind = kind,
                                        label = label,
                                        statements = statements,
                                        transfer = transfer})
                in
                   fn () => ()
                end)
            val blocks = Vector.fromList (!blocks)
         in
            Function.new {args = args,
                          blocks = blocks,
                          name = name,
                          raises = raises,
                          returns = returns,
                          start = start}
         end
      (* Must process main first, because it defines globals that are
       * used in other functions.
       *)
      val main = loopFunction main
      val functions = List.revMap (functions, loopFunction)
   in
      Program.T {functions = functions,
                 handlesSignals = handlesSignals,
                 main = main,
                 objectTypes = objectTypes,
                 profileInfo = profileInfo}
   end

fun shrink (Program.T {functions, handlesSignals, main, objectTypes, profileInfo}): Program.t =
   let
      val p =
         Program.T {functions = List.revMap (functions, shrinkFunction),
            handlesSignals = handlesSignals,
                    main = shrinkFunction main,
                    objectTypes = objectTypes,
                    profileInfo = profileInfo}
      val p = copyProp p
      val () = Program.clear p
   in
      p
   end

end
