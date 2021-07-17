(* Copyright (C) 2017,2019,2021 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Simplify2 (S: SIMPLIFY2_STRUCTS): SIMPLIFY2 = 
struct

open S

structure DeepFlatten = DeepFlatten (S)
structure Profile2 = Profile2 (S)
structure RefFlatten = RefFlatten (S)
structure RemoveUnused2 = RemoveUnused2 (S)
structure Zone = Zone (S)

type pass = {name: string,
             doit: Program.t -> Program.t,
             execute: bool}

val ssa2PassesDefault = 
   {name = "deepFlatten", doit = DeepFlatten.transform2, execute = true} ::
   {name = "refFlatten", doit = RefFlatten.transform2, execute = true} ::
   {name = "removeUnused5", doit = RemoveUnused2.transform2, execute = true} ::
   {name = "zone", doit = Zone.transform2, execute = false} ::
   nil

val ssa2PassesMinimal =
   nil

val ssa2Passes : pass list ref = ref ssa2PassesDefault

local
   type passGen = string -> pass option

   fun mkSimplePassGen (name, doit): passGen =
      let val count = Counter.generator 1
      in fn s => if s = name
                    then SOME {name = concat [name, "#",
                                              Int.toString (count ())],
                               doit = doit,
                               execute = true}
                    else NONE
      end


   val passGens = 
      List.map([("deepFlatten", DeepFlatten.transform2),
                ("refFlatten", RefFlatten.transform2),
                ("removeUnused", RemoveUnused2.transform2),
                ("zone", Zone.transform2),
                ("ssa2AddProfile", Profile2.addProfile),
                ("ssa2DropProfile", Profile2.dropProfile),
                ("ssa2EliminateDeadBlocks", S.eliminateDeadBlocks),
                ("ssa2OrderFunctions", S.orderFunctions),
                ("ssa2ReverseFunctions", S.reverseFunctions),
                ("ssa2Shrink", S.shrink)],
               mkSimplePassGen)
in
   fun ssa2PassesSetCustom s =
      Exn.withEscape
      (fn esc =>
       (let val ss = String.split (s, #":")
        in 
           ssa2Passes := 
           List.map(ss, fn s =>
                    case (List.peekMap (passGens, fn gen => gen s)) of
                       NONE => esc (Result.No s)
                     | SOME pass => pass)
           ; Result.Yes ()
        end))
end

fun ssa2PassesSet s =
   case s of
      "default" => (ssa2Passes := ssa2PassesDefault
                    ; Result.Yes ())
    | "minimal" => (ssa2Passes := ssa2PassesMinimal
                    ; Result.Yes ())
    | _ => ssa2PassesSetCustom s
val _ = Control.OptimizationPasses.register {il = "ssa2", set = ssa2PassesSet}

fun simplify p =
   let
      val ssa2Passes = AppendList.fromList (!ssa2Passes)
      val ssa2Passes =
         if !Control.profile <> Control.ProfileNone
            andalso !Control.profileIL = Control.ProfileSSA2
            then AppendList.snoc (ssa2Passes,
                                  {name = "ssa2AddProfile",
                                   doit = Profile2.addProfile,
                                   execute = true})
            else ssa2Passes
      val ssa2Passes =
         AppendList.snoc (ssa2Passes, {name = "ssa2OrderFunctions",
                                       doit = S.orderFunctions,
                                       execute = true})
      val ssa2Passes = AppendList.toList ssa2Passes
      val p =
         Control.simplifyPasses
         {arg = p,
          passes = ssa2Passes,
          stats = Program.layoutStats,
          toFile = Program.toFile,
          typeCheck = typeCheck}
   in
      p
   end
end
