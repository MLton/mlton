(* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Simplify2 (S: SIMPLIFY2_STRUCTS): SIMPLIFY2 = 
struct

open S

(* structure CommonArg = CommonArg (S) *)
(* structure CommonBlock = CommonBlock (S) *)
(* structure CommonSubexp = CommonSubexp (S) *)
(* structure ConstantPropagation = ConstantPropagation (S) *)
(* structure Contify = Contify (S) *)
structure DeepFlatten = DeepFlatten (S)
(* structure Flatten = Flatten (S) *)
(* structure Inline = Inline (S) *)
(* structure IntroduceLoops = IntroduceLoops (S) *)
(* structure KnownCase = KnownCase (S) *)
(* structure LocalFlatten = LocalFlatten (S) *)
(* structure LocalRef = LocalRef (S) *)
(* structure LoopInvariant = LoopInvariant (S) *)
(* structure PolyEqual = PolyEqual (S) *)
structure Profile2 = Profile2 (S)
(* structure Redundant = Redundant (S) *)
(* structure RedundantTests = RedundantTests (S) *)
structure RefFlatten = RefFlatten (S)
structure RemoveUnused2 = RemoveUnused2 (S)
(* structure SimplifyTypes = SimplifyTypes (S) *)
(* structure Useless = Useless (S) *)
structure Zone = Zone (S)

type pass = {name: string,
             doit: Program.t -> Program.t,
             execute: bool}

val ssa2PassesDefault = 
   {name = "deepFlatten", doit = DeepFlatten.transform2, execute = true} ::
   {name = "refFlatten", doit = RefFlatten.transform2, execute = true} ::
   {name = "removeUnused5", doit = RemoveUnused2.transform2, execute = true} ::
   {name = "zone", doit = Zone.transform2, execute = true} ::
   nil

val ssa2PassesMinimal =
   nil

val ssa2Passes : pass list ref = ref ssa2PassesDefault

local
   type passGen = string -> pass option

   fun mkSimplePassGen (name, doit, execute): passGen =
      let val count = Counter.new 1
      in fn s => if s = name
                    then SOME {name = concat [name, "#",
                                              Int.toString (Counter.next count)],
                               doit = doit,
                               execute = execute}
                    else NONE
      end


   val passGens = 
      List.map([("addProfile", Profile2.addProfile, true),
                ("deepFlatten", DeepFlatten.transform2, true),
                ("dropProfile", Profile2.dropProfile, true),
                ("refFlatten", RefFlatten.transform2, true),
                ("removeUnused", RemoveUnused2.transform2, true),
                ("zone", Zone.transform2, true),
                ("eliminateDeadBlocks",S.eliminateDeadBlocks, true),
                ("orderFunctions",S.orderFunctions, true),
                ("reverseFunctions",S.reverseFunctions, true),
                ("shrink", S.shrink, true)],
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

val ssa2PassesString = ref "default"
val ssa2PassesGet = fn () => !ssa2PassesString
val ssa2PassesSet = fn s =>
   let
      val _ = ssa2PassesString := s
   in
      case s of
         "default" => (ssa2Passes := ssa2PassesDefault
                       ; Result.Yes ())
       | "minimal" => (ssa2Passes := ssa2PassesMinimal
                       ; Result.Yes ())
       | _ => ssa2PassesSetCustom s
   end
val _ = List.push (Control.optimizationPasses,
                   {il = "ssa2", get = ssa2PassesGet, set = ssa2PassesSet})

fun pass ({name, doit, midfix}, p) =
   let
      val _ =
         let open Control
         in maybeSaveToFile
            ({name = name, 
              suffix = midfix ^ "pre.ssa2"},
             Control.No, p, Control.Layouts Program.layouts)
         end
      val p =
         Control.passTypeCheck
         {display = Control.Layouts Program.layouts,
          name = name,
          stats = Program.layoutStats,
          style = Control.No,
          suffix = midfix ^ "post.ssa2",
          thunk = fn () => doit p,
          typeCheck = typeCheck}
   in
      p
   end 
fun maybePass ({name, doit, execute, midfix}, p) =
   if List.foldr (!Control.executePasses, execute, fn ((re, new), old) =>
                  if Regexp.Compiled.matchesAll (re, name)
                     then new
                     else old)
      then pass ({name = name, doit = doit, midfix = midfix}, p)
      else p

fun simplify p =
   let
      fun simplify' n p =
         let
            val midfix = if n = 0
                            then ""
                         else concat [Int.toString n,"."]
         in
            if n = !Control.loopPasses
               then p
            else simplify' 
                 (n + 1)
                 (List.fold
                  (!ssa2Passes, p, fn ({name, doit, execute}, p) =>
                   maybePass ({name = name, doit = doit, execute = execute, midfix = midfix}, p)))
         end
      val p = simplify' 0 p
   in
      p
   end

val simplify = fn p => let
                         (* Always want to type check the initial and final SSA 
                          * programs, even if type checking is turned off, just
                          * to catch bugs.
                          *)
                         val _ = typeCheck p
                         val p = simplify p
                         val p =
                            if !Control.profile <> Control.ProfileNone
                               andalso !Control.profileIL = Control.ProfileSSA2
                               then pass ({name = "addProfile2",
                                           doit = Profile2.addProfile,
                                           midfix = ""}, p)
                            else p
                         val p = maybePass ({name = "orderFunctions2",
                                             doit = S.orderFunctions,
                                             execute = true,
                                             midfix = ""}, p)
                         val _ = typeCheck p
                       in
                         p
                       end
end
