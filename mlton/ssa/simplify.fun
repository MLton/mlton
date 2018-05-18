(* Copyright (C) 2009,2017 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Simplify (S: SIMPLIFY_STRUCTS): SIMPLIFY = 
struct

open S

structure CommonArg = CommonArg (S)
structure CommonBlock = CommonBlock (S)
structure CommonSubexp = CommonSubexp (S)
structure CombineConversions = CombineConversions (S)
structure ConstantPropagation = ConstantPropagation (S)
structure Contify = Contify (S)
structure Flatten = Flatten (S)
structure Inline = Inline (S)
structure IntroduceLoops = IntroduceLoops (S)
structure KnownCase = KnownCase (S)
structure LocalFlatten = LocalFlatten (S)
structure LocalRef = LocalRef (S)
structure LoopInvariant = LoopInvariant (S)
structure LoopUnroll = LoopUnroll (S)
structure LoopUnswitch = LoopUnswitch (S)
structure PolyEqual = PolyEqual (S)
structure PolyHash = PolyHash (S)
structure Profile = Profile (S)
structure Redundant = Redundant (S)
structure RedundantTests = RedundantTests (S)
structure RemoveUnused = RemoveUnused (S)
structure ShareZeroVec = ShareZeroVec (S)
structure SimplifyTypes = SimplifyTypes (S)
structure Useless = Useless (S)

type pass = {name: string,
             doit: Program.t -> Program.t,
             execute: bool}

val ssaPassesDefault =
   {name = "removeUnused1", doit = RemoveUnused.transform, execute = true} ::
   {name = "introduceLoops1", doit = IntroduceLoops.transform, execute = true} ::
   {name = "loopInvariant1", doit = LoopInvariant.transform, execute = true} ::
   {name = "inlineLeaf1", doit = fn p => 
    Inline.inlineLeaf (p, !Control.inlineLeafA), execute = true} ::
   {name = "inlineLeaf2", doit = fn p => 
    Inline.inlineLeaf (p, !Control.inlineLeafB), execute = true} ::
   {name = "contify1", doit = Contify.transform, execute = true} ::
   {name = "localFlatten1", doit = LocalFlatten.transform, execute = true} ::
   {name = "constantPropagation", doit = ConstantPropagation.transform, execute = true} ::
   (* useless should run 
    *   - after constant propagation because constant propagation makes
    *     slots of tuples that are constant useless
    *)
   {name = "useless", doit = Useless.transform, execute = true} ::
   (* loopUnroll should run
    *   - after constants have been globalized
    *)
   {name = "loopUnroll1", doit = LoopUnroll.transform, execute = false} ::
   {name = "removeUnused2", doit = RemoveUnused.transform, execute = true} ::
   {name = "simplifyTypes", doit = SimplifyTypes.transform, execute = true} ::
   (* polyEqual should run
    *   - after types are simplified so that many equals are turned into eqs
    *   - before inlining so that equality functions can be inlined
    *)
   {name = "polyEqual", doit = PolyEqual.transform, execute = true} ::
   (* polyHash should run
    *   - after types are simplified
    *   - before inlining so that hash functions can be inlined
    *)
   {name = "polyHash", doit = PolyHash.transform, execute = true} ::
   {name = "introduceLoops2", doit = IntroduceLoops.transform, execute = true} ::
   {name = "loopInvariant2", doit = LoopInvariant.transform, execute = true} ::
   (* loopUnswitch should run
    *   - after loop invariant code motion so invariant conditions are obvious
    *   - before a knownCase pass to cleanup after unswitching
    *)
   {name = "loopUnswitch1", doit = LoopUnswitch.transform, execute = false} ::
   {name = "knownCase1", doit = KnownCase.transform, execute = false} ::
   {name = "contify2", doit = Contify.transform, execute = true} ::
   {name = "inlineNonRecursive", doit = fn p =>
    Inline.inlineNonRecursive (p, !Control.inlineNonRec), execute = true} ::
   {name = "localFlatten2", doit = LocalFlatten.transform, execute = true} ::
   {name = "removeUnused3", doit = RemoveUnused.transform, execute = true} ::
   {name = "contify3", doit = Contify.transform, execute = true} ::
   {name = "introduceLoops3", doit = IntroduceLoops.transform, execute = true} ::
   {name = "loopInvariant3", doit = LoopInvariant.transform, execute = true} ::
   {name = "localRef", doit = LocalRef.transform, execute = true} ::
   {name = "flatten", doit = Flatten.transform, execute = true} ::
   {name = "localFlatten3", doit = LocalFlatten.transform, execute = true} ::
   {name = "combineConversions", doit = CombineConversions.transform, execute = true} ::
   {name = "commonArg", doit = CommonArg.transform, execute = true} ::
   {name = "commonSubexp1", doit = CommonSubexp.transform, execute = true} ::
   {name = "commonBlock", doit = CommonBlock.transform, execute = true} ::
   (* shareZeroVec should run
    *  - after useless because sharing of zero-length array inhibits
    *    changing type of flow-disjoint vector data
    *  - after simplifyTypes because it may make previously distinct
    *    types equal and allow more sharing of zero-length arrays
    *  - after inlining because shareZeroVec (slightly) increases size
    *  - before redundantTests because shareZeroVec introduces
    *    comparisons with zero
    *)
   {name = "shareZeroVec", doit = ShareZeroVec.transform, execute = true} ::
   {name = "redundantTests", doit = RedundantTests.transform, execute = true} ::
   {name = "redundant", doit = Redundant.transform, execute = true} ::
   {name = "loopUnswitch2", doit = LoopUnswitch.transform, execute = false} ::
   {name = "knownCase2", doit = KnownCase.transform, execute = true} ::
   {name = "loopUnroll2", doit = LoopUnroll.transform, execute = false} ::
   {name = "commonSubexp2", doit = CommonSubexp.transform, execute = false} ::
   {name = "removeUnused4", doit = RemoveUnused.transform, execute = true} ::
   nil

val ssaPassesMinimal =
   (* polyEqual cannot be omitted.  It implements MLton_equal. *)
   {name = "polyEqual", doit = PolyEqual.transform, execute = true} ::
   (* polyHash cannot be omitted.  It implements MLton_hash. *)
   {name = "polyHash", doit = PolyHash.transform, execute = true} ::
   nil

val ssaPasses : pass list ref = ref ssaPassesDefault

local
   type passGen = string -> pass option

   fun mkSimplePassGen (name, doit): passGen =
      let val count = Counter.new 1
      in fn s => if s = name
                    then SOME {name = concat [name, "#",
                                              Int.toString (Counter.next count)],
                               doit = doit,
                               execute = true}
                    else NONE
      end

   val inlinePassGen =
      let
         datatype t = Bool of bool | IntOpt of int option
         val count = Counter.new 1
         fun nums s =
            Exn.withEscape
            (fn escape =>
             if s = ""
                then SOME []
             else let
                     val l = String.length s
                  in
                     if String.sub (s, 0) = #"(" 
                        andalso String.sub (s, l - 1)= #")"
                        then let
                                val s = String.substring2 (s, {start = 1, finish = l - 1})
                                fun doit s =
                                   if s = "true"
                                      then Bool true
                                   else if s = "false"
                                      then Bool false
                                   else if s = "inf"
                                      then IntOpt NONE
                                   else if String.forall (s, Char.isDigit)
                                      then IntOpt (Int.fromString s)
                                   else escape NONE
                             in
                                case List.map (String.split (s, #","), doit) of
                                   l as _::_ => SOME l
                                 | _ => NONE
                             end
                    else NONE
                 end)
      in
         fn s =>
         if String.hasPrefix (s, {prefix = "inlineNonRecursive"})
            then let
                    fun mk (product, small) =
                       SOME {name = concat ["inlineNonRecursive(", 
                                            Int.toString product, ",",
                                            Int.toString small, ")#",
                                            Int.toString (Counter.next count)],
                             doit = (fn p => 
                                     Inline.inlineNonRecursive 
                                     (p, {small = small, product = product})),
                             execute = true}
                    val s = String.dropPrefix (s, String.size "inlineNonRecursive")
                 in
                    case nums s of
                       SOME [IntOpt (SOME product), IntOpt (SOME small)] => 
                          mk (product, small)
                     | _ => NONE
                 end
         else if String.hasPrefix (s, {prefix = "inlineLeaf"})
            then let
                    fun mk (loops, repeat, size) =
                       SOME {name = concat ["inlineLeafRepeat(", 
                                            Bool.toString loops, ",",
                                            Bool.toString repeat, ",",
                                            Option.toString Int.toString size, ")#",
                                            Int.toString (Counter.next count)],
                             doit = (fn p => 
                                     Inline.inlineLeaf
                                     (p, {loops = loops, repeat = repeat, size = size})),
                             execute = true}
                    val s = String.dropPrefix (s, String.size "inlineLeaf")
                 in
                    case nums s of
                       SOME [Bool loops, Bool repeat, IntOpt size] => 
                          mk (loops, repeat, size)
                     | _ => NONE
                 end
         else NONE
      end

   val passGens = 
      inlinePassGen ::
      (List.map([("combineConversions",  CombineConversions.transform),
                 ("commonArg", CommonArg.transform),
                 ("commonBlock", CommonBlock.transform),
                 ("commonSubexp", CommonSubexp.transform),
                 ("constantPropagation", ConstantPropagation.transform),
                 ("contify", Contify.transform),
                 ("flatten", Flatten.transform),
                 ("introduceLoops", IntroduceLoops.transform),
                 ("knownCase", KnownCase.transform),
                 ("localFlatten", LocalFlatten.transform),
                 ("localRef", LocalRef.transform),
                 ("loopInvariant", LoopInvariant.transform),
                 ("loopUnroll", LoopUnroll.transform),
                 ("loopUnswitch", LoopUnswitch.transform),
                 ("polyEqual", PolyEqual.transform),
                 ("polyHash", PolyHash.transform),
                 ("redundant", Redundant.transform),
                 ("redundantTests", RedundantTests.transform),
                 ("removeUnused", RemoveUnused.transform),
                 ("shareZeroVec", ShareZeroVec.transform),
                 ("simplifyTypes", SimplifyTypes.transform),
                 ("useless", Useless.transform),
                 ("ssaAddProfile", Profile.addProfile),
                 ("ssaDropProfile", Profile.dropProfile),
                 ("ssaBreakCriticalEdges", fn p => S.breakCriticalEdges (p, {codeMotion = true})),
                 ("ssaEliminateDeadBlocks", S.eliminateDeadBlocks),
                 ("ssaOrderFunctions", S.orderFunctions),
                 ("ssaReverseFunctions", S.reverseFunctions),
                 ("ssaShrink", S.shrink)],
                mkSimplePassGen))
in
   fun ssaPassesSetCustom s =
      Exn.withEscape
      (fn esc =>
       (let val ss = String.split (s, #":")
        in 
           ssaPasses := 
           List.map(ss, fn s =>
                    case (List.peekMap (passGens, fn gen => gen s)) of
                       NONE => esc (Result.No s)
                     | SOME pass => pass)
           ; Result.Yes ()
        end))
end

val ssaPassesString = ref "default"
val ssaPassesGet = fn () => !ssaPassesString
val ssaPassesSet = fn s =>
   let
      val _ = ssaPassesString := s
   in
      case s of
         "default" => (ssaPasses := ssaPassesDefault
                       ; Result.Yes ())
       | "minimal" => (ssaPasses := ssaPassesMinimal
                       ; Result.Yes ())
       | _ => ssaPassesSetCustom s
   end
val _ = List.push (Control.optimizationPasses,
                   {il = "ssa", get = ssaPassesGet, set = ssaPassesSet})

fun pass ({name, doit, midfix}, p) =
   let
      val _ =
         let open Control
         in maybeSaveToFile
            ({name = name, 
              suffix = midfix ^ "pre.ssa"},
             Control.No, p, Control.Layouts Program.layouts)
         end
      val p =
         Control.passTypeCheck
         {display = Control.Layouts Program.layouts,
          name = name,
          stats = Program.layoutStats,
          style = Control.No,
          suffix = midfix ^ "post.ssa",
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
      else (Control.messageStr (Control.Pass, name ^ " skipped"); p)

fun simplify p =
   let
      fun simplify' n p =
         let
            val midfix = if !Control.loopSsaPasses = 1
                            then ""
                         else concat [Int.toString n, "."]
         in
            if n = !Control.loopSsaPasses
               then p
            else simplify' 
                 (n + 1)
                 (List.fold
                  (!ssaPasses, p, fn ({name, doit, execute}, p) =>
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
                               andalso !Control.profileIL = Control.ProfileSSA
                               then pass ({name = "ssaAddProfile",
                                           doit = Profile.addProfile,
                                           midfix = ""}, p)
                            else p
                         val p = maybePass ({name = "ssaOrderFunctions",
                                             doit = S.orderFunctions,
                                             execute = true,
                                             midfix = ""}, p)
                         val _ = typeCheck p
                       in
                         p
                       end
end
