(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
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
structure PolyEqual = PolyEqual (S)
structure PolyHash = PolyHash (S)
structure Profile = Profile (S)
structure Redundant = Redundant (S)
structure RedundantTests = RedundantTests (S)
structure RemoveUnused = RemoveUnused (S)
structure SimplifyTypes = SimplifyTypes (S)
structure Useless = Useless (S)

type pass = {name: string,
             doit: Program.t -> Program.t}

val ssaPassesDefault =
   {name = "removeUnused1", doit = RemoveUnused.transform} ::
   {name = "introduceLoops1", doit = IntroduceLoops.transform} ::
   {name = "loopInvariant1", doit = LoopInvariant.transform} ::
   {name = "inlineLeaf1", doit = fn p => 
    Inline.inlineLeaf (p, !Control.inlineLeafA)} ::
   {name = "inlineLeaf2", doit = fn p => 
    Inline.inlineLeaf (p, !Control.inlineLeafB)} ::
   {name = "contify1", doit = Contify.transform} ::
   {name = "localFlatten1", doit = LocalFlatten.transform} ::
   {name = "constantPropagation", doit = ConstantPropagation.transform} ::
   (* useless should run 
    *   - after constant propagation because constant propagation makes
    *     slots of tuples that are constant useless
    *)
   {name = "useless", doit = Useless.transform} ::
   {name = "removeUnused2", doit = RemoveUnused.transform} ::
   {name = "simplifyTypes", doit = SimplifyTypes.transform} ::
   (* polyEqual should run
    *   - after types are simplified so that many equals are turned into eqs
    *   - before inlining so that equality functions can be inlined
    *)
   {name = "polyEqual", doit = PolyEqual.transform} ::
   (* polyHash should run
    *   - after types are simplified
    *   - before inlining so that hash functions can be inlined
    *)
   {name = "polyHash", doit = PolyHash.transform} ::
   {name = "introduceLoops2", doit = IntroduceLoops.transform} ::
   {name = "loopInvariant2", doit = LoopInvariant.transform} ::
   {name = "contify2", doit = Contify.transform} ::
   {name = "inlineNonRecursive", doit = fn p =>
    Inline.inlineNonRecursive (p, !Control.inlineNonRec)} ::
   {name = "localFlatten2", doit = LocalFlatten.transform} ::
   {name = "removeUnused3", doit = RemoveUnused.transform} ::
   {name = "contify3", doit = Contify.transform} ::
   {name = "introduceLoops3", doit = IntroduceLoops.transform} ::
   {name = "loopInvariant3", doit = LoopInvariant.transform} ::
   {name = "localRef", doit = LocalRef.eliminate} ::
   {name = "flatten", doit = Flatten.transform} ::
   {name = "localFlatten3", doit = LocalFlatten.transform} ::
   {name = "combineConversions", doit = CombineConversions.transform} ::
   {name = "commonArg", doit = CommonArg.transform} ::
   {name = "commonSubexp", doit = CommonSubexp.transform} ::
   {name = "commonBlock", doit = CommonBlock.transform} ::
   {name = "redundantTests", doit = RedundantTests.transform} ::
   {name = "redundant", doit = Redundant.transform} ::
   {name = "knownCase", doit = KnownCase.simplify} ::
   {name = "removeUnused4", doit = RemoveUnused.transform} ::
   nil

val ssaPassesMinimal =
   (* polyEqual cannot be omitted.  It implements MLton_equal. *)
   {name = "polyEqual", doit = PolyEqual.transform} ::
   (* polyHash cannot be omitted.  It implements MLton_hash. *)
   {name = "polyHash", doit = PolyHash.transform} ::
   nil

val ssaPasses : pass list ref = ref ssaPassesDefault

local
   type passGen = string -> pass option

   fun mkSimplePassGen (name, doit): passGen =
      let val count = Counter.new 1
      in fn s => if s = name
                    then SOME {name = concat [name, "#",
                                              Int.toString (Counter.next count)],
                               doit = doit}
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
                                     (p, {small = small, product = product}))}
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
                                     (p, {loops = loops, repeat = repeat, size = size}))}
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
      (List.map([("addProfile", Profile.addProfile),
                 ("combineConversions",  CombineConversions.transform),
                 ("commonArg", CommonArg.transform),
                 ("commonBlock", CommonBlock.transform),
                 ("commonSubexp", CommonSubexp.transform),
                 ("constantPropagation", ConstantPropagation.transform),
                 ("contify", Contify.transform),
                 ("dropProfile", Profile.dropProfile),
                 ("flatten", Flatten.transform),
                 ("introduceLoops", IntroduceLoops.transform),
                 ("knownCase", KnownCase.simplify),
                 ("localFlatten", LocalFlatten.transform),
                 ("localRef", LocalRef.eliminate),
                 ("loopInvariant", LoopInvariant.transform),
                 ("polyEqual", PolyEqual.transform),
                 ("polyHash", PolyHash.transform),
                 ("redundant", Redundant.transform),
                 ("redundantTests", RedundantTests.transform),
                 ("removeUnused", RemoveUnused.transform),
                 ("simplifyTypes", SimplifyTypes.transform),
                 ("useless", Useless.transform),
                 ("breakCriticalEdges",fn p => 
                  S.breakCriticalEdges (p, {codeMotion = true})),
                 ("eliminateDeadBlocks",S.eliminateDeadBlocks),
                 ("orderFunctions",S.orderFunctions),
                 ("reverseFunctions",S.reverseFunctions),
                 ("shrink", S.shrink)], 
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
fun maybePass ({name, doit, midfix}, p) =
   if List.exists (!Control.dropPasses, fn re =>
                   Regexp.Compiled.matchesAll (re, name))
      then p
   else pass ({name = name, doit = doit, midfix = midfix}, p)

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
                  (!ssaPasses, p, fn ({name, doit}, p) =>
                   maybePass ({name = name, doit = doit, midfix = midfix}, p)))
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
                               then pass ({name = "addProfile1",
                                           doit = Profile.addProfile,
                                           midfix = ""}, p)
                            else p
                         val p = maybePass ({name = "orderFunctions1",
                                             doit = S.orderFunctions,
                                             midfix = ""}, p)
                         val _ = typeCheck p
                       in
                         p
                       end
end
