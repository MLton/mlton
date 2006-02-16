(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
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
structure Redundant = Redundant (S)
structure RedundantTests = RedundantTests (S)
structure RemoveUnused = RemoveUnused (S)
structure SimplifyTypes = SimplifyTypes (S)
structure Useless = Useless (S)

fun inlineNonRecursive (product, small) p =
   Ref.fluidLet
   (Control.inline, 
    Control.NonRecursive {product = product, small = small}, 
    fn () => Inline.inline p)
fun inlineLeaf size p =
   Ref.fluidLet
   (Control.inlineIntoMain, true, fn () =>
    Ref.fluidLet
    (Control.inline, Control.Leaf {size = SOME size}, fn () =>
     Inline.inline p))
fun inlineLeafNoLoop size p =
   Ref.fluidLet
   (Control.inline, 
    Control.LeafNoLoop {size = SOME size}, 
    fn () => Inline.inline p)

type pass = {name: string,
             doit: Program.t -> Program.t}

val ssaPassesDefault =
   {name = "removeUnused1", doit = RemoveUnused.remove} ::
   {name = "leafInline", doit = inlineLeaf 20} ::
   {name = "contify1", doit = Contify.contify} ::
   {name = "localFlatten1", doit = LocalFlatten.flatten} ::
   {name = "constantPropagation", doit = ConstantPropagation.simplify} ::
   (* useless should run 
    *   - after constant propagation because constant propagation makes
    *     slots of tuples that are constant useless
    *)
   {name = "useless", doit = Useless.useless} ::
   {name = "removeUnused2", doit = RemoveUnused.remove} ::
   {name = "simplifyTypes", doit = SimplifyTypes.simplify} ::
   (* polyEqual should run
    *   - after types are simplified so that many equals are turned into eqs
    *   - before inlining so that equality functions can be inlined
    *)
   {name = "polyEqual", doit = PolyEqual.polyEqual} ::
   {name = "contify2", doit = Contify.contify} ::
   {name = "inline", doit = Inline.inline} ::
   {name = "localFlatten2", doit = LocalFlatten.flatten} ::
   {name = "removeUnused3", doit = RemoveUnused.remove} ::
   {name = "contify3", doit = Contify.contify} ::
   {name = "introduceLoops", doit = IntroduceLoops.introduceLoops} ::
   {name = "loopInvariant", doit = LoopInvariant.loopInvariant} ::
   {name = "localRef", doit = LocalRef.eliminate} ::
   {name = "flatten", doit = Flatten.flatten} ::
   {name = "localFlatten3", doit = LocalFlatten.flatten} ::
   {name = "commonArg", doit = CommonArg.eliminate} ::
   {name = "commonSubexp", doit = CommonSubexp.eliminate} ::
   {name = "commonBlock", doit = CommonBlock.eliminate} ::
   {name = "redundantTests", doit = RedundantTests.simplify} ::
   {name = "redundant", doit = Redundant.redundant} ::
   {name = "knownCase", doit = KnownCase.simplify} ::
   {name = "removeUnused4", doit = RemoveUnused.remove} ::
   nil

val ssaPassesMinimal =
   (* constantPropagation cannot be omitted. It implements Array_array0. *)
   {name = "constantPropagation", doit = ConstantPropagation.simplify} ::
   (* polyEqual cannot be omitted.  It implements MLton_equal. *)
   {name = "polyEqual", doit = PolyEqual.polyEqual} ::
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
         val count = Counter.new 1
         fun nums s =
            if s = ""
               then SOME []
            else if String.sub (s, 0) = #"(" 
                  andalso String.sub (s, String.size s - 1)= #")"
               then let
                       val s = String.dropFirst (String.dropLast s)
                    in
                       case List.fold (String.split (s, #","), SOME [],
                                       fn (s,SOME nums) => (case Int.fromString s of
                                                               SOME i => SOME (i::nums)
                                                             | NONE => NONE)
                                        | (_, NONE) => NONE) of
                          SOME (l as _::_) => SOME (List.rev l)
                        | _ => NONE
                    end
            else NONE
      in
         fn s =>
         if String.hasPrefix (s, {prefix = "inlineNonRecursive"})
            then let
                    fun mk (product, small) =
                       SOME {name = concat ["inlineNonRecursive(", 
                                            Int.toString product, ",",
                                            Int.toString small, ")#",
                                            Int.toString (Counter.next count)],
                             doit = inlineNonRecursive (product, small)}
                    val s = String.dropPrefix (s, String.size "inlineNonRecursive")
                 in
                    case nums s of
                       SOME [] => mk (320, 60)
                     | SOME [product, small] => mk (product, small)
                     | _ => NONE
                 end
         else if String.hasPrefix (s, {prefix = "inlineLeafNoLoop"})
            then let
                    fun mk size =
                       SOME {name = concat ["inlineLeafNoLoop(", 
                                            Int.toString size, ")#",
                                            Int.toString (Counter.next count)],
                             doit = inlineLeafNoLoop size}
                    val s = String.dropPrefix (s, String.size "inlineLeafNoLoop")
                 in
                    case nums s of
                       SOME [] => mk 20
                     | SOME [size] => mk size
                     | _ => NONE
                 end
         else if String.hasPrefix (s, {prefix = "inlineLeaf"})
            then let
                    fun mk size =
                       SOME {name = concat ["inlineLeaf(", 
                                            Int.toString size, ")#",
                                            Int.toString (Counter.next count)],
                             doit = inlineLeaf size}
                    val s = String.dropPrefix (s, String.size "inlineLeaf")
                 in
                    case nums s of
                       SOME [] => mk 20
                     | SOME [size] => mk size
                     | _ => NONE
                 end
         else NONE
      end

   val passGens = 
      inlinePassGen ::
      (List.map([("commonArg", CommonArg.eliminate),
                 ("commonBlock", CommonBlock.eliminate),
                 ("commonSubexp", CommonSubexp.eliminate),
                 ("constantPropagation", ConstantPropagation.simplify),
                 ("contify", Contify.contify),
                 ("dropProfile", S.dropProfile),
                 ("flatten", Flatten.flatten),
                 ("introduceLoops", IntroduceLoops.introduceLoops),
                 ("knownCase", KnownCase.simplify),
                 ("localFlatten", LocalFlatten.flatten),
                 ("localRef", LocalRef.eliminate),
                 ("loopInvariant", LoopInvariant.loopInvariant),
                 ("polyEqual", PolyEqual.polyEqual),
                 ("redundant", Redundant.redundant),
                 ("redundantTests", RedundantTests.simplify),
                 ("removeUnused", RemoveUnused.remove),
                 ("simplifyTypes", SimplifyTypes.simplify),
                 ("useless", Useless.useless),
                 ("breakCriticalEdges",fn p => 
                  S.breakCriticalEdges (p, {codeMotion = true})),
                 ("eliminateDeadBlocks",S.eliminateDeadBlocks),
                 ("reverseFunctions",S.reverseFunctions),
                 ("shrink", S.shrink)], 
                mkSimplePassGen))

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
           ; Control.ssaPasses := ss
           ; Result.Yes ()
        end))

   datatype t = datatype Control.optimizationPasses
   fun ssaPassesSet opt =
      case opt of
         OptPassesDefault => (ssaPasses := ssaPassesDefault
                              ; Control.ssaPasses := ["default"]
                              ; Result.Yes ())
       | OptPassesMinimal => (ssaPasses := ssaPassesMinimal
                              ; Control.ssaPasses := ["minimal"]
                              ; Result.Yes ())
       | OptPassesCustom s => ssaPassesSetCustom s
in
   val _ = Control.ssaPassesSet := ssaPassesSet
   val _ = List.push (Control.optimizationPassesSet, ("ssa", ssaPassesSet))
end

fun stats p = Control.message (Control.Detail, fn () => Program.layoutStats p)

fun simplify p =
   let
      fun simplify' n p =
         let
            val mkSuffix = if n = 0
                             then fn s => s
                             else let val n' = Int.toString n
                                  in fn s => concat [n',".",s]
                                  end
         in
            if n = !Control.loopPasses
               then p
            else simplify' 
                 (n + 1)
                 (List.fold
                  (!ssaPasses, p, fn ({name, doit}, p) =>
                   if List.exists (!Control.dropPasses, fn re =>
                                   Regexp.Compiled.matchesAll (re, name))
                      then p
                   else
                     let
                       val _ =
                          let open Control
                          in maybeSaveToFile
                             ({name = name, suffix = mkSuffix "pre.ssa"},
                              Control.No, p, Control.Layouts Program.layouts)
                          end
                       val p =
                          Control.passTypeCheck
                          {name = name,
                           suffix = mkSuffix "post.ssa",
                           style = Control.No,
                           thunk = fn () => doit p,
                           display = Control.Layouts Program.layouts,
                           typeCheck = typeCheck}
                       val _ = stats p
                     in
                       p
                     end))
         end
   in
     stats p
     ; simplify' 0 p
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
                               then Program.profile p
                            else p
                         val _ = typeCheck p
                       in
                         p
                       end
end
