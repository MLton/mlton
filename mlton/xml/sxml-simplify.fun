(* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor SxmlSimplify (S: SXML_SIMPLIFY_STRUCTS): SXML_SIMPLIFY = 
struct

open S

structure ImplementExceptions = ImplementExceptions (open S)
structure ImplementSuffix = ImplementSuffix (open S)
structure Polyvariance = Polyvariance (open S)
(* structure Uncurry = Uncurry (open S) *)
structure CPSTransform = CPSTransform (open S)

fun polyvariance (hofo, rounds, small, product) p =
   Ref.fluidLet
   (Control.polyvariance,
    SOME {hofo = hofo, rounds = rounds, small = small, product = product},
    fn () => Polyvariance.duplicate p)

type pass = {name: string,
             doit: Program.t -> Program.t}

val sxmlPassesDefault =
   {name = "sxmlShrink1", doit = S.shrink} ::
   {name = "implementSuffix", doit = ImplementSuffix.doit} ::
   {name = "sxmlShrink2", doit = S.shrink} ::
   {name = "implementExceptions", doit = ImplementExceptions.doit} ::
   {name = "sxmlShrink3", doit = S.shrink} ::
   (* {name = "uncurry", doit = Uncurry.uncurry} :: *)
   (* {name = "sxmlShrink4", doit = S.shrink} :: *)
   {name = "polyvariance", doit = Polyvariance.duplicate} ::
   {name = "sxmlShrink4", doit = S.shrink} ::
   nil

val sxmlPassesCpsTransform =
   sxmlPassesDefault @
   {name = "cpsTransform", doit = CPSTransform.doit} ::
   {name = "cpsSxmlShrink5", doit = S.shrink} ::
   {name = "cpsPolyvariance", doit = Polyvariance.duplicate} ::
   {name = "cpsSxmlShrink6", doit = S.shrink} ::
   nil

val sxmlPassesMinimal =
   {name = "implementSuffix", doit = ImplementSuffix.doit} ::
   {name = "implementExceptions", doit = ImplementExceptions.doit} ::
   nil

val sxmlPasses : pass list ref = ref sxmlPassesDefault

local
   type passGen = string -> pass option

   fun mkSimplePassGen (name, doit): passGen =
      let val count = Counter.new 1
      in fn s => if s = name
                    then SOME {name = name ^ "#" ^ 
                               (Int.toString (Counter.next count)),
                               doit = doit}
                    else NONE
      end

   val polyvariancePassGen =
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
         if String.hasPrefix (s, {prefix = "polyvariance"})
            then let
                    fun mk (hofo, rounds, small, product) =
                       SOME {name = concat ["polyvariance(", 
                                            Bool.toString hofo, ",",
                                            Int.toString rounds, ",",
                                            Int.toString small, ",",
                                            Int.toString product, ")#",
                                            Int.toString (Counter.next count)],
                             doit = polyvariance (hofo, rounds, small, product)}
                    val s = String.dropPrefix (s, String.size "polyvariance")
                 in
                    case nums s of
                       SOME [] => mk (true, 2, 30, 300)
                     | SOME [hofo, rounds, small, product] =>
                          mk (hofo <> 0, rounds, small, product)
                     | _ => NONE
                 end
         else NONE
      end

   val passGens =
      polyvariancePassGen ::
      (List.map([("sxmlShrink", S.shrink),
                 ("implementExceptions", ImplementExceptions.doit), 
                 ("implementSuffix", ImplementSuffix.doit)],
                mkSimplePassGen))
in
   fun sxmlPassesSetCustom s =
      Exn.withEscape
      (fn esc =>
       (let val ss = String.split (s, #":")
        in
           sxmlPasses :=
           List.map(ss, fn s =>
                    case (List.peekMap (passGens, fn gen => gen s)) of
                       NONE => esc (Result.No s)
                     | SOME pass => pass)
           ; Result.Yes ()
        end))
end

val sxmlPassesString = ref "default"
val sxmlPassesGet = fn () => !sxmlPassesString
val sxmlPassesSet = fn s =>
   let
      val _ = sxmlPassesString := s
   in
      case s of
         "default" => (sxmlPasses := sxmlPassesDefault
                       ; Result.Yes ())
       | "cpsTransform" => (sxmlPasses := sxmlPassesCpsTransform
                            ; Result.Yes ())
       | "minimal" => (sxmlPasses := sxmlPassesMinimal
                       ; Result.Yes ())
       | _ => sxmlPassesSetCustom s
   end
val _ = List.push (Control.optimizationPasses,
                   {il = "sxml", get = sxmlPassesGet, set = sxmlPassesSet})

fun pass ({name, doit}, p) =
   let
      val _ =
         let open Control
         in maybeSaveToFile
            ({name = name,
              suffix = "pre.sxml"},
             Control.No, p, Control.Layouts Program.layouts)
         end
      val p =
         Control.passTypeCheck
         {display = Control.Layouts Program.layouts,
          name = name,
          stats = Program.layoutStats,
          style = Control.No,
          suffix = "post.sxml",
          thunk = fn () => doit p,
          typeCheck = typeCheck}
   in
      p
   end
fun maybePass ({name, doit}, p) =
   if List.exists (!Control.dropPasses, fn re =>
                   Regexp.Compiled.matchesAll (re, name))
      then p
   else pass ({name = name, doit = doit}, p)

fun simplify p =
   let
      fun simplify' p =
         List.fold
         (!sxmlPasses, p, fn ({name, doit}, p) =>
          maybePass ({name = name, doit = doit}, p))
      val p = simplify' p
   in
      p
   end

val simplify = fn p => let
                         (* Always want to type check the initial and final XML
                          * programs, even if type checking is turned off, just
                          * to catch bugs.
                          *)
                         val _ = typeCheck p
                         val p' = simplify p
                         val _ = typeCheck p'
                       in
                         p'
                       end
end
