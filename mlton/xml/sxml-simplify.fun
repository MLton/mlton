(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
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

fun polyvariance (rounds, small, product) p =
   Ref.fluidLet
   (Control.polyvariance,
    SOME {rounds = rounds, small = small, product = product},
    fn () => Polyvariance.duplicate p)

type pass = {name: string,
             enable: unit -> bool,
             doit: Program.t -> Program.t}

val sxmlPassesDefault =
   {name = "sxmlShrink1", 
    enable = fn () => true, doit = S.shrink} ::
   {name = "implementSuffix", 
    enable = fn () => true, doit = ImplementSuffix.doit} ::
   {name = "sxmlShrink2", 
    enable = fn () => true, doit = S.shrink} ::
   {name = "implementExceptions", 
    enable = fn () => true, doit = ImplementExceptions.doit} ::
   {name = "sxmlShrink3", 
    enable = fn () => true, doit = S.shrink} ::
(*
   {name = "uncurry", 
    enable = fn () => true, doit = Uncurry.uncurry} ::
   {name = "sxmlShrink4", 
    enable = fn () => true, doit = S.shrink} ::
*)
   {name = "polyvariance", 
    enable = fn () => true, doit = Polyvariance.duplicate} ::
   {name = "sxmlShrink4", 
    enable = fn () => true, doit = S.shrink} ::
   {name = "cpsTransform", 
    enable = fn () => !Control.cpsTransform, doit = CPSTransform.doit} ::
   {name = "cpsSxmlShrink5", 
    enable = fn () => !Control.cpsTransform, doit = S.shrink} ::
   {name = "cpsPolyvariance", 
    enable = fn () => !Control.cpsTransform, doit = Polyvariance.duplicate} ::
   {name = "cpsSxmlShrink6", 
    enable = fn () => !Control.cpsTransform, doit = S.shrink} ::
   nil

val sxmlPassesMinimal =
   {name = "implementSuffix", 
    enable = fn () => true, doit = ImplementSuffix.doit} ::
   {name = "sxmlShrink2", 
    enable = fn () => true, doit = S.shrink} ::
   {name = "implementExceptions", 
    enable = fn () => true, doit = ImplementExceptions.doit} ::
   nil

val sxmlPasses : pass list ref = ref sxmlPassesDefault

local
   type passGen = string -> pass option

   fun mkSimplePassGen (name, doit): passGen =
      let val count = Counter.new 1
      in fn s => if s = name
                    then SOME {name = name ^ "#" ^ 
                               (Int.toString (Counter.next count)),
                               enable = fn () => true,
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
                    fun mk (rounds, small, product) =
                       SOME {name = concat ["polyvariance(", 
                                            Int.toString rounds, ",",
                                            Int.toString small, ",",
                                            Int.toString product, ")#",
                                            Int.toString (Counter.next count)],
                             enable = fn () => true,
                             doit = polyvariance (rounds, small, product)}
                    val s = String.dropPrefix (s, String.size "polyvariance")
                 in
                    case nums s of
                       SOME [] => mk (2, 30, 300)
                     | SOME [rounds, small, product] => mk (rounds, small, product)
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
           ; Control.sxmlPasses := ss
           ; Result.Yes ()
        end))

   datatype t = datatype Control.optimizationPasses
   fun sxmlPassesSet opt =
      case opt of
         OptPassesDefault => (sxmlPasses := sxmlPassesDefault
                              ; Control.sxmlPasses := ["default"]
                              ; Result.Yes ())
       | OptPassesMinimal => (sxmlPasses := sxmlPassesMinimal
                              ; Control.sxmlPasses := ["minimal"]
                              ; Result.Yes ())
       | OptPassesCustom s => sxmlPassesSetCustom s
in
   val _ = Control.sxmlPassesSet := sxmlPassesSet
   val _ = List.push (Control.optimizationPassesSet, ("sxml", sxmlPassesSet))
end

fun stats p =
   Control.message (Control.Detail, fn () => Program.layoutStats p)

fun simplify p =
   (stats p
    ; (List.fold
       (!sxmlPasses, p, fn ({name, enable, doit}, p) =>
      if List.exists (!Control.dropPasses, fn re =>
                      Regexp.Compiled.matchesAll (re, name))
         orelse not (enable ())
         then p
      else
         let
            val _ =
               let open Control
               in maybeSaveToFile
                  ({name = name, suffix = "pre.sxml"},
                   Control.No, p, Control.Layout Program.layout)
               end
            val p =
               Control.passTypeCheck
               {name = name,
                suffix = "post.sxml",
                style = Control.No,
                thunk = fn () => doit p,
                display = Control.Layout Program.layout,
                typeCheck = typeCheck}
            val _ = stats p
         in
            p
         end)))

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
