(* Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor XmlSimplify (S: XML_SIMPLIFY_STRUCTS): XML_SIMPLIFY = 
struct

open S

structure SimplifyTypes = SimplifyTypes (structure Input = S
                                         structure Output = S)

type pass = {name: string,
             doit: Program.t -> Program.t}

val xmlPassesDefault =
   {name = "xmlShrink", doit = S.shrink} ::
   {name = "xmlSimplifyTypes", doit = SimplifyTypes.simplifyTypes} ::
   nil

val xmlPassesMinimal =
   nil

val xmlPasses : pass list ref = ref xmlPassesDefault

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

   val passGens =
      (List.map([("xmlShrink", S.shrink),
                 ("xmlSimplifyTypes", SimplifyTypes.simplifyTypes)],
                mkSimplePassGen))

   fun xmlPassesSetCustom s =
      Exn.withEscape
      (fn esc =>
       (let val ss = String.split (s, #":")
        in
           xmlPasses :=
           List.map(ss, fn s =>
                    case (List.peekMap (passGens, fn gen => gen s)) of
                       NONE => esc (Result.No s)
                     | SOME pass => pass)
           ; Control.xmlPasses := ss
           ; Result.Yes ()
        end))

   datatype t = datatype Control.optimizationPasses
   fun xmlPassesSet opt =
      case opt of
         OptPassesDefault => (xmlPasses := xmlPassesDefault
                              ; Control.xmlPasses := ["default"]
                              ; Result.Yes ())
       | OptPassesMinimal => (xmlPasses := xmlPassesMinimal
                              ; Control.xmlPasses := ["minimal"]
                              ; Result.Yes ())
       | OptPassesCustom s => xmlPassesSetCustom s
in
   val _ = Control.xmlPassesSet := xmlPassesSet
   val _ = List.push (Control.optimizationPassesSet, ("xml", xmlPassesSet))
end

fun pass ({name, doit}, p) =
   let
      val _ =
         let open Control
         in maybeSaveToFile
            ({name = name,
              suffix = "pre.xml"},
             Control.No, p, Control.Layouts Program.layouts)
         end
      val p =
         Control.passTypeCheck
         {display = Control.Layouts Program.layouts,
          name = name,
          stats = Program.layoutStats,
          style = Control.No,
          suffix = "post.xml",
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
         (!xmlPasses, p, fn ({name, doit}, p) =>
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
