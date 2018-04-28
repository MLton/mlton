(* Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor XmlSimplify (S: XML_SIMPLIFY_STRUCTS): XML_SIMPLIFY = 
struct

open S

structure SimplifyTypes = SimplifyTypes (structure Input = S
                                         structure Output = S)

type pass = {name: string,
             doit: Program.t -> Program.t,
             execute: bool}

val xmlPassesDefault =
   {name = "xmlShrink", doit = S.shrink, execute = true} ::
   {name = "xmlSimplifyTypes", doit = SimplifyTypes.simplifyTypes, execute = true} ::
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
                               doit = doit,
                               execute = true}
                    else NONE
      end

   val passGens =
      (List.map([("xmlShrink", S.shrink),
                 ("xmlSimplifyTypes", SimplifyTypes.simplifyTypes)],
                mkSimplePassGen))
in
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
           ; Result.Yes ()
        end))
end

val xmlPassesString = ref "default"
val xmlPassesGet = fn () => !xmlPassesString
val xmlPassesSet = fn s =>
   let
      val _ = xmlPassesString := s
   in
      case s of
         "default" => (xmlPasses := xmlPassesDefault
                       ; Result.Yes ())
       | "minimal" => (xmlPasses := xmlPassesMinimal
                       ; Result.Yes ())
       | _ => xmlPassesSetCustom s
   end
val _ = List.push (Control.optimizationPasses,
                   {il = "xml", get = xmlPassesGet, set = xmlPassesSet})

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
fun maybePass ({name, doit, execute}, p) =
   if List.foldr (!Control.executePasses, execute, fn ((re, new), old) =>
                  if Regexp.Compiled.matchesAll (re, name)
                     then new
                     else old)
      then pass ({name = name, doit = doit}, p)
      else (Control.messageStr (Control.Pass, name ^ " skipped"); p)
fun simplify p =
   let
      fun simplify' p =
         List.fold
         (!xmlPasses, p, fn ({name, doit, execute}, p) =>
          maybePass ({name = name, doit = doit, execute = execute}, p))
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
