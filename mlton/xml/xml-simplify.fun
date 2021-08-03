(* Copyright (C) 2019,2021 Matthew Fluet.
 * Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
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
      let val count = Counter.generator 1
      in fn s => if s = name
                    then SOME {name = concat [name, "#",
                                              Int.toString (count ())],
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

fun xmlPassesSet s =
   case s of
      "default" => (xmlPasses := xmlPassesDefault
                    ; Result.Yes ())
    | "minimal" => (xmlPasses := xmlPassesMinimal
                    ; Result.Yes ())
    | _ => xmlPassesSetCustom s
val _ = Control.OptimizationPasses.register {il = "xml", set = xmlPassesSet}

fun simplify p =
   let
      val xmlPasses = !xmlPasses
      val p =
         Control.simplifyPasses
         {arg = p,
          passes = xmlPasses,
          stats = Program.layoutStats,
          toFile = Program.toFile,
          typeCheck = typeCheck}
   in
      p
   end
end
