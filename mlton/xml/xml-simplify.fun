(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor XmlSimplify (S: XML_SIMPLIFY_STRUCTS): XML_SIMPLIFY = 
struct

open S

structure SimplifyTypes = SimplifyTypes (structure Input = S
					 structure Output = S)

type pass = {name: string,
	     doit: Program.t -> Program.t}

val xmlPasses : pass list ref = ref
   [
    {name = "xmlShrink1", doit = S.shrink},
    {name = "simplifyTypes", doit = SimplifyTypes.simplifyTypes}
   ]

local
   type passGen = string -> pass option
     
   fun mkSimplePassGen (name,doit) =
      let val count = Counter.new 1
      in fn s => if s = name
		    then SOME {name = name ^ "#" ^ 
			       (Int.toString (Counter.next count)),
			       doit = doit}
		    else NONE
      end

   val passGens =
      (List.map([("shrink", S.shrink),
		 ("simplifyTypes", SimplifyTypes.simplifyTypes)],
		mkSimplePassGen))

   fun xmlPassesSet s =
      DynamicWind.withEscape
      (fn esc =>
       (let val ss = String.split (s, #":")
	in
	   xmlPasses :=
	   List.map(ss, fn s =>
		    case (List.peekMap (passGens, fn gen => gen s)) of
		       NONE => esc (Result.No s)
		     | SOME pass => pass)
	   ; Result.Yes ss
	end))
in
   val _ = Control.xmlPassesSet := xmlPassesSet
end

   
fun stats p =
   Control.message (Control.Detail, fn () => Program.layoutStats p)

fun simplify p =
   (stats p
    ; (List.fold
       (!xmlPasses, p, fn ({name, doit}, p) =>
      if List.exists (!Control.dropPasses, fn re =>
		      Regexp.Compiled.matchesAll (re, name))
         then p
      else
         let
            val _ =
	       let open Control
	       in maybeSaveToFile
		  ({name = name, suffix = "pre.xml"},
		   Control.No, p, Control.Layout Program.layout)
	       end
            val p =
               Control.passTypeCheck
               {name = name,
                suffix = "post.xml",
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
