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

fun traces s p = (Trace.Immediate.on s; p)

val passes =
   [
    ("xmlShrink1", S.shrink),
    ("simplifyTypes", SimplifyTypes.simplifyTypes)
   ]
   
fun stats p =
   Control.message (Control.Detail, fn () => Program.layoutStats p)

fun simplify p =
   (stats p
    ; (List.fold
       (passes, p, fn ((name, pass), p) =>
      if List.contains (!Control.dropPasses, name, String.equals)
         then p
      else
         let
            val _ =
	       let
		  open Control
	       in maybeSaveToFile
		  ({name = name, suffix = "pre.xml"},
		   Control.No, p, Control.Layout Program.layout)
	       end
            val p =
               Control.passTypeCheck
               {name = name,
                suffix = "post.xml",
                style = Control.No,
                thunk = fn () => pass p,
                display = Control.Layout Program.layout,
                typeCheck = typeCheck}
            val _ = stats p
         in
            p
         end)))

val typeCheck = S.typeCheck

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
