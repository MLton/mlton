(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

(* Integer arithmetic without overflow checking. *)
val +? = Primitive.Int.+?
val -? = Primitive.Int.-?
infix 7 *?
infix 6 +? -?
   
val not =
   fn true => false
    | false => true

fun x <> y = not (x = y)

fun die (s: string): 'a =
   (Primitive.Stdio.print s
    ; PosixPrimitive.Process.exit 1
    ; let exception DieFailed
      in raise DieFailed
      end)

val _ =
   Primitive.TopLevel.setHandler 
   (fn exn => (Primitive.Stdio.print ("unhandled exception: ")
	       ; Primitive.Stdio.print (Primitive.Exn.name exn)
	       ; die "\n"))
