(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)

(* Integer arithmetic without overflow checking. *)
val *? = Primitive.Int.*?
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
