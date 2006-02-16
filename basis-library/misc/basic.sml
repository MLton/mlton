(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
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
