(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Int32 : INTEGER_EXTRA =
   Integer
   (structure P = Primitive.Int32
    open P
   )
structure Int = Int32
structure IntGlobal: INTEGER_GLOBAL = Int
open IntGlobal
