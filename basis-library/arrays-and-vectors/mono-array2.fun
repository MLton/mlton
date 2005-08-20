(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor MonoArray2 (type elem
                    structure V: MONO_VECTOR
                      where type elem = elem
                        and type vector = elem Vector.vector): MONO_ARRAY2 =
   struct
      type elem = V.elem
      type vector = V.vector
      open Array2
      type array = elem array
      type region = {base: array,
                     row: int,
                     col: int,
                     nrows: int option,
                     ncols: int option}
   end
