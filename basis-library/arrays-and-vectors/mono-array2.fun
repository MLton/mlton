(* Copyright (C) 2015 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor MonoArray2 (type elem
                    structure MV: MONO_VECTOR
                      where type elem = elem
                        and type vector = elem Vector.vector): MONO_ARRAY2 =
   struct
      type elem = MV.elem
      type vector = MV.vector
      open Array2
      type array = elem array
      type region = {base: array,
                     row: int,
                     col: int,
                     nrows: int option,
                     ncols: int option}
   end
