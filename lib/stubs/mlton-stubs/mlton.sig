(* Copyright (C) 2009,2013,2019,2022 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON =
   sig
      include MLTON_ROOT

      structure Array: MLTON_ARRAY
      structure Exn: MLTON_EXN
      structure GC: MLTON_GC
      structure Platform: MLTON_PLATFORM
      structure Process: MLTON_PROCESS
      structure Profile: MLTON_PROFILE
      structure Random: MLTON_RANDOM
      structure Rusage: MLTON_RUSAGE
      structure TextIO: MLTON_TEXT_IO
      structure Vector: MLTON_VECTOR
   end
