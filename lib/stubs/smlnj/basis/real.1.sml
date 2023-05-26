(* Copyright (C) 2009,2019,2022-2023 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor FixReal(PReal: sig include PERVASIVE_REAL end) : REAL =
   struct
      open PReal

      (* SML/NJ doesn't support EXACT. *)
      fun fmt f =
         PReal.fmt
         (let
             datatype z = datatype StringCvt.realfmt
          in
             case f of
                EXACT => StringCvt.GEN NONE
              | FIX io => StringCvt.FIX io
              | GEN io => StringCvt.GEN io
              | SCI io => StringCvt.SCI io
          end)
   end

structure Real = FixReal(struct open Pervasive.Real end)
