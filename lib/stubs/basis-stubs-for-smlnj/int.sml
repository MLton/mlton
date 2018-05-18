(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor FixInt(PInt: sig include INTEGER end) : INTEGER =
   struct
      open PInt

      local
         (* SML/NJ uses lower instead of upper case. *)
         val toUpper = String.translate (Char.toString o Char.toUpper)
      in
         fun fmt r w = toUpper (PInt.fmt r w)
         fun toString w = toUpper (PInt.toString w)
      end
   end

structure FixedInt = FixInt(struct open Pervasive.FixedInt end)
structure Int = FixInt(struct open Pervasive.Int end)
structure Int31 = FixInt(struct open Pervasive.Int31 end)
structure Int32 = FixInt(struct open Pervasive.Int32 end)
structure Int64 = FixInt(struct open Pervasive.Int64 end)
structure LargeInt = FixInt(struct open Pervasive.LargeInt end)
structure Position = FixInt(struct open Pervasive.Position end)
