(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Atoms (S: ATOMS_STRUCTS): ATOMS =
struct

structure Atoms =
   struct
      open S

      structure ProfileLabel = ProfileLabel ()
      structure SourceInfo = SourceInfo ()
      structure ProfileExp = ProfileExp (structure SourceInfo = SourceInfo)
      structure Var = Var ()
      structure Tycon = Tycon (structure CharSize = CharSize
                               structure IntSize = IntSize
                               structure RealSize = RealSize
                               structure WordSize = WordSize)
      structure Con = Con ()
      structure CType = CType (structure RealSize = RealSize
                               structure WordSize = WordSize)
      structure WordX = WordX (structure WordSize = WordSize)
      structure RealX = RealX (structure RealSize = RealSize
                               structure WordX = WordX)
      structure WordXVector = WordXVector (structure WordSize = WordSize
                                           structure WordX = WordX)
      structure Func =
         struct
            open Var
            fun newNoname () = newString "F"
         end
      structure Label =
         struct
            open Func
            fun newNoname () = newString "L"
         end
      structure Const = Const (structure RealX = RealX
                               structure WordX = WordX
                               structure WordXVector = WordXVector)
      structure CFunction = CFunction (structure CType = CType)
      structure Prim = Prim (structure CFunction = CFunction
                             structure CType = CType
                             structure Con = Con
                             structure Const = Const
                             structure RealSize = RealSize
                             structure WordSize = WordSize)
      structure Ffi = Ffi (structure CFunction = CFunction
                           structure CType = CType)
      structure Vars = UnorderedSet (Var)
   end

open Atoms

end
