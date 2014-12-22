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

      structure CharSize = CharSize ()
      structure IntSize = IntSize ()
      structure RealSize = RealSize ()
      structure WordSize = WordSize ()

      structure WordX = WordX (structure WordSize = WordSize)
      structure WordXVector = WordXVector (structure WordSize = WordSize
                                           structure WordX = WordX)
      structure RealX = RealX (structure RealSize = RealSize
                               structure WordX = WordX)
      structure Const = Const (structure RealX = RealX
                               structure WordX = WordX
                               structure WordXVector = WordXVector)

      structure Symbol = Symbol ()
      structure Field = Field (structure Symbol = Symbol)
      structure Record = Record (val isSorted = false
                                 structure Field = Field)
      structure SortedRecord = Record (val isSorted = true
                                       structure Field = Field)

      structure Tyvar = Tyvar ()
      structure AdmitsEquality = AdmitsEquality ()
      structure TyconKind = TyconKind ()
      structure Tycon = Tycon (structure AdmitsEquality = AdmitsEquality
                               structure CharSize = CharSize
                               structure IntSize = IntSize
                               structure Kind = TyconKind
                               structure RealSize = RealSize
                               structure WordSize = WordSize)
      structure Con = Con ()
      structure Var = Var ()
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

      structure CType = CType (structure RealSize = RealSize
                               structure WordSize = WordSize)
      structure CFunction = CFunction (structure CType = CType)
      structure Ffi = Ffi (structure CFunction = CFunction
                           structure CType = CType)

      structure Prim = Prim (structure CFunction = CFunction
                             structure CType = CType
                             structure Con = Con
                             structure Const = Const
                             structure RealSize = RealSize
                             structure WordSize = WordSize)

      structure SourceInfo = SourceInfo ()
      structure ProfileLabel = ProfileLabel ()
      structure ProfileExp = ProfileExp (structure SourceInfo = SourceInfo)
   end

open Atoms

end
