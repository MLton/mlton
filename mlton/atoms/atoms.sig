(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature ATOMS_STRUCTS =
   sig
   end

signature ATOMS' =
   sig
      include ATOMS_STRUCTS

      structure AdmitsEquality: ADMITS_EQUALITY
      structure CFunction: C_FUNCTION
      structure CType: C_TYPE
      structure CharSize: CHAR_SIZE
      structure Con: CON
      structure Const: CONST
      structure Ffi: FFI
      structure Field: FIELD
      structure Func: FUNC
      structure IntSize: INT_SIZE
      structure Label: LABEL
      structure Prim: PRIM
      structure ProfileExp: PROFILE_EXP
      structure ProfileLabel: PROFILE_LABEL
      structure RealSize: REAL_SIZE
      structure RealX: REAL_X
      structure Record: RECORD
      structure SortedRecord: RECORD
      structure SourceInfo: SOURCE_INFO
      structure Symbol: SYMBOL
      structure Tycon: TYCON
      structure TyconKind: TYCON_KIND
      structure Tyvar: TYVAR
      structure Var: VAR 
      structure WordSize: WORD_SIZE
      structure WordX: WORD_X
      structure WordXVector: WORD_X_VECTOR

      sharing CFunction = Ffi.CFunction = Prim.CFunction
      sharing CType = CFunction.CType = Ffi.CType = Prim.CType
      sharing CharSize = Tycon.CharSize
      sharing Con = Prim.Con
      sharing Const = Prim.Const
      sharing Field = Record.Field = SortedRecord.Field
      sharing IntSize = Tycon.IntSize
      sharing RealSize = CType.RealSize = Prim.RealSize = RealX.RealSize
         = Tycon.RealSize
      sharing RealX = Const.RealX
      sharing SourceInfo = ProfileExp.SourceInfo
      sharing WordSize = CType.WordSize = Prim.WordSize = Tycon.WordSize
         = WordX.WordSize
      sharing WordX = Const.WordX = WordXVector.WordX
      sharing WordXVector = Const.WordXVector
   end

signature ATOMS =
   sig
      structure Atoms: ATOMS'

      include ATOMS'

      (* For each structure, like CFunction, I would like to write two sharing
       * constraints
       *   sharing Atoms = CFunction
       *   sharing CFunction = Atoms.CFunction
       * but I can't because of a bug in SML/NJ that reports "Sharing structure
       * with a descendent substructure".  So, I am forced to write out lots
       * of individual sharing constraints.  Blech.
       *)
      sharing CFunction = Atoms.CFunction
      sharing CType = Atoms.CType
      sharing Con = Atoms.Con
      (* sharing Cons = Atoms.Cons *)
      sharing Const = Atoms.Const
      sharing Ffi = Atoms.Ffi
      sharing Field = Atoms.Field
      sharing Func = Atoms.Func
      sharing Label = Atoms.Label
      sharing Prim = Atoms.Prim
      sharing ProfileLabel = Atoms.ProfileLabel
      sharing ProfileExp = Atoms.ProfileExp
      sharing RealSize = Atoms.RealSize
      sharing RealX = Atoms.RealX
      sharing Record = Atoms.Record
      sharing SortedRecord = Atoms.SortedRecord
      sharing SourceInfo = Atoms.SourceInfo
      sharing Tycon = Atoms.Tycon
      (* sharing Tycons = Atoms.Tycons *)
      sharing Tyvar = Atoms.Tyvar
      (* sharing Tyvars = Atoms.Tyvars *)
      sharing Var = Atoms.Var
      sharing WordSize = Atoms.WordSize
      sharing WordX = Atoms.WordX
   end
