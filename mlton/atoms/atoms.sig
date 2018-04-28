(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
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

      sharing AdmitsEquality = Tycon.AdmitsEquality
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
      sharing TyconKind = Tycon.Kind
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
      sharing AdmitsEquality = Atoms.AdmitsEquality
      sharing CFunction = Atoms.CFunction
      sharing CType = Atoms.CType
      sharing CharSize = Atoms.CharSize
      sharing Con = Atoms.Con
      sharing Const = Atoms.Const
      sharing Ffi = Atoms.Ffi
      sharing Field = Atoms.Field
      sharing Func = Atoms.Func
      sharing IntSize = Atoms.IntSize
      sharing Label = Atoms.Label
      sharing Prim = Atoms.Prim
      sharing ProfileExp = Atoms.ProfileExp
      sharing ProfileLabel = Atoms.ProfileLabel
      sharing RealSize = Atoms.RealSize
      sharing RealX = Atoms.RealX
      sharing Record = Atoms.Record
      sharing SortedRecord = Atoms.SortedRecord
      sharing SourceInfo = Atoms.SourceInfo
      sharing Symbol = Atoms.Symbol
      sharing Tycon = Atoms.Tycon
      sharing TyconKind = Atoms.TyconKind
      sharing Tyvar = Atoms.Tyvar
      sharing Var = Atoms.Var
      sharing WordSize = Atoms.WordSize
      sharing WordX = Atoms.WordX
      sharing WordXVector = Atoms.WordXVector
   end
