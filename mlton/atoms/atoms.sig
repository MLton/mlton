(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature ATOMS_STRUCTS =
   sig
      structure Field: FIELD
      structure IntSize: INT_SIZE
      structure RealSize: REAL_SIZE
      structure Record: RECORD
      structure SortedRecord: RECORD
      structure Symbol: SYMBOL
      structure Tyvar: TYVAR
      structure WordSize: WORD_SIZE
      sharing Field = Record.Field = SortedRecord.Field
   end

signature ATOMS' =
   sig
      include ATOMS_STRUCTS

      structure CFunction: C_FUNCTION
      structure CType: C_TYPE
      structure Con: CON
      structure Cons: SET
      structure Const: CONST
      structure Ffi: FFI
      structure IntX: INT_X
      structure Prim: PRIM 
      structure ProfileExp: PROFILE_EXP
      structure RealX: REAL_X
      structure SourceInfo: SOURCE_INFO
      structure Tycon: TYCON
      structure Tycons: SET
      structure Var: VAR
      structure Vars: SET
      structure Tyvars: SET
      structure WordX: WORD_X

      sharing CFunction = Ffi.CFunction = Prim.CFunction
      sharing CFunction.CType = CType = Ffi.CType = Prim.CType
      sharing Con = Prim.Con
      sharing Const = Prim.Const
      sharing Field = Record.Field = SortedRecord.Field
      sharing IntSize = CType.IntSize = IntX.IntSize = Prim.IntSize =
	 Tycon.IntSize
      sharing IntX = Const.IntX
      sharing RealSize = CType.RealSize = Prim.RealSize = RealX.RealSize
	 = Tycon.RealSize
      sharing RealX = Const.RealX
      sharing SourceInfo = ProfileExp.SourceInfo
      sharing WordSize = CType.WordSize = Prim.WordSize = Tycon.WordSize
	 = WordX.WordSize
      sharing WordX = Const.WordX
      sharing type Con.t = Cons.Element.t
      sharing type Tycon.t = Tycons.Element.t
      sharing type Tyvar.t = Tyvars.Element.t
      sharing type Var.t = Vars.Element.t
   end

signature ATOMS =
   sig
      structure Atoms: ATOMS'
	 
      include ATOMS'

      sharing CFunction = Atoms.CFunction
      sharing CType = Atoms.CType
      sharing Con = Atoms.Con
      sharing Cons = Atoms.Cons
      sharing Const = Atoms.Const
      sharing Ffi = Atoms.Ffi
      sharing Field = Atoms.Field
      sharing IntSize = Atoms.IntSize
      sharing IntX = Atoms.IntX
      sharing Prim = Atoms.Prim
      sharing ProfileExp = Atoms.ProfileExp
      sharing RealSize = Atoms.RealSize
      sharing RealX = Atoms.RealX
      sharing Record = Atoms.Record
      sharing SortedRecord = Atoms.SortedRecord
      sharing SourceInfo = Atoms.SourceInfo
(*      sharing Symbol = Con.Symbol = Tycon.Symbol = Var.Symbol *)
      sharing Tycon = Atoms.Tycon
      sharing Tycons = Atoms.Tycons
      sharing Tyvar = Atoms.Tyvar
      sharing Tyvars = Atoms.Tyvars
      sharing Var = Atoms.Var
      sharing Vars = Atoms.Vars
      sharing WordSize = Atoms.WordSize
      sharing WordX = Atoms.WordX
   end
