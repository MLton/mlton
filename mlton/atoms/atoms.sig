(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
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
      structure Func: FUNC
      structure IntX: INT_X
      structure Label: LABEL
      structure ObjectType: OBJECT_TYPE
      structure PointerTycon: POINTER_TYCON
      structure Prim: PRIM
      structure ProfileLabel: PROFILE_LABEL
      structure RepType: REP_TYPE
      structure ProfileExp: PROFILE_EXP
      structure RealX: REAL_X
      structure Runtime: RUNTIME
      structure SourceInfo: SOURCE_INFO
      structure Tycon: TYCON
      structure Tycons: SET
      structure Var: VAR
      structure Vars: SET
      structure Tyvars: SET
      structure WordX: WORD_X

      sharing CFunction = Ffi.CFunction = Prim.CFunction
      sharing CType = Ffi.CType = Prim.CType = RepType.CType
      sharing Con = Prim.Con
      sharing Const = Prim.Const
      sharing IntSize = IntX.IntSize = Prim.IntSize = RepType.IntSize
	 = Tycon.IntSize
      sharing IntX = Const.IntX = RepType.IntX
      sharing Label = RepType.Label
      sharing ObjectType = RepType.ObjectType
      sharing PointerTycon = ObjectType.PointerTycon = RepType.PointerTycon
      sharing RealSize = Prim.RealSize = RealX.RealSize = RepType.RealSize
	 = Tycon.RealSize
      sharing RepType = CFunction.RepType = Prim.RepType
      sharing RealX = Const.RealX
      sharing Runtime = ObjectType.Runtime = RepType.Runtime
      sharing SourceInfo = ProfileExp.SourceInfo
      sharing WordSize = Prim.WordSize = RepType.WordSize = Tycon.WordSize
	 = WordX.WordSize
      sharing WordX = Const.WordX = RepType.WordX
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
      sharing Cons = Atoms.Cons
      sharing Const = Atoms.Const
      sharing Ffi = Atoms.Ffi
      sharing Field = Atoms.Field
      sharing Func = Atoms.Func
      sharing IntSize = Atoms.IntSize
      sharing IntX = Atoms.IntX
      sharing Label = Atoms.Label
      sharing ObjectType = Atoms.ObjectType
      sharing PointerTycon = Atoms.PointerTycon
      sharing Prim = Atoms.Prim
      sharing ProfileLabel = Atoms.ProfileLabel
      sharing ProfileExp = Atoms.ProfileExp
      sharing RealSize = Atoms.RealSize
      sharing RealX = Atoms.RealX
      sharing Record = Atoms.Record
      sharing RepType = Atoms.RepType
      sharing Runtime = Atoms.Runtime
      sharing SortedRecord = Atoms.SortedRecord
      sharing SourceInfo = Atoms.SourceInfo
      sharing Tycon = Atoms.Tycon
      sharing Tycons = Atoms.Tycons
      sharing Tyvar = Atoms.Tyvar
      sharing Tyvars = Atoms.Tyvars
      sharing Var = Atoms.Var
      sharing Vars = Atoms.Vars
      sharing WordSize = Atoms.WordSize
      sharing WordX = Atoms.WordX
   end
