(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor Atoms (S: ATOMS_STRUCTS): ATOMS =
struct

structure Atoms =
   struct
      open S

      structure SourceInfo = SourceInfo ()
      structure ProfileExp = ProfileExp (structure SourceInfo = SourceInfo)
      structure Var = Var (structure Symbol = Symbol)
      structure Tycon = Tycon (structure IntSize = IntSize
			       structure RealSize = RealSize
			       structure Symbol = Symbol
			       structure WordSize = WordSize)
      structure Con = Con (structure Symbol = Symbol)
      structure CType = CType (structure IntSize = IntSize
			       structure RealSize = RealSize
			       structure WordSize = WordSize)
      structure CFunction = CFunction (structure CType = CType)
      structure Ffi = Ffi (structure CFunction = CFunction
			   structure CType = CType)
      structure IntX = IntX (structure IntSize = IntSize)
      structure RealX = RealX (structure RealSize = RealSize)
      structure WordX = WordX (structure WordSize = WordSize)
      structure Const = Const (structure IntX = IntX
			       structure RealX = RealX
			       structure WordX = WordX)
      structure Prim = Prim (structure CFunction = CFunction
			     structure CType = CType
			     structure Con = Con
			     structure Const = Const
			     structure IntSize = IntSize
			     structure RealSize = RealSize
			     structure WordSize = WordSize)
      structure Tyvars = UnorderedSet (Tyvar)
      structure Vars = UnorderedSet (Var)
      structure Cons = UnorderedSet (Con)
      structure Tycons = UnorderedSet (Tycon)
   end

open Atoms

end
