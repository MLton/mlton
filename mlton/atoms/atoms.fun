(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
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

      structure ProfileLabel = ProfileLabel ()
      structure SourceInfo = SourceInfo ()
      structure ProfileExp = ProfileExp (structure SourceInfo = SourceInfo)
      structure Var = Var ()
      structure Tycon = Tycon (structure IntSize = IntSize
			       structure RealSize = RealSize
			       structure WordSize = WordSize)
      structure Con = Con ()
      structure CType = CType ()
      structure RealX = RealX (structure RealSize = RealSize)
      structure WordX = WordX (structure WordSize = WordSize)
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
			       structure WordX = WordX)
      structure CFunction = CFunction ()
      structure Prim = Prim (structure CFunction = CFunction
			     structure CType = CType
			     structure Con = Con
			     structure Const = Const
			     structure RealSize = RealSize
			     structure WordSize = WordSize)
      structure Ffi = Ffi (structure CFunction = CFunction
			   structure CType = CType)
      structure Tyvars = UnorderedSet (Tyvar)
      structure Vars = UnorderedSet (Var)
      structure Cons = UnorderedSet (Con)
      structure Tycons = UnorderedSet (Tycon)
   end

open Atoms

end
