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
      structure Var = Var (structure AstId = Ast.Var)
      structure Tycon = Tycon (structure AstId = Ast.Tycon
			       structure IntSize = IntSize
			       structure RealSize = RealSize
			       structure WordSize = WordSize)
      fun f (x: IntSize.t): Tycon.IntSize.t = x
      structure Type =
	 Type (structure Ast = Ast
	       structure IntSize = IntSize
	       structure Record = Ast.SortedRecord
	       structure Tyvar = Ast.Tyvar
	       structure Tycon = Tycon
	       structure WordSize = WordSize)
      structure Scheme: SCHEME =
	 struct
	    structure Arg =
	       struct
		  structure Tycon = Tycon
		  structure Tyvar = Ast.Tyvar
		  structure Type = Type
	       end
	    structure S = GenericScheme (Arg)
	    open S Arg
	 end
      structure Con = Con (structure AstId = Ast.Con
			  structure Var = Var)
      structure CType = CType (structure IntSize = IntSize
			       structure RealSize = RealSize
			       structure WordSize = WordSize)
      structure CFunction = CFunction (structure CType = CType)
      structure Ffi = Ffi (structure CFunction = CFunction
			   structure CType = CType)
      structure IntX = IntX (structure IntSize = IntSize)
      structure RealX = RealX (structure RealSize = RealSize)
      structure WordX = WordX (structure WordSize = WordSize)
      structure Const = Const (structure Ast = Ast
			       structure IntX = IntX
			       structure RealX = RealX
			       structure WordX = WordX)
      structure Prim = Prim (structure CFunction = CFunction
			     structure CType = CType
			     structure Con = Con
			     structure Const = Const
			     structure IntSize = IntSize
			     structure Longid = Ast.Longvid
			     structure RealSize = RealSize
			     structure Scheme = Scheme
			     structure Type = Type
			     structure WordSize = WordSize)
      structure Record = Ast.Record
      structure SortedRecord = Ast.SortedRecord
      structure Tyvar = Ast.Tyvar
      structure Tyvars = UnorderedSet (Tyvar)
      structure Vars = UnorderedSet (Var)
      structure Cons = UnorderedSet (Con)
      structure Tycons = UnorderedSet (Tycon)
   end

open Atoms

end
