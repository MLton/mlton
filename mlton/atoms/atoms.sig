(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature ATOMS_STRUCTS =
   sig
      structure Ast: AST
      structure IntSize: INT_SIZE
      structure RealSize: REAL_SIZE
      structure WordSize: WORD_SIZE
   end

signature ATOMS' =
   sig
      include ATOMS_STRUCTS

      structure Con: CON
      structure Cons: SET
      structure Const: CONST
      structure IntX: INT_X
      structure Prim: PRIM 
      structure ProfileExp: PROFILE_EXP
      structure RealX: REAL_X
      structure Record: RECORD
      structure Scheme: SCHEME
      structure SortedRecord: RECORD
      structure SourceInfo: SOURCE_INFO
      structure Tycon: TYCON
      structure Tycons: SET
      structure Tyvar: TYVAR
      structure Var: VAR
      structure Vars: SET
      structure TyvarEnv:
	 sig
	    include MONO_ENV 

	    (* rename (env, tyvars) extends env by mapping each tyvar to
	     * a new tyvar (with the same equality property).  It returns
	     * the extended environment and the list of new tyvars
	     *)
            val rename: t * Tyvar.t vector -> t * Tyvar.t vector
	 end
      structure Tyvars: SET
      structure WordX: WORD_X

      sharing Ast = Const.Ast = Prim.Type.Ast
      sharing Ast.Con = Con.AstId
      sharing Ast.Tycon = Tycon.AstId
      sharing Ast.Tyvar = Scheme.Tyvar
      sharing Ast.Var = Var.AstId
      sharing Con = Prim.Con
      sharing Const = Prim.Const
      sharing IntSize = IntX.IntSize = Prim.IntSize = Tycon.IntSize
      sharing IntX = Const.IntX
      sharing RealSize = Prim.RealSize = RealX.RealSize = Tycon.RealSize
      sharing RealX = Const.RealX
      sharing Record = Ast.Record
      sharing Scheme = Prim.Scheme
      sharing SortedRecord = Ast.SortedRecord
      sharing SourceInfo = ProfileExp.SourceInfo
      sharing Tycon = Scheme.Tycon
      sharing Tyvar = Ast.Tyvar
      sharing WordSize = Prim.WordSize = Tycon.WordSize = WordX.WordSize
      sharing WordX = Const.WordX
      sharing type Con.t = Cons.Element.t
      sharing type Tycon.t = Tycons.Element.t
      sharing type Tyvar.t = TyvarEnv.Domain.t
      sharing type Tyvar.t = TyvarEnv.Range.t
      sharing type Tyvar.t = Tyvars.Element.t
      sharing type Var.t = Vars.Element.t
   end

signature ATOMS =
   sig
      structure Atoms: ATOMS'
	 
      include ATOMS'

      sharing Ast = Atoms.Ast
      sharing Const = Atoms.Const
      sharing Var = Atoms.Var
      sharing Con = Atoms.Con
      sharing Prim = Atoms.Prim
      sharing ProfileExp = Atoms.ProfileExp
      sharing Tycon = Atoms.Tycon
      sharing Tyvar = Atoms.Tyvar
      sharing Record = Atoms.Record
      sharing SourceInfo = Atoms.SourceInfo
      sharing Vars = Atoms.Vars
      sharing Cons = Atoms.Cons
      sharing Tycons = Atoms.Tycons
      sharing Tyvars = Atoms.Tyvars
   end
