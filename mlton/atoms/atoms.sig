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
   end

signature ATOMS' =
   sig
      include ATOMS_STRUCTS

      structure Con: CON
      structure Cons: SET
      structure Const: CONST
      structure Prim: PRIM 
      structure ProfileExp: PROFILE_EXP
      structure Record: RECORD
      structure Scheme: SCHEME
      structure SortedRecord: RECORD
      structure SourceInfo: SOURCE_INFO
      structure Tycon: TYCON
      structure Tycons: SET
      structure Tyvar: TYVAR
      structure UnaryTycon: UNARY_TYCON
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

      sharing Ast = Const.Ast = Prim.Type.Ast
      sharing Ast.Con = Con.AstId
      sharing Ast.Tycon = Tycon.AstId
      sharing Ast.Tyvar = Scheme.Tyvar
      sharing Ast.Var = Var.AstId
      sharing Con = Prim.Con
      sharing Const = Prim.Const
      sharing Record = Ast.Record
      sharing Scheme = Prim.Scheme
      sharing SortedRecord = Ast.SortedRecord
      sharing SourceInfo = ProfileExp.SourceInfo
      sharing Tycon = Const.Tycon
      sharing Tycon = Scheme.Tycon
      sharing Tycon = UnaryTycon.Tycon
      sharing Tyvar = Ast.Tyvar
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
