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

      structure Con: CON sharing Con.AstId = Ast.Con
      structure Const: CONST
      structure Prim: PRIM sharing Con = Prim.Con sharing Const = Prim.Const
      structure Tycon: TYCON sharing Tycon.AstId = Ast.Tycon
      structure UnaryTycon: UNARY_TYCON sharing Tycon = UnaryTycon.Tycon
      structure Scheme: SCHEME
      structure Var: VAR sharing Var.AstId = Ast.Var
      sharing Tycon = Const.Tycon
      sharing Ast = Const.Ast = Prim.Type.Ast
      sharing Tycon = Scheme.Tycon
      sharing Ast.Tyvar = Scheme.Tyvar
      sharing Scheme = Prim.Scheme

      structure Record: RECORD
      sharing Record = Ast.Record
      structure SortedRecord: RECORD
      sharing SortedRecord = Ast.SortedRecord

      structure Tyvar: TYVAR
      sharing Tyvar = Ast.Tyvar

      structure Tyvars: SET sharing type Tyvars.Element.t = Tyvar.t
      structure Cons: SET sharing type Cons.Element.t = Con.t
      structure Vars: SET sharing type Vars.Element.t = Var.t
      structure Tycons: SET sharing type Tycons.Element.t = Tycon.t

      structure TyvarEnv:
	 sig
	    include MONO_ENV 

	    (* rename (env, tyvars) extends env by mapping each tyvar to
	     * a new tyvar (with the same equality property).  It returns
	     * the extended environment and the list of new tyvars
	     *)
            val rename: t * Tyvar.t vector -> t * Tyvar.t vector
	 end
      sharing type TyvarEnv.Domain.t = Tyvar.t
      sharing type TyvarEnv.Range.t = Tyvar.t
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
      sharing Tycon = Atoms.Tycon
      sharing Tyvar = Atoms.Tyvar
      sharing Record = Atoms.Record
      sharing Vars = Atoms.Vars
      sharing Cons = Atoms.Cons
      sharing Tycons = Atoms.Tycons
      sharing Tyvars = Atoms.Tyvars
   end
