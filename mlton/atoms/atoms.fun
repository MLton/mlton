(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Atoms (S: ATOMS_STRUCTS): ATOMS =
struct

structure Atoms =
   struct
      open S

      structure Var = Var (structure AstId = Ast.Var)
      structure Tycon = Tycon (structure AstId = Ast.Tycon)
      structure UnaryTycon = UnaryTycon (structure Tycon = Tycon)
      structure Type =
	 Type (structure Ast = Ast
	      structure Record = Ast.SortedRecord
	      structure Tyvar = Ast.Tyvar
	      structure Tycon = Tycon)
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
      structure Const = Const (structure Ast = Ast
			      structure Tycon = Tycon)
      structure Prim = Prim (structure Con = Con
			     structure Const = Const
			     structure Longid = Ast.Longvid
			     structure Type = Type
			     structure Scheme = Scheme)
      structure Record = Ast.Record
      structure SortedRecord = Ast.SortedRecord
      structure Tyvar = Ast.Tyvar
      structure Tyvars = UnorderedSet (Tyvar)
      structure Vars = UnorderedSet (Var)
      structure Cons = UnorderedSet (Con)
      structure Tycons = UnorderedSet (Tycon)
      structure TyvarEnv =
	 struct
	    structure Env = MonoEnv (structure Domain = UseName (Tyvar)
				    structure Range = Tyvar)
	    open Env

	    fun rename (env: t, tyvars: Tyvar.t vector): t * Tyvar.t vector =
	       let
		  val (tyvars, env) =
		     Vector.mapAndFold
		     (tyvars, env, fn (tyv, env) =>
		      let
			 val tyv' =
			    Tyvar.newNoname {equality = Tyvar.isEquality tyv}
		      in (tyv', extend (env, tyv, tyv'))
		      end)
	       in (env, tyvars)
	       end
	 end
   end

open Atoms

end
