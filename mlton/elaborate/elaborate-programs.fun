(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor ElaboratePrograms (S: ELABORATE_PROGRAMS_STRUCTS): ELABORATE_PROGRAMS = 
struct

open S

structure ElaborateModules = ElaborateModules (structure Ast = Ast
					       structure ConstType = ConstType
					       structure CoreML = CoreML
					       structure Decs = Decs
					       structure Env = Env)
val lookupConstant = ElaborateModules.lookupConstant

fun elaborateProgram (program, {env = E: Env.t}) =
   let
      val Ast.Program.T decs = Ast.Program.coalesce program 
      fun elabTopdec d = ElaborateModules.elaborateTopdec (d, {env = E})
   in
      List.fold (decs, Decs.empty, fn (ds, decs) =>
		 List.fold (ds, decs, fn (d, decs) =>
			    Decs.append (decs, elabTopdec d)))
   end

end
