(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ElaboratePrograms (S: ELABORATE_PROGRAMS_STRUCTS): ELABORATE_PROGRAMS = 
struct

open S

structure ElaborateModules = ElaborateModules (structure Ast = Ast
                                               structure CoreML = CoreML
                                               structure Decs = Decs
                                               structure Env = Env)

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
