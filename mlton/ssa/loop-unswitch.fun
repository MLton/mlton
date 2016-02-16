(* Copyright (C) 1999-2005, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Moves a conditional statement outside a loop by duplicating the loops body
 * under each branch of the conditional.
 *)
functor LoopUnswitch(S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM = 
struct

open S

fun transform (Program.T {datatypes, globals, functions, main}) =
   let
      val () = print "Unswitching loops\n"
   in
      Program.T {datatypes = datatypes,
                 globals = globals,
                 functions = functions,
                 main = main}
   end

end
