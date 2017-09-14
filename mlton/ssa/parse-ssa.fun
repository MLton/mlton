(* Copyright (C) 2017 James Reilly.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ParseSsa (S: PARSE_SSA_STRUCTS): PARSE_SSA =
struct
   fun parse s = 
      Program.T
         {datatypes = Vector.new0(),
          functions = [],
          globals = Vector.new0(),
          main = Null} 
end
