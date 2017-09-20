(* Copyright (C) 2017 James Reilly.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ParseSsa (S: PARSE_SSA_STRUCTS): PARSE_SSA =
struct
   open S
   open SsaTree
   structure T = StreamParser
   open T.Ops
   infix 1 <|> >>=
   infix 2 <&>
   infix  3 <*> <* *>
   infixr 4 <$> <$$> <$$$> <$
   
   fun makeProgram() =
      Program.T
         {datatypes = Vector.new0(),
          functions = [],
          globals = Vector.new0(),
          main = Func.newNoname() } 
   
   fun parse s = makeProgram()
      
end
