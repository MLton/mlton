(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor AstConst(S: AST_CONST_STRUCTS) :> AST_CONST =
struct
	 
datatype t =
   Char of char
 | Int of string
 | Real of string
 | String of string
 | Word of word

val equals = fn _ => Error.unimplemented "Ast.Const.equals"

val fromInt = Int o Int.toString

local
   open Layout
   fun wrap(pre, post, s) = seq[str pre, String.layout s, str post]
in
   fun layout c =
      case c of
	 Char c => wrap("#\"", "\"", String.implode[c])
       | Int s => str s
       | Real l => String.layout l
       | String s => wrap("\"", "\"", s)
       | Word w => seq[str "0wx", str(Word.toString w)]
end

end
