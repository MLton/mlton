(* Copyright (C) 2024 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor AstConst (S: AST_CONST_STRUCTS): AST_CONST =
struct

open S Region.Wrap

datatype value =
   Bool of bool
 | Char of IntInf.t
 | Int of IntInf.t
 | Real of string
 | String of {char: IntInf.t, yytext: string} vector
 | Word of IntInf.t
datatype node = Node of {value: value, yytext: string}
type t = node Region.Wrap.t
type node' = node
type obj = t

local
   fun mk sel c = let val Node r = node c in sel r end
in
   val value = mk #value
   val yytext = mk #yytext
end

fun layout (c: t) = Layout.str (yytext c)

end
