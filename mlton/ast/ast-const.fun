(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor AstConst (S: AST_CONST_STRUCTS) :> AST_CONST =
struct

open Region.Wrap
datatype node =
   Char of char
 | Int of string
 | Real of string
 | String of string
 | Word of word
type t = node Region.Wrap.t
type node' = node
type obj = t

val equals = fn _ => Error.unimplemented "Ast.Const.equals"

local
   open Layout
   fun wrap (pre, post, s) = seq [str pre, String.layout s, str post]
in
   fun layout c =
      case node c of
	 Char c => wrap ("#\"", "\"", String.implode [c])
       | Int s => str s
       | Real l => String.layout l
       | String s => wrap ("\"", "\"", s)
       | Word w => seq [str "0wx", str (Word.toString w)]
end

val toString = Layout.toString o layout

end
