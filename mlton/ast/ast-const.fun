(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor AstConst (S: AST_CONST_STRUCTS): AST_CONST =
struct

open Region.Wrap
datatype node =
   Bool of bool
 | Char of char
 | Int of IntInf.t
 | Real of string
 | String of string
 | Word of IntInf.t
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
	 Bool b => if b then str "true" else str "false"
       | Char c => wrap ("#\"", "\"", String.implode [c])
       | Int s => str (IntInf.toString s)
       | Real l => String.layout l
       | String s => wrap ("\"", "\"", s)
       | Word w => str (concat ["0wx", IntInf.format (w, StringCvt.HEX)])
end

val toString = Layout.toString o layout

end
