(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor AstConst (S: AST_CONST_STRUCTS): AST_CONST =
struct

open S Region.Wrap
   
datatype node =
   Bool of bool
 | Char of IntInf.t
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
       | Char c =>
	    let
	       fun loop (n: int, c: IntInf.t, ac: char list) =
		  if n = 0
		     then implode (rev ac)
		  else
		     let
			val (q, r) = IntInf.quotRem (c, 0x10)
		     in
			loop (n - 1, q,
			      Char.fromHexDigit (Int.fromIntInf r) :: ac)
		     end
	       fun doit (n, esc) = concat ["\\", esc, loop (n, c, [])]
	    in
	       wrap ("#\"", "\"",
		     if c <= 0xFF
			then Char.escapeSML (Char.fromInt (Int.fromIntInf c))
		     else if c <= 0xFFFF
			     then doit (4, "u")
			  else doit (8, "U"))
	    end
       | Int s => str (IntInf.toString s)
       | Real l => String.layout l
       | String s => wrap ("\"", "\"", s)
       | Word w => str (concat ["0wx", IntInf.format (w, StringCvt.HEX)])
end

val toString = Layout.toString o layout

end
