(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor AstConst (S: AST_CONST_STRUCTS): AST_CONST =
struct

open S Region.Wrap

datatype node =
   Bool of bool
 | Char of IntInf.t
 | Int of IntInf.t
 | Real of string
 | String of IntInf.t vector
 | Word of IntInf.t
type t = node Region.Wrap.t
type node' = node
type obj = t

fun ordToString (c: IntInf.t): string =
      let
         fun loop (n: int, c: IntInf.t, ac: char list) =
            if n = 0
               then implode ac
            else
               let
                  val (q, r) = IntInf.quotRem (c, 0x10)
               in
                  loop (n - 1, q, Char.fromHexDigit (Int.fromIntInf r) :: ac)
               end
         fun doit (n, esc) = concat ["\\", esc, loop (n, c, [])]
      in
         if c <= 0xFF
            then Char.escapeSML (Char.fromInt (Int.fromIntInf c))
         else if c <= 0xFFFF
            then doit (4, "u")
         else doit (8, "U")
      end

local
   open Layout
in
   fun layout c =
      case node c of
         Bool b => if b then str "true" else str "false"
       | Char c => str (concat ["#\"", ordToString c, "\""])
       | Int s => str (IntInf.toString s)
       | Real l => String.layout l
       | String s =>
            str (concat ["\"", concat (Vector.toListMap (s, ordToString)), "\""])
       | Word w => str (concat ["0wx", IntInf.format (w, StringCvt.HEX)])
end

end
