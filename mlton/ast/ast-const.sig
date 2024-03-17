(* Copyright (C) 2024 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature AST_CONST_STRUCTS =
   sig
   end

signature AST_CONST =
   sig
      include AST_CONST_STRUCTS

      type t
      datatype value =
         Bool of bool
       | Char of IntInf.t
       | Int of IntInf.t
       | Real of string
       | String of {char: IntInf.t, yytext: string} vector
       | Word of IntInf.t
      datatype node = Node of {value: value, yytext: string}
      include WRAPPED sharing type node' = node
                      sharing type obj = t

      val layout: t -> Layout.t
      val value: t -> value
   end
