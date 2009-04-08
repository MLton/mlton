(* Based on the file entity/lexer.sig in the SML/NJ CM sources. *)

(*
 * entity/lexer.sig: lexical analysis of description files
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature CM_LEXER = sig

    exception LexicalError of string * string
    exception SyntaxError of string * string
    exception UserError of string * string

    datatype keyword =
        K_GROUP | K_LIBRARY | K_ALIAS | K_IS
      | K_SIGNATURE | K_STRUCTURE | K_FUNSIG | K_FUNCTOR
      | K_IF | K_ELIF | K_ELSE | K_ENDIF | K_DEFINED
      | K_ERROR

    datatype lconn = L_AND | L_OR | L_NOT

    datatype arith = A_PLUS | A_MINUS | A_TIMES | A_DIV | A_MOD

    datatype compare = C_LT | C_LE | C_GT | C_GE | C_EQ | C_NE

    datatype token =
        T_COLON
      | T_HASH
      | T_KEYWORD of keyword
      | T_SYMBOL of string
      | T_STRING of string
      | T_NUMBER of int
      | T_LPAREN
      | T_RPAREN
      | T_ARITH of arith
      | T_LCONN of lconn
      | T_COMPARE of compare
      | T_NL
      | T_EOF

    type mode

    val NORMAL: mode
    val MEMBERS: mode

    val lexer: {
                strdef: string -> bool,
                sigdef: string -> bool,
                fctdef: string -> bool,
                fsigdef: string -> bool,
                symval: string -> int option
               } ->
        string * In.t -> mode -> token

end
