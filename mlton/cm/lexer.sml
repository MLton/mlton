(* Based on the file entity/lexer.sml in the SML/NJ CM sources. *)

(*
 * entity/lexer.sml: lexical analysis of description files
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
structure CMLexer: CM_LEXER = struct

    exception LexicalError of string * string
    exception UserError of string * string
    exception SyntaxError of string * string
    exception LexerBug

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

    datatype mode = NORMAL | PREPROC | MEMBERS | ERRORMSG

    fun lexer { strdef, sigdef, fctdef, fsigdef, symval } (fname, stream) = let

        fun lexerr s = raise LexicalError (fname, s)
        fun synerr s = raise SyntaxError (fname, s)
        fun usererr s = raise UserError (fname, s)

        val lookahead: char list ref = ref []

        fun getc () =
            case !lookahead of
                [] => let
                    val new = String.explode (In.input stream)
                in
                    case new of
                        [] => NONE
                      | h :: t => (lookahead := t; SOME h)
                end
              | h :: t => (lookahead := t; SOME h)

        fun ungetc c = (lookahead := (c :: (!lookahead)))

        fun skip_white mode = let

            fun skip_scheme_comment () =
                case getc () of
                    NONE => ()
                  | SOME #"\n" => (ungetc #"\n")
                  | _ => skip_scheme_comment ()

            fun skip_ml_comment () = let
                fun incomplete () = lexerr "incomplete ML-style comment"
            in
                case getc () of
                    SOME #"*" =>
                        (case getc () of
                             SOME #")" => ()
                           | NONE => incomplete ()
                           | SOME c => (ungetc c; skip_ml_comment ()))
                  | SOME #"(" => 
                        (case getc () of
                             SOME #"*" =>
                                 (skip_ml_comment (); skip_ml_comment ())
                           | NONE => incomplete ()
                           | SOME c => (ungetc c; skip_ml_comment ()))
                (*| SOME #";" => (skip_scheme_comment (); skip_ml_comment ())*)
                  | NONE => incomplete ()
                  | SOME _ => skip_ml_comment ()
            end

            fun skip () = let
                fun done () = ()
                fun preproc_nl thunk =
                    (if mode = PREPROC orelse mode = ERRORMSG then
                         ungetc #"\n"
                     else thunk ())
            in
                case getc () of
                    NONE => ()
                  | SOME #";" => (skip_scheme_comment (); skip ())
                  | SOME #"\n" =>
                        (case getc () of
                             NONE => preproc_nl done
                           | SOME #"#" => (ungetc #"#"; preproc_nl done)
                           | SOME c => (ungetc c; preproc_nl skip))
                  | SOME #"(" =>
                        (case getc () of
                             NONE => ungetc #"("
                           | SOME #"*" => (skip_ml_comment (); skip ())
                           | SOME c => (ungetc c; ungetc #"("))
                  | SOME c =>
                        if Char.isSpace c then skip () else ungetc c
            end
        in
            skip
        end

        fun rawlex mode = let

            val skip = skip_white mode

            fun getc_nonwhite () = (skip (); getc ())

            fun getnum c = let
                fun loop (n, c) = let
                    val n = 10 * n + Char.ord c - Char.ord #"0"
                in
                    case getc () of
                        NONE => n
                      | SOME c => if Char.isDigit c then loop (n, c)
                                  else (ungetc c; n)
                end
            in
                loop (0, c) handle Overflow => lexerr "arithmetic overflow"
            end

            fun expect (c, t) =
                if getc () = SOME c then t
                else lexerr (concat ["expecting ", String.implode [c]])

            fun ifnext (c, ty, tn) =
                case getc () of
                    NONE => tn
                  | SOME c1 =>
                        if c = c1 then ty else (ungetc c1; tn)

            fun getsym (c, delim) = let
                fun loop (accu, c) = let
                    val accu = c :: accu
                in
                    case getc () of
                        NONE => String.implode (rev accu)
                      | SOME c =>
                            if Char.isSpace c orelse String.contains(delim, c)
                               then (ungetc c; String.implode (rev accu))
                            else loop (accu, c)
                end
            in
                loop ([], c)
            end

            fun getline c = let
                fun loop accu =
                    case getc () of
                        NONE => String.implode (rev accu)
                      | SOME #"\n" => String.implode (rev accu)
                      | SOME c => loop (c :: accu)
            in
                loop [c]
            end

            val preproc_delim = "():;#+-*/%&!|><="
            val non_preproc_delim = "():;#"

            fun preproc_sym "if" = T_KEYWORD K_IF
              | preproc_sym "elif" = T_KEYWORD K_ELIF
              | preproc_sym "else" = T_KEYWORD K_ELSE
              | preproc_sym "endif" = T_KEYWORD K_ENDIF
              | preproc_sym "defined" = T_KEYWORD K_DEFINED
              | preproc_sym "structure" = T_KEYWORD K_STRUCTURE
              | preproc_sym "signature" = T_KEYWORD K_SIGNATURE
              | preproc_sym "functor" = T_KEYWORD K_FUNCTOR
              | preproc_sym "funsig" = T_KEYWORD K_FUNSIG
              | preproc_sym "error" = T_KEYWORD K_ERROR
              | preproc_sym s = T_SYMBOL s

            fun normal_sym "group" = T_KEYWORD K_GROUP
              | normal_sym "Group" = T_KEYWORD K_GROUP
              | normal_sym "GROUP" = T_KEYWORD K_GROUP
              | normal_sym "library" = T_KEYWORD K_LIBRARY
              | normal_sym "Library" = T_KEYWORD K_LIBRARY
              | normal_sym "LIBRARY" = T_KEYWORD K_LIBRARY
              | normal_sym "alias" = T_KEYWORD K_ALIAS
              | normal_sym "Alias" = T_KEYWORD K_ALIAS
              | normal_sym "ALIAS" = T_KEYWORD K_ALIAS
              | normal_sym "is" = T_KEYWORD K_IS
              | normal_sym "IS" = T_KEYWORD K_IS
              | normal_sym "structure" = T_KEYWORD K_STRUCTURE
              | normal_sym "signature" = T_KEYWORD K_SIGNATURE
              | normal_sym "functor" = T_KEYWORD K_FUNCTOR
              | normal_sym "funsig" = T_KEYWORD K_FUNSIG
              | normal_sym s = T_SYMBOL s

            fun string () = let
                fun collect l =
                    case getc () of
                        NONE => lexerr "missing string delimiter"
                      | SOME #"\"" =>
                            (case getc () of
                                 SOME #"\"" => collect (#"\"" :: l)
                               | SOME c => (ungetc c; String.implode (rev l))
                               | NONE => String.implode (rev l))
                      | SOME c => collect (c :: l)
            in
                collect []
            end

        in
          if mode = ERRORMSG then
              T_SYMBOL (case getc_nonwhite () of
                            NONE => "error"
                          | SOME #"\n" => "error"
                          | SOME c => getline c)
          else
            case getc_nonwhite () of
                NONE => T_EOF
              | SOME #":" => T_COLON
              | SOME #"\n" => T_NL
              | SOME #"#" => T_HASH
              | SOME #"\"" =>
                    (case mode of
                         NORMAL => T_STRING (string ())
                       | MEMBERS => T_STRING (string ())
                       | _ => 
                             synerr "quoted string in wrong context")
              | SOME c =>
                    if mode = PREPROC then
                        case c of
                            #"(" => T_LPAREN
                          | #")" => T_RPAREN
                          | #"+" => T_ARITH A_PLUS
                          | #"-" => T_ARITH A_MINUS
                          | #"*" => T_ARITH A_TIMES
                          | #"/" => T_ARITH A_DIV
                          | #"%" => T_ARITH A_MOD
                          | #"&" => expect (#"&", T_LCONN L_AND)
                          | #"|" => expect (#"|", T_LCONN L_OR)
                          | #"!" =>
                                ifnext (#"=", T_COMPARE C_NE, T_LCONN L_NOT)
                          | #">" =>
                                ifnext (#"=", T_COMPARE C_GE, T_COMPARE C_GT)
                          | #"<" =>
                                ifnext (#"=", T_COMPARE C_LE, T_COMPARE C_LT)
                          | #"=" => expect (#"=", T_COMPARE C_EQ)
                          | _ =>
                                if Char.isDigit c then
                                    T_NUMBER (getnum c)
                                else if Char.isAlpha c then
                                    preproc_sym (getsym (c, preproc_delim))
                                else
                                    synerr "illegal preprocessor line"
                    else let
                        val s = getsym (c, non_preproc_delim)
                    in
                        if mode = NORMAL then
                            normal_sym s
                        else
                            T_SYMBOL s
                    end
        end

        val lex = let

            val lookahead: token list ref = ref []

            fun gett () =
                case !lookahead of
                    [] => rawlex PREPROC
                  | (h :: t) => (lookahead := t; h)

            fun ungett t = lookahead := (t :: (!lookahead))

            fun leftrec (f, tokf) = let
                fun loop accu = let
                    val nt = gett ()
                in
                    case tokf nt of
                        NONE => (ungett nt; accu)
                      | SOME c => loop (c (accu, f ()))
                end
            in
                loop (f ())
            end

            fun nonassoc (f, tokf) = let
                val lhs = f ()
                val nt = gett ()
            in
                case tokf nt of
                    NONE => (ungett nt; lhs)
                  | SOME c => c (lhs, f ())
            end

            fun expect (t, m) =
                if gett () = t then () else synerr (concat ["missing ", m])

            fun intbool f (x: unit -> int, y: unit -> int) =
                fn () => if f (x (), y ()) then 1 else 0

            fun orf (x, y) =
                fn () => if (x () <> 0) orelse (y () <> 0) then 1 else 0
            fun andf (x, y) =
                fn () => if (x () <> 0) andalso (y () <> 0) then 1 else 0
            fun notf x = fn () => if x () <> 0 then 0 else 1
            val eqf = intbool (op =)
            val nef = intbool (op <>)
            val gtf = intbool (op >)
            val gef = intbool (op >=)
            val ltf = intbool (op <)
            val lef = intbool (op <=)

            fun binaryf binop (x: unit -> int, y: unit -> int) =
                fn () => (binop (x (), y ()))
            fun unaryf uop (x: unit -> int) =
                fn () => uop (x ())

            val plusf = binaryf (op +)
            val minusf = binaryf (op -)
            val timesf = binaryf (op * )
            val divf = binaryf (op div)
            val modf = binaryf (op mod)
            val negatef = unaryf ~

            fun expression () = disjunction ()

            and disjunction () = let
                fun tokf (T_LCONN L_OR) = SOME orf
                  | tokf _ = NONE
            in
                leftrec (conjunction, tokf)
            end

            and conjunction () = let
                fun tokf (T_LCONN L_AND) = SOME andf
                  | tokf _ = NONE
            in
                leftrec (equivalence, tokf)
            end

            and equivalence () = let
                fun tokf (T_COMPARE C_EQ) = SOME eqf
                  | tokf (T_COMPARE C_NE) = SOME nef
                  | tokf _ = NONE
            in
                nonassoc (comparison, tokf)
            end

            and comparison () = let
                fun tokf (T_COMPARE C_GT) = SOME gtf
                  | tokf (T_COMPARE C_GE) = SOME gef
                  | tokf (T_COMPARE C_LT) = SOME ltf
                  | tokf (T_COMPARE C_LE) = SOME lef
                  | tokf _ = NONE
            in
                nonassoc (sum, tokf)
            end

            and sum () = let
                fun tokf (T_ARITH A_PLUS) = SOME plusf
                  | tokf (T_ARITH A_MINUS) = SOME minusf
                  | tokf _ = NONE
            in
                leftrec (product, tokf)
            end

            and product () = let
                fun tokf (T_ARITH A_TIMES) = SOME timesf
                  | tokf (T_ARITH A_DIV) = SOME divf
                  | tokf (T_ARITH A_MOD) = SOME modf
                  | tokf _ = NONE
            in
                leftrec (unary, tokf)
            end

            and unary () =
                case gett () of
                    T_LCONN L_NOT => notf (unary ())
                  | T_ARITH A_MINUS => negatef (unary ())
                  | nt => (ungett nt; primary ())

            and primary () =
                case gett () of
                    T_LPAREN =>
                        expression ()
                        before expect (T_RPAREN, "right parenthesis")
                  | T_NUMBER n => (fn () => n)
                  | T_SYMBOL s =>
                        (fn () =>
                         (case symval s of
                              NONE => synerr (concat ["undefined symbol: ", s])
                            | SOME v => v))
                  | T_KEYWORD K_DEFINED => let
                        val _ = expect (T_LPAREN, "left parenthesis")
                    in
                        case gett () of
                            T_KEYWORD k => let
                                val look =
                                    case k of
                                        K_STRUCTURE => strdef
                                      | K_SIGNATURE => sigdef
                                      | K_FUNCTOR => fctdef
                                      | K_FUNSIG => fsigdef
                                      | _ => synerr "unexpected keyword"
                            in
                                case gett () of
                                    T_SYMBOL s =>
                                        (expect (T_RPAREN,
                                                 "right parenthesis");
                                         fn () => if look s then 1 else 0)
                                  | _ => synerr "missing symbol"
                            end
                          | T_SYMBOL s =>
                                (expect (T_RPAREN, "right parenthesis");
                                 fn () => (case symval s of
                                               NONE => 0
                                             | SOME _ => 1))
                          | _ => synerr "illegal `defined' construct"
                    end
                  | _ => synerr "unexpected token"

            datatype localstate =
                T_C | T | E_C | E

            datatype cmd =
                IF of unit -> int
              | ELIF of unit -> int
              | ELSE
              | ENDIF

            type state = localstate * bool

            fun iscopying s =
                case s of
                    [] => true
                  | (_, copying) :: _ => copying

            fun transform (IF c, s) =
                if iscopying s andalso c () <> 0 then
                    (T_C, true) :: s
                else
                    (T, false) :: s
              | transform (ELIF _, (T_C, _) :: s) = (T_C, false) :: s
              | transform (ELIF c, (T, _) :: s) =
                if iscopying s andalso c () <> 0 then
                    (T_C, true) :: s
                else
                    (T, false) :: s
              | transform (ELIF _, _) = synerr "unexpected #elif"
              | transform (ELSE, (T_C, _) :: s) = (E, false) :: s
              | transform (ELSE, (T, _) :: s) = (E_C, iscopying s) :: s
              | transform (ELSE, _) = synerr "unexpected #else"
              | transform (ENDIF, []) = synerr "unexpected #endif"
              | transform (ENDIF, _ :: s) = s

            val state: state list ref = ref []

            fun checklook () =
                case !lookahead of
                    [] => ()
                  | _ => raise LexerBug

            fun condition () = let
                val e = expression ()
            in
                fn () =>
                (e ()
                 handle Overflow => synerr "arithmetic overflow in condition"
                      | Div => synerr "divide by zero in condition")
            end

            fun nexttoken mode =
                case rawlex mode of
                    T_HASH =>
                        (case rawlex PREPROC of
                             T_KEYWORD K_IF => let
                                 val c = condition ()
                                 val _ = expect (T_NL, "line break (#if)")
                                 val _ = checklook ()
                             in
                                 state := transform (IF c, !state);
                                 nexttoken mode
                             end
                           | T_KEYWORD K_ELSE =>
                                 (expect (T_NL, "line break (#else)");
                                  checklook ();
                                  state := transform (ELSE, !state);
                                  nexttoken mode)
                           | T_KEYWORD K_ELIF => let
                                 val c = condition ()
                                 val _ = expect (T_NL, "line break (#elif)")
                                 val _ = checklook ()
                             in
                                 state := transform (ELIF c, !state);
                                 nexttoken mode
                             end
                           | T_KEYWORD K_ENDIF =>
                                 (expect (T_NL, "line break (#endif)");
                                  checklook ();
                                  state := transform (ENDIF, !state);
                                  nexttoken mode)
                           | T_KEYWORD K_ERROR => let
                                 val msg =
                                     case rawlex ERRORMSG of
                                         T_SYMBOL msg => msg
                                       | _ => raise LexerBug
                             in
                                 if iscopying (!state) then
                                     usererr msg
                                 else
                                     (checklook (); nexttoken mode)
                             end
                           | _ => synerr "illegal preprocessor line")
                  | T_EOF =>
                        if (!state) = [] then T_EOF
                        else synerr "missing #endif"
                  | t => if iscopying (!state) then t else nexttoken mode

        in
            nexttoken
        end

    in
        lex
    end
end
