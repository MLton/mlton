(* Modified by mfluet@acm.org on 2005-8-01.
 * Update with SML/NJ 110.55+.
 *)
(* Modified by sweeks@acm.org on 2000-8-24.
 * Ported to MLton.
 *)
type int = Int.int

(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

(* base.sig: Base signature file for SML-Yacc.  This file contains signatures
   that must be loaded before any of the files produced by ML-Yacc are loaded
*)

(* STREAM: signature for a lazy stream.*)

signature STREAM =
 sig type 'xa stream
     val streamify : (unit -> '_a) -> '_a stream
     val cons : '_a * '_a stream -> '_a stream
     val get : '_a stream -> '_a * '_a stream
 end

(* LR_TABLE: signature for an LR Table.

   The list of actions and gotos passed to mkLrTable must be ordered by state
   number. The values for state 0 are the first in the list, the values for
    state 1 are next, etc.
*)

signature LR_TABLE =
    sig
        datatype ('a,'b) pairlist = EMPTY | PAIR of 'a * 'b * ('a,'b) pairlist
        datatype state = STATE of int
        datatype term = T of int
        datatype nonterm = NT of int
        datatype action = SHIFT of state
                        | REDUCE of int
                        | ACCEPT
                        | ERROR
        type table
        
        val numStates : table -> int
        val numRules : table -> int
        val describeActions : table -> state ->
                                (term,action) pairlist * action
        val describeGoto : table -> state -> (nonterm,state) pairlist
        val action : table -> state * term -> action
        val goto : table -> state * nonterm -> state
        val initialState : table -> state
        exception Goto of state * nonterm

        val mkLrTable : {actions : ((term,action) pairlist * action) array,
                         gotos : (nonterm,state) pairlist array,
                         numStates : int, numRules : int,
                         initialState : state} -> table
    end

(* TOKEN: signature revealing the internal structure of a token. This signature
   TOKEN distinct from the signature {parser name}_TOKENS produced by ML-Yacc.
   The {parser name}_TOKENS structures contain some types and functions to
    construct tokens from values and positions.

   The representation of token was very carefully chosen here to allow the
   polymorphic parser to work without knowing the types of semantic values
   or line numbers.

   This has had an impact on the TOKENS structure produced by SML-Yacc, which
   is a structure parameter to lexer functors.  We would like to have some
   type 'a token which functions to construct tokens would create.  A
   constructor function for a integer token might be

          INT: int * 'a * 'a -> 'a token.
 
   This is not possible because we need to have tokens with the representation
   given below for the polymorphic parser.

   Thus our constructur functions for tokens have the form:

          INT: int * 'a * 'a -> (svalue,'a) token

   This in turn has had an impact on the signature that lexers for SML-Yacc
   must match and the types that a user must declare in the user declarations
   section of lexers.
*)

signature TOKEN =
    sig
        structure LrTable : LR_TABLE
        datatype ('a,'b) token = TOKEN of LrTable.term * ('a * 'b * 'b)
        val sameToken : ('a,'b) token * ('a,'b) token -> bool
    end

(* LR_PARSER: signature for a polymorphic LR parser *)

signature LR_PARSER =
    sig
        structure Stream: STREAM
        structure LrTable : LR_TABLE
        structure Token : TOKEN

        sharing LrTable = Token.LrTable

        exception ParseError

        val parse : {table : LrTable.table,
                     lexer : ('_b,'_c) Token.token Stream.stream,
                     arg: 'arg,
                     saction : int *
                               '_c *
                                (LrTable.state * ('_b * '_c * '_c)) list * 
                                'arg ->
                                     LrTable.nonterm *
                                     ('_b * '_c * '_c) *
                                     ((LrTable.state *('_b * '_c * '_c)) list),
                     void : '_b,
                     ec : { is_keyword : LrTable.term -> bool,
                            noShift : LrTable.term -> bool,
                            preferred_change : (LrTable.term list * LrTable.term list) list,
                            errtermvalue : LrTable.term -> '_b,
                            showTerminal : LrTable.term -> string,
                            terms: LrTable.term list,
                            error : string * '_c * '_c -> unit
                           },
                     lookahead : int  (* max amount of lookahead used in *)
                                      (* error correction *)
                        } -> '_b *
                             (('_b,'_c) Token.token Stream.stream)
    end

(* LEXER: a signature that most lexers produced for use with SML-Yacc's
   output will match.  The user is responsible for declaring type token,
   type pos, and type svalue in the UserDeclarations section of a lexer.

   Note that type token is abstract in the lexer.  This allows SML-Yacc to
   create a TOKENS signature for use with lexers produced by ML-Lex that
   treats the type token abstractly.  Lexers that are functors parametrized by
   a Tokens structure matching a TOKENS signature cannot examine the structure
   of tokens.
*)

signature LEXER =
   sig
       structure UserDeclarations :
           sig
                type ('a,'b) token
                type pos
                type svalue
           end
        val makeLexer : (int -> string) -> unit -> 
         (UserDeclarations.svalue,UserDeclarations.pos) UserDeclarations.token
   end

(* ARG_LEXER: the %arg option of ML-Lex allows users to produce lexers which
   also take an argument before yielding a function from unit to a token
*)

signature ARG_LEXER =
   sig
       structure UserDeclarations :
           sig
                type ('a,'b) token
                type pos
                type svalue
                type arg
           end
        val makeLexer : (int -> string) -> UserDeclarations.arg -> unit -> 
         (UserDeclarations.svalue,UserDeclarations.pos) UserDeclarations.token
   end

(* PARSER_DATA: the signature of ParserData structures in {parser name}LrValsFun
   produced by  SML-Yacc.  All such structures match this signature.  

   The {parser name}LrValsFun produces a structure which contains all the values
   except for the lexer needed to call the polymorphic parser mentioned
   before.

*)

signature PARSER_DATA =
   sig
        (* the type of line numbers *)

        type pos

        (* the type of semantic values *)

        type svalue

         (* the type of the user-supplied argument to the parser *)
        type arg
 
        (* the intended type of the result of the parser.  This value is
           produced by applying extract from the structure Actions to the
           final semantic value resultiing from a parse.
         *)

        type result

        structure LrTable : LR_TABLE
        structure Token : TOKEN
        sharing Token.LrTable = LrTable

        (* structure Actions contains the functions which mantain the
           semantic values stack in the parser.  Void is used to provide
           a default value for the semantic stack.
         *)

        structure Actions : 
          sig
              val actions : int * pos *
                   (LrTable.state * (svalue * pos * pos)) list * arg->
                         LrTable.nonterm * (svalue * pos * pos) *
                         ((LrTable.state *(svalue * pos * pos)) list)
              val void : svalue
              val extract : svalue -> result
          end

        (* structure EC contains information used to improve error
           recovery in an error-correcting parser *)

        structure EC :
           sig
             val is_keyword : LrTable.term -> bool
             val noShift : LrTable.term -> bool
             val preferred_change : (LrTable.term list * LrTable.term list) list
             val errtermvalue : LrTable.term -> svalue
             val showTerminal : LrTable.term -> string
             val terms: LrTable.term list
           end

        (* table is the LR table for the parser *)

        val table : LrTable.table
    end

(* signature PARSER is the signature that most user parsers created by 
   SML-Yacc will match.
*)

signature PARSER =
    sig
        structure Token : TOKEN
        structure Stream : STREAM
        exception ParseError

        (* type pos is the type of line numbers *)

        type pos

        (* type result is the type of the result from the parser *)

        type result

         (* the type of the user-supplied argument to the parser *)
        type arg
        
        (* type svalue is the type of semantic values for the semantic value
           stack
         *)

        type svalue

        (* val makeLexer is used to create a stream of tokens for the parser *)

        val makeLexer : (int -> string) ->
                         (svalue,pos) Token.token Stream.stream

        (* val parse takes a stream of tokens and a function to print
           errors and returns a value of type result and a stream containing
           the unused tokens
         *)

        val parse : int * ((svalue,pos) Token.token Stream.stream) *
                    (string * pos * pos -> unit) * arg ->
                                result * (svalue,pos) Token.token Stream.stream

        val sameToken : (svalue,pos) Token.token * (svalue,pos) Token.token ->
                                bool
     end

(* signature ARG_PARSER is the signature that will be matched by parsers whose
    lexer takes an additional argument.
*)

signature ARG_PARSER = 
    sig
        structure Token : TOKEN
        structure Stream : STREAM
        exception ParseError

        type arg
        type lexarg
        type pos
        type result
        type svalue

        val makeLexer : (int -> string) -> lexarg ->
                         (svalue,pos) Token.token Stream.stream
        val parse : int * ((svalue,pos) Token.token Stream.stream) *
                    (string * pos * pos -> unit) * arg ->
                                result * (svalue,pos) Token.token Stream.stream

        val sameToken : (svalue,pos) Token.token * (svalue,pos) Token.token ->
                                bool
     end

