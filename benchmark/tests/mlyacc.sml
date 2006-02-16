(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi 
 *
 * $Log$
 * Revision 1.1.1.1  1996/01/31  16:01:46  george
 * Version 109
 * 
 *)

signature ORDSET =
   sig
      type set
      type elem
      exception Select_arb
      val app : (elem -> unit) -> set -> unit
          and card: set -> int
          and closure: set * (elem -> set) -> set
          and difference: set * set -> set
          and elem_eq: (elem * elem -> bool)
          and elem_gt : (elem * elem -> bool)
          and empty: set
          and exists: (elem * set) -> bool
          and find : (elem * set)  ->  elem option
          and fold: ((elem * 'b) -> 'b) -> set -> 'b -> 'b
          and insert: (elem * set) -> set
          and is_empty: set -> bool
          and make_list: set -> elem list
          and make_set: (elem list -> set)
          and partition: (elem -> bool) -> (set -> set * set)
          and remove: (elem * set) -> set
          and revfold: ((elem * 'b) -> 'b) -> set -> 'b -> 'b
          and select_arb: set -> elem
          and set_eq: (set * set) -> bool
          and set_gt: (set * set) -> bool
          and singleton: (elem -> set)
          and union: set * set -> set
   end

signature TABLE =
   sig
        type 'a table
        type key
        val size : 'a table -> int
        val empty: 'a table
        val exists: (key * 'a table) -> bool
        val find : (key * 'a table)  ->  'a option
        val insert: ((key * 'a) * 'a table) -> 'a table
        val make_table : (key * 'a ) list -> 'a table
        val make_list : 'a table -> (key * 'a) list
        val fold : ((key * 'a) * 'b -> 'b) -> 'a table -> 'b -> 'b
   end

signature HASH =
  sig
    type table
    type elem

    val size : table -> int
    val add : elem * table -> table
    val find : elem * table -> int option
    val exists : elem * table -> bool
    val empty : table
  end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi 
 *
 * $Log$
 * Revision 1.1.1.1  1996/01/31  16:01:42  george
 * Version 109
 * 
 *)

(* base.sig: Base signature file for SML-Yacc.  This file contains signatures
   that must be loaded before any of the files produced by ML-Yacc are loaded
*)

(* STREAM: signature for a lazy stream.*)

signature STREAM =
 sig type 'xa stream
     val streamify : (unit -> 'a) -> 'a stream
     val cons : 'a * 'a stream -> 'a stream
     val get : 'a stream -> 'a * 'a stream
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
                     lexer : ('b,'c) Token.token Stream.stream,
                     arg: 'arg,
                     saction : int *
                               'c *
                                (LrTable.state * ('b * 'c * 'c)) list * 
                                'arg ->
                                     LrTable.nonterm *
                                     ('b * 'c * 'c) *
                                     ((LrTable.state *('b * 'c * 'c)) list),
                     void : 'b,
                     ec : { is_keyword : LrTable.term -> bool,
                            noShift : LrTable.term -> bool,
                            preferred_change : (LrTable.term list * LrTable.term list) list,
                            errtermvalue : LrTable.term -> 'b,
                            showTerminal : LrTable.term -> string,
                            terms: LrTable.term list,
                            error : string * 'c * 'c -> unit
                           },
                     lookahead : int  (* max amount of lookahead used in *)
                                      (* error correction *)
                        } -> 'b *
                             (('b,'c) Token.token Stream.stream)
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

(* ML-Yacc Parser Generator (c) 1989, 1991 Andrew W. Appel, David R. Tarditi 
 *
 * $Log$
 * Revision 1.2  1996/02/26  15:02:38  george
 *    print no longer overloaded.
 *    use of makestring has been removed and replaced with Int.toString ..
 *    use of IO replaced with TextIO
 *
 * Revision 1.1.1.1  1996/01/31  16:01:46  george
 * Version 109
 * 
 *)

signature HEADER =
  sig
    type pos (*= int   1998-5-14 STW: taken out because leads to nonstandard sharing constraint on line 3386 *)
    val lineno : pos ref
    val text : string list ref 

    type inputSource
    val newSource : string * TextIO.instream * TextIO.outstream -> inputSource
    val error : inputSource -> pos -> string -> unit
    val warn : inputSource -> pos -> string -> unit
    val errorOccurred : inputSource -> unit -> bool

    datatype symbol = SYMBOL of string * pos
    val symbolName : symbol -> string
    val symbolPos : symbol -> pos
    val symbolMake : string * int -> symbol

    type ty
    val tyName : ty -> string
    val tyMake : string -> ty

    (* associativities: each kind of associativity is assigned a unique
       integer *)

    datatype prec = LEFT | RIGHT | NONASSOC
    datatype control = NODEFAULT | VERBOSE | PARSER_NAME of symbol |
                       FUNCTOR of string  | START_SYM of symbol |
                       NSHIFT of symbol list | POS of string | PURE |
                       PARSE_ARG of string * string
                           
    datatype rule = RULE of {lhs : symbol, rhs : symbol list,
                             code : string, prec : symbol option}

    datatype declData = DECL of 
                        {eop : symbol list,
                         keyword : symbol list,
                         nonterm : (symbol * ty option) list option,
                         prec : (prec * (symbol list)) list,
                         change: (symbol list * symbol list) list,
                         term : (symbol * ty option) list option,
                         control : control list,
                         value : (symbol * string) list}

     val join_decls : declData * declData * inputSource * pos -> declData

     type parseResult
     val getResult : parseResult -> string * declData * rule list
  end;

signature PARSE_GEN_PARSER =
  sig
    structure Header : HEADER
    val parse : string -> Header.parseResult * Header.inputSource
  end;

signature PARSE_GEN =
  sig
    val parseGen : string -> unit
  end;

signature GRAMMAR =
    sig
        
        datatype term = T of int
        datatype nonterm = NT of int
        datatype symbol = TERM of term | NONTERM of nonterm

        (* grammar:
             terminals should be numbered from 0 to terms-1,
             nonterminals should be numbered from 0 to nonterms-1,
             rules should be numbered between 0 and (length rules) - 1,
             higher precedence binds tighter,
             start nonterminal should not occur on the rhs of any rule
        *)

        datatype grammar = GRAMMAR of
                        {rules: {lhs : nonterm, rhs : symbol list,
                                 precedence : int option, rulenum : int } list,
                        terms: int,
                        nonterms: int,
                        start : nonterm,
                        eop : term list,
                        noshift : term list,
                        precedence : term -> int option,
                        termToString : term -> string,
                        nontermToString : nonterm -> string} 
   end

(* signature for internal version of grammar *)

signature INTGRAMMAR =
    sig
        structure Grammar  : GRAMMAR
        structure SymbolAssoc : TABLE
        structure NontermAssoc : TABLE

        sharing type SymbolAssoc.key = Grammar.symbol
        sharing type NontermAssoc.key = Grammar.nonterm

        datatype rule = RULE of
                {lhs : Grammar.nonterm,
                 rhs : Grammar.symbol list,

        (* internal number of rule - convenient for producing LR graph *)

                 num : int,     
                 rulenum : int,
                 precedence : int option}

        val gtTerm : Grammar.term * Grammar.term -> bool
        val eqTerm : Grammar.term * Grammar.term -> bool

        val gtNonterm : Grammar.nonterm * Grammar.nonterm -> bool
        val eqNonterm : Grammar.nonterm * Grammar.nonterm -> bool

        val gtSymbol : Grammar.symbol * Grammar.symbol -> bool
        val eqSymbol : Grammar.symbol * Grammar.symbol -> bool

        (* Debugging information will be generated only if DEBUG is true. *)

        val DEBUG : bool

        val prRule : (Grammar.symbol -> string) * (Grammar.nonterm -> string) *
                                (string -> 'b) -> rule -> unit
        val prGrammar : (Grammar.symbol -> string)*(Grammar.nonterm -> string) *
                                (string -> unit) -> Grammar.grammar -> unit
    end

signature CORE =
    sig
        structure Grammar : GRAMMAR
        structure IntGrammar : INTGRAMMAR
        sharing Grammar = IntGrammar.Grammar

        datatype item = ITEM of
                        { rule : IntGrammar.rule,
                          dot : int,

(* rhsAfter: The portion of the rhs of a rule that lies after the dot *)

                          rhsAfter: Grammar.symbol list }

(* eqItem and gtItem compare items *)

        val eqItem : item * item -> bool
        val gtItem : item * item -> bool

(* functions for maintaining ordered item lists *)

        val insert : item * item list -> item list
        val union : item list * item list -> item list

(* core:  a set of items.  It is represented by an ordered list of items. 
   The list is in ascending order The rule numbers and the positions of the
   dots are used to order the items. *)

        datatype core = CORE of item list * int (* state # *)

(* gtCore and eqCore compare the lists of items *)

        val gtCore : core * core -> bool
        val eqCore : core * core -> bool

(* functions for debugging *)

        val prItem : (Grammar.symbol -> string) * (Grammar.nonterm -> string) *
                                (string -> unit) -> item -> unit
        val prCore : (Grammar.symbol -> string) * (Grammar.nonterm -> string) *
                                (string -> unit) -> core -> unit
end

signature CORE_UTILS =
    sig

        structure Grammar : GRAMMAR
        structure IntGrammar : INTGRAMMAR
        structure Core : CORE

        sharing Grammar = IntGrammar.Grammar = Core.Grammar
        sharing IntGrammar = Core.IntGrammar

(* mkFuncs: create functions for the set of productions derived from a
   nonterminal, the cores that result from shift/gotos from a core,
    and return a list of rules *)

        val mkFuncs : Grammar.grammar ->
                { produces : Grammar.nonterm -> IntGrammar.rule list,

(* shifts: take a core and compute all the cores that result from shifts/gotos
   on symbols *)

                  shifts : Core.core -> (Grammar.symbol*Core.item list) list,
                  rules: IntGrammar.rule list,

(* epsProds: take a core compute epsilon productions for it *)

                  epsProds : Core.core -> IntGrammar.rule list}
        end

signature LRGRAPH =
    sig
        structure Grammar : GRAMMAR
        structure IntGrammar : INTGRAMMAR
        structure Core : CORE

        sharing Grammar = IntGrammar.Grammar = Core.Grammar
        sharing IntGrammar = Core.IntGrammar

        type graph
        val edges : Core.core * graph -> {edge:Grammar.symbol,to:Core.core} list
        val nodes : graph -> Core.core list
        val shift : graph -> int * Grammar.symbol -> int (* int = state # *)
        val core : graph -> int -> Core.core (* get core for a state *)

(* mkGraph: compute the LR(0) sets of items *)

        val mkGraph :  Grammar.grammar ->
                         {graph : graph,
                          produces : Grammar.nonterm -> IntGrammar.rule list,
                          rules : IntGrammar.rule list,
                          epsProds: Core.core -> IntGrammar.rule list}

        val prGraph: (Grammar.symbol -> string)*(Grammar.nonterm -> string) *
                                (string -> unit) -> graph -> unit
    end

signature LOOK =
    sig
        structure Grammar : GRAMMAR
        structure IntGrammar : INTGRAMMAR
        sharing Grammar = IntGrammar.Grammar

        val union : Grammar.term list * Grammar.term list -> Grammar.term list
        val make_set : Grammar.term list -> Grammar.term list

        val mkFuncs :  {rules : IntGrammar.rule list, nonterms : int,
                        produces : Grammar.nonterm -> IntGrammar.rule list} ->
                            {nullable: Grammar.nonterm -> bool,
                             first : Grammar.symbol list -> Grammar.term list}

        val prLook : (Grammar.term -> string) * (string -> unit) -> 
                        Grammar.term list -> unit
   end

signature LALR_GRAPH =
    sig
        structure Grammar : GRAMMAR
        structure IntGrammar : INTGRAMMAR
        structure Core : CORE
        structure Graph : LRGRAPH

        sharing Grammar = IntGrammar.Grammar = Core.Grammar = Graph.Grammar
        sharing IntGrammar = Core.IntGrammar = Graph.IntGrammar
        sharing Core = Graph.Core

        datatype lcore = LCORE of (Core.item * Grammar.term list) list * int
        val addLookahead : {graph : Graph.graph,
                            first : Grammar.symbol list -> Grammar.term list,
                            eop : Grammar.term list,
                            nonterms : int,
                            nullable: Grammar.nonterm -> bool,
                            produces : Grammar.nonterm -> IntGrammar.rule list,
                            rules : IntGrammar.rule list,
                            epsProds : Core.core -> IntGrammar.rule list,
                            print : string -> unit,  (* for debugging *)
                            termToString : Grammar.term -> string,
                            nontermToString : Grammar.nonterm -> string} ->
                                lcore list
        val prLcore : (Grammar.symbol -> string) * (Grammar.nonterm -> string) *
                      (Grammar.term -> string) * (string -> unit) ->
                                         lcore -> unit
    end

(* LR_ERRS: errors found while constructing an LR table *)

signature LR_ERRS =
  sig
    structure LrTable : LR_TABLE

    (* RR = reduce/reduce,
       SR = shift/reduce
       NS: non-shiftable terminal found on the rhs of a rule
       NOT_REDUCED n: rule number n was not reduced
       START n : start symbol found on the rhs of rule n *)

    datatype err = RR of LrTable.term * LrTable.state * int * int
                 | SR of LrTable.term * LrTable.state * int
                 | NS of LrTable.term * int  
                 | NOT_REDUCED of int
                 | START of int

     val summary : err list -> {rr : int, sr: int,
                            not_reduced : int, start : int,nonshift : int}

     val printSummary : (string -> unit) -> err list -> unit
                                      
  end

(* PRINT_STRUCT: prints a structure which includes a value 'table' and a
   structure Table whose signature matches LR_TABLE.  The table in the printed
   structure will contain the same information as the one passed to
   printStruct, although the representation may be different.  It returns
   the number of entries left in the table after compaction.*)
  
signature PRINT_STRUCT =
  sig
        structure LrTable : LR_TABLE
        val makeStruct :
                {table : LrTable.table,
                 name : string,
                 print: string -> unit,
                 verbose : bool
                } -> int
  end

(* VERBOSE: signature for a structure which takes a table and creates a
   verbose description of it *)

signature VERBOSE =
  sig
        structure Errs : LR_ERRS
        val printVerbose :
                {table : Errs.LrTable.table,
                 entries : int,
                 termToString : Errs.LrTable.term -> string,
                 nontermToString : Errs.LrTable.nonterm -> string,
                 stateErrs : Errs.LrTable.state -> Errs.err list,
                 errs : Errs.err list,
                 print: string -> unit,
                 printCores : (string -> unit) -> Errs.LrTable.state -> unit,
                 printRule : (string -> unit) -> int -> unit} -> unit
  end

(* MAKE_LR_TABLE: signature for a structure which includes a structure
   matching the signature LR_TABLE and a function which maps grammars
   to tables *)

signature MAKE_LR_TABLE =
   sig
        structure Grammar : GRAMMAR
        structure Errs : LR_ERRS
        structure LrTable : LR_TABLE
        sharing Errs.LrTable = LrTable

        sharing type LrTable.term = Grammar.term
        sharing type LrTable.nonterm = Grammar.nonterm

        (* boolean value determines whether default reductions will be used.
           If it is true, reductions will be used. *)

        val mkTable : Grammar.grammar * bool ->
               LrTable.table *
              (LrTable.state -> Errs.err list) *   (* errors in a state *)
              ((string -> unit) -> LrTable.state -> unit) *
               Errs.err list    (* list of all errors *)
   end;

(* SHRINK_LR_TABLE: finds unique action entry rows in the  action table
   for the LR parser *)

signature SHRINK_LR_TABLE =
   sig
       (* Takes an action table represented as a list of action rows.
          It returns the number of unique rows left in the action table,
          a list of integers which maps each original row to a unique
          row, and a list of unique rows *)
       structure LrTable : LR_TABLE
       val shrinkActionList : LrTable.table * bool ->
                (int * int list *
                   ((LrTable.term,LrTable.action) LrTable.pairlist *
                    LrTable.action) list) * int
    end
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi 
 *
 * $Log$
 * Revision 1.2  1996/02/26  15:02:34  george
 *    print no longer overloaded.
 *    use of makestring has been removed and replaced with Int.toString ..
 *    use of IO replaced with TextIO
 *
 * Revision 1.1.1.1  1996/01/31  16:01:45  george
 * Version 109
 * 
 *)

functor HeaderFun () : HEADER =
  struct
        val DEBUG = true

        type pos = int
        val lineno = ref 0
        val text = ref (nil: string list)
        type inputSource = {name : string,
                            errStream : TextIO.outstream,
                            inStream : TextIO.instream,
                            errorOccurred : bool ref}

        val newSource = 
          fn (s : string,i : TextIO.instream ,errs : TextIO.outstream) =>
              {name=s,errStream=errs,inStream=i,
               errorOccurred = ref false}
                        
        val errorOccurred = fn (s : inputSource) =>fn () => !(#errorOccurred s)

        val pr = fn out : TextIO.outstream => fn s : string => TextIO.output(out,s)

        val error = fn {name,errStream, errorOccurred,...} : inputSource =>
              let val pr = pr errStream
              in fn l : pos => fn msg : string =>
                  (pr name; pr ", line "; pr (Int.toString l); pr ": Error: ";
                   pr msg; pr "\n"; errorOccurred := true)
              end

        val warn = fn {name,errStream, errorOccurred,...} : inputSource =>
              let val pr = pr errStream
              in fn l : pos => fn msg : string =>
                  (pr name; pr ", line "; pr (Int.toString l); pr ": Warning: ";
                   pr msg; pr "\n")
              end

        datatype prec = LEFT | RIGHT | NONASSOC

        datatype symbol = SYMBOL of string * pos
        val symbolName = fn SYMBOL(s,_) => s
        val symbolPos = fn SYMBOL(_,p) => p
        val symbolMake = fn sp => SYMBOL sp
    
        type ty = string
        val tyName = fn i => i
        val tyMake = fn i => i
 
        datatype control = NODEFAULT | VERBOSE | PARSER_NAME of symbol |
                           FUNCTOR of string  | START_SYM of symbol |
                           NSHIFT of symbol list | POS of string | PURE |
                           PARSE_ARG of string * string
                           
        datatype declData = DECL of
                        {eop : symbol list,
                         keyword : symbol list,
                         nonterm : (symbol*ty option) list option,
                         prec : (prec * (symbol list)) list,
                         change: (symbol list * symbol list) list,
                         term : (symbol* ty option) list option,
                         control : control list,
                         value : (symbol * string) list}

        type rhsData = {rhs:symbol list,code:string, prec:symbol option} list
        datatype rule = RULE of {lhs : symbol, rhs : symbol list,
                                 code : string, prec : symbol option}

        type parseResult = string * declData * rule list
        val getResult = fn p => p

        fun join_decls
              (DECL {eop=e,control=c,keyword=k,nonterm=n,prec,
                change=su,term=t,value=v}:declData,
               DECL {eop=e',control=c',keyword=k',nonterm=n',prec=prec',
                     change=su',term=t',value=v'} : declData,
               inputSource,pos) =
          let val ignore = fn s => 
                        (warn inputSource pos ("ignoring duplicate " ^ s ^
                                            " declaration"))
              val join = fn (e,NONE,NONE) => NONE
                          | (e,NONE,a) => a
                          | (e,a,NONE) => a
                          | (e,a,b) => (ignore e; a)
              fun mergeControl (nil,a) = [a]
                | mergeControl (l as h::t,a) =
                     case (h,a)
                     of (PARSER_NAME _,PARSER_NAME n1) => (ignore "%name"; l)
                      | (FUNCTOR _,FUNCTOR _) => (ignore "%header"; l)
                      | (PARSE_ARG _,PARSE_ARG _) => (ignore "%arg"; l)
                      | (START_SYM _,START_SYM s) => (ignore "%start"; l)
                      | (POS _,POS _) => (ignore "%pos"; l)
                      | (NSHIFT a,NSHIFT b) => (NSHIFT (a@b)::t)
                      | _ => h :: mergeControl(t,a)
              fun loop (nil,r) = r
                | loop (h::t,r) = loop(t,mergeControl(r,h))
         in DECL {eop=e@e',control=loop(c',c),keyword=k'@k,
            nonterm=join("%nonterm",n,n'), prec=prec@prec',
            change=su@su', term=join("%term",t,t'),value=v@v'} :
                   declData
        end
end;

structure Header = HeaderFun();
      
signature Mlyacc_TOKENS =
sig
type ('a,'b) token
type svalue
val BOGUS_VALUE:  'a * 'a -> (svalue,'a) token
val UNKNOWN: (string) *  'a * 'a -> (svalue,'a) token
val VALUE:  'a * 'a -> (svalue,'a) token
val VERBOSE:  'a * 'a -> (svalue,'a) token
val TYVAR: (string) *  'a * 'a -> (svalue,'a) token
val TERM:  'a * 'a -> (svalue,'a) token
val START:  'a * 'a -> (svalue,'a) token
val SUBST:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val PROG: (string) *  'a * 'a -> (svalue,'a) token
val PREFER:  'a * 'a -> (svalue,'a) token
val PREC_TAG:  'a * 'a -> (svalue,'a) token
val PREC: (Header.prec) *  'a * 'a -> (svalue,'a) token
val PERCENT_ARG:  'a * 'a -> (svalue,'a) token
val PERCENT_POS:  'a * 'a -> (svalue,'a) token
val PERCENT_PURE:  'a * 'a -> (svalue,'a) token
val PERCENT_EOP:  'a * 'a -> (svalue,'a) token
val OF:  'a * 'a -> (svalue,'a) token
val NOSHIFT:  'a * 'a -> (svalue,'a) token
val NONTERM:  'a * 'a -> (svalue,'a) token
val NODEFAULT:  'a * 'a -> (svalue,'a) token
val NAME:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
val KEYWORD:  'a * 'a -> (svalue,'a) token
val INT: (string) *  'a * 'a -> (svalue,'a) token
val PERCENT_HEADER:  'a * 'a -> (svalue,'a) token
val IDDOT: (string) *  'a * 'a -> (svalue,'a) token
val ID: (string*int) *  'a * 'a -> (svalue,'a) token
val HEADER: (string) *  'a * 'a -> (svalue,'a) token
val FOR:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
val DELIMITER:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val CHANGE:  'a * 'a -> (svalue,'a) token
val BAR:  'a * 'a -> (svalue,'a) token
val BLOCK:  'a * 'a -> (svalue,'a) token
val ASTERISK:  'a * 'a -> (svalue,'a) token
val ARROW:  'a * 'a -> (svalue,'a) token
end
signature Mlyacc_LRVALS=
sig
structure Tokens : Mlyacc_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
functor MlyaccLrValsFun(structure Hdr : HEADER 
                                        where type prec = Header.prec
                        structure Token : TOKEN) = 
                             
struct
structure ParserData=
struct
structure Header = 
struct
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

(* parser for the ML parser generator *)

open Hdr

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\074\000\000\000\
\\001\000\005\000\024\000\008\000\023\000\014\000\022\000\016\000\021\000\
\\019\000\020\000\020\000\019\000\021\000\018\000\022\000\017\000\
\\024\000\016\000\025\000\015\000\026\000\014\000\027\000\013\000\
\\028\000\012\000\030\000\011\000\034\000\010\000\035\000\009\000\
\\036\000\008\000\038\000\007\000\039\000\006\000\000\000\
\\001\000\006\000\061\000\000\000\
\\001\000\006\000\072\000\000\000\
\\001\000\006\000\084\000\000\000\
\\001\000\006\000\096\000\000\000\
\\001\000\007\000\083\000\032\000\082\000\000\000\
\\001\000\009\000\000\000\000\000\
\\001\000\010\000\059\000\000\000\
\\001\000\011\000\003\000\000\000\
\\001\000\012\000\025\000\000\000\
\\001\000\012\000\027\000\000\000\
\\001\000\012\000\028\000\000\000\
\\001\000\012\000\031\000\000\000\
\\001\000\012\000\042\000\013\000\041\000\000\000\
\\001\000\012\000\042\000\013\000\041\000\017\000\040\000\031\000\039\000\
\\037\000\038\000\000\000\
\\001\000\012\000\046\000\000\000\
\\001\000\012\000\051\000\000\000\
\\001\000\012\000\069\000\015\000\068\000\000\000\
\\001\000\012\000\069\000\015\000\068\000\032\000\067\000\000\000\
\\001\000\012\000\075\000\000\000\
\\001\000\012\000\078\000\000\000\
\\001\000\012\000\099\000\000\000\
\\001\000\031\000\035\000\000\000\
\\001\000\031\000\048\000\000\000\
\\001\000\031\000\055\000\000\000\
\\001\000\031\000\098\000\000\000\
\\001\000\031\000\102\000\000\000\
\\104\000\012\000\051\000\000\000\
\\105\000\000\000\
\\106\000\000\000\
\\107\000\004\000\056\000\000\000\
\\108\000\004\000\056\000\000\000\
\\109\000\000\000\
\\110\000\000\000\
\\111\000\000\000\
\\112\000\000\000\
\\113\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\\117\000\000\000\
\\118\000\000\000\
\\119\000\001\000\064\000\002\000\063\000\012\000\042\000\013\000\041\000\000\000\
\\120\000\000\000\
\\121\000\000\000\
\\122\000\000\000\
\\123\000\001\000\064\000\002\000\063\000\012\000\042\000\013\000\041\000\000\000\
\\124\000\000\000\
\\125\000\000\000\
\\126\000\004\000\073\000\000\000\
\\127\000\000\000\
\\128\000\000\000\
\\129\000\004\000\058\000\000\000\
\\130\000\000\000\
\\131\000\001\000\064\000\002\000\063\000\012\000\042\000\013\000\041\000\000\000\
\\132\000\023\000\089\000\000\000\
\\133\000\001\000\064\000\002\000\063\000\012\000\042\000\013\000\041\000\000\000\
\\134\000\023\000\057\000\000\000\
\\135\000\004\000\092\000\000\000\
\\136\000\000\000\
\\137\000\000\000\
\\138\000\000\000\
\\139\000\012\000\033\000\000\000\
\\140\000\000\000\
\\141\000\000\000\
\\142\000\000\000\
\\143\000\000\000\
\\144\000\000\000\
\\145\000\000\000\
\\146\000\000\000\
\\147\000\000\000\
\\148\000\012\000\042\000\013\000\041\000\000\000\
\\149\000\001\000\064\000\002\000\063\000\012\000\042\000\013\000\041\000\000\000\
\\150\000\001\000\064\000\002\000\063\000\012\000\042\000\013\000\041\000\000\000\
\\151\000\001\000\064\000\002\000\063\000\012\000\042\000\013\000\041\000\000\000\
\\152\000\000\000\
\\153\000\000\000\
\\154\000\000\000\
\\155\000\000\000\
\\156\000\000\000\
\\157\000\029\000\094\000\000\000\
\"
val actionRowNumbers =
"\009\000\030\000\001\000\029\000\
\\010\000\044\000\011\000\012\000\
\\013\000\063\000\063\000\023\000\
\\015\000\046\000\063\000\063\000\
\\011\000\045\000\016\000\063\000\
\\024\000\017\000\063\000\025\000\
\\031\000\058\000\034\000\053\000\
\\039\000\008\000\037\000\063\000\
\\033\000\002\000\047\000\071\000\
\\066\000\069\000\019\000\014\000\
\\076\000\035\000\040\000\032\000\
\\042\000\036\000\041\000\028\000\
\\061\000\003\000\050\000\038\000\
\\000\000\048\000\020\000\015\000\
\\013\000\021\000\062\000\015\000\
\\070\000\015\000\015\000\006\000\
\\004\000\068\000\079\000\078\000\
\\077\000\060\000\063\000\063\000\
\\063\000\056\000\057\000\052\000\
\\054\000\043\000\072\000\073\000\
\\067\000\018\000\015\000\059\000\
\\081\000\049\000\051\000\015\000\
\\005\000\075\000\063\000\026\000\
\\022\000\055\000\015\000\081\000\
\\064\000\080\000\074\000\027\000\
\\065\000\007\000"
val gotoT =
"\
\\001\000\101\000\000\000\
\\006\000\002\000\000\000\
\\005\000\003\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\024\000\000\000\
\\000\000\
\\013\000\028\000\014\000\027\000\000\000\
\\003\000\030\000\000\000\
\\003\000\032\000\000\000\
\\000\000\
\\007\000\035\000\017\000\034\000\000\000\
\\000\000\
\\003\000\041\000\000\000\
\\003\000\042\000\000\000\
\\002\000\043\000\000\000\
\\000\000\
\\000\000\
\\003\000\045\000\000\000\
\\000\000\
\\010\000\048\000\011\000\047\000\000\000\
\\003\000\052\000\015\000\051\000\016\000\050\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\058\000\000\000\
\\000\000\
\\000\000\
\\007\000\060\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\064\000\008\000\063\000\000\000\
\\007\000\068\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\069\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\035\000\017\000\074\000\000\000\
\\013\000\075\000\014\000\027\000\000\000\
\\000\000\
\\000\000\
\\007\000\035\000\017\000\077\000\000\000\
\\000\000\
\\007\000\035\000\017\000\078\000\000\000\
\\007\000\035\000\017\000\079\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\084\000\009\000\083\000\000\000\
\\003\000\052\000\015\000\085\000\016\000\050\000\000\000\
\\003\000\086\000\000\000\
\\000\000\
\\007\000\060\000\000\000\
\\000\000\
\\000\000\
\\007\000\060\000\000\000\
\\007\000\060\000\000\000\
\\007\000\060\000\000\000\
\\000\000\
\\004\000\088\000\000\000\
\\007\000\035\000\017\000\089\000\000\000\
\\000\000\
\\012\000\091\000\000\000\
\\000\000\
\\000\000\
\\007\000\035\000\017\000\093\000\000\000\
\\000\000\
\\007\000\060\000\000\000\
\\003\000\095\000\000\000\
\\000\000\
\\000\000\
\\007\000\060\000\000\000\
\\007\000\035\000\017\000\098\000\000\000\
\\012\000\099\000\000\000\
\\000\000\
\\000\000\
\\007\000\060\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 102
val numrules = 54
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = Hdr.inputSource
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | UNKNOWN of unit ->  (string) | TYVAR of unit ->  (string)
 | PROG of unit ->  (string) | PREC of unit ->  (Header.prec)
 | INT of unit ->  (string) | IDDOT of unit ->  (string)
 | ID of unit ->  (string*int) | HEADER of unit ->  (string)
 | TY of unit ->  (string)
 | CHANGE_DEC of unit ->  ( ( Hdr.symbol list * Hdr.symbol list ) )
 | CHANGE_DECL of unit ->  ( ( Hdr.symbol list * Hdr.symbol list )  list)
 | SUBST_DEC of unit ->  ( ( Hdr.symbol list * Hdr.symbol list ) )
 | SUBST_DECL of unit ->  ( ( Hdr.symbol list * Hdr.symbol list )  list)
 | G_RULE_PREC of unit ->  (Hdr.symbol option)
 | G_RULE_LIST of unit ->  (Hdr.rule list)
 | G_RULE of unit ->  (Hdr.rule list)
 | RHS_LIST of unit ->  ({ rhs:Hdr.symbol list,code:string,prec:Hdr.symbol option }  list)
 | RECORD_LIST of unit ->  (string) | QUAL_ID of unit ->  (string)
 | MPC_DECLS of unit ->  (Hdr.declData)
 | MPC_DECL of unit ->  (Hdr.declData) | LABEL of unit ->  (string)
 | ID_LIST of unit ->  (Hdr.symbol list)
 | CONSTR_LIST of unit ->  ( ( Hdr.symbol * Hdr.ty option )  list)
 | BEGIN of unit ->  (string*Hdr.declData* ( Hdr.rule list ) )
end
type svalue = MlyValue.svalue
type result = string*Hdr.declData* ( Hdr.rule list ) 
end
structure EC=
struct
open LrTable
val is_keyword =
fn _ => false
val preferred_change = 
nil
val noShift = 
fn (T 8) => true | _ => false
val showTerminal =
fn (T 0) => "ARROW"
  | (T 1) => "ASTERISK"
  | (T 2) => "BLOCK"
  | (T 3) => "BAR"
  | (T 4) => "CHANGE"
  | (T 5) => "COLON"
  | (T 6) => "COMMA"
  | (T 7) => "DELIMITER"
  | (T 8) => "EOF"
  | (T 9) => "FOR"
  | (T 10) => "HEADER"
  | (T 11) => "ID"
  | (T 12) => "IDDOT"
  | (T 13) => "PERCENT_HEADER"
  | (T 14) => "INT"
  | (T 15) => "KEYWORD"
  | (T 16) => "LBRACE"
  | (T 17) => "LPAREN"
  | (T 18) => "NAME"
  | (T 19) => "NODEFAULT"
  | (T 20) => "NONTERM"
  | (T 21) => "NOSHIFT"
  | (T 22) => "OF"
  | (T 23) => "PERCENT_EOP"
  | (T 24) => "PERCENT_PURE"
  | (T 25) => "PERCENT_POS"
  | (T 26) => "PERCENT_ARG"
  | (T 27) => "PREC"
  | (T 28) => "PREC_TAG"
  | (T 29) => "PREFER"
  | (T 30) => "PROG"
  | (T 31) => "RBRACE"
  | (T 32) => "RPAREN"
  | (T 33) => "SUBST"
  | (T 34) => "START"
  | (T 35) => "TERM"
  | (T 36) => "TYVAR"
  | (T 37) => "VERBOSE"
  | (T 38) => "VALUE"
  | (T 39) => "UNKNOWN"
  | (T 40) => "BOGUS_VALUE"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms = (T 0) :: (T 1) :: (T 2) :: (T 3) :: (T 4) :: (T 5) :: (T 6
) :: (T 7) :: (T 8) :: (T 9) :: (T 13) :: (T 15) :: (T 16) :: (T 17)
 :: (T 18) :: (T 19) :: (T 20) :: (T 21) :: (T 22) :: (T 23) :: (T 24)
 :: (T 25) :: (T 26) :: (T 28) :: (T 29) :: (T 31) :: (T 32) :: (T 33)
 :: (T 34) :: (T 35) :: (T 37) :: (T 38) :: (T 40) :: nil
end
structure Actions =
struct 
exception mlyAction of int
local open Header
in
val actions = 
fn (i392,defaultPos,stack,
    (inputSource):arg) =>
case (i392,stack)
of (0,(_,(MlyValue.G_RULE_LIST G_RULE_LIST1,_,G_RULE_LIST1right))::_::
(_,(MlyValue.MPC_DECLS MPC_DECLS1,_,_))::(_,(MlyValue.HEADER HEADER1,
HEADER1left,_))::rest671) => let val result=MlyValue.BEGIN(fn _ => 
let val HEADER as HEADER1=HEADER1 ()
val MPC_DECLS as MPC_DECLS1=MPC_DECLS1 ()
val G_RULE_LIST as G_RULE_LIST1=G_RULE_LIST1 ()
 in (HEADER,MPC_DECLS,rev G_RULE_LIST) end
)
 in (LrTable.NT 0,(result,HEADER1left,G_RULE_LIST1right),rest671) end
| (1,(_,(MlyValue.MPC_DECL MPC_DECL1,MPC_DECLleft,MPC_DECL1right))::(_
,(MlyValue.MPC_DECLS MPC_DECLS1,MPC_DECLS1left,_))::rest671) => let 
val result=MlyValue.MPC_DECLS(fn _ => let val MPC_DECLS as MPC_DECLS1=
MPC_DECLS1 ()
val MPC_DECL as MPC_DECL1=MPC_DECL1 ()
 in (join_decls(MPC_DECLS,MPC_DECL,inputSource,MPC_DECLleft)) end
)
 in (LrTable.NT 5,(result,MPC_DECLS1left,MPC_DECL1right),rest671) end
| (2,rest671) => let val result=MlyValue.MPC_DECLS(fn _ => (
DECL {prec=nil,nonterm=NONE,term=NONE,eop=nil,control=nil,
                   keyword=nil,change=nil,
                   value=nil}
))
 in (LrTable.NT 5,(result,defaultPos,defaultPos),rest671) end
| (3,(_,(MlyValue.CONSTR_LIST CONSTR_LIST1,_,CONSTR_LIST1right))::(_,(
_,TERM1left,_))::rest671) => let val result=MlyValue.MPC_DECL(fn _ => 
let val CONSTR_LIST as CONSTR_LIST1=CONSTR_LIST1 ()
 in (
DECL { prec=nil,nonterm=NONE,
               term = SOME CONSTR_LIST, eop =nil,control=nil,
                change=nil,keyword=nil,
                value=nil}
) end
)
 in (LrTable.NT 4,(result,TERM1left,CONSTR_LIST1right),rest671) end
| (4,(_,(MlyValue.CONSTR_LIST CONSTR_LIST1,_,CONSTR_LIST1right))::(_,(
_,NONTERM1left,_))::rest671) => let val result=MlyValue.MPC_DECL(fn _
 => let val CONSTR_LIST as CONSTR_LIST1=CONSTR_LIST1 ()
 in (
DECL { prec=nil,control=nil,nonterm= SOME CONSTR_LIST,
               term = NONE, eop=nil,change=nil,keyword=nil,
               value=nil}
) end
)
 in (LrTable.NT 4,(result,NONTERM1left,CONSTR_LIST1right),rest671) end
| (5,(_,(MlyValue.ID_LIST ID_LIST1,_,ID_LIST1right))::(_,(
MlyValue.PREC PREC1,PREC1left,_))::rest671) => let val result=
MlyValue.MPC_DECL(fn _ => let val PREC as PREC1=PREC1 ()
val ID_LIST as ID_LIST1=ID_LIST1 ()
 in (
DECL {prec= [(PREC,ID_LIST)],control=nil,
              nonterm=NONE,term=NONE,eop=nil,change=nil,
              keyword=nil,value=nil}
) end
)
 in (LrTable.NT 4,(result,PREC1left,ID_LIST1right),rest671) end
| (6,(_,(MlyValue.ID ID1,_,ID1right))::(_,(_,START1left,_))::rest671)
 => let val result=MlyValue.MPC_DECL(fn _ => let val ID as ID1=ID1 ()
 in (
DECL {prec=nil,control=[START_SYM (symbolMake ID)],nonterm=NONE,
               term = NONE, eop = nil,change=nil,keyword=nil,
               value=nil}
) end
)
 in (LrTable.NT 4,(result,START1left,ID1right),rest671) end
| (7,(_,(MlyValue.ID_LIST ID_LIST1,_,ID_LIST1right))::(_,(_,
PERCENT_EOP1left,_))::rest671) => let val result=MlyValue.MPC_DECL(fn 
_ => let val ID_LIST as ID_LIST1=ID_LIST1 ()
 in (
DECL {prec=nil,control=nil,nonterm=NONE,term=NONE,
                eop=ID_LIST, change=nil,keyword=nil,
                value=nil}
) end
)
 in (LrTable.NT 4,(result,PERCENT_EOP1left,ID_LIST1right),rest671) end
| (8,(_,(MlyValue.ID_LIST ID_LIST1,_,ID_LIST1right))::(_,(_,
KEYWORD1left,_))::rest671) => let val result=MlyValue.MPC_DECL(fn _
 => let val ID_LIST as ID_LIST1=ID_LIST1 ()
 in (
DECL {prec=nil,control=nil,nonterm=NONE,term=NONE,eop=nil,
                change=nil,keyword=ID_LIST,
                value=nil}
) end
)
 in (LrTable.NT 4,(result,KEYWORD1left,ID_LIST1right),rest671) end
| (9,(_,(MlyValue.ID_LIST ID_LIST1,_,ID_LIST1right))::(_,(_,
PREFER1left,_))::rest671) => let val result=MlyValue.MPC_DECL(fn _ => 
let val ID_LIST as ID_LIST1=ID_LIST1 ()
 in (
DECL {prec=nil,control=nil,nonterm=NONE,term=NONE,eop=nil,
                    change=map (fn i=>([],[i])) ID_LIST,keyword=nil,
                    value=nil}
) end
)
 in (LrTable.NT 4,(result,PREFER1left,ID_LIST1right),rest671) end
| (10,(_,(MlyValue.CHANGE_DECL CHANGE_DECL1,_,CHANGE_DECL1right))::(_,
(_,CHANGE1left,_))::rest671) => let val result=MlyValue.MPC_DECL(fn _
 => let val CHANGE_DECL as CHANGE_DECL1=CHANGE_DECL1 ()
 in (
DECL {prec=nil,control=nil,nonterm=NONE,term=NONE,eop=nil,
                change=CHANGE_DECL,keyword=nil,
                value=nil}
) end
)
 in (LrTable.NT 4,(result,CHANGE1left,CHANGE_DECL1right),rest671) end
| (11,(_,(MlyValue.SUBST_DECL SUBST_DECL1,_,SUBST_DECL1right))::(_,(_,
SUBST1left,_))::rest671) => let val result=MlyValue.MPC_DECL(fn _ => 
let val SUBST_DECL as SUBST_DECL1=SUBST_DECL1 ()
 in (
DECL {prec=nil,control=nil,nonterm=NONE,term=NONE,eop=nil,
                change=SUBST_DECL,keyword=nil,
                value=nil}
) end
)
 in (LrTable.NT 4,(result,SUBST1left,SUBST_DECL1right),rest671) end
| (12,(_,(MlyValue.ID_LIST ID_LIST1,_,ID_LIST1right))::(_,(_,
NOSHIFT1left,_))::rest671) => let val result=MlyValue.MPC_DECL(fn _
 => let val ID_LIST as ID_LIST1=ID_LIST1 ()
 in (
DECL {prec=nil,control=[NSHIFT ID_LIST],nonterm=NONE,term=NONE,
                    eop=nil,change=nil,keyword=nil,
                    value=nil}
) end
)
 in (LrTable.NT 4,(result,NOSHIFT1left,ID_LIST1right),rest671) end
| (13,(_,(MlyValue.PROG PROG1,_,PROG1right))::(_,(_,
PERCENT_HEADER1left,_))::rest671) => let val result=MlyValue.MPC_DECL(
fn _ => let val PROG as PROG1=PROG1 ()
 in (
DECL {prec=nil,control=[FUNCTOR PROG],nonterm=NONE,term=NONE,
                    eop=nil,change=nil,keyword=nil,
                    value=nil}
) end
)
 in (LrTable.NT 4,(result,PERCENT_HEADER1left,PROG1right),rest671) end
| (14,(_,(MlyValue.ID ID1,_,ID1right))::(_,(_,NAME1left,_))::rest671)
 => let val result=MlyValue.MPC_DECL(fn _ => let val ID as ID1=ID1 ()
 in (
DECL {prec=nil,control=[PARSER_NAME (symbolMake ID)],
                    nonterm=NONE,term=NONE,
                    eop=nil,change=nil,keyword=nil, value=nil}
) end
)
 in (LrTable.NT 4,(result,NAME1left,ID1right),rest671) end
| (15,(_,(MlyValue.TY TY1,_,TY1right))::_::(_,(MlyValue.PROG PROG1,_,_
))::(_,(_,PERCENT_ARG1left,_))::rest671) => let val result=
MlyValue.MPC_DECL(fn _ => let val PROG as PROG1=PROG1 ()
val TY as TY1=TY1 ()
 in (
DECL {prec=nil,control=[PARSE_ARG(PROG,TY)],nonterm=NONE,
                    term=NONE,eop=nil,change=nil,keyword=nil,
                     value=nil}
) end
)
 in (LrTable.NT 4,(result,PERCENT_ARG1left,TY1right),rest671) end
| (16,(_,(_,VERBOSE1left,VERBOSE1right))::rest671) => let val result=
MlyValue.MPC_DECL(fn _ => (
DECL {prec=nil,control=[Hdr.VERBOSE],
                nonterm=NONE,term=NONE,eop=nil,
                change=nil,keyword=nil,
                value=nil}
))
 in (LrTable.NT 4,(result,VERBOSE1left,VERBOSE1right),rest671) end
| (17,(_,(_,NODEFAULT1left,NODEFAULT1right))::rest671) => let val 
result=MlyValue.MPC_DECL(fn _ => (
DECL {prec=nil,control=[Hdr.NODEFAULT],
                nonterm=NONE,term=NONE,eop=nil,
                change=nil,keyword=nil,
                value=nil}
))
 in (LrTable.NT 4,(result,NODEFAULT1left,NODEFAULT1right),rest671) end
| (18,(_,(_,PERCENT_PURE1left,PERCENT_PURE1right))::rest671) => let 
val result=MlyValue.MPC_DECL(fn _ => (
DECL {prec=nil,control=[Hdr.PURE],
                nonterm=NONE,term=NONE,eop=nil,
                change=nil,keyword=nil,
                value=nil}
))
 in (LrTable.NT 4,(result,PERCENT_PURE1left,PERCENT_PURE1right),
rest671) end
| (19,(_,(MlyValue.TY TY1,_,TY1right))::(_,(_,PERCENT_POS1left,_))::
rest671) => let val result=MlyValue.MPC_DECL(fn _ => let val TY as TY1
=TY1 ()
 in (
DECL {prec=nil,control=[Hdr.POS TY],
                nonterm=NONE,term=NONE,eop=nil,
                change=nil,keyword=nil,
                value=nil}
) end
)
 in (LrTable.NT 4,(result,PERCENT_POS1left,TY1right),rest671) end
| (20,(_,(MlyValue.PROG PROG1,_,PROG1right))::(_,(MlyValue.ID ID1,_,_)
)::(_,(_,VALUE1left,_))::rest671) => let val result=MlyValue.MPC_DECL(
fn _ => let val ID as ID1=ID1 ()
val PROG as PROG1=PROG1 ()
 in (
DECL {prec=nil,control=nil,
                nonterm=NONE,term=NONE,eop=nil,
                change=nil,keyword=nil,
                value=[(symbolMake ID,PROG)]}
) end
)
 in (LrTable.NT 4,(result,VALUE1left,PROG1right),rest671) end
| (21,(_,(MlyValue.CHANGE_DECL CHANGE_DECL1,_,CHANGE_DECL1right))::_::
(_,(MlyValue.CHANGE_DEC CHANGE_DEC1,CHANGE_DEC1left,_))::rest671) => 
let val result=MlyValue.CHANGE_DECL(fn _ => let val CHANGE_DEC as 
CHANGE_DEC1=CHANGE_DEC1 ()
val CHANGE_DECL as CHANGE_DECL1=CHANGE_DECL1 ()
 in (CHANGE_DEC :: CHANGE_DECL) end
)
 in (LrTable.NT 14,(result,CHANGE_DEC1left,CHANGE_DECL1right),rest671)
 end
| (22,(_,(MlyValue.CHANGE_DEC CHANGE_DEC1,CHANGE_DEC1left,
CHANGE_DEC1right))::rest671) => let val result=MlyValue.CHANGE_DECL(
fn _ => let val CHANGE_DEC as CHANGE_DEC1=CHANGE_DEC1 ()
 in ([CHANGE_DEC]) end
)
 in (LrTable.NT 14,(result,CHANGE_DEC1left,CHANGE_DEC1right),rest671)
 end
| (23,(_,(MlyValue.ID_LIST ID_LIST2,_,ID_LIST2right))::_::(_,(
MlyValue.ID_LIST ID_LIST1,ID_LIST1left,_))::rest671) => let val result
=MlyValue.CHANGE_DEC(fn _ => let val ID_LIST1=ID_LIST1 ()
val ID_LIST2=ID_LIST2 ()
 in (ID_LIST1, ID_LIST2) end
)
 in (LrTable.NT 15,(result,ID_LIST1left,ID_LIST2right),rest671) end
| (24,(_,(MlyValue.SUBST_DECL SUBST_DECL1,_,SUBST_DECL1right))::_::(_,
(MlyValue.SUBST_DEC SUBST_DEC1,SUBST_DEC1left,_))::rest671) => let 
val result=MlyValue.SUBST_DECL(fn _ => let val SUBST_DEC as SUBST_DEC1
=SUBST_DEC1 ()
val SUBST_DECL as SUBST_DECL1=SUBST_DECL1 ()
 in (SUBST_DEC :: SUBST_DECL) end
)
 in (LrTable.NT 12,(result,SUBST_DEC1left,SUBST_DECL1right),rest671)
 end
| (25,(_,(MlyValue.SUBST_DEC SUBST_DEC1,SUBST_DEC1left,SUBST_DEC1right
))::rest671) => let val result=MlyValue.SUBST_DECL(fn _ => let val 
SUBST_DEC as SUBST_DEC1=SUBST_DEC1 ()
 in ([SUBST_DEC]) end
)
 in (LrTable.NT 12,(result,SUBST_DEC1left,SUBST_DEC1right),rest671)
 end
| (26,(_,(MlyValue.ID ID2,_,ID2right))::_::(_,(MlyValue.ID ID1,ID1left
,_))::rest671) => let val result=MlyValue.SUBST_DEC(fn _ => let val 
ID1=ID1 ()
val ID2=ID2 ()
 in ([symbolMake ID2],[symbolMake ID1]) end
)
 in (LrTable.NT 13,(result,ID1left,ID2right),rest671) end
| (27,(_,(MlyValue.TY TY1,_,TY1right))::_::(_,(MlyValue.ID ID1,_,_))::
_::(_,(MlyValue.CONSTR_LIST CONSTR_LIST1,CONSTR_LIST1left,_))::rest671
) => let val result=MlyValue.CONSTR_LIST(fn _ => let val CONSTR_LIST
 as CONSTR_LIST1=CONSTR_LIST1 ()
val ID as ID1=ID1 ()
val TY as TY1=TY1 ()
 in ((symbolMake ID,SOME (tyMake TY))::CONSTR_LIST) end
)
 in (LrTable.NT 1,(result,CONSTR_LIST1left,TY1right),rest671) end
| (28,(_,(MlyValue.ID ID1,_,ID1right))::_::(_,(MlyValue.CONSTR_LIST 
CONSTR_LIST1,CONSTR_LIST1left,_))::rest671) => let val result=
MlyValue.CONSTR_LIST(fn _ => let val CONSTR_LIST as CONSTR_LIST1=
CONSTR_LIST1 ()
val ID as ID1=ID1 ()
 in ((symbolMake ID,NONE)::CONSTR_LIST) end
)
 in (LrTable.NT 1,(result,CONSTR_LIST1left,ID1right),rest671) end
| (29,(_,(MlyValue.TY TY1,_,TY1right))::_::(_,(MlyValue.ID ID1,ID1left
,_))::rest671) => let val result=MlyValue.CONSTR_LIST(fn _ => let val 
ID as ID1=ID1 ()
val TY as TY1=TY1 ()
 in ([(symbolMake ID,SOME (tyMake TY))]) end
)
 in (LrTable.NT 1,(result,ID1left,TY1right),rest671) end
| (30,(_,(MlyValue.ID ID1,ID1left,ID1right))::rest671) => let val 
result=MlyValue.CONSTR_LIST(fn _ => let val ID as ID1=ID1 ()
 in ([(symbolMake ID,NONE)]) end
)
 in (LrTable.NT 1,(result,ID1left,ID1right),rest671) end
| (31,(_,(MlyValue.RHS_LIST RHS_LIST1,_,RHS_LIST1right))::_::(_,(
MlyValue.ID ID1,ID1left,_))::rest671) => let val result=
MlyValue.G_RULE(fn _ => let val ID as ID1=ID1 ()
val RHS_LIST as RHS_LIST1=RHS_LIST1 ()
 in (
map (fn {rhs,code,prec} =>
                  Hdr.RULE {lhs=symbolMake ID,rhs=rhs,
                               code=code,prec=prec})
         RHS_LIST
) end
)
 in (LrTable.NT 9,(result,ID1left,RHS_LIST1right),rest671) end
| (32,(_,(MlyValue.G_RULE G_RULE1,_,G_RULE1right))::(_,(
MlyValue.G_RULE_LIST G_RULE_LIST1,G_RULE_LIST1left,_))::rest671) => 
let val result=MlyValue.G_RULE_LIST(fn _ => let val G_RULE_LIST as 
G_RULE_LIST1=G_RULE_LIST1 ()
val G_RULE as G_RULE1=G_RULE1 ()
 in (G_RULE@G_RULE_LIST) end
)
 in (LrTable.NT 10,(result,G_RULE_LIST1left,G_RULE1right),rest671) end
| (33,(_,(MlyValue.G_RULE G_RULE1,G_RULE1left,G_RULE1right))::rest671)
 => let val result=MlyValue.G_RULE_LIST(fn _ => let val G_RULE as 
G_RULE1=G_RULE1 ()
 in (G_RULE) end
)
 in (LrTable.NT 10,(result,G_RULE1left,G_RULE1right),rest671) end
| (34,(_,(MlyValue.ID_LIST ID_LIST1,_,ID_LIST1right))::(_,(MlyValue.ID
 ID1,ID1left,_))::rest671) => let val result=MlyValue.ID_LIST(fn _ => 
let val ID as ID1=ID1 ()
val ID_LIST as ID_LIST1=ID_LIST1 ()
 in (symbolMake ID :: ID_LIST) end
)
 in (LrTable.NT 2,(result,ID1left,ID_LIST1right),rest671) end
| (35,rest671) => let val result=MlyValue.ID_LIST(fn _ => (nil))
 in (LrTable.NT 2,(result,defaultPos,defaultPos),rest671) end
| (36,(_,(MlyValue.PROG PROG1,_,PROG1right))::(_,(MlyValue.G_RULE_PREC
 G_RULE_PREC1,_,_))::(_,(MlyValue.ID_LIST ID_LIST1,ID_LIST1left,_))::
rest671) => let val result=MlyValue.RHS_LIST(fn _ => let val ID_LIST
 as ID_LIST1=ID_LIST1 ()
val G_RULE_PREC as G_RULE_PREC1=G_RULE_PREC1 ()
val PROG as PROG1=PROG1 ()
 in ([{rhs=ID_LIST,code=PROG,prec=G_RULE_PREC}]) end
)
 in (LrTable.NT 8,(result,ID_LIST1left,PROG1right),rest671) end
| (37,(_,(MlyValue.PROG PROG1,_,PROG1right))::(_,(MlyValue.G_RULE_PREC
 G_RULE_PREC1,_,_))::(_,(MlyValue.ID_LIST ID_LIST1,_,_))::_::(_,(
MlyValue.RHS_LIST RHS_LIST1,RHS_LIST1left,_))::rest671) => let val 
result=MlyValue.RHS_LIST(fn _ => let val RHS_LIST as RHS_LIST1=
RHS_LIST1 ()
val ID_LIST as ID_LIST1=ID_LIST1 ()
val G_RULE_PREC as G_RULE_PREC1=G_RULE_PREC1 ()
val PROG as PROG1=PROG1 ()
 in ({rhs=ID_LIST,code=PROG,prec=G_RULE_PREC}::RHS_LIST) end
)
 in (LrTable.NT 8,(result,RHS_LIST1left,PROG1right),rest671) end
| (38,(_,(MlyValue.TYVAR TYVAR1,TYVAR1left,TYVAR1right))::rest671) => 
let val result=MlyValue.TY(fn _ => let val TYVAR as TYVAR1=TYVAR1 ()
 in (TYVAR) end
)
 in (LrTable.NT 16,(result,TYVAR1left,TYVAR1right),rest671) end
| (39,(_,(_,_,RBRACE1right))::(_,(MlyValue.RECORD_LIST RECORD_LIST1,_,
_))::(_,(_,LBRACE1left,_))::rest671) => let val result=MlyValue.TY(fn 
_ => let val RECORD_LIST as RECORD_LIST1=RECORD_LIST1 ()
 in ("{ "^RECORD_LIST^" } ") end
)
 in (LrTable.NT 16,(result,LBRACE1left,RBRACE1right),rest671) end
| (40,(_,(_,_,RBRACE1right))::(_,(_,LBRACE1left,_))::rest671) => let 
val result=MlyValue.TY(fn _ => ("{}"))
 in (LrTable.NT 16,(result,LBRACE1left,RBRACE1right),rest671) end
| (41,(_,(MlyValue.PROG PROG1,PROG1left,PROG1right))::rest671) => let 
val result=MlyValue.TY(fn _ => let val PROG as PROG1=PROG1 ()
 in (" ( "^PROG^" ) ") end
)
 in (LrTable.NT 16,(result,PROG1left,PROG1right),rest671) end
| (42,(_,(MlyValue.QUAL_ID QUAL_ID1,_,QUAL_ID1right))::(_,(MlyValue.TY
 TY1,TY1left,_))::rest671) => let val result=MlyValue.TY(fn _ => let 
val TY as TY1=TY1 ()
val QUAL_ID as QUAL_ID1=QUAL_ID1 ()
 in (TY^" "^QUAL_ID) end
)
 in (LrTable.NT 16,(result,TY1left,QUAL_ID1right),rest671) end
| (43,(_,(MlyValue.QUAL_ID QUAL_ID1,QUAL_ID1left,QUAL_ID1right))::
rest671) => let val result=MlyValue.TY(fn _ => let val QUAL_ID as 
QUAL_ID1=QUAL_ID1 ()
 in (QUAL_ID) end
)
 in (LrTable.NT 16,(result,QUAL_ID1left,QUAL_ID1right),rest671) end
| (44,(_,(MlyValue.TY TY2,_,TY2right))::_::(_,(MlyValue.TY TY1,TY1left
,_))::rest671) => let val result=MlyValue.TY(fn _ => let val TY1=TY1 
()
val TY2=TY2 ()
 in (TY1^"*"^TY2) end
)
 in (LrTable.NT 16,(result,TY1left,TY2right),rest671) end
| (45,(_,(MlyValue.TY TY2,_,TY2right))::_::(_,(MlyValue.TY TY1,TY1left
,_))::rest671) => let val result=MlyValue.TY(fn _ => let val TY1=TY1 
()
val TY2=TY2 ()
 in (TY1 ^ " -> " ^ TY2) end
)
 in (LrTable.NT 16,(result,TY1left,TY2right),rest671) end
| (46,(_,(MlyValue.TY TY1,_,TY1right))::_::(_,(MlyValue.LABEL LABEL1,_
,_))::_::(_,(MlyValue.RECORD_LIST RECORD_LIST1,RECORD_LIST1left,_))::
rest671) => let val result=MlyValue.RECORD_LIST(fn _ => let val 
RECORD_LIST as RECORD_LIST1=RECORD_LIST1 ()
val LABEL as LABEL1=LABEL1 ()
val TY as TY1=TY1 ()
 in (RECORD_LIST^","^LABEL^":"^TY) end
)
 in (LrTable.NT 7,(result,RECORD_LIST1left,TY1right),rest671) end
| (47,(_,(MlyValue.TY TY1,_,TY1right))::_::(_,(MlyValue.LABEL LABEL1,
LABEL1left,_))::rest671) => let val result=MlyValue.RECORD_LIST(fn _
 => let val LABEL as LABEL1=LABEL1 ()
val TY as TY1=TY1 ()
 in (LABEL^":"^TY) end
)
 in (LrTable.NT 7,(result,LABEL1left,TY1right),rest671) end
| (48,(_,(MlyValue.ID ID1,ID1left,ID1right))::rest671) => let val 
result=MlyValue.QUAL_ID(fn _ => let val ID as ID1=ID1 ()
 in ((fn (a,_) => a) ID) end
)
 in (LrTable.NT 6,(result,ID1left,ID1right),rest671) end
| (49,(_,(MlyValue.QUAL_ID QUAL_ID1,_,QUAL_ID1right))::(_,(
MlyValue.IDDOT IDDOT1,IDDOT1left,_))::rest671) => let val result=
MlyValue.QUAL_ID(fn _ => let val IDDOT as IDDOT1=IDDOT1 ()
val QUAL_ID as QUAL_ID1=QUAL_ID1 ()
 in (IDDOT^QUAL_ID) end
)
 in (LrTable.NT 6,(result,IDDOT1left,QUAL_ID1right),rest671) end
| (50,(_,(MlyValue.ID ID1,ID1left,ID1right))::rest671) => let val 
result=MlyValue.LABEL(fn _ => let val ID as ID1=ID1 ()
 in ((fn (a,_) => a) ID) end
)
 in (LrTable.NT 3,(result,ID1left,ID1right),rest671) end
| (51,(_,(MlyValue.INT INT1,INT1left,INT1right))::rest671) => let val 
result=MlyValue.LABEL(fn _ => let val INT as INT1=INT1 ()
 in (INT) end
)
 in (LrTable.NT 3,(result,INT1left,INT1right),rest671) end
| (52,(_,(MlyValue.ID ID1,_,ID1right))::(_,(_,PREC_TAG1left,_))::
rest671) => let val result=MlyValue.G_RULE_PREC(fn _ => let val ID as 
ID1=ID1 ()
 in (SOME (symbolMake ID)) end
)
 in (LrTable.NT 11,(result,PREC_TAG1left,ID1right),rest671) end
| (53,rest671) => let val result=MlyValue.G_RULE_PREC(fn _ => (NONE))
 in (LrTable.NT 11,(result,defaultPos,defaultPos),rest671) end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.BEGIN x => x
| _ => let exception ParseInternal
        in raise ParseInternal end) a ()
end
end
structure Tokens : Mlyacc_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ASTERISK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun BLOCK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun BAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun CHANGE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun DELIMITER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun HEADER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.HEADER (fn () => i),p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun IDDOT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.IDDOT (fn () => i),p1,p2))
fun PERCENT_HEADER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun KEYWORD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun NAME (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NODEFAULT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun NONTERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun NOSHIFT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun PERCENT_EOP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun PERCENT_PURE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun PERCENT_POS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun PERCENT_ARG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun PREC (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.PREC (fn () => i),p1,p2))
fun PREC_TAG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun PREFER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun PROG (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.PROG (fn () => i),p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun SUBST (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun START (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun TYVAR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.TYVAR (fn () => i),p1,p2))
fun VERBOSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun VALUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun UNKNOWN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.UNKNOWN (fn () => i),p1,p2))
fun BOGUS_VALUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
end
end
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi 
 *
 * $Log$
 * Revision 1.1.1.1  1996/01/31  16:01:42  george
 * Version 109
 * 
 *)

structure LrTable : LR_TABLE = 
    struct
        open Array List
        infix 9 sub
        datatype ('a,'b) pairlist = EMPTY
                                  | PAIR of 'a * 'b * ('a,'b) pairlist
        datatype term = T of int
        datatype nonterm = NT of int
        datatype state = STATE of int
        datatype action = SHIFT of state
                        | REDUCE of int (* rulenum from grammar *)
                        | ACCEPT
                        | ERROR
        exception Goto of state * nonterm
        type table = {states: int, rules : int,initialState: state,
                      action: ((term,action) pairlist * action) array,
                      goto :  (nonterm,state) pairlist array}
        val numStates = fn ({states,...} : table) => states
        val numRules = fn ({rules,...} : table) => rules
        val describeActions =
           fn ({action,...} : table) => 
                   fn (STATE s) => action sub s
        val describeGoto =
           fn ({goto,...} : table) =>
                   fn (STATE s) => goto sub s
        fun findTerm (T term,row,default) =
            let fun find (PAIR (T key,data,r)) =
                       if key < term then find r
                       else if key=term then data
                       else default
                   | find EMPTY = default
            in find row
            end
        fun findNonterm (NT nt,row) =
            let fun find (PAIR (NT key,data,r)) =
                       if key < nt then find r
                       else if key=nt then SOME data
                       else NONE
                   | find EMPTY = NONE
            in find row
            end
        val action = fn ({action,...} : table) =>
                fn (STATE state,term) =>
                  let val (row,default) = action sub state
                  in findTerm(term,row,default)
                  end
        val goto = fn ({goto,...} : table) =>
                        fn (a as (STATE state,nonterm)) =>
                          case findNonterm(nonterm,goto sub state)
                          of SOME state => state
                           | NONE => raise (Goto a)
        val initialState = fn ({initialState,...} : table) => initialState
        val mkLrTable = fn {actions,gotos,initialState,numStates,numRules} =>
             ({action=actions,goto=gotos,
               states=numStates,
               rules=numRules,
               initialState=initialState} : table)
end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi 
 *
 * $Log$
 * Revision 1.1.1.1  1996/01/31  16:01:43  george
 * Version 109
 * 
 *)

(* Stream: a structure implementing a lazy stream.  The signature STREAM
   is found in base.sig *)

structure Stream :> STREAM =
struct
   datatype 'a str = EVAL of 'a * 'a str ref | UNEVAL of (unit->'a)

   type 'a stream = 'a str ref

   fun get(ref(EVAL t)) = t
     | get(s as ref(UNEVAL f)) = 
            let val t = (f(), ref(UNEVAL f)) in s := EVAL t; t end

   fun streamify f = ref(UNEVAL f)
   fun cons(a,s) = ref(EVAL(a,s))

end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi 
 *
 * $Log$
 * Revision 1.3  1996/10/03  03:36:58  jhr
 * Qualified identifiers that are no-longer top-level (quot, rem, min, max).
 *
 * Revision 1.2  1996/02/26  15:02:29  george
 *    print no longer overloaded.
 *    use of makestring has been removed and replaced with Int.toString ..
 *    use of IO replaced with TextIO
 *
 * Revision 1.1.1.1  1996/01/31  16:01:42  george
 * Version 109
 * 
 *)

(* parser.sml:  This is a parser driver for LR tables with an error-recovery
   routine added to it.  The routine used is described in detail in this
   article:

        'A Practical Method for LR and LL Syntactic Error Diagnosis and
         Recovery', by M. Burke and G. Fisher, ACM Transactions on
         Programming Langauges and Systems, Vol. 9, No. 2, April 1987,
         pp. 164-197.

    This program is an implementation is the partial, deferred method discussed
    in the article.  The algorithm and data structures used in the program
    are described below.  

    This program assumes that all semantic actions are delayed.  A semantic
    action should produce a function from unit -> value instead of producing the
    normal value.  The parser returns the semantic value on the top of the
    stack when accept is encountered.  The user can deconstruct this value
    and apply the unit -> value function in it to get the answer.

    It also assumes that the lexer is a lazy stream.

    Data Structures:
    ----------------
        
        * The parser:

           The state stack has the type

                 (state * (semantic value * line # * line #)) list

           The parser keeps a queue of (state stack * lexer pair).  A lexer pair
         consists of a terminal * value pair and a lexer.  This allows the 
         parser to reconstruct the states for terminals to the left of a
         syntax error, and attempt to make error corrections there.

           The queue consists of a pair of lists (x,y).  New additions to
         the queue are cons'ed onto y.  The first element of x is the top
         of the queue.  If x is nil, then y is reversed and used
         in place of x.

    Algorithm:
    ----------

        * The steady-state parser:  

            This parser keeps the length of the queue of state stacks at
        a steady state by always removing an element from the front when
        another element is placed on the end.

            It has these arguments:

           stack: current stack
           queue: value of the queue
           lexPair ((terminal,value),lex stream)

        When SHIFT is encountered, the state to shift to and the value are
        are pushed onto the state stack.  The state stack and lexPair are
        placed on the queue.  The front element of the queue is removed.

        When REDUCTION is encountered, the rule is applied to the current
        stack to yield a triple (nonterm,value,new stack).  A new
        stack is formed by adding (goto(top state of stack,nonterm),value)
        to the stack.

        When ACCEPT is encountered, the top value from the stack and the
        lexer are returned.

        When an ERROR is encountered, fixError is called.  FixError
        takes the arguments to the parser, fixes the error if possible and
        returns a new set of arguments.

        * The distance-parser:

        This parser includes an additional argument distance.  It pushes
        elements on the queue until it has parsed distance tokens, or an
        ACCEPT or ERROR occurs.  It returns a stack, lexer, the number of
        tokens left unparsed, a queue, and an action option.
*)

signature FIFO = 
  sig type 'a queue
      val empty : 'a queue
      exception Empty
      val get : 'a queue -> 'a * 'a queue
      val put : 'a * 'a queue -> 'a queue
  end

(* drt (12/15/89) -- the functor should be used in development work, but
   it wastes space in the release version.

functor ParserGen(structure LrTable : LR_TABLE
                  structure Stream : STREAM) : LR_PARSER =
*)

structure LrParser :> LR_PARSER =
   struct
      structure LrTable = LrTable
      structure Stream = Stream

      structure Token : TOKEN =
        struct
            structure LrTable = LrTable
            datatype ('a,'b) token = TOKEN of LrTable.term * ('a * 'b * 'b)
            val sameToken = fn (TOKEN(t,_),TOKEN(t',_)) => t=t'
        end

      open LrTable
      open Token

      val DEBUG1 = false
      val DEBUG2 = false
      exception ParseError
      exception ParseImpossible of int

      structure Fifo :> FIFO =
        struct
          type 'a queue = ('a list * 'a list)
          val empty = (nil,nil)
          exception Empty
          fun get(a::x, y) = (a, (x,y))
            | get(nil, nil) = raise Empty
            | get(nil, y) = get(rev y, nil)
          fun put(a,(x,y)) = (x,a::y)
        end

      type ('a,'b) elem = (state * ('a * 'b * 'b))
      type ('a,'b) stack = ('a,'b) elem list
      type ('a,'b) lexv = ('a,'b) token
      type ('a,'b) lexpair = ('a,'b) lexv * (('a,'b) lexv Stream.stream)
      type ('a,'b) distanceParse =
                 ('a,'b) lexpair *
                 ('a,'b) stack * 
                 (('a,'b) stack * ('a,'b) lexpair) Fifo.queue *
                 int ->
                   ('a,'b) lexpair *
                   ('a,'b) stack * 
                   (('a,'b) stack * ('a,'b) lexpair) Fifo.queue *
                   int *
                   action option

      type ('a,'b) ecRecord =
         {is_keyword : term -> bool,
          preferred_change : (term list * term list) list,
          error : string * 'b * 'b -> unit,
          errtermvalue : term -> 'a,
          terms : term list,
          showTerminal : term -> string,
          noShift : term -> bool}

      local 
         val print = fn s => TextIO.output(TextIO.stdOut,s)
         val println = fn s => (print s; print "\n")
         val showState = fn (STATE s) => "STATE " ^ (Int.toString s)
      in
        fun printStack(stack: ('a,'b) stack, n: int) =
         case stack
           of (state,_) :: rest =>
                 (print("\t" ^ Int.toString n ^ ": ");
                  println(showState state);
                  printStack(rest, n+1))
            | nil => ()
                
        fun prAction showTerminal
                 (stack as (state,_) :: _, next as (TOKEN (term,_),_), action) =
             (println "Parse: state stack:";
              printStack(stack, 0);
              print("       state="
                         ^ showState state      
                         ^ " next="
                         ^ showTerminal term
                         ^ " action="
                        );
              case action
                of SHIFT state => println ("SHIFT " ^ (showState state))
                 | REDUCE i => println ("REDUCE " ^ (Int.toString i))
                 | ERROR => println "ERROR"
                 | ACCEPT => println "ACCEPT")
        | prAction _ (_,_,action) = ()
     end

    (* ssParse: parser which maintains the queue of (state * lexvalues) in a
        steady-state.  It takes a table, showTerminal function, saction
        function, and fixError function.  It parses until an ACCEPT is
        encountered, or an exception is raised.  When an error is encountered,
        fixError is called with the arguments of parseStep (lexv,stack,and
        queue).  It returns the lexv, and a new stack and queue adjusted so
        that the lexv can be parsed *)
        
    val ssParse =
      fn (table,showTerminal,saction,fixError,arg) =>
        let val prAction = prAction showTerminal
            val action = LrTable.action table
            val goto = LrTable.goto table
            fun parseStep(args as
                         (lexPair as (TOKEN (terminal, value as (_,leftPos,_)),
                                      lexer
                                      ),
                          stack as (state,_) :: _,
                          queue)) =
              let val nextAction = action (state,terminal)
                  val _ = if DEBUG1 then prAction(stack,lexPair,nextAction)
                          else ()
              in case nextAction
                 of SHIFT s =>
                  let val newStack = (s,value) :: stack
                      val newLexPair = Stream.get lexer
                      val (_,newQueue) =Fifo.get(Fifo.put((newStack,newLexPair),
                                                            queue))
                  in parseStep(newLexPair,(s,value)::stack,newQueue)
                  end
                 | REDUCE i =>
                     (case saction(i,leftPos,stack,arg)
                      of (nonterm,value,stack as (state,_) :: _) =>
                          parseStep(lexPair,(goto(state,nonterm),value)::stack,
                                    queue)
                       | _ => raise (ParseImpossible 197))
                 | ERROR => parseStep(fixError args)
                 | ACCEPT => 
                        (case stack
                         of (_,(topvalue,_,_)) :: _ =>
                                let val (token,restLexer) = lexPair
                                in (topvalue,Stream.cons(token,restLexer))
                                end
                          | _ => raise (ParseImpossible 202))
              end
            | parseStep _ = raise (ParseImpossible 204)
        in parseStep
        end

    (*  distanceParse: parse until n tokens are shifted, or accept or
        error are encountered.  Takes a table, showTerminal function, and
        semantic action function.  Returns a parser which takes a lexPair
        (lex result * lexer), a state stack, a queue, and a distance
        (must be > 0) to parse.  The parser returns a new lex-value, a stack
        with the nth token shifted on top, a queue, a distance, and action
        option. *)

    val distanceParse =
      fn (table,showTerminal,saction,arg) =>
        let val prAction = prAction showTerminal
            val action = LrTable.action table
            val goto = LrTable.goto table
            fun parseStep(lexPair,stack,queue,0) = (lexPair,stack,queue,0,NONE)
              | parseStep(lexPair as (TOKEN (terminal, value as (_,leftPos,_)),
                                      lexer
                                     ),
                          stack as (state,_) :: _,
                          queue,distance) =
              let val nextAction = action(state,terminal)
                  val _ = if DEBUG1 then prAction(stack,lexPair,nextAction)
                          else ()
              in case nextAction
                 of SHIFT s =>
                  let val newStack = (s,value) :: stack
                      val newLexPair = Stream.get lexer
                  in parseStep(newLexPair,(s,value)::stack,
                               Fifo.put((newStack,newLexPair),queue),distance-1)
                  end
                 | REDUCE i =>
                    (case saction(i,leftPos,stack,arg)
                      of (nonterm,value,stack as (state,_) :: _) =>
                         parseStep(lexPair,(goto(state,nonterm),value)::stack,
                                 queue,distance)
                      | _ => raise (ParseImpossible 240))
                 | ERROR => (lexPair,stack,queue,distance,SOME nextAction)
                 | ACCEPT => (lexPair,stack,queue,distance,SOME nextAction)
              end
           | parseStep _ = raise (ParseImpossible 242)
        in parseStep : ('a,'b) distanceParse 
        end

(* mkFixError: function to create fixError function which adjusts parser state
   so that parse may continue in the presence of an error *)

fun mkFixError({is_keyword,terms,errtermvalue,
              preferred_change,noShift,
              showTerminal,error,...} : ('a,'b) ecRecord,
             distanceParse : ('a,'b) distanceParse,
             minAdvance,maxAdvance) 

            (lexv as (TOKEN (term,value as (_,leftPos,_)),_),stack,queue) =
    let val _ = if DEBUG2 then
                        error("syntax error found at " ^ (showTerminal term),
                              leftPos,leftPos)
                else ()

        fun tokAt(t,p) = TOKEN(t,(errtermvalue t,p,p))

        val minDelta = 3

        (* pull all the state * lexv elements from the queue *)

        val stateList = 
           let fun f q = let val (elem,newQueue) = Fifo.get q
                         in elem :: (f newQueue)
                         end handle Fifo.Empty => nil
           in f queue
           end

        (* now number elements of stateList, giving distance from
           error token *)

        val (_, numStateList) =
              List.foldr (fn (a,(num,r)) => (num+1,(a,num)::r)) (0, []) stateList

        (* Represent the set of potential changes as a linked list.

           Values of datatype Change hold information about a potential change.

           oper = oper to be applied
           pos = the # of the element in stateList that would be altered.
           distance = the number of tokens beyond the error token which the
             change allows us to parse.
           new = new terminal * value pair at that point
           orig = original terminal * value pair at the point being changed.
         *)

        datatype ('a,'b) change = CHANGE of
           {pos : int, distance : int, leftPos: 'b, rightPos: 'b,
            new : ('a,'b) lexv list, orig : ('a,'b) lexv list}


         val showTerms = concat o map (fn TOKEN(t,_) => " " ^ showTerminal t)

         val printChange = fn c =>
          let val CHANGE {distance,new,orig,pos,...} = c
          in (print ("{distance= " ^ (Int.toString distance));
              print (",orig ="); print(showTerms orig);
              print (",new ="); print(showTerms new);
              print (",pos= " ^ (Int.toString pos));
              print "}\n")
          end

        val printChangeList = app printChange

(* parse: given a lexPair, a stack, and the distance from the error
   token, return the distance past the error token that we are able to parse.*)

        fun parse (lexPair,stack,queuePos : int) =
            case distanceParse(lexPair,stack,Fifo.empty,queuePos+maxAdvance+1)
             of (_,_,_,distance,SOME ACCEPT) => 
                        if maxAdvance-distance-1 >= 0 
                            then maxAdvance 
                            else maxAdvance-distance-1
              | (_,_,_,distance,_) => maxAdvance - distance - 1

(* catList: concatenate results of scanning list *)

        fun catList l f = List.foldr (fn(a,r)=> f a @ r) [] l

        fun keywordsDelta new = if List.exists (fn(TOKEN(t,_))=>is_keyword t) new
                       then minDelta else 0

        fun tryChange{lex,stack,pos,leftPos,rightPos,orig,new} =
             let val lex' = List.foldr (fn (t',p)=>(t',Stream.cons p)) lex new
                 val distance = parse(lex',stack,pos+length new-length orig)
              in if distance >= minAdvance + keywordsDelta new 
                   then [CHANGE{pos=pos,leftPos=leftPos,rightPos=rightPos,
                                distance=distance,orig=orig,new=new}] 
                   else []
             end


(* tryDelete: Try to delete n terminals.
              Return single-element [success] or nil.
              Do not delete unshiftable terminals. *)


    fun tryDelete n ((stack,lexPair as (TOKEN(term,(_,l,r)),_)),qPos) =
        let fun del(0,accum,left,right,lexPair) =
                  tryChange{lex=lexPair,stack=stack,
                            pos=qPos,leftPos=left,rightPos=right,
                            orig=rev accum, new=[]}
              | del(n,accum,left,right,(tok as TOKEN(term,(_,_,r)),lexer)) =
                   if noShift term then []
                   else del(n-1,tok::accum,left,r,Stream.get lexer)
         in del(n,[],l,r,lexPair)
        end

(* tryInsert: try to insert tokens before the current terminal;
       return a list of the successes  *)

        fun tryInsert((stack,lexPair as (TOKEN(_,(_,l,_)),_)),queuePos) =
               catList terms (fn t =>
                 tryChange{lex=lexPair,stack=stack,
                           pos=queuePos,orig=[],new=[tokAt(t,l)],
                           leftPos=l,rightPos=l})
                              
(* trySubst: try to substitute tokens for the current terminal;
       return a list of the successes  *)

        fun trySubst ((stack,lexPair as (orig as TOKEN (term,(_,l,r)),lexer)),
                      queuePos) =
              if noShift term then []
              else
                  catList terms (fn t =>
                      tryChange{lex=Stream.get lexer,stack=stack,
                                pos=queuePos,
                                leftPos=l,rightPos=r,orig=[orig],
                                new=[tokAt(t,r)]})

     (* do_delete(toks,lexPair) tries to delete tokens "toks" from "lexPair".
         If it succeeds, returns SOME(toks',l,r,lp), where
             toks' is the actual tokens (with positions and values) deleted,
             (l,r) are the (leftmost,rightmost) position of toks', 
             lp is what remains of the stream after deletion 
     *)
        fun do_delete(nil,lp as (TOKEN(_,(_,l,_)),_)) = SOME(nil,l,l,lp)
          | do_delete([t],(tok as TOKEN(t',(_,l,r)),lp')) =
               if t=t'
                   then SOME([tok],l,r,Stream.get lp')
                   else NONE
          | do_delete(t::rest,(tok as TOKEN(t',(_,l,r)),lp')) =
               if t=t'
                   then case do_delete(rest,Stream.get lp')
                         of SOME(deleted,l',r',lp'') =>
                               SOME(tok::deleted,l,r',lp'')
                          | NONE => NONE
                   else NONE
                             
        fun tryPreferred((stack,lexPair),queuePos) =
            catList preferred_change (fn (delete,insert) =>
               if List.exists noShift delete then [] (* should give warning at
                                                 parser-generation time *)
               else case do_delete(delete,lexPair)
                     of SOME(deleted,l,r,lp) => 
                            tryChange{lex=lp,stack=stack,pos=queuePos,
                                      leftPos=l,rightPos=r,orig=deleted,
                                      new=map (fn t=>(tokAt(t,r))) insert}
                      | NONE => [])

        val changes = catList numStateList tryPreferred @
                        catList numStateList tryInsert @
                          catList numStateList trySubst @
                            catList numStateList (tryDelete 1) @
                              catList numStateList (tryDelete 2) @
                                catList numStateList (tryDelete 3)

        val findMaxDist = fn l => 
          foldr (fn (CHANGE {distance,...},high) => Int.max(distance,high)) 0 l

(* maxDist: max distance past error taken that we could parse *)

        val maxDist = findMaxDist changes

(* remove changes which did not parse maxDist tokens past the error token *)

        val changes = catList changes 
              (fn(c as CHANGE{distance,...}) => 
                  if distance=maxDist then [c] else [])

      in case changes 
          of (l as change :: _) =>
              let fun print_msg (CHANGE {new,orig,leftPos,rightPos,...}) =
                  let val s = 
                      case (orig,new)
                          of (_::_,[]) => "deleting " ^ (showTerms orig)
                           | ([],_::_) => "inserting " ^ (showTerms new)
                           | _ => "replacing " ^ (showTerms orig) ^
                                 " with " ^ (showTerms new)
                  in error ("syntax error: " ^ s,leftPos,rightPos)
                  end
                   
                  val _ = 
                      (if length l > 1 andalso DEBUG2 then
                           (print "multiple fixes possible; could fix it by:\n";
                            app print_msg l;
                            print "chosen correction:\n")
                       else ();
                       print_msg change)

                  (* findNth: find nth queue entry from the error
                   entry.  Returns the Nth queue entry and the  portion of
                   the queue from the beginning to the nth-1 entry.  The
                   error entry is at the end of the queue.

                   Examples:

                   queue = a b c d e
                   findNth 0 = (e,a b c d)
                   findNth 1 =  (d,a b c)
                   *)

                  val findNth = fn n =>
                      let fun f (h::t,0) = (h,rev t)
                            | f (h::t,n) = f(t,n-1)
                            | f (nil,_) = let exception FindNth
                                          in raise FindNth
                                          end
                      in f (rev stateList,n)
                      end
                
                  val CHANGE {pos,orig,new,...} = change
                  val (last,queueFront) = findNth pos
                  val (stack,lexPair) = last

                  val lp1 = foldl(fn (_,(_,r)) => Stream.get r) lexPair orig
                  val lp2 = foldr(fn(t,r)=>(t,Stream.cons r)) lp1 new

                  val restQueue = 
                      Fifo.put((stack,lp2),
                               foldl Fifo.put Fifo.empty queueFront)

                  val (lexPair,stack,queue,_,_) =
                      distanceParse(lp2,stack,restQueue,pos)

              in (lexPair,stack,queue)
              end
        | nil => (error("syntax error found at " ^ (showTerminal term),
                        leftPos,leftPos); raise ParseError)
    end

   val parse = fn {arg,table,lexer,saction,void,lookahead,
                   ec=ec as {showTerminal,...} : ('a,'b) ecRecord} =>
        let val distance = 15   (* defer distance tokens *)
            val minAdvance = 1  (* must parse at least 1 token past error *)
            val maxAdvance = Int.max(lookahead,0)(* max distance for parse check *)
            val lexPair = Stream.get lexer
            val (TOKEN (_,(_,leftPos,_)),_) = lexPair
            val startStack = [(initialState table,(void,leftPos,leftPos))]
            val startQueue = Fifo.put((startStack,lexPair),Fifo.empty)
            val distanceParse = distanceParse(table,showTerminal,saction,arg)
            val fixError = mkFixError(ec,distanceParse,minAdvance,maxAdvance)
            val ssParse = ssParse(table,showTerminal,saction,fixError,arg)
            fun loop (lexPair,stack,queue,_,SOME ACCEPT) =
                   ssParse(lexPair,stack,queue)
              | loop (lexPair,stack,queue,0,_) = ssParse(lexPair,stack,queue)
              | loop (lexPair,stack,queue,distance,SOME ERROR) =
                 let val (lexPair,stack,queue) = fixError(lexPair,stack,queue)
                 in loop (distanceParse(lexPair,stack,queue,distance))
                 end
              | loop _ = let exception ParseInternal
                         in raise ParseInternal
                         end
        in loop (distanceParse(lexPair,startStack,startQueue,distance))
        end
 end;

(* drt (12/15/89) -- needed only when the code above is functorized

structure LrParser = ParserGen(structure LrTable=LrTable
                             structure Stream=Stream);
*)
functor LexMLYACC(structure Tokens : Mlyacc_TOKENS
                  structure Hdr : HEADER
                                  where type prec = Header.prec
                                  and type inputSource = Header.inputSource
                                      and type pos = int)
                  : ARG_LEXER =
   struct
    structure UserDeclarations =
      struct
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi

   yacc.lex: Lexer specification
 *)

structure Tokens = Tokens
type svalue = Tokens.svalue
type pos = int
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

type lexarg = Hdr.inputSource
type arg = lexarg

open Tokens
val error = Hdr.error
val lineno = Hdr.lineno
val text = Hdr.text

val pcount = ref 0
val commentLevel = ref 0
val actionstart = ref 0

val eof = fn i => (if (!pcount)>0 then
                        error i (!actionstart)
                              " eof encountered in action beginning here !"
                   else (); EOF(!lineno,!lineno))

val Add = fn s => (text := s::(!text))

local val dict = [("%prec",PREC_TAG),("%term",TERM),
                  ("%nonterm",NONTERM), ("%eop",PERCENT_EOP),("%start",START),
                  ("%prefer",PREFER),("%subst",SUBST),("%change",CHANGE),
                  ("%keyword",KEYWORD),("%name",NAME),
                  ("%verbose",VERBOSE), ("%nodefault",NODEFAULT),
                  ("%value",VALUE), ("%noshift",NOSHIFT),
                  ("%header",PERCENT_HEADER),("%pure",PERCENT_PURE),
                  ("%arg",PERCENT_ARG),
                  ("%pos",PERCENT_POS)]
in val lookup =
    fn (s,left,right) =>
         let fun f ((a,d)::b) = if a=s then d(left,right) else f b
               | f nil = UNKNOWN(s,left,right)
         in f dict
         end
end

fun inc (ri as ref i) = (ri := i+1)
fun dec (ri as ref i) = (ri := i-1)

end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
        struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s0 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
val s1 =
"\015\015\015\015\015\015\015\015\015\015\021\015\015\015\015\015\
\\015\015\015\015\015\015\015\015\015\015\015\015\015\015\015\015\
\\015\015\015\015\015\019\015\015\017\015\015\015\015\015\015\015\
\\015\015\015\015\015\015\015\015\015\015\015\015\015\015\015\015\
\\015\015\015\015\015\015\015\015\015\015\015\015\015\015\015\015\
\\015\015\015\015\015\015\015\015\015\015\015\015\015\015\015\015\
\\015\015\015\015\015\015\015\015\015\015\015\015\015\015\015\015\
\\015\015\015\015\015\015\015\015\015\015\015\015\015\015\015\015\
\\015"
val s3 =
"\022\022\022\022\022\022\022\022\022\065\067\022\022\022\022\022\
\\022\022\022\022\022\022\022\022\022\022\022\022\022\022\022\022\
\\065\022\022\022\022\045\022\043\041\022\040\022\039\037\022\022\
\\035\035\035\035\035\035\035\035\035\035\034\022\022\022\022\022\
\\022\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\022\022\022\022\022\
\\022\026\026\026\026\026\031\026\026\026\026\026\026\026\026\029\
\\026\026\026\026\026\026\026\026\026\026\026\025\024\023\022\022\
\\022"
val s5 =
"\068\068\068\068\068\068\068\068\068\068\021\068\068\068\068\068\
\\068\068\068\068\068\068\068\068\068\068\068\068\068\068\068\068\
\\068\068\072\068\068\068\068\068\070\069\068\068\068\068\068\068\
\\068\068\068\068\068\068\068\068\068\068\068\068\068\068\068\068\
\\068\068\068\068\068\068\068\068\068\068\068\068\068\068\068\068\
\\068\068\068\068\068\068\068\068\068\068\068\068\068\068\068\068\
\\068\068\068\068\068\068\068\068\068\068\068\068\068\068\068\068\
\\068\068\068\068\068\068\068\068\068\068\068\068\068\068\068\068\
\\068"
val s7 =
"\073\073\073\073\073\073\073\073\073\075\021\073\073\073\073\073\
\\073\073\073\073\073\073\073\073\073\073\073\073\073\073\073\073\
\\075\073\073\073\073\073\073\073\073\073\073\073\073\073\073\073\
\\073\073\073\073\073\073\073\073\073\073\073\073\073\073\073\073\
\\073\073\073\073\073\073\073\073\073\073\073\073\073\073\073\073\
\\073\073\073\073\073\073\073\073\073\073\073\073\074\073\073\073\
\\073\073\073\073\073\073\073\073\073\073\073\073\073\073\073\073\
\\073\073\073\073\073\073\073\073\073\073\073\073\073\073\073\073\
\\073"
val s9 =
"\077\077\077\077\077\077\077\077\077\077\021\077\077\077\077\077\
\\077\077\077\077\077\077\077\077\077\077\077\077\077\077\077\077\
\\077\077\077\077\077\077\077\077\081\080\078\077\077\077\077\077\
\\077\077\077\077\077\077\077\077\077\077\077\077\077\077\077\077\
\\077\077\077\077\077\077\077\077\077\077\077\077\077\077\077\077\
\\077\077\077\077\077\077\077\077\077\077\077\077\077\077\077\077\
\\077\077\077\077\077\077\077\077\077\077\077\077\077\077\077\077\
\\077\077\077\077\077\077\077\077\077\077\077\077\077\077\077\077\
\\077"
val s11 =
"\083\083\083\083\083\083\083\083\083\083\088\083\083\083\083\083\
\\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\
\\083\083\087\083\083\083\083\083\083\083\083\083\083\083\083\083\
\\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\
\\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\
\\083\083\083\083\083\083\083\083\083\083\083\083\084\083\083\083\
\\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\
\\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\
\\083"
val s13 =
"\089\089\089\089\089\089\089\089\089\089\021\089\089\089\089\089\
\\089\089\089\089\089\089\089\089\089\089\089\089\089\089\089\089\
\\089\089\089\089\089\089\089\089\093\092\090\089\089\089\089\089\
\\089\089\089\089\089\089\089\089\089\089\089\089\089\089\089\089\
\\089\089\089\089\089\089\089\089\089\089\089\089\089\089\089\089\
\\089\089\089\089\089\089\089\089\089\089\089\089\089\089\089\089\
\\089\089\089\089\089\089\089\089\089\089\089\089\089\089\089\089\
\\089\089\089\089\089\089\089\089\089\089\089\089\089\089\089\089\
\\089"
val s15 =
"\016\016\016\016\016\016\016\016\016\016\000\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\000\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016"
val s17 =
"\016\016\016\016\016\016\016\016\016\016\000\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\000\016\016\016\016\018\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016"
val s19 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\020\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
val s26 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\027\000\000\000\000\000\000\028\000\
\\027\027\027\027\027\027\027\027\027\027\000\000\000\000\000\000\
\\000\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\027\027\027\027\027\027\027\027\027\000\000\000\000\027\
\\000\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\027\027\027\027\027\027\027\027\027\000\000\000\000\000\
\\000"
val s29 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\027\000\000\000\000\000\000\028\000\
\\027\027\027\027\027\027\027\027\027\027\000\000\000\000\000\000\
\\000\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\027\027\027\027\027\027\027\027\027\000\000\000\000\027\
\\000\027\027\027\027\027\030\027\027\027\027\027\027\027\027\027\
\\027\027\027\027\027\027\027\027\027\027\027\000\000\000\000\000\
\\000"
val s31 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\027\000\000\000\000\000\000\028\000\
\\027\027\027\027\027\027\027\027\027\027\000\000\000\000\000\000\
\\000\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\027\027\027\027\027\027\027\027\027\000\000\000\000\027\
\\000\027\027\027\027\027\027\027\027\027\027\027\027\027\027\032\
\\027\027\027\027\027\027\027\027\027\027\027\000\000\000\000\000\
\\000"
val s32 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\027\000\000\000\000\000\000\028\000\
\\027\027\027\027\027\027\027\027\027\027\000\000\000\000\000\000\
\\000\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\027\027\027\027\027\027\027\027\027\000\000\000\000\027\
\\000\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\033\027\027\027\027\027\027\027\027\000\000\000\000\000\
\\000"
val s35 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\036\036\036\036\036\036\036\036\036\036\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
val s37 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\038\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
val s41 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\042\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
val s43 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\044\000\000\000\000\000\000\000\000\
\\044\044\044\044\044\044\044\044\044\044\000\000\000\000\000\000\
\\000\044\044\044\044\044\044\044\044\044\044\044\044\044\044\044\
\\044\044\044\044\044\044\044\044\044\044\044\000\000\000\000\044\
\\000\044\044\044\044\044\044\044\044\044\044\044\044\044\044\044\
\\044\044\044\044\044\044\044\044\044\044\044\000\000\000\000\000\
\\000"
val s45 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\064\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\046\
\\000\046\046\046\046\046\046\046\046\046\046\046\060\046\052\046\
\\046\046\047\046\046\046\046\046\046\046\046\000\000\000\000\000\
\\000"
val s46 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\046\
\\000\046\046\046\046\046\046\046\046\046\046\046\046\046\046\046\
\\046\046\046\046\046\046\046\046\046\046\046\000\000\000\000\000\
\\000"
val s47 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\046\
\\000\046\046\046\046\046\046\046\046\048\046\046\046\046\046\046\
\\046\046\046\046\046\046\046\046\046\046\046\000\000\000\000\000\
\\000"
val s48 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\046\
\\000\046\046\046\046\046\046\049\046\046\046\046\046\046\046\046\
\\046\046\046\046\046\046\046\046\046\046\046\000\000\000\000\000\
\\000"
val s49 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\046\
\\000\046\046\046\046\046\046\046\050\046\046\046\046\046\046\046\
\\046\046\046\046\046\046\046\046\046\046\046\000\000\000\000\000\
\\000"
val s50 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\046\
\\000\046\046\046\046\046\046\046\046\046\046\046\046\046\046\046\
\\046\046\046\046\051\046\046\046\046\046\046\000\000\000\000\000\
\\000"
val s52 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\046\
\\000\046\046\046\046\046\046\046\046\046\046\046\046\046\046\053\
\\046\046\046\046\046\046\046\046\046\046\046\000\000\000\000\000\
\\000"
val s53 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\046\
\\000\046\046\046\046\046\046\046\046\046\046\046\046\046\054\046\
\\046\046\046\046\046\046\046\046\046\046\046\000\000\000\000\000\
\\000"
val s54 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\046\
\\000\055\046\046\046\046\046\046\046\046\046\046\046\046\046\046\
\\046\046\046\046\046\046\046\046\046\046\046\000\000\000\000\000\
\\000"
val s55 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\046\
\\000\046\046\046\046\046\046\046\046\046\046\046\046\046\046\046\
\\046\046\046\056\046\046\046\046\046\046\046\000\000\000\000\000\
\\000"
val s56 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\046\
\\000\046\046\046\046\046\046\046\046\046\046\046\046\046\046\046\
\\046\046\046\057\046\046\046\046\046\046\046\000\000\000\000\000\
\\000"
val s57 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\046\
\\000\046\046\046\046\046\046\046\046\046\046\046\046\046\046\058\
\\046\046\046\046\046\046\046\046\046\046\046\000\000\000\000\000\
\\000"
val s58 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\046\
\\000\046\046\059\046\046\046\046\046\046\046\046\046\046\046\046\
\\046\046\046\046\046\046\046\046\046\046\046\000\000\000\000\000\
\\000"
val s60 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\046\
\\000\046\046\046\046\061\046\046\046\046\046\046\046\046\046\046\
\\046\046\046\046\046\046\046\046\046\046\046\000\000\000\000\000\
\\000"
val s61 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\046\
\\000\046\046\046\046\046\062\046\046\046\046\046\046\046\046\046\
\\046\046\046\046\046\046\046\046\046\046\046\000\000\000\000\000\
\\000"
val s62 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\046\
\\000\046\046\046\046\046\046\046\046\046\046\046\046\046\046\046\
\\046\046\046\046\063\046\046\046\046\046\046\000\000\000\000\000\
\\000"
val s65 =
"\000\000\000\000\000\000\000\000\000\066\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\066\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
val s68 =
"\068\068\068\068\068\068\068\068\068\068\000\068\068\068\068\068\
\\068\068\068\068\068\068\068\068\068\068\068\068\068\068\068\068\
\\068\068\000\068\068\068\068\068\000\000\068\068\068\068\068\068\
\\068\068\068\068\068\068\068\068\068\068\068\068\068\068\068\068\
\\068\068\068\068\068\068\068\068\068\068\068\068\068\068\068\068\
\\068\068\068\068\068\068\068\068\068\068\068\068\068\068\068\068\
\\068\068\068\068\068\068\068\068\068\068\068\068\068\068\068\068\
\\068\068\068\068\068\068\068\068\068\068\068\068\068\068\068\068\
\\068"
val s70 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\071\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
val s75 =
"\000\000\000\000\000\000\000\000\000\076\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\076\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
val s77 =
"\077\077\077\077\077\077\077\077\077\077\000\077\077\077\077\077\
\\077\077\077\077\077\077\077\077\077\077\077\077\077\077\077\077\
\\077\077\077\077\077\077\077\077\000\000\000\077\077\077\077\077\
\\077\077\077\077\077\077\077\077\077\077\077\077\077\077\077\077\
\\077\077\077\077\077\077\077\077\077\077\077\077\077\077\077\077\
\\077\077\077\077\077\077\077\077\077\077\077\077\077\077\077\077\
\\077\077\077\077\077\077\077\077\077\077\077\077\077\077\077\077\
\\077\077\077\077\077\077\077\077\077\077\077\077\077\077\077\077\
\\077"
val s78 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\079\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
val s81 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\082\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
val s83 =
"\083\083\083\083\083\083\083\083\083\083\000\083\083\083\083\083\
\\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\
\\083\083\000\083\083\083\083\083\083\083\083\083\083\083\083\083\
\\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\
\\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\
\\083\083\083\083\083\083\083\083\083\083\083\083\000\083\083\083\
\\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\
\\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\083\
\\083"
val s84 =
"\000\000\000\000\000\000\000\000\000\086\086\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\086\000\085\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
val s89 =
"\089\089\089\089\089\089\089\089\089\089\000\089\089\089\089\089\
\\089\089\089\089\089\089\089\089\089\089\089\089\089\089\089\089\
\\089\089\089\089\089\089\089\089\000\000\000\089\089\089\089\089\
\\089\089\089\089\089\089\089\089\089\089\089\089\089\089\089\089\
\\089\089\089\089\089\089\089\089\089\089\089\089\089\089\089\089\
\\089\089\089\089\089\089\089\089\089\089\089\089\089\089\089\089\
\\089\089\089\089\089\089\089\089\089\089\089\089\089\089\089\089\
\\089\089\089\089\089\089\089\089\089\089\089\089\089\089\089\089\
\\089"
val s90 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\091\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
val s93 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\094\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
in Vector.fromList
[{fin = [], trans = s0},
{fin = [], trans = s1},
{fin = [], trans = s1},
{fin = [], trans = s3},
{fin = [], trans = s3},
{fin = [], trans = s5},
{fin = [], trans = s5},
{fin = [], trans = s7},
{fin = [], trans = s7},
{fin = [], trans = s9},
{fin = [], trans = s9},
{fin = [], trans = s11},
{fin = [], trans = s11},
{fin = [], trans = s13},
{fin = [], trans = s13},
{fin = [(N 11),(N 18)], trans = s15},
{fin = [(N 11)], trans = s15},
{fin = [(N 11),(N 18)], trans = s17},
{fin = [(N 2),(N 11)], trans = s15},
{fin = [(N 18)], trans = s19},
{fin = [(N 14)], trans = s0},
{fin = [(N 16)], trans = s0},
{fin = [(N 94)], trans = s0},
{fin = [(N 36),(N 94)], trans = s0},
{fin = [(N 87),(N 94)], trans = s0},
{fin = [(N 34),(N 94)], trans = s0},
{fin = [(N 90),(N 94)], trans = s26},
{fin = [(N 90)], trans = s26},
{fin = [(N 77)], trans = s0},
{fin = [(N 90),(N 94)], trans = s29},
{fin = [(N 28),(N 90)], trans = s26},
{fin = [(N 90),(N 94)], trans = s31},
{fin = [(N 90)], trans = s32},
{fin = [(N 32),(N 90)], trans = s26},
{fin = [(N 85),(N 94)], trans = s0},
{fin = [(N 80),(N 94)], trans = s35},
{fin = [(N 80)], trans = s35},
{fin = [(N 94)], trans = s37},
{fin = [(N 43)], trans = s0},
{fin = [(N 38),(N 94)], trans = s0},
{fin = [(N 40),(N 94)], trans = s0},
{fin = [(N 92),(N 94)], trans = s41},
{fin = [(N 5)], trans = s0},
{fin = [(N 73),(N 94)], trans = s43},
{fin = [(N 73)], trans = s43},
{fin = [(N 94)], trans = s45},
{fin = [(N 70)], trans = s46},
{fin = [(N 70)], trans = s47},
{fin = [(N 70)], trans = s48},
{fin = [(N 70)], trans = s49},
{fin = [(N 70)], trans = s50},
{fin = [(N 56),(N 70)], trans = s46},
{fin = [(N 70)], trans = s52},
{fin = [(N 70)], trans = s53},
{fin = [(N 70)], trans = s54},
{fin = [(N 70)], trans = s55},
{fin = [(N 70)], trans = s56},
{fin = [(N 70)], trans = s57},
{fin = [(N 70)], trans = s58},
{fin = [(N 66),(N 70)], trans = s46},
{fin = [(N 70)], trans = s60},
{fin = [(N 70)], trans = s61},
{fin = [(N 70)], trans = s62},
{fin = [(N 49),(N 70)], trans = s46},
{fin = [(N 83)], trans = s0},
{fin = [(N 25),(N 94)], trans = s65},
{fin = [(N 25)], trans = s65},
{fin = [(N 20)], trans = s0},
{fin = [(N 103)], trans = s68},
{fin = [(N 98)], trans = s0},
{fin = [(N 96)], trans = s70},
{fin = [(N 8)], trans = s0},
{fin = [(N 100)], trans = s0},
{fin = [(N 147)], trans = s0},
{fin = [(N 145),(N 147)], trans = s0},
{fin = [(N 143),(N 147)], trans = s75},
{fin = [(N 143)], trans = s75},
{fin = [(N 114)], trans = s77},
{fin = [(N 105)], trans = s78},
{fin = [(N 108)], trans = s0},
{fin = [(N 105)], trans = s0},
{fin = [(N 105)], trans = s81},
{fin = [(N 111)], trans = s0},
{fin = [(N 134)], trans = s83},
{fin = [(N 129)], trans = s84},
{fin = [(N 137)], trans = s0},
{fin = [(N 140)], trans = s0},
{fin = [(N 127)], trans = s0},
{fin = [(N 131)], trans = s0},
{fin = [(N 125)], trans = s89},
{fin = [(N 116)], trans = s90},
{fin = [(N 119)], trans = s0},
{fin = [(N 116)], trans = s0},
{fin = [(N 116)], trans = s93},
{fin = [(N 122)], trans = s0}]
end
structure StartStates =
        struct
        datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val A = STARTSTATE 3;
val CODE = STARTSTATE 5;
val COMMENT = STARTSTATE 9;
val EMPTYCOMMENT = STARTSTATE 13;
val F = STARTSTATE 7;
val INITIAL = STARTSTATE 1;
val STRING = STARTSTATE 11;

end
type result = UserDeclarations.lexresult
        exception LexerError (* raised if illegal leaf action tried *)
end

fun makeLexer yyinput = 
let 
        val yyb = ref "\n"              (* buffer *)
        val yybl = ref 1                (*buffer length *)
        val yybufpos = ref 1            (* location of next character to use *)
        val yygone = ref 1              (* position in file of beginning of buffer *)
        val yydone = ref false          (* eof found yet? *)
        val yybegin = ref 1             (*Current 'start state' for lexer *)

        val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
                 yybegin := x

fun lex (yyarg as (inputSource)) =
let fun continue() : Internal.result = 
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
        let fun action (i,nil) = raise LexError
        | action (i,nil::l) = action (i-1,l)
        | action (i,(node::acts)::l) =
                case node of
                    Internal.N yyk => 
                        (let val yytext = substring(!yyb,i0,i-i0)
                             val yypos = i0+ !yygone
                        open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

                        (* Application actions *)

  100 => (Add yytext; YYBEGIN STRING; continue())
| 103 => (Add yytext; continue())
| 105 => (Add yytext; continue())
| 108 => (Add yytext; dec commentLevel;
                    if !commentLevel=0
                         then BOGUS_VALUE(!lineno,!lineno)
                         else continue()
                   )
| 11 => (Add yytext; continue())
| 111 => (Add yytext; inc commentLevel; continue())
| 114 => (Add yytext; continue())
| 116 => (continue())
| 119 => (dec commentLevel;
                          if !commentLevel=0 then YYBEGIN A else ();
                          continue ())
| 122 => (inc commentLevel; continue())
| 125 => (continue())
| 127 => (Add yytext; YYBEGIN CODE; continue())
| 129 => (Add yytext; continue())
| 131 => (Add yytext; error inputSource (!lineno) "unclosed string";
                    inc lineno; YYBEGIN CODE; continue())
| 134 => (Add yytext; continue())
| 137 => (Add yytext; continue())
| 14 => (YYBEGIN A; HEADER (concat (rev (!text)),!lineno,!lineno))
| 140 => (Add yytext;
                        if substring(yytext,1,1)="\n" then inc lineno else ();
                        YYBEGIN F; continue())
| 143 => (Add yytext; continue())
| 145 => (Add yytext; YYBEGIN STRING; continue())
| 147 => (Add yytext; error inputSource (!lineno) "unclosed string";
                    YYBEGIN CODE; continue())
| 16 => (Add yytext; inc lineno; continue())
| 18 => (Add yytext; continue())
| 2 => (Add yytext; YYBEGIN COMMENT; commentLevel := 1;
                    continue() before YYBEGIN INITIAL)
| 20 => (inc lineno; continue ())
| 25 => (continue())
| 28 => (OF(!lineno,!lineno))
| 32 => (FOR(!lineno,!lineno))
| 34 => (LBRACE(!lineno,!lineno))
| 36 => (RBRACE(!lineno,!lineno))
| 38 => (COMMA(!lineno,!lineno))
| 40 => (ASTERISK(!lineno,!lineno))
| 43 => (ARROW(!lineno,!lineno))
| 49 => (PREC(Hdr.LEFT,!lineno,!lineno))
| 5 => (YYBEGIN EMPTYCOMMENT; commentLevel := 1; continue())
| 56 => (PREC(Hdr.RIGHT,!lineno,!lineno))
| 66 => (PREC(Hdr.NONASSOC,!lineno,!lineno))
| 70 => (lookup(yytext,!lineno,!lineno))
| 73 => (TYVAR(yytext,!lineno,!lineno))
| 77 => (IDDOT(yytext,!lineno,!lineno))
| 8 => (Add yytext; YYBEGIN COMMENT; commentLevel := 1;
                    continue() before YYBEGIN CODE)
| 80 => (INT (yytext,!lineno,!lineno))
| 83 => (DELIMITER(!lineno,!lineno))
| 85 => (COLON(!lineno,!lineno))
| 87 => (BAR(!lineno,!lineno))
| 90 => (ID ((yytext,!lineno),!lineno,!lineno))
| 92 => (pcount := 1; actionstart := (!lineno);
                    text := nil; YYBEGIN CODE; continue() before YYBEGIN A)
| 94 => (UNKNOWN(yytext,!lineno,!lineno))
| 96 => (inc pcount; Add yytext; continue())
| 98 => (dec pcount;
                    if !pcount = 0 then
                         PROG (concat (rev (!text)),!lineno,!lineno)
                    else (Add yytext; continue()))
| _ => raise Internal.LexerError

                ) end )

        val {fin,trans} = Vector.sub(Internal.tab, s)
        val NewAcceptingLeaves = fin::AcceptingLeaves
        in if l = !yybl then
             if trans = #trans(Vector.sub(Internal.tab,0))
               then action(l,NewAcceptingLeaves
) else      let val newchars= if !yydone then "" else yyinput 1024
            in if (size newchars)=0
                  then (yydone := true;
                        if (l=i0) then UserDeclarations.eof yyarg
                                  else action(l,NewAcceptingLeaves))
                  else (if i0=l then yyb := newchars
                     else yyb := substring(!yyb,i0,l-i0)^newchars;
                     yygone := !yygone+i0;
                     yybl := size (!yyb);
                     scan (s,AcceptingLeaves,l-i0,0))
            end
          else let val NewChar = Char.ord(String.sub(!yyb,l))
                val NewState = if NewChar<128 then Char.ord(String.sub(trans,NewChar)) else Char.ord(String.sub(trans,128))
                in if NewState=0 then action(l,NewAcceptingLeaves)
                else scan(NewState,NewAcceptingLeaves,l+1,i0)
        end
        end
(*
        val start= if substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
        in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
in continue end
  in lex
  end
end
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi 
 *
 * $Log$
 * Revision 1.1.1.1  1996/01/31  16:01:42  george
 * Version 109
 * 
 *)

(* functor Join creates a user parser by putting together a Lexer structure,
   an LrValues structure, and a polymorphic parser structure.  Note that
   the Lexer and LrValues structure must share the type pos (i.e. the type
   of line numbers), the type svalues for semantic values, and the type
   of tokens.
*)

functor Join(structure Lex : LEXER
             structure ParserData: PARSER_DATA
             structure LrParser : LR_PARSER
             sharing ParserData.LrTable = LrParser.LrTable
             sharing ParserData.Token = LrParser.Token
             sharing type Lex.UserDeclarations.svalue = ParserData.svalue
             sharing type Lex.UserDeclarations.pos = ParserData.pos
             sharing type Lex.UserDeclarations.token = ParserData.Token.token)
                 : PARSER =
struct
    structure Token = ParserData.Token
    structure Stream = LrParser.Stream
 
    exception ParseError = LrParser.ParseError

    type arg = ParserData.arg
    type pos = ParserData.pos
    type result = ParserData.result
    type svalue = ParserData.svalue
    val makeLexer = LrParser.Stream.streamify o Lex.makeLexer
    val parse = fn (lookahead,lexer,error,arg) =>
        (fn (a,b) => (ParserData.Actions.extract a,b))
     (LrParser.parse {table = ParserData.table,
                lexer=lexer,
                lookahead=lookahead,
                saction = ParserData.Actions.actions,
                arg=arg,
                void= ParserData.Actions.void,
                ec = {is_keyword = ParserData.EC.is_keyword,
                      noShift = ParserData.EC.noShift,
                      preferred_change = ParserData.EC.preferred_change,
                      errtermvalue = ParserData.EC.errtermvalue,
                      error=error,
                      showTerminal = ParserData.EC.showTerminal,
                      terms = ParserData.EC.terms}}
      )
     val sameToken = Token.sameToken
end

(* functor JoinWithArg creates a variant of the parser structure produced 
   above.  In this case, the makeLexer take an additional argument before
   yielding a value of type unit -> (svalue,pos) token
 *)

functor JoinWithArg(structure Lex : ARG_LEXER
             structure ParserData: PARSER_DATA
             structure LrParser : LR_PARSER
             sharing ParserData.LrTable = LrParser.LrTable
             sharing ParserData.Token = LrParser.Token
             sharing type Lex.UserDeclarations.svalue = ParserData.svalue
             sharing type Lex.UserDeclarations.pos = ParserData.pos
             sharing type Lex.UserDeclarations.token = ParserData.Token.token)
                 : ARG_PARSER  =
struct
    structure Token = ParserData.Token
    structure Stream = LrParser.Stream

    exception ParseError = LrParser.ParseError

    type arg = ParserData.arg
    type lexarg = Lex.UserDeclarations.arg
    type pos = ParserData.pos
    type result = ParserData.result
    type svalue = ParserData.svalue

    val makeLexer = fn s => fn arg =>
                 LrParser.Stream.streamify (Lex.makeLexer s arg)
    val parse = fn (lookahead,lexer,error,arg) =>
        (fn (a,b) => (ParserData.Actions.extract a,b))
     (LrParser.parse {table = ParserData.table,
                lexer=lexer,
                lookahead=lookahead,
                saction = ParserData.Actions.actions,
                arg=arg,
                void= ParserData.Actions.void,
                ec = {is_keyword = ParserData.EC.is_keyword,
                      noShift = ParserData.EC.noShift,
                      preferred_change = ParserData.EC.preferred_change,
                      errtermvalue = ParserData.EC.errtermvalue,
                      error=error,
                      showTerminal = ParserData.EC.showTerminal,
                      terms = ParserData.EC.terms}}
      )
    val sameToken = Token.sameToken
end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi 
 *
 * $Log$
 * Revision 1.2  1996/02/26  15:02:38  george
 *    print no longer overloaded.
 *    use of makestring has been removed and replaced with Int.toString ..
 *    use of IO replaced with TextIO
 *
 * Revision 1.1.1.1  1996/01/31  16:01:46  george
 * Version 109
 * 
 *)

functor ParseGenParserFun(S : sig
                                 structure Parser : ARG_PARSER
                                 structure Header : HEADER
                                 sharing type Parser.pos = Header.pos
                                 sharing type Parser.result = Header.parseResult
                                 sharing type Parser.arg = Header.inputSource =
                                    Parser.lexarg
                              end where type Header.pos = int
                         ) : PARSE_GEN_PARSER =

 struct
    open S
      structure Header = Header
      val parse = fn file =>
          let
              val in_str = TextIO.openIn file
              val source = Header.newSource(file,in_str,TextIO.stdOut)
              val error = fn (s : string,i:int,_) =>
                              Header.error source i s
              val stream =  Parser.makeLexer (fn i => (TextIO.inputN(in_str,i)))
                            source
              val (result,_) = (Header.lineno := 1; 
                                Header.text := nil;
                                Parser.parse(15,stream,error,source))
           in (TextIO.closeIn in_str; (result,source))
           end
  end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi 
 *
 * $Log$
 * Revision 1.1.1.1  1996/01/31  16:01:47  george
 * Version 109
 * 
 *)

(* Implementation of ordered sets using ordered lists and red-black trees.  The
   code for red-black trees was originally written by Norris Boyd, which was
   modified for use here.
*)   

(* ordered sets implemented using ordered lists.

   Upper bound running times for functions implemented here:

   app  = O(n)
   card = O(n)
   closure = O(n^2)
   difference = O(n+m), where n,m = the size of the two sets used here.
   empty = O(1)
   exists = O(n)
   find = O(n)
   fold = O(n)
   insert = O(n)
   is_empty = O(1)
   make_list = O(1)
   make_set = O(n^2)
   partition = O(n)
   remove = O(n)
   revfold = O(n)
   select_arb = O(1)
   set_eq = O(n), where n = the cardinality of the smaller set
   set_gt = O(n), ditto
   singleton = O(1)
   union = O(n+m)
*)

functor ListOrdSet(B : sig type elem
                        val gt : elem * elem -> bool
                        val eq : elem * elem -> bool
                    end ) : ORDSET =

struct
 type elem = B.elem
 val elem_gt = B.gt
 val elem_eq = B.eq 

 type set = elem list
 exception Select_arb
 val empty = nil

 val insert = fn (key,s) =>
        let fun f (l as (h::t)) =
                 if elem_gt(key,h) then h::(f t)
                 else if elem_eq(key,h) then key::t
                 else key::l
              | f nil = [key]
        in f s
        end
                
 val select_arb = fn nil => raise Select_arb
                   | a::b => a

 val exists = fn (key,s) =>
        let fun f (h::t) = if elem_gt(key,h) then f t
                           else elem_eq(h,key) 
              | f nil = false
        in f s
        end

 val find = fn (key,s) =>
        let fun f (h::t) = if elem_gt(key,h) then f t
                           else if elem_eq(h,key) then SOME h
                           else NONE
              | f nil = NONE
        in f s
        end
   
 fun revfold f lst init = List.foldl f init lst
 fun fold f lst init = List.foldr f init lst
 val app = List.app

fun set_eq(h::t,h'::t') = 
        (case elem_eq(h,h')
          of true => set_eq(t,t')
           | a => a)
  | set_eq(nil,nil) = true
  | set_eq _ = false

fun set_gt(h::t,h'::t') =
        (case elem_gt(h,h')
          of false => (case (elem_eq(h,h'))
                        of true => set_gt(t,t')
                         | a => a)
           |  a => a)
  | set_gt(_::_,nil) = true
  | set_gt _ = false
                
fun union(a as (h::t),b as (h'::t')) =
          if elem_gt(h',h) then h::union(t,b)
          else if elem_eq(h,h') then h::union(t,t')
          else h'::union(a,t')
  | union(nil,s) = s
  | union(s,nil) = s

val make_list = fn s => s

val is_empty = fn nil => true | _ => false

val make_set = fn l => List.foldr insert [] l

val partition = fn f => fn s =>
    fold (fn (e,(yes,no)) =>
            if (f e) then (e::yes,no) else (e::no,yes)) s (nil,nil)

val remove = fn (e,s) =>
    let fun f (l as (h::t)) = if elem_gt(h,e) then l
                              else if elem_eq(h,e) then t
                              else h::(f t)
          | f nil = nil
    in f s
    end

 (* difference: X-Y *)

 fun difference (nil,_) = nil
   | difference (r,nil) = r
   | difference (a as (h::t),b as (h'::t')) =
          if elem_gt (h',h) then h::difference(t,b)
          else if elem_eq(h',h) then difference(t,t')
          else difference(a,t')

 fun singleton X = [X]

 fun card(S) = fold (fn (a,count) => count+1) S 0

      local
            fun closure'(from, f, result) =
              if is_empty from then result
              else
                let val (more,result) =
                        fold (fn (a,(more',result')) =>
                                let val more = f a
                                    val new = difference(more,result)
                                in (union(more',new),union(result',new))
                                end) from
                                 (empty,result)
                in closure'(more,f,result)
                end
      in
         fun closure(start, f) = closure'(start, f, start)
      end
end

(* ordered set implemented using red-black trees:

   Upper bound running time of the functions below:

   app: O(n)
   card: O(n)
   closure: O(n^2 ln n)
   difference: O(n ln n)
   empty: O(1)
   exists: O(ln n)
   find: O(ln n)
   fold: O(n)
   insert: O(ln n)
   is_empty: O(1)
   make_list: O(n)
   make_set: O(n ln n)
   partition: O(n ln n)
   remove: O(n ln n)
   revfold: O(n)
   select_arb: O(1)
   set_eq: O(n)
   set_gt: O(n)
   singleton: O(1)
   union: O(n ln n)
*)

functor RbOrdSet (B : sig type elem
                         val eq : (elem*elem) -> bool
                         val gt : (elem*elem) -> bool
                     end
                ) : ORDSET =
struct

 type elem = B.elem
 val elem_gt = B.gt
 val elem_eq = B.eq 

 datatype Color = RED | BLACK

 abstype set = EMPTY | TREE of (B.elem * Color * set * set)
 with exception Select_arb
      val empty = EMPTY

 fun insert(key,t) =
  let fun f EMPTY = TREE(key,RED,EMPTY,EMPTY)
        | f (TREE(k,BLACK,l,r)) =
            if elem_gt (key,k)
            then case f r
                 of r as TREE(rk,RED, rl as TREE(rlk,RED,rll,rlr),rr) =>
                        (case l
                         of TREE(lk,RED,ll,lr) =>
                                TREE(k,RED,TREE(lk,BLACK,ll,lr),
                                           TREE(rk,BLACK,rl,rr))
                          | _ => TREE(rlk,BLACK,TREE(k,RED,l,rll),
                                                TREE(rk,RED,rlr,rr)))
                  | r as TREE(rk,RED,rl, rr as TREE(rrk,RED,rrl,rrr)) =>
                        (case l
                         of TREE(lk,RED,ll,lr) =>
                                TREE(k,RED,TREE(lk,BLACK,ll,lr),
                                           TREE(rk,BLACK,rl,rr))
                          | _ => TREE(rk,BLACK,TREE(k,RED,l,rl),rr))
                  | r => TREE(k,BLACK,l,r)
            else if elem_gt(k,key)
            then case f l
                 of l as TREE(lk,RED,ll, lr as TREE(lrk,RED,lrl,lrr)) =>
                        (case r
                         of TREE(rk,RED,rl,rr) =>
                                TREE(k,RED,TREE(lk,BLACK,ll,lr),
                                           TREE(rk,BLACK,rl,rr))
                          | _ => TREE(lrk,BLACK,TREE(lk,RED,ll,lrl),
                                                TREE(k,RED,lrr,r)))
                  | l as TREE(lk,RED, ll as TREE(llk,RED,lll,llr), lr) =>
                        (case r
                         of TREE(rk,RED,rl,rr) =>
                                TREE(k,RED,TREE(lk,BLACK,ll,lr),
                                           TREE(rk,BLACK,rl,rr))
                          | _ => TREE(lk,BLACK,ll,TREE(k,RED,lr,r)))
                  | l => TREE(k,BLACK,l,r)
            else TREE(key,BLACK,l,r)
        | f (TREE(k,RED,l,r)) =
            if elem_gt(key,k) then TREE(k,RED,l, f r)
            else if elem_gt(k,key) then TREE(k,RED, f l, r)
            else TREE(key,RED,l,r)
   in case f t
      of TREE(k,RED, l as TREE(_,RED,_,_), r) => TREE(k,BLACK,l,r)
       | TREE(k,RED, l, r as TREE(_,RED,_,_)) => TREE(k,BLACK,l,r)
       | t => t
  end

 fun select_arb (TREE(k,_,l,r)) = k
   | select_arb EMPTY = raise Select_arb
   
 fun exists(key,t) =
  let fun look EMPTY = false
        | look (TREE(k,_,l,r)) =
                if elem_gt(k,key) then look l
                else if elem_gt(key,k) then look r
                else true
   in look t
   end

 fun find(key,t) =
  let fun look EMPTY = NONE
        | look (TREE(k,_,l,r)) =
                if elem_gt(k,key) then look l
                else if elem_gt(key,k) then look r
                else SOME k
   in look t
  end

  fun revfold f t start =
     let fun scan (EMPTY,value) = value
           | scan (TREE(k,_,l,r),value) = scan(r,f(k,scan(l,value)))
     in scan(t,start)
     end

   fun fold f t start =
        let fun scan(EMPTY,value) = value
              | scan(TREE(k,_,l,r),value) = scan(l,f(k,scan(r,value)))
        in scan(t,start)
        end

   fun app f t =
      let fun scan EMPTY = ()
            | scan(TREE(k,_,l,r)) = (scan l; f k; scan r)
      in scan t
      end

(* equal_tree : test if two trees are equal.  Two trees are equal if
   the set of leaves are equal *)

   fun set_eq (tree1 as (TREE _),tree2 as (TREE _)) =
     let datatype pos = L | R | M
         exception Done
         fun getvalue(stack as ((a,position)::b)) =
            (case a
             of (TREE(k,_,l,r)) =>
                (case position
                 of L => getvalue ((l,L)::(a,M)::b)
                  | M => (k,case r of  EMPTY => b | _ => (a,R)::b)
                  | R => getvalue ((r,L)::b)
                 )
              | EMPTY => getvalue b
             )
            | getvalue(nil) = raise Done
          fun f (nil,nil) = true
            | f (s1 as (_ :: _),s2 as (_ :: _ )) =
                          let val (v1,news1) = getvalue s1
                              and (v2,news2) = getvalue s2
                          in (elem_eq(v1,v2)) andalso f(news1,news2)
                          end
            | f _ = false
      in f ((tree1,L)::nil,(tree2,L)::nil) handle Done => false
      end
    | set_eq (EMPTY,EMPTY) = true
    | set_eq _ = false

   (* gt_tree : Test if tree1 is greater than tree 2 *)

   fun set_gt (tree1,tree2) =
     let datatype pos = L | R | M
         exception Done
         fun getvalue(stack as ((a,position)::b)) =
            (case a
             of (TREE(k,_,l,r)) =>
                (case position
                 of L => getvalue ((l,L)::(a,M)::b)
                  | M => (k,case r of EMPTY => b | _ => (a,R)::b)
                  | R => getvalue ((r,L)::b)
                 )
              | EMPTY => getvalue b
             )
            | getvalue(nil) = raise Done
          fun f (nil,nil) = false
            | f (s1 as (_ :: _),s2 as (_ :: _ )) =
                          let val (v1,news1) = getvalue s1
                              and (v2,news2) = getvalue s2
                          in (elem_gt(v1,v2)) orelse (elem_eq(v1,v2) andalso f(news1,news2))
                          end
            | f (_,nil) = true
            | f (nil,_) = false
      in f ((tree1,L)::nil,(tree2,L)::nil) handle Done => false
      end

      fun is_empty S = (let val _ = select_arb S in false end
                         handle Select_arb => true)

      fun make_list S = fold (op ::) S nil

      fun make_set l = List.foldr insert empty l

      fun partition F S = fold (fn (a,(Yes,No)) =>
                                if F(a) then (insert(a,Yes),No)
                                else (Yes,insert(a,No)))
                             S (empty,empty)

      fun remove(X, XSet) =
             let val (YSet, _) =
                        partition (fn a => not (elem_eq (X, a))) XSet
             in  YSet
             end

      fun difference(Xs, Ys) =
           fold (fn (p as (a,Xs')) =>
                      if exists(a,Ys) then Xs' else insert p)
           Xs empty

      fun singleton X = insert(X,empty)

      fun card(S) = fold (fn (_,count) => count+1) S 0

      fun union(Xs,Ys)= fold insert Ys Xs

      local
            fun closure'(from, f, result) =
              if is_empty from then result
              else
                let val (more,result) =
                        fold (fn (a,(more',result')) =>
                                let val more = f a
                                    val new = difference(more,result)
                                in (union(more',new),union(result',new))
                                end) from
                                 (empty,result)
                in closure'(more,f,result)
                end
      in
         fun closure(start, f) = closure'(start, f, start)
      end
   end
end
(*
signature TABLE =
   sig
        type 'a table
        type key
        val size : 'a table -> int
        val empty: 'a table
        val exists: (key * 'a table) -> bool
        val find : (key * 'a table)  ->  'a option
        val insert: ((key * 'a) * 'a table) -> 'a table
        val make_table : (key * 'a ) list -> 'a table
        val make_list : 'a table -> (key * 'a) list
        val fold : ((key * 'a) * 'b -> 'b) -> 'a table -> 'b -> 'b
   end
*)
functor Table (B : sig type key
                      val gt : (key * key) -> bool
                     end
                ) : TABLE =
struct

 datatype Color = RED | BLACK
 type key = B.key

 abstype 'a table = EMPTY
                  | TREE of ((B.key * 'a ) * Color * 'a table * 'a table)
 with

 val empty = EMPTY

 fun insert(elem as (key,data),t) =
  let val key_gt = fn (a,_) => B.gt(key,a)
      val key_lt = fn (a,_) => B.gt(a,key)
        fun f EMPTY = TREE(elem,RED,EMPTY,EMPTY)
        | f (TREE(k,BLACK,l,r)) =
            if key_gt k
            then case f r
                 of r as TREE(rk,RED, rl as TREE(rlk,RED,rll,rlr),rr) =>
                        (case l
                         of TREE(lk,RED,ll,lr) =>
                                TREE(k,RED,TREE(lk,BLACK,ll,lr),
                                           TREE(rk,BLACK,rl,rr))
                          | _ => TREE(rlk,BLACK,TREE(k,RED,l,rll),
                                                TREE(rk,RED,rlr,rr)))
                  | r as TREE(rk,RED,rl, rr as TREE(rrk,RED,rrl,rrr)) =>
                        (case l
                         of TREE(lk,RED,ll,lr) =>
                                TREE(k,RED,TREE(lk,BLACK,ll,lr),
                                           TREE(rk,BLACK,rl,rr))
                          | _ => TREE(rk,BLACK,TREE(k,RED,l,rl),rr))
                  | r => TREE(k,BLACK,l,r)
            else if key_lt k
            then case f l
                 of l as TREE(lk,RED,ll, lr as TREE(lrk,RED,lrl,lrr)) =>
                        (case r
                         of TREE(rk,RED,rl,rr) =>
                                TREE(k,RED,TREE(lk,BLACK,ll,lr),
                                           TREE(rk,BLACK,rl,rr))
                          | _ => TREE(lrk,BLACK,TREE(lk,RED,ll,lrl),
                                                TREE(k,RED,lrr,r)))
                  | l as TREE(lk,RED, ll as TREE(llk,RED,lll,llr), lr) =>
                        (case r
                         of TREE(rk,RED,rl,rr) =>
                                TREE(k,RED,TREE(lk,BLACK,ll,lr),
                                           TREE(rk,BLACK,rl,rr))
                          | _ => TREE(lk,BLACK,ll,TREE(k,RED,lr,r)))
                  | l => TREE(k,BLACK,l,r)
            else TREE(elem,BLACK,l,r)
        | f (TREE(k,RED,l,r)) =
            if key_gt k then TREE(k,RED,l, f r)
            else if key_lt k then TREE(k,RED, f l, r)
            else TREE(elem,RED,l,r)
   in case f t
      of TREE(k,RED, l as TREE(_,RED,_,_), r) => TREE(k,BLACK,l,r)
       | TREE(k,RED, l, r as TREE(_,RED,_,_)) => TREE(k,BLACK,l,r)
       | t => t
  end

 fun exists(key,t) =
  let fun look EMPTY = false
        | look (TREE((k,_),_,l,r)) =
                if B.gt(k,key) then look l
                else if B.gt(key,k) then look r
                else true
   in look t
   end

 fun find(key,t) =
  let fun look EMPTY = NONE
        | look (TREE((k,data),_,l,r)) =
                if B.gt(k,key) then look l
                else if B.gt(key,k) then look r
                else SOME data
   in look t
  end

  fun fold f t start =
        let fun scan(EMPTY,value) = value
              | scan(TREE(k,_,l,r),value) = scan(l,f(k,scan(r,value)))
        in scan(t,start)
        end

  fun make_table l = List.foldr insert empty l

  fun size S = fold (fn (_,count) => count+1) S 0

  fun make_list table = fold (op ::) table nil

  end
end;

(* assumes that a functor Table with signature TABLE from table.sml is
   in the environment *)
(*
signature HASH =
  sig
    type table
    type elem

    val size : table -> int
    val add : elem * table -> table
    val find : elem * table -> int option
    val exists : elem * table -> bool
    val empty : table
  end
*)
(* hash: creates a hash table of size n which assigns each distinct member
   a unique integer between 0 and n-1 *)

functor Hash(B : sig type elem
                     val gt : elem * elem -> bool
                 end) : HASH =
struct
    type elem=B.elem
    structure HashTable = Table(type key=B.elem
                                val gt = B.gt)

    type table = {count : int, table : int HashTable.table}

    val empty = {count=0,table=HashTable.empty}
    val size = fn {count,table} => count
    val add = fn (e,{count,table}) =>
                {count=count+1,table=HashTable.insert((e,count),table)}
    val find = fn (e,{table,count}) => HashTable.find(e,table)
    val exists = fn (e,{table,count}) => HashTable.exists(e,table)
end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi 
 *
 * $Log$
 * Revision 1.2  1996/02/26  15:02:31  george
 *    print no longer overloaded.
 *    use of makestring has been removed and replaced with Int.toString ..
 *    use of IO replaced with TextIO
 *
 * Revision 1.1.1.1  1996/01/31  16:01:44  george
 * Version 109
 * 
 *)

functor mkCore(structure IntGrammar : INTGRAMMAR) : CORE =
        struct
                open IntGrammar
                open  Grammar
                structure IntGrammar = IntGrammar
                structure Grammar = Grammar

                datatype item = ITEM of
                                { rule : rule,
                                  dot : int,
                                  rhsAfter : symbol list
                                }

                val eqItem = fn (ITEM{rule=RULE{num=n,...},dot=d,...},
                                 ITEM{rule=RULE{num=m,...},dot=e,...}) =>
                                        n=m andalso d=e

                val gtItem =  fn (ITEM{rule=RULE{num=n,...},dot=d,...},
                                  ITEM{rule=RULE{num=m,...},dot=e,...}) =>
                                        n>m orelse (n=m andalso d>e)

                structure ItemList = ListOrdSet
                        (struct
                                type elem = item
                                val eq = eqItem
                                val gt = gtItem
                        end)
                
                open ItemList
                datatype core = CORE of item list * int

                val gtCore = fn (CORE (a,_),CORE (b,_)) => ItemList.set_gt(a,b)
                val eqCore = fn (CORE (a,_),CORE (b,_)) => ItemList.set_eq(a,b)

                (* functions for printing and debugging *)

                 val prItem = fn (symbolToString,nontermToString,print) =>
                   let val printInt = print o (Int.toString : int -> string)
                       val prSymbol = print o symbolToString
                       val prNonterm = print o nontermToString
                       fun showRest nil = ()
                         | showRest (h::t) = (prSymbol h; print " "; showRest t)
                       fun showRhs (l,0) = (print ". "; showRest l)
                         | showRhs (nil,_) = ()
                         | showRhs (h::t,n) = (prSymbol h;
                                               print " ";
                                               showRhs(t,n-1))
                   in fn (ITEM {rule=RULE {lhs,rhs,rulenum,num,...},
                                dot,rhsAfter,...}) =>
                        (prNonterm lhs; print " : "; showRhs(rhs,dot);
                         case rhsAfter 
                         of nil => (print " (reduce by rule "; 
                                    printInt rulenum;
                                    print ")")
                          | _ => ();
                          if DEBUG then 
                             (print " (num "; printInt num; print ")")
                          else ())
                   end

                 val prCore = fn a as (_,_,print) =>
                    let val prItem = prItem a
                    in fn (CORE (items,state)) =>
                          (print "state ";
                           print (Int.toString state);
                           print ":\n\n";
                           app (fn i => (print "\t";
                                         prItem i; print "\n")) items;
                           print "\n")
                    end
end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi 
 *
 * $Log$
 * Revision 1.1.1.1  1996/01/31  16:01:45  george
 * Version 109
 * 
 *)

functor mkCoreUtils(structure Core : CORE) : CORE_UTILS =
        struct
                open Array List
                infix 9 sub
                val DEBUG = true
                structure Core = Core
                structure IntGrammar = Core.IntGrammar
                structure Grammar = IntGrammar.Grammar

                open Grammar IntGrammar Core

                structure Assoc = SymbolAssoc

                structure NtList = ListOrdSet
                        (struct
                                type elem = nonterm
                                val eq = eqNonterm
                                val gt = gtNonterm
                         end)

        val mkFuncs = fn (GRAMMAR {rules,terms,nonterms,...}) =>
           let val derives=array(nonterms,nil : rule list)

(* sort rules by their lhs nonterminal by placing them in an array indexed
   in their lhs nonterminal *)

               val _ =
                 let val f = fn {lhs=lhs as (NT n), rhs, precedence,rulenum} =>
                        let val rule=RULE{lhs=lhs,rhs=rhs,precedence=precedence,
                                          rulenum=rulenum,num=0}
                        in update(derives,n,rule::(derives sub n))
                        end
                  in app f rules
                  end

(* renumber rules so that rule numbers increase monotonically with
   the number of their lhs nonterminal, and so that rules are numbered
   sequentially.  **Functions below assume that this number is true**, 
   i.e. productions for nonterm i are numbered from j to k, 
   productions for nonterm i+1 are numbered from k+1 to m, and
   productions for nonterm 0 start at 0 *)

                val _ =
                   let val f =
                         fn (RULE{lhs,rhs,precedence,rulenum,num}, (l,i)) =>
                            (RULE{lhs=lhs,rhs=rhs, precedence=precedence,
                                  rulenum=rulenum, num=i}::l,i+1)
                        fun g(i,num) =
                          if i<nonterms then
                            let val (l,n) =
                                        List.foldr f ([], num) (derives sub i)
                            in update(derives,i,rev l); g(i+1,n)
                            end
                          else ()
                    in g(0,0)
                    end

(* list of rules - sorted by rule number. *)

                 val rules = 
                     let fun g i =
                        if i < nonterms then (derives sub i) @ (g (i+1))
                        else nil
                     in g 0
                     end

(* produces: set of productions with nonterminal n as the lhs.  The set
   of productions *must* be sorted by rule number, because functions
   below assume that this list is sorted *)

                val produces = fn (NT n) =>
                  if DEBUG andalso (n<0 orelse n>=nonterms) then
                     let exception Produces of int in raise (Produces n) end
                  else derives sub n

                val memoize = fn f =>
                   let fun loop i = if i = nonterms then nil
                                  else f (NT i) :: (loop (i+1))
                       val data = Array.fromList(loop 0)
                   in fn (NT i) => data sub i
                   end

 (* compute nonterminals which must be added to a closure when a given
    nonterminal is added, i.e all nonterminals C for each nonterminal A such
    that A =*=> Cx *)

                val nontermClosure =
                        let val collectNonterms = fn n =>
                              List.foldr (fn (r,l) =>
                                  case r
                                  of RULE {rhs=NONTERM n :: _,...} =>
                                            NtList.insert(n,l)
                                   | _ => l) NtList.empty (produces n)
                            val closureNonterm = fn n =>
                                   NtList.closure(NtList.singleton n,
                                                  collectNonterms)
                        in memoize closureNonterm
                        end

(* ntShifts: Take the items produced by a nonterminal, and sort them
   by their first symbol.  For each first symbol, make sure the item
   list associated with the symbol is sorted also.   ** This function
   assumes that the item list returned by produces is sorted **

   Create a table of item lists keyed by symbols.  Scan the list
   of items produced by a nonterminal, and insert those with a first
   symbol on to the beginning of the item list for that symbol, creating
   a list if necessary.  Since produces returns an item list that is
   already in order, the list for each symbol will also end up in order.
 *)

                fun sortItems nt =
                 let fun add_item (a as RULE{rhs=symbol::rest,...},r) =
                       let val item = ITEM{rule=a,dot=1,rhsAfter=rest}
                       in Assoc.insert((symbol,case Assoc.find (symbol,r)
                                                of SOME l => item::l
                                                 | NONE => [item]),r)
                       end
                       | add_item (_,r) = r
                 in List.foldr add_item Assoc.empty (produces nt)
                 end

                 val ntShifts = memoize sortItems

(* getNonterms: get the nonterminals with a .  before them in a core.
   Returns a list of nonterminals in ascending order *)

                fun getNonterms l =
                  List.foldr (fn (ITEM {rhsAfter=NONTERM sym ::_, ...},r) =>
                                NtList.insert(sym,r)
                              | (_,r) => r) [] l

(* closureNonterms: compute the nonterminals that would have a . before them
   in the closure of the core.  Returns a list of nonterminals in ascending
   order *)
                fun closureNonterms a =
                        let val nonterms = getNonterms a
                        in List.foldr (fn (nt,r) =>
                                   NtList.union(nontermClosure nt,r))
                           nonterms nonterms
                        end

(* shifts: compute the core sets that result from shift/gotoing on 
   the closure of a kernal set.  The items in core sets are sorted, of
   course.

   (1) compute the core sets that result just from items added
       through the closure operation.
   (2) then add the shift/gotos on kernal items.

   We can do (1) the following way.  Keep a table  which for each shift/goto
symbol gives the list of items that result from shifting or gotoing on the
symbol.  Compute the nonterminals that would have dots before them in the
closure of the kernal set.  For each of these nonterminals, we already have an
item list in sorted order for each possible shift symbol.  Scan the nonterminal
list from back to front.  For each nonterminal, prepend the shift/goto list
for each shift symbol to the list already in the table.

   We end up with the list of items in correct order for each shift/goto
symbol.  We have kept the item lists in order, scanned the nonterminals from
back to front (=> that the items end up in ascending order), and never had any
duplicate items (each item is derived from only one nonterminal). *)

        fun shifts (CORE (itemList,_)) =
            let

(* mergeShiftItems: add an item list for a shift/goto symbol to the table *)

fun mergeShiftItems (args as ((k,l),r)) =
                  case Assoc.find(k,r)
                  of NONE => Assoc.insert args
                   | SOME old => Assoc.insert ((k,l@old),r)

(* mergeItems: add all items derived from a nonterminal to the table.  We've
   kept these items sorted by their shift/goto symbol (the first symbol on
   their rhs) *)

                fun mergeItems (n,r) =
                        Assoc.fold mergeShiftItems (ntShifts n) r

(* nonterms: a list of nonterminals that are in a core after the
   closure operation *)

                val nonterms = closureNonterms itemList

(* now create a table which for each shift/goto symbol gives the sorted list
   of closure items which would result from first taking all the closure items
   and then sorting them by the shift/goto symbols *)

                val newsets = List.foldr mergeItems Assoc.empty nonterms

(* finally prepare to insert the kernal items of a core *)

                fun insertItem ((k,i),r) =
                   case (Assoc.find(k,r))
                     of NONE => Assoc.insert((k,[i]),r)
                      | SOME l => Assoc.insert((k,Core.insert(i,l)),r)
                fun shiftCores(ITEM{rule,dot,rhsAfter=symbol::rest},r) =
                   insertItem((symbol,
                              ITEM{rule=rule,dot=dot+1,rhsAfter=rest}),r)
                  | shiftCores(_,r) = r

(* insert the kernal items of a core *)

                val newsets = List.foldr shiftCores newsets itemList
           in Assoc.make_list newsets
           end

(* nontermEpsProds: returns a list of epsilon productions produced by a
   nonterminal sorted by rule number. ** Depends on produces returning
   an ordered list **.  It does not alter the order in which the rules
   were returned by produces; it only removes non-epsilon productions *)

           val nontermEpsProds =
              let val f = fn nt =>
                  List.foldr
                    (fn (rule as RULE {rhs=nil,...},results) => rule :: results
                      | (_,results) => results)
                    [] (produces nt)
               in memoize f
               end 

(* epsProds: take a core and compute a list of epsilon productions for it
   sorted by rule number.  ** Depends on closureNonterms returning a list
   of nonterminals sorted by nonterminal #, rule numbers increasing
   monotonically with their lhs production #, and nontermEpsProds returning
   an ordered item list for each production 
*)

        fun epsProds (CORE (itemList,state)) =
           let val prods = map nontermEpsProds (closureNonterms itemList)
           in List.concat prods
           end

     in {produces=produces,shifts=shifts,rules=rules,epsProds=epsProds}
     end
end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi 
 *
 * $Log$
 * Revision 1.2  1996/02/26  15:02:34  george
 *    print no longer overloaded.
 *    use of makestring has been removed and replaced with Int.toString ..
 *    use of IO replaced with TextIO
 *
 * Revision 1.1.1.1  1996/01/31  16:01:45  george
 * Version 109
 * 
 *)

functor mkGraph(structure IntGrammar : INTGRAMMAR
                structure Core : CORE
                structure CoreUtils : CORE_UTILS
                sharing IntGrammar = Core.IntGrammar = CoreUtils.IntGrammar
                sharing CoreUtils.Core = Core
                ) : LRGRAPH =
        struct
                open Array List
                infix 9 sub
                structure Core = Core
                structure Grammar = IntGrammar.Grammar
                structure IntGrammar = IntGrammar
                open Core Core.Grammar CoreUtils IntGrammar

                structure NodeSet = RbOrdSet
                        (struct
                                type elem = core
                                val eq = eqCore
                                val gt = gtCore
                        end)

                open NodeSet
                exception Shift of int * symbol

                type graph = {edges: {edge:symbol,to:core} list array,
                              nodes: core list,nodeArray : core array}
                val edges = fn (CORE (_,i),{edges,...}:graph) => edges sub i
                val nodes = fn ({nodes,...} : graph) => nodes
                val shift = fn ({edges,nodes,...} : graph) => fn a as (i,sym) =>
                        let fun find nil = raise (Shift a)
                              | find ({edge,to=CORE (_,state)} :: r) =
                                        if gtSymbol(sym,edge) then find r
                                        else if eqSymbol(edge,sym) then state
                                        else raise (Shift a)
                        in find (edges sub i)
                        end

                val core = fn ({nodeArray,...} : graph) =>
                                 fn i => nodeArray sub i

                val mkGraph = fn (g as (GRAMMAR {start,...})) =>
                   let val {shifts,produces,rules,epsProds} =
                                  CoreUtils.mkFuncs g
                       fun add_goto ((symbol,a),(nodes,edges,future,num)) =
                                case find(CORE (a,0),nodes)
                                  of NONE =>
                                     let val core =CORE (a,num)
                                         val edge = {edge=symbol,to=core}
                                     in (insert(core,nodes),edge::edges,
                                         core::future,num+1)
                                     end
                                   | (SOME c) =>
                                        let val edge={edge=symbol,to=c}
                                        in (nodes,edge::edges,future,num)
                                        end
                       fun f (nodes,node_list,edge_list,nil,nil,num) =
                            let val nodes=rev node_list
                            in {nodes=nodes,
                                edges=Array.fromList (rev edge_list),
                                nodeArray = Array.fromList nodes
                                }
                            end
                         | f (nodes,node_list,edge_list,nil,y,num) =
                                f (nodes,node_list,edge_list,rev y,nil,num)
                         | f (nodes,node_list,edge_list,h::t,y,num) =
                                let val (nodes,edges,future,num) =
                                   List.foldr add_goto (nodes,[],y,num) (shifts h)
                                in f (nodes,h::node_list,
                                       edges::edge_list,t,future,num)
                                end
                in {graph=
                   let val makeItem = fn (r as (RULE {rhs,...})) =>
                                                ITEM{rule=r,dot=0,rhsAfter=rhs}
                        val initialItemList = map makeItem (produces start)
                        val orderedItemList =
                           List.foldr Core.insert [] initialItemList
                        val initial = CORE (orderedItemList,0)
                   in f(empty,nil,nil,[initial],nil,1)
                   end,
                   produces=produces,
                   rules=rules,
                   epsProds=epsProds}
                end
        val prGraph = fn a as (nontermToString,termToString,print) => fn g =>
           let val printCore = prCore a
               val printSymbol = print o nontermToString
               val nodes = nodes g
               val printEdges = fn n => 
                 List.app (fn {edge,to=CORE (_,state)} =>
                        (print "\tshift on ";
                         printSymbol edge;
                         print " to ";
                         print (Int.toString state);
                         print "\n")) (edges (n,g))
         in List.app (fn c => (printCore c; print "\n"; printEdges c)) nodes
         end
end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi 
 *
 * $Log$
 * Revision 1.1.1.1  1996/01/31  16:01:46  george
 * Version 109
 * 
 *)

functor mkLook (structure IntGrammar : INTGRAMMAR) : LOOK =
    struct
        open Array List
        infix 9 sub
        structure Grammar = IntGrammar.Grammar
        structure IntGrammar = IntGrammar
        open Grammar IntGrammar

        structure TermSet = ListOrdSet
                (struct
                        type elem = term
                        val eq = eqTerm
                        val gt = gtTerm
                end)

        val union = TermSet.union
        val make_set = TermSet.make_set

        val prLook = fn (termToString,print) =>
                let val printTerm = print o termToString
                    fun f nil = print " "
                      | f (a :: b) = (printTerm a; print " "; f b)
                in f
                end

        structure NontermSet = ListOrdSet
                (struct
                        type elem = nonterm
                        val eq = eqNonterm
                        val gt = gtNonterm
                end)
        
        val mkFuncs = fn {rules : rule list, nonterms : int,
                          produces : nonterm -> rule list} =>

        let

        (* nullable: create a function which tells if a nonterminal is nullable
           or not.

           Method: Keep an array of booleans.  The nth entry is true if
           NT i is nullable.  If is false if we don't know whether NT i
           is nullable.

           Keep a list of rules whose remaining rhs we must prove to be
           null.  First, scan the list of rules and remove those rules
           whose rhs contains a terminal.  These rules are not nullable.

           Now iterate through the rules that were left:
                   (1) if there is no remaining rhs we have proved that
                    the rule is nullable, mark the nonterminal for the
                    rule as nullable
                   (2) if the first element of the remaining rhs is
                       nullable, place the rule back on the list with
                       the rest of the rhs
                   (3) if we don't know whether the nonterminal is nullable,
                       place it back on the list
                   (4) repeat until the list does not change.

           We have found all the possible nullable rules. 
      *)

        val nullable =
          let fun ok_rhs nil = true
                | ok_rhs ((TERM _)::_) = false
                | ok_rhs ((NONTERM i)::r) = ok_rhs r
              fun add_rule (RULE {lhs,rhs,...},r) =
                 if ok_rhs rhs then (lhs,map (fn (NONTERM (NT i)) => i) rhs)::r
                 else r
              val items = List.foldr add_rule [] rules
              val nullable = array(nonterms,false)
              val f = fn ((NT i,nil),(l,_)) => (update(nullable,i,true);
                                               (l,true))
                       | (a as (lhs,(h::t)),(l,change)) =>
                                case (nullable sub h) 
                                  of false => (a::l,change)
                                   | true => ((lhs,t)::l,true)
              fun prove(l,true) = prove(List.foldr f (nil,false) l)
                | prove(_,false) = ()
        in (prove(items,true); fn (NT i) => nullable sub i)
        end

     (* scanRhs : look at a list of symbols, scanning past nullable
        nonterminals, applying addSymbol to the symbols scanned *)

    fun scanRhs addSymbol =
        let fun f (nil,result) = result
              | f ((sym as NONTERM nt) :: rest,result) =
                if nullable nt then f (rest,addSymbol(sym,result))
                else addSymbol(sym,result)
              | f ((sym as TERM _) :: _,result) = addSymbol(sym,result)
        in f 
        end

     (* accumulate: look at the start of the right-hand-sides of rules,
        looking past nullable nonterminals, applying addObj to the visible
        symbols. *)

      fun accumulate(rules, empty, addObj) =
       List.foldr (fn (RULE {rhs,...},r) =>(scanRhs addObj) (rhs,r)) empty rules

      val nontermMemo = fn f =>
        let val lookup = array(nonterms,nil)
            fun g i = if i=nonterms then ()
                      else (update(lookup,i,f (NT i)); g (i+1))
        in (g 0; fn (NT j) => lookup sub j)
        end

     (* first1: the FIRST set of a nonterminal in the grammar. Only looks
        at other terminals, but it is clever enough to move past nullable
        nonterminals at the start of a production. *)

      fun first1 nt = accumulate(produces nt, TermSet.empty,
                                 fn (TERM t, set) => TermSet.insert (t,set)
                                  | (_, set) => set)

      val first1 = nontermMemo(first1)

     (* starters1: given a nonterminal "nt", return the set of nonterminals
        which can start its productions. Looks past nullables, but doesn't
        recurse *)

      fun starters1 nt = accumulate(produces nt, nil,
                                    fn (NONTERM nt, set) =>
                                         NontermSet.insert(nt,set)
                                     | (_, set) => set)

     val starters1 = nontermMemo(starters1)

     (* first: maps a nonterminal to its first-set. Get all the starters of
        the nonterminal, get the first1 terminal set of each of these,
        union the whole lot together *)

      fun first nt =
             List.foldr (fn (a,r) => TermSet.union(r,first1 a))
               [] (NontermSet.closure (NontermSet.singleton nt, starters1))

      val first = nontermMemo(first)

     (* prefix: all possible terminals starting a symbol list *)

      fun prefix symbols =
          scanRhs (fn (TERM t,r) => TermSet.insert(t,r)
                    | (NONTERM nt,r) => TermSet.union(first nt,r))
          (symbols,nil)

      fun nullable_string ((TERM t) :: r) = false
        | nullable_string ((NONTERM nt) :: r) =
                (case (nullable nt)
                   of true => nullable_string r
                    | f => f)
        | nullable_string nil = true
          
    in {nullable = nullable, first = prefix}
    end
end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi 
 *
 * $Log$
 * Revision 1.3  1996/10/03  03:37:12  jhr
 * Qualified identifiers that are no-longer top-level (quot, rem, min, max).
 *
 * Revision 1.2  1996/02/26  15:02:35  george
 *    print no longer overloaded.
 *    use of makestring has been removed and replaced with Int.toString ..
 *    use of IO replaced with TextIO
 *
 * Revision 1.1.1.1  1996/01/31  16:01:45  george
 * Version 109
 * 
 *)

functor mkLalr ( structure IntGrammar : INTGRAMMAR
                structure Core : CORE
                structure Graph : LRGRAPH
                structure Look: LOOK
                sharing Graph.Core = Core
                sharing Graph.IntGrammar = Core.IntGrammar =
                        Look.IntGrammar = IntGrammar) : LALR_GRAPH =
    struct
        open Array List
        infix 9 sub
        open IntGrammar.Grammar IntGrammar Core Graph Look
        structure Graph = Graph
        structure Core = Core
        structure Grammar = IntGrammar.Grammar
        structure IntGrammar = IntGrammar

        datatype tmpcore = TMPCORE of (item * term list ref) list * int
        datatype lcore = LCORE of (item * term list) list * int
        

         val prLcore =
          fn a as (SymbolToString,nontermToString,termToString,print) =>
            let val printItem = prItem (SymbolToString,nontermToString,print)
                val printLookahead = prLook(termToString,print)
            in fn (LCORE (items,state)) =>
                (print "\n";
                 print "state ";
                 print (Int.toString state);
                 print " :\n\n";
                 List.app (fn (item,lookahead) =>
                        (print "{";
                         printItem item;
                         print ",";
                         printLookahead lookahead;
                         print "}\n")) items)
            end

        exception Lalr of int

        structure ItemList = ListOrdSet
                (struct
                   type elem = item * term list ref
                   val eq = fn ((a,_),(b,_)) => eqItem(a,b)
                   val gt = fn ((a,_),(b,_)) => gtItem(a,b)
                 end)

        structure NontermSet = ListOrdSet
                (struct
                   type elem = nonterm
                   val gt = gtNonterm
                   val eq = eqNonterm
                 end)

(* NTL: nonterms with lookahead *)

        structure NTL = RbOrdSet
                (struct
                   type elem = nonterm * term list
                   val gt = fn ((i,_),(j,_)) => gtNonterm(i,j)
                   val eq = fn ((i,_),(j,_)) => eqNonterm(i,j)
                 end)

        val DEBUG = false

        val addLookahead  = fn {graph,nullable,first,eop,
                                rules,produces,nonterms,epsProds,
                                print,termToString,nontermToString} =>
          let

                val eop = Look.make_set eop

                val symbolToString = fn (TERM t) => termToString t
                                      | (NONTERM t) => nontermToString t

                val print = if DEBUG then print
                            else fn _ => ()

                val prLook = if DEBUG then prLook (termToString,print)
                             else fn _ => ()

                val prNonterm = print o nontermToString

                val prRule = if DEBUG
                              then prRule(symbolToString,nontermToString,print)
                              else fn _ => ()

                val printInt = print o (Int.toString : int -> string)

                val printItem = prItem(symbolToString,nontermToString,print)

(* look_pos: position in the rhs of a rule at which we should start placing
   lookahead ref cells, i.e. the minimum place at which A -> x .B y, where
   B is a nonterminal and y =*=> epsilon, or A -> x. is true.  Positions are
   given by the number of symbols before the place.  The place before the first
   symbol is 0, etc. *)

             val look_pos =
                 let val positions = array(length rules,0)

(* rule_pos: calculate place in the rhs of a rule at which we should start
   placing lookahead ref cells *)

                      val rule_pos = fn (RULE {rhs,...}) =>
                        case (rev rhs)
                          of nil => 0
                           | (TERM t) :: r => length rhs
                           | (l as (NONTERM n) :: r) =>

                              (* f assumes that everything after n in the
                                 rule has proven to be nullable so far.
                                 Remember that the rhs has been reversed,
                                 implying that this is true initially *)
                                
                                        (* A -> .z t B y, where y is nullable *)

                              let fun f (NONTERM b :: (r as (TERM _ :: _))) =
                                        (length r)

                                        (* A -> .z B C y *)

                                    | f (NONTERM c :: (r as (NONTERM b :: _))) =
                                         if nullable c then f r
                                         else (length r)

                                        (* A -> .B y, where y is nullable *)

                                    | f (NONTERM b :: nil) = 0 
                              in  f l
                              end
                             
                        val check_rule = fn (rule as RULE {num,...}) =>
                            let val pos = rule_pos rule
                            in (print "look_pos: ";
                                prRule rule;
                                print " = ";
                                printInt pos;
                                print "\n";
                                update(positions,num,rule_pos rule))
                            end
                   in app check_rule rules;
                    fn RULE{num,...} => (positions sub num)
                   end

(* rest_is_null: true for items of the form A -> x .B y, where y is nullable *)

             val rest_is_null =
                 fn (ITEM{rule,dot, rhsAfter=NONTERM _ :: _}) =>
                         dot >= (look_pos rule)
                  | _ => false

(* map core to a new core including only items of the form A -> x. or
   A -> x. B y, where y =*=> epsilon.  It also adds epsilon productions to the
   core. Each item is given a ref cell to hold the lookahead nonterminals for
   it.*)

              val map_core =
                let val f = fn (item as ITEM {rhsAfter=nil,...},r) =>
                                (item,ref nil) :: r
                             | (item,r) =>
                                 if (rest_is_null item)
                                    then (item,ref nil)::r
                                    else r
                in fn (c as CORE (items,state)) =>
                   let val epsItems =
                           map  (fn rule=>(ITEM{rule=rule,dot=0,rhsAfter=nil},
                                           ref (nil : term list))
                                ) (epsProds c)
                   in TMPCORE(ItemList.union(List.foldr f [] items,epsItems),state)
                   end
                end

              val new_nodes = map map_core (nodes graph)

              exception Find

(* findRef: state * item -> lookahead ref cell for item *)

              val findRef = 
                let val states = Array.fromList new_nodes
                    val dummy = ref nil
                in fn (state,item) =>
                    let val TMPCORE (l,_) = states sub state
                    in case ItemList.find((item,dummy),l)
                                   of SOME (_,look_ref) => look_ref
                                    | NONE => (print "find failed: state ";
                                               printInt state;
                                               print "\nitem =\n";
                                               printItem item;
                                               print "\nactual items =\n";
                                               app (fn (i,_) => (printItem i;
                                                    print "\n")) l;
                                                raise Find)
                    end
                end 
                        

(* findRuleRefs: state -> rule -> lookahead refs for rule. *)
                
               val findRuleRefs =
                 let val shift = shift graph
                 in fn state =>
                        (* handle epsilon productions *)
                  fn (rule as RULE {rhs=nil,...}) => 
                         [findRef(state,ITEM{rule=rule,dot=0,rhsAfter=nil})]
                   | (rule as RULE {rhs=sym::rest,...}) =>
                   let  val pos = Int.max(look_pos rule,1)
                        fun scan'(state,nil,pos,result) =
                                findRef(state,ITEM{rule=rule,
                                                   dot=pos,
                                                   rhsAfter=nil}) :: result
                          | scan'(state,rhs as sym::rest,pos,result) =
                                scan'(shift(state,sym), rest, pos+1,
                                      findRef(state,ITEM{rule=rule,
                                                         dot=pos,
                                                         rhsAfter=rhs})::result)
                                
(* find first item of the form A -> x .B y, where y =*=> epsilon and
   x is not epsilon, or A -> x.  use scan' to pick up all refs after this
   point *)

                         fun scan(state,nil,_) =
                           [findRef(state,ITEM{rule=rule,dot=pos,rhsAfter=nil})]
                           | scan(state,rhs,0) = scan'(state,rhs,pos,nil)
                           | scan(state,sym::rest,place) =
                                    scan(shift(state,sym),rest,place-1)

                  in scan(shift(state,sym),rest,pos-1)
                  end

             end

(* function to compute for some nonterminal n the set of nonterminals A added
   through the closure of nonterminal n such that n =c*=> .A x, where x is
   nullable *)

              val nonterms_w_null = fn nt =>
                  let val collect_nonterms = fn n =>
                    List.foldr (fn (rule as RULE {rhs=rhs as NONTERM n :: _,...},r) =>
                           (case
                             (rest_is_null(ITEM {dot=0,rhsAfter=rhs,rule=rule}))
                                 of true => n :: r
                                  | false => r)
                           | (_,r) => r) [] (produces n)
                       fun dfs(a as (n,r)) =
                         if (NontermSet.exists a) then r 
                         else List.foldr dfs (NontermSet.insert(n,r))
                                (collect_nonterms n)
                  in dfs(nt,NontermSet.empty)
                  end

                val nonterms_w_null =
                   let val data = array(nonterms,NontermSet.empty)
                       fun f n = if n=nonterms then ()
                                 else (update(data,n,nonterms_w_null (NT n));
                                       f (n+1))
                   in (f 0; fn (NT nt) => data sub nt)
                   end

(* look_info: for some nonterminal n the set of nonterms A added
   through the closure of the nonterminal such that n =c+=> .Ax and the
   lookahead accumlated for each nonterm A *)

                val look_info = fn nt =>
                   let val collect_nonterms = fn n =>
                      List.foldr (fn (RULE {rhs=NONTERM n :: t,...},r) =>
                             (case NTL.find ((n,nil),r)
                              of SOME (key,data) =>
                                 NTL.insert((n,Look.union(data,first t)),r)
                               | NONE => NTL.insert ((n,first t),r))
                             | (_,r) => r)
                            NTL.empty (produces n)
                        fun dfs(a as ((key1,data1),r)) =
                          case (NTL.find a)
                           of SOME (_,data2) =>
                               NTL.insert((key1,Look.union(data1,data2)),r)
                            | NONE => NTL.fold dfs (collect_nonterms key1)
                                                   (NTL.insert a)
                    in dfs((nt,nil),NTL.empty)
                    end

                val look_info = 
                  if not DEBUG then look_info
                  else fn nt =>
                       (print "look_info of "; prNonterm nt; print "=\n";
                        let val info = look_info nt
                        in (NTL.app (fn (nt,lookahead) =>
                                    (prNonterm nt; print ": "; prLook lookahead;
                                     print "\n\n")) info;
                           info)
                        end)

(* prop_look: propagate lookaheads for nonterms added in the closure of a
   nonterm.  Lookaheads must be propagated from each nonterminal m to
   all nonterminals { n | m =c+=> nx, where x=*=>epsilon} *)

                  val prop_look = fn ntl =>
                    let val upd_lookhd = fn new_look => fn (nt,r) =>
                          case NTL.find ((nt,new_look),r)
                          of SOME (_,old_look) =>
                             NTL.insert((nt, Look.union(new_look,old_look)),r)
                           | NONE => raise (Lalr 241)
                         val upd_nonterm = fn ((nt,look),r) =>
                           NontermSet.fold (upd_lookhd look)
                                           (nonterms_w_null nt) r
                     in NTL.fold upd_nonterm ntl ntl
                     end

                val prop_look = 
                  if not DEBUG then prop_look
                  else fn ntl =>
                    (print "prop_look =\n";
                     let val info = prop_look ntl
                     in (NTL.app (fn (nt,lookahead) =>
                                    (prNonterm nt;
                                     print ": ";
                                     prLook lookahead;
                                     print "\n\n")) info; info)
                     end)

(* now put the information from these functions together.  Create a function
   which takes a nonterminal n and returns a list of triplets of
         (a nonterm added through closure,
          the lookahead for the nonterm,
          whether the nonterm should include the lookahead for the nonterminal
          whose closure is being taken (i.e. first(y) for an item j of the
          form A -> x .n y and lookahead(j) if y =*=> epsilon)
*)

                 val closure_nonterms =
                   let val data =
                          array(nonterms,nil: (nonterm * term list * bool) list)
                       val do_nonterm = fn i =>
                        let val nonterms_followed_by_null =
                                nonterms_w_null i
                            val nonterms_added_through_closure = 
                              NTL.make_list (prop_look (look_info i))
                            val result =
                            map (fn (nt,l) =>
                         (nt,l,NontermSet.exists (nt,nonterms_followed_by_null))
                                ) nonterms_added_through_closure
                         in if DEBUG then
                               (print "closure_nonterms = ";
                                prNonterm i;
                                print "\n";
                                app (fn (nt,look,nullable) =>
                                  (prNonterm nt;
                                   print ":";
                                   prLook look;
                                   case nullable
                                     of false => print "(false)\n"
                                      | true => print "(true)\n")) result;
                                print "\n")
                             else ();
                             result
                         end
                        fun f i =
                          if i=nonterms then ()
                          else (update(data,i,do_nonterm (NT i)); f (i+1))
                        val _ = f 0
                    in fn (NT i) => data sub i
                    end

(* add_nonterm_lookahead: Add lookahead to all completion items for rules added
   when the closure of a given nonterm in some state is taken.  It returns
   a list of lookahead refs to which the given nonterm's lookahead should
   be propagated.   For each rule, it must trace the shift/gotos in the LR(0)
   graph to find all items of the form A-> x .B y where y =*=> epsilon or
   A -> x.
*)

                val add_nonterm_lookahead = fn (nt,state) =>
                  let val f = fn ((nt,lookahead,nullable),r) =>
                        let val refs = map (findRuleRefs state) (produces nt)
                            val refs = List.concat refs
                            val _ = app (fn r =>
                                     r := (Look.union (!r,lookahead))) refs
                        in if nullable then refs @ r else r
                        end
                 in List.foldr f [] (closure_nonterms nt)
                 end

(* scan_core: Scan a core for all items of the form A -> x .B y.  Applies
   add_nonterm_lookahead to each such B, and then merges first(y) into
   the list of refs returned by add_nonterm_lookahead.  It returns
   a list of ref * ref list for all the items where y =*=> epsilon *)

                val scan_core = fn (CORE (l,state)) =>
                  let fun f ((item as ITEM{rhsAfter= NONTERM b :: y,
                                           dot,rule})::t,r) =
                        (case (add_nonterm_lookahead(b,state))
                          of nil => r
                           | l =>
                            let val first_y = first y
                                val newr  = if dot >= (look_pos rule)
                                        then (findRef(state,item),l)::r
                                        else r
                            in (app (fn r =>
                                         r := Look.union(!r,first_y)) l;
                                f (t,newr))
                            end)
                        | f (_ :: t,r) = f (t,r)
                        | f (nil,r) = r
                  in f (l,nil)
                  end

(* add end-of-parse symbols to set of items consisting of all items
   immediately derived from the start symbol *)

                val add_eop = fn (c as CORE (l,state),eop) =>
                  let fun f (item as ITEM {rule,dot,...}) =
                    let val refs = findRuleRefs state rule
                    in

(* first take care of kernal items.  Add the end-of-parse symbols to
   the lookahead sets for these items.  Epsilon productions of the
   start symbol do not need to be handled specially because they will
   be in the kernal also *)

                       app (fn r => r := Look.union(!r,eop)) refs;

(* now take care of closure items.  These are all nonterminals C which
   have a derivation S =+=> .C x, where x is nullable *)

                       if dot >= (look_pos rule) then
                          case item
                          of ITEM{rhsAfter=NONTERM b :: _,...} =>
                             (case add_nonterm_lookahead(b,state)
                              of nil => ()
                               | l => app (fn r => r := Look.union(!r,eop)) l)
                           | _ => ()
                       else ()
                    end
                  in app f l
                  end

                val iterate = fn l =>
                   let fun f lookahead (nil,done) = done
                         | f lookahead (h::t,done) =
                            let val old = !h
                            in h := Look.union (old,lookahead);
                               if (length (!h)) <> (length old)
                                         then f lookahead (t,false)
                                         else f lookahead(t,done)
                            end
                       fun g ((from,to)::rest,done) =
                        let val new_done = f (!from) (to,done)
                        in g (rest,new_done)
                        end
                         | g (nil,done) = done
                       fun loop true = ()
                         | loop false = loop (g (l,true))
                   in loop false
                   end

                val lookahead = List.concat (map scan_core (nodes graph))

(* used to scan the item list of a TMPCORE and remove the items not
   being reduced *)

                val create_lcore_list =
                        fn ((item as ITEM {rhsAfter=nil,...},ref l),r) =>
                                (item,l) :: r
                         | (_,r) => r

        in  add_eop(Graph.core graph 0,eop);
            iterate lookahead;
            map (fn (TMPCORE (l,state)) =>
                       LCORE (List.foldr create_lcore_list [] l, state)) new_nodes
        end
end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi 
 *
 * $Log$
 * Revision 1.3  1996/05/31  14:05:01  dbm
 * Rewrote definition of convert_to_pairlist to conform to value restriction.
 *
 * Revision 1.2  1996/02/26  15:02:36  george
 *    print no longer overloaded.
 *    use of makestring has been removed and replaced with Int.toString ..
 *    use of IO replaced with TextIO
 *
 * Revision 1.1.1.1  1996/01/31  16:01:46  george
 * Version 109
 * 
 *)

functor mkMakeLrTable (structure IntGrammar : INTGRAMMAR
                     structure LrTable : LR_TABLE
                     sharing type LrTable.term = IntGrammar.Grammar.term
                     sharing type LrTable.nonterm = IntGrammar.Grammar.nonterm
                    ) : MAKE_LR_TABLE = 
   struct 
        open Array List
        infix 9 sub
        structure Core = mkCore(structure IntGrammar = IntGrammar)
        structure CoreUtils = mkCoreUtils(structure IntGrammar = IntGrammar
                                          structure Core = Core)
        structure Graph = mkGraph(structure IntGrammar = IntGrammar
                                  structure Core = Core
                                  structure CoreUtils = CoreUtils)
        structure Look = mkLook(structure IntGrammar = IntGrammar)
        structure Lalr = mkLalr(structure IntGrammar = IntGrammar
                                structure Core = Core
                                structure Graph = Graph
                                structure Look = Look)
        structure LrTable = LrTable
        structure IntGrammar = IntGrammar
        structure Grammar = IntGrammar.Grammar
        structure GotoList = ListOrdSet
                (struct
                   type elem = Grammar.nonterm * LrTable.state
                   val eq = fn ((Grammar.NT a,_),(Grammar.NT b,_)) => a=b
                   val gt = fn ((Grammar.NT a,_),(Grammar.NT b,_)) => a>b
                end)
        structure Errs : LR_ERRS =
            struct
                structure LrTable = LrTable
                datatype err = RR of LrTable.term * LrTable.state * int * int
                             | SR of LrTable.term * LrTable.state * int
                             | NOT_REDUCED of int
                             | NS of LrTable.term * int
                             | START of int

                val summary = fn l =>
                   let val numRR = ref 0
                       val numSR = ref 0
                       val numSTART = ref 0
                       val numNOT_REDUCED = ref 0
                       val numNS = ref 0
                       fun loop (h::t) = 
                       (case h
                         of RR _ => numRR := !numRR+1
                          | SR _ => numSR := !numSR+1
                          | START _ => numSTART := !numSTART+1
                          | NOT_REDUCED _ => numNOT_REDUCED := !numNOT_REDUCED+1
                          | NS _ => numNS := !numNS+1; loop t)
                      | loop nil = {rr = !numRR, sr = !numSR,
                                start = !numSTART,
                                not_reduced = !numNOT_REDUCED,
                                nonshift = !numNS}
                  in loop l
                  end

                  val printSummary = fn say => fn l =>
                   let val {rr,sr,start,
                            not_reduced,nonshift} = summary l
                       val say_plural = fn (i,s) =>
                          (say (Int.toString i); say " ";
                           case i
                             of 1 => (say s)
                              | _ => (say s; say "s"))
                       val say_error = fn (args as (i,s)) =>
                          case i
                          of 0 => ()
                           | i => (say_plural args; say "\n")
                   in say_error(rr,"reduce/reduce conflict");
                      say_error(sr,"shift/reduce conflict");
                      if nonshift<>0 then
                           (say "non-shiftable terminal used on the rhs of ";
                           say_plural(start,"rule"); say "\n")
                      else ();
                      if start<>0 then (say "start symbol used on the rhs of ";
                                        say_plural(start,"rule"); say "\n")
                      else ();
                      if not_reduced<>0 then (say_plural(not_reduced,"rule");
                                              say " not reduced\n")
                      else ()
                end
            end


        open IntGrammar Grammar Errs LrTable Core 

(* rules for resolving conflicts:

        shift/reduce:

                  If either the terminal or the rule has no
                  precedence, a shift/reduce conflict is reported.
                  A shift is chosen for the table.

                  If both have precedences, the action with the
                  higher precedence is chosen.

                  If the precedences are equal, neither the
                  shift nor the reduce is chosen.

      reduce/reduce:

                  A reduce/reduce conflict is reported.  The lowest
                  numbered rule is chosen for reduction.
*)


(* method for filling tables - first compute the reductions called for in a
   state, then add the shifts for the state to this information.

How to compute the reductions:

   A reduction initially is given as an item and a lookahead set calling
for reduction by that item.  The first reduction is mapped to a list of
terminal * rule pairs.  Each additional reduction is then merged into this
list and reduce/reduce conflicts are resolved according to the rule
given.

Missed Errors:

   This method misses some reduce/reduce conflicts that exist because
some reductions are removed from the list before conflicting reductions
can be compared against them.  All reduce/reduce conflicts, however,
can be generated given a list of the reduce/reduce conflicts generated
by this method.
        
   This can be done by taking the transitive closure of the relation given
by the list.  If reduce/reduce (a,b) and reduce/reduce (b,c)  are true,
then reduce/reduce (a,c) is true.   The relation is symmetric and transitive.
                  
Adding shifts:

    Finally scan the list merging in shifts and resolving conflicts
according to the rule given.

Missed Shift/Reduce Errors:

    Some errors may be missed by this method because some reductions were
removed as the result of reduce/reduce conflicts.  For a shift/reduce
conflict of term a, reduction by rule n, shift/reduce conficts exist
for all rules y such that reduce/reduce (x,y) or reduce/reduce (y,x)
is true.
*)

    val mergeReduces =
        let val merge = fn state =>
          let fun f (j as (pair1 as (T t1,action1)) :: r1,
                     k as (pair2 as (T t2,action2)) :: r2,result,errs) =
                    if t1 < t2 then f(r1,k,pair1::result,errs)
                    else if t1 > t2 then f(j,r2,pair2::result,errs)
                    else let val REDUCE num1 = action1
                             val REDUCE num2 = action2
                             val errs = RR(T t1,state,num1,num2) :: errs
                             val action = if num1 < num2 then pair1 else pair2
                         in f(r1,r2,action::result,errs)
                          end
                | f (nil,nil,result,errs) = (rev result,errs)
                | f (pair1::r,nil,result,errs) = f(r,nil,pair1::result,errs)
                | f (nil,pair2 :: r,result,errs) = f(nil,r,pair2::result,errs)
            in f
            end
         in fn state => fn ((ITEM {rule=RULE {rulenum,...},...}, lookahead),
                 (reduces,errs)) =>
                let val action = REDUCE rulenum
                    val actions = map (fn a=>(a,action)) lookahead
                in case reduces
                   of nil => (actions,errs)
                    | _ =>  merge state (reduces,actions,nil,errs)
                end
         end

  val computeActions = fn (rules,precedence,graph,defaultReductions) =>
     
    let val rulePrec =
          let val precData = array(length rules,NONE : int option)
          in app (fn RULE {rulenum=r,precedence=p,...} => update(precData,r,p))
             rules;
             fn i => precData sub i
          end

        fun mergeShifts(state,shifts,nil) = (shifts,nil)
          | mergeShifts(state,nil,reduces) = (reduces,nil)
          | mergeShifts(state,shifts,reduces) =
          let fun f(shifts as (pair1 as (T t1,_)) :: r1,
                        reduces as (pair2 as (T t2,action)) :: r2,
                        result,errs) =
                if t1 < t2 then f(r1,reduces,pair1 :: result,errs)
                else if t1 > t2 then f(shifts,r2,pair2 :: result,errs)
                else let val REDUCE rulenum = action
                         val (term1,_) = pair1
                     in case (precedence term1,rulePrec rulenum)
                      of (SOME i,SOME j) =>
                         if i>j then f(r1,r2,pair1 :: result,errs)
                         else if j>i then f(r1,r2,pair2 :: result,errs)
                         else f(r1,r2,(T t1, ERROR)::result,errs)
                       | (_,_) =>
                           f(r1,r2,pair1 :: result,
                             SR (term1,state,rulenum)::errs)
                     end
                | f (nil,nil,result,errs) = (rev result,errs)
                | f (nil,h::t,result,errs) =
                        f (nil,t,h::result,errs)
                | f (h::t,nil,result,errs) = 
                        f (t,nil,h::result,errs)
          in f(shifts,reduces,nil,nil)
          end

        fun mapCore ({edge=symbol,to=CORE (_,state)}::r,shifts,gotos) =
                (case symbol
                 of (TERM t) => mapCore (r,(t,SHIFT(STATE state))::shifts,gotos)
                  | (NONTERM nt) => mapCore(r,shifts,(nt,STATE state)::gotos)
                )
          | mapCore (nil,shifts,gotos) = (rev shifts,rev gotos)

        fun pruneError ((_,ERROR)::rest) = pruneError rest
          | pruneError (a::rest) = a :: pruneError rest
          | pruneError nil = nil

  in fn (Lalr.LCORE (reduceItems,state),c as CORE (shiftItems,state')) =>
        if DEBUG andalso (state <> state') then
                 let exception MkTable in raise MkTable end
        else
         let val (shifts,gotos) = mapCore (Graph.edges(c,graph),nil,nil)
             val tableState = STATE state
         in case reduceItems
                of nil => ((shifts,ERROR),gotos,nil)
                 | h :: nil =>
                    let val (ITEM {rule=RULE {rulenum,...},...}, l) = h
                        val (reduces,_) = mergeReduces tableState (h,(nil,nil))
                        val (actions,errs) = mergeShifts(tableState,
                                                         shifts,reduces)
                        val actions' = pruneError actions
                        val (actions,default) =
                           let fun hasReduce (nil,actions) = 
                                              (rev actions,REDUCE rulenum)
                                 | hasReduce ((a as (_,SHIFT _)) :: r,actions) =
                                                hasReduce(r,a::actions)
                                 | hasReduce (_ :: r,actions) =
                                                 hasReduce(r,actions)
                               fun loop (nil,actions) = (rev actions,ERROR)
                                 | loop ((a as (_,SHIFT _)) :: r,actions) =
                                                loop(r,a::actions)
                                 | loop ((a as (_,REDUCE _)) :: r,actions) =
                                                hasReduce(r,actions)
                                 | loop (_ :: r,actions) = loop(r,actions)
                          in if defaultReductions 
                               andalso length actions = length actions'
                             then loop(actions,nil)
                             else (actions',ERROR)
                          end
                     in ((actions,default), gotos,errs)
                    end
                 | l =>
                  let val (reduces,errs1) =
                         List.foldr (mergeReduces tableState) (nil,nil) l
                      val (actions,errs2) =
                         mergeShifts(tableState,shifts,reduces)
                  in ((pruneError actions,ERROR),gotos,errs1@errs2)
                  end
         end
   end                  

        val mkTable = fn (grammar as GRAMMAR{rules,terms,nonterms,start,
                                  precedence,termToString,noshift,
                                  nontermToString,eop},defaultReductions) =>
             let val symbolToString = fn (TERM t) => termToString t
                                       | (NONTERM nt) => nontermToString nt
                 val {rules,graph,produces,epsProds,...} = Graph.mkGraph grammar
                 val {nullable,first} =
                   Look.mkFuncs{rules=rules,produces=produces,nonterms=nonterms}
                 val lcores = Lalr.addLookahead
                                            {graph=graph,
                                             nullable=nullable,
                                             produces=produces,
                                             eop=eop,
                                             nonterms=nonterms,
                                             first=first,
                                             rules=rules,
                                             epsProds=epsProds,
                                             print=(fn s=>TextIO.output(TextIO.stdOut,s)),
                                             termToString = termToString,
                                             nontermToString = nontermToString}

                  fun zip (h::t,h'::t') = (h,h') :: zip(t,t')
                    | zip (nil,nil) = nil
                    | zip _ = let exception MkTable in raise MkTable end
                  
                  fun unzip l =
                   let fun f ((a,b,c)::r,j,k,l) = f(r,a::j,b::k,c::l)
                         | f (nil,j,k,l) = (rev j,rev k,rev l)
                   in f(l,nil,nil,nil)
                   end
                
                   val (actions,gotos,errs) =
                        let val doState =
                            computeActions(rules,precedence,graph,
                                           defaultReductions)
                        in unzip (map doState (zip(lcores,Graph.nodes graph)))
                        end

                  (* add goto from state 0 to a new state.  The new state
                     has accept actions for all of the end-of-parse symbols *)

                   val (actions,gotos,errs) =
                     case gotos
                     of nil => (actions,gotos,errs)
                      | h :: t =>
                        let val newStateActions = 
                           (map (fn t => (t,ACCEPT)) (Look.make_set eop),ERROR)
                           val state0Goto = 
                               GotoList.insert((start,STATE (length actions)),h)
                        in (actions @ [newStateActions],
                            state0Goto :: (t @ [nil]),
                            errs @ [nil])
                        end 

                val startErrs =
                  List.foldr (fn (RULE {rhs,rulenum,...},r) =>
                        if (exists (fn NONTERM a => a=start
                                     | _ => false) rhs)
                          then START rulenum :: r
                          else r) [] rules

                val nonshiftErrs =
                  List.foldr (fn (RULE {rhs,rulenum,...},r) =>
                          (List.foldr (fn (nonshift,r) =>
                           if (exists (fn TERM a => a=nonshift
                                     | _ => false) rhs)
                            then NS(nonshift,rulenum) :: r
                            else r) r noshift)
                       ) [] rules

                val notReduced =
                  let val ruleReduced = array(length rules,false)
                      val test = fn REDUCE i => update(ruleReduced,i,true)
                                  | _ => ()
                      val _ = app (fn (actions,default) =>
                                     (app (fn (_,r) => test r) actions;
                                      test default)
                                  ) actions;
                      fun scan (i,r) =
                         if i >= 0 then
                              scan(i-1, if ruleReduced sub i then r
                                        else NOT_REDUCED i :: r)
                        else r
                  in scan(Array.length ruleReduced-1,nil)
                  end handle Subscript =>
                        (if DEBUG then
                                print "rules not numbered correctly!"
                         else (); nil)

                val numstates = length actions

                val allErrs = startErrs @ notReduced @ nonshiftErrs @ 
                              (List.concat errs)

                fun convert_to_pairlist(nil : ('a * 'b) list): ('a,'b) pairlist =
                      EMPTY
                  | convert_to_pairlist ((a,b) :: r) =
                      PAIR(a,b,convert_to_pairlist r)

              in (mkLrTable {actions=Array.fromList(map (fn (a,b) =>
                                     (convert_to_pairlist a,b)) actions),
                             gotos=Array.fromList (map convert_to_pairlist gotos),
                             numRules=length rules,numStates=length actions,
                             initialState=STATE 0},
                  let val errArray = Array.fromList errs
                  in fn (STATE state) => errArray sub state
                  end,

                  fn print =>
                    let val printCore =
                          prCore(symbolToString,nontermToString,print)
                        val core = Graph.core graph
                    in fn STATE state =>
                         printCore (if state=(numstates-1) then
                                         Core.CORE (nil,state)
                                        else (core state))
                    end,
                    allErrs)
              end
end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi 
 *
 * $Log$
 * Revision 1.2  1996/02/26  15:02:33  george
 *    print no longer overloaded.
 *    use of makestring has been removed and replaced with Int.toString ..
 *    use of IO replaced with TextIO
 *
 * Revision 1.1.1.1  1996/01/31  16:01:45  george
 * Version 109
 * 
 *)

structure Grammar : GRAMMAR =
        struct

                (* define types term and nonterm using those in LrTable
                   datatype term = T of int 
                   datatype nonterm = NT of int *)

                open LrTable
                datatype symbol = TERM of term | NONTERM of nonterm
                datatype grammar = GRAMMAR of
                                {rules: {lhs: nonterm,
                                         rhs: symbol list, 
                                         precedence: int option,
                                         rulenum: int} list,
                                noshift : term list,
                                eop : term list,
                                terms: int,
                                nonterms: int,
                                start : nonterm,
                                precedence : term -> int option,
                                termToString : term -> string,
                                nontermToString : nonterm -> string}
end;

structure IntGrammar : INTGRAMMAR =
        struct
                structure Grammar = Grammar
                open Grammar

                datatype rule = RULE of
                                {lhs: nonterm,
                                 rhs: symbol list,
                                 num: int,(* internal # assigned by coreutils *)
                                 rulenum: int,
                                 precedence: int option}
                
                val eqTerm = (op =)
                val gtTerm = fn (T i,T j) => i>j

                val eqNonterm = (op =)
                val gtNonterm = fn (NT i,NT j) => i>j

                val eqSymbol = (op =)
                val gtSymbol = fn (TERM (T i),TERM (T j)) => i>j
                                | (NONTERM (NT i),NONTERM (NT j)) => i>j
                                | (TERM _,NONTERM _) => false
                                | (NONTERM _,TERM _) => true


                structure SymbolAssoc = Table(type key = symbol
                                              val gt = gtSymbol)

                structure NontermAssoc = Table(type key =  nonterm
                                               val gt = gtNonterm)

                val DEBUG = false

                val prRule = fn (a as symbolToString,nontermToString,print) =>
                   let val printSymbol = print o symbolToString
                       fun printRhs (h::t) = (printSymbol h; print " ";
                                              printRhs t)
                         | printRhs nil = ()
                   in fn (RULE {lhs,rhs,num,rulenum,precedence,...}) =>
                        ((print o nontermToString) lhs; print " : ";
                         printRhs rhs;
                         if DEBUG then (print " num = ";
                                        print (Int.toString num);
                                        print " rulenum = ";
                                        print (Int.toString rulenum);
                                        print " precedence = ";
                                        case precedence
                                            of NONE => print " none"
                                             | (SOME i) =>
                                                 print (Int.toString i);
                                        ())
                        else ())
                   end
                        
                val prGrammar =
                         fn (a as (symbolToString,nontermToString,print)) =>
                             fn (GRAMMAR {rules,terms,nonterms,start,...}) =>
                 let val printRule =
                        let val prRule = prRule a
                        in  fn {lhs,rhs,precedence,rulenum} =>
                             (prRule (RULE {lhs=lhs,rhs=rhs,num=0,
                                      rulenum=rulenum, precedence=precedence});
                              print "\n")
                        end
                 in print "grammar = \n";
                    List.app printRule rules;
                    print "\n";
                    print (" terms = " ^ (Int.toString terms) ^
                             " nonterms = " ^ (Int.toString nonterms) ^
                             " start = ");
                    (print o nontermToString) start;
                    ()
                 end
        end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi 
 *
 * $Log$
 * Revision 1.2  1996/02/26  15:02:39  george
 *    print no longer overloaded.
 *    use of makestring has been removed and replaced with Int.toString ..
 *    use of IO replaced with TextIO
 *
 * Revision 1.1.1.1  1996/01/31  16:01:47  george
 * Version 109
 * 
 *)

functor mkVerbose(structure Errs : LR_ERRS) : VERBOSE =
struct
   structure Errs = Errs
   open Errs Errs.LrTable
   val mkPrintAction = fn print =>
        let val printInt = print o (Int.toString : int -> string)
        in fn (SHIFT (STATE i)) =>
                        (print "\tshift ";
                         printInt i;
                        print "\n")
             | (REDUCE rulenum) =>
                        (print "\treduce by rule ";
                         printInt rulenum;
                         print "\n")
             | ACCEPT => print "\taccept\n"
             | ERROR => print "\terror\n"
        end
   val mkPrintGoto = fn (printNonterm,print) =>
      let val printInt = print o (Int.toString : int -> string)
      in fn (nonterm,STATE i) =>
                (print "\t";
                 printNonterm nonterm;
                 print "\tgoto ";
                 printInt i;
                 print "\n")
      end

   val mkPrintTermAction = fn (printTerm,print) =>
        let val printAction = mkPrintAction print
        in fn (term,action) =>
                (print "\t";
                 printTerm term;
                 printAction action)
        end
   val mkPrintGoto = fn (printNonterm,print) =>
        fn (nonterm,STATE i) =>
            let val printInt = print o (Int.toString : int -> string)
            in (print "\t";
                printNonterm nonterm;
                print "\tgoto ";
                printInt i;
                print "\n")
            end
   val mkPrintError = fn (printTerm,printRule,print) =>
     let val printInt = print o (Int.toString : int -> string)
         val printState = fn STATE s => (print " state "; printInt s)
     in fn (RR (term,state,r1,r2)) =>
                (print "error: ";
                 printState state;
                 print ": reduce/reduce conflict between rule ";
                 printInt r1;
                 print " and rule ";
                 printInt r2;
                 print " on ";
                 printTerm term;
                 print "\n")
         | (SR (term,state,r1)) =>
                (print "error: ";
                 printState state;
                 print ": shift/reduce conflict ";
                 print "(shift ";
                 printTerm term;
                 print ", reduce by rule ";
                 printInt r1;
                 print ")\n")
         | NOT_REDUCED i =>
                (print "warning: rule <";
                 printRule i;
                 print "> will never be reduced\n")
         | START i => 
                (print "warning: start symbol appears on the rhs of ";
                 print "<";
                 printRule i;
                 print ">\n")
         | NS (term,i) =>
                (print "warning: non-shiftable terminal ";
                 printTerm term;
                 print  "appears on the rhs of ";
                 print "<";
                 printRule i;
                 print ">\n")
      end
   structure PairList : sig 
                          val app : ('a * 'b -> unit) -> ('a,'b) pairlist -> unit
                          val length : ('a,'b) pairlist -> int
                        end
       =
      struct
         val app = fn f =>
             let fun g EMPTY = ()
                   | g (PAIR(a,b,r)) = (f(a,b); g r)
             in g
             end
         val length = fn l =>
             let fun g(EMPTY,len) = len
                   | g(PAIR(_,_,r),len) = g(r,len+1)
             in g(l,0)
             end
      end
   val printVerbose =
        fn {termToString,nontermToString,table,stateErrs,entries:int,
            print,printRule,errs,printCores} =>
           let 
                val printTerm = print o termToString
                val printNonterm = print o nontermToString

                val printCore = printCores print
                val printTermAction = mkPrintTermAction(printTerm,print)
                val printAction = mkPrintAction print
                val printGoto = mkPrintGoto(printNonterm,print)
                val printError = mkPrintError(printTerm,printRule print,print)

                val gotos = LrTable.describeGoto table
                val actions = LrTable.describeActions table
                val states = numStates table

                val gotoTableSize = ref 0
                val actionTableSize = ref 0
                
                val _ = if length errs > 0 
                           then (printSummary print errs;
                                 print "\n";
                                 app printError errs)
                           else ()  
                fun loop i =
                  if i=states then ()
                  else let val s = STATE i
                       in (app printError (stateErrs s);
                           print "\n";
                           printCore s;
                           let val (actionList,default) = actions s
                               val gotoList = gotos s
                           in (PairList.app printTermAction actionList;
                               print "\n";
                               PairList.app printGoto gotoList;
                               print "\n";
                               print "\t.";
                               printAction default;
                               print "\n";
                               gotoTableSize:=(!gotoTableSize)+
                                              PairList.length gotoList;
                               actionTableSize := (!actionTableSize) +
                                               PairList.length actionList + 1
                               ) 
                           end;
                           loop (i+1))
                        end
          in loop 0;
              print (Int.toString entries ^ " of " ^
                     Int.toString (!actionTableSize)^
                     " action table entries left after compaction\n");
              print (Int.toString (!gotoTableSize)^ " goto table entries\n")
          end
end;


(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi 
 *
 * $Log$
 * Revision 1.2  1996/02/26  15:02:37  george
 *    print no longer overloaded.
 *    use of makestring has been removed and replaced with Int.toString ..
 *    use of IO replaced with TextIO
 *
 * Revision 1.1.1.1  1996/01/31  16:01:46  george
 * Version 109
 * 
 *)

functor mkPrintStruct(structure LrTable : LR_TABLE
                      structure ShrinkLrTable : SHRINK_LR_TABLE
                      sharing LrTable = ShrinkLrTable.LrTable):PRINT_STRUCT =
   struct
      open Array List
      infix 9 sub
      structure LrTable = LrTable
      open ShrinkLrTable LrTable
      
     
      (* lineLength = approximately the largest number of characters to allow
         on a line when printing out an encode string *)
          
      val lineLength = 72

      (* maxLength = length of a table entry.  All table entries are encoded
         using two 16-bit integers, one for the terminal number and the other
         for the entry.  Each integer is printed as two characters (low byte,
         high byte), using the ML ascii escape sequence.  We need 4
         characters for each escape sequence and 16 characters for each entry
      *)

      val maxLength =  16

      (* number of entries we can fit on a row *)

      val numEntries = lineLength div maxLength

      (* convert integer between 0 and 255 to the three character ascii
         decimal escape sequence for it *)

      val chr =
        let val lookup = Array.array(256,"\000")
            val intToString = fn i =>
                if i>=100 then "\\" ^ (Int.toString i)
                else if i>=10 then "\\0" ^ (Int.toString i)
                else  "\\00" ^ (Int.toString i)
            fun loop n = if n=256 then ()
                         else (Array.update(lookup,n,intToString n); loop (n+1))
        in loop 0; fn i => lookup sub i
        end

      val makeStruct = fn {table,name,print,verbose} =>
       let
         val states = numStates table
         val rules = numRules table
         fun printPairList (prEntry : 'a * 'b -> unit) l =
               let fun f (EMPTY,_) = ()
                     | f (PAIR(a,b,r),count) =
                            if count >= numEntries then
                               (print "\\\n\\"; prEntry(a,b); f(r,1))
                            else (prEntry(a,b); f(r,(count+1)))
               in f(l,0)
               end
         val printList : ('a -> unit) -> 'a list -> unit =
           fn prEntry => fn l =>
                let fun f (nil,_) = ()
                      | f (a :: r,count) =
                             if count >= numEntries then
                                 (print "\\\n\\"; prEntry a; f(r,1))
                                else (prEntry a; f(r,count+1))
                in f(l,0)
                end
         val prEnd = fn _ => print "\\000\\000\\\n\\"
         fun printPairRow prEntry =
               let val printEntries = printPairList prEntry
               in fn l => (printEntries l; prEnd())
               end
         fun printPairRowWithDefault (prEntry,prDefault) =
               let val f = printPairRow prEntry
               in fn (l,default) => (prDefault default; f l)
               end
         fun printTable (printRow,count) =
               (print "\"\\\n\\";
                let fun f i = if i=count then ()
                               else (printRow i; f (i+1))
                in f 0
                end;
                print"\"\n")
         val printChar = print o chr

          (* print an integer between 0 and 2^16-1 as a 2-byte character,
             with the low byte first *)

         val printInt = fn i => (printChar (i mod 256);
                                  printChar (i div 256))

         (* encode actions as integers:

                ACCEPT => 0
                ERROR => 1
                SHIFT i => 2 + i
                REDUCE rulenum => numstates+2+rulenum
         *)

         val printAction =
              fn (REDUCE rulenum) => printInt (rulenum+states+2)
                 | (SHIFT (STATE i)) => printInt (i+2)
                 | ACCEPT => printInt 0
                 | ERROR => printInt 1
        
         val printTermAction = fn (T t,action) =>
                (printInt (t+1); printAction action)

         val printGoto = fn (NT n,STATE s) => (printInt (n+1); printInt s)

         val ((rowCount,rowNumbers,actionRows),entries)= 
                   shrinkActionList(table,verbose)
         val getActionRow = let val a = Array.fromList actionRows
                            in fn i => a sub i
                            end
         val printGotoRow : int -> unit = 
               let val f = printPairRow printGoto
                   val g = describeGoto table
               in fn i => f (g (STATE i))
               end
        val printActionRow =
              let val f = printPairRowWithDefault(printTermAction,printAction)
              in fn i => f (getActionRow i)
              end
        in print "val ";
           print name;
           print "=";
           print "let val actionRows =\n";
           printTable(printActionRow,rowCount);
           print "val actionRowNumbers =\n\"";
           printList (fn i => printInt i) rowNumbers;
           print "\"\n";
           print "val gotoT =\n"; 
           printTable(printGotoRow,states);
           print "val numstates = ";
           print (Int.toString states);
           print "\nval numrules = ";
           print (Int.toString rules);
           print "\n\
\val s = ref \"\" and index = ref 0\n\
\val string_to_int = fn () => \n\
\let val i = !index\n\
\in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256\n\
\end\n\
\val string_to_list = fn s' =>\n\
\    let val len = String.size s'\n\
\        fun f () =\n\
\           if !index < len then string_to_int() :: f()\n\
\           else nil\n\
\   in index := 0; s := s'; f ()\n\
\   end\n\
\val string_to_pairlist = fn (conv_key,conv_entry) =>\n\
\     let fun f () =\n\
\         case string_to_int()\n\
\         of 0 => EMPTY\n\
\          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())\n\
\     in f\n\
\     end\n\
\val string_to_pairlist_default = fn (conv_key,conv_entry) =>\n\
\    let val conv_row = string_to_pairlist(conv_key,conv_entry)\n\
\    in fn () =>\n\
\       let val default = conv_entry(string_to_int())\n\
\           val row = conv_row()\n\
\       in (row,default)\n\
\       end\n\
\   end\n\
\val string_to_table = fn (convert_row,s') =>\n\
\    let val len = String.size s'\n\
\        fun f ()=\n\
\           if !index < len then convert_row() :: f()\n\
\           else nil\n\
\     in (s := s'; index := 0; f ())\n\
\     end\n\
\local\n\
\  val memo = Array.array(numstates+numrules,ERROR)\n\
\  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))\n\
\       fun f i =\n\
\            if i=numstates then g i\n\
\            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))\n\
\          in f 0 handle Subscript => ()\n\
\          end\n\
\in\n\
\val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))\n\
\end\n\
\val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))\n\
\val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)\n\
\val actionRowNumbers = string_to_list actionRowNumbers\n\
\val actionT = let val actionRowLookUp=\n\
\let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end\n\
\in Array.fromList(map actionRowLookUp actionRowNumbers)\n\
\end\n\
\in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,\n\
\numStates=numstates,initialState=STATE ";
print (Int.toString ((fn (STATE i) => i) (initialState table)));
print "}\nend\n";
      entries
      end
end;
(* ML-Yacc Parser Generator (c) 1991 Andrew W. Appel, David R. Tarditi 
 *
 * $Log$
 * Revision 1.2  1996/05/30  17:52:58  dbm
 * Lifted a let to a local in definition of createEquivalences to conform with
 * value restriction.
 *
 * Revision 1.1.1.1  1996/01/31  16:01:46  george
 * Version 109
 * 
 *)

signature SORT_ARG =
  sig
     type entry
     val gt : entry * entry -> bool
  end
signature SORT =
  sig
     type entry
     val sort : entry list -> entry list
  end
signature EQUIV_ARG =
   sig
     type entry
     val gt : entry * entry -> bool
     val eq : entry * entry -> bool
   end
signature EQUIV =
  sig
     type entry

     (* equivalences: take a list of entries and divides them into
        equivalence classes numbered 0 to n-1.

        It returns a triple consisting of:

          * the number of equivalence classes
          * a list which maps each original entry to an equivalence
            class.  The nth entry in this list gives the equivalence
            class for the nth entry in the original entry list.
          * a list which maps equivalence classes to some representative
            element.  The nth entry in this list is an element from the
            nth equivalence class
      *)

     val equivalences : entry list -> (int * int list * entry list)
  end

(* An O(n lg n) merge sort routine *)

functor MergeSortFun(A : SORT_ARG) : SORT =
  struct
      type entry = A.entry

      (* sort: an O(n lg n) merge sort routine.  We create a list of lists
         and then merge these lists in passes until only one list is left.*)

      fun sort nil = nil
        | sort l =
             let (* merge: merge two lists *)

                 fun merge (l as a::at,r as b::bt) =
                       if A.gt(a,b)
                       then b :: merge(l,bt)
                       else a :: merge(at,r)
                   | merge (l,nil) = l
                   | merge (nil,r) = r

                 (* scan: merge pairs of lists on a list of lists.
                    Reduces the number of lists by about 1/2 *)

                 fun scan (a :: b :: rest) = merge(a,b) :: scan rest
                   | scan l = l

                 (* loop: calls scan on a list of lists until only
                    one list is left.  It terminates only if the list of
                    lists is nonempty.  (The pattern match for sort
                    ensures this.) *)

                 fun loop (a :: nil) = a
                   | loop l = loop (scan l)

              in loop (map (fn a => [a]) l)
              end
   end

(* an O(n lg n) routine for placing items in equivalence classes *)

functor EquivFun(A : EQUIV_ARG) : EQUIV =
   struct
       open Array List
       infix 9 sub

      (* Our algorithm for finding equivalence class is simple.  The basic
         idea is to sort the entries and place duplicates entries in the same
          equivalence class.

         Let the original entry list be E.  We map E to a list of a pairs
         consisting of the entry and its position in E, where the positions
         are numbered 0 to n-1.  Call this list of pairs EP.

         We then sort EP on the original entries.  The second elements in the
         pairs now specify a permutation that will return us to EP.

         We then scan the sorted list to create a list R of representative
         entries, a list P of integers which permutes the sorted list back to
         the original list and a list SE of integers  which gives the
         equivalence class for the nth entry in the sorted list .

         We then return the length of R, R, and the list that results from
         permuting SE by P.
     *) 

       type entry = A.entry
             
       val gt = fn ((a,_),(b,_)) => A.gt(a,b)

       structure Sort = MergeSortFun(type entry = A.entry * int
                                     val gt = gt)
       val assignIndex =
          fn l =>
             let fun loop (index,nil) = nil
                   | loop (index,h :: t) = (h,index) :: loop(index+1,t)
             in loop (0,l)
             end 
    
       local fun loop ((e,_) :: t, prev, class, R , SE) =
               if A.eq(e,prev)
                 then loop(t,e,class,R, class :: SE)
                 else loop(t,e,class+1,e :: R, (class + 1) :: SE)
             | loop (nil,_,_,R,SE) = (rev R, rev SE)
       in val createEquivalences =
           fn nil => (nil,nil)
            | (e,_) :: t => loop(t, e, 0, [e],[0])
       end

       val inversePermute = fn permutation =>
              fn nil => nil
               | l as h :: _ =>
                   let val result = array(length l,h)
                       fun loop (elem :: r, dest :: s) =
                             (update(result,dest,elem); loop(r,s))
                         | loop _ = ()
                       fun listofarray i =
                          if i < Array.length result then 
                                (result sub i) :: listofarray (i+1)
                          else nil
                    in loop (l,permutation); listofarray 0
                    end

       fun makePermutation x = map (fn (_,b) => b) x

       val equivalences = fn l =>
           let val EP = assignIndex l
               val sorted = Sort.sort EP
               val P = makePermutation sorted
               val (R, SE) = createEquivalences sorted
            in (length R, inversePermute P SE, R)
            end
end

functor ShrinkLrTableFun(structure LrTable : LR_TABLE) : SHRINK_LR_TABLE =
    struct
        structure LrTable = LrTable
        open LrTable
        val gtAction = fn (a,b) =>
              case a
              of SHIFT (STATE s) => 
                   (case b of SHIFT (STATE s') => s>s' | _ => true)
               | REDUCE i => (case b of SHIFT _ => false | REDUCE i' => i>i'
                                      | _ => true)
               | ACCEPT => (case b of ERROR => true | _ => false)
               | ERROR =>  false
        structure ActionEntryList =
            struct
                type entry = (term,action) pairlist * action
                val rec eqlist =
                      fn (EMPTY,EMPTY) => true
                       | (PAIR (T t,d,r),PAIR(T t',d',r')) =>
                             t=t' andalso d=d' andalso eqlist(r,r')
                       | _ => false
                 val rec gtlist =
                            fn (PAIR _,EMPTY) => true
                             | (PAIR(T t,d,r),PAIR(T t',d',r')) =>
                                     t>t' orelse (t=t' andalso
                                       (gtAction(d,d') orelse
                                         (d=d' andalso gtlist(r,r'))))
                             | _ => false
                 val eq = fn ((l,a),(l',a')) => a=a' andalso eqlist(l,l')
                 val gt = fn ((l,a),(l',a')) => gtAction(a,a')
                                orelse (a=a' andalso gtlist(l,l'))
            end
(*        structure GotoEntryList =
            struct
               type entry = (nonterm,state) pairlist
               val rec eq = 
                    fn (EMPTY,EMPTY) => true
                     | (PAIR (t,d,r),PAIR(t',d',r')) =>
                            t=t' andalso d=d' andalso eq(r,r')
                     | _ => false
               val rec gt =
                    fn (PAIR _,EMPTY) => true
                     | (PAIR(NT t,STATE d,r),PAIR(NT t',STATE d',r')) =>
                           t>t' orelse (t=t' andalso
                           (d>d' orelse (d=d' andalso gt(r,r'))))
                     | _ => false
            end *)
        structure EquivActionList = EquivFun(ActionEntryList)
        val states = fn max =>
            let fun f i=if i<max then STATE i :: f(i+1) else nil
            in f 0
            end
        val length : ('a,'b) pairlist -> int =
           fn l =>
             let fun g(EMPTY,len) = len
                   | g(PAIR(_,_,r),len) = g(r,len+1)
             in g(l,0)
             end
        val size : (('a,'b) pairlist * 'c) list -> int =
           fn l =>
             let val c = ref 0
             in (app (fn (row,_) => c := !c + length row) l; !c)
             end
       val shrinkActionList = 
         fn (table,verbose) =>
           case EquivActionList.equivalences
                     (map (describeActions table) (states (numStates table)))
           of result as (_,_,l) => (result,if verbose then size l else 0)
end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi 
 *
 * $Log$
 * Revision 1.1.1.1  1996/01/31  16:01:44  george
 * Version 109
 * 
 *)

signature ABSYN =
    sig
       datatype exp = EVAR of string
                    | EAPP of exp * exp
                    | ETUPLE of exp list
                    | EINT of int
                    | FN of pat * exp
                    | LET of decl list * exp
                    | UNIT
                    | SEQ of exp * exp
                    | CODE of string
       and      pat = PVAR of string
                    | PAPP of string * pat
                    | PTUPLE of pat list
                    | PLIST of pat list
                    | PINT of int
                    | WILD
                    | AS of pat * pat
       and     decl = VB of pat * exp
       and     rule = RULE of pat * exp
       val printRule : ((string -> unit) * (string -> unit)) -> rule -> unit
    end
(* ML-Yacc Parser Generator (c) 1989, 1990 Andrew W. Appel, David R. Tarditi 
 *
 * $Log$
 * Revision 1.3  1996/05/30  18:05:09  dbm
 * Made changes to generate code that conforms to the value restriction by
 * lifting lets to locals in the code generated to define errtermvalue and action.
 *
 * Revision 1.2  1996/02/26  15:02:40  george
 *    print no longer overloaded.
 *    use of makestring has been removed and replaced with Int.toString ..
 *    use of IO replaced with TextIO
 *
 * Revision 1.1.1.1  1996/01/31  16:01:48  george
 * Version 109
 * 
 *)

functor ParseGenFun(structure ParseGenParser : PARSE_GEN_PARSER where type Header.pos = int
                    structure MakeTable : MAKE_LR_TABLE
                    structure Verbose : VERBOSE
                    structure PrintStruct : PRINT_STRUCT

                    sharing MakeTable.LrTable = PrintStruct.LrTable
                    sharing MakeTable.Errs = Verbose.Errs

                    structure Absyn : ABSYN
                    ) : PARSE_GEN =
  struct
    open Array List
    infix 9 sub
    structure Grammar = MakeTable.Grammar
    structure Header = ParseGenParser.Header

    open Header Grammar

    (* approx. maximum length of a line *)

    val lineLength = 70

    (* record type describing names of structures in the program being
        generated *)

    datatype names = NAMES 
                        of {miscStruct : string,  (* Misc{n} struct name *)
                            tableStruct : string, (* LR table structure *)
                            tokenStruct : string, (* Tokens{n} struct name *)
                            actionsStruct : string, (* Actions structure *)
                            valueStruct: string, (* semantic value structure *)
                            ecStruct : string,  (* error correction structure *)
                            arg: string, (* user argument for parser *)
                            tokenSig : string,  (* TOKENS{n} signature *)
                            miscSig :string, (* Signature for Misc structure *)
                            dataStruct:string, (* name of structure in Misc *)
                                                (* which holds parser data *)
                            dataSig:string (* signature for this structure *)
                                        
                            }

    val DEBUG = true
    exception Semantic

    (* common functions and values used in printing out program *)

    datatype values = VALS
                      of {say : string -> unit,
                          saydot : string -> unit,
                          sayln : string -> unit,
                          pureActions: bool,
                          pos_type : string,
                          arg_type : string,
                          ntvoid : string,
                          termvoid : string,
                          start : Grammar.nonterm,
                          hasType : Grammar.symbol -> bool,

                          (* actual (user) name of terminal *)

                          termToString : Grammar.term -> string,
                          symbolToString : Grammar.symbol -> string,

                          (* type symbol comes from the HDR structure,
                             and is now abstract *)

                          term : (Header.symbol * ty option) list,
                          nonterm : (Header.symbol * ty option) list,
                          terms : Grammar.term list}
                          
    structure SymbolHash = Hash(type elem = string
                                val gt = (op >) : string*string -> bool)

    structure TermTable = Table(type key = Grammar.term
                                val gt = fn (T i,T j) => i > j)

    structure SymbolTable = Table(
        type key = Grammar.symbol
        val gt = fn (TERM(T i),TERM(T j)) => i>j
                  | (NONTERM(NT i),NONTERM(NT j)) => i>j
                  | (NONTERM _,TERM _) => true
                  | (TERM _,NONTERM _) => false)

    (* printTypes: function to print the following types in the LrValues
       structure and a structure containing the datatype svalue:

                type svalue -- it holds semantic values on the parse
                                   stack
                type pos -- the type of line numbers
                type result -- the type of the value that results
                                   from the parse

        The type svalue is set equal to the datatype svalue declared
        in the structure named by valueStruct.  The datatype svalue
        is declared inside the structure named by valueStruct to deal
        with the scope of constructors.
    *)

    val printTypes = fn (VALS {say,sayln,term,nonterm,symbolToString,pos_type,
                                 arg_type,
                                 termvoid,ntvoid,saydot,hasType,start,
                                 pureActions,...},
                           NAMES {valueStruct,...},symbolType) =>
     let val prConstr = fn (symbol,SOME s) => 
                           say (" | " ^ (symbolName symbol) ^ " of " ^
                                  (if pureActions then "" else "unit -> ") ^
                                " (" ^ tyName s ^ ")"
                                )
                         | _ => ()
     in sayln "local open Header in";
        sayln ("type pos = " ^ pos_type);
        sayln ("type arg = " ^ arg_type);
        sayln ("structure " ^ valueStruct ^ " = ");
        sayln "struct";
        say ("datatype svalue = " ^ termvoid ^ " | " ^ ntvoid ^ " of" ^
             (if pureActions then "" else " unit -> ") ^ " unit");
        app prConstr term;
        app prConstr nonterm;
        sayln "\nend";
        sayln ("type svalue = " ^ valueStruct ^ ".svalue");
        say "type result = ";
        case symbolType (NONTERM start)
        of NONE => sayln "unit"
         | SOME t => (say (tyName t); sayln "");
        sayln "end"
    end

     (* function to print Tokens{n} structure *)

    val printTokenStruct =
     fn (VALS {say, sayln, termToString, hasType,termvoid,terms,
               pureActions,...},
         NAMES {miscStruct,tableStruct,valueStruct,
                tokenStruct,tokenSig,dataStruct,...}) =>
                (sayln ("structure " ^ tokenStruct ^ " : " ^ tokenSig ^ " =");
                 sayln "struct";
                 sayln ("type svalue = " ^ dataStruct ^ ".svalue");
                 sayln "type ('a,'b) token = ('a,'b) Token.token";
                 let val f = fn term as T i =>
                        (say "fun "; say (termToString term);
                         say " (";
                         if (hasType (TERM term)) then say "i," else ();
                         say "p1,p2) = Token.TOKEN (";
                         say (dataStruct ^ "." ^ tableStruct ^ ".T ");
                         say (Int.toString i);
                         say ",(";
                         say (dataStruct ^ "." ^ valueStruct ^ ".");
                         if (hasType (TERM term)) then 
                            (say (termToString term);
                             if pureActions then say " i"
                             else say " (fn () => i)")
                         else say termvoid;
                         say ",";
                         sayln "p1,p2))")
                in app f terms
                end;
                sayln "end")
                          
    (* function to print signatures out - takes print function which
        does not need to insert line breaks *)

    val printSigs = fn (VALS {term,...},
                        NAMES {tokenSig,tokenStruct,miscSig,
                                dataStruct, dataSig, ...},
                        say) =>
          say  ("signature " ^ tokenSig ^ " =\nsig\n\
                 \type ('a,'b) token\ntype svalue\n" ^
                 (List.foldr (fn ((s,ty),r) => String.concat [
                    "val ", symbolName s,
                    (case ty
                     of NONE => ": " 
                      | SOME l => ": (" ^ (tyName l) ^ ") * "),
                    " 'a * 'a -> (svalue,'a) token\n", r]) "" term) ^
                 "end\nsignature " ^ miscSig ^
                  "=\nsig\nstructure Tokens : " ^ tokenSig ^
                  "\nstructure " ^ dataStruct ^ ":" ^ dataSig ^
                  "\nsharing type " ^ dataStruct ^
                  ".Token.token = Tokens.token\nsharing type " ^
                  dataStruct ^ ".svalue = Tokens.svalue\nend\n")
                
    (* function to print structure for error correction *)

    val printEC = fn (keyword : term list,
                      preferred_change : (term list * term list) list,
                      noshift : term list,
                      value : (term * string) list,
                      VALS {termToString, say,sayln,terms,saydot,hasType,
                            termvoid,pureActions,...},
                      NAMES {ecStruct,tableStruct,valueStruct,...}) =>
       let

         val sayterm = fn (T i) => (say "(T "; say (Int.toString i); say ")")

         val printBoolCase = fn ( l : term list) =>
            (say "fn ";
             app (fn t => (sayterm t; say " => true"; say " | ")) l;
             sayln "_ => false")

         val printTermList = fn (l : term list) =>
            (app (fn t => (sayterm t; say " :: ")) l; sayln "nil")

         fun printChange () =
            (sayln "val preferred_change = ";
             app (fn (d,i) =>
                    (say"("; printTermList d; say ","; printTermList i; 
                     sayln ")::"
                    )
                 ) preferred_change;
             sayln "nil")

         val printErrValues = fn (l : (term * string) list) =>
            (sayln "local open Header in";
             sayln "val errtermvalue=";
             say "fn ";
             app (fn (t,s) =>
                    (sayterm t; say " => ";
                     saydot valueStruct; say (termToString t);
                     say "(";
                     if pureActions then () else say "fn () => ";
                     say "("; say s; say "))";
                     sayln " | "
                    )
                 ) l;
            say "_ => ";
            say (valueStruct ^ ".");
            sayln termvoid; sayln "end")
              

          val printNames = fn () =>
                let val f = fn term =>
                         (sayterm term; say " => "; say "\"";
                          say (termToString term); sayln "\""; say "  | ")
                in (sayln "val showTerminal =";
                    say "fn ";
                    app f terms;
                    sayln "_ => \"bogus-term\"")
                end

           val ecTerms = 
                List.foldr (fn (t,r) =>
                  if hasType (TERM t) orelse exists (fn (a,_)=>a=t) value
                    then r
                    else t::r)
                [] terms
                                  
        in  say "structure ";
            say ecStruct;
            sayln "=";
            sayln "struct";
            say "open ";
            sayln tableStruct;
            sayln "val is_keyword =";
            printBoolCase keyword;
            printChange();
            sayln "val noShift = ";
            printBoolCase noshift;
            printNames ();
            printErrValues value;
            say "val terms = ";
            printTermList ecTerms;
            sayln "end"
        end

val printAction = fn (rules,
                          VALS {hasType,say,sayln,termvoid,ntvoid,
                                symbolToString,saydot,start,pureActions,...},
                          NAMES {actionsStruct,valueStruct,tableStruct,arg,...}) =>
let val printAbsynRule = Absyn.printRule(say,sayln)
    val is_nonterm = fn (NONTERM i) => true | _ => false
    val numberRhs = fn r =>
        List.foldl (fn (e,(r,table)) =>
                let val num = case SymbolTable.find(e,table)
                               of SOME i => i
                                | NONE => 1
                 in ((e,num,hasType e orelse is_nonterm e)::r,
                     SymbolTable.insert((e,num+1),table))
                 end) (nil,SymbolTable.empty) r

    val saySym = symbolToString

    val printCase = fn (i:int, r as {lhs=lhs as (NT lhsNum),prec,
                                        rhs,code,rulenum}) =>

       (* mkToken: Build an argument *)

       let open Absyn
           val mkToken = fn (sym,num : int,typed) =>
             let val symString = symbolToString sym
               val symNum = symString ^ (Int.toString num)
             in PTUPLE[WILD,
                     PTUPLE[if not (hasType sym) then
                              (if is_nonterm sym then
                                   PAPP(valueStruct^"."^ntvoid,
                                        PVAR symNum)
                              else WILD)
                           else 
                               PAPP(valueStruct^"."^symString,
                                 if num=1 andalso pureActions
                                     then AS(PVAR symNum,PVAR symString)
                                 else PVAR symNum),
                             if num=1 then AS(PVAR (symString^"left"),
                                              PVAR(symNum^"left"))
                             else PVAR(symNum^"left"),
                             if num=1 then AS(PVAR(symString^"right"),
                                              PVAR(symNum^"right"))
                             else PVAR(symNum^"right")]]
             end

            val numberedRhs = #1 (numberRhs rhs)

        (* construct case pattern *)

           val pat = PTUPLE[PINT i,PLIST(map mkToken numberedRhs @
                                           [PVAR "rest671"])]

        (* remove terminals in argument list w/o types *)

           val argsWithTypes =
                  List.foldr (fn ((_,_,false),r) => r
                         | (s as (_,_,true),r) => s::r) nil numberedRhs

        (* construct case body *)

           val defaultPos = EVAR "defaultPos"
           val resultexp = EVAR "result"
           val resultpat = PVAR "result"
           val code = CODE code
           val rest = EVAR "rest671"

           val body =
             LET([VB(resultpat,
                     EAPP(EVAR(valueStruct^"."^
                             (if hasType (NONTERM lhs)
                                  then saySym(NONTERM lhs)
                                  else ntvoid)),
                          if pureActions then code
                          else if argsWithTypes=nil then FN(WILD,code)
                          else
                           FN(WILD,
                             let val body =
                                LET(map (fn (sym,num:int,_) =>
                                  let val symString = symbolToString sym
                                      val symNum = symString ^ Int.toString num
                                  in VB(if num=1 then
                                             AS(PVAR symString,PVAR symNum)
                                        else PVAR symNum,
                                        EAPP(EVAR symNum,UNIT))
                                  end) (rev argsWithTypes),
                                      code)
                             in if hasType (NONTERM lhs) then
                                    body else SEQ(body,UNIT)
                             end)))],
                   ETUPLE[EAPP(EVAR(tableStruct^".NT"),EINT(lhsNum)),
                          case rhs
                          of nil => ETUPLE[resultexp,defaultPos,defaultPos]
                           | r =>let val (rsym,rnum,_) = hd(numberedRhs)
                                     val (lsym,lnum,_) = hd(rev numberedRhs)
                                 in ETUPLE[resultexp,
                                           EVAR (symbolToString lsym ^
                                                 Int.toString lnum ^ "left"),
                                           EVAR (symbolToString rsym ^
                                                  Int.toString rnum ^ "right")]
                                 end,
                           rest])
    in printAbsynRule (RULE(pat,body))
    end

          val prRules = fn () =>
             (sayln "fn (i392,defaultPos,stack,";
              say   "    ("; say arg; sayln "):arg) =>";
              sayln "case (i392,stack)";
              say "of ";
              app (fn (rule as {rulenum,...}) =>
                   (printCase(rulenum,rule); say "| ")) rules;
             sayln "_ => raise (mlyAction i392)")

        in say "structure ";
           say actionsStruct;
           sayln " =";
           sayln "struct ";
           sayln "exception mlyAction of int";
           sayln "local open Header in";
           sayln "val actions = ";
           prRules();
           sayln "end";
           say "val void = ";
           saydot valueStruct;
           sayln termvoid;
           say "val extract = ";
           say "fn a => (fn ";
           saydot valueStruct;
           if hasType (NONTERM start)
              then say (symbolToString (NONTERM start))
              else say "ntVOID";
           sayln " x => x";
           sayln "| _ => let exception ParseInternal";
           say "\tin raise ParseInternal end) a ";
           sayln (if pureActions then "" else "()");
           sayln "end"
        end

    val make_parser = fn ((header,
         DECL {eop,change,keyword,nonterm,prec,
               term, control,value} : declData,
               rules : rule list),spec,error : pos -> string -> unit,
               wasError : unit -> bool) =>
     let
        val verbose = List.exists (fn VERBOSE=>true | _ => false) control
        val defaultReductions = not (List.exists (fn NODEFAULT=>true | _ => false) control)
        val pos_type =
           let fun f nil = NONE
                 | f ((POS s)::r) = SOME s 
                 | f (_::r) = f r
           in f control
           end
        val start =
           let fun f nil = NONE
                 | f ((START_SYM s)::r) = SOME s 
                 | f (_::r) = f r
           in f control
           end
        val name =
           let fun f nil = NONE
                 | f ((PARSER_NAME s)::r) = SOME s 
                 | f (_::r) = f r
           in f control
           end
        val header_decl =
           let fun f nil = NONE
                 | f ((FUNCTOR s)::r) = SOME s 
                 | f (_::r) = f r
           in f control
           end
        val arg_decl =
           let fun f nil = ("()","unit")
                 | f ((PARSE_ARG s)::r) = s 
                 | f (_::r) = f r
           in f control
           end

        val noshift =
           let fun f nil = nil
                 | f ((NSHIFT s)::r) = s 
                 | f (_::r) = f r
           in f control
           end

        val pureActions =
           let fun f nil = false
                 | f ((PURE)::r) = true 
                 | f (_::r) = f r
           in f control
           end

        val term =
         case term
           of NONE => (error 1 "missing %term definition"; nil)
            | SOME l => l

        val nonterm =
         case nonterm
          of NONE => (error 1 "missing %nonterm definition"; nil)
           | SOME l => l

        val pos_type =
         case pos_type
          of NONE => (error 1 "missing %pos definition"; "")
           | SOME l => l


        val termHash = 
          List.foldr (fn ((symbol,_),table) =>
              let val name = symbolName symbol
              in if SymbolHash.exists(name,table) then
                   (error (symbolPos symbol)
                          ("duplicate definition of " ^ name ^ " in %term");
                    table)
                else SymbolHash.add(name,table)
              end) SymbolHash.empty term

        val isTerm = fn name => SymbolHash.exists(name,termHash)

        val symbolHash = 
          List.foldr (fn ((symbol,_),table) =>
            let val name = symbolName symbol
            in if SymbolHash.exists(name,table) then
                 (error (symbolPos symbol)
                     (if isTerm name then
                          name ^ " is defined as a terminal and a nonterminal"
                      else 
                          "duplicate definition of " ^ name ^ " in %nonterm");
                     table)
             else SymbolHash.add(name,table)
            end) termHash nonterm

        fun makeUniqueId s =
                if SymbolHash.exists(s,symbolHash) then makeUniqueId (s ^ "'")
                else s

        val _ = if wasError() then raise Semantic else ()

        val numTerms = SymbolHash.size termHash
        val numNonterms = SymbolHash.size symbolHash - numTerms

        val symError = fn sym => fn err => fn symbol =>
          error (symbolPos symbol)
                (symbolName symbol^" in "^err^" is not defined as a " ^ sym)

        val termNum : string -> Header.symbol -> term =
          let val termError = symError "terminal" 
          in fn stmt =>
             let val stmtError = termError stmt
             in fn symbol =>
                case SymbolHash.find(symbolName symbol,symbolHash)
                of NONE => (stmtError symbol; T ~1)
                 | SOME i => T (if i<numTerms then i
                                else (stmtError symbol; ~1))
             end
          end
                        
        val nontermNum : string -> Header.symbol -> nonterm =
          let val nontermError = symError "nonterminal" 
          in fn stmt =>
             let val stmtError = nontermError stmt
             in fn symbol =>
                case SymbolHash.find(symbolName symbol,symbolHash)
                of NONE => (stmtError symbol; NT ~1)
                 | SOME i => if i>=numTerms then NT (i-numTerms)
                             else (stmtError symbol;NT ~1)
             end
          end

        val symbolNum : string -> Header.symbol -> Grammar.symbol =
          let val symbolError = symError "symbol" 
          in fn stmt =>
             let val stmtError = symbolError stmt
             in fn symbol =>
                case SymbolHash.find(symbolName symbol,symbolHash)
                of NONE => (stmtError symbol; NONTERM (NT ~1))
                 | SOME i => if i>=numTerms then NONTERM(NT (i-numTerms))
                             else TERM(T i)
             end
          end

(* map all symbols in the following values to terminals and check that
   the symbols are defined as terminals:

        eop : symbol list
        keyword: symbol list
        prec: (lexvalue * (symbol list)) list
        change: (symbol list * symbol list) list
*)

        val eop = map (termNum "%eop") eop
        val keyword = map (termNum "%keyword") keyword
        val prec = map (fn (a,l) => 
                        (a,case a
                           of LEFT => map (termNum "%left") l
                            | RIGHT => map (termNum "%right") l
                            | NONASSOC => map (termNum "%nonassoc") l
                        )) prec
        val change =
         let val mapTerm = termNum "%prefer, %subst, or %change"
         in map (fn (a,b) => (map mapTerm a, map mapTerm b)) change
         end
        val noshift = map (termNum "%noshift") noshift
        val value =
          let val mapTerm = termNum "%value"
          in map (fn (a,b) => (mapTerm a,b)) value
          end
        val (rules,_) =
           let val symbolNum = symbolNum "rule"
               val nontermNum = nontermNum "rule"
               val termNum = termNum "%prec tag"
           in List.foldr
           (fn (RULE {lhs,rhs,code,prec},(l,n)) =>
             ( {lhs=nontermNum lhs,rhs=map symbolNum rhs,
                code=code,prec=case prec
                                of NONE => NONE
                                 | SOME t => SOME (termNum t),
                 rulenum=n}::l,n-1))
                 (nil,length rules-1) rules
        end

        val _ = if wasError() then raise Semantic else ()

        (* termToString: map terminals back to strings *)

        val termToString =
           let val data = array(numTerms,"")
               val unmap = fn (symbol,_) =>
                   let val name = symbolName symbol
                   in update(data,
                             case SymbolHash.find(name,symbolHash)
                             of SOME i => i,name)
                   end
               val _ = app unmap term
           in fn T i =>
                if DEBUG andalso (i<0 orelse i>=numTerms)
                  then "bogus-num" ^ (Int.toString i)
                  else data sub i
           end

        val nontermToString = 
           let val data = array(numNonterms,"")
               val unmap = fn (symbol,_) =>
                    let val name = symbolName symbol
                    in update(data,
                              case SymbolHash.find(name,symbolHash)
                              of SOME i => i-numTerms,name)
                    end
               val _ = app unmap nonterm
           in fn NT i =>
                if DEBUG andalso (i<0 orelse i>=numNonterms)
                  then "bogus-num" ^ (Int.toString i)
                  else data sub i
           end

(* create functions mapping terminals to precedence numbers and rules to
  precedence numbers.

  Precedence statements are listed in order of ascending (tighter binding)
  precedence in the specification.   We receive a list composed of pairs
  containing the kind of precedence (left,right, or assoc) and a list of
  terminals associated with that precedence.  The list has the same order as
  the corresponding declarations did in the specification.

  Internally, a tighter binding has a higher precedence number.  We give
  precedences using multiples of 3:

                p+2 = right associative (force shift of symbol)
                p+1 = precedence for rule
                p = left associative (force reduction of rule)

  Nonassociative terminals are given also given a precedence of p+1.  The
table generator detects when the associativity of a nonassociative terminal
is being used to resolve a shift/reduce conflict by checking if the
precedences of the rule and the terminal are equal.

  A rule is given the precedence of its rightmost terminal *)

        val termPrec =
            let val precData = array(numTerms, NONE : int option)
                val addPrec = fn termPrec => fn term as (T i) =>
                   case precData sub i
                   of SOME _ =>
                     error 1 ("multiple precedences specified for terminal " ^
                            (termToString term))
                    | NONE => update(precData,i,termPrec)
                val termPrec = fn ((LEFT,_) ,i) => i
                              | ((RIGHT,_),i) => i+2
                              | ((NONASSOC,l),i) => i+1
                val _ = List.foldl (fn (args as ((_,l),i)) =>
                                (app (addPrec (SOME (termPrec args))) l; i+3))
                          0 prec
           in fn (T i) =>
                if  DEBUG andalso (i < 0 orelse i >= numTerms) then
                        NONE
                else precData sub i
           end

        val elimAssoc =  fn i => (i - (i mod 3) + 1)
        val rulePrec = 
           let fun findRightTerm (nil,r) = r
                 | findRightTerm (TERM t :: tail,r) =
                                 findRightTerm(tail,SOME t)
                 | findRightTerm (_ :: tail,r) = findRightTerm(tail,r)
           in fn rhs =>
                 case findRightTerm(rhs,NONE)
                 of NONE => NONE
                  | SOME term => 
                       case termPrec term
                       of SOME i => SOME  (elimAssoc i)
                        | a => a
           end

        val grammarRules =
          let val conv = fn {lhs,rhs,code,prec,rulenum} =>
                {lhs=lhs,rhs =rhs,precedence=
                        case prec
                          of SOME t => (case termPrec t
                                        of SOME i => SOME(elimAssoc i)
                                         | a => a)
                           | _ => rulePrec rhs,
                 rulenum=rulenum}
          in map conv rules
          end

    (* get start symbol *)

        val start =
         case start
           of NONE => #lhs (hd grammarRules)
            | SOME name => 
                nontermNum "%start" name

        val symbolType = 
           let val data = array(numTerms+numNonterms,NONE : ty option)
               val unmap = fn (symbol,ty) =>
                      update(data,
                             case SymbolHash.find(symbolName symbol,symbolHash)
                             of SOME i => i,ty)
               val _ = (app unmap term; app unmap nonterm)
           in fn NONTERM(NT i) =>
                if DEBUG andalso (i<0 orelse i>=numNonterms)
                  then NONE
                  else data sub (i+numTerms)
               | TERM (T i) =>
                if DEBUG andalso (i<0 orelse i>=numTerms)
                  then NONE
                  else data sub i
           end

        val symbolToString = 
             fn NONTERM i => nontermToString i
              | TERM i => termToString i

        val grammar  = GRAMMAR {rules=grammarRules,
                                 terms=numTerms,nonterms=numNonterms,
                                 eop = eop, start=start,noshift=noshift,
                                 termToString = termToString,
                                 nontermToString = nontermToString,
                                 precedence = termPrec}

        val name' = case name 
                    of NONE => ""
                     | SOME s => symbolName s

        val names = NAMES {miscStruct=name' ^ "LrValsFun",
                           valueStruct="MlyValue",
                           tableStruct="LrTable",
                           tokenStruct="Tokens",
                           actionsStruct="Actions",
                           ecStruct="EC",
                           arg= #1 arg_decl,
                           tokenSig = name' ^ "_TOKENS",
                           miscSig = name' ^ "_LRVALS",
                           dataStruct = "ParserData",
                           dataSig = "PARSER_DATA"}
                       
        val (table,stateErrs,corePrint,errs) =
                 MakeTable.mkTable(grammar,defaultReductions)

        val entries = ref 0 (* save number of action table entries here *)
        
    in  let val result = TextIO.openOut (spec ^ ".sml")
            val sigs = TextIO.openOut (spec ^ ".sig")
            val pos = ref 0
            val pr = fn s => TextIO.output(result,s)
            val say = fn s => let val l = String.size s
                                   val newPos = (!pos) + l
                              in if newPos > lineLength 
                                    then (pr "\n"; pos := l)
                                    else (pos := newPos);
                                   pr s
                              end
            val saydot = fn s => (say (s ^ "."))
            val sayln = fn t => (pr t; pr "\n"; pos := 0)
            val termvoid = makeUniqueId "VOID"
            val ntvoid = makeUniqueId "ntVOID"
            val hasType = fn s => case symbolType s
                                  of NONE => false
                                   | _ => true
            val terms = let fun f n = if n=numTerms then nil
                                      else (T n) :: f(n+1)
                        in f 0
                        end
            val values = VALS {say=say,sayln=sayln,saydot=saydot,
                               termvoid=termvoid, ntvoid = ntvoid,
                               hasType=hasType, pos_type = pos_type,
                               arg_type = #2 arg_decl,
                               start=start,pureActions=pureActions,
                               termToString=termToString,
                               symbolToString=symbolToString,term=term,
                               nonterm=nonterm,terms=terms}

            val (NAMES {miscStruct,tableStruct,dataStruct,tokenSig,tokenStruct,dataSig,...}) = names
         in case header_decl
            of NONE => (say "functor "; say miscStruct; 
                        sayln "(structure Token : TOKEN)";
                        say " : sig structure ";
                        say dataStruct;
                        say " : "; sayln dataSig;
                        say "       structure ";
                        say tokenStruct; say " : "; sayln tokenSig;
                        sayln "   end")
             | SOME s => say s;
            sayln " = ";
            sayln "struct";
            sayln ("structure " ^ dataStruct ^ "=");
            sayln "struct";
            sayln "structure Header = ";
            sayln "struct";
            sayln header;
            sayln "end";
            sayln "structure LrTable = Token.LrTable";
            sayln "structure Token = Token";
            sayln "local open LrTable in ";
            entries := PrintStruct.makeStruct{table=table,print=pr,
                                              name = "table",
                                              verbose=verbose};
            sayln "end";
            printTypes(values,names,symbolType);
            printEC (keyword,change,noshift,value,values,names);
            printAction(rules,values,names);
            sayln "end";
            printTokenStruct(values,names);
            sayln "end";
            printSigs(values,names,fn s => TextIO.output(sigs,s));    
            TextIO.closeOut sigs;
            TextIO.closeOut result;
            MakeTable.Errs.printSummary
            (fn s => () (* commented out by sweeks so it runs silently
                         TextIO.output(TextIO.stdOut,s) *)) errs
        end;
        if verbose then
         let val f = TextIO.openOut (spec ^ ".desc")
             val say = fn s=> TextIO.output(f,s)
             val printRule =
                let val rules = Array.fromList grammarRules
                in fn say => 
                   let val prRule = fn {lhs,rhs,precedence,rulenum} =>
                     ((say o nontermToString) lhs; say " : ";
                      app (fn s => (say (symbolToString s); say " ")) rhs)
                   in fn i => prRule (rules sub i)
                   end
                end
         in Verbose.printVerbose
            {termToString=termToString,nontermToString=nontermToString,
             table=table, stateErrs=stateErrs,errs = errs,entries = !entries,
             print=say, printCores=corePrint,printRule=printRule};
            TextIO.closeOut f
         end
        else ()
    end

    val parseGen = fn spec =>
                let val (result,inputSource) = ParseGenParser.parse spec
                in make_parser(getResult result,spec,Header.error inputSource,
                                errorOccurred inputSource)
                end
end;
(* ML-Yacc Parser Generator (c) 1991 Andrew W. Appel, David R. Tarditi 
 *
 * $Log$
 * Revision 1.3  1996/02/26  15:02:30  george
 *    print no longer overloaded.
 *    use of makestring has been removed and replaced with Int.toString ..
 *    use of IO replaced with TextIO
 *
 * Revision 1.2  1996/02/15  01:51:38  jhr
 * Replaced character predicates (isalpha, isnum) with functions from Char.
 *
 * Revision 1.1.1.1  1996/01/31  16:01:44  george
 * Version 109
 * 
 *)

structure Absyn : ABSYN =
  struct
    datatype exp
      = CODE of string
      | EAPP of exp * exp
      | EINT of int
      | ETUPLE of exp list
      | EVAR of string
      | FN of pat * exp
      | LET of decl list * exp
      | SEQ of exp * exp
      | UNIT
    and pat
      = PVAR of string
      | PAPP of string * pat
      | PINT of int
      | PLIST of pat list
      | PTUPLE of pat list
      | WILD
      | AS of pat * pat
    and decl = VB of pat * exp
    and rule = RULE of pat * exp

    fun idchar #"'" = true
      | idchar #"_" = true
      | idchar c = Char.isAlpha c orelse Char.isDigit c

    fun code_to_ids s = let
          fun g(nil,r) = r
            | g(a as (h::t),r) = if Char.isAlpha h then f(t,[h],r) else g(t,r)
          and f(nil,accum,r)= implode(rev accum)::r
            | f(a as (h::t),accum,r) =
                if idchar h then f(t,h::accum,r) else g(a,implode (rev accum) :: r)
          in g(explode s,nil)
          end

         val simplifyRule : rule -> rule = fn (RULE(p,e)) =>
            let val used : (string -> bool) =
               let fun f(CODE s) = code_to_ids s
                     | f(EAPP(a,b)) = f a @ f b
                     | f(ETUPLE l) = List.concat (map f l)
                     | f(EVAR s) = [s]
                     | f(FN(_,e)) = f e
                     | f(LET(dl,e)) =
                          (List.concat (map (fn VB(_,e) => f e) dl)) @ f e
                     | f(SEQ(a,b)) = f a @ f b
                     | f _ = nil
                   val identifiers = f e
               in fn s => List.exists (fn a=>a=s) identifiers
               end
              val simplifyPat : pat -> pat =
                let fun f a =
                    case a
                    of (PVAR s) => if used s then a else WILD
                     | (PAPP(s,pat)) =>
                         (case f pat
                          of WILD => WILD
                           | pat' => PAPP(s,pat'))
                     | (PLIST l) =>
                          let val l' = map f l
                          in if List.exists(fn WILD=>false | _ => true) l'
                                then PLIST l'
                             else WILD
                          end
                     | (PTUPLE l) =>
                          let val l' = map f l
                          in if List.exists(fn WILD=>false | _ => true) l'
                             then PTUPLE l' 
                             else WILD
                          end
                     | (AS(a,b)) =>
                         let val a'=f a
                             val b'=f b
                         in case(a',b')
                            of (WILD,_) => b'
                             | (_,WILD) => a'
                             | _ => AS(a',b')
                         end
                     | _ => a
               in f
               end
           val simplifyExp : exp -> exp =
               let fun f(EAPP(a,b)) = EAPP(f a,f b)
                     | f(ETUPLE l) = ETUPLE(map f l)
                     | f(FN(p,e)) = FN(simplifyPat p,f e) 
                     | f(LET(dl,e)) = 
                          LET(map (fn VB(p,e) =>
                                  VB(simplifyPat p,f e)) dl,
                              f e)
                     | f(SEQ(a,b)) = SEQ(f a,f b)
                     | f a = a
               in f
               end
       in RULE(simplifyPat p,simplifyExp e)
       end

       fun printRule (say : string -> unit, sayln:string -> unit) = let
         val lp = ["("]
         val rp = [")"]
         val sp = [" "]
         val sm = [";"]
         val cm = [","]
         val cr = ["\n"]
         val unit = ["()"]
          fun printExp c =
           let fun f (CODE c) = ["(",c,")"]
                 | f (EAPP(EVAR a,UNIT)) = [a," ","()"]
                 | f (EAPP(EVAR a,EINT i)) =  [a," ",Int.toString i]
                 | f (EAPP(EVAR a,EVAR b)) = [a," ",b]
                 | f (EAPP(EVAR a,b)) = List.concat[[a],lp,f b,rp]
                 | f (EAPP(a,b)) = List.concat [lp,f a,rp,lp,f b,rp]
                 | f (EINT i) = [Int.toString i]
                 | f (ETUPLE (a::r)) = 
                      let fun scan nil = [rp]
                            | scan (h :: t) = cm :: f h :: scan t
                      in List.concat (lp :: f a :: scan r)
                      end
                 | f (ETUPLE _) = ["<bogus-tuple>"]
                 | f (EVAR s) = [s]
                 | f (FN (p,b)) = List.concat[["fn "],printPat p,[" => "],f b]
                 | f (LET (nil,body)) = f body
                 | f (LET (dl,body)) =
                      let fun scan nil = [[" in "],f body,[" end"],cr]
                            | scan (h :: t) = printDecl h :: scan t
                      in List.concat(["let "] :: scan dl)
                      end
                 | f (SEQ (a,b)) = List.concat [lp,f a,sm,f b,rp]
                 | f (UNIT) = unit
          in f c
          end
         and printDecl (VB (pat,exp)) =
                  List.concat[["val "],printPat pat,["="],printExp exp,cr]
         and printPat c =
           let fun f (AS(PVAR a,PVAR b)) = [a," as ",b]
                 | f (AS(a,b)) = List.concat [lp,f a,[") as ("],f b,rp]
                 | f (PAPP(a,WILD)) = [a," ","_"]
                 | f (PAPP(a,PINT i)) =  [a," ",Int.toString i]
                 | f (PAPP(a,PVAR b)) = [a," ",b]
                 | f (PAPP(a,b)) = List.concat [lp,[a],sp,f b,rp]
                 | f (PINT i) = [Int.toString i]
                 | f (PLIST nil) = ["<bogus-list>"]
                 | f (PLIST l) =
                      let fun scan (h :: nil) = [f h]
                            | scan (h :: t) = f h :: ["::"] :: scan t
                      in List.concat (scan l)
                      end
                 | f (PTUPLE (a::r)) = 
                      let fun scan nil = [rp]
                            | scan (h :: t) = cm :: f h :: scan t
                      in List.concat (lp :: f a :: scan r)
                      end
                 | f (PTUPLE nil) = ["<bogus-pattern-tuple>"]
                 | f (PVAR a) = [a]
                 | f WILD = ["_"]
           in f c
           end
           fun oursay "\n" = sayln ""
             | oursay a = say a
         in fn a => 
              let val RULE(p,e) = simplifyRule a
              in app oursay (printPat p);
                 say " => ";
                 app oursay (printExp e)
              end
         end
end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi 
 *
 * $Log$
 * Revision 1.1.1.1  1996/01/31  16:01:45  george
 * Version 109
 * 
 *)
local

(* create parser *)

   structure LrVals = MlyaccLrValsFun(structure Token = LrParser.Token
                                      structure Hdr = Header)
   structure Lex = LexMLYACC(structure Tokens = LrVals.Tokens
                             structure Hdr = Header)
   structure Parser = JoinWithArg(structure Lex=Lex
                                 structure ParserData = LrVals.ParserData
                                 structure LrParser= LrParser)
   structure ParseGenParser =
           ParseGenParserFun(structure Parser = Parser
                             structure Header = Header)

(* create structure for computing LALR table from a grammar *)

   structure MakeLrTable = mkMakeLrTable(structure IntGrammar =IntGrammar
                                     structure LrTable = LrTable)

(* create structures for printing LALR tables:

   Verbose prints a verbose description of an lalr table
   PrintStruct prints an ML structure representing that is an lalr table *)

   structure Verbose = mkVerbose(structure Errs = MakeLrTable.Errs)
   structure PrintStruct =
       mkPrintStruct(structure LrTable = MakeLrTable.LrTable
                     structure ShrinkLrTable =
                          ShrinkLrTableFun(structure LrTable=LrTable))
in

(* returns function which takes a file name, invokes the parser on the file,
  does semantic checks, creates table, and prints it *)

   structure ParseGen = ParseGenFun(structure ParseGenParser = ParseGenParser
                                    structure MakeTable = MakeLrTable
                                    structure Verbose = Verbose
                                    structure PrintStruct = PrintStruct
                                    structure Absyn = Absyn)
end

signature BMARK =
  sig
    val doit : int -> unit
    val testit : TextIO.outstream -> unit
  end;
(* main.sml
 *)

structure Main : BMARK =
  struct
    val s = OS.FileSys.getDir()
    fun doit size =
       let
          fun loop n =
             if n = 0
                then ()
             else (ParseGen.parseGen(s^"/DATA/ml.grm");
                   loop(n - 1))
       in loop size
       end
    fun testit _ = ParseGen.parseGen(s^"/DATA/ml.grm")
  end
