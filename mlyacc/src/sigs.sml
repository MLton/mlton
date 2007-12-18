(* Modified by mfluet@acm.org on 2005-8-01.
 * Update with SML/NJ 110.55+.
 *)
(* Modified by sweeks@acm.org on 2000-8-24.
 * Ported to MLton.
 *)
type int = Int.int

(* ML-Yacc Parser Generator (c) 1989, 1991 Andrew W. Appel, David R. Tarditi *)

signature HEADER =
  sig
    type pos = {line : int, col : int}
    val pos : {line : int ref, start : int ref}
    val text : string list ref 

    type inputSource
    val newSource : string * TextIO.instream * TextIO.outstream -> inputSource
    val error : inputSource -> pos -> string -> unit
    val warn : inputSource -> pos -> string -> unit
    val errorOccurred : inputSource -> unit -> bool

    datatype symbol = SYMBOL of string * pos
    val symbolName : symbol -> string
    val symbolPos : symbol -> pos
    val symbolMake : string * pos -> symbol

    type ty
    val tyName : ty -> string
    val tyMake : string -> ty

    (* associativities: each kind of associativity is assigned a unique
       integer *)

    datatype prec = LEFT | RIGHT | NONASSOC
    datatype control = NODEFAULT | VERBOSE | PARSER_NAME of symbol |
                       FUNCTOR of string  | START_SYM of symbol |
                       NSHIFT of symbol list | POS of string | PURE |
                       PARSE_ARG of string * string |
                       TOKEN_SIG_INFO of string
                           
    datatype rule = RULE of {lhs : symbol, rhs : symbol list,
                             code : {text : string, pos : pos},
                             prec : symbol option}

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
