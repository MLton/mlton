(* Modified by mfluet@acm.org on 2005-8-01.
 * Update with SML/NJ 110.55+.
 *)
(* Modified by sweeks@acm.org on 2000-8-24.
 * Ported to MLton.
 *)
type int = Int.int

(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)
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
