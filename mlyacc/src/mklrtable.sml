(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

functor mkMakeLrTable (structure IntGrammar : INTGRAMMAR
                     structure LrTable : LR_TABLE
                     sharing type LrTable.term = IntGrammar.Grammar.term
                     sharing type LrTable.nonterm = IntGrammar.Grammar.nonterm
                    ) : MAKE_LR_TABLE =
   struct
        val sub = Array.sub
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

    fun unREDUCE (REDUCE num) = num
      | unREDUCE _ = raise Fail "bug: unexpected action (expected REDUCE)"

    val mergeReduces =
        let val merge = fn state =>
          let fun f (j as (pair1 as (T t1,action1)) :: r1,
                     k as (pair2 as (T t2,action2)) :: r2,result,errs) =
                    if t1 < t2 then f(r1,k,pair1::result,errs)
                    else if t1 > t2 then f(j,r2,pair2::result,errs)
                    else let val num1 = unREDUCE action1
                             val num2 = unREDUCE action2
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
          let val precData = Array.array(length rules,NONE : int option)
          in app (fn RULE {rulenum=r,precedence=p,...} => Array.update(precData,r,p))
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
                else let val rulenum = unREDUCE action
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
                        if (List.exists (fn NONTERM a => a=start
                                     | _ => false) rhs)
                          then START rulenum :: r
                          else r) [] rules

                val nonshiftErrs =
                  List.foldr (fn (RULE {rhs,rulenum,...},r) =>
                          (List.foldr (fn (nonshift,r) =>
                           if (List.exists (fn TERM a => a=nonshift
                                     | _ => false) rhs)
                            then NS(nonshift,rulenum) :: r
                            else r) r noshift)
                       ) [] rules

                val notReduced =
                  let val ruleReduced = Array.array(length rules,false)
                      val test = fn REDUCE i => Array.update(ruleReduced,i,true)
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
