(* Modified by mfluet@acm.org on 2005-8-01.
 * Update with SML/NJ 110.55+.
 *)
(* Modified by sweeks@acm.org on 2000-8-24.
 * Ported to MLton.
 *)
type int = Int.int

(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

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
             in g(l,0: int)
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


