(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

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
		
		val eqTerm : term * term -> bool = (op =)
		val gtTerm : term * term -> bool = fn (T i,T j) => i>j

		val eqNonterm : nonterm * nonterm -> bool = (op =)
		val gtNonterm : nonterm * nonterm -> bool =
		    fn (NT i,NT j) => i>j

		val eqSymbol : symbol * symbol -> bool = (op =)
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
