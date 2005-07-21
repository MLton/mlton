(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

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
