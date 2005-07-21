(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

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

	val nullable = let
	    fun add_rule (RULE { lhs, rhs, ... }, r) = let
		fun addNT (TERM _, _) = NONE
		  | addNT (_, NONE) = NONE
		  | addNT (NONTERM (NT i), SOME ntlist) = SOME (i :: ntlist)
	    in
		case foldr addNT (SOME []) rhs of
		    NONE => r
		  | SOME ntlist => (lhs, ntlist) :: r
	    end
	    val items = List.foldr add_rule [] rules
	    val nullable = array(nonterms,false)
	    fun f ((NT i,nil),(l,_)) = (update(nullable,i,true);
					(l,true))
	      | f (a as (lhs,(h::t)),(l,change)) =
		(case (nullable sub h) of
		     false => (a::l,change)
		   | true => ((lhs,t)::l,true))
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
