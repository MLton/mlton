(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

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
