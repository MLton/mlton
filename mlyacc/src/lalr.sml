(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

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

		      fun rule_pos (RULE {rhs,...}) =
			  case (rev rhs) of
			      nil => 0
			    | (TERM t) :: r => length rhs
			    | (NONTERM n :: r) => let
				  (* f assumes that everything after n in the
				   * rule has proven to be nullable so far.
				   * Remember that the rhs has been reversed,
				   * implying that this is true initially *)
				  (* A -> .z t B y, where y is nullable *)
				  fun f (b, (r as (TERM _ :: _))) = length r
				    (* A -> .z B C y *)
				    | f (c, (NONTERM b :: r)) =
				      if nullable c then f (b, r)
				      else length r + 1
				    (* A -> .B y, where y is nullable *)
				    | f (_, []) = 0
			      in  f (n, r)
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
		   let	val pos = Int.max(look_pos rule,1)
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
