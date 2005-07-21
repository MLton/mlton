(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

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
