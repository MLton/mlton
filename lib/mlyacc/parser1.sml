(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

(* drt (12/15/89) -- the functor should be used during development work,
   but it is wastes space in the release version.
   
functor ParserGen(structure LrTable : LR_TABLE
		  structure Stream : STREAM) : LR_PARSER =
*)

structure LrParser :> LR_PARSER =
 struct
     val print = fn s => output(std_out,s)
     val println = fn s => (print s; print "\n")
     structure LrTable = LrTable
     structure Stream = Stream
     structure Token : TOKEN =
	struct
	    structure LrTable = LrTable
	    datatype ('a,'b) token = TOKEN of LrTable.term * ('a * 'b * 'b)
	    val sameToken = fn (TOKEN (t,_),TOKEN(t',_)) => t=t'
	end
     

     open LrTable 
     open Token

     val DEBUG = false
     exception ParseError

      type ('a,'b) elem = (state * ('a * 'b * 'b))
      type ('a,'b) stack = ('a,'b) elem list

      val showState = fn (STATE s) => ("STATE " ^ (makestring s))

      fun printStack(stack: ('a,'b) elem list, n: int) =
         case stack
           of (state, _) :: rest =>
                 (print("          " ^ makestring n ^ ": ");
                  println(showState state);
                  printStack(rest, n+1)
                 )
            | nil => ()

      val parse = fn {arg : 'a,
		      table : LrTable.table,
		      lexer : ('_b,'_c) token Stream.stream,
		      saction : int * '_c * ('_b,'_c) stack * 'a ->
				nonterm * ('_b * '_c * '_c) * ('_b,'_c) stack,
		      void : '_b,
		      ec = {is_keyword,preferred_change,
			    errtermvalue,showTerminal,
			    error,terms,noShift},
		      lookahead} =>
 let fun prAction(stack as (state, _) :: _, 
		  next as (TOKEN (term,_),_), action) =
             (println "Parse: state stack:";
              printStack(stack, 0);
              print("       state="
                         ^ showState state	
                         ^ " next="
                         ^ showTerminal term
                         ^ " action="
                        );
              case action
                of SHIFT s => println ("SHIFT " ^ showState s)
                 | REDUCE i => println ("REDUCE " ^ (makestring i))
                 | ERROR => println "ERROR"
		 | ACCEPT => println "ACCEPT";
              action)
        | prAction (_,_,action) = action

      val action = LrTable.action table
      val goto = LrTable.goto table

      fun parseStep(next as (TOKEN (terminal, value as (_,leftPos,_)),lexer) :
			('_b,'_c) token * ('_b,'_c) token Stream.stream,
		    stack as (state,_) :: _ : ('_b ,'_c) stack) =
         case (if DEBUG then prAction(stack, next,action(state, terminal))
               else action(state, terminal))
              of SHIFT s => parseStep(Stream.get lexer, (s,value) :: stack)
               | REDUCE i =>
		    let val (nonterm,value,stack as (state,_) :: _ ) =
					 saction(i,leftPos,stack,arg)
		    in parseStep(next,(goto(state,nonterm),value)::stack)
		    end
               | ERROR => let val (_,leftPos,rightPos) = value
		          in error("syntax error\n",leftPos,rightPos);
			     raise ParseError
			  end
  	       | ACCEPT => let val (_,(topvalue,_,_)) :: _ = stack
			       val (token,restLexer) = next
			   in (topvalue,Stream.cons(token,lexer))
			   end
      val next as (TOKEN (terminal,(_,leftPos,_)),_) = Stream.get lexer
   in parseStep(next,[(initialState table,(void,leftPos,leftPos))])
   end
end;

