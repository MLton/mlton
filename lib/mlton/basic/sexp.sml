structure Sexp: SEXP = 
struct

datatype t =
   Atom of string
 | List of t list

local open Layout
in
   fun layout sexp =
      case sexp of
	 Atom s => str s
       | List sexps => paren(align(List.map(sexps, layout)))
end
      
fun input ins =
   let
      fun endAtom c =
	 Char.isSpace c
	 orelse (case c of
		    #"(" => true
		  | #")" => true
		  | _ => false)
      fun char() = In.inputChar ins
      fun atom() = Atom(In.inputTo(ins, endAtom))
      fun sexp() =
(*	 Trace.trace("sexp", Unit.layout, Option.layout layout)*)
	 (fn () => (In.ignoreSpaces ins ;
		    case In.peekChar ins of
		       SOME #"(" => (char() ; SOME(finishList []))
		     | SOME _ => SOME(atom())
		     | NONE => NONE))()
      and finishList elts =
	 (In.ignoreSpaces ins
	  ; (case In.peekChar ins of
		SOME #")" => (char() ; List(rev elts))
	      | SOME _ => (case sexp() of
			      NONE => Error.bug "finishList1"
			    | SOME sexp => finishList(sexp :: elts))
	      | NONE => Error.bug "finishList2"))
   in sexp()
   end
end
