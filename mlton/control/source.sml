structure Source: SOURCE =
struct

datatype t = T of {file: File.t,
		   lineNum: int ref,
		   lineIndexes: int list ref}

local
   fun make f (T r) = f r
in
   val file = make #file
end

fun new file = T {file = file,
		  lineNum = ref 1,
		  lineIndexes = ref [0]}

fun currentIndex (T {lineIndexes, ...}) =
   case !lineIndexes of
      index :: _ => index
    | _ => Error.bug "Region.current"

fun newline (T {lineIndexes, lineNum, ...}, index) =
   (Int.inc lineNum
    ; List.push (lineIndexes, index))

fun indexPosition (T {lineIndexes, lineNum, ...}, index) =
   let
      fun loop (indexes, lineNum) =
	 case indexes of
	    i :: indexes =>
	       if index >= i
		  then {line = lineNum,
			column = index - i}
	       else loop (indexes, lineNum - 1)
	  | _ => {line = ~1, column = ~1} (*Error.bug "indexToPos"*)
   in loop (!lineIndexes, !lineNum)
   end

end

