functor SourceInfo (S: SOURCE_INFO_STRUCTS): SOURCE_INFO =
struct

type t = string

fun toString s = s

val layout = Layout.str o toString

val equals: t * t -> bool = op =

val hash = String.hash
   
val main = "<main>"
val polyEqual = "<poly-equal>"
val unknown = "<unknown>"

val basisPrefix = "<basis>/"
   
fun fromRegion r =
   case Region.left r of
      NONE => "<unknown>"
    | SOME (SourcePos.T {file, line, ...}) =>
	 let
	    val s = "/basis-library/"
	    val file = 
	       case String.findSubstring {string = file, substring = s} of
		  NONE => file
		| SOME i =>
		     concat [basisPrefix,
			     String.dropPrefix (file, i + String.size s)]
	 in
	    concat [file, ":", Int.toString line]
	 end

fun isBasis s =
   String.isPrefix {prefix = basisPrefix,
		    string = s}

end
