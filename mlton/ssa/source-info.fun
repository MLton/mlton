functor SourceInfo (S: SOURCE_INFO_STRUCTS): SOURCE_INFO =
struct

datatype t =
   Bogus
 | Main
 | PolyEqual
 | Region of Region.t

val bogus = Bogus
val fromRegion = Region
val main = Main
val polyEqual = PolyEqual

val toString =
   fn Bogus => "<unknown>"
    | Main => "<main>"
    | PolyEqual => "<poly-equal>"
    | Region r =>
	 case Region.left r of
	    NONE => "<unknown>"
	  | SOME (SourcePos.T {file, line, ...}) =>
	       let
		  val s = "/basis-library/"
		  val file = 
		     case String.findSubstring {string = file,
						substring = s} of
			NONE => file
		      | SOME i =>
			   concat ["<basis>/",
				   String.dropPrefix (file, i + String.size s)]
	       in
		  concat [file, ":", Int.toString line]
	       end

val layout = Layout.str o toString

end
