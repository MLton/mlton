functor SourceInfo (S: SOURCE_INFO_STRUCTS): SOURCE_INFO =
struct

structure Pos =
   struct
      datatype t =
	 Known of {file: string,
		   isBasis: bool,
		   line: int}
       | Unknown

      fun equals (p, p') =
	 case (p, p') of
	    (Known {file = f, line = l, ...},
	     Known {file = f', line = l', ...}) =>
	       f = f' andalso l = l'
	   | (Unknown, Unknown) => true
	   | _ => false

      fun toString p =
	 case p of
	    Known {file, line, ...} =>
	       concat [file, ": ", Int.toString line]
	  | Unknown => "<unknown>"

      fun fromRegion r =
	 case Region.left r of
	    NONE => Unknown
	  | SOME (SourcePos.T {file, line, ...}) =>
	       let
		  val s = "/basis-library/"
		  val (file, isBasis) = 
		     case String.findSubstring {string = file, substring = s} of
			NONE => (file, false)
		      | SOME i =>
			   (concat ["<basis>/",
				    String.dropPrefix (file, i + String.size s)],
			    true)
	       in
		  Known {file = file,
			 isBasis = isBasis,
			 line = line}
	       end

      fun isBasis p =
	 case p of
	    Known {isBasis, ...} => isBasis
	  | Unknown => false
   end

datatype info =
   Anonymous of Pos.t
 | C of string
 | Function of {name: string,
		pos: Pos.t}

datatype t = T of {hash: word,
		   info: info,
		   plist: PropertyList.t}

fun new info = T {hash = Random.word (),
		  info = info,
		  plist = PropertyList.new ()}

local
   fun make f (T r) = f r
in
   val hash = make #hash
   val info = make #info
   val plist = make #plist
end

fun anonymous r = new (Anonymous (Pos.fromRegion r))

local
   val set: {hash: word,
	     name: string,
	     sourceInfo: t} HashSet.t =
      HashSet.new {hash = #hash}
in   
   fun fromC (name: string) =
      let
	 val hash = String.hash name
      in
	 #sourceInfo
	 (HashSet.lookupOrInsert
	  (set, hash, fn {hash = h, ...} => hash = h,
	   fn () => {hash = hash,
		     name = name,
		     sourceInfo = new (C name)}))
      end
end

fun function {name, region} =
   new (Function {name = name,
		  pos = Pos.fromRegion region})

fun toString si =
   case info si of
      Anonymous p => Pos.toString p
    | C s => concat ["<", s, ">"]
    | Function {name, pos} => concat [name, "\t", Pos.toString pos]

val layout = Layout.str o toString

val equals: t * t -> bool =
   fn (s, s') => PropertyList.equals (plist s, plist s')

val equals =
   Trace.trace2 ("SourceInfo.equals", layout, layout, Bool.layout) equals
   
fun isBasis (s: t): bool =
   case info s of
      Anonymous p => Pos.isBasis p
    | C _ => false
    | Function {pos, ...} => Pos.isBasis pos

val isBasis =
   Trace.trace ("SourceInfo.isBasis", layout, Bool.layout) isBasis

val gc = fromC "gc"
val gcArrayAllocate = fromC "GC_arrayAllocate>"
val main = fromC "main"
val polyEqual = fromC "poly-equal"
val unknown = fromC "unknown"

end
