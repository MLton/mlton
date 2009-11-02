val prefix = "\
   \<?xml version='1.0' encoding='windows-1252'?>\n\
   \<Wix xmlns='http://schemas.microsoft.com/wix/2006/wi'>\n\
   \  <Fragment Id='FragmentDirs'>\n\
   \    <DirectoryRef Id='INSTALLDIR'>\n"
val suffix = "\
   \    </DirectoryRef>\n\
   \  </Fragment>\n\
   \</Wix>\n"

(* Identifiers must be unique per path.
 *   => Therefore we include a 64-bit hash.
 *      + Careful: / and \ must hash the same.
 * They must also be alphanumeric and less than 78 bytes.
 *   => We convert non-alnums to _
 *   => We trim the filename to the last 40 bytes.
 * 
 * The contents of the installer cab are sorted by the identifier we choose.
 * Similar files compress better when they are near each other.
 *   => Use: rev(filename).hash as the identifier
 *      + Sorts first by file-type
 *      + Same-named files (in different directories) get clumped together.
 *)
fun slashes c = if c = #"\\" then #"/" else c
fun hash (c, w) = w * 0w5746711073709751657 + Word64.fromInt (Char.ord (slashes c))
fun alnum c = if Char.isAlphaNum c orelse c = #"." then c else #"_"
fun trim s = if String.size s > 40 then String.substring (s, 0, 40) else s
fun rev s = 
   let val len = CharVector.length s in
   CharVector.tabulate (len, fn i => CharVector.sub (s, len-1-i)) end
fun escape s =
   (trim o rev o CharVector.map alnum o #file o OS.Path.splitDirFile) s
   ^ "." ^ Word64.toString (foldl hash 0w0 (explode s))

val depth = ref 3
fun pad () = CharVector.tabulate (!depth * 2, fn _ => #" ")
fun indent (t, head) =
  let
     val path = head @ [t]
     val paths = String.concatWith "/" path
     val () =
        print (pad () ^ "<Directory Id='dir." ^ escape paths ^ "' \
                                   \Name='" ^ t ^ "'>\n")
     val () = depth := !depth + 1
  in
    head
  end

fun unindent _ =
   (depth := !depth - 1
   ; print (pad () ^ "</Directory>\n"))

fun makeDir (prevArcs, path) =
  let
     fun common ([], _) = 0
       | common (_, []) = 0
       | common (x :: r, y :: s) =
           if x = y then 1 + common (r, s) else 0
     val arcs = #arcs (OS.Path.fromString path)
     val common = common (prevArcs, arcs)
     val tailPrev = List.drop (prevArcs, common)
     val tailPath = List.drop (arcs, common)
     val () = List.app unindent tailPrev
     val _ = List.foldl indent (List.take (arcs, common)) tailPath
  in
     arcs
  end

fun trim path = String.substring (path, 0, String.size path - 1)
fun loop last =
   case TextIO.inputLine TextIO.stdIn of NONE => last | SOME path =>
   loop (makeDir (last, trim (path)))

val () = print prefix
val () = List.app unindent (loop [])
val () = print suffix
