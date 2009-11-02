val myId =
   case CommandLine.arguments () of
      [] => (print "Missing component group command-line argument.\n"; "XXX")
    | (x :: []) => x
    | (x :: _) => (print "Too many arguments supplied.\n"; x)

val prefix = "\
   \<?xml version='1.0' encoding='windows-1252'?>\n\
   \<Wix xmlns='http://schemas.microsoft.com/wix/2006/wi'>\n\
   \  <Fragment Id='Fragment" ^ myId ^ "'>\n\
   \    <ComponentGroup Id='component." ^ myId ^ "'>\n"
val suffix = "\
   \    </ComponentGroup>\n\
   \  </Fragment>\n\
   \</Wix>\n"

fun slashes c = if c = #"\\" then #"/" else c
fun hash (c, w) = w * 0w5746711073709751657 + Word64.fromInt (Char.ord (slashes c))
fun alnum c = if Char.isAlphaNum c orelse c = #"." then c else #"_"
fun trim s = if String.size s > 40 then String.substring (s, 0, 40) else s
fun escape s = Word64.toString (foldl hash 0w0 (explode s)) ^ "." ^ 
               trim (CharVector.map alnum (#file (OS.Path.splitDirFile s)))

fun component path = 
   case OS.Path.splitDirFile path of {file, dir} =>
   if file = "" orelse dir = "" then "" 
   else "      <ComponentRef Id='component." ^ escape path ^ "' />\n"
 
fun tail path = String.substring (path, 0, String.size path - 1)
fun head path = if String.isPrefix "./" path 
                then String.extract (path, 2, NONE) 
                else path
val trim = head o tail
fun loop () =
   case TextIO.inputLine TextIO.stdIn of NONE => () | SOME path =>
   ((print o component o trim) path; loop ())

val () = MLton.Random.srand (Word.fromLargeInt (Time.toNanoseconds (Time.now ())))
val () = print prefix
val () = loop ()
val () = print suffix
