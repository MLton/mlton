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

fun tail s =
    if String.size s < 60 then s else
    String.extract (s, String.size s - 60, NONE)
fun escape c = if Char.isAlphaNum c orelse c = #"." then c else #"_"
val escape = tail o CharVector.map escape

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
