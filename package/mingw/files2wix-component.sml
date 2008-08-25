val myId =
   case CommandLine.arguments () of
      [] => (print "Missing component group command-line argument.\n"; "XXX")
    | (x :: []) => x
    | (x :: _) => (print "Too many arguments supplied.\n"; x)

val prefix = "\
   \<?xml version='1.0' encoding='windows-1252'?>\n\
   \<Wix xmlns='http://schemas.microsoft.com/wix/2003/01/wi'>\n\
   \  <Fragment Id='Fragment" ^ myId ^ "'>\n"
val suffix = "\
   \  </Fragment>\n\
   \</Wix>\n"

fun escape c = if Char.isAlphaNum c orelse c = #"." then c else #"_"
val escape = CharVector.map escape

fun feature paths =
   let
      val prefix = "\
         \    <ComponentGroup Id='component." ^ myId ^ "'>\n"
      fun component path = 
         case OS.Path.splitDirFile path of {file, dir} =>
         if file = "" orelse dir = "" then "" else "\
         \      <ComponentRef Id='component." ^ escape path ^ "' />\n"
      val suffix = "\
         \    </ComponentGroup>\n"
   in
      print prefix
      ; List.app (print o component) paths
      ; print suffix
   end
 
fun dirEntry path =
   let
      val {dir, file} = OS.Path.splitDirFile path
      val dirId = "dir." ^ escape dir
      val uglypath = escape path
      val guid = guid path
      val file83 = file83 file
   in
      if file = "" orelse dir = "" then "" else
      "    <DirectoryRef Id='" ^ dirId ^ "'>\n\
      \      <Component Id='component." ^ uglypath ^ "' \
                       \Guid='" ^ guid ^ "'>\n\
      \         <File Id='file." ^ uglypath ^ "' \
                     \LongName='" ^ file ^ "' \
                     \Name='" ^ file83 ^ "' DiskId='1' Vital='yes' \
                     \Source='staging/" ^ path ^ "' />\n\
      \      </Component>\n\
      \    </DirectoryRef>\n"
   end 
and guid path = 
   let
      val w32 = Word32.fromLarge o Word.toLarge o MLton.Random.rand
      val w16 = Word16.fromLarge o Word.toLarge o MLton.Random.rand
      val zero = "00000000"
      fun pad i s = String.substring (zero, 0, i - String.size s) ^ s 
      val w32 = pad 8 o Word32.toString o w32
      val w16 = pad 4 o Word16.toString o w16
   in
      w32 () ^"-"^ w16 () ^"-"^ w16 () ^"-"^ w16 () ^"-"^ w16 () ^ w32 ()
   end
and file83 file = 
   let
      val {base, ext} = OS.Path.splitBaseExt file
      fun trunc x s = String.substring (s, 0, Int.min (x, String.size s))
      val (base, ext) = (trunc 8 base, Option.map (trunc 3) ext)
      fun crush c = if Char.contains "\\?|><:/*+,;=[] " c then #"_" else c
      val crush = CharVector.map crush
      val (base, ext) = (crush base, Option.map crush ext)
   in
      case ext of NONE => base | SOME ext => base ^ "." ^ ext
   end

fun trim path = String.substring (path, 0, String.size path - 1)
fun loop files =
   case TextIO.inputLine TextIO.stdIn of NONE => files | SOME path =>
   (print (dirEntry (trim path)); loop ((trim path) :: files))

val () = MLton.Random.srand (Word.fromLargeInt (Time.toNanoseconds (Time.now ())))
val () = print prefix
val files = loop []
val () = feature (rev files)
val () = print suffix
 
