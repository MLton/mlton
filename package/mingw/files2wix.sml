val prefix = "\
   \<?xml version='1.0' encoding='windows-1252'?>\n\
   \<Wix xmlns='http://schemas.microsoft.com/wix/2006/wi'>\n\
   \  <Fragment Id='FragmentFiles'>\n"
val suffix = "\
   \  </Fragment>\n\
   \</Wix>\n"

fun tail s =
    if String.size s < 60 then s else
    String.extract (s, String.size s - 60, NONE)
fun escape c = if Char.isAlphaNum c orelse c = #"." then c else #"_"
val escape = tail o CharVector.map escape

fun dirEntry path =
   let
      val {dir, file} = OS.Path.splitDirFile path
      val dirId = "dir." ^ escape dir
      val uglypath = escape path
      val guid = guid path
   in
      if file = "" orelse dir = "" then "" else
      "    <DirectoryRef Id='" ^ dirId ^ "'>\n\
      \      <Component Id='component." ^ uglypath ^ "' \
                       \Guid='" ^ guid ^ "'>\n\
      \         <File Id='file." ^ uglypath ^ "' \
                     \Name='" ^ file ^ "' DiskId='1' Vital='yes' \
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

fun tail path = String.substring (path, 0, String.size path - 1)
fun head path = if String.isPrefix "./" path 
                then String.extract (path, 2, NONE) 
                else path
val trim = head o tail
fun loop () =
   case TextIO.inputLine TextIO.stdIn of NONE => () | SOME path =>
   ((print o dirEntry o trim) path; loop ())

val () = MLton.Random.srand (Word.fromLargeInt (Time.toNanoseconds (Time.now ())))
val () = print prefix
val () = loop ()
val () = print suffix
 
