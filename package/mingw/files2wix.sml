val md5sum =
   case CommandLine.arguments () of
      [md5sum] => md5sum
    | _ => (print "Specify path to md5sum executable\n"; 
            OS.Process.exit OS.Process.failure)

val prefix = "\
   \<?xml version='1.0' encoding='windows-1252'?>\n\
   \<Wix xmlns='http://schemas.microsoft.com/wix/2006/wi'>\n\
   \  <Fragment Id='FragmentFiles'>\n"
val suffix = "\
   \  </Fragment>\n\
   \</Wix>\n"

fun slashes c = if c = #"\\" then #"/" else c
fun hash (c, w) = w * 0w5746711073709751657 + Word64.fromInt (Char.ord (slashes c))
fun alnum c = if Char.isAlphaNum c orelse c = #"." then c else #"_"
fun trim s = if String.size s > 40 then String.substring (s, 0, 40) else s
fun escape s = Word64.toString (foldl hash 0w0 (explode s)) ^ "." ^ 
               trim (CharVector.map alnum (#file (OS.Path.splitDirFile s)))

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
      val md5sum = 
         MLton.Process.create {
            args = ["staging/" ^ path],
            env = NONE,
            path = md5sum,
            stdin  = MLton.Process.Param.null,
            stderr = MLton.Process.Param.self,
            stdout = MLton.Process.Param.pipe
         }
      val input = MLton.Process.Child.textIn (MLton.Process.getStdout md5sum)
      val md5 =
         case TextIO.inputLine input of
            NONE => raise Fail "md5sum provided no hash"
          | SOME s => s
      val _ = MLton.Process.reap md5sum
      
      (* Compute the GUID as the combiniation of content hash + path hash *)
      val pathHash = foldl hash 0w0 (explode path)
      val contentHash = valOf (Word64.fromString (String.substring (md5, 0, 16)))
      val xor = Word64.xorb (pathHash, contentHash)
      
      val zero = "00000000"
      fun pad i s = String.substring (zero, 0, i - String.size s) ^ s 
      val c32 = pad 8 o Word32.toString o Word32.fromLarge o Word64.toLarge
      val c16 = pad 4 o Word16.toString o Word16.fromLarge o Word64.toLarge
      fun s32 i = String.substring (md5, i, 8)
      fun s16 i = String.substring (md5, i, 4)
      val s = Word64.>>
      
      val a32 = c32 (s (xor, 0w32))
      val b16 = c16 (s (xor, 0w16))
      val c16 = c16 xor
      val d16 = s16 16
      val e16 = s16 20
      val f32 = s32 24
   in
      concat [a32, "-", b16, "-", c16, "-", d16, "-", e16, f32 ]
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
