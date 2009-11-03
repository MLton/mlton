val prefix = "\
   \<?xml version='1.0' encoding='windows-1252'?>\n\
   \<Wix xmlns='http://schemas.microsoft.com/wix/2006/wi'>\n\
   \  <Fragment Id='FragmentFiles'>\n"
val suffix = "\
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
                     \Source='staging/" ^ path ^ "' KeyPath='yes' />\n\
      \      </Component>\n\
      \    </DirectoryRef>\n"
   end 
and guid path = 
   let
      val guid = 
         MLton.Process.create {
            args = [path],
            env = NONE,
            path = "./guid",
            stdin  = MLton.Process.Param.null,
            stderr = MLton.Process.Param.self,
            stdout = MLton.Process.Param.pipe
         }
      val input = MLton.Process.Child.textIn (MLton.Process.getStdout guid)
      val out =
         case TextIO.inputLine input of
            NONE => raise Fail "guid provided no hash"
          | SOME s => s
      val _ = MLton.Process.reap guid
   in
      out
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
