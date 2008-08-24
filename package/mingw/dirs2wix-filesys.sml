val prefix = "\
   \<?xml version='1.0' encoding='windows-1252'?>\n\
   \<Wix xmlns='http://schemas.microsoft.com/wix/2003/01/wi'>\n\
   \  <Fragment Id='FragmentFileSys'>\n\
   \    <DirectoryRef Id='INSTALLDIR'>\n"
val suffix = "\
   \    </DirectoryRef>\n\
   \  </Fragment>\n\
   \</Wix>\n"

fun escape c = if Char.isAlphaNum c orelse c = #"." then c else #"_"
val escape = CharVector.map escape

val depth = ref 3
fun pad () = CharVector.tabulate (!depth * 2, fn _ => #" ")
fun indent (t, head) =
  let
     val path = head @ [t]
     val paths = String.concatWith "/" path
     val file83 = file83 t
     val long = if file83 = t then "" else " LongName='" ^ t ^ "'"
     val () =
        print (pad () ^ "<Directory Id='dir." ^ escape paths ^ "' \
                                   \Name='" ^ file83 ^ "'" ^ long ^ ">\n")
     val () = depth := !depth + 1
  in
    head
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
