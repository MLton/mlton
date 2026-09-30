(* See https://github.com/MLton/mlton/issues/658. *)

fun show label fromString toString s =
   (print o concat)
   [label, " \"", String.toString s, "\" = ",
    case fromString s of
       NONE => "NONE"
     | SOME r => concat ["SOME ", toString r],
    "\n"]

fun stringToString s = concat ["\"", String.toString s, "\""]
fun charToString c = concat ["#\"", Char.toString c, "\""]

val showCharFromString =
   show "Char.fromString" Char.fromString charToString

val showStringFromString =
   show "String.fromString" String.fromString stringToString
val showStringFromCString =
   show "String.fromCString" String.fromCString stringToString

fun scanString scan s =
  Option.map (fn (r, ss) => (r, Substring.string ss)) (scan Substring.getc (Substring.full s))
fun scanStringResToString toString (r, s) =
   concat ["(", toString r, ", ", stringToString s, ")"]


val showScanStringCharScan =
   show "scanString Char.scan" (scanString Char.scan) (scanStringResToString charToString)
val showScanStringStringScan =
   show "scanString String.scan" (scanString String.scan) (scanStringResToString stringToString)

val l = ["",
         "\\  \\", "\\  \\abc", "abc\\  \\", "\\  \\abc\\  \\", "abc\\  \\def\\  \\ghi",
         "\\n", "\\nabc", "abc\\n", "\\nabc\\n", "abc\\ndef\\nghi",
         "\\q", "\\qabc", "abc\\q", "\\qabc\\q", "abc\\qdef\\qghi",
         "\n", "\nabc", "abc\n", "\nabc\n", "abc\ndef\nghi",
         "'", "'abc", "abc'", "'abc'", "abc'def'ghi",
         "\"", "\"abc", "abc\"", "\"abc\"", "abc\"def\"ghi",
         "\\\"", "\\\"abc", "abc\\\"", "\\\"abc\\\"", "abc\\\"def\\\"ghi"]

fun doit (f : string -> unit) = (List.app f l; print "\n")

val _ = doit showCharFromString
val _ = doit showStringFromString
val _ = doit showStringFromCString
val _ = doit showScanStringCharScan
val _ = doit showScanStringStringScan
