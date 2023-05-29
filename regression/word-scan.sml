fun testScan cvt s =
   (print o concat)
   ["Word.scan ",
    case cvt of
       StringCvt.BIN => "StringCvt.BIN"
     | StringCvt.OCT => "StringCvt.OCT"
     | StringCvt.DEC => "StringCvt.DEC"
     | StringCvt.HEX => "StringCvt.HEX",
    " \"",
    s,
    "\" = ",
    case Word.scan cvt Substring.getc (Substring.full s) of
       NONE => "NONE"
     | SOME (result, rest) =>
          concat ["SOME (0w", Word.fmt StringCvt.DEC result,
                  ", \"", Substring.string rest, "\")"],
    "\n"]

val () = List.app (testScan StringCvt.BIN)
                  ["0", "0w", "0wx", "0x",
                   "01", "0w1", "0wx1", "0x1",
                   "0z", "0wz", "0wxz", "0xz",
                   "01z", "0w1z", "0wx1z", "0x1z"];
val () = List.app (testScan StringCvt.OCT)
                  ["0", "0w", "0wx", "0x",
                   "01", "0w1", "0wx1", "0x1",
                   "0z", "0wz", "0wxz", "0xz",
                   "01z", "0w1z", "0wx1z", "0x1z"];
val () = List.app (testScan StringCvt.DEC)
                  ["0", "0w", "0wx", "0x",
                   "01", "0w1", "0wx1", "0x1",
                   "0z", "0wz", "0wxz", "0xz",
                   "01z", "0w1z", "0wx1z", "0x1z"];
val () = List.app (testScan StringCvt.HEX)
                  ["0", "0w", "0wx", "0x",
                   "01", "0w1", "0wx1", "0x1",
                   "0z", "0wz", "0wxz", "0xz",
                   "01z", "0w1z", "0wx1z", "0x1z"];
