(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Postscript: POSTSCRIPT =
struct

nonfix mod div

structure Char =
   struct
      open Char

      fun escapePostscript c =
         if isPrint c
            then (case c of
                     #"\\" => "\\\\"
                   | #"(" => "\\("
                   | #")" => "\\)"
                   | _ => toString c)
         else escapeC c
   end

structure String =
   struct
      open String

      fun escapePostscript s = translate(s, Char.escapePostscript)
   end


datatype t =
   (* Atoms *)
   int of real
 | real of real
 | string of string
 | literal of string
   (* Operators *)
 | abs
 | add
 | arc
 | arcn
 | arct
 | arcto
 | ashow
 | atan
 | awidthshow
 | ceiling
 | charpath
 | clear
 | cleartomark
 | clip
 | clippath
 | cliprestor
 | clipsave
 | closepath
 | colorimage
 | composefont
 | copy
 | copypage
 | cos
 | count
 | counttomark
 | cshow
 | currentcolor
 | currentcolorspace
 | currentdash
 | currentfont
 | currentglobal
 | currentgray
 | currentgstate
 | currenthsbcolor
 | currentlinecap
 | currentlinejoin
 | currentlinewidth
 | currentmiterlimit
 | currentmykcolor
 | currentpagedevice
 | currentpoint
 | currentrgbcolor
 | currentstrokeadjust
 | curveto
 | definefont
 | defineuserobject
 | div
 | dup
 | eoclip
 | eofill
 | erasepage
 | exch
 | execuserobject
 | exp
 | fill
 | findfont
 | flattenpath
 | floor
 | font
 | forall
 | gcheck
 | glyphshow
 | grestore
 | grestoreall
 | gsave
 | gstate
 | idiv
 | image
 | imagemask
 | index
 | initclip
 | initgraphics
 | kshow
 | lineto
 | ln
 | log
 | makefont
 | mark
 | mod
 | moveto
 | mul
 | neg
 | newpath
 | nulldevice
 | pathbbox
 | pathforall
 | pop
 | rand
 | rcurveto
 | rectclip
 | rectfill
 | rectstroke
 | restore
 | reversepath
 | rlineto
 | rmoveto
 | roll
 | rootfont
 | round
 | rrand
 | save
 | scalefont
 | selectfont
 | setbbox
 | setcolor
 | setcolorspace
 | setdash
 | setfont
 | setglobal
 | setgray
 | setgstate
 | sethsbcolor
 | setlinecap
 | setlinejoin
 | setlinewidth
 | setmiterlimit
 | setmykcolor
 | setpagedevice
 | setrgbcolor
 | setstrokeadjust
 | shfill
 | show
 | showpage
 | sin
 | sqrt
 | srand
 | startjob
 | stringwidth
 | stroke
 | strokepath
 | sub
 | truncate
 | uappend
 | ucache
 | ueofill
 | ufill
 | undefinefont
 | undefineuserobject
 | upath
 | userobjects
 | ustroke
 | ustrokepath
 | widthshow
 | xshow
 | xyshow
 | yshow

fun tildeToMinus s =
   String.translate(s, fn #"~" => "-" | c => Char.toString c)

val toString =
   fn int r => tildeToMinus(Int.toString(Real.round r))
    | real r => tildeToMinus(Real.toString r)
    | string s => concat["(", String.escapePostscript s, ")"]
    | literal s => concat["/", s]
    | abs => "abs"
    | add => "add"
    | arc => "arc"
    | arcn => "arcn"
    | arct => "arct"
    | arcto => "arcto"
    | ashow => "ashow"
    | atan => "atan"
    | awidthshow => "awidthshow"
    | ceiling => "ceiling"
    | charpath => "charpath"
    | clear => "clear"
    | cleartomark => "cleartomark"
    | clip => "clip"
    | clippath => "clippath"
    | cliprestor => "cliprestor"
    | clipsave => "clipsave"
    | closepath => "closepath"
    | colorimage => "colorimage"
    | composefont => "composefont"
    | copy => "copy"
    | copypage => "copypage"
    | cos => "cos"
    | count => "count"
    | counttomark => "counttomark"
    | cshow => "cshow"
    | currentcolor => "currentcolor"
    | currentcolorspace => "currentcolorspace"
    | currentdash => "currentdash"
    | currentfont => "currentfont"
    | currentglobal => "currentglobal"
    | currentgray => "currentgray"
    | currentgstate => "currentgstate"
    | currenthsbcolor => "currenthsbcolor"
    | currentlinecap => "currentlinecap"
    | currentlinejoin => "currentlinejoin"
    | currentlinewidth => "currentlinewidth"
    | currentmiterlimit => "currentmiterlimit"
    | currentmykcolor => "currentmykcolor"
    | currentpagedevice => "currentpagedevice"
    | currentpoint => "currentpoint"
    | currentrgbcolor => "currentrgbcolor"
    | currentstrokeadjust => "currentstrokeadjust"
    | curveto => "curveto"
    | definefont => "definefont"
    | defineuserobject => "defineuserobject"
    | div => "div"
    | dup => "dup"
    | eoclip => "eoclip"
    | eofill => "eofill"
    | erasepage => "erasepage"
    | exch => "exch"
    | execuserobject => "execuserobject"
    | exp => "exp"
    | fill => "fill"
    | findfont => "findfont"
    | flattenpath => "flattenpath"
    | floor => "floor"
    | font => "font"
    | forall => "forall"
    | gcheck => "gcheck"
    | glyphshow => "glyphshow"
    | grestore => "grestore"
    | grestoreall => "grestoreall"
    | gsave => "gsave"
    | gstate => "gstate"
    | idiv => "idiv"
    | image => "image"
    | imagemask => "imagemask"
    | index => "index"
    | initclip => "initclip"
    | initgraphics => "initgraphics"
    | kshow => "kshow"
    | lineto => "lineto"
    | ln => "ln"
    | log => "log"
    | makefont => "makefont"
    | mark => "mark"
    | mod => "mod"
    | moveto => "moveto"
    | mul => "mul"
    | neg => "neg"
    | newpath => "newpath"
    | nulldevice => "nulldevice"
    | pathbbox => "pathbbox"
    | pathforall => "pathforall"
    | pop => "pop"
    | rand => "rand"
    | rcurveto => "rcurveto"
    | rectclip => "rectclip"
    | rectfill => "rectfill"
    | rectstroke => "rectstroke"
    | restore => "restore"
    | reversepath => "reversepath"
    | rlineto => "rlineto"
    | rmoveto => "rmoveto"
    | roll => "roll"
    | rootfont => "rootfont"
    | round => "round"
    | rrand => "rrand"
    | save => "save"
    | scalefont => "scalefont"
    | selectfont => "selectfont"
    | setbbox => "setbbox"
    | setcolor => "setcolor"
    | setcolorspace => "setcolorspace"
    | setdash => "setdash"
    | setfont => "setfont"
    | setglobal => "setglobal"
    | setgray => "setgray"
    | setgstate => "setgstate"
    | sethsbcolor => "sethsbcolor"
    | setlinecap => "setlinecap"
    | setlinejoin => "setlinejoin"
    | setlinewidth => "setlinewidth"
    | setmiterlimit => "setmiterlimit"
    | setmykcolor => "setmykcolor"
    | setpagedevice => "setpagedevice"
    | setrgbcolor => "setrgbcolor"
    | setstrokeadjust => "setstrokeadjust"
    | shfill => "shfill"
    | show => "show"
    | showpage => "showpage"
    | sin => "sin"
    | sqrt => "sqrt"
    | srand => "srand"
    | startjob => "startjob"
    | stringwidth => "stringwidth"
    | stroke => "stroke"
    | strokepath => "strokepath"
    | sub => "sub"
    | truncate => "truncate"
    | uappend => "uappend"
    | ucache => "ucache"
    | ueofill => "ueofill"
    | ufill => "ufill"
    | undefinefont => "undefinefont"
    | undefineuserobject => "undefineuserobject"
    | upath => "upath"
    | userobjects => "userobjects"
    | ustroke => "ustroke"
    | ustrokepath => "ustrokepath"
    | widthshow => "widthshow"
    | xshow => "xshow"
    | xyshow => "xyshow"
    | yshow => "yshow"

fun programString(os: t list): string =
   let
      fun loop(os: t list,
               lineLen: int,
               line: string list,
               lines: string list): string =
         let
            fun newLine() = concat("\n" :: rev line) :: lines
         in case os of
            [] => concat(rev(newLine()))
          | oper :: os =>
               let
                  val oper = toString oper
                  val m = String.size oper
                  val lineLen = m + 1 + lineLen
               in if lineLen > 80
                     then loop(os, m + 1, [" ", oper], newLine())
                  else loop(os, lineLen, " " :: oper :: line, lines)
               end
         end
   in loop(os, 0, [], ["%!PS\n"])
   end

val pointsPerInch = 72.0
fun inches(x: real): real = x * pointsPerInch
val pageWidth = inches 8.5
val pageHeight = inches 11.0
val margin = inches 0.2
val dateHeight = inches 0.3
val userHeight = inches 1.2
val width = pageWidth - 2.0 * margin
val dateRatio = 0.6
val dateBase = pageHeight - margin - dateHeight * (1.0 + dateRatio) / 2.0
val userRatio = 0.6
val userBase =
   pageHeight - margin - dateHeight - userHeight * (1.0 + userRatio) / 2.0

fun makeHeader{
               host: string,
               job: string,
               user: string
               }: string =
   let val now = Date.now()
      val time = string(concat["Time: ", Date.fmt(now, "%I:%M:%S %p")])
   in programString
      [save,
       (* Draw boxes *)
       real 0.0, setgray,
       int margin, int (pageHeight - margin), moveto,
       int width, int 0.0, rlineto,
       int 0.0, int (~(dateHeight + userHeight)), rlineto,
       int (~width), int 0.0, rlineto, closepath,
       int 1.0, setlinewidth, stroke,
       int margin, int (pageHeight - margin - dateHeight), moveto,
       int width, int 0.0, rlineto,
       int 2.0, setlinewidth, stroke,
       (* Set the font for dates *)
       literal "Helvetica", findfont,
       int (dateHeight * dateRatio * 1.3),
       scalefont, setfont,
       (* Show the date *)
       int (2.0 * margin),
       int dateBase,
       moveto,
       string(concat["Date: ", Date.fmt(now, "%m/%d/%y")]),
       show,
       (* Show the job name *)
       int (pageWidth / 2.0), string job, stringwidth, pop, int 2.0, div, sub,
       int dateBase, moveto,
       string job, show,
       (* Show the time *)
       int (pageWidth - 2.0 * margin),
       time,
       stringwidth, pop, sub,
       int dateBase, moveto,
       time, show,
       (* Show the user *)
       literal "Helvetica", findfont,
       int (userHeight * userRatio * 1.3),
       scalefont, setfont,
       int (pageWidth / 2.0), string user, stringwidth, pop, int 2.0, div, sub,
       int userBase, moveto,
       real 0.3, setgray,
       string user, show,
       (* Finish *)
       restore,
       showpage]
   end

end      
