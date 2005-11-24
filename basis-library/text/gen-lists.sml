(* parse the entire standard input *)
fun parse s =  Vector.fromList (String.fields (fn c => c = #";") s)
fun scanin h = 
   case TextIO.inputLine h of
      NONE => []
    | SOME s => s :: scanin h

val UnicodeData = List.map parse (scanin (TextIO.openIn "UnicodeData.txt"))
val PropList    = List.map parse (scanin (TextIO.openIn "PropList.txt"))
fun hex2int s = valOf (StringCvt.scanString (Int.scan StringCvt.HEX) s)

(* There are four tables of data we must provide for the basis:
  *  category: c => letter | numeral | control | punct
  *  case:     c => lower | upper | space | other
  *    Because Unicode stores clumps characters of similar type together,
  *    we record the data as [ <val0>, <count0>, <val1>, <count1>, ... <valN> ]
  *    val0 is the value of the field, count is the repitition of that value
  *  toupper:  c => c
  *  tolower:  c => c
  *)


(* select characters by their category *)
fun category select = 
   List.mapPartial (fn v => if select (Vector.sub (v, 2)) 
                            then hex2int (Vector.sub (v, 0))
                            else NONE)
                   records

(* convert a alist of integers into closed integer ranges *)
fun compress [] = []
  | compress (x :: r) =
   let
      fun helper ((l, u), []) = [(l, u)]
        | helper ((l, u), n :: r) =
             if u+1 = n then helper ((l, n), r) else
             (l, u) :: helper ((n, n), r)
   in
      helper ((x, x), r)
   end

fun dump [] = ()
  | dump ((l, u) :: r) = (
       print (Int.fmt StringCvt.HEX l ^ "..." ^ Int.fmt StringCvt.HEX u ^ " = " ^ Int.toString (u - l) ^ "\n");
       dump r)
