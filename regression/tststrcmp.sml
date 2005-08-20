(*tststrcmp.sml  27/06/1997 17:21. tho.*)
(*test <, etc. on strings*)

fun pr s = print s;
fun prl s = pr (s ^ "\n");
local
  fun string' 0 = "0"
  |   string' 1 = "1"
  |   string' 2 = "2"
  |   string' 3 = "3"
  |   string' 4 = "4"
  |   string' 5 = "5"
  |   string' 6 = "6"
  |   string' 7 = "7"
  |   string' 8 = "8"
  |   string' 9 = "9"
  |   string' n = string' (n div 10) ^ string' (n mod 10)
in
  fun int_to_string n = if n < 0 then "~" ^ string' (~n) else string' n
end
  
local val r = ref 0
in
fun etst s e1 e2 =
      prl (int_to_string (r := !r + 1; !r)
           ^ (if e1 = e2 then " ok" else " * n o t   o k ! *"))
end

val () = etst "1" ("" < "abc") true;         (*den ene tom*)
val () = etst "2" ("" > "abc") false;
val () = etst "2" ("" >= "abc") false;
val () = etst "2" ("" <= "abc") true;
val () = etst "2" ("" = "abc") false;
val () = etst "2" ("" <> "abc") true;
val () = etst "3" ("abc" < "") false;        (*den anden tom*)
val () = etst "4" ("abc" > "") true;
val () = etst "4" ("abc" >= "") true;
val () = etst "4" ("abc" <= "") false;
val () = etst "4" ("abc" = "") false;
val () = etst "4" ("abc" <> "") true;
val () = etst "5" ("abc" < "abc") false;     (*ens*)
val () = etst "6" ("abc" > "abc") false;
val () = etst "6" ("abc" >= "abc") true;
val () = etst "6" ("abc" <= "abc") true;
val () = etst "6" ("abc" = "abc") true;
val () = etst "6" ("abc" <> "abc") false;
val () = etst "5" ("" < "") false;          (*begge tomme*)
val () = etst "6" ("" > "") false;
val () = etst "6" ("" >= "") true;
val () = etst "6" ("" <= "") true;
val () = etst "6" ("" = "") true;
val () = etst "6" ("" <> "") false;
val () = etst "7" ("abc" < "abcd") true;    (*den ene lngere*)
val () = etst "8" ("abc" > "abcd") false;
val () = etst "8" ("abc" >= "abcd") false;
val () = etst "8" ("abc" <= "abcd") true;
val () = etst "8" ("abc" = "abcd") false;
val () = etst "8" ("abc" <> "abcd") true;
val () = etst "-" ("abcd" < "abc") false;   (*den anden lngere*)
val () = etst "-" ("abcd" > "abc") true;
val () = etst "-" ("abcd" >= "abc") true;
val () = etst "-" ("abcd" <= "abc") false;
val () = etst "-" ("abcd" = "abc") false;
val () = etst "-" ("abcd" <> "abc") true;
val () = etst "-" ("abc" < "abd") true;     (*lige lange, sidste strst*)
val () = etst "-" ("abc" > "abd") false;
val () = etst "-" ("abc" >= "abd") false;
val () = etst "-" ("abc" <= "abd") true;
val () = etst "-" ("abc" = "abd") false;
val () = etst "-" ("abc" <> "abd") true;
val () = etst "-" ("abd" < "abc") false;    (*lige lange, frste strst*)
val () = etst "-" ("abd" > "abc") true;
val () = etst "-" ("abd" >= "abc") true;
val () = etst "-" ("abd" <= "abc") false;
val () = etst "-" ("abd" = "abc") false;
val () = etst "-" ("abd" <> "abc") true;


(*& nu hele molevitten igen bare med meget lange strenge.
 Det er godt nok strenge...*)
  
fun repeat 0 s = ""
  | repeat n s = s ^ repeat (n-1) s
val long = repeat  50 "Der laa den Ridder i Graesset og drev.\n\
                      \Hejsa, nu sadler vi af.\n\
                      \Der laa hans Harnisk, hans Skjold og Vaerge,\n\
                      \Hans Tanker de floej over alle Bjerge.\n\
                      \De floej paa Skyer gennem Luften den blaa ---\n\
                      \Den Rejse man bruger ej Vaaben paa.\n\
                      \\n\
                      \Den Ridder han laa, hvor han steded sig foerst\n\
                      \Han kendte ej Sult, han kendte ej Toerst\n\
                      \Og Solen kom og Stedet og gik;\n\
                      \Han lytted som efter en sagte Musik.\n\
                      \\n\
                      \\n"

val () = etst "1" (long ^ "" < long ^ "abc") true;         (*den ene tom*)
val () = etst "2" (long ^ "" > long ^ "abc") false;
val () = etst "2" (long ^ "" >= long ^ "abc") false;
val () = etst "2" (long ^ "" <= long ^ "abc") true;
val () = etst "2" (long ^ "" = long ^ "abc") false;
val () = etst "2" (long ^ "" <> long ^ "abc") true;
val () = etst "3" (long ^ "abc" < long ^ "") false;        (*den anden tom*)
val () = etst "4" (long ^ "abc" > long ^ "") true;
val () = etst "4" (long ^ "abc" >= long ^ "") true;
val () = etst "4" (long ^ "abc" <= long ^ "") false;
val () = etst "4" (long ^ "abc" = long ^ "") false;
val () = etst "4" (long ^ "abc" <> long ^ "") true;
val () = etst "5" (long ^ "abc" < long ^ "abc") false;     (*ens*)
val () = etst "6" (long ^ "abc" > long ^ "abc") false;
val () = etst "6" (long ^ "abc" >= long ^ "abc") true;
val () = etst "6" (long ^ "abc" <= long ^ "abc") true;
val () = etst "6" (long ^ "abc" = long ^ "abc") true;
val () = etst "6" (long ^ "abc" <> long ^ "abc") false;
val () = etst "5" (long ^ "" < long ^ "") false;          (*begge tomme*)
val () = etst "6" (long ^ "" > long ^ "") false;
val () = etst "6" (long ^ "" >= long ^ "") true;
val () = etst "6" (long ^ "" <= long ^ "") true;
val () = etst "6" (long ^ "" = long ^ "") true;
val () = etst "6" (long ^ "" <> long ^ "") false;
val () = etst "7" (long ^ "abc" < long ^ "abcd") true;    (*den ene lngere*)
val () = etst "8" (long ^ "abc" > long ^ "abcd") false;
val () = etst "8" (long ^ "abc" >= long ^ "abcd") false;
val () = etst "8" (long ^ "abc" <= long ^ "abcd") true;
val () = etst "8" (long ^ "abc" = long ^ "abcd") false;
val () = etst "8" (long ^ "abc" <> long ^ "abcd") true;
val () = etst "-" (long ^ "abcd" < long ^ "abc") false;   (*den anden lngere*)
val () = etst "-" (long ^ "abcd" > long ^ "abc") true;
val () = etst "-" (long ^ "abcd" >= long ^ "abc") true;
val () = etst "-" (long ^ "abcd" <= long ^ "abc") false;
val () = etst "-" (long ^ "abcd" = long ^ "abc") false;
val () = etst "-" (long ^ "abcd" <> long ^ "abc") true;
val () = etst "-" (long ^ "abc" < long ^ "abd") true;     (*lige lange, sidste strst*)
val () = etst "-" (long ^ "abc" > long ^ "abd") false;
val () = etst "-" (long ^ "abc" >= long ^ "abd") false;
val () = etst "-" (long ^ "abc" <= long ^ "abd") true;
val () = etst "-" (long ^ "abc" = long ^ "abd") false;
val () = etst "-" (long ^ "abc" <> long ^ "abd") true;
val () = etst "-" (long ^ "abd" < long ^ "abc") false;    (*lige lange, frste strst*)
val () = etst "-" (long ^ "abd" > long ^ "abc") true;
val () = etst "-" (long ^ "abd" >= long ^ "abc") true;
val () = etst "-" (long ^ "abd" <= long ^ "abc") false;
val () = etst "-" (long ^ "abd" = long ^ "abc") false;
val () = etst "-" (long ^ "abd" <> long ^ "abc") true;

val _ = prl long;
