structure Result:> RESULT =
struct

datatype 'a t =
   No of string
 | Yes of 'a

local
   open Layout
in
   fun layout layoutA =
      fn No s => seq[str "No", paren(str s)]
       | Yes a => seq[str "Yes", paren(layoutA a)]
end

fun map(r, f) =
   case r of
      No s => No s
    | Yes x => Yes(f x)

val isNo = fn No _ => true | _ => false

val isYes = fn Yes _ => true | _ => false
   
end
