functor AdmitsEquality (S: ADMITS_EQUALITY_STRUCTS): ADMITS_EQUALITY = 
struct

open S

datatype t = Always | Never | Sometimes

val toString =
   fn Always => "Always"
    | Never => "Never"
    | Sometimes => "Sometimes"

val layout = Layout.str o toString

val or =
   fn (Always, _) => Always
    | (_, Always) => Always
    | (Sometimes, _) => Sometimes
    | (_, Sometimes) => Sometimes
    | _ => Never
   
end
