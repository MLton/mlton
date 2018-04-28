(* Copyright (C) 2003-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor AdmitsEquality (S: ADMITS_EQUALITY_STRUCTS): ADMITS_EQUALITY = 
struct

open S

datatype t = Always | Never | Sometimes

val toString =
   fn Always => "Always"
    | Never => "Never"
    | Sometimes => "Sometimes"

val layout = Layout.str o toString

val op <= =
   fn (Never, _) => true
    | (Sometimes, Never) => false
    | (Sometimes, _) => true
    | (Always, Always) => true
    | (Always, _) => false

val op <= =
   Trace.trace2 ("AdmitsEquality.<=", layout, layout, Bool.layout) (op <=)

val or =
   fn (Always, _) => Always
    | (_, Always) => Always
    | (Sometimes, _) => Sometimes
    | (_, Sometimes) => Sometimes
    | _ => Never

end
