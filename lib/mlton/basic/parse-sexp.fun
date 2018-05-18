(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ParseSexp (S: PARSE_SEXP_STRUCTS): PARSE_SEXP =
struct

open S

type 'a t = Sexp.t -> 'a

fun parse (p, s) = p s

fun anything x = x

datatype sexp = datatype Sexp.t

exception Parse

fun wrap (parser, f) sexp = f (parser sexp)

fun anyString sexp =
   case sexp of
      Atom s => s
    | _ => raise Parse

fun atom f = wrap(anyString, f)
fun string s = atom(fn s' => if String.equals(s, s') then () else raise Parse)

fun cons(fx, fl) s =
   case s of
      List(x :: l) => (fx x, fl(List l))
    | _ => raise Parse

fun list f s =
   case s of
      List l => List.map(l, f)
    | _ => raise Parse

fun tuple2(f1, f2) s =
   case s of
      List[s1, s2] => (f1 s1, f2 s2)
    | _ => raise Parse

fun tuple3(f1, f2, f3) s =
   case s of
      List[s1, s2, s3] => (f1 s1, f2 s2, f3 s3)
    | _ => raise Parse

fun tuple4(f1, f2, f3, f4) s =
   case s of
      List[s1, s2, s3, s4] => (f1 s1, f2 s2, f3 s3, f4 s4)
    | _ => raise Parse

fun tuple5(f1, f2, f3, f4, f5) s =
   case s of
      List[s1, s2, s3, s4, s5] => (f1 s1, f2 s2, f3 s3, f4 s4, f5 s5)
    | _ => raise Parse

fun or [] s = raise Parse
  | or (f :: fs) s = f s handle Parse => or fs s   

fun fold (parse: 'a t, base: 'b, f: 'a * 'b -> 'b): 'b t =
   wrap (list parse, fn l => List.fold (l, base, f))

end

structure ParseSexp = ParseSexp(structure Sexp = Sexp)
