(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Sexp: SEXP = 
struct

datatype t =
   Atom of string
 | List of t list
 | String of string

fun layout sexp =
   let 
      open Layout
   in
      case sexp of
         Atom s => str s
       | List sexps => paren (align (List.map (sexps, layout)))
       | String s =>
            str (concat ["\"",
                         String.translate (s, fn c =>
                                           case c of
                                              #"\"" => "\"\""
                                            | #"\\" => "\\\\"
                                            | _ => String.fromChar c),
                         "\""])
   end

val toString = Layout.toString o layout

datatype parseResult =
   Eof
 | Error of string
 | Sexp of t

fun parse (peekChar: unit -> char option,
           getChar: unit -> char option): parseResult =
   let
      exception Err of string
      fun error s = raise (Err s)
      fun atom (cs: char list): t =
         let
            fun done () = Atom (String.fromListRev cs)
         in
            case peekChar () of
               NONE => done ()
             | SOME c =>
                  if Char.isSpace c
                     orelse c = #"(" orelse c = #")" orelse c = #"\""
                         orelse c = #";"
                     then done ()
                  else
                     case getChar () of
                        NONE => done ()
                      | SOME c => atom (c :: cs)
         end
      fun string (cs: char list): t =
         case getChar () of
            NONE => error "eof in middle of string"
          | SOME c =>
               (case c of
                   #"\"" => String (String.fromListRev cs)
                 | #"\\" => (case getChar () of
                                NONE => error "eof in middle of string"
                              | SOME c => string (c :: cs))
                 | _ => string (c :: cs))
      fun ignoreLine (): bool =
         case getChar () of
            NONE => false
          | SOME c => c = #"\n" orelse ignoreLine ()
      fun sexp (): t option =
         case getChar () of
            NONE => NONE
          | SOME c => sexpChar c
      and sexpChar (c: char): t option =
         case c of
            #"(" => SOME (List (finishList []))
          | #")" => error "unmatched )"
          | #"\"" => SOME (string [])
          | #";" => if ignoreLine ()
                       then sexp ()
                    else NONE
          | _ => if Char.isSpace c
                    then sexp ()
                 else SOME (atom [c])
      and finishList (elts: t list): t list =
         case getChar () of
            NONE => error "unmatched ("
          | SOME c =>
               (case c of
                   #")" => rev elts
                 | #";" =>
                      if ignoreLine ()
                         then finishList elts
                      else error "unmatched ("
                 | _ =>
                      if Char.isSpace c
                         then finishList elts
                      else 
                         case sexpChar c of
                            NONE => error "unmatched ("
                          | SOME s => finishList (s :: elts))
   in
      (case sexp () of
          NONE => Eof
        | SOME s => Sexp s) handle Err s => Error s
   end

fun input ins =
   parse (fn () => In.peekChar ins,
          fn () => In.inputChar ins)

fun fromString s =
   let
      val n = String.size s
      val r = ref 0
      fun peekChar () =
         let
            val i = !r
         in
            if i = n
               then NONE
            else SOME (String.sub (s, i))
         end
      fun getChar () =
         let
            val res = peekChar ()
            val _ = if isSome res then r := 1 + !r else ()
         in
            res
         end
   in
      parse (peekChar, getChar)
   end

end
