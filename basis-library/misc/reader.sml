(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Reader: READER =
struct

open Primitive.Int
   
type ('a, 'b) reader = 'b -> ('a * 'b) option

(* local
 *    fun make finish p reader state =
 *       let
 *       fun loop (state, token, tokens) =
 *          case reader state of
 *             NONE => SOME (rev (finish (token, tokens)), state)
 *           | SOME (x, state) =>
 *                let
 *                   val (token, tokens) =
 *                      if p x then ([], finish (token, tokens))
 *                      else (x :: token, tokens)
 *                in loop (state, token, tokens)
 *                end
 *       in loop (state, [], [])
 *       end
 * in
 *     fun tokens p = make (fn (token, tokens) =>
 *                     case token of
 *                        [] => tokens
 *                      | _ => (rev token) :: tokens) p
 *    fun fields p = make (fn (field, fields) => (rev field) :: fields) p
 * end
 *)
   
fun list (reader: ('a, 'b) reader): ('a list, 'b) reader =
   fn state =>
   let
      fun loop (state, accum) =
         case reader state of
            NONE => SOME (rev accum, state)
          | SOME (a, state) => loop (state, a :: accum)
   in loop (state, [])
   end
               
fun readerN (reader: ('a, 'b) reader, n: int): ('a list, 'b) reader =
   fn (state :'b) =>
   let
      fun loop (n, state, accum) =
         if n <= 0
            then SOME (rev accum, state)
         else case reader state of
            NONE => NONE
          | SOME (x, state) => loop (n - 1, state, x :: accum)
   in loop (n, state, [])
   end

fun ignore f reader =
   let
      fun loop state =
         case reader state of
            NONE => NONE
          | SOME (x, state) =>
               if f x
                  then loop state
               else SOME (x, state)
   in loop
   end
val _ = ignore
   
fun map (f: 'a -> 'c) (reader: ('a, 'b) reader): ('c, 'b) reader =
   fn (b: 'b) =>
   case reader b of
      NONE => NONE
    | SOME (a, b) => SOME (f a, b)

fun mapOpt (f: 'a -> 'c option) (reader: ('a, 'b) reader): ('c, 'b) reader =
   fn (b: 'b) =>
   case reader b of
      NONE => NONE
    | SOME (a, b) =>
         case f a of
            NONE => NONE
          | SOME c => SOME (c, b)

fun reader2 reader =
   map (fn [y, z] => (y, z) | _ => raise Fail "Reader.reader2")
   (readerN (reader, 2))
val _ = reader2
   
fun reader3 reader =
   map (fn [x, y, z] => (x, y, z) | _ => raise Fail "Reader.reader3")
   (readerN (reader, 3))
   
fun reader4 reader =
   map (fn [w, x, y, z] => (w, x, y, z) | _ => raise Fail "Reader.reader4")
   (readerN (reader, 4))
   
end   
