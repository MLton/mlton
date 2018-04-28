(* Copyright (C) 2009,2017 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Popt: POPT =
struct

datatype t =
   Bool of bool -> unit
 | Digit of int -> unit
 | Int of int -> unit
 | Mem of int -> unit
 | None of unit -> unit
 | Real of real -> unit
 | SpaceString of string -> unit
 | SpaceString2 of string * string -> unit
 | String of string -> unit
 | Word of word -> unit

local
   fun make b (r: bool ref): t = None (fn () => r := b)
in
   val trueRef = make true
   val falseRef = make false
end

fun boolRef (r: bool ref): t = Bool (fn b => r := b)

fun intRef (r: int ref): t = Int (fn n => r := n)

fun stringRef (r: string ref): t = String (fn s => r := s)

fun wordRef (r: word ref): t = Word (fn w => r := w)

val trace = ("trace", SpaceString (fn s =>
                                   let
                                      open Trace.Immediate
                                      val _ = debug := Out Out.error
                                   in case s of
                                      "*" => always ()
                                    | _ => (flagged ()
                                            ; on (String.split (s, #",")))
                                   end))

fun memString (s: string): int option =
   let
      val n = String.size s
      fun loop (i, ac) =
         if i >= n
            then SOME ac
         else
            let val c = String.sub (s, i)
               fun done n = SOME (n * ac)
            in case c of
               #"m" => done 1048576
             | #"k" => done 1024
             | _ =>
                  case Char.digitToInt c of
                     NONE => NONE
                   | SOME c => loop (i + 1, ac * 10 + c)
            end
   in loop (0, 0)
   end

(* Parse the command line opts and return any remaining args. *)
fun parse {switches: string list,
           opts: (string * t) list}: string list Result.t =
   let
      exception Error of string
      val rec loop =
         fn [] => []
          | switch :: switches =>
               let
                  fun error s = raise (Error s)
               in
                  case String.sub (switch, 0) of
                     #"-" =>
                     let val switch = String.dropPrefix (switch, 1)
                     in case List.peek (opts, fn (switch', _) =>
                                        switch = switch') of
                        NONE =>
                           let
                              (* Handle the switches where there is no space
                               * separating the argument.
                               *)
                              val rec loop' =
                                 fn [] => error (concat ["unknown switch: -",
                                                         switch])
                                  | (switch', arg) :: opts =>
                                       let
                                          fun doit f =
                                             if String.hasPrefix
                                                (switch, {prefix = switch'})
                                                then f (String.dropPrefix
                                                        (switch, String.size switch'))
                                             else loop' opts
                                       in case arg of
                                          Digit f =>
                                             doit (fn s =>
                                                   let
                                                      val error =
                                                         fn () =>
                                                         error (concat ["invalid digit ", s, " used with -", switch])
                                                   in
                                                      if size s = 1
                                                         then (case Char.digitToInt
                                                                  (String.sub (s, 0)) of
                                                                  NONE => error ()
                                                                | SOME i => f i)
                                                      else error ()
                                                   end)
                                        | String f => doit f
                                        | _ => loop' opts
                                       end
                           in loop' opts
                              ; loop switches
                           end
                      | SOME (_, arg) =>
                           let
                              fun next (f: 'a -> unit, get, msg) =
                                 case switches of
                                    [] =>
                                       error (concat ["-", switch, " requires an argument"])
                                  | switch' :: switches =>
                                       case get switch' of
                                          NONE => error (concat ["-", switch, " requires ", msg])
                                        | SOME n => (f n; loop switches)
                           in
                              case arg of
                                 Bool f => next (f, Bool.fromString, "a boolean")
                               | Digit _ =>
                                    error (concat ["-", switch, " requires a digit"])
                               | Int f => next (f, Int.fromString, "an integer")
                               | Mem f => next (f, memString, "a memory amount")
                               | None f => (f (); loop switches)
                               | Real f => next (f, Real.fromString, "a real")
                               | SpaceString f => next (f, SOME, "")
                               | SpaceString2 f =>
                                    (case switches of
                                        s1 :: s2 :: switches =>
                                           (f (s1, s2); loop switches)
                                      | _ => error (concat ["-", switch, " requires two arguments"]))
                               | String f => (f ""; loop switches)
                               | Word f => next (f, Word.fromString, "a word")
                           end
                     end
                   | _ => switch :: switches
               end
   in
      Result.Yes (loop switches) handle Error s => Result.No s
   end

datatype optionStyle = Normal | Expert

fun makeUsage {mainUsage, makeOptions, showExpert} =
   let
      val usageRef: (string -> unit) option ref = ref NONE
      fun usage (s: string): unit = valOf (!usageRef) s
      fun options () = makeOptions {usage = usage}
      val _ =
         usageRef :=
         SOME
         (fn s =>
          let
             val out = Out.error
             fun message s = Out.outputl (out, s)
             val opts =
                List.fold
                (rev (options ()), [],
                 fn ({arg, desc, opt = _, name, style}, rest) =>
                 if style = Normal orelse showExpert ()
                    then [concat ["    -", name, arg, " "], desc] :: rest
                 else rest)
             val table =
                let
                   open Justify
                in
                   table {columnHeads = NONE,
                          justs = [Left, Left],
                          rows = opts}
                end
          in
             message s
             ; message (concat ["usage: ", mainUsage])
             ; List.foreach (table, fn ss =>
                             message (String.removeTrailing
                                      (concat ss, Char.isSpace)))
             ; let open OS.Process
               in if MLton.isMLton
                     then exit failure
                  else Error.bug "Popt.makeUsage"
               end
          end)
      val parse =
         fn switches =>
         parse {opts = List.map (options (), fn {name, opt, ...} => (name, opt)),
                switches = switches}
   in
      {parse = parse,
       usage = usage}
   end

end
