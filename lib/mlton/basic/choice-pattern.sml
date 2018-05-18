(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure ChoicePattern: CHOICE_PATTERN =
struct

datatype t =
   Concat of t vector
 | Choice of t vector
 | String of string

fun layout t =
   let
      open Layout
   in
      case t of
         Concat v => seq [str "Concat ", Vector.layout layout v]
       | Choice v => seq [str "Choice ", Vector.layout layout v]
       | String s => seq [str "\"", String.layout s, str "\""]
   end

fun fromString (s: string): t Result.t =
   let
      val n = String.size s
      exception Error of string
      fun error ss = raise Error (concat ss)
      datatype state =
         Nest of {start: int}
        | Normal
      fun loop (cur: int,
                ac: char list,
                prev: t list,
                prevChoices: t list,
                state: state): int * t =
         let
            fun accum () = String (String.fromListRev ac) :: prev
            fun finishChoice () =
               Concat (Vector.fromListRev (accum ())) :: prevChoices
            fun keepChar cur =
               loop (cur + 1, String.sub (s, cur) :: ac,
                     prev, prevChoices, state)
         in
            if cur < n
               then
                  let
                     val c = String.sub (s, cur)
                  in
                     case c of
                        #"{" => let
                                   val (cur, t) =
                                      loop (cur + 1, [], [], [],
                                            Nest {start = cur})
                                in
                                   loop (cur, [], t :: accum (), prevChoices,
                                         state)
                                end
                      | #"}" =>
                           (case state of
                               Nest _ =>
                                  (cur + 1,
                                   Choice (Vector.fromList (finishChoice ())))
                             | Normal =>
                                  error ["unmatched } at position ",
                                         Int.toString cur])
                      | #"," =>
                           (case state of
                               Nest _ => loop (cur + 1, [], [], finishChoice (),
                                               state)
                             | Normal => keepChar cur)
                      | #"\\" =>
                           let
                              val cur = cur + 1
                           in
                              if cur = n
                                 then error ["terminating backslash"]
                              else keepChar cur
                           end
                      | _ => keepChar cur
                  end
            else
               (case state of
                   Nest {start} =>
                      error ["unmatched { at position ",
                             Int.toString start]
                 | Normal =>
                      (cur, Concat (Vector.fromListRev (accum ()))))
         end
   in
      Result.Yes (#2 (loop (0, [], [], [], Normal)))
      handle Error s => Result.No s
   end

val fromString =
   Trace.trace ("ChoicePattern.fromString", String.layout, Result.layout layout)
   fromString

fun foldDown (v, a, f) =
   let
      fun loop (i, a) =
         if i < 0
            then a
         else loop (i - 1, f (Vector.sub (v, i), a))
   in
      loop (Vector.length v - 1, a)
   end

fun expandTree (t: t): string list =
   case t of
      Choice v =>
         Vector.fold (v, [], fn (t, ac) =>
                      expandTree t @ ac)
    | Concat v =>
         foldDown (v, [""], fn (t, ac) =>
                   List.fold
                   (expandTree t, [], fn (s, all) =>
                    List.fold
                    (ac, all, fn (s', all) =>
                     concat [s, s'] :: all)))
    | String s => [s]

fun expand (s: string): string list Result.t =
   Result.map (fromString s, expandTree)

end      
