(* Copyright (C) 2009,2014 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Layout: LAYOUT =
struct

structure Out = Outstream0   
structure Int = Pervasive.Int
val detailed = ref false

fun switch {detailed = d,normal = n} x =
   if !detailed then d x else n x

structure String = String0

datatype t = T of {length: int,
                   tree: tree}
and tree =
   Empty
  | String of string
  | Sequence of t list
  | Align of {force: bool, rows: t list}
  | Indent of t * int

fun length (T {length, ...}) = length

val empty = T {length = 0, tree = Empty}

fun isEmpty (T {length = 0, ...}) = true
  | isEmpty _ = false

fun str s =
   case s of
      "" => empty
    | _ => T {length = String.size s, tree = String s}

fun fold (l, b, f) = foldl f b l

fun seq ts =
   let val len = fold (ts, 0, fn (t,n) => n + length t)
   in case len of
      0 => empty
    | _ => T {length = len, tree = Sequence ts}
   end

local
   fun make force ts =
      let
         fun loop ts =
            case ts of
               [] => (ts, 0)
             | t :: ts =>
                  let val (ts, n) = loop ts
                  in case length t of
                     0 => (ts, n)
                   | n' => (t :: ts, n + n' + 1)
                  end
         val (ts, len) = loop ts
      in case len of
         0 => empty
       | _ => T {length = len - 1, tree = Align {force = force, rows = ts}}
      end
in
   val align = make true
   val mayAlign = make false
end

fun indent (t, n) = T {length = length t, tree = Indent (t, n)}

val tabSize: int = 8

fun blanks (n: int): string =
   concat [String.make (n div tabSize, #"\t"),
           String.make (n mod tabSize, #" ")]

fun outputTree (t, out) =
   let val print = Out.outputc out
      fun loop (T {tree, length}) =
         (print "(length "
          ; print (Int.toString length)
          ; print ")"
          ; (case tree of
                Empty => print "Empty"
              | String s => (print "(String "; print s; print ")")
              | Sequence ts => loops ("Sequence", ts)
              | Align {rows, ...} => loops ("Align", rows)
              | Indent (t, n) => (print "(Indent "
                                  ; print (Int.toString n)
                                  ; print " "
                                  ; loop t
                                  ; print ")")))
      and loops (s, ts) = (print "("
                           ; print s
                           ; app (fn t => (print " " ; loop t)) ts
                           ; print ")")
   in loop t
   end

fun toString t =
   let
      fun loop (T {tree, ...}, accum) =
         case tree of
            Empty => accum
          | String s => s :: accum
          | Sequence ts => fold (ts, accum, loop)
          | Align {rows, ...} =>
               (case rows of
                   [] => accum
                 | t :: ts =>
                      fold (ts, loop (t, accum), fn (t, ac) =>
                            loop (t, " " :: ac)))
          | Indent (t, _) => loop (t, accum)
   in
      String.concat (rev (loop (t, [])))
   end

fun print {tree: t,
           print: string -> unit,
           lineWidth: int} =
   let
      (*val _ = outputTree (t, out)*)
      fun newline () = print "\n"

      fun outputCompact (t, {at, printAt = _}) =
         let
            fun loop (T {tree, ...}) =
               case tree of
                  Empty => ()
                | String s => print s
                | Sequence ts => app loop ts
                | Indent (t, _) => loop t
                | Align {rows, ...} =>
                     case rows of
                        [] => ()
                      | t :: ts => (loop t
                                    ; app (fn t => (print " "; loop t)) ts)
            val at = at + length t
         in loop t
            ; {at = at, printAt = at}
         end

      fun loop (t as T {length, tree}, state as {at, printAt}) =
         let
            fun prePrint () =
               if at >= printAt
                  then () (* can't back up *)
               else print (blanks (printAt - at))
         in (*Out.print (concat ["at ", Int.toString at,
             * "  printAt ", Int.toString printAt,
             * "\n"]);
             *)
            (*outputTree (t, Out.error)*)
            case tree of
               Empty => state
             | Indent (t, n) => loop (t, {at = at, printAt = printAt + n})
             | Sequence ts => fold (ts, state, loop)
             | String s =>
                  (prePrint ()
                   ; print s
                   ; let val at = printAt + length
                     in {at = at, printAt = at}
                     end)
             | Align {force, rows} =>
                  if not force andalso printAt + length <= lineWidth
                     then (prePrint ()
                           ; outputCompact (t, state))
                  else (case rows of
                           [] => state
                         | t :: ts =>
                              fold
                              (ts, loop (t, state), fn (t, _) =>
                               (newline ()
                                ; loop (t, {at = 0, printAt = printAt}))))
         end
   in ignore (loop (tree, {at = 0, printAt = 0}))
   end

fun outputWidth (t, width, out) =
   print {tree = t,
          lineWidth = width,
          print = Out.outputc out}

local
   val defaultWidth: int = 80
in
   fun output (t, out) = outputWidth (t, defaultWidth, out)
   val print =
      fn (t, p) => print {tree = t, lineWidth = defaultWidth, print = p}
end

fun outputl (t, out) = (output (t, out); Out.newline out)

fun makeOutput layoutX (x, out) = output (layoutX x, out)

fun ignore _ = empty

fun separate (ts, s) =
   case ts of
      [] => []
    | t :: ts => t :: (let val s = str s
                           fun loop [] = []
                             | loop (t :: ts) = s :: t:: (loop ts)
                       in loop ts
                       end)

fun separateLeft (ts, s) =
   case ts of
      [] => []
    | [_] => ts
    | t :: ts => t :: (map (fn t => seq [str s, t]) ts)

fun separateRight (ts, s) =
   rev (let val ts = rev ts
        in case ts of
           [] => []
         | [_] => ts
         | t :: ts => t :: (map (fn t => seq [t, str s]) ts)
        end)

fun alignPrefix (ts, prefix) =
   case ts of
      [] => empty
    | t :: ts =>
         mayAlign [t, indent (mayAlign (map (fn t => seq [str prefix, t]) ts),
                              ~ (String.size prefix))]

local
   fun fillAux ts =
      case ts of
         [] => []
       | [t] => [t]
       | t1::t2::ts => mayAlign [t1, t2] :: fillAux ts
in
   fun fill ts =
      case ts of
         [] => empty
       | [t] => t
       | _ => fill (fillAux ts)
end

local
   fun sequence (start, finish, sep) ts =
      seq [str start, mayAlign (separateRight (ts, sep)), str finish]
in
   val list = sequence ("[", "]", ",")
   val schemeList = sequence ("(", ")", " ")
   val tuple = sequence ("(", ")", ",")
   fun record fts =
      sequence ("{", "}", ",")
      (map (fn (f, t) => seq [str (f ^ " = "), t]) fts)
   fun vector v =
      sequence ("#[", "]", ",")
      (Pervasive.Vector.foldr (op ::) [] v)
   fun array v =
      sequence ("#![", "]", ",")
      (Pervasive.Array.foldr (op ::) [] v)
end

fun namedRecord (name, fields) = seq [str name, str " ", record fields]

fun paren t = seq [str "(", t, str ")"]

fun tuple2 (l1, l2) (x1, x2) = tuple [l1 x1, l2 x2]
fun tuple3 (l1, l2, l3) (x1, x2, x3) = tuple [l1 x1, l2 x2, l3 x3]
fun tuple4 (l1, l2, l3, l4) (x1, x2, x3, x4) = tuple [l1 x1, l2 x2, l3 x3, l4 x4]
fun tuple5 (l1, l2, l3, l4, l5) (x1, x2, x3, x4, x5) =
   tuple [l1 x1, l2 x2, l3 x3, l4 x4, l5 x5]

end
