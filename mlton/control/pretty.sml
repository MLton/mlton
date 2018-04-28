(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 2003-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Pretty: PRETTY =
struct

open Layout

fun casee {default, rules, test} =
   let
      val rules =
         case default of
            NONE => rules
          | SOME l => Vector.concat [rules, Vector.new1 (str "_", l)]
   in
      align [seq [str "case ", test, str " of"],
             indent (alignPrefix (Vector.toListMap
                                  (rules, fn (lhs, rhs) =>
                                   mayAlign [seq [lhs, str " =>"], rhs]),
                                  "| "),
                     2)]
   end

fun conApp {arg, con, targs} =
   seq [con,
        if !Control.showTypes
           then tuple (Vector.toList targs)
        else empty,
        case arg of
           NONE => empty
         | SOME x => seq [str " ", x]]

fun handlee {catch, handler, try} =
   align [try,
          seq [str "handle ", catch, str " => ", handler]]

fun nest (prefix, x, y) =
   align [seq [str prefix, x],
          str "in",
          indent (y, 3),
          str "end"]

fun lett (d, e) = nest ("let ", d, e)

fun locall (d, d') = nest ("local ", d, d')

fun primApp {args, prim, targs} =
   seq [prim,
        if !Control.showTypes
           andalso 0 < Vector.length targs
           then list (Vector.toList targs)
        else empty,
        str " ",
        tuple (Vector.toList args)]

fun raisee exn = seq [str "raise ", exn]

fun seq es = mayAlign (separateLeft (Vector.toList es, ";"))

end
