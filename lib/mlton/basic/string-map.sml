(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure StringMap: STRING_MAP = 
struct

datatype 'a t = T of {map: {name: string,
                            value: 'a} list ref,
                      default: unit -> 'a}

fun new default = T{map = ref [], default = default}

fun clear (T {map, ...}) = map := []

fun lookup (T {map, default}, name) =
   case List.peek (!map, fn {name = name', ...} => name = name') of
      NONE => let
                 val value = default ()
              in List.push (map, {name = name, value = value})
                 ; value
              end
    | SOME {value, ...} => value

fun domain (T {map, ...}) = List.revMap (!map, fn {name, ...} => name)

fun keepAll (T{map, ...}, pred) = 
   List.keepAllMap (!map, fn {name, value} =>
                    if pred value then SOME name else NONE)

fun foreach (T{map, ...}, f) =
   List.foreach (!map, fn {value, ...} => f value)

end
