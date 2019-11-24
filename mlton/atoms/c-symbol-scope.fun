(* Copyright (C) 2019 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor CSymbolScope (S: C_SYMBOL_SCOPE_STRUCTS): C_SYMBOL_SCOPE =
struct

open S

datatype t =
   External
 | Private
 | Public

val equals =
   fn (External, External) => true
    | (Private, Private) => true
    | (Public, Public) => true
    | _ => false

val all = [External, Private, Public]

val toString =
   fn External => "external"
    | Private => "private"
    | Public => "public"

val layout = Layout.str o toString

val parse =
   let
      open Parse
      infix 3 *>
   in
      any (List.map (all, fn ss => kw (toString ss) *> pure ss))
   end

end
