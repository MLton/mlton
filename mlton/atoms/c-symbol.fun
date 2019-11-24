(* Copyright (C) 2019 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor CSymbol (S: C_SYMBOL_STRUCTS): C_SYMBOL =
struct

open S

datatype t = T of {cty: CType.t option,
                   name: string,
                   symbolScope: CSymbolScope.t}

fun equals (T {cty = cty1, name = name1, symbolScope = symbolScope1},
            T {cty = cty2, name = name2, symbolScope = symbolScope2}) =
   Option.equals (cty1, cty2, CType.equals)
   andalso String.equals (name1, name2)
   andalso CSymbolScope.equals (symbolScope1, symbolScope2)

fun hash (T {name, ...}) = String.hash name

fun layout (T {cty, name, symbolScope}) =
  Layout.record [("name", Layout.str name),
                 ("cty", Option.layout CType.layout cty),
                 ("symbolScope", CSymbolScope.layout symbolScope)]

val toString = Layout.toString o layout

val parse =
   let
      open Parse
      infix  1 >>=
      infix  3 *>
      infixr 4 <$$>
      val name =
         spaces *>
         (fn (c, cs) => String.implode (c::cs)) <$$>
         (nextSat (fn c => Char.isAlpha c orelse c = #"_"),
          many (nextSat (fn c => Char.isAlphaNum c orelse c = #"_")))
   in
     cbrack (ffield ("cty", option CType.parse) >>= (fn cty =>
             nfield ("name", name) >>= (fn name =>
             nfield ("symbolScope", CSymbolScope.parse) >>= (fn symbolScope =>
             pure (T {cty = cty, name = name, symbolScope = symbolScope})))))
   end

end
