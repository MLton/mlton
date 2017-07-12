(* Copyright (C) 2017 Jason Carr.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ParseSxml (S: PARSE_SXML_STRUCTS): PARSE_SXML =
struct
   open S
   open XmlTree
   structure T = StreamParser
   open T.Ops
   infix 1 <|> >>=
   infix 2 <&>
   infix  3 <*> <* *>
   infixr 4 <$> <$$> <$$$> <$

   fun isInfixChar b = case List.index
      (String.explode "!%&$#+-/:<=>?@\\~'^|*",
       fn c => b = c) of
          SOME _ => true
        | NONE   => false

   fun isIdentFirst b = Char.isAlpha b orelse b = #"'"
   fun isIdentRest b = Char.isAlphaNum b orelse b = #"'" orelse b = #"_" orelse b = #"."

   val skipComments = T.optional(
      T.string "(*" *> T.cut (T.manyCharsFailing(T.string "*)") *> T.string "*)"
         <|> T.failCut "Closing comment")
      ) *> T.next

   val space = T.sat(T.next, Char.isSpace)
   val spaces = T.many(space)
   fun token s = T.notFollowedBy
      (T.string s,
       (T.char #"_") <|> (T.sat(T.next,Char.isAlphaNum))) <* spaces
   fun symbol s = T.notFollowedBy
      (T.string s,
       (T.sat(T.next,fn b => isInfixChar b orelse b = #"_"))) <* spaces

   val clOptions = T.manyCharsFailing(T.string "Datatypes:")

   fun 'a makeNameResolver(f: string -> 'a): string -> 'a =
      let
         val hash = String.hash
         val map = HashSet.new{hash= hash o #1}
         fun eq x (a: string * 'a) = String.equals(x, #1 a)
      in
         fn x => (#2 o HashSet.lookupOrInsert)(map, hash x, eq x, fn () => (x, f x))
      end


   val ident = (
      String.implode <$> (T.any
         [(op ::) <$$>
             (T.sat(T.next, isIdentFirst),
              T.many (T.sat(T.next, isIdentRest))),
          List.append <$$>
             (T.many1 (T.sat(T.next, isInfixChar)),
              (op ::) <$$> (T.char #"_", T.many (T.sat(T.next, Char.isDigit))) <|> T.pure []
              (* just for collecting _0 *)
              )])) <|> T.failCut "identifier"



   (* parse a tuple of parsers which must begin with a paren but may be unary *)
   fun tupleOf p = Vector.fromList <$>
      (T.char #"(" *> T.sepBy(spaces *> p, T.char #",") <* T.char #")")

   fun vectorOf p = Vector.fromList <$>
      (T.char #"[" *> T.sepBy(spaces *> p, T.char #",") <* T.char #"]")

   fun doneRecord' () = T.char #"{" <* T.many(T.delay doneRecord' <|> T.failing(T.char #"}") *> T.next) <* T.char #"}"
   val doneRecord = doneRecord' ()
   fun fromRecord name p = T.peek
      (T.char #"{" *> T.many (() <$ T.delay doneRecord' <|> () <$ T.failing (token name <* symbol "=") <* T.next)
       *> token name *> symbol "=" *> p)

   fun casesOf(con, left, right) = Vector.fromList <$> T.sepBy1
      (left <* spaces <* token "=>" >>= (fn l =>
         right >>= (fn r => con (l, r))),
       spaces)

   fun optionOf p = SOME <$> (token "Some" *> T.cut(p)) <|> NONE <$ token "None"


   fun possibly t = t >>= (fn x => case x of NONE => T.fail "Syntax error"
                                           | SOME y => T.pure y)
   val parseInt = possibly ((Int.fromString o String.implode) <$>
         T.many (T.sat(T.next, Char.isDigit))) <|> T.failCut "integer"
   (* too many arguments for the maps, curried to use <*> instead *)
   fun makeTyp resolveTycon (args, ident) = Type.con (resolveTycon ident, args)

   local
      fun typ' resolveTycon () = (makeTyp resolveTycon) <$$>
         (((tupleOf (T.delay (typ' resolveTycon))) <|> T.pure (Vector.new0 ())),
         (spaces *> ident <* spaces))
   in
      fun typ resolveTycon = typ' resolveTycon ()
   end

   val ctype = (T.any o List.map)
               (CType.all, fn ct =>
                ct <$ token (CType.toString ct))

   fun makeCon resolveCon (name, arg) = {con = resolveCon name, arg = arg}

   (* parse in a constructor (to Con.t) *)
   fun constructor resolveCon resolveTycon = (makeCon resolveCon) <$$>
      (ident <* spaces,
      T.optional (token "of" *> (typ resolveTycon) <* spaces) )


   fun makeDt resolveTycon (tycon, cons) =
      {tyvars = Vector.new0 (),
       tycon = resolveTycon tycon, cons = cons}

   fun datatyp resolveCon resolveTycon = (makeDt resolveTycon) <$$>
      ((spaces *> ident <* spaces <* symbol "="),
       (Vector.fromList <$> T.sepBy1
          ((constructor resolveCon resolveTycon) <* spaces,
           T.char #"|" *> spaces)))

   fun datatypes resolveCon resolveTycon =
      token "Datatypes:" *> Vector.fromList <$> T.many (datatyp resolveCon resolveTycon)

   fun overflow resolveVar = T.string "Overflow:" *> spaces *> (
          (T.string "Some" *> spaces *> SOME <$> resolveVar <$> ident <* spaces) <|>
          (NONE <$ T.string "None" <* spaces))


   val stringToken = (fn (x, y) => [x, y]) <$$> (T.char #"\\", T.next) <|>
                           (fn x      => [x]   ) <$> T.next
   val parseString = possibly ((String.fromString o String.implode o List.concat) <$>
         (T.char #"\"" *> (T.manyFailing(stringToken, T.char #"\"")) <* T.char #"\""))
   val parseIntInf = possibly ((IntInf.fromString o String.implode) <$>
         T.many (T.sat(T.next, fn c => Char.isDigit c orelse c = #"~")))
   fun makeReal s = (case (RealX.make s) of NONE => NONE | x => x) handle Fail _ => NONE
   fun parseReal sz = possibly (makeReal <$$> (String.implode <$>
         List.concat <$> T.each
         [T.many (T.sat(T.next, fn c => Char.isDigit c orelse c = #"~")),
          T.char #"." *> T.pure [#"."] <|> T.pure [],
          T.many (T.sat(T.next, fn c => Char.isDigit c orelse c = #"E" orelse c = #"~"))],
         T.pure sz))
   val parseHex = T.fromReader (IntInf.scan(StringCvt.HEX, T.toReader T.next))
   val parseBool = true <$ token "true" <|> false <$ token "false"

   fun makeWord typ int =
      if Tycon.isWordX typ
         then T.pure (WordX.fromIntInf(int, (Tycon.deWordX typ)))
         else T.fail "Invalid word"
   val parseWord8Vector = WordXVector.fromVector <$$>
        (T.pure {elementSize=WordSize.word8},
         T.char #"#" *> vectorOf (parseHex >>= makeWord (Tycon.word WordSize.word8)))

   fun exp resolveCon resolveTycon resolveVar =
      let
         fun makeLet(decs, result) = Exp.make {decs=decs, result=result}
         fun makeExnDec(ca) = Dec.Exception ca
         fun makeValDec(var, ty, exp) = Dec.MonoVal {exp=exp, ty=ty, var=var}
         fun makeFunDecs(lambdas) = Dec.Fun {decs=Vector.fromList lambdas, tyvars=Vector.new0 ()}
         fun makeFunDec((var, ty), lambda) = {lambda=lambda, ty=ty, var=var}
         val var = resolveVar <$> ident <* spaces
         val typedvar = (fn (x,y) => (x,y)) <$$>
            (var,
             symbol ":" *> (typ resolveTycon) <* spaces)
         fun makeVarExp var = VarExp.T {var=var, targs = Vector.new0 ()}
         val varExp =
            T.failing (token "in" <|> token "exception" <|> token "val") *>
            makeVarExp <$> var
         fun makeApp(func, arg) = {arg=arg, func=func}
         fun makeConApp(con, targs, arg) = {arg=arg, con=con, targs=targs}
         fun conApp v = makeConApp <$$$>
            (resolveCon <$> ident <* spaces,
             T.pure (Vector.new0 ()),
             T.optional v)
         val conAppExp = token "new" *> T.cut (conApp varExp)
         fun constExp typ =
            if Tycon.isWordX typ then
               Const.Word <$> (T.string "0x" *> parseHex >>= makeWord typ) <|> T.failCut "word"
            else if Tycon.isRealX typ then
               Const.Real <$> parseReal (Tycon.deRealX typ) <|> T.failCut "real"
            else if Tycon.isIntX typ then
               Const.IntInf <$> parseIntInf <|> T.failCut "integer"
            else if Tycon.equals(typ, Tycon.vector) then
               (* assume it's a word8 vector *)
               T.any
               [Const.string <$> parseString,
                Const.wordVector <$> parseWord8Vector,
                T.failCut "string constant"]

            else
               T.fail "constant"
         fun makeRaise(NONE, exn) = {exn=exn, extend=false}
           | makeRaise(SOME _, exn) = {exn=exn, extend=true}
         val raiseExp = token "raise" *> T.cut (makeRaise <$$> (T.optional (token "extend"), varExp <* spaces))
         fun makeHandle(try, catch, handler) = {catch=catch, handler=handler, try=try}
         fun makeLambda(mayInline, (var, typ), exp) = Lambda.make {arg=var, argType=typ, body=exp, mayInline=mayInline}

         val parseConvention = CFunction.Convention.Cdecl <$ token "cdecl" <|>
                                    CFunction.Convention.Stdcall <$ token "stdcall"
         fun makeRuntimeTarget bytes ens mayGC maySwitch modifies readsSt writesSt =
            CFunction.Kind.Runtime ({bytesNeeded=bytes, ensuresBytesFree=ens,
            mayGC=mayGC, maySwitchThreads=maySwitch, modifiesFrontier=modifies,
            readsStackTop=readsSt, writesStackTop=writesSt})
         val parseRuntimeTarget = makeRuntimeTarget
            <$> fromRecord "bytesNeeded" (optionOf parseInt)
            <*> fromRecord "ensuresBytesFree" parseBool
            <*> fromRecord "mayGC" parseBool
            <*> fromRecord "maySwitchThreads" parseBool
            <*> fromRecord "modifiesFrontier" parseBool
            <*> fromRecord "readsStackTop" parseBool
            <*> fromRecord "writesStackTop" parseBool
            <* doneRecord
         val parseKind = CFunction.Kind.Impure <$ token "Impure" <|>
                         CFunction.Kind.Pure <$ token "Pure" <|>
                         token "Runtime" *> T.cut parseRuntimeTarget

         val parsePrototype = (fn x => x) <$$>
            (fromRecord "args" (tupleOf ctype),
             fromRecord "res" (optionOf ctype)) <* doneRecord
         val parseSymbolScope = T.any
            [CFunction.SymbolScope.External <$ token "external",
             CFunction.SymbolScope.Private <$ token "private",
             CFunction.SymbolScope.Public <$ token "public"]


         val parseTarget = CFunction.Target.Indirect <$ symbol "<*>" <|>
                           CFunction.Target.Direct <$> ident
         fun makeFFI args conv kind prototype return symbolScope target =
            Prim.ffi (CFunction.T
               {args=args, convention=conv, kind=kind,
                prototype=prototype, return=return, symbolScope=symbolScope,
                target = target})
         val resolveFFI = token "FFI" *> T.cut(
            makeFFI
            <$> fromRecord "args" (tupleOf (typ resolveTycon))
            <*> fromRecord "convention" parseConvention
            <*> fromRecord "kind" parseKind
            <*> fromRecord "prototype" parsePrototype
            <*> fromRecord "return" (typ resolveTycon)
            <*> fromRecord "symbolScope" parseSymbolScope
            <*> fromRecord "target" parseTarget
            <* doneRecord)
         fun makeFFISym name cty symbolScope = Prim.ffiSymbol {name=name, cty=cty, symbolScope=symbolScope}
         val resolveFFISym = token "FFI_Symbol" *> T.cut(
            makeFFISym
            <$> fromRecord "name" ident
            <*> fromRecord "cty" (optionOf ctype)
            <*> fromRecord "symbolScope" parseSymbolScope
            <* doneRecord)

         fun resolvePrim p = case Prim.fromString p
            of SOME p' => T.pure p'
             | NONE => T.fail ("valid primitive, got " ^ p)
         fun makePrimApp(prim, targs, args) = {args=args, prim=prim, targs=targs}
         val primAppExp = token "prim" *> T.cut (makePrimApp <$$$>
            (T.any [
               resolveFFI,
               resolveFFISym,
               (ident <* spaces >>= resolvePrim)],
             (vectorOf (typ resolveTycon) <|> T.pure (Vector.new0 ())) <* spaces,
             tupleOf varExp <* spaces))
         fun makeSelect(offset, var) = {offset=offset, tuple=var}
         val selectExp = symbol "#" *> T.cut(makeSelect <$$>
            (parseInt <* spaces,
             varExp))
         val profileExp = (ProfileExp.Enter <$> (token "Enter" *> SourceInfo.fromC <$> T.info) <|>
                           ProfileExp.Leave <$> (token "Leave" *> SourceInfo.fromC <$> T.info ))
            <* T.char #"<" <* T.manyCharsFailing(T.char #">") <* T.char #">" <* spaces
         fun makeConCases var (cons, def) =
            {test=var,
             cases=Cases.Con cons,
             default=Option.map(def, fn x => (x, Region.bogus))}
         fun makeWordCases var s (wds, def) =
            {test=var,
             cases=Cases.Word (case s of
                 8 => Type.WordSize.word8
               | 16 => Type.WordSize.word16
               | 32 => Type.WordSize.word32
               | 64 => Type.WordSize.word64
               | _ => raise Fail "makeWordCases" (* can't happen *)
               , wds),
             default=Option.map(def, fn x => (x, Region.bogus))}
         fun makePat(con, exp) = T.pure (Pat.T con, exp)
         fun makeCaseWord size (int, exp) = case size of
             (* this is repetetive, but it's a bit awkward to rework around the fail *)
            8 => T.pure ((WordX.fromIntInf(int, Type.WordSize.word8)), exp)
          | 16 => T.pure ((WordX.fromIntInf(int, Type.WordSize.word16)), exp)
          | 32 => T.pure ((WordX.fromIntInf(int, Type.WordSize.word32)), exp)
          | 64 => T.pure ((WordX.fromIntInf(int, Type.WordSize.word64)), exp)
          | _ => T.fail "valid word size for cases (8, 16, 32 or 64)"


         fun exp' () = makeLet <$$>
            (token "let" *>
             T.many (dec ()) <* token "in", varExp <* T.string "end")
         and dec () = T.any
            [token "exception" *>
             T.cut (makeExnDec <$> (constructor resolveCon resolveTycon)),
             token "val" *> token "rec" *>
             T.cut (makeFunDecs <$>
                    T.many (makeFunDec <$$> (typedvar <* symbol "=" <* spaces, lambdaExp ()))),
             token "val" *>
             T.cut (makeValDec <$>
                    (typedvar >>= (fn (var, ty) =>
                     (symbol "=" *> primexp ty <* spaces) >>= (fn primexp =>
                      T.pure (var, ty, primexp)))))]
         and primexp typ = T.any
            [PrimExp.Case <$> casesExp (),
             PrimExp.ConApp <$> conAppExp,
             PrimExp.Lambda <$> lambdaExp (),
             (* const must come before select due to vector constants *)
             PrimExp.Const <$> constExp (Type.tycon typ),
             PrimExp.Handle <$> handleExp (),
             PrimExp.PrimApp <$> primAppExp,
             PrimExp.Profile <$> profileExp,
             PrimExp.Raise <$> raiseExp,
             PrimExp.Select <$> selectExp,
             PrimExp.Tuple <$> (tupleOf varExp) <* spaces,
             (* put these last, they just take identifiers so they're pretty greedy *)
             (* App *must* procede var, due to greediness *)
             PrimExp.App <$> makeApp <$$> (varExp, varExp),
             PrimExp.Var <$> varExp]
         and handleExp () = makeHandle <$$$>
            (T.delay exp' <* spaces,
             token "handle" *> T.cut typedvar,
             T.cut (token "=>" *> T.delay exp' <* spaces)
             )
         and lambdaExp () = token "fn" *> T.cut(makeLambda <$$$>
            (false <$ (token "noinline") <|> T.pure true,
             typedvar,
             token "=>" *> T.delay exp' <* spaces))
         and casesExp () = T.string "case" *> T.cut
            (T.optional parseInt <* T.many1 space >>= (fn size =>
               varExp <* token "of" <* spaces >>= (fn var =>
                  case size of
                      NONE => makeConCases var <$$>
                        (casesOf(makePat, conApp typedvar, T.delay exp'),
                         spaces *> T.optional(token "_" *> token "=>" *> T.delay exp'))
                    | SOME s => makeWordCases var s <$$>
                        (casesOf(makeCaseWord s, T.string "0x" *> parseHex, T.delay exp'),
                         spaces *> T.optional(token "_" *> token "=>" *> T.delay exp'))
                      )))
      in
         exp' ()
      end

      fun body resolveCon resolveTycon resolveVar = T.string "Body:" *> spaces
         *> exp resolveCon resolveTycon resolveVar
         (*pure (Exp.fromPrimExp (PrimExp.Tuple (Vector.new0 ()),
   Type.tuple (Vector.new0 ())))*)

   fun makeProgram(datatypes, overflow, body) =
      Program.T
         {body = body,
          datatypes = datatypes,
          overflow = overflow}


   val program : Program.t StreamParser.t =
      let
         fun strip_unique s  = T.parse
            (String.implode <$> T.manyCharsFailing(
               T.char #"_" *> T.many1 (T.sat(T.next, Char.isDigit)) *> T.failing T.next),
             Stream.fromList (String.explode s))
         val resolveCon0 = makeNameResolver(Con.newString o strip_unique)
         fun resolveCon ident =
            case List.peek ([Con.falsee, Con.truee, Con.overflow, Con.reff], fn con =>
                            ident = Con.toString con) of
               SOME con => con
             | NONE => resolveCon0 ident
         val resolveTycon0 = makeNameResolver(Tycon.newString o strip_unique)
         fun resolveTycon ident =
            case List.peekMap (Tycon.prims, fn {name, tycon, ...} =>
                               if ident = name then SOME tycon else NONE) of
               SOME con => con
             | NONE => if ident = "unit"
                          then Tycon.tuple
                       else resolveTycon0 ident
         val resolveVar = makeNameResolver(Var.newString o strip_unique)
      in
         T.compose(skipComments,
         clOptions *>
         (makeProgram <$$$>
            (datatypes resolveCon resolveTycon,
            overflow resolveVar,
            body resolveCon resolveTycon resolveVar <* spaces <* T.failing T.next)))
            (* failing next to check for end of file *)
      end

   fun parse s = T.parse(program, s)
end
