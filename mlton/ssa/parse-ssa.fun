(* Copyright (C) 2017 James Reilly.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ParseSsa (S: PARSE_SSA_STRUCTS): PARSE_SSA =
struct
   open S
   open SsaTree
   structure T = StreamParser
   open T.Ops
   infix 1 <|> >>=
   infix 2 <&>
   infix  3 <*> <* *>
   infixr 4 <$> <$$> <$$$> <$ <$$$$>
   
   fun isInfixChar b = case List.index
      (String.explode "!%&$#+-/:<=>?@\\~'^|*",
       fn c => b = c) of
          SOME _ => true
        | NONE   => false

   fun isIdentFirst b = Char.isAlpha b orelse b = #"'"
   fun isIdentRest b = Char.isAlphaNum b orelse b = #"'" orelse b = #"_" orelse b = #"."

   val stringToken = (fn (x, y) => [x, y]) <$$> (T.char #"\\", T.next) <|>
                     (fn x      => [x]   ) <$> T.next
   fun skipComments () = T.any
      [T.string "(*" *> T.cut (T.manyCharsFailing(T.string "*)" <|> T.string "(*") *>
            ((T.string "*)" <|> "" <$ T.delay skipComments) *> T.each [T.next]
          <|> T.failCut "Closing comment")),
       List.concat <$> T.each
         [T.each [T.char #"\""],
          List.concat <$> T.manyFailing(stringToken, T.char #"\""),
          T.each [T.char #"\""]],
       T.each [T.next]]

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
      (T.char #"(" *> T.sepBy(spaces *> p, T.char #",") <* T.char #")")

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
   fun makeTyp resolveTycon (args, ident) = 
         case ident of
              "tuple" => Type.tuple args
            | "array" => Type.array (Vector.first args) 
            | "vector" => Type.vector (Vector.first args)
            | "ref" => Type.reff (Vector.first args)
            | "word8" => Type.word WordSize.word8
            | "word16" => Type.word WordSize.word16
            | "word32" => Type.word WordSize.word32
            | "word64" => Type.word WordSize.word64
            | "unit" => Type.unit
            | "pointer" => Type.cpointer
            | _ => Type.datatypee (resolveTycon ident)

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

   fun makeCon resolveCon (name, args) = {con = resolveCon name, args = args}

   (* parse in a constructor (to Con.t) *)
   fun constructor resolveCon resolveTycon = (makeCon resolveCon) <$$>
      (ident <* spaces,
      (token "of" *> vectorOf (typ resolveTycon)) <|> Vector.fromList <$> T.many (token "of" *> (T.char #"(" *> (typ
      resolveTycon) <* T.char #")")))

   fun makeDt resolveTycon (tycon, cons) =
      Datatype.T
      {tycon = resolveTycon tycon,
       cons = cons}

   fun datatyp resolveCon resolveTycon = (makeDt resolveTycon) <$$>
      ((spaces *> ident <* spaces <* symbol "="),
       (Vector.fromList <$> T.sepBy1
          ((constructor resolveCon resolveTycon) <* spaces,
           T.char #"|" *> spaces)))

   fun datatypes resolveCon resolveTycon =
      token "Datatypes:" *> Vector.fromList <$> T.many (datatyp resolveCon resolveTycon)

   fun possibly t = t >>= (fn x => case x of NONE => T.fail "Syntax error"
                                           | SOME y => T.pure y)

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

   fun makeStatement resolveTycon resolveVar (var, ty, exp) = 
      (print("\n\nGLOBAL:\n");
      print(Layout.toString (Var.layout var));
      print("\n");
      print(Layout.toString (Type.layout ty));
      print("\n");
      print(Layout.toString (Exp.layout exp));
      print("\n");
      Statement.T
      {var = SOME var,
       ty = ty,
       exp = exp })

   fun globls resolveCon resolveTycon resolveVar = 
      let
         val var = resolveVar <$> ident <* spaces
         val typedvar = (fn (x,y) => (x,y)) <$$>
            (var,
             symbol ":" *> (typ resolveTycon) <* spaces)
         fun makeVarExp var = var
         val varExp =
            T.failing (token "in" <|> token "exception" <|> token "val") *>
            makeVarExp <$> var
         fun makeApp(func, arg) = {arg=arg, func=func}
         fun makeConApp(con, args) = { con=con, args=args }
         fun conApp v = 
            makeConApp <$$>
                  (resolveCon <$> ident <* spaces,
                   v)
         val conAppExp = token "new" *> T.cut (conApp ((vectorOf varExp) <|> T.pure (Vector.new0 ())))
         fun constExp typ =
            (
            print("\nCONST\n");
            print (Layout.toString(Type.layout typ));
            case Type.dest typ of
                 Type.Word ws => Const.Word <$> (T.string "0x" *> parseHex >>=
                 makeWord (Tycon.word ws)) <|> T.failCut "word"
               | Type.Real rs => Const.Real <$> parseReal rs <|> T.failCut "real"
               | Type.IntInf => Const.IntInf <$> parseIntInf <|> T.failCut "integer"
               | Type.CPointer => Const.null <$ token "NULL" <|> T.failCut "null"
               | Type.Vector _  => T.any
                  [Const.string <$> parseString,
                   Const.wordVector <$> parseWord8Vector,
                   T.failCut "string constant"]
               | _ => T.fail "constant"
            )
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
         fun glbl resolveTycon resolveVar = (makeStatement resolveTycon resolveVar)
            <$>
            (typedvar >>= (fn (var, ty) =>
             (symbol "=" *> exp ty <* spaces) >>= (fn exp => 
               T.pure (var, ty, exp))))
         and exp typ = T.any
            [Exp.ConApp <$> conAppExp,
             Exp.Const <$> constExp typ,
             Exp.PrimApp <$> primAppExp,
             Exp.Profile <$> profileExp,
             Exp.Select <$> selectExp,
             Exp.Tuple <$> (tupleOf varExp),
             Exp.Var <$> varExp]
         fun globals' () = spaces *> token "Globals:" *> Vector.fromList <$>
            T.many (glbl resolveTycon resolveVar)
      in
         globals' ()
      end

   fun makeFunction () =
      Function.new 
      {args = Vector.new0 (),
       blocks = Vector.new0 (),
       mayInline = false,
       name = Func.newNoname(),
       raises = NONE,
       returns = NONE,
       start = Label.newNoname()}

   fun funcs resolveTycon resolveVar = T.pure(makeFunction())

   fun functns resolveTycon resolveVar = spaces *> token "Functions:" *> T.many (funcs resolveTycon
      resolveVar) 

   fun mainFunc resolveFunc = spaces *> token "Main:" *> resolveFunc <$> ident <* spaces

   fun makeProgram (datatypes, globals, main) =
      Program.T
         {datatypes = datatypes,
          functions = [],
          globals = globals,
          main = main} 
   
   val program : Program.t StreamParser.t=
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
            case ident of
                 "bool" => Tycon.bool
               | "exn" => Tycon.exn
               | _ => resolveTycon0 ident

         val resolveVar = makeNameResolver(Var.newString o strip_unique)
         val resolveFunc = makeNameResolver(Func.newString o strip_unique)
      in
         T.compose(skipComments (),
            clOptions *>
            (makeProgram <$$$> (datatypes resolveCon resolveTycon, globls
            resolveCon resolveTycon resolveVar,
            mainFunc resolveFunc)))
      end
   
   fun parse s = T.parse(program, s)
      
end
