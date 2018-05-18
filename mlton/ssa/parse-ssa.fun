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
      (T.char #"[" *> T.sepBy(spaces *> p, T.char #",") <* T.char #"]")

   fun parenOf p = (T.char #"(" *> spaces *> p <* spaces <* T.char #")")

   fun doneRecord' () = T.char #"{" <* T.many(T.delay doneRecord' <|> T.failing(T.char #"}") *> T.next) <* T.char #"}"
   val doneRecord = doneRecord' ()
   fun fromRecord name p = T.peek
      (T.char #"{" *> T.many (() <$ T.delay doneRecord' <|> () <$ T.failing (token name <* symbol "=") <* T.next)
       *> token name *> symbol "=" *> p)

   fun casesOf(con, left, right) = Vector.fromList <$> T.sepBy1
      (left <* spaces <* token "=>" >>= (fn l =>
         right >>= (fn r => con (l, r))),
      spaces *> T.char #"|" *> spaces)

   fun optionOf p = SOME <$> (token "Some" *> T.cut(p)) <|> NONE <$ token "None"


   fun possibly t = t >>= (fn x => case x of NONE => T.fail "Syntax error"
                                           | SOME y => T.pure y)
   val parseInt = possibly ((Int.fromString o String.implode) <$>
         T.many (T.sat(T.next, Char.isDigit))) <|> T.failCut "integer"

   (* too many arguments for the maps, curried to use <*> instead *)
   fun makeTyp resolveTycon (args, ident) = 
         case ident of
              "array" => Type.array (Vector.first args)
            | "intInf" => Type.intInf
            | "pointer" => Type.cpointer
            | "real32" => Type.real RealSize.R32
            | "real64" => Type.real RealSize.R64
            | "ref" => Type.reff (Vector.first args)
            | "thread" => Type.thread
            | "tuple" => Type.tuple args
            | "unit" => Type.unit
            | "vector" => Type.vector (Vector.first args)
            | "weak" => Type.weak (Vector.first args)
            | "word8" => Type.word WordSize.word8
            | "word16" => Type.word WordSize.word16
            | "word32" => Type.word WordSize.word32
            | "word64" => Type.word WordSize.word64
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
      (token "of" *> tupleOf (typ resolveTycon)) <|> Vector.fromList <$> T.many (token "of" *> (T.char #"(" *> (typ
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


   (* Prim EXP *)

   
   fun primAppExp resolveTycon resolveVar = 
      let 
         val var = resolveVar <$> ident <* spaces

         val varExp =
         T.failing (token "in" <|> token "exception" <|> token "val") *> var
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

         fun resolvePrim p = 
            case Prim.fromString p
               of SOME p' =>  T.pure p'
                | NONE => T.fail ("valid primitive, got " ^ p)

         fun makePrimApp(prim, targs, args) = {args=args, prim=prim, targs=targs}
      in
         token "prim" *> T.cut (makePrimApp <$$$>
         (T.any [
            resolveFFI,
            resolveFFISym,
            (ident <* spaces >>= resolvePrim)],
          (tupleOf (typ resolveTycon) <* T.peek(spaces *> tupleOf varExp
          <* spaces) <|> T.pure (Vector.new0 ())),
          spaces *> tupleOf varExp <* spaces))
      end

   fun makeStatement (var, ty, exp) =
      Statement.T
      {var = var,
       ty = ty,
       exp = exp}

   fun statements resolveCon resolveTycon resolveVar =
      let

         val var = resolveVar <$> ident <* spaces

         val varExp =
         T.failing (token "in" <|> token "exception" <|> token "val") *> var

         val typedvar = (fn (x,y) => (x,y)) <$$>
            (SOME <$> var <|> token "_" *> T.pure(NONE),
             symbol ":" *> (typ resolveTycon) <* spaces)
         fun makeConApp(con, args) = { con=con, args=args }
         fun conApp v = 
            makeConApp <$$>
                  (resolveCon <$> ident <* spaces,
                   v)
         val conAppExp = token "new" *> T.cut (conApp ((tupleOf varExp) <|> T.pure (Vector.new0 ())))
         fun constExp typ =
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
         
         fun makeSelect(offset, var) = {offset=offset, tuple=var}
         val selectExp = symbol "#" *> T.cut(makeSelect <$$>
            (parseInt <* spaces,
             T.char #"(" *> varExp <* T.char #")"))
         val profileExp = (ProfileExp.Enter <$> (token "Enter" *> SourceInfo.fromC <$> T.info) <|>
                           ProfileExp.Leave <$> (token "Leave" *> SourceInfo.fromC <$> T.info ))
            <* T.char #"<" <* T.manyCharsFailing(T.char #">") <* T.char #">" <* spaces
         fun exp typ = T.any
            [Exp.ConApp <$> conAppExp,
             Exp.Const <$> constExp typ,
             Exp.PrimApp <$> (primAppExp resolveTycon resolveVar),
             Exp.Profile <$> profileExp,
             Exp.Select <$> selectExp,
             Exp.Tuple <$> (tupleOf varExp ),
             Exp.Var <$> varExp]
         val statement' = makeStatement
            <$>
            (typedvar >>= (fn (var, ty) =>
             (symbol "=" *> exp ty <* spaces) >>= (fn exp =>
               T.pure (var, ty, exp))))
      in
         statement'
      end
   

   fun globls resolveCon resolveTycon resolveVar = 
      let
         fun globals' () = spaces *> token "Globals:" *> Vector.fromList <$>
            T.many (statements resolveCon resolveTycon resolveVar)
      in
         globals' ()
      end

   fun makeFunction name args returns raises label blocks =
      Function.new 
      {args = args,
       blocks = blocks,
       mayInline = false,
       name = name,
       raises = raises,
       returns = returns,
       start = label}


   fun functns resolveCon resolveTycon resolveVar resolveFunc resolveLabel = 
         let 
            val name =  spaces *> symbol "fun" *> resolveFunc <$> ident <*
            spaces


            val var = resolveVar <$> ident <* spaces

            val label = spaces *> symbol "=" *> resolveLabel <$> ident <*
            spaces <* token "()"

            val label' = resolveLabel <$> ident <* spaces
            val con' = spaces *> resolveCon <$> ident <* spaces

             val labelWithArgs = spaces *> resolveLabel <$> ident

            val typedvar = (fn (x,y) => (x,y)) <$$>
               (var ,
                symbol ":" *> (typ resolveTycon) <* spaces)
            val args = spaces *> (tupleOf typedvar <|> T.pure (Vector.new0 ()))
            val vars = spaces *> (tupleOf var <|> T.pure (Vector.new0 ()))

            fun makeConCases var (cons, def) =
               {test=var,
                cases=Cases.Con cons,
                default=def}
            fun makeWordCases var s (wds, def) =
               {test=var,
                cases=Cases.Word (case s of
                    8 => WordSize.word8
                  | 16 => WordSize.word16
                  | 32 => WordSize.word32
                  | 64 => WordSize.word64
                  | _ => raise Fail "makeWordCases" (* can't happen *)
                  , wds),
                default=def}
            fun makePat(con, exp) = T.pure (con, exp)
            fun makeCaseWord size (int, exp) = case size of
                (* this is repetetive, but it's a bit awkward to rework around the fail *)
               8 => T.pure ((WordX.fromIntInf(int, WordSize.word8)), exp)
             | 16 => T.pure ((WordX.fromIntInf(int, WordSize.word16)), exp)
             | 32 => T.pure ((WordX.fromIntInf(int, WordSize.word32)), exp)
             | 64 => T.pure ((WordX.fromIntInf(int, WordSize.word64)), exp)
             | _ => T.fail "valid word size for cases (8, 16, 32 or 64)"

            val defaultCase =
               spaces *> T.optional(T.char #"|" *> spaces *> token "_" *> spaces *> token
                            "=>" *> spaces *> label')

            val casesExp = T.string "case" *>
               T.optional parseInt <* T.many1 space >>= (fn size => T.cut(
                  var <* token "of" <* spaces >>= (fn test =>
                     case size of
                         NONE => makeConCases test <$$>
                           (casesOf(makePat, con', label'),
                            defaultCase)
                       | SOME s => makeWordCases test s <$$>
                           (casesOf(makeCaseWord s, T.string "0x" *> parseHex,
                            label'),
                            defaultCase)
                         )))

            val transferCase = Transfer.Case <$> casesExp
            
            fun makeGoto dst args = 
               Transfer.Goto {dst = dst, args = args}

            val transferGoto = makeGoto
               <$> labelWithArgs
               <*> vars 

            fun makeArith (ty, success, {prim, targs = _, args}, overflow) =
               Transfer.Arith {
                  prim = prim,
                  args = args,
                  overflow = overflow,
                  success = success,
                  ty = ty}

            val transferArith = makeArith <$$$$>
               (T.string "arith" *> spaces *> (typ resolveTycon) <* spaces,
               label',
               symbol "(" *> (primAppExp resolveTycon resolveVar) <* symbol
               ")",
               spaces *> T.string "handle Overflow => " *> label' <* spaces)

            fun makeReturnNonTail cont (handler) = 
               Return.NonTail {
                  cont=cont,
                  handler=
                     case handler of
                          "raise" => Handler.Caller
                        | "dead" => Handler.Dead
                        | _ => Handler.Handle (resolveLabel handler)
               }

            fun returnNonTail cont = makeReturnNonTail cont <$>
               (T.string "handle _ => " *> ident <* spaces)

            fun getReturn return = 
               case return of
                    "dead" => T.pure(Return.Dead)
                  | "return" => T.pure(Return.Tail)
                  | _ => returnNonTail (resolveLabel return)


            fun makeCall args func (return) =
               Transfer.Call {
                  args=args,
                  func=func,
                  return=return 
               }

            val transferCall = 
               spaces *> T.string "call" *> spaces *> ident <* spaces >>= (fn return =>
                  symbol "(" *> resolveFunc <$> ident <* spaces >>= (fn func =>
                      vars <* symbol ")" <* spaces >>= (fn argus =>
                        makeCall argus func <$> (getReturn return)
                     )))

            val transferBug = spaces *> T.string "Bug" *> T.pure(Transfer.Bug) <* spaces

            fun makeRuntime (return , {prim, targs = _, args}) =
               Transfer.Runtime {
                  prim = prim,
                  args = args,
                  return = return
               }

            val transferRuntime = makeRuntime <$$>
            (label',
             parenOf (primAppExp resolveTycon resolveVar))

            fun makeTransferReturn vars = Transfer.Return vars

            val transferReturn = makeTransferReturn <$> (T.string "return" *>
            spaces *> vars <* spaces)

            fun makeTransferRaise vars = Transfer.Raise vars

            val transferRaise = makeTransferRaise <$> (T.string "raise" *>
            spaces *> vars <* spaces)

            val transfer = T.any
               [transferArith, transferCase, transferCall, transferBug,
                transferRuntime, transferRaise, transferReturn, transferGoto]

            fun makeBlock label args statements transfer = 
               Block.T {
                  args = args,
                  label = label,
                  statements = statements,
                  transfer = transfer}

            val block = makeBlock
               <$> labelWithArgs
               <*> args <* spaces
               <*> (Vector.fromList <$> T.many(statements resolveCon resolveTycon
               resolveVar))
               <*> transfer

            val funcs = makeFunction
               <$> name
               <*> args <* symbol ":" <* spaces
               <*> fromRecord "returns" (optionOf (tupleOf (typ resolveTycon) <|> T.pure (Vector.new0 ())))
               <*> fromRecord "raises" (optionOf (tupleOf (typ resolveTycon) <|> T.pure (Vector.new0 ())))
               <* doneRecord
               <*> label
               <*> (Vector.fromList <$> T.manyFailing(block, T.peek(name)))

            val functns' = spaces *> token "Functions:" *> T.many funcs
         in
            functns'
         end

   fun mainFunc resolveFunc = spaces *> token "Main:" *> resolveFunc <$> ident <* spaces

   fun makeProgram (datatypes, globals, main, functions) =
      Program.T
         {datatypes = datatypes,
          functions = functions,
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
         val resolveLabel = makeNameResolver(Label.newString o strip_unique)
      in
         T.compose(skipComments (),
            clOptions *>
            (makeProgram <$$$$> (datatypes resolveCon resolveTycon, globls
            resolveCon resolveTycon resolveVar,
            mainFunc resolveFunc,
            functns resolveCon resolveTycon resolveVar resolveFunc resolveLabel)))
      end
   
   fun parse s = T.parse(program, s)
      
end
