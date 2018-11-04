(* Copyright (C) 2017 James Reilly.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ParseSsa (S: PARSE_SSA_STRUCTS): PARSE_SSA =
struct
   open S
   open SsaTree
   structure P = Parse
   open P.Ops
   infix 1 <|> >>=
   infix  3 <*> <* *>
   infixr 4 <$> <$$> <$$$> <$$$$> <$ <$?> 

   fun isInfixChar b = case List.index
      (String.explode "!%&$#+-/:<=>?@\\~'^|*",
       fn c => b = c) of
          SOME _ => true
        | NONE   => false

   fun isIdentFirst b = Char.isAlpha b orelse b = #"'"
   fun isIdentRest b = Char.isAlphaNum b orelse b = #"'" orelse b = #"_" orelse b = #"."

  val stringToken = (fn (x, y) => [x, y]) <$$> (P.char #"\\", P.next) <|>
                     (fn x      => [x]   ) <$> P.next
   fun skipComments () = P.any
      [P.str "(*" *> P.cut (P.manyCharsFailing(P.str "*)" <|> P.str "(*") *>
            ((P.str "*)" <|> "" <$ P.delay skipComments) *> P.each [P.next]
          <|> P.failCut "Closing comment")),
       List.concat <$> P.each
         [P.each [P.char #"\""],
          List.concat <$> P.manyFailing(stringToken, P.char #"\""),
          P.each [P.char #"\""]],
       P.each [P.next]]

   fun token s = P.notFollowedBy
      (P.str s,
       (P.char #"_") <|> (P.nextSat Char.isAlphaNum)) <* P.spaces
   fun symbol s = P.notFollowedBy
      (P.str s,
       (P.nextSat (fn b => isInfixChar b orelse b = #"_"))) <* P.spaces

   val clOptions = P.manyCharsFailing(P.str "Datatypes:")

   fun 'a makeNameResolver(f: string -> 'a): string -> 'a =
      let
         val table = HashTable.new {hash = String.hash, equals = String.equals}
      in
         fn x => HashTable.lookupOrInsert (table, x, fn () => f x)
      end


   val ident = (
      String.implode <$> (P.any
         [(op ::) <$$>
             (P.nextSat isIdentFirst,
              P.many (P.nextSat isIdentRest)),
          List.append <$$>
             (P.many1 (P.nextSat isInfixChar),
              (op ::) <$$> (P.char #"_", P.many (P.nextSat Char.isDigit)) <|> P.pure []
              (* just for collecting _0 *)
              )])) <|> P.failCut "identifier"

   fun parenOf p = (P.char #"(" *> P.spaces *> p <* P.spaces <* P.char #")")

   fun doneRecord' () = P.char #"{" <* P.many(P.delay doneRecord' <|> P.failing(P.char #"}") *> P.next) <* P.char #"}"
   val doneRecord = doneRecord' ()
   fun fromRecord name p = P.peek
      (P.char #"{" *> P.many (() <$ P.delay doneRecord' <|> () <$ P.failing (token name <* symbol "=") <* P.next)
       *> token name *> symbol "=" *> p)

   fun casesOf(con, left, right) = Vector.fromList <$> P.sepBy1
      (left <* P.spaces <* token "=>" >>= (fn l =>
         right >>= (fn r => con (l, r))),
       P.spaces *> P.char #"|" *> P.spaces)

   fun optionOf p = SOME <$> (token "Some" *> P.cut(p)) <|> NONE <$ token "None"


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
         (((P.tuple (P.delay (typ' resolveTycon))) <|> P.pure (Vector.new0 ())),
         (P.spaces *> ident <* P.spaces))
   in
      fun typ resolveTycon = typ' resolveTycon ()
   end

   val ctype = (P.any o List.map)
               (CType.all, fn ct =>
                ct <$ token (CType.toString ct))

   fun makeCon resolveCon (name, args) = {con = resolveCon name, args = args}

   (* parse in a constructor (to Con.t) *)
   fun constructor resolveCon resolveTycon = (makeCon resolveCon) <$$>
      (ident <* P.spaces,
      (token "of" *> P.tuple (typ resolveTycon)) <|> Vector.fromList <$> P.many (token "of" *> (P.char #"(" *> (typ
      resolveTycon) <* P.char #")")))

   fun makeDt resolveTycon (tycon, cons) =
      Datatype.T
      {tycon = resolveTycon tycon,
       cons = cons}

   fun datatyp resolveCon resolveTycon = (makeDt resolveTycon) <$$>
      ((P.spaces *> ident <* P.spaces <* symbol "="),
       (Vector.fromList <$> P.sepBy1
          ((constructor resolveCon resolveTycon) <* P.spaces,
           P.char #"|" *> P.spaces)))

   fun datatypes resolveCon resolveTycon =
      token "Datatypes:" *> Vector.fromList <$> P.many (datatyp resolveCon resolveTycon)


   val digits = P.many (P.nextSat (fn c => Char.isDigit c orelse c = #"~"))
   val parseIntInf = ((IntInf.fromString o String.implode) <$?> digits) <|> P.failCut "integer"
   val parseString = ((String.fromString o String.implode o List.concat) <$?>
         (P.char #"\"" *> (P.manyFailing(stringToken, P.char #"\"")) <* P.char #"\""))
   fun makeReal s = (case (RealX.make s) of NONE => NONE | x => x) handle Fail _ => NONE
   fun parseReal sz = (makeReal <$?> (fn p => p) <$$> (String.implode <$>
         List.concat <$> P.each
         [P.many (P.nextSat (fn c => Char.isDigit c orelse c = #"~")),
          P.char #"." *> P.pure [#"."] <|> P.pure [],
          P.many (P.nextSat (fn c => Char.isDigit c orelse c = #"E" orelse c = #"~"))],
         P.pure sz))
   val parseReal = fn sz =>
      P.any
      [P.str "inf" *> P.pure (RealX.posInf sz),
       P.str "~inf" *> P.pure (RealX.negInf sz),
       parseReal sz]
   val parseHex = P.fromReader (IntInf.scan(StringCvt.HEX, P.toReader P.next))
   val parseBool = true <$ token "true" <|> false <$ token "false"

   fun makeWord typ int =
      if Tycon.isWordX typ
         then P.pure (WordX.fromIntInf(int, (Tycon.deWordX typ)))
         else P.fail "Invalid word"
   val parseWord8Vector = WordXVector.fromVector <$$>
        (P.pure {elementSize=WordSize.word8},
         P.char #"#" *> P.vector (parseHex >>= makeWord (Tycon.word WordSize.word8)))


   (* Prim EXP *)

   
   fun primAppExp resolveTycon resolveVar = 
      let 
         val var = resolveVar <$> ident <* P.spaces

         val varExp =
         P.failing (token "in" <|> token "exception" <|> token "val") *> var
         val parseConvention = CFunction.Convention.Cdecl <$ token "cdecl" <|>
                                    CFunction.Convention.Stdcall <$ token "stdcall"
         fun makeRuntimeTarget bytes ens mayGC maySwitch modifies readsSt writesSt =
            CFunction.Kind.Runtime ({bytesNeeded=bytes, ensuresBytesFree=ens,
            mayGC=mayGC, maySwitchThreads=maySwitch, modifiesFrontier=modifies,
            readsStackTop=readsSt, writesStackTop=writesSt})
         val parseRuntimeTarget = makeRuntimeTarget
            <$> fromRecord "bytesNeeded" (optionOf P.uint)
            <*> fromRecord "ensuresBytesFree" parseBool
            <*> fromRecord "mayGC" parseBool
            <*> fromRecord "maySwitchThreads" parseBool
            <*> fromRecord "modifiesFrontier" parseBool
            <*> fromRecord "readsStackTop" parseBool
            <*> fromRecord "writesStackTop" parseBool
            <* doneRecord
         val parseKind = CFunction.Kind.Impure <$ token "Impure" <|>
                         CFunction.Kind.Pure <$ token "Pure" <|>
                         token "Runtime" *> P.cut parseRuntimeTarget

         val parsePrototype = (fn x => x) <$$>
            (fromRecord "args" (P.tuple ctype),
             fromRecord "res" (optionOf ctype)) <* doneRecord
         val parseSymbolScope = P.any
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
         val resolveFFI = token "FFI" *> P.cut(
            makeFFI
            <$> fromRecord "args" (P.tuple (typ resolveTycon))
            <*> fromRecord "convention" parseConvention
            <*> fromRecord "kind" parseKind
            <*> fromRecord "prototype" parsePrototype
            <*> fromRecord "return" (typ resolveTycon)
            <*> fromRecord "symbolScope" parseSymbolScope
            <*> fromRecord "target" parseTarget
            <* doneRecord)
         fun makeFFISym name cty symbolScope = Prim.ffiSymbol {name=name, cty=cty, symbolScope=symbolScope}

         val resolveFFISym = token "FFI_Symbol" *> P.cut(
            makeFFISym
            <$> fromRecord "name" ident
            <*> fromRecord "cty" (optionOf ctype)
            <*> fromRecord "symbolScope" parseSymbolScope
            <* doneRecord)

         fun resolvePrim p = 
            case Prim.fromString p
               of SOME p' =>  P.pure p'
                | NONE => P.fail ("valid primitive, got " ^ p)

         fun makePrimApp(prim, targs, args) = {args=args, prim=prim, targs=targs}
      in
         token "prim" *> P.cut (makePrimApp <$$$>
         (P.any [
            resolveFFI,
            resolveFFISym,
            (ident <* P.spaces >>= resolvePrim)],
          (P.tuple (typ resolveTycon) <* P.peek(P.spaces *> P.tuple varExp
          <* P.spaces) <|> P.pure (Vector.new0 ())),
          P.spaces *> P.tuple varExp <* P.spaces))
      end

   fun makeStatement (var, ty, exp) = 
      Statement.T
      {var = var,
       ty = ty,
       exp = exp}

   fun statements resolveCon resolveTycon resolveVar =
      let

         val var = resolveVar <$> ident <* P.spaces

         val varExp =
         P.failing (token "in" <|> token "exception" <|> token "val") *> var

         val typedvar = (fn (x,y) => (x,y)) <$$>
            (SOME <$> var <|> token "_" *> P.pure(NONE),
             symbol ":" *> (typ resolveTycon) <* P.spaces)
         fun makeConApp(con, args) = { con=con, args=args }
         fun conApp v = 
            makeConApp <$$>
                  (resolveCon <$> ident <* P.spaces,
                   v)
         val conAppExp = token "new" *> P.cut (conApp ((P.tuple varExp) <|> P.pure (Vector.new0 ())))
         fun constExp typ =
            case Type.dest typ of
                 Type.Word ws => Const.Word <$> (P.str "0x" *> parseHex >>=
                 makeWord (Tycon.word ws)) <|> P.failCut "word"
               | Type.Real rs => Const.Real <$> parseReal rs <|> P.failCut "real"
               | Type.IntInf => Const.IntInf <$> parseIntInf <|> P.failCut "integer"
               | Type.CPointer => Const.null <$ token "NULL" <|> P.failCut "null"
               | Type.Vector _  => 
                  (* assume it's a word8 vector *)
                  P.any
                  [Const.string <$> parseString,
                   Const.wordVector <$> parseWord8Vector,
                   P.failCut "string constant"]
               | _ => P.fail "constant"
         
         fun makeSelect(offset, var) = {offset=offset, tuple=var}
         val selectExp = symbol "#" *> P.cut(makeSelect <$$>
            (P.uint <* P.spaces,
             P.char #"(" *> varExp <* P.char #")"))
         val profileExp = (ProfileExp.Enter <$ token "Enter" <|>
                           ProfileExp.Leave <$ token "Leave") <*>
                           P.cut ((SourceInfo.fromC o String.implode) <$>
                              P.manyCharsFailing(P.char #"\n") <* P.char #"\n" <* P.spaces)
         fun exp typ = P.any
            [Exp.ConApp <$> conAppExp,
             Exp.Const <$> constExp typ,
             Exp.PrimApp <$> (primAppExp resolveTycon resolveVar),
             Exp.Profile <$> profileExp,
             Exp.Select <$> selectExp,
             Exp.Tuple <$> (P.tuple varExp ),
             Exp.Var <$> varExp]
         val statement' =
            makeStatement
            <$>
            (typedvar >>= (fn (var, ty) =>
             (symbol "=" *> exp ty <* P.spaces) >>= (fn exp => 
               P.pure (var, ty, exp))))
      in
         statement'
      end
   

   fun globls resolveCon resolveTycon resolveVar = 
      let
         fun globals' () = P.spaces *> token "Globals:" *> Vector.fromList <$>
            P.many (statements resolveCon resolveTycon resolveVar)
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
            val name =  P.spaces *> symbol "fun" *> resolveFunc <$> ident <*
            P.spaces


            val var = resolveVar <$> ident <* P.spaces

            val label = P.spaces *> symbol "=" *> resolveLabel <$> ident <*
            P.spaces <* token "()"

            val label' = resolveLabel <$> ident <* P.spaces
            val con' = P.spaces *> resolveCon <$> ident <* P.spaces

             val labelWithArgs = P.spaces *> resolveLabel <$> ident

            val typedvar = (fn (x,y) => (x,y)) <$$>
               (var ,
                symbol ":" *> (typ resolveTycon) <* P.spaces)
            val args = P.spaces *> (P.tuple typedvar <|> P.pure (Vector.new0 ()))
            val vars = P.spaces *> (P.tuple var <|> P.pure (Vector.new0 ()))

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
            fun makePat(con, exp) = P.pure (con, exp)
            fun makeCaseWord size (int, exp) = case size of
                (* this is repetetive, but it's a bit awkward to rework around the fail *)
               8 => P.pure ((WordX.fromIntInf(int, WordSize.word8)), exp)
             | 16 => P.pure ((WordX.fromIntInf(int, WordSize.word16)), exp)
             | 32 => P.pure ((WordX.fromIntInf(int, WordSize.word32)), exp)
             | 64 => P.pure ((WordX.fromIntInf(int, WordSize.word64)), exp)
             | _ => P.fail "valid word size for cases (8, 16, 32 or 64)"

            val defaultCase =
               P.spaces *> P.optional(P.char #"|" *> P.spaces *> token "_" *> P.spaces *> token
                            "=>" *> P.spaces *> label')

            val casesExp = P.str "case" *>
               P.optional P.uint <* P.many1 P.space >>= (fn size => P.cut(
                  var <* token "of" <* P.spaces >>= (fn test =>
                     case size of
                         NONE => makeConCases test <$$>
                           (casesOf(makePat, con', label'),
                            defaultCase)
                       | SOME s => makeWordCases test s <$$>
                           (casesOf(makeCaseWord s, P.str "0x" *> parseHex,
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
               (P.str "arith" *> P.spaces *> (typ resolveTycon) <* P.spaces,
               label',
               symbol "(" *> (primAppExp resolveTycon resolveVar) <* symbol
               ")",
               P.spaces *> P.str "handle Overflow => " *> label' <* P.spaces)

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
               (P.str "handle _ => " *> ident <* P.spaces)

            fun getReturn return = 
               case return of
                    "dead" => P.pure(Return.Dead)
                  | "return" => P.pure(Return.Tail)
                  | _ => returnNonTail (resolveLabel return)


            fun makeCall args func (return) =
               Transfer.Call {
                  args=args,
                  func=func,
                  return=return 
               }

            val transferCall = 
               P.spaces *> P.str "call" *> P.spaces *> ident <* P.spaces >>= (fn return =>
                  symbol "(" *> resolveFunc <$> ident <* P.spaces >>= (fn func =>
                      vars <* symbol ")" <* P.spaces >>= (fn argus =>
                        makeCall argus func <$> (getReturn return)
                     )))

            val transferBug = P.spaces *> P.str "Bug" *> P.pure(Transfer.Bug) <* P.spaces

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

            val transferReturn = makeTransferReturn <$> (P.str "return" *>
            P.spaces *> vars <* P.spaces)

            fun makeTransferRaise vars = Transfer.Raise vars

            val transferRaise = makeTransferRaise <$> (P.str "raise" *>
            P.spaces *> vars <* P.spaces)

            val transfer = P.any
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
               <*> args <* P.spaces
               <*> (Vector.fromList <$> P.many(statements resolveCon resolveTycon
               resolveVar))
               <*> transfer

            val funcs = makeFunction
               <$> name
               <*> args <* symbol ":" <* P.spaces
               <*> fromRecord "returns" (optionOf (P.tuple (typ resolveTycon) <|> P.pure (Vector.new0 ())))
               <*> fromRecord "raises" (optionOf (P.tuple (typ resolveTycon) <|> P.pure (Vector.new0 ())))
               <* doneRecord
               <*> label
               <*> (Vector.fromList <$> P.manyFailing(block, P.peek(name)))

            val functns' = P.spaces *> token "Functions:" *> P.many funcs
         in
            functns'
         end

   fun mainFunc resolveFunc = P.spaces *> token "Main:" *> resolveFunc <$> ident <* P.spaces

   fun makeProgram (datatypes, globals, main, functions) =
      Program.T
         {datatypes = datatypes,
          functions = functions,
          globals = globals,
          main = main} 
   
   val program : Program.t Parse.t =
      let
         fun strip_unique s  = case P.parseString
            (String.implode <$> P.manyCharsFailing(
               P.char #"_" *> P.many1 (P.nextSat Char.isDigit) *> P.failing P.next),
             s) of Result.Yes s' => s'
                 | Result.No _ => s
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
         P.compose(skipComments (),
         clOptions *>
         (makeProgram <$$$$>
             (datatypes resolveCon resolveTycon,
              globls resolveCon resolveTycon resolveVar,
              mainFunc resolveFunc,
              functns resolveCon resolveTycon resolveVar resolveFunc resolveLabel
              (* failing next to check for end of file *)
              <* P.spaces <* (P.failing P.next <|> P.failCut "End of file"))))
      end
      
end
