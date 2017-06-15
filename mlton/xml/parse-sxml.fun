functor ParseSxml(S: PARSE_SXML_STRUCTS) = 
struct
   open S
   structure T = StreamParser
   open T.Ops
   open XmlTree
   structure DE = DirectExp
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


   val space = T.sat(T.next, Char.isSpace) 
   val spaces = T.many(space)
   fun token s = T.notFollowedBy
      (T.string s,
       (T.char #"_") <|> (T.sat(T.next,Char.isAlphaNum))) <* spaces
   fun symbol s = T.notFollowedBy
      (T.string s,
       (T.sat(T.next,fn b => isInfixChar b orelse b = #"_"))) <* spaces

   val clOptions = T.many (T.failing (T.string "Datatypes:") *> T.next)

   fun 'a makeNameResolver(f: string -> 'a): string -> 'a =
      let
         val hash = String.hash 
         val map = HashSet.new{hash= hash o #1}
         fun eq x (a: string * 'a) = String.equals(x, #1 a)
      in
         fn x => (#2 o HashSet.lookupOrInsert)(map, hash x, eq x, fn () => (x, f x))
      end


   val ident = T.failing (token "in" <|> token "val" <|> token "fn" <|> token
   "_" <|> token "=>" <|> token "case" <|> token "prim") *>
      String.implode <$> (T.any
         [(op ::) <$$>
             (T.sat(T.next, isIdentFirst),
              T.many (T.sat(T.next, isIdentRest))),
          List.append <$$>
             (T.many1 (T.sat(T.next, isInfixChar)),
              (op ::) <$$> (T.char #"_", T.many (T.sat(T.next, Char.isDigit))) <|> T.pure []
              (* just for collecting _0 *)
              )])

       

   (* parse a tuple of parsers which must begin with a paren but may be unary *)
   fun tupleOf p = Vector.fromList <$>
      (T.char #"(" *> T.sepBy(p, T.char #"," *> spaces) <* T.char #")")

   fun vectorOf p = Vector.fromList <$>
      (T.char #"[" *> T.sepBy(p, T.char #"," *> spaces) <* T.char #"]")

   fun casesOf(con, left, right) = Vector.fromList <$> T.sepBy1
      (left <* spaces <* token "=>" >>= (fn l =>  
         right >>= (fn r => con (l, r))),
       spaces)


   (* too many arguments for the maps, curried to use <*> instead *)
   fun makeTyp resolveTycon (args, ident) =
      let
         fun makeNullary() =
            case ident of
               "bool" => Type.bool
             (*| "char" => Type.char*)
             | "cpointer" => Type.cpointer
             | "intInf" => Type.intInf
             | "thread" => Type.thread
             | "unit" => Type.unit
             | "word8" => Type.word8
             | "word16" => Type.word Type.WordSize.word16
             | "word32" => Type.word32
             | "word64" => Type.word Type.WordSize.word64
             | other => Type.con (resolveTycon other, args)
         fun makeUnary() = 
            let 
               val arg1 = Vector.sub(args, 0)
            in
               case ident of
                  "array" => Type.array arg1
                | "list" => Type.list arg1
                | "tuple" => Type.tuple args
                | "ref" => Type.reff arg1
                | "vector" => Type.vector arg1
                | "weak" => Type.weak arg1
                | other => Type.con (resolveTycon other, args)
            end
         fun makeBinary() = 
            let
               val arg1 = Vector.sub(args, 0)
               val arg2 = Vector.sub(args, 1)
            in
               case ident of
                  "arrow" => Type.arrow(arg1, arg2)
                | "tuple" => Type.tuple(args)
                | other => Type.con (resolveTycon other, args)
            end
         val numArgs = Vector.length args
         val typ = case numArgs of 
            0 => makeNullary()
          | 1 => makeUnary()
          | 2 => makeBinary()
          | _ => case ident of
                    "tuple" => Type.tuple args
                  | other => Type.con(resolveTycon other, args)
      in
         typ
      end

   local
      fun typ' resolveTycon () = (makeTyp resolveTycon) <$$> 
         (((tupleOf (T.delay (typ' resolveTycon))) <|> T.pure (Vector.new0 ())),
         (spaces *> ident <* spaces))
   in
      fun typ resolveTycon = typ' resolveTycon () 
   end

   
   fun makeCon resolveCon (name, arg) = {con = resolveCon name, arg = arg}

   (* parse in a constructor (to Con.t) *) 
   fun constructor resolveCon resolveTycon = (makeCon resolveCon) <$$> 
      (ident, 
      T.optional (T.many space *> token "of" *> (typ resolveTycon))) <* spaces

   
   fun makeDt resolveTycon (tycon, cons) = 
      {tyvars = Vector.new0 (), 
       tycon = resolveTycon tycon, cons = cons}

   fun datatyp resolveCon resolveTycon = (makeDt resolveTycon) <$$>
      ((spaces *> ident <* spaces <* symbol "="),
       (Vector.fromList <$> T.sepBy1
          ((constructor resolveCon resolveTycon) <* spaces, 
           T.char #"|" *> spaces))) 

   fun datatypes resolveCon resolveTycon = 
      T.string "Datatypes:" *> spaces *> Vector.fromList <$> T.many (datatyp resolveCon resolveTycon)

   fun overflow resolveVar = T.string "Overflow:" *> spaces *> (
          (T.string "Some" *> spaces *> SOME <$> resolveVar <$> ident <* spaces) <|>
          (NONE <$ T.string "None" <* spaces))

   fun possibly t = t >>= (fn x => case x of NONE => T.fail "Syntax error"
                                           | SOME y => T.pure y)

   val stringToken = (fn (x, y) => [x, y]) <$$> (T.char #"\\", T.next) <|>
                           (fn x      => [x]   ) <$> T.next
   val parseString = possibly ((String.fromString o String.implode o List.concat) <$>
         (T.char #"\"" *> (T.many(T.failing (T.char #"\"") *> stringToken)) <* T.char #"\""))
   val parseIntInf = possibly ((IntInf.fromString o String.implode) <$>
         T.many (T.sat(T.next, Char.isDigit)))
   val parseInt = possibly ((Int.fromString o String.implode) <$>
         T.many (T.sat(T.next, Char.isDigit)))
   val parseHex = T.fromReader (IntInf.scan(StringCvt.HEX, T.toReader T.next))

   fun makeWord typ int =
      if Tycon.isWordX typ
         then T.pure (WordX.fromIntInf(int, (Tycon.deWordX typ)))
         else T.fail "Invalid word"


   fun exp resolveCon resolveTycon resolveVar = 
      let
         open XmlTree

         fun makeLet(decs, result) = Exp.make {decs=decs, result=result}
         fun makeValDec(var, ty, exp) = Dec.MonoVal {exp=exp, ty=ty, var=var}
         fun makeFunDecs(lambdas) = Dec.Fun {decs=Vector.fromList lambdas, tyvars=Vector.new0 ()}
         fun makeFunDec((var, ty), lambda) = {lambda=lambda, ty=ty, var=var}
         val var = resolveVar <$> ident <* spaces
         val typedvar = (fn (x,y) => (x,y)) <$$>
            (resolveVar <$> ident <* spaces,
             symbol ":" *> (typ resolveTycon) <* spaces)
         fun makeVarExp var = VarExp.T {var=var, targs = Vector.new0 ()}
         val varExp = makeVarExp <$> (var <* spaces)
         fun makeApp(func, arg) = {arg=arg, func=func}
         val appExp = makeApp <$$> (varExp, varExp)
         fun makeConApp(con, targs, arg) = {arg=arg, con=con, targs=targs}
         fun conApp v = makeConApp <$$$>
            (resolveCon <$> ident <* spaces,
             T.pure (Vector.new0 ()),
             T.optional v)
         val conAppExp = token "new" *> conApp varExp
         fun constExp typ = 
            if Tycon.isWordX typ then
               Const.Word <$> (T.string "0x" *> parseHex >>= makeWord typ) <|> T.fail "Expected word"
            else if Tycon.isRealX typ then
               T.fail "Expected real"
            else if Tycon.isIntX typ then
               Const.IntInf <$> parseIntInf <|> T.fail "Expected integer"
            else
               Const.string <$> parseString
         fun makeRaise(NONE, exn) = {exn=exn, extend=false}
           | makeRaise(SOME _, exn) = {exn=exn, extend=true}
         val raiseExp = makeRaise <$$> (token "raise" *> T.optional (token "extend"), varExp <* spaces)
         fun makeHandle(try, catch, handler) = {catch=catch, handler=handler, try=try}
         fun makeLambda((var, typ), exp) = Lambda.make {arg=var, argType=typ, body=exp, mayInline=false}
         fun resolvePrim p = case Prim.fromString p
            of SOME p' => T.pure p'
             | NONE => T.fail ("Invalid primitive application: " ^ p)
         fun makePrimApp(prim, targs, args) = {args=args, prim=prim, targs=targs}
         val primAppExp = makePrimApp <$$$>
            (token "prim" *> ident <* spaces >>= resolvePrim,
             (vectorOf (typ resolveTycon) <|> T.pure (Vector.new0 ())) <* spaces,
             tupleOf varExp <* spaces)
         fun makeSelect(offset, var) = {offset=offset, tuple=var}
         val selectExp = makeSelect <$$>
            (symbol "#" *> parseInt <* spaces,
             varExp)
         val profileExp = ProfileExp.Enter <$> (token "Enter" *> SourceInfo.fromC <$> T.info) <|>
                          ProfileExp.Leave <$> (token "Leave" *> SourceInfo.fromC <$> T.info )
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
            8 => T.pure((WordX.fromIntInf(int, Type.WordSize.word8)), exp)
          | 16 => T.pure ((WordX.fromIntInf(int, Type.WordSize.word16)), exp)
          | 32 => T.pure ((WordX.fromIntInf(int, Type.WordSize.word8)), exp)
          | 64 => T.pure ((WordX.fromIntInf(int, Type.WordSize.word64)), exp)
          | _ => T.fail "Invalid word size for cases"
            

         fun exp' () = makeLet <$$> 
            (token "let" *>
            T.many (dec ()) <* token "in", varExp <* T.string "end")
         and dec () = T.any
            [token "val rec" *> makeFunDecs <$>
                T.many (makeFunDec <$$> (typedvar <* symbol "=" <* spaces, lambdaExp ())),
             makeValDec <$>
                ((token "val" *> typedvar) >>= (fn var =>
                 (symbol "=" *> primexp (#2 var) <* spaces) >>= (fn primexp =>
                     T.pure(#1 var, #2 var, primexp))))]
         and primexp typ = T.failing(token "in") *> T.any
            [PrimExp.Case <$> casesExp (),
             PrimExp.ConApp <$> conAppExp,
             PrimExp.Lambda <$> lambdaExp (),
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
             token "handle" *> typedvar,
             token "=>" *> T.delay exp' <* spaces
             )
         and lambdaExp () = makeLambda <$$>
            (token "fn" *> typedvar,
             token "=>" *> T.delay exp' <* spaces)
         and casesExp () = 
            (T.string "case" *> T.optional (parseInt) <* T.many1 space >>= (fn size =>
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


   val program : XmlTree.Program.t StreamParser.t =
      let
         fun strip_unique s  = T.parse
            (String.implode <$> T.many(T.failing(T.char #"_") *> T.next),
             Stream.fromList (String.explode s))
         val resolveCon = makeNameResolver(Con.fromString o strip_unique)
         val resolveTycon = makeNameResolver(Tycon.fromString o strip_unique)
         val resolveVar = makeNameResolver(Var.fromString o strip_unique)
      in
         clOptions *>
         (makeProgram <$$$>
            (datatypes resolveCon resolveTycon,
            overflow resolveVar,
            body resolveCon resolveTycon resolveVar))

      end
end


