functor ParseSxml(S: PARSE_SXML_STRUCTS) = 
struct
   open S
   structure T = StreamParser
   open T
   open XmlTree
   structure DE = DirectExp
   infix 1 <|> >>=
   infix 2 <&>
   infix  3 <*> <* *>
   infixr 4 <$> <$$> <$$$> <$


   val space = T.sat(T.next, Char.isSpace) 
   val spaces = T.many(space)
   fun token s = T.notFollowedBy
      (T.string s,
       (T.char #"_") <|> (T.sat(T.next,Char.isAlphaNum))) *> spaces
   
   val clOptions = T.many (T.failing (T.string "Datatypes:") *> T.next)

   fun 'a makeNameResolver(f: string -> 'a): string -> 'a  =
      let
         val hash = String.hash 
         val map = HashSet.new{hash= hash o #1}
         fun eq x (a: string * 'a) = String.equals(x, #1 a)
      in
         fn x => (#2 o HashSet.lookupOrInsert)(map, hash x, eq x, fn () => (x, f x))
      end

   fun isInfixChar b = case List.index
      (String.explode "!%&$#+-/:<=>?@\\~'^|*",
       fn c => b = c) of
          SOME _ => true
        | NONE   => false

   fun isIdentFirst b = Char.isAlpha b orelse b = #"'" 
   fun isIdentRest b = Char.isAlphaNum b orelse b = #"'" orelse b = #"_" orelse b = #"."
      

   val ident = T.failing (token "in" <|> token "val" <|> token "prim") *>
      String.implode <$> (T.any
         [(op ::) <$$>
             (T.sat(T.next, isIdentFirst),
              T.many (T.sat(T.next, isIdentRest))),
          List.append <$$>
             (T.many1 (T.sat(T.next, isInfixChar)),
              T.many (T.sat(T.next, isIdentRest)) (* just for collecting _0 *)
              )])

       

   (* parse a tuple of parsers which must begin with a paren but may be unary *)
   fun tupleOf p = Vector.fromList <$>
      (T.char #"(" *> T.sepBy1(p, T.char #"," *> spaces) <* T.char #")")

   fun vectorOf p = Vector.fromList <$>
      (T.char #"[" *> T.sepBy1(p, T.char #"," *> spaces) <* T.char #"]")

   fun casesOf con left right = T.sepBy1
      (con <$$> (left <* spaces <* T.string "=>" <* spaces, right),
       spaces *> T.char #"|" *> spaces)


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
         (((tupleOf (T.delay (typ' resolveTycon))) <|> pure (Vector.new0 ())),
         (spaces *> ident <* spaces))
   in
      fun typ resolveTycon = typ' resolveTycon () 
   end

   
   fun makeCon resolveCon (name, arg) = {con = resolveCon name, arg = arg}

   (* parse in a constructor (to Con.t) *) 
   fun constructor resolveCon resolveTycon = (makeCon resolveCon) <$$> 
      (ident, 
      T.optional (T.many1 space *> T.string "of" *> T.many1 space *> (typ resolveTycon))) <* spaces

   
   fun makeDt resolveTycon (tycon, cons) = 
      {tyvars = Vector.new0 (), 
       tycon = resolveTycon tycon, cons = cons}

   fun datatyp resolveCon resolveTycon = (makeDt resolveTycon) <$$>
      ((spaces *> ident <* spaces <* T.char #"=" <* spaces),
       (Vector.fromList <$> sepBy1
          ((constructor resolveCon resolveTycon) <* spaces, 
           T.char #"|" *> spaces))) 

   fun datatypes resolveCon resolveTycon = 
      T.string "Datatypes:" *> spaces *> Vector.fromList <$> T.many (datatyp resolveCon resolveTycon)

   fun overflow resolveVar = T.string "Overflow:" *> spaces *> (
          (T.string "Some" *> spaces *> SOME <$> resolveVar <$> ident <* spaces) <|>
          (NONE <$ T.string "None" <* spaces))


   fun exp resolveCon resolveTycon resolveVar = 
      let
         fun makeLet(decs, result) = Exp.make {decs=decs, result=result}
         fun makeValDec(var, ty, exp) = Dec.MonoVal{exp=exp, ty=ty, var=var}
         val var = resolveVar <$> ident <* spaces
         val typedvar = (fn (x,y) => (x,y)) <$$>
            (resolveVar <$> ident <* spaces,
             token ":" *> spaces *> (typ resolveTycon) <* spaces)
         fun makeVarExp var = VarExp.T {var=var, targs = Vector.new0 ()}
         val varExp = makeVarExp <$> (var <* spaces)
       
         fun makeApp(func, arg) = {arg=arg, func=func}
         val appExp = makeApp <$$> (varExp, varExp)

         fun possibly t = t >>= (fn x => case x of NONE => T.fail "Syntax error"
                                                 | SOME y => T.pure y)

         val stringToken = (fn (x, y) => [x, y]) <$$> (T.char #"\\", T.next) <|>
                           (fn x      => [x]   ) <$> T.next
         val constString = possibly ((String.fromString o String.implode o List.concat) <$>
         (T.char #"\"" *> (T.many(T.failing (T.char #"\"") *> stringToken)) <* T.char #"\""))
         val constInt = possibly ((IntInf.fromString o String.implode) <$>
         T.many (T.sat(T.next, Char.isDigit)))

         fun makeWord typ int =
            if Tycon.isWordX typ
               then T.pure (WordX.fromIntInf(int, (Tycon.deWordX typ)))
               else fail "Invalid word"

         fun makeConApp(con, targs, arg) = {arg=arg, con=con, targs=targs}
         val conAppExp = makeConApp <$$$>
            (token "new" *> resolveCon <$> ident <* spaces,
             pure (Vector.new0 ()),
             T.optional varExp)

         fun constExp typ = 
            if Tycon.isWordX typ then
               Const.Word <$> (constInt >>= makeWord typ) <|> T.fail "Expected word"
            else if Tycon.isRealX typ then
               T.fail "Expected real"
            else if Tycon.isIntX typ then
               Const.IntInf <$> constInt <|> T.fail "Expected integer"
            else
               Const.string <$> constString

         fun makeRaise(NONE, exn) = {exn=exn, extend=false}
           | makeRaise(SOME _, exn) = {exn=exn, extend=true}
         val raiseExp = makeRaise <$$> (token "raise" *> T.optional (token "extend"), varExp <* spaces)

         fun makeHandle(try, catch, handler) = {catch=catch, handler=handler, try=try}

         fun resolvePrim p = case XmlTree.Prim.fromString p
            of SOME p' => T.pure p'
             | NONE => T.fail ("Invalid primitive application: " ^ p)
         fun makePrimApp(prim, targs, args) = {args=args, prim=prim, targs=targs}
         val primAppExp = makePrimApp <$$$>
            (token "prim" *> ident <* spaces >>= resolvePrim,
             (vectorOf (typ resolveTycon) <|> pure (Vector.new0 ())) <* spaces,
             tupleOf varExp <* spaces)

         fun exp' () = makeLet <$$> 
            (token "let" *>
            (*(fn x=> [x]) <$> dec () <* token "in", varExp)*)
            T.many1 (dec ()) <* token "in", varExp)
         and dec () = makeValDec <$>
               ((token "val" *> typedvar <* spaces) >>= (fn var =>
                (token "="  *> primexp (#2 var) <* spaces) >>= (fn primexp =>
                     T.pure(#1 var, #2 var, primexp))))
         and primexp typ = T.failing(token "in") *> T.any
            [PrimExp.ConApp <$> conAppExp,
             PrimExp.Const <$> constExp (Type.tycon typ),
             PrimExp.PrimApp <$> primAppExp,
             PrimExp.Raise <$> raiseExp,
             PrimExp.Tuple <$> (tupleOf varExp),
             PrimExp.Handle <$> handleExp (),
             (* put these last, they just take identifiers so they're pretty greedy *)
             PrimExp.Var <$> varExp,
             PrimExp.App <$> makeApp <$$> (varExp, varExp)]
         and handleExp () = makeHandle <$$$>
            (delay exp' <* spaces,
             token "handle" *> typedvar <* spaces,
             token "=>" *> delay exp' <* spaces
             )

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


