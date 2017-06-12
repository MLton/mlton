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
   fun token s = T.notFollowedBy(T.string s, (T.char #"_") <|> (T.sat(T.next, Char.isAlphaNum)))
   
   val clOptions = T.many (T.failing (T.string "Datatypes:") *> T.next)

   fun 'a makeNameResolver(f: string -> 'a): string -> 'a  =
      let
         val hash = String.hash 
         val map = HashSet.new{hash= hash o #1}
         fun eq x (a: string * 'a) = String.equals(x, #1 a)
      in
         fn x => (#2 o HashSet.lookupOrInsert)(map, hash x, eq x, fn () => (x, f x))
      end



   val ident = String.implode <$> (op ::) <$$> 
      (T.any [T.char #"_", T.sat(T.next, Char.isAlpha)], 
       T.many (T.any [T.char #"_", T.char #".", T.sat(T.next, Char.isAlphaNum)]))

   (* parse a tuple of parsers which must begin with a paren but may be unary *)
   fun tupleOf p = Vector.fromList <$> 
      (T.char #"(" *> T.sepBy1(p, T.char #"," *> spaces) <* T.char #")")

   (* too many arguments for the maps, curried to use <*> instead *)
   fun makeTyp resolveTycon (args, ident) =
      let
         fun makeNullary() =
            case ident of
               "bool" => Type.bool
             | "cpointer" => Type.cpointer
             | "intInf" => Type.intInf
             | "thread" => Type.thread
             | "unit" => Type.unit
             | "word8" => Type.unit
             | "word32" => Type.word32
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
         (((tupleOf (T.delay (typ' resolveTycon))) <|> pure (Vector.fromList [])),
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
      {tyvars = Vector.fromList [], 
       tycon = resolveTycon tycon, cons = cons}

   fun datatyp resolveCon resolveTycon = (makeDt resolveTycon) <$$>
      ((spaces *> ident <* spaces <* T.char #"=" <* spaces),
       (Vector.fromList <$> sepBy1
          ((constructor resolveCon resolveTycon) <* spaces, 
           T.char #"|" *> spaces))) 

   fun datatypes resolveCon resolveTycon = 
      T.string "Datatypes:" *> spaces *> Vector.fromList <$> T.many (datatyp resolveCon resolveTycon)

   fun overflow resolveVar = T.string "Overflow:" *> spaces *> (
          (T.string "Some" *> spaces *> SOME <$> resolveVar <$> ident) <|> 
          (NONE <$ T.string "None" <* spaces))


   fun makeProgram(datatypes, overflow, body) = 
         Program.T {body = body,
          datatypes = datatypes,
          overflow = overflow
         }
   
   fun body _ _ _ = pure (Exp.fromPrimExp (PrimExp.Tuple (Vector.fromList []),
   Type.tuple (Vector.fromList [])))

   

   fun exp resolveCon resolveTycon resolveVar = 
      let
         fun makeLet(decs, body) = DE.lett{decs=decs, body=body}
         fun makeValDec(var, ty, exp) = Dec.MonoVal{exp=exp, ty=ty, var=var}
         val var = resolveVar <$> ident
         val constString = String.implode <$> (T.char #"\"" *> 
               (T.many(T.failing (T.char #"\"") *> T.next)) <* T.char #"\"")
         fun possibly t = t >>= (fn x => case x of NONE => T.fail "Syntax error"
                                                 | SOME y => T.pure y)
         val constInt = possibly ((IntInf.fromString o String.implode) <$>
         T.many (T.sat(T.next, Char.isDigit)))

         fun makeWord typ int =
            if Tycon.isWordX typ then T.pure (WordX.fromIntInf(int,
            (Tycon.deWordX typ))) else fail "Invalid word"

         fun constExp typ = 
            if Tycon.isWordX typ then
               Const.Word <$> (constInt >>= makeWord typ) <|> fail "Expected word"
            else if Tycon.isRealX typ then
               fail "Expected real"
            else if Tycon.isIntX typ then
               Const.IntInf <$> constInt <|> fail "Expected integer"
            else
               Const.string <$> constString 
         fun casesOf con left right = T.sepBy1(con <$$> (left <* spaces <*
            T.string "=>" <* spaces, right),
            spaces *> T.char #"|" *> spaces)

         fun exp' () = makeLet <$$> 
            (token "let" *> spaces *> T.many (dec ()) <* token "in" <* spaces, T.delay exp')
         and dec () = T.any
            [makeValDec <$> 
               ((token "val" *> spaces *> var <* spaces) >>= (fn var => 
                  (token ":" *> spaces *> (typ resolveTycon) <* spaces) >>= (fn typ2 =>
                     (token "=" *> spaces *> primexp typ2 <* spaces) >>= (fn primexp => 
                     T.pure(var, typ2, primexp) ))))]
         and primexp typ = T.any 
            [PrimExp.Const <$> constExp (Type.tycon typ)]
      in
         exp' ()
      end


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

functor TestParseSxml (S: PARSE_SXML): sig end =
struct

open S

val _ =
   Assert.assert
   ("TestParseSxml", fn () => 
      false
   )

end

