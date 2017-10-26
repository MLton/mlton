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
   infixr 4 <$> <$$> <$$$> <$
   
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

   (* too many arguments for the maps, curried to use <*> instead *)
   fun makeTyp resolveTycon (args, ident) = 
         case ident of
             "bool" => Type.bool
            | "tuple" => Type.tuple args
            | "array" => Type.array (Vector.first args) 
            | "vector" => Type.vector (Vector.first args)
            | "ref" => Type.reff (Vector.first args)
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
   
   fun makeStatement resolveVar (var, ty, exp) = 
      (print("\n\nGLOBAL:\n");
       print(var);
       print("\n");
       print(ty);
       print("\n");
       print(exp);
      Statement.T
      {var = NONE,
       ty = Type.unit,
       exp = Exp.Var (resolveVar exp)})

   fun glbl resolveVar = (makeStatement resolveVar) <$$$>
      ((spaces *> ident <* T.char #":"),
        (spaces *> ident <* spaces <* symbol "="),
        (spaces *> ident <* spaces))

   fun globls resolveVar = token "Globals:" *> Vector.fromList <$> T.many (glbl resolveVar)

   fun makeProgram (datatypes, globals) =
      Program.T
         {datatypes = datatypes,
          functions = [],
          globals = globals,
          main = Func.newNoname() } 
   
   val program : Program.t StreamParser.t=
      let
         fun strip_unique s  = T.parse
            (String.implode <$> T.manyCharsFailing(
               T.char #"_" *> T.many1 (T.sat(T.next, Char.isDigit)) *> T.failing T.next),
             Stream.fromList (String.explode s))
         val resolveCon0 = makeNameResolver(Con.fromString)
         fun resolveCon ident =
            case List.peek ([Con.falsee, Con.truee, Con.overflow, Con.reff], fn con =>
                            ident = Con.toString con) of
               SOME con => con
             | NONE => resolveCon0 ident
         fun resolveTycon ident = makeNameResolver(Tycon.fromString) ident
         val resolveVar = makeNameResolver(Var.fromString)
      in
         T.compose(skipComments (),
            clOptions *>
            (makeProgram <$$> (datatypes resolveCon resolveTycon, globls
            resolveVar)))
      end
   
   fun parse s = T.parse(program, s)
      
end
