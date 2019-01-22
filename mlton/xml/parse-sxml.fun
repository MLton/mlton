(* Copyright (C) 2019 Matthew Fluet.
 * Copyright (C) 2017 Jason Carr.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ParseSxml (S: PARSE_SXML_STRUCTS): PARSE_SXML =
struct
   open S
   open XmlTree
   structure P = Parse
   open P.Ops
   infix 1 <|> >>=
   infix  3 <*> <* *>
   infixr 4 <$> <$$> <$$$> <$ <$?> 

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
         val hash = String.hash
         val map = HashSet.new{hash= hash o #1}
         fun eq x (a: string * 'a) = String.equals(x, #1 a)
      in
         fn x => (#2 o HashSet.lookupOrInsert)(map, hash x, eq x, fn () => (x, f x))
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

   fun doneRecord' () = P.char #"{" <* P.many(P.delay doneRecord' <|> P.failing(P.char #"}") *> P.next) <* P.char #"}"

   fun casesOf(con, left, right) = Vector.fromList <$> P.sepBy1
      (left <* P.spaces <* token "=>" >>= (fn l =>
         right >>= (fn r => con (l, r))),
       P.spaces)


   (* too many arguments for the maps, curried to use <*> instead *)
   fun makeTyp resolveTycon (args, ident) = Type.con (resolveTycon ident, args)

   local
      fun typ' resolveTycon () = (makeTyp resolveTycon) <$$>
         (((P.spaces *> P.tuple (P.delay (typ' resolveTycon))) <|> P.pure (Vector.new0 ())),
         (P.spaces *> ident <* P.spaces))
   in
      fun typ resolveTycon = typ' resolveTycon ()
   end

   fun makeCon resolveCon (name, arg) = {con = resolveCon name, arg = arg}

   (* parse in a constructor (to Con.t) *)
   fun constructor resolveCon resolveTycon = (makeCon resolveCon) <$$>
      (ident <* P.spaces,
      P.optional (token "of" *> (typ resolveTycon) <* P.spaces) )


   fun makeDt resolveTycon (tycon, cons) =
      {tyvars = Vector.new0 (),
       tycon = resolveTycon tycon, cons = cons}

   fun datatyp resolveCon resolveTycon = (makeDt resolveTycon) <$$>
      ((P.spaces *> ident <* P.spaces <* symbol "="),
       (Vector.fromList <$> P.sepBy1
          ((constructor resolveCon resolveTycon) <* P.spaces,
           P.char #"|" *> P.spaces)))

   fun datatypes resolveCon resolveTycon =
      token "Datatypes:" *> Vector.fromList <$> P.many (datatyp resolveCon resolveTycon)

   fun overflow resolveVar = P.str "Overflow:" *> P.spaces *> (
          (P.str "Some" *> P.spaces *> SOME <$> resolveVar <$> ident <* P.spaces) <|>
          (NONE <$ P.str "None" <* P.spaces))


   fun exp resolveCon resolveTycon resolveVar =
      let
         fun makeLet(decs, result) = Exp.make {decs=decs, result=result}
         fun makeExnDec(ca) = Dec.Exception ca
         fun makeValDec(var, ty, exp) = Dec.MonoVal {exp=exp, ty=ty, var=var}
         fun makeFunDecs(lambdas) = Dec.Fun {decs=Vector.fromList lambdas, tyvars=Vector.new0 ()}
         fun makeFunDec((var, ty), lambda) = {lambda=lambda, ty=ty, var=var}
         val var = resolveVar <$> ident <* P.spaces
         val typedvar = (fn (x,y) => (x,y)) <$$>
            (var,
             symbol ":" *> (typ resolveTycon) <* P.spaces)
         fun makeVarExp var = VarExp.T {var=var, targs = Vector.new0 ()}
         val varExp =
            P.failing (token "in" <|> token "exception" <|> token "val") *>
            makeVarExp <$> var
         fun makeApp(func, arg) = {arg=arg, func=func}
         fun makeConApp(con, targs, arg) = {arg=arg, con=con, targs=targs}
         fun conApp v = makeConApp <$$$>
            (resolveCon <$> ident <* P.spaces,
             P.pure (Vector.new0 ()),
             P.optional v)
         val conAppExp = token "new" *> P.cut (conApp varExp)
         val constExp = Const.parse
         fun makeRaise(NONE, exn) = {exn=exn, extend=false}
           | makeRaise(SOME _, exn) = {exn=exn, extend=true}
         val raiseExp = token "raise" *> P.cut (makeRaise <$$> (P.optional (token "extend"), varExp <* P.spaces))
         fun makeHandle(try, catch, handler) = {catch=catch, handler=handler, try=try}
         fun makeLambda(mayInline, (var, typ), exp) = Lambda.make {arg=var, argType=typ, body=exp, mayInline=mayInline}
         fun makePrimApp(prim, targs, args) = {args=args, prim=prim, targs=targs}
         val primAppExp = token "prim" *> P.cut (makePrimApp <$$$>
            (Prim.parseFull (typ resolveTycon),
             (P.vector (typ resolveTycon) <|> P.pure (Vector.new0 ())) <* P.spaces,
             P.tuple varExp <* P.spaces))
         fun makeSelect(offset, var) = {offset=offset, tuple=var}
         val selectExp = symbol "#" *> P.cut(makeSelect <$$>
            (P.uint <* P.spaces,
             varExp))
         val profileExp = (ProfileExp.Enter <$ token "Enter" <|>
                           ProfileExp.Leave <$ token "Leave") <*>
                           P.cut ((SourceInfo.fromC o String.implode) <$>
                              P.manyCharsFailing(P.char #"\n") <* P.char #"\n" <* P.spaces)
         fun makeConCases var (cons, def) =
            {test=var,
             cases=Cases.Con cons,
             default=Option.map(def, fn x => (x, Region.bogus))}
         fun makeWordCases var s (wds, def) =
            {test=var,
             cases=Cases.Word (WordSize.fromBits (Bits.fromInt s), wds),
             default=Option.map(def, fn x => (x, Region.bogus))}
         fun makePat(con, exp) = P.pure (Pat.T con, exp)
         fun makeCaseWord(w, exp) = P.pure (w, exp)

         fun exp' () = makeLet <$$>
            (token "let" *>
             P.many (dec ()) <* token "in", varExp <* P.str "end")
         and dec () = P.any
            [token "exception" *>
             P.cut (makeExnDec <$> (constructor resolveCon resolveTycon)),
             token "val" *> token "rec" *>
             P.cut (makeFunDecs <$>
                    P.many (makeFunDec <$$> (typedvar <* symbol "=" <* P.spaces, lambdaExp ()))),
             token "val" *>
             P.cut (makeValDec <$>
                    (typedvar >>= (fn (var, ty) =>
                     (symbol "=" *> primexp () <* P.spaces) >>= (fn primexp =>
                      P.pure (var, ty, primexp)))))]
         and primexp () = P.any
            [PrimExp.Case <$> casesExp (),
             PrimExp.ConApp <$> conAppExp,
             PrimExp.Lambda <$> lambdaExp (),
             (* const must come before select due to vector constants *)
             PrimExp.Const <$> constExp,
             PrimExp.Handle <$> handleExp (),
             PrimExp.PrimApp <$> primAppExp,
             PrimExp.Profile <$> profileExp,
             PrimExp.Raise <$> raiseExp,
             PrimExp.Select <$> selectExp,
             PrimExp.Tuple <$> (P.tuple varExp) <* P.spaces,
             (* put these last, they just take identifiers so they're pretty greedy *)
             (* App *must* procede var, due to greediness *)
             PrimExp.App <$> makeApp <$$> (varExp, varExp),
             PrimExp.Var <$> varExp]
         and handleExp () = makeHandle <$$$>
            (P.delay exp' <* P.spaces,
             token "handle" *> P.cut typedvar,
             P.cut (token "=>" *> P.delay exp' <* P.spaces)
             )
         and lambdaExp () = token "fn" *> P.cut(makeLambda <$$$>
            (false <$ (token "noinline") <|> P.pure true,
             typedvar,
             token "=>" *> P.delay exp' <* P.spaces))
         and casesExp () = P.str "case" *>
            P.optional P.uint <* P.many1 P.space >>= (fn size => P.cut(
               varExp <* token "of" <* P.spaces >>= (fn var =>
                  case size of
                      NONE => makeConCases var <$$>
                        (casesOf(makePat, conApp typedvar, P.delay exp'),
                         P.spaces *> P.optional(token "_" *> token "=>" *> P.delay exp'))
                    | SOME s => makeWordCases var s <$$>
                        (casesOf(makeCaseWord, WordX.parse, P.delay exp'),
                         P.spaces *> P.optional(token "_" *> token "=>" *> P.delay exp'))
                      )))
      in
         exp' ()
      end

      fun body resolveCon resolveTycon resolveVar = P.str "Body:" *> P.spaces
         *> exp resolveCon resolveTycon resolveVar
         (*pure (Exp.fromPrimExp (PrimExp.Tuple (Vector.new0 ()),
   Type.tuple (Vector.new0 ())))*)

   fun makeProgram(datatypes, overflow, body) =
      Program.T
         {body = body,
          datatypes = datatypes,
          overflow = overflow}


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
            case List.peekMap (Tycon.prims, fn {name, tycon, ...} =>
                               if ident = name then SOME tycon else NONE) of
               SOME con => con
             | NONE => if ident = "unit"
                          then Tycon.tuple
                       else resolveTycon0 ident
         val resolveVar = makeNameResolver(Var.newString o strip_unique)
      in
         P.compose(skipComments (),
         clOptions *>
         (makeProgram <$$$>
            (datatypes resolveCon resolveTycon,
            overflow resolveVar,
            body resolveCon resolveTycon resolveVar <* P.spaces <* (P.failing P.next <|> P.failCut "End of file"))))
            (* failing next to check for end of file *)
      end

end
