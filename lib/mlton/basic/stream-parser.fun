
functor StreamParser(S: STREAM_PARSER_STRUCTS):STREAM_PARSER = 
struct

infix 1 <|> >>=
infix 2 <&>
infix  3 <*> <* *> 
infixr 4 <$> <$$> <$$$> <$



open S

type location = {line: int, column: int}
type info = string
(* this is our state representation for readers, but info is unchanging *)
type state = info * (char * location) Stream.t
type 'b t = 
   (info * (char * location) Stream.t) -> ('b * (char * location) Stream.t)


exception Parse of string

fun indexStream({line, column}, s) = 
   case Stream.force s of
      NONE => Stream.empty ()
    | SOME(h, r) =>
         Stream.cons((h, {line=line, column=column}), 
            Stream.delay(fn () =>
               if h = #"\n"
               then 
                  indexStream({line=line+1, column=0}, r)
               else
                  indexStream({line=line, column=column+1}, r)
            )
         )


fun 'b parseWithFile(p : 'b t, f, s) : 'b =
   case p (File.toString f, indexStream({line=1, column=1}, s)) 
   of (b, _) => b

fun 'b parse(p : 'b t, s) : 'b = 
   case p ("String input", indexStream({line=1, column=1}, s)) 
   of (b, _) => b

fun pure a (s : state)  =
  (a, #2 s)

fun tf <*> tx = fn (s : state) => 
   case tf s
      of (f, s') =>
          case tx (#1 s, s')
             of (b, s'') =>
                   (f b, s'')

fun ta >>= f = fn (s : state) =>
   case ta s
      of (a, s') =>
         f a (#1 s, s')
            

fun fst a _ = a
fun snd _ b = b

fun curry f a b = f (a, b)
fun curry3 f a b c = f (a, b, c)

fun f <$> p = (pure f) <*> p
fun f <$$> (p1, p2) = curry <$> (pure f) <*> p1 <*> p2
fun f <$$$> (p1, p2, p3) = curry3 <$> (pure f) <*> p1 <*> p2 <*> p3
fun a <* b = fst <$> a <*> b
fun a *> b = snd <$> a <*> b
fun v <$ p = (fn _ => v) <$> p
fun a <|> b = fn s => (a s) handle Parse _ => (b s)
fun a <&> b = fn s => 
   let
      val _ = (a s) 
   in
      (b s)
   end

fun fail msg (s : state) = case Stream.force (#2 s) 
   of NONE => raise Parse "Parse error at end of file"
    | SOME((_, p : location), _) => raise Parse
         ("Parse error at "^((Int.toString o #line) p)^":"^((Int.toString o #column) p)^" : " ^ msg ^ 
        "\n    Near " ^ implode (List.map(Stream.firstNSafe(#2 s, 20), fn (c, _) => c)))

fun delay p = fn s => p () s

fun next (s : state)  = case Stream.force (#2 s) 
   of NONE => raise Parse "End of file"
    | SOME((h, _), r) => (h, r)

fun sat(t, p) s = 
   let 
      val (h', r) = t s
   in case p h' of true => (h', r)
                 | false => fail "Syntax error" s
   end

fun peek p (s : state) =
   let 
      val (h', _) = p s
   in
      (h', #2 s)
   end


fun failing p s =
   let
      val (succeeded, s') = (true <$ p) s handle
         Parse _ => (false, #2 s)
   in
      if succeeded
         then fail "Suceeded on parser intended to fail" s
         else ((), s')
   end
   
fun notFollowedBy(p, c) =
   fst <$> p <*> (peek (failing c))
   


fun any([]) s = (fail "No valid parse" s)
  | any(p::ps) s = 
       (p s) 
          handle Parse _ => any(ps) s

fun 'b many (t : 'b t) = (op ::) <$$> (t, fn s => many t s) <|> pure []
fun 'b many1 (t : 'b t) = (op ::) <$$> (t, many t)

fun sepBy1(t, sep) = (op ::) <$$> (t, many (sep *> t))
fun sepBy(t, sep) = sepBy1(t, sep) <|> pure []
   
fun optional t = SOME <$> t <|> pure NONE

fun char c s = case Stream.force (#2 s)
   of NONE => raise Parse "End of file"
    | SOME((h, n), r) => 
         if h = c 
            then (h, r)
            else fail ("Expected " ^ 
                        String.fromChar c ^ 
                        "; Got " ^ String.fromChar h) s


fun each([]) = pure []
  | each(p::ps) = (curry (op ::)) <$> p <*> (each ps)

fun string str = (String.implode <$> each (List.map((String.explode str), char))) <|>
                 (fail ("Expected " ^ str))

fun info (s : state) = (#1 s, #2 s)
fun location (s : state) = case Stream.force (#2 s) of
       NONE => raise Parse "End of file"
     | SOME((h, n), r) => (n, #2 s)

fun toReader (p : 'b t) (s : state) : ('b * state) option = 
   let
      val res = p s
   in
      SOME (#1 res, (#1 s, #2 res))
   end 
   handle Parse _ =>
      NONE

fun fromReader (r : state -> ('b * state) option) (s : state) = 
   case r s of 
      SOME (b, s') => 
         (b, #2 s')
    | NONE => raise Parse "fromReader"



end
