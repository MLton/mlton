
functor StreamParser(S: STREAM_PARSER_STRUCTS):STREAM_PARSER = 
struct

infix 2 <|>
infix 3 <*> <* *> <$
infixr 4 <$> <$$> <$$$> 



open S

type pos = int
type 'b t = 
   (char * pos) Stream.t -> ('b * (char * pos) Stream.t)

exception Parse of string

fun indexStream(n, s) = 
   case Stream.force s of
      NONE => Stream.empty ()
    | SOME(h, r) =>
         Stream.cons((h, n), 
            Stream.delay(fn () =>
               indexStream(n + 1, r)
               )
            )

fun parse(p, str) =
  case p(indexStream(0, str)) 
   of (b, _) => b

fun pure a s =
  (a, s)

fun tf <*> tx = fn s =>
   case tf s
    of (f, s') =>
          case tx s'
             of (b, s'') =>
                   (f b, s'')


   

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

fun fail msg s = raise Parse (msg ^ "\n\tNear " ^ implode 
  (List.map(Stream.firstNSafe(s, 20), fn (c, _) => c)))

fun next s = case Stream.force s 
   of NONE => raise Parse "Eof"
    | SOME((h, _), r) => (h, r)

fun sat(t, p) s = 
  let 
     val (s', r) = t s
  in case p s' of true => (s', r)
                | false => fail "Syntax error" s
  end

fun any([]) s = (fail "No valid parse" s)
  | any(p::ps) s = 
       (p s) 
          handle Parse _ => any(ps) s

fun many t = fn s =>
   (case t s of
      (b, s') =>
         case many t s' of
            (bs, s'') =>
               (b::bs, s''))
   handle Parse _ =>
      ([], s)

fun optional t = any [SOME <$> t, pure NONE]

fun char c s = case Stream.force s
   of NONE => raise Parse "End of file"
    | SOME((h, n), r) => 
         if h = c 
            then (h, r)
            else fail ("Expected " ^ 
                        String.fromChar c ^ 
                        "; Got " ^ String.fromChar h) s


fun each([]) = pure []
  | each(p::ps) = (curry (op ::)) <$> p <*> (each ps)

fun string str = String.implode <$> each (List.map ((String.explode str), char))

end
