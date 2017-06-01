functor StreamTransformer(S: STREAM_TRANSFORMER_STRUCTS):STREAM_TRANSFORMER = 
struct

open S

type pos = int
type ('a, 'b) t = 
   ('a * pos) Stream.t -> ('b * ('a * pos) Stream.t)

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

infix 3 <*> <* *>
fun op <*> (tf, tx) = fn s => 
   case tf s
    of (f, s') =>
          case tx s'
             of (b, s'') =>
                   (f b, s'')
fun fst a _ = a
fun snd _ b = b


infixr 4 <$>
(*fun <$> f p = raise Parse "Undefined"*)
fun op <$> (f, p) = (pure f) <*> p 
fun op <* (a, b) = fst <$> a <*> b
fun op *> (a, b) = snd <$> a <*> b


fun one s = case Stream.force s 
   of NONE => raise Parse "Eof"
    | SOME((h, _), r) => (h, r)

fun any([]) = (fn _ => raise Parse "No parse")
  | any(p::ps) = fn s =>
       (p s) 
          handle Parse msg => any(ps) s

fun many t = fn s =>
   case t s of
      (b, s') =>
         case many t s' of
            (bs, s'') =>
               (b::bs, s'')
   handle Parse _ =>
      ([], s)

fun optional t = any [SOME <$> t, pure NONE]

fun equals c s = case Stream.force s
   of NONE => raise Parse "Eof"
    | SOME((h, n), r) => 
         if h = c 
            then (h, r)
            else raise Parse ("Syntax error")

fun curry f a b = f (a, b)

fun each([]) = pure []
  | each(p::ps) = (curry (op ::)) <$> p <*> (each ps)


fun string str = implode <$> each (List.map ((explode str), equals))

end

