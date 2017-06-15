
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
datatype 'b result = Success of 'b * (char * location) Stream.t
                   | Failure of (string * location) list (* expected values *)
                   | FailCut of (string * location) list (* as failure, but the
                   closest upstream choice point won't try other options, and
                   their errors will be silenced *)
type state = info * (char * location) Stream.t
type 'b t = 
   state -> 'b result


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
   of Success (b, _) => b

fun 'b parse(p : 'b t, s) : 'b = 
   case p ("String input", indexStream({line=1, column=1}, s)) 
   of Success (b, _) => b

fun pure a (s : state)  =
  Success (a, #2 s)

fun tf <*> tx = fn (s : state) => 
   case tf s
      of Success (f, s') =>
          (case tx (#1 s, s')
             of Success (b, s'') =>
                   Success (f b, s'')
              (* constructors have to be explict to typecheck *)
              | FailCut err => FailCut err
              | Failure err => Failure err)
       | Failure err => Failure err
       | FailCut err => FailCut err

fun ta >>= f = fn (s : state) =>
   case ta s
      of Success (a, s') =>
         f a (#1 s, s')
       | Failure err => Failure err
       | FailCut err => FailCut err
            

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
fun a <|> b = fn s => case (a s) 
   of Success r => Success r
    | Failure err1 => (case (b s) of
        Success r => Success r
      | Failure err2 => Failure (List.append(err1, err2))
      | FailCut err2 => Failure (List.append(err1, err2)))
    | FailCut err1 => Failure err1
fun a <&> b = fn s => case (a s) 
   of Success r => (case (b s) of
        Success r' => Success r'
      | Failure err => Failure err
      | FailCut err => Failure err)
    | Failure err => Failure err
    | FailCut err => Failure err


structure Ops = struct
   val (op >>=) = (op >>=)
   val (op <*>) = (op <*>)
   val (op <$>) = (op <$>)
   val (op <$$>) = (op <$$>)
   val (op <$$$>) = (op <$$$>)
   val (op <*) = (op <*)
   val (op *>) = (op *>)
   val (op <$) = (op <$)
   val (op <|>) = (op <|>)
   val (op <&>) = (op <&>)
end



fun fail m (s : state) = case Stream.force (#2 s) 
   of NONE => Failure []
    | SOME((_, p : location), _) => Failure [(m, p)]

fun failCut m (s : state) = case Stream.force (#2 s) 
   of NONE => FailCut []
    | SOME((_, p : location), _) => FailCut [(m, p)]


fun delay p = fn s => p () s

fun next (s : state)  = case Stream.force (#2 s) 
   of NONE => Failure []
    | SOME((h, _), r) => Success (h, r)

fun satExpects(t, p, m) s =
   case t s of
       Success (a, s') =>
         (if p a then Success (a, s') else fail m s)
     | Failure err => Failure err
     | FailCut err => FailCut err

fun sat(t, p) s = satExpects(t, p, "Syntax error") s


fun peek p (s : state) =
   case p s of Success (h', _) => Success (h', #2 s)
             | err => err

fun failing p s =
   case p s
      of Success _ => fail "failure" s
       | _ => Success ((), #2 s)
      
fun notFollowedBy(p, c) =
   fst <$> p <*> (peek (failing c))
   


fun any([]) s = Failure []
  | any(p::ps) s =
       case p s of
          Success (a, s) => Success (a, s)
        | Failure m => case any(ps) s
           of Failure m2 => Failure (List.append(m, m2))
            | succ => succ
        | FailCut m => Failure m

fun 'b many (t : 'b t) = (op ::) <$$> (t, fn s => many t s) <|> pure []
fun 'b many1 (t : 'b t) = (op ::) <$$> (t, many t)

fun sepBy1(t, sep) = (op ::) <$$> (t, many (sep *> t))
fun sepBy(t, sep) = sepBy1(t, sep) <|> pure []
   
fun optional t = SOME <$> t <|> pure NONE

fun char c s = case Stream.force (#2 s)
   of NONE => Failure []
    | SOME((h, n), r) => 
         if h = c 
            then Success (h, r)
            else fail (String.fromChar c) s


fun each([]) = pure []
  | each(p::ps) = (curry (op ::)) <$> p <*> (each ps)

fun string str = (String.implode <$> each (List.map((String.explode str), char))) <|>
                 (fail str)

fun info (s : state) = Success (#1 s, #2 s)
fun location (s : state) = case Stream.force (#2 s) of
       NONE => Failure []
     | SOME((h, n), r) => Success (n, #2 s)

fun toReader (p : 'b t) (s : state) : ('b * state) option = 
   case p s of
      Success (a, s') => SOME (a, (#1 s, s'))
    | _ => NONE

fun fromReader (r : state -> ('b * state) option) (s : state) = 
   case r s of 
      SOME (b, s') => 
         Success (b, #2 s')
    | NONE => fail "fromReader" s



end
