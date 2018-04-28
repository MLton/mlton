(* Copyright (C) 2017 Jason Carr.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Parse :> PARSE =
struct

infix 1 <|> >>=
infix  3 <*> <* *>
infixr 4 <$> <$$> <$$$> <$ <$?> 

structure Location =
   struct
      type t = {line: int, column: int}
   end
structure State =
   struct
      (* this is our state representation for readers *)
      type t = (char * Location.t) Stream.t
   end


datatype 'a result = Success of 'a * (char * Location.t) Stream.t
                   | Failure of string list (* expected options *)
                   | FailCut of string list (* as failure, but the
                   closest upstream choice point won't try other options, and
                   their errors will be silenced *)
type 'a t =
   State.t -> 'a result


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

fun doFail([]) = Result.No ("Parse error")
  | doFail([msg]) = Result.No ("Parse error: Expected " ^ msg)
  | doFail(msgs) = Result.No ("Parse error: Expected one of \n" ^
       (String.concat(List.map(msgs, fn x => x ^ "\n"))))

fun parseStream(p : 'a t, stream) : 'a Result.t =
   case p (indexStream({line=1, column=1}, stream))
   of Success (b, _) => Result.Yes b
    | Failure ms => doFail ms
    | FailCut ms => doFail ms
fun parseString(p : 'a t, string) : 'a Result.t =
   parseStream(p, Stream.fromList (String.explode string))
fun parseFile(p : 'a t, file) : 'a Result.t =
   File.withIn
   (file, fn i =>
    let
       fun toStream () =
          case In.inputChar i of
             SOME c => Stream.cons (c, Stream.delay toStream)
           | NONE => Stream.empty ()
    in
       parseStream(p, toStream ())
    end)



fun tf <*> tx = fn (s : State.t) =>
   case tf s
      of Success (f, s') =>
          (case tx s'
             of Success (b, s'') =>
                   Success (f b, s'')
              (* constructors have to be explict to typecheck *)
              | Failure err => Failure err
              | FailCut err => FailCut err)
       | Failure err => Failure err
       | FailCut err => FailCut err

fun ta >>= f = fn (s : State.t) =>
   case ta s
      of Success (a, s') =>
         f a s'
       | Failure err => Failure err
       | FailCut err => FailCut err


fun fst a _ = a
fun snd _ b = b

fun curry f a b = f (a, b)
fun curry3 f a b c = f (a, b, c)

fun pure a (s : State.t)  =
  Success (a, s)

fun f <$> p = (pure f) <*> p
fun f <$$> (p1, p2) = curry <$> (pure f) <*> p1 <*> p2
fun f <$$$> (p1, p2, p3) = curry3 <$> (pure f) <*> p1 <*> p2 <*> p3
fun f <$?> p = p >>= (fn a => case f a of SOME b => pure b
                                        | NONE => fn _ => Failure [])
fun a <* b = fst <$> a <*> b
fun a *> b = snd <$> a <*> b
fun v <$ p = (fn _ => v) <$> p
fun a <|> b = fn s => case (a s)
   of Success r => Success r
    | Failure err1 => (case (b s) of
        Success r => Success r
      | Failure err2 => Failure (List.append(err1, err2))
      | FailCut err2 => Failure (err2))
    | FailCut err1 => Failure err1

structure Ops = struct
   val (op >>=) = (op >>=)
   val (op <*>) = (op <*>)
   val (op <$>) = (op <$>)
   val (op <$?>) = (op <$?>)
   val (op <$$>) = (op <$$>)
   val (op <$$$>) = (op <$$$>)
   val (op <*) = (op <*)
   val (op *>) = (op *>)
   val (op <$) = (op <$)
   val (op <|>) = (op <|>)
end



fun failString (m, p : Location.t, s : (char * Location.t) Stream.t) =
   (m ^ " at " ^
      (Int.toString (#line p)) ^ ":" ^ (Int.toString (#column p)) ^
      "\n     Near: " ^ (String.implode (List.map(Stream.firstNSafe(s, 20), #1))))

fun fail m (s : State.t) = case Stream.force (s)
   of NONE => Failure []
    | SOME((_, p : Location.t), _) => Failure [failString (m, p, s)]

fun failCut m (s : State.t) = case Stream.force (s)
   of NONE => FailCut []
    | SOME((_, p : Location.t), _) => FailCut [failString (m, p, s)]

fun cut p s = case p s
   of Success x => Success x
    | Failure m => FailCut m
    | FailCut m => FailCut m

fun uncut p s = case p s of
    Success x => Success x
  | Failure m => Failure m
  | FailCut m => Failure m

fun delay p = fn s => p () s

fun next (s : State.t)  = case Stream.force (s)
   of NONE => Failure ["Any character at end of file"]
    | SOME((h, _), r) => Success (h, r)

fun satExpects(t, p, m) s =
   case t s of
       Success (a, s') =>
         (if p a then Success (a, s') else fail m s)
     | Failure err => Failure err
     | FailCut err => FailCut err

fun sat(t, p) s = satExpects(t, p, "Satisfying") s
fun nextSat p s = case Stream.force s
   of NONE => Failure ["Any character at end of file"]
    | SOME((h, _), r) => (case p h of
         false => Failure ["Satisfying character"]
       | true => Success (h, r))

fun peek p (s : State.t) =
   case p s of Success (h', _) => Success (h', s)
             | err => err

fun failing p s =
   case p s
      of Success _ => fail "failure" s
       | _ => Success ((), s)

fun notFollowedBy(p, c) =
   p <* failing c

fun any'([]) s = Failure []
  | any'(p::ps) s =
       case p s of
          Success (a, s) => Success (a, s)
        | Failure m => (case any'(ps) s
           of Failure m2 => Failure (List.append(m, m2))
            | succ => succ)
        | FailCut m => FailCut m
fun 'a any ps = uncut (any' ps)


fun 'a many' (t : 'a t) s = case ((op ::) <$$> (t, fn s' => many' t s')) s of
    Success x => Success x
  | Failure y => pure [] s
  | FailCut z => FailCut z
fun 'a many t = uncut (many' t)
fun 'a many1 (t : 'a t) = uncut ((op ::) <$$> (t, many' t))

fun manyFailing(p, f) = many (failing f *> p)
fun manyCharsFailing f = many (failing f *> next)

fun sepBy1(t, sep) = uncut ((op ::) <$$> (t, many' (sep *> t)))
fun sepBy(t, sep) = uncut ((op ::) <$$> (t, many' (sep *> t)) <|> pure [])

fun optional t = SOME <$> t <|> pure NONE

fun char c s = case Stream.force (s)
   of NONE => Failure [String.fromChar c ^ " at end of file"]
    | SOME((h, _), r) =>
         if h = c
            then Success (h, r)
            else fail (String.fromChar c) s


fun each([]) = pure []
  | each(p::ps) = (curry (op ::)) <$> p <*> (each ps)


fun matchList s1 l2 = case (Stream.force s1, l2)
   of (_, []) => Success ((), s1)
    | (NONE, (_::_)) => Failure []
    | (SOME ((h, _), r), (x :: xs)) => if h = x then matchList r xs else Failure []
fun str str s = case matchList (s) (String.explode str)
   of Success ((), r) => Success (str, r)
    | _ => fail str s

fun location (s : State.t) = case Stream.force s of
       NONE => Failure ["any character end of file location"]
     | SOME((h, n), r) => Success (n, s)

fun toReader (p : 'a t) (s : State.t) : ('a * State.t) option =
   case p s of
      Success (a, s') => SOME (a, s')
    | _ => NONE

fun fromReader (r : State.t -> ('a * State.t) option) (s : State.t) =
   case r s of
      SOME (b, s') =>
         Success (b, s')
    | NONE => fail "fromReader" s

fun compose (p1 : char list t, p2 : 'a t) (s : State.t) =
   let
      (* easiest way to escape here *)
      exception ComposeFail of string list
      fun makeStr s' () = case Stream.force s' of
         NONE => Stream.empty ()
       | SOME ((_, pos), r) =>
            (case p1 s' of
                Success (b, r) => (case b of
                    (* equivalent, but avoids the jumping from append of fromList *)
                    c::[] => Stream.cons((c, pos), Stream.delay (makeStr r))
                  | _  => Stream.append
                        (indexStream(pos, Stream.fromList b),
                         Stream.delay (makeStr r)))
              | Failure m => raise ComposeFail m
              | FailCut m => raise ComposeFail m)
   in
      p2 (makeStr (s) () ) handle ComposeFail m => Failure m end


val digits = many (nextSat (fn c => Char.isDigit c orelse c = #"~"))

val int = (fromReader (Int.scan (StringCvt.DEC, (toReader next)))) <|> failCut "integer"
val intInf = (fromReader (IntInf.scan (StringCvt.DEC, (toReader next)))) <|> failCut "integer"

val uint = (fromReader (Int.scan (StringCvt.DEC, (toReader (nextSat Char.isDigit))))) <|> failCut "unsigned integer"
val uintInf = (fromReader (IntInf.scan (StringCvt.DEC, (toReader (nextSat Char.isDigit))))) <|> failCut "unsigned integer"

val space = nextSat Char.isSpace

val spaces = many space

fun tuple p = Vector.fromList <$>
   (char #"(" *> sepBy(spaces *> p, char #",") <* char #")")

fun vector p = Vector.fromList <$>
   (char #"[" *> sepBy(spaces *> p, char #",") <* char #"]")


end
