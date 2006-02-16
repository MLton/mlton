(*
 * Translated by Stephen Weeks (sweeks@sweeks.com) 2000-10-11 from the
 * PLClub OCaml winning entry to the 2000 ICFP programming contest.
 *)

(* raytrace.sml *)
signature CAML =
   sig
      type float = real
      type int = int
   end

structure Caml =
struct

type int = int
type float = real

val op div = Int.div

exception Not_found
exception Failure of string

fun failwith s = raise(Failure s)
   
structure Char =
   struct
      open Char   
      val code = ord
      val chr = chr
      val unsafe_chr = chr
      val lowercase = toLower
      val uppercase = toUpper
   end

local
   open TextIO
in
   type out_channel = outstream
   val open_out = openOut
   val open_out_bin = open_out
   fun output_string (out, s) = output(out, s)
   val close_out = closeOut
end

type float = real

structure Array =
   struct
      local open Array
      in
         val array = array
         val copy = copy
         val of_list = fromList
         val length = length
         val sub = sub
         val update = update
         val unsafe_get = Array.sub
         val unsafe_set = Array.update
         val make = array
         fun map f a = Array.tabulate(length a, fn i => f(Array.sub(a, i)))
         val init = tabulate
      end
   end

fun for(a: int, b, f) =
   let
      fun loop a =
         if a > b
            then ()
         else (f a; loop(a + 1))
   in loop a
   end

fun forDown(b: int, a, f) =
   let
      fun loop b =
         if b < a
            then ()
         else (f b; loop(b - 1))
   in loop b
   end

local
   open Real
   open Math
in
   val abs_float = abs
   val acos = acos
   val asin = asin
   val cos = cos
   val float = fromInt
   val float_of_int = float
   val sin = sin
   val sqrt = sqrt
   val tan = tan
   val truncate = trunc
   val ** = Math.pow
   infix 8 ** 
end

(* A hack for hash tables with string domain where performance doesn't matter. *)
structure Hashtbl:
   sig
      type ('a, 'b) t

      val add: ('a, 'b) t -> string -> 'b -> unit
      val create: int -> ('a, 'b) t
      val find: ('a, 'b) t -> string -> 'b
   end =
   struct
      datatype ('a, 'b) t = T of (string * 'b) list ref
            
      fun create _ = T (ref [])

      fun add (T t) k d = t := (k, d) :: !t

      fun find (T (ref t)) k =
         case List.find (fn (k', _) => k = k') t of
            NONE => raise Not_found
          | SOME(_, d) => d
   end

structure List =
   struct
      local open List
      in
         val iter = app
         val map = map
         val filter = filter
         val nth = nth
         val rev = rev
      end
   end

fun exit i = Posix.Process.exit(Word8.fromInt i)
   
end
structure Math =
struct

open Caml
   
val epsilon = 1E~5

val dtr = acos (~1.0) / 180.0
val rtd = 180.0 / acos (~1.0)

fun dcos t = cos (t * dtr)
fun dsin t = sin (t * dtr)
fun dtan t = tan (t * dtr)
fun dacos x = rtd * acos x

val infinity = Real.posInf
val minus_infinity = Real.negInf

fun max_float (x, y : float) = if x >= y then x else y

end
signature MATRIX =
   sig
      include CAML

      (**** Matrix arithmetic ****)

      type t = float array (* 4-dimension matrix *)
      type v = float * float * float * float (* 4-dimension vector *)

      (* Basic matrices *)
      val identity : t
      val translate : (*x:*)float * (*y:*)float * (*z:*)float -> t
      val scale : (*x:*)float * (*y:*)float * (*z:*)float -> t
      val uscale : float -> t
      val unscale : (*x:*)float * (*y:*)float * (*z:*)float -> t
      val unuscale : float -> t
      val rotatex : float -> t
      val rotatey : float -> t
      val rotatez : float -> t

      (* Operations on matrices *)
      val mul : t * t -> t
      val vmul : t * v -> v
      val transpose : t -> t

      val add_scaled : v * float * v -> v
      val add : v * v -> v
      val sub : v * v -> v
      val prod : v * v -> float
      val square : v -> float
      val normalize : v -> v
      val neg : v -> v
   end
structure Matrix: MATRIX =
struct

open Caml
open Math

type t = float array
type v = float * float * float * float

(**** Basic matrices ****)

val identity =
   Array.of_list[1.0, 0.0, 0.0, 0.0,
                  0.0, 1.0, 0.0, 0.0,
                  0.0, 0.0, 1.0, 0.0,
                  0.0, 0.0, 0.0, 1.0]

fun translate(x, y, z) =
   Array.of_list[1.0, 0.0, 0.0, ~ x,
                  0.0, 1.0, 0.0, ~ y,
                  0.0, 0.0, 1.0, ~ z,
                  0.0, 0.0, 0.0, 1.0]

fun unscale(x, y, z) =
   Array.of_list[ x,  0.0, 0.0, 0.0,
                  0.0, y,  0.0, 0.0,
                  0.0, 0.0, z,  0.0,
                  0.0, 0.0, 0.0, 1.0]
  
fun scale(x, y, z) = unscale (1.0 / x, 1.0 / y, 1.0 / z)

fun unuscale s = unscale (s, s, s)

fun uscale s = scale (s, s, s)

fun rotatex t =
  let
     val co = dcos t
     val si = dsin t
  in
     Array.of_list[ 1.0,   0.0,  0.0, 0.0,
                    0.0,   co,  si, 0.0,
                    0.0, ~ si, co, 0.0,
                    0.0,   0.0,  0.0, 1.0 ]
  end

fun rotatey t =
  let
     val co = dcos t
     val si = dsin t
  in
     Array.of_list[ co, 0.0, ~ si, 0.0,
                    0.0, 1.0,   0.0,  0.0,
                    si, 0.0,   co,  0.0,
                    0.0, 0.0,   0.0,  1.0 ]
  end

fun rotatez t =
  let
     val co = dcos t
     val si = dsin t
  in
     Array.of_list[   co,  si, 0.0, 0.0,
                    ~ si, co, 0.0, 0.0,
                    0.0,  0.0, 1.0, 0.0,
                    0.0,  0.0, 0.0, 1.0 ]
  end

(*** Operations on matrices ***)

fun get (m : t, i, j) = Array.unsafe_get (m, i * 4 + j)
fun set (m : t, i, j, v) = Array.unsafe_set (m, i * 4 + j, v)

fun mul (m, m') =
  let
     val m'' = Array.make (16, 0.0)
  in
     for(0, 3, fn i =>
         for(0, 3, fn j => let
            fun lp (4, s) = s
              | lp (k, s) = lp (k+1, s + get(m, i, k) * get(m', k, j))
            in
              set(m'', i, j, lp(0, 0.0))
            end))
     ; m''
  end

fun transpose m =
  let val m' = Array.make (16, 0.0)
  in for(0, 3, fn i =>
         for(0, 3, fn j =>
             set (m', i, j, get (m, j, i))))
     ; m'
  end

fun vmul (m, (x, y, z, t)) =
   (x * get(m, 0, 0) + y * get(m, 0, 1) + z * get(m, 0, 2) + t * get(m, 0, 3),
    x * get(m, 1, 0) + y * get(m, 1, 1) + z * get(m, 1, 2) + t * get(m, 1, 3),
    x * get(m, 2, 0) + y * get(m, 2, 1) + z * get(m, 2, 2) + t * get(m, 2, 3),
    x * get(m, 3, 0) + y * get(m, 3, 1) + z * get(m, 3, 2) + t * get(m, 3, 3))

fun add_scaled (x: v, t, v: v) : v =
   ( #1 x + t * #1 v,
     #2 x + t * #2 v,
     #3 x + t * #3 v,
     #4 x + t * #4 v )

fun add (x: v, y: v) : v =
   ( #1 x + #1 y,
     #2 x + #2 y,
     #3 x + #3 y,
     #4 x + #4 y )

fun sub (x: v, y: v) : v =
   (#1 x - #1 y,
    #2 x - #2 y,
    #3 x - #3 y,
    #4 x - #4 y)

fun prod (x: v, y: v) : real =
   #1 x * #1 y + #2 x * #2 y + #3 x * #3 y + #4 x * #4 y

fun square (vx, vy, vz, vt) : real =
   vx * vx + vy * vy + vz * vz + vt * vt

fun normalize (x: v): v =
  let
     val nx = sqrt (prod (x, x))
  in
     (#1 x / nx,
      #2 x / nx,
      #3 x / nx,
      #4 x / nx)
  end

fun neg (x: v) : v =
   (~(#1 x),
    ~(#2 x),
    ~(#3 x),
    ~(#4 x))

end
signature LEX_TOKEN_STRUCTS = 
   sig
   end

signature LEX_TOKEN = 
   sig
      include LEX_TOKEN_STRUCTS
      
      datatype t =
         Binder of string
       | Bool of bool
       | Eof
       | Identifier of string
       | Int of int
       | Lbrace
       | Lbracket
       | Rbrace
       | Rbracket
       | Real of real
       | String of string
   end
functor LexToken(S: LEX_TOKEN_STRUCTS): LEX_TOKEN = 
struct

open S

datatype t =
   Binder of string
 | Bool of bool
 | Eof
 | Identifier of string
 | Int of int
 | Lbrace
 | Lbracket
 | Rbrace
 | Rbracket
 | Real of real
 | String of string
     
end
type int = Int.int
functor Lex(structure Token: LEX_TOKEN)=
   struct
    structure UserDeclarations =
      struct
val chars: char list ref = ref []

type lexarg = unit

type lexresult = Token.t

val eof: lexarg -> lexresult =
   fn () => Token.Eof

fun fail s = raise Fail s

end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
        struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\000\000\000\000\000\000\000\000\000\026\026\026\000\026\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\026\000\025\000\000\024\000\000\000\000\000\000\000\023\000\021\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\009\009\009\009\009\009\009\009\009\009\009\009\009\009\009\
\\009\009\009\009\009\009\009\009\009\009\009\011\000\010\000\000\
\\000\009\009\009\009\009\009\009\009\009\009\009\009\009\009\009\
\\009\009\009\009\009\009\009\009\009\009\009\008\000\007\000\000\
\\000"
),
 (3, 
"\000\000\000\000\000\000\000\000\000\027\029\029\000\028\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\000\
\\000"
),
 (5, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\030\030\031\030\030\030\030\030\030\030\030\030\030\030\030\030\
\\030\030\030\030\030\030\030\030\030\030\030\030\030\030\030\030\
\\030\030\030\030\030\030\030\030\030\030\030\030\030\030\030\030\
\\030\030\030\030\030\030\030\030\030\030\030\030\030\030\030\030\
\\030\030\030\030\030\030\030\030\030\030\030\030\030\030\030\030\
\\030\030\030\030\030\030\030\030\030\030\030\030\030\030\030\000\
\\000"
),
 (9, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\009\000\000\
\\009\009\009\009\009\009\009\009\009\009\000\000\000\000\000\000\
\\000\009\009\009\009\009\009\009\009\009\009\009\009\009\009\009\
\\009\009\009\009\009\009\009\009\009\009\009\000\000\000\000\009\
\\000\009\009\009\009\009\009\009\009\009\009\009\009\009\009\009\
\\009\009\009\009\009\009\009\009\009\009\009\000\000\000\000\000\
\\000"
),
 (12, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\016\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\000\000\000\000\013\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\013\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (13, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\015\000\000\
\\014\014\014\014\014\014\014\014\014\014\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (14, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\014\014\014\014\014\014\014\014\014\014\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (16, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\017\017\017\017\017\017\017\017\017\017\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (17, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\017\017\017\017\017\017\017\017\017\017\000\000\000\000\000\000\
\\000\000\000\000\000\018\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\018\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (18, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\020\000\000\
\\019\019\019\019\019\019\019\019\019\019\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (19, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\019\019\019\019\019\019\019\019\019\019\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (21, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\022\022\022\022\022\022\022\022\022\022\022\022\022\022\022\
\\022\022\022\022\022\022\022\022\022\022\022\000\000\000\000\000\
\\000\022\022\022\022\022\022\022\022\022\022\022\022\022\022\022\
\\022\022\022\022\022\022\022\022\022\022\022\000\000\000\000\000\
\\000"
),
 (22, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\022\000\000\
\\022\022\022\022\022\022\022\022\022\022\000\000\000\000\000\000\
\\000\022\022\022\022\022\022\022\022\022\022\022\022\022\022\022\
\\022\022\022\022\022\022\022\022\022\022\022\000\000\000\000\022\
\\000\022\022\022\022\022\022\022\022\022\022\022\022\022\022\022\
\\022\022\022\022\022\022\022\022\022\022\022\000\000\000\000\000\
\\000"
),
 (23, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (28, 
"\000\000\000\000\000\000\000\000\000\000\029\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = map f (rev (tl (rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [], trans = 3},
{fin = [], trans = 3},
{fin = [], trans = 5},
{fin = [], trans = 5},
{fin = [(N 13)], trans = 0},
{fin = [(N 11)], trans = 0},
{fin = [(N 49)], trans = 9},
{fin = [(N 9)], trans = 0},
{fin = [(N 7)], trans = 0},
{fin = [(N 39)], trans = 12},
{fin = [], trans = 13},
{fin = [(N 35)], trans = 14},
{fin = [], trans = 14},
{fin = [], trans = 16},
{fin = [(N 35)], trans = 17},
{fin = [], trans = 18},
{fin = [(N 35)], trans = 19},
{fin = [], trans = 19},
{fin = [], trans = 21},
{fin = [(N 20)], trans = 22},
{fin = [], trans = 23},
{fin = [(N 43)], trans = 0},
{fin = [(N 41)], trans = 0},
{fin = [(N 5)], trans = 0},
{fin = [(N 58)], trans = 0},
{fin = [(N 55)], trans = 28},
{fin = [(N 55)], trans = 0},
{fin = [(N 62)], trans = 0},
{fin = [(N 60),(N 62)], trans = 0}])
end
structure StartStates =
        struct
        datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val C = STARTSTATE 3;
val INITIAL = STARTSTATE 1;
val S = STARTSTATE 5;

end
type result = UserDeclarations.lexresult
        exception LexerError (* raised if illegal leaf action tried *)
end

type int = Int.int
fun makeLexer (yyinput: int -> string) =
let     val yygone0:int=1
        val yyb = ref "\n"              (* buffer *)
        val yybl: int ref = ref 1               (*buffer length *)
        val yybufpos: int ref = ref 1           (* location of next character to use *)
        val yygone: int ref = ref yygone0       (* position in file of beginning of buffer *)
        val yydone = ref false          (* eof found yet? *)
        val yybegin: int ref = ref 1            (*Current 'start state' for lexer *)

        val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
                 yybegin := x

fun lex (yyarg as (())) =
let fun continue() : Internal.result = 
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0: int) =
        let fun action (i: int,nil) = raise LexError
        | action (i,nil::l) = action (i-1,l)
        | action (i,(node::acts)::l) =
                case node of
                    Internal.N yyk => 
                        (let fun yymktext() = String.substring(!yyb,i0,i-i0)
                             val yypos: int = i0+ !yygone
                        fun REJECT() = action(i,acts::l)
                        open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

                        (* Application actions *)

  11 => (Token.Lbrace)
| 13 => (Token.Rbrace)
| 20 => let val yytext=yymktext() in Token.Binder(String.extract(yytext, 1, NONE)) end
| 35 => let val yytext=yymktext() in Token.Real(case Real.fromString yytext of
                                  NONE =>
                                     fail(concat["bad real constant ", yytext])
                                | SOME r => r) end
| 39 => let val yytext=yymktext() in Token.Int(case Int.fromString yytext of
                                NONE => 
                                   fail(concat["bad int constant ", yytext])
                              | SOME i => i) end
| 41 => (chars := []; YYBEGIN S; continue())
| 43 => (YYBEGIN C; continue())
| 49 => let val yytext=yymktext() in Token.Identifier yytext end
| 5 => (continue())
| 55 => (YYBEGIN INITIAL; continue())
| 58 => (continue())
| 60 => (let val s = (implode(rev(!chars)) before chars := nil)
               in YYBEGIN INITIAL
                  ; Token.String s
               end)
| 62 => let val yytext=yymktext() in chars := String.sub(yytext, 0) :: !chars
               ; continue() end
| 7 => (Token.Lbracket)
| 9 => (Token.Rbracket)
| _ => raise Internal.LexerError

                ) end )

        val {fin,trans} = Vector.sub(Internal.tab, s)
        val NewAcceptingLeaves = fin::AcceptingLeaves
        in if l = !yybl then
             if trans = #trans(Vector.sub(Internal.tab,0))
               then action(l,NewAcceptingLeaves
) else      let val newchars= if !yydone then "" else yyinput 1024
            in if (String.size newchars)=0
                  then (yydone := true;
                        if (l=i0) then UserDeclarations.eof yyarg
                                  else action(l,NewAcceptingLeaves))
                  else (if i0=l then yyb := newchars
                     else yyb := String.substring(!yyb,i0,l-i0)^newchars;
                     yygone := !yygone+i0;
                     yybl := String.size (!yyb);
                     scan (s,AcceptingLeaves,l-i0,0))
            end
          else let val NewChar = Char.ord(CharVector.sub(!yyb,l))
                val NewChar = if NewChar<128 then NewChar else 128
                val NewState = Char.ord(CharVector.sub(trans,NewChar))
                in if NewState=0 then action(l,NewAcceptingLeaves)
                else scan(NewState,NewAcceptingLeaves,l+1,i0)
        end
        end
(*
        val start= if String.substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
        in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
in continue end
  in lex
  end
end
signature PROGRAM =
   sig
      include CAML
      (**** Basic types: programs, values, ... ****)

      datatype k =
         Acos | Addi | Addf | Apply | Asin | Clampf | Cone | Cos | Cube
       | Cylinder | Difference | Divi | Divf | Eqi | Eqf | Floor | Frac
       | Get | Getx | Gety | Getz | If | Intersect | Length | Lessi | Lessf
       | Light | Modi | Muli | Mulf | Negi | Negf | Plane | Point
       | Pointlight | Real | Render | Rotatex | Rotatey | Rotatez | Scale
       | Sin | Sphere | Spotlight | Sqrt | Subi | Subf | Translate | Union
       | Uscale

      (* Program tokens *)
      datatype t =
         Fun of t list
       | Arr of t list
       | Ident of string
       | Binder of string
       | Int of int
       | Float of float
       | Bool of bool
       | String of string
       | Prim of k

      (* internal representation of program tokens *)
      datatype t' =
         Fun' of t' list
       | Arr' of t' list
       | Ident' of int (* index to environment stack *)
       | Binder'
       (*
     | Int' of int
     | Float' of float
     | Bool' of bool
     | String' of string
        *)
       | Prim' of k
       | Val' of v (* inlined value *)

      (* Values *)
      and v =
         VInt of int
        | VFloat of float
        | VBool of bool
        | VStr of string
        | VClos of v list * t' list
        | VFun of (v list -> v list) (* XXX for the compiler *)
        | VArr of v array
        | VPoint of v * v * v (* XXX Maybe these should be floats? *)
        | VObj of obj
        | VLight of v * v
        | VPtLight of v * v
        | VStLight of v * v * v * v * v

      and obj =
         OObj of kind * closure ref
        | OTransform of
          obj *
          Matrix.t *     (* World to object *)
          Matrix.t *     (* Object to world *)
          float *        (* Scale factor *)
          bool           (* Isometry? *)
        | OUnion of obj * obj
        | OInter of obj * obj
        | ODiff of obj * obj

      and kind =
         OSphere
        | OCube
        | OCylind
        | OCone
        | OPlane

      and closure =
         Unopt of v (* Unoptimized function *)
        | Opt of v
        | Cst of (float * float * float * float * float * float)

      (* Translation of an identifier *)
      val translate : string -> t

      (* Get the name of an identifier *)
(*      val name : t' -> string *)

      exception Stuck_computation of v list * v list * t' list
      exception Stuck_computation' (* for compiler *)

      val read: TextIO.instream -> t list
   end
structure Program: PROGRAM =
struct

open Caml

datatype k =
    Acos | Addi | Addf | Apply | Asin | Clampf | Cone | Cos | Cube
  | Cylinder | Difference | Divi | Divf | Eqi | Eqf | Floor | Frac
  | Get | Getx | Gety | Getz | If | Intersect | Length | Lessi | Lessf
  | Light | Modi | Muli | Mulf | Negi | Negf | Plane | Point
  | Pointlight | Real | Render | Rotatex | Rotatey | Rotatez | Scale
  | Sin | Sphere | Spotlight | Sqrt | Subi | Subf | Translate | Union
  | Uscale

datatype t =
    Fun of t list
  | Arr of t list
  | Ident of string
  | Binder of string
  | Int of int
  | Float of float
  | Bool of bool
  | String of string
  | Prim of k

datatype t' =
    Fun' of t' list
  | Arr' of t' list
  | Ident' of int (* index to environment stack *)
  | Binder'
(*
  | Int' of int
  | Float' of float
  | Bool' of bool
  | String' of string
*)
  | Prim' of k
  | Val' of v (* inlined value *)

and v =
   VInt of int
  | VFloat of float
  | VBool of bool
  | VStr of string
  | VClos of v list * t' list
  | VFun of (v list -> v list) (* XXX for the compiler *)
  | VArr of v array
  | VPoint of v * v * v
  | VObj of obj
  | VLight of v * v
  | VPtLight of v * v
  | VStLight of v * v * v * v * v

and obj =
    OObj of kind * closure ref
  | OTransform of
      obj *
      Matrix.t *     (* World to object *)
      Matrix.t *     (* Object to world *)
      float *        (* Scale factor *)
      bool           (* Isometry? *)
  | OUnion of obj * obj
  | OInter of obj * obj
  | ODiff of obj * obj

and kind =
    OSphere
  | OCube
  | OCylind
  | OCone
  | OPlane

and closure =
    Unopt of v
  | Opt of v
  | Cst of (float * float * float * float * float * float)

fun create_hashtables size init =
   let
      val tbl: (string, t) Hashtbl.t = Hashtbl.create size 
(*      val tbl' = Hashtbl.create size  *)
   in
      List.iter (fn (key, data) => Hashtbl.add tbl key data) init;
(*      List.iter (fn (data, key) -> Hashtbl.add tbl' key data) init; *)
      tbl (*, tbl' *)
   end

val keywords(*, keyword_name)*) =
  create_hashtables 101
(* Booleans are either the literal true or the literal false. *)
    [ ("true", Bool true),
      ("false", Bool false),
(* Operators (see appendix) *)
      ("acos", Prim Acos),
      ("addi", Prim Addi),
      ("addf", Prim Addf),
      ("apply", Prim Apply),
      ("asin", Prim Asin),
      ("clampf", Prim Clampf),
      ("cone", Prim Cone),
      ("cos", Prim Cos),
      ("cube", Prim Cube),
      ("cylinder", Prim Cylinder),
      ("difference", Prim Difference),
      ("divi", Prim Divi),
      ("divf", Prim Divf),
      ("eqi", Prim Eqi),
      ("eqf", Prim Eqf),
      ("floor", Prim Floor),
      ("frac", Prim Frac),
      ("get", Prim Get),
      ("getx", Prim Getx),
      ("gety", Prim Gety),
      ("getz", Prim Getz),
      ("if", Prim If),
      ("intersect", Prim Intersect),
      ("length", Prim Length),
      ("lessi", Prim Lessi),
      ("lessf", Prim Lessf),
      ("light", Prim Light),
      ("modi", Prim Modi),
      ("muli", Prim Muli),
      ("mulf", Prim Mulf),
      ("negi", Prim Negi),
      ("negf", Prim Negf),
      ("plane", Prim Plane),
      ("point", Prim Point),
      ("pointlight", Prim Pointlight),
      ("real", Prim Real),
      ("render", Prim Render),
      ("rotatex", Prim Rotatex),
      ("rotatey", Prim Rotatey),
      ("rotatez", Prim Rotatez),
      ("scale", Prim Scale),
      ("sin", Prim Sin),
      ("sphere", Prim Sphere),
      ("spotlight", Prim Spotlight),
      ("sqrt", Prim Sqrt),
      ("subi", Prim Subi),
      ("subf", Prim Subf),
      ("translate", Prim Translate),
      ("union", Prim Union),
      ("uscale", Prim Uscale)]

fun translate i =
   Hashtbl.find keywords i
   handle Not_found => Ident i

(* fun name token =
 *   Hashtbl.find keyword_name
 *     (match token with
 *       Prim' k -> Prim k
 *     | _       -> raise Not_found)
 * 
 *)
exception Stuck_computation of v list * v list * t' list
exception Stuck_computation' (* for compiler *)

structure LexToken = LexToken()
structure Lex = Lex(structure Token = LexToken)

fun read(ins: TextIO.instream): t list =
   let
      val lex: unit -> LexToken.t =
         Lex.makeLexer(fn n => TextIO.inputN(ins, n))()
      local
         val next: LexToken.t option ref = ref NONE
      in
         fun token(): LexToken.t =
            case !next of
               NONE => lex()
             | SOME t => (next := NONE; t)
         fun save(t: LexToken.t): unit =
            next := SOME t
      end
      fun bad() = failwith "invalid input"
      fun many(done: LexToken.t -> bool): t list =
         let
            fun loop(ac: t list) =
               case one() of
                  NONE => if done(token())
                             then rev ac
                          else bad()
                | SOME t => loop(t :: ac)
         in loop []
         end
      and one(): t option =
         let fun tok t = SOME t
         in case token() of
            LexToken.Binder x => tok(Binder x)
          | LexToken.Bool b => tok(Bool b)
          | LexToken.Identifier x => tok(translate x)
          | LexToken.Int i => tok(Int i)
          | LexToken.Lbrace =>
               SOME(Fun(many(fn LexToken.Rbrace => true | _ => false)))
          | LexToken.Lbracket =>
               SOME(Arr(many(fn LexToken.Rbracket => true | _ =>false)))
          | LexToken.Real r => tok(Float r)
          | LexToken.String s => tok(String s)
          | t => (save t; NONE)
         end
   in many(fn LexToken.Eof => true | _ => false)
   end

end
signature PPM =
   sig
      include CAML

      type pixmap

      val init : (*width:*)int * (*height:*)int -> pixmap
      val dump : string * pixmap -> unit
(*      val load : string -> pixmap *)

      val width : pixmap -> int
      val height : pixmap -> int

      val get : pixmap * int * int * int -> int
      val set : pixmap * int * int * int * int -> unit
      val setp : pixmap * int * int * int * int * int -> unit
   end
structure Ppm: PPM =
struct
   
open Caml

structure Array = Word8Array
structure Word = Word8
   
type pixmap = Array.array * int

fun get ((img, width), i, j, k) =
   Word.toInt (Array.sub (img, ((j * width) + i) * 3 + k))

fun set ((img, width), i, j, k, v) =
   Array.update (img, ((j * width) + i) * 3 + k, Word.fromInt v)

fun setp ((img, width), i, j, r, g, b) =
  let val p = ((j * width) + i) * 3
  in Array.update(img, p, Word.fromInt r)
     ; Array.update(img, p + 1, Word.fromInt g)
     ; Array.update(img, p + 2, Word.fromInt b)
  end

fun init (width, height) =
   (Array.array(height * width * 3, 0w0), width)

fun width (s, width) = width
fun height (s, width) = Array.length s div width div 3

fun dump (file, (img, width)) =
  let
     val sz = Array.length img
     val height = sz div 3 div width
     val f = open_out_bin file
  in output_string (f, "P6\n# PL Club - translated to SML\n")
     ; output_string (f, concat[Int.toString width, " ",
                               Int.toString height, "\n255\n"])
     ; output_string (f, Byte.unpackString (Word8ArraySlice.slice
                                            (img, 0, NONE)))
     ; close_out f
  end

(* fun load file =
 *   let f = open_in_bin file in
 *   assert (input_line f = "P6");
 *   assert ((input_line f).[0] = '#');
 *   let s = input_line f in
 *   let i = ref 0 in
 *   while s.[!i] >= '0' && s.[!i] <= '9' do incr i done;
 *   let width = int_of_string (String.sub s 0 !i) in
 *   let height =
 *     int_of_string (String.sub s (!i + 1) (String.length s - !i - 1)) in
 *   assert (input_line f = "255");
 *   let (s, _) as img = init width height in
 *   really_input f s 0 (String.length s);
 *   close_in f;
 *   img
 *)
end
signature RENDER =
   sig
      include CAML
         
      val apply : (Program.v * Program.v list -> Program.v list) ref
      val inline_closure : (Program.v -> Program.v) ref

      val f :
         (*amb:*)(float * float * float) * (*lights:*) Program.v array *
         (*obj:*)Program.obj * (*depth:*)int * (*fov:*)float *
         (*wid:*)int * (*ht:*)int *
         (*file:*)string -> unit
   end
structure Render: RENDER =
struct

open Caml
infix 9 **
open Program

(* Scene description *)
datatype kind = (* section 3.2 *)
    SSphere of Matrix.v (* Center *) * float (* Square of the radius *)
  | SEllips
  | SCube of Matrix.v (* Normal x = 0 *) *
             Matrix.v (* Normal y = 0 *) *
             Matrix.v (* Normal z = 0 *)
  | SCylind of Matrix.v (* Normal *)
  | SCone of Matrix.v (* Normal *)
  | SPlane of Matrix.v (* Equation *) * Matrix.v (* Normal *)

datatype scene = (* section 3.7 *)
    SObj of kind * closure ref (* surface function *) * Matrix.t
  | SBound of scene * Matrix.v (* Center *) * float (* Square of the radius *)
  | SUnion of scene * scene
  | SInter of scene * scene
  | SDiff of scene * scene

datatype light = (* section 3.5 *)
    Light of Matrix.v (* negated & normalized *) * (float * float * float)
  | PtLight of Matrix.v * (float * float * float)
  | StLight of Matrix.v * Matrix.v (* negated & normalized *) *
               (float * float * float) * float (* cos *) * float

type desc =
  { amb : float * float * float,
    lights : light array,
    scene : scene }

open Math
open Matrix

(**** Scene calculation ****)

(* Plane equation and normal in world coordinates *)
fun plane_eq(m, v) =
  let
     val n = vmul (transpose m, v )
  in
     (n, normalize(#1 n, #2 n, #3 n, 0.0))
  end

val origin = ( 0.0, 0.0, 0.0, 1.0 )
val cube_center = ( 0.5, 0.5, 0.5, 1.0 )
val cylinder_center = ( 0.0, 0.5, 0.0, 1.0 )
val cone_center = ( 0.0, 1.0, 0.0, 1.0 )

fun intern_obj(m, m1, scale, isom, ob) =
(* apply transformations *)
  case ob of
    OObj (OSphere, f) =>
       if isom
          then
             let
                val center = vmul (m1, origin)
                val radius = scale * scale
             in
                SBound (SObj (SSphere (center, radius), f, m), center, radius)
             end
       else
          let
             val center = vmul (m1, origin)
             val radius = scale * scale
          in
             SBound (SObj (SEllips, f, m), center, radius)
          end
  | OObj (OCube, f) =>
      let
         val (nx, nx') = plane_eq(m, (1.0, 0.0, 0.0, 0.0))
         val (ny, ny') = plane_eq(m, (0.0, 1.0, 0.0, 0.0))
         val (nz, nz') = plane_eq(m, (0.0, 0.0, 1.0, 0.0))
         val c = SObj (SCube (nx', ny', nz'), f, m)
      in
         SBound (c, vmul (m1, cube_center), scale * scale * 0.75)
      end
  | OObj (OCylind, f) =>
      let
         val (n, n') = plane_eq(m, (0.0, 1.0, 0.0, 0.0))
         val c = SObj (SCylind n', f, m)
      in
         SBound (c, vmul(m1, cylinder_center), scale * scale * 1.25)
      end
  | OObj (OCone, f) =>
      let
         val (n, n') = plane_eq(m, (0.0, 1.0, 0.0, 0.0))
         val c = SObj (SCone n', f, m)
      in
         SBound (c, vmul(m1, cone_center), scale * scale)
      end
  | OObj (OPlane, f) =>
      let
         val (n, n') = plane_eq(m, (0.0, 1.0, 0.0, 0.0))
      in
         SObj (SPlane (n, n'), f, m)
      end
  | OTransform (o', m', m'1, scale', isom') =>
      intern_obj
        (Matrix.mul(m', m), Matrix.mul(m1, m'1),
         scale * scale', isom andalso isom', o')
  | OUnion (o1, o2) =>
      SUnion (intern_obj(m, m1, scale, isom, o1),
              intern_obj(m, m1, scale, isom, o2))
  | OInter (o1, o2) =>
      SInter (intern_obj(m, m1, scale, isom, o1),
              intern_obj(m, m1, scale, isom, o2))
  | ODiff (ODiff (o1, o2), o3) =>
      (* Better to have unions that diffs for introducing bounds *)
      intern_obj(m, m1, scale, isom, (ODiff (o1, OUnion (o2, o3))))
  | ODiff (o1, o2) =>
      SDiff (intern_obj(m, m1, scale, isom, o1),
             intern_obj(m, m1, scale, isom, o2))

fun intern_lights a =
  Array.map
    (fn VLight (VPoint (VFloat x, VFloat y, VFloat z),
                 VPoint (VFloat r, VFloat g, VFloat b)) =>
           Light (normalize (neg (x, y, z, 0.0)), (r, g, b))
       | VPtLight (VPoint (VFloat x, VFloat y, VFloat z),
                   VPoint (VFloat r, VFloat g, VFloat b)) =>
           PtLight ((x, y, z, 1.0), (r, g, b))
       | VStLight (VPoint (VFloat x, VFloat y, VFloat z),
                   VPoint (VFloat x', VFloat y', VFloat z'),
                   VPoint (VFloat r, VFloat g, VFloat b),
                   VFloat cutoff, VFloat exp) =>
           StLight ((x, y, z, 1.0),
                    normalize (x - x', y - y', z - z', 0.0),
                    (r, g, b), dcos cutoff, exp)
       | _ =>
           raise(Fail "assert false"))
    a

(**** Scene optimization ****)

fun flatten_rec(sc, rem) =
  case sc of
    SUnion (sc1, sc2) => flatten_rec(sc1, flatten_rec(sc2, rem))
  | sc                => sc :: rem

fun flatten_union sc = flatten_rec(sc, [])

fun object_cost k : int =
  case k of
    SSphere _ => 1
  | SEllips   => 2
  | SCube _   => 4
  | SCylind _ => 2
  | SCone _   => 2
  | SPlane _  => 0 (* Planes do not have a bounding box anyway *)

fun add_bound (r0, (x, r, cost, sc)) =
  if r0 < 0.0
     then
         if r < 0.0 orelse cost <= 1
            then (cost, sc)
         else
            (1, SBound (sc, x, r))
  else
     (* Cost of bounds *)
     let
        val c0 = r0 + r * float cost 
        (* Cost ofout bounds *)
        val c1 = r0 * float cost
     in
        if c0 < c1 then
           (1, SBound (sc, x, r))
        else
           (cost, sc)
     end

fun union_bound (dsc1 as (x1, r1, cost1, sc1),
                 dsc2 as (x2, r2, cost2, sc2)) =
  if r1 < 0.0 then
    let
       val (cost2', sc2') = add_bound(r1, dsc2)
    in
       (x1, r1, cost1, SUnion (sc1, sc2'))
    end
  else if r2 < 0.0 then
    let
       val (cost1', sc1') = add_bound (r2, dsc1)
    in
       (x2, r2, cost2, SUnion (sc1', sc2))
    end
  else
    let
       val d = sqrt (square (sub(x2, x1)))
       val r1' = sqrt r1
       val r2' = sqrt r2
    in
       if d + r2' <= r1' then
          let
             val (cost2', sc2') = add_bound (r1, dsc2)
          in
             (x1, r1, cost1 + cost2', SUnion (sc1, sc2'))
          end
       else if d + r1' <= r2' then
          let
             val (cost1', sc1') = add_bound (r2, dsc1)
          in
             (x2, r2, cost1' + cost2, SUnion (sc1', sc2))
          end
            else
               let
                  val r' = (r1' + r2' + d) * 0.5
                  val r = r' * r'
                  val x = add_scaled (x1, (r' - r1') / d, sub(x2, x1))
                  val (cost1', sc1') = add_bound (r, dsc1)
                  val (cost2', sc2') = add_bound (r, dsc2)
               in
                  (x, r, cost1' + cost2', SUnion (sc1', sc2'))
               end
    end
 
fun union_radius (dsc1 as (x1, r1, cost1, sc1),
                  dsc2 as (x2, r2, cost2, sc2)) =
    let
       val d = sqrt (square (sub (x2, x1)))
       val r1' = sqrt r1
       val r2' = sqrt r2
    in
       if d + r2' <= r1' then r1 else
          if d + r1' <= r2' then r2 else
             let
                val r' = (r1' + r2' + d) * 0.5
             in
                r' * r'
             end
    end

fun merge2 l =
  case l of
    sc1 :: sc2 :: r => union_bound (sc1, sc2) :: merge2 r
  | _               => l

fun merge_union l =
  case l of
    []    => raise(Fail "assert false")
  | [sc1] => sc1
  | l     => merge_union (merge2 l)

fun opt_union l =
  case l of
    [] => l
  | [_] => l
  | [sc1, sc2] => [union_bound(sc1, sc2)]
  | _ =>
       let
          val c = Array.of_list l
          val n = Array.length c
          val m = Array2.array(n, n, infinity)
          val _ =
             for(0, n - 1, fn i =>
                 for(0, n - 1, fn j =>
                     if i <> j
                        then Array2.update(m, i, j,
                                           union_radius
                                           (Array.sub(c, i), Array.sub(c, j)))
                     else ()))
          val remain = Array.init (n, fn i => i)
          val _ =
             forDown
             (n - 1, 1, fn k =>
              let
                 val gain = ref infinity
                 val i0 = ref 0
                 val j0 = ref 0
                 val _ =
                    for(0, k, fn i =>
                        for(0, k, fn j =>
                            let
                               val i' = Array.sub(remain, i)
                               val j' = Array.sub(remain, j)
                            in
                               if Array2.sub(m, i', j') < !gain
                                  then 
                                     (gain := Array2.sub(m, i', j')
                                      ; i0 := i
                                      ; j0 := j)
                               else ()
                            end))
                 val i = Array.sub(remain, !i0)
                 val j = Array.sub(remain, !j0)
              in
                 Array.update(remain, !j0, Array.sub(remain, k));
                 Array.update(c, i,
                              union_bound (Array.sub(c, i), Array.sub(c, j)));
                 for(0, k - 1, fn j0 =>
                     let
                        val j = Array.sub(remain, j0)
                     in
                        if i <> j
                           then
                              (
                               Array2.update
                               (m, i, j,
                                union_radius
                                (Array.sub(c, i), Array.sub(c, j)));
                               Array2.update
                               (m, j, i,
                                union_radius
                                (Array.sub(c, i), Array.sub(c, j))))
                        else ()
                     end)
              end)
       in [Array.sub(c, Array.sub(remain, 0))]
       end

fun optimize_rec sc =
  case sc of
    SObj (kind, _, _) =>
      (origin, ~1.0, object_cost kind, sc)
  | SUnion _ =>
       let
          val l = List.map optimize_rec (flatten_union sc)
          val unbounded = List.filter (fn (_, r, _, _) => r < 0.0) l
          val bounded = List.filter (fn (_, r, _, _) => r >= 0.0) l
       in
          merge_union (opt_union bounded @ unbounded)
       end
  | SInter (sc1, sc2) =>
       let
          val (x1, r1, cost1, sc1) = optimize_rec sc1
          val (x2, r2, cost2, sc2) = optimize_rec sc2
       in
          (* XXX We could have a tighter bound... *)
          if r2 < 0.0 then
             (x2, r2, cost2, SInter (sc1, sc2))
          else if r1 < 0.0 then
             (x1, r1, cost1, SInter (sc2, sc1))
               else if r1 < r2 then
                  (x1, r1, cost1, SInter (sc1, sc2))
                    else
                       (x2, r2, cost1, SInter (sc2, sc1))
       end
  | SDiff (sc1, sc2) =>
       let
          val (x1, r1, cost1, sc1) = optimize_rec sc1
          val dsc2 as (x2, r2, cost2, sc2) = optimize_rec sc2
          val (cost2', sc2') = add_bound (r1, dsc2)
       in
          (x1, r1, cost1, SDiff (sc1, sc2'))
       end
  | SBound (sc1, x, r) =>
       let
          val (_, _, cost1, sc1) = optimize_rec sc1
       in
          (x, r, cost1, sc1)
       end

fun optimize sc = #2 (add_bound (~1.0, optimize_rec sc))

(**** Rendering ****)

(* operations for intervals *)
fun union (l1, l2) : (float * scene * float * scene) list = (* ES: checked *)
  case (l1, l2) of
    ([], _) => l2
  | (_, []) => l1
  | ((i1 as (t1, o1, t1', o1')) :: r1,
     (i2 as (t2, o2, t2', o2')) :: r2) =>
    if t1' < t2
       then i1 :: union(r1, l2)
    else if t2' < t1
            then i2 :: union(l1, r2)
         else
            if t1 < t2 then
               if t1' < t2' then
                  union(r1, (t1, o1, t2', o2')::r2)
               else
                  union((t1, o1, t1', o1')::r1, r2)
            else
               if t1' < t2' then
                  union(r1, ((t2, o2, t2', o2')::r2))
               else
                  union((t2, o2, t1', o1')::r1, r2)

fun inter (l1, l2) : (float * scene * float * scene) list = (* ES: checked *)
  case (l1, l2) of
    ([], _) => []
  | (_, []) => []
  | ((i1 as (t1, o1, t1', o1')) :: r1,
     (i2 as (t2, o2, t2', o2')) :: r2) =>
    if t1' <= t2
       then inter(r1, l2)
    else if t2' <= t1
            then inter(l1, r2)
         else
            if t1 < t2 then
               if t1' < t2' then
                  (t2, o2, t1', o1') :: inter(r1, l2)
               else
                  i2 :: inter(l1, r2)
            else
               if t1' < t2' then
                  i1 :: inter(r1, l2)
               else
                  (t1, o1, t2', o2') :: inter(l1, r2)

fun diff (l1, l2) : (float * scene * float * scene) list = (* ES: checked *)
  case (l1, l2) of
     ([], _) => []
   | (_, []) => l1
  | ((i1 as (t1, o1, t1', o1')) :: r1,
     (i2 as (t2, o2, t2', o2')) :: r2) =>
    if t1' <= t2
       then i1 :: diff(r1, l2)
    else if t2' <= t1
            then diff(l1, r2)
         else
            if t1 < t2 then
               if t1' < t2' then
                  (t1, o1, t2, o2) :: diff(r1, l2)
               else
                  (t1, o1, t2, o2) :: diff((t2', o2', t1', o1') :: r1, r2)
            else
               if t1' < t2' then
                  diff(r1, l2)
               else
                  diff((t2', o2', t1', o1') :: r1, r2)

(* intersection of ray and object *)
fun plane (orig, dir, scene, eq) : (float * scene * float * scene) list =
   (* XXX Need to be checked *)
   let
      val porig = prod (eq, orig)
      val pdir = prod (eq, dir)
      val t = ~ porig / pdir
   in
      if porig < 0.0 then
         if t > 0.0 then
            [(0.0, scene, t, scene)]
         else
            [(0.0, scene, infinity, scene)]
      else
         if t > 0.0 then
            [(t, scene, infinity, scene)]
         else
            []
   end

fun band (obj, x, v, i) : (float * scene * float * scene) list = (* ES: checked *)
   let
      val t1 = ~ (i x) / (i v)
      val t2 = (1.0 - (i x)) / (i v)
      val t2' = if t1 >= t2 then t1 else t2
   in
      if t2' < 0.0 then
         []
      else
         let val t1' = if t1 <= t2 then t1 else t2
         in
            if t1' < 0.0 then
               [(0.0, obj, t2', obj)]
            else
               [(t1', obj, t2', obj)]
         end
   end

fun cube (orig, dir, scene, m): (float * scene * float * scene) list =
   (* ES: checked *)
   let
      val x = vmul (m, orig)
      val v = vmul (m, dir)
   in
      case band (scene, x, v, #1) of
         [] => []
       | l0 =>
            case inter (l0, band (scene, x, v, #2)) of
               [] => []
             | l1 => inter (l1, band (scene, x, v, #3))
   end

fun sphere (orig, dir, scene, x, r2): (float * scene * float * scene) list =
   let
      val v = sub (x, orig)
      (* Square of the distance between the origin and the center of the sphere *)
      val v2 = square v
      val dir2 = square dir
      val p = prod (v, dir)
      (* Square of the distance between the ray and the center *)
      val d2 = v2 - p * p / dir2
      val delta = r2 - d2
   in  if delta <= 0.0
          then []
       else
          let
             val sq = sqrt (delta / dir2)
             val t1 = p / dir2 - sq
             val t2 = p / dir2 + sq
          in
             if t2 < 0.0
                then []
             else
                [(max_float (0.0, t1), scene, t2, scene)]
          end
   end

fun ellipsoid (orig, dir, scene, m): (float * scene * float * scene) list =
   (* ES: checked *)
   let
      val x = vmul (m, orig)
      val v = vmul (m, dir)
      val x2 = square x
      val v2 = square v
      val xv = prod (x, v)
      val delta = xv * xv - v2 * (x2 - 2.0)
   in
      if delta <= 0.0 then
         []
      else
         let
            val sq = sqrt delta
            val t1 = (~ xv - sq) / v2
            val t2 = (~ xv + sq) / v2
         in    if t2 < 0.0 then
            []
               else
                  [(max_float (0.0, t1), scene, t2, scene)]
         end
   end

fun cylinder (orig, dir, scene, m): (float * scene * float * scene) list =
   let
      val x = vmul (m, orig)
      val v = vmul (m, dir)
      val x2 = #1 x * #1 x + #3 x * #3 x - 1.0
      val v2 = #1 v * #1 v + #3 v * #3 v
      val xv = #1 x * #1 v + #3 x * #3 v
      val delta = xv * xv - v2 * x2
   in
      if delta <= 0.0 then
         []
      else
         let
            val sq = sqrt delta
            val t1 = (~ xv - sq) / v2
            val t2 = (~ xv + sq) / v2
         in    if t2 < 0.0 then
            []
               else
                  inter
                  ([(max_float (0.0, t1), scene, t2, scene)],
                   band (scene, x, v, #2))
         end
   end

fun cone (orig, dir, scene, m): (float * scene * float * scene) list = 
   let
      val x = vmul (m, orig)
      val v = vmul (m, dir)
      val x2 = #1 x * #1 x + #3 x * #3 x - #2 x * #2 x
      val v2 = #1 v * #1 v + #3 v * #3 v - #2 v * #2 v
      val xv = #1 x * #1 v + #3 x * #3 v - #2 x * #2 v
      val delta = xv * xv - v2 * x2
   in
      if delta <= 0.0 then
         []
      else
         let
            val sq = sqrt delta
            val t1 = (~ xv - sq) / v2
            val t2 = (~ xv + sq) / v2
         in
            if t1 <= t2 then
               if t2 < 0.0 then
                  []
               else
                  inter
                  ([(max_float(0.0, t1), scene, t2, scene)],
                   band (scene, x, v, #2))
            else
               inter
               (if t1 <= 0.0 then
                   [(0.0, scene, infinity, scene)]
                else if t2 <= 0.0 then
                   [(t1, scene, infinity, scene)]
                     else
                        [(0.0, scene, t2, scene), (t1, scene, infinity, scene)],
                band (scene, x, v, #2))
         end
   end

(* XXX Maybe we should check whether the sphere is completely behind us ? *)
fun intersect (orig, dir, x, r2) =
   let
      val (vx, vy, vz, vt) = sub (x, orig)
  (* Square of the distance between the origin and the center of the sphere *)
      val v2 = vx * vx + vy * vy + vz * vz + vt * vt
      val (dx, dy, dz, dt) = dir
      val dir2 = dx * dx + dy * dy + dz * dz + dt * dt
      val p = vx * dx + vy * dy + vz * dz + vt * dt
      (* Square of the distance between the ray and the center *)
      val d2 = v2 - p * p / dir2
   in r2 > d2
   end

fun find_all (orig, dir, scene) =
  case scene of
    SObj (SSphere (x, r2), _, m) =>
      sphere (orig, dir, scene, x, r2)
  | SObj (SEllips, _, m) =>
      ellipsoid (orig, dir, scene, m)
  | SObj (SCube _, _, m) =>
      cube (orig, dir, scene, m)
  | SObj (SCylind _, _, m) =>
      cylinder (orig, dir, scene, m)
  | SObj (SCone _, _, m) =>
      cone (orig, dir, scene, m)
  | SObj (SPlane (eq, _), _, m) =>
      plane (orig, dir, scene, eq)
  | SBound (sc, x, r2) =>
      if intersect (orig, dir, x, r2)
         then find_all (orig, dir, sc)
      else []
  | SUnion (sc1, sc2) =>
      union (find_all (orig, dir, sc1), find_all (orig, dir, sc2))
  | SInter (sc1, sc2) =>
      let val l1 = find_all (orig, dir, sc1)
      in
         case l1 of
            [] => []
          | _ => inter(l1, find_all (orig, dir, sc2))
      end
  | SDiff (sc1, sc2) =>
      let val l1 = find_all(orig, dir, sc1)
      in
         case l1 of
            [] => []
          | _ => diff(l1, find_all(orig, dir, sc2))
      end

fun filter_inter_list l =
  case l of
    (t, _, _, _)::r =>
       if t < epsilon
          then filter_inter_list r
       else l
  | _ => l

fun hit_from_inter bounded l0 =
  let val l = filter_inter_list l0
  in
     case l of
        [] => false
      | (t, _, _, _)::r => (not bounded orelse  t <= 1.0)
  end

fun hit(orig, dir, scene, bounded) =
  case scene of
    SObj (kind, _, m) =>
       (case
           (case kind of
               SSphere (x, r2) => sphere (orig, dir, scene, x, r2)
             | SEllips         => ellipsoid (orig, dir, scene, m)
             | SCube _         => cube (orig, dir, scene, m)
             | SCylind _       => cylinder (orig, dir, scene, m)
             | SCone _         => cone (orig, dir, scene, m)
             | SPlane (eq, _)  => plane (orig, dir, scene, eq)) of
               [] => false
             | [(t, _, _, _)] =>
                  if bounded andalso t > 1.0
                     then false
                  else if t < epsilon
                          then false
                       else true
             | _ => true)
  | SBound (sc, x, r2) =>
      intersect (orig, dir, x, r2)  andalso hit (orig, dir, sc, bounded)
  | SUnion (sc1, sc2) =>
      hit (orig, dir, sc1, bounded) orelse hit (orig, dir, sc2, bounded)
  | SInter (sc1, sc2) =>
      let val l1 = find_all (orig, dir, sc1)
      in
         case l1 of
            [] => false
          | _ => hit_from_inter bounded (inter(l1, find_all (orig, dir, sc2)))
      end
  | SDiff (sc1, sc2) =>
      let
         val l1 = find_all(orig, dir, sc1)
      in
         case l1 of
            [] => false
          | _ => hit_from_inter bounded (diff(l1, find_all(orig, dir, sc2)))
      end

fun visible (desc: desc, orig, dir, bounded) =
  not (hit(orig, dir, #scene desc, bounded))

val black = (0.0, 0.0, 0.0)

val apply : ((Program.v * Program.v list) -> Program.v list) ref =
   ref (fn _ => raise(Fail "assert false"))
val inline_closure : (Program.v -> Program.v) ref =
   ref (fn _ => raise(Fail "assert false"))

(* Value between 0 and 1 from the sinus and cosinus *)
(* Actually, only the sign of the sinus is used *)
fun angle (si, co) =
  let
     val u = dacos co / 360.0
  in
     if si > 0.0 then u else 1.0 - u
  end

(* XXX Check that 0 <= u,v <= 1 *)
fun texture_coord (kind, x: v) = (* section 3.6 *) (* ES: checked *)
   let
      fun ellipsOrSphere() =
         let
            val y = #2 x
            val v = (y + 1.0) * 0.5
         in
            if v < epsilon
               then [VFloat v, VFloat 0.0, VInt 0]
            else
               let
                  val u = angle (#1 x, #3 x / sqrt (1.0 - y * y))
               in
                  [VFloat v, VFloat u, VInt 0]
               end
         end
   in  (* [v; u; face] *)
      case kind of
         SEllips => ellipsOrSphere()
       | SSphere _ => ellipsOrSphere()
       | SCube _ =>
      if abs_float (#3 x) < epsilon then
        [VFloat (#2 x), VFloat (#1 x), VInt 0]
      else if abs_float ((#3 x) - 1.0) < epsilon then
        [VFloat (#2 x), VFloat (#1 x), VInt 1]
      else if abs_float (#1 x) < epsilon then
        [VFloat (#2 x), VFloat (#3 x), VInt 2]
      else if abs_float ((#1 x) - 1.0) < epsilon then
        [VFloat (#2 x), VFloat (#3 x), VInt 3]
      else if abs_float ((#2 x) - 1.0) < epsilon then
        [VFloat (#3 x), VFloat (#1 x), VInt 4]
      else (* if abs_float (#2 x) < epsilon then *)
        [VFloat (#3 x), VFloat (#1 x), VInt 5]
  | SCylind _ =>
      if abs_float (#2 x) < epsilon then
        [VFloat (((#3 x) + 1.0) * 0.5), VFloat (((#1 x) + 1.0) * 0.5), VInt 2]
      else if abs_float ((#2 x) - 1.0) < epsilon then
        [VFloat (((#3 x) + 1.0) * 0.5), VFloat (((#1 x) + 1.0) * 0.5), VInt 1]
      else
        let
           val u = angle (#1 x, #3 x)
        in
           [VFloat (#2 x), VFloat u, VInt 0]
        end
  | SCone _ =>
      let val v = (#2 x)
      in
         if abs_float v < epsilon then
            [VFloat v, VFloat 0.0, VInt 0]
         else
            if abs_float ((#2 x) - 1.0) < epsilon
               then
                  [VFloat (((#3 x) + 1.0) * 0.5),
                   VFloat (((#1 x) + 1.0) * 0.5),
                   VInt 1]
            else
               let
                  val u = angle (#1 x, (#3 x) / v)
               in
                  [VFloat v, VFloat u, VInt 0]
               end
      end
  | SPlane _ =>
      [VFloat (#3 x), VFloat (#1 x), VInt 0]
   end

fun normal (kind, m, x', x) =
  case kind of
    SSphere (x0, _) =>
      normalize (sub (x, x0))
  | SEllips =>
      let val (n0, n1, n2, _) = vmul (transpose m, x')
      in
         normalize(n0, n1, n2, 0.0)
      end
  | SCylind n =>
      if abs_float (#2 x') < epsilon
         orelse abs_float (#2 x') - 1.0 < epsilon then
        n
      else
        (* XXX Could be optimized... *)
        let
           val (n0, n1, n2, _) = vmul (transpose m, (#1 x', 0.0, #3 x', 0.0))
        in
           normalize(n0, n1, n2, 0.0)
        end
  | SCone n =>
      if abs_float (#2 x') - 1.0 < epsilon
         then n
      else
        let
           val (n0, n1, n2, _) =
              vmul (transpose m, (#1 x', ~(#2 x'), #3 x', 0.0))
        in
           normalize(n0, n1, n2, 0.0)
        end
  | SCube (nx, ny, nz) =>
      if abs_float (#3 x') < epsilon
         orelse abs_float (#3 x') - 1.0 < epsilon
         then nz
      else if abs_float (#1 x') < epsilon
              orelse abs_float (#1 x') - 1.0 < epsilon
              then nx
           else ny
  | SPlane (_, n) =>
      n

fun apply_surface_fun (f, v) =
  case !apply(f, v) of
    [VFloat n, VFloat ks, VFloat kd,
     VPoint (VFloat cr, VFloat cg, VFloat cb)] =>
       (n, ks, kd, cr, cg, cb)
  | _ =>
      failwith "A surface function returns some incorrect values"

fun trace (desc: desc, depth: int, orig, dir) =
   let
      val dir = normalize dir
   in
      case filter_inter_list (find_all(orig, dir, #scene desc)) of
         [] => black
       | (t, ob, _, _) :: _ => trace_2(desc, depth, orig, dir, t, ob)
   end

and trace_2 (desc, depth: int, orig, dir, t, obj) =
   let
      val x = add_scaled (orig, t, dir)
   in
      case obj of
         SObj (kind, f, m) =>
            let
               val x' = vmul (m, x)
               val (n, ks, kd, cr, cg, cb) =
                  (case !f of
                      Unopt g =>
                         (* First we check whether the function would fail *)
                         let
                            val res = apply_surface_fun(g, texture_coord(kind, x'))
                            fun stuck() = f := Opt (!inline_closure g)
                         in
                            (* Then, we check whether it is a constant function *)
                            ((ignore (apply_surface_fun(g, 
                                      [VInt 0, VInt 0, VFloat 0.0]))
                              ; f := Cst res)
                             handle Stuck_computation _ => stuck()
                                  | Stuck_computation' => stuck())
                            ; res
                         end
                    | Opt g =>
                         apply_surface_fun (g, texture_coord (kind, x'))
                    | Cst res =>
                         res)
               val nm = normal (kind, m, x', x)
               val p = prod (dir, nm)
               val nm = if p > 0.0 then neg nm else nm
               val p = ~(abs_float p)
               (* Ambient composant *)
               val (ar, ag, ab) = #amb desc
               val r = ref (kd * ar)
               val g = ref (kd * ag)
               val b = ref (kd * ab)
                  (* Lights *)
               val lights = #lights desc
               val _ =
                  for(0, Array.length lights - 1, fn i =>
                      case (Array.sub(lights, i)) of
                         Light (ldir, (lr, lg, lb)) =>
                            let
                               val p' = prod (ldir, nm)
                            in
                               if p' > 0.0 andalso visible (desc, x, ldir, false)
                                  then
                                     let
                                        val int =
                                           if ks > epsilon then
                                              kd * p' +
                                              ks * prod (normalize
                                                         (sub (ldir, dir)),
                                                         nm) ** n
                                           else
                                              kd * p'
                                     in
                                        r := !r + int * lr;
                                        g := !g + int * lg;
                                        b := !b + int * lb
                                     end
                               else ()
                            end
                       | PtLight (src, (lr, lg, lb)) =>
                            let
                               val ldir = sub (src, x)
                               val ldir' = normalize ldir
                               val p' = prod (ldir', nm)
                            in
                               if p' > 0.0 andalso visible(desc, x, ldir, true)
                                  then
                                     let
                                        val int =
                                           if ks > epsilon
                                              then
                                                 kd * p' +
                                                 ks * prod (normalize (sub (ldir', dir)),
                                                            nm) ** n
                                           else
                                              kd * p'
                                        val int = 100.0 * int / (99.0 + square ldir)
                                     in
                                        r := !r + int * lr;
                                        g := !g + int * lg;
                                        b := !b + int * lb
                                     end
                               else ()
                            end
                       | StLight (src, maindir, (lr, lg, lb), cutoff, exp) =>
                            let
                               val ldir = sub (src, x)
                               val ldir' = normalize ldir
                               val p' = prod (ldir', nm)
                               val p'' = prod (ldir', maindir)
                            in
                               if p' > 0.0 andalso p'' > cutoff
                                  andalso visible(desc, x, ldir, true)
                                  then
                                     let
                                        val int =
                                           if ks > epsilon
                                              then
                                                 kd * p' +
                                                 ks * prod (normalize (sub(ldir', dir)),
                                                            nm) ** n
                                           else
                                              kd * p'
                                        val int =
                                           100.0 * int / (99.0 + square ldir) *
                                           (p'' ** exp)
                                     in
                                        r := !r + int * lr;
                                        g := !g + int * lg;
                                        b := !b + int * lb
                                     end
                               else ()
                            end)
               val _ =
                  (* Reflexion *)
                  if ks > epsilon  andalso depth > 0
                     then
                        let
                           val dir' = add_scaled (dir, ~2.0 * p, nm)
                           val (r', g', b') = trace(desc, depth - 1, x, dir')
                        in
                           r := !r + ks * r';
                           g := !g + ks * g';
                           b := !b + ks * b'
                        end
                  else ()
            in (!r * cr, !g * cg, !b * cb)
            end
       | _ => raise(Fail "assert false")
   end

fun conv c : int =
   let
      val i = truncate (c * 256.0)
   in
      if i < 0 then 0 else
         if i >= 256 then 255 else
            i
   end

fun f (amb, lights, obj, depth: int, fov, wid, ht, file) =
   let
      val scene = intern_obj(Matrix.identity, Matrix.identity, 1.0, true, obj)
      val scene = optimize scene
      val img = Ppm.init (wid, ht)
      val orig = ( 0.0, 0.0, ~1.0, 1.0 )
      val width = 2.0 * dtan (0.5 * fov)
      val delta = width / float wid
      val x0 = ~ width / 2.0
      val y0 = delta * float ht / 2.0
      val desc = { amb = amb, lights = intern_lights lights, scene = scene }
   in
      for(0, ht - 1, fn j =>
          for(0, wid - 1, fn i =>
              let
                 val dir =
                    (x0 + (float i + 0.5) * delta,
                     y0 - (float j + 0.5) * delta,
                     1.0,
                     0.0)
                 val (r, g, b) = trace(desc, depth, orig, dir)
              in
                 Ppm.setp (img, i, j, conv r, conv g, conv b)
              end))
      ; Ppm.dump (file, img)
   end

end
signature EVAL =
   sig
      val f : Program.t list -> unit
   end
structure Eval: EVAL =
struct

open Caml
open Program

val rtd = 180.0 / acos (~1.0)
val dtr = acos (~1.0) / 180.0
fun deg x = rtd * x
fun rad x = dtr * x
val zero = VFloat 0.0
val one = VFloat 1.0

fun lookup (env, s) : int =
  case env of
     [] => failwith ("Unbound variable \"" ^ s ^ "\"")
   | s' :: env' =>
        if s = s'
           then 0
        else 1 + (lookup(env', s))

(* XXX embed values *)
fun conv (absenv, p) =
   case p of
      [] => []
    | Float x :: Float y :: Float z :: Prim Point :: r =>
         Val' (VPoint (VFloat x, VFloat y, VFloat z)) :: conv(absenv, r)
    | t :: r =>
         (case t of
             Fun p' => Fun' (conv(absenv, p')) :: conv(absenv, r)
           | Arr p' => Arr' (conv(absenv, p')) :: conv(absenv, r)
           | Ident s => Ident' (lookup(absenv, s)) :: conv(absenv, r)
           | Binder s => Binder' :: conv (s :: absenv, r)
           | Int i => Val' (VInt i) :: conv(absenv, r)
           | Float f => Val' (VFloat f) :: conv(absenv, r)
           | Bool b => Val' (VBool b) :: conv(absenv, r)
           | String s => Val' (VStr s) :: conv(absenv, r)
           | Prim k => Prim' k :: conv(absenv, r))

fun inline (offset, env, p) =
   case p of
      [] => []
    | t :: r =>
         let
            fun normal() = t :: inline(offset, env, r)
         in case t of
            Fun' p' => Fun' (inline(offset, env, p')) :: inline(offset, env, r)
          | Arr' p' => Arr' (inline(offset, env, p')) :: inline(offset, env, r)
          | Ident' i =>
               if i >= offset
                  then Val' (List.nth (env, i - offset)) :: inline(offset, env, r)
               else normal()
          | Binder' => Binder' :: inline (1 + offset, env, r)
          | Prim' _ => normal()
          | Val' _ => normal()
         end

val inline_closure =
   fn (VClos (env, p)) => VClos ([], inline(0, env, p))
    | _ => failwith "a surface function was actually not a function"

val _ = Render.inline_closure := inline_closure

fun eval (env, st, p) =
  case (st, p) of
(* inlined value *)
    (_, Val' v :: r) => eval(env, (v :: st), r)
(* Rule 1 *)
(* Rule 2 *)
  | (v::st', Binder' :: r) => eval((v :: env), st', r)
(* Rule 3 *)
  | (_, Ident' i :: r) =>
      let val v = List.nth(env, i)
      in eval(env, (v :: st), r)
      end
(* Rule 4 *)
  | (_, Fun' f :: r) => eval(env, (VClos (env, f) :: st), r)
(* Rule 5 *)
  | (VClos (env', f) :: st', Prim' Apply :: r) =>
      eval(env, eval(env', st', f), r)
(* Rule 6 *)
  | (_, Arr' a :: r) =>
      eval(env, (VArr (Array.of_list (List.rev (eval(env, [], a))))) :: st, r)
(* Rules 7 and 8 *)
  | (VClos _ :: VClos (env', iftrue) :: VBool true :: st', Prim' If :: r) =>
      eval(env, eval(env', st', iftrue), r)
  | (VClos (env', iffalse) :: VClos _ :: VBool false :: st', Prim' If :: r) =>
      eval(env, eval(env', st', iffalse), r)
(* Operations on numbers *)
  | (VInt n2 :: VInt n1 :: st', Prim' Addi :: r) =>
       eval(env, (VInt (n1 + n2) :: st'), r)
  | (VFloat f2 :: VFloat f1 :: st', Prim' Addf :: r) =>
      eval(env, (VFloat (f1 + f2) :: st'), r)
  | (VFloat f :: st', Prim' Acos :: r) =>
       eval(env, (VFloat (deg (acos f)) :: st'), r)
  | (VFloat f :: st', Prim' Asin :: r) =>
       eval(env, (VFloat (deg (asin f)) :: st'), r)
  | ((vf as VFloat f):: st', Prim' Clampf :: r) =>
      let val f' = if f < 0.0 then zero else if f > 1.0 then one else vf
      in eval(env, (f' :: st'), r)
      end
  | (VFloat f :: st', Prim' Cos :: r) =>
       eval(env, (VFloat (cos (rad f)) :: st'), r)
  | (VInt n2 :: VInt n1 :: st', Prim' Divi :: r) =>
       eval(env, (VInt (n1 div n2) :: st'), r)
  | (VFloat f2 :: VFloat f1 :: st', Prim' Divf :: r) =>
      eval(env, (VFloat (f1 / f2) :: st'), r)
  | (VInt n2 :: VInt n1 :: st', Prim' Eqi :: r) =>
       eval(env, (VBool (n1 = n2) :: st'), r)
  | (VFloat f2 :: VFloat f1 :: st', Prim' Eqf :: r) =>
      eval(env, (VBool (Real.==(f1, f2)) :: st'), r)
  | (VFloat f :: st', Prim' Floor :: r) =>
      eval(env, (VInt (Real.floor f) :: st'), r)
  | (VFloat f :: st', Prim' Frac :: r) =>
       eval(env, (VFloat (Real.realMod f) :: st'), r)
  | (VInt n2 :: VInt n1 :: st', Prim' Lessi :: r) =>
      eval(env, (VBool (n1 < n2) :: st'), r)
  | (VFloat f2 :: VFloat f1 :: st', Prim' Lessf :: r) =>
      eval(env, (VBool (f1 < f2) :: st'), r)
  | (VInt n2 :: VInt n1 :: st', Prim' Modi :: r) =>
      eval(env, (VInt (n1 mod n2) :: st'), r)
  | (VInt n2 :: VInt n1 :: st', Prim' Muli :: r) =>
       eval(env, (VInt (n1 * n2) :: st'), r)
  | (VFloat f2 :: VFloat f1 :: st', Prim' Mulf :: r) =>
      eval(env, (VFloat (f1 * f2) :: st'), r)
  | (VInt n :: st', Prim' Negi :: r) => eval(env, (VInt (~ n) :: st'), r)
  | (VFloat f :: st', Prim' Negf :: r) => eval(env, (VFloat (~ f) :: st'), r)
  | (VInt n :: st', Prim' Real :: r) => eval(env, (VFloat (float n) :: st'), r)
  | (VFloat f :: st', Prim' Sin :: r) => eval(env, (VFloat (sin (rad f)) :: st'), r)
  | (VFloat f :: st', Prim' Sqrt :: r) => eval(env, (VFloat (sqrt f) :: st'), r)
  | (VInt n2 :: VInt n1 :: st', Prim' Subi :: r) => eval(env, (VInt (n1 - n2) :: st'), r)
  | (VFloat f2 :: VFloat f1 :: st', Prim' Subf :: r) =>
      eval(env, (VFloat (f1 - f2) :: st'), r)
(* Operations on points *)
  | (VPoint (x, _, _) :: st', Prim' Getx :: r ) => eval(env, (x :: st'), r)
  | (VPoint (_, y, _) :: st', Prim' Gety :: r ) => eval(env, (y :: st'), r)
  | (VPoint (_, _, z) :: st', Prim' Getz :: r ) => eval(env, (z :: st'), r)
  | ((z as VFloat _) :: (y as VFloat _) :: (x as VFloat _) :: st',
     Prim' Point :: r) =>
      eval(env, (VPoint (x, y, z) :: st'), r)
  | (VInt i :: VArr a :: st', Prim' Get :: r) =>
      (* if compiled of "-unsafe" *)
      if i < 0 orelse i >= Array.length a
      then failwith "illegal access beyond array boundary"
      else eval(env, (Array.sub(a, i) :: st'), r)
  | (VArr a :: st', Prim' Length :: r) =>
      eval(env, (VInt (Array.length a) :: st'), r)
(* Geometric primitives *)
  | ((f as VClos _) :: st', Prim' Sphere :: r  ) =>
      eval(env, (VObj (OObj (OSphere, ref (Unopt f))) :: st'), r)
  | ((f as VClos _) :: st', Prim' Cube :: r    ) =>
      eval(env, (VObj (OObj (OCube, ref (Unopt f)))   :: st'), r)
  | ((f as VClos _) :: st', Prim' Cylinder :: r) =>
      eval(env, (VObj (OObj (OCylind, ref (Unopt f))) :: st'), r)
  | ((f as VClos _) :: st', Prim' Cone :: r    ) =>
      eval(env, (VObj (OObj (OCone, ref (Unopt f)))   :: st'), r)
  | ((f as VClos _) :: st', Prim' Plane :: r   ) =>
      eval(env, (VObj (OObj (OPlane, ref (Unopt f)))  :: st'), r)
(* Transformations *)
  | (VFloat z :: VFloat y :: VFloat x :: VObj ob :: st', Prim' Translate :: r) =>
      eval(env,
        (VObj (OTransform (ob,
                           Matrix.translate (x, y, z),
                           Matrix.translate (~ x, ~ y, ~ z),
                           1.0, true)) :: st'),
        r)
  | (VFloat z :: VFloat y :: VFloat x :: VObj ob :: st', Prim' Scale :: r) =>
       eval( env,
        (VObj (OTransform (ob,
                           Matrix.scale (x, y, z),
                           Matrix.unscale (x, y, z),
                           Real.max (abs_float x,
                                     (Real.max (abs_float y, abs_float z))),
                           false)) :: st'),
        r)
  | (VFloat s :: VObj ob :: st', Prim' Uscale :: r) =>
      eval(env,
        (VObj (OTransform (ob, Matrix.uscale s, Matrix.unuscale s,
                           abs_float s, true)) :: st'),
        r)
  | (VFloat t :: VObj ob :: st', Prim' Rotatex :: r) =>
      eval(env,
        (VObj (OTransform (ob, Matrix.rotatex t, Matrix.rotatex (~ t),
                           1.0, true)) :: st'),
        r)
  | (VFloat t :: VObj ob :: st', Prim' Rotatey :: r) =>
      eval(env,
        (VObj (OTransform (ob, Matrix.rotatey t, Matrix.rotatey (~ t),
                           1.0, true)) :: st'),
        r)
  | (VFloat t :: VObj ob :: st', Prim' Rotatez :: r) =>
      eval(env,
        (VObj (OTransform (ob, Matrix.rotatez t, Matrix.rotatez (~ t),
                           1.0, true)) :: st'),
        r)
(* Lights *)
  | ((color as VPoint _) :: (dir as VPoint _) :: st', Prim' Light :: r) =>
      eval(env, (VLight (dir, color) :: st'), r)
  | ((color as VPoint _) :: (pos as VPoint _) :: st', Prim' Pointlight :: r) =>
      eval(env, (VPtLight (pos, color) :: st'), r)
  | ((expon as VFloat _) :: (cutoff as VFloat _) :: (color as VPoint _) ::
    (at as VPoint _) :: (pos as VPoint _) :: st', Prim' Spotlight :: r) =>
      eval(env, (VStLight (pos, at, color, cutoff, expon) :: st'), r)
(* Constructive geometry *)
  | ((VObj o2) :: (VObj o1) :: st', Prim' Union :: r) =>
      eval(env, (VObj (OUnion (o1, o2)) :: st'), r)
  | ((VObj o2) :: (VObj o1) :: st', Prim' Intersect :: r) =>
      eval(env, (VObj (OInter (o1, o2)) :: st'), r)
  | ((VObj o2) :: (VObj o1) :: st', Prim' Difference :: r) =>
      eval(env, (VObj (ODiff (o1, o2)) :: st'), r)
(* Rendering *)
  | (VStr file :: VInt ht :: VInt wid :: VFloat fov :: VInt depth ::
    VObj obj :: VArr lights :: VPoint (VFloat ax, VFloat ay, VFloat az) ::
    st', Prim' Render :: r) =>
(*
amb the intensity of ambient light (a point). 
lights is an array of lights used to illuminate the scene. 
obj is the scene to render. 
depth is an integer limit on the recursive depth of the ray tracing. 
fov is the horizontal field of view in degrees (a real number). 
wid is the width of the rendered image in pixels (an integer). 
ht is the height of the rendered image in pixels (an integer). 
file is a string specifying output file for the rendered image. 
*)
    (Render.f ((ax, ay, az), lights, obj, depth, fov, wid, ht, file)
     ; eval(env, st', r))
(* Termination *)
  | (_, []) => st
(* Failure *)
  | _ =>
      raise (Stuck_computation (env, st, p))

fun apply (f, st) =
  case f of
    VClos (env, p) => eval(env, st, p)
  | _ => raise Fail "assert false"

val _ = Render.apply := apply

fun f p =
   let
      val st = eval([], [], (conv([], p)))
   in
      case st of
         [] => ()
       | _ => failwith "error"
   end handle Stuck_computation (env, st, p) => failwith "stuck"
      
end
structure Main =
   struct
      fun doit () =
         Eval.f (Program.read (TextIO.openIn "DATA/chess.gml"))
         handle _ => ()

      val doit =
         fn n =>
         let
            fun loop n =
               if n = 0
                  then ()
               else (doit();
                     loop(n-1))
         in loop n
         end
   end
