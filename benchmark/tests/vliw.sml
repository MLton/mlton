(* From the SML/NJ benchmark suite. *)

fun print _ = ()
   
signature BMARK =
  sig
    val doit : int -> unit
    val testit : TextIO.outstream -> unit
  end;

open Array (* List *)
infix 9 sub

fun fold f x y = List.foldr f y x
fun revfold f x y = List.foldl f y x
val makestring = Int.toString

local 
open Real
in
val realEq = ==
val realNe = !=
end

exception NotAChar
fun fromStr x = 
  (case Char.fromString x
    of SOME c => c
     | NONE => raise NotAChar)

fun ordof(s, i) = Char.ord(String.sub(s, i))


val explode = (fn x => map Char.toString (explode x))
val implode = (fn x => implode (map fromStr x))
fun ord s = Char.ord (fromStr s)

val output = TextIO.output
val std_out = TextIO.stdOut
val open_in = TextIO.openIn
val open_out = TextIO.openOut
val close_in = TextIO.closeIn
val close_out = TextIO.closeOut
val input_line =
   fn ins =>
   case TextIO.inputLine ins of
      NONE => ""
    | SOME s => s
type instream = TextIO.instream
type outstream = TextIO.outstream
fun outputc f x = TextIO.output(f, x)

exception NotAReal

fun strToReal s = 
  (case Real.fromString s
    of SOME r => r
     | _ => raise NotAReal)

fun intToReal x = 
  (strToReal ((Int.toString x) ^ ".0"))

structure Bits = 
struct

fun wrap (f : Word.word * Word.word -> Word.word)
  = (fn (x : int, y : int) => 
       Word.toInt(f(Word.fromInt x, Word.fromInt y)))

val orb  = wrap Word.orb
val andb = wrap Word.andb
val xorb = wrap Word.xorb
val lshift = wrap Word.<<
val rshift = wrap Word.>>

end
structure Ref = 
struct
  val inc = fn x => (x := !x + 1)
  val dec = fn x => (x := !x - 1)
end
 
(* stringmap.sml *)

signature STRINGMAP =
  sig type 'a stringmap
      exception Stringmap
      val new : unit -> 'a stringmap
      val add : 'a stringmap -> string * 'a -> unit
      val rm  : 'a stringmap -> string -> unit
      val map : 'a stringmap -> string -> 'a
      val app : (string * 'a -> unit) -> 'a stringmap -> unit
      val isin : 'a stringmap -> string -> bool
      val extract : 'a stringmap -> 'a list
  end

structure Stringmap : STRINGMAP =
struct
  type 'a stringmap = (string * 'a) list array
  exception Stringmap
  val hashFactor = 32
  and tableSize = 2357

  (* a string hashing function
     returns a number between 0 and tableSize-1 *)
  fun hash(str: string) : int =
      let val nchars = String.size str

          fun loop(i,n,r) = 
            if i < n then 
             loop(i+1,n,(hashFactor * r + ordof(str,i)) mod tableSize)
            else r

       in loop(0,nchars,0)
(*        while !i < nchars do
              (n := (hashFactor * !n + ordof(str, !i)) mod tableSize;
               i := !i + 1);
          !n
*)
      end

  (* create a new stringmap *)
  fun new (): 'a stringmap = array(tableSize,nil)

  (* add a mapping pair s +-> x to the stringmap a *)
  fun add a (s,x) = 
    let val index = hash s
     in update(a,index,(s,x)::(a sub index))
    end

  (* apply the stringmap a to the index string s *)
  fun map a s = 
    let fun find ((s',x)::r) = if s=s' then x else find r
          | find nil = raise Stringmap
     in find (a sub (hash s))
    end

  (* return true if the string is in the map, false otherwise *)
  fun isin a s =
      ((map a s; true)
       handle Stringmap => false)

  (* remove all pairs mapping string s from stringmap a *)
  fun rm a s = let fun f ((b as (s',j))::r) = 
                                if s=s' then f r else b :: f r
                      | f nil = nil
                    val index = hash s
                 in update(a,index, f(a sub index))
                end

  (* apply a function f to all mapping pairs in stringmap a *)
  fun app (f: string * 'a -> unit) a =
      let fun zap 0 = ()
            | zap n = let val m = n-1 in List.app f (a sub m); zap m end
      in  zap tableSize
      end

  (* extract the stringmap items as a list *)
  fun extract a =
      let fun atol n =
          if n < Array.length a then (a sub n) :: atol (n + 1)
          else nil
          val al = atol 0
          fun flatten (a, b) = a @ b
          val fal = fold flatten al nil
          fun strip (s, v) = v
          val answer = List.map strip fal
      in
          answer
      end

end  (* Stringmap *)



structure StrPak :
    sig
        val stringListString : string list -> string
    end = 

struct

fun sl nil = "]"
  | sl (h::nil) = h ^ "]"
  | sl (h::n::t) = h ^ "," ^ sl (n::t)

fun stringListString l = "[" ^ sl l

end
signature SortObjSig = 
    sig
        type obj
        val gt : obj * obj -> bool
    end

functor Sort ( objfun : SortObjSig ) :
    sig
        type obj
        val sort : obj list -> obj list
    end = 

struct

open objfun

type obj = objfun.obj
        
fun sort l =
    let fun m2 (nil, b) = b
          | m2 (a, nil) = a
          | m2 (ha::ta, hb::tb) =
            if gt(ha, hb) then hb::(m2(ha::ta, tb))
            else ha::(m2(ta, hb::tb))
        fun ml (nil) = nil
          | ml (h::nil) = h
          | ml (h1::h2::nil) = m2(h1, h2)
          | ml (h1::h2::l) = ml [m2(h1, h2), (ml l)]
    in
        ml (map (fn x => [x]) l)
    end

end

structure IntImp =
    struct
        type obj = int
        fun gt(a:obj, b:obj) = a > b
    end
                           

structure INTSort = Sort ( IntImp )

structure Set :
    sig
        exception SET
        exception LISTUNION
        type 'a set
        val make : ''a set
        val makeEQ : ('a * 'a -> bool) -> 'a set
        val listToSet : ''a list -> ''a set
        val listToSetEQ : ('a * 'a -> bool) * 'a list -> 'a set
        val add : 'a set * 'a -> 'a set
        val union : 'a set * 'a set -> 'a set
        val listUnion : 'a set list -> 'a set
        val listUnionEQ : ('a * 'a -> bool) * 'a set list -> 'a set
        val rm : 'a set * 'a -> 'a set
        val intersect : 'a set * 'a set -> 'a set
        val diff : 'a set * 'a set -> 'a set
        val member : 'a set * 'a -> bool
        val set : 'a set -> 'a list
        val mag : 'a set -> int
        val empty : 'a set -> bool
    end = 
struct
datatype 'a set = S of ('a*'a->bool) * 'a list

exception SET
exception LISTUNION

fun eqf (x, y) = x = y

val make = S (eqf, nil)

fun makeEQ eqf = S (eqf, nil)

fun set (S (eqf, a)) = a

fun member (S (eqf, nil), e) = false
  | member (S (eqf, (s::t)), e) = eqf(e, s) orelse member(S (eqf, t), e)

fun add(st as (S (eqf, s)), e) = if member(st, e) then st else S(eqf, e::s)

fun listToSetEQ (eqf, l) =
    let fun f (nil, s) = s
          | f (h::t, s) = f(t, add(s, h))
    in
        f(l, makeEQ eqf)
    end

fun listToSet l = listToSetEQ (eqf, l)


fun union (a, S (eqf, nil)) = a
  | union (S (eqf, nil), b) = b
  | union (S (eqf, e::a), b) = union(S (eqf, a), add(b, e))

fun listUnion (h::t) = fold union t h
  | listUnion _ = raise LISTUNION

fun listUnionEQ (eqf, l) = fold union l (makeEQ eqf)


fun rm (S (eqf, nil), x) = raise SET
  | rm (S (eqf, s::t), x) =
    if eqf(s, x) then S (eqf, t) else S(eqf, s :: set(rm(S (eqf, t), x)))

fun intersect1 (a, S (eqf, nil), c) = S (eqf, c)
  | intersect1 (S (eqf, nil), b, c) = S (eqf, c)
  | intersect1 (S (eqf, a::t), b, c) =
    if member(b, a) then intersect1(S (eqf, t), b, a::c)
    else intersect1(S (eqf, t), b, c)

fun intersect (a, b) = intersect1 (a, b, nil)

fun diff (S (eqf, nil), b) = S (eqf, nil)
  | diff (S (eqf, a::t), b) = if member(b, a) then diff(S (eqf, t), b)
                         else S (eqf, a :: set(diff(S (eqf, t), b)))


fun mag s = List.length (set s)

(* fun empty s = set s = nil *)

fun empty (S(eqf, nil)) = true
  | empty (S(eqf, _)) = false

end
(* Copyright 1989 by AT&T Bell Laboratories *)
(* updated by John Danskin at Princeton *)
structure AbsMach =
struct
  type reg = (int*string)
  type label = (int*string)
  datatype values =
      INT of int
    | REAL of real
    | LABVAL of int * int
        
  datatype arithop = imul | iadd | isub | idiv 
                  | orb | andb | xorb | rshift | lshift
                   | fadd | fdiv | fmul | fsub
                  | real | floor | logb

  datatype comparison = ilt | ieq | igt | ile | ige | ine
                      | flt | feq | fgt | fle | fge | fne 
                      | inrange | outofrange
  datatype opcode =
      FETCH of {immutable: bool, offset: int, ptr: reg, dst: reg}
                (* dst := M[ptr+offset]
                   if immutable then unaffected by any STORE
                   other than through the allocptr *)
    | STORE of {offset: int, src: reg, ptr: reg}
                (* M[ptr+offset] := src *)
    | GETLAB of {lab: label, dst: reg}
    | GETREAL of {value: string, dst: reg}
    | ARITH of {oper: arithop, src1: reg, src2: reg, dst: reg}
    | ARITHI of {oper: arithop, src1: reg, src2: int, dst: reg}
    | MOVE of {src: reg, dst: reg}
    | BRANCH of {test: comparison, src1: reg, src2: reg, dst: label, 
                 live: reg list}
    | JUMP of {dst: reg, live: reg list}
    | LABEL of {lab:label, live: reg list}
    | WORD of {value: int}
    | LABWORD of {lab: label}
    | NOP
    | BOGUS of {reads: reg list, writes: reg list}

  val opcodeEq : opcode * opcode -> bool = (op =)

end

structure AbsMachImp :
    sig
        type reg
        type operation
        val oeq : operation * operation  -> bool
        type comparison
        val ceq : comparison * comparison -> bool
        val write_o : operation -> reg Set.set
        val read_o : operation -> reg Set.set
        val write_c : comparison -> reg Set.set
        val read_c : comparison -> reg Set.set
        val resources_ok : operation list * comparison list -> bool
        datatype codetypes =
            ASSIGNMENT of operation
          | LABELREF of int * operation
          | COMPARISON of int * operation
          | FLOW of int * operation
          | TARGET of int * operation
          | EXIT of operation
          | JUNK of operation
          | NERGLE
        val classify : operation -> codetypes
        val maxreg : AbsMach.opcode list -> int
    end =
struct

type reg = int (* register strings will gum up set operations etc *)
type operation = AbsMach.opcode
type comparison = AbsMach.opcode

fun oeq (a, b) = AbsMach.opcodeEq(a, b)
fun ceq (a, b) = AbsMach.opcodeEq(a, b)

fun reg(i, s) = i
fun label(i, s) = i


fun srl rl = Set.listToSet((map reg) rl)
fun sr r = srl [r]

val immutableMem = ~1
val mutableMem = ~2
val flowControl = ~3

(* comparisons are limited to one because of difficulty writing larger trees *)
fun resources_ok(ops, c) = (List.length ops) <= 4 andalso (List.length c) <= 1

fun allocptr r = reg r = 1

fun write_o i = 
    let open Set
        open AbsMach
        val f =
            fn FETCH{dst, ...} => sr dst
             | STORE{ptr, ...} =>
                   if allocptr ptr then listToSet [immutableMem, mutableMem]
                   else listToSet [mutableMem]
             | GETLAB {dst, ...} => sr dst
             | GETREAL {dst, ...} => sr dst
             | ARITH {dst, ...} => sr dst
             | ARITHI {dst, ...} => sr dst
             | MOVE {dst, ...} => sr dst
             | JUMP _  => listToSet [flowControl]
             | BOGUS {writes, ...} => srl writes
             | _  =>  make
    in
        f i
    end

fun write_c c = Set.listToSet [flowControl]

val std_reg_list = [(1, ""), (2, ""), (3, ""), (4, ""), (5, "")]
                   
fun read i =
    let open Set
        open AbsMach
        val f =
        fn FETCH {immutable, ptr, ...} =>
        let val mem = if immutable then immutableMem else mutableMem
        in
            add(sr ptr, mem)
        end
         | STORE {src, ptr, ...} => srl [src, ptr]
         | ARITH {src1, src2, ...} => srl [src1, src2]
         | ARITHI {src1, ...} => sr src1
         | MOVE {src, ...} => sr src
         | BRANCH {src1, src2, ...} => srl [src1, src2]
         | JUMP {dst, ...} => srl (dst :: std_reg_list)
         | BOGUS {reads, ...} => srl reads
         | _ => make
    in
        f i
    end

fun read_o i = read i
fun read_c i = read i

datatype codetypes =
    ASSIGNMENT of operation
  | LABELREF of int * operation
  | COMPARISON of int * operation
  | FLOW of int * operation
  | TARGET of int * operation
  | EXIT of operation
  | JUNK of operation
  | NERGLE

fun maxreg li =
    let fun f (a, b) = Int.max(a, b)
        val r =
            (Set.set (Set.listUnion((map write_o li) @
                                    (map read li))))
    in
        fold f r 0
    end


fun classify i =
    let open AbsMach
        val f =
        fn FETCH _ => ASSIGNMENT i
         | STORE _ => ASSIGNMENT i
         | GETLAB{lab, dst} => LABELREF(label lab, i)
         | GETREAL _  => ASSIGNMENT i
         | ARITH _  =>  ASSIGNMENT i
         | ARITHI _ =>  ASSIGNMENT i
         | MOVE{src, dst} =>
               if reg src = reg dst then NERGLE
               else ASSIGNMENT i
         | BRANCH{test,src1,src2,dst,live} =>
               if test = ieq andalso (reg src1) = (reg src2)
                   then FLOW (label dst, i)
               else COMPARISON (label dst, i)
         | JUMP _ => EXIT i
         | LABEL {lab, ...} => TARGET(label lab, i)
         | WORD _ => JUNK i
         | LABWORD _ => JUNK i
         | NOP =>  JUNK i
         | BOGUS _ =>  ASSIGNMENT i
    in
        f i
    end
end
structure ReadAbs : sig val read: instream -> AbsMach.opcode list end =
struct

open AbsMach

exception ReadError

fun readline(i,f) =
let
    
    fun error s = (print("Error in line "^makestring i^": "^s^"\n");
                   raise ReadError)

fun b(" "::rest) = b rest | b rest = rest

val aop =
 fn "i"::"m"::"u"::"l"::l => (imul,l)
  | "i"::"a"::"d"::"d"::l => (iadd,l)
  | "i"::"s"::"u"::"b"::l => (isub,l)
  | "i"::"d"::"i"::"v"::l => (idiv,l)
  | "o"::"r"::"b"::" "::l=> (orb,l)
  | "a"::"n"::"d"::"b"::l => (andb,l)
  | "x"::"o"::"r"::"b"::l => (xorb,l)
  | "r"::"s"::"h"::"i"::"f"::"t"::l => (rshift,l)
  | "l"::"s"::"h"::"i"::"f"::"t"::l => (lshift,l)
  | "f"::"a"::"d"::"d"::l => (fadd,l)
  | "f"::"d"::"i"::"v"::l => (fdiv,l)
  | "f"::"m"::"u"::"l"::l => (fmul,l)
  | "f"::"s"::"u"::"b"::l => (fsub,l)
  | "r"::"e"::"a"::"l"::l => (real,l)
  | "f"::"l"::"o"::"o"::"r"::l => (floor,l)
  | "l"::"o"::"g"::"b"::l => (logb,l)
  | _ => error "illegal arithmetic operator"

val com =
  fn "i"::"l"::"t"::l => (ilt,l)
   | "i"::"e"::"q"::l => (ieq,l)
   | "i"::"g"::"t"::l => (igt,l)
   | "i"::"l"::"e"::l => (ile,l)
   | "i"::"g"::"e"::l => (ige,l)
   | "i"::"n"::"e"::l => (ine,l)
   | "f"::"l"::"t"::l => (flt,l)
   | "f"::"e"::"q"::l => (feq,l)
   | "f"::"g"::"t"::l => (fgt,l)
   | "f"::"l"::"e"::l => (fle,l)
   | "f"::"g"::"e"::l => (fge,l)
   | "f"::"n"::"e"::l => (fne,l)
   | "i"::"n"::"r"::"a"::"n"::"g"::"e"::l => (inrange,l)
   | "o"::"u"::"t"::"o"::"f"::"r"::"a"::"n"::"g"::"e"::l => (outofrange,l)
   | _ => error "illegal comparison operator"

fun immut("i"::l) = (true,l) | immut("m"::l) = (false,l) 
  | immut _ = error "i or m required"

fun int l =
  let val z = ord "0"
      fun f(n,l0 as d::l) = if d>="0" andalso d<="9"
                              then f(n*10+ord(d)-z, l)
                            else (n,l0)
        | f _ = error "in readabs.int"
   in f(0,l)
  end

fun string l =
  let fun f("/"::l) = (nil,l)
        | f(a::l) = let val (s,l') = f l
                     in (a::s, l')
                    end
        | f _ = error "name not terminated by \"/\""
      val (s,l') = f l
   in (implode s, l')
  end

  fun realc s =
    let val (sign,s) = case explode s of "~"::rest => (~1.0,rest) 
                                       | s => (1.0,s)
        fun j(exp,d::dl,mant) = j(exp,dl,mant * 0.1 + intToReal(d))
          | j(0,nil,mant) = mant*sign
          | j(exp,nil,mant) = if exp>0 then j(exp-1,nil,mant*10.0)
                                       else j(exp+1,nil,mant*0.1)
        fun h(esign,wholedigits,diglist,exp,nil) = 
                          j(esign*exp+wholedigits-1,diglist,0.0)
          | h(es,fd,dl,exp,d::s) = h(es,fd,dl,exp*10+(ord d - ord "0"),s)
        fun g(i,r,"E"::"~"::s)=h(~1,i,r,0,s)
          | g(i,r,"E"::s)=h(1,i,r,0,s)
          | g(i,r,d::s) = if d>="0" andalso d<="9" then
                            g(i, (ord d - ord "0")::r, s)
                          else h(1,i,r,0,nil)
          | g(i,r,nil) = h(1,i,r,0,nil)
        fun f(i,r,"."::s)=g(i,r,s)
          | f(i,r,s as "E"::_)=g(i,r,s)
          | f(i,r,d::s) = f(i+1,(ord(d)-ord("0"))::r,s)
          | f _ = error "bad in readdabs"
     in f(0,nil,s)
    end handle Overflow => error ("real constant "^s^" out of range")

fun require((a:string)::ar, b::br) = if a=b then require(ar,br)
                           else error(a^" required")
  | require(nil, br) = br
  | require(a::_,_) = error(a^" required")

fun reg l = let val (s,l) = string l
                val l = require(["R"],l)
                val (i,l) = int l
             in ((i,s),l)
            end
fun lab l = let val (s,l) = string l
                val l = require(["L"],l)
                val (i,l) = int l
             in ((i,s),l)
            end

fun live l =
 let fun f(")"::_) = nil
       | f l = let val (r,l) = reg l
                in r::f(b l)
               end
  in f(b(require(["("],l)))
 end

val opcode = 
 fn "F"::"E"::"T"::"C"::"H"::l =>
        let val (imm,l) = immut(b l)
            val (dst,l) = reg(b l)
            val (ptr,l) = reg(b(require(["M","["],b(require([":","="],b l)))))
            val (offset,l) = int(b(require(["+"],b l)))
        in require(["]"], b l);
           FETCH{immutable=imm,dst=dst,ptr=ptr,offset=offset}
        end
  | "S"::"T"::"O"::"R"::"E"::l =>
        let val (ptr,l) = reg(b(require(["M","["],b l)))
            val (offset,l) = int(b(require(["+"],b l)))
            val (src,l) = reg(b(require([":","="],b(require(["]"], b l)))))
         in STORE{src=src,ptr=ptr,offset=offset}
        end
  | "G"::"E"::"T"::"L"::"A"::"B"::l =>
        let val (dst,l) = reg(b l)
            val (lab,l) = lab(b(require([":","="],b l)))
        in GETLAB{dst=dst,lab=lab}
        end
  | "G"::"E"::"T"::"R"::"E"::"A"::"L"::l =>
        let val (dst,l) = reg(b l)
            val r = realc(implode(b(require([":","="],b l))))
        in GETREAL{dst=dst,value=Real.toString r}
        end
  | "A"::"R"::"I"::"T"::"H"::"I"::l =>
        let val (dst,l) = reg(b l)
            val (s1,l) = reg(b(require([":","="],b l)))
            val (oper,l) = aop(b l)
            val (s2,l) = int(b l)
        in ARITHI{oper=oper,src1=s1,src2=s2,dst=dst}
        end
  | "A"::"R"::"I"::"T"::"H"::l =>
        let val (dst,l) = reg(b l)
            val (s1,l) = reg(b(require([":","="],b l)))
            val (oper,l) = aop(b l)
            val (s2,l) = reg(b l)
        in ARITH{oper=oper,src1=s1,src2=s2,dst=dst}
        end
  | "M"::"O"::"V"::"E"::l =>
        let val (dst,l) = reg(b l)
            val (s1,l) = reg(b(require([":","="],b l)))
        in MOVE{src=s1,dst=dst}
        end
  | "B"::"R"::"A"::"N"::"C"::"H"::l =>
        let val (s1,l) = reg(b(require(["I","F"],b l)))
            val (test,l) = com(b l)
            val (s2,l) = reg(b l)
            val (dst,l) = lab(b(require(["G","O","T","O"],b l)))
            val liv = live(b l)
        in BRANCH{test=test,src1=s1,src2=s2,dst=dst,live=liv}
        end
  | "J"::"U"::"M"::"P"::l =>
        let val (dst,l) = reg(b l)
            val live = live(b l)
         in JUMP{dst=dst,live=live}
        end
  | "L"::"A"::"B"::"E"::"L"::l =>
        let val (lab,l) = lab(b l)
            val live = live(b(require([":"],l)))
         in LABEL{lab=lab,live=live}
        end
  | "W"::"O"::"R"::"D"::l =>
        let val (i,l) = int(b l)
         in WORD{value=i}
        end
  | "L"::"A"::"B"::"W"::"O"::"R"::"D"::l =>
        let val (i,l) = lab(b l)
         in LABWORD{lab=i}
        end
  | "N"::"O"::"P"::_ => NOP
  | _ => error "illegal opcode name"
in
  case explode(input_line f)
   of nil => nil
    | l => opcode(b l)::readline(i+1,f)
end

fun read f = readline(0,f)

end

structure PrintAbs :
    sig
        val show: outstream -> AbsMach.opcode list -> unit
        val str: AbsMach.opcode list -> string
    end =
struct

open AbsMach

fun xstr prog =

let 

val outstr = ref ""
fun pr s = outstr := !outstr ^ s

val aop = 
 fn imul => "imul"
  | iadd => "iadd"
  | isub => "isub"
  | idiv => "idiv"
  | orb => "orb"
  | andb => "andb"
  | xorb => "xorb"
  | rshift => "rshift"
  | lshift => "lshift"
  | fadd => "fadd"
  | fdiv => "fdiv"
  | fmul => "fmul"
  | fsub => "fsub"
  | real => "real"
  | floor => "floor"
  | logb => "logb"

val com =
  fn ilt => "ilt"
   | ieq => "ieq"
   | igt => "igt"
   | ile => "ile"
   | ige => "ige"
   | ine => "ine"
   | flt => "flt"
   | feq => "feq"
   | fgt => "fgt"
   | fle => "fle"
   | fge => "fge"
   | fne => "fne"
   | inrange => "inrange"
   | outofrange => "outofrange"

fun bo true = "t" | bo false = "f"

fun reg(i,s) = (pr(s); pr "/R"; pr(makestring i))
fun label(i,s) = (pr(s); pr "/L"; pr(makestring i))

val p =
  fn FETCH{immutable,offset,ptr,dst} =>
      (pr "FETCH";
       if immutable then pr "i  " else pr "m  ";
       reg dst; pr " := M[ "; reg ptr;
       pr " + "; pr (makestring offset); pr(" ]\n"))
   | STORE{offset,ptr,src} =>
      (pr "STORE   ";
       pr "M[ "; reg ptr;
       pr " + "; pr (makestring offset); pr(" ] := ");
       reg src;
       pr "\n")
   | GETLAB{lab, dst} =>
      (pr "GETLAB  "; reg dst;
       pr " := "; label lab;
       pr "\n")
   | GETREAL{value,dst} =>
      (pr "GETREAL "; reg dst;
       pr " := ";
       pr value;
       pr "\n")
   | ARITH{oper,src1,src2,dst} =>
      (pr "ARITH   "; reg dst;
       pr " := "; reg src1;
       pr " "; pr(aop oper); pr " "; 
       reg src2;
       pr "\n")
   | ARITHI{oper,src1,src2,dst} =>
      (pr "ARITHI  "; reg dst;
       pr " := "; reg src1;
       pr " "; pr(aop oper); pr " ";
       pr(makestring src2);
       pr "\n")
   | MOVE{src,dst} =>
      (pr "MOVE    "; reg dst;
       pr " := "; reg src;
       pr "\n")
   | BRANCH{test,src1,src2,dst,live} =>
      (pr "BRANCH  ";
       pr "IF "; reg src1;
       pr " ";  pr(com test); pr " ";
       reg src2;
       pr " GOTO ";
       label dst;
       pr "   ( ";
       List.app (fn r => (reg r; pr " ")) live;
       pr ")\n")
   | JUMP{dst,live} =>
      (pr "JUMP    "; reg dst;
       pr "   ( ";
       List.app (fn r => (reg r; pr " ")) live;
       pr ")\n")
   | LABEL{lab, live} =>
      (pr "LABEL   "; label lab;
       pr ":      ( ";
       List.app (fn r => (reg r; pr " ")) live;
       pr ")\n")
   | WORD{value} =>
      (pr "WORD    ";
       pr (makestring value);
       pr "\n")
   | LABWORD{lab} =>
      (pr "LABWORD "; label lab;
       pr "\n")
   | NOP => pr "NOP\n"
   | BOGUS{reads, writes} =>
         (pr "BOGUS";
          pr "   ( ";
          List.app (fn r => (reg r; pr " ")) writes;
          pr ") := (";
          List.app (fn r => (reg r; pr " ")) reads;
          pr ")\n")
  
                         
in (List.app p prog; !outstr)
end

fun str prog =
    let fun cat (a, b) = (xstr [a]) ^ b
    in
        fold cat prog ""
    end

fun show out prog =
    let fun f nil = ()
          | f (h::t) = (outputc out (xstr [h]);
                        f t)
    in
        f prog
    end
    
end


structure HM = AbsMachImp
structure BreakInst :
    sig 
        val breaki : AbsMach.opcode list -> AbsMach.opcode list
    end =
struct

open AbsMach
open HM

val maxreg = AbsMachImp.maxreg

fun reg(i:int, s:string) = i
fun rstr(i:int, s:string) = s

val new_reg_val = ref 0
val new_reg_pairs:(AbsMach.reg * AbsMach.reg) list ref = ref nil

fun new_reg_init li = (new_reg_val := maxreg li;
                       new_reg_pairs := nil)

fun new_reg (r:AbsMach.reg) =
    let fun f nil =
        let val nr = (new_reg_val := !new_reg_val + 1; (!new_reg_val, rstr r))
        in
            (new_reg_pairs := (r, nr) :: !new_reg_pairs;
             nr)
        end
          | f ((a, b)::t) = if r = a then b else f t
    in
        f (!new_reg_pairs)
    end

fun breaki l =
    let fun f i =
        let val g =
            fn ARITH{oper, src1, src2, dst}  =>
            if reg dst = reg src1 orelse reg dst = reg src2 then
                let val nr = new_reg(dst)
                in
                    [ARITH{oper=oper, src1=src2, src2=src2, dst=nr},
                     MOVE{src=nr, dst=dst}]
                end
            else [i]
             | ARITHI{oper, src1, src2, dst}  =>
                   if reg dst = reg src1 then
                       let val nr = new_reg(dst)
                       in
                           [ARITHI{oper=oper, src1=src1, src2=src2, dst=nr},
                            MOVE{src=nr, dst=dst}]
                       end
                   else [i]
             | FETCH{immutable, offset, ptr, dst} =>
                   if reg ptr = reg dst then
                       let val nr = new_reg(dst)
                       in
                           [FETCH{immutable=immutable, offset=offset,
                                  ptr=ptr, dst=nr},
                            MOVE{src=nr, dst=dst}]
                       end
                   else [i]
             | MOVE{src, dst} =>
                   if reg src = reg dst then nil
                   else [i]
             | _ => [i]
        in
            g i
        end
        fun h (a, b) = f a @ b
        val foo = new_reg_init l
    in
        fold h l nil
    end

end
structure OutFilter :
    sig 
        val remnops : AbsMach.opcode list -> AbsMach.opcode list
    end =
struct

open AbsMach

fun remnops ol =
    let fun f (NOP, NOP::b) = NOP::b
          | f (a, b) = a::b
    in
        fold f ol nil
    end

end
structure Delay  :
    sig
        val init: AbsMach.opcode list -> unit
        val add_delay: AbsMach.opcode list ->  AbsMach.opcode list
        val rm_bogus: AbsMach.opcode list -> AbsMach.opcode list
        val is_bogus_i : AbsMach.opcode -> bool
        val is_bogus_reg : AbsMach.reg -> bool
        val idempotency : int ref
    end =
struct

open AbsMach

val maxreg = ref 0
val maxdelay = 12

val idempotency = ref 0

fun is_bogus_i (BOGUS _ ) = true
  | is_bogus_i _ = false

fun bogus_reg ((i, s), which) = (!maxreg + maxdelay * i + which, s)

fun is_bogus_reg (i, s) = i > !maxreg

fun unbogus_reg (i, s) = if is_bogus_reg (i, s) then (i div maxdelay, s)
                         else (i, s)

val max_bog_reg = ref 0
val curr_idem_reg = ref 0

fun idem_reg() =
    (curr_idem_reg := !curr_idem_reg + 1;
     (!curr_idem_reg, "idem"))

fun init il = (
               maxreg := AbsMachImp.maxreg il;
               max_bog_reg := (!maxreg + 1) *  maxdelay;
               curr_idem_reg := !max_bog_reg + 1
               )

exception DELAY

fun delay i =
    let fun opdelay oper =
        let val f =
            fn imul => 5
             | iadd => 2
             | isub => 2
             | idiv => 12
             | orb => 2
             | andb => 2
             | xorb => 2
             | rshift => 2
             | lshift => 2
             | fadd => 2
             | fdiv => 12
             | fmul => 4
             | fsub => 2
             | real => 2
             | floor => 2
             | logb => 2
        in
            f oper
        end
        val id =
            fn FETCH{immutable,offset,ptr,dst} => 2
             | STORE{offset,ptr,src} => 2
             | GETLAB{lab, dst} => 2
             | GETREAL{value,dst} => 2
             | ARITH{oper,src1,src2,dst} => opdelay oper
             | ARITHI{oper,src1,src2,dst} => opdelay oper
             | MOVE{src,dst} => 1
             | BRANCH{test,src1,src2,dst,live} => 5
             | JUMP{dst,live} => 1
             | LABEL{lab, live} => 0
             | NOP => 1
             | _ => raise DELAY
    in
        id i
    end

fun b_idemx (0, r, w) = nil
  | b_idemx (1, r, w) = BOGUS{reads=r @ w, writes = [idem_reg()]} :: nil
  | b_idemx (n, r, w) =
    let val ir = idem_reg()
    in
        BOGUS{reads=r @ w, writes = [ir]} :: b_idemx(n-1, r, [ir])
    end

fun b_idem (n, r, w) =
    let fun fil ((i, s), b) = if i = 0 then b else (i, s) :: b
        val nr = fold fil r nil
    in
        if null nr then nil
        else b_idemx(n, nr, w)
    end
        
fun b_assx (0, r) = nil
  | b_assx (1, r) = BOGUS{reads=[bogus_reg(r, 1)], writes=[r]} :: nil
  | b_assx (n, r) =
    BOGUS{reads=[bogus_reg(r, n)], writes=[bogus_reg(r, n-1)]} ::
    b_assx(n-1, r)

fun b_ass(n, r) = BOGUS{reads=[r], writes=[bogus_reg(r, n-1)]} ::
    b_assx(n-1, r)

fun b_brxx (0, rl) = nil
  | b_brxx (1, rl) =
    let fun b r = bogus_reg(r, 1)
    in
        BOGUS{reads=rl, writes=map b rl} :: nil
    end
  | b_brxx (n, rl) =
    let fun br r = bogus_reg(r, n - 1)
        fun bw r = bogus_reg(r, n)
    in
        BOGUS{reads=map br rl, writes=map bw rl} :: b_brxx (n - 1, rl)
    end

fun b_brx (n, rl) =
    let fun br r = bogus_reg(r, n-1)
    in
        BOGUS{reads=map br rl, writes=rl} :: b_brxx(n-1, rl)
    end
    
fun b_br (b, n, rl) = rev (b :: b_brx(n, rl))

fun is_flow i =
    let open AbsMachImp
        fun f (FLOW _) = true
          | f _ = false
    in
        f (classify i)
    end
    
fun add_delay il =
    let fun idem (r, w) = b_idem (!idempotency, r, w)
        fun g i =
        let val d = delay i
            val f =
                fn FETCH{immutable,offset,ptr,dst} =>
                i :: (idem([ptr], [dst]) @ b_ass(d, dst))
                 | STORE{offset,ptr,src} => [i]
                 | GETLAB{lab, dst} => i :: b_ass(d, dst)
                 | GETREAL{value,dst} => i :: b_ass(d, dst)
                 | ARITH{oper,src1,src2,dst} =>
                       i :: (idem([src1, src2], [dst]) @ b_ass(d, dst))
                 | ARITHI{oper,src1,src2,dst} =>
                       i :: (idem([src1], [dst]) @ b_ass(d, dst))
                 | MOVE{src,dst} => i :: idem([src], [dst])
                 | BRANCH{test,src1,src2,dst,live} =>
                       if is_flow i then [i]
                       else
                           b_br (BRANCH{test=test,
                                        src1=src1,src2=src2,dst=dst,
                                        live=live},
                                 d, [src1, src2])
                 | _  =>  [i]
        in
            f i
        end
        fun apnd (nil, b) = b
          | apnd (a::t, b) = a :: apnd(t, b)
        fun fld(a, b) = apnd(g a, b)
    in
        fold fld il nil
    end

fun rm_bogus il =
    let fun g nil = nil
          | g (i::t) =
        let val f =
            fn FETCH{immutable,offset,ptr,dst} =>
            FETCH{immutable=immutable, offset=offset, ptr=ptr,
                  dst= unbogus_reg dst} ::
            g t
             | STORE{offset,ptr,src} => i :: g t
             | GETLAB{lab, dst} =>
                   GETLAB{lab=lab, dst= unbogus_reg dst} :: g t
             | GETREAL{value,dst} =>
                   GETREAL{value=value, dst=unbogus_reg dst} :: g t
             | ARITH{oper,src1,src2,dst} =>
                   ARITH{oper=oper,src1=src1,src2=src2,dst=unbogus_reg dst} ::
                   g t
             | ARITHI{oper,src1,src2,dst} =>
                   ARITHI{oper=oper,src1=src1,src2=src2,dst=unbogus_reg dst} ::
                   g t
             | MOVE{src,dst} => i :: g t
             | BRANCH{test,src1,src2,dst,live} =>
                   BRANCH{test=test,
                          src1=unbogus_reg src1,
                          src2=unbogus_reg src2,
                          dst=dst, live=live
                          } :: g t
             | BOGUS _ => g t   
             | _  =>  i :: g t
        in
            f i
        end
    in
        g il
    end
end
structure Ntypes :
    sig
        type name
        val init_names : unit -> unit
        val new_name : name -> name
        val prime_name : name -> name
        val name_prefix_eq : (name * name) -> bool
        type test
        val teq : test * test -> bool
        type reg
        type assignment
        val aeq : assignment * assignment -> bool

        datatype test_or_name =
            TEST of test
          | NAME of name
          | NEITHER

        val toneq : test_or_name * test_or_name -> bool

        datatype test_or_assign =
            TST of test
          | ASS of assignment

        val toaeq : test_or_assign * test_or_assign -> bool

    end = 

struct


type test = HM.comparison
val teq = HM.ceq

type reg = int*string

type assignment = HM.operation
val aeq = HM.oeq

type name = string

val ct = ref 0

fun init_names () = ct := 0

fun nn() = (ct := !ct + 1; !ct - 1)

fun pref nil = nil
  | pref ("_" :: t) = nil
  | pref (h :: t) = h :: pref t

val name_prefix = implode o pref o explode
fun name_prefix_eq(a, b) = (name_prefix a) = (name_prefix b)
(*
fun new_name n = n ^ "_" ^ (makestring (nn()))
*)
fun new_name n = name_prefix n ^ "_" ^ (makestring (nn()))
fun prime_name n = (new_name n) ^ "'"

datatype test_or_name =
    TEST of test
  | NAME of name
  | NEITHER

fun toneq (TEST a, TEST b) = teq (a, b)
  | toneq (NAME a, NAME b) = a = b
  | toneq _ = false

datatype test_or_assign =
    TST of test
  | ASS of assignment

fun toaeq (TST a, TST b) = teq (a, b)
  | toaeq (ASS a, ASS b) = aeq (a, b)
  | toaeq _ = false

end
structure Dag :
    sig
        exception DAG
        exception DAGnotfound
        type dag
        val make : dag
        val tests_of : dag -> Ntypes.test Set.set
        val sel_of : dag -> ((Ntypes.test * bool) -> Ntypes.test_or_name)
        val root_of : dag -> Ntypes.test_or_name
        val succ_of : dag -> Ntypes.name Set.set
        val attach : Ntypes.test * dag * dag -> dag
        val reach : dag * Ntypes.test_or_name -> dag
        val replace_edge : dag * Ntypes.name list -> dag
        val newdag : (Ntypes.test Set.set *
                      ((Ntypes.test * bool) -> Ntypes.test_or_name) *
                      Ntypes.test_or_name *
                      Ntypes.name Set.set)
            -> dag
        val dagToString : dag -> string
    end = 
struct

open Ntypes;
        
    
exception DAGnotfound
exception DAG

datatype dag =
    D of
    test Set.set *
    ((test * bool) -> test_or_name) *
    test_or_name *
    name Set.set

fun tonToString (TEST t) = "TEST t"
  | tonToString (NAME n) = "NAME " ^ n
  | tonToString NEITHER = "NEITHER"

fun sep (a, b) = a ^ ", " ^ b

fun dagToString (D(t, sel, rt, s)) =
    "D([" ^ PrintAbs.str (Set.set t) ^ "]" ^
    "fn, " ^ (tonToString rt) ^ ", " ^ (fold sep (Set.set s) ")")
    
val make = D(Set.makeEQ teq, fn x => raise DAGnotfound, NEITHER, Set.make)

fun newdag x = D x

fun tests_of(D (b, sel, r, h)) = b
fun sel_of(D (b, sel, r, h)) = sel
fun root_of(D (b, sel, r, h)) = r
fun succ_of(D (b, sel, r, h)) = h

fun attach (t, D dt, D df) = 
    let open Set
        val (b1, sel1, r1, h1) = dt
        val (b2, sel2, r2, h2) = df
    in
        D(add(union(b1, b2), t),
          (fn(x, y) =>
           if teq(x, t) then if y then r1 else r2
           else sel1(x, y) handle DAGnotfound => sel2(x, y)),
          TEST t,
          union(h1,h2)
          )
    end
       
fun reach (D d, tn) =
    let open Set
        val (b, sel, r, h) = d
        fun f (TEST t) =
            if not (member(b, t)) then raise DAGnotfound
            else attach(t, reach(D d, sel(t, true)), reach(D d, sel(t, false)))
         | f (NAME n) =
           D(makeEQ teq, fn x => raise DAGnotfound, NAME n, listToSet [n])
           | f (_) = raise DAGnotfound
    in
        f tn
    end

fun replace_edge (D d, nil) = D d
  | replace_edge (D d, old::new::tl) =
    let open Set
        val (b, sel, r, h)  = d
        val nh = if member(h, old) then add(rm(h, old), new) else h
        val nr = if toneq(r, NAME old) then NAME new else r
        val nsel = fn(x, y) =>
            let val v = sel(x, y)
            in
                if toneq(v,  NAME old) then NAME new else v
            end
    in
        D (b, nsel, nr, nh)
    end
  | replace_edge _ = raise DAG

end
















structure Node :
    sig
        type node
        type program
        val delete_debug : bool ref
        val move_op_debug : bool ref
        val move_test_debug : bool ref
        val rw_debug : bool ref
        val ntn_debug : bool ref
        val prog_node_debug : bool ref
        val prog_node_debug_verbose : bool ref
        val closure_progs_debug : bool ref
        val cpsiCheck : bool ref
        val makeProg : unit -> program
        val make :
            Ntypes.name * Ntypes.assignment Set.set *
            Dag.dag * Ntypes.name Set.set-> node
        val name_of : node -> Ntypes.name
        val assignment_of : node -> Ntypes.assignment Set.set
        val dag_of : node -> Dag.dag
        val succ : program * node -> Ntypes.name Set.set
        val prednm : program * Ntypes.name -> Ntypes.name Set.set
        val pred : program * node -> Ntypes.name Set.set
        val succNodes : program * node -> node Set.set
        val predNodes : program * node -> node Set.set
        val readNode : node -> int Set.set
        val writeNode : node -> int Set.set
        val unreachable : program * node -> bool
        val num_ops_node : node -> int
        val num_tests_node : node -> int
        val num_things_node : node -> int
        val replace_edge_node : node * string list -> node
        exception NAMETONODE
        val nameToNode : program * Ntypes.name -> node
        val nameSetToNodeSet : program * Ntypes.name Set.set -> node Set.set
        val eqn : node * node -> bool
        val n00 : node
        val fin : node
        val delete : program * node -> program
        val move_op :
            program * Ntypes.assignment * node Set.set * node -> program
        val move_test :  program * Ntypes.test * node * node -> program
        val nodeToString : node -> string
        val progToString : program -> string
        val entries : program -> node list
        val programs : program -> program list
        val addPredInfo : program -> program
        val closure : program * node -> program
        val sortNodes : node list -> node list
        val updateNode : program * node -> program
        val addNode : program * node -> program
        val rmNode : program * node -> program
    end = 
struct

open Ntypes
open Dag
open StrPak
datatype node = N of name * assignment Set.set * dag * name Set.set
type program = node Stringmap.stringmap * node * node

type debug_fun = unit -> string
val delete_debug = ref false
val move_op_debug = ref false
val dead_set_debug = ref false
val move_test_debug = ref false
val rw_debug = ref false
val prog_node_debug = ref false
val prog_node_debug_verbose = ref false
val closure_progs_debug = ref false

fun name_of(N(n, a, d, prd)) = n
fun assignment_of(N(n, a, d, prd)) = a
fun dag_of(N(n, a, d, prd)) = d
fun pred_of(N(n, a, d, prd)) = prd

fun eqn(n1, n2) = name_of n1 = name_of n2

val start:name = "START"
val finish:name = "FINISH"

fun printstringlist sl = stringListString sl
val psl = printstringlist

fun nodeToString (N(n, a, d, prd)) =
    "\nN(" ^ n ^ ", [" ^ PrintAbs.str (Set.set a) ^ "], " ^
    Dag.dagToString d ^
    "pred(" ^ psl (Set.set prd) ^ "))"

fun progToString (ns, n0, F) =
    "P (" ^ (psl o (map nodeToString) o Stringmap.extract) ns ^ ",\n" ^
    nodeToString n0 ^ ",\n" ^
    nodeToString F ^ ")\n"

fun make (n, a, t, prd) = N(n, a, t, prd)

val n00 = make(start, Set.makeEQ aeq, Dag.make, Set.make)
val fin = make(finish, Set.makeEQ aeq, Dag.make, Set.make)

fun makeProg() = (Stringmap.new():node Stringmap.stringmap, n00, fin)

fun addPredNode (N(n, a, t, prd), p) = (N(n, a, t, Set.add(prd, p)))
fun unionPredNode (N(n, a, t, prd), ps) = (N(n, a, t, Set.union(prd, ps)))
fun setPredNode (N(n, a, t, prd), p) = (N(n, a, t, p))
fun rmPredNode (N(n, a, t, prd), p) = (N(n, a, t, Set.rm(prd, p)))

fun p_n_debug (f:debug_fun) =
    if !prog_node_debug then print ("p_n:" ^ f() ^ "\n")
    else ()


fun updateNode(P as (ns, n0, F), new_node) =
    let val answer =
        (Stringmap.rm (ns:node Stringmap.stringmap)
         ((name_of new_node):string);
         Stringmap.add ns ((name_of new_node), new_node);
         if name_of new_node = name_of n0 then (ns, new_node, F)
         else if name_of new_node = name_of F then (ns, n0, new_node)
              else P)
        val foo = p_n_debug
            (fn () =>
             ("updateNode n=" ^ nodeToString new_node ^
              "=>" ^
                  (if !prog_node_debug_verbose then progToString answer
                   else "(program)")))
    in
        answer
    end

fun addNode(P as (ns, n0, F), new_node) =
    let val answer =
        if Stringmap.isin ns (name_of new_node) then updateNode(P, new_node)
        else (Stringmap.add ns ((name_of new_node), new_node);
              P)
        val foo = p_n_debug
            (fn () =>
             ("addNode n=" ^ nodeToString new_node ^
              "=>" ^ 
                  (if !prog_node_debug_verbose then progToString answer
                   else "(program)")))
    in
        answer
    end


fun rmNode(P as (ns, n0, F), node) =
    let val answer = (Stringmap.rm ns (name_of node);
                      P)
        val foo = p_n_debug
            (fn () =>
             ("rmNode n=" ^ nodeToString node ^
              "=>" ^ 
                  (if !prog_node_debug_verbose then progToString answer
                   else "(program)")))
    in
        answer
    end


fun succ(p, n) = (succ_of o dag_of) n
fun pred(p, n) = pred_of  n

val ntn_debug = ref true
fun ntnPrint (f:debug_fun) = if !ntn_debug then print ("ntn:" ^ f() ^ "\n") else ()

exception NAMETONODE
fun nameToNode(P as (ns, n0, F), nm) =
    Stringmap.map ns nm
    handle Stringmap =>
        (ntnPrint (fn () => ("nameToNode " ^ nm ^ "not found"));
         raise NAMETONODE)

exception NAMESETTONODESET
fun nameSetToNodeSet(P, ns) =
    Set.listToSetEQ(eqn, map (fn x => nameToNode(P, x)) (Set.set ns))
    handle NAMETONODE => raise NAMESETTONODESET

fun prednm(p, nm) = pred(p, nameToNode(p, nm))

fun succNodes (p, n) = nameSetToNodeSet(p, succ(p, n))
fun predNodes (p, n) = nameSetToNodeSet(p, pred(p, n))


(* a correctness assertion *)
exception CPSI
val cpsiCheck = ref false
fun checkPredSuccInfo(from, P as (ns, n0, F)) =
    let val nl = Stringmap.extract ns
        val badnode = ref n0
        fun fail s = (print ("CPSI:" ^ s ^ " failed\nfrom " ^ from ^
                             "\nbadnode=" ^ nodeToString (!badnode) ^
                             "\nprogram=" ^ progToString P ^ "\n");
                      raise CPSI)
        fun chk (xpred, xsuccN, n) =
            let val foo = badnode := n
                val s = Set.set(xsuccN(P, n))
                    handle NAMESETTONODESET =>
                        fail "NAMESETTONODESET"
                fun cs x = Set.member(xpred x, name_of n)
                fun fs (x, b) = b andalso cs x
            in
                fold fs s true
            end
        fun cp (x, b) = b andalso chk(pred_of, succNodes, x)
        fun cs (x, b) = b andalso chk((succ_of o dag_of), predNodes, x)
    in
        if not (fold cp nl true) then fail "cp"
        else if not (fold cs nl true) then fail "cs"
             else ()
    end
fun cpsi x = if !cpsiCheck then checkPredSuccInfo x else ()


fun empty n =
    let open Set in
        empty (assignment_of n) andalso empty ((tests_of o dag_of) n)
    end

fun unreachable(P as (ns, n0, F), n) =
    not (eqn (n0, n)) andalso Set.empty (pred(P, n))

fun read (TST(t)) = HM.read_c t
  | read (ASS(a)) = HM.read_o a

fun write (TST(t)) = HM.write_c t
  | write (ASS(a)) = HM.write_o a

fun read_write_debug (f:debug_fun) =
    if !rw_debug then print (f() ^ "\n")
    else ()

fun readNode n =
    let open Set 
        val answer =
            union
            (listUnion (make::(map (read o ASS) ((set o assignment_of) n))),
             listUnion (make::(map
                               (read o TST) ((set o tests_of o dag_of) n))))
        val foo = read_write_debug
            (fn () =>
             ("readNode " ^ nodeToString n ^ "=>" ^
                 stringListString (map makestring (set answer))))
    in
        answer
    end

fun writeNode n =
    let open Set 
        val answer =
            union
            (listUnion (make::(map (write o ASS) ((set o assignment_of) n))),
             listUnion (make::(map
                               (write o TST) ((set o tests_of o dag_of) n))))
        val foo = read_write_debug
            (fn () =>
             ("writeNode " ^ nodeToString n ^ "=>" ^
                 stringListString (map makestring (set answer))))
    in
        answer
    end

fun no_write_conflict (ta, n) =
    let open Set in
        empty (intersect(writeNode n, (union(read ta, write ta))))
    end

fun no_read_conflict (ta, n) =
    let open Set in
        empty (intersect (write ta, readNode n))
    end

fun empty n =
     let open Set in
         (empty o assignment_of) n andalso (empty o tests_of o dag_of) n
     end

fun replace_edge_node(N (n, a, d, p), nl) = N(n, a, replace_edge(d, nl), p)

fun except_bogus nil = nil
  | except_bogus (h::t) =
    if Delay.is_bogus_i h then except_bogus t else h :: except_bogus t

val num_ops_node = List.length o except_bogus o Set.set o assignment_of
val num_tests_node = List.length o Set.set o tests_of o dag_of
fun num_things_node n = (num_ops_node n) + (num_tests_node n)

fun dead_debug (f:debug_fun) =
    if !dead_set_debug then print ("dead" ^ f() ^ "\n") else ()

exception DEAD
fun dead(P:program, r:HM.reg, n:node, done: name Set.set) =
    let val foo =
        dead_debug (fn () => "(P, " ^ makestring r ^ ", " ^ nodeToString n ^ ")")
        val new_done = Set.add(done, name_of n)
        fun nfil(a, b) = if Set.member(new_done, a) then b
                         else a::b
        fun drl nil = true
          | drl (h::t) = dead(P, r, h, new_done) andalso drl t
        fun ntn n = nameToNode (P, n) handle NAMETONODE => raise DEAD
        val next = fold nfil (Set.set (succ(P, n))) nil
        val answer = (
                      not (Set.member(readNode n, r)) andalso
                      (Set.member(writeNode n, r)      orelse
                       drl (map ntn next))
                      )
        val foo = dead_debug(fn () => "=>" ^ Bool.toString answer)
    in
        answer
    end

fun deadset(P, rs, n) =
    let val foo = dead_debug (fn () => "deadset(" ^
                              stringListString
                              (map makestring (Set.set rs)) ^ ",\n" ^
                              nodeToString n ^ ")")
        fun f nil = true
          | f (r::t) = dead(P, r, n, Set.make) andalso f t
        val answer = f (Set.set rs)
        val foo = dead_debug(fn () => "deadset=>" ^ Bool.toString answer ^ "\n")
    in
        answer
    end

fun del_debug (f:debug_fun) =
    if !delete_debug then print ("delete:" ^ f() ^ "\n")
    else ()

exception DELETE
exception DELETE_HD
exception DELETE_WIERDSUCC
fun delete (P as (ns, n0, F), n) =
    let val foo = cpsi("delete enter", P)
        val em = empty n
        val un = unreachable(P, n)
        fun ntn n = nameToNode(P, n) handle NAMETONODE => raise DELETE
        val p = Set.listToSetEQ(eqn, (map ntn (Set.set (pred(P, n)))))
        open Set

        val foo = del_debug
            (fn () =>
             "delete( n=" ^ (name_of n) ^ "\n" ^
             "em=" ^ (Bool.toString em) ^ "\n" ^
             "un=" ^ (Bool.toString un) ^ "\n" ^
             "p =" ^ (psl (map name_of (Set.set p))) ^ "\n" ^
             ")")
    in
        if (em orelse un) andalso not (eqn(n, F)) then
            if not un then
                let 
                    val foo = del_debug (fn () => "complex deletion")
                    val s0 = Set.set (succ(P, n))
                    val nprime = if List.length s0 = 1 then hd s0
                                 else (print (Int.toString (List.length s0));
                                       raise DELETE_WIERDSUCC)
                    val new_nprime =
                        rmPredNode(unionPredNode(ntn nprime, pred_of n),
                                   name_of n)
                    fun ren x =
                        replace_edge_node(x, [name_of n, name_of new_nprime])
                    val pprime = map ren (set p)
                    fun updt(n, p) = updateNode(p, n)
                    val Nprime = fold updt (new_nprime :: pprime) P

                    val foo = del_debug (fn () => "nprime=" ^ nprime)
                    val foo = del_debug
                        (fn () =>
                         "pprime=" ^ (psl (map nodeToString pprime)))
                    val answer = rmNode(Nprime, n)
                    val foo = cpsi("delete leave cd", answer)
                in
                    answer
                end
            else (del_debug (fn () => "simple_deletion");
                  let val s = Set.set(nameSetToNodeSet(P, (succ(P, n))))
                      fun updt(s, p) = updateNode(p, rmPredNode(s, name_of n))
                      val np = rmNode(fold updt s P, n)
                      val foo = cpsi("delete leave sd", np)
                  in
                      np
                  end)
        else (del_debug (fn () => "No deletion");
              P)
    end handle Hd => raise DELETE_HD

fun mop_debug (f:debug_fun) =
    if !move_op_debug then
        (dead_set_debug := true;
         print ("mop:" ^ f() ^ "\n"))
    else dead_set_debug := false
       

fun can_move_op1(P as (ns, n0, F), x, move_set, m) =
    let open Set
        val foo = mop_debug (fn () => "can_move_op")
        val rok = HM.resources_ok(set (add(assignment_of m, x)),
                                  set ((tests_of o dag_of) m))
        val foo = mop_debug(fn () => "1")
        val p = diff(nameSetToNodeSet(P, succ(P, m)), move_set)
        val foo = mop_debug(fn () => "2")
        val l = (write o ASS) x
        val foo = mop_debug(fn () => "3")
        fun dlpf nil = true
          | dlpf (pj::t) = deadset(P, l, pj) andalso dlpf t
        fun cond nil = true
          | cond (nj::t) =
            (not o eqn)(nj, F)          andalso
            (* no_read_conflict(ASS x, nj) andalso *)
            (* change ex model so it can run on a sequential machine *)
            no_read_conflict(ASS x, m) andalso
            no_write_conflict(ASS x, m) andalso
            cond t
        val foo = mop_debug(fn () => "4")
        val answer = rok andalso cond (set move_set) andalso dlpf (set p)
        val foo = mop_debug (fn () => "can_move_op=>" ^ Bool.toString answer)
    in
        answer
    end

fun can_move_op(P, x, move_set, m) =
    let open Set
        val ms = set move_set
        fun pf n = pred(P, n)
        val ps = set(listUnion (map pf ms))
        fun all (x, b) = b andalso can_move_op1(P, x, move_set, m)
    in
        if List.length ps > 1 then
            if List.length ms > 1 then false
            else fold all ((set o assignment_of o hd) ms) true
        else can_move_op1(P, x, move_set, m)
    end

fun move_op (P as (ns, n0, F), x, move_set, m) =
    let val foo = cpsi("move_op enter", P)
        val foo = 
        mop_debug (fn () =>
                   "move_op(x=" ^
                   PrintAbs.str [x] ^
                   "move_set\n" ^
                   (stringListString (map nodeToString
                                            (Set.set move_set))) ^
                   "\nm=" ^ nodeToString m ^"\n)\n")
    in
    if not (can_move_op(P, x, move_set, m)) then P
    else
        let open Set
            exception NOTFOUND
            val primed_pairs = ref nil
            fun pnf nm =
                let fun f nil = 
                    let val nn = prime_name nm
                    in
                        (primed_pairs := (nm, nn) :: !primed_pairs;
                         nn)
                    end
                      | f ((a, b)::t) = if nm = a then b else f t
                    val answer = f (!primed_pairs)
                    val foo = mop_debug (fn () => "pnf " ^ nm ^ "=>" ^ answer)
                in
                    answer
                end
            val foo = mop_debug(fn () => "1")
            fun njp nil = nil
              | njp ((N(n, a, d, prd))::t) =
                N(pnf n, rm(a, x), d, listToSet [name_of m]) :: njp t
            fun ojp l = map (fn x => rmPredNode(x, name_of m)) l
            fun replist nil = nil
              | replist (h::t) = h :: pnf h :: replist t
            val rlist = replist (map name_of (set move_set))
            val foo = mop_debug(fn () => "2")
            val mprime =
                let val aprime = add(assignment_of m, x)
                    val dprime = replace_edge(dag_of m, rlist)
                in
                    N(name_of m, aprime, dprime, pred_of m)
                end
            val foo = mop_debug(fn () => "3")
            val nj = njp(set move_set)
            val foo = mop_debug(fn () =>
                                "nj=" ^
                                stringListString (map name_of nj))
            fun uptd(n, p) = updateNode(p, n)
            val np = fold uptd (mprime :: (ojp (set move_set))) P
            fun addnpi(n, p) =
                let val s = set (succNodes(p, n))
                    fun ap x = addPredNode(x, name_of n)
                    fun updt(x, p) = updateNode(p, ap x)
                in
                    fold updt s p
                end
            fun addn(n, p) = addnpi(n, addNode(p, n))
            val nnp = fold addn nj np
            val foo = mop_debug(fn () => "4")
            val answer = nnp
            val foo = mop_debug(fn () => "5")
            val foo = cpsi("move_op leave", answer)
        in
            mop_debug(fn () => "6");
            answer
        end
    end

fun updt_sel (d, nsel) =
    let val tst = tests_of d
        val rt = root_of d
        val s = succ_of d
    in
        newdag(tst, nsel, rt, s)
    end

fun mt_debug (f:debug_fun) =
    if !move_test_debug then print ("move_test" ^ f() ^ "\n")
    else ()

fun can_move_test(P as (ns, n0, F):program, x:test, n:node, m:node) =
    let val foo = cpsi("move_test enter", P)
        val foo = mt_debug (fn () => "can_move_test")
        val answer =
            no_write_conflict(TST x, m) andalso

            (* hack because sel can't distinguish xj *)
            not (Set.member(tests_of(dag_of m), x)) andalso

            HM.resources_ok(Set.set (assignment_of m),
                            Set.set (Set.add((tests_of o dag_of) m, x)))
        val foo = mt_debug (fn () => "can_move_test=>" ^ Bool.toString answer)
    in
        answer
    end

fun move_test (P as (ns, n0, F):program, x:test, n:node, m:node) =
    if not (can_move_test(P, x, n, m))  then P
    else
        let val foo =
            mt_debug (fn () => "move_test" ^ name_of n ^ " " ^ name_of m)
            open Set
            val d_n = dag_of n
            val sel_n = sel_of d_n
            val rt_n = root_of d_n
            val nt =
                let val newname = (new_name o name_of) n ^ "tt"
                    fun nsel (z, b) =
                        let val v = sel_n(z, b) in
                            if toneq(v, TEST x) then sel_n(x, true)
                            else v
                        end
                    val nC = 
                        if TEST x = rt_n then
                            reach(updt_sel(d_n, nsel), sel_n(x, true))
                        else
                            reach(updt_sel(d_n, nsel), rt_n)
                in
                    N(newname, assignment_of n, nC, listToSet [name_of m])
                end
            val foo = mt_debug (fn () => "got nt")
            val nf =
                let val newname = ((new_name o name_of) n) ^ "ff"
                    fun nsel (z, b) =
                        let val v = sel_n(z, b) in
                            if toneq(v, TEST x) then sel_n(x, false)
                            else v
                        end
                    val nC =
                        if TEST x = rt_n then
                            reach(updt_sel(d_n, nsel), sel_n(x, false))
                        else
                            reach(updt_sel(d_n, nsel), rt_n)
                in
                    N(newname, assignment_of n, nC, listToSet [name_of m])
                end
            val foo = mt_debug (fn () => "got nf")
            val d_m = dag_of m
            val sel_m = sel_of d_m
            fun nton n = NAME( name_of n)
            fun nsel (z, b) =
                if teq(z, x) then if b then nton nt else nton nf
                else
                    let val v = sel_m(z, b) in
                        if toneq(v, NAME(name_of n)) then TEST x else v
                    end
            val nb = add(tests_of d_m, x)
            val nh =
                add(add(rm(succ_of d_m, name_of n), name_of nt), name_of nf)
            fun new_rt (NAME rt) = TEST x
              | new_rt t = t
            val nc = newdag(nb, nsel, (new_rt o root_of) d_m, nh)
            val new_m = N(name_of m, assignment_of m, nc, pred_of m)
            fun updt_t s = addPredNode(s, name_of nt)
            fun updt_f s = addPredNode(s, name_of nf)
            val upt = map updt_t (set (nameSetToNodeSet(P, succ(P, nt))))
            val upf = map updt_f (set (nameSetToNodeSet(P, succ(P, nf))))
            fun updtl(n, p) = updateNode(p, n)
            val np =
                fold updtl ([rmPredNode(n, name_of m), new_m] @ upt @ upf) P
            val answer = np
            val foo = mt_debug (fn () => "mtst done")
            val foo = cpsi("move_test leave", answer)           
        in
            answer
        end


fun entries (P as (ns, n0, F)) =
    let val nl = Stringmap.extract ns
        fun f (a, b) = if unreachable(P, a) then a::b else b
    in
        n0 :: (fold f nl nil)
    end

fun addPredInfo(P as (ns, n0, F)) =
    let fun rmpi n = setPredNode (n, Set.make)
        val nl = map rmpi (Stringmap.extract ns)
        fun updt(n, p) = updateNode(p, n)
        val np =  fold updt nl P
        fun addpi (n, p) =
             let val s = Set.set (succNodes(p, n))
                 fun api(s, p) = updateNode(p, addPredNode(s, name_of n))
             in
                 fold api s p
             end
    in
        fold addpi nl np
    end

fun cp_debug (f:debug_fun) =
    if !closure_progs_debug then print ("cp:" ^ f() ^ "\n")
    else ()

fun closure (P as (ns, n0, F), entry) =
    let open Set
        val foo = cp_debug
            (fn () =>
             "closure:entry=" ^ name_of entry ^ "\nprogram=" ^ progToString P)
        val isin = Stringmap.isin
        fun dfs(p, parent, nil) = p
          | dfs(p as (ns, n0, F), parent, cur::todo) =
            if not (isin ns (name_of cur)) then
                let val np = dfs(addNode(p, cur), cur, set(succNodes(P, cur)))
                in
                    dfs(np, parent, todo)
                end
            else dfs(p, parent, todo)
        val prog:program = (Stringmap.new(), entry, F)
        val answer = dfs(addNode(prog, entry),
                         entry,
                         set(succNodes(P, entry)))
        val foo = cp_debug
            (fn () =>
             "\nclosure=>" ^ progToString answer)
    in
        answer
    end

fun programs(P as (ns, n0, F):program) =
    let val foo = cp_debug (fn () => "programs")
        val l = entries (addPredInfo P)
        (* make sure preds are in closure*)
        fun cf e = addPredInfo(closure(P, e))
        val answer = map cf l
        val foo = cp_debug (fn () => "programs done")
    in
        answer
    end

structure ns =
    struct
        type obj = node
            
        fun int l =
            let val z = ord "0"
                fun f(n, nil) = n
                  | f (n, d::l) =
                    if d>="0" andalso d<="9" then f(n*10+ord(d)-z, l)
                    else n
            in
                f(0,l)
            end

        fun gt (a, b) =
            let val a = explode(name_of a)
                val b = explode(name_of b)
            in
                (int a) > (int b)
            end
    end

structure sortN = Sort(ns)

val sortNodes = sortN.sort

end

structure Compress :
    sig
        val compress_debug : bool ref
        val compress : (int * Node.program) -> Node.program
        val move_things_node :
            Node.program * Ntypes.name * Ntypes.name Set.set -> Node.program
        val do_move_tests : bool ref
        val do_move_ops : bool ref

        val dbg_p : Node.program ref

    end = 

struct

open Ntypes
open Dag
open Node

val do_move_tests = ref false
val do_move_ops = ref true

exception COMPRESS

fun error (s:string) =
    (print (s ^ "\n");
     raise COMPRESS)

val compress_debug = ref false

val dbg_p =  ref (makeProg())

type debug_fun = unit -> string
fun debug (f:debug_fun) =
    if !compress_debug then print (f() ^ "\n")
    else ()
        
exception FILTERSUCC

fun filterSucc(P, nm, fence_set) =
    let open Set
        val s = set(succ(P, nameToNode(P, nm)))
            handle NAMETONODE => raise FILTERSUCC
        fun f (nm, l) = if member(fence_set, nm) then l else nm::l
    in
        fold f s nil
    end

(*
val inP = ref false
val finP = ref makeProg
val foutP = ref makeProg

fun chinP (p, from) =
    let val nm = "11_100'_110tt_119'"
        val prd = prednm(p, nm)
        val pe = Set.empty(prd)
    in
        if !inP then
            if pe then (foutP := p; error ("chinP gone -" ^ from)) else ()
        else if pe then ()
             else (inP := true;
                   print ("chinP found it -" ^ from ^ "\n");
                   finP := p;
                   nameToNode(p, nm);
                   ())
    end
*)

exception MOVETHINGSNODE
fun move_things_node(P, nm, fence_set) =
    let open Set
        (*
        val foo = debug
            (fn () =>
            "move_things_node(\n" ^
            progToString P ^ ",\n" ^
            nm ^ ", [" ^
            fold (fn (a, b) => a ^ ", " ^ b) (set fence_set) "]" ^
            ")")
            *)
        fun ntn (p, nm) = ((* chinP (p, "ntn");*) nameToNode (p, nm))
            handle NAMETONODE => (dbg_p := P; raise MOVETHINGSNODE)
        fun s_nm_list p = filterSucc(p, nm, fence_set)
        fun nd nm = ntn(P, nm) handle MOVETHINGSNODE => error "nd  nm"
        val au = listUnionEQ(aeq, map (assignment_of o nd) (s_nm_list P))
        val tu = listUnionEQ(teq, map (tests_of o dag_of o nd) (s_nm_list P))
        fun ms (p, a) =
            let fun f(nm, l) =
                ((*chinP (p, "ms"); *)
                 if member(assignment_of(ntn(p, nm)), a) then nm::l
                 else l
                     )
                handle MOVETHINGSNODE => (dbg_p := p; error "ms")
            in
                fold f (s_nm_list p) nil
            end
        fun move_a1(a, p) =
            let val msl = ms (p, a)
                val ms_set = nameSetToNodeSet(p, listToSet msl)
                fun dms(a, p) = delete(p, ntn(p, a))
                fun mop() =
                    let val foo = debug (fn () => "mop start " ^ nm)
                        val new_p = move_op(p, a, ms_set, ntn(p, nm))
                            handle MOVETHINGSNODE => error "move_a move_op"
                        val foo = debug (fn () => "mop end")
                    in
                        new_p
                    end
                val mpa = mop()
                    (*
                val foo = chinP(mpa,
                                "a_move_a amop " ^ nm ^
                                StrPak.stringListString
                                (map name_of (set ms_set)))
                     *)
                val answer = fold dms msl mpa
                    (*
                val foo = chinP(answer, "a_move_a adel")
                     *)
            in
                answer
            end
        fun move_a(a, p) = if !do_move_ops then move_a1(a, p) else p
        fun tset (p, t) =
            let fun f(nm, l) =
                ((*chinP (p, "tset");*)
                 if member(tests_of(dag_of(ntn(p, nm))), t) then nm::l
                 else l
                     )
                handle MOVETHINGSNODE => error "tset"
            in
                fold f (s_nm_list p) nil
            end
        fun move_t1(t, p) =
            let val ts = tset (p, t)
                val answer =
                    if List.length ts > 0 then
                        move_test(p, t,
                                  (ntn(p, hd ts)
                                   handle MOVETHINGSNODE => error "move_t 1"),
                                  (ntn(p, nm)
                                   handle MOVETHINGSNODE => error "move_t 2"))

                    else p
                (*val foo = chinP(answer, "a_move_t")*)
            in
                answer
            end
        fun move_t(t, p) = if !do_move_tests then move_t1(t, p) else p
    in
        debug (fn () => "movethingsnode " ^ nm ^ "\n");
        fold move_t (set tu) (fold move_a (set au) P)
    end

exception MOVETHINGSWINDOW
fun move_things_window(P, w, nm, fence_set) =
    let open Set
        (*
        val foo = debug (fn () =>
                         "move_things_window(\n" ^
                         progToString P ^ ",\n" ^
                         (makestring w) ^ ", " ^
                         nm ^ ", [" ^
                         fold (fn (a, b) => a ^ ", " ^ b) (set fence_set) "]" ^
                         ")\n")
            *)
        fun ntn (P, nm) = (nameToNode (P, nm))
            handle NAMETONODE =>  raise MOVETHINGSWINDOW
        val node = ntn(P, nm)
        val things = num_things_node node
        val s_nm_list = filterSucc(P, nm, fence_set)
        fun nxt(nm, p) =
            move_things_window(p, w - things, nm, fence_set)
        val child_p = if w > things then fold nxt s_nm_list P else P
    in
        debug (fn () => "movethingswindow " ^ nm ^ "\n");
        move_things_node(child_p, nm, fence_set)
    end


exception CPRESS
exception CPRESS1
exception CPRESS2
exception CPRESS3
exception CPRESS4
exception CPRESS5
fun cpress(window, P, fence_set, everin_fence_set) =
    let open Set
        fun nxt(nm, p:program) = 
            ((* dbg_p := p; *)
             move_things_window(p, window, nm, fence_set))
            handle MOVETHINGSWINDOW => raise CPRESS1
        val filled = fold nxt (set fence_set) P
            handle CPRESS1 => raise CPRESS2
        fun succf nm = succ(filled, nameToNode(filled, nm))
            handle NAMETONODE => raise CPRESS
        val nfence_set = listUnion(make::(map succf (set fence_set)))
        fun filt(a, l) = if member(everin_fence_set, a) then l else a::l
        val f_fence_set = listToSet(fold filt (set nfence_set) nil)
        val n_everin_fc =
            fold (fn (a, s) => add(s, a)) (set f_fence_set) everin_fence_set
    in
        debug (fn () => "cpress: fence_set=" ^
               StrPak.stringListString (set fence_set) ^
               "\n f_fence_set =" ^ StrPak.stringListString (set f_fence_set));
        if not (empty f_fence_set)
            then cpress(window, filled, f_fence_set, n_everin_fc)
                handle CPRESS => raise CPRESS3
                    handle CPRESS1 => raise CPRESS4
                        handle CPRESS2 => raise CPRESS5
        else filled
    end

fun clean_up (P as (ns, n0, F):program) =
    let val foo = debug (fn () => "cleanup")
        val clos = closure(P, n0)
        val (ns, n0, F) = clos
        val l = (map name_of (Stringmap.extract ns))
        fun f (n, p) =
            (debug (fn () => "cleanup deleting " ^ n);
            delete(p, nameToNode(p, n)))
        val answer = fold f l clos
        val foo = debug (fn () => "exiting cleanup")
    in
        answer
    end
    
fun compress(window, P as (ns, n0, F)) =
    let open Set
        val fence = n0
        val fence_set = add(make, name_of n0)
        val everin_fence_set = add(makeEQ(name_prefix_eq), name_of n0)
        val uc = cpress(window, P, fence_set, everin_fence_set)
        val cu = clean_up uc
    in
        debug (fn () => "compress");
        cu
    end
        


end
structure ReadI :
    sig
        val readI :
            HM.operation list -> (HM.operation list * Node.program list)
            
        val writeI :
            (HM.operation list * Node.program list) -> HM.operation list

        val progMap : Node.program -> string

        val read_debug : bool ref
        val write_debug : bool ref
        val live_debug : bool ref
    end = 

struct

val read_debug = ref false
val write_debug = ref false
val live_debug = ref false

fun read_dbg f =
    if !read_debug then print ("readI.read:" ^ f() ^ "\n")
    else ()
        
fun write_dbg f =
    if !write_debug then print ("writeI.read:" ^ f() ^ "\n")
    else ()

fun write_dbg_s s = write_dbg (fn () => s)
    
exception BTARGET

fun btarget (nil, n) = (fn x => raise BTARGET)
  | btarget (h::t, n) =
    let open HM
        val rf = btarget(t, n + 1)
        fun g lbl x = if lbl = x then n else rf x
        fun f (TARGET(lbl, inst)) = (g lbl)
          | f _ = rf
    in
        f h
    end


val programs = Node.programs

exception BNODES

fun buildNodes l =
    let open HM
        open Ntypes
        val t = btarget(l, 0)
        fun f (nil, n) = nil
          | f (ci::rest, n) =
            let open Dag
                open AbsMach
                val nm = makestring n
                val nxtnm = makestring (n + 1)
                fun asn i = Set.listToSetEQ(aeq, i)
                val edag = reach(Dag.make, NAME nxtnm)
                fun tgtnm tgt = makestring (t tgt)
                fun edagt tgt = reach(Dag.make, NAME (tgtnm tgt))
                val finDag = reach(Dag.make, NAME (Node.name_of Node.fin))
                fun cdag (tgt,tst)  = attach(tst, edagt tgt, edag)
                val g =
                    fn ASSIGNMENT i => Node.make(nm, asn [i], edag, Set.make)
                     | NERGLE  =>  Node.make(nm, asn [], edag, Set.make)
                     | LABELREF (tgt, i as GETLAB{lab, dst}) =>
                           Node.make(nm,
                                     asn [GETLAB{lab=(t tgt, tgtnm tgt),
                                                 dst=dst}],
                                     edag, Set.make)
                     | COMPARISON (tgt, tst) =>
                           Node.make(nm, asn nil, cdag(tgt, tst), Set.make)
                     | FLOW (tgt, i) =>
                           Node.make(nm, asn nil, edagt tgt, Set.make)
                     | EXIT i => Node.make(nm, asn [i], finDag, Set.make)
                     | TARGET (lbl, i) =>
                           Node.make(nm, asn nil, edag, Set.make)
                     | _ => raise BNODES
            in
                (g ci)::Node.fin::(f (rest, n + 1))
            end
        fun addn(n, p) = Node.addNode(p, n)
        val prog = fold addn (Node.fin :: f(l, 0)) (Node.makeProg())
    in
        prog
    end
            
exception READI
exception READI_NTN
fun readI ol =
    let open HM
        fun junkfil (JUNK a, (junk, other)) = (JUNK a :: junk, other)
          | junkfil (x, (junk, other)) = (junk, x::other)
        val cl = map HM.classify ol
        val (junk, other) = fold junkfil cl (nil, nil)
        fun ntn x = (Node.nameToNode x )
            handle NAMETONODE => raise READI_NTN
        val (ns, foo, fin) = buildNodes other
        val nn = (ns, ntn((ns, foo, fin), "0"), fin)
        fun unjunk (JUNK i) = i
          | unjunk _ = raise READI
        val progs = programs nn
        val foo = read_dbg
            (fn () => ("progs =>" ^
                       (StrPak.stringListString
                        (map Node.progToString progs))))
    in
         (map unjunk junk,  progs)
    end

structure ps =
    struct
        open Ntypes
        type obj = Node.program

        fun int l =
            let val z = ord "0"
                fun f(n, nil) = n
                  | f (n, d::l) =
                    if d>="0" andalso d<="9" then f(n*10+ord(d)-z, l)
                    else n
            in
                f(0,l)
            end

        fun gt((nsa, n0a, Fa), (nsb, n0b, Fb)) =
            let val a = explode (Node.name_of n0a)
                val b = explode (Node.name_of n0b)
            in
                (int a) > (int b)
            end
    end

structure sortP = Sort (ps)

fun live_dbg f = if !live_debug then print ("live:" ^ f() ^ "\n")
                 else ()

fun build_live_tab(P as (ns, n0, F): Node.program) =
    let open Ntypes
        open Node
        open Set
        fun fil (a, b) = if a < 0 orelse Delay.is_bogus_reg (a, "") then b
                         else add(b, a)
        fun fil_lset s = fold fil (set s) make
        val lt:(int set) Stringmap.stringmap = Stringmap.new()
        val finset = listToSet [0, 1, 2, 3, 4, 5]
        fun flive f n =
            if Stringmap.isin lt (name_of n) then Stringmap.map lt (name_of n)
            else f n 
        fun dfs cur =
            let fun fl n = flive dfs n
                val nm = name_of cur
                val gen = (fil_lset o readNode) cur
                val kill = writeNode cur
                val foo  = Stringmap.add lt (nm, gen)
                val children = succNodes(P, cur)
                val ch_live = if empty children then finset
                             else listUnion (map fl (set children))
                val live = union(diff(ch_live, kill), gen)
                val foo = Stringmap.rm lt nm
                val foo = Stringmap.add lt (nm, live)
            in
                live
            end
    in
        dfs n0;
        (fn nm => 
         let val ans = Stringmap.map lt nm
             val foo = live_dbg (fn () => nm ^ "=>" ^
                                 StrPak.stringListString
                                 (map makestring (set ans)))
         in
             ans
         end)
    end

(* live is the union of live in successors *)
fun branch_live (P, tab, nm) =
    let open Node
        val s = Set.set (succ(P, nameToNode(P, nm)))
        val l:int Set.set = Set.listUnion (map tab s)
        val foo = live_dbg
            (fn()=>("branch_live " ^ nm ^ " s=" ^
                    StrPak.stringListString s ^ " -> " ^
                    StrPak.stringListString (map makestring (Set.set l))))
    in
        l
    end

exception WRITEP
exception WRITEP1
exception WRITEP_NTN

fun writeP (entry_map,  lbl_fun, P as (ns, n0, F):Node.program) =
    let open Ntypes
        open Node
        open Set
        open HM
        open AbsMach
        val foo = write_dbg(fn () => "program:" ^ progToString P)
        fun blblmap nil = (fn x => (print ("blblmap_" ^ x); raise WRITEP))
          | blblmap (nm::t) =
            let val mp = blblmap t
                val mylab = lbl_fun()
            in
                    (fn x => if x = nm then mylab else mp x)
            end
        val lblmap = blblmap(map name_of (Stringmap.extract ns))
        val live_tab = build_live_tab P
        fun label_list nm = map (fn r => (r, "")) (set (live_tab nm))
        fun br_list nm =
            map (fn r => (r, "")) (set (branch_live(P, live_tab, nm)))
        fun getlab (GETLAB{lab=(i,s), dst}) =
            GETLAB{lab=(entry_map s, "node" ^ s), dst=dst}
          | getlab _ = raise WRITEP1
        fun dogetlabs (i as GETLAB _, l) = (getlab i) :: l
          | dogetlabs (i, l) = i :: l
        fun ubranch (frm, nm) =
            BRANCH{test=ieq, src1=(0, "zero"), src2=(0, "zero"),
                   dst=(lblmap nm, "node" ^ nm), live=br_list frm}
        fun cbranch (BRANCH{test, src1, src2, dst, live}, frm, nm) =
            BRANCH{test=test, src1=src1, src2=src2,
                   dst=(lblmap nm, "node" ^ nm), live=br_list frm}
          | cbranch _ = (print "cbranch"; raise Match)
        fun label nm = LABEL{lab=(lblmap nm, "node" ^ nm), live=label_list nm}
        fun entry_label nm =
            LABEL{lab=(entry_map nm, "entry"), live=label_list nm}

        fun f (done, lastnm, nm) =
            let val foo = write_dbg
                    (fn () =>
                     "f (" ^
                     StrPak.stringListString (set done) ^ "," ^
                     nm ^ ")")
            in
            if nm = name_of F then (write_dbg_s "fin"; (done, [NOP]))
            else if member(done, nm) then (write_dbg_s "already";
                                           (done, [NOP, ubranch(lastnm, nm)]))
            else
                let open Dag
                    val foo = write_dbg_s "doing"
                    val node = nameToNode(P, nm)
                        handle NAMETONODE => raise WRITEP_NTN
                    val needlabel = 
                        let val pd = set (pred (P, node))
                            val foo = write_dbg
                                (fn () => ("needlabel pd=" ^
                                           StrPak.stringListString pd))
                            fun f nil = false
                              | f ((p::nil):Ntypes.name list) =
                                let val pn = nameToNode(P, p:Ntypes.name)
                                    val foo = write_dbg
                                        (fn () => ("ndlbl: pn=" ^
                                                   nodeToString pn))
                                    val d = dag_of pn
                                    val sel = sel_of d
                                    val rt = root_of d
                                    fun istst (TEST t) =
                                        (write_dbg_s "ist true\n";
                                         true)
                                      | istst (NAME n) =
                                        (write_dbg_s "ist false\n";
                                         false)
                                      | istst NEITHER =
                                        (write_dbg_s "ist false\n";
                                         false)
                                    fun untst (TEST t) = t
                                      | untst _ = (print "needlabel1";
                                                  raise Match)
                                    fun unnm (NAME nm) = nm
                                      | unnm _ = (print "needlabel2";
                                                  raise Match)
                                    val foo =
                                        if istst rt then
                                            write_dbg
                                            (fn () =>
                                             ("sel=" ^
                                              unnm(sel(untst rt, true)) ^
                                              "\n"))
                                            else ()
                                in
                                    istst rt andalso
                                    (sel(untst rt, true) = NAME nm)
                                end
                              | f (a::b::c) = true
                            val answer = f pd
                            val foo = write_dbg
                                (fn () => ("needlabel=>" ^
                                           Bool.toString answer))
                        in
                            answer
                        end
                    val nodelabel = if needlabel then [label nm] else nil
                    val nodeNOP = [NOP]
                    val a = fold dogetlabs (set (assignment_of node)) nil
                    val d = dag_of node
                    val sel = sel_of d
                    val rt = root_of d
                    (* only works for <= 1 test *)
                    fun dag_code NEITHER = (nil, nil)
                      | dag_code (NAME n) = ([n], nil)
                      | dag_code (TEST t) =
                        let fun unnm (NAME x) = x
                              | unnm _ = (print "dag_code"; raise Match)
                            val t_n = unnm(sel(t, true))
                            val f_n = unnm(sel(t, false))
                        in
                            ([f_n, t_n], [cbranch(t, nm, t_n)])
                        end
                    val (nl, cd) = dag_code rt
                    exception DFS_SURPRISE
                    fun dfs (done, nil) = (write_dbg_s "dfs nil";
                                           (done, nil))
                      | dfs (done, h::nil) = (write_dbg_s "dfs 1";
                                              f(done, nm, h))
                      | dfs (done, h::nxt::nil) =
                        let val foo = write_dbg_s "dfs 2"
                            val (dn1, cd1) = f(done, nm, h)
                            val (dn2, cd2) =
                                if member(dn1, nxt) then (dn1, nil)
                                else dfs(dn1, nxt::nil)
                            val lbl =
                                if nxt = name_of F orelse
                                    member(dn2, nxt) then [NOP]
                                else [NOP, label nxt]
                        in
                            (dn2, cd1 @ lbl @ cd2)
                        end
                      | dfs _ = raise DFS_SURPRISE
                    val (dn, dcd) = dfs(add(done, nm), nl)
                in
                    (dn, NOP :: nodelabel @ a @ cd @ dcd)
                end
            end
            val (done, code) = f (Set.make, "badname", name_of n0)
    in
        (entry_label (name_of n0)) :: (label (name_of n0)) :: code
    end

exception WRITEI

fun progMap(p as (ns, n0, F)) =
    let val l = Node.sortNodes (Stringmap.extract ns)
        val outstr = ref ""
        fun pr s = outstr := !outstr ^ s
        fun ntn n = Node.nameToNode(p, n)
        val n0nm = Node.name_of n0
        val nFnm = Node.name_of F
        fun f n =
            let val s = Set.set (Node.succ(p, n))
                val nm = Node.name_of n
                val pre = if nm = n0nm then "->\t"
                          else "\t"
                val post = if nm = nFnm then "\t->\n"
                           else "\n"
            in
                pr (pre ^
                    Node.name_of n ^ "\t->\t" ^ StrPak.stringListString s ^
                    post)
            end
    in
        List.app f l;
        !outstr
    end

fun writeI(j:AbsMach.opcode list, p:Node.program list) =
    let val labelid = ref 0
        fun newlabel () = (labelid := !labelid + 1; !labelid - 1)
        fun bentrymap nil = (fn x => (print ("bentrymap_" ^ x); raise WRITEI))
          | bentrymap ((ns, n0, F)::t) =
            let val mp = bentrymap t
                val mylab = newlabel()
            in
                (fn x => if x = Node.name_of n0 then mylab else mp x)
            end
        val entry_map = bentrymap p
        val sp = sortP.sort p
        fun wp p = writeP (entry_map, newlabel, p)
        fun f(a, b) = (wp a) @ b
        val i = fold f sp nil
    in
        i @ j
    end
            

end



signature SIMLABS =
  sig
    exception Data_dependency_checked
    exception End_of_Program
    exception Simulator_error_1
    exception Simulator_error_2
    exception illegal_branch_within_branchdelay
    exception illegal_jump_within_branchdelay
    exception illegal_operator_or_operand
    exception negative_label_offset
    exception no_address_in_register
    exception no_label_in_register
    exception no_memory_address_in_register
    exception runtime_error_in_labwords
    exception runtime_error_in_words_or_labwords
    exception type_mismatch_in_comparison
    exception wrong_label
    val breakptr : int -> unit
    val clock : int ref
    val d_m : int * int -> unit
    val d_ms : int list -> unit
    val d_pc : unit -> unit
    val d_r : unit -> unit
    val d_regs : int list -> unit
    val init : AbsMach.opcode list -> unit
    val mcell : int -> AbsMach.values
    val pc : unit -> AbsMach.opcode list
    val pinit : int * (AbsMach.arithop -> int) * int * AbsMach.opcode list 
                -> unit
    val pptr : unit -> int
    val prun : unit -> unit
    val pstep : unit -> unit
    val regc : int -> AbsMach.values
    val run : unit -> unit
    val runcount : int ref
    val step : unit -> unit
    val vinit : int * AbsMach.opcode list -> unit
    val vpc : unit -> unit
    val vrun1 : unit -> unit
    val vrun2 : unit -> unit
    val vrun3 : unit -> unit
    val vstep1 : unit -> unit
    val vstep2 : unit -> unit
    val vstep3 : unit -> unit

    val Memory : (AbsMach.values array) ref
  end;


structure SetEnv : SIMLABS=
struct

  open AbsMach;
  
  val codes : (opcode list ref)=ref nil;

  val RegN=ref 0 and LabN=ref 0 and memorysize=ref 10000;
  (*RegN = (pointer to) number of registers needed;
    LabN = (pointer to) number of labels;
    memorysize=(pointer to) memory space size.
   *)
  val IP: (opcode list) ref =ref nil;
  val inivalue=(INT 0);
  (*IP = Program Pointer;
    inivalue = zero- initial value of memory and registers.
   *)
  val Reg=ref (array(0,inivalue)) and Memory=ref (array(0,inivalue))
      and Lab_Array=ref (array(0, (0,IP) ));
  (*Reg = register array;
    Memory = memory cell array;
    Lab_Array = label-opcode list array.
   *)

  fun max(n1:int,n2:int)=if (n1>n2) then n1 else n2;

  (* hvnop tests whether the instruction is not a real machine instruction,
     but only useful in simulation.
   *)
  fun hvnop(LABEL{...})=true |
      hvnop(LABWORD{...})=true |
      hvnop(WORD{...})=true |
      hvnop(_)=false;

  (*count_number is used to take into account register references and label
    declarations, and change RegN or LabN.
   *)
  fun count_number(FETCH {ptr=(n1,_),dst=(n2,_),...})=
      (RegN:=max((!RegN),max(n1,n2)) )             |
      count_number(STORE {src=(n1,_),ptr=(n2,_),...})=
      (RegN:=max((!RegN),max(n1,n2)) )             |
      count_number(ARITHI {src1=(n1,_),dst=(n2,_),...})=
      (RegN:=max((!RegN),max(n1,n2)) )             |
      count_number(MOVE {src=(n1,_),dst=(n2,_)})=
      (RegN:=max((!RegN),max(n1,n2)) )             |
      count_number(BRANCH {src1=(n1,_),src2=(n2,_),...})=
      (RegN:=max((!RegN),max(n1,n2)) )             |
      count_number(GETLAB {dst=(n,_),...})=
      (RegN:=max((!RegN),n) )                      |
      count_number(GETREAL {dst=(n,_),...})=
      (RegN:=max((!RegN),n) )                      |
      count_number(ARITH{src1=(n1,_),src2=(n2,_),dst=(n3,_),...})=
      (RegN:=max((!RegN),max(n1,max(n2,n3)) )  )   |
      count_number(LABEL{...})=
      ( Ref.inc(LabN) )                            |
      count_number(_)=();

  (* scan is used to scan the opcode list for the first time, to determine
     the size of Reg and Lab_Array, i.e. number of registers and labels.
   *)
  fun scan(nil)=() |
      scan(h::t)=(count_number(h);scan(t));
  
  (* setlabels is used to set the label array, of which each item is a 
     pair (label, codep), codep points to the codes containing the LABEL
     statement and afterwards codes.
   *)
  fun setlabels(nil,_)= () |
      setlabels(codel as ((LABEL {lab=(l,_),...})::t),k)=
      (update((!Lab_Array),k,(l,ref codel)); setlabels(t,k+1) ) |
      setlabels(h::t,k)=setlabels(t,k) ;
  
  (* initializing the enviroment of the simulation.
   *)
  fun init(l)=(RegN:=0; LabN:=0; IP:=l; codes:=l;
              scan(!IP); Ref.inc(RegN);
              Reg:=array( (!RegN), inivalue ) ;
              Memory:=array( (!memorysize), inivalue ) ;
              Lab_Array:=array( (!LabN), (0,IP));
              setlabels(!IP,0) 
              );



  exception wrong_label;
  exception runtime_error_in_labwords;
  exception runtime_error_in_words_or_labwords;
  exception negative_label_offset;
  exception no_label_in_register;
  exception illegal_operator_or_operand;
  exception type_mismatch_in_comparison ;
  exception no_address_in_register;
  exception no_memory_address_in_register;

  (* getresult gives the results of arithmtic operations      
   *)
  fun getresult(iadd,INT (n1:int),INT (n2:int))=INT (n1+n2) |
      getresult(isub,INT (n1:int),INT (n2:int))=INT (n1-n2) |
      getresult(imul,INT (n1:int),INT (n2:int))=INT (n1*n2) |
      getresult(idiv,INT (n1:int),INT (n2:int))=INT (n1 div n2) |
      getresult(fadd,REAL (r1:real),REAL (r2:real))=REAL (r1+r2) |
      getresult(fsub,REAL (r1:real),REAL (r2:real))=REAL (r1-r2) |
      getresult(fmul,REAL (r1:real),REAL (r2:real))=REAL (r1*r2) |
      getresult(fdiv,REAL (r1:real),REAL (r2:real))=REAL (r1/r2) |
      getresult(iadd,INT (n1:int),LABVAL (l,k))=LABVAL (l,k+n1)  |
      getresult(iadd,LABVAL (l,k),INT (n1:int))=LABVAL (l,k+n1)  |
      getresult(isub,LABVAL (l,k),INT (n1:int))=LABVAL (l,k-n1)  |
      getresult(orb,INT n1,INT n2)=INT (Bits.orb(n1,n2))         |
      getresult(andb,INT n1,INT n2)=INT (Bits.andb(n1,n2))       |
      getresult(xorb,INT n1,INT n2)=INT (Bits.xorb(n1,n2))       |
      getresult(rshift,INT n1,INT n2)=INT (Bits.rshift(n1,n2))   |
      getresult(lshift,INT n1,INT n2)=INT (Bits.lshift(n1,n2))   |
      getresult(real,INT n,_)=REAL (intToReal(n))                  |
      getresult(floor,REAL r,_)=INT (Real.floor(r))                |
(*    getresult(logb,REAL r,_)=INT (System.Unsafe.Assembly.A.logb(r))| *)
      getresult(_)=raise illegal_operator_or_operand;

  (* compare gives the results of comparisons in BRANCH statement.
   *)
  fun compare(ilt,INT n1,INT n2)= (n1<n2) |
      compare(ieq,INT n1,INT n2)= (n1=n2) |
      compare(igt,INT n1,INT n2)= (n1>n2) |
      compare(ile,INT n1,INT n2)= (n1<=n2) |
      compare(ige,INT n1,INT n2)= (n1>=n2) |
      compare(ine,INT n1,INT n2)= (n1<>n2) |
      compare(flt,REAL r1,REAL r2)= (r1<r2) |
      compare(feq,REAL r1,REAL r2)= (realEq(r1,r2)) |
      compare(fgt,REAL r1,REAL r2)= (r1>r2) |
      compare(fle,REAL r1,REAL r2)= (r1<=r2) |
      compare(fge,REAL r1,REAL r2)= (r1>=r2) |
      compare(fne,REAL r1,REAL r2)= (realNe(r1,r2)) |
      compare(inrange,INT a,INT b)= (a>=0) andalso (a<b) |
      compare(outofrange,INT a,INT b)=(a<0) orelse (a>b) |
      compare(inrange,REAL a,REAL b)= (a>=0.0) andalso (a<b) |
      compare(outofrange,REAL a,REAL b)=(a<0.0) orelse (a>b) |
      compare(_)=raise type_mismatch_in_comparison ;

  (* findjmp_place returns the pointer to the codes corresponding to the 
     given label (the codes containing the LABEL statement itself).
   *)
  fun findjmp_place lab =
      let val ipp=ref (ref nil) and i=ref 0 and flag=ref true;
          val none=(while ( (!i < !LabN) andalso (!flag) ) do
                       (  let val (l,p)=((!Lab_Array) sub (!i)) in 
                          if (l=lab) then (ipp:=p;flag:=false)
                                     else Ref.inc(i)
                          end
                          )
                     )
      in if (!flag) then raise wrong_label
                   else (!ipp)           
      end; 

  (* findjmp_word returns the content of the k th labword in a code stream.
   *)
  fun findjmp_word(k,ip)=if (k<0) then raise negative_label_offset
                         else let fun f2(1,LABWORD{lab=(herepos,_)}::t)
                                                                   =herepos |
                                      f2(k,LABWORD{...}::t)=f2(k-1,t)       |
                                      f2(_)=raise runtime_error_in_labwords ;
                              in  f2(k, (!ip) )
                              end;

  (* inst_word returns the content of the k'th word or labword in a code
     stream.
   *)
  fun inst_word(k,ip)=if (k<0) then raise negative_label_offset
                      else let fun f(1,LABWORD{lab=(herepos,_)}::t)
                                   =LABVAL (herepos,0)              |
                                   f(1,WORD{value=n}::t)=INT n      |
                                   f(k,LABWORD{...}::t)=f(k-1,t)    |
                                   f(k,WORD{...}::t)=f(k-1,t)       |
                                   f(_)=raise 
                                          runtime_error_in_words_or_labwords   
                           in f(k,(!ip))
                           end;
                

  (* execjmp changes IP, makes it point to the codes of the given label.
   *)
  fun execjmp(LABVAL (l,0))= (IP:= !(findjmp_place l) ) |
      execjmp(LABVAL (l,k))= (IP:= 
                              ! (findjmp_place
                                   (findjmp_word(k,findjmp_place(l) ) ) )
                                 )                      |
      execjmp(_) = raise no_label_in_register;

  (* addrplus returns the result of address+offset.
   *)
  fun addrplus(INT n,ofst)= n+ofst |
      addrplus(_,_)=raise no_memory_address_in_register;

  (* content gives the content of the fetched word.
   *)
  fun content(INT n,ofst)= (!Memory) sub (n+ofst) |
      content(LABVAL (l,k),ofst)=inst_word(k+ofst,findjmp_place(l))    |
      content(_,_)=raise no_address_in_register;

  (* exec executes the given instruction.
   *)
  fun exec(FETCH{immutable=_,offset=ofst,ptr=(p,_),dst=(d,_)})=
        update((!Reg),d,content((!Reg) sub p,ofst) )                   |
      exec(STORE{offset=ofst,src=(s,_),ptr=(p,_)})=
        update((!Memory),addrplus((!Reg) sub p,ofst),(!Reg) sub s)     |
      exec(GETLAB {lab=(l,_),dst=(d,_)})=
        update((!Reg),d,(LABVAL (l,0)) )                       |
      exec(GETREAL {value=v,dst=(d,_)})=
        update((!Reg),d,(REAL (strToReal v)))                  |
      exec(MOVE{src=(s,_),dst=(d,_)})=
        update((!Reg),d, (!Reg) sub s )                        |
      exec(LABEL {...})= 
        ()                                                     |
      exec(LABWORD {...}) = 
        ()                                                     |
      exec(WORD{...})=
        ()                                                     |
      exec(JUMP {dst=(d,_),...})=
        execjmp((!Reg) sub d)                                  |
      exec(ARITH {oper=opn,src1=(s1,_),src2=(s2,_),dst=(d,_)})= 
        update((!Reg),d,getresult(opn,(!Reg) sub s1,(!Reg) sub s2) )   |
      exec(ARITHI {oper=opn,src1=(s1,_),src2=n1,dst=(d,_)})=
        update((!Reg),d,getresult(opn,(!Reg) sub s1,(INT n1) ) )       |
      exec(BRANCH{test=comp,src1=(s1,_),src2=(s2,_),dst=(labnum,_),...})=
        if compare(comp,(!Reg) sub s1,(!Reg) sub s2) 
        then (IP:= !(findjmp_place(labnum) ) )
        else ()                                                        |
      exec(NOP)= () |
      exec(BOGUS _)= raise Match
            
      ;



  exception End_of_Program;

  fun step () =let 
                 val Instruction=(hd(!IP) handle Hd=> raise End_of_Program) 
               in
               (IP:=tl(!IP) handle Tl=>raise End_of_Program;
                exec(Instruction) )
               end;
  fun run () =(step();run() )
              handle End_of_Program =>output(std_out,"End of program\n");

  (* bms, ims, rms are simply abbreviations.
   *)
  val bms : bool -> string = Bool.toString
  and ims : int -> string = Int.toString 
  and rms : real -> string = Real.toString

  (* dispv shows the content of a register, dispm shows the content of a 
     memory word.
   *)
  fun dispv(n,INT k)=output(std_out,"Register "^ims(n)^": "^
                                    "INT "^ims(k)^"\n") |
      dispv(n,REAL r)=output(std_out,"Register "^ims(n)^": "^
                                    "REAL "^rms(r)^"\n") |
      dispv(n,LABVAL (l,0))=output(std_out,
                                   "Register "^ims(n)^": "^
                                   "LABEL "^ims(l)^"\n") |
      dispv(n,LABVAL (l,k))=output(std_out,
                                   "Register "^ims(n)^": "^
                                   "LABWORD "^ims(k)^" after"^
                                   "LABEL "^ims(l)^"\n") ;

  fun dispm(n,INT k)=output(std_out,"Memory "^ims(n)^": "^
                                    "INT "^ims(k)^"\n") |
      dispm(n,REAL r)=output(std_out,"Memory "^ims(n)^": "^
                                    "REAL "^rms(r)^"\n") |
      dispm(n,LABVAL (l,0))=output(std_out,
                                   "Memory "^ims(n)^": "^
                                   "LABEL "^ims(l)^"\n") |
      dispm(n,LABVAL (l,k))=output(std_out,
                                   "Memory "^ims(n)^": "^
                                   "LABWORD "^ims(k)^" after"^
                                   "LABEL "^ims(l)^"\n") ;

  (* oms and cms give the strings of the functions and comparisions.
   *)
  fun oms(iadd)="iadd" | oms(isub)="isub" |
      oms(imul)="imul" | oms(idiv)="idiv" |
      oms(fadd)="fadd" | oms(fsub)="fsub" |
      oms(fmul)="fmul" | oms(fdiv)="fdiv" |
      oms(real)="real" | oms(floor)="floor" | oms(logb)="logb" |
      oms(orb)="orb" | oms(andb)="andb" | oms(xorb)="xorb" |
      oms(rshift)="rshift" | oms(lshift)="lshift" ;

  fun cms(ilt)="ilt" | cms(igt)="igt" | cms(ieq)="ieq" |
      cms(ile)="ile" | cms(ige)="ige" | cms(ine)="ine" |
      cms(flt)="flt" | cms(fgt)="fgt" | cms(feq)="feq" |
      cms(fle)="fle" | cms(fge)="fge" | cms(fne)="fne" |
      cms(outofrange)="outofrange" | cms(inrange)="inrange" ;
  
  (* lms gives the string of the live register list.
   *)
  fun lms(nil)="" | 
      lms((h,s)::nil)="("^ims(h)^","^s^")" | 
      lms((h,s)::t)="("^ims(h)^","^s^"),"^lms(t);

  (* disp gives the string for the instruction.
   *)
  fun disp(FETCH{immutable=b,offset=ofst,ptr=(p,s1),dst=(d,s2)}) =
      "FETCH{immutable="^bms(b)^",offset="^ims(ofst) ^",ptr=("^ims(p)^","^s1
      ^"),dst=("^ims(d)^","^s2^")}\n"                                      |

      disp(STORE{offset=ofst,src=(s,s1),ptr=(p,s2)}) =
      "STORE{offset="^ims(ofst)^",src=("^ims(s)^","^s1^"),ptr=("
      ^ims(p)^","^s2^")}\n"                                                |

      disp(GETLAB{lab=(l,ls),dst=(d,ds)}) =
      "GETLAB{lab=("^ims(l)^","^ls^"),dst=("^ims(d)^","^ds^")}\n"          |

      disp(GETREAL{value=r,dst=(d,ds)}) =
      "GETREAL{value="^r^",dst=("^ims(d)^","^ds^")}\n"                |

      disp(ARITH{oper=opn,src1=(s1,ss1),src2=(s2,ss2),dst=(d,ds)})=
      "ARITH{oper="^oms(opn)^",src1=("^ims(s1)^","^ss1^"),src2=("^ims(s2)
      ^","^ss2^"),dst=("^ims(d)^","^ds^")}\n"                              |

      disp(ARITHI{oper=opn,src1=(s1,ss1),src2=n,dst=(d,ds)})=
      "ARITH{oper="^oms(opn)^",src1=("^ims(s1)^","^ss1^"),src2="^ims(n)^
      ",dst=("^ims(d)^","^ds^")}\n"                                        |

      disp(MOVE{src=(s,ss),dst=(d,ds)})=
      "MOVE{src=("^ims(s)^","^ss^"),dst=("^ims(d)^","^ds^")}\n"            |

      disp(BRANCH{test=comp,src1=(s1,ss1),src2=(s2,ss2),dst=(labnum,ss3),
                  live=lt})=
      "BRANCH{test="^cms(comp)^",src1=("^ims(s1)^","^ss1^"),src2=("^ims(s2)
      ^","^ss2^"),dst=("^ims(labnum)^","^ss3^"),live=["^lms(lt)^"]}\n"     |

      disp(JUMP{dst=(d,ds),live=lt}) = 
      "JUMP{dst=("^ims(d)^","^ds^"),live=["^lms(lt)^"]}\n"                 |
                
      disp(LABWORD{lab=(l,s)})="LABWORD{lab=("^ims(l)^","^s^")}\n"         |

      disp(LABEL{lab=(l,s),live=lt})=
      "LABEL{lab=("^ims(l)^","^s^"),live=["^lms(lt)^"]}\n"                 |

      disp(WORD{value=n})="WORD{value="^ims(n)^"}\n"                       |

      disp(NOP)="NOP" |
      disp(BOGUS _) = raise Match

      ;

  fun d_pc () =output(std_out,disp(hd(!IP)) handle Hd=>"No More Instruction\n");
  fun pc () = (!IP);
  fun pptr () =(List.length(!codes)-List.length(!IP))+1;
  fun breakptr k=let fun goon (LABEL {lab=(l,_),...})=(l<>k) |
                         goon (_)=true
                 in while goon(hd(!IP)) do step()
                 end;
  fun regc n=((!Reg) sub n);
  fun d_r () =let val i=ref 0 in
                (while ( !i < !RegN) do
                   (dispv((!i),(!Reg) sub (!i)); Ref.inc(i) )
                 )
                end;
  fun d_regs (nil)=() |
      d_regs (h::t)=(dispv(h,(!Reg) sub h);d_regs(t));

  fun mcell n=((!Memory) sub n);
  fun d_m (n,m)=let val i=ref n in
                while ( !i <=m) do (dispm(!i,(!Memory) sub !i); Ref.inc(i) )
                end;
  fun d_ms nil =() |
      d_ms (h::t)=(dispm(h,(!Memory) sub h); d_ms(t) );


(* This part for the VLIW mode execution.                                  *)


  val runcount=ref 0 and sizen=ref 0 and flag=ref true; 
  exception Simulator_error_1;
  exception Simulator_error_2;
  exception Data_dependency_checked;

  (* member tests whether element a is in a list.
   *)
  fun member(a,nil)=false |
      member(a,h::t)=if (a=h) then true else member(a,t);
  (* hvcom tests whether the intersection of two list isnot nil.
   *)
  fun hvcom(nil,l)=false |
      hvcom(h::t,l)=member(h,l) orelse hvcom(t,l);
  
  (* gset returns the list of registers refered in a instruction.
     gwset returns the list of the register being written in a instruction.
   *)
  fun gset(FETCH{ptr=(p,_),dst=(d,_),...})=[p,d] |
      gset(STORE{src=(s,_),ptr=(p,_),...})=[s,p] |
      gset(GETLAB{dst=(d,_),...})=[d] |
      gset(GETREAL{dst=(d,_),...})=[d] |
      gset(ARITH{src1=(s1,_),src2=(s2,_),dst=(d,_),...})=[s1,s2,d] |
      gset(ARITHI{src1=(s1,_),dst=(d,_),...})=[s1,d] |
      gset(MOVE{src=(s,_),dst=(d,_)})=[s,d] |
      gset(BRANCH{src1=(s1,_),src2=(s2,_),...})=[s1,s2] |
      gset(JUMP{dst=(d,_),...})=[d] |
      gset(_)=nil ;
  fun gwset(FETCH{dst=(d,_),...})=[d] |
      gwset(GETLAB{dst=(d,_),...})=[d] |
      gwset(GETREAL{dst=(d,_),...})=[d] |
      gwset(ARITH{dst=(d,_),...})=[d] |
      gwset(ARITHI{dst=(d,_),...})=[d] |
      gwset(MOVE{dst=(d,_),...})=[d] |
      gwset(_)=nil ;
  
  (* fetchcode returns the instruction word which contains the next k 
     instruction.  fetchcode3 is used in version 3 of VLIW mode, in which case
     labels within instruction words are OK.
   *)
  fun fetchcode(0)=nil |
      fetchcode(k)=let val h=hd(!IP) in
                     (IP:=tl(!IP); 
                      if hvnop(h) 
                      then (output(std_out,
                            "Warning: labels within the instruction word\n");
                            fetchcode(k)
                            )
                      else h::fetchcode(k-1) )
                   end handle Hd=>nil;
  fun fetchcode3(0)=nil |
      fetchcode3(k)=let val h=hd(!IP) in
                     (IP:=tl(!IP); 
                      if hvnop(h) then fetchcode3(k)
                                  else h::fetchcode3(k-1) )
                   end handle Hd=>nil;

  (* allnop tests if all instructions left mean no operation.
   *)
  fun allnop(nil)=true |
      allnop(NOP::t)=allnop(t) |
      allnop(_)=false;

  (* nopcut cut the instruction stream in a way that the first half are all 
     NOP instruction.
   *)
  fun nopcut(nil)=(nil,nil) |
      nopcut(NOP::t)=let val (l1,l2)=nopcut(t) in (NOP::l1,l2) end |
      nopcut(l)=(nil,l);

  (* cmdd tests the data dependency on memory cells and IP.
   *)
  fun cmdd(_,nil)=false |
      cmdd(wset,STORE{ptr=(p,_),offset=ofst,...}::t)=
        cmdd(addrplus((!Reg) sub p,ofst)::wset,t) |
      cmdd(wset,FETCH{ptr=(p,_),offset=ofst,...}::t)=
        member(addrplus((!Reg) sub p,ofst),wset) orelse cmdd(wset,t) |
      cmdd(wset,BRANCH{...}::t)=if allnop(t) then false else true    |
      cmdd(wset,JUMP{...}::t)=if allnop(t) then false else true      |
      cmdd(wset,h::t)=cmdd(wset,t);

  (* crdd test the data dependency on registers.
   *)
  fun crdd(_,nil)=false |
      crdd(wset,h::t)=if hvcom(gset(h),wset) then true 
                      else crdd(gwset(h)@wset,t) ;

  (* check_dd checks whether there is data dependency in instruction stream l.
   *)
  fun check_dd(l)= crdd(nil,l) orelse cmdd(nil,l); 

  (* rddcut seperate the longest part of the instruction stream that have no 
     data dependency on registers , from the left.
   *)
  fun rddcut(_,nil)= (nil,nil)                                   |
      rddcut(wset,l as (h::t))=
        if hvcom(gset(h),wset) then (nil,l)
        else  let val (l1,l2)=rddcut(gwset(h)@wset,t)
              in (h::l1,l2) end
      ;
  (* mddcut seperate the longest part of the instruction stream that have no data
     dependency on memory cells and IP, from the left.
   *)
  fun mddcut(_,nil)= (nil,nil)                                   |
      mddcut(wset,(h as STORE{ptr=(p,_),offset=ofst,...})::t)=
        let val (l1,l2)=mddcut(addrplus((!Reg) sub p,ofst)::wset,t)
        in (h::l1,l2) end                                        |
      mddcut(wset,(h as FETCH{ptr=(p,_),offset=ofst,...})::t)=
        if member(addrplus((!Reg) sub p,ofst),wset) 
        then (nil,h::t) 
        else let val (l1,l2)=mddcut(wset,t) in (h::l1,l2) end    |
      mddcut(wset,(h as BRANCH{...})::t)=
        let val (l1,l2)=nopcut(t) in (h::l1,l2) end              |
      mddcut(wset,(h as JUMP{...})::t)=
        let val (l1,l2)=nopcut(t) in (h::l1,l2) end              |
      mddcut(wset,h::t)=
        let val (l1,l2)=mddcut(wset,t) in (h::l1,l2) end    
      ;
  
  (* calcult returns the necessary value list corresponding to a instruction 
     stream.  And change the IP when necessary.
   *)
  fun calcult(nil)=nil                                                    |
      calcult(FETCH{ptr=(p,_),offset=ofst,...}::t)=
        content((!Reg) sub p,ofst)::calcult(t)                            |
      calcult(STORE{src=(s,_),...}::t)=((!Reg) sub s )::calcult(t)        |
      calcult(MOVE{src=(s,_),...}::t)=((!Reg) sub s)::calcult(t)          |
      calcult(ARITH{oper=opn,src1=(s1,_),src2=(s2,_),...}::t)=
         getresult(opn,(!Reg) sub s1,(!Reg) sub s2)::calcult(t)           |
      calcult(ARITHI{oper=opn,src1=(s1,_),src2=n1,...}::t)=
         getresult(opn,(!Reg) sub s1,(INT n1))::calcult(t)                |
      calcult(JUMP{dst=(d,_),...}::t)=((!Reg) sub d)::calcult(t)          |
      calcult(h::t)=calcult(t);

  (* dowr does the actual writing operations.
   *)
  fun dowr(nil,nil)=() |
      dowr(nil,h::t)=raise Simulator_error_1                              |
      dowr(FETCH{...}::t,nil)=raise Simulator_error_2                     |
      dowr(STORE{...}::t,nil)=raise Simulator_error_2                     |
      dowr(MOVE{...}::t,nil)=raise Simulator_error_2                      |
      dowr(ARITH{...}::t,nil)=raise Simulator_error_2                     |
      dowr(ARITHI{...}::t,nil)=raise Simulator_error_2                    |
      dowr(JUMP{...}::t,nil)=raise Simulator_error_2                      |
      dowr(FETCH{dst=(d,_),...}::t,vh::vt)=(update((!Reg),d,vh);
                                            dowr(t,vt) )                  |
      dowr(STORE{ptr=(p,_),offset=ofst,...}::t,vh::vt)=
        (update((!Memory),addrplus((!Reg) sub p,ofst),vh); dowr(t,vt) )   |
      dowr(GETLAB{lab=(l,_),dst=(d,_)}::t,vt)=
        (update((!Reg),d,(LABVAL (l,0)) ); dowr(t,vt) )                   |
      dowr(GETREAL{value=v,dst=(d,_)}::t,vt)=
        (update((!Reg),d,(REAL (strToReal v)) ); dowr(t,vt) )                         |
      dowr(MOVE{dst=(d,_),...}::t,vh::vt)=
        (update((!Reg),d,vh); dowr(t,vt) )                                |
      dowr(ARITH{dst=(d,_),...}::t,vh::vt)=
        (update((!Reg),d,vh); dowr(t,vt) )                                |
      dowr(ARITHI{dst=(d,_),...}::t,vh::vt)=
        (update((!Reg),d,vh); dowr(t,vt) )                                |
      dowr(JUMP{...}::t,vh::vt)=
        (execjmp(vh); flag:=false; dowr(t,vt) )                           |
      dowr(BRANCH{test=comp,src1=(s1,_),src2=(s2,_),
                     dst=(labnum,_),...}::t,vt)=
        if compare(comp,(!Reg) sub s1,(!Reg) sub s2) 
        then (IP:= !(findjmp_place(labnum)); flag:=false; dowr(t,vt) )
        else dowr(t,vt)                                                   |
      dowr(h::t,vt)=dowr(t,vt)                                            
      ;

  (* vv3 executes an instruction word in version 3 mode.
   *)
  fun vv3(nil)= () |
      vv3(l)=let val (l1,l2)=rddcut(nil,l);
                 val (l3,l4)=mddcut(nil,l1)
             in (flag:=true; dowr(l3,calcult(l3)); Ref.inc(runcount);
                 if (!flag) then vv3(l4@l2) else () )
             end;

  fun vinit(k,l)=(init(l); sizen:=k; runcount:=0 ) ;

  fun vstep1()=let val f=(while hvnop(hd(!IP)) do IP:=tl(!IP))
                         handle Hd=>raise End_of_Program;
                   val codel=fetchcode(!sizen) 
               in
                 (dowr(codel,calcult(codel)); Ref.inc(runcount) ) 
               end;

  fun vstep2()=let val f=(while hvnop(hd(!IP)) do IP:=tl(!IP))
                         handle Hd=>raise End_of_Program;
                   val codel=fetchcode(!sizen) 
               in
                 if check_dd(codel) 
                 then (output(std_out,"Data dependency checked in:\n");
                       let fun f(nil)=() |
                               f(h::t)=(output(std_out,":"^disp(h)); f(t))
                       in f(codel) end;
                       raise Data_dependency_checked
                       )
                 else (dowr(codel,calcult(codel)); Ref.inc(runcount) )
               end;
                  
  fun vstep3()=let val f=if (!IP)=nil then raise End_of_Program else ();
                   val codel=fetchcode3(!sizen) 
               in vv3(codel) end;

  fun vrun1()=(vstep1();vrun1()) 
              handle End_of_Program =>
                     output(std_out,"End of program.\nTotal runtime: "
                                    ^ims(!runcount)^" steps.\n");
  fun vrun2()=(vstep2(); vrun2()) 
              handle End_of_Program =>
                     output(std_out,"End of program.\nTotal runtime: "
                                    ^ims(!runcount)^" steps.\n")|
                     Data_dependency_checked=>
                     output(std_out,"Program halted.\n") ;
  fun vrun3()=(vstep3(); vrun3())
              handle End_of_Program =>
                     output(std_out,"End of program.\nTotal runtime: "
                                    ^ims(!runcount)^" substeps.\n");

  fun vpc()=let val codel=(!IP) ;
                fun f (_,nil)=() |
                    f (0,_)= () |
                    f (k,h::l)=if k<=0 then ()
                            else (output(std_out,disp(h) );
                                  if hvnop(h) then f(k,l)
                                  else f(k-1,l) )
            in f((!sizen),codel) end;
                

(*  This part for Pipeline mode                                 *)


  exception illegal_jump_within_branchdelay;
  exception illegal_branch_within_branchdelay;
  exception illegal_label_within_branchdelay;
  exception illegal_labword_within_branchdelay;
  exception illegal_word_within_branchdelay;
  (* Rdelay points to the timing array of registers.
   *)
  val Rdelay=ref ( array(0,0) ); 
  (* clock records run time.  withindelay is a flag used in BRANCH and JUMP delays.
   *)
  val clock=ref 0 and withindelay=ref false;
  val fdelay=ref 1 and ardelay: ((arithop->int) ref)=ref (fn k=>1) 
      and jdelay=ref 1;

  (* pexec executes one instruction, increasing the clock when necessary, which 
     corresponding to the holding down of instruction streams.
   *)
  fun pexec(FETCH{immutable=_,offset=ofst,ptr=(p,_),dst=(d,_)})=
        (let val t=(!Rdelay) sub p in 
           if (!clock)<t then clock:=t else ()
         end;
         update((!Reg),d,content((!Reg) sub p,ofst) );
         update((!Rdelay),d,(!clock)+(!fdelay));
         Ref.inc(clock)
         )                                                              |
      pexec(STORE{offset=ofst,src=(s,_),ptr=(p,_)})=
        (let val t1=((!Rdelay) sub p) and t2=((!Rdelay) sub s) ;
             val t=Int.max(t1,t2)  in
           if (!clock)<t then clock:=t else ()
         end;
         update((!Memory),addrplus((!Reg) sub p,ofst),(!Reg) sub s);
         Ref.inc(clock)
         )                                                             |
      pexec(GETLAB{lab=(l,_),dst=(d,_)})=
        (update((!Reg),d,(LABVAL (l,0)) );
         Ref.inc(clock)
         )                                                             |
      pexec(GETREAL{value=v,dst=(d,_)})=
        (update((!Reg),d,(REAL (strToReal v)) );
         Ref.inc(clock)
         )                                                             |
      pexec(MOVE{src=(s,_),dst=(d,_)})=
        (let val t=(!Rdelay) sub s in
           if (!clock)<t then clock:=t else ()
         end;
         update((!Reg),d,(!Reg) sub s);
         Ref.inc(clock)
         )                                                             |
      pexec(ARITH{oper=opn,src1=(s1,_),src2=(s2,_),dst=(d,_)})=
        (let val t1=((!Rdelay) sub s1) and t2=((!Rdelay) sub s2);
             val t=Int.max(t1,t2) in
           if (!clock)<t then clock:=t else ()
         end;
         update((!Reg),d,getresult(opn,(!Reg) sub s1,(!Reg) sub s2) );
         update((!Rdelay),d,((!ardelay) opn)+(!clock) );
         Ref.inc(clock)
         )                                                             |
      pexec(ARITHI{oper=opn,src1=(s1,_),src2=n1,dst=(d,_)})=
        (let val t=((!Rdelay) sub s1) in
           if (!clock)<t then clock:=t else ()
         end;
         update((!Reg),d,getresult(opn,(!Reg) sub s1,(INT n1) ) );
         update((!Rdelay),d,((!ardelay) opn)+(!clock) );
         Ref.inc(clock)
         )                                                             |
      pexec(JUMP {dst=(d,_),...})=
        if (!withindelay) then raise illegal_jump_within_branchdelay
        else 
        (let val t=((!Rdelay) sub d) in
           if (!clock)<t then clock:=t else ()
         end;
         Ref.inc(clock); withindelay:=true;
         let val i=ref 0 in
           while ((!i)<(!jdelay)) do 
             (let val h=hd(!IP) in
                ( pexec(h); Ref.inc(i) )
              end handle Hd=> (i:=(!jdelay) ) ;
              (IP:=tl(!IP)) handle Tl=>()
              )
         end;
         execjmp((!Reg) sub d)
         )                                                             |
      pexec(BRANCH{test=comp,src1=(s1,_),src2=(s2,_),dst=(labnum,_),...})=
        if (!withindelay) then raise illegal_branch_within_branchdelay
        else 
        (let val t1=((!Rdelay) sub s1) and t2=((!Rdelay) sub s2);
             val t=Int.max(t1,t2) in
           if (!clock)<t then clock:=t else ()
         end;
         Ref.inc(clock); withindelay:=true;
         let val i=ref 0 in
           while ((!i)<(!jdelay)) do 
             (let val h=hd(!IP) in
                ( pexec(h); Ref.inc(i) )
              end handle Hd=> (i:=(!jdelay) ) ;
              (IP:=tl(!IP)) handle Tl=>()
              )
         end;
         if compare(comp,(!Reg) sub s1,(!Reg) sub s2) 
         then (IP:= !(findjmp_place(labnum) ) )
         else ()
         )                                                             |
      pexec(NOP)=Ref.inc(clock)                                        |
      pexec(LABEL{...})=if (!withindelay) 
                        then raise illegal_label_within_branchdelay 
                        else ()                                        |
      pexec(LABWORD{...})=if (!withindelay) 
                          then raise illegal_labword_within_branchdelay 
                          else ()                                      |
      pexec(WORD{...})=if (!withindelay) 
                       then raise illegal_word_within_branchdelay 
                       else ()    
      ;
       
  fun pinit(fetchdelay,arithdelay,jumpdelay,l)=
       (init(l);
        Rdelay:=array((!RegN),0); 
        clock:=0; fdelay:=fetchdelay; 
        ardelay:=arithdelay; jdelay:=jumpdelay );

  fun pstep()=
    let 
      val Instruction=(hd(!IP) handle Hd=>raise End_of_Program)
    in (IP:=tl(!IP) handle Tl=>raise End_of_Program;
        withindelay:=false; pexec(Instruction) )
    end;

  fun prun()=(pstep(); prun() ) handle End_of_Program=>
             (output(std_out,"End of program.\n");
              output(std_out,"Total time used: "^ims(!clock)^" cycles.\n") );

end;
structure SimStuff =
struct

fun read file =
    let val if1 = (open_in "simprelude.s")
        val if2 = (open_in file)
        val if3 = (open_in "simpostlude.s")
        val prelude = ReadAbs.read if1
        val prog = ReadAbs.read if2
        val postlude = ReadAbs.read if3
    in
        close_in if1;
        close_in if2;
        close_in if3;
        prelude @ prog @ postlude
    end

fun init file = SetEnv.init (read file)

val runcount = ref 0

fun run ()=
    let open AbsMach
        val foo = runcount := 0
        fun updc NOP = runcount := !runcount + 1
          | updc _ = ()
        open SetEnv
        fun f () = (step(); (updc o hd o pc)(); f())
    in
        f()
    end

fun srun () = let open SetEnv in d_pc(); step(); srun() end;

fun memsave () = !SetEnv.Memory


fun memcmp(a:AbsMach.values array, b:AbsMach.values array) = 
    let open AbsMach
        fun cmp (INT a, INT b) = a = b
          | cmp (REAL a, REAL b) = realEq(a, b)
          | cmp (LABVAL _, LABVAL _) = true
          | cmp _ = false
        fun f 0 = ~1
          | f n = if cmp((a sub n), (b sub n)) then f (n - 1) else n
        val al = Array.length a
        val bl = Array.length b
    in
        if al = bl then f (al - 1) else (print "size\n"; 0)
    end


fun copyarray a =
    let val la = Array.length a
        val na = array(la, a sub 0)
        fun f n = if n > 0 then (update(na, n, a sub n) ; f (n - 1)) else ()
        val foo = f (la - 1)
    in
        na
    end


exception PROG_NO_END

local open AbsMach
in
    fun vstring (INT i) = "INT " ^ makestring i
      | vstring (REAL i) = "REAL " ^ Real.toString i
      | vstring (LABVAL(i, j)) =
        "LABVAL(" ^ makestring i ^ ", " ^ makestring j ^ ")"
end

fun runf f = 
    ((init f;
      run ();
      raise PROG_NO_END))
    handle End_of_Program => (print "eop\n";
                              SetEnv.regc 4)
                               
    
fun cmprog(f1, f2) =
    let open AbsMach
        fun intof (INT i) = i
        fun ptsat p = SetEnv.mcell (intof p)
        val p1 = runf f1
        (* val foo = print ("cmprog1:" ^ vstring p1 ^ "\n") *)
        val v1 = ptsat p1 
        val r1 = !runcount
        val p2 = runf f2
        (* val foo = print ("cmprog2:" ^ vstring p2 ^ "\n") *)
        val v2 = ptsat p2
        val r2 = !runcount

    in
        (f1 ^ " ct " ^ makestring r1 ^ " ptr " ^ vstring p1 ^
          " val " ^ vstring v1 ^ 
         f2 ^ " ct " ^ makestring r2 ^ " ptr " ^ vstring p2 ^
         " val " ^ vstring v2 ^  "\n")
    end

end

fun time str f =
    let (* open System.Timer
        val s = start_timer() *)
        val v = f()
        (*
        val e = check_timer s
        val foo = print (str ^ " took " ^ makestring e ^ "sec.usec\n")
        *)
    in
        v
    end


fun writeprog(file, j, p) =
    let val ot = (open_out file)
        val prog = ReadI.writeI(j, p)
        val filp = (Delay.rm_bogus o OutFilter.remnops) prog
        val xxx = PrintAbs.show ot filp
    in
        close_out ot
    end;
   
fun wp(file, prog) =
    let val ot = (open_out file)
        val filp = Delay.rm_bogus prog
        val xxx = PrintAbs.show ot filp
    in
        close_out ot
    end;
     
fun dodelay i = (Delay.init i; Delay.add_delay i);
    
val _ = (
Node.move_test_debug := false;
Node.move_op_debug := false;
Node.rw_debug := false;
Node.delete_debug := false;
Node.ntn_debug := true;
Node.prog_node_debug := false;
Node.prog_node_debug_verbose := false;
Node.closure_progs_debug := false;
Node.cpsiCheck := false;
Compress.compress_debug := false;
ReadI.read_debug := false;
ReadI.write_debug := false;
ReadI.live_debug := false
)
    
fun pm pl = print (StrPak.stringListString (map ReadI.progMap pl));
fun pp pl = print (StrPak.stringListString (map PrintAbs.str pl));
    
fun ndnm nil = raise Node.NAMETONODE
| ndnm(h::t) = (fn (nm) => Node.nameToNode(h, nm)
                handle Node.NAMETONODE => ndnm t nm);

exception ERROR;

fun err (s:string) = (print s; raise ERROR);

fun pmem nil = (err "oh well")
  | pmem ((ns, n0, f)::t) =
    fn n => if Set.member(ns, n) then (ns, n0, f)
            else pmem t n;

structure Main = struct

fun doitx (ifile:string, ofile:string, c_ofile:string, ws:int) =
let val foo = Ntypes.init_names()
   val ins = open_in ifile
    val i = (dodelay o BreakInst.breaki o ReadAbs.read) ins
   val _ = close_in ins
    val (j, p) = time "Building Nodes" (fn () => ReadI.readI i)
    val x = time "writing unopt" (fn () => writeprog(ofile, j, p))
    fun cwin p = Compress.compress(ws, p)
    val cp = time "compressing program" (fn () => map cwin p)
    val xx = time "writing opt program" (fn () => writeprog(c_ofile, j, cp))
    val answer = "" (* SimStuff.cmprog(ofile, c_ofile) *)
    val code_motions = Ntypes.new_name "0"
in
    print (answer ^ "code_motions " ^ code_motions ^ " \n")
end

fun main(s:string list, env:string list) =
    let val idemp = ref 0
        val ws = ref 0
        val ifile = ref "/dev/null"
        val ofile = ref "/dev/null"
        val c_ofile = ref "/dev/null"
        val gotifile = ref false
        val gotofile = ref false
        fun digit d =
        if ord d >= ord "0" andalso ord d <= ord "9" then ord d - ord "0"
            else err ("expected digit. got " ^ d)
        val parse =
        fn ("-" :: "i" :: "d" :: "e" :: "m" :: d :: nil) =>
        idemp := digit d
         | ("-" :: "w" :: "s" :: d :: nil) =>
               ws := digit d
         | ("-" :: t)  =>
               (print ("usage: comp [-ws#] [-idem#]" ^
                       "input_file temp_file compressed_file\n");
                print ("ws is the window size\nidem is the idempotency\n");
                err "exiting")
         | s  => if !gotofile then c_ofile := implode s
                 else if !gotifile then (gotofile := true;
                                         ofile := implode s)
                      else (gotifile := true;
                            ifile := implode s)
        val foo = List.app (parse o explode) (tl s)
        val foo = print ("compressing " ^ !ifile ^ " into (uncompressed)" ^
                         !ofile ^
                         " and (compressed)" ^ !c_ofile ^
                         " with idempotency " ^ makestring (!idemp) ^
                         " and window size " ^ makestring (!ws) ^ "\n")
    in
        Delay.idempotency := !idemp;
        doitx(!ifile, !ofile, !c_ofile, !ws)
    end

val s = OS.FileSys.getDir()

fun doit() = main(["foobar", "-ws9", 
                   s^"/DATA/ndotprod.s", 
                   s^"/DATA/tmp.s", 
                   s^"/DATA/cmp.s"], 
                  nil)
fun testit _ = ()
end 

structure Main : BMARK =
   struct
      open Main

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
