(* Modified by sweeks@sweeks.com 2001-10-03 to go in the MLton benchmark suite.
 * Hardwired in the u6 list of polynomials and added a loop.
 *)
(* tyan.sml
 * A Grobner Basis calculation for polynomials over F17
 * Adapted from the TIL benchmark suite by Allyn Dimock:
 * update to SML '97, Standard Basis Library, comment out unreachable code
 * Original code from Thomas Yan, who has given his permission for this
 * code be used as a benchmarking code for SML compilers 
 * (e-mail message Tue, 10 Apr 2001 13:07:44 -0400 (EDT))
 *
 * The data structure for the intermediate results is described in
 * @article{Yan:1998:GBSP,
 *   author = {Yan, Thomas},
 *   title = {The Geobucked Data Structure For Polynomials},
 *   journal = {Journal Of Symbolic Computation},
 *   volume = 23,
 *   number = 3,
 *   pages = {285 -- 293},
 *   year = 1998,
 * }
 *)

val print : string -> unit = print
type 'a array1 = 'a Array.array
val sub1 = Array.sub
val update1 = Array.update
val array1 = Array.array
val length1 = Array.length
val op && = fn (i1, i2) => (Word.toInt (Word.andb (Word.fromInt (i1), Word.fromInt (i2))))
val op || = fn (i1, i2) => (Word.toInt (Word.orb (Word.fromInt (i1), Word.fromInt (i2))))
val op << = fn (i1, i2) => (Word.toInt (Word.<< (Word.fromInt (i1), Word.fromInt (i2))))
val op >> = fn (i1, i2) => (Word.toInt (Word.>> (Word.fromInt (i1), Word.fromInt (i2))))
infix && || << >>
fun fold f l b = List.foldl f b l
fun revfold f l b = List.foldr f b l

val input_line = TextIO.inputLine
val end_of_stream = TextIO.endOfStream
val open_in = TextIO.openIn
val close_in = TextIO.closeIn

nonfix smlnj_mod
nonfix smlnj_div
val smlnj_mod = op mod
val smlnj_div = op div
infix 7 smlnj_mod
infix 7 smlnj_div

exception Tabulate
fun tabulate (i,f) = 
  if i <= 0 then raise Tabulate else
    let val a = array1(i,f 0)
      fun tabify j = if j < i then (update1(a,j,f j); tabify (j+1)) else a
    in
      tabify 1
    end

exception ArrayofList
fun arrayoflist (hd::tl) =
  let val a = array1((length tl) + 1,hd)
    fun al([],_) = a
      | al(hd::tl,i) = (update1(a,i,hd); al(tl,i+1))
  in
    al(tl,1)
  end
  | arrayoflist ([]) = raise ArrayofList


structure Util = struct
    datatype relation = Less | Equal | Greater

    exception NotImplemented of string
    exception Impossible of string (* flag "impossible" condition  *)
    exception Illegal of string (* flag function use violating precondition *)

    fun error exn msg = raise (exn msg)
    fun notImplemented msg = error NotImplemented msg
    fun impossible msg = error Impossible msg
    fun illegal msg = error Illegal msg

    (* arr[i] := obj :: arr[i]; extend non-empty arr if necessary *)
    fun insert (obj,i,arr) = let
          val len = length1 arr
          val res =  if i<len then (update1(arr,i,obj::sub1(arr,i)); arr)
             else let val arr' = array1(Int.max(i+1,len+len),[])
                      fun copy ~1 = (update1(arr',i,[obj]); arr')
                        | copy j = (update1(arr',j,sub1(arr,j));
                                    copy(j-1))
                      in copy(len-1) end 
          in res
          end

(*
    fun arrayoflists [] = arrayoflist []
      | arrayoflists ([]::ls) = arrayoflists ls
      | arrayoflists [l] = arrayoflist l
      | arrayoflists (ls as (obj0::_)::_) = let  
          val a = array1(revfold (fn (l,n) => length l + n) ls 0,obj0)
          fun ins (i,[]) = i | ins (i,x::l) = (update1(a,i,x); ins(i+1,l))
          fun insert (i,[]) = a | insert (i,l::ll) = insert(ins(i,l),ll)
          in insert(0,ls) end
*)

    (* given compare and array a, return list of contents of a sorted in
     * ascending order, with duplicates stripped out; which copy of a duplicate
     * remains is random.  NOTE that a is modified.
     *)
    fun stripSort compare = fn a => let
          infix sub


          val op sub = sub1 and update = update1
          fun swap (i,j) = let val ai = a sub i
                           in update(a,i,a sub j); update(a,j,ai) end
          (* sort all a[k], 0<=i<=k<j<=length a *)
          fun s (i,j,acc) = if i=j then acc else let
                val pivot = a sub ((i+j) smlnj_div 2)
                fun partition (lo,k,hi) = if k=hi then (lo,hi) else
                      case compare (pivot,a sub k) of
                          Less => (swap (lo,k); partition (lo+1,k+1,hi))
                        | Equal => partition (lo,k+1,hi)
                        | Greater => (swap (k,hi-1); partition (lo,k,hi-1))
                val (lo,hi) = partition (i,i,j)
                in s(i,lo,pivot::s(hi,j,acc)) end
           val res = s(0,length1 a,[]) 

          in 
           res
          end
end

structure F = struct
    val p = 17

    datatype field = F of int (* for (F n), always 0<=n<p *)
    (* exception Div = Integer.Div *)
(* unused code unless P.show, commented out in earlier version, is used
    fun show (F x) = print (Int.toString x)
*)
(* unused code
    val char = p
*)

(* unused code unless P.display is used
    val zero = F 0
*)
    val one = F 1
    fun coerceInt n = F (n smlnj_mod p)

    fun add (F n,F m) = let val k = n+m in if k>=p then F(k-p) else F k end
    fun subtract (F n,F m) = if n>=m then F(n-m) else F(n-m+p)
    fun negate (F 0) = F 0 | negate (F n) = F(p-n)
    fun multiply (F n,F m) = F ((n*m) smlnj_mod p)
    fun reciprocal (F 0) = raise Div
      | reciprocal (F n) = let
          (* consider euclid gcd alg on (a,b) starting with a=p, b=n.
           * if maintain a = a1 n + a2 p, b = b1 n + b2 p, a>b,
           * then when 1 = a = a1 n + a2 p, have a1 = inverse of n mod p
           * note that it is not necessary to keep a2, b2 around.
           *)
          fun gcd ((a,a1),(b,b1)) =
              if b=1 then (* by continued fraction expansion, 0<|b1|<p *)
                 if b1<0 then F(p+b1) else F b1
              else let val q = a smlnj_div b
                   in gcd((b,b1),(a-q*b,a1-q*b1)) end
          in gcd ((p,0),(n,1)) end
(* unused code
    fun divide (n,m) = multiply (n, reciprocal m)
*)

(* unused code unless power is used
    val andb = op &&
    val rshift = op >>
*)

(* unused code
    fun power(n,k) =
          if k<=3 then case k of
              0 => one
            | 1 => n
            | 2 => multiply(n,n)
            | 3 => multiply(n,multiply(n,n))
            | _ => reciprocal (power (n,~k)) (* know k<0 *)
          else if andb(k,1)=0 then power(multiply(n,n),rshift(k,1))
               else multiply(n,power(multiply(n,n),rshift(k,1)))
*)

    fun isZero (F n) = n=0
(* unused codeunless P.display is used
    fun equal (F n,F m) = n=m

    fun display (F n) = if n<=p smlnj_div 2 then Int.toString n
                        else "-" ^ Int.toString (p-n)
*)
end

structure M = struct (* MONO *)
    local
        val andb = op &&
        infix sub << >> andb
(*      val op << = Bits.lshift and op >> = Bits.rshift and op andb = Bits.andb
*)
    in

(* encode (var,pwr) as a long word: hi word is var, lo word is pwr
   masks 0xffff for pwr, mask ~0x10000 for var, rshift 16 for var
   note that encoded pairs u, v have same var if u>=v, u andb ~0x10000<v
*)

    datatype mono = M of int list
(*
    fun show (M x) = (print "<"; app (fn i => (print (Int.toString i); print ",")) x; print">")
*)
    exception DoesntDivide

(* unused code
    val numVars = 32
*)

    val one = M []
    fun x_i v = M [(v<<16)+1]
    fun explode (M l) = map (fn v => (v>>16,v andb 65535)) l
    fun implode l = M (map (fn (v,p) => (v<<16)+p) l)

    val deg = let fun d([],n) = n | d(u::ul,n) = d(ul,(u andb 65535) + n)
              in fn (M l) => d(l,0) end

    (* x^k > y^l if x>k or x=y and k>l *)
    val compare = let
          fun cmp ([],[]) = Util.Equal
            | cmp (_::_,[]) = Util.Greater
            | cmp ([],_::_) = Util.Less
            | cmp ((u::us), (v::vs)) = if u=v then cmp (us,vs)
                                  else if u<v then Util.Less
                                  else (* u>v *)   Util.Greater
          in fn (M m,M m') => cmp(m,m') end

    fun display (M (l : int list)) : string = 
      let
        fun dv v = if v<26 then chr (v+ord #"a") else chr (v-26+ord #"A")
        fun d (vv,acc) = let val v = vv>>16 and p = vv andb 65535
                         in if p=1 then dv v::acc
                            else 
                              (dv v)::(String.explode (Int.toString p)) @ acc
                         end
      in String.implode(fold d l []) end

    val multiply = let
          fun mul ([],m) = m
            | mul (m,[]) = m
            | mul (u::us, v::vs) = let
                val uu = u andb ~65536
                in if uu = (v andb ~65536) then let
                      val w = u + (v andb 65535)
                      in if uu = (w andb ~65536) then w::mul(us,vs)
                         else 
                           (Util.illegal 
                            (String.concat ["Mono.multiply overflow: ",
                                            display (M(u::us)),", ",
                                            display (M(v::vs))]))
                      end
                   else if u>v then u :: mul(us,v::vs)
                   else (* u<v *) v :: mul(u::us,vs)
                end
          in fn (M m,M m') => M (mul (m,m')) end

    val lcm = let
          fun lcm ([],m) = m
            | lcm (m,[]) = m
            | lcm (u::us, v::vs) =
                if u>=v then if (u andb ~65536)<v then u::lcm(us,vs)
                                                    else u::lcm(us,v::vs)
                        else if (v andb ~65536)<u then v::lcm(us,vs)
                                                    else v::lcm(u::us,vs)
          in fn (M m,M m') => M (lcm (m,m')) end
    val tryDivide = let
          fun rev([],l) = l | rev(x::xs,l)=rev(xs,x::l)
          fun d (m,[],q) = SOME(M(rev(q,m)))
            | d ([],_::_,_) = NONE
            | d (u::us,v::vs,q) =
                if u<v then NONE
                else if (u andb ~65536) = (v andb ~65536) then
                    if u=v then d(us,vs,q) else d(us,vs,u-(v andb 65535)::q)
                else d(us,v::vs,u::q)
          in fn (M m,M m') => d (m,m',[]) end
    fun divide (m,m') =
          case tryDivide(m,m') of SOME q => q | NONE => raise DoesntDivide

end end (* local, structure M *)

structure MI = struct (* MONO_IDEAL *)

    (* trie:
     * index first by increasing order of vars
     * children listed in increasing degree order
     *)
    datatype 'a mono_trie = MT of 'a option * (int * 'a mono_trie) list
                            (* tag, encoded (var,pwr) and children *)
    datatype 'a mono_ideal = MI of (int * 'a mono_trie) ref
                            (* int maxDegree = least degree > all elements *)
    
    fun rev ([],l) = l | rev (x::xs,l) = rev(xs,x::l)
(* unused code
    fun tl (_::l) = l | tl [] = raise (Util.Impossible "MONO_IDEAL.tl")
    fun hd (x::_) = x | hd [] = raise (Util.Impossible "MONO_IDEAL.hd")
*)
    val emptyTrie = MT(NONE,[])
    fun mkEmpty () = MI(ref (0,emptyTrie))

(* unused code unless searchDeg is used
    fun maxDeg (MI(x)) = #1(!x)
*)

    val lshift = op <<
(* unused code unless decode is used 
    val rshift = op >>
*)
    val andb = op &&
(* unused code
    val orb = op ||
*)
    fun encode (var,pwr) = lshift(var,16)+pwr
(* unused code
    fun decode vp = (rshift(vp,16),andb(vp,65535))
*)
    fun grabVar vp = andb(vp,~65536)
    fun grabPwr vp = andb(vp,65535)
    fun smallerVar (vp,vp') = vp < andb(vp',~65536)

    exception Found
    fun search (MI(x),M.M m') = let 
          val (d,mt) = !x
          val result = ref NONE
          (* exception Found of M.mono * '_a *)
          (* s works on remaining input mono, current output mono, tag, trie *)
          fun s (_,m,MT(SOME a,_)) =
                raise(result := SOME (M.M m,a); Found)
            | s (m',m,MT(NONE,trie)) = s'(m',m,trie)
          and s'([],_,_) = NONE
            | s'(_,_,[]) = NONE
            | s'(vp'::m',m,trie as (vp,child)::children) =
                if smallerVar(vp',vp) then s'(m',m,trie)
                else if grabPwr vp = 0 then (s(vp'::m',m,child);
                                             s'(vp'::m',m,children))
                else if smallerVar(vp,vp') then NONE 
                else if vp<=vp' then (s(m',vp::m,child);
                                      s'(vp'::m',m,children))
                else NONE
          in s(rev(m',[]),[],mt)
             handle Found (* (m,a) => SOME(m,a) *) => !result
          end

   (* assume m is a new generator, i.e. not a multiple of an existing one *)
    fun insert (MI (mi),m,a) = let
          val (d,mt) = !mi
          fun i ([],MT (SOME _,_)) = Util.illegal "MONO_IDEAL.insert duplicate"
            | i ([],MT (NONE,children)) = MT(SOME a,children)
            | i (vp::m,MT(a',[])) = MT(a',[(vp,i(m,emptyTrie))])
            | i (vp::m,mt as MT(a',trie as (vp',_)::_)) = let
                fun j [] = [(vp,i(m,emptyTrie))]
                  | j ((vp',child)::children) =
                      if vp<vp' then (vp,i(m,emptyTrie))::(vp',child)::children
                      else if vp=vp' then (vp',i(m,child))::children
                      else (vp',child) :: j children
                in 
                   if smallerVar(vp,vp') then
                       MT(a',[(grabVar vp,MT(NONE,trie)),(vp,i(m,emptyTrie))])
                   else if smallerVar(vp',vp) then i(grabVar vp'::vp::m,mt)
                   else MT(a',j trie)
                end
          in mi := (Int.max(d,M.deg m),i (rev(map encode(M.explode m),[]),mt)) end

    fun mkIdeal [] = mkEmpty() 
      | mkIdeal (orig_ms : (M.mono * '_a) list)= let
          fun ins ((m,a),arr) = Util.insert((m,a),M.deg m,arr)
          val msa = arrayoflist orig_ms
          val ms : (M.mono * '_a) list = 
              Util.stripSort (fn ((m,_),(m',_)) => M.compare (m,m')) msa
          val buckets = revfold ins ms (array1(0,[]))
          val n = length1 buckets
          val mi = mkEmpty()
          fun sort i = if i>=n then mi else let
                fun redundant (m,_) = case search(mi,m) of NONE => false
                                                         | SOME _ => true
                fun filter ([],l) = app (fn (m,a) => insert(mi,m,a)) l
                  | filter (x::xx,l) = if redundant x then filter(xx,l)
                                       else filter(xx,x::l)
                in filter(sub1(buckets,i),[]);
                   update1(buckets,i,[]);
                   sort(i+1)
                end
          in sort 0 end

    fun fold g (MI(x)) init = let
          val (_,mt) = !x
          fun f(acc,m,MT(NONE,children)) = f'(acc,m,children)
            | f(acc,m,MT(SOME a,children)) =
                f'(g((M.M m,a),acc),m,children)
          and f'(acc,m,[]) = acc
            | f'(acc,m,(vp,child)::children) =
                if grabPwr vp=0 then f'(f(acc,m,child),m,children)
                else f'(f(acc,vp::m,child),m,children)
          in f(init,[],mt) end

(* unused code
    fun searchDeg (mi,d) =
          if d>maxDeg mi then []
          else fold (fn ((m,a),l) => if M.deg m=d then (m,a)::l else l) mi []
*)

end (* structure MI *)


val log = let fun log(n,l) = if n<=1 then l else log((n >> 1),1+l)
          in fn n => log(n,0) end
val maxLeft = ref 0
val maxRight = ref 0
val counts = tabulate(20,fn _ => array1(20,0))
val indices = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]

(* unused code
fun resetCounts() = app(fn i => app (fn j => update1(sub1(counts,i),j,0)) indices) indices
*)

fun pair(l,r) = let
      val l = log l and r = log r
      val _ = maxLeft := Int.max(!maxLeft,l) and _ = maxRight := Int.max(!maxRight,r)
      val a = sub1(counts,l)
      in update1(a,r,sub1(a,r)+1) end
(* unused code unless printCounts is used
fun getCounts () = 
  map (fn i => map (fn j => sub1(sub1(counts,i),j)) indices) indices
*)

structure P = struct

    datatype poly = P of (F.field*M.mono) list (* descending mono order *)
(*      
    fun show (P x) = (print "[ "; 
                      app (fn (f, m) =>
                           (print "("; F.show f; print ","; M.show m; print ") ")) x;
                      print " ]")
*)
    val zero = P []
(* unused code unless power is used
    val one = P [(F.one,M.one)]
*)
(* unused code
    fun coerceInt n = P [(F.coerceInt n,M.one)]
    fun coerceField a = P [(a,M.one)]
    fun coerceMono m = P [(F.one,m)]
*)
    fun coerce (a,m) = P [(a,m)]
    fun implode p = P p
    fun cons (am,P p) = P (am::p)

local
    fun neg p = (map (fn (a,m) => (F.negate a,m)) p)
    fun plus ([],p2) = p2
      | plus (p1,[]) = p1
      | plus ((a,m)::ms,(b,n)::ns) = case M.compare(m,n) of
            Util.Less => (b,n) :: plus ((a,m)::ms,ns)
          | Util.Greater => (a,m) :: plus (ms,(b,n)::ns)
          | Util.Equal => let val c = F.add(a,b)
                             in if F.isZero c then plus(ms,ns)
                                else (c,m)::plus(ms,ns)
                             end
    fun minus ([],p2) = neg p2
      | minus (p1,[]) = p1
      | minus ((a,m)::ms,(b,n)::ns) = case M.compare(m,n) of
            Util.Less => (F.negate b,n) :: minus ((a,m)::ms,ns)
          | Util.Greater => (a,m) :: minus (ms,(b,n)::ns)
          | Util.Equal => let val c = F.subtract(a,b)
                             in if F.isZero c then minus(ms,ns)
                                else (c,m)::minus(ms,ns)
                             end
    fun termMult (a,m,p) =
          (map (fn (a',m') => (F.multiply(a,a'),M.multiply(m,m'))) p)
in
(* unused code
    fun negate (P p) = P (neg p)
*)
    fun add (P p1,P p2) = (pair(length p1,length p2); P (plus(p1,p2)))
    fun subtract (P p1,P p2) = (pair(length p1,length p2); P (minus(p1,p2)))

(* unused code unless power is used
    val multiply = let
          fun times (p1,p2) = 
                revfold (fn ((a,m),tot) => plus (termMult(a,m,p2),tot)) p1 []
          in fn (P p1,P p2) => if length p1 > length p2 then P(times (p2,p1))
                               else P(times (p1,p2))
          end
*)

(* unused code
    fun singleReduce (P y,a,m,P x) = 
      (pair(length y,length x); P(minus(y,termMult(a,m,x))))
*)

    fun spair (a,m,P f,b,n,P g) = 
      (pair(length f,length g); P(minus(termMult(a,m,f),termMult(b,n,g))))
    val termMult = fn (a,m,P f) => P(termMult(a,m,f))
end

(* unused code unless power is used
    val rshift = op >>
    val lshift = op <<
*)

(* unused code
    val andb = op &&
    val orb = op ||
*)

    fun scalarMult (a,P p) = P (map (fn (b,m) => (F.multiply(a,b),m)) p)


(* unused code
    fun power(p,k) =
          if k<=3 then case k of
              0 => one
            | 1 => p
            | 2 => multiply(p,p)
            | 3 => multiply(p,multiply(p,p))
            | _ => Util.illegal "POLY.power with k<0"
          else if andb(k,1)=0 then power(multiply(p,p),rshift(k,1))
               else multiply(p,power(multiply(p,p),rshift(k,1)))
*)

    fun isZero (P []) = true | isZero (P (_::_)) = false

(* unused code
    val equal = let
          fun eq ([],[]) = true
            | eq (_::_,[]) = false
            | eq ([],_::_) = false
            | eq ((a,m)::p,(b,n)::q) =
                F.equal(a,b) andalso M.compare(m,n)=Util.Equal
                andalso eq (p,q)
          in fn (P p,P q) => eq (p,q) end
*)

    (* these should only be called if there is a leading term, i.e. poly<>0 *)
(* unused code
    fun leadTerm (P(am::_)) = am
      | leadTerm (P []) = Util.illegal "POLY.leadTerm"
*)
    fun leadMono (P((_,m)::_)) = m
      | leadMono (P []) = Util.illegal "POLY.leadMono"
    fun leadCoeff (P((a,_)::_)) = a
      | leadCoeff (P []) = Util.illegal "POLY.leadCoeff"
    fun rest (P (_::p)) = P p
      | rest (P []) = Util.illegal "POLY.rest"
    fun leadAndRest (P (lead::rest)) = (lead,P rest)
      | leadAndRest (P []) = Util.illegal "POLY.leadAndRest"

    fun deg (P []) = Util.illegal "POLY.deg on zero poly"
      | deg (P ((_,m)::_)) = M.deg m (* homogeneous poly *)
    fun numTerms (P p) = length p

(* only used if r is used
    fun display (P []) = F.display F.zero
      | display (P p) = let
          fun dsp (a,m) = let
                val s = 
                      if M.deg m = 0 then F.display a
                      else if F.equal(F.one,F.negate a) then "-" ^ M.display m
                      else if F.equal(F.one,a) then M.display m
                      else F.display a ^ M.display m
                in if substring(s,0,1)="-" then s else "+" ^ s end
          in String.concat(map dsp p) end
*)
end

structure HP = struct
        datatype hpoly = HP of P.poly array1
        val log = let
              fun log(n,l) = if n<8 then l else log((n >> 2),1+l)
              in fn n => log(n,0) end
        fun mkHPoly p = let
              val l = log(P.numTerms p)
              in HP(tabulate(l+1,fn i => if i=l then p else P.zero)) end
        fun add(p,HP ps) = let
              val l = log(P.numTerms p)
              in if l>=length1 ps then let
                   val n = length1 ps
                   in HP(tabulate(n+n,
                         fn i => if i<n then sub1(ps,i)
                                 else if i=l then p else P.zero))
                   end
                 else let
                   val p = P.add(p,sub1(ps,l))
                   in if l=log(P.numTerms p) then (update1(ps,l,p); HP ps)
                      else (update1(ps,l,P.zero); add (p,HP ps))
                   end
              end
        fun leadAndRest (HP ps) = let
              val n = length1 ps
              fun lar (m,indices,i) = if i>=n then lar'(m,indices) else let
                    val p = sub1(ps,i)
                    in if P.isZero p then lar(m,indices,i+1)
                       else if null indices then lar(P.leadMono p,[i],i+1)
                            else case M.compare(m,P.leadMono p) of
                                Util.Less => lar(P.leadMono p,[i],i+1)
                              | Util.Equal => lar(m,i::indices,i+1)
                              | Util.Greater => lar(m,indices,i+1)
                    end
              and lar' (_,[]) = NONE
                | lar' (m,i::is) = let
                    fun extract i = case P.leadAndRest(sub1(ps,i)) of
                          ((a,_),rest) => (update1(ps,i,rest); a)
                    val a = revfold (fn (j,b) => F.add(extract j,b))
                                    is (extract i)
                    in if F.isZero a then lar(M.one,[],0) else SOME(a,m,HP ps)
                    end
              in lar(M.one,[],0) end
end

structure G = struct
    val autoReduce = ref true
    val maxDeg = ref 10000
    val maybePairs = ref 0
    val primePairs = ref 0
    val usedPairs = ref 0
    val newGens = ref 0

    fun reset () = (maybePairs:=0; primePairs:=0; usedPairs:=0; newGens:=0)

    fun inc r = r := !r + 1

    fun reduce (f,mi) = if P.isZero f then f else let
          (* use accumulator and reverse at end? *)
          fun r hp = case HP.leadAndRest hp of
                NONE => []
              | (SOME(a,m,hp)) => case MI.search(mi,m) of
                    NONE => (a,m)::(r hp)
                  | SOME (m',p) => r (HP.add(P.termMult(F.negate a,M.divide(m,m'),!p),hp))
          in P.implode(r (HP.mkHPoly f)) end

    (* assume f<>0 *)
    fun mkMonic f = P.scalarMult(F.reciprocal(P.leadCoeff f),f)

    (* given monic h, a monomial ideal mi of m's tagged with g's representing
     * an ideal (g1,...,gn): a poly g is represented as (lead mono m,rest of g).
     * update pairs to include new s-pairs induced by h on g's:
     * 1) compute minimal gi1...gik so that <gij:h's> generate <gi:h's>, i.e.
     *    compute monomial ideal for gi:h's tagged with gi
     * 2) toss out gij's whose lead mono is rel. prime to h's lead mono (why?)
     * 3) put (h,gij) pairs into degree buckets: for h,gij with lead mono's m,m'
     *    deg(h,gij) = deg lcm(m,m') = deg (lcm/m) + deg m = deg (m':m) + deg m
     * 4) store list of pairs (h,g1),...,(h,gn) as vector (h,g1,...,gn)
     *)
    fun addPairs (h,mi,pairs) = let
          val m = P.leadMono h
          val d = M.deg m
          fun tag ((m' : M.mono,g' : P.poly ref),quots) = (inc maybePairs;
                                     (M.divide(M.lcm(m,m'),m),(m',!g'))::quots)
          fun insert ((mm,(m',g')),arr) = (* recall mm = m':m *)
                if M.compare(m',mm)=Util.Equal then (* rel. prime *)
                    (inc primePairs; arr)
                else (inc usedPairs;
                      Util.insert(P.cons((F.one,m'),g'),M.deg mm+d,arr))
          val buckets = MI.fold insert (MI.mkIdeal (MI.fold tag mi []))
                                       (array1(0,[]))
          fun ins (~1,pairs) = pairs
            | ins (i,pairs) = case sub1(buckets,i) of
                    [] => ins(i-1,pairs)
                  | gs => ins(i-1,Util.insert(arrayoflist(h::gs),i,pairs))
          in ins(length1 buckets - 1,pairs) end

    fun grobner fs = let
         fun pr l = print (String.concat (l@["\n"]))
          val fs = revfold (fn (f,fs) => Util.insert(f,P.deg f,fs))
                           fs (array1(0,[]))
          (* pairs at least as long as fs, so done when done w/ all pairs *)
          val pairs = ref(array1(length1 fs,[]))
          val mi = MI.mkEmpty()
          val newDegGens = ref []
          val addGen = (* add and maybe auto-reduce new monic generator h *)
                if not(!autoReduce) then
                    fn h => MI.insert (mi,P.leadMono h,ref (P.rest h))
                else fn h => let
                    val ((_,m),rh) = P.leadAndRest h
                    fun autoReduce f =
                          if P.isZero f then f
                          else let val ((a,m'),rf) = P.leadAndRest f
                               in case M.compare(m,m') of
                                   Util.Less => P.cons((a,m'),autoReduce rf)
                                 | Util.Equal => P.subtract(rf,P.scalarMult(a,rh))
                                 | Util.Greater => f
                               end
                    val rrh = ref rh
                    in
                        MI.insert (mi,P.leadMono h,rrh);
                        app (fn f => f:=autoReduce(!f)) (!newDegGens);
                        newDegGens := rrh :: !newDegGens
                    end
          val tasksleft = ref 0
          fun feedback () = let
                val n = !tasksleft
                in 
                    if (n && 15)=0 then print (Int.toString n) else (); 
                        print "."; 
                        TextIO.flushOut TextIO.stdOut;
                        tasksleft := n-1
                end

          fun try h = 
              let
                  val _ = feedback ()
                  val h = reduce(h,mi)
              in if P.isZero h 
                     then ()
                 else let val h = mkMonic h
                          val _ = (print "#"; TextIO.flushOut TextIO.stdOut)
                      in pairs := addPairs(h,mi,!pairs);
                          addGen h;
                          inc newGens
                      end
              end

          fun tryPairs fgs = let
                val ((a,m),f) = P.leadAndRest (sub1(fgs,0))
                fun tryPair i = if i=0 then () else let
                      val ((b,n),g) = P.leadAndRest (sub1(fgs,i))
                      val k = M.lcm(m,n)
                      in 
                         try (P.spair(b,M.divide(k,m),f,a,M.divide(k,n),g));
                         tryPair (i-1)
                      end
                in tryPair (length1 fgs -1) end

          fun numPairs ([],n) = n
            | numPairs (p::ps,n) = numPairs(ps,n-1+length1 p)

          fun gb d = if d>=length1(!pairs) then mi else
                (* note: i nullify entries to reclaim space *)
                (
pr ["DEGREE ",Int.toString d," with ",
    Int.toString(numPairs(sub1(!pairs,d),0))," pairs ",
    if d>=length1 fs then "0" else Int.toString(length(sub1(fs,d))),
      " generators to do"];
                 tasksleft := numPairs(sub1(!pairs,d),0);
                 if d>=length1 fs then () 
                 else tasksleft := !tasksleft + length (sub1(fs,d));
                   if d>(!maxDeg) then ()
                   else (             
                         reset();
                         newDegGens := [];
                         app tryPairs (sub1(!pairs,d));
                         update1(!pairs,d,[]);
                         if d>=length1 fs then ()
                         else (app try (sub1(fs,d)); update1(fs,d,[]));
                           pr ["maybe ",Int.toString(!maybePairs)," prime ",
                               Int.toString (!primePairs),
                               " using ",Int.toString (!usedPairs),
                               "; found ",Int.toString (!newGens)]
                           );
                 gb(d+1)
                )
          in gb 0 end

local
    (* grammar:
     dig  ::= 0 | ... | 9
     var  ::= a | ... | z | A | ... | Z
     sign ::= + | -
     nat  ::= dig | nat dig
     mono ::=  | var mono | var num mono
     term ::= nat mono | mono
     poly ::= term | sign term | poly sign term
    *)
    datatype char = Dig of int | Var of int | Sign of int
    fun char ch =
        let val och = ord ch in
          if ord #"0"<=och andalso och<=ord #"9" then Dig (och - ord #"0")
          else if ord #"a"<=och andalso och<=ord #"z" then Var (och - ord #"a")
          else if ord #"A"<=och andalso och<=ord #"Z" then Var (och - ord #"A" + 26)
          else if och = ord #"+" then Sign 1
          else if och = ord #"-" then Sign ~1
               else Util.illegal ("bad ch in poly: " ^ (Char.toString(ch)))
        end

    fun nat (n,Dig d::l) = nat(n*10+d,l) | nat (n,l) = (n,l)
    fun mono (m,Var v::Dig d::l) =
          let val (n,l) = nat(d,l)
          in mono(M.multiply(M.implode[(v,n)],m),l) end
      | mono (m,Var v::l) = mono(M.multiply(M.x_i v,m),l)
      | mono (m,l) = (m,l)

    fun term l = let
          val (n,l) = case l of (Dig d::l) => nat(d,l) | _ => (1,l)
          val (m,l) = mono(M.one,l)
          in ((F.coerceInt n,m),l) end
    fun poly (p,[]) = p
      | poly (p,l) = let
          val (s,l) = case l of Sign s::l => (F.coerceInt s,l) | _ => (F.one,l)
          val ((a,m),l) = term l
          in poly(P.add(P.coerce(F.multiply(s,a),m),p),l) end

in
    fun parsePoly s = poly (P.zero,map char(String.explode s))

(* unused code
    fun readIdeal stream = let
          fun readLine () = let
                val s = input_line stream
                val n = size s
                val n = if n>0 andalso substring(s,n-1,1)="\n" then n-1 else n
                fun r i = if i>=n then []
                          else case substring(s,i,1) of
                              ";" => r(i+1)
                            | " " => r(i+1)
                            | _ => map char (String.explode(substring(s,i,n-i)))
                in r 0 end
          fun r () = if end_of_stream stream then []
                     else poly(P.zero,readLine()) :: r()
          fun num() = if end_of_stream stream then Util.illegal "missing #"
                      else case nat(0,readLine()) of
                          (_,_::_) => Util.illegal "junk after #"
                        | (n,_) => n
          val _ = 1=num() orelse Util.illegal "stream doesn't start w/ `1'"
          val n = num()
          val i = r()
          val _ = length i = n orelse Util.illegal "wrong # poly's"
          in i end
*)

(* unused code
fun read filename = let
      val stream = open_in filename
      val i = readIdeal stream
      val _ = close_in stream
      in i end
*)
end (* local *) 

end (* structure G *)



val _ = G.maxDeg:=1000000

fun grab mi = MI.fold (fn ((m,g),l) => P.cons((F.one,m),!g)::l) mi []

(* unused code
fun r mi s = let
      val p = G.parsePoly s
      in print (P.display p); print "\n";
         print (P.display(G.reduce(p,mi))); print "\n"
      end
*)

(* unused code unless printCounts is used
fun p6 i= let val s= Int.toString (i:int)
              val n= size s
          in print(substring("      ",0,6-n)); print s end
*)

(* unused code
fun hex n = let
      fun h n = if n=0 then ""
                else h(n smlnj_div 16) ^ substring("0123456789ABCDEF",n smlnj_mod 16,1)
      in if n=0 then "0" else h n end
fun printCounts () = map (fn l => (map p6 l; print "\n")) (getCounts())
fun totalCount () = revfold (fn (l,c) => revfold op + l c) (getCounts()) 0
*)

(* unused code
fun maxCount () = revfold (fn (l,m) => revfold Int.max l m) (getCounts()) 0
*)

(* unused code unless analyze is used
fun terms (p,tt) = if P.isZero p then tt else terms(P.rest p,P.leadMono p::tt)
fun tails ([],tt) = tt
  | tails (t as _::t',tt) = tails (t',t::tt)
*)

(* Unused code unless sort (analyze) is used
local
    val a = 16807.0  and  m = 2147483647.0
in
    val seed = ref 1.0
    fun random n = let val t = a*(!seed)
                   in seed := t - m * real(floor(t/m));
                      floor(real n * !seed/m)
                   end
end
*)

(* Unused code unless analyze is used
fun sort [] = []
  | sort a = 
let
    val a = arrayoflist a
    val b = tabulate(length1 a,fn i => i)
    val sub = sub1 and update = update1
    infix sub
    fun swap (i,j) = let val ai = a sub i in update(a,i,a sub j); update(a,j,ai) end
    (* sort all k, 0<=i<=k<j<=length a *)
    fun s (i,j,acc) = if i=j then acc else let
        val pivot = a sub (b sub (i+random(j-i)))
        fun partition (dup,lo,k,hi) = if k=hi then (dup,lo,hi) else
            (case M.compare (pivot, a sub (b sub k)) of
                 Util.Less => (swap (lo,k); partition (dup,lo+1,k+1,hi))
               | Util.Equal => partition (dup+1,lo,k+1,hi)
               | Util.Greater => (swap (k,hi-1); partition (dup,lo,k,hi-1)))
        val (dup,lo,hi) = partition (0,i,i,j)
        in s(i,lo,(dup,pivot)::s(hi,j,acc)) end
    in s(0,length1 a,[]) end
*)

(* Unused code unless analyze is used
fun sum f l = revfold op + (map f l) 0
*)

(* Unused code included in the benchmark
fun analyze gb = let
      val aa = revfold terms gb []
      val bb = map M.explode aa
      val aa = sort aa
      fun len m = length (M.explode m)
      fun prt (s:string) (i:int) = (print s; print(Int.toString i); print "\n"; i)
      val m=  sum #1 aa
      val u=  length aa
      val cm =sum (fn (d,l) => d*len l) aa
      val cu =sum (len o #2) aa
      val for=length(sort(map M.implode (revfold tails bb [])))
      val bak=length(sort(map (M.implode o rev) (revfold tails (map rev bb) [])))
    in
     {m=prt "m  = " m, u=prt "u  = " u, cm =prt "cm = " cm, cu =prt "cu = " cu, for=prt "for= " for, bak=prt "bak= " bak}
    end
*)

fun gb fs = 
  let
    val g = G.grobner fs handle (Util.Illegal s) => (print s; raise Div)
    val fs = grab g
    fun info f = app print
      [M.display(P.leadMono f), 
       " + ", Int.toString(P.numTerms f - 1), " terms\n"]
  in app info fs end


fun report (e as Tabulate) = (print "exn: Tabulate\n"; raise e)
  | report (e as ArrayofList) = (print "exn: ArrayofList\n"; raise e)
  | report (e as (Util.NotImplemented s)) = 
  (print ("exn: NotImplemented " ^ s ^ "\n"); raise e)
  | report (e as (Util.Impossible s)) = 
  (print ("exn: Impossible " ^ s ^ "\n"); raise e)
  | report (e as (Util.Illegal s)) = 
  (print ("exn: Illegal " ^ s ^ "\n"); raise e)
  | report (e as (M.DoesntDivide)) = (print ("exn: DoesntDivide\n"); raise e)
  | report (e as (MI.Found)) = (print ("exn: Found\n"); raise e)


(* rather long running test case  *)
(* val fs = map G.parsePoly 
 *   ["-El-Dh-Cd+Bo+xn+tm","-Fo+Ep-Ek-Dg-Cc+Ao+wn+sm","-Fn-Ej+Dp-Df-Cb+zo+vn+rm",
 *    "-Fm-Ei-De+Cp-Ca+yo+un+qm","Fl-Bp+Bk-Al-zh-yd+xj+ti","El-Bo-zg-yc+wj+si",
 *    "Dl-Bn-Aj+zk-zf-yb+vj+ri","Cl-Bm-Ai-ze+yk-ya+uj+qi",
 *    "Fh+Bg-xp+xf-wl-vh-ud+te","Eh+Ag-xo-wk+wf-vg-uc+se","Dh+zg-xn-wj-ub+re",
 *    "Ch+yg-xm-wi-ve+uf-ua+qe","Fd+Bc+xb-tp+ta-sl-rh-qd",
 *    "Ed+Ac+wb-to-sk+sa-rg-qc","Dd+zc+vb-tn-sj-rf+ra-qb","Cd+yc+ub-tm-si-re"]
 *)

(* rather long running test case *)
(* val u7 = map G.parsePoly
 *   ["abcdefg-h7","a+b+c+d+e+f+g","ab+bc+cd+de+ef+fg+ga",
 *    "abc+bcd+cde+def+efg+fga+gab","abcd+bcde+cdef+defg+efga+fgab+gabc",
 *    "abcde+bcdef+cdefg+defga+efgab+fgabc+gabcd",
 *    "abcdef+bcdefg+cdefga+defgab+efgabc+fgabcd+gabcde"]
 *)


(* val u5 = map G.parsePoly ["abcde-f5","a+b+c+d+e","ab+bc+cd+de+ea",
 *                        "abc+bcd+cde+dea+eab","abcd+bcde+cdea+deab+eabc"]
 * 
 * val u4 = map G.parsePoly ["abcd-e4","a+b+c+d","ab+bc+cd+da","abc+bcd+cda+dab"]
 * 
 *)

(* fun runit () = 
 *   let
 *     val _ = (print "Enter fs, u7, u6, u5, or u4: "; 
 *           TextIO.flushOut TextIO.stdOut)
 *     val s = TextIO.inputN(TextIO.stdIn,2)
 *     val data = 
 *       if (s = "fs") then fs else if (s = "u7") then u7 
 *       else if (s = "u6") then u6 else if (s = "u5") then u5 
 *       else if (s = "u4") then u4 else 
 *      (print "no such data\n"; raise (Util.Impossible "no such data"))
 *   in
 *     gb data handle e => report e
 *   end
 *)

structure Main =
   struct
      fun doit n =
         let
            val u6 =
               map G.parsePoly
               ["abcdef-g6","a+b+c+d+e+f","ab+bc+cd+de+ef+fa",
                "abc+bcd+cde+def+efa+fab",
                "abcd+bcde+cdef+defa+efab+fabc",
                "abcde+bcdef+cdefa+defab+efabc+fabcd"]
            fun loop n =
               if n = 0
                  then ()
               else (gb u6; loop (n - 1))
         in
            loop n
         end
   end
