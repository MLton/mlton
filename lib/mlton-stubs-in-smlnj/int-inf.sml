(* int-inf.sml
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories. See COPYRIGHT file for details.
 *
 * This package is derived from Andrzej Filinski's bignum package.  It is versy
 * close to the definition of the optional IntInf structure in the SML'97 basis.
 * 
 * It is implemented almost totally on the abstraction presented by
 * the BigNat structure. The only concrete type information it assumes 
 * is that BigNat.bignat = 'a list and that BigNat.zero = [].
 * Some trivial additional efficiency could be obtained by assuming that
 * type bignat is really int list, and that if (v : bignat) = [d], then
 * bignat d = [d].
 *
 * At some point, this should be reimplemented to make use of Word32, or
 * have compiler/runtime support.
 *
 * Also, for booting, this module could be broken into one that has
 * all the types and arithmetic functions, but doesn't use NumScan,
 * constructing values from strings using bignum arithmetic. Various
 * integer and word scanning, such as NumScan, could then be constructed 
 * from IntInf. Finally, a user-level IntInf could be built by 
 * importing the basic IntInf, but replacing the scanning functions
 * by more efficient ones based on the functions in NumScan.
 *
 *)

structure IntInf' :> INT_INF =
  struct
     open Pervasive
     
  (* It is not clear what advantage there is to having NumFormat as
   * a submodule.
   *)

    structure NumScan : sig

        val skipWS : (char, 'a) StringCvt.reader -> 'a -> 'a

        val scanInt : StringCvt.radix
	      ->  (char, 'a) StringCvt.reader
	        -> 'a -> (int * 'a) option
	    (** should be to int32 **)

      end = struct

        structure W = Word32
        structure I = Int31
    
        val op <  = W.<
        val op >= = W.>=
        val op +  = W.+
        val op -  = W.-
        val op *  = W.*
    
        val largestWordDiv10 : Word32.word = 0w429496729(* 2^32-1 divided by 10 *)
        val largestWordMod10 : Word32.word = 0w5	(* remainder *)
        val largestNegInt : Word32.word = 0w1073741824	(* absolute value of ~2^30 *)
        val largestPosInt : Word32.word = 0w1073741823	(* 2^30-1 *)
    
        type 'a chr_strm = {getc : (char, 'a) StringCvt.reader}
    
      (* A table for mapping digits to values.  Whitespace characters map to
       * 128, and the characters 0-9,A-Z,a-z map to their 
       * base-36 value.  All other characters map to 255.
       *)
        local
          val cvtTable = "\
    	    \\255\255\255\255\255\255\255\255\255\128\128\255\255\255\255\255\
    	    \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    	    \\128\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    	    \\000\001\002\003\004\005\006\007\008\009\255\255\255\255\255\255\
    	    \\255\010\011\012\013\014\015\016\017\018\019\020\021\022\023\024\
    	    \\025\026\027\028\029\030\031\032\033\034\035\255\255\255\255\255\
    	    \\255\010\011\012\013\014\015\016\017\018\019\020\021\022\023\024\
    	    \\025\026\027\028\029\030\031\032\033\034\035\255\255\255\255\255\
    	    \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    	    \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    	    \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    	    \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    	    \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    	    \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    	    \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    	    \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    	  \"
        val ord = Char.ord
        in
	fun code (c : char) = W.fromInt(ord(CharVector.sub(cvtTable, ord c)))
        val wsCode : Word32.word = 0w128
        end (* local *)
    
        fun skipWS (getc : (char, 'a) StringCvt.reader) cs = let
              fun skip cs = (case (getc cs)
		     of NONE => cs
		      | (SOME(c, cs')) => if (code c = wsCode) then skip cs' else cs
		    (* end case *))
              in
                skip cs
              end
    
      (* for power of 2 bases (2, 8 & 16), we can check for overflow by looking
       * at the hi (1, 3 or 4) bits.
       *)
        fun chkOverflow mask w =
    	  if (W.andb(mask, w) = 0w0) then () else raise Overflow
    
        fun scan getc cs = case getc (skipWS getc cs)
           of NONE => NONE
            | SOME(c,rest) => SOME(code c, rest)
         
        fun scanBin getc cs = (case (scan getc cs)
    	   of NONE => NONE
    	    | (SOME(next, rest)) => let
    		fun isDigit (d : Word32.word) = (d < 0w2)
    		val chkOverflow = chkOverflow 0wx80000000
    		fun cvt (w, rest) = (case (getc rest)
    		       of NONE => SOME (w, rest)
    			| SOME(c, rest') => let val d = code c
    			    in
    			      if (isDigit d)
    				then (
    				  chkOverflow w;
    				  cvt(W.+(W.<<(w, 0w1), d), rest'))
    				else SOME(w, rest)
    			    end
    		      (* end case *))
    		in
    		  if (isDigit next)
    		    then cvt(next, rest)
    		    else NONE
    		end
    	  (* end case *))
    
        fun scanOct getc cs = (case (scan getc cs)
    	   of NONE => NONE
    	    | (SOME(next, rest)) => let
    		fun isDigit (d : Word32.word) = (d < 0w8)
    		val chkOverflow = chkOverflow 0wxE0000000
    		fun cvt (w, rest) = (case (getc rest)
    		       of NONE => SOME(w, rest)
    			| SOME(c, rest') => let val d = code c
    			    in
    			      if (isDigit d)
    				then (
    				  chkOverflow w;
    				  cvt(W.+(W.<<(w, 0w3), d), rest'))
    				else SOME(w, rest)
    			    end
    		      (* end case *))
    		in
    		  if (isDigit next)
    		    then cvt(next, rest)
    		    else NONE
    		end
    	  (* end case *))
    
        fun scanDec getc cs = (case (scan getc cs)
    	   of NONE => NONE
    	    | (SOME(next, rest)) => let
    		fun isDigit (d : Word32.word) = (d < 0w10)
    		fun cvt (w, rest) = (case (getc rest)
    		       of NONE => SOME(w, rest)
    			| SOME(c, rest') => let val d = code c
    			    in
    			      if (isDigit d)
    				then (
    				  if ((w >= largestWordDiv10)
    				  andalso ((largestWordDiv10 < w)
    				    orelse (largestWordMod10 < d)))
    				    then raise Overflow
    				    else ();
    				  cvt (0w10*w+d, rest'))
    				else SOME(w, rest)
    			    end
    		      (* end case *))
    		in
    		  if (isDigit next)
    		    then cvt(next, rest)
    		    else NONE
    		end
    	  (* end case *))
    
        fun scanHex getc cs = (case (scan getc cs)
    	   of NONE => NONE
    	    | (SOME(next, rest)) => let
    		fun isDigit (d : Word32.word) = (d < 0w16)
    		val chkOverflow = chkOverflow 0wxF0000000
    		fun cvt (w, rest) = (case (getc rest)
    		       of NONE => SOME(w, rest)
    			| SOME(c, rest') => let val d = code c
    			    in
    			      if (isDigit d)
    				then (
    				  chkOverflow w;
    				  cvt(W.+(W.<<(w, 0w4), d), rest'))
    				else SOME(w, rest)
    			    end
    		      (* end case *))
    		in
    		  if (isDigit next)
    		    then cvt(next, rest)
    		    else NONE
    		end
    	  (* end case *))
    
        fun finalInt scanFn getc cs = (case (scanFn getc cs)
    	   of NONE => NONE
    	    | (SOME(word, rest)) =>
    		if (largestPosInt < word)
    		  then raise Overflow
    		  else SOME(W.toInt word, rest)
    	  (* end case *))
    
        fun scanInt StringCvt.BIN = finalInt scanBin
          | scanInt StringCvt.OCT = finalInt scanOct
          | scanInt StringCvt.DEC = finalInt scanDec
          | scanInt StringCvt.HEX = finalInt scanHex
    
      end (* structure NumScan *)

    structure NumFormat : sig

        val fmtWord : StringCvt.radix -> Word32.word -> string
        val fmtInt : StringCvt.radix -> int -> string	(** should be int32 **)

      end = struct

        structure W = Word32
        structure I = Int
    
        val op < = W.<
        val op - = W.-
        val op * = W.*
        val op div = W.div
    
        fun mkDigit (w : Word32.word) =
    	  CharVector.sub("0123456789abcdef", W.toInt w)
    
        fun wordToBin w = let
    	  fun mkBit w = if (W.andb(w, 0w1) = 0w0) then #"0" else #"1"
    	  fun f (0w0, n, l) = (I.+(n, 1), #"0" :: l)
    	    | f (0w1, n, l) = (I.+(n, 1), #"1" :: l)
    	    | f (w, n, l) = f(W.>>(w, 0w1), I.+(n, 1), (mkBit w) :: l)
    	  in
    	    f (w, 0, [])
    	  end
        fun wordToOct w = let
    	  fun f (w, n, l) = if (w < 0w8)
    		then (I.+(n, 1), (mkDigit w) :: l)
    		else f(W.>>(w, 0w3), I.+(n, 1), mkDigit(W.andb(w, 0w7)) :: l)
    	  in
    	    f (w, 0, [])
    	  end
        fun wordToDec w = let
    	  fun f (w, n, l) = if (w < 0w10)
    		then (I.+(n, 1), (mkDigit w) :: l)
    		else let val j = w div 0w10
    		  in
    		    f (j,  I.+(n, 1), mkDigit(w - 0w10*j) :: l)
    		  end
    	  in
    	    f (w, 0, [])
    	  end
        fun wordToHex w = let
    	  fun f (w, n, l) = if (w < 0w16)
    		then (I.+(n, 1), (mkDigit w) :: l)
    		else f(W.>>(w, 0w4), I.+(n, 1), mkDigit(W.andb(w, 0w15)) :: l)
    	  in
    	    f (w, 0, [])
    	  end
    
        fun fmtW StringCvt.BIN = #2 o wordToBin
          | fmtW StringCvt.OCT = #2 o wordToOct
          | fmtW StringCvt.DEC = #2 o wordToDec
          | fmtW StringCvt.HEX = #2 o wordToHex
    
        fun fmtWord radix = String.implode o (fmtW radix)
    
    (** NOTE: this currently uses 31-bit integers, but really should use 32-bit
     ** ints (once they are supported).
     **)
        fun fmtInt radix = let
    	  val fmtW = fmtW radix
    	  val itow = W.fromInt
    	  fun fmt i = if I.<(i, 0)
    		then let
    		  val (digits) = fmtW(itow(I.~ i))
    		  in
    		    String.implode(#"~"::digits)
    		  end
    		    handle _ => (case radix
    		       of StringCvt.BIN => "~1111111111111111111111111111111"
    			| StringCvt.OCT => "~7777777777"
    			| StringCvt.DEC => "~1073741824"
    			| StringCvt.HEX => "~3fffffff"
    		      (* end case *))
    		else String.implode(fmtW(itow i))
    	  in
    	    fmt
    	  end
    
      end (* structure NumFormat *)

    structure BigNat =
      struct

	exception Negative

        val itow = Word.fromInt
	val wtoi = Word.toIntX

	val lgBase = 30             (* No. of bits per digit; must be even *)
	val nbase = ~0x40000000     (* = ~2^lgBase *)

	val maxDigit = ~(nbase + 1)
	val realBase = (real maxDigit) + 1.0

	val lgHBase = Int.quot (lgBase, 2)    (* half digits *)
	val hbase = Word.<<(0w1, itow lgHBase)
	val hmask = hbase-0w1

	fun quotrem (i, j) = (Int.quot (i, j), Int.rem (i, j))
	fun scale i = if i = maxDigit then 1 else nbase div (~(i+1))

	type bignat = int list (* least significant digit first *)

	val zero = []
	val one = [1]

	fun bignat 0 = zero
	  | bignat i = let
	      val notNbase = Word.notb(itow nbase)
              fun bn 0w0 = []
        	| bn i = let
		    fun dmbase n = 
		      (Word.>> (n, itow lgBase), Word.andb (n, notNbase))
		    val (q,r) = dmbase i
		  in
		    (wtoi r)::(bn q)
		  end
              in
        	if i > 0 
        	  then if i <= maxDigit then [i] else bn (itow i)
        	  else raise Negative
              end

	fun int [] = 0
	  | int [d] = d
	  | int [d,e] = ~(nbase*e) + d
	  | int (d::r) = ~(nbase*int r) + d

	fun consd (0, []) = []
	  | consd (d, r) = d::r

	fun hl i = let
	  val w = itow i
        in
	  (wtoi(Word.~>> (w, itow lgHBase)),  (* MUST sign-extend *)
	   wtoi(Word.andb(w, hmask)))
        end

	fun sh i = wtoi(Word.<< (itow i, itow lgHBase))

	fun addOne [] = [1]
	  | addOne (m::rm) = let
              val c = nbase+m+1
              in
        	if c < 0 then (c-nbase)::rm else c::(addOne rm)
              end

	fun add ([], digits) = digits
	  | add (digits, []) = digits
	  | add (dm::rm, dn::rn) = addd (nbase+dm+dn, rm, rn)
	and addd (s, m, n) = 
              if s < 0 then (s-nbase) :: add (m, n) else (s :: addc (m, n))
	and addc (m, []) = addOne m
	  | addc ([], n) = addOne n
	  | addc (dm::rm, dn::rn) = addd (nbase+dm+dn+1, rm, rn)

	fun subtOne (0::mr) = maxDigit::(subtOne mr)
	  | subtOne [1] = []
	  | subtOne (n::mr) = (n-1)::mr
	  | subtOne [] = raise Fail ""

	fun subt (m, []) = m
	  | subt ([], n) = raise Negative
	  | subt (dm::rm, dn::rn) = subd(dm-dn,rm,rn)
	and subb ([], n) = raise Negative
	  | subb (dm::rm, []) = subd (dm-1, rm, [])
	  | subb (dm::rm, dn::rn) = subd (dm-dn-1, rm, rn)
	and subd (d, m, n) = 
              if d >= 0 then consd(d, subt (m, n)) else consd(d-nbase, subb (m, n))

               (* multiply 2 digits *)
	fun mul2 (m, n) = let 
              val (mh, ml) = hl m
              val (nh, nl) = hl n
              val x = mh*nh
              val y = (mh-ml)*(nh-nl) (* x-y+z = mh*nl + ml*nh *)
              val z = ml*nl
              val (zh, zl) = hl z
              val (uh,ul) = hl (nbase+x+z-y+zh) (* can't overflow *)
              in (x+uh+wtoi hbase, sh ul+zl) end

            (* multiply bigint by digit *)
	fun muld (m, 0) = []
	  | muld (m, 1) = m (* speedup *)
	  | muld (m, i) = let
              fun muldc ([], 0) = []
        	| muldc ([], c) = [c]
        	| muldc (d::r, c) = let
                    val (h, l) = mul2 (d, i)
                    val l1 = l+nbase+c
                    in 
                      if l1 >= 0 
                	then l1::muldc (r, h+1)
                	else (l1-nbase)::muldc (r, h) 
                    end
              in muldc (m, 0) end

	fun mult (m, []) = []
	  | mult (m, [d]) = muld (m, d) (* speedup *)
	  | mult (m, 0::r) = consd (0, mult (m, r)) (* speedup *)
	  | mult (m, n) = let 
              fun muln [] = []
        	| muln (d::r) = add (muld (n, d), consd (0, muln r))
              in muln m end

            (* divide DP number by digit; assumes u < i , i >= base/2 *)
	fun divmod2 ((u,v), i) = let
              val (vh,vl) = hl v
              val (ih,il) = hl i
              fun adj (q,r) = if r<0 then adj (q-1, r+i) else (q, r)
              val (q1,r1) = quotrem (u, ih)
              val (q1,r1) = adj (q1, sh r1+vh-q1*il)
              val (q0,r0) = quotrem (r1, ih)
              val (q0,r0) = adj (q0, sh r0+vl-q0*il)
              in (sh q1+q0, r0) end

            (* divide bignat by digit>0 *)
	fun divmodd (m, 1) = (m, 0) (* speedup *)
	  | divmodd (m, i) = let
              val scale = scale i
              val i' = i * scale
              val m' = muld (m, scale)
              fun dmi [] = ([], 0)
        	| dmi (d::r) = let 
                    val (qt,rm) = dmi r
                    val (q1,r1) = divmod2 ((rm,d), i')
                    in (consd (q1,qt), r1) end
              val (q,r) = dmi m'
              in (q, r div scale) end

            (* From Knuth Vol II, 4.3.1, but without opt. in step D3 *)
	fun divmod (m, []) = raise Div
	  | divmod ([], n) = ([], []) (* speedup *)
	  | divmod (d::r, 0::s) = let 
              val (qt,rm) = divmod (r,s)
              in (qt, consd (d, rm)) end (* speedup *)
	  | divmod (m, [d]) = let 
              val (qt, rm) = divmodd (m, d)
              in (qt, if rm=0 then [] else [rm]) end
	  | divmod (m, n) = let
              val ln = length n (* >= 2 *)
              val scale = scale(List.nth (n,ln-1))
              val m' = muld (m, scale)
              val n' = muld (n, scale)
              val n1 = List.nth (n', ln-1) (* >= base/2 *)
              fun divl [] = ([], [])
        	| divl (d::r) = let
                    val (qt,rm) = divl r
                    val m = consd (d, rm)
                    fun msds ([],_) = (0,0)
                      | msds ([d],1) = (0,d)
                      | msds ([d2,d1],1) = (d1,d2)
                      | msds (d::r,i) = msds (r,i-1)
                    val (m1,m2) = msds (m, ln)
                    val tq = if m1 = n1 then maxDigit
                             else #1 (divmod2 ((m1,m2), n1))
                    fun try (q,qn') = (q, subt (m,qn'))
                	  handle Negative => try (q-1, subt (qn', n'))
                    val (q,rr) = try (tq, muld (n',tq))
                    in (consd (q,qt), rr) end
              val (qt,rm') = divl m'
              val (rm,_(*0*)) = divmodd (rm',scale)
              in (qt,rm) end

	fun cmp ([],[]) = EQUAL
	  | cmp (_,[]) = GREATER
	  | cmp ([],_) = LESS
	  | cmp ((i : int)::ri,j::rj) =
              case cmp (ri,rj) of
        	EQUAL => if i = j then EQUAL 
                         else if i < j then LESS 
                         else GREATER
              | c => c

	fun exp (_, 0) = one
	  | exp ([], n) = if n > 0 then zero else raise Div
	  | exp (m, n) = 
              if n < 0 then zero
              else let
        	fun expm 0 = [1]
        	  | expm 1 = m
        	  | expm i = let
                      val r = expm (i div 2)
                      val r2 = mult (r,r)
                      in
                	if i mod 2 = 0 then r2 else mult (r2, m)
                      end
        	in expm n end

        local 
          fun try n = if n >= lgHBase then n else try (2*n)
          val pow2lgHBase = try 1
        in
        fun log2 [] = raise Domain
          | log2 (h::t) = let
              fun qlog (x,0) = 0
                | qlog (x,b) = 
		  if x >= wtoi(Word.<< (0w1, itow b)) then
		    b+qlog (wtoi(Word.>> (itow x, itow b)), b div 2)
                                 else qlog (x, b div 2)
              fun loop (d,[],lg) = lg + qlog (d,pow2lgHBase)
                | loop (_,h::t,lg) = loop (h,t,lg + lgBase)
            in
              loop (h,t,0)
            end
        end (* local *)

            (* find maximal maxpow s.t. radix^maxpow < base 
             * basepow = radix^maxpow
             *)
        fun mkPowers radix = let
	      val powers = let
                    val bnd = Int.quot (nbase, (~radix))
                    fun try (tp,l) =
                          (if tp <= bnd then try (radix*tp,tp::l)
                          else (tp::l))
                            handle _ => tp::l
                    in Vector.fromList(rev(try (radix,[1]))) end
	      val maxpow = Vector.length powers - 1
              in
                (maxpow, Vector.sub(powers,maxpow), powers)
              end
        val powers2 = mkPowers 2
        val powers8 = mkPowers 8
        val powers10 = mkPowers 10
        val powers16 = mkPowers 16

	fun fmt (pow, radpow, puti) n = let 
              val pad = StringCvt.padLeft #"0" pow
              fun ms0 (0,a) = (pad "")::a
        	| ms0 (i,a) = (pad (puti i))::a
              fun ml (n,a) =
                    case divmodd (n, radpow) of
                      ([],d) => (puti d)::a
                    | (q,d) => ml (q, ms0 (d, a)) 
              in 
                concat (ml (n,[])) 
              end

        val fmt2 = fmt (#1 powers2, #2 powers2, NumFormat.fmtInt StringCvt.BIN)
        val fmt8 = fmt (#1 powers8, #2 powers8, NumFormat.fmtInt StringCvt.OCT)
        val fmt10 = fmt (#1 powers10, #2 powers10, NumFormat.fmtInt StringCvt.DEC)
        val fmt16 = fmt (#1 powers16, #2 powers16, NumFormat.fmtInt StringCvt.HEX)

        fun scan (bound,powers,geti) getc cs = let
              fun get (l,cs) = if l = bound then NONE
                               else case getc cs of
                                 NONE => NONE
                               | SOME(c,cs') => SOME(c, (l+1,cs'))
              fun loop (acc,cs) =
                    case geti get (0,cs) of
                      NONE => (acc,cs)
                    | SOME(0,(sh,cs')) => 
                        loop(add(muld(acc,Vector.sub(powers,sh)),[]),cs')
                    | SOME(i,(sh,cs')) => 
                        loop(add(muld(acc,Vector.sub(powers,sh)),[i]),cs')
              in
                case geti get (0,cs) of
                  NONE => NONE
                | SOME(0,(_,cs')) => SOME (loop([],cs'))
                | SOME(i,(_,cs')) => SOME (loop([i],cs'))
              end

        fun scan2 getc = scan(#1 powers2, #3 powers2, NumScan.scanInt StringCvt.BIN) getc
        fun scan8 getc = scan(#1 powers8, #3 powers8, NumScan.scanInt StringCvt.OCT) getc
        fun scan10 getc = scan(#1 powers10, #3 powers10, NumScan.scanInt StringCvt.DEC) getc
        fun scan16 getc = scan(#1 powers16, #3 powers16, NumScan.scanInt StringCvt.HEX) getc

      end (* structure BigNat *)

    structure BN = BigNat

    datatype sign = POS | NEG
    datatype int = BI of {
        sign : sign,
        digits : BN.bignat
      }

    val zero = BI{sign=POS, digits=BN.zero}
    val one = BI{sign=POS, digits=BN.one}
    val minus_one = BI{sign=NEG, digits=BN.one}
    fun posi digits = BI{sign=POS, digits=digits}
    fun negi digits = BI{sign=NEG, digits=digits}
    fun zneg [] = zero
      | zneg digits = BI{sign=NEG, digits=digits}

    local
    val minNeg = valOf Int.minInt
    val bigNatMinNeg = BN.addOne (BN.bignat (~(minNeg+1)))
    val bigIntMinNeg = negi bigNatMinNeg
    in

    fun toInt (BI{digits=[], ...}) = 0
      | toInt (BI{sign=POS, digits}) = BN.int digits
      | toInt (BI{sign=NEG, digits}) =
              (~(BN.int digits)) handle _ =>
                if digits = bigNatMinNeg then minNeg else raise Overflow

    fun fromInt 0 = zero
      | fromInt i =
          if i < 0
            then if (i = minNeg)
              then bigIntMinNeg
              else BI{sign=NEG, digits= BN.bignat (~i)}
            else BI{sign=POS, digits= BN.bignat i}
    end (* local *)

      (* The following assumes LargeInt = Int32.
       * If IntInf is provided, it will be LargeInt and toLarge and fromLarge
       * will be the identity function.
       *)
    local
    val minNeg = valOf LargeInt.minInt
    val maxDigit = LargeInt.fromInt BN.maxDigit
    val nbase = LargeInt.fromInt BN.nbase
    val lgBase = Word.fromInt BN.lgBase
    val notNbase = Word32.notb(Word32.fromInt BN.nbase)
    fun largeNat (0 : LargeInt.int) = []
      | largeNat i = let
          fun bn (0w0 : Word32.word) = []
       	    | bn i = let
	        fun dmbase n = (Word32.>> (n, lgBase), Word32.andb (n, notNbase))
	        val (q,r) = dmbase i
	      in
	        (Word32.toInt r)::(bn q)
	      end
          in
       	    if i <= maxDigit then [LargeInt.toInt i] else bn (Word32.fromLargeInt i)
          end

    fun large [] = 0
      | large [d] = LargeInt.fromInt d
      | large [d,e] = ~(nbase*(LargeInt.fromInt e)) + (LargeInt.fromInt d)
      | large (d::r) = ~(nbase*large r) + (LargeInt.fromInt d)

    val bigNatMinNeg = BN.addOne (largeNat (~(minNeg+1)))
    val bigIntMinNeg = negi bigNatMinNeg
    in

    fun toLarge (BI{digits=[], ...}) = 0
      | toLarge (BI{sign=POS, digits}) = large digits
      | toLarge (BI{sign=NEG, digits}) =
              (~(large digits)) handle _ =>
                if digits = bigNatMinNeg then minNeg else raise Overflow

    fun fromLarge 0 = zero
      | fromLarge i =
          if i < 0
            then if (i = minNeg)
              then bigIntMinNeg
              else BI{sign=NEG, digits= largeNat (~i)}
            else BI{sign=POS, digits= largeNat i}
    end (* local *)

    fun negSign POS = NEG
      | negSign NEG = POS

    fun subtNat (m, []) = {sign=POS, digits=m}
      | subtNat ([], n) = {sign=NEG, digits=n}
      | subtNat (m,n) =
            ({sign=POS,digits = BN.subt(m,n)})
              handle BN.Negative => ({sign=NEG,digits = BN.subt(n,m)})

    val precision = NONE
    val minInt = NONE
    val maxInt = NONE

    fun ~ (i as BI{digits=[], ...}) = i
      | ~ (BI{sign=POS, digits}) = BI{sign=NEG, digits=digits}
      | ~ (BI{sign=NEG, digits}) = BI{sign=POS, digits=digits}

    fun op * (_,BI{digits=[], ...}) = zero
      | op * (BI{digits=[], ...},_) = zero
      | op * (BI{sign=POS, digits=d1}, BI{sign=NEG, digits=d2}) =
          BI{sign=NEG,digits=BN.mult(d1,d2)}
      | op * (BI{sign=NEG, digits=d1}, BI{sign=POS, digits=d2}) =
          BI{sign=NEG,digits=BN.mult(d1,d2)}
      | op * (BI{digits=d1,...}, BI{digits=d2,...}) =
          BI{sign=POS,digits=BN.mult(d1,d2)}

    fun op + (BI{digits=[], ...}, i2) = i2
      | op + (i1, BI{digits=[], ...}) = i1
      | op + (BI{sign=POS, digits=d1}, BI{sign=NEG, digits=d2}) =
          BI(subtNat(d1, d2))
      | op + (BI{sign=NEG, digits=d1}, BI{sign=POS, digits=d2}) =
          BI(subtNat(d2, d1))
      | op + (BI{sign, digits=d1}, BI{digits=d2, ...}) =
          BI{sign=sign, digits=BN.add(d1, d2)}

    fun op - (i1, BI{digits=[], ...}) = i1
      | op - (BI{digits=[], ...}, BI{sign, digits}) =
          BI{sign=negSign sign, digits=digits}
      | op - (BI{sign=POS, digits=d1}, BI{sign=POS, digits=d2}) =
            BI(subtNat(d1, d2))
      | op - (BI{sign=NEG, digits=d1}, BI{sign=NEG, digits=d2}) =
            BI(subtNat(d2, d1))
      | op - (BI{sign, digits=d1}, BI{digits=d2, ...}) =
          BI{sign=sign, digits=BN.add(d1, d2)}

    fun quotrem (BI{sign=POS,digits=m},BI{sign=POS,digits=n}) =
          (case BN.divmod (m,n) of (q,r) => (posi q, posi r))
      | quotrem (BI{sign=POS,digits=m},BI{sign=NEG,digits=n}) =
          (case BN.divmod (m,n) of (q,r) => (zneg q, posi r))
      | quotrem (BI{sign=NEG,digits=m},BI{sign=POS,digits=n}) =
          (case BN.divmod (m,n) of (q,r) => (zneg q, zneg r))
      | quotrem (BI{sign=NEG,digits=m},BI{sign=NEG,digits=n}) =
          (case BN.divmod (m,n) of (q,r) => (posi q, zneg r))

    fun divmod (BI{sign=POS,digits=m},BI{sign=POS,digits=n}) =
          (case BN.divmod (m,n) of (q,r) => (posi q, posi r))
      | divmod (BI{sign=POS,digits=[]},BI{sign=NEG,digits=n}) = (zero,zero)
      | divmod (BI{sign=POS,digits=m},BI{sign=NEG,digits=n}) = let
          val (q,r) = BN.divmod (BN.subtOne m, n)
          in (negi(BN.addOne q), zneg(BN.subtOne(BN.subt(n,r)))) end
      | divmod (BI{sign=NEG,digits=m},BI{sign=POS,digits=n}) = let
          val (q,r) = BN.divmod (BN.subtOne m, n)
          in (negi(BN.addOne q), posi(BN.subtOne(BN.subt(n,r)))) end
      | divmod (BI{sign=NEG,digits=m},BI{sign=NEG,digits=n}) =
          (case BN.divmod (m,n) of (q,r) => (posi q, zneg r))

    fun op div arg = #1(divmod arg)
    fun op mod arg = #2(divmod arg)
    fun op quot arg = #1(quotrem arg)
    fun op rem arg = #2(quotrem arg)

    fun compare (BI{sign=NEG,...},BI{sign=POS,...}) = LESS
      | compare (BI{sign=POS,...},BI{sign=NEG,...}) = GREATER
      | compare (BI{sign=POS,digits=d},BI{sign=POS,digits=d'}) = BN.cmp (d,d')
      | compare (BI{sign=NEG,digits=d},BI{sign=NEG,digits=d'}) = BN.cmp (d',d)

    fun op < arg = case compare arg of LESS => true | _ => false
    fun op > arg = case compare arg of GREATER => true | _ => false
    fun op <= arg = case compare arg of GREATER => false | _ => true
    fun op >= arg = case compare arg of LESS => false | _ => true

    fun abs (BI{sign=NEG, digits}) = BI{sign=POS, digits=digits}
      | abs i = i

    fun max arg = case compare arg of GREATER => #1 arg | _ => #2 arg
    fun min arg = case compare arg of LESS => #1 arg | _ => #2 arg

    fun sign (BI{sign=NEG,...}) = ~1
      | sign (BI{digits=[],...}) = 0
      | sign _ = 1

    fun sameSign (i,j) = sign i = sign j

    local
      fun fmt' fmtFn i =
            case i of 
              (BI{digits=[],...}) => "0"
            | (BI{sign=NEG,digits}) => "~"^(fmtFn digits)
            | (BI{sign=POS,digits}) => fmtFn digits
    in
    fun fmt StringCvt.BIN = fmt' (BN.fmt2)
      | fmt StringCvt.OCT = fmt' (BN.fmt8)
      | fmt StringCvt.DEC = fmt' (BN.fmt10)
      | fmt StringCvt.HEX = fmt' (BN.fmt16)
    end

    val toString = fmt StringCvt.DEC

    local
      fun scan' scanFn getc cs = let
            val cs' = NumScan.skipWS getc cs
            fun cvt (NONE,_) = NONE
              | cvt (SOME(i,cs),wr) = SOME(wr i, cs)
            in
              case (getc cs')
               of (SOME((#"~" | #"-"), cs'')) => cvt(scanFn getc cs'',zneg)
                | (SOME(#"+", cs'')) => cvt(scanFn getc cs'',posi)
                | (SOME _) => cvt(scanFn getc cs',posi)
                | NONE => NONE
              (* end case *)
            end
    in
    fun scan StringCvt.BIN = scan' (BN.scan2)
      | scan StringCvt.OCT = scan' (BN.scan8)
      | scan StringCvt.DEC = scan' (BN.scan10)
      | scan StringCvt.HEX = scan' (BN.scan16)
    end

    fun fromString s = StringCvt.scanString (scan StringCvt.DEC) s

    fun pow (_, 0) = one
      | pow (BI{sign=POS,digits}, n) = posi(BN.exp(digits,n))
      | pow (BI{sign=NEG,digits}, n) = 
          if Int.mod (n, 2) = 0
            then posi(BN.exp(digits,n))
            else zneg(BN.exp(digits,n))

    fun log2 (BI{sign=POS,digits}) = BN.log2 digits
      | log2 _ = raise Domain

  end (* structure IntInf *)

structure IntInf =
   struct
      open IntInf'

      val toInt = toLarge
      val sign = Pervasive.Int32.fromInt o sign
      val fromInt = fromLarge
      val divMod = divmod
      val quotRem = quotrem
      val precision: Pervasive.Int32.int option = NONE
      fun toLarge x = x
      fun fromLarge x = x
      val log2 = Pervasive.Int32.fromInt o log2
      fun pow (a, b) = IntInf'.pow (a, Pervasive.Int32.toInt b)
   end

structure LargeInt = IntInf
