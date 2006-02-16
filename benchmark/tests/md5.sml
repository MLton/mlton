(* Copyright (C) 2001 Daniel Wang. All rights reserved.
 Derived from the RSA Data Security, Inc. MD5 Message-Digest Algorithm.
 *)
signature MD5 =
  sig
    type md5state
(*    type slice = (Word8Vector.vector * int * int option) *)
    val init : md5state
    (* val updateSlice : (md5state * slice) -> md5state
    *)
    val update : (md5state * Word8Vector.vector) -> md5state
    val final  : md5state -> Word8Vector.vector
    val toHexString :  Word8Vector.vector -> string
  end

(* Quick and dirty transliteration of C code *)
structure MD5 :> MD5 =
  struct
    structure W32 = Word32
    structure W8V = 
      struct
        open Word8Vector
        fun extract (vec, s, l) =
           let
              val n =
                 case l of
                    NONE => length vec - s
                  | SOME i => i
           in
              tabulate (n, fn i => sub (vec, s + i))
           end
      end
    type word64  = {hi:W32.word,lo:W32.word}
    type word128 = {A:W32.word, B:W32.word, C:W32.word,  D:W32.word}
    type md5state = {digest:word128,
                       mlen:word64, 
                        buf:Word8Vector.vector}



    val w64_zero = ({hi=0w0,lo=0w0}:word64)
    fun mul8add ({hi,lo},n) = let
      val mul8lo = W32.<< (W32.fromInt (n),0w3)
      val mul8hi = W32.>> (W32.fromInt (n),0w29)
      val lo = W32.+ (lo,mul8lo)
      val cout = if W32.< (lo,mul8lo) then 0w1 else 0w0
      val hi = W32.+ (mul8hi,W32.+ (hi,cout))
    in {hi=hi,lo=lo}
    end
  
    fun packLittle wrds = let
      fun loop [] = []
        | loop (w::ws) = let
            val b0 = Word8.fromLarge (W32.toLarge w)
            val b1 = Word8.fromLarge (W32.toLarge (W32.>> (w,0w8)))
            val b2 = Word8.fromLarge (W32.toLarge (W32.>> (w,0w16)))
            val b3 = Word8.fromLarge (W32.toLarge (W32.>> (w,0w24)))
          in b0::b1::b2::b3:: (loop ws)
          end
    in W8V.fromList (loop wrds)
    end
    
    val S11 = 0w7
    val S12 = 0w12
    val S13 = 0w17
    val S14 = 0w22
    val S21 = 0w5
    val S22 = 0w9
    val S23 = 0w14
    val S24 = 0w20
    val S31 = 0w4
    val S32 = 0w11
    val S33 = 0w16
    val S34 = 0w23
    val S41 = 0w6
    val S42 = 0w10
    val S43 = 0w15
    val S44 = 0w21
      
    fun PADDING i =  W8V.tabulate (i,(fn 0 => 0wx80 | _ => 0wx0))

    fun F (x,y,z) = W32.orb (W32.andb (x,y),
                           W32.andb (W32.notb x,z))
    fun G (x,y,z) = W32.orb (W32.andb (x,z),
                           W32.andb (y,W32.notb z))
    fun H (x,y,z) = W32.xorb (x,W32.xorb (y,z))
    fun I (x,y,z) = W32.xorb (y,W32.orb (x,W32.notb z))
    fun ROTATE_LEFT (x,n) =
      W32.orb (W32.<< (x,n), W32.>> (x,0w32 - n))

    fun XX f (a,b,c,d,x,s,ac) = let
      val a = W32.+ (a,W32.+ (W32.+ (f (b,c,d),x),ac))
      val a = ROTATE_LEFT (a,s)
    in W32.+ (a,b)
    end
                            
    val FF = XX F
    val GG = XX G
    val HH = XX H
    val II = XX I

    val empty_buf = W8V.tabulate (0,(fn x => raise (Fail "buf")))
    val init = {digest= {A=0wx67452301,
                        B=0wxefcdab89,
                        C=0wx98badcfe,
                        D=0wx10325476},
                mlen=w64_zero,
                buf=empty_buf} : md5state

    fun update ({buf,digest,mlen}:md5state,input) = let
      val inputLen = W8V.length input
      val needBytes = 64 - W8V.length buf
      fun loop (i,digest) =
        if i + 63 < inputLen then
          loop (i + 64,transform (digest,i,input))
        else (i,digest)
      val (buf,(i,digest)) =
        if inputLen >= needBytes then  let
          val buf = W8V.concat [buf,W8V.extract (input,0,SOME needBytes)]
          val digest = transform (digest,0,buf)
        in (empty_buf,loop (needBytes,digest))
        end
        else (buf,(0,digest))
      val buf = W8V.concat [buf, W8V.extract (input,i,SOME (inputLen-i))]
      val mlen = mul8add (mlen,inputLen)
    in {buf=buf,digest=digest,mlen=mlen}
    end
    and final (state:md5state) = let
      val {mlen= {lo,hi},buf,...} = state
      val bits = packLittle [lo,hi]
      val index = W8V.length buf
      val padLen = if index < 56 then 56 - index else 120 - index
      val state = update (state,PADDING padLen)
      val {digest= {A,B,C,D},...} = update (state,bits)
    in packLittle [A,B,C,D]
    end
    and transform ({A,B,C,D},i,buf) = let
      val off = i div PackWord32Little.bytesPerElem
      fun x (n)  = Word32.fromLarge (PackWord32Little.subVec (buf,n + off))
      val (a,b,c,d) = (A,B,C,D)
      (* fetch to avoid range checks *)
      val x_00 = x (0)  val x_01 = x (1)  val x_02 = x (2)  val x_03 = x (3)
      val x_04 = x (4)  val x_05 = x (5)  val x_06 = x (6)  val x_07 = x (7)
      val x_08 = x (8)  val x_09 = x (9)  val x_10 = x (10) val x_11 = x (11)
      val x_12 = x (12) val x_13 = x (13) val x_14 = x (14) val x_15 = x (15)

      val a = FF (a, b, c, d, x_00, S11, 0wxd76aa478) (* 1 *)
      val d = FF (d, a, b, c, x_01, S12, 0wxe8c7b756) (* 2 *)
      val c = FF (c, d, a, b, x_02, S13, 0wx242070db) (* 3 *)
      val b = FF (b, c, d, a, x_03, S14, 0wxc1bdceee) (* 4 *)
      val a = FF (a, b, c, d, x_04, S11, 0wxf57c0faf) (* 5 *)
      val d = FF (d, a, b, c, x_05, S12, 0wx4787c62a) (* 6 *)
      val c = FF (c, d, a, b, x_06, S13, 0wxa8304613) (* 7 *)
      val b = FF (b, c, d, a, x_07, S14, 0wxfd469501) (* 8 *)
      val a = FF (a, b, c, d, x_08, S11, 0wx698098d8) (* 9 *)
      val d = FF (d, a, b, c, x_09, S12, 0wx8b44f7af) (* 10 *)
      val c = FF (c, d, a, b, x_10, S13, 0wxffff5bb1) (* 11 *)
      val b = FF (b, c, d, a, x_11, S14, 0wx895cd7be) (* 12 *)
      val a = FF (a, b, c, d, x_12, S11, 0wx6b901122) (* 13 *)
      val d = FF (d, a, b, c, x_13, S12, 0wxfd987193) (* 14 *)
      val c = FF (c, d, a, b, x_14, S13, 0wxa679438e) (* 15 *)
      val b = FF (b, c, d, a, x_15, S14, 0wx49b40821) (* 16 *)
          
      (* Round 2 *)
      val a = GG (a, b, c, d, x_01, S21, 0wxf61e2562) (* 17 *)
      val d = GG (d, a, b, c, x_06, S22, 0wxc040b340) (* 18 *)
      val c = GG (c, d, a, b, x_11, S23, 0wx265e5a51) (* 19 *)
      val b = GG (b, c, d, a, x_00, S24, 0wxe9b6c7aa) (* 20 *)
      val a = GG (a, b, c, d, x_05, S21, 0wxd62f105d) (* 21 *)
      val d = GG (d, a, b, c, x_10, S22,  0wx2441453) (* 22 *)
      val c = GG (c, d, a, b, x_15, S23, 0wxd8a1e681) (* 23 *)
      val b = GG (b, c, d, a, x_04, S24, 0wxe7d3fbc8) (* 24 *)
      val a = GG (a, b, c, d, x_09, S21, 0wx21e1cde6) (* 25 *)
      val d = GG (d, a, b, c, x_14, S22, 0wxc33707d6) (* 26 *)
      val c = GG (c, d, a, b, x_03, S23, 0wxf4d50d87) (* 27 *)
      val b = GG (b, c, d, a, x_08, S24, 0wx455a14ed) (* 28 *)
      val a = GG (a, b, c, d, x_13, S21, 0wxa9e3e905) (* 29 *)
      val d = GG (d, a, b, c, x_02, S22, 0wxfcefa3f8) (* 30 *)
      val c = GG (c, d, a, b, x_07, S23, 0wx676f02d9) (* 31 *)
      val b = GG (b, c, d, a, x_12, S24, 0wx8d2a4c8a) (* 32 *)
          
      (* Round 3 *)
      val a = HH (a, b, c, d, x_05, S31, 0wxfffa3942) (* 33 *)
      val d = HH (d, a, b, c, x_08, S32, 0wx8771f681) (* 34 *)
      val c = HH (c, d, a, b, x_11, S33, 0wx6d9d6122) (* 35 *)
      val b = HH (b, c, d, a, x_14, S34, 0wxfde5380c) (* 36 *)
      val a = HH (a, b, c, d, x_01, S31, 0wxa4beea44) (* 37 *)
      val d = HH (d, a, b, c, x_04, S32, 0wx4bdecfa9) (* 38 *)
      val c = HH (c, d, a, b, x_07, S33, 0wxf6bb4b60) (* 39 *)
      val b = HH (b, c, d, a, x_10, S34, 0wxbebfbc70) (* 40 *)
      val a = HH (a, b, c, d, x_13, S31, 0wx289b7ec6) (* 41 *)
      val d = HH (d, a, b, c, x_00, S32, 0wxeaa127fa) (* 42 *)
      val c = HH (c, d, a, b, x_03, S33, 0wxd4ef3085) (* 43 *)
      val b = HH (b, c, d, a, x_06, S34,  0wx4881d05) (* 44 *)
      val a = HH (a, b, c, d, x_09, S31, 0wxd9d4d039) (* 45 *)
      val d = HH (d, a, b, c, x_12, S32, 0wxe6db99e5) (* 46 *)
      val c = HH (c, d, a, b, x_15, S33, 0wx1fa27cf8) (* 47 *)
      val b = HH (b, c, d, a, x_02, S34, 0wxc4ac5665) (* 48 *)
          
      (* Round 4 *)
      val a = II (a, b, c, d, x_00, S41, 0wxf4292244) (* 49 *)
      val d = II (d, a, b, c, x_07, S42, 0wx432aff97) (* 50 *)
      val c = II (c, d, a, b, x_14, S43, 0wxab9423a7) (* 51 *)
      val b = II (b, c, d, a, x_05, S44, 0wxfc93a039) (* 52 *)
      val a = II (a, b, c, d, x_12, S41, 0wx655b59c3) (* 53 *)
      val d = II (d, a, b, c, x_03, S42, 0wx8f0ccc92) (* 54 *)
      val c = II (c, d, a, b, x_10, S43, 0wxffeff47d) (* 55 *)
      val b = II (b, c, d, a, x_01, S44, 0wx85845dd1) (* 56 *)
      val a = II (a, b, c, d, x_08, S41, 0wx6fa87e4f) (* 57 *)
      val d = II (d, a, b, c, x_15, S42, 0wxfe2ce6e0) (* 58 *)
      val c = II (c, d, a, b, x_06, S43, 0wxa3014314) (* 59 *)
      val b = II (b, c, d, a, x_13, S44, 0wx4e0811a1) (* 60 *)
      val a = II (a, b, c, d, x_04, S41, 0wxf7537e82) (* 61 *)
      val d = II (d, a, b, c, x_11, S42, 0wxbd3af235) (* 62 *)
      val c = II (c, d, a, b, x_02, S43, 0wx2ad7d2bb) (* 63 *)
      val b = II (b, c, d, a, x_09, S44, 0wxeb86d391) (* 64 *)

      val A = Word32.+ (A,a)
      val B = Word32.+ (B,b)
      val C = Word32.+ (C,c)
      val D = Word32.+ (D,d)
    in {A=A,B=B,C=C,D=D}
    end

    val hxd = "0123456789abcdef"
    fun toHexString v = let
      fun byte2hex (b,acc) =
        (String.sub (hxd,(Word8.toInt b) div 16))::
        (String.sub (hxd,(Word8.toInt b) mod 16))::acc
      val digits = Word8Vector.foldr byte2hex [] v
    in String.implode (digits)
    end
  end

structure Test =
  struct
    val tests =
      [("", "d41d8cd98f00b204e9800998ecf8427e"),
       ("a", "0cc175b9c0f1b6a831c399e269772661"),
       ("abc", "900150983cd24fb0d6963f7d28e17f72"),
       ("message digest", "f96b697d7cb7938d525a2f31aaf161d0"),
       ("abcdefghijklmnopqrstuvwxyz", "c3fcd3d76192e4007dfb496cca67e13b"),
       ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
        "d174ab98d277d9f5a5611c2c9f419d9f"),
       ("12345678901234567890123456789012345678901234567890123456789012345678901234567890",
        "57edf4a22be3c955ac49da2e2107b67a")]
   
    fun do_tests () =  let
      fun f (x,s) = let
        val mstate = MD5.update (MD5.init,Byte.stringToBytes x)
        val hash = MD5.final (mstate)
      in print ("   input: "^x^"\n");
        print ("expected: "^s^"\n");
        print ("produced: "^MD5.toHexString (hash)^"\n")
      end
    in List.app f tests
    end
    val BLOCK_LEN = 10000
    val BLOCK_COUNT = 100000
    fun time_test () = let
      val block = Word8Vector.tabulate (BLOCK_LEN,Word8.fromInt)
      fun loop (n,s) =
        if n < BLOCK_COUNT then
          loop (n+1,MD5.update (s,block))
        else s
    in
       loop (0,MD5.init)
    end
  end

structure Main =
   struct
      fun doit n =
         if n = 0
            then ()
         else (Test.time_test ()
               ; doit (n - 1))
   end
