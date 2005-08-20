(*fft.sml*)

(*by Torben Mogensen (torbenm@diku.dk)*)


val pi = 3.14159265358979

fun pr (s : string) : unit = print(s)
exception Impossible
fun impossible s = impossible0 (s ^ "\n")
and impossible0 s = (pr ("\nimpossible: " ^ s); raise Impossible)

fun zipWith f ([],[]) = []
  | zipWith f ((a::b),(c::d)) = f (a,c) :: zipWith f (b,d)
  | zipWith f _ = impossible "zipWith"

fun zip ([],[]) = []
  | zip ((a::b),(c::d)) = (a,c) :: zip (b,d)
  | zip _ = impossible "zip"

fun ~+ ((x:real,y:real),(v,w)) = (x+v,y+w)

fun ~- ((x:real,y:real),(v,w)) = (x-v,y-w)

fun ~* ((x:real,y:real),(v,w)) = (x*v-y*w,x*w+y*v)

fun evens [] = []
  | evens (x::y::l) = x :: evens l
  | evens _ = impossible "evens"

fun odds [] = []
  | odds (x::y::l) = y :: odds l
  | odds _ = impossible "odds"

fun fmul (c,pin,[]) = []
  | fmul (c,pin,(a::b))
  =  ~*((Math.cos(c),Math.sin(c)), a) :: fmul (c+pin,pin,b)

fun cp [] = []
  | cp (a::b) = a :: cp b

fun fft ([(a,b)], 1)  = [(a+0.0,b+0.0)]
  | fft (x, n2)
  = let val n = n2 div 2
        val a = fft (evens x, n)
        val cb = fmul (0.0,pi/(real n),fft (odds x, n))
    in
        let val l1 =  zipWith ~+ (a,cb)
            val l2 =  zipWith ~- (a,cb)
        in (*resetRegions a; resetRegions cb;*) l1 @ l2
        end
    end

local val a = 16807.0 and m = 2147483678.0
in
   fun nextrand seed = 
         let val t = a*seed
         in t - m * real(floor (t/m)) end
end


fun mkList(tr as (seed,0,acc)) = tr
  | mkList(seed,n,acc) = mkList(nextrand seed, n-1, seed::acc)

val n = 256 * 256

fun run () = (pr "\nfft by Torben Mogensen (torbenm@diku.dk)\n\nfft'ing... ";
              let val r = fft (zip (#3(mkList(7.0,n,[])),
                                    #3(mkList(8.0,n,[]))), n) in
              pr " done\n" end);

val _ = run ()
