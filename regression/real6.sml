local
   val c = ref 0
in
   fun assert b =
      (c := !c + 1
       ; if b then ()
	 else print ("assertion " ^ Int.toString (!c) ^ " failed\n"))
end

open Real
open Math
open IEEEReal
infix 4 == != ?=

val classToString =
   fn NAN QUIET => "NAN QUIET"
    | NAN SIGNALLING => "NAN SIGNALLING"
    | INF => "INF"
    | ZERO => "ZERO"
    | NORMAL => "NORMAL"
    | SUBNORMAL => "SUBNORMAL"

val nan = posInf + negInf

val _ =
   app (fn spec =>
	app (fn x => (print (fmt spec x); print "\n"))
	[~1.234, 0.0, 1234.5678, 10E20, posInf, nan, negInf])
   let open StringCvt
   in [SCI NONE, FIX NONE, GEN NONE,
       SCI (SOME 0), FIX (SOME 0), GEN (SOME 1),
       SCI (SOME 10), FIX (SOME 10), GEN (SOME 10)]
   end

val _ =
   (
    List.app (fn (r,c) =>
	      (print (classToString (class r) ^ "\n")
	       ; assert (class r = c)))
    [(maxFinite, NORMAL),
     (minPos, SUBNORMAL),
     (minNormalPos, NORMAL),
     (nan, NAN QUIET),
     (posInf, INF),
     (negInf, INF),
     (1.0 / 0.0, INF),
     (~1.0 / 0.0, INF),
     (0.0, ZERO),
     (~0.0, ZERO)] ;

    (*11 check NaN's *)
    List.app (assert o isNan)
    [nan, nan + 1.0, nan - 1.0, nan * 1.0, nan/1.0] ;

    (*16 check infs *)
    List.app (assert o op ==)
    [(posInf + posInf, posInf),
     (negInf + negInf, negInf),
     (posInf - negInf, posInf),
     (negInf - posInf, negInf)];
    (*20*)
    List.app (assert o isNan)
    [posInf + negInf, negInf + posInf, posInf - posInf, negInf - negInf];

    (*24*)
    assert (1.0 + 2.0 == 2.0 + 1.0);
    assert (1.0 * 2.0 == 2.0 * 1.0);
    assert (1.0 - 2.0 == 0.0 - 1.0);
    assert ( *+ (1.0,2.0,3.0) == 1.0 * 2.0 + 3.0);
    assert ( *- (1.0,2.0,3.0) == 1.0 * 2.0 - 3.0);
    assert (~ (~ 1.0) == 1.0);
    (*30*)
    assert (~ posInf == negInf);
    assert (~ negInf == posInf);
    assert (abs 1.0 == 1.0);
    assert (abs ~1.0 == 1.0);
    assert (abs posInf == posInf);
    (*35*)
    assert (abs negInf == posInf);
    assert (min (1.0,2.0) == 1.0);
    assert (max (1.0,2.0) == 2.0);
    assert (sign 1.0 = 1);
    assert (sign 0.0 = 0);
    (*40*)
    assert (sign ~1.0 = ~1);
    assert (fromManExp (toManExp 1.0) == 1.0);
    ()
    )
