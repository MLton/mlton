(* From the SML/NJ benchmark suite. *)
(* tree.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * Trees for the TSP program.
 *)

structure Tree =
  struct

    datatype tree
      = NULL
      | ND of {
            left : tree, right : tree,
            x : real, y : real,
            sz : int,
            prev : tree ref, next : tree ref
          }

    fun mkNode (l, r, x, y, sz) = ND{
            left = l, right = r, x = x, y = y, sz = sz,
            prev = ref NULL, next = ref NULL
          }

    fun printTree (outS, NULL) = ()
      | printTree (outS, ND{x, y, left, right, ...}) = (
          TextIO.output(outS, String.concat [
            Real.toString x, " ", Real.toString y, "\n"]);
          printTree (outS, left);
          printTree (outS, right))

    fun printList (outS, NULL) = ()
      | printList (outS, start as ND{next, ...}) = let
          fun cycle (ND{next=next', ...}) = (next = next')
            | cycle _ = false
          fun prt (NULL) = ()
            | prt (t as ND{x, y, next, ...}) = (
                TextIO.output(outS, String.concat [
                    Real.toString x, " ", Real.toString y, "\n"
                  ]);
                if (cycle (!next))
                  then ()
                  else prt (!next))
          in
            prt start
          end

  end;

(* tsp.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *)

structure TSP : sig

    val tsp : (Tree.tree * int) -> Tree.tree

  end = struct

    structure T = Tree

    fun setPrev (T.ND{prev, ...}, x) = prev := x
    fun setNext (T.ND{next, ...}, x) = next := x
    fun link (a as T.ND{next, ...}, b as T.ND{prev, ...}) = (
          next := b; prev := a)

    fun sameNd (T.ND{next, ...}, T.ND{next=next', ...}) = (next = next')
      | sameNd (T.NULL, T.NULL) = true
      | sameNd _ = false

  (* Find Euclidean distance from a to b *)
    fun distance (T.ND{x=ax, y=ay, ...}, T.ND{x=bx, y=by, ...}) =
          Math.sqrt(((ax-bx)*(ax-bx)+(ay-by)*(ay-by)))
      | distance _ = raise Fail "distance"

  (* sling tree nodes into a list -- requires root to be tail of list, and
   * only fills in next field, not prev.
   *)
    fun makeList T.NULL = T.NULL
      | makeList (t as T.ND{left, right, next = t_next, ...}) = let
          val retVal = (case (makeList left, makeList right)
                 of (T.NULL, T.NULL) => t
                  | (l as T.ND{...}, T.NULL) => (setNext(left, t); l)
                  | (T.NULL, r as T.ND{...}) => (setNext(right, t); r)
                  | (l as T.ND{...}, r as T.ND{...}) => (
                      setNext(right, t); setNext(left, r); l)
                (* end case *))
          in
            t_next := T.NULL;
            retVal
          end

  (* reverse orientation of list *)
    fun reverse T.NULL = ()
      | reverse (t as T.ND{next, prev, ...}) = let
          fun rev (_, T.NULL) = ()
            | rev (back, tmp as T.ND{prev, next, ...}) = let
                val tmp' = !next
                in
                  next := back;  setPrev(back, tmp);
                  rev (tmp, tmp')
                end
          in
            setNext (!prev, T.NULL);
            prev := T.NULL;
            rev (t, !next)
          end

  (* Use closest-point heuristic from Cormen Leiserson and Rivest *)
    fun conquer (T.NULL) = T.NULL
      | conquer t = let
          val (cycle as T.ND{next=cycle_next, prev=cycle_prev, ...}) = makeList t
          fun loop (T.NULL) = ()
            | loop (t as T.ND{next=ref doNext, prev, ...}) =
                let
                fun findMinDist (min, minDist, tmp as T.ND{next, ...}) =
                      if (sameNd(cycle, tmp))
                        then min
                        else let
                          val test = distance(t, tmp)
                          in
                            if (test < minDist)
                              then findMinDist (tmp, test, !next)
                              else findMinDist (min, minDist, !next)
                          end
                val (min as T.ND{next=ref min_next, prev=ref min_prev, ...}) =
                        findMinDist (cycle, distance(t, cycle), !cycle_next)
                val minToNext = distance(min, min_next)
                val minToPrev = distance(min, min_prev)
                val tToNext = distance(t, min_next)
                val tToPrev = distance(t, min_prev)
                in
                  if ((tToPrev - minToPrev) < (tToNext - minToNext))
                    then ( (* insert between min and min_prev *)
                      link (min_prev, t);
                      link (t, min))
                    else (
                      link (min, t);
                      link (t, min_next));
                  loop doNext
                end
          val t' = !cycle_next
          in
          (* Create initial cycle *)
            cycle_next := cycle;  cycle_prev := cycle;
            loop t';
            cycle
          end

  (* Merge two cycles as per Karp *)
    fun merge (a as T.ND{next, ...}, b, t) = let
          fun locateCycle (start as T.ND{next, ...}) = let
                fun findMin (min, minDist, tmp as T.ND{next, ...}) =
                      if (sameNd(start, tmp))
                        then (min, minDist)
                        else let val test = distance(t, tmp)
                          in
                            if (test < minDist)
                              then findMin (tmp, test, !next)
                              else findMin (min, minDist, !next)
                          end
                val (min as T.ND{next=ref next', prev=ref prev', ...}, minDist) =
                        findMin (start, distance(t, start), !next)
                val minToNext = distance(min, next')
                val minToPrev = distance(min, prev')
                val tToNext = distance(t, next')
                val tToPrev = distance(t, prev')
                in
                  if ((tToPrev - minToPrev) < (tToNext - minToNext))
                  (* would insert between min and prev *)
                    then (prev', tToPrev, min, minDist)
                  (* would insert between min and next *)
                    else (min, minDist, next', tToNext)
                end
        (* Compute location for first cycle *)
          val (p1, tToP1, n1, tToN1) = locateCycle a
        (* compute location for second cycle *)
          val (p2, tToP2, n2, tToN2) = locateCycle b
        (* Now we have 4 choices to complete:
         *   1:t,p1 t,p2 n1,n2
         *   2:t,p1 t,n2 n1,p2
         *   3:t,n1 t,p2 p1,n2
         *   4:t,n1 t,n2 p1,p2
         *)
          val n1ToN2 = distance(n1, n2)
          val n1ToP2 = distance(n1, p2)
          val p1ToN2 = distance(p1, n2)
          val p1ToP2 = distance(p1, p2)
          fun choose (testChoice, test, choice, minDist) =
                if (test < minDist) then (testChoice, test) else (choice, minDist)
          val (choice, minDist) = (1, tToP1+tToP2+n1ToN2)
          val (choice, minDist) = choose(2, tToP1+tToN2+n1ToP2, choice, minDist)
          val (choice, minDist) = choose(3, tToN1+tToP2+p1ToN2, choice, minDist)
          val (choice, minDist) = choose(4, tToN1+tToN2+p1ToP2, choice, minDist)
          in
            case choice
             of 1 => (  (* 1:p1,t t,p2 n2,n1 -- reverse 2! *)
                  reverse n2;
                  link (p1, t);
                  link (t, p2);
                  link (n2, n1))
              | 2 => (  (* 2:p1,t t,n2 p2,n1 -- OK *)
                  link (p1, t);
                  link (t, n2);
                  link (p2, n1))
              | 3 => (  (* 3:p2,t t,n1 p1,n2 -- OK *)
                  link (p2, t);
                  link (t, n1);
                  link (p1, n2))
              | 4 => (  (* 4:n1,t t,n2 p2,p1 -- reverse 1! *)
                  reverse n1;
                  link (n1, t);
                  link (t, n2);
                  link (p2, p1))
            (* end case *);
            t
          end (* merge *)

  (* Compute TSP for the tree t -- use conquer for problems <= sz * *)
    fun tsp (t as T.ND{left, right, sz=sz', ...}, sz) =
          if (sz' <= sz)
            then conquer t
            else merge (tsp(left, sz), tsp(right, sz), t)
      | tsp (T.NULL, _) = T.NULL

  end;

(* rand-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories. See COPYRIGHT file for details.
 * COPYRIGHT (c) 1998 by AT&T Laboratories.
 *
 * Signature for a simple random number generator.
 *
 *)

signature RAND =
  sig

    type rand = Word.word

    val randMin : rand
    val randMax : rand

    val random : rand -> rand
      (* Given seed, return value randMin <= v <= randMax
       * Iteratively using the value returned by random as the
       * next seed to random will produce a sequence of pseudo-random
       * numbers.
       *)

    val mkRandom : rand -> unit -> rand
      (* Given seed, return function generating a sequence of
       * random numbers randMin <= v <= randMax
       *)

    val norm : rand -> real
      (* Map values in the range [randMin,randMax] to (0.0,1.0) *)

    val range : (int * int) -> rand -> int 
      (* Map v, randMin <= v <= randMax, to integer range [i,j]
       * Exception -
       *   Fail if j < i
       *)

  end (* RAND *)

(* rand.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.  See COPYRIGHT file for details
 * COPYRIGHT (c) 1998 by AT&T Laboratories.  See COPYRIGHT file for details
 *
 * Random number generator taken from Paulson, pp. 170-171.
 * Recommended by Stephen K. Park and Keith W. Miller, 
 * Random number generators: good ones are hard to find,
 * CACM 31 (1988), 1192-1201
 * Updated to include the new preferred multiplier of 48271
 * CACM 36 (1993), 105-110
 * Updated to use on Word.
 *
 * Note: The Random structure provides a better generator.
 *)

structure Rand : RAND =
  struct

    type rand = Word.word
    type rand' = Int.int  (* internal representation *)

    val a : rand' = 48271
    val m : rand' = valOf Int.maxInt  (* 2^31 - 1 *)
    val m_1 = m - 1
    val q = m div a
    val r = m mod a

    val extToInt = Word.toInt
    val intToExt = Word.fromInt

    val randMin : rand = 0w1
    val randMax : rand = intToExt m_1

    fun chk 0w0 = 1
      | chk 0wx7fffffff = m_1
      | chk seed = extToInt seed

    fun random' seed = let 
          val hi = seed div q
          val lo = seed mod q
          val test = a * lo - r * hi
          in
            if test > 0 then test else test + m
          end

    val random = intToExt o random' o chk

    fun mkRandom seed = let
          val seed = ref (chk seed)
          in
            fn () => (seed := random' (!seed); intToExt (!seed))
          end

    val real_m = Real.fromInt m
    fun norm s = (Real.fromInt (Word.toInt s)) / real_m

    fun range (i,j) = 
          if j < i 
            then raise Fail "Random.range: hi < lo"
          else if j = i then fn _ => i
          else let 
            val R = Int.fromInt j - Int.fromInt i
            val cvt = Word.toIntX o Word.fromInt
            in
              if R = m then Word.toIntX
              else fn s => i + cvt ((extToInt s) mod (R+1))
            end

  end (* Rand *)

(* build.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * Build a two-dimensional tree for TSP.
 *)

structure BuildTree : sig

    datatype axis = X_AXIS | Y_AXIS

    val buildTree : {
            n : int, dir : axis,
            min_x : real, min_y : real, max_x : real, max_y : real
          } -> Tree.tree

  end = struct

    structure T = Tree

    val m_e     = 2.7182818284590452354
    val m_e2    = 7.3890560989306502274
    val m_e3    = 20.08553692318766774179
    val m_e6    = 403.42879349273512264299
    val m_e12   = 162754.79141900392083592475

    datatype axis = X_AXIS | Y_AXIS

  (* builds a 2D tree of n nodes in specified range with dir as primary axis *)
    fun buildTree arg = let
          val rand = Rand.mkRandom 0w314
          fun drand48 () = Rand.norm (rand ())
          fun median {min, max, n} = let
                val t = drand48(); (* in [0.0..1.0) *)
                val retval = if (t > 0.5)
                      then Math.ln(1.0-(2.0*(m_e12-1.0)*(t-0.5)/m_e12))/12.0
                      else ~(Math.ln(1.0-(2.0*(m_e12-1.0)*t/m_e12))/12.0)
                in
                  min + ((retval + 1.0) * (max - min)/2.0)
                end
          fun uniform {min, max} = min + (drand48() * (max - min))
          fun build {n = 0, ...} = T.NULL
            | build {n, dir=X_AXIS, min_x, min_y, max_x, max_y} = let
                val med = median{min=min_y, max=max_y, n=n}
                fun mkTree (min, max) = build{
                        n=n div 2, dir=Y_AXIS, min_x=min_x, max_x=max_x,
                        min_y=min, max_y=max
                      }
                in
                  T.mkNode(
                    mkTree(min_y, med), mkTree(med, max_y),
                    uniform{min=min_x, max=max_x}, med, n)
                end
            | build {n, dir=Y_AXIS, min_x, min_y, max_x, max_y} = let
                val med = median{min=min_x, max=max_x, n=n}
                fun mkTree (min, max) = build{
                        n=n div 2, dir=X_AXIS, min_x=min, max_x=max,
                        min_y=min_y, max_y=max_y
                      }
                in
                  T.mkNode(
                    mkTree(min_x, med), mkTree(med, max_x),
                    med, uniform{min=min_y, max=max_y}, n)
                end
          in
            build arg
          end

  end; (* Build *)

signature BMARK =
  sig
    val doit : int -> unit
    val testit : TextIO.outstream -> unit
  end;
(* main.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 *)

structure Main : sig

    include BMARK

    val dumpPS : TextIO.outstream -> unit

  end = struct

    val name = "TSP"

    val problemSz = ref 32767
    val divideSz = ref 150

    fun printLength (outS, Tree.NULL) = print "(* 0 points *)\n"
      | printLength (outS, start as Tree.ND{next, x, y, ...}) = let
          fun cycle (Tree.ND{next=next', ...}) = (next = next')
            | cycle _ = false
          fun distance (ax, ay, bx, by) = let
                val dx = ax-bx and dy = ay-by
                in
                  Math.sqrt (dx*dx + dy*dy)
                end
          fun length (Tree.NULL, px, py, n, len) = (n, len+distance(px, py, x, y))
            | length (t as Tree.ND{x, y, next, ...}, px, py, n, len) =
                if (cycle t)
                  then (n, len+distance(px, py, x, y))
                  else length(!next, x, y, n+1, len+distance(px, py, x, y))
          in
            if (cycle(!next))
              then TextIO.output (outS, "(* 1 point *)\n")
              else let
                val (n, len) = length(!next, x, y, 1, 0.0)
                in
                  TextIO.output (outS, concat[
                      "(* ", Int.toString n, "points, cycle length = ",
                      Real.toString len, " *)\n"
                    ])
                end
          end

    fun mkTree n = BuildTree.buildTree {
            n=n, dir=BuildTree.X_AXIS,
            min_x=0.0, max_x=1.0, 
            min_y=0.0, max_y=1.0
          }

    fun doit' n = TSP.tsp (mkTree n, !divideSz)

    fun dumpPS outS = (
          TextIO.output (outS, "newgraph\n");
          TextIO.output (outS, "newcurve pts\n");
          Tree.printList (outS, doit' (!problemSz));
          TextIO.output (outS, "linetype solid\n"))

    fun testit strm = printLength (strm, doit' (!problemSz))

    val _ = problemSz := 2097151
    fun doit () = doit' (!problemSz)

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
