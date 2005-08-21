(*kitknuth_bendix36c.sml*)

(* quicksort-random.sml
 *
 * Input....:   Random list (pseudo-random integers)
 * Optimised:   'arg as ...' in quickSort'() and  partition(). 
 *              Copying left-parts after partitioning inside quickSort'().
 *              `Bertelsen transformation' of argument to tail-recursive
 *              call to quickSort'().
 *
 * Sestoft & Bertelsen, December 1995
 *)

val _ = 
let
fun map f nil = nil
  | map f (x :: L) = f x :: map f L
fun rev l =
  let fun rev_rec(p as ([], acc)) = p
        | rev_rec(x::xs, acc) = rev_rec(xs, x::acc)
  in #2 (rev_rec(l,nil))
  end
fun length [] = 0
  | length (x::xs) = 1 + length xs
fun app f [] = ()
  | app f (x::xs) = (f x; app f xs)


(* Quicksort -- Paulson p. 98 and answer to exercise 3.29 *)
(* Optimised for the Kit with Regions *)

(* NOTE: 
 * This is the most space efficient version of quicksort with the current 
 * storage mode analysis (implemented in 25q); copyList() will be called "sat"
 * inside partition() and the `innermost' recursive call to quickSort'() will 
 * be "atbot" for the regions holding right'. Unfortunately, calling 
 * copyList() after (the `innermost' recursive call to) quickSort'() means
 * that we  keep the regions holding the `original list' live during the
 * call to quickSort'(). This should not be necessary, since a::bs will be
 * copied (i.e. partitioned) into to left and right parts, but rules 28 and 26 
 * in the region analysis are a bit too conservative in this case...
 *)

  fun say(s) = print s

  type elem = int

  fun copyList [] = []
    | copyList (x::xr) = x::(copyList xr)

  fun quickSort' (arg as ([], sorted)) = arg
    | quickSort' ([a], sorted) = ([], a::sorted)
    | quickSort' (a::bs, sorted) =  (* "a" is the pivot *)
        let 
          fun partition (arg as (_, _, []: elem list)) = arg
            | partition (left, right, x::xr) =
                if x<=a then partition(x::left, right, xr)
                        else partition(left, x::right, xr)
          val arg' =
            let val (left', right) = 
                 let val (left, right, _) = partition([], [], bs)
                 in  (*forceResetting bs; *)
                     (copyList left, right)
                 end
                val sorted' = #2 (quickSort'(right, sorted))
            in
              (left', a::sorted')
            end
        in
          quickSort' arg'
        end
  fun quickSort l = #2 (quickSort'(l, []))


(* Generating random numbers.  Paulson, page 96 *)

  val min   = 1
  val max   = 100000
  val a     = 16807.0
  val m     = 2147483647.0
  val w     = real(max - min)/m
  fun seed0() = 117.0

  fun nextRand seed =
    let val t = a*seed
    in 
      t - m*real(floor(t/m))
    end

  fun randomList' (arg as (0, _, res)) = arg
    | randomList' (i, seed, res) =
      let val res' = min+floor(seed*w) :: res
          (* NOTE: It is significant to use seed for
           * calculating res' before calling nextRand()... 
           *)
      in
        randomList'(i-1, nextRand seed, res')
      end
  fun randomList n = #3 (randomList'(n, seed0(), []))


(* Building input list, sorting it and testing the result *)

  fun isSorted [] = true
    | isSorted [x: elem] = true
    | isSorted (x::(xr as (y::yr))) = (x <= y) andalso (isSorted xr)

in
  if isSorted (quickSort(randomList 100000)) then say("Ok!\n")
  else say("Oops...\n") 
end
