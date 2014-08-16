(*
 * 2001-2-14.
 * Stephen Weeks (sweeks@sweeks.com) generated this file from the hamlet SML
 * interpreter written by Andreas Rossberg.
 * The sources are from http://www.ps.uni-sb.de/~rossberg/hamlet/hamlet.tar
 *
 * The file consists of the concatenation of all of the source code (plus SML/NJ
 * library code) in the correct order, with a simple test case to test the
 * interpreter at the end.
 *
 * I also removed uses of the nonstandard Unsafe structure.
 *
 * I also made a minor change so that it could read in from a file instead of
 * from stdIn.
 *)

val ins = ref TextIO.stdIn

(* start of STAMP.sml *)
(*
 * Stamp generator.
 *)


signature STAMP =
  sig

    eqtype stamp

    val stamp:    unit  -> stamp
    val toString: stamp -> string

    val reset:    unit -> unit

    val compare:  stamp * stamp -> order

  end
(* stop of STAMP.sml *)
(* start of Stamp.sml *)
(*
 * Stamp generator.
 *)


structure Stamp :> STAMP =
  struct

    type stamp = int

    val r = ref 0

    fun reset()  =  r := 0
    fun stamp()  = (r := !r + 1; !r)

    val toString = Int.toString
    val compare  = Int.compare

  end
(* stop of Stamp.sml *)
(* start of smlnj-lib/Util/ord-key-sig.sml *)
(* ord-key-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Abstract linearly ordered keys.
 *
 *)

signature ORD_KEY =
  sig
    type ord_key

    val compare : ord_key * ord_key -> order

  end (* ORD_KEY *)
(* stop of smlnj-lib/Util/ord-key-sig.sml *)
(* start of smlnj-lib/Util/lib-base-sig.sml *)
(* lib-base-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *)

signature LIB_BASE =
  sig

    exception Unimplemented of string
        (* raised to report unimplemented features *)
    exception Impossible of string
        (* raised to report internal errors *)

    exception NotFound
        (* raised by searching operations *)

    val failure : {module : string, func : string, msg : string} -> 'a
        (* raise the exception Fail with a standard format message. *)

    val version : {date : string, system : string, version_id : int list}
    val banner : string

  end (* LIB_BASE *)

(* stop of smlnj-lib/Util/lib-base-sig.sml *)
(* start of smlnj-lib/Util/lib-base.sml *)
(* lib-base.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *)

structure LibBase : LIB_BASE =
  struct

  (* raised to report unimplemented features *)
    exception Unimplemented of string

  (* raised to report internal errors *)
    exception Impossible of string

  (* raised by searching operations *)
    exception NotFound

  (* raise the exception Fail with a standard format message. *)
    fun failure {module, func, msg} =
          raise (Fail(concat[module, ".", func, ": ", msg]))

    val version = {
            date = "June 1, 1996", 
            system = "SML/NJ Library",
            version_id = [1, 0]
          }

    fun f ([], l) = l
      | f ([x : int], l) = (Int.toString x)::l
      | f (x::r, l) = (Int.toString x) :: "." :: f(r, l)

    val banner = concat (
            #system version :: ", Version " ::
            f (#version_id version, [", ", #date version]))

  end (* LibBase *)

(* stop of smlnj-lib/Util/lib-base.sml *)
(* start of smlnj-lib/Util/ord-map-sig.sml *)
(* ord-map-sig.sml
 *
 * COPYRIGHT (c) 1996 by AT&T Research.  See COPYRIGHT file for details.
 *
 * Abstract signature of an applicative-style finite maps (dictionaries)
 * structure over ordered monomorphic keys.
 *)

signature ORD_MAP =
  sig

    structure Key : ORD_KEY

    type 'a map

    val empty : 'a map
        (* The empty map *)

    val isEmpty : 'a map -> bool
        (* Return true if and only if the map is empty *)

    val singleton : (Key.ord_key * 'a) -> 'a map
        (* return the specified singleton map *)

    val insert  : 'a map * Key.ord_key * 'a -> 'a map
    val insert' : ((Key.ord_key * 'a) * 'a map) -> 'a map
        (* Insert an item. *)

    val find : 'a map * Key.ord_key -> 'a option
        (* Look for an item, return NONE if the item doesn't exist *)

    val inDomain : ('a map * Key.ord_key) -> bool
        (* return true, if the key is in the domain of the map *)

    val remove : 'a map * Key.ord_key -> 'a map * 'a
        (* Remove an item, returning new map and value removed.
         * Raises LibBase.NotFound if not found.
         *)

    val first : 'a map -> 'a option
    val firsti : 'a map -> (Key.ord_key * 'a) option
        (* return the first item in the map (or NONE if it is empty) *)

    val numItems : 'a map ->  int
        (* Return the number of items in the map *)

    val listItems  : 'a map -> 'a list
    val listItemsi : 'a map -> (Key.ord_key * 'a) list
        (* Return an ordered list of the items (and their keys) in the map. *)

    val listKeys : 'a map -> Key.ord_key list
        (* return an ordered list of the keys in the map. *)

    val collate : ('a * 'a -> order) -> ('a map * 'a map) -> order
        (* given an ordering on the map's range, return an ordering
         * on the map.
         *)

    val unionWith  : ('a * 'a -> 'a) -> ('a map * 'a map) -> 'a map
    val unionWithi : (Key.ord_key * 'a * 'a -> 'a) -> ('a map * 'a map) -> 'a map
        (* return a map whose domain is the union of the domains of the two input
         * maps, using the supplied function to define the map on elements that
         * are in both domains.
         *)

    val intersectWith  : ('a * 'b -> 'c) -> ('a map * 'b map) -> 'c map
    val intersectWithi : (Key.ord_key * 'a * 'b -> 'c) -> ('a map * 'b map) -> 'c map
        (* return a map whose domain is the intersection of the domains of the
         * two input maps, using the supplied function to define the range.
         *)

    val app  : ('a -> unit) -> 'a map -> unit
    val appi : ((Key.ord_key * 'a) -> unit) -> 'a map -> unit
        (* Apply a function to the entries of the map in map order. *)

    val map  : ('a -> 'b) -> 'a map -> 'b map
    val mapi : (Key.ord_key * 'a -> 'b) -> 'a map -> 'b map
        (* Create a new map by applying a map function to the
         * name/value pairs in the map.
         *)

    val foldl  : ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val foldli : (Key.ord_key * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b
        (* Apply a folding function to the entries of the map
         * in increasing map order.
         *)

    val foldr  : ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val foldri : (Key.ord_key * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b
        (* Apply a folding function to the entries of the map
         * in decreasing map order.
         *)

    val filter  : ('a -> bool) -> 'a map -> 'a map
    val filteri : (Key.ord_key * 'a -> bool) -> 'a map -> 'a map
        (* Filter out those elements of the map that do not satisfy the
         * predicate.  The filtering is done in increasing map order.
         *)

    val mapPartial  : ('a -> 'b option) -> 'a map -> 'b map
    val mapPartiali : (Key.ord_key * 'a -> 'b option) -> 'a map -> 'b map
        (* map a partial function over the elements of a map in increasing
         * map order.
         *)

  end (* ORD_MAP *)
(* stop of smlnj-lib/Util/ord-map-sig.sml *)
(* start of smlnj-lib/Util/binary-map-fn.sml *)
(* binary-map-fn.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * This code was adapted from Stephen Adams' binary tree implementation
 * of applicative integer sets.
 *
 *   Copyright 1992 Stephen Adams.
 *
 *    This software may be used freely provided that:
 *      1. This copyright notice is attached to any copy, derived work,
 *         or work including all or part of this software.
 *      2. Any derived work must contain a prominent notice stating that
 *         it has been altered from the original.
 *
 *
 *   Name(s): Stephen Adams.
 *   Department, Institution: Electronics & Computer Science,
 *      University of Southampton
 *   Address:  Electronics & Computer Science
 *             University of Southampton
 *           Southampton  SO9 5NH
 *           Great Britian
 *   E-mail:   sra@ecs.soton.ac.uk
 *
 *   Comments:
 *
 *     1.  The implementation is based on Binary search trees of Bounded
 *         Balance, similar to Nievergelt & Reingold, SIAM J. Computing
 *         2(1), March 1973.  The main advantage of these trees is that
 *         they keep the size of the tree in the node, giving a constant
 *         time size operation.
 *
 *     2.  The bounded balance criterion is simpler than N&R's alpha.
 *         Simply, one subtree must not have more than `weight' times as
 *         many elements as the opposite subtree.  Rebalancing is
 *         guaranteed to reinstate the criterion for weight>2.23, but
 *         the occasional incorrect behaviour for weight=2 is not
 *         detrimental to performance.
 *
 *)

functor BinaryMapFn (K : ORD_KEY) : ORD_MAP =
  struct

    structure Key = K

    (*
    **  val weight = 3
    **  fun wt i = weight * i
    *)
    fun wt (i : int) = i + i + i

    datatype 'a map
      = E 
      | T of {
          key : K.ord_key, 
          value : 'a, 
          cnt : int, 
          left : 'a map, 
          right : 'a map
        }

    val empty = E

    fun isEmpty E = true
      | isEmpty _ = false

    fun numItems E = 0
      | numItems (T{cnt,...}) = cnt

  (* return the first item in the map (or NONE if it is empty) *)
    fun first E = NONE
      | first (T{value, left=E, ...}) = SOME value
      | first (T{left, ...}) = first left

  (* return the first item in the map and its key (or NONE if it is empty) *)
    fun firsti E = NONE
      | firsti (T{key, value, left=E, ...}) = SOME(key, value)
      | firsti (T{left, ...}) = firsti left

local
    fun N(k,v,E,E) = T{key=k,value=v,cnt=1,left=E,right=E}
      | N(k,v,E,r as T n) = T{key=k,value=v,cnt=1+(#cnt n),left=E,right=r}
      | N(k,v,l as T n,E) = T{key=k,value=v,cnt=1+(#cnt n),left=l,right=E}
      | N(k,v,l as T n,r as T n') = 
          T{key=k,value=v,cnt=1+(#cnt n)+(#cnt n'),left=l,right=r}

    fun single_L (a,av,x,T{key=b,value=bv,left=y,right=z,...}) = 
          N(b,bv,N(a,av,x,y),z)
      | single_L _ = raise Match
    fun single_R (b,bv,T{key=a,value=av,left=x,right=y,...},z) = 
          N(a,av,x,N(b,bv,y,z))
      | single_R _ = raise Match
    fun double_L (a,av,w,T{key=c,value=cv,left=T{key=b,value=bv,left=x,right=y,...},right=z,...}) =
          N(b,bv,N(a,av,w,x),N(c,cv,y,z))
      | double_L _ = raise Match
    fun double_R (c,cv,T{key=a,value=av,left=w,right=T{key=b,value=bv,left=x,right=y,...},...},z) = 
          N(b,bv,N(a,av,w,x),N(c,cv,y,z))
      | double_R _ = raise Match

    fun T' (k,v,E,E) = T{key=k,value=v,cnt=1,left=E,right=E}
      | T' (k,v,E,r as T{right=E,left=E,...}) =
          T{key=k,value=v,cnt=2,left=E,right=r}
      | T' (k,v,l as T{right=E,left=E,...},E) =
          T{key=k,value=v,cnt=2,left=l,right=E}

      | T' (p as (_,_,E,T{left=T _,right=E,...})) = double_L p
      | T' (p as (_,_,T{left=E,right=T _,...},E)) = double_R p

        (* these cases almost never happen with small weight*)
      | T' (p as (_,_,E,T{left=T{cnt=ln,...},right=T{cnt=rn,...},...})) =
          if ln < rn then single_L p else double_L p
      | T' (p as (_,_,T{left=T{cnt=ln,...},right=T{cnt=rn,...},...},E)) =
          if ln > rn then single_R p else double_R p

      | T' (p as (_,_,E,T{left=E,...})) = single_L p
      | T' (p as (_,_,T{right=E,...},E)) = single_R p

      | T' (p as (k,v,l as T{cnt=ln,left=ll,right=lr,...},
                      r as T{cnt=rn,left=rl,right=rr,...})) =
          if rn >= wt ln then (*right is too big*)
            let val rln = numItems rl
                val rrn = numItems rr
            in
              if rln < rrn then  single_L p  else  double_L p
            end
        
          else if ln >= wt rn then  (*left is too big*)
            let val lln = numItems ll
                val lrn = numItems lr
            in
              if lrn < lln then  single_R p  else  double_R p
            end
    
          else T{key=k,value=v,cnt=ln+rn+1,left=l,right=r}

    local
      fun min (T{left=E,key,value,...}) = (key,value)
        | min (T{left,...}) = min left
        | min _ = raise Match
  
      fun delmin (T{left=E,right,...}) = right
        | delmin (T{key,value,left,right,...}) = T'(key,value,delmin left,right)
        | delmin _ = raise Match
    in
      fun delete' (E,r) = r
        | delete' (l,E) = l
        | delete' (l,r) = let val (mink,minv) = min r in
            T'(mink,minv,l,delmin r)
          end
    end
in
    fun mkDict () = E
    
    fun singleton (x,v) = T{key=x,value=v,cnt=1,left=E,right=E}

    fun insert (E,x,v) = T{key=x,value=v,cnt=1,left=E,right=E}
      | insert (T(set as {key,left,right,value,...}),x,v) =
          case K.compare (key,x) of
            GREATER => T'(key,value,insert(left,x,v),right)
          | LESS => T'(key,value,left,insert(right,x,v))
          | _ => T{key=x,value=v,left=left,right=right,cnt= #cnt set}
    fun insert' ((k, x), m) = insert(m, k, x)

    fun inDomain (set, x) = let 
          fun mem E = false
            | mem (T(n as {key,left,right,...})) = (case K.compare (x,key)
                 of GREATER => mem right
                  | EQUAL => true
                  | LESS => mem left
                (* end case *))
          in
            mem set
          end

    fun find (set, x) = let 
          fun mem E = NONE
            | mem (T(n as {key,left,right,...})) = (case K.compare (x,key)
                 of GREATER => mem right
                  | EQUAL => SOME(#value n)
                  | LESS => mem left
                (* end case *))
          in
            mem set
          end

    fun remove (E,x) = raise LibBase.NotFound
      | remove (set as T{key,left,right,value,...},x) = (
          case K.compare (key,x)
           of GREATER => let
                val (left', v) = remove(left, x)
                in
                  (T'(key, value, left', right), v)
                end
            | LESS => let
                val (right', v) = remove (right, x)
                in
                  (T'(key, value, left, right'), v)
                end
            | _ => (delete'(left,right),value)
          (* end case *))

    fun listItems d = let
          fun d2l (E, l) = l
            | d2l (T{key,value,left,right,...}, l) =
                d2l(left, value::(d2l(right,l)))
          in
            d2l (d,[])
          end

    fun listItemsi d = let
          fun d2l (E, l) = l
            | d2l (T{key,value,left,right,...}, l) =
                d2l(left, (key,value)::(d2l(right,l)))
          in
            d2l (d,[])
          end

    fun listKeys d = let
          fun d2l (E, l) = l
            | d2l (T{key,left,right,...}, l) = d2l(left, key::(d2l(right,l)))
          in
            d2l (d,[])
          end

    local
      fun next ((t as T{right, ...})::rest) = (t, left(right, rest))
        | next _ = (E, [])
      and left (E, rest) = rest
        | left (t as T{left=l, ...}, rest) = left(l, t::rest)
    in
    fun collate cmpRng (s1, s2) = let
          fun cmp (t1, t2) = (case (next t1, next t2)
                 of ((E, _), (E, _)) => EQUAL
                  | ((E, _), _) => LESS
                  | (_, (E, _)) => GREATER
                  | ((T{key=x1, value=y1, ...}, r1), (T{key=x2, value=y2, ...}, r2)) => (
                      case Key.compare(x1, x2)
                       of EQUAL => (case cmpRng(y1, y2)
                             of EQUAL => cmp (r1, r2)
                              | order => order
                            (* end case *))
                        | order => order
                      (* end case *))
                (* end case *))
          in
            cmp (left(s1, []), left(s2, []))
          end
    end (* local *)

    fun appi f d = let
          fun app' E = ()
            | app' (T{key,value,left,right,...}) = (
                app' left; f(key, value); app' right)
          in
            app' d
          end
    fun app f d = let
          fun app' E = ()
            | app' (T{value,left,right,...}) = (
                app' left; f value; app' right)
          in
            app' d
          end

    fun mapi f d = let
          fun map' E = E
            | map' (T{key,value,left,right,cnt}) = let
                val left' = map' left
                val value' = f(key, value)
                val right' = map' right
                in
                  T{cnt=cnt, key=key, value=value', left = left', right = right'}
                end
          in
            map' d
          end
    fun map f d = mapi (fn (_, x) => f x) d

    fun foldli f init d = let
          fun fold (E, v) = v
            | fold (T{key,value,left,right,...}, v) =
                fold (right, f(key, value, fold(left, v)))
          in
            fold (d, init)
          end
    fun foldl f init d = foldli (fn (_, v, accum) => f (v, accum)) init d

    fun foldri f init d = let
          fun fold (E,v) = v
            | fold (T{key,value,left,right,...},v) =
                fold (left, f(key, value, fold(right, v)))
          in
            fold (d, init)
          end
    fun foldr f init d = foldri (fn (_, v, accum) => f (v, accum)) init d

(** To be implemented **
    val filter  : ('a -> bool) -> 'a map -> 'a map
    val filteri : (Key.ord_key * 'a -> bool) -> 'a map -> 'a map
**)

    end (* local *)

(* the following are generic implementations of the unionWith and intersectWith
 * operetions.  These should be specialized for the internal representations
 * at some point.
 *)
    fun unionWith f (m1, m2) = let
          fun ins  f (key, x, m) = (case find(m, key)
                 of NONE => insert(m, key, x)
                  | (SOME x') => insert(m, key, f(x, x'))
                (* end case *))
          in
            if (numItems m1 > numItems m2)
              then foldli (ins (fn (a, b) => f (b, a))) m1 m2
              else foldli (ins f) m2 m1
          end
    fun unionWithi f (m1, m2) = let
          fun ins f (key, x, m) = (case find(m, key)
                 of NONE => insert(m, key, x)
                  | (SOME x') => insert(m, key, f(key, x, x'))
                (* end case *))
          in
            if (numItems m1 > numItems m2)
              then foldli (ins (fn (k, a, b) => f (k, b, a))) m1 m2
              else foldli (ins f) m2 m1
          end

    fun intersectWith f (m1, m2) = let
        (* iterate over the elements of m1, checking for membership in m2 *)
          fun intersect f (m1, m2) = let
                fun ins (key, x, m) = (case find(m2, key)
                       of NONE => m
                        | (SOME x') => insert(m, key, f(x, x'))
                      (* end case *))
                in
                  foldli ins empty m1
                end
          in
            if (numItems m1 > numItems m2)
              then intersect f (m1, m2)
              else intersect (fn (a, b) => f(b, a)) (m2, m1)
          end
    fun intersectWithi f (m1, m2) = let
        (* iterate over the elements of m1, checking for membership in m2 *)
          fun intersect f (m1, m2) = let
                fun ins (key, x, m) = (case find(m2, key)
                       of NONE => m
                        | (SOME x') => insert(m, key, f(key, x, x'))
                      (* end case *))
                in
                  foldli ins empty m1
                end
          in
            if (numItems m1 > numItems m2)
              then intersect f (m1, m2)
              else intersect (fn (k, a, b) => f(k, b, a)) (m2, m1)
          end

  (* this is a generic implementation of filter.  It should
   * be specialized to the data-structure at some point.
   *)
    fun filter predFn m = let
          fun f (key, item, m) = if predFn item
                then insert(m, key, item)
                else m
          in
            foldli f empty m
          end
    fun filteri predFn m = let
          fun f (key, item, m) = if predFn(key, item)
                then insert(m, key, item)
                else m
          in
            foldli f empty m
          end

  (* this is a generic implementation of mapPartial.  It should
   * be specialized to the data-structure at some point.
   *)
    fun mapPartial f m = let
          fun g (key, item, m) = (case f item
                 of NONE => m
                  | (SOME item') => insert(m, key, item')
                (* end case *))
          in
            foldli g empty m
          end
    fun mapPartiali f m = let
          fun g (key, item, m) = (case f(key, item)
                 of NONE => m
                  | (SOME item') => insert(m, key, item')
                (* end case *))
          in
            foldli g empty m
          end

  end (* functor BinaryMapFn *)
(* stop of smlnj-lib/Util/binary-map-fn.sml *)
(* start of FIN_MAP.sml *)
(*
 * Standard ML finite maps
 *
 * Definition, section 4.2
 *
 * Note:
 *     This signature just extends the one available in the SML/NJ lib.
 *     Actually, the operation added here would be general purpose and useful enough
 *     (and more efficient) to be in the lib. Also see FIN_SET.
 *)

signature FIN_MAP =
  sig

    include ORD_MAP

    val fromList:       (Key.ord_key * 'a) list -> 'a map

    val all:            ('a -> bool) -> 'a map -> bool
    val exists:         ('a -> bool) -> 'a map -> bool
    val alli:           (Key.ord_key * 'a -> bool) -> 'a map -> bool
    val existsi:        (Key.ord_key * 'a -> bool) -> 'a map -> bool

    val disjoint:       'a map * 'a map -> bool

  end
(* stop of FIN_MAP.sml *)
(* start of FinMapFn.sml *)
(*
 * Standard ML finite maps
 *
 * Definition, section 4.2
 *
 * Note:
 *     This functor just extends the one available in the SML/NJ lib.
 *     Actually, the operation added here would be general purpose and useful enough
 *     (and more efficient) to be in the lib. Also see FinSetFn.
 *)

functor FinMapFn(Key: ORD_KEY) :> FIN_MAP where type Key.ord_key = Key.ord_key =
  struct

    structure BinaryMap = BinaryMapFn(Key)

    open BinaryMap

    fun fromList kvs    = List.foldl (fn((k, v),m) => insert(m, k, v)) empty kvs

    fun all p           = foldl (fn(v, b) => b andalso p v) true
    fun exists p        = foldl (fn(v, b) => b orelse p v) false
    fun alli p          = foldli (fn(k, v, b) => b andalso p(k, v)) true
    fun existsi p       = foldli (fn(k, v, b) => b orelse p(k, v)) false

    fun disjoint(m1,m2) = isEmpty(intersectWith #2 (m1, m2))

  end
(* stop of FinMapFn.sml *)
(* start of ID.sml *)
(*
 * Standard ML identifiers
 *
 * Definition, section 2.4
 *
 * Note:
 *   This is a generic signature to represent all kinds of identifiers (except
 *   for labels and tyvars).
 *)


signature ID =
  sig

    (* Type [Section 2.4] *)

    eqtype Id                                   (* [id] *)

    (* Operations *)

    val invent:         unit -> Id

    val fromString:     string -> Id
    val toString:       Id -> string

    val compare:        Id * Id -> order

  end
(* stop of ID.sml *)
(* start of IdFn.sml *)
(*
 * Standard ML identifiers
 *
 * Definition, section 2.4
 *
 * Note:
 *   This is a generic functor to represent all kinds of identifiers (except
 *   for labels tyvars).
 *)


functor IdFn() :> ID =
  struct

    (* Type [Section 2.4] *)

    type Id = string                            (* [id] *)


    (* Creation *)

    fun invent()     = "_id" ^ Stamp.toString(Stamp.stamp())

    fun fromString s = s
    fun toString s   = s


    (* Ordering *)

    val compare = String.compare

  end
(* stop of IdFn.sml *)
(* start of IdsModule.sml *)
(*
 * Standard ML identifiers for modules
 *
 * Definition, section 3.2
 *)


structure SigId = IdFn()
structure FunId = IdFn()
(* stop of IdsModule.sml *)
(* start of AssembliesModule.sml *)
(*
 * Standard ML sets and maps for the module semantics
 *
 * Definition, sections 5.1 and 7.2
 *)

structure SigIdMap   = FinMapFn(type ord_key = SigId.Id
                                val  compare = SigId.compare)

structure FunIdMap   = FinMapFn(type ord_key = FunId.Id
                                val  compare = FunId.compare)
(* stop of AssembliesModule.sml *)
(* start of LONGID.sml *)
(*
 * Standard ML long identifiers
 *
 * Definition, section 2.4
 *
 * Note:
 *   This is a generic signature to represent all kinds of long identifiers.
 *)


signature LONGID =
  sig

    (* Import *)

    structure Id:    ID
    structure StrId: ID

    type Id    = Id.Id
    type StrId = StrId.Id


    (* Type [Section 2.4] *)

    eqtype longId                               (* [longid] *)


    (* Operations *)

    val invent:         unit   -> longId
    val fromId:         Id     -> longId
    val toId:           longId -> Id
    val toString:       longId -> string

    val strengthen:     StrId * longId -> longId
    val implode:        StrId list * Id -> longId
    val explode:        longId -> StrId list * Id

    val isUnqualified:  longId -> bool

    val compare:        longId * longId -> order

  end
(* stop of LONGID.sml *)
(* start of LongIdFn.sml *)
(*
 * Standard ML long identifiers
 *
 * Definition, section 2.4
 *
 * Note:
 *   This is a generic functor that generates a long identifier type from a
 *   given identifier type and the StrId type.
 *)


functor LongIdFn(structure Id:    ID
                 structure StrId: ID
                ) :> LONGID where type Id.Id    = Id.Id
                            and   type StrId.Id = StrId.Id
                  =
  struct

    (* Import *)

    structure Id    = Id
    structure StrId = StrId

    type Id         = Id.Id
    type StrId      = StrId.Id


    (* Type [Section 2.4] *)

    type longId = StrId list * Id                       (* [longid] *)


    (* Conversions *)

    fun toId(strid, id) = id
    fun fromId id       = ([],id)
    fun invent()        = ([],Id.invent())

    fun toString(strids, id) =
        let
            fun prefix   []     = Id.toString id
              | prefix(id::ids) = StrId.toString id ^ "." ^ prefix ids
        in
            prefix strids
        end

    fun strengthen(strid, (strids, id)) = (strid::strids, id)

    fun implode longid = longid
    fun explode longid = longid

    fun isUnqualified (strids,id) = List.null strids


    (* Ordering *)

    fun compare(longid1, longid2) =
            String.compare(toString longid1, toString longid2)

  end
(* stop of LongIdFn.sml *)
(* start of IdsCore.sml *)
(*
 * Standard ML identifiers for the core
 *
 * Definition, section 2.4
 *)


structure VId       = IdFn()
structure TyCon     = IdFn()
structure StrId     = IdFn()

structure LongVId   = LongIdFn(structure Id    = VId
                               structure StrId = StrId)
structure LongTyCon = LongIdFn(structure Id    = TyCon
                               structure StrId = StrId)
structure LongStrId = LongIdFn(structure Id    = StrId
                               structure StrId = StrId)
(* stop of IdsCore.sml *)
(* start of smlnj-lib/Util/ord-set-sig.sml *)
(* ordset-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Signature for a set of values with an order relation.
 *)

signature ORD_SET =
  sig

    structure Key : ORD_KEY

    type item = Key.ord_key
    type set

    val empty : set
        (* The empty set *)

    val singleton : item -> set
        (* Create a singleton set *)

    val add  : set * item -> set
    val add' : (item * set) -> set
        (* Insert an item. *)

    val addList : set * item list -> set
        (* Insert items from list. *)

    val delete : set * item -> set
        (* Remove an item. Raise NotFound if not found. *)

    val member : set * item -> bool
        (* Return true if and only if item is an element in the set *)

    val isEmpty : set -> bool
        (* Return true if and only if the set is empty *)

    val equal : (set * set) -> bool
        (* Return true if and only if the two sets are equal *)

    val compare : (set * set) -> order
        (* does a lexical comparison of two sets *)

    val isSubset : (set * set) -> bool
        (* Return true if and only if the first set is a subset of the second *)

    val numItems : set ->  int
        (* Return the number of items in the table *)

    val listItems : set -> item list
        (* Return an ordered list of the items in the set *)

    val union : set * set -> set
        (* Union *)

    val intersection : set * set -> set
        (* Intersection *)

    val difference : set * set -> set
        (* Difference *)

    val map : (item -> item) -> set -> set
        (* Create a new set by applying a map function to the elements
         * of the set.
         *)
     
    val app : (item -> unit) -> set -> unit
        (* Apply a function to the entries of the set 
         * in decreasing order
         *)

    val foldl : (item * 'b -> 'b) -> 'b -> set -> 'b
        (* Apply a folding function to the entries of the set 
         * in increasing order
         *)

    val foldr : (item * 'b -> 'b) -> 'b -> set -> 'b
        (* Apply a folding function to the entries of the set 
         * in decreasing order
         *)

    val filter : (item -> bool) -> set -> set

    val exists : (item -> bool) -> set -> bool

    val find : (item -> bool) -> set -> item option

  end (* ORD_SET *)
(* stop of smlnj-lib/Util/ord-set-sig.sml *)
(* start of smlnj-lib/Util/binary-set-fn.sml *)
(* binary-set-fn.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * This code was adapted from Stephen Adams' binary tree implementation
 * of applicative integer sets.
 *
 *    Copyright 1992 Stephen Adams.
 *
 *    This software may be used freely provided that:
 *      1. This copyright notice is attached to any copy, derived work,
 *         or work including all or part of this software.
 *      2. Any derived work must contain a prominent notice stating that
 *         it has been altered from the original.
 *
 *   Name(s): Stephen Adams.
 *   Department, Institution: Electronics & Computer Science,
 *      University of Southampton
 *   Address:  Electronics & Computer Science
 *             University of Southampton
 *         Southampton  SO9 5NH
 *         Great Britian
 *   E-mail:   sra@ecs.soton.ac.uk
 *
 *   Comments:
 *
 *     1.  The implementation is based on Binary search trees of Bounded
 *         Balance, similar to Nievergelt & Reingold, SIAM J. Computing
 *         2(1), March 1973.  The main advantage of these trees is that
 *         they keep the size of the tree in the node, giving a constant
 *         time size operation.
 *
 *     2.  The bounded balance criterion is simpler than N&R's alpha.
 *         Simply, one subtree must not have more than `weight' times as
 *         many elements as the opposite subtree.  Rebalancing is
 *         guaranteed to reinstate the criterion for weight>2.23, but
 *         the occasional incorrect behaviour for weight=2 is not
 *         detrimental to performance.
 *
 *     3.  There are two implementations of union.  The default,
 *         hedge_union, is much more complex and usually 20% faster.  I
 *         am not sure that the performance increase warrants the
 *         complexity (and time it took to write), but I am leaving it
 *         in for the competition.  It is derived from the original
 *         union by replacing the split_lt(gt) operations with a lazy
 *         version. The `obvious' version is called old_union.
 *
 *     4.  Most time is spent in T', the rebalancing constructor.  If my
 *         understanding of the output of *<file> in the sml batch
 *         compiler is correct then the code produced by NJSML 0.75
 *         (sparc) for the final case is very disappointing.  Most
 *         invocations fall through to this case and most of these cases
 *         fall to the else part, i.e. the plain contructor,
 *         T(v,ln+rn+1,l,r).  The poor code allocates a 16 word vector
 *         and saves lots of registers into it.  In the common case it
 *         then retrieves a few of the registers and allocates the 5
 *         word T node.  The values that it retrieves were live in
 *         registers before the massive save.
 *
 *   Modified to functor to support general ordered values
 *)

functor BinarySetFn (K : ORD_KEY) : ORD_SET =
  struct

    structure Key = K

    type item = K.ord_key

    datatype set
      = E 
      | T of {
          elt : item, 
          cnt : int, 
          left : set,
          right : set
        }

    fun numItems E = 0
      | numItems (T{cnt,...}) = cnt
        
    fun isEmpty E = true
      | isEmpty _ = false

    fun mkT(v,n,l,r) = T{elt=v,cnt=n,left=l,right=r}

      (* N(v,l,r) = T(v,1+numItems(l)+numItems(r),l,r) *)
    fun N(v,E,E) = mkT(v,1,E,E)
      | N(v,E,r as T{cnt=n,...}) = mkT(v,n+1,E,r)
      | N(v,l as T{cnt=n,...}, E) = mkT(v,n+1,l,E)
      | N(v,l as T{cnt=n,...}, r as T{cnt=m,...}) = mkT(v,n+m+1,l,r)

    fun single_L (a,x,T{elt=b,left=y,right=z,...}) = N(b,N(a,x,y),z)
      | single_L _ = raise Match
    fun single_R (b,T{elt=a,left=x,right=y,...},z) = N(a,x,N(b,y,z))
      | single_R _ = raise Match
    fun double_L (a,w,T{elt=c,left=T{elt=b,left=x,right=y,...},right=z,...}) =
          N(b,N(a,w,x),N(c,y,z))
      | double_L _ = raise Match
    fun double_R (c,T{elt=a,left=w,right=T{elt=b,left=x,right=y,...},...},z) =
          N(b,N(a,w,x),N(c,y,z))
      | double_R _ = raise Match

    (*
    **  val weight = 3
    **  fun wt i = weight * i
    *)
    fun wt (i : int) = i + i + i

    fun T' (v,E,E) = mkT(v,1,E,E)
      | T' (v,E,r as T{left=E,right=E,...}) = mkT(v,2,E,r)
      | T' (v,l as T{left=E,right=E,...},E) = mkT(v,2,l,E)

      | T' (p as (_,E,T{left=T _,right=E,...})) = double_L p
      | T' (p as (_,T{left=E,right=T _,...},E)) = double_R p

        (* these cases almost never happen with small weight*)
      | T' (p as (_,E,T{left=T{cnt=ln,...},right=T{cnt=rn,...},...})) =
            if ln<rn then single_L p else double_L p
      | T' (p as (_,T{left=T{cnt=ln,...},right=T{cnt=rn,...},...},E)) =
            if ln>rn then single_R p else double_R p

      | T' (p as (_,E,T{left=E,...})) = single_L p
      | T' (p as (_,T{right=E,...},E)) = single_R p

      | T' (p as (v,l as T{elt=lv,cnt=ln,left=ll,right=lr},
              r as T{elt=rv,cnt=rn,left=rl,right=rr})) =
          if rn >= wt ln (*right is too big*)
            then
              let val rln = numItems rl
                  val rrn = numItems rr
              in
                if rln < rrn then single_L p else double_L p
              end
          else if ln >= wt rn (*left is too big*)
            then
              let val lln = numItems ll
                  val lrn = numItems lr
              in
                if lrn < lln then single_R p else double_R p
              end
          else mkT(v,ln+rn+1,l,r)

    fun add (E,x) = mkT(x,1,E,E)
      | add (set as T{elt=v,left=l,right=r,cnt},x) =
          case K.compare(x,v) of
            LESS => T'(v,add(l,x),r)
          | GREATER => T'(v,l,add(r,x))
          | EQUAL => mkT(x,cnt,l,r)
    fun add' (s, x) = add(x, s)

    fun concat3 (E,v,r) = add(r,v)
      | concat3 (l,v,E) = add(l,v)
      | concat3 (l as T{elt=v1,cnt=n1,left=l1,right=r1}, v, 
                  r as T{elt=v2,cnt=n2,left=l2,right=r2}) =
        if wt n1 < n2 then T'(v2,concat3(l,v,l2),r2)
        else if wt n2 < n1 then T'(v1,l1,concat3(r1,v,r))
        else N(v,l,r)

    fun split_lt (E,x) = E
      | split_lt (T{elt=v,left=l,right=r,...},x) =
          case K.compare(v,x) of
            GREATER => split_lt(l,x)
          | LESS => concat3(l,v,split_lt(r,x))
          | _ => l

    fun split_gt (E,x) = E
      | split_gt (T{elt=v,left=l,right=r,...},x) =
          case K.compare(v,x) of
            LESS => split_gt(r,x)
          | GREATER => concat3(split_gt(l,x),v,r)
          | _ => r

    fun min (T{elt=v,left=E,...}) = v
      | min (T{left=l,...}) = min l
      | min _ = raise Match
        
    fun delmin (T{left=E,right=r,...}) = r
      | delmin (T{elt=v,left=l,right=r,...}) = T'(v,delmin l,r)
      | delmin _ = raise Match

    fun delete' (E,r) = r
      | delete' (l,E) = l
      | delete' (l,r) = T'(min r,l,delmin r)

    fun concat (E, s) = s
      | concat (s, E) = s
      | concat (t1 as T{elt=v1,cnt=n1,left=l1,right=r1}, 
                  t2 as T{elt=v2,cnt=n2,left=l2,right=r2}) =
          if wt n1 < n2 then T'(v2,concat(t1,l2),r2)
          else if wt n2 < n1 then T'(v1,l1,concat(r1,t2))
          else T'(min t2,t1, delmin t2)


    local
      fun trim (lo,hi,E) = E
        | trim (lo,hi,s as T{elt=v,left=l,right=r,...}) =
            if K.compare(v,lo) = GREATER
              then if K.compare(v,hi) = LESS then s else trim(lo,hi,l)
              else trim(lo,hi,r)
                
      fun uni_bd (s,E,_,_) = s
        | uni_bd (E,T{elt=v,left=l,right=r,...},lo,hi) = 
             concat3(split_gt(l,lo),v,split_lt(r,hi))
        | uni_bd (T{elt=v,left=l1,right=r1,...}, 
                   s2 as T{elt=v2,left=l2,right=r2,...},lo,hi) =
            concat3(uni_bd(l1,trim(lo,v,s2),lo,v),
                v, 
                uni_bd(r1,trim(v,hi,s2),v,hi))
              (* inv:  lo < v < hi *)

        (* all the other versions of uni and trim are
         * specializations of the above two functions with
         *     lo=-infinity and/or hi=+infinity 
         *)

      fun trim_lo (_, E) = E
        | trim_lo (lo,s as T{elt=v,right=r,...}) =
            case K.compare(v,lo) of
              GREATER => s
            | _ => trim_lo(lo,r)

      fun trim_hi (_, E) = E
        | trim_hi (hi,s as T{elt=v,left=l,...}) =
            case K.compare(v,hi) of
              LESS => s
            | _ => trim_hi(hi,l)
                
      fun uni_hi (s,E,_) = s
        | uni_hi (E,T{elt=v,left=l,right=r,...},hi) = 
             concat3(l,v,split_lt(r,hi))
        | uni_hi (T{elt=v,left=l1,right=r1,...}, 
                   s2 as T{elt=v2,left=l2,right=r2,...},hi) =
            concat3(uni_hi(l1,trim_hi(v,s2),v),v,uni_bd(r1,trim(v,hi,s2),v,hi))

      fun uni_lo (s,E,_) = s
        | uni_lo (E,T{elt=v,left=l,right=r,...},lo) = 
             concat3(split_gt(l,lo),v,r)
        | uni_lo (T{elt=v,left=l1,right=r1,...}, 
                   s2 as T{elt=v2,left=l2,right=r2,...},lo) =
            concat3(uni_bd(l1,trim(lo,v,s2),lo,v),v,uni_lo(r1,trim_lo(v,s2),v))

      fun uni (s,E) = s
        | uni (E,s) = s
        | uni (T{elt=v,left=l1,right=r1,...}, 
                s2 as T{elt=v2,left=l2,right=r2,...}) =
            concat3(uni_hi(l1,trim_hi(v,s2),v), v, uni_lo(r1,trim_lo(v,s2),v))

    in
      val hedge_union = uni
    end

      (* The old_union version is about 20% slower than
       *  hedge_union in most cases 
       *)
    fun old_union (E,s2)  = s2
      | old_union (s1,E)  = s1
      | old_union (T{elt=v,left=l,right=r,...},s2) = 
          let val l2 = split_lt(s2,v)
              val r2 = split_gt(s2,v)
          in
            concat3(old_union(l,l2),v,old_union(r,r2))
          end

    val empty = E
    fun singleton x = T{elt=x,cnt=1,left=E,right=E}

    fun addList (s,l) = List.foldl (fn (i,s) => add(s,i)) s l

    val add = add

    fun member (set, x) = let
          fun pk E = false
            | pk (T{elt=v, left=l, right=r, ...}) = (
                case K.compare(x,v)
                 of LESS => pk l
                  | EQUAL => true
                  | GREATER => pk r
                (* end case *))
          in
            pk set
          end

    local
        (* true if every item in t is in t' *)
      fun treeIn (t,t') = let
            fun isIn E = true
              | isIn (T{elt,left=E,right=E,...}) = member(t',elt)
              | isIn (T{elt,left,right=E,...}) = 
                  member(t',elt) andalso isIn left
              | isIn (T{elt,left=E,right,...}) = 
                  member(t',elt) andalso isIn right
              | isIn (T{elt,left,right,...}) = 
                  member(t',elt) andalso isIn left andalso isIn right
            in
              isIn t
            end
    in
    fun isSubset (E,_) = true
      | isSubset (_,E) = false
      | isSubset (t as T{cnt=n,...},t' as T{cnt=n',...}) =
          (n<=n') andalso treeIn (t,t')

    fun equal (E,E) = true
      | equal (t as T{cnt=n,...},t' as T{cnt=n',...}) =
          (n=n') andalso treeIn (t,t')
      | equal _ = false
    end

    local
      fun next ((t as T{right, ...})::rest) = (t, left(right, rest))
        | next _ = (E, [])
      and left (E, rest) = rest
        | left (t as T{left=l, ...}, rest) = left(l, t::rest)
    in
    fun compare (s1, s2) = let
          fun cmp (t1, t2) = (case (next t1, next t2)
                 of ((E, _), (E, _)) => EQUAL
                  | ((E, _), _) => LESS
                  | (_, (E, _)) => GREATER
                  | ((T{elt=e1, ...}, r1), (T{elt=e2, ...}, r2)) => (
                      case Key.compare(e1, e2)
                       of EQUAL => cmp (r1, r2)
                        | order => order
                      (* end case *))
                (* end case *))
          in
            cmp (left(s1, []), left(s2, []))
          end
    end

    fun delete (E,x) = raise LibBase.NotFound
      | delete (set as T{elt=v,left=l,right=r,...},x) =
          case K.compare(x,v) of
            LESS => T'(v,delete(l,x),r)
          | GREATER => T'(v,l,delete(r,x))
          | _ => delete'(l,r)

    val union = hedge_union

    fun intersection (E, _) = E
      | intersection (_, E) = E
      | intersection (s, T{elt=v,left=l,right=r,...}) = let
          val l2 = split_lt(s,v)
          val r2 = split_gt(s,v)
          in
            if member(s,v)
              then concat3(intersection(l2,l),v,intersection(r2,r))
              else concat(intersection(l2,l),intersection(r2,r))
          end

    fun difference (E,s) = E
      | difference (s,E)  = s
      | difference (s, T{elt=v,left=l,right=r,...}) =
          let val l2 = split_lt(s,v)
              val r2 = split_gt(s,v)
          in
            concat(difference(l2,l),difference(r2,r))
          end

    fun map f set = let
          fun map'(acc, E) = acc
            | map'(acc, T{elt,left,right,...}) =
                map' (add (map' (acc, left), f elt), right)
          in 
            map' (E, set)
          end

    fun app apf =
         let fun apply E = ()
               | apply (T{elt,left,right,...}) = 
                   (apply left;apf elt; apply right)
         in
           apply
         end

    fun foldl f b set = let
          fun foldf (E, b) = b
            | foldf (T{elt,left,right,...}, b) = 
                foldf (right, f(elt, foldf (left, b)))
          in
            foldf (set, b)
          end

    fun foldr f b set = let
          fun foldf (E, b) = b
            | foldf (T{elt,left,right,...}, b) = 
                foldf (left, f(elt, foldf (right, b)))
          in
            foldf (set, b)
          end

    fun listItems set = foldr (op::) [] set

    fun filter pred set =
          foldl (fn (item, s) => if (pred item) then add(s, item) else s)
            empty set

    fun find p E = NONE
      | find p (T{elt,left,right,...}) = (case find p left
           of NONE => if (p elt)
                then SOME elt
                else find p right
            | a => a
          (* end case *))

    fun exists p E = false
      | exists p (T{elt, left, right,...}) =
          (exists p left) orelse (p elt) orelse (exists p right)

  end (* BinarySetFn *)
(* stop of smlnj-lib/Util/binary-set-fn.sml *)
(* start of FIN_SET.sml *)
(*
 * Standard ML finite sets
 *
 * Definition, section 4.2
 *
 * Note:
 *     This signature just extends the one available in the SML/NJ lib.
 *     Actually, the operation added here would be general purpose and useful enough
 *     to be in the lib. Also see FIN_MAP.
 *)

signature FIN_SET =
  sig

    include ORD_SET

    val fromList:  item list -> set

  end
(* stop of FIN_SET.sml *)
(* start of FinSetFn.sml *)
(*
 * Standard ML finite sets
 *
 * Definition, section 4.2
 *
 * Note:
 *     This functor just extends the one available in the SML/NJ lib.
 *     Actually, the operation added here would be general purpose and useful enough
 *     to be in the lib. Also see FinMapFn.
 *)

functor FinSetFn(Key: ORD_KEY) :> FIN_SET where type Key.ord_key = Key.ord_key =
  struct

    structure BinarySet = BinarySetFn(Key)

    open BinarySet

    fun fromList xs = addList(empty, xs)

  end
(* stop of FinSetFn.sml *)
(* start of TYVAR.sml *)
(*
 * Standard ML type variables
 *
 * Definition, sections 2.4 and 4.1
 *)


signature TYVAR =
  sig

    (* Type [Sections 2.4 and 4.1]*)

    eqtype TyVar                        (* [alpha] or [tyvar] *)


    (* Operations *)

    val invent:         bool -> TyVar
    val fromIndex:      bool -> int -> TyVar
    val fromString:     string -> TyVar
    val toString:       TyVar -> string

    val admitsEquality: TyVar -> bool
    val isExplicit:     TyVar -> bool

    val instance:       TyVar -> TyVar
    val normalise:      TyVar * int -> TyVar

    val compare:        TyVar * TyVar -> order

  end
(* stop of TYVAR.sml *)
(* start of TyVar.sml *)
(*
 * Standard ML type variables
 *
 * Definition, sections 2.4 and 4.1
 *
 * Note:
 *  - Internally generated tyvars get names '#xxx, where xxx is a stamp number.
 *  - Tyvars generated from integers are mapped to 'a,'b,..,'z,'aa,'bb,..,'zz,
 *    'aaa,...
 *)


structure TyVar :> TYVAR =
  struct

    (* Type [Sections 2.4 and 4.1]*)

    type TyVar = { name: string, equality: bool }       (* [alpha] or [tyvar] *)


    (* Creation *)

    fun invent equality =
        { name="'#" ^ Stamp.toString(Stamp.stamp()),
          equality=equality }

    fun fromIndex equality n =
        let
            fun rep(0,c) = c
              | rep(n,c) = c ^ rep(n-1,c)

            val c    = String.str(Char.chr(Char.ord #"a" + n mod 26))
            val name = (if equality then "''" else "'") ^ rep(n div 26, c)
        in
            { name=name, equality=equality }
        end

    fun fromString s =
        { name = s,
          equality = String.size(s) > 1 andalso String.sub(s,1) = #"'" }

    fun toString{name,equality}       = name


    (* Attributes [Section 4.1] *)

    fun admitsEquality{name,equality} = equality

    fun isExplicit{name,equality} =
        String.size name = 1 orelse String.sub(name,1) <> #"#"


    (* Small helpers *)

    fun normalise({name,equality}, n) = fromIndex equality n

    fun instance{name,equality}       = invent equality


    (* Ordering *)

    fun compare(alpha1: TyVar, alpha2: TyVar) =
        String.compare(#name alpha1, #name alpha2)

  end
(* stop of TyVar.sml *)
(* start of TYNAME.sml *)
(*
 * Standard ML type names
 *
 * Definition, section 4.1
 *
 * Notes:
 * - Equality is not a boolean attribute. We distinguish a 3rd kind of special
 *   type names which have equality regardless of the types applied. This
 *   implements ref, array, and equivalent types.
 * - For easy checking of pattern exhaustiveness we add an attribute
 *   `span' counting the number of constructors of the type.
 *)


signature TYNAME =
  sig

    (* Import *)

    type TyCon = TyCon.Id


    (* Type [Section 4.1] *)

    eqtype TyName                                       (* [t] *)

    datatype Equality = NOEQ | EQ | SPECIALEQ


    (* Operations *)

    val tyname:         TyCon * int * Equality * int -> TyName
    val invent:         int * Equality -> TyName
    val rename:         TyName -> TyName
    val removeEquality: TyName -> TyName
    val Abs:            TyName -> TyName

    val arity:          TyName -> int
    val equality:       TyName -> Equality
    val span:           TyName -> int
    val tycon:          TyName -> TyCon
    val toString:       TyName -> string

    val compare:        TyName * TyName -> order

  end
(* stop of TYNAME.sml *)
(* start of TyName.sml *)
(*
 * Standard ML type names
 *
 * Definition, section 4.1
 *
 * Notes:
 * - Equality is not a boolean attribute. We distinguish a 3rd kind of special
 *   type names which have equality regardless of the types applied. This
 *   implements ref, array, and equivalent types.
 * - For easy checking of pattern exhaustiveness we add an attribute
 *   `span' counting the number of constructors of the type.
 *)


structure TyName :> TYNAME =
  struct

    (* Import *)

    type TyCon = TyCon.Id
    type stamp = Stamp.stamp


    (* Type [Section 4.1] *)

    datatype Equality = NOEQ | EQ | SPECIALEQ

    type TyName =                                     (* [t] *)
         { tycon:       TyCon
         , stamp:       stamp
         , arity:       int
         , equality:    Equality
         , span:        int
         }


    (* Creation *)

    fun tyname(tycon, arity, equality, span) =
        { tycon    = tycon
        , stamp    = Stamp.stamp()
        , arity    = arity
        , equality = equality
        , span     = span
        }

    fun invent(arity, equality) = tyname(TyCon.invent(), arity, equality, 0)


    (* Creation from existing *)

    fun rename{tycon, stamp, arity, equality, span} =
            tyname(tycon, arity, equality, span)

    fun removeEquality{tycon, stamp, arity, equality, span} =
            tyname(tycon, arity, NOEQ, span)

    fun Abs{tycon, stamp, arity, equality, span} =
            tyname(tycon, arity, NOEQ, 0)


    (* Attributes [Section 4.1] *)

    fun arity   {tycon, stamp, arity, equality, span} = arity
    fun equality{tycon, stamp, arity, equality, span} = equality
    fun span    {tycon, stamp, arity, equality, span} = span
    fun tycon   {tycon, stamp, arity, equality, span} = tycon

    fun toString{tycon, stamp, arity, equality, span} = TyCon.toString tycon


    (* Ordering *)

    fun compare(t1: TyName, t2: TyName) = Stamp.compare(#stamp t1, #stamp t2)

  end
(* stop of TyName.sml *)
(* start of SCON.sml *)
(*
 * Standard ML special constants
 *
 * Definition, section 2.2
 *)


signature SCON =
  sig

    (* Type [Section 2.2] *)

    datatype SCon =                             (* [scon] *)
          INT    of int
        | WORD   of word
        | STRING of string
        | CHAR   of char
        | REAL   of real

    (* Operations *)

    val fromInt:    int    -> SCon
    val fromWord:   word   -> SCon
    val fromString: string -> SCon
    val fromChar:   char   -> SCon
    val fromReal:   real   -> SCon

    val toString:   SCon   -> string

    val compare:    SCon * SCon -> order

  end
(* stop of SCON.sml *)
(* start of SCon.sml *)
(*
 * Standard ML special constants
 *
 * Definition, section 2.2
 *)


structure SCon :> SCON =
  struct

    (* Type [Section 2.2] *)

    datatype SCon =                             (* [scon] *)
          INT    of int
        | WORD   of word
        | STRING of string
        | CHAR   of char
        | REAL   of real


    (* Conversions *)

    val fromInt    = INT
    val fromWord   = WORD
    val fromString = STRING
    val fromChar   = CHAR
    val fromReal   = REAL

    fun toString(INT i)    = Int.toString i
      | toString(WORD w)   = "0wx" ^ Word.toString w
      | toString(STRING s) = "\""  ^ String.toCString s ^ "\""
      | toString(CHAR c)   = "\"#" ^ Char.toCString c   ^ "\""
      | toString(REAL r)   = Real.toString r


    (* Ordering *)

    fun compare(INT n1,    INT n2)    = Int.compare(n1, n2)
      | compare(WORD w1,   WORD w2)   = Word.compare(w1, w2)
      | compare(STRING s1, STRING s2) = String.compare(s1, s2)
      | compare(CHAR c1,   CHAR c2)   = Char.compare(c1, c2)
      | compare(REAL x1,   REAL x2)   = Real.compare(x1, x2)
      | compare  _                    = raise Domain

  end
(* stop of SCon.sml *)
(* start of LAB.sml *)
(*
 * Standard ML label identifiers
 *
 * Definition, section 2.4
 *)


signature LAB =
  sig

    (* Type [Section 2.4] *)

    eqtype Lab                                  (* [lab] *)


    (* Operations *)

    val fromString:     string -> Lab
    val fromInt:        int    -> Lab
    val toString:       Lab    -> string

    val compare:        Lab * Lab -> order

  end
(* stop of LAB.sml *)
(* start of Lab.sml *)
(*
 * Standard ML label identifiers
 *
 * Definition, section 2.4
 *)


structure Lab :> LAB =
  struct

    (* Type [Section 2.4] *)

    type Lab = string                                   (* [lab] *)


    (* Conversions *)

    fun fromString s = s
    val fromInt      = Int.toString
    fun toString s   = s


    (* Ordering *)

    fun compare(lab1,lab2) =
      case (Int.fromString lab1, Int.fromString lab2)
        of (SOME i1, SOME i2) => Int.compare(i1,i2)
         |     _              => String.compare(lab1,lab2)

  end
(* stop of Lab.sml *)
(* start of AssembliesCoreStatic.sml *)
(*
 * Standard ML sets and maps for the static semantics of the core
 *
 * Definition, section 4.2
 *)

structure TyVarSet   = FinSetFn(type ord_key = TyVar.TyVar
                                val  compare = TyVar.compare)

structure TyNameSet  = FinSetFn(type ord_key = TyName.TyName
                                val  compare = TyName.compare)

structure SConSet    = FinSetFn(type ord_key = SCon.SCon
                                val  compare = SCon.compare)

structure VIdSet     = FinSetFn(type ord_key = VId.Id
                                val  compare = VId.compare)

structure LongVIdSet = FinSetFn(type ord_key = LongVId.longId
                                val  compare = LongVId.compare)


structure LabMap     = FinMapFn(type ord_key = Lab.Lab
                                val  compare = Lab.compare)

structure VIdMap     = FinMapFn(type ord_key = VId.Id
                                val  compare = VId.compare)

structure TyConMap   = FinMapFn(type ord_key = TyCon.Id
                                val  compare = TyCon.compare)

structure TyVarMap   = FinMapFn(type ord_key = TyVar.TyVar
                                val  compare = TyVar.compare)

structure TyNameMap  = FinMapFn(type ord_key = TyName.TyName
                                val  compare = TyName.compare)

structure StrIdMap   = FinMapFn(type ord_key = StrId.Id
                                val  compare = StrId.compare)
(* stop of AssembliesCoreStatic.sml *)
(* start of OVERLOADINGCLASS.sml *)
(*
 * Standard ML overloading classes
 *
 * Definition, appendix E
 *
 * Note:
 *  Overloading -- and defaulting in particular -- is not well formalised in
 *  the Definition. We describe an overloading class as a pair (T,t) of a set
 *  of type names (like the definition does), plus the default type name t.
 *  For overloading to be sound some well-formedness properties have to be
 *  enforced for all existing overloading classes (T,t):
 *  (1) t elem T
 *  (2) Eq T = 0  \/  t admits equality
 *  (3) forall (T',t') . (  TT' = 0  \/  t = t' )
 *  where Eq T = { t elem T | t admits equality } and we write TT' for the
 *  T intersect T' and 0 for the empty set.
 *  The reason for (1) is obvious. (2) guarantees that we do not loose the
 *  default if we enforce equality. (3) ensures the same if we have to unify
 *  two overloading classes. (2) and (3) also allow the resulting set to become
 *  empty which will cause a type error.
 *)

signature OVERLOADINGCLASS =
  sig

    (* Import types *)

    type TyName    = TyName.TyName
    type TyNameSet = TyNameSet.set


    (* Type *)

    type OverloadingClass                               (* [O] *)


    (* Operations *)

    val make:           TyNameSet * TyName -> OverloadingClass

    val isEmpty:        OverloadingClass -> bool
    val isSingular:     OverloadingClass -> bool
    val default:        OverloadingClass -> TyName
    val set:            OverloadingClass -> TyNameSet
    val member:         OverloadingClass * TyName -> bool
    val getItem:        OverloadingClass -> TyName

    val makeEquality:   OverloadingClass -> OverloadingClass option
    val intersection:   OverloadingClass * OverloadingClass ->
                                           OverloadingClass option
    val union:          OverloadingClass * OverloadingClass ->
                                           OverloadingClass

  end
(* stop of OVERLOADINGCLASS.sml *)
(* start of OverloadingClass.sml *)
(*
 * Standard ML overloading classes
 *
 * Definition, appendix E
 *
 * Note:
 *  Overloading -- and defaulting in particular -- is not well formalised in
 *  the Definition. We describe an overloading class as a pair (T,t) of a set
 *  of type names (like the definition does), plus the default type name t.
 *  For overloading to be sound some well-formedness properties have to be
 *  enforced for all existing overloading classes (T,t):
 *  (1) t elem T
 *  (2) Eq T = 0  \/  t admits equality
 *  (3) forall (T',t') . (  TT' = 0  \/  t = t' )
 *  where Eq T = { t elem T | t admits equality } and we write TT' for the
 *  T intersect T' and 0 for the empty set.
 *  The reason for (1) is obvious. (2) guarantees that we do not loose the
 *  default if we enforce equality. (3) ensures the same if we have to unify
 *  two overloading classes. (2) and (3) also allow the resulting set to become
 *  empty which will cause a type error.
 *)

structure OverloadingClass :> OVERLOADINGCLASS =
  struct

    (* Import types *)

    type TyName    = TyName.TyName
    type TyNameSet = TyNameSet.set


    (* Type *)

    type OverloadingClass = TyNameSet * TyName          (* [O] *)


    (* Simple operations *)

    fun make O            = O

    fun isEmpty (T,t)     = TyNameSet.isEmpty T
    fun isSingular (T,t)  = TyNameSet.numItems T = 1
    fun default (T,t)     = t
    fun set (T,t)         = T
    fun member((T,t), t') = TyNameSet.member(T, t')
    fun getItem (T,t)     = valOf(TyNameSet.find (fn _ => true) T)


    (* Filter equality types *)

    fun makeEquality (T,t) =
        let
            val T' = TyNameSet.filter (fn t => TyName.equality t = TyName.EQ) T
        in
            if TyNameSet.isEmpty T' then
                NONE
            else if TyName.equality t <> TyName.NOEQ then
                SOME(T',t)
            else
                raise Fail "OverloadingClass.makeEquality: \
                           \inconsistent overloading classes"
        end


    (* Intersection and union *)

    fun intersection((T1,t1), (T2,t2)) =
        let
            val T' = TyNameSet.intersection(T1,T2)
        in
            if TyNameSet.isEmpty T' then
                NONE
            else if t1 = t2 then
                SOME(T',t1)
            else
                raise Fail "OverloadingClass.intersect: \
                           \inconsistent overloading classes"
        end


    fun union((T1,t1), (T2,t2)) = ( TyNameSet.union(T1,T2), t2 )

  end
(* stop of OverloadingClass.sml *)
(* start of TYPE.sml *)
(*
 * Standard ML types
 *
 * Definition, section 4.2 and 4.4
 *
 * Notes:
 *   - Types are references so that unification can work via side effects.
 *     We need links (forwards) to unify two type variables.
 *   - Types built bottom-up have to be `normalised' to induce the required
 *     sharing on type variables.
 *   - Care has to be taken to clone types at the proper places.
 *   - Substitution creates a clone, but shares free type variables.
 *   - To represent overloaded type (variables), we add a special type.
 *   - Record types may contain a row variable to represent open record types
 *     (which appear during type inference). Flexible rows have to carry an
 *     equality flag to properly propagate equality enforced by unification
 *     when extending a row.
 *)

signature TYPE =
  sig

    (* Import types *)

    type Lab              = Lab.Lab
    type TyVar            = TyVar.TyVar
    type TyVarSet         = TyVarSet.set
    type TyName           = TyName.TyName
    type TyNameSet        = TyNameSet.set
    type OverloadingClass = OverloadingClass.OverloadingClass
    type 'a LabMap        = 'a LabMap.map
    type 'a TyVarMap      = 'a TyVarMap.map
    type 'a TyNameMap     = 'a TyNameMap.map
 

    (* Types [Section 4.2] *)

    datatype RowVar = CLOSEDRow | FLEXRow of bool               (* [r] *)

    datatype Type' =                                            (* [tau] *)
          TyVar      of TyVar
        | RowType    of (*RowType*) (Type' ref LabMap * RowVar)
        | FunType    of (*FunType*) (Type' ref * Type' ref)
        | ConsType   of (*ConsType*)(Type' ref list * TyName)
        | Overloaded of OverloadingClass
        | Link       of (*Type*) Type' ref

    type Type         = Type' ref

    type RowType      = Type LabMap * RowVar                    (* [rho] *)
    type FunType      = Type * Type
    type ConsType     = Type list * TyName

    type TypeFcn      = TyVar list * Type                       (* [theta] *)

    type Substitution = Type TyVarMap                           (* [mu] *)
    type Realisation  = TypeFcn TyNameMap                       (* [phi] *)


    (* Operations *)

    val invent:                 unit -> Type
    val fromTyVar:              TyVar -> Type
    val fromRowType:            RowType -> Type
    val fromFunType:            FunType -> Type
    val fromConsType:           ConsType -> Type
    val fromOverloadingClass:   OverloadingClass -> Type

    val range:                  Type -> Type
    val tyname:                 Type -> TyName

    val normalise:              Type -> Type
    val substitute:             Substitution -> Type -> Type
    val realise:                Realisation  -> Type -> Type

    val tyvars:                 Type -> TyVarSet
    val tynames:                Type -> TyNameSet
    val admitsEquality:         Type -> bool
    val isFlexible:             Type -> bool

    exception Unify
    val unify:                  Type * Type -> unit (* Unify *)
    val unifyRestricted:        TyVarSet -> Type * Type -> unit (* Unify *)
    val makeEquality:           Type -> unit (* Unify *)

    val defaultOverloaded:      Type -> unit


    (* Operations on rows *)

    val emptyRho:               RowType
    val singletonRho:           Lab * Type -> RowType
    val insertRho:              RowType * Lab * Type -> RowType
    val inventRho:              unit -> RowType
    val findLab:                RowType * Lab -> Type option

  end
(* stop of TYPE.sml *)
(* start of Type.sml *)
(*
 * Standard ML types
 *
 * Definition, section 4.2 and 4.4
 *
 * Notes:
 *   - Types are references so that unification can work via side effects.
 *     We need links (forwards) to unify two type variables.
 *   - Types built bottom-up have to be `normalised' to induce the required
 *     sharing on type variables.
 *   - Care has to be taken to clone types at the proper places.
 *   - Substitution creates a clone, but shares free type variables.
 *   - To represent overloaded type (variables), we add a special type.
 *   - Record types may contain a row variable to represent open record types
 *     (which appear during type inference). Flexible rows have to carry an
 *     equality flag to properly propagate equality enforced by unification
 *     when extending a row.
 *)

structure Type :> TYPE =
  struct

    (* Import types *)

    type Lab              = Lab.Lab
    type TyVar            = TyVar.TyVar
    type TyVarSet         = TyVarSet.set
    type TyName           = TyName.TyName
    type TyNameSet        = TyNameSet.set
    type OverloadingClass = OverloadingClass.OverloadingClass
    type 'a LabMap        = 'a LabMap.map
    type 'a TyVarMap      = 'a TyVarMap.map
    type 'a TyNameMap     = 'a TyNameMap.map


    (* Types [Section 4.2] *)

    datatype RowVar = CLOSEDRow | FLEXRow of bool       (* [r] *)

    datatype Type' =                                    (* [tau] *)
          TyVar      of TyVar
        | RowType    of RowType
        | FunType    of FunType
        | ConsType   of ConsType
        | Overloaded of OverloadingClass
        | Link       of Type

    withtype Type     = Type' ref

    and RowType       = Type' ref LabMap * RowVar       (* [rho] *)
    and FunType       = Type' ref * Type' ref
    and ConsType      = Type' ref list * TyName

    type TypeFcn      = TyVar list * Type               (* [theta] *)

    type Substitution = Type TyVarMap                   (* [mu] *)
    type Realisation  = TypeFcn TyNameMap               (* [phi] *)


    (* Creation *)

    fun invent() = ref(TyVar(TyVar.invent false))

    fun fromTyVar alpha        = ref(TyVar alpha)
    fun fromRowType rho        = ref(RowType rho)
    fun fromFunType x          = ref(FunType x)
    fun fromConsType x         = ref(ConsType x)
    fun fromOverloadingClass O = ref(Overloaded O)


    (* Projections *)

    fun range(ref(FunType(tau1,tau2))) = tau2
      | range tau                      = tau

    fun tyname(ref(ConsType(taus,t)))  = t
      | tyname  _                      =
            raise Fail "Type.tyname: non-constructed type"


    (* Induce sharing on equal type variables in a type *)

    fun normalise tau =
        let
            (* Note that Overloaded nodes also have to be shared.
             * But since such types are always pre-built rather than
             * infered, we just take care that we construct them with
             * proper sharing and ignore Overloaded nodes here.
             *)

            val alphas = ref []

            fun normalise(tau as ref(TyVar(alpha))) =
                (case List.find (fn(alpha1,_) => alpha1 = alpha) (!alphas)
                   of SOME(_,tau1) => tau1
                    | NONE         => ( alphas := (alpha,tau) :: !alphas
                                      ; tau
                                      )
                )
              | normalise(ref(Link(tau)))  = normalise tau
              | normalise(tau as ref tau') = ( tau := normalise' tau' ; tau )

            and normalise'(RowType(Rho,r)) =
                    RowType(LabMap.map normalise Rho, r)

              | normalise'(FunType(tau1,tau2)) =
                    FunType(normalise tau1, normalise tau2)

              | normalise'(ConsType(taus,t)) =
                    ConsType(List.map normalise taus, t)

              | normalise'(Overloaded(O)) =
                    Overloaded(O)

              | normalise' _ =
                    raise Fail "Type.normalise: bypassed type variable or link"
        in
            normalise tau
        end



    (* Cloning under a substitution and a type realisation *)

    fun clone (mu,phi) tau =
        let
            (* Cloning must respect sharing, so an association list is used
             * to remember nodes already visited together with their copy.
             *)

            val mu'    = ref mu
            val cloned = ref []

            fun clone tau =
                case List.find (fn(tau1,_) => tau1 = tau) (!cloned)
                  of SOME(_,tau2) => tau2
                   | NONE         => let val tau2 = clone' tau in
                                         cloned := (tau,tau2) :: !cloned
                                        ; tau2
                                     end

            and clone'(tau as ref(TyVar(alpha))) =
                (case TyVarMap.find(!mu', alpha)
                   of NONE     => tau
                    | SOME tau => tau
                )
              | clone'(ref(RowType(Rho,r))) =
                    ref(RowType(LabMap.map clone Rho, r))

              | clone'(ref(FunType(tau1,tau2))) =
                    ref(FunType(clone tau1, clone tau2))

              | clone'(tau as ref(ConsType(taus,t))) =
                let
                    val taus2 = List.map clone taus
                in
                    case TyNameMap.find(phi, t)
                      of NONE              => ref(ConsType(taus2,t))
                       | SOME(alphas,tau1) =>
                         let
                            val cloned' = !cloned
                         in
                            mu' := ListPair.foldl
                                    (fn(alpha,tau2,mu) =>
                                        TyVarMap.insert(mu,alpha,tau2))
                                    (!mu') (alphas,taus2)
                            ; clone' tau1
                            before cloned := cloned'
                         end
                end

              | clone'(ref(Overloaded(O))) =
                    ref(Overloaded(O))

              | clone'(ref(Link(tau))) =
                    clone tau
        in
            clone tau
        end


    (* Substitution, and realisation [Section 5.2] *)

    fun substitute mu = clone(mu,TyNameMap.empty)
    fun realise phi   = clone(TyVarMap.empty,phi)


    (* Type variable and type name extraction [Section 4.2] *)

    fun tyvars(ref tau') = tyvars' tau'

    and tyvars'(TyVar(alpha)) = TyVarSet.singleton alpha

      | tyvars'(RowType(Rho,r)) =
            LabMap.foldl (fn(tau,U) => TyVarSet.union(U, tyvars tau))
                         TyVarSet.empty Rho

      | tyvars'(FunType(tau1,tau2)) =
            TyVarSet.union(tyvars tau1, tyvars tau2)
 
      | tyvars'(ConsType(taus,t)) =
            List.foldl (fn(tau,U) => TyVarSet.union(U, tyvars tau))
                       TyVarSet.empty taus

      | tyvars'(Overloaded(O)) =
            TyVarSet.empty

      | tyvars'(Link(tau)) =
            tyvars tau


    fun tynames(ref tau') = tynames' tau'

    and tynames'(TyVar(alpha)) = TyNameSet.empty

      | tynames'(RowType(Rho,r)) =
            LabMap.foldl (fn(tau,T) =>
                          TyNameSet.union(T, tynames tau)) TyNameSet.empty Rho

      | tynames'(FunType(tau1,tau2)) =
            TyNameSet.union(tynames tau1, tynames tau2)
 
      | tynames'(ConsType(taus,t)) =
        let
            val T = List.foldl (fn(tau,T) => TyNameSet.union(T, tynames tau))
                               TyNameSet.empty taus
        in
            TyNameSet.add(T, t)
        end

      | tynames'(Overloaded(O)) =
            (* Conservative approximation *)
            OverloadingClass.set O

      | tynames'(Link(tau)) =
            tynames tau



    (* Check for equality type [Section 4.4] *)

    fun admitsEquality(ref tau') = admitsEquality' tau'

    and admitsEquality'(TyVar alpha) =
            TyVar.admitsEquality alpha orelse
            not(TyVar.isExplicit alpha)

      | admitsEquality'(RowType(Rho,CLOSEDRow)) =
            LabMap.all admitsEquality Rho

      | admitsEquality'(RowType(Rho,FLEXRow _)) =
            raise Fail "Type.admitsEquality: flexible row"

      | admitsEquality'(FunType _) = false

      | admitsEquality'(ConsType(taus,t)) =
        (case TyName.equality t
           of TyName.SPECIALEQ => true
            | TyName.EQ        => List.all admitsEquality taus
            | TyName.NOEQ      => false
        )

      | admitsEquality'(Overloaded(O)) =
            raise Fail "Type.admitsEquality: overloaded type"

      | admitsEquality'(Link(tau)) =
            admitsEquality tau



    (* Look for flexible records *)

    fun isFlexible(ref tau') = isFlexible' tau'

    and isFlexible'(TyVar(alpha')) = false

      | isFlexible'(RowType(Rho,r)) =
            r <> CLOSEDRow orelse LabMap.exists isFlexible Rho

      | isFlexible'(FunType(tau1,tau2)) =
            isFlexible tau1 orelse isFlexible tau2

      | isFlexible'(ConsType(taus,t)) =
            List.exists isFlexible taus

      | isFlexible'(Overloaded(O)) = false

      | isFlexible'(Link(tau)) = isFlexible tau



    (* Unification *)

    exception Unify


    fun occurs(alpha, ref tau') = occurs'(alpha, tau')

    and occurs'(alpha, TyVar(alpha')) =
            alpha = alpha'

      | occurs'(alpha, RowType(Rho,r)) =
            LabMap.exists (fn tau => occurs(alpha, tau)) Rho

      | occurs'(alpha, FunType(tau1,tau2)) =
            occurs(alpha, tau1) orelse occurs(alpha, tau2)

      | occurs'(alpha, ConsType(taus,t)) =
            List.exists (fn tau => occurs(alpha, tau)) taus

      | occurs'(alpha, Overloaded(O)) =
            false

      | occurs'(alpha, Link(tau)) =
            occurs(alpha, tau)


    fun unify(ref(Link(tau1)), tau2) = unify(tau1, tau2)
      | unify(tau1, ref(Link(tau2))) = unify(tau1, tau2)

      | unify(tau1 as ref tau1', tau2 as ref tau2') =
        if tau1 = tau2 then () else
        let
            val tau' = Link(ref(unify'(tau1',tau2')))
        in
            tau1 := tau' ; tau2 := tau'
        end

    and unify'(TyVar(alpha), tau')  = unifyTyVar(alpha, tau')
      | unify'(tau', TyVar(alpha))  = unifyTyVar(alpha, tau')
      | unify'(Overloaded(O), tau') = unifyOverloaded(O, tau')
      | unify'(tau', Overloaded(O)) = unifyOverloaded(O, tau')

      | unify'(tau' as FunType(tau11,tau12), FunType(tau21,tau22)) =
          ( unify(tau11,tau21)
          ; unify(tau12,tau22)
          ; tau'
          )

      | unify'(RowType(Rho1,r1), RowType(Rho2,r2)) =
        let
            fun unifyField r (lab, tau1, Rho) =
                case LabMap.find(Rho, lab)
                  of SOME tau2  => ( unify(tau1,tau2)
                                   ; #1(LabMap.remove(Rho,lab))
                                   )
                   | NONE =>
                case r
                  of CLOSEDRow  => raise Unify
                   | FLEXRow eq => ( if eq then makeEquality tau1 else ()
                                   ; Rho
                                   )

            val Rho1'  = LabMap.foldli (unifyField r1) Rho1 Rho2
            val _      = LabMap.foldli (unifyField r2) Rho2 Rho1'
            val r      = case (r1,r2)
                           of (CLOSEDRow, _) => CLOSEDRow
                            | (_, CLOSEDRow) => CLOSEDRow
                            | (FLEXRow eq1, FLEXRow eq2) =>
                                  FLEXRow(eq1 orelse eq2)
        in
            RowType(LabMap.unionWith #2 (Rho2,Rho1'), r)
        end


      | unify'(tau' as ConsType(taus1,t1), ConsType(taus2,t2)) =
        if t1 = t2 then
            ( ListPair.app unify (taus1,taus2)
            ; tau'
            )
        else
            raise Unify

      | unify' _ = raise Unify


    and unifyTyVar(alpha1, TyVar(alpha2)) =
        if alpha1 = alpha2 then
            TyVar(alpha2)
        else if not(TyVar.isExplicit alpha1) then
            bindTyVar(alpha1, TyVar(alpha2))
        else if not(TyVar.isExplicit alpha2) then
            bindTyVar(alpha2, TyVar(alpha1))
        else
            raise Unify

      | unifyTyVar(alpha, tau') =
        if TyVar.isExplicit alpha orelse occurs'(alpha, tau') then
            raise Unify
        else
            bindTyVar(alpha, tau')

    and bindTyVar(alpha, tau') =
        if TyVar.admitsEquality alpha then
            makeEquality' tau'
        else
            tau'


    and unifyOverloaded(O, TyVar(alpha2)) =
            unifyTyVar(alpha2, Overloaded(O))

      | unifyOverloaded(O, tau' as ConsType([],t)) =
        if OverloadingClass.member(O, t) then
            tau'
        else
            raise Unify

      | unifyOverloaded(O1, Overloaded(O2)) =
        (case OverloadingClass.intersection(O1,O2)
           of NONE   => raise Unify
            | SOME O => Overloaded(O)
        )

      | unifyOverloaded(O, _) =
            raise Unify


    and makeEquality(tau as ref tau') = tau := makeEquality' tau'

    and makeEquality'(TyVar(alpha)) =
        if TyVar.admitsEquality alpha then
            TyVar(alpha)
        else if TyVar.isExplicit alpha then
            raise Unify
        else
            TyVar(TyVar.invent true)

      | makeEquality'(RowType(Rho,r)) =
            ( LabMap.app makeEquality Rho
            ; RowType(Rho, case r of CLOSEDRow => CLOSEDRow
                                   | FLEXRow _ => FLEXRow true)
            )

      | makeEquality'(FunType _) =
            raise Unify

      | makeEquality'(tau' as ConsType(taus,t)) =
        (case TyName.equality t
           of TyName.SPECIALEQ => tau'
            | TyName.EQ        => ( List.app makeEquality taus ; tau' )
            | TyName.NOEQ      => raise Unify
        )

      | makeEquality'(Overloaded(O)) =
        (case OverloadingClass.makeEquality O
           of NONE    => raise Unify
            | SOME O' => Overloaded(O')
        )
      | makeEquality'(Link(tau)) =
            ( makeEquality tau ; Link(tau) )



    fun unifyRestricted U (tau1,tau2) =
        let
            fun skolemise(alpha, mu) =
                let
                   val equality = if TyVar.admitsEquality alpha then TyName.EQ
                                                                else TyName.NOEQ
                   val tau'     = ConsType([], TyName.invent(0,equality))
                in
                   TyVarMap.insert(mu, alpha, ref tau')
                end

            val mu = TyVarSet.foldl skolemise TyVarMap.empty U
        in
            unify(substitute mu tau1, substitute mu tau2)
        end



    (* Assign default type to overloaded type components [Appendix E] *)

    fun defaultOverloaded(tau as ref(Overloaded(O))) =
            tau := ConsType([], OverloadingClass.default O)

      | defaultOverloaded(ref tau') = defaultOverloaded' tau'

    and defaultOverloaded'(TyVar(alpha')) = ()

      | defaultOverloaded'(RowType(Rho,r)) =
            LabMap.app defaultOverloaded Rho

      | defaultOverloaded'(FunType(tau1,tau2)) =
            ( defaultOverloaded tau1 ; defaultOverloaded tau2 )

      | defaultOverloaded'(ConsType(taus,t)) =
            List.app defaultOverloaded taus

      | defaultOverloaded'(Overloaded(O)) =
            raise Fail "Type.defaultOverloaded: bypassed overloaded type"

      | defaultOverloaded'(Link(tau)) =
            defaultOverloaded tau



    (* Operations on rows *)

    val emptyRho                     = ( LabMap.empty, CLOSEDRow )
    fun singletonRho(lab,tau)        = ( LabMap.singleton(lab,tau), CLOSEDRow )
    fun inventRho()                  = ( LabMap.empty, FLEXRow false )
    fun insertRho((Rho,r), lab, tau) = ( LabMap.insert(Rho, lab, tau), r )
    fun findLab((Rho,r), lab)        = LabMap.find(Rho, lab)

  end
(* stop of Type.sml *)
(* start of TYPESCHEME.sml *)
(*
 * Standard ML type schemes
 *
 * Definition, section 4.2, 4.5, and 4.8
 *
 * Note:
 *    Instantiation copies a type (except free type variables).
 *    Closure does not!
 *)

signature TYPESCHEME =
  sig

    (* Import types *)

    type Type         = Type.Type
    type TyVar        = TyVar.TyVar
    type TyVarSet     = TyVarSet.set
    type TyName       = TyName.TyName
    type TyNameSet    = TyNameSet.set
    type Substitution = Type.Substitution
    type Realisation  = Type.Realisation
    type 'a TyNameMap = 'a TyNameMap.map


    (* Type [Section 4.2] *)

    type TypeScheme = TyVar list * Type                 (* [sigma] *)


    (* Operations *)

    val instance:       TypeScheme -> Type
    val instance':      TypeScheme -> TyVar list * Type
    val Clos:           Type -> TypeScheme
    val ClosRestricted: TyVarSet -> Type -> TypeScheme
    val isClosed:       TypeScheme -> bool

    val tyvars:         TypeScheme -> TyVarSet
    val tynames:        TypeScheme -> TyNameSet
    val normalise:      TypeScheme -> TypeScheme

    val generalises:    TypeScheme * TypeScheme -> bool
    val equals:         TypeScheme * TypeScheme -> bool

    val substitute:     Substitution -> TypeScheme -> TypeScheme
    val realise:        Realisation  -> TypeScheme -> TypeScheme

  end
(* stop of TYPESCHEME.sml *)
(* start of TypeScheme.sml *)
(*
 * Standard ML type schemes
 *
 * Definition, section 4.2, 4.5, and 4.8
 *
 * Note:
 *    Instantiation copies a type (except free type variables).
 *    Closure does not!
 *)

structure TypeScheme :> TYPESCHEME =
  struct

    (* Import types *)

    type Type         = Type.Type
    type TyVar        = TyVar.TyVar
    type TyVarSet     = TyVarSet.set
    type TyName       = TyName.TyName
    type TyNameSet    = TyNameSet.set
    type Substitution = Type.Substitution
    type Realisation  = Type.Realisation
    type 'a TyNameMap = 'a TyNameMap.map


    (* Type [Section 4.2] *)

    type TypeScheme = TyVar list * Type                 (* [sigma] *)


    (* Some helper (this should be in the library...) *)

    fun List_foldri f y0 xs =
        let
            fun fold(n,  [])   = y0
              | fold(n, x::xs) = f(n, x, fold(n+1,xs))
        in
            fold(0,xs)
        end



    (* Type variable and type name extraction [Section 4.2] *)

    fun tyvars (alphas,tau) =
        let
            val U = Type.tyvars tau
        in
            List.foldl
                (fn(alpha,U) => TyVarSet.delete(U,alpha)
                                handle LibBase.NotFound => U)
                U alphas
        end

    fun tynames (alphas,tau) = Type.tynames tau



    (* Instantiation *)

    fun instance' (alphas,tau) =
        let
            val alphas' = List.map TyVar.instance alphas
            val mu      = ListPair.foldl
                             (fn(alpha, alpha', mu) =>
                              TyVarMap.insert(mu, alpha, Type.fromTyVar alpha'))
                             TyVarMap.empty (alphas, alphas')
        in
            ( alphas', Type.substitute mu tau )
        end

    fun instance sigma = #2(instance' sigma)



    (* Generalisation [Section 4.5] *)

    fun generalisesType(sigma, tau) =
        let
            val U = Type.tyvars tau
        in
            ( Type.unifyRestricted U (instance sigma, tau) ; true )
            handle Type.Unify => false
        end

    fun generalises(sigma1, sigma2) =
            generalisesType(sigma1, instance sigma2)



    (* Closure [Section 4.8] *)

    fun Clos tau =
            (* Does not copy! *)
            ( TyVarSet.listItems(Type.tyvars tau), tau )

    fun ClosRestricted U tau =
            ( TyVarSet.listItems(TyVarSet.difference(Type.tyvars tau, U)), tau )

    fun isClosed (alphas,tau) =
        TyVarSet.isSubset(Type.tyvars tau, TyVarSet.fromList alphas)


    (* Comparison [Section 4.5] *)

    fun equals((alphas1,tau1), (alphas2,tau2)) =
        List.length alphas1 = List.length alphas2 andalso
        let
            fun insert(alpha1, alpha2, mu) =
                TyVarMap.insert(mu, alpha1, Type.fromTyVar alpha2)

            val (alphas2',tau2') = instance' (alphas2,tau2)
            val mu    = ListPair.foldl insert TyVarMap.empty (alphas1,alphas2')
            val tau1' = Type.substitute mu tau1
            val U     = TyVarSet.fromList alphas2'
        in
            ( Type.unifyRestricted U (tau1',tau2') ; true )
            handle Type.Unify => false
        end


    (* Normalisation (for output) *)

    fun normalise (alphas,tau) =
        let
            fun insert(n, alpha, (alphas',mu)) =
                let
                    val alpha' = TyVar.normalise(alpha, n)
                    val tau    = Type.fromTyVar alpha'
                in
                    ( alpha'::alphas', TyVarMap.insert(mu, alpha,tau) )
                end

            val (alphas',mu) = List_foldri insert (nil,TyVarMap.empty) alphas
        in
            ( alphas', Type.substitute mu tau )
        end


    (* Substitution *)

    fun substitute mu (alphas,tau) =
        let
            val mu' = List.foldl (fn(alpha,mu) =>
                                    #1(TyVarMap.remove(mu,alpha))
                                    handle LibBase.NotFound => mu) mu alphas
        in
            ( alphas, Type.substitute mu' tau )
        end


    (* Realisation [Section 5.2] *)

    fun realise phi (alphas,tau) = (alphas, Type.realise phi tau)

  end
(* stop of TypeScheme.sml *)
(* start of TYPEFCN.sml *)
(*
 * Standard ML type functions
 *
 * Definition, section 4.2, 4.4, and 4.8
 *
 * Note:
 *    Application copies the type (except free type variables).
 *)

signature TYPEFCN =
  sig

    (* Import types *)

    type Type         = Type.Type
    type TyVar        = TyVar.TyVar
    type TyVarSet     = TyVarSet.set
    type TyName       = TyName.TyName
    type TyNameSet    = TyNameSet.set
    type Realisation  = Type.TypeFcn TyNameMap.map


    (* Type [Section 4.2] *)

    type TypeFcn = Type.TypeFcn                 (* [theta] *)


    (* Operations *)

    val fromTyName:     TyName  -> TypeFcn
    val toTyName:       TypeFcn -> TyName option
    val isClosed:       TypeFcn -> bool

    val arity:          TypeFcn -> int
    val admitsEquality: TypeFcn -> bool

    val tyvars:         TypeFcn -> TyVarSet
    val tynames:        TypeFcn -> TyNameSet
    val normalise:      TypeFcn -> TypeFcn
    val rename:         TypeFcn -> TypeFcn

    val equals:         TypeFcn * TypeFcn -> bool

    exception Apply
    val apply:          Type list * TypeFcn -> Type     (* may raise Apply *)

    val realise:        Realisation -> TypeFcn -> TypeFcn

    val makeEquality:   TypeFcn -> unit

  end
(* stop of TYPEFCN.sml *)
(* start of TypeFcn.sml *)
(*
 * Standard ML type functions
 *
 * Definition, section 4.2, 4.4, and 4.8
 *
 * Note:
 *    Application copies the type (except free type variables).
 *)

structure TypeFcn :> TYPEFCN =
  struct

    (* Import types *)

    type Type         = Type.Type
    type TyVar        = TyVar.TyVar
    type TyVarSet     = TyVarSet.set
    type TyName       = TyName.TyName
    type TyNameSet    = TyNameSet.set
    type Realisation  = Type.TypeFcn TyNameMap.map


    (* Type [Section 4.2] *)

    type TypeFcn = Type.TypeFcn                         (* [theta] *)


    (* Operations *)

    val tyvars    = TypeScheme.tyvars           (* same type ;-) *)
    val tynames   = TypeScheme.tynames
    val equals    = TypeScheme.equals
    val isClosed  = TypeScheme.isClosed
    val realise   = TypeScheme.realise
    val rename    = TypeScheme.instance'
    val normalise = TypeScheme.normalise


    (* Arity [Section 4.4] *)

    fun arity (alphas,tau) = List.length alphas


    (* Equality [Section 4.4] *)

    fun admitsEquality (alphas,tau) =
        let
            fun insert(alpha, mu) =
                TyVarMap.insert(mu, alpha, Type.fromTyVar(TyVar.invent true))

            val mu = List.foldl insert TyVarMap.empty alphas
        in
            Type.admitsEquality(Type.substitute mu tau)
        end
    

    (* Eta-conversion [Section 4.4] *)

    fun fromTyName t =
        let
            val alphas = List.tabulate(TyName.arity t, TyVar.fromIndex false)
        in
            ( alphas, Type.fromConsType(List.map Type.fromTyVar alphas, t) )
        end

    fun toTyName(alphas, ref(Type.ConsType(taus,t))) = t
      | toTyName _ = raise Fail "TypeFcn.toTyName: invalid type function"

    fun toTyName(alphas, ref(Type.ConsType(taus,t))) =
        let
            fun isSame(alpha, ref(Type.TyVar alpha')) = alpha = alpha'
              | isSame(alpha,            _          ) = false
        in
            if List.length alphas = List.length taus
            andalso ListPair.all isSame (alphas, taus) then
                SOME t
            else
                NONE
        end

      | toTyName _ = NONE


    (* Application [Section 4.4] *)

    exception Apply

    fun apply(taus, (alphas,tau)) =
        if List.length taus <> List.length alphas then raise Apply else
        let
            fun insert(alpha, tau, mu) = TyVarMap.insert(mu, alpha, tau)
            val mu = ListPair.foldl insert TyVarMap.empty (alphas, taus)
        in
            Type.substitute mu tau
        end


    (* Make it an equality type *)

    fun makeEquality (alphas,tau) = Type.makeEquality tau

  end
(* stop of TypeFcn.sml *)
(* start of IDSTATUS.sml *)
(*
 * Standard ML identifier status
 *
 * Definition, sections 4.1 and 5.5
 *)


signature IDSTATUS =
  sig

    (* Type [Section 4.1] *)

    datatype IdStatus = c | e | v                       (* [is] *)


    (* Operations *)

    val generalises: IdStatus * IdStatus -> bool

  end
(* stop of IDSTATUS.sml *)
(* start of IdStatus.sml *)
(*
 * Standard ML identifier status
 *
 * Definition, sections 4.1 and 5.5
 *)


structure IdStatus :> IDSTATUS =
  struct

    (* Type [Section 4.1] *)

    datatype IdStatus = c | e | v                       (* [is] *)


    (* Generalisation [Section 5.5] *)

    fun generalises(is1,is2) = is1 = is2 orelse is2 = v

  end
(* stop of IdStatus.sml *)
(* start of GENERIC_ENV.sml *)
(*
 * Standard ML generic core environment
 *
 * Definition, sections 4.2, 4.3, 6.3 and 7.2
 *
 * Notes:
 *   - A datatype Str is necessary to break the recursion
 *     between Env and StrEnv.
 *   - Also, all types are parameterised over the range of value and type
 *     environments. This is because of the recursion between values and
 *     the dynamic environment (via function closures) -- we cannot make them
 *     into functor parameters as this would require recursive structures.
 *)

signature GENERIC_ENV =
  sig

    (* Import types *)

    type VId         = VId.Id
    type TyCon       = TyCon.Id
    type StrId       = StrId.Id
    type longVId     = LongVId.longId
    type longTyCon   = LongTyCon.longId
    type longStrId   = LongStrId.longId
    type IdStatus    = IdStatus.IdStatus

    type 'a VIdMap   = 'a VIdMap.map
    type 'a TyConMap = 'a TyConMap.map
    type 'a StrIdMap = 'a StrIdMap.map


    (* Export types [Section 4.2 and 6.3] *)

    datatype ('a,'b) Str' = Str of (*Env*)
                            ('a,'b) Str' StrIdMap * 'b TyConMap * 'a VIdMap

    type 'a ValEnv'       = 'a VIdMap
    type 'b TyEnv'        = 'b TyConMap
    type ('a,'b) StrEnv'  = ('a,'b) Str' StrIdMap

    type ('a,'b) Env'     = ('a,'b) StrEnv' * 'b TyEnv' * 'a ValEnv'


    (* Operations *)

    val empty:          ('a,'b) Env'

    val fromSE:         ('a,'b) StrEnv'         -> ('a,'b) Env'
    val fromTE:         'b TyEnv'               -> ('a,'b) Env'
    val fromVE:         'a ValEnv'              -> ('a,'b) Env'
    val fromVEandTE:    'a ValEnv' * 'b TyEnv'  -> ('a,'b) Env'

    val plus:           ('a,'b) Env' * ('a,'b) Env'    -> ('a,'b) Env'
    val plusVE:         ('a,'b) Env' * 'a ValEnv'      -> ('a,'b) Env'
    val plusTE:         ('a,'b) Env' * 'b TyEnv'       -> ('a,'b) Env'
    val plusSE:         ('a,'b) Env' * ('a,'b) StrEnv' -> ('a,'b) Env'
    val plusVEandTE:    ('a,'b) Env' * ('a ValEnv' * 'b TyEnv') -> ('a,'b) Env'

    val findVId:        ('a,'b) Env' * VId       -> 'a option
    val findTyCon:      ('a,'b) Env' * TyCon     -> 'b option
    val findStrId:      ('a,'b) Env' * StrId     -> ('a,'b) Str' option
    val findLongVId:    ('a,'b) Env' * longVId   -> 'a option
    val findLongTyCon:  ('a,'b) Env' * longTyCon -> 'b option
    val findLongStrId:  ('a,'b) Env' * longStrId -> ('a,'b) Str' option

    val disjoint:       ('a,'b) Env' * ('a,'b) Env' -> bool

  end
(* stop of GENERIC_ENV.sml *)
(* start of GenericEnvFn.sml *)
(*
 * Standard ML generic core environment
 *
 * Definition, sections 4.2, 4.3, 6.3 and 7.2
 *
 * Notes:
 *   - A datatype Str is necessary to break the recursion
 *     between Env and StrEnv.
 *   - Also, all types are parameterised over the range of value and type
 *     environments. This is because of the recursion between values and
 *     the dynamic environment (via function closures) -- we cannot make them
 *     into functor parameters as this would require recursive structures.
 *)

functor GenericEnvFn() :> GENERIC_ENV =
  struct

    (* Import types *)

    type VId         = VId.Id
    type TyCon       = TyCon.Id
    type StrId       = StrId.Id
    type longVId     = LongVId.longId
    type longTyCon   = LongTyCon.longId
    type longStrId   = LongStrId.longId
    type IdStatus    = IdStatus.IdStatus

    type 'a VIdMap   = 'a VIdMap.map
    type 'a TyConMap = 'a TyConMap.map
    type 'a StrIdMap = 'a StrIdMap.map


    (* Export types [Section 4.2 and 6.3] *)

    datatype ('a,'b) Str' = Str of (*Env*)
                           ('a,'b) Str' StrIdMap * 'b TyConMap * 'a VIdMap

    type 'a ValEnv'       = 'a VIdMap                           (* [VE] *)
    type 'b TyEnv'        = 'b TyConMap                         (* [TE] *)
    type ('a,'b) StrEnv'  = ('a,'b) Str' StrIdMap               (* [SE] *)

    type ('a,'b) Env'     = ('a,'b) StrEnv' * 'b TyEnv' * 'a ValEnv' (* [E] *)


    (* Injections [Section 4.3] *)

    val empty              = ( StrIdMap.empty, TyConMap.empty, VIdMap.empty )

    fun fromSE SE          = ( SE,             TyConMap.empty, VIdMap.empty )
    fun fromTE TE          = ( StrIdMap.empty, TE,             VIdMap.empty )
    fun fromVE VE          = ( StrIdMap.empty, TyConMap.empty, VE           )
    fun fromVEandTE(VE,TE) = ( StrIdMap.empty, TE,             VE           )


    (* Modifications [Section 4.3] *)

    infix plus plusVE plusTE plusSE plusVEandTE

    fun (SE,TE,VE) plus (SE',TE',VE') =
        ( StrIdMap.unionWith #2 (SE,SE')
        , TyConMap.unionWith #2 (TE,TE')
        , VIdMap.unionWith   #2 (VE,VE')
        )

    fun (SE,TE,VE) plusVE VE' = ( SE, TE, VIdMap.unionWith #2 (VE,VE') )
    fun (SE,TE,VE) plusTE TE' = ( SE, TyConMap.unionWith #2 (TE,TE'), VE )
    fun (SE,TE,VE) plusSE SE' = ( StrIdMap.unionWith #2 (SE,SE'), TE, VE )
    fun (SE,TE,VE) plusVEandTE (VE',TE') =
        ( SE
        , TyConMap.unionWith #2 (TE,TE')
        , VIdMap.unionWith   #2 (VE,VE')
        )


    (* Application (lookup) [Section 4.3] *)

    fun findVId  ((SE,TE,VE), vid)   = VIdMap.find(VE, vid)
    fun findTyCon((SE,TE,VE), tycon) = TyConMap.find(TE, tycon)
    fun findStrId((SE,TE,VE), strid) = StrIdMap.find(SE, strid)

    fun findLongX'(E, findX,      [],       x) = findX(E, x)
      | findLongX'(E, findX, strid::strids, x) =
            Option.mapPartial (fn E => findLongX'(E, findX, strids, x))
                              (Option.map (fn Str E => E) (findStrId(E, strid)))

    fun findLongX (explodeLongX, findX) (E, longX) =
        let
            val (strids,x) = explodeLongX longX
        in
            findLongX'(E, findX, strids, x)
        end

    fun findLongVId   x = findLongX (LongVId.explode,   findVId) x
    fun findLongTyCon x = findLongX (LongTyCon.explode, findTyCon) x
    fun findLongStrId x = findLongX (LongStrId.explode, findStrId) x


    (* Disjointness *)

    fun disjoint((SE1,TE1,VE1), (SE2,TE2,VE2)) =
            StrIdMap.disjoint(SE1,SE2) andalso
            TyConMap.disjoint(TE1,TE2) andalso
            VIdMap.disjoint(VE1,VE2)

  end
(* stop of GenericEnvFn.sml *)
(* start of STATIC_ENV.sml *)
(*
 * Standard ML environments of the static semantics of the core
 *
 * Definition, sections 4.2, 4.3, 4.8, 4.9, and 5.5
 *
 * Note:
 *     We call the domain type of value environments ValStr.
 *)

signature STATIC_ENV =
  sig

    (* Inheritance *)

    include GENERIC_ENV


    (* Import types *)

    type TypeScheme   = TypeScheme.TypeScheme
    type TypeFcn      = TypeFcn.TypeFcn
    type TyVar        = TyVar.TyVar
    type TyVarSet     = TyVarSet.set
    type TyName       = TyName.TyName
    type TyNameSet    = TyNameSet.set
    type Realisation  = Type.Realisation


    (* Export types [Section 4.2] *)

    type ValStr = TypeScheme * IdStatus
    type ValEnv = ValStr VIdMap                                 (* [VE] *)

    type TyStr  = TypeFcn * ValEnv
    type TyEnv  = TyStr TyConMap                                (* [TE] *)

    type Str    = (ValStr, TyStr) Str'
    type StrEnv = Str StrIdMap                                  (* [SE] *)
 
    type Env    = StrEnv * TyEnv * ValEnv                       (* [E] *)


    (* Operations *)

    val tyvarsVE:               ValEnv -> TyVarSet
    val tyvars:                 Env    -> TyVarSet
    val tynamesTE:              TyEnv  -> TyNameSet
    val tynamesSE:              StrEnv -> TyNameSet
    val tynames:                Env    -> TyNameSet

    val isWellFormed:           Env -> bool

    val Clos:                   ValEnv -> ValEnv
    val containsFlexibleType:   ValEnv -> bool
    val defaultOverloaded:      ValEnv -> unit
    val makeEquality:           TyEnv  -> unit
    val maximiseEquality:       TyEnv * ValEnv -> TyEnv * ValEnv
    val Abs:                    TyEnv * Env -> Env
    val realise:                Realisation -> Env -> Env

    val enriches:               Env * Env -> bool

  end
(* stop of STATIC_ENV.sml *)
(* start of StaticEnv.sml *)
(*
 * Standard ML environments of the static semantics of the core
 *
 * Definition, sections 4.2, 4.3, 4.8, 4.9, and 5.5
 *
 * Note:
 *     We call the domain type of value environments ValStr.
 *)

structure StaticEnv :> STATIC_ENV =
  struct

    (* Inheritance *)

    structure GenericEnv = GenericEnvFn()

    open GenericEnv


    (* Import types *)

    type TypeScheme   = TypeScheme.TypeScheme
    type TypeFcn      = TypeFcn.TypeFcn
    type TyVar        = TyVar.TyVar
    type TyVarSet     = TyVarSet.set
    type TyName       = TyName.TyName
    type TyNameSet    = TyNameSet.set
    type Realisation  = Type.Realisation


    (* Export types [Section 4.2] *)

    type ValStr = TypeScheme * IdStatus
    type ValEnv = ValStr VIdMap                                 (* [VE] *)

    type TyStr  = TypeFcn * ValEnv
    type TyEnv  = TyStr TyConMap                                (* [TE] *)

    type Str    = (ValStr, TyStr) Str'
    type StrEnv = Str StrIdMap                                  (* [SE] *)
 
    type Env    = StrEnv * TyEnv * ValEnv                       (* [E] *)


    (* Further modifications [Section 4.3] *)

    infix TEplus

    fun TE' TEplus (SE,TE,VE) = ( SE, TyConMap.unionWith #2 (TE',TE), VE )


    (* Type variable and type name set [Section 4.2] *)

    fun tyvarsVE VE =
        VIdMap.foldl
            (fn((sigma,is), U) => TyVarSet.union(U, TypeScheme.tyvars sigma))
            TyVarSet.empty VE

    fun tyvarsTE TE =
        TyConMap.foldl
            (fn((theta,VE), U) => TyVarSet.union(TyVarSet.union
                                (U, TypeFcn.tyvars theta), tyvarsVE VE))
            TyVarSet.empty TE

    fun tyvarsSE SE =
        StrIdMap.foldl
            (fn(Str E, U) => TyVarSet.union(U, tyvars E))
            TyVarSet.empty SE

    and tyvars (SE,TE,VE) =
        TyVarSet.union(TyVarSet.union(tyvarsSE SE, tyvarsTE TE), tyvarsVE VE)


    fun tynamesVE VE =
        VIdMap.foldl
            (fn((sigma,is), T) => TyNameSet.union(T, TypeScheme.tynames sigma))
            TyNameSet.empty VE

    fun tynamesTE TE =
        TyConMap.foldl
            (fn((theta,VE), T) => TyNameSet.union(TyNameSet.union
                                (T, TypeFcn.tynames theta), tynamesVE VE))
            TyNameSet.empty TE

    fun tynamesSE SE =
        StrIdMap.foldl
            (fn(Str E, T) => TyNameSet.union(T, tynames E))
            TyNameSet.empty SE

    and tynames (SE,TE,VE) =
        TyNameSet.union(TyNameSet.union(tynamesSE SE, tynamesTE TE),
                                        tynamesVE VE)


    (* Well-formedness [Section 4.9] *)

    fun isWellFormedTyStr (theta,VE) =
        VIdMap.isEmpty VE orelse isSome(TypeFcn.toTyName theta)

    fun isWellFormedTE TE =
        TyConMap.all isWellFormedTyStr TE

    fun isWellFormedSE SE =
        StrIdMap.all (fn Str E => isWellFormed E) SE

    and isWellFormed (SE,TE,VE) =
        isWellFormedTE TE andalso isWellFormedSE SE



    (* Closure [Section 4.8] *)

    fun Clos VE =
            VIdMap.map (fn((_,tau), is) => (TypeScheme.Clos tau, is)) VE


    (* Check for unresolved flexible record types [Section 4.11, item 1] *)

    fun containsFlexibleType VE =
            VIdMap.exists (fn((_,tau), is) => Type.isFlexible tau) VE


    (* Assign default types to overloaded types [Appendix E] *)

    fun defaultOverloaded VE =
            VIdMap.app (fn((_,tau), is) => Type.defaultOverloaded tau) VE


    (* Realisation [Section 5.2] *)

    fun realiseVE phi VE =
        VIdMap.map (fn(sigma,is) => ( TypeScheme.realise phi sigma, is )) VE

    and realiseTE phi TE =
        TyConMap.map (fn(theta,VE) => ( TypeFcn.realise phi theta
                                      , realiseVE phi VE
                                      )) TE
    and realiseSE phi SE =
        StrIdMap.map (fn(Str E) => Str(realise phi E)) SE

    and realise phi (SE,TE,VE) =
        ( realiseSE phi SE
        , realiseTE phi TE
        , realiseVE phi VE
        )


    (* Make all type names bound in a type environment equality types *)

    (* Assumes abstract types, i.e. no constructors. *)

    fun makeEquality TE =
        TyConMap.app (fn(theta,VE) => TypeFcn.makeEquality theta) TE



    (* Maximise equality of a type environment [Section 4.9],
     * together with its appendant value envrionment
     *)

    fun admitsEqualityValStr ((_,tau),_) = Type.admitsEquality tau

    fun maximiseEquality(TE,VE) =
        let
            fun checkTyStr((theta,VE), (phi,changed)) = 
                let
                    val t = valOf(TypeFcn.toTyName theta)
                in
                    if TyName.equality t = TyName.EQ
                    andalso not(VIdMap.all admitsEqualityValStr VE) then
                        ( TyNameMap.insert(phi,
                                           t,
                                           TypeFcn.fromTyName
                                                (TyName.removeEquality t)
                                          )
                        , true
                        )
                    else
                        ( phi, changed )
                end

            fun checkTE(TE, phi) =
                let
                    val (phi',change) = TyConMap.foldl checkTyStr (phi,false) TE
                    val TE'           = realiseTE phi' TE
                in
                    if change then checkTE(TE', phi')
                              else (TE', phi')
                end

            val (TE',phi) = checkTE(TE, TyNameMap.empty)
        in
            ( TE', realiseVE phi VE )
        end



    (* Abstraction of a type environment [Section 4.9] *)

    fun AbsTE(TE) = TyConMap.map (fn(theta,_) => (theta,VIdMap.empty)) TE

    fun Abs(TE,E) =
        let
            val ts  = tynamesTE TE
            val phi = TyNameSet.foldl
                        (fn(t,phi) => TyNameMap.insert(phi, t,
                                        TypeFcn.fromTyName(TyName.Abs t)))
                        TyNameMap.empty ts
        in
            realise phi (AbsTE(TE) TEplus E)
        end


    (* Disjointness *)

    fun disjoint((SE1,TE1,VE1), (SE2,TE2,VE2)) =
            StrIdMap.disjoint(SE1,SE2) andalso
            TyConMap.disjoint(TE1,TE2) andalso
            VIdMap.disjoint(VE1,VE2)


    (* Enrichment [Section 5.5] *)

    fun equalsVE(VE1,VE2) =
        VIdMap.numItems VE1 = VIdMap.numItems VE2 andalso
        VIdMap.alli
            (fn(vid, (sigma1,is1)) =>
                case VIdMap.find(VE2, vid)
                  of NONE             => false
                   | SOME(sigma2,is2) =>
                        TypeScheme.equals(sigma1,sigma2) andalso is1 = is2
            )
            VE1


    fun enriches((SE1,TE1,VE1), (SE2,TE2,VE2)) =
            enrichesSE(SE1,SE2) andalso
            enrichesTE(TE1,TE2) andalso
            enrichesVE(VE1,VE2)

    and enrichesSE(SE1,SE2) =
        StrIdMap.alli
            (fn(strid, Str E2) =>
                case StrIdMap.find(SE1, strid)
                  of NONE         => false
                   | SOME(Str E1) => enriches(E1,E2)
            )
            SE2

    and enrichesTE(TE1,TE2) =
        TyConMap.alli
            (fn(tycon, tystr2) =>
                case TyConMap.find(TE1, tycon)
                  of NONE        => false
                   | SOME tystr1 => enrichesTyStr(tystr1,tystr2)
            )
            TE2

    and enrichesVE(VE1,VE2) =
        VIdMap.alli
            (fn(vid, valstr2) =>
                case VIdMap.find(VE1, vid)
                  of NONE         => false
                   | SOME valstr1 => enrichesValStr(valstr1,valstr2)
            )
            VE2

    and enrichesTyStr((theta1,VE1), (theta2,VE2)) =
            TypeFcn.equals(theta1,theta2) andalso
            ( VIdMap.isEmpty VE2 orelse equalsVE(VE1,VE2) )

    and enrichesValStr((sigma1,is1), (sigma2,is2)) =
            TypeScheme.generalises(sigma1,sigma2) andalso
            IdStatus.generalises(is1,is2)

  end
(* stop of StaticEnv.sml *)
(* start of SIG.sml *)
(*
 * Standard ML signatures
 *
 * Definition, sections 5.1, 5.3, and 5.6
 *)


signature SIG =
  sig

    (* Import types *)

    type Env         = StaticEnv.Env
    type TyVarSet    = TyVarSet.set
    type TyNameSet   = TyNameSet.set
    type Realisation = Type.Realisation


    (* Type [Section 5.1] *)

    type Sig = TyNameSet * Env                          (* [Sigma] *)


    (* Operations *)

    val tyvars:         Sig -> TyVarSet
    val tynames:        Sig -> TyNameSet

    val rename:         Sig -> Sig

    exception Match
    val match:          Env * Sig -> Env * Realisation (* Matching *)

  end
(* stop of SIG.sml *)
(* start of Sig.sml *)
(*
 * Standard ML signatures
 *
 * Definition, sections 5.1, 5.3, and 5.6
 *)


structure Sig :> SIG =
  struct

    (* Import types *)

    type Env         = StaticEnv.Env
    type TyVarSet    = TyVarSet.set
    type TyNameSet   = TyNameSet.set
    type Realisation = Type.Realisation


    (* Type [Section 5.1] *)

    type Sig = TyNameSet * Env                          (* [Sigma] *)


    (* Type variable and type name extraction [Section 4.2] *)

    fun tyvars  (T,E) = StaticEnv.tyvars E
    fun tynames (T,E) = TyNameSet.difference(StaticEnv.tynames E, T)


    (* Alpha Renaming *)

    fun rename (T,E) =
        let
            val phi' = TyNameSet.foldl
                         (fn(t,phi')=> TyNameMap.insert(phi',t,TyName.rename t))
                         TyNameMap.empty T
            val phi = TyNameMap.map TypeFcn.fromTyName phi'
            val T'  = TyNameSet.map (fn t => valOf(TyNameMap.find(phi',t))) T
            val E'  = StaticEnv.realise phi E
        in
            (T',E')
        end


    (* Matching [Section 5.6] *)

    exception Match

    fun matchTypeFcn(theta', theta, phi, T) =
        if TypeFcn.arity theta <> TypeFcn.arity theta' then
            raise Match
        else
        case TypeFcn.toTyName theta
          of NONE   => phi
           | SOME t =>
                if isSome(TyNameMap.find(phi, t))
                orelse not(TyNameSet.member(T, t)) then
                    phi
                else
                let
                    val phi' = TyNameMap.insert(phi, t, TypeFcn.rename theta')
                in
                    TyNameMap.map (TypeFcn.realise phi') phi'
                end

    fun matchTE(TE', TE, phi, T) =
        let
            fun matchTyStr(tycon, (theta,VE), phi) =
                case TyConMap.find(TE', tycon)
                  of NONE             => raise Match
                   | SOME(theta',VE') => matchTypeFcn(theta', theta, phi, T)
        in
            TyConMap.foldli matchTyStr phi TE
        end

    fun matchSE(SE', SE, phi, T) =
        let
            fun matchStr(strid, StaticEnv.Str E, phi) =
                case StrIdMap.find(SE', strid)
                  of NONE                   => raise Match
                   | SOME(StaticEnv.Str E') => matchE(E', E, phi, T)
        in
            StrIdMap.foldli matchStr phi SE
        end

    and matchE((SE',TE',VE'), (SE,TE,VE), phi, T) =
        let
            val phi1 = matchTE(TE', TE, phi, T)
            val phi2 = matchSE(SE', SE, phi1, T)
        in
            phi2
        end

    fun match(E', (T,E)) =
        let
            val phi = matchE(E', E, TyNameMap.empty, T)
            val E'' = StaticEnv.realise phi E
        in
            if StaticEnv.enriches(E',E'') then
                (E'', phi)
            else
                raise Match
        end

  end
(* stop of Sig.sml *)
(* start of FUNSIG.sml *)
(*
 * Standard ML functor signatures
 *
 * Definition, sections 5.1 and 5.4
 *)


signature FUNSIG =
  sig

    (* Import types *)

    type Env       = StaticEnv.Env
    type Sig       = Sig.Sig
    type TyVarSet  = TyVarSet.set
    type TyNameSet = TyNameSet.set


    (* Type [Section 5.1] *)

    type FunSig = TyNameSet * (Env * Sig)               (* [Phi] *)


    (* Operations *)

    val tyvars:         FunSig -> TyVarSet
    val tynames:        FunSig -> TyNameSet

  end
(* stop of FUNSIG.sml *)
(* start of FunSig.sml *)
(*
 * Standard ML functor signatures
 *
 * Definition, sections 5.1 and 5.4
 *)


structure FunSig :> FUNSIG =
  struct

    (* Import types *)

    type Env       = StaticEnv.Env
    type Sig       = Sig.Sig
    type TyVarSet  = TyVarSet.set
    type TyNameSet = TyNameSet.set


    (* Type [Section 5.1] *)

    type FunSig = TyNameSet * (Env * Sig)               (* [Phi] *)


    (* Type variable and type name extraction [Section 4.2] *)

    fun tyvars (T,(E,Sigma)) =
        TyVarSet.union(StaticEnv.tyvars E, Sig.tyvars Sigma)

    fun tynames (T,(E,Sigma)) =
        TyNameSet.difference(TyNameSet.union(StaticEnv.tynames E,
                                             Sig.tynames Sigma), T)

  end
(* stop of FunSig.sml *)
(* start of CONTEXT.sml *)
(*
 * Standard ML contexts
 *
 * Definition, sections 4.2, 4.3, 4.7, and 4.9
 *)

signature CONTEXT =
  sig

    (* Import types *)

    type VId        = VId.Id
    type TyCon      = TyCon.Id
    type StrId      = StrId.Id
    type longVId    = LongVId.longId
    type longTyCon  = LongTyCon.longId
    type longStrId  = LongStrId.longId
    type TyName     = TyName.TyName
    type TyNameSet  = TyNameSet.set
    type TyVar      = TyVar.TyVar
    type TyVarSet   = TyVarSet.set
    type IdStatus   = IdStatus.IdStatus
    type TypeScheme = TypeScheme.TypeScheme
    type StrEnv     = StaticEnv.StrEnv
    type Str        = StaticEnv.Str
    type TyStr      = StaticEnv.TyStr
    type TyEnv      = StaticEnv.TyEnv
    type ValStr     = StaticEnv.ValStr
    type ValEnv     = StaticEnv.ValEnv
    type Env        = StaticEnv.Env


    (* Type [Section 4.2] *)

    type Context = TyNameSet * TyVarSet * Env           (* [C] *)


    (* Operations *)

    val Tof:            Context -> TyNameSet
    val Uof:            Context -> TyVarSet
    val Eof:            Context -> Env

    val plusVE:         Context * ValEnv   -> Context
    val plusU:          Context * TyVarSet -> Context
    val oplusE:         Context * Env      -> Context
    val oplusTE:        Context * TyEnv    -> Context
    val oplusVEandTE:   Context * (ValEnv * TyEnv) -> Context

    val findVId:        Context * VId       -> ValStr option
    val findTyCon:      Context * TyCon     -> TyStr option
    val findStrId:      Context * StrId     -> Str option
    val findLongVId:    Context * longVId   -> ValStr option
    val findLongTyCon:  Context * longTyCon -> TyStr option
    val findLongStrId:  Context * longStrId -> Str option

    val tyvars:         Context -> TyVarSet

  end
(* stop of CONTEXT.sml *)
(* start of Context.sml *)
(*
 * Standard ML contexts
 *
 * Definition, sections 4.2, 4.3, 4.7, and 4.9
 *)

structure Context :> CONTEXT =
  struct

    (* Import types *)

    type VId        = VId.Id
    type TyCon      = TyCon.Id
    type StrId      = StrId.Id
    type longVId    = LongVId.longId
    type longTyCon  = LongTyCon.longId
    type longStrId  = LongStrId.longId
    type TyName     = TyName.TyName
    type TyNameSet  = TyNameSet.set
    type TyVar      = TyVar.TyVar
    type TyVarSet   = TyVarSet.set
    type IdStatus   = IdStatus.IdStatus
    type TypeScheme = TypeScheme.TypeScheme
    type StrEnv     = StaticEnv.StrEnv
    type Str        = StaticEnv.Str
    type TyStr      = StaticEnv.TyStr
    type TyEnv      = StaticEnv.TyEnv
    type ValStr     = StaticEnv.ValStr
    type ValEnv     = StaticEnv.ValEnv
    type Env        = StaticEnv.Env


    (* Type [Section 4.2] *)

    type Context = TyNameSet * TyVarSet * Env           (* [C] *)


    (* Projections [Section 4.3] *)

    fun Tof (T,U,E) = T
    fun Uof (T,U,E) = U
    fun Eof (T,U,E) = E


    (* Modification [Section 4.3] *)

    infix plusVE plusU oplusE oplusTE oplusVEandTE

    fun (T,U,E) plusVE VE = ( T, U, StaticEnv.plusVE(E,VE) )
    fun (T,U,E) plusU  U' = ( T, TyVarSet.union(U,U'), E )

    fun (T,U,E) oplusE E' =
        ( TyNameSet.union(T, StaticEnv.tynames E)
        , U
        , StaticEnv.plus(E,E')
        )

    fun (T,U,E) oplusTE TE =
        ( TyNameSet.union(T, StaticEnv.tynamesTE TE)
        , U
        , StaticEnv.plusTE(E,TE)
        )

    fun (T,U,E) oplusVEandTE (VE,TE) =
        ( TyNameSet.union(T, StaticEnv.tynamesTE TE)
        , U
        , StaticEnv.plusVEandTE(E, (VE,TE))
        )


    (* Application (lookup) [Section 4.3] *)

    fun findVId  ((T,U,E), vid)   = StaticEnv.findVId(E, vid)
    fun findTyCon((T,U,E), tycon) = StaticEnv.findTyCon(E, tycon)
    fun findStrId((T,U,E), strid) = StaticEnv.findStrId(E, strid)

    fun findLongVId  ((T,U,E), longvid)   = StaticEnv.findLongVId(E,longvid)
    fun findLongTyCon((T,U,E), longtycon) = StaticEnv.findLongTyCon(E,longtycon)
    fun findLongStrId((T,U,E), longstrid) = StaticEnv.findLongStrId(E,longstrid)


    (* Calculation of tyvars [Section 4.2] *)

    fun tyvars (T,U,E) = TyVarSet.union(U, StaticEnv.tyvars E)

  end
(* stop of Context.sml *)
(* start of STATIC_BASIS.sml *)
(*
 * Standard ML static basis and environments of modules
 *
 * Definition, section 5.1
 *)

signature STATIC_BASIS =
  sig

    (* Import types *)

    type StrId       = StrId.Id
    type SigId       = SigId.Id
    type FunId       = FunId.Id
    type longStrId   = LongStrId.longId
    type longTyCon   = LongTyCon.longId
    type Env         = StaticEnv.Env
    type StrEnv      = StaticEnv.StrEnv
    type Str         = StaticEnv.Str
    type TyStr       = StaticEnv.TyStr
    type Context     = Context.Context
    type Sig         = Sig.Sig
    type FunSig      = FunSig.FunSig
    type TyVarSet    = TyVarSet.set
    type TyNameSet   = TyNameSet.set
    type 'a SigIdMap = 'a SigIdMap.map
    type 'a FunIdMap = 'a FunIdMap.map


    (* Types [Section 5.1] *)

    type SigEnv = Sig SigIdMap                                  (* [G] *)
    type FunEnv = FunSig FunIdMap                               (* [F] *)

    type Basis  = TyNameSet * FunEnv * SigEnv * Env             (* [B] *)


    (* Operations *)

    val empty:          Basis
    val fromTandE:      TyNameSet * Env    -> Basis
    val fromTandF:      TyNameSet * FunEnv -> Basis
    val fromTandG:      TyNameSet * SigEnv -> Basis

    val Tof:            Basis -> TyNameSet
    val Cof:            Basis -> Context

    val plus:           Basis * Basis     -> Basis
    val plusT:          Basis * TyNameSet -> Basis
    val oplusSE:        Basis * StrEnv    -> Basis
    val oplusG:         Basis * SigEnv    -> Basis
    val oplusF:         Basis * FunEnv    -> Basis
    val oplusE:         Basis * Env       -> Basis

    val findStrId:      Basis * StrId     -> Str option
    val findSigId:      Basis * SigId     -> Sig option
    val findFunId:      Basis * FunId     -> FunSig option
    val findLongStrId:  Basis * longStrId -> Str option
    val findLongTyCon:  Basis * longTyCon -> TyStr option

    val tyvars:         Basis  -> TyVarSet
    val tynamesF:       FunEnv -> TyNameSet
    val tynamesG:       SigEnv -> TyNameSet
 
  end
(* stop of STATIC_BASIS.sml *)
(* start of StaticBasis.sml *)
(*
 * Standard ML static basis and environments of modules
 *
 * Definition, section 5.1
 *)

structure StaticBasis :> STATIC_BASIS =
  struct

    (* Import types *)

    type StrId       = StrId.Id
    type SigId       = SigId.Id
    type FunId       = FunId.Id
    type longStrId   = LongStrId.longId
    type longTyCon   = LongTyCon.longId
    type Env         = StaticEnv.Env
    type StrEnv      = StaticEnv.StrEnv
    type Str         = StaticEnv.Str
    type TyStr       = StaticEnv.TyStr
    type Context     = Context.Context
    type Sig         = Sig.Sig
    type FunSig      = FunSig.FunSig
    type TyVarSet    = TyVarSet.set
    type TyNameSet   = TyNameSet.set
    type 'a SigIdMap = 'a SigIdMap.map
    type 'a FunIdMap = 'a FunIdMap.map


    (* Types [Section 5.1] *)

    type SigEnv = Sig SigIdMap                                  (* [G] *)
    type FunEnv = FunSig FunIdMap                               (* [F] *)

    type Basis  = TyNameSet * FunEnv * SigEnv * Env             (* [B] *)



    (* Calculation of type variable and type name sets [Section 4.2] *)

    fun tyvarsG G =
        SigIdMap.foldl
            (fn(Sigma, U) => TyVarSet.union(U, Sig.tyvars Sigma))
            TyVarSet.empty G

    fun tyvarsF F =
        FunIdMap.foldl
            (fn(Phi, U) => TyVarSet.union(U, FunSig.tyvars Phi))
            TyVarSet.empty F

    fun tyvars (T,F,G,E) = TyVarSet.union(TyVarSet.union(
                                tyvarsF F, tyvarsG G), StaticEnv.tyvars E)
    


    fun tynamesG G =
        SigIdMap.foldl
            (fn(Sigma, T) => TyNameSet.union(T, Sig.tynames Sigma))
            TyNameSet.empty G

    fun tynamesF F =
        FunIdMap.foldl
            (fn(Phi, T) => TyNameSet.union(T, FunSig.tynames Phi))
            TyNameSet.empty F



    (* Injection [Sections 4.3 and 5.1] *)

    val empty =
        ( TyNameSet.empty, FunIdMap.empty, SigIdMap.empty, StaticEnv.empty )

    fun fromTandE(T,E) = ( T, FunIdMap.empty, SigIdMap.empty, E )
    fun fromTandF(T,F) = ( T, F, SigIdMap.empty, StaticEnv.empty )
    fun fromTandG(T,G) = ( T, FunIdMap.empty, G, StaticEnv.empty )


    (* Projections [Sections 4.3 and 5.1] *)

    fun Tof (T,F,G,E) = T
    fun Cof (T,F,G,E) = (T, TyVarSet.empty, E)


    (* Modifications [Sections 4.3 and 5.1] *)

    infix plus plusT oplusG oplusF oplusE oplusSE

    fun (T,F,G,E) plus (T',F',G',E') =
        ( TyNameSet.union(T,T')
        , FunIdMap.unionWith #2 (F,F')
        , SigIdMap.unionWith #2 (G,G')
        , StaticEnv.plus(E,E')
        )

    fun (T,F,G,E) plusT T' = ( TyNameSet.union(T,T'), F, G, E )

    fun (T,F,G,E) oplusG G' =
        ( TyNameSet.union(T, tynamesG G')
        , F
        , SigIdMap.unionWith #2 (G,G')
        , E
        )

    fun (T,F,G,E) oplusF F' =
        ( TyNameSet.union(T, tynamesF F')
        , FunIdMap.unionWith #2 (F,F')
        , G
        , E
        )

    fun (T,F,G,E) oplusE E' =
        ( TyNameSet.union(T, StaticEnv.tynames E')
        , F
        , G
        , StaticEnv.plus(E,E')
        )

    fun (T,F,G,E) oplusSE SE =
        ( TyNameSet.union(T, StaticEnv.tynamesSE SE)
        , F
        , G
        , StaticEnv.plusSE(E,SE)
        )

    (* Application (lookup) [Sections 5.1 and 4.3] *)

    fun findStrId((T,F,G,E), strid) = StaticEnv.findStrId(E, strid)
    fun findSigId((T,F,G,E), sigid) = SigIdMap.find(G, sigid)
    fun findFunId((T,F,G,E), funid) = FunIdMap.find(F, funid)
    fun findLongStrId((T,F,G,E), longstrid) =
        StaticEnv.findLongStrId(E, longstrid)
    fun findLongTyCon((T,F,G,E), longtycon) =
        StaticEnv.findLongTyCon(E, longtycon)

  end
(* stop of StaticBasis.sml *)
(* start of INITIAL_STATIC_ENV.sml *)
(*
 * Standard ML core view of the initial static basis
 *
 * Definition, appendices C and E
 *)

signature INITIAL_STATIC_ENV =
  sig

    (* Import types *)

    type Type             = Type.Type
    type TyNameSet        = TyNameSet.set
    type OverloadingClass = OverloadingClass.OverloadingClass
    type Env              = StaticEnv.Env


    (* Predefined monomorphic types [Figure 24] *)

    val tauBool:        Type
    val tauInt:         Type
    val tauWord:        Type
    val tauReal:        Type
    val tauString:      Type
    val tauChar:        Type
    val tauExn:         Type

    (* Overloading classes [Appendix E.1] *)

    val Int:            OverloadingClass
    val Real:           OverloadingClass
    val Word:           OverloadingClass
    val String:         OverloadingClass
    val Char:           OverloadingClass
    val WordInt:        OverloadingClass
    val RealInt:        OverloadingClass
    val Num:            OverloadingClass
    val NumTxt:         OverloadingClass

    (* Initial environment [Appendix C] *)

    val T0:             TyNameSet
    val E0:             Env

  end
(* stop of INITIAL_STATIC_ENV.sml *)
(* start of InitialStaticEnv.sml *)
(*
 * Standard ML core view of the initial static basis
 *
 * Definition, appendices C and E
 *)

structure InitialStaticEnv :> INITIAL_STATIC_ENV =
  struct

    (* Import *)

    type Type             = Type.Type
    type TyNameSet        = TyNameSet.set
    type Overloadingclass = OverloadingClass.OverloadingClass
    type VIdSet           = VIdSet.set
    type Env              = StaticEnv.Env
    type ValEnv           = StaticEnv.ValEnv


    open Type
    open IdStatus


    (* Helpers *)

    fun pairType(tau1,tau2) =
        let
            val Rho = LabMap.insert(LabMap.insert(LabMap.empty,
                                                  Lab.fromInt 1, tau1),
                                                  Lab.fromInt 2, tau2)
        in
            fromRowType (Rho,CLOSEDRow)
        end


    (* VIds [Figure 25] *)

    val vidEq     = VId.fromString "="
    val vidAssign = VId.fromString ":="

    val vidFalse  = VId.fromString "false"
    val vidTrue   = VId.fromString "true"
    val vidNil    = VId.fromString "nil"
    val vidCons   = VId.fromString "::"
    val vidRef    = VId.fromString "ref"

    val vidMatch  = VId.fromString "Match"
    val vidBind   = VId.fromString "Bind"


    (* TyCons [Figure 24] *)

    val tyconUnit   = TyCon.fromString "unit"
    val tyconBool   = TyCon.fromString "bool"
    val tyconInt    = TyCon.fromString "int"
    val tyconWord   = TyCon.fromString "word"
    val tyconReal   = TyCon.fromString "real"
    val tyconString = TyCon.fromString "string"
    val tyconChar   = TyCon.fromString "char"
    val tyconList   = TyCon.fromString "list"
    val tyconRef    = TyCon.fromString "ref"
    val tyconExn    = TyCon.fromString "exn"


    (* TyNames [Appendix C] *)

    val tBool   = TyName.tyname(tyconBool,   0, TyName.EQ, 2)
    val tInt    = TyName.tyname(tyconInt,    0, TyName.EQ, 0)
    val tWord   = TyName.tyname(tyconWord,   0, TyName.EQ, 0)
    val tReal   = TyName.tyname(tyconReal,   0, TyName.NOEQ, 0)
    val tString = TyName.tyname(tyconString, 0, TyName.EQ, 0)
    val tChar   = TyName.tyname(tyconChar,   0, TyName.EQ, 0)
    val tList   = TyName.tyname(tyconList,   1, TyName.EQ, 2)
    val tRef    = TyName.tyname(tyconRef,    1, TyName.SPECIALEQ, 1)
    val tExn    = TyName.tyname(tyconExn,    0, TyName.NOEQ, 0)

    val T0      = TyNameSet.fromList[tBool, tInt, tWord, tReal, tString, tChar,
                                     tList, tRef, tExn]


    (* Types *)

    val alpha      = TyVar.fromString "'a"
    val alphaEq    = TyVar.fromString "''a"
    val tauAlpha   = fromTyVar alpha
    val tauAlphaEq = fromTyVar alphaEq

    val tauUnit      = fromRowType emptyRho
    val tauBool      = fromConsType([], tBool)
    val tauInt       = fromConsType([], tInt)
    val tauWord      = fromConsType([], tWord)
    val tauReal      = fromConsType([], tReal)
    val tauString    = fromConsType([], tString)
    val tauChar      = fromConsType([], tChar)
    val tauExn       = fromConsType([], tExn)
    val tauAlphaList = fromConsType([tauAlpha], tList)
    val tauAlphaRef  = fromConsType([tauAlpha], tRef)


    (* TypeSchemes [Figure 25] *)

    val sigmaEq     = ([alphaEq], fromFunType(pairType(tauAlphaEq,tauAlphaEq),
                                              tauBool))
    val sigmaAssign = ([alpha],   fromFunType(pairType(tauAlphaRef,tauAlpha),
                                              tauUnit))
    val sigmaFalse  = ([], tauBool)
    val sigmaTrue   = ([], tauBool)
    val sigmaNil    = ([alpha], tauAlphaList)
    val sigmaCons   = ([alpha], fromFunType(pairType(tauAlpha, tauAlphaList),
                                            tauAlphaList))
    val sigmaRef    = ([alpha], fromFunType(tauAlpha, tauAlphaRef))

    val sigmaMatch  = ([], tauExn)
    val sigmaBind   = ([], tauExn)


    (* Value entries [Figure 25] *)

    val valstrEq     = (sigmaEq,     v)
    val valstrAssign = (sigmaAssign, v)

    val valstrFalse  = (sigmaFalse,  c)
    val valstrTrue   = (sigmaTrue,   c)
    val valstrNil    = (sigmaNil,    c)
    val valstrCons   = (sigmaCons,   c)
    val valstrRef    = (sigmaRef,    c)

    val valstrMatch  = (sigmaMatch,  e)
    val valstrBind   = (sigmaBind,   e)


    (* TypeFcns [Figure 24] *)

    val thetaUnit   = ([], tauUnit)
    val thetaBool   = ([], tauBool)
    val thetaInt    = ([], tauInt)
    val thetaWord   = ([], tauWord)
    val thetaReal   = ([], tauReal)
    val thetaString = ([], tauString)
    val thetaChar   = ([], tauChar)
    val thetaExn    = ([], tauExn)
    val thetaList   = ([alpha], tauAlphaList)
    val thetaRef    = ([alpha], tauAlphaRef)


    (* TyStrs [Figure 25] *)

    val VEEmpty = VIdMap.empty
    val VEBool  = VIdMap.fromList[(vidFalse, valstrFalse),
                                  (vidTrue,  valstrTrue)] : ValEnv
    val VEList  = VIdMap.fromList[(vidNil,   valstrNil),
                                  (vidCons,  valstrCons)]
    val VERef   = VIdMap.fromList[(vidRef,   valstrRef)]

    val tystrUnit   = (thetaUnit,   VEEmpty)
    val tystrBool   = (thetaBool,   VEBool )
    val tystrInt    = (thetaInt,    VEEmpty)
    val tystrWord   = (thetaWord,   VEEmpty)
    val tystrReal   = (thetaReal,   VEEmpty)
    val tystrString = (thetaString, VEEmpty)
    val tystrChar   = (thetaChar,   VEEmpty)
    val tystrList   = (thetaList,   VEList )
    val tystrRef    = (thetaRef,    VERef  )
    val tystrExn    = (thetaExn,    VEEmpty)


    (* Environments [Appendix C] *)

    val SE0 = StrIdMap.empty

    val TE0 = TyConMap.fromList[(tyconUnit,   tystrUnit),
                                (tyconBool,   tystrBool),
                                (tyconInt,    tystrInt),
                                (tyconWord,   tystrWord),
                                (tyconReal,   tystrReal),
                                (tyconString, tystrString),
                                (tyconChar,   tystrChar),
                                (tyconList,   tystrList),
                                (tyconRef,    tystrRef),
                                (tyconExn,    tystrExn)]

    val VE0 = VIdMap.fromList  [(vidEq,     valstrEq),
                                (vidAssign, valstrAssign),
                                (vidRef,    valstrRef),
                                (vidNil,    valstrNil),
                                (vidCons,   valstrCons),
                                (vidFalse,  valstrFalse),
                                (vidTrue,   valstrTrue),
                                (vidMatch,  valstrMatch),
                                (vidBind,   valstrBind)]

    val E0 = (SE0,TE0,VE0)


    (* Overloading classes [Section E.1] *)

    val Int     = OverloadingClass.make(TyNameSet.singleton tInt,    tInt)
    val Real    = OverloadingClass.make(TyNameSet.singleton tReal,   tReal)
    val Word    = OverloadingClass.make(TyNameSet.singleton tWord,   tWord)
    val String  = OverloadingClass.make(TyNameSet.singleton tString, tString)
    val Char    = OverloadingClass.make(TyNameSet.singleton tChar,   tChar)
    val WordInt = OverloadingClass.union(Word, Int)     (* default is 2nd *)
    val RealInt = OverloadingClass.union(Real, Int)
    val Num     = OverloadingClass.union(Word, RealInt)
    val Txt     = OverloadingClass.union(String, Char)
    val NumTxt  = OverloadingClass.union(Txt, Num)

  end
(* stop of InitialStaticEnv.sml *)
(* start of INITIAL_STATIC_BASIS.sml *)
(*
 * Standard ML initial static basis
 *
 * Definition, appendices C and E
 *)

signature INITIAL_STATIC_BASIS =
  sig

    (* Import *)

    type Basis = StaticBasis.Basis


    (* Export *)

    val B0: Basis

  end
(* stop of INITIAL_STATIC_BASIS.sml *)
(* start of InitialStaticBasis.sml *)
(*
 * Standard ML initial static basis
 *
 * Definition, appendices C and E
 *)

structure InitialStaticBasis :> INITIAL_STATIC_BASIS =
  struct

    (* Import *)

    type Basis = StaticBasis.Basis


    (* Enviornments *)

    val T0 = InitialStaticEnv.T0
    val F0 = FunIdMap.empty
    val G0 = SigIdMap.empty
    val E0 = InitialStaticEnv.E0

    val B0 = (T0,F0,G0,E0)

  end
(* stop of InitialStaticBasis.sml *)
(* start of EXNAME.sml *)
(*
 * Standard ML exception names
 *
 * Definition, section 6.2
 *)

signature EXNAME =
  sig

    (* Import *)

    type VId = VId.Id


    (* Type [Section 6.2] *)

    eqtype ExName                                       (* [en] *)


    (* Operations *)

    val exname:   VId -> ExName
    val toString: ExName -> string

    val compare:  ExName * ExName -> order

  end
(* stop of EXNAME.sml *)
(* start of ExName.sml *)
(*
 * Standard ML exception names
 *
 * Definition, section 6.2
 *)


structure ExName :> EXNAME =
  struct

    (* Import *)

    type VId   = VId.Id
    type stamp = Stamp.stamp


    (* Type [Section 6.2] *)

    type ExName =                                     (* [en] *)
         { vid:   VId
         , stamp: stamp
         }


    (* Creation *)

    fun exname vid = { vid = vid, stamp = Stamp.stamp() }


    (* Conversion *)

    fun toString{vid, stamp} = VId.toString vid


    (* Ordering *)

    fun compare(en1: ExName, en2: ExName) =
            Stamp.compare(#stamp en1, #stamp en2)

  end
(* stop of ExName.sml *)
(* start of ADDR.sml *)
(*
 * Standard ML addresses
 *
 * Definition, section 6.2
 *)

signature ADDR =
  sig

    (* Type [Section 6.2] *)

    eqtype Addr                                 (* [a] *)


    (* Operations *)

    val addr:    unit -> Addr
    val compare: Addr * Addr -> order

  end
(* stop of ADDR.sml *)
(* start of Addr.sml *)
(*
 * Standard ML addresses
 *
 * Definition, section 6.2
 *)


structure Addr :> ADDR =
  struct

    (* Type [Section 6.2] *)

    type Addr = Stamp.stamp                             (* [a] *)


    (* Operations *)

    val addr    = Stamp.stamp
    val compare = Stamp.compare

  end
(* stop of Addr.sml *)
(* start of AssembliesCoreDynamic.sml *)
(*
 * Standard ML additional sets and maps for the dynamic semantics of the core
 *
 * Definition, section 6.3
 *)

structure ExNameSet  = FinSetFn(type ord_key = ExName.ExName
                                val  compare = ExName.compare)

structure AddrMap    = FinMapFn(type ord_key = Addr.Addr
                                val  compare = Addr.compare)
(* stop of AssembliesCoreDynamic.sml *)
(* start of SVAL.sml *)
(*
 * Standard ML special values
 *
 * Definition, section 6.2
 *)


signature SVAL =
  sig 

    (* Type [Section 6.2] *)

    datatype SVal =                             (* [sv] *)
          INT    of int
        | WORD   of word
        | STRING of string
        | CHAR   of char
        | REAL   of real

    (* Operations *)

    val toString:  SVal -> string
    val equal:     SVal * SVal -> bool

  end
(* stop of SVAL.sml *)
(* start of SVal.sml *)
(*
 * Standard ML special values
 *
 * Definition, section 6.2
 *)


structure SVal :> SVAL =
  struct

    (* Type [Section 6.2] *)

    datatype SVal =                             (* [sv] *)
          INT    of int
        | WORD   of word
        | STRING of string
        | CHAR   of char
        | REAL   of real


    (* Conversions *)

    fun toString(INT i)    = Int.toString i
      | toString(WORD w)   = "0wx" ^ Word.toString w
      | toString(STRING s) = "\""  ^ String.toString s ^ "\""
      | toString(CHAR c)   = "\"#" ^ Char.toString c   ^ "\""
      | toString(REAL r)   = Real.toString r

    (* Equality *)

    fun equal(INT n1,    INT n2   ) = n1 = n2
      | equal(WORD w1,   WORD w2  ) = w1 = w2
      | equal(STRING s1, STRING s2) = s1 = s2
      | equal(CHAR c1,   CHAR c2  ) = c1 = c2
      | equal _ = raise Fail "type error: equality type expected"

  end
(* stop of SVal.sml *)
(* start of VAL.sml *)
(*
 * Standard ML values
 *
 * Definition, sections 6.2, 6.3, and 6.4
 *
 * Note:
 *   - All value types are parameterised over the representation of function
 *     closures to break up the recursion between values and environments.
 *   - The basic values are just strings.
 *)


signature VAL =
  sig 

    (* Import *)

    type Addr      = Addr.Addr
    type ExName    = ExName.ExName
    type SVal      = SVal.SVal
    type VId       = VId.Id
    type 'a LabMap = 'a LabMap.map


    (* Types [Sections 6.2 and 6.3] *)

    type BasVal = string                        (* [b] *)

    datatype 'a Val =                           (* [v] *)
          :=
        | SVal   of SVal
        | BasVal of BasVal
        | VId    of VId
        | VIdVal of VId * 'a Val
        | ExVal  of 'a ExVal
        | Record of (*Record*) 'a Val LabMap
        | Addr   of Addr
        | FcnClosure of 'a

    and 'a ExVal =                              (* [e] *)
          ExName    of ExName
        | ExNameVal of ExName * 'a Val

    type 'a Record = 'a Val LabMap              (* [r] *)


    (* Operations *)

    val equal:      'a Val * 'a Val -> bool

    val toBoolVal:  bool -> 'a Val 
    val unpair:     'a Val -> ('a Val * 'a Val) option

  end
(* stop of VAL.sml *)
(* start of Val.sml *)
(*
 * Standard ML values
 *
 * Definition, sections 6.2, 6.3, and 6.4
 *
 * Note:
 *   - All value types are parameterised over the representation of function
 *     closures to break up the recursion between values and environments.
 *   - The basic values are just strings.
 *)


structure Val :> VAL =
  struct

    (* Import *)

    type Addr      = Addr.Addr
    type ExName    = ExName.ExName
    type SVal      = SVal.SVal
    type VId       = VId.Id
    type 'a LabMap = 'a LabMap.map


    (* Types [Sections 6.2 and 6.3] *)

    type BasVal = string                        (* [b] *)


    datatype 'a Val =                           (* [v] *)
          op:=
        | SVal   of SVal
        | BasVal of BasVal
        | VId    of VId
        | VIdVal of VId * 'a Val
        | ExVal  of 'a ExVal
        | Record of 'a Record
        | Addr   of Addr
        | FcnClosure of 'a

    and 'a ExVal =                              (* [e] *)
          ExName    of ExName
        | ExNameVal of ExName * 'a Val

    withtype 'a Record = 'a Val LabMap          (* [r] *)


    (* Operations *)

    fun toBoolVal b = VId(VId.fromString(if b then "true" else "false"))


    fun unpair(Record r) =
        (case (LabMap.find(r, Lab.fromInt 1), LabMap.find(r, Lab.fromInt 2))
           of (SOME v1, SOME v2) => SOME(v1, v2)
            | _ => NONE
        )
      | unpair _ = NONE



    (* Implementation of polymorphic equality *)

    fun equal(SVal sv1,          SVal sv2         ) = SVal.equal(sv1, sv2)
      | equal(VId vid1,          VId vid2         ) = vid1 = vid2
      | equal(ExVal(ExName en1), ExVal(ExName en2)) = en1 = en2
      | equal(Addr a1,           Addr a2          ) = a1 = a2

      | equal(VIdVal(vid1, v1), VIdVal(vid2, v2)) =
            vid1 = vid2 andalso equal(v1, v2)

      | equal(ExVal(ExNameVal(en1,v1)), ExVal(ExNameVal(en2,v2))) =
            en1 = en2 andalso equal(v1, v2)

      | equal(Record r1, Record r2) =
            LabMap.numItems r1 = LabMap.numItems r2 andalso
            LabMap.alli (fn(lab, v1) =>
                           case LabMap.find(r2, lab)
                             of SOME v2 => equal(v1, v2)
                              | NONE    => false
                        ) r1

      | equal _ = false

  end
(* stop of Val.sml *)
(* start of STATE.sml *)
(*
 * Standard ML state
 *
 * Definition, section 6.3
 *
 * Notes:
 *   - Memory gets represented by references. This avoids expanding out all
 *     occurances of the state convention in the inference rules.
 *   - Since exception names are generated by stamps we do not really need the
 *     exception name set. We maintain it anyway.
 *)

signature STATE =
  sig

    (* Import *)

    type Addr       = Addr.Addr
    type ExName     = ExName.ExName
    type ExNameSet  = ExNameSet.set
    type 'a Val     = 'a Val.Val
    type 'a AddrMap = 'a AddrMap.map


    (* Types [Section 6.3] *)

    type 'a Mem   = 'a Val AddrMap                      (* [mem] *)

    type 'a State = 'a Mem * ExNameSet                  (* [s] *)


    (* Operations *)

    val insertAddr:     'a State * Addr * 'a Val -> 'a State
    val insertExName:   'a State * ExName        -> 'a State

    val findAddr:       'a State * Addr -> 'a Val option

  end
(* stop of STATE.sml *)
(* start of State.sml *)
(*
 * Standard ML state
 *
 * Definition, section 6.3
 *
 * Notes:
 *   - Memory gets represented by references. This avoids expanding out all
 *     occurances of the state convention in the inference rules.
 *   - Since exception names are generated by stamps we do not really need the
 *     exception name set. We maintain it anyway.
 *)

structure State :> STATE =
  struct

    (* Import *)

    type Addr       = Addr.Addr
    type ExName     = ExName.ExName
    type ExNameSet  = ExNameSet.set
    type 'a Val     = 'a Val.Val
    type 'a AddrMap = 'a AddrMap.map


    (* Types [Section 6.3] *)

    type 'a Mem   = 'a Val AddrMap                      (* [mem] *)

    type 'a State = 'a Mem * ExNameSet                  (* [s] *)


    (* Operations *)

    fun insertAddr  ((mem,ens), a, v) = ( AddrMap.insert(mem, a, v), ens )
    fun insertExName((mem,ens), en)   = ( mem, ExNameSet.add(ens, en) )

    fun findAddr((mem,ens), a) = AddrMap.find(mem, a)

  end
(* stop of State.sml *)
(* start of GRAMMAR_CORE.sml *)
(*
 * Standard ML abstract core grammar
 *
 * Definition, section 2.8
 *
 * Note:
 *   This is the syntax used in the inference rules for the core [Definition,
 *   sections 4.10 and 6.7]. It omits almost anything having to do with infixed
 *   identifiers:
 *     - fixity directives
 *     - infixed application
 *     - infixed value construction
 *   However, op prefixes are kept, since they are required for rebuilding the
 *   syntax tree during fixity resolution.
 *   Optional semicolons are also omitted.
 *)

signature GRAMMAR_CORE =
  sig

    (* Import *)

    type Info

    type SCon           = SCon.SCon
    type Lab            = Lab.Lab
    type VId            = VId.Id
    type TyCon          = TyCon.Id
    type TyVar          = TyVar.TyVar
    type StrId          = StrId.Id
    type longVId        = LongVId.longId
    type longTyCon      = LongTyCon.longId
    type longStrId      = LongStrId.longId


    (* Optional keyword `op' *)

    datatype Op = SANSOp | WITHOp


    (* Expressions [Figures 2 and 4] *)

    datatype AtExp =
          SCONAtExp      of Info * SCon
        | LONGVIDAtExp   of Info * Op * longVId
        | RECORDAtExp    of Info * ExpRow option
        | LETAtExp       of Info * Dec * Exp
        | PARAtExp       of Info * Exp

    and ExpRow =
          ExpRow         of Info * Lab * Exp * ExpRow option

    and Exp =
          ATEXPExp       of Info * AtExp
        | APPExp         of Info * Exp * AtExp
        | TYPEDExp       of Info * Exp * Ty
        | HANDLEExp      of Info * Exp * Match
        | RAISEExp       of Info * Exp
        | FNExp          of Info * Match

    (* Matches [Figures 2 and 4] *)

    and Match =
          Match          of Info * Mrule * Match option

    and Mrule =
          Mrule          of Info * Pat * Exp

    (* Declarations [Figures 2 and 4] *)

    and Dec =
          VALDec         of Info * TyVarseq * ValBind
        | TYPEDec        of Info * TypBind
        | DATATYPEDec    of Info * DatBind
        | REPLICATIONDec of Info * TyCon * longTyCon
        | ABSTYPEDec     of Info * DatBind * Dec
        | EXCEPTIONDec   of Info * ExBind
        | LOCALDec       of Info * Dec * Dec
        | OPENDec        of Info * longStrId list
        | EMPTYDec       of Info
        | SEQDec         of Info * Dec * Dec

    (* Bindings [Figures 2 and 4] *)

    and ValBind =
          PLAINValBind   of Info * Pat * Exp * ValBind option
        | RECValBind     of Info * ValBind

    and TypBind =
          TypBind        of Info * TyVarseq * TyCon * Ty * TypBind option

    and DatBind =
          DatBind        of Info * TyVarseq * TyCon * ConBind * DatBind option

    and ConBind =
          ConBind        of Info * Op * VId * Ty option * ConBind option

    and ExBind =
          NEWExBind      of Info * Op * VId * Ty option * ExBind option
        | EQUALExBind    of Info * Op * VId * Op * longVId * ExBind option

    (* Patterns [Figures 2 and 3] *)

    and AtPat =
          WILDCARDAtPat  of Info
        | SCONAtPat      of Info * SCon
        | LONGVIDAtPat   of Info * Op * longVId
        | RECORDAtPat    of Info * PatRow option
        | PARAtPat       of Info * Pat

    and PatRow =
          WILDCARDPatRow of Info
        | ROWPatRow      of Info * Lab * Pat * PatRow option

    and Pat =
          ATPATPat       of Info * AtPat
        | CONPat         of Info * Op * longVId * AtPat
        | TYPEDPat       of Info * Pat * Ty
        | ASPat          of Info * Op * VId * Ty option * Pat

    (* Type expressions [Figures 2 and 3] *)

    and Ty =
          TYVARTy        of Info * TyVar
        | RECORDTy       of Info * TyRow option
        | TYCONTy        of Info * Tyseq * longTyCon
        | ARROWTy        of Info * Ty * Ty
        | PARTy          of Info * Ty

    and TyRow =
          TyRow          of Info * Lab * Ty * TyRow option

    (* Sequences [Section 2.8] *)

    and Tyseq =
          Tyseq          of Info * Ty list

    and TyVarseq =
          TyVarseq       of Info * TyVar list


    (* Operations *)

    val infoAtExp:      AtExp   -> Info
    val infoExpRow:     ExpRow  -> Info
    val infoExp:        Exp     -> Info
    val infoMatch:      Match   -> Info
    val infoMrule:      Mrule   -> Info
    val infoDec:        Dec     -> Info
    val infoValBind:    ValBind -> Info
    val infoTypBind:    TypBind -> Info
    val infoDatBind:    DatBind -> Info
    val infoConBind:    ConBind -> Info
    val infoExBind:     ExBind  -> Info
    val infoAtPat:      AtPat   -> Info
    val infoPatRow:     PatRow  -> Info
    val infoPat:        Pat     -> Info
    val infoTy:         Ty      -> Info
    val infoTyRow:      TyRow   -> Info
    val infoTyseq:      Tyseq   -> Info
    val infoTyVarseq:   TyVarseq -> Info

  end
(* stop of GRAMMAR_CORE.sml *)
(* start of GrammarCoreFn.sml *)
(*
 * Standard ML abstract core grammar
 *
 * Definition, section 2.8
 *
 * Note:
 *   This is the syntax used in the inference rules for the core [Definition,
 *   sections 4.10 and 6.7]. It omits almost anything having to do with infixed
 *   identifiers:
 *     - fixity directives
 *     - infixed application
 *     - infixed value construction
 *   However, op prefixes are kept, since they are required for rebuilding the
 *   syntax tree during fixity resolution.
 *   Optional semicolons are also omitted.
 *)

functor GrammarCoreFn(type Info) : GRAMMAR_CORE =
  struct

    (* Import *)

    type Info           = Info

    type SCon           = SCon.SCon
    type Lab            = Lab.Lab
    type VId            = VId.Id
    type TyCon          = TyCon.Id
    type TyVar          = TyVar.TyVar
    type StrId          = StrId.Id
    type longVId        = LongVId.longId
    type longTyCon      = LongTyCon.longId
    type longStrId      = LongStrId.longId


    (* Optional keyword `op' *)

    datatype Op = SANSOp | WITHOp


    (* Expressions [Figures 2 and 4] *)

    datatype AtExp =
          SCONAtExp      of Info * SCon
        | LONGVIDAtExp   of Info * Op * longVId
        | RECORDAtExp    of Info * ExpRow option
        | LETAtExp       of Info * Dec * Exp
        | PARAtExp       of Info * Exp

    and ExpRow =
          ExpRow         of Info * Lab * Exp * ExpRow option

    and Exp =
          ATEXPExp       of Info * AtExp
        | APPExp         of Info * Exp * AtExp
        | TYPEDExp       of Info * Exp * Ty
        | HANDLEExp      of Info * Exp * Match
        | RAISEExp       of Info * Exp
        | FNExp          of Info * Match

    (* Matches [Figures 2 and 4] *)

    and Match =
          Match          of Info * Mrule * Match option

    and Mrule =
          Mrule          of Info * Pat * Exp

    (* Declarations [Figures 2 and 4] *)

    and Dec =
          VALDec         of Info * TyVarseq * ValBind
        | TYPEDec        of Info * TypBind
        | DATATYPEDec    of Info * DatBind
        | REPLICATIONDec of Info * TyCon * longTyCon
        | ABSTYPEDec     of Info * DatBind * Dec
        | EXCEPTIONDec   of Info * ExBind
        | LOCALDec       of Info * Dec * Dec
        | OPENDec        of Info * longStrId list
        | EMPTYDec       of Info
        | SEQDec         of Info * Dec * Dec

    (* Bindings [Figures 2 and 4] *)

    and ValBind =
          PLAINValBind   of Info * Pat * Exp * ValBind option
        | RECValBind     of Info * ValBind

    and TypBind =
          TypBind        of Info * TyVarseq * TyCon * Ty * TypBind option

    and DatBind =
          DatBind        of Info * TyVarseq * TyCon * ConBind * DatBind option

    and ConBind =
          ConBind        of Info * Op * VId * Ty option * ConBind option

    and ExBind =
          NEWExBind      of Info * Op * VId * Ty option * ExBind option
        | EQUALExBind    of Info * Op * VId * Op * longVId * ExBind option

    (* Patterns [Figures 2 and 3] *)

    and AtPat =
          WILDCARDAtPat  of Info
        | SCONAtPat      of Info * SCon
        | LONGVIDAtPat   of Info * Op * longVId
        | RECORDAtPat    of Info * PatRow option
        | PARAtPat       of Info * Pat

    and PatRow =
          WILDCARDPatRow of Info
        | ROWPatRow      of Info * Lab * Pat * PatRow option

    and Pat =
          ATPATPat       of Info * AtPat
        | CONPat         of Info * Op * longVId * AtPat
        | TYPEDPat       of Info * Pat * Ty
        | ASPat          of Info * Op * VId * Ty option * Pat

    (* Type expressions [Figures 2 and 3] *)

    and Ty =
          TYVARTy        of Info * TyVar
        | RECORDTy       of Info * TyRow option
        | TYCONTy        of Info * Tyseq * longTyCon
        | ARROWTy        of Info * Ty * Ty
        | PARTy          of Info * Ty

    and TyRow =
          TyRow          of Info * Lab * Ty * TyRow option

    (* Sequences [Section 2.8] *)

    and Tyseq =
          Tyseq          of Info * Ty list

    and TyVarseq =
          TyVarseq       of Info * TyVar list



    (* Extracting info fields *)

    fun infoAtExp(SCONAtExp(I,_))               = I
      | infoAtExp(LONGVIDAtExp(I,_,_))          = I
      | infoAtExp(RECORDAtExp(I,_))             = I
      | infoAtExp(LETAtExp(I,_,_))              = I
      | infoAtExp(PARAtExp(I,_))                = I

    fun infoExpRow(ExpRow(I,_,_,_))             = I

    fun infoExp(ATEXPExp(I,_))                  = I
      | infoExp(APPExp(I,_,_))                  = I
      | infoExp(TYPEDExp(I,_,_))                = I
      | infoExp(HANDLEExp(I,_,_))               = I
      | infoExp(RAISEExp(I,_))                  = I
      | infoExp(FNExp(I,_))                     = I

    fun infoMatch(Match(I,_,_))                 = I

    fun infoMrule(Mrule(I,_,_))                 = I

    fun infoDec(VALDec(I,_,_))                  = I
      | infoDec(TYPEDec(I,_))                   = I
      | infoDec(DATATYPEDec(I,_))               = I
      | infoDec(REPLICATIONDec(I,_,_))          = I
      | infoDec(ABSTYPEDec(I,_,_))              = I
      | infoDec(EXCEPTIONDec(I,_))              = I
      | infoDec(LOCALDec(I,_,_))                = I
      | infoDec(OPENDec(I,_))                   = I
      | infoDec(EMPTYDec(I))                    = I
      | infoDec(SEQDec(I,_,_))                  = I

    fun infoValBind(PLAINValBind(I,_,_,_))      = I
      | infoValBind(RECValBind(I,_))            = I

    fun infoTypBind(TypBind(I,_,_,_,_))         = I

    fun infoDatBind(DatBind(I,_,_,_,_))         = I

    fun infoConBind(ConBind(I,_,_,_,_))         = I

    fun infoExBind(NEWExBind(I,_,_,_,_))        = I
      | infoExBind(EQUALExBind(I,_,_,_,_,_))    = I

    fun infoAtPat(WILDCARDAtPat(I))             = I
      | infoAtPat(SCONAtPat(I,_))               = I
      | infoAtPat(LONGVIDAtPat(I,_,_))          = I
      | infoAtPat(RECORDAtPat(I,_))             = I
      | infoAtPat(PARAtPat(I,_))                = I

    fun infoPatRow(WILDCARDPatRow(I))           = I
      | infoPatRow(ROWPatRow(I,_,_,_))          = I

    fun infoPat(ATPATPat(I,_))                  = I
      | infoPat(CONPat(I,_,_,_))                = I
      | infoPat(TYPEDPat(I,_,_))                = I
      | infoPat(ASPat(I,_,_,_,_))               = I

    fun infoTy(TYVARTy(I,_))                    = I
      | infoTy(RECORDTy(I,_))                   = I
      | infoTy(TYCONTy(I,_,_))                  = I
      | infoTy(ARROWTy(I,_,_))                  = I
      | infoTy(PARTy(I,_))                      = I

    fun infoTyRow(TyRow(I,_,_,_))               = I

    fun infoTyseq(Tyseq(I,_))                   = I
    fun infoTyVarseq(TyVarseq(I,_))             = I

  end
(* stop of GrammarCoreFn.sml *)
(* start of SOURCE.sml *)
(*
 * Helpers for handling source strings
 *)

signature SOURCE =
  sig

    type source = string
    type region = int * int

    val over:     region * region -> region
    val between:  region * region -> region

    val compare:  region * region -> order

  end
(* stop of SOURCE.sml *)
(* start of Source.sml *)
(*
 * Helpers for handling source strings
 *)

structure Source :> SOURCE =
  struct

    type source = string
    type region = int * int

    fun over(r1: region, r2: region)    = (#1 r1, #2 r2)
    fun between(r1: region, r2: region) = (#2 r1, #1 r2)

    fun compare((m1,n1), (m2,n2)) =
        case Int.compare(m1, m2)
          of EQUAL => Int.compare(n1, n2)
           | order => order

  end
(* stop of Source.sml *)
(* start of GRAMMAR_MODULE.sml *)
(*
 * Standard ML abstract module grammar
 *
 * Definition, section 3.4
 *
 * Notes:
 *   This is the syntax used in the inference rules for modules [Definition,
 *   sections 5.7 and 7.3]. Optional semicolons are omitted.
 *   The structure sharing derived form [Definition, Appendix A] has been added,
 *   because it cannot be derived purely syntactically.
 *)


signature GRAMMAR_MODULE =
  sig

    (* Import *)

    structure Core: GRAMMAR_CORE

    type Info

    type VId            = Core.VId
    type TyCon          = Core.TyCon
    type TyVar          = Core.TyVar
    type StrId          = Core.StrId
    type longVId        = Core.longVId
    type longTyCon      = Core.longTyCon
    type longStrId      = Core.longStrId
    type Dec            = Core.Dec
    type Ty             = Core.Ty
    type TyVarseq       = Core.TyVarseq

    type SigId          = SigId.Id
    type FunId          = FunId.Id


    (* Structures [Figures 5 and 6] *)

    datatype StrExp =
          STRUCTStrExp    of Info * StrDec
        | LONGSTRIDStrExp of Info * longStrId
        | TRANSStrExp     of Info * StrExp * SigExp
        | OPAQStrExp      of Info * StrExp * SigExp
        | APPStrExp       of Info * FunId * StrExp
        | LETStrExp       of Info * StrDec * StrExp

    and StrDec =
          DECStrDec       of Info * Dec
        | STRUCTUREStrDec of Info * StrBind
        | LOCALStrDec     of Info * StrDec * StrDec
        | EMPTYStrDec     of Info
        | SEQStrDec       of Info * StrDec * StrDec

    and StrBind =
          StrBind         of Info * StrId * StrExp * StrBind option

    (* Signatures [Figures 5 and 6] *)

    and SigExp =
          SIGSigExp       of Info * Spec
        | SIGIDSigExp     of Info * SigId
        | WHERETYPESigExp of Info * SigExp * TyVarseq * longTyCon * Ty

    and SigDec =
          SigDec          of Info * SigBind

    and SigBind =
          SigBind         of Info * SigId * SigExp * SigBind option

    (* Specifications [Figures 5 and 7] *)

    and Spec =
          VALSpec         of Info * ValDesc
        | TYPESpec        of Info * TypDesc
        | EQTYPESpec      of Info * TypDesc
        | DATATYPESpec    of Info * DatDesc
        | REPLICATIONSpec of Info * TyCon * longTyCon
        | EXCEPTIONSpec   of Info * ExDesc
        | STRUCTURESpec   of Info * StrDesc
        | INCLUDESpec     of Info * SigExp
        | EMPTYSpec       of Info
        | SEQSpec         of Info * Spec * Spec
        | SHARINGTYPESpec of Info * Spec * longTyCon list
        | SHARINGSpec     of Info * Spec * longStrId list

    and ValDesc =
          ValDesc         of Info * VId * Ty * ValDesc option

    and TypDesc =
          TypDesc         of Info * TyVarseq * TyCon * TypDesc option

    and DatDesc =
          DatDesc         of Info * TyVarseq * TyCon * ConDesc * DatDesc option

    and ConDesc =
          ConDesc         of Info * VId * Ty option * ConDesc option

    and ExDesc =
          ExDesc          of Info * VId * Ty option * ExDesc option

    and StrDesc =
          StrDesc         of Info * StrId * SigExp * StrDesc option

    (* Functors [Figures 5 and 8] *)

    and FunDec =
          FunDec          of Info * FunBind

    and FunBind =
          FunBind         of Info * FunId * StrId * SigExp * StrExp
                                  * FunBind option

    (* Top-level declarations [Figures 5 and 8] *)

    and TopDec =
          STRDECTopDec    of Info * StrDec * TopDec option
        | SIGDECTopDec    of Info * SigDec * TopDec option
        | FUNDECTopDec    of Info * FunDec * TopDec option


    (* Operations *)

    val infoStrExp:     StrExp  -> Info
    val infoStrDec:     StrDec  -> Info
    val infoStrBind:    StrBind -> Info
    val infoSigExp:     SigExp  -> Info
    val infoSigBind:    SigBind -> Info
    val infoSpec:       Spec    -> Info
    val infoValDesc:    ValDesc -> Info
    val infoTypDesc:    TypDesc -> Info
    val infoDatDesc:    DatDesc -> Info
    val infoConDesc:    ConDesc -> Info
    val infoExDesc:     ExDesc  -> Info
    val infoStrDesc:    StrDesc -> Info
    val infoFunDec:     FunDec  -> Info
    val infoFunBind:    FunBind -> Info
    val infoTopDec:     TopDec  -> Info

  end
(* stop of GRAMMAR_MODULE.sml *)
(* start of GrammarModuleFn.sml *)
(*
 * Standard ML abstract module grammar
 *
 * Definition, section 3.4
 *
 * Notes:
 *   This is the syntax used in the inference rules for modules [Definition,
 *   sections 5.7 and 7.3]. Optional semicolons are omitted.
 *   The structure sharing derived form [Definition, Appendix A] has been added,
 *   because it cannot be derived purely syntactically.
 *)


functor GrammarModuleFn(type Info
                        structure Core: GRAMMAR_CORE
                       ) : GRAMMAR_MODULE =
  struct

    (* Import *)

    structure Core = Core
    type      Info = Info

    open Core

    type SigId = SigId.Id
    type FunId = FunId.Id


    (* Structures [Figures 5 and 6] *)

    datatype StrExp =
          STRUCTStrExp    of Info * StrDec
        | LONGSTRIDStrExp of Info * longStrId
        | TRANSStrExp     of Info * StrExp * SigExp
        | OPAQStrExp      of Info * StrExp * SigExp
        | APPStrExp       of Info * FunId * StrExp
        | LETStrExp       of Info * StrDec * StrExp

    and StrDec =
          DECStrDec       of Info * Dec
        | STRUCTUREStrDec of Info * StrBind
        | LOCALStrDec     of Info * StrDec * StrDec
        | EMPTYStrDec     of Info
        | SEQStrDec       of Info * StrDec * StrDec

    and StrBind =
          StrBind         of Info * StrId * StrExp * StrBind option

    (* Signatures [Figures 5 and 6] *)

    and SigExp =
          SIGSigExp       of Info * Spec
        | SIGIDSigExp     of Info * SigId
        | WHERETYPESigExp of Info * SigExp * TyVarseq * longTyCon * Ty

    and SigDec =
          SigDec          of Info * SigBind

    and SigBind =
          SigBind         of Info * SigId * SigExp * SigBind option

    (* Specifications [Figures 5 and 7] *)

    and Spec =
          VALSpec         of Info * ValDesc
        | TYPESpec        of Info * TypDesc
        | EQTYPESpec      of Info * TypDesc
        | DATATYPESpec    of Info * DatDesc
        | REPLICATIONSpec of Info * TyCon * longTyCon
        | EXCEPTIONSpec   of Info * ExDesc
        | STRUCTURESpec   of Info * StrDesc
        | INCLUDESpec     of Info * SigExp
        | EMPTYSpec       of Info
        | SEQSpec         of Info * Spec * Spec
        | SHARINGTYPESpec of Info * Spec * longTyCon list
        | SHARINGSpec     of Info * Spec * longStrId list

    and ValDesc =
          ValDesc         of Info * VId * Ty * ValDesc option

    and TypDesc =
          TypDesc         of Info * TyVarseq * TyCon * TypDesc option

    and DatDesc =
          DatDesc         of Info * TyVarseq * TyCon * ConDesc * DatDesc option

    and ConDesc =
          ConDesc         of Info * VId * Ty option * ConDesc option

    and ExDesc =
          ExDesc          of Info * VId * Ty option * ExDesc option

    and StrDesc =
          StrDesc         of Info * StrId * SigExp * StrDesc option

    (* Functors [Figures 5 and 8] *)

    and FunDec =
          FunDec          of Info * FunBind

    and FunBind =
          FunBind         of Info * FunId * StrId * SigExp * StrExp
                                  * FunBind option

    (* Top-level declarations [Figures 5 and 8] *)

    and TopDec =
          STRDECTopDec    of Info * StrDec * TopDec option
        | SIGDECTopDec    of Info * SigDec * TopDec option
        | FUNDECTopDec    of Info * FunDec * TopDec option


    (* Extracting info fields *)

    fun infoStrExp(STRUCTStrExp(I,_))           = I
      | infoStrExp(LONGSTRIDStrExp(I,_))        = I
      | infoStrExp(TRANSStrExp(I,_,_))          = I
      | infoStrExp(OPAQStrExp(I,_,_))           = I
      | infoStrExp(APPStrExp(I,_,_))            = I
      | infoStrExp(LETStrExp(I,_,_))            = I

    fun infoStrDec(DECStrDec(I,_))              = I
      | infoStrDec(STRUCTUREStrDec(I,_))        = I
      | infoStrDec(LOCALStrDec(I,_,_))          = I
      | infoStrDec(EMPTYStrDec(I))              = I
      | infoStrDec(SEQStrDec(I,_,_))            = I

    fun infoStrBind(StrBind(I,_,_,_))           = I

    fun infoSigExp(SIGSigExp(I,_))              = I
      | infoSigExp(SIGIDSigExp(I,_))            = I
      | infoSigExp(WHERETYPESigExp(I,_,_,_,_))  = I

    fun infoSigDec(SigDec(I,_))                 = I

    fun infoSigBind(SigBind(I,_,_,_))           = I

    fun infoSpec(VALSpec(I,_))                  = I
      | infoSpec(TYPESpec(I,_))                 = I
      | infoSpec(EQTYPESpec(I,_))               = I
      | infoSpec(DATATYPESpec(I,_))             = I
      | infoSpec(REPLICATIONSpec(I,_,_))        = I
      | infoSpec(EXCEPTIONSpec(I,_))            = I
      | infoSpec(STRUCTURESpec(I,_))            = I
      | infoSpec(INCLUDESpec(I,_))              = I
      | infoSpec(EMPTYSpec(I))                  = I
      | infoSpec(SEQSpec(I,_,_))                = I
      | infoSpec(SHARINGTYPESpec(I,_,_))        = I
      | infoSpec(SHARINGSpec(I,_,_))            = I

    fun infoValDesc(ValDesc(I,_,_,_))           = I
    fun infoTypDesc(TypDesc(I,_,_,_))           = I
    fun infoDatDesc(DatDesc(I,_,_,_,_))         = I
    fun infoConDesc(ConDesc(I,_,_,_))           = I
    fun infoExDesc(ExDesc(I,_,_,_))             = I
    fun infoStrDesc(StrDesc(I,_,_,_))           = I

    fun infoFunDec(FunDec(I,_))                 = I

    fun infoFunBind(FunBind(I,_,_,_,_,_))       = I

    fun infoTopDec(STRDECTopDec(I,_,_))         = I
      | infoTopDec(SIGDECTopDec(I,_,_))         = I
      | infoTopDec(FUNDECTopDec(I,_,_))         = I

  end
(* stop of GrammarModuleFn.sml *)
(* start of GRAMMAR_PROGRAM.sml *)
(*
 * Standard ML abstract program grammar
 *
 * Definition, section 8
 *)


signature GRAMMAR_PROGRAM =
  sig

    (* Import *)

    structure Module: GRAMMAR_MODULE

    type Info   = Module.Info

    type TopDec = Module.TopDec


    (* Programs *)

    datatype Program = Program of Info * TopDec * Program option


    (* Extracting the info field *)

    val infoProgram: Program -> Info

  end
(* stop of GRAMMAR_PROGRAM.sml *)
(* start of GrammarProgramFn.sml *)
(*
 * Standard ML abstract program grammar
 *
 * Definition, section 8
 *)


functor GrammarProgramFn(type Info
                         structure Module: GRAMMAR_MODULE
                        ) : GRAMMAR_PROGRAM =
  struct

    (* Import *)

    structure Module = Module
    type      Info   = Info

    open Module


    (* Programs *)

    datatype Program = Program of Info * TopDec * Program option


    (* Extracting the info field *)

    fun infoProgram(Program(I,_,_)) = I

  end
(* stop of GrammarProgramFn.sml *)
(* start of Grammars.sml *)
structure GrammarCore    = GrammarCoreFn(type Info = Source.region)

structure GrammarModule  = GrammarModuleFn(type Info = Source.region
                                           structure Core = GrammarCore)

structure GrammarProgram = GrammarProgramFn(type Info = Source.region
                                            structure Module = GrammarModule)
(* stop of Grammars.sml *)
(* start of DYNAMIC_ENV.sml *)
(*
 * Standard ML environments of the dynamic semantics of the core
 *
 * Definition, sections 6.3 and 6.6
 *
 * Notes:
 *   - We call the domain type of value environments ValStr.
 *   - The type definitions here are heavily recursive and it is really stupid
 *     that SML allows no withtype in signatures...
 *)

signature DYNAMIC_ENV =
  sig

    (* Inheritance *)

    include GENERIC_ENV


    (* Import types *)

    type 'a Val = 'a Val.Val
    type Match  = GrammarCore.Match


    (* Export types [Section 6.6] *)

    datatype FcnClosure =
             FcnClosure of Match
                        * ( (*Env*)
                            ( FcnClosure Val * IdStatus
                            , (FcnClosure Val * IdStatus) VIdMap
                            ) Str' StrIdMap
                          * (FcnClosure Val * IdStatus) VIdMap TyConMap 
                          * (FcnClosure Val * IdStatus) VIdMap
                          )
                        * (*ValEnv*) (FcnClosure Val * IdStatus) VIdMap

    type ValStr = FcnClosure Val * IdStatus
    type ValEnv = ValStr VIdMap                         (* [VE] *)

    type TyStr  = ValEnv
    type TyEnv  = TyStr TyConMap                        (* [TE] *)

    type Str    = (ValStr, TyStr) Str'
    type StrEnv = Str StrIdMap                          (* [SE] *)

    type Env    = StrEnv * TyEnv * ValEnv               (* [E] *)


    (* Operations *)

    val Rec:    ValEnv -> ValEnv

  end
(* stop of DYNAMIC_ENV.sml *)
(* start of DynamicEnv.sml *)
(*
 * Standard ML environments of the dynamic semantics of the core
 *
 * Definition, sections 6.3 and 6.6
 *
 * Notes:
 *   - We call the domain type of value environments ValStr.
 *   - The type definitions here are heavily recursive. It would be easier if
 *     SML would define withtype sequentially (as SML/NJ implements it).
 *     However, it is still better than inside the signature...
 *)

structure DynamicEnv :> DYNAMIC_ENV =
  struct

    (* Inheritance *)

    structure GenericEnv = GenericEnvFn()

    open GenericEnv


    (* Import types *)

    type 'a Val = 'a Val.Val
    type Match  = GrammarCore.Match


    (* Export types [Section 6.6] *)

    datatype FcnClosure =
             FcnClosure of Match * ((*Env*) StrEnv * TyEnv * ValEnv) * ValEnv

    withtype ValEnv = (FcnClosure Val * IdStatus) VIdMap          (* [VE] *)
    and      TyEnv  = (FcnClosure Val * IdStatus) VIdMap TyConMap (* [TE] *)
    and      StrEnv = (FcnClosure Val * IdStatus,
                       (FcnClosure Val * IdStatus) VIdMap) Str' StrIdMap
                                                                  (* [SE] *)
    type ValStr = FcnClosure Val * IdStatus
    type TyStr  = ValEnv
    type Str    = (ValStr, TyStr) Str'

    type Env    = StrEnv * TyEnv * ValEnv                         (* [E] *)


    (* Unrolling [Section 6.6] *)

    fun Rec VE =
            VIdMap.map
                (fn (Val.FcnClosure(FcnClosure(match',E',VE')), IdStatus.v) =>
                    (Val.FcnClosure(FcnClosure(match',E',VE)), IdStatus.v)
                  | valstr => valstr
                ) VE

  end
(* stop of DynamicEnv.sml *)
(* start of INITIAL_DYNAMIC_ENV.sml *)
(*
 * Standard ML core view of the initial dynamic basis
 *
 * Definition, appendix D and section 6.5
 *
 * Note:
 *     The Definition does not specify what the initial state has to contain.
 *     This is a bug as it must at least contain the exception names Match
 *     and Bind. We put the state associated with the initial environment in
 *     here, too.
 *)

signature INITIAL_DYNAMIC_ENV =
  sig

    (* Import types *)

    type Env    = DynamicEnv.Env
    type ExName = ExName.ExName
    type State  = DynamicEnv.FcnClosure State.State


    (* Basic exception names [Section 6.5] *)

    val enMatch: ExName
    val enBind:  ExName

    (* Initial environment [Appendix D] *)

    val E0: Env

    (* Associated state *)

    val s: State

  end
(* stop of INITIAL_DYNAMIC_ENV.sml *)
(* start of InitialDynamicEnv.sml *)
(*
 * Standard ML core view of the initial dynamic basis
 *
 * Definition, appendix D and section 6.5
 *
 * Note:
 *     The Definition does not specify what the initial state has to contain.
 *     This is a bug as it must at least contain the exception names Match
 *     and Bind. We put the state associated with the initial environment in
 *     here, too.
 *)

structure InitialDynamicEnv :> INITIAL_DYNAMIC_ENV =
  struct

    (* Import *)

    type Env    = DynamicEnv.Env
    type ValEnv = DynamicEnv.ValEnv
    type ExName = ExName.ExName
    type State  = DynamicEnv.FcnClosure State.State


    open Val
    open IdStatus


    (* VIds [Appendix D] *)

    val vidEq     = VId.fromString "="
    val vidAssign = VId.fromString ":="

    val vidFalse  = VId.fromString "false"
    val vidTrue   = VId.fromString "true"
    val vidNil    = VId.fromString "nil"
    val vidCons   = VId.fromString "::"
    val vidRef    = VId.fromString "ref"

    val vidMatch  = VId.fromString "Match"
    val vidBind   = VId.fromString "Bind"


    (* Basic exception names [Section 6.5] *)

    val enMatch = ExName.exname vidMatch
    val enBind  = ExName.exname vidBind


    (* Value entries [Appendix D] *)

    val valstrEq     = (BasVal "=", v)
    val valstrAssign = (op:=,       v)

    val valstrFalse  = (VId vidFalse, c)
    val valstrTrue   = (VId vidTrue,  c)
    val valstrNil    = (VId vidNil,   c)
    val valstrCons   = (VId vidCons,  c)
    val valstrRef    = (VId vidRef,   c)

    val valstrMatch  = (ExVal(ExName enMatch), e)
    val valstrBind   = (ExVal(ExName enBind),  e)


    (* TyCons [Figure 26] *)

    val tyconUnit   = TyCon.fromString "unit"
    val tyconBool   = TyCon.fromString "bool"
    val tyconInt    = TyCon.fromString "int"
    val tyconWord   = TyCon.fromString "word"
    val tyconReal   = TyCon.fromString "real"
    val tyconString = TyCon.fromString "string"
    val tyconChar   = TyCon.fromString "char"
    val tyconList   = TyCon.fromString "list"
    val tyconRef    = TyCon.fromString "ref"
    val tyconExn    = TyCon.fromString "exn"


    (* Type ValEnvs [Figure 26] *)

    val VEUnit   = VIdMap.empty
    val VEBool   = VIdMap.fromList[(vidFalse, valstrFalse),
                                   (vidTrue,  valstrTrue)] : ValEnv
    val VEInt    = VIdMap.empty
    val VEWord   = VIdMap.empty
    val VEReal   = VIdMap.empty
    val VEString = VIdMap.empty
    val VEChar   = VIdMap.empty
    val VEList   = VIdMap.fromList[(vidNil,   valstrNil),
                                   (vidCons,  valstrCons)] : ValEnv
    val VERef    = VIdMap.fromList[(vidRef,   valstrRef)] : ValEnv
    val VEExn    = VIdMap.empty


    (* Environments [Appendix D] *)

    val SE0 = StrIdMap.empty

    val TE0 = TyConMap.fromList[(tyconUnit,   VEUnit),
                                (tyconBool,   VEBool),
                                (tyconInt,    VEInt),
                                (tyconWord,   VEWord),
                                (tyconReal,   VEReal),
                                (tyconString, VEString),
                                (tyconChar,   VEChar),
                                (tyconList,   VEList),
                                (tyconRef,    VERef),
                                (tyconExn,    VEExn)]

    val VE0 = VIdMap.fromList  [(vidEq,     valstrEq),
                                (vidAssign, valstrAssign),
                                (vidRef,    valstrRef),
                                (vidNil,    valstrNil),
                                (vidCons,   valstrCons),
                                (vidFalse,  valstrFalse),
                                (vidTrue,   valstrTrue),
                                (vidMatch,  valstrMatch),
                                (vidBind,   valstrBind)] : ValEnv

    val E0 = (SE0,TE0,VE0)


    (* Associated state *)

    val mem = AddrMap.empty
    val ens = ExNameSet.fromList[enMatch, enBind]

    val s = (mem, ens)

  end
(* stop of InitialDynamicEnv.sml *)
(* start of INTERFACE.sml *)
(*
 * Standard ML interfaces
 *
 * Definition, section 7.2
 *)

signature INTERFACE =
  sig

    (* Inheritance *)

    include GENERIC_ENV


    (* Import *)

    type Env = DynamicEnv.Env


    (* Export types [Section 7.2] *)

    type ValInt = IdStatus VIdMap                               (* [VI] *)
    type TyInt  = ValInt TyConMap                               (* [TI] *)

    type Str    = (IdStatus, ValInt) Str'
    type StrInt = Str StrIdMap                                  (* [SI] *)
 
    type Int    = StrInt * TyInt * ValInt                       (* [I] *)


    (* Operations *)

    val fromSI:         StrInt -> Int
    val fromTI:         TyInt  -> Int
    val fromVI:         ValInt -> Int
    val fromVIandTI:    ValInt * TyInt -> Int

    val Inter:          Env -> Int
    val cutdown:        Env * Int -> Env

  end
(* stop of INTERFACE.sml *)
(* start of Interface.sml *)
(*
 * Standard ML interfaces
 *
 * Definition, section 7.2
 *)


structure Interface :> INTERFACE =
  struct

    (* Inheritance *)

    structure GenericEnv = GenericEnvFn()

    open GenericEnv


    (* Import *)

    type Env = DynamicEnv.Env


    (* Export types [Section 7.2] *)

    type ValInt = IdStatus VIdMap                               (* [VI] *)
    type TyInt  = ValInt TyConMap                               (* [TI] *)

    type Str    = (IdStatus, ValInt) Str'
    type StrInt = Str StrIdMap                                  (* [SI] *)
 
    type Int    = StrInt * TyInt * ValInt                       (* [I] *)


    (* Injections [Section 4.3] *)

    val fromSI      = fromSE
    val fromTI      = fromTE
    val fromVI      = fromVE
    val fromVIandTI = fromVEandTE


    (* Extracting interfaces from environments [Section 7.2] *)

    fun InterVE VE = VIdMap.map (fn(v,is) => is) VE
    fun InterTE TE = TyConMap.map (fn VE => InterVE VE) TE
    fun InterSE SE = StrIdMap.map (fn DynamicEnv.Str E => Str(Inter E)) SE

    and Inter (SE,TE,VE) = (InterSE SE, InterTE TE, InterVE VE)


    (* Cutting down environments [Section 7.2] *)

    fun cutdownVE(VE, VI) =
        VIdMap.foldli
            (fn(vid, is, VE') =>
                case VIdMap.find(VE, vid)
                  of SOME(v,is') => VIdMap.insert(VE', vid, (v,is))
                   | NONE        => VE'
            ) VIdMap.empty VI

    fun cutdownTE(TE, TI) =
        TyConMap.foldli
            (fn(tycon, VI', TE') =>
                case TyConMap.find(TE, tycon)
                  of SOME VE' => TyConMap.insert(TE', tycon, cutdownVE(VE',VI'))
                   | NONE     => TE'
            ) TyConMap.empty TI

    fun cutdownSE(SE, SI) =
        StrIdMap.foldli
            (fn(strid, Str I, SE') =>
                case StrIdMap.find(SE, strid)
                  of SOME(DynamicEnv.Str E) =>
                       StrIdMap.insert(SE', strid, DynamicEnv.Str(cutdown(E,I)))
                   | NONE => SE'
            ) StrIdMap.empty SI

    and cutdown((SE,TE,VE), (SI,TI,VI)) =
        ( cutdownSE(SE, SI), cutdownTE(TE, TI), cutdownVE(VE, VI) )

  end
(* stop of Interface.sml *)
(* start of DYNAMIC_BASIS.sml *)
(*
 * Standard ML dynamic basis and environments of modules
 *
 * Definition, section 7.2
 *)

signature DYNAMIC_BASIS =
  sig

    (* Import types *)

    type StrId       = StrId.Id
    type SigId       = SigId.Id
    type FunId       = FunId.Id
    type longStrId   = LongStrId.longId
    type longTyCon   = LongTyCon.longId
    type Env         = DynamicEnv.Env
    type ValEnv      = DynamicEnv.ValEnv
    type StrEnv      = DynamicEnv.StrEnv
    type Str         = DynamicEnv.Str
    type Int         = Interface.Int
    type StrExp      = GrammarModule.StrExp

    type 'a SigIdMap = 'a SigIdMap.map
    type 'a FunIdMap = 'a FunIdMap.map


    (* Types [Section 7.2] *)

    datatype FunctorClosure =
        FunctorClosure of (StrId * Int) * StrExp *
                        (*Basis*) (FunctorClosure FunIdMap * Int SigIdMap * Env)

    type SigEnv   = Int SigIdMap                                (* [G] *)
    type FunEnv   = FunctorClosure FunIdMap                     (* [F] *)

    type Basis    = FunEnv * SigEnv * Env                       (* [B] *)


    (* Operations *)

    val empty:          Basis
    val fromE:          Env    -> Basis
    val fromF:          FunEnv -> Basis
    val fromG:          SigEnv -> Basis

    val Eof:            Basis    -> Env

    val plus:           Basis * Basis     -> Basis
    val plusSE:         Basis * StrEnv    -> Basis
    val plusG:          Basis * SigEnv    -> Basis
    val plusF:          Basis * FunEnv    -> Basis
    val plusE:          Basis * Env       -> Basis

    val findStrId:      Basis * StrId     -> Str option
    val findSigId:      Basis * SigId     -> Int option
    val findFunId:      Basis * FunId     -> FunctorClosure option
    val findLongStrId:  Basis * longStrId -> Str option
    val findLongTyCon:  Basis * longTyCon -> ValEnv option
 
  end
(* stop of DYNAMIC_BASIS.sml *)
(* start of DynamicBasis.sml *)
(*
 * Standard ML dynamic basis and environments of modules
 *
 * Definition, section 7.2
 *)


structure DynamicBasis :> DYNAMIC_BASIS =
  struct

    (* Import types *)

    type StrId       = StrId.Id
    type SigId       = SigId.Id
    type FunId       = FunId.Id
    type longStrId   = LongStrId.longId
    type longTyCon   = LongTyCon.longId
    type Env         = DynamicEnv.Env
    type ValEnv      = DynamicEnv.ValEnv
    type StrEnv      = DynamicEnv.StrEnv
    type Str         = DynamicEnv.Str
    type Int         = Interface.Int
    type StrExp      = GrammarModule.StrExp

    type 'a SigIdMap = 'a SigIdMap.map
    type 'a FunIdMap = 'a FunIdMap.map


    (* Types [Section 7.2] *)

    datatype FunctorClosure =
        FunctorClosure of (StrId * Int) * StrExp *
                          (*Basis*) (FunEnv * SigEnv * Env)

    withtype SigEnv   = Int SigIdMap                            (* [G] *)
    and      FunEnv   = FunctorClosure FunIdMap                 (* [F] *)

    type     Basis    = FunEnv * SigEnv * Env                   (* [B] *)



    (* Injections [Sections 4.3 and 7.2] *)

    val empty = ( FunIdMap.empty, SigIdMap.empty, DynamicEnv.empty )

    fun fromE E = ( FunIdMap.empty, SigIdMap.empty, E )
    fun fromF F = ( F, SigIdMap.empty, DynamicEnv.empty )
    fun fromG G = ( FunIdMap.empty, G, DynamicEnv.empty )


    (* Injections [Sections 4.3 and 7.2] *)

    fun Eof (F,G,E) = E


    (* Modifications [Sections 4.3 and 7.2] *)

    infix plus plusG plusF plusE plusSE IBplusI

    fun (F,G,E) plus (F',G',E') =
        ( FunIdMap.unionWith #2 (F,F')
        , SigIdMap.unionWith #2 (G,G')
        , DynamicEnv.plus(E,E')
        )

    fun (F,G,E) plusG  G' = ( F, SigIdMap.unionWith #2 (G,G'), E )
    fun (F,G,E) plusF  F' = ( FunIdMap.unionWith #2 (F,F'), G, E )
    fun (F,G,E) plusE  E' = ( F, G, DynamicEnv.plus(E,E') )
    fun (F,G,E) plusSE SE = ( F, G, DynamicEnv.plusSE(E,SE) )


    (* Application (lookup) [Sections 7.2 and 4.3] *)

    fun findStrId((F,G,E), strid) = DynamicEnv.findStrId(E, strid)
    fun findSigId((F,G,E), sigid) = SigIdMap.find(G, sigid)
    fun findFunId((F,G,E), funid) = FunIdMap.find(F, funid)
    fun findLongStrId((F,G,E), longstrid) =
        DynamicEnv.findLongStrId(E, longstrid)
    fun findLongTyCon((F,G,E), longtycon) =
        DynamicEnv.findLongTyCon(E, longtycon)

  end
(* stop of DynamicBasis.sml *)
(* start of INITIAL_DYNAMIC_BASIS.sml *)
(*
 * Standard ML initial dynamic basis
 *
 * Definition, appendix D
 *
 * Note:
 *     The Definition does not specify what the initial state has to contain.
 *     This is a bug as it must at least contain the exception names Match
 *     and Bind. We put the state associated with the initial basis in
 *     here, too.
 *)

signature INITIAL_DYNAMIC_BASIS =
  sig

    (* Import *)

    type Basis = DynamicBasis.Basis
    type State = InitialDynamicEnv.State


    (* Export *)

    val B0: Basis
    val s:  State

  end
(* stop of INITIAL_DYNAMIC_BASIS.sml *)
(* start of InitialDynamicBasis.sml *)
(*
 * Standard ML initial dynamic basis
 *
 * Definition, appendix D
 *
 * Note:
 *     The Definition does not specify what the initial state has to contain.
 *     This is a bug as it must at least contain the exception names Match
 *     and Bind. We put the state associated with the initial basis in
 *     here, too.
 *)

structure InitialDynamicBasis :> INITIAL_DYNAMIC_BASIS =
  struct

    (* Import *)

    type Basis = DynamicBasis.Basis
    type State = InitialDynamicEnv.State


    (* Enviornments *)

    val F0 = FunIdMap.empty
    val G0 = SigIdMap.empty
    val E0 = InitialDynamicEnv.E0

    val B0 = (F0,G0,E0)


    (* Associated state *)

    val s = InitialDynamicEnv.s

  end
(* stop of InitialDynamicBasis.sml *)
(* start of ERROR.sml *)
(*
 * Error handling.
 *)


signature ERROR =
  sig

    (* Import *)

    type position = Source.region


    (* Export *)

    exception Error of position * string

    val error:    position * string -> 'a
    val warning:  position * string -> unit

  end
(* stop of ERROR.sml *)
(* start of Error.sml *)
(*
 * Error handling.
 *)


structure Error :> ERROR =
  struct

    (* Import *)

    type position = Source.region


    (* Helper *)

    fun print((pos1,pos2), message) =
        let
            val a = Int.toString pos1
            val b = Int.toString pos2
        in
            TextIO.output(TextIO.stdErr, a ^ "-" ^ b ^ ": " ^ message ^ "\n")
          ; TextIO.flushOut TextIO.stdErr
        end


    (* Export *)

    exception Error of position * string

    fun error(pos, message) =
        ( print(pos, message)
        ; raise Error(pos, message)
        )

    fun warning(pos, message) =
        print(pos, "warning: " ^ message)

  end
(* stop of Error.sml *)
(* start of INFIX.sml *)
(*
 * Standard ML infix resolution
 *
 * Definition, section 2.6
 *)


signature INFIX =
  sig

    (* Import *)

    type Info    = GrammarCore.Info

    type Op      = GrammarCore.Op
    type VId     = GrammarCore.VId
    type longVId = GrammarCore.longVId
    type Exp     = GrammarCore.Exp
    type Pat     = GrammarCore.Pat
    type AtExp   = GrammarCore.AtExp
    type AtPat   = GrammarCore.AtPat


    (* Modifying fixity status *)

    datatype Assoc = LEFT | RIGHT

    type InfStatus = Assoc * int
    type InfEnv    = InfStatus VIdMap.map       (* [J] *)

    val empty:          InfEnv
    val assign:         InfEnv * VId list * InfStatus -> InfEnv
    val cancel:         InfEnv * VId list -> InfEnv


    (* Resolving phrases containing infixed identifiers *)

    val parseExp:       InfEnv * AtExp list -> Exp
    val parsePat:       InfEnv * AtPat list -> Pat
    val parseFmrule:    InfEnv * AtPat list -> Op * VId * AtPat list

  end
(* stop of INFIX.sml *)
(* start of Infix.sml *)
(*
 * Standard ML infix resolution
 *
 * Definition, section 2.6
 *)


structure Infix :> INFIX =
  struct

    (* Import *)

    open GrammarCore


    (* Type definitions *)

    datatype Assoc = LEFT | RIGHT

    type InfStatus = Assoc * int

    type InfEnv    = InfStatus VIdMap.map               (* [J] *)



    (* Modifying infix environments *)

    val empty = VIdMap.empty

    fun assign(J, vids, infstatus) =
        let
            fun insert(vid, J) = VIdMap.insert(J, vid, infstatus)
        in
            List.foldl insert J vids
        end

    fun cancel(J, vids) =
        let
            fun remove(vid, J) = #1(VIdMap.remove(J, vid))
        in
            List.foldl remove J vids
        end



    (* Helpers for error messages *)

    val error                           = Error.error
    fun errorVId(I, s, vid)             = error(I, s ^ VId.toString vid)
    fun errorLongVId(I, s, longvid)     = error(I, s ^ LongVId.toString longvid)



    (* Categorisation of atomic expressions and patterns *)

    datatype 'a FixityCategory = NONFIX of 'a
                               | INFIX  of InfStatus * VId * Info

    fun isInfix J (longvid) =
        LongVId.isUnqualified longvid andalso
        VIdMap.find(J, LongVId.toId longvid) <> NONE

    fun categoriseLongVId J (atomic, I, longvid) =
        if LongVId.isUnqualified longvid then
            let
                val vid = LongVId.toId longvid
            in
                case VIdMap.find(J, vid)
                  of NONE           => NONFIX(atomic)
                   | SOME infstatus => INFIX(infstatus, vid, I)
            end
        else
            NONFIX(atomic)

    fun categoriseAtExp J (atexp as LONGVIDAtExp(I, SANSOp, longvid)) =
            categoriseLongVId J (atexp, I, longvid)
      | categoriseAtExp J (atexp) = NONFIX(atexp)

    fun categoriseAtPat J (atpat as LONGVIDAtPat(I, SANSOp, longvid)) =
            categoriseLongVId J (atpat, I, longvid)
      | categoriseAtPat J (atpat) = NONFIX(atpat)



    (* Resolving infixing [Section 2.6] *)

    fun parse(app, infapp, es) =
        let
            fun loop(NONFIX(e)::[], []) = e

              | loop(NONFIX(e2)::NONFIX(e1)::s', i) =
                    (* reduce nonfix application *)
                    loop(NONFIX(app(e1, e2))::s', i)

              | loop(s, NONFIX(e)::i') =
                    (* shift *)
                    loop(NONFIX(e)::s, i')

              | loop(s as NONFIX(e)::[], INFIX(x)::i') =
                    (* shift *)
                    loop(INFIX(x)::s, i')

              | loop(NONFIX(e2)::INFIX(_,vid,_)::NONFIX(e1)::s', []) =
                    (* reduce infix application *)
                    loop(NONFIX(infapp(e1, vid, e2))::s', [])

              | loop(s as NONFIX(e2)::INFIX((a1,p1),vid1,I1)::NONFIX(e1)::s',
                       i as INFIX(x2 as ((a2,p2),vid2,I2))::i') =
                if p1 > p2 then
                    (* reduce infix application *)
                    loop(NONFIX(infapp(e1, vid1, e2))::s', i)
                else if p1 < p2 then
                    (* shift *)
                    loop(INFIX(x2)::s, i')
                else if a1 <> a2 then
                    error(Source.over(I1,I2), "conflicting infix associativity")
                else if a1 = LEFT then
                    (* reduce infix application *)
                    loop(NONFIX(infapp(e1, vid1, e2))::s', i)
                else (* a1 = RIGHT *)
                    (* shift *)
                    loop(INFIX(x2)::s, i')

              | loop(INFIX(_, vid, I)::s, []) =
                    errorVId(I, "misplaced infix identifier ", vid)

              | loop(INFIX(x)::s, INFIX(_, vid, I)::i) =
                    errorVId(I, "misplaced infix identifier ", vid)

              | loop([], INFIX(_, vid, I)::i) =
                    errorVId(I, "misplaced infix identifier ", vid)

              | loop _ = raise Fail "Infix.parse: inconsistency"
        in
            loop([], es)
        end


    (* Resolving infixed expressions [Section 2.6] *)

    fun atexpExp(PARAtExp(_, exp)) = exp
      | atexpExp atexp             = ATEXPExp(infoAtExp atexp, atexp)

    fun appExp(atexp1, atexp2) =
        let
            val I1 = infoAtExp atexp1
            val I2 = infoAtExp atexp2
            val I  = Source.over(I1, I2)
        in
            PARAtExp(I, APPExp(I, atexpExp atexp1, atexp2))
        end

    fun pairExp(atexp1, atexp2) =
        let
            val I1      = infoAtExp atexp1
            val I2      = infoAtExp atexp2
            val lab1    = Lab.fromInt 1
            val lab2    = Lab.fromInt 2
            val exprow2 = ExpRow(I2, lab2, atexpExp atexp2, NONE)
            val exprow1 = ExpRow(I1, lab1, atexpExp atexp1, SOME exprow2)
        in
            RECORDAtExp(Source.over(I1,I2), SOME exprow1)
        end

    fun infappExp(atexp1, vid, atexp2) =
        let
            val Ivid    = Source.between(infoAtExp atexp1, infoAtExp atexp2)
            val longvid = LongVId.fromId vid
            val atexp1' = LONGVIDAtExp(Ivid, SANSOp, longvid)
            val atexp2' = pairExp(atexp1, atexp2)
        in
            appExp(atexp1', atexp2')
        end


    fun parseExp(J, atexps) =
        let
            val atexp = parse(appExp, infappExp,
                              List.map (categoriseAtExp J) atexps)
        in
            atexpExp atexp
        end


    (* Resolving infixed patterns [Section 2.6] *)

    fun atpatPat(PARAtPat(_, pat)) = pat
      | atpatPat atpat             = ATPATPat(infoAtPat atpat, atpat)

    fun conPat(LONGVIDAtPat(I1, op_opt, longvid), atpat) =
        let
            val I2 = infoAtPat atpat
            val I  = Source.over(I1, I2)
        in
            PARAtPat(I, CONPat(I, op_opt, longvid, atpat))
        end

      | conPat(_, atpat) =
            error(infoAtPat atpat, "misplaced atomic pattern")

    fun pairPat(atpat1, atpat2) =
        let
            val I1      = infoAtPat atpat1
            val I2      = infoAtPat atpat2
            val lab1    = Lab.fromInt 1
            val lab2    = Lab.fromInt 2
            val patrow2 = ROWPatRow(I2, lab2, atpatPat atpat2, NONE)
            val patrow1 = ROWPatRow(I1, lab1, atpatPat atpat1, SOME patrow2)
        in
            RECORDAtPat(Source.over(I1,I2), SOME patrow1)
        end

    fun infconPat(atpat1, vid, atpat2) =
        let
            val Ivid    = Source.between(infoAtPat atpat1, infoAtPat atpat2)
            val longvid = LongVId.fromId vid
            val atpat1' = LONGVIDAtPat(Ivid, SANSOp, longvid)
            val atpat2' = pairPat(atpat1, atpat2)
        in
            conPat(atpat1', atpat2')
        end


    fun parsePat(J, atpats) =
        let
            val atpat = parse(conPat, infconPat,
                              List.map (categoriseAtPat J) atpats)
        in
            atpatPat atpat
        end


    (* Resolving fun match rules [Figure 21, note] *)

    fun parseFmrule(J, atpats) =
        (*
         * Allowed is the following:
         * (1) <op> vid atpat+
         * (2) (atpat infix_vid atpat) atpat*
         * (3) atpat infix_vid atpat
         *)
        let
            fun checkNonfixity []           = true
              | checkNonfixity(NONFIX _::t) = checkNonfixity t
              | checkNonfixity(INFIX(_, vid, I)::t) =
                    errorVId(I, "misplaced infix identifier ", vid)

            fun maybeNonfixClause(ps) =
                case List.hd atpats
                  of LONGVIDAtPat(I, op_opt, longvid) =>
                        if not(LongVId.isUnqualified longvid) then
                            errorLongVId(I, "misplaced long identifier ",
                                         longvid)
                        else if List.length atpats < 2 then
                            error(I, "missing function arguments")
                        else
                            ( checkNonfixity ps (* including 1st *)
                            ; ( op_opt, LongVId.toId longvid, List.tl atpats )
                            )
                   | WILDCARDAtPat(I) =>
                        error(I, "misplaced wildcard pattern")
                   | SCONAtPat(I, _) =>
                        error(I, "misplaced constant pattern")
                   | RECORDAtPat(I, _) =>
                        error(I, "misplaced record or tuple pattern")
                   | PARAtPat(I, _) =>
                        error(I, "misplaced parenthesised pattern")

            fun maybeParenthesisedInfixClause(ps) =
                case List.hd ps
                  of NONFIX(PARAtPat(_, CONPat(I, SANSOp, longvid, atpat))) =>
                        if not(LongVId.isUnqualified longvid) then
                            errorLongVId(I, "misplaced long identifier ",
                                         longvid)
                        else if not(isInfix J longvid) then
                            error(I, "misplaced non-infix pattern")
                        else
                            (* Now, longvid has infix status but is sans `op',
                               so it can only result from resolving an
                               appropriate infix construction. *)
                            ( checkNonfixity(List.tl ps)
                            ; ( SANSOp, LongVId.toId longvid,
                                atpat::List.tl atpats )
                            )

                   | NONFIX(PARAtPat(_, pat)) =>
                        error(infoPat pat, "misplaced non-infix pattern")

                   | _ => maybeNonfixClause(ps)

            fun maybePlainInfixClause(ps) =
                case ps
                  of [NONFIX atpat1, INFIX(_, vid, I), NONFIX atpat2] =>
                         ( SANSOp, vid, pairPat(atpat1, atpat2)::[] )

                   | _ => maybeParenthesisedInfixClause(ps)
        in
            maybePlainInfixClause(List.map (categoriseAtPat J) atpats)
        end

  end
(* stop of Infix.sml *)
(* start of INITIAL_INFIX_ENV.sml *)
(*
 * Standard ML initial infix environment
 *
 * Definition, Appendix C
 *)

signature INITIAL_INFIX_ENV =
  sig

    (* Import type *)

    type InfEnv = Infix.InfEnv

    (* Export *)

    val J0:  InfEnv

  end
(* stop of INITIAL_INFIX_ENV.sml *)
(* start of InitialInfixEnv.sml *)
(*
 * Standard ML initial infix environment
 *
 * Definition, Appendix C
 *)

structure InitialInfixEnv :> INITIAL_INFIX_ENV =
  struct

    (* Import type *)

    type InfEnv = Infix.InfEnv

    (* Value identifiers *)

    val vidCons   = VId.fromString "::"
    val vidEqual  = VId.fromString "="
    val vidAssign = VId.fromString ":="

    (* Export *)

    val J0 = VIdMap.fromList[(vidCons,   (Infix.RIGHT, 5)),
                             (vidEqual,  (Infix.LEFT,  4)),
                             (vidAssign, (Infix.LEFT,  3))]
  end
(* stop of InitialInfixEnv.sml *)
(* start of BASIS.sml *)
(*
 * Standard ML combined basis
 *
 * Definition, section 8
 *)

signature BASIS =
  sig

    (* Import types *)

    type StaticBasis  = StaticBasis.Basis               (* [B_STAT] *)
    type DynamicBasis = DynamicBasis.Basis              (* [B_DYN] *)

    (* Type [Section 8] *)

    type Basis = StaticBasis * DynamicBasis             (* [B] *)


    (* Operations *)

    val B_STATof:       Basis -> StaticBasis
    val B_DYNof:        Basis -> DynamicBasis

    val oplus:          Basis * Basis -> Basis

  end
(* stop of BASIS.sml *)
(* start of Basis.sml *)
(*
 * Standard ML combined basis
 *
 * Definition, section 8
 *)

structure Basis :> BASIS =
  struct

    (* Import types *)

    type StaticBasis  = StaticBasis.Basis               (* [B_STAT] *)
    type DynamicBasis = DynamicBasis.Basis              (* [B_DYN] *)

    (* Type [Section 8] *)

    type Basis = StaticBasis * DynamicBasis             (* [B] *)


    (* Projections *)

    fun B_STATof (B_STAT,B_DYN) = B_STAT
    fun B_DYNof  (B_STAT,B_DYN) = B_DYN


    (* Modification [Section 4.3] *)

    infix oplus

    fun (B_STAT,B_DYN) oplus (B_STAT',B_DYN') =
            ( StaticBasis.plus(B_STAT, B_STAT')
            , DynamicBasis.plus(B_DYN, B_DYN')
            )

  end
(* stop of Basis.sml *)
(* start of PACK.sml *)
(*
 * Standard ML exception packets
 *
 * Definition, section 6.2
 *)


signature PACK =
  sig

    (* Import *)

    type 'a ExVal   = 'a Val.ExVal
    type FcnClosure = DynamicEnv.FcnClosure


    (* Definitions [Section 6.2] *)

    type Pack = FcnClosure ExVal                        (* [p] *)

    exception Pack of Pack

  end
(* stop of PACK.sml *)
(* start of Pack.sml *)
(*
 * Standard ML exception packets
 *
 * Definition, section 6.2
 *)


structure Pack :> PACK =
  struct

    (* Import *)

    type 'a ExVal   = 'a Val.ExVal
    type FcnClosure = DynamicEnv.FcnClosure


    (* Definitions [Section 6.2] *)

    type Pack = FcnClosure ExVal                        (* [p] *)

    exception Pack of Pack

  end
(* stop of Pack.sml *)
(* start of BASVAL.sml *)
(*
 * Standard ML basic values
 *
 * Definition, section 6.4
 *)


signature BASVAL =
  sig 

    (* Import *)

    type BasVal = Val.BasVal
    type 'a Val = 'a Val.Val

    exception Pack of Pack.Pack (* = Pack.Pack *)


    (* Operations *)

    exception TypeError

    val APPLY:     BasVal * 'a Val -> 'a Val (* / Pack *)

    val toString:  BasVal -> string

  end
(* stop of BASVAL.sml *)
(* start of BasVal.sml *)
(*
 * Standard ML basic values
 *
 * Definition, section 6.4
 *)


structure BasVal :> BASVAL =
  struct

    (* Import *)

    type BasVal = Val.BasVal
    type 'a Val = 'a Val.Val

    exception Pack = Pack.Pack


    (* Conversions *)

    fun toString b = b


    (* Application of basic values *)

    exception TypeError

    fun APPLY("=", v) =
        (case Val.unpair v
           of SOME vv => Val.toBoolVal(Val.equal vv)
            | NONE    => raise TypeError
        )
      | APPLY _ = raise Fail "BasVal.APPLY: unknown basic value"

  end
(* stop of BasVal.sml *)
(* start of EVAL_CORE.sml *)
(*
 * Standard ML core evaluation
 *
 * Definition, section 6.7
 *
 * Notes:
 *   - State is passed as reference and modified via side effects. This way
 *     expanding out of the state and exception convention in the inference
 *     rules can be avoided (would really be a pain). Note that the state
 *     therefore never is returned.
 *   - Doing so, we can model the exception convention using exceptions.
 *)


signature EVAL_CORE =
  sig

    (* Import types *)

    type Dec   = GrammarCore.Dec
    type Env   = DynamicEnv.Env
    type State = DynamicEnv.FcnClosure State.State

    (* Export *)

    val evalDec: State ref * Env * Dec -> Env

  end
(* stop of EVAL_CORE.sml *)
(* start of EvalCore.sml *)
(*
 * Standard ML core evaluation
 *
 * Definition, sections 6.7 and 6.2
 *
 * Notes:
 *   - State is passed as reference and modified via side effects. This way
 *     expanding out the state and exception convention in the inference rules
 *     can be avoided (would really be a pain). Note that the state therefore
 *     never is returned.
 *   - Doing so, we can model the exception convention using exceptions.
 *     Rules of the form A |- phrase => A'/p therefore turn into
 *     A |- phrase => A'.
 *   - We only pass the state where necessary.
 *   - Special constants have already been evaluated inside the Lexer.
 *)

structure EvalCore :> EVAL_CORE =
  struct

    (* Import *)

    type Dec   = GrammarCore.Dec
    type Env   = DynamicEnv.Env
    type State = DynamicEnv.FcnClosure State.State

    exception Pack = Pack.Pack

    open GrammarCore


    (* Some helpers for error messages *)

    val error = Error.error

    fun errorLab(I, s, lab)         = error(I, s ^ Lab.toString lab)
    fun errorLongVId(I, s, longvid) = error(I, s ^ LongVId.toString longvid)
    fun errorLongTyCon(I, s, longtycon) =
            error(I, s ^ LongTyCon.toString longtycon)
    fun errorLongStrId(I, s, longstrid) =
            error(I, s ^ LongStrId.toString longstrid)


    (* Helpers for environment modification *)

    val plus        = DynamicEnv.plus
    val plusVE      = DynamicEnv.plusVE
    val plusTE      = DynamicEnv.plusTE
    val plusVEandTE = DynamicEnv.plusVEandTE

    infix plus plusVE plusTE plusVEandTE



    (* Evaluating special constants [Section 6.2] *)

    fun valSCon(SCon.INT n)    = Val.SVal(SVal.INT n)
      | valSCon(SCon.WORD w)   = Val.SVal(SVal.WORD w)
      | valSCon(SCon.CHAR c)   = Val.SVal(SVal.CHAR c)
      | valSCon(SCon.REAL x)   = Val.SVal(SVal.REAL x)
      | valSCon(SCon.STRING s) = Val.SVal(SVal.STRING s)


    (* Inference rules [Section 6.7] *)

    exception FAIL


    (* Atomic Expressions *)

    fun evalAtExp(s,E, SCONAtExp(I, scon)) =
        (* [Rule 90] *)
        valSCon scon

      | evalAtExp(s,E, LONGVIDAtExp(I, _, longvid)) =
        (* [Rule 91] *)
        let
            val (v,is) = case DynamicEnv.findLongVId(E, longvid)
                           of SOME valstr => valstr
                            | NONE =>
                              errorLongVId(I, "runtime error: \
                                              \unknown identifier ", longvid)
        in
            v
        end

      | evalAtExp(s,E, RECORDAtExp(I, exprow_opt)) =
        (* [Rule 92] *)
        let
            val r = case exprow_opt
                      of NONE        => LabMap.empty
                       | SOME exprow => evalExpRow(s,E, exprow)
        in
            Val.Record r
        end

      | evalAtExp(s,E, LETAtExp(I, dec, exp)) =
        (* [Rule 93] *)
        let
            val E' = evalDec(s,E, dec)
            val v  = evalExp(s,E plus E', exp)
        in
            v
        end

      | evalAtExp(s,E, PARAtExp(I, exp)) =
        (* [Rule 94] *)
        let
            val v = evalExp(s,E, exp)
        in
            v
        end


    (* Expression Rows *)

    and evalExpRow(s,E, ExpRow(I, lab, exp, exprow_opt)) =
        (* [Rule 95] *)
        let
            val v = evalExp(s,E, exp)
            val r = case exprow_opt
                      of NONE        => LabMap.empty
                       | SOME exprow => evalExpRow(s,E, exprow)
        in
            LabMap.insert(r, lab, v)
        end


    (* Expressions *)

    and evalExp(s,E, ATEXPExp(I, atexp)) =
        (* [Rule 96] *)
        let
            val v = evalAtExp(s,E, atexp)
        in
            v
        end

      | evalExp(s,E, APPExp(I, exp, atexp)) =
        (* [Rules 97 to 103] *)
        let
            val v1 = evalExp(s,E, exp)
            val v  = evalAtExp(s,E, atexp)
        in
            case v1
              of Val.VId vid =>
                 if vid = VId.fromString "ref" then
                     (* [Rule 99] *)
                     let
                         val a = Addr.addr()
                     in
                          s := State.insertAddr(!s, a, v)
                        ; Val.Addr a
                     end
                 else
                     (* [Rule 97] *)
                     Val.VIdVal (vid,v)

               | Val.ExVal(Val.ExName en) =>
                 (* [Rule 98] *)
                 Val.ExVal(Val.ExNameVal(en,v))

               | Val.:= =>
                 (* [Rule 100] *)
                 (case Val.unpair v
                    of SOME(Val.Addr a, v) =>
                        ( s := State.insertAddr(!s, a, v)
                        ; Val.Record LabMap.empty
                        )
                     | _ => error(I, "runtime type error: address expected")
                 )

               | Val.BasVal b =>
                 (* [Rule 101] *)
                 BasVal.APPLY(b, v)

               | Val.FcnClosure(DynamicEnv.FcnClosure(match,E',VE)) =>
                 (* [Rule 102] *)
                 (let
                     val v' = evalMatch(s,E' plusVE DynamicEnv.Rec VE, v, match)
                  in
                     v'
                  end
                  handle FAIL =>
                     (* [Rule 103] *)
                     raise Pack(Val.ExName InitialDynamicEnv.enMatch)
                 )
               | _ =>
                 error(I, "runtime type error: applicative value expected")
        end

      | evalExp(s,E, TYPEDExp(I, exp, _)) =
        (* Omitted [Section 6.1] *)
        evalExp(s,E, exp)

      | evalExp(s,E, HANDLEExp(I, exp, match)) =
        (* [Rule 104 to 106] *)
        (let
            val v = evalExp(s,E, exp)
         in
            (* [Rule 104] *)
            v
         end
         handle Pack.Pack e =>
            let
                val v = evalMatch(s,E,Val.ExVal e, match)
            in
                (* [Rule 105] *)
                v
            end
            handle FAIL =>
                (* [Rule 106] *)
                raise Pack.Pack e
        )

      | evalExp(s,E, RAISEExp(I, exp)) =
        (* [Rule 107] *)
        let
            val e = case evalExp(s,E, exp)
                      of Val.ExVal e => e
                       | _ => error(I, "runtime type error: \
                                       \exception value expected")
        in
            raise Pack.Pack e
        end

      | evalExp(s,E, FNExp(I, match)) =
        (* [Rule 108] *)
        Val.FcnClosure(DynamicEnv.FcnClosure(match,E,VIdMap.empty))


    (* Matches *)

    and evalMatch(s,E,v, Match(I, mrule, match_opt)) =
        (* [Rules 109 to 111] *)
        let
            val v' = evalMrule(s,E,v, mrule)
        in
            (* [Rule 109] *)
            v'
        end
        handle FAIL =>
            case match_opt
              of NONE =>
                 (* [Rule 110] *)
                 raise FAIL

               | SOME match =>
                 (* [Rule 111] *)
                 let
                     val v' = evalMatch(s,E,v, match)
                 in
                     v'
                 end


    (* Match rules *)

    and evalMrule(s,E,v, Mrule(I, pat, exp)) =
        (* [Rules 112 and 113] *)
        let
            val VE = evalPat(s,E,v, pat)
            (* [Rule 112] *)
            val v' = evalExp(s,E plusVE VE, exp)
        in
            v'
        end
        (* FAIL on evalPat propagates through [Rule 113] *)


    (* Declarations *)

    and evalDec(s,E, VALDec(I, tyvarseq, valbind)) =
        (* [Rule 114] *)
        let
            val VE = evalValBind(s,E, valbind)
        in
            DynamicEnv.fromVE VE
        end

      | evalDec(s,E, TYPEDec(I, typbind)) =
        (* [Rule 115] *)
        let
            val TE = evalTypBind(typbind)
        in
            DynamicEnv.fromTE TE
        end

      | evalDec(s,E, DATATYPEDec(I, datbind)) =
        (* [Rule 116] *)
        let
            val (VE,TE) = evalDatBind(datbind)
        in
            DynamicEnv.fromVEandTE(VE,TE)
        end

      | evalDec(s,E, REPLICATIONDec(I, tycon, longtycon)) =
        (* [Rule 117] *)
        let
            val VE = case DynamicEnv.findLongTyCon(E, longtycon)
                       of SOME VE => VE
                        | NONE =>
                          errorLongTyCon(I, "runtime error: unknown type ",
                                            longtycon)
        in
            DynamicEnv.fromVEandTE(VE, TyConMap.singleton(tycon, VE))
        end

      | evalDec(s,E, ABSTYPEDec(I, datbind, dec)) =
        (* [Rule 118] *)
        let
            val (VE,TE) = evalDatBind(datbind)
            val    E'   = evalDec(s,E plusVEandTE (VE,TE), dec)
        in
            E'
        end

      | evalDec(s,E, EXCEPTIONDec(I, exbind)) =
        (* [Rule 119] *)
        let
            val VE = evalExBind(s,E, exbind)
        in
            DynamicEnv.fromVE VE
        end

      | evalDec(s,E, LOCALDec(I, dec1, dec2)) =
        (* [Rule 120] *)
        let
            val E1 = evalDec(s,E, dec1)
            val E2 = evalDec(s,E plus E1, dec2)
        in
            E2
        end

      | evalDec(s,E, OPENDec(I, longstrids)) =
        (* [Rule 121] *)
        let
            val Es =
                List.map
                    (fn longstrid =>
                        case DynamicEnv.findLongStrId(E, longstrid)
                          of SOME(DynamicEnv.Str E) => E
                           | NONE =>
                             errorLongStrId(I, "runtime error: unknown \
                                               \structure ", longstrid) )
                    longstrids
        in
            List.foldl DynamicEnv.plus DynamicEnv.empty Es
        end

      | evalDec(s,E, EMPTYDec(I)) =
        (* [Rule 122] *)
        DynamicEnv.empty

      | evalDec(s,E, SEQDec(I, dec1, dec2)) =
        (* [Rule 123] *)
        let
            val E1 = evalDec(s,E, dec1)
            val E2 = evalDec(s,E plus E1, dec2)
        in
            E1 plus E2
        end


    (* Value Bindings *)

    and evalValBind(s,E, PLAINValBind(I, pat, exp, valbind_opt)) =
        (* [Rule 124 and 125] *)
        (let
            val v   = evalExp(s,E, exp)
            val VE  = evalPat(s,E,v, pat)
            (* [Rule 124] *)
            val VE' = case valbind_opt
                        of NONE         => VIdMap.empty
                         | SOME valbind => evalValBind(s,E, valbind)
         in
            VIdMap.unionWith #2 (VE, VE')
         end
         handle FAIL =>
            (* [Rule 125] *)
            raise Pack.Pack(Val.ExName InitialDynamicEnv.enBind)
        )

      | evalValBind(s,E, RECValBind(I, valbind)) =
        (* [Rule 126] *)
        let
            val VE = evalValBind(s,E, valbind)
        in
            DynamicEnv.Rec VE
        end


    (* Type Bindings *)

    and evalTypBind(TypBind(I, tyvarseq, tycon, ty, typbind_opt)) =
        (* [Rule 127] *)
        let
            val TE = case typbind_opt
                       of NONE         => TyConMap.empty
                        | SOME typbind => evalTypBind(typbind)
        in
            TyConMap.insert(TE, tycon, VIdMap.empty)
        end


    (* Datatype Bindings *)

    and evalDatBind(DatBind(I, tyvarseq, tycon, conbind, datbind_opt)) =
        (* [Rule 128] *)
        let
            val  VE       = evalConBind(conbind)
            val (VE',TE') = case datbind_opt
                              of NONE          => ( VIdMap.empty, TyConMap.empty )
                               | SOME datbind' => evalDatBind(datbind')
        in
            ( VIdMap.unionWith #2 (VE, VE')
            , TyConMap.insert(TE', tycon, VE)
            )
        end


    (* Constructor Bindings *)

    and evalConBind(ConBind(I, _, vid, _, conbind_opt)) =
        (* [Rule 129] *)
        let
            val VE = case conbind_opt
                       of NONE         => VIdMap.empty
                        | SOME conbind => evalConBind(conbind)
        in
            VIdMap.insert(VE, vid, (Val.VId vid,IdStatus.c))
        end


    (* Exception Bindings *)

    and evalExBind(s,E, NEWExBind(I, _, vid, _, exbind_opt)) =
        (* [Rule 130] *)
        let
            val en = ExName.exname vid
            val VE = case exbind_opt
                       of NONE        => VIdMap.empty
                        | SOME exbind => evalExBind(s,E, exbind)
        in
            s := State.insertExName(!s, en)
          ; VIdMap.insert(VE, vid, (Val.ExVal(Val.ExName en),IdStatus.e))
        end

      | evalExBind(s,E, EQUALExBind(I, _, vid, _, longvid, exbind_opt)) =
        (* [Rule 131] *)
        let
            val en = case DynamicEnv.findLongVId(E, longvid)
                       of SOME(en,IdStatus.e) => en
                        | SOME _ =>
                          errorLongVId(I, "runtime error: non-exception \
                                          \identifier ", longvid)
                        | NONE =>
                          errorLongVId(I, "runtime error: unknown identifier ",
                                          longvid)
            val VE = case exbind_opt
                       of NONE        => VIdMap.empty
                        | SOME exbind => evalExBind(s,E, exbind)
        in
            VIdMap.insert(VE, vid, (en,IdStatus.e))
        end


    (* Atomic Patterns *)

    and evalAtPat(s,E,v, WILDCARDAtPat(I)) =
        (* [Rule 132] *)
        VIdMap.empty

      | evalAtPat(s,E,v, SCONAtPat(I, scon)) =
        (* [Rule 133 and 134] *)
        (case v
           of Val.SVal sv =>
              if Val.equal(v, valSCon(scon)) then
                 (* [Rule 133] *)
                 VIdMap.empty
              else
                 (* [Rule 134] *)
                 raise FAIL

            | _ => error(I, "runtime type error: special constant expected")
        )

      | evalAtPat(s,E,v, LONGVIDAtPat(I, _, longvid)) =
        (* [Rule 135 to 137] *)
        let
            val (strids,vid) = LongVId.explode longvid
        in
            if List.null strids andalso
               ( case DynamicEnv.findVId(E, vid)
                   of NONE       => true
                    | SOME(_,is) => is = IdStatus.v ) then
                (* [Rule 135] *)
                VIdMap.singleton(vid, (v,IdStatus.v))
            else
                let
                    val (v',is) = case DynamicEnv.findLongVId(E, longvid)
                                    of SOME valstr => valstr
                                     | NONE =>
                                        errorLongVId(I,"runtime error: \
                                                       \unknown constructor ",
                                                        longvid)
                in
                    if Val.equal(v, v') then
                        (* [Rule 136] *)
                        VIdMap.empty
                    else
                        (* [Rule 137] *)
                        raise FAIL
                end
        end

      | evalAtPat(s,E,v, RECORDAtPat(I, patrow_opt)) =
        (* [Rule 138] *)
        let
            val r  = case v
                       of Val.Record r => r
                        | _ =>
                          error(I, "runtime type error: record expected")

            val VE = case patrow_opt
                       of NONE        =>
                          if LabMap.isEmpty r then
                             VIdMap.empty
                          else
                             error(I, "runtime type error: \
                                      \empty record expected")

                        | SOME patrow =>
                              evalPatRow(s,E,r, patrow)
        in
            VE
        end

      | evalAtPat(s,E,v, PARAtPat(I, pat)) =
        (* [Rule 139] *)
        let
            val VE = evalPat(s,E,v, pat)
        in
            VE
        end


    (* Pattern Rows *)

    and evalPatRow(s,E,r, WILDCARDPatRow(I)) =
        (* [Rule 140] *)
        VIdMap.empty

      | evalPatRow(s,E,r, ROWPatRow(I, lab, pat, patrow_opt)) =
        (* [Rule 141 and 142] *)
        let
            val v   = case LabMap.find(r, lab)
                        of SOME v => v
                         | _ => errorLab(I, "runtime type error: \
                                            \unmatched label ", lab)
            val VE  = evalPat(s,E,v, pat)
            (* [Rule 142] *)
            val VE' = case patrow_opt
                        of NONE        => VIdMap.empty
                         | SOME patrow => evalPatRow(s,E,r, patrow)
        in
            VIdMap.unionWithi #2 (VE, VE')
        end
        (* FAIL on evalPat propagates through [Rule 142] *)


    (* Patterns *)

    and evalPat(s,E,v, ATPATPat(I, atpat)) =
        (* [Rule 143] *)
        let
            val VE = evalAtPat(s,E,v, atpat)
        in
            VE
        end

      | evalPat(s,E,v, CONPat(I, _, longvid, atpat)) =
        (* [Rules 144 to 148] *)
        (case (DynamicEnv.findLongVId(E, longvid), v)
           of ( SOME(Val.VId vid, IdStatus.c),
                     Val.VIdVal(vid',v') ) =>   
                 if vid = VId.fromString "ref" then
                    error(I, "runtime type error: address expected")
                 else if vid = vid' then
                    (* [Rule 144] *)
                    let
                        val VE = evalAtPat(s,E,v', atpat)
                    in
                        VE
                    end
                 else
                    (* [Rule 145] *)
                    raise FAIL

            | ( SOME(Val.ExVal(Val.ExName en),IdStatus.e),
                     Val.ExVal(Val.ExNameVal(en',v')) ) =>      
                 if en = en' then
                    (* [Rule 146] *)
                    let
                        val VE = evalAtPat(s,E,v', atpat)
                    in
                        VE
                    end
                 else
                    (* [Rule 147] *)
                    raise FAIL

            | ( SOME(Val.VId vid, IdStatus.c),
                     Val.Addr a ) =>    
                 if vid = VId.fromString "ref" then
                    (* [Rule 148] *)
                    let
                        val v =  case State.findAddr(!s, a)
                                   of SOME v => v
                                    | NONE   =>
                                      raise Fail "EvalCore.evalPat: \
                                                 \invalid address"
                        val VE = evalAtPat(s,E,v, atpat)
                    in
                        VE
                    end
                else
                    error(I, "runtime type error: reference expected")

            | _ =>
                 error(I, "runtime type error: constructor expected")
        )

      | evalPat(s,E,v, TYPEDPat(I, pat, _)) =
        (* Omitted [Section 6.1] *)
        evalPat(s,E,v, pat)

      | evalPat(s,E,v, ASPat(I, _, vid, _, pat)) =
        (* [Rule 149] *)
        let
            val VE = evalPat(s,E,v, pat)
        in
            VIdMap.insert(VE, vid, (v,IdStatus.v))
        end

  end
(* stop of EvalCore.sml *)
(* start of INTBASIS.sml *)
(*
 * Standard ML interface basis
 *
 * Definition, section 7.2
 *)

signature INTBASIS =
  sig

    (* Import types *)

    type SigId       = SigId.Id
    type longTyCon   = LongTyCon.longId
    type Basis       = DynamicBasis.Basis
    type SigEnv      = DynamicBasis.SigEnv
    type Env         = DynamicEnv.Env
    type Int         = Interface.Int
    type ValInt      = Interface.ValInt

    type 'a SigIdMap = 'a SigIdMap.map


    (* Types [Section 7.2] *)

    type IntBasis = SigEnv * Int                                (* [IB] *)


    (* Operations *)

    val Inter:          Basis -> IntBasis

    val plusI:          IntBasis * Int    -> IntBasis

    val findSigId:      IntBasis * SigId     -> Int option
    val findLongTyCon:  IntBasis * longTyCon -> ValInt option
 
  end
(* stop of INTBASIS.sml *)
(* start of IntBasis.sml *)
(*
 * Standard ML interface basis
 *
 * Definition, section 7.2
 *)


structure IntBasis :> INTBASIS =
  struct

    (* Import types *)

    type SigId       = SigId.Id
    type longTyCon   = LongTyCon.longId
    type Basis       = DynamicBasis.Basis
    type SigEnv      = DynamicBasis.SigEnv
    type Env         = DynamicEnv.Env
    type Int         = Interface.Int
    type ValInt      = Interface.ValInt

    type 'a SigIdMap = 'a SigIdMap.map


    (* Types [Section 7.2] *)

    type IntBasis = SigEnv * Int                                (* [IB] *)


    (* Injections [Section 7.2] *)

    fun Inter (F,G,E) = (G, Interface.Inter E)


    (* Modifications [Sections 4.3 and 7.2] *)

    infix plusI

    fun (G,I) plusI I'  = ( G, Interface.plus(I,I') )


    (* Application (lookup) [Sections 7.2 and 4.3] *)

    fun findSigId((G,I), sigid) = SigIdMap.find(G, sigid)

    fun findLongTyCon((G,I), longtycon) = Interface.findLongTyCon(I, longtycon)

  end
(* stop of IntBasis.sml *)
(* start of EVAL_MODULE.sml *)
(*
 * Standard ML modules evaluation
 *
 * Definition, section 7.3
 *
 * Notes:
 *   - State is passed as reference and modified via side effects. This way
 *     expanding out the state and exception convention in the inference rules
 *     can be avoided (would really be a pain). Note that the state therefore
 *     never is returned.
 *   - Doing so, we can model the exception convention using exceptions.
 *)

signature EVAL_MODULE =
  sig

    (* Import types *)

    type TopDec = GrammarModule.TopDec
    type Basis  = DynamicBasis.Basis
    type State  = EvalCore.State


    (* Export *)

    val evalTopDec:  State ref * Basis * TopDec -> Basis

  end
(* stop of EVAL_MODULE.sml *)
(* start of EvalModule.sml *)
(*
 * Standard ML modules evaluation
 *
 * Definition, section 7.3
 *
 * Notes:
 *   - State is passed as reference and modified via side effects. This way
 *     expanding out the state and exception convention in the inference rules
 *     can be avoided (would really be a pain). Note that the state therefore
 *     never is returned.
 *   - Doing so, we can model the exception convention using exceptions.
 *     Rules of the form A |- phrase => A'/p therefore turn into
 *     A |- phrase => A'.
 *   - We only pass the state where necessary, ie. strexp, strdec, strbind, and
 *     topdec (compare note in [Section 7.3]).
 *   - There is a typo in the Definition in rule 182: both occurances of IB
 *     should be replaced by B.
 *   - The rules for toplevel declarations are all wrong: in the conclusions,
 *     the result right of the arrow must be B' <+ B''> instead of B'<'> in
 *     all three rules.
 *)

structure EvalModule :> EVAL_MODULE =
  struct

    (* Import *)

    type TopDec = GrammarModule.TopDec
    type Basis  = DynamicBasis.Basis
    type State  = EvalCore.State


    open GrammarModule


    (* Helpers for error messages *)

    val error = Error.error

    fun errorSigId(I, s, sigid) = error(I, s ^ SigId.toString sigid)
    fun errorFunId(I, s, funid) = error(I, s ^ FunId.toString funid)

    fun errorLongTyCon(I, s, longtycon) =
        error(I, s ^ LongTyCon.toString longtycon)
    fun errorLongStrId(I, s, longstrid) =
        error(I, s ^ LongStrId.toString longstrid)


    (* Helpers for basis modification *)

    val plus    = DynamicBasis.plus
    val plusSE  = DynamicBasis.plusSE
    val plusG   = DynamicBasis.plusG
    val plusF   = DynamicBasis.plusF
    val plusE   = DynamicBasis.plusE

    infix plus plusG plusF plusE plusSE



    (* Inference rules [Section 7.3] *)


    (* Structure Expressions *)

    fun evalStrExp(s,B, STRUCTStrExp(I, strdec)) =
        (* [Rule 150] *)
        let
            val E = evalStrDec(s,B, strdec)
        in
            E
        end

      | evalStrExp(s,B, LONGSTRIDStrExp(I, longstrid)) =
        (* [Rule 151] *)
        let
            val E = case DynamicBasis.findLongStrId(B, longstrid)
                      of SOME(DynamicEnv.Str E) => E
                       | NONE =>
                         errorLongStrId(I, "runtime error: unknown structure ",
                                           longstrid)
        in
            E
        end

      | evalStrExp(s,B, TRANSStrExp(I, strexp, sigexp)) =
        (* [Rule 152] *)
        let
            val E = evalStrExp(s,B, strexp)
            val I = evalSigExp(IntBasis.Inter B, sigexp)
        in
            Interface.cutdown(E, I)
        end

      | evalStrExp(s,B, OPAQStrExp(I, strexp, sigexp)) =
        (* [Rule 153] *)
        let
            val E = evalStrExp(s,B, strexp)
            val I = evalSigExp(IntBasis.Inter B, sigexp)
        in
            Interface.cutdown(E, I)
        end

      | evalStrExp(s,B, APPStrExp(I, funid, strexp)) =
        (* [Rule 154] *)
        let
            val DynamicBasis.FunctorClosure((strid, I), strexp', B') =
                      case DynamicBasis.findFunId(B, funid)
                        of SOME funcclos => funcclos
                         | NONE => errorFunId(I, "runtime error: \
                                                 \unknown functor ", funid)
            val E  = evalStrExp(s,B, strexp)
            val E' = evalStrExp(
                        s,
                        B' plusSE
                            StrIdMap.singleton(strid, 
                                DynamicEnv.Str(Interface.cutdown(E, I))),
                        strexp')
        in
            E'
        end

      | evalStrExp(s,B, LETStrExp(I, strdec, strexp)) =
        (* [Rule 155] *)
        let
            val E  = evalStrDec(s,B, strdec)
            val E' = evalStrExp(s,B plusE E, strexp)
        in
            E'
        end


    (* Structure-level Declarations *)

    and evalStrDec(s,B, DECStrDec(I, dec)) =
        (* [Rule 156] *)
        let
            val E' = EvalCore.evalDec(s,DynamicBasis.Eof B, dec)
        in
            E'
        end

      | evalStrDec(s,B, STRUCTUREStrDec(I, strbind)) =
        (* [Rule 157] *)
        let
            val SE = evalStrBind(s,B, strbind)
        in
            DynamicEnv.fromSE SE
        end

      | evalStrDec(s,B, LOCALStrDec(I, strdec1, strdec2)) =
        (* [Rule 158] *)
        let
            val E1 = evalStrDec(s,B, strdec1)
            val E2 = evalStrDec(s,B plusE E1, strdec2)
        in
            E2
        end

      | evalStrDec(s,B, EMPTYStrDec(I)) =
        (* [Rule 159] *)
        DynamicEnv.empty

      | evalStrDec(s,B, SEQStrDec(I, strdec1, strdec2)) =
        (* [Rule 160] *)
        let
            val E1 = evalStrDec(s,B, strdec1)
            val E2 = evalStrDec(s,B plusE E1, strdec2)
        in
            DynamicEnv.plus(E1, E2)
        end


    (* Structure Bindings *)

    and evalStrBind(s,B, StrBind(I, strid, strexp, strbind_opt)) =
        (* [Rule 161] *)
        let
            val E  = evalStrExp(s,B, strexp)
            val SE = case strbind_opt
                       of NONE         => StrIdMap.empty
                        | SOME strbind => evalStrBind(s,B, strbind)
        in
            StrIdMap.insert(SE, strid, DynamicEnv.Str E)
        end


    (* Signature Expressions *)

    and evalSigExp(IB, SIGSigExp(I, spec)) =
        (* [Rule 162] *)
        let
            val I = evalSpec(IB, spec)
        in
            I
        end

      | evalSigExp(IB, SIGIDSigExp(I, sigid)) =
        (* [Rule 163] *)
        let
            val I = case IntBasis.findSigId(IB, sigid)
                      of SOME I => I
                       | NONE   => errorSigId(I, "runtime error: unknown \
                                                 \signature ",sigid)
        in
            I
        end

      | evalSigExp(IB, WHERETYPESigExp(I, sigexp, _, _, _)) =
        (* Omitted [Section 7.1] *)
        evalSigExp(IB, sigexp)


    (* Signature Declarations *)

    and evalSigDec(IB, SigDec(I, sigbind)) =
        (* [Rule 164] *)
        let
            val G = evalSigBind(IB, sigbind)
        in
            G
        end


    (* Signature Bindings *)

    and evalSigBind(IB, SigBind(I, sigid, sigexp, sigbind_opt)) =
        (* [Rule 165] *)
        let
            val I = evalSigExp(IB, sigexp)
            val G = case sigbind_opt
                      of NONE         => SigIdMap.empty
                       | SOME sigbind => evalSigBind(IB, sigbind)
        in
            SigIdMap.insert(G, sigid, I)
        end


    (* Specifications *)

    and evalSpec(IB, VALSpec(I, valdesc)) =
        (* [Rule 166] *)
        let
            val VI = evalValDesc(valdesc)
        in
            Interface.fromVI VI
        end

      | evalSpec(IB, TYPESpec(I, typdesc)) =
        (* [Rule 167] *)
        let
            val TI = evalTypDesc(typdesc)
        in
            Interface.fromTI TI
        end

      | evalSpec(IB, EQTYPESpec(I, typdesc)) =
        (* [Rule 168] *)
        let
            val TI = evalTypDesc(typdesc)
        in
            Interface.fromTI TI
        end

      | evalSpec(IB, DATATYPESpec(I, datdesc)) =
        (* [Rule 169] *)
        let
            val (VI,TI) = evalDatDesc(datdesc)
        in
            Interface.fromVIandTI(VI,TI)
        end

      | evalSpec(IB, REPLICATIONSpec(I, tycon, longtycon)) =
        (* [Rule 170] *)
        let
            val VI = case IntBasis.findLongTyCon(IB, longtycon)
                       of SOME VI => VI
                        | NONE => errorLongTyCon(I, "runtime error: \
                                                    \unknown type ", longtycon)
            val TI = TyConMap.singleton(tycon, VI)
        in
            Interface.fromVIandTI(VI,TI)
        end

      | evalSpec(IB, EXCEPTIONSpec(I, exdesc)) =
        (* [Rule 171] *)
        let
            val VI = evalExDesc(exdesc)
        in
            Interface.fromVI VI
        end

      | evalSpec(IB, STRUCTURESpec(I, strdesc)) =
        (* [Rule 172] *)
        let
            val SI = evalStrDesc(IB, strdesc)
        in
            Interface.fromSI SI
        end

      | evalSpec(IB, INCLUDESpec(I, sigexp)) =
        (* [Rule 173] *)
        let
            val I = evalSigExp(IB, sigexp)
        in
            I
        end

      | evalSpec(IB, EMPTYSpec(I)) =
        (* [Rule 174] *)
        Interface.empty

      | evalSpec(IB, SEQSpec(I, spec1, spec2)) =
        (* [Rule 77] *)
        let
            val I1 = evalSpec(IB, spec1)
            val I2 = evalSpec(IntBasis.plusI(IB, I1), spec2)
        in
            Interface.plus(I1,I2)
        end

      | evalSpec(IB, SHARINGTYPESpec(I, spec, longtycons)) =
        (* Omitted [Section 7.1] *)
        evalSpec(IB, spec)

      | evalSpec(IB, SHARINGSpec(I, spec, longstrids)) =
        (* Omitted [Section 7.1] *)
        evalSpec(IB, spec)


    (* Value Descriptions *)

    and evalValDesc(ValDesc(I, vid, _, valdesc_opt)) =
        (* [Rule 176] *)
        let
            val VI = case valdesc_opt
                       of NONE         => VIdMap.empty
                        | SOME valdesc => evalValDesc(valdesc)
        in
            VIdMap.insert(VI, vid, IdStatus.v)
        end


    (* Type Descriptions *)

    and evalTypDesc(TypDesc(I, tyvarseq, tycon, typdesc_opt)) =
        (* [Rule 177] *)
        let
            val TI = case typdesc_opt
                       of NONE         => TyConMap.empty
                        | SOME typdesc => evalTypDesc(typdesc)
        in
            TyConMap.insert(TI, tycon, VIdMap.empty)
        end


    (* Datatype Descriptions *)

    and evalDatDesc(DatDesc(I, tyvarseq, tycon, condesc, datdesc_opt)) =
        (* [Rule 178] *)
        let
            val  VI       = evalConDesc(condesc)
            val (VI',TI') = case datdesc_opt
                              of NONE          => ( VIdMap.empty, TyConMap.empty )
                               | SOME datdesc' => evalDatDesc(datdesc')
        in
            ( VIdMap.unionWith #2 (VI, VI')
            , TyConMap.insert(TI', tycon, VI)
            )
        end


    (* Constructor Descriptions *)

    and evalConDesc(ConDesc(I, vid, _, condesc_opt)) =
        (* [Rule 179] *)
        let
            val VI = case condesc_opt
                       of NONE         => VIdMap.empty
                        | SOME condesc => evalConDesc(condesc)
        in
            VIdMap.insert(VI, vid, IdStatus.c)
        end


    (* Exception Description *)

    and evalExDesc(ExDesc(I, vid, _, exdesc_opt)) =
        (* [Rule 180] *)
        let
            val VI = case exdesc_opt
                       of NONE        => VIdMap.empty
                        | SOME exdesc => evalExDesc(exdesc)
        in
            VIdMap.insert(VI, vid, IdStatus.e)
        end


    (* Structure Descriptions *)

    and evalStrDesc(IB, StrDesc(I, strid, sigexp, strdesc_opt)) =
        (* [Rule 181] *)
        let
            val I  = evalSigExp(IB, sigexp)
            val SI = case strdesc_opt
                       of NONE         => StrIdMap.empty
                        | SOME strdesc => evalStrDesc(IB, strdesc)
        in
            StrIdMap.insert(SI, strid, Interface.Str I)
        end


    (* Functor Bindings *)

    and evalFunBind(B, FunBind(I, funid, strid, sigexp, strexp, funbind_opt)) =
        (* [Rule 182] *)
        (* Note that there is a typo in this rule. *)
        let
            val I  = evalSigExp(IntBasis.Inter B, sigexp)
            val F  = case funbind_opt
                       of NONE         => FunIdMap.empty
                        | SOME funbind => evalFunBind(B, funbind)
        in
            FunIdMap.insert(F, funid,
                            DynamicBasis.FunctorClosure((strid,I),strexp,B))
        end


    (* Functor Declarations *)

    and evalFunDec(B, FunDec(I, funbind)) =
        (* [Rule 183] *)
        let
            val F = evalFunBind(B, funbind)
        in
            F
        end


    (* Top-level Declarations *)

    and evalTopDec(s,B, STRDECTopDec(I, strdec, topdec_opt)) =
        (* [Rule 184] *)
        (* Note the mistake in the conclusion of this rule. *)
        let
            val E   = evalStrDec(s,B, strdec)
            val B'  = DynamicBasis.fromE E
            val B'' = case topdec_opt
                        of NONE        => DynamicBasis.empty
                         | SOME topdec => evalTopDec(s,B plus B', topdec)
        in
            B' plus B''
        end

      | evalTopDec(s,B, SIGDECTopDec(I, sigdec, topdec_opt)) =
        (* [Rule 185] *)
        (* Note the mistake in the conclusion of this rule. *)
        let
            val G   = evalSigDec(IntBasis.Inter B, sigdec)
            val B'  = DynamicBasis.fromG G
            val B'' = case topdec_opt
                        of NONE        => DynamicBasis.empty
                         | SOME topdec => evalTopDec(s,B plus B', topdec)
        in
            B' plus B''
        end

      | evalTopDec(s,B, FUNDECTopDec(I, fundec, topdec_opt)) =
        (* [Rule 186] *)
        (* Note the mistake in the conclusion of this rule. *)
        let
            val F   = evalFunDec(B, fundec)
            val B'  = DynamicBasis.fromF F
            val B'' = case topdec_opt
                        of NONE        => DynamicBasis.empty
                         | SOME topdec => evalTopDec(s,B plus B', topdec)
        in
            B' plus B''
        end

  end
(* stop of EvalModule.sml *)
(* start of PRETTY_PRINT.sml *)
(*
 * A generic pretty printer.
 *
 * Based on:
 *    Philip Wadler. "A prettier printer"
 *    http://cm.bell-labs.com/cm/cs/who/wadler/
 * and Christian Lindig's port to OCaml.
 *
 * The semantics has been extended to allow 4 different kinds of
 * groups (`boxes'), 2 modes of nesting, and varying break representations.
 * This is no more easily described by an algebra though, and the `below'
 * combinator looses optimality.
 *)

signature PRETTY_PRINT =
  sig

    type doc

    val empty :         doc                     (* empty document *)
    val break :         doc                     (* space or line break *)
    val ebreak :        doc                     (* empty or line break *)
    val text :          string -> doc           (* raw text *)

    val ^^ :            doc * doc -> doc        (* concatenation *)
    val ^/^ :           doc * doc -> doc        (* concatenation with break *)

    val hbox :          doc -> doc              (* horizontal box *)
    val vbox :          doc -> doc              (* vertical box *)
    val fbox :          doc -> doc              (* fill box (h and v) *)
    val abox :          doc -> doc              (* auto box (h or v) *)

    val nest :          int -> doc -> doc       (* indentation by k char's *)
    val below :         doc -> doc              (* keep current indentation *)

    val isEmpty :       doc -> bool

    val toString :      doc * int -> string
    val output :        TextIO.outstream * doc * int -> unit

  end
(* stop of PRETTY_PRINT.sml *)
(* start of PrettyPrint.sml *)
(*
 * A generic pretty printer.
 *
 * Based on:
 *    Philip Wadler. "A prettier printer"
 *    http://cm.bell-labs.com/cm/cs/who/wadler/
 * and Christian Lindig's port to OCaml.
 *
 * The semantics has been extended to allow 4 different kinds of
 * groups (`boxes'), 2 modes of nesting, and varying break representations.
 * This is no more easily described by an algebra though, and the `below'
 * combinator looses optimality.
 *)

structure PrettyPrint :> PRETTY_PRINT =
  struct

    (* Types *)

    datatype mode = H | V | F | A

    datatype doc =
          EMPTY
        | BREAK of string
        | TEXT  of string
        | CONS  of doc * doc
        | BOX   of mode * doc
        | NEST  of int * doc
        | BELOW of doc

    datatype prim =
          PTEXT of string
        | PLINE of int


    (* Interface operators *)

    infixr ^^ ^/^

    val empty   = EMPTY
    val break   = BREAK " "
    val ebreak  = BREAK ""
    val text    = TEXT

    fun x ^^ EMPTY      = x
      | EMPTY ^^ y      = y
      | x ^^ y          = CONS(x, y)

    fun x ^/^ EMPTY     = x
      | EMPTY ^/^ y     = y
      | x ^/^ y         = CONS(x, CONS(break, y))

    fun below EMPTY     = EMPTY
      | below x         = BELOW x

    fun hbox EMPTY      = EMPTY
      | hbox x          = BOX(H, x)

    fun vbox EMPTY      = EMPTY
      | vbox x          = BOX(V, x)

    fun fbox EMPTY      = EMPTY
      | fbox x          = BOX(F, x)

    fun abox EMPTY      = EMPTY
      | abox x          = BOX(A, x)

    fun nest k EMPTY    = EMPTY
      | nest k x        = NEST(k, x)


    fun isEmpty EMPTY   = true
      | isEmpty _       = false


    (* Check whether the first line of a document fits into remaining characters *)

    (* We abuse the mode A (which can never occur in the lists passed to
     * fits) to flag breaks which occur inside swallowed vboxes.
     *)

    fun fits(w, z) =
        w >= 0 andalso
        case z
          of []                 => true
           | (i,m,EMPTY)::z     => fits(w, z)
           | (i,m,CONS(x,y))::z => fits(w, (i,m,x)::(i,m,y)::z)
           | (i,m,TEXT s)::z    => fits(w - String.size s, z)
           | (i,H,BREAK s)::z   => fits(w - String.size s, z)
           | (i,A,BREAK s)::z   => false
           | (i,m,BREAK s)::z   => true
           | (i,m,BOX(V,x))::z  => fits(w, (i,A,x)::z)
           | (i,m,BOX(n,x))::z  => fits(w, (i,H,x)::z)
           | (i,m,NEST(j,x))::z => fits(w, (i,m,x)::z)
           | (i,m,BELOW x)::z   => fits(w, (i,m,x)::z)


    (* Layout *)

    fun best(w, k, z, a) =
        case z
          of []                 => List.rev a
           | (i,m,EMPTY)::z     => best(w, k, z, a)
           | (i,m,CONS(x,y))::z => best(w, k, (i,m,x)::(i,m,y)::z, a)
           | (i,m,TEXT s)::z    => best(w, k + String.size s, z, PTEXT(s)::a)
           | (i,H,BREAK s)::z   => horizontal(w, k, s, z, a)
           | (i,V,BREAK s)::z   => vertical(w, i, z, a)
           | (i,F,BREAK s)::z   => if fits(w - k - String.size s, z)
                                   then horizontal(w, k, s, z, a)
                                   else vertical(w, i, z, a)
           | (i,A,BREAK s)::z   => raise Fail "PrettyPrint.best"
           | (i,m,BOX(A,x))::z  => if fits(w - k, (i,H,x)::z)
                                   then best(w, k, (i,H,x)::z, a)
                                   else best(w, k, (i,V,x)::z, a)
           | (i,m,BOX(n,x))::z  => best(w, k, (i,n,x)::z, a)
           | (i,m,NEST(j,x))::z => best(w, k, (i+j,m,x)::z, a)
           | (i,m,BELOW x)::z   => best(w, k, (k,m,x)::z, a)

    and horizontal(w, k, s, z, a) =
            best(w, k + String.size s, z, PTEXT(s)::a)

    and vertical(w, i, z, a) =
            best(w, i, z, PLINE(i)::a)


    fun layout(doc, w) = best(w, 0, [(0,V,doc)], [])



    (* Convert a document *)

    fun primToString(PTEXT s) = s
      | primToString(PLINE i) = 
            String.implode(#"\n" :: List.tabulate(i, fn _ => #" "))

    val toString = String.concat o List.map primToString o layout



    (* Output a document directly (is MUCH faster!) *)

    fun loop 0 f = ()
      | loop n f = ( f() ; loop (n-1) f )

    fun outputPrim os (PTEXT s) = TextIO.output(os, s)
      | outputPrim os (PLINE i) =
            ( TextIO.output1(os, #"\n")
            ; loop i (fn() => TextIO.output1(os, #" "))
            )

    fun output(os, doc, w) = List.app (outputPrim os) (layout(doc, w))

  end
(* stop of PrettyPrint.sml *)
(* start of PP_MISC.sml *)
(*
 * Standard ML miscellaneous pretty printing helpers
 *)

signature PP_MISC =
  sig

    type doc = PrettyPrint.doc

    val nest:           doc -> doc

    val paren:          doc -> doc
    val brace:          doc -> doc
    val brack:          doc -> doc

    val ppCommaList:    ('a -> doc) -> 'a list -> doc
    val ppStarList:     ('a -> doc) -> 'a list -> doc
    val ppSeq:          ('a -> doc) -> 'a list -> doc
    val ppSeqPrec:      (int -> 'a -> doc) -> int -> 'a list -> doc

  end
(* stop of PP_MISC.sml *)
(* start of PPMisc.sml *)
(*
 * Standard ML miscellaneous pretty printing helpers
 *)

structure PPMisc :> PP_MISC =
  struct

    (* Import *)

    open PrettyPrint

    infixr ^^


    (* Some PP combinators *)

    val nest = nest 2

    fun paren doc = text "(" ^^ fbox(below doc) ^^ text ")"
    fun brace doc = text "{" ^^ fbox(below doc) ^^ text "}"
    fun brack doc = text "[" ^^ fbox(below doc) ^^ text "]"

    fun ppCommaList ppX   []    = empty
      | ppCommaList ppX   [x]   = ppX x
      | ppCommaList ppX (x::xs) = ppX x ^^ text "," ^^ break ^^
                                  ppCommaList ppX xs

    fun ppStarList ppX   []     = empty
      | ppStarList ppX   [x]    = ppX x
      | ppStarList ppX (x::xs)  = hbox(ppX x ^^ break ^^ text "*") ^^ break ^^
                                  ppStarList ppX xs

    fun ppSeqPrec ppXPrec n []  = empty
      | ppSeqPrec ppXPrec n [x] = ppXPrec n x
      | ppSeqPrec ppXPrec n  xs = paren(ppCommaList (ppXPrec 0) xs)

    fun ppSeq ppX = ppSeqPrec (fn _ => ppX) 0

  end
(* stop of PPMisc.sml *)
(* start of PP_VAL.sml *)
(*
 * Standard ML pretty printing of values
 *)

signature PP_VAL =
  sig

    type doc      = PrettyPrint.doc
    type 'a State = 'a State.State
    type 'a Val   = 'a Val.Val
    type 'a ExVal = 'a Val.ExVal

    val ppVal:    'a State * 'a Val -> doc
    val ppExVal:  'a State * 'a ExVal -> doc

  end
(* stop of PP_VAL.sml *)
(* start of PPVal.sml *)
(*
 * Standard ML pretty printing of values
 *)

structure PPVal :> PP_VAL =
  struct

    (* Import *)

    type 'a State = 'a State.State

    open Val
    open PrettyPrint
    open PPMisc

    infixr ^^ ^/^


    (* Simple objects *)

    val ppFn        = text "<fn>"

    fun ppLab lab   = text(Lab.toString lab)
    fun ppVId vid   = text(VId.toString vid)
    fun ppExName en = text(ExName.toString en)
    fun ppSVal sv   = text(SVal.toString sv)


    (* Values *)

    (* Precedence:
     *  0 : plain expressions
     *  1 : constructor arguments
     *)

    fun ppVal  (s, v) = fbox(below(nest(ppValPrec (0, s) v)))
    and ppExVal(s, e) = fbox(below(nest(ppExValPrec (0, s) e)))

    and ppValPrec (p, s) (op:=) =
            ppFn

      | ppValPrec (p, s) (SVal sv) =
            ppSVal sv

      | ppValPrec (p, s) (BasVal b) =
            ppFn

      | ppValPrec (p, s) (VId vid) =
            ppVId vid

      | ppValPrec (p, s) (v as VIdVal(vid, v')) =
        let
            exception NotAList

            fun items(VId vid, vs) =
                if vid <> VId.fromString "nil" then
                    raise NotAList
                else
                    List.rev vs

              | items(VIdVal(vid, v), vs) =
                if vid <> VId.fromString "::" then
                    raise NotAList
                else
                    (case Val.unpair v
                       of NONE         => raise NotAList
                        | SOME(v1, v2) => items(v2, v1::vs)
                    )

              | items(_, vs) = raise NotAList
        in
            let
                val vs = items(v, [])
            in
                brack(ppCommaList (ppValPrec (0, s)) vs)
            end
            handle NotAList =>
            let
                val doc = ppVId vid ^/^ ppValPrec (1, s) v'
            in
                if p = 0 then
                    doc
                else
                    paren doc
            end
        end

      | ppValPrec (p, s) (ExVal e) =
            ppExValPrec (p, s) e

      | ppValPrec (p, s) (Record r) =
        let
            fun isTuple(   [],     n) = n > 2
              | isTuple(lab::labs, n) =
                    lab = Lab.fromInt n andalso isTuple(labs, n+1)

            val labvs     = LabMap.listItemsi r
            val (labs,vs) = ListPair.unzip labvs
        in
            if List.null labs then
                text "()"
            else if isTuple(labs, 1) then
                paren(ppCommaList (ppValPrec (0, s)) vs)
            else
                brace(ppCommaList (ppLabVal s) labvs)
        end

      | ppValPrec (p, s) (Addr a) =
        let
            val v   = case State.findAddr(s, a)
                        of SOME v => v
                         | NONE   => raise Fail "PPVal.ppVal: invalid address"

            val doc = text "ref" ^/^ ppValPrec (1, s) v
        in
            if p = 0 then
                doc
            else
                paren doc
        end

      | ppValPrec (p, s) (FcnClosure _) =
            ppFn


    and ppLabVal s (lab, v) =
            abox(
                hbox(
                    ppLab lab ^/^
                    text "="
                ) ^^
                below(nest(break ^^
                    ppVal(s, v)
                ))
            )


    and ppExValPrec (p, s) (ExName en) =
            ppExName en

      | ppExValPrec (p, s) (ExNameVal(en, v)) =
        let
            val doc = ppExName en ^/^ ppValPrec (1, s) v
        in
            if p = 0 then
                doc
            else
                paren doc
        end

  end
(* stop of PPVal.sml *)
(* start of PP_DYNAMIC_ENV.sml *)
(*
 * Standard ML pretty printing of the dynamic environment
 *)

signature PP_DYNAMIC_ENV =
  sig

    type doc   = PrettyPrint.doc
    type Env   = DynamicEnv.Env
    type Str   = DynamicEnv.Str
    type State = DynamicEnv.FcnClosure State.State

    val ppEnv: State * Env -> doc
    val ppStr: State * Str -> doc

  end
(* stop of PP_DYNAMIC_ENV.sml *)
(* start of PPDynamicEnv.sml *)
(*
 * Standard ML pretty printing of the dynamic environment
 *)

structure PPDynamicEnv :> PP_DYNAMIC_ENV =
  struct

    (* Import *)

    type Env   = DynamicEnv.Env
    type Str   = DynamicEnv.Str
    type State = DynamicEnv.FcnClosure State.State

    open PrettyPrint
    open PPMisc

    infixr ^^ ^/^


    (* Simple objects *)

    fun ppVId vid     = text(VId.toString vid)
    fun ppStrId strid = text(StrId.toString strid)


    (* Environments *)

    fun ppValEnv(s, VE) =
        VIdMap.foldri
            (fn(vid, (v,IdStatus.v), doc) =>
                abox(
                    hbox(
                        text "val" ^/^
                        ppVId vid ^/^
                        text "="
                    ) ^^
                    nest(break ^^
                        abox(PPVal.ppVal(s, v))
                    )
                ) ^/^
                doc

             | (vid, (v,_), doc) => doc
            )
            empty VE

    fun ppExEnv VE =
        VIdMap.foldri
            (fn(vid, (v,IdStatus.e), doc) =>
                hbox(
                    text "exception" ^/^
                    ppVId vid
                ) ^/^
                doc

             | (vid, (v,_), doc) => doc
            )
            empty VE

    fun ppConEnv VE =
        VIdMap.foldli
            (fn(vid, (v,IdStatus.c), doc) =>
                hbox(
                    text "con" ^/^
                    ppVId vid
                ) ^/^
                doc

             | (vid, (v,_), doc) => doc
            )
            empty VE

    fun ppStrEnv(s, SE) =
        StrIdMap.foldri
            (fn(strid, S, doc) =>
                abox(
                    hbox(
                        text "structure" ^/^
                        ppStrId strid ^/^
                        text "="
                    ) ^^
                    nest(break ^^
                        ppStr(s, S)
                    )
                ) ^/^
                doc
            )
            empty SE

    and ppEnv(s, (SE,TE,VE)) =
            vbox(
                ppStrEnv(s, SE) ^/^
                ppConEnv VE ^/^
                ppExEnv  VE ^/^
                ppValEnv(s, VE)
            )


    (* Structures *)

    and ppStr(s, DynamicEnv.Str E) =
        let
            val doc = ppEnv(s, E)
        in
            abox(below(
                text "struct" ^^
                (if isEmpty doc then
                    empty
                 else
                    nest(vbox(break ^^ doc))
                ) ^^ break ^^
                text "end"
            ))
        end

  end
(* stop of PPDynamicEnv.sml *)
(* start of PP_DYNAMIC_BASIS.sml *)
(*
 * Standard ML pretty printing of the dynamic basis
 *)

signature PP_DYNAMIC_BASIS =
  sig

    type doc   = PrettyPrint.doc
    type Basis = DynamicBasis.Basis
    type State = DynamicEnv.FcnClosure State.State

    val ppBasis: State * Basis -> doc

  end
(* stop of PP_DYNAMIC_BASIS.sml *)
(* start of PPDynamicBasis.sml *)
(*
 * Standard ML pretty printing of the dynamic basis
 *)

structure PPDynamicBasis :> PP_DYNAMIC_BASIS =
  struct

    (* Import *)

    type Basis = DynamicBasis.Basis
    type State = DynamicEnv.FcnClosure State.State

    open PrettyPrint

    infixr ^^ ^/^


    (* Simple objects *)

    fun ppFunId funid = text(FunId.toString funid)


    (* Environments *)

    fun ppFunEnv F =
        FunIdMap.foldri
            (fn(funid, _, doc) =>
                hbox(
                    text "functor" ^/^
                    ppFunId funid
                ) ^/^
                doc
            )
            empty F


    (* Basis *)

    fun ppBasis(s, (F,G,E)) =
            vbox(
                ppFunEnv F ^/^
                PPDynamicEnv.ppEnv(s, E) ^/^
                text ""
            )

  end
(* stop of PPDynamicBasis.sml *)
(* start of CHECK_PATTERN.sml *)
(*
 * Standard ML consistency of patterns and matches
 *
 * Definition, section 4.11
 *
 * Note:
 *     The requirement to check for irredundancy of matches is a `bug' in the
 *     definition since this cannot be checked in general for two reasons:
 *
 *     (1) There may be (hidden) aliasing of exception constructors.
 *         Consequently, we only detect redundant exception constructors
 *         if they are denoted by the same longvid.
 *
 *     (2) There is no requirement of consistency for constructors in
 *         sharing specifications or type realisations (actually, we
 *         consider this a serious bug). For example,
 *              datatype t1 = A | B
 *              datatype t2 = C
 *              sharing type t1 = t2
 *         is a legal specification. This allows a mix of the constructors
 *         to appear in matches, rendering the terms of irredundancy and
 *         exhaustiveness meaningless. We make no attempt to detect this,
 *         so generated warnings may or may not make sense in that situation.
 *)


signature CHECK_PATTERN =
  sig

    (* Import *)

    type Pat   = GrammarCore.Pat
    type Match = GrammarCore.Match
    type Env   = StaticEnv.Env


    (* Operations *)

    val checkPat:       Env * Pat   -> unit
    val checkMatch:     Env * Match -> unit

  end
(* stop of CHECK_PATTERN.sml *)
(* start of CheckPattern.sml *)
(*
 * Standard ML consistency of patterns and matches
 *
 * Definition, section 4.11
 *
 * Note:
 *     The requirement to check for irredundancy of matches is a `bug' in the
 *     definition since this cannot be checked in general for two reasons:
 *
 *     (1) There may be (hidden) aliasing of exception constructors.
 *         Consequently, we only detect redundant exception constructors
 *         if they are denoted by the same longvid.
 *
 *     (2) There is no requirement of consistency for constructors in
 *         sharing specifications or type realisations (actually, we
 *         consider this a serious bug). For example,
 *              datatype t1 = A | B
 *              datatype t2 = C
 *              sharing type t1 = t2
 *         is a legal specification. This allows a mix of the constructors
 *         to appear in matches, rendering the terms of irredundancy and
 *         exhaustiveness meaningless. We make no attempt to detect this,
 *         so generated warnings may or may not make sense in that situation.
 *
 * Bugs:
 *     All types of special constants are assumed to be infinite, so that
 *     a match only gets exhaustive by placing a variable. This is a bit
 *     inaccurate for char in particular where the programmer actually would
 *     be able to write down the complete set of values.
 *     The reason is that for special constants to be treated properly in
 *     the presence of overloading we would require the (resolved) type
 *     information.
 *)

structure CheckPattern :> CHECK_PATTERN =
  struct

    (* Import *)

    type SCon    = SCon.SCon
    type Lab     = Lab.Lab
    type VId     = VId.Id
    type longVId = LongVId.longId
    type Pat     = GrammarCore.Pat
    type Match   = GrammarCore.Match
    type Env     = StaticEnv.Env

    type SConSet    = SConSet.set
    type VIdSet     = VIdSet.set
    type LongVIdSet = LongVIdSet.set
    type 'a LabMap  = 'a LabMap.map


    open GrammarCore



    (*
     * Algorithm has been derived from:
     *    Peter Sestoft.
     *         "ML pattern matching compilation and partial evaluation",
     *    in: Dagstuhl Seminar on Partial Evaluation,
     *        Lecture Notes in Computer Science, Springer-Verlag 1996
     *)


    (* Value description *)

    datatype description =
          ANY
        | SCON      of SCon
        | NOT_SCON  of SConSet
        | EXCON     of longVId * description option
        | NOT_EXCON of LongVIdSet
        | CON       of VId * description option
        | NOT_CON   of VIdSet
        | RECORD    of description LabMap

    datatype context =
          EXCON'  of longVId
        | CON'    of VId
        | LAB'    of Lab
        | RECORD' of description LabMap

    type knowledge = description * context list

    type continuations = PatRow option list * Match option



    (* Extending the context on partial success *)

    fun augment(EXCON'(longvid)::context, desc) =
            augment(context, EXCON(longvid, SOME desc))

      | augment(CON'(vid)::context, desc) =
            augment(context, CON(vid, SOME desc))

      | augment(LAB'(lab)::RECORD'(descs)::context, desc) =
            RECORD'(LabMap.insert(descs, lab, desc)) :: context

      | augment _ = raise Fail "CheckPattern.augment: invalid context"


    (* Building the description on failure *)

    fun build([], desc) =
            desc

      | build(EXCON'(longvid)::context, desc) =
            build(context, EXCON(longvid, SOME desc))

      | build(CON'(vid)::context, desc) =
            build(context, CON(vid, SOME desc))

      | build(LAB'(lab)::RECORD'(descs)::context, desc) =
            build(context, RECORD(LabMap.insert(descs, lab, desc)))

      | build _ = raise Fail "CheckPattern.build: invalid context"


    (* Result type for static matching *)

    structure RegionSet = FinSetFn(type ord_key = Source.region
                                   val compare  = Source.compare)

    type result = RegionSet.set * bool

    val success = ( RegionSet.empty, true )
    val failure = ( RegionSet.empty, false )

    fun branch((P1, exhaustive1), (P2, exhaustive2)) =
            ( RegionSet.union(P1, P2), exhaustive1 andalso exhaustive2 )

    fun reached(I, (P, exhaustive)) =
            ( RegionSet.add(P, I), exhaustive )



    (* Static pattern matching *)

    fun matchMatch(E, desc, Match(_, mrule, match_opt)) =
            matchMrule(E, desc, mrule, match_opt)


    and matchMrule(E, desc, Mrule(I, pat, exp), match_opt) =
            reached(I, matchPat(E, (desc, []), pat, ([], match_opt)))


    and matchAtPat(E, know, atpat, cont) =
        case atpat
          of WILDCARDAtPat(_) =>
                succeed(E, know, cont)

           | SCONAtPat(_, scon) =>
                matchSCon(E, know, scon, cont)

           | LONGVIDAtPat(_, _, longvid) =>
               (case StaticEnv.findLongVId(E, longvid)
                  of NONE =>
                        succeed(E, know, cont)

                   | SOME(sigma, IdStatus.v) =>
                        succeed(E, know, cont)

                   | SOME(sigma, IdStatus.e) =>
                        matchExCon(E, know, longvid, NONE, cont)

                   | SOME((_,tau), IdStatus.c) =>
                     let
                        val vid  = LongVId.toId longvid
                        val span = TyName.span(Type.tyname(Type.range tau))
                     in
                        matchCon(E, know, vid, span, NONE, cont)
                     end
               )

           | RECORDAtPat(_, patrow_opt) =>
                matchRecord(E, know, patrow_opt, cont)

           | PARAtPat(_, pat) =>
                matchPat(E, know, pat, cont)


    and matchPat(E, know, pat, cont) =
        case pat
          of ATPATPat(_, atpat) =>
                matchAtPat(E, know, atpat, cont)

           | CONPat(_, _, longvid, atpat) =>
               (case StaticEnv.findLongVId(E, longvid)
                  of SOME(sigma, IdStatus.e) =>
                        matchExCon(E, know, longvid, SOME atpat, cont)

                   | SOME((_,tau), IdStatus.c) =>
                     let
                        val vid  = LongVId.toId longvid
                        val span = TyName.span(Type.tyname(Type.range tau))
                     in
                        matchCon(E, know, vid, span, SOME atpat, cont)
                     end

                   | _ => raise Fail "CheckMatching.matchPat: \
                                     \invalid constructed pattern"
               )

          | TYPEDPat(_, pat, ty) =>
                matchPat(E, know, pat, cont)

          | ASPat(_, _, vid, ty_opt, pat) =>
                matchPat(E, know, pat, cont)


    and matchRecord(E, (desc, context), patrow_opt, cont) =
        let
            val descs = case desc
                          of ANY          => LabMap.empty
                           | RECORD descs => descs
                           | _ =>
                             raise Fail "CheckPattern.matchRecord: type error"
        in
            matchPatRowOpt(E, RECORD'(descs)::context, patrow_opt, cont)
        end


    and matchPatRowOpt(E, RECORD'(descs)::context, patrow_opt,
                                        cont as (patrow_opts, match_opt)) =
        (case patrow_opt
           of SOME(ROWPatRow(_, lab, pat, patrow_opt')) =>
              let
                  val desc' = case LabMap.find(descs, lab)
                                of NONE       => ANY
                                 | SOME desc' => desc'
              in
                  matchPat(E, (desc', LAB'(lab)::RECORD'(descs)::context), pat,
                           (patrow_opt'::patrow_opts, match_opt))
              end

            | _ =>
                  succeed(E, (RECORD descs, context), cont)
        )
      | matchPatRowOpt _ =
            raise Fail "CheckPattern.matchPatRowOpt: inconsistent context"


    and matchSCon(E, know as (desc, context), scon, cont) =
        let
           val knowSucc       = (SCON scon, context)
           fun knowFail scons = (NOT_SCON(SConSet.add(scons, scon)), context)
        in
           case desc
             of ANY =>
                branch(succeed(E, knowSucc, cont),
                       fail(E, knowFail SConSet.empty, cont)
                      )

              | SCON scon' =>
                if SCon.compare(scon, scon') = EQUAL then
                    succeed(E, know, cont)
                else
                    fail(E, know, cont)

              | NOT_SCON scons =>
                if SConSet.member(scons, scon) then
                    fail(E, know, cont)
                else
                    branch(succeed(E, knowSucc, cont),
                           fail(E, knowFail scons, cont)
                          )

              | _ => raise Fail "CheckPattern.matchSCon: type error"
        end


    and matchExCon(E, know as (desc, context), longvid, atpat_opt, cont) =
        let
           val knowSucc = (EXCON(longvid, NONE), EXCON'(longvid)::context)
           fun knowFail longvids =
                (NOT_EXCON(LongVIdSet.add(longvids, longvid)), context)
        in
           case desc
             of ANY =>
                branch(matchArgOpt(E, knowSucc, SOME ANY, atpat_opt, cont),
                       fail(E, knowFail LongVIdSet.empty, cont)
                      )

              | EXCON(longvid', desc_opt) =>
                if longvid = longvid' then
                    matchArgOpt(E, knowSucc, desc_opt, atpat_opt, cont)
                else
                    fail(E, know, cont)

              | NOT_EXCON longvids =>
                if LongVIdSet.member(longvids, longvid) then
                    fail(E, know, cont)
                else
                    branch(matchArgOpt(E, knowSucc, SOME ANY, atpat_opt, cont),
                           fail(E, knowFail longvids, cont)
                          )

              | _ => raise Fail "CheckPattern.matchSCon: type error"
        end


    and matchCon(E, know as (desc, context), vid, span, atpat_opt, cont) =
        let
           val knowSucc      = (CON(vid, NONE), CON'(vid)::context)
           fun knowFail vids = (NOT_CON(VIdSet.add(vids, vid)), context)
        in
           case desc
             of ANY =>
                if span = 1 then
                    matchArgOpt(E, knowSucc, SOME ANY, atpat_opt, cont)
                else
                    branch(matchArgOpt(E, knowSucc, SOME ANY, atpat_opt, cont),
                           fail(E, knowFail VIdSet.empty, cont)
                    )

              | CON(vid', desc_opt) =>
                if vid = vid' then
                    matchArgOpt(E, knowSucc, desc_opt, atpat_opt, cont)
                else
                    fail(E, know, cont)

              | NOT_CON vids =>
                if VIdSet.member(vids, vid) then
                    fail(E, know, cont)
                else if VIdSet.numItems vids = span - 1 then
                    matchArgOpt(E, knowSucc, SOME ANY, atpat_opt, cont)
                else
                    branch(matchArgOpt(E, knowSucc, SOME ANY, atpat_opt, cont),
                           fail(E, knowFail vids, cont)
                          )

              | _ => raise Fail "CheckPattern.matchSCon: type error"
        end


    and matchArgOpt(E, (desc, context), desc_opt, atpat_opt, cont) =
        case atpat_opt
          of NONE =>
                succeed(E, (desc, List.tl context), cont)

           | SOME atpat =>
                matchAtPat(E, (valOf desc_opt, context), atpat, cont)



    and succeed(E, know, ([], match_opt)) =
            success

      | succeed(E, (desc, context), (patrow_opt::patrow_opts, match_opt)) =
        let
            val context' = augment(context, desc)
        in
            matchPatRowOpt(E, context', patrow_opt, (patrow_opts, match_opt))
        end


    and fail(E, know, (_, NONE)) =
            failure

      | fail(E, (desc, context), (_, SOME match)) =
            matchMatch(E, build(context, desc), match)



    (* Checking matches [Section 4.11, item 2] *)

    fun checkReachableMrule(reachables, Mrule(I, _, _)) =
            if RegionSet.member(reachables, I) then
                ()
            else
                Error.warning(I, "redundant match rule")

    fun checkReachableMatchOpt(reachables, NONE)                             = ()
      | checkReachableMatchOpt(reachables, SOME(Match(_, mrule, match_opt))) =
            ( checkReachableMrule(reachables, mrule)
            ; checkReachableMatchOpt(reachables, match_opt)
            )

    fun checkMatch(E, match) =
        let
            val (reachables, exhaustive) = matchMatch(E, ANY, match)
        in
            checkReachableMatchOpt(reachables, SOME match)
          ; if exhaustive then
                ()
            else
                Error.warning(infoMatch match, "match not exhaustive")
        end



    (* Checking single patterns [Section 4.11, item 3] *)

    fun checkPat(E, pat) =
        let
            val (_, exhaustive) = matchPat(E, (ANY, []), pat, ([], NONE))
        in
            if exhaustive then
                ()
            else
                Error.warning(infoPat pat, "pattern not exhaustive")
        end

  end
(* stop of CheckPattern.sml *)
(* start of ELAB_CORE.sml *)
(*
 * Standard ML core elaboration
 *
 * Definition, sections 4.10, 4.11, 4.6, 4.7, 2.9
 *
 * Notes:
 *   - Elaboration also checks the syntactic restrictions [Section 2.9]
 *     and the further restrictions [Section 4.11].
 *   - To implement the 3rd restriction in 4.11 elabDec is passed an
 *     additional boolean argument to recognise being on the toplevel.
 *)


signature ELAB_CORE =
  sig

    (* Import types *)

    type Dec      = GrammarCore.Dec
    type Ty       = GrammarCore.Ty
    type TyVarseq = GrammarCore.TyVarseq

    type VId      = VId.Id
    type TyVar    = TyVar.TyVar
    type TyVarSet = TyVarSet.set
    type Type     = Type.Type
    type Env      = StaticEnv.Env
    type Context  = Context.Context


    (* Export *)

    val elabDec:                bool * Context * Dec -> Env
    val elabTy:                 Context * Ty -> Type

    val tyvars:                 TyVarseq -> TyVarSet * TyVar list

    val validBindVId:           VId -> bool
    val validConBindVId:        VId -> bool

  end
(* stop of ELAB_CORE.sml *)
(* start of ElabCore.sml *)
(*
 * Standard ML core elaboration
 *
 * Definition, sections 4.10, 4.11, 4.6, 4.7, 2.9
 *
 * Notes:
 *   - Elaboration also checks the syntactic restrictions [Section 2.9]
 *     and the further restrictions [Section 4.11].
 *   - To implement the 3rd restriction in 4.11 some elab functions are
 *     passed an additional boolean argument to recognise being on the toplevel.
 *   - There is a bug in the Definition -- an important syntactic restriction
 *     is missing:
 *       "Any tyvar occuring on the right side of a typbind or datbind of the
 *        form tyvarseq tycon = ... must occur in tyvarseq."
 *   - The definition says that overloaded types get defaulted if the
 *     "surrounding text" does not resolve it. It leaves some freedom to
 *     how large this context may be. We choose the innermost value binding.
 *   - The definition states that "the program context" must determine the
 *     exact type of flexible records, but it does not say how large this
 *     context may be either. Again we choose the innermost surrounding value
 *     binding.
 *   - Most conditions on type names can be ignored since they are
 *     always ensured by the Stamp mechanism.
 *
 * Bugs:
 *   - Unresolved overloading is left unnoticed if it never propagates to a
 *     value binding's result environment. To resolve all cases we either had
 *     to annotate all expressions with their types and walk the whole tree
 *     for each value binding's RHS, or extend the inference results with
 *     complicated information on overloaded type variables, or use some dirty
 *     side effect hack.
 *   - The same goes for unresolved flexible record types, for essentially the
 *     same reason.
 *)

structure ElabCore :> ELAB_CORE =
  struct

    (* Import *)

    type Dec      = GrammarCore.Dec
    type Ty       = GrammarCore.Ty
    type TyVarseq = GrammarCore.TyVarseq

    type VId      = VId.Id
    type TyVar    = TyVar.TyVar
    type TyVarSet = TyVarSet.set
    type Type     = Type.Type
    type Env      = StaticEnv.Env
    type Context  = Context.Context


    open GrammarCore


    (* Some helpers for error messages *)

    val error = Error.error

    fun errorLab(I, s, lab)     = error(I, s ^ Lab.toString lab)
    fun errorVId(I, s, vid)     = error(I, s ^ VId.toString vid)
    fun errorTyCon(I, s, tycon) = error(I, s ^ TyCon.toString tycon)
    fun errorTyVar(I, s, tyvar) = error(I, s ^ TyVar.toString tyvar)

    fun errorLongVId(I, s, longvid) = error(I, s ^ LongVId.toString longvid)
    fun errorLongTyCon(I, s, longtycon) =
            error(I, s ^ LongTyCon.toString longtycon)
    fun errorLongStrId(I, s, longstrid) =
            error(I, s ^ LongStrId.toString longstrid)



    (* Helpers for context modification *)

    val plus         = StaticEnv.plus
    val plusU        = Context.plusU
    val plusVE       = Context.plusVE
    val oplusE       = Context.oplusE
    val oplusTE      = Context.oplusTE
    val oplusVEandTE = Context.oplusVEandTE

    infix plusU plusVE oplusE oplusTE oplusVEandTE



    (* Checking restriction for vids in binding [Section 2.9, 5th bullet] *)

    fun validBindVId vid =
            vid <> VId.fromString "true" andalso
            vid <> VId.fromString "false" andalso
            vid <> VId.fromString "nil" andalso
            vid <> VId.fromString "::"  andalso
            vid <> VId.fromString "ref"

    fun validConBindVId vid =
            validBindVId vid andalso
            vid <> VId.fromString "it"


    (* Treating tyvarseqs *)

    fun tyvars(TyVarseq(I, tyvars)) =
        let
            fun collect(     [],       U) = U
              | collect(tyvar::tyvars, U) =
                    if TyVarSet.member(U, tyvar) then
                        (* Syntactic restriction [Section 2.9, 3rd bullet] *)
                        errorTyVar(I, "duplicate type variable ", tyvar)
                    else
                        collect(tyvars, TyVarSet.add(U, tyvar))
        in
            ( collect(tyvars, TyVarSet.empty), tyvars )
        end



    (* Typing special constants [Section 4.1, Appendix E.1] *)

    fun typeSCon(SCon.INT _)  = Type.fromOverloadingClass InitialStaticEnv.Int
      | typeSCon(SCon.WORD _) = Type.fromOverloadingClass InitialStaticEnv.Word
      | typeSCon(SCon.CHAR _) = Type.fromOverloadingClass InitialStaticEnv.Char
      | typeSCon(SCon.REAL _) = Type.fromOverloadingClass InitialStaticEnv.Real
      | typeSCon(SCon.STRING _) =
            Type.fromOverloadingClass InitialStaticEnv.String


    (* Calculate sets of unguarded explicit type variables [Section 4.6] *)

    local
        val op+ = TyVarSet.union

        fun ? tyvarsX  NONE    = TyVarSet.empty
          | ? tyvarsX (SOME x) = tyvarsX x
    in

    fun unguardedTyVarsAtExp(RECORDAtExp(_, exprow_opt)) =
            ?unguardedTyVarsExpRow exprow_opt
      | unguardedTyVarsAtExp(LETAtExp(_, dec, exp)) =
            unguardedTyVarsDec dec + unguardedTyVarsExp exp
      | unguardedTyVarsAtExp(PARAtExp(_, exp)) =
            unguardedTyVarsExp exp
      | unguardedTyVarsAtExp _ = TyVarSet.empty

    and unguardedTyVarsExpRow(ExpRow(_, lab, exp, exprow_opt)) =
            unguardedTyVarsExp exp + ?unguardedTyVarsExpRow exprow_opt

    and unguardedTyVarsExp(ATEXPExp(_, atexp)) =
            unguardedTyVarsAtExp atexp
      | unguardedTyVarsExp(APPExp(_, exp, atexp)) =
            unguardedTyVarsExp exp + unguardedTyVarsAtExp atexp
      | unguardedTyVarsExp(TYPEDExp(_, exp, ty)) =
            unguardedTyVarsExp exp + unguardedTyVarsTy ty
      | unguardedTyVarsExp(HANDLEExp(_, exp, match)) =
            unguardedTyVarsExp exp + unguardedTyVarsMatch match
      | unguardedTyVarsExp(RAISEExp(_, exp)) =
            unguardedTyVarsExp exp
      | unguardedTyVarsExp(FNExp(_, match)) =
            unguardedTyVarsMatch match

    and unguardedTyVarsMatch(Match(_, mrule, match_opt)) =
            unguardedTyVarsMrule mrule + ?unguardedTyVarsMatch match_opt

    and unguardedTyVarsMrule(Mrule(_, pat, exp)) =
            unguardedTyVarsPat pat + unguardedTyVarsExp exp

    and unguardedTyVarsDec(ABSTYPEDec(_, datbind, dec)) =
            unguardedTyVarsDec dec
      | unguardedTyVarsDec(EXCEPTIONDec(_, exbind)) =
            unguardedTyVarsExBind exbind
      | unguardedTyVarsDec(LOCALDec(_, dec1, dec2)) =
            unguardedTyVarsDec dec1 + unguardedTyVarsDec dec2
      | unguardedTyVarsDec(SEQDec(_, dec1, dec2)) =
            unguardedTyVarsDec dec1 + unguardedTyVarsDec dec2
      | unguardedTyVarsDec _ = TyVarSet.empty

    and unguardedTyVarsValBind(PLAINValBind(_, pat, exp, valbind_opt)) =
            unguardedTyVarsPat pat + unguardedTyVarsExp exp +
            ?unguardedTyVarsValBind valbind_opt
      | unguardedTyVarsValBind(RECValBind(_, valbind)) =
            unguardedTyVarsValBind valbind

    and unguardedTyVarsExBind(NEWExBind(_, _, vid, ty_opt, exbind_opt)) =
            ?unguardedTyVarsTy ty_opt + ?unguardedTyVarsExBind exbind_opt
      | unguardedTyVarsExBind(EQUALExBind(_, _, vid, _, longvid, exbind_opt)) =
            ?unguardedTyVarsExBind exbind_opt

    and unguardedTyVarsAtPat(RECORDAtPat(_, patrow_opt)) =
            ?unguardedTyVarsPatRow patrow_opt
      | unguardedTyVarsAtPat(PARAtPat(_, pat)) =
            unguardedTyVarsPat pat
      | unguardedTyVarsAtPat _ = TyVarSet.empty

    and unguardedTyVarsPatRow(WILDCARDPatRow(_)) = TyVarSet.empty
      | unguardedTyVarsPatRow(ROWPatRow(_, lab, pat, patrow_opt)) =
            unguardedTyVarsPat pat + ?unguardedTyVarsPatRow patrow_opt

    and unguardedTyVarsPat(ATPATPat(_, atpat)) =
            unguardedTyVarsAtPat atpat
      | unguardedTyVarsPat(CONPat(_, _, longvid, atpat)) =
            unguardedTyVarsAtPat atpat
      | unguardedTyVarsPat(TYPEDPat(_, pat, ty)) =
            unguardedTyVarsPat pat + unguardedTyVarsTy ty
      | unguardedTyVarsPat(ASPat(_, _, vid, ty_opt, pat)) =
            ?unguardedTyVarsTy ty_opt + unguardedTyVarsPat pat

    and unguardedTyVarsTy(TYVARTy(_, tyvar)) = TyVarSet.singleton tyvar
      | unguardedTyVarsTy(RECORDTy(_, tyrow_opt)) =
            ?unguardedTyVarsTyRow tyrow_opt
      | unguardedTyVarsTy(TYCONTy(_, tyseq, longtycon)) =
            unguardedTyVarsTyseq tyseq
      | unguardedTyVarsTy(ARROWTy(_, ty, ty')) =
            unguardedTyVarsTy ty + unguardedTyVarsTy ty'
      | unguardedTyVarsTy(PARTy(_, ty)) =
            unguardedTyVarsTy ty

    and unguardedTyVarsTyRow(TyRow(_, lab, ty, tyrow_opt)) =
            unguardedTyVarsTy ty + ?unguardedTyVarsTyRow tyrow_opt

    and unguardedTyVarsTyseq(Tyseq(_, tys)) =
            List.foldl (fn(ty,U) => unguardedTyVarsTy ty + U) TyVarSet.empty tys

    end (* local *)



    (* Check whether a pattern binds an identifier *)

    local
        fun ? boundByX(NONE,   vid) = false
          | ? boundByX(SOME x, vid) = boundByX(x, vid)
    in

    fun boundByAtPat(WILDCARDAtPat(_), vid)   = false
      | boundByAtPat(SCONAtPat(_, scon), vid) = false
      | boundByAtPat(LONGVIDAtPat(_, _, longvid), vid) =
        let
            val (strids,vid') = LongVId.explode longvid
        in
            List.null strids andalso vid = vid'
        end
      | boundByAtPat(RECORDAtPat(_, patrow_opt), vid) =
            ?boundByPatRow(patrow_opt, vid)
      | boundByAtPat(PARAtPat(_, pat), vid) = boundByPat(pat, vid)

    and boundByPatRow(WILDCARDPatRow(_), vid) = false
      | boundByPatRow(ROWPatRow(_, lab, pat, patrow_opt), vid) =
            boundByPat(pat, vid) orelse ?boundByPatRow(patrow_opt, vid)

    and boundByPat(ATPATPat(_, atpat), vid)           = boundByAtPat(atpat, vid)
      | boundByPat(CONPat(_, _, longvid, atpat), vid) = boundByAtPat(atpat, vid)
      | boundByPat(TYPEDPat(_, pat, ty), vid)         = boundByPat(pat, vid)
      | boundByPat(ASPat(_, _, vid', ty_opt, pat), vid) =
            vid = vid' orelse boundByPat(pat, vid)

    end (* local *)



    (* Non-expansive expressions [Section 4.7] *)

    local
        fun ? isNonExpansiveX C  NONE    = true
          | ? isNonExpansiveX C (SOME x) = isNonExpansiveX C x
    in

    fun isNonExpansiveAtExp C (SCONAtExp(_, scon))          = true
      | isNonExpansiveAtExp C (LONGVIDAtExp(_, _, longvid)) = true
      | isNonExpansiveAtExp C (RECORDAtExp(_, exprow_opt))  =
            ?isNonExpansiveExpRow C exprow_opt
      | isNonExpansiveAtExp C (PARAtExp(_, exp))    = isNonExpansiveExp C exp
      | isNonExpansiveAtExp C  _                    = false

    and isNonExpansiveExpRow C (ExpRow(_, lab, exp, exprow_opt)) =
            isNonExpansiveExp C exp andalso ?isNonExpansiveExpRow C exprow_opt

    and isNonExpansiveExp C (ATEXPExp(_, atexp))   = isNonExpansiveAtExp C atexp
      | isNonExpansiveExp C (APPExp(_, exp, atexp)) =
            isConExp C exp andalso isNonExpansiveAtExp C atexp
      | isNonExpansiveExp C (TYPEDExp(_, exp, ty))  = isNonExpansiveExp C exp
      | isNonExpansiveExp C (FNExp(_, match))       = true
      | isNonExpansiveExp C  _                      = false

    and isConAtExp C (PARAtExp(_, exp))            = isConExp C exp
      | isConAtExp C (LONGVIDAtExp(_, _, longvid)) =
            LongVId.explode longvid <> ([],VId.fromString "ref") andalso
            (case Context.findLongVId(C, longvid)
               of SOME(_,is) => is=IdStatus.c orelse is=IdStatus.e
                | NONE       => false
            )
      | isConAtExp C  _                            = false

    and isConExp C (ATEXPExp(_, atexp))                  = isConAtExp C atexp
      | isConExp C (TYPEDExp(_, ATEXPExp(_, atexp), ty)) = isConAtExp C atexp
      | isConExp C  _                                    = false

    end (* local *)



    (* Closure of value environments [Section 4.8] *)

    fun hasNonExpansiveRHS C (vid, PLAINValBind(I, pat, exp, valbind_opt)) =
        if boundByPat(pat, vid) then
            isNonExpansiveExp C exp
        else
            hasNonExpansiveRHS C (vid, valOf valbind_opt)

      | hasNonExpansiveRHS C (vid, RECValBind _) =
            (* A rec valbind can only contain functions. *)
            true

    fun Clos (C,valbind) VE =
        let
            val tyvarsC = Context.tyvars C

            fun alphas(vid, tau) =
                if hasNonExpansiveRHS C (vid, valbind) then
                    TyVarSet.listItems
                        (TyVarSet.difference(Type.tyvars tau, tyvarsC))
                else
                    []
        in
            VIdMap.mapi
                (fn(vid, ((_,tau),is)) => ((alphas(vid,tau),tau),is))
                VE
        end


    (* Inference rules [Section 4.10] *)


    (* Atomic Expressions *)

    fun elabAtExp(C, SCONAtExp(I, scon)) =
        (* [Rule 1] *)
        typeSCon scon

      | elabAtExp(C, LONGVIDAtExp(I, _, longvid)) =
        (* [Rule 2] *)
        let
            val (sigma,is) = case Context.findLongVId(C, longvid)
                               of SOME valstr => valstr
                                | NONE =>
                                  errorLongVId(I, "unknown identifier ",longvid)
            val tau = TypeScheme.instance sigma
        in
            tau
        end

      | elabAtExp(C, RECORDAtExp(I, exprow_opt)) =
        (* [Rule 3] *)
        let
            val rho = case exprow_opt
                        of NONE        => Type.emptyRho
                         | SOME exprow => elabExpRow(C, exprow)
        in
            Type.fromRowType rho
        end

      | elabAtExp(C, LETAtExp(I, dec, exp)) =
        (* [Rule 4] *)
        let
            val E   = elabDec(false, C, dec)
            val tau = elabExp(C oplusE E, exp)
        in
            if TyNameSet.isSubset(Type.tynames tau, Context.Tof C) then
                tau
            else
                error(I, "escaping local type name in let expression")
        end

      | elabAtExp(C, PARAtExp(I, exp)) =
        (* [Rule 5] *)
        let
            val tau = elabExp(C, exp)
        in
            tau
        end


    (* Expression Rows *)

    and elabExpRow(C, ExpRow(I, lab, exp, exprow_opt)) =
        (* [Rule 6] *)
        let
            val tau = elabExp(C, exp)
            val rho = case exprow_opt
                        of NONE        => Type.emptyRho
                         | SOME exprow => elabExpRow(C, exprow)
        in
            if isSome(Type.findLab(rho, lab)) then
                (* Syntactic restriction [Section 2.9, 1st bullet] *)
                errorLab(I, "duplicate label ", lab)
            else
                Type.insertRho(rho, lab, tau)
        end


    (* Expressions *)

    and elabExp(C, ATEXPExp(I, atexp)) =
        (* [Rule 7] *)
        let
            val tau = elabAtExp(C, atexp)
        in
            tau
        end

      | elabExp(C, APPExp(I, exp, atexp)) =
        (* [Rule 8] *)
        let
            val tau1 = elabExp(C, exp)
            val tau' = elabAtExp(C, atexp)
            val tau  = Type.invent()
        in
            Type.unify(tau1, Type.fromFunType(tau',tau))
            handle Type.Unify => error(I, "type mismatch on application")
          ; tau
        end

      | elabExp(C, TYPEDExp(I, exp, ty)) =
        (* [Rule 9] *)
        let
            val tau1 = elabExp(C, exp)
            val tau  = elabTy(C, ty)
        in
            Type.unify(tau1,tau)
            handle Type.Unify =>
                   error(I, "expression does not match annotation")
          ; tau
        end

      | elabExp(C, HANDLEExp(I, exp, match)) =
        (* [Rule 10] *)
        let
            val tau1 = elabExp(C, exp)
            val tau2 = elabMatch(C, match)
        in
            Type.unify(tau1,tau2)
            handle Type.Unify =>
                   error(I, "type mismatch between expression and handler")
          ; tau1
        end

      | elabExp(C, RAISEExp(I, exp)) =
        (* [Rule 11] *)
        let
            val tau1 = elabExp(C, exp)
        in
            Type.unify(tau1, InitialStaticEnv.tauExn)
            handle Type.Unify =>
                   error(I, "raised expression is not an exception")
          ; Type.invent()
        end

      | elabExp(C, FNExp(I, match)) =
        (* [Rule 12] *)
        let
            val tau = elabMatch(C, match)
        in
            (* Further restriction [Section 4.11, item 2] *)
            CheckPattern.checkMatch(Context.Eof C, match)
          ; tau
        end


    (* Matches *)

    and elabMatch(C, Match(I, mrule, match_opt)) =
        (* [Rule 13] *)
        let
            val tau = elabMrule(C, mrule)
        in
            case match_opt
              of NONE       => tau
               | SOME match =>
                 let
                     val tau2 = elabMatch(C, match)
                 in
                     Type.unify(tau, tau2)
                     handle Type.Unify =>
                            error(I, "type mismatch between different matches")
                   ; tau
                 end
        end


    (* Match rules *)

    and elabMrule(C, Mrule(I, pat, exp)) =
        (* [Rule 14] *)
        let
            val (VE,tau) = elabPat(C, pat)
            val  tau'    = elabExp(C plusVE VE, exp)
            (* Side condition on type names is always ensured. *)
        in
            Type.fromFunType(tau,tau')
        end


    (* Declarations *)

    and elabDec(toplevel, C, VALDec(I, tyvarseq, valbind)) =
        (* [Rule 15] *)
        let
            val U'  = #1(tyvars(tyvarseq))
                      (* Collect implicitly bound tyvars [Section 4.6] *)
            val U   = TyVarSet.union(U',
                        TyVarSet.difference(unguardedTyVarsValBind valbind,
                                            Context.Uof C))
            val VE  = elabValBind(toplevel, C plusU U, valbind)
            val VE' = Clos(C,valbind) VE
            val  _  = StaticEnv.defaultOverloaded VE'
        in
            if not(TyVarSet.isEmpty(
                                TyVarSet.intersection(Context.Uof C, U))) then
                (* Syntactic restriction [Section 2.9, last bullet] *)
                error(I, "some type variables shadow previous ones")
            else if StaticEnv.containsFlexibleType VE' then
                (* Further restriction [Section 4.11, item 1] *)
                error(I, "unresolved flexible record type")
            else if TyVarSet.isEmpty(
                        TyVarSet.intersection(U, StaticEnv.tyvarsVE VE')) then
                StaticEnv.fromVE VE'
            else
                error(I, "some explicit type variables cannot be generalised")
        end

      | elabDec(toplevel, C, TYPEDec(I, typbind)) =
        (* [Rule 16] *)
        let
            val TE = elabTypBind(C, typbind)
        in
            StaticEnv.fromTE TE
        end

      | elabDec(toplevel, C, DATATYPEDec(I, datbind)) =
        (* [Rule 17] *)
        let
            val      TE1  = lhsDatBind datbind
            val (VE2,TE2) = elabDatBind(C oplusTE TE1, datbind)
            val (TE, VE)  = StaticEnv.maximiseEquality(TE2,VE2)
            (* Side condition on type names is always ensured. *)
        in
            StaticEnv.fromVEandTE(VE,TE)
        end

      | elabDec(toplevel, C, REPLICATIONDec(I, tycon, longtycon)) =
        (* [Rule 18] *)
        let
            val (theta,VE) = case Context.findLongTyCon(C, longtycon)
                              of SOME tystr => tystr
                               | NONE =>
                                 errorLongTyCon(I, "unknown type ", longtycon)
            val  TE        = TyConMap.singleton(tycon, (theta,VE))
        in
            StaticEnv.fromVEandTE(VE,TE)
        end

      | elabDec(toplevel, C, ABSTYPEDec(I, datbind, dec)) =
        (* [Rule 19] *)
        let
            val      TE1  = lhsDatBind datbind
            val (VE2,TE2) = elabDatBind(C oplusTE TE1, datbind)
            val (TE, VE)  = StaticEnv.maximiseEquality(TE2,VE2)
            val    E      = elabDec(false, C oplusVEandTE (VE,TE), dec)
            (* Side condition on type names is always ensured. *)
        in
            StaticEnv.Abs(TE,E)
        end

      | elabDec(toplevel, C, EXCEPTIONDec(I, exbind)) =
        (* [Rule 20] *)
        let
            val VE = elabExBind(C, exbind)
        in
            StaticEnv.fromVE VE
        end

      | elabDec(toplevel, C, LOCALDec(I, dec1, dec2)) =
        (* [Rule 21] *)
        let
            val E1 = elabDec(false, C, dec1)
            val E2 = elabDec(false, C oplusE E1, dec2)
        in
            E2
        end

      | elabDec(toplevel, C, OPENDec(I, longstrids)) =
        (* [Rule 22] *)
        let
            val Es =
                List.map
                    (fn longstrid =>
                        case Context.findLongStrId(C, longstrid)
                          of SOME(StaticEnv.Str E) => E
                           | NONE =>
                             errorLongStrId(I, "unknown structure ", longstrid))
                    longstrids
        in
            List.foldl StaticEnv.plus StaticEnv.empty Es
        end

      | elabDec(toplevel, C, EMPTYDec(I)) =
        (* [Rule 23] *)
        StaticEnv.empty

      | elabDec(toplevel, C, SEQDec(I, dec1, dec2)) =
        (* [Rule 24] *)
        let
            val E1 = elabDec(toplevel, C, dec1)
            val E2 = elabDec(toplevel, C oplusE E1, dec2)
        in
            StaticEnv.plus(E1, E2)
        end


    (* Value Bindings *)

    and elabValBind(toplevel, C, PLAINValBind(I, pat, exp, valbind_opt)) =
        (* [Rule 25] *)
        let
            val (VE,tau1) = elabPat(C, pat)
            val     tau2  = elabExp(C, exp)
            val VE'       = case valbind_opt
                              of NONE         => VIdMap.empty
                               | SOME valbind => elabValBind(toplevel, C, valbind)
        in
            Type.unify(tau1,tau2)
            handle Type.Unify =>
                   error(I, "type mismatch between pattern and expression")
          ; if toplevel then () else
                (* Further restriction [Section 4.11, item 3] *)
                CheckPattern.checkPat(Context.Eof C, pat)
          ; VIdMap.unionWithi
                (fn(vid,_,_) =>
                 (* Syntactic restriction [Section 2.9, 2nd bullet] *)
                 errorVId(I, "duplicate variable ", vid))
                (VE,VE')
        end

      | elabValBind(toplevel, C, RECValBind(I, valbind)) =
        (* [Rule 26] *)
        let
            val VE1 = lhsRecValBind valbind
            val VE  = elabValBind(toplevel, C plusVE VE1, valbind)
            (* Side condition on type names is always ensured. *)
        in
            VE
        end


    (* Type Bindings *)

    and elabTypBind(C, TypBind(I, tyvarseq, tycon, ty, typbind_opt)) =
        (* [Rule 27] *)
        let
            val (U,alphas) = tyvars tyvarseq
            val tau        = elabTy(C, ty)
            val TE         = case typbind_opt
                               of NONE         => TyConMap.empty
                                | SOME typbind => elabTypBind(C, typbind)
        in
            if not(TyVarSet.isSubset(Type.tyvars tau, U)) then
                (* Syntactic restriction (missing in the Definition!) *)
                error(I, "free type variables in type binding")
            else if isSome(TyConMap.find(TE, tycon)) then
                (* Syntactic restriction [Section 2.9, 2nd bullet] *)
                errorTyCon(I, "duplicate type constructor ", tycon)
            else
                TyConMap.insert(TE, tycon, ((alphas,tau),VIdMap.empty))
        end


    (* Datatype Bindings *)

    and elabDatBind(C, DatBind(I, tyvarseq, tycon, conbind, datbind_opt)) =
        (* [Rule 28, part 2] *)
        let
            val (U,alphas)   = tyvars tyvarseq
            val (alphas,tau) = case Context.findTyCon(C, tycon)
                                 of SOME(theta,VE) => theta
                                  | NONE => (* lhsDatBind inserted it! *)
                                    raise Fail "ElabCore.elabDatBind: \
                                                \tycon not pre-bound"
            val VE       = elabConBind(C,tau, conbind)
            val(VE',TE') = case datbind_opt
                             of NONE         => ( VIdMap.empty, TyConMap.empty )
                              | SOME datbind => elabDatBind(C, datbind)
                                (* Side condition on t is always true. *)
            val ClosVE   = if TyVarSet.isSubset(StaticEnv.tyvarsVE VE, U) then
                             StaticEnv.Clos VE
                           else
                             (* Syntactic restriction (missing in Definition!)*)
                             error(I, "free type variables in datatype binding")
        in
            if isSome(TyConMap.find(TE', tycon)) then
                  (* Syntactic restriction [Section 2.9, 2nd bullet] *)
                  errorTyCon(I, "duplicate type constructor ", tycon)
            else
            ( VIdMap.unionWithi (fn(vid,_,_) =>
                  (* Syntactic restriction [Section 2.9, 2nd bullet] *)
                  errorVId(I, "duplicate data cnstructor ", vid)) (ClosVE,VE')
            , TyConMap.insert(TE', tycon, ((alphas,tau),ClosVE))
            )
        end


    (* Constructor Bindings *)

    and elabConBind(C,tau, ConBind(I, _, vid, ty_opt, conbind_opt)) =
        (* [Rule 29] *)
        let
            val tau1 = case ty_opt
                         of NONE    => tau
                          | SOME ty =>
                            let
                                val tau' = elabTy(C, ty)
                            in
                                Type.fromFunType(tau',tau)
                            end
            val VE   = case conbind_opt
                         of NONE         => VIdMap.empty
                          | SOME conbind => elabConBind(C,tau, conbind)
        in
            if isSome(VIdMap.find(VE, vid)) then
                (* Syntactic restriction [Section 2.9, 2nd bullet] *)
                errorVId(I, "duplicate data constructor ", vid)
            else if not(validConBindVId vid) then
                (* Syntactic restriction [Section 2.9, 5th bullet] *)
                errorVId(I, "illegal rebinding of identifier ", vid)
            else
                VIdMap.insert(VE, vid, (([],tau1),IdStatus.c))
        end


    (* Exception Bindings *)

    and elabExBind(C, NEWExBind(I, _, vid, ty_opt, exbind_opt)) =
        (* [Rule 30] *)
        let
            val tau1 = case ty_opt
                         of NONE    => InitialStaticEnv.tauExn
                          | SOME ty =>
                            let
                                val tau = elabTy(C, ty)
                            in
                                Type.fromFunType(tau, InitialStaticEnv.tauExn)
                            end
            val VE   = case exbind_opt
                         of NONE        => VIdMap.empty
                          | SOME exbind => elabExBind(C, exbind)
        in
            if isSome(VIdMap.find(VE, vid)) then
                (* Syntactic restriction [Section 2.9, 2nd bullet] *)
                errorVId(I, "duplicate exception constructor ", vid)
            else if not(validConBindVId vid) then
                (* Syntactic restriction [Section 2.9, 5th bullet] *)
                errorVId(I, "illegal rebinding of identifier ", vid)
            else
                VIdMap.insert(VE, vid, (([],tau1),IdStatus.e))
        end

      | elabExBind(C, EQUALExBind(I, _, vid, _, longvid, exbind_opt)) =
        (* [Rule 31] *)
        let
            val tau = case Context.findLongVId(C, longvid)
                        of SOME(([],tau),IdStatus.e) => tau
                         | SOME _ =>
                           errorLongVId(I, "non-exception identifier ", longvid)
                         | NONE =>
                           errorLongVId(I, "unknown identifier ", longvid)
            val VE  = case exbind_opt
                        of NONE        => VIdMap.empty
                         | SOME exbind => elabExBind(C, exbind)
        in
            if isSome(VIdMap.find(VE, vid)) then
                (* Syntactic restriction [Section 2.9, 2nd bullet] *)
                errorVId(I, "duplicate exception constructor ", vid)
            else
                VIdMap.insert(VE, vid, (([],tau),IdStatus.e))
        end


    (* Atomic Patterns *)

    and elabAtPat(C, WILDCARDAtPat(I)) =
        (* [Rule 32] *)
        ( VIdMap.empty, Type.invent() )

      | elabAtPat(C, SCONAtPat(I, scon)) =
        (* [Rule 33] *)
        (case scon
           of SCon.REAL _ =>
              (* Syntactic restriction [Section 2.9, 6th bullet] *)
              error(I, "real constant in pattern")
            | _ =>
              ( VIdMap.empty, typeSCon scon )
        )

      | elabAtPat(C, LONGVIDAtPat(I, _, longvid)) =
        (* [Rule 34 and 35] *)
        let
            val (strids,vid) = LongVId.explode longvid
        in
            if List.null strids andalso
               ( case Context.findVId(C, vid)
                   of NONE           => true
                    | SOME(sigma,is) => is = IdStatus.v ) then
                (* [Rule 34] *)
                let
                    val tau = Type.invent()
                in
                    ( VIdMap.singleton(vid, (([],tau),IdStatus.v))
                    , tau )
                end
            else
                (* [Rule 35] *)
                let
                    val (sigma,is) = case Context.findLongVId(C, longvid)
                                       of SOME valstr => valstr
                                        | NONE =>
                                          errorLongVId(I,"unknown constructor ",
                                                         longvid)
                    val  tau       = TypeScheme.instance sigma
                    (* Note that tau will always be a ConsType. *)
                in
                    if is <> IdStatus.v then
                        ( VIdMap.empty, tau )
                    else
                        error(I, "non-constructor long identifier in pattern")
                end
        end

      | elabAtPat(C, RECORDAtPat(I, patrow_opt)) =
        (* [Rule 36] *)
        let
            val (VE,rho) = case patrow_opt
                             of NONE        => ( VIdMap.empty, Type.emptyRho )
                              | SOME patrow => elabPatRow(C, patrow)
        in
            (VE, Type.fromRowType rho)
        end

      | elabAtPat(C, PARAtPat(I, pat)) =
        (* [Rule 37] *)
        let
            val (VE,tau) = elabPat(C, pat)
        in
            (VE,tau)
        end


    (* Pattern Rows *)

    and elabPatRow(C, WILDCARDPatRow(I)) =
        (* [Rule 38] *)
        ( VIdMap.empty, Type.inventRho() )

      | elabPatRow(C, ROWPatRow(I, lab, pat, patrow_opt)) =
        (* [Rule 39] *)
        let
            val (VE,tau)  = elabPat(C, pat)
            val (VE',rho) = case patrow_opt
                              of NONE        => ( VIdMap.empty, Type.emptyRho )
                               | SOME patrow => elabPatRow(C, patrow)
        in
            if isSome(Type.findLab(rho, lab)) then
                (* Syntactic restriction [Section 2.9, 1st bullet] *)
                errorLab(I, "duplicate label ", lab)
            else
                ( VIdMap.unionWithi (fn(vid,_,_) =>
                      errorVId(I, "duplicate variable ", vid)) (VE,VE')
                , Type.insertRho(rho, lab, tau)
                )
        end


    (* Patterns *)

    and elabPat(C, ATPATPat(I, atpat)) =
        (* [Rule 40] *)
        let
            val (VE,tau) = elabAtPat(C, atpat)
        in
            (VE,tau)
        end

      | elabPat(C, CONPat(I, _, longvid, atpat)) =
        (* [Rule 41] *)
        let
            val (sigma,is) = case Context.findLongVId(C, longvid)
                               of SOME valstr => valstr
                                | NONE =>
                                errorLongVId(I, "unknown constructor ", longvid)
            val _          = if is <> IdStatus.v then () else
                                errorLongVId(I, "non-constructor ", longvid)
            val (tau',tau) = case !(TypeScheme.instance sigma)
                               of Type.FunType(tau',tau) => (tau', tau)
                                | _ =>
                                errorLongVId(I,"misplaced nullary constructor ",
                                                longvid)
            val (VE,tau'2)  = elabAtPat(C, atpat)
        in
            Type.unify(tau',tau'2)
            handle Type.Unify =>
                   error(I, "type mismatch in constructor pattern")
          ; (VE,tau)
        end

      | elabPat(C, TYPEDPat(I, pat, ty)) =
        (* [Rule 42] *)
        let
            val (VE,tau1) = elabPat(C, pat)
            val     tau   = elabTy(C, ty)
        in
            Type.unify(tau1,tau)
            handle Type.Unify => error(I, "pattern does not match annotation")
          ; (VE,tau)
        end

      | elabPat(C, ASPat(I, _, vid, ty_opt, pat)) =
        (* [Rule 43] *)
        let
            val (VE1,tau1) = elabPat(C, pat)
            val (VE, tau)  =
                case ty_opt
                  of NONE    => (VE1,tau1)
                   | SOME ty =>
                     let
                         val tau = elabTy(C, ty)
                     in
                         Type.unify(tau1,tau)
                         handle Type.Unify =>
                                error(I, "pattern does not match annotation")
                       ; (VE1,tau)
                     end
        in
            if not( case Context.findVId(C, vid)
                      of NONE           => true
                       | SOME(sigma,is) => is = IdStatus.v ) then
                errorVId(I, "misplaced constructor ", vid)
            else if isSome(VIdMap.find(VE, vid)) then
                errorVId(I, "duplicate variable ", vid)
            else
                ( VIdMap.insert(VE, vid, (([],tau),IdStatus.v)), tau )
        end


    (* Type Expressions *)

    and elabTy(C, ty) = Type.normalise(elabTy'(C, ty))

    and elabTy'(C, TYVARTy(I, tyvar)) =
        (* [Rule 44] *)
        let
            val alpha = tyvar
        in
            Type.fromTyVar alpha
        end

      | elabTy'(C, RECORDTy(I, tyrow_opt)) =
        (* [Rule 45] *)
        let
            val rho = case tyrow_opt
                        of NONE       => Type.emptyRho
                         | SOME tyrow => elabTyRow'(C, tyrow)
        in
            Type.fromRowType rho
        end

      | elabTy'(C, TYCONTy(I, tyseq, longtycon)) =
        (* [Rule 46] *)
        let
            val Tyseq(I',tys) = tyseq
            val k             = List.length tys
            val taus          = List.map (fn ty => elabTy'(C, ty)) tys
            val (theta,VE)    =
                case Context.findLongTyCon(C, longtycon)
                  of SOME tystr => tystr
                   | NONE =>
                     errorLongTyCon(I, "unknown type constructor ", longtycon)
        in
            TypeFcn.apply(taus, theta)
            handle TypeFcn.Apply =>
                errorLongTyCon(I, "arity mismatch at type application ",
                                  longtycon)
        end

      | elabTy'(C, ARROWTy(I, ty, ty')) =
        (* [Rule 47] *)
        let
            val tau  = elabTy'(C, ty)
            val tau' = elabTy'(C, ty')
        in
            Type.fromFunType(tau,tau')
        end

      | elabTy'(C, PARTy(I, ty)) =
        (* [Rule 48] *)
        let
            val tau = elabTy'(C, ty)
        in
            tau
        end


    (* Type-expression Rows *)

    and elabTyRow'(C, TyRow(I, lab, ty, tyrow_opt)) =
        (* [Rule 49] *)
        let
            val tau = elabTy'(C, ty)
            val rho = case tyrow_opt
                        of NONE       => Type.emptyRho
                         | SOME tyrow => elabTyRow'(C, tyrow)
        in
            if isSome(Type.findLab(rho, lab)) then
                (* Syntactic restriction [Section 2.9, 1st bullet] *)
                errorLab(I, "duplicate label ", lab)
            else
                Type.insertRho(rho, lab, tau)
        end



    (* Build tentative VE from LHSs of recursive valbind *)

    and lhsRecValBind(PLAINValBind(I, pat, exp, valbind_opt)) =
        let
            val VE  = lhsRecValBindPat pat
            val VE' = case valbind_opt
                        of NONE         => VIdMap.empty
                         | SOME valbind => lhsRecValBind valbind
            val _   = case exp
                        of FNExp _ => ()
                         | _ =>
                           (* Syntactic restriction [Section 2.9, 4th bullet] *)
                           error(I, "illegal expression within recursive \
                                    \value binding")
        in
            VIdMap.unionWithi
                (fn(vid,_,_) =>
                       (* Syntactic restriction [Section 2.9, 2nd bullet] *)
                       errorVId(I, "duplicate variable ", vid))  (VE,VE')
        end

      | lhsRecValBind(RECValBind(I, valbind)) =
            lhsRecValBind valbind

    and lhsRecValBindPat(ATPATPat(I, atpat)) =
            lhsRecValBindAtPat atpat

      | lhsRecValBindPat(CONPat(I, _, longvid, atpat)) =
            lhsRecValBindAtPat atpat

      | lhsRecValBindPat(TYPEDPat(I, pat, ty)) =
            lhsRecValBindPat pat

      | lhsRecValBindPat(ASPat(I, _, vid, ty_opt, pat)) =
        let
            val VE = lhsRecValBindPat pat
        in
            if isSome(VIdMap.find(VE, vid)) then
                (* Syntactic restriction [Section 2.9, 2nd bullet] *)
                errorVId(I, "duplicate variable ", vid)
            else if not(validBindVId vid) then
                (* Syntactic restriction [Section 2.9, 5th bullet] *)
                errorVId(I, "illegal rebinding of identifier ", vid)
            else
                VIdMap.insert(VE, vid, (([],Type.invent()), IdStatus.v))
        end

    and lhsRecValBindAtPat(WILDCARDAtPat(I)) =
            VIdMap.empty

      | lhsRecValBindAtPat(SCONAtPat(I, scon)) =
            VIdMap.empty

      | lhsRecValBindAtPat(LONGVIDAtPat(I, _, longvid)) =
           (case LongVId.explode longvid
              of ([], vid) =>
                    if not(validBindVId vid) then
                        (* Syntactic restriction [Section 2.9, 5th bullet] *)
                        errorVId(I, "illegal rebinding of identifier ", vid)
                    else
                        VIdMap.singleton(vid, (([],Type.invent()),IdStatus.v))

               | _ => VIdMap.empty
           )

      | lhsRecValBindAtPat(RECORDAtPat(I, patrow_opt)) =
           (case patrow_opt
              of NONE        => VIdMap.empty
               | SOME patrow => lhsRecValBindPatRow patrow
           )

      | lhsRecValBindAtPat(PARAtPat(I, pat)) =
            lhsRecValBindPat pat

    and lhsRecValBindPatRow(WILDCARDPatRow(I)) =
            VIdMap.empty

      | lhsRecValBindPatRow(ROWPatRow(I, lab, pat, patrow_opt)) =
        let
            val VE = lhsRecValBindPat pat
        in
            case patrow_opt
              of NONE        => VE
               | SOME patrow =>
                 let
                     val VE' = lhsRecValBindPatRow patrow
                 in
                     VIdMap.unionWithi (fn(vid,_,_) =>
                         (* Syntactic restriction [Section 2.9, 2nd bullet] *)
                         errorVId(I, "duplicate variable ", vid)) (VE,VE')
                 end
        end



    (* Build tentative TE from LHSs of datbind *)

    and lhsDatBind(DatBind(I, tyvarseq, tycon, conbind, datbind_opt)) =
        (* [Rule 28, part 1] *)
        let
            val (U,alphas) = tyvars tyvarseq
            val k          = List.length alphas
            val span       = lhsConBind conbind
            val t          = TyName.tyname(tycon, k, TyName.EQ, span)
            val tau        = Type.fromConsType(List.map Type.fromTyVar alphas,t)
            val TE'        = case datbind_opt
                               of NONE         => TyConMap.empty
                                | SOME datbind => lhsDatBind datbind
        in
            if isSome(TyConMap.find(TE', tycon)) then
                (* Syntactic restriction [Section 2.9, 2nd bullet] *)
                errorTyCon(I, "duplicate type constructor ", tycon)
            else
                TyConMap.insert(TE', tycon, ((alphas,tau), VIdMap.empty))
        end

    and lhsConBind(ConBind(I, _, vid, ty_opt, conbind_opt)) =
        case conbind_opt
          of NONE         => 1
           | SOME conbind => 1 + lhsConBind conbind

  end
(* stop of ElabCore.sml *)
(* start of ELAB_MODULE.sml *)
(*
 * Standard ML modules elaboration
 *
 * Definition, sections 5.7 and 3.5
 *
 * Note:
 *     Elaboration also checks the syntactic restrictions [Section 3.5].
 *)

signature ELAB_MODULE =
  sig

    (* Import types *)

    type TopDec = GrammarModule.TopDec
    type Basis  = StaticBasis.Basis


    (* Export *)

    val elabTopDec:     Basis * TopDec -> Basis

  end
(* stop of ELAB_MODULE.sml *)
(* start of ElabModule.sml *)
(*
 * Standard ML modules elaboration
 *
 * Definition, sections 5.7 and 3.5
 *
 * Notes:
 *   - Elaboration also checks the syntactic restrictions [Section 3.5].
 *   - To implement the 3rd restriction in 4.11 some elab functions are
 *     passed an additional boolean argument to recognise being on the toplevel.
 *)

structure ElabModule :> ELAB_MODULE =
  struct

    (* Import *)

    type TopDec = GrammarModule.TopDec
    type Basis  = StaticBasis.Basis


    open GrammarModule


    (* Helpers for error messages *)

    val error = Error.error

    fun errorVId  (I, s, vid)   = error(I, s ^ VId.toString vid)
    fun errorTyCon(I, s, tycon) = error(I, s ^ TyCon.toString tycon)
    fun errorStrId(I, s, strid) = error(I, s ^ StrId.toString strid)
    fun errorSigId(I, s, sigid) = error(I, s ^ SigId.toString sigid)
    fun errorFunId(I, s, funid) = error(I, s ^ FunId.toString funid)

    fun errorLongTyCon(I, s, longtycon) =
        error(I, s ^ LongTyCon.toString longtycon)
    fun errorLongStrId(I, s, longstrid) =
        error(I, s ^ LongStrId.toString longstrid)


    (* Helpers for basis modification *)

    val plus    = StaticBasis.plus
    val plusT   = StaticBasis.plusT
    val oplusSE = StaticBasis.oplusSE
    val oplusG  = StaticBasis.oplusG
    val oplusF  = StaticBasis.oplusF
    val oplusE  = StaticBasis.oplusE

    infix plus plusT oplusG oplusF oplusE oplusSE



    (* Inference rules [Section 5.7] *)


    (* Structure Expressions *)

    fun elabStrExp(B, STRUCTStrExp(I, strdec)) =
        (* [Rule 50] *)
        let
            val E = elabStrDec(false, B, strdec)
        in
            E
        end

      | elabStrExp(B, LONGSTRIDStrExp(I, longstrid)) =
        (* [Rule 51] *)
        let
            val E = case StaticBasis.findLongStrId(B, longstrid)
                      of SOME(StaticEnv.Str E) => E
                       | NONE =>
                         errorLongStrId(I, "unknown structure ", longstrid)
        in
            E
        end

      | elabStrExp(B, TRANSStrExp(I, strexp, sigexp)) =
        (* [Rule 52] *)
        let
            val E      = elabStrExp(B, strexp)
            val Sigma  = elabSigExp(B, sigexp)
            val (E',_) = Sig.match(E, Sigma)
                         handle Sig.Match =>
                                error(I, "structure does not match constraint")
        in
            E'
        end

      | elabStrExp(B, OPAQStrExp(I, strexp, sigexp)) =
        (* [Rule 53] *)
        let
            val E       = elabStrExp(B, strexp)
            val (T',E') = Sig.rename(elabSigExp(B, sigexp))
            val (E'',_) = Sig.match(E, (T',E'))
                          handle Sig.Match =>
                                 error(I, "structure does not match constraint")
            (* Renaming ensures side condition on T' *)
        in
            E'
        end

      | elabStrExp(B, APPStrExp(I, funid, strexp)) =
        (* [Rule 54] *)
        let
            val E = elabStrExp(B, strexp)
            val (T1'',(E1'',(T1',E1'))) =
                      case StaticBasis.findFunId(B, funid)
                        of SOME Phi => Phi
                         | NONE     => errorFunId(I, "unknown functor ", funid)
            val (E'',phi) = Sig.match(E, (T1'',E1''))
                            handle Sig.Match =>
                                error(I, "structure does not match constraint")
            val (T',E')   = Sig.rename (T1', StaticEnv.realise phi E1')
            (* Renaming ensures side condition on T' *)
        in
            E'
        end

      | elabStrExp(B, LETStrExp(I, strdec, strexp)) =
        (* [Rule 55] *)
        let
            val E1 = elabStrDec(false, B, strdec)
            val E2 = elabStrExp(B oplusE E1, strexp)
        in
            E2
        end


    (* Structure-level Declarations *)

    and elabStrDec(toplevel, B, DECStrDec(I, dec)) =
        (* [Rule 56] *)
        let
            val E = ElabCore.elabDec(toplevel, StaticBasis.Cof B, dec)
        in
            E
        end

      | elabStrDec(toplevel, B, STRUCTUREStrDec(I, strbind)) =
        (* [Rule 57] *)
        let
            val SE = elabStrBind(B, strbind)
        in
            StaticEnv.fromSE SE
        end

      | elabStrDec(toplevel, B, LOCALStrDec(I, strdec1, strdec2)) =
        (* [Rule 58] *)
        let
            val E1 = elabStrDec(false, B, strdec1)
            val E2 = elabStrDec(false, B oplusE E1, strdec2)
        in
            E2
        end

      | elabStrDec(toplevel, B, EMPTYStrDec(I)) =
        (* [Rule 59] *)
        StaticEnv.empty

      | elabStrDec(toplevel, B, SEQStrDec(I, strdec1, strdec2)) =
        (* [Rule 60] *)
        let
            val E1 = elabStrDec(toplevel, B, strdec1)
            val E2 = elabStrDec(toplevel, B oplusE E1, strdec2)
        in
            StaticEnv.plus(E1,E2)
        end


    (* Structure Bindings *)

    and elabStrBind(B, StrBind(I, strid, strexp, strbind_opt)) =
        (* [Rule 61] *)
        let
            val E  = elabStrExp(B, strexp)
            val SE = case strbind_opt
                       of NONE         => StrIdMap.empty
                        | SOME strbind =>
                          elabStrBind(B plusT StaticEnv.tynames E, strbind)
        in
            if isSome(StrIdMap.find(SE, strid)) then
                (* Syntactic restriction [Section 3.5, 1st bullet] *)
                errorStrId(I, "duplicate structure identifier ", strid)
            else
                StrIdMap.insert(SE, strid, StaticEnv.Str E)
        end


    (* Signature Expressions *)

    and elabSigExpE(B, SIGSigExp(I, spec)) =
        (* [Rule 62] *)
        let
            val E = elabSpec(B, spec)
        in
            E
        end

      | elabSigExpE(B, SIGIDSigExp(I, sigid)) =
        (* [Rule 63] *)
        let
            val (T,E) = case StaticBasis.findSigId(B, sigid)
                          of SOME Sigma => Sig.rename Sigma
                           | NONE => errorSigId(I, "unknown signature ",sigid)
        in
            E
        end

      | elabSigExpE(B, WHERETYPESigExp(I, sigexp, tyvarseq, longtycon, ty)) =
        (* [Rule 64] *)
        let
            val E      = elabSigExpE(B, sigexp)
            val alphas = #2(ElabCore.tyvars tyvarseq)
            val tau    = ElabCore.elabTy(StaticBasis.Cof B, ty)
            val t      = case StaticEnv.findLongTyCon(E,longtycon)
                           of NONE =>
                              errorLongTyCon(I, "unknown type ", longtycon)
                            | SOME(theta,VE) =>
                         case TypeFcn.toTyName theta
                           of NONE =>
                              errorLongTyCon(I, "non-flexible type ", longtycon)
                            | SOME t => t
            val  _     = if not(TyNameSet.member(StaticBasis.Tof B, t)) then ()
                         else errorLongTyCon(I, "rigid type ", longtycon)
            val phi    = TyNameMap.singleton(t, (alphas,tau))
            val  _     = if TyName.equality t = TyName.NOEQ
                         orelse TypeFcn.admitsEquality (alphas,tau) then () else
                          error(I, "type realisation does not respect equality")
            val E'     = StaticEnv.realise phi E
            val  _     = if StaticEnv.isWellFormed E' then () else
                          error(I, "type realisation does not respect datatype")
        in
            E'
        end

    and elabSigExp(B, sigexp) =
        (* [Rule 65] *)
        let
            val E = elabSigExpE(B, sigexp)
            val T = TyNameSet.difference(StaticEnv.tynames E, StaticBasis.Tof B)
        in
            (T,E)
        end


    (* Signature Declarations *)

    and elabSigDec(B, SigDec(I, sigbind)) =
        (* [Rule 66] *)
        let
            val G = elabSigBind(B, sigbind)
        in
            G
        end


    (* Signature Bindings *)

    and elabSigBind(B, SigBind(I, sigid, sigexp, sigbind_opt)) =
        (* [Rule 67] *)
        let
            val Sigma = elabSigExp(B, sigexp)
            val G     = case sigbind_opt
                          of NONE         => SigIdMap.empty
                           | SOME sigbind => elabSigBind(B, sigbind)
        in
            if isSome(SigIdMap.find(G, sigid)) then
                (* Syntactic restriction [Section 3.5, 1st bullet] *)
                errorSigId(I, "duplicate signature identifier ", sigid)
            else
                SigIdMap.insert(G, sigid, Sigma)
        end


    (* Specifications *)

    and elabSpec(B, VALSpec(I, valdesc)) =
        (* [Rule 68] *)
        let
            val VE = elabValDesc(StaticBasis.Cof B, valdesc)
        in
            StaticEnv.fromVE(StaticEnv.Clos VE)
        end

      | elabSpec(B, TYPESpec(I, typdesc)) =
        (* [Rule 69] *)
        let
            val TE = elabTypDesc(StaticBasis.Cof B, typdesc)
            (* Side condition on type names is always ensured. *)
        in
            StaticEnv.fromTE TE
        end

      | elabSpec(B, EQTYPESpec(I, typdesc)) =
        (* [Rule 70] *)
        let
            val TE = elabTypDesc(StaticBasis.Cof B, typdesc)
            val _  = StaticEnv.makeEquality TE
        in
            StaticEnv.fromTE TE
        end

      | elabSpec(B, DATATYPESpec(I, datdesc)) =
        (* [Rule 71] *)
        let
            val      TE1  = lhsDatDesc datdesc
            val (VE2,TE2) = elabDatDesc(Context.oplusTE(StaticBasis.Cof B,TE1),
                                        datdesc)
            val (TE, VE)  = StaticEnv.maximiseEquality(TE2,VE2)
            (* Side condition on type names is always ensured. *)
        in
            StaticEnv.fromVEandTE(VE,TE)
        end

      | elabSpec(B, REPLICATIONSpec(I, tycon, longtycon)) =
        (* [Rule 72] *)
        let
            val (theta,VE) = case StaticBasis.findLongTyCon(B, longtycon)
                              of SOME tystr => tystr
                               | NONE =>
                                 errorLongTyCon(I, "unknown type ", longtycon)
            val  TE        = TyConMap.singleton(tycon, (theta,VE))
        in
            StaticEnv.fromVEandTE(VE,TE)
        end

      | elabSpec(B, EXCEPTIONSpec(I, exdesc)) =
        (* [Rule 73] *)
        let
            val VE = elabExDesc(StaticBasis.Cof B, exdesc)
        in
            StaticEnv.fromVE VE
        end

      | elabSpec(B, STRUCTURESpec(I, strdesc)) =
        (* [Rule 74] *)
        let
            val SE = elabStrDesc(B, strdesc)
        in
            StaticEnv.fromSE SE
        end

      | elabSpec(B, INCLUDESpec(I, sigexp)) =
        (* [Rule 75] *)
        let
            val E = elabSigExpE(B, sigexp)
        in
            E
        end

      | elabSpec(B, EMPTYSpec(I)) =
        (* [Rule 76] *)
        StaticEnv.empty

      | elabSpec(B, SEQSpec(I, spec1, spec2)) =
        (* [Rule 77] *)
        let
            val E1 = elabSpec(B, spec1)
            val E2 = elabSpec(B oplusE E1, spec2)
            val _  = if StaticEnv.disjoint(E1,E2) then () else
                     error(I, "duplicate specifications in signature")
        in
            StaticEnv.plus(E1,E2)
        end

      | elabSpec(B, SHARINGTYPESpec(I, spec, longtycons)) =
        (* [Rule 78] *)
        let
            val E  = elabSpec(B, spec)
            val ts =
                List.map
                (fn longtycon =>
                 case StaticEnv.findLongTyCon(E, longtycon)
                   of NONE =>
                        errorLongTyCon(I, "unknown type ", longtycon)
                    | SOME(theta,VE) =>
                 case TypeFcn.toTyName theta
                   of NONE =>
                        errorLongTyCon(I, "non-flexible type ", longtycon)
                    | SOME t =>
                      if TyNameSet.member(StaticBasis.Tof B, t) then
                        errorLongTyCon(I, "rigid type ", longtycon)
                      else
                        t
                )
                longtycons
            val equality = if List.exists
                                (fn t => TyName.equality t <> TyName.NOEQ) ts
                           then TyName.EQ
                           else TyName.NOEQ
            val span  = List.foldl
                                (fn(t, span) => Int.max(TyName.span t, span))
                                0 ts
            val t1    = List.hd ts
            val t     = TyName.tyname(TyName.tycon t1, TyName.arity t1,
                                      equality, span)
            val theta = TypeFcn.fromTyName t
            val phi   = List.foldl
                            (fn(ti, phi) => TyNameMap.insert(phi, ti, theta))
                            TyNameMap.empty ts
        in
            StaticEnv.realise phi E
        end

      | elabSpec(B, SHARINGSpec(I, spec, longstrids)) =
        (* [Appendix A] *)
        let
            fun shareFlexibleTyName(t1, t2, phi) =
                let
                    val equality = if TyName.equality t1 <> TyName.NOEQ
                                   orelse TyName.equality t2 <> TyName.NOEQ
                                   then TyName.EQ
                                   else TyName.NOEQ
                    val t        = TyName.tyname(TyName.tycon t1,
                                                 TyName.arity t1,
                                                 equality,
                                                 Int.max(TyName.span t1,
                                                         TyName.span t2))
                    val theta    = TypeFcn.fromTyName t
                in
                    TyNameMap.insert(TyNameMap.insert(phi,
                        t1, theta),
                        t2, theta)
                end

            fun shareTE(TE1, TE2, phi) =
                TyConMap.foldli
                    (fn(tycon, (theta1,VE1), phi) =>
                        case TyConMap.find(TE2, tycon)
                          of NONE             => phi
                           | SOME(theta2,VE2) =>
                        case (TypeFcn.toTyName(TypeFcn.realise phi theta1),
                              TypeFcn.toTyName(TypeFcn.realise phi theta2))
                          of (SOME t1, SOME t2) =>
                             if TyNameSet.member(StaticBasis.Tof B, t1)
                             orelse TyNameSet.member(StaticBasis.Tof B,t2) then
                                errorTyCon(I, "structure contains rigid type ",
                                              tycon)
                             else
                                shareFlexibleTyName(t1, t2, phi)
                           | _ =>
                             errorTyCon(I, "structure contains non-flexible \
                                           \type ", tycon)
                    )
                    phi TE1

            fun shareSE(SE1, SE2, phi) =
                StrIdMap.foldli
                    (fn(strid, StaticEnv.Str E1, phi) =>
                        case StrIdMap.find(SE2, strid)
                          of NONE                   => phi
                           | SOME(StaticEnv.Str E2) => shareE(E1, E2, phi)
                    )
                    phi SE1

            and shareE((SE1,TE1,VE1), (SE2,TE2,VE2), phi) =
                let
                    val phi'  = shareTE(TE1, TE2, phi)
                    val phi'' = shareSE(SE1, SE2, phi')
                in
                    phi''
                end

            fun share1(E1,   [],   phi) = phi
              | share1(E1, E2::Es, phi) =
                let
                    val phi' = shareE(E1, E2, phi)
                in
                    share1(E1, Es, phi')
                end

            fun shareAll( [],   phi) = phi
              | shareAll(E::Es, phi) =
                let
                    val phi' = share1(E, Es, phi)
                in
                    shareAll(Es, phi')
                end

            val E   = elabSpec(B, spec)
            val Es  = List.map
                        (fn longstrid =>
                         case StaticEnv.findLongStrId(E, longstrid)
                           of SOME(StaticEnv.Str E') => E'
                            | NONE =>
                              errorLongStrId(I, "unknown structure ", longstrid)
                        ) longstrids
            val phi = shareAll(Es, TyNameMap.empty)
        in
            StaticEnv.realise phi E
        end


    (* Value Descriptions *)

    and elabValDesc(C, ValDesc(I, vid, ty, valdesc_opt)) =
        (* [Rule 79] *)
        let
            val tau = ElabCore.elabTy(C, ty)
            val VE  = case valdesc_opt
                        of NONE         => VIdMap.empty
                         | SOME valdesc => elabValDesc(C, valdesc)
        in
            if isSome(VIdMap.find(VE, vid)) then
                (* Syntactic restriction [Section 3.5, 2nd bullet] *)
                errorVId(I, "duplicate variable ", vid)
            else if not(ElabCore.validBindVId vid) then
                (* Syntactic restriction [Section 3.5, 5th bullet] *)
                errorVId(I, "illegal specification of identifier ", vid)
            else
                VIdMap.insert(VE, vid, (([],tau),IdStatus.v))
        end


    (* Type Descriptions *)

    and elabTypDesc(C, TypDesc(I, tyvarseq, tycon, typdesc_opt)) =
        (* [Rule 80] *)
        let
            val alphas = #2(ElabCore.tyvars tyvarseq)
            val k      = List.length alphas
            val t      = TyName.tyname(tycon, k, TyName.NOEQ, 0)
            val TE     = case typdesc_opt
                           of NONE         => TyConMap.empty
                            | SOME typdesc => elabTypDesc(C, typdesc)
                              (* Side condition on t is always true. *)
            val tau    = Type.fromConsType (List.map Type.fromTyVar alphas, t)
        in
            if isSome(TyConMap.find(TE, tycon)) then
                (* Syntactic restriction [Section 3.5, 2nd bullet] *)
                errorTyCon(I, "duplicate type constructor ", tycon)
            else
                TyConMap.insert(TE, tycon, ((alphas,tau),VIdMap.empty))
        end


    (* Datatype Descriptions *)

    and elabDatDesc(C, DatDesc(I, tyvarseq, tycon, condesc, datdesc_opt)) =
        (* [Rule 81, part 2] *)
        let
            val (U,alphas)   = ElabCore.tyvars tyvarseq
            val (alphas,tau) = case Context.findTyCon(C, tycon)
                                 of SOME(theta,VE) => theta
                                  | NONE => (* lhsDatDesc inserted it! *)
                                    raise Fail "ElabCore.elabDatDesc: \
                                                \tycon not pre-bound"
            val VE       = elabConDesc(C,tau, condesc)
            val(VE',TE') = case datdesc_opt
                             of NONE         => ( VIdMap.empty, TyConMap.empty )
                              | SOME datdesc => elabDatDesc(C, datdesc)
                                (* Side condition on t is always true. *)
            val ClosVE   = if TyVarSet.isSubset(StaticEnv.tyvarsVE VE, U) then
                             StaticEnv.Clos VE
                           else
                             (* Syntactic restriction [Section 3.5,4th bullet]*)
                             error(I, "free type variables \
                                      \in datatype description")
        in
            if isSome(TyConMap.find(TE', tycon)) then
                  (* Syntactic restriction [Section 3.5, 2nd bullet] *)
                  errorTyCon(I, "duplicate type constructor ", tycon)
            else
            ( VIdMap.unionWithi (fn(vid,_,_) =>
                  (* Syntactic restriction [Section 3.5, 2nd bullet] *)
                  errorVId(I, "duplicate data cnstructor ", vid)) (ClosVE,VE')
            , TyConMap.insert(TE', tycon, ((alphas,tau),ClosVE))
            )
        end


    (* Constructor Descriptions *)

    and elabConDesc(C,tau, ConDesc(I, vid, ty_opt, condesc_opt)) =
        (* [Rule 82] *)
        let
            val tau1 = case ty_opt
                         of NONE    => tau
                          | SOME ty =>
                            let
                                val tau' = ElabCore.elabTy(C, ty)
                            in
                                Type.fromFunType(tau',tau)
                            end
            val VE   = case condesc_opt
                         of NONE         => VIdMap.empty
                          | SOME condesc => elabConDesc(C,tau, condesc)
        in
            if isSome(VIdMap.find(VE, vid)) then
                (* Syntactic restriction [Section 3.5, 2nd bullet] *)
                errorVId(I, "duplicate data constructor ", vid)
            else if not(ElabCore.validConBindVId vid) then
                (* Syntactic restriction [Section 3.5, 5th bullet] *)
                errorVId(I, "illegal specifiation of identifier ", vid)
            else
                VIdMap.insert(VE, vid, (([],tau1),IdStatus.c))
        end


    (* Exception Description *)

    and elabExDesc(C, ExDesc(I, vid, ty_opt, exdesc_opt)) =
        (* [Rule 83] *)
        let
            val tau1 = case ty_opt
                         of NONE    => InitialStaticEnv.tauExn
                          | SOME ty =>
                            let
                                val tau = ElabCore.elabTy(C, ty)
                                val  _  = if TyVarSet.isEmpty(Type.tyvars tau)
                                          then () else
                                          error(I, "free type variables \
                                                   \in exception description")
                            in
                                Type.fromFunType(tau, InitialStaticEnv.tauExn)
                            end
            val VE   = case exdesc_opt
                         of NONE        => VIdMap.empty
                          | SOME exdesc => elabExDesc(C, exdesc)
        in
            if isSome(VIdMap.find(VE, vid)) then
                (* Syntactic restriction [Section 3.5, 2nd bullet] *)
                errorVId(I, "duplicate exception constructor ", vid)
            else if not(ElabCore.validConBindVId vid) then
                (* Syntactic restriction [Section 3.5, 5th bullet] *)
                errorVId(I, "illegal specification of identifier ", vid)
            else
                VIdMap.insert(VE, vid, (([],tau1),IdStatus.e))
        end


    (* Structure Descriptions *)

    and elabStrDesc(B, StrDesc(I, strid, sigexp, strdesc_opt)) =
        (* [Rule 84] *)
        let
            val E  = elabSigExpE(B, sigexp)
            val SE = case strdesc_opt
                       of NONE         => StrIdMap.empty
                        | SOME strdesc =>
                          elabStrDesc(B plusT StaticEnv.tynames E, strdesc)
        in
            if isSome(StrIdMap.find(SE, strid)) then
                (* Syntactic restriction [Section 3.5, 2nd bullet] *)
                errorStrId(I, "duplicate structure identifier ", strid)
            else
                StrIdMap.insert(SE, strid, StaticEnv.Str E)
        end


    (* Functor Declarations *)

    and elabFunDec(B, FunDec(I, funbind)) =
        (* [Rule 85] *)
        let
            val F = elabFunBind(B, funbind)
        in
            F
        end


    (* Functor Bindings *)

    and elabFunBind(B, FunBind(I, funid, strid, sigexp, strexp, funbind_opt)) =
        (* [Rule 86] *)
        let
            val (T,E) = elabSigExp(B, sigexp)
            val  E'   = elabStrExp(
                           B oplusSE StrIdMap.singleton(strid,StaticEnv.Str E),
                           strexp)
            (* Side condition on T is always ensured. *)
            val T'    = TyNameSet.difference(StaticEnv.tynames E',
                                TyNameSet.union(StaticBasis.Tof B, T))
            val F     = case funbind_opt
                          of NONE         => FunIdMap.empty
                           | SOME funbind => elabFunBind(B, funbind)
        in
            if isSome(FunIdMap.find(F, funid)) then
                (* Syntactic restriction [Section 3.5, 1st bullet] *)
                errorFunId(I, "duplicate functor identifier ", funid)
            else
                FunIdMap.insert(F, funid, (T,(E,(T',E'))))
        end


    (* Top-level Declarations *)

    and elabTopDec(B, STRDECTopDec(I, strdec, topdec_opt)) =
        (* [Rule 87] *)
        let
            val E   = elabStrDec(true, B, strdec)
            val B'  = case topdec_opt
                        of NONE        => StaticBasis.empty
                         | SOME topdec => elabTopDec(B oplusE E, topdec)
            val B'' = StaticBasis.plus
                        (StaticBasis.fromTandE(StaticEnv.tynames E, E), B')
        in
            if TyVarSet.isEmpty(StaticBasis.tyvars B'') then
                B''
            else
                error(I, "free type variables on top-level")
        end

      | elabTopDec(B, SIGDECTopDec(I, sigdec, topdec_opt)) =
        (* [Rule 88] *)
        let
            val G   = elabSigDec(B, sigdec)
            val B'  = case topdec_opt
                        of NONE        => StaticBasis.empty
                         | SOME topdec => elabTopDec(B oplusG G, topdec)
            val B'' = StaticBasis.plus
                        (StaticBasis.fromTandG(StaticBasis.tynamesG G, G), B')
        in
            B''
        end

      | elabTopDec(B, FUNDECTopDec(I, fundec, topdec_opt)) =
        (* [Rule 89] *)
        let
            val F   = elabFunDec(B, fundec)
            val B'  = case topdec_opt
                        of NONE        => StaticBasis.empty
                         | SOME topdec => elabTopDec(B oplusF F, topdec)
            val B'' = StaticBasis.plus
                        (StaticBasis.fromTandF(StaticBasis.tynamesF F, F), B')
        in
            if TyVarSet.isEmpty(StaticBasis.tyvars B'') then
                B''
            else
                error(I, "free type variables on top-level")
        end



    (* Build tentative TE from LHSs of datdesc *)

    and lhsDatDesc(DatDesc(I, tyvarseq, tycon, condesc, datdesc_opt)) =
        (* [Rule 81, part 1] *)
        let
            val (U,alphas) = ElabCore.tyvars tyvarseq
            val k          = List.length alphas
            val span       = lhsConDesc condesc
            val t          = TyName.tyname(tycon, k, TyName.EQ, span)
            val tau        = Type.fromConsType(List.map Type.fromTyVar alphas,t)
            val TE'        = case datdesc_opt
                               of NONE         => TyConMap.empty
                                | SOME datdesc => lhsDatDesc datdesc
        in
            if isSome(TyConMap.find(TE', tycon)) then
                (* Syntactic restriction [Section 3.5, 2nd bullet] *)
                errorTyCon(I, "duplicate type constructor ", tycon)
            else
                TyConMap.insert(TE', tycon, ((alphas,tau), VIdMap.empty))
        end

    and lhsConDesc(ConDesc(I, vid, ty_opt, condesc_opt)) =
        case condesc_opt
          of NONE         => 1
           | SOME condesc => 1 + lhsConDesc condesc

  end
(* stop of ElabModule.sml *)
(* start of PP_TYPE.sml *)
(*
 * Standard ML pretty printing of types and type schemes
 *)

signature PP_TYPE =
  sig

    type doc        = PrettyPrint.doc
    type Type       = Type.Type
    type TypeScheme = TypeScheme.TypeScheme

    val ppType:         Type -> doc
    val ppTypeScheme:   TypeScheme -> doc

  end
(* stop of PP_TYPE.sml *)
(* start of PPType.sml *)
(*
 * Standard ML pretty printing of types and type schemes
 *)

structure PPType :> PP_TYPE =
  struct

    (* Import *)

    type TypeScheme = TypeScheme.TypeScheme

    open Type
    open PrettyPrint
    open PPMisc

    infixr ^^ ^/^


    (* Simple objects *)

    fun ppLab lab     = text(Lab.toString lab)
    fun ppTyVar alpha = text(TyVar.toString alpha)
    fun ppTyName t    = text(TyName.toString t)

    fun ppOverloadingClass O =
        let
            val T  = OverloadingClass.set O
            val t  = OverloadingClass.default O
            val ts = t :: TyNameSet.listItems(TyNameSet.delete(T,t))
        in
            brack(ppCommaList ppTyName ts)
        end


    fun ppRowVar CLOSEDRow  = empty
      | ppRowVar(FLEXRow _) = text "," ^^ break ^^ text "..."


    (* Types *)

    (* Precedence:
     *  0 : function arrow (ty1 -> ty2)
     *  1 : tuple (ty1 * ... * tyn)
     *  2 : constructed type (tyseq tycon)
     *)

    fun ppType tau = fbox(below(nest(ppTypePrec 0 tau)))

    and ppTypePrec p (ref tau')        = ppType'Prec p tau'

    and ppType'Prec p (TyVar(alpha))   = ppTyVar alpha

      | ppType'Prec p (RowType(Rho,r)) =
        let
            fun isTuple(   [],     n) = n > 2
              | isTuple(lab::labs, n) =
                    lab = Lab.fromInt n andalso isTuple(labs, n+1)

            val labtaus     = LabMap.listItemsi Rho
            val (labs,taus) = ListPair.unzip labtaus
        in
            if r = CLOSEDRow andalso List.null labs then
                text "unit"
            else if r = CLOSEDRow andalso isTuple(labs, 1) then
                let
                    val doc = ppStarList (ppTypePrec 2) taus
                in
                    if p > 1 then
                        paren doc
                    else
                        fbox(below(nest doc))
                end
            else
                brace(ppCommaList ppLabType labtaus ^^ ppRowVar r)
        end

      | ppType'Prec p (FunType(tau1,tau2)) =
        let
            val doc = ppTypePrec 1 tau1 ^/^
                      text "->" ^/^
                      ppTypePrec 0 tau2
        in
            if p > 0 then
                paren doc
            else
                doc
        end

      | ppType'Prec p (ConsType(taus,t)) =
            fbox(nest(ppSeqPrec ppTypePrec 2 taus ^/^ ppTyName t))

      | ppType'Prec p (Overloaded(O)) =
            text "'" ^^ ppOverloadingClass O

      | ppType'Prec p (Link tau) =
            ppTypePrec p tau

    and ppLabType(lab, tau) =
            abox(
                hbox(
                    ppLab lab ^/^
                    text ":"
                ) ^^
                below(nest(break ^^
                    ppType tau
                ))
            )


    (* Type schemes *)

    fun ppTypeScheme sigma =
        let
            val (alphas,tau) = TypeScheme.normalise sigma
        in
            ppType tau
        end

  end
(* stop of PPType.sml *)
(* start of PP_STATIC_ENV.sml *)
(*
 * Standard ML pretty printing of the static environment
 *)

signature PP_STATIC_ENV =
  sig

    type doc       = PrettyPrint.doc
    type ValEnv    = StaticEnv.ValEnv
    type TyEnv     = StaticEnv.TyEnv
    type Env       = StaticEnv.Env
    type TyNameSet = TyNameSet.set

    val ppEnv:   Env -> doc
    val ppSig:   TyNameSet * Env -> doc

    val ppTyEnv: TyNameSet * TyEnv -> doc
    val ppExEnv: ValEnv -> doc

  end
(* stop of PP_STATIC_ENV.sml *)
(* start of PPStaticEnv.sml *)
(*
 * Standard ML pretty printing of the static environment
 *)

structure PPStaticEnv :> PP_STATIC_ENV =
  struct

    (* Import *)

    type ValEnv    = StaticEnv.ValEnv
    type TyEnv     = StaticEnv.TyEnv
    type Env       = StaticEnv.Env
    type TyNameSet = TyNameSet.set

    open PrettyPrint
    open PPMisc

    infixr ^^ ^/^


    (* Simple objects *)

    fun ppVId vid     = text(VId.toString vid)
    fun ppTyCon tycon = text(TyCon.toString tycon)
    fun ppTyVar alpha = text(TyVar.toString alpha)
    fun ppStrId strid = text(StrId.toString strid)

    fun ppTyName t    = text(TyName.toString t)


    (* Environments *)

    fun ppConTypeScheme (_, ref(Type.FunType(tau,_))) =
            text "of" ^^ break ^^ PPType.ppType tau

      | ppConTypeScheme _ = empty


    fun ppValEnv VE =
        VIdMap.foldri
            (fn(vid, (sigma,IdStatus.v), doc) =>
                abox(
                    hbox(
                        text "val" ^/^
                        ppVId vid ^/^
                        text ":"
                    ) ^^
                    nest(break ^^
                        abox(PPType.ppTypeScheme sigma)
                    )
                ) ^/^
                doc

             | (vid, (sigma,_), doc) => doc
            )
            empty VE

    fun ppExEnv VE =
        VIdMap.foldri
            (fn(vid, (sigma,IdStatus.e), doc) =>
                abox(
                    hbox(
                        text "exception" ^/^
                        ppVId vid
                    ) ^^
                    nest(break ^^
                        abox(ppConTypeScheme sigma)
                    )
                ) ^/^
                doc

             | (vid, (sigma,_), doc) => doc
            )
            empty VE

    fun ppConEnv VE =
        VIdMap.foldli
            (fn(vid, (sigma,_), doc) =>
                doc ^/^
                abox(
                    hbox(
                        (if isEmpty doc then empty else text "|") ^/^
                        ppVId vid
                    ) ^^
                    nest(text "" ^/^
                        abox(ppConTypeScheme sigma)
                    )
                )
            )
            empty VE


    fun absTy(T, tycon, theta) =
        case TypeFcn.toTyName theta
          of NONE    => NONE
           | SOME t  => if TyName.tycon t = tycon
                        andalso TyNameSet.member(T, t) then
                            SOME(TyName.equality t <> TyName.NOEQ)
                        else
                            NONE

    fun ppAbsTyEnv(T,TE) =
        TyConMap.foldri
            (fn(tycon, (theta as (alphas,tau), VE), doc) =>
                if VIdMap.isEmpty VE then
                case absTy(T, tycon, theta)
                 of NONE    => doc
                  | SOME eq =>
                    abox(
                        hbox(
                            text(if eq then "eqtype" else "type") ^/^
                            ppSeq ppTyVar alphas ^/^
                            ppTyCon tycon
                        )
                    ) ^/^
                    doc
                else
                    doc
            )
            empty TE

    fun ppSynTyEnv(T,TE) =
        TyConMap.foldri
            (fn(tycon, (theta as (alphas,tau), VE), doc) =>
                if VIdMap.isEmpty VE
                andalso not(isSome(absTy(T, tycon, theta))) then
                    abox(
                        hbox(
                            text "type" ^/^
                            ppSeq ppTyVar alphas ^/^
                            ppTyCon tycon ^/^
                            text "="
                        ) ^^
                        nest(break ^^
                            abox(PPType.ppType tau)
                        )
                    ) ^/^
                    doc
                else
                    doc
            )
            empty TE

    fun ppDataTyEnv TE =
        TyConMap.foldri
            (fn(tycon, ((alphas,tau),VE), doc) =>
                if VIdMap.isEmpty VE then
                    doc
                else
                    abox(
                        hbox(
                            text "datatype" ^/^
                            ppSeq ppTyVar alphas ^/^
                            ppTyCon tycon ^/^
                            text "="
                        ) ^^
                        nest(break ^^
                            abox(ppConEnv VE)
                        )
                    ) ^/^
                    doc
            )
            empty TE

    fun ppTyEnv(T,TE) =
            vbox(
                ppAbsTyEnv(T,TE) ^/^
                ppSynTyEnv(T,TE) ^/^
                ppDataTyEnv TE
            )

    fun ppStrEnv(T,SE) =
        StrIdMap.foldri
            (fn(strid, StaticEnv.Str E, doc) =>
                abox(
                    hbox(
                        text "structure" ^/^
                        ppStrId strid ^/^
                        text ":"
                    ) ^^
                    nest(break ^^
                        ppSig (T,E)
                    )
                ) ^/^
                doc
            )
            empty SE

    and ppEnv'(T,(SE,TE,VE)) =
            vbox(
                ppStrEnv(T,SE) ^/^
                ppTyEnv(T,TE) ^/^
                ppExEnv VE ^/^
                ppValEnv VE
            )

    and ppEnv E = ppEnv'(TyNameSet.empty,E)


    (* Signatures *)

    and ppSig (T,E) =
        let
            val doc = ppEnv'(T, E)
        in
            abox(below(
                text "sig" ^^
                brace(ppCommaList ppTyName (TyNameSet.listItems T)) ^^
                (if isEmpty doc then
                    empty
                 else
                    nest(vbox(break ^^ doc))
                ) ^^ break ^^
                text "end"
            ))
        end

  end
(* stop of PPStaticEnv.sml *)
(* start of PP_STATIC_BASIS.sml *)
(*
 * Standard ML pretty printing of the static basis
 *)

signature PP_STATIC_BASIS =
  sig

    type doc    = PrettyPrint.doc
    type Basis  = StaticBasis.Basis
    type SigEnv = StaticBasis.SigEnv
    type FunEnv = StaticBasis.FunEnv

    val ppBasis:   Basis  -> doc
    val ppSigEnv:  SigEnv -> doc
    val ppFunEnv:  FunEnv -> doc

  end
(* stop of PP_STATIC_BASIS.sml *)
(* start of PPStaticBasis.sml *)
(*
 * Standard ML pretty printing of the static basis
 *)

structure PPStaticBasis :> PP_STATIC_BASIS =
  struct

    (* Import *)

    type Basis  = StaticBasis.Basis
    type SigEnv = StaticBasis.SigEnv
    type FunEnv = StaticBasis.FunEnv

    open PrettyPrint
    open PPMisc

    infixr ^^ ^/^


    (* Simple objects *)

    fun ppSigId sigid = text(SigId.toString sigid)
    fun ppFunId funid = text(FunId.toString funid)


    (* Environments *)

    fun ppSigEnv G =
        SigIdMap.foldri
            (fn(sigid, Sigma, doc) =>
                abox(
                    hbox(
                        text "signature" ^/^
                        ppSigId sigid ^/^
                        text "="
                    ) ^^
                    nest(break ^^
                        PPStaticEnv.ppSig Sigma
                    )
                ) ^/^
                doc
            )
            empty G

    fun ppFunEnv F =
        FunIdMap.foldri
            (fn(funid, (T,(E,Sigma)), doc) =>
                abox(
                    hbox(
                        text "functor" ^/^
                        ppFunId funid
                    ) ^^
                    nest(ebreak ^^
                        abox(
                            hbox(
                                text "(" ^^
                                text "Arg" ^/^
                                text ":"
                            ) ^^
                            nest(break ^^
                                PPStaticEnv.ppSig(T,E)
                            ) ^^ ebreak ^^
                            hbox(
                                text ")" ^/^
                                text ":"
                            )
                        ) ^/^
                        PPStaticEnv.ppSig Sigma
                    )
                ) ^/^
                doc
            )
            empty F


    (* Basis *)

    fun ppBasis (T,F,G,E) =
            vbox(
                ppSigEnv G ^/^
                ppFunEnv F ^/^
                PPStaticEnv.ppEnv E ^/^
                text ""
            )

  end
(* stop of PPStaticBasis.sml *)
(* start of PP_ENV.sml *)
(*
 * Standard ML pretty printing of the combined static/dynamic environment
 *)

signature PP_ENV =
  sig

    type doc   = PrettyPrint.doc
    type Env   = StaticEnv.Env * DynamicEnv.Env
    type State = PPDynamicEnv.State

    val ppEnv: State * Env -> doc

  end
(* stop of PP_ENV.sml *)
(* start of PPEnv.sml *)
(*
 * Standard ML pretty printing of the combined static/dynamic environment
 *)

structure PPEnv :> PP_ENV =
  struct

    (* Import *)

    type Env   = StaticEnv.Env * DynamicEnv.Env
    type State = PPDynamicEnv.State

    open PrettyPrint
    open PPMisc

    infixr ^^ ^/^


    (* Simple objects *)

    fun ppVId vid     = text(VId.toString vid)
    fun ppStrId strid = text(StrId.toString strid)


    (* Environments *)

    fun ppValEnv(s, (VE_STAT,VE_DYN)) =
        VIdMap.foldri
            (fn(vid, (sigma,IdStatus.v), doc) =>
             let
                val (v,is) = valOf(VIdMap.find(VE_DYN, vid))
             in
                fbox(
                    hbox(
                        text "val" ^/^
                        ppVId vid
                    ) ^^
                    nest(break ^^
                        text "=" ^/^
                        below(abox(PPVal.ppVal(s, v))) ^/^
                        text ":" ^/^
                        below(abox(PPType.ppTypeScheme sigma))
                    )
                ) ^/^
                doc
             end

             | (vid, (sigma,_), doc) => doc
            )
            empty VE_STAT

    fun ppStrEnv(s, T, (SE_STAT,SE_DYN)) =
        StrIdMap.foldri
            (fn(strid, StaticEnv.Str E_STAT, doc) =>
             let
                val DynamicEnv.Str E_DYN = valOf(StrIdMap.find(SE_DYN, strid))
             in
                abox(
                    hbox(
                        text "structure" ^/^
                        ppStrId strid ^/^
                        text "="
                    ) ^^
                    nest(break ^^
                        ppStr (s, T, (E_STAT,E_DYN))
                    )
                ) ^/^
                doc
             end
            )
            empty SE_STAT

    and ppEnv'(s, T, ((SE_STAT,TE_STAT,VE_STAT), (SE_DYN, TE_DYN, VE_DYN))) =
            vbox(
                ppStrEnv(s, T, (SE_STAT,SE_DYN)) ^/^
                PPStaticEnv.ppTyEnv(T,TE_STAT) ^/^
                PPStaticEnv.ppExEnv VE_STAT ^/^
                ppValEnv(s, (VE_STAT,VE_DYN))
            )

    and ppEnv(s, E) = ppEnv'(s, TyNameSet.empty, E)


    (* Structures *)

    and ppStr(s, T, E) =
        let
            val doc = ppEnv'(s, T, E)
        in
            abox(below(
                text "struct" ^^
                (if isEmpty doc then
                    empty
                 else
                    nest(vbox(break ^^ doc))
                ) ^^ break ^^
                text "end"
            ))
        end

  end
(* stop of PPEnv.sml *)
(* start of PP_BASIS.sml *)
(*
 * Standard ML pretty printing of the combined basis
 *)

signature PP_BASIS =
  sig

    type doc   = PrettyPrint.doc
    type Basis = Basis.Basis
    type State = PPEnv.State

    val ppBasis: State * Basis -> doc

  end
(* stop of PP_BASIS.sml *)
(* start of PPBasis.sml *)
(*
 * Standard ML pretty printing of the combined basis
 *)

structure PPBasis :> PP_BASIS =
  struct

    (* Import *)

    type Basis = Basis.Basis
    type State = PPEnv.State

    open PrettyPrint

    infixr ^^ ^/^


    (* Basis *)

    fun ppBasis (s, ((T,F_STAT,G_STAT,E_STAT), (F_DYN,G_DYN,E_DYN))) =
            vbox(
                PPStaticBasis.ppSigEnv G_STAT ^/^
                PPStaticBasis.ppFunEnv F_STAT ^/^
                PPEnv.ppEnv(s, (E_STAT,E_DYN)) ^/^
                text ""
            )

  end
(* stop of PPBasis.sml *)
(* start of PROGRAM.sml *)
(*
 * Standard ML programs
 *
 * Definition, section 8
 *
 * Note:
 *     State is passed as reference and modified via side effects. This way
 *     expanding out the state and exception convention in the inference rules
 *     of modules and core can be avoided. Note that the state therefore
 *     never is returned.
 *)

signature PROGRAM =
  sig

    (* Import types *)

    type Program      = GrammarProgram.Program
    type StaticBasis  = StaticBasis.Basis
    type DynamicBasis = DynamicBasis.Basis
    type Basis        = Basis.Basis
    type State        = EvalModule.State


    (* Export *)

    val execProgram:    State ref * Basis * Program -> Basis
    val elabProgram:    StaticBasis * Program -> StaticBasis
    val evalProgram:    State ref * DynamicBasis * Program -> DynamicBasis

  end
(* stop of PROGRAM.sml *)
(* start of Program.sml *)
(*
 * Standard ML programs
 *
 * Definition, section 8
 *
 * Note:
 *     State is passed as reference and modified via side effects. This way
 *     expanding out the state and exception convention in the inference rules
 *     of modules and core can be avoided. Note that the state therefore
 *     never is returned.
 *)

structure Program :> PROGRAM =
  struct

    (* Import *)

    type StaticBasis  = StaticBasis.Basis
    type DynamicBasis = DynamicBasis.Basis
    type Basis        = Basis.Basis
    type State        = EvalModule.State


    open GrammarProgram


    (* Helpers for output *)

    val width = 79

    fun printException(s, e) =
        ( TextIO.output(TextIO.stdOut, "Uncaught exception: ")
        ; PrettyPrint.output(TextIO.stdOut, PPVal.ppExVal(s, e), width)
        ; TextIO.output1(TextIO.stdOut, #"\n")
        ; TextIO.flushOut TextIO.stdOut
        )

    fun printStaticBasis B_STAT =
        ( PrettyPrint.output(TextIO.stdOut, PPStaticBasis.ppBasis B_STAT,
                             width)
        ; TextIO.flushOut TextIO.stdOut
        )

    fun printDynamicBasis(s, B_DYN) =
        ( PrettyPrint.output(TextIO.stdOut, PPDynamicBasis.ppBasis(s, B_DYN),
                             width)
        ; TextIO.flushOut TextIO.stdOut
        )

    fun printBasis(s, B) =
        ( PrettyPrint.output(TextIO.stdOut, PPBasis.ppBasis(s, B), width)
        ; TextIO.flushOut TextIO.stdOut
        )


    (* Helpers for basis modification *)

    val oplus = Basis.oplus

    infix oplus


    (* Inference rules [Section 8] *)

    fun execProgram(s,B, Program(I, topdec, program_opt)) =
        (* [Rules 187 to 189] *)
        let
            val B_STAT1 = ElabModule.elabTopDec(Basis.B_STATof B, topdec)
            val B_DYN1  = EvalModule.evalTopDec(s,Basis.B_DYNof B, topdec)
            (* [Rule 189] *)
            val _   = printBasis(!s, (B_STAT1,B_DYN1))
            val B'  = B oplus (B_STAT1,B_DYN1)
            val B'' = case program_opt
                        of NONE         => B'
                         | SOME program => execProgram(s,B', program)
        in
            B''
        end
        handle Error.Error m =>
               (* [Rule 187] *)
               let
                   val B' = case program_opt
                              of NONE         => B
                               | SOME program => execProgram(s,B, program)
               in
                   B'
               end

             | Pack.Pack e =>
               (* [Rule 188] *)
               let
                   val _  = printException(!s, e)
                   val B' = case program_opt
                              of NONE         => B
                               | SOME program => execProgram(s,B, program)
               in
                   B'
               end


    (* Elaboration only *)

    fun elabProgram(B_STAT, Program(I, topdec, program_opt)) =
        let
            val B_STAT1  = ElabModule.elabTopDec(B_STAT, topdec)
            val  _       = printStaticBasis B_STAT1
            val B_STAT'  = StaticBasis.plus(B_STAT, B_STAT1)
            val B_STAT'' = case program_opt
                             of NONE         => B_STAT'
                              | SOME program => elabProgram(B_STAT', program)
        in
            B_STAT''
        end
        handle Error.Error m =>
               B_STAT


    (* Evaluation only *)

    fun evalProgram(s,B_DYN, Program(I, topdec, program_opt)) =
        let
            val B_DYN1  = EvalModule.evalTopDec(s,B_DYN, topdec)
            val  _      = printDynamicBasis(!s, B_DYN1)
            val B_DYN'  = DynamicBasis.plus(B_DYN, B_DYN1)
            val B_DYN'' = case program_opt
                            of NONE         => B_DYN'
                             | SOME program => evalProgram(s,B_DYN', program)
        in
            B_DYN''
        end
        handle Error.Error m =>
               (* Runtime error *)
               let
                   val B_DYN' = case program_opt
                                  of NONE         => B_DYN
                                   | SOME program =>
                                        evalProgram(s,B_DYN, program)
               in
                   B_DYN'
               end

             | Pack.Pack e =>
               let
                   val  _     = printException(!s, e)
                   val B_DYN' = case program_opt
                                  of NONE         => B_DYN
                                   | SOME program =>
                                        evalProgram(s,B_DYN, program)
               in
                   B_DYN'
               end

  end
(* stop of Program.sml *)
(* start of ml-yacc/lib/base.sig *)
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

(* base.sig: Base signature file for SML-Yacc.  This file contains signatures
   that must be loaded before any of the files produced by ML-Yacc are loaded
*)

(* STREAM: signature for a lazy stream.*)

signature STREAM =
 sig type 'xa stream
     val streamify : (unit -> '_a) -> '_a stream
     val cons : '_a * '_a stream -> '_a stream
     val get : '_a stream -> '_a * '_a stream
 end

(* LR_TABLE: signature for an LR Table.

   The list of actions and gotos passed to mkLrTable must be ordered by state
   number. The values for state 0 are the first in the list, the values for
    state 1 are next, etc.
*)

signature LR_TABLE =
    sig
        datatype ('a,'b) pairlist = EMPTY | PAIR of 'a * 'b * ('a,'b) pairlist
        datatype state = STATE of int
        datatype term = T of int
        datatype nonterm = NT of int
        datatype action = SHIFT of state
                        | REDUCE of int
                        | ACCEPT
                        | ERROR
        type table
        
        val numStates : table -> int
        val numRules : table -> int
        val describeActions : table -> state ->
                                (term,action) pairlist * action
        val describeGoto : table -> state -> (nonterm,state) pairlist
        val action : table -> state * term -> action
        val goto : table -> state * nonterm -> state
        val initialState : table -> state
        exception Goto of state * nonterm

        val mkLrTable : {actions : ((term,action) pairlist * action) array,
                         gotos : (nonterm,state) pairlist array,
                         numStates : int, numRules : int,
                         initialState : state} -> table
    end

(* TOKEN: signature revealing the internal structure of a token. This signature
   TOKEN distinct from the signature {parser name}_TOKENS produced by ML-Yacc.
   The {parser name}_TOKENS structures contain some types and functions to
    construct tokens from values and positions.

   The representation of token was very carefully chosen here to allow the
   polymorphic parser to work without knowing the types of semantic values
   or line numbers.

   This has had an impact on the TOKENS structure produced by SML-Yacc, which
   is a structure parameter to lexer functors.  We would like to have some
   type 'a token which functions to construct tokens would create.  A
   constructor function for a integer token might be

          INT: int * 'a * 'a -> 'a token.
 
   This is not possible because we need to have tokens with the representation
   given below for the polymorphic parser.

   Thus our constructur functions for tokens have the form:

          INT: int * 'a * 'a -> (svalue,'a) token

   This in turn has had an impact on the signature that lexers for SML-Yacc
   must match and the types that a user must declare in the user declarations
   section of lexers.
*)

signature TOKEN =
    sig
        structure LrTable : LR_TABLE
        datatype ('a,'b) token = TOKEN of LrTable.term * ('a * 'b * 'b)
        val sameToken : ('a,'b) token * ('a,'b) token -> bool
    end

(* LR_PARSER: signature for a polymorphic LR parser *)

signature LR_PARSER =
    sig
        structure Stream: STREAM
        structure LrTable : LR_TABLE
        structure Token : TOKEN

        sharing LrTable = Token.LrTable

        exception ParseError

        val parse : {table : LrTable.table,
                     lexer : ('_b,'_c) Token.token Stream.stream,
                     arg: 'arg,
                     saction : int *
                               '_c *
                                (LrTable.state * ('_b * '_c * '_c)) list * 
                                'arg ->
                                     LrTable.nonterm *
                                     ('_b * '_c * '_c) *
                                     ((LrTable.state *('_b * '_c * '_c)) list),
                     void : '_b,
                     ec : { is_keyword : LrTable.term -> bool,
                            noShift : LrTable.term -> bool,
                            preferred_change : (LrTable.term list * LrTable.term list) list,
                            errtermvalue : LrTable.term -> '_b,
                            showTerminal : LrTable.term -> string,
                            terms: LrTable.term list,
                            error : string * '_c * '_c -> unit
                           },
                     lookahead : int  (* max amount of lookahead used in *)
                                      (* error correction *)
                        } -> '_b *
                             (('_b,'_c) Token.token Stream.stream)
    end

(* LEXER: a signature that most lexers produced for use with SML-Yacc's
   output will match.  The user is responsible for declaring type token,
   type pos, and type svalue in the UserDeclarations section of a lexer.

   Note that type token is abstract in the lexer.  This allows SML-Yacc to
   create a TOKENS signature for use with lexers produced by ML-Lex that
   treats the type token abstractly.  Lexers that are functors parametrized by
   a Tokens structure matching a TOKENS signature cannot examine the structure
   of tokens.
*)

signature LEXER =
   sig
       structure UserDeclarations :
           sig
                type ('a,'b) token
                type pos
                type svalue
           end
        val makeLexer : (int -> string) -> unit -> 
         (UserDeclarations.svalue,UserDeclarations.pos) UserDeclarations.token
   end

(* ARG_LEXER: the %arg option of ML-Lex allows users to produce lexers which
   also take an argument before yielding a function from unit to a token
*)

signature ARG_LEXER =
   sig
       structure UserDeclarations :
           sig
                type ('a,'b) token
                type pos
                type svalue
                type arg
           end
        val makeLexer : (int -> string) -> UserDeclarations.arg -> unit -> 
         (UserDeclarations.svalue,UserDeclarations.pos) UserDeclarations.token
   end

(* PARSER_DATA: the signature of ParserData structures in {parser name}LrValsFun
   produced by  SML-Yacc.  All such structures match this signature.  

   The {parser name}LrValsFun produces a structure which contains all the values
   except for the lexer needed to call the polymorphic parser mentioned
   before.

*)

signature PARSER_DATA =
   sig
        (* the type of line numbers *)

        type pos

        (* the type of semantic values *)

        type svalue

         (* the type of the user-supplied argument to the parser *)
        type arg
 
        (* the intended type of the result of the parser.  This value is
           produced by applying extract from the structure Actions to the
           final semantic value resultiing from a parse.
         *)

        type result

        structure LrTable : LR_TABLE
        structure Token : TOKEN
        sharing Token.LrTable = LrTable

        (* structure Actions contains the functions which mantain the
           semantic values stack in the parser.  Void is used to provide
           a default value for the semantic stack.
         *)

        structure Actions : 
          sig
              val actions : int * pos *
                   (LrTable.state * (svalue * pos * pos)) list * arg->
                         LrTable.nonterm * (svalue * pos * pos) *
                         ((LrTable.state *(svalue * pos * pos)) list)
              val void : svalue
              val extract : svalue -> result
          end

        (* structure EC contains information used to improve error
           recovery in an error-correcting parser *)

        structure EC :
           sig
             val is_keyword : LrTable.term -> bool
             val noShift : LrTable.term -> bool
             val preferred_change : (LrTable.term list * LrTable.term list) list
             val errtermvalue : LrTable.term -> svalue
             val showTerminal : LrTable.term -> string
             val terms: LrTable.term list
           end

        (* table is the LR table for the parser *)

        val table : LrTable.table
    end

(* signature PARSER is the signature that most user parsers created by 
   SML-Yacc will match.
*)

signature PARSER =
    sig
        structure Token : TOKEN
        structure Stream : STREAM
        exception ParseError

        (* type pos is the type of line numbers *)

        type pos

        (* type result is the type of the result from the parser *)

        type result

         (* the type of the user-supplied argument to the parser *)
        type arg
        
        (* type svalue is the type of semantic values for the semantic value
           stack
         *)

        type svalue

        (* val makeLexer is used to create a stream of tokens for the parser *)

        val makeLexer : (int -> string) ->
                         (svalue,pos) Token.token Stream.stream

        (* val parse takes a stream of tokens and a function to print
           errors and returns a value of type result and a stream containing
           the unused tokens
         *)

        val parse : int * ((svalue,pos) Token.token Stream.stream) *
                    (string * pos * pos -> unit) * arg ->
                                result * (svalue,pos) Token.token Stream.stream

        val sameToken : (svalue,pos) Token.token * (svalue,pos) Token.token ->
                                bool
     end

(* signature ARG_PARSER is the signature that will be matched by parsers whose
    lexer takes an additional argument.
*)

signature ARG_PARSER = 
    sig
        structure Token : TOKEN
        structure Stream : STREAM
        exception ParseError

        type arg
        type lexarg
        type pos
        type result
        type svalue

        val makeLexer : (int -> string) -> lexarg ->
                         (svalue,pos) Token.token Stream.stream
        val parse : int * ((svalue,pos) Token.token Stream.stream) *
                    (string * pos * pos -> unit) * arg ->
                                result * (svalue,pos) Token.token Stream.stream

        val sameToken : (svalue,pos) Token.token * (svalue,pos) Token.token ->
                                bool
     end

(* stop of ml-yacc/lib/base.sig *)
(* start of ml-yacc/lib/join.sml *)
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

(* functor Join creates a user parser by putting together a Lexer structure,
   an LrValues structure, and a polymorphic parser structure.  Note that
   the Lexer and LrValues structure must share the type pos (i.e. the type
   of line numbers), the type svalues for semantic values, and the type
   of tokens.
*)

functor Join(structure Lex : LEXER
             structure ParserData: PARSER_DATA
             structure LrParser : LR_PARSER
             sharing ParserData.LrTable = LrParser.LrTable
             sharing ParserData.Token = LrParser.Token
             sharing type Lex.UserDeclarations.svalue = ParserData.svalue
             sharing type Lex.UserDeclarations.pos = ParserData.pos
             sharing type Lex.UserDeclarations.token = ParserData.Token.token)
                 : PARSER =
struct
    structure Token = ParserData.Token
    structure Stream = LrParser.Stream
 
    exception ParseError = LrParser.ParseError

    type arg = ParserData.arg
    type pos = ParserData.pos
    type result = ParserData.result
    type svalue = ParserData.svalue
    val makeLexer = LrParser.Stream.streamify o Lex.makeLexer
    val parse = fn (lookahead,lexer,error,arg) =>
        (fn (a,b) => (ParserData.Actions.extract a,b))
     (LrParser.parse {table = ParserData.table,
                lexer=lexer,
                lookahead=lookahead,
                saction = ParserData.Actions.actions,
                arg=arg,
                void= ParserData.Actions.void,
                ec = {is_keyword = ParserData.EC.is_keyword,
                      noShift = ParserData.EC.noShift,
                      preferred_change = ParserData.EC.preferred_change,
                      errtermvalue = ParserData.EC.errtermvalue,
                      error=error,
                      showTerminal = ParserData.EC.showTerminal,
                      terms = ParserData.EC.terms}}
      )
     val sameToken = Token.sameToken
end

(* functor JoinWithArg creates a variant of the parser structure produced 
   above.  In this case, the makeLexer take an additional argument before
   yielding a value of type unit -> (svalue,pos) token
 *)

functor JoinWithArg(structure Lex : ARG_LEXER
             structure ParserData: PARSER_DATA
             structure LrParser : LR_PARSER
             sharing ParserData.LrTable = LrParser.LrTable
             sharing ParserData.Token = LrParser.Token
             sharing type Lex.UserDeclarations.svalue = ParserData.svalue
             sharing type Lex.UserDeclarations.pos = ParserData.pos
             sharing type Lex.UserDeclarations.token = ParserData.Token.token)
                 : ARG_PARSER  =
struct
    structure Token = ParserData.Token
    structure Stream = LrParser.Stream

    exception ParseError = LrParser.ParseError

    type arg = ParserData.arg
    type lexarg = Lex.UserDeclarations.arg
    type pos = ParserData.pos
    type result = ParserData.result
    type svalue = ParserData.svalue

    val makeLexer = fn s => fn arg =>
                 LrParser.Stream.streamify (Lex.makeLexer s arg)
    val parse = fn (lookahead,lexer,error,arg) =>
        (fn (a,b) => (ParserData.Actions.extract a,b))
     (LrParser.parse {table = ParserData.table,
                lexer=lexer,
                lookahead=lookahead,
                saction = ParserData.Actions.actions,
                arg=arg,
                void= ParserData.Actions.void,
                ec = {is_keyword = ParserData.EC.is_keyword,
                      noShift = ParserData.EC.noShift,
                      preferred_change = ParserData.EC.preferred_change,
                      errtermvalue = ParserData.EC.errtermvalue,
                      error=error,
                      showTerminal = ParserData.EC.showTerminal,
                      terms = ParserData.EC.terms}}
      )
    val sameToken = Token.sameToken
end;
(* stop of ml-yacc/lib/join.sml *)
(* start of ml-yacc/lib/lrtable.sml *)
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)
structure LrTable : LR_TABLE = 
    struct
        open Array List
        infix 9 sub
        datatype ('a,'b) pairlist = EMPTY
                                  | PAIR of 'a * 'b * ('a,'b) pairlist
        datatype term = T of int
        datatype nonterm = NT of int
        datatype state = STATE of int
        datatype action = SHIFT of state
                        | REDUCE of int (* rulenum from grammar *)
                        | ACCEPT
                        | ERROR
        exception Goto of state * nonterm
        type table = {states: int, rules : int,initialState: state,
                      action: ((term,action) pairlist * action) array,
                      goto :  (nonterm,state) pairlist array}
        val numStates = fn ({states,...} : table) => states
        val numRules = fn ({rules,...} : table) => rules
        val describeActions =
           fn ({action,...} : table) => 
                   fn (STATE s) => action sub s
        val describeGoto =
           fn ({goto,...} : table) =>
                   fn (STATE s) => goto sub s
        fun findTerm (T term,row,default) =
            let fun find (PAIR (T key,data,r)) =
                       if key < term then find r
                       else if key=term then data
                       else default
                   | find EMPTY = default
            in find row
            end
        fun findNonterm (NT nt,row) =
            let fun find (PAIR (NT key,data,r)) =
                       if key < nt then find r
                       else if key=nt then SOME data
                       else NONE
                   | find EMPTY = NONE
            in find row
            end
        val action = fn ({action,...} : table) =>
                fn (STATE state,term) =>
                  let val (row,default) = action sub state
                  in findTerm(term,row,default)
                  end
        val goto = fn ({goto,...} : table) =>
                        fn (a as (STATE state,nonterm)) =>
                          case findNonterm(nonterm,goto sub state)
                          of SOME state => state
                           | NONE => raise (Goto a)
        val initialState = fn ({initialState,...} : table) => initialState
        val mkLrTable = fn {actions,gotos,initialState,numStates,numRules} =>
             ({action=actions,goto=gotos,
               states=numStates,
               rules=numRules,
               initialState=initialState} : table)
end;
(* stop of ml-yacc/lib/lrtable.sml *)
(* start of ml-yacc/lib/stream.sml *)
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

(* Stream: a structure implementing a lazy stream.  The signature STREAM
   is found in base.sig *)

structure Stream :> STREAM =
struct
   datatype 'a str = EVAL of 'a * 'a str ref | UNEVAL of (unit->'a)

   type 'a stream = 'a str ref

   fun get(ref(EVAL t)) = t
     | get(s as ref(UNEVAL f)) = 
            let val t = (f(), ref(UNEVAL f)) in s := EVAL t; t end

   fun streamify f = ref(UNEVAL f)
   fun cons(a,s) = ref(EVAL(a,s))

end;
(* stop of ml-yacc/lib/stream.sml *)
(* start of ml-yacc/lib/parser2.sml *)
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

(* parser.sml:  This is a parser driver for LR tables with an error-recovery
   routine added to it.  The routine used is described in detail in this
   article:

        'A Practical Method for LR and LL Syntactic Error Diagnosis and
         Recovery', by M. Burke and G. Fisher, ACM Transactions on
         Programming Langauges and Systems, Vol. 9, No. 2, April 1987,
         pp. 164-197.

    This program is an implementation is the partial, deferred method discussed
    in the article.  The algorithm and data structures used in the program
    are described below.  

    This program assumes that all semantic actions are delayed.  A semantic
    action should produce a function from unit -> value instead of producing the
    normal value.  The parser returns the semantic value on the top of the
    stack when accept is encountered.  The user can deconstruct this value
    and apply the unit -> value function in it to get the answer.

    It also assumes that the lexer is a lazy stream.

    Data Structures:
    ----------------
        
        * The parser:

           The state stack has the type

                 (state * (semantic value * line # * line #)) list

           The parser keeps a queue of (state stack * lexer pair).  A lexer pair
         consists of a terminal * value pair and a lexer.  This allows the 
         parser to reconstruct the states for terminals to the left of a
         syntax error, and attempt to make error corrections there.

           The queue consists of a pair of lists (x,y).  New additions to
         the queue are cons'ed onto y.  The first element of x is the top
         of the queue.  If x is nil, then y is reversed and used
         in place of x.

    Algorithm:
    ----------

        * The steady-state parser:  

            This parser keeps the length of the queue of state stacks at
        a steady state by always removing an element from the front when
        another element is placed on the end.

            It has these arguments:

           stack: current stack
           queue: value of the queue
           lexPair ((terminal,value),lex stream)

        When SHIFT is encountered, the state to shift to and the value are
        are pushed onto the state stack.  The state stack and lexPair are
        placed on the queue.  The front element of the queue is removed.

        When REDUCTION is encountered, the rule is applied to the current
        stack to yield a triple (nonterm,value,new stack).  A new
        stack is formed by adding (goto(top state of stack,nonterm),value)
        to the stack.

        When ACCEPT is encountered, the top value from the stack and the
        lexer are returned.

        When an ERROR is encountered, fixError is called.  FixError
        takes the arguments to the parser, fixes the error if possible and
        returns a new set of arguments.

        * The distance-parser:

        This parser includes an additional argument distance.  It pushes
        elements on the queue until it has parsed distance tokens, or an
        ACCEPT or ERROR occurs.  It returns a stack, lexer, the number of
        tokens left unparsed, a queue, and an action option.
*)

signature FIFO = 
  sig type 'a queue
      val empty : 'a queue
      exception Empty
      val get : 'a queue -> 'a * 'a queue
      val put : 'a * 'a queue -> 'a queue
  end

(* drt (12/15/89) -- the functor should be used in development work, but
   it wastes space in the release version.

functor ParserGen(structure LrTable : LR_TABLE
                  structure Stream : STREAM) : LR_PARSER =
*)

structure LrParser :> LR_PARSER =
   struct
      structure LrTable = LrTable
      structure Stream = Stream

      structure Token : TOKEN =
        struct
            structure LrTable = LrTable
            datatype ('a,'b) token = TOKEN of LrTable.term * ('a * 'b * 'b)
            val sameToken = fn (TOKEN(t,_),TOKEN(t',_)) => t=t'
        end

      open LrTable
      open Token

      val DEBUG1 = false
      val DEBUG2 = false
      exception ParseError
      exception ParseImpossible of int

      structure Fifo :> FIFO =
        struct
          type 'a queue = ('a list * 'a list)
          val empty = (nil,nil)
          exception Empty
          fun get(a::x, y) = (a, (x,y))
            | get(nil, nil) = raise Empty
            | get(nil, y) = get(rev y, nil)
          fun put(a,(x,y)) = (x,a::y)
        end

      type ('a,'b) elem = (state * ('a * 'b * 'b))
      type ('a,'b) stack = ('a,'b) elem list
      type ('a,'b) lexv = ('a,'b) token
      type ('a,'b) lexpair = ('a,'b) lexv * (('a,'b) lexv Stream.stream)
      type ('a,'b) distanceParse =
                 ('a,'b) lexpair *
                 ('a,'b) stack * 
                 (('a,'b) stack * ('a,'b) lexpair) Fifo.queue *
                 int ->
                   ('a,'b) lexpair *
                   ('a,'b) stack * 
                   (('a,'b) stack * ('a,'b) lexpair) Fifo.queue *
                   int *
                   action option

      type ('a,'b) ecRecord =
         {is_keyword : term -> bool,
          preferred_change : (term list * term list) list,
          error : string * 'b * 'b -> unit,
          errtermvalue : term -> 'a,
          terms : term list,
          showTerminal : term -> string,
          noShift : term -> bool}

      local 
         val print = fn s => TextIO.output(TextIO.stdOut,s)
         val println = fn s => (print s; print "\n")
         val showState = fn (STATE s) => "STATE " ^ (Int.toString s)
      in
        fun printStack(stack: ('a,'b) stack, n: int) =
         case stack
           of (state,_) :: rest =>
                 (print("\t" ^ Int.toString n ^ ": ");
                  println(showState state);
                  printStack(rest, n+1))
            | nil => ()
                
        fun prAction showTerminal
                 (stack as (state,_) :: _, next as (TOKEN (term,_),_), action) =
             (println "Parse: state stack:";
              printStack(stack, 0);
              print("       state="
                         ^ showState state      
                         ^ " next="
                         ^ showTerminal term
                         ^ " action="
                        );
              case action
                of SHIFT state => println ("SHIFT " ^ (showState state))
                 | REDUCE i => println ("REDUCE " ^ (Int.toString i))
                 | ERROR => println "ERROR"
                 | ACCEPT => println "ACCEPT")
        | prAction _ (_,_,action) = ()
     end

    (* ssParse: parser which maintains the queue of (state * lexvalues) in a
        steady-state.  It takes a table, showTerminal function, saction
        function, and fixError function.  It parses until an ACCEPT is
        encountered, or an exception is raised.  When an error is encountered,
        fixError is called with the arguments of parseStep (lexv,stack,and
        queue).  It returns the lexv, and a new stack and queue adjusted so
        that the lexv can be parsed *)
        
    val ssParse =
      fn (table,showTerminal,saction,fixError,arg) =>
        let val prAction = prAction showTerminal
            val action = LrTable.action table
            val goto = LrTable.goto table
            fun parseStep(args as
                         (lexPair as (TOKEN (terminal, value as (_,leftPos,_)),
                                      lexer
                                      ),
                          stack as (state,_) :: _,
                          queue)) =
              let val nextAction = action (state,terminal)
                  val _ = if DEBUG1 then prAction(stack,lexPair,nextAction)
                          else ()
              in case nextAction
                 of SHIFT s =>
                  let val newStack = (s,value) :: stack
                      val newLexPair = Stream.get lexer
                      val (_,newQueue) =Fifo.get(Fifo.put((newStack,newLexPair),
                                                            queue))
                  in parseStep(newLexPair,(s,value)::stack,newQueue)
                  end
                 | REDUCE i =>
                     (case saction(i,leftPos,stack,arg)
                      of (nonterm,value,stack as (state,_) :: _) =>
                          parseStep(lexPair,(goto(state,nonterm),value)::stack,
                                    queue)
                       | _ => raise (ParseImpossible 197))
                 | ERROR => parseStep(fixError args)
                 | ACCEPT => 
                        (case stack
                         of (_,(topvalue,_,_)) :: _ =>
                                let val (token,restLexer) = lexPair
                                in (topvalue,Stream.cons(token,restLexer))
                                end
                          | _ => raise (ParseImpossible 202))
              end
            | parseStep _ = raise (ParseImpossible 204)
        in parseStep
        end

    (*  distanceParse: parse until n tokens are shifted, or accept or
        error are encountered.  Takes a table, showTerminal function, and
        semantic action function.  Returns a parser which takes a lexPair
        (lex result * lexer), a state stack, a queue, and a distance
        (must be > 0) to parse.  The parser returns a new lex-value, a stack
        with the nth token shifted on top, a queue, a distance, and action
        option. *)

    val distanceParse =
      fn (table,showTerminal,saction,arg) =>
        let val prAction = prAction showTerminal
            val action = LrTable.action table
            val goto = LrTable.goto table
            fun parseStep(lexPair,stack,queue,0) = (lexPair,stack,queue,0,NONE)
              | parseStep(lexPair as (TOKEN (terminal, value as (_,leftPos,_)),
                                      lexer
                                     ),
                          stack as (state,_) :: _,
                          queue,distance) =
              let val nextAction = action(state,terminal)
                  val _ = if DEBUG1 then prAction(stack,lexPair,nextAction)
                          else ()
              in case nextAction
                 of SHIFT s =>
                  let val newStack = (s,value) :: stack
                      val newLexPair = Stream.get lexer
                  in parseStep(newLexPair,(s,value)::stack,
                               Fifo.put((newStack,newLexPair),queue),distance-1)
                  end
                 | REDUCE i =>
                    (case saction(i,leftPos,stack,arg)
                      of (nonterm,value,stack as (state,_) :: _) =>
                         parseStep(lexPair,(goto(state,nonterm),value)::stack,
                                 queue,distance)
                      | _ => raise (ParseImpossible 240))
                 | ERROR => (lexPair,stack,queue,distance,SOME nextAction)
                 | ACCEPT => (lexPair,stack,queue,distance,SOME nextAction)
              end
           | parseStep _ = raise (ParseImpossible 242)
        in parseStep : ('_a,'_b) distanceParse 
        end

(* mkFixError: function to create fixError function which adjusts parser state
   so that parse may continue in the presence of an error *)

fun mkFixError({is_keyword,terms,errtermvalue,
              preferred_change,noShift,
              showTerminal,error,...} : ('_a,'_b) ecRecord,
             distanceParse : ('_a,'_b) distanceParse,
             minAdvance,maxAdvance) 

            (lexv as (TOKEN (term,value as (_,leftPos,_)),_),stack,queue) =
    let val _ = if DEBUG2 then
                        error("syntax error found at " ^ (showTerminal term),
                              leftPos,leftPos)
                else ()

        fun tokAt(t,p) = TOKEN(t,(errtermvalue t,p,p))

        val minDelta = 3

        (* pull all the state * lexv elements from the queue *)

        val stateList = 
           let fun f q = let val (elem,newQueue) = Fifo.get q
                         in elem :: (f newQueue)
                         end handle Fifo.Empty => nil
           in f queue
           end

        (* now number elements of stateList, giving distance from
           error token *)

        val (_, numStateList) =
              List.foldr (fn (a,(num,r)) => (num+1,(a,num)::r)) (0, []) stateList

        (* Represent the set of potential changes as a linked list.

           Values of datatype Change hold information about a potential change.

           oper = oper to be applied
           pos = the # of the element in stateList that would be altered.
           distance = the number of tokens beyond the error token which the
             change allows us to parse.
           new = new terminal * value pair at that point
           orig = original terminal * value pair at the point being changed.
         *)

        datatype ('a,'b) change = CHANGE of
           {pos : int, distance : int, leftPos: 'b, rightPos: 'b,
            new : ('a,'b) lexv list, orig : ('a,'b) lexv list}


         val showTerms = concat o map (fn TOKEN(t,_) => " " ^ showTerminal t)

         val printChange = fn c =>
          let val CHANGE {distance,new,orig,pos,...} = c
          in (print ("{distance= " ^ (Int.toString distance));
              print (",orig ="); print(showTerms orig);
              print (",new ="); print(showTerms new);
              print (",pos= " ^ (Int.toString pos));
              print "}\n")
          end

        val printChangeList = app printChange

(* parse: given a lexPair, a stack, and the distance from the error
   token, return the distance past the error token that we are able to parse.*)

        fun parse (lexPair,stack,queuePos : int) =
            case distanceParse(lexPair,stack,Fifo.empty,queuePos+maxAdvance+1)
             of (_,_,_,distance,SOME ACCEPT) => 
                        if maxAdvance-distance-1 >= 0 
                            then maxAdvance 
                            else maxAdvance-distance-1
              | (_,_,_,distance,_) => maxAdvance - distance - 1

(* catList: concatenate results of scanning list *)

        fun catList l f = List.foldr (fn(a,r)=> f a @ r) [] l

        fun keywordsDelta new = if List.exists (fn(TOKEN(t,_))=>is_keyword t) new
                       then minDelta else 0

        fun tryChange{lex,stack,pos,leftPos,rightPos,orig,new} =
             let val lex' = List.foldr (fn (t',p)=>(t',Stream.cons p)) lex new
                 val distance = parse(lex',stack,pos+length new-length orig)
              in if distance >= minAdvance + keywordsDelta new 
                   then [CHANGE{pos=pos,leftPos=leftPos,rightPos=rightPos,
                                distance=distance,orig=orig,new=new}] 
                   else []
             end


(* tryDelete: Try to delete n terminals.
              Return single-element [success] or nil.
              Do not delete unshiftable terminals. *)


    fun tryDelete n ((stack,lexPair as (TOKEN(term,(_,l,r)),_)),qPos) =
        let fun del(0,accum,left,right,lexPair) =
                  tryChange{lex=lexPair,stack=stack,
                            pos=qPos,leftPos=left,rightPos=right,
                            orig=rev accum, new=[]}
              | del(n,accum,left,right,(tok as TOKEN(term,(_,_,r)),lexer)) =
                   if noShift term then []
                   else del(n-1,tok::accum,left,r,Stream.get lexer)
         in del(n,[],l,r,lexPair)
        end

(* tryInsert: try to insert tokens before the current terminal;
       return a list of the successes  *)

        fun tryInsert((stack,lexPair as (TOKEN(_,(_,l,_)),_)),queuePos) =
               catList terms (fn t =>
                 tryChange{lex=lexPair,stack=stack,
                           pos=queuePos,orig=[],new=[tokAt(t,l)],
                           leftPos=l,rightPos=l})
                              
(* trySubst: try to substitute tokens for the current terminal;
       return a list of the successes  *)

        fun trySubst ((stack,lexPair as (orig as TOKEN (term,(_,l,r)),lexer)),
                      queuePos) =
              if noShift term then []
              else
                  catList terms (fn t =>
                      tryChange{lex=Stream.get lexer,stack=stack,
                                pos=queuePos,
                                leftPos=l,rightPos=r,orig=[orig],
                                new=[tokAt(t,r)]})

     (* do_delete(toks,lexPair) tries to delete tokens "toks" from "lexPair".
         If it succeeds, returns SOME(toks',l,r,lp), where
             toks' is the actual tokens (with positions and values) deleted,
             (l,r) are the (leftmost,rightmost) position of toks', 
             lp is what remains of the stream after deletion 
     *)
        fun do_delete(nil,lp as (TOKEN(_,(_,l,_)),_)) = SOME(nil,l,l,lp)
          | do_delete([t],(tok as TOKEN(t',(_,l,r)),lp')) =
               if t=t'
                   then SOME([tok],l,r,Stream.get lp')
                   else NONE
          | do_delete(t::rest,(tok as TOKEN(t',(_,l,r)),lp')) =
               if t=t'
                   then case do_delete(rest,Stream.get lp')
                         of SOME(deleted,l',r',lp'') =>
                               SOME(tok::deleted,l,r',lp'')
                          | NONE => NONE
                   else NONE
                             
        fun tryPreferred((stack,lexPair),queuePos) =
            catList preferred_change (fn (delete,insert) =>
               if List.exists noShift delete then [] (* should give warning at
                                                 parser-generation time *)
               else case do_delete(delete,lexPair)
                     of SOME(deleted,l,r,lp) => 
                            tryChange{lex=lp,stack=stack,pos=queuePos,
                                      leftPos=l,rightPos=r,orig=deleted,
                                      new=map (fn t=>(tokAt(t,r))) insert}
                      | NONE => [])

        val changes = catList numStateList tryPreferred @
                        catList numStateList tryInsert @
                          catList numStateList trySubst @
                            catList numStateList (tryDelete 1) @
                              catList numStateList (tryDelete 2) @
                                catList numStateList (tryDelete 3)

        val findMaxDist = fn l => 
          foldr (fn (CHANGE {distance,...},high) => Int.max(distance,high)) 0 l

(* maxDist: max distance past error taken that we could parse *)

        val maxDist = findMaxDist changes

(* remove changes which did not parse maxDist tokens past the error token *)

        val changes = catList changes 
              (fn(c as CHANGE{distance,...}) => 
                  if distance=maxDist then [c] else [])

      in case changes 
          of (l as change :: _) =>
              let fun print_msg (CHANGE {new,orig,leftPos,rightPos,...}) =
                  let val s = 
                      case (orig,new)
                          of (_::_,[]) => "deleting " ^ (showTerms orig)
                           | ([],_::_) => "inserting " ^ (showTerms new)
                           | _ => "replacing " ^ (showTerms orig) ^
                                 " with " ^ (showTerms new)
                  in error ("syntax error: " ^ s,leftPos,rightPos)
                  end
                   
                  val _ = 
                      (if length l > 1 andalso DEBUG2 then
                           (print "multiple fixes possible; could fix it by:\n";
                            app print_msg l;
                            print "chosen correction:\n")
                       else ();
                       print_msg change)

                  (* findNth: find nth queue entry from the error
                   entry.  Returns the Nth queue entry and the  portion of
                   the queue from the beginning to the nth-1 entry.  The
                   error entry is at the end of the queue.

                   Examples:

                   queue = a b c d e
                   findNth 0 = (e,a b c d)
                   findNth 1 =  (d,a b c)
                   *)

                  val findNth = fn n =>
                      let fun f (h::t,0) = (h,rev t)
                            | f (h::t,n) = f(t,n-1)
                            | f (nil,_) = let exception FindNth
                                          in raise FindNth
                                          end
                      in f (rev stateList,n)
                      end
                
                  val CHANGE {pos,orig,new,...} = change
                  val (last,queueFront) = findNth pos
                  val (stack,lexPair) = last

                  val lp1 = foldl(fn (_,(_,r)) => Stream.get r) lexPair orig
                  val lp2 = foldr(fn(t,r)=>(t,Stream.cons r)) lp1 new

                  val restQueue = 
                      Fifo.put((stack,lp2),
                               foldl Fifo.put Fifo.empty queueFront)

                  val (lexPair,stack,queue,_,_) =
                      distanceParse(lp2,stack,restQueue,pos)

              in (lexPair,stack,queue)
              end
        | nil => (error("syntax error found at " ^ (showTerminal term),
                        leftPos,leftPos); raise ParseError)
    end

   val parse = fn {arg,table,lexer,saction,void,lookahead,
                   ec=ec as {showTerminal,...} : ('_a,'_b) ecRecord} =>
        let val distance = 15   (* defer distance tokens *)
            val minAdvance = 1  (* must parse at least 1 token past error *)
            val maxAdvance = Int.max(lookahead,0)(* max distance for parse check *)
            val lexPair = Stream.get lexer
            val (TOKEN (_,(_,leftPos,_)),_) = lexPair
            val startStack = [(initialState table,(void,leftPos,leftPos))]
            val startQueue = Fifo.put((startStack,lexPair),Fifo.empty)
            val distanceParse = distanceParse(table,showTerminal,saction,arg)
            val fixError = mkFixError(ec,distanceParse,minAdvance,maxAdvance)
            val ssParse = ssParse(table,showTerminal,saction,fixError,arg)
            fun loop (lexPair,stack,queue,_,SOME ACCEPT) =
                   ssParse(lexPair,stack,queue)
              | loop (lexPair,stack,queue,0,_) = ssParse(lexPair,stack,queue)
              | loop (lexPair,stack,queue,distance,SOME ERROR) =
                 let val (lexPair,stack,queue) = fixError(lexPair,stack,queue)
                 in loop (distanceParse(lexPair,stack,queue,distance))
                 end
              | loop _ = let exception ParseInternal
                         in raise ParseInternal
                         end
        in loop (distanceParse(lexPair,startStack,startQueue,distance))
        end
 end;

(* stop of ml-yacc/lib/parser2.sml *)
(* start of DERIVED_FORMS_CORE.sml *)
(*
 * Standard ML core derived forms
 *
 * Definition, Section 2.7 and appendix A
 *
 * Note:
 *   Two phrases named Fmatch and Fmrule have been added to factorize FvalBind.
 *)


signature DERIVED_FORMS_CORE =
  sig

    (* Import *)

    type Info      = GrammarCore.Info

    type Lab       = GrammarCore.Lab
    type VId       = GrammarCore.VId

    type Op        = GrammarCore.Op
    type AtExp     = GrammarCore.AtExp
    type AppExp    = GrammarCore.AtExp list
    type InfExp    = GrammarCore.Exp
    type Exp       = GrammarCore.Exp
    type Match     = GrammarCore.Match
    type Mrule     = GrammarCore.Mrule
    type Dec       = GrammarCore.Dec
    type ValBind   = GrammarCore.ValBind
    type FvalBind  = GrammarCore.ValBind
    type Fmatch    = GrammarCore.Match * VId * int
    type Fmrule    = GrammarCore.Mrule * VId * int
    type TypBind   = GrammarCore.TypBind
    type DatBind   = GrammarCore.DatBind
    type AtPat     = GrammarCore.AtPat
    type PatRow    = GrammarCore.PatRow
    type Pat       = GrammarCore.Pat
    type Ty        = GrammarCore.Ty
    type TyVarseq  = GrammarCore.TyVarseq


    (* Expressions [Figure 15] *)

    val UNITAtExp:      Info                                    -> AtExp
    val TUPLEAtExp:     Info * Exp list                         -> AtExp
    val HASHAtExp:      Info * Lab                              -> AtExp
    val CASEExp:        Info * Exp * Match                      -> Exp
    val IFExp:          Info * Exp * Exp * Exp                  -> Exp
    val ANDALSOExp:     Info * Exp * Exp                        -> Exp
    val ORELSEExp:      Info * Exp * Exp                        -> Exp
    val SEQAtExp:       Info * Exp list                         -> AtExp
    val LETAtExp:       Info * Dec * Exp list                   -> AtExp
    val WHILEExp:       Info * Exp * Exp                        -> Exp
    val LISTAtExp:      Info * Exp list                         -> AtExp

    (* Patterns [Figure 16] *)

    val UNITAtPat:      Info                                    -> AtPat
    val TUPLEAtPat:     Info * Pat list                         -> AtPat
    val LISTAtPat:      Info * Pat list                         -> AtPat

    val VIDPatRow:      Info * VId * Ty option * Pat option * PatRow option
                                                                -> PatRow
    (* Types [Figure 16] *)

    val TUPLETy:        Info * Ty list                          -> Ty

    (* Function-value bindings [Figure 17] *)

    val FvalBind:       Info * Fmatch * FvalBind option         -> FvalBind
    val Fmatch:         Info * Fmrule * Fmatch option           -> Fmatch
    val Fmrule:         Info * Op * VId * AtPat list * Ty option * Exp -> Fmrule

    (* Declarations [Figure 17] *)

    val FUNDec:         Info * TyVarseq * FvalBind              -> Dec
    val DATATYPEDec:    Info * DatBind * TypBind option         -> Dec
    val ABSTYPEDec:     Info * DatBind * TypBind option * Dec   -> Dec

  end
(* stop of DERIVED_FORMS_CORE.sml *)
(* start of DerivedFormsCore.sml *)
(*
 * Standard ML core derived forms
 *
 * Definition, Section 2.7 and appendix A
 *
 * Notes:
 * - Two phrases named Fmatch and Fmrule have been added to factorize FvalBind.
 * - In Fvalbinds we do not enforce that all optional type annotations are
 *   syntactically identical (as the Definition enforces, although this seems
 *   to be a mistake).
 * - The Definition is somewhat inaccurate about the derived forms of Exp
 *   [Definition, Appendix A, Figure 15] in that most forms are actually AtExp
 *   derived forms, as can be seen from the full grammar [Definition,
 *   Appendix B, Figure 20]. To achieve consistency, the equivalent forms must
 *   be put in parentheses in some cases.
 * - The same goes for pattern derived forms [Definition, Appendix A, Figure 16;
 *   Appendix B, Figure 22].
 *)


structure DerivedFormsCore :> DERIVED_FORMS_CORE =
  struct

    (* Import *)

    structure C    = GrammarCore

    type Info      = C.Info

    type Lab       = C.Lab
    type VId       = C.VId

    type Op        = C.Op
    type AtExp     = C.AtExp
    type AppExp    = C.AtExp list
    type InfExp    = C.Exp
    type Exp       = C.Exp
    type Match     = C.Match
    type Mrule     = C.Mrule
    type Dec       = C.Dec
    type ValBind   = C.ValBind
    type FvalBind  = C.ValBind
    type Fmatch    = C.Match * C.VId * int
    type Fmrule    = C.Mrule * C.VId * int
    type TypBind   = C.TypBind
    type DatBind   = C.DatBind
    type AtPat     = C.AtPat
    type PatRow    = C.PatRow
    type Pat       = C.Pat
    type Ty        = C.Ty
    type TyVarseq  = C.TyVarseq


    (* Some helpers *)

    val vidFALSE               = VId.fromString "false"
    val vidTRUE                = VId.fromString "true"
    val vidNIL                 = VId.fromString "nil"
    val vidCONS                = VId.fromString "::"

    val longvidCONS            = LongVId.fromId vidCONS


    fun LONGVIDExp(I, longvid) = C.ATEXPExp(I, C.LONGVIDAtExp(I, C.SANSOp,
                                                                 longvid))
    fun LONGVIDPat(I, longvid) = C.ATPATPat(I, C.LONGVIDAtPat(I, C.SANSOp,
                                                                 longvid))

    fun VIDExp(I, vid)         = LONGVIDExp(I, LongVId.fromId vid)
    fun VIDPat(I, vid)         = LONGVIDPat(I, LongVId.fromId vid)

    fun FALSEExp(I)            = VIDExp(I, vidFALSE)
    fun TRUEExp(I)             = VIDExp(I, vidTRUE)
    fun NILExp(I)              = VIDExp(I, vidNIL)
    fun CONSExp(I)             = VIDExp(I, vidCONS)

    fun FALSEPat(I)            = VIDPat(I, vidFALSE)
    fun TRUEPat(I)             = VIDPat(I, vidTRUE)
    fun NILPat(I)              = VIDPat(I, vidNIL)


    (* Rewriting of withtype declarations [Appendix A, 2nd bullet] *)

    fun lookupTyCon(tycon, C.TypBind(_, tyvarseq, tycon', ty, typbind_opt)) =
            if tycon' = tycon then
                (tyvarseq, ty)
            else
                lookupTyCon(tycon, Option.valOf typbind_opt)
                (* may raise Option *)


    fun replaceTy (C.TyVarseq(_,tyvars), C.Tyseq(i',tys)) (C.TYVARTy(i,tyvar)) =
        let
            fun loop(tyvar'::tyvars', ty'::tys') =
                    if tyvar' = tyvar then
                        ty'
                    else
                        loop(tyvars', tys')
              | loop([],_) =
                    Error.error(i, "unbound type variable")
              | loop(_,[]) =
                    Error.error(i', "type sequence has wrong arity")
        in
            loop(tyvars, tys)
        end

      | replaceTy tyvarseq_tyseq (C.RECORDTy(I, tyrow_opt)) =
            C.RECORDTy(I, Option.map (replaceTyRow tyvarseq_tyseq) tyrow_opt)

      | replaceTy tyvarseq_tyseq (C.TYCONTy(I, tyseq', tycon)) =
            C.TYCONTy(I, replaceTyseq tyvarseq_tyseq tyseq', tycon)

      | replaceTy tyvarseq_tyseq (C.ARROWTy(I, ty1, ty2)) =
            C.ARROWTy(I, replaceTy tyvarseq_tyseq ty1,
                         replaceTy tyvarseq_tyseq ty2)

      | replaceTy tyvarseq_tyseq (C.PARTy(I, ty)) =
            C.PARTy(I, replaceTy tyvarseq_tyseq ty)

    and replaceTyRow tyvarseq_tyseq (C.TyRow(I, lab, ty, tyrow_opt)) =
            C.TyRow(I, lab, replaceTy tyvarseq_tyseq ty, 
                       Option.map (replaceTyRow tyvarseq_tyseq) tyrow_opt)

    and replaceTyseq tyvarseq_tyseq (C.Tyseq(I, tys)) =   
            C.Tyseq(I, List.map (replaceTy tyvarseq_tyseq) tys)


    fun rewriteTy typbind (ty as C.TYVARTy _) = ty

      | rewriteTy typbind (C.RECORDTy(I, tyrow_opt)) =
            C.RECORDTy(I, Option.map (rewriteTyRow typbind) tyrow_opt)

      | rewriteTy typbind (C.TYCONTy(I, tyseq, longtycon)) =
        let 
            val tyseq'          = rewriteTyseq typbind tyseq
            val (strids, tycon) = LongTyCon.explode longtycon
        in
            if not(List.null strids) then
                C.TYCONTy(I, tyseq', longtycon)
            else
                let 
                    val (tyvarseq', ty') = lookupTyCon(tycon, typbind)
                in
                    replaceTy (tyvarseq',tyseq') ty'
                end
                handle Option => C.TYCONTy(I, tyseq', longtycon)
        end

      | rewriteTy typbind (C.ARROWTy(I, ty1, ty2)) =
            C.ARROWTy(I, rewriteTy typbind ty1, rewriteTy typbind ty2)

      | rewriteTy typbind (C.PARTy(I, ty)) =
            C.PARTy(I, rewriteTy typbind ty)

    and rewriteTyRow typbind (C.TyRow(I, lab, ty, tyrow_opt)) =
            C.TyRow(I, lab, rewriteTy typbind ty,
                       Option.map (rewriteTyRow typbind) tyrow_opt)

    and rewriteTyseq typbind (C.Tyseq(I, tys)) =
            C.Tyseq(I, List.map (rewriteTy typbind) tys)

    fun rewriteConBind typbind (C.ConBind(I, op_opt, vid, ty_opt, conbind_opt))=
            C.ConBind(I, op_opt, vid,
                         Option.map (rewriteTy typbind) ty_opt,
                         Option.map (rewriteConBind typbind) conbind_opt)

    fun rewriteDatBind typbind (C.DatBind(I, tyvarseq, tycon, conbind,
                                                              datbind_opt)) =
            C.DatBind(I, tyvarseq, tycon, rewriteConBind typbind conbind,
                         Option.map (rewriteDatBind typbind) datbind_opt)


    (* Patterns [Figure 16] *)

    fun UNITAtPat(I) = C.RECORDAtPat(I, NONE)

    fun TUPLEAtPat(I, [pat]) = C.PARAtPat(I, pat)
      | TUPLEAtPat(I,  pats) =
        let
            fun toPatRow(n,    []     ) = NONE
              | toPatRow(n, pat::pats') =
                  SOME(C.ROWPatRow(I, Lab.fromInt n, pat, toPatRow(n+1,pats')))
        in
            C.RECORDAtPat(I, toPatRow(1, pats))
        end

    fun LISTAtPat(I, pats)  =
        let
            fun toPatList    []       = NILPat(I)
              | toPatList(pat::pats') =
                C.CONPat(I, C.SANSOp, longvidCONS,
                             TUPLEAtPat(I, [pat,toPatList pats']))
        in
            C.PARAtPat(I, toPatList pats)
        end


    (* Pattern Rows [Figure 16] *)

    fun VIDPatRow(I, vid, ty_opt, pat_opt, patrow_opt) =
        let
            val lab    = Lab.fromString(VId.toString vid)
            val vidPat = VIDPat(I, vid)
            val pat    =
                case (ty_opt, pat_opt)
                  of (NONE,    NONE) => vidPat
                   | (SOME ty, NONE) => C.TYPEDPat(I, vidPat, ty)
                   | ( _ , SOME pat) => C.ASPat(I, C.SANSOp,vid,ty_opt,pat)
        in
            C.ROWPatRow(I, lab, pat, patrow_opt)
        end


    (* Expressions [Figure 15] *)

    fun UNITAtExp(I) = C.RECORDAtExp(I, NONE)

    fun TUPLEAtExp(I, [exp]) = C.PARAtExp(I, exp)
      | TUPLEAtExp(I,  exps) =
        let
            fun toExpRow(n,    []     ) = NONE
              | toExpRow(n, exp::exps') =
                  SOME(C.ExpRow(I, Lab.fromInt n, exp, toExpRow(n+1, exps')))
        in
            C.RECORDAtExp(I, toExpRow(1, exps))
        end

    fun HASHAtExp(I, lab) =
        let
            val vid    = VId.invent()
            val dots   = C.WILDCARDPatRow(I)
            val patrow = C.ROWPatRow(I, lab, VIDPat(I, vid), SOME dots)
            val pat    = C.ATPATPat(I, C.RECORDAtPat(I, SOME patrow))
            val mrule  = C.Mrule(I, pat, VIDExp(I, vid))
            val match  = C.Match(I, mrule, NONE)
        in
            C.PARAtExp(I, C.FNExp(I, match))
        end

    fun CASEExp(I, exp, match) =
        let
            val function = C.ATEXPExp(I, C.PARAtExp(I, C.FNExp(I, match)))
        in
            C.APPExp(I, function, C.PARAtExp(I, exp))
        end

    fun IFExp(I, exp1, exp2, exp3) =
        let
            val mruleTrue  = C.Mrule(I, TRUEPat(I), exp2)
            val mruleFalse = C.Mrule(I, FALSEPat(I), exp3)
            val matchFalse = C.Match(I, mruleFalse, NONE)
            val matchTrue  = C.Match(I, mruleTrue, SOME matchFalse)
        in
            CASEExp(I, exp1, matchTrue)
        end

    fun ORELSEExp (I, exp1, exp2) = IFExp(I, exp1, TRUEExp(I), exp2)

    fun ANDALSOExp(I, exp1, exp2) = IFExp(I, exp1, exp2, FALSEExp(I))

    fun SEQAtExp(I, exps) =
        let
            val wildcard             = C.ATPATPat(I, C.WILDCARDAtPat(I))

            fun toExpSeq []          = raise Fail "DerivedFormsCore.SEQAtExp: \
                                                  \empty exp list"
              | toExpSeq [exp]       = exp
              | toExpSeq(exp::exps') =
                  let
                      val mrule = C.Mrule(I, wildcard, toExpSeq exps')
                      val match = C.Match(I, mrule, NONE)
                  in
                      CASEExp(I, exp, match)
                  end
        in
            C.PARAtExp(I, toExpSeq exps)
        end

    fun LETAtExp(I, dec, [exp]) = C.LETAtExp(I, dec, exp)
      | LETAtExp(I, dec,  exps) =
            C.LETAtExp(I, dec, C.ATEXPExp(I, SEQAtExp(I, exps)))

    fun WHILEExp(I, exp1, exp2) =
        let
            val vid       = VId.invent()
            val vidExp    = VIDExp(I, vid)
            val unitAtExp = UNITAtExp(I)
            val unitExp   = C.ATEXPExp(I, unitAtExp)
            val callVid   = C.APPExp(I, vidExp, unitAtExp)

            val seqExp    = C.ATEXPExp(I, SEQAtExp(I, [exp2, callVid]))
            val fnBody    = IFExp(I, exp1, seqExp, unitExp)
            val mrule     = C.Mrule(I, C.ATPATPat(I, UNITAtPat(I)), fnBody)
            val match     = C.Match(I, mrule, NONE)
            val fnExp     = C.FNExp(I, match)
            val fnBind    = C.PLAINValBind(I, VIDPat(I, vid), fnExp, NONE)
            val valbind   = C.RECValBind(I, fnBind)
            val dec       = C.VALDec(I, C.TyVarseq(I, []), valbind)
        in
            C.ATEXPExp(I, C.LETAtExp(I, dec, callVid))
        end

    fun LISTAtExp(I, exps) =
        let
            fun toExpList    []       = NILExp(I)
              | toExpList(exp::exps') =
                  C.APPExp(I, CONSExp(I), TUPLEAtExp(I, [exp, toExpList exps']))
        in
            C.PARAtExp(I, toExpList exps)
        end


    (* Type Expressions [Figure 16] *)

    fun TUPLETy(I, [ty]) = ty
      | TUPLETy(I,  tys) =
        let
            fun toTyRow(n,   []    ) = NONE
              | toTyRow(n, ty::tys') =
                  SOME(C.TyRow(I, Lab.fromInt n, ty, toTyRow(n+1, tys')))
        in
            C.RECORDTy(I, toTyRow(1, tys))
        end


    (* Function-value Bindings [Figure 17] *)

    fun FvalBind(I, (match, vid, arity), fvalbind_opt) =
        let
            fun abstract(0, vidExps) =
                let
                    val exp = C.ATEXPExp(I, TUPLEAtExp(I, List.rev vidExps))
                in
                    CASEExp(I, exp, match)
                end

              | abstract(n, vidExps) =
                let
                    val vid   = VId.invent()
                    val exp   = VIDExp(I, vid)
                    val pat   = VIDPat(I, vid)
                    val mrule = C.Mrule(I, pat, abstract(n-1, exp::vidExps))
                in
                    C.FNExp(I, C.Match(I, mrule, NONE))
                end

            val exp = abstract(arity, [])
            val pat = VIDPat(I, vid)
        in
            C.PLAINValBind(I, pat, exp, fvalbind_opt)
        end


    fun Fmatch(I, (mrule, vid, arity), NONE) =
            ( C.Match(I, mrule, NONE), vid, arity )

      | Fmatch(I, (mrule, vid, arity), SOME(match, vid', arity')) =
            if vid <> vid' then
                Error.error(I, "inconsistent function identifier")
             else if arity <> arity' then
                Error.error(I, "inconsistent function arity")
             else
                ( C.Match(I, mrule, SOME match), vid, arity )


    fun Fmrule(I, _, vid, atpats, ty_opt, exp) =
        let
            val pats = List.map (fn atpat => C.ATPATPat(I, atpat)) atpats
            val pat' = C.ATPATPat(I, TUPLEAtPat(I, pats))
            val exp' = case ty_opt
                         of NONE    => exp
                          | SOME ty => C.TYPEDExp(I, exp, ty)
            val arity = List.length atpats
        in
            ( C.Mrule(I, pat', exp'), vid, arity )
        end


    (* Declarations [Figure 17] *)

    fun FUNDec(I, tyvarseq, fvalbind) =
            C.VALDec(I, tyvarseq, C.RECValBind(I, fvalbind))

    fun DATATYPEDec(I, datbind, NONE)         = C.DATATYPEDec(I, datbind)
      | DATATYPEDec(I, datbind, SOME typbind) =
        let
            val datbind' = rewriteDatBind typbind datbind
        in
            C.SEQDec(I, C.DATATYPEDec(C.infoDatBind datbind, datbind'),
                        C.TYPEDec(C.infoTypBind typbind, typbind))
        end

    fun ABSTYPEDec(I, datbind, NONE, dec)         = C.ABSTYPEDec(I, datbind,dec)
      | ABSTYPEDec(I, datbind, SOME typbind, dec) =
        let
            val I'       = C.infoTypBind typbind
            val datbind' = rewriteDatBind typbind datbind
        in
            C.ABSTYPEDec(I, datbind', C.SEQDec(I, C.TYPEDec(I', typbind), dec))
        end

  end
(* stop of DerivedFormsCore.sml *)
(* start of DERIVED_FORMS_MODULE.sml *)
(*
 * Standard ML modules derived forms
 *
 * Definition, Appendix A
 *
 * Notes:
 * - A phrase named SynDesc has been added to factorize type synonym
 *   specifications.
 * - Similarly, a phrase named TyReaDesc has been added to factorize type
 *   realisation signature expressions.
 * - The structure sharing derived form is missing since it cannot be resolved
 *   syntactically. It has been moved to the bare grammar.
 *)


signature DERIVED_FORMS_MODULE =
  sig

    (* Import *)

    type Info       = GrammarModule.Info

    type VId        = GrammarCore.VId
    type TyCon      = GrammarCore.TyCon
    type StrId      = GrammarCore.StrId
    type SigId      = GrammarModule.SigId
    type FunId      = GrammarModule.FunId
    type longTyCon  = GrammarCore.longTyCon

    type Ty         = GrammarCore.Ty
    type TyVarseq   = GrammarCore.TyVarseq

    type StrExp     = GrammarModule.StrExp
    type StrDec     = GrammarModule.StrDec
    type StrBind    = GrammarModule.StrBind
    type SigExp     = GrammarModule.SigExp
    type TyReaDesc  = (Info * TyVarseq * longTyCon * Ty) list
    type Spec       = GrammarModule.Spec
    type SynDesc    = (Info * TyVarseq * TyCon * Ty) list
    type FunBind    = GrammarModule.FunBind


    (* Structure Bindings [Figure 18] *)

    val TRANSStrBind:     Info * StrId * SigExp option * StrExp
                               * StrBind option                 -> StrBind
    val OPAQStrBind:      Info * StrId * SigExp * StrExp
                               * StrBind option                 -> StrBind

    (* Structure Expressions [Figure 18] *)

    val APPDECStrExp:     Info * FunId * StrDec                 -> StrExp

    (* Functor Bindings [Figure 18] *)

    val TRANSFunBind:     Info * FunId * StrId * SigExp * SigExp option
                               * StrExp * FunBind option        -> FunBind
    val OPAQFunBind:      Info * FunId * StrId * SigExp * SigExp
                               * StrExp * FunBind option        -> FunBind
    val TRANSSPECFunBind: Info * FunId * Spec * SigExp option
                               * StrExp * FunBind option        -> FunBind
    val OPAQSPECFunBind:  Info * FunId * Spec * SigExp
                               * StrExp * FunBind option        -> FunBind

    (* Specifications [Figure 19] *)

    val SYNSpec:          Info * SynDesc                        -> Spec
    val INCLUDEMULTISpec: Info * SigId list                     -> Spec

    val SynDesc:          Info * TyVarseq * TyCon * Ty
                               * SynDesc option                 -> SynDesc

    (* Signature Expressions [Figure 19] *)

    val WHERETYPESigExp:  Info * SigExp * TyReaDesc             -> SigExp

    val TyReaDesc:        Info * TyVarseq * longTyCon * Ty
                               * TyReaDesc option               -> TyReaDesc
  end
(* stop of DERIVED_FORMS_MODULE.sml *)
(* start of DerivedFormsModule.sml *)
(*
 * Standard ML modules derived forms
 *
 * Definition, Appendix A
 *
 * Notes:
 * - A phrase named SynDesc has been added to factorize type synonym
 *   specifications.
 * - Similarly, a phrase named TyReaDesc has been added to factorize type
 *   realisation signature expressions.
 * - The structure sharing derived form is missing since it cannot be resolved
 *   syntactically. It has been moved to the bare grammar.
 *)


structure DerivedFormsModule :> DERIVED_FORMS_MODULE =
  struct

    (* Import *)

    structure C     = GrammarCore
    structure M     = GrammarModule

    type Info       = M.Info

    type VId        = M.VId
    type TyCon      = M.TyCon
    type StrId      = M.StrId
    type SigId      = M.SigId
    type FunId      = M.FunId
    type longTyCon  = M.longTyCon

    type Ty         = M.Ty
    type TyVarseq   = M.TyVarseq

    type StrExp     = M.StrExp
    type StrDec     = M.StrDec
    type StrBind    = M.StrBind
    type SigExp     = M.SigExp
    type TyReaDesc  = (M.Info * M.TyVarseq * M.longTyCon * M.Ty) list
    type Spec       = M.Spec
    type SynDesc    = (M.Info * M.TyVarseq * M.TyCon * M.Ty) list
    type FunBind    = M.FunBind


    (* Structure Bindings [Figure 18] *)

    fun TRANSStrBind(I, strid, NONE, strexp, strbind_opt) =
            M.StrBind(I, strid, strexp, strbind_opt)

      | TRANSStrBind(I, strid, SOME sigexp, strexp, strbind_opt) =
            M.StrBind(I, strid, M.TRANSStrExp(I, strexp, sigexp), strbind_opt)

    fun OPAQStrBind(I, strid, sigexp, strexp, strbind_opt) =
            M.StrBind(I, strid, M.OPAQStrExp(I, strexp, sigexp), strbind_opt)


    (* Structure Expressions [Figure 18] *)

    fun APPDECStrExp(I, funid, strdec) =
            M.APPStrExp(I, funid, M.STRUCTStrExp(M.infoStrDec strdec, strdec))


    (* Functor Bindings [Figure 18] *)

    fun TRANSFunBind(I, funid, strid, sigexp, NONE, strexp, funbind_opt) =
            M.FunBind(I, funid, strid, sigexp, strexp, funbind_opt)

      | TRANSFunBind(I, funid, strid,sigexp, SOME sigexp', strexp, funbind_opt)=
            M.FunBind(I, funid, strid, sigexp, M.TRANSStrExp(I, strexp,sigexp'),
                         funbind_opt)

    fun OPAQFunBind(I, funid, strid, sigexp, sigexp', strexp, funbind_opt) =
            M.FunBind(I, funid, strid, sigexp, M.OPAQStrExp(I, strexp, sigexp'),
                         funbind_opt)


    fun TRANSSPECFunBind(I, funid, spec, sigexp_opt, strexp, funbind_opt) =
        let
            val I'     = M.infoStrExp strexp
            val strid  = StrId.invent()
            val sigexp = M.SIGSigExp(M.infoSpec spec, spec)

            val strdec = M.DECStrDec(I', C.OPENDec(I',[LongStrId.fromId strid]))
            val strexp'= case sigexp_opt
                           of NONE         => strexp
                            | SOME sigexp' => M.TRANSStrExp(I', strexp, sigexp')
            val letexp = M.LETStrExp(I', strdec, strexp')
        in
            M.FunBind(I, funid, strid, sigexp, letexp, funbind_opt)
        end

    fun OPAQSPECFunBind(I, funid, spec, sigexp', strexp, funbind_opt) =
        let
            val I'     = M.infoStrExp strexp
            val strid  = StrId.invent()
            val sigexp = M.SIGSigExp(M.infoSpec spec, spec)

            val strdec = M.DECStrDec(I', C.OPENDec(I',[LongStrId.fromId strid]))
            val strexp'= M.TRANSStrExp(I', strexp, sigexp')
            val letexp = M.LETStrExp(I', strdec, strexp')
        in
            M.FunBind(I, funid, strid, sigexp, letexp, funbind_opt)
        end


    (* Specifications [Figure 19] *)

    fun SYNSpec(I, [])                            = M.EMPTYSpec(I)
      | SYNSpec(I, (I',tyvarseq,tycon,ty)::syns') =
        let
            val longtycon = LongTyCon.fromId tycon
            val typdesc = M.TypDesc(I', tyvarseq, tycon, NONE)
            val sigexp  = M.SIGSigExp(I', M.TYPESpec(I', typdesc))
            val sigexp' = M.WHERETYPESigExp(I', sigexp, tyvarseq, longtycon, ty)
            val spec1   = M.INCLUDESpec(I', sigexp')
        in
            M.SEQSpec(I, spec1, SYNSpec(I, syns'))
        end

    fun INCLUDEMULTISpec(I,      []       ) = M.EMPTYSpec(I)
      | INCLUDEMULTISpec(I, sigid::sigids') =
        let
            val spec1 = M.INCLUDESpec(I, M.SIGIDSigExp(I, sigid))
        in
            M.SEQSpec(I, spec1, INCLUDEMULTISpec(I, sigids'))
        end


    fun SynDesc(I, tyvarseq, tycon, ty, NONE) =
            (I, tyvarseq, tycon, ty) :: []

      | SynDesc(I, tyvarseq, tycon, ty, SOME syndesc) =
            (I, tyvarseq, tycon, ty) :: syndesc


    (* Signature Expressions [Figure 19] *)

    fun WHERETYPESigExp(I, sigexp,                           []     ) = sigexp
      | WHERETYPESigExp(I, sigexp, (I',tyvarseq,longtycon,ty)::reas') =
        let
            val sigexp' = M.WHERETYPESigExp(I', sigexp, tyvarseq, longtycon, ty)
        in
            WHERETYPESigExp(I, sigexp', reas')
        end


    fun TyReaDesc(I, tyvarseq, longtycon, ty, NONE) =
            (I, tyvarseq, longtycon, ty) :: []

      | TyReaDesc(I, tyvarseq, longtycon, ty, SOME tyreadesc) =
            (I, tyvarseq, longtycon, ty) :: tyreadesc

  end
(* stop of DerivedFormsModule.sml *)
(* start of DERIVED_FORMS_PROGRAM.sml *)
(*
 * Standard ML program derived forms
 *
 * Definition, Appendix A
 *)


signature DERIVED_FORMS_PROGRAM =
  sig

    (* Import *)

    type Info    = GrammarProgram.Info

    type Exp     = GrammarCore.Exp
    type TopDec  = GrammarModule.TopDec
    type Program = GrammarProgram.Program


    (* Programs [Figure 18] *)

    val TOPDECProgram:  Info * TopDec * Program option -> Program
    val EXPProgram:     Info *  Exp   * Program option -> Program

  end
(* stop of DERIVED_FORMS_PROGRAM.sml *)
(* start of DerivedFormsProgram.sml *)
(*
 * Standard ML program derived forms
 *
 * Definition, Appendix A
 *)


structure DerivedFormsProgram :> DERIVED_FORMS_PROGRAM =
  struct

    (* Import *)

    structure C  = GrammarCore
    structure M  = GrammarModule
    structure P  = GrammarProgram

    type Info    = GrammarProgram.Info

    type Exp     = GrammarCore.Exp
    type TopDec  = GrammarModule.TopDec
    type Program = GrammarProgram.Program


    (* Programs [Figure 18] *)

    fun TOPDECProgram(I, topdec, program_opt) =
            P.Program(I, topdec, program_opt)

    fun EXPProgram(I, exp, program_opt) =
        let
            val longvid = LongVId.fromId(VId.fromString "it")
            val pat     = C.ATPATPat(I, C.LONGVIDAtPat(I, C.SANSOp, longvid))
            val valbind = C.PLAINValBind(I, pat, exp, NONE)
            val dec     = C.VALDec(I, C.TyVarseq(I, []), valbind)
            val topdec  = M.STRDECTopDec(I, M.DECStrDec(I, dec), NONE)
        in
            P.Program(I, topdec, program_opt)
        end

  end
(* stop of DerivedFormsProgram.sml *)
(* start of Parser.grm.sig *)
signature Parser_TOKENS =
sig
type ('a,'b) token
type svalue
val LONGID: (string list*string) *  'a * 'a -> (svalue,'a) token
val ETYVAR: (string) *  'a * 'a -> (svalue,'a) token
val TYVAR: (string) *  'a * 'a -> (svalue,'a) token
val STAR:  'a * 'a -> (svalue,'a) token
val SYMBOL: (string) *  'a * 'a -> (svalue,'a) token
val ALPHA: (string) *  'a * 'a -> (svalue,'a) token
val CHAR: (char) *  'a * 'a -> (svalue,'a) token
val STRING: (string) *  'a * 'a -> (svalue,'a) token
val REAL: (real) *  'a * 'a -> (svalue,'a) token
val WORD: (word) *  'a * 'a -> (svalue,'a) token
val INT: (int) *  'a * 'a -> (svalue,'a) token
val NUMERIC: (int) *  'a * 'a -> (svalue,'a) token
val DIGIT: (int) *  'a * 'a -> (svalue,'a) token
val ZERO:  'a * 'a -> (svalue,'a) token
val COLONGREATER:  'a * 'a -> (svalue,'a) token
val WHERE:  'a * 'a -> (svalue,'a) token
val STRUCTURE:  'a * 'a -> (svalue,'a) token
val STRUCT:  'a * 'a -> (svalue,'a) token
val SIGNATURE:  'a * 'a -> (svalue,'a) token
val SIG:  'a * 'a -> (svalue,'a) token
val SHARING:  'a * 'a -> (svalue,'a) token
val INCLUDE:  'a * 'a -> (svalue,'a) token
val FUNCTOR:  'a * 'a -> (svalue,'a) token
val EQTYPE:  'a * 'a -> (svalue,'a) token
val HASH:  'a * 'a -> (svalue,'a) token
val ARROW:  'a * 'a -> (svalue,'a) token
val DARROW:  'a * 'a -> (svalue,'a) token
val EQUALS:  'a * 'a -> (svalue,'a) token
val BAR:  'a * 'a -> (svalue,'a) token
val UNDERBAR:  'a * 'a -> (svalue,'a) token
val DOTS:  'a * 'a -> (svalue,'a) token
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
val RBRACK:  'a * 'a -> (svalue,'a) token
val LBRACK:  'a * 'a -> (svalue,'a) token
val RPAR:  'a * 'a -> (svalue,'a) token
val LPAR:  'a * 'a -> (svalue,'a) token
val WHILE:  'a * 'a -> (svalue,'a) token
val WITHTYPE:  'a * 'a -> (svalue,'a) token
val WITH:  'a * 'a -> (svalue,'a) token
val VAL:  'a * 'a -> (svalue,'a) token
val TYPE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val REC:  'a * 'a -> (svalue,'a) token
val RAISE:  'a * 'a -> (svalue,'a) token
val ORELSE:  'a * 'a -> (svalue,'a) token
val OPEN:  'a * 'a -> (svalue,'a) token
val OP:  'a * 'a -> (svalue,'a) token
val OF:  'a * 'a -> (svalue,'a) token
val NONFIX:  'a * 'a -> (svalue,'a) token
val LOCAL:  'a * 'a -> (svalue,'a) token
val LET:  'a * 'a -> (svalue,'a) token
val INFIXR:  'a * 'a -> (svalue,'a) token
val INFIX:  'a * 'a -> (svalue,'a) token
val IN:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val HANDLE:  'a * 'a -> (svalue,'a) token
val FUN:  'a * 'a -> (svalue,'a) token
val FN:  'a * 'a -> (svalue,'a) token
val EXCEPTION:  'a * 'a -> (svalue,'a) token
val END:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val DATATYPE:  'a * 'a -> (svalue,'a) token
val DO:  'a * 'a -> (svalue,'a) token
val CASE:  'a * 'a -> (svalue,'a) token
val AS:  'a * 'a -> (svalue,'a) token
val ANDALSO:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val ABSTYPE:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature Parser_LRVALS=
sig
structure Tokens : Parser_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
(* stop of Parser.grm.sig *)
(* start of Parser.grm.sml *)
functor LrValsFn(structure Token: TOKEN) = 
struct
structure ParserData=
struct
structure Header = 
struct
(*                                                                              *)
(* Standard ML syntactical analysis                                             *)
(*                                                                              *)
(* Definition, sections 2, 3, and 8, Appendix A and B                           *)
(*                                                                              *)
(* Notes:                                                                       *)
(*   - Two phrases named Fmatch and Fmrule have been added to factorize         *)
(*     Fvalbind.                                                                *)
(*   - A phrase named SynDesc has been added to factorize type synonym          *)
(*     specifications. Similarly, a phrase named TyReaDesc has been added to    *)
(*     factorize type realisation signature expressions.                        *)
(*   - Infix expressions [Definition, section 2.6] are resolved externally in   *)
(*     structure Infix. The parser just maintains the infix environment J by    *)
(*     side effect. To achieve correct treatment of scoped fixity directives,   *)
(*     a stack of environments is used. To handle `local' we even need a        *)
(*     second environment J' (together with a a second stack).                  *)
(*   - Syntactic restrictions [Definition, sections 2.9 and 3.5] are checked    *)
(*     during elaboration, as well as the Fvalbind derived form.                *)
(*   - The Definition is not clear about whether `=' should also be legal as    *)
(*     a tycon. Since this would result in massive conflicts, and a type named  *)
(*     `=' could only be used legally if an implementation would be mad enough  *)
(*     to predefine it anyway, we simply disallow it.                           *)
(*   - The Definition is also vague about what consists a non-infixed occurance *)
(*     of an infix identifier: we assume any occurances in expressions          *)
(*     or patterns. This implies that uses of the keyword `op' in constructor   *)
(*     and exception bindings are completely redundant.                         *)
(*   - Datatype replication requires rules for datatype to be duplicated to     *)
(*     avoid conflicts on empty tyvarseqs.                                      *)
(*   - Layered patterns require some grammar transformation hack, see pat.      *)
(*   - The messy `sigexp where type ... and type ...' syntax requires some      *)
(*     really ugly transformations (in absence of a lookahead of 2), watch out  *)
(*     for non-terminals of the form xxx__AND_yyybind_opt.                      *)
(*   - ML-Yacc does not seem to like comments that stretch over several         *)
(*     lines... Similarly, comments in semantic actions make it puke...         *)
(*                                                                              *)
(* Bugs:                                                                        *)
(*   - We do NOT support declarations like                                      *)
(*        fun f p1 = case e1 of p2 => e2                                        *)
(*          | f p3 = e3                                                         *)
(*     (without parentheses around the case) because the transformations        *)
(*     required to support this would be even a magnitude uglier than those     *)
(*     above. In fact, no compiler I know of supports this.                     *)
(*                                                                              *)



    (* Import *)

    open GrammarCore
    open GrammarModule
    open GrammarProgram
    open DerivedFormsCore
    open DerivedFormsModule
    open DerivedFormsProgram


    (* Helper to build info fields *)

    fun I(left, right) = if right = 0 then (left, left) else (left, right)


    (* Handling infix environments *)

    val J  = ref Infix.empty    (* context *)
    val J' = ref Infix.empty    (* local environment (+ enclosing one) *)

    val stackJ  = ref [] : Infix.InfEnv list ref
    val stackJ' = ref [] : Infix.InfEnv list ref

    fun initJandJ'(J0) =
        (
            J       := J0;
            J'      := J0;
            stackJ  := [];
            stackJ' := []
        )

    fun pushJ() =
        (
            stackJ  := !J :: !stackJ
        )

    fun popJ() =
        (
            J       := List.hd(!stackJ);
            stackJ  := List.tl(!stackJ)
        )

    fun pushJ'shiftJ() =
        (
            stackJ' := !J' :: !stackJ';
            J'      := List.hd(!stackJ)
        )

    fun popJandJ'() =
        (
            J       := !J';
            J'      := List.hd(!stackJ');
            stackJ  := List.tl(!stackJ);
            stackJ' := List.tl(!stackJ')
        )


    fun assignInfix(infstatus, vids) =
        (
            J  := Infix.assign(!J, vids, infstatus);
            J' := Infix.assign(!J', vids, infstatus)
        )

    fun cancelInfix(vids) =
        (
            J  := Infix.cancel(!J, vids);
            J' := Infix.cancel(!J', vids)
        )


    (* Helper for long identifiers *)

    fun toLongId toId (strids, id) =
            ( List.map StrId.fromString strids, toId id )


    (* Helper to handle typed patterns (needed because of layered patterns) *)

    fun typedPat(pat,   []   ) = pat
      | typedPat(pat, ty::tys) =
        let
            val I = Source.over(infoPat pat, infoTy ty)
        in
            typedPat(TYPEDPat(I, pat, ty), tys)
        end




end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\184\003\002\000\049\000\006\000\048\000\008\000\047\000\
\\011\000\046\000\012\000\045\000\013\000\044\000\015\000\043\000\
\\017\000\042\000\018\000\041\000\019\000\040\000\020\000\039\000\
\\021\000\038\000\023\000\037\000\024\000\036\000\026\000\035\000\
\\029\000\034\000\030\000\033\000\033\000\032\000\034\000\031\000\
\\036\000\030\000\038\000\029\000\042\000\174\003\046\000\151\002\
\\049\000\028\000\051\000\027\000\055\000\026\000\057\000\025\000\
\\060\000\024\000\061\000\023\000\062\000\022\000\063\000\021\000\
\\064\000\020\000\065\000\019\000\066\000\018\000\067\000\017\000\
\\068\000\151\002\069\000\151\002\070\000\151\002\073\000\151\002\000\000\
\\001\000\002\000\139\002\003\000\139\002\008\000\139\002\010\000\139\002\
\\011\000\139\002\013\000\139\002\016\000\139\002\017\000\139\002\
\\018\000\139\002\020\000\139\002\021\000\139\002\024\000\139\002\
\\029\000\139\002\030\000\139\002\034\000\141\002\035\000\139\002\
\\041\000\139\002\042\000\139\002\051\000\139\002\055\000\139\002\
\\057\000\139\002\059\000\139\002\000\000\
\\001\000\002\000\178\002\003\000\178\002\004\000\178\002\007\000\178\002\
\\008\000\178\002\009\000\178\002\010\000\178\002\011\000\178\002\
\\013\000\178\002\014\000\178\002\016\000\178\002\017\000\178\002\
\\018\000\178\002\019\000\040\000\020\000\178\002\021\000\178\002\
\\022\000\178\002\023\000\037\000\024\000\178\002\025\000\178\002\
\\028\000\178\002\029\000\178\002\030\000\178\002\034\000\031\000\
\\035\000\178\002\036\000\030\000\037\000\178\002\038\000\029\000\
\\039\000\178\002\040\000\178\002\041\000\178\002\042\000\178\002\
\\045\000\178\002\046\000\151\002\049\000\028\000\051\000\178\002\
\\055\000\178\002\057\000\178\002\060\000\024\000\061\000\023\000\
\\062\000\022\000\063\000\021\000\064\000\020\000\065\000\019\000\
\\066\000\018\000\067\000\017\000\068\000\151\002\069\000\151\002\
\\070\000\151\002\073\000\151\002\000\000\
\\001\000\002\000\034\003\003\000\034\003\004\000\034\003\005\000\034\003\
\\007\000\034\003\008\000\034\003\009\000\034\003\010\000\034\003\
\\011\000\034\003\013\000\034\003\014\000\034\003\016\000\034\003\
\\017\000\034\003\018\000\034\003\020\000\034\003\021\000\034\003\
\\022\000\034\003\024\000\034\003\025\000\034\003\028\000\034\003\
\\029\000\034\003\030\000\034\003\031\000\034\003\032\000\034\003\
\\035\000\034\003\037\000\034\003\039\000\034\003\040\000\034\003\
\\041\000\034\003\042\000\034\003\045\000\034\003\046\000\034\003\
\\047\000\034\003\048\000\034\003\050\000\034\003\051\000\034\003\
\\052\000\034\003\053\000\034\003\055\000\034\003\057\000\034\003\
\\058\000\034\003\059\000\034\003\068\000\045\003\069\000\045\003\
\\070\000\248\000\073\000\045\003\000\000\
\\001\000\002\000\075\003\003\000\148\001\008\000\075\003\010\000\075\003\
\\011\000\075\003\013\000\075\003\016\000\075\003\017\000\075\003\
\\018\000\075\003\020\000\075\003\021\000\075\003\024\000\075\003\
\\029\000\075\003\030\000\075\003\035\000\075\003\041\000\056\003\
\\042\000\075\003\051\000\075\003\055\000\075\003\057\000\075\003\
\\059\000\056\003\000\000\
\\001\000\002\000\075\003\003\000\148\001\008\000\075\003\010\000\075\003\
\\011\000\075\003\013\000\075\003\016\000\075\003\017\000\075\003\
\\018\000\075\003\020\000\075\003\021\000\075\003\024\000\075\003\
\\029\000\075\003\030\000\075\003\035\000\075\003\041\000\086\003\
\\042\000\075\003\051\000\075\003\055\000\075\003\057\000\075\003\
\\058\000\086\003\059\000\086\003\000\000\
\\001\000\002\000\075\003\003\000\097\002\008\000\075\003\010\000\075\003\
\\011\000\075\003\013\000\075\003\016\000\075\003\017\000\075\003\
\\018\000\075\003\020\000\075\003\021\000\075\003\024\000\075\003\
\\029\000\075\003\030\000\075\003\035\000\075\003\041\000\101\003\
\\042\000\075\003\051\000\075\003\055\000\075\003\057\000\075\003\
\\058\000\101\003\059\000\101\003\000\000\
\\001\000\002\000\093\003\003\000\086\001\008\000\093\003\011\000\093\003\
\\013\000\093\003\017\000\093\003\018\000\093\003\020\000\093\003\
\\021\000\093\003\024\000\093\003\029\000\093\003\030\000\093\003\
\\042\000\093\003\051\000\093\003\055\000\093\003\057\000\093\003\
\\058\000\086\003\000\000\
\\001\000\002\000\093\003\003\000\077\002\008\000\093\003\011\000\093\003\
\\013\000\093\003\017\000\093\003\018\000\093\003\020\000\093\003\
\\021\000\093\003\024\000\093\003\029\000\093\003\030\000\093\003\
\\042\000\093\003\051\000\093\003\055\000\093\003\057\000\093\003\
\\058\000\101\003\000\000\
\\001\000\002\000\164\003\003\000\032\002\008\000\164\003\011\000\164\003\
\\013\000\164\003\017\000\164\003\018\000\164\003\020\000\164\003\
\\021\000\164\003\024\000\164\003\029\000\164\003\030\000\164\003\
\\041\000\056\003\042\000\164\003\051\000\164\003\055\000\164\003\
\\057\000\164\003\059\000\056\003\000\000\
\\001\000\002\000\164\003\003\000\032\002\008\000\164\003\011\000\164\003\
\\013\000\164\003\017\000\164\003\018\000\164\003\020\000\164\003\
\\021\000\164\003\024\000\164\003\029\000\164\003\030\000\164\003\
\\041\000\086\003\042\000\164\003\051\000\164\003\055\000\164\003\
\\057\000\164\003\058\000\086\003\059\000\086\003\000\000\
\\001\000\002\000\164\003\003\000\108\002\008\000\164\003\011\000\164\003\
\\013\000\164\003\017\000\164\003\018\000\164\003\020\000\164\003\
\\021\000\164\003\024\000\164\003\029\000\164\003\030\000\164\003\
\\041\000\101\003\042\000\164\003\051\000\164\003\055\000\164\003\
\\057\000\164\003\058\000\101\003\059\000\101\003\000\000\
\\001\000\002\000\049\000\006\000\048\000\008\000\047\000\011\000\046\000\
\\012\000\045\000\013\000\044\000\015\000\043\000\017\000\042\000\
\\018\000\041\000\019\000\040\000\020\000\039\000\021\000\038\000\
\\023\000\037\000\024\000\036\000\026\000\035\000\029\000\034\000\
\\030\000\033\000\033\000\032\000\034\000\031\000\036\000\030\000\
\\038\000\029\000\042\000\174\003\046\000\151\002\049\000\028\000\
\\051\000\027\000\055\000\026\000\057\000\025\000\060\000\024\000\
\\061\000\023\000\062\000\022\000\063\000\021\000\064\000\020\000\
\\065\000\019\000\066\000\018\000\067\000\017\000\068\000\151\002\
\\069\000\151\002\070\000\151\002\073\000\151\002\000\000\
\\001\000\003\000\005\002\008\000\152\003\010\000\152\003\011\000\152\003\
\\029\000\152\003\030\000\152\003\035\000\152\003\050\000\152\003\
\\052\000\152\003\053\000\152\003\057\000\152\003\058\000\086\003\000\000\
\\001\000\003\000\101\002\008\000\152\003\010\000\152\003\011\000\152\003\
\\029\000\152\003\030\000\152\003\035\000\152\003\050\000\152\003\
\\052\000\152\003\053\000\152\003\057\000\152\003\058\000\101\003\000\000\
\\001\000\004\000\059\000\007\000\183\000\014\000\058\000\025\000\057\000\
\\041\000\056\000\000\000\
\\001\000\004\000\059\000\009\000\131\001\014\000\058\000\025\000\057\000\
\\041\000\056\000\000\000\
\\001\000\004\000\059\000\014\000\058\000\022\000\242\000\025\000\057\000\
\\041\000\056\000\000\000\
\\001\000\004\000\059\000\014\000\058\000\025\000\057\000\028\000\224\000\
\\041\000\056\000\000\000\
\\001\000\004\000\059\000\014\000\058\000\025\000\057\000\035\000\180\000\
\\040\000\179\000\041\000\056\000\042\000\178\000\000\000\
\\001\000\004\000\059\000\014\000\058\000\025\000\057\000\041\000\056\000\
\\042\000\055\000\000\000\
\\001\000\005\000\133\002\039\000\133\002\040\000\133\002\041\000\133\002\
\\046\000\126\002\000\000\
\\001\000\005\000\134\002\039\000\134\002\040\000\134\002\041\000\134\002\
\\046\000\127\002\000\000\
\\001\000\005\000\135\002\039\000\135\002\040\000\135\002\041\000\135\002\
\\046\000\128\002\000\000\
\\001\000\005\000\012\003\035\000\029\003\037\000\029\003\039\000\029\003\
\\040\000\029\003\041\000\186\000\046\000\029\003\047\000\029\003\000\000\
\\001\000\005\000\013\003\023\000\144\002\034\000\144\002\035\000\144\002\
\\036\000\144\002\037\000\144\002\038\000\144\002\039\000\144\002\
\\040\000\144\002\041\000\031\001\044\000\144\002\046\000\144\002\
\\047\000\144\002\060\000\144\002\061\000\144\002\062\000\144\002\
\\063\000\144\002\064\000\144\002\065\000\144\002\066\000\144\002\
\\067\000\144\002\068\000\144\002\069\000\144\002\070\000\144\002\
\\073\000\144\002\000\000\
\\001\000\005\000\118\001\000\000\
\\001\000\006\000\048\000\012\000\045\000\015\000\043\000\019\000\040\000\
\\023\000\037\000\026\000\035\000\033\000\032\000\034\000\031\000\
\\036\000\030\000\037\000\163\002\038\000\029\000\046\000\151\002\
\\049\000\028\000\060\000\024\000\061\000\023\000\062\000\022\000\
\\063\000\021\000\064\000\020\000\065\000\019\000\066\000\018\000\
\\067\000\017\000\068\000\151\002\069\000\151\002\070\000\151\002\
\\073\000\151\002\000\000\
\\001\000\008\000\233\001\068\000\067\000\069\000\066\000\070\000\065\000\000\000\
\\001\000\010\000\157\001\000\000\
\\001\000\010\000\198\001\000\000\
\\001\000\010\000\238\001\000\000\
\\001\000\010\000\020\002\000\000\
\\001\000\010\000\027\002\000\000\
\\001\000\010\000\070\002\000\000\
\\001\000\010\000\072\002\000\000\
\\001\000\016\000\045\001\000\000\
\\001\000\016\000\047\001\000\000\
\\001\000\016\000\190\001\000\000\
\\001\000\016\000\251\001\000\000\
\\001\000\019\000\081\001\056\000\080\001\068\000\079\001\073\000\118\000\000\000\
\\001\000\023\000\037\000\034\000\142\000\035\000\018\003\036\000\107\000\
\\037\000\018\003\038\000\106\000\039\000\018\003\040\000\018\003\
\\044\000\105\000\046\000\018\003\047\000\018\003\060\000\024\000\
\\061\000\023\000\062\000\022\000\063\000\021\000\064\000\020\000\
\\065\000\019\000\066\000\018\000\067\000\017\000\068\000\151\002\
\\069\000\151\002\070\000\151\002\073\000\151\002\000\000\
\\001\000\023\000\037\000\034\000\142\000\035\000\026\003\036\000\107\000\
\\037\000\026\003\038\000\106\000\039\000\026\003\040\000\026\003\
\\041\000\026\003\044\000\105\000\046\000\026\003\047\000\026\003\
\\060\000\024\000\061\000\023\000\062\000\022\000\063\000\021\000\
\\064\000\020\000\065\000\019\000\066\000\018\000\067\000\017\000\
\\068\000\151\002\069\000\151\002\070\000\151\002\073\000\151\002\000\000\
\\001\000\023\000\037\000\034\000\142\000\036\000\107\000\037\000\003\003\
\\038\000\106\000\044\000\105\000\060\000\024\000\061\000\023\000\
\\062\000\022\000\063\000\021\000\064\000\020\000\065\000\019\000\
\\066\000\018\000\067\000\017\000\068\000\151\002\069\000\151\002\
\\070\000\151\002\073\000\151\002\000\000\
\\001\000\029\000\096\001\068\000\071\000\073\000\118\000\000\000\
\\001\000\029\000\155\001\000\000\
\\001\000\029\000\160\001\000\000\
\\001\000\029\000\160\001\068\000\074\000\000\000\
\\001\000\029\000\162\001\068\000\071\000\073\000\118\000\000\000\
\\001\000\029\000\024\002\000\000\
\\001\000\029\000\024\002\068\000\071\000\000\000\
\\001\000\029\000\040\002\000\000\
\\001\000\029\000\040\002\068\000\071\000\000\000\
\\001\000\029\000\088\002\000\000\
\\001\000\029\000\088\002\068\000\077\000\000\000\
\\001\000\031\000\066\001\000\000\
\\001\000\034\000\113\000\068\000\150\000\069\000\149\000\071\000\104\000\000\000\
\\001\000\034\000\173\000\000\000\
\\001\000\034\000\151\001\000\000\
\\001\000\035\000\181\000\000\000\
\\001\000\035\000\182\000\000\000\
\\001\000\035\000\038\001\000\000\
\\001\000\035\000\040\001\040\000\039\001\000\000\
\\001\000\035\000\041\001\000\000\
\\001\000\035\000\071\001\000\000\
\\001\000\035\000\073\001\040\000\072\001\000\000\
\\001\000\035\000\090\001\000\000\
\\001\000\035\000\217\001\058\000\082\001\000\000\
\\001\000\035\000\246\001\000\000\
\\001\000\035\000\249\001\041\000\248\001\059\000\247\001\000\000\
\\001\000\037\000\177\000\000\000\
\\001\000\037\000\037\001\000\000\
\\001\000\039\000\174\000\000\000\
\\001\000\039\000\033\001\000\000\
\\001\000\039\000\069\001\000\000\
\\001\000\041\000\070\001\000\000\
\\001\000\041\000\091\001\000\000\
\\001\000\041\000\150\001\059\000\149\001\000\000\
\\001\000\041\000\166\001\000\000\
\\001\000\041\000\173\001\000\000\
\\001\000\041\000\034\002\059\000\033\002\000\000\
\\001\000\042\000\050\000\000\000\
\\001\000\043\000\200\000\061\000\083\000\062\000\082\000\068\000\199\000\
\\069\000\198\000\070\000\197\000\000\000\
\\001\000\046\000\068\000\068\000\067\000\069\000\066\000\070\000\065\000\
\\073\000\064\000\000\000\
\\001\000\046\000\122\000\068\000\067\000\069\000\066\000\070\000\065\000\000\000\
\\001\000\046\000\172\000\000\000\
\\001\000\046\000\175\000\000\000\
\\001\000\046\000\187\000\000\000\
\\001\000\046\000\241\000\000\000\
\\001\000\046\000\255\000\000\000\
\\001\000\046\000\035\001\000\000\
\\001\000\046\000\043\001\000\000\
\\001\000\046\000\050\001\000\000\
\\001\000\046\000\059\001\000\000\
\\001\000\046\000\065\001\000\000\
\\001\000\046\000\083\001\058\000\082\001\000\000\
\\001\000\046\000\167\001\000\000\
\\001\000\046\000\177\001\000\000\
\\001\000\046\000\215\001\000\000\
\\001\000\046\000\223\001\000\000\
\\001\000\046\000\227\001\000\000\
\\001\000\046\000\230\001\000\000\
\\001\000\046\000\001\002\058\000\082\001\000\000\
\\001\000\046\000\029\002\000\000\
\\001\000\046\000\030\002\000\000\
\\001\000\046\000\036\002\000\000\
\\001\000\046\000\063\002\058\000\082\001\000\000\
\\001\000\046\000\083\002\000\000\
\\001\000\046\000\084\002\000\000\
\\001\000\046\000\089\002\000\000\
\\001\000\046\000\103\002\000\000\
\\001\000\047\000\233\000\000\000\
\\001\000\054\000\003\001\068\000\074\000\000\000\
\\001\000\058\000\087\001\000\000\
\\001\000\058\000\006\002\000\000\
\\001\000\061\000\083\000\062\000\082\000\068\000\081\000\069\000\080\000\
\\070\000\079\000\000\000\
\\001\000\068\000\067\000\069\000\066\000\070\000\065\000\000\000\
\\001\000\068\000\067\000\069\000\066\000\070\000\065\000\073\000\064\000\000\000\
\\001\000\068\000\067\000\069\000\066\000\070\000\065\000\073\000\192\000\000\000\
\\001\000\068\000\071\000\000\000\
\\001\000\068\000\071\000\073\000\118\000\000\000\
\\001\000\068\000\074\000\000\000\
\\001\000\068\000\077\000\000\000\
\\001\000\068\000\150\000\069\000\149\000\000\000\
\\001\000\068\000\150\000\069\000\149\000\073\000\247\000\000\000\
\\001\000\071\000\104\000\000\000\
\\111\002\000\000\
\\112\002\000\000\
\\113\002\000\000\
\\113\002\041\000\248\001\059\000\247\001\000\000\
\\114\002\000\000\
\\115\002\000\000\
\\116\002\000\000\
\\117\002\000\000\
\\118\002\000\000\
\\119\002\000\000\
\\120\002\000\000\
\\121\002\000\000\
\\122\002\000\000\
\\123\002\000\000\
\\124\002\000\000\
\\125\002\000\000\
\\126\002\000\000\
\\127\002\000\000\
\\128\002\000\000\
\\129\002\000\000\
\\130\002\000\000\
\\131\002\000\000\
\\132\002\000\000\
\\133\002\000\000\
\\134\002\000\000\
\\135\002\000\000\
\\136\002\000\000\
\\137\002\000\000\
\\138\002\000\000\
\\139\002\000\000\
\\140\002\000\000\
\\141\002\000\000\
\\142\002\000\000\
\\143\002\000\000\
\\144\002\000\000\
\\145\002\000\000\
\\145\002\041\000\186\000\000\000\
\\146\002\000\000\
\\147\002\000\000\
\\148\002\000\000\
\\149\002\000\000\
\\150\002\000\000\
\\151\002\006\000\048\000\012\000\045\000\015\000\043\000\019\000\040\000\
\\023\000\037\000\026\000\035\000\033\000\032\000\034\000\031\000\
\\035\000\093\000\036\000\030\000\038\000\029\000\049\000\028\000\
\\060\000\024\000\061\000\023\000\062\000\022\000\063\000\021\000\
\\064\000\020\000\065\000\019\000\066\000\018\000\067\000\017\000\000\000\
\\151\002\006\000\048\000\012\000\045\000\015\000\043\000\019\000\040\000\
\\023\000\037\000\026\000\035\000\033\000\032\000\034\000\031\000\
\\036\000\030\000\038\000\029\000\049\000\028\000\060\000\024\000\
\\061\000\023\000\062\000\022\000\063\000\021\000\064\000\020\000\
\\065\000\019\000\066\000\018\000\067\000\017\000\000\000\
\\151\002\008\000\063\001\023\000\037\000\000\000\
\\151\002\023\000\037\000\000\000\
\\151\002\023\000\037\000\027\000\109\000\034\000\108\000\036\000\107\000\
\\038\000\106\000\044\000\105\000\060\000\024\000\061\000\023\000\
\\062\000\022\000\063\000\021\000\064\000\020\000\065\000\019\000\
\\066\000\018\000\067\000\017\000\071\000\104\000\000\000\
\\151\002\023\000\037\000\027\000\109\000\034\000\142\000\036\000\107\000\
\\038\000\106\000\044\000\105\000\060\000\024\000\061\000\023\000\
\\062\000\022\000\063\000\021\000\064\000\020\000\065\000\019\000\
\\066\000\018\000\067\000\017\000\000\000\
\\151\002\023\000\037\000\034\000\108\000\036\000\107\000\038\000\106\000\
\\044\000\105\000\060\000\024\000\061\000\023\000\062\000\022\000\
\\063\000\021\000\064\000\020\000\065\000\019\000\066\000\018\000\
\\067\000\017\000\071\000\104\000\000\000\
\\151\002\023\000\037\000\034\000\142\000\035\000\208\000\036\000\107\000\
\\038\000\106\000\044\000\105\000\060\000\024\000\061\000\023\000\
\\062\000\022\000\063\000\021\000\064\000\020\000\065\000\019\000\
\\066\000\018\000\067\000\017\000\000\000\
\\151\002\023\000\037\000\034\000\142\000\035\000\208\000\036\000\107\000\
\\038\000\106\000\044\000\105\000\060\000\024\000\061\000\023\000\
\\062\000\022\000\063\000\021\000\064\000\020\000\065\000\019\000\
\\066\000\018\000\067\000\017\000\071\000\104\000\000\000\
\\151\002\023\000\037\000\034\000\142\000\036\000\107\000\038\000\106\000\
\\044\000\105\000\060\000\024\000\061\000\023\000\062\000\022\000\
\\063\000\021\000\064\000\020\000\065\000\019\000\066\000\018\000\
\\067\000\017\000\000\000\
\\152\002\000\000\
\\153\002\000\000\
\\154\002\000\000\
\\155\002\000\000\
\\156\002\000\000\
\\157\002\000\000\
\\158\002\000\000\
\\159\002\000\000\
\\160\002\000\000\
\\161\002\000\000\
\\162\002\000\000\
\\164\002\000\000\
\\165\002\004\000\059\000\014\000\058\000\025\000\057\000\040\000\176\000\
\\041\000\056\000\000\000\
\\166\002\000\000\
\\167\002\000\000\
\\168\002\004\000\059\000\014\000\058\000\025\000\057\000\041\000\056\000\
\\042\000\188\001\000\000\
\\169\002\000\000\
\\170\002\004\000\059\000\014\000\058\000\025\000\057\000\041\000\056\000\
\\042\000\178\000\000\000\
\\171\002\000\000\
\\172\002\000\000\
\\173\002\004\000\059\000\014\000\058\000\025\000\057\000\040\000\114\001\
\\041\000\056\000\000\000\
\\174\002\000\000\
\\175\002\061\000\083\000\062\000\082\000\068\000\081\000\069\000\080\000\
\\070\000\079\000\000\000\
\\176\002\000\000\
\\177\002\000\000\
\\179\002\000\000\
\\180\002\000\000\
\\181\002\004\000\059\000\041\000\056\000\000\000\
\\182\002\004\000\059\000\025\000\057\000\041\000\056\000\000\000\
\\183\002\000\000\
\\184\002\004\000\059\000\014\000\058\000\025\000\057\000\041\000\056\000\000\000\
\\185\002\004\000\059\000\014\000\058\000\025\000\057\000\041\000\056\000\000\000\
\\186\002\004\000\059\000\014\000\058\000\025\000\057\000\041\000\056\000\000\000\
\\187\002\000\000\
\\188\002\000\000\
\\189\002\000\000\
\\190\002\000\000\
\\191\002\045\000\235\000\000\000\
\\192\002\004\000\059\000\014\000\058\000\025\000\057\000\041\000\056\000\000\000\
\\193\002\002\000\049\000\008\000\047\000\011\000\046\000\013\000\044\000\
\\017\000\042\000\018\000\041\000\020\000\221\000\021\000\038\000\
\\024\000\036\000\029\000\034\000\030\000\033\000\042\000\220\000\000\000\
\\194\002\002\000\049\000\008\000\047\000\011\000\046\000\013\000\044\000\
\\017\000\042\000\018\000\041\000\020\000\221\000\021\000\038\000\
\\024\000\036\000\029\000\034\000\030\000\033\000\042\000\220\000\000\000\
\\195\002\000\000\
\\196\002\000\000\
\\197\002\002\000\049\000\008\000\047\000\011\000\046\000\013\000\044\000\
\\017\000\042\000\018\000\041\000\020\000\221\000\021\000\038\000\
\\024\000\036\000\029\000\034\000\030\000\033\000\000\000\
\\198\002\000\000\
\\199\002\000\000\
\\200\002\000\000\
\\201\002\000\000\
\\202\002\000\000\
\\203\002\000\000\
\\204\002\000\000\
\\205\002\000\000\
\\206\002\000\000\
\\207\002\000\000\
\\208\002\000\000\
\\209\002\000\000\
\\210\002\000\000\
\\211\002\000\000\
\\212\002\000\000\
\\213\002\000\000\
\\214\002\032\000\239\000\000\000\
\\215\002\000\000\
\\216\002\046\000\122\000\068\000\067\000\069\000\066\000\070\000\065\000\000\000\
\\217\002\000\000\
\\218\002\068\000\071\000\073\000\118\000\000\000\
\\219\002\000\000\
\\220\002\060\000\128\000\061\000\127\000\000\000\
\\221\002\000\000\
\\222\002\000\000\
\\223\002\000\000\
\\224\002\003\000\117\001\004\000\059\000\014\000\058\000\025\000\057\000\
\\041\000\056\000\000\000\
\\225\002\000\000\
\\226\002\000\000\
\\227\002\003\000\232\000\000\000\
\\228\002\000\000\
\\229\002\000\000\
\\230\002\045\000\230\000\000\000\
\\231\002\004\000\059\000\014\000\058\000\025\000\057\000\041\000\056\000\000\000\
\\232\002\000\000\
\\233\002\000\000\
\\234\002\003\000\186\001\000\000\
\\235\002\000\000\
\\236\002\000\000\
\\237\002\000\000\
\\238\002\000\000\
\\239\002\003\000\139\001\000\000\
\\240\002\000\000\
\\241\002\000\000\
\\242\002\045\000\242\001\000\000\
\\243\002\000\000\
\\244\002\022\000\058\001\000\000\
\\244\002\022\000\058\001\046\000\057\001\000\000\
\\245\002\000\000\
\\246\002\000\000\
\\247\002\000\000\
\\248\002\003\000\134\001\000\000\
\\249\002\000\000\
\\249\002\041\000\186\000\000\000\
\\250\002\000\000\
\\251\002\000\000\
\\252\002\000\000\
\\253\002\000\000\
\\254\002\000\000\
\\255\002\000\000\
\\000\003\000\000\
\\001\003\000\000\
\\002\003\000\000\
\\004\003\000\000\
\\005\003\040\000\036\001\000\000\
\\006\003\000\000\
\\007\003\000\000\
\\008\003\000\000\
\\009\003\000\000\
\\010\003\000\000\
\\011\003\040\000\182\001\000\000\
\\012\003\000\000\
\\013\003\041\000\227\000\000\000\
\\014\003\000\000\
\\015\003\005\000\121\001\000\000\
\\016\003\000\000\
\\017\003\043\000\200\000\061\000\083\000\062\000\082\000\068\000\199\000\
\\069\000\198\000\070\000\197\000\000\000\
\\019\003\041\000\186\000\000\000\
\\020\003\000\000\
\\021\003\000\000\
\\022\003\000\000\
\\023\003\000\000\
\\024\003\000\000\
\\025\003\000\000\
\\027\003\000\000\
\\028\003\000\000\
\\029\003\041\000\186\000\000\000\
\\030\003\048\000\249\000\000\000\
\\031\003\000\000\
\\032\003\000\000\
\\033\003\000\000\
\\035\003\000\000\
\\036\003\000\000\
\\037\003\000\000\
\\038\003\000\000\
\\039\003\000\000\
\\040\003\000\000\
\\041\003\000\000\
\\042\003\040\000\200\001\000\000\
\\043\003\000\000\
\\044\003\061\000\083\000\062\000\082\000\068\000\081\000\069\000\080\000\
\\070\000\079\000\000\000\
\\046\003\034\000\165\000\038\000\164\000\071\000\104\000\000\000\
\\047\003\000\000\
\\048\003\000\000\
\\049\003\040\000\072\001\000\000\
\\050\003\000\000\
\\051\003\034\000\113\000\071\000\104\000\000\000\
\\052\003\000\000\
\\053\003\000\000\
\\054\003\000\000\
\\055\003\040\000\042\001\000\000\
\\056\003\000\000\
\\057\003\058\000\082\001\000\000\
\\057\003\058\000\245\001\000\000\
\\057\003\058\000\079\002\000\000\
\\058\003\058\000\082\001\000\000\
\\058\003\058\000\245\001\000\000\
\\058\003\058\000\079\002\000\000\
\\059\003\000\000\
\\060\003\000\000\
\\061\003\000\000\
\\062\003\000\000\
\\063\003\000\000\
\\064\003\002\000\049\000\008\000\047\000\011\000\046\000\013\000\044\000\
\\017\000\042\000\018\000\041\000\020\000\039\000\021\000\038\000\
\\024\000\036\000\029\000\034\000\030\000\033\000\042\000\216\000\
\\057\000\025\000\000\000\
\\065\003\002\000\049\000\008\000\047\000\011\000\046\000\013\000\044\000\
\\017\000\042\000\018\000\041\000\019\000\081\001\020\000\039\000\
\\021\000\038\000\024\000\036\000\029\000\034\000\030\000\033\000\
\\042\000\216\000\056\000\080\001\057\000\025\000\068\000\079\001\
\\073\000\118\000\000\000\
\\065\003\002\000\049\000\008\000\047\000\011\000\046\000\013\000\044\000\
\\017\000\042\000\018\000\041\000\020\000\039\000\021\000\038\000\
\\024\000\036\000\029\000\034\000\030\000\033\000\042\000\216\000\
\\057\000\025\000\000\000\
\\066\003\000\000\
\\067\003\002\000\049\000\008\000\047\000\011\000\046\000\013\000\044\000\
\\017\000\042\000\018\000\041\000\020\000\039\000\021\000\038\000\
\\024\000\036\000\029\000\034\000\030\000\033\000\057\000\025\000\000\000\
\\068\003\000\000\
\\069\003\000\000\
\\070\003\000\000\
\\071\003\000\000\
\\072\003\000\000\
\\073\003\000\000\
\\074\003\000\000\
\\076\003\000\000\
\\077\003\000\000\
\\078\003\000\000\
\\079\003\000\000\
\\080\003\000\000\
\\081\003\000\000\
\\082\003\000\000\
\\083\003\000\000\
\\084\003\058\000\082\001\000\000\
\\085\003\041\000\171\000\059\000\170\000\000\000\
\\085\003\041\000\171\000\059\000\164\001\000\000\
\\085\003\041\000\171\000\059\000\003\002\000\000\
\\086\003\000\000\
\\087\003\000\000\
\\088\003\000\000\
\\089\003\000\000\
\\089\003\068\000\074\000\000\000\
\\090\003\000\000\
\\091\003\000\000\
\\092\003\000\000\
\\094\003\000\000\
\\095\003\000\000\
\\096\003\000\000\
\\097\003\000\000\
\\098\003\000\000\
\\099\003\000\000\
\\100\003\000\000\
\\101\003\003\000\074\002\000\000\
\\102\003\008\000\020\001\011\000\019\001\029\000\018\001\030\000\017\001\
\\050\000\015\001\052\000\014\001\053\000\089\001\057\000\012\001\000\000\
\\103\003\008\000\020\001\011\000\019\001\029\000\018\001\030\000\017\001\
\\042\000\016\001\050\000\015\001\052\000\014\001\053\000\013\001\
\\057\000\012\001\000\000\
\\103\003\008\000\020\001\011\000\019\001\029\000\018\001\030\000\017\001\
\\042\000\016\001\050\000\015\001\052\000\014\001\053\000\013\001\
\\057\000\012\001\068\000\071\000\000\000\
\\104\003\000\000\
\\105\003\000\000\
\\106\003\000\000\
\\107\003\000\000\
\\108\003\000\000\
\\109\003\000\000\
\\110\003\000\000\
\\111\003\000\000\
\\112\003\000\000\
\\113\003\000\000\
\\114\003\000\000\
\\115\003\000\000\
\\116\003\000\000\
\\117\003\000\000\
\\118\003\000\000\
\\119\003\000\000\
\\120\003\058\000\082\001\000\000\
\\121\003\000\000\
\\122\003\000\000\
\\123\003\068\000\074\000\000\000\
\\124\003\000\000\
\\125\003\046\000\042\002\000\000\
\\126\003\000\000\
\\127\003\000\000\
\\128\003\046\000\007\002\000\000\
\\129\003\000\000\
\\130\003\000\000\
\\131\003\000\000\
\\132\003\003\000\012\002\000\000\
\\133\003\000\000\
\\134\003\000\000\
\\135\003\003\000\225\001\000\000\
\\135\003\003\000\225\001\046\000\227\001\000\000\
\\136\003\000\000\
\\137\003\000\000\
\\138\003\003\000\045\002\000\000\
\\139\003\000\000\
\\140\003\000\000\
\\141\003\000\000\
\\142\003\000\000\
\\143\003\003\000\017\002\000\000\
\\144\003\000\000\
\\145\003\000\000\
\\146\003\045\000\050\002\000\000\
\\147\003\000\000\
\\148\003\000\000\
\\149\003\003\000\229\001\000\000\
\\150\003\000\000\
\\151\003\000\000\
\\153\003\000\000\
\\154\003\000\000\
\\155\003\000\000\
\\156\003\000\000\
\\157\003\000\000\
\\158\003\000\000\
\\159\003\000\000\
\\160\003\000\000\
\\161\003\000\000\
\\162\003\000\000\
\\163\003\000\000\
\\165\003\000\000\
\\166\003\000\000\
\\167\003\000\000\
\\168\003\000\000\
\\169\003\000\000\
\\170\003\000\000\
\\171\003\000\000\
\\172\003\000\000\
\\173\003\000\000\
\\175\003\000\000\
\\176\003\000\000\
\\177\003\000\000\
\\178\003\000\000\
\\179\003\002\000\049\000\008\000\047\000\011\000\046\000\013\000\044\000\
\\017\000\042\000\018\000\041\000\020\000\039\000\021\000\038\000\
\\024\000\036\000\029\000\034\000\030\000\033\000\051\000\027\000\
\\055\000\026\000\057\000\025\000\000\000\
\\180\003\000\000\
\\181\003\000\000\
\\182\003\000\000\
\\183\003\000\000\
\"
val actionRowNumbers =
"\127\000\013\000\207\001\201\001\
\\082\000\206\001\206\001\206\001\
\\096\001\021\000\204\000\003\000\
\\202\000\084\000\179\000\139\000\
\\138\000\140\000\137\000\136\000\
\\135\000\134\000\133\000\120\000\
\\122\000\123\000\116\000\201\000\
\\028\000\169\000\170\000\173\000\
\\073\001\170\000\121\000\168\000\
\\085\000\128\000\128\000\245\000\
\\245\000\170\000\175\000\178\000\
\\172\000\057\000\170\000\073\001\
\\001\000\204\001\205\001\203\001\
\\202\001\001\000\068\001\170\000\
\\178\000\170\000\203\000\159\000\
\\180\000\161\000\162\000\152\000\
\\151\000\150\000\160\000\097\001\
\\111\001\156\000\119\001\086\000\
\\157\000\187\001\058\000\158\000\
\\182\000\145\000\144\000\143\000\
\\147\000\146\000\073\000\200\000\
\\087\000\191\000\189\000\071\000\
\\020\000\060\000\061\000\183\000\
\\016\000\174\000\044\001\088\000\
\\020\001\042\000\224\000\119\000\
\\074\001\023\001\155\000\022\001\
\\043\001\044\000\177\000\174\000\
\\072\001\124\000\228\000\126\000\
\\209\000\234\000\243\000\166\000\
\\167\000\237\000\148\000\241\000\
\\149\000\092\001\219\000\085\000\
\\244\000\142\000\141\000\085\000\
\\019\000\178\000\039\001\019\001\
\\043\000\255\000\252\000\226\000\
\\118\000\112\000\216\000\213\000\
\\176\000\233\000\117\000\124\000\
\\239\000\239\000\089\000\154\000\
\\153\000\018\000\124\000\239\000\
\\208\001\210\001\209\001\125\000\
\\058\001\004\000\056\001\054\001\
\\205\000\060\001\067\001\068\001\
\\207\000\208\000\206\000\090\000\
\\113\000\113\000\113\000\132\001\
\\181\000\170\000\170\000\185\000\
\\170\000\170\000\188\000\186\000\
\\184\000\170\000\225\000\046\001\
\\068\001\170\000\045\001\051\001\
\\021\001\026\000\163\000\074\000\
\\042\001\039\001\091\000\024\000\
\\023\000\022\000\033\001\031\001\
\\029\001\072\000\062\000\063\000\
\\064\000\077\001\025\001\247\000\
\\092\000\242\000\240\000\093\001\
\\090\001\037\000\095\001\220\000\
\\218\000\038\000\223\000\128\000\
\\236\000\235\000\170\000\227\000\
\\093\000\068\001\050\001\253\000\
\\178\000\250\000\178\000\170\000\
\\214\000\178\000\014\001\094\000\
\\230\000\073\001\229\000\171\000\
\\178\000\095\000\056\000\059\001\
\\164\000\165\000\068\001\068\001\
\\075\000\066\001\076\000\065\000\
\\066\000\041\000\114\001\096\000\
\\117\001\131\001\110\001\120\001\
\\008\000\114\000\133\001\130\001\
\\067\000\077\000\120\000\045\000\
\\113\000\073\001\135\001\117\000\
\\073\001\117\000\057\000\199\000\
\\190\000\196\000\195\000\192\000\
\\211\000\053\001\249\000\047\001\
\\027\000\068\001\048\001\024\001\
\\041\001\178\000\178\000\027\001\
\\075\001\178\000\028\001\026\001\
\\126\000\068\001\094\001\131\000\
\\222\000\170\000\219\000\017\000\
\\170\000\038\001\254\000\251\000\
\\217\000\215\000\018\001\172\000\
\\068\001\172\000\238\000\008\001\
\\117\000\125\000\212\000\172\000\
\\219\000\057\001\055\001\061\001\
\\068\001\069\001\068\001\062\001\
\\099\001\005\000\078\000\086\001\
\\059\000\002\000\128\000\128\000\
\\046\000\041\000\030\000\122\001\
\\122\000\047\000\134\001\049\000\
\\112\001\113\000\148\001\079\000\
\\138\001\097\000\125\000\150\001\
\\149\001\118\001\142\001\124\000\
\\140\001\080\000\143\001\141\001\
\\124\000\147\001\013\001\145\001\
\\144\001\124\000\098\000\197\000\
\\116\000\052\001\246\000\174\000\
\\178\000\025\000\037\001\178\000\
\\037\001\030\001\032\001\076\001\
\\003\001\092\001\194\000\129\000\
\\039\000\170\000\000\001\015\001\
\\172\000\084\000\012\001\008\001\
\\005\001\073\001\013\001\231\000\
\\008\001\031\000\065\001\070\001\
\\071\001\102\001\120\000\113\000\
\\113\000\091\001\092\001\092\001\
\\115\001\073\001\100\001\116\001\
\\121\001\123\001\073\001\139\001\
\\125\000\099\000\113\000\068\000\
\\113\000\121\000\136\001\100\000\
\\151\001\152\001\164\001\068\001\
\\165\001\179\001\102\000\029\000\
\\198\000\248\000\049\001\035\001\
\\083\000\040\001\034\001\001\001\
\\073\001\132\000\170\000\032\000\
\\131\000\210\000\017\001\018\001\
\\006\001\007\001\011\001\004\001\
\\232\000\063\001\116\000\101\001\
\\006\000\083\001\104\001\080\001\
\\103\001\069\000\078\001\070\000\
\\129\000\040\000\125\000\125\000\
\\137\001\041\000\103\000\113\001\
\\180\001\014\000\115\000\158\001\
\\157\001\125\000\162\001\073\001\
\\161\001\068\001\177\001\117\000\
\\117\000\173\001\013\001\125\000\
\\036\001\002\001\033\000\193\000\
\\187\000\219\000\016\001\009\001\
\\172\000\064\001\105\001\050\000\
\\088\001\113\000\113\000\087\001\
\\034\000\041\000\104\000\105\000\
\\190\001\010\000\081\000\041\000\
\\106\000\113\000\182\001\120\000\
\\052\000\121\000\155\001\154\001\
\\163\001\159\001\117\000\168\001\
\\178\001\173\001\170\001\073\001\
\\176\001\146\001\098\001\132\000\
\\010\001\106\001\073\001\082\001\
\\079\001\085\001\130\000\068\001\
\\068\001\193\001\123\000\113\000\
\\113\000\191\001\041\000\107\000\
\\181\001\183\001\073\001\156\001\
\\125\000\160\001\166\001\073\001\
\\171\001\172\001\124\000\174\001\
\\117\000\035\000\125\000\036\000\
\\129\001\009\000\192\001\195\001\
\\011\000\084\001\194\001\081\001\
\\188\001\041\000\125\000\153\001\
\\167\001\124\000\108\000\175\001\
\\221\000\109\000\089\001\127\001\
\\046\000\124\001\125\001\048\000\
\\196\001\054\000\189\001\110\000\
\\101\000\117\000\068\001\128\001\
\\126\001\197\001\073\001\068\001\
\\173\001\007\000\125\000\015\000\
\\169\001\107\001\108\001\051\000\
\\111\000\184\001\185\001\053\000\
\\109\001\068\001\186\001\012\000\
\\198\001\199\001\055\000\200\001\
\\000\000"
val gotoT =
"\
\\142\000\108\002\145\000\001\000\000\000\
\\001\000\014\000\015\000\013\000\016\000\012\000\025\000\011\000\
\\026\000\010\000\027\000\009\000\033\000\008\000\087\000\007\000\
\\097\000\006\000\132\000\005\000\139\000\004\000\140\000\003\000\
\\143\000\002\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\033\000\008\000\087\000\007\000\097\000\006\000\132\000\005\000\
\\140\000\050\000\141\000\049\000\000\000\
\\033\000\008\000\087\000\007\000\097\000\006\000\132\000\005\000\
\\140\000\050\000\141\000\051\000\000\000\
\\033\000\008\000\087\000\007\000\097\000\006\000\132\000\005\000\
\\140\000\050\000\141\000\052\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\014\000\015\000\013\000\016\000\058\000\000\000\
\\000\000\
\\005\000\061\000\011\000\060\000\012\000\059\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\068\000\088\000\067\000\000\000\
\\009\000\071\000\098\000\070\000\000\000\
\\010\000\074\000\133\000\073\000\000\000\
\\003\000\076\000\000\000\
\\003\000\084\000\022\000\083\000\023\000\082\000\000\000\
\\001\000\014\000\015\000\013\000\016\000\012\000\017\000\087\000\
\\018\000\086\000\025\000\011\000\026\000\010\000\027\000\085\000\000\000\
\\001\000\014\000\015\000\013\000\016\000\012\000\019\000\090\000\
\\021\000\089\000\025\000\011\000\026\000\010\000\027\000\088\000\000\000\
\\001\000\014\000\015\000\013\000\016\000\012\000\025\000\011\000\
\\026\000\010\000\027\000\092\000\000\000\
\\001\000\101\000\007\000\100\000\015\000\099\000\038\000\098\000\
\\056\000\097\000\057\000\096\000\066\000\095\000\068\000\094\000\
\\081\000\093\000\000\000\
\\007\000\100\000\045\000\110\000\080\000\109\000\081\000\108\000\000\000\
\\001\000\014\000\015\000\013\000\016\000\012\000\025\000\011\000\
\\026\000\010\000\027\000\112\000\000\000\
\\008\000\115\000\014\000\114\000\036\000\113\000\000\000\
\\000\000\
\\004\000\119\000\005\000\118\000\035\000\117\000\000\000\
\\146\000\121\000\000\000\
\\146\000\122\000\000\000\
\\002\000\124\000\037\000\123\000\000\000\
\\002\000\124\000\037\000\127\000\000\000\
\\001\000\014\000\015\000\013\000\016\000\012\000\025\000\011\000\
\\026\000\010\000\027\000\128\000\000\000\
\\001\000\101\000\007\000\100\000\015\000\136\000\040\000\135\000\
\\042\000\134\000\044\000\133\000\056\000\132\000\057\000\131\000\
\\067\000\130\000\081\000\129\000\000\000\
\\001\000\101\000\015\000\099\000\028\000\139\000\030\000\138\000\
\\056\000\097\000\057\000\096\000\066\000\137\000\068\000\094\000\000\000\
\\015\000\142\000\054\000\141\000\000\000\
\\006\000\146\000\007\000\100\000\048\000\145\000\049\000\144\000\
\\081\000\143\000\000\000\
\\001\000\014\000\015\000\013\000\016\000\012\000\025\000\011\000\
\\026\000\010\000\027\000\149\000\000\000\
\\007\000\100\000\047\000\151\000\080\000\150\000\081\000\108\000\000\000\
\\001\000\014\000\015\000\013\000\016\000\012\000\025\000\011\000\
\\026\000\010\000\027\000\009\000\033\000\008\000\087\000\007\000\
\\097\000\006\000\132\000\005\000\139\000\004\000\140\000\003\000\
\\143\000\153\000\144\000\152\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\014\000\015\000\013\000\016\000\012\000\025\000\011\000\
\\026\000\010\000\027\000\009\000\033\000\008\000\087\000\007\000\
\\097\000\006\000\132\000\005\000\139\000\004\000\140\000\003\000\
\\143\000\153\000\144\000\154\000\000\000\
\\007\000\161\000\070\000\160\000\071\000\159\000\072\000\158\000\
\\073\000\157\000\074\000\156\000\078\000\155\000\000\000\
\\001\000\014\000\015\000\013\000\016\000\012\000\025\000\011\000\
\\026\000\010\000\027\000\164\000\000\000\
\\001\000\101\000\015\000\099\000\028\000\165\000\030\000\138\000\
\\056\000\097\000\057\000\096\000\066\000\137\000\068\000\094\000\000\000\
\\001\000\014\000\015\000\013\000\016\000\012\000\025\000\011\000\
\\026\000\010\000\027\000\166\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\094\000\167\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\101\000\015\000\099\000\038\000\182\000\056\000\097\000\
\\057\000\096\000\066\000\095\000\068\000\094\000\000\000\
\\069\000\183\000\000\000\
\\000\000\
\\069\000\186\000\000\000\
\\001\000\101\000\015\000\136\000\056\000\132\000\057\000\131\000\
\\067\000\187\000\000\000\
\\000\000\
\\005\000\189\000\012\000\188\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\194\000\005\000\193\000\061\000\192\000\062\000\191\000\000\000\
\\001\000\101\000\015\000\099\000\056\000\097\000\057\000\096\000\
\\058\000\201\000\059\000\200\000\066\000\199\000\068\000\094\000\000\000\
\\001\000\101\000\007\000\205\000\015\000\099\000\056\000\097\000\
\\057\000\096\000\060\000\204\000\066\000\203\000\068\000\094\000\
\\082\000\202\000\000\000\
\\001\000\101\000\015\000\099\000\038\000\207\000\056\000\097\000\
\\057\000\096\000\066\000\095\000\068\000\094\000\000\000\
\\000\000\
\\006\000\208\000\000\000\
\\000\000\
\\007\000\205\000\082\000\202\000\000\000\
\\000\000\
\\000\000\
\\008\000\115\000\014\000\114\000\036\000\209\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\119\000\005\000\118\000\035\000\210\000\000\000\
\\000\000\
\\033\000\008\000\085\000\213\000\086\000\212\000\087\000\211\000\000\000\
\\031\000\217\000\032\000\216\000\033\000\215\000\000\000\
\\004\000\119\000\005\000\118\000\035\000\220\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\119\000\005\000\118\000\035\000\221\000\000\000\
\\000\000\
\\001\000\101\000\015\000\136\000\040\000\223\000\042\000\134\000\
\\044\000\133\000\056\000\132\000\057\000\131\000\067\000\130\000\000\000\
\\064\000\224\000\000\000\
\\000\000\
\\001\000\101\000\015\000\136\000\056\000\132\000\057\000\131\000\
\\067\000\226\000\000\000\
\\043\000\227\000\000\000\
\\041\000\229\000\000\000\
\\000\000\
\\005\000\061\000\012\000\188\000\000\000\
\\000\000\
\\029\000\232\000\000\000\
\\000\000\
\\001\000\101\000\015\000\099\000\056\000\097\000\057\000\096\000\
\\060\000\204\000\066\000\203\000\068\000\094\000\000\000\
\\000\000\
\\005\000\234\000\000\000\
\\006\000\235\000\000\000\
\\034\000\236\000\000\000\
\\034\000\238\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\241\000\000\000\
\\034\000\242\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\244\000\013\000\243\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\250\000\075\000\249\000\076\000\248\000\000\000\
\\007\000\161\000\070\000\252\000\071\000\159\000\072\000\158\000\
\\073\000\157\000\074\000\156\000\078\000\155\000\079\000\251\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\000\001\095\000\255\000\096\000\254\000\000\000\
\\009\000\000\001\095\000\002\001\096\000\254\000\000\000\
\\009\000\000\001\095\000\005\001\096\000\004\001\100\000\003\001\000\000\
\\008\000\009\001\105\000\008\001\106\000\007\001\107\000\006\001\000\000\
\\000\000\
\\001\000\014\000\015\000\013\000\016\000\012\000\025\000\011\000\
\\026\000\010\000\027\000\019\001\000\000\
\\001\000\014\000\015\000\013\000\016\000\012\000\018\000\020\001\
\\025\000\011\000\026\000\010\000\027\000\085\000\000\000\
\\000\000\
\\001\000\014\000\015\000\013\000\016\000\012\000\021\000\022\001\
\\025\000\011\000\026\000\010\000\027\000\021\001\000\000\
\\001\000\014\000\015\000\013\000\016\000\012\000\018\000\023\001\
\\025\000\011\000\026\000\010\000\027\000\085\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\014\000\015\000\013\000\016\000\012\000\025\000\011\000\
\\026\000\010\000\027\000\024\001\000\000\
\\000\000\
\\000\000\
\\007\000\161\000\070\000\025\001\071\000\159\000\072\000\158\000\
\\073\000\157\000\074\000\156\000\078\000\155\000\000\000\
\\001\000\014\000\015\000\013\000\016\000\012\000\025\000\011\000\
\\026\000\010\000\027\000\026\001\000\000\
\\000\000\
\\000\000\
\\000\000\
\\064\000\028\001\069\000\027\001\000\000\
\\069\000\030\001\000\000\
\\000\000\
\\000\000\
\\064\000\032\001\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\033\000\008\000\086\000\042\001\087\000\211\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\032\000\044\001\033\000\215\000\000\000\
\\000\000\
\\000\000\
\\146\000\046\001\000\000\
\\000\000\
\\000\000\
\\001\000\014\000\015\000\013\000\016\000\012\000\025\000\011\000\
\\026\000\010\000\027\000\047\001\000\000\
\\000\000\
\\000\000\
\\007\000\161\000\070\000\049\001\071\000\159\000\072\000\158\000\
\\073\000\157\000\074\000\156\000\078\000\155\000\000\000\
\\000\000\
\\000\000\
\\001\000\101\000\015\000\136\000\042\000\050\001\044\000\133\000\
\\056\000\132\000\057\000\131\000\067\000\130\000\000\000\
\\000\000\
\\001\000\101\000\015\000\136\000\040\000\051\001\042\000\134\000\
\\044\000\133\000\056\000\132\000\057\000\131\000\067\000\130\000\000\000\
\\001\000\014\000\015\000\013\000\016\000\012\000\025\000\011\000\
\\026\000\010\000\027\000\052\001\000\000\
\\000\000\
\\001\000\101\000\015\000\099\000\028\000\053\001\030\000\138\000\
\\056\000\097\000\057\000\096\000\066\000\137\000\068\000\094\000\000\000\
\\053\000\054\001\000\000\
\\000\000\
\\000\000\
\\007\000\100\000\045\000\058\001\080\000\109\000\081\000\108\000\000\000\
\\000\000\
\\015\000\060\001\051\000\059\001\000\000\
\\001\000\101\000\015\000\099\000\028\000\062\001\030\000\138\000\
\\056\000\097\000\057\000\096\000\066\000\137\000\068\000\094\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\161\000\072\000\065\001\073\000\157\000\074\000\156\000\
\\078\000\155\000\000\000\
\\007\000\161\000\070\000\066\001\071\000\159\000\072\000\158\000\
\\073\000\157\000\074\000\156\000\078\000\155\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\115\000\010\000\076\001\014\000\075\001\083\000\074\001\
\\084\000\073\001\090\000\072\001\000\000\
\\000\000\
\\000\000\
\\000\000\
\\105\000\082\001\106\000\007\001\107\000\006\001\000\000\
\\000\000\
\\000\000\
\\099\000\083\001\000\000\
\\000\000\
\\000\000\
\\107\000\086\001\000\000\
\\000\000\
\\000\000\
\\008\000\091\001\127\000\090\001\000\000\
\\008\000\115\000\014\000\093\001\112\000\092\001\000\000\
\\009\000\097\001\095\000\096\001\096\000\254\000\108\000\095\001\000\000\
\\007\000\100\000\080\000\099\001\081\000\108\000\115\000\098\001\000\000\
\\000\000\
\\005\000\101\001\113\000\100\001\000\000\
\\007\000\100\000\080\000\104\001\081\000\108\000\115\000\103\001\
\\117\000\102\001\000\000\
\\005\000\106\001\125\000\105\001\000\000\
\\006\000\110\001\007\000\100\000\081\000\109\001\120\000\108\001\
\\121\000\107\001\000\000\
\\024\000\111\001\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\069\000\113\001\000\000\
\\039\000\114\001\000\000\
\\000\000\
\\000\000\
\\007\000\161\000\070\000\117\001\071\000\159\000\072\000\158\000\
\\073\000\157\000\074\000\156\000\078\000\155\000\000\000\
\\000\000\
\\000\000\
\\065\000\118\001\000\000\
\\001\000\101\000\015\000\099\000\056\000\097\000\057\000\096\000\
\\066\000\120\001\068\000\094\000\000\000\
\\001\000\101\000\015\000\099\000\056\000\097\000\057\000\096\000\
\\059\000\121\001\066\000\199\000\068\000\094\000\000\000\
\\000\000\
\\000\000\
\\001\000\101\000\015\000\099\000\056\000\097\000\057\000\096\000\
\\059\000\122\001\066\000\199\000\068\000\094\000\000\000\
\\000\000\
\\000\000\
\\007\000\205\000\082\000\123\001\000\000\
\\007\000\161\000\070\000\124\001\071\000\159\000\072\000\158\000\
\\073\000\157\000\074\000\156\000\078\000\155\000\000\000\
\\033\000\008\000\086\000\042\001\087\000\211\000\000\000\
\\148\000\125\001\000\000\
\\032\000\044\001\033\000\215\000\000\000\
\\001\000\014\000\015\000\013\000\016\000\012\000\020\000\127\001\
\\025\000\011\000\026\000\010\000\027\000\126\001\000\000\
\\031\000\128\001\032\000\216\000\033\000\215\000\000\000\
\\000\000\
\\001\000\014\000\015\000\013\000\016\000\012\000\025\000\011\000\
\\026\000\010\000\027\000\130\001\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\055\000\131\001\000\000\
\\015\000\133\001\000\000\
\\007\000\161\000\070\000\134\001\071\000\159\000\072\000\158\000\
\\073\000\157\000\074\000\156\000\078\000\155\000\000\000\
\\015\000\060\001\051\000\135\001\000\000\
\\000\000\
\\050\000\136\001\000\000\
\\005\000\138\001\000\000\
\\006\000\244\000\013\000\139\001\000\000\
\\000\000\
\\015\000\060\001\051\000\140\001\000\000\
\\031\000\141\001\032\000\216\000\033\000\215\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\161\000\070\000\142\001\071\000\159\000\072\000\158\000\
\\073\000\157\000\074\000\156\000\078\000\155\000\000\000\
\\000\000\
\\007\000\161\000\070\000\144\001\071\000\159\000\072\000\158\000\
\\073\000\157\000\074\000\156\000\078\000\155\000\079\000\143\001\000\000\
\\000\000\
\\000\000\
\\089\000\145\001\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\146\000\150\001\000\000\
\\146\000\151\001\000\000\
\\103\000\152\001\000\000\
\\008\000\115\000\010\000\076\001\014\000\075\001\083\000\074\001\
\\084\000\073\001\090\000\154\001\000\000\
\\000\000\
\\000\000\
\\009\000\071\000\098\000\156\001\000\000\
\\101\000\157\001\103\000\152\001\000\000\
\\000\000\
\\008\000\115\000\014\000\093\001\112\000\159\001\000\000\
\\094\000\161\001\000\000\
\\009\000\000\001\095\000\163\001\096\000\254\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\244\000\013\000\167\001\110\000\166\001\000\000\
\\000\000\
\\000\000\
\\009\000\169\001\108\000\168\001\000\000\
\\000\000\
\\006\000\170\001\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\172\001\000\000\
\\000\000\
\\053\000\173\001\000\000\
\\000\000\
\\000\000\
\\006\000\174\001\000\000\
\\000\000\
\\000\000\
\\003\000\084\000\022\000\176\001\000\000\
\\000\000\
\\000\000\
\\001\000\101\000\015\000\099\000\038\000\177\001\056\000\097\000\
\\057\000\096\000\066\000\095\000\068\000\094\000\000\000\
\\001\000\101\000\015\000\099\000\056\000\097\000\057\000\096\000\
\\066\000\178\001\068\000\094\000\000\000\
\\069\000\113\001\000\000\
\\063\000\179\001\000\000\
\\001\000\101\000\015\000\099\000\056\000\097\000\057\000\096\000\
\\066\000\181\001\068\000\094\000\000\000\
\\063\000\182\001\000\000\
\\000\000\
\\000\000\
\\000\000\
\\046\000\183\001\000\000\
\\033\000\008\000\085\000\185\001\086\000\212\000\087\000\211\000\000\000\
\\000\000\
\\147\000\187\001\000\000\
\\000\000\
\\001\000\014\000\015\000\013\000\016\000\012\000\025\000\011\000\
\\026\000\010\000\027\000\189\001\000\000\
\\000\000\
\\000\000\
\\015\000\142\000\054\000\190\001\000\000\
\\005\000\061\000\011\000\191\001\012\000\059\000\000\000\
\\000\000\
\\050\000\192\001\000\000\
\\000\000\
\\007\000\100\000\047\000\193\001\080\000\150\000\081\000\108\000\000\000\
\\053\000\194\001\000\000\
\\000\000\
\\050\000\195\001\000\000\
\\000\000\
\\077\000\197\001\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\068\000\088\000\199\001\000\000\
\\009\000\000\001\091\000\202\001\095\000\201\001\096\000\200\001\000\000\
\\009\000\000\001\091\000\204\001\095\000\203\001\096\000\200\001\000\000\
\\008\000\115\000\010\000\076\001\014\000\075\001\033\000\008\000\
\\083\000\207\001\084\000\206\001\085\000\205\001\086\000\212\000\
\\087\000\211\000\000\000\
\\033\000\008\000\085\000\208\001\086\000\212\000\087\000\211\000\000\000\
\\033\000\008\000\085\000\209\001\086\000\212\000\087\000\211\000\000\000\
\\000\000\
\\007\000\100\000\080\000\210\001\081\000\108\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\100\000\080\000\211\001\081\000\108\000\000\000\
\\000\000\
\\006\000\244\000\013\000\167\001\110\000\212\001\000\000\
\\000\000\
\\009\000\000\001\095\000\214\001\096\000\254\000\000\000\
\\000\000\
\\009\000\000\001\095\000\218\001\096\000\217\001\129\000\216\001\000\000\
\\008\000\115\000\014\000\220\001\111\000\219\001\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\169\001\108\000\168\001\000\000\
\\116\000\222\001\000\000\
\\007\000\161\000\070\000\224\001\071\000\159\000\072\000\158\000\
\\073\000\157\000\074\000\156\000\078\000\155\000\000\000\
\\116\000\222\001\000\000\
\\126\000\226\001\000\000\
\\000\000\
\\005\000\230\001\123\000\229\001\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\194\000\005\000\193\000\061\000\232\001\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\100\000\045\000\233\001\080\000\109\000\081\000\108\000\000\000\
\\149\000\234\001\000\000\
\\001\000\014\000\015\000\013\000\016\000\012\000\020\000\235\001\
\\025\000\011\000\026\000\010\000\027\000\126\001\000\000\
\\000\000\
\\148\000\237\001\000\000\
\\000\000\
\\000\000\
\\055\000\238\001\000\000\
\\000\000\
\\000\000\
\\052\000\239\001\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\250\000\075\000\241\001\000\000\
\\000\000\
\\089\000\242\001\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\147\000\248\001\000\000\
\\000\000\
\\006\000\244\000\013\000\250\001\000\000\
\\006\000\244\000\013\000\251\001\000\000\
\\000\000\
\\008\000\115\000\010\000\076\001\014\000\075\001\083\000\254\001\
\\084\000\253\001\135\000\252\001\000\000\
\\000\000\
\\094\000\000\002\000\000\
\\000\000\
\\128\000\002\002\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\244\000\013\000\007\002\109\000\006\002\000\000\
\\000\000\
\\007\000\100\000\080\000\099\001\081\000\108\000\115\000\008\002\000\000\
\\114\000\009\002\000\000\
\\007\000\161\000\070\000\011\002\071\000\159\000\072\000\158\000\
\\073\000\157\000\074\000\156\000\078\000\155\000\000\000\
\\000\000\
\\005\000\106\001\125\000\012\002\000\000\
\\005\000\230\001\123\000\013\002\000\000\
\\122\000\014\002\000\000\
\\053\000\016\002\000\000\
\\006\000\244\000\013\000\017\002\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\031\000\019\002\032\000\216\000\033\000\215\000\000\000\
\\000\000\
\\000\000\
\\015\000\060\001\051\000\020\002\000\000\
\\000\000\
\\000\000\
\\092\000\021\002\103\000\152\001\000\000\
\\000\000\
\\009\000\000\001\095\000\023\002\096\000\254\000\000\000\
\\009\000\000\001\095\000\024\002\096\000\254\000\000\000\
\\000\000\
\\000\000\
\\008\000\115\000\010\000\076\001\014\000\075\001\083\000\026\002\
\\084\000\206\001\000\000\
\\000\000\
\\000\000\
\\000\000\
\\134\000\029\002\000\000\
\\000\000\
\\008\000\115\000\010\000\076\001\014\000\075\001\083\000\254\001\
\\084\000\253\001\135\000\033\002\000\000\
\\000\000\
\\009\000\000\001\095\000\035\002\096\000\254\000\000\000\
\\000\000\
\\008\000\091\001\127\000\036\002\000\000\
\\103\000\152\001\130\000\037\002\000\000\
\\008\000\115\000\014\000\220\001\111\000\039\002\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\101\001\113\000\041\002\000\000\
\\118\000\042\002\000\000\
\\000\000\
\\122\000\044\002\000\000\
\\000\000\
\\007\000\100\000\080\000\046\002\081\000\108\000\119\000\045\002\000\000\
\\124\000\047\002\000\000\
\\000\000\
\\000\000\
\\149\000\049\002\000\000\
\\000\000\
\\000\000\
\\007\000\100\000\080\000\050\002\081\000\108\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\147\000\051\002\000\000\
\\007\000\161\000\070\000\052\002\071\000\159\000\072\000\158\000\
\\073\000\157\000\074\000\156\000\078\000\155\000\000\000\
\\007\000\161\000\070\000\053\002\071\000\159\000\072\000\158\000\
\\073\000\157\000\074\000\156\000\078\000\155\000\000\000\
\\000\000\
\\010\000\074\000\133\000\054\002\000\000\
\\009\000\000\001\095\000\057\002\096\000\056\002\136\000\055\002\000\000\
\\009\000\000\001\095\000\059\002\096\000\056\002\136\000\058\002\000\000\
\\000\000\
\\008\000\115\000\010\000\076\001\014\000\075\001\083\000\254\001\
\\084\000\253\001\135\000\060\002\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\100\000\080\000\062\002\081\000\108\000\000\000\
\\000\000\
\\006\000\244\000\013\000\007\002\109\000\063\002\000\000\
\\000\000\
\\000\000\
\\007\000\100\000\080\000\065\002\081\000\108\000\117\000\064\002\000\000\
\\000\000\
\\000\000\
\\006\000\066\002\000\000\
\\000\000\
\\005\000\230\001\123\000\067\002\000\000\
\\000\000\
\\006\000\244\000\013\000\069\002\000\000\
\\000\000\
\\104\000\071\002\000\000\
\\099\000\074\002\102\000\073\002\104\000\071\002\000\000\
\\000\000\
\\000\000\
\\134\000\076\002\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\115\000\010\000\076\001\014\000\075\001\083\000\254\001\
\\084\000\253\001\135\000\078\002\000\000\
\\006\000\244\000\013\000\079\002\000\000\
\\000\000\
\\000\000\
\\006\000\080\002\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\103\000\083\002\000\000\
\\000\000\
\\000\000\
\\009\000\071\000\098\000\156\001\101\000\084\002\103\000\083\002\000\000\
\\000\000\
\\103\000\152\001\137\000\085\002\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\230\001\123\000\088\002\000\000\
\\007\000\161\000\070\000\089\002\071\000\159\000\072\000\158\000\
\\073\000\157\000\074\000\156\000\078\000\155\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\100\000\080\000\090\002\081\000\108\000\000\000\
\\007\000\161\000\070\000\091\002\071\000\159\000\072\000\158\000\
\\073\000\157\000\074\000\156\000\078\000\155\000\000\000\
\\122\000\092\002\000\000\
\\089\000\094\002\093\000\093\002\104\000\071\002\000\000\
\\006\000\244\000\013\000\096\002\000\000\
\\104\000\071\002\128\000\098\002\131\000\097\002\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\068\000\088\000\199\001\092\000\100\002\103\000\083\002\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\091\001\103\000\083\002\127\000\036\002\130\000\102\002\000\000\
\\000\000\
\\007\000\161\000\070\000\103\002\071\000\159\000\072\000\158\000\
\\073\000\157\000\074\000\156\000\078\000\155\000\000\000\
\\000\000\
\\104\000\071\002\134\000\105\002\138\000\104\002\000\000\
\\000\000\
\\000\000\
\\010\000\074\000\103\000\083\002\133\000\054\002\137\000\107\002\000\000\
\\000\000\
\\000\000\
\"
val numstates = 621
val numrules = 330
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = Infix.InfEnv
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | LONGID of unit ->  (string list*string)
 | ETYVAR of unit ->  (string) | TYVAR of unit ->  (string)
 | SYMBOL of unit ->  (string) | ALPHA of unit ->  (string)
 | CHAR of unit ->  (char) | STRING of unit ->  (string)
 | REAL of unit ->  (real) | WORD of unit ->  (word)
 | INT of unit ->  (int) | NUMERIC of unit ->  (int)
 | DIGIT of unit ->  (int) | popLocalInfix of unit ->  (unit)
 | pushLocalInfix of unit ->  (unit) | popInfix of unit ->  (unit)
 | pushInfix of unit ->  (unit) | initInfix of unit ->  (unit)
 | program_opt of unit ->  (Program option)
 | program' of unit ->  (Program)
 | program of unit ->  (Program*Infix.InfEnv)
 | topdec_opt of unit ->  (TopDec option)
 | topdec1 of unit ->  (TopDec) | topdec of unit ->  (TopDec)
 | AND_tyreadesc_opt__AND_funbind_opt of unit ->  (TyReaDesc option*FunBind option)
 | tyreadesc__AND_funbind_opt of unit ->  (TyReaDesc*FunBind option)
 | sigexp__AND_funbind_opt of unit ->  (SigExp*FunBind option)
 | strexp__AND_funbind_opt of unit ->  (StrExp*FunBind option)
 | AND_funbind_opt of unit ->  (FunBind option)
 | funbind of unit ->  (FunBind) | fundec of unit ->  (FunDec)
 | AND_tyreadesc_opt__AND_strdesc_opt of unit ->  (TyReaDesc option*StrDesc option)
 | tyreadesc__AND_strdesc_opt of unit ->  (TyReaDesc*StrDesc option)
 | sigexp__AND_strdesc_opt of unit ->  (SigExp*StrDesc option)
 | AND_strdesc_opt of unit ->  (StrDesc option)
 | strdesc of unit ->  (StrDesc)
 | AND_exdesc_opt of unit ->  (ExDesc option)
 | exdesc of unit ->  (ExDesc)
 | BAR_condesc_opt of unit ->  (ConDesc option)
 | condesc of unit ->  (ConDesc)
 | AND_datdesc_opt of unit ->  (DatDesc option)
 | datdesc1 of unit ->  (DatDesc) | datdesc0 of unit ->  (DatDesc)
 | datdesc of unit ->  (DatDesc)
 | AND_syndesc_opt of unit ->  (SynDesc option)
 | syndesc of unit ->  (SynDesc)
 | AND_typdesc_opt of unit ->  (TypDesc option)
 | typdesc of unit ->  (TypDesc)
 | AND_valdesc_opt of unit ->  (ValDesc option)
 | valdesc of unit ->  (ValDesc)
 | longstrid_EQUALS_list2 of unit ->  (longStrId list)
 | longstrid_EQUALS_list1 of unit ->  (longStrId list)
 | longtycon_EQUALS_list2 of unit ->  (longTyCon list)
 | longtycon_EQUALS_list1 of unit ->  (longTyCon list)
 | sigid_list2 of unit ->  (SigId list) | spec1' of unit ->  (Spec)
 | spec1 of unit ->  (Spec) | spec of unit ->  (Spec)
 | AND_tyreadesc_opt of unit ->  (TyReaDesc option)
 | tyreadesc of unit ->  (TyReaDesc)
 | AND_tyreadesc_opt__AND_sigbind_opt of unit ->  (TyReaDesc option*SigBind option)
 | tyreadesc__AND_sigbind_opt of unit ->  (TyReaDesc*SigBind option)
 | sigexp__AND_sigbind_opt of unit ->  (SigExp*SigBind option)
 | AND_sigbind_opt of unit ->  (SigBind option)
 | sigbind of unit ->  (SigBind) | sigdec of unit ->  (SigDec)
 | sigexp' of unit ->  (SigExp) | sigexp of unit ->  (SigExp)
 | COLON_sigexp_opt of unit ->  (SigExp option)
 | AND_tyreadesc_opt__AND_strbind_opt of unit ->  (TyReaDesc option*StrBind option)
 | tyreadesc__AND_strbind_opt of unit ->  (TyReaDesc*StrBind option)
 | sigexp__AND_strbind_opt of unit ->  (SigExp*StrBind option)
 | strexp__AND_strbind_opt of unit ->  (StrExp*StrBind option)
 | AND_strbind_opt of unit ->  (StrBind option)
 | strbind of unit ->  (StrBind) | strdec1' of unit ->  (StrDec)
 | strdec1 of unit ->  (StrDec) | strdec of unit ->  (StrDec)
 | strexp' of unit ->  (StrExp) | strexp of unit ->  (StrExp)
 | tyvar_COMMA_list1 of unit ->  (TyVar list)
 | tyvarseq1 of unit ->  (TyVarseq) | tyvarseq of unit ->  (TyVarseq)
 | ty_COMMA_list2 of unit ->  (Ty list) | tyseq of unit ->  (Tyseq)
 | COMMA_tyrow_opt of unit ->  (TyRow option)
 | tyrow_opt of unit ->  (TyRow option) | tyrow of unit ->  (TyRow)
 | atty of unit ->  (Ty) | consty of unit ->  (Ty)
 | ty_STAR_list of unit ->  (Ty list) | tupty of unit ->  (Ty)
 | ty of unit ->  (Ty) | COLON_ty_list1 of unit ->  (Ty list)
 | atpat_list2 of unit ->  (AtPat list)
 | atpat_list1 of unit ->  (AtPat list) | pat of unit ->  (Pat)
 | AS_pat_opt of unit ->  (Pat option)
 | COLON_ty_opt of unit ->  (Ty option)
 | COMMA_patrow_opt of unit ->  (PatRow option)
 | patrow_opt of unit ->  (PatRow option)
 | patrow of unit ->  (PatRow)
 | pat_COMMA_list2 of unit ->  (Pat list)
 | pat_COMMA_list1 of unit ->  (Pat list)
 | pat_COMMA_list0 of unit ->  (Pat list) | atpat' of unit ->  (AtPat)
 | atpat of unit ->  (AtPat)
 | AND_exbind_opt of unit ->  (ExBind option)
 | exbind of unit ->  (ExBind) | OF_ty_opt of unit ->  (Ty option)
 | BAR_conbind_opt of unit ->  (ConBind option)
 | conbind of unit ->  (ConBind)
 | AND_datbind_opt of unit ->  (DatBind option)
 | datbind1 of unit ->  (DatBind) | datbind0 of unit ->  (DatBind)
 | datbind of unit ->  (DatBind)
 | AND_typbind_opt of unit ->  (TypBind option)
 | typbind of unit ->  (TypBind) | fmrule of unit ->  (Fmrule)
 | BAR_fmatch_opt of unit ->  (Fmatch option)
 | fmatch of unit ->  (Fmatch)
 | AND_fvalbind_opt of unit ->  (FvalBind option)
 | fvalbind of unit ->  (FvalBind)
 | AND_valbind_opt of unit ->  (ValBind option)
 | valbind of unit ->  (ValBind) | d_opt of unit ->  (int)
 | longstrid_list1 of unit ->  (longStrId list)
 | vid_list1 of unit ->  (VId list)
 | WITHTYPE_typbind_opt of unit ->  (TypBind option)
 | dec1' of unit ->  (Dec) | dec1 of unit ->  (Dec)
 | dec of unit ->  (Dec) | mrule of unit ->  (Mrule)
 | BAR_match_opt of unit ->  (Match option)
 | match of unit ->  (Match) | exp of unit ->  (Exp)
 | infexp of unit ->  (InfExp) | appexp of unit ->  (AppExp)
 | COMMA_exprow_opt of unit ->  (ExpRow option)
 | exprow_opt of unit ->  (ExpRow option)
 | exprow of unit ->  (ExpRow)
 | exp_SEMICOLON_list2 of unit ->  (Exp list)
 | exp_SEMICOLON_list1 of unit ->  (Exp list)
 | exp_COMMA_list2 of unit ->  (Exp list)
 | exp_COMMA_list1 of unit ->  (Exp list)
 | exp_COMMA_list0 of unit ->  (Exp list) | atexp of unit ->  (AtExp)
 | OP_opt of unit ->  (Op) | longstrid of unit ->  (longStrId)
 | longtycon of unit ->  (longTyCon) | longvid' of unit ->  (longVId)
 | longvid of unit ->  (longVId) | funid of unit ->  (FunId)
 | sigid of unit ->  (SigId) | strid of unit ->  (StrId)
 | tyvar of unit ->  (TyVar) | tycon of unit ->  (TyCon)
 | vid' of unit ->  (VId) | vid of unit ->  (VId)
 | lab of unit ->  (Lab) | d of unit ->  (int)
 | scon of unit ->  (SCon)
end
type svalue = MlyValue.svalue
type result = Program*Infix.InfEnv
end
structure EC=
struct
open LrTable
val is_keyword =
fn (T 1) => true | (T 2) => true | (T 3) => true | (T 4) => true | (T 
5) => true | (T 6) => true | (T 7) => true | (T 8) => true | (T 9)
 => true | (T 10) => true | (T 11) => true | (T 12) => true | (T 13)
 => true | (T 14) => true | (T 15) => true | (T 16) => true | (T 17)
 => true | (T 18) => true | (T 19) => true | (T 20) => true | (T 21)
 => true | (T 22) => true | (T 23) => true | (T 24) => true | (T 25)
 => true | (T 26) => true | (T 27) => true | (T 28) => true | (T 29)
 => true | (T 30) => true | (T 31) => true | (T 32) => true | (T 49)
 => true | (T 50) => true | (T 51) => true | (T 52) => true | (T 53)
 => true | (T 54) => true | (T 55) => true | (T 56) => true | (T 57)
 => true | _ => false
val preferred_change = 
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ABSTYPE"
  | (T 2) => "AND"
  | (T 3) => "ANDALSO"
  | (T 4) => "AS"
  | (T 5) => "CASE"
  | (T 6) => "DO"
  | (T 7) => "DATATYPE"
  | (T 8) => "ELSE"
  | (T 9) => "END"
  | (T 10) => "EXCEPTION"
  | (T 11) => "FN"
  | (T 12) => "FUN"
  | (T 13) => "HANDLE"
  | (T 14) => "IF"
  | (T 15) => "IN"
  | (T 16) => "INFIX"
  | (T 17) => "INFIXR"
  | (T 18) => "LET"
  | (T 19) => "LOCAL"
  | (T 20) => "NONFIX"
  | (T 21) => "OF"
  | (T 22) => "OP"
  | (T 23) => "OPEN"
  | (T 24) => "ORELSE"
  | (T 25) => "RAISE"
  | (T 26) => "REC"
  | (T 27) => "THEN"
  | (T 28) => "TYPE"
  | (T 29) => "VAL"
  | (T 30) => "WITH"
  | (T 31) => "WITHTYPE"
  | (T 32) => "WHILE"
  | (T 33) => "LPAR"
  | (T 34) => "RPAR"
  | (T 35) => "LBRACK"
  | (T 36) => "RBRACK"
  | (T 37) => "LBRACE"
  | (T 38) => "RBRACE"
  | (T 39) => "COMMA"
  | (T 40) => "COLON"
  | (T 41) => "SEMICOLON"
  | (T 42) => "DOTS"
  | (T 43) => "UNDERBAR"
  | (T 44) => "BAR"
  | (T 45) => "EQUALS"
  | (T 46) => "DARROW"
  | (T 47) => "ARROW"
  | (T 48) => "HASH"
  | (T 49) => "EQTYPE"
  | (T 50) => "FUNCTOR"
  | (T 51) => "INCLUDE"
  | (T 52) => "SHARING"
  | (T 53) => "SIG"
  | (T 54) => "SIGNATURE"
  | (T 55) => "STRUCT"
  | (T 56) => "STRUCTURE"
  | (T 57) => "WHERE"
  | (T 58) => "COLONGREATER"
  | (T 59) => "ZERO"
  | (T 60) => "DIGIT"
  | (T 61) => "NUMERIC"
  | (T 62) => "INT"
  | (T 63) => "WORD"
  | (T 64) => "REAL"
  | (T 65) => "STRING"
  | (T 66) => "CHAR"
  | (T 67) => "ALPHA"
  | (T 68) => "SYMBOL"
  | (T 69) => "STAR"
  | (T 70) => "TYVAR"
  | (T 71) => "ETYVAR"
  | (T 72) => "LONGID"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms = (T 0) :: (T 1) :: (T 2) :: (T 3) :: (T 4) :: (T 5) :: (T 6
) :: (T 7) :: (T 8) :: (T 9) :: (T 10) :: (T 11) :: (T 12) :: (T 13)
 :: (T 14) :: (T 15) :: (T 16) :: (T 17) :: (T 18) :: (T 19) :: (T 20)
 :: (T 21) :: (T 22) :: (T 23) :: (T 24) :: (T 25) :: (T 26) :: (T 27)
 :: (T 28) :: (T 29) :: (T 30) :: (T 31) :: (T 32) :: (T 33) :: (T 34)
 :: (T 35) :: (T 36) :: (T 37) :: (T 38) :: (T 39) :: (T 40) :: (T 41)
 :: (T 42) :: (T 43) :: (T 44) :: (T 45) :: (T 46) :: (T 47) :: (T 48)
 :: (T 49) :: (T 50) :: (T 51) :: (T 52) :: (T 53) :: (T 54) :: (T 55)
 :: (T 56) :: (T 57) :: (T 58) :: (T 59) :: (T 69) :: nil
end
structure Actions =
struct 
type int = Int.int
exception mlyAction of int
local open Header in
val actions = 
fn (i392:int,defaultPos,stack,
    (J0):arg) =>
case (i392,stack)
of (0,rest671) => let val result=MlyValue.initInfix(fn _ => (
 initJandJ'(J0) ))
 in (LrTable.NT 144,(result,defaultPos,defaultPos),rest671) end
| (1,rest671) => let val result=MlyValue.pushInfix(fn _ => ( pushJ() )
)
 in (LrTable.NT 145,(result,defaultPos,defaultPos),rest671) end
| (2,rest671) => let val result=MlyValue.popInfix(fn _ => ( popJ() ))
 in (LrTable.NT 146,(result,defaultPos,defaultPos),rest671) end
| (3,rest671) => let val result=MlyValue.pushLocalInfix(fn _ => (
 pushJ'shiftJ() ))
 in (LrTable.NT 147,(result,defaultPos,defaultPos),rest671) end
| (4,rest671) => let val result=MlyValue.popLocalInfix(fn _ => (
 popJandJ'() ))
 in (LrTable.NT 148,(result,defaultPos,defaultPos),rest671) end
| (5,(_,(_,ZERO1left,ZERO1right))::rest671) => let val result=
MlyValue.scon(fn _ => ( SCon.fromInt 0 ))
 in (LrTable.NT 0,(result,ZERO1left,ZERO1right),rest671) end
| (6,(_,(MlyValue.DIGIT DIGIT1,DIGIT1left,DIGIT1right))::rest671) => 
let val result=MlyValue.scon(fn _ => let val DIGIT as DIGIT1=DIGIT1 ()
 in ( SCon.fromInt DIGIT ) end
)
 in (LrTable.NT 0,(result,DIGIT1left,DIGIT1right),rest671) end
| (7,(_,(MlyValue.NUMERIC NUMERIC1,NUMERIC1left,NUMERIC1right))::
rest671) => let val result=MlyValue.scon(fn _ => let val NUMERIC as 
NUMERIC1=NUMERIC1 ()
 in ( SCon.fromInt NUMERIC ) end
)
 in (LrTable.NT 0,(result,NUMERIC1left,NUMERIC1right),rest671) end
| (8,(_,(MlyValue.INT INT1,INT1left,INT1right))::rest671) => let val 
result=MlyValue.scon(fn _ => let val INT as INT1=INT1 ()
 in ( SCon.fromInt INT ) end
)
 in (LrTable.NT 0,(result,INT1left,INT1right),rest671) end
| (9,(_,(MlyValue.WORD WORD1,WORD1left,WORD1right))::rest671) => let 
val result=MlyValue.scon(fn _ => let val WORD as WORD1=WORD1 ()
 in ( SCon.fromWord WORD ) end
)
 in (LrTable.NT 0,(result,WORD1left,WORD1right),rest671) end
| (10,(_,(MlyValue.STRING STRING1,STRING1left,STRING1right))::rest671)
 => let val result=MlyValue.scon(fn _ => let val STRING as STRING1=
STRING1 ()
 in ( SCon.fromString STRING ) end
)
 in (LrTable.NT 0,(result,STRING1left,STRING1right),rest671) end
| (11,(_,(MlyValue.CHAR CHAR1,CHAR1left,CHAR1right))::rest671) => let 
val result=MlyValue.scon(fn _ => let val CHAR as CHAR1=CHAR1 ()
 in ( SCon.fromChar CHAR ) end
)
 in (LrTable.NT 0,(result,CHAR1left,CHAR1right),rest671) end
| (12,(_,(MlyValue.REAL REAL1,REAL1left,REAL1right))::rest671) => let 
val result=MlyValue.scon(fn _ => let val REAL as REAL1=REAL1 ()
 in ( SCon.fromReal REAL ) end
)
 in (LrTable.NT 0,(result,REAL1left,REAL1right),rest671) end
| (13,(_,(_,ZERO1left,ZERO1right))::rest671) => let val result=
MlyValue.d(fn _ => ( 0 ))
 in (LrTable.NT 1,(result,ZERO1left,ZERO1right),rest671) end
| (14,(_,(MlyValue.DIGIT DIGIT1,DIGIT1left,DIGIT1right))::rest671) => 
let val result=MlyValue.d(fn _ => let val DIGIT as DIGIT1=DIGIT1 ()
 in ( DIGIT ) end
)
 in (LrTable.NT 1,(result,DIGIT1left,DIGIT1right),rest671) end
| (15,(_,(MlyValue.ALPHA ALPHA1,ALPHA1left,ALPHA1right))::rest671) => 
let val result=MlyValue.lab(fn _ => let val ALPHA as ALPHA1=ALPHA1 ()
 in ( Lab.fromString ALPHA ) end
)
 in (LrTable.NT 2,(result,ALPHA1left,ALPHA1right),rest671) end
| (16,(_,(MlyValue.SYMBOL SYMBOL1,SYMBOL1left,SYMBOL1right))::rest671)
 => let val result=MlyValue.lab(fn _ => let val SYMBOL as SYMBOL1=
SYMBOL1 ()
 in ( Lab.fromString SYMBOL ) end
)
 in (LrTable.NT 2,(result,SYMBOL1left,SYMBOL1right),rest671) end
| (17,(_,(_,STAR1left,STAR1right))::rest671) => let val result=
MlyValue.lab(fn _ => ( Lab.fromString "*" ))
 in (LrTable.NT 2,(result,STAR1left,STAR1right),rest671) end
| (18,(_,(MlyValue.DIGIT DIGIT1,DIGIT1left,DIGIT1right))::rest671) => 
let val result=MlyValue.lab(fn _ => let val DIGIT as DIGIT1=DIGIT1 ()
 in ( Lab.fromInt DIGIT ) end
)
 in (LrTable.NT 2,(result,DIGIT1left,DIGIT1right),rest671) end
| (19,(_,(MlyValue.NUMERIC NUMERIC1,NUMERIC1left,NUMERIC1right))::
rest671) => let val result=MlyValue.lab(fn _ => let val NUMERIC as 
NUMERIC1=NUMERIC1 ()
 in ( Lab.fromInt NUMERIC ) end
)
 in (LrTable.NT 2,(result,NUMERIC1left,NUMERIC1right),rest671) end
| (20,(_,(MlyValue.vid' vid'1,vid'1left,vid'1right))::rest671) => let 
val result=MlyValue.vid(fn _ => let val vid' as vid'1=vid'1 ()
 in ( vid' ) end
)
 in (LrTable.NT 3,(result,vid'1left,vid'1right),rest671) end
| (21,(_,(_,EQUALS1left,EQUALS1right))::rest671) => let val result=
MlyValue.vid(fn _ => ( VId.fromString "=" ))
 in (LrTable.NT 3,(result,EQUALS1left,EQUALS1right),rest671) end
| (22,(_,(MlyValue.ALPHA ALPHA1,ALPHA1left,ALPHA1right))::rest671) => 
let val result=MlyValue.vid'(fn _ => let val ALPHA as ALPHA1=ALPHA1 ()
 in ( VId.fromString ALPHA ) end
)
 in (LrTable.NT 4,(result,ALPHA1left,ALPHA1right),rest671) end
| (23,(_,(MlyValue.SYMBOL SYMBOL1,SYMBOL1left,SYMBOL1right))::rest671)
 => let val result=MlyValue.vid'(fn _ => let val SYMBOL as SYMBOL1=
SYMBOL1 ()
 in ( VId.fromString SYMBOL ) end
)
 in (LrTable.NT 4,(result,SYMBOL1left,SYMBOL1right),rest671) end
| (24,(_,(_,STAR1left,STAR1right))::rest671) => let val result=
MlyValue.vid'(fn _ => ( VId.fromString "*" ))
 in (LrTable.NT 4,(result,STAR1left,STAR1right),rest671) end
| (25,(_,(MlyValue.ALPHA ALPHA1,ALPHA1left,ALPHA1right))::rest671) => 
let val result=MlyValue.tycon(fn _ => let val ALPHA as ALPHA1=ALPHA1 
()
 in ( TyCon.fromString ALPHA ) end
)
 in (LrTable.NT 5,(result,ALPHA1left,ALPHA1right),rest671) end
| (26,(_,(MlyValue.SYMBOL SYMBOL1,SYMBOL1left,SYMBOL1right))::rest671)
 => let val result=MlyValue.tycon(fn _ => let val SYMBOL as SYMBOL1=
SYMBOL1 ()
 in ( TyCon.fromString SYMBOL ) end
)
 in (LrTable.NT 5,(result,SYMBOL1left,SYMBOL1right),rest671) end
| (27,(_,(MlyValue.TYVAR TYVAR1,TYVAR1left,TYVAR1right))::rest671) => 
let val result=MlyValue.tyvar(fn _ => let val TYVAR as TYVAR1=TYVAR1 
()
 in ( TyVar.fromString TYVAR ) end
)
 in (LrTable.NT 6,(result,TYVAR1left,TYVAR1right),rest671) end
| (28,(_,(MlyValue.ALPHA ALPHA1,ALPHA1left,ALPHA1right))::rest671) => 
let val result=MlyValue.strid(fn _ => let val ALPHA as ALPHA1=ALPHA1 
()
 in ( StrId.fromString ALPHA ) end
)
 in (LrTable.NT 7,(result,ALPHA1left,ALPHA1right),rest671) end
| (29,(_,(MlyValue.ALPHA ALPHA1,ALPHA1left,ALPHA1right))::rest671) => 
let val result=MlyValue.sigid(fn _ => let val ALPHA as ALPHA1=ALPHA1 
()
 in ( SigId.fromString ALPHA ) end
)
 in (LrTable.NT 8,(result,ALPHA1left,ALPHA1right),rest671) end
| (30,(_,(MlyValue.ALPHA ALPHA1,ALPHA1left,ALPHA1right))::rest671) => 
let val result=MlyValue.funid(fn _ => let val ALPHA as ALPHA1=ALPHA1 
()
 in ( FunId.fromString ALPHA ) end
)
 in (LrTable.NT 9,(result,ALPHA1left,ALPHA1right),rest671) end
| (31,(_,(MlyValue.longvid' longvid'1,longvid'1left,longvid'1right))::
rest671) => let val result=MlyValue.longvid(fn _ => let val longvid'
 as longvid'1=longvid'1 ()
 in ( longvid' ) end
)
 in (LrTable.NT 10,(result,longvid'1left,longvid'1right),rest671) end
| (32,(_,(_,EQUALS1left,EQUALS1right))::rest671) => let val result=
MlyValue.longvid(fn _ => ( LongVId.fromId(VId.fromString "=") ))
 in (LrTable.NT 10,(result,EQUALS1left,EQUALS1right),rest671) end
| (33,(_,(MlyValue.vid' vid'1,vid'1left,vid'1right))::rest671) => let 
val result=MlyValue.longvid'(fn _ => let val vid' as vid'1=vid'1 ()
 in ( LongVId.fromId vid' ) end
)
 in (LrTable.NT 11,(result,vid'1left,vid'1right),rest671) end
| (34,(_,(MlyValue.LONGID LONGID1,LONGID1left,LONGID1right))::rest671)
 => let val result=MlyValue.longvid'(fn _ => let val LONGID as LONGID1
=LONGID1 ()
 in ( LongVId.implode(toLongId VId.fromString LONGID) ) end
)
 in (LrTable.NT 11,(result,LONGID1left,LONGID1right),rest671) end
| (35,(_,(MlyValue.tycon tycon1,tycon1left,tycon1right))::rest671) => 
let val result=MlyValue.longtycon(fn _ => let val tycon as tycon1=
tycon1 ()
 in ( LongTyCon.fromId tycon ) end
)
 in (LrTable.NT 12,(result,tycon1left,tycon1right),rest671) end
| (36,(_,(MlyValue.LONGID LONGID1,LONGID1left,LONGID1right))::rest671)
 => let val result=MlyValue.longtycon(fn _ => let val LONGID as 
LONGID1=LONGID1 ()
 in ( LongTyCon.implode(toLongId TyCon.fromString LONGID) ) end
)
 in (LrTable.NT 12,(result,LONGID1left,LONGID1right),rest671) end
| (37,(_,(MlyValue.strid strid1,strid1left,strid1right))::rest671) => 
let val result=MlyValue.longstrid(fn _ => let val strid as strid1=
strid1 ()
 in ( LongStrId.fromId strid ) end
)
 in (LrTable.NT 13,(result,strid1left,strid1right),rest671) end
| (38,(_,(MlyValue.LONGID LONGID1,LONGID1left,LONGID1right))::rest671)
 => let val result=MlyValue.longstrid(fn _ => let val LONGID as 
LONGID1=LONGID1 ()
 in ( LongStrId.implode(toLongId StrId.fromString LONGID) ) end
)
 in (LrTable.NT 13,(result,LONGID1left,LONGID1right),rest671) end
| (39,(_,(_,OP1left,OP1right))::rest671) => let val result=
MlyValue.OP_opt(fn _ => ( WITHOp ))
 in (LrTable.NT 14,(result,OP1left,OP1right),rest671) end
| (40,rest671) => let val result=MlyValue.OP_opt(fn _ => ( SANSOp ))
 in (LrTable.NT 14,(result,defaultPos,defaultPos),rest671) end
| (41,(_,(MlyValue.scon scon1,sconleft as scon1left,sconright as 
scon1right))::rest671) => let val result=MlyValue.atexp(fn _ => let 
val scon as scon1=scon1 ()
 in ( SCONAtExp(I(sconleft,sconright), scon) ) end
)
 in (LrTable.NT 15,(result,scon1left,scon1right),rest671) end
| (42,(_,(MlyValue.longvid longvid1,_,longvidright as longvid1right))
::(_,(MlyValue.OP_opt OP_opt1,OP_optleft as OP_opt1left,_))::rest671)
 => let val result=MlyValue.atexp(fn _ => let val OP_opt as OP_opt1=
OP_opt1 ()
val longvid as longvid1=longvid1 ()
 in (
 LONGVIDAtExp(I(OP_optleft,longvidright),
                                       OP_opt, longvid) 
) end
)
 in (LrTable.NT 15,(result,OP_opt1left,longvid1right),rest671) end
| (43,(_,(_,_,RBRACEright as RBRACE1right))::(_,(MlyValue.exprow_opt 
exprow_opt1,_,_))::(_,(_,LBRACEleft as LBRACE1left,_))::rest671) => 
let val result=MlyValue.atexp(fn _ => let val exprow_opt as 
exprow_opt1=exprow_opt1 ()
 in ( RECORDAtExp(I(LBRACEleft,RBRACEright), exprow_opt) ) end
)
 in (LrTable.NT 15,(result,LBRACE1left,RBRACE1right),rest671) end
| (44,(_,(MlyValue.lab lab1,_,labright as lab1right))::(_,(_,HASHleft
 as HASH1left,_))::rest671) => let val result=MlyValue.atexp(fn _ => 
let val lab as lab1=lab1 ()
 in ( HASHAtExp(I(HASHleft,labright), lab) ) end
)
 in (LrTable.NT 15,(result,HASH1left,lab1right),rest671) end
| (45,(_,(_,_,RPARright as RPAR1right))::(_,(_,LPARleft as LPAR1left,_
))::rest671) => let val result=MlyValue.atexp(fn _ => (
 UNITAtExp(I(LPARleft,RPARright)) ))
 in (LrTable.NT 15,(result,LPAR1left,RPAR1right),rest671) end
| (46,(_,(_,_,RPARright as RPAR1right))::(_,(MlyValue.exp_COMMA_list2 
exp_COMMA_list21,_,_))::(_,(_,LPARleft as LPAR1left,_))::rest671) => 
let val result=MlyValue.atexp(fn _ => let val exp_COMMA_list2 as 
exp_COMMA_list21=exp_COMMA_list21 ()
 in ( TUPLEAtExp(I(LPARleft,RPARright), exp_COMMA_list2) ) end
)
 in (LrTable.NT 15,(result,LPAR1left,RPAR1right),rest671) end
| (47,(_,(_,_,RBRACKright as RBRACK1right))::(_,(
MlyValue.exp_COMMA_list0 exp_COMMA_list01,_,_))::(_,(_,LBRACKleft as 
LBRACK1left,_))::rest671) => let val result=MlyValue.atexp(fn _ => 
let val exp_COMMA_list0 as exp_COMMA_list01=exp_COMMA_list01 ()
 in ( LISTAtExp(I(LBRACKleft,RBRACKright),
                                    exp_COMMA_list0 ))
 end
)
 in (LrTable.NT 15,(result,LBRACK1left,RBRACK1right),rest671) end
| (48,(_,(_,_,RPARright as RPAR1right))::(_,(
MlyValue.exp_SEMICOLON_list2 exp_SEMICOLON_list21,_,_))::(_,(_,
LPARleft as LPAR1left,_))::rest671) => let val result=MlyValue.atexp(
fn _ => let val exp_SEMICOLON_list2 as exp_SEMICOLON_list21=
exp_SEMICOLON_list21 ()
 in ( SEQAtExp(I(LPARleft,RPARright), exp_SEMICOLON_list2) ) end
)
 in (LrTable.NT 15,(result,LPAR1left,RPAR1right),rest671) end
| (49,(_,(_,_,ENDright as END1right))::(_,(MlyValue.popInfix popInfix1
,_,_))::(_,(MlyValue.exp_SEMICOLON_list1 exp_SEMICOLON_list11,_,_))::_
::(_,(MlyValue.dec dec1,_,_))::(_,(MlyValue.pushInfix pushInfix1,_,_))
::(_,(_,LETleft as LET1left,_))::rest671) => let val result=
MlyValue.atexp(fn _ => let val pushInfix1=pushInfix1 ()
val dec as dec1=dec1 ()
val exp_SEMICOLON_list1 as exp_SEMICOLON_list11=exp_SEMICOLON_list11 
()
val popInfix1=popInfix1 ()
 in ( LETAtExp(I(LETleft,ENDright),
                                   dec, exp_SEMICOLON_list1) )
 end
)
 in (LrTable.NT 15,(result,LET1left,END1right),rest671) end
| (50,(_,(_,_,RPARright as RPAR1right))::(_,(MlyValue.exp exp1,_,_))::
(_,(_,LPARleft as LPAR1left,_))::rest671) => let val result=
MlyValue.atexp(fn _ => let val exp as exp1=exp1 ()
 in ( PARAtExp(I(LPARleft,RPARright), exp) ) end
)
 in (LrTable.NT 15,(result,LPAR1left,RPAR1right),rest671) end
| (51,(_,(MlyValue.exp_COMMA_list1 exp_COMMA_list11,
exp_COMMA_list11left,exp_COMMA_list11right))::rest671) => let val 
result=MlyValue.exp_COMMA_list0(fn _ => let val exp_COMMA_list1 as 
exp_COMMA_list11=exp_COMMA_list11 ()
 in ( exp_COMMA_list1 ) end
)
 in (LrTable.NT 16,(result,exp_COMMA_list11left,exp_COMMA_list11right)
,rest671) end
| (52,rest671) => let val result=MlyValue.exp_COMMA_list0(fn _ => (
 [] ))
 in (LrTable.NT 16,(result,defaultPos,defaultPos),rest671) end
| (53,(_,(MlyValue.exp_COMMA_list1 exp_COMMA_list11,_,
exp_COMMA_list11right))::_::(_,(MlyValue.exp exp1,exp1left,_))::
rest671) => let val result=MlyValue.exp_COMMA_list1(fn _ => let val 
exp as exp1=exp1 ()
val exp_COMMA_list1 as exp_COMMA_list11=exp_COMMA_list11 ()
 in ( exp::exp_COMMA_list1 ) end
)
 in (LrTable.NT 17,(result,exp1left,exp_COMMA_list11right),rest671)
 end
| (54,(_,(MlyValue.exp exp1,exp1left,exp1right))::rest671) => let val 
result=MlyValue.exp_COMMA_list1(fn _ => let val exp as exp1=exp1 ()
 in ( exp::[] ) end
)
 in (LrTable.NT 17,(result,exp1left,exp1right),rest671) end
| (55,(_,(MlyValue.exp_COMMA_list1 exp_COMMA_list11,_,
exp_COMMA_list11right))::_::(_,(MlyValue.exp exp1,exp1left,_))::
rest671) => let val result=MlyValue.exp_COMMA_list2(fn _ => let val 
exp as exp1=exp1 ()
val exp_COMMA_list1 as exp_COMMA_list11=exp_COMMA_list11 ()
 in ( exp::exp_COMMA_list1 ) end
)
 in (LrTable.NT 18,(result,exp1left,exp_COMMA_list11right),rest671)
 end
| (56,(_,(MlyValue.exp_SEMICOLON_list1 exp_SEMICOLON_list11,_,
exp_SEMICOLON_list11right))::_::(_,(MlyValue.exp exp1,exp1left,_))::
rest671) => let val result=MlyValue.exp_SEMICOLON_list1(fn _ => let 
val exp as exp1=exp1 ()
val exp_SEMICOLON_list1 as exp_SEMICOLON_list11=exp_SEMICOLON_list11 
()
 in ( exp::exp_SEMICOLON_list1 ) end
)
 in (LrTable.NT 19,(result,exp1left,exp_SEMICOLON_list11right),rest671
) end
| (57,(_,(MlyValue.exp exp1,exp1left,exp1right))::rest671) => let val 
result=MlyValue.exp_SEMICOLON_list1(fn _ => let val exp as exp1=exp1 
()
 in ( exp::[] ) end
)
 in (LrTable.NT 19,(result,exp1left,exp1right),rest671) end
| (58,(_,(MlyValue.exp_SEMICOLON_list2 exp_SEMICOLON_list21,_,
exp_SEMICOLON_list21right))::_::(_,(MlyValue.exp exp1,exp1left,_))::
rest671) => let val result=MlyValue.exp_SEMICOLON_list2(fn _ => let 
val exp as exp1=exp1 ()
val exp_SEMICOLON_list2 as exp_SEMICOLON_list21=exp_SEMICOLON_list21 
()
 in ( exp::exp_SEMICOLON_list2 ) end
)
 in (LrTable.NT 20,(result,exp1left,exp_SEMICOLON_list21right),rest671
) end
| (59,(_,(MlyValue.exp exp2,_,exp2right))::_::(_,(MlyValue.exp exp1,
exp1left,_))::rest671) => let val result=MlyValue.exp_SEMICOLON_list2(
fn _ => let val exp1=exp1 ()
val exp2=exp2 ()
 in ( [exp1, exp2] ) end
)
 in (LrTable.NT 20,(result,exp1left,exp2right),rest671) end
| (60,(_,(MlyValue.COMMA_exprow_opt COMMA_exprow_opt1,_,
COMMA_exprow_optright as COMMA_exprow_opt1right))::(_,(MlyValue.exp 
exp1,_,_))::_::(_,(MlyValue.lab lab1,lableft as lab1left,_))::rest671)
 => let val result=MlyValue.exprow(fn _ => let val lab as lab1=lab1 ()
val exp as exp1=exp1 ()
val COMMA_exprow_opt as COMMA_exprow_opt1=COMMA_exprow_opt1 ()
 in (
 ExpRow(I(lableft,COMMA_exprow_optright),
                                         lab, exp, COMMA_exprow_opt) 
) end
)
 in (LrTable.NT 21,(result,lab1left,COMMA_exprow_opt1right),rest671)
 end
| (61,(_,(MlyValue.exprow exprow1,_,exprow1right))::(_,(_,COMMA1left,_
))::rest671) => let val result=MlyValue.COMMA_exprow_opt(fn _ => let 
val exprow as exprow1=exprow1 ()
 in ( SOME exprow ) end
)
 in (LrTable.NT 23,(result,COMMA1left,exprow1right),rest671) end
| (62,rest671) => let val result=MlyValue.COMMA_exprow_opt(fn _ => (
 NONE ))
 in (LrTable.NT 23,(result,defaultPos,defaultPos),rest671) end
| (63,(_,(MlyValue.exprow exprow1,exprow1left,exprow1right))::rest671)
 => let val result=MlyValue.exprow_opt(fn _ => let val exprow as 
exprow1=exprow1 ()
 in ( SOME exprow ) end
)
 in (LrTable.NT 22,(result,exprow1left,exprow1right),rest671) end
| (64,rest671) => let val result=MlyValue.exprow_opt(fn _ => ( NONE ))
 in (LrTable.NT 22,(result,defaultPos,defaultPos),rest671) end
| (65,(_,(MlyValue.atexp atexp1,atexp1left,atexp1right))::rest671) => 
let val result=MlyValue.appexp(fn _ => let val atexp as atexp1=atexp1 
()
 in ( atexp::[] ) end
)
 in (LrTable.NT 24,(result,atexp1left,atexp1right),rest671) end
| (66,(_,(MlyValue.atexp atexp1,_,atexp1right))::(_,(MlyValue.appexp 
appexp1,appexp1left,_))::rest671) => let val result=MlyValue.appexp(
fn _ => let val appexp as appexp1=appexp1 ()
val atexp as atexp1=atexp1 ()
 in ( atexp::appexp ) end
)
 in (LrTable.NT 24,(result,appexp1left,atexp1right),rest671) end
| (67,(_,(MlyValue.appexp appexp1,appexp1left,appexp1right))::rest671)
 => let val result=MlyValue.infexp(fn _ => let val appexp as appexp1=
appexp1 ()
 in ( Infix.parseExp(!J, List.rev appexp) ) end
)
 in (LrTable.NT 25,(result,appexp1left,appexp1right),rest671) end
| (68,(_,(MlyValue.infexp infexp1,infexp1left,infexp1right))::rest671)
 => let val result=MlyValue.exp(fn _ => let val infexp as infexp1=
infexp1 ()
 in ( infexp ) end
)
 in (LrTable.NT 26,(result,infexp1left,infexp1right),rest671) end
| (69,(_,(MlyValue.ty ty1,_,tyright as ty1right))::_::(_,(MlyValue.exp
 exp1,expleft as exp1left,_))::rest671) => let val result=MlyValue.exp
(fn _ => let val exp as exp1=exp1 ()
val ty as ty1=ty1 ()
 in ( TYPEDExp(I(expleft,tyright), exp, ty) ) end
)
 in (LrTable.NT 26,(result,exp1left,ty1right),rest671) end
| (70,(_,(MlyValue.exp exp2,_,exp2right))::_::(_,(MlyValue.exp exp1,
exp1left,_))::rest671) => let val result=MlyValue.exp(fn _ => let val 
exp1=exp1 ()
val exp2=exp2 ()
 in ( ANDALSOExp(I(exp1left,exp2right), exp1, exp2)) end
)
 in (LrTable.NT 26,(result,exp1left,exp2right),rest671) end
| (71,(_,(MlyValue.exp exp2,_,exp2right))::_::(_,(MlyValue.exp exp1,
exp1left,_))::rest671) => let val result=MlyValue.exp(fn _ => let val 
exp1=exp1 ()
val exp2=exp2 ()
 in ( ORELSEExp(I(exp1left,exp2right), exp1, exp2) ) end
)
 in (LrTable.NT 26,(result,exp1left,exp2right),rest671) end
| (72,(_,(MlyValue.match match1,_,matchright as match1right))::_::(_,(
MlyValue.exp exp1,expleft as exp1left,_))::rest671) => let val result=
MlyValue.exp(fn _ => let val exp as exp1=exp1 ()
val match as match1=match1 ()
 in ( HANDLEExp(I(expleft,matchright), exp, match) ) end
)
 in (LrTable.NT 26,(result,exp1left,match1right),rest671) end
| (73,(_,(MlyValue.exp exp1,_,expright as exp1right))::(_,(_,RAISEleft
 as RAISE1left,_))::rest671) => let val result=MlyValue.exp(fn _ => 
let val exp as exp1=exp1 ()
 in ( RAISEExp(I(RAISEleft,expright), exp) ) end
)
 in (LrTable.NT 26,(result,RAISE1left,exp1right),rest671) end
| (74,(_,(MlyValue.exp exp3,_,exp3right))::_::(_,(MlyValue.exp exp2,_,
_))::_::(_,(MlyValue.exp exp1,_,_))::(_,(_,IFleft as IF1left,_))::
rest671) => let val result=MlyValue.exp(fn _ => let val exp1=exp1 ()
val exp2=exp2 ()
val exp3=exp3 ()
 in ( IFExp(I(IFleft,exp3right), exp1, exp2, exp3) ) end
)
 in (LrTable.NT 26,(result,IF1left,exp3right),rest671) end
| (75,(_,(MlyValue.exp exp2,_,exp2right))::_::(_,(MlyValue.exp exp1,_,
_))::(_,(_,WHILEleft as WHILE1left,_))::rest671) => let val result=
MlyValue.exp(fn _ => let val exp1=exp1 ()
val exp2=exp2 ()
 in ( WHILEExp(I(WHILEleft,exp2right), exp1, exp2) ) end
)
 in (LrTable.NT 26,(result,WHILE1left,exp2right),rest671) end
| (76,(_,(MlyValue.match match1,_,matchright as match1right))::_::(_,(
MlyValue.exp exp1,_,_))::(_,(_,CASEleft as CASE1left,_))::rest671) => 
let val result=MlyValue.exp(fn _ => let val exp as exp1=exp1 ()
val match as match1=match1 ()
 in ( CASEExp(I(CASEleft,matchright), exp, match) ) end
)
 in (LrTable.NT 26,(result,CASE1left,match1right),rest671) end
| (77,(_,(MlyValue.match match1,_,matchright as match1right))::(_,(_,
FNleft as FN1left,_))::rest671) => let val result=MlyValue.exp(fn _
 => let val match as match1=match1 ()
 in ( FNExp(I(FNleft,matchright), match) ) end
)
 in (LrTable.NT 26,(result,FN1left,match1right),rest671) end
| (78,(_,(MlyValue.BAR_match_opt BAR_match_opt1,_,BAR_match_optright
 as BAR_match_opt1right))::(_,(MlyValue.mrule mrule1,mruleleft as 
mrule1left,_))::rest671) => let val result=MlyValue.match(fn _ => let 
val mrule as mrule1=mrule1 ()
val BAR_match_opt as BAR_match_opt1=BAR_match_opt1 ()
 in (
 Match(I(mruleleft,BAR_match_optright),
                                        mrule, BAR_match_opt) )
 end
)
 in (LrTable.NT 27,(result,mrule1left,BAR_match_opt1right),rest671)
 end
| (79,(_,(MlyValue.match match1,_,match1right))::(_,(_,BAR1left,_))::
rest671) => let val result=MlyValue.BAR_match_opt(fn _ => let val 
match as match1=match1 ()
 in ( SOME match ) end
)
 in (LrTable.NT 28,(result,BAR1left,match1right),rest671) end
| (80,rest671) => let val result=MlyValue.BAR_match_opt(fn _ => (
 NONE ))
 in (LrTable.NT 28,(result,defaultPos,defaultPos),rest671) end
| (81,(_,(MlyValue.exp exp1,_,expright as exp1right))::_::(_,(
MlyValue.pat pat1,patleft as pat1left,_))::rest671) => let val result=
MlyValue.mrule(fn _ => let val pat as pat1=pat1 ()
val exp as exp1=exp1 ()
 in ( Mrule(I(patleft,expright), pat, exp) ) end
)
 in (LrTable.NT 29,(result,pat1left,exp1right),rest671) end
| (82,(_,(MlyValue.dec1 dec11,dec11left,dec11right))::rest671) => let 
val result=MlyValue.dec(fn _ => let val dec1 as dec11=dec11 ()
 in ( dec1 ) end
)
 in (LrTable.NT 30,(result,dec11left,dec11right),rest671) end
| (83,rest671) => let val result=MlyValue.dec(fn _ => (
 EMPTYDec(I(defaultPos,defaultPos)) ))
 in (LrTable.NT 30,(result,defaultPos,defaultPos),rest671) end
| (84,(_,(MlyValue.dec1' dec1'1,dec1'1left,dec1'1right))::rest671) => 
let val result=MlyValue.dec1(fn _ => let val dec1' as dec1'1=dec1'1 ()
 in ( dec1' ) end
)
 in (LrTable.NT 31,(result,dec1'1left,dec1'1right),rest671) end
| (85,(_,(_,_,ENDright as END1right))::(_,(MlyValue.popLocalInfix 
popLocalInfix1,_,_))::(_,(MlyValue.dec dec2,_,_))::(_,(
MlyValue.pushLocalInfix pushLocalInfix1,_,_))::_::(_,(MlyValue.dec 
dec1,_,_))::(_,(MlyValue.pushInfix pushInfix1,_,_))::(_,(_,LOCALleft
 as LOCAL1left,_))::rest671) => let val result=MlyValue.dec1(fn _ => 
let val pushInfix1=pushInfix1 ()
val dec1=dec1 ()
val pushLocalInfix1=pushLocalInfix1 ()
val dec2=dec2 ()
val popLocalInfix1=popLocalInfix1 ()
 in ( LOCALDec(I(LOCALleft,ENDright), dec1, dec2) ) end
)
 in (LrTable.NT 31,(result,LOCAL1left,END1right),rest671) end
| (86,(_,(MlyValue.dec1 dec12,_,dec12right))::(_,(MlyValue.dec1 dec11,
dec11left,_))::rest671) => let val result=MlyValue.dec1(fn _ => let 
val dec11=dec11 ()
val dec12=dec12 ()
 in ( SEQDec(I(dec11left,dec12right), dec11, dec12) ) end
)
 in (LrTable.NT 31,(result,dec11left,dec12right),rest671) end
| (87,(_,(_,SEMICOLON1left,SEMICOLON1right))::rest671) => let val 
result=MlyValue.dec1(fn _ => ( EMPTYDec(I(defaultPos,defaultPos)) ))
 in (LrTable.NT 31,(result,SEMICOLON1left,SEMICOLON1right),rest671)
 end
| (88,(_,(MlyValue.valbind valbind1,_,valbindright as valbind1right))
::(_,(_,VALleft as VAL1left,_))::rest671) => let val result=
MlyValue.dec1'(fn _ => let val valbind as valbind1=valbind1 ()
 in (
 VALDec(I(VALleft,valbindright),
                             TyVarseq(I(defaultPos,defaultPos), []), valbind) 
) end
)
 in (LrTable.NT 32,(result,VAL1left,valbind1right),rest671) end
| (89,(_,(MlyValue.valbind valbind1,_,valbindright as valbind1right))
::(_,(MlyValue.tyvarseq1 tyvarseq11,_,_))::(_,(_,VALleft as VAL1left,_
))::rest671) => let val result=MlyValue.dec1'(fn _ => let val 
tyvarseq1 as tyvarseq11=tyvarseq11 ()
val valbind as valbind1=valbind1 ()
 in ( VALDec(I(VALleft,valbindright), tyvarseq1, valbind) ) end
)
 in (LrTable.NT 32,(result,VAL1left,valbind1right),rest671) end
| (90,(_,(MlyValue.fvalbind fvalbind1,_,fvalbindright as 
fvalbind1right))::(_,(_,FUNleft as FUN1left,_))::rest671) => let val 
result=MlyValue.dec1'(fn _ => let val fvalbind as fvalbind1=fvalbind1 
()
 in (
 FUNDec(I(FUNleft,fvalbindright),
                            TyVarseq(I(defaultPos,defaultPos), []), fvalbind) 
) end
)
 in (LrTable.NT 32,(result,FUN1left,fvalbind1right),rest671) end
| (91,(_,(MlyValue.fvalbind fvalbind1,_,fvalbindright as 
fvalbind1right))::(_,(MlyValue.tyvarseq1 tyvarseq11,_,_))::(_,(_,
FUNleft as FUN1left,_))::rest671) => let val result=MlyValue.dec1'(fn 
_ => let val tyvarseq1 as tyvarseq11=tyvarseq11 ()
val fvalbind as fvalbind1=fvalbind1 ()
 in ( FUNDec(I(FUNleft,fvalbindright), tyvarseq1, fvalbind)) end
)
 in (LrTable.NT 32,(result,FUN1left,fvalbind1right),rest671) end
| (92,(_,(MlyValue.typbind typbind1,_,typbindright as typbind1right))
::(_,(_,TYPEleft as TYPE1left,_))::rest671) => let val result=
MlyValue.dec1'(fn _ => let val typbind as typbind1=typbind1 ()
 in ( TYPEDec(I(TYPEleft,typbindright), typbind) ) end
)
 in (LrTable.NT 32,(result,TYPE1left,typbind1right),rest671) end
| (93,(_,(MlyValue.WITHTYPE_typbind_opt WITHTYPE_typbind_opt1,_,
WITHTYPE_typbind_optright as WITHTYPE_typbind_opt1right))::(_,(
MlyValue.datbind0 datbind01,_,_))::(_,(_,DATATYPEleft as DATATYPE1left
,_))::rest671) => let val result=MlyValue.dec1'(fn _ => let val 
datbind0 as datbind01=datbind01 ()
val WITHTYPE_typbind_opt as WITHTYPE_typbind_opt1=
WITHTYPE_typbind_opt1 ()
 in (
 DATATYPEDec(I(DATATYPEleft,WITHTYPE_typbind_optright),
                                     datbind0, WITHTYPE_typbind_opt) 
) end
)
 in (LrTable.NT 32,(result,DATATYPE1left,WITHTYPE_typbind_opt1right),
rest671) end
| (94,(_,(MlyValue.WITHTYPE_typbind_opt WITHTYPE_typbind_opt1,_,
WITHTYPE_typbind_optright as WITHTYPE_typbind_opt1right))::(_,(
MlyValue.datbind1 datbind11,_,_))::(_,(_,DATATYPEleft as DATATYPE1left
,_))::rest671) => let val result=MlyValue.dec1'(fn _ => let val 
datbind1 as datbind11=datbind11 ()
val WITHTYPE_typbind_opt as WITHTYPE_typbind_opt1=
WITHTYPE_typbind_opt1 ()
 in (
 DATATYPEDec(I(DATATYPEleft,WITHTYPE_typbind_optright),
                                      datbind1, WITHTYPE_typbind_opt) 
) end
)
 in (LrTable.NT 32,(result,DATATYPE1left,WITHTYPE_typbind_opt1right),
rest671) end
| (95,(_,(MlyValue.longtycon longtycon1,_,longtyconright as 
longtycon1right))::_::_::(_,(MlyValue.tycon tycon1,_,_))::(_,(_,
DATATYPEleft as DATATYPE1left,_))::rest671) => let val result=
MlyValue.dec1'(fn _ => let val tycon as tycon1=tycon1 ()
val longtycon as longtycon1=longtycon1 ()
 in (
 REPLICATIONDec(I(DATATYPEleft,longtyconright),
                                         tycon, longtycon) 
) end
)
 in (LrTable.NT 32,(result,DATATYPE1left,longtycon1right),rest671) end
| (96,(_,(_,_,ENDright as END1right))::(_,(MlyValue.dec dec1,_,_))::_
::(_,(MlyValue.WITHTYPE_typbind_opt WITHTYPE_typbind_opt1,_,_))::(_,(
MlyValue.datbind datbind1,_,_))::(_,(_,ABSTYPEleft as ABSTYPE1left,_))
::rest671) => let val result=MlyValue.dec1'(fn _ => let val datbind
 as datbind1=datbind1 ()
val WITHTYPE_typbind_opt as WITHTYPE_typbind_opt1=
WITHTYPE_typbind_opt1 ()
val dec as dec1=dec1 ()
 in (
 ABSTYPEDec(I(ABSTYPEleft,ENDright), datbind,
                                     WITHTYPE_typbind_opt, dec) 
) end
)
 in (LrTable.NT 32,(result,ABSTYPE1left,END1right),rest671) end
| (97,(_,(MlyValue.exbind exbind1,_,exbindright as exbind1right))::(_,
(_,EXCEPTIONleft as EXCEPTION1left,_))::rest671) => let val result=
MlyValue.dec1'(fn _ => let val exbind as exbind1=exbind1 ()
 in ( EXCEPTIONDec(I(EXCEPTIONleft,exbindright), exbind) ) end
)
 in (LrTable.NT 32,(result,EXCEPTION1left,exbind1right),rest671) end
| (98,(_,(MlyValue.longstrid_list1 longstrid_list11,_,
longstrid_list1right as longstrid_list11right))::(_,(_,OPENleft as 
OPEN1left,_))::rest671) => let val result=MlyValue.dec1'(fn _ => let 
val longstrid_list1 as longstrid_list11=longstrid_list11 ()
 in (
 OPENDec(I(OPENleft,longstrid_list1right),
                                  longstrid_list1) )
 end
)
 in (LrTable.NT 32,(result,OPEN1left,longstrid_list11right),rest671)
 end
| (99,(_,(MlyValue.vid_list1 vid_list11,_,vid_list1right as 
vid_list11right))::(_,(MlyValue.d_opt d_opt1,_,_))::(_,(_,INFIXleft
 as INFIX1left,_))::rest671) => let val result=MlyValue.dec1'(fn _ => 
let val d_opt as d_opt1=d_opt1 ()
val vid_list1 as vid_list11=vid_list11 ()
 in (
 assignInfix((Infix.LEFT, d_opt), vid_list1);
                          EMPTYDec(I(INFIXleft,vid_list1right)) 
) end
)
 in (LrTable.NT 32,(result,INFIX1left,vid_list11right),rest671) end
| (100,(_,(MlyValue.vid_list1 vid_list11,_,vid_list1right as 
vid_list11right))::(_,(MlyValue.d_opt d_opt1,_,_))::(_,(_,INFIXRleft
 as INFIXR1left,_))::rest671) => let val result=MlyValue.dec1'(fn _
 => let val d_opt as d_opt1=d_opt1 ()
val vid_list1 as vid_list11=vid_list11 ()
 in (
 assignInfix((Infix.RIGHT, d_opt), vid_list1);
                          EMPTYDec(I(INFIXRleft,vid_list1right)) 
) end
)
 in (LrTable.NT 32,(result,INFIXR1left,vid_list11right),rest671) end
| (101,(_,(MlyValue.vid_list1 vid_list11,_,vid_list1right as 
vid_list11right))::(_,(_,NONFIXleft as NONFIX1left,_))::rest671) => 
let val result=MlyValue.dec1'(fn _ => let val vid_list1 as vid_list11=
vid_list11 ()
 in (
 cancelInfix(vid_list1);
                          EMPTYDec(I(NONFIXleft,vid_list1right)) )
 end
)
 in (LrTable.NT 32,(result,NONFIX1left,vid_list11right),rest671) end
| (102,(_,(MlyValue.typbind typbind1,_,typbind1right))::(_,(_,
WITHTYPE1left,_))::rest671) => let val result=
MlyValue.WITHTYPE_typbind_opt(fn _ => let val typbind as typbind1=
typbind1 ()
 in ( SOME typbind ) end
)
 in (LrTable.NT 33,(result,WITHTYPE1left,typbind1right),rest671) end
| (103,rest671) => let val result=MlyValue.WITHTYPE_typbind_opt(fn _
 => ( NONE ))
 in (LrTable.NT 33,(result,defaultPos,defaultPos),rest671) end
| (104,(_,(MlyValue.vid_list1 vid_list11,_,vid_list11right))::(_,(
MlyValue.vid vid1,vid1left,_))::rest671) => let val result=
MlyValue.vid_list1(fn _ => let val vid as vid1=vid1 ()
val vid_list1 as vid_list11=vid_list11 ()
 in ( vid::vid_list1 ) end
)
 in (LrTable.NT 34,(result,vid1left,vid_list11right),rest671) end
| (105,(_,(MlyValue.vid vid1,vid1left,vid1right))::rest671) => let 
val result=MlyValue.vid_list1(fn _ => let val vid as vid1=vid1 ()
 in ( vid::[] ) end
)
 in (LrTable.NT 34,(result,vid1left,vid1right),rest671) end
| (106,(_,(MlyValue.longstrid_list1 longstrid_list11,_,
longstrid_list11right))::(_,(MlyValue.longstrid longstrid1,
longstrid1left,_))::rest671) => let val result=
MlyValue.longstrid_list1(fn _ => let val longstrid as longstrid1=
longstrid1 ()
val longstrid_list1 as longstrid_list11=longstrid_list11 ()
 in ( longstrid::longstrid_list1 ) end
)
 in (LrTable.NT 35,(result,longstrid1left,longstrid_list11right),
rest671) end
| (107,(_,(MlyValue.longstrid longstrid1,longstrid1left,
longstrid1right))::rest671) => let val result=MlyValue.longstrid_list1
(fn _ => let val longstrid as longstrid1=longstrid1 ()
 in ( longstrid::[] ) end
)
 in (LrTable.NT 35,(result,longstrid1left,longstrid1right),rest671)
 end
| (108,(_,(MlyValue.d d1,d1left,d1right))::rest671) => let val result=
MlyValue.d_opt(fn _ => let val d as d1=d1 ()
 in ( d ) end
)
 in (LrTable.NT 36,(result,d1left,d1right),rest671) end
| (109,rest671) => let val result=MlyValue.d_opt(fn _ => ( 0 ))
 in (LrTable.NT 36,(result,defaultPos,defaultPos),rest671) end
| (110,(_,(MlyValue.AND_valbind_opt AND_valbind_opt1,_,
AND_valbind_optright as AND_valbind_opt1right))::(_,(MlyValue.exp exp1
,_,_))::_::(_,(MlyValue.pat pat1,patleft as pat1left,_))::rest671) => 
let val result=MlyValue.valbind(fn _ => let val pat as pat1=pat1 ()
val exp as exp1=exp1 ()
val AND_valbind_opt as AND_valbind_opt1=AND_valbind_opt1 ()
 in (
 PLAINValBind(I(patleft,AND_valbind_optright),
                                       pat, exp, AND_valbind_opt) 
) end
)
 in (LrTable.NT 37,(result,pat1left,AND_valbind_opt1right),rest671)
 end
| (111,(_,(MlyValue.valbind valbind1,_,valbindright as valbind1right))
::(_,(_,RECleft as REC1left,_))::rest671) => let val result=
MlyValue.valbind(fn _ => let val valbind as valbind1=valbind1 ()
 in ( RECValBind(I(RECleft,valbindright), valbind) ) end
)
 in (LrTable.NT 37,(result,REC1left,valbind1right),rest671) end
| (112,(_,(MlyValue.valbind valbind1,_,valbind1right))::(_,(_,AND1left
,_))::rest671) => let val result=MlyValue.AND_valbind_opt(fn _ => let 
val valbind as valbind1=valbind1 ()
 in ( SOME valbind ) end
)
 in (LrTable.NT 38,(result,AND1left,valbind1right),rest671) end
| (113,rest671) => let val result=MlyValue.AND_valbind_opt(fn _ => (
 NONE ))
 in (LrTable.NT 38,(result,defaultPos,defaultPos),rest671) end
| (114,(_,(MlyValue.AND_fvalbind_opt AND_fvalbind_opt1,_,
AND_fvalbind_optright as AND_fvalbind_opt1right))::(_,(MlyValue.fmatch
 fmatch1,fmatchleft as fmatch1left,_))::rest671) => let val result=
MlyValue.fvalbind(fn _ => let val fmatch as fmatch1=fmatch1 ()
val AND_fvalbind_opt as AND_fvalbind_opt1=AND_fvalbind_opt1 ()
 in (
 FvalBind(I(fmatchleft,AND_fvalbind_optright),
                                     fmatch, AND_fvalbind_opt) 
) end
)
 in (LrTable.NT 39,(result,fmatch1left,AND_fvalbind_opt1right),rest671
) end
| (115,(_,(MlyValue.fvalbind fvalbind1,_,fvalbind1right))::(_,(_,
AND1left,_))::rest671) => let val result=MlyValue.AND_fvalbind_opt(fn 
_ => let val fvalbind as fvalbind1=fvalbind1 ()
 in ( SOME fvalbind ) end
)
 in (LrTable.NT 40,(result,AND1left,fvalbind1right),rest671) end
| (116,rest671) => let val result=MlyValue.AND_fvalbind_opt(fn _ => (
 NONE ))
 in (LrTable.NT 40,(result,defaultPos,defaultPos),rest671) end
| (117,(_,(MlyValue.BAR_fmatch_opt BAR_fmatch_opt1,_,
BAR_fmatch_optright as BAR_fmatch_opt1right))::(_,(MlyValue.fmrule 
fmrule1,fmruleleft as fmrule1left,_))::rest671) => let val result=
MlyValue.fmatch(fn _ => let val fmrule as fmrule1=fmrule1 ()
val BAR_fmatch_opt as BAR_fmatch_opt1=BAR_fmatch_opt1 ()
 in (
 Fmatch(I(fmruleleft,BAR_fmatch_optright),
                                 fmrule, BAR_fmatch_opt) 
) end
)
 in (LrTable.NT 41,(result,fmrule1left,BAR_fmatch_opt1right),rest671)
 end
| (118,(_,(MlyValue.fmatch fmatch1,_,fmatch1right))::(_,(_,BAR1left,_)
)::rest671) => let val result=MlyValue.BAR_fmatch_opt(fn _ => let val 
fmatch as fmatch1=fmatch1 ()
 in ( SOME fmatch ) end
)
 in (LrTable.NT 42,(result,BAR1left,fmatch1right),rest671) end
| (119,rest671) => let val result=MlyValue.BAR_fmatch_opt(fn _ => (
 NONE ))
 in (LrTable.NT 42,(result,defaultPos,defaultPos),rest671) end
| (120,(_,(MlyValue.exp exp1,_,expright as exp1right))::_::(_,(
MlyValue.COLON_ty_opt COLON_ty_opt1,_,_))::(_,(MlyValue.atpat_list1 
atpat_list11,atpat_list1left as atpat_list11left,_))::rest671) => let 
val result=MlyValue.fmrule(fn _ => let val atpat_list1 as atpat_list11
=atpat_list11 ()
val COLON_ty_opt as COLON_ty_opt1=COLON_ty_opt1 ()
val exp as exp1=exp1 ()
 in (
 let
                            val (op_opt, vid, atpats) =
                                Infix.parseFmrule(!J, atpat_list1)
                          in
                            Fmrule(I(atpat_list1left,expright),
                                   op_opt, vid, atpats, COLON_ty_opt, exp)
                          end 
) end
)
 in (LrTable.NT 43,(result,atpat_list11left,exp1right),rest671) end
| (121,(_,(MlyValue.AND_typbind_opt AND_typbind_opt1,_,
AND_typbind_optright as AND_typbind_opt1right))::(_,(MlyValue.ty ty1,_
,_))::_::(_,(MlyValue.tycon tycon1,_,_))::(_,(MlyValue.tyvarseq 
tyvarseq1,tyvarseqleft as tyvarseq1left,_))::rest671) => let val 
result=MlyValue.typbind(fn _ => let val tyvarseq as tyvarseq1=
tyvarseq1 ()
val tycon as tycon1=tycon1 ()
val ty as ty1=ty1 ()
val AND_typbind_opt as AND_typbind_opt1=AND_typbind_opt1 ()
 in (
 TypBind(I(tyvarseqleft,AND_typbind_optright),
                                  tyvarseq, tycon, ty, AND_typbind_opt) 
) end
)
 in (LrTable.NT 44,(result,tyvarseq1left,AND_typbind_opt1right),
rest671) end
| (122,(_,(MlyValue.typbind typbind1,_,typbind1right))::(_,(_,AND1left
,_))::rest671) => let val result=MlyValue.AND_typbind_opt(fn _ => let 
val typbind as typbind1=typbind1 ()
 in ( SOME typbind ) end
)
 in (LrTable.NT 45,(result,AND1left,typbind1right),rest671) end
| (123,rest671) => let val result=MlyValue.AND_typbind_opt(fn _ => (
 NONE ))
 in (LrTable.NT 45,(result,defaultPos,defaultPos),rest671) end
| (124,(_,(MlyValue.AND_datbind_opt AND_datbind_opt1,_,
AND_datbind_optright as AND_datbind_opt1right))::(_,(MlyValue.conbind 
conbind1,_,_))::_::(_,(MlyValue.tycon tycon1,_,_))::(_,(
MlyValue.tyvarseq tyvarseq1,tyvarseqleft as tyvarseq1left,_))::rest671
) => let val result=MlyValue.datbind(fn _ => let val tyvarseq as 
tyvarseq1=tyvarseq1 ()
val tycon as tycon1=tycon1 ()
val conbind as conbind1=conbind1 ()
val AND_datbind_opt as AND_datbind_opt1=AND_datbind_opt1 ()
 in (
 DatBind(I(tyvarseqleft,AND_datbind_optright),
                                  tyvarseq, tycon, conbind, AND_datbind_opt) 
) end
)
 in (LrTable.NT 46,(result,tyvarseq1left,AND_datbind_opt1right),
rest671) end
| (125,(_,(MlyValue.AND_datbind_opt AND_datbind_opt1,_,
AND_datbind_optright as AND_datbind_opt1right))::(_,(MlyValue.conbind 
conbind1,_,_))::_::(_,(MlyValue.tycon tycon1,tyconleft as tycon1left,_
))::rest671) => let val result=MlyValue.datbind0(fn _ => let val tycon
 as tycon1=tycon1 ()
val conbind as conbind1=conbind1 ()
val AND_datbind_opt as AND_datbind_opt1=AND_datbind_opt1 ()
 in (
 DatBind(I(tyconleft,AND_datbind_optright),
                                  TyVarseq(I(defaultPos,defaultPos), []),
                                  tycon, conbind, AND_datbind_opt) 
) end
)
 in (LrTable.NT 47,(result,tycon1left,AND_datbind_opt1right),rest671)
 end
| (126,(_,(MlyValue.AND_datbind_opt AND_datbind_opt1,_,
AND_datbind_optright as AND_datbind_opt1right))::(_,(MlyValue.conbind 
conbind1,_,_))::_::(_,(MlyValue.tycon tycon1,_,_))::(_,(
MlyValue.tyvarseq1 tyvarseq11,tyvarseq1left as tyvarseq11left,_))::
rest671) => let val result=MlyValue.datbind1(fn _ => let val tyvarseq1
 as tyvarseq11=tyvarseq11 ()
val tycon as tycon1=tycon1 ()
val conbind as conbind1=conbind1 ()
val AND_datbind_opt as AND_datbind_opt1=AND_datbind_opt1 ()
 in (
 DatBind(I(tyvarseq1left,AND_datbind_optright),
                                  tyvarseq1, tycon, conbind, AND_datbind_opt) 
) end
)
 in (LrTable.NT 48,(result,tyvarseq11left,AND_datbind_opt1right),
rest671) end
| (127,(_,(MlyValue.datbind datbind1,_,datbind1right))::(_,(_,AND1left
,_))::rest671) => let val result=MlyValue.AND_datbind_opt(fn _ => let 
val datbind as datbind1=datbind1 ()
 in ( SOME datbind ) end
)
 in (LrTable.NT 49,(result,AND1left,datbind1right),rest671) end
| (128,rest671) => let val result=MlyValue.AND_datbind_opt(fn _ => (
 NONE ))
 in (LrTable.NT 49,(result,defaultPos,defaultPos),rest671) end
| (129,(_,(MlyValue.BAR_conbind_opt BAR_conbind_opt1,_,
BAR_conbind_optright as BAR_conbind_opt1right))::(_,(
MlyValue.OF_ty_opt OF_ty_opt1,_,_))::(_,(MlyValue.vid' vid'1,_,_))::(_
,(MlyValue.OP_opt OP_opt1,OP_optleft as OP_opt1left,_))::rest671) => 
let val result=MlyValue.conbind(fn _ => let val OP_opt as OP_opt1=
OP_opt1 ()
val vid' as vid'1=vid'1 ()
val OF_ty_opt as OF_ty_opt1=OF_ty_opt1 ()
val BAR_conbind_opt as BAR_conbind_opt1=BAR_conbind_opt1 ()
 in (
 ConBind(I(OP_optleft,BAR_conbind_optright),
                                  OP_opt, vid', OF_ty_opt, BAR_conbind_opt) 
) end
)
 in (LrTable.NT 50,(result,OP_opt1left,BAR_conbind_opt1right),rest671)
 end
| (130,(_,(MlyValue.conbind conbind1,_,conbind1right))::(_,(_,BAR1left
,_))::rest671) => let val result=MlyValue.BAR_conbind_opt(fn _ => let 
val conbind as conbind1=conbind1 ()
 in ( SOME conbind ) end
)
 in (LrTable.NT 51,(result,BAR1left,conbind1right),rest671) end
| (131,rest671) => let val result=MlyValue.BAR_conbind_opt(fn _ => (
 NONE ))
 in (LrTable.NT 51,(result,defaultPos,defaultPos),rest671) end
| (132,(_,(MlyValue.ty ty1,_,ty1right))::(_,(_,OF1left,_))::rest671)
 => let val result=MlyValue.OF_ty_opt(fn _ => let val ty as ty1=ty1 ()
 in ( SOME ty ) end
)
 in (LrTable.NT 52,(result,OF1left,ty1right),rest671) end
| (133,rest671) => let val result=MlyValue.OF_ty_opt(fn _ => ( NONE ))
 in (LrTable.NT 52,(result,defaultPos,defaultPos),rest671) end
| (134,(_,(MlyValue.AND_exbind_opt AND_exbind_opt1,_,
AND_exbind_optright as AND_exbind_opt1right))::(_,(MlyValue.OF_ty_opt 
OF_ty_opt1,_,_))::(_,(MlyValue.vid' vid'1,_,_))::(_,(MlyValue.OP_opt 
OP_opt1,OP_optleft as OP_opt1left,_))::rest671) => let val result=
MlyValue.exbind(fn _ => let val OP_opt as OP_opt1=OP_opt1 ()
val vid' as vid'1=vid'1 ()
val OF_ty_opt as OF_ty_opt1=OF_ty_opt1 ()
val AND_exbind_opt as AND_exbind_opt1=AND_exbind_opt1 ()
 in (
 NEWExBind(I(OP_optleft,AND_exbind_optright),
                                    OP_opt, vid', OF_ty_opt, AND_exbind_opt) 
) end
)
 in (LrTable.NT 53,(result,OP_opt1left,AND_exbind_opt1right),rest671)
 end
| (135,(_,(MlyValue.AND_exbind_opt AND_exbind_opt1,_,
AND_exbind_optright as AND_exbind_opt1right))::(_,(MlyValue.longvid 
longvid1,_,_))::(_,(MlyValue.OP_opt OP_opt2,_,_))::_::(_,(
MlyValue.vid' vid'1,_,_))::(_,(MlyValue.OP_opt OP_opt1,OP_opt1left,_))
::rest671) => let val result=MlyValue.exbind(fn _ => let val OP_opt1=
OP_opt1 ()
val vid' as vid'1=vid'1 ()
val OP_opt2=OP_opt2 ()
val longvid as longvid1=longvid1 ()
val AND_exbind_opt as AND_exbind_opt1=AND_exbind_opt1 ()
 in (
 EQUALExBind(I(OP_opt1left,AND_exbind_optright),
                                      OP_opt1, vid', OP_opt2, longvid,
                                      AND_exbind_opt) 
) end
)
 in (LrTable.NT 53,(result,OP_opt1left,AND_exbind_opt1right),rest671)
 end
| (136,(_,(MlyValue.exbind exbind1,_,exbind1right))::(_,(_,AND1left,_)
)::rest671) => let val result=MlyValue.AND_exbind_opt(fn _ => let val 
exbind as exbind1=exbind1 ()
 in ( SOME exbind ) end
)
 in (LrTable.NT 54,(result,AND1left,exbind1right),rest671) end
| (137,rest671) => let val result=MlyValue.AND_exbind_opt(fn _ => (
 NONE ))
 in (LrTable.NT 54,(result,defaultPos,defaultPos),rest671) end
| (138,(_,(MlyValue.atpat' atpat'1,atpat'1left,atpat'1right))::rest671
) => let val result=MlyValue.atpat(fn _ => let val atpat' as atpat'1=
atpat'1 ()
 in ( atpat' ) end
)
 in (LrTable.NT 55,(result,atpat'1left,atpat'1right),rest671) end
| (139,(_,(MlyValue.longvid' longvid'1,_,longvid'right as 
longvid'1right))::(_,(MlyValue.OP_opt OP_opt1,OP_optleft as 
OP_opt1left,_))::rest671) => let val result=MlyValue.atpat(fn _ => 
let val OP_opt as OP_opt1=OP_opt1 ()
val longvid' as longvid'1=longvid'1 ()
 in (
 LONGVIDAtPat(I(OP_optleft,longvid'right),
                                       OP_opt, longvid') 
) end
)
 in (LrTable.NT 55,(result,OP_opt1left,longvid'1right),rest671) end
| (140,(_,(_,UNDERBARleft as UNDERBAR1left,UNDERBARright as 
UNDERBAR1right))::rest671) => let val result=MlyValue.atpat'(fn _ => (
 WILDCARDAtPat(I(UNDERBARleft,UNDERBARright)) ))
 in (LrTable.NT 56,(result,UNDERBAR1left,UNDERBAR1right),rest671) end
| (141,(_,(MlyValue.scon scon1,sconleft as scon1left,sconright as 
scon1right))::rest671) => let val result=MlyValue.atpat'(fn _ => let 
val scon as scon1=scon1 ()
 in ( SCONAtPat(I(sconleft,sconright), scon) ) end
)
 in (LrTable.NT 56,(result,scon1left,scon1right),rest671) end
| (142,(_,(_,_,RBRACEright as RBRACE1right))::(_,(MlyValue.patrow_opt 
patrow_opt1,_,_))::(_,(_,LBRACEleft as LBRACE1left,_))::rest671) => 
let val result=MlyValue.atpat'(fn _ => let val patrow_opt as 
patrow_opt1=patrow_opt1 ()
 in ( RECORDAtPat(I(LBRACEleft,RBRACEright), patrow_opt) ) end
)
 in (LrTable.NT 56,(result,LBRACE1left,RBRACE1right),rest671) end
| (143,(_,(_,_,RPARright as RPAR1right))::(_,(_,LPARleft as LPAR1left,
_))::rest671) => let val result=MlyValue.atpat'(fn _ => (
 UNITAtPat(I(LPARleft,RPARright)) ))
 in (LrTable.NT 56,(result,LPAR1left,RPAR1right),rest671) end
| (144,(_,(_,_,RPARright as RPAR1right))::(_,(MlyValue.pat_COMMA_list2
 pat_COMMA_list21,_,_))::(_,(_,LPARleft as LPAR1left,_))::rest671) => 
let val result=MlyValue.atpat'(fn _ => let val pat_COMMA_list2 as 
pat_COMMA_list21=pat_COMMA_list21 ()
 in ( TUPLEAtPat(I(LPARleft,RPARright), pat_COMMA_list2) ) end
)
 in (LrTable.NT 56,(result,LPAR1left,RPAR1right),rest671) end
| (145,(_,(_,_,RBRACKright as RBRACK1right))::(_,(
MlyValue.pat_COMMA_list0 pat_COMMA_list01,_,_))::(_,(_,LBRACKleft as 
LBRACK1left,_))::rest671) => let val result=MlyValue.atpat'(fn _ => 
let val pat_COMMA_list0 as pat_COMMA_list01=pat_COMMA_list01 ()
 in ( LISTAtPat(I(LBRACKleft,RBRACKright),
                                    pat_COMMA_list0) )
 end
)
 in (LrTable.NT 56,(result,LBRACK1left,RBRACK1right),rest671) end
| (146,(_,(_,_,RPARright as RPAR1right))::(_,(MlyValue.pat pat1,_,_))
::(_,(_,LPARleft as LPAR1left,_))::rest671) => let val result=
MlyValue.atpat'(fn _ => let val pat as pat1=pat1 ()
 in ( PARAtPat(I(LPARleft,RPARright), pat) ) end
)
 in (LrTable.NT 56,(result,LPAR1left,RPAR1right),rest671) end
| (147,(_,(MlyValue.pat_COMMA_list1 pat_COMMA_list11,
pat_COMMA_list11left,pat_COMMA_list11right))::rest671) => let val 
result=MlyValue.pat_COMMA_list0(fn _ => let val pat_COMMA_list1 as 
pat_COMMA_list11=pat_COMMA_list11 ()
 in ( pat_COMMA_list1 ) end
)
 in (LrTable.NT 57,(result,pat_COMMA_list11left,pat_COMMA_list11right)
,rest671) end
| (148,rest671) => let val result=MlyValue.pat_COMMA_list0(fn _ => (
 [] ))
 in (LrTable.NT 57,(result,defaultPos,defaultPos),rest671) end
| (149,(_,(MlyValue.pat_COMMA_list1 pat_COMMA_list11,_,
pat_COMMA_list11right))::_::(_,(MlyValue.pat pat1,pat1left,_))::
rest671) => let val result=MlyValue.pat_COMMA_list1(fn _ => let val 
pat as pat1=pat1 ()
val pat_COMMA_list1 as pat_COMMA_list11=pat_COMMA_list11 ()
 in ( pat::pat_COMMA_list1 ) end
)
 in (LrTable.NT 58,(result,pat1left,pat_COMMA_list11right),rest671)
 end
| (150,(_,(MlyValue.pat pat1,pat1left,pat1right))::rest671) => let 
val result=MlyValue.pat_COMMA_list1(fn _ => let val pat as pat1=pat1 
()
 in ( pat::[] ) end
)
 in (LrTable.NT 58,(result,pat1left,pat1right),rest671) end
| (151,(_,(MlyValue.pat_COMMA_list1 pat_COMMA_list11,_,
pat_COMMA_list11right))::_::(_,(MlyValue.pat pat1,pat1left,_))::
rest671) => let val result=MlyValue.pat_COMMA_list2(fn _ => let val 
pat as pat1=pat1 ()
val pat_COMMA_list1 as pat_COMMA_list11=pat_COMMA_list11 ()
 in ( pat::pat_COMMA_list1 ) end
)
 in (LrTable.NT 59,(result,pat1left,pat_COMMA_list11right),rest671)
 end
| (152,(_,(_,DOTSleft as DOTS1left,DOTSright as DOTS1right))::rest671)
 => let val result=MlyValue.patrow(fn _ => (
 WILDCARDPatRow(I(DOTSleft,DOTSright)) ))
 in (LrTable.NT 60,(result,DOTS1left,DOTS1right),rest671) end
| (153,(_,(MlyValue.COMMA_patrow_opt COMMA_patrow_opt1,_,
COMMA_patrow_optright as COMMA_patrow_opt1right))::(_,(MlyValue.pat 
pat1,_,_))::_::(_,(MlyValue.lab lab1,lableft as lab1left,_))::rest671)
 => let val result=MlyValue.patrow(fn _ => let val lab as lab1=lab1 ()
val pat as pat1=pat1 ()
val COMMA_patrow_opt as COMMA_patrow_opt1=COMMA_patrow_opt1 ()
 in (
 ROWPatRow(I(lableft,COMMA_patrow_optright),
                                      lab, pat, COMMA_patrow_opt) 
) end
)
 in (LrTable.NT 60,(result,lab1left,COMMA_patrow_opt1right),rest671)
 end
| (154,(_,(MlyValue.COMMA_patrow_opt COMMA_patrow_opt1,_,
COMMA_patrow_optright as COMMA_patrow_opt1right))::(_,(
MlyValue.AS_pat_opt AS_pat_opt1,_,_))::(_,(MlyValue.COLON_ty_opt 
COLON_ty_opt1,_,_))::(_,(MlyValue.vid' vid'1,vid'left as vid'1left,_))
::rest671) => let val result=MlyValue.patrow(fn _ => let val vid' as 
vid'1=vid'1 ()
val COLON_ty_opt as COLON_ty_opt1=COLON_ty_opt1 ()
val AS_pat_opt as AS_pat_opt1=AS_pat_opt1 ()
val COMMA_patrow_opt as COMMA_patrow_opt1=COMMA_patrow_opt1 ()
 in (
 VIDPatRow(I(vid'left,COMMA_patrow_optright),
                                    vid', COLON_ty_opt, AS_pat_opt,
                                    COMMA_patrow_opt) 
) end
)
 in (LrTable.NT 60,(result,vid'1left,COMMA_patrow_opt1right),rest671)
 end
| (155,(_,(MlyValue.patrow patrow1,_,patrow1right))::(_,(_,COMMA1left,
_))::rest671) => let val result=MlyValue.COMMA_patrow_opt(fn _ => let 
val patrow as patrow1=patrow1 ()
 in ( SOME patrow ) end
)
 in (LrTable.NT 62,(result,COMMA1left,patrow1right),rest671) end
| (156,rest671) => let val result=MlyValue.COMMA_patrow_opt(fn _ => (
 NONE ))
 in (LrTable.NT 62,(result,defaultPos,defaultPos),rest671) end
| (157,(_,(MlyValue.ty ty1,_,ty1right))::(_,(_,COLON1left,_))::rest671
) => let val result=MlyValue.COLON_ty_opt(fn _ => let val ty as ty1=
ty1 ()
 in ( SOME ty ) end
)
 in (LrTable.NT 63,(result,COLON1left,ty1right),rest671) end
| (158,rest671) => let val result=MlyValue.COLON_ty_opt(fn _ => (
 NONE ))
 in (LrTable.NT 63,(result,defaultPos,defaultPos),rest671) end
| (159,(_,(MlyValue.pat pat1,_,pat1right))::(_,(_,AS1left,_))::rest671
) => let val result=MlyValue.AS_pat_opt(fn _ => let val pat as pat1=
pat1 ()
 in ( SOME pat ) end
)
 in (LrTable.NT 64,(result,AS1left,pat1right),rest671) end
| (160,rest671) => let val result=MlyValue.AS_pat_opt(fn _ => ( NONE )
)
 in (LrTable.NT 64,(result,defaultPos,defaultPos),rest671) end
| (161,(_,(MlyValue.patrow patrow1,patrow1left,patrow1right))::rest671
) => let val result=MlyValue.patrow_opt(fn _ => let val patrow as 
patrow1=patrow1 ()
 in ( SOME patrow ) end
)
 in (LrTable.NT 61,(result,patrow1left,patrow1right),rest671) end
| (162,rest671) => let val result=MlyValue.patrow_opt(fn _ => ( NONE )
)
 in (LrTable.NT 61,(result,defaultPos,defaultPos),rest671) end
| (163,(_,(MlyValue.atpat atpat1,atpat1left,atpat1right))::rest671)
 => let val result=MlyValue.pat(fn _ => let val atpat as atpat1=atpat1
 ()
 in ( Infix.parsePat(!J, [atpat]) ) end
)
 in (LrTable.NT 65,(result,atpat1left,atpat1right),rest671) end
| (164,(_,(MlyValue.atpat_list2 atpat_list21,atpat_list21left,
atpat_list21right))::rest671) => let val result=MlyValue.pat(fn _ => 
let val atpat_list2 as atpat_list21=atpat_list21 ()
 in ( Infix.parsePat(!J, atpat_list2) ) end
)
 in (LrTable.NT 65,(result,atpat_list21left,atpat_list21right),rest671
) end
| (165,(_,(MlyValue.COLON_ty_list1 COLON_ty_list11,_,
COLON_ty_list11right))::(_,(MlyValue.atpat' atpat'1,atpat'1left,_))::
rest671) => let val result=MlyValue.pat(fn _ => let val atpat' as 
atpat'1=atpat'1 ()
val COLON_ty_list1 as COLON_ty_list11=COLON_ty_list11 ()
 in (
 let val pat = Infix.parsePat(!J, [atpat'])
                          in typedPat(pat, COLON_ty_list1) end 
) end
)
 in (LrTable.NT 65,(result,atpat'1left,COLON_ty_list11right),rest671)
 end
| (166,(_,(MlyValue.COLON_ty_list1 COLON_ty_list11,_,
COLON_ty_list11right))::(_,(MlyValue.atpat_list2 atpat_list21,
atpat_list21left,_))::rest671) => let val result=MlyValue.pat(fn _ => 
let val atpat_list2 as atpat_list21=atpat_list21 ()
val COLON_ty_list1 as COLON_ty_list11=COLON_ty_list11 ()
 in (
 let val pat = Infix.parsePat(!J, atpat_list2)
                          in typedPat(pat, COLON_ty_list1) end 
) end
)
 in (LrTable.NT 65,(result,atpat_list21left,COLON_ty_list11right),
rest671) end
| (167,(_,(MlyValue.COLON_ty_list1 COLON_ty_list11,_,
COLON_ty_list11right))::(_,(MlyValue.vid' vid'1,_,vid'right))::(_,(
MlyValue.OP_opt OP_opt1,OP_optleft as OP_opt1left,_))::rest671) => 
let val result=MlyValue.pat(fn _ => let val OP_opt as OP_opt1=OP_opt1 
()
val vid' as vid'1=vid'1 ()
val COLON_ty_list1 as COLON_ty_list11=COLON_ty_list11 ()
 in (
 let val atpat = LONGVIDAtPat(I(OP_optleft,vid'right),
                                                       OP_opt,
                                                       LongVId.fromId vid')
                              val pat   = Infix.parsePat(!J, [atpat])
                          in typedPat(pat, COLON_ty_list1) end 
) end
)
 in (LrTable.NT 65,(result,OP_opt1left,COLON_ty_list11right),rest671)
 end
| (168,(_,(MlyValue.COLON_ty_list1 COLON_ty_list11,_,
COLON_ty_list11right))::(_,(MlyValue.LONGID LONGID1,_,LONGIDright))::(
_,(MlyValue.OP_opt OP_opt1,OP_optleft as OP_opt1left,_))::rest671) => 
let val result=MlyValue.pat(fn _ => let val OP_opt as OP_opt1=OP_opt1 
()
val LONGID as LONGID1=LONGID1 ()
val COLON_ty_list1 as COLON_ty_list11=COLON_ty_list11 ()
 in (
 let val longvid = LongVId.implode
                                                (toLongId VId.fromString LONGID)
                              val atpat = LONGVIDAtPat(I(OP_optleft,LONGIDright),
                                                       OP_opt, longvid)
                              val pat   = Infix.parsePat(!J, [atpat])
                          in typedPat(pat, COLON_ty_list1) end 
) end
)
 in (LrTable.NT 65,(result,OP_opt1left,COLON_ty_list11right),rest671)
 end
| (169,(_,(MlyValue.pat pat1,_,patright as pat1right))::_::(_,(
MlyValue.COLON_ty_opt COLON_ty_opt1,_,_))::(_,(MlyValue.vid' vid'1,_,
vid'right))::(_,(MlyValue.OP_opt OP_opt1,OP_optleft as OP_opt1left,_))
::rest671) => let val result=MlyValue.pat(fn _ => let val OP_opt as 
OP_opt1=OP_opt1 ()
val vid' as vid'1=vid'1 ()
val COLON_ty_opt as COLON_ty_opt1=COLON_ty_opt1 ()
val pat as pat1=pat1 ()
 in (
 Infix.parsePat(!J,
                                 [ LONGVIDAtPat(I(OP_optleft,vid'right),
                                                OP_opt,
                                                LongVId.implode([],vid')) ] ) ;
                          ASPat(I(OP_optleft,patright),
                                OP_opt, vid', COLON_ty_opt, pat) 
) end
)
 in (LrTable.NT 65,(result,OP_opt1left,pat1right),rest671) end
| (170,(_,(MlyValue.atpat_list1 atpat_list11,_,atpat_list11right))::(_
,(MlyValue.atpat atpat1,atpat1left,_))::rest671) => let val result=
MlyValue.atpat_list1(fn _ => let val atpat as atpat1=atpat1 ()
val atpat_list1 as atpat_list11=atpat_list11 ()
 in ( atpat::atpat_list1 ) end
)
 in (LrTable.NT 66,(result,atpat1left,atpat_list11right),rest671) end
| (171,(_,(MlyValue.atpat atpat1,atpat1left,atpat1right))::rest671)
 => let val result=MlyValue.atpat_list1(fn _ => let val atpat as 
atpat1=atpat1 ()
 in ( atpat::[] ) end
)
 in (LrTable.NT 66,(result,atpat1left,atpat1right),rest671) end
| (172,(_,(MlyValue.atpat_list1 atpat_list11,_,atpat_list11right))::(_
,(MlyValue.atpat atpat1,atpat1left,_))::rest671) => let val result=
MlyValue.atpat_list2(fn _ => let val atpat as atpat1=atpat1 ()
val atpat_list1 as atpat_list11=atpat_list11 ()
 in ( atpat::atpat_list1 ) end
)
 in (LrTable.NT 67,(result,atpat1left,atpat_list11right),rest671) end
| (173,(_,(MlyValue.COLON_ty_list1 COLON_ty_list11,_,
COLON_ty_list11right))::(_,(MlyValue.ty ty1,_,_))::(_,(_,COLON1left,_)
)::rest671) => let val result=MlyValue.COLON_ty_list1(fn _ => let val 
ty as ty1=ty1 ()
val COLON_ty_list1 as COLON_ty_list11=COLON_ty_list11 ()
 in ( ty::COLON_ty_list1 ) end
)
 in (LrTable.NT 68,(result,COLON1left,COLON_ty_list11right),rest671)
 end
| (174,(_,(MlyValue.ty ty1,_,ty1right))::(_,(_,COLON1left,_))::rest671
) => let val result=MlyValue.COLON_ty_list1(fn _ => let val ty as ty1=
ty1 ()
 in ( ty::[] ) end
)
 in (LrTable.NT 68,(result,COLON1left,ty1right),rest671) end
| (175,(_,(MlyValue.tupty tupty1,tupty1left,tupty1right))::rest671)
 => let val result=MlyValue.ty(fn _ => let val tupty as tupty1=tupty1 
()
 in ( tupty ) end
)
 in (LrTable.NT 69,(result,tupty1left,tupty1right),rest671) end
| (176,(_,(MlyValue.ty ty1,_,tyright as ty1right))::_::(_,(
MlyValue.tupty tupty1,tuptyleft as tupty1left,_))::rest671) => let 
val result=MlyValue.ty(fn _ => let val tupty as tupty1=tupty1 ()
val ty as ty1=ty1 ()
 in ( ARROWTy(I(tuptyleft,tyright), tupty, ty) ) end
)
 in (LrTable.NT 69,(result,tupty1left,ty1right),rest671) end
| (177,(_,(MlyValue.ty_STAR_list ty_STAR_list1,ty_STAR_listleft as 
ty_STAR_list1left,ty_STAR_listright as ty_STAR_list1right))::rest671)
 => let val result=MlyValue.tupty(fn _ => let val ty_STAR_list as 
ty_STAR_list1=ty_STAR_list1 ()
 in (
 TUPLETy(I(ty_STAR_listleft,ty_STAR_listright),
                                   ty_STAR_list) )
 end
)
 in (LrTable.NT 70,(result,ty_STAR_list1left,ty_STAR_list1right),
rest671) end
| (178,(_,(MlyValue.ty_STAR_list ty_STAR_list1,_,ty_STAR_list1right))
::_::(_,(MlyValue.consty consty1,consty1left,_))::rest671) => let val 
result=MlyValue.ty_STAR_list(fn _ => let val consty as consty1=consty1
 ()
val ty_STAR_list as ty_STAR_list1=ty_STAR_list1 ()
 in ( consty::ty_STAR_list ) end
)
 in (LrTable.NT 71,(result,consty1left,ty_STAR_list1right),rest671)
 end
| (179,(_,(MlyValue.consty consty1,consty1left,consty1right))::rest671
) => let val result=MlyValue.ty_STAR_list(fn _ => let val consty as 
consty1=consty1 ()
 in ( consty::[] ) end
)
 in (LrTable.NT 71,(result,consty1left,consty1right),rest671) end
| (180,(_,(MlyValue.atty atty1,atty1left,atty1right))::rest671) => 
let val result=MlyValue.consty(fn _ => let val atty as atty1=atty1 ()
 in ( atty ) end
)
 in (LrTable.NT 72,(result,atty1left,atty1right),rest671) end
| (181,(_,(MlyValue.longtycon longtycon1,_,longtyconright as 
longtycon1right))::(_,(MlyValue.tyseq tyseq1,tyseqleft as tyseq1left,_
))::rest671) => let val result=MlyValue.consty(fn _ => let val tyseq
 as tyseq1=tyseq1 ()
val longtycon as longtycon1=longtycon1 ()
 in ( TYCONTy(I(tyseqleft,longtyconright),
                                          tyseq, longtycon) )
 end
)
 in (LrTable.NT 72,(result,tyseq1left,longtycon1right),rest671) end
| (182,(_,(MlyValue.tyvar tyvar1,tyvarleft as tyvar1left,tyvarright
 as tyvar1right))::rest671) => let val result=MlyValue.atty(fn _ => 
let val tyvar as tyvar1=tyvar1 ()
 in ( TYVARTy(I(tyvarleft,tyvarright), tyvar) ) end
)
 in (LrTable.NT 73,(result,tyvar1left,tyvar1right),rest671) end
| (183,(_,(_,_,RBRACEright as RBRACE1right))::(_,(MlyValue.tyrow_opt 
tyrow_opt1,_,_))::(_,(_,LBRACEleft as LBRACE1left,_))::rest671) => 
let val result=MlyValue.atty(fn _ => let val tyrow_opt as tyrow_opt1=
tyrow_opt1 ()
 in ( RECORDTy(I(LBRACEleft,RBRACEright), tyrow_opt) ) end
)
 in (LrTable.NT 73,(result,LBRACE1left,RBRACE1right),rest671) end
| (184,(_,(_,_,RPARright as RPAR1right))::(_,(MlyValue.ty ty1,_,_))::(
_,(_,LPARleft as LPAR1left,_))::rest671) => let val result=
MlyValue.atty(fn _ => let val ty as ty1=ty1 ()
 in ( PARTy(I(LPARleft,RPARright), ty) ) end
)
 in (LrTable.NT 73,(result,LPAR1left,RPAR1right),rest671) end
| (185,(_,(MlyValue.COMMA_tyrow_opt COMMA_tyrow_opt1,_,
COMMA_tyrow_optright as COMMA_tyrow_opt1right))::(_,(MlyValue.ty ty1,_
,_))::_::(_,(MlyValue.lab lab1,lableft as lab1left,_))::rest671) => 
let val result=MlyValue.tyrow(fn _ => let val lab as lab1=lab1 ()
val ty as ty1=ty1 ()
val COMMA_tyrow_opt as COMMA_tyrow_opt1=COMMA_tyrow_opt1 ()
 in (
 TyRow(I(lableft,COMMA_tyrow_optright),
                                lab, ty, COMMA_tyrow_opt) 
) end
)
 in (LrTable.NT 74,(result,lab1left,COMMA_tyrow_opt1right),rest671)
 end
| (186,(_,(MlyValue.tyrow tyrow1,_,tyrow1right))::(_,(_,COMMA1left,_))
::rest671) => let val result=MlyValue.COMMA_tyrow_opt(fn _ => let val 
tyrow as tyrow1=tyrow1 ()
 in ( SOME tyrow ) end
)
 in (LrTable.NT 76,(result,COMMA1left,tyrow1right),rest671) end
| (187,rest671) => let val result=MlyValue.COMMA_tyrow_opt(fn _ => (
 NONE ))
 in (LrTable.NT 76,(result,defaultPos,defaultPos),rest671) end
| (188,(_,(MlyValue.tyrow tyrow1,tyrow1left,tyrow1right))::rest671)
 => let val result=MlyValue.tyrow_opt(fn _ => let val tyrow as tyrow1=
tyrow1 ()
 in ( SOME tyrow ) end
)
 in (LrTable.NT 75,(result,tyrow1left,tyrow1right),rest671) end
| (189,rest671) => let val result=MlyValue.tyrow_opt(fn _ => ( NONE ))
 in (LrTable.NT 75,(result,defaultPos,defaultPos),rest671) end
| (190,(_,(MlyValue.consty consty1,constyleft as consty1left,
constyright as consty1right))::rest671) => let val result=
MlyValue.tyseq(fn _ => let val consty as consty1=consty1 ()
 in ( Tyseq(I(constyleft,constyright),
                                                [consty]) ) end
)
 in (LrTable.NT 77,(result,consty1left,consty1right),rest671) end
| (191,rest671) => let val result=MlyValue.tyseq(fn _ => (
 Tyseq(I(defaultPos,defaultPos), []) ))
 in (LrTable.NT 77,(result,defaultPos,defaultPos),rest671) end
| (192,(_,(_,_,RPARright as RPAR1right))::(_,(MlyValue.ty_COMMA_list2 
ty_COMMA_list21,_,_))::(_,(_,LPARleft as LPAR1left,_))::rest671) => 
let val result=MlyValue.tyseq(fn _ => let val ty_COMMA_list2 as 
ty_COMMA_list21=ty_COMMA_list21 ()
 in ( Tyseq(I(LPARleft,RPARright),
                                                ty_COMMA_list2) ) end
)
 in (LrTable.NT 77,(result,LPAR1left,RPAR1right),rest671) end
| (193,(_,(MlyValue.ty_COMMA_list2 ty_COMMA_list21,_,
ty_COMMA_list21right))::_::(_,(MlyValue.ty ty1,ty1left,_))::rest671)
 => let val result=MlyValue.ty_COMMA_list2(fn _ => let val ty as ty1=
ty1 ()
val ty_COMMA_list2 as ty_COMMA_list21=ty_COMMA_list21 ()
 in ( ty::ty_COMMA_list2 ) end
)
 in (LrTable.NT 78,(result,ty1left,ty_COMMA_list21right),rest671) end
| (194,(_,(MlyValue.ty ty2,_,ty2right))::_::(_,(MlyValue.ty ty1,
ty1left,_))::rest671) => let val result=MlyValue.ty_COMMA_list2(fn _
 => let val ty1=ty1 ()
val ty2=ty2 ()
 in ( [ty1, ty2] ) end
)
 in (LrTable.NT 78,(result,ty1left,ty2right),rest671) end
| (195,(_,(MlyValue.tyvarseq1 tyvarseq11,tyvarseq11left,
tyvarseq11right))::rest671) => let val result=MlyValue.tyvarseq(fn _
 => let val tyvarseq1 as tyvarseq11=tyvarseq11 ()
 in ( tyvarseq1 ) end
)
 in (LrTable.NT 79,(result,tyvarseq11left,tyvarseq11right),rest671)
 end
| (196,rest671) => let val result=MlyValue.tyvarseq(fn _ => (
 TyVarseq(I(defaultPos,defaultPos),
                                                   []) ))
 in (LrTable.NT 79,(result,defaultPos,defaultPos),rest671) end
| (197,(_,(MlyValue.tyvar tyvar1,tyvarleft as tyvar1left,tyvarright
 as tyvar1right))::rest671) => let val result=MlyValue.tyvarseq1(fn _
 => let val tyvar as tyvar1=tyvar1 ()
 in ( TyVarseq(I(tyvarleft,tyvarright),
                                                   [tyvar]) ) end
)
 in (LrTable.NT 80,(result,tyvar1left,tyvar1right),rest671) end
| (198,(_,(_,_,RPARright as RPAR1right))::(_,(
MlyValue.tyvar_COMMA_list1 tyvar_COMMA_list11,_,_))::(_,(_,LPARleft
 as LPAR1left,_))::rest671) => let val result=MlyValue.tyvarseq1(fn _
 => let val tyvar_COMMA_list1 as tyvar_COMMA_list11=tyvar_COMMA_list11
 ()
 in ( TyVarseq(I(LPARleft,RPARright),
                                                   tyvar_COMMA_list1) )
 end
)
 in (LrTable.NT 80,(result,LPAR1left,RPAR1right),rest671) end
| (199,(_,(MlyValue.tyvar_COMMA_list1 tyvar_COMMA_list11,_,
tyvar_COMMA_list11right))::_::(_,(MlyValue.tyvar tyvar1,tyvar1left,_))
::rest671) => let val result=MlyValue.tyvar_COMMA_list1(fn _ => let 
val tyvar as tyvar1=tyvar1 ()
val tyvar_COMMA_list1 as tyvar_COMMA_list11=tyvar_COMMA_list11 ()
 in ( tyvar::tyvar_COMMA_list1 ) end
)
 in (LrTable.NT 81,(result,tyvar1left,tyvar_COMMA_list11right),rest671
) end
| (200,(_,(MlyValue.tyvar tyvar1,tyvar1left,tyvar1right))::rest671)
 => let val result=MlyValue.tyvar_COMMA_list1(fn _ => let val tyvar
 as tyvar1=tyvar1 ()
 in ( tyvar::[] ) end
)
 in (LrTable.NT 81,(result,tyvar1left,tyvar1right),rest671) end
| (201,(_,(MlyValue.strexp' strexp'1,strexp'1left,strexp'1right))::
rest671) => let val result=MlyValue.strexp(fn _ => let val strexp' as 
strexp'1=strexp'1 ()
 in ( strexp' ) end
)
 in (LrTable.NT 82,(result,strexp'1left,strexp'1right),rest671) end
| (202,(_,(MlyValue.sigexp sigexp1,_,sigexpright as sigexp1right))::_
::(_,(MlyValue.strexp strexp1,strexpleft as strexp1left,_))::rest671)
 => let val result=MlyValue.strexp(fn _ => let val strexp as strexp1=
strexp1 ()
val sigexp as sigexp1=sigexp1 ()
 in (
 TRANSStrExp(I(strexpleft,sigexpright),
                                      strexp, sigexp) )
 end
)
 in (LrTable.NT 82,(result,strexp1left,sigexp1right),rest671) end
| (203,(_,(MlyValue.sigexp sigexp1,_,sigexpright as sigexp1right))::_
::(_,(MlyValue.strexp strexp1,strexpleft as strexp1left,_))::rest671)
 => let val result=MlyValue.strexp(fn _ => let val strexp as strexp1=
strexp1 ()
val sigexp as sigexp1=sigexp1 ()
 in ( OPAQStrExp(I(strexpleft,sigexpright), strexp, sigexp)) end
)
 in (LrTable.NT 82,(result,strexp1left,sigexp1right),rest671) end
| (204,(_,(_,_,ENDright as END1right))::(_,(MlyValue.popInfix 
popInfix1,_,_))::(_,(MlyValue.strdec strdec1,_,_))::(_,(
MlyValue.pushInfix pushInfix1,_,_))::(_,(_,STRUCTleft as STRUCT1left,_
))::rest671) => let val result=MlyValue.strexp'(fn _ => let val 
pushInfix1=pushInfix1 ()
val strdec as strdec1=strdec1 ()
val popInfix1=popInfix1 ()
 in ( STRUCTStrExp(I(STRUCTleft,ENDright), strdec) ) end
)
 in (LrTable.NT 83,(result,STRUCT1left,END1right),rest671) end
| (205,(_,(MlyValue.longstrid longstrid1,longstridleft as 
longstrid1left,longstridright as longstrid1right))::rest671) => let 
val result=MlyValue.strexp'(fn _ => let val longstrid as longstrid1=
longstrid1 ()
 in (
 LONGSTRIDStrExp(I(longstridleft,longstridright),
                                          longstrid) )
 end
)
 in (LrTable.NT 83,(result,longstrid1left,longstrid1right),rest671)
 end
| (206,(_,(_,_,RPARright as RPAR1right))::(_,(MlyValue.strexp strexp1,
_,_))::_::(_,(MlyValue.funid funid1,funidleft as funid1left,_))::
rest671) => let val result=MlyValue.strexp'(fn _ => let val funid as 
funid1=funid1 ()
val strexp as strexp1=strexp1 ()
 in ( APPStrExp(I(funidleft,RPARright), funid, strexp) ) end
)
 in (LrTable.NT 83,(result,funid1left,RPAR1right),rest671) end
| (207,(_,(_,_,RPARright as RPAR1right))::(_,(MlyValue.strdec strdec1,
_,_))::_::(_,(MlyValue.funid funid1,funidleft as funid1left,_))::
rest671) => let val result=MlyValue.strexp'(fn _ => let val funid as 
funid1=funid1 ()
val strdec as strdec1=strdec1 ()
 in ( APPDECStrExp(I(funidleft,RPARright), funid, strdec) ) end
)
 in (LrTable.NT 83,(result,funid1left,RPAR1right),rest671) end
| (208,(_,(_,_,ENDright as END1right))::(_,(MlyValue.popInfix 
popInfix1,_,_))::(_,(MlyValue.strexp strexp1,_,_))::_::(_,(
MlyValue.strdec strdec1,_,_))::(_,(MlyValue.pushInfix pushInfix1,_,_))
::(_,(_,LETleft as LET1left,_))::rest671) => let val result=
MlyValue.strexp'(fn _ => let val pushInfix1=pushInfix1 ()
val strdec as strdec1=strdec1 ()
val strexp as strexp1=strexp1 ()
val popInfix1=popInfix1 ()
 in ( LETStrExp(I(LETleft,ENDright), strdec, strexp) ) end
)
 in (LrTable.NT 83,(result,LET1left,END1right),rest671) end
| (209,(_,(MlyValue.strdec1 strdec11,strdec11left,strdec11right))::
rest671) => let val result=MlyValue.strdec(fn _ => let val strdec1 as 
strdec11=strdec11 ()
 in ( strdec1 ) end
)
 in (LrTable.NT 84,(result,strdec11left,strdec11right),rest671) end
| (210,rest671) => let val result=MlyValue.strdec(fn _ => (
 EMPTYStrDec(I(defaultPos,defaultPos)) ))
 in (LrTable.NT 84,(result,defaultPos,defaultPos),rest671) end
| (211,(_,(MlyValue.strdec1' strdec1'1,strdec1'1left,strdec1'1right))
::rest671) => let val result=MlyValue.strdec1(fn _ => let val strdec1'
 as strdec1'1=strdec1'1 ()
 in ( strdec1' ) end
)
 in (LrTable.NT 85,(result,strdec1'1left,strdec1'1right),rest671) end
| (212,(_,(MlyValue.strdec1 strdec12,_,strdec12right))::(_,(
MlyValue.strdec1 strdec11,strdec11left,_))::rest671) => let val result
=MlyValue.strdec1(fn _ => let val strdec11=strdec11 ()
val strdec12=strdec12 ()
 in (
 SEQStrDec(I(strdec11left,strdec12right),
                                    strdec11, strdec12) 
) end
)
 in (LrTable.NT 85,(result,strdec11left,strdec12right),rest671) end
| (213,(_,(_,SEMICOLONleft as SEMICOLON1left,SEMICOLONright as 
SEMICOLON1right))::rest671) => let val result=MlyValue.strdec1(fn _
 => ( EMPTYStrDec(I(SEMICOLONleft,SEMICOLONright)) ))
 in (LrTable.NT 85,(result,SEMICOLON1left,SEMICOLON1right),rest671)
 end
| (214,(_,(MlyValue.dec1' dec1'1,dec1'left as dec1'1left,dec1'right
 as dec1'1right))::rest671) => let val result=MlyValue.strdec1'(fn _
 => let val dec1' as dec1'1=dec1'1 ()
 in ( DECStrDec(I(dec1'left,dec1'right), dec1') ) end
)
 in (LrTable.NT 86,(result,dec1'1left,dec1'1right),rest671) end
| (215,(_,(MlyValue.strbind strbind1,_,strbindright as strbind1right))
::(_,(_,STRUCTUREleft as STRUCTURE1left,_))::rest671) => let val 
result=MlyValue.strdec1'(fn _ => let val strbind as strbind1=strbind1 
()
 in ( STRUCTUREStrDec(I(STRUCTUREleft,strbindright),
                                          strbind) )
 end
)
 in (LrTable.NT 86,(result,STRUCTURE1left,strbind1right),rest671) end
| (216,(_,(_,_,ENDright as END1right))::(_,(MlyValue.popLocalInfix 
popLocalInfix1,_,_))::(_,(MlyValue.strdec strdec2,_,_))::(_,(
MlyValue.pushLocalInfix pushLocalInfix1,_,_))::_::(_,(MlyValue.strdec 
strdec1,_,_))::(_,(MlyValue.pushInfix pushInfix1,_,_))::(_,(_,
LOCALleft as LOCAL1left,_))::rest671) => let val result=
MlyValue.strdec1'(fn _ => let val pushInfix1=pushInfix1 ()
val strdec1=strdec1 ()
val pushLocalInfix1=pushLocalInfix1 ()
val strdec2=strdec2 ()
val popLocalInfix1=popLocalInfix1 ()
 in ( LOCALStrDec(I(LOCALleft,ENDright), strdec1, strdec2) ) end
)
 in (LrTable.NT 86,(result,LOCAL1left,END1right),rest671) end
| (217,(_,(MlyValue.strexp__AND_strbind_opt strexp__AND_strbind_opt1,_
,strexp__AND_strbind_optright as strexp__AND_strbind_opt1right))::_::(
_,(MlyValue.COLON_sigexp_opt COLON_sigexp_opt1,_,_))::(_,(
MlyValue.strid strid1,stridleft as strid1left,_))::rest671) => let 
val result=MlyValue.strbind(fn _ => let val strid as strid1=strid1 ()
val COLON_sigexp_opt as COLON_sigexp_opt1=COLON_sigexp_opt1 ()
val strexp__AND_strbind_opt as strexp__AND_strbind_opt1=
strexp__AND_strbind_opt1 ()
 in (
 TRANSStrBind(I(stridleft,
                                         strexp__AND_strbind_optright),
                                       strid, COLON_sigexp_opt,
                                       #1 strexp__AND_strbind_opt,
                                       #2 strexp__AND_strbind_opt) 
) end
)
 in (LrTable.NT 87,(result,strid1left,strexp__AND_strbind_opt1right),
rest671) end
| (218,(_,(MlyValue.strexp__AND_strbind_opt strexp__AND_strbind_opt1,_
,strexp__AND_strbind_optright as strexp__AND_strbind_opt1right))::_::(
_,(MlyValue.sigexp sigexp1,_,_))::_::(_,(MlyValue.strid strid1,
stridleft as strid1left,_))::rest671) => let val result=
MlyValue.strbind(fn _ => let val strid as strid1=strid1 ()
val sigexp as sigexp1=sigexp1 ()
val strexp__AND_strbind_opt as strexp__AND_strbind_opt1=
strexp__AND_strbind_opt1 ()
 in (
 OPAQStrBind(I(stridleft,strexp__AND_strbind_optright),
                                      strid, sigexp, #1 strexp__AND_strbind_opt,
                                      #2 strexp__AND_strbind_opt) 
) end
)
 in (LrTable.NT 87,(result,strid1left,strexp__AND_strbind_opt1right),
rest671) end
| (219,(_,(MlyValue.strbind strbind1,_,strbind1right))::(_,(_,AND1left
,_))::rest671) => let val result=MlyValue.AND_strbind_opt(fn _ => let 
val strbind as strbind1=strbind1 ()
 in ( SOME strbind ) end
)
 in (LrTable.NT 88,(result,AND1left,strbind1right),rest671) end
| (220,rest671) => let val result=MlyValue.AND_strbind_opt(fn _ => (
 NONE ))
 in (LrTable.NT 88,(result,defaultPos,defaultPos),rest671) end
| (221,(_,(MlyValue.AND_strbind_opt AND_strbind_opt1,_,
AND_strbind_opt1right))::(_,(MlyValue.strexp' strexp'1,strexp'1left,_)
)::rest671) => let val result=MlyValue.strexp__AND_strbind_opt(fn _
 => let val strexp' as strexp'1=strexp'1 ()
val AND_strbind_opt as AND_strbind_opt1=AND_strbind_opt1 ()
 in ( ( strexp', AND_strbind_opt ) ) end
)
 in (LrTable.NT 89,(result,strexp'1left,AND_strbind_opt1right),rest671
) end
| (222,(_,(MlyValue.sigexp__AND_strbind_opt sigexp__AND_strbind_opt1,_
,sigexp__AND_strbind_optright as sigexp__AND_strbind_opt1right))::_::(
_,(MlyValue.strexp strexp1,strexpleft as strexp1left,_))::rest671) => 
let val result=MlyValue.strexp__AND_strbind_opt(fn _ => let val strexp
 as strexp1=strexp1 ()
val sigexp__AND_strbind_opt as sigexp__AND_strbind_opt1=
sigexp__AND_strbind_opt1 ()
 in (
 ( TRANSStrExp(I(strexpleft,
                                          sigexp__AND_strbind_optright),
                                        strexp, #1 sigexp__AND_strbind_opt),
                            #2 sigexp__AND_strbind_opt ) 
) end
)
 in (LrTable.NT 89,(result,strexp1left,sigexp__AND_strbind_opt1right),
rest671) end
| (223,(_,(MlyValue.sigexp__AND_strbind_opt sigexp__AND_strbind_opt1,_
,sigexp__AND_strbind_optright as sigexp__AND_strbind_opt1right))::_::(
_,(MlyValue.strexp strexp1,strexpleft as strexp1left,_))::rest671) => 
let val result=MlyValue.strexp__AND_strbind_opt(fn _ => let val strexp
 as strexp1=strexp1 ()
val sigexp__AND_strbind_opt as sigexp__AND_strbind_opt1=
sigexp__AND_strbind_opt1 ()
 in (
 ( OPAQStrExp(I(strexpleft,
                                         sigexp__AND_strbind_optright),
                                       strexp, #1 sigexp__AND_strbind_opt),
                            #2 sigexp__AND_strbind_opt ) 
) end
)
 in (LrTable.NT 89,(result,strexp1left,sigexp__AND_strbind_opt1right),
rest671) end
| (224,(_,(MlyValue.AND_strbind_opt AND_strbind_opt1,_,
AND_strbind_opt1right))::(_,(MlyValue.sigexp' sigexp'1,sigexp'1left,_)
)::rest671) => let val result=MlyValue.sigexp__AND_strbind_opt(fn _
 => let val sigexp' as sigexp'1=sigexp'1 ()
val AND_strbind_opt as AND_strbind_opt1=AND_strbind_opt1 ()
 in ( ( sigexp', AND_strbind_opt ) ) end
)
 in (LrTable.NT 90,(result,sigexp'1left,AND_strbind_opt1right),rest671
) end
| (225,(_,(MlyValue.tyreadesc__AND_strbind_opt 
tyreadesc__AND_strbind_opt1,_,tyreadesc__AND_strbind_optright as 
tyreadesc__AND_strbind_opt1right))::_::(_,(MlyValue.sigexp sigexp1,
sigexpleft as sigexp1left,_))::rest671) => let val result=
MlyValue.sigexp__AND_strbind_opt(fn _ => let val sigexp as sigexp1=
sigexp1 ()
val tyreadesc__AND_strbind_opt as tyreadesc__AND_strbind_opt1=
tyreadesc__AND_strbind_opt1 ()
 in (
 ( WHERETYPESigExp(I(sigexpleft,
                                              tyreadesc__AND_strbind_optright),
                                           sigexp,
                                           #1 tyreadesc__AND_strbind_opt),
                            #2 tyreadesc__AND_strbind_opt ) 
) end
)
 in (LrTable.NT 90,(result,sigexp1left,
tyreadesc__AND_strbind_opt1right),rest671) end
| (226,(_,(MlyValue.AND_tyreadesc_opt__AND_strbind_opt 
AND_tyreadesc_opt__AND_strbind_opt1,_,
AND_tyreadesc_opt__AND_strbind_optright as 
AND_tyreadesc_opt__AND_strbind_opt1right))::(_,(MlyValue.ty ty1,_,_))
::_::(_,(MlyValue.longtycon longtycon1,_,_))::(_,(MlyValue.tyvarseq 
tyvarseq1,_,_))::(_,(_,TYPEleft as TYPE1left,_))::rest671) => let val 
result=MlyValue.tyreadesc__AND_strbind_opt(fn _ => let val tyvarseq
 as tyvarseq1=tyvarseq1 ()
val longtycon as longtycon1=longtycon1 ()
val ty as ty1=ty1 ()
val AND_tyreadesc_opt__AND_strbind_opt as 
AND_tyreadesc_opt__AND_strbind_opt1=
AND_tyreadesc_opt__AND_strbind_opt1 ()
 in (
 ( TyReaDesc(I(TYPEleft,
                                       AND_tyreadesc_opt__AND_strbind_optright),
                                      tyvarseq, longtycon, ty,
                                      #1 AND_tyreadesc_opt__AND_strbind_opt),
                            #2 AND_tyreadesc_opt__AND_strbind_opt ) 
) end
)
 in (LrTable.NT 91,(result,TYPE1left,
AND_tyreadesc_opt__AND_strbind_opt1right),rest671) end
| (227,(_,(MlyValue.AND_strbind_opt AND_strbind_opt1,
AND_strbind_opt1left,AND_strbind_opt1right))::rest671) => let val 
result=MlyValue.AND_tyreadesc_opt__AND_strbind_opt(fn _ => let val 
AND_strbind_opt as AND_strbind_opt1=AND_strbind_opt1 ()
 in ( ( NONE, AND_strbind_opt ) ) end
)
 in (LrTable.NT 92,(result,AND_strbind_opt1left,AND_strbind_opt1right)
,rest671) end
| (228,(_,(MlyValue.tyreadesc__AND_strbind_opt 
tyreadesc__AND_strbind_opt1,_,tyreadesc__AND_strbind_opt1right))::(_,(
_,AND1left,_))::rest671) => let val result=
MlyValue.AND_tyreadesc_opt__AND_strbind_opt(fn _ => let val 
tyreadesc__AND_strbind_opt as tyreadesc__AND_strbind_opt1=
tyreadesc__AND_strbind_opt1 ()
 in (
 ( SOME(#1 tyreadesc__AND_strbind_opt),
                                    #2 tyreadesc__AND_strbind_opt ) 
) end
)
 in (LrTable.NT 92,(result,AND1left,tyreadesc__AND_strbind_opt1right),
rest671) end
| (229,(_,(MlyValue.sigexp sigexp1,_,sigexp1right))::(_,(_,COLON1left,
_))::rest671) => let val result=MlyValue.COLON_sigexp_opt(fn _ => let 
val sigexp as sigexp1=sigexp1 ()
 in ( SOME sigexp ) end
)
 in (LrTable.NT 93,(result,COLON1left,sigexp1right),rest671) end
| (230,rest671) => let val result=MlyValue.COLON_sigexp_opt(fn _ => (
 NONE ))
 in (LrTable.NT 93,(result,defaultPos,defaultPos),rest671) end
| (231,(_,(MlyValue.sigexp' sigexp'1,sigexp'1left,sigexp'1right))::
rest671) => let val result=MlyValue.sigexp(fn _ => let val sigexp' as 
sigexp'1=sigexp'1 ()
 in ( sigexp' ) end
)
 in (LrTable.NT 94,(result,sigexp'1left,sigexp'1right),rest671) end
| (232,(_,(MlyValue.tyreadesc tyreadesc1,_,tyreadescright as 
tyreadesc1right))::_::(_,(MlyValue.sigexp sigexp1,sigexpleft as 
sigexp1left,_))::rest671) => let val result=MlyValue.sigexp(fn _ => 
let val sigexp as sigexp1=sigexp1 ()
val tyreadesc as tyreadesc1=tyreadesc1 ()
 in (
 WHERETYPESigExp(I(sigexpleft,tyreadescright),
                                          sigexp, tyreadesc) 
) end
)
 in (LrTable.NT 94,(result,sigexp1left,tyreadesc1right),rest671) end
| (233,(_,(_,_,ENDright as END1right))::(_,(MlyValue.spec spec1,_,_))
::(_,(_,SIGleft as SIG1left,_))::rest671) => let val result=
MlyValue.sigexp'(fn _ => let val spec as spec1=spec1 ()
 in ( SIGSigExp(I(SIGleft,ENDright), spec) ) end
)
 in (LrTable.NT 95,(result,SIG1left,END1right),rest671) end
| (234,(_,(MlyValue.sigid sigid1,sigidleft as sigid1left,sigidright
 as sigid1right))::rest671) => let val result=MlyValue.sigexp'(fn _
 => let val sigid as sigid1=sigid1 ()
 in ( SIGIDSigExp(I(sigidleft,sigidright), sigid) ) end
)
 in (LrTable.NT 95,(result,sigid1left,sigid1right),rest671) end
| (235,(_,(MlyValue.sigbind sigbind1,_,sigbindright as sigbind1right))
::(_,(_,SIGNATUREleft as SIGNATURE1left,_))::rest671) => let val 
result=MlyValue.sigdec(fn _ => let val sigbind as sigbind1=sigbind1 ()
 in ( SigDec(I(SIGNATUREleft,sigbindright), sigbind) ) end
)
 in (LrTable.NT 96,(result,SIGNATURE1left,sigbind1right),rest671) end
| (236,(_,(MlyValue.sigexp__AND_sigbind_opt sigexp__AND_sigbind_opt1,_
,sigexp__AND_sigbind_optright as sigexp__AND_sigbind_opt1right))::_::(
_,(MlyValue.sigid sigid1,sigidleft as sigid1left,_))::rest671) => let 
val result=MlyValue.sigbind(fn _ => let val sigid as sigid1=sigid1 ()
val sigexp__AND_sigbind_opt as sigexp__AND_sigbind_opt1=
sigexp__AND_sigbind_opt1 ()
 in (
 SigBind(I(sigidleft,sigexp__AND_sigbind_optright),
                                  sigid, #1 sigexp__AND_sigbind_opt,
                                  #2 sigexp__AND_sigbind_opt) 
) end
)
 in (LrTable.NT 97,(result,sigid1left,sigexp__AND_sigbind_opt1right),
rest671) end
| (237,(_,(MlyValue.sigbind sigbind1,_,sigbind1right))::(_,(_,AND1left
,_))::rest671) => let val result=MlyValue.AND_sigbind_opt(fn _ => let 
val sigbind as sigbind1=sigbind1 ()
 in ( SOME sigbind ) end
)
 in (LrTable.NT 98,(result,AND1left,sigbind1right),rest671) end
| (238,rest671) => let val result=MlyValue.AND_sigbind_opt(fn _ => (
 NONE ))
 in (LrTable.NT 98,(result,defaultPos,defaultPos),rest671) end
| (239,(_,(MlyValue.AND_sigbind_opt AND_sigbind_opt1,_,
AND_sigbind_opt1right))::(_,(MlyValue.sigexp' sigexp'1,sigexp'1left,_)
)::rest671) => let val result=MlyValue.sigexp__AND_sigbind_opt(fn _
 => let val sigexp' as sigexp'1=sigexp'1 ()
val AND_sigbind_opt as AND_sigbind_opt1=AND_sigbind_opt1 ()
 in ( ( sigexp', AND_sigbind_opt ) ) end
)
 in (LrTable.NT 99,(result,sigexp'1left,AND_sigbind_opt1right),rest671
) end
| (240,(_,(MlyValue.tyreadesc__AND_sigbind_opt 
tyreadesc__AND_sigbind_opt1,_,tyreadesc__AND_sigbind_optright as 
tyreadesc__AND_sigbind_opt1right))::_::(_,(MlyValue.sigexp sigexp1,
sigexpleft as sigexp1left,_))::rest671) => let val result=
MlyValue.sigexp__AND_sigbind_opt(fn _ => let val sigexp as sigexp1=
sigexp1 ()
val tyreadesc__AND_sigbind_opt as tyreadesc__AND_sigbind_opt1=
tyreadesc__AND_sigbind_opt1 ()
 in (
 ( WHERETYPESigExp(I(sigexpleft,
                                              tyreadesc__AND_sigbind_optright),
                                           sigexp,
                                           #1 tyreadesc__AND_sigbind_opt),
                            #2 tyreadesc__AND_sigbind_opt ) 
) end
)
 in (LrTable.NT 99,(result,sigexp1left,
tyreadesc__AND_sigbind_opt1right),rest671) end
| (241,(_,(MlyValue.AND_tyreadesc_opt__AND_sigbind_opt 
AND_tyreadesc_opt__AND_sigbind_opt1,_,
AND_tyreadesc_opt__AND_sigbind_optright as 
AND_tyreadesc_opt__AND_sigbind_opt1right))::(_,(MlyValue.ty ty1,_,_))
::_::(_,(MlyValue.longtycon longtycon1,_,_))::(_,(MlyValue.tyvarseq 
tyvarseq1,_,_))::(_,(_,TYPEleft as TYPE1left,_))::rest671) => let val 
result=MlyValue.tyreadesc__AND_sigbind_opt(fn _ => let val tyvarseq
 as tyvarseq1=tyvarseq1 ()
val longtycon as longtycon1=longtycon1 ()
val ty as ty1=ty1 ()
val AND_tyreadesc_opt__AND_sigbind_opt as 
AND_tyreadesc_opt__AND_sigbind_opt1=
AND_tyreadesc_opt__AND_sigbind_opt1 ()
 in (
 ( TyReaDesc(I(TYPEleft,
                                       AND_tyreadesc_opt__AND_sigbind_optright),
                                      tyvarseq, longtycon, ty,
                                      #1 AND_tyreadesc_opt__AND_sigbind_opt),
                            #2 AND_tyreadesc_opt__AND_sigbind_opt ) 
) end
)
 in (LrTable.NT 100,(result,TYPE1left,
AND_tyreadesc_opt__AND_sigbind_opt1right),rest671) end
| (242,(_,(MlyValue.AND_sigbind_opt AND_sigbind_opt1,
AND_sigbind_opt1left,AND_sigbind_opt1right))::rest671) => let val 
result=MlyValue.AND_tyreadesc_opt__AND_sigbind_opt(fn _ => let val 
AND_sigbind_opt as AND_sigbind_opt1=AND_sigbind_opt1 ()
 in ( ( NONE, AND_sigbind_opt) ) end
)
 in (LrTable.NT 101,(result,AND_sigbind_opt1left,AND_sigbind_opt1right
),rest671) end
| (243,(_,(MlyValue.tyreadesc__AND_sigbind_opt 
tyreadesc__AND_sigbind_opt1,_,tyreadesc__AND_sigbind_opt1right))::(_,(
_,AND1left,_))::rest671) => let val result=
MlyValue.AND_tyreadesc_opt__AND_sigbind_opt(fn _ => let val 
tyreadesc__AND_sigbind_opt as tyreadesc__AND_sigbind_opt1=
tyreadesc__AND_sigbind_opt1 ()
 in (
 ( SOME(#1 tyreadesc__AND_sigbind_opt),
                                    #2 tyreadesc__AND_sigbind_opt ) 
) end
)
 in (LrTable.NT 101,(result,AND1left,tyreadesc__AND_sigbind_opt1right)
,rest671) end
| (244,(_,(MlyValue.AND_tyreadesc_opt AND_tyreadesc_opt1,_,
AND_tyreadesc_optright as AND_tyreadesc_opt1right))::(_,(MlyValue.ty 
ty1,_,_))::_::(_,(MlyValue.longtycon longtycon1,_,_))::(_,(
MlyValue.tyvarseq tyvarseq1,_,_))::(_,(_,TYPEleft as TYPE1left,_))::
rest671) => let val result=MlyValue.tyreadesc(fn _ => let val tyvarseq
 as tyvarseq1=tyvarseq1 ()
val longtycon as longtycon1=longtycon1 ()
val ty as ty1=ty1 ()
val AND_tyreadesc_opt as AND_tyreadesc_opt1=AND_tyreadesc_opt1 ()
 in (
 TyReaDesc(I(TYPEleft,AND_tyreadesc_optright),
                                    tyvarseq, longtycon, ty,
                                    AND_tyreadesc_opt) 
) end
)
 in (LrTable.NT 102,(result,TYPE1left,AND_tyreadesc_opt1right),rest671
) end
| (245,(_,(MlyValue.tyreadesc tyreadesc1,_,tyreadesc1right))::(_,(_,
AND1left,_))::rest671) => let val result=MlyValue.AND_tyreadesc_opt(
fn _ => let val tyreadesc as tyreadesc1=tyreadesc1 ()
 in ( SOME tyreadesc ) end
)
 in (LrTable.NT 103,(result,AND1left,tyreadesc1right),rest671) end
| (246,rest671) => let val result=MlyValue.AND_tyreadesc_opt(fn _ => (
 NONE ))
 in (LrTable.NT 103,(result,defaultPos,defaultPos),rest671) end
| (247,(_,(MlyValue.spec1 spec11,spec11left,spec11right))::rest671)
 => let val result=MlyValue.spec(fn _ => let val spec1 as spec11=
spec11 ()
 in ( spec1 ) end
)
 in (LrTable.NT 104,(result,spec11left,spec11right),rest671) end
| (248,rest671) => let val result=MlyValue.spec(fn _ => (
 EMPTYSpec(I(defaultPos,defaultPos)) ))
 in (LrTable.NT 104,(result,defaultPos,defaultPos),rest671) end
| (249,(_,(MlyValue.spec1' spec1'1,spec1'1left,spec1'1right))::rest671
) => let val result=MlyValue.spec1(fn _ => let val spec1' as spec1'1=
spec1'1 ()
 in ( spec1' ) end
)
 in (LrTable.NT 105,(result,spec1'1left,spec1'1right),rest671) end
| (250,(_,(MlyValue.spec1' spec1'1,_,spec1'right as spec1'1right))::(_
,(MlyValue.spec1 spec11,spec1left as spec11left,_))::rest671) => let 
val result=MlyValue.spec1(fn _ => let val spec1 as spec11=spec11 ()
val spec1' as spec1'1=spec1'1 ()
 in ( SEQSpec(I(spec1left,spec1'right), spec1, spec1') ) end
)
 in (LrTable.NT 105,(result,spec11left,spec1'1right),rest671) end
| (251,(_,(_,SEMICOLON1left,SEMICOLON1right))::rest671) => let val 
result=MlyValue.spec1(fn _ => ( EMPTYSpec(I(defaultPos,defaultPos)) ))
 in (LrTable.NT 105,(result,SEMICOLON1left,SEMICOLON1right),rest671)
 end
| (252,(_,(MlyValue.longtycon_EQUALS_list2 longtycon_EQUALS_list21,_,
longtycon_EQUALS_list2right as longtycon_EQUALS_list21right))::_::(_,(
_,SHARINGleft as SHARING1left,_))::rest671) => let val result=
MlyValue.spec1(fn _ => let val longtycon_EQUALS_list2 as 
longtycon_EQUALS_list21=longtycon_EQUALS_list21 ()
 in (
 SHARINGTYPESpec(I(SHARINGleft,
                                            longtycon_EQUALS_list2right),
                                          EMPTYSpec(I(SHARINGleft,SHARINGleft)),
                                          longtycon_EQUALS_list2) 
) end
)
 in (LrTable.NT 105,(result,SHARING1left,longtycon_EQUALS_list21right)
,rest671) end
| (253,(_,(MlyValue.longtycon_EQUALS_list2 longtycon_EQUALS_list21,_,
longtycon_EQUALS_list2right as longtycon_EQUALS_list21right))::_::_::(
_,(MlyValue.spec1 spec11,spec1left as spec11left,_))::rest671) => let 
val result=MlyValue.spec1(fn _ => let val spec1 as spec11=spec11 ()
val longtycon_EQUALS_list2 as longtycon_EQUALS_list21=
longtycon_EQUALS_list21 ()
 in (
 SHARINGTYPESpec(I(spec1left,
                                            longtycon_EQUALS_list2right),
                                          spec1, longtycon_EQUALS_list2) 
) end
)
 in (LrTable.NT 105,(result,spec11left,longtycon_EQUALS_list21right),
rest671) end
| (254,(_,(MlyValue.longstrid_EQUALS_list2 longstrid_EQUALS_list21,_,
longstrid_EQUALS_list2right as longstrid_EQUALS_list21right))::(_,(_,
SHARINGleft as SHARING1left,_))::rest671) => let val result=
MlyValue.spec1(fn _ => let val longstrid_EQUALS_list2 as 
longstrid_EQUALS_list21=longstrid_EQUALS_list21 ()
 in (
 SHARINGSpec(I(SHARINGleft,
                                        longstrid_EQUALS_list2right),
                                      EMPTYSpec(I(SHARINGleft,SHARINGleft)),
                                      longstrid_EQUALS_list2) 
) end
)
 in (LrTable.NT 105,(result,SHARING1left,longstrid_EQUALS_list21right)
,rest671) end
| (255,(_,(MlyValue.longstrid_EQUALS_list2 longstrid_EQUALS_list21,_,
longstrid_EQUALS_list2right as longstrid_EQUALS_list21right))::_::(_,(
MlyValue.spec1 spec11,spec1left as spec11left,_))::rest671) => let 
val result=MlyValue.spec1(fn _ => let val spec1 as spec11=spec11 ()
val longstrid_EQUALS_list2 as longstrid_EQUALS_list21=
longstrid_EQUALS_list21 ()
 in (
 SHARINGSpec(I(spec1left,longstrid_EQUALS_list2right),
                                      spec1, longstrid_EQUALS_list2) 
) end
)
 in (LrTable.NT 105,(result,spec11left,longstrid_EQUALS_list21right),
rest671) end
| (256,(_,(MlyValue.valdesc valdesc1,_,valdescright as valdesc1right))
::(_,(_,VALleft as VAL1left,_))::rest671) => let val result=
MlyValue.spec1'(fn _ => let val valdesc as valdesc1=valdesc1 ()
 in ( VALSpec(I(VALleft,valdescright), valdesc) ) end
)
 in (LrTable.NT 106,(result,VAL1left,valdesc1right),rest671) end
| (257,(_,(MlyValue.typdesc typdesc1,_,typdescright as typdesc1right))
::(_,(_,TYPEleft as TYPE1left,_))::rest671) => let val result=
MlyValue.spec1'(fn _ => let val typdesc as typdesc1=typdesc1 ()
 in ( TYPESpec(I(TYPEleft,typdescright), typdesc) ) end
)
 in (LrTable.NT 106,(result,TYPE1left,typdesc1right),rest671) end
| (258,(_,(MlyValue.typdesc typdesc1,_,typdescright as typdesc1right))
::(_,(_,EQTYPEleft as EQTYPE1left,_))::rest671) => let val result=
MlyValue.spec1'(fn _ => let val typdesc as typdesc1=typdesc1 ()
 in ( EQTYPESpec(I(EQTYPEleft,typdescright), typdesc) ) end
)
 in (LrTable.NT 106,(result,EQTYPE1left,typdesc1right),rest671) end
| (259,(_,(MlyValue.syndesc syndesc1,_,syndescright as syndesc1right))
::(_,(_,TYPEleft as TYPE1left,_))::rest671) => let val result=
MlyValue.spec1'(fn _ => let val syndesc as syndesc1=syndesc1 ()
 in ( SYNSpec(I(TYPEleft,syndescright), syndesc) ) end
)
 in (LrTable.NT 106,(result,TYPE1left,syndesc1right),rest671) end
| (260,(_,(MlyValue.datdesc0 datdesc01,_,datdesc0right as 
datdesc01right))::(_,(_,DATATYPEleft as DATATYPE1left,_))::rest671)
 => let val result=MlyValue.spec1'(fn _ => let val datdesc0 as 
datdesc01=datdesc01 ()
 in ( DATATYPESpec(I(DATATYPEleft,datdesc0right), datdesc0)) end
)
 in (LrTable.NT 106,(result,DATATYPE1left,datdesc01right),rest671) end
| (261,(_,(MlyValue.datdesc1 datdesc11,_,datdesc1right as 
datdesc11right))::(_,(_,DATATYPEleft as DATATYPE1left,_))::rest671)
 => let val result=MlyValue.spec1'(fn _ => let val datdesc1 as 
datdesc11=datdesc11 ()
 in ( DATATYPESpec(I(DATATYPEleft,datdesc1right), datdesc1)) end
)
 in (LrTable.NT 106,(result,DATATYPE1left,datdesc11right),rest671) end
| (262,(_,(MlyValue.longtycon longtycon1,_,longtyconright as 
longtycon1right))::_::_::(_,(MlyValue.tycon tycon1,_,_))::(_,(_,
DATATYPEleft as DATATYPE1left,_))::rest671) => let val result=
MlyValue.spec1'(fn _ => let val tycon as tycon1=tycon1 ()
val longtycon as longtycon1=longtycon1 ()
 in (
 REPLICATIONSpec(I(DATATYPEleft,longtyconright),
                                          tycon, longtycon) 
) end
)
 in (LrTable.NT 106,(result,DATATYPE1left,longtycon1right),rest671)
 end
| (263,(_,(MlyValue.exdesc exdesc1,_,exdescright as exdesc1right))::(_
,(_,EXCEPTIONleft as EXCEPTION1left,_))::rest671) => let val result=
MlyValue.spec1'(fn _ => let val exdesc as exdesc1=exdesc1 ()
 in ( EXCEPTIONSpec(I(EXCEPTIONleft,exdescright), exdesc) ) end
)
 in (LrTable.NT 106,(result,EXCEPTION1left,exdesc1right),rest671) end
| (264,(_,(MlyValue.strdesc strdesc1,_,strdescright as strdesc1right))
::(_,(_,STRUCTUREleft as STRUCTURE1left,_))::rest671) => let val 
result=MlyValue.spec1'(fn _ => let val strdesc as strdesc1=strdesc1 ()
 in ( STRUCTURESpec(I(STRUCTUREleft,strdescright), strdesc)) end
)
 in (LrTable.NT 106,(result,STRUCTURE1left,strdesc1right),rest671) end
| (265,(_,(MlyValue.sigexp sigexp1,_,sigexpright as sigexp1right))::(_
,(_,INCLUDEleft as INCLUDE1left,_))::rest671) => let val result=
MlyValue.spec1'(fn _ => let val sigexp as sigexp1=sigexp1 ()
 in ( INCLUDESpec(I(INCLUDEleft,sigexpright), sigexp) ) end
)
 in (LrTable.NT 106,(result,INCLUDE1left,sigexp1right),rest671) end
| (266,(_,(MlyValue.sigid_list2 sigid_list21,_,sigid_list2right as 
sigid_list21right))::(_,(_,INCLUDEleft as INCLUDE1left,_))::rest671)
 => let val result=MlyValue.spec1'(fn _ => let val sigid_list2 as 
sigid_list21=sigid_list21 ()
 in (
 INCLUDEMULTISpec(I(INCLUDEleft,sigid_list2right),
                                           sigid_list2) 
) end
)
 in (LrTable.NT 106,(result,INCLUDE1left,sigid_list21right),rest671)
 end
| (267,(_,(MlyValue.sigid_list2 sigid_list21,_,sigid_list21right))::(_
,(MlyValue.sigid sigid1,sigid1left,_))::rest671) => let val result=
MlyValue.sigid_list2(fn _ => let val sigid as sigid1=sigid1 ()
val sigid_list2 as sigid_list21=sigid_list21 ()
 in ( sigid::sigid_list2 ) end
)
 in (LrTable.NT 107,(result,sigid1left,sigid_list21right),rest671) end
| (268,(_,(MlyValue.sigid sigid2,_,sigid2right))::(_,(MlyValue.sigid 
sigid1,sigid1left,_))::rest671) => let val result=MlyValue.sigid_list2
(fn _ => let val sigid1=sigid1 ()
val sigid2=sigid2 ()
 in ( sigid1::sigid2::[] ) end
)
 in (LrTable.NT 107,(result,sigid1left,sigid2right),rest671) end
| (269,(_,(MlyValue.longtycon_EQUALS_list1 longtycon_EQUALS_list11,_,
longtycon_EQUALS_list11right))::_::(_,(MlyValue.longtycon longtycon1,
longtycon1left,_))::rest671) => let val result=
MlyValue.longtycon_EQUALS_list1(fn _ => let val longtycon as 
longtycon1=longtycon1 ()
val longtycon_EQUALS_list1 as longtycon_EQUALS_list11=
longtycon_EQUALS_list11 ()
 in ( longtycon::longtycon_EQUALS_list1 ) end
)
 in (LrTable.NT 108,(result,longtycon1left,
longtycon_EQUALS_list11right),rest671) end
| (270,(_,(MlyValue.longtycon longtycon1,longtycon1left,
longtycon1right))::rest671) => let val result=
MlyValue.longtycon_EQUALS_list1(fn _ => let val longtycon as 
longtycon1=longtycon1 ()
 in ( longtycon::[] ) end
)
 in (LrTable.NT 108,(result,longtycon1left,longtycon1right),rest671)
 end
| (271,(_,(MlyValue.longtycon_EQUALS_list1 longtycon_EQUALS_list11,_,
longtycon_EQUALS_list11right))::_::(_,(MlyValue.longtycon longtycon1,
longtycon1left,_))::rest671) => let val result=
MlyValue.longtycon_EQUALS_list2(fn _ => let val longtycon as 
longtycon1=longtycon1 ()
val longtycon_EQUALS_list1 as longtycon_EQUALS_list11=
longtycon_EQUALS_list11 ()
 in ( longtycon::longtycon_EQUALS_list1 ) end
)
 in (LrTable.NT 109,(result,longtycon1left,
longtycon_EQUALS_list11right),rest671) end
| (272,(_,(MlyValue.longstrid_EQUALS_list1 longstrid_EQUALS_list11,_,
longstrid_EQUALS_list11right))::_::(_,(MlyValue.longstrid longstrid1,
longstrid1left,_))::rest671) => let val result=
MlyValue.longstrid_EQUALS_list1(fn _ => let val longstrid as 
longstrid1=longstrid1 ()
val longstrid_EQUALS_list1 as longstrid_EQUALS_list11=
longstrid_EQUALS_list11 ()
 in ( longstrid::longstrid_EQUALS_list1 ) end
)
 in (LrTable.NT 110,(result,longstrid1left,
longstrid_EQUALS_list11right),rest671) end
| (273,(_,(MlyValue.longstrid longstrid1,longstrid1left,
longstrid1right))::rest671) => let val result=
MlyValue.longstrid_EQUALS_list1(fn _ => let val longstrid as 
longstrid1=longstrid1 ()
 in ( longstrid::[] ) end
)
 in (LrTable.NT 110,(result,longstrid1left,longstrid1right),rest671)
 end
| (274,(_,(MlyValue.longstrid_EQUALS_list1 longstrid_EQUALS_list11,_,
longstrid_EQUALS_list11right))::_::(_,(MlyValue.longstrid longstrid1,
longstrid1left,_))::rest671) => let val result=
MlyValue.longstrid_EQUALS_list2(fn _ => let val longstrid as 
longstrid1=longstrid1 ()
val longstrid_EQUALS_list1 as longstrid_EQUALS_list11=
longstrid_EQUALS_list11 ()
 in ( longstrid::longstrid_EQUALS_list1 ) end
)
 in (LrTable.NT 111,(result,longstrid1left,
longstrid_EQUALS_list11right),rest671) end
| (275,(_,(MlyValue.AND_valdesc_opt AND_valdesc_opt1,_,
AND_valdesc_optright as AND_valdesc_opt1right))::(_,(MlyValue.ty ty1,_
,_))::_::(_,(MlyValue.vid' vid'1,vid'left as vid'1left,_))::rest671)
 => let val result=MlyValue.valdesc(fn _ => let val vid' as vid'1=
vid'1 ()
val ty as ty1=ty1 ()
val AND_valdesc_opt as AND_valdesc_opt1=AND_valdesc_opt1 ()
 in (
 ValDesc(I(vid'left,AND_valdesc_optright),
                                  vid', ty, AND_valdesc_opt) 
) end
)
 in (LrTable.NT 112,(result,vid'1left,AND_valdesc_opt1right),rest671)
 end
| (276,(_,(MlyValue.valdesc valdesc1,_,valdesc1right))::(_,(_,AND1left
,_))::rest671) => let val result=MlyValue.AND_valdesc_opt(fn _ => let 
val valdesc as valdesc1=valdesc1 ()
 in ( SOME valdesc ) end
)
 in (LrTable.NT 113,(result,AND1left,valdesc1right),rest671) end
| (277,rest671) => let val result=MlyValue.AND_valdesc_opt(fn _ => (
 NONE ))
 in (LrTable.NT 113,(result,defaultPos,defaultPos),rest671) end
| (278,(_,(MlyValue.AND_typdesc_opt AND_typdesc_opt1,_,
AND_typdesc_optright as AND_typdesc_opt1right))::(_,(MlyValue.tycon 
tycon1,_,_))::(_,(MlyValue.tyvarseq tyvarseq1,tyvarseqleft as 
tyvarseq1left,_))::rest671) => let val result=MlyValue.typdesc(fn _
 => let val tyvarseq as tyvarseq1=tyvarseq1 ()
val tycon as tycon1=tycon1 ()
val AND_typdesc_opt as AND_typdesc_opt1=AND_typdesc_opt1 ()
 in (
 TypDesc(I(tyvarseqleft,AND_typdesc_optright),
                                  tyvarseq, tycon, AND_typdesc_opt) 
) end
)
 in (LrTable.NT 114,(result,tyvarseq1left,AND_typdesc_opt1right),
rest671) end
| (279,(_,(MlyValue.typdesc typdesc1,_,typdesc1right))::(_,(_,AND1left
,_))::rest671) => let val result=MlyValue.AND_typdesc_opt(fn _ => let 
val typdesc as typdesc1=typdesc1 ()
 in ( SOME typdesc ) end
)
 in (LrTable.NT 115,(result,AND1left,typdesc1right),rest671) end
| (280,rest671) => let val result=MlyValue.AND_typdesc_opt(fn _ => (
 NONE ))
 in (LrTable.NT 115,(result,defaultPos,defaultPos),rest671) end
| (281,(_,(MlyValue.AND_syndesc_opt AND_syndesc_opt1,_,
AND_syndesc_optright as AND_syndesc_opt1right))::(_,(MlyValue.ty ty1,_
,_))::_::(_,(MlyValue.tycon tycon1,_,_))::(_,(MlyValue.tyvarseq 
tyvarseq1,tyvarseqleft as tyvarseq1left,_))::rest671) => let val 
result=MlyValue.syndesc(fn _ => let val tyvarseq as tyvarseq1=
tyvarseq1 ()
val tycon as tycon1=tycon1 ()
val ty as ty1=ty1 ()
val AND_syndesc_opt as AND_syndesc_opt1=AND_syndesc_opt1 ()
 in (
 SynDesc(I(tyvarseqleft,AND_syndesc_optright),
                                  tyvarseq, tycon, ty, AND_syndesc_opt) 
) end
)
 in (LrTable.NT 116,(result,tyvarseq1left,AND_syndesc_opt1right),
rest671) end
| (282,(_,(MlyValue.syndesc syndesc1,_,syndesc1right))::(_,(_,AND1left
,_))::rest671) => let val result=MlyValue.AND_syndesc_opt(fn _ => let 
val syndesc as syndesc1=syndesc1 ()
 in ( SOME syndesc ) end
)
 in (LrTable.NT 117,(result,AND1left,syndesc1right),rest671) end
| (283,rest671) => let val result=MlyValue.AND_syndesc_opt(fn _ => (
 NONE ))
 in (LrTable.NT 117,(result,defaultPos,defaultPos),rest671) end
| (284,(_,(MlyValue.AND_datdesc_opt AND_datdesc_opt1,_,
AND_datdesc_optright as AND_datdesc_opt1right))::(_,(MlyValue.condesc 
condesc1,_,_))::_::(_,(MlyValue.tycon tycon1,_,_))::(_,(
MlyValue.tyvarseq tyvarseq1,tyvarseqleft as tyvarseq1left,_))::rest671
) => let val result=MlyValue.datdesc(fn _ => let val tyvarseq as 
tyvarseq1=tyvarseq1 ()
val tycon as tycon1=tycon1 ()
val condesc as condesc1=condesc1 ()
val AND_datdesc_opt as AND_datdesc_opt1=AND_datdesc_opt1 ()
 in (
 DatDesc(I(tyvarseqleft,AND_datdesc_optright),
                                  tyvarseq, tycon, condesc, AND_datdesc_opt) 
) end
)
 in (LrTable.NT 118,(result,tyvarseq1left,AND_datdesc_opt1right),
rest671) end
| (285,(_,(MlyValue.AND_datdesc_opt AND_datdesc_opt1,_,
AND_datdesc_optright as AND_datdesc_opt1right))::(_,(MlyValue.condesc 
condesc1,_,_))::_::(_,(MlyValue.tycon tycon1,tyconleft as tycon1left,_
))::rest671) => let val result=MlyValue.datdesc0(fn _ => let val tycon
 as tycon1=tycon1 ()
val condesc as condesc1=condesc1 ()
val AND_datdesc_opt as AND_datdesc_opt1=AND_datdesc_opt1 ()
 in (
 DatDesc(I(tyconleft,AND_datdesc_optright),
                                  TyVarseq(I(defaultPos,defaultPos), []),
                                  tycon, condesc, AND_datdesc_opt) 
) end
)
 in (LrTable.NT 119,(result,tycon1left,AND_datdesc_opt1right),rest671)
 end
| (286,(_,(MlyValue.AND_datdesc_opt AND_datdesc_opt1,_,
AND_datdesc_optright as AND_datdesc_opt1right))::(_,(MlyValue.condesc 
condesc1,_,_))::_::(_,(MlyValue.tycon tycon1,_,_))::(_,(
MlyValue.tyvarseq1 tyvarseq11,tyvarseq1left as tyvarseq11left,_))::
rest671) => let val result=MlyValue.datdesc1(fn _ => let val tyvarseq1
 as tyvarseq11=tyvarseq11 ()
val tycon as tycon1=tycon1 ()
val condesc as condesc1=condesc1 ()
val AND_datdesc_opt as AND_datdesc_opt1=AND_datdesc_opt1 ()
 in (
 DatDesc(I(tyvarseq1left,AND_datdesc_optright),
                                  tyvarseq1, tycon, condesc, AND_datdesc_opt) 
) end
)
 in (LrTable.NT 120,(result,tyvarseq11left,AND_datdesc_opt1right),
rest671) end
| (287,(_,(MlyValue.datdesc datdesc1,_,datdesc1right))::(_,(_,AND1left
,_))::rest671) => let val result=MlyValue.AND_datdesc_opt(fn _ => let 
val datdesc as datdesc1=datdesc1 ()
 in ( SOME datdesc ) end
)
 in (LrTable.NT 121,(result,AND1left,datdesc1right),rest671) end
| (288,rest671) => let val result=MlyValue.AND_datdesc_opt(fn _ => (
 NONE ))
 in (LrTable.NT 121,(result,defaultPos,defaultPos),rest671) end
| (289,(_,(MlyValue.BAR_condesc_opt BAR_condesc_opt1,_,
BAR_condesc_optright as BAR_condesc_opt1right))::(_,(
MlyValue.OF_ty_opt OF_ty_opt1,_,_))::(_,(MlyValue.vid' vid'1,vid'left
 as vid'1left,_))::rest671) => let val result=MlyValue.condesc(fn _
 => let val vid' as vid'1=vid'1 ()
val OF_ty_opt as OF_ty_opt1=OF_ty_opt1 ()
val BAR_condesc_opt as BAR_condesc_opt1=BAR_condesc_opt1 ()
 in (
 ConDesc(I(vid'left,BAR_condesc_optright),
                                  vid', OF_ty_opt, BAR_condesc_opt) 
) end
)
 in (LrTable.NT 122,(result,vid'1left,BAR_condesc_opt1right),rest671)
 end
| (290,(_,(MlyValue.condesc condesc1,_,condesc1right))::(_,(_,BAR1left
,_))::rest671) => let val result=MlyValue.BAR_condesc_opt(fn _ => let 
val condesc as condesc1=condesc1 ()
 in ( SOME condesc ) end
)
 in (LrTable.NT 123,(result,BAR1left,condesc1right),rest671) end
| (291,rest671) => let val result=MlyValue.BAR_condesc_opt(fn _ => (
 NONE ))
 in (LrTable.NT 123,(result,defaultPos,defaultPos),rest671) end
| (292,(_,(MlyValue.AND_exdesc_opt AND_exdesc_opt1,_,
AND_exdesc_optright as AND_exdesc_opt1right))::(_,(MlyValue.OF_ty_opt 
OF_ty_opt1,_,_))::(_,(MlyValue.vid' vid'1,vid'left as vid'1left,_))::
rest671) => let val result=MlyValue.exdesc(fn _ => let val vid' as 
vid'1=vid'1 ()
val OF_ty_opt as OF_ty_opt1=OF_ty_opt1 ()
val AND_exdesc_opt as AND_exdesc_opt1=AND_exdesc_opt1 ()
 in (
 ExDesc(I(vid'left,AND_exdesc_optright),
                                 vid', OF_ty_opt, AND_exdesc_opt) 
) end
)
 in (LrTable.NT 124,(result,vid'1left,AND_exdesc_opt1right),rest671)
 end
| (293,(_,(MlyValue.exdesc exdesc1,_,exdesc1right))::(_,(_,AND1left,_)
)::rest671) => let val result=MlyValue.AND_exdesc_opt(fn _ => let val 
exdesc as exdesc1=exdesc1 ()
 in ( SOME exdesc ) end
)
 in (LrTable.NT 125,(result,AND1left,exdesc1right),rest671) end
| (294,rest671) => let val result=MlyValue.AND_exdesc_opt(fn _ => (
 NONE ))
 in (LrTable.NT 125,(result,defaultPos,defaultPos),rest671) end
| (295,(_,(MlyValue.sigexp__AND_strdesc_opt sigexp__AND_strdesc_opt1,_
,sigexp__AND_strdesc_optright as sigexp__AND_strdesc_opt1right))::_::(
_,(MlyValue.strid strid1,stridleft as strid1left,_))::rest671) => let 
val result=MlyValue.strdesc(fn _ => let val strid as strid1=strid1 ()
val sigexp__AND_strdesc_opt as sigexp__AND_strdesc_opt1=
sigexp__AND_strdesc_opt1 ()
 in (
 StrDesc(I(stridleft,sigexp__AND_strdesc_optright),
                                  strid, #1 sigexp__AND_strdesc_opt,
                                  #2 sigexp__AND_strdesc_opt) 
) end
)
 in (LrTable.NT 126,(result,strid1left,sigexp__AND_strdesc_opt1right),
rest671) end
| (296,(_,(MlyValue.strdesc strdesc1,_,strdesc1right))::(_,(_,AND1left
,_))::rest671) => let val result=MlyValue.AND_strdesc_opt(fn _ => let 
val strdesc as strdesc1=strdesc1 ()
 in ( SOME strdesc ) end
)
 in (LrTable.NT 127,(result,AND1left,strdesc1right),rest671) end
| (297,rest671) => let val result=MlyValue.AND_strdesc_opt(fn _ => (
 NONE ))
 in (LrTable.NT 127,(result,defaultPos,defaultPos),rest671) end
| (298,(_,(MlyValue.AND_strdesc_opt AND_strdesc_opt1,_,
AND_strdesc_opt1right))::(_,(MlyValue.sigexp' sigexp'1,sigexp'1left,_)
)::rest671) => let val result=MlyValue.sigexp__AND_strdesc_opt(fn _
 => let val sigexp' as sigexp'1=sigexp'1 ()
val AND_strdesc_opt as AND_strdesc_opt1=AND_strdesc_opt1 ()
 in ( ( sigexp', AND_strdesc_opt ) ) end
)
 in (LrTable.NT 128,(result,sigexp'1left,AND_strdesc_opt1right),
rest671) end
| (299,(_,(MlyValue.tyreadesc__AND_strdesc_opt 
tyreadesc__AND_strdesc_opt1,_,tyreadesc__AND_strdesc_optright as 
tyreadesc__AND_strdesc_opt1right))::_::(_,(MlyValue.sigexp sigexp1,
sigexpleft as sigexp1left,_))::rest671) => let val result=
MlyValue.sigexp__AND_strdesc_opt(fn _ => let val sigexp as sigexp1=
sigexp1 ()
val tyreadesc__AND_strdesc_opt as tyreadesc__AND_strdesc_opt1=
tyreadesc__AND_strdesc_opt1 ()
 in (
 ( WHERETYPESigExp(I(sigexpleft,
                                              tyreadesc__AND_strdesc_optright),
                                           sigexp,
                                           #1 tyreadesc__AND_strdesc_opt),
                            #2 tyreadesc__AND_strdesc_opt ) 
) end
)
 in (LrTable.NT 128,(result,sigexp1left,
tyreadesc__AND_strdesc_opt1right),rest671) end
| (300,(_,(MlyValue.AND_tyreadesc_opt__AND_strdesc_opt 
AND_tyreadesc_opt__AND_strdesc_opt1,_,
AND_tyreadesc_opt__AND_strdesc_optright as 
AND_tyreadesc_opt__AND_strdesc_opt1right))::(_,(MlyValue.ty ty1,_,_))
::_::(_,(MlyValue.longtycon longtycon1,_,_))::(_,(MlyValue.tyvarseq 
tyvarseq1,_,_))::(_,(_,TYPEleft as TYPE1left,_))::rest671) => let val 
result=MlyValue.tyreadesc__AND_strdesc_opt(fn _ => let val tyvarseq
 as tyvarseq1=tyvarseq1 ()
val longtycon as longtycon1=longtycon1 ()
val ty as ty1=ty1 ()
val AND_tyreadesc_opt__AND_strdesc_opt as 
AND_tyreadesc_opt__AND_strdesc_opt1=
AND_tyreadesc_opt__AND_strdesc_opt1 ()
 in (
 ( TyReaDesc(I(TYPEleft,
                                       AND_tyreadesc_opt__AND_strdesc_optright),
                                      tyvarseq, longtycon, ty,
                                      #1 AND_tyreadesc_opt__AND_strdesc_opt),
                            #2 AND_tyreadesc_opt__AND_strdesc_opt ) 
) end
)
 in (LrTable.NT 129,(result,TYPE1left,
AND_tyreadesc_opt__AND_strdesc_opt1right),rest671) end
| (301,(_,(MlyValue.AND_strdesc_opt AND_strdesc_opt1,
AND_strdesc_opt1left,AND_strdesc_opt1right))::rest671) => let val 
result=MlyValue.AND_tyreadesc_opt__AND_strdesc_opt(fn _ => let val 
AND_strdesc_opt as AND_strdesc_opt1=AND_strdesc_opt1 ()
 in ( ( NONE, AND_strdesc_opt ) ) end
)
 in (LrTable.NT 130,(result,AND_strdesc_opt1left,AND_strdesc_opt1right
),rest671) end
| (302,(_,(MlyValue.tyreadesc__AND_strdesc_opt 
tyreadesc__AND_strdesc_opt1,_,tyreadesc__AND_strdesc_opt1right))::(_,(
_,AND1left,_))::rest671) => let val result=
MlyValue.AND_tyreadesc_opt__AND_strdesc_opt(fn _ => let val 
tyreadesc__AND_strdesc_opt as tyreadesc__AND_strdesc_opt1=
tyreadesc__AND_strdesc_opt1 ()
 in (
 ( SOME(#1 tyreadesc__AND_strdesc_opt),
                                    #2 tyreadesc__AND_strdesc_opt ) 
) end
)
 in (LrTable.NT 130,(result,AND1left,tyreadesc__AND_strdesc_opt1right)
,rest671) end
| (303,(_,(MlyValue.funbind funbind1,_,funbindright as funbind1right))
::(_,(_,FUNCTORleft as FUNCTOR1left,_))::rest671) => let val result=
MlyValue.fundec(fn _ => let val funbind as funbind1=funbind1 ()
 in ( FunDec(I(FUNCTORleft,funbindright), funbind) ) end
)
 in (LrTable.NT 131,(result,FUNCTOR1left,funbind1right),rest671) end
| (304,(_,(MlyValue.strexp__AND_funbind_opt strexp__AND_funbind_opt1,_
,strexp__AND_funbind_optright as strexp__AND_funbind_opt1right))::_::(
_,(MlyValue.COLON_sigexp_opt COLON_sigexp_opt1,_,_))::_::(_,(
MlyValue.sigexp sigexp1,_,_))::_::(_,(MlyValue.strid strid1,_,_))::_::
(_,(MlyValue.funid funid1,funidleft as funid1left,_))::rest671) => 
let val result=MlyValue.funbind(fn _ => let val funid as funid1=funid1
 ()
val strid as strid1=strid1 ()
val sigexp as sigexp1=sigexp1 ()
val COLON_sigexp_opt as COLON_sigexp_opt1=COLON_sigexp_opt1 ()
val strexp__AND_funbind_opt as strexp__AND_funbind_opt1=
strexp__AND_funbind_opt1 ()
 in (
 TRANSFunBind(I(funidleft,
                                         strexp__AND_funbind_optright),
                                       funid, strid, sigexp, COLON_sigexp_opt,
                                       #1 strexp__AND_funbind_opt,
                                       #2 strexp__AND_funbind_opt) 
) end
)
 in (LrTable.NT 132,(result,funid1left,strexp__AND_funbind_opt1right),
rest671) end
| (305,(_,(MlyValue.strexp__AND_funbind_opt strexp__AND_funbind_opt1,_
,strexp__AND_funbind_optright as strexp__AND_funbind_opt1right))::_::(
_,(MlyValue.sigexp sigexp2,_,_))::_::_::(_,(MlyValue.sigexp sigexp1,_,
_))::_::(_,(MlyValue.strid strid1,_,_))::_::(_,(MlyValue.funid funid1,
funidleft as funid1left,_))::rest671) => let val result=
MlyValue.funbind(fn _ => let val funid as funid1=funid1 ()
val strid as strid1=strid1 ()
val sigexp1=sigexp1 ()
val sigexp2=sigexp2 ()
val strexp__AND_funbind_opt as strexp__AND_funbind_opt1=
strexp__AND_funbind_opt1 ()
 in (
 OPAQFunBind(I(funidleft,strexp__AND_funbind_optright),
                                      funid, strid, sigexp1, sigexp2,
                                      #1 strexp__AND_funbind_opt,
                                      #2 strexp__AND_funbind_opt) 
) end
)
 in (LrTable.NT 132,(result,funid1left,strexp__AND_funbind_opt1right),
rest671) end
| (306,(_,(MlyValue.strexp__AND_funbind_opt strexp__AND_funbind_opt1,_
,strexp__AND_funbind_optright as strexp__AND_funbind_opt1right))::_::(
_,(MlyValue.COLON_sigexp_opt COLON_sigexp_opt1,_,_))::_::(_,(
MlyValue.spec spec1,_,_))::_::(_,(MlyValue.funid funid1,funidleft as 
funid1left,_))::rest671) => let val result=MlyValue.funbind(fn _ => 
let val funid as funid1=funid1 ()
val spec as spec1=spec1 ()
val COLON_sigexp_opt as COLON_sigexp_opt1=COLON_sigexp_opt1 ()
val strexp__AND_funbind_opt as strexp__AND_funbind_opt1=
strexp__AND_funbind_opt1 ()
 in (
 TRANSSPECFunBind(I(funidleft,
                                             strexp__AND_funbind_optright),
                                           funid, spec, COLON_sigexp_opt,
                                           #1 strexp__AND_funbind_opt,
                                           #2 strexp__AND_funbind_opt) 
) end
)
 in (LrTable.NT 132,(result,funid1left,strexp__AND_funbind_opt1right),
rest671) end
| (307,(_,(MlyValue.strexp__AND_funbind_opt strexp__AND_funbind_opt1,_
,strexp__AND_funbind_optright as strexp__AND_funbind_opt1right))::_::(
_,(MlyValue.sigexp sigexp1,_,_))::_::_::(_,(MlyValue.spec spec1,_,_))
::_::(_,(MlyValue.funid funid1,funidleft as funid1left,_))::rest671)
 => let val result=MlyValue.funbind(fn _ => let val funid as funid1=
funid1 ()
val spec as spec1=spec1 ()
val sigexp as sigexp1=sigexp1 ()
val strexp__AND_funbind_opt as strexp__AND_funbind_opt1=
strexp__AND_funbind_opt1 ()
 in (
 OPAQSPECFunBind(I(funidleft,
                                            strexp__AND_funbind_optright),
                                          funid, spec, sigexp,
                                          #1 strexp__AND_funbind_opt,
                                          #2 strexp__AND_funbind_opt) 
) end
)
 in (LrTable.NT 132,(result,funid1left,strexp__AND_funbind_opt1right),
rest671) end
| (308,(_,(MlyValue.funbind funbind1,_,funbind1right))::(_,(_,AND1left
,_))::rest671) => let val result=MlyValue.AND_funbind_opt(fn _ => let 
val funbind as funbind1=funbind1 ()
 in ( SOME funbind ) end
)
 in (LrTable.NT 133,(result,AND1left,funbind1right),rest671) end
| (309,rest671) => let val result=MlyValue.AND_funbind_opt(fn _ => (
 NONE ))
 in (LrTable.NT 133,(result,defaultPos,defaultPos),rest671) end
| (310,(_,(MlyValue.AND_funbind_opt AND_funbind_opt1,_,
AND_funbind_opt1right))::(_,(MlyValue.strexp' strexp'1,strexp'1left,_)
)::rest671) => let val result=MlyValue.strexp__AND_funbind_opt(fn _
 => let val strexp' as strexp'1=strexp'1 ()
val AND_funbind_opt as AND_funbind_opt1=AND_funbind_opt1 ()
 in ( ( strexp', AND_funbind_opt ) ) end
)
 in (LrTable.NT 134,(result,strexp'1left,AND_funbind_opt1right),
rest671) end
| (311,(_,(MlyValue.sigexp__AND_funbind_opt sigexp__AND_funbind_opt1,_
,sigexp__AND_funbind_optright as sigexp__AND_funbind_opt1right))::_::(
_,(MlyValue.strexp strexp1,strexpleft as strexp1left,_))::rest671) => 
let val result=MlyValue.strexp__AND_funbind_opt(fn _ => let val strexp
 as strexp1=strexp1 ()
val sigexp__AND_funbind_opt as sigexp__AND_funbind_opt1=
sigexp__AND_funbind_opt1 ()
 in (
 ( TRANSStrExp(I(strexpleft,
                                          sigexp__AND_funbind_optright),
                                        strexp, #1 sigexp__AND_funbind_opt),
                            #2 sigexp__AND_funbind_opt ) 
) end
)
 in (LrTable.NT 134,(result,strexp1left,sigexp__AND_funbind_opt1right)
,rest671) end
| (312,(_,(MlyValue.sigexp__AND_funbind_opt sigexp__AND_funbind_opt1,_
,sigexp__AND_funbind_optright as sigexp__AND_funbind_opt1right))::_::(
_,(MlyValue.strexp strexp1,strexpleft as strexp1left,_))::rest671) => 
let val result=MlyValue.strexp__AND_funbind_opt(fn _ => let val strexp
 as strexp1=strexp1 ()
val sigexp__AND_funbind_opt as sigexp__AND_funbind_opt1=
sigexp__AND_funbind_opt1 ()
 in (
 ( OPAQStrExp(I(strexpleft,
                                         sigexp__AND_funbind_optright),
                                       strexp, #1 sigexp__AND_funbind_opt),
                            #2 sigexp__AND_funbind_opt ) 
) end
)
 in (LrTable.NT 134,(result,strexp1left,sigexp__AND_funbind_opt1right)
,rest671) end
| (313,(_,(MlyValue.AND_funbind_opt AND_funbind_opt1,_,
AND_funbind_opt1right))::(_,(MlyValue.sigexp' sigexp'1,sigexp'1left,_)
)::rest671) => let val result=MlyValue.sigexp__AND_funbind_opt(fn _
 => let val sigexp' as sigexp'1=sigexp'1 ()
val AND_funbind_opt as AND_funbind_opt1=AND_funbind_opt1 ()
 in ( ( sigexp', AND_funbind_opt ) ) end
)
 in (LrTable.NT 135,(result,sigexp'1left,AND_funbind_opt1right),
rest671) end
| (314,(_,(MlyValue.tyreadesc__AND_funbind_opt 
tyreadesc__AND_funbind_opt1,_,tyreadesc__AND_funbind_optright as 
tyreadesc__AND_funbind_opt1right))::_::(_,(MlyValue.sigexp sigexp1,
sigexpleft as sigexp1left,_))::rest671) => let val result=
MlyValue.sigexp__AND_funbind_opt(fn _ => let val sigexp as sigexp1=
sigexp1 ()
val tyreadesc__AND_funbind_opt as tyreadesc__AND_funbind_opt1=
tyreadesc__AND_funbind_opt1 ()
 in (
 ( WHERETYPESigExp(I(sigexpleft,
                                              tyreadesc__AND_funbind_optright),
                                           sigexp,
                                           #1 tyreadesc__AND_funbind_opt),
                            #2 tyreadesc__AND_funbind_opt ) 
) end
)
 in (LrTable.NT 135,(result,sigexp1left,
tyreadesc__AND_funbind_opt1right),rest671) end
| (315,(_,(MlyValue.AND_tyreadesc_opt__AND_funbind_opt 
AND_tyreadesc_opt__AND_funbind_opt1,_,
AND_tyreadesc_opt__AND_funbind_optright as 
AND_tyreadesc_opt__AND_funbind_opt1right))::(_,(MlyValue.ty ty1,_,_))
::_::(_,(MlyValue.longtycon longtycon1,_,_))::(_,(MlyValue.tyvarseq 
tyvarseq1,_,_))::(_,(_,TYPEleft as TYPE1left,_))::rest671) => let val 
result=MlyValue.tyreadesc__AND_funbind_opt(fn _ => let val tyvarseq
 as tyvarseq1=tyvarseq1 ()
val longtycon as longtycon1=longtycon1 ()
val ty as ty1=ty1 ()
val AND_tyreadesc_opt__AND_funbind_opt as 
AND_tyreadesc_opt__AND_funbind_opt1=
AND_tyreadesc_opt__AND_funbind_opt1 ()
 in (
 ( TyReaDesc(I(TYPEleft,
                                       AND_tyreadesc_opt__AND_funbind_optright),
                                      tyvarseq, longtycon, ty,
                                      #1 AND_tyreadesc_opt__AND_funbind_opt),
                            #2 AND_tyreadesc_opt__AND_funbind_opt ) 
) end
)
 in (LrTable.NT 136,(result,TYPE1left,
AND_tyreadesc_opt__AND_funbind_opt1right),rest671) end
| (316,(_,(MlyValue.AND_funbind_opt AND_funbind_opt1,
AND_funbind_opt1left,AND_funbind_opt1right))::rest671) => let val 
result=MlyValue.AND_tyreadesc_opt__AND_funbind_opt(fn _ => let val 
AND_funbind_opt as AND_funbind_opt1=AND_funbind_opt1 ()
 in ( ( NONE, AND_funbind_opt ) ) end
)
 in (LrTable.NT 137,(result,AND_funbind_opt1left,AND_funbind_opt1right
),rest671) end
| (317,(_,(MlyValue.tyreadesc__AND_funbind_opt 
tyreadesc__AND_funbind_opt1,_,tyreadesc__AND_funbind_opt1right))::(_,(
_,AND1left,_))::rest671) => let val result=
MlyValue.AND_tyreadesc_opt__AND_funbind_opt(fn _ => let val 
tyreadesc__AND_funbind_opt as tyreadesc__AND_funbind_opt1=
tyreadesc__AND_funbind_opt1 ()
 in (
 ( SOME(#1 tyreadesc__AND_funbind_opt),
                            #2 tyreadesc__AND_funbind_opt ) 
) end
)
 in (LrTable.NT 137,(result,AND1left,tyreadesc__AND_funbind_opt1right)
,rest671) end
| (318,(_,(MlyValue.topdec1 topdec11,topdec11left,topdec11right))::
rest671) => let val result=MlyValue.topdec(fn _ => let val topdec1 as 
topdec11=topdec11 ()
 in ( topdec1 ) end
)
 in (LrTable.NT 138,(result,topdec11left,topdec11right),rest671) end
| (319,rest671) => let val result=MlyValue.topdec(fn _ => (
 STRDECTopDec(I(defaultPos,defaultPos),
                                       EMPTYStrDec(I(defaultPos,defaultPos)),
                                       NONE) 
))
 in (LrTable.NT 138,(result,defaultPos,defaultPos),rest671) end
| (320,(_,(MlyValue.topdec_opt topdec_opt1,_,topdec_optright as 
topdec_opt1right))::(_,(MlyValue.strdec1' strdec1'1,strdec1'left as 
strdec1'1left,_))::rest671) => let val result=MlyValue.topdec1(fn _
 => let val strdec1' as strdec1'1=strdec1'1 ()
val topdec_opt as topdec_opt1=topdec_opt1 ()
 in (
 STRDECTopDec(I(strdec1'left,topdec_optright),
                                       strdec1', topdec_opt) 
) end
)
 in (LrTable.NT 139,(result,strdec1'1left,topdec_opt1right),rest671)
 end
| (321,(_,(MlyValue.topdec_opt topdec_opt1,_,topdec_optright as 
topdec_opt1right))::(_,(MlyValue.sigdec sigdec1,sigdecleft as 
sigdec1left,_))::rest671) => let val result=MlyValue.topdec1(fn _ => 
let val sigdec as sigdec1=sigdec1 ()
val topdec_opt as topdec_opt1=topdec_opt1 ()
 in (
 SIGDECTopDec(I(sigdecleft,topdec_optright),
                                       sigdec, topdec_opt) 
) end
)
 in (LrTable.NT 139,(result,sigdec1left,topdec_opt1right),rest671) end
| (322,(_,(MlyValue.topdec_opt topdec_opt1,_,topdec_optright as 
topdec_opt1right))::(_,(MlyValue.fundec fundec1,fundecleft as 
fundec1left,_))::rest671) => let val result=MlyValue.topdec1(fn _ => 
let val fundec as fundec1=fundec1 ()
val topdec_opt as topdec_opt1=topdec_opt1 ()
 in (
 FUNDECTopDec(I(fundecleft,topdec_optright),
                                       fundec, topdec_opt) 
) end
)
 in (LrTable.NT 139,(result,fundec1left,topdec_opt1right),rest671) end
| (323,(_,(MlyValue.topdec1 topdec11,topdec11left,topdec11right))::
rest671) => let val result=MlyValue.topdec_opt(fn _ => let val topdec1
 as topdec11=topdec11 ()
 in ( SOME topdec1 ) end
)
 in (LrTable.NT 140,(result,topdec11left,topdec11right),rest671) end
| (324,rest671) => let val result=MlyValue.topdec_opt(fn _ => ( NONE )
)
 in (LrTable.NT 140,(result,defaultPos,defaultPos),rest671) end
| (325,(_,(MlyValue.program' program'1,_,program'1right))::(_,(
MlyValue.initInfix initInfix1,initInfix1left,_))::rest671) => let val 
result=MlyValue.program(fn _ => let val initInfix1=initInfix1 ()
val program' as program'1=program'1 ()
 in ( (program', !J) ) end
)
 in (LrTable.NT 141,(result,initInfix1left,program'1right),rest671)
 end
| (326,(_,(MlyValue.program_opt program_opt1,_,program_opt1right))::(_
,(_,_,SEMICOLONright))::(_,(MlyValue.topdec topdec1,topdecleft as 
topdec1left,_))::rest671) => let val result=MlyValue.program'(fn _ => 
let val topdec as topdec1=topdec1 ()
val program_opt as program_opt1=program_opt1 ()
 in (
 TOPDECProgram(I(topdecleft,SEMICOLONright),
                                        topdec, program_opt) 
) end
)
 in (LrTable.NT 142,(result,topdec1left,program_opt1right),rest671)
 end
| (327,(_,(MlyValue.program_opt program_opt1,_,program_opt1right))::(_
,(_,_,SEMICOLONright))::(_,(MlyValue.exp exp1,expleft as exp1left,_))
::rest671) => let val result=MlyValue.program'(fn _ => let val exp as 
exp1=exp1 ()
val program_opt as program_opt1=program_opt1 ()
 in (
 EXPProgram(I(expleft,SEMICOLONright),
                                     exp, program_opt) )
 end
)
 in (LrTable.NT 142,(result,exp1left,program_opt1right),rest671) end
| (328,(_,(MlyValue.program' program'1,program'1left,program'1right))
::rest671) => let val result=MlyValue.program_opt(fn _ => let val 
program' as program'1=program'1 ()
 in ( SOME program' ) end
)
 in (LrTable.NT 143,(result,program'1left,program'1right),rest671) end
| (329,rest671) => let val result=MlyValue.program_opt(fn _ => ( NONE 
))
 in (LrTable.NT 143,(result,defaultPos,defaultPos),rest671) end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
| _ => let exception ParseInternal
        in raise ParseInternal end) a ()
end
end
structure Tokens : Parser_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ABSTYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun ANDALSO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun AS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun CASE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun DATATYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun EXCEPTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun FN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun HANDLE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun INFIX (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun INFIXR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun LOCAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun NONFIX (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun OP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun OPEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun ORELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun RAISE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun REC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun VAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun WITH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun WITHTYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun DOTS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun UNDERBAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun BAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.VOID,p1,p2))
fun DARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(
ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 47,(
ParserData.MlyValue.VOID,p1,p2))
fun HASH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 48,(
ParserData.MlyValue.VOID,p1,p2))
fun EQTYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 49,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCTOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 50,(
ParserData.MlyValue.VOID,p1,p2))
fun INCLUDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 51,(
ParserData.MlyValue.VOID,p1,p2))
fun SHARING (p1,p2) = Token.TOKEN (ParserData.LrTable.T 52,(
ParserData.MlyValue.VOID,p1,p2))
fun SIG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 53,(
ParserData.MlyValue.VOID,p1,p2))
fun SIGNATURE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 54,(
ParserData.MlyValue.VOID,p1,p2))
fun STRUCT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 55,(
ParserData.MlyValue.VOID,p1,p2))
fun STRUCTURE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 56,(
ParserData.MlyValue.VOID,p1,p2))
fun WHERE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 57,(
ParserData.MlyValue.VOID,p1,p2))
fun COLONGREATER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 58,(
ParserData.MlyValue.VOID,p1,p2))
fun ZERO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 59,(
ParserData.MlyValue.VOID,p1,p2))
fun DIGIT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 60,(
ParserData.MlyValue.DIGIT (fn () => i),p1,p2))
fun NUMERIC (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 61,(
ParserData.MlyValue.NUMERIC (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 62,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun WORD (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 63,(
ParserData.MlyValue.WORD (fn () => i),p1,p2))
fun REAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 64,(
ParserData.MlyValue.REAL (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 65,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun CHAR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 66,(
ParserData.MlyValue.CHAR (fn () => i),p1,p2))
fun ALPHA (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 67,(
ParserData.MlyValue.ALPHA (fn () => i),p1,p2))
fun SYMBOL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 68,(
ParserData.MlyValue.SYMBOL (fn () => i),p1,p2))
fun STAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 69,(
ParserData.MlyValue.VOID,p1,p2))
fun TYVAR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 70,(
ParserData.MlyValue.TYVAR (fn () => i),p1,p2))
fun ETYVAR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 71,(
ParserData.MlyValue.ETYVAR (fn () => i),p1,p2))
fun LONGID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 72,(
ParserData.MlyValue.LONGID (fn () => i),p1,p2))
end
end
(* stop of Parser.grm.sml *)
(* start of Lexer.lex.sml *)
type int = Int.int
 functor LexerFn(structure Tokens: Parser_TOKENS) =
   struct
    structure UserDeclarations =
      struct
(*
 * Standard ML lexical analysis
 *
 * Definition, sections 2.1-2.5, 3.1
 *
 * Notes:
 *   Since all lexical classes must be disjoint:
 *   - There is no single class ID, use ALPHA|SYMBOL|STAR|EQUALS.
 *   - There is no class LAB, use ALPHA|SYMBOL|NUMERIC|DIGIT|STAR.
 *   - ID does not contain `=' and `*', those are EQUALS and STAR.
 *   - LONGID does not contain unqualified ids (but allows for `=' and `*').
 *   - INT does not contain positive decimal integers without leading 0,
 *     and single DIGIT integers, those are in NUMERIC, DIGIT, and ZERO.
 *   - NUMERIC does not contain single digit numbers, those are in DIGIT.
 *   - DIGIT does not contain 0, that is ZERO.
 *
 *   The parser uses a global variable to recognise nested comments, so it is
 *   not reentrant.
 *)


    open Tokens


    (* Types to match structure LEXER.UserDeclaration *)

    type ('a,'b) token = ('a,'b) Tokens.token
    type pos           = int
    type svalue        = Tokens.svalue
    type lexresult     = (svalue, pos) token



    (* Handling nested comments *)

    val nesting = ref 0         (* non-reentrant side-effect way :-P *)


    fun eof() =
        if !nesting = 0 then
            Tokens.EOF(0, 0)
        else
            Error.error((0,0), "unclosed comment")



    (* Some helpers to create tokens *)

    open Tokens


    fun toLRPos(yypos, yytext) =
        let
            val yypos = yypos - 2       (* bug in ML-Lex... *)
        in
            (yypos, yypos + String.size yytext)
        end

    fun token(TOKEN, yypos, yytext) =
        TOKEN(toLRPos(yypos, yytext))

    fun tokenOf(TOKEN, toVal, yypos, yytext) =
        let
            val i as (l,r) = toLRPos(yypos, yytext)
        in
            TOKEN(toVal(yytext, i), l, r)
        end

    fun error(yypos, yytext, s) =
            Error.error(toLRPos(yypos,yytext), s)

    fun invalid(yypos, yytext) =
        let
            val s = "invalid character `" ^ String.toCString yytext ^ "'"
        in
            error(yypos, yytext, s)
        end



    (* Convert identifiers *)

    fun toId(s, i)     = s

    fun toLongId(s, i) =
        let
            fun split  []    = raise Fail "Lexer.toLongId: empty longid"
              | split [x]    = ([],x)
              | split(x::xs) = let val (ys,y) = split xs in (x::ys,y) end
        in
            split(String.fields (fn c => c = #".") s)
        end


    (* Convert constants [Section 2.2] *)

    local open StringCvt in

    fun toInt(s,i) =
        (case String.explode s
          of #"0" :: #"x" :: s' =>
                  valOf(scanString (Int.scan HEX) (String.implode s'))
           | #"~" :: #"0" :: #"x" :: s' =>
                  ~(valOf(scanString (Int.scan HEX) (String.implode s')))
           | _ => valOf(scanString (Int.scan DEC) s)
        ) handle Overflow =>
                Error.error(i, "integer constant too big")

    fun toWord(s,i) =
        (case String.explode s
          of #"0" :: #"w" :: #"x" :: s' =>
                valOf(scanString (Word.scan HEX) (String.implode s'))
           | #"0" :: #"w" :: s' =>
                valOf(scanString (Word.scan DEC) (String.implode s'))
           | _ => raise Fail "Lexer.toWord: invalid word constant"
        ) handle Overflow =>
                Error.error(i, "word constant too big")

    fun toReal(s,i) = valOf(scanString Real.scan s)


    fun toString(s,i) =
        let
            fun convert(#"\\"::s, cs)  = escape(s, cs)
              | convert([#"\""],  cs)  = cs
              | convert(c::s,     cs)  = convert(s, c::cs)
              | convert([],       cs)  =
                    raise Fail "Lexer.toString: unclosed string literal"

            and escape(#"a"::s, cs)    = convert(s, #"\a"::cs)
              | escape(#"b"::s, cs)    = convert(s, #"\b"::cs)
              | escape(#"t"::s, cs)    = convert(s, #"\t"::cs)
              | escape(#"n"::s, cs)    = convert(s, #"\n"::cs)
              | escape(#"v"::s, cs)    = convert(s, #"\v"::cs)
              | escape(#"f"::s, cs)    = convert(s, #"\f"::cs)
              | escape(#"r"::s, cs)    = convert(s, #"\r"::cs)
              | escape(#"\""::s, cs)   = convert(s, #"\""::cs)
              | escape(#"\\"::s, cs)   = convert(s, #"\\"::cs)
              | escape(#"^"::c::s, cs) =
                    convert(s, Char.chr(Char.ord c - 64)::cs)

              | escape(#"u"::x1::x2::x3::x4::s, cs) =
                    convert(s, unicode[x1,x2,x3,x4]::cs)

              | escape(c::s, cs) =
                if Char.isDigit c then
                    case s
                      of c2::c3::s => convert(s, ascii[c,c2,c3]::cs)
                       | _ => raise Fail
                                "Lexer.toString: invalid ASCII escape sequence"
                else if Char.isSpace c then
                    escapeGap(s,cs)
                else
                    raise Fail "Lexer.toString: invalid escape sequence"

              | escape([], cs) =
                    raise Fail "Lexer.toString: empty escape character"

            and escapeGap(c::s, cs) =
                    if Char.isSpace c then
                        escapeGap(s, cs)
                    else (* c = #"\\" *)
                        convert(s, cs)

              | escapeGap([], cs) =
                    raise Fail "Lexer.toString: invalid string gap"

            and ascii s =
                Char.chr(valOf(scanString (Int.scan DEC) (String.implode s)))
                handle Chr =>
                         Error.error(i, "ASCII escape character too big")
                     | Overflow =>
                         Error.error(i, "ASCII escape character too big")

            and unicode s =
                Char.chr(valOf(scanString (Int.scan HEX) (String.implode s)))
                handle Chr =>
                         Error.error(i, "unicode escape character too big")
                     | Overflow =>
                         Error.error(i, "unicode escape character too big")

            val cs = List.tl(String.explode s)
        in
            String.implode(List.rev(convert(cs, [])))
        end


    fun toChar(s,i) =
        let
            val s'  = String.substring(s, 1, String.size s-1)
            val ss' = toString(s',i)
        in
            if String.size ss' = 1 then
                String.sub(ss',0)
            else if ss' = "" then
                Error.error(i, "empty character constant")
            else
                Error.error(i, "character constant too long")
        end

    end (* local *)


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
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (1, 
"\005\005\005\005\005\005\005\005\005\235\236\235\235\235\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\235\180\223\211\180\180\180\209\207\206\205\180\204\202\199\180\
\\191\189\189\189\189\189\189\189\189\189\187\186\180\184\180\180\
\\180\025\025\025\025\025\025\025\025\025\025\025\025\025\025\025\
\\025\025\025\025\025\025\025\025\025\025\025\183\180\182\180\181\
\\180\166\025\162\153\134\126\025\120\108\025\025\101\025\095\085\
\\025\025\078\055\048\025\045\030\025\025\025\024\023\022\006\005\
\\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005"
),
 (3, 
"\237\237\237\237\237\237\237\237\237\237\242\237\237\237\237\237\
\\237\237\237\237\237\237\237\237\237\237\237\237\237\237\237\237\
\\237\237\237\237\237\237\237\237\240\237\238\237\237\237\237\237\
\\237\237\237\237\237\237\237\237\237\237\237\237\237\237\237\237\
\\237\237\237\237\237\237\237\237\237\237\237\237\237\237\237\237\
\\237\237\237\237\237\237\237\237\237\237\237\237\237\237\237\237\
\\237\237\237\237\237\237\237\237\237\237\237\237\237\237\237\237\
\\237\237\237\237\237\237\237\237\237\237\237\237\237\237\237\237\
\\237\237\237\237\237\237\237\237\237\237\237\237\237\237\237\237\
\\237\237\237\237\237\237\237\237\237\237\237\237\237\237\237\237\
\\237\237\237\237\237\237\237\237\237\237\237\237\237\237\237\237\
\\237\237\237\237\237\237\237\237\237\237\237\237\237\237\237\237\
\\237\237\237\237\237\237\237\237\237\237\237\237\237\237\237\237\
\\237\237\237\237\237\237\237\237\237\237\237\237\237\237\237\237\
\\237\237\237\237\237\237\237\237\237\237\237\237\237\237\237\237\
\\237\237\237\237\237\237\237\237\237\237\237\237\237\237\237\237"
),
 (6, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\007\000\007\007\007\007\000\000\000\007\007\000\007\000\007\
\\019\008\008\008\008\008\008\008\008\008\007\000\007\007\007\007\
\\007\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\007\000\007\000\
\\007\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\007\000\007\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (7, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\007\000\007\007\007\007\000\000\000\007\007\000\007\000\007\
\\000\000\000\000\000\000\000\000\000\000\007\000\007\007\007\007\
\\007\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\007\000\007\000\
\\007\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\007\000\007\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (8, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\000\000\000\000\009\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\009\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (9, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\010\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (10, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\011\011\011\011\011\011\011\011\011\011\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (12, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\012\012\012\012\012\012\012\012\012\012\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (13, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\014\014\014\014\014\014\014\014\014\014\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (14, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\014\014\014\014\014\014\014\014\014\014\000\000\000\000\000\000\
\\000\000\000\000\000\015\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\015\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (15, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\018\018\018\018\018\018\018\018\018\018\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\016\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
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
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (18, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\018\018\018\018\018\018\018\018\018\018\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (19, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\000\000\000\000\009\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\009\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\020\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (20, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\021\021\021\021\021\021\021\021\021\021\000\000\000\000\000\000\
\\000\021\021\021\021\021\021\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\021\021\021\021\021\021\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (25, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (27, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\028\000\028\028\028\028\000\000\000\028\028\000\028\000\028\
\\000\000\000\000\000\000\000\000\000\000\028\000\028\028\028\028\
\\028\029\029\029\029\029\029\029\029\029\029\029\029\029\029\029\
\\029\029\029\029\029\029\029\029\029\029\029\000\028\000\028\000\
\\028\029\029\029\029\029\029\029\029\029\029\029\029\029\029\029\
\\029\029\029\029\029\029\029\029\029\029\029\000\028\000\028\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (28, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\028\000\028\028\028\028\000\000\000\028\028\000\028\000\028\
\\000\000\000\000\000\000\000\000\000\000\028\000\028\028\028\028\
\\028\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\028\000\028\000\
\\028\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\028\000\028\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (29, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\029\000\000\000\000\000\000\027\000\
\\029\029\029\029\029\029\029\029\029\029\000\000\000\000\000\000\
\\000\029\029\029\029\029\029\029\029\029\029\029\029\029\029\029\
\\029\029\029\029\029\029\029\029\029\029\029\000\000\000\000\029\
\\000\029\029\029\029\029\029\029\029\029\029\029\029\029\029\029\
\\029\029\029\029\029\029\029\029\029\029\029\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (30, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\038\031\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (31, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\032\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (32, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\033\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (33, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\034\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (34, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\035\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (35, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\036\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (36, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\037\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (38, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\042\026\026\026\039\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (39, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\040\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (40, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\041\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (42, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\043\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (43, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\044\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (45, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\046\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (46, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\047\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (48, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\052\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\049\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (49, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\050\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (50, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\051\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (52, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\053\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (53, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\054\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (55, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\072\064\026\026\026\026\026\026\
\\026\026\026\026\056\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (56, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\057\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (57, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\058\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (58, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\059\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (59, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\060\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (60, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\061\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (61, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\062\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (62, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\063\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (64, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\065\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (65, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\066\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (66, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\067\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (67, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\068\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (68, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\069\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (69, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\070\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (70, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\071\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (72, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\073\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (73, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\074\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (74, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\075\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (75, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\076\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (76, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\077\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (78, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\081\026\026\026\079\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (79, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\080\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (81, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\082\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (82, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\083\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (83, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\084\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (85, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\094\026\026\026\026\026\026\026\026\026\
\\091\026\086\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (86, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\087\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (87, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\088\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (88, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\089\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (89, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\090\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (91, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\092\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (92, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\093\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (95, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\096\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (96, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\097\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (97, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\098\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (98, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\099\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (99, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\100\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (101, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\106\026\026\026\026\026\026\026\026\026\102\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (102, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\103\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (103, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\104\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (104, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\105\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (106, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\107\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (108, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\119\026\026\026\026\026\026\026\109\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (109, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\114\026\026\110\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (110, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\111\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (111, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\112\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (112, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\113\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (114, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\115\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (115, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\116\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (116, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\117\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (117, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\118\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (120, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\121\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (121, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\122\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (122, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\123\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (123, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\124\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (124, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\125\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (126, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\133\026\
\\026\026\026\026\026\127\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (127, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\128\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (128, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\129\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (129, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\130\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (130, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\131\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (131, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\132\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (134, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\150\026\148\026\
\\026\143\026\026\026\026\026\026\135\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (135, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\136\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (136, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\137\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (137, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\138\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (138, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\139\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (139, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\140\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (140, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\141\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (141, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\142\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (143, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\144\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (144, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\145\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (145, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\146\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (146, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\147\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (148, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\149\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (150, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\151\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (151, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\152\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (153, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\155\026\026\026\026\026\026\026\026\026\026\026\026\026\154\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (155, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\156\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (156, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\157\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (157, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\158\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (158, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\159\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (159, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\160\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (160, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\161\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (162, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\163\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (163, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\164\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (164, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\165\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (166, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\174\026\026\026\026\026\026\026\026\026\026\026\168\026\
\\026\026\026\167\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (168, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\169\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (169, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\170\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (170, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\171\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (171, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\172\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (172, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\173\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (174, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\175\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (175, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\176\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (176, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\177\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (177, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\178\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (178, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\026\000\000\000\000\000\000\027\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\026\
\\000\026\026\026\026\179\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (184, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\007\000\007\007\007\007\000\000\000\007\007\000\007\000\007\
\\000\000\000\000\000\000\000\000\000\000\007\000\007\007\185\007\
\\007\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\007\000\007\000\
\\007\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\007\000\007\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (187, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\007\000\007\007\007\007\000\000\000\007\007\000\007\000\007\
\\000\000\000\000\000\000\000\000\000\000\007\000\007\007\188\007\
\\007\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\007\000\007\000\
\\007\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\007\000\007\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (189, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\\190\190\190\190\190\190\190\190\190\190\000\000\000\000\000\000\
\\000\000\000\000\000\009\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\009\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (191, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\\198\198\198\198\198\198\198\198\198\198\000\000\000\000\000\000\
\\000\000\000\000\000\009\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\009\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\194\192\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (192, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\193\193\193\193\193\193\193\193\193\193\000\000\000\000\000\000\
\\000\193\193\193\193\193\193\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\193\193\193\193\193\193\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (194, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\197\197\197\197\197\197\197\197\197\197\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\195\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (195, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\196\196\196\196\196\196\196\196\196\196\000\000\000\000\000\000\
\\000\196\196\196\196\196\196\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\196\196\196\196\196\196\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (197, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\197\197\197\197\197\197\197\197\197\197\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (198, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\\198\198\198\198\198\198\198\198\198\198\000\000\000\000\000\000\
\\000\000\000\000\000\009\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\009\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (199, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\200\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (200, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\201\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (202, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\007\000\007\007\007\007\000\000\000\007\007\000\007\000\007\
\\000\000\000\000\000\000\000\000\000\000\007\000\007\007\203\007\
\\007\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\007\000\007\000\
\\007\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\007\000\007\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (207, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\208\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (209, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\210\000\000\000\000\000\000\000\000\
\\210\210\210\210\210\210\210\210\210\210\000\000\000\000\000\000\
\\000\210\210\210\210\210\210\210\210\210\210\210\210\210\210\210\
\\210\210\210\210\210\210\210\210\210\210\210\000\000\000\000\210\
\\000\210\210\210\210\210\210\210\210\210\210\210\210\210\210\210\
\\210\210\210\210\210\210\210\210\210\210\210\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (211, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\007\212\007\007\007\007\000\000\000\007\007\000\007\000\007\
\\000\000\000\000\000\000\000\000\000\000\007\000\007\007\007\007\
\\007\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\007\000\007\000\
\\007\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\007\000\007\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (212, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\212\212\222\212\212\212\212\212\212\212\212\212\212\212\212\212\
\\212\212\212\212\212\212\212\212\212\212\212\212\212\212\212\212\
\\212\212\212\212\212\212\212\212\212\212\212\212\212\212\212\212\
\\212\212\212\212\212\212\212\212\212\212\212\212\213\212\212\212\
\\212\212\212\212\212\212\212\212\212\212\212\212\212\212\212\212\
\\212\212\212\212\212\212\212\212\212\212\212\212\212\212\212\000\
\\212\212\212\212\212\212\212\212\212\212\212\212\212\212\212\212\
\\212\212\212\212\212\212\212\212\212\212\212\212\212\212\212\212\
\\212\212\212\212\212\212\212\212\212\212\212\212\212\212\212\212\
\\212\212\212\212\212\212\212\212\212\212\212\212\212\212\212\212\
\\212\212\212\212\212\212\212\212\212\212\212\212\212\212\212\212\
\\212\212\212\212\212\212\212\212\212\212\212\212\212\212\212\212\
\\212\212\212\212\212\212\212\212\212\212\212\212\212\212\212\212\
\\212\212\212\212\212\212\212\212\212\212\212\212\212\212\212\212"
),
 (213, 
"\000\000\000\000\000\000\000\000\000\221\221\221\221\221\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\221\000\212\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\219\219\219\219\219\219\219\219\219\219\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\212\000\218\000\
\\000\212\212\000\000\000\212\000\000\000\000\000\000\000\212\000\
\\000\000\212\000\212\214\212\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (214, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\215\215\215\215\215\215\215\215\215\215\000\000\000\000\000\000\
\\000\215\215\215\215\215\215\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\215\215\215\215\215\215\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (215, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\216\216\216\216\216\216\216\216\216\216\000\000\000\000\000\000\
\\000\216\216\216\216\216\216\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\216\216\216\216\216\216\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (216, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\217\217\217\217\217\217\217\217\217\217\000\000\000\000\000\000\
\\000\217\217\217\217\217\217\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\217\217\217\217\217\217\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (217, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\212\212\212\212\212\212\212\212\212\212\000\000\000\000\000\000\
\\000\212\212\212\212\212\212\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\212\212\212\212\212\212\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (218, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\212\212\212\212\212\212\212\212\212\212\212\212\212\212\212\212\
\\212\212\212\212\212\212\212\212\212\212\212\212\212\212\212\212\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (219, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\220\220\220\220\220\220\220\220\220\220\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (220, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\212\212\212\212\212\212\212\212\212\212\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (221, 
"\000\000\000\000\000\000\000\000\000\221\221\221\221\221\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\221\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\212\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (223, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\224\224\234\224\224\224\224\224\224\224\224\224\224\224\224\224\
\\224\224\224\224\224\224\224\224\224\224\224\224\224\224\224\224\
\\224\224\224\224\224\224\224\224\224\224\224\224\224\224\224\224\
\\224\224\224\224\224\224\224\224\224\224\224\224\225\224\224\224\
\\224\224\224\224\224\224\224\224\224\224\224\224\224\224\224\224\
\\224\224\224\224\224\224\224\224\224\224\224\224\224\224\224\000\
\\224\224\224\224\224\224\224\224\224\224\224\224\224\224\224\224\
\\224\224\224\224\224\224\224\224\224\224\224\224\224\224\224\224\
\\224\224\224\224\224\224\224\224\224\224\224\224\224\224\224\224\
\\224\224\224\224\224\224\224\224\224\224\224\224\224\224\224\224\
\\224\224\224\224\224\224\224\224\224\224\224\224\224\224\224\224\
\\224\224\224\224\224\224\224\224\224\224\224\224\224\224\224\224\
\\224\224\224\224\224\224\224\224\224\224\224\224\224\224\224\224\
\\224\224\224\224\224\224\224\224\224\224\224\224\224\224\224\224"
),
 (225, 
"\000\000\000\000\000\000\000\000\000\233\233\233\233\233\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\233\000\224\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\231\231\231\231\231\231\231\231\231\231\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\224\000\230\000\
\\000\224\224\000\000\000\224\000\000\000\000\000\000\000\224\000\
\\000\000\224\000\224\226\224\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (226, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\227\227\227\227\227\227\227\227\227\227\000\000\000\000\000\000\
\\000\227\227\227\227\227\227\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\227\227\227\227\227\227\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (227, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\228\228\228\228\228\228\228\228\228\228\000\000\000\000\000\000\
\\000\228\228\228\228\228\228\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\228\228\228\228\228\228\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (228, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\229\229\229\229\229\229\229\229\229\229\000\000\000\000\000\000\
\\000\229\229\229\229\229\229\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\229\229\229\229\229\229\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (229, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\224\224\224\224\224\224\224\224\224\224\000\000\000\000\000\000\
\\000\224\224\224\224\224\224\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\224\224\224\224\224\224\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (230, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\224\224\224\224\224\224\224\224\224\224\224\224\224\224\224\224\
\\224\224\224\224\224\224\224\224\224\224\224\224\224\224\224\224\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (231, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\232\232\232\232\232\232\232\232\232\232\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (232, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\224\224\224\224\224\224\224\224\224\224\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (233, 
"\000\000\000\000\000\000\000\000\000\233\233\233\233\233\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\233\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\224\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (235, 
"\000\000\000\000\000\000\000\000\000\236\236\236\236\236\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\236\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (238, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\239\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
),
 (240, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\241\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
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
{fin = [(N 472)], trans = 0},
{fin = [(N 436),(N 472)], trans = 6},
{fin = [(N 436)], trans = 7},
{fin = [(N 304)], trans = 8},
{fin = [], trans = 9},
{fin = [], trans = 10},
{fin = [(N 342)], trans = 10},
{fin = [(N 342)], trans = 12},
{fin = [], trans = 13},
{fin = [(N 342)], trans = 14},
{fin = [], trans = 15},
{fin = [], trans = 16},
{fin = [(N 342)], trans = 16},
{fin = [(N 342)], trans = 18},
{fin = [(N 304)], trans = 19},
{fin = [], trans = 20},
{fin = [(N 304)], trans = 20},
{fin = [(N 43),(N 472)], trans = 0},
{fin = [(N 41),(N 436),(N 472)], trans = 7},
{fin = [(N 39),(N 472)], trans = 0},
{fin = [(N 433),(N 472)], trans = 25},
{fin = [(N 433)], trans = 25},
{fin = [], trans = 27},
{fin = [(N 455)], trans = 28},
{fin = [(N 455)], trans = 29},
{fin = [(N 433),(N 472)], trans = 30},
{fin = [(N 433)], trans = 31},
{fin = [(N 433)], trans = 32},
{fin = [(N 273),(N 433)], trans = 33},
{fin = [(N 433)], trans = 34},
{fin = [(N 433)], trans = 35},
{fin = [(N 433)], trans = 36},
{fin = [(N 282),(N 433)], trans = 25},
{fin = [(N 433)], trans = 38},
{fin = [(N 433)], trans = 39},
{fin = [(N 433)], trans = 40},
{fin = [(N 268),(N 433)], trans = 25},
{fin = [(N 433)], trans = 42},
{fin = [(N 433)], trans = 43},
{fin = [(N 262),(N 433)], trans = 25},
{fin = [(N 433),(N 472)], trans = 45},
{fin = [(N 433)], trans = 46},
{fin = [(N 256),(N 433)], trans = 25},
{fin = [(N 433),(N 472)], trans = 48},
{fin = [(N 433)], trans = 49},
{fin = [(N 433)], trans = 50},
{fin = [(N 252),(N 433)], trans = 25},
{fin = [(N 433)], trans = 52},
{fin = [(N 433)], trans = 53},
{fin = [(N 247),(N 433)], trans = 25},
{fin = [(N 433),(N 472)], trans = 55},
{fin = [(N 433)], trans = 56},
{fin = [(N 433)], trans = 57},
{fin = [(N 433)], trans = 58},
{fin = [(N 433)], trans = 59},
{fin = [(N 232),(N 433)], trans = 60},
{fin = [(N 433)], trans = 61},
{fin = [(N 433)], trans = 62},
{fin = [(N 242),(N 433)], trans = 25},
{fin = [(N 433)], trans = 64},
{fin = [(N 215),(N 433)], trans = 65},
{fin = [(N 433)], trans = 66},
{fin = [(N 433)], trans = 67},
{fin = [(N 433)], trans = 68},
{fin = [(N 433)], trans = 69},
{fin = [(N 433)], trans = 70},
{fin = [(N 225),(N 433)], trans = 25},
{fin = [(N 433)], trans = 72},
{fin = [(N 433)], trans = 73},
{fin = [(N 433)], trans = 74},
{fin = [(N 433)], trans = 75},
{fin = [(N 433)], trans = 76},
{fin = [(N 211),(N 433)], trans = 25},
{fin = [(N 433),(N 472)], trans = 78},
{fin = [(N 433)], trans = 79},
{fin = [(N 203),(N 433)], trans = 25},
{fin = [(N 433)], trans = 81},
{fin = [(N 433)], trans = 82},
{fin = [(N 433)], trans = 83},
{fin = [(N 199),(N 433)], trans = 25},
{fin = [(N 433),(N 472)], trans = 85},
{fin = [(N 433)], trans = 86},
{fin = [(N 433)], trans = 87},
{fin = [(N 433)], trans = 88},
{fin = [(N 433)], trans = 89},
{fin = [(N 193),(N 433)], trans = 25},
{fin = [(N 181),(N 433)], trans = 91},
{fin = [(N 433)], trans = 92},
{fin = [(N 186),(N 433)], trans = 25},
{fin = [(N 178),(N 433)], trans = 25},
{fin = [(N 433),(N 472)], trans = 95},
{fin = [(N 433)], trans = 96},
{fin = [(N 433)], trans = 97},
{fin = [(N 433)], trans = 98},
{fin = [(N 433)], trans = 99},
{fin = [(N 175),(N 433)], trans = 25},
{fin = [(N 433),(N 472)], trans = 101},
{fin = [(N 433)], trans = 102},
{fin = [(N 433)], trans = 103},
{fin = [(N 433)], trans = 104},
{fin = [(N 168),(N 433)], trans = 25},
{fin = [(N 433)], trans = 106},
{fin = [(N 162),(N 433)], trans = 25},
{fin = [(N 433),(N 472)], trans = 108},
{fin = [(N 137),(N 433)], trans = 109},
{fin = [(N 433)], trans = 110},
{fin = [(N 433)], trans = 111},
{fin = [(N 151),(N 433)], trans = 112},
{fin = [(N 158),(N 433)], trans = 25},
{fin = [(N 433)], trans = 114},
{fin = [(N 433)], trans = 115},
{fin = [(N 433)], trans = 116},
{fin = [(N 433)], trans = 117},
{fin = [(N 145),(N 433)], trans = 25},
{fin = [(N 134),(N 433)], trans = 25},
{fin = [(N 433),(N 472)], trans = 120},
{fin = [(N 433)], trans = 121},
{fin = [(N 433)], trans = 122},
{fin = [(N 433)], trans = 123},
{fin = [(N 433)], trans = 124},
{fin = [(N 131),(N 433)], trans = 25},
{fin = [(N 433),(N 472)], trans = 126},
{fin = [(N 433)], trans = 127},
{fin = [(N 116),(N 433)], trans = 128},
{fin = [(N 433)], trans = 129},
{fin = [(N 433)], trans = 130},
{fin = [(N 433)], trans = 131},
{fin = [(N 124),(N 433)], trans = 25},
{fin = [(N 112),(N 433)], trans = 25},
{fin = [(N 433),(N 472)], trans = 134},
{fin = [(N 433)], trans = 135},
{fin = [(N 433)], trans = 136},
{fin = [(N 433)], trans = 137},
{fin = [(N 433)], trans = 138},
{fin = [(N 433)], trans = 139},
{fin = [(N 433)], trans = 140},
{fin = [(N 433)], trans = 141},
{fin = [(N 109),(N 433)], trans = 25},
{fin = [(N 433)], trans = 143},
{fin = [(N 433)], trans = 144},
{fin = [(N 433)], trans = 145},
{fin = [(N 433)], trans = 146},
{fin = [(N 99),(N 433)], trans = 25},
{fin = [(N 433)], trans = 148},
{fin = [(N 92),(N 433)], trans = 25},
{fin = [(N 433)], trans = 150},
{fin = [(N 433)], trans = 151},
{fin = [(N 88),(N 433)], trans = 25},
{fin = [(N 433),(N 472)], trans = 153},
{fin = [(N 83),(N 433)], trans = 25},
{fin = [(N 433)], trans = 155},
{fin = [(N 433)], trans = 156},
{fin = [(N 433)], trans = 157},
{fin = [(N 433)], trans = 158},
{fin = [(N 433)], trans = 159},
{fin = [(N 433)], trans = 160},
{fin = [(N 80),(N 433)], trans = 25},
{fin = [(N 433),(N 472)], trans = 162},
{fin = [(N 433)], trans = 163},
{fin = [(N 433)], trans = 164},
{fin = [(N 71),(N 433)], trans = 25},
{fin = [(N 433),(N 472)], trans = 166},
{fin = [(N 66),(N 433)], trans = 25},
{fin = [(N 433)], trans = 168},
{fin = [(N 55),(N 433)], trans = 169},
{fin = [(N 433)], trans = 170},
{fin = [(N 433)], trans = 171},
{fin = [(N 433)], trans = 172},
{fin = [(N 63),(N 433)], trans = 25},
{fin = [(N 433)], trans = 174},
{fin = [(N 433)], trans = 175},
{fin = [(N 433)], trans = 176},
{fin = [(N 433)], trans = 177},
{fin = [(N 433)], trans = 178},
{fin = [(N 51),(N 433)], trans = 25},
{fin = [(N 436),(N 472)], trans = 7},
{fin = [(N 37),(N 472)], trans = 0},
{fin = [(N 35),(N 472)], trans = 0},
{fin = [(N 33),(N 472)], trans = 0},
{fin = [(N 28),(N 436),(N 472)], trans = 184},
{fin = [(N 31),(N 436)], trans = 7},
{fin = [(N 26),(N 472)], trans = 0},
{fin = [(N 21),(N 436),(N 472)], trans = 187},
{fin = [(N 24),(N 436)], trans = 7},
{fin = [(N 286),(N 289),(N 304),(N 472)], trans = 189},
{fin = [(N 289),(N 304)], trans = 189},
{fin = [(N 284),(N 304),(N 472)], trans = 191},
{fin = [], trans = 192},
{fin = [(N 304)], trans = 192},
{fin = [], trans = 194},
{fin = [], trans = 195},
{fin = [(N 314)], trans = 195},
{fin = [(N 314)], trans = 197},
{fin = [(N 304)], trans = 198},
{fin = [(N 472)], trans = 199},
{fin = [], trans = 200},
{fin = [(N 19)], trans = 0},
{fin = [(N 436),(N 472)], trans = 202},
{fin = [(N 15),(N 436)], trans = 7},
{fin = [(N 12),(N 472)], trans = 0},
{fin = [(N 10),(N 436),(N 472)], trans = 7},
{fin = [(N 8),(N 472)], trans = 0},
{fin = [(N 6),(N 472)], trans = 207},
{fin = [(N 458)], trans = 0},
{fin = [(N 428),(N 472)], trans = 209},
{fin = [(N 428)], trans = 209},
{fin = [(N 4),(N 436),(N 472)], trans = 211},
{fin = [], trans = 212},
{fin = [], trans = 213},
{fin = [], trans = 214},
{fin = [], trans = 215},
{fin = [], trans = 216},
{fin = [], trans = 217},
{fin = [], trans = 218},
{fin = [], trans = 219},
{fin = [], trans = 220},
{fin = [], trans = 221},
{fin = [(N 423)], trans = 0},
{fin = [(N 470),(N 472)], trans = 223},
{fin = [], trans = 223},
{fin = [], trans = 225},
{fin = [], trans = 226},
{fin = [], trans = 227},
{fin = [], trans = 228},
{fin = [], trans = 229},
{fin = [], trans = 230},
{fin = [], trans = 231},
{fin = [], trans = 232},
{fin = [], trans = 233},
{fin = [(N 382)], trans = 0},
{fin = [(N 2),(N 472)], trans = 235},
{fin = [(N 2)], trans = 235},
{fin = [(N 466)], trans = 0},
{fin = [(N 466)], trans = 238},
{fin = [(N 464)], trans = 0},
{fin = [(N 466)], trans = 240},
{fin = [(N 461)], trans = 0},
{fin = [(N 468)], trans = 0}])
end
structure StartStates =
        struct
        datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val COMMENT = STARTSTATE 3;
val INITIAL = STARTSTATE 1;

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

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0: int) =
        let fun action (i: int,nil) = raise LexError
        | action (i,nil::l) = action (i-1,l)
        | action (i,(node::acts)::l) =
                case node of
                    Internal.N yyk => 
                        (let fun yymktext() = String.substring(!yyb,i0,i-i0)
                             val yypos: int = i0+ !yygone
                        open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

                        (* Application actions *)

  10 => let val yytext=yymktext() in  token(STAR,      yypos, yytext)  end
| 109 => let val yytext=yymktext() in  token(EXCEPTION, yypos, yytext)  end
| 112 => let val yytext=yymktext() in  token(FN,        yypos, yytext)  end
| 116 => let val yytext=yymktext() in  token(FUN,       yypos, yytext)  end
| 12 => let val yytext=yymktext() in  token(COMMA,     yypos, yytext)  end
| 124 => let val yytext=yymktext() in  token(FUNCTOR,   yypos, yytext)  end
| 131 => let val yytext=yymktext() in  token(HANDLE,    yypos, yytext)  end
| 134 => let val yytext=yymktext() in  token(IF,        yypos, yytext)  end
| 137 => let val yytext=yymktext() in  token(IN,        yypos, yytext)  end
| 145 => let val yytext=yymktext() in  token(INCLUDE,   yypos, yytext)  end
| 15 => let val yytext=yymktext() in  token(ARROW,     yypos, yytext)  end
| 151 => let val yytext=yymktext() in  token(INFIX,     yypos, yytext)  end
| 158 => let val yytext=yymktext() in  token(INFIXR,    yypos, yytext)  end
| 162 => let val yytext=yymktext() in  token(LET,       yypos, yytext)  end
| 168 => let val yytext=yymktext() in  token(LOCAL,     yypos, yytext)  end
| 175 => let val yytext=yymktext() in  token(NONFIX,    yypos, yytext)  end
| 178 => let val yytext=yymktext() in  token(OF,        yypos, yytext)  end
| 181 => let val yytext=yymktext() in  token(OP,        yypos, yytext)  end
| 186 => let val yytext=yymktext() in  token(OPEN,      yypos, yytext)  end
| 19 => let val yytext=yymktext() in  token(DOTS,      yypos, yytext)  end
| 193 => let val yytext=yymktext() in  token(ORELSE,    yypos, yytext)  end
| 199 => let val yytext=yymktext() in  token(RAISE,     yypos, yytext)  end
| 2 => ( continue() )
| 203 => let val yytext=yymktext() in  token(REC,       yypos, yytext)  end
| 21 => let val yytext=yymktext() in  token(COLON,     yypos, yytext)  end
| 211 => let val yytext=yymktext() in  token(SHARING,   yypos, yytext)  end
| 215 => let val yytext=yymktext() in  token(SIG,       yypos, yytext)  end
| 225 => let val yytext=yymktext() in  token(SIGNATURE, yypos, yytext)  end
| 232 => let val yytext=yymktext() in  token(STRUCT,    yypos, yytext)  end
| 24 => let val yytext=yymktext() in  token(COLONGREATER, yypos, yytext)  end
| 242 => let val yytext=yymktext() in  token(STRUCTURE, yypos, yytext)  end
| 247 => let val yytext=yymktext() in  token(THEN,      yypos, yytext)  end
| 252 => let val yytext=yymktext() in  token(TYPE,      yypos, yytext)  end
| 256 => let val yytext=yymktext() in  token(VAL,       yypos, yytext)  end
| 26 => let val yytext=yymktext() in  token(SEMICOLON, yypos, yytext)  end
| 262 => let val yytext=yymktext() in  token(WHERE,     yypos, yytext)  end
| 268 => let val yytext=yymktext() in  token(WHILE,     yypos, yytext)  end
| 273 => let val yytext=yymktext() in  token(WITH,      yypos, yytext)  end
| 28 => let val yytext=yymktext() in  token(EQUALS,    yypos, yytext)  end
| 282 => let val yytext=yymktext() in  token(WITHTYPE,  yypos, yytext)  end
| 284 => let val yytext=yymktext() in  token  (ZERO,              yypos, yytext)  end
| 286 => let val yytext=yymktext() in  tokenOf(DIGIT,   toInt,    yypos, yytext)  end
| 289 => let val yytext=yymktext() in  tokenOf(NUMERIC, toInt,    yypos, yytext)  end
| 304 => let val yytext=yymktext() in  tokenOf(INT,     toInt,    yypos, yytext)  end
| 31 => let val yytext=yymktext() in  token(DARROW,    yypos, yytext)  end
| 314 => let val yytext=yymktext() in  tokenOf(WORD,    toWord,   yypos, yytext)  end
| 33 => let val yytext=yymktext() in  token(LBRACK,    yypos, yytext)  end
| 342 => let val yytext=yymktext() in  tokenOf(REAL,    toReal,   yypos, yytext)  end
| 35 => let val yytext=yymktext() in  token(RBRACK,    yypos, yytext)  end
| 37 => let val yytext=yymktext() in  token(UNDERBAR,  yypos, yytext)  end
| 382 => let val yytext=yymktext() in  tokenOf(STRING,  toString, yypos, yytext)  end
| 39 => let val yytext=yymktext() in  token(LBRACE,    yypos, yytext)  end
| 4 => let val yytext=yymktext() in  token(HASH,      yypos, yytext)  end
| 41 => let val yytext=yymktext() in  token(BAR,       yypos, yytext)  end
| 423 => let val yytext=yymktext() in  tokenOf(CHAR,    toChar,   yypos, yytext)  end
| 428 => let val yytext=yymktext() in  tokenOf(TYVAR,   toId,     yypos, yytext)  end
| 43 => let val yytext=yymktext() in  token(RBRACE,    yypos, yytext)  end
| 433 => let val yytext=yymktext() in  tokenOf(ALPHA,   toId,     yypos, yytext)  end
| 436 => let val yytext=yymktext() in  tokenOf(SYMBOL,  toId,     yypos, yytext)  end
| 455 => let val yytext=yymktext() in  tokenOf(LONGID,  toLongId, yypos, yytext)  end
| 458 => ( nesting := 1 ; YYBEGIN COMMENT ; continue() )
| 461 => ( nesting := !nesting+1 ; continue() )
| 464 => ( nesting := !nesting-1 ;
                             if !nesting = 0 then YYBEGIN INITIAL else () ;
                             continue() )
| 466 => ( continue() )
| 468 => ( continue() )
| 470 => let val yytext=yymktext() in  error(yypos, yytext, "invalid string")  end
| 472 => let val yytext=yymktext() in  invalid(yypos, yytext)  end
| 51 => let val yytext=yymktext() in  token(ABSTYPE,   yypos, yytext)  end
| 55 => let val yytext=yymktext() in  token(AND,       yypos, yytext)  end
| 6 => let val yytext=yymktext() in  token(LPAR,      yypos, yytext)  end
| 63 => let val yytext=yymktext() in  token(ANDALSO,   yypos, yytext)  end
| 66 => let val yytext=yymktext() in  token(AS,        yypos, yytext)  end
| 71 => let val yytext=yymktext() in  token(CASE,      yypos, yytext)  end
| 8 => let val yytext=yymktext() in  token(RPAR,      yypos, yytext)  end
| 80 => let val yytext=yymktext() in  token(DATATYPE,  yypos, yytext)  end
| 83 => let val yytext=yymktext() in  token(DO,        yypos, yytext)  end
| 88 => let val yytext=yymktext() in  token(ELSE,      yypos, yytext)  end
| 92 => let val yytext=yymktext() in  token(END,       yypos, yytext)  end
| 99 => let val yytext=yymktext() in  token(EQTYPE,    yypos, yytext)  end
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
                        if (l=i0) then UserDeclarations.eof ()
                                  else action(l,NewAcceptingLeaves))
                  else (if i0=l then yyb := newchars
                     else yyb := String.substring(!yyb,i0,l-i0)^newchars;
                     yygone := !yygone+i0;
                     yybl := String.size (!yyb);
                     scan (s,AcceptingLeaves,l-i0,0))
            end
          else let val NewChar = Char.ord(CharVector.sub(!yyb,l))
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
end
  in lex
  end
end
(* stop of Lexer.lex.sml *)
(* start of PARSE.sml *)
signature PARSE =
  sig

    (* Import *)

    type source  = Source.source
    type InfEnv  = Infix.InfEnv
    type Program = GrammarProgram.Program


    (* Export *)

    val parse: InfEnv * source -> InfEnv * Program

  end
(* stop of PARSE.sml *)
(* start of Parse.sml *)
structure Parse :> PARSE =
  struct

    (* Import *)

    type source  = Source.source
    type InfEnv  = Infix.InfEnv
    type Program = GrammarProgram.Program


    (* Build Yacc parser *)

    structure LrVals = LrValsFn(structure Token      = LrParser.Token)
    structure Lexer  = LexerFn (structure Tokens     = LrVals.Tokens)
    structure Parser = Join    (structure LrParser   = LrParser
                                structure ParserData = LrVals.ParserData
                                structure Lex        = Lexer)


    (* The actual parsing function *)

    fun parse(J, source) =
        let
            val yyread = ref false
            fun yyinput _ =
                if !yyread then
                    ""
                else
                    ( yyread := true; source )

            val lexer = Parser.makeLexer yyinput

            fun onError(s, pos1, pos2) = Error.error((pos1,pos2), s)

            val ((program,J'), lexer') = Parser.parse(0, lexer, onError, J)
        in
            (J',program)
        end

  end
(* stop of Parse.sml *)
(* start of SML.sml *)
(*
 * Standard ML implementation main structure
 *)

signature SML =
  sig

    val parseString:    string -> unit  (* Parse only *)
    val elabString:     string -> unit  (* Parse and elaborate *)
    val evalString:     string -> unit  (* Parse and evaluate *)
    val execString:     string -> unit  (* Parse, elaborate, and evaluate *)

    val parseFile:      string -> unit
    val elabFile:       string -> unit
    val evalFile:       string -> unit
    val execFile:       string -> unit

    val parseFiles:     string -> unit
    val elabFiles:      string -> unit
    val evalFiles:      string -> unit
    val execFiles:      string -> unit

    val parseSession:   unit -> unit
    val elabSession:    unit -> unit
    val evalSession:    unit -> unit
    val execSession:    unit -> unit

  end
(* stop of SML.sml *)
(* start of Sml.sml *)
(*
 * Standard ML implementation main structure
 *)

structure Sml :> SML =
  struct

    (* Initial arguments *)

    val J0      = InitialInfixEnv.J0
    val B_STAT0 = InitialStaticBasis.B0
    val B_DYN0  = InitialDynamicBasis.B0
    val B0      = (B_STAT0, B_DYN0)
    val s0      = InitialDynamicBasis.s


    (* Parsing only *)

    fun parse J source =
        let
            val (J',program) = Parse.parse(J, source)
            val    _         = TextIO.output(TextIO.stdOut, "OK\n")
        in
            J'
        end

    val parseInitialArg = J0
    val parseInitial    = parse parseInitialArg


    (* Parsing and elaboration *)

    val elabInitialArg = (J0, B_STAT0)

    fun elab (J, B_STAT) source =
        let
            val (J',program) = Parse.parse(J, source)
            val B_STAT'      = Program.elabProgram(B_STAT, program)
        in
            (J', B_STAT')
        end


    (* Parsing and evaluation *)

    val evalInitialArg = (J0, B_DYN0, s0)

    fun eval (J, B_DYN, s) source =
        let
            val (J',program) = Parse.parse(J, source)
            val s'           = ref s
            val B_DYN'       = Program.evalProgram(s', B_DYN, program)
        in
            (J', B_DYN', !s')
        end


    (* Parsing, elaboration, and evaluation *)

    val execInitialArg = (J0, B0, s0)

    fun exec (J, B, s) source =
        let
            val (J',program) = Parse.parse(J, source)
            val s'           = ref s
            val B'           = Program.execProgram(s', B, program)
        in
            (J', B', !s' )
        end


    (* Processing of strings *)

    fun processString (process, arg) source =
            ignore(process arg source)
            handle Error.Error _ => ()  (* Syntax error *)

    val parseString = processString(parse, parseInitialArg)
    val elabString  = processString(elab, elabInitialArg)
    val evalString  = processString(eval, evalInitialArg)
    val execString  = processString(exec, execInitialArg)


    (* Processing of files *)

    fun processFile (process, arg) name =
        let
            val file   = TextIO.openIn name
            val source = TextIO.inputAll file
            val _      = TextIO.closeIn file
        in
            ignore(process arg source)
            handle Error.Error _ => ()  (* Syntax error *)
        end

    val parseFile = processFile(parse, parseInitialArg)
    val elabFile  = processFile(elab, elabInitialArg)
    val evalFile  = processFile(eval, evalInitialArg)
    val execFile  = processFile(exec, execInitialArg)


    (* Processing several files mentioned in a list file *)

    fun processFiles (process, initialArg) name =
        let
            val file    = TextIO.openIn name
            val content = TextIO.inputAll file
            val _       = TextIO.closeIn file

            val _       = Stamp.reset()

            fun loop(arg,     [] )     = ()
              | loop(arg,  "" ::names) = loop(arg, names)
              | loop(arg, name::names) =
                let
                    val file   = TextIO.openIn name
                    val source = TextIO.inputAll file
                    val _      = TextIO.closeIn file
                    val _      = TextIO.output(TextIO.stdOut,
                                               ">> File \"" ^ name ^ "\":\n")
                in
                    loop(process arg source, names)
                    handle Error.Error _ =>     (* Syntax error *)
                        loop(arg, names)
                end
        in
            loop(initialArg, String.fields Char.isSpace content)
        end

    val parseFiles = processFiles(parse, parseInitialArg)
    val elabFiles  = processFiles(elab,  elabInitialArg)
    val evalFiles  = processFiles(eval,  evalInitialArg)
    val execFiles  = processFiles(exec,  execInitialArg)


    (* Session *)

    fun processSession(process, initialArg) =
        let
           val ins = !ins
            fun loop arg =
                let
                    val _      = TextIO.output(TextIO.stdOut, "SML> ")
                    val _      = TextIO.flushOut TextIO.stdOut
                in
                   case TextIO.inputLine ins of
                      NONE => ()
                    | SOME source =>
                        loop(process arg source)
                        handle Error.Error _ => (* Syntax error *)
                            loop arg
                end
        in
            loop initialArg
        end

    fun parseSession() = processSession(parse, parseInitialArg)
    fun elabSession()  = processSession(elab,  elabInitialArg)
    fun evalSession()  = processSession(eval,  evalInitialArg)
    fun execSession()  = processSession(exec,  execInitialArg)

  end
(* stop of Sml.sml *)
(* start of Main.sml *)
(*
 * Standard ML implementation stand-alone
 *)

structure Main =
  struct

    val version = "0.5"

    fun usage() =
        ( TextIO.output(TextIO.stdErr,
            "Usage: hamlet -<mode>\n\
            \where <mode> is one of:\n\
            \  h   help:       print this message\n\
            \  p   parse mode: just parse input\n\
            \  l   elab mode:  parse and elaborate\n\
            \  v   eval mode:  parse and evaluate (no type checking!)\n\
            \  x   exec mode:  parse, elaborate, and evaluate\n"
          )
        ; TextIO.flushOut TextIO.stdErr
        ; OS.Process.failure
        )

    fun start process =
        ( TextIO.output(TextIO.stdOut, "HaMLet " ^ version ^
                                       " - to be or not to be SML\n")
        ; TextIO.flushOut TextIO.stdOut
        ; process()
        ; TextIO.output(TextIO.stdOut, "\n")
        ; TextIO.flushOut TextIO.stdOut
        ; OS.Process.success
        )

    fun main' ["-h"] = ( usage() ; OS.Process.success )
      | main' ["-p"] = start Sml.parseSession
      | main' ["-l"] = start Sml.elabSession
      | main' ["-v"] = start Sml.evalSession
      | main' ["-x"] = start Sml.execSession
      | main'  _     = usage()

    fun main() = OS.Process.exit(main'(CommandLine.arguments()))

  end
(* stop of Main.sml *)

(* Here begins the simple test case. *)

structure Main =
   struct
      fun doit size =
         let
            open TextIO
            fun loop n =
               if n < 0
                  then ()
               else
                  let
                     val _ = ins := openIn "DATA/hamlet-input.sml"
                     val _ = Main.main' ["-x"]
                     val _ = closeIn (!ins)
                  in loop (n - 1)
                  end
         in 
            loop size
         end
   end
