(* splay-map-fn.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Functor implementing dictionaries using splay trees.
 *
 *)

functor SplayMapFn (K: ORD_KEY): ORD_MAP =
  struct
    structure Key = K
    open SplayTree

    datatype 'a map = 
        EMPTY
      | MAP of {
        root: (K.ord_key * 'a) splay ref,
        nobj: int
      }

    fun cmpf k (k', _) = K.compare(k', k)

    val empty = EMPTY
    
	(* Insert an item.  
	 *)
    fun insert (EMPTY, key, v) =
          MAP{nobj=1, root=ref(SplayObj{value=(key, v), left=SplayNil, right=SplayNil})}
      | insert (MAP{root,nobj}, key, v) =
          case splay (cmpf key, !root) of
            (EQUAL, SplayObj {left, right, ...}) => 
              MAP{nobj=nobj, root=ref(SplayObj{value=(key, v), left=left, right=right})}
          | (LESS, SplayObj{value, left, right}) => 
              MAP{
                nobj=nobj+1,
                root=ref(SplayObj{value=(key, v), left=SplayObj{value=value, left=left, right=SplayNil}, right=right})
              }
          | (GREATER, SplayObj{value, left, right}) => 
              MAP{
                nobj=nobj+1,
                root=ref(SplayObj{
                  value=(key, v),
                  left=left,
                  right=SplayObj{value=value, left=SplayNil, right=right}
                })
              }
          | (_, SplayNil) => raise LibBase.Impossible "SplayMapFn.insert SplayNil"
    fun insert' ((k, x), m) = insert(m, k, x)

  (* Look for an item, return NONE if the item doesn't exist *)
    fun find (EMPTY, _) = NONE
      | find (MAP {root, ...}, key) = (case splay (cmpf key, !root)
	   of (EQUAL, r as SplayObj{value, ...}) => (root := r; SOME(#2 value))
	    | (_, r) => (root := r; NONE))

	(* Remove an item.
         * Raise LibBase.NotFound if not found
	 *)
    fun remove (EMPTY, _) = raise LibBase.NotFound
      | remove (MAP{root,nobj}, key) = (case (splay (cmpf key, !root))
	 of (EQUAL, SplayObj{value, left, right}) => 
	      if nobj = 1
		then (EMPTY, #2 value)
		else (MAP{root=ref(join(left, right)),nobj=nobj-1}, #2 value)
	    | (_, r) => (root := r; raise LibBase.NotFound)
	  (* end case *))

	(* Return the number of items in the table *)
    fun numItems EMPTY = 0
      | numItems (MAP{nobj, ...}) = nobj

	(* Return a list of the items (and their keys) in the dictionary *)
    fun listItems EMPTY = []
      | listItems (MAP{root, ...}) = let
	  fun apply (SplayNil, l) = l
            | apply (SplayObj{value=(_, v), left, right}, l) =
                apply(left, v :: (apply (right, l)))
        in
          apply (!root, [])
        end
    fun listItemsi EMPTY = []
      | listItemsi (MAP{root, ...}) = let
	  fun apply (SplayNil, l) = l
            | apply (SplayObj{value, left, right}, l) =
                apply(left, value :: (apply (right, l)))
        in
          apply (!root, [])
        end

    local
      fun next ((t as SplayObj{right, ...}) :: rest) = (t, left(right, rest))
	| next _ = (SplayNil, [])
      and left (SplayNil, rest) = rest
	| left (t as SplayObj{left=l, ...}, rest) = left(l, t :: rest)
    in
    fun collate _ (EMPTY, EMPTY) = EQUAL
      | collate _ (EMPTY, _) = LESS
      | collate _ (_, EMPTY) = GREATER
      | collate cmpRng (MAP{root=s1, ...}, MAP{root=s2, ...}) = let
	  fun cmp (t1, t2) = (case (next t1, next t2)
		 of ((SplayNil, _), (SplayNil, _)) => EQUAL
		  | ((SplayNil, _), _) => LESS
		  | (_, (SplayNil, _)) => GREATER
		  | ((SplayObj{value=(x1, y1), ...}, r1),
		     (SplayObj{value=(x2, y2), ...}, r2)
		    ) => (
		      case Key.compare(x1, x2)
		       of EQUAL => (case cmpRng (y1, y2)
			     of EQUAL => cmp (r1, r2)
			      | order => order
			    (* end case *))
			| order => order
		      (* end case *))
		(* end case *))
	  in
	    cmp (left(!s1, []), left(!s2, []))
	  end
    end (* local *)

	(* Apply a function to the entries of the dictionary *)
    fun appi _ EMPTY = ()
      | appi af (MAP{root, ...}) =
          let fun apply SplayNil = ()
                | apply (SplayObj{value, left, right}) = 
                    (apply left; af value; apply right)
        in
          apply (!root)
        end

    fun app _ EMPTY = ()
      | app af (MAP{root, ...}) =
          let fun apply SplayNil = ()
                | apply (SplayObj{value=(_, value), left, right}) = 
                    (apply left; af value; apply right)
        in
          apply (!root)
        end
(*
    fun revapp af (MAP{root, ...}) =
          let fun apply SplayNil = ()
                | apply (SplayObj{value, left, right}) = 
                    (apply right; af value; apply left)
        in
          apply (!root)
        end
*)

	(* Fold function *)
    fun foldri (_: K.ord_key * 'a * 'b -> 'b) b EMPTY = b
      | foldri (abf: K.ord_key * 'a * 'b -> 'b) b (MAP{root, ...}) =
          let fun apply (SplayNil: (K.ord_key * 'a) splay, b) = b
                | apply (SplayObj{value, left, right}, b) =
                    apply(left, abf(#1 value, #2 value, apply(right, b)))
        in
          apply (!root, b)
        end

    fun foldr (_: 'a * 'b -> 'b) b EMPTY = b
      | foldr (abf: 'a * 'b -> 'b) b (MAP{root, ...}) =
          let fun apply (SplayNil: (K.ord_key * 'a) splay, b) = b
                | apply (SplayObj{value=(_, value), left, right}, b) =
                    apply(left, abf(value, apply(right, b)))
        in
          apply (!root, b)
        end

    fun foldli (_: K.ord_key * 'a * 'b -> 'b) b EMPTY = b
      | foldli (abf: K.ord_key * 'a * 'b -> 'b) b (MAP{root, ...}) =
          let fun apply (SplayNil: (K.ord_key * 'a) splay, b) = b
                | apply (SplayObj{value, left, right}, b) =
                    apply(right, abf(#1 value, #2 value, apply(left, b)))
        in
          apply (!root, b)
        end

    fun foldl (_: 'a * 'b -> 'b) b EMPTY = b
      | foldl (abf: 'a * 'b -> 'b) b (MAP{root, ...}) =
          let fun apply (SplayNil: (K.ord_key * 'a) splay, b) = b
                | apply (SplayObj{value=(_, value), left, right}, b) =
                    apply(right, abf(value, apply(left, b)))
        in
          apply (!root, b)
        end

	(* Map a table to a new table that has the same keys*)
    fun mapi (_: K.ord_key * 'a -> 'b) EMPTY = EMPTY
      | mapi (af: K.ord_key * 'a -> 'b) (MAP{root,nobj}) =
          let fun ap (SplayNil: (K.ord_key * 'a) splay) = SplayNil
                | ap (SplayObj{value, left, right}) = let
                    val left' = ap left
                    val value' = (#1 value, af value)
                    in
                      SplayObj{value = value', left = left', right = ap right}
                    end
        in
          MAP{root = ref(ap (!root)), nobj = nobj}
        end

    fun map (_: 'a -> 'b) EMPTY = EMPTY
      | map (af: 'a -> 'b) (MAP{root,nobj}) =
          let fun ap (SplayNil: (K.ord_key * 'a) splay) = SplayNil
                | ap (SplayObj{value, left, right}) = let
                    val left' = ap left
                    val value' = (#1 value, af (#2 value))
                    in
                      SplayObj{value = value', left = left', right = ap right}
                    end
        in
          MAP{root = ref(ap (!root)), nobj = nobj}
        end

(* the following are generic implementations of the unionWith and intersectWith
 * operetions.  These should be specialized for the internal representations
 * at some point.
 *)
    fun unionWith f (m1, m2) = let
	  fun ins f (key, x, m) = (case find(m, key)
		 of NONE => insert(m, key, x)
		  | (SOME x') => insert(m, key, f(x, x'))
		(* end case *))
	  in
	    if (numItems m1 > numItems m2)
	      then foldli (ins (fn (a, b) => f(b, a))) m1 m2
	      else foldli (ins f) m2 m1
	  end
    fun unionWithi f (m1, m2) = let
	  fun ins f (key, x, m) = (case find(m, key)
		 of NONE => insert(m, key, x)
		  | (SOME x') => insert(m, key, f(key, x, x'))
		(* end case *))
	  in
	    if (numItems m1 > numItems m2)
	      then foldli (ins (fn (k, a, b) => f(k, b, a))) m1 m2
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

  end (* SplayDictFn *)
