(* Modified by mfluet@acm.org on 2005-8-01.
 * Update with SML/NJ 110.55+.
 *)
(* Modified by sweeks@acm.org on 2000-8-24.
 * Ported to MLton.
 *)
type int = Int.int

(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

(* Implementation of ordered sets using ordered lists and red-black trees.  The
   code for red-black trees was originally written by Norris Boyd, which was
   modified for use here.
*)   

(* ordered sets implemented using ordered lists.

   Upper bound running times for functions implemented here:

   app  = O(n)
   card = O(n)
   closure = O(n^2)
   difference = O(n+m), where n,m = the size of the two sets used here.
   empty = O(1)
   exists = O(n)
   find = O(n)
   fold = O(n)
   insert = O(n)
   is_empty = O(1)
   make_list = O(1)
   make_set = O(n^2)
   partition = O(n)
   remove = O(n)
   revfold = O(n)
   select_arb = O(1)
   set_eq = O(n), where n = the cardinality of the smaller set
   set_gt = O(n), ditto
   singleton = O(1)
   union = O(n+m)
*)

functor ListOrdSet(B : sig type elem
                        val gt : elem * elem -> bool
                        val eq : elem * elem -> bool
                    end ) : ORDSET =

struct
 type elem = B.elem
 val elem_gt = B.gt
 val elem_eq = B.eq 

 type set = elem list
 exception Select_arb
 val empty = nil

 val insert = fn (key,s) =>
        let fun f (l as (h::t)) =
                 if elem_gt(key,h) then h::(f t)
                 else if elem_eq(key,h) then key::t
                 else key::l
              | f nil = [key]
        in f s
        end
                
 val select_arb = fn nil => raise Select_arb
                   | a::b => a

 val exists = fn (key,s) =>
        let fun f (h::t) = if elem_gt(key,h) then f t
                           else elem_eq(h,key) 
              | f nil = false
        in f s
        end

 val find = fn (key,s) =>
        let fun f (h::t) = if elem_gt(key,h) then f t
                           else if elem_eq(h,key) then SOME h
                           else NONE
              | f nil = NONE
        in f s
        end
   
 fun revfold f lst init = List.foldl f init lst
 fun fold f lst init = List.foldr f init lst
 val app = List.app

fun set_eq(h::t,h'::t') = 
        (case elem_eq(h,h')
          of true => set_eq(t,t')
           | a => a)
  | set_eq(nil,nil) = true
  | set_eq _ = false

fun set_gt(h::t,h'::t') =
        (case elem_gt(h,h')
          of false => (case (elem_eq(h,h'))
                        of true => set_gt(t,t')
                         | a => a)
           |  a => a)
  | set_gt(_::_,nil) = true
  | set_gt _ = false
                
fun union(a as (h::t),b as (h'::t')) =
          if elem_gt(h',h) then h::union(t,b)
          else if elem_eq(h,h') then h::union(t,t')
          else h'::union(a,t')
  | union(nil,s) = s
  | union(s,nil) = s

val make_list = fn s => s

val is_empty = fn nil => true | _ => false

val make_set = fn l => List.foldr insert [] l

val partition = fn f => fn s =>
    fold (fn (e,(yes,no)) =>
            if (f e) then (e::yes,no) else (e::no,yes)) s (nil,nil)

val remove = fn (e,s) =>
    let fun f (l as (h::t)) = if elem_gt(h,e) then l
                              else if elem_eq(h,e) then t
                              else h::(f t)
          | f nil = nil
    in f s
    end

 (* difference: X-Y *)

 fun difference (nil,_) = nil
   | difference (r,nil) = r
   | difference (a as (h::t),b as (h'::t')) =
          if elem_gt (h',h) then h::difference(t,b)
          else if elem_eq(h',h) then difference(t,t')
          else difference(a,t')

 fun singleton X = [X]

 fun card(S): int = fold (fn (a,count) => count+1) S 0

      local
            fun closure'(from, f, result) =
              if is_empty from then result
              else
                let val (more,result) =
                        fold (fn (a,(more',result')) =>
                                let val more = f a
                                    val new = difference(more,result)
                                in (union(more',new),union(result',new))
                                end) from
                                 (empty,result)
                in closure'(more,f,result)
                end
      in
         fun closure(start, f) = closure'(start, f, start)
      end
end

(* ordered set implemented using red-black trees:

   Upper bound running time of the functions below:

   app: O(n)
   card: O(n)
   closure: O(n^2 ln n)
   difference: O(n ln n)
   empty: O(1)
   exists: O(ln n)
   find: O(ln n)
   fold: O(n)
   insert: O(ln n)
   is_empty: O(1)
   make_list: O(n)
   make_set: O(n ln n)
   partition: O(n ln n)
   remove: O(n ln n)
   revfold: O(n)
   select_arb: O(1)
   set_eq: O(n)
   set_gt: O(n)
   singleton: O(1)
   union: O(n ln n)
*)

functor RbOrdSet (B : sig type elem
                         val eq : (elem*elem) -> bool
                         val gt : (elem*elem) -> bool
                     end
                ) : ORDSET =
struct

 type elem = B.elem
 val elem_gt = B.gt
 val elem_eq = B.eq 

 datatype Color = RED | BLACK

 abstype set = EMPTY | TREE of (B.elem * Color * set * set)
 with exception Select_arb
      val empty = EMPTY

 fun insert(key,t) =
  let fun f EMPTY = TREE(key,RED,EMPTY,EMPTY)
        | f (TREE(k,BLACK,l,r)) =
            if elem_gt (key,k)
            then case f r
                 of r as TREE(rk,RED, rl as TREE(rlk,RED,rll,rlr),rr) =>
                        (case l
                         of TREE(lk,RED,ll,lr) =>
                                TREE(k,RED,TREE(lk,BLACK,ll,lr),
                                           TREE(rk,BLACK,rl,rr))
                          | _ => TREE(rlk,BLACK,TREE(k,RED,l,rll),
                                                TREE(rk,RED,rlr,rr)))
                  | r as TREE(rk,RED,rl, rr as TREE(rrk,RED,rrl,rrr)) =>
                        (case l
                         of TREE(lk,RED,ll,lr) =>
                                TREE(k,RED,TREE(lk,BLACK,ll,lr),
                                           TREE(rk,BLACK,rl,rr))
                          | _ => TREE(rk,BLACK,TREE(k,RED,l,rl),rr))
                  | r => TREE(k,BLACK,l,r)
            else if elem_gt(k,key)
            then case f l
                 of l as TREE(lk,RED,ll, lr as TREE(lrk,RED,lrl,lrr)) =>
                        (case r
                         of TREE(rk,RED,rl,rr) =>
                                TREE(k,RED,TREE(lk,BLACK,ll,lr),
                                           TREE(rk,BLACK,rl,rr))
                          | _ => TREE(lrk,BLACK,TREE(lk,RED,ll,lrl),
                                                TREE(k,RED,lrr,r)))
                  | l as TREE(lk,RED, ll as TREE(llk,RED,lll,llr), lr) =>
                        (case r
                         of TREE(rk,RED,rl,rr) =>
                                TREE(k,RED,TREE(lk,BLACK,ll,lr),
                                           TREE(rk,BLACK,rl,rr))
                          | _ => TREE(lk,BLACK,ll,TREE(k,RED,lr,r)))
                  | l => TREE(k,BLACK,l,r)
            else TREE(key,BLACK,l,r)
        | f (TREE(k,RED,l,r)) =
            if elem_gt(key,k) then TREE(k,RED,l, f r)
            else if elem_gt(k,key) then TREE(k,RED, f l, r)
            else TREE(key,RED,l,r)
   in case f t
      of TREE(k,RED, l as TREE(_,RED,_,_), r) => TREE(k,BLACK,l,r)
       | TREE(k,RED, l, r as TREE(_,RED,_,_)) => TREE(k,BLACK,l,r)
       | t => t
  end

 fun select_arb (TREE(k,_,l,r)) = k
   | select_arb EMPTY = raise Select_arb
   
 fun exists(key,t) =
  let fun look EMPTY = false
        | look (TREE(k,_,l,r)) =
                if elem_gt(k,key) then look l
                else if elem_gt(key,k) then look r
                else true
   in look t
   end

 fun find(key,t) =
  let fun look EMPTY = NONE
        | look (TREE(k,_,l,r)) =
                if elem_gt(k,key) then look l
                else if elem_gt(key,k) then look r
                else SOME k
   in look t
  end

  fun revfold f t start =
     let fun scan (EMPTY,value) = value
           | scan (TREE(k,_,l,r),value) = scan(r,f(k,scan(l,value)))
     in scan(t,start)
     end

   fun fold f t start =
        let fun scan(EMPTY,value) = value
              | scan(TREE(k,_,l,r),value) = scan(l,f(k,scan(r,value)))
        in scan(t,start)
        end

   fun app f t =
      let fun scan EMPTY = ()
            | scan(TREE(k,_,l,r)) = (scan l; f k; scan r)
      in scan t
      end

(* equal_tree : test if two trees are equal.  Two trees are equal if
   the set of leaves are equal *)

   fun set_eq (tree1 as (TREE _),tree2 as (TREE _)) =
     let datatype pos = L | R | M
         exception Done
         fun getvalue(stack as ((a,position)::b)) =
            (case a
             of (TREE(k,_,l,r)) =>
                (case position
                 of L => getvalue ((l,L)::(a,M)::b)
                  | M => (k,case r of  EMPTY => b | _ => (a,R)::b)
                  | R => getvalue ((r,L)::b)
                 )
              | EMPTY => getvalue b
             )
            | getvalue(nil) = raise Done
          fun f (nil,nil) = true
            | f (s1 as (_ :: _),s2 as (_ :: _ )) =
                          let val (v1,news1) = getvalue s1
                              and (v2,news2) = getvalue s2
                          in (elem_eq(v1,v2)) andalso f(news1,news2)
                          end
            | f _ = false
      in f ((tree1,L)::nil,(tree2,L)::nil) handle Done => false
      end
    | set_eq (EMPTY,EMPTY) = true
    | set_eq _ = false

   (* gt_tree : Test if tree1 is greater than tree 2 *)

   fun set_gt (tree1,tree2) =
     let datatype pos = L | R | M
         exception Done
         fun getvalue(stack as ((a,position)::b)) =
            (case a
             of (TREE(k,_,l,r)) =>
                (case position
                 of L => getvalue ((l,L)::(a,M)::b)
                  | M => (k,case r of EMPTY => b | _ => (a,R)::b)
                  | R => getvalue ((r,L)::b)
                 )
              | EMPTY => getvalue b
             )
            | getvalue(nil) = raise Done
          fun f (nil,nil) = false
            | f (s1 as (_ :: _),s2 as (_ :: _ )) =
                          let val (v1,news1) = getvalue s1
                              and (v2,news2) = getvalue s2
                          in (elem_gt(v1,v2)) orelse (elem_eq(v1,v2) andalso f(news1,news2))
                          end
            | f (_,nil) = true
            | f (nil,_) = false
      in f ((tree1,L)::nil,(tree2,L)::nil) handle Done => false
      end

      fun is_empty S = (let val _ = select_arb S in false end
                         handle Select_arb => true)

      fun make_list S = fold (op ::) S nil

      fun make_set l = List.foldr insert empty l

      fun partition F S = fold (fn (a,(Yes,No)) =>
                                if F(a) then (insert(a,Yes),No)
                                else (Yes,insert(a,No)))
                             S (empty,empty)

      fun remove(X, XSet) =
             let val (YSet, _) =
                        partition (fn a => not (elem_eq (X, a))) XSet
             in  YSet
             end

      fun difference(Xs, Ys) =
           fold (fn (p as (a,Xs')) =>
                      if exists(a,Ys) then Xs' else insert p)
           Xs empty

      fun singleton X = insert(X,empty)

      fun card(S): int = fold (fn (_,count) => count+1) S 0

      fun union(Xs,Ys)= fold insert Ys Xs

      local
            fun closure'(from, f, result) =
              if is_empty from then result
              else
                let val (more,result) =
                        fold (fn (a,(more',result')) =>
                                let val more = f a
                                    val new = difference(more,result)
                                in (union(more',new),union(result',new))
                                end) from
                                 (empty,result)
                in closure'(more,f,result)
                end
      in
         fun closure(start, f) = closure'(start, f, start)
      end
   end
end

(* In utils.sig
signature TABLE =
   sig
        type 'a table
        type key
        val size : 'a table -> int
        val empty: 'a table
        val exists: (key * 'a table) -> bool
        val find : (key * 'a table)  ->  'a option
        val insert: ((key * 'a) * 'a table) -> 'a table
        val make_table : (key * 'a ) list -> 'a table
        val make_list : 'a table -> (key * 'a) list
        val fold : ((key * 'a) * 'b -> 'b) -> 'a table -> 'b -> 'b
   end
*)

functor Table (B : sig type key
                      val gt : (key * key) -> bool
                     end
                ) : TABLE =
struct

 datatype Color = RED | BLACK
 type key = B.key

 abstype 'a table = EMPTY
                  | TREE of ((B.key * 'a ) * Color * 'a table * 'a table)
 with

 val empty = EMPTY

 fun insert(elem as (key,data),t) =
  let val key_gt = fn (a,_) => B.gt(key,a)
      val key_lt = fn (a,_) => B.gt(a,key)
        fun f EMPTY = TREE(elem,RED,EMPTY,EMPTY)
        | f (TREE(k,BLACK,l,r)) =
            if key_gt k
            then case f r
                 of r as TREE(rk,RED, rl as TREE(rlk,RED,rll,rlr),rr) =>
                        (case l
                         of TREE(lk,RED,ll,lr) =>
                                TREE(k,RED,TREE(lk,BLACK,ll,lr),
                                           TREE(rk,BLACK,rl,rr))
                          | _ => TREE(rlk,BLACK,TREE(k,RED,l,rll),
                                                TREE(rk,RED,rlr,rr)))
                  | r as TREE(rk,RED,rl, rr as TREE(rrk,RED,rrl,rrr)) =>
                        (case l
                         of TREE(lk,RED,ll,lr) =>
                                TREE(k,RED,TREE(lk,BLACK,ll,lr),
                                           TREE(rk,BLACK,rl,rr))
                          | _ => TREE(rk,BLACK,TREE(k,RED,l,rl),rr))
                  | r => TREE(k,BLACK,l,r)
            else if key_lt k
            then case f l
                 of l as TREE(lk,RED,ll, lr as TREE(lrk,RED,lrl,lrr)) =>
                        (case r
                         of TREE(rk,RED,rl,rr) =>
                                TREE(k,RED,TREE(lk,BLACK,ll,lr),
                                           TREE(rk,BLACK,rl,rr))
                          | _ => TREE(lrk,BLACK,TREE(lk,RED,ll,lrl),
                                                TREE(k,RED,lrr,r)))
                  | l as TREE(lk,RED, ll as TREE(llk,RED,lll,llr), lr) =>
                        (case r
                         of TREE(rk,RED,rl,rr) =>
                                TREE(k,RED,TREE(lk,BLACK,ll,lr),
                                           TREE(rk,BLACK,rl,rr))
                          | _ => TREE(lk,BLACK,ll,TREE(k,RED,lr,r)))
                  | l => TREE(k,BLACK,l,r)
            else TREE(elem,BLACK,l,r)
        | f (TREE(k,RED,l,r)) =
            if key_gt k then TREE(k,RED,l, f r)
            else if key_lt k then TREE(k,RED, f l, r)
            else TREE(elem,RED,l,r)
   in case f t
      of TREE(k,RED, l as TREE(_,RED,_,_), r) => TREE(k,BLACK,l,r)
       | TREE(k,RED, l, r as TREE(_,RED,_,_)) => TREE(k,BLACK,l,r)
       | t => t
  end

 fun exists(key,t) =
  let fun look EMPTY = false
        | look (TREE((k,_),_,l,r)) =
                if B.gt(k,key) then look l
                else if B.gt(key,k) then look r
                else true
   in look t
   end

 fun find(key,t) =
  let fun look EMPTY = NONE
        | look (TREE((k,data),_,l,r)) =
                if B.gt(k,key) then look l
                else if B.gt(key,k) then look r
                else SOME data
   in look t
  end

  fun fold f t start =
        let fun scan(EMPTY,value) = value
              | scan(TREE(k,_,l,r),value) = scan(l,f(k,scan(r,value)))
        in scan(t,start)
        end

  fun make_table l = List.foldr insert empty l

  fun size S : int = fold (fn (_,count) => count+1) S 0

  fun make_list table = fold (op ::) table nil

  end
end;

(* assumes that a functor Table with signature TABLE from table.sml is
   in the environment *)

(* In utils.sig
signature HASH =
  sig
    type table
    type elem

    val size : table -> int
    val add : elem * table -> table
    val find : elem * table -> int option
    val exists : elem * table -> bool
    val empty : table
  end
*)

(* hash: creates a hash table of size n which assigns each distinct member
   a unique integer between 0 and n-1 *)

functor Hash(B : sig type elem
                     val gt : elem * elem -> bool
                 end) : HASH =
struct
    type elem=B.elem
    structure HashTable = Table(type key=B.elem
                                val gt = B.gt)

    type table = {count : int, table : int HashTable.table}

    val empty: table = {count=0,table=HashTable.empty}
    val size = fn {count,table} => count
    val add = fn (e,{count,table}) =>
       ({count=count+1,table=HashTable.insert((e,count),table)}: table)
    val find = fn (e,{table,count}) => HashTable.find(e,table)
    val exists = fn (e,{table,count}) => HashTable.exists(e,table)
end;
