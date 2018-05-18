(* ML-Yacc Parser Generator (c) 1991 Andrew W. Appel, David R. Tarditi *)

signature SORT_ARG =
  sig
     type entry
     val gt : entry * entry -> bool
  end
signature SORT =
  sig
     type entry
     val sort : entry list -> entry list
  end
signature EQUIV_ARG =
   sig
     type entry
     val gt : entry * entry -> bool
     val eq : entry * entry -> bool
   end
signature EQUIV =
  sig
     type entry

     (* equivalences: take a list of entries and divides them into
        equivalence classes numbered 0 to n-1.

        It returns a triple consisting of:

          * the number of equivalence classes
          * a list which maps each original entry to an equivalence
            class.  The nth entry in this list gives the equivalence
            class for the nth entry in the original entry list.
          * a list which maps equivalence classes to some representative
            element.  The nth entry in this list is an element from the
            nth equivalence class
      *)

     val equivalences : entry list -> (int * int list * entry list)
  end

(* An O(n lg n) merge sort routine *)

functor MergeSortFun(A : SORT_ARG) : SORT =
  struct
      type entry = A.entry

      (* sort: an O(n lg n) merge sort routine.  We create a list of lists
         and then merge these lists in passes until only one list is left.*)

      fun sort nil = nil
        | sort l =
             let (* merge: merge two lists *)

                 fun merge (l as a::at,r as b::bt) =
                       if A.gt(a,b)
                       then b :: merge(l,bt)
                       else a :: merge(at,r)
                   | merge (l,nil) = l
                   | merge (nil,r) = r

                 (* scan: merge pairs of lists on a list of lists.
                    Reduces the number of lists by about 1/2 *)

                 fun scan (a :: b :: rest) = merge(a,b) :: scan rest
                   | scan l = l

                 (* loop: calls scan on a list of lists until only
                    one list is left.  It terminates only if the list of
                    lists is nonempty.  (The pattern match for sort
                    ensures this.) *)

                 fun loop (a :: nil) = a
                   | loop l = loop (scan l)

              in loop (map (fn a => [a]) l)
              end
   end

(* an O(n lg n) routine for placing items in equivalence classes *)

functor EquivFun(A : EQUIV_ARG) : EQUIV =
   struct
       val sub = Array.sub
       infix 9 sub

      (* Our algorithm for finding equivalence class is simple.  The basic
         idea is to sort the entries and place duplicates entries in the same
          equivalence class.

         Let the original entry list be E.  We map E to a list of a pairs
         consisting of the entry and its position in E, where the positions
         are numbered 0 to n-1.  Call this list of pairs EP.

         We then sort EP on the original entries.  The second elements in the
         pairs now specify a permutation that will return us to EP.

         We then scan the sorted list to create a list R of representative
         entries, a list P of integers which permutes the sorted list back to
         the original list and a list SE of integers  which gives the
         equivalence class for the nth entry in the sorted list .

         We then return the length of R, R, and the list that results from
         permuting SE by P.
     *)

       type entry = A.entry

       val gt = fn ((a,_),(b,_)) => A.gt(a,b)

       structure Sort = MergeSortFun(type entry = A.entry * int
                                     val gt = gt)
       val assignIndex =
          fn l =>
             let fun loop (index,nil) = nil
                   | loop (index,h :: t) = (h,index) :: loop(index+1,t)
             in loop (0,l)
             end

       local fun loop ((e,_) :: t, prev, class, R , SE) =
               if A.eq(e,prev)
                 then loop(t,e,class,R, class :: SE)
                 else loop(t,e,class+1,e :: R, (class + 1) :: SE)
             | loop (nil,_,_,R,SE) = (rev R, rev SE)
       in val createEquivalences =
           fn nil => (nil,nil)
            | (e,_) :: t => loop(t, e, 0, [e],[0])
       end

       val inversePermute = fn permutation =>
              fn nil => nil
               | l as h :: _ =>
                   let val result = Array.array(length l,h)
                       fun loop (elem :: r, dest :: s) =
                             (Array.update(result,dest,elem); loop(r,s))
                         | loop _ = ()
                       fun listofarray i =
                          if i < Array.length result then
                                (result sub i) :: listofarray (i+1)
                          else nil
                    in loop (l,permutation); listofarray 0
                    end

       fun makePermutation x = map (fn (_,b) => b) x

       val equivalences = fn l =>
           let val EP = assignIndex l
               val sorted = Sort.sort EP
               val P = makePermutation sorted
               val (R, SE) = createEquivalences sorted
            in (length R, inversePermute P SE, R)
            end
end

functor ShrinkLrTableFun(structure LrTable : LR_TABLE) : SHRINK_LR_TABLE =
    struct
        structure LrTable = LrTable
        open LrTable
        val gtAction = fn (a,b) =>
              case a
              of SHIFT (STATE s) =>
                   (case b of SHIFT (STATE s') => s>s' | _ => true)
               | REDUCE i => (case b of SHIFT _ => false | REDUCE i' => i>i'
                                      | _ => true)
               | ACCEPT => (case b of ERROR => true | _ => false)
               | ERROR =>  false
        structure ActionEntryList =
            struct
                type entry = (term, action) pairlist * action
                local
                    fun eqlist (EMPTY, EMPTY) = true
                      | eqlist (PAIR (T t,d,r),PAIR(T t',d',r')) =
                        t=t' andalso d=d' andalso eqlist(r,r')
                      | eqlist _ = false
                    fun gtlist (PAIR _,EMPTY) = true
                      | gtlist (PAIR(T t,d,r),PAIR(T t',d',r')) =
                        t>t' orelse (t=t' andalso
                                     (gtAction(d,d') orelse
                                      (d=d' andalso gtlist(r,r'))))
                      | gtlist _ = false
                in
                    fun eq ((l,a): entry, (l',a'): entry) =
                        a = a' andalso eqlist (l,l')
                    fun gt ((l,a): entry, (l',a'): entry) =
                        gtAction(a,a') orelse (a=a' andalso gtlist(l,l'))
                end
            end
(*        structure GotoEntryList =
            struct
               type entry = (nonterm,state) pairlist
               val rec eq =
                    fn (EMPTY,EMPTY) => true
                     | (PAIR (t,d,r),PAIR(t',d',r')) =>
                            t=t' andalso d=d' andalso eq(r,r')
                     | _ => false
               val rec gt =
                    fn (PAIR _,EMPTY) => true
                     | (PAIR(NT t,STATE d,r),PAIR(NT t',STATE d',r')) =>
                           t>t' orelse (t=t' andalso
                           (d>d' orelse (d=d' andalso gt(r,r'))))
                     | _ => false
            end *)
        structure EquivActionList = EquivFun(ActionEntryList)
        val states = fn max =>
            let fun f i=if i<max then STATE i :: f(i+1) else nil
            in f 0
            end
        val length : ('a,'b) pairlist -> int =
           fn l =>
             let fun g(EMPTY,len) = len
                   | g(PAIR(_,_,r),len) = g(r,len+1)
             in g(l,0)
             end
        val size : (('a,'b) pairlist * 'c) list -> int =
           fn l =>
             let val c = ref 0
             in (app (fn (row,_) => c := !c + length row) l; !c)
             end
       val shrinkActionList =
         fn (table,verbose) =>
           case EquivActionList.equivalences
                     (map (describeActions table) (states (numStates table)))
           of result as (_,_,l) => (result,if verbose then size l else 0)
end;
