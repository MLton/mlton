(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure SeqIndex =
   struct
      open SeqIndex

      val maxLen' = maxInt'
      local
         structure S =
            Int_ChooseInt
            (type 'a t = 'a -> int
             val fInt8 = SeqIndex.sextdFromInt8
             val fInt16 = SeqIndex.sextdFromInt16
             val fInt32 = SeqIndex.sextdFromInt32
             val fInt64 = SeqIndex.sextdFromInt64
             val fIntInf = SeqIndex.sextdFromIntInf)
      in
         val fromIntUnsafe = S.f
      end
      local
         structure S =
            Int_ChooseInt
            (type 'a t = 'a -> int
             val fInt8 = SeqIndex.schckFromInt8
             val fInt16 = SeqIndex.schckFromInt16
             val fInt32 = SeqIndex.schckFromInt32
             val fInt64 = SeqIndex.schckFromInt64
             val fIntInf = SeqIndex.schckFromIntInf)
      in
         val fromInt = S.f
      end
      local
         structure S =
            Int_ChooseInt
            (type 'a t = int -> 'a
             val fInt8 = SeqIndex.sextdToInt8
             val fInt16 = SeqIndex.sextdToInt16
             val fInt32 = SeqIndex.sextdToInt32
             val fInt64 = SeqIndex.sextdToInt64
             val fIntInf = SeqIndex.sextdToIntInf)
      in
         val toIntUnsafe = S.f
      end
      local
         structure S =
            Int_ChooseInt
            (type 'a t = int -> 'a
             val fInt8 = SeqIndex.schckToInt8
             val fInt16 = SeqIndex.schckToInt16
             val fInt32 = SeqIndex.schckToInt32
             val fInt64 = SeqIndex.schckToInt64
             val fIntInf = SeqIndex.schckToIntInf)
      in
         val toInt = S.f
      end
   end

functor Sequence (S: sig
                        type 'a sequence 
                        type 'a elt
                        (* fromArray should be constant time. *)
                        val fromArray: 'a elt array -> 'a sequence 
                        val isMutable: bool
                        val length: 'a sequence -> SeqIndex.int
                        val subUnsafe: 'a sequence * SeqIndex.int -> 'a elt
                     end
                  ): SEQUENCE =
   struct
      structure Array = Primitive.Array

      val op +? = SeqIndex.+?
      val op + = SeqIndex.+
      val op -? = SeqIndex.-?
      val op < = SeqIndex.<
      val op <= = SeqIndex.<=
      val op > = SeqIndex.>
      val op >= = SeqIndex.>=
      val gtu = SeqIndex.gtu
      val geu = SeqIndex.geu

      (* fun wrap1 f = fn (i) => f (SeqIndex.toIntUnsafe i) *)
      fun wrap2 f = fn (i, x) => f (SeqIndex.toIntUnsafe i, x)
      fun wrap3 f = fn (i, x, y) => f (SeqIndex.toIntUnsafe i, x, y)
      fun unwrap1 f = fn (i) => f (SeqIndex.fromIntUnsafe i)
      fun unwrap2 f = fn (i, x) => f (SeqIndex.fromIntUnsafe i, x)

      type 'a sequence = 'a S.sequence
      type 'a elt = 'a S.elt

      (* 
       * In general, *' values are in terms of SeqIndex.int,
       * while * values are in terms of Int.int.
       *)

      local
         fun doit (precision, toInt, fromInt, maxInt') =
            if Primitive.Int32.>= (valOf SeqIndex.precision, precision)
               then (fromInt maxInt', maxInt')
            else (SeqIndex.maxLen', toInt SeqIndex.maxLen')
         structure S =
            Int_ChooseInt
            (type 'a t = SeqIndex.int * 'a
             val fInt8 = doit (valOf Primitive.Int8.precision,
                               SeqIndex.schckToInt8, SeqIndex.schckFromInt8,
                               Primitive.Int8.maxInt')
             val fInt16 = doit (valOf Primitive.Int16.precision,
                               SeqIndex.schckToInt16, SeqIndex.schckFromInt16,
                                Primitive.Int16.maxInt')
             val fInt32 = doit (valOf Primitive.Int32.precision,
                                SeqIndex.schckToInt32, SeqIndex.schckFromInt32,
                                Primitive.Int32.maxInt')
             val fInt64 = doit (valOf Primitive.Int64.precision,
                                SeqIndex.schckToInt64, SeqIndex.schckFromInt64,
                                Primitive.Int64.maxInt')
             val fIntInf = (SeqIndex.maxLen', SeqIndex.schckToIntInf SeqIndex.maxLen'))
      in
         val (maxLen', maxLen) = S.f
      end

      fun fromIntForLength n =
         if Primitive.Controls.safe
            then (SeqIndex.fromInt n) handle Overflow => raise Size
            else SeqIndex.fromIntUnsafe n

      fun length' s = S.length s
      fun length s = 
         if Primitive.Controls.safe
            then (SeqIndex.toInt (length' s))
                 handle Overflow => raise Fail "Sequence.length"
            else SeqIndex.toIntUnsafe (length' s)

      fun arrayUninit' n =
         if not S.isMutable andalso n = 0
            then Array.array0Const ()
            else if Primitive.Controls.safe
                    andalso (n < 0 orelse n > maxLen')
                    then raise Size
                    else Array.arrayUnsafe n
      fun arrayUninit n = arrayUninit' (fromIntForLength n)

      fun newUninit' n = S.fromArray (arrayUninit' n)
      fun newUninit n = S.fromArray (arrayUninit n)

      fun seq0 () = S.fromArray (arrayUninit' 0)

      fun generate' n =
        let
           val a = arrayUninit' n
           val subLim = ref 0
           fun sub i =
              if Primitive.Controls.safe andalso geu (i, !subLim) then
                 raise Subscript
              else
                 Array.subUnsafe (a, i)
           val updateLim = ref 0
           fun update (i, x) =
              if Primitive.Controls.safe andalso geu (i, !updateLim) then
                 if i = !updateLim andalso i < n then
                    (Array.updateUnsafe (a, i, x);
                     subLim := i + 1;
                     updateLim := i + 1)
                 else
                    raise Subscript
              else
                 Array.updateUnsafe (a, i, x)
           val gotIt = ref false
           fun done () =
              if !gotIt then
                 raise Fail "already got vector"
              else
                 if n = !updateLim then
                    (gotIt := true;
                     updateLim := 0;
                     S.fromArray a)
                 else
                    raise Fail "vector not full"
        in
           {done = done,
            sub = sub,
            update = update}
        end

      fun generate n =
         let
            val {done, sub, update} = generate' (fromIntForLength n)
         in
            {done = done,
             sub = unwrap1 sub,
             update = unwrap2 update}
         end

      fun unfoldi' (n, b, f) =
         let
            val a = arrayUninit' n
            fun loop (i, b)  =
               if i >= n
                  then b
               else
                  let
                     val (x, b') = f (i, b)
                     val () = Array.updateUnsafe (a, i, x)
                  in
                     loop (i +? 1, b')
                  end
            val b = loop (0, b)
         in
            (S.fromArray a, b)
         end
      fun unfoldi (n, b, f) = unfoldi' (fromIntForLength n, b, wrap2 f)
      fun unfold (n, b, f) = unfoldi (n, b, f o #2)

      fun tabulate' (n, f) =
         #1 (unfoldi' (n, (), fn (i, ()) => (f i, ())))
      fun tabulate (n, f) =
         #1 (unfoldi (n, (), fn (i, ()) => (f i, ())))

      fun new' (n, x) = tabulate' (n, fn _ => x)
      fun new (n, x) = tabulate (n, fn _ => x)

      fun fromList l =
         let
            val a = arrayUninit (List.length l)
            val _ =
               List.foldl (fn (x, i) => (Array.updateUnsafe (a, i, x) ; (i +? 1))) 0 l
         in
            S.fromArray a
         end

      structure Slice =
         struct
            type 'a sequence = 'a S.sequence
            type 'a elt = 'a S.elt
            datatype 'a t = T of {seq: 'a sequence, 
                                  start: SeqIndex.int, len: SeqIndex.int}
            type 'a slice = 'a t

            fun length' (T {len, ...}) = len
            fun length sl = 
               if Primitive.Controls.safe
                  then (SeqIndex.toInt (length' sl)) 
                       handle Overflow => raise Fail "Sequence.Slice.length"
                  else SeqIndex.toIntUnsafe (length' sl)
            fun unsafeSub' (T {seq, start, ...}, i) =
               S.subUnsafe (seq, start +? i)
            fun unsafeSub (sl, i) =
               unsafeSub' (sl, SeqIndex.fromIntUnsafe i)
            fun sub' (sl as T {len, ...}, i) =
               if Primitive.Controls.safe andalso geu (i, len)
                  then raise Subscript
                  else unsafeSub' (sl, i)
            fun sub (sl, i) =
               if Primitive.Controls.safe
                  then let
                          val i = 
                             (SeqIndex.fromInt i)
                             handle Overflow => raise Subscript
                       in
                          sub' (sl, i)
                       end
                  else unsafeSub (sl, i)
            fun unsafeUpdateMk' updateUnsafe (T {seq, start, ...}, i, x) =
               updateUnsafe (seq, start +? i, x)
            fun unsafeUpdateMk updateUnsafe (sl, i, x) =
               unsafeUpdateMk' updateUnsafe (sl, SeqIndex.fromIntUnsafe i, x)
            fun updateMk' updateUnsafe (sl as T {len, ...}, i, x) =
               if Primitive.Controls.safe andalso geu (i, len)
                  then raise Subscript
                  else unsafeUpdateMk' updateUnsafe (sl, i, x)
            fun updateMk updateUnsafe (sl, i, x) =
               if Primitive.Controls.safe
                  then let
                          val i =
                             (SeqIndex.fromInt i)
                             handle Overflow => raise Subscript
                       in
                          updateMk' updateUnsafe (sl, i, x)
                       end
                  else unsafeUpdateMk updateUnsafe (sl, i, x)
            fun full (seq: 'a sequence) : 'a slice = 
               T {seq = seq, start = 0, len = S.length seq}
            fun unsafeSubslice' (T {seq, start, len}, start', len') = 
               T {seq = seq, 
                  start = start +? start',
                  len = (case len' of
                            NONE => len -? start'
                          | SOME len' => len')}
            fun unsafeSubslice (sl, start, len) =
               unsafeSubslice' 
               (sl, SeqIndex.fromIntUnsafe start, 
                Option.map SeqIndex.fromIntUnsafe len)
            fun unsafeSlice' (seq, start, len) =
               unsafeSubslice' (full seq, start, len)
            fun unsafeSlice (seq, start, len) =
               unsafeSubslice  (full seq, start, len)
            fun subslice' (T {seq, start, len}, start', len') =
               case len' of
                  NONE => 
                     if Primitive.Controls.safe 
                        andalso gtu (start', len)
                        then raise Subscript
                        else T {seq = seq,
                                start = start +? start',
                                len = len -? start'}
                | SOME len' => 
                     if Primitive.Controls.safe
                        andalso (gtu (start', len)
                                 orelse gtu (len', len -? start'))
                        then raise Subscript
                        else T {seq = seq,
                                start = start +? start',
                                len = len'}
            fun subslice (sl, start, len) =
               if Primitive.Controls.safe
                  then (subslice' (sl, 
                                   SeqIndex.fromInt start,
                                   Option.map SeqIndex.fromInt len))
                       handle Overflow => raise Subscript
                  else unsafeSubslice (sl, start, len)
            fun slice' (seq: 'a sequence, start, len) =
               subslice' (full seq, start, len)
            fun slice (seq: 'a sequence, start, len) =
               subslice (full seq, start, len)
            fun base' (T {seq, start, len}) = 
               (seq, start, len)
            fun base (T {seq, start, len}) = 
               (seq, SeqIndex.toIntUnsafe start, SeqIndex.toIntUnsafe len)
            fun isEmpty sl = length sl = 0
            fun getItem (sl as T {seq, start, len}) =
               if isEmpty sl
                  then NONE
               else SOME (S.subUnsafe (seq, start), 
                          T {seq = seq, 
                             start = start +? 1, 
                             len = len -? 1})
            fun foldli' f b (T {seq, start, len}) =
               let
                  val min = start
                  val len = len -? 1
                  val max = start +? len
                  fun loop (i, b) =
                     if i > max then b
                     else loop (i +? 1, f (i -? min, S.subUnsafe (seq, i), b))
               in loop (min, b)
               end
            fun foldli f b sl = foldli' (wrap3 f) b sl
            fun foldri' f b (T {seq, start, len}) =
               let
                  val min = start
                  val len = len -? 1
                  val max = start +? len
                  fun loop (i, b) =
                     if i < min then b
                     else loop (i -? 1, f (i -? min, S.subUnsafe (seq, i), b))
               in loop (max, b)
               end
            fun foldri f b sl = foldri' (wrap3 f) b sl
            local
               fun make foldi f b sl = foldi (fn (_, x, b) => f (x, b)) b sl
            in
               fun foldl f = make foldli' f
               fun foldr f = make foldri' f
            end
            fun appi' f sl = foldli' (fn (i, x, ()) => f (i, x)) () sl
            fun appi f sl = appi' (wrap2 f) sl
            fun app f sl = appi (f o #2) sl
            fun createi' tabulate' f (T {seq, start, len}) =
               tabulate' (len, fn i => f (i, S.subUnsafe (seq, start +? i)))
            fun createi tabulate' f sl = createi' tabulate' (wrap2 f) sl
            fun create tabulate' f sl = createi tabulate' (f o #2) sl
            fun mapi' f sl = createi' tabulate' f sl
            fun mapi f sl = mapi' (wrap2 f) sl
            fun map f sl = mapi (f o #2) sl
            fun findi' p (T {seq, start, len}) = 
               let
                  val min = start
                  val len = len -? 1
                  val max = start +? len
                  fun loop i =
                     if i > max
                        then NONE
                     else let val z = (i -? min, S.subUnsafe (seq, i))
                          in if p z
                                then SOME z
                             else loop (i +? 1)
                          end
               in loop min
               end
            fun findi p sl = Option.map (wrap2 (fn z => z)) (findi' (wrap2 p) sl)
            fun find p sl = Option.map #2 (findi (p o #2) sl)
            fun existsi' p sl = Option.isSome (findi' p sl)
            fun existsi p sl = existsi' (wrap2 p) sl
            fun exists p sl = existsi (p o #2) sl
            fun alli' p sl = not (existsi' (not o p) sl)
            fun alli p sl = alli' (wrap2 p) sl
            fun all p sl = alli (p o #2) sl
            fun collate cmp (T {seq = seq1, start = start1, len = len1},
                             T {seq = seq2, start = start2, len = len2}) =
               let
                  val min1 = start1
                  val min2 = start2
                  val max1 = start1 +? len1
                  val max2 = start2 +? len2
                  fun loop (i, j) =
                     case (i >= max1, j >= max2) of
                        (true, true) => EQUAL
                      | (true, false) => LESS
                      | (false, true) => GREATER
                      | (false, false) => 
                           (case cmp (S.subUnsafe (seq1, i), 
                                      S.subUnsafe (seq2, j)) of
                              EQUAL => loop (i +? 1, j +? 1)
                            | ans => ans)
               in loop (min1, min2)
               end
            fun sequence (sl as T {seq, start, len}): 'a sequence =
               if S.isMutable orelse (start <> 0 orelse len <> S.length seq)
                  then map (fn x => x) sl
               else seq
            fun append (sl1: 'a slice, sl2: 'a slice): 'a sequence =
               if length' sl1 = 0
                  then sequence sl2
               else if length' sl2 = 0
                  then sequence sl1
               else
                  let
                     val l1 = length' sl1
                     val l2 = length' sl2
                     val n = (l1 + l2) handle Overflow => raise Size
                  in
                     #1 (unfoldi' 
                         (n, (0, sl1), fn (_, (i, sl)) =>
                          if SeqIndex.< (i, length' sl)
                             then (unsafeSub' (sl, i), 
                                   (i +? 1, sl))
                             else (unsafeSub' (sl2, 0), 
                                   (1, sl2))))
                  end
            fun concat (sls: 'a slice list): 'a sequence =
               case sls of
                  [] => seq0 ()
                | [sl] => sequence sl
                | sls' as sl::sls =>
                     let
                        val n = 
                           (List.foldl (fn (sl, s) => s +? length' sl) 0 sls')
                           handle Overflow => raise Size
                     in
                        #1 (unfoldi' 
                            (n, (0, sl, sls), fn (_, ac) =>
                             let
                                fun loop (i, sl, sls) =
                                   if SeqIndex.< (i, length' sl)
                                      then (unsafeSub' (sl, i), 
                                            (i +? 1, sl, sls))
                                      else case sls of
                                         [] => raise Fail "Sequence.Slice.concat"
                                       | sl :: sls => loop (0, sl, sls)
                             in loop ac
                             end))
                     end
            fun concatWith (sep: 'a sequence) (sls: 'a slice list): 'a sequence =
               let val sep = full sep
               in case sls of
                     [] => seq0 ()
                   | [sl] => sequence sl
                   | sl::sls =>
                       List.foldl (fn (sl,seq) => 
                                   concat [full seq, sep, full (sequence sl)])
                                  (sequence sl) sls
               end
            fun triml k =
               if Primitive.Controls.safe andalso Int.< (k, 0)
                  then raise Subscript
               else
                  (fn (T {seq, start, len}) =>
                   let
                      val k = 
                         if Primitive.Controls.safe
                            then SeqIndex.fromInt k
                            else SeqIndex.fromIntUnsafe k
                   in
                      if SeqIndex.> (k, len)
                         then unsafeSlice' (seq, start +? len, SOME 0)
                         else unsafeSlice' (seq, start +? k, SOME (len -? k))
                   end handle Overflow => unsafeSlice' (seq, start +? len, SOME 0))
            fun trimr k =
               if Primitive.Controls.safe andalso Int.< (k, 0)
                  then raise Subscript
               else 
                  (fn (T {seq, start, len}) =>
                   let
                      val k =
                         if Primitive.Controls.safe
                            then SeqIndex.fromInt k
                            else SeqIndex.fromIntUnsafe k
                   in
                      if SeqIndex.> (k, len)
                         then unsafeSlice' (seq, start, SOME 0)
                         else unsafeSlice' (seq, start, SOME (len -? k))
                   end handle Overflow => unsafeSlice' (seq, start, SOME 0))
            fun isSubsequence (eq: 'a elt * 'a elt -> bool)
                              (seq: 'a sequence)
                              (sl: 'a slice) =
               let
                  val n = S.length seq
                  val n' = length' sl
               in
                  if n <= n'
                     then let
                             val n'' = n' -? n
                             fun loop (i, j) =
                                if i > n''
                                   then false
                                else if j >= n
                                   then true
                                else if eq (S.subUnsafe (seq, j), 
                                            unsafeSub' (sl, i +? j))
                                   then loop (i, j +? 1)
                                else loop (i +? 1, 0)
                          in
                             loop (0, 0)
                          end
                  else false
               end
            fun isPrefix (eq: 'a elt * 'a elt -> bool)
                         (seq: 'a sequence)
                         (sl: 'a slice) =
               let
                  val n = S.length seq
                  val n' = length' sl
               in
                  if n <= n'
                     then let
                             fun loop (j) =
                                if j >= n
                                   then true
                                else if eq (S.subUnsafe (seq, j), 
                                            unsafeSub' (sl, j))
                                   then loop (j +? 1)
                                else false
                          in
                             loop (0)
                          end
                  else false
               end
            fun isSuffix (eq: 'a elt * 'a elt -> bool)
                         (seq: 'a sequence)
                         (sl: 'a slice) =
               let
                  val n = S.length seq
                  val n' = length' sl
               in
                  if n <= n'
                     then let
                             val n'' = n' -? n
                             fun loop (j) =
                                if j >= n
                                   then true
                                else if eq (S.subUnsafe (seq, j), 
                                            unsafeSub' (sl, n'' +? j))
                                   then loop (j +? 1)
                                else false
                          in
                             loop (0)
                          end
                  else false
               end
            fun split' (T {seq, start, len}, i) =
               (unsafeSlice' (seq, start, SOME (i -? start)),
                unsafeSlice' (seq, i, SOME (len -? (i -? start))))
            fun splitl f (sl as T {seq, start, len}) =
               let
                  val stop = start +? len
                  fun loop i =
                     if i >= stop
                        then i
                     else if f (S.subUnsafe (seq, i))
                             then loop (i +? 1)
                          else i
               in split' (sl, loop start)
               end
            fun splitr f (sl as T {seq, start, len}) =
               let
                  fun loop i =
                     if i < start
                        then start
                     else if f (S.subUnsafe (seq, i))
                             then loop (i -? 1)
                          else i +? 1
               in split' (sl, loop (start +? len -? 1))
               end
            fun splitAt' (T {seq, start, len}, i) =
               if Primitive.Controls.safe andalso SeqIndex.gtu (i, len)
                  then raise Subscript
               else (unsafeSlice' (seq, start, SOME i),
                     unsafeSlice' (seq, start +? i, SOME (len -? i)))
            fun splitAt (sl, i) =
               if Primitive.Controls.safe
                  then (splitAt' (sl, SeqIndex.fromInt i))
                       handle Overflow => raise Subscript
                  else splitAt' (sl, SeqIndex.fromIntUnsafe i)
            fun dropl p s = #2 (splitl p s)
            fun dropr p s = #1 (splitr p s)
            fun takel p s = #1 (splitl p s)
            fun taker p s = #2 (splitr p s)
            fun position (eq: 'a elt * 'a elt -> bool)
                         (seq': 'a sequence)
                         (sl as T {seq, start, len}) =
               let
                  val len' = S.length seq'
                  val max = start +? len -? len' +? 1
                  (* loop returns the index of the front of the suffix. *)
                  fun loop i =
                     if i >= max
                        then start +? len
                     else let
                             fun loop' j =
                                if j >= len'
                                   then i
                                else if eq (S.subUnsafe (seq, i +? j), 
                                            S.subUnsafe (seq', j))
                                        then loop' (j +? 1)
                                     else loop (i +? 1)
                          in loop' 0
                          end
               in split' (sl, loop start)
               end
            fun span (eq: 'a sequence * 'a sequence -> bool)
                     (T {seq, start, ...},
                      T {seq = seq', start = start', len = len'}) =
               if Primitive.Controls.safe andalso 
                  (not (eq (seq, seq')) orelse start' +? len' < start)
                  then raise Span
               else unsafeSlice' (seq, start, SOME ((start' +? len') -? start))
            fun translate f (sl: 'a slice) =
               concat (List.rev (foldl (fn (c, l) => (full (f c)) :: l) [] sl))
            local
               fun make finish p (T {seq, start, len}) =
                  let
                     val max = start +? len
                     fun loop (i, start, sls) =
                        if i >= max
                           then List.rev (finish (seq, start, i, sls))
                        else
                           if p (S.subUnsafe (seq, i))
                              then loop (i +? 1, i +? 1, finish (seq, start, i, sls))
                           else loop (i +? 1, start, sls)
                  in loop (start, start, []) 
                  end
            in
               fun tokens p sl =
                  make (fn (seq, start, stop, sls) =>
                        if start = stop
                           then sls
                        else
                           (unsafeSlice' (seq, start, SOME (stop -? start)))
                           :: sls)
                       p sl
               fun fields p sl = 
                  make (fn (seq, start, stop, sls) =>
                        (unsafeSlice' (seq, start, SOME (stop -? start)))
                        :: sls)
                       p sl
            end
            fun toList (sl: 'a slice) = foldr (fn (a,l) => a::l) [] sl
         end

      local
        fun make f seq = f (Slice.full seq)
        fun make2 f (seq1, seq2) = f (Slice.full seq1, Slice.full seq2)
      in
        fun sub (seq, i) = Slice.sub (Slice.full seq, i)
        fun sub' (seq, i) = Slice.sub' (Slice.full seq, i)
        fun unsafeSub (seq, i) = Slice.unsafeSub (Slice.full seq, i)
        fun unsafeSub' (seq, i) = Slice.unsafeSub' (Slice.full seq, i)
        fun updateMk updateUnsafe (seq, i, x) = 
           Slice.updateMk updateUnsafe (Slice.full seq, i, x)
        fun updateMk' updateUnsafe (seq, i, x) = 
           Slice.updateMk' updateUnsafe (Slice.full seq, i, x)
        fun unsafeUpdateMk updateUnsafe (seq, i, x) = 
           Slice.unsafeUpdateMk updateUnsafe (Slice.full seq, i, x)
        fun unsafeUpdateMk' updateUnsafe (seq, i, x) = 
           Slice.unsafeUpdateMk' updateUnsafe (Slice.full seq, i, x)
        fun append seqs = make2 Slice.append seqs
        fun concat seqs = Slice.concat (List.map Slice.full seqs)
        fun appi' f = make (Slice.appi' f)
        fun appi f = make (Slice.appi f)
        fun app f = make (Slice.app f)
        fun mapi' f = make (Slice.mapi' f)
        fun mapi f = make (Slice.mapi f)
        fun map f = make (Slice.map f)
        fun foldli' f b = make (Slice.foldli' f b)
        fun foldli f b = make (Slice.foldli f b)
        fun foldl f b = make (Slice.foldl f b)
        fun foldri' f b = make (Slice.foldri' f b)
        fun foldri f b = make (Slice.foldri f b)
        fun foldr f b = make (Slice.foldr f b)
        fun findi' p = make (Slice.findi' p)
        fun findi p = make (Slice.findi p)
        fun find p = make (Slice.find p)
        fun existsi' p = make (Slice.existsi' p)
        fun existsi p = make (Slice.existsi p)
        fun exists p = make (Slice.exists p)
        fun alli' p = make (Slice.alli' p)
        fun alli p = make (Slice.alli p)
        fun all p = make (Slice.all p)
        fun collate cmp = make2 (Slice.collate cmp)
        fun concatWith sep seqs = Slice.concatWith sep (List.map Slice.full seqs)
        fun isPrefix eq seq = make (Slice.isPrefix eq seq)
        fun isSubsequence eq seq = make (Slice.isSubsequence eq seq)
        fun isSuffix eq seq = make (Slice.isSuffix eq seq)
        fun translate f = make (Slice.translate f)
        fun tokens f seq = List.map Slice.sequence (make (Slice.tokens f) seq)
        fun fields f seq = List.map Slice.sequence (make (Slice.fields f) seq)
        fun createi' tabulate' f seq = make (Slice.createi' tabulate' f) seq
        fun createi tabulate' f seq = make (Slice.createi tabulate' f) seq
        fun create tabulate' f seq = make (Slice.create tabulate' f) seq
        fun duplicate seq = make Slice.sequence seq
        fun toList seq = make Slice.toList seq
      end
   end
