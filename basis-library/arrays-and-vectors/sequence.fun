(* Copyright (C) 2013,2017-2018 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure SeqIndex =
   struct
      open SeqIndex

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

      fun fromIntForLength n =
         if Primitive.Controls.safe
            then (fromInt n) handle Overflow => raise Size
            else fromIntUnsafe n
   end

functor Sequence (S: PRIM_SEQUENCE): SEQUENCE =
   struct
      val op +? = SeqIndex.+?
      val op +! = SeqIndex.+!
      val op -? = SeqIndex.-?
      val op <= = SeqIndex.<=
      val op > = SeqIndex.>
      val op >= = SeqIndex.>=

      (* fun wrap1 f = fn (i) => f (SeqIndex.toIntUnsafe i) *)
      fun wrap2 f = fn (i, x) => f (SeqIndex.toIntUnsafe i, x)
      fun wrap3 f = fn (i, x, y) => f (SeqIndex.toIntUnsafe i, x, y)
      fun unwrap1 f = fn (i) => f (SeqIndex.fromIntUnsafe i)
      fun unwrap2 f = fn (i, x) => f (SeqIndex.fromIntUnsafe i, x)

      type 'a sequence = 'a S.sequence
      type 'a elt = 'a S.elt

      (* S.maxLen must be representable as an Int.int already *)
      val maxLen = SeqIndex.toInt S.maxLen

      fun length s = 
         if Primitive.Controls.safe
            then (SeqIndex.toInt (S.length s))
                 handle Overflow => raise Fail "Sequence.length"
            else SeqIndex.toIntUnsafe (S.length s)

      fun alloc n = S.alloc (SeqIndex.fromIntForLength n)
      fun unsafeAlloc n = S.unsafeAlloc (SeqIndex.fromIntUnsafe n)

      fun create n =
         let
            val {done, sub, update} = S.create (SeqIndex.fromIntForLength n)
         in
            {done = done,
             sub = unwrap1 sub,
             update = unwrap2 update}
         end

      fun unfoldi (n, b, f) = S.unfoldi (SeqIndex.fromIntForLength n, b, wrap2 f)
      fun unfold (n, b, f) = S.unfold (SeqIndex.fromIntForLength n, b, f)
      fun unsafeUnfold (n, b, f) = S.unfold (SeqIndex.fromIntUnsafe n, b, f)

      fun seq0 () = #1 (unfold (0, (), fn _ => raise Fail "Sequence.seq0"))

      fun tabulate (n, f) =
         #1 (unfoldi (n, (), fn (i, ()) => (f i, ())))

      fun new (n, x) = 
         #1 (unfold (n, (), fn () => (x, ())))
      fun unsafeNew (n, x) =
         #1 (unsafeUnfold (n, (), fn () => (x, ())))

      fun fromList l =
         #1 (unfold (List.length l, l, fn l =>
                     case l of
                        nil => raise Fail "Sequence.fromList"
                      | h::t => (h, t)))

      structure Slice =
         struct
            type 'a sequence = 'a S.Slice.sequence
            type 'a elt = 'a S.Slice.elt
            type 'a slice = 'a S.Slice.slice

            fun length sl = 
               if Primitive.Controls.safe
                  then (SeqIndex.toInt (S.Slice.length sl))
                       handle Overflow => raise Fail "Sequence.Slice.length"
                  else SeqIndex.toIntUnsafe (S.Slice.length sl)

            fun unsafeSub (sl, i) =
               S.Slice.unsafeSub (sl, SeqIndex.fromIntUnsafe i)
            fun sub (sl, i) = 
               if Primitive.Controls.safe
                  then let 
                          val i = 
                             (SeqIndex.fromInt i)
                             handle Overflow => raise Subscript
                       in
                          S.Slice.sub (sl, i)
                       end
                  else unsafeSub (sl, i)

            fun unsafeUpdate (sl, i, x) =
               S.Slice.unsafeUpdate (sl, SeqIndex.fromIntUnsafe i, x)
            fun update (sl, i, x) =
               if Primitive.Controls.safe
                  then let
                          val i =
                             (SeqIndex.fromInt i)
                             handle Overflow => raise Subscript
                       in
                          S.Slice.update (sl, i, x)
                       end
               else unsafeUpdate (sl, i, x)

            val uninitIsNop = S.Slice.uninitIsNop
            fun unsafeUninit (sl, i) =
               S.Slice.unsafeUninit (sl, SeqIndex.fromIntUnsafe i)
            fun uninit (sl, i) =
               if Primitive.Controls.safe
                  then let
                          val i =
                             (SeqIndex.fromInt i)
                             handle Overflow => raise Subscript
                       in
                          S.Slice.uninit (sl, i)
                       end
               else unsafeUninit (sl, i)

            fun unsafeCopy {dst, di, src} =
               S.Slice.unsafeCopy
               {dst = dst,
                di = SeqIndex.fromIntUnsafe di,
                src = src}
            fun copy {dst, di, src} =
               (S.Slice.copy
                {dst = dst,
                 di = SeqIndex.fromInt di,
                 src = src})
               handle Overflow => raise Subscript

            val full = S.Slice.full
            fun unsafeSubslice (sl, start, len) =
               S.Slice.unsafeSubslice 
               (sl, SeqIndex.fromIntUnsafe start, 
                Option.map SeqIndex.fromIntUnsafe len)
            fun unsafeSlice (seq, start, len) =
               unsafeSubslice (full seq, start, len)
            fun subslice (sl, start, len) =
               if Primitive.Controls.safe
                  then (S.Slice.subslice (sl, 
                                          SeqIndex.fromInt start,
                                          Option.map SeqIndex.fromInt len))
                       handle Overflow => raise Subscript
                  else unsafeSubslice (sl, start, len)
            fun slice (seq: 'a sequence, start, len) =
               subslice (full seq, start, len)
            fun base sl = 
               let
                  val (seq, start, len) = S.Slice.base sl
               in
                  if Primitive.Controls.safe
                     then (seq, SeqIndex.toInt start, SeqIndex.toInt len)
                          handle Overflow => raise Fail "Sequence.Slice.base"
                     else (seq, 
                           SeqIndex.toIntUnsafe start, 
                           SeqIndex.toIntUnsafe len)
               end
            val isEmpty = S.Slice.isEmpty
            val getItem = S.Slice.getItem
            fun foldli f b sl = S.Slice.foldli (wrap3 f) b sl
            fun foldri f b sl = S.Slice.foldri (wrap3 f) b sl
            val foldl = S.Slice.foldl
            val foldr = S.Slice.foldr
            fun appi f sl = S.Slice.appi (wrap2 f) sl
            val app = S.Slice.app
            fun mapi f sl = S.Slice.mapi (wrap2 f) sl
            val map = S.Slice.map
            fun findi p sl = 
               Option.map (wrap2 (fn z => z)) (S.Slice.findi (wrap2 p) sl)
            val find = S.Slice.find
            fun existsi p sl = S.Slice.existsi (wrap2 p) sl
            val exists = S.Slice.exists
            fun alli p sl = S.Slice.alli (wrap2 p) sl
            val all = S.Slice.all
            val collate = S.Slice.collate
            val sequence = S.Slice.sequence
            val append = S.Slice.append

            fun concatGen (xs: 'b list, toSlice: 'b -> 'a slice): 'a sequence =
               case xs of
                  [] => seq0 ()
                | [x] => sequence (toSlice x)
                | xs =>
                     let
                        val add =
                           if Primitive.Controls.safe 
                              then (fn (x, s) =>
                                       (s +! S.Slice.length (toSlice x))
                                       handle Overflow => raise Size)
                              else (fn (x, s) => s +? S.Slice.length (toSlice x))
                        val n = List.foldl add 0 xs
                        val a = Primitive.Array.alloc n
                        fun loop (di, xs) =
                           case xs of
                              [] => S.unsafeFromArray a
                            | x::xs =>
                                 let val sl = toSlice x
                                 in
                                    S.Slice.unsafeCopy {dst = a, di = di, src = sl}
                                    ; loop (di +? S.Slice.length sl, xs)
                                 end
                     in
                        loop (0, xs)
                     end
            fun concat (sls: 'a slice list): 'a sequence =
               concatGen (sls, fn sl => sl)
            fun concatWithGen (sep: 'a sequence) (xs: 'b list, toSlice: 'b -> 'a slice): 'a sequence =
               case xs of
                  [] => seq0 ()
                | [x] => sequence (toSlice x)
                | x::xs =>
                     let
                        val sep = S.Slice.full sep
                        val sepn = S.Slice.length sep
                        val add = 
                           if Primitive.Controls.safe 
                              then (fn (x, s) =>
                                       (s +! sepn +! S.Slice.length (toSlice x))
                                       handle Overflow => raise Size)
                              else (fn (x, s) =>
                                       (s +? sepn +? S.Slice.length (toSlice x)))
                        val n = List.foldl add (S.Slice.length (toSlice x)) xs
                        val a = Primitive.Array.alloc n
                        fun loop (di, xs) =
                           case xs of
                              [] => raise Fail "Sequence.Slice.concatWithGen"
                            | [x] =>
                                 let
                                    val sl = toSlice x
                                    val _ = S.Slice.unsafeCopy {dst = a, di = di, src = sl}
                                 in
                                    S.unsafeFromArray a
                                 end
                            | x::xs =>
                                 let
                                    val sl = toSlice x
                                    val _ = S.Slice.unsafeCopy {dst = a, di = di, src = sl}
                                    val di = di +? S.Slice.length sl
                                    val _ = S.Slice.unsafeCopy {dst = a, di = di, src = sep}
                                    val di = di +? sepn
                                 in
                                    loop (di, xs)
                                 end
                     in
                        loop (0, x::xs)
                     end
            fun concatWith sep sls = concatWithGen sep (sls, fn sl => sl)
            fun triml k sl =
               if Primitive.Controls.safe andalso Int.< (k, 0)
                  then raise Subscript
               else let
                       val len = S.Slice.length sl
                       val k = 
                          if Primitive.Controls.safe
                             then SeqIndex.fromInt k
                             else SeqIndex.fromIntUnsafe k
                    in
                       if SeqIndex.> (k, len)
                          then S.Slice.unsafeSubslice (sl, len, SOME 0)
                          else S.Slice.unsafeSubslice (sl, k, SOME (len -? k))
                    end handle Overflow => 
                           (* k is positive, so behavior is specified! *)
                           S.Slice.unsafeSubslice (sl, S.Slice.length sl, SOME 0)
            fun trimr k sl =
               if Primitive.Controls.safe andalso Int.< (k, 0)
                  then raise Subscript
               else let
                       val len = S.Slice.length sl
                       val k =
                          if Primitive.Controls.safe
                             then SeqIndex.fromInt k
                             else SeqIndex.fromIntUnsafe k
                    in
                       if SeqIndex.> (k, len)
                          then S.Slice.unsafeSubslice (sl, 0, SOME 0)
                          else S.Slice.unsafeSubslice (sl, 0, SOME (len -? k))
                    end handle Overflow => 
                           (* k is positive, so behavior is specified! *)
                           S.Slice.unsafeSubslice (sl, 0, SOME 0)
            fun isSubsequence (eq: 'a elt * 'a elt -> bool)
                              (seq: 'a sequence)
                              (sl: 'a slice) =
               let
                  val n = S.length seq
                  val n' = S.Slice.length sl
               in
                  if n <= n'
                     then let
                             val n'' = n' -? n
                             fun loop (i, j) =
                                if i > n''
                                   then false
                                else if j >= n
                                   then true
                                else if eq (S.unsafeSub (seq, j), 
                                            S.Slice.unsafeSub (sl, i +? j))
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
                  val n' = S.Slice.length sl
               in
                  if n <= n'
                     then let
                             fun loop (j) =
                                if j >= n
                                   then true
                                else if eq (S.unsafeSub (seq, j), 
                                            S.Slice.unsafeSub (sl, j))
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
                  val n' = S.Slice.length sl
               in
                  if n <= n'
                     then let
                             val n'' = n' -? n
                             fun loop (j) =
                                if j >= n
                                   then true
                                else if eq (S.unsafeSub (seq, j), 
                                            S.Slice.unsafeSub (sl, n'' +? j))
                                   then loop (j +? 1)
                                else false
                          in
                             loop (0)
                          end
                  else false
               end
            val splitl = S.Slice.splitl
            val splitr = S.Slice.splitr
            fun splitAt (sl, i) = 
               if Primitive.Controls.safe
                  then (S.Slice.splitAt (sl, SeqIndex.fromInt i))
                       handle Overflow => raise Subscript
                  else S.Slice.splitAt (sl, SeqIndex.fromIntUnsafe i)
            val dropl = S.Slice.dropl
            val dropr = S.Slice.dropr
            val takel = S.Slice.takel
            val taker = S.Slice.taker
            val position = S.Slice.position
            fun translate f (sl: 'a slice) =
               concat (List.rev (foldl (fn (c, l) => (full (f c)) :: l) [] sl))
            local
               fun make finish p sl =
                  let
                     val (seq, start, len) = S.Slice.base sl
                     val max = start +? len
                     fun loop (i, start, sls) =
                        if i >= max
                           then List.rev (finish (seq, start, i, sls))
                        else
                           if p (S.unsafeSub (seq, i))
                              then loop (i +? 1, i +? 1, finish (seq, start, i, sls))
                           else loop (i +? 1, start, sls)
                  in loop (start, start, []) 
                  end
            in
               fun tokensGen fromSlice p sl =
                  make (fn (seq, start, stop, sls) =>
                        if start = stop
                           then sls
                        else
                           (fromSlice
                            (S.Slice.unsafeSlice
                             (seq, start, SOME (stop -? start))))
                           :: sls)
                       p sl
               fun fieldsGen fromSlice p sl =
                  make (fn (seq, start, stop, sls) =>
                        (fromSlice
                         (S.Slice.unsafeSlice
                          (seq, start, SOME (stop -? start))))
                        :: sls)
                       p sl
            end
            fun tokens p sl = tokensGen (fn sl => sl) p sl
            fun fields p sl = fieldsGen (fn sl => sl) p sl
            fun toList (sl: 'a slice) = foldr (fn (a,l) => a::l) [] sl 
         end

      local
        fun make f seq = f (Slice.full seq)
        fun make2 f (seq1, seq2) = f (Slice.full seq1, Slice.full seq2)
      in
        fun sub (seq, i) = Slice.sub (Slice.full seq, i)
        fun unsafeSub (seq, i) = Slice.unsafeSub (Slice.full seq, i) 
        fun update (seq, i, x) = Slice.update (Slice.full seq, i, x)
        fun unsafeUpdate (seq, i, x) = Slice.unsafeUpdate (Slice.full seq, i, x)
        fun uninitIsNop seq = Slice.uninitIsNop (Slice.full seq)
        fun uninit (seq, i) = Slice.uninit (Slice.full seq, i)
        fun unsafeUninit (seq, i) = Slice.unsafeUninit (Slice.full seq, i)
        fun copy {dst, di, src} =
           Slice.copy {dst = dst, di = di, src = Slice.full src}
        fun unsafeCopy {dst, di, src} =
           Slice.unsafeCopy {dst = dst, di = di, src = Slice.full src}
        fun append seqs = make2 Slice.append seqs 
        fun concat seqs = Slice.concatGen (seqs, Slice.full)
        fun appi f = make (Slice.appi f)
        fun app f = make (Slice.app f)
        fun mapi f = make (Slice.mapi f)
        fun map f = make (Slice.map f)
        fun foldli f b = make (Slice.foldli f b)
        fun foldl f b = make (Slice.foldl f b)
        fun foldri f b = make (Slice.foldri f b)
        fun foldr f b = make (Slice.foldr f b)
        fun findi p = make (Slice.findi p)
        fun find p = make (Slice.find p)
        fun existsi p = make (Slice.existsi p)
        fun exists p = make (Slice.exists p)
        fun alli p = make (Slice.alli p)
        fun all p = make (Slice.all p) 
        fun collate cmp = make2 (Slice.collate cmp) 
        fun concatWith sep seqs = Slice.concatWithGen sep (seqs, Slice.full)
        fun isPrefix eq seq = make (Slice.isPrefix eq seq)
        fun isSubsequence eq seq = make (Slice.isSubsequence eq seq)
        fun isSuffix eq seq = make (Slice.isSuffix eq seq) 
        fun translate f = make (Slice.translate f)
        fun tokens f seq = make (Slice.tokensGen Slice.sequence f) seq
        fun fields f seq = make (Slice.fieldsGen Slice.sequence f) seq
        fun duplicate seq = make Slice.sequence seq
        fun toList seq = make Slice.toList seq
      end
   end
