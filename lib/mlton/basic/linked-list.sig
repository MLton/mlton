(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature LINKED_LIST =
   sig
      type 'a t

      val empty: unit -> 'a t
      val fold: 'a t * 'b * ('a * 'b -> 'b) -> 'b
      val fromList: 'a list -> 'a t
      val layout: ('a -> Layout.t) -> 'a t -> Layout.t
      (* in-place reverse *)
      val reverse: 'a t -> unit
      (* splice (l, r) extends l with r *)
      val splice: 'a t * 'a t -> unit
      val toList: 'a t -> 'a list
      val unfold: 'a * ('a -> ('b * 'a) option) -> 'b t
      val unfoldi: int * 'a * (int * 'a -> 'b * 'a) -> 'b t
      val unfoldr: 'a * ('a -> ('b * 'a) option) -> 'b t
      val unfoldri: int * 'a * (int * 'a -> 'b * 'a) -> 'b t
   end

functor TestLinkedList (S: LINKED_LIST): sig end =
struct

open S

val _ = 
   Assert.assert
   ("TestLinkedList", fn () =>
    List.forall ([[],
                  [1],
                  [1, 2],
                  [1, 2, 3]],
                 fn l =>
                 l = toList (fromList l)
                 andalso rev l = toList (let
                                            val l' = fromList l
                                            val _ = reverse l'
                                         in
                                            l'
                                         end)
                 andalso
                 let
                    val l' = fromList l
                    val l'' = fromList l
                    val _ = splice (l', l'')
                 in
                    l @ l = toList l'
                 end))

end
