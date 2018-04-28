(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure LinkedList: LINKED_LIST =
struct

structure Node =
   struct
      datatype 'a t = T of {elt: 'a,
                            next: 'a t option ref}

      fun new elt = T {elt = elt,
                       next = ref NONE}

      fun cons (elt, next) = T {elt = elt,
                                next = ref (SOME next)}

      fun clearNext (T {next, ...}) = next := NONE
      fun setNext (T {next, ...}, n) = next := SOME n

      fun fold (n, ac, f) =
         let
            fun loop (T {elt, next}, ac) =
               let
                  val ac = f (elt, ac)
               in
                  case !next of
                     NONE => ac
                   | SOME n => loop (n, ac)
               end
         in
            loop (n, ac)
         end
   end

datatype 'a t = T of {first: 'a Node.t option ref,
                      last: 'a Node.t option ref}

fun invariant (T {first, last}) =
   case (!first, !last) of
      (NONE, NONE) => true
    | (SOME _, SOME (Node.T {next, ...})) => not (isSome (!next))
    | _ => false

fun fold (T {first, ...}, ac, f) =
   case !first of
      NONE => ac
    | SOME n => Node.fold (n, ac, f)

fun toList l = rev (fold (l, [], op ::))

fun layout layoutX l = List.layout layoutX (toList l)

fun empty () = T {first = ref NONE,
                  last = ref NONE}

fun splice (T {first = f, last = l}, T {first = f', last = l'}): unit =
   case (!l, !f') of
      (NONE, NONE) => ()
    | (NONE, _) => (f := !f'; l := !l')
    | (_, NONE) => ()
    | (SOME ln, SOME f'n) =>
         (Node.setNext (ln, f'n)
          ; l := !l')

val ('a, 'b) unfoldr: 'a * ('a -> ('b * 'a) option) -> 'b t =
   fn (a, f) =>
   case f a of
      NONE => empty ()
    | SOME (b, a) =>
         let
            val last = Node.new b
            fun loop (a: 'a, n: 'b Node.t): 'b Node.t =
               case f a of
                  NONE => n
                | SOME (b, a) => loop (a, Node.cons (b, n))
            val first = loop (a, last)
         in
            T {first = ref (SOME first),
               last = ref (SOME last)}
         end

val unfoldri: int * 'a * (int * 'a -> 'b * 'a) -> 'b t =
   fn (n, a, f) =>
   if n < 0
      then Error.bug "LinkedList.unfoldri"
   else
      unfoldr ((n - 1, a), fn (i, a) =>
               if i < 0
                  then NONE
               else let
                       val (b, a) = f (i, a)
                    in
                       SOME (b, (i - 1, a))
                    end)

val ('a, 'b) unfold: 'a * ('a -> ('b * 'a) option) -> 'b t =
   fn (a, f) =>
   case f a of
      NONE => empty ()
    | SOME (b, a) =>
         let
            val first = Node.new b
            fun loop (a: 'a, n: 'b Node.t): 'b Node.t =
               case f a of
                  NONE => n
                | SOME (b, a) => 
                     let
                        val n' = Node.new b
                        val _ = Node.setNext (n, n')
                     in
                        loop (a, n')
                     end
            val last = loop (a, first)
         in
            T {first = ref (SOME first),
               last = ref (SOME last)}
         end

val unfoldi: int * 'a * (int * 'a -> 'b * 'a) -> 'b t =
   fn (n, a, f) =>
   if n < 0
      then Error.bug "LinkedList.unfoldi"
   else
      unfold ((0, a), fn (i, a) =>
              if i >= n
                 then NONE
              else let
                      val (b, a') = f (i, a)
                   in
                      SOME (b, (i + 1, a'))
                   end)

fun reverse (ll as T {first, last}) =
   (if invariant ll
       then ()
    else Out.output (Out.error, "reverse 1\n")
    ;
   case !first of
      NONE => ()
    | SOME (n as Node.T {next, ...}) =>
         case !next of
            NONE => ()
          | SOME n' =>
               let
                  val _ = Node.clearNext n
                  fun loop (n, n' as Node.T {next, ...}) =
                     let
                        val no = !next
                        val _ = next := SOME n
                     in
                        case no of
                           NONE => ()
                         | SOME n'' => loop (n', n'')
                     end
                  val _ = loop (n, n')
                  val _ = Ref.swap (first, last)
               in
                  ()
               end
            ; if invariant ll
       then ()
    else Out.output (Out.error, "reverse 2\n"))

fun fromList l =
   unfold (l,
           fn [] => NONE
            | x :: l => SOME (x, l))

end
