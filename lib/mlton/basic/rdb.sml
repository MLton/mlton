(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure RDB: RDB =
struct

structure Rep =
   struct
      type t = exn
   end

structure Domain =
   struct
      datatype t = T of {compare : Rep.t * Rep.t -> Relation.t,
                         contains: Rep.t -> bool,
                         just: Justify.t,
                         toString: Rep.t -> string}
   end

structure Value =
   struct
      datatype t = T of {domain: Domain.t,
                         rep: Rep.t}
   end

structure Domain =
   struct
      open Domain

      fun contains (T {contains, ...}, Value.T {rep = r, ...}) = contains r

      fun new {compare: 'a * 'a -> Relation.t,
               just: Justify.t,
               toString: 'a -> string}: t * ('a -> Value.t) =
         let
            exception E of 'a
            val contains = fn E _ => true | _ => false
            val toString = fn E x => toString x 
                            | _ => Error.bug "RDB.Domain.new.toString"
            val compare =
               fn (E x, E y) => compare (x, y)
                | _ => Error.bug "RDB.Domain.new.compare"
            val d = T {compare = compare,
                      contains = contains,
                      just = just,
                      toString = toString}
            fun make (a: 'a): Value.t = Value.T {domain = d, rep = E a}
         in (d, make)
         end

      val (bool, boolV) = new {compare = Bool.compare,
                               just = Justify.Left,
                               toString = Bool.toString}

      val (int, intV) = new {compare =  Int.compare,
                             just = Justify.Right,
                             toString = Int.toString}

      val (real, realV) = new {compare = Real.compare,
                               just = Justify.Right,
                               toString =
                               fn r => Real.format (r, Real.Format.fix (SOME 1))}
(*                             Real.toString *)

      val (string, stringV) = new {compare = String.compare,
                                   just = Justify.Left,
                                   toString = String.toString}
   end

structure Value =
   struct
      open Value

      local open Domain
      in
         val bool = boolV
         val int = intV
         val real = realV
         val string = stringV
      end

      local
         fun unary f (T {domain = Domain.T d, rep = r, ...}) = f d r
      in
         val toString = unary #toString
      end

      fun justification (T {domain = Domain.T {just, ...}, ...}) = just

      local
         fun binary f (T {domain = Domain.T d, rep = r, ...}, T {rep = r', ...}) =
            f d (r, r')
      in
         val compare = binary #compare
      end

      val {<, <=, equals, >=, >, ...} = Relation.compare compare
   end

structure Attribute =
   struct
      open String

      val new = fn s => s
   end

structure Heading =
   struct
      datatype t = T of (Attribute.t * Domain.t) list

      fun degree (T l) = List.length l

      fun info (T l, a) =
         case List.peeki (l, fn (_, (a', _)) => Attribute.equals (a, a')) of
            NONE => Error.bug "RDB.Heading.info"
          | SOME (i, (_, d)) => (i, d)

      val position = #1 o info
   end

datatype t = T of {heading: Heading.t,
                   body: Value.t list list ref}

fun add (T {heading = Heading.T attrs, body, ...}, r) =
   List.push
   (body,
    List.fold (rev attrs, [], fn ((a, d), ac) =>
              case List.peek (r, fn (a', _) => Attribute.equals (a, a')) of
                 NONE => Error.bug "RDB.add"
               | SOME (_, v) =>
                    if Domain.contains (d, v)
                       then v :: ac
                    else Error.bug "RDB.add"))

fun cardinality (T {body, ...}) = List.length (!body)

fun degree (T {heading, ...}) = Heading.degree heading

fun new {heading} = T {heading = Heading.T heading,
                     body = ref []}

fun project (T {heading, body, ...}, a: Attribute.t): Value.t list =
   let val n = Heading.position (heading, a)
   in List.fold (!body, [], fn (vs, ac) =>
                List.insert (vs, List.nth (vs, n), Value.<=))
   end

fun outputTable (t, out) =
   let
      val print = Out.outputc out
   in
      List.foreach (t, fn ss =>
                   (case ss of
                       [] => ()
                     | s :: ss =>
                          (print s
                           ; List.foreach (ss, fn s =>
                                          (print " "; print s)))
                          ; print "\n"))
   end

fun printTable {rdb as T {body, heading, ...}, row, col, entry, out}: unit =
   let
      val default = "*"
      val body = !body
      val rows = project (rdb, row)
      val cols = project (rdb, col)
      val nr = Heading.position (heading, row)
      val nc = Heading.position (heading, col)
      val ne = Heading.position (heading, entry)
      val table =
         ("" :: List.map (cols, Value.toString)) ::
         let
            val cols = rev cols
         in List.fold
            (rev rows, [], fn (r, ac) =>
             let
                val row =
                   List.fold
                   (cols, [], fn (c, ac) =>
                    let
                       val e =
                          case (List.peek
                                (body, fn t =>
                                 Value.equals (r, List.nth (t, nr))
                                 andalso Value.equals (c, List.nth (t, nc))))
                             of
                                NONE => default
                              | SOME t => Value.toString (List.nth (t, ne))
                    in e :: ac
                    end)
             in (Value.toString r :: row) :: ac
             end)
         end
      val justs =
         (Value.justification (hd rows)
          :: List.map (cols, Value.justification))
      val t = Justify.table {columnHeads = NONE, justs = justs, rows = table}
   in outputTable (t, out)
   end

fun printTable' {rdb as T {body, heading = Heading.T ads, ...},
                cols, sortBy, out}: unit =
   let
      val is = List.revMap (cols, fn a =>
                           valOf (List.index (ads, fn (a', _) =>
                                            Attribute.equals (a, a'))))
      val rows =
         List.revMap (!body, fn r =>
                     let val a = Array.fromList r
                     in List.revMap (is, fn i => Array.sub (a, i))
                     end)
      val justs = List.map (hd rows, Value.justification)
      val i = valOf (List.index (cols, fn a => Attribute.equals (a, sortBy)))
      val rows =
         QuickSort.sortList (rows, fn (r, r') =>
                             Value.<= (List.nth (r, i), List.nth (r', i)))
      val rows =
         List.map (cols, Attribute.toString)
         :: List.map (rows, fn r => List.map (r, Value.toString))
      val t = Justify.table {columnHeads = NONE,
                             justs = justs,
                             rows = rows}
   in outputTable (t, out)
   end

end
