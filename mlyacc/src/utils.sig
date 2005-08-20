(* Modified by mfluet@acm.org on 2005-8-01.
 * Update with SML/NJ 110.55+.
 *)
(* Modified by sweeks@acm.org on 2000-8-24.
 * Ported to MLton.
 *)
type int = Int.int

(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

signature ORDSET =
   sig
      type set
      type elem
      exception Select_arb
      val app : (elem -> unit) -> set -> unit
          and card: set -> int
          and closure: set * (elem -> set) -> set
          and difference: set * set -> set
          and elem_eq: (elem * elem -> bool)
          and elem_gt : (elem * elem -> bool)
          and empty: set
          and exists: (elem * set) -> bool
          and find : (elem * set)  ->  elem option
          and fold: ((elem * 'b) -> 'b) -> set -> 'b -> 'b
          and insert: (elem * set) -> set
          and is_empty: set -> bool
          and make_list: set -> elem list
          and make_set: (elem list -> set)
          and partition: (elem -> bool) -> (set -> set * set)
          and remove: (elem * set) -> set
          and revfold: ((elem * 'b) -> 'b) -> set -> 'b -> 'b
          and select_arb: set -> elem
          and set_eq: (set * set) -> bool
          and set_gt: (set * set) -> bool
          and singleton: (elem -> set)
          and union: set * set -> set
   end

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

signature HASH =
  sig
    type table
    type elem

    val size : table -> int
    val add : elem * table -> table
    val find : elem * table -> int option
    val exists : elem * table -> bool
    val empty : table
  end;
