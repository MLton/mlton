(* From the SML/NJ benchmark suite. *)
(* terms.sml:
 *
 * Manipulations over terms
 *)

signature TERMS =
  sig
    type head;
    datatype term =
      Var of int
    | Prop of head * term list;
    datatype binding = Bind of int * term;      
    val get: string -> head
    and headname: head -> string
    and add_lemma: term -> unit
    and apply_subst: binding list -> term -> term
    and rewrite: term -> term
  end;

structure Terms:TERMS =
  struct

    datatype term
      = Var of int
      | Prop of { name: string, props: (term * term) list ref } * term list

    type head = { name: string, props: (term * term) list ref }

    val lemmas = ref ([] : head list)

(* replacement for property lists *)

    fun headname {name = n, props=p} = n;

fun get name =
  let fun get_rec ((hd1 as {name=n,...})::hdl) =
      if n = name then hd1 else get_rec hdl
        | get_rec [] =
      let val entry = {name = name, props = ref []} in
        lemmas := entry :: !lemmas;
        entry
      end
  in
    get_rec (!lemmas)
  end
;

fun add_lemma (Prop(_, [(left as Prop({props=r,...},_)), right])) =
  r := (left, right) :: !r
;

(* substitutions *)

exception failure of string;

datatype binding = Bind of int * term
;

fun get_binding v =
  let fun get_rec [] = raise (failure "unbound")
        | get_rec (Bind(w,t)::rest) =
            if v = w then t else get_rec rest
  in
    get_rec
  end
;

fun apply_subst alist =
  let fun as_rec (term as Var v) =
            ((get_binding v alist) handle failure _ => term)
        | as_rec (Prop (head,argl)) =
            Prop (head, map as_rec argl)
  in
    as_rec
  end
;

exception Unify;

fun unify (term1, term2) = unify1 (term1, term2, [])
and unify1 (term1, term2, unify_subst) =
 (case term2 of
    Var v =>
      ((if get_binding v unify_subst = term1
        then unify_subst
        else raise Unify)
       handle failure _ =>
        Bind(v,term1)::unify_subst)
  | Prop (head2,argl2) =>
      case term1 of
         Var _ => raise Unify
       | Prop (head1,argl1) =>
           if head1=head2 then unify1_lst (argl1, argl2, unify_subst)
                          else raise Unify)
and unify1_lst ([], [], unify_subst) = unify_subst
  | unify1_lst (h1::r1, h2::r2, unify_subst) =
      unify1_lst(r1, r2, unify1(h1, h2, unify_subst))
  | unify1_lst _ = raise Unify
;

fun rewrite (term as Var _) = term
  | rewrite (Prop ((head as {props=p,...}), argl)) =
      rewrite_with_lemmas (Prop (head, map rewrite argl),  !p)
and rewrite_with_lemmas (term, []) = term
  | rewrite_with_lemmas (term, (t1,t2)::rest) =
        rewrite (apply_subst (unify (term, t1)) t2)
      handle unify =>
        rewrite_with_lemmas (term, rest)
;
end;
(* rules.sml:
 *)

structure Rules =
  struct

    open Terms;

    datatype cterm = CVar of int | CProp of string * cterm list;

    fun cterm_to_term (CVar v) = Var v
      | cterm_to_term (CProp(p, l)) = Prop(get p, map cterm_to_term l)

    fun add t = add_lemma (cterm_to_term t)

    val _ = (
add (CProp
("equal",
 [CProp ("compile",[CVar 5]), 
  CProp
  ("reverse",
   [CProp ("codegen",[CProp ("optimize",[CVar 5]), CProp ("nil",[])])])]));
add (CProp
("equal",
 [CProp ("eqp",[CVar 23, CVar 24]), 
  CProp ("equal",[CProp ("fix",[CVar 23]), CProp ("fix",[CVar 24])])]));
add (CProp
("equal",
 [CProp ("gt",[CVar 23, CVar 24]), CProp ("lt",[CVar 24, CVar 23])]));
add (CProp
("equal",
 [CProp ("le",[CVar 23, CVar 24]), CProp ("ge",[CVar 24, CVar 23])]));
add (CProp
("equal",
 [CProp ("ge",[CVar 23, CVar 24]), CProp ("le",[CVar 24, CVar 23])]));
add (CProp
("equal",
 [CProp ("boolean",[CVar 23]), 
  CProp
  ("or",
   [CProp ("equal",[CVar 23, CProp ("true",[])]), 
    CProp ("equal",[CVar 23, CProp ("false",[])])])]));
add (CProp
("equal",
 [CProp ("iff",[CVar 23, CVar 24]), 
  CProp
  ("and",
   [CProp ("implies",[CVar 23, CVar 24]), 
    CProp ("implies",[CVar 24, CVar 23])])]));
add (CProp
("equal",
 [CProp ("even1",[CVar 23]), 
  CProp
  ("if",
   [CProp ("zerop",[CVar 23]), CProp ("true",[]), 
    CProp ("odd",[CProp ("sub1",[CVar 23])])])]));
add (CProp
("equal",
 [CProp ("countps_",[CVar 11, CVar 15]), 
  CProp ("countps_loop",[CVar 11, CVar 15, CProp ("zero",[])])]));
add (CProp
("equal",
 [CProp ("fact_",[CVar 8]), 
  CProp ("fact_loop",[CVar 8, CProp ("one",[])])]));
add (CProp
("equal",
 [CProp ("reverse_",[CVar 23]), 
  CProp ("reverse_loop",[CVar 23, CProp ("nil",[])])]));
add (CProp
("equal",
 [CProp ("divides",[CVar 23, CVar 24]), 
  CProp ("zerop",[CProp ("remainder",[CVar 24, CVar 23])])]));
add (CProp
("equal",
 [CProp ("assume_true",[CVar 21, CVar 0]), 
  CProp ("cons",[CProp ("cons",[CVar 21, CProp ("true",[])]), CVar 0])]));
add (CProp
("equal",
 [CProp ("assume_false",[CVar 21, CVar 0]), 
  CProp ("cons",[CProp ("cons",[CVar 21, CProp ("false",[])]), CVar 0])]));
add (CProp
("equal",
 [CProp ("tautology_checker",[CVar 23]), 
  CProp ("tautologyp",[CProp ("normalize",[CVar 23]), CProp ("nil",[])])]));
add (CProp
("equal",
 [CProp ("falsify",[CVar 23]), 
  CProp ("falsify1",[CProp ("normalize",[CVar 23]), CProp ("nil",[])])]));
add (CProp
("equal",
 [CProp ("prime",[CVar 23]), 
  CProp
  ("and",
   [CProp ("not",[CProp ("zerop",[CVar 23])]), 
    CProp
    ("not",
     [CProp ("equal",[CVar 23, CProp ("add1",[CProp ("zero",[])])])]), 
    CProp ("prime1",[CVar 23, CProp ("sub1",[CVar 23])])])]));
add (CProp
("equal",
 [CProp ("and",[CVar 15, CVar 16]), 
  CProp
  ("if",
   [CVar 15, 
    CProp ("if",[CVar 16, CProp ("true",[]), CProp ("false",[])]), 
    CProp ("false",[])])]));
add (CProp
("equal",
 [CProp ("or",[CVar 15, CVar 16]), 
  CProp
  ("if",
   [CVar 15, CProp ("true",[]), 
    CProp ("if",[CVar 16, CProp ("true",[]), CProp ("false",[])]), 
    CProp ("false",[])])]));
add (CProp
("equal",
 [CProp ("not",[CVar 15]), 
  CProp ("if",[CVar 15, CProp ("false",[]), CProp ("true",[])])]));
add (CProp
("equal",
 [CProp ("implies",[CVar 15, CVar 16]), 
  CProp
  ("if",
   [CVar 15, 
    CProp ("if",[CVar 16, CProp ("true",[]), CProp ("false",[])]), 
    CProp ("true",[])])]));
add (CProp
("equal",
 [CProp ("fix",[CVar 23]), 
  CProp ("if",[CProp ("numberp",[CVar 23]), CVar 23, CProp ("zero",[])])]));
add (CProp
("equal",
 [CProp ("if",[CProp ("if",[CVar 0, CVar 1, CVar 2]), CVar 3, CVar 4]), 
  CProp
  ("if",
   [CVar 0, CProp ("if",[CVar 1, CVar 3, CVar 4]), 
    CProp ("if",[CVar 2, CVar 3, CVar 4])])]));
add (CProp
("equal",
 [CProp ("zerop",[CVar 23]), 
  CProp
  ("or",
   [CProp ("equal",[CVar 23, CProp ("zero",[])]), 
    CProp ("not",[CProp ("numberp",[CVar 23])])])]));
add (CProp
("equal",
 [CProp ("plus",[CProp ("plus",[CVar 23, CVar 24]), CVar 25]), 
  CProp ("plus",[CVar 23, CProp ("plus",[CVar 24, CVar 25])])]));
add (CProp
("equal",
 [CProp ("equal",[CProp ("plus",[CVar 0, CVar 1]), CProp ("zero",[])]), 
  CProp ("and",[CProp ("zerop",[CVar 0]), CProp ("zerop",[CVar 1])])]));
add (CProp
("equal",[CProp ("difference",[CVar 23, CVar 23]), CProp ("zero",[])]));
add (CProp
("equal",
 [CProp
  ("equal",
   [CProp ("plus",[CVar 0, CVar 1]), CProp ("plus",[CVar 0, CVar 2])]), 
  CProp ("equal",[CProp ("fix",[CVar 1]), CProp ("fix",[CVar 2])])]));
add (CProp
("equal",
 [CProp
  ("equal",[CProp ("zero",[]), CProp ("difference",[CVar 23, CVar 24])]), 
  CProp ("not",[CProp ("gt",[CVar 24, CVar 23])])]));
add (CProp
("equal",
 [CProp ("equal",[CVar 23, CProp ("difference",[CVar 23, CVar 24])]), 
  CProp
  ("and",
   [CProp ("numberp",[CVar 23]), 
    CProp
    ("or",
     [CProp ("equal",[CVar 23, CProp ("zero",[])]), 
      CProp ("zerop",[CVar 24])])])]));
add (CProp
("equal",
 [CProp
  ("meaning",
   [CProp ("plus_tree",[CProp ("append",[CVar 23, CVar 24])]), CVar 0]), 
  CProp
  ("plus",
   [CProp ("meaning",[CProp ("plus_tree",[CVar 23]), CVar 0]), 
    CProp ("meaning",[CProp ("plus_tree",[CVar 24]), CVar 0])])]));
add (CProp
("equal",
 [CProp
  ("meaning",
   [CProp ("plus_tree",[CProp ("plus_fringe",[CVar 23])]), CVar 0]), 
  CProp ("fix",[CProp ("meaning",[CVar 23, CVar 0])])]));
add (CProp
("equal",
 [CProp ("append",[CProp ("append",[CVar 23, CVar 24]), CVar 25]), 
  CProp ("append",[CVar 23, CProp ("append",[CVar 24, CVar 25])])]));
add (CProp
("equal",
 [CProp ("reverse",[CProp ("append",[CVar 0, CVar 1])]), 
  CProp
  ("append",[CProp ("reverse",[CVar 1]), CProp ("reverse",[CVar 0])])]));
add (CProp
("equal",
 [CProp ("times",[CVar 23, CProp ("plus",[CVar 24, CVar 25])]), 
  CProp
  ("plus",
   [CProp ("times",[CVar 23, CVar 24]), 
    CProp ("times",[CVar 23, CVar 25])])]));
add (CProp
("equal",
 [CProp ("times",[CProp ("times",[CVar 23, CVar 24]), CVar 25]), 
  CProp ("times",[CVar 23, CProp ("times",[CVar 24, CVar 25])])]));
add (CProp
("equal",
 [CProp
  ("equal",[CProp ("times",[CVar 23, CVar 24]), CProp ("zero",[])]), 
  CProp ("or",[CProp ("zerop",[CVar 23]), CProp ("zerop",[CVar 24])])]));
add (CProp
("equal",
 [CProp ("exec",[CProp ("append",[CVar 23, CVar 24]), CVar 15, CVar 4]), 
  CProp
  ("exec",[CVar 24, CProp ("exec",[CVar 23, CVar 15, CVar 4]), CVar 4])]));
add (CProp
("equal",
 [CProp ("mc_flatten",[CVar 23, CVar 24]), 
  CProp ("append",[CProp ("flatten",[CVar 23]), CVar 24])]));
add (CProp
("equal",
 [CProp ("member",[CVar 23, CProp ("append",[CVar 0, CVar 1])]), 
  CProp
  ("or",
   [CProp ("member",[CVar 23, CVar 0]), 
    CProp ("member",[CVar 23, CVar 1])])]));
add (CProp
("equal",
 [CProp ("member",[CVar 23, CProp ("reverse",[CVar 24])]), 
  CProp ("member",[CVar 23, CVar 24])]));
add (CProp
("equal",
 [CProp ("length",[CProp ("reverse",[CVar 23])]), 
  CProp ("length",[CVar 23])]));
add (CProp
("equal",
 [CProp ("member",[CVar 0, CProp ("intersect",[CVar 1, CVar 2])]), 
  CProp
  ("and",
   [CProp ("member",[CVar 0, CVar 1]), CProp ("member",[CVar 0, CVar 2])])]));
add (CProp
("equal",[CProp ("nth",[CProp ("zero",[]), CVar 8]), CProp ("zero",[])]));
add (CProp
("equal",
 [CProp ("exp",[CVar 8, CProp ("plus",[CVar 9, CVar 10])]), 
  CProp
  ("times",
   [CProp ("exp",[CVar 8, CVar 9]), CProp ("exp",[CVar 8, CVar 10])])]));
add (CProp
("equal",
 [CProp ("exp",[CVar 8, CProp ("times",[CVar 9, CVar 10])]), 
  CProp ("exp",[CProp ("exp",[CVar 8, CVar 9]), CVar 10])]));
add (CProp
("equal",
 [CProp ("reverse_loop",[CVar 23, CVar 24]), 
  CProp ("append",[CProp ("reverse",[CVar 23]), CVar 24])]));
add (CProp
("equal",
 [CProp ("reverse_loop",[CVar 23, CProp ("nil",[])]), 
  CProp ("reverse",[CVar 23])]));
add (CProp
("equal",
 [CProp ("count_list",[CVar 25, CProp ("sort_lp",[CVar 23, CVar 24])]), 
  CProp
  ("plus",
   [CProp ("count_list",[CVar 25, CVar 23]), 
    CProp ("count_list",[CVar 25, CVar 24])])]));
add (CProp
("equal",
 [CProp
  ("equal",
   [CProp ("append",[CVar 0, CVar 1]), CProp ("append",[CVar 0, CVar 2])]), 
  CProp ("equal",[CVar 1, CVar 2])]));
add (CProp
("equal",
 [CProp
  ("plus",
   [CProp ("remainder",[CVar 23, CVar 24]), 
    CProp ("times",[CVar 24, CProp ("quotient",[CVar 23, CVar 24])])]), 
  CProp ("fix",[CVar 23])]));
add (CProp
("equal",
 [CProp
  ("power_eval",[CProp ("big_plus",[CVar 11, CVar 8, CVar 1]), CVar 1]), 
  CProp ("plus",[CProp ("power_eval",[CVar 11, CVar 1]), CVar 8])]));
add (CProp
("equal",
 [CProp
  ("power_eval",
   [CProp ("big_plus",[CVar 23, CVar 24, CVar 8, CVar 1]), CVar 1]), 
  CProp
  ("plus",
   [CVar 8, 
    CProp
    ("plus",
     [CProp ("power_eval",[CVar 23, CVar 1]), 
      CProp ("power_eval",[CVar 24, CVar 1])])])]));
add (CProp
("equal",
 [CProp ("remainder",[CVar 24, CProp ("one",[])]), CProp ("zero",[])]));
add (CProp
("equal",
 [CProp ("lt",[CProp ("remainder",[CVar 23, CVar 24]), CVar 24]), 
  CProp ("not",[CProp ("zerop",[CVar 24])])]));
add (CProp
("equal",[CProp ("remainder",[CVar 23, CVar 23]), CProp ("zero",[])]));
add (CProp
("equal",
 [CProp ("lt",[CProp ("quotient",[CVar 8, CVar 9]), CVar 8]), 
  CProp
  ("and",
   [CProp ("not",[CProp ("zerop",[CVar 8])]), 
    CProp
    ("or",
     [CProp ("zerop",[CVar 9]), 
      CProp ("not",[CProp ("equal",[CVar 9, CProp ("one",[])])])])])]));
add (CProp
("equal",
 [CProp ("lt",[CProp ("remainder",[CVar 23, CVar 24]), CVar 23]), 
  CProp
  ("and",
   [CProp ("not",[CProp ("zerop",[CVar 24])]), 
    CProp ("not",[CProp ("zerop",[CVar 23])]), 
    CProp ("not",[CProp ("lt",[CVar 23, CVar 24])])])]));
add (CProp
("equal",
 [CProp ("power_eval",[CProp ("power_rep",[CVar 8, CVar 1]), CVar 1]), 
  CProp ("fix",[CVar 8])]));
add (CProp
("equal",
 [CProp
  ("power_eval",
   [CProp
    ("big_plus",
     [CProp ("power_rep",[CVar 8, CVar 1]), 
      CProp ("power_rep",[CVar 9, CVar 1]), CProp ("zero",[]), 
      CVar 1]), 
    CVar 1]), 
  CProp ("plus",[CVar 8, CVar 9])]));
add (CProp
("equal",
 [CProp ("gcd",[CVar 23, CVar 24]), CProp ("gcd",[CVar 24, CVar 23])]));
add (CProp
("equal",
 [CProp ("nth",[CProp ("append",[CVar 0, CVar 1]), CVar 8]), 
  CProp
  ("append",
   [CProp ("nth",[CVar 0, CVar 8]), 
    CProp
    ("nth",
     [CVar 1, CProp ("difference",[CVar 8, CProp ("length",[CVar 0])])])])]));
add (CProp
("equal",
 [CProp ("difference",[CProp ("plus",[CVar 23, CVar 24]), CVar 23]), 
  CProp ("fix",[CVar 24])]));
add (CProp
("equal",
 [CProp ("difference",[CProp ("plus",[CVar 24, CVar 23]), CVar 23]), 
  CProp ("fix",[CVar 24])]));
add (CProp
("equal",
 [CProp
  ("difference",
   [CProp ("plus",[CVar 23, CVar 24]), CProp ("plus",[CVar 23, CVar 25])]), 
  CProp ("difference",[CVar 24, CVar 25])]));
add (CProp
("equal",
 [CProp ("times",[CVar 23, CProp ("difference",[CVar 2, CVar 22])]), 
  CProp
  ("difference",
   [CProp ("times",[CVar 2, CVar 23]), 
    CProp ("times",[CVar 22, CVar 23])])]));
add (CProp
("equal",
 [CProp ("remainder",[CProp ("times",[CVar 23, CVar 25]), CVar 25]), 
  CProp ("zero",[])]));
add (CProp
("equal",
 [CProp
  ("difference",
   [CProp ("plus",[CVar 1, CProp ("plus",[CVar 0, CVar 2])]), CVar 0]), 
  CProp ("plus",[CVar 1, CVar 2])]));
add (CProp
("equal",
 [CProp
  ("difference",
   [CProp ("add1",[CProp ("plus",[CVar 24, CVar 25])]), CVar 25]), 
  CProp ("add1",[CVar 24])]));
add (CProp
("equal",
 [CProp
  ("lt",
   [CProp ("plus",[CVar 23, CVar 24]), CProp ("plus",[CVar 23, CVar 25])]), 
  CProp ("lt",[CVar 24, CVar 25])]));
add (CProp
("equal",
 [CProp
  ("lt",
   [CProp ("times",[CVar 23, CVar 25]), 
    CProp ("times",[CVar 24, CVar 25])]), 
  CProp
  ("and",
   [CProp ("not",[CProp ("zerop",[CVar 25])]), 
    CProp ("lt",[CVar 23, CVar 24])])]));
add (CProp
("equal",
 [CProp ("lt",[CVar 24, CProp ("plus",[CVar 23, CVar 24])]), 
  CProp ("not",[CProp ("zerop",[CVar 23])])]));
add (CProp
("equal",
 [CProp
  ("gcd",
   [CProp ("times",[CVar 23, CVar 25]), 
    CProp ("times",[CVar 24, CVar 25])]), 
  CProp ("times",[CVar 25, CProp ("gcd",[CVar 23, CVar 24])])]));
add (CProp
("equal",
 [CProp ("value",[CProp ("normalize",[CVar 23]), CVar 0]), 
  CProp ("value",[CVar 23, CVar 0])]));
add (CProp
("equal",
 [CProp
  ("equal",
   [CProp ("flatten",[CVar 23]), 
    CProp ("cons",[CVar 24, CProp ("nil",[])])]), 
  CProp
  ("and",
   [CProp ("nlistp",[CVar 23]), CProp ("equal",[CVar 23, CVar 24])])]));
add (CProp
("equal",
 [CProp ("listp",[CProp ("gother",[CVar 23])]), 
  CProp ("listp",[CVar 23])]));
add (CProp
("equal",
 [CProp ("samefringe",[CVar 23, CVar 24]), 
  CProp
  ("equal",[CProp ("flatten",[CVar 23]), CProp ("flatten",[CVar 24])])]));
add (CProp
("equal",
 [CProp
  ("equal",
   [CProp ("greatest_factor",[CVar 23, CVar 24]), CProp ("zero",[])]), 
  CProp
  ("and",
   [CProp
    ("or",
     [CProp ("zerop",[CVar 24]), 
      CProp ("equal",[CVar 24, CProp ("one",[])])]), 
    CProp ("equal",[CVar 23, CProp ("zero",[])])])]));
add (CProp
("equal",
 [CProp
  ("equal",
   [CProp ("greatest_factor",[CVar 23, CVar 24]), CProp ("one",[])]), 
  CProp ("equal",[CVar 23, CProp ("one",[])])]));
add (CProp
("equal",
 [CProp ("numberp",[CProp ("greatest_factor",[CVar 23, CVar 24])]), 
  CProp
  ("not",
   [CProp
    ("and",
     [CProp
      ("or",
       [CProp ("zerop",[CVar 24]), 
        CProp ("equal",[CVar 24, CProp ("one",[])])]), 
      CProp ("not",[CProp ("numberp",[CVar 23])])])])]));
add (CProp
("equal",
 [CProp ("times_list",[CProp ("append",[CVar 23, CVar 24])]), 
  CProp
  ("times",
   [CProp ("times_list",[CVar 23]), CProp ("times_list",[CVar 24])])]));
add (CProp
("equal",
 [CProp ("prime_list",[CProp ("append",[CVar 23, CVar 24])]), 
  CProp
  ("and",
   [CProp ("prime_list",[CVar 23]), CProp ("prime_list",[CVar 24])])]));
add (CProp
("equal",
 [CProp ("equal",[CVar 25, CProp ("times",[CVar 22, CVar 25])]), 
  CProp
  ("and",
   [CProp ("numberp",[CVar 25]), 
    CProp
    ("or",
     [CProp ("equal",[CVar 25, CProp ("zero",[])]), 
      CProp ("equal",[CVar 22, CProp ("one",[])])])])]));
add (CProp
("equal",
 [CProp ("ge",[CVar 23, CVar 24]), 
  CProp ("not",[CProp ("lt",[CVar 23, CVar 24])])]));
add (CProp
("equal",
 [CProp ("equal",[CVar 23, CProp ("times",[CVar 23, CVar 24])]), 
  CProp
  ("or",
   [CProp ("equal",[CVar 23, CProp ("zero",[])]), 
    CProp
    ("and",
     [CProp ("numberp",[CVar 23]), 
      CProp ("equal",[CVar 24, CProp ("one",[])])])])]));
add (CProp
("equal",
 [CProp ("remainder",[CProp ("times",[CVar 24, CVar 23]), CVar 24]), 
  CProp ("zero",[])]));
add (CProp
("equal",
 [CProp ("equal",[CProp ("times",[CVar 0, CVar 1]), CProp ("one",[])]), 
  CProp
  ("and",
   [CProp ("not",[CProp ("equal",[CVar 0, CProp ("zero",[])])]), 
    CProp ("not",[CProp ("equal",[CVar 1, CProp ("zero",[])])]), 
    CProp ("numberp",[CVar 0]), CProp ("numberp",[CVar 1]), 
    CProp ("equal",[CProp ("sub1",[CVar 0]), CProp ("zero",[])]), 
    CProp ("equal",[CProp ("sub1",[CVar 1]), CProp ("zero",[])])])]));
add (CProp
("equal",
 [CProp
  ("lt",
   [CProp ("length",[CProp ("delete",[CVar 23, CVar 11])]), 
    CProp ("length",[CVar 11])]), 
  CProp ("member",[CVar 23, CVar 11])]));
add (CProp
("equal",
 [CProp ("sort2",[CProp ("delete",[CVar 23, CVar 11])]), 
  CProp ("delete",[CVar 23, CProp ("sort2",[CVar 11])])]));
add (CProp ("equal",[CProp ("dsort",[CVar 23]), CProp ("sort2",[CVar 23])]));
add (CProp
("equal",
 [CProp
  ("length",
   [CProp
    ("cons",
     [CVar 0, 
      CProp
      ("cons",
       [CVar 1, 
        CProp
        ("cons",
         [CVar 2, 
          CProp
          ("cons",
           [CVar 3, 
            CProp ("cons",[CVar 4, CProp ("cons",[CVar 5, CVar 6])])])])])])])
  , CProp ("plus",[CProp ("six",[]), CProp ("length",[CVar 6])])]));
add (CProp
("equal",
 [CProp
  ("difference",
   [CProp ("add1",[CProp ("add1",[CVar 23])]), CProp ("two",[])]), 
  CProp ("fix",[CVar 23])]));
add (CProp
("equal",
 [CProp
  ("quotient",
   [CProp ("plus",[CVar 23, CProp ("plus",[CVar 23, CVar 24])]), 
    CProp ("two",[])]), 
  CProp
  ("plus",[CVar 23, CProp ("quotient",[CVar 24, CProp ("two",[])])])]));
add (CProp
("equal",
 [CProp ("sigma",[CProp ("zero",[]), CVar 8]), 
  CProp
  ("quotient",
   [CProp ("times",[CVar 8, CProp ("add1",[CVar 8])]), CProp ("two",[])])]));
add (CProp
("equal",
 [CProp ("plus",[CVar 23, CProp ("add1",[CVar 24])]), 
  CProp
  ("if",
   [CProp ("numberp",[CVar 24]), 
    CProp ("add1",[CProp ("plus",[CVar 23, CVar 24])]), 
    CProp ("add1",[CVar 23])])]));
add (CProp
("equal",
 [CProp
  ("equal",
   [CProp ("difference",[CVar 23, CVar 24]), 
    CProp ("difference",[CVar 25, CVar 24])]), 
  CProp
  ("if",
   [CProp ("lt",[CVar 23, CVar 24]), 
    CProp ("not",[CProp ("lt",[CVar 24, CVar 25])]), 
    CProp
    ("if",
     [CProp ("lt",[CVar 25, CVar 24]), 
      CProp ("not",[CProp ("lt",[CVar 24, CVar 23])]), 
      CProp ("equal",[CProp ("fix",[CVar 23]), CProp ("fix",[CVar 25])])])])])
);
add (CProp
("equal",
 [CProp
  ("meaning",
   [CProp ("plus_tree",[CProp ("delete",[CVar 23, CVar 24])]), CVar 0]), 
  CProp
  ("if",
   [CProp ("member",[CVar 23, CVar 24]), 
    CProp
    ("difference",
     [CProp ("meaning",[CProp ("plus_tree",[CVar 24]), CVar 0]), 
      CProp ("meaning",[CVar 23, CVar 0])]), 
    CProp ("meaning",[CProp ("plus_tree",[CVar 24]), CVar 0])])]));
add (CProp
("equal",
 [CProp ("times",[CVar 23, CProp ("add1",[CVar 24])]), 
  CProp
  ("if",
   [CProp ("numberp",[CVar 24]), 
    CProp
    ("plus",
     [CVar 23, CProp ("times",[CVar 23, CVar 24]), 
      CProp ("fix",[CVar 23])])])]));
add (CProp
("equal",
 [CProp ("nth",[CProp ("nil",[]), CVar 8]), 
  CProp
  ("if",[CProp ("zerop",[CVar 8]), CProp ("nil",[]), CProp ("zero",[])])]));
add (CProp
("equal",
 [CProp ("last",[CProp ("append",[CVar 0, CVar 1])]), 
  CProp
  ("if",
   [CProp ("listp",[CVar 1]), CProp ("last",[CVar 1]), 
    CProp
    ("if",
     [CProp ("listp",[CVar 0]), 
      CProp ("cons",[CProp ("car",[CProp ("last",[CVar 0])]), CVar 1]), 
      CVar 1])])]));
add (CProp
("equal",
 [CProp ("equal",[CProp ("lt",[CVar 23, CVar 24]), CVar 25]), 
  CProp
  ("if",
   [CProp ("lt",[CVar 23, CVar 24]), 
    CProp ("equal",[CProp ("true",[]), CVar 25]), 
    CProp ("equal",[CProp ("false",[]), CVar 25])])]));
add (CProp
("equal",
 [CProp ("assignment",[CVar 23, CProp ("append",[CVar 0, CVar 1])]), 
  CProp
  ("if",
   [CProp ("assignedp",[CVar 23, CVar 0]), 
    CProp ("assignment",[CVar 23, CVar 0]), 
    CProp ("assignment",[CVar 23, CVar 1])])]));
add (CProp
("equal",
 [CProp ("car",[CProp ("gother",[CVar 23])]), 
  CProp
  ("if",
   [CProp ("listp",[CVar 23]), 
    CProp ("car",[CProp ("flatten",[CVar 23])]), CProp ("zero",[])])]));
add (CProp
("equal",
 [CProp ("flatten",[CProp ("cdr",[CProp ("gother",[CVar 23])])]), 
  CProp
  ("if",
   [CProp ("listp",[CVar 23]), 
    CProp ("cdr",[CProp ("flatten",[CVar 23])]), 
    CProp ("cons",[CProp ("zero",[]), CProp ("nil",[])])])]));
add (CProp
("equal",
 [CProp ("quotient",[CProp ("times",[CVar 24, CVar 23]), CVar 24]), 
  CProp
  ("if",
   [CProp ("zerop",[CVar 24]), CProp ("zero",[]), 
    CProp ("fix",[CVar 23])])]));
add (CProp
("equal",
 [CProp ("get",[CVar 9, CProp ("set",[CVar 8, CVar 21, CVar 12])]), 
  CProp
  ("if",
   [CProp ("eqp",[CVar 9, CVar 8]), CVar 21, 
    CProp ("get",[CVar 9, CVar 12])])])))

  end; (* Rules *)

(* boyer.sml:
 *
 * Tautology checker
 *)

signature BOYER =
  sig
    include TERMS
    val tautp: term -> bool
  end

structure Boyer: BOYER =
  struct

open Terms

fun mem x [] = false
  | mem x (y::L) = x=y orelse mem x L

fun truep (x, lst) =
  case x of
    Prop(head, _) =>
      headname head = "true" orelse mem x lst
  | _ =>
      mem x lst

and falsep (x, lst) =
  case x of
    Prop(head, _) =>
      headname head = "false" orelse mem x lst
  | _ =>
      mem x lst

fun tautologyp (x, true_lst, false_lst) =
 if truep (x, true_lst) then true else
 if falsep (x, false_lst) then false else
 (case x of
     Var _ => false
   | Prop (head,[test, yes, no]) =>
        if headname head = "if" then
          if truep (test, true_lst) then
            tautologyp (yes, true_lst, false_lst)
          else if falsep (test, false_lst) then
            tautologyp (no, true_lst, false_lst)
          else tautologyp (yes, test::true_lst, false_lst) andalso
               tautologyp (no, true_lst, test::false_lst)
        else
          false)

    fun tautp x = tautologyp(rewrite x, [], []);

  end; (* Boyer *)

signature BMARK =
  sig
    val doit : unit -> unit
    val testit : TextIO.outstream -> unit
  end;

(* the benchmark *)
structure Main : BMARK =
  struct

    open Terms;
    open Boyer;

val subst =
[Bind(23,
             Prop
              (get "f",
               [Prop
                (get "plus",
                 [Prop (get "plus",[Var 0, Var 1]),
                  Prop (get "plus",[Var 2, Prop (get "zero",[])])])])),
 Bind(24,
             Prop
              (get "f",
               [Prop
                (get "times",
                 [Prop (get "times",[Var 0, Var 1]),
                  Prop (get "plus",[Var 2, Var 3])])])),
 Bind(25,
             Prop
              (get "f",
               [Prop
                (get "reverse",
                 [Prop
                  (get "append",
                   [Prop (get "append",[Var 0, Var 1]),
                    Prop (get "nil",[])])])])),
 Bind(20,
             Prop
              (get "equal",
               [Prop (get "plus",[Var 0, Var 1]),
                Prop (get "difference",[Var 23, Var 24])])),
 Bind(22,
             Prop
              (get "lt",
               [Prop (get "remainder",[Var 0, Var 1]),
                Prop (get "member",[Var 0, Prop (get "length",[Var 1])])]))]

val term =
           Prop
            (get "implies",
             [Prop
              (get "and",
               [Prop (get "implies",[Var 23, Var 24]),
                Prop
                (get "and",
                 [Prop (get "implies",[Var 24, Var 25]),
                  Prop
                  (get "and",
                   [Prop (get "implies",[Var 25, Var 20]),
                    Prop (get "implies",[Var 20, Var 22])])])]),
              Prop (get "implies",[Var 23, Var 22])])

    fun testit outstrm = if tautp (apply_subst subst term)
          then TextIO.output (outstrm, "Proved!\n")
          else TextIO.output (outstrm, "Cannot prove!\n")

    fun doit () = (tautp (apply_subst subst term); ())

  end; (* Main *)

structure Main =
  struct
    val doit = 
       fn n =>
       let
          fun loop n =
             if n = 0
                then ()
             else (Main.doit ();
                   loop(n-1))
       in loop n
       end
  end;
