val toString = Int.toString
   
(* Ported to kit 23/4/97 by Mads.
   Commented out module phrases and changed a couple of sml/nj things (like makestring)
   No rewriting to please region inference.
   Still it uses less that 100Kb when it runs (see region.ps)
*)

(* This is a quickly written type checker for subtyping, using the
   MATCH and TYPE algorithms from the Fuh and Mishra paper.

   The code is ugly at places, and no consideration has been given to
   effeciency  -- please accept my apologies 
   
   -- Mads*)

(*
structure List =  (* list utilities *)
struct
*)
  type 'a set = 'a list;   (* sets are represented by lists without repeated
                              elements *)
  val emptyset = []

  fun isin(x,[])=false
    | isin(x,(y::rest)) = x=y orelse isin(x,rest)
  
  fun union([],l) = l
    | union((x::rest),l) = if isin(x,l) then union(rest,l) 
                           else x:: union(rest,l);
  fun intersect([],l) = []
    | intersect((x::rest),l) = if isin(x,l) then x:: intersect(rest,l) 
                               else  intersect(rest,l);
  fun setminus([],l) = []
    | setminus(x::l,l') = if isin(x,l') then setminus(l,l') 
                          else x:: setminus(l,l');
  fun foldr f b [] = b
    | foldr f b (x::rest) = f(x,foldr f b rest);

  fun foldl f base []      = base
  |   foldl f base (x::xs) = foldl f (f(x, base)) xs

  (* function for finding transitive closure. 
     Culled from other application.
     DO NOT READ (CRUDE AND INEFFICIENT) *)

  fun transClos (eq: 'a * 'a -> bool)
                (insert_when_eq: bool)
                (newsets: 'a set list,
                 oldsets: 'a set list): bool * 'a set list =
(* The lists in oldsets are pairwise disjoint, but nothing is known about the
lists in newsets. The resulting sets are pairwise disjoint,
and moreover, the resulting boolean is true if and only if 
every member (i.e. list) of newsets, is contained in some member (i.e., list)
of oldsets *)
  let
    val any: bool = true (* could be false, for that matter *)

    fun isin(x,[]) = false
      | isin(x,x'::rest) = eq(x,x') orelse isin(x,rest)

   (* In the following function, L is a list of pairwise disjoint
      sets. S' is the set currently under construction;
      it is also disjoint from the sets in L.  x 
      will be added to S' if x is not in any set in L.  *)

    fun add(x:'a,
              (found_fixpt: bool, found_class: bool, L as [], S')):
               bool * bool * 'a list list * 'a list =
           if isin(x,S') 
           then if insert_when_eq
                then
                  (found_fixpt andalso found_class, found_class,[],x::S')
                else
                  (found_fixpt andalso found_class, found_class,[],S')
           else
                 (false, any, [], x::S')

      | add(x,(found_fixpt,found_class,(S::L), S')) =
           if isin(x,S) 
           then
             if insert_when_eq 
             then
               (found_fixpt andalso not found_class,true, L, x::(S @ S'))
             else
               (found_fixpt andalso not found_class,true, L, S @ S')
           else
              let val (found_fixpt1, found_class1, L1, S1)=
                    add(x,(found_fixpt,found_class,L,S'))
               in 
                 (found_fixpt andalso found_fixpt1,
                  found_class orelse found_class1,
                  S::L1,
                  S1
                 )
              end
                        
    fun add_S (S,(found_fixpt, oldsets)) : bool * 'a set list =
      let
        val (found_fixpt1, _, L1, S1) = 
          foldl add (found_fixpt, false, oldsets, emptyset) S
       in 
         case S1 of 
           [] => (found_fixpt andalso found_fixpt1, L1)
         | _  => (found_fixpt andalso found_fixpt1, S1::L1)
      end

   in
     foldl add_S (true, oldsets) newsets
  end
(*
end;
*)
(*
structure Variables = (* program variables *)
struct
*)
  type var  = string
(*
end;
*)
(*structure Type =  (* Types, substitutions and matching *)
struct
local
(*  open List Variables*)
in
*)  datatype ty = INT | REAL | ARROW of ty * ty | PROD of ty * ty | TYVAR of int
  
  type subst = ty -> ty
  
  fun mk_subst_ty(i: int,ty:ty):subst =
  let
    fun S(INT) = INT
      | S(REAL)  = REAL
      | S(ARROW(ty1,ty2)) = ARROW(S ty1, S ty2)
      | S(PROD(ty1,ty2)) = PROD(S ty1, S ty2)
      | S(tv as TYVAR j) = if i=j then ty else tv
  in 
    S
  end;
  
  val Id = fn x => x;
  
  val r = ref 0;  (* counter for fresh type variables *)
  fun fresh() = (r:= !r + 1; ! r);
  fun fresh_ty() = TYVAR(fresh());

  (* free variables *)
  
  fun fv_ty (INT,acc) = acc
    | fv_ty (REAL,acc) = acc
    | fv_ty(ARROW(t1,t2), acc) = fv_ty(t1,fv_ty(t2,acc))
    | fv_ty(PROD(t1,t2), acc) = fv_ty(t1,fv_ty(t2,acc))
    | fv_ty((TYVAR i),acc) = if isin(i,acc) then acc else i :: acc;
  
  fun tyvars(t:ty) = fv_ty(t,[])

  fun fv_tyenv TE =
    foldr (fn((i,tau),acc)=> fv_ty(tau,acc)) [] TE

  (* ALLNEW, page 168: create a fresh copy of a type, using type variables 
     at the leaves of the type *)

  fun allnew(INT) = fresh_ty()
    | allnew(REAL) = fresh_ty()
    | allnew(ARROW(t1,t2)) = ARROW(allnew(t1),allnew(t2))
    | allnew(PROD(t1,t2)) = PROD(allnew(t1),allnew(t2))
    | allnew(TYVAR _) = fresh_ty();

  (* PAIR, page 168: create list of atomic types that occur in the same
     places of t1 and t2 *)

  fun pair(ARROW(t1,t2),ARROW(t1',t2')) = pair(t1,t1') @ pair (t2,t2')
    | pair(PROD(t1,t2),PROD(t1',t2')) = pair(t1,t1') @ pair (t2,t2')
    | pair(t1,t2) = [[t1,t2]]

  (* dealing with equivalence classes of atomic types -- for algorithm MATCH *)

  (* M_v, page 168: removing that equivalence class containing v from M *)

  fun remove([], v) = []
    | remove((class::rest), v) = 
       if isin(v,class) then rest else class:: remove(rest,v);

  (* [a]_M, page 168: the equivalence class containing a *)

  exception ClassOff

  fun class_of([],a) = raise ClassOff
    | class_of((class::rest), a) = 
       if isin(a,class) then class else class_of(rest,a);
 
  (* [t]^M, page 168: the set of type variables eqivalent to some type variable
     occurring in non-atomic type t *)

  fun equiv_tyvars(t,M): ty list = 
      foldr union [] ((map (fn a=> class_of(M,TYVAR a))  (tyvars(t))): ty list list) ;

  fun transitive_closure(oldsets,newsets) = 
     #2(transClos(op = : ty*ty-> bool)(false)(newsets,oldsets))
  
  (* Coercion sets *)

  type coercion = ty * ty

  type coercion_set = coercion list

  fun on_C(S,C) = map (fn(t1,t2) => (S t1, S t2)) C

  fun atomic t = 
         case t of 
           INT => true
         | REAL => true
         | TYVAR _ => true
         | _ => false;

  (* diagonal is used in match to create initial equivalence relation*)


  fun diagonal(C: coercion_set) = 
    let
      fun diag(t,acc) =
        case t of
          INT => union([INT],acc)
        | REAL => union([REAL],acc)
        | PROD(t1,t2) => diag(t1,diag(t2,acc))
        | ARROW(t1,t2) => diag(t1,diag(t2,acc))
        | TYVAR _ => union([t],acc)
    
      fun diag2((t1,t2),acc) = diag(t1,diag(t2,acc))

      val atomics = foldr diag2 [] C
    in
      map (fn atomic => [atomic,atomic]) atomics
    end

  fun ground_atomic t = 
         case t of 
           INT => true
         | REAL => true
         | _ => false;

  fun contains_no_type_constant [] = true
    | contains_no_type_constant (t::rest) = 
        not (ground_atomic t) andalso contains_no_type_constant rest;

  exception MATCH

  local
    fun match1([]:coercion_set,S:subst,M) = S
      | match1((t1,t2)::C, S, M) =
         if atomic t1 andalso atomic t2 then  atomicElimination(t1,t2,C,S,M)
         else if atomic t1 then expansion(t1,t2,C,S,M)
         else if atomic t2 then expansion(t2,t1,C,S,M)
         else decomposition(t1,t2,C,S,M)
  
    and atomicElimination(t1,t2,C,S,M) =
         match1(C,S,transitive_closure(M,[[t1,t2]]))
  
    and decomposition(ARROW(t1,t2),ARROW(t1',t2'),C,S,M)=
         match1((t1',t1)::(t2,t2')::C, S, M)
      | decomposition(PROD(t1,t2),PROD(t1',t2'),C,S,M)=
         match1((t1,t1')::(t2,t2')::C, S, M)
      | decomposition(_) = raise MATCH
  
    and expansion(t1:ty,t2:ty,C,S,M) =
         case intersect(class_of(M,t1), equiv_tyvars(t2,M))  (* occurs check *)
         of 
          []=> if contains_no_type_constant(class_of(M,t1))  
               then
                    (* not matching of int or real with arrow or prod *)
                    let 
                      fun loop([],C,S,M) = match1(C,S,M)
                        | loop((TYVAR alpha)::tyvars, C, S, M) =
                           let val t' = allnew t2
                               val delta = mk_subst_ty(alpha,t')
                           in
                              loop(tyvars, on_C(delta,C), delta o S, 
                                   transitive_closure(M,pair(t2,t')))
                           end
                        | loop((_::tyvars), C, S, M) = 
                           (* skip atomic types, that are not variables *)
                           loop(tyvars, C, S, M)
                   in
                     loop(class_of(M,t1),C,S,M)
                   end
          
               else (* matching of int or real with arrow or prod *)
                    raise MATCH
  
         | _ => (* occurs check failed *)
                    raise MATCH
  in

    fun match(C) = match1(C,Id,diagonal C);

  end

  (* Type Environments *)

  type tyenv = (var * ty)list  (* no polymorphism! *)

  (* looking up variables in the type environement *)

  exception Lookup;
  
  fun lookup(i,[]) = raise Lookup
    | lookup(i,((j,sigma)::rest)) = if i=j then sigma else lookup(i,rest);

  fun on_tyenv(S:subst,TE:tyenv) = 
      map (fn(i,tau) => (i, S tau)) TE
  
  (* pretty printing -- actually not very pretty, but it will do *)
  
  (* precedences: ->  :  1
                  *   :  2  *)
  
  fun pp_ty' (context:int,INT) = "INT"
    | pp_ty'(_,REAL) = "REAL"
    | pp_ty'(context,ARROW(ty1, ty2)) = 
        let val s = pp_ty'(2,ty1) ^ "->" ^
                   pp_ty'(1, ty2) 
        in 
          if context > 1 then "(" ^ s ^ ")"  else s
        end       
    | pp_ty'(context,PROD(ty1, ty2)) = 
        let val s = pp_ty'(3,ty1) ^ "*" ^  pp_ty'(3,ty2)
        in
          if context > 2 then "(" ^ s ^ ")"
          else s
        end
    | pp_ty'(context,(TYVAR i)) = "'a" ^ toString i;
  
  fun pp_ty ty = pp_ty'(0,ty)
  
  local
    fun filter [] = []
      | filter ((x,sigma)::rest) = 
          if isin(x,["fst","snd","floor"]) 
             (* hack to avoid printing of built-in variables fst, snd and floor *)
          then filter rest
          else (x,sigma)::filter rest
    
    fun pp_tyenv [] = ""
      | pp_tyenv ([(x,sigma)]) = 
             x ^ ":" ^ pp_ty sigma ^ " "
      | pp_tyenv ((x,sigma)::rest) = 
             x ^ ":" ^ pp_ty sigma ^ "," ^ pp_tyenv rest;
  in
    val pp_tyenv = pp_tyenv o filter
  end

  fun pp_coercion(tau1,tau2) = "\n    " ^ pp_ty tau1 ^ " |> " ^ pp_ty tau2

  fun pp_coercion_set [] = ""
    | pp_coercion_set[coercion] = pp_coercion coercion
    | pp_coercion_set(coercion::rest) = 
         pp_coercion coercion ^ ", " ^ pp_coercion_set rest

(*
end (* local *)
end;
*)

(*structure Expressions =
struct
local
  open List Variables
in
*)  datatype exp = VAR of var
               | INTCON of int 
               | REALCON of real
               | LAM of var * exp 
               | APP of exp * exp  
               | LET of var * exp * exp
               | PLUS of exp 
               | MINUS of exp 
               | PAIR of exp * exp;

  (* pretty printing *)

  fun pp_exp(VAR x) = x
    | pp_exp(LAM(x,e')) = "(fn " ^ x ^ " =>" ^ pp_exp e' ^ ")"
    | pp_exp(APP(e1,e2)) = "(" ^ pp_exp e1 ^ " @ " ^ pp_exp e2 ^ ")"
    | pp_exp(LET(x,e1,e2)) = "let " ^ x ^ "=" ^ pp_exp e1 ^ " in " ^ pp_exp e2 ^ "end"
    | pp_exp(INTCON i) = toString i 
    | pp_exp(REALCON r) = toString(floor r) ^ ".?"
    | pp_exp(PLUS(e1)) = "(+" ^ pp_exp e1 ^ ")"
    | pp_exp(MINUS(e1)) = "(-" ^ pp_exp e1 ^ ")"
    | pp_exp(PAIR(e1,e2)) = "(" ^ pp_exp e1 ^ "," ^ pp_exp e2 ^ ")"
(*
end (* local *) 
end;   
*)


(*structure TypeChecker=
struct  
local
  open List Variables Type Expressions
in
*)
  type job = tyenv * exp * ty

  fun TYPE(TE:tyenv,e:exp) : ty * coercion_set = 
      let val alpha0 = fresh_ty()
          val C = TY([(TE,e,alpha0)],[])
      in  (alpha0, C)
      end

  and TY([]: job list,            C: coercion_set)= C
    | TY((job as (TE,e,tau)) :: rest, C: coercion_set)=
        case e of
          VAR x => TY(rest, (lookup(x,TE),tau)::C)
        | LAM(x,e') =>
            let val (new_ty1,new_ty2) = (fresh_ty(),fresh_ty());
                val TE' = (x,(new_ty1))::TE
             in 
                TY((TE',e',new_ty2)::rest, (ARROW(new_ty1,new_ty2), tau):: C)
            end
        | APP(e1,e2) =>
            let val (new_ty1,new_ty2) = (fresh_ty(),fresh_ty());
             in
                TY((TE,e1,ARROW(new_ty1,new_ty2))::(TE,e2,new_ty1)::rest,
                   (new_ty2,tau)::C)
            end
        | LET(x,e1,e2) =>
            let val (new_ty1,new_ty2) = (fresh_ty(),fresh_ty());
             in
                TY((TE,e1,new_ty1)::((x,new_ty1)::TE,e2,new_ty2)::rest,
                   (new_ty2,tau)::C)
            end
        | INTCON i => TY(rest, (INT, tau):: C)
        | REALCON i => TY(rest, (REAL, tau):: C)
      
        | PLUS e1 =>  TY((TE,e1, PROD(INT,INT))::rest, (PROD(INT,INT),tau):: C)
        | MINUS e1 => TY((TE,e1, PROD(INT,INT))::rest, (PROD(INT,INT),tau):: C)
        | PAIR(e1,e2) =>
            let val (new_ty1,new_ty2) = (fresh_ty(),fresh_ty());
             in
               TY((TE,e1,new_ty1)::(TE,e2,new_ty2)::rest, 
                  (PROD(new_ty1,new_ty2),tau)::C)
            end

  type judgement = coercion_set * tyenv * exp * ty

  fun pp_judgement(C,TE,e,tau) =
  "\nCoersion set:     " ^ pp_coercion_set C ^ 
  "\nType environment: " ^ pp_tyenv TE ^     
  "\nExpression:       " ^ pp_exp e ^        
  "\nType:             " ^ pp_ty tau;

(*
end (* local *)
end;
*)

  
(*structure Run =
struct
local
  open List Variables Type Expressions TypeChecker
in  
*)  val e0 = (* let val Id = fn x => x in (Id 3, Id 4.0) end *)
    let
      val Id = LAM("x",VAR "x")
      val pair = PAIR(APP(VAR "Id", INTCON 3), APP(VAR "Id", REALCON 4.0))
    in
      LET("Id",Id, pair)
    end;
  
  val e1 = (* let val makepair = fn x => (x,x) in makepair 3.14 *)
    let
      val makepair = LAM("x",PAIR(VAR "x", VAR "x"))
    in
      LET("makepair", makepair, APP(VAR "makepair", REALCON 3.14))
    end
  
  val e2 = (* let fun mappair = fn f => fn x => (f (fst x), f (snd x)) 
              in mappair floor (3.14,2.18) end  *)
  
    let
      val mappair = LAM("f",LAM("x",
                          PAIR(APP(VAR "f",APP(VAR "fst", VAR "x")), 
                                   APP(VAR "f",APP(VAR "snd", VAR "x")))))
    in
      LET("mappair",mappair, APP(APP(VAR "mappair", VAR "floor"), 
                  PAIR(REALCON 3.14, REALCON 2.18)))
    end;
  
  val e3 = (* fn x => x *) 
     LAM("x",VAR "x")
  val e4 = (* fn f => fn x => f x *)   
     LAM("f",LAM("x",APP(VAR "f", VAR "x")));
  val e5 = (* fn f => fn x => f(f x) *)   
     LAM("f",LAM("x",APP(VAR "f", APP(VAR "f", VAR "x"))));


  fun run(e: exp, outfilename: string) =
  let
    val _ = r:= 5;   (* reset counter for type variables *)
    val _  = print("\nprocessing " ^ outfilename ^ "...")
    val TE0 = 
         [("fst", (ARROW(PROD(TYVAR 0,TYVAR 1),TYVAR 0))),  (* the type of fst *)
          ("snd", (ARROW(PROD(TYVAR 0,TYVAR 1),TYVAR 1))),  (* the type of snd *)
          ("floor", (ARROW(REAL,INT)))]        (* the type of floor *)
    val _  = print("running TYPE...")
    val (ty,C) = TYPE(TE0,e) ;
    val os = TextIO.openOut outfilename;
    val _ = TextIO.output(os, "\nResult of TYPE:\n\n\n " ^ pp_judgement(C,TE0,e,ty)); 
    val _  = print("running MATCH...")
    val S = match C
    val judgem' as (C',TE',e',ty') = (on_C(S,C),on_tyenv(S,TE0),e,S ty)
    val _ = TextIO.output(os, "\n\n\nResult of MATCH:\n\n\n " ^ pp_judgement judgem'); 
  in
    TextIO.closeOut os
  end;
  
  val _ = run(e0,"outFuhMishra0");
  val _ = run(e1,"outFuhMishra1");
  val _ = run(e2,"outFuhMishra2");
  val _ = run(e3,"outFuhMishra3");
  val _ = run(e4,"outFuhMishra4");
  val _ = run(e5,"outFuhMishra5");

(*
end (* local *)
end (*struct*);
*)

val _ = print "\n"
