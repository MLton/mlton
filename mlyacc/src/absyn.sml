(* ML-Yacc Parser Generator (c) 1991 Andrew W. Appel, David R. Tarditi *)

structure Absyn : ABSYN =
  struct
    datatype exp
      = CODE of {text : string, pos : Header.pos}
      | EAPP of exp * exp
      | EINT of int
      | ETUPLE of exp list
      | EVAR of string
      | FN of pat * exp
      | LET of decl list * exp
      | SEQ of exp * exp
      | UNIT
    and pat
      = PVAR of string
      | PAPP of string * pat
      | PINT of int
      | PLIST of pat list * pat option
      | PTUPLE of pat list
      | WILD
      | AS of string * pat
    and decl = VB of pat * exp
    and rule = RULE of pat * exp

    fun idchar #"'" = true
      | idchar #"_" = true
      | idchar c = Char.isAlpha c orelse Char.isDigit c

    fun code_to_ids s = let
          fun g(nil,r) = r
            | g(a as (h::t),r) = if Char.isAlpha h then f(t,[h],r) else g(t,r)
          and f(nil,accum,r)= implode(rev accum)::r
            | f(a as (h::t),accum,r) =
                if idchar h then f(t,h::accum,r) else g(a,implode (rev accum) :: r)
          in g(explode s,nil)
          end

         val simplifyRule : rule -> rule = fn (RULE(p,e)) =>
            let val used : (string -> bool) =
               let fun f(CODE s) = code_to_ids (#text s)
                     | f(EAPP(a,b)) = f a @ f b
                     | f(ETUPLE l) = List.concat (map f l)
                     | f(EVAR s) = [s]
                     | f(FN(_,e)) = f e
                     | f(LET(dl,e)) =
                          (List.concat (map (fn VB(_,e) => f e) dl)) @ f e
                     | f(SEQ(a,b)) = f a @ f b
                     | f _ = nil
                   val identifiers = f e
               in fn s => List.exists (fn a=>a=s) identifiers
               end
              val simplifyPat : pat -> pat =
                let fun f a =
                    case a
                    of (PVAR s) => if used s then a else WILD
                     | (PAPP(s,pat)) =>
                         (case f pat
                          of WILD => WILD
                           | pat' => PAPP(s,pat'))
                     | (PLIST (l, topt)) =>
                         let val l' = map f l
                             val topt' = Option.map f topt
                             fun notWild WILD = false
                               | notWild _ = true
                         in case topt' of
                                SOME WILD => if List.exists notWild l' then
                                                 PLIST (l', topt')
                                             else WILD
                              | _ => PLIST (l', topt')
                         end
                     | (PTUPLE l) =>
                          let val l' = map f l
                          in if List.exists(fn WILD=>false | _ => true) l'
                             then PTUPLE l' 
                             else WILD
                          end
                     | (AS(a,b)) =>
                         if used a then
                             case f b of
                                 WILD => PVAR a
                               | b' => AS(a,b')
                         else f b
                     | _ => a
               in f
               end
           val simplifyExp : exp -> exp =
               let fun f(EAPP(a,b)) = EAPP(f a,f b)
                     | f(ETUPLE l) = ETUPLE(map f l)
                     | f(FN(p,e)) = FN(simplifyPat p,f e) 
                     | f(LET(dl,e)) = 
                          LET(map (fn VB(p,e) =>
                                  VB(simplifyPat p,f e)) dl,
                              f e)
                     | f(SEQ(a,b)) = SEQ(f a,f b)
                     | f a = a
               in f
               end
       in RULE(simplifyPat p,simplifyExp e)
       end

       fun printRule (S : string -> unit, sayPos) r = let
           fun flat (a, []) = rev a
             | flat (a, SEQ (e1, e2) :: el) = flat (a, e1 :: e2 :: el)
             | flat (a, e :: el) = flat (e :: a, el)
           fun pl (lb, rb, c, f, []) = (S" "; S lb; S rb)
             | pl (lb, rb, c, f, h :: t) =
               (S" "; S lb; f h; app (fn x => (S c ; f x)) t; S rb)
           fun pe (CODE {text, pos}) =
               (S" ("; sayPos (SOME pos); S text; sayPos NONE; S")")
             | pe (EAPP (x, y as (EAPP _))) = (pe x; S" ("; pe y; S")")
             | pe (EAPP (x, y)) = (pe x; pe y)
             | pe (EINT i) = (S" "; S (Int.toString i))
             | pe (ETUPLE l) = pl ("(", ")", ",", pe, l)
             | pe (EVAR v) = (S" "; S v)
             | pe (FN (p, b)) = (S" (fn"; pp p; S" =>"; pe b; S")")
             | pe (LET ([], b)) = pe b
             | pe (LET (dl, b)) =
               let fun pr (VB (p, e)) = (S"\n"; S"   val "; pp p; S" ="; pe e)
               in
                  S" let"; app pr dl ; S"\n"; S" in"; pe b; S"\n"; S" end"
               end
             | pe (SEQ (e1, e2)) = pl ("(", ")", ";", pe, flat ([], [e1, e2]))
             | pe (UNIT) = S" ()"
           and pp (PVAR v) = (S" "; S v)
             | pp (PAPP (x, y as PAPP _)) = (S" "; S x; S" ("; pp y; S")")
             | pp (PAPP (x, y)) = (S" "; S x; pp y)
             | pp (PINT i) = (S" "; S (Int.toString i))
             | pp (PLIST (l, NONE)) = (pl ("[", "]", ",", pp, l))
             | pp (PLIST (l, SOME t)) =
               (S" ("; app (fn x => (pp x; S" ::")) l; pp t; S")")
             | pp (PTUPLE l) = pl ("(", ")", ",", pp, l)
             | pp (WILD) = S" _"
             | pp (AS (v, PVAR v')) = (S" ("; S v; S" as "; S v'; S")")
             | pp (AS (v, p)) = (S" ("; S v; S" as ("; pp p; S"))")
       in
           case simplifyRule r of
               RULE (p, e) => (pp p; S" =>"; pe e; S"\n")
       end
end;
