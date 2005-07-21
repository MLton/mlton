(* ML-Yacc Parser Generator (c) 1991 Andrew W. Appel, David R. Tarditi *)

structure Absyn : ABSYN =
  struct
    datatype exp
      = CODE of string
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
               let fun f(CODE s) = code_to_ids s
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

       fun printRule (say : string -> unit, sayln:string -> unit) r = let
	   fun flat (a, []) = rev a
	     | flat (a, SEQ (e1, e2) :: el) = flat (a, e1 :: e2 :: el)
	     | flat (a, e :: el) = flat (e :: a, el)
	   fun pl (lb, rb, c, f, [], a) = " " :: lb :: rb :: a
	     | pl (lb, rb, c, f, h :: t, a) =
	         " " :: lb :: f (h, foldr (fn (x, a) => c :: f (x, a))
					  (rb :: a)
					  t)
	   fun pe (CODE c, a) = " (" :: c :: ")" :: a
	     | pe (EAPP (x, y as (EAPP _)), a) =
	         pe (x, " (" :: pe (y, ")" :: a))
	     | pe (EAPP (x, y), a) =
	         pe (x, pe (y, a))
	     | pe (EINT i, a) =
	         " " :: Int.toString i :: a
	     | pe (ETUPLE l, a) = pl ("(", ")", ",", pe, l, a)
	     | pe (EVAR v, a) =
	         " " :: v :: a
	     | pe (FN (p, b), a) =
	         " (fn" :: pp (p, " =>" :: pe (b, ")" :: a))
	     | pe (LET ([], b), a) =
	         pe (b, a)
	     | pe (LET (dl, b), a) =
	       let fun pr (VB (p, e), a) =
		       " val " :: pp (p, " =" :: pe (e, "\n" :: a))
	       in " let" :: foldr pr (" in" :: pe (b, "\nend" :: a)) dl
	       end
	     | pe (SEQ (e1, e2), a) =
	         pl ("(", ")", ";", pe, flat ([], [e1, e2]), a)
	     | pe (UNIT, a) =
	         " ()" :: a
	   and pp (PVAR v, a) =
	         " " :: v :: a
	     | pp (PAPP (x, y as PAPP _), a) =
	         " " :: x :: " (" :: pp (y, ")" :: a)
	     | pp (PAPP (x, y), a) =
	         " " :: x :: pp (y, a)
	     | pp (PINT i, a) =
	         " " :: Int.toString i :: a
	     | pp (PLIST (l, NONE), a) =
	         pl ("[", "]", ",", pp, l, a)
	     | pp (PLIST (l, SOME t), a) =
	         " (" :: foldr (fn (x, a) => pp (x, " ::" :: a))
			       (pp (t, ")" :: a))
			       l
	     | pp (PTUPLE l, a) =
	         pl ("(", ")", ",", pp, l, a)
	     | pp (WILD, a) =
	         " _" :: a
	     | pp (AS (v, PVAR v'), a) =
	         " (" :: v :: " as " :: v' :: ")" :: a
	     | pp (AS (v, p), a) =
	         " (" :: v :: " as (" :: pp (p, "))" :: a)
	   fun out "\n" = sayln ""
	     | out s = say s
       in
	   case simplifyRule r of
	       RULE (p, e) => app out (pp (p, " =>" :: pe (e, ["\n"])))
       end
end;
