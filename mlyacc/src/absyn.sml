(* ML-Yacc Parser Generator (c) 1991 Andrew W. Appel, David R. Tarditi 
 *
 * $Log: absyn.sml,v $
 * Revision 1.1.1.1  1997/01/14 01:38:05  george
 *   Version 109.24
 *
 * Revision 1.3  1996/02/26  15:02:30  george
 *    print no longer overloaded.
 *    use of makestring has been removed and replaced with Int.toString ..
 *    use of IO replaced with TextIO
 *
 * Revision 1.2  1996/02/15  01:51:38  jhr
 * Replaced character predicates (isalpha, isnum) with functions from Char.
 *
 * Revision 1.1.1.1  1996/01/31  16:01:44  george
 * Version 109
 * 
 *)

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
      | PLIST of pat list
      | PTUPLE of pat list
      | WILD
      | AS of pat * pat
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
                     | (PLIST l) =>
	                  let val l' = map f l
                          in if List.exists(fn WILD=>false | _ => true) l'
                                then PLIST l'
                             else WILD
                          end
                     | (PTUPLE l) =>
                          let val l' = map f l
                          in if List.exists(fn WILD=>false | _ => true) l'
                             then PTUPLE l' 
                             else WILD
                          end
                     | (AS(a,b)) =>
                         let val a'=f a
                             val b'=f b
                         in case(a',b')
                            of (WILD,_) => b'
                             | (_,WILD) => a'
                             | _ => AS(a',b')
                         end
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

       fun printRule (say : string -> unit, sayln:string -> unit) = let
	 val lp = ["("]
         val rp = [")"]
         val sp = [" "]
         val sm = [";"]
         val cm = [","]
         val cr = ["\n"]
         val unit = ["()"]
          fun printExp c =
	   let fun f (CODE c) = ["(",c,")"]
                 | f (EAPP(EVAR a,UNIT)) = [a," ","()"]
                 | f (EAPP(EVAR a,EINT i)) =  [a," ",Int.toString i]
                 | f (EAPP(EVAR a,EVAR b)) = [a," ",b]
                 | f (EAPP(EVAR a,b)) = List.concat[[a],lp,f b,rp]
                 | f (EAPP(a,b)) = List.concat [lp,f a,rp,lp,f b,rp]
	         | f (EINT i) = [Int.toString i]
                 | f (ETUPLE (a::r)) = 
	              let fun scan nil = [rp]
                            | scan (h :: t) = cm :: f h :: scan t
                      in List.concat (lp :: f a :: scan r)
                      end
                 | f (ETUPLE _) = ["<bogus-tuple>"]
                 | f (EVAR s) = [s]
                 | f (FN (p,b)) = List.concat[["fn "],printPat p,[" => "],f b]
                 | f (LET (nil,body)) = f body
                 | f (LET (dl,body)) =
	              let fun scan nil = [[" in "],f body,[" end"],cr]
                            | scan (h :: t) = printDecl h :: scan t
	              in List.concat(["let "] :: scan dl)
	              end
                 | f (SEQ (a,b)) = List.concat [lp,f a,sm,f b,rp]
                 | f (UNIT) = unit
          in f c
          end
         and printDecl (VB (pat,exp)) =
                  List.concat[["val "],printPat pat,["="],printExp exp,cr]
         and printPat c =
	   let fun f (AS(PVAR a,PVAR b)) = [a," as ",b]
                 | f (AS(a,b)) = List.concat [lp,f a,[") as ("],f b,rp]
                 | f (PAPP(a,WILD)) = [a," ","_"]
                 | f (PAPP(a,PINT i)) =  [a," ",Int.toString i]
                 | f (PAPP(a,PVAR b)) = [a," ",b]
                 | f (PAPP(a,b)) = List.concat [lp,[a],sp,f b,rp]
	         | f (PINT i) = [Int.toString i]
                 | f (PLIST nil) = ["<bogus-list>"]
                 | f (PLIST l) =
	              let fun scan (h :: nil) = [f h]
                            | scan (h :: t) = f h :: ["::"] :: scan t
			    | scan _ = raise Fail "scan"
                      in List.concat (scan l)
                      end
                 | f (PTUPLE (a::r)) = 
	              let fun scan nil = [rp]
                            | scan (h :: t) = cm :: f h :: scan t
                      in List.concat (lp :: f a :: scan r)
                      end
                 | f (PTUPLE nil) = ["<bogus-pattern-tuple>"]
                 | f (PVAR a) = [a]
                 | f WILD = ["_"]
           in f c
           end
	   fun oursay "\n" = sayln ""
	     | oursay a = say a
         in fn a => 
	      let val RULE(p,e) = simplifyRule a
              in app oursay (printPat p);
	         say " => ";
                 app oursay (printExp e)
              end
         end
end;
