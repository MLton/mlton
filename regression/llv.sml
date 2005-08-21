
structure LLV =
  struct

    datatype place = P of int

  datatype info = Info1 | Info2 | Info3

  datatype 'a tr = TR of 'a exp * info | K of 'a -> int
       and 'a exp = SWITCH_I of ('a, int) switch
                  | SWITCH_S of ('a, string) switch
                  | STRING of string * 'a
       and ('a,'c) switch = SWITCH of 'a tr * ('c * 'a tr) list

datatype 'a pgm = PGM of string * 'a tr 

  type mulexp = place exp
   and multrip = place tr
  type mulexp_llv = (place*int) exp
   and trip_llv = (place*int) tr

    fun llv(tr: multrip as TR(e,Info1)) : trip_llv =
      let
          val e' = llvExp e
      in
          TR(e',Info2)
      end
  
    and llvExp(e: mulexp) : mulexp_llv =
      let 
        fun llv_switch(SWITCH(e,branches)) =
          (* Note: e is trivial *)
          let val branches' = map (fn (c,e) => (c,llv e)) branches
          in  
               SWITCH(llv e, branches')
          end
      in
      case e of
       SWITCH_I(switch) => 
          let val switch' = llv_switch switch
          in SWITCH_I(switch')
          end 
      | SWITCH_S(switch) => 
          let val switch' = llv_switch switch
          in SWITCH_S(switch')
          end 
      | STRING(s,place) => STRING(s, (place, 5))
      end 



    val  llv = fn (PGM (label,expression)) =>
      let
         val tr' = llv expression
      in
         PGM(label, tr')
      end

end


