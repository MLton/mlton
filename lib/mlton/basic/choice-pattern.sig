type int = Int.t
   
signature CHOICE_PATTERN =
   sig
      (* expand "ab{c{d,e},f{gh}}{i,j}" =
       * ["abcdi", "abcdj", "abcei", "abcej", "abfghi", "abfghj"]
       *)
      val expand: string -> string list Result.t
   end
