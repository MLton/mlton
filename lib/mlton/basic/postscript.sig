type int = Int.t
   
signature POSTSCRIPT =
   sig
      val makeHeader: {host: string, job: string, user: string} -> string
   end
