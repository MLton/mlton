type int = Int.t

signature LINES =
   sig
      (* Print out starting at line start and dropping the last lines. *)
      val dropLast: In.t * Out.t * {start: int, last: int} -> unit
      (* Print out lines start through stop. *)
      val startStop: In.t * Out.t * {start: int, stop: int} -> unit
   end

