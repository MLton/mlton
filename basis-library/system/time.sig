signature TIME =
   sig
      eqtype time
      exception Time

      val + : time * time -> time 
      val - : time * time -> time 
      val < : time * time -> bool 
      val <= : time * time -> bool 
      val > : time * time -> bool 
      val >= : time * time -> bool 
      val compare: time * time -> order 
      val fmt: int -> time -> string 
      val fromMicroseconds: LargeInt.int -> time 
      val fromMilliseconds: LargeInt.int -> time
      val fromNanoseconds: LargeInt.int -> time
      val fromReal: LargeReal.real -> time 
      val fromSeconds: LargeInt.int -> time 
      val fromString: string -> time option 
      val now: unit -> time 
      val scan: (char, 'a) StringCvt.reader -> (time, 'a) StringCvt.reader
      val toMicroseconds: time -> LargeInt.int 
      val toMilliseconds: time -> LargeInt.int
      val toNanoseconds: time -> LargeInt.int
      val toReal: time -> LargeReal.real 
      val toSeconds: time -> LargeInt.int 
      val toString: time -> string 
      val zeroTime: time 
   end

signature TIME_EXTRA =
   sig
      include TIME

      val fromTicks: LargeInt.int -> time
      val ticksPerSecond: LargeInt.int
   end
