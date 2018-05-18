(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature TIME = 
   sig
      exception Time

      type int = Pervasive.LargeInt.int

      include ORDER

      type times =
          {
            self:     {utime: t, (* user time of process *)
                       stime: t  (* system time of process *)
                       },
            children: {utime: t, (* user time of terminated child processes *)
                       stime: t  (* system time of terminated child processes *)
                       },
            gc:       {utime: t, (* user time of gc *)
                       stime: t  (* system time of gc *)
                       }
           }

      val + : t * t -> t
      val - : t * t -> t
      val days: int -> t
      val fromString: string -> t option
      val fromMicroseconds: int -> t
      val fromMilliseconds: int -> t
      val fromReal: real -> t
      val hours: int -> t
      val minutes: int -> t
      val now: unit -> t
      val output: t * Out.t -> unit
      val seconds: int -> t
      val times: unit -> times 
      val timeThunk: (unit -> unit) -> t
      val toMicroseconds: t -> int
      val toMilliseconds: t -> int
      val toReal: t -> real
      val toSeconds: t -> int
      val toString: t -> string
      val weeks:int -> t
      val years: int -> t
      val zero: t
  end
