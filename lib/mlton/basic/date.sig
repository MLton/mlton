(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature DATE_STRUCTS = 
   sig
   end

signature DATE = 
   sig
      type t

      structure Weekday:
         sig
            datatype t = Mon | Tue | Wed | Thu | Fri | Sat | Sun
         end

      structure Month:
         sig
            datatype t =
               Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec

            val toInt: t -> int
         end

      exception Date
      val compare: t * t -> order
      val date: {year: int,
                 month: Month.t,
                 day: int,
                 hour: int,
                 minute: int,
                 second: int,
                 offset: Time.t option} -> t 
      val day: t -> int 
      val fmt: t * string -> string 
      val fromString: string -> t option 
      val fromTimeLocal: Time.t -> t 
      val fromTimeUniv: Time.t -> t 
      val hour: t -> int
      val isDst: t -> bool option 
      val layout: t -> Layout.t
      val localOffset: unit -> Time.t
      val minute: t -> int 
      val month: t -> Month.t 
      val now: unit -> t
      val offset: t -> Time.t option 
      val scan: 'a * (char, 'a) StringCvt.reader -> (t * 'a) option
      val second: t -> int 
      val toString: t -> string 
      val toTime: t -> Time.t 
      val weekDay: t -> Weekday.t 
      val year: t -> int 
      val yearDay: t -> int 
   end
