(* Modified from the ML Kit 4.1.4; basislib/Date.sml
 * by mfluet@acm.org on 2006-4-25
 * by mfluet@acm.org on 2005-8-10 based on
 *  modifications from the ML Kit Version 3; basislib/Date.sml
 *  by sweeks@research.nj.nec.com on 1999-1-3 and
 *  by sweeks@acm.org on 2000-1-18.
 *)

(* Date -- 1995-07-03, 1998-04-07 *)

structure Date :> DATE =
  struct
     structure Prim = PrimitiveFFI.Date
     structure Tm = Prim.Tm

     (* Patch to make Time look like it deals with Int.int
      * instead of LargeInt.int.
      *)
     structure Time =
        struct
           open Time
           val toSeconds = LargeInt.toInt o toSeconds
           val fromSeconds = fromSeconds o LargeInt.fromInt
        end

    datatype weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun

    datatype month = Jan | Feb | Mar | Apr | May | Jun
                   | Jul | Aug | Sep | Oct | Nov | Dec

    datatype t =
       T of {day: int, (* 1-31 *)
             hour: int, (* 0-23 *)
             isDst: bool option, (* daylight savings time in force *)
             minute: int, (* 0-59 *)
             month: month,
             offset: int option, (* signed seconds East of UTC:
                                  * this zone = UTC+t; ~82800 < t <= 82800 *)
             second: int, (* 0-61 (allowing for leap seconds) *)
             weekDay: weekday,
             year: int, (* e.g. 1995 *)
             yearDay: int} (* 0-365 *)
    type date = t

    local
       fun make f (T r) = f r
    in
       val day = make #day
       val hour = make #hour
       val isDst = make #isDst
       val minute = make #minute
       val month = make #month
       val second = make #second
       val weekDay = make #weekDay
       val year = make #year
       val yearDay = make #yearDay
    end

    exception Date

    (* 86400 = 24*60*6 is the number of seconds per day *)

    type tmoz = {tm_hour  : C_Int.t,
                 tm_isdst : C_Int.t,       (* 0 = no, 1 = yes, ~1 = don't know *)
                 tm_mday  : C_Int.t,
                 tm_min   : C_Int.t,
                 tm_mon   : C_Int.t,
                 tm_sec   : C_Int.t, 
                 tm_wday  : C_Int.t,
                 tm_yday  : C_Int.t,
                 tm_year  : C_Int.t}
    local
       fun make (f: C_Time.t ref -> C_Int.t C_Errno.t) (n: C_Time.t) : tmoz =
          (ignore (f (ref n))
           ; {tm_hour = Tm.getHour (),
              tm_isdst = Tm.getIsDst (),
              tm_mday = Tm.getMDay (),
              tm_min = Tm.getMin (),
              tm_mon = Tm.getMon (),
              tm_sec = Tm.getSec (),
              tm_wday = Tm.getWDay (),
              tm_yday = Tm.getYDay (),
              tm_year = Tm.getYear ()})
    in
       val getlocaltime_ = make Prim.localTime
       val getgmtime_ = make Prim.gmTime
    end

    fun setTmBuf ({tm_hour, tm_isdst, tm_mday, tm_min, tm_mon, 
                   tm_sec, tm_wday, tm_yday, tm_year}: tmoz) : unit =
       (Tm.setHour tm_hour
        ; Tm.setIsDst tm_isdst
        ; Tm.setMDay tm_mday
        ; Tm.setMin tm_min
        ; Tm.setMon tm_mon
        ; Tm.setSec tm_sec
        ; Tm.setWDay tm_wday
        ; Tm.setYDay tm_yday
        ; Tm.setYear tm_year)

    fun mktime_ (t: tmoz): C_Time.t = C_Errno.check (setTmBuf t; Prim.mkTime ())

    (* The offset to add to local time to get UTC: positive West of UTC *)
    val localoffset: int = C_Double.round (Prim.localOffset ())

    val toweekday: int -> weekday =
       fn 0 => Sun | 1 => Mon | 2 => Tue | 3 => Wed
        | 4 => Thu | 5 => Fri | 6 => Sat 
        | _ => raise Fail "Internal error: Date.toweekday"

    val fromwday: weekday -> int =
       fn Sun => 0 | Mon => 1 | Tue => 2 | Wed => 3 
        | Thu => 4 | Fri => 5 | Sat => 6

    val tomonth: int -> month =
       fn 0 => Jan | 1 => Feb |  2 => Mar |  3 => Apr
        | 4 => May | 5 => Jun |  6 => Jul |  7 => Aug
        | 8 => Sep | 9 => Oct | 10 => Nov | 11 => Dec
        | _ => raise Fail "Internal error: Date.tomonth"

    val frommonth: month -> int =
       fn Jan => 0 | Feb => 1 | Mar => 2  | Apr => 3
        | May => 4 | Jun => 5 | Jul => 6  | Aug => 7
        | Sep => 8 | Oct => 9 | Nov => 10 | Dec => 11

    fun tmozToDate ({tm_hour, tm_isdst, tm_mday, tm_min, tm_mon, 
                     tm_sec, tm_wday, tm_yday, tm_year}: tmoz) offset = 
       T {day = C_Int.toInt tm_mday,
          hour = C_Int.toInt tm_hour,
          isDst = (case tm_isdst of
                      0 => SOME false 
                    | 1 => SOME true
                    | _ => NONE),
          minute = C_Int.toInt tm_min, 
          month = tomonth (C_Int.toInt tm_mon), 
          offset = offset,
          second = C_Int.toInt tm_sec,
          weekDay = toweekday (C_Int.toInt tm_wday),
          yearDay = C_Int.toInt tm_yday,
          year = (C_Int.toInt tm_year) + 1900}

    fun leapyear (y: int) =
       y mod 4 = 0 andalso y mod 100 <> 0 orelse y mod 400 = 0   

    fun monthdays year month : int = 
      case month of
         Jan => 31
       | Feb => if leapyear year then 29 else 28
       | Mar => 31
       | Apr => 30
       | May => 31
       | Jun => 30
       | Jul => 31
       | Aug => 31
       | Sep => 30
       | Oct => 31
       | Nov => 30
       | Dec => 31

    (* Check whether date may be passed to ISO/ANSI C functions: *)

    fun okDate (T {year, month, day, hour, minute, second, ...}) =
        1 <= day    andalso day    <= monthdays year month
        andalso 0 <= hour   andalso hour   <= 23
        andalso 0 <= minute andalso minute <= 59
        andalso 0 <= second andalso second <= 61 (* leap seconds *)

    fun dateToTmoz (dt as T {year, month, day, hour, minute, second,
                             weekDay, yearDay, isDst, ...}): tmoz =
        if not (okDate dt)
           then raise Date
        else {tm_hour = C_Int.fromInt hour,
              tm_isdst = (case isDst of
                             SOME false => 0
                           | SOME true => 1
                           | NONE=> ~1),
              tm_mday = C_Int.fromInt day,
              tm_min = C_Int.fromInt minute, 
              tm_mon = C_Int.fromInt (frommonth month),
              tm_sec = C_Int.fromInt second, 
              tm_wday = C_Int.fromInt (fromwday weekDay),
              tm_yday = C_Int.fromInt yearDay,
              tm_year = C_Int.fromInt (year - 1900)}

    (* -------------------------------------------------- *)
    (* Translated from Emacs's calendar.el:               *)

    (* Reingold: Number of the day within the year: *)

    fun dayinyear (year: int, month: month, day: int): int = 
        let val monthno = frommonth month
        in
            day - 1 + 31 * monthno
            - (if monthno > 1 then 
                   (27 + 4 * monthno) div 10 - (if leapyear year then 1 else 0)
               else 0)
        end

    (* Reingold: Find the number of days elapsed from the (imagined)
       Gregorian date Sunday, December 31, 1 BC to the given date. *)

    fun todaynumber year month day = 
        let val prioryears = year - 1
        in
            dayinyear (year, month, day)
          + 1
          + 365 * prioryears
          + prioryears div 4
          - prioryears div 100
          + prioryears div 400
        end

    (* Reingold et al: from absolute day number to year, month, date: *)

    fun fromdaynumber n = 
        let val d0 = n - 1
            val n400 = d0 div 146097
            val d1 = d0 mod 146097
            val n100 = d1 div 36524
            val d2 = d1 mod 36524
            val n4 = d2 div 1461
            val d3 = d2 mod 1461
            val n1 = d3 div 365
            val day = 1 + d3 mod 365
            val year = 400 * n400 + 100 * n100 + n4 * 4 + n1 + 1
            fun loop month day =
                let val mdays = monthdays year (tomonth month)
                in 
                    if mdays < day then loop (month+1) (day-mdays)
                    else (year, tomonth month, day)
                end
        in 
            if n100 = 4 orelse n1 = 4 then 
                (year-1, Dec, 31)
            else
                loop 0 day 
        end         

    (* -------------------------------------------------- *)

    fun weekday daynumber = toweekday (daynumber mod 7)

    (* Normalize a date, disregarding leap seconds: *)

    fun normalizedate yr0 mo0 dy0 hr0 mn0 sec0 offset =
        let val mn1    = mn0 + sec0 div 60
            val second = sec0 mod 60
            val hr1    = hr0 + mn1 div 60
            val minute = mn1 mod 60
            val dayno  = todaynumber yr0 mo0 dy0 + hr1 div 24
            val hour   = hr1 mod 24
            val (year, month, day) = fromdaynumber dayno
            val date1 = T {day = day,
                           hour = hour,
                           isDst = (case offset of 
                                       NONE   => NONE 
                                     | SOME _ => SOME false),
                           minute = minute,
                           month = month,
                           offset = offset,
                           second = second,
                           weekDay = weekday dayno,
                           year = year,
                           yearDay = dayinyear (year, month, day)}
        in 
            (* One cannot reliably compute DST in non-local timezones,
            not even given the offset from UTC.  Countries in the
            Northern hemisphere have DST during Mar-Oct, those around
            Equator do not have DST, and those in the Southern
            hemisphere have DST during Oct-Mar. *)
            if year < 1970 orelse year > 2037 then date1
            else 
                case offset of 
                    NONE   => 
                        tmozToDate (getlocaltime_ (mktime_ (dateToTmoz date1)))
                        offset
                  | SOME _ => date1
        end

    fun fromTimeLocal t = 
        tmozToDate (getlocaltime_ (C_Time.fromInt (Time.toSeconds t))) NONE

    fun fromTimeUniv t = 
        tmozToDate (getgmtime_ (C_Time.fromInt (Time.toSeconds t))) (SOME 0)

    (* The following implements conversion from a local date to 
     * a Time.time.  It IGNORES wday and yday.
     *)

    fun toTime (date as T {offset, ...}) = 
        let
           val secoffset = 
              case offset of
                 NONE      => 0
               | SOME secs => localoffset + secs
            val clock = C_Time.toInt (mktime_ (dateToTmoz date)) - secoffset
        in
            if clock < 0 then raise Date
            else Time.fromSeconds clock
        end

    fun localOffset () = Time.fromSeconds (localoffset mod 86400)

    local
       val isFormatChar =
          let
             val a = Array.tabulate (Char.maxOrd + 1, fn _ => false)
             val validChars = "aAbBcdHIjmMpSUwWxXyYZ%"
          in Natural.foreach
             (size validChars, fn i =>
              Array.update (a, Char.ord (String.sub (validChars, i)), true));
             fn c => Array.sub (a, Char.ord c)
          end
    in     
       fun fmt fmtStr d =
          let
             val _ = setTmBuf (dateToTmoz d)
             val bufLen = 50 (* more than enough for a single format char *)
             val buf = Array.arrayUninit bufLen
             fun strftime fmtChar =
                let
                   val len =
                      Prim.strfTime
                      (buf, C_Size.fromInt bufLen,
                       NullString.fromString (concat ["%", str fmtChar, "\000"]))
                   val len = C_Size.toInt len
                in if len = 0
                      then raise Fail "Date.fmt"
                   else ArraySlice.vector (ArraySlice.slice (buf, 0, SOME len))
                end
             val max = size fmtStr
             fun loop (i, start, accum) =
                let
                   fun newAccum () =
                      let val len = i - start
                      in
                         if len = 0
                            then accum
                         else String.extract (fmtStr, start, SOME len) :: accum
                      end
                in
                   if i >= max
                      then newAccum ()
                   else
                      if #"%" = String.sub (fmtStr, i)
                         then
                            let
                               val i = i + 1
                            in
                               if i >= max
                                  then newAccum ()
                               else let
                                       val c = String.sub (fmtStr, i)
                                    in
                                       if isFormatChar c
                                          then loop (i + 1, i + 1,
                                                     strftime c :: newAccum ())
                                       else loop (i, i, newAccum ())
                                    end
                            end
                      else loop (i + 1, start, accum)
                end
          in concat (rev (loop (0, 0, [])))
          end
    end

    val toString = fmt "%a %b %d %H:%M:%S %Y"

    type ('a, 'b) reader = ('a, 'b) Reader.reader

    fun scan (reader: (char, 'a) reader): (t, 'a) reader =
       let
          type 'b t = ('b, 'a) reader
          val none: 'b t = fn _ => NONE
          fun done (b: 'b): 'b t = fn a => SOME (b, a)
          fun peek1 (f: char -> 'b t): 'b t =
             fn a =>
             case reader a of
                NONE => NONE
              | SOME (c, _) => f c a
          fun read1 (f: char -> 'b t): 'b t =
             fn a =>
             case reader a of
                NONE => NONE
              | SOME (c, a) => f c a
          fun skipSpace (r: 'b t): 'b t =
             let
                fun loop (): 'b t =
                   peek1 (fn c =>
                          if Char.isSpace c
                             then read1 (fn _ => loop ())
                          else r)
             in
                loop ()
             end
          fun readN (n: int, f: string -> 'b t): 'b t =
             let
                fun loop (n: int, ac: char list): 'b t =
                   if 0 = n
                      then f (implode (rev ac))
                   else read1 (fn c => loop (n - 1, c :: ac))
             in
                loop (n, [])
             end
          fun readChar (c: char, r: 'b t): 'b t =
             read1 (fn c' => if c = c' then r else none)
          fun readWeekDay (f: weekday -> 'b t): 'b t =
             readN (3, fn s =>
                    case s of
                       "Mon" => f Mon
                     | "Tue" => f Tue
                     | "Wed" => f Wed
                     | "Thu" => f Thu
                     | "Fri" => f Fri
                     | "Sat" => f Sat
                     | "Sun" => f Sun
                     | _ => none)
          fun readMonth (f: month -> 'b t): 'b t =
             readN (3, fn s =>
                    case s of
                       "Jan" => f Jan
                     | "Feb" => f Feb
                     | "Mar" => f Mar
                     | "Apr" => f Apr
                     | "May" => f May
                     | "Jun" => f Jun
                     | "Jul" => f Jul
                     | "Aug" => f Aug
                     | "Sep" => f Sep
                     | "Oct" => f Oct
                     | "Nov" => f Nov
                     | "Dec" => f Dec
                     | _ => none)
          fun readDigs (n: int, lower: int, upper: int, f: int -> 'b t): 'b t =
             readN (n, fn s =>
                    if not (CharVector.all Char.isDigit s)
                       then none
                    else
                       let
                          val v =
                             CharVector.foldl
                             (fn (c, ac) =>
                              ac * 10 + (Char.ord c - Char.ord #"0"))
                             0 s
                       in
                          if lower <= v andalso v <= upper
                             then f v
                          else none
                       end)
          fun readDay f =
             peek1 (fn c =>
                    if c = #" "
                       then read1 (fn _ => readDigs (1, 1, 9, f))
                    else readDigs (2, 1, 31, f))
          fun readHour f = readDigs (2, 0, 23, f)
          fun readMinute f = readDigs (2, 0, 59, f)
          fun readSeconds f = readDigs (2, 0, 61, f)
          fun readYear f = readDigs (4, 0, 9999, f)
       in
          skipSpace
          (readWeekDay
           (fn weekDay =>
            readChar
            (#" ",
             readMonth
             (fn month =>
              readChar
              (#" ",
               readDay
               (fn day =>
                readChar
                (#" ",
                 readHour
                 (fn hour =>
                  readChar
                  (#":",
                   readMinute
                   (fn minute =>
                    (readChar
                     (#":",
                      readSeconds
                      (fn second =>
                       readChar
                       (#" ",
                        readYear
                        (fn year =>
                         done (T {day = day,
                                  hour = hour,
                                  isDst = NONE,
                                  minute = minute,
                                  month = month,
                                  offset = NONE,
                                  second = second,
                                  weekDay = weekDay,
                                  year = year,
                                  yearDay = dayinyear (year, month, day)}
                               ))))))))))))))))
       end

    fun fromString s = StringCvt.scanString scan s

    (* Ignore timezone and DST when comparing dates: *)
    fun compare 
        (T {year=y1,month=mo1,day=d1,hour=h1,minute=mi1,second=s1, ...},
         T {year=y2,month=mo2,day=d2,hour=h2,minute=mi2,second=s2, ...}) =
        let
           fun cmp (v1, v2, cmpnext) = 
              case Int.compare (v1, v2) of
                 EQUAL => cmpnext ()
               | r => r
        in 
            cmp (y1, y2, 
            fn _ => cmp (frommonth mo1, frommonth mo2, 
            fn _ => cmp (d1, d2,                
            fn _ => cmp (h1, h2,
            fn _ => cmp (mi1, mi2,
            fn _ => cmp (s1, s2,
            fn _ => EQUAL))))))
        end

    fun date { year, month, day, hour, minute, second, offset } =
       if year < 0 then raise Date 
       else
          let
             val (dayoffset, offset') = 
                case offset of
                   NONE => (0, NONE)
                 | SOME time => 
                      let
                         val secs = Time.toSeconds time
                         val secoffset = 
                            if secs <= 82800 then ~secs else 86400 - secs
                      in
                         (Int.quot (secs, 86400), SOME secoffset)
                      end
             val day' = day + dayoffset
          in
             normalizedate year month day' hour minute second offset'
          end

    fun offset (T {offset, ...}) = 
        Option.map
        (fn secs => Time.fromSeconds ((86400 + secs) mod 86400)) 
        offset
  end
