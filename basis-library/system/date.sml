(* Modified from the ML Kit Version 3 by sweeks@research.nj.nec.com on 1999-1-3.
 * Further modifications by sweeks@acm.org on 2000-1-18.
 *)
 
(* Date -- 1995-07-03, 1998-04-07 *)

structure Date :> DATE =
  struct
     structure Prim = Primitive.Date
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

    datatype date = DATE of {
	year   : int,			(* e.g. 1995 *)
	month  : month,
	day    : int,       		(* 1-31  *)
	hour   : int,       		(* 0-23  *)
	minute : int,       		(* 0-59  *)
	second : int,       		(* 0-61 (allowing for leap seconds) *)
	wday   : weekday,
	yday   : int,		        (* 0-365 *)
	isDst  : bool option,		(* daylight savings time in force *)
	offset : int option		(* signed seconds East of UTC:
				           this zone = UTC+t; ~43200 < t <= 43200 *)
      }

    exception Date

    (* 86400 = 24*60*6 is the number of seconds per day *)

    type tmoz = {tm_hour   : int,
		 tm_isdst  : int,	(* 0 = no, 1 = yes, ~1 = don't know *)
		 tm_mday   : int,
		 tm_min    : int,
		 tm_mon    : int,
		 tm_sec    : int, 
		 tm_wday   : int,
		 tm_yday   : int,
		 tm_year   : int
		 }

    local
       fun make f (n: int): tmoz =
	  (f (ref n)
	   ; {
	      tm_hour = Tm.hour (),
	      tm_isdst = Tm.isdst (),
	      tm_mday = Tm.mday (),
	      tm_min = Tm.min (),
	      tm_mon = Tm.mon (),
	      tm_sec = Tm.sec (),
	      tm_wday = Tm.wday (),
	      tm_yday = Tm.yday (),
	      tm_year = Tm.year ()
	      })
    in
       val getlocaltime_ = make Prim.localTime
       val getunivtime_ = make Prim.gmTime
    end

    fun setTmBuf {tm_hour, tm_isdst, tm_mday, tm_min, tm_mon, tm_sec, tm_wday,
		 tm_yday, tm_year} =
       (Tm.setHour tm_hour
	; Tm.setIsdst tm_isdst
	; Tm.setMday tm_mday
	; Tm.setMin tm_min
	; Tm.setMon tm_mon
	; Tm.setSec tm_sec
	; Tm.setWday tm_wday
	; Tm.setYday tm_yday
	; Tm.setYear tm_year)
	
    fun mktime_ (t: tmoz): int = (setTmBuf t; Prim.mkTime ())

    (* The offset to add to local time to get UTC: positive West of UTC *)
    val localoffset: int = Prim.localOffset ()

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
	
    fun tmozToDate ({tm_hour, tm_isdst, tm_mday, tm_min, tm_mon, tm_sec,
		    tm_wday, tm_yday, tm_year}: tmoz) offset = 
	DATE {year = tm_year + 1900, month = tomonth tm_mon, 
	      day = tm_mday, hour = tm_hour, minute = tm_min, 
	      second = tm_sec, wday = toweekday tm_wday,
	      yday = tm_yday, 
	      isDst = (case tm_isdst of
			  0 => SOME false 
			| 1 => SOME true
			| _ => NONE),
	      offset = offset }

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

    fun yeardays year = if leapyear year then 366 else 365

    (* Check whether date may be passed to ISO/ANSI C functions: *)

    fun okDate (DATE {year, month, day, hour, minute, second, ...}) =
	1900 <= year 
	andalso 1 <= day    andalso day    <= monthdays year month
	andalso 0 <= hour   andalso hour   <= 23
	andalso 0 <= minute andalso minute <= 59
	andalso 0 <= second andalso second <= 61 (* leap seconds *)

    fun dateToTmoz (dt as DATE {year, month, day, hour, minute, second,
				wday, yday, isDst, offset}): tmoz =
	if okDate dt then 
	    {tm_hour = hour, tm_mday = day, tm_min = minute, 
	     tm_mon = frommonth month, tm_sec = second, 
	     tm_year = year -? 1900, 
	     tm_isdst = case isDst of SOME false=>0 | SOME true=>1 | NONE=> ~1,
 	     tm_wday = fromwday wday, tm_yday = yday} 
	else
	    raise Date;

    (* -------------------------------------------------- *)
    (* Translated from Emacs's calendar.el:               *)

    (* Reingold: Number of the day within the year: *)

    fun dayinyear (year: int) (month: month) (day: int): int = 
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
	    dayinyear year month day 
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
	    val date1 = DATE {
			      year   = year,
			      month  = month,
			      day    = day,
			      hour   = hour,
			      minute = minute,
			      second = second,
			      wday   = weekday dayno,
			      yday   = dayinyear year month day,
			      offset = offset,
			      isDst  = case offset of 
			                   NONE   => NONE 
					 | SOME _ => SOME false }
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
		  | SOME t => date1
	end

    fun fromTimeLocal t = 
	tmozToDate (getlocaltime_ (Time.toSeconds t)) NONE;

    fun fromTimeUniv t = 
	tmozToDate (getunivtime_ (Time.toSeconds t)) (SOME 0);

    (* The following implements conversion from a local date to 
       a Time.time.  It IGNORES wday and yday.  *)

    fun toTime (date as DATE {offset, ...}) = 
	let
	   val secoffset = 
	      case offset of
		 NONE      => 0
	       | SOME secs => localoffset + secs
	    val clock = mktime_ (dateToTmoz date) - secoffset
	in
	    if clock < 0 then raise Date
	    else Time.fromSeconds clock
	end;

    fun localOffset () = Time.fromSeconds (localoffset mod 86400)

    fun toString date =
       (setTmBuf (dateToTmoz date)
	; C.CS.extractToChar (Prim.ascTime (), #"\n"))

    local
       val isFormatChar =
	  let
	     val a = Array.tabulate (Char.maxOrd + 1, fn _ => false)
	     val validChars = "aAbBcdHIjmMpSUwWxXyYZ%"
	  in Util.naturalForeach
	     (size validChars, fn i =>
	      Array.update (a, Char.ord (String.sub (validChars, i)), true));
	     fn c => Array.sub (a, Char.ord c)
	  end
    in	   
       fun fmt fmtStr d =
	  let
	     val _ = setTmBuf (dateToTmoz d)
	     val bufLen = 50 (* more than enough for a single format char *)
	     val buf = Primitive.Array.array bufLen
	     fun strftime fmtChar =
		let
		   val len = Prim.strfTime (buf, bufLen,
					    concat ["%", str fmtChar, "\000"])
		in if len = 0
		      then raise Fail "Date.fmt"
		   else Array.extract (buf, 0, SOME len)
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

    (* To scan dates in the format "Wed Mar  8 19:06:45 1995" *)

    exception BadFormat;
    fun getVal (SOME v) = v
      | getVal NONE     = raise BadFormat;
	
    fun scan getc src =
    let val getstring  = StringCvt.splitl Char.isAlpha getc
	fun getint src = getVal (Int.scan StringCvt.DEC getc src)
	fun drop p     = StringCvt.dropl p getc
	fun isColon c  = (c = #":")

	local
	   fun err () = raise BadFormat
	   fun check1 (s, c1, r) = if String.sub(s,1) = c1
	                              then r
				   else err ()
	   fun check2 (s, c2, r) = if String.sub(s,2) = c2
	                              then r
				   else err ()
	   fun check12 (s, c1, c2, r) = if String.sub(s,1) = c1
	                                   andalso 
					   String.sub(s,2) = c2
					   then r
					else err ()
 	in
	  val getMonth = fn m =>
	     if String.size m <> 3
	        then err ()
	     else
	        (case String.sub (m, 0) of
		    #"J" => (case String.sub (m, 1) of
			        #"a" => check2 (m, #"n", Jan)
			      | #"u" => (case String.sub (m, 2) of
					    #"n" => Jun
					  | #"l" => Jul
					  | _ => err ())
			      | _ => err ())
		  | #"F" => check12 (m, #"e", #"b", Feb)
		  | #"M" => check1 (m, #"a", case String.sub (m, 2) of
				                #"r" => Mar
					      | #"y" => May
					      | _ => err ())
		  | #"A" => (case String.sub (m, 1) of
			        #"p" => check2 (m, #"r", Apr)
			      | #"u" => check2 (m, #"g", Aug)
			      | _ => err ())
		  | #"S" => check12 (m, #"e", #"p", Sep)
		  | #"O" => check12 (m, #"c", #"t", Oct)
		  | #"N" => check12 (m, #"o", #"v", Nov)
		  | #"D" => check12 (m, #"e", #"c", Dec)
		  | _ => err ())
	  val getWday = fn w =>
	     if String.size w <> 3
	        then err ()
	     else
	        (case String.sub (w, 0) of
		    #"S" => (case String.sub (w,1) of
			        #"u" => check2 (w, #"n", Sun)
			      | #"a" => check2 (w, #"t", Sat)
			      | _ => err ())
		  | #"M" => check12 (w, #"o", #"n", Mon)
		  | #"T" => (case String.sub (w,1) of
			        #"u" => check2 (w, #"e", Tue)
			      | #"h" => check2 (w, #"u", Thu)
			      | _ => err ())
		  | #"W" => check12 (w, #"e", #"d", Wed)
		  | #"F" => check12 (w, #"r", #"i", Fri)
		  | _ => err ())
	end

	val (wday, src1)  = getstring src
	val (month, src2) = getstring (drop Char.isSpace src1)
	val (day, src3)   = getint src2
	val (hour, src4)  = getint src3
	val (min, src5)   = getint (drop isColon src4)
	val (sec, src6)   = getint (drop isColon src5)
	val (year, src7)  = getint src6
	val month         = getMonth month
    in SOME (DATE {year = year, month = month,
		   day = day,  hour = hour, minute = min, 
		   second = sec, wday = getWday wday, 
		   yday = dayinyear year month day, 
		   isDst = NONE, offset = NONE}, src7) 
    end
    handle BadFormat => NONE

    fun fromString s = StringCvt.scanString scan s

    (* Ignore timezone and DST when comparing dates: *)
    fun compare 
	(DATE {year=y1,month=mo1,day=d1,hour=h1,minute=mi1,second=s1, ...},
	 DATE {year=y2,month=mo2,day=d2,hour=h2,minute=mi2,second=s2, ...}) =
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
	    let val (dayoffset, offset') = 
	        case offset of
		    NONE      => (0, NONE)
		  | SOME time => 
			let val secs      = Time.toSeconds time
			    val secoffset = 
				if secs <= 43200 then ~secs else 86400 - secs
			in (Int.quot (secs, 86400), SOME secoffset) end
		val day' = day + dayoffset
	    in
		normalizedate year month day' hour minute second offset'
	    end

    fun year (DATE { year, ... }) = year
	
    fun month (DATE { month, ... }) = month
	
    fun day (DATE { day, ... }) = day

    fun hour (DATE { hour, ... }) = hour

    fun minute (DATE { minute, ... }) = minute

    fun second (DATE { second, ... }) = second

    fun weekDay (DATE { wday, ... }) = wday

    fun yearDay (DATE { yday, ... }) = yday

    fun isDst (DATE { isDst, ... }) = isDst

    fun offset (DATE { offset, ... }) = 
	Option.map
	(fn secs => Time.fromSeconds ((86400 + secs) mod 86400)) 
	offset
  end
