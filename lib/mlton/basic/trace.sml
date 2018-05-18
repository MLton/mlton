(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor TraceControl (structure StringMap: STRING_MAP
                      datatype status = Always | Flagged | Never
                      type flags
                      val map: flags StringMap.t
                      val getFlag: flags -> bool ref
                      val default: bool ref
                      val status: status) =
struct

val status = ref status

fun always () = status := Always
fun flagged () = status := Flagged
fun never () = status := Never

fun isOn b =
   case !status of
      Always => true
    | Flagged => !b
    | Never => false

val default = default

fun flag name = getFlag (StringMap.lookup (map, name))

fun whats b () = StringMap.keepAll (map, fn flags => b = !(getFlag flags))

val whatsOn = whats true
val whatsOff = whats false

fun sets b () =
   StringMap.foreach (map, fn flags => getFlag flags := b)

val all = sets true
val none = sets false

fun some b ss = List.foreach (ss, fn s => flag s := b)

val on = some true
val off = some false

end

(*-------------------------------------------------------------------*)
(*                               Trace                               *)
(*-------------------------------------------------------------------*)

structure Trace: TRACE =
struct

structure IC = IntermediateComputation
structure Timer = Time

val immediateDefault = ref false
val delayedDefault = ref false
val timeDefault = ref false

type flags = {immediate: bool ref,
              delayed: bool ref,
              time: bool ref}

val map = StringMap.new (fn () => {immediate = ref (!immediateDefault),
                                   delayed = ref (!delayedDefault),
                                   time = ref (!timeDefault)})

fun traceable () = StringMap.domain map

fun outputTraceable () =
   Layout.output (List.layout Layout.str (traceable ()),
                 Out.standard)

datatype status = Always | Flagged | Never

structure Immediate =
   TraceControl (type flags = flags
                 val map = map
                 structure StringMap = StringMap
                 datatype status = datatype status
                 fun getFlag ({immediate, ...}: flags) = immediate
                 val default = immediateDefault
                 val status = Never)
structure Delayed =
   struct
      structure C = 
         TraceControl (type flags = flags
                      val map = map
                      structure StringMap = StringMap
                      datatype status = datatype status
                      fun getFlag ({delayed, ...}: flags) = delayed
                      val default = delayedDefault
                      val status = Never)
      open C

      val keepAll = ref true
   end

structure Time =
   TraceControl (type flags = flags
                val map = map
                structure StringMap = StringMap
                datatype status = datatype status
                fun getFlag ({time, ...}: flags) = time
                val default = timeDefault
                val status = Never)

fun never () = (Immediate.never ()
               ; Delayed.never ()
               ; Time.never ())

fun always () = (Immediate.always ()
                ; Delayed.always ()
                ; Time.always ())

fun flagged () = (Immediate.flagged ()
                 ; Delayed.flagged ()
                 ; Time.flagged ())

fun reset () =
   StringMap.foreach (map, fn {immediate, delayed, time} =>
                      (immediate := false
                       ; delayed := false
                       ; time := false))

(*---------------------------------------------------*)
(*                 Delayed Feedback                  *)
(*---------------------------------------------------*)

structure Computation = IC.Computation

datatype comp =
   Working of IC.t
  | Finished of Computation.t

val emptyIc = IC.empty
fun empty () = Working (emptyIc ())

val currentComputation = ref (empty ())

fun clear () = currentComputation := empty ()

fun finishedComputation () =
   case !currentComputation of
      Working ic => let val c = IC.finish ic
                    in currentComputation := Finished c
                       ; c
                    end
    | Finished c => c
val computation = finishedComputation

fun history () = Computation.output (finishedComputation (),
                                   Out.error)
fun calls () = Computation.outputCalls (finishedComputation (),
                                      Out.error)
fun times () = Computation.outputTimes (finishedComputation (),
                                      Out.error)
fun inspect () = Computation.inspect (finishedComputation ())

fun ic () =
   case !currentComputation of
      Finished _ => let val ic = emptyIc ()
                    in currentComputation := Working ic
                       ; ic
                    end
    | Working ic => ic

fun delayedCall (name, layoutArg, layoutAns) =
   {call = fn () =>
    let val comp = ic ()
       val comp = if !Delayed.keepAll orelse not (IC.atTopLevel comp)
                     then comp
                  else (clear (); ic ())
    in IC.call (comp, name, layoutArg)
    end,
    raisee = fn (t, _) => IC.raisee (ic (), t),
    return = fn (ans, t) => IC.return (ic (),
                                     fn () => layoutAns ans,
                                     t)}

(*---------------------------------------------------*)
(*                Immediate Feedback                 *)
(*---------------------------------------------------*)

structure Immediate =
   struct
      open Immediate

      datatype debug =
         None
       | Terminal
       | Out of Out.t

      val debug = ref None

      val showTime = ref false

      val indentation: int ref = ref 0
      val space: int = 3
      fun left () = indentation := !indentation - space
      fun right () = indentation := !indentation + space

      val inChild = ref false
      fun inChildProcess () = (inChild := true; indentation := 0)

      fun message (l: Layout.t): unit =
         case !debug of
            None => ()
          | _ => 
               let
                  val (out, done) =
                     case !debug of
                        Terminal => (Out.openOut "/dev/tty", Out.close)
                      | Out out => (out, Out.flush)
                      | _ => Error.bug "Trace.message"
                  open Layout
               in output (seq [if !inChild
                                then seq [Pid.layout (Pid.current ()), str ": "]
                             else empty,
                             if !showTime
                                then str (Date.fmt
                                          (Date.now (), "%b %d %H:%M:%S "))
                             else empty,
                             indent (l, !indentation)],
                         out)
                  ; Out.newline out
                  ; done out
               end

      fun finish (t, res) =
         (left ()
          ; message (let open Layout
                    in case t of
                       NONE => seq [str "==> ", res]
                     | SOME t =>
                          align [seq [str "==> time = ", Timer.layout t],
                                res]
                    end))

      fun call (name, outArg, layoutAns) =
         let
            open Layout
            fun call () = (message (seq [str name, str " ==> ",
                                         outArg ()
                                         handle e =>
                                            seq [str "layout argument error: ",
                                                 Exn.layout e]])
                           ; right ())
            fun raisee (t, e) = finish (t, seq [str "raise: ", Exn.layout e])
            fun return (ans, t) =
               finish (t,
                       layoutAns ans
                       handle e => seq [str "layout answer error: ",
                                        Exn.layout e])
         in {call = call,
             raisee = raisee,
             return = return}
         end

      val message =
         fn l =>
         (left ()
          ; indentation := 1 + !indentation
          ; message l
          ; indentation := !indentation - 1
          ; right ())

      val messageStr = message o Layout.str
   end

(*---------------------------------------------------*)
(*                  Instrumentation                  *)
(*---------------------------------------------------*)

type info = {name: string, flags: flags}

val bogusInfo = {name = "bogus", flags = {delayed = ref false,
                                          immediate = ref false,
                                          time = ref false}}

val shouldTrace = Assert.debug

fun info name =
   if shouldTrace
      then {name = name, flags = StringMap.lookup (map, name)}
   else bogusInfo

fun traceInfo ({name, flags = {immediate, delayed, time}},
               layoutArg, layoutAns, check) f a =
   if not shouldTrace
      then f a
   else 
      let
         val immediate = Immediate.isOn immediate
         val delayed = Delayed.isOn delayed
         val time = Time.isOn time
      in
         if not (immediate orelse delayed orelse time orelse Assert.debug)
            then f a
         else let val outArg = fn () => layoutArg a
                  val noCall = {call = fn _ => (),
                                raisee = fn _ => (),
                                return = fn _ => ()}
                  val immed = if immediate
                                 then Immediate.call (name, outArg, layoutAns)
                              else noCall
                  val delay = if delayed
                                 then delayedCall (name, outArg, layoutAns)
                              else noCall
                  val _ = (#call delay ()
                           ; #call immed ())
                  val check =
                     if Assert.debug
                        then let val (b, check) = check a
                                 val _ = Assert.assert (concat [name, " argument"],
                                                       fn () => b)
                             in check
                             end
                     else fn _ => true
                  val startTime = if time then SOME (Timer.times ()) else NONE
                  fun getTime () =
                     case startTime of
                        NONE => NONE
                      | SOME {self = {utime = u, stime = s}, ...} =>
                           SOME (let val {self = {utime = u', stime = s'},
                                          ...} = Timer.times ()
                                in Timer.+ (Timer.- (u', u),
                                            Timer.- (s', s))
                                end)
                  val ans = f a handle exn => let val t = getTime ()
                                              in #raisee delay (t, exn)
                                                 ; #raisee immed (t, exn)
                                                 ; raise exn
                                              end
                  val t = getTime ()
              in #return delay (ans, t)
                 ; #return immed (ans, t)
                 ; Assert.assert (concat [name, " result"], fn () => check ans)
                 ; ans
              end
      end

fun assertTrue _ = (true, fn _ => true)

fun traceInfo' (info, layoutArg, layoutAns) =
   traceInfo (info, layoutArg, layoutAns, assertTrue)

fun traceAssert (name, layoutArg, layoutAns, check) =
   traceInfo (info name, layoutArg, layoutAns, check)

fun trace (name, layoutArg, layoutAns) =
   traceAssert (name, layoutArg, layoutAns, assertTrue)

fun ignore _ = Layout.empty

fun traceCall s = trace (s, ignore, ignore)

fun traceRec info =
   let val trace = trace info
   in fn f => let fun fix f a = trace (f (fix f)) a
              in fix f
              end
   end

fun trace0 (name, layoutAns) =
   trace (name, Unit.layout, layoutAns)

fun trace2 (name, layout1, layout2, layoutAns) =
   trace (name, Layout.tuple2 (layout1, layout2), layoutAns)

fun trace3 (name, out1, out2, out3, outAns) =
   trace (name, Layout.tuple3 (out1, out2, out3), outAns)

fun trace4 (name, out1, out2, out3, out4, outAns) =
   trace (name, Layout.tuple4 (out1, out2, out3, out4), outAns)

fun trace5 (name, out1, out2, out3, out4, out5, outAns) =
   trace (name, Layout.tuple5 (out1, out2, out3, out4, out5), outAns)

end

structure Computation = Trace.Computation
