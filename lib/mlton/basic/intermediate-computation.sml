(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure IntermediateComputation: INTERMEDIATE_COMPUTATION = 
struct

structure In = In0
structure Int = Pervasive.Int

structure Result =
   struct
      datatype t =
         Raise
       | Return of unit -> Layout.t

      local open Layout
      in
         fun layout r =
            case r of
               Raise => str "raise"
             | Return f => f()
      end
   end

structure Computation =
   struct
      structure Time = Time

      datatype t = T of callResult list
      withtype callResult = {name: string,
                             layoutArg: unit -> Layout.t,
                             time: Time.t option,
                             body: t,
                             result: Result.t}

      structure CR =
         struct
            open Layout

            type t = callResult

            fun layoutName({name, ...}:t) = str(name ^ " ")

            fun layoutCall(cr as {layoutArg, ...}:t) =
               seq[layoutName cr, layoutArg()]

            val darrow = str "==> "

            fun layoutDarrow _ = darrow

            fun layoutTime({time, ...}:t) =
               case time of
                  SOME t => Time.layout t
                | NONE => empty

            fun layoutReturn({result, ...}:t) =
               seq[darrow, Result.layout result]
         end

      fun time(T crs) = List.fold(crs, Time.zero, fn ({time, ...}, t) =>
                                  case time of
                                     NONE => t
                                   | SOME t' => Time.+(t, t'))

      fun keepAll(c, pred) =
         let
            fun keepAll(T crs) =
               T(List.foldr(crs, [],
                            fn ({name, body, layoutArg, time, result}, crs) =>
                            let val body as T crs' = keepAll body
                            in if pred name
                                  then {name = name, body = body,
                                        layoutArg = layoutArg, time = time,
                                        result = result} :: crs
                               else crs' @ crs
                            end))
         in keepAll c
         end

      fun makeOutputs(pre, post, filter) out =
         let val indentation = ref 0
            val space = 3
            fun left() = indentation := !indentation - space
            fun right() = indentation := !indentation + space
            fun print l = (Layout.output(Layout.indent(l, !indentation), out)
                           ; Out.newline out)
            fun output(T crs) = List.foreach(crs, outputCr)
            and outputCr(cr as {body, ...}) =
               let val printCr = filter cr
               in if printCr
                     then (print(pre cr); right())
                  else ()
                     ; output body
                     ; if printCr then (left(); print(post cr)) else ()
               end
         in (output, outputCr)
         end

      val makeOutput =
         makeOutputs(CR.layoutCall, CR.layoutReturn, fn _ => true)
      fun output(c, out) = #1(makeOutput out) c
      fun outputCr(cr, out) = #2(makeOutput out) cr

      val makeOutputCalls =
         makeOutputs(CR.layoutName, CR.layoutDarrow, fn _ => true)
      fun outputCalls(c, out) = #1(makeOutputCalls out) c
      fun outputCrCalls(cr, out) = #2(makeOutputCalls out) cr

      fun outputTimes(c, out) =
         #1(makeOutputs(CR.layoutName, CR.layoutTime,
                        fn {time, ...} =>
                        case time of
                           NONE => false
                         | SOME _ => true) out) c

      local
         val out = Out.error
         val print = Out.outputc out

         structure Int =
            struct
               open Int

               exception Input
               fun input i = (In.ignoreSpaces i
                              ; (case fromString(In.inputToSpace i) of
                                    NONE => raise Input
                                  | SOME n => n))

               fun inputBetween{ins, error: unit -> unit, min, max} =
                  let
                     fun loop() =
                        let fun continue() = (error() ; loop())
                        in let val n = input ins
                           in if min <= n andalso n <= max
                                 then n
                              else continue()
                           end handle Input => continue()
                        end
                  in loop()
                  end

               val layout = Layout.str o toString
            end

         fun inputBetween(min, max) =
           Int.inputBetween{ins = In.standard,
                            error = fn () => Out.output(out, "? "),
                            min = min, max = max}

         fun choose (choices: (string * 'a) list): 'a =
            let
               val n =
                  List.fold(choices, 0, fn ((name, _),n) =>
                            (Layout.output(Int.layout n, out)
                             ; print(concat[". ", name, "\n"])
                             ; n+1))
               val _ = Out.output(out, "? ")
               val m = inputBetween(0,n-1)
            in #2(List.nth(choices, m))
            end

         fun chooseThunk cs = choose cs ()

         exception Quit
         exception Back

         val standardChoices = [("quit", fn () => raise Quit),
                                ("back", fn () => raise Back)]

         fun inspect(c as T crs) =
            case crs of
               [cr] => inspectCr cr
             | _ =>
                  let
                     fun loop() =
                        (chooseThunk
                         (standardChoices
                          @ [("skip raises", fn () =>
                              (skipRaises c; raise Back)),
                             ("output", fn () => output(c, out)),
                             ("output calls",
                              fn () => outputCalls(c, out))]
                          @ choices c)
                         ; loop())
                  in loop() handle Back => ()
                  end
         and choices(T crs) =
            List.map(crs, fn cr as {name, ...} =>
                     ("  " ^ name, fn () => inspectCr cr))
         and inspectCr(cr as {name, layoutArg, body, result, time = _}) =
            (print(concat["Call to ", name, "\n"])
             ; let
                  fun loop() =
                     (chooseThunk
                      (standardChoices
                       @ [("skip raises", fn () =>
                           (skipRaisesCr cr; raise Back)),
                          ("output", fn () =>
                           outputCr(cr, out)),
                          ("output calls", fn () =>
                           outputCrCalls(cr, out)),
                          ("argument", fn () =>
                           (Layout.output(layoutArg(), out)
                            ; Out.newline out)),
                          ("result", fn () =>
                           (Layout.output(Result.layout result, out)
                            ; Out.newline out))]
                       @ choices body)
                      ; loop())
               in loop() handle Back => ()
               end)
         and skipRaises(c as T crs) =
            case crs of
               [] => ()
             | _ => (skipRaisesCr(List.last crs)
                     ; inspect c)
         and skipRaisesCr(cr as {result, body, ...}) =
            (case result of
                Result.Raise => skipRaises body
              | Result.Return _ => ()
             ; inspectCr cr)
      in val inspect = fn c => inspect c handle Quit => ()
      end      

   end

(*---------------------------------------------------*)
(*                   Main Datatype                   *)
(*---------------------------------------------------*)

datatype t =
   T of {calls: {name: string,
                  layoutArg: unit -> Layout.t,
                  prev: Computation.callResult list} list ref,
         after: Computation.callResult list ref}

fun empty() = T{calls = ref [],
                after = ref []}

fun atTopLevel(T{calls, ...}) = List.isEmpty(!calls)

fun call(T{calls, after},name, layoutArg) =
   (List.push(calls, {name = name, layoutArg = layoutArg, prev = !after})
    ; after := [])

fun return(T{calls, after}, result, time) =
   case !calls of
      [] => Error.bug "IntermediateComputation.return: without a call"
    | {name, layoutArg, prev} :: cs =>
         (calls := cs
          ; after := {name = name, layoutArg = layoutArg,
                      result = result, time = time,
                      body = Computation.T(List.rev(!after))} :: prev)

fun raisee(c, t) = return(c, Result.Raise, t)
val return = fn (c, r, t) => return(c, Result.Return r, t)

fun finish(c as T{calls, after}) =
   let
      fun loop() =
         case !calls of
            [] => Computation.T(List.rev(!after))
          | _ => (raisee(c, NONE); loop())
   in loop()
   end

end
