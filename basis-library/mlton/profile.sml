(* Copyright (C) 2003-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonProfile: MLTON_PROFILE =
struct

structure P = Primitive.MLton.Profile

val gcState = Primitive.MLton.GCState.gcState

val isOn = P.isOn

structure Data =
   struct
      datatype t = T of {isCurrent: bool ref,
                         isFreed: bool ref,
                         raw: P.Data.t}

      val all: t list ref = ref []

      local
         fun make f (T r) = f r
      in
         val isFreed = make #isFreed
         val raw = make #raw
      end

      fun equals (d, d') =
         isFreed d = isFreed d'

      fun free (d as T {isCurrent, isFreed, raw, ...}) =
         if not isOn
            then ()
         else
            if !isFreed
               then raise Fail "free of freed profile data"
            else if !isCurrent
                    then raise Fail "free of current profile data"
                 else
                    (all := List.foldl (fn (d', ac) =>
                                        if equals (d, d')
                                           then ac
                                        else d' :: ac) [] (!all)
                     ; P.Data.free (gcState, raw)
                     ; isFreed := true)

      fun make (raw: P.Data.t): t =
         T {isCurrent = ref false,
            isFreed = ref false,
            raw = raw}

      fun malloc (): t =
         let
            val array =
               if isOn
                  then P.Data.malloc gcState
               else P.Data.dummy
            val d = make array
            val _ = all := d :: !all
         in
            d
         end

      fun write (T {isFreed, raw, ...}, file) =
         if not isOn then
            ()
         else if !isFreed then
            raise Fail "write of freed profile data"
         else
            P.Data.write (gcState, raw,
                          Primitive.NullString8.fromString
                          (String.nullTerm file))
   end

val r: Data.t ref = ref (Data.make P.Data.dummy)

fun current () = !r

fun setCurrent (d as Data.T {isCurrent, isFreed, raw, ...}) =
   if not isOn
      then ()
   else
      if !isFreed
         then raise Fail "setCurrent of freed profile data"
      else
         let
            val Data.T {isCurrent = ic, ...} = current ()
            val _ = ic := false
            val _ = isCurrent := true
            val _ = r := d
            val _ = P.setCurrent (gcState, raw)
         in
            ()
         end

fun withData (d: Data.t, f: unit -> 'a): 'a =
   let
      val old = current ()
      val _ = setCurrent d
   in
      DynamicWind.wind (f, fn () => setCurrent old)
   end

fun init () = setCurrent (Data.make (P.getCurrent gcState))

val _ =
   if not isOn
      then ()
   else
      let
         val _ =
            Cleaner.addNew
            (Cleaner.atExit, fn () =>
             (P.done gcState
              ; Data.write (current (), "mlmon.out")
              ; List.app (fn d => P.Data.free (gcState, Data.raw d)) 
                         (!Data.all)))
         val _ =
            Cleaner.addNew
            (Cleaner.atLoadWorld, fn () =>
             ((* In a new world, all of the old profiling data is invalid. *)
              Data.all := []
              ; init ()))
      in
         init ()
      end

end

