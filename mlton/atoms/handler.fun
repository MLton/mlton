(* Copyright (C) 2009,2017,2019 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Handler (S: HANDLER_STRUCTS): HANDLER =
   struct
      open S

      datatype t =
         Caller
       | Dead
       | Handle of Label.t

      fun layout (h: t): Layout.t =
         let
            open Layout
         in
            case h of
               Caller => str "Caller"
             | Dead => str "Dead"
             | Handle l => seq [str "Handle ", Label.layout l]
         end

      val equals =
         fn (Caller, Caller) => true
          | (Dead, Dead) => true
          | (Handle l, Handle l') => Label.equals (l, l')
          | _ => false

      fun foldLabel (h: t, a: 'a, f: Label.t * 'a -> 'a): 'a =
         case h of
            Caller => a
          | Dead => a
          | Handle l => f (l, a)

      fun foreachLabel (h, f) = foldLabel (h, (), f o #1)

      fun map (h, f) =
         case h of
            Caller => Caller
          | Dead => Dead
          | Handle l => Handle (f l)

      local
         val newHash = Random.word
         val caller = newHash ()
         val dead = newHash ()
         val handlee = newHash ()
      in
         fun hash (h: t): word =
            case h of
               Caller => caller
             | Dead => dead
             | Handle l => Hash.combine (handlee, Label.hash l)
      end
   end
