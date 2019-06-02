(* Copyright (C) 2009,2017,2019 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Return (S: RETURN_STRUCTS): RETURN =
   struct
      open S

      datatype t =
         Dead
       | NonTail of {cont: Label.t,
                     handler: Handler.t}
       | Tail

      fun layout r =
         let
            open Layout
         in
            case r of
               Dead => str "Dead"
             | NonTail {cont, handler} =>
                  seq [str "NonTail ",
                       Layout.record
                       [("cont", Label.layout cont),
                        ("handler", Handler.layout handler)]]
             | Tail => str "Tail"
         end

      fun equals (r, r'): bool =
         case (r, r') of
            (Dead, Dead) => true
          | (NonTail {cont = c, handler = h},
             NonTail {cont = c', handler = h'}) =>
               Label.equals (c, c') andalso Handler.equals (h, h')
           | (Tail, Tail) => true
           | _ => false

      fun foldLabel (r: t, a, f) =
         case r of
            Dead => a
          | NonTail {cont, handler} =>
               Handler.foldLabel (handler, f (cont, a), f)
          | Tail => a

      fun foreachLabel (r, f) = foldLabel (r, (), f o #1)

      fun foreachHandler (r, f) =
         case r of
            Dead => ()
          | NonTail {handler, ...} => Handler.foreachLabel (handler, f)
          | Tail => ()

      fun map (r, f) =
         case r of
            Dead => Dead
          | NonTail {cont, handler} =>
               NonTail {cont = f cont,
                        handler = Handler.map (handler, f)}
          | Tail => Tail

      fun compose (r, r') =
         case r' of
            Dead => Dead
          | NonTail {cont, handler} =>
               NonTail
               {cont = cont,
                handler = (case handler of
                              Handler.Caller =>
                                 (case r of
                                     Dead => Handler.Caller
                                   | NonTail {handler, ...} => handler
                                   | Tail => Handler.Caller)
                            | Handler.Dead => handler
                            | Handler.Handle _ => handler)}
          | Tail => r

      local
         val newHash = Random.word
         val dead = newHash ()
         val nonTail = newHash ()
         val tail = newHash ()
      in
         fun hash r =
            case r of
               Dead => dead
             | NonTail {cont, handler} =>
                  Hash.combine3 (nonTail, Label.hash cont, Handler.hash handler)
             | Tail => tail
      end
   end
