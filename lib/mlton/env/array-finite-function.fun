(* Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ArrayFiniteFunction(): ARRAY_FINITE_FUNCTION = 
struct

structure Domain =
   struct
      type 'a t =
         {size: int, fromInt: int -> 'a, toInt: 'a -> int}

      fun pair({size, fromInt, toInt}: 'a1 t,
               {size=size', fromInt=fromInt', toInt=toInt'}: 'a2 t,
               inj: 'a1 -> 'a,
               inj': 'a2 -> 'a,
               out: 'a * ('a1 -> int) * ('a2 -> int) -> int) =
         {size = size + size',
          toInt = fn d => out(d, toInt, fn d' => size + toInt' d'),
          fromInt = fn n => if n < size then inj(fromInt n)
                            else inj'(fromInt'(n - size))}
   end

datatype ('a, 'b) t =
   T of {domain: 'a Domain.t,
         array: 'b Array.t}

fun empty(domain: 'a Domain.t) =
   T{domain = domain,
     array = Array.new(#size domain, NONE)}

fun new(domain: 'a Domain.t, x) =
   T{domain = domain,
     array = Array.new(#size domain, x)}

fun tabulate(domain as {size, fromInt, ...}: 'a Domain.t, f) =
   T{domain = domain,
     array = Array.tabulate(size, f o fromInt)}

fun size(T{domain={size, ...}, ...}) = size

fun lookup(T{domain={toInt, ...}, array}, x) = Array.sub(array, toInt x)

fun foreach(T{domain={fromInt, ...}, array}, f) =
   Array.foreachIndex(array, fn (i, x) => f(fromInt i, x))

fun set(T{domain={toInt, ...}, array}, x, y) =
   Array.update(array, toInt x, y)

fun toFunction f a = lookup(f, a)

end

structure ArrayFiniteFunction = ArrayFiniteFunction()
