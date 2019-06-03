(* Copyright (C) 2019 Matthew Fluet.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor PowerSetLattice_ListSet (S: POWERSET_LATTICE_STRUCTS): POWERSET_LATTICE =
struct

open S

structure Elt = Element

structure EltSet :>
   sig
      type t
      val add: t * Elt.t -> t
      val contains: t * Elt.t -> bool
      val empty: t
      val foreach: t * (Elt.t -> unit) -> unit
      val fromList: Elt.t list -> t
      val layout: t -> Layout.t
      val singleton: Elt.t -> t
      val toList: t -> Elt.t list
   end =
   struct
      type t = Elt.t list
      val {add, contains, empty, singleton, ...} =
         List.set {equals = Elt.equals, layout = Elt.layout}
      val foreach = List.foreach
      fun fromList es = List.removeDuplicates (es, Elt.equals)
      fun layout es =
         Layout.seq [Layout.str "{",
                     (Layout.fill o Layout.separateRight)
                     (List.map (es, Elt.layout), ","),
                     Layout.str "}"]
      val toList = fn es => es
   end

datatype t = T of {elements: EltSet.t ref,
                   handlers: (Elt.t -> unit) list ref}

fun layout (T {elements, ...}) =
   EltSet.layout (!elements)

fun new es = T {elements = ref es,
                handlers = ref []}

fun empty () = new EltSet.empty
fun singleton e = new (EltSet.singleton e)
fun fromList es = new (EltSet.fromList es)

fun getElements (es as T {elements, ...}) =
   EltSet.toList (!elements)

fun addHandler (T {elements, handlers, ...}, h) =
   (List.push (handlers, h);
    EltSet.foreach (!elements, fn e => h e))

fun op<< (e, T {elements, handlers, ...}) =
   if EltSet.contains (!elements, e)
      then ()
      else (elements := EltSet.add (!elements, e);
            List.foreach (!handlers, fn h => h e))

fun op<= (es, es') =
   addHandler(es, fn e => << (e, es'))

end


functor PowerSetLattice_UniqueSet (S: POWERSET_LATTICE_STRUCTS): POWERSET_LATTICE =
struct

open S

structure Elt = Element

structure EltSet = UniqueSet (structure Element = Element
                              val cacheSize: int = 7
                              val bits: int = 13)
structure EltSet =
   struct
      open EltSet
      fun layout es =
         Layout.seq [Layout.str "{",
                     (Layout.fill o Layout.separateRight)
                     (List.map (toList es, Elt.layout), ","),
                     Layout.str "}"]
   end


datatype t = T of {elements: EltSet.t ref,
                   handlers: (Elt.t -> unit) list ref,
                   coercedTo: t list ref}

fun equals (T {elements = elements1, ...}, T {elements = elements2, ...}) =
   elements1 = elements2

fun layout (T {elements, ...}) =
   EltSet.layout (!elements)

fun new es = T {elements = ref es,
                handlers = ref [],
                coercedTo = ref []}

fun empty () = new EltSet.empty
fun singleton e = new (EltSet.singleton e)
fun fromList es = new (EltSet.fromList es)

fun getElements (es as T {elements, ...}) =
   EltSet.toList (!elements)

fun addHandler (T {elements, handlers, ...}, h) =
   (List.push (handlers, h);
    EltSet.foreach (!elements, fn e => h e))

fun send (T {elements, handlers, coercedTo}, es) =
   let
      val diff = EltSet.- (es, !elements)
   in
      if EltSet.isEmpty diff
         then ()
         else (elements := EltSet.+ (diff, !elements);
               List.foreach (!coercedTo, fn to => send (to, diff));
               List.foreach (!handlers, fn h =>
                             EltSet.foreach (diff, fn e => h e)))
   end

fun op<= (from as T {elements, coercedTo, ...}, to) =
   if List.exists (!coercedTo, fn es => equals (es, to))
      then ()
      else (List.push (coercedTo, to)
            ; send (to, !elements))

fun op<< (e, es) =
   send (es, EltSet.singleton e)

end
