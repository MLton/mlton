(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Longid (S: LONGID_STRUCTS): LONGID =
struct

open S

datatype node = T of {strids: Strid.t list,
                      id: Id.t}

type node' = node
structure Wrap = Region.Wrap
open Wrap
type t = node Wrap.t
type obj = t

fun split id =
   let
      val T {strids, id, ...} = node id
   in
      (strids, id)
   end

val equals =
   fn (id, id') =>
   let
      val T {strids=ss, id=i} = node id
      val T {strids=ss', id=i'} = node id'
   in
      List.equals (ss, ss', Strid.equals) andalso Id.equals (i, i')
   end

fun long (strids, id) =
   makeRegion (T {strids = strids, id = id},
               case strids of
                  [] => Id.region id
                | s :: _ => Region.append (Strid.region s, Id.region id))

fun short id = long ([], id)

fun layout id =
   let
      val T {strids, id} = node id
      open Layout
   in
      seq [case strids of
              [] => empty
            | _ => seq [seq (separate (List.map (strids, Strid.layout), ".")),
                        str "."],
           Id.layout id]
   end

val toString = Layout.toString o layout

fun fromSymbols (ss: Symbol.t list, region: Region.t): t =
   let
      val srs =
         case Region.left region of
             NONE => List.map (ss, fn s => (s, region))
           | SOME p =>
             let
                val file = SourcePos.file p
                val line = SourcePos.line p
             in
                List.unfold
                ((ss, SourcePos.column p),
                 fn (s::ss, cl) =>
                    let
                       val cr = cl + String.length (Symbol.toString s) - 1
                    in
                       SOME
                       ((s, Region.make
                            {left = SourcePos.make {column = cl,
                                                    file = file,
                                                    line = line},
                             right = SourcePos.make {column = cr,
                                                     file = file,
                                                     line = line}}),
                        (ss, cr + 2))
                    end
                  | ([], _) => NONE)
             end
      val (strids, id) = List.splitLast srs
   in
      makeRegion (T {strids = List.map (strids, Strid.fromSymbol),
                     id = Id.fromSymbol id},
                  region)
   end

end
