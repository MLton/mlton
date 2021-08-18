(* Copyright (C) 2009,2014,2016-2017,2019,2021 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor SourceMaps (S: SOURCE_MAPS_STRUCTS): SOURCE_MAPS =
struct

open S

datatype t =
   T of {sourceNames: string vector,
         sourceSeqs: {sourceIndex: int} vector vector,
         sources: {sourceNameIndex: int,
                   successorSourceSeqIndex: int} vector}

val empty = T {sourceNames = Vector.new0 (),
               sourceSeqs = Vector.new0 (),
               sources = Vector.new0 ()}

fun layout (T {sourceNames, sourceSeqs, sources}) =
   Layout.record
   [("sourceNames", Vector.layout String.layout sourceNames),
    ("sourceSeqs",
     Vector.layout (Vector.layout (fn {sourceIndex} =>
                                   Layout.record
                                   [("sourceIndex", Int.layout sourceIndex)]))
     sourceSeqs),
    ("sources",
     Vector.layout (fn {sourceNameIndex, successorSourceSeqIndex} =>
                    Layout.record [("sourceNameIndex", Int.layout sourceNameIndex),
                                   ("successorSourceSeqIndex", Int.layout successorSourceSeqIndex)])
     sources)]

fun layouts (pi, output) = output (layout pi)

fun check (T {sourceNames, sourceSeqs, sources}): bool =
   if !Control.profile = Control.ProfileNone
      then Vector.isEmpty sourceNames
           andalso Vector.isEmpty sourceSeqs
           andalso Vector.isEmpty sources
      else let
              val sourceNamesLength = Vector.length sourceNames
              val sourceSeqsLength = Vector.length sourceSeqs
              val sourcesLength = Vector.length sources
           in
              (Vector.forall
               (sourceSeqs, fn v =>
                Vector.forall
                (v, fn {sourceIndex} =>
                 0 <= sourceIndex andalso sourceIndex < sourcesLength)))
              andalso (Vector.forall
                       (sources, fn {sourceNameIndex, successorSourceSeqIndex} =>
                        0 <= sourceNameIndex andalso sourceNameIndex < sourceNamesLength
                        andalso
                        0 <= successorSourceSeqIndex
                        andalso successorSourceSeqIndex < sourceSeqsLength))
           end

fun checkSourceSeqIndex (T {sourceSeqs, ...}, sourceSeqIndex) =
   0 <= sourceSeqIndex andalso sourceSeqIndex < Vector.length sourceSeqs
end
