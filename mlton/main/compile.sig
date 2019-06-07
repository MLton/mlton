(* Copyright (C) 2019 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature COMPILE_STRUCTS =
   sig
   end

signature COMPILE =
   sig
      include COMPILE_STRUCTS

      val mkCompile:
         {outputC: unit -> {file: File.t,
                            print: string -> unit,
                            done: unit -> unit},
          outputLL: unit -> {file: File.t,
                             print: string -> unit,
                             done: unit -> unit},
          outputS: unit -> {file: File.t,
                            print: string -> unit,
                            done: unit -> unit}} ->
         {mlb: {frontend: File.t -> unit, compile: File.t -> unit},
          sml: {frontend: File.t -> unit, compile: File.t -> unit},
          xml: {frontend: File.t -> unit, compile: File.t -> unit},
          sxml: {frontend: File.t -> unit, compile: File.t -> unit},
          ssa: {frontend: File.t -> unit, compile: File.t -> unit},
          ssa2: {frontend: File.t -> unit, compile: File.t -> unit}}

      val compileMLB: {input: File.t,
                       outputC: unit -> {file: File.t,
                                         print: string -> unit,
                                         done: unit -> unit},
                       outputLL: unit -> {file: File.t,
                                          print: string -> unit,
                                          done: unit -> unit},
                       outputS: unit -> {file: File.t,
                                         print: string -> unit,
                                         done: unit -> unit}} -> unit
      val compileSML: {input: File.t,
                       outputC: unit -> {file: File.t,
                                         print: string -> unit,
                                         done: unit -> unit},
                       outputLL: unit -> {file: File.t,
                                          print: string -> unit,
                                          done: unit -> unit},
                       outputS: unit -> {file: File.t,
                                         print: string -> unit,
                                         done: unit -> unit}} -> unit
      val compileXML: {input: File.t,
                       outputC: unit -> {file: File.t,
                                         print: string -> unit,
                                         done: unit -> unit},
                       outputLL: unit -> {file: File.t,
                                          print: string -> unit,
                                          done: unit -> unit},
                       outputS: unit -> {file: File.t,
                                         print: string -> unit,
                                         done: unit -> unit}} -> unit
      val compileSXML: {input: File.t,
                        outputC: unit -> {file: File.t,
                                          print: string -> unit,
                                          done: unit -> unit},
                        outputLL: unit -> {file: File.t,
                                           print: string -> unit,
                                           done: unit -> unit},
                        outputS: unit -> {file: File.t,
                                          print: string -> unit,
                                          done: unit -> unit}} -> unit
      val compileSSA: {input: File.t,
                       outputC: unit -> {file: File.t,
                                         print: string -> unit,
                                         done: unit -> unit},
                       outputLL: unit -> {file: File.t,
                                          print: string -> unit,
                                          done: unit -> unit},
                       outputS: unit -> {file: File.t,
                                         print: string -> unit,
                                         done: unit -> unit}} -> unit
      val compileSSA2: {input: File.t,
                        outputC: unit -> {file: File.t,
                                          print: string -> unit,
                                          done: unit -> unit},
                        outputLL: unit -> {file: File.t,
                                           print: string -> unit,
                                           done: unit -> unit},
                        outputS: unit -> {file: File.t,
                                          print: string -> unit,
                                          done: unit -> unit}} -> unit

      val elaborateMLB: {input: File.t} -> unit
      val elaborateSML: {input: File.t} -> unit
      val setCommandLineConstant: {name: string, value: string} -> unit
      val sourceFilesMLB: {input: File.t} -> File.t vector
      (* output a C file to print out the basis constants. *)
      val outputBasisConstants: Out.t -> unit
   end
