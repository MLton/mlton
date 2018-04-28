(* Copyright (C) 2004-2009 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Control: CONTROL =
struct

structure C = Control ()
open C

val debug = control {name = "debug",
                     default = false,
                     toString = Bool.toString}

val allSU = control {name = "allSU",
                     default = false,
                     toString = Bool.toString}

val collect_enums = control {name = "collect_enums",
                             default = true,
                             toString = Bool.toString}

val cppopts = control {name = "cppopts",
                       default = [],
                       toString = List.toString (fn s => s)}

val dir = control {name = "dir",
                   default = "NLFFI-Generated",
                   toString = fn s => s}

val enum_cons = control {name = "enum_cons",
                         default = false,
                         toString = Bool.toString}

val extramembers = control {name = "extramembers",
                            default = [],
                            toString = List.toString (fn s => s)}

val gensym = control {name = "gensym",
                      default = "",
                      toString = fn s => s}

val libhandle = control {name = "libhandle",
                         default = "Library.libh",
                         toString = fn s => s}

structure Linkage =
   struct
      datatype t = Archive | Dynamic | Shared

      val toString =
         fn Archive => "archive"
          | Dynamic => "dynamic"
          | Shared => "shared"
   end
val linkage = control {name = "linkage",
                       default = Linkage.Dynamic,
                       toString = Linkage.toString}

val match = control {name = "match",
                     default = fn _ => false,
                     toString = fn _ => "<fn>"}

val mlbfile = control {name = "mlbfile",
                       default = "nlffi-generated.mlb",
                       toString = fn s => s}

val namedargs = control {name = "namedargs",
                         default = false,
                         toString = Bool.toString}

val prefix = control {name = "prefix",
                      default = "",
                      toString = fn s => s}

structure Target =
   struct
      open MLton.Platform
      datatype arch = datatype Arch.t
      datatype os = datatype OS.t

      datatype t = T of {arch: arch, os: os}
      val host = T {arch = Arch.host, os = OS.host}

      fun toString (T {arch, os}) =
         concat [Arch.toString arch, "-", OS.toString os]

      fun fromString s =
         case String.split (s, #"-") of
            [arch, os] =>
               (case (Arch.fromString arch, OS.fromString os) of
                   (SOME arch, SOME os) =>
                      SOME (T {arch = arch, os = os})
                 | _ => NONE)
          | _ => NONE

      fun make (t as T {arch, os}) =
         case (arch, os) of
            (AMD64, _) => SOME {name = toString t, sizes = SizesAMD64.sizes,
                                endianShift = EndianLittle.shift}
          | (HPPA, _) => SOME {name = toString t, sizes = SizesHPPA.sizes,
                               endianShift = EndianBig.shift}
          | (IA64, Hurd) => SOME {name = toString t, sizes = SizesIA64.sizes,
                                  endianShift = EndianBig.shift}
          | (IA64, HPUX) => SOME {name = toString t, sizes = SizesIA64.sizes,
                                  endianShift = EndianBig.shift}
          | (IA64, Linux) => SOME {name = toString t, sizes = SizesIA64.sizes,
                                   endianShift = EndianLittle.shift}
          | (Sparc, _) => SOME {name = toString t, sizes = SizesSparc.sizes,
                                endianShift = EndianBig.shift}
          | (PowerPC, _) => SOME {name = toString t, sizes = SizesPPC.sizes,
                                  endianShift = EndianLittle.shift}
          | (PowerPC64, _) => SOME {name = toString t, 
                                    sizes = SizesPowerPC64.sizes,
                                    endianShift = EndianLittle.shift}
          | (X86, _) => SOME {name = toString t, sizes = SizesX86.sizes,
                              endianShift = EndianLittle.shift}
          | _ => NONE
   end

val target = control {name = "target",
                      default = Target.make Target.host,
                      toString = Option.toString (fn {name, ...} => name)}

val weight = control {name = "weight",
                      default = {heavy = true, light = true},
                      toString = fn {heavy, light} =>
                      concat ["{heavy = ", Bool.toString heavy,
                              ", light = ", Bool.toString light, "}"]}

val width = control {name = "width",
                     default = 75,
                     toString = Int.toString}

val defaults = setDefaults
val _ = defaults ()

end
