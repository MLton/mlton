(* Copyright (C) 2010-2011,2013-2014,2019-2020 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor LookupConstant (S: LOOKUP_CONSTANT_STRUCTS): LOOKUP_CONSTANT = 
struct

open S
local
   open Const
in
   structure RealX = RealX
   structure WordX = WordX
end
structure WordSize = WordX.WordSize

val buildConstants: (string * (unit -> string)) list =
   let
      val bool = Bool.toString
      val int = Int.toString
      open Control
   in
      [("MLton_Align_align", fn () => int (case !align of
                                              Align4 => 4
                                            | Align8 => 8)),
       ("MLton_Codegen_codegen", fn () => int (case !codegen of
                                                  CCodegen => 0
                                                | X86Codegen => 1
                                                | AMD64Codegen => 2
                                                | LLVMCodegen => 3)),
       ("MLton_FFI_numExports", fn () => int (!numExports)),
       ("MLton_Platform_Format", fn () => case !format of
                                             Archive => "archive"
                                           | Executable => "executable"
                                           | LibArchive => "libarchive"
                                           | Library => "library"),
       ("MLton_Profile_isOn", fn () => bool (case !profile of
                                                ProfileNone => false
                                              | ProfileCallStack => false
                                              | ProfileDrop => false
                                              | ProfileLabel => false
                                              | _ => true))]
   end

datatype z = datatype ConstType.t

fun load (ins: In.t, commandLineConstants)
   : {default: string option, name: string} * ConstType.t -> Const.t =
   let
      val table: (string, string) HashTable.t =
         HashTable.new {hash = String.hash, equals = String.equals}
      fun add {name, value} =
         (ignore o HashTable.lookupOrInsert)
         (table, name, fn () => value)
      val () =
         List.foreach (buildConstants, fn (name, f) =>
                       add {name = name, value = f ()})
      val () =
         List.foreach
         (commandLineConstants, fn {name, value} =>
          let
          in
             add {name = name, value = value}
          end)
      val _ = 
         In.foreachLine
         (ins, fn l =>
          case String.tokens (l, Char.isSpace) of
             [name, "=", value] => add {name = name, value = value}
           | _ => Error.bug 
                  (concat ["LookupConstants.load: strange constants line: ", l]))
      fun lookupConstant ({default, name}, ty: ConstType.t): Const.t =
         let
            val value =
               HashTable.lookupOrInsert
               (table, name,
                fn () =>
                case default of
                   SOME value => value
                 | NONE => Error.bug
                           (concat ["LookupConstants.load.lookupConstant: ",
                                    "constant not found: ",
                                    name]))
            fun error (t: string) =
               Error.bug (concat ["LookupConstants.load.lookupConstant: ",
                                  "constant ", name, " expects a ", t,
                                  " but got ", value, "."])
         in
            case ty of
               Bool =>
                  (case Bool.fromString value of
                      NONE => error "bool"
                    | SOME b => Const.Word (WordX.fromIntInf (if b then 1 else 0, WordSize.bool)))
             | Real rs =>
                  (case RealX.make (value, rs) of
                      NONE => error "real"
                    | SOME r => Const.Real r)
             | String => Const.string value
             | Word ws =>
                  (case IntInf.fromString value of
                      NONE => error "word"
                    | SOME i => Const.Word (WordX.fromIntInf (i, ws)))
         end
   in
      lookupConstant
   end

end
