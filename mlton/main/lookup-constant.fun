(* Copyright (C) 2010-2011,2013-2014 Matthew Fluet.
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
       ("MLton_FFI_numExports", fn () => int (Ffi.numExports ())),
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

val gcFields =
   [
    "atomicState",
    "currentThread",
    "sourceMaps.curSourceSeqsIndex",
    "exnStack",
    "frontier",
    "generationalMaps.cardMapAbsolute",
    "limit",
    "limitPlusSlop",
    "maxFrameSize",
    "signalsInfo.signalIsPending",
    "stackBottom",
    "stackLimit",
    "stackTop"
    ]

val gcFieldsOffsets =
   List.map (gcFields, fn s =>
             {name = s ^ "_Offset",
              value = concat ["(", Ffi.CType.toString Ffi.CType.Word32 ,")",
                              "(offsetof (struct GC_state, ", s, "))"],
              ty = ConstType.Word WordSize.word32})
val gcFieldsSizes =
   List.map (gcFields, fn s =>
             {name = s ^ "_Size",
              value = concat ["(", Ffi.CType.toString Ffi.CType.Word32 ,")",
                              "(sizeof (gcState.", s, "))"],
              ty = ConstType.Word WordSize.word32})

fun build (constants, out) =
   let
      val constants =
         List.fold
         (constants, gcFieldsSizes @ gcFieldsOffsets, fn ((name, ty), ac) =>
          if List.exists (buildConstants, fn (name', _) => name = name')
             then ac
          else {name = name, value = name, ty = ty} :: ac)
   in
      List.foreach
      (List.concat
       [["#define MLTON_GC_INTERNAL_TYPES",
         "#include \"platform.h\"",
         "struct GC_state gcState;",
         "",
         "int main (int argc, char **argv) {"],
        List.revMap
        (constants, fn {name, value, ty} =>
         let
            val (format, value) =
               case ty of
                  Bool => ("%s", concat [value, "? \"true\" : \"false\""])
                | Real _ => ("%.20f", value)
                | String => ("%s", value)
                | Word ws => 
                     (case WordSize.prim (WordSize.roundUpToPrim ws) of
                         WordSize.W8 => "%\"PRIu8\""
                       | WordSize.W16 => "%\"PRIu16\""
                       | WordSize.W32 => "%\"PRIu32\""
                       | WordSize.W64 => "%\"PRIu64\"",
                      value)
         in
            concat ["fprintf (stdout, \"", name, " = ", format, "\\n\", ",
                    value, ");"]
         end),
        ["return 0;}"]],
       fn l => (Out.output (out, l); Out.newline out))
   end

fun load (ins: In.t, commandLineConstants)
   : {default: string option, name: string} * ConstType.t -> Const.t =
   let
      val table: {hash: word, name: string, value: string} HashSet.t =
         HashSet.new {hash = #hash}
      fun add {name, value} =
         let
            val hash = String.hash name
            val _ = 
               HashSet.lookupOrInsert
               (table, hash,
                fn {name = name', ...} => name = name',
                fn () => {hash = hash, name = name, value = value})
         in
            ()
         end
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
            val {value, ...} =
               let
                  val hash = String.hash name
               in
                  HashSet.lookupOrInsert
                  (table, hash,
                   fn {name = name', ...} => name = name',
                   fn () =>
                   case default of
                      NONE => Error.bug 
                              (concat ["LookupConstants.load.lookupConstant: ",
                                       "constant not found: ", 
                                       name])
                    | SOME value =>
                         {hash = hash,
                          name = name,
                          value = value})
               end
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
