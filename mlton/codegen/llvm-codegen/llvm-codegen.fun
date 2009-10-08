(* Copyright (C) 2009 Wesley W. Terpstra.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor LLVMCodegen(S: LLVM_CODEGEN_STRUCTS): LLVM_CODEGEN =
struct

open S
      
local
   open Rssa
in
   structure Block = Block
   structure CFunction = CFunction
   structure Func = Func
   structure Function = Function
   structure CType = CType
(*
   structure Kind = Kind
   structure Label = Label
*)
   structure Operand = Operand
   structure ObjectType = ObjectType
   structure ObjptrTycon = ObjptrTycon
   structure Prim = Prim
   structure Program = Program
(*
   structure RealX = RealX
   structure Runtime = Runtime
   structure Scale = Scale
*)
   structure Statement = Statement
(*
   structure Switch = Switch
*)
   structure Transfer = Transfer
   structure Type = Type
(*
   structure WordSize = WordSize
   structure WordX = WordX
*)
end

structure RealSize = Prim.RealSize

fun implementsPrim p =
   let
      datatype z = datatype Prim.Name.t
   in
      case Prim.name p of
         CPointer_add => true
       | CPointer_diff => true
       | CPointer_equal => true
       | CPointer_fromWord => true
       | CPointer_lt => true
       | CPointer_sub => true
       | CPointer_toWord => true
       | FFI_Symbol _ => true
(*
       | Real_Math_acos _ => true
       | Real_Math_asin _ => true
       | Real_Math_atan _ => true
       | Real_Math_atan2 _ => true
*)
       | Real_Math_cos _ => true
       | Real_Math_exp _ => true
(*
       | Real_Math_ln _ => true
       | Real_Math_log10 _ => true
*)
       | Real_Math_sin _ => true
       | Real_Math_sqrt _ => true
(*
       | Real_Math_tan _ => true
       | Real_abs _ => true
*)
       | Real_add _ => true
       | Real_castToWord _ => true
       | Real_div _ => true
       | Real_equal _ => true
(*
       | Real_ldexp _ => false
*)
       | Real_le _ => true
       | Real_lt _ => true
       | Real_mul _ => true
(*
       | Real_muladd _ => false
       | Real_mulsub _ => false
*)
       | Real_neg _ => true
(*
       | Real_qequal _ => false
*)
       | Real_rndToReal _ => true
       | Real_rndToWord _ => true
(*
       | Real_round _ => true
*)
       | Real_sub _ => true
       | Word_add _ => true
       | Word_addCheck _ => true
       | Word_andb _ => true
       | Word_castToReal _ => true
       | Word_equal _ => true
       | Word_extdToWord _ => true
       | Word_lshift _ => true
       | Word_lt _ => true
       | Word_mul _ => true
       | Word_mulCheck _ => true
       | Word_neg _ => true
       | Word_negCheck _ => true
       | Word_notb _ => true
       | Word_orb _ => true
       | Word_quot _ => true
       | Word_rem _ => true
       | Word_rndToReal _ => true
(*
       | Word_rol _ => true
       | Word_ror _ => true
*)
       | Word_rshift _ => true
       | Word_sub _ => true
       | Word_subCheck _ => true
       | Word_xorb _ => true
       | _ => false
   end

(*
structure RealX =
   struct
      open RealX

      fun fmtLLVM (r: t): string =
         let
            (* LLVM uses 64-bit hexadecimal for floats and doubles *)
            val r = 
               case Real64.fromString (toString r) of
                  SOME r => r
                | NONE => Error.bug "LLVMCodegen.RealX.fmtLLVM: bad real"
            val bytes = Array.tabulate (8, fn _ => 0w0)
            val () = Pervasive.PackReal64Little.update (bytes, 0, r)
            val w = Pervasive.PackWord64Little.subArr (bytes, 0)
         in
            "0x" ^ Pervasive.Word64.toString w
         end
   end
*)

structure CType =
   struct
      datatype t = datatype CType.t
      
      val fmtLLVM = fn
         CPointer => "i8*"
       | Int8  => "i8"
       | Int16  => "i16"
       | Int32 => "i32"
       | Int64 => "i64"
       | Objptr => "%obj"
       | Real32 => "float"
       | Real64 => "double"
       | Word8  => "i8"
       | Word16 => "i16"
       | Word32 => "i32"
       | Word64 => "i64"

      open CType
   end

structure Type =
   struct
      fun fmtLLVM ty = 
         if Type.isObjptr ty then 
            case Type.deObjptr ty of
               NONE => "%obj"
             | SOME oty => 
                  concat [ "%opt_", Int.toString (ObjptrTycon.index oty), "*"]
         else
            CType.fmtLLVM (Type.toCType ty)
      
      open Type
   end

fun declareType print (i, ty) =
   let
      datatype z = datatype ObjectType.t
      val cpointer = Type.fmtLLVM (Type.cpointer ())
      val layout =
         case ty of
            Array {elt, ... } => [ "[0 x ", Type.fmtLLVM elt, "] ; array"]
          | Stack => ["opaque ; stack"]
          | Weak NONE => ["{ ", cpointer, " } ; weak "]
          | Weak (SOME ty) => [ "{ ", cpointer, ", ", Type.fmtLLVM ty, " } ; weak"]
          | Normal {ty, ... } =>
               case Type.deSeq ty of
                  NONE => ["{ ", Type.fmtLLVM ty, " } ; simple"]
                | SOME v =>
                    "{ " ::
                    List.separate (Vector.toListMap (v, Type.fmtLLVM), ", ") @
                    [" } ; sequence"]
      val layout = 
         "%opt_" :: Int.toString i :: " = type " :: layout
      val () = print (concat layout)
   in
      print "\n"
   end

fun declareFFI print f =
   let
      val { blocks, ... } = Function.dest f
      
      val seen = String.memoize (fn _ => ref false)
      fun doit (name: string, declare: unit -> string): unit =
         let
            val r = seen name
         in
            if !r
               then ()
            else (r := true; print (declare ()))
         end
   in
      Vector.foreach
      (blocks, fn Block.T {statements, transfer, ...} =>
       let
          datatype z = datatype CFunction.SymbolScope.t
          fun windows s = 
             case !Control.Target.os of
                Control.Target.Cygwin => s
              | Control.Target.MinGW => s
              | _ => ""
          val _ =
             Vector.foreach
             (statements, fn s =>
              case s of
                 Statement.PrimApp {prim, ...} =>
                    (case Prim.name prim of
                        Prim.Name.FFI_Symbol {name, cty, symbolScope} =>
                           doit
                           (name, fn () =>
                            concat ["@", name,
                                    " = external ",
                                    case symbolScope of
                                       External => windows "dllimport"
                                     | Private => "hidden"
                                     | Public => "protected",
                                    " global ",
                                    case cty of
                                       SOME x => CType.fmtLLVM x
                                     | NONE => "opaque",
                                    "\n"])
                      | _ => ())
               | _ => ())
          val _ =
             case transfer of
                Transfer.CCall {func, ...} =>
                   let
                      datatype z = datatype CFunction.Target.t
                      val CFunction.T {target, prototype=(args, ret), 
                                       symbolScope, ... } = func
                      val args = Vector.map (args, CType.fmtLLVM)
                      val args = Vector.toList args
                      val args = concat (List.separate (args, ","))
                      val symbolScope =
                         case symbolScope of
                            External => windows "dllimport"
                          | Private => "hidden"
                          | Public => "protected"
                      val ret =
                         case ret of
                            SOME x => CType.fmtLLVM x
                          | NONE => "void"
                   in
                      case target of
                         Direct name =>
                            doit (name, fn () =>
                                  concat [
                                     "declare ",
                                     symbolScope,
                                     " ccc ",
                                     ret,
                                     " @",
                                     name,
                                     "(",
                                     args,
                                     ")\n"])
                       | Indirect => ()
                   end
              | _ => ()
       in
          ()
       end)
   end

fun declareCalls print f =
   let
      val { blocks, ... } = Function.dest f
      val seen = String.memoize (fn _ => ref false)
      fun doit (name: string, declare: unit -> string): unit =
         let
            val r = seen name
         in
            if !r
               then ()
            else (r := true; print (declare ()))
         end
   in
      Vector.foreach
      (blocks, 
       fn Block.T { transfer=Transfer.Call { func, args, ... }, ...} =>
       doit (Func.toString func, fn () =>
       let
          val cpointer = Type.fmtLLVM (Type.cpointer ())
          val args = Vector.toList (Vector.map (args, Type.fmtLLVM o Operand.ty))
          val args = concat (List.separate (args, ","))
          val layout =
             ["declare hidden fastcc ", cpointer, " @", Func.toString func,
              "(", cpointer, ", ", args, ")\n" ]
       in
          concat layout
       end)
       | _ => ())
   end

fun output {program, outputLL} = 
   let
      val Program.T { objectTypes, functions, main, ... } = program
      val { done, print, file=_ } = outputLL ()

      fun arithHelpers cTys helpers =
         let
            fun decl h cTy =
               let
                  val ty = CType.fmtLLVM cTy
                  val layout =
                     ["declare {", ty, ", i1} @llvm.", h, ".with.overflow.", 
                      ty, "(", ty, ", ", ty, ")\n"]
               in
                  print (concat layout)
               end
         in
            List.foreach (helpers, fn h => List.foreach (cTys, decl h))
         end
      
      fun floatHelpers tys helpers =
         let
            fun decl h ty =
               let
                  val bits = Bits.toString (Type.width ty)
                  val ty = Type.fmtLLVM ty
                  val layout =
                     ["declare ", ty, " @llvm.", h, ".f", bits, "(",
                      ty, ")\n"]
               in
                  print (concat layout)
               end
         in
            List.foreach (helpers, fn h => List.foreach (tys, decl h))
         end
      
      val () = 
         arithHelpers
         [CType.Int8, CType.Int16, CType.Int32, CType.Int64]
         ["sadd", "uadd", "ssub", "usub", "smul", "umul"]
      val () = print "\n"
      val () = 
         floatHelpers 
         [Type.real RealSize.R32, Type.real RealSize.R64 ]
         ["sin", "cos", "sqrt", "pow"]
      val () = print "\n"
      val () = print "@gcState = external hidden global i8\n"
      val () = print "\n"
      val () = print "%obj = type i8*\n"
      val () = Vector.foreachi (objectTypes, declareType print)
      val () = print "\n"
      val () = List.foreach (functions, declareFFI print)
      val () = declareFFI print main
      val () = print "\n"
      val () = List.foreach (functions, declareCalls print)
      val () = declareCalls print main
   in
      done ()
   end

end
