(* Copyright (C) 2019-2022 Matthew Fluet.
 * Copyright (C) 2013-2014 Matthew Fluet, Brian Leibig.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor LLVMCodegen(S: LLVM_CODEGEN_STRUCTS): LLVM_CODEGEN =
struct

structure AList = AppendList

open S

open Machine

structure LLVM =
   struct
      fun escape s =
         let
            fun needsEscape c = Char.isCntrl c orelse Char.equals (c, #"\\") orelse Char.equals (c, Char.dquote)
         in
            if String.exists (s, needsEscape)
               then String.translate
                    (s, fn c =>
                     if needsEscape c
                        then let
                                val hex = Int.format (Char.ord c, StringCvt.HEX)
                             in
                                if String.length hex < 2
                                   then "\\0" ^ hex
                                   else "\\" ^ hex
                             end
                        else Char.toString c)
               else s
         end
      structure Type =
         struct
            datatype t =
               Array of int * t
             | Function of t list * t
             | Label
             | Pointer of t
             | Real of RealSize.t
             | Struct of bool * t list
             | Void
             | Word of WordSize.t

            fun equals (ty1, ty2) =
               case (ty1, ty2) of
                  (Array (n1, ty1), Array (n2, ty2)) =>
                     Int.equals (n1, n2) andalso equals (ty1, ty2)
                | (Function (atys1, rty1), Function (atys2, rty2)) =>
                     equalss (atys1, atys2) andalso equals (rty1, rty2)
                | (Label, Label) => true
                | (Pointer ty1, Pointer ty2) => equals (ty1, ty2)
                | (Real rs1, Real rs2) => RealSize.equals (rs1, rs2)
                | (Struct (b1, tys1), Struct (b2, tys2)) =>
                     Bool.equals (b1, b2) andalso equalss (tys1, tys2)
                | (Void, Void) => true
                | (Word ws1, Word ws2) => WordSize.equals (ws1, ws2)
                | _ => false
            and equalss (tys1, tys2) = List.equals (tys1, tys2, equals)

            local
               val array = Random.word ()
               val function = Random.word ()
               val label = Random.word ()
               val pointer = Random.word ()
               val real = Random.word ()
               val str = Random.word ()
               val void = Random.word ()
               val word = Random.word ()
            in
               fun hash ty =
                  case ty of
                     Array (n, ty) => Hash.combine3 (array, Word.fromInt n, hash ty)
                   | Function (atys, rty) => Hash.combine3 (function, Hash.listMap (atys, hash), hash rty)
                   | Label => label
                   | Pointer ty => Hash.combine (pointer, hash ty)
                   | Real rs => Hash.combine (real, RealSize.hash rs)
                   | Struct (b, tys) => Hash.combine3 (str, Bool.hash b, Hash.listMap (tys, hash))
                   | Void => void
                   | Word ws => Hash.combine (word, WordSize.hash ws)
            end

            val bool = Word (WordSize.fromBits Bits.one)
            val word8 = Word WordSize.word8
            val word16 = Word WordSize.word16
            val word32 = Word WordSize.word32
            val word64 = Word WordSize.word64

            fun toString ty =
               case ty of
                  Array (n, ty) =>
                     concat ["[", Int.toString n, " x ", toString ty, "]"]
                | Function (args, res) =>
                     concat [toString res, "(",
                             String.concatWith (List.map (args, toString), ","),
                             ")"]
                | Label => "label"
                | Pointer ty => concat [toString ty, "*"]
                | Struct (packed, tys) =>
                     let
                        val (l,r) = if packed then ("<{", "}>") else ("{","}")
                     in
                        concat [l,
                                String.concatWith (List.map (tys, toString), ","),
                                r]
                     end
                | Real rs => (case rs of
                                 RealSize.R32 => "float"
                               | RealSize.R64 => "double")
                | Word ws => concat ["i", WordSize.toString ws]
                | Void => "void"

            fun dePointer ty =
               case ty of
                  Pointer ty => ty
                | _ => Error.bug ("LLVMCodegen.LLVM.Type.dePointer: " ^ toString ty)

            val blockaddress = Pointer word8
         end
      structure ParamAttr =
         struct
            datatype t =
               SignExt | ZeroExt
            fun toString attr =
               case attr of
                  SignExt => "signext"
                | ZeroExt => "zeroext"
         end
      structure ParamAttrs =
         struct
            datatype t = T of ParamAttr.t list
            fun toString (T attrs) =
               String.concatWith (List.map (attrs, ParamAttr.toString), " ")
            val empty = T []
            val signext = T [ParamAttr.SignExt]
            val zeroext = T [ParamAttr.ZeroExt]
            fun isEmpty (T attrs) = List.isEmpty attrs
         end
      structure Param =
         struct
            type t = Type.t * ParamAttrs.t
            fun fromCType ct =
               case ct of
                  CType.CPointer => (Type.Pointer Type.word8, ParamAttrs.empty)
                | CType.Int8 => (Type.word8, ParamAttrs.signext)
                | CType.Int16 => (Type.word16, ParamAttrs.signext)
                | CType.Int32 => (Type.word32, ParamAttrs.signext)
                | CType.Int64 => (Type.word64, ParamAttrs.signext)
                | CType.Objptr => (Type.Pointer (Type.Word WordSize.word8), ParamAttrs.empty)
                | CType.Real32 => (Type.Real RealSize.R32, ParamAttrs.empty)
                | CType.Real64 => (Type.Real RealSize.R64, ParamAttrs.empty)
                | CType.Word8 => (Type.word8, ParamAttrs.zeroext)
                | CType.Word16 => (Type.word16, ParamAttrs.zeroext)
                | CType.Word32 => (Type.word32, ParamAttrs.zeroext)
                | CType.Word64 => (Type.word64, ParamAttrs.zeroext)
            val cpointer = fromCType CType.CPointer
            val void = (Type.Void, ParamAttrs.empty)

            fun argToString (ty, attrs) =
               if ParamAttrs.isEmpty attrs
                  then Type.toString ty
                  else concat [Type.toString ty, " ",
                               ParamAttrs.toString attrs]
            fun resToString (ty, attrs) =
               if ParamAttrs.isEmpty attrs
                  then Type.toString ty
                  else concat [ParamAttrs.toString attrs, " ",
                               Type.toString ty]
         end
      structure Type =
         struct
            open Type

            val fromCType = #1 o Param.fromCType

            val cpointer = fromCType CType.CPointer

            val uintptr = Promise.lazy (Word o WordSize.cpointer)
         end
      structure Param =
         struct
            open Param
            val uintptr = Promise.lazy (fn () => (Type.uintptr (), ParamAttrs.zeroext))
         end
      structure Formal =
         struct
            type t = string * Param.t
            fun toString (name, param) =
               concat [Param.argToString param, " ", name]
         end
      structure Actual =
         struct
            (* type t = string * Param.t *)
            fun toString (x, param) =
               concat [Param.argToString param, " ", x]
            fun toValue (x, (ty, _)) = (x, ty)
         end
      structure Value =
         struct
            type t = string * Type.t

            fun equals ((s1, ty1), (s2, ty2)) = String.equals (s1, s2) andalso Type.equals (ty1, ty2)
            fun hash (s, ty) = Hash.combine (String.hash s, Type.hash ty)
            fun toString (s, ty) = concat [Type.toString ty, " ", s]

            fun fnptr (s, args, res) = (s, Type.Pointer (Type.Function (args, res)))
            fun globptr (s, ty) = (s, Type.Pointer ty)
            fun label' s = ("%" ^ s, Type.Label)
            fun label l = label' (Label.toString l)
            val null = ("null", Type.Pointer Type.word8)
            fun real r =
               let
                  val s = RealX.toString (r, {suffix = false})
                  val s =
                     case s of
                        "~inf" => "0xFFF0000000000000"
                      | "inf" => "0x7FF0000000000000"
                      | "nan" => "0xFFF8000000000000"
                      | _ => String.translate (s, fn #"~" => "-" | c => String.fromChar c)
               in
                  (s, Type.Real (RealX.size r))
               end
            fun fnegZero rs = ("-0.0", Type.Real rs)
            (* fun undef ty = ("undef", ty) *)
            fun word w =
               (IntInf.toString (WordX.toIntInf w),
                Type.Word (WordX.size w))
            fun zero ws = word (WordX.zero ws)
            fun negOne ws = word (WordX.fromIntInf (~1, ws))
         end
      structure Instr =
         struct
            type t = string AList.t

            (* terminator *)
            fun br {test = (test, testTy), truee = (truee, trueeTy), falsee = (falsee, falseeTy)} =
               AList.fromList ["br ", Type.toString testTy, " ", test,
                               ", ", Type.toString trueeTy, " ", truee,
                               ", ", Type.toString falseeTy, " ", falsee]
            fun indirectbr {addr = (addr, addrTy), labels} =
               AList.append
               (AList.fromList ["indirectbr ", Type.toString addrTy, " ", addr, ", ["],
                if List.length labels > 3
                   then AList.append
                        ((AList.appends o List.mapi)
                         (labels, fn (i, (label, labelTy)) =>
                          AList.fromList [if i > 0 then ",\n\t\t" else "\n\t\t",
                                          Type.toString labelTy, " ", label]),
                         AList.single "\n\t]")
                   else AList.append
                        ((AList.appends o List.mapi)
                         (labels, fn (i, (label, labelTy)) =>
                          AList.fromList [if i > 0 then ", " else "",
                                          Type.toString labelTy,  " ", label]),
                         AList.single "]"))
            fun jmp (label, labelTy) = AList.fromList ["br ", Type.toString labelTy, " ", label]
            fun ret (res, resTy) = AList.fromList ["ret ", Type.toString resTy, " ", res]
            fun unreachable () = AList.single "unreachable"
            fun switch {value = (value, valueTy), default = (default, defaultTy), table} =
               AList.append
               (AList.fromList ["switch ",
                                Type.toString valueTy, " ", value, ", ",
                                Type.toString defaultTy, " ", default, " ["],
                if List.length table > 2
                   then AList.appends
                        [AList.single "\n",
                         (AList.appends o List.map)
                         (table, fn ((index, indexTy), (label, labelTy)) =>
                          AList.fromList ["\t\t",
                                          Type.toString indexTy, " ", index, ", ",
                                          Type.toString labelTy, " ", label, "\n"]),
                         AList.single "\t]"]
                   else AList.append
                        ((AList.appends o List.map)
                         (table, fn ((index, indexTy), (label, labelTy)) =>
                          AList.fromList [" ",
                                          Type.toString indexTy, " ", index, ", ",
                                          Type.toString labelTy, " ", label, " "]),
                         AList.single "]"))

            (* nary *)
            fun naryop {dst = (dst, _), oper = (oper, operTy), args} =
               AList.append
               (AList.fromList [dst, " = ", oper, " ", Type.toString operTy, " "],
                (AList.appends o List.mapi)
                (args, fn (i, (arg, _)) =>
                 AList.fromList [if i > 0 then ", " else "", arg]))

            (* aggregate *)
            fun xval {dst = (dst, _), src = (src, srcTy), args} =
               AList.append
               (AList.fromList [dst, " = extractvalue ", Type.toString srcTy, " ", src],
                (AList.appends o List.map)
                (args, fn arg => AList.fromList [", ", arg]))

            (* memory *)
            fun alloca {dst = (dst, dstTy)} =
               AList.fromList [dst, " = alloca ", Type.toString (Type.dePointer dstTy)]
            fun gep {dst = (dst, _), src = (src, srcTy), args} =
               AList.append
               (AList.fromList [dst, " = getelementptr inbounds ",
                                Type.toString (Type.dePointer srcTy),
                                ", ", Type.toString srcTy, " ", src],
                (AList.appends o List.map)
                (args, fn (arg, argTy) => AList.fromList [", ", Type.toString argTy, " ", arg]))
            fun load' {dst = (dst, dstTy), src = (src, srcTy), volatile} =
               AList.fromList [dst, " = load ", if volatile then "volatile " else "",
                               Type.toString dstTy, ", ", Type.toString srcTy, " ", src]
            fun load {dst, src} = load' {dst = dst, src = src, volatile = false}
            fun store' {dst = (dst, dstTy), src = (src, srcTy), volatile} =
               AList.fromList ["store ", if volatile then "volatile " else "",
                               Type.toString srcTy, " ", src,  ", ", Type.toString dstTy, " ", dst]
            fun store {dst, src} = store' {dst = dst, src = src, volatile = false}

            (* conversion *)
            fun convop {dst = (dst, dstTy), oper, src = (src, srcTy)} =
               AList.fromList [dst, " = ", oper, " ", Type.toString srcTy, " ", src, " to ", Type.toString dstTy]
            local
               fun mk oper {dst, src} =
                  convop {dst = dst, oper = oper, src = src}
            in
               val trunc = mk "trunc"
               val zext = mk "zext"
               val sext = mk "sext"
               val fptrunc = mk "fptrunc"
               val fpext = mk "fpext"
               val fptoui = mk "fptoui"
               val fptosi = mk "fptosi"
               val uitofp = mk "uitofp"
               val sitofp = mk "sitofp"
               val ptrtoint = mk "ptrtoint"
               val inttoptr = mk "inttoptr"
               val bitcast = mk "bitcast"
            end
            fun resize {dst as (_, dstTy), src as (_, srcTy), signed} =
               case (srcTy, dstTy) of
                  (Type.Word ws, Type.Word wd) =>
                     (case WordSize.compare (ws, wd) of
                         LESS => if signed then sext else zext
                       | EQUAL => bitcast
                       | GREATER => trunc) {dst = dst, src = src}
                | _ => Error.bug "LLVMCodegen.LLVM.Instr.resize"
            fun fpresize {dst as (_, dstTy), src as (_, srcTy)} =
               case (srcTy, dstTy) of
                  (Type.Real rs, Type.Real rd) =>
                     (case RealSize.compare (rs, rd) of
                         LESS => fpext
                       | EQUAL => bitcast
                       | GREATER => fptrunc) {dst = dst, src = src}
                | _ => Error.bug "LLVMCodegen.LLVM.Instr.fpresize"
            fun cast (arg as {dst = (_, dstTy), src = (_, srcTy)}) =
               (case (srcTy, dstTy) of
                   (Type.Pointer _, Type.Word _) => ptrtoint
                 | (Type.Word _, Type.Pointer _) => inttoptr
                 | _ => bitcast) arg

            (* other *)
            fun call {dst = (dst, (dstTy, dstAttrs)), tail, cconv, fnptr = (fnptr, _), args} =
               AList.appends
               [case dstTy of Type.Void => AList.empty | _ => AList.fromList [dst, " = "],
                case tail of NONE => AList.empty | SOME tail => AList.fromList [tail, " "],
                AList.single "call ",
                case cconv of NONE => AList.empty | SOME cconv => AList.fromList [cconv, " "],
                AList.fromList [Param.resToString (dstTy, dstAttrs), " ", fnptr, "("],
                (AList.appends o List.mapi)
                (args, fn (i, arg) =>
                 AList.fromList [if i > 0 then ", " else "", Actual.toString arg]),
                AList.single ")"]
            fun call' {dst = (dst, dstTy), tail, cconv, fnptr, args} =
               call {dst = (dst, (dstTy, ParamAttrs.empty)),
                     tail = tail,
                     cconv = cconv,
                     fnptr = fnptr,
                     args = List.map (args, fn (arg, argTy) =>
                                      (arg, (argTy, ParamAttrs.empty)))}

            fun addMetaData (i, md) =
               case md of
                  NONE => i
                | SOME md => AList.append (i, AList.fromList [", ", md])
         end
      structure MetaData =
         struct
            structure Id =
               struct
                  type t = string
                  val equals = String.equals
                  val hash = String.hash
                  fun toString id = id
               end
            structure Value =
               struct
                  datatype t = Id of Id.t | Node of t list | String of String.t | Value of Value.t
                  fun equals (v1, v2) =
                     case (v1, v2) of
                        (Id i1, Id i2) => Id.equals (i1, i2)
                      | (Node vs1, Node vs2) => List.equals (vs1, vs2, equals)
                      | (String s1, String s2) => String.equals (s1, s2)
                      | (Value v1, Value v2) => Value.equals (v1, v2)
                      | _ => false
                  local
                     val id = Random.word ()
                     val node = Random.word ()
                     val string = Random.word ()
                     val value = Random.word ()
                  in
                     fun hash v =
                        case v of
                           Id i => Hash.combine (id, Id.hash i)
                         | Node vs => Hash.combine (node, Hash.listMap (vs, hash))
                         | String s => Hash.combine (string, String.hash s)
                         | Value v => Hash.combine (value, Value.hash v)
                  end
                  fun toString v =
                     case v of
                        Id i => Id.toString i
                      | Node vs => concat ["!{", String.concatWith (List.map (vs, toString), ", "), "}"]
                      | String s => concat ["!\"", escape s, "\""]
                      | Value v => Value.toString v
               end
            val id = Value.Id
            val string = Value.String
            val value = Value.Value
            datatype t = T of unit ref option * Value.t
            fun node vs = T (NONE, Value.Node vs)
            fun equals (T (xo1, v1), T (xo2, v2)) =
               Option.equals (xo1, xo2, Ref.equals) andalso Value.equals (v1, v2)
            local
               val none = Random.word ()
               val some = Random.word ()
            in
               fun hash (T (xo, v)) =
                  Hash.combine (case xo of NONE => none | SOME _ => some, Value.hash v)
            end
            fun toString (T (xo, v)) =
               case xo of
                  NONE => Value.toString v
                | SOME _ => concat ["distinct ", Value.toString v]
         end
      structure ModuleContext =
         struct
            datatype t = T of {fnDecls: (string, {argParams: Param.t list,
                                                  resParam: Param.t,
                                                  vis: string option}) HashTable.t,
                               fnDefns: (string, unit) HashTable.t,
                               globDecls: (string, {const: bool,
                                                    ty: Type.t,
                                                    vis: string option}) HashTable.t,
                               metaData: (MetaData.t, MetaData.Id.t) HashTable.t}
            fun new () = T {fnDecls = HashTable.new {equals = String.equals, hash = String.hash},
                            fnDefns = HashTable.new {equals = String.equals, hash = String.hash},
                            globDecls = HashTable.new {equals = String.equals, hash = String.hash},
                            metaData = HashTable.new {equals = MetaData.equals, hash = MetaData.hash}}
            fun emit (T {fnDecls, fnDefns, globDecls, metaData}, print) =
               let
                  val empty = ref true
                  val _ =
                     HashTable.foreachi
                     (globDecls, fn (name, {const, ty, vis}) =>
                      (empty := false
                       ; print name
                       ; print " = external "
                       ; Option.app (vis, fn vis => (print vis; print " "))
                       ; print (if const then "constant " else "global ")
                       ; print (Type.toString ty)
                       ; print "\n"))
                  val _ = if !empty then () else print "\n"
                  val empty = ref true
                  val _ =
                     HashTable.foreachi
                     (fnDecls, fn (name, {argParams, resParam, vis}) =>
                      case HashTable.peek (fnDefns, name) of
                         NONE =>
                            (empty := false
                             ; print "declare"
                             ; Option.app (vis, fn vis => (print " "; print vis))
                             ; print " "
                             ; print (Param.resToString resParam)
                             ; print " "
                             ; print name
                             ; print "("
                             ; List.foreachi (argParams, fn (i, argParam) =>
                                              (if i > 0 then print ", " else ()
                                               ; print (Param.argToString argParam)))
                             ; print ")\n")
                       | SOME _ => ())
                  val _ = if !empty then () else print "\n"
                  val empty = ref true
                  val _ =
                     HashTable.foreachi
                     (metaData, fn (md, id) =>
                      (empty := false
                       ; print (MetaData.Id.toString id)
                       ; print " = "
                       ; print (MetaData.toString md)
                       ; print "\n"))
                  val _ = if !empty then () else print "\n"
               in
                  ()
               end
            fun addFnDecl (T {fnDecls, ...}, name, argParams_resParam_vis as {argParams, resParam, ...}) =
               ((ignore o HashTable.insertIfNew)
                (fnDecls, name, fn () => argParams_resParam_vis, ignore)
                ; Value.fnptr (name, List.map (argParams, #1), #1 resParam))
            fun addFnDefn (T {fnDefns, ...}, name) =
               (ignore o HashTable.insertIfNew)
               (fnDefns, name, fn () => (), ignore)
            fun addGlobDecl (T {globDecls, ...}, name, const_ty_vis as {ty, ...}) =
               ((ignore o HashTable.insertIfNew)
                (globDecls, name, fn () => const_ty_vis, ignore)
                ; Value.globptr (name, ty))
            fun addMetaData (T {metaData, ...}, md) =
               HashTable.lookupOrInsert
               (metaData, md, fn () => "!" ^ Int.toString (HashTable.size metaData))
            fun intrinsic (mc, name, {argTys, resTy}) =
               addFnDecl
               (mc, "@llvm." ^ name,
                {argParams = List.map (argTys, fn argTy => (argTy, ParamAttrs.empty)),
                 resParam = (resTy, ParamAttrs.empty),
                 vis = NONE})
         end
   end

structure ChunkLabel =
   struct
      open ChunkLabel
      val toStringForC = toString
      fun toStringXForC cl = "X" ^ toStringForC cl
      fun toString cl = "@" ^ toStringForC cl
      fun toStringX cl = "@" ^ toStringXForC cl
      fun toString' cl =
         if !Control.llvmCC10
            then toStringX cl
            else toString cl
   end

local
   open Runtime
in
   structure GCField = GCField
end

structure Type =
   struct
      open Type
      val toLLVMType = LLVM.Type.fromCType o toCType
   end

fun primApp (prim: 'a Prim.t): ({args: LLVM.Value.t list,
                                 mc: LLVM.ModuleContext.t,
                                 newTemp: LLVM.Type.t -> LLVM.Value.t,
                                 $ : LLVM.Instr.t -> unit} ->
                                LLVM.Value.t) option =
   let
      open LLVM.Instr
      val nth = List.nth
      fun compare oper {args, mc = _, newTemp, $} =
         let
            val tmp = newTemp LLVM.Type.bool
            val res = newTemp (LLVM.Type.Word WordSize.bool)
            val  _ = $(naryop {dst = tmp, oper = oper, args = args})
            val _ = $(zext {dst = res, src = tmp})
         in
            res
         end
      fun conv (instr, ty) {args, mc = _, newTemp, $} =
         let
            val res = newTemp ty
            val _ = $(instr {dst = res, src = nth (args, 0)})
         in
            res
         end
      fun cpointerAdd {args, mc = _, newTemp, $} =
         let
            val res = newTemp LLVM.Type.cpointer
            val _ = $(gep {dst = res, src = nth (args, 0),
                           args = [nth (args, 1)]})
         in
            res
         end
      fun cpointerCompare cond =
         compare ("icmp " ^ cond, LLVM.Type.cpointer)
      fun realCompare (cond, rs) = compare ("fcmp " ^ cond, LLVM.Type.Real rs)
      fun realMath' (oper, rs, fargs) {args, mc, newTemp, $} =
         let
            val args = fargs args
            val atys = List.map (args, #2)
            val rty = LLVM.Type.Real rs
            val name = concat [oper, ".f", RealSize.toString rs]
            val fnptr = LLVM.ModuleContext.intrinsic (mc, name, {argTys = atys, resTy = rty})
            val res = newTemp rty
            val _ = $(call' {dst = res, tail = NONE, cconv = NONE, fnptr = fnptr, args = args})
         in
            res
         end
      fun realMath (oper, rs) = realMath' (oper, rs, fn args => args)
      fun realNary' (oper, rs, fargs) {args, mc = _, newTemp, $} =
         let
            val args = fargs args
            val ty = LLVM.Type.Real rs
            val res = newTemp ty
            val _ = $(naryop {dst = res, oper = (oper, ty), args = args})
         in
            res
         end
      fun realNary (oper, rs) = realNary' (oper, rs, fn args => args)
      fun wordCompare (cond, ws) = compare ("icmp " ^ cond, LLVM.Type.Word ws)
      fun wordCheckP' (oper, ws, fargs) {args, mc, newTemp, $} =
         let
            val args = fargs args
            val atys = List.map (args, #2)
            val wty = LLVM.Type.Word ws
            val sty = LLVM.Type.Struct (false, [wty, LLVM.Type.bool])
            val name = concat [oper, ".with.overflow.i", WordSize.toString ws]
            val fnptr = LLVM.ModuleContext.intrinsic (mc, name, {argTys = atys, resTy = sty})
            val tmps = newTemp sty
            val tmpb = newTemp LLVM.Type.bool
            val res = newTemp (LLVM.Type.Word WordSize.bool)
            val _ = $(call' {dst = tmps, tail = NONE, cconv = NONE, fnptr = fnptr, args = args})
            val _ = $(xval {dst = tmpb, src = tmps, args = ["1"]})
            val _ = $(zext {dst = res, src = tmpb})
         in
            res
         end
      fun wordCheckP (oper, ws) = wordCheckP' (oper, ws, fn args => args)
      fun wordNary' (oper, ws, fargs) {args, mc = _, newTemp, $} =
         let
            val args = fargs args
            val ty = LLVM.Type.Word ws
            val res = newTemp ty
            val _ = $(naryop {dst = res, oper = (oper, ty), args = args})
         in
            res
         end
      fun wordNary (oper, ws) = wordNary' (oper, ws, fn args => args)
      fun wordRotate (oper, ws) {args, mc, newTemp, $} =
         let
            val wty = LLVM.Type.Word ws
            val atys = [wty, wty, wty]
            val rty = wty
            val name = concat [oper, ".i", WordSize.toString ws]
            val fnptr = LLVM.ModuleContext.intrinsic (mc, name, {argTys = atys, resTy = rty})
            val arg1 = newTemp wty
            val res = newTemp wty
            val _ = $(resize {dst = arg1, src = nth (args, 1), signed = false})
            val _ = $(call' {dst = res, tail = NONE, cconv = NONE, fnptr = fnptr, args = [nth (args, 0), nth (args, 0), arg1]})
         in
            res
         end
      fun wordShift (oper, ws) {args, mc = _, newTemp, $} =
         let
            val ty = LLVM.Type.Word ws
            val arg1 = newTemp ty
            val res = newTemp ty
            val _ = $(resize {dst = arg1, src = nth (args, 1), signed = false})
            val _ = $(naryop {dst = res, oper = (oper, ty), args = [nth (args, 0), arg1]})
         in
            res
         end
      datatype z = datatype Prim.t
   in
      case prim of
         CPointer_add => SOME cpointerAdd
       | CPointer_diff => SOME (fn {args, mc = _, newTemp, $} =>
            let
               val wptr = LLVM.Type.uintptr ()
               val arg0 = newTemp wptr
               val arg1 = newTemp wptr
               val res = newTemp wptr
               val _ = $(ptrtoint {dst = arg0, src = nth (args, 0)})
               val _ = $(ptrtoint {dst = arg1, src = nth (args, 1)})
               val _ = $(naryop {dst = res, oper = ("sub", wptr), args = [arg0, arg1]})
            in
               res
            end)
       | CPointer_equal => SOME (cpointerCompare "eq")
       | CPointer_fromWord => SOME (conv (inttoptr, LLVM.Type.cpointer))
       | CPointer_lt => SOME (cpointerCompare "ult")
       | CPointer_sub => SOME (fn {args, mc, newTemp, $} =>
            let
               fun mk args = {args = args, mc = mc, newTemp = newTemp, $ = $}
               val ws = WordSize.cpointer ()
               val tmp = wordNary ("sub", ws) (mk [LLVM.Value.zero ws, nth (args, 1)])
               val res = cpointerAdd (mk [nth (args, 0), tmp])
            in
               res
            end)
       | CPointer_toWord => SOME (conv (ptrtoint, LLVM.Type.uintptr ()))
       | Real_Math_acos _ => NONE
       | Real_Math_asin _ => NONE
       | Real_Math_atan _ => NONE
       | Real_Math_atan2 _ => NONE
       | Real_Math_cos rs => SOME (realMath ("cos", rs))
       | Real_Math_exp rs => SOME (realMath ("exp", rs))
       | Real_Math_ln rs => SOME (realMath ("log", rs))
       | Real_Math_log10 rs => SOME (realMath ("log10", rs))
       | Real_Math_sin rs => SOME (realMath ("sin", rs))
       | Real_Math_sqrt rs => SOME (realMath ("sqrt", rs))
       | Real_Math_tan _ => NONE
       | Real_abs rs => SOME (realMath ("fabs", rs))
       | Real_add rs => SOME (realNary ("fadd", rs))
       | Real_castToWord (_, ws) => SOME (conv (bitcast, LLVM.Type.Word ws))
       | Real_div rs => SOME (realNary ("fdiv", rs))
       | Real_equal rs => SOME (realCompare ("oeq", rs))
       | Real_ldexp _ => NONE
       | Real_le rs => SOME (realCompare ("ole", rs))
       | Real_lt rs => SOME (realCompare ("olt", rs))
       | Real_mul rs => SOME (realNary ("fmul", rs))
       | Real_muladd rs => SOME (realMath ("fma", rs))
       | Real_mulsub rs => SOME (fn {args, mc, newTemp, $} =>
            let
               fun mk args = {args = args, mc = mc, newTemp = newTemp, $ = $}
               val tmp =
                 if false
                    then realNary ("fneg", rs) (mk [nth (args, 2)])
                    else realNary ("fsub", rs) (mk [LLVM.Value.fnegZero rs, nth (args, 2)])
               val res = realMath ("fma", rs) (mk [nth (args, 0), nth (args, 1), tmp])
            in
               res
            end)
       | Real_neg rs => SOME (fn {args, mc, newTemp, $} =>
            let
               fun mk args = {args = args, mc = mc, newTemp = newTemp, $ = $}
               val res =
                 if false
                    then realNary ("fneg", rs) (mk [nth (args, 0)])
                    else realNary ("fsub", rs) (mk [LLVM.Value.fnegZero rs, nth (args, 0)])
            in
               res
            end)
       | Real_qequal rs => SOME (realCompare ("ueq", rs))
       | Real_rndToReal (_, rs) => SOME (conv (fpresize, LLVM.Type.Real rs))
       | Real_rndToWord (_, ws, {signed}) => SOME (conv (if signed then fptosi else fptoui, LLVM.Type.Word ws))
       | Real_round rs => SOME (realMath ("rint", rs))
       | Real_sub rs => SOME (realNary ("fsub", rs))
       | Thread_returnToC  => NONE
       | Word_add ws => SOME (wordNary ("add", ws))
       | Word_addCheckP (ws, {signed}) => SOME (wordCheckP (if signed then "sadd" else "uadd", ws))
       | Word_andb ws => SOME (wordNary ("and", ws))
       | Word_castToReal (_, rs) => SOME (conv (bitcast, LLVM.Type.Real rs))
       | Word_equal ws => SOME (wordCompare ("eq", ws))
       | Word_extdToWord (_, ws, {signed}) => SOME (conv (fn {dst, src} => resize {dst = dst, src = src, signed = signed}, LLVM.Type.Word ws))
       | Word_lshift ws => SOME (wordShift ("shl", ws))
       | Word_lt (ws, {signed}) => SOME (wordCompare (if signed then "slt" else "ult", ws))
       | Word_mul (ws, _) => SOME (wordNary ("mul", ws))
       | Word_mulCheckP (ws, {signed}) => SOME (wordCheckP (if signed then "smul" else "umul", ws))
       | Word_neg ws => SOME (wordNary' ("sub", ws, fn args => (LLVM.Value.zero ws)::args))
       | Word_negCheckP (ws, {signed}) => SOME (wordCheckP' (if signed then "ssub" else "usub", ws, fn args => (LLVM.Value.zero ws)::args))
       | Word_notb ws => SOME (wordNary' ("xor", ws, fn args =>(LLVM.Value.negOne ws)::args))
       | Word_orb ws => SOME (wordNary ("or", ws))
       | Word_quot (ws, {signed}) => SOME (wordNary (if signed then "sdiv" else "udiv", ws))
       | Word_rem (ws, {signed}) => SOME (wordNary (if signed then "srem" else "urem", ws))
       | Word_rndToReal (_, rs, {signed}) => SOME (conv (if signed then sitofp else uitofp, LLVM.Type.Real rs))
       | Word_rol ws => SOME (wordRotate ("fshl", ws))
       | Word_ror ws => SOME (wordRotate ("fshr", ws))
       | Word_rshift (ws, {signed}) => SOME (wordShift (if signed then "ashr" else "lshr", ws))
       | Word_sub ws => SOME (wordNary ("sub", ws))
       | Word_subCheckP (ws, {signed}) => SOME (wordCheckP (if signed then "ssub" else "usub", ws))
       | Word_xorb ws => SOME (wordNary ("xor", ws))
       | _ => NONE
   end
fun implementsPrim (p: 'a Prim.t): bool = Option.isSome (primApp p)

fun primAppOpAndCheck {args: LLVM.Value.t list,
                       prim: 'a Prim.t,
                       mc: LLVM.ModuleContext.t,
                       newTemp: LLVM.Type.t -> LLVM.Value.t,
                       $ : LLVM.Instr.t -> unit}: LLVM.Value.t * LLVM.Value.t =
   let
      open LLVM.Instr
      fun doit' (oper, ws, fargs) =
         let
            val args = fargs args
            val atys = List.map (args, #2)
            val wty = LLVM.Type.Word ws
            val sty = LLVM.Type.Struct (false, [wty, LLVM.Type.bool])
            val name = concat [oper, ".with.overflow.i", WordSize.toString ws]
            val fnptr = LLVM.ModuleContext.intrinsic (mc, name, {argTys = atys, resTy = sty})
            val tmps = newTemp sty
            val res1 = newTemp wty
            val tmpb = newTemp LLVM.Type.bool
            val res2 = newTemp (LLVM.Type.Word WordSize.bool)
            val _ = $(call' {dst = tmps, tail = NONE, cconv = NONE, fnptr = fnptr, args = args})
            val _ = $(xval {dst = res1, src = tmps, args = ["0"]})
            val _ = $(xval {dst = tmpb, src = tmps, args = ["1"]})
            val _ = $(zext {dst = res2, src = tmpb})
         in
            (res1, res2)
         end
      fun doit (oper, ws) = doit' (oper, ws, fn args => args)
      datatype z = datatype Prim.t
   in
      case prim of
         Word_addCheckP (ws, {signed}) => doit (if signed then "sadd" else "uadd", ws)
       | Word_mulCheckP (ws, {signed}) => doit (if signed then "smul" else "umul", ws)
       | Word_negCheckP (ws, {signed}) => doit' (if signed then "ssub" else "usub", ws, fn args => (LLVM.Value.zero ws)::args)
       | Word_subCheckP (ws, {signed}) => doit (if signed then "ssub" else "usub", ws)
       | _ => Error.bug "LLVMCodegen.primAppOpAndChk"
   end

fun aamd (oper, mc) =
   case !Control.llvmAAMD of
      Control.LLVMAliasAnalysisMetaData.None => NONE
    | Control.LLVMAliasAnalysisMetaData.Scope =>
         let
            val domain =
               LLVM.ModuleContext.addMetaData
               (mc, LLVM.MetaData.node [LLVM.MetaData.string "MLton Scope Domain"])
            fun scope s =
               LLVM.ModuleContext.addMetaData
               (mc, LLVM.MetaData.node [LLVM.MetaData.string s,
                                        LLVM.MetaData.id domain])
            val (gcstate,global,heap,other,stack) =
               (scope "GCState", scope "Global", scope "Heap", scope "Other", scope "Stack")
            val scopes = [global,gcstate,heap,other,stack]
            fun scope s =
               let
                  fun scopeSet ss =
                     LLVM.ModuleContext.addMetaData
                     (mc, LLVM.MetaData.node (List.map (ss, LLVM.MetaData.id)))
                  val noalias = scopeSet (List.remove (scopes, fn s' => LLVM.MetaData.Id.equals (s, s')))
                  val alias = scopeSet [s]
               in
                  SOME (concat ["!noalias ", LLVM.MetaData.Id.toString noalias,
                                ", !alias.scope ", LLVM.MetaData.Id.toString alias])
               end
         in
            case oper of
               Operand.Frontier => NONE (* alloca *)
             | Operand.Global _ => scope global
             | Operand.Offset {base = Operand.GCState, ...} => scope gcstate
             | Operand.Offset {base, ...} => if Type.isObjptr (Operand.ty base)
                                                then scope heap
                                                else scope other
             | Operand.SequenceOffset {base, ...} => if Type.isObjptr (Operand.ty base)
                                                        then scope heap
                                                        else scope other
             | Operand.StackOffset _ =>
                  (* Unsound: At raise, exception results are written to the stack via an
                   * `Offset` with `base` corresponding to `StackBottom + exnStack` and
                   * then read from the stack via a `StackOffset` by the handler.
                   *)
                  scope stack
             | Operand.StackTop => NONE (* alloca *)
             | Operand.Temporary _ => NONE (* alloca *)
             | _ => NONE (* not lvalue *)
         end
    | Control.LLVMAliasAnalysisMetaData.TBAA {gcstate, global, heap, other, stack} =>
         let
            fun tbaa path =
               let
                  val root =
                     LLVM.ModuleContext.addMetaData
                     (mc, LLVM.MetaData.node [LLVM.MetaData.string "MLton TBAA Root"])
                  val (desc, _) =
                     List.foldr
                     (path, (root, ""), fn (node,(desc,name)) =>
                      let
                         val name =
                            if String.isEmpty name
                               then node
                               else concat [name, " ", node]
                      in
                         (LLVM.ModuleContext.addMetaData
                          (mc, LLVM.MetaData.node [LLVM.MetaData.string name,
                                                   LLVM.MetaData.id desc]),
                          name)
                      end)
                  val acc =
                     LLVM.ModuleContext.addMetaData
                     (mc, LLVM.MetaData.node [LLVM.MetaData.id desc,
                                              LLVM.MetaData.id desc,
                                              LLVM.MetaData.value (LLVM.Value.zero WordSize.word32)])
                in
                  SOME (concat ["!tbaa ", LLVM.MetaData.Id.toString acc])
               end
            val other = fn () =>
               if other then tbaa ["Other"] else NONE
         in
            case oper of
               Operand.Frontier => NONE (* alloca *)
             | Operand.Global g =>
                  (case global of
                      NONE => NONE
                    | SOME {cty = doCTy, index = doIndex} =>
                         let
                            val path = ["Global"]
                            val path =
                               if doCTy
                                  then (CType.name (Type.toCType (Global.ty g)))::path
                                  else path
                            val path =
                               if doIndex
                                  then (Int.toString (Global.index g))::path
                                  else path
                         in
                            tbaa path
                         end)
             | Operand.Offset {base = Operand.GCState, offset, ...} =>
                  (case gcstate of
                      NONE => NONE
                    | SOME {offset = doOffset} =>
                         let
                            val path = ["GCState"]
                            val path =
                               if doOffset
                                  then (Bytes.toString offset)::path
                                  else path
                         in
                            tbaa path
                         end)
             | Operand.Offset {base, offset, ty, ...} =>
                  (if Type.isObjptr (Operand.ty base)
                      then (case heap of
                               NONE => NONE
                             | SOME {cty = doCTy, kind = doKind, offset = doOffset, tycon = doTycon} =>
                                  let
                                     val path = ["Heap"]
                                     val path =
                                        if doKind
                                           then "Normal"::path
                                           else path
                                     val path =
                                        if doTycon
                                           then (case Type.deObjptr (Operand.ty base) of
                                                    NONE => path
                                                  | SOME tyc => (ObjptrTycon.toString tyc)::path)
                                           else path
                                     val path =
                                        if doCTy
                                           then (CType.name (Type.toCType ty))::path
                                           else path
                                     val path =
                                        if doOffset
                                           then (Bytes.toString offset)::path
                                           else path
                                  in
                                     tbaa path
                                  end)
                      else other ())
             | Operand.SequenceOffset {base, offset, ty, ...} =>
                  (if Type.isObjptr (Operand.ty base)
                      then (case heap of
                               NONE => NONE
                             | SOME {cty = doCTy, kind = doKind, offset = doOffset, tycon = doTycon} =>
                                  let
                                     val path = ["Heap"]
                                     val path =
                                        if doKind
                                           then "Sequence"::path
                                           else path
                                     (* Unsound: Around a `Array_toVector` primitive, a sequence may
                                      * be written to at one `ObjptrTycon.t` (corresponding to an
                                      * `array`) and then read from at a distinct `ObjptrTycon.t`
                                      * (corresponding to a `vector`).
                                      *)
                                     val path =
                                        if doTycon
                                           then (case Type.deObjptr (Operand.ty base) of
                                                    NONE => path
                                                  | SOME tyc => (ObjptrTycon.toString tyc)::path)
                                           else path
                                     (* Unsound: `WordArray_{sub,update}Word {seqSize, elemSize}`
                                      * and `WordVector_subWord {seqSize, elemSize}` primitives (for
                                      * `signature PACK_WORD`) are translated to `SequenceOffset`
                                      * with `base` corresponding to a sequence of `seqSize`,
                                      * `offset = Bytes.zero`, `scale` corresponding to `elemSize`,
                                      * and `ty` corresponding to the `elemSize`; thus, the same
                                      * address can be accessed for `Word8` and `Word64` elements.
                                      *)
                                     val path =
                                        if doCTy
                                           then (CType.name (Type.toCType ty))::path
                                           else path
                                     val path =
                                        if doOffset
                                           then (Bytes.toString offset)::path
                                           else path
                                  in
                                     tbaa path
                                  end)
                      else other ())
             | Operand.StackOffset (StackOffset.T {offset, ...}) =>
                  (case stack of
                      NONE => NONE
                    | SOME {offset = doOffset} =>
                         let
                            (* Unsound: At raise, exception results are written to the stack via an
                             * `Offset` with `base` corresponding to `StackBottom + exnStack` and
                             * then read from the stack via a `StackOffset` by the handler.
                             *)
                            val path = ["StackOffset"]
                            (* Unsound: At non-tail call/return, arguments/results are written to
                             * the stack relative to the callee/caller stack frame and then read
                             * from the stack relative to the caller/callee stack frame.  In
                             * general, around a stack push/pop, distinct offsets correspond to the
                             * same location.
                             *)
                            val path =
                               if doOffset
                                  then (Bytes.toString offset)::path
                                  else path
                         in
                            tbaa path
                         end)
             | Operand.StackTop => NONE (* alloca *)
             | Operand.Temporary _ => NONE (* alloca *)
             | _ => NONE (* not lvalue *)
         end

fun output {program as Machine.Program.T {chunks, frameInfos, main, ...},
            outputC: unit -> {file: File.t,
                              print: string -> unit,
                              done: unit -> unit},
            outputLL: unit -> {file: File.t,
                               print: string -> unit,
                               done: unit -> unit}} =
   let
      val {get = labelInfo: Label.t -> {block: Block.t,
                                        chunkLabel: ChunkLabel.t,
                                        index: int option},
           set = setLabelInfo, ...} =
         Property.getSetOnce
         (Label.plist, Property.initRaise ("LLVMCodeGen.labelInfo", Label.layout))
      val nextChunks = Array.new (Vector.length frameInfos, NONE)
      val _ =
         List.foreach
         (chunks, fn Chunk.T {blocks, chunkLabel, ...} =>
          Vector.foreach
          (blocks, fn block as Block.T {kind, label, ...} =>
           let
              val index =
                 case Kind.frameInfoOpt kind of
                    NONE => NONE
                  | SOME fi =>
                       let
                          val index = FrameInfo.index fi
                       in
                          if Kind.isEntry kind
                             then Array.update (nextChunks, index, SOME label)
                             else ()
                          ; SOME index
                       end
           in
              setLabelInfo (label, {block = block,
                                    chunkLabel = chunkLabel,
                                    index = index})
           end))
      val nextChunks = Vector.keepAllMap (Vector.fromArray nextChunks, fn lo => lo)
      val labelChunk = #chunkLabel o labelInfo
      val labelIndex = valOf o #index o labelInfo
      fun labelIndexValue l =
         (LLVM.Value.word o WordX.fromInt)
         (labelIndex l, WordSize.cpointer ())

      val amTimeProfiling =
         !Control.profile = Control.ProfileTime

      fun creturnName (ct: CType.t): string =
         concat ["%CReturn", CType.name ct]
      fun creturnVarC (ct: CType.t): LLVM.Value.t =
         (creturnName ct, LLVM.Type.Pointer (LLVM.Type.fromCType ct))
      fun creturnVar t = creturnVarC (Type.toCType t)
      fun globalName (ct: CType.t): string =
         concat ["@global", CType.toString ct]
      fun globalValC (ct: CType.t, mc): LLVM.Value.t =
         let
            val name = globalName ct
            val ty = LLVM.Type.Array (Global.numberOfType ct, LLVM.Type.fromCType ct)
         in
            LLVM.ModuleContext.addGlobDecl (mc, name, {const = false, ty = ty, vis = SOME "hidden"})
         end
      fun globalVal (c, mc) = globalValC (Type.toCType c, mc)
      fun temporaryName (ct: CType.t, index: int): string =
         concat ["%T", CType.name ct, "_", Int.toString index]
      fun temporaryVarC (ct: CType.t, index: int): LLVM.Value.t =
         (temporaryName (ct, index), LLVM.Type.Pointer (LLVM.Type.fromCType ct))
      fun temporaryVar (t, index) = temporaryVarC (Type.toCType t, index)
      fun staticHeapVal (kind, mc): LLVM.Value.t =
         let
            val name = concat ["@", Label.toString (StaticHeap.Kind.label kind)]
            val ty = LLVM.Type.word8
            val const =
               case kind of
                  StaticHeap.Kind.Immutable => true
                | _ => false
         in
            LLVM.ModuleContext.addGlobDecl (mc, name, {const = const, ty = ty, vis = SOME "hidden"})
         end

      local
         fun formalToArg (formal: LLVM.Formal.t) = (#1 formal, #1 (#2 formal))
      in
         val gcStateParam = LLVM.Param.cpointer
         val gcStateFormal = ("%gcState", gcStateParam)
         val gcStateArg = formalToArg gcStateFormal
         local
            fun mk (name, param as (ty, _)) =
               let
                  val formal = (name ^ "Arg", param)
               in
                  (param, formal, formalToArg formal, (name, LLVM.Type.Pointer ty))
               end
         in
            val (stackTopParam, stackTopFormal, stackTopArg, stackTopVar) =
               mk ("%stackTop", LLVM.Param.cpointer)
            val (frontierParam, frontierFormal, frontierArg, frontierVar) =
               mk ("%frontier", LLVM.Param.cpointer)
            val (nextBlockParam, nextBlockFormal, nextBlockArg, nextBlockVar) =
               mk ("%nextBlock", LLVM.Param.uintptr ())
         end
      end
      val chunkFnFormals = [gcStateFormal, stackTopFormal, frontierFormal, nextBlockFormal]
      val chunkFnArgParams = List.map (chunkFnFormals, #2)
      val chunkFnArgTys = List.map (chunkFnArgParams, #1)
      val chunkFnResParam = LLVM.Param.uintptr ()
      val chunkFnResTy = #1 chunkFnResParam
      val chunkFnTy = LLVM.Type.Function (chunkFnArgTys, chunkFnResTy)
      val chunkFnPtrTy = LLVM.Type.Pointer chunkFnTy
      local
         fun mk tos (cl: ChunkLabel.t, mc): LLVM.Value.t =
            LLVM.ModuleContext.addFnDecl
            (mc, tos cl,
             {argParams = chunkFnArgParams,
              resParam = chunkFnResParam,
              vis = SOME "hidden"})
      in
         val chunkFnValX = mk ChunkLabel.toStringX
         val chunkFnVal' = mk ChunkLabel.toString'
      end
      fun nextChunksVar mc =
         let
            val name = if !Control.llvmCC10 then "@nextXChunks" else "@nextChunks"
            val ty = LLVM.Type.Array (Vector.length nextChunks, chunkFnPtrTy)
         in
            LLVM.ModuleContext.addGlobDecl (mc, name, {const = true, ty = ty, vis = SOME "hidden"})
         end

      val doSwitchNextBlock = LLVM.Value.label' "doSwitchNextBlock"

      fun outputChunkFn (Chunk.T {chunkLabel, blocks, tempsMax, ...}, mc, print) =
         let
            val selfChunk = chunkLabel

            local
               fun tb () = print "\t"
               fun ln () = print "\n"
            in
               fun prints ss = List.foreach (ss, print)
               fun println s = (print s; ln ())
               fun printsln ss = (prints ss; ln ())
               fun tbprintsln ss = (tb (); prints ss; ln ())
            end

            local
               val next = Counter.generator 0
            in
               fun newTemp' (param as (ty, _)) =
                  let
                     val name = concat ["%t", Int.toString (next ())]
                  in
                     ((name, param), (name, ty))
                  end
               fun newTemp ty = #2 (newTemp' (ty, LLVM.ParamAttrs.empty))
            end

            open LLVM.Instr
            fun $ i = (print "\t"; AList.foreach (i, print); print "\n")

            fun operandToLValue oper =
               let
                  val (addr, volatile) =
                     case oper of
                        Operand.Frontier => (frontierVar, false)
                      | Operand.Global g =>
                           let
                              val ty = Global.ty g
                              val index =
                                 LLVM.Value.word
                                 (WordX.fromInt (Global.index g, WordSize.word32))
                              val res = newTemp (LLVM.Type.Pointer (Type.toLLVMType ty))
                              val _ = $(gep {dst = res, src = globalVal (ty, mc),
                                             args = [LLVM.Value.zero WordSize.word32, index]})
                           in
                              (res, false)
                           end
                      | Operand.Offset {base, offset, ty, volatile} =>
                           let
                              val base = operandToRValue base
                              val offset = LLVM.Value.word (WordX.fromBytes (offset, WordSize.word32))
                              val tmp = newTemp LLVM.Type.cpointer
                              val res = newTemp (LLVM.Type.Pointer (Type.toLLVMType ty))
                              val _ = $(gep {dst = tmp, src = base, args = [offset]})
                              val _ = $(cast {dst = res, src = tmp})
                           in
                              (res, volatile)
                           end
                      | Operand.SequenceOffset {base, index, offset, scale, ty, volatile} =>
                           let
                              val base = operandToRValue base
                              val index as (_, indexTy) = operandToRValue index
                              val scale = LLVM.Value.word (WordX.fromBytes (Scale.toBytes scale, WordSize.cptrdiff ()))
                              val offset = LLVM.Value.word (WordX.fromBytes (offset, WordSize.word32))
                              val tmp1 = newTemp indexTy
                              val tmp2 = newTemp LLVM.Type.cpointer
                              val tmp3 = newTemp LLVM.Type.cpointer
                              val res = newTemp (LLVM.Type.Pointer (Type.toLLVMType ty))
                              val _ = $(naryop {dst = tmp1, oper = ("mul nsw", indexTy),
                                                args = [index, scale]})
                              val _ = $(gep {dst = tmp2, src = base, args = [tmp1]})
                              val _ = $(gep {dst = tmp3, src = tmp2, args = [offset]})
                              val _ = $(cast {dst = res, src = tmp3})
                           in
                              (res, volatile)
                           end
                      | Operand.StackOffset (StackOffset.T {offset, ty, volatile}) =>
                           let
                              val stackTop = newTemp LLVM.Type.cpointer
                              val addr = newTemp LLVM.Type.cpointer
                              val res = newTemp (LLVM.Type.Pointer (Type.toLLVMType ty))
                              val _ = $(load {dst = stackTop, src = stackTopVar})
                              val _ = $(gep {dst = addr, src = stackTop,
                                             args = [LLVM.Value.word
                                                     (WordX.fromBytes
                                                      (offset, WordSize.word32))]})
                              val _ = $(cast {dst = res, src = addr})
                           in
                              (res, volatile)
                           end
                      | Operand.StackTop => (stackTopVar, false)
                      | Operand.Temporary t => (temporaryVar (Temporary.ty t, Temporary.index t), false)
                      | _ => Error.bug ("LLVMCodegen.operandToLValue: " ^ Operand.toString oper)
                  val aamd = aamd (oper, mc)
               in
                  (fn {dst} => addMetaData (load' {dst = dst, src = addr, volatile = volatile}, aamd),
                   fn {src} => addMetaData (store' {dst = addr, src = src, volatile = volatile}, aamd))
               end
            and operandToRValue oper =
               let
                  val load = fn () =>
                     let
                        val (loadOper, _) = operandToLValue oper
                        val res = newTemp (Type.toLLVMType (Operand.ty oper))
                        val _ = $(loadOper {dst = res})
                     in
                        res
                     end
               in
                  case oper of
                     Operand.Cast (oper, ty) =>
                        let
                           val oper = operandToRValue oper
                           val res = newTemp (Type.toLLVMType ty)
                           val _ = $(cast {dst = res, src = oper})
                        in
                           res
                        end
                   | Operand.Const (Const.CSymbol (CSymbol.T {name, cty, symbolScope})) =>
                        let
                           val name = "@" ^ name
                           val ty =
                              case cty of
                                 NONE => LLVM.Type.Word WordSize.word8
                               | SOME ty => LLVM.Type.fromCType ty
                           val vis =
                              case symbolScope of
                                 CSymbolScope.External => "default"
                               | CSymbolScope.Private => "hidden"
                               | CSymbolScope.Public => "default"
                           val globptr =
                              LLVM.ModuleContext.addGlobDecl
                              (mc, name, {const = false, ty = ty, vis = SOME vis})
                           val res = newTemp LLVM.Type.cpointer
                           val _ = $(bitcast {dst = res, src = globptr})
                        in
                           res
                        end
                   | Operand.Const Const.Null => LLVM.Value.null
                   | Operand.Const (Const.Real r) => LLVM.Value.real r
                   | Operand.Const (Const.Word w) => LLVM.Value.word w
                   | Operand.Const _ => Error.bug "LLVMCodegen.operandToRValue: Const"
                   | Operand.Frontier => load ()
                   | Operand.GCState => gcStateArg
                   | Operand.Global _ => load ()
                   | Operand.Label label => labelIndexValue label
                   | Operand.Offset _ => load ()
                   | Operand.SequenceOffset _ => load ()
                   | Operand.StackOffset _ => load ()
                   | Operand.StackTop => load ()
                   | Operand.StaticHeapRef (StaticHeap.Ref.T {kind, offset, ty, ...}) =>
                        let
                           val tmp = newTemp LLVM.Type.cpointer
                           val res = newTemp (Type.toLLVMType ty)
                           val _ = $(gep {dst = tmp, src = staticHeapVal (kind, mc),
                                          args = [LLVM.Value.word
                                                  (WordX.fromBytes
                                                   (offset, WordSize.word32))]})
                           val _ = $(cast {dst = res, src = tmp})
                        in
                           res
                        end
                   | Operand.Temporary _ => load ()
               end
            fun operandsToRValues opers =
               (List.rev o Vector.fold)
               (opers, [], fn (oper, opers) => (operandToRValue oper)::opers)

            fun outputStatement (s: Statement.t): unit =
               let in
                  case s of
                     Statement.Move {dst, src} =>
                        let
                           val (_, storeDst) = operandToLValue dst
                           val src = operandToRValue src
                           val _ = $(storeDst {src = src})
                        in
                           ()
                        end
                     | Statement.PrimApp {args, dst, prim} =>
                        let
                           val args = operandsToRValues args
                           val res =
                              (valOf (primApp prim))
                              {args = args, mc = mc, newTemp = newTemp, $ = $}
                           val _ =
                              case dst of
                                 NONE => ()
                               | SOME dst =>
                                    let
                                       val (_, storeDst) = operandToLValue dst
                                       val _ = $(storeDst {src = res})
                                    in
                                       ()
                                    end
                        in
                           ()
                        end
               end
            local
               fun mk (dst, src) () =
                  outputStatement (Statement.Move {dst = dst, src = src})
               val stackTop = Operand.StackTop
               val gcStateStackTop = Operand.gcField GCField.StackTop
               val frontier = Operand.Frontier
               val gcStateFrontier = Operand.gcField GCField.Frontier
            in
               val cacheStackTop = mk (stackTop, gcStateStackTop)
               val flushStackTop = mk (gcStateStackTop, stackTop)
               val cacheFrontier = mk (frontier, gcStateFrontier)
               val flushFrontier = mk (gcStateFrontier, frontier)
            end
            (* StackTop += size *)
            fun adjStackTop (size: Bytes.t) =
               (outputStatement (Statement.PrimApp
                                 {args = Vector.new2
                                         (Operand.StackTop,
                                          Operand.word
                                          (WordX.fromBytes
                                           (size,
                                            WordSize.cptrdiff ()))),
                                  dst = SOME Operand.StackTop,
                                  prim = Prim.CPointer_add})
                ; if amTimeProfiling
                     then flushStackTop ()
                     else ())
            fun pop (fi: FrameInfo.t) =
               adjStackTop (Bytes.~ (FrameInfo.size fi))
            fun push (return: Label.t, size: Bytes.t) =
               (outputStatement (Statement.Move
                                 {dst = Operand.stackOffset
                                        {offset = Bytes.- (size, Runtime.labelSize ()),
                                         ty = Type.label return,
                                         volatile = amTimeProfiling},
                                  src = Operand.Label return})
                ; adjStackTop size)
            (* LeaveChunk(nextChunk, nextBlock)
                 if (TailCall) {
                   return nextChunk(gcState, stackTop, frontier, nextBlock);
                 } else {
                   flushFrontier();
                   flushStackTop();
                   return nextBlock;
                }
            *)
            fun leaveChunk (nextChunk, nextBlock) =
               if !Control.chunkTailCall
                  then let
                          val gcStateActual = gcStateFormal
                          val (stackTopActual, stackTop) =
                             newTemp' stackTopParam
                          val (frontierActual, frontier) =
                             newTemp' frontierParam
                          val nextBlockActual =
                             (#1 nextBlock, nextBlockParam)
                          val (resParam, res) =
                             newTemp' chunkFnResParam
                       in
                          $(load {dst = stackTop, src = stackTopVar})
                          ; $(load {dst = frontier, src = frontierVar})
                          ; $(call {dst = resParam,
                                    tail = SOME "musttail",
                                    cconv = if !Control.llvmCC10
                                               then SOME "cc10"
                                               else NONE,
                                    fnptr = nextChunk,
                                    args = [gcStateActual,
                                            stackTopActual,
                                            frontierActual,
                                            nextBlockActual]})
                          ; $(ret res)
                       end
                  else (flushFrontier ()
                        ; flushStackTop ()
                        ; $(ret nextBlock))
            (* IndJump(mustReturnToSelf, mayReturnToSelf, mustReturnToOther)
                 nextBlock = *(uintptr_t* )(StackTop - sizeof(uintptr_t));
                 if (mustReturnToSelf) {
                   goto doSwitchNextBlock;
                 } else {
                   ChunkFnPtr_t nextChunk = nextChunks[nextBlock];
                   if (mayReturnToSelf && (nextChunk == selfChunk)) {
                     goto doSwitchNextBlock;
                   }
                   if (mustReturnToOther != NULL) {
                     LeaveChunk( *mustReturnToOther, nextBlock);
                   } else {
                     LeaveChunk( *nextChunk, nextBlock);
                   }
                }
            *)
            fun indJump (mustReturnToSelf, mayReturnToSelf, mustReturnToOther) =
               let
                  val nextBlock =
                     operandToRValue
                     (Operand.stackOffset
                      {offset = Bytes.~ (Runtime.labelSize ()),
                       ty = Type.label (Label.newNoname ()),
                       volatile = false})
                  val _ = $(store {dst = nextBlockVar, src = nextBlock})
               in
                  if mustReturnToSelf
                     then $(jmp doSwitchNextBlock)
                     else let
                             val nextChunkAddr = newTemp (LLVM.Type.Pointer chunkFnPtrTy)
                             val nextChunk = newTemp chunkFnPtrTy
                             val doNextChunk =
                                Promise.delay
                                (fn () =>
                                 ($(gep {dst = nextChunkAddr, src = nextChunksVar mc,
                                         args = [LLVM.Value.zero WordSize.word32, nextBlock]})
                                  ; $(load {dst = nextChunk, src = nextChunkAddr})))
                             val _ =
                                if mayReturnToSelf
                                   then let
                                           val _ = Promise.force doNextChunk
                                           val rToSelf = Label.newNoname ()
                                           val rToOther = Label.newNoname ()
                                           val test = newTemp LLVM.Type.bool
                                           val _ = $(naryop {dst = test,
                                                             oper = ("icmp eq", chunkFnPtrTy),
                                                             args = [nextChunk, chunkFnVal' (selfChunk, mc)]})
                                           val _ = $(br {test = test,
                                                         truee = LLVM.Value.label rToSelf,
                                                         falsee = LLVM.Value.label rToOther})
                                           val _ = printsln [Label.toString rToSelf, ":"]
                                           val _ = $(jmp doSwitchNextBlock)
                                           val _ = printsln [Label.toString rToOther, ":"]
                                        in
                                           ()
                                        end
                                   else ()
                             val _ =
                                case mustReturnToOther of
                                   NONE => (Promise.force doNextChunk; leaveChunk (nextChunk, nextBlock))
                                 | SOME dstChunk => leaveChunk (chunkFnVal' (dstChunk, mc), nextBlock)
                          in
                             ()
                          end
               end
            fun outputTransfer (t: Transfer.t): unit =
               let
                  fun jump label =
                     let
                        val dstChunk = labelChunk label
                     in
                        if ChunkLabel.equals (dstChunk, selfChunk)
                           then $(jmp (LLVM.Value.label label))
                           else leaveChunk (chunkFnVal' (dstChunk, mc),
                                            labelIndexValue label)
                     end
                  fun rtrans rsTo =
                     let
                        val mustRToOne =
                           case rsTo of
                              [] => NONE
                            | l::rsTo =>
                                 if List.forall (rsTo, fn l' => Label.equals (l, l'))
                                    then SOME l
                                    else NONE
                        fun isSelf c = ChunkLabel.equals (selfChunk, c)
                        val rsTo =
                           List.fold
                           (rsTo, [], fn (l, cs) =>
                            let
                               val c = labelChunk l
                            in
                               if List.contains (cs, c, ChunkLabel.equals)
                                  then cs
                                  else c::cs
                            end)
                        val mayRToSelf = List.exists (rsTo, isSelf)
                        val (mustRToSelf, mustRToOther) =
                           case List.revKeepAll (rsTo, not o isSelf) of
                              [] => (true, NONE)
                            | c::rsTo =>
                                 (false,
                                  if List.forall (rsTo, fn c' => ChunkLabel.equals (c, c'))
                                     then SOME c
                                     else NONE)
                     in
                        case (!Control.chunkMustRToSingOpt, mustRToOne) of
                           (true, SOME dst) => jump dst
                         | _ =>
                              indJump (!Control.chunkMustRToSelfOpt andalso mustRToSelf,
                                       !Control.chunkMayRToSelfOpt andalso mayRToSelf,
                                       if (!Control.chunkMustRToOtherOpt andalso
                                           (!Control.chunkMayRToSelfOpt orelse not mayRToSelf))
                                          then mustRToOther
                                          else NONE)
                     end
                  val _ =
                     if !Control.codegenComments > 0
                        then tbprintsln ["; ", Layout.toString (Transfer.layout t)]
                        else ()
               in
                  case t of
                     Transfer.CCall {func =
                                     CFunction.T
                                     {target =
                                      CFunction.Target.Direct "Thread_returnToC", ...},
                                     return = SOME {return, size = SOME size}, ...} =>
                        let
                           val _ = push (return, size)
                           val _ = flushFrontier ();
                           val _ = flushStackTop ();
                           val (tmpParam, tmp) = newTemp' (LLVM.Param.uintptr ())
                           val fnptr =
                              LLVM.ModuleContext.addFnDecl
                              (mc, "@Thread_returnToC",
                               {argParams = [],
                                resParam = #2 tmpParam,
                                vis = SOME "hidden"})
                           val _ = $(call {dst = tmpParam,
                                           tail = NONE, cconv = NONE,
                                           fnptr = fnptr, args = []})
                           val _ = $(ret tmp)
                        in
                           ()
                        end
                   | Transfer.CCall {args, func, return} =>
                        let
                           val CFunction.T {return = returnTy, prototype = (argsProto, resProto), target, symbolScope, ...} = func
                           val args = operandsToRValues args
                           val _ = Option.app (return, fn {return, size} =>
                                               Option.app (size, fn size =>
                                                           push (return, size)))
                           val _ = if CFunction.modifiesFrontier func then flushFrontier () else ()
                           val _ = if CFunction.readsStackTop func then flushStackTop () else ()
                           val resParam =
                              case resProto of
                                 NONE => LLVM.Param.void
                               | SOME resCTy => LLVM.Param.fromCType resCTy
                           val argParams =
                              Vector.toListMap (argsProto, LLVM.Param.fromCType)
                           val argActuals =
                              List.map2
                              (args, argParams, fn ((arg, _), argParam) =>
                               (arg, argParam))
                           val (fnptr, argActuals) =
                              case target of
                                 CFunction.Target.Direct name =>
                                    let
                                       val name = "@" ^ name
                                       val vis =
                                          case symbolScope of
                                             CFunction.SymbolScope.External => "default"
                                           | CFunction.SymbolScope.Private => "hidden"
                                           | CFunction.SymbolScope.Public => "default"
                                       val fnptr =
                                          LLVM.ModuleContext.addFnDecl
                                          (mc, name,
                                           {argParams = argParams,
                                            resParam = resParam,
                                            vis = SOME vis})
                                    in
                                       (fnptr, argActuals)
                                    end
                               | CFunction.Target.Indirect =>
                                    let
                                       val (cptr, argActuals) =
                                          case argActuals of
                                             cptrActual::argActuals =>
                                                (LLVM.Actual.toValue cptrActual,
                                                 argActuals)
                                           | _ => Error.bug "LLVMCodegen.outputTransfer: CCall,Indirect"
                                       val argTys = List.map (argActuals, #1 o #2)
                                       val resTy = #1 resParam
                                       val fnty = LLVM.Type.Function (argTys, resTy)
                                       val fnptr = newTemp (LLVM.Type.Pointer fnty)
                                       val _ = $(cast {dst = fnptr, src = cptr})
                                    in
                                       (fnptr, argActuals)
                                    end
                           val (resParam, res) = newTemp' resParam
                           val _ = $(call {dst = resParam,
                                           tail = NONE, cconv = NONE,
                                           fnptr = fnptr, args = argActuals})
                           val _ =
                              case return of
                                 NONE =>
                                    let
                                       val (tmpParam, tmp) =
                                          newTemp' (LLVM.Param.uintptr ())
                                       val fnptr =
                                          LLVM.ModuleContext.addFnDecl
                                          (mc, "@MLton_unreachable",
                                           {argParams = [],
                                            resParam = #2 tmpParam,
                                            vis = SOME "hidden"})
                                       val _ = $(call {dst = tmpParam,
                                                       tail = NONE, cconv = NONE,
                                                       fnptr = fnptr, args = []})
                                       val _ = $(ret tmp)
                                    in
                                       ()
                                    end
                               | SOME {return, ...} =>
                                    let
                                       val _ = if CFunction.modifiesFrontier func then cacheFrontier () else ()
                                       val _ = if CFunction.writesStackTop func then cacheStackTop () else ()
                                       val _ = if Type.isUnit returnTy
                                                  then ()
                                                  else $(store {dst = creturnVar returnTy, src = res})
                                       val _ =
                                          if CFunction.maySwitchThreadsFrom func
                                             then indJump (false, true, NONE)
                                             else $(jmp (LLVM.Value.label return))
                                    in
                                       ()
                                    end
                        in
                           ()
                        end
                   | Transfer.Call {label, return, ...} =>
                        (Option.app (return, fn {return, size, ...} => push (return, size))
                         ; jump label)
                   | Transfer.Goto dst => $(jmp (LLVM.Value.label dst))
                   | Transfer.Raise {raisesTo} =>
                        (outputStatement (Statement.PrimApp
                                          {args = Vector.new2
                                                  (Operand.gcField GCField.StackBottom,
                                                   Operand.gcField GCField.ExnStack),
                                           dst = SOME Operand.StackTop,
                                           prim = Prim.CPointer_add})
                         ; rtrans raisesTo)
                   | Transfer.Return {returnsTo} => rtrans returnsTo
                   | Transfer.Switch (Switch.T {cases, default, expect, test, ...}) =>
                        let
                           val test = operandToRValue test
                           val test =
                              case expect of
                                 NONE => test
                               | SOME w =>
                                    let
                                       val ws = WordX.size w
                                       val wty = LLVM.Type.Word ws
                                       val name = concat ["expect.i", WordSize.toString ws]
                                       val fnptr =
                                          LLVM.ModuleContext.intrinsic
                                          (mc, name, {argTys = [wty, wty], resTy = wty})
                                       val tmp = newTemp wty
                                       val args = [test, LLVM.Value.word w]
                                       val _ = $(call' {dst = tmp, tail = NONE, cconv = NONE, fnptr = fnptr, args = args})
                                    in
                                       tmp
                                    end
                           val (default, extra) =
                              case default of
                                 SOME d => (d, fn () => ())
                               | NONE => let
                                            val d = Label.newNoname ()
                                         in
                                            (d, fn () =>
                                             (printsln [Label.toString d, ":"]
                                              ; $(unreachable ())))
                                         end
                           val _ = $(switch {value = test, default = LLVM.Value.label default,
                                             table = Vector.toListMap (cases, fn (w, l) =>
                                                                       (LLVM.Value.word w,
                                                                        LLVM.Value.label l))})
                           val _ = extra ()
                        in
                           ()
                        end

               end
            val outputStatement = fn s =>
               let
                  val _ =
                     if !Control.codegenComments > 1
                        then tbprintsln ["; ", Layout.toString (Statement.layout s)]
                        else ()
               in
                  outputStatement s
               end
            (* Fusing of adjacent `Word<N>_<op>` and `Word{S,U}<N>_<op>CheckP`
             * primitives *does not* depends on the relative order of `!a` and `?a`
             * in /basis-library/primitive/prim1.sml:mkOverflow
             *)
            fun outputStatementsFuseOpAndChk statements =
               (ignore o Vector.foldi)
               (statements, false, fn (i, s1, skip) =>
                let
                   fun default () = (outputStatement s1; false)
                in
                   if skip then false else
                   case s1 of
                      Statement.PrimApp {args = args1, dst = SOME dst1, prim = prim1} =>
                         let
                            fun fuse chk =
                               (case Vector.sub (statements, i + 1) of
                                   s2 as Statement.PrimApp {args = args2, dst = SOME dst2, prim = prim2} =>
                                      if Vector.equals (args1, args2, Operand.equals)
                                         then (case chk prim2 of
                                                  NONE=> default ()
                                                | SOME prim =>
                                                     let
                                                        val _ =
                                                           if !Control.codegenComments > 1
                                                              then (tbprintsln ["; ", Layout.toString (Statement.layout s1)]
                                                                    ; tbprintsln ["; ", Layout.toString (Statement.layout s2)])
                                                              else ()
                                                        val args = operandsToRValues args1
                                                        val (res1, res2) =
                                                           primAppOpAndCheck
                                                           {args = args, prim = prim, mc = mc, newTemp = newTemp, $ = $}
                                                        val (_, storeDst1) = operandToLValue dst1
                                                        val _ = $(storeDst1 {src = res1})
                                                        val (_, storeDst2) = operandToLValue dst2
                                                        val _ = $(storeDst2 {src = res2})
                                                     in
                                                        true
                                                     end)
                                         else default ()
                                 | _ => default ())
                               handle Subscript => default ()
                         in
                            case prim1 of
                               Prim.Word_add ws1 =>
                                  fuse (fn prim2 =>
                                        case prim2 of
                                           Prim.Word_addCheckP (ws2, _) =>
                                              if WordSize.equals (ws1, ws2)
                                                 then SOME prim2
                                                 else NONE
                                         | _ => NONE)
                             | Prim.Word_addCheckP (ws1, _) =>
                                  fuse (fn prim2 =>
                                        case prim2 of
                                           Prim.Word_add ws2 =>
                                              if WordSize.equals (ws1, ws2)
                                                 then SOME prim1
                                                 else NONE
                                         | _ => NONE)
                             | Prim.Word_mul (ws1, {signed = signed1}) =>
                                  fuse (fn prim2 =>
                                        case prim2 of
                                           Prim.Word_mulCheckP (ws2, {signed = signed2}) =>
                                              if WordSize.equals (ws1, ws2)
                                                 andalso Bool.equals (signed1, signed2)
                                                 then SOME prim2
                                                 else NONE
                                         | _ => NONE)
                             | Prim.Word_mulCheckP (ws1, {signed = signed1}) =>
                                  fuse (fn prim2 =>
                                        case prim2 of
                                           Prim.Word_mul (ws2, {signed = signed2}) =>
                                              if WordSize.equals (ws1, ws2)
                                                 andalso Bool.equals (signed1, signed2)
                                                 then SOME prim1
                                                 else NONE
                                         | _ => NONE)
                             | Prim.Word_neg ws1 =>
                                  fuse (fn prim2 =>
                                        case prim2 of
                                           Prim.Word_negCheckP (ws2, _) =>
                                              if WordSize.equals (ws1, ws2)
                                                 then SOME prim2
                                                 else NONE
                                         | _ => NONE)
                             | Prim.Word_negCheckP (ws1, _) =>
                                  fuse (fn prim2 =>
                                        case prim2 of
                                           Prim.Word_neg ws2 =>
                                              if WordSize.equals (ws1, ws2)
                                                 then SOME prim1
                                                 else NONE
                                         | _ => NONE)
                             | Prim.Word_sub ws1 =>
                                  fuse (fn prim2 =>
                                        case prim2 of
                                           Prim.Word_subCheckP (ws2, _) =>
                                              if WordSize.equals (ws1, ws2)
                                                 then SOME prim2
                                                 else NONE
                                         | _ => NONE)
                             | Prim.Word_subCheckP (ws1, _) =>
                                  fuse (fn prim2 =>
                                        case prim2 of
                                           Prim.Word_sub ws2 =>
                                              if WordSize.equals (ws1, ws2)
                                                 then SOME prim1
                                                 else NONE
                                         | _ => NONE)
                             | _ => default ()
                         end
                    | _ => default ()
                end)
            fun outputBlock (Block.T {kind, label, statements, transfer, ...}) =
               let
                  val _ = printsln [Label.toString label, ":"]
                  val _ =
                     case kind of
                        Kind.Cont {frameInfo, ...} => pop frameInfo
                      | Kind.CReturn {dst, frameInfo, ...} =>
                           (Option.app (frameInfo, pop)
                            ; (Option.app
                               (dst, fn dst =>
                                let
                                   val dst = Live.toOperand dst
                                   val ty = Operand.ty dst
                                   val creturn = newTemp (Type.toLLVMType ty)
                                   val _ = $(load {dst = creturn, src = creturnVar ty})
                                   val (_, storeDst) = operandToLValue dst
                                   val _ = $(storeDst {src = creturn})
                                in
                                   ()
                                end))
                            ; ())
                      | Kind.Func _ => ()
                      | Kind.Handler {frameInfo, ...} => pop frameInfo
                      | Kind.Jump => ()
                  val _ =
                     if !Control.codegenFuseOpAndChk
                        then outputStatementsFuseOpAndChk statements
                        else Vector.foreach (statements, outputStatement)
                  val _ = outputTransfer transfer
                  val _ = print "\n"
               in
                  ()
               end

            val entries =
               let
                  val entries = ref []
                  val _ =
                     Vector.foreach
                     (blocks, fn Block.T {kind, label, ...} =>
                      if Kind.isEntry kind
                         then List.push (entries, (label, labelIndex label))
                         else ())
               in
                  List.insertionSort (!entries, fn ((_, i1), (_, i2)) => i1 <= i2)
               end
            val numEntries = List.length entries
            val nextLabels = (concat [ChunkLabel.toString' chunkLabel, ".nextLabels"],
                              LLVM.Type.Pointer (LLVM.Type.Array (numEntries, LLVM.Type.blockaddress)))
            val _ = if !Control.chunkJumpTable
                        then let
                                val _ =
                                   prints [#1 nextLabels,
                                           " = internal constant ",
                                           LLVM.Type.toString (LLVM.Type.dePointer (#2 nextLabels)),
                                           " ["]
                                val _ =
                                   List.foreachi (entries, fn (i, (label, _)) =>
                                                  (if i > 0 then print "," else ()
                                                   ; prints ["\n\ti8* blockaddress(",
                                                             ChunkLabel.toString' chunkLabel,
                                                             ", ", "%", Label.toString label, ")"]))
                                val _ = print " ]\n"
                         in
                            ()
                         end
                    else ()

            val chunkFormals =
               concat ["(",
                       String.concatWith (List.map (chunkFnFormals, LLVM.Formal.toString), ", "),
                       ")"]
            val _ = LLVM.ModuleContext.addFnDefn (mc, ChunkLabel.toString chunkLabel)
            val _ = printsln ["define hidden ",
                              LLVM.Param.resToString chunkFnResParam, " ", ChunkLabel.toString chunkLabel,
                              chunkFormals, " {"]
            val _ =
               if !Control.llvmCC10
                  then let
                          val res = ("%res", chunkFnResParam)
                          val _ = $(call {dst = res, tail = NONE, cconv = SOME "cc10",
                                          fnptr = chunkFnValX (chunkLabel, mc),
                                          args = chunkFnFormals})
                          val _ = $(ret (LLVM.Actual.toValue res))
                          val _ = println "}"
                          val _ = LLVM.ModuleContext.addFnDefn (mc, ChunkLabel.toStringX chunkLabel)
                          val _ = printsln ["define hidden cc10 ",
                                            LLVM.Param.resToString chunkFnResParam, " ", ChunkLabel.toStringX chunkLabel,
                                            chunkFormals, " {"]
                       in
                          ()
                       end
                  else ()

            val _ = print "start:\n"
            val _ = List.foreach (CType.all, fn ct =>
                                  $(alloca {dst = creturnVarC ct}))
            val _ = List.foreach (CType.all, fn ct =>
                                  Int.for (0, 1 + tempsMax ct, fn i =>
                                           $(alloca {dst = temporaryVarC (ct, i)})))
            val _ = $(alloca {dst = stackTopVar})
            val _ = $(store {dst = stackTopVar, src = stackTopArg})
            val _ = $(alloca {dst = frontierVar})
            val _ = $(store {dst = frontierVar, src = frontierArg})
            val _ = $(alloca {dst = nextBlockVar})
            val _ = $(store {dst = nextBlockVar, src = nextBlockArg})
            val _ = $(jmp doSwitchNextBlock)
            val _ = print "\n"

            val _ = print "doSwitchNextBlock:\n"
            val nextBlock = newTemp (LLVM.Type.uintptr ())
            val _ = $(load {dst = nextBlock, src = nextBlockVar})
            val _ =
               if !Control.chunkJumpTable
                  then let
                          val index = newTemp (LLVM.Type.uintptr ())
                          val nextLabelAddr = newTemp (LLVM.Type.Pointer LLVM.Type.blockaddress)
                          val nextLabel = newTemp LLVM.Type.blockaddress
                          val bias = LLVM.Value.word (WordX.fromInt (#2 (List.first entries),
                                                                     WordSize.cpointer ()))

                          val _ = $(naryop {dst = index, oper = ("sub nuw nsw", LLVM.Type.uintptr ()),
                                            args = [nextBlock, bias]})
                          val _ = $(gep {dst = nextLabelAddr, src = nextLabels,
                                         args = [LLVM.Value.zero WordSize.word32, index]})
                          val _ = $(load {dst = nextLabel, src = nextLabelAddr})
                          val _ = $(indirectbr {addr = nextLabel, labels = List.map (entries, LLVM.Value.label o #1)})
                       in
                          ()
                       end
                  else let
                          val _ = $(switch {value = nextBlock,
                                            default = LLVM.Value.label' "switchNextBlockDefault",
                                            table = List.map (entries, fn (label, index) =>
                                                              (LLVM.Value.word
                                                               (WordX.fromInt
                                                                (index,
                                                                 WordSize.cpointer ())),
                                                               LLVM.Value.label label))})
                          val _ = print "switchNextBlockDefault:\n"
                          val _ = $(unreachable ())
                       in
                          ()
                       end
            val _ = print "\n"

            val _ = Vector.foreach (blocks, outputBlock)

            val _ = print "}\n\n"
         in
            ()
         end

      fun outputChunks chunks =
         let
            val {done, print, ...} = outputLL ()
            val mc = LLVM.ModuleContext.new ()
         in
            print "\n"
            ; List.foreach (chunks, fn chunk => outputChunkFn (chunk, mc, print))
            ; LLVM.ModuleContext.emit (mc, print)
            ; done ()
         end
      val chunksWithSizes =
         List.revMap
         (chunks, fn chunk as Chunk.T {blocks, ...} =>
          (chunk,
           Vector.fold
           (blocks, 0, fn (Block.T {statements, ...}, n) =>
            n + Vector.length statements + 1)))
      fun batch (chunksWithSizes, acc, n) =
         case chunksWithSizes of
            [] => outputChunks acc
          | (chunk, s)::chunksWithSizes' =>
               let
                  val m = n + s
               in
                  if List.isEmpty acc orelse m <= !Control.chunkBatch
                     then batch (chunksWithSizes', chunk::acc, m)
                     else (outputChunks acc;
                           batch (chunksWithSizes, [], 0))
               end
      val _ = batch (chunksWithSizes, [], 0)

      val {print, done, ...} = outputC ()
      fun defineNextChunks (nextChunksName, chunkName) =
         (List.foreach
          (chunks, fn Chunk.T {chunkLabel, ...} =>
           (print "PRIVATE extern ChunkFn_t "
            ; print (chunkName chunkLabel)
            ; print ";\n"))
          ; print "PRIVATE ChunkFnPtr_t const "
          ; print nextChunksName
          ; print "["
          ; print (Int.toString (Vector.length nextChunks))
          ; print "] = {\n"
          ; Vector.foreachi
            (nextChunks, fn (i, label) =>
             (print "\t"
              ; print "/* "
              ; print (Int.toString i)
              ; print ": */ "
              ; print "/* "
              ; print (Label.toString label)
              ; print " */ &("
              ; print (chunkName (labelChunk label))
              ; print "),\n"))
          ; print "};\n")
      val defineNextChunks = fn () =>
         (defineNextChunks ("nextChunks", ChunkLabel.toStringForC)
          ; if !Control.llvmCC10
               then defineNextChunks ("nextXChunks", ChunkLabel.toStringXForC)
               else ())
      val additionalMainArgs =
         let
            val mainLabel = #label main
         in
            [concat [Int.toString (labelIndex mainLabel),
                     " /* ", Label.toString mainLabel, " */"]]
         end
      val _ =
         CCodegen.outputDeclarations
         {additionalMainArgs = additionalMainArgs,
          includes = ["c-main.h"],
          program = program,
          print = print,
          rest = defineNextChunks}
      val _ = done ()
   in
      ()
   end

end
