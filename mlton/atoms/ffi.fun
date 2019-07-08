(* Copyright (C) 2019 Matthew Fluet.
 * Copyright (C) 2004-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Ffi (S: FFI_STRUCTS): FFI = 
struct

open S

structure Convention = CFunction.Convention
structure SymbolScope = CFunction.SymbolScope

local
   val scopes: (String.t, SymbolScope.t) HashTable.t =
      HashTable.new {hash = String.hash, equals = String.equals}
in
   fun checkScope {name, symbolScope} =
      HashTable.lookupOrInsert
      (scopes, name,
       fn () => symbolScope)
end

val exports: {args: CType.t vector,
              convention: Convention.t,
              id: int,
              name: string,
              res: CType.t option,
              symbolScope: SymbolScope.t} list ref = ref []
val symbols: {name: string,
              ty: CType.t,
              symbolScope: SymbolScope.t} list ref = ref []

fun numExports () = List.length (!exports)

local
   val nextId = Counter.generator 0
in
   fun addExport {args, convention, name, res, symbolScope} =
      let
         val id = nextId ()
         val _ = List.push (exports, {args = args,
                                      convention = convention,
                                      id = id,
                                      name = name,
                                      res = res,
                                      symbolScope = symbolScope})
      in
         id
      end
   fun addSymbol {name, ty, symbolScope} = 
      ignore (List.push (symbols, {name = name, 
                                   ty = ty, 
                                   symbolScope = symbolScope}))
end

val headers: string list ref = ref []

fun declareExports {print} =
   (List.foreach
    (!symbols, fn {name, ty, symbolScope} =>
     let
        val (headerSymbolScope, symbolScope) =
           case symbolScope of
              SymbolScope.External =>
                 Error.bug "Ffi.declareExports.symbols: External"
            | SymbolScope.Private => ("MLLIB_PRIVATE", "PRIVATE")
            | SymbolScope.Public => ("MLLIB_PUBLIC", "PUBLIC")
        val headerDecl =
           concat [headerSymbolScope,
                   "(extern ",
                   CType.toString ty, " ",
                   name, ";)"]
        val decl =
           concat [symbolScope, " ",
                   CType.toString ty, " ",
                   name]
     in
       List.push (headers, headerDecl);
       print (decl ^ ";\n")
     end);
    if List.isEmpty (!exports)
       then ()
       else print "MLtonCallFromC ()\n";
    List.foreach
    (!exports, fn {args, convention, id, name, res, symbolScope} =>
     let
        val args =
           Vector.mapi
           (args, fn (i,t) =>
            let
               val x = concat ["x", Int.toString i]
               val t = CType.toString t
            in
               (concat [t, " ", x],
                concat ["\tlocalOpArgsRes[", Int.toString (i + 1), "] = ",
                        "(CPointer)(&", x, ");\n"])
            end)
        val (headerSymbolScope, symbolScope) =
           case symbolScope of
              SymbolScope.External =>
                 Error.bug "Ffi.declareExports.exports: External"
            | SymbolScope.Private => ("MLLIB_PRIVATE","PRIVATE")
            | SymbolScope.Public => ("MLLIB_PUBLIC","PUBLIC")
        val prototype =
           concat [case res of
                      NONE => "void"
                    | SOME t => CType.toString t,
                   if convention <> Convention.Cdecl
                      then concat [" __attribute__ ((",
                                   Convention.toString convention,
                                   ")) "]
                   else " ",
                   name, " (",
                   concat (List.separate (Vector.toListMap (args, #1), ", ")),
                   ")"]
        val n =
           1 + (Vector.length args)
           + (case res of NONE => 0 | SOME _ => 1)
     in
        List.push (headers, concat [headerSymbolScope, "(", prototype, ";)"])
        ; print (concat [symbolScope, " ", prototype, " {\n"])
        ; print (concat ["\tCPointer localOpArgsRes[", Int.toString n,"];\n"])
        ; print (concat ["\tInt32 localOp = ", Int.toString id, ";\n",
                         "\tlocalOpArgsRes[0] = (CPointer)(&localOp);\n"])
        ; Vector.foreach (args, fn (_, set) => print set)
        ; (case res of
              NONE => ()
            | SOME t =>
                 print (concat ["\t", CType.toString t, " localRes;\n",
                                "\tlocalOpArgsRes[", Int.toString (Vector.length args + 1), "] = ",
                                "(CPointer)(&localRes);\n"]))
        ; print ("\tMLton_callFromC (localOpArgsRes);\n")
        ; (case res of
              NONE => ()
            | SOME _ => print "\treturn localRes;\n")
        ; print "}\n"
     end))

fun exportHeader f =
   File.withOut
   (f, fn out =>
    let
       fun print s = Out.output (out, s)
       val libname = !Control.libname
       val libcap = CharVector.map Char.toUpper libname
       val _ = print ("#ifndef __" ^ libcap ^ "_ML_H__\n")
       val _ = print ("#define __" ^ libcap ^ "_ML_H__\n")
       val _ = print "\n"
       val _ =
          File.outputContents
          (concat [!Control.libDir, "/include/ml-types.h"], out)
       val _ = print "\n"
       val _ =
          File.outputContents
          (concat [!Control.libDir, "/include/export.h"], out)
       val _ = print "\n"
       (* How do programs link against this library by default *)
       val defaultLinkage =
          case !Control.format of
             Control.Archive    => "STATIC_LINK"
           | Control.Executable => "PART_OF"
           | Control.LibArchive => "NO_DEFAULT_LINK"
           | Control.Library    => "DYNAMIC_LINK"
       val _ =
          print ("#if !defined(PART_OF_"      ^ libcap ^ ") && \\\n\
                 \    !defined(STATIC_LINK_"  ^ libcap ^ ") && \\\n\
                 \    !defined(DYNAMIC_LINK_" ^ libcap ^ ")\n")
       val _ =
          print ("#define " ^ defaultLinkage ^ "_" ^ libcap ^ "\n")
       val _ = print "#endif\n"
       val _ = print "\n"
       val _ = print ("#if defined(PART_OF_" ^ libcap ^ ")\n")
       val _ = print "#define MLLIB_PRIVATE(x) PRIVATE x\n"
       val _ = print "#define MLLIB_PUBLIC(x) PUBLIC x\n"
       val _ = print ("#elif defined(STATIC_LINK_" ^ libcap ^ ")\n")
       val _ = print "#define MLLIB_PRIVATE(x)\n"
       val _ = print "#define MLLIB_PUBLIC(x) PUBLIC x\n"
       val _ = print ("#elif defined(DYNAMIC_LINK_" ^ libcap ^ ")\n")
       val _ = print "#define MLLIB_PRIVATE(x)\n"
       val _ = print "#define MLLIB_PUBLIC(x) EXTERNAL x\n"
       val _ = print "#else\n"
       val _ = print ("#error Must specify linkage for " ^ libname ^ "\n")
       val _ = print "#define MLLIB_PRIVATE(x)\n"
       val _ = print "#define MLLIB_PUBLIC(x)\n"
       val _ = print "#endif\n"
       val _ = print "\n"
       val _ = print "#ifdef __cplusplus\n"
       val _ = print "extern \"C\" {\n"
       val _ = print "#endif\n"
       val _ = print "\n"
       val _ =
          if !Control.format = Control.Executable then () else
              (print ("MLLIB_PUBLIC(void " ^ libname ^ "_open(int argc, const char** argv);)\n")
              ;print ("MLLIB_PUBLIC(void " ^ libname ^ "_close();)\n"))
       val _ = declareExports {print = fn _ => ()}
       val _ = List.foreach (!headers, fn s => (print s; print "\n"))
       val _ = print "\n"
       val _ = print "#undef MLLIB_PRIVATE\n"
       val _ = print "#undef MLLIB_PUBLIC\n"
       val _ = print "\n"
       val _ = print "#ifdef __cplusplus\n"
       val _ = print "}\n"
       val _ = print "#endif\n"
       val _ = print "\n"
       val _ = print ("#endif /* __" ^ libcap ^ "_ML_H__ */\n")
    in
       ()
    end)

val exportHeader =
   Control.trace (Control.Detail, "exportHeader") exportHeader

end
