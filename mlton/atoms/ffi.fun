(* Copyright (C) 2004-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Ffi (S: FFI_STRUCTS): FFI = 
struct

open S

structure Convention = CFunction.Convention
structure SymbolScope = CFunction.SymbolScope

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
   val exportCounter = Counter.new 0
in
   fun addExport {args, convention, name, res, symbolScope} =
      let
         val id = Counter.next exportCounter
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
   let
      val _ = print "INTERNAL Pointer MLton_FFI_opArgsResPtr;\n"
   in
      List.foreach
      (!symbols, fn {name, ty, symbolScope} =>
       let
          val symbolScope = 
             case symbolScope of 
                SymbolScope.External =>
                   Error.bug "Ffi.declareExports.symbols: External"
              | SymbolScope.Private => "INTERNAL "
              | SymbolScope.Public => "EXPORTED "
          val decl = symbolScope ^ CType.toString ty ^ " " ^ name;
       in
         List.push (headers, "extern MLLIB_" ^ decl);
         print (decl ^ ";\n")
       end);
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
                          "(Pointer)(&", x, ");\n"])
              end)
          val header =
             concat [case symbolScope of 
                        SymbolScope.External =>
                           Error.bug "Ffi.declareExports.exports: External"
                      | SymbolScope.Private => "INTERNAL "
                      | SymbolScope.Public => "EXPORTED ",
                     case res of
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
          List.push (headers, "MLLIB_" ^ header)
          ; print (concat [header, " {\n"])
          ; print (concat ["\tPointer localOpArgsRes[", Int.toString n,"];\n"])
          ; print (concat ["\tMLton_FFI_opArgsResPtr = (Pointer)(localOpArgsRes);\n"])
          ; print (concat ["\tInt32 localOp = ", Int.toString id, ";\n",
                           "\tlocalOpArgsRes[0] = (Pointer)(&localOp);\n"])
          ; Vector.foreach (args, fn (_, set) => print set)
          ; (case res of
                NONE => ()
              | SOME t =>
                   print (concat ["\t", CType.toString t, " localRes;\n",
                                  "\tlocalOpArgsRes[", Int.toString (Vector.length args + 1), "] = ",
                                  "(Pointer)(&localRes);\n"]))
          ; print ("\tMLton_callFromC ();\n")
          ; (case res of
                NONE => ()
              | SOME _ => print "\treturn localRes;\n")
          ; print "}\n"
       end)
   end

fun declareHeaders {print} =
   (declareExports {print = fn _ => ()}
    ; List.foreach (!headers, fn s => (print s; print ";\n")))

end
