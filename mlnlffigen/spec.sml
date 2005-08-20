(* spec.sml
 * 2005 Matthew Fluet (mfluet@acm.org)
 *  Adapted for MLton.
 *)

(*
 * spec.sml - A data structure describing the export interface of a
 *            C program.
 *
 *  (C) 2001, Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
structure Spec = struct

    datatype constness = RO | RW
    type tag = string

    datatype basic_ctype =
        SCHAR | UCHAR 
      | SSHORT | USHORT 
      | SINT | UINT 
      | SLONG | ULONG
      | SLONGLONG | ULONGLONG
      | FLOAT | DOUBLE

    datatype ctype =
        BASIC of basic_ctype
      | VOIDPTR
      | STRUCT of tag
      | UNION of tag
      | ENUM of tag * bool
      | FPTR of cft
      | PTR of cobj
      | ARR of { t: ctype, d: int, esz: int }
      | UNIMPLEMENTED of string

    withtype cft = { args: ctype list, res: ctype option }

    and cobj = constness * ctype

    datatype fieldspec =
        OFIELD of { offset: int, spec: cobj, synthetic: bool }
      | SBF of { offset: int, constness: constness, bits: word, shift: word }
      | UBF of { offset: int, constness: constness, bits: word, shift: word }

    type field = { name: string, spec: fieldspec }

    type s =
         { src: string,
           tag: tag, 
           anon: bool, 
           size: word, 
           fields: field list,
           exclude: bool }
    type u =
         { src: string,
           tag: tag, 
           anon: bool, 
           size: word, 
           all: field list,
           exclude: bool }

    type gty = { src: string, name: string, spec: ctype }

    type gvar = { src: string, name: string, spec: cobj }

    type gfun = { src: string,
                  name: string, 
                  spec: cft, 
                  argnames: string list option }

    type enumval = { name: string, spec: LargeInt.int }

    type enum = { src: string,
                  tag: tag, 
                  anon: bool, 
                  descr: string, 
                  spec: enumval list,
                  exclude: bool }

    type spec = { structs: s list,
                  unions: u list,
                  gtys: gty list,
                  gvars: gvar list,
                  gfuns: gfun list,
                  enums: enum list }

    fun join (x: spec, y: spec) = let
        fun uniq sel = let
            fun loop ([], a) = rev a
              | loop (h :: t, a) =
                loop (t, if List.exists
                                (fn x => (sel x : string) = sel h) a then a
                         else h :: a)
        in
            loop
        end
    in
        { structs = uniq #tag (#structs x, #structs y),
          unions = uniq #tag (#unions x, #unions y),
          gtys = uniq #name (#gtys x, #gtys y),
          gvars = uniq #name (#gvars x, #gvars y),
          gfuns = uniq #name (#gfuns x, #gfuns y),
          enums = uniq #tag (#enums x, #enums y) } : spec
    end

    val empty : spec = { structs = [], 
                         unions = [], 
                         gtys = [], 
                         gvars = [],
                         gfuns = [], 
                         enums = [] }
end
