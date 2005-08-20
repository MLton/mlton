(* hash.sml
 * 2005 Matthew Fluet (mfluet@acm.org)
 *  Adapted for MLton.
 *)

(*
 * hash.sml - Generating unique hash codes for C function types and
 *            for ML types.
 *
 *  (C) 2002, Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
structure Hash : sig
    val mkFHasher : unit -> Spec.cft -> int
    val mkTHasher : unit -> PrettyPrint.mltype -> int
end = struct

    structure S = Spec
    structure PP = PrettyPrint
    structure SM = StringMap
    structure LM = IntListMap

    fun tyConId S.SCHAR = 0
      | tyConId S.UCHAR = 1
      | tyConId S.SSHORT = 2
      | tyConId S.USHORT = 3
      | tyConId S.SINT = 4
      | tyConId S.UINT = 5
      | tyConId S.SLONG = 6
      | tyConId S.ULONG = 7
      | tyConId S.SLONGLONG = 8
      | tyConId S.ULONGLONG = 9
      | tyConId S.FLOAT = 10
      | tyConId S.DOUBLE = 11

    fun conConId S.RW = 0
      | conConId S.RO = 1

    fun look (next, find, insert) tab k =
        case find (!tab, k) of
            SOME i => i
          | NONE => let
                val i = !next
            in
                next := i + 1;
                tab := insert (!tab, k, i);
                i
            end

    fun mkFHasher () = let
        val stab = ref SM.empty
        val utab = ref SM.empty
        val etab = ref SM.empty
        val ltab = ref LM.empty

        val next = ref 13

        val tlook = look (next, SM.find, SM.insert)
        val llook = look (next, LM.find, LM.insert) ltab

        fun hash (S.STRUCT t) = tlook stab t
          | hash (S.UNION t) = tlook utab t
          | hash (S.ENUM (t, _)) = tlook etab t
          | hash (S.FPTR x) = cfthash x
          | hash (S.PTR (c, ty)) = llook [1, conConId c, hash ty]
          | hash (S.ARR { t, d, esz }) = llook [2, hash t, d, esz]
          | hash (S.BASIC ty) = tyConId ty
          | hash (S.VOIDPTR) = 12
          | hash _ = raise Fail "hash"

        and cfthash { args, res } = llook (0 :: opthash res :: map hash args)

        and opthash NONE = 0
          | opthash (SOME ty) = 1 + hash ty
    in
        cfthash
    end

    fun mkTHasher () = let
        val stab = ref SM.empty
        val ltab = ref LM.empty

        val next = ref 0

        val slook = look (next, SM.find, SM.insert) stab
        val llook = look (next, LM.find, LM.insert) ltab

        fun hash (PP.ARROW (t, t')) = llook [0, hash t, hash t']
          | hash (PP.TUPLE tl) = llook (1 :: map hash tl)
          | hash (PP.CON (c, tl)) = llook (2 :: slook c :: map hash tl)
          | hash (PP.RECORD pl) = llook (3 :: map phash pl)

        and phash (n, t) = llook [4, slook n, hash t]
    in
        hash
    end
end
