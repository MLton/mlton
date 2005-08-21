(* c-int.sig
 * 2005 Matthew Fluet (mfluet@acm.org)
 *  Adapted for MLton.
 *)

(*
 * A "private" extension to the encoding of C types in SML.
 *   The routines here are for use by code that will be automatically
 *   generated from corresponding C files.  User code is not supposed
 *   to access them because they are unsafe.  (As if subverting the C
 *   type system were such a big deal...)
 *
 *   (C) 2001, Lucent Technologies, Bell Laboratories
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
signature C_INT = sig

    include C

    type addr = CMemory.addr

    (* make struct or union size from its size given as a word;
     * needs explicit type constraint *)
    val mk_su_size : word -> 's S.size

    (* make struct or union RTTI given its corresponding size *)
    val mk_su_typ : 's su S.size -> 's su T.typ

    (* make function pointer type give the ML function that
     * implements the calling protocol *)
    val mk_fptr_typ : (addr -> 'a -> 'b) -> ('a -> 'b) fptr T.typ

    (* mk_obj' makes light-weight objects *)
    val mk_obj' : addr -> ('t, 'c) obj'

    (* make a void* from an address *)
    val mk_voidptr : addr -> voidptr

    (* given the function that implements the calling protocol and the
     * functions raw address, make a function pointer *)
    val mk_fptr : (addr -> 'a -> 'b) * addr -> ('a -> 'b) fptr

    (* making normal and const-declared struct- or union-fields 
     * given the field's type and its offset *)
    val mk_rw_field : 'm T.typ * int * ('s, 'c) su_obj -> ('m, 'c) obj
    val mk_ro_field : 'm T.typ * int * ('s, 'c) su_obj -> ('m, ro) obj

    (* light version *)
    (* NOTE: We do not pass RTTI to the light version (which would
     * internally throw it away anyway).  This means that we
     * will need an explicit type constraint. *)
    val mk_field' : int * ('s, 'ac) su_obj' -> ('m, 'rc) obj'

    (* making normal signed bitfields *)
    val mk_rw_sbf : int * word * word -> (* offset * bits * shift *)
                    ('s, 'c) su_obj -> 'c sbf
    val mk_ro_sbf : int * word * word -> (* offset * bits * shift *)
                    ('s, 'c) su_obj -> ro sbf

    (* light versions *)
    val mk_rw_sbf' : int * word * word -> (* offset * bits * shift *)
                     ('s, 'c) su_obj' -> 'c sbf
    val mk_ro_sbf' : int * word * word -> (* offset * bits * shift *)
                     ('s, 'c) su_obj' -> ro sbf

    (* making normal unsigned bitfields *)
    val mk_rw_ubf : int * word * word -> (* offset * bits * shift *)
                    ('s, 'c) su_obj -> 'c ubf
    val mk_ro_ubf : int * word * word -> (* offset * bits * shift *)
                    ('s, 'c) su_obj -> ro ubf

    (* light versions *)
    val mk_rw_ubf' : int * word * word -> (* offset * bits * shift *)
                     ('s, 'c) su_obj' -> 'c ubf
    val mk_ro_ubf' : int * word * word -> (* offset * bits * shift *)
                     ('s, 'c) su_obj' -> ro ubf

    (* reveal address behind void*; this is used to
     * implement the function-call protocol for functions that have
     * pointer arguments *)
    val reveal : voidptr -> addr
    val freveal : 'f fptr' -> addr

    val vcast : addr -> voidptr
    val pcast : addr -> 'o ptr'
    val fcast : addr -> 'f fptr'

    (* unsafe low-level array subscript that does not require RTTI *)
    val unsafe_sub : int ->             (* element size *)
                     (('t, 'n) arr, 'c) obj' * int ->
                     ('t, 'n) obj'
end
