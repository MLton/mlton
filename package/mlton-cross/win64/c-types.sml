(* Copyright (C) 2004-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)


(* C *)
structure C_Bool = WordToBool (type t = Word8.word val zero: t = 0wx0 val one: t = 0wx1)
structure C_Char = struct open Int8 type t = int end
functor C_Char_ChooseIntN (A: CHOOSE_INTN_ARG) = ChooseIntN_Int8 (A)
structure C_SChar = struct open Int8 type t = int end
functor C_SChar_ChooseIntN (A: CHOOSE_INTN_ARG) = ChooseIntN_Int8 (A)
structure C_UChar = struct open Word8 type t = word end
functor C_UChar_ChooseWordN (A: CHOOSE_WORDN_ARG) = ChooseWordN_Word8 (A)
structure C_Short = struct open Int16 type t = int end
functor C_Short_ChooseIntN (A: CHOOSE_INTN_ARG) = ChooseIntN_Int16 (A)
structure C_SShort = struct open Int16 type t = int end
functor C_SShort_ChooseIntN (A: CHOOSE_INTN_ARG) = ChooseIntN_Int16 (A)
structure C_UShort = struct open Word16 type t = word end
functor C_UShort_ChooseWordN (A: CHOOSE_WORDN_ARG) = ChooseWordN_Word16 (A)
structure C_Int = struct open Int32 type t = int end
functor C_Int_ChooseIntN (A: CHOOSE_INTN_ARG) = ChooseIntN_Int32 (A)
structure C_SInt = struct open Int32 type t = int end
functor C_SInt_ChooseIntN (A: CHOOSE_INTN_ARG) = ChooseIntN_Int32 (A)
structure C_UInt = struct open Word32 type t = word end
functor C_UInt_ChooseWordN (A: CHOOSE_WORDN_ARG) = ChooseWordN_Word32 (A)
structure C_Long = struct open Int32 type t = int end
functor C_Long_ChooseIntN (A: CHOOSE_INTN_ARG) = ChooseIntN_Int32 (A)
structure C_SLong = struct open Int32 type t = int end
functor C_SLong_ChooseIntN (A: CHOOSE_INTN_ARG) = ChooseIntN_Int32 (A)
structure C_ULong = struct open Word32 type t = word end
functor C_ULong_ChooseWordN (A: CHOOSE_WORDN_ARG) = ChooseWordN_Word32 (A)
structure C_LongLong = struct open Int64 type t = int end
functor C_LongLong_ChooseIntN (A: CHOOSE_INTN_ARG) = ChooseIntN_Int64 (A)
structure C_SLongLong = struct open Int64 type t = int end
functor C_SLongLong_ChooseIntN (A: CHOOSE_INTN_ARG) = ChooseIntN_Int64 (A)
structure C_ULongLong = struct open Word64 type t = word end
functor C_ULongLong_ChooseWordN (A: CHOOSE_WORDN_ARG) = ChooseWordN_Word64 (A)
structure C_Float = struct open Real32 type t = real end
functor C_Float_ChooseRealN (A: CHOOSE_REALN_ARG) = ChooseRealN_Real32 (A)
structure C_Double = struct open Real64 type t = real end
functor C_Double_ChooseRealN (A: CHOOSE_REALN_ARG) = ChooseRealN_Real64 (A)
structure C_Size = struct open Word64 type t = word end
functor C_Size_ChooseWordN (A: CHOOSE_WORDN_ARG) = ChooseWordN_Word64 (A)

structure C_Pointer = struct open Word64 type t = word end
functor C_Pointer_ChooseWordN (A: CHOOSE_WORDN_ARG) = ChooseWordN_Word64 (A)
structure C_String = struct open Word64 type t = word end
functor C_String_ChooseWordN (A: CHOOSE_WORDN_ARG) = ChooseWordN_Word64 (A)
structure C_StringArray = struct open Word64 type t = word end
functor C_StringArray_ChooseWordN (A: CHOOSE_WORDN_ARG) = ChooseWordN_Word64 (A)

(* Generic integers *)
structure C_Fd = C_Int
functor C_Fd_ChooseIntN (A: CHOOSE_INTN_ARG) = C_Int_ChooseIntN (A)
structure C_Signal = C_Int
functor C_Signal_ChooseIntN (A: CHOOSE_INTN_ARG) = C_Int_ChooseIntN (A)
structure C_Status = C_Int
functor C_Status_ChooseIntN (A: CHOOSE_INTN_ARG) = C_Int_ChooseIntN (A)
structure C_Sock = C_Int
functor C_Sock_ChooseIntN (A: CHOOSE_INTN_ARG) = C_Int_ChooseIntN (A)

(* C99 *)
structure C_Ptrdiff = struct open Int64 type t = int end
functor C_Ptrdiff_ChooseIntN (A: CHOOSE_INTN_ARG) = ChooseIntN_Int64 (A)
structure C_Intmax = struct open Int64 type t = int end
functor C_Intmax_ChooseIntN (A: CHOOSE_INTN_ARG) = ChooseIntN_Int64 (A)
structure C_UIntmax = struct open Word64 type t = word end
functor C_UIntmax_ChooseWordN (A: CHOOSE_WORDN_ARG) = ChooseWordN_Word64 (A)
structure C_Intptr = struct open Int64 type t = int end
functor C_Intptr_ChooseIntN (A: CHOOSE_INTN_ARG) = ChooseIntN_Int64 (A)
structure C_UIntptr = struct open Word64 type t = word end
functor C_UIntptr_ChooseWordN (A: CHOOSE_WORDN_ARG) = ChooseWordN_Word64 (A)

(* from <dirent.h> *)
structure C_DirP = struct open Word64 type t = word end
functor C_DirP_ChooseWordN (A: CHOOSE_WORDN_ARG) = ChooseWordN_Word64 (A)

(* from <poll.h> *)
structure C_NFds = struct open Word32 type t = word end
functor C_NFds_ChooseWordN (A: CHOOSE_WORDN_ARG) = ChooseWordN_Word32 (A)

(* from <resource.h> *)
structure C_RLim = struct open Word32 type t = word end
functor C_RLim_ChooseWordN (A: CHOOSE_WORDN_ARG) = ChooseWordN_Word32 (A)

(* from <sys/types.h> *)
structure C_Clock = struct open Int32 type t = int end
functor C_Clock_ChooseIntN (A: CHOOSE_INTN_ARG) = ChooseIntN_Int32 (A)
structure C_Dev = struct open Word32 type t = word end
functor C_Dev_ChooseWordN (A: CHOOSE_WORDN_ARG) = ChooseWordN_Word32 (A)
structure C_GId = struct open Word16 type t = word end
functor C_GId_ChooseWordN (A: CHOOSE_WORDN_ARG) = ChooseWordN_Word16 (A)
structure C_INo = struct open Word16 type t = word end
functor C_INo_ChooseWordN (A: CHOOSE_WORDN_ARG) = ChooseWordN_Word16 (A)
structure C_Mode = struct open Word16 type t = word end
functor C_Mode_ChooseWordN (A: CHOOSE_WORDN_ARG) = ChooseWordN_Word16 (A)
structure C_NLink = struct open Int16 type t = int end
functor C_NLink_ChooseIntN (A: CHOOSE_INTN_ARG) = ChooseIntN_Int16 (A)
structure C_Off = struct open Int32 type t = int end
functor C_Off_ChooseIntN (A: CHOOSE_INTN_ARG) = ChooseIntN_Int32 (A)
structure C_PId = struct open Int64 type t = int end
functor C_PId_ChooseIntN (A: CHOOSE_INTN_ARG) = ChooseIntN_Int64 (A)
structure C_SSize = struct open Int64 type t = int end
functor C_SSize_ChooseIntN (A: CHOOSE_INTN_ARG) = ChooseIntN_Int64 (A)
structure C_SUSeconds = struct open Int32 type t = int end
functor C_SUSeconds_ChooseIntN (A: CHOOSE_INTN_ARG) = ChooseIntN_Int32 (A)
structure C_Time = struct open Int64 type t = int end
functor C_Time_ChooseIntN (A: CHOOSE_INTN_ARG) = ChooseIntN_Int64 (A)
structure C_UId = struct open Word16 type t = word end
functor C_UId_ChooseWordN (A: CHOOSE_WORDN_ARG) = ChooseWordN_Word16 (A)

(* from <sys/socket.h> *)
structure C_Socklen = struct open Int32 type t = int end
functor C_Socklen_ChooseIntN (A: CHOOSE_INTN_ARG) = ChooseIntN_Int32 (A)

(* from <termios.h> *)
structure C_CC = struct open Word8 type t = word end
functor C_CC_ChooseWordN (A: CHOOSE_WORDN_ARG) = ChooseWordN_Word8 (A)
structure C_Speed = struct open Word32 type t = word end
functor C_Speed_ChooseWordN (A: CHOOSE_WORDN_ARG) = ChooseWordN_Word32 (A)
structure C_TCFlag = struct open Word32 type t = word end
functor C_TCFlag_ChooseWordN (A: CHOOSE_WORDN_ARG) = ChooseWordN_Word32 (A)

(* from "gmp.h" *)
structure C_MPLimb = struct open Word64 type t = word end
functor C_MPLimb_ChooseWordN (A: CHOOSE_WORDN_ARG) = ChooseWordN_Word64 (A)

