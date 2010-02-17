/* Copyright (C) 2004-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _MLTON_CTYPES_H_
#define _MLTON_CTYPES_H_


/* C */
typedef /* _Bool */ Word8_t C_Bool_t;
typedef /* char */ Int8_t C_Char_t;
typedef /* signed char */ Int8_t C_SChar_t;
typedef /* unsigned char */ Word8_t C_UChar_t;
typedef /* short */ Int16_t C_Short_t;
typedef /* signed short */ Int16_t C_SShort_t;
typedef /* unsigned short */ Word16_t C_UShort_t;
typedef /* int */ Int32_t C_Int_t;
typedef /* signed int */ Int32_t C_SInt_t;
typedef /* unsigned int */ Word32_t C_UInt_t;
typedef /* long */ Int32_t C_Long_t;
typedef /* signed long */ Int32_t C_SLong_t;
typedef /* unsigned long */ Word32_t C_ULong_t;
typedef /* long long */ Int64_t C_LongLong_t;
typedef /* signed long long */ Int64_t C_SLongLong_t;
typedef /* unsigned long long */ Word64_t C_ULongLong_t;
typedef /* float */ Real32_t C_Float_t;
typedef /* double */ Real64_t C_Double_t;
typedef /* size_t */ Word64_t C_Size_t;

typedef /* unsigned char* */ Word64_t C_Pointer_t;
typedef /* char* */ Word64_t C_String_t;
typedef /* char** */ Word64_t C_StringArray_t;

/* Generic integers */
typedef C_Int_t C_Fd_t;
typedef C_Int_t C_Signal_t;
typedef C_Int_t C_Status_t;
typedef C_Int_t C_Sock_t;

/* C99 */
typedef /* ptrdiff_t */ Int64_t C_Ptrdiff_t;
typedef /* intmax_t */ Int64_t C_Intmax_t;
typedef /* uintmax_t */ Word64_t C_UIntmax_t;
typedef /* intptr_t */ Int64_t C_Intptr_t;
typedef /* uintptr_t */ Word64_t C_UIntptr_t;

/* from <dirent.h> */
typedef /* DIR* */ Word64_t C_DirP_t;

/* from <poll.h> */
typedef /* MLton_nfds_t */ Word32_t C_NFds_t;

/* from <resource.h> */
typedef /* MLton_rlim_t */ Word32_t C_RLim_t;

/* from <sys/types.h> */
typedef /* clock_t */ Int32_t C_Clock_t;
typedef /* dev_t */ Word32_t C_Dev_t;
typedef /* MLton_gid_t */ Word16_t C_GId_t;
typedef /* ino_t */ Word16_t C_INo_t;
typedef /* mode_t */ Word16_t C_Mode_t;
typedef /* MLton_nlink_t */ Int16_t C_NLink_t;
typedef /* off_t */ Int32_t C_Off_t;
typedef /* pid_t */ Int64_t C_PId_t;
typedef /* ssize_t */ Int64_t C_SSize_t;
typedef /* MLton_suseconds_t */ Int32_t C_SUSeconds_t;
typedef /* time_t */ Int64_t C_Time_t;
typedef /* MLton_uid_t */ Word16_t C_UId_t;

/* from <sys/socket.h> */
typedef /* socklen_t */ Int32_t C_Socklen_t;

/* from <termios.h> */
typedef /* MLton_cc_t */ Word8_t C_CC_t;
typedef /* MLton_speed_t */ Word32_t C_Speed_t;
typedef /* MLton_tcflag_t */ Word32_t C_TCFlag_t;

/* from "gmp.h" */
typedef /* mp_limb_t */ Word64_t C_MPLimb_t;

#define C_Errno_t(t) t

#endif /* _MLTON_CTYPES_H_ */
