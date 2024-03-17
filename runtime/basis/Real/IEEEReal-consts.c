#include "platform.h"

const C_Int_t IEEEReal_RoundingMode_FE_NOSUPPORT = FE_NOSUPPORT;

#ifdef FE_TONEAREST
const C_Int_t IEEEReal_RoundingMode_FE_TONEAREST = FE_TONEAREST;
#else
const C_Int_t IEEEReal_RoundingMode_FE_TONEAREST = FE_NOSUPPORT;
#endif

#ifdef FE_DOWNWARD
const C_Int_t IEEEReal_RoundingMode_FE_DOWNWARD = FE_DOWNWARD;
#else
const C_Int_t IEEEReal_RoundingMode_FE_DOWNWARD = FE_NOSUPPORT;
#endif

#ifdef FE_UPWARD
const C_Int_t IEEEReal_RoundingMode_FE_UPWARD = FE_UPWARD;
#else
const C_Int_t IEEEReal_RoundingMode_FE_UPWARD = FE_NOSUPPORT;
#endif

#ifdef FE_TOWARDZERO
const C_Int_t IEEEReal_RoundingMode_FE_TOWARDZERO = FE_TOWARDZERO;
#else
const C_Int_t IEEEReal_RoundingMode_FE_TOWARDZERO = FE_NOSUPPORT;
#endif
