#define MLton_Platform_Arch_host "amd64"

#define POINTER_BITS 64
#if (defined (__CYGWIN__) || defined (__MINGW32__))
#define ADDRESS_BITS 43
#elif (defined (__linux__))
#define ADDRESS_BITS 48
#else
#define ADDRESS_BITS 40
#endif
