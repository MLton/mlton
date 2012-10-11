#define MLton_Platform_Arch_host "arm"

/* Work around broken rounding mode control on ARM
 *
 * ARM CPUs can be running with either hardware or software.
 * Neither support rounding modes with fe{get,set}round.
 * Rounding direction is part of the instruction format.
 *
 * Unfortunately, due to vendor stupidity, every OS deals
 * with this differently:
 *  Some pretend to support it and ignore calls.
 *  Some throw an error even for the supported mode (TO_NEAREST).
 *  All of them define all of the TO_* macros in defiance of the standard.
 * ... basically, the system libraries are useless.
 *
 * The solution for MLton: replace fe{get,set}round with our own honest
 * versions which accept TO_NEAREST and fail for other values.
 */

#undef fegetround
#undef fesetround
#define fegetround MLton_fegetround
#define fesetround MLton_fesetround

static inline int fegetround(void) {
  return FE_TONEAREST;
}

static inline int fesetround(int rounding_mode) {
  return (rounding_mode==FE_TONEAREST)?0:1;
}
