#include <sys/types.h>
#include <unistd.h>
#include "mlton-posix.h"
#include "posix-constants.h"

/* Right now this is messy because MLton has no way of dealing with unsigned 
 * shorts (i.e. gid_t).
 */

Int Posix_ProcEnv_getgroups (Pointer groups) {
	int             i;
	int 		result;
	gid_t           groupList[Posix_ProcEnv_numgroups];

	result = getgroups (Posix_ProcEnv_numgroups, groupList);

	for (i = 0; i < result; i++)
		((Word *) groups)[i] = groupList[i];

	return result;
}
