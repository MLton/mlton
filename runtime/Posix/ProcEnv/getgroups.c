#define _ISOC99_SOURCE
#define _BSD_SOURCE
#define _POSIX_SOURCE

#include "platform.h"

/* It's easier to let the basis library deal with an array of known-size
 * words than to deal with a gid_t size that varies from platform to platform.
 * So, this code copies the array of gid_t's to the array of words passed by the
 * basis.
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
