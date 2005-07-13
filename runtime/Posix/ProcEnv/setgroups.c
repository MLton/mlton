#include "platform.h"

Int Posix_ProcEnv_setgroups (Pointer groups) {
	gid_t *list;
	int res;
	int size;

	size = GC_arrayNumElements (groups);
	ARRAY (gid_t*, list, size);
	assert (size <= cardof (list));
	for (int i = 0; i < size; ++i)
		list[i] = ((Word*)groups)[i];
	res = setgroups (size, list);
	free (list);
	return res;
}
