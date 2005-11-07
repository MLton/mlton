#include "platform.h"

Int Posix_ProcEnv_setgroups (Pointer groups) {
        int i;
        gid_t *list;
        int res;
        int size;

        size = GC_getArrayLength (groups);
        list = (gid_t*)(calloc_safe (size, sizeof(*list)));
        assert (size <= (sizeof(list) / sizeof(*list)));
        for (i = 0; i < size; ++i)
                list[i] = ((Word*)groups)[i];
        res = setgroups (size, list);
        free (list);
        return res;
}
