
/* On AIX 5.1 (and older) there is no fegetround() or fesetround().
   Instead, float.h defines fp_read_rnd() and fp_swap_rnd() with
   equivalent functionality.  GCC has its own version of float.h, so
   we include the system header directly before everything else. */
#include "/usr/include/float.h"
#include "platform.h"

#include <sys/mman.h>
#include <sys/procfs.h>
#include <sys/vminfo.h>

#include "getrusage.c"
#include "mkdir2.c"
#include "recv.nonblock.c"
#include "ssmmap.c"
#include "use-mmap.c"

int fegetround(void)
{
        return fp_read_rnd ();
}

void fesetround(int mode)
{
        fp_swap_rnd (mode);
}

int fpclassify64(double d)
{
        int c;
        c = class (d);
        switch (c) {
        case FP_PLUS_NORM:
        case FP_MINUS_NORM:
                return FP_NORMAL;
        case FP_PLUS_ZERO:
        case FP_MINUS_ZERO:
                return FP_ZERO;
        case FP_PLUS_INF:
        case FP_MINUS_INF:
                return FP_INFINITE;
        case FP_PLUS_DENORM:
        case FP_MINUS_DENORM:
                return FP_SUBNORMAL;
        case FP_SNAN:
        case FP_QNAN:
                return FP_NAN;
        default:
                die ("Real_class error: invalid class %d\n", c);
        }
}

W32 totalRam (GC_state s) {
        struct vminfo info;
        int pagesize;

        pagesize = sysconf (_SC_PAGESIZE);
        if (vmgetinfo (&info, VMINFO, sizeof(info)) < 0)
                diee ("totalRam error: vmgetinfo failed\n");
        return info.memsizepgs * pagesize;
}


struct map_type {
        int flag;
        char *type;
};

static struct map_type map_types[] =
        {{MA_MAINEXEC, "main"},
         {MA_KERNTEXT, "kern"},
         {MA_SHARED,   "shared"},
         {MA_STACK,    "stack"},
         {0, NULL}};


struct map_segment {
        prptr64_t start;
        prptr64_t end;
        char *name;
};

static struct map_segment map_segments[] =
        {{0x00000000, 0x0fffffff, "kernel"},
         /* Application program text. */
         {0x10000000, 0x1fffffff, "text"},
         /* Application program data and the application stack. */
         {0x20000000, 0x2fffffff, "data"},
         /* Available for use by shared memory or mmap services. */
         {0x30000000, 0xafffffff, "mmap"},
         /* Shared library text. */
         {0xd0000000, 0xdfffffff, "shtext"},
         /* Miscellaneous kernel data. */
         {0xe0000000, 0xefffffff, "kdata"},
         /* Application shared library data. */
         {0xf0000000, 0xffffffff, "shdata"},
         {0, 0, NULL}};


static char *
get_map_type(int flags, prptr64_t addr)
{
        struct map_type *m;

        for (m = map_types; m->flag; m++)
                if (m->flag & flags)
                        return m->type;
        if ((addr >= 0xd0000000 && addr <= 0xdfffffff)
            || (addr >= 0xf0000000 && addr <= 0xffffffff))
                return "shlib";
        return "";
}

static char *
get_map_segment(prptr64_t addr)
{
        struct map_segment *m;

        for (m = map_segments; m->name; m++)
                if (m->start <= addr && m->end >= addr)
                        return m->name;
        return "";
}

#define BUFLEN 65536

void showMem(void)
{
        pid_t pid = getpid ();
        char fname[128];
        int fd = 0;
        char *buf;
        struct prmap *map;

        printf ("va_start  va_end perm type  segment file (member) [object]\n");
        printf ("--------+--------+---+------+------+----------------------\n");

        snprintf (fname, sizeof (fname), "/proc/%d/map", pid);
        fd = open (fname, O_RDONLY);
        if (fd == -1)
                diee ("showMem error: opening %s failed", fname);

        /* I couldn't figure out a way to get the size of the map file
           beforehand (only open, read, write, and close work on files under
           /proc), so let's just hope that 64k will be enough. */
        buf = malloc (BUFLEN);
        if (buf == NULL)
                die ("showMem error: out of memory.");

        read (fd, buf, BUFLEN);
        map = (struct prmap*)buf;

        for (map = (struct prmap*)buf; map->pr_size; map++) {
                char *m = buf + map->pr_pathoff;
                m += strlen (m) + 1;
                if (!m[0])
                        m = NULL;
                printf ("%08llx %08llx %s%s%s %-6s %-6s %s %s%s%s[%s]\n",
                       map->pr_vaddr, map->pr_vaddr + map->pr_size,
                       map->pr_mflags & MA_READ  ? "r" : "-",
                       map->pr_mflags & MA_WRITE ? "w" : "-",
                       map->pr_mflags & MA_EXEC  ? "x" : "-",
                       get_map_type (map->pr_mflags, map->pr_vaddr),
                       get_map_segment (map->pr_vaddr),
                       buf + map->pr_pathoff,
                       m ? "(" : "", m ? m : "", m ? ") " : "",
                       map->pr_mapname);
        }
}
