/* On AIX 5.1 (and older) there is no fegetround() or fesetround().
   Instead, float.h defines fp_read_rnd() and fp_swap_rnd() with
   equivalent functionality.  GCC has its own version of float.h, so
   we include the system header directly before everything else. */
#include "/usr/include/float.h"
#include "platform.h"

#include <sys/mman.h>
#include <sys/procfs.h>
#include <sys/vminfo.h>

#include "diskBack.unix.c"
#include "mkdir2.c"
#include "mmap-protect.c"
#include "nonwin.c"
#include "recv.nonblock.c"
#include "use-mmap.c"

int fegetround (void)
{
        return fp_read_rnd ();
}

void fesetround (int mode)
{
        fp_swap_rnd (mode);
}

int fpclassify64 (double d)
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

size_t GC_pageSize (void) {
        long pageSize;

        pageSize = sysconf (_SC_PAGESIZE);
        if (pageSize < 0)
                diee ("GC_pageSize error: sysconf (_SC_PAGESIZE) failed");

        return (size_t)pageSize;
}

/* We cannot use _SC_PHYS_PAGES from sysconf.c.  It fails on some
   versions of AIX. */
uintmax_t GC_physMem (void) {
        struct vminfo vminfo;
        uintmax_t physMem;

        if (vmgetinfo (&vminfo, VMINFO, sizeof (vminfo)) < 0)
                diee ("GC_physMem error: vmgetinfo failed");

        physMem = (uintmax_t)vminfo.memsizepgs * (uintmax_t)4096;
        return physMem;
}


struct map_type {
        int flag;
        const char *type;
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
        const char *name;
};

static struct map_segment map_segments[] =
        {{(prptr64_t)0x00000000, (prptr64_t)0x0fffffff, "kernel"},
         /* Application program text. */
         {(prptr64_t)0x10000000, (prptr64_t)0x1fffffff, "text"},
         /* Application program data and the application stack. */
         {(prptr64_t)0x20000000, (prptr64_t)0x2fffffff, "data"},
         /* Available for use by shared memory or mmap services. */
         {(prptr64_t)0x30000000, (prptr64_t)0xafffffff, "mmap"},
         /* Shared library text. */
         {(prptr64_t)0xd0000000, (prptr64_t)0xdfffffff, "shtext"},
         /* Miscellaneous kernel data. */
         {(prptr64_t)0xe0000000, (prptr64_t)0xefffffff, "kdata"},
         /* Application shared library data. */
         {(prptr64_t)0xf0000000, (prptr64_t)0xffffffff, "shdata"},
         {0, 0, NULL}};


static const char *
get_map_type (int flags, prptr64_t addr)
{
        struct map_type *m;

        for (m = map_types; m->flag; m++)
                if (m->flag & flags)
                        return m->type;
        if ((addr >= (prptr64_t)0xd0000000 && addr <= (prptr64_t)0xdfffffff)
            || (addr >= (prptr64_t)0xf0000000 && addr <= (prptr64_t)0xffffffff))
                return "shlib";
        return "";
}

static const char *
get_map_segment (prptr64_t addr)
{
        struct map_segment *m;

        for (m = map_segments; m->name; m++)
                if (m->start <= addr && m->end >= addr)
                        return m->name;
        return "";
}

#define BUFLEN 65536

void GC_displayMem (void)
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
