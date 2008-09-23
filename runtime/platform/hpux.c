#include "platform.h"

#include <sys/mman.h>
#include <sys/newsig.h>
#include <sys/param.h>
#include <sys/pstat.h>

#define MAP_ANON MAP_ANONYMOUS

#include "diskBack.unix.c"
#include "mkdir2.c"
#include "mmap-protect.c"
#include "nonwin.c"
#include "recv.nonblock.c"
#include "setenv.putenv.c"
#include "use-mmap.c"

struct pstnames {
        int type;
        const char *name;
};

static struct pstnames pst_type_names[] =
        {{ PS_NOTUSED,      "unused" },
         { PS_USER_AREA,    "user" },
         { PS_TEXT,         "text" },
         { PS_DATA,         "data" },
         { PS_STACK,        "stack" },
         { PS_SHARED,       "shared" },
         { PS_NULLDEREF,    "null" },
         { PS_IO,           "io" },
         { PS_MMF,          "mmap" },
         { PS_GRAPHICS,     "gfx" },
         { PS_GRAPHICS_DMA, "gfxdma" },
#ifdef PS_RSESTACK
         { PS_RSESTACK,     "rsestack" },
#endif
         { 0, NULL }};

static const char *
pst_type_name(int type)
{
        int i;

        for (i = 0; pst_type_names[i].name; i++)
                if (pst_type_names[i].type == type)
                        return pst_type_names[i].name;
        return "unknown";
}

static const char*
pst_filename(struct pst_vm_status vm)
{
        static char fname[256];
#ifdef PSTAT_FILEDETAILS
        if (pstat_getpathname (fname, sizeof (fname), &vm.pst_fid) < 0)
#endif
                strcpy (fname, "unknown");
        return fname;
}

void GC_displayMem (void) {
        int i;
        struct pst_vm_status buf;
        size_t page_size = sysconf (_SC_PAGE_SIZE);

        printf("va_start  va_end  perms   type   phys   filename\n");
        printf("--------+--------+-----+-------+------+-----------\n");
        for (i = 0;; i++) {
                if (pstat_getprocvm (&buf, sizeof (buf), 0, i) < 0)
                        break;
                printf("%p %p  %s%s%s  %-8s %4d   %s\n",
                       (void*)buf.pst_vaddr,
                       (void*)(buf.pst_vaddr + buf.pst_length * page_size - 1),
                       (buf.pst_flags & PS_PROT_READ) ? "-" : "r",
                       (buf.pst_flags & PS_PROT_WRITE) ? "-" : "w",
                       (buf.pst_flags & PS_PROT_EXECUTE) ? "-" : "x",
                       pst_type_name (buf.pst_type),
                       buf.pst_phys_pages,
                       pst_filename (buf));
        }
}


static void catcher (__attribute__ ((unused)) int sig,
                     __attribute__ ((unused)) siginfo_t* sip,
                     void* mystery) {
        ucontext_t* ucp = (ucontext_t*)mystery;
        GC_handleSigProf ((code_pointer) (ucp->uc_link));
}

void GC_setSigProfHandler (struct sigaction *sa) {
        sa->sa_flags = SA_ONSTACK | SA_RESTART | SA_SIGINFO;
        sa->sa_sigaction = (void (*)(int, siginfo_t*, void*))catcher;
}

size_t GC_pageSize (void) {
        struct pst_static buf;

        if (pstat_getstatic (&buf, sizeof (buf), 1, 0) < 0)
                diee ("failed to get page size");
        return buf.page_size;
}

uintmax_t GC_physMem (void) {
        struct pst_static buf;

        if (pstat_getstatic (&buf, sizeof (buf), 1, 0) < 0)
                diee ("failed to get physical memory size");
        return buf.physical_memory * buf.page_size;
}

#ifdef __hppa__
float modff (float x, float *iptr)
{
        double d, i;
        d = modf ((double)x, &i);
        *iptr = (float)i;
        return d;
}

float rintf (float x) {
        return (float)rint ((double)x);
}

float frexpf (float x, int *e) {
        return (float)frexp ((double)x, e);
}

float ldexpf (float x, int e) {
        return (float)ldexp ((double)x, e);
}
#endif /* __hppa__ */
