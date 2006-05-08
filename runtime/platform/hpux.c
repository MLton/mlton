#include "platform.h"

#include <sys/mman.h>
#define MAP_ANON MAP_ANONYMOUS

#include <sys/param.h>
#include <sys/pstat.h>
#include <sys/newsig.h>

#include "ssmmap.c"
#include "use-mmap.c"
#include "mkdir2.c"
#include "setenv.putenv.c"

W32 totalRam (GC_state s) {
        struct pst_static buf;

        if (pstat_getstatic (&buf, sizeof(buf), 1, 0) < 0)
                diee ("failed to get physical memory size");
        return buf.physical_memory * buf.page_size;
}


struct pstnames {
        int type;
        char *name;
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
        if (pstat_getpathname(fname, sizeof(fname), &vm.pst_fid) < 0)
#endif
                strcpy(fname, "unknown");
        return fname;
}

void showMem () {
        int i;
        struct pst_vm_status buf;
        size_t page_size = sysconf(_SC_PAGE_SIZE);

        printf("va_start  va_end  perms   type   phys   filename\n");
        printf("--------+--------+-----+-------+------+-----------\n");
        for (i = 0;; i++) {
                if (pstat_getprocvm (&buf, sizeof(buf), 0, i) < 0)
                        break;
                printf("%p %p  %s%s%s  %-8s %4d   %s\n",
                       (void*)buf.pst_vaddr,
                       (void*)buf.pst_vaddr + buf.pst_length * page_size - 1,
                       (buf.pst_flags & PS_PROT_READ) ? "-" : "r",
                       (buf.pst_flags & PS_PROT_WRITE) ? "-" : "w",
                       (buf.pst_flags & PS_PROT_EXECUTE) ? "-" : "x",
                       pst_type_name(buf.pst_type),
                       buf.pst_phys_pages,
                       pst_filename(buf));
        }
}


static void catcher (int sig, siginfo_t* sip, void* mystery) {
        ucontext_t* ucp = (ucontext_t*)mystery;
        GC_handleSigProf ((pointer) (ucp->uc_link));
}

void setSigProfHandler (struct sigaction *sa) {
        sa->sa_flags = SA_ONSTACK | SA_RESTART | SA_SIGINFO;
        sa->sa_sigaction = (void (*)(int, siginfo_t*, void*))catcher;
}

extern void *__text_start;
extern void *etext;

void *getTextStart () {
        return &__text_start;
}
void *getTextEnd () {
        return &etext;
}
