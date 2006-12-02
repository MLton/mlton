/* ------------------------------------------------- */
/*                       Posix                       */
/* ------------------------------------------------- */

__attribute__ ((noreturn))
void Posix_IO_setbin (__attribute__ ((unused)) C_Fd_t fd) {
        die("Posix_IO_setbin not implemented");
}

__attribute__ ((noreturn))
void Posix_IO_settext (__attribute__ ((unused)) C_Fd_t fd) {
        die("Posix_IO_settext not implemented");
}

/* ------------------------------------------------- */
/*                      Process                      */
/* ------------------------------------------------- */

__attribute__ ((noreturn))
C_Errno_t(C_PId_t) MLton_Process_cwait (__attribute__ ((unused)) C_PId_t pid, 
                                        __attribute__ ((unused)) Ref(C_Status_t) status) {
        die("MLton_Process_cwait not implemented");
}
