#include <unistd.h>
#include "mlton-posix.h"

#if (defined (__linux__))

Pid Posix_Process_fork() {
	return fork();
}

#elif (defined (__CYGWIN__))


/* There is a bug in Cygwin in that they don't wait in the parent so that the
 * child can copy the memory.  So, we make the parent wait on a signal from
 * the child.
 */

/* Unfortunately, there are some other Cygwin problems with signals that make
 * that approach not work either.  So, for now, I just have the parent sleep
 * in the hope that that gives the child time.
 */
Pid Posix_Process_fork() {
 	pid_t pid;

	pid = fork();
	if (0 != pid)
		sleep(1);
	return pid;
}

#if 0
#include <signal.h>
#include <stdio.h>
#define PARENT_CHILD_SIGNAL SIGUSR1

static volatile int handled = 0;
static void handler(int sig) {
	handled = 1;
}

Pid Posix_Process_fork() {
	struct sigaction act, oldact;
	pid_t pid;
	
	act.sa_flags = 0;
	act.sa_handler = &handler;
	sigemptyset(&act.sa_mask);
	sigaction(PARENT_CHILD_SIGNAL, &act, &oldact);
	pid = fork();
	if (0 == pid) {
		/* Child.  Send a signal to the parent. */
		kill(getppid(), PARENT_CHILD_SIGNAL);
		return 0;
	} else	{
		/* Parent.  Wait on signal from child. */
		while (!handled)
				sigsuspend(NULL);
		sigaction(PARENT_CHILD_SIGNAL, &oldact, NULL);
		return pid;
	}
}
#endif

#endif
