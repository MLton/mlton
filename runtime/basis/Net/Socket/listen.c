#include <sys/types.h>
#include <sys/socket.h>
#include "mlton-basis.h"
#include "my-lib.h"

Int Socket_listen(Int s, Int backlog) {
	return listen(s, backlog);
}
