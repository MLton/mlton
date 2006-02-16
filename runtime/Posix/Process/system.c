#include "platform.h"

int Posix_Process_system (const char* cmd) {
        return system (cmd);
}
