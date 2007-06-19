/* Simulates MSG_DONTWAIT using fcntl() and O_NONBLOCK. */

static void fd_modify(int fd, int flags, int add, int shouldRemove)
{
        if (flags & MSG_DONTWAIT) {
                int f = fcntl(fd, F_GETFL);
                fcntl(fd, F_SETFL, (f | add) & ~shouldRemove);
        }
}

static void set_nonblock(int fd, int flags)
{
        fd_modify(fd, flags, O_NONBLOCK, 0);
}

static void clear_nonblock(int fd, int flags)
{
        fd_modify(fd, flags, 0, O_NONBLOCK);
}

int MLton_recv(int s, void *buf, int len, int flags)
{
        int ret;
        set_nonblock(s, flags);
        ret = recv(s, buf, len, flags & ~MSG_DONTWAIT);
        clear_nonblock(s, flags);
        return ret;
}

int MLton_recvfrom(int s, void *buf, int len, int flags, void *from,
                   socklen_t *fromlen)
{
        int ret;
        set_nonblock(s, flags);
        ret = recvfrom(s, buf, len, flags & ~MSG_DONTWAIT, from, fromlen);
        clear_nonblock(s, flags);
        return ret;
}
