void Posix_IO_setbin (Fd fd, Bool useWindows) {
	if (useWindows)
		_setmode(fd, _O_BINARY);
	else
		/* cygwin has a different method for working with its fds */
		setmode(fd, O_BINARY);
}

void Posix_IO_settext (Fd fd, Bool useWindows) {
	if (useWindows)
		_setmode (fd, _O_TEXT);
	else
		/* cygwin has a different method for working with its fds */
		setmode (fd, O_TEXT);
}
