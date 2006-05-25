static int tempFileDes (void) {
  int fd;
  char *template;
  const char *tmpDir;
  const char *tag = "/TempFileXXXXXXXXXX";
  mode_t m;

  tmpDir = getenv ("TMP");
  if (NULL == tmpDir) {
    tmpDir = getenv ("TMPDIR");
    if (NULL == tmpDir)
      tmpDir = "/var/tmp";
  }
  template = malloc_safe (strlen(tmpDir) + strlen(tag) + 1);
  strcpy (template, tmpDir);
  strcat (template, tag);
  m = umask(077);
  fd = mkstemp_safe (template);
  (void)umask(m);
  unlink_safe (template);
  free (template);
  return fd;
}

typedef struct {
  int fd;
} *WriteToDiskData;

void diskBack_read (void *data, pointer buf, size_t size) {
  int fd;

  fd = ((WriteToDiskData)data)->fd;
  lseek (fd, 0, SEEK_SET);
  read_safe (fd, buf, size);
}

void diskBack_close (void *data) {
  int fd;

  fd = ((WriteToDiskData)data)->fd;
  close_safe (fd);
  free (data);
}

void *diskBack_write (pointer buf, size_t size) {
  int fd;
  WriteToDiskData d;

  fd = tempFileDes ();
  write_safe (fd, buf, size);
  d = (WriteToDiskData)(malloc_safe (sizeof(*d)));
  d->fd = fd;
  return d;
}
