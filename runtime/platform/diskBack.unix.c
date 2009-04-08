static FILE *tempFileDes (void) {
  int fd;
  FILE *f;
  char *template;
  const char *tmpDir;
  const char *tag = "/TempFileXXXXXXXXXX";
  size_t tmpDirLen, tagLen;
  mode_t m;

  tmpDir = getenv ("TMP");
  if (NULL == tmpDir) {
    tmpDir = getenv ("TMPDIR");
    if (NULL == tmpDir)
      tmpDir = "/var/tmp";
  }
  tmpDirLen = strlen(tmpDir);
  tagLen = strlen(tag);
  template = malloc_safe (tmpDirLen + tagLen + 1);
  strncpy (template, tmpDir, tmpDirLen + 1);
  strncpy (template + tmpDirLen, tag, tagLen + 1);
  m = umask(077);
  fd = mkstemp_safe (template);
  f = fdopen_safe (fd, "w+");
  (void)umask(m);
  unlink_safe (template);
  free (template);
  return f;
}

typedef struct {
  FILE *f;
} *WriteToDiskData;

void GC_diskBack_read (void *data, pointer buf, size_t size) {
  FILE *f;

  const size_t READ_CHUNK_SIZE = 0x2000000; /* 32M */

  f = ((WriteToDiskData)data)->f;
  fseek_safe (f, 0, SEEK_SET);
  /* fread (_, 1, size, _) succeeds
   * with size >= 2^31
   * for a 32-bit executable on 64-bit linux.
   * Nonetheless, match GC_diskBack_write.
   */
  while (size > 0) {
    size_t s = min (READ_CHUNK_SIZE, size);
    fread_safe (buf, 1, s, f);
    buf += s;
    size -= s;
  }
}

void GC_diskBack_close (void *data) {
  FILE *f;

  f = ((WriteToDiskData)data)->f;
  fclose_safe (f);
  free (data);
}

void *GC_diskBack_write (pointer buf, size_t size) {
  FILE *f;
  WriteToDiskData d;

  const size_t WRITE_CHUNK_SIZE = 0x2000000; /* 32M */

  f = tempFileDes ();
  /* fwrite (_, 1, size, _) fails
   * (with no helpful error conditions!)
   * with size >= 2^31
   * on x86-linux.
   */
  while (size > 0) {
    size_t s = min (WRITE_CHUNK_SIZE, size);
    fwrite_safe (buf, 1, s, f);
    buf += s;
    size -= s;
  }
  d = (WriteToDiskData)(malloc_safe (sizeof(*d)));
  d->f = f;
  return d;
}
