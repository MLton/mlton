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

  f = ((WriteToDiskData)data)->f;
  fseek_safe (f, 0, SEEK_SET);
  fread_safe (buf, 1, size, f);
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

  f = tempFileDes ();
  fwrite_safe (buf, 1, size, f);
  d = (WriteToDiskData)(malloc_safe (sizeof(*d)));
  d->f = f;
  return d;
}
