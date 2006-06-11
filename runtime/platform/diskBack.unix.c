static FILE *tempFileDes (void) {
  int fd;
  FILE *f;
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
  f = fdopen(f, "w+");
  (void)umask(m);
  unlink_safe (template);
  free (template);
  return f;
}

typedef struct {
  FILE *f;
} *WriteToDiskData;

void diskBack_read (void *data, pointer buf, size_t size) {
  FILE *f;

  f = ((WriteToDiskData)data)->f;
  fread_safe (buf, 1, size, f);
}

void diskBack_close (void *data) {
  FILE *f;

  f = ((WriteToDiskData)data)->f;
  fclose_safe (f);
  free (data);
}

void *diskBack_write (pointer buf, size_t size) {
  FILE *f;
  WriteToDiskData d;

  f = tempFileDes ();
  fwrite_safe (buf, 1, size, f);
  d = (WriteToDiskData)(malloc_safe (sizeof(*d)));
  d->f = f;
  return d;
}
