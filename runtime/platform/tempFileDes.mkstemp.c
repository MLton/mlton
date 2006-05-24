int tempFileDes (void) {
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
