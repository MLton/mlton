/* This implementation of setenv has a space leak, but I don't see how to avoid
 * it, since the specification of putenv is that it uses the memory for its arg.
 */
int setenv (const char *name, const char *value, int overwrite) {
        size_t len;
        char *b;

        if (!overwrite && getenv (name))
                return 0;

        len = strlen (name) + strlen (value) + 2 /* = and \000 */;
        b = malloc (len);
        snprintf (b, len, "%s=%s", name, value);
        return putenv (b);
}
