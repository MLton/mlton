int fixedGetrusage (int who, struct rusage *rup) {
        return getrusage (who, rup);
}
