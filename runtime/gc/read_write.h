/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

char readChar (int fd);
pointer readPointer (int fd);
objptr readObjptr (int fd);
size_t readSize (int fd);
uint32_t readUint32 (int fd);
uintptr_t readUintptr (int fd);
void writeChar (int fd, char c);
void writePointer (int fd, pointer p);
void writeObjptr (int fd, objptr op);
void writeSize (int fd, size_t z);
void writeUint32 (int fd, uint32_t u);
void writeUintptr (int fd, uintptr_t u);
void writeString (int fd, char* s);
void writeUint32U (int fd, uint32_t u);
void writeUintmaxU (int fd, uintmax_t u);
void writeUint32X (int fd, uint32_t u);
void writeUintmaxX (int fd, uintmax_t u);
void writeNewline (int fd);
