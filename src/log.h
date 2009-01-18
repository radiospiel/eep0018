#ifndef LOG_H
#define LOG_H

#ifdef NDEBUG
#define NO_LOG
#endif

#include <stdio.h>

extern void do_fdump(FILE* file, const void *buf, int len);
extern void do_flog(FILE* file, const char* label, int mode, const void *buf, int len);


#ifndef NO_LOG

#define fdump do_fdump
#define flog do_flog

#else

#define nop()   do { (void)(0); } while(0)

#define fdump(file, buf, len)             nop()
#define flog(file, label, mode, buf, len) nop()

#endif

#endif
