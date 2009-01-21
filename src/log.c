#include "eep0018.h"
#include "log.h"

#undef fdump
#undef flog

#include <ctype.h>

void do_fdump(FILE* file, const void *_buf, int len) {
  const unsigned char * buf = (const unsigned char *)_buf;
  const int line_len = 16;
  int ofs = 0;
  while(len > 0) {
    int i;
    
    fprintf(file, "\t%04x   ", ofs);
    for(i=0; i<line_len; ++i) {
      fprintf(file, i < len ? "%02x " : "   ", (unsigned char)(buf[i]));
    }

    fprintf(file, " | ");
    for(i=0; i<line_len; ++i) {
      fprintf(file,  i < len ? "%c" : " ", isprint(buf[i]) ? buf[i] : '.');
    }
    fprintf(file, "\n");
    
    len -= line_len;
    buf += line_len;
    ofs += line_len;
  }
}

void do_flog(FILE* file, const char* label, int mode, const void *buf, int len) {
  if(mode)
    fprintf(file, "%s *** 0x%02x: %d byte *****\n", label, mode, len);
  else if(buf)
    fprintf(file, "%s *** %d byte *****\n", label, len);
  else
    fprintf(file, "%s\n", label);

  do_fdump(file, buf, len);
}
