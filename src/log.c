#include "eep0018.h"
#include "log.h"

#undef fdump
#undef flog

#include <ctype.h>

void fdump(FILE* file, const void *_buf, int len) {
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

static const char* log_string(unsigned char value) {
  #define test(x) case EEP0018_ ## x: return #x

  static char buf[20];
  
  switch(value) {
    case 0: return "";
    
    test(JSON_PARSE);
    
    test(ATOM);
    test(NUMBER);
    test(STRING);
    test(KEY);
    test(MAP);
    test(ARRAY);
    test(END);
  };

  sprintf(buf, "0x%02x", value);
  return buf;
}


void flog(FILE* file, const char* label, int mode, const void *buf, int len) {
  if(mode)
    fprintf(file, "%s *** %s(%d) %d byte *****\n", label, log_string(mode), mode, len);
  else if(buf)
    fprintf(file, "%s *** %d byte *****\n", label, len);
  else
    fprintf(file, "%s\n", label);

  fdump(file, buf, len);
}
