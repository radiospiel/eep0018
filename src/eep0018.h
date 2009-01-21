#ifndef EEP0018_H
#define EEP0018_H

/* commands */

#define EEP0018_PARSE_EI                   2

/* parameters */

#define EEP0018_PARSE_VALUE                0x01

#define EEP0018_PARSE_NUMBERS_AS_NUMBER    0x02
#define EEP0018_PARSE_NUMBERS_AS_FLOAT     0x04
#define EEP0018_PARSE_NUMBERS_AS_TUPLE     0x08

#define EEP0018_PARSE_NUMBERS_MASK         (EEP0018_PARSE_NUMBERS_AS_NUMBER|\
                EEP0018_PARSE_NUMBERS_AS_FLOAT|\
                EEP0018_PARSE_NUMBERS_AS_TUPLE)

#define EEP0018_EI       17

/* project includes */

#include "log.h"    
#include <assert.h>    

/* YAJL includes and settings */

#include <yajl/yajl_parse.h>
#include <yajl/yajl_gen.h>

#define YAJL_ALLOW_COMMENTS 1
#define YAJL_CHECK_UTF8 1

/* ei includes */

#include "ei.h"    
#include <erl_driver.h>

static inline ErlDrvPort CtxToPort(void* ctx) {
  return (ErlDrvPort)ctx;
}

static inline void output(void* ctx, const char* data, int len) {
  driver_output(CtxToPort(ctx), (char*)data, len);
}

static inline void output2(void* ctx, const char* data, int len, const char* data2, int len2) {
  driver_output2(CtxToPort(ctx), (char*)data, len, (char*)data2, len2);
}

static inline int send_data(void* ctx, char type, const char* data, unsigned int len)
{ 
  flog(stderr, "->>>", type, data, len);

  assert(!data || len);

  if(!data)
    output(ctx, &type, 1);
  else
    output2(ctx, &type, 1, data, len); 
  
  return 1;
}

extern void json_parse(ErlDrvData session, const unsigned char* s, int len, int opts);
extern void json_parse_to_ei(ErlDrvData session, const unsigned char* s, int len, int opts);

#endif
