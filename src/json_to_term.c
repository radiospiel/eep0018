#include <stdlib.h>
#include <stdio.h>

#include "eep0018.h"

/* the json parser part */

#include <yajl/yajl_parse.h>
#include <yajl/yajl_gen.h>

static inline int send_data(void* ctx, char type, const char* data, unsigned int len)
{ 
  fprintf(stderr, "Sending %d byte of type %d\n", len, type);
  /*
  write type
  return if !data
  write len in network order
  return if !len
  write data
  */
  
  return 1;
}

static inline int send_atom(void* ctx, char atom)
{
  return send_data(ctx, EEP0018_ATOM, &atom, 1);
}

static int erl_json_null(void* ctx) {
  return send_atom(ctx, EEP0018_NULL);
}

static int erl_json_boolean(void* ctx, int boolVal) {
  return send_atom(ctx, boolVal ? EEP0018_TRUE : EEP0018_FALSE);
}

static int erl_json_number(void* ctx, const char * stringVal, unsigned int stringLen) {
  return send_data(ctx, EEP0018_NUMBER, (const char*) stringVal, stringLen);
}

static int erl_json_string(void* ctx, const unsigned char * stringVal, unsigned int stringLen) {
  return send_data(ctx, EEP0018_STRING, (const char*) stringVal, stringLen);
}

static int erl_json_start_map(void* ctx) {
  return send_data(ctx, EEP0018_MAP, NULL, 0);
}

static int erl_json_end_map(void* ctx) {
  return send_data(ctx, EEP0018_MAP_END, NULL, 0);
}

static int erl_json_map_key(void* ctx, const unsigned char * key, unsigned int stringLen) {
  return send_data(ctx, EEP0018_MAP_KEY, (const char*) key, stringLen);
}

static int erl_json_start_array(void* ctx) {
  return send_data(ctx, EEP0018_ARRAY, NULL, 0);
}

static int erl_json_end_array(void* ctx) {
  return send_data(ctx, EEP0018_ARRAY_END, NULL, 0);
}

static yajl_callbacks erl_json_callbacks = {
  erl_json_null,
  erl_json_boolean,
  NULL,
  NULL,
  erl_json_number,
  erl_json_string,
  erl_json_start_map,
  erl_json_map_key,
  erl_json_end_map,
  erl_json_start_array,
  erl_json_end_array
};

#define ALLOW_COMMENTS 1
#define CHECK_UTF8 1

static void parse_json(void* ctx, const unsigned char* s, int len) {
  /* get a parser handle */
  yajl_parser_config conf = { ALLOW_COMMENTS, CHECK_UTF8 };
  yajl_handle handle = yajl_alloc(&erl_json_callbacks, &conf, ctx);

  /* start parser */
  yajl_status stat = yajl_parse(handle, s, len);
  
  /* if result is not ok: we might raise an error?? */
  if (stat != yajl_status_ok)
  {
    unsigned char* msg =  yajl_get_error(handle, 0, s, len); /* non verbose error message */
    fprintf(stderr, "%s", (const char *) msg);
    yajl_free_error(msg);
  } 
  else /* result is ok: send encoded data */
  {
    // fwrite(buf, 1, len, stdout);
  }  
}
