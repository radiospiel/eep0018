#include "eep0018.h"    

/* the json parser part */

static int erl_json_null(void* ctx) {
  return send_data(ctx, EEP0018_ATOM, "null", 4);
}

static int erl_json_boolean(void* ctx, int boolVal) {
  if(boolVal) send_data(ctx, EEP0018_ATOM, "true", 4);
  else send_data(ctx, EEP0018_ATOM, "false", 5);
  
  return 1;
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
  return send_data(ctx, EEP0018_END, NULL, 0);
}

static int erl_json_map_key(void* ctx, const unsigned char * key, unsigned int stringLen) {
  return send_data(ctx, EEP0018_KEY, (const char*) key, stringLen);
}

static int erl_json_start_array(void* ctx) {
  return send_data(ctx, EEP0018_ARRAY, NULL, 0);
}

static int erl_json_end_array(void* ctx) {
  return send_data(ctx, EEP0018_END, NULL, 0);
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

void json_parse(ErlDrvData session, const unsigned char* s, int len, int parseInArray) {
  ErlDrvPort port = (ErlDrvPort) session;

  /* get a parser handle */
  yajl_parser_config conf = { YAJL_ALLOW_COMMENTS, YAJL_CHECK_UTF8 };
  yajl_handle handle = yajl_alloc(&erl_json_callbacks, &conf, port);

  /* start parser */
  yajl_status stat;
  if(parseInArray) stat = yajl_parse(handle, (const unsigned char*) "[ ", 2);
  stat = yajl_parse(handle, s, len);
  if(parseInArray) stat = yajl_parse(handle, (const unsigned char*) " ]", 2);
  
  /* if result is not ok: we might raise an error?? */
  if (stat != yajl_status_ok)
  {
    unsigned char* msg =  yajl_get_error(handle, 0, s, len); /* non verbose error message */
    fprintf(stderr, "%s", (const char *) msg);
    yajl_free_error(msg);
  } 
  else /* result is ok: send encoded data */
  {
    // flush_output(port);
  }  
}
