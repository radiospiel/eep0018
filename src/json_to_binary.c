#include "erl_interface.h"
#include "ei.h"    

#include "log.h"    

#include <stdlib.h>

#include <yajl/yajl_parse.h>
#include <yajl/yajl_gen.h>

#include <ei.h>

#include "putget.h"

/* 
 * See ei documentation at 
 *
 * http://erlang.org/doc/man/ei.html 
 */

// The current pState->
typedef struct {
  ei_x_buff *buf;
  int skip_value_list_header;
} State;

#define FALSE 0
#define TRUE  -1

static void inline value_list_header(State* pState) {
  if(!pState->skip_value_list_header) 
    ei_x_encode_list_header(pState->buf, 1);
  else
    pState->skip_value_list_header = FALSE;
}

static int erl_json_null(void* ctx) {
  State* pState = (State*) ctx;
  
  flog(stderr, "null", 0, 0, 0);
  
  value_list_header(pState);
  ei_x_encode_atom_len(pState->buf, "null", 4);

  return 1;
}


static int erl_json_boolean(void* ctx, int boolVal) {
  State* pState = (State*) ctx;

  flog(stderr, boolVal ? "true" : "false", 0, 0, 0);
  
  value_list_header(pState);
  
  if(boolVal)
    ei_x_encode_atom_len(pState->buf, "true", 4);
  else
    ei_x_encode_atom_len(pState->buf, "false", 5);

  return 1;
}

static int erl_json_number(void* ctx, const char * numberVal, unsigned int numberLen) {
  State* pState = (State*) ctx;

  flog(stderr, "number", 0, (const char*)numberVal, numberLen);

  value_list_header(pState);
  ei_x_encode_ulong(pState->buf, 1); /* just an example */
  
  return 1;
}

static int erl_json_string(void* ctx, const unsigned char * stringVal, unsigned int stringLen) {
  State* pState = (State*) ctx;

  flog(stderr, "string", 0, (const char*)stringVal, stringLen);
  
  value_list_header(pState);
  ei_x_encode_binary(pState->buf, stringVal, stringLen);
  return 1;
}
 
static int erl_json_start_map(void* ctx) {
  flog(stderr, "start map", 0, 0, 0);

  return 1;
}

static int erl_json_end_map(void* ctx) {
  State* pState = (State*) ctx;

  flog(stderr, "end map", 0, 0, 0);
  ei_x_encode_empty_list(pState->buf);
  return 1;
}

static int erl_json_map_key(void* ctx, const unsigned char* buf, unsigned int len) {
  State* pState = (State*) ctx;

  flog(stderr, "map key", 0, buf, len);
  ei_x_encode_list_header(pState->buf, 1);
  
  ei_x_encode_tuple_header(pState->buf, 2);
  ei_x_encode_string_len(pState->buf, (const char*) buf, len);
  
  pState->skip_value_list_header = TRUE;
  
  return 1;
}

static int erl_json_start_array(void* ctx) {
  flog(stderr, "start array", 0, 0, 0);
  return 1;
}

static int erl_json_end_array(void* ctx) {
  State* pState = (State*) ctx;

  flog(stderr, "end array", 0, 0, 0);
  ei_x_encode_empty_list(pState->buf);

  return 1;
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

void json_to_binary(ei_x_buff *buf, const unsigned char* s, int len)
{
  /*
   * initialize state
   */
  State state;
  state.buf = buf;
  state.skip_value_list_header = FALSE;
  
  flog(stderr, "json_to_binary", 0, 0, 0);
  
  /* get a parser handle */
  yajl_parser_config conf = { ALLOW_COMMENTS, CHECK_UTF8 };
  yajl_handle handle = yajl_alloc(&erl_json_callbacks, &conf, &state);

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
