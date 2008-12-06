#include "erl_interface.h"
#include "ei.h"    

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

#define CONTAINER_LIST  0x01
#define CONTAINER_MAP   0x02

typedef struct {
  unsigned int mode;
  unsigned int pos;
  unsigned int arity;
}  ContainerState;

#define MAX_CONTAINERS_DEPTH 64    

// The current pState->
typedef struct {
  ei_x_buff buf;                                        /* ei's buffer */
  
  ContainerState containers[MAX_CONTAINERS_DEPTH];
  unsigned int csp;
} State;

/* starts a new container */
static void container_open(State *pState, int mode) {
  pState->csp++;

  pState->containers[pState->csp].mode = mode;  
  pState->containers[pState->csp].pos = pState->buf.index;
  pState->containers[pState->csp].arity = 0;
}

/* starts a new container */
static void container_register_element(State *pState, int mode) {
  if(pState->containers[pState->csp].mode == mode)
    pState->containers[pState->csp].arity++;
}

static void container_close(State *pState) {
  pState->csp--;
}

static void container_fix_arity(State *pState) {
  char* s = pState->buf.buff + pState->containers[pState->csp].pos;

  s += 1;
  put32be(s,pState->containers[pState->csp].arity);
}

static int erl_json_null(void* ctx) {
  State* pState = (State*) ctx;
   
  container_register_element(pState, CONTAINER_LIST);
  
  ei_x_encode_atom_len(&pState->buf, "null", 4);
  return 1;
}

static int erl_json_boolean(void* ctx, int boolVal) {
  State* pState = (State*) ctx;
   
  container_register_element(pState, CONTAINER_LIST);
  
  if(boolVal)
    ei_x_encode_atom_len(&pState->buf, "true", 4);
  else
    ei_x_encode_atom_len(&pState->buf, "false", 5);

  return 1;
}

static int erl_json_number(void* ctx, const char * numberVal, unsigned int numberLen) {
  State* pState = (State*) ctx;
   
  container_register_element(pState, CONTAINER_LIST);
  
  return 1;
}

static int erl_json_string(void* ctx, const unsigned char * stringVal, unsigned int stringLen) {
  State* pState = (State*) ctx;
   
  container_register_element(pState, CONTAINER_LIST);

  /* as a string: ei_x_encode_string_len(&pState->buf, stringVal, stringLen); */
  /* as a binary */
  ei_x_encode_binary(&pState->buf, stringVal, stringLen);
  return 1;
}

#define VERY_LARGE_ARITY 0xCAFE           /* Must be large enough to not fit into a single byte */
 
static int erl_json_start_map(void* ctx) {
  State* pState = (State*) ctx;
   
  container_register_element(pState, CONTAINER_LIST);
  
  /*
   * We need the arity in here; however, at that very moment we don't even
   * know about it. Therefore we just use a VERY LARGE arity and fix it later.
   */
  container_open(pState, CONTAINER_MAP);

  /* encode a dummy arity header */
  ei_x_encode_tuple_header(&pState->buf, VERY_LARGE_ARITY);

  return 1;
}

static int erl_json_end_map(void* ctx) {
  State* pState = (State*) ctx;
   
  container_fix_arity(pState);
  container_close(pState);

  return 1;
}

static int erl_json_map_key(void* ctx, const unsigned char * key, unsigned int stringLen) {
  State* pState = (State*) ctx;
   
  container_register_element(pState, CONTAINER_MAP);

  return 1;
}

static int erl_json_start_array(void* ctx) {
  State* pState = (State*) ctx;
   
  container_register_element(pState, CONTAINER_LIST);

  /*
   * We need the arity in here; however, at that very moment we don't even
   * know about it. Therefore we just use a VERY LARGE arity and fix it later.
   */
  container_open(pState, CONTAINER_LIST);

  /* encode a dummy arity header */
  ei_x_encode_list_header(&pState->buf, VERY_LARGE_ARITY);

  return 1;
}

static int erl_json_end_array(void* ctx) {
  State* pState = (State*) ctx;

  if(pState->containers[pState->csp].arity == 0) {
    // This was an empty list. So we replace the list header with an 
    // empty list's header. This works because nothing got encoded
    // in the meantime. 
    pState->buf.index = pState->containers[pState->csp].pos;
    ei_x_encode_list_header(&pState->buf, 0);
  } 
  else {
    /* replace list arity w/correct value */
    container_fix_arity(pState);
  }

  container_close(pState);
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

void json_to_binary(const unsigned char* s, int len) {
  /*
   * initialize state
   */
  State state;
  ei_x_new_with_version(&state.buf);
  state.csp = 0;

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
  
  /*
   * free state
   */
  ei_x_free(&state.buf);
}
