#include "eep0018.h"    

/* 
 * See ei documentation at 
 *
 * http://erlang.org/doc/man/ei.html 
 */

// The current state
typedef struct {
  ei_x_buff ei_buf;
  int skip_value_list_header;
} State;

#define FALSE 0
#define TRUE  -1

static void inline value_list_header(State* pState) {
  if(!pState->skip_value_list_header) 
    ei_x_encode_list_header(&pState->ei_buf, 1);
  else
    pState->skip_value_list_header = FALSE;
}

static int erl_json_ei_null(void* ctx) {
  State* pState = (State*) ctx;
  
  flog(stderr, "null", 0, 0, 0);
  
  value_list_header(pState);
  ei_x_encode_atom_len(&pState->ei_buf, "null", 4);

  return 1;
}


static int erl_json_ei_boolean(void* ctx, int boolVal) {
  State* pState = (State*) ctx;

  flog(stderr, boolVal ? "true" : "false", 0, 0, 0);
  
  value_list_header(pState);
  
  if(boolVal)
    ei_x_encode_atom_len(&pState->ei_buf, "true", 4);
  else
    ei_x_encode_atom_len(&pState->ei_buf, "false", 5);

  return 1;
}

static int erl_json_ei_integer(void * ctx, long val) {
  State* pState = (State*) ctx;

  value_list_header(pState);
  ei_x_encode_long(&pState->ei_buf, val);
  
  return 1;
}

static int erl_json_ei_double(void * ctx, double val) {
  State* pState = (State*) ctx;

  value_list_header(pState);
  ei_x_encode_double(&pState->ei_buf, val);

  return 1;
}

static int erl_json_ei_number(void* ctx, const char * val, unsigned int len) {
  State* pState = (State*) ctx;

  value_list_header(pState);
  
  ei_x_encode_tuple_header(&pState->ei_buf, 2);
  ei_x_encode_atom_len(&pState->ei_buf, "number", 6);
  ei_x_encode_string_len(&pState->ei_buf, val, len);
  
  return 1;
}

static int erl_json_ei_string(void* ctx, const unsigned char* val, unsigned int len) {
  State* pState = (State*) ctx;

  flog(stderr, "string", 0, (const char*)val, len);
  
  value_list_header(pState);
  ei_x_encode_binary(&pState->ei_buf, val, len);
  return 1;
}

static int erl_json_ei_start_map(void* ctx) {
  flog(stderr, "start map", 0, 0, 0);

  return 1;
}

static int erl_json_ei_end_map(void* ctx) {
  State* pState = (State*) ctx;

  flog(stderr, "end map", 0, 0, 0);
  ei_x_encode_empty_list(&pState->ei_buf);
  return 1;
}

static int erl_json_ei_map_key(void* ctx, const unsigned char* buf, unsigned int len) {
  State* pState = (State*) ctx;

  flog(stderr, "map key", 0, buf, len);
  ei_x_encode_list_header(&pState->ei_buf, 1);
  
  ei_x_encode_tuple_header(&pState->ei_buf, 2);
  // ei_x_encode_string_len(&pState->ei_buf, (const char*) buf, len);
  ei_x_encode_binary(&pState->ei_buf, buf, len);
  
  pState->skip_value_list_header = TRUE;
  
  return 1;
}

static int erl_json_ei_start_array(void* ctx) {
  flog(stderr, "start array", 0, 0, 0);
  return 1;
}

static int erl_json_ei_end_array(void* ctx) {
  State* pState = (State*) ctx;

  flog(stderr, "end array", 0, 0, 0);
  ei_x_encode_empty_list(&pState->ei_buf);

  return 1;
}

/*
 * This setting sends numbers as a {number, <<"String">>} tuple
 * back to erlang. This needs some work on the erlang side.
 */ 
static yajl_callbacks callbacks_w_number_tuple = {
  erl_json_ei_null,
  erl_json_ei_boolean,
  NULL,
  NULL,
  erl_json_ei_number,
  erl_json_ei_string,
  erl_json_ei_start_map,
  erl_json_ei_map_key,
  erl_json_ei_end_map,
  erl_json_ei_start_array,
  erl_json_ei_end_array
};

/*
 * This setting sends numbers as (long) integers or doubles back 
 * to erlang. If a number is outside the valid range parsing fails. 
 */ 
static yajl_callbacks callbacks_w_numbers = {
  erl_json_ei_null,
  erl_json_ei_boolean,
  erl_json_ei_integer,
  erl_json_ei_double,
  NULL, // erl_json_ei_number,
  erl_json_ei_string,
  erl_json_ei_start_map,
  erl_json_ei_map_key,
  erl_json_ei_end_map,
  erl_json_ei_start_array,
  erl_json_ei_end_array
};

void json_parse_ei(ErlDrvData session, const unsigned char* s, int len, int parseInArray) {
  ErlDrvPort port = (ErlDrvPort) session;

  /*
   * initialize state and buffer
   */
  State state;
  ei_x_new_with_version(&state.ei_buf);
  state.skip_value_list_header = FALSE;

  /* get a parser handle */
  yajl_parser_config conf = { YAJL_ALLOW_COMMENTS, YAJL_CHECK_UTF8 };
  yajl_handle handle = yajl_alloc(&callbacks_w_numbers, &conf, &state);

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
    // fwrite(buf, 1, len, stdout);
  }  

  send_data(port, EEP0018_EI, state.ei_buf.buff, state.ei_buf.index);
  ei_x_free(&state.ei_buf);
}
