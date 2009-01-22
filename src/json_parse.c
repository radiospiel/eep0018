#include "eep0018.h"    
#include <string.h>
#include <stdlib.h>

/*
 * 
 *  This parser encodes the "sax" stream of json tokens into ei. Note that 
 *  this is not a one-on-one match: ei lists are different from JSON arrays.
 *
 *  For example, this is how we encode [true,[false]] in ei:
 *
 *    ei_x_encode_list_header(&state.ei_buf, 1);        // start array
 *    ei_x_encode_atom_len(&state.ei_buf, "true", 4);   // atom
 *    ei_x_encode_list_header(&state.ei_buf, 1);        // next value
 *    ei_x_encode_list_header(&state.ei_buf, 1);        // start array
 *    ei_x_encode_atom_len(&state.ei_buf, "false", 5);  // atom
 *    ei_x_encode_empty_list(&state.ei_buf);            // end array
 *    ei_x_encode_empty_list(&state.ei_buf);            // end array
 *
 *  i.e. all values are prependad an arity-one list header, except
 *  on the first item in the array. Sounds weird?!
 */

typedef struct {
  ei_x_buff ei_buf;
  
  int options;
  
  /*
   * How to encode a list:
   *
   * The ei man page states that a list can be encoded as
   *  list_header(1) Value list_header(1) Value ... list_header(0)
   * while an empty list would be encoded as 
   *  list_header(0)
   *
   * Now a value may appear only in a list, as the key in an object
   * or as the value in an object. In both object cases we do have 
   * complete control about the occurence: after all both key and
   * value are comprised of exactly one value,
   *
   * That gives us the following rules on whether or not to prepend
   * a value (incl lists and objects) with an ei list header:
   *
   *  
   *
   * In other words: a value has to be prepended a list_header(1), 
   * if it appears inside a list. 
   */
  /*
   * skip_list_header_for_value might contain the index of the last 
   * list header. This is used to undo the last list header 
   * for empty lists, and to remember that we are on a list head.
   */
  int skip_list_header_for_value;
} State;

static inline int numbers_as(State* p) {
  return (p->options & EEP0018_PARSE_NUMBERS_MASK);
}

static inline int keys_as(State* p) {
  return (p->options & EEP0018_PARSE_KEYS_MASK);
}

#ifndef NDEBUG

static int ei_x_encode_list_header_log(ei_x_buff* x, int arity) {
  fprintf(stderr, "<< list header: %d\n", arity); 
  return ei_x_encode_list_header(x, arity);
}

static int ei_x_encode_empty_list_log(ei_x_buff* x) {
  fprintf(stderr, "<< list header: %d\n", 0); 
  return ei_x_encode_empty_list(x);
}

static int ei_x_encode_tuple_header_log(ei_x_buff* x, int arity) {
  fprintf(stderr, "<< tuple header: %d\n", arity); 
  return ei_x_encode_tuple_header(x, arity);
}

#define ei_x_encode_list_header   ei_x_encode_list_header_log
#define ei_x_encode_tuple_header  ei_x_encode_tuple_header_log
#define ei_x_encode_empty_list    ei_x_encode_empty_list_log

#endif

static void inline list_header_for_value(State* pState) {
  if(!pState->skip_list_header_for_value) 
    ei_x_encode_list_header(&pState->ei_buf, 1);
  else
    pState->skip_list_header_for_value = 0;
}

static int erl_json_ei_null(void* ctx) {
  State* pState = (State*) ctx;
  
  flog(stderr, "null", 0, 0, 0);
  
  list_header_for_value(pState);
  ei_x_encode_atom_len(&pState->ei_buf, "null", 4);

  return 1;
}


static int erl_json_ei_boolean(void* ctx, int boolVal) {
  State* pState = (State*) ctx;

  flog(stderr, boolVal ? "true" : "false", 0, 0, 0);
  
  list_header_for_value(pState);
  
  if(boolVal)
    ei_x_encode_atom_len(&pState->ei_buf, "true", 4);
  else
    ei_x_encode_atom_len(&pState->ei_buf, "false", 5);

  return 1;
}

static int erl_json_ei_number(void* ctx, const char * val, unsigned int len) {
  State* pState = (State*) ctx;

  flog(stderr, "number", 0, val, len);

  list_header_for_value(pState);

  switch(numbers_as(pState)) {
    case EEP0018_PARSE_NUMBERS_AS_NUMBER:
    {
      if(memchr(val, '.', len) || memchr(val, 'e', len) || memchr(val, 'E', len))
        ei_x_encode_double(&pState->ei_buf, strtod(val, 0));
      else
        ei_x_encode_long(&pState->ei_buf, strtol(val, 0, 10));
      break;
    }
    case EEP0018_PARSE_NUMBERS_AS_FLOAT:
    {
      ei_x_encode_double(&pState->ei_buf, strtod(val, 0));
      break;
    }
    case EEP0018_PARSE_NUMBERS_AS_TUPLE:
    {
      /* 
        While "1e1" is a valid JSON number, it is not a valid parameter to list_to_float/1 
        We fix that by inserting ".0" before the exponent e. 
      */
      const char* exp = memchr(val, 'e', len);
      if(exp && exp > val) {
        const char* dot = memchr(val, '.', exp - val);
        if(!dot) {
          char* tmp = alloca(len + 5);
          memcpy(tmp, val, exp - val);
          memcpy(tmp + (exp - val), ".0", 2);
          memcpy(tmp + (exp - val) + 2, exp, len - (exp - val));
          len += 2;
          val = tmp;
          tmp[len] = 0;
        }
      }

      ei_x_encode_tuple_header(&pState->ei_buf, 3);
      ei_x_encode_atom_len(&pState->ei_buf, "number", 6);
      ei_x_encode_string_len(&pState->ei_buf, val, len);
      ei_x_encode_atom_len(&pState->ei_buf, "a", 1);      // a dummy
      break;
    }
  }

  
  return 1;
}

static int erl_json_ei_string(void* ctx, const unsigned char* val, unsigned int len) {
  State* pState = (State*) ctx;

  flog(stderr, "string", 0, (const char*)val, len);
  
  list_header_for_value(pState);
  ei_x_encode_binary(&pState->ei_buf, val, len);
  return 1;
}

static int erl_json_ei_start_array(void* ctx) {
  State* pState = (State*) ctx;

  flog(stderr, "start array", 0, 0, 0);
  
  list_header_for_value(pState);
  
  pState->skip_list_header_for_value = pState->ei_buf.index;
  ei_x_encode_list_header(&pState->ei_buf, 1);

  return 1;
}

static int erl_json_ei_end_array(void* ctx) {
  State* pState = (State*) ctx;

  flog(stderr, "end array", 0, 0, 0);
  
  if(pState->skip_list_header_for_value) {
    pState->ei_buf.index = pState->skip_list_header_for_value;
    pState->skip_list_header_for_value = 0;
  }
  ei_x_encode_empty_list(&pState->ei_buf);

  return 1;
}

static int erl_json_ei_start_map(void* ctx) {
  //State* pState = (State*) ctx;
  flog(stderr, "start map", 0, 0, 0);
  
  // list_header_for_value(pState);
  // ei_x_encode_tuple_header(&pState->ei_buf, 1);

  return erl_json_ei_start_array(ctx);
}

static int erl_json_ei_end_map(void* ctx) {
  State* pState = (State*) ctx;

  flog(stderr, "end map", 0, 0, 0);
  
  if(pState->skip_list_header_for_value) {
    ei_x_encode_tuple_header(&pState->ei_buf, 0);
    pState->skip_list_header_for_value = 0;
  }
  ei_x_encode_empty_list(&pState->ei_buf);

  return 1;
}

static int erl_json_ei_map_key(void* ctx, const unsigned char* buf, unsigned int len) {
  State* pState = (State*) ctx;

  flog(stderr, "map key", 0, buf, len);

  list_header_for_value(pState);
  
  ei_x_encode_tuple_header(&pState->ei_buf, 2);

  switch(keys_as(pState)) {
    case EEP0018_PARSE_KEYS_AS_ATOM:
      ei_x_encode_atom_len(&pState->ei_buf, buf, len);
      break;
    case EEP0018_PARSE_KEYS_AS_BINARY:
      ei_x_encode_binary(&pState->ei_buf, buf, len);
      break;
  }

  pState->skip_list_header_for_value = -1;
  
  return 1;
}


/*
 * This setting sends numbers as a {number, <<"String">>} tuple
 * back to erlang. This needs some work on the erlang side.
 */ 
static yajl_callbacks callbacks = {
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

void json_parse_to_ei(ErlDrvData session, const unsigned char* s, int len, int opts) {
  int parseValue = opts & EEP0018_PARSE_VALUE;
  
  unsigned char* yajl_msg = 0;
  unsigned char* msg = 0;
  ErlDrvPort port = (ErlDrvPort) session;

  /*
   * initialize yajl parser
   */
  State state;
  ei_x_new_with_version(&state.ei_buf);
  state.skip_list_header_for_value = -1;
  state.options = opts;

  yajl_parser_config conf = { YAJL_ALLOW_COMMENTS, YAJL_CHECK_UTF8 };
  yajl_handle handle = yajl_alloc(&callbacks, &conf, &state);

  /* start parser */
  yajl_status stat;
  if(parseValue) stat = yajl_parse(handle, (const unsigned char*) "[ ", 2);
  stat = yajl_parse(handle, s, len);
  if(parseValue) stat = yajl_parse(handle, (const unsigned char*) " ]", 2);

  /*
   * sometimes the parser is still stuck inside a JSON token. This finishs
   * the token no matter what.
   */
  if(stat == yajl_status_insufficient_data)
    stat = yajl_parse(handle, (const unsigned char*) " ", 1);

  /* 
   * get an error message on errors
   */
  switch(stat) {
    case yajl_status_ok: break;
    case yajl_status_insufficient_data: 
      msg = (unsigned char*)"Insufficient data"; 
      break;
    default:
      msg = yajl_msg = yajl_get_error(handle, 0, s, len);
      break;
  }

  /* 
   * if result is not ok: we write {error, "reason"} instead. This is 
   * something that will never be encoded from any JSON data.
   */
  if(msg) {
    ei_x_free(&state.ei_buf);
    ei_x_new_with_version(&state.ei_buf);

    ei_x_encode_tuple_header(&state.ei_buf, 2);
    ei_x_encode_atom_len(&state.ei_buf, "error", 5);
    ei_x_encode_string_len(&state.ei_buf, (const char*) msg, strlen((const char*) msg));

    if(yajl_msg) yajl_free_error(yajl_msg);
  } 

  send_data(port, EEP0018_EI, state.ei_buf.buff, state.ei_buf.index);
  ei_x_free(&state.ei_buf);
}
