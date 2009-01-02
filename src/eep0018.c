/* eep0018.c */

#include "erl_interface.h"
#include "ei.h"

#include "eep0018.h"

#include <stdlib.h>
#include <string.h>

#include <stdio.h>

#include <unistd.h>
#include <assert.h>
#include <erl_driver.h>

#include "log.h"    

/* the json parser part */

#include <yajl/yajl_parse.h>
#include <yajl/yajl_gen.h>

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

#define ALLOW_COMMENTS 1
#define CHECK_UTF8 1

static void json_parse(ErlDrvPort port, const unsigned char* s, int len) {
  /* get a parser handle */
  yajl_parser_config conf = { ALLOW_COMMENTS, CHECK_UTF8 };
  yajl_handle handle = yajl_alloc(&erl_json_callbacks, &conf, port);

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
    // flush_output(port);
  }  
}

/*
 * == OTP driver management ===========================================
 */

static ErlDrvData eep0018_drv_start(ErlDrvPort port, char *buf)
{
  set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
  return (ErlDrvData)port;
}

static void eep0018_drv_stop(ErlDrvData handle)
{
}

static void eep0018_drv_on_input(ErlDrvData session, char *buf, int len)
{
  ErlDrvPort port = (ErlDrvPort) session;

  switch(*buf) {
  case EEP0018_JSON_PARSE:
    json_parse(port, (unsigned char*) buf+1, len-1);
    break;

  case EEP0018_JSON_PARSE_EI: {
    ei_x_buff x;
    ei_x_new_with_version(&x);

#if 1
    json_to_binary(&x, (unsigned char*) buf+1, len-1);
#endif

#if 0     
    ei_x_encode_list_header(&x, 1);
    ei_x_encode_ulong(&x, 1); 
    ei_x_encode_list_header(&x, 1);
    ei_x_encode_ulong(&x, 2);
    ei_x_encode_empty_list(&x);
#endif

#if 0
    ei_x_encode_list_header(&x, 1);
    ei_x_encode_tuple_header(&x, 2);
    ei_x_encode_binary(&x, "key", 3);
    ei_x_encode_ulong(&x, 2); 
    ei_x_encode_empty_list(&x);
#endif

    send_data(port, EEP0018_EI, x.buff, x.index);
    ei_x_free(&x);
  break;
  }
  default:
    flog(stderr, "Unknown input", 0, buf, len);
  }
}

static ErlDrvEntry eep0018_driver_entry = {
  NULL,               /* F_PTR init, N/A */
  eep0018_drv_start,  /* L_PTR start, called when port is opened */
  eep0018_drv_stop,   /* F_PTR stop, called when port is closed */
  eep0018_drv_on_input,               /* F_PTR output, called when erlang has sent data to the port */
  NULL,               /* F_PTR ready_input, called when input descriptor ready to read*/
  NULL,               /* F_PTR ready_output, called when output descriptor ready to write */
  "eep0018_drv",      /* char *driver_name, the argument to open_port */
  NULL,               /* F_PTR finish, called when unloaded */
  NULL,               /* F_PTR control, port_command callback */
  NULL,               /* F_PTR timeout, reserved */
  NULL                /* F_PTR outputv, reserved */
};

DRIVER_INIT(eep0018_drv) /* must match name in driver_entry */
{
  flog(stderr, "driver loaded.\n", 0, 0, 0);
  return &eep0018_driver_entry;
}
