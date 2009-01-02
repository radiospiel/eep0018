#include "eep0018.h"

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
  switch(*buf) {
  case EEP0018_JSON_PARSE:
  case EEP0018_JSON_PARSE_VALUE:
    json_parse(session, (unsigned char*) buf+1, len-1, *buf == EEP0018_JSON_PARSE_VALUE);
    break;

  case EEP0018_JSON_PARSE_EI: 
  case EEP0018_JSON_PARSE_VALUE_EI:
    json_parse_ei(session, (unsigned char*) buf+1, len-1, *buf == EEP0018_JSON_PARSE_VALUE_EI);
    break;
    
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
