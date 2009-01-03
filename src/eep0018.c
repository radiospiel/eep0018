#include "eep0018.h"
#include "log.h"

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
  {
    flog(stderr, "input", 0, buf, len);
  }
  
  char cmd = buf[0];
  char opts = buf[1];
  
  buf += 2; len -= 2;

  flog(stderr, "parsing", 0, buf, len);
  
  switch(cmd) {
  case EEP0018_JSON_PARSE_EI: 
    json_parse_ei(session, (unsigned char*) buf, len, opts);
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
