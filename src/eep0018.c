/* eep0018.c */

#include <stdio.h>
#include <erl_driver.h>

int twice(int x){
  return 2*x;
}

int sum(int x, int y){
  return x+y;
}

typedef struct {
    ErlDrvPort port;
} eep0018_data;

static ErlDrvData eep0018_drv_start(ErlDrvPort port, char *buff)
{
    eep0018_data* d = (eep0018_data*)driver_alloc(sizeof(eep0018_data));
    d->port = port;
    return (ErlDrvData)d;
}

static void eep0018_drv_stop(ErlDrvData handle)
{
    driver_free((char*)handle);
}

static void eep0018_drv_output(ErlDrvData handle, char *buff, int buflen)
{
    eep0018_data* d = (eep0018_data*)handle;
    char fn = buff[0], arg = buff[1], res;
    if (fn == 1) {
      res = twice(arg);
    } else if (fn == 2) {
      res = sum(buff[1], buff[2]);
    }
    else {
      res = 3;
    }
    
    driver_output(d->port, &res, 1);
}

ErlDrvEntry eep0018_driver_entry = {
    NULL,               /* F_PTR init, N/A */
    eep0018_drv_start,  /* L_PTR start, called when port is opened */
    eep0018_drv_stop,   /* F_PTR stop, called when port is closed */
    eep0018_drv_output, /* F_PTR output, called when erlang has sent
			   data to the port */
    NULL,               /* F_PTR ready_input, 
                           called when input descriptor ready to read*/
    NULL,               /* F_PTR ready_output, 
                           called when output descriptor ready to write */
    "eep0018_drv",     /* char *driver_name, the argument to open_port */
    NULL,               /* F_PTR finish, called when unloaded */
    NULL,               /* F_PTR control, port_command callback */
    NULL,               /* F_PTR timeout, reserved */
    NULL                /* F_PTR outputv, reserved */
};

DRIVER_INIT(eep0018_drv) /* must match name in driver_entry */
{
    return &eep0018_driver_entry;
}
