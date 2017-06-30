#include "example1.h"

#include "erl_driver.h"

typedef struct {
    ErlDrvPort port;
}example_data;

static ErlDrvData example_drv_start(ErlDrvPort port, char *buff) {
    example_data* d = (example_data *)driver_alloc(sizeof(example_data));
    d->port = port;
    return (ErlDrvData)d;
}

static void example_drv_stop(ErlDrvData handle) {
    driver_free((char*)handle);
}

static void example_drv_output(ErlDrvData handle, char *buff, int bufflen) {
    example_data* d = (example_data*)handle;
    char fn = buff[0], arg = buff[1], res;
    if (fn == 1) {
        res = twice(arg);
    } else if (fn == 2) {
        res = sum(buff[1], buff[2]);
    }
    driver_output(d->port, &res, 1);
}

ErlDrvEntry example_driver_entry = {
    NULL, 
    example_drv_start,
    example_drv_stop,
    example_drv_output,

    NULL,

    NULL,

    "example1_drv",
    NULL,
    NULL,
    NULL,
    NULL
};

DRIVER_INIT(example1_drv) {
    return &example_driver_entry;
}
