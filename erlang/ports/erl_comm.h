#ifndef __ERL_COMM_H
#define __ERL_COMM_H

#include <unistd.h>

typedef unsigned char byte;

int read_cmd(byte *buf);
int write_cmd(byte *buf, int len);

#endif
