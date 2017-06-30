#include "example1.h"
#include "erl_comm.h"

int main(int argc, char *argv[])
{
    int fn, arg1, arg2, result;
    byte buff[100];

    while(read_cmd(buff) > 0) {
        fn = buff[0];
        if (fn == 1) {
            arg1 = buff[1];
            result = twice(arg1);
        } else if (fn == 2) {
            arg1 = buff[1];
            arg2 = buff[2];

            result = sum(arg1, arg2);
        }

        buff[0] = result;
        write_cmd(buff, 1);
    }
    return 0;
}
