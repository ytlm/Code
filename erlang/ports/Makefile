.SUFFIXES: .erl .beam .yrl

ERLC = erlc +debug_info

CC = gcc
OPS = -std=c99

.erl.beam:
	${ERLC} -W $<

MODS = example1 example1_lid

all: ${MODS:%=%.beam} example1 example1_drv.so

example1: example1.c erl_comm.c example1_driver.c
	$(CC) $(OPS) -o example1 example1.c erl_comm.c example1_driver.c

example1_drv.so: example1_lid.c example1.c
	$(CC) $(OPS) -o example1_drv.so -fpic -shared example1.c example1_lid.c

clean:
	rm -rf example1 example1_drv.so *.beam
