.SUFFIXES: .erl .beam .yrl

all: compile

ERLC = erlc

MODS = my_fac_server name_server name_server1 new_name_server \
	   server1 server2 server3 server4 server5

.erl.beam:
	${ERLC} -W $<

compile: ${MODS:%=%.beam}

.PHONY:clean

clean:
	rm -rf *.beam erl_crash.dump
