.SUFFIXES: .erl .beam .yrl

all: compile

ERLC = erlc +debug_info

MODS = fac_server name_server server empty_server

.erl.beam:
	${ERLC} -W $<

compile: ${MODS:%=%.beam}

.PHONY:clean

clean:
	rm -rf *.beam erl_crash.dump


#
# server:start(name_server, name_server).
# name_server:add(joe, "at home").
# name_server:whereis(joe).
#
# Pid = empty_server:start().
# Pid ! {become, fun fac_server:loop/0}.
# empty_server:rpc(Pid, {fac, 30}).
#
