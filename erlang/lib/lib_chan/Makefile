.SUFFIXES: .erl .beam .yrl

all: compile

ERLC = erlc +debug_info

MODS = kvs mode_name_server lib_chan lib_chan_cs lib_chan_mm lib_chan_auth

.erl.beam:
	${ERLC} -W $<

compile: ${MODS:%=%.beam}

.PHONY:clean

clean:
	rm -rf *.beam erl_crash.dump
