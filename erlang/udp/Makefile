.SUFFIXES: .erl .beam .yrl

all: compile

ERLC = erlc +debug_info

MODS = udp broadcast

.erl.beam:
	${ERLC} -W $<

compile: ${MODS:%=%.beam}

.PHONY:clean

clean:
	rm -rf *.beam erl_crash.dump
