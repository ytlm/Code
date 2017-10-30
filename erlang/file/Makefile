.SUFFIXES: .erl .beam .yrl

all: compile

ERLC = erlc +debug_info

MODS = id3_v1

.erl.beam:
	${ERLC} -W $<

compile: ${MODS:%=%.beam}

.PHONY:clean

clean:
	rm -rf *.beam erl_crash.dump
