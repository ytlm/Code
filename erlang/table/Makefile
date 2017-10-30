.SUFFIXES: .erl .beam .yrl

all: compile

ERLC = erlc +debug_info

MODS = dets_test ets_test lib_trigrams

.erl.beam:
	${ERLC} -W $<

compile: ${MODS:%=%.beam}

.PHONY:clean

clean:
	rm -rf *.beam erl_crash.dump
