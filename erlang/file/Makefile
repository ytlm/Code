.SUFFIXES: .erl .beam .yrl

all: compile

ERL = erl -boot start_clean -pa ../lib -pa ../lib/lib_misc

ERLC = erlc +debug_info

MODS = id3_v1

.erl.beam:
	${ERLC} -W $<

compile: ${MODS:%=%.beam}

id3:
	${ERL} -s id3_v1 test

.PHONY:clean

clean:
	rm -rf *.beam erl_crash.dump
