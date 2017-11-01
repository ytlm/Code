.SUFFIXES: .erl .beam .yrl

all: compile

ERL = erl -boot start_clean -pa ../lib -pa ../lib/lib_misc

ERLC = erlc +debug_info

MODS = shout mp3_manager

.erl.beam:
	${ERLC} -W $<

compile: ${MODS:%=%.beam}

mp3:
	${ERL} -s mp3_manager start

.PHONY:clean

clean:
	rm -rf *.beam erl_crash.dump
