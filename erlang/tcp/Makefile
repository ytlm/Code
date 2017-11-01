.SUFFIXES: .erl .beam .yrl

all: compile

ERLC = erlc +debug_info

MODS = client server socket

.erl.beam:
	${ERLC} -W $<

compile: ${MODS:%=%.beam}

.PHONY:clean

clean:
	rm -rf *.beam erl_crash.dump


#
# erl -pa ../lib/lib_misc
#
