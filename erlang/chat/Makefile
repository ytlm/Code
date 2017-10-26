.SUFFIXES: .erl .beam .yrl

all: compile

ERLC = erlc

ERL = erl -boot start_clean

MODS = chat_client chat_group chat_server io_widget \
	   mod_chat_controller

.erl.beam:
	${ERLC} -W $<

compile: ${MODS:%=%.beam}

chat_server:
	${ERL} -s chat_server start

chat_client:
	${ERL} -s chat_client test

.PHONY:clean

clean:
	rm -rf *.beam erl_crash.dump
