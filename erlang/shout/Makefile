.SUFFIXES: .erl .beam .yrl

all: compile

ERL = erl -boot start_clean -pa ../lib -pa ../lib/lib_misc

ERLC = erlc +debug_info

MODS = shout mp3_manager id3_tag_lengths mp3_sync

.erl.beam:
	${ERLC} -W $<

compile: ${MODS:%=%.beam}

mp3:
	${ERL} -s mp3_manager start

.PHONY:clean

clean:
	rm -rf *.beam erl_crash.dump

#
# make && make mp3
# shout:start().
# xmms http://127.0.0.1:3000
#
