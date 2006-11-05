SOURCES=$(wildcard *.erl)
TARGETS=$(patsubst %.erl,%.beam,$(SOURCES))

all: $(TARGETS)
	make -C execdaemon
	make -C ibrowse-1.0/src

run_prereqs: all

run: run_prereqs
	/opt/yaws/bin/yaws --conf yaws.conf -i

daemon: run_prereqs
	/opt/yaws/bin/yaws --conf yaws.conf -D

clean: cleanlog
	rm -f $(TARGETS)
	make -C execdaemon clean
	make -C ibrowse-1.0/src clean

cleanlog:
	rm -f auth.log report.log
	rm -f *.access

%.beam: %.erl
	erlc -I /opt/yaws/lib/yaws/include $<
