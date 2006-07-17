SOURCES=$(wildcard *.erl)
TARGETS=$(patsubst %.erl,%.beam,$(SOURCES))

all: $(TARGETS)
	make -C execdaemon
	make -C ibrowse-1.0/src

dojo: dojo.zip
	rm -rf dojo
	unzip dojo.zip
	touch dojo

run_prereqs: all dojo

run: run_prereqs
	/opt/yaws/bin/yaws --conf yaws.conf -i

daemon: run_prereqs
	/opt/yaws/bin/yaws --conf yaws.conf -D

clean: cleanlog
	rm -f $(TARGETS)
	rm -rf dojo
	make -C execdaemon clean
	make -C ibrowse-1.0/src clean

cleanlog:
	rm -f auth.log report.log
	rm -f *.access

%.beam: %.erl
	erlc -I /opt/yaws/lib/yaws/include $<
