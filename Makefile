SOURCES=$(wildcard *.erl)
TARGETS=$(patsubst %.erl,%.beam,$(SOURCES))

all: $(TARGETS)
	make -C ibrowse-1.0/src

run: all
	/opt/yaws/bin/yaws --conf yaws.conf -i

clean: cleanlog
	rm -f $(TARGETS)
	make -C ibrowse-1.0/src clean

cleanlog:
	rm -f auth.log report.log
	rm -f *.access

%.beam: %.erl
	erlc $<
