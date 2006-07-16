SOURCES=$(wildcard *.erl)
TARGETS=$(patsubst %.erl,%.beam,$(SOURCES))

all: $(TARGETS)
	make -C ibrowse-1.0/src

dojo: dojo.zip
	rm -rf dojo
	unzip dojo.zip
	touch dojo

run: all dojo
	chmod a+x wrapper.sh
	/opt/yaws/bin/yaws --conf yaws.conf -i

clean: cleanlog
	rm -f $(TARGETS)
	rm -rf dojo
	make -C ibrowse-1.0/src clean

cleanlog:
	rm -f auth.log report.log
	rm -f *.access

%.beam: %.erl
	erlc $<
