SOURCE_DIR=src
EBIN_DIR=ebin
INCLUDE_DIR=include

SOURCES=$(wildcard $(SOURCE_DIR)/*.erl)
INCLUDES=$(wildcard $(INCLUDE_DIR)/*.hrl)
TARGETS=$(patsubst $(SOURCE_DIR)/%.erl, $(EBIN_DIR)/%.beam,$(SOURCES))

IBROWSE_DIR=priv/ibrowse-1.0
IBROWSE_SOURCE_DIR=$(IBROWSE_DIR)/src
IBROWSE_EBIN_DIR=$(IBROWSE_DIR)/ebin

ERLC_OPTS=-I $(INCLUDE_DIR) -o $(EBIN_DIR) -Wall -v +debug_info
ERLC=erlc $(ERLC_OPTS)

ERL_CMD=erl -pa $(EBIN_DIR) -pa $(IBROWSE_EBIN_DIR)

all: $(TARGETS)
	make -C priv/execdaemon
	make -C $(IBROWSE_SOURCE_DIR)

run_prereqs: all

run: run_prereqs
	$(ERL_CMD) -sname jukebox -s jukebox

daemon: run_prereqs
	$(ERL_CMD) -detached -sname jukebox -s jukebox

stop:
	erl_call -a 'jukebox stop_and_halt []' -sname jukebox

clean: cleanlog
	rm -f $(TARGETS)
	make -C priv/execdaemon clean
	make -C $(IBROWSE_SOURCE_DIR) clean

cleanlog:
	rm -f auth.log report.log
	rm -f *.access

$(EBIN_DIR)/%.beam: $(SOURCE_DIR)/%.erl $(INCLUDES)
	$(ERLC) $<
