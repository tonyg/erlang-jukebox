SOURCE_DIR=src
EBIN_DIR=ebin
INCLUDE_DIR=include

SOURCES=$(wildcard $(SOURCE_DIR)/*.erl)
INCLUDES=$(wildcard $(INCLUDE_DIR)/*.hrl)
TARGETS=$(patsubst $(SOURCE_DIR)/%.erl, $(EBIN_DIR)/%.beam,$(SOURCES))

IBROWSE_DIR=priv/ibrowse-1.0
IBROWSE_SOURCE_DIR=$(IBROWSE_DIR)/src
IBROWSE_EBIN_DIR=$(IBROWSE_DIR)/ebin

RFC4627_DIR=../erlang-rfc4627
RFC4627_EBIN_DIR=$(RFC4627_DIR)/ebin

ERLC_OPTS=-I $(INCLUDE_DIR) -o $(EBIN_DIR) -Wall -v +debug_info
ERLC=erlc $(ERLC_OPTS)

ERL_CMD=erl \
	-boot start_sasl \
	-sasl errlog_type error \
	-kernel error_logger '{file,"./jukebox-kernel.log"}' \
	-sasl sasl_error_logger '{file,"./jukebox-sasl.log"}' \
	+W w \
	-pa $(EBIN_DIR) -pa $(IBROWSE_EBIN_DIR) -pa $(RFC4627_EBIN_DIR)

all: $(TARGETS) $(IBROWSE_EBIN_DIR)
	make -C priv/execdaemon
	make -C $(IBROWSE_SOURCE_DIR)

# You will need debian-multimedia / medibuntu for aacgain
install_dependencies_debian:
	sudo apt-get install vlc mp3gain vorbisgain aacgain flac imagemagick

$(IBROWSE_EBIN_DIR):
	mkdir $(IBROWSE_EBIN_DIR)

run_prereqs: all

run: run_prereqs
	$(ERL_CMD) -sname jukebox -s crypto -s jukebox

daemon: run_prereqs
	$(ERL_CMD) -detached -sname jukebox -s crypto -s jukebox >>jukebox-stdout.log 2>&1

stop:
	erl_call -a 'jukebox stop_and_halt []' -sname jukebox
	@echo
	@while (ps wwwax | grep 'beam.*sname jukebox' | grep -q -v grep); do \
		echo 'Waiting for jukebox to exit...'; \
		sleep 1; \
	done
	@echo 'Jukebox has exited.'

restart: stop daemon

restart_clean: stop cleanstate daemon

clean: cleanlog
	rm -f $(TARGETS)
	make -C priv/execdaemon clean
	make -C $(IBROWSE_SOURCE_DIR) clean

cleanstate:
	rm -f ejukebox.state

cleanlog:
	rm -f priv/server_root/logs/{access_log,error_log,security_log}
	rm -f jukebox-*.log

$(EBIN_DIR)/%.beam: $(SOURCE_DIR)/%.erl $(INCLUDES)
	$(ERLC) $<
