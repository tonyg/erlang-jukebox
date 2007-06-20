#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <signal.h>
#include <fcntl.h>
#include <ctype.h>

#include <sys/types.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <unistd.h>

static pid_t kid = 0;

static int want_debug = 0;

static char *command = NULL;
static int command_buflen = 0;
static int command_length = 0;
static int current_request = 0;

static char *program_name = NULL;
static int arg_count = 0;
static char **arg_list = NULL;

#define COMMAND_TERMINATOR '\0'

static struct SignalTable {
  char *sig_name;
  int sig_num;
} signal_table[] = {
  { "HUP", SIGHUP },
  { "INT", SIGINT },
  { "QUIT", SIGQUIT },
  { "ILL", SIGILL },
  { "TRAP", SIGTRAP },
  { "ABRT", SIGABRT },
  { "IOT", SIGIOT },
  { "BUS", SIGBUS },
  { "FPE", SIGFPE },
  { "KILL", SIGKILL },
  { "USR1", SIGUSR1 },
  { "SEGV", SIGSEGV },
  { "USR2", SIGUSR2 },
  { "PIPE", SIGPIPE },
  { "ALRM", SIGALRM },
  { "TERM", SIGTERM },
  { "CHLD", SIGCHLD },
  { "CONT", SIGCONT },
  { "STOP", SIGSTOP },
  { "TSTP", SIGTSTP },
  { "TTIN", SIGTTIN },
  { "TTOU", SIGTTOU },
  { "URG", SIGURG },
  { "XCPU", SIGXCPU },
  { "XFSZ", SIGXFSZ },
  { "VTALRM", SIGVTALRM },
  { "PROF", SIGPROF },
  { "WINCH", SIGWINCH },
  { "IO", SIGIO },
  { NULL, 0 }
};

void append_command_char(int ch) {
  if (command_length >= command_buflen) {
    int delta = 1024;
    command = realloc(command, command_buflen + delta);
    command_buflen += delta;
  }

  command[command_length++] = ch;
}

int read_command(void) {
  int ch;
  command_length = 0;
  while (1) {
    ch = fgetc(stdin);
    if ((ch == EOF) || (ch == COMMAND_TERMINATOR)) {
      break;
    }
    append_command_char(ch);
  }
  append_command_char(0);
  command_length--; /* don't count the trailing nul */
  return command_length;
}

void write_response(char *code, char *arg) {
  if (want_debug) fprintf(stderr, "response: %d:%s,%s\n", current_request, code, arg);
  fprintf(stdout, "%d:%s,%s", current_request, code, arg);
  fputc(COMMAND_TERMINATOR, stdout);
  fflush(stdout);
}

char *num_buf(int n) {
  static char buf[32];
  sprintf(buf, "%d", n);
  return buf;
}

void errno_response() {
  int e = errno;
  write_response("errno", num_buf(e));
}

void handle_program(char *cmd, char *arg) {
  if (program_name != NULL) {
    free(program_name);
  }
  program_name = strdup(arg);
  write_response("ok", arg);
}

void handle_argc(char *cmd, char *arg) {
  int i;
  if (arg_count > 0) {
    for (i = 0; i < arg_count; i++) {
      if (arg_list[i] != NULL) {
	free(arg_list[i]);
      }
    }
    free(arg_list);
  }

  arg_count = (int) strtoul(arg, NULL, 10);

  /* Allocate arg_count+2 slots because execv wants a
     NULL-terminated array and we copy the program name into the 0th
     arg slot before execing */
  arg_list = calloc(arg_count + 2, sizeof(char *));
  write_response("ok", arg);
}

void handle_arg(char *cmd, char *arg) {
  char *val = strchr(arg, ',');
  int index;
  if (arg == NULL) {
    val = "";
  } else {
    *val = '\0';
    val++;
  }
  index = (int) strtoul(arg, NULL, 10);
  if (index < 0 || index >= arg_count) {
    write_response("bad", arg);
  } else {
    if (arg_list[index + 1] != NULL) {
      free(arg_list[index + 1]);
    }
    arg_list[index + 1] = strdup(val);
    write_response("ok", arg);
  }
}

void handle_execv(char *cmd, char *arg) {
  if (kid != 0) {
    write_response("bad", "already");
    return;
  }

  if (command_length == 0) {
    write_response("bad", "command");
    return;
  }

  if (arg_list == NULL) {
    write_response("bad", "args");
    return;
  }

  arg_list[0] = strdup(program_name);

  kid = fork();
  if (kid == 0) {
    int nullfd = open("/dev/null", O_RDWR, 0777);
    close(0); dup2(nullfd, 0);
    close(1); dup2(nullfd, 1);
    close(2); dup2(nullfd, 2);
    close(nullfd);
    if (execv(program_name, arg_list) == -1) {
      exit(1);
    }
  } else {
    write_response("pid", num_buf((int) kid));
  }
}

int lookup_signal(char *name) {
  struct SignalTable *entry;
  for (entry = &signal_table[0]; entry->sig_name != NULL; entry++) {
    if (!strcasecmp(name, entry->sig_name)) {
      return entry->sig_num;
    }
  }
  return 0;
}

void handle_sendsig(char *cmd, char *arg) {
  int signum;
  if (isdigit(arg[0])) {
    signum = (int) strtoul(arg, NULL, 10);
  } else {
    signum = lookup_signal(arg);
  }

  if (signum == 0) {
    write_response("bad", "invalid");
    return;
  }

  if (kid == 0) {
    write_response("bad", "not_running");
    return;
  }

  if (kill(kid, signum) == 0) {
    write_response("ok","");
  } else {
    errno_response();
  }
}

struct CommandTableEntry {
  char *name;
  void (*handler)(char *cmd, char *arg);
} cmd_table[] = {
  {"program", handle_program},
  {"argc", handle_argc},
  {"arg", handle_arg},
  {"execv", handle_execv},
  {"sendsig", handle_sendsig},
  {NULL, NULL}
};

void eval_command(void) {
  char *reqnumstr, *cmd, *arg;
  struct CommandTableEntry *cte;

  reqnumstr = command;
  cmd = strchr(command, ':');
  if (cmd == NULL) {
    exit(1);
  } else {
    *cmd = '\0';
    cmd++;
  }

  current_request = (int) strtoul(reqnumstr, NULL, 10);
  if (current_request == 0) {
    exit(1);
  }

  arg = strchr(cmd, ',');
  if (arg == NULL) {
    arg = "";
  } else {
    *arg = '\0';
    arg++;
  }

  if (want_debug) fprintf(stderr, "command: %d:%s,%s\n", current_request, cmd, arg);

  for (cte = &cmd_table[0]; cte->name != NULL; cte++) {
    if (!strcmp(cte->name, cmd)) {
      cte->handler(cmd, arg);
      current_request = 0;
      return;
    }
  }

  write_response("bad_command", cmd);
  current_request = 0;
}

int main(int argc, char *argv[]) {
  fd_set fds;
  int keep_going = 1;

  want_debug = (argc > 1) && (!strcmp(argv[1], "-debug"));

  if (want_debug) fprintf(stderr, "execdaemon starting.\n");

  while (keep_going) {
    struct timeval timeout;
    timeout.tv_sec = 0;
    timeout.tv_usec = 100000; /* 100 ms */

    FD_ZERO(&fds);
    FD_SET(0, &fds);

    switch (select(1, &fds, NULL, NULL, &timeout)) {
      case 1: {
	if (!read_command()) {
	  keep_going = 0;
	} else {
	  eval_command();
	}
	break;
      }

      case 0:
	break;

      case -1:
	perror("select");
	exit(1);
    }

    if (kid != 0) {
      int pidstatus;
      pid_t result = waitpid(kid, &pidstatus, WNOHANG);
      if (result != 0) {
	if (WIFEXITED(pidstatus)) {
	  write_response("exit", num_buf(WEXITSTATUS(pidstatus)));
	} else if (WIFSIGNALED(pidstatus)) {
	  write_response("signal", num_buf(WTERMSIG(pidstatus)));
	} else {
	  write_response("died", "");
	}
	kid = 0;
      }
    }
  }

  if (want_debug) fprintf(stderr, "execdaemon exiting.\n");
  return 0;
}
