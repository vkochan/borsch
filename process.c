#define _GNU_SOURCE
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <stddef.h>
#include <sys/wait.h>
#include <errno.h>
#include <string.h>
#if defined(__linux__) || defined(__CYGWIN__)
# include <pty.h>
#elif defined(__FreeBSD__) || defined(__DragonFly__)
# include <libutil.h>
#elif defined(__OpenBSD__) || defined(__NetBSD__) || defined(__APPLE__)
# include <util.h>
#endif
#include <pwd.h>

#include "common.h"
#include "buffer.h"
#include "event.h"
#include "process.h"
#include "vt.h"
#include "api.h"

#ifdef _AIX
# include "forkpty-aix.c"
#elif defined __sun
# include "forkpty-sunos.c"
#endif

typedef struct
{
	pid_t 			pid;
} ProcessInfo;

static int proc_fd[2];

static const char *prog_name = PROGNAME;
static Process proc_list;
static char term_name[32];
static const char *shell;

Process *process_first(void)
{
	return proc_list.next;
}

Process *process_next(Process *proc)
{
	return proc->next;
}

Process *process_alloc(void)
{
	Process *proc = calloc(1, sizeof(Process));

	if (!proc)
		return NULL;

	proc->is_status_set = false;
	proc->status = -1;
	proc->pid = -1;
	proc->in = -1;
	proc->out = -1;
	proc->err = -1;

	return proc;
}

void process_free(Process *proc)
{
	if (proc->env) {
		for (const char **env = proc->env; *env; env++)
			free((char *)*env);
		free(proc->env);
	}
	free(proc->prog);
	free(proc->cwd);
	free(proc);
}

void process_insert(Process *proc)
{
	proc->next = proc_list.next;
	proc->prev = &proc_list;

	if (proc_list.next)
		proc_list.next->prev = proc;
	proc_list.next = proc;
}

void process_remove(Process *proc)
{
	if (proc->prev)
		proc->prev->next = proc->next;
	if (proc->next)
		proc->next->prev = proc->prev;
}

bool process_is_async(Process *proc)
{
	return proc->async;
}

void process_died_set(Process *proc, bool is_died)
{
	proc->is_died = is_died;
}

bool process_is_died(Process *proc)
{
	return proc->is_died;
}

int process_status_get(Process *proc)
{
	return proc->status;
}

void process_status_set(Process *proc, int status)
{
	proc->is_status_set = true;
	proc->status = status;
}

Buffer *process_buffer_get(Process *proc)
{
	return proc->buf;
}

void process_buffer_set(Process *proc, Buffer *buf)
{
	proc->buf = buf;
}

Vt *process_term_get(Process *proc)
{
	return proc->term;
}

pid_t process_pid_get(Process *proc)
{
	return proc->pid;
}

Process *process_by_pid(pid_t pid)
{
	for (Process *proc = proc_list.next; proc; proc = proc->next) {
		if (proc->pid == pid)
			return proc;
	}

	return NULL;
}

int process_wait(Process *proc, int *status)
{
	pid_t pid;

	if (proc->is_died || proc->is_status_set) {
		*status = proc->status;
		return 0;
	}

	pid = waitpid(proc->pid, status, 0);
	if (pid == proc->pid && WIFEXITED(*status)) {
		*status = WEXITSTATUS(*status);
		return 0;
	}

	if (proc->is_status_set) {
		*status = proc->status;
		return 0;
	}
	
	return -1;
}

int process_kill(Process *proc)
{
	pid_t pid = proc->pid;
	int status = -1;
	event_t evt = {};

	if (pid != -1 && !proc->is_died) {
		kill(-pid, SIGKILL);
		waitpid(pid, &status, 0);
		process_died_set(proc, true);

		if (proc->term) {
			event_fd_handler_unregister(vt_pty_get(proc->term));
			vt_destroy(proc->term);
		}
		if (WIFEXITED(status)) {
			process_status_set(proc, WEXITSTATUS(status));
		}
		return 0;
	}

	return status;
}

void process_kill_async(Process *proc)
{
	kill(-proc->pid, SIGKILL);
}

void process_destroy(Process *proc)
{
	if (process_buffer_get(proc))
		buffer_proc_set(process_buffer_get(proc), NULL);
	process_kill(proc);
	process_remove(proc);
	process_free(proc);
}

void process_destroy_dead(void)
{
	Process *proc = proc_list.next;

	while (proc) {
		Process *next = proc->next;

		if (process_is_died(proc))
			process_destroy(proc);
	
		proc = next;
	}
}

void process_cleanup(void)
{
	Process *proc = proc_list.next;

	while (proc) {
		Process *next = proc->next;

		process_destroy(proc);
	
		proc = next;
	}
}

static bool checkshell(const char *shell)
{
	if (shell == NULL || *shell == '\0' || *shell != '/')
		return false;
	if (!strcmp(strrchr(shell, '/')+1, prog_name))
		return false;
	if (access(shell, X_OK))
		return false;
	return true;
}

static const char *getshell(void)
{
	const char *shell = getenv("SHELL");
	struct passwd *pw;

	if (checkshell(shell))
		return shell;
	if ((pw = getpwuid(getuid())) && checkshell(pw->pw_shell))
		return pw->pw_shell;
	return "/bin/sh";
}

const char* process_shell(void)
{
	return shell;
}

static void sigchld_handler(int sig) {
	ProcessInfo pinfo;
	Process *proc;
	int errsv = errno;
	int status;
	pid_t pid;

	while ((pid = waitpid(-1, &status, WNOHANG)) != 0) {
		if (pid == -1) {
			if (errno == ECHILD) {
				/* no more child processes */
				break;
			}
			eprint("waitpid: %s\n", strerror(errno));
			break;
		}
		proc = process_by_pid(pid);
		if (proc) {
			if (WIFEXITED(status)) {
				process_status_set(proc, WEXITSTATUS(status));
			}

			pinfo.pid = pid;
			write(proc_fd[1], &pinfo, sizeof(pinfo));
		}
	}

	errno = errsv;
}

static void handle_sigchld_io(int fd, void *arg)
{
	ProcessInfo pinfo;
	event_t evt = {};
	ssize_t len;

	len = read(fd, &pinfo, sizeof(pinfo));
	if (len == sizeof(pinfo)) {
		Process *proc = process_by_pid(pinfo.pid);
		if (proc) {
			process_died_set(proc, true);
			evt.eid = EVT_PROC_EXIT;
			evt.oid = pinfo.pid;
			scheme_event_handle(evt);
		}
	}
}

void process_init(void)
{
	char *term = getenv("BORSCH_TERM");
	struct sigaction sa;

	pipe2(proc_fd, O_NONBLOCK);

	memset(&sa, 0, sizeof sa);
	sa.sa_flags = 0;
	sigemptyset(&sa.sa_mask);
	sa.sa_handler = sigchld_handler;
	sigaction(SIGCHLD, &sa, NULL);

	if (!term)
		term = "borsch";
	snprintf(term_name, sizeof term_name, "%s%s", term, COLORS >= 256 ? "-256color" : "");

	event_fd_handler_register(proc_fd[0], handle_sigchld_io, NULL);

	shell = getshell();
}

static void process_handle_vt(int fd, void *arg)
{
	Process *proc = arg;
	Buffer *buf = process_buffer_get(proc);

	if (vt_process(proc->term) < 0 && errno == EIO) {
		event_t evt = {};
		evt.eid = EVT_PROC_EXIT;
		evt.oid = process_pid_get(proc);
		scheme_event_handle(evt);
	}
}

static pid_t __process_fork(const char *p, const char *argv[], const char *cwd, const char *env[], int *to, int *from, int *err, Vt *vt)
{
	int vt2in[2], err2vt[2], out2vt[2];
	struct winsize ws;
	pid_t pid;

	if (to && pipe(vt2in)) {
		*to = -1;
		to = NULL;
	}
	if (err && pipe(err2vt)) {
		*err = -1;
		err = NULL;
	}
	if (from && pipe(out2vt)) {
		*from = -1;
		from = NULL;
	}

	if (vt) {
		int rows, cols;
		int pty;

		vt_size_get(vt, &rows, &cols);
		ws.ws_xpixel = ws.ws_ypixel = 0;
		ws.ws_row = rows;
		ws.ws_col = cols;

		pid = forkpty(&pty, NULL, NULL, &ws);
		vt_pty_set(vt, pty);
		vt_pid_set(vt, pid);
	} else {
		pid = fork();
	}

	if (pid < 0)
		return -1;

	if (pid == 0) {
		setsid();

		sigset_t emptyset;
		sigemptyset(&emptyset);
		sigprocmask(SIG_SETMASK, &emptyset, NULL);

		if (to) {
			close(vt2in[1]);
			dup2(vt2in[0], STDIN_FILENO);
			close(vt2in[0]);
		}
		if (err) {
			close(err2vt[0]);
			dup2(err2vt[1], STDERR_FILENO);
			close(err2vt[1]);
		} else {
			dup2(out2vt[1], STDERR_FILENO);
		}
		if (from) {
			close(out2vt[0]);
			dup2(out2vt[1], STDOUT_FILENO);
			close(out2vt[1]);
		}

		int maxfd = sysconf(_SC_OPEN_MAX);
		for (int fd = 3; fd < maxfd; fd++)
			if (close(fd) == -1 && errno == EBADF)
				break;

		setenv("TERM", term_name, 1);
		for (const char **envp = env; envp && envp[0]; envp += 2)
			setenv(envp[0], envp[1], 1);

		if (cwd)
			chdir(cwd);

		execvp(p, (char *const *)argv);
		fprintf(stderr, "\nexecv() failed.\nCommand: '%s'\n", argv[0]);
		exit(1);
	}

	if (to) {
		close(vt2in[0]);
		*to = vt2in[1];
	}
	if (err) {
		close(err2vt[1]);
		*err = err2vt[0];
	}
	if (from) {
		close(out2vt[1]);
		*from = out2vt[0];
	}

	return pid;
}

Process *process_create(const char *prog, const char *cwd, int *in, int *out, int *err, const char **env, bool pty, bool async)
{
	const char *pargs[4] = { shell, NULL };
	Vt *term = NULL;
	Process *proc;

	if (prog) {
		pargs[1] = "-c";
		pargs[2] = prog;
		pargs[3] = NULL;
	}

	proc = process_alloc();
	if (!proc)
		return NULL;

	if (pty) {
		term = vt_create();
		if (!term) {
			process_free(proc);
			return NULL;
		}
	}

	if (prog)
		proc->prog = strdup(prog);
	if (cwd)
		proc->cwd = strdup(cwd);
	proc->async = async;
	proc->term = term;
	proc->env = env;

	proc->pid = __process_fork(shell, pargs, cwd, env, in, out, err, term);
	if (proc->pid == -1) {
		process_destroy(proc);
		return NULL;
	}
	if (in)
		proc->in = *in;
	if (out)
		proc->out = *out;
	if (err)
		proc->err = *err;

	if (term)
		event_fd_handler_register(vt_pty_get(term), process_handle_vt, proc);

	process_insert(proc);

	return proc;
}
