#ifndef PROCESS_H
#define PROCESS_H

#include <stdbool.h>
#include <signal.h>

typedef struct Buffer Buffer;
typedef struct Vt Vt;

typedef struct Process
{
	struct Process 		*next;
	struct Process 		*prev;
	char			*prog;
	const char		**env;
	char			*cwd;
	Vt 			*term;
	bool			is_status_set;
	int			status;
	int 			in;
	int 			out;
	int 			err;
	pid_t 			pid;
	Buffer 			*buf;
	volatile sig_atomic_t 	is_died;
	bool			async;
} Process;

Process *process_first(void);
Process *process_next(Process *proc);
Process *process_alloc(void);
void process_free(Process *proc);
void process_insert(Process *proc);
void process_remove(Process *proc);
bool process_is_async(Process *proc);
void process_died_set(Process *proc, bool is_died);
bool process_is_died(Process *proc);
int process_status_get(Process *proc);
void process_status_set(Process *proc, int status);
Buffer *process_buffer_get(Process *proc);
void process_buffer_set(Process *proc, Buffer *buf);
Vt *process_term_get(Process *proc);
pid_t process_pid_get(Process *proc);
Process *process_by_pid(pid_t pid);
int process_wait(Process *proc, int *status);
int process_kill(Process *proc);
void process_kill_async(Process *proc);
void process_destroy(Process *proc);
void process_destroy_dead(void);
void process_cleanup(void);
const char* process_shell(void);
void process_init(void);
Process *process_create(const char *prog, const char *cwd, int *in, int *out, int *err, const char **env, bool pty, bool async);

#endif /* PROCESS_H */
