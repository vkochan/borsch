/*
 * Borsch is based on dvtm implementation with ChezScheme glue
 *
 * © 2021      Vadym Kochan <vadim4j@gmail.com>
 *
 * dvtm code is implemented by
 *
 * © 2007-2016 Marc André Tanner <mat at brain-dump dot org>
 *
 * It is highly inspired by the original X11 dwm and
 * reuses some code of it which is mostly
 *
 * © 2006-2007 Anselm R. Garbe <garbeam at gmail dot com>
 *
 * See LICENSE for details.
 */
#define _GNU_SOURCE
#include <stdlib.h>
#include <unistd.h>
#include <stdint.h>
#include <wchar.h>
#include <limits.h>
#include <libgen.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/types.h>
#include <fcntl.h>
#include <curses.h>
#include <stdio.h>
#include <stdarg.h>
#include <signal.h>
#include <locale.h>
#include <string.h>
#include <unistd.h>
#include <stdbool.h>
#include <errno.h>
#include <pwd.h>
#if defined(__linux__) || defined(__CYGWIN__)
# include <pty.h>
#elif defined(__FreeBSD__) || defined(__DragonFly__)
# include <libutil.h>
#elif defined(__OpenBSD__) || defined(__NetBSD__) || defined(__APPLE__)
# include <util.h>
#endif

#include "array.h"
#include "event.h"
#include "buffer.h"
#include "keymap.h"
#include "view.h"
#include "syntax.h"
#include "text/text-motions.h"
#include "text/text-objects.h"
#if defined __CYGWIN__ || defined __sun
# include <termios.h>
#endif
#include "api.h"
#include "vt.h"

#ifdef _AIX
# include "forkpty-aix.c"
#elif defined __sun
# include "forkpty-sunos.c"
#endif

#ifdef PDCURSES
int ESCDELAY;
#endif

#ifndef NCURSES_REENTRANT
# define set_escdelay(d) (ESCDELAY = (d))
#endif

/* scroll back buffer size in lines */
#define SCROLL_HISTORY 4000

static int scr_history = SCROLL_HISTORY;

typedef struct {
	const char *symbol;
	void (*arrange)(unsigned int, unsigned int, unsigned int, unsigned int);
} Layout;

typedef struct Window Window;
struct Window {
	Buffer *prev_buf;
	Buffer *buf;
	View *view;
	UiWin *win;
	const char *cmd;
	int order;
	unsigned short int id;
	bool minimized;
	bool urgent;
	Window *next;
	Window *prev;
	Window *snext;
	unsigned int tags;
	bool highlight_mark;
	bool pending_draw_evt;
};

typedef struct {
	short fg;
	short bg;
	short fg256;
	short bg256;
	short pair;
} Color;

/* #define ALT(k)      ((k) + (161 - 'a')) */
#define ALT	27
#if defined CTRL && defined _AIX
  #undef CTRL
#endif
#ifndef CTRL
  #define CTRL(k)   ((k) & 0x1F)
#endif
#define CTRL_ALT(k) ((k) + (129 - 'a'))

#define MAX_ARGS 8

typedef struct {
	void (*cmd)(const char *args[]);
	const char *args[3];
} Action;

#define MAX_KEYS 4

typedef struct {
	mmask_t mask;
	Action action;
} Button;

typedef struct {
	const char *name;
	Action action;
} Cmd;

typedef struct {
	unsigned int key_index;
	KeyCode keys[MAX_KEYS];
} KeyBuf;

static KeyBuf kbuf;

enum { BAR_TOP, BAR_BOTTOM, BAR_OFF };
enum { BAR_LEFT, BAR_RIGHT };

enum { MIN_ALIGN_HORIZ, MIN_ALIGN_VERT };

typedef struct {
	int fd;
	const char *file;
	unsigned short int id;
} Fifo;

typedef struct {
	char *data;
	size_t len;
	size_t size;
} Register;

static Ui *ui;

#define LENGTH(arr) (sizeof(arr) / sizeof((arr)[0]))
#define MAX(x, y)   ((x) > (y) ? (x) : (y))
#define MIN(x, y)   ((x) < (y) ? (x) : (y))
#define TAGMASK     ((1 << LENGTH(tags)) - 1)

extern int scheme_init(const char *);
extern void scheme_uninit(void);
extern int scheme_event_handle(event_t evt);
extern int scheme_eval_file(const char *scm_in, const char *out);
extern void *scheme_env_alloc(void);
extern void scheme_env_free(void *env);

static char *scheme_init_script = "";

/* commands for use by keybindings */
static void focusn(const char *args[]);
static void focusnextnm(const char *args[]);
static void focusprevnm(const char *args[]);
static void focuslast(const char *args[]);
static void killwindow(void);
static void killother(const char *args[]);
static void quit(const char *args[]);
static void redraw(const char *args[]);
static void scrollback(const char *args[]);
static void setlayout(const char *args[]);
static int getnmaster(void);
static float getmfact(void);
static void toggleminimize(void);
static void minimizeother(const char *args[]);
static void togglemouse(const char *args[]);
static void togglerunall(const char *args[]);
static void toggletag(const char *args[]);
static void toggleview(const char *args[]);
static void viewprevtag(const char *args[]);
static void zoom(const char *args[]);
static void doeval(const char *args[]);

/* commands for use by mouse bindings */
static void mouse_focus(const char *args[]);
static void mouse_fullscreen(const char *args[]);
static void mouse_minimize(const char *args[]);
static void mouse_zoom(const char *args[]);

static void attachafter(Window *c, Window *a);
static Window* nextvisible(Window *c);

static void focus(Window *c);
static void resize(Window *c, int x, int y, int w, int h);
static unsigned int waw, wah, wax, way;
static Window *windows = NULL;
static char *title;
static bool show_tagnamebycwd = false;

static void buf_list_update(void);

static bool is_in_kbd_action;
static KeyMap *win_min_kmap;
static KeyMap *global_kmap;
static KeyMap *curr_kmap;

static Window *minibuf;
static Window *topbar;

/* valid curses attributes are listed below they can be ORed
 *
 * A_NORMAL        Normal display (no highlight)
 * A_STANDOUT      Best highlighting mode of the terminal.
 * A_UNDERLINE     Underlining
 * A_REVERSE       Reverse video
 * A_BLINK         Blinking
 * A_DIM           Half bright
 * A_BOLD          Extra bright or bold
 * A_PROTECT       Protected mode
 * A_INVIS         Invisible or blank mode
 */

enum {
	DEFAULT,
	BLUE,
	RED,
	MAGENTA,
	BLUE_BG,
};

static Color colors[] = {
	[DEFAULT] = { .fg = -1,         .bg = -1, .fg256 = -1, .bg256 = -1, },
	[BLUE]    = { .fg = COLOR_BLUE, .bg = -1, .fg256 = 68, .bg256 = -1, },
	[RED]   = { .fg = COLOR_RED, .bg = -1, .fg256 = 0, .bg256 = -1, },
	[MAGENTA]   = { .fg = COLOR_MAGENTA, .bg = -1, .fg256 = 0, .bg256 = -1, },
	[BLUE_BG]   = { .fg = -1, .bg = COLOR_BLUE, .fg256 = -1, .bg256 = 0, },
};

/* status bar (command line option -s) position */
#define BAR_POS         BAR_TOP /* BAR_BOTTOM, BAR_OFF */
/* master width factor [0.1 .. 0.9] */
#define MFACT 0.5
/* number of windows in master area */
#define NMASTER 1

const char tags[][8] = { "1", "2", "3", "4", "5", "6", "7", "8", "9" };

#include "tile.c"
#include "grid.c"
#include "bstack.c"
#include "fullscreen.c"

/* by default the first layout entry is used */
static Layout layouts[] = {
	{ "[]=", tile },
	{ "+++", grid },
	{ "TTT", bstack },
	{ "[ ]", fullscreen },
};

/* possible values for the mouse buttons are listed below:
 *
 * BUTTON1_PRESSED          mouse button 1 down
 * BUTTON1_RELEASED         mouse button 1 up
 * BUTTON1_CLICKED          mouse button 1 clicked
 * BUTTON1_DOUBLE_CLICKED   mouse button 1 double clicked
 * BUTTON1_TRIPLE_CLICKED   mouse button 1 triple clicked
 * BUTTON2_PRESSED          mouse button 2 down
 * BUTTON2_RELEASED         mouse button 2 up
 * BUTTON2_CLICKED          mouse button 2 clicked
 * BUTTON2_DOUBLE_CLICKED   mouse button 2 double clicked
 * BUTTON2_TRIPLE_CLICKED   mouse button 2 triple clicked
 * BUTTON3_PRESSED          mouse button 3 down
 * BUTTON3_RELEASED         mouse button 3 up
 * BUTTON3_CLICKED          mouse button 3 clicked
 * BUTTON3_DOUBLE_CLICKED   mouse button 3 double clicked
 * BUTTON3_TRIPLE_CLICKED   mouse button 3 triple clicked
 * BUTTON4_PRESSED          mouse button 4 down
 * BUTTON4_RELEASED         mouse button 4 up
 * BUTTON4_CLICKED          mouse button 4 clicked
 * BUTTON4_DOUBLE_CLICKED   mouse button 4 double clicked
 * BUTTON4_TRIPLE_CLICKED   mouse button 4 triple clicked
 * BUTTON_SHIFT             shift was down during button state change
 * BUTTON_CTRL              control was down during button state change
 * BUTTON_ALT               alt was down during button state change
 * ALL_MOUSE_EVENTS         report all button state changes
 * REPORT_MOUSE_POSITION    report mouse movement
 */

#ifdef NCURSES_MOUSE_VERSION
# define CONFIG_MOUSE /* compile in mouse support if we build against ncurses */
#endif

#define ENABLE_MOUSE true /* whether to enable mouse events by default */

#ifdef CONFIG_MOUSE
static Button buttons[] = {
	{ BUTTON1_CLICKED,        { mouse_focus,      { NULL  } } },
	{ BUTTON1_DOUBLE_CLICKED, { mouse_fullscreen, { "[ ]" } } },
	{ BUTTON2_CLICKED,        { mouse_zoom,       { NULL  } } },
	{ BUTTON3_CLICKED,        { mouse_minimize,   { NULL  } } },
};
#endif /* CONFIG_MOUSE */

#define CWD_MAX		256

typedef struct {
	unsigned int curtag, prevtag;
	int nmaster[LENGTH(tags) + 1];
	float mfact[LENGTH(tags) + 1];
	Layout *layout[LENGTH(tags) + 1];
	Layout *layout_prev[LENGTH(tags) + 1];
	bool runinall[LENGTH(tags) + 1];
	char *cwd[LENGTH(tags) + 1];
	char *name[LENGTH(tags) + 1];
	bool msticky[LENGTH(tags) + 1];
	Window *popup[LENGTH(tags) + 1];
} Pertag;

typedef struct
{
	pid_t 			pid;
	int			status;
} ProcessInfo;

typedef struct Process
{
	struct Process 		*next;
	struct Process 		*prev;
	char			*prog;
	const char		**env;
	char			*cwd;
	Vt 			*term;
	int			status;
	int 			in;
	int 			out;
	int 			err;
	pid_t 			pid;
	Window 			*win;
	Buffer 			*buf;
	volatile sig_atomic_t 	is_died;
} Process;

static Process proc_list;
static int proc_fd[2];

/* global variables */
static const char *prog_name = PROGNAME;
static Pertag pertag;
static Window *stack = NULL;
static Window *sel = NULL;
static Window *lastsel = NULL;
static Window *msel = NULL;
static unsigned int seltags;
static unsigned int tagset[2] = { 1, 1 };
static bool mouse_events_enabled = ENABLE_MOUSE;
static Layout *layout = layouts;

static Fifo cmdfifo = { .fd = -1 };
static Fifo retfifo = { .fd = -1 };

static const char *shell;
static volatile sig_atomic_t running = true;
static bool runinall = false;
/* make sense only in layouts which has master window (tile, bstack) */
static int min_align = MIN_ALIGN_HORIZ;

static Cmd commands[] = {
	{ "eval", { doeval, { NULL } } },
};

static Array style_array;

static void draw(Window *c, bool force);
static void draw_title(Window *c);
static void drawbar(void);
static bool isarrange(void (*func)());
static Window *current_window(void);
static bool isvisible(Window *c);

static char term_name[32];

static void
term_title_handler(Vt *term, const char *title) {
	/* Window *c = (Window *)vt_data_get(term); */
	/* if (title) */
	/* 	strncpy(c->title, title, sizeof(c->title) - 1); */
	/* c->title[title ? sizeof(c->title) - 1 : 0] = '\0'; */
	/* settitle(c); */
	/* if (!isarrange(fullscreen)) */
	/* 	draw_title(c); */
}

static void
term_urgent_handler(Vt *term) {
	Window *c = (Window *)vt_data_get(term);
	c->urgent = true;
	printf("\a");
	fflush(stdout);
	drawbar();
	if (!isarrange(fullscreen) && current_window() != c && isvisible(c))
		draw_title(c);
}

static void destroy(Window *w);
static void process_died_set(Process *proc, bool is_died);
Buffer *process_buffer_get(Process *proc);
void process_destroy(Process *proc);
int process_kill(Process *proc);

static void process_handle_vt(int fd, void *arg) {
	Process *proc = arg;
	Buffer *buf = process_buffer_get(proc);

	if (!vt_is_processed(proc->term)) {
		if (vt_process(proc->term) < 0 && errno == EIO) {
			Window *win = proc->win;
			process_destroy(proc);
			destroy(win);
		} else {
			if (buf) {
				vt_processed_set(proc->term, true);
				buffer_dirty_set(buf, true);
			}
		}
	}
}

static Process *process_alloc(void)
{
	Process *proc = calloc(1, sizeof(Process));

	if (!proc)
		return NULL;

	proc->status = -1;
	proc->pid = -1;
	proc->in = -1;
	proc->out = -1;
	proc->err = -1;

	return proc;
}

static void process_free(Process *proc)
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

static void process_insert(Process *proc)
{
	proc->next = proc_list.next;
	proc->prev = &proc_list;

	if (proc_list.next)
		proc_list.next->prev = proc;
	proc_list.next = proc;
}

static void process_remove(Process *proc)
{
	if (proc->prev)
		proc->prev->next = proc->next;
	if (proc->next)
		proc->next->prev = proc->prev;
}

static void process_died_set(Process *proc, bool is_died)
{
	proc->is_died = is_died;
}

static bool process_is_died(Process *proc)
{
	return proc->is_died;
}

static int process_status_get(Process *proc)
{
	return proc->status;
}

static void process_status_set(Process *proc, int status)
{
	proc->status = status;
}

static void process_attach_win(Process *proc, Window *w)
{
	proc->win = w;

	ui_window_on_view_update_set(w->win, NULL);
	ui_window_sidebar_width_set(w->win, 0);
	ui_window_ops_draw_set(w->win, vt_draw);
	ui_window_priv_set(w->win, proc->term);

	vt_attach(proc->term, w->win);
	vt_data_set(proc->term, w);
	vt_dirty(proc->term);
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
	if (proc->buf)
		buffer_proc_set(proc->buf, NULL);
	process_kill(proc);
	process_remove(proc);
	process_free(proc);
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

static Process *process_create(const char *prog, const char *cwd, int *in, int *out, int *err, const char **env, bool pty)
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
		term = vt_create(ui_height_get(ui), ui_width_get(ui), scr_history);
		if (!term) {
			process_free(proc);
			return NULL;
		}
		vt_urgent_handler_set(term, term_urgent_handler);
		vt_title_handler_set(term, term_title_handler);
	}

	if (prog)
		proc->prog = strdup(prog);
	if (cwd)
		proc->cwd = strdup(cwd);
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

static int style_init(void)
{
	Style default_style = {
		.fg = UI_TEXT_COLOR_WHITE,
		.bg = UI_TEXT_COLOR_BLACK,
		.name = "default",
	};
	Style highlight_style = {
		.fg = UI_TEXT_COLOR_WHITE,
		.bg = UI_TEXT_COLOR_BLUE,
		.name = "highlight",
	};

	array_init_sized(&style_array, sizeof(Style));
	style_add(&default_style);
	style_add(&highlight_style);
}

static void style_cleanup(void)
{
	array_release(&style_array);
}

void eprint(const char *errstr, ...) {
	va_list ap;
	va_start(ap, errstr);
	vfprintf(stderr, errstr, ap);
	va_end(ap);
}

static void
error(const char *errstr, ...) {
	va_list ap;
	va_start(ap, errstr);
	vfprintf(stderr, errstr, ap);
	va_end(ap);
	exit(EXIT_FAILURE);
}

static bool
isarrange(void (*func)()) {
	return func == layout->arrange;
}

static Window *current_window(void)
{
	return sel;
}

static void set_current_window(Window *w)
{
	sel = w;
}

static bool
isvisible(Window *c) {
	return c->tags & tagset[seltags];
}

static bool
is_content_visible(Window *c) {
	if (!c)
		return false;
	if (isarrange(fullscreen))
		return current_window() == c;
	else if (c == minibuf)
		return true;
	else if (c == topbar)
		return true;
	return isvisible(c) && !c->minimized;
}

static Window*
nextvisible(Window *c) {
	for (; c && !isvisible(c); c = c->next);
	return c;
}

static bool ismaster(Window *c) {
	int n = 0;
	Window *m;

	for (m = nextvisible(windows); m && n < getnmaster(); m = nextvisible(m->next), n++) {
		if (c == m)
			return true;
	}

	return false;
}

static bool ismastersticky(Window *c) {
	int n = 0;
	Window *m;

	if (!pertag.msticky[pertag.curtag])
		return false;
	if (!c)
		return true;

	for (m = nextvisible(windows); m && n < getnmaster(); m = nextvisible(m->next), n++) {
		if (c == m)
			return true;
	}

	return false;
}

char *window_get_title(Window *c)
{
	if (strlen(ui_window_title_get(c->win)))
		return ui_window_title_get(c->win);

	return buffer_name_get(c->buf);
}

Window *get_popup(void)
{
	return pertag.popup[pertag.curtag];
}

void *set_popup(Window *p)
{
	pertag.popup[pertag.curtag] = p;
}

static void
update_screen_size(void) {
	int dec_h = 0;
	int top_h = 0;
	wax = 0;
	way = 0;

	if (topbar)
		top_h = ui_window_height_get(topbar->win);
	if (minibuf)
		dec_h = ui_window_height_get(minibuf->win);

	wah = ui_height_get(ui)-dec_h;
	waw = ui_width_get(ui);
	if (BAR_POS == BAR_TOP) {
		wah -= top_h;
		way += top_h;
	} else if (BAR_POS == BAR_BOTTOM) {
		wah -= top_h;
	}

	if (minibuf)
		ui_window_move(minibuf->win, 0, ui_height_get(ui)-dec_h);
}

static void
drawbar(void) {
	if (topbar)
		draw(topbar, true);
}

static void draw_title(Window *c) {
	ui_text_style_t title_style = UI_TEXT_STYLE_NORMAL;
	int title_fg = UI_TEXT_COLOR_WHITE;
	int title_bg = UI_TEXT_COLOR_BRIGHT_BLACK;
	int x, y, maxlen, title_y, title_x;
	int w_w = ui_window_width_get(c->win);
	int has_border = ui_window_border_is_enabled(c->win);
	size_t status_len, name_len;
	Selection *view_cursor = view_selections_primary_get(c->view);
	size_t line, col;
	char status[100];
	char title[256];
	char name[100];
	char *name_ptr;

	if (!ui_window_has_title(c->win))
		return;

	if (current_window() == c || (pertag.runinall[pertag.curtag] && !c->minimized)) {
		title_fg = UI_TEXT_COLOR_BLACK;
		title_bg = UI_TEXT_COLOR_WHITE;
	}

	title_y = ui_window_height_get(c->win)-1;
	title_y -= has_border;
	title_x = has_border;
	w_w -= has_border;

	ui_window_cursor_get(c->win, &x, &y);

	ui_window_draw_char_attr(c->win, title_x, title_y, ACS_HLINE, w_w-(has_border*2),
				 title_bg, title_bg,
				 UI_TEXT_STYLE_NORMAL);

	maxlen = ui_window_width_get(c->win) - 10;
	if (maxlen < 0)
		maxlen = 0;
	strncpy(name, buffer_name_get(c->buf), sizeof(name));

	line = view_cursors_line(view_cursor);
	col = view_cursors_col(view_cursor);

	status_len = snprintf(status, sizeof(status), "[%d:%d] %s%s %s  [%d|%s]%s",
			line,
			col,
			buffer_is_modified(c->buf) ? "[+] " : "",
			buffer_mode_name_get(c->buf),
			buffer_state_name_get(c->buf),
			c->order,
			ismastersticky(c) ? "*" : "",
			buffer_is_readonly(c->buf) ? "[RO]" : "");

	if (maxlen)
		name_len = maxlen - status_len - 1;
	else
		name_len = strlen(name);

	name_ptr = name;

	if (name_len < strlen(name))
		name_ptr += strlen(name) - name_len;

	if (name_len >= 3 && name_len < strlen(buffer_name_get(c->buf))) {
		name_ptr[0] = '.';
		name_ptr[1] = '.';
		name_ptr[2] = '.';
	}

	snprintf(title, sizeof(title), "%s %s", status, name_ptr);

	ui_window_draw_text_attr(c->win, 0, title_y, title, w_w,
			title_fg, title_bg,
			UI_TEXT_STYLE_NORMAL);

	ui_window_cursor_set(c->win, x, y);
}

static void buf_update(Window *w);

static void
__draw(Window *c, bool force, bool fire_event) {
	if (is_in_kbd_action) {
		c->pending_draw_evt = true;
		return;
	}

	if ((force || buffer_is_dirty(c->buf) && is_content_visible(c)) || c == get_popup()) {
		debug("%s: buffer name: %s\n", __func__, buffer_name_get(c->buf));
		/* we assume that it will be set on EVT_WIN_DRAW */
		/* ui_window_sidebar_width_set(c->win, 0); */
		ui_window_clear(c->win);

		buf_update(c);

		if (fire_event) {
			event_t evt = {};

			evt.eid = EVT_WIN_DRAW;
			evt.oid = c->id;
			scheme_event_handle(evt);
		}

		ui_window_draw(c->win);

		if (!isarrange(fullscreen) || c == current_window())
			draw_title(c);

		ui_window_refresh(c->win);
	}
}

static void draw(Window *c, bool force) {
	__draw(c, force, true);
}

static void
draw_all(void) {
	if (topbar) {
		draw(topbar, true);
	}
	if (minibuf) {
		draw(minibuf, true);
	}

	if (!nextvisible(windows)) {
		set_current_window(NULL);
		ui_cursor_enable(ui, false);
		ui_clear(ui);
		drawbar();
		ui_update(ui);
		return;
	}

	if (!isarrange(fullscreen)) {
		for (Window *c = nextvisible(windows); c; c = nextvisible(c->next)) {
			if (c != current_window()) {
				draw(c, true);
			}
		}
	}

	/* as a last step the selected window is redrawn,
	 * this has the effect that the cursor position is
	 * accurate
	 */
	if (current_window()) {
		draw(current_window(), true);
	}
}

static void
arrange(void) {
	unsigned int m = 0, n = 0, dh = 0;
	for (Window *c = nextvisible(windows); c; c = nextvisible(c->next)) {
		c->order = ++n;
		if (c->minimized)
			m++;
	}

	ui_clear(ui);

	if (m && !isarrange(fullscreen)) {
		if (min_align == MIN_ALIGN_VERT)
			dh = m;
		else
			dh = 1;
	}
	wah -= dh;
	layout->arrange(wax, way, waw, wah);
	if (m && !isarrange(fullscreen)) {
		unsigned int i = 0, nw = waw / m, nx = wax;
		for (Window *c = nextvisible(windows); c; c = nextvisible(c->next)) {
			if (c->minimized) {
				if (min_align == MIN_ALIGN_VERT) {
					resize(c, nx, way+wah+i, waw, 1);
					i++;
				} else {
					resize(c, nx, way+wah, ++i == m ? waw - nx : nw, 1);
					nx += nw;
				}
			}
		}
		wah += dh;
	}
	focus(NULL);
	ui_refresh(ui);
	drawbar();
	draw_all();
}

static Window *
lastmaster(unsigned int tag) {
	Window *c = windows;
	int n = 1;

	for (; c && !(c->tags & tag); c = c->next);
	for (; c && n < getnmaster(); c = c->next, n++);

	return c;
}

static void
attachfirst(Window *c) {
	if (windows)
		windows->prev = c;
	c->next = windows;
	c->prev = NULL;
	windows = c;
	for (int o = 1; c; c = nextvisible(c->next), o++)
		c->order = o;
}

static void
attach(Window *c) {
	if (ismastersticky(NULL)) {
		Window *master = lastmaster(c->tags);

		if (master) {
			attachafter(c, master);
			return;
		}
	}

	attachfirst(c);
}

static void
attachafter(Window *c, Window *a) { /* attach c after a */
	if (c == a)
		return;
	if (!a)
		for (a = windows; a && a->next; a = a->next);

	if (a) {
		if (a->next)
			a->next->prev = c;
		c->next = a->next;
		c->prev = a;
		a->next = c;
		for (int o = a->order; c; c = nextvisible(c->next))
			c->order = ++o;
	}
}

static void
attachstack(Window *c) {
	c->snext = stack;
	stack = c;
}

static void
detach(Window *c) {
	Window *d;
	if (c->prev)
		c->prev->next = c->next;
	if (c->next) {
		c->next->prev = c->prev;
		for (d = nextvisible(c->next); d; d = nextvisible(d->next))
			--d->order;
	}
	if (c == windows)
		windows = c->next;
	c->next = c->prev = NULL;
}

static void
settitle(Window *c) {
	char *term, *t = title;
	char *ctitle = window_get_title(c);
	if (!t && current_window() == c && ctitle && strlen(ctitle))
		t = ctitle;
	if (t && (term = getenv("TERM")) && !strstr(term, "linux")) {
		printf("\033]0;%s\007", t);
		fflush(stdout);
	}
}

static void
detachstack(Window *c) {
	Window **tc;
	for (tc = &stack; *tc && *tc != c; tc = &(*tc)->snext);
	*tc = c->snext;
}

static void
focus(Window *c) {
	if (!c)
		for (c = stack; c && !isvisible(c); c = c->snext);

	if (current_window() == c)
		return;

	if (c) {
		if (c->minimized)
			curr_kmap = win_min_kmap;
		else
			curr_kmap = global_kmap;
	}

	lastsel = current_window();
	set_current_window(c);
	if (lastsel) {
		lastsel->urgent = false;
		if (!isarrange(fullscreen)) {
			draw_title(lastsel);
			ui_window_refresh(lastsel->win);
		}
	}

	if (c) {
		Process *proc = buffer_proc_get(c->buf);
		Selection *s;

		detachstack(c);
		attachstack(c);
		settitle(c);
		c->urgent = false;

		if (proc && buffer_ref_count(c->buf) > 2) {
			vt_resize(process_term_get(proc),
					ui_window_height_get(c->win) - ui_window_has_title(c->win),
					ui_window_width_get(c->win));
		}

		if (isarrange(fullscreen)) {
			draw(c, true);
		} else {
			draw_title(c);
			ui_window_refresh(c->win);
		}

		if (proc) {
			ui_cursor_enable(ui, c && !c->minimized &&
					vt_cursor_visible(process_term_get(proc)));
		} else {
			size_t curs_view = view_cursor_get(c->view);

			buffer_cursor_set(c->buf, curs_view);
			buffer_dirty_set(c->buf, true);
			ui_cursor_enable(ui, true);
		}
	}
}

static void
resize_window(Window *c, int w, int h) {
	ui_window_resize(c->win, w, h);

	if (buffer_proc_get(c->buf)) {
		Process *proc = buffer_proc_get(c->buf);

		if (c == get_popup()) {
			w-=-2;
			h--;
		}
		vt_resize(process_term_get(proc), h - ui_window_has_title(c->win), w);
	}
}

static void
resize(Window *c, int x, int y, int w, int h) {
	resize_window(c, w, h);
	ui_window_move(c->win, x, y);
}

static Window*
get_window_by_coord(unsigned int x, unsigned int y) {
	if (y < way || y >= way+wah)
		return NULL;
	if (isarrange(fullscreen))
		return current_window();
	for (Window *c = nextvisible(windows); c; c = nextvisible(c->next)) {
		int w_h = ui_window_height_get(c->win);
		int w_w = ui_window_width_get(c->win);
		int w_y = ui_window_y_get(c->win);
		int w_x = ui_window_x_get(c->win);

		if (x >= w_x && x < w_x + w_w && y >= w_y && y < w_y + w_h) {
			debug("mouse event, x: %d y: %d window: %d\n", x, y, c->order);
			return c;
		}
	}
	return NULL;
}

static void
sigchld_handler(int sig) {
	ProcessInfo pinfo;
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
		pinfo.status = status;
		pinfo.pid = pid;
		write(proc_fd[1], &pinfo, sizeof(pinfo));
	}

	errno = errsv;
}

static void
sigterm_handler(int sig) {
	running = false;
}

static unsigned int
bitoftag(const char *tag) {
	unsigned int i;
	if (!tag)
		return ~0;
	for (i = 0; (i < LENGTH(tags)) && strcmp(tags[i], tag); i++);
	return (i < LENGTH(tags)) ? (1 << i) : 0;
}

static void
tagschanged() {
	bool allminimized = true;
	for (Window *c = nextvisible(windows); c; c = nextvisible(c->next)) {
		if (!c->minimized) {
			allminimized = false;
			break;
		}
	}
	if (allminimized && nextvisible(windows)) {
		focus(NULL);
		toggleminimize();
	}
	arrange();
}

static void
toggletag(const char *args[]) {
	if (!current_window())
		return;
	unsigned int newtags = current_window()->tags ^ (bitoftag(args[0]) & TAGMASK);
	if (newtags) {
		current_window()->tags = newtags;
		tagschanged();
	}
}

static void
setpertag(void) {
	layout = pertag.layout[pertag.curtag];
	runinall = pertag.runinall[pertag.curtag];
}

static void
toggleview(const char *args[]) {
	int i;

	unsigned int newtagset = tagset[seltags] ^ (bitoftag(args[0]) & TAGMASK);
	if (newtagset) {
		if(newtagset == TAGMASK) {
			pertag.prevtag = pertag.curtag;
			pertag.curtag = 0;
		} else if(!(newtagset & 1 << (pertag.curtag - 1))) {
			pertag.prevtag = pertag.curtag;
			for (i=0; !(newtagset &1 << i); i++) ;
			pertag.curtag = i + 1;
		}
		setpertag();
		tagset[seltags] = newtagset;
		tagschanged();
	}
}

static void
viewprevtag(const char *args[]) {
	unsigned int tmptag;

	seltags ^= 1;
	tmptag = pertag.prevtag;
	pertag.prevtag = pertag.curtag;
	pertag.curtag = tmptag;
	setpertag();
	tagschanged();
}

static void
keypress(int code) {
	int key = -1;
	unsigned int len = 1;
	char buf[8] = { '\e' };

	if (code == '\e') {
		/* pass characters following escape to the underlying app */
		nodelay(stdscr, TRUE);
		for (int t; len < sizeof(buf) && (t = getch()) != ERR; len++) {
			if (t > 255) {
				key = t;
				break;
			}
			buf[len] = t;
		}
		nodelay(stdscr, FALSE);
	}

	for (Window *c = pertag.runinall[pertag.curtag] ? nextvisible(windows) : current_window(); c; c = nextvisible(c->next)) {
		if (is_content_visible(c)) {
			c->urgent = false;

			if (buffer_proc_get(c->buf)) {
				Vt *term = process_term_get(buffer_proc_get(c->buf));

				if (code == '\e')
					vt_write(term, buf, len);
				else
					vt_keypress(term, code);
				if (key != -1)
					vt_keypress(term, key);
			} else if (buffer_text_input_is_enabled(c->buf)) {
				event_t evt = {};

				evt.eid = EVT_TEXT_INSERT;
				evt.oid = code;
				scheme_event_handle(evt);
			}
		}
		if (!pertag.runinall[pertag.curtag])
			break;
	}
}

static void
mouse_setup(void) {
#ifdef CONFIG_MOUSE
	mmask_t mask = 0;

	if (mouse_events_enabled) {
		mask = BUTTON1_CLICKED | BUTTON2_CLICKED;
		for (unsigned int i = 0; i < LENGTH(buttons); i++)
			mask |= buttons[i].mask;
	}
	mousemask(mask, NULL);
#endif /* CONFIG_MOUSE */
}

static void
initpertag(void) {
	int i;

	pertag.curtag = pertag.prevtag = 1;
	for(i=0; i <= LENGTH(tags); i++) {
		pertag.nmaster[i] = NMASTER;
		pertag.mfact[i] = MFACT;
		pertag.layout[i] = layout;
		pertag.layout_prev[i] = layout;
		pertag.runinall[i] = runinall;
		pertag.msticky[i] = false;
		pertag.name[i] = NULL;
		pertag.cwd[i] = calloc(CWD_MAX, 1);
		getcwd(pertag.cwd[i], CWD_MAX);
	}
}

static bool
checkshell(const char *shell) {
	if (shell == NULL || *shell == '\0' || *shell != '/')
		return false;
	if (!strcmp(strrchr(shell, '/')+1, prog_name))
		return false;
	if (access(shell, X_OK))
		return false;
	return true;
}

static const char *
getshell(void) {
	const char *shell = getenv("SHELL");
	struct passwd *pw;

	if (checkshell(shell))
		return shell;
	if ((pw = getpwuid(getuid())) && checkshell(pw->pw_shell))
		return pw->pw_shell;
	return "/bin/sh";
}

static void init_default_keymap(void)
{
	global_kmap = keymap_new(NULL);
	curr_kmap = global_kmap;

	win_min_kmap = keymap_new(global_kmap);
	keymap_bind(win_min_kmap, "<Enter>", toggleminimize, NULL);
	keymap_bind(win_min_kmap, "x", killwindow, NULL);
}

static CellStyle get_default_cell_style(Ui *ui)
{
	Style *default_style = style_get_by_id(0);
	CellStyle cell_style = {
		.attr = default_style->attr,
		.fg = default_style->fg,
		.bg = default_style->bg,
	};

	return cell_style;
}

static void
setup(void) {
	shell = getshell();
	setlocale(LC_CTYPE, "");

	char *term = getenv("BORSCH_TERM");
	if (!term)
		term = "borsch";
	snprintf(term_name, sizeof term_name, "%s%s", term, COLORS >= 256 ? "-256color" : "");

	style_init();

	ui = ui_term_new();
	ui->get_default_cell_style = get_default_cell_style;
	ui_init(ui);

	init_default_keymap();
	mouse_setup();
	syntax_init();
	style_init();
	vt_init();

	for (unsigned int i = 0; i < LENGTH(colors); i++) {
		if (COLORS == 256) {
			if (colors[i].fg256)
				colors[i].fg = colors[i].fg256;
			if (colors[i].bg256)
				colors[i].bg = colors[i].bg256;
		}
		colors[i].pair = ui_color_make(ui, colors[i].fg, colors[i].bg);
	}
	initpertag();
	update_screen_size();
	arrange();
	struct sigaction sa;
	memset(&sa, 0, sizeof sa);
	sa.sa_flags = 0;
	sigemptyset(&sa.sa_mask);
	sa.sa_handler = sigchld_handler;
	sigaction(SIGCHLD, &sa, NULL);
	sa.sa_handler = sigterm_handler;
	sigaction(SIGTERM, &sa, NULL);
	sa.sa_handler = SIG_IGN;
	sigaction(SIGPIPE, &sa, NULL);
	scheme_init(scheme_init_script);
	buf_list_update();
}

static void __win_del(Window *w)
{
	if (current_window() == w)
		focusnextnm(NULL);

	if (w != get_popup()) {
		detach(w);
	} else {
		set_popup(NULL);
	}
	detachstack(w);

	if (current_window() == w) {
		Window *next = nextvisible(windows);
		if (next) {
			focus(next);
			toggleminimize();
		} else {
			set_current_window(NULL);
		}
	}
	if (lastsel == w)
		lastsel = NULL;
	ui_window_free(w->win);
	view_free(w->view);
	free(w);
	arrange();
}

static Buffer *__buf_new(const char *name, KeyMap *kmap)
{
	Buffer *buf = buffer_new(name);

	if (buf) {
		keymap_parent_set(buffer_keymap_get(buf), kmap);
		buffer_env_set(buf, scheme_env_alloc());
		buffer_ref_get(buf);
		return buf;
	}

	return NULL;
}

void buf_prop_del_cb(Buffer *buf, size_t type, size_t start, size_t end,
		     void *data, void *arg)
{
	if (type == PROPERTY_TYPE_TEXT_KEYMAP) {
		KeyMap *map = data;

		keymap_parent_set(map, NULL);
		keymap_ref_put(map);
	} else {
		free(data);
	}
}

static void __buf_del(Buffer *buf)
{
	void *env;

	env = buffer_env_get(buf);

	buffer_property_remove_cb(buf, PROPERTY_TYPE_ALL, EPOS, EPOS, NULL, NULL, buf_prop_del_cb);
	buffer_ref_put(buf);
	if (buffer_del(buf)) {
		if (buffer_proc_get(buf))
			process_destroy(buffer_proc_get(buf));
		scheme_env_free(env);
	}
}

static void
destroy(Window *w) {
	Buffer *buf = w->buf;

	__win_del(w);
	__buf_del(buf);
}

static void
cleanup(void) {
	Process *proc;
	Buffer *b;
	int i;

	/* issue when deleting fd handlers during execution the fd handler ? */
	event_fd_handler_unregister(STDIN_FILENO);
	if (cmdfifo.fd != -1)
		event_fd_handler_unregister(cmdfifo.fd);

	scheme_uninit();
	while (windows)
		destroy(windows);
	for(i=0; i <= LENGTH(tags); i++) {
		if (pertag.popup[i])
			destroy(pertag.popup[i]);
	}

	b = buffer_first_get();
	while (b) {
		Buffer *nextb = buffer_next_get(b);
		__buf_del(b);
		b = nextb;
	}

	proc = proc_list.next;
	while (proc) {
		Process *next = proc->next;

		process_destroy(proc);
	
		proc = next;
	}

	keymap_free(win_min_kmap);
	keymap_free(global_kmap);
	vt_shutdown();
	syntax_cleanup();
	style_cleanup();
	ui_free(ui);
	if (cmdfifo.fd > 0)
		close(cmdfifo.fd);
	if (cmdfifo.file)
		unlink(cmdfifo.file);
	if (retfifo.fd > 0)
		close(retfifo.fd);
	if (retfifo.file)
		unlink(retfifo.file);
	for(i=0; i <= LENGTH(tags); i++) {
		free(pertag.name[i]);
		free(pertag.cwd[i]);
	}
}

static void __win_buf_attach(Window *w, Buffer *buf)
{
#if 0
	ui_window_text_style_set(w->win, buffer_text_style_get(buf));
	ui_window_text_fg_set(w->win, buffer_text_fg_get(buf));
	ui_window_text_bg_set(w->win, buffer_text_bg_get(buf));
#endif
}

static char *getcwd_by_pid(Window *c, char *buf) {
	if (!c)
		return NULL;
	Process *proc = buffer_proc_get(c->buf);
	char tmp[32];
	snprintf(tmp, sizeof(tmp), "/proc/%d/cwd", process_pid_get(proc));
	return realpath(tmp, buf);
}

static void
synctitle(Window *c)
{
	size_t len = 256;
	char buf[128];
	char path[64];
	size_t blen;
	char *eol;
	pid_t pid;
	int pty;
	int ret;
	int fd;

	pty = vt_pty_get(process_term_get(buffer_proc_get(c->buf)));

	pid = tcgetpgrp(pty);
	if (pid == -1)
		return;

	snprintf(path, sizeof(path), "/proc/%d/cmdline", pid);

	fd = open(path, O_RDONLY);
	if (fd == -1)
		return;

	blen = MIN(sizeof(buf), len);

	ret = read(fd, buf, blen);
	if (ret <= 0)
		goto done;

	buf[ret - 1] = '\0';

	buffer_name_set(c->buf, basename(buf));

	settitle(c);
	if (!isarrange(fullscreen) || current_window() == c)
		draw_title(c);
done:
	close(fd);
}

static void vt_filter(Vt *vt, char *ch, size_t len, void *arg)
{
	Buffer *buf = arg;
	event_t evt = {};

	evt.eid = EVT_VTERM_FILTER;
	evt.oid = buffer_id_get(buf);
	evt.len = len;
	evt.str = ch;
	scheme_event_handle(evt);
}

int term_create(const char *prog, const char *title, const char *cwd, const char **env) {
	Process *proc;
	char tmppath[PATH_MAX];
	char tmp[256];

	if (get_popup())
		return -1;

	Window *c = calloc(1, sizeof(Window));
	if (!c)
		return -1;
	c->tags = tagset[seltags];
	c->id = ++cmdfifo.id;

	c->buf = __buf_new(title, global_kmap);
	if (!c->buf) {
		free(c);
		return -1;
	}

	c->view = view_new(buffer_text_get(c->buf));
	if (!c->view) {
		__buf_del(c->buf);
		free(c);
	}

	c->win = ui_window_new(ui, c->view);
	if (!c->win) {
		view_free(c->view);
		__buf_del(c->buf);
		free(c);
		return -1;
	}
	ui_window_resize(c->win, waw, wah);
	ui_window_move(c->win, wax, way);

	proc = process_create(prog, cwd, NULL, NULL, NULL, env, true);
	if (!proc) {
		view_free(c->view);
		__buf_del(c->buf);
		free(c);
		return -1;
	}
	ui_window_has_title_set(c->win, true);
	process_buffer_set(proc, c->buf);
	buffer_proc_set(c->buf, proc);
	process_attach_win(proc, c);

	if (prog) {
		c->cmd = prog;
		strncpy(tmppath, prog, sizeof(tmppath));
		tmppath[sizeof(tmppath)-1] = '\0';
		strncpy(tmp, basename(tmppath), sizeof(tmp));
		if (!buffer_name_is_locked(c->buf))
			buffer_name_set(c->buf, tmp);
	} else {
		c->cmd = shell;
	}

	ui_window_resize(c->win, waw, wah);
	ui_window_move(c->win, wax, way);
	attach(c);
	focus(c);
	arrange();

	return c->id;
}

static void
focusn(const char *args[]) {
	for (Window *c = nextvisible(windows); c; c = nextvisible(c->next)) {
		if (c->order == atoi(args[0])) {
			focus(c);
			if (c->minimized)
				toggleminimize();
			return;
		}
	}
}

static void
__focusid(int win_id) {
	for (Window *c = windows; c; c = c->next) {
		if (c->id == win_id) {
			focus(c);
			if (!isvisible(c)) {
				c->tags |= tagset[seltags];
				tagschanged();
			}
			return;
		}
	}

	if (minibuf && minibuf->id == win_id)
		focus(minibuf);
}

static void
focusnextnm(const char *args[]) {
	if (!current_window())
		return;
	Window *c = current_window();
	do {
		c = nextvisible(c->next);
		if (!c)
			c = nextvisible(windows);
	} while (c && c->minimized && c != current_window());
	focus(c);
}

static void
focusprevnm(const char *args[]) {
	if (!current_window())
		return;
	Window *c = current_window();
	do {
		for (c = c->prev; c && !isvisible(c); c = c->prev);
		if (!c) {
			for (c = windows; c && c->next; c = c->next);
			for (; c && !isvisible(c); c = c->prev);
		}
	} while (c && c != current_window() && c->minimized);
	focus(c);
}

static void
focuslast(const char *args[]) {
	if (lastsel)
		focus(lastsel);
}

static void
killwindow(void) {
	Window *target = current_window();

	if (!target)
		return;

	destroy(target);
}

static void killother(const char *args[]) {
	unsigned int n;
	Window *c;

	for (n = 0, c = nextvisible(windows); c; c = nextvisible(c->next)) {
		if (ismastersticky(c) || current_window() == c)
			continue;
		destroy(c);
	}
}

static void
quit(const char *args[]) {
	cleanup();
	exit(EXIT_SUCCESS);
}

static void
redraw(const char *args[]) {
	for (Window *c = windows; c; c = c->next) {
		if (!c->minimized) {
			Process *proc = buffer_proc_get(c->buf);

			if (proc)
				vt_dirty(process_term_get(proc));
			ui_window_redraw(c->win);
		}
	}
	ui_redraw(ui);
	update_screen_size();
	arrange();
}

static void
scrollback(const char *args[]) {
	int w_h = ui_window_height_get(current_window()->win);
	Process *proc;
	Vt *term;

	if (!is_content_visible(current_window()))
		return;

	proc = buffer_proc_get(current_window()->buf);
	if (!proc)
		return;

	term = process_term_get(proc);
	if (term)
		if (!args[0] || atoi(args[0]) < 0)
			vt_scroll(term, -w_h/2);
		else
			vt_scroll(term,  w_h/2);

	if (term)
		ui_cursor_enable(ui, vt_cursor_visible(term));
	else
		ui_cursor_enable(ui, true);
	draw(current_window(), true);
}

static void
setlayout(const char *args[]) {
	unsigned int i;

	if (!args || !args[0]) {
		if (++layout == &layouts[LENGTH(layouts)])
			layout = &layouts[0];
	} else {
		for (i = 0; i < LENGTH(layouts); i++)
			if (!strcmp(args[0], layouts[i].symbol))
				break;
		if (i == LENGTH(layouts))
			return;
		layout = &layouts[i];
	}
	pertag.layout_prev[pertag.curtag] = pertag.layout[pertag.curtag];
	pertag.layout[pertag.curtag] = layout;
	arrange();
}

static int
getnmaster(void) {
	return pertag.nmaster[pertag.curtag];
}

static float
getmfact(void) {
	return pertag.mfact[pertag.curtag];
}

static void
toggleminimize(void)
{
	Window *c, *m, *t;
	unsigned int n;

	if (!current_window())
		return;
	/* do not minimize sticked master */
	if (ismastersticky(current_window()))
		return;
	/* the last window can't be minimized */
	if (!current_window()->minimized) {
		for (n = 0, c = nextvisible(windows); c; c = nextvisible(c->next))
			if (!c->minimized)
				n++;
		if (n == 1)
			return;
	}
	current_window()->minimized = !current_window()->minimized;
	m = current_window();
	/* check whether the master window was minimized */
	if (current_window() == nextvisible(windows) && current_window()->minimized) {
		c = nextvisible(current_window()->next);
		detach(c);
		attach(c);
		focus(c);
		detach(m);
		for (; c && (t = nextvisible(c->next)) && !t->minimized; c = t);
		attachafter(m, c);
	} else if (m->minimized) {
		/* non master window got minimized move it above all other
		 * minimized ones */
		focusnextnm(NULL);
		detach(m);
		for (c = nextvisible(windows); c && (t = nextvisible(c->next)) && !t->minimized; c = t);
		attachafter(m, c);
	} else { /* window is no longer minimized, move it to the master area */
		Process *proc = buffer_proc_get(m->buf);
		if (proc)
			vt_dirty(process_term_get(proc));
		detach(m);
		attach(m);
	}
	arrange();
}

static void minimizeother(const char *args[])
{
	unsigned int n;
	Window *c;

	for (n = 0, c = nextvisible(windows); c; c = nextvisible(c->next)) {
		if (ismastersticky(c) || current_window() == c)
			continue;

		c->minimized = true;
	}

	arrange();
}

static void
togglemouse(const char *args[]) {
	mouse_events_enabled = !mouse_events_enabled;
	mouse_setup();
}

static void
togglerunall(const char *args[]) {
	pertag.runinall[pertag.curtag] = !pertag.runinall[pertag.curtag];
	drawbar();
	draw_all();
}

static void
zoom(const char *args[]) {
	Window *c;

	if (!current_window())
		return;
	if (args && args[0])
		focusn(args);
	if ((c = current_window()) == nextvisible(windows))
		if (!(c = nextvisible(c->next)))
			return;
	detach(c);
	attachfirst(c);
	focus(c);
	if (c->minimized)
		toggleminimize();
	arrange();
}

/* commands for use by mouse bindings */
static void
mouse_focus(const char *args[]) {
	focus(msel);
	if (msel->minimized)
		toggleminimize();
}

static void
mouse_fullscreen(const char *args[]) {
	mouse_focus(NULL);
	setlayout(isarrange(fullscreen) ? NULL : args);
}

static void
mouse_minimize(const char *args[]) {
	focus(msel);
	toggleminimize();
}

static void
mouse_zoom(const char *args[]) {
	focus(msel);
	zoom(NULL);
}

static Cmd *
get_cmd_by_name(const char *name) {
	for (unsigned int i = 0; i < LENGTH(commands); i++) {
		if (!strcmp(name, commands[i].name))
			return &commands[i];
	}
	return NULL;
}

static void
handle_cmd(char *cmdbuf) {
	char *p, *s, c;
	Cmd *cmd;

	p = cmdbuf;
	while (*p) {
		/* find the command name */
		for (; *p == ' ' || *p == '\n'; p++);
		for (s = p; *p && *p != ' ' && *p != '\n'; p++);
		if ((c = *p))
			*p++ = '\0';
		if (*s && (cmd = get_cmd_by_name(s)) != NULL) {
			bool quote = false;
			int argc = 0;
			const char *args[MAX_ARGS], *arg;
			memset(args, 0, sizeof(args));
			/* if arguments were specified in config.h ignore the one given via
			 * the named pipe and thus skip everything until we find a new line
			 */
			if (cmd->action.args[0] || c == '\n') {
				debug("execute %s", s);
				cmd->action.cmd(cmd->action.args);
				while (*p && *p != '\n')
					p++;
				continue;
			}
			/* no arguments were given in config.h so we parse the command line */
			while (*p == ' ')
				p++;
			arg = p;
			for (; (c = *p); p++) {
				switch (*p) {
				case '\\':
					/* remove the escape character '\\' move every
					 * following character to the left by one position
					 */
					switch (p[1]) {
						case '\\':
						case '\'':
						case '\"': {
							char *t = p+1;
							do {
								t[-1] = *t;
							} while (*t++);
						}
					}
					break;
				case '\'':
				case '\"':
					quote = !quote;
					break;
				case ' ':
					if (!quote) {
				case '\n':
						/* remove trailing quote if there is one */
						if (*(p - 1) == '\'' || *(p - 1) == '\"')
							*(p - 1) = '\0';
						*p++ = '\0';
						/* remove leading quote if there is one */
						if (*arg == '\'' || *arg == '\"')
							arg++;
						if (argc < MAX_ARGS)
							args[argc++] = arg;

						while (*p == ' ')
							++p;
						arg = p--;
					}
					break;
				}

				if (c == '\n' || *p == '\n') {
					if (!*p)
						p++;
					debug("execute %s", s);
					for(int i = 0; i < argc; i++)
						debug(" %s", args[i]);
					debug("\n");
					cmd->action.cmd(args);
					break;
				}
			}
		}
	}
}

static void handle_cmdfifo(int fd, void *arg) {
	char *p, cmdbuf[512];
	int r;

	r = read(cmdfifo.fd, cmdbuf, sizeof cmdbuf - 1);
	if (r <= 0) {
		cmdfifo.fd = -1;
		return;
	}

	cmdbuf[r] = '\0';
	handle_cmd(cmdbuf);
}

static void doeval(const char *args[]) {
	char tmp[10];
	int ret;

	if (!args || !args[0] || !args[1])
		return;

	ret = scheme_eval_file(args[0], args[1]);

	write(retfifo.fd, tmp, snprintf(tmp, sizeof(tmp), "%u\n", ret));
}

static void
handle_mouse(void) {
#ifdef CONFIG_MOUSE
	unsigned int i;
	MEVENT event;
	int w_x, w_y;

	if (getmouse(&event) != OK)
		return;

	msel = get_window_by_coord(event.x, event.y);
	if (!msel)
		return;

	w_x = ui_window_x_get(msel->win);
	w_y = ui_window_y_get(msel->win);

	debug("mouse x:%d y:%d cx:%d cy:%d mask:%d\n", event.x, event.y, event.x - w_x, event.y - w_y, event.bstate);

	if (buffer_proc_get(msel->buf)) {
		Process *proc = buffer_proc_get(msel->buf);
		vt_mouse(process_term_get(proc), event.x - w_x, event.y - w_y, event.bstate);
	}

	for (i = 0; i < LENGTH(buttons); i++) {
		if (event.bstate & buttons[i].mask)
			buttons[i].action.cmd(buttons[i].action.args);
	}

	msel = NULL;
#endif /* CONFIG_MOUSE */
}

static int
__open_or_create_fifo(const char *name, const char **name_created, int flags) {
	struct stat info;
	int fd;

	do {
		if ((fd = open(name, flags)) == -1) {
			if (errno == ENOENT && !mkfifo(name, S_IRUSR|S_IWUSR)) {
				*name_created = name;
				continue;
			}
			error("%s\n", strerror(errno));
		}
	} while (fd == -1);

	if (fstat(fd, &info) == -1)
		error("%s\n", strerror(errno));
	if (!S_ISFIFO(info.st_mode))
		error("%s is not a named pipe\n", name);
	return fd;
}

static int
open_or_create_fifo(const char *name, const char **name_created) {
	return __open_or_create_fifo(name, name_created, O_RDWR|O_NONBLOCK);
}

static void
usage(void) {
	cleanup();
	eprint("usage: "PROGNAME" [-v] [-M] [-m mod] [-d delay] [-h lines] [-t title] "
	       "[-s status-fifo] [-c cmd-fifo] [cmd...]\n");
	exit(EXIT_FAILURE);
}

static bool
parse_args(int argc, char *argv[]) {
	bool init = false;
	const char *name = argv[0];

	if (name && (name = strrchr(name, '/')))
		prog_name = name + 1;
	if (!getenv("ESCDELAY"))
		set_escdelay(100);

	for (int arg = 1; arg < argc; arg++) {
		if (strcmp(argv[arg], "-i") == 0) {
			scheme_init_script = argv[arg+1];
			arg++;
		} else if (strcmp(argv[arg], "-n") == 0) {
			scheme_init_script = NULL;
		}
	}

	return init;
}

static int buf_keymap_prop_match(Buffer *buf, int id, size_t start, size_t end, void *data,
				 void *arg)
{
	KeyMap **map = arg;
	*map = (KeyMap *) data;
}

static KeyMap *buf_keymap_get(Buffer *buf)
{
	size_t cursor = buffer_cursor_get(buf);
	KeyMap *tmap = NULL;

	buffer_properties_walk(buf, PROPERTY_TYPE_TEXT_KEYMAP,
			       cursor, cursor+1, &tmap, buf_keymap_prop_match);

	if (tmap)
		return tmap;

	return buffer_keymap_get(buf);
}

static bool keybuf_enqueue(KeyBuf *kbuf, int code, int flags)
{
	KeyCode *key;

	if (kbuf->key_index >= MAX_KEYS)
		return false;

	key = &kbuf->keys[kbuf->key_index++];

	key->flags = flags;
	key->code = code;
	return true;
}

static void keybuf_clear(KeyBuf *kbuf)
{
	memset(kbuf->keys, 0, sizeof(kbuf->keys));
	kbuf->key_index = 0;
}

static void keybuf_flush(KeyBuf *kbuf)
{
	for (int i = 0; i < kbuf->key_index; i++) {
		KeyCode *key = &kbuf->keys[i];
		int code = key->code;

		if (key->flags & KEY_MOD_F_CTL)
			code = CTRL(code);
		else if (key->flags & KEY_MOD_F_ALT)
			keypress(ALT);

		keypress(code);
	}

	keybuf_clear(kbuf);
}

static void handle_keypress(int fd, void *arg)
{
	KeyBinding *kbd = NULL;
	KeyBuf *kbuf = arg;
	int alt_code;
	event_t evt = {};
	int flags;
	int code;

	if (!current_window()) {
		curr_kmap = global_kmap;
	} else if (current_window() && !current_window()->minimized) {
		KeyMap *map = buf_keymap_get(current_window()->buf);
		if (map)
			curr_kmap = map;
	};

reenter:
	code = getch();
	flags = 0;

	evt.eid = EVT_KEY_PRESS;
	evt.oid = code;
	scheme_event_handle(evt);

	if (code == ALT) {
		nodelay(stdscr, TRUE);
		alt_code = getch();
		nodelay(stdscr, FALSE);
		if (alt_code > 0) {
			flags |= KEY_MOD_F_ALT;
			code = alt_code;
		}
	} else if (code < 0x1f && code != 0xd) {
		flags |= KEY_MOD_F_CTL;
		code = code + 0x60;
	}

	if (code < 0)
		return;

	if (code == KEY_MOUSE) {
		handle_mouse();
		return;
	}


	if (!keybuf_enqueue(kbuf, code, flags)) {
		keybuf_flush(kbuf);
		return;	
	}

	if ((kbd = keymap_match(curr_kmap, kbuf->keys, kbuf->key_index))) {
		if (keymap_kbd_is_map(kbd)) {
			curr_kmap = keymap_kbd_map_get(kbd);
			keybuf_clear(kbuf);
			goto reenter;
		}

		if (keymap_kbd_len(kbd) == kbuf->key_index) {
			debug("kbd action: enter\n");
			is_in_kbd_action = true;
			keymap_kbd_action(kbd);
			debug("kbd action: exit\n");
			is_in_kbd_action = false;
			keybuf_clear(kbuf);
		}
	} else {
		keybuf_flush(kbuf);
	}

	for (Window *c = nextvisible(windows); c; c = nextvisible(c->next)) {
		if (c->pending_draw_evt) {
			c->pending_draw_evt = false;
			draw(c, true);
		}
	}
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
			if (WIFEXITED(pinfo.status)) {
				process_status_set(proc, WEXITSTATUS(pinfo.status));
			}
			if (!process_buffer_get(proc)) {
				process_died_set(proc, true);
				evt.eid = EVT_PROC_EXIT;
				evt.oid = pinfo.pid;
				scheme_event_handle(evt);
			}
		}
	}
}

int main(int argc, char *argv[]) {
	sigset_t blockset;
	event_t evt = {};

	setenv("BORSCH", VERSION, 1);
	if (!parse_args(argc, argv)) {
		setup();
	}

	sigemptyset(&blockset);
	sigaddset(&blockset, SIGWINCH);
	sigprocmask(SIG_BLOCK, &blockset, NULL);

	if (cmdfifo.fd != -1)
		event_fd_handler_register(cmdfifo.fd, handle_cmdfifo, NULL);

	pipe2(proc_fd, O_NONBLOCK);
	event_fd_handler_register(proc_fd[0], handle_sigchld_io, NULL);

	event_fd_handler_register(STDIN_FILENO, handle_keypress, &kbuf);

	update_screen_size();

	while (running) {
		Process *proc = proc_list.next;

		if (ui_resize(ui)) {
			update_screen_size();
			redraw(NULL);
			continue;
		}

		while (proc) {
			Process *next = proc->next;

			if (process_is_died(proc))
				process_destroy(proc);
	
			proc = next;
		}

		for (Window *c = windows; c; ) {
			if (buffer_proc_get(c->buf)) {
				Process *proc = buffer_proc_get(c->buf);
				vt_processed_set(process_term_get(proc), false);
			}
			c = c->next;
		}
		/* TODO: what to do with a died buffers ? */

		ui_update(ui);

		event_process();

	        if (topbar) {
		   draw(topbar, true);
	        }
		if (minibuf) {
			draw(minibuf, false);
		}

		for (Window *c = windows; c; c = c->next) {
			if (is_content_visible(c)) {
				if (buffer_proc_get(c->buf) && !buffer_name_is_locked(c->buf))
					synctitle(c);
				if (c != current_window()) {
					draw(c, false);
				}
			} else if (!isarrange(fullscreen) && isvisible(c)
					&& c->minimized) {
				draw_title(c);
				ui_window_refresh(c->win);
			}
		}

		if (is_content_visible(current_window())) {
			int x, y;

			if (buffer_proc_get(current_window()->buf)) {
				Process *proc = buffer_proc_get(current_window()->buf);
				ui_cursor_enable(ui, vt_cursor_visible(process_term_get(proc)));
			} else {
				ui_cursor_enable(ui, true);
			}
			draw(current_window(), false);
		        ui_window_cursor_get(current_window()->win, &x, &y);
		        ui_window_cursor_set(current_window()->win, x, y);
		}
	}

	cleanup();
	return 0;
}

static Window *window_get_by_id(int id)
{
	for (Window *c = windows; c; c = c->next) {
		if (c->id == id)
			return c;
	}

	if (get_popup() && get_popup()->id == id)
		return get_popup();
	else if (minibuf && minibuf->id == id)
		return minibuf;
	else if (topbar && topbar->id == id)
		return topbar;

	return NULL;
}

static void on_view_update_cb(UiWin *win);

static void window_switch_buf(Window *w, Buffer *b)
{
	if (w && b && w->buf != b) {
		buffer_ref_put(w->buf);

		if (buffer_proc_get(b)) {
			process_attach_win(buffer_proc_get(b), w);
		} else {
			ui_window_on_view_update_set(w->win, on_view_update_cb);
			ui_window_ops_draw_set(w->win, NULL);
			ui_window_priv_set(w->win, w);
		}

		__win_buf_attach(w, b);

		view_reload(w->view, buffer_text_get(b));
		buffer_dirty_set(b, true);
		w->prev_buf = w->buf;
		w->buf = b;
		buffer_ref_get(w->buf);
	}
}

/* External API */
int win_get_by_coord(int x, int y)
{
	Window *c = get_window_by_coord(x, y);

	if (c)
		return c->id;

	return 0;
}

bool win_is_visible(int wid)
{
	Window *c = window_get_by_id(wid);

	if (c)
		return isvisible(c) || c == minibuf || c == topbar;

	return false;
}

int win_first_get(void)
{
	if (windows)
		return windows->id;

	return 0;
}

int win_prev_get(int wid)
{
	Window *c = window_get_by_id(wid);

	if (!c)
		return 0;

	if (c->prev)
		return c->prev->id;

	return 0;
}

int win_next_get(int wid)
{
	Window *c = window_get_by_id(wid);

	if (!c)
		return 0;

	if (c->next)
		return c->next->id;

	return 0;
}

int win_upper_get(int wid)
{
	Window *c = window_get_by_id(wid);
	int w_x, w_y;
	Window *u;

	if (!c)
		return 0;

	w_x = ui_window_x_get(c->win);
	w_y = ui_window_y_get(c->win);

	/* avoid vertical separator, hence +1 in x direction */
	u = get_window_by_coord(w_x + 1, w_y - 1);
	if (u)
		return u->id;

	return 0;
}

int win_lower_get(int wid)
{
	Window *c = window_get_by_id(wid);
	int w_x, w_y;
	Window *l;

	if (!c)
		return 0;

	w_x = ui_window_x_get(c->win);
	w_y = ui_window_y_get(c->win);

	l = get_window_by_coord(w_x, w_y + ui_window_height_get(c->win));
	if (l)
		return l->id;

	return 0;
}

int win_left_get(int wid)
{
	Window *c = window_get_by_id(wid);
	int w_x, w_y;
	Window *l;

	if (!c)
		return 0;

	w_x = ui_window_x_get(c->win);
	w_y = ui_window_y_get(c->win);

	l = get_window_by_coord(w_x - 2, w_y);
	if (l)
		return l->id;

	return 0;
}

int win_right_get(int wid)
{
	Window *c = window_get_by_id(wid);
	int w_x, w_y;
	Window *r;

	if (!c)
		return 0;

	w_x = ui_window_x_get(c->win);
	w_y = ui_window_y_get(c->win);

	r = get_window_by_coord(w_x + ui_window_width_get(c->win) + 1, w_y);
	if (r)
		return r->id;

	return 0;
}

int win_current_get(void)
{
	if (current_window())
		return current_window()->id;

	return 0;
}

int win_current_set(int wid)
{
	if (!get_popup())
		__focusid(wid);
	return 0;
}

int win_prev_selected(void)
{
	if (lastsel)
		return lastsel->id;
	if (current_window())
		return current_window()->id;
	return 0;
}

int win_viewport_pos(int wid, char type)
{
	Window *w = window_get_by_id(wid);
	Filerange v;
	Text *text;

	if (!w)
		return -1;

	v = view_viewport_get(w->view);
	text = buffer_text_get(w->buf);

	switch (type) {
	case 'H':
		return text_line_start(text, v.start);
	case 'L':
		return text_line_start(text, v.end > 0 ? v.end-1 : v.end);
	}

	return -1;
}

int win_viewport_coord(int wid, int pos, int *l, int *x, int *y)
{
	Window *w = window_get_by_id(wid);
	Line *line;

	if (!w)
		return -1;

	if (view_coord_get(w->view, pos, &line, y, x)) {
		*l = line->lineno;
		return 0;
	}

	return -1;
}

int win_viewport_size_get(int wid, int *width, int *height)
{
	Window *w = window_get_by_id(wid);

	if (!w)
		return -1;

	if (width)
		*width = view_width_get(w->view);
	if (height)
		*height = view_height_get(w->view);

	return 0;
}

int win_scroll(int wid, char type, int n)
{
	Window *w = window_get_by_id(wid);

	if (!w)
		return -1;

	switch (type) {
	case 'd':
		return view_scroll_halfpage_down(w->view);
	case 'u':
		return view_scroll_halfpage_up(w->view);
	case 'f':
		return view_scroll_page_down(w->view);
	case 'b':
		return view_scroll_page_up(w->view);
	case 'L':
		return view_scroll_up(w->view, n);
	case 'l':
		return view_scroll_down(w->view, n);
	}

	return -1;
}

void win_sidebar_set(int wid, int width)
{
	Window *w = window_get_by_id(wid);

	if (w && width != ui_window_sidebar_width_get(w->win)) {
		ui_window_sidebar_width_set(w->win, width);
		buffer_dirty_set(w->buf, true);
		__draw(w, true, false);
	}
}

int win_sidebar_get(int wid)
{
	Window *w = window_get_by_id(wid);

	if (w) {
		return ui_window_sidebar_width_get(w->win);
	}

	return 0;
}

void win_sidebar_draw(int wid, int x, int y, const char *text, short fg, short bg, int attr)
{
	Window *w = window_get_by_id(wid);

	if (w) {
		ui_window_sidebar_draw(w->win, x, y, text, fg, bg, attr);
	}
}

void win_update(int wid)
{
	Window *w = window_get_by_id(wid);

	if (w) {
		ui_window_update(w->win);
	}
}

static void __style_draw(View *view, size_t start, size_t end, Style *style)
{
	Style *default_style = style_get_by_id(0);
	Style *bind_style;
	CellStyle cell_style;

	if (style->id != -1) {
		bind_style = style_get_by_id(style->id);
		if (bind_style)
			style = bind_style;
	}

	cell_style.attr = style->attr;
	cell_style.fg = style->fg;
	cell_style.bg = style->bg;

	if (cell_style.attr == 0)
		cell_style.attr = default_style->attr;
	if (cell_style.fg == -1)
		cell_style.fg = default_style->fg;
	if (cell_style.bg == -1)
		cell_style.bg = default_style->bg;

	view_style(view, cell_style, start, end);
}

static int style_prop_draw(Buffer *buf, int id, size_t start, size_t end, void *data,
		void *arg)
{
	Style *style = data;
	View *view = arg;

	__style_draw(view, start, end, style);

	return 0;
}

static int style_syntax_draw(SyntaxParser *parser, int id, size_t start, size_t end, void *data,
		void *arg)
{
	Style *style = data;
	View *view = arg;

	__style_draw(view, start, end, style);

	return 0;
}

static void on_view_update_cb(UiWin *win)
{
	Window *w = ui_window_priv_get(win);
	Filerange v = view_viewport_get(w->view);
	char *eof_sym = "~";
	size_t eof_len = strlen(eof_sym);
	Style *default_style = style_get_by_id(0);

	buffer_syntax_rules_walk(w->buf, SYNTAX_RULE_TYPE_STYLE,
			v.start, v.end, w->view, style_syntax_draw);
	buffer_properties_walk(w->buf, PROPERTY_TYPE_TEXT_STYLE,
			v.start, v.end, w->view, style_prop_draw);
	buffer_properties_walk(w->buf, PROPERTY_TYPE_TEXT_HIGHLIGHT,
			v.start, v.end, w->view, style_prop_draw);

	if (w->highlight_mark) {
		size_t start = buffer_mark_get(w->buf);
		size_t end = buffer_cursor_get(w->buf);
		Style *highlight_style = style_get_by_id(1);
		CellStyle cell_style = {
			.attr = highlight_style->attr,
			.fg = highlight_style->fg,
			.bg = highlight_style->bg,
		};

		view_style(w->view, cell_style, MIN(start, end), MAX(start, end));
	}

	for (Line *l = view_lines_last(w->view)->next; l; l = l->next) {
		l->cells[0].style.fg = default_style->fg;
		l->cells[0].style.bg = default_style->bg;
		strncpy(l->cells[0].data, eof_sym, eof_len);
		l->cells[0].len = eof_len;
	}
}

int win_new(int bid)
{
	Window *c = calloc(1, sizeof(Window));

	if (!c)
		return -1;

	c->tags = tagset[seltags];
	c->id = ++cmdfifo.id;

	if (bid) {
		c->buf = buffer_by_id(bid);
		buffer_dirty_set(c->buf, true);
		buffer_ref_get(c->buf);
	} else {
		c->buf = __buf_new("", global_kmap);
	}

	if (!c->buf) {
		free(c);
		return -1;
	}

	c->view = view_new(buffer_text_get(c->buf));
	if (!c->view) {
		__buf_del(c->buf);
		free(c);
		return -1;
	}

	c->win = ui_window_new(ui, c->view);
	if (!c->win) {
		view_free(c->view);
		__buf_del(c->buf);
		free(c);
		return -1;
	}

	if (buffer_proc_get(c->buf)) {
		process_attach_win(buffer_proc_get(c->buf), c);
	} else {
		ui_window_priv_set(c->win, c);
		ui_window_on_view_update_set(c->win, on_view_update_cb);
	}
	__win_buf_attach(c, c->buf);

	ui_window_has_title_set(c->win, true);
	ui_window_resize(c->win, waw, wah);
	ui_window_move(c->win, wax, way);

	attach(c);
	focus(c);
	arrange();

	return c->id;
}

void win_del(int wid)
{
	Window *c = window_get_by_id(wid);

	if (c) {
		destroy(c);
	}
}

void win_close(int wid)
{
	Window *w = window_get_by_id(wid);

	if (w) {
		Buffer *buf = w->buf;
		__win_del(w);
		buffer_ref_put(buf);
	}
}

char *win_title_get(int wid)
{
	Window *c = window_get_by_id(wid);

	if (c)
		return window_get_title(c);

	return NULL;
}

int win_title_set(int wid, char *title)
{
	Window *c = window_get_by_id(wid);

	if (c) {
		ui_window_title_set(c->win, title);
		settitle(c);
		if (!isarrange(fullscreen))
			draw_title(c);
		return 0;
	}

	return -1;
}

static int tag_to_bit(int tag)
{
	if (!tag)
		return ~0;

	tag--;
	return TAGMASK & ((tag < LENGTH(tags)) ? (1 << tag) : 0);
}

int win_tag_set(int wid, int tag)
{
	unsigned int ntags = tag_to_bit(tag);
	Window *c;

	if (!ntags)
		return -1;

	c = window_get_by_id(wid);
	if (!c)
		return -1;

	c->tags = ntags;
	tagschanged();
	return 0;
}

int win_tag_bits(int wid)
{
	Window *c;

	c = window_get_by_id(wid);
	if (!c)
		return 0;

	return c->tags;
}

int win_tag_toggle(int wid, int tag)
{
	unsigned int ntags;
	Window *c;

	c = window_get_by_id(wid);
	if (!c)
		return -1;

	ntags = c->tags ^ tag_to_bit(tag);
	if (ntags) {
		c->tags = ntags;
		tagschanged();
	}
}

int win_tag_add(int wid, int tag)
{
	unsigned int ntags;
	Window *c;

	c = window_get_by_id(wid);
	if (!c)
		return -1;

	ntags = c->tags | tag_to_bit(tag);
	c->tags = ntags;
	tagschanged();
	return 0;
}

int win_tag_del(int wid, int tag)
{
	unsigned int ntags;
	Window *c;

	c = window_get_by_id(wid);
	if (!c)
		return -1;

	ntags = c->tags & ~tag_to_bit(tag);
	c->tags = ntags;
	tagschanged();
	return 0;
}

win_state_t win_state_get(int wid)
{
	Window *c = window_get_by_id(wid);

	if (!c)
		return -1;

	if (c->minimized) {
		return WIN_STATE_MINIMIZED;
	} else if (isarrange(fullscreen)) {
		return WIN_STATE_MAXIMIZED;
	} else if (ismaster(c)) {
		return WIN_STATE_MASTER;
	}

	return -1;
}

int win_state_set(int wid, win_state_t st)
{
	const char *maxi[] = { "[ ]" };
	Window *c, *orig;

	if (get_popup())
		return -1;

	c = window_get_by_id(wid);
	if (!c)
		return -1;

	orig = current_window();

	switch (st) {
	case WIN_STATE_MINIMIZED:
		if (!c->minimized) {
			win_current_set(wid);
			toggleminimize();
			/* switch to the original window */
			if (orig != c)
				win_current_set(orig->id);
		}
		break;

	case WIN_STATE_MAXIMIZED:
		win_current_set(wid);
		setlayout(maxi);
		break;

	case WIN_STATE_MASTER:
		win_current_set(wid);
		detach(c);
		attachfirst(c);
		focus(c);
		if (c->minimized)
			toggleminimize();
		arrange();
		/* switch to the original window */
		if (orig)
			win_current_set(orig->id);
		break;

        default: return -1;
	}

	return 0;
}

int win_state_toggle(int wid, win_state_t st)
{
	const char *maxi[] = { "[ ]" };
	Window *c, *orig;

	c = window_get_by_id(wid);
	if (!c)
		return -1;

	orig = current_window();

	switch (st) {
	case WIN_STATE_MINIMIZED:
		win_current_set(wid);
		toggleminimize();
		/* switch to the original window */
		if (orig)
			win_current_set(orig->id);
		break;

	case WIN_STATE_MAXIMIZED:
		if (isarrange(fullscreen)) {
			layout = pertag.layout_prev[pertag.curtag];
			pertag.layout[pertag.curtag] = layout;
			arrange();
		} else {
			setlayout(maxi);
		}
		break;

        default: return -1;
	}

	return 0;
}

int win_buf_get(int wid)
{
	Window *c = window_get_by_id(wid);

	if (!c || !c->buf)
		return -1;

	return buffer_id_get(c->buf);
}

void win_mark_highlight(int wid, bool enable)
{
	Window *c = window_get_by_id(wid);

	if (c) {
		if (enable != c->highlight_mark)
			buffer_dirty_set(c->buf, true);
		c->highlight_mark = enable;
	}
}

void win_popup(int wid, bool enable)
{
	Window *w = window_get_by_id(wid);
	int x, y;

	if (w) {
		/* TODO: add support for term window */
		if (buffer_proc_get(w->buf))
			return;

		if (enable) {
			int pw = waw/3;
			int ph = wah/3;

			detach(w);

			if (get_popup())
				attach(get_popup());

			ui_window_border_enable(w->win, true);
			resize(w, waw-(waw-pw), wah-(wah-ph), pw, ph);
			set_popup(w);
			arrange();
			focus(w);
		} else {
			set_popup(NULL);
			ui_window_border_enable(w->win, false);
			attach(w);
			arrange();
		}

		ui_window_cursor_get(w->win, &x, &y);
		ui_window_cursor_set(w->win, x+1, y+1);
		ui_window_refresh(w->win);
	}
}

void win_size_set(int wid, int width, int height)
{
	Window *w = window_get_by_id(wid);

	if (w) {
		bool is_changed = false;

		if (width > 0 && width != ui_window_width_get(w->win)) {
			ui_window_width_set(w->win, width);
			is_changed = true;
		}
		if (height > 0 && height != ui_window_height_get(w->win)) {
			ui_window_height_set(w->win, height);
			is_changed = true;
		}

		if (!is_changed)
			return;

		if (w == minibuf || w == topbar) {
			update_screen_size();
			buffer_dirty_set(w->buf, true);
			draw(w, true);
			arrange();
		} else {
			redraw(NULL);
		}
	}
}

int win_size_get(int wid, int *width, int *height)
{
	Window *w = window_get_by_id(wid);

	if (w) {
		if (width)
			*width = ui_window_width_get(w->win);
		if (height)
			*height = ui_window_height_get(w->win);
		return 0;
	}

	return -1;
}

void win_border_set(int wid, bool enable)
{
	Window *w = window_get_by_id(wid);

	if (w) {
		ui_window_border_enable(w->win, enable);
	}
}

void win_buf_switch(int wid, int bid)
{
	Window *w = window_get_by_id(wid);
	Buffer *b = buffer_by_id(bid);

	window_switch_buf(w, b);
}

Style *style_new(void)
{
	Style *style;

	style = calloc(1, sizeof(*style));
	if (!style)
		return NULL;

	style->id = -1;
	style->fg = -1;
	style->bg = -1;

	return style;
}

int style_add(Style *style)
{
	if (style->name && style->name[0] && style_get_by_name(style->name)) {
		return -1;
	}

	if (array_add(&style_array, style)) {
		int id = array_length(&style_array) - 1;
		Style *added = array_get(&style_array, id);
		
		added->id = id;
		return id;
	} else {
		return -1;
	}
}

int style_update(int id, Style *update)
{
	Style *style;

	style = style_get_by_id(id);
	if (!style) {
		return -1;
	}

	*style = *update;
	style->id = id;

	return 0;
}

Style *style_get_by_id(int id)
{
	return array_get(&style_array, id);
}

Style *style_get_by_name(const char *name)
{
	if (!name || !name[0])
		return NULL;

	for (int i = 0; i < array_length(&style_array); i++) {
		Style *style = array_get(&style_array, i);

		if (strcmp(style->name, name) == 0)
			return style;
	}

	return NULL;
}

int kmap_add(int pid)
{
	KeyMap *pmap = keymap_by_id(pid);
	KeyMap *kmap;

	kmap = keymap_new(pmap);
	if (kmap) {
		keymap_ref_get(kmap);
		return keymap_id_get(kmap);
	}

	return -1;
}

int kmap_parent_set(int kid, char *name, int pid)
{
	KeyMap *kmap = keymap_by_id(kid);

	if (kmap) {
		keymap_parent_name_set(kmap, name);

		if (pid > 0) {
			KeyMap *parent = keymap_by_id(pid);
			if (parent) {
				keymap_parent_set(kmap, parent);
			}
		}
		return 0;
	}

	return -1;
}

int kmap_parent_get(int kid)
{
	KeyMap *kmap = keymap_by_id(kid);

	if (kmap) {
		KeyMap *parent = keymap_parent_get(kmap);

		if (parent)
			return keymap_id_get(parent);
	}

	return -1;
}

void kmap_del(int kid)
{
	KeyMap *kmap = keymap_by_id(kid);

	if (kmap) {
		keymap_ref_put(kmap);
		keymap_free(kmap);
	}
}

int buf_new(char *name)
{
	Buffer *buf = __buf_new(name, global_kmap);

	if (buf)
		return buffer_id_get(buf);
	return 0;
}

bool buf_is_valid(int bid)
{
	return !!buffer_by_id(bid);
}

void buf_del(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		__buf_del(buf);
	}
}

int buf_kmap_set(int bid, char *name)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_keymap_set(buf, name);
		return 0;
	}

	return -1;
}

int buf_kmap_get(int bid)
{
	Buffer *buf = buffer_by_id(bid);
	KeyMap *kmap;

	if (buf)
		kmap = buffer_keymap_get(buf);
	if (kmap)
		return keymap_id_get(kmap);

	return -1;
}

int buf_current_get(void)
{
	if (current_window())
		return buffer_id_get(current_window()->buf);

	return 0;
}

int buf_first_get(void)
{
	Buffer *buf = buffer_first_get();

	if (buf)
		return buffer_id_get(buf);

	return 0;
}

int buf_next_get(int bid)
{
	Buffer *buf = buffer_by_id(bid);
	Buffer *next;

	if (buf) {
		next = buffer_next_get(buf);
		if (next)
			return buffer_id_get(next);
	}

	return 0;
}

void buf_name_set(int bid, const char *name)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_name_lock(buf, true);
		buffer_name_set(buf, name);

		for (Window *c = windows; c; c = c->next) {
			if (isvisible(c) && c->buf == buf)
				draw_title(c);
		}
	}
}

char *buf_name_get(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf)
		return buffer_name_get(buf);

	return NULL;
}

void buf_readonly_set(int bid, bool is_readonly)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_readonly_set(buf, is_readonly);

		for (Window *w = windows; w; w = w->next) {
			if (isvisible(w) && w->buf == buf)
				draw_title(w);
		}
	}
}

bool buf_is_readonly(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf)
		return buffer_is_readonly(buf);

	return false;
}

int buf_by_name(const char *name)
{
	Buffer *buf = buffer_by_name(name);

	if (buf)
		return buffer_id_get(buf);

	return 0;
}

static void buf_update(Window *w)
{
	View *view = w->view;
	Buffer *buf = w->buf;
	UiWin *win = w->win;

	if (buffer_is_dirty(buf)) {
		size_t pos = buffer_cursor_get(buf);
		int x, y;

		view_invalidate(view);

		if (w == current_window() || w == minibuf || w == topbar) {
			void (*scroll_fn)(View *, size_t) = view_scroll_to;
			Filerange r = view_viewport_get(view);
			Text *text = buffer_text_get(buf);

			if (pos < r.start || pos > r.end) {
				size_t lines;
				size_t start;
				size_t end;

				if (pos < r.start) {
					start = pos;
					end = r.start;
				} else if (pos > r.end) {
					start = r.end;
					end = pos;
				}

				lines = text_lines_count(text, start, end - start);
				if (lines > (view_height_get(view) / 2))
					scroll_fn = view_cursor_to;
			}

			scroll_fn(view, pos);

			if (view_coord_get(view, pos, NULL, &y, &x)) {
				if (w == current_window())
					ui_window_cursor_set(win, x, y);
			}

			/* TODO: better to make buffer to know about it's
			 * windows and mark them as dirty on text update */
			buffer_dirty_set(buf, false);
		}
	}
}

static void buf_list_update(void)
{
	if (topbar)
		buf_update(topbar);

	for (Window *w = windows; w; w = w->next) {
		if (is_content_visible(w)) {
			buf_update(w);
		}
	}
	if (topbar)
		buf_update(topbar);
	if (get_popup())
		buf_update(get_popup());
	if (minibuf)
		buf_update(minibuf);
}

size_t buf_text_insert(int bid, const char *text)
{
	Buffer *buf = buffer_by_id(bid);
	size_t pos = EPOS;

	if (buf) {
		pos = buffer_text_insert(buf, buffer_cursor_get(buf), text);
	}

	return pos;
}

size_t buf_text_insert_char(int bid, char ch)
{
	Buffer *buf = buffer_by_id(bid);
	size_t pos = EPOS;

	if (buf) {
		pos = buffer_text_insert_len(buf, buffer_cursor_get(buf), &ch, 1);
	}

	return pos;
}

size_t buf_text_insert_nl(int bid, int pos)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf)
		return buffer_text_insert_nl(buf, pos);

	return EPOS;
}

size_t buf_text_insert_file(int bid, const char *path)
{
	Buffer *buf = buffer_by_id(bid);
	struct stat info;
	char data[512];
	ssize_t len;
	size_t pos;
	size_t rem;
	int fd;

	if (!buf)
		return EPOS;
	if (buffer_is_readonly(buf))
		return EPOS;

	pos = buffer_cursor_get(buf);

	fd = open(path, O_RDONLY);
	if (fd == -1)
		return EPOS;

	fstat(fd, &info);
	rem = info.st_size;

	while (rem > 0) {
		len = read(fd, data, sizeof(data));
		if (len == -1) {
			close(fd);
			return EPOS;
		} else if (len == 0) {
			break;
		} else {
			pos = buffer_text_insert_len(buf, pos, data, len);
			rem -= len;
		}
	}
	close(fd);

	return pos;
}

void *text_move_fn_get(char obj, int n)
{
	size_t (*obj_move)(Text *t, size_t pos);

	switch (obj) {
	case 'c':
		if (n >= 0)
			obj_move = text_char_next;
		else
			obj_move = text_char_prev;
		break;
	case 'w':
		if (n >= 0)
			obj_move = text_word_start_next;
		else
			obj_move = text_word_start_prev;
		break;
	case 'e':
		if (n >= 0)
			obj_move = text_word_end_next;
		else
			obj_move = text_word_end_prev;
		break;
	case 'W':
		if (n >= 0)
			obj_move = text_longword_start_next;
		else
			obj_move = text_longword_start_prev;
		break;
	case 'E':
		if (n >= 0)
			obj_move = text_longword_end_next;
		else
			obj_move = text_longword_end_prev;
		break;
	case 'l':
		if (n >= 0)
			obj_move = text_line_down;
		else
			obj_move = text_line_up;
		break;
	case 'L':
		if (n >= 0)
			obj_move = text_line_next;
		else
			obj_move = text_line_prev;
		break;
	case '0':
		if (n >= 0)
			obj_move = text_line_finish;
		else
			obj_move = text_line_start;
		break;
	case '1':
		if (n >= 0)
			obj_move = text_line_end;
		else
			obj_move = text_line_begin;
		break;
	case 'p':
		if (n >= 0)
			obj_move = text_paragraph_next;
		else
			obj_move = text_paragraph_prev;
		break;
	case 's':
		if (n >= 0)
			obj_move = text_sentence_next;
		else
			obj_move = text_sentence_prev;
		break;
	case 'g':
		if (n >= 0)
			obj_move = text_begin;
		else
			obj_move = text_end;
		break;
	default:
		return NULL;
	}

	return obj_move;
}

size_t buf_text_obj_move(int bid, size_t pos, char obj, int n, bool move)
{
	size_t (*obj_move)(Text *t, size_t pos);
	Buffer *buf = buffer_by_id(bid);
	Text *txt;

	obj_move = text_move_fn_get(obj, n);

	if (obj_move && buf) {
		txt = buffer_text_get(buf);

		for (n = abs(n); n; n--)
			pos = obj_move(txt, pos);

		if (move) {
			buffer_cursor_set(buf, pos);
			/* just to make UI update */
			buffer_dirty_set(buf, true);
		}
	}

	return pos;
}

int buf_text_obj_range(int bid, size_t pos, char obj, int *start, int *end, bool inner)
{
	Filerange (*obj_range)(Text *t, size_t pos);
	Buffer *buf = buffer_by_id(bid);
	Filerange ret;
	int shift = 0;

	if (buf) {
		switch (obj) {
		case 'w':
			obj_range = inner ? text_object_word :
				text_object_word_outer;
			break;
		case 'W':
			obj_range = inner ? text_object_longword :
				text_object_longword_outer;
			break;
		case 'l':
			obj_range = inner ? text_object_line_inner :
				text_object_line;
			break;
		case '[':
			obj_range = text_object_square_bracket;
			shift = !inner;
			break;
		case '{':
			obj_range = text_object_curly_bracket;
			shift = !inner;
			break;
		case '<':
			obj_range = text_object_angle_bracket;
			shift = !inner;
			break;
		case '(':
			obj_range = text_object_parenthesis;
			shift = !inner;
			break;
		case '\"':
			obj_range = text_object_quote;
			shift = !inner;
			break;
		case '\'':
			obj_range = text_object_single_quote;
			shift = !inner;
			break;
		case '`':
			obj_range = text_object_backtick;
			shift = !inner;
			break;
		default:
			goto err;
		}

		ret = obj_range(buffer_text_get(buf), pos);
		if (ret.start == EPOS || ret.end == EPOS)
			goto err;

		ret.start -= shift;
		ret.end += shift;

		*start = ret.start;
		*end = ret.end;
		return 0;
	}

err:
	*start = *end = pos;
	return -1;
}

size_t buf_text_range_del(int bid, int start, int end)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		start = buffer_text_delete(buf, start, end);
	}

	return start;
}

char *buf_text_get(int bid, int start, int len)
{
	Buffer *buf = buffer_by_id(bid);
	char *data = NULL;

	if (buf)
		data = buffer_text_extract(buf, start, len);

	return data;
}

static void buf_text_style_update(Buffer *buf, char what)
{
	for (Window *c = windows; c; c = c->next) {
		if (c->buf == buf) {
			switch (what) {
			case 'f':
				ui_window_text_fg_set(c->win,
						buffer_text_fg_get(buf));
				break;
			case 'b':
				ui_window_text_bg_set(c->win,
						buffer_text_bg_get(buf));
				break;
			case 's':
				ui_window_text_style_set(c->win,
						buffer_text_style_get(buf));
			}
		}
	}
}

void buf_text_fg_set(int bid, short fg)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_text_fg_set(buf, fg);
		buf_text_style_update(buf, 'f');
	}
}

int buf_text_fg_get(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf)
		return buffer_text_fg_get(buf);

	return -1;
}

void buf_text_bg_set(int bid, short bg)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_text_bg_set(buf, bg);
		buf_text_style_update(buf, 'b');
	}
}

short buf_text_bg_get(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf)
		return buffer_text_bg_get(buf);

	return -1;
}

void buf_text_style_set(int bid, int style)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_text_style_set(buf, style);
		buf_text_style_update(buf, 's');
	}
}

int buf_text_style_get(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf)
		return buffer_text_style_get(buf);

	return -1;
}

size_t buf_cursor_get(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf)
		return buffer_cursor_get(buf);

	return EPOS;
}

void buf_cursor_set(int bid, size_t pos)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_cursor_set(buf, pos);
		/* just to make UI update */
		buffer_dirty_set(buf, true);
	}
}

size_t buf_line_num(int bid, size_t pos)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		return buffer_line_num(buf, pos);
	}

	return EPOS;
}

void buf_input_enable(int bid, bool enable)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_text_input_enable(buf, enable);
	}
}

void buf_mode_name_set(int bid, char *name)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_mode_name_set(buf, name);
		if (current_window())
			draw_title(current_window());
	}
}

void buf_state_name_set(int bid, char *name)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_state_name_set(buf, name);
		if (current_window())
			draw_title(current_window());
	}
}

int buf_file_open(int bid, const char *file)
{
	Buffer *buf = buffer_by_id(bid);
	int err;

	if (buf) {
		err = buffer_file_open(buf, file);
		if (err)
			return -1;

		/* update view with new text */
		for (Window *c = windows; c; c = c->next) {
			if (buf == c->buf) {
				Text *txt = buffer_text_get(buf);

				if (view_text(c->view) != txt)
					view_reload(c->view, txt);
			}
		}
	}
}

void buf_file_set(int bid, const char *file)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_filename_set(buf, file);
	}
}

char *buf_file_get(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		return buffer_filename_get(buf);
	}

	return NULL;
}

int buf_save(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf)
		return buffer_save(buf);
}

void buf_mark_set(int bid, size_t pos)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf)
		buffer_mark_set(buf, pos);
}

bool buf_mark_is_set(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf)
		return buffer_is_mark_set(buf);
	return false;
}

size_t buf_mark_get(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf)
		return buffer_mark_get(buf);

	return EPOS;
}

void buf_mark_clear(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf)
		buffer_mark_clear(buf);
}

bool buf_is_term(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf)
		return buffer_proc_get(buf) != NULL;

	return false;
}

bool buf_is_visible(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		for (Window *c = nextvisible(windows); c; c = nextvisible(c->next)) {
			if (c->buf == buf)
				return true;
		}
	}

	return false;
}

int buf_prop_style_add(int bid, int type, int fg, int bg, int attr, const char *style_name, int start, int end, const char *regex)
{
	Buffer *buf = buffer_by_id(bid);
	Style *style, *style_bind;
	int err;

	if (!buf)
		return -1;

	style = style_new();
	if (!style)
		return -1;

	style_bind = style_get_by_name(style_name);
	if (style_bind) {
		style->id = style_bind->id;
	} else {
		style->attr = attr;
		style->fg = fg;
		style->bg = bg;
	}

	err = buffer_property_add(buf, type, start, end, style, regex);
	if (err) {
		free(style);
		return err;
	}

	buffer_dirty_set(buf, true);
	return 0;
}


int buf_prop_kmap_add(int bid, int kid, int start, int end, const char *regex)
{
	Buffer *buf = buffer_by_id(bid);
	KeyMap *map = keymap_by_id(kid);
	int err;

	if (!map || !buf)
		return -1;

	err = buffer_property_add(buf, PROPERTY_TYPE_TEXT_KEYMAP, start, end, map, regex);
	if (err)
		return err;

	keymap_parent_set(map, buffer_keymap_get(buf));
	keymap_ref_get(map);
	return 0;
}

void buf_prop_del(int bid, int type, int start, int end, const char *regex)
{
	Buffer *buf = buffer_by_id(bid);

	buffer_property_remove_cb(buf, type, start == -1 ? EPOS : start,
			end == -1 ? EPOS : end, regex, NULL, buf_prop_del_cb);
	buffer_dirty_set(buf, true);
}

/* void buf_prop_walk(int bid, int type, int start, int end, void *arg, */
/* 			void (*cb)(int bid, int type, int start, int end, void */
/* 				*arg)); */

void *buf_env_get(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		return buffer_env_get(buf);
	}

	return NULL;
}

void buf_env_set(int bid, void *env)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_env_set(buf, env);
	}
}

void buf_snapshot(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_snapshot(buf);
	}
}

void buf_undo(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_undo(buf);
	}
}

void buf_redo(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_redo(buf);
	}
}

size_t buf_search_regex(int bid, size_t pos, const char *pattern, int dir)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		return buffer_search_regex(buf, pos, pattern, dir);
	}

	return EPOS;
}

int buf_parser_set(int bid, const char *lang)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		return buffer_parser_set(buf, lang);
	}

	return -1;
}

int stx_lang_style_add(const char *lang, int fg, int bg, int attr, const char *style_name, const char *rule)
{
	Style *style, *style_bind;
	int err;

	style = style_new();
	if (!style)
		return -1;

	style_bind = style_get_by_name(style_name);
	if (style_bind) {
		style->id = style_bind->id;
	} else {
		style->attr = attr;
		style->fg = fg;
		style->bg = bg;
	}

	err = syntax_lang_rule_add(lang, SYNTAX_RULE_TYPE_STYLE, rule, style);
	if (err)
		return err;

	return 0;
}

void stx_lang_style_del(const char *lang, const char *rule)
{
	syntax_lang_rule_remove(lang, SYNTAX_RULE_TYPE_STYLE, rule);
}

void stx_lang_style_clear(const char *lang)
{
	syntax_lang_rules_clear(lang, SYNTAX_RULE_TYPE_STYLE);
}

static Window *widget_create(const char *name, int x, int y, int width, int height)
{
	Window *w;

	w = calloc(1, sizeof(Window));
	if (!w)
		return NULL;

	/* c->tags = tagset[seltags]; */
	w->id = ++cmdfifo.id;

	w->buf = __buf_new(name, NULL);
	if (!w->buf) {
		free(w);
		return NULL;
	}

	w->view = view_new(buffer_text_get(w->buf));
	if (!w->view) {
		__buf_del(w->buf);
		free(w);
		return NULL;
	}

	w->win = ui_window_new(ui, w->view);
	if (!w->win) {
		view_free(w->view);
		__buf_del(w->buf);
		free(w);
		return NULL;
	}

	ui_window_on_view_update_set(w->win, on_view_update_cb);
	ui_window_resize(w->win, width, height);
	ui_window_priv_set(w->win, w);
	ui_window_move(w->win, x, y);
	ui_window_draw(w->win);

	return w;
}

int minibuf_create(void)
{
	minibuf = widget_create("*minibuf*", 0, ui_height_get(ui)-1, waw, 1);
	update_screen_size();
	return minibuf->id;
}

int topbar_create(void)
{
	topbar = widget_create("*topbar*", 0, 0, waw, 1);
	update_screen_size();
	return topbar->id;
}

int term_keys_send(int bid, char *keys)
{
	Buffer *buf = buffer_by_id(bid);

	if (!buf)
		return -1;

	if (buffer_proc_get(buf)) {
		Process *proc = buffer_proc_get(buf);

		vt_write(process_term_get(proc), keys, strlen(keys));
	}
	return 0;
}

int term_text_send(int bid, char *text)
{
	Buffer *buf = buffer_by_id(bid);

	if (!buf)
		return -1;

	if (buffer_proc_get(buf)) {
		Process *proc = buffer_proc_get(buf);

		vt_write(process_term_get(proc), text, strlen(text));
	}
	return 0;
}

int term_text_get(int bid, char **buf, size_t *len)
{
	Buffer *b = buffer_by_id(bid);

	if (b && buffer_proc_get(b)) {
		Process *proc = buffer_proc_get(b);
		*len = vt_content_get(process_term_get(proc), buf, false);
		return 0;
	} else {
		return -1;
	}
}

int term_current_line_get(int bid, char **buf, size_t *len)
{
	Buffer *b = buffer_by_id(bid);

	if (b && buffer_proc_get(b)) {
		Process *proc = buffer_proc_get(b);
		*len = vt_current_line_get(process_term_get(proc), buf);
		return 0;
	} else {
		return -1;
	}
}

int term_filter_enable(int bid, bool enable)
{
	Buffer *b = buffer_by_id(bid);

	if (b && buffer_proc_get(b)) {
		Process *proc = buffer_proc_get(b);
		Vt *vt = process_term_get(proc);

		if (enable) {
			vt_filter_set(vt, vt_filter, b);
		} else {
			vt_filter_set(vt, NULL, NULL);
		} 

		return 0;
	}

	return -1;
}

int view_current_get(void)
{
	return pertag.curtag;
}

int view_current_set(int tag)
{
	int i;

	unsigned int newtagset = tag_to_bit(tag);
	if (tagset[seltags] != newtagset && newtagset) {
		seltags ^= 1; /* toggle current_window() tagset */
		pertag.prevtag = pertag.curtag;
		pertag.curtag = tag;
		setpertag();
		tagset[seltags] = newtagset;
		tagschanged();
	}
}

const char *view_name_get(int tag)
{
	if (pertag.name[tag] && strlen(pertag.name[tag])) {
		return pertag.name[tag];
	} else {
		return tags[tag-1];
	}
}

int view_name_set(int tag, char *name)
{
	free(pertag.name[tag]);
	pertag.name[tag] = NULL;

	if (name && strlen(name))
		pertag.name[tag] = strdup(name);
	drawbar();
}

char *view_cwd_get(int tag)
{
	return pertag.cwd[tag];
}

int view_cwd_set(int tag, char *cwd)
{
	strncpy(pertag.cwd[tag], cwd, CWD_MAX - 1);
	drawbar();
	return 0;
}

layout_t layout_current_get(int tag)
{
	if (pertag.layout[tag]->arrange == fullscreen) {
		return LAYOUT_MAXIMIZED;
	} else if (pertag.layout[tag]->arrange == tile) {
		return LAYOUT_TILED;
	} else if (pertag.layout[tag]->arrange == bstack) {
		return LAYOUT_BSTACK;
	} else if (pertag.layout[tag]->arrange == grid) {
		return LAYOUT_GRID;
	} else {
		return -1;
	}
}

int layout_current_set(int tag, layout_t lay)
{
	if (get_popup())
		return -1;

	layout = &layouts[lay];
	pertag.layout_prev[pertag.curtag] = pertag.layout[pertag.curtag];
	pertag.layout[pertag.curtag] = layout;
	arrange();
}

int layout_nmaster_get(int tag)
{
	return pertag.nmaster[tag];
}

int layout_nmaster_set(int tag, int n)
{
	if (get_popup() || isarrange(fullscreen) || isarrange(grid))
		return -1;

	pertag.nmaster[tag] = n;
	arrange();

	return 0;
}

float layout_fmaster_get(int tag)
{
	return pertag.mfact[tag];
}

int layout_fmaster_set(int tag, float f)
{
	if (get_popup() || isarrange(fullscreen) || isarrange(grid))
		return -1;

	pertag.mfact[tag] = f;
	arrange();

	return 0;
}

bool layout_sticky_get(int tag)
{
	return pertag.msticky[tag];
}

int layout_sticky_set(int tag, bool is_sticky)
{
	if (get_popup())
		return -1;

	pertag.msticky[tag] = is_sticky;
	draw_all();
	return 0;
}

int bind_key(char *key, void (*act)(void), int kid, char *tname)
{
	KeyMap *kmap = global_kmap;

	if (kid) {
		kmap = keymap_by_id(kid);
		if (!kmap)
			return -1;
	}

	if (kmap)
		return keymap_bind(kmap, key, act, tname);

	return -1;
}

int unbind_key(char *key, int kid)
{
	KeyMap *kmap = global_kmap;

	if (kid) {
		kmap = keymap_by_id(kid);
		if (!kmap)
			return -1;
	}

	if (kmap)
		return keymap_unbind(kmap, key);

	return -1;
}

int fifo_create(void)
{
	char *cmd, *ret, *sta;
	char rundir[128];
	struct stat st;
	char tmp[128];
	pid_t pid;

	snprintf(rundir, sizeof(rundir), "/run/user/%d/"PROGNAME, getuid());

	if (stat(rundir, &st)) {
		if (mkdir(rundir, 0777)) {
			return -1;
		}
	}

	pid = getpid();

	snprintf(tmp, sizeof(tmp), "%s/"PROGNAME"-cmd-fifo-%d", rundir, pid);
	cmdfifo.fd = open_or_create_fifo(tmp, &cmdfifo.file);
	if (!(cmd = realpath(tmp, NULL)))
		return -1;
	cmdfifo.file = strdup(cmd);

	snprintf(tmp, sizeof(tmp), "%s/"PROGNAME"-ret-fifo-%d", rundir, pid);
	retfifo.fd = __open_or_create_fifo(tmp, &retfifo.file, O_RDWR);
	if (!(ret = realpath(tmp, NULL))) {
		close(cmdfifo.fd);
		unlink(cmdfifo.file);
		return -1;
	}
	retfifo.file = strdup(ret);

	setenv("BORSCH_CMD_FIFO", cmd, 1);
	setenv("BORSCH_RET_FIFO", ret, 1);

	return 0;
}

int evt_fd_handler_add(int fd, void (*fn)(int fd, void *), void *arg)
{
	return event_fd_handler_register(fd, fn, arg);
}

void evt_fd_handler_del(int fd)
{
	event_fd_handler_unregister(fd);
}

pid_t proc_create(const char *prog, const char *cwd, int *in, int *out, int *err, const char **env, bool async)
{
	Process *proc;

	proc = process_create(prog, cwd, in, out, err, env, false);
	if (!proc)
		return -1;	

	return process_pid_get(proc);
}

bool proc_is_alive(pid_t pid)
{
	int status;
	pid_t ret;

	ret = waitpid(pid, &status, WNOHANG);
	if (ret != pid) {
		return false;
	} else {
		return true;
	}
}

int proc_status_get(pid_t pid)
{
	Process *proc = process_by_pid(pid);

	if (proc) {
		return process_status_get(proc);
	}
	return -1;
}

void proc_del(pid_t pid)
{
	Process *proc = process_by_pid(pid);

	if (proc) {
		process_destroy(proc);
	}
}

void proc_kill(pid_t pid)
{
	Process *proc = process_by_pid(pid);

	if (proc) {
		process_kill_async(proc);
	}
}

int proc_wait(pid_t pid, int *status)
{
	Process *proc = process_by_pid(pid);

	if (!proc)
		return -1;

	if (proc->is_died) {
		*status = proc->status;
		return 0;
	}

	pid = waitpid(proc->pid, status, 0);
	if (pid == proc->pid && WIFEXITED(*status)) {
		*status = WEXITSTATUS(*status);
		return 0;
	}

	return -1;
}

void do_quit(void)
{
	quit(NULL);
}
