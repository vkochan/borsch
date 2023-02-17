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
#include <stdint.h>
#include <wchar.h>
#include <limits.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <sys/wait.h>
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

#include "common.h"
#include "array.h"
#include "event.h"
#include "buffer.h"
#include "process.h"
#include "keymap.h"
#include "window.h"
#include "style.h"
#include "view.h"
#include "syntax.h"
#include "text/text-motions.h"
#include "text/text-objects.h"
#include "xstr.h"
#include "api.h"
#include "vt.h"

#ifdef PDCURSES
int ESCDELAY;
#endif

#ifndef NCURSES_REENTRANT
# define set_escdelay(d) (ESCDELAY = (d))
#endif

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

typedef struct {
	int fd;
	const char *file;
	unsigned short int id;
} Fifo;

Ui *g_ui;

static char *scheme_init_script = "";
static bool start_in_graphic = false;

/* commands for use by keybindings */
static void quit(const char *args[]);
static void setlayout(const char *args[]);
static void togglemouse(const char *args[]);
static void doeval(const char *args[]);

/* commands for use by mouse bindings */
static void mouse_focus(const char *args[]);
static void mouse_fullscreen(const char *args[]);
static void mouse_zoom(const char *args[]);

static KeyMap *global_kmap;
static KeyMap *curr_kmap;

#ifdef NCURSES_MOUSE_VERSION
# define CONFIG_MOUSE /* compile in mouse support if we build against ncurses */
#endif

#define ENABLE_MOUSE true /* whether to enable mouse events by default */

#ifdef CONFIG_MOUSE
static Button buttons[] = {
	{ BUTTON1_CLICKED,        { mouse_focus,      { NULL  } } },
	{ BUTTON1_DOUBLE_CLICKED, { mouse_fullscreen, { "[ ]" } } },
	{ BUTTON2_CLICKED,        { mouse_zoom,       { NULL  } } },
};
#endif /* CONFIG_MOUSE */

/* global variables */
static Window *msel = NULL;
static bool mouse_events_enabled = ENABLE_MOUSE;

static Fifo cmdfifo = { .fd = -1 };
static Fifo retfifo = { .fd = -1 };

static volatile sig_atomic_t running = true;

static Cmd commands[] = {
	{ "eval", { doeval, { NULL } } },
};

static void
term_title_handler(Vt *term, const char *title) {
	/* Window *c = (Window *)vt_data_get(term); */
	/* if (title) */
	/* 	strncpy(c->title, title, sizeof(c->title) - 1); */
	/* c->title[title ? sizeof(c->title) - 1 : 0] = '\0'; */
	/* if (!layout_is_arrange(LAYOUT_MAXIMIZED)) */
	/* 	window_draw_title(c); */
}

static void
term_urgent_handler(Vt *term) {
	Window *c = (Window *)vt_data_get(term);
	c->urgent = true;
	printf("\a");
	fflush(stdout);
	if (!layout_is_arrange(LAYOUT_MAXIMIZED) && window_current() != c)
		window_draw_title(c);
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

static void
sigterm_handler(int sig) {
	running = false;
}

static void keypress(int code)
{
	Window *c = window_current();
	char buf[8] = { '\e' };

	if (window_is_visible(c)) {
		c->urgent = false;

		if (buffer_proc_get(c->buf)) {
			Vt *term = process_term_get(buffer_proc_get(c->buf));

			if (code == '\e')
				vt_write(term, buf, 1);
			else
				vt_keypress(term, code);
		} else if (buffer_text_input_is_enabled(c->buf)) {
			event_t evt = {};

			evt.eid = EVT_TEXT_INSERT;
			evt.oid = code;
			scheme_event_handle(evt);
		}
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

static void init_default_keymap(void)
{
	global_kmap = keymap_new(NULL);
	curr_kmap = global_kmap;
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

static void handle_keypress(KeyCode *key);

static int handle_ui_event(Ui *ui, enum UiEventType type, void *evt, void *arg)
{
	switch (type)
	{
	case UiEventType_KeyPress:
		handle_keypress(evt);
		break;
	}

	return 0;
}

static void setup_ui(void)
{
	struct sigaction sa;

	if (start_in_graphic)
		g_ui = ui_x_new();
	else
		g_ui = ui_term_new();

	g_ui->get_default_cell_style = get_default_cell_style;
	ui_event_handler_set(g_ui, handle_ui_event);
	ui_init(g_ui);
	init_default_keymap();
	mouse_setup();
	window_init(g_ui);
	
	window_draw_all(true);

	memset(&sa, 0, sizeof sa);
	sa.sa_flags = 0;
	sigemptyset(&sa.sa_mask);
	sa.sa_handler = sigterm_handler;
	sigaction(SIGTERM, &sa, NULL);
	sa.sa_handler = SIG_IGN;
	sigaction(SIGPIPE, &sa, NULL);
}

static void setup(void)
{
	setlocale(LC_CTYPE, "");

	process_init();
	syntax_init();
	style_init();
	vt_init();

	setup_ui();

	scheme_init(scheme_init_script);
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

static void
cleanup(void) {
	Buffer *b;
	int i;

	if (cmdfifo.fd != -1)
		event_fd_handler_unregister(cmdfifo.fd);

	scheme_uninit();

	window_cleanup();

	process_cleanup();

	b = buffer_first_get();
	while (b) {
		Buffer *nextb = buffer_next_get(b);
		buffer_del(b);
		b = nextb;
	}

	keymap_free(global_kmap);
	vt_shutdown();
	syntax_cleanup();
	style_cleanup();
	ui_free(g_ui);
	if (cmdfifo.fd > 0)
		close(cmdfifo.fd);
	if (cmdfifo.file)
		unlink(cmdfifo.file);
	if (retfifo.fd > 0)
		close(retfifo.fd);
	if (retfifo.file)
		unlink(retfifo.file);
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

static void
quit(const char *args[]) {
	cleanup();
	exit(EXIT_SUCCESS);
}

static void
setlayout(const char *args[]) {
	Layout *layout = frame_current()->layout;
	unsigned int i;

	if (!args || !args[0]) {
		if (++layout == layout_get(LAYOUT_MAX))
			layout = layout_get(LAYOUT_FIRST);
	} else {
		for (i = 0; i < LAYOUT_MAX; i++)
			if (!strcmp(args[0], layout_get(i)->symbol))
				break;
		if (i == LAYOUT_MAX)
			return;
		layout = layout_get(i);
	}
	frame_current()->layout_prev = frame_current()->layout;
	frame_current()->layout = layout;
	layout_changed(true);
}

static void
togglemouse(const char *args[]) {
	mouse_events_enabled = !mouse_events_enabled;
	mouse_setup();
}

/* commands for use by mouse bindings */
static void
mouse_focus(const char *args[]) {
	window_focus(msel);
}

static void
mouse_fullscreen(const char *args[]) {
	mouse_focus(NULL);
	setlayout(layout_is_arrange(LAYOUT_MAXIMIZED) ? NULL : args);
}

static void
mouse_zoom(const char *args[]) {
	window_focus(msel);
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

	msel = window_get_by_coord(event.x, event.y);
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

static void parse_args(int argc, char *argv[])
{
	if (!getenv("ESCDELAY"))
		set_escdelay(100);

	for (int arg = 1; arg < argc; arg++) {
		if (strcmp(argv[arg], "-i") == 0) {
			scheme_init_script = argv[arg+1];
			arg++;
		} else if (strcmp(argv[arg], "-n") == 0) {
			scheme_init_script = NULL;
		} else if (strcmp(argv[arg], "-g") == 0) {
			start_in_graphic = true;
		}
	}
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
			       cursor, cursor+1, NULL, &tmap, buf_keymap_prop_match);

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

static void handle_keypress(KeyCode *key)
{
	KeyBinding *kbd = NULL;
	event_t evt = {};
	int flags = key->flags;
	int code = key->code;

	if (!window_current()) {
		curr_kmap = global_kmap;
	} else if (!curr_kmap && window_current()) {
		KeyMap *map = buf_keymap_get(window_current()->buf);
		if (map)
			curr_kmap = map;
	} else if (!curr_kmap) {
		curr_kmap = global_kmap;
	};

	if (code < 0) {
		curr_kmap = NULL;
		return;
	}

	evt.eid = EVT_KEY_PRESS;
	evt.oid = code;
	scheme_event_handle(evt);

	if (!keybuf_enqueue(&kbuf, code, flags)) {
		keybuf_flush(&kbuf);
		curr_kmap = NULL;
		return;	
	}

	if ((kbd = keymap_match(curr_kmap, kbuf.keys, kbuf.key_index))) {
		if (keymap_kbd_is_map(kbd)) {
			curr_kmap = keymap_kbd_map_get(kbd);
			keybuf_clear(&kbuf);
			curr_kmap = NULL;
			return;
		}

		if (keymap_kbd_len(kbd) == kbuf.key_index) {
			debug("kbd action: enter\n");
			keymap_kbd_action(kbd);
			debug("kbd action: exit\n");
			keybuf_clear(&kbuf);
			curr_kmap = NULL;
		}
	} else {
		keybuf_flush(&kbuf);
		curr_kmap = NULL;
	}
}

void process_ui(void)
{
	sigset_t blockset;

	sigemptyset(&blockset);
	sigaddset(&blockset, SIGWINCH);
	sigprocmask(SIG_BLOCK, &blockset, NULL);

	if (cmdfifo.fd != -1)
		event_fd_handler_register(cmdfifo.fd, handle_cmdfifo, NULL);

	while (running) {
		process_destroy_dead();

		/* TODO: what to do with a died buffers ? */

		ui_event_process(g_ui);

		window_draw_all(false);
	}
}

int main(int argc, char *argv[]) {
	parse_args(argc, argv);
	setup();

	process_ui();

	cleanup();
	return 0;
}

static void window_switch_buf(Window *w, Buffer *b)
{
	if (w && b && w->buf != b) {
		window_buffer_switch(w, b);
		view_reload(w->view, buffer_text_get(b));
		buffer_dirty_set(b, true);
	}
}

/* External API */
void win_update(int wid)
{
	Window *w = window_get_by_id(wid);

	if (w) {
		ui_window_update(w->win);
	}
}

int win_new(int bid)
{
	Buffer *buf;
	Window *c;

 	if (bid) {
		buf = buffer_by_id(bid);
		buffer_dirty_set(buf, true);
 	} else {
		buf = __buf_new("", global_kmap);
 	}

	c = window_create(buf);
	if (!c && !bid) {
		buffer_del(buf);
 		return -1;
 	}

 	return c->id;
}

char *win_title_get(int wid)
{
	Window *c = window_get_by_id(wid);

	if (c)
		return window_title_get(c);

	return NULL;
}

int win_title_set(int wid, char *title)
{
	Window *c = window_get_by_id(wid);

	if (c) {
		ui_window_title_set(c->win, title);
		if (!layout_is_arrange(LAYOUT_MAXIMIZED))
			window_draw_title(c);
		return 0;
	}

	return -1;
}

win_state_t win_state_get(int wid)
{
	Window *c = window_get_by_id(wid);

	if (!c)
		return -1;

	if (layout_is_arrange(LAYOUT_MAXIMIZED)) {
		return WIN_STATE_MAXIMIZED;
	} else if (window_is_master(c)) {
		return WIN_STATE_MASTER;
	}

	return -1;
}

int win_state_set(int wid, win_state_t st)
{
	const char *maxi[] = { "[ ]" };
	Window *c, *orig;

	c = window_get_by_id(wid);
	if (!c)
		return -1;

	orig = window_current();

	switch (st) {
	case WIN_STATE_MAXIMIZED:
		window_focus(c);
		setlayout(maxi);
		break;

	case WIN_STATE_MASTER:
		window_focus(c);
		window_remove(c);
		window_insert_first(c);
		window_focus(c);
		layout_changed(true);
		/* switch to the original window */
		if (orig)
			window_focus(orig);
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

	orig = window_current();

	switch (st) {
	case WIN_STATE_MAXIMIZED:
		if (layout_is_arrange(LAYOUT_MAXIMIZED)) {
			frame_current()->layout = frame_current()->layout_prev;
			layout_changed(true);
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

		layout_changed(true);
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
		buffer_del(buf);
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
	if (window_current())
		return buffer_id_get(window_current()->buf);

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

		Window *c;
		for_each_window(c) {
			if (c->buf == buf)
				window_draw_title(c);
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

		Window *w;
		for_each_window(w) {
			if (w->buf == buf)
				window_draw_title(w);
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
	Window *c;
	for_each_window(c) {
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
		if (window_current())
			window_draw_title(window_current());
	}
}

char *buf_mode_name_get(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		return buffer_mode_name_get(buf);
	}

	return "";
}

void buf_state_name_set(int bid, char *name)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_state_name_set(buf, name);
		if (window_current())
			window_draw_title(window_current());
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
		Window *c;
		for_each_window(c) {
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

void buf_term_set(int bid, pid_t pid)
{
	Process *proc = process_by_pid(pid);
	Buffer *buf = buffer_by_id(bid);

	if (!proc || !buf)
		return;

	process_buffer_set(proc, buf);
	buffer_proc_set(buf, proc);
}

bool buf_is_visible(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		Window *c;
		for_each_window(c) {
			if (c->buf == buf)
				return true;
		}
	}

	return false;
}

int buf_prop_style_add(int bid, int type, int fg, int bg, int attr, const char *style_name, int start, int end,
		       const char *regex, char *name, bool expand)
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
		style->expand = expand;
	}

	err = buffer_property_add(buf, type, start, end, style, regex, name, NULL);
	if (err) {
		free(style);
		return err;
	}

	buffer_dirty_set(buf, true);
	return 0;
}


static void buf_prop_kmap_free(void *data)
{
	KeyMap *map = data;

	keymap_parent_set(map, NULL);
	keymap_ref_put(map);
}

int buf_prop_kmap_add(int bid, int kid, int start, int end, const char *regex, char *name)
{
	Buffer *buf = buffer_by_id(bid);
	KeyMap *map = keymap_by_id(kid);
	int err;

	if (!map || !buf)
		return -1;

	err = buffer_property_add(buf, PROPERTY_TYPE_TEXT_KEYMAP, start, end, map, regex, name, buf_prop_kmap_free);
	if (err)
		return err;

	keymap_parent_set(map, buffer_keymap_get(buf));
	keymap_ref_get(map);
	return 0;
}

int buf_prop_symbol_add(int bid, const char *symbol, int start, int end, const char *regex, char *name)
{
	Buffer *buf = buffer_by_id(bid);
	int err;

	if (!buf) {
		return -1;
	}

	err = buffer_property_add(buf, PROPERTY_TYPE_TEXT_SYMBOL, start, end, strdup(symbol), regex, name, NULL);
	if (err) {
		return err;
	}

	return 0;
}

int buf_prop_data_add(int bid, void *data, int start, int end, const char *regex, char *name, void (*free_fn)(void *data))
{
	Buffer *buf = buffer_by_id(bid);
	int err;

	if (!buf) {
		return -1;
	}

	err = buffer_property_add(buf, PROPERTY_TYPE_TEXT_DATA, start, end, data, regex, name, free_fn);
	if (err) {
		return err;
	}

	return 0;
}

void buf_prop_del(int bid, int type, int start, int end, const char *regex, char *name)
{
	Buffer *buf = buffer_by_id(bid);

	buffer_property_remove(buf, type, start == -1 ? EPOS : start,
			       end == -1 ? EPOS : end, regex, name);
	buffer_dirty_set(buf, true);
}

void buf_prop_walk(int bid, int type, int start, int end, char *name, void *arg,
		void (*cb)(Buffer *buf, int id, size_t start, size_t end, void *data,
				 void *arg))
{
	Buffer *buf = buffer_by_id(bid);

	buffer_properties_walk(buf, type, start, end, name, arg, cb);
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

void do_quit(void)
{
	quit(NULL);
}
