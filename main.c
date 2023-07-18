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
#include <limits.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <fcntl.h>
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

/* commands for use by keybindings */
static void quit(const char *args[]);
static void doeval(const char *args[]);

static KeyMap *global_kmap;
static KeyMap *curr_kmap;

static Fifo cmdfifo = { .fd = -1 };
static Fifo retfifo = { .fd = -1 };

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
		/*buffer_dirty_set(c->buf, true);*/
}

static void
term_urgent_handler(Vt *term) {
	/*
	Window *c = (Window *)vt_data_get(term);
	c->urgent = true;
	printf("\a");
	fflush(stdout);
	if (!layout_is_arrange(LAYOUT_MAXIMIZED) && window_current() != c)
		buffer_dirty_set(c->buf, true);
	*/
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

static void keypress(int code)
{
	Window *c = window_current();

	if (!c)
		return;

	c->urgent = false;

	if (buffer_proc_get(c->buf)) {
		Vt *term = process_term_get(buffer_proc_get(c->buf));

		vt_keypress(term, code);
	} else if (buffer_text_input_is_enabled(c->buf)) {
		event_t evt = {};

		evt.eid = EVT_TEXT_INSERT;
		evt.oid = code;
		scheme_event_handle(evt);
	}
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

static void handle_cmdfifo(int fd, void *arg);

void setup_ui(int ui_type)
{
	struct sigaction sa;
	sigset_t blockset;

	if (ui_type == 0)
		g_ui = ui_term_new();
	else
		g_ui = ui_x_new();

	g_ui->get_default_cell_style = get_default_cell_style;
	ui_event_handler_set(g_ui, handle_ui_event);
	ui_init(g_ui);
	window_init(g_ui);
	
	memset(&sa, 0, sizeof sa);
	sa.sa_flags = 0;
	sigemptyset(&sa.sa_mask);
	sa.sa_handler = SIG_IGN;
	sigaction(SIGPIPE, &sa, NULL);

	sigemptyset(&blockset);
	sigaddset(&blockset, SIGWINCH);
	sigprocmask(SIG_BLOCK, &blockset, NULL);

	if (cmdfifo.fd != -1)
		event_fd_handler_register(cmdfifo.fd, handle_cmdfifo, NULL);
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
	if (g_ui)
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

static void
quit(const char *args[]) {
	cleanup();
	exit(EXIT_SUCCESS);
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

int main(int argc, char *argv[]) {
	return scheme_init(argc, argv);
}

/* External API */
int runtime_init(void)
{
	if (!getenv("ESCDELAY"))
		set_escdelay(100);

	setlocale(LC_CTYPE, "");

	init_default_keymap();

	process_init();
	syntax_init();
	style_init();
	vt_init();
}

void runtime_cleanup(void)
{
	cleanup();
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

size_t buf_line_num(int bid, size_t pos)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		return buffer_line_num(buf, pos);
	}

	return EPOS;
}

int buf_file_open(int bid, const char *file)
{
	Buffer *buf = buffer_by_id(bid);
	int err;

	if (buf) {
		err = buffer_file_open(buf, file);
		if (err) {
			return -1;
		}

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

int buf_prop_style_add(int bid, int type, int fg, int bg, int attr, int is_set, const char *style_name, int start, int end,
		       const char *regex, char *name, bool expand, wchar_t ch)
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
		style->ch = ch;
		style->is_set = is_set;
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
