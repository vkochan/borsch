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
#include <stdlib.h>
#include <unistd.h>
#include <stdint.h>
#include <wchar.h>
#include <limits.h>
#include <libgen.h>
#include <sys/select.h>
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

#include "buffer.h"
#include "keymap.h"
#include "view.h"
#include "text/text-motions.h"
#include "text/text-objects.h"
#if defined __CYGWIN__ || defined __sun
# include <termios.h>
#endif
#include "api.h"
#include "vt.h"

#ifdef PDCURSES
int ESCDELAY;
#endif

#ifndef NCURSES_REENTRANT
# define set_escdelay(d) (ESCDELAY = (d))
#endif

/* scroll back buffer size in lines */
#define SCROLL_HISTORY 500

static int scr_history = SCROLL_HISTORY;

typedef struct {
	const char *symbol;
	void (*arrange)(unsigned int, unsigned int, unsigned int, unsigned int);
} Layout;

typedef struct Window Window;
struct Window {
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
};

typedef struct {
	short fg;
	short bg;
	int attr;
} StyleProperty;

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

#define MAX_KEYS 3

typedef unsigned int KeyCombo[MAX_KEYS];

typedef struct {
	mmask_t mask;
	Action action;
} Button;

typedef struct {
	const char *name;
	Action action;
} Cmd;

enum { BAR_TOP, BAR_BOTTOM, BAR_OFF };
enum { BAR_LEFT, BAR_RIGHT };

enum { MIN_ALIGN_HORIZ, MIN_ALIGN_VERT };

typedef struct {
	int fd;
	int pos, lastpos;
	int align;
	bool autohide;
	unsigned short int h;
	unsigned short int y;
	char text[512];
	const char *file;
} StatusBar;

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

#ifdef NDEBUG
 #define debug(format, args...)
#else
 #define debug eprint
#endif

extern int scheme_init(const char *);
extern void scheme_uninit(void);
extern int scheme_event_handle(event_t evt);
extern int scheme_eval_file(const char *scm_in, const char *out);
extern void *scheme_env_alloc(void);
extern void scheme_env_free(void *env);

static char *scheme_init_script;

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
static void togglebarpos(const char *args[]);
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

static KeyMap *win_min_kmap;
static KeyMap *global_kmap;
static KeyMap *curr_kmap;

static Window *minibuf;

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

#define COLOR(c)        COLOR_PAIR(colors[c].pair)
/* curses attributes for the currently focused window */
#define SELECTED_ATTR   (COLOR(BLUE) | A_NORMAL)
/* curses attributes for normal (not selected) windows */
#define NORMAL_ATTR     (COLOR(DEFAULT) | A_NORMAL)
/* curses attributes for a window with pending urgent flag */
#define URGENT_ATTR     NORMAL_ATTR
/* curses attributes for the status bar */
#define BAR_ATTR        (COLOR(BLUE) | A_NORMAL)
/* characters for beginning and end of status bar message */
#define BAR_BEGIN       '['
#define BAR_END         ']'
/* status bar (command line option -s) position */
#define BAR_POS         BAR_TOP /* BAR_BOTTOM, BAR_OFF */
/* whether status bar should be hidden if only one window exists */
#define BAR_AUTOHIDE    false
/* master width factor [0.1 .. 0.9] */
#define MFACT 0.5
/* number of windows in master area */
#define NMASTER 1
/* printf format string for the tag in the status bar */
#define TAG_SYMBOL   "[%s%s%s]"
/* curses attributes for the currently selected tags */
#define TAG_SEL      (COLOR(RED) | A_BOLD)
/* curses attributes for not selected tags which contain no windows */
#define TAG_NORMAL   (COLOR(DEFAULT) | A_NORMAL)
/* curses attributes for not selected tags which contain windows */
#define TAG_OCCUPIED (COLOR(BLUE) | A_NORMAL)
/* curses attributes for not selected tags which with urgent windows */
#define TAG_URGENT (COLOR(BLUE) | A_NORMAL | A_BLINK)

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

static char const * const keytable[] = {
	/* add your custom key escape sequences */
};

#define CWD_MAX		256

typedef struct {
	unsigned int curtag, prevtag;
	int nmaster[LENGTH(tags) + 1];
	float mfact[LENGTH(tags) + 1];
	Layout *layout[LENGTH(tags) + 1];
	Layout *layout_prev[LENGTH(tags) + 1];
	int barpos[LENGTH(tags) + 1];
	int barlastpos[LENGTH(tags) + 1];
	bool runinall[LENGTH(tags) + 1];
	char *cwd[LENGTH(tags) + 1];
	char *name[LENGTH(tags) + 1];
	bool msticky[LENGTH(tags) + 1];
	Window *popup[LENGTH(tags) + 1];
} Pertag;

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
static StatusBar bar = { .fd = -1,
			 .lastpos = BAR_POS,
			 .pos = BAR_POS,
			 .align = BAR_RIGHT,
			 .autohide = BAR_AUTOHIDE,
			 .h = 1
		       };

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

static void
eprint(const char *errstr, ...) {
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

static bool
isvisible(Window *c) {
	return c->tags & tagset[seltags];
}

static bool
is_content_visible(Window *c) {
	if (!c)
		return false;
	if (isarrange(fullscreen))
		return sel == c;
	else if (c == minibuf)
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
	bar.y = 0;
	wax = 0;
	way = 0;

	if (minibuf)
		dec_h = ui_window_height_get(minibuf->win);

	wah = ui_height_get(ui)-dec_h;
	waw = ui_width_get(ui);
	if (bar.pos == BAR_TOP) {
		wah -= bar.h;
		way += bar.h;
	} else if (bar.pos == BAR_BOTTOM) {
		wah -= bar.h;
		bar.y = wah;
	}

	if (minibuf)
		ui_window_move(minibuf->win, 0, ui_height_get(ui)-dec_h);
}

static void
hidebar(void) {
	if (bar.pos != BAR_OFF) {
		bar.lastpos = pertag.barlastpos[pertag.curtag] = bar.pos;
		bar.pos = pertag.barpos[pertag.curtag] = BAR_OFF;
	}
}

static void
showbar(void) {
	if (bar.pos == BAR_OFF)
		bar.pos = pertag.barpos[pertag.curtag] = bar.lastpos;
}

static void
drawbar(void) {
	char buf[128];
	int sx, sy, x, y, width;
	unsigned int occupied = 0, urgent = 0;
	if (bar.pos == BAR_OFF)
		return;

	for (Window *c = windows; c; c = c->next) {
		occupied |= c->tags;
		if (c->urgent)
			urgent |= c->tags;
	}

	getyx(stdscr, sy, sx);
	attrset(BAR_ATTR);
	move(bar.y, 0);

	for (unsigned int i = 0; i < LENGTH(tags); i++){
		if (tagset[seltags] & (1 << i))
			attrset(TAG_SEL);
		else if (urgent & (1 << i))
			attrset(TAG_URGENT);
		else if (occupied & (1 << i))
			attrset(TAG_OCCUPIED);
		else
			attrset(TAG_NORMAL);

		if (pertag.name[i+1] && strlen(pertag.name[i+1])) {
			printw(TAG_SYMBOL, tags[i], ":", pertag.name[i+1]);
		} else if (strlen(pertag.cwd[i+1]) && show_tagnamebycwd && (occupied & (1 << i))) {
			printw(TAG_SYMBOL, tags[i], ":", basename(pertag.cwd[i+1]));
		} else {
			printw(TAG_SYMBOL, tags[i], "", "");
		}
	}

	attrset(pertag.runinall[pertag.curtag] ? TAG_SEL : TAG_NORMAL);
	addstr(layout->symbol);
	attrset(TAG_NORMAL);

	getyx(stdscr, y, x);
	(void)y;
	int maxwidth = ui_width_get(ui) - x - 2;

	addch(BAR_BEGIN);
	attrset(BAR_ATTR);

	wchar_t wbuf[sizeof bar.text];
	size_t numchars = mbstowcs(wbuf, bar.text, sizeof bar.text);

	if (numchars != (size_t)-1 && (width = wcswidth(wbuf, maxwidth)) != -1) {
		int pos = 0;

		if (bar.align == BAR_RIGHT) {
			for (; pos + width < maxwidth; pos++)
				addch(' ');
		}

		for (size_t i = 0; i < numchars; i++) {
			pos += wcwidth(wbuf[i]);
			if (pos > maxwidth)
				break;
			addnwstr(wbuf+i, 1);
		}

		if (bar.align == BAR_LEFT) {
			for (; pos + width < maxwidth; pos++)
				addch(' ');
		}

		clrtoeol();
	}

	attrset(TAG_NORMAL);
	mvaddch(bar.y, ui_width_get(ui) - 1, BAR_END);
	attrset(NORMAL_ATTR);
	move(sy, sx);
	ui_refresh(ui);
}

static void draw_title(Window *c) {
	ui_text_style_t title_style = UI_TEXT_STYLE_NORMAL;
	int title_fg = UI_TEXT_COLOR_WHITE;
	int title_bg = UI_TEXT_COLOR_BRIGHT_BLACK;
	int x, y, maxlen, title_y, title_x;
	int w_w = ui_window_width_get(c->win);
	int has_border = ui_window_border_is_enabled(c->win);
	char title[256];
	char tmp[256];
	size_t len;

	if (!ui_window_has_title(c->win)) {
		/* HACK for the minibuf which does not has the title so it will be
		 * skipped */
		ui_window_cursor_get(c->win, &x, &y);
		ui_window_cursor_set(c->win, x, y);
		return;
	}

	if (sel == c || (pertag.runinall[pertag.curtag] && !c->minimized)) {
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
	strncpy(tmp, window_get_title(c), sizeof(tmp));
	if ((size_t)maxlen < strlen(tmp)) {
		tmp[maxlen] = '\0';
	}

	len = snprintf(title, sizeof(title), "%s%s   [%d|%s%s]",
			buffer_is_modified(c->buf) ? "[+] " : "",
			buffer_mode_get(c->buf),
			c->order,
			ismastersticky(c) ? "*" : "",
			tmp[0] ? tmp : "");
	ui_window_draw_text_attr(c->win, 0, title_y, title, w_w,
			title_fg, title_bg,
			UI_TEXT_STYLE_NORMAL);

	ui_window_cursor_set(c->win, x, y);
}

static void buf_update(Window *w);

static void
draw(Window *c) {
	if (is_content_visible(c) || c == get_popup()) {
		event_t evt;

		/* we assume that it will be set on EVT_WIN_DRAW */
		/* ui_window_sidebar_width_set(c->win, 0); */
		ui_window_clear(c->win);

		buf_update(c);

		evt.eid = EVT_WIN_DRAW;
		evt.oid = c->id;
		scheme_event_handle(evt);

		buf_update(c);

		ui_window_redraw(c->win);
		ui_window_draw(c->win);
	}
	if (!isarrange(fullscreen) || c == sel)
		draw_title(c);

	ui_window_refresh(c->win);
}

static void
draw_all(void) {
	if (minibuf)
		draw(minibuf);

	if (!nextvisible(windows)) {
		sel = NULL;
		ui_cursor_enable(ui, false);
		ui_clear(ui);
		drawbar();
		ui_update(ui);
		return;
	}

	if (!isarrange(fullscreen)) {
		for (Window *c = nextvisible(windows); c; c = nextvisible(c->next)) {
			if (c != sel)
				draw(c);
		}
	}

	/* as a last step the selected window is redrawn,
	 * this has the effect that the cursor position is
	 * accurate
	 */
	if (sel)
		draw(sel);
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

	if (bar.fd == -1 && bar.autohide) {
		if ((!windows || !windows->next) && n == 1)
			hidebar();
		else
			showbar();
		update_screen_size();
	}
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
	if (!t && sel == c && ctitle && strlen(ctitle))
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

	if (sel == c)
		return;

	if (c) {
		if (c->minimized)
			curr_kmap = win_min_kmap;
		else
			curr_kmap = global_kmap;
	}

	lastsel = sel;
	sel = c;
	if (lastsel) {
		lastsel->urgent = false;
		if (!isarrange(fullscreen)) {
			draw_title(lastsel);
			ui_window_refresh(lastsel->win);
		}
	}

	if (c) {
		Selection *s;

		detachstack(c);
		attachstack(c);
		settitle(c);
		c->urgent = false;

		if (buffer_term_get(c->buf) && buffer_ref_count(c->buf) > 2) {
			vt_resize(buffer_term_get(c->buf),
					ui_window_height_get(c->win) - ui_window_has_title(c->win),
					ui_window_width_get(c->win));
		}

		if (isarrange(fullscreen)) {
			draw(c);
		} else {
			draw_title(c);
			ui_window_refresh(c->win);
		}

		if (buffer_term_get(c->buf)) {
			ui_cursor_enable(ui, c && !c->minimized &&
					vt_cursor_visible(buffer_term_get(c->buf)));
		} else {
			size_t curs_view = view_cursor_get(c->view);

			buffer_cursor_set(c->buf, curs_view);
			buffer_dirty_set(c->buf, true);
			ui_cursor_enable(ui, true);
		}
	}
}

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
	if (!isarrange(fullscreen) && sel != c && isvisible(c))
		draw_title(c);
}

static void
resize_window(Window *c, int w, int h) {
	ui_window_resize(c->win, w, h);

	if (buffer_term_get(c->buf)) {
		if (c == get_popup()) {
			w-=-2;
			h--;
		}
		vt_resize(buffer_term_get(c->buf), h - ui_window_has_title(c->win), w);
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
		return sel;
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
	int errsv = errno;
	int status;
	pid_t pid;

	while ((pid = waitpid(-1, &status, WNOHANG)) != 0) {
		Buffer *buf;

		if (pid == -1) {
			if (errno == ECHILD) {
				/* no more child processes */
				break;
			}
			eprint("waitpid: %s\n", strerror(errno));
			break;
		}

		debug("child with pid %d died\n", pid);

		buf = buffer_by_pid(pid);
		if (buf)
			buffer_died_set(buf, true);
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
	if (!sel)
		return;
	unsigned int newtags = sel->tags ^ (bitoftag(args[0]) & TAGMASK);
	if (newtags) {
		sel->tags = newtags;
		tagschanged();
	}
}

static void
setpertag(void) {
	layout = pertag.layout[pertag.curtag];
	if (bar.pos != pertag.barpos[pertag.curtag]) {
		bar.pos = pertag.barpos[pertag.curtag];
		update_screen_size();
	}
	bar.lastpos = pertag.barlastpos[pertag.curtag];
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

	for (Window *c = pertag.runinall[pertag.curtag] ? nextvisible(windows) : sel; c; c = nextvisible(c->next)) {
		if (is_content_visible(c)) {
			Vt *term = buffer_term_get(c->buf);

			c->urgent = false;

			if (buffer_term_get(c->buf)) {
				if (code == '\e')
					vt_write(term, buf, len);
				else
					vt_keypress(term, code);
				if (key != -1)
					vt_keypress(term, key);
			} else if (buffer_text_input_is_enabled(c->buf)) {
				size_t pos = buffer_cursor_get(c->buf);
				if (key == -1 && code <= UCHAR_MAX)
					buffer_text_insert_len(c->buf, pos, (char *)&code, 1);
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
		pertag.barpos[i] = bar.pos;
		pertag.barlastpos[i] = bar.lastpos;
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

static void
setup(void) {
	shell = getshell();
	setlocale(LC_CTYPE, "");

	ui = ui_term_new();
	ui_init(ui);

	init_default_keymap();

	mouse_setup();

	vt_init();
	vt_keytable_set(keytable, LENGTH(keytable));
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
	if (sel == w)
		focusnextnm(NULL);

	if (w != get_popup()) {
		detach(w);
	} else {
		set_popup(NULL);
	}
	detachstack(w);

	if (sel == w) {
		Window *next = nextvisible(windows);
		if (next) {
			focus(next);
			toggleminimize();
		} else {
			sel = NULL;
		}
	}
	if (lastsel == w)
		lastsel = NULL;
	ui_window_free(w->win);
	view_free(w->view);
	free(w);
	arrange();
}

static void
destroy(Window *w) {
	Buffer *buf = w->buf;
	void *env;

	buffer_ref_put(w->buf);
	__win_del(w);

	env = buffer_env_get(buf);
	if (buffer_del(buf))
		scheme_env_free(env);
}

static void
cleanup(void) {
	Buffer *b;
	int i;

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
		void *env = buffer_env_get(b);
		if (buffer_del(b))
			scheme_env_free(env);
		b = nextb;
	}

	keymap_free(win_min_kmap);
	keymap_free(global_kmap);
	vt_shutdown();
	ui_free(ui);
	if (bar.fd > 0)
		close(bar.fd);
	if (bar.file)
		unlink(bar.file);
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

static char *getcwd_by_pid(Window *c, char *buf) {
	if (!c)
		return NULL;
	char tmp[32];
	snprintf(tmp, sizeof(tmp), "/proc/%d/cwd", buffer_pid_get(c->buf));
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

	pty = vt_pty_get(buffer_term_get(c->buf));

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
	if (!isarrange(fullscreen) || sel == c)
		draw_title(c);
done:
	close(fd);
}

int create(const char *prog, const char *title, const char *cwd) {
	const char *pargs[4] = { shell, NULL };
	char buf[8];
	const char *env[] = {
		"BORSCH_WINDOW_ID", buf,
		NULL
	};
	char tmppath[PATH_MAX];
	char tmp[256];
	pid_t pid;
	Vt *term;

	if (prog) {
		pargs[1] = "-c";
		pargs[2] = prog;
		pargs[3] = NULL;
	}
	Window *c = calloc(1, sizeof(Window));
	if (!c)
		return -1;
	c->tags = tagset[seltags];
	c->id = ++cmdfifo.id;
	snprintf(buf, sizeof buf, "%d", c->id);

	c->buf = buffer_new(title);
	if (!c->buf) {
		free(c);
		return -1;
	}

	c->view = view_new(buffer_text_get(c->buf));
	if (!c->view) {
		buffer_del(c->buf);
		free(c);
	}

	c->win = ui_window_new(ui, c->view);
	if (!c->win) {
		view_free(c->view);
		buffer_del(c->buf);
		free(c);
		return -1;
	}
	ui_window_resize(c->win, waw, wah);
	ui_window_move(c->win, wax, way);

	term = vt_create(ui_height_get(ui), ui_width_get(ui), scr_history);
	if (!term) {
		view_free(c->view);
		buffer_del(c->buf);
		free(c);
		return -1;
	}

	ui_window_has_title_set(c->win, true);
	ui_window_ops_draw_set(c->win, vt_draw);
	ui_window_priv_set(c->win, term);
	buffer_term_set(c->buf, term);
	vt_attach(term, c->win);

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

	if (!cwd)
		cwd = getcwd_by_pid(sel, tmppath);
	else if (strlen(pertag.cwd[pertag.curtag]))
		cwd = pertag.cwd[pertag.curtag];

	pid = vt_forkpty(term, shell, pargs, cwd, env, NULL, NULL);
	buffer_pid_set(c->buf, pid);

	buffer_env_set(c->buf, scheme_env_alloc());
	buffer_ref_get(c->buf);

	vt_data_set(term, c);
	vt_title_handler_set(term, term_title_handler);
	vt_urgent_handler_set(term, term_urgent_handler);
	ui_window_resize(c->win, waw, wah);
	ui_window_move(c->win, wax, way);
	debug("window with pid %d forked\n", pid);
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
	if (!sel)
		return;
	Window *c = sel;
	do {
		c = nextvisible(c->next);
		if (!c)
			c = nextvisible(windows);
	} while (c && c->minimized && c != sel);
	focus(c);
}

static void
focusprevnm(const char *args[]) {
	if (!sel)
		return;
	Window *c = sel;
	do {
		for (c = c->prev; c && !isvisible(c); c = c->prev);
		if (!c) {
			for (c = windows; c && c->next; c = c->next);
			for (; c && !isvisible(c); c = c->prev);
		}
	} while (c && c != sel && c->minimized);
	focus(c);
}

static void
focuslast(const char *args[]) {
	if (lastsel)
		focus(lastsel);
}

static void
killwindow(void) {
	Window *target = sel;
	pid_t pid;

	if (!target)
		return;

	pid = buffer_pid_get(target->buf);
	if (!pid)
		return;

	debug("killing window with pid: %d\n", pid);
	kill(-pid, SIGKILL);
}

static void killother(const char *args[]) {
	unsigned int n;
	Window *c;

	for (n = 0, c = nextvisible(windows); c; c = nextvisible(c->next)) {
		if (ismastersticky(c) || sel == c)
			continue;
		kill(-(buffer_pid_get(c->buf)), SIGKILL);
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
			if (buffer_term_get(c->buf))
				vt_dirty(buffer_term_get(c->buf));
			ui_window_redraw(c->win);
		}
	}
	ui_redraw(ui);
	update_screen_size();
	arrange();
}

static void
scrollback(const char *args[]) {
	int w_h = ui_window_height_get(sel->win);
	Vt *term;

	if (!is_content_visible(sel))
		return;

	term = buffer_term_get(sel->buf);

	if (term)
		if (!args[0] || atoi(args[0]) < 0)
			vt_scroll(term, -w_h/2);
		else
			vt_scroll(term,  w_h/2);

	if (term)
		ui_cursor_enable(ui, vt_cursor_visible(term));
	else
		ui_cursor_enable(ui, true);
	draw(sel);
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
togglebarpos(const char *args[]) {
	switch (bar.pos == BAR_OFF ? bar.lastpos : bar.pos) {
	case BAR_TOP:
		bar.pos = pertag.barpos[pertag.curtag] = BAR_BOTTOM;
		break;
	case BAR_BOTTOM:
		bar.pos = pertag.barpos[pertag.curtag] = BAR_TOP;
		break;
	}
	update_screen_size();
	redraw(NULL);
}

static void
toggleminimize(void)
{
	Window *c, *m, *t;
	unsigned int n;

	if (!sel)
		return;
	/* do not minimize sticked master */
	if (ismastersticky(sel))
		return;
	/* the last window can't be minimized */
	if (!sel->minimized) {
		for (n = 0, c = nextvisible(windows); c; c = nextvisible(c->next))
			if (!c->minimized)
				n++;
		if (n == 1)
			return;
	}
	sel->minimized = !sel->minimized;
	m = sel;
	/* check whether the master window was minimized */
	if (sel == nextvisible(windows) && sel->minimized) {
		c = nextvisible(sel->next);
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
		if (buffer_term_get(m->buf))
			vt_dirty(buffer_term_get(m->buf));
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
		if (ismastersticky(c) || sel == c)
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

	if (!sel)
		return;
	if (args && args[0])
		focusn(args);
	if ((c = sel) == nextvisible(windows))
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

static void
handle_cmdfifo(void) {
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

	if (buffer_term_get(msel->buf))
		vt_mouse(buffer_term_get(msel->buf), event.x - w_x, event.y - w_y, event.bstate);

	for (i = 0; i < LENGTH(buttons); i++) {
		if (event.bstate & buttons[i].mask)
			buttons[i].action.cmd(buttons[i].action.args);
	}

	msel = NULL;
#endif /* CONFIG_MOUSE */
}

static void
handle_statusbar(void) {
	char *p;
	int r;
	switch (r = read(bar.fd, bar.text, sizeof bar.text - 1)) {
		case -1:
			strncpy(bar.text, strerror(errno), sizeof bar.text - 1);
			bar.text[sizeof bar.text - 1] = '\0';
			bar.fd = -1;
			break;
		case 0:
			bar.fd = -1;
			break;
		default:
			bar.text[r] = '\0';
			p = bar.text + r - 1;
			for (; p >= bar.text && *p == '\n'; *p-- = '\0');
			for (; p >= bar.text && *p != '\n'; --p);
			if (p >= bar.text)
				memmove(bar.text, p + 1, strlen(p));
			drawbar();
	}
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
		}
	}

	return init;
}

int
main(int argc, char *argv[]) {
	KeyCombo keys;
	unsigned int key_index = 0;
	memset(keys, 0, sizeof(keys));
	sigset_t emptyset, blockset;

	setenv("BORSCH", VERSION, 1);
	if (!parse_args(argc, argv)) {
		setup();
	}

	sigemptyset(&emptyset);
	sigemptyset(&blockset);
	sigaddset(&blockset, SIGWINCH);
	sigaddset(&blockset, SIGCHLD);
	sigprocmask(SIG_BLOCK, &blockset, NULL);

	while (running) {
		int r, nfds = 0;
		fd_set rd;

		ui_resize(ui);
		update_screen_size();

		FD_ZERO(&rd);
		FD_SET(STDIN_FILENO, &rd);

		if (cmdfifo.fd != -1) {
			FD_SET(cmdfifo.fd, &rd);
			nfds = cmdfifo.fd;
		}

		if (bar.fd != -1) {
			FD_SET(bar.fd, &rd);
			nfds = MAX(nfds, bar.fd);
		}

		for (Window *c = windows; c; ) {
			if (buffer_is_died(c->buf)) {
				Window *t = c->next;
				destroy(c);
				c = t;
				continue;
			}
			if (buffer_term_get(c->buf)) {
				int pty = vt_pty_get(buffer_term_get(c->buf));
				vt_processed_set(buffer_term_get(c->buf), false);
				FD_SET(pty, &rd);
				nfds = MAX(nfds, pty);
			}
			c = c->next;
		}
		if (get_popup()) {
			if (buffer_is_died(get_popup()->buf)) {
				destroy(get_popup());
			}
		}
		/* TODO: what to do with a died buffers ? */

		ui_update(ui);

		r = pselect(nfds + 1, &rd, NULL, NULL, NULL, &emptyset);

		if (r < 0) {
			if (errno == EINTR)
				continue;
			perror("select()");
			exit(EXIT_FAILURE);
		}

		if (FD_ISSET(STDIN_FILENO, &rd)) {
			int alt_code;
			event_t evt;
			int code;

			if (sel && !sel->minimized) {
				KeyMap *map = buffer_keymap_get(sel->buf);
				if (map)
					curr_kmap = map;
			};
reenter:
			code = getch();

			evt.eid = EVT_KEY_PRESS;
			evt.oid = code;
			scheme_event_handle(evt);

			if (code == ALT) {
				nodelay(stdscr, TRUE);
				alt_code = getch();
				nodelay(stdscr, FALSE);
				if (alt_code > 0) {
					keys[key_index++] = code;
					code = alt_code;
				}
			}

			if (code >= 0) {
				keys[key_index++] = code;
				KeyBinding *kbd = NULL;
				if (code == KEY_MOUSE) {
					key_index = 0;
					handle_mouse();
				} else if ((kbd = keymap_match(curr_kmap, keys, key_index))) {
					if (keymap_kbd_is_map(kbd)) {
						curr_kmap = keymap_kbd_map_get(kbd);
						memset(keys, 0, sizeof(keys));
						key_index = 0;
						goto reenter;
					}

					if (keymap_kbd_len(kbd) == key_index) {
						keymap_kbd_action(kbd);
						memset(keys, 0, sizeof(keys));
						key_index = 0;
					}
				} else {
					key_index = 0;
					memset(keys, 0, sizeof(keys));
					keypress(code);
				}
			}
		}

		if (cmdfifo.fd != -1 && FD_ISSET(cmdfifo.fd, &rd))
			handle_cmdfifo();

		if (bar.fd != -1 && FD_ISSET(bar.fd, &rd))
			handle_statusbar();

		if (minibuf)
			draw(minibuf);

		for (Window *c = windows; c; c = c->next) {
			pid_t pid = buffer_pid_get(c->buf);
			Vt *term = buffer_term_get(c->buf);

			if (term) {
				if (!vt_is_processed(term) && FD_ISSET(vt_pty_get(term), &rd)) {
					if (vt_process(term) < 0 && errno == EIO) {
						buffer_died_set(c->buf, true);
						continue;
					}
					vt_processed_set(term, true);
				}
			}

			if (is_content_visible(c)) {
				if (pid && !buffer_name_is_locked(c->buf))
					synctitle(c);
				if (c != sel) {
					draw(c);
				}
			} else if (!isarrange(fullscreen) && isvisible(c)
					&& c->minimized) {
				draw_title(c);
				ui_window_refresh(c->win);
			}
		}

		if (is_content_visible(sel)) {
			if (buffer_term_get(sel->buf)) {
				ui_cursor_enable(ui, vt_cursor_visible(buffer_term_get(sel->buf)));
			} else {
				ui_cursor_enable(ui, true);
			}
			draw(sel);
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

	return NULL;
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
		return isvisible(c) || c == minibuf;

	return false;
}

int win_first_get(void)
{
	Window *c;

	for (c = windows; c && !isvisible(c); c = c->next);

	if (c && isvisible(c))
		return c->id;

	return 0;
}

int win_prev_get(int wid)
{
	Window *c = window_get_by_id(wid);
	Window *p;

	if (!c)
		return 0;

	for (p = c->prev; p && !isvisible(p); p = p->prev);

	if (p && isvisible(p))
		return p->id;

	return 0;
}

int win_next_get(int wid)
{
	Window *c = window_get_by_id(wid);
	Window *n;

	if (!c)
		return 0;

	for (n = c->next; n && !isvisible(n); n = n->next);

	if (n && isvisible(n))
		return n->id;

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
	if (sel)
		return sel->id;

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
	if (sel)
		return sel->id;
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

	if (w) {
		ui_window_sidebar_width_set(w->win, width);
		buffer_dirty_set(w->buf, true);
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

static int style_prop_draw(Buffer *buf, int id, size_t start, size_t end, void *data,
		void *arg)
{
	StyleProperty *prop = data;
	View *view = arg;
	CellStyle style = {
		.attr = prop->attr,
		.fg = prop->fg,
		.bg = prop->bg,
	};

	view_style(view, style, start, end);

	return 0;
}

static void on_view_update_cb(UiWin *win)
{
	Window *w = ui_window_priv_get(win);
	Filerange v = view_viewport_get(w->view);

	buffer_properties_walk(w->buf, PROPERTY_TYPE_TEXT_STYLE,
			v.start, v.end, w->view, style_prop_draw);
	buffer_properties_walk(w->buf, PROPERTY_TYPE_TEXT_HIGHLIGHT,
			v.start, v.end, w->view, style_prop_draw);

	if (w->highlight_mark) {
		size_t start = buffer_mark_get(w->buf);
		size_t end = buffer_cursor_get(w->buf);
		CellStyle style = {
			.fg = UI_TEXT_COLOR_WHITE,
			.bg = UI_TEXT_COLOR_BLUE,
		};

		view_style(w->view, style, MIN(start, end), MAX(start, end));
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
	} else {
		c->buf = buffer_new("");
	}

	buffer_ref_get(c->buf);

	if (!c->buf) {
		free(c);
		return -1;
	}

	c->view = view_new(buffer_text_get(c->buf));
	if (!c->view) {
		buffer_del(c->buf);
		free(c);
		return -1;
	}

	c->win = ui_window_new(ui, c->view);
	if (!c->win) {
		view_free(c->view);
		buffer_del(c->buf);
		free(c);
		return -1;
	}

	if (!bid)
		buffer_env_set(c->buf, scheme_env_alloc());

	if (buffer_term_get(c->buf)) {
		Vt *term = buffer_term_get(c->buf);

		vt_title_handler_set(term, term_title_handler);
		vt_urgent_handler_set(term, term_urgent_handler);
		ui_window_ops_draw_set(c->win, vt_draw);
		ui_window_priv_set(c->win, term);
		buffer_term_set(c->buf, term);
		vt_attach(term, c->win);
		vt_data_set(term, c);
		vt_dirty(term);
	} else {
		ui_window_priv_set(c->win, c);
		ui_window_on_view_update_set(c->win, on_view_update_cb);
	}

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

	if (c && buffer_pid_get(c->buf))
		kill(-buffer_pid_get(c->buf), SIGKILL);
	else
		destroy(c);
}

void win_close(int wid)
{
	Window *w = window_get_by_id(wid);

	if (w) {
		buffer_ref_put(w->buf);
		__win_del(w);
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

	orig = sel;

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

	orig = sel;

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
		c->highlight_mark = enable;
	}
}

void win_popup(int wid, bool enable)
{
	Window *w = window_get_by_id(wid);
	int x, y;

	if (w) {
		/* TODO: add support for term window */
		if (buffer_term_get(w->buf))
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
		if (width > 0)
			ui_window_width_set(w->win, width);
		if (height > 0)
			ui_window_height_set(w->win, height);

		if (w == minibuf) {
			update_screen_size();
			buffer_dirty_set(w->buf, true);
			draw(w);
			arrange();
		} else {
			redraw(NULL);
		}
	}
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

	if (w && b && w->buf != b) {
		buffer_ref_put(w->buf);

		if (buffer_term_get(b)) {
			Vt *term = buffer_term_get(b);

			ui_window_on_view_update_set(w->win, NULL);
			ui_window_priv_set(w->win, term);
			ui_window_ops_draw_set(w->win, vt_draw);
			vt_data_set(term, w);
			vt_dirty(term);
		} else {
			ui_window_on_view_update_set(w->win, on_view_update_cb);
			ui_window_ops_draw_set(w->win, NULL);
			ui_window_priv_set(w->win, w);
		}

		view_reload(w->view, buffer_text_get(b));
		buffer_dirty_set(b, true);
		w->buf = b;
		buffer_ref_get(w->buf);
	}
}

int kmap_add(int pid)
{
	KeyMap *pmap = keymap_by_id(pid);
	KeyMap *kmap;

	kmap = keymap_new(pmap);
	if (kmap)
		return keymap_id_get(kmap);

	return -1;
}

int kmap_parent_set(int kid, char *name)
{
	KeyMap *kmap = keymap_by_id(kid);

	if (kmap && name) {
		keymap_parent_set(kmap, name);
		return 0;
	}

	return -1;
}

void kmap_del(int kid)
{
	KeyMap *kmap = keymap_by_id(kid);

	if (kmap)
		keymap_ref_put(kmap);
}

int buf_new(char *name)
{
	Buffer *buf = buffer_new(name);

	if (buf) {
		buffer_env_set(buf, scheme_env_alloc());
		buffer_ref_get(buf);
		return buffer_id_get(buf);
	}

	return 0;
}

void buf_del(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		void *env = buffer_env_get(buf);
		buffer_ref_put(buf);
		if (buffer_del(buf))
			scheme_env_free(env);
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
	if (sel)
		return buffer_id_get(sel->buf);

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

		if (w == sel || w == minibuf) {
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
				if (w == sel)
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
	for (Window *w = windows; w; w = w->next) {
		if (is_content_visible(w)) {
			buf_update(w);
		}
	}
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
		if (minibuf) {
			draw(minibuf);
		}
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
			pos += buffer_text_insert_len(buf, pos, data, len);
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

void buf_input_enable(int bid, bool enable)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_text_input_enable(buf, enable);
	}
}

void buf_mode_set(int bid, char *name)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_mode_set(buf, name);
		if (sel)
			draw_title(sel);
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
		return buffer_term_get(buf) != NULL;

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

int buf_prop_style_add(int bid, int type, int fg, int bg, int attr,  int start, int end)
{
	Buffer *buf = buffer_by_id(bid);
	StyleProperty *style;
	int err;

	if (!buf)
		return -1;

	style = calloc(1, sizeof(StyleProperty));
	if (!style)
		return -1;

	style->attr = attr;
	style->fg = fg;
	style->bg = bg;

	if (type == PROPERTY_TYPE_TEXT_HIGHLIGHT) {
		style->attr = 0;
		style->fg = UI_TEXT_COLOR_WHITE;
		style->bg = UI_TEXT_COLOR_BLUE;
	}

	err = buffer_property_add(buf, type, start, end, style);
	if (err) {
		free(style);
		return err;
	}

	buffer_dirty_set(buf, true);
	return 0;
}

void buf_prop_del(int bid, int type, int start, int end)
{
	Buffer *buf = buffer_by_id(bid);

	buffer_property_remove(buf, type, start == -1 ? EPOS : start,
			end == -1 ? EPOS : end);
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

int minibuf_create(void)
{
	KeyMap *kmap;
	Window *w;

	if (minibuf)
		return minibuf->id;

	w = calloc(1, sizeof(Window));
	if (!w)
		return -1;

	/* c->tags = tagset[seltags]; */
	w->id = ++cmdfifo.id;

	w->buf = buffer_new("*minibuf*");
	if (!w->buf) {
		free(w);
		return -1;
	}

	kmap = keymap_new(NULL);
	if (!kmap) {
		buffer_del(w->buf);
		free(w);
		return -1;
	}

	w->view = view_new(buffer_text_get(w->buf));
	if (!w->view) {
		keymap_free(kmap);
		buffer_del(w->buf);
		free(w);
		return -1;
	}

	w->win = ui_window_new(ui, w->view);
	if (!w->win) {
		view_free(w->view);
		keymap_free(kmap);
		buffer_del(w->buf);
		free(w);
		return -1;
	}

	buffer_env_set(w->buf, scheme_env_alloc());
	buffer_ref_get(w->buf);

	ui_window_on_view_update_set(w->win, on_view_update_cb);
	ui_window_resize(w->win, waw, 1);
	ui_window_priv_set(w->win, w);
	ui_window_move(w->win, 0, ui_height_get(ui)-1);
	ui_window_draw(w->win);

	minibuf = w;
	update_screen_size();
	return w->id;
}

int term_create(char *prog, char *title)
{
	if (!get_popup())
		return create(prog, title, NULL);
	return -1;
}

int term_keys_send(int bid, char *keys)
{
	Buffer *buf = buffer_by_id(bid);

	if (!buf)
		return -1;

	if (buffer_term_get(buf))
		vt_write(buffer_term_get(buf), keys, strlen(keys));
	return 0;
}

int term_text_send(int bid, char *text)
{
	Buffer *buf = buffer_by_id(bid);

	if (!buf)
		return -1;

	if (buffer_term_get(buf))
		vt_write(buffer_term_get(buf), text, strlen(text));
	return 0;
}

int term_text_get(int bid, char **buf, size_t *len)
{
	Buffer *b = buffer_by_id(bid);

	if (b && buffer_term_get(b)) {
		*len = vt_content_get(buffer_term_get(b), buf, false);
		return 0;
	} else {
		return -1;
	}
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
		seltags ^= 1; /* toggle sel tagset */
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
			fprintf(stderr, "could not create %s\n", rundir);
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

	snprintf(tmp, sizeof(tmp), "%s/"PROGNAME"-status-fifo-%d", rundir, pid);
	bar.fd = __open_or_create_fifo(tmp, &bar.file, O_RDWR);
	if (!(sta = realpath(tmp, NULL))) {
		close(cmdfifo.fd);
		unlink(cmdfifo.file);
		close(retfifo.fd);
		unlink(retfifo.file);
		return -1;
	}
	bar.file = strdup(sta);

	setenv("BORSCH_CMD_FIFO", cmd, 1);
	setenv("BORSCH_RET_FIFO", ret, 1);

	return 0;
}

int tagbar_status_set(const char *s)
{
	write(bar.fd, s, strlen(s));
	return 0;
}

int tagbar_status_align(int align)
{
	if (align)
		bar.align = BAR_LEFT;
	else
		bar.align = BAR_RIGHT;

	drawbar();
	return 0;
}

int tagbar_show(bool show)
{
	if (!show)
		bar.pos = BAR_OFF;
	else
		bar.pos = bar.lastpos;

	if (bar.pos == BAR_OFF)
		hidebar();
	else
		showbar();

	bar.autohide = false;
	update_screen_size();
	redraw(NULL);
	return 0;
}

void do_quit(void)
{
	quit(NULL);
}
