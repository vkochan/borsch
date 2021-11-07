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

typedef struct {
	int history;
	int w;
	int h;
	volatile sig_atomic_t need_resize;
} Screen;

typedef struct {
	const char *symbol;
	void (*arrange)(void);
} Layout;

typedef struct Client Client;
struct Client {
	WINDOW *window;
	Vt *term;
	Vt *overlay, *app;
	bool is_editor;
	int editor_fds[2];
	volatile sig_atomic_t overlay_died;
	const char *cmd;
	char title[255];
	bool sync_title;
	int order;
	pid_t pid;
	unsigned short int id;
	unsigned short int x;
	unsigned short int y;
	unsigned short int w;
	unsigned short int h;
	bool has_title_line;
	bool minimized;
	bool urgent;
	volatile sig_atomic_t died;
	Client *next;
	Client *prev;
	Client *snext;
	unsigned int tags;
};

typedef struct {
	short fg;
	short bg;
	short fg256;
	short bg256;
	short pair;
} Color;

typedef struct {
	const char *title;
	attr_t attrs;
	Color *color;
} ColorRule;

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
	KeyCombo keys;
	Action action;
} KeyBinding;

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

typedef struct {
	char *name;
	const char *argv[4];
	bool filter;
	bool color;
} Editor;

#define LENGTH(arr) (sizeof(arr) / sizeof((arr)[0]))
#define MAX(x, y)   ((x) > (y) ? (x) : (y))
#define MIN(x, y)   ((x) < (y) ? (x) : (y))
#define TAGMASK     ((1 << LENGTH(tags)) - 1)

#ifdef NDEBUG
 #define debug(format, args...)
#else
 #define debug eprint
#endif

extern int scheme_init(void);
extern void scheme_uninit(void);
extern int scheme_event_handle(event_t evt);
extern int scheme_eval_file(const char *scm_in, const char *out);

/* commands for use by keybindings */
static void create(const char *args[]);
static void editor(const char *args[]);
static void copymode(const char *args[]);
static void focusn(const char *args[]);
static void focusnextnm(const char *args[]);
static void focusprevnm(const char *args[]);
static void focuslast(const char *args[]);
static void killclient(const char *args[]);
static void killother(const char *args[]);
static void quit(const char *args[]);
static void redraw(const char *args[]);
static void scrollback(const char *args[]);
static void setlayout(const char *args[]);
static void togglemaximize(const char *args[]);
static int getnmaster(void);
static float getmfact(void);
static void startup(const char *args[]);
static void togglebar(const char *args[]);
static void togglebarpos(const char *args[]);
static void toggleminimize(const char *args[]);
static void minimizeother(const char *args[]);
static void togglemouse(const char *args[]);
static void togglerunall(const char *args[]);
static void toggletag(const char *args[]);
static void toggleview(const char *args[]);
static void viewprevtag(const char *args[]);
static void zoom(const char *args[]);
static void doeval(const char *args[]);
static void setminimized(const char *args[]);

/* commands for use by mouse bindings */
static void mouse_focus(const char *args[]);
static void mouse_fullscreen(const char *args[]);
static void mouse_minimize(const char *args[]);
static void mouse_zoom(const char *args[]);

static void attachafter(Client *c, Client *a);
static Client* nextvisible(Client *c);

static void focus(Client *c);
static void resize(Client *c, int x, int y, int w, int h);
extern Screen screen;
static unsigned int waw, wah, wax, way;
static Client *clients = NULL;
static char *title;
static bool show_tagnamebycwd = false;

static KeyBinding *scmkeyb = NULL;
static int scmkeybn;

static KeyBinding *modkeyb = NULL;
static int modkeybn;

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
/* whether status bar should be hidden if only one client exists */
#define BAR_AUTOHIDE    false
/* master width factor [0.1 .. 0.9] */
#define MFACT 0.5
/* number of clients in master area */
#define NMASTER 1
/* scroll back buffer size in lines */
#define SCROLL_HISTORY 500
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

#define MOD  CTRL('g')

static KeyBinding min_bindings[] = {
	{ { '\n',   }, { toggleminimize,   { NULL }   } },
	{ { '\r',   }, { toggleminimize,   { NULL }   } },
	{ { 'x',    }, { killclient,       { NULL }   } },
};

static const ColorRule colorrules[] = {
	{ "", A_NORMAL, &colors[DEFAULT] }, /* default */
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

/* gets executed when app is started */
static Action actions[] = {
	{ create, { NULL } },
};

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
} Pertag;

/* global variables */
static const char *prog_name = PROGNAME;
Screen screen = { .history = SCROLL_HISTORY };
static Pertag pertag;
static Client *stack = NULL;
static Client *sel = NULL;
static Client *lastsel = NULL;
static Client *msel = NULL;
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
static Register copyreg;
static volatile sig_atomic_t running = true;
static bool runinall = false;
/* make sense only in layouts which has master window (tile, bstack) */
static int min_align = MIN_ALIGN_HORIZ;

static Cmd commands[] = {
	/* create [cmd]: create a new window, run `cmd` in the shell if specified */
	{ "create", { create,	{ NULL } } },
	/* put/get window to/from master area */
	{ "zoom", { zoom, { NULL } } },
	/* set cwd per tag or for current */
	/* change layout by name or select next */
	{ "setlayout", { setlayout, { NULL } } },
	{ "setminimized", { setminimized, { NULL } } },
	{ "kill", { killclient, { NULL } } },
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
isvisible(Client *c) {
	return c->tags & tagset[seltags];
}

static bool
is_content_visible(Client *c) {
	if (!c)
		return false;
	if (isarrange(fullscreen))
		return sel == c;
	return isvisible(c) && !c->minimized;
}

static Client*
nextvisible(Client *c) {
	for (; c && !isvisible(c); c = c->next);
	return c;
}

static bool ismaster(Client *c) {
	int n = 0;
	Client *m;

	for (m = nextvisible(clients); m && n < getnmaster(); m = nextvisible(m->next), n++) {
		if (c == m)
			return true;
	}

	return false;
}

static bool ismastersticky(Client *c) {
	int n = 0;
	Client *m;

	if (!pertag.msticky[pertag.curtag])
		return false;
	if (!c)
		return true;

	for (m = nextvisible(clients); m && n < getnmaster(); m = nextvisible(m->next), n++) {
		if (c == m)
			return true;
	}

	return false;
}

static void
updatebarpos(void) {
	bar.y = 0;
	wax = 0;
	way = 0;
	wah = screen.h;
	waw = screen.w;
	if (bar.pos == BAR_TOP) {
		wah -= bar.h;
		way += bar.h;
	} else if (bar.pos == BAR_BOTTOM) {
		wah -= bar.h;
		bar.y = wah;
	}
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

	for (Client *c = clients; c; c = c->next) {
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
	int maxwidth = screen.w - x - 2;

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
	mvaddch(bar.y, screen.w - 1, BAR_END);
	attrset(NORMAL_ATTR);
	move(sy, sx);
	wnoutrefresh(stdscr);
}

static int
show_border(void) {
	return (bar.pos != BAR_OFF) || (clients && clients->next);
}

static void
draw_border(Client *c) {
	int attrs_title = (COLOR(MAGENTA) | A_NORMAL);
	int x, y, maxlen, attrs = NORMAL_ATTR;
	char t = '\0';


	if (!show_border())
		return;
	if (sel != c && c->urgent)
		attrs = URGENT_ATTR;
	if (sel == c || (pertag.runinall[pertag.curtag] && !c->minimized))
		attrs_title = attrs = SELECTED_ATTR;

	wattrset(c->window, attrs);
	getyx(c->window, y, x);
	if (c == sel) {
	        wattrset(c->window, COLOR(BLUE_BG));
		mvwhline(c->window, 0, 0, ' ', c->w);
	        wattrset(c->window, attrs);
	} else {
		mvwhline(c->window, 0, 0, ACS_HLINE, c->w);
	}
	maxlen = c->w - 10;
	if (maxlen < 0)
		maxlen = 0;
	if ((size_t)maxlen < sizeof(c->title)) {
		t = c->title[maxlen];
		c->title[maxlen] = '\0';
	}

	wattrset(c->window, attrs_title);
	mvwprintw(c->window, 0, 2, "[%d|%s%s]",
		  c->order,
		  ismastersticky(c) ? "*" : "",
	          *c->title ? c->title : "");
	wattrset(c->window, attrs);
	if (t)
		c->title[maxlen] = t;
	wmove(c->window, y, x);
}

static void
draw_content(Client *c) {
	vt_draw(c->term, c->window, c->has_title_line, 0);
}

static void
draw(Client *c) {
	if (is_content_visible(c)) {
		redrawwin(c->window);
		draw_content(c);
	}
	if (!isarrange(fullscreen) || c == sel)
		draw_border(c);
	wnoutrefresh(c->window);
}

static void
draw_all(void) {
	if (!nextvisible(clients)) {
		sel = NULL;
		curs_set(0);
		erase();
		drawbar();
		doupdate();
		return;
	}

	if (!isarrange(fullscreen)) {
		for (Client *c = nextvisible(clients); c; c = nextvisible(c->next)) {
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
	for (Client *c = nextvisible(clients); c; c = nextvisible(c->next)) {
		c->order = ++n;
		if (c->minimized)
			m++;
	}
	erase();
	attrset(NORMAL_ATTR);
	if (bar.fd == -1 && bar.autohide) {
		if ((!clients || !clients->next) && n == 1)
			hidebar();
		else
			showbar();
		updatebarpos();
	}
	if (m && !isarrange(fullscreen)) {
		if (min_align == MIN_ALIGN_VERT)
			dh = m;
		else
			dh = 1;
	}
	wah -= dh;
	layout->arrange();
	if (m && !isarrange(fullscreen)) {
		unsigned int i = 0, nw = waw / m, nx = wax;
		for (Client *c = nextvisible(clients); c; c = nextvisible(c->next)) {
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
	wnoutrefresh(stdscr);
	drawbar();
	draw_all();
}

static Client *
lastmaster(unsigned int tag) {
	Client *c = clients;
	int n = 1;

	for (; c && !(c->tags & tag); c = c->next);
	for (; c && n < getnmaster(); c = c->next, n++);

	return c;
}

static void
attachfirst(Client *c) {
	if (clients)
		clients->prev = c;
	c->next = clients;
	c->prev = NULL;
	clients = c;
	for (int o = 1; c; c = nextvisible(c->next), o++)
		c->order = o;
}

static void
attach(Client *c) {
	if (ismastersticky(NULL)) {
		Client *master = lastmaster(c->tags);

		if (master) {
			attachafter(c, master);
			return;
		}
	}

	attachfirst(c);
}

static void
attachafter(Client *c, Client *a) { /* attach c after a */
	if (c == a)
		return;
	if (!a)
		for (a = clients; a && a->next; a = a->next);

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
attachstack(Client *c) {
	c->snext = stack;
	stack = c;
}

static void
detach(Client *c) {
	Client *d;
	if (c->prev)
		c->prev->next = c->next;
	if (c->next) {
		c->next->prev = c->prev;
		for (d = nextvisible(c->next); d; d = nextvisible(d->next))
			--d->order;
	}
	if (c == clients)
		clients = c->next;
	c->next = c->prev = NULL;
}

static void
settitle(Client *c) {
	char *term, *t = title;
	if (!t && sel == c && *c->title)
		t = c->title;
	if (t && (term = getenv("TERM")) && !strstr(term, "linux")) {
		printf("\033]0;%s\007", t);
		fflush(stdout);
	}
}

static void
detachstack(Client *c) {
	Client **tc;
	for (tc = &stack; *tc && *tc != c; tc = &(*tc)->snext);
	*tc = c->snext;
}

static void
focus(Client *c) {
	if (!c)
		for (c = stack; c && !isvisible(c); c = c->snext);

	if (c) {
		if (c->minimized) {
			modkeybn = LENGTH(min_bindings);
			modkeyb = min_bindings;
		} else {
			modkeybn = 0;
			modkeyb = NULL;
		}
	}

	if (sel == c)
		return;
	lastsel = sel;
	sel = c;
	if (lastsel) {
		lastsel->urgent = false;
		if (!isarrange(fullscreen)) {
			draw_border(lastsel);
			wnoutrefresh(lastsel->window);
		}
	}

	if (c) {
		detachstack(c);
		attachstack(c);
		settitle(c);
		c->urgent = false;
		if (isarrange(fullscreen)) {
			draw(c);
		} else {
			draw_border(c);
			wnoutrefresh(c->window);
		}
	}
	curs_set(c && !c->minimized && vt_cursor_visible(c->term));

	if (c) {
		event_t evt;

		evt.eid = EVT_WIN_SELECTED;
		evt.oid = c->id;
		scheme_event_handle(evt);
	}
}

static void
applycolorrules(Client *c) {
	const ColorRule *r = colorrules;
	short fg = r->color->fg, bg = r->color->bg;
	attr_t attrs = r->attrs;

	for (unsigned int i = 1; i < LENGTH(colorrules); i++) {
		r = &colorrules[i];
		if (strstr(c->title, r->title)) {
			attrs = r->attrs;
			fg = r->color->fg;
			bg = r->color->bg;
			break;
		}
	}

	vt_default_colors_set(c->term, attrs, fg, bg);
}

static void
term_title_handler(Vt *term, const char *title) {
	/* Client *c = (Client *)vt_data_get(term); */
	/* if (title) */
	/* 	strncpy(c->title, title, sizeof(c->title) - 1); */
	/* c->title[title ? sizeof(c->title) - 1 : 0] = '\0'; */
	/* c->sync_title = false; */
	/* settitle(c); */
	/* if (!isarrange(fullscreen)) */
	/* 	draw_border(c); */
	/* applycolorrules(c); */
}

static void
term_urgent_handler(Vt *term) {
	Client *c = (Client *)vt_data_get(term);
	c->urgent = true;
	printf("\a");
	fflush(stdout);
	drawbar();
	if (!isarrange(fullscreen) && sel != c && isvisible(c))
		draw_border(c);
}

static void
move_client(Client *c, int x, int y) {
	if (c->x == x && c->y == y)
		return;
	debug("moving, x: %d y: %d\n", x, y);
	if (mvwin(c->window, y, x) == ERR) {
		eprint("error moving, x: %d y: %d\n", x, y);
	} else {
		c->x = x;
		c->y = y;
	}
}

static void
resize_client(Client *c, int w, int h) {
	bool has_title_line = show_border();
	bool resize_window = c->w != w || c->h != h;
	if (resize_window) {
		debug("resizing, w: %d h: %d\n", w, h);
		if (wresize(c->window, h, w) == ERR) {
			eprint("error resizing, w: %d h: %d\n", w, h);
		} else {
			c->w = w;
			c->h = h;
		}
	}
	if (resize_window || c->has_title_line != has_title_line) {
		c->has_title_line = has_title_line;
		vt_resize(c->app, h - has_title_line, w);
		if (c->overlay)
			vt_resize(c->overlay, h - has_title_line, w);
	}
}

static void
resize(Client *c, int x, int y, int w, int h) {
	resize_client(c, w, h);
	move_client(c, x, y);
}

static Client*
get_client_by_coord(unsigned int x, unsigned int y) {
	if (y < way || y >= way+wah)
		return NULL;
	if (isarrange(fullscreen))
		return sel;
	for (Client *c = nextvisible(clients); c; c = nextvisible(c->next)) {
		if (x >= c->x && x < c->x + c->w && y >= c->y && y < c->y + c->h) {
			debug("mouse event, x: %d y: %d client: %d\n", x, y, c->order);
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
		if (pid == -1) {
			if (errno == ECHILD) {
				/* no more child processes */
				break;
			}
			eprint("waitpid: %s\n", strerror(errno));
			break;
		}

		debug("child with pid %d died\n", pid);

		for (Client *c = clients; c; c = c->next) {
			if (c->pid == pid) {
				c->died = true;
				break;
			}
			if (c->overlay && vt_pid_get(c->overlay) == pid) {
				c->overlay_died = true;
				break;
			}
		}
	}

	errno = errsv;
}

static void
sigwinch_handler(int sig) {
	screen.need_resize = true;
}

static void
sigterm_handler(int sig) {
	running = false;
}

static void
resize_screen(void) {
	struct winsize ws;

	if (ioctl(0, TIOCGWINSZ, &ws) == -1) {
		getmaxyx(stdscr, screen.h, screen.w);
	} else {
		screen.w = ws.ws_col;
		screen.h = ws.ws_row;
	}

	debug("resize_screen(), w: %d h: %d\n", screen.w, screen.h);

	resizeterm(screen.h, screen.w);
	wresize(stdscr, screen.h, screen.w);
	updatebarpos();
	clear();
	arrange();
}

static KeyBinding*
keybindmatch(KeyBinding *keyb, unsigned int keybn, KeyCombo keys, unsigned int keycount) {
	for (unsigned int b = 0; b < keybn; b++) {
		for (unsigned int k = 0; k < keycount; k++) {
			if (keys[k] != keyb[b].keys[k])
				break;
			if (k == keycount - 1)
				return &keyb[b];
		}
	}
	return NULL;
}

static KeyBinding*
keybinding(KeyCombo keys, unsigned int keycount) {
	KeyBinding *keyb = NULL;

	if (modkeyb)
		keyb = keybindmatch(modkeyb, modkeybn, keys, keycount);
	if (!keyb)
		keyb = keybindmatch(scmkeyb, scmkeybn, keys, keycount);
	return keyb;
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
	for (Client *c = nextvisible(clients); c; c = nextvisible(c->next)) {
		if (!c->minimized) {
			allminimized = false;
			break;
		}
	}
	if (allminimized && nextvisible(clients)) {
		focus(NULL);
		toggleminimize(NULL);
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
	event_t evt;

	layout = pertag.layout[pertag.curtag];
	if (bar.pos != pertag.barpos[pertag.curtag]) {
		bar.pos = pertag.barpos[pertag.curtag];
		updatebarpos();
	}
	bar.lastpos = pertag.barlastpos[pertag.curtag];
	runinall = pertag.runinall[pertag.curtag];

	evt.eid = EVT_VIEW_SELECTED;
	evt.oid = pertag.curtag;
	scheme_event_handle(evt);
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

	for (Client *c = pertag.runinall[pertag.curtag] ? nextvisible(clients) : sel; c; c = nextvisible(c->next)) {
		if (is_content_visible(c)) {
			c->urgent = false;
			if (code == '\e')
				vt_write(c->term, buf, len);
			else
				vt_keypress(c->term, code);
			if (key != -1)
				vt_keypress(c->term, key);
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

static void
setup(void) {
	shell = getshell();
	setlocale(LC_CTYPE, "");
	initscr();
	start_color();
	noecho();
	nonl();
	keypad(stdscr, TRUE);
	mouse_setup();
	raw();
	vt_init();
	vt_keytable_set(keytable, LENGTH(keytable));
	for (unsigned int i = 0; i < LENGTH(colors); i++) {
		if (COLORS == 256) {
			if (colors[i].fg256)
				colors[i].fg = colors[i].fg256;
			if (colors[i].bg256)
				colors[i].bg = colors[i].bg256;
		}
		colors[i].pair = vt_color_reserve(colors[i].fg, colors[i].bg);
	}
	initpertag();
	resize_screen();
	struct sigaction sa;
	memset(&sa, 0, sizeof sa);
	sa.sa_flags = 0;
	sigemptyset(&sa.sa_mask);
	sa.sa_handler = sigwinch_handler;
	sigaction(SIGWINCH, &sa, NULL);
	sa.sa_handler = sigchld_handler;
	sigaction(SIGCHLD, &sa, NULL);
	sa.sa_handler = sigterm_handler;
	sigaction(SIGTERM, &sa, NULL);
	sa.sa_handler = SIG_IGN;
	sigaction(SIGPIPE, &sa, NULL);
	scheme_init();
}

static void
destroy(Client *c) {
	event_t evt;

	if (sel == c)
		focusnextnm(NULL);
	detach(c);
	detachstack(c);
	if (sel == c) {
		Client *next = nextvisible(clients);
		if (next) {
			focus(next);
			toggleminimize(NULL);
		} else {
			sel = NULL;
		}
	}
	if (lastsel == c)
		lastsel = NULL;
	werase(c->window);
	wnoutrefresh(c->window);
	vt_destroy(c->term);
	delwin(c->window);
	if (!clients && LENGTH(actions)) {
		if (!strcmp(c->cmd, shell))
			quit(NULL);
		else
			create(NULL);
	}

	evt.eid = EVT_WIN_DELETED;
	evt.oid = c->id;
	scheme_event_handle(evt);

	free(c);
	arrange();
}

static void
cleanup(void) {
	int i;
	scheme_uninit();
	while (clients)
		destroy(clients);
	vt_shutdown();
	endwin();
	free(copyreg.data);
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

static char *getcwd_by_pid(Client *c) {
	if (!c)
		return NULL;
	char buf[32];
	snprintf(buf, sizeof buf, "/proc/%d/cwd", c->pid);
	return realpath(buf, NULL);
}

static void
synctitle(Client *c)
{
	size_t len = sizeof(c->title);
	char buf[128];
	char path[64];
	size_t blen;
	char *eol;
	pid_t pid;
	int pty;
	int ret;
	int fd;

	pty = c->overlay ? vt_pty_get(c->overlay) : vt_pty_get(c->app);

	pid = tcgetpgrp(pty);
	if (pid == -1)
		return;

	snprintf(path, sizeof(path), "/proc/%d/cmdline", pid);

	fd = open(path, O_RDONLY);
	if (fd == -1)
		return;

	blen = MIN(sizeof(buf), sizeof(c->title));

	ret = read(fd, buf, blen);
	if (ret <= 0)
		goto done;

	buf[ret - 1] = '\0';

	strncpy(c->title, basename(buf), ret);

	settitle(c);
	if (!isarrange(fullscreen) || sel == c)
		draw_border(c);
done:
	close(fd);
}

int __create(const char *args[]) {
	const char *pargs[4] = { shell, NULL };
	char buf[8], *cwd = NULL;
	const char *env[] = {
		"DVTM_WINDOW_ID", buf,
		NULL
	};
	event_t evt;

	if (args && args[0]) {
		pargs[1] = "-c";
		pargs[2] = args[0];
		pargs[3] = NULL;
	}
	Client *c = calloc(1, sizeof(Client));
	if (!c)
		return -1;
	c->tags = tagset[seltags];
	c->id = ++cmdfifo.id;
	snprintf(buf, sizeof buf, "%d", c->id);

	if (!(c->window = newwin(wah, waw, way, wax))) {
		free(c);
		return -1;
	}

	c->term = c->app = vt_create(screen.h, screen.w, screen.history);
	if (!c->term) {
		delwin(c->window);
		free(c);
		return -1;
	}

	if (args && args[0]) {
		c->cmd = args[0];
		char name[PATH_MAX];
		strncpy(name, args[0], sizeof(name));
		name[sizeof(name)-1] = '\0';
		strncpy(c->title, basename(name), sizeof(c->title));
	} else {
		c->cmd = shell;
	}

	if (args && args[1])
		strncpy(c->title, args[1], sizeof(c->title));
	c->title[sizeof(c->title)-1] = '\0';

	if (strlen(c->title) == 0)
		c->sync_title = true;
	else
		c->sync_title = false;

	if (args && args[2])
		cwd = !strcmp(args[2], "$CWD") ? getcwd_by_pid(sel) : (char*)args[2];
	else if (strlen(pertag.cwd[pertag.curtag]))
		cwd = pertag.cwd[pertag.curtag];

	c->pid = vt_forkpty(c->term, shell, pargs, cwd, env, NULL, NULL);
	if (args && args[2] && !strcmp(args[2], "$CWD"))
		free(cwd);

	vt_data_set(c->term, c);
	vt_title_handler_set(c->term, term_title_handler);
	vt_urgent_handler_set(c->term, term_urgent_handler);
	applycolorrules(c);
	c->x = wax;
	c->y = way;
	debug("client with pid %d forked\n", c->pid);
	attach(c);
	focus(c);
	arrange();

	evt.eid = EVT_WIN_CREATED;
	evt.oid = c->id;
	scheme_event_handle(evt);
	return c->id;
}

static void create(const char *args[]) {
	__create(args);
}

static void
editor(const char *args[]) {
	const char *editor[3] = { NULL };

	editor[0] = getenv("DVTM_EDITOR");
	if (!editor[0])
		editor[0] = getenv("VISUAL");
	if (!editor[0])
		editor[0] = getenv("EDITOR");
	if (!editor[0])
		editor[0] = "vi";

	create(editor);
}

static void
copymode(const char *args[]) {
	if (!args || !args[0] || !sel || sel->overlay)
		return;

	bool colored = strstr(args[0], "pager") != NULL;

	if (!(sel->overlay = vt_create(sel->h - sel->has_title_line, sel->w, 0)))
		return;

	int *to = &sel->editor_fds[0];
	int *from = strstr(args[0], "editor") ? &sel->editor_fds[1] : NULL;
	sel->editor_fds[0] = sel->editor_fds[1] = -1;

	const char *argv[3] = { args[0], NULL, NULL };
	char argline[32];
	int line = vt_content_start(sel->app);
	snprintf(argline, sizeof(argline), "+%d", line);
	argv[1] = argline;

	if (vt_forkpty(sel->overlay, args[0], argv, NULL, NULL, to, from) < 0) {
		vt_destroy(sel->overlay);
		sel->overlay = NULL;
		return;
	}

	sel->term = sel->overlay;

	if (sel->editor_fds[0] != -1) {
		char *buf = NULL;
		size_t len = vt_content_get(sel->app, &buf, colored);
		char *cur = buf;
		while (len > 0) {
			ssize_t res = write(sel->editor_fds[0], cur, len);
			if (res < 0) {
				if (errno == EAGAIN || errno == EINTR)
					continue;
				break;
			}
			cur += res;
			len -= res;
		}
		free(buf);
		close(sel->editor_fds[0]);
		sel->editor_fds[0] = -1;
	}
	sel->is_editor = true;

	if (args[1])
		vt_write(sel->overlay, args[1], strlen(args[1]));
}

static void
focusn(const char *args[]) {
	for (Client *c = nextvisible(clients); c; c = nextvisible(c->next)) {
		if (c->order == atoi(args[0])) {
			focus(c);
			if (c->minimized)
				toggleminimize(NULL);
			return;
		}
	}
}

static void
__focusid(int win_id) {
	for (Client *c = clients; c; c = c->next) {
		if (c->id == win_id) {
			focus(c);
			if (c->minimized)
				toggleminimize(NULL);
			if (!isvisible(c)) {
				c->tags |= tagset[seltags];
				tagschanged();
			}
			return;
		}
	}
}

static void
focusnextnm(const char *args[]) {
	if (!sel)
		return;
	Client *c = sel;
	do {
		c = nextvisible(c->next);
		if (!c)
			c = nextvisible(clients);
	} while (c->minimized && c != sel);
	focus(c);
}

static void
focusprevnm(const char *args[]) {
	if (!sel)
		return;
	Client *c = sel;
	do {
		for (c = c->prev; c && !isvisible(c); c = c->prev);
		if (!c) {
			for (c = clients; c && c->next; c = c->next);
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
killclient(const char *args[]) {
	Client *target = sel;

	if (args && args[0]) {
		int order = atoi(args[0]);
		Client *c;

		for (c = nextvisible(clients); c; c = nextvisible(c->next)) {
			if (c->order == order) {
				target = c;
				break;
			}
		}
	}

	if (!target)
		return;

	debug("killing client with pid: %d\n", target->pid);
	kill(-target->pid, SIGKILL);
}

static void killother(const char *args[]) {
	unsigned int n;
	Client *c;

	for (n = 0, c = nextvisible(clients); c; c = nextvisible(c->next)) {
		if (ismastersticky(c) || sel == c)
			continue;
		kill(-c->pid, SIGKILL);
	}
}

static void
quit(const char *args[]) {
	cleanup();
	exit(EXIT_SUCCESS);
}

static void
redraw(const char *args[]) {
	for (Client *c = clients; c; c = c->next) {
		if (!c->minimized) {
			vt_dirty(c->term);
			wclear(c->window);
			wnoutrefresh(c->window);
		}
	}
	resize_screen();
}

static void
scrollback(const char *args[]) {
	if (!is_content_visible(sel))
		return;

	if (!args[0] || atoi(args[0]) < 0)
		vt_scroll(sel->term, -sel->h/2);
	else
		vt_scroll(sel->term,  sel->h/2);

	draw(sel);
	curs_set(vt_cursor_visible(sel->term));
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

	if (sel && isarrange(fullscreen)) {
		event_t evt;

		evt.eid = EVT_WIN_MAXIMIZED;
		evt.oid = sel->id;
		scheme_event_handle(evt);
	}
}

static void
togglemaximize(const char *args[]) {
	if (isarrange(fullscreen)) {
		layout = pertag.layout_prev[pertag.curtag];
		pertag.layout[pertag.curtag] = layout;
		arrange();
	} else {
		const char *maxargs[] = { "[ ]" };
		setlayout(maxargs);
	}
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
startup(const char *args[]) {
	for (unsigned int i = 0; i < LENGTH(actions); i++)
		actions[i].cmd(actions[i].args);
}

static void
togglebar(const char *args[]) {
	if (bar.pos == BAR_OFF)
		showbar();
	else
		hidebar();
	bar.autohide = false;
	updatebarpos();
	redraw(NULL);
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
	updatebarpos();
	redraw(NULL);
}

static void
toggleminimize(const char *args[]) {
	Client *c, *m, *t;
	unsigned int n;
	event_t evt;

	if (!sel)
		return;
	/* do not minimize sticked master */
	if (ismastersticky(sel))
		return;
	/* the last window can't be minimized */
	if (!sel->minimized) {
		for (n = 0, c = nextvisible(clients); c; c = nextvisible(c->next))
			if (!c->minimized)
				n++;
		if (n == 1)
			return;
	}
	sel->minimized = !sel->minimized;
	m = sel;
	/* check whether the master client was minimized */
	if (sel == nextvisible(clients) && sel->minimized) {
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
		for (c = nextvisible(clients); c && (t = nextvisible(c->next)) && !t->minimized; c = t);
		attachafter(m, c);
	} else { /* window is no longer minimized, move it to the master area */
		vt_dirty(m->term);
		detach(m);
		attach(m);
	}
	arrange();

	if (m->minimized) {
		evt.eid = EVT_WIN_MINIMIZED;
		evt.oid = c->id;
		scheme_event_handle(evt);
	}
}

static void minimizeother(const char *args[])
{
	unsigned int n;
	Client *c;

	for (n = 0, c = nextvisible(clients); c; c = nextvisible(c->next)) {
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
	Client *c;

	if (!sel)
		return;
	if (args && args[0])
		focusn(args);
	if ((c = sel) == nextvisible(clients))
		if (!(c = nextvisible(c->next)))
			return;
	detach(c);
	attachfirst(c);
	focus(c);
	if (c->minimized)
		toggleminimize(NULL);
	arrange();
}

/* commands for use by mouse bindings */
static void
mouse_focus(const char *args[]) {
	focus(msel);
	if (msel->minimized)
		toggleminimize(NULL);
}

static void
mouse_fullscreen(const char *args[]) {
	mouse_focus(NULL);
	setlayout(isarrange(fullscreen) ? NULL : args);
}

static void
mouse_minimize(const char *args[]) {
	focus(msel);
	toggleminimize(NULL);
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

static void setminimized(const char *args[]) {
	if (!args || !args[0] || !args[1])
		return;

	if (strcmp("align", args[0]) == 0) {
		if (strcmp("vert", args[1]) == 0)
			min_align = MIN_ALIGN_VERT;
		else if (strcmp("horiz", args[1]) == 0)
			min_align = MIN_ALIGN_HORIZ;
	}
}

static void
handle_mouse(void) {
#ifdef CONFIG_MOUSE
	MEVENT event;
	unsigned int i;
	if (getmouse(&event) != OK)
		return;
	msel = get_client_by_coord(event.x, event.y);

	if (!msel)
		return;

	debug("mouse x:%d y:%d cx:%d cy:%d mask:%d\n", event.x, event.y, event.x - msel->x, event.y - msel->y, event.bstate);

	vt_mouse(msel->term, event.x - msel->x, event.y - msel->y, event.bstate);

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

static void
handle_editor(Client *c) {
	event_t evt;

	if (!copyreg.data && (copyreg.data = malloc(screen.history)))
		copyreg.size = screen.history;
	copyreg.len = 0;
	while (c->editor_fds[1] != -1 && copyreg.len < copyreg.size) {
		ssize_t len = read(c->editor_fds[1], copyreg.data + copyreg.len, copyreg.size - copyreg.len);
		if (len == -1) {
			if (errno == EINTR)
				continue;
			break;
		}
		if (len == 0)
			break;
		copyreg.len += len;
		if (copyreg.len == copyreg.size) {
			copyreg.size *= 2;
			if (!(copyreg.data = realloc(copyreg.data, copyreg.size))) {
				copyreg.size = 0;
				copyreg.len = 0;
			}
		}
	}

	if (copyreg.len >= 2) {
		if (copyreg.data[copyreg.len - 2] == '\r')
			copyreg.len--;
		if (copyreg.data[copyreg.len - 1] == '\n')
			copyreg.len--;
	}

	evt.eid = EVT_WIN_COPIED;
	evt.oid = c->id;
	scheme_event_handle(evt);
}

static void
handle_overlay(Client *c) {
	if (c->is_editor)
		handle_editor(c);

	c->overlay_died = false;
	c->is_editor = false;
	c->editor_fds[1] = -1;
	vt_destroy(c->overlay);
	c->overlay = NULL;
	c->term = c->app;
	vt_dirty(c->term);
	draw_content(c);
	wnoutrefresh(c->window);
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
	return init;
}

int
main(int argc, char *argv[]) {
	KeyCombo keys;
	unsigned int key_index = 0;
	memset(keys, 0, sizeof(keys));
	sigset_t emptyset, blockset;

	setenv("DVTM", VERSION, 1);
	if (!parse_args(argc, argv)) {
		setup();
		startup(NULL);
	}

	sigemptyset(&emptyset);
	sigemptyset(&blockset);
	sigaddset(&blockset, SIGWINCH);
	sigaddset(&blockset, SIGCHLD);
	sigprocmask(SIG_BLOCK, &blockset, NULL);

	while (running) {
		int r, nfds = 0;
		fd_set rd;

		if (screen.need_resize) {
			resize_screen();
			screen.need_resize = false;
		}

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

		for (Client *c = clients; c; ) {
			if (c->overlay && c->overlay_died)
				handle_overlay(c);
			if (!c->overlay && c->died) {
				Client *t = c->next;
				destroy(c);
				c = t;
				continue;
			}
			int pty = c->overlay ? vt_pty_get(c->overlay) : vt_pty_get(c->app);
			FD_SET(pty, &rd);
			nfds = MAX(nfds, pty);
			c = c->next;
		}

		doupdate();
		r = pselect(nfds + 1, &rd, NULL, NULL, NULL, &emptyset);

		if (r < 0) {
			if (errno == EINTR)
				continue;
			perror("select()");
			exit(EXIT_FAILURE);
		}

		if (FD_ISSET(STDIN_FILENO, &rd)) {
			int code = getch();
rescan:
			if (code >= 0) {
				keys[key_index++] = code;
				KeyBinding *binding = NULL;
				if (code == KEY_MOUSE) {
					key_index = 0;
					handle_mouse();
				} else if ((binding = keybinding(keys, key_index))) {
					unsigned int key_length = MAX_KEYS;
					int alt_code;

					if (code == ALT) {
						nodelay(stdscr, TRUE);
						alt_code = getch();
						nodelay(stdscr, FALSE);
						if (alt_code >= 0)
							code = alt_code;
						goto rescan;
					}

					while (key_length > 1 && !binding->keys[key_length-1])
						key_length--;
					if (key_index == key_length) {
						binding->action.cmd(binding->action.args);
						key_index = 0;
						memset(keys, 0, sizeof(keys));
					}
				} else {
					key_index = 0;
					memset(keys, 0, sizeof(keys));
					keypress(code);
				}
			}
			if (r == 1) /* no data available on pty's */
				continue;
		}

		if (cmdfifo.fd != -1 && FD_ISSET(cmdfifo.fd, &rd))
			handle_cmdfifo();

		if (bar.fd != -1 && FD_ISSET(bar.fd, &rd))
			handle_statusbar();

		for (Client *c = clients; c; c = c->next) {
			if (FD_ISSET(vt_pty_get(c->term), &rd)) {
				if (vt_process(c->term) < 0 && errno == EIO) {
					if (c->overlay)
						c->overlay_died = true;
					else
						c->died = true;
					continue;
				}
			}

			if (is_content_visible(c)) {
				if (c->sync_title)
					synctitle(c);
				if (c != sel) {
					draw_content(c);
					wnoutrefresh(c->window);
				}
			} else if (!isarrange(fullscreen) && isvisible(c)
					&& c->minimized) {
				draw_border(c);
				wnoutrefresh(c->window);
			}
		}

		if (is_content_visible(sel)) {
			draw_content(sel);
			curs_set(vt_cursor_visible(sel->term));
			wnoutrefresh(sel->window);
		}
	}

	cleanup();
	return 0;
}

static Client *client_get_by_id(int id)
{
	for (Client *c = clients; c; c = c->next) {
		if (c->id == id)
			return c;
	}

	return NULL;
}

/* External API */
int win_get_by_coord(int x, int y)
{
	Client *c = get_client_by_coord(x, y);

	if (c)
		return c->id;

	return 0;
}

bool win_is_visible(int wid)
{
	Client *c = client_get_by_id(wid);

	if (c)
		return isvisible(c);

	return false;
}

int win_first_get(void)
{
	Client *c;

	for (c = clients; c && !isvisible(c); c = c->next);

	if (c && isvisible(c))
		return c->id;

	return 0;
}

int win_prev_get(int wid)
{
	Client *c = client_get_by_id(wid);
	Client *p;

	if (!c)
		return 0;

	for (p = c->prev; p && !isvisible(p); p = p->prev);

	if (p && isvisible(p))
		return p->id;

	return 0;
}

int win_next_get(int wid)
{
	Client *c = client_get_by_id(wid);
	Client *n;

	for (n = n->next; n && !isvisible(n); n = n->next);

	if (n && isvisible(n))
		return n->id;

	return 0;
}

int win_upper_get(int wid)
{
	Client *c = client_get_by_id(wid);
	Client *u;

	if (!c)
		return 0;

	/* avoid vertical separator, hence +1 in x direction */
	u = get_client_by_coord(c->x + 1, c->y - 1);
	if (u)
		return u->id;

	return 0;
}

int win_lower_get(int wid)
{
	Client *c = client_get_by_id(wid);
	Client *l;

	if (!c)
		return 0;

	l = get_client_by_coord(c->x, c->y + c->h);
	if (l)
		return l->id;

	return 0;
}

int win_left_get(int wid)
{
	Client *c = client_get_by_id(wid);
	Client *l;

	if (!c)
		return 0;

	l = get_client_by_coord(c->x - 2, c->y);
	if (l)
		return l->id;

	return 0;
}

int win_right_get(int wid)
{
	Client *c = client_get_by_id(wid);
	Client *r;

	if (!c)
		return 0;

	r = get_client_by_coord(c->x + c->w + 1, c->y);
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
	__focusid(wid);
	return 0;
}

int win_create(char *prog)
{
	const char *args[3] = {NULL};

	args[0] = prog;
	return __create(args);
}

void win_del(int wid)
{
	Client *c = client_get_by_id(wid);

	if (c)
		kill(-c->pid, SIGKILL);
		/* destroy(c); */
}

char *win_title_get(int wid)
{
	Client *c = client_get_by_id(wid);

	if (c)
		return c->title;

	return NULL;
}

int win_title_set(int wid, char *title)
{
	Client *c = client_get_by_id(wid);

	if (c) {
		strncpy(c->title, title, sizeof(c->title) - 1);
		c->sync_title = false;
		settitle(c);
		if (!isarrange(fullscreen))
			draw_border(c);
		applycolorrules(c);
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
	Client *c;

	if (!ntags)
		return -1;

	c = client_get_by_id(wid);
	if (!c)
		return -1;

	c->tags = ntags;
	tagschanged();
	return 0;
}

int win_tag_toggle(int wid, int tag)
{
	unsigned int ntags;
	Client *c;

	c = client_get_by_id(wid);
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
	Client *c;

	c = client_get_by_id(wid);
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
	Client *c;

	c = client_get_by_id(wid);
	if (!c)
		return -1;

	ntags = c->tags & ~tag_to_bit(tag);
	c->tags = ntags;
	tagschanged();
	return 0;
}

win_state_t win_state_get(int wid)
{
	Client *c = client_get_by_id(wid);

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
	Client *c, *orig;

	c = client_get_by_id(wid);
	if (!c)
		return -1;

	orig = sel;

	switch (st) {
	case WIN_STATE_MINIMIZED:
		if (!c->minimized) {
			win_current_set(wid);
			toggleminimize(NULL);
			/* switch to the original window */
			if (orig)
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
			toggleminimize(NULL);
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
	Client *c, *orig;

	c = client_get_by_id(wid);
	if (!c)
		return -1;

	orig = sel;

	switch (st) {
	case WIN_STATE_MINIMIZED:
		win_current_set(wid);
		toggleminimize(NULL);
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

int win_keys_send(int wid, char *keys)
{
	Client *c = client_get_by_id(wid);

	if (!c)
		return -1;

	vt_write(c->term, keys, strlen(keys));
	return 0;
}

int win_text_send(int wid, char *text)
{
	Client *c = client_get_by_id(wid);

	if (!c)
		return -1;

	vt_write(c->term, text, strlen(text));
	return 0;
}

int win_pager_mode(int wid)
{
	const char *args[] = {PROGNAME"-pager", NULL};
	Client *c = client_get_by_id(wid);
	Client *tmp = sel;

	if (!c)
		return -1;

	sel = c;
	copymode(args);
	sel = tmp;

	return 0;
}

int win_copy_mode(int wid)
{
	const char *args[] = {PROGNAME"-editor", NULL};
	Client *c = client_get_by_id(wid);
	Client *tmp = sel;

	if (!c)
		return -1;

	sel = c;
	copymode(args);
	sel = tmp;

	return 0;
}

char *win_capture(int wid)
{
	return NULL;
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
	if (isarrange(fullscreen) || isarrange(grid))
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
	if (isarrange(fullscreen) || isarrange(grid))
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
	pertag.msticky[tag] = is_sticky;
	draw_all();
	return 0;
}

static void bind_key_cmd(const char *args[]) {
	bind_key_cb_t cb = (bind_key_cb_t) args[0];

	cb();
}

static int parse_key(KeyBinding *key, char *str)
{
	char *tok_ptr = str;
	char tmp[60] = {0};
	char *tok;
	int i;

	strncpy(tmp, str, sizeof(tmp)-1);

	tok = strtok_r(tmp, " ", &tok_ptr);

	for (i = 0; i < MAX_KEYS && tok; i++) {
		if (strlen(tok) == 3 && tok[0] == 'C' && tok[1] == '-') {
			key->keys[i] = CTRL(tok[2]);
		} else if (strlen(tok) == 3 && tok[0] == 'M' && tok[1] == '-') {
			key->keys[i++] = ALT;
			key->keys[i] = tok[2];
		} else if (strcmp(tok, "<Space>") == 0) {
			key->keys[i] = ' ';
		} else if (strcmp(tok, "<Enter>") == 0) {
			key->keys[i] = '\r';
		} else if (strcmp(tok, "<Tab>") == 0) {
			key->keys[i] = '\t';
		} else {
			key->keys[i] = tok[0];
		}

		tok = strtok_r(NULL, " ", &tok_ptr);
	}

	return 0;
}

static KeyBinding *find_key(char *str)
{
	KeyBinding *it = NULL, key;
	int i, j;

	parse_key(&key, str);

	for (i = 0; i < scmkeybn; i++) {
		it = &scmkeyb[i];

		for (j = 0; j < MAX_KEYS; j++) {
			if (it->keys[j] != key.keys[j]) {
				it = NULL;
				break;
			}
		}

		if (it)
			break;
	}

	return it;
}

int bind_key(char *map, bind_key_cb_t cb)
{
	KeyBinding *key;

	key = find_key(map);
	if (!key) {
		scmkeybn++;
		scmkeyb = realloc(scmkeyb, sizeof(*scmkeyb) * scmkeybn);
		if (!scmkeyb)
			error("fail on scmkeyb realloc\n");

		key = &scmkeyb[scmkeybn - 1];
		memset(key, 0, sizeof(*key));

		parse_key(key, map);
	}

	key->action.args[0] = (const char *) cb;
	key->action.cmd = bind_key_cmd;

	return 0;
}

int unbind_key(char *map)
{
	KeyBinding key, *it = NULL, tmp;
	int i, j;

	parse_key(&key, map);

	it = find_key(map);
	if (it) {
		tmp = scmkeyb[scmkeybn-1];
		scmkeyb[scmkeybn-1] = *it;
		*it = tmp;
	}

	scmkeybn--;
	scmkeyb = realloc(scmkeyb, sizeof(*scmkeyb) * scmkeybn);
	if (!scmkeyb)
		error("fail on scmkeyb realloc\n");

	return 0;
}

char *copy_buf_get(size_t *len)
{
	*len = copyreg.len;

	if (!copyreg.len)
		return NULL;

	return copyreg.data;
}

int copy_buf_set(char *str)
{
	size_t len;

	if (str) {
		len = strlen(str);

		if (copyreg.size < len) {
			copyreg.data = realloc(copyreg.data, len * 2);
			copyreg.size = len * 2;
			copyreg.len = len;
		}

		memcpy(copyreg.data, str, len);

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

	setenv("DVTM_CMD_FIFO", cmd, 1);
	setenv("DVTM_RET_FIFO", ret, 1);

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
	updatebarpos();
	redraw(NULL);
	return 0;
}

void do_quit(void)
{
	quit(NULL);
}
