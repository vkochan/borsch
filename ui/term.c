#include <stdlib.h>
#include <curses.h>
#include <limits.h>
#include <signal.h>
#include <string.h>
#include <sys/ioctl.h>

#include "keymap.h"
#include "event.h"
#include "ui/ui.h"

#define ALT	27

#define MIN(x, y) ((x) < (y) ? (x) : (y))

#ifndef NCURSES_ATTR_SHIFT
# define NCURSES_ATTR_SHIFT 8
#endif

#ifndef NCURSES_ACS
# ifdef PDCURSES
#  define NCURSES_ACS(c) (acs_map[(unsigned char)(c)])
# else /* BSD curses */
#  define NCURSES_ACS(c) (_acs_map[(unsigned char)(c)])
# endif
#endif

#ifdef NCURSES_VERSION
# ifndef NCURSES_EXT_COLORS
#  define NCURSES_EXT_COLORS 0
# endif
# if !NCURSES_EXT_COLORS
#  define MAX_COLOR_PAIRS MIN(COLOR_PAIRS, 256)
# endif
#endif
#ifndef MAX_COLOR_PAIRS
# define MAX_COLOR_PAIRS COLOR_PAIRS
#endif

typedef struct {
	Ui ui;
} UiTerm;

typedef struct {
	UiWin win;
	int cur_x, cur_y;
} WinTerm;

static bool is_utf8, has_default_colors;
static short color_pairs_reserved, color_pairs_max, color_pair_current;
static short *color2palette, default_fg, default_bg;
static volatile int scr_height, scr_width;
volatile sig_atomic_t need_resize;

static short term_color2curses(short color)
{
	return color;
}

static unsigned term_style2curses(ui_text_style_t style)
{
	switch (style) {
	case UI_TEXT_STYLE_NORMAL: return A_NORMAL;
	case UI_TEXT_STYLE_BOLD: return A_BOLD;
	case UI_TEXT_STYLE_DIM: return A_DIM;
	case UI_TEXT_STYLE_ITALIC: return A_ITALIC;
	case UI_TEXT_STYLE_UNDERLINE: return A_UNDERLINE;
	case UI_TEXT_STYLE_BLINK: return A_BLINK;
	case UI_TEXT_STYLE_REVERSE: return A_REVERSE;
	case UI_TEXT_STYLE_INVIS: return A_INVIS;
	}

	return 0;
}

unsigned term_style2attr(ui_text_style_t style)
{
	unsigned attr = 0;
	int i;

	for (i = 0; i < UI_TEXT_STYLE_MAX; i++) {
		int bit = (1 << i);
		if (bit & style)
			attr |= term_style2curses(bit);
	}

	return attr;
}

static void term_sigwinch_handler(int sig) {
	need_resize = true;
	struct winsize ws;

	if (ioctl(0, TIOCGWINSZ, &ws) == -1) {
		getmaxyx(stdscr, scr_height, scr_width);
	} else {
		scr_width = ws.ws_col;
		scr_height = ws.ws_row;
	}

	resize_term(scr_height, scr_width);
	wrefresh(curscr);
}

static int term_height_get(Ui *ui)
{
	return scr_height;
}

static int term_width_get(Ui *ui)
{
	return scr_width;
}

static void term_redraw(Ui *ui) {
	struct winsize ws;

	if (ioctl(0, TIOCGWINSZ, &ws) == -1) {
		getmaxyx(stdscr, scr_height, scr_width);
	} else {
		scr_width = ws.ws_col;
		scr_height = ws.ws_row;
	}

	resizeterm(scr_height, scr_width);
	wresize(stdscr, scr_height, scr_width);
	clear();
}

static unsigned int term_color_hash(short fg, short bg)
{
	if (fg == -1)
		fg = COLORS;
	if (bg == -1)
		bg = COLORS + 1;
	return fg * (COLORS + 2) + bg;
}

short term_color_make(Ui *ui, short fg, short bg)
{
	if (!color2palette || fg >= COLORS || bg >= COLORS)
		return 0;
	if (!has_default_colors && fg == -1)
		fg = default_fg;
	if (!has_default_colors && bg == -1)
		bg = default_bg;
	if (fg == -1 && bg == -1)
		return 0;
	unsigned int index = term_color_hash(fg, bg);
	if (color2palette[index] >= 0) {
		if (init_pair(color_pairs_reserved + 1, fg, bg) == OK)
			color2palette[index] = -(++color_pairs_reserved);
	}
	short color_pair = color2palette[index];
	return color_pair >= 0 ? color_pair : -color_pair;
}

short term_colors_max_get(Ui *ui)
{
	return COLORS;
}

static void term_init_colors(Ui *ui)
{
	pair_content(0, &default_fg, &default_bg);
	if (default_fg == -1)
		default_fg = COLOR_WHITE;
	if (default_bg == -1)
		default_bg = COLOR_BLACK;
	has_default_colors = (use_default_colors() == OK);
	color_pairs_max = MIN(MAX_COLOR_PAIRS, SHRT_MAX);
	if (COLORS)
		color2palette = calloc((COLORS + 2) * (COLORS + 2), sizeof(short));
	/*
	 * XXX: On undefined color-pairs NetBSD curses pair_content() set fg
	 *      and bg to default colors while ncurses set them respectively to
	 *      0 and 0. Initialize all color-pairs in order to have consistent
	 *      behaviour despite the implementation used.
	 */
	for (short i = 1; i < color_pairs_max; i++)
		init_pair(i, 0, 0);
	term_color_make(ui, COLOR_WHITE, COLOR_BLACK);
}

void term_handle_keypress(int fd, void *arg)
{
	Ui *ui = arg;
	int code = getch();
	int alt_code;
	int flags = 0;
	KeyCode key = {};

	if (code == ALT) {
		nodelay(stdscr, TRUE);
		alt_code = getch();
		nodelay(stdscr, FALSE);
		if (alt_code > 0) {
			flags |= KEY_MOD_F_ALT;
			code = alt_code;
		}
	} else if (code < 0x1f && code != 0xd && code != 0x9) {
		flags |= KEY_MOD_F_CTL;
		code = code + 0x60;
	}

	key.flags = flags;
	key.code = code;

	if (ui->event_handler_cb)
		ui->event_handler_cb(ui, UiEventType_KeyPress, &key);
}

static int term_init(Ui *ui)
{
	UiTerm *tui = (UiTerm*)ui;
	struct sigaction sa;

	initscr();
	start_color();
	noecho();
	nonl();
	keypad(stdscr, TRUE);
	raw();
	curs_set(false);

	term_init_colors(ui);

	memset(&sa, 0, sizeof sa);
	sa.sa_flags = 0;
	sigemptyset(&sa.sa_mask);
	sa.sa_handler = term_sigwinch_handler;
	sigaction(SIGWINCH, &sa, NULL);

	term_redraw(ui);

	event_fd_handler_register(STDIN_FILENO, term_handle_keypress, ui);
	return 0;
}

static void term_free(Ui *ui)
{
	event_fd_handler_unregister(STDIN_FILENO);
	endwin();
	free(ui);
}

static bool term_resize(Ui *ui)
{
	bool do_resize = need_resize;

	if (need_resize) {
		need_resize = false;
	}

        return do_resize;
}

static void term_clear(Ui *ui)
{
	erase();
}

static void term_update(Ui *ui)
{
	doupdate();
}

static void term_event_process(Ui *ui)
{
	doupdate();
	event_process(NULL);
}

static void term_refresh(Ui *ui)
{
	wnoutrefresh(stdscr);
}

static void term_draw_wchar(Ui *ui, int x, int y, wchar_t ch, short fg, short bg, ui_text_style_t style)
{
	attrset(term_style2attr(style));
	color_set(term_color_make(ui, term_color2curses(fg), term_color2curses(bg)), NULL);

	mvaddnwstr(y, x, &ch, 1);

	attrset(A_NORMAL);
	color_set(term_color_make(ui, default_fg, default_bg), NULL);
}

static void term_draw_cell(Ui *ui, int x, int y, Cell *c)
{
	attrset(term_style2attr(c->style.attr));
	color_set(term_color_make(ui, term_color2curses(c->style.fg), term_color2curses(c->style.bg)), NULL);

	mvaddnstr(y, x, c->data, c->len);

	attrset(A_NORMAL);
	color_set(term_color_make(ui, default_fg, default_bg), NULL);
}

static UiWin *term_window_new(Ui *ui, View *view)
{
	WinTerm *twin;

	twin = calloc(1, sizeof(*twin));

	ui_window_text_fg_set(&twin->win, default_fg);
	ui_window_text_bg_set(&twin->win, default_bg);

	return (UiWin*)twin;
}

static void term_window_free(UiWin *win)
{
	WinTerm *twin = (WinTerm*)win;

	free(twin);
}

Ui *ui_term_new(void)
{
	UiTerm *tui;

	tui = calloc(1, sizeof(*tui));

	tui->ui.init = term_init;
	tui->ui.free = term_free;
	tui->ui.height_get = term_height_get;
	tui->ui.width_get = term_width_get;
	tui->ui.resize = term_resize;
	tui->ui.clear = term_clear;
	tui->ui.update = term_update;
	tui->ui.event_process = term_event_process;
	tui->ui.refresh = term_refresh;
	tui->ui.colors_max_get = term_colors_max_get;
	tui->ui.draw_wchar = term_draw_wchar;
	tui->ui.draw_cell = term_draw_cell;
	tui->ui.window_new = term_window_new;
	tui->ui.window_free = term_window_free;

	return (Ui *)tui;
}
