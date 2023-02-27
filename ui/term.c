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
	WINDOW *cwin;
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

static short term_window_color_get(UiWin *win, short fg, short bg)
{
	WinTerm *t = (WinTerm*)win;
	short curr_fg = term_color2curses(ui_window_text_fg_get(win));
	short curr_bg = term_color2curses(ui_window_text_bg_get(win));

	if (fg >= COLORS)
		fg = (t ? curr_fg : default_fg);
	if (bg >= COLORS)
		bg = (t ? curr_bg : default_bg);

	if (!has_default_colors) {
		if (fg == -1)
			fg = (t && curr_fg != -1 ? curr_fg : default_fg);
		if (bg == -1)
			bg = (t && curr_bg != -1 ? curr_bg : default_bg);
	}

	if (!color2palette || (fg == -1 && bg == -1))
		return 0;
	unsigned int index = term_color_hash(fg, bg);
	if (color2palette[index] == 0) {
		short oldfg, oldbg;
		for (;;) {
			if (++color_pair_current >= color_pairs_max)
				color_pair_current = color_pairs_reserved + 1;
			pair_content(color_pair_current, &oldfg, &oldbg);
			unsigned int old_index = term_color_hash(oldfg, oldbg);
			if (color2palette[old_index] >= 0) {
				if (init_pair(color_pair_current, fg, bg) == OK) {
					color2palette[old_index] = 0;
					color2palette[index] = color_pair_current;
				}
				break;
			}
		}
	}

	short color_pair = color2palette[index];
	return color_pair >= 0 ? color_pair : -color_pair;
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
	event_process();
}

static void term_refresh(Ui *ui)
{
	wnoutrefresh(stdscr);
}

static void term_draw_char(Ui *ui, int x, int y, unsigned int ch, int n)
{
	mvhline(y, x, ch, n);
}

static void term_draw_char_vert(Ui *ui, int x, int y, unsigned int ch, int n)
{
	mvvline(y, x, ch, n);
}

static UiWin *term_window_new(Ui *ui, View *view)
{
	WinTerm *twin;

	twin = calloc(1, sizeof(*twin));

	twin->cwin = newwin(0, 0, 0, 0);
	if (!twin->cwin) {
		free(twin);
		return NULL;
	}

	ui_window_text_fg_set(&twin->win, default_fg);
	ui_window_text_bg_set(&twin->win, default_bg);

	return (UiWin*)twin;
}

static void term_window_free(UiWin *win)
{
	WinTerm *twin = (WinTerm*)win;

	werase(twin->cwin);
	wnoutrefresh(twin->cwin);
	delwin(twin->cwin);
	free(twin);
}

static void term_window_resize(UiWin *win, int w, int h)
{
	WinTerm *twin = (WinTerm*)win;

	wresize(twin->cwin, h, w);
}

static void term_window_move(UiWin *win, int x, int y)
{
	WinTerm *twin = (WinTerm*)win;

	mvwin(twin->cwin, y, x);
}

void term_window_draw_char(UiWin *win, int x, int y, unsigned int ch, int n)
{
	WinTerm *twin = (WinTerm*)win;

	mvwhline(twin->cwin, y, x, ch, n);
}

void term_window_draw_text(UiWin *win, int x, int y, const char *text, int n)
{
	WinTerm *twin = (WinTerm*)win;

	mvwaddnstr(twin->cwin, y, x, text, n);
}

void term_window_draw_char_attr(UiWin *win, int x, int y, unsigned ch, int n,
				short fg, short bg, ui_text_style_t style)
{
	WinTerm *twin = (WinTerm*)win;
	unsigned tmp_attr = ui_window_text_style_get(win);
	short tmp_fg = ui_window_text_fg_get(win);
	short tmp_bg = ui_window_text_bg_get(win);

	wattrset(twin->cwin, term_style2attr(style));
	wcolor_set(twin->cwin,
		   term_color_make(win->ui, term_color2curses(fg),
			   term_color2curses(bg)),
		   NULL);

	mvwhline(twin->cwin, y, x, ch, n);

	wattrset(twin->cwin, term_style2curses(tmp_attr));
	wcolor_set(twin->cwin,
		   term_color_make(win->ui, term_color2curses(tmp_fg),
			   term_color2curses(tmp_bg)),
		   NULL);
}

void term_window_draw_text_attr(UiWin *win, int x, int y, const char *text, int n,
				short fg, short bg, ui_text_style_t style)
{
	WinTerm *twin = (WinTerm*)win;
	unsigned tmp_attr = ui_window_text_style_get(win);
	short tmp_fg = ui_window_text_fg_get(win);
	short tmp_bg = ui_window_text_bg_get(win);

	wattrset(twin->cwin, term_style2attr(style));
	wcolor_set(twin->cwin,
		   term_color_make(win->ui, term_color2curses(fg),
				   term_color2curses(bg)),
		   NULL);

	mvwaddnstr(twin->cwin, y, x, text, n);

	wattrset(twin->cwin, term_style2curses(tmp_attr));
	wcolor_set(twin->cwin,
		   term_color_make(win->ui, tmp_fg, tmp_bg),
		   NULL);
	/* TODO: better to call it via separate ui_ api call */
	wnoutrefresh(twin->cwin);
}

static void term_window_draw(UiWin *win)
{
	const Line *line = view_lines_first(win->view);
	int view_width = view_width_get(win->view);
	WinTerm *twin = (WinTerm*)win;
	int sidebar = ui_window_sidebar_width_get(win);
	int x0 = win->has_border + sidebar;
	int y = 0;
	int curs_x, curs_y;
	int sx, sy;

	getyx(twin->cwin, sy, sx);
	wmove(twin->cwin, y, x0);

	ui_window_cursor_get(win, &curs_x, &curs_y);

	for (const Line *l = line; l; l = l->next, y++) {
		for (int cx = 0, x = x0; cx < view_width; cx++,x++) {
			Cell c = l->cells[cx];

			if (!c.len) {
				c.data[0] = ' ';
				c.len = 1;
			}

			if (cx == curs_x && y == curs_y &&
				ui_window_is_focused(win) && !ui_window_is_cursor_disabled(win)) {
				c.style.fg = default_bg;
				c.style.bg = default_fg;
			}
			/* wattrset(twin->cwin, term_style2attr(c->style.attr)); */

			/* wcolor_set(twin->cwin, term_window_color_get(win, */
			/* 			term_color2curses(c->style.fg), */
			/* 			term_color2curses(c->style.bg)), */
			/* 		NULL); */
			wattrset(twin->cwin, term_style2attr(c.style.attr));
			wcolor_set(twin->cwin,
					term_color_make(win->ui, term_color2curses(c.style.fg),
						term_color2curses(c.style.bg)),
					NULL);

			wmove(twin->cwin, y, x);
			waddnstr(twin->cwin, c.data, c.len);
		}
	}

	if (win->has_border) {
		mvwhline(twin->cwin, 0, 1, ACS_HLINE, ui_window_width_get(win)-2);
		mvwhline(twin->cwin, 0, 0, ACS_ULCORNER, 1);
		mvwhline(twin->cwin, 0, ui_window_width_get(win)-1, ACS_URCORNER, 1);
		mvwvline(twin->cwin, 1, 0, ACS_VLINE, ui_window_height_get(win)-1);
		mvwvline(twin->cwin, 1, ui_window_width_get(win)-1, ACS_VLINE,
				ui_window_height_get(win)-1);
	}

	wattrset(twin->cwin, term_style2attr(ui_window_text_style_get(win)));
	wcolor_set(twin->cwin, term_color_make(win->ui,
			term_color2curses(ui_window_text_fg_get(win)),
			term_color2curses(ui_window_text_bg_get(win))),
			NULL);

	wmove(twin->cwin, sy, sx);
	wnoutrefresh(twin->cwin);
}

static void term_window_refresh(UiWin *win)
{
	WinTerm *twin = (WinTerm*)win;

	wnoutrefresh(twin->cwin);
}

static void term_window_clear(UiWin *win)
{
	int sidebar = ui_window_sidebar_width_get(win);
	WinTerm *twin = (WinTerm*)win;

	if (sidebar) {
		int y = win->has_border;
		char ch = ' ';
		int sx, sy;

		getyx(twin->cwin, sy, sx);

		for (; y < ui_window_height_get(win) - 1; y++) {
			mvwhline(twin->cwin, y, 0, ch, sidebar);
		}

		wmove(twin->cwin, sy, sx);
		wnoutrefresh(twin->cwin);
	}
}

static void term_window_redraw(UiWin *win)
{
	WinTerm *twin = (WinTerm*)win;

	redrawwin(twin->cwin);
}

Ui *ui_term_new(void)
{
	UiTerm *tui;

	tui = calloc(1, sizeof(*tui));

	tui->ui.init = term_init;
	tui->ui.free = term_free;
	tui->ui.height_get = term_height_get;
	tui->ui.width_get = term_width_get;
	tui->ui.redraw = term_redraw;
	tui->ui.resize = term_resize;
	tui->ui.clear = term_clear;
	tui->ui.update = term_update;
	tui->ui.event_process = term_event_process;
	tui->ui.refresh = term_refresh;
	tui->ui.color_make = term_color_make;
	tui->ui.colors_max_get = term_colors_max_get;
	tui->ui.draw_char = term_draw_char;
	tui->ui.draw_char_vert = term_draw_char_vert;
	tui->ui.window_new = term_window_new;
	tui->ui.window_free = term_window_free;
	tui->ui.window_draw = term_window_draw;
	tui->ui.window_redraw = term_window_redraw;
	tui->ui.window_refresh = term_window_refresh;
	tui->ui.window_refresh = term_window_draw;
	tui->ui.window_clear = term_window_clear;
	tui->ui.window_resize = term_window_resize;
	tui->ui.window_move = term_window_move;
	tui->ui.window_draw_text = term_window_draw_text;
	tui->ui.window_draw_char_attr = term_window_draw_char_attr;
	tui->ui.window_draw_text_attr = term_window_draw_text_attr;

	return (Ui *)tui;
}
