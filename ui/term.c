#include <stdlib.h>
#include <curses.h>
#include <limits.h>

#include "ui/ui.h"

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

	if (fg >= COLORS)
		fg = (t ? t->win.deffg : default_fg);
	if (bg >= COLORS)
		bg = (t ? t->win.defbg : default_bg);

	if (!has_default_colors) {
		if (fg == -1)
			fg = (t && t->win.deffg != -1 ? t->win.deffg : default_fg);
		if (bg == -1)
			bg = (t && t->win.defbg != -1 ? t->win.defbg : default_bg);
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

static int term_init(Ui *ui)
{
	UiTerm *tui = (UiTerm*)ui;

	initscr();
	start_color();
	noecho();
	nonl();
	keypad(stdscr, TRUE);
	raw();

	term_init_colors(ui);

	return 0;
}

static void term_free(Ui *ui)
{
	endwin();
	free(ui);
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

	twin->win.deffg = default_fg;
	twin->win.defbg = default_bg;
	twin->cur_x = 0;
	twin->cur_y = 1;

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

void term_window_text_color_set(UiWin *win, short fg, short bg)
{
	WinTerm *twin = (WinTerm*)win;

	wcolor_set(twin->cwin, term_window_color_get(win, fg, bg), NULL);
}

void term_window_text_attr_set(UiWin *win, unsigned attrs)
{
	WinTerm *twin = (WinTerm*)win;

	wattrset(twin->cwin, attrs);
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

static void term_window_cursor_set(UiWin *win, int x, int y)
{
	WinTerm *twin = (WinTerm*)win;

	wmove(twin->cwin, y, x);
	twin->cur_x = x;
	twin->cur_y = y;
	wnoutrefresh(twin->cwin);
}

static void term_window_cursor_get(UiWin *win, int *x, int *y)
{
	WinTerm *twin = (WinTerm*)win;

	*x = twin->cur_x;
	*y = twin->cur_y;
}

static void term_window_draw(UiWin *win)
{
	const Line *line = view_lines_first(win->view);
	int view_width = view_width_get(win->view);
	WinTerm *twin = (WinTerm*)win;
	int x=0, y = 1; /* TODO: consider if to show title */
	int sx, sy;

	getyx(twin->cwin, sy, sx);
	wmove(twin->cwin, y, x);
	wclrtobot(twin->cwin);

	term_window_text_attr_set(win, term_color_make(win->ui, twin->win.deffg, twin->win.defbg));

	for (const Line *l = line; l; l = l->next, y++) {
		for (x = 0; x < view_width; x++) {
			Cell *c = &l->cells[x];

			wmove(twin->cwin, y, x);
			waddnstr(twin->cwin, c->data, c->len);
		}
	}

	wmove(twin->cwin, sy, sx);
	wnoutrefresh(twin->cwin);
}

static void term_window_refresh(UiWin *win)
{
	WinTerm *twin = (WinTerm*)win;

	wnoutrefresh(twin->cwin);
}

static void term_window_redraw(UiWin *win)
{
	WinTerm *twin = (WinTerm*)win;

	wclear(twin->cwin);
	wnoutrefresh(twin->cwin);
}

Ui *ui_term_new(void)
{
	UiTerm *tui;

	tui = calloc(1, sizeof(*tui));

	tui->ui.init = term_init;
	tui->ui.free = term_free;
	tui->ui.color_make = term_color_make;
	tui->ui.draw_char = term_draw_char;
	tui->ui.draw_char_vert = term_draw_char_vert;
	tui->ui.window_new = term_window_new;
	tui->ui.window_free = term_window_free;
	tui->ui.window_cursor_set = term_window_cursor_set;
	tui->ui.window_cursor_get = term_window_cursor_get;
	tui->ui.window_draw = term_window_draw;
	tui->ui.window_redraw = term_window_redraw;
	tui->ui.window_refresh = term_window_refresh;
	tui->ui.window_resize = term_window_resize;
	tui->ui.window_move = term_window_move;
	tui->ui.window_text_color_set = term_window_text_color_set;
	tui->ui.window_text_attr_set = term_window_text_attr_set;
	tui->ui.window_draw_char = term_window_draw_char;
	tui->ui.window_draw_text = term_window_draw_text;

	return (Ui *)tui;
}
