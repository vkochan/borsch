#include <unistd.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <curses.h>

#include "process.h"
#include "vt.h"
#include "common.h"
#include "window.h"
#include "buffer.h"
#include "view.h"
#include "ui/ui.h"
#include "xstr.h"

static unsigned int waw, wah, wax, way;
static bool layout_needs_arrange = false;
static unsigned int curr_tab;
static Tab tabs[MAXTABS + 1];
static char *title;
static Ui *ui;

static void layout_tiled(unsigned int wax, unsigned int way, unsigned int waw, unsigned int wah);
static void layout_grid(unsigned int wax, unsigned int way, unsigned int waw, unsigned int wah);
static void layout_bstack(unsigned int wax, unsigned int way, unsigned int waw, unsigned int wah);
static void layout_fullscreen(unsigned int wax, unsigned int way, unsigned int waw, unsigned int wah);

/* by default the first layout entry is used */
static Layout layouts[] = {
	{ LAYOUT_TILED,     "[]=", layout_tiled },
	{ LAYOUT_GRID,      "+++", layout_grid },
	{ LAYOUT_BSTACK,    "TTT", layout_bstack },
	{ LAYOUT_MAXIMIZED, "[ ]", layout_fullscreen },
};

static void layout_tiled(unsigned int wax, unsigned int way, unsigned int waw, unsigned int wah)
{
	unsigned int lax = wax, lay = way-1, law = waw, lah = wah;
	unsigned int i = 0, n = 0, nx, ny, nw, nh, m, mw, mh, th;
	Window *c;

	for_each_window(c)
		n++;

	m  = MAX(1, MIN(n, layout_current_nmaster()));
	mw = n == m ? waw : layout_current_fmaster() * waw;
	mh = wah / m;
	th = n == m ? 0 : wah / (n - m);
	nx = lax;
	ny = lay;

	for_each_window(c) {
		if (i < m) {	/* master */
			nw = mw;
			nh = (i < m - 1) ? mh : (lay + wah) - ny;
		} else {	/* tile window */
			if (i == m) {
				ny = lay;
				nx += mw;
				ui_draw_char_vert(ui, nx, ny, ACS_VLINE, wah);
				ui_draw_char(ui, nx, ny, ACS_TTEE, 1);
				nx++;
				nw = waw - mw -1;
			}
			nh = (i < n - 1) ? th : (lay + wah) - ny;
			if (i > m)
				ui_draw_char(ui, nx - 1, ny, ACS_LTEE, 1);
		}
		window_move_resize(c, nx, ny+(way-lay), nw, nh);
		ny += nh;
		i++;
	}

	/* Fill in nmaster intersections */
	if (n > m) {
		ny = lay + mh;
		for (i = 1; i < m; i++) {
			ui_draw_char(ui, nx - 1, ny, ((ny - 1) % th ? ACS_RTEE : ACS_PLUS), 1);
			ny += mh;
		}
	}
}

static void layout_grid(unsigned int wax, unsigned int way, unsigned int waw, unsigned int wah)
{
	unsigned int lax = wax, lay = way-1, law = waw, lah = wah;
	unsigned int i = 0, n = 0, nx, ny, nw, nh, aw, ah, cols, rows;
	Window *c;

	for_each_window(c)
		n++;
	
	/* grid dimensions */
	for (cols = 0; cols <= n / 2; cols++)
		if (cols * cols >= n)
			break;
	rows = (cols && (cols - 1) * cols >= n) ? cols - 1 : cols;
	/* window geoms (cell height/width) */
	nh = lah / (rows ? rows : 1);
	nw = law / (cols ? cols : 1);
	for_each_window(c) {
		/* if there are less windows in the last row than normal adjust the
		 * split rate to fill the empty space */
		if (rows > 1 && i == (rows * cols) - cols && (n - i) <= (n % cols))
			nw = law / (n - i);
		nx = (i % cols) * nw + lax;
		ny = (i / cols) * nh + lay;
		/* adjust height/width of last row/column's windows */
		ah = (i >= cols * (rows - 1)) ? lah - nh * rows : 0;
		/* special case if there are less windows in the last row */
		if (rows > 1 && i == n - 1 && (n - i) < (n % cols))
			/* (n % cols) == number of windows in the last row */
			aw = law - nw * (n % cols);
		else
			aw = ((i + 1) % cols == 0) ? law - nw * cols : 0;
		if (i % cols) {
			ui_draw_char_vert(ui, nx, ny, ACS_VLINE, nh + ah);
			/* if we are on the first row, or on the last one and there are fewer windows
			 * than normal whose border does not match the line above, print a top tree char
			 * otherwise a plus sign. */
			if (i <= cols
			    || (i >= rows * cols - cols && n % cols
				&& (cols - (n % cols)) % 2))
				ui_draw_char(ui, nx, ny, ACS_TTEE, 1);
			else
				ui_draw_char(ui, nx, ny, ACS_PLUS, 1);
			nx++, aw--;
		}
		window_move_resize(c, nx, ny+(way-lay), nw + aw, nh + ah);
		i++;
	}
}

static void layout_bstack(unsigned int wax, unsigned int way, unsigned int waw, unsigned int wah)
{
	unsigned int lax = wax, lay = way-1, law = waw, lah = wah;
	unsigned int i = 0, n = 0, nx, ny, nw, nh, m, mw, mh, tw;
	Window *c;

	for_each_window(c)
		n++;

	m  = MAX(1, MIN(n, layout_current_nmaster()));
	mh = n == m ? lah : layout_current_fmaster() * lah;
	mw = law / m;
	tw = n == m ? 0 : law / (n - m);
	nx = lax;
	ny = lay;

	for_each_window(c) {
		if (i < m) {	/* master */
			if (i > 0) {
				ui_draw_char_vert(ui, nx, ny, ACS_VLINE, nh);
				ui_draw_char(ui, nx, ny, ACS_TTEE, 1);
				nx++;
			}
			nh = mh;
			nw = (i < m - 1) ? mw : (lax + law) - nx;
		} else {	/* tile window */
			if (i == m) {
				nx = lax;
				ny += mh;
				nh = (lay + lah) - ny;
			}
			if (i > m) {
				ui_draw_char_vert(ui, nx, ny, ACS_VLINE, nh);
				ui_draw_char(ui, nx, ny, ACS_TTEE, 1);
				nx++;
			}
			nw = (i < n - 1) ? tw : (lax + law) - nx;
		}
		window_move_resize(c, nx, ny+(way-lay), nw, nh);
		nx += nw;
		i++;
	}

	/* Fill in nmaster intersections */
	if (n > m) {
		nx = lax;
		for (i = 0; i < m; i++) {
			if (i > 0) {
				ui_draw_char(ui, nx, ny, ACS_PLUS, 1);
				nx++;
			}
			nw = (i < m - 1) ? mw : (lax + law) - nx;
			nx += nw;
		}
	}
}

static void layout_fullscreen(unsigned int wax, unsigned int way, unsigned int waw, unsigned int wah)
{
	Window *c;
	for_each_window(c)
		window_move_resize(c, wax, way, waw, wah);
}

Layout *layout_get(int id)
{
	return &layouts[id];
}

Layout *layout_current(void)
{
	return frame_current()->layout;
}

bool layout_is_changed(void)
{
	return layout_needs_arrange;
}

void layout_changed(bool changed)
{
	layout_needs_arrange = changed;
}

bool layout_is_arrange(int id)
{
	return layout_current()->id == id;
}

layout_t layout_current_get(int tab)
{
	return tab_get(tab)->f->layout->id;
}

int layout_current_set(int tab, layout_t lay)
{
	if (window_popup_get())
		return -1;

	frame_current()->layout_prev = frame_current()->layout;
	frame_current()->layout = layout_get(lay);
	layout_changed(true);
}

int layout_current_nmaster(void)
{
	return frame_current()->nmaster;
}

float layout_current_fmaster(void)
{
	return frame_current()->mfact;
}

unsigned int layout_current_x(void)
{
	return wax;
}

unsigned int layout_current_y(void)
{
	return way;
}

void layout_current_move(unsigned int x, unsigned y)
{
	wax = x;
	way = y;
}

unsigned int layout_current_width(void)
{
	return waw;
}

unsigned int layout_current_height(void)
{
	return wah;
}

void layout_current_resize(unsigned int width, unsigned height)
{
	waw = width;
	wah = height;
}

void layout_current_arrange(void)
{
	frame_current()->layout->arrange(wax, way, waw, wah);
}

int layout_nmaster_get(int tab)
{
	return tab_get(tab)->f->nmaster;
}

int layout_nmaster_set(int tab, int n)
{
	if (window_popup_get() || layout_is_arrange(LAYOUT_MAXIMIZED) || layout_is_arrange(LAYOUT_GRID))
		return -1;

	tab_get(tab)->f->nmaster = n;
	layout_changed(true);

	return 0;
}

float layout_fmaster_get(int tab)
{
	return tab_get(tab)->f->mfact;
}

int layout_fmaster_set(int tab, float mfact)
{
	if (window_popup_get() || layout_is_arrange(LAYOUT_MAXIMIZED) || layout_is_arrange(LAYOUT_GRID))
		return -1;

	tab_get(tab)->f->mfact = mfact;
	layout_changed(true);

	return 0;
}

bool layout_sticky_get(int tab)
{
	return tab_get(tab)->f->msticky;
}


Tab *tab_get(int tab)
{
	return &tabs[tab];
}

int tab_current_id_get(void)
{
	return curr_tab;
}

void tab_current_id_set(int tab)
{
	curr_tab = tab;
}

Frame *frame_get(int fid)
{
	return tabs[fid].f;
}

int frame_current_id(void)
{
	return tab_current_id_get();
}

Frame *frame_current(void)
{
	return frame_get(tab_current_id_get());
}

int frame_current_set(int tab)
{
	tab_current_id_set(tab);
	layout_changed(true);
}

const char *frame_name_get(int tab)
{
	if (tab_get(tab)->f->name && strlen(tab_get(tab)->f->name)) {
		return tab_get(tab)->f->name;
	} else {
		return NULL;
	}
}

int frame_name_set(int tab, char *name)
{
	free(tab_get(tab)->f->name);
	tab_get(tab)->f->name = NULL;

	if (name && strlen(name))
		tab_get(tab)->f->name = strdup(name);
}

char *frame_cwd_get(int tab)
{
	return tab_get(tab)->f->cwd;
}

int frame_cwd_set(int tab, char *cwd)
{
	strncpy(tab_get(tab)->f->cwd, cwd, CWD_MAX - 1);
	return 0;
}

static void tabs_init(void) {
	int i;

	tab_current_id_set(1);
	for(i=0; i <= MAXTABS; i++) {
		tabs[i].f = calloc(1, sizeof(Frame));
		tabs[i].f->nmaster = NMASTER;
		tabs[i].f->mfact = MFACT;
		tabs[i].f->layout = layouts;
		tabs[i].f->layout_prev = layouts;
		tabs[i].f->msticky = false;
		tabs[i].f->name = NULL;
		tabs[i].f->cwd = calloc(CWD_MAX, 1);
		getcwd(tabs[i].f->cwd, CWD_MAX);
	}
}

static void tabs_cleanup(void)
{
	for(int i = 0; i <= MAXTABS; i++) {
		free(tab_get(i)->f->name);
		free(tab_get(i)->f->cwd);
		free(tab_get(i)->f);
	}
}

void window_init(Ui *_ui)
{
	tabs_init();
	ui = _ui;
}

void window_cleanup(void)
{
	tabs_cleanup();
}

Window *window_popup_get(void)
{
	return frame_current()->popup;
}

void *window_popup_set(Window *p)
{
	frame_current()->popup = p;
}

Window *window_current(void)
{
	return frame_current()->sel;
}

Window *window_get_by_coord(unsigned int x, unsigned int y)
{
	Window *c;

	if (y < way || y >= way+wah)
		return NULL;
	if (layout_is_arrange(LAYOUT_MAXIMIZED))
		return window_current();
	for_each_window(c) {
		int w_h = ui_window_height_get(c->win);
		int w_w = ui_window_width_get(c->win);
		int w_y = ui_window_y_get(c->win);
		int w_x = ui_window_x_get(c->win);

		if (x >= w_x && x < w_x + w_w && y >= w_y && y < w_y + w_h) {
			return c;
		}
	}
	return NULL;
}

void window_current_set(Window *w)
{
	frame_current()->sel = w;
}

Window *window_last_selected(void)
{
	return frame_current()->lastsel;
}

void window_last_selected_set(Window *w)
{
	frame_current()->lastsel = w;
}

Window *windows_list_by_fid(int fid)
{
	return frame_get(fid)->windows;
}


Window *window_first(void)
{
	return frame_current()->windows;
}

void window_first_set(Window *w)
{
	frame_current()->windows = w;
}

void window_next_set(Window *w, Window *n)
{
	window_remove(w);

	if (n->prev)
		n->prev->next = w;
	w->prev = n->prev;
	w->next = n;
	n->prev = w;

	layout_changed(true);
}

void window_prev_set(Window *w, Window *p)
{
	window_remove(w);

	if (p->next)
		p->next->prev = w;
	w->next = p->next;
	w->prev = p;
	p->next = w;

	layout_changed(true);
}

void window_insert_first(Window *c)
{
	if (window_first())
		window_first()->prev = c;

	c->next = window_first();
	c->prev = NULL;

	window_first_set(c);

	for (int o = 1; c; c = c->next, o++)
		c->order = o;
}

void window_insert_after(Window *c, Window *a)
{
	if (c == a)
		return;
	if (!a)
		for_each_window_except_last(a);

	if (a) {
		if (a->next)
			a->next->prev = c;
		c->next = a->next;
		c->prev = a;
		a->next = c;
		for (int o = a->order; c; c = c->next)
			c->order = ++o;
	}
}

static Window *window_last_master(void)
{
	Window *m, *last = NULL;

	for_each_window_master(m)
		last = m;

	return last;
}

void window_insert(Window *c)
{
	if (window_is_master_sticky(NULL)) {
		Window *master = window_last_master();

		if (master) {
			window_insert_after(c, master);
			return;
		}
	}

	window_insert_first(c);
}

void window_remove(Window *c)
{
	Window *d;
	if (c->prev)
		c->prev->next = c->next;
	if (c->next) {
		c->next->prev = c->prev;
		for (d = c->next; d; d = d->next)
			--d->order;
	}
	if (c == window_first())
		window_first_set(c->next);
	c->next = c->prev = NULL;
}


bool window_is_master(Window *w)
{
	Window *m;

	for_each_window_master(m) {
		if (w == m)
			return true;
	}

	return false;
}

bool window_is_master_sticky(Window *c)
{
	int n = 0;
	Window *m;

	if (!frame_current()->msticky)
		return false;
	if (!c)
		return true;

	return window_is_master(c);
}

bool window_is_widget(Window *w)
{
	return w->is_widget;
}

Window *window_stack(void)
{
	return frame_current()->stack;
}

static window_stack_set(Window *stack)
{
	frame_current()->stack = stack;
}

void window_stack_insert(Window *c)
{
	c->snext = window_stack();
	window_stack_set(c);
}

void window_stack_remove(Window *c)
{
	Window **tc;
	for (tc = &frame_current()->stack; *tc && *tc != c; tc = &(*tc)->snext);
	*tc = c->snext;
}

void window_move_resize(Window *c, int x, int y, int w, int h)
{
	ui_window_resize(c->win, w, h);
	ui_window_move(c->win, x, y);
}

char *window_title_get(Window *c)
{
	if (strlen(ui_window_title_get(c->win)))
		return ui_window_title_get(c->win);

	return buffer_name_get(c->buf);
}

static void term_title_set(Window *c)
{
	char *term, *t = title;
	char *ctitle = window_title_get(c);
	if (!t && window_current() == c && ctitle && strlen(ctitle))
		t = ctitle;
	if (t && (term = getenv("TERM")) && !strstr(term, "linux")) {
		printf("\033]0;%s\007", t);
		fflush(stdout);
	}
}

bool window_is_visible(Window *c)
{
	if (!c)
		return false;
	if (layout_is_arrange(LAYOUT_MAXIMIZED))
		return window_current() == c;
	else if (window_is_widget(c))
		return true;
	return true;
}

void window_update(Window *w)
{
	View *view = w->view;
	Buffer *buf = w->buf;
	UiWin *win = w->win;

	if (buffer_is_dirty(buf)) {
		size_t pos = buffer_cursor_get(buf);
		int x, y;

		view_invalidate(view);

		if (w == window_current() || window_is_widget(w)) {
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
				if (w == window_current())
					ui_window_cursor_set(win, x, y);
			}

			/* TODO: better to make buffer to know about it's
			 * windows and mark them as dirty on text update */
			buffer_dirty_set(buf, false);
		}
	}
}

void window_draw_title(Window *c)
{
	ui_text_style_t title_style = UI_TEXT_STYLE_NORMAL;
	int title_fg = UI_TEXT_COLOR_WHITE;
	int title_bg = UI_TEXT_COLOR_BRIGHT_BLACK;
	int maxlen, title_y, title_x;
	int w_w = ui_window_width_get(c->win);
	int has_border = ui_window_border_is_enabled(c->win);
	size_t status_len, name_len;
	Selection *view_cursor = view_selections_primary_get(c->view);
	size_t line, col;
	char status[100];
	char title[256];
	char name[200];
	char *name_ptr;
	xstr_t bname_str;

	if (!ui_window_has_title(c->win))
		return;

	if (window_current() == c) {
		title_fg = UI_TEXT_COLOR_BLACK;
		title_bg = UI_TEXT_COLOR_WHITE;
	}

	title_y = ui_window_height_get(c->win)-1;
	title_y -= has_border;
	title_x = has_border;
	w_w -= has_border;

	ui_window_draw_char_attr(c->win, title_x, title_y, ACS_HLINE, w_w-(has_border*2),
				 title_bg, title_bg,
				 UI_TEXT_STYLE_NORMAL);

	maxlen = ui_window_width_get(c->win) - 3;
	bname_str = xstr(buffer_name_get(c->buf));
	name_ptr = xstr_cptr(bname_str);

	line = view_cursors_line(view_cursor);
	col = view_cursors_cell_get(view_cursor);

	status_len = snprintf(status, sizeof(status), "[%d:%d] %s(%s) %s  [%d|%s]%s",
			line,
			col,
			buffer_is_modified(c->buf) ? "[+] " : "",
			buffer_mode_name_get(c->buf),
			buffer_state_name_get(c->buf),
			c->order,
			window_is_master_sticky(c) ? "*" : "",
			buffer_is_readonly(c->buf) ? "[RO]" : "");

	name_len = MIN(xstr_len(bname_str), maxlen - status_len);

	if (name_len < xstr_len(bname_str)) {
		name_ptr += xstr_len(bname_str) - name_len;
	}

	strncpy(name, name_ptr, MIN(sizeof(name)-1, name_len));
	name[MIN(sizeof(name)-1, name_len)] = '\0';

	if (name_len >= 3 && name_len < xstr_len(bname_str)) {
		name[0] = '.';
		name[1] = '.';
		name[2] = '.';
	}

	snprintf(title, sizeof(title), "%s %s", status, name);

	ui_window_draw_text_attr(c->win, 0, title_y, title, w_w,
			title_fg, title_bg,
			UI_TEXT_STYLE_NORMAL);

	if (window_current() == c)
		term_title_set(c);

	xstr_del(bname_str);
}

void window_draw_flags(Window *c, int flags)
{
	bool fire_event = !(flags & WIN_DRAW_F_NO_EVENT);
	bool force = flags & WIN_DRAW_F_FORCE;

	if ((force || buffer_is_dirty(c->buf) && window_is_visible(c)) || c == window_popup_get()) {
		/* we assume that it will be set on EVT_WIN_DRAW */
		/* ui_window_sidebar_width_set(c->win, 0); */
		ui_window_clear(c->win);

		window_update(c);

		if (fire_event) {
			event_t evt = {};

			evt.eid = EVT_WIN_DRAW;
			evt.oid = c->id;
			scheme_event_handle(evt);
		}

		ui_window_draw(c->win);

		if (!layout_is_arrange(LAYOUT_MAXIMIZED) || c == window_current())
			window_draw_title(c);

		ui_window_refresh(c->win);
	}
}

void window_draw(Window *c)
{
	window_draw_flags(c, 0);
}

void window_focus(Window *c)
{
	Window *lastsel;

	if (!c)
		c = window_stack();

	if (window_current() == c)
		return;

	window_last_selected_set(window_current());
	lastsel = window_last_selected();
	window_current_set(c);
	if (lastsel) {
		ui_window_focus(lastsel->win, false);
		lastsel->urgent = false;
		if (!layout_is_arrange(LAYOUT_MAXIMIZED)) {
			window_draw_title(lastsel);
			ui_window_refresh(lastsel->win);
		}
	}

	if (c) {
		Process *proc = buffer_proc_get(c->buf);
		Selection *s;

		window_stack_remove(c);
		window_stack_insert(c);
		c->urgent = false;

		if (proc && buffer_ref_count(c->buf) > 2) {
			vt_resize(process_term_get(proc),
					ui_window_height_get(c->win) - ui_window_has_title(c->win),
					ui_window_width_get(c->win));
		}

		ui_window_focus(c->win, true);

		if (layout_is_arrange(LAYOUT_MAXIMIZED)) {
			window_draw_flags(c, WIN_DRAW_F_FORCE);
		} else {
			window_draw_title(c);
			ui_window_refresh(c->win);
		}

		if (proc) {
			ui_window_cursor_disable(c->win,
				!vt_cursor_visible(process_term_get(proc)));
		} else {
			size_t curs_view = view_cursor_get(c->view);

			buffer_cursor_set(c->buf, curs_view);
			buffer_dirty_set(c->buf, true);
		}
	}
}
