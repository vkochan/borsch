#include <unistd.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include "process.h"
#include "vt.h"
#include "common.h"
#include "window.h"
#include "buffer.h"
#include "view.h"
#include "style.h"
#include "ui/ui.h"
#include "xstr.h"

#define for_each_frame(__f) \
	for (__f = frame_list; __f; __f = __f->next)

#define for_each_widget(__w) \
	for (__w = widgets; __w; __w = __w->next)

#define for_each_new_window(__w) \
	for (__w = new_windows; __w; __w = __w->next)

static unsigned int waw, wah, wax, way;
static bool layout_needs_arrange = false;
static Window *widgets;
static Window *new_windows;
static int win_id;
static Frame *current_frame;
static Frame *frame_list;
static int frame_id;
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
				ui_draw_wchar_vert(ui, nx, ny, UI_TEXT_SYMBOL_VLINE, wah,
							UI_TEXT_COLOR_DEFAULT, UI_TEXT_COLOR_DEFAULT, UI_TEXT_STYLE_NORMAL);
				ui_draw_wchar(ui, nx, ny, UI_TEXT_SYMBOL_TTEE, 1,
						UI_TEXT_COLOR_DEFAULT, UI_TEXT_COLOR_DEFAULT, UI_TEXT_STYLE_NORMAL);
				nx++;
				nw = waw - mw -1;
			}
			nh = (i < n - 1) ? th : (lay + wah) - ny;
			if (i > m)
				ui_draw_wchar(ui, nx - 1, ny, UI_TEXT_SYMBOL_LTEE, 1,
						UI_TEXT_COLOR_DEFAULT, UI_TEXT_COLOR_DEFAULT, UI_TEXT_STYLE_NORMAL);
		}
		window_move_resize(c, nx, ny+(way-lay), nw, nh);
		ny += nh;
		i++;
	}

	/* Fill in nmaster intersections */
	if (n > m) {
		ny = lay + mh;
		for (i = 1; i < m; i++) {
			ui_draw_wchar(ui, nx - 1, ny, ((ny - 1) % th ? UI_TEXT_SYMBOL_RTEE : UI_TEXT_SYMBOL_PLUS), 1,
					UI_TEXT_COLOR_DEFAULT, UI_TEXT_COLOR_DEFAULT, UI_TEXT_STYLE_NORMAL);
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
			ui_draw_wchar_vert(ui, nx, ny, UI_TEXT_SYMBOL_VLINE, nh + ah,
						UI_TEXT_COLOR_DEFAULT, UI_TEXT_COLOR_DEFAULT, UI_TEXT_STYLE_NORMAL);
			/* if we are on the first row, or on the last one and there are fewer windows
			 * than normal whose border does not match the line above, print a top tree char
			 * otherwise a plus sign. */
			if (i <= cols
			    || (i >= rows * cols - cols && n % cols
				&& (cols - (n % cols)) % 2))
				ui_draw_wchar(ui, nx, ny, UI_TEXT_SYMBOL_TTEE, 1,
						UI_TEXT_COLOR_DEFAULT, UI_TEXT_COLOR_DEFAULT, UI_TEXT_STYLE_NORMAL);
			else
				ui_draw_wchar(ui, nx, ny, UI_TEXT_SYMBOL_PLUS, 1,
						UI_TEXT_COLOR_DEFAULT, UI_TEXT_COLOR_DEFAULT, UI_TEXT_STYLE_NORMAL);
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
				ui_draw_wchar_vert(ui, nx, ny, UI_TEXT_SYMBOL_VLINE, nh,
							UI_TEXT_COLOR_DEFAULT, UI_TEXT_COLOR_DEFAULT, UI_TEXT_STYLE_NORMAL);
				ui_draw_wchar(ui, nx, ny, UI_TEXT_SYMBOL_TTEE, 1,
						UI_TEXT_COLOR_DEFAULT, UI_TEXT_COLOR_DEFAULT, UI_TEXT_STYLE_NORMAL);
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
				ui_draw_wchar_vert(ui, nx, ny, UI_TEXT_SYMBOL_VLINE, nh,
							UI_TEXT_COLOR_DEFAULT, UI_TEXT_COLOR_DEFAULT, UI_TEXT_STYLE_NORMAL);
				ui_draw_wchar(ui, nx, ny, UI_TEXT_SYMBOL_TTEE, 1,
						UI_TEXT_COLOR_DEFAULT, UI_TEXT_COLOR_DEFAULT, UI_TEXT_STYLE_NORMAL);
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
				ui_draw_wchar(ui, nx, ny, UI_TEXT_SYMBOL_PLUS, 1,
						UI_TEXT_COLOR_DEFAULT, UI_TEXT_COLOR_DEFAULT, UI_TEXT_STYLE_NORMAL);
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
	if (!frame_current())
		return NULL;

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

layout_t layout_current_get(int fid)
{
	return layout_current()->id;
}

int layout_current_set(int fid, layout_t lay)
{
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
	if (frame_current())
	frame_current()->layout->arrange(wax, way, waw, wah);
}

int layout_nmaster_get(int fid)
{
	return frame_current()->nmaster;
}

int layout_nmaster_set(int fid, int n)
{
	if (layout_is_arrange(LAYOUT_MAXIMIZED) || layout_is_arrange(LAYOUT_GRID))
		return -1;

	frame_current()->nmaster = n;
	layout_changed(true);

	return 0;
}

float layout_fmaster_get(int fid)
{
	return frame_current()->mfact;
}

int layout_fmaster_set(int fid, float mfact)
{
	if (layout_is_arrange(LAYOUT_MAXIMIZED) || layout_is_arrange(LAYOUT_GRID))
		return -1;

	frame_current()->mfact = mfact;
	layout_changed(true);

	return 0;
}

bool layout_sticky_get(int fid)
{
	return frame_current()->msticky;
}

int layout_sticky_set(int fid, bool is_sticky)
{
	Window *m;

	frame_current()->msticky = is_sticky;
	
	for_each_window_master(m)
		buffer_dirty_set(m->buf, true);
	return 0;
}

int frame_current_id(void)
{
	if (frame_current())
		return frame_current()->id;
	return 0;
}

Frame *frame_current(void)
{
	return current_frame;
}

int frame_current_set(Frame *f)
{
	current_frame = f;
	layout_changed(true);
}

static frame_insert(Frame *f)
{
	f->prev = NULL;
	f->next = frame_list;
	if (frame_list)
		frame_list->prev = f;
	frame_list = f;
}

static frame_remove(Frame *f)
{
	if (f->prev)
		f->prev->next = f->next;
	if (f->next)
		f->next->prev = f->prev;
	if (f == frame_list)
		frame_list = f->next;
	f->next = f->prev = NULL;
}

Frame *frame_create(void)
{
	Frame *f;

	f = calloc(1, sizeof(Frame));
	if (!f)
		return NULL;

	f->nmaster = NMASTER;
	f->mfact = MFACT;
	f->layout = layouts;
	f->msticky = false;
	f->id = ++frame_id;

	frame_insert(f);

	if (!current_frame)
		current_frame = f;

	return f;
}

void frame_delete(Frame *f)
{
	while (f->windows) {
		window_delete(f->windows);
	}
	frame_remove(f);
	free(f);
}

Frame *frame_by_id(int fid)
{
	Frame *f;

	for_each_frame(f)
		if (f->id == fid)
			return f;

	return NULL;
}

static void frames_cleanup(void)
{
	while (frame_list) {
		frame_delete(frame_list);
	}
}

static void widget_insert(Window *w)
{
	if (widgets)
		widgets->prev = w;

	w->next = widgets;
	w->prev = NULL;

	widgets = w;
}

static void widget_remove(Window *w)
{
	if (w->prev)
		w->prev->next = w->next;
	if (w->next) {
		w->next->prev = w->prev;
	}
	if (w == widgets)
		widgets = w->next;
	w->next = w->prev = NULL;
}

void window_init(Ui *_ui)
{
	ui = _ui;
}

void window_cleanup(void)
{
	while (new_windows) {
		Window *w = new_windows;
		Buffer *buf = w->buf;

		window_delete(w);
	}
	while (widgets) {
		Window *w = widgets;
		Buffer *buf = w->buf;

		window_delete(w);
	}
	frames_cleanup();
}

void window_coord(Window *w, int *x, int *y)
{
	if (x) *x = ui_window_x_get(w->win);
	if (y) *y = ui_window_y_get(w->win);
}

Window *window_current(void)
{
	if (!frame_current())
		return NULL;

	return frame_current()->sel;
}

Window *window_get_by_id(int id)
{
	Window *c;

	for_each_new_window(c) {
		if (c->id == id)
			return c;
	}
	for_each_window(c) {
		if (c->id == id)
			return c;
	}
	for_each_widget(c) {
		if (c->id == id)
			return c;
	}

	return NULL;
}

static Window *window_stack_top(void);

Window *window_last_selected(void)
{
	return window_stack_top();
}

Window *windows_list(Frame *f)
{
	return f->windows;
}

Window *window_first(void)
{
	if (!frame_current())
		return NULL;

	return frame_current()->windows;
}

void window_next_set(Window *w, Window *n)
{
	window_remove(n);

	if (w->next)
		w->next->prev = n;
	n->next = w->next;
	w->next = n;
	n->prev = w;

	layout_changed(true);
}

void window_prev_set(Window *w, Window *p)
{
	window_remove(p);

	if (w->prev)
		w->prev->next = p;
	p->prev = w->prev;
	w->prev = p;
	p->next = w;

	layout_changed(true);
}

Window *window_prev(Window *w)
{
	if (w)
		return w->prev;
	return NULL;
}

Window *window_next(Window *w)
{
	if (w)
		return w->next;
	return NULL;
}

void window_insert_first(Window *c)
{
	window_remove(c);

	if (c->frame->windows)
		c->frame->windows->prev = c;

	c->next = c->frame->windows;
	c->prev = NULL;

	c->frame->windows = c;

	layout_changed(true);
}

void window_insert_new(Window *w)
{
	w->prev = NULL;
	w->next = new_windows;
	if (new_windows)
		new_windows->prev = w;
	new_windows = w;
	w->is_new = true;
}

void window_remove_new(Window *w)
{
	if (w->prev)
		w->prev->next = w->next;
	if (w->next)
		w->next->prev = w->prev;
	if (w == new_windows)
		new_windows = w->next;
	w->next = w->prev = NULL;
	w->is_new = false;
}

void window_remove(Window *c)
{
	Window *d;

	if (c->is_new) {
		window_remove_new(c);
		return;
	}

	if (c->prev)
		c->prev->next = c->next;
	if (c->next) {
		c->next->prev = c->prev;
	}
	if (c == c->frame->windows)
		c->frame->windows = c->next;
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

	if (!frame_current())
		return false;

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

static Window *window_stack_top(void)
{
	if (!frame_current())
		return NULL;

	return frame_current()->stack;
}

static window_stack_set(Window *stack)
{
	if (frame_current())
		frame_current()->stack = stack;
}

static window_stack_insert(Window *c)
{
	c->snext = window_stack_top();
	window_stack_set(c);
}

static window_stack_remove(Window *c)
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

bool window_is_visible(Window *w)
{
	if (!w)
		return false;

	if (window_is_widget(w))
		return true;
	else if (layout_is_arrange(LAYOUT_MAXIMIZED))
		return window_current() == w;
	return w->frame == frame_current();
}

int window_viewport_pos(Window *w, char type)
{
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

int window_viewport_pos_to_coord(Window *w, int pos, int *l, int *x, int *y)
{
	Line *line;

	if (!w)
		return -1;

	if (view_coord_get(w->view, pos, &line, y, x)) {
		*l = line->lineno;
		return 0;
	}

	return -1;
}

int window_viewport_size(Window *w, int *width, int *height)
{
	if (!w)
		return -1;

	if (width)
		*width = view_width_get(w->view);
	if (height)
		*height = view_height_get(w->view);

	return 0;
}

int window_scroll(Window *w, char type, int n)
{
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

void window_sidebar_width_set(Window *w, int width)
{
	if (w && width != ui_window_sidebar_width_get(w->win)) {
		ui_window_sidebar_width_set(w->win, width);
		buffer_dirty_set(w->buf, true);
		window_draw_flags(w, WIN_DRAW_F_FORCE | WIN_DRAW_F_NO_EVENT);
	}
}

int window_sidebar_width(Window *w)
{
	if (w) {
		return ui_window_sidebar_width_get(w->win);
	}

	return 0;
}

void window_sidebar_draw(Window *w, int x, int y, const char *text, short fg, short bg, int attr)
{
	if (w) {
		ui_window_sidebar_draw(w->win, x, y, text, fg, bg, attr);
	}
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
		}
		/* TODO: better to make buffer to know about it's
		 * windows and mark them as dirty on text update */
		buffer_dirty_set(buf, false);
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

	ui_window_draw_wchar(c->win, title_x, title_y, UI_TEXT_SYMBOL_HLINE, w_w-(has_border*2),
			     title_bg, title_bg, UI_TEXT_STYLE_NORMAL);

	maxlen = ui_window_width_get(c->win) - 3;
	bname_str = xstr(buffer_name_get(c->buf));
	name_ptr = xstr_cptr(bname_str);

	line = view_cursors_line(view_cursor);
	col = view_cursors_cell_get(view_cursor);

	status_len = snprintf(status, sizeof(status), "[%d:%d] %s(%s) %s %s %s",
			line,
			col,
			buffer_is_modified(c->buf) ? "[+] " : "",
			buffer_mode_name_get(c->buf),
			buffer_state_name_get(c->buf),
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

	if (!c)
		return;

	if (c == window_current() && window_is_visible(c)) {
		if (buffer_proc_get(window_current()->buf)) {
			Process *proc = buffer_proc_get(window_current()->buf);
			ui_window_cursor_disable(window_current()->win,
				!vt_cursor_visible(process_term_get(proc)));
		}
	}

	if ((force || buffer_is_dirty(c->buf) && window_is_visible(c))) {
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
	}
}

void window_draw(Window *c)
{
	window_draw_flags(c, 0);
}

static void window_current_set(Window *w)
{
	if (frame_current())
		frame_current()->sel = w;
}

void window_focus(Window *c)
{
	Window *prev = window_current();

	if (window_current() == c)
		return;

	window_current_set(c);

	if (prev) {
		ui_window_focus(prev->win, false);
		buffer_dirty_set(prev->buf, true);
		prev->urgent = false;
	}

	if (c) {
		Process *proc = buffer_proc_get(c->buf);
		Selection *s;

		if (!c->is_widget) {
			window_stack_remove(c);
			window_stack_insert(c);
		}
		c->urgent = false;

		if (proc && buffer_ref_count(c->buf) > 1) {
			vt_resize(process_term_get(proc),
					ui_window_height_get(c->win) - ui_window_has_title(c->win),
					ui_window_width_get(c->win));
		}

		ui_window_focus(c->win, true);

		if (proc) {
			ui_window_cursor_disable(c->win,
				!vt_cursor_visible(process_term_get(proc)));
		} else {
			size_t curs_view = view_cursor_get(c->view);

			buffer_cursor_set(c->buf, curs_view);
		}
		buffer_dirty_set(c->buf, true);
	}
}

void window_delete(Window *w)
{
	Buffer *buf = w->buf;

	window_stack_remove(w);

	if (w->is_widget)
		widget_remove(w);
	else
		window_remove(w);

	if (window_current() == w) {
		Window *last = window_stack_top();
		Window *next = w->next;
		Window *focus;

		if (last) {
			focus = last;
		} else {
			focus = next;
		}
		window_focus(focus);
	}

	ui_window_free(w->win);
	view_free(w->view);
	free(w);
	layout_changed(true);
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

	view_style(view, cell_style, start, end, style->expand);
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
			v.start, v.end, NULL, w->view, style_prop_draw);
	buffer_properties_walk(w->buf, PROPERTY_TYPE_TEXT_HIGHLIGHT,
			v.start, v.end, NULL, w->view, style_prop_draw);

	if (w->highlight_mark) {
		size_t start = buffer_mark_get(w->buf);
		size_t end = buffer_cursor_get(w->buf);
		Style *highlight_style = style_get_by_id(1);
		CellStyle cell_style = {
			.attr = highlight_style->attr,
			.fg = highlight_style->fg,
			.bg = highlight_style->bg,
		};

		view_style(w->view, cell_style, MIN(start, end), MAX(start, end), false);
	}

	for (Line *l = view_lines_last(w->view)->next; l; l = l->next) {
		l->cells[0].style.fg = default_style->fg;
		l->cells[0].style.bg = default_style->bg;
		strncpy(l->cells[0].data, eof_sym, eof_len);
		l->cells[0].len = eof_len;
	}
}

void window_buffer_switch(Window *w, Buffer *b)
{
	w->prev_buf = w->buf;
	w->buf = b;

	if (buffer_proc_get(b)) {
		vt_attach(process_term_get(buffer_proc_get(b)), w);
	} else {
		ui_window_cursor_disable(w->win, false);
		ui_window_on_view_update_set(w->win, on_view_update_cb);
		ui_window_ops_draw_set(w->win, NULL);
		ui_window_on_resize_set(w->win, NULL);
		ui_window_priv_set(w->win, w);
	}
}

Window *__window_create(Buffer *buf, bool is_widget, int x, int y, int width, int height)
{
	Window *w;

	if (!buf)
		return NULL;

	w = calloc(1, sizeof(Window));
	if (!w)
		return NULL;
	w->id = ++win_id;
	w->is_widget = is_widget;
	w->buf = buf;
	w->frame = frame_current();

	w->view = view_new(buffer_text_get(w->buf));
	if (!w->view) {
		free(w);
		return NULL;
	}

	w->win = ui_window_new(ui, w->view);
	if (!w->win) {
		view_free(w->view);
		free(w);
		return NULL;
	}

	window_buffer_switch(w, buf);

	if (is_widget) {
		widget_insert(w);
	} else {
		ui_window_has_title_set(w->win, true);
		window_insert_new(w);
	}

	ui_window_resize(w->win, width, height);
	ui_window_move(w->win, x, y);
	layout_changed(true);
	return w;
}

Window *window_create(Buffer *buf)
{
	return __window_create(buf, false, layout_current_x(), layout_current_y(),
				    layout_current_width(), layout_current_height());
}

Window *widget_create(Buffer *buf, int x, int y, int width, int height, int pos_flags)
{
	Window *w = __window_create(buf, true, x, y, width, height);

	if (!w)
		return w;

	w->pos_flags = pos_flags;
	return w;
}

static void window_update_screen_size(void) {
	int waw, wah, way = 0, wax = 0;
	int top_h = 0;
	int bot_h = 0;
	Window *w;

	for_each_widget(w) {
		if (w->pos_flags & WIN_POS_F_TOP)
			top_h += ui_window_height_get(w->win);
		else if (w->pos_flags & WIN_POS_F_BOT)
			bot_h += ui_window_height_get(w->win);
	}

	wah = ui_height_get(ui)-bot_h;
	waw = ui_width_get(ui);
	wah -= top_h;
	way += top_h;

	layout_current_resize(waw, wah);
	layout_current_move(wax, way);

	for_each_widget(w) {
		ui_window_width_set(w->win, ui_width_get(ui));
		if (w->pos_flags & WIN_POS_F_BOT)
			ui_window_move(w->win, 0, ui_height_get(ui)-bot_h);
	}
}

bool window_layout_is_changed(void)
{
	return ui_resize(ui) || layout_is_changed();
}

void window_update_layout(void)
{
	int n = 0;
	Window *w;

	layout_changed(false);

	window_update_screen_size();

	layout_current_arrange();
}
