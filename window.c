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

static Window *widgets;
static Window *new_windows;
static int win_id;
static Frame *current_frame;
static Frame *frame_list;
static int frame_id;
static char *title;
static Ui *ui;

Window *window_new_list(void)
{
	return new_windows;
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

	f->id = ++frame_id;

	frame_insert(f);

	if (!current_frame)
		current_frame = f;

	return f;
}

void frame_delete(Frame *f)
{
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

Window *window_current(void)
{
	if (!frame_current())
		return NULL;

	return frame_current()->sel;
}

Window *window_get_by_id(int id)
{
	Window *c;

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

void window_insert_new(Window *w)
{
	w->prev = NULL;
	w->next = new_windows;
	if (new_windows)
		new_windows->prev = w;
	new_windows = w;
	w->is_new = true;
}

void window_remove(Window *w)
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

bool window_is_widget(Window *w)
{
	return w->is_widget;
}

void window_move_resize(Window *c, int x, int y, int w, int h)
{
	ui_window_resize(c->win, w, h);
	ui_window_move(c->win, x, y);
}

static void term_title_set(Window *c)
{
	char *term, *t = title;
	char *ctitle = "";
	if (!t && window_current() == c && ctitle && strlen(ctitle))
		t = ctitle;
	if (t && (term = getenv("TERM")) && !strstr(term, "linux")) {
		printf("\033]0;%s\007", t);
		fflush(stdout);
	}
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
	ui_window_sidebar_width_set(w->win, width);
}

int window_sidebar_width(Window *w)
{
	if (w) {
		return ui_window_sidebar_width_get(w->win);
	}

	return 0;
}

void window_update_cursor(Window *w)
{
	View *view = w->view;
	Buffer *buf = w->buf;
	UiWin *win = w->win;

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
}

static void window_current_set(Window *w)
{
	if (frame_current())
		frame_current()->sel = w;
}

void window_focus(Window *c)
{
	Window *prev = window_current();

	window_current_set(c);

	if (prev) {
		ui_window_focus(prev->win, false);
	}

	if (c) {
		Process *proc = buffer_proc_get(c->buf);

		ui_window_focus(c->win, true);

		if (proc) {
			ui_window_cursor_disable(c->win,
				!vt_cursor_visible(process_term_get(proc)));
		} else {
			size_t curs_view = view_cursor_get(c->view);

			buffer_cursor_set(c->buf, curs_view);
		}
	}
}

void window_delete(Window *w)
{
	Buffer *buf = w->buf;

	if (w == window_current())
		window_current_set(NULL);
	
	if (w->is_widget)
		widget_remove(w);
	else
		window_remove(w);

	ui_window_free(w->win);
	view_free(w->view);
	free(w);
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
	cell_style.ch = style->ch;

	if (cell_style.attr == 0)
		cell_style.attr = default_style->attr;
	if (cell_style.fg == -1)
		cell_style.fg = default_style->fg;
	if (cell_style.bg == -1)
		cell_style.bg = default_style->bg;

	view_style(view, cell_style, start, end, style->expand, style->is_set);
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

	if (data)
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

	for (Line *l = view_lines_last(w->view)->next; l; l = l->next) {
		l->cells[0].style.fg = default_style->fg;
		l->cells[0].style.bg = default_style->bg;
		strncpy(l->cells[0].data, eof_sym, eof_len);
		l->cells[0].len = eof_len;
	}
}

void window_buffer_switch(Window *w, Buffer *b)
{
	w->buf = b;

	if (buffer_proc_get(b)) {
		vt_attach(process_term_get(buffer_proc_get(b)), w);
	} else {
		ui_window_cursor_disable(w->win, false);
		ui_window_on_view_update_set(w->win, on_view_update_cb);
		ui_window_ops_update_set(w->win, NULL);
		ui_window_on_resize_set(w->win, NULL);
		ui_window_priv_set(w->win, w);
	}
}

Window *window_create(Buffer *buf, bool is_widget)
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

	return w;
}
