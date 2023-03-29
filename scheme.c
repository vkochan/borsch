#undef VERSION
#include <errno.h>
#include <scheme.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/wait.h>

#include "text/text.h"
#include "process.h"
#include "buffer.h"
#include "window.h"
#include "keymap.h"
#include "timer.h"
#include "xstr.h"
#include "api.h"
#include "syntax.h"
#include "event.h"

#define CALL0(who) Scall0(Stop_level_value(Sstring_to_symbol(who)))
#define CALL1(who, arg) Scall1(Stop_level_value(Sstring_to_symbol(who)), arg)

#ifndef Sunboundp
#define Sunboundp(x) (((uptr)(x)) == 0x1E)
#endif

/* looks like cleanup() can ba called twice if
 * MOD-q-q was pressed, so use it to do deinit
 * only once. */
static int scheme_initialized = 0;

struct scheme_list
{
	ptr head;
	ptr tail;
};

static void scheme_list_insert(struct scheme_list *slist, ptr data)
{
	ptr snew = Scons(data, Snil);

	if (slist->head) {
		Sset_cdr(slist->tail, snew);
	} else {
		slist->head = snew;
	}

	slist->tail = snew;
}

static void scheme_plist_insert(struct scheme_list *slist, const char *name, ptr data)
{
	scheme_list_insert(slist, Sstring_to_symbol(name));
	scheme_list_insert(slist, data);
}

static int scheme_run_init(int argc, char *argv[])
{
	struct scheme_list args_list = {0};
	ptr scheme_args;

	for (int i = 0; i < argc; i++) {
		scheme_list_insert(&args_list, Sstring(argv[i]));
	}
	scheme_args = args_list.head ? args_list.head : Scons(Snil, Snil);

	CALL1("main-init", scheme_args);
}

/* Scheme foreign interface */
void scheme_ui_init(int ui_type)
{
	runtime_init(ui_type);
}

void scheme_ui_event_process(void)
{
	ui_event_process(g_ui);
}

void scheme_ui_clear_screen(void)
{
	ui_clear(g_ui);
}

void scheme_ui_draw_char(int x, int y, wchar_t ch, int n, short fg, short bg, int style)
{
	ui_draw_wchar(g_ui, x, y, ch, n, fg, bg, style);
}

void scheme_ui_refresh_screen(void)
{
	ui_refresh(g_ui);
}

ptr scheme_screen_width_get(void)
{
	return Sinteger(ui_width_get(g_ui));
}

ptr scheme_screen_height_get(void)
{
	return Sinteger(ui_height_get(g_ui));
}

bool scheme_win_layout_is_changed(void)
{
	return window_layout_is_changed();
}

void scheme_win_update_layout(void)
{
	window_update_layout();
}

void scheme_win_draw(int wid, bool enforce)
{
	int draw_flags = enforce ? WIN_DRAW_F_FORCE : 0;
	Window *w = window_get_by_id(wid);

	if (w) {
		window_draw_flags(w, draw_flags);
	}
}

bool scheme_win_is_visible(int wid)
{
	Window *w = window_get_by_id(wid);

	return window_is_visible(w);
}

ptr scheme_win_first_get(int fid)
{
	Frame *f = frame_by_id(fid);
	int wid = 0;

	if (!f)
		f = frame_current();
	if (!f)
		return Sfalse;

	if (windows_list(f))
		wid = windows_list(f)->id;

	return wid ? Sinteger(wid) : Sfalse;
}

ptr scheme_win_prev_get(int wid)
{
	Window *w = window_get_by_id(wid);
	Window *p = window_prev(w);

	if (p)
		return Sinteger(p->id);
	return Sfalse;
}

ptr scheme_win_next_get(int wid)
{
	Window *w = window_get_by_id(wid);
	Window *n = window_next(w);

	if (n)
		return Sinteger(n->id);
	return Sfalse;
}

ptr scheme_win_first_set(int wid)
{
	Window *w = window_get_by_id(wid);

	if (!w)
		return Sfalse;

	window_insert_first(w);
	return Strue;
}

ptr scheme_win_prev_set(int wid, int prev)
{
	Window *w = window_get_by_id(wid);
	Window *p = window_get_by_id(prev);

	if (!w || !p)
		return Sfalse;

	window_prev_set(w, p);
	return Strue;
}

ptr scheme_win_next_set(int wid, int next)
{
	Window *w = window_get_by_id(wid);
	Window *n = window_get_by_id(next);

	if (!w || !n)
		return Sfalse;

	window_next_set(w, n);
	return Strue;
}

ptr scheme_win_coord_get(int wid)
{
	Window *w = window_get_by_id(wid);
	int x, y;

	if (!w)
		return Sfalse;

	window_coord(w, &x, &y);
	return Scons(Sinteger(x), Sinteger(y));
}

ptr scheme_win_current_get(void)
{
	if (window_current())
		return Sinteger(window_current()->id);
	return Sfalse;
}

int scheme_win_current_set(int wid)
{
	Window *w = window_get_by_id(wid);
	if (w)
		window_focus(w);
	return 0;
}

ptr scheme_win_new(int bid)
{
	Buffer *buf;
	Window *w;

	buf = buffer_by_id(bid);
	buffer_dirty_set(buf, true);

	w = window_create(buf);
	if (!w)
 		return Sfalse;
	return Sinteger(w->id);
}

void scheme_win_del(int wid)
{
	Window *w = window_get_by_id(wid);

	if (w)
		window_delete(w);
}

ptr scheme_win_title_get(int wid)
{
	Window *w = window_get_by_id(wid);
	char *title = NULL;

	if (w)
		title = window_title_get(w);

	if (title)
		return Sstring(title);
	return Sfalse;
}

int scheme_win_title_set(int wid, char *title)
{
	Window *w = window_get_by_id(wid);

	if (w) {
		ui_window_title_set(w->win, title);
		if (!layout_is_arrange(LAYOUT_MAXIMIZED))
			buffer_dirty_set(w->buf, true);
		return 0;
	}

	return -1;
}

ptr scheme_win_buf_get(int wid)
{
	Window *c = window_get_by_id(wid);

	if (!c || !c->buf)
		return Sfalse;

	return Sinteger(buffer_id_get(c->buf));
}

void scheme_win_mark_highlight(int wid, bool enable)
{
	Window *w = window_get_by_id(wid);

	if (w) {
		if (enable != w->highlight_mark)
			buffer_dirty_set(w->buf, true);
		w->highlight_mark = enable;
	}
}

void scheme_win_size_set(int wid, int width, int height)
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

static int win_size_get(int wid, int *width, int *height)
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

ptr scheme_win_width_get(int wid)
{
	int width;
	int err;

	err = win_size_get(wid, &width, NULL);
	if (err)
		return Sfalse;
	return Sinteger(width);
}

ptr scheme_win_height_get(int wid)
{
	int height;
	int err;

	err = win_size_get(wid, NULL, &height);
	if (err)
		return Sfalse;
	return Sinteger(height);
}

ptr scheme_win_viewport_width_get(int wid)
{
	int width;
	int err;

	err = window_viewport_size(window_get_by_id(wid), &width, NULL);
	if (err)
		return Sfalse;
	return Sinteger(width);
}

ptr scheme_win_viewport_height_get(int wid)
{
	int height;
	int err;

	err = window_viewport_size(window_get_by_id(wid), NULL, &height);
	if (err)
		return Sfalse;
	return Sinteger(height);
}

void scheme_win_border_set(int wid, bool enable)
{
	Window *w = window_get_by_id(wid);

	if (w) {
		ui_window_border_enable(w->win, enable);
	}
}

void scheme_win_buf_switch(int wid, int bid)
{
	Window *w = window_get_by_id(wid);
	Buffer *b = buffer_by_id(bid);

	if (w && b && w->buf != b) {
		window_buffer_switch(w, b);
		view_reload(w->view, buffer_text_get(b));
		buffer_dirty_set(b, true);
	}
}

ptr scheme_win_prev_selected(void)
{
	if (window_last_selected())
		return Sinteger(window_last_selected()->id);
	return Sfalse;
}

ptr scheme_win_viewport_pos(int wid, char type)
{
	return Sinteger(window_viewport_pos(window_get_by_id(wid), type));
}

ptr scheme_win_viewport_coord(int wid, int pos)
{
	int l, x, y;
	int err;

	err = window_viewport_pos_to_coord(window_get_by_id(wid), pos, &l, &x, &y);
	if (!err) {
		return Scons(Sinteger(x), Scons(Sinteger(y), Scons(Sinteger(l), Snil)));
	}

	return Sfalse;
}

ptr scheme_win_scroll(int wid, char type, int n)
{
	return Sinteger(window_scroll(window_get_by_id(wid), type, n));
}

void scheme_win_sidebar_set(int wid, int width)
{
	window_sidebar_width_set(window_get_by_id(wid), width);
}

ptr scheme_win_sidebar_get(int wid)
{
	return Sinteger(window_sidebar_width(window_get_by_id(wid)));
}

void scheme_win_sidebar_draw(int wid, int x, int y, const char *text, short fg, short bg, int attr)
{
	window_sidebar_draw(window_get_by_id(wid), x, y, text, fg, bg, attr);
}

void scheme_win_update(int wid)
{
	win_update(wid);
}

static int kmap_add(int pid)
{
	KeyMap *pmap = keymap_by_id(pid);
	KeyMap *kmap;

	kmap = keymap_new(pmap);
	if (kmap) {
		keymap_ref_get(kmap);
		return keymap_id_get(kmap);
	}

	return -1;
}

static int kmap_parent_set(int kid, char *name, int pid)
{
	KeyMap *kmap = keymap_by_id(kid);

	if (kmap) {
		keymap_parent_name_set(kmap, name);

		if (pid > 0) {
			KeyMap *parent = keymap_by_id(pid);
			if (parent) {
				keymap_parent_set(kmap, parent);
			}
		}
		return 0;
	}

	return -1;
}

static int kmap_parent_get(int kid)
{
	KeyMap *kmap = keymap_by_id(kid);

	if (kmap) {
		KeyMap *parent = keymap_parent_get(kmap);

		if (parent)
			return keymap_id_get(parent);
	}

	return -1;
}

static void kmap_del(int kid)
{
	KeyMap *kmap = keymap_by_id(kid);

	if (kmap) {
		keymap_ref_put(kmap);
		keymap_free(kmap);
	}
}

ptr scheme_kmap_add(char *parent)
{
	int ret = kmap_add(0);

	if (ret) {
		if (parent && strlen(parent))
			kmap_parent_set(ret, parent, -1);
		return Sinteger(ret);
	}

	return Sfalse;
}

void scheme_kmap_parent_set(int kid, char *name, int pid)
{
	kmap_parent_set(kid, name, pid);
}

ptr scheme_kmap_parent_get(int kid)
{
	int ret = kmap_parent_get(kid);

	if (ret > 0)
		return Sinteger(ret);
	return Sfalse;
}

void scheme_kmap_del(int kid)
{
	kmap_del(kid);
}

ptr scheme_buf_new(char *name)
{
	int ret = buf_new(name);

	if (ret)
		return Sinteger(ret);
	return Sfalse;
}

ptr scheme_buf_ref_get(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_ref_get(buf);
		return Strue;
	}
	return Sfalse;
}

ptr scheme_buf_ref_put(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_ref_put(buf);
		return Strue;
	}
	return Sfalse;
}

ptr scheme_buf_ref(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		return Sinteger(buffer_ref_count(buf));
	}
	return Sfalse;
}

ptr scheme_buf_is_valid(int bid)
{
	return Sboolean(!!buffer_by_id(bid));
}

void scheme_buf_del(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_del(buf);
	}
}

void scheme_buf_kmap_set(int bid, char *name)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_keymap_set(buf, name);
	}
}

ptr scheme_buf_kmap_get(int bid)
{
	Buffer *buf = buffer_by_id(bid);
	KeyMap *kmap;
	int ret;

	if (buf)
		kmap = buffer_keymap_get(buf);
	if (kmap)
		ret = keymap_id_get(kmap);

	if (ret)
		return Sinteger(ret);

	return Sfalse;
}

ptr scheme_buf_current_get(void)
{
	if (window_current())
		return Sinteger(buffer_id_get(window_current()->buf));
	return Sfalse;
}

ptr scheme_buf_name_get(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf)
		return Sstring(buffer_name_get(buf));

	return Sstring(NULL);
}

void scheme_buf_name_set(int bid, const char *name)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_name_lock(buf, true);
		buffer_name_set(buf, name);

		Window *c;
		for_each_window(c) {
			if (c->buf == buf)
				buffer_dirty_set(c->buf, true);
		}
	}
}

ptr scheme_buf_readonly_get(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf)
		return Sboolean(buffer_is_readonly(buf));
	return Sfalse;
}

ptr scheme_buf_is_modified(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf)
		return Sboolean(buffer_is_modified(buf));
	return Sfalse;
}

void scheme_buf_readonly_set(int bid, bool is_readonly)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_readonly_set(buf, is_readonly);

		Window *w;
		for_each_window(w) {
			if (w->buf == buf)
				buffer_dirty_set(w->buf, true);
		}
	}
}

ptr scheme_buf_by_name(const char *name)
{
	Buffer *buf = buffer_by_name(name);

	if (buf)
		return Sinteger(buffer_id_get(buf));

	return Sfalse;
}

ptr scheme_buf_text_insert(int bid, const char *text)
{
	size_t pos = buf_text_insert(bid, text);

	if (pos != EPOS)
		return Sinteger(pos);

	return Sfalse;
}

ptr scheme_buf_text_insert_char(int bid, char ch)
{
	size_t pos = buf_text_insert_char(bid, ch);

	if (pos != EPOS)
		return Sinteger(pos);

	return Sfalse;
}

ptr scheme_buf_text_insert_nl(int bid, int pos)
{
	pos = buf_text_insert_nl(bid, pos);

	if (pos != EPOS)
		return Sinteger(pos);

	return Sfalse;
}

ptr scheme_buf_text_insert_file(int bid, const char *path)
{
	size_t pos = buf_text_insert_file(bid, path);

	if (pos != EPOS)
		return Sinteger(pos);

	return Sfalse;
}

ptr scheme_buf_text_obj_pos(int bid, size_t pos, char obj, int n)
{
	return Sinteger(buf_text_obj_move(bid, pos, obj, n, false));
}

ptr scheme_buf_text_obj_range(int bid, size_t pos, char obj, bool inner)
{
	int start, end;

	buf_text_obj_range(bid, pos, obj, &start, &end, inner);

	return Scons(Sinteger(start), Sinteger(end));
}

ptr scheme_buf_text_range_del(int bid, int start, int end)
{
	size_t pos = buf_text_range_del(bid, start, end);

	if (pos != EPOS)
		return Sinteger(pos);

	return Sfalse;
}

ptr scheme_buf_text_get(int bid, int start, int len)
{
	char *data = buf_text_get(bid, start, len);
	ptr ret;

	if (data)
		ret = Sstring_utf8(data, len);
	else
		ret = Sfalse;

	free(data);
	return ret;
}

void scheme_buf_text_fg_set(int bid, int fg)
{
	buf_text_fg_set(bid, fg);
}

ptr scheme_buf_text_fg_get(int bid)
{
	return Sinteger(buf_text_fg_get(bid));
}

void scheme_buf_text_bg_set(int bid, int bg)
{
	buf_text_bg_set(bid, bg);
}

ptr scheme_buf_text_bg_get(int bid)
{
	return Sinteger(buf_text_bg_get(bid));
}

void scheme_buf_text_style_set(int bid, int style)
{
	buf_text_style_set(bid, style);
}

ptr scheme_buf_text_style_get(int bid)
{
	return Sinteger(buf_text_style_get(bid));
}

ptr scheme_buf_cursor_get(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		size_t pos = buffer_cursor_get(buf);
		if (pos != EPOS)
			return Sinteger(pos);
	}

	return Sfalse;
}

void scheme_buf_cursor_set(int bid, size_t pos)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_cursor_set(buf, pos);
		/* just to make UI update */
		buffer_dirty_set(buf, true);
	}
}

ptr scheme_buf_line_num(int bid, size_t pos)
{
	size_t line = buf_line_num(bid, pos);

	if (line != EPOS)
		return Sinteger(line);

	return Sfalse;
}

void scheme_buf_text_input_enable(int bid, bool enable)
{
	buf_input_enable(bid, enable);
}

void scheme_buf_mode_name_set(int bid, char *name)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_mode_name_set(buf, name);
		buffer_dirty_set(buf, true);
	}
}

ptr scheme_buf_mode_name_get(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		return Sstring(buffer_mode_name_get(buf));
	}
	return Sstring("");
}

void scheme_buf_state_name_set(int bid, char *name)
{
	Buffer *buf = buffer_by_id(bid);
	if (buf) {
		buffer_state_name_set(buf, name);
		buffer_dirty_set(buf, true);
	}
}

ptr scheme_buf_state_name_get(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		return Sstring(buffer_state_name_get(buf));
	}
	return Sstring("");
}

ptr scheme_buf_file_open(int bid, const char *file)
{
	return Sinteger(buf_file_open(bid, file));
}

void scheme_buf_file_set(int bid, const char *file)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_filename_set(buf, file);
	}
}

ptr scheme_buf_file_get(int bid)
{
	Buffer *buf = buffer_by_id(bid);
	char *name = NULL;

	if (buf) {
		name = buffer_filename_get(buf);
	}

	if (name)
		return Sstring(name);
	return Sstring("");
}

ptr scheme_buf_save(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf)
		return Sboolean(buffer_save(buf));
	return Sfalse;
}

ptr scheme_buf_mark_is_set(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf)
		return Sboolean(buffer_is_mark_set(buf));

	return Sfalse;
}

void scheme_buf_mark_set(int bid, size_t pos)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf)
		buffer_mark_set(buf, pos);
}

ptr scheme_buf_mark_get(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf)
		return Sinteger(buffer_mark_get(buf));

	return Sinteger(EPOS);
}

void scheme_buf_mark_clear(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf)
		buffer_mark_clear(buf);
}

ptr scheme_buf_is_term(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf)
		return Sboolean(buffer_proc_get(buf) != NULL);
	return Sfalse;
}

void scheme_buf_term_set(int bid, pid_t pid)
{
	Process *proc = process_by_pid(pid);
	Buffer *buf = buffer_by_id(bid);

	if (!proc || !buf)
		return;

	process_buffer_set(proc, buf);
	buffer_proc_set(buf, proc);
}

ptr scheme_buf_is_visible(int bid)
{
	Buffer *buf = buffer_by_id(bid);
	if (buf) {
		Window *w;
		for_each_window(w) {
			if (w->buf == buf)
				return Strue;
		}
	}
	return Sfalse;
}

ptr scheme_buf_prop_style_add(int bid, int type, int fg, int bg, int attr, const char *style_name, int start, int end,
			      const char *regex, char *name, bool expand)
{
	int ret = buf_prop_style_add(bid, type, fg, bg, attr, style_name, start, end, regex, name, expand);

	if (ret == 0)
		Sinteger(ret);
	return Sfalse;
}

ptr scheme_buf_prop_kmap_add(int bid, int kid, int start, int end, const char *regex, char *name)
{
	int ret = buf_prop_kmap_add(bid, kid, start, end, regex, name);

	if (ret == 0)
		Sinteger(ret);
	return Sfalse;
}

ptr scheme_buf_prop_symbol_add(int bid, const char *symbol, int start, int end, const char *regex, char *name)
{
	int ret = buf_prop_symbol_add(bid, symbol, start, end, regex, name);

	if (ret == 0)
		Sinteger(ret);
	return Sfalse;
}

static void scheme_buf_prop_data_free(void *data)
{
	Sunlock_object((ptr)data);
}

ptr scheme_buf_prop_data_add(int bid, ptr data, int start, int end, const char *regex, char *name)
{
	int ret = buf_prop_data_add(bid, data, start, end, regex, name, scheme_buf_prop_data_free);

	if (ret == 0) {
		Slock_object(data);
		Sinteger(ret);
	}
	return Sfalse;
}

void scheme_buf_prop_del(int bid, int type, int start, int end, const char *regex, char *name)
{
	buf_prop_del(bid, type, start, end, regex, name);
}

static ptr scheme_color_to_name(short color)
{
	const char *color_name = "default";

	switch (color) {
	case UI_TEXT_COLOR_BLACK:
		color_name = "black"; break;
	case UI_TEXT_COLOR_RED:
		color_name = "red"; break;
	case UI_TEXT_COLOR_GREEN:
		color_name = "green"; break;
	case UI_TEXT_COLOR_YELLOW:
		color_name = "yellow"; break;
	case UI_TEXT_COLOR_BLUE:
		color_name = "blue"; break;
	case UI_TEXT_COLOR_MAGENTA:
		color_name = "magenta"; break;
	case UI_TEXT_COLOR_CYAN:
		color_name = "cyan"; break;
	case UI_TEXT_COLOR_WHITE:
		color_name = "white"; break;
	case UI_TEXT_COLOR_BRIGHT_BLACK:
		color_name = "bright-black"; break;
	case UI_TEXT_COLOR_BRIGHT_RED:
		color_name = "bright-red"; break;
	case UI_TEXT_COLOR_BRIGHT_GREEN:
		color_name = "bright-green"; break;
	case UI_TEXT_COLOR_BRIGHT_YELLOW:
		color_name = "bright-yellow"; break;
	case UI_TEXT_COLOR_BRIGHT_BLUE:
		color_name = "bright-blue"; break;
	case UI_TEXT_COLOR_BRIGHT_MAGENTA:
		color_name = "bright-magenta"; break;
	case UI_TEXT_COLOR_BRIGHT_CYAN:
		color_name = "bright-cyan"; break;
	case UI_TEXT_COLOR_BRIGHT_WHITE:
		color_name = "bright-white"; break;
	}

	return Sstring(color_name);
}

/*
  '(
      ((:type . style) (:start . 0) (:end . 1) (:fg . "") (:bg . "") (:attr . ""))
      ((:type . style) (:start . 1) (:end . 2) (:fg . "") (:bg . "") (:attr . ""))
   )
*/
static void scheme_buf_prop_walk(Buffer *buf, int id, size_t start, size_t end, void *data, void *arg)
{
	struct scheme_list *plist = arg;
	ptr sprop = NULL;

	switch (id) {
	case PROPERTY_TYPE_TEXT_STYLE:
	{
		struct scheme_list style_plist = {0};
		struct scheme_list attr_plist = {0};
		char attr_name[128];
		Style *style = data;

		attr_name[0] = '\0';

		for (int i = 0; i < UI_TEXT_STYLE_MAX; i++) {
			const char *a = NULL;
			int bit = (1 << i);

			if (bit & style->attr) {
				switch (bit) {
				case UI_TEXT_STYLE_NORMAL: a = "normal"; break;
				case UI_TEXT_STYLE_BOLD: a = "bold"; break;
				case UI_TEXT_STYLE_DIM: a = "dim"; break;
				case UI_TEXT_STYLE_ITALIC: a = "italic"; break;
				case UI_TEXT_STYLE_UNDERLINE: a = "underline"; break;
				case UI_TEXT_STYLE_BLINK: a = "blink"; break;
				case UI_TEXT_STYLE_REVERSE: a = "reverse"; break;
				case UI_TEXT_STYLE_INVIS: a = "invisible"; break;
				default: a = "unknown"; break;
				}

				if (attr_name[0])
					strncat(attr_name, " ", sizeof(attr_name)-1);

				strncat(attr_name, a, sizeof(attr_name)-1);
			}
		}

		scheme_plist_insert(&attr_plist, "fg:", scheme_color_to_name(style->fg));
		scheme_plist_insert(&attr_plist, "bg:", scheme_color_to_name(style->bg));
		scheme_plist_insert(&attr_plist, "attr:", Sstring(attr_name));
						       
		//scheme_plist_insert(&style_plist, ":type", Sstring_to_symbol(":style"));
		scheme_plist_insert(&style_plist, "start:", Sinteger(start));
		scheme_plist_insert(&style_plist, "end:", Sinteger(end));
		scheme_plist_insert(&style_plist, "style:", attr_plist.head);
		
		scheme_list_insert(plist, style_plist.head);
		break;
	}

	case PROPERTY_TYPE_TEXT_SYMBOL:
	{
		struct scheme_list symbol_plist = {0};
		char *symbol = data;

		//scheme_plist_insert(&symbol_plist, ":type", Sstring_to_symbol(":symbol"));
		scheme_plist_insert(&symbol_plist, "start:", Sinteger(start));
		scheme_plist_insert(&symbol_plist, "end:", Sinteger(end));
		scheme_plist_insert(&symbol_plist, "symbol:", Sstring_to_symbol(symbol));
						       
		scheme_list_insert(plist, symbol_plist.head);
		break;
	}

	case PROPERTY_TYPE_TEXT_DATA:
	{
		struct scheme_list data_plist = {0};
		ptr pdata = data;

		//scheme_plist_insert(&data_plist, ":type", Sstring_to_symbol(":data"));
		scheme_plist_insert(&data_plist, "start:", Sinteger(start));
		scheme_plist_insert(&data_plist, "end:", Sinteger(end));
		scheme_plist_insert(&data_plist, "data:", pdata);
		
		scheme_list_insert(plist, data_plist.head);
		break;
	}

	}
}

ptr scheme_buf_prop_get(int bid, int type, int start, int end, char *name)
{
	struct scheme_list plist = {0};

	buf_prop_walk(bid, type, start, end, name, &plist, scheme_buf_prop_walk);

	return plist.head ? plist.head : Scons(Snil, Snil);
}

ptr scheme_buf_env_get(int bid)
{
	Buffer *buf = buffer_by_id(bid);
	void *env = NULL;

	if (buf) {
		env = buffer_env_get(buf);
	}

	if (env)
		return (ptr)env;
	return Sfalse;
}

void scheme_buf_snapshot(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_snapshot(buf);
	}
}

void scheme_buf_undo(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_undo(buf);
	}
}

void scheme_buf_redo(int bid)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		buffer_redo(buf);
	}
}

ptr scheme_buf_search_regex(int bid, size_t pos, const char *pattern, int dir)
{
	Buffer *buf = buffer_by_id(bid);
	int ret = EPOS;

	if (buf) {
		ret = buffer_search_regex(buf, pos, pattern, dir);
	}

	if (ret != EPOS)
		return Sinteger(ret);
	return Sfalse;
}

int scheme_buf_tag_set(int bid, int tag)
{
	return 0;
}

int scheme_buf_tag_bits(int bid)
{
	return 0;
}

int scheme_buf_tag_toggle(int bid, int tag)
{
	return 0;
}

int scheme_buf_tag_add(int bid, int tag)
{
	return 0;
}

int scheme_buf_tag_del(int bid, int tag)
{
	return 0;
}

ptr scheme_buf_parser_set(int bid, const char *lang)
{
	Buffer *buf = buffer_by_id(bid);

	if (buf) {
		int err = buffer_parser_set(buf, lang);
		if (err)
			return Sfalse;
		return Strue;
	}

	return Sfalse;
}

ptr scheme_stx_lang_style_add(const char *lang, int fg, int bg, int attr, const char *style_name, const char *rule)
{
	Style *style, *style_bind;
	int err;

	style = style_new();
	if (!style)
		return Sfalse;

	style_bind = style_get_by_name(style_name);
	if (style_bind) {
		style->id = style_bind->id;
	} else {
		style->attr = attr;
		style->fg = fg;
		style->bg = bg;
	}

	err = syntax_lang_rule_add(lang, SYNTAX_RULE_TYPE_STYLE, rule, style);
	if (err)
		return Sfalse;

	return Strue;
}

void scheme_stx_lang_style_del(const char *lang, const char *rule)
{
	syntax_lang_rule_remove(lang, SYNTAX_RULE_TYPE_STYLE, rule);
}

void scheme_stx_lang_style_clear(const char *lang)
{
	syntax_lang_rules_clear(lang, SYNTAX_RULE_TYPE_STYLE);
}

ptr scheme_style_add(const char *name, int fg, int bg, int attr)
{
	Style style = {
		.name = name,
		.attr = attr,
		.fg = fg,
		.bg = bg,
	};
	int id;

	id = style_add(&style);
	if (id >= 0) {
		return Sinteger(id);
	}

	return Sfalse;
}

ptr scheme_style_set(const char *name, int fg, int bg, int attr)
{
	Style *style;

	style = style_get_by_name(name);
	if (style) {
		if (fg != -1)
			style->fg = fg;
		if (bg != -1)
			style->bg = bg;
		if (attr)
			style->attr = attr;

		int err = style_update(style->id, style);
		if (err) {
			return Sfalse;
		}
		return Strue;
	}

	return Sfalse;
}

ptr scheme_style_get(const char *name)
{
	Style *style;

	style = style_get_by_name(name);
	if (style) {
		return Scons(Sinteger(style->fg),
				Scons(Sinteger(style->bg),
					Scons(Sinteger(style->attr), Snil)));
	}

	return Sfalse;
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

ptr scheme_widget_create(const char *name, int x, int y, int width, int height, int type)
{
	Window *w;
	Buffer *b;

	b = __buf_new(name, NULL);
	if (!b) {
		return Sfalse;
	}

	w = widget_create(b, x, y, width, height, type);
	if (!w) {
		buffer_del(b);
		return Sfalse;
	}
	return Sinteger(w->id);
}

static char *scheme_string_to_cptr(ptr str)
{
	size_t str_len;
	char *cptr;

	if (!Sstringp(str))
		return NULL;

	str_len = Sstring_length(str);

	cptr = calloc(1, str_len + 1);
	if (!cptr)
		return NULL;

	for (int i = 0; i < str_len; i++) {
		char c = Sstring_ref(str, i);
		cptr[i] = c;
	}

	return cptr;
}

static char **scheme_list_to_env(ptr list, char **env, int i)
{
	char *name, *value;
	ptr var;

	env = realloc(env, sizeof(char **) * (i + 2));
	env[i] = NULL;

	if (Snullp(list))
		return env;

	var = Scar(list);
	env[i] = scheme_string_to_cptr(Scar(var));
	env[i+1] = scheme_string_to_cptr(Scdr(var));

	return scheme_list_to_env(Scdr(list), env, i + 2);
}

int scheme_term_keys_send(int bid, char *text)
{
	return term_keys_send(bid, text);
}

int scheme_term_text_send(int bid, char *text)
{
	return term_text_send(bid, text);
}

ptr scheme_term_text_get(int bid)
{
	char *text = NULL;
	size_t len;
	int err;
	ptr s;

	err = term_text_get(bid, &text, &len);
	if (err) {
		return Sfalse;
	}

	s = Sstring_utf8(text, len);
	free(text);
	return s;
}

ptr scheme_term_current_line_get(int bid)
{
	char *text = NULL;
	size_t len;
	int err;
	ptr s;

	err = term_current_line_get(bid, &text, &len);
	if (err) {
		return Sfalse;
	}

	s = Sstring_utf8(text, len);
	free(text);
	return s;
}

void scheme_term_filter_enable(int bid, bool enable)
{
	term_filter_enable(bid, enable);
}

void scheme_frame_current_set(int fid)
{
	Frame *f = frame_by_id(fid);

	if (f)
		frame_current_set(f);
}

ptr scheme_frame_create(void)
{
	Frame *f = frame_create();

	if (f)
		return Sinteger(f->id);
	return Sfalse;
}

void scheme_frame_delete(int fid)
{
	Frame *f = frame_by_id(fid);

	if (f) {
		frame_delete(f);
	}
}

int scheme_layout_current_get(int fid)
{
	return layout_current_get(fid);
}

int scheme_layout_current_set(int fid, layout_t lay)
{
	return layout_current_set(fid, lay);
}

int scheme_layout_nmaster_get(int fid)
{
	return layout_nmaster_get(fid);
}

int scheme_layout_nmaster_set(int fid, int n)
{
	return layout_nmaster_set(fid, n);
}

float scheme_layout_fmaster_get(int fid)
{
	return layout_fmaster_get(fid);
}

int scheme_layout_fmaster_set(int fid, float f)
{
	return layout_fmaster_set(fid, f);
}

bool scheme_layout_sticky_get(int fid)
{
	return layout_sticky_get(fid);
}

int scheme_layout_sticky_set(int fid, bool is_sticky)
{
	return layout_sticky_set(fid, is_sticky);
}

static int bind_key(char *key, void (*act)(void), int kid, char *tname)
{
	KeyMap *kmap = keymap_by_id(kid);
	if (!kmap)
		return -1;
	return keymap_bind(kmap, key, act, tname);
}

ptr scheme_bind_key(char *key, bind_key_cb_t cb, int mid, char *tname)
{
	int ret = bind_key(key, cb, mid, tname);

	if (ret)
		return Sinteger(ret);

	return Sfalse;
}

static int unbind_key(char *key, int kid)
{
	KeyMap *kmap = keymap_by_id(kid);
	if (!kmap)
		return -1;

	return keymap_unbind(kmap, key);
}

ptr scheme_unbind_key(char *key, int mid)
{
	int ret = unbind_key(key, mid);
	if (ret)
		return Sinteger(ret);

	return Sfalse;
}

ptr scheme_evt_fd_handler_add(int fd, void (*fn)(int fd, void *))
{
	int ret = event_fd_handler_register(fd, fn, NULL);

	if (ret == 0)
		return Sinteger(ret);
	return Sfalse;
}

void scheme_evt_fd_handler_del(int fd)
{
	event_fd_handler_unregister(fd);
}

ptr scheme_timer_add(void (*cb)(void *ctx))
{
	int fd = timer_add(cb, NULL);

	if (fd < 0)
		return Sfalse;

	return Sinteger(fd);
}

void scheme_timer_del(int fd)
{
	timer_del(fd);
}

void scheme_timer_interval_set(int fd, unsigned long ms)
{
	timer_interval_set(fd, ms);
}

void scheme_timer_time_set(int fd, unsigned long sec, unsigned long nsec)
{
	timer_time_set(fd, sec, nsec);
}

void scheme_process_destroy_dead(void)
{
	process_destroy_dead();
}

ptr scheme_process_create(const char *prog, const char *cwd, bool redir_in, bool redir_out, bool redir_err, ptr env, bool pty, bool async)
{
	int *in_ptr = NULL, *out_ptr = NULL, *err_ptr = NULL;
	int in = -1, out = -1, err = -1;
	Process *proc;
	pid_t pid;
	ptr ret;
	
	if (redir_in)
		in_ptr = &in;
	if (redir_out)
		out_ptr = &out;
	if (redir_err)
		err_ptr = &err;

	proc = process_create(prog, cwd, in_ptr, out_ptr, err_ptr, scheme_list_to_env(env, NULL, 0), pty, async);
	if (!proc)
		return Sfalse;

	return Scons(Sinteger(in),
			Scons(Sinteger(out),
				Scons(Sinteger(err),
					Scons(Sinteger(process_pid_get(proc)), Snil))));
}

void scheme_process_delete(int pid)
{
	Process *proc = process_by_pid(pid);

	if (proc) {
		process_destroy(proc);
	}
}

void scheme_process_kill(int pid)
{
	Process *proc = process_by_pid(pid);

	if (proc) {
		process_kill_async(proc);
	}
}

ptr scheme_process_wait(int pid)
{
	Process *proc = process_by_pid(pid);
	int status = -1;
	int err;

	if (!proc)
		return Sfalse;

	err = process_wait(proc, &status);
	if (err)
		return Sfalse;

	return Sinteger(status);
}

ptr scheme_process_is_alive(int pid)
{
	int status;
	pid_t ret;

	ret = waitpid(pid, &status, WNOHANG);
	if (ret != pid) {
		return Sboolean(false);
	} else {
		return Sboolean(true);
	}
}

ptr scheme_process_is_async(int pid)
{
	Process *proc = process_by_pid(pid);

	if (proc) {
		return Sboolean(process_is_async(proc));
	}
	return Sboolean(false);
}

ptr scheme_process_status_get(int pid)
{
	Process *proc = process_by_pid(pid);

	if (proc) {
		return Sinteger(process_status_get(proc));
	}

	return Sinteger(-1);
}

extern char **environ;

static ptr scheme_os_environment_list(char **env)
{
	char *name, *value;
	ptr var = Snil;
	char *tok;

	if (!env || !*env)
		return Snil;

	tok = strchr(*env, '=');
	if (tok) {
		ptr s_name = Sstring_of_length(*env, tok - *env);
		ptr s_value = Sstring_of_length(tok + 1, strlen(tok + 1));

		var = Scons(s_name, s_value);
	} else {
		return Snil;
	}

	return Scons(var, scheme_os_environment_list(env+1));
}

ptr scheme_os_environment_get(void)
{
	return scheme_os_environment_list(environ);
}

void scheme_do_quit(void)
{
	do_quit();
}

static void scheme_export_symbols(void)
{
	Sregister_symbol("cs_ui_init", scheme_ui_init);
	Sregister_symbol("cs_ui_event_process", scheme_ui_event_process);
	Sregister_symbol("cs_ui_refresh_screen", scheme_ui_refresh_screen);
	Sregister_symbol("cs_ui_clear_screen", scheme_ui_clear_screen);
	Sregister_symbol("cs_ui_draw_char", scheme_ui_draw_char);

	Sregister_symbol("cs_screen_width_get", scheme_screen_width_get);
	Sregister_symbol("cs_screen_height_get", scheme_screen_height_get);

	Sregister_symbol("cs_win_layout_is_changed", scheme_win_layout_is_changed);
	Sregister_symbol("cs_win_update_layout", scheme_win_update_layout);
	Sregister_symbol("cs_win_draw", scheme_win_draw);
	Sregister_symbol("cs_win_is_visible", scheme_win_is_visible);
	Sregister_symbol("cs_win_first_get", scheme_win_first_get);
	Sregister_symbol("cs_win_prev_get", scheme_win_prev_get);
	Sregister_symbol("cs_win_next_get", scheme_win_next_get);
	Sregister_symbol("cs_win_first_set", scheme_win_first_set);
	Sregister_symbol("cs_win_prev_set", scheme_win_prev_set);
	Sregister_symbol("cs_win_next_set", scheme_win_next_set);
	Sregister_symbol("cs_win_coord_get", scheme_win_coord_get);
	Sregister_symbol("cs_win_current_get", scheme_win_current_get);
	Sregister_symbol("cs_win_current_set", scheme_win_current_set);
	Sregister_symbol("cs_win_new", scheme_win_new);
	Sregister_symbol("cs_win_del", scheme_win_del);
	Sregister_symbol("cs_win_title_get", scheme_win_title_get);
	Sregister_symbol("cs_win_title_set", scheme_win_title_set);
	Sregister_symbol("cs_win_buf_get", scheme_win_buf_get);
	Sregister_symbol("cs_win_mark_highlight", scheme_win_mark_highlight);
	Sregister_symbol("cs_win_size_set", scheme_win_size_set);
	Sregister_symbol("cs_win_width_get", scheme_win_width_get);
	Sregister_symbol("cs_win_height_get", scheme_win_height_get);
	Sregister_symbol("cs_win_viewport_width_get", scheme_win_viewport_width_get);
	Sregister_symbol("cs_win_viewport_height_get", scheme_win_viewport_height_get);
	Sregister_symbol("cs_win_border_set", scheme_win_border_set);
	Sregister_symbol("cs_win_buf_switch", scheme_win_buf_switch);
	Sregister_symbol("cs_win_prev_selected", scheme_win_prev_selected);
	Sregister_symbol("cs_win_viewport_pos", scheme_win_viewport_pos);
	Sregister_symbol("cs_win_viewport_coord", scheme_win_viewport_coord);
	Sregister_symbol("cs_win_scroll", scheme_win_scroll);
	Sregister_symbol("cs_win_sidebar_set", scheme_win_sidebar_set);
	Sregister_symbol("cs_win_sidebar_get", scheme_win_sidebar_get);
	Sregister_symbol("cs_win_sidebar_draw", scheme_win_sidebar_draw);
	Sregister_symbol("cs_win_update", scheme_win_update);

	Sregister_symbol("cs_kmap_add", scheme_kmap_add);
	Sregister_symbol("cs_kmap_parent_set", scheme_kmap_parent_set);
	Sregister_symbol("cs_kmap_parent_get", scheme_kmap_parent_get);
	Sregister_symbol("cs_kmap_del", scheme_kmap_del);

	Sregister_symbol("cs_buf_new", scheme_buf_new);
	Sregister_symbol("cs_buf_ref_get", scheme_buf_ref_get);
	Sregister_symbol("cs_buf_ref_put", scheme_buf_ref_put);
	Sregister_symbol("cs_buf_ref", scheme_buf_ref);
	Sregister_symbol("cs_buf_is_valid", scheme_buf_is_valid);
	Sregister_symbol("cs_buf_del", scheme_buf_del);
	Sregister_symbol("cs_buf_kmap_get", scheme_buf_kmap_get);
	Sregister_symbol("cs_buf_kmap_set", scheme_buf_kmap_set);
	Sregister_symbol("cs_buf_current_get", scheme_buf_current_get);
	Sregister_symbol("cs_buf_name_get", scheme_buf_name_get);
	Sregister_symbol("cs_buf_name_set", scheme_buf_name_set);
	Sregister_symbol("cs_buf_readonly_get", scheme_buf_readonly_get);
	Sregister_symbol("cs_buf_readonly_set", scheme_buf_readonly_set);
	Sregister_symbol("cs_buf_is_modified", scheme_buf_is_modified);
	Sregister_symbol("cs_buf_by_name", scheme_buf_by_name);
	Sregister_symbol("cs_buf_text_insert", scheme_buf_text_insert);
	Sregister_symbol("cs_buf_text_insert_char", scheme_buf_text_insert_char);
	Sregister_symbol("cs_buf_text_insert_nl", scheme_buf_text_insert_nl);
	Sregister_symbol("cs_buf_text_insert_file", scheme_buf_text_insert_file);
	Sregister_symbol("cs_buf_text_obj_pos", scheme_buf_text_obj_pos);
	Sregister_symbol("cs_buf_text_obj_range", scheme_buf_text_obj_range);
	Sregister_symbol("cs_buf_text_get", scheme_buf_text_get);
	Sregister_symbol("cs_buf_text_range_del", scheme_buf_text_range_del);
	Sregister_symbol("cs_buf_text_input_enable", scheme_buf_text_input_enable);
	Sregister_symbol("cs_buf_text_fg_set", scheme_buf_text_fg_set);
	Sregister_symbol("cs_buf_text_bg_set", scheme_buf_text_bg_set);
	Sregister_symbol("cs_buf_text_style_set", scheme_buf_text_style_set);
	Sregister_symbol("cs_buf_text_fg_get", scheme_buf_text_fg_get);
	Sregister_symbol("cs_buf_text_bg_get", scheme_buf_text_bg_get);
	Sregister_symbol("cs_buf_text_style_get", scheme_buf_text_style_get);

	Sregister_symbol("cs_buf_mode_name_set", scheme_buf_mode_name_set);
	Sregister_symbol("cs_buf_mode_name_get", scheme_buf_mode_name_get);
	Sregister_symbol("cs_buf_state_name_set", scheme_buf_state_name_set);
	Sregister_symbol("cs_buf_state_name_get", scheme_buf_state_name_get);
	Sregister_symbol("cs_buf_cursor_get", scheme_buf_cursor_get);
	Sregister_symbol("cs_buf_cursor_set", scheme_buf_cursor_set);
	Sregister_symbol("cs_buf_line_num", scheme_buf_line_num);
	Sregister_symbol("cs_buf_file_open", scheme_buf_file_open);
	Sregister_symbol("cs_buf_file_set", scheme_buf_file_set);
	Sregister_symbol("cs_buf_file_get", scheme_buf_file_get);
	Sregister_symbol("cs_buf_save", scheme_buf_save);
	Sregister_symbol("cs_buf_mark_set", scheme_buf_mark_set);
	Sregister_symbol("cs_buf_mark_is_set", scheme_buf_mark_is_set);
	Sregister_symbol("cs_buf_mark_get", scheme_buf_mark_get);
	Sregister_symbol("cs_buf_mark_clear", scheme_buf_mark_clear);
	Sregister_symbol("cs_buf_is_term", scheme_buf_is_term);
	Sregister_symbol("cs_buf_term_set", scheme_buf_term_set);
	Sregister_symbol("cs_buf_is_visible", scheme_buf_is_visible);

	Sregister_symbol("cs_buf_prop_style_add", scheme_buf_prop_style_add);
	Sregister_symbol("cs_buf_prop_kmap_add", scheme_buf_prop_kmap_add);
	Sregister_symbol("cs_buf_prop_symbol_add", scheme_buf_prop_symbol_add);
	Sregister_symbol("cs_buf_prop_data_add", scheme_buf_prop_data_add);
	Sregister_symbol("cs_buf_prop_del", scheme_buf_prop_del);
	Sregister_symbol("cs_buf_prop_get", scheme_buf_prop_get);

	Sregister_symbol("cs_buf_env_get", scheme_buf_env_get);

	Sregister_symbol("cs_buf_snapshot", scheme_buf_snapshot);
	Sregister_symbol("cs_buf_undo", scheme_buf_undo);
	Sregister_symbol("cs_buf_redo", scheme_buf_redo);
	Sregister_symbol("cs_buf_search_regex", scheme_buf_search_regex);

	Sregister_symbol("cs_buf_tag_set", scheme_buf_tag_set);
	Sregister_symbol("cs_buf_tag_bits", scheme_buf_tag_bits);
	Sregister_symbol("cs_buf_tag_toggle", scheme_buf_tag_toggle);
	Sregister_symbol("cs_buf_tag_add", scheme_buf_tag_add);
	Sregister_symbol("cs_buf_tag_del", scheme_buf_tag_del);

	Sregister_symbol("cs_buf_parser_set", scheme_buf_parser_set);
	Sregister_symbol("cs_stx_lang_style_add", scheme_stx_lang_style_add);
	Sregister_symbol("cs_stx_lang_style_del", scheme_stx_lang_style_del);
	Sregister_symbol("cs_stx_lang_style_clear", scheme_stx_lang_style_clear);

	Sregister_symbol("cs_style_add",  scheme_style_add);
	Sregister_symbol("cs_style_set",  scheme_style_set);
	Sregister_symbol("cs_style_get",  scheme_style_get);

	Sregister_symbol("cs_widget_create", scheme_widget_create);

	Sregister_symbol("cs_term_keys_send", scheme_term_keys_send);
	Sregister_symbol("cs_term_text_send", scheme_term_text_send);
	Sregister_symbol("cs_term_text_get", scheme_term_text_get);
	Sregister_symbol("cs_term_current_line_get", scheme_term_current_line_get);
	Sregister_symbol("cs_term_filter_enable", scheme_term_filter_enable);

	Sregister_symbol("cs_frame_current_set", scheme_frame_current_set);
	Sregister_symbol("cs_frame_create", scheme_frame_create);
	Sregister_symbol("cs_frame_delete", scheme_frame_delete);

	Sregister_symbol("cs_layout_current_get", scheme_layout_current_get);
	Sregister_symbol("cs_layout_current_set", scheme_layout_current_set);
	Sregister_symbol("cs_layout_nmaster_get", scheme_layout_nmaster_get);
	Sregister_symbol("cs_layout_nmaster_set", scheme_layout_nmaster_set);
	Sregister_symbol("cs_layout_fmaster_get", scheme_layout_fmaster_get);
	Sregister_symbol("cs_layout_fmaster_set", scheme_layout_fmaster_set);
	Sregister_symbol("cs_layout_sticky_get", scheme_layout_sticky_get);
	Sregister_symbol("cs_layout_sticky_set", scheme_layout_sticky_set);

	Sregister_symbol("cs_bind_key", scheme_bind_key);
	Sregister_symbol("cs_unbind_key", scheme_unbind_key);

	Sregister_symbol("cs_evt_fd_handler_add", scheme_evt_fd_handler_add);
	Sregister_symbol("cs_evt_fd_handler_del", scheme_evt_fd_handler_del);

	Sregister_symbol("cs_timer_add", scheme_timer_add);
	Sregister_symbol("cs_timer_del", scheme_timer_del);
	Sregister_symbol("cs_timer_interval_set", scheme_timer_interval_set);
	Sregister_symbol("cs_timer_time_set", scheme_timer_time_set);

	Sregister_symbol("cs_process_is_alive", scheme_process_is_alive);
	Sregister_symbol("cs_process_is_async", scheme_process_is_async);
	Sregister_symbol("cs_process_status_get", scheme_process_status_get);
	Sregister_symbol("cs_process_destroy_dead", scheme_process_destroy_dead);
	Sregister_symbol("cs_process_create", scheme_process_create);
	Sregister_symbol("cs_process_del", scheme_process_delete);
	Sregister_symbol("cs_process_kill", scheme_process_kill);
	Sregister_symbol("cs_process_wait", scheme_process_wait);

	Sregister_symbol("cs_os_environment_get", scheme_os_environment_get);

	Sregister_symbol("cs_do_quit", scheme_do_quit);
}

static void *scheme_symb_resolver(keymap_symb_t type, char *name)
{
	ptr p = Stop_level_value(Sstring_to_symbol(name));

	if (Sunboundp(p))
		return NULL;

	return (void *)Sinteger32_value(p);
}

int scheme_init(int argc, char *argv[])
{
	int err;

	Sscheme_init(NULL);
	Sregister_boot_file("/usr/lib/csv"VERSION"/"MACHINE_TYPE"/petite.boot");
	Sregister_boot_file("/usr/lib/csv"VERSION"/"MACHINE_TYPE"/scheme.boot");
	Sregister_boot_file("/usr/lib/csv"VERSION"/"MACHINE_TYPE"/borsch.boot");
	Sbuild_heap(NULL, scheme_export_symbols);

	CALL1("source-directories", Scons(Sstring(LIB_PATH), Snil));

	err = fifo_create();
	if (err) {
		fprintf(stderr, "failed to create fifo\n");
		return err;
	}

	keymap_symb_resolver_set(scheme_symb_resolver);

	scheme_initialized = 1;

	scheme_run_init(argc, argv);
	return 0;
}

void scheme_uninit(void)
{
	if (scheme_initialized) {
		Sscheme_deinit();
		scheme_initialized = 0;
	}
}

int scheme_event_handle(event_t evt)
{
	ptr str = Sfalse;

	if (evt.str && evt.len)
		str = Sstring_utf8(evt.str, evt.len);

	debug("scheme: event: enter: eid=%d, oid=%d\n", evt.eid, evt.oid);
	Scall3(Stop_level_value(Sstring_to_symbol("__on-event-handler")),
			Sinteger(evt.eid),
			Sinteger(evt.oid),
			str);
	debug("scheme: event: exit\n");
	return 0;
}

int scheme_eval_file(const char *in, const char *out)
{
	return Sinteger32_value(Scall2(Stop_level_value(Sstring_to_symbol("__do-eval-file")),
				Sstring(in),
				Sstring(out)));
}

void *scheme_env_alloc(void)
{
	ptr env;

	env = CALL1("copy-environment", CALL0("scheme-environment"));
	Slock_object(env);

	return env;
}

void scheme_env_free(void *env)
{
	if (env)
		Sunlock_object((ptr)env);
}
