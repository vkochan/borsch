#undef VERSION
#include <errno.h>
#include <scheme.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>

#include "text/text.h"
#include "keymap.h"
#include "timer.h"
#include "api.h"

#define CALL0(who) Scall0(Stop_level_value(Sstring_to_symbol(who)))
#define CALL1(who, arg) Scall1(Stop_level_value(Sstring_to_symbol(who)), arg)

#ifndef Sunboundp
#define Sunboundp(x) (((uptr)(x)) == 0x1E)
#endif

/* looks like cleanup() can ba called twice if
 * MOD-q-q was pressed, so use it to do deinit
 * only once. */
static int scheme_initialized = 0;

#define SCHEME_INIT_SCRIPT "/."PROGNAME"/init.ss"

static int scheme_run_script(const char *path)
{
	struct stat st;

	if (stat(path, &st) == 0)
		return Sscheme_script(path, 0, NULL);

	return 0;
}

static int scheme_run_init_script(void)
{
	char *home = getenv("HOME");
	size_t home_len;
	struct stat st;
	char path[128];
	char *p;

	if (!home)
		home = "/root";

	home_len = strlen(home);
	path[0] = '\0';

        strncat(path, home, sizeof(path));
	strncat(path+home_len, SCHEME_INIT_SCRIPT, sizeof(path) - home_len);

	return scheme_run_script(path);
}

/* Scheme foreign interface */
ptr scheme_win_get_by_coord(int x, int y)
{
	int ret = win_get_by_coord(x, y);

	if (ret)
		return Sinteger(ret);

	return Sfalse;
}

bool scheme_win_is_visible(int wid)
{
	return win_is_visible(wid);
}

ptr scheme_win_first_get(void)
{
	int ret = win_first_get();

	if (ret)
		return Sinteger(ret);

	return Sfalse;
}

ptr scheme_win_prev_get(int wid)
{
	int ret = win_prev_get(wid);

	if (ret)
		return Sinteger(ret);

	return Sfalse;
}

ptr scheme_win_next_get(int wid)
{
	int ret = win_next_get(wid);

	if (ret)
		return Sinteger(ret);

	return Sfalse;
}

ptr scheme_win_upper_get(int wid)
{
	int ret = win_upper_get(wid);

	if (ret)
		return Sinteger(ret);

	return Sfalse;
}

ptr scheme_win_lower_get(int wid)
{
	int ret = win_lower_get(wid);

	if (ret)
		return Sinteger(ret);

	return Sfalse;
}

ptr scheme_win_right_get(int wid)
{
	int ret = win_right_get(wid);

	if (ret)
		return Sinteger(ret);

	return Sfalse;
}

ptr scheme_win_left_get(int wid)
{
	int ret = win_left_get(wid);

	if (ret)
		return Sinteger(ret);

	return Sfalse;
}

ptr scheme_win_current_get(void)
{
	int ret = win_current_get();

	if (ret)
		return Sinteger(ret);

	return Sfalse;
}

int scheme_win_current_set(int wid)
{
	return win_current_set(wid);
}

ptr scheme_win_new(int bid)
{
	int ret = win_new(bid);

	if (ret)
		return Sinteger(ret);

	return Sfalse;
}

void scheme_win_del(int wid)
{
	return win_del(wid);
}

void scheme_win_close(int wid)
{
	return win_close(wid);
}

ptr scheme_win_title_get(int wid)
{
	return Sstring(win_title_get(wid));
}

int scheme_win_title_set(int wid, char *title)
{
	return win_title_set(wid, title);
}

int scheme_win_tag_set(int wid, int tag)
{
	return win_tag_set(wid, tag);
}

int scheme_win_tag_bits(int wid)
{
	return win_tag_bits(wid);
}

int scheme_win_tag_toggle(int wid, int tag)
{
	return win_tag_toggle(wid, tag);
}

int scheme_win_tag_add(int wid, int tag)
{
	return win_tag_add(wid, tag);
}

int scheme_win_tag_del(int wid, int tag)
{
	return win_tag_del(wid, tag);
}

int scheme_win_state_get(int wid)
{
	return win_state_get(wid);
}

int scheme_win_state_set(int wid, win_state_t st)
{
	return win_state_set(wid, st);
}

int scheme_win_state_toggle(int wid, win_state_t st)
{
	return win_state_toggle(wid, st);
}

ptr scheme_win_buf_get(int wid)
{
	int ret = win_buf_get(wid);

	if (ret)
		return Sinteger(ret);

	return Sfalse;
}

void scheme_win_mark_highlight(int wid, bool enable)
{
	win_mark_highlight(wid, enable);
}

void scheme_win_popup(int wid, bool enable)
{
	win_popup(wid, enable);
}

void scheme_win_size_set(int wid, int width, int height)
{
	win_size_set(wid, width, height);
}

void scheme_win_border_set(int wid, bool enable)
{
	win_border_set(wid, enable);
}

void scheme_win_buf_switch(int wid, int bid)
{
	win_buf_switch(wid, bid);
}

ptr scheme_win_prev_selected(void)
{
	int ret = win_prev_selected();

	if (ret)
		return Sinteger(ret);
	return Sfalse;
}

ptr scheme_win_viewport_pos(int wid, char type)
{
	return Sinteger(win_viewport_pos(wid, type));
}

ptr scheme_win_viewport_coord(int wid, int pos)
{
	int l, x, y;
	int err;

	err = win_viewport_coord(wid, pos, &l, &x, &y);
	if (!err) {
		return Scons(Sinteger(x), Scons(Sinteger(y), Scons(Sinteger(l), Snil)));
	}

	return Sfalse;
}

ptr scheme_win_scroll(int wid, char type, int n)
{
	return Sinteger(win_scroll(wid, type, n));
}

void scheme_win_sidebar_set(int wid, int width)
{
	win_sidebar_set(wid, width);
}

ptr scheme_win_sidebar_get(int wid)
{
	return Sinteger(win_sidebar_get(wid));
}

void scheme_win_sidebar_draw(int wid, int x, int y, const char *text, short fg, short bg, int attr)
{
	win_sidebar_draw(wid, x, y, text, fg, bg, attr);
}

void scheme_win_update(int wid)
{
	win_update(wid);
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

void scheme_buf_del(int bid)
{
	buf_del(bid);
}

void scheme_buf_kmap_set(int bid, char *name)
{
	buf_kmap_set(bid, name);
}

ptr scheme_buf_kmap_get(int bid)
{
	int ret = buf_kmap_get(bid);

	if (ret)
		return Sinteger(ret);

	return Sfalse;
}

ptr scheme_buf_current_get(void)
{
	int ret = buf_current_get();

	if (ret)
		return Sinteger(ret);

	return Sfalse;
}

ptr scheme_buf_first_get(void)
{
	int ret = buf_first_get();

	if (ret)
		return Sinteger(ret);

	return Sfalse;
}

ptr scheme_buf_next_get(int bid)
{
	int ret = buf_next_get(bid);

	if (ret)
		return Sinteger(ret);

	return Sfalse;
}

ptr scheme_buf_name_get(int bid)
{
	return Sstring(buf_name_get(bid));
}

void scheme_buf_name_set(int bid, const char *name)
{
	buf_name_set(bid, name);
}

ptr scheme_buf_readonly_get(int bid)
{
	return Sboolean(buf_is_readonly(bid));
}

void scheme_buf_readonly_set(int bid, bool is_readonly)
{
	buf_readonly_set(bid, is_readonly);
}

ptr scheme_buf_by_name(const char *name)
{
	int ret = buf_by_name(name);

	if (ret)
		return Sinteger(ret);

	return Sfalse;
}
ptr scheme_buf_text_insert(int bid, const char *text)
{
	size_t pos = buf_text_insert(bid, text);

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
	size_t pos = buf_cursor_get(bid);

	if (pos != EPOS)
		return Sinteger(pos);

	return Sfalse;
}

void scheme_buf_cursor_set(int bid, size_t pos)
{
	buf_cursor_set(bid, pos);
}

void scheme_buf_text_input_enable(int bid, bool enable)
{
	buf_input_enable(bid, enable);
}

void scheme_buf_mode_name_set(int bid, char *mode)
{
	buf_mode_name_set(bid, mode);
}

void scheme_buf_state_name_set(int bid, char *mode)
{
	buf_state_name_set(bid, mode);
}

ptr scheme_buf_file_open(int bid, const char *file)
{
	return Sinteger(buf_file_open(bid, file));
}

void scheme_buf_file_set(int bid, const char *file)
{
	buf_file_set(bid, file);
}

ptr scheme_buf_file_get(int bid)
{
	char *name = buf_file_get(bid);

	if (name)
		return Sstring(name);
	return Sstring("");
}

ptr scheme_buf_save(int bid)
{
	return Sboolean(buf_save(bid));
}

void scheme_buf_mark_set(int bid, size_t pos)
{
	buf_mark_set(bid, pos);
}

ptr scheme_buf_mark_get(int bid)
{
	return Sinteger(buf_mark_get(bid));
}

void scheme_buf_mark_clear(int bid)
{
	buf_mark_clear(bid);
}

ptr scheme_buf_is_term(int bid)
{
	if (buf_is_term(bid))
		return Strue;
	return Sfalse;
}

ptr scheme_buf_is_visible(int bid)
{
	if (buf_is_visible(bid))
		return Strue;
	return Sfalse;
}

ptr scheme_buf_prop_style_add(int bid, int type, int fg, int bg, int attr, int start, int end)
{
	int ret = buf_prop_style_add(bid, type, fg, bg, attr, start, end);

	if (ret == 0)
		Sinteger(ret);
	return Sfalse;
}

ptr scheme_buf_prop_kmap_add(int bid, int kid, int start, int end)
{
	int ret = buf_prop_kmap_add(bid, kid, start, end);

	if (ret == 0)
		Sinteger(ret);
	return Sfalse;
}

void scheme_buf_prop_del(int bid, int type, int start, int end)
{
	buf_prop_del(bid, type, start, end);
}

ptr scheme_buf_env_get(int bid)
{
	void *env = buf_env_get(bid);

	if (env)
		return (ptr)env;
	return Sfalse;
}

void scheme_buf_snapshot(int bid)
{
	buf_snapshot(bid);
}

void scheme_buf_undo(int bid)
{
	buf_undo(bid);
}

void scheme_buf_redo(int bid)
{
	buf_redo(bid);
}

ptr scheme_buf_search_regex(int bid, size_t pos, const char *pattern, int dir)
{
	size_t ret = buf_search_regex(bid, pos, pattern, dir);

	if (ret != EPOS)
		return Sinteger(ret);
	return Sinteger(pos);
}

ptr scheme_buf_parser_set(int bid, const char *lang)
{
	int err = buf_parser_set(bid, lang);

	if (err)
		return Sfalse;
	return Strue;
}

ptr scheme_stx_lang_style_add(const char *lang, int fg, int bg, int attr, const char *match)
{
	int ret = stx_lang_style_add(lang, fg, bg, attr, match);

	if (ret == 0)
		return Strue;
	return Sfalse;
}

void scheme_stx_lang_style_del(const char *lang, const char *match)
{
	stx_lang_style_del(lang, match);
}

void scheme_stx_lang_style_clear(const char *lang)
{
	stx_lang_style_clear(lang);
}

ptr scheme_minibuf_create(void)
{
	int ret = minibuf_create();
	if (ret > 0)
		return Sinteger(ret);
	return Sfalse;
}

ptr scheme_topbar_create(void)
{
	int ret = topbar_create();
	if (ret > 0)
		return Sinteger(ret);
	return Sfalse;
}

ptr scheme_term_create(char *prog, char *title, char *cwd)
{
	int ret = term_create(prog, title, cwd);

	if (ret)
		return Sinteger(ret);

	return Sfalse;
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

int scheme_view_current_get(void)
{
	return view_current_get();
}

int scheme_view_current_set(int tag)
{
	return view_current_set(tag);
}

ptr scheme_view_name_get(int tag)
{
	return Sstring(view_name_get(tag));
}

int scheme_view_name_set(int tag, char *name)
{
	return view_name_set(tag, name);
}

ptr scheme_view_cwd_get(int tag)
{
	return Sstring(view_cwd_get(tag));
}

int scheme_view_cwd_set(int tag, char *cwd)
{
	return view_cwd_set(tag, cwd);
}

int scheme_layout_current_get(int tag)
{
	return layout_current_get(tag);
}

int scheme_layout_current_set(int tag, layout_t lay)
{
	return layout_current_set(tag, lay);
}

int scheme_layout_nmaster_get(int tag)
{
	return layout_nmaster_get(tag);
}

int scheme_layout_nmaster_set(int tag, int n)
{
	return layout_nmaster_set(tag, n);
}

float scheme_layout_fmaster_get(int tag)
{
	return layout_fmaster_get(tag);
}

int scheme_layout_fmaster_set(int tag, float f)
{
	return layout_fmaster_set(tag, f);
}

bool scheme_layout_sticky_get(int tag)
{
	return layout_sticky_get(tag);
}

int scheme_layout_sticky_set(int tag, bool is_sticky)
{
	return layout_sticky_set(tag, is_sticky);
}

ptr scheme_bind_key(char *key, bind_key_cb_t cb, int mid, char *tname)
{
	int ret = bind_key(key, cb, mid, tname);

	if (ret)
		return Sinteger(ret);

	return Sfalse;
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
	int ret = evt_fd_handler_add(fd, fn, NULL);

	if (ret == 0)
		return Sinteger(ret);
	return Sfalse;
}

void scheme_evt_fd_handler_del(int fd)
{
	evt_fd_handler_del(fd);
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

ptr scheme_process_is_alive(int pid)
{
	return Sboolean(process_is_alive(pid));
}

void scheme_do_quit(void)
{
	do_quit();
}

static void scheme_export_symbols(void)
{
	Sregister_symbol("cs_win_get_by_coord", scheme_win_get_by_coord);
	Sregister_symbol("cs_win_is_visible", scheme_win_is_visible);
	Sregister_symbol("cs_win_first_get", scheme_win_first_get);
	Sregister_symbol("cs_win_prev_get", scheme_win_prev_get);
	Sregister_symbol("cs_win_next_get", scheme_win_next_get);
	Sregister_symbol("cs_win_upper_get", scheme_win_upper_get);
	Sregister_symbol("cs_win_lower_get", scheme_win_lower_get);
	Sregister_symbol("cs_win_right_get", scheme_win_right_get);
	Sregister_symbol("cs_win_left_get", scheme_win_left_get);
	Sregister_symbol("cs_win_current_get", scheme_win_current_get);
	Sregister_symbol("cs_win_current_set", scheme_win_current_set);
	Sregister_symbol("cs_win_new", scheme_win_new);
	Sregister_symbol("cs_win_del", scheme_win_del);
	Sregister_symbol("cs_win_close", scheme_win_close);
	Sregister_symbol("cs_win_title_get", scheme_win_title_get);
	Sregister_symbol("cs_win_title_set", scheme_win_title_set);
	Sregister_symbol("cs_win_tag_set", scheme_win_tag_set);
	Sregister_symbol("cs_win_tag_bits", scheme_win_tag_bits);
	Sregister_symbol("cs_win_tag_toggle", scheme_win_tag_toggle);
	Sregister_symbol("cs_win_tag_add", scheme_win_tag_add);
	Sregister_symbol("cs_win_tag_del", scheme_win_tag_del);
	Sregister_symbol("cs_win_state_get", scheme_win_state_get);
	Sregister_symbol("cs_win_state_set", scheme_win_state_set);
	Sregister_symbol("cs_win_state_toggle", scheme_win_state_toggle);
	Sregister_symbol("cs_win_buf_get", scheme_win_buf_get);
	Sregister_symbol("cs_win_mark_highlight", scheme_win_mark_highlight);
	Sregister_symbol("cs_win_popup", scheme_win_popup);
	Sregister_symbol("cs_win_size_set", scheme_win_size_set);
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
	Sregister_symbol("cs_buf_del", scheme_buf_del);
	Sregister_symbol("cs_buf_kmap_get", scheme_buf_kmap_get);
	Sregister_symbol("cs_buf_kmap_set", scheme_buf_kmap_set);
	Sregister_symbol("cs_buf_current_get", scheme_buf_current_get);
	Sregister_symbol("cs_buf_first_get", scheme_buf_first_get);
	Sregister_symbol("cs_buf_next_get", scheme_buf_next_get);
	Sregister_symbol("cs_buf_name_get", scheme_buf_name_get);
	Sregister_symbol("cs_buf_name_set", scheme_buf_name_set);
	Sregister_symbol("cs_buf_readonly_get", scheme_buf_readonly_get);
	Sregister_symbol("cs_buf_readonly_set", scheme_buf_readonly_set);
	Sregister_symbol("cs_buf_by_name", scheme_buf_by_name);
	Sregister_symbol("cs_buf_text_insert", scheme_buf_text_insert);
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
	Sregister_symbol("cs_buf_state_name_set", scheme_buf_state_name_set);
	Sregister_symbol("cs_buf_cursor_get", scheme_buf_cursor_get);
	Sregister_symbol("cs_buf_cursor_set", scheme_buf_cursor_set);
	Sregister_symbol("cs_buf_file_open", scheme_buf_file_open);
	Sregister_symbol("cs_buf_file_set", scheme_buf_file_set);
	Sregister_symbol("cs_buf_file_get", scheme_buf_file_get);
	Sregister_symbol("cs_buf_save", scheme_buf_save);
	Sregister_symbol("cs_buf_mark_set", scheme_buf_mark_set);
	Sregister_symbol("cs_buf_mark_get", scheme_buf_mark_get);
	Sregister_symbol("cs_buf_mark_clear", scheme_buf_mark_clear);
	Sregister_symbol("cs_buf_is_term", scheme_buf_is_term);
	Sregister_symbol("cs_buf_is_visible", scheme_buf_is_visible);

	Sregister_symbol("cs_buf_prop_style_add", scheme_buf_prop_style_add);
	Sregister_symbol("cs_buf_prop_kmap_add", scheme_buf_prop_kmap_add);
	Sregister_symbol("cs_buf_prop_del", scheme_buf_prop_del);

	Sregister_symbol("cs_buf_env_get", scheme_buf_env_get);

	Sregister_symbol("cs_buf_snapshot", scheme_buf_snapshot);
	Sregister_symbol("cs_buf_undo", scheme_buf_undo);
	Sregister_symbol("cs_buf_redo", scheme_buf_redo);
	Sregister_symbol("cs_buf_search_regex", scheme_buf_search_regex);

	Sregister_symbol("cs_buf_parser_set", scheme_buf_parser_set);
	Sregister_symbol("cs_stx_lang_style_add", scheme_stx_lang_style_add);
	Sregister_symbol("cs_stx_lang_style_del", scheme_stx_lang_style_del);
	Sregister_symbol("cs_stx_lang_style_clear", scheme_stx_lang_style_clear);

	Sregister_symbol("cs_minibuf_create", scheme_minibuf_create);
	Sregister_symbol("cs_topbar_create", scheme_topbar_create);

	Sregister_symbol("cs_term_create", scheme_term_create);
	Sregister_symbol("cs_term_keys_send", scheme_term_keys_send);
	Sregister_symbol("cs_term_text_send", scheme_term_text_send);
	Sregister_symbol("cs_term_text_get", scheme_term_text_get);

	Sregister_symbol("cs_view_current_get", scheme_view_current_get);
	Sregister_symbol("cs_view_current_set", scheme_view_current_set);
	Sregister_symbol("cs_view_name_get", scheme_view_name_get);
	Sregister_symbol("cs_view_name_set", scheme_view_name_set);
	Sregister_symbol("cs_view_cwd_get", scheme_view_cwd_get);
	Sregister_symbol("cs_view_cwd_set", scheme_view_cwd_set);

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

	Sregister_symbol("cs_do_quit", scheme_do_quit);
}

static void *scheme_symb_resolver(keymap_symb_t type, char *name)
{
	ptr p = Stop_level_value(Sstring_to_symbol(name));

	if (Sunboundp(p))
		return NULL;

	return (void *)Sinteger32_value(p);
}

int scheme_init(const char *init_script)
{
	int err;

	Sscheme_init(NULL);
	Sregister_boot_file("/usr/lib/csv"VERSION"/"MACHINE_TYPE"/petite.boot");
	Sregister_boot_file("/usr/lib/csv"VERSION"/"MACHINE_TYPE"/scheme.boot");
	Sbuild_heap(NULL, NULL);

	CALL1("source-directories", Scons(Sstring(LIB_PATH), Snil));

	scheme_export_symbols();

	err = scheme_run_script(LIB_PATH"/main.ss");
	if (err)
		return err;

	if (init_script && strlen(init_script))
		err = scheme_run_script(init_script);
	else
		err = scheme_run_init_script();
	if (err)
		return err;

	err = fifo_create();
	if (err) {
		fprintf(stderr, "failed to create fifo\n");
		return err;
	}

	keymap_symb_resolver_set(scheme_symb_resolver);

	scheme_initialized = 1;
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

	Scall2(Stop_level_value(Sstring_to_symbol("__on-event-handler")),
			Sinteger(evt.eid),
			Sinteger(evt.oid));
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
