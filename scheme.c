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
#include "api.h"

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

ptr scheme_win_create(char *prog, char *title)
{
	int ret = win_create(prog, title);

	if (ret)
		return Sinteger(ret);

	return Sfalse;
}

ptr scheme_win_new(void)
{
	int ret = win_new();

	if (ret)
		return Sinteger(ret);

	return Sfalse;
}

void scheme_win_del(int wid)
{
	return win_del(wid);
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

int scheme_win_keys_send(int wid, char *text)
{
	return win_keys_send(wid, text);
}

int scheme_win_text_send(int wid, char *text)
{
	return win_text_send(wid, text);
}

ptr scheme_win_buf_get(int wid)
{
	int ret = win_buf_get(wid);

	if (ret)
		return Sinteger(ret);

	return Sfalse;
}

ptr scheme_kmap_add(char *parent)
{
	int ret = kmap_add(0);

	if (ret) {
		if (parent && strlen(parent))
			kmap_parent_set(ret, parent);
		return Sinteger(ret);
	}

	return Sfalse;
}

void scheme_kmap_parent_set(int kid, char *name)
{
	kmap_parent_set(kid, name);
}

void scheme_kmap_del(int kid)
{
	kmap_del(kid);
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

ptr scheme_buf_text_obj_pos(int bid, char obj, int n)
{
	size_t pos = buf_text_obj_move(bid, obj, n, false);

	if (pos != EPOS)
		return Sinteger(pos);

	return Sfalse;
}

ptr scheme_buf_text_range_del(int bid, int start, int end)
{
	size_t pos = buf_text_range_del(bid, start, end);

	if (pos != EPOS)
		return Sinteger(pos);

	return Sfalse;
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

void scheme_buf_mode_set(int bid, char *mode)
{
	buf_mode_set(bid, mode);
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

int scheme_tagbar_status_set(char *s)
{
	return tagbar_status_set(s);
}

int scheme_tagbar_status_align(int align)
{
	return tagbar_status_align(align);
}

int scheme_tagbar_show(bool show)
{
	return tagbar_show(show);
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

ptr scheme_copy_buf_get(void)
{
	size_t len;
	char *buf;
	ptr s;

	buf = copy_buf_get(&len);

	return Sstring_of_length(buf, len);
}

int scheme_copy_buf_set(char *str)
{
	return copy_buf_set(str);
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
	Sregister_symbol("cs_win_create", scheme_win_create);
	Sregister_symbol("cs_win_new", scheme_win_new);
	Sregister_symbol("cs_win_del", scheme_win_del);
	Sregister_symbol("cs_win_title_get", scheme_win_title_get);
	Sregister_symbol("cs_win_title_set", scheme_win_title_set);
	Sregister_symbol("cs_win_tag_set", scheme_win_tag_set);
	Sregister_symbol("cs_win_tag_toggle", scheme_win_tag_toggle);
	Sregister_symbol("cs_win_tag_add", scheme_win_tag_add);
	Sregister_symbol("cs_win_tag_del", scheme_win_tag_del);
	Sregister_symbol("cs_win_state_get", scheme_win_state_get);
	Sregister_symbol("cs_win_state_set", scheme_win_state_set);
	Sregister_symbol("cs_win_state_toggle", scheme_win_state_toggle);
	Sregister_symbol("cs_win_keys_send", scheme_win_keys_send);
	Sregister_symbol("cs_win_text_send", scheme_win_text_send);
	Sregister_symbol("cs_win_buf_get", scheme_win_buf_get);

	Sregister_symbol("cs_kmap_add", scheme_kmap_add);
	Sregister_symbol("cs_kmap_parent_set", scheme_kmap_parent_set);
	Sregister_symbol("cs_kmap_del", scheme_kmap_del);

	Sregister_symbol("cs_buf_kmap_get", scheme_buf_kmap_get);
	Sregister_symbol("cs_buf_kmap_set", scheme_buf_kmap_set);
	Sregister_symbol("cs_buf_current_get", scheme_buf_current_get);
	Sregister_symbol("cs_buf_first_get", scheme_buf_first_get);
	Sregister_symbol("cs_buf_next_get", scheme_buf_next_get);
	Sregister_symbol("cs_buf_name_get", scheme_buf_name_get);
	Sregister_symbol("cs_buf_name_set", scheme_buf_name_set);
	Sregister_symbol("cs_buf_by_name", scheme_buf_by_name);
	Sregister_symbol("cs_buf_text_insert", scheme_buf_text_insert);
	Sregister_symbol("cs_buf_text_insert_nl", scheme_buf_text_insert_nl);
	Sregister_symbol("cs_buf_text_insert_file", scheme_buf_text_insert_file);
	Sregister_symbol("cs_buf_text_obj_pos", scheme_buf_text_obj_pos);
	Sregister_symbol("cs_buf_text_range_del", scheme_buf_text_range_del);
	Sregister_symbol("cs_buf_text_input_enable", scheme_buf_text_input_enable);
	Sregister_symbol("cs_buf_mode_set", scheme_buf_mode_set);
	Sregister_symbol("cs_buf_cursor_get", scheme_buf_cursor_get);
	Sregister_symbol("cs_buf_cursor_set", scheme_buf_cursor_set);

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

	Sregister_symbol("cs_tagbar_status_align", scheme_tagbar_status_align);
	Sregister_symbol("cs_tagbar_status_set", scheme_tagbar_status_set);
	Sregister_symbol("cs_tagbar_show", scheme_tagbar_show);

	Sregister_symbol("cs_copy_buf_set", scheme_copy_buf_set);
	Sregister_symbol("cs_copy_buf_get", scheme_copy_buf_get);

	Sregister_symbol("cs_bind_key", scheme_bind_key);
	Sregister_symbol("cs_unbind_key", scheme_unbind_key);

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
