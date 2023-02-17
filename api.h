#ifndef API_H
#define API_H

#include <stddef.h>
#include <stdbool.h>

#include "window.h"
#include "common.h"
#include "style.h"

typedef struct Buffer Buffer;

typedef enum {
	WIN_STATE_MINIMIZED = 0,
	WIN_STATE_MAXIMIZED = 1,
	WIN_STATE_MASTER    = 2,
} win_state_t;

typedef void (*bind_key_cb_t)(void);

void eprint(const char *errstr, ...);

#ifdef NDEBUG
 #define debug(format, args...)
#else
 #define debug eprint
#endif

extern Ui *g_ui;

int win_new(int bid);
int win_state_set(int wid, win_state_t st);
int win_state_toggle(int wid, win_state_t st);
win_state_t win_state_get(int wid);
void win_size_set(int wid, int width, int height);
int win_size_get(int wid, int *width, int *height);
void win_border_set(int wid, bool enable);
void win_buf_switch(int wid, int bid);
void win_update(int wid);

Style *style_new(void);

int style_add(Style *style);
int style_update(int id, Style *update);
Style *style_get_by_id(int id);
Style *style_get_by_name(const char *name);

int buf_new(char *name);
void buf_del(int bid);
int buf_kmap_set(int bid, char *name);
int buf_kmap_get(int bid);
void buf_name_set(int bid, const char *name);
char *buf_name_get(int bid);
void buf_readonly_set(int bid, bool is_readonly);
bool buf_is_readonly(int bid);
int buf_by_name(const char *name);
size_t buf_text_insert(int bid, const char *text);
size_t buf_text_insert_char(int bid, char ch);
size_t buf_text_insert_nl(int bid, int pos);
size_t buf_text_insert_file(int bid, const char *path);
size_t buf_text_obj_move(int bid, size_t pos, char obj, int n, bool move);
int buf_text_obj_range(int bid, size_t pos, char obj, int *start, int *end, bool inner);
size_t buf_text_range_del(int bid, int start, int end);
char *buf_text_get(int bid, int start, int len);
void buf_text_fg_set(int bid, short fg);
int buf_text_fg_get(int bid);
void buf_text_bg_set(int bid, short bg);
short buf_text_bg_get(int bid);
void buf_text_style_set(int bid, int style);
int buf_text_style_get(int bid);
size_t buf_cursor_get(int bid);
void buf_cursor_set(int bid, size_t pos);
size_t buf_line_num(int bid, size_t pos);
void buf_input_enable(int bid, bool enable);
void buf_mode_name_set(int bid, char *name);
char *buf_mode_name_get(int bid);
void buf_state_name_set(int bid, char *name);
int buf_file_open(int bid, const char *file);

int buf_save(int bid);
void buf_mark_set(int bid, size_t pos);
bool buf_mark_is_set(int bid);
size_t buf_mark_get(int bid);
void buf_mark_clear(int bid);
bool buf_is_term(int bid);
void buf_term_set(int bid, pid_t pid);
bool buf_is_visible(int bid);

int buf_prop_style_add(int bid, int type, int fg, int bg, int attr, const char *style_name, int start, int end,
		       const char *regex, char *name, bool expand);
int buf_prop_kmap_add(int bid, int kid, int start, int end, const char *regex, char *name);
int buf_prop_symbol_add(int bid, const char *symbol, int start, int end, const char *regex, char *name);
int buf_prop_data_add(int bid, void *data, int start, int end, const char *regex, char *name, void (*free_fn)(void *data));
void buf_prop_del(int bid, int type, int start, int end, const char *regex, char *name);
void buf_prop_walk(int bid, int type, int start, int end, char *name, void *arg,
		void (*cb)(Buffer *buf, int id, size_t start, size_t end, void *data,
				 void *arg));

int term_keys_send(int bid, char *keys);
int term_text_send(int bid, char *text);
int term_text_get(int bid, char **buf, size_t *len);
int term_current_line_get(int bid, char **buf, size_t *len);
int term_filter_enable(int bid, bool enable);

int fifo_create(void);

void do_quit(void);

int scheme_init(const char *);
void scheme_uninit(void);
int scheme_event_handle(event_t evt);
int scheme_eval_file(const char *scm_in, const char *out);
void *scheme_env_alloc(void);
void scheme_env_free(void *env);

#endif /* API_H */
