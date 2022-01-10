#ifndef API_H
#define API_H

#include <stdbool.h>

typedef enum {
	WIN_STATE_MINIMIZED = 0,
	WIN_STATE_MAXIMIZED = 1,
	WIN_STATE_MASTER    = 2,
} win_state_t;

typedef enum {
	LAYOUT_TILED = 0,
	LAYOUT_GRID = 1,
	LAYOUT_BSTACK = 2,
	LAYOUT_MAXIMIZED = 3,
} layout_t;

typedef void (*bind_key_cb_t)(void);

int win_get_by_coord(int x, int y);
bool win_is_visible(int wid);
int win_first_get(void);
int win_prev_get(int wid);
int win_next_get(int wid);
int win_upper_get(int wid);
int win_lower_get(int wid);
int win_right_get(int wid);
int win_left_get(int wid);
int win_current_get(void);
int win_current_set(int wid);
int win_new(int bid);
void win_del(int wid);
void win_close(int wid);
char* win_title_get(int wid);
int win_title_set(int wid, char *title);
int win_tag_set(int wid, int tag);
int win_tag_add(int wid, int tag);
int win_tag_del(int wid, int tag);
int win_tag_toggle(int wid, int tag);
int win_state_set(int wid, win_state_t st);
int win_state_toggle(int wid, win_state_t st);
win_state_t win_state_get(int wid);
int win_buf_get(int wid);
void win_mark_highlight(int wid, bool enable);
void win_popup(int wid, bool enable);
void win_size_set(int wid, int width, int height);
void win_border_set(int wid, bool enable);
void win_buf_switch(int wid, int bid);
int win_prev_selected(void);

int kmap_add(int pid);
int kmap_parent_set(int kid, char *name);
void kmap_del(int kid);

int buf_kmap_set(int bid, char *name);
int buf_kmap_get(int bid);
int buf_current_get(void);
int buf_first_get(void);
int buf_next_get(int bid);
void buf_name_set(int bid, const char *name);
char *buf_name_get(int bid);
int buf_by_name(const char *name);
size_t buf_text_insert(int bid, const char *text);
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
void buf_input_enable(int bid, bool enable);
void buf_mode_set(int bid, char *name);
int buf_file_open(int bid, const char *file);
int buf_save(int bid);
void buf_mark_set(int bid, size_t pos);
size_t buf_mark_get(int bid);
void buf_mark_clear(int bid);
bool buf_is_term(int bid);
bool buf_is_visible(int bid);

int buf_prop_style_add(int bid, int type, int fg, int bg, int attr, int start, int end);
void buf_prop_del(int bid, int type, int start, int end);
/* void buf_prop_walk(int bid, int type, int start, int end, void *arg, */
/* 			void (*cb)(int bid, int type, int start, int end, void */
/* 				*arg)); */

void *buf_env_get(int bid);
void buf_env_set(int bid, void *env);

void buf_snapshot(int bid);
void buf_undo(int bid);
void buf_redo(int bid);

int term_create(char *prog, char *title);
int term_keys_send(int wid, char *keys);
int term_text_send(int wid, char *text);

int view_current_get(void);
int view_current_set(int tag);
const char *view_name_get(int tag);
int view_name_set(int tag, char *name);
char *view_cwd_get(int tag);
int view_cwd_set(int tag, char *cwd);
layout_t layout_current_get(int tag);
int layout_current_set(int tag, layout_t lay);
int layout_nmaster_get(int tag);
int layout_nmaster_set(int tag, int n);
float layout_fmaster_get(int tag);
int layout_fmaster_set(int tag, float f);
bool layout_sticky_get(int tag);
int layout_sticky_set(int tag, bool is_sticky);

int bind_key(char *map, bind_key_cb_t cb, int kid, char *tmap);
int unbind_key(char *map, int kid);

char *copy_buf_get(size_t *len);
int copy_buf_set(char *str);

int fifo_create(void);

int tagbar_status_set(const char *s);
int tagbar_status_align(int align);
int tagbar_show(bool show);

void do_quit(void);

#endif /* API_H */
