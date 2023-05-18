#ifndef WINDOW_H
#define WINDOW_H

#include <stdbool.h>

#include "buffer.h"
#include "view.h"

typedef struct _Frame Frame;

typedef struct Window Window;
struct Window {
	Buffer *prev_buf;
	Buffer *buf;
	Frame *frame;
	View *view;
	UiWin *win;
	const char *cmd;
	unsigned short int id;
	bool urgent;
	Window *next;
	Window *prev;
	Window *snext;
	bool highlight_mark;
	bool pending_draw_evt;
	bool is_widget;
	bool is_new;
};

typedef struct _Frame {
	struct _Frame *next;
	struct _Frame *prev;
	int id;
	Window *sel;
	Window *windows;
} Frame;

Frame *frame_current(void);
int frame_current_id(void);
int frame_current_set(Frame *f);
Frame *frame_create(void);
void frame_delete(Frame *f);
Frame *frame_by_id(int fid);

#define for_each_window(__w) \
	for (__w = window_first(); __w; __w = __w->next)

#define WIN_DRAW_F_FORCE	(1 << 0)

#define WIN_POS_F_TOP		(1 << 0)
#define WIN_POS_F_BOT		(1 << 1)

void window_init(Ui *ui);
void window_cleanup(void);

void window_coord(Window *w, int *x, int *y);
Window *window_current(void);
Window *window_get_by_id(int id);
Window *windows_list(Frame *f);
Window *window_first(void);
void window_next_set(Window *w, Window *n);
void window_prev_set(Window *w, Window *n);
Window *window_prev(Window *w);
Window *window_next(Window *w);
void window_insert_first(Window *c);
void window_remove(Window *c);
bool window_is_master(Window *w);
bool window_is_widget(Window *w);
void window_move_resize(Window *c, int x, int y, int w, int h);
char *window_title_get(Window *c);
int window_viewport_pos(Window *w, char type);
int window_viewport_pos_to_coord(Window *w, int pos, int *l, int *x, int *y);
int window_viewport_size(Window *w, int *width, int *height);
void window_sidebar_width_set(Window *w, int width);
int window_sidebar_width(Window *w);
void window_update_cursor(Window *w);
int window_scroll(Window *w, char type, int n);
void window_focus(Window *c);
void window_delete(Window *w);
void window_buffer_switch(Window *w, Buffer *b);
Window *window_create(Buffer *buf, int x, int y, int width, int height);
Window *widget_create(Buffer *buf, int x, int y, int width, int height);

#endif /* WINDOW_H */
