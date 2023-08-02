#ifndef WINDOW_H
#define WINDOW_H

#include <stdbool.h>

#include "buffer.h"
#include "view.h"

typedef struct _Frame Frame;

typedef struct Window Window;
struct Window {
	Buffer *buf;
	Frame *frame;
	View *view;
	UiWin *win;
};

typedef struct _Frame {
	struct _Frame *next;
	struct _Frame *prev;
	int id;
} Frame;

Frame *frame_current(void);
int frame_current_id(void);
int frame_current_set(Frame *f);
Frame *frame_create(void);
void frame_delete(Frame *f);
Frame *frame_by_id(int fid);

void window_init(Ui *ui);
void window_cleanup(void);

Window *window_current(void);
bool window_is_master(Window *w);
void window_move_resize(Window *c, int x, int y, int w, int h);
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
Window *window_create(Buffer *buf, bool is_widget);

#endif /* WINDOW_H */
