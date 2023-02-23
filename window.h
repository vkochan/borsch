#ifndef WINDOW_H
#define WINDOW_H

#include <stdbool.h>

#include "buffer.h"
#include "view.h"

typedef enum {
	LAYOUT_FIRST = 0,
	LAYOUT_TILED = LAYOUT_FIRST,
	LAYOUT_GRID = 1,
	LAYOUT_BSTACK = 2,
	LAYOUT_MAXIMIZED = 3,

	LAYOUT_MAX
} layout_t;

typedef struct {
	int id;
	const char *symbol;
	void (*arrange)(unsigned int, unsigned int, unsigned int, unsigned int);
} Layout;

typedef struct _Frame Frame;

typedef struct Window Window;
struct Window {
	Buffer *prev_buf;
	Buffer *buf;
	Frame *frame;
	View *view;
	UiWin *win;
	const char *cmd;
	int order;
	unsigned short int id;
	bool urgent;
	Window *next;
	Window *prev;
	Window *snext;
	bool highlight_mark;
	bool pending_draw_evt;
	bool is_widget;
	bool is_new;
	int pos_flags;
};

typedef struct _Frame {
	int nmaster;
	float mfact;
	Layout *layout;
	bool msticky;
	Window *sel;
	Window *windows;
	Window *stack;
} Frame;

typedef struct {
	Frame *f;
} Tab;

/* master width factor [0.1 .. 0.9] */
#define MFACT 0.5
/* number of windows in master area */
#define NMASTER 1

#define MAXTABS	9

Layout *layout_get(int id);
Layout *layout_current(void);
bool layout_is_changed(void);
void layout_changed(bool changed);
void layout_set_arrange(int id, void (*arrange)(unsigned int, unsigned int, unsigned int, unsigned int));
bool layout_is_arrange(int id);
layout_t layout_current_get(int tab);
int layout_current_set(int tab, layout_t lay);
int layout_current_nmaster(void);
float layout_current_fmaster(void);
unsigned int layout_current_x(void);
unsigned int layout_current_y(void);
void layout_current_move(unsigned int x, unsigned y);
unsigned int layout_current_width(void);
unsigned int layout_current_height(void);
void layout_current_resize(unsigned int width, unsigned height);
void layout_current_arrange(void);
int layout_nmaster_get(int tab);
int layout_nmaster_set(int tab, int n);
float layout_fmaster_get(int tab);
int layout_fmaster_set(int tab, float mfact);
bool layout_sticky_get(int tab);
int layout_sticky_set(int tab, bool is_sticky);

Tab *tab_get(int tab);
int tab_current_id_get(void);
void tab_current_id_set(int tab);

Frame *frame_get(int fid);
Frame *frame_current(void);
int frame_current_id(void);
int frame_current_set(int tab);

#define for_each_window(__w) \
	for (__w = window_first(); __w; __w = __w->next)

#define for_each_window_except_last(__w) \
	for (__w = window_first(); __w && __w->next; __w = __w->next)

#define for_each_window_master(__m) \
	for (int __n = ({__m = window_first();0;}); __m && __n < layout_current_nmaster(); __m = __m->next, __n++)

#define WIN_DRAW_F_FORCE	(1 << 0)
#define WIN_DRAW_F_NO_EVENT	(1 << 1)

#define WIN_POS_F_TOP		(1 << 0)
#define WIN_POS_F_BOT		(1 << 1)

void window_init(Ui *ui);
void window_cleanup(void);

Window *window_current(void);
Window *window_get_by_id(int id);
Window *window_get_by_coord(unsigned int x, unsigned int y);
Window *window_last_selected(void);
Window *windows_list_by_fid(int fid);
Window *window_first(void);
Window *window_upper(Window *w);
Window *window_lower(Window *w);
Window *window_left(Window *w);
Window *window_right(Window *w);
void window_first_set(Window *w);
void window_next_set(Window *w, Window *n);
void window_prev_set(Window *w, Window *n);
Window *window_prev(Window *w);
Window *window_next(Window *w);
void window_insert_first(Window *c);
void window_remove(Window *c);
bool window_is_master(Window *w);
bool window_is_master_sticky(Window *c);
bool window_is_widget(Window *w);
void window_move_resize(Window *c, int x, int y, int w, int h);
char *window_title_get(Window *c);
bool window_is_visible(Window *c);
int window_viewport_pos(Window *w, char type);
int window_viewport_pos_to_coord(Window *w, int pos, int *l, int *x, int *y);
int window_viewport_size(Window *w, int *width, int *height);
void window_sidebar_width_set(Window *w, int width);
int window_sidebar_width(Window *w);
void window_sidebar_draw(Window *w, int x, int y, const char *text, short fg, short bg, int attr);
void window_update(Window *w);
int window_scroll(Window *w, char type, int n);
void window_draw_title(Window *c);
void window_draw_flags(Window *c, int flags);
void window_draw(Window *c);
void window_focus(Window *c);
void window_delete(Window *w);
void window_close(Window *w);
void window_buffer_switch(Window *w, Buffer *b);
Window *window_create(Buffer *buf);
Window *widget_create(Buffer *buf, int x, int y, int width, int height, int pos_flags);
void window_draw_all(bool redraw);

#endif /* WINDOW_H */
