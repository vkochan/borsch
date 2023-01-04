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

typedef struct Window Window;
struct Window {
	Buffer *prev_buf;
	Buffer *buf;
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
};

#define CWD_MAX		256

typedef struct {
	int nmaster;
	float mfact;
	Layout *layout;
	Layout *layout_prev;
	char *cwd;
	char *name;
	bool msticky;
	Window *popup;
	Window *sel;
	Window *lastsel;
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
int layout_nmaster_get(int tab);
int layout_nmaster_set(int tab, int n);
float layout_fmaster_get(int tab);
int layout_fmaster_set(int tab, float mfact);
bool layout_sticky_get(int tab);

Tab *tab_get(int tab);
int tab_current_id_get(void);
void tab_current_id_set(int tab);

Frame *frame_get(int fid);
Frame *frame_current(void);
int frame_current_set(int tab);
const char *frame_name_get(int tab);
int frame_name_set(int tab, char *name);
char *frame_cwd_get(int tab);
int frame_cwd_set(int tab, char *cwd);

#define for_each_window(__w) \
	for (__w = window_first(); __w; __w = __w->next)

#define for_each_window_except_last(__w) \
	for (__w = window_first(); __w && __w->next; __w = __w->next)

#define for_each_window_master(__m) \
	for (int __n = ({__m = window_first();0;}); __m && __n < layout_current_nmaster(); __m = __m->next, __n++)

void windows_init(Ui *ui);
void windows_cleanup(void);

Window *window_current(void);
Window *window_popup_get(void);
void *window_popup_set(Window *p);
Window *window_first(void);
void window_first_set(Window *w);
void window_insert_first(Window *c);
void window_insert_after(Window *c, Window *a);
void window_insert(Window *c);
bool window_is_master(Window *w);
bool window_is_master_sticky(Window *c);
Window *window_stack(void);
void window_stack_insert(Window *c);
void window_stack_remove(Window *c);
void window_move_resize(Window *c, int x, int y, int w, int h);
void window_draw_title(Window *c);

#endif /* WINDOW_H */
