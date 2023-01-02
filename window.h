#ifndef WINDOW_H
#define WINDOW_H

#include <stdbool.h>

#include "buffer.h"
#include "view.h"

typedef struct {
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

bool layout_is_changed(void);
void layout_changed(bool changed);

#endif /* WINDOW_H */
