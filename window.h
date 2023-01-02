#ifndef WINDOW_H
#define WINDOW_H

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

#endif /* WINDOW_H */
