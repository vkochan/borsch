#ifndef UI_H
#define UI_H

#include <stdbool.h>
#include <stdarg.h>
#include <stdint.h>

/* enable large file optimization for files larger than: */
#define UI_LARGE_FILE_SIZE (1 << 25)
/* enable large file optimization for files containing lines longer than: */
#define UI_LARGE_FILE_LINE_SIZE (1 << 16)

typedef struct Ui Ui;
typedef struct UiWin UiWin;

enum UiLayout {
	UI_LAYOUT_HORIZONTAL,
	UI_LAYOUT_VERTICAL,
};

enum UiOption {
	UI_OPTION_NONE = 0,
	UI_OPTION_LINE_NUMBERS_ABSOLUTE = 1 << 0,
	UI_OPTION_LINE_NUMBERS_RELATIVE = 1 << 1,
	UI_OPTION_SYMBOL_SPACE = 1 << 2,
	UI_OPTION_SYMBOL_TAB = 1 << 3,
	UI_OPTION_SYMBOL_TAB_FILL = 1 << 4,
	UI_OPTION_SYMBOL_EOL = 1 << 5,
	UI_OPTION_SYMBOL_EOF = 1 << 6,
	UI_OPTION_CURSOR_LINE = 1 << 7,
	UI_OPTION_STATUSBAR = 1 << 8,
	UI_OPTION_ONELINE = 1 << 9,
	UI_OPTION_LARGE_FILE = 1 << 10,
};

enum UiStyle {
	UI_STYLE_LEXER_MAX = 64,
	UI_STYLE_DEFAULT,
	UI_STYLE_CURSOR,
	UI_STYLE_CURSOR_PRIMARY,
	UI_STYLE_CURSOR_LINE,
	UI_STYLE_SELECTION,
	UI_STYLE_LINENUMBER,
	UI_STYLE_LINENUMBER_CURSOR,
	UI_STYLE_COLOR_COLUMN,
	UI_STYLE_STATUS,
	UI_STYLE_STATUS_FOCUSED,
	UI_STYLE_SEPARATOR,
	UI_STYLE_INFO,
	UI_STYLE_EOF,
	UI_STYLE_MAX,
};

#if CONFIG_CURSES
typedef uint64_t CellAttr;
typedef short CellColor;

static inline bool cell_color_equal(CellColor c1, CellColor c2) {
	return c1 == c2;
}
#else
typedef uint8_t CellAttr;
typedef struct {
	uint8_t r, g, b;
	uint8_t index;
} CellColor;

static inline bool cell_color_equal(CellColor c1, CellColor c2) {
	if (c1.index != (uint8_t)-1 || c2.index != (uint8_t)-1)
		return c1.index == c2.index;
	return c1.r == c2.r && c1.g == c2.g && c1.b == c2.b;
}

#endif

typedef struct {
	CellAttr attr;
	CellColor fg, bg;
} CellStyle;

#include "text/text.h"
#include "view.h"

struct Ui {
	int (*init)(Ui*);
	void (*free)(Ui*);
	void (*resize)(Ui*);
	UiWin* (*window_new)(Ui*, View *view);
	void (*window_free)(UiWin*);
	void (*window_cursor_set)(UiWin*, int x, int y);
	void (*window_cursor_get)(UiWin*, int *x, int *y);
	void (*window_draw)(UiWin *);
	void (*window_redraw)(UiWin*);
	void (*window_refresh)(UiWin*);
	void (*window_resize)(UiWin*, int width, int height);
	void (*window_move)(UiWin*, int x, int y);
	void (*window_text_color_set)(UiWin*, short fg, short bg);
	void (*window_text_attr_set)(UiWin*, unsigned attrs);
	void (*window_draw_char)(UiWin*, int x, int y, unsigned int ch, int n);
	void (*window_draw_text)(UiWin *win, int x, int y, const char *text, int n);
	short (*window_color_get)(UiWin *win, short fg, short bg);
	void (*arrange)(Ui*, enum UiLayout);
	void (*draw)(Ui*);
	void (*draw_char)(Ui *ui, int x, int y, unsigned int ch, int n);
	void (*draw_char_vert)(Ui *ui, int x, int y, unsigned int ch, int n);
	void (*redraw)(Ui*);
	void (*suspend)(Ui*);
	void (*resume)(Ui*);
	short (*color_make)(Ui *ui, short fg, short bg);
};

struct UiWin {
	Ui *ui;
	View *view;
	void *priv;
	int x, y;
	int width, height;
	unsigned defattrs;
	short deffg, defbg;
	char title[256];
	CellStyle (*style_get)(UiWin*, enum UiStyle);
	void (*status)(UiWin*, const char *txt);
	void (*options_set)(UiWin*, enum UiOption);
	enum UiOption (*options_get)(UiWin*);
	bool (*style_define)(UiWin*, int id, const char *style);
	void (*resize)(UiWin *, int, int);
	void (*draw)(UiWin *);
};

bool is_default_color(CellColor c);

Ui *ui_term_new(void);
int ui_init(Ui *ui);
void ui_free(Ui *ui);
short ui_color_make(Ui *ui, short fg, short bg);
void ui_draw_char(Ui *ui, int x, int y, unsigned int ch, int n);
void ui_draw_char_vert(Ui *ui, int x, int y, unsigned int ch, int n);
UiWin *ui_window_new(Ui *ui, View *view);
void ui_window_free(UiWin *win);
void ui_window_cursor_set(UiWin *win, int x, int y);
void ui_window_cursor_get(UiWin *win, int *x, int *y);
void ui_window_text_color_set(UiWin *win, short fg, short bg);
void ui_window_text_attr_set(UiWin *win, unsigned attrs);
void ui_window_draw(UiWin *win);
void ui_window_draw_char(UiWin *win, int x, int y, unsigned int ch, int n);
void ui_window_draw_text(UiWin *win, int x, int y, const char *text, int n);
void ui_window_redraw(UiWin *win);
void ui_window_refresh(UiWin *win);
void ui_window_resize(UiWin *win, int width, int height);
void ui_window_move(UiWin *win, int x, int y);
void ui_window_default_colors_set(UiWin *win, unsigned attrs, short fg, short bg);
unsigned ui_window_default_attrs_get(UiWin *win);
short ui_window_default_fg_get(UiWin *win);
short ui_window_default_bg_get(UiWin *win);
void ui_window_title_set(UiWin *win, const char *title);
char *ui_window_title_get(UiWin *win);
void ui_window_width_set(UiWin *win, int width);
int ui_window_width_get(UiWin *win);
void ui_window_height_set(UiWin *win, int height);
int ui_window_height_get(UiWin *win);
int ui_window_x_get(UiWin *win);
int ui_window_y_get(UiWin *win);
void ui_window_priv_set(UiWin *win, void *priv);
void *ui_window_priv_get(UiWin *win);
void ui_window_ops_draw_set(UiWin *win, void (*fn)(UiWin *));
void *ui_window_ops_draw_get(UiWin *win);
void ui_window_ops_resize_set(UiWin *win, void (*fn)(UiWin *, int, int));
void *ui_window_ops_resize_get(UiWin *win);

#endif
