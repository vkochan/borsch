#ifndef UI_H
#define UI_H

#include <stdbool.h>
#include <stdarg.h>
#include <stdint.h>

#include "ui/style.h"

/* enable large file optimization for files larger than: */
#define UI_LARGE_FILE_SIZE (1 << 25)
/* enable large file optimization for files containing lines longer than: */
#define UI_LARGE_FILE_LINE_SIZE (1 << 16)

typedef struct Ui Ui;
typedef struct UiWin UiWin;

enum UiEventType {
   UiEventType_KeyPress = 1,
};

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

#define CONFIG_CURSES 1
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
	int (*height_get)(Ui*);
	int (*width_get)(Ui*);
	UiWin* (*window_new)(Ui*, View *view);
	void (*window_focus)(UiWin*, bool);
	void (*window_free)(UiWin*);
	void (*window_draw)(UiWin *);
	void (*window_redraw)(UiWin*);
	void (*window_refresh)(UiWin*);
	void (*window_clear)(UiWin*);
	void (*window_resize)(UiWin*, int width, int height);
	void (*window_move)(UiWin*, int x, int y);
	void (*window_draw_text)(UiWin *win, int x, int y, const char *text, int n);
	void (*window_draw_char_attr)(UiWin *win, int x, int y, unsigned ch, int n,
				      short fg, short bg, ui_text_style_t style);
	void (*window_draw_text_attr)(UiWin *win, int x, int y, const char *text, int n,
				      short fg, short bg, ui_text_style_t style);
	void (*draw)(Ui*);
	void (*draw_char)(Ui *ui, int x, int y, unsigned int ch, int n);
	void (*draw_char_vert)(Ui *ui, int x, int y, unsigned int ch, int n);
	void (*draw_wchar)(Ui *ui, int x, int y, wchar_t ch, short fg, short bg, ui_text_style_t style);
	void (*redraw)(Ui*);
	bool (*resize)(Ui*);
	void (*clear)(Ui*);
	void (*update)(Ui*);
	void (*refresh)(Ui*);
	short (*colors_max_get)(Ui *ui);
	CellStyle (*get_default_cell_style)(Ui *ui);

	int (*event_handler_cb)(Ui *ui, enum UiEventType type, void *evt);
	void (*event_process)(Ui *ui);
};

struct UiWin {
	Ui *ui;
	View *view;
	void *priv;
	int x, y;
	int curs_x, curs_y;
	bool curs_disable;
	int width, height;
	bool has_title;
	unsigned curr_style;
	short curr_fg, curr_bg;
	char title[256];
	bool has_border;
	int sidebar_width;
	bool is_focused;
	CellStyle (*style_get)(UiWin*, enum UiStyle);
	void (*status)(UiWin*, const char *txt);
	void (*options_set)(UiWin*, enum UiOption);
	enum UiOption (*options_get)(UiWin*);
	bool (*style_define)(UiWin*, int id, const char *style);
	void (*draw)(UiWin *);
	void (*on_view_update)(UiWin *win);
	void (*on_resize)(UiWin *win);
};

Ui *ui_term_new(void);
Ui *ui_x_new(void);

int ui_init(Ui *ui);
void ui_free(Ui *ui);
void ui_redraw(Ui *ui);
bool ui_resize(Ui *ui);
void ui_clear(Ui *ui);
void ui_update(Ui *ui);
void ui_event_handler_set(Ui *ui, int (*cb)(Ui *ui, enum UiEventType type, void *evt));
void ui_event_process(Ui *ui);
void ui_refresh(Ui *ui);
int ui_height_get(Ui *ui);
int ui_width_get(Ui *ui);
void ui_draw_char(Ui *ui, int x, int y, unsigned int ch, int n);
void ui_draw_char_vert(Ui *ui, int x, int y, unsigned int ch, int n);
void ui_draw_wchar(Ui *ui, int x, int y, wchar_t ch, short fg, short bg, ui_text_style_t style);
short ui_colors_max_get(Ui *ui);

UiWin *ui_window_new(Ui *ui, View *view);
void ui_window_free(UiWin *win);
void ui_window_cursor_set(UiWin *win, int x, int y);
void ui_window_cursor_get(UiWin *win, int *x, int *y);
void ui_window_cursor_disable(UiWin *win, bool disable);
bool ui_window_is_cursor_disabled(UiWin *win);
void ui_window_draw(UiWin *win);
void ui_window_draw_text(UiWin *win, int x, int y, const char *text, int n);
void ui_window_draw_char_attr(UiWin *win, int x, int y, unsigned ch, int n,
			      short fg, short bg, ui_text_style_t style);
void ui_window_draw_text_attr(UiWin *win, int x, int y, const char *text, int n,
			      short fg, short bg, ui_text_style_t style);
void ui_window_redraw(UiWin *win);
void ui_window_refresh(UiWin *win);
void ui_window_clear(UiWin *win);
void ui_window_resize(UiWin *win, int width, int height);
void ui_window_move(UiWin *win, int x, int y);
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
void ui_window_text_fg_set(UiWin *win, short fg);
void ui_window_text_bg_set(UiWin *win, short bg);
void ui_window_text_style_set(UiWin *win, ui_text_style_t style);
short ui_window_text_fg_get(UiWin *win);
short ui_window_text_bg_get(UiWin *win);
short ui_window_text_style_get(UiWin *win);
void ui_window_on_view_update_set(UiWin *win, void (*cb)(UiWin *win));
void ui_window_on_resize_set(UiWin *win, void (*cb)(UiWin *win));
void ui_window_border_enable(UiWin *win, bool enable);
bool ui_window_border_is_enabled(UiWin *win);
void ui_window_sidebar_width_set(UiWin *win, int width);
int ui_window_sidebar_width_get(UiWin *win);
void ui_window_sidebar_draw(UiWin *win, int x, int y, const char *text,
			    short fg, short bg, ui_text_style_t style);
void ui_window_has_title_set(UiWin *win, bool has_title);
bool ui_window_has_title(UiWin *win);

void ui_window_update(UiWin *win);

void ui_window_focus(UiWin *win, bool focus);
bool ui_window_is_focused(UiWin *win);

#endif
