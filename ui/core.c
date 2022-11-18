#include <stdlib.h>
#include <string.h>

#include "ui/ui.h"

int ui_init(Ui *ui)
{
	if (ui->init)
		return ui->init(ui);

	return 0;
}

void ui_free(Ui *ui)
{
	if (ui->free)
		ui->free(ui);
	else
		free(ui);
}

void ui_redraw(Ui *ui)
{
	if (ui->redraw)
		ui->redraw(ui);
}

bool ui_resize(Ui *ui)
{
	if (ui->resize)
		return ui->resize(ui);

	return false;
}

void ui_clear(Ui *ui)
{
	if (ui->clear)
		ui->clear(ui);
}

void ui_update(Ui *ui)
{
	if (ui->update)
		ui->update(ui);
}

void ui_event_handler_set(Ui *ui, int (*cb)(Ui *ui, enum UiEventType type, void *evt))
{
	ui->event_handler_cb = cb;
}

void ui_event_process(Ui *ui)
{
	if (ui->event_process)
		ui->event_process(ui);
}

void ui_refresh(Ui *ui)
{
	if (ui->refresh)
		ui->refresh(ui);
}

int ui_height_get(Ui *ui)
{
	if (ui->height_get)
		return ui->height_get(ui);
	return 0;
}

int ui_width_get(Ui *ui)
{
	if (ui->width_get)
		return ui->width_get(ui);
	return 0;
}

void ui_draw_char(Ui *ui, int x, int y, unsigned int ch, int n)
{
	if (ui->draw_char)
		ui->draw_char(ui, x, y, ch, n);
}

void ui_draw_char_vert(Ui *ui, int x, int y, unsigned int ch, int n)
{
	if (ui->draw_char_vert)
		ui->draw_char_vert(ui, x, y, ch, n);
}

short ui_colors_max_get(Ui *ui)
{
	if (ui->colors_max_get)
		return ui->colors_max_get(ui);

	return 8;
}

static CellStyle ui_window_style_get(UiWin *win, enum UiStyle style)
{
	CellStyle st = {
		.attr = win->curr_style,
		.fg = win->curr_fg,
		.bg = win->curr_bg,
	};

	return win->ui->get_default_cell_style(win->ui);
}

UiWin *ui_window_new(Ui *ui, View *view)
{
	UiWin *win = NULL;

	if (ui->window_new) {
		win = ui->window_new(ui, view);
		if (win) {
			win->style_get = ui_window_style_get;
			view_ui(view, win);
			win->view = view;
			win->ui = ui;
		}
	}

	return win;
}

void ui_window_free(UiWin *win)
{
	if (win->ui->window_free)
		win->ui->window_free(win);
	else
		free(win);
}

void ui_window_cursor_set(UiWin *win, int x, int y)
{
	win->curs_x = x;
	win->curs_y = y;
}

void ui_window_cursor_get(UiWin *win, int *x, int *y)
{
	*x = win->curs_x;
	*y = win->curs_y;
}

void ui_window_cursor_disable(UiWin *win, bool disable)
{
	win->curs_disable = disable;
}

bool ui_window_is_cursor_disabled(UiWin *win)
{
	return win->curs_disable;
}

void __ui_window_draw(UiWin *win, bool force)
{
	if (force)
		view_invalidate(win->view);

	if (view_update(win->view) || win->draw) {
		if (win->draw)
			win->draw(win);
		else if (win->ui->window_draw)
			view_draw(win->view);

		win->ui->window_draw(win);
	}
}

void ui_window_draw(UiWin *win)
{
	__ui_window_draw(win, false);
}

void ui_window_redraw(UiWin *win)
{
	if (win->ui->window_redraw)
		win->ui->window_redraw(win);
}

void ui_window_refresh(UiWin *win)
{
	if (win->ui->window_refresh)
		win->ui->window_refresh(win);
}

void ui_window_clear(UiWin *win)
{
	if (win->ui->window_clear)
		win->ui->window_clear(win);
}

void ui_window_resize(UiWin *win, int width, int height)
{
	if (width != win->width || height != win->height) {
		int border = win->has_border;

		if (height > 0)
			win->height = height;
		if (width > 0)
			win->width = width;

		view_resize(win->view, win->width - win->sidebar_width - (border*2),
				       win->height - (border + win->has_title));

		if (win->ui->window_resize)
			win->ui->window_resize(win, win->width, win->height);

		ui_window_clear(win);
	}
}

void ui_window_move(UiWin *win, int x, int y)
{
	if (x != win->x || y != win->y) {
		win->x = x;
		win->y = y;

		if (win->ui->window_move)
			win->ui->window_move(win, x, y);
	}
}

void ui_window_draw_text(UiWin *win, int x, int y, const char *text, int n)
{
	int skip_x = win->has_border;
	int skip_y = win->has_border;

	if (win->ui->window_draw_text)
		win->ui->window_draw_text(win, x+skip_x, y+skip_y, text, n);
}

void ui_window_draw_char_attr(UiWin *win, int x, int y, unsigned ch, int n,
			      short fg, short bg, ui_text_style_t style)
{
	int skip_x = win->has_border;
	int skip_y = win->has_border;

	if (win->ui->window_draw_char_attr)
		win->ui->window_draw_char_attr(win, x+skip_x, y+skip_y, ch, n, fg, bg, style);
}

void ui_window_draw_text_attr(UiWin *win, int x, int y, const char *text, int n,
			      short fg, short bg, ui_text_style_t style)
{
	int skip_x = win->has_border;
	int skip_y = win->has_border;

	if (win->ui->window_draw_text_attr)
		win->ui->window_draw_text_attr(win, x+skip_x, y+skip_y, text, n, fg, bg, style);
}

void ui_window_title_set(UiWin *win, const char *title)
{
	strncpy(win->title, title, sizeof(win->title));
}

char *ui_window_title_get(UiWin *win)
{
	return win->title;
}

void ui_window_width_set(UiWin *win, int width)
{
	ui_window_resize(win, width, -1);
}

int ui_window_width_get(UiWin *win)
{
	return win->width;
}

void ui_window_height_set(UiWin *win, int height)
{
	ui_window_resize(win, -1, height);
}

int ui_window_height_get(UiWin *win)
{
	return win->height;
}

int ui_window_x_get(UiWin *win)
{
	return win->x;
}

int ui_window_y_get(UiWin *win)
{
	return win->y;
}

void ui_window_priv_set(UiWin *win, void *priv)
{
	win->priv = priv;
}

void *ui_window_priv_get(UiWin *win)
{
	return win->priv;
}

void ui_window_ops_draw_set(UiWin *win, void (*fn)(UiWin *))
{
	win->draw = fn;
}

void *ui_window_ops_draw_get(UiWin *win)
{
	return win->draw;
}

void ui_window_text_fg_set(UiWin *win, short fg)
{
	win->curr_fg = fg;
}

void ui_window_text_bg_set(UiWin *win, short bg)
{
	win->curr_bg = bg;
}

void ui_window_text_style_set(UiWin *win, ui_text_style_t style)
{
	win->curr_style = style;
}

short ui_window_text_fg_get(UiWin *win)
{
	return win->curr_fg;
}

short ui_window_text_bg_get(UiWin *win)
{
	return win->curr_bg;
}

short ui_window_text_style_get(UiWin *win)
{
	return win->curr_style;
}

void ui_window_on_view_update_set(UiWin *win, void (*cb)(UiWin *))
{
	win->on_view_update = cb;
}

void ui_window_border_enable(UiWin *win, bool enable)
{
	if (win->has_border != enable)
		view_invalidate(win->view);

	win->has_border = enable;
}

bool ui_window_border_is_enabled(UiWin *win)
{
	return win->has_border;
}

void ui_window_sidebar_width_set(UiWin *win, int width)
{
	if (win->sidebar_width != width) {
		int border = win->has_border;

		win->sidebar_width = width;

		/* just to resize only view */
		ui_window_resize(win, -1, -1);
		__ui_window_draw(win, true);
	}
}

int ui_window_sidebar_width_get(UiWin *win)
{
	return win->sidebar_width;
}

void ui_window_sidebar_draw(UiWin *win, int x, int y, const char *text,
			    short fg, short bg, ui_text_style_t style)
{
	int skip_x = win->has_border;
	int skip_y = win->has_border;

	if (win->ui->window_draw_text_attr)
		win->ui->window_draw_text_attr(win, x+skip_x, y+skip_y, text,
				strlen(text), fg, bg, style);
}

void ui_window_has_title_set(UiWin *win, bool has_title)
{
	win->has_title = has_title;
}

bool ui_window_has_title(UiWin *win)
{
	return win->has_title;
}

void ui_window_update(UiWin *win)
{
	__ui_window_draw(win, true);
}

void ui_window_focus(UiWin *win, bool focus)
{
	win->is_focused = focus;
}

bool ui_window_is_focused(UiWin *win)
{
	return win->is_focused;
}
