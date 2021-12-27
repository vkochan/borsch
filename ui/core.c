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

void ui_resize(Ui *ui)
{
	if (ui->resize)
		ui->resize(ui);
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

short ui_color_make(Ui *ui, short fg, short bg)
{
	if (ui->color_make)
		return ui->color_make(ui, fg, bg);
	else
		return -1;
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

static CellStyle ui_window_style_get(UiWin *win, enum UiStyle style)
{
	CellStyle st = {
		.attr = win->curr_style,
		.fg = win->curr_fg,
		.bg = win->curr_bg,
	};

	return st;
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
	if (win->ui->window_cursor_set)
		win->ui->window_cursor_set(win, x, y);
}

void ui_window_cursor_get(UiWin *win, int *x, int *y)
{
	if (win->ui->window_cursor_get)
		win->ui->window_cursor_get(win, x, y);
}

void ui_window_draw(UiWin *win)
{
	if (view_update(win->view)) {
		view_draw(win->view);

		if (win->draw)
			win->draw(win);
		else if (win->ui->window_draw)
			win->ui->window_draw(win);
	}
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

void ui_window_resize(UiWin *win, int width, int height)
{
	if (width != win->width || height != win->height) {

		win->height = height;
		win->width = width;

		view_resize(win->view, width, height-1);

		if (win->resize)
			win->resize(win, width, height);
		else if (win->ui->window_resize)
			win->ui->window_resize(win, width, height);
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

void ui_window_draw_char(UiWin *win, int x, int y, unsigned int ch, int n)
{
	if (win->ui->window_draw_char)
		win->ui->window_draw_char(win, x, y, ch, n);
}

void ui_window_draw_text(UiWin *win, int x, int y, const char *text, int n)
{
	if (win->ui->window_draw_text)
		win->ui->window_draw_text(win, x, y, text, n);
}

void ui_window_draw_char_attr(UiWin *win, int x, int y, unsigned ch, int n,
			      short fg, short bg, ui_text_style_t style)
{
	if (win->ui->window_draw_char_attr)
		win->ui->window_draw_char_attr(win, x, y, ch, n, fg, bg, style);
}

void ui_window_draw_text_attr(UiWin *win, int x, int y, const char *text, int n,
			      short fg, short bg, ui_text_style_t style)
{
	if (win->ui->window_draw_text_attr)
		win->ui->window_draw_text_attr(win, x, y, text, n, fg, bg, style);
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
	win->width = width;
}

int ui_window_width_get(UiWin *win)
{
	return win->width;
}

void ui_window_height_set(UiWin *win, int height)
{
	win->height = height;
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

void ui_window_ops_resize_set(UiWin *win, void (*fn)(UiWin *, int, int))
{
	win->resize = fn;
}

void *ui_window_ops_resize_get(UiWin *win)
{
	return win->resize;
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

void ui_cursor_enable(Ui *ui, bool enable)
{
	if (ui->cursor_enable)
		ui->cursor_enable(ui, enable);
}

short ui_colors_max_get(Ui *ui)
{
	if (ui->colors_max_get)
		return ui->colors_max_get(ui);

	return 8;
}
