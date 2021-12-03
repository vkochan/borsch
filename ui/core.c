#include <stdlib.h>

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

UiWin *ui_window_new(Ui *ui, View *view)
{
	UiWin *win = NULL;

	if (ui->window_new) {
		win = ui->window_new(ui, view);
		if (win) {
			win->deffg = -1;
			win->defbg = -1;
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
	view_draw(win->view);

	if (win->ui->window_draw)
		win->ui->window_draw(win);
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
	view_resize(win->view, width, height);

	if (win->ui->window_resize)
		win->ui->window_resize(win, width, height);
}

void ui_window_move(UiWin *win, int x, int y)
{
	win->x = x;
	win->y = y;

	if (win->ui->window_move)
		win->ui->window_move(win, x, y);
}

void ui_window_text_color_set(UiWin *win, short fg, short bg)
{
	if (win->ui->window_text_color_set)
		win->ui->window_text_color_set(win, fg, bg);
}

void ui_window_text_attr_set(UiWin *win, unsigned attrs)
{
	if (win->ui->window_text_attr_set)
		win->ui->window_text_attr_set(win, attrs);
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

void ui_window_default_colors_set(UiWin *win, unsigned attrs, short fg, short bg)
{
	win->defattrs = attrs;
	win->deffg = fg;
	win->defbg = bg;
}

unsigned ui_window_default_attrs_get(UiWin *win)
{
	return win->defattrs;
}

short ui_window_default_fg_get(UiWin *win)
{
	return win->deffg;
}

short ui_window_default_bg_get(UiWin *win)
{
	return win->defbg;
}
