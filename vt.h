/*
 * Copyright © 2004 Bruno T. C. de Oliveira
 * Copyright © 2006 Pierre Habouzit
 * Copyright © 2008-2013 Marc André Tanner
 * Copyright © 2021      Vadym Kochan
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF MIND, USE, DATA OR PROFITS, WHETHER
 * IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING
 * OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
#ifndef VT_H
#define VT_H

#include <curses.h>
#include <stdbool.h>
#include <sys/types.h>

#include "window.h"
#include "ui/ui.h"

#ifndef NCURSES_MOUSE_VERSION
#define mmask_t unsigned long
#endif

typedef struct Vt Vt;
typedef void (*vt_title_handler_t)(Vt*, const char *title);
typedef void (*vt_urgent_handler_t)(Vt*);

void vt_init(void);
void vt_shutdown(void);

void vt_title_handler_set(Vt*, vt_title_handler_t);
void vt_urgent_handler_set(Vt*, vt_urgent_handler_t);
void vt_data_set(Vt*, void *);
void *vt_data_get(Vt*);

Vt *vt_create(void);
void vt_size_get(Vt *vt, int *rows, int *cols);
void vt_attach(Vt *vt, Window *win);
void vt_draw(UiWin *win);
void vt_resize(Vt*, int rows, int cols);
void vt_destroy(Vt*);
int vt_pty_get(Vt*);
void vt_pty_set(Vt*, int);
bool vt_cursor_visible(Vt*);

int vt_process(Vt *);
void vt_keypress(Vt *, int keycode);
ssize_t vt_write(Vt*, const char *buf, size_t len);
void vt_mouse(Vt*, int x, int y, mmask_t mask);
void vt_dirty(Vt*);

void vt_scroll(Vt*, int rows);
void vt_noscroll(Vt*);

pid_t vt_pid_get(Vt*);
void vt_pid_set(Vt*, pid_t);
size_t vt_content_get(Vt*, char **s, bool colored);
size_t vt_current_line_get(Vt*, char **s);
int vt_content_start(Vt*);
void vt_filter_set(Vt *, void (*h)(Vt *vt, char *ch, size_t len, void *arg), void *arg);

#endif /* VT_H */
