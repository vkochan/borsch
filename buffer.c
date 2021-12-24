#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <libgen.h>
#include <fcntl.h>

#include "view.h"
#include "keymap.h"
#include "text/text.h"

typedef struct {
	char *path;
} File;

typedef struct Buffer {
	int buf_id;
	struct Buffer *next;
	struct Buffer *prev;
	char kmap_name[64];
	KeyMap *keymap;
	size_t cursor;
	ui_text_style_t curr_style;
	short curr_fg;
	short curr_bg;
	Text *text;
	bool is_name_locked;
	char name[256];
	char mode[32];
	size_t ref_count;
	bool is_input_enabled;
	bool is_read_only;
	bool is_dirty;
	File file;
} Buffer;

static Buffer buf_list;
static int buf_count;

static int buffer_id_gen(void)
{
	Buffer *buf;
	int id;

	for (id = 1; id <= buf_count; id++) {
		for (buf = buf_list.next; buf && buf->buf_id != id; buf = buf->next)
			;

		if (!buf)
			return id;
	}

	return buf_count;
}

static void buffer_list_add(Buffer *head, Buffer *buf)
{
	buf->prev = head;
	buf->next = head->next;

	if (head->next)
		head->next->prev = buf;

	head->next = buf;
	buf_count++;
}

static void buffer_list_del(Buffer *buf)
{
	if (buf->prev)
		buf->prev->next = buf->next;
	if (buf->next)
		buf->next->prev = buf->prev;
	buf_count--;
}

Buffer *buffer_new(const char *name)
{
	Buffer *buf;

	buf = calloc(1, sizeof(*buf));
	if (!buf)
		return NULL;

	buf->text = text_load(NULL);
	if (!buf->text) {
		free(buf);
		return NULL;
	}
	buf->ref_count = 1;

	buf_count++;
	buf->buf_id = buffer_id_gen();

	if (name && strlen(name)) {
		strncpy(buf->name, name, sizeof(buf->name));
		buf->is_name_locked = true;
	} else {
		snprintf(buf->name, sizeof(buf->name), "new%d", buf->buf_id);
	}

	strncpy(buf->mode, "none", sizeof(buf->mode));

	buf->curr_style = UI_TEXT_STYLE_NORMAL;
	buf->curr_fg = UI_TEXT_COLOR_DEFAULT;
	buf->curr_bg = UI_TEXT_COLOR_DEFAULT;

	buffer_list_add(&buf_list, buf);

	return buf;
}

void buffer_del(Buffer *buf)
{
	if (buf->ref_count)
		buf->ref_count--;

	if (!buf->ref_count) {
		if (buf->keymap)
			keymap_ref_put(buf->keymap);
		buffer_list_del(buf);
		/* TODO: check if buffer is not saved and ask user to save it */
		text_free(buf->text);
		free(buf->file.path);
		free(buf);
	}
}

int buffer_file_open(Buffer *buf, const char *file)
{
	struct stat st;
	char tmp[256];
	char *fname;
	Text *text;
	int err;

	err = stat(file, &st);
	if (!err) {
		text = text_load(file);
		/* TODO show err message */
		if (!text)
			return -1;
	}

	/* close opened file */
	if (buf->file.path) {
		/* TODO show error message */
		if (text_modified(buf->text))
			text_save(buf->text, buf->file.path);
		free(buf->file.path);
		text_free(buf->text);
	}

	strncpy(tmp, file, sizeof(tmp));
	fname = basename(tmp);
	strncpy(buf->name, fname, sizeof(buf->name));

	buf->file.path = strdup(file);
	buf->text = text;
	buf->cursor = 0;

	return 0;
}

bool buffer_save(Buffer *buf)
{
	if (buf->is_read_only)
		return -1;

	return text_save(buf->text, buf->file.path);
}

bool buffer_is_modified(Buffer *buf)
{
	return text_modified(buf->text);
}

Buffer *buffer_first_get(void)
{
	return buf_list.next;
}

Buffer *buffer_next_get(Buffer *buf)
{
	return buf->next;
}

Text *buffer_text_get(Buffer *buf)
{
	return buf->text;
}

int buffer_id_get(Buffer *buf)
{
	return buf->buf_id;
}

Buffer *buffer_by_id(int bid)
{
	Buffer *buf;

	for (buf = buf_list.next; buf; buf = buf->next) {
		if (buf->buf_id == bid)
			return buf;
	}

	return NULL;
}


void buffer_cursor_set(Buffer *buf, size_t pos)
{
	buf->cursor = pos;
}

size_t buffer_cursor_get(Buffer *buf)
{
	return buf->cursor;
}

char *buffer_name_get(Buffer *buf)
{
	return buf->name;
}

void buffer_name_set(Buffer *buf, const char *name)
{
	strncpy(buf->name, name, sizeof(buf->name));
}

void buffer_name_lock(Buffer *buf, bool lock)
{
	buf->is_name_locked = lock;
}

bool buffer_name_is_locked(Buffer *buf)
{
	return buf->is_name_locked;
}

Buffer *buffer_by_name(const char *name)
{
	Buffer *buf;

	for (buf = buf_list.next; buf; buf = buf->next) {
		if (strcmp(buf->name, name) == 0)
			return buf;
	}

	return NULL;
}

void buffer_ref_get(Buffer *buf)
{
	buf->ref_count++;
}

void buffer_ref_put(Buffer *buf)
{
	if (buf->ref_count)
		buf->ref_count--;
}

void buffer_keymap_set(Buffer *buf, char *name)
{
	if (strcmp(buf->kmap_name, name) == 0)
		return;

	if (buf->keymap)
		keymap_ref_put(buf->keymap);

	if (name)
		strncpy(buf->kmap_name, name, sizeof(buf->kmap_name));

	buf->keymap = NULL;
}

KeyMap *buffer_keymap_get(Buffer *buf)
{
	if (buf->keymap)
		return buf->keymap;

	buf->keymap = keymap_by_name(buf->kmap_name);

	if (buf->keymap)
		keymap_ref_get(buf->keymap);

	return buf->keymap;
}

size_t buffer_text_insert(Buffer *buf, size_t pos, const char *text)
{
	size_t len;

	len = strlen(text);

	if (text_insert(buf->text, pos, text, len)) {
		buf->cursor = pos + len;
		buf->is_dirty = true;
		pos += len;
	} else {
		pos = EPOS;
	}

	return pos;
}

size_t buffer_text_insert_len(Buffer *buf, size_t pos, const char *text, size_t len)
{
	if (text_insert(buf->text, pos, text, len)) {
		buf->cursor = pos + len;
		buf->is_dirty = true;
		pos += len;
	} else {
		pos = EPOS;
	}

	return pos;
}

size_t buffer_text_insert_nl(Buffer *buf, size_t pos)
{
	Text *txt = buf->text;
	char byte;
	/* insert second newline at end of file, except if there is already one */
	bool eof = pos == text_size(txt);
	bool nl2 = eof && !(pos > 0 && text_byte_get(txt, pos-1, &byte) && byte == '\n');

	text_insert(txt, pos, "\n", 1);
	if (eof) {
		if (nl2)
			text_insert(txt, text_size(txt), "\n", 1);
		else
			pos--; /* place cursor before, not after nl */
	}
	pos++;

	buf->is_dirty = true;
	buf->cursor = pos;

	return pos;
}

size_t buffer_text_delete(Buffer *buf, size_t start, size_t end)
{
	size_t tmp;
	Text *txt;

	if (start > end) {
		tmp = end;
		end = start;
		start = tmp;
	}

	if (text_delete(buf->text, start, end - start)) {
		buf->is_dirty = true;
		buf->cursor = start;
	}

	return start;
}

char *buffer_text_extract(Buffer *buf, size_t pos, size_t len)
{
	return text_bytes_alloc0(buf->text, pos, len);
}

void buffer_text_input_enable(Buffer *buf, bool enable)
{
	buf->is_input_enabled = enable;
}

bool buffer_text_input_is_enabled(Buffer *buf)
{
	return buf->is_input_enabled;
}

void buffer_text_fg_set(Buffer *buf, short fg)
{
	if (fg != buf->curr_fg)
		buf->is_dirty = true;
	buf->curr_fg = fg;
}

short buffer_text_fg_get(Buffer *buf)
{
	return buf->curr_fg;
}

void buffer_text_bg_set(Buffer *buf, short bg)
{
	if (bg != buf->curr_bg)
		buf->is_dirty = true;
	buf->curr_bg = bg;
}

short  buffer_text_bg_get(Buffer *buf)
{
	return buf->curr_bg;
}

void buffer_text_style_set(Buffer *buf, ui_text_style_t style)
{
	if (style != buf->curr_style)
		buf->is_dirty = true;
	buf->curr_style = style;
}

ui_text_style_t buffer_text_style_get(Buffer *buf)
{
	return buf->curr_style;
}

bool buffer_is_dirty(Buffer *buf)
{
	return buf->is_dirty;
}

void buffer_dirty_set(Buffer *buf, bool dirty)
{
	buf->is_dirty = dirty;
}

char *buffer_mode_get(Buffer *buf)
{
	return buf->mode;
}

void buffer_mode_set(Buffer *buf, char *name)
{
	strncpy(buf->mode, name, sizeof(buf->mode));
}
