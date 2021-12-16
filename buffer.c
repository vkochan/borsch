#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "view.h"
#include "keymap.h"
#include "text/text.h"

typedef struct Buffer {
	int buf_id;
	struct Buffer *next;
	struct Buffer *prev;
	KeyMap *keymap;
	size_t cursor;
	Text *text;
	bool is_name_locked;
	char name[256];
	size_t ref_count;
	bool is_dirty;
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

	buf->keymap = keymap_new(NULL);
	if (!buf->keymap) {
		text_free(buf->text);
		free(buf);
		return NULL;
	}

	buf_count++;
	buf->buf_id = buffer_id_gen();

	if (name && strlen(name)) {
		strncpy(buf->name, name, sizeof(buf->name));
		buf->is_name_locked = true;
	} else {
		snprintf(buf->name, sizeof(buf->name), "new%d", buf->buf_id);
	}

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
		text_free(buf->text);
		free(buf);
	}
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

void buffer_keymap_set(Buffer *buf, KeyMap *kmap)
{
	if (kmap)
		keymap_ref_get(kmap);

	if (kmap != buf->keymap && buf->keymap)
		keymap_ref_put(buf->keymap);

	buf->keymap = kmap;
}

KeyMap *buffer_keymap_get(Buffer *buf)
{
	return buf->keymap;
}

size_t buffer_text_insert(Buffer *buf, size_t pos, const char *text)
{
	size_t len;

	len = strlen(text);

	if (text_insert(buf->text, pos, text, len)) {
		buf->is_dirty = true;
		buf->cursor = pos + len;
		pos += len;
	} else {
		pos = EPOS;
	}

	return pos;
}

size_t buffer_text_len_insert(Buffer *buf, size_t pos, const char *text, size_t len)
{
	if (text_insert(buf->text, pos, text, len)) {
		buf->is_dirty = true;
		buf->cursor = pos + len;
		pos += len;
	} else {
		pos = EPOS;
	}

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

bool buffer_is_dirty(Buffer *buf)
{
	return buf->is_dirty;
}

void buffer_dirty_set(Buffer *buf, bool dirty)
{
	buf->is_dirty = dirty;
}
