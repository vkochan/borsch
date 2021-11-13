#include <string.h>
#include <stdlib.h>

#include "view.h"
#include "text/text.h"

typedef struct Buffer {
	int buf_id;
	struct Buffer *next;
	struct Buffer *prev;
	size_t cursor;
	Text *text;
	char name[256];
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

Buffer *buffer_new(void)
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

	buf->buf_id = buffer_id_gen();

	buffer_list_add(&buf_list, buf);

	return buf;
}

void buffer_del(Buffer *buf)
{
	buffer_list_del(buf);
	text_free(buf->text);
	free(buf);
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

/*
const char *buffer_name_get(Buffer *buf)
{
	return buf->name;
}

void buffer_name_set(Buffer *buf, const char *name)
{
	strncpy(buf->name, name, sizeof(buf->name));
}

Buffer *buffer_by_name(const char *name)
{
	return NULL;
}
*/
