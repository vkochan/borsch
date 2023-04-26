#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <libgen.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/param.h>

#include "api.h"
#include "process.h"
#include "vt.h"
#include "view.h"
#include "keymap.h"
#include "buffer.h"
#include "syntax.h"
#include "text/text.h"
#include "text/text-regex.h"
#include "text/text-motions.h"

typedef struct TextProperty {
	struct TextProperty *prev;
	struct TextProperty *next;
	size_t start;
	size_t len;
	void *data;
	int prio;
	int type;
	void (*action)(Buffer *buf, struct TextProperty *prop, size_t start, size_t end, void *arg, buffer_property_cb_t cb);
	void (*free)(void *data);
	char *regex_pattern;
	Regex *regex;
	char *name;
} TextProperty;

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
	char state[32];
	char mode[32];
	size_t ref_count;
	bool is_input_enabled;
	bool is_read_only;
	bool is_dirty;
	File file;
	Process *proc;
	size_t mark;
	bool is_mark_set;
	TextProperty *min_prop;
	TextProperty *max_prop;
	TextProperty props;
	SyntaxParser *parser;
	void *env;
} Buffer;

static Buffer buf_list;
static int buf_count;

void buffer_cursor_set(Buffer *buf, size_t pos);

static const char *buffer_parser_chunk_read(void *payload, uint32_t byte_index, uint32_t *bytes_read)
{
	Buffer *buf = payload;
	const Text *txt = buf->text;
	size_t pos = byte_index;
	Iterator it;

	it = text_iterator_get(txt, pos);
	if (!text_iterator_valid(&it)) {
		*bytes_read = 0;
		return NULL;
	}

	*bytes_read = it.end - it.text;
	return it.text;
}

static size_t buffer_parser_text_read(void *payload, size_t pos, size_t len, char *buf)
{
	Buffer *_buf = payload;

	return text_bytes_get(_buf->text, pos, len, buf);
}

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

	buf->keymap = keymap_new(NULL);
	if (!buf->keymap) {
		free(buf);
		return NULL;
	}

	buf->text = text_load(NULL);
	if (!buf->text) {
		keymap_free(buf->keymap);
		free(buf);
		return NULL;
	}
	buf->ref_count = 0;
	buf->mark = EPOS;

	buf_count++;
	buf->buf_id = buffer_id_gen();

	if (name && strlen(name)) {
		strncpy(buf->name, name, sizeof(buf->name));
		buf->is_name_locked = true;
	} else {
		snprintf(buf->name, sizeof(buf->name), "new%d", buf->buf_id);
	}

	buf->curr_style = UI_TEXT_STYLE_NORMAL;
	buf->curr_fg = UI_TEXT_COLOR_DEFAULT;
	buf->curr_bg = UI_TEXT_COLOR_DEFAULT;

	buffer_list_add(&buf_list, buf);

	return buf;
}

bool buffer_del(Buffer *buf)
{
	buffer_property_remove(buf, PROPERTY_TYPE_ALL, EPOS, EPOS, NULL, NULL);
	if (buffer_proc_get(buf)) {
		event_t evt = {};
		evt.eid = EVT_PROC_EXIT;
		evt.oid = process_pid_get(buffer_proc_get(buf));
		scheme_event_handle(evt);
	}
	scheme_env_free(buffer_env_get(buf));
	buffer_list_del(buf);
	/* TODO: check if buffer is not saved and ask user to save it */
	syntax_parser_delete(buf->parser);
	keymap_free(buf->keymap);
	text_free(buf->text);
	free(buf->file.path);
	free(buf);

	return true;
}

void buffer_readonly_set(Buffer *buf, bool is_readonly)
{
	buf->is_read_only = is_readonly;
}

bool buffer_is_readonly(Buffer *buf)
{
	return buf->is_read_only;
}

int buffer_file_open(Buffer *buf, const char *file)
{
	bool exist = false;
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
		exist = true;
	}

	/* close opened file */
	if (buf->file.path) {
		/* TODO show error message */
		if (text_modified(buf->text))
			text_save(buf->text, buf->file.path);
		free(buf->file.path);
		text_free(buf->text);
	} else if (!exist) {
		text_free(buf->text);
		text = text_load(NULL);
		if (!text)
			return -1;
	}

	strncpy(tmp, file, sizeof(tmp));
	fname = basename(tmp);
	strncpy(buf->name, fname, sizeof(buf->name));

	buf->file.path = strdup(file);
	buffer_cursor_set(buf, 0);
	buf->text = text;

	return 0;
}

char *buffer_filename_get(Buffer *buf)
{
	return buf->file.path;
}

void buffer_filename_set(Buffer *buf, const char *name)
{
	free(buf->file.path);
	buf->file.path = strdup(name);
	strncpy(buf->name, name, sizeof(buf->name));
}

bool buffer_save(Buffer *buf)
{
	if (buf->is_read_only)
		return false;
	if (buf->file.path)
		return text_save(buf->text, buf->file.path);
	return false;
}

bool buffer_is_modified(Buffer *buf)
{
	if (buf->file.path)
		return text_modified(buf->text);
	return false;
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
	if (!buf->is_mark_set)
		buf->mark = pos;
	if (pos > text_end(buf->text, 0))
		pos = text_end(buf->text, 0);
	buf->is_dirty = true;
	buf->cursor = pos;
}

size_t buffer_cursor_get(Buffer *buf)
{
	return buf->cursor;
}

size_t buffer_line_num(Buffer *buf, size_t pos)
{
	return text_lineno_by_pos(buf->text, pos);
}

char *buffer_name_get(Buffer *buf)
{
	if (buf->file.path)
		return buf->file.path;
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
		if (strcmp(buffer_name_get(buf), name) == 0)
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
	if (!buf->ref_count)
		buffer_del(buf);
}

int buffer_ref_count(Buffer *buf)
{
	return buf->ref_count;
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

static void buffer_properties_pos_update(Buffer *buf, size_t pos, int len);

static void buffer_update_parser(Buffer *buf, size_t pos, int len)
{
	if (buf->parser)
		syntax_parser_edit(buf->parser, pos, len);
}

static void buffer_text_changed(Buffer *buf, size_t pos, int len)
{
	buffer_properties_pos_update(buf, pos, len);
	buffer_update_parser(buf, pos, len);
}

size_t buffer_text_insert(Buffer *buf, size_t pos, const char *text)
{
	size_t len;

	if (buf->is_read_only)
		return buf->cursor;

	len = strlen(text);

	if (text_insert(buf->text, pos, text, len)) {
		buffer_text_changed(buf, pos, len);
		buffer_cursor_set(buf, pos + len);
		buf->is_dirty = true;
		pos += len;
	} else {
		pos = EPOS;
	}

	return pos;
}

size_t buffer_text_insert_len(Buffer *buf, size_t pos, const char *text, size_t len)
{
	if (buf->is_read_only)
		return buf->cursor;

	if (text_insert(buf->text, pos, text, len)) {
		buffer_text_changed(buf, pos, len);
		buffer_cursor_set(buf, pos + len);
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
	size_t pos_orig = pos;
	size_t indent_len = 0;
	char *indent = NULL;
	size_t len = 1;
	char byte;

	if (buf->is_read_only)
		return buf->cursor;

	/* insert second newline at end of file, except if there is already one */
	bool eof = pos == text_size(txt);
	bool nl2 = eof && !(pos > 0 && text_byte_get(txt, pos-1, &byte) && byte == '\n');

	if (true) {
		/* copy leading white space of current line */
		size_t begin = text_line_begin(txt, pos);
		size_t start = text_line_start(txt, begin);
		size_t end = text_line_end(txt, start);
		if (start > pos)
			start = pos;
		indent_len = start >= begin ? start-begin : 0;
		if (start == end) {
			pos = begin;
		} else {
			indent = malloc(indent_len+1);
			if (indent)
				indent_len = text_bytes_get(txt, begin, indent_len, indent);
		}
	}

	text_insert(txt, pos, "\n", 1);
	if (eof) {
		if (nl2) {
			text_insert(txt, text_size(txt), "\n", 1);
			len++;
		} else {
			pos--; /* place cursor before, not after nl */
		}
	}
	pos++;

	if (indent)
		text_insert(txt, pos, indent, indent_len);
	free(indent);

	pos += indent_len;
	len += indent_len;

	buffer_text_changed(buf, pos_orig, len);
	buffer_cursor_set(buf, pos);
	buf->is_dirty = true;

	return pos;
}

size_t buffer_text_delete(Buffer *buf, size_t start, size_t end)
{
	size_t tmp;
	Text *txt;

	if (buf->is_read_only)
		return buf->cursor;

	if (start > end) {
		tmp = end;
		end = start;
		start = tmp;
	}

	if (text_delete(buf->text, start, end - start)) {
		buffer_cursor_set(buf, start);
		buf->is_dirty = true;
	}

	buffer_text_changed(buf, start, -(end - start));
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

char *buffer_mode_name_get(Buffer *buf)
{
	return buf->mode;
}

void buffer_mode_name_set(Buffer *buf, char *name)
{
	strncpy(buf->mode, name, sizeof(buf->mode));
}

char *buffer_state_name_get(Buffer *buf)
{
	return buf->state;
}

void buffer_state_name_set(Buffer *buf, char *name)
{
	strncpy(buf->state, name, sizeof(buf->state));
}

bool buffer_is_mark_set(Buffer *buf)
{
	return buf->is_mark_set;
}

void buffer_mark_set(Buffer *buf, size_t pos)
{
	buf->is_mark_set = true;
	buf->mark = pos;
}

void buffer_mark_clear(Buffer *buf)
{
	buf->is_mark_set = false;
	buf->mark = EPOS;
}

size_t buffer_mark_get(Buffer *buf)
{
	return buf->mark;
}

void buffer_proc_set(Buffer *buf, Process *proc)
{
	buf->proc = proc;
}

Process *buffer_proc_get(Buffer *buf)
{
	return buf->proc;
}

static void text_property_insert_after(TextProperty *head, TextProperty *prop)
{
	prop->prev = head;
	prop->next = head->next;

	if (head->next)
		head->next->prev = prop;

	head->next = prop;
}

static void text_property_remove(TextProperty *prop)
{
	if (prop->prev)
		prop->prev->next = prop->next;
	if (prop->next)
		prop->next->prev = prop->prev;
}

static void buffer_property_regex_action(Buffer *buf, TextProperty *prop, size_t start, size_t end, void *arg, buffer_property_cb_t cb)
{
	bool found;
	char c;

	if (start > end)
		return;

	do {
		int flags = text_byte_get(buf->text, start+1, &c) && c == '\n' ? 0 : REG_NOTBOL;
		RegexMatch match[1] = {0};

		found = !text_search_range_forward(buf->text, start, end - start, prop->regex, 1, match, flags);
		if (found)
			cb(buf, prop->type, match[0].start, match[0].end-1, prop->data, arg);

		start = match[0].end;
	} while (found);
}

int buffer_property_add(Buffer *buf, int type, size_t start, size_t end, void *data, const char *pattern,
			char *name, void (*free_fn)(void *))
{
	Regex *regex = NULL;
	TextProperty *p;

	if (pattern) {
		int cflags = REG_EXTENDED|REG_NEWLINE;
		regex = text_regex_new();
		if (!regex)
			return -1;
		if (text_regex_compile(regex, pattern, cflags) != 0) {
			text_regex_free(regex);
			return -1;
		}
	}


	p = calloc(1, sizeof(*p));
	if (!p)
		return -1;

	if (regex) {
		p->action = buffer_property_regex_action;
		p->regex_pattern = strdup(pattern);
		p->regex = regex;
	}

	p->free = free_fn;
	p->start = start;
	p->data = data;
	p->len = end - start;
	p->type = type;

	if (name)
		p->name = strdup(name);

	if (buf->max_prop && start > buf->max_prop->start) {
		text_property_insert_after(buf->max_prop, p);
		buf->max_prop = p;
	} else if (buf->min_prop && start < buf->min_prop->start) {
		text_property_insert_after(&buf->props, p);
		buf->min_prop = p;
	} else {
		TextProperty *max_prop = buf->props.next;

		while (max_prop && start > max_prop->start) {
			if (!max_prop->next)
				break;
			max_prop = max_prop->next;
		}

		if (!max_prop)
			text_property_insert_after(&buf->props, p);
		else
			text_property_insert_after(max_prop, p);

		buf->max_prop = p;
		if (!buf->min_prop)
			buf->min_prop = buf->max_prop;
	}

	buf->is_dirty = true;
	return 0;
}

static void __buffer_property_remove(Buffer *buf, TextProperty *it)
{
	if (it->free)
		it->free(it->data);
	else
		free(it->data);

	if (it == buf->min_prop)
		buf->min_prop = it->next;
	if (it == buf->max_prop)
		buf->max_prop = it->prev;

	text_regex_free(it->regex);
	free(it->regex_pattern);
	free(it->name);
	text_property_remove(it);
	free(it);
}

static bool buffer_property_remove_range(Buffer *buf, size_t type, size_t start, size_t end, const char *pattern, char *name)
{
	TextProperty *it = buf->props.next;
	size_t rem_count = 0;
	int exp = 0;

	if (start != EPOS && end != EPOS)
		exp++;
	if (type)
		exp++;
	if (pattern)
		exp++;
	if (name)
		exp++;

	if (!exp)
		return false;

	while (it) {
		TextProperty *next = it->next;
		int match = 0;

		if (it->start >= start && it->start + it->len <= end)
			match++;
		if (type == it->type)
			match++;
		if (type == PROPERTY_TYPE_ALL)
			match++;
		if (pattern && strcmp(pattern, it->regex_pattern) == 0)
			match++;
		if (name && it->name && strcmp(name, it->name) == 0)
			match++;

		if (match >= exp) {
			__buffer_property_remove(buf, it);
			rem_count++;
		}
		it = next;
	}

	return rem_count > 0;
}

bool buffer_property_remove(Buffer *buf, size_t type, size_t start, size_t end, const char *pattern, char *name)
{
	if (buffer_property_remove_range(buf, type, start, end, pattern, name)) {
		buf->is_dirty = true;
		return true;
	}

	return false;
}

void buffer_properties_walk(Buffer *buf, int type, size_t start, size_t end, char *name, void *arg, buffer_property_cb_t cb)
{
	TextProperty *it = buf->props.next;
	int exp = 0;

	if (start != EPOS && end != EPOS) {
		/* TODO: this does not work */
		/* if (buf->min_prop && end < buf->min_prop->start) */
		/* 	return; */
		/* if (buf->max_prop && start > buf->max_prop->end) */
		/* 	return; */
		exp++;
	}
	if (type)
		exp++;
	if (name)
		exp++;

	for (; it; it = it->next) {
		int match = 0;

		if (it->regex && it->type == type) {
			it->action(buf, it, start, end, arg, cb);
			continue;
		}

		if (it->start + it->len >= start && (it->start >= start && it->start <= end ||
				start >= it->start && it->start <= end))
			match++;
		if (it->type == type)
			match++;
		if (type == PROPERTY_TYPE_ALL)
			match++;
		if (name && it->name && strcmp(name, it->name) == 0)
			match++;

		if (match >= exp)
			cb(buf, it->type, it->start, it->start + it->len, it->data, arg);
	}
}

static void buffer_properties_pos_update(Buffer *buf, size_t pos, int len)
{
	TextProperty *it = buf->props.next;

	while (it) {
		size_t pos_end = pos + abs(len);

		if (it->start >= pos_end) {
			it->start += len;
		} else if (pos >= it->start && pos < it->start + it->len && ((pos_end >= it->start + it->len) || (pos_end <= it->start + it->len))) {
			if (len < 0)
				len = -MIN(abs(len), it->len);
			it->len += len;

			if (it->len == 0) {
				TextProperty *next = it->next;

				__buffer_property_remove(buf, it);
				it = next;
				continue;
			}
		}
		it = it->next;
	}
}

void buffer_env_set(Buffer *buf, void *env)
{
	buf->env = env;
}

void *buffer_env_get(Buffer *buf)
{
	return buf->env;
}

void buffer_snapshot(Buffer *buf)
{
	text_snapshot(buf->text);
}

void buffer_undo(Buffer *buf)
{
	if (buf->is_read_only)
		return;

	buffer_cursor_set(buf, text_undo(buf->text));
	buf->is_dirty = true;
	buffer_update_parser(buf, 0, text_size(buf->text));
}

void buffer_redo(Buffer *buf)
{
	if (buf->is_read_only)
		return;

	buffer_cursor_set(buf, text_redo(buf->text));
	buf->is_dirty = true;
	buffer_update_parser(buf, 0, text_size(buf->text));
}

size_t buffer_search_regex(Buffer *buf, size_t pos, const char *pattern, int dir)
{
	int cflags = REG_EXTENDED|REG_NEWLINE;
	Regex *regex = text_regex_new();
	if (!regex)
		return EPOS;
	if (text_regex_compile(regex, pattern, cflags) != 0) {
		text_regex_free(regex);
		return EPOS;
	}
	if (dir > 0)
		pos = text_search_forward(buf->text, pos, regex);
	else
		pos = text_search_backward(buf->text, pos, regex);
	text_regex_free(regex);
	return pos;
}

int buffer_parser_set(Buffer *buf, const char *lang)
{
	SyntaxInput input = {
		.chunk_read = buffer_parser_chunk_read,
		.text_read = buffer_parser_text_read,
		.payload = buf,
	};

	if (!lang || !strlen(lang)) {
		syntax_parser_delete(buf->parser);
		buf->parser = NULL;
		return 0;
	}

	buf->parser = syntax_parser_new(lang);
	if (!buf->parser)
		return -1;

	syntax_parser_input_set(buf->parser, &input);

	return syntax_parser_parse(buf->parser);
}

void buffer_syntax_rules_walk(Buffer *buf, syntax_rule_type_t type, size_t start, size_t end, void *arg,
			      int (*cb) (SyntaxParser *parser, int type, size_t start, size_t end, void *data, void *arg))
{
	if (buf->parser) {
		syntax_parser_rules_walk(buf->parser, type, start, end, arg, cb);
	}
}
