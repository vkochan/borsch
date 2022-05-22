#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <libgen.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/param.h>

#include <tree_sitter/api.h>

#include "vt.h"
#include "view.h"
#include "keymap.h"
#include "buffer.h"
#include "text/text.h"
#include "text/text-regex.h"
#include "text/text-motions.h"

TSLanguage *tree_sitter_c();

typedef struct Buffer Buffer;

typedef struct TextProperty {
	struct TextProperty *prev;
	struct TextProperty *next;
	size_t start;
	size_t end;
	void *data;
	int prio;
	int type;
} TextProperty;

typedef struct {
	char *path;
} File;

typedef struct {
	syntax_rule_type_t type;
	const char *match;
	TSQuery *query;
	void *data;
} SyntaxRule;

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
	Vt *term;
	pid_t pid;
	volatile sig_atomic_t is_died;
	size_t mark;
	TextProperty *min_prop;
	TextProperty *max_prop;
	TextProperty props;
	void *env;
	/* Syntax parser */
	TSQueryCursor *syntax_cursor;
	uint32_t parser_n_changes;
	TSRange *parser_changes;
	TSTree *parser_tree;
	TSParser *parser;
	SyntaxRule *syntax_rules;
	int n_syntax_rules;
} Buffer;

static Buffer buf_list;
static int buf_count;

void buffer_cursor_set(Buffer *buf, size_t pos);

static const char *buffer_parser_read(void *payload, uint32_t byte_index, TSPoint position,
				      uint32_t *bytes_read)
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

static void buffer_parser_delete(Buffer *buf)
{
	ts_tree_delete(buf->parser_tree);
	ts_parser_delete(buf->parser);
	free(buf->parser_changes);
}

int buffer_parser_parse(Buffer *buf)
{
	TSInput input = {
		.encoding = TSInputEncodingUTF8,
		.read = buffer_parser_read,
		.payload = buf,
	};
	/* TODO: add support of tree edit */
	/* TSTree *old_tree = buf->parser_tree; */
	TSTree *old_tree = NULL;
	TSRange *new_changes = NULL;
	uint32_t n_changes;
	TSTree *new_tree;

	new_tree = ts_parser_parse(buf->parser, old_tree, input);
	if (!new_tree)
		return -1;

	if (old_tree)
		new_changes = ts_tree_get_changed_ranges(old_tree, new_tree, &n_changes);

	free(buf->parser_changes);
	ts_tree_delete(old_tree);

	buf->parser_n_changes = n_changes;
	buf->parser_changes = new_changes;
	buf->parser_tree = new_tree;

	return 0;
}

static void buffer_parser_edit(Buffer *buf, size_t pos, size_t len)
{
	/* TODO: implement editing */
	buffer_parser_parse(buf);
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

#if 0
	buf->parser = ts_parser_new();
	if (!buf->parser) {
		keymap_free(buf->keymap);
		free(buf);
		return NULL;
	}
#endif

	buf->text = text_load(NULL);
	if (!buf->text) {
		keymap_free(buf->keymap);
		free(buf);
		return NULL;
	}
	buf->ref_count = 1;
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
	if (buf->ref_count)
		buf->ref_count--;

	if (!buf->ref_count) {
		if (buf->term)
			vt_destroy(buf->term);
		buffer_list_del(buf);
		/* TODO: check if buffer is not saved and ask user to save it */
		if (buf->syntax_cursor)
			ts_query_cursor_delete(buf->syntax_cursor);
		for (int i = 0; i < buf->n_syntax_rules; i++) {
			free(buf->syntax_rules[i].data);
		}
		free(buf->syntax_rules);
		buffer_parser_delete(buf);
		keymap_free(buf->keymap);
		text_free(buf->text);
		free(buf->file.path);
		free(buf);

		return true;
	}

	return false;
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
	if (buf->mark == EPOS)
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

static void buffer_text_changed(Buffer *buf, size_t pos, int len)
{
	buffer_properties_pos_update(buf, pos, len);
	if (buf->parser)
		buffer_parser_edit(buf, pos, len);
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
	size_t len = 1;
	char byte;

	if (buf->is_read_only)
		return buf->cursor;

	/* insert second newline at end of file, except if there is already one */
	bool eof = pos == text_size(txt);
	bool nl2 = eof && !(pos > 0 && text_byte_get(txt, pos-1, &byte) && byte == '\n');

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

void buffer_mark_set(Buffer *buf, size_t pos)
{
	buf->mark = pos;
}

void buffer_mark_clear(Buffer *buf)
{
	buf->mark = EPOS;
}

size_t buffer_mark_get(Buffer *buf)
{
	return buf->mark;
}

void buffer_term_set(Buffer *buf, Vt *term)
{
	buf->term = term;
}

Vt *buffer_term_get(Buffer *buf)
{
	return buf->term;
}

void buffer_pid_set(Buffer *buf, pid_t pid)
{
	buf->pid = pid;
}

pid_t buffer_pid_get(Buffer *buf)
{
	return buf->pid;
}

Buffer *buffer_by_pid(pid_t pid)
{
	Buffer *buf;

	for (buf = buf_list.next; buf; buf = buf->next) {
		if (buf->pid == pid)
			return buf;
	}

	return NULL;
}

void buffer_died_set(Buffer *buf, bool died)
{
	buf->is_died = died;
}

bool buffer_is_died(Buffer *buf)
{
	return buf->is_died;
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

int buffer_property_add(Buffer *buf, int type, size_t start, size_t end, void *data)
{
	TextProperty *p;

	p = calloc(1, sizeof(*p));
	if (!p)
		return -1;

	p->start = start;
	p->data = data;
	p->end = end;
	p->type = type;

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

bool buffer_property_remove_cb(Buffer *buf, size_t type, size_t start, size_t end, void *arg,
		void (*cb)(Buffer *buf, size_t type, size_t start, size_t end,
			void *data, void *arg))
{
	TextProperty *it = buf->props.next;
	size_t rem_count = 0;
	int exp = 0;

	if (start != EPOS && end != EPOS)
		exp++;
	if (type)
		exp++;

	if (!exp)
		return false;

	while (it) {
		TextProperty *next = it->next;
		int match = 0;

		if (it->start >= start && it->end <= end)
			match++;
		if (type == it->type)
			match++;
		if (type == PROPERTY_TYPE_ALL)
			match++;

		if (match >= exp) {
			if (cb)
				cb(buf, it->type, it->start, it->end, it->data, arg);
			else
				free(it->data);

			if (it == buf->min_prop)
				buf->min_prop = it->next;
			if (it == buf->max_prop)
				buf->max_prop = it->prev;

			text_property_remove(it);
			rem_count++;
		}
		it = next;
	}

	return rem_count > 0;
}

bool buffer_property_remove(Buffer *buf, size_t type, size_t start, size_t end)
{
	if (buffer_property_remove_cb(buf, type, start, end, NULL, NULL)) {
		buf->is_dirty = true;
		return true;
	}

	return false;
}

void buffer_properties_walk(Buffer *buf, int type, size_t start, size_t end, void *arg,
		int (*cb) (Buffer *buf, int type, size_t start, size_t end, void *data, void *arg))
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

	for (; it; it = it->next) {
		int match = 0;

		if (it->end >= start && (it->start >= start && it->start <= end ||
				start >= it->start && it->start <= end))
			match++;
		if (it->type == type)
			match++;
		if (type == PROPERTY_TYPE_ALL)
			match++;

		if (match >= exp)
			cb(buf, it->type, it->start, MIN(it->end, end), it->data, arg);
	}
}

static void buffer_properties_pos_update(Buffer *buf, size_t pos, int len)
{
	TextProperty *it = buf->props.next;

	for (; it; it = it->next) {
		if (it->start >= pos) {
			it->start += len;
			it->end += len;
		}
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

	text_undo(buf->text);
	buf->is_dirty = true;
}

void buffer_redo(Buffer *buf)
{
	if (buf->is_read_only)
		return;

	text_redo(buf->text);
	buf->is_dirty = true;
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
	bool is_set = false;
	TSParser *parser;

	if (!lang || !strlen(lang)) {
		buffer_parser_delete(buf);
		return 0;
	}

	parser = ts_parser_new();
	if (!parser)
		return -1;

	if (strcmp("C", lang) == 0) {
		is_set = ts_parser_set_language(parser, tree_sitter_c());
	}

	if (is_set) {
		buf->syntax_cursor = ts_query_cursor_new();
		if (!buf->syntax_cursor)
			return -1;

		buf->parser = parser;
		return buffer_parser_parse(buf);
	}

	ts_parser_delete(parser);
	return -1;
}

int buffer_parser_rule_add(Buffer *buf, syntax_rule_type_t type, const char *match, void *data)
{
	TSQueryError error_type;
	uint32_t error_offset;
	SyntaxRule *rule;
	TSQuery *query;

	query = ts_query_new(ts_parser_language(buf->parser), match, strlen(match),
  			     &error_offset, &error_type);
	if (!query) {
		char *errmsg = "unknown";

		switch (error_type) {
		case TSQueryErrorSyntax: errmsg = "syntax"; break;
		case TSQueryErrorNodeType: errmsg = "node type"; break;
		case TSQueryErrorField: errmsg = "field"; break;
		case TSQueryErrorCapture: errmsg = "capture"; break;
		case TSQueryErrorStructure: errmsg = "structure"; break;
		case TSQueryErrorLanguage: errmsg = "language"; break;
		}

		fprintf(stderr, "Invalid syntax query [%s]: (%s)\n", match, errmsg);
		return -2;
	}

	buf->n_syntax_rules++;
	buf->syntax_rules = realloc(buf->syntax_rules, sizeof(*rule) * buf->n_syntax_rules);
	if (!buf->syntax_rules) {
		ts_query_delete(query);
		return -1;
	}

	rule = &buf->syntax_rules[buf->n_syntax_rules-1];
	memset(rule, 0, sizeof(*rule));
	rule->match = match;
	rule->query = query;
	rule->type = type;
	rule->data = data;

	return 0;
}

int buffer_parser_rule_remove(Buffer *buf, syntax_rule_type_t type, const char *match)
{
	SyntaxRule *rule;
	int i;

	for (i = 0; i < buf->n_syntax_rules; i++) {
		rule = &buf->syntax_rules[i];

		if (rule->type == type && strcmp(rule->match, match) == 0) {
			ts_query_delete(rule->query);
			/* TODO: pass a data destructor */
			free(rule->data);
			*rule = buf->syntax_rules[buf->n_syntax_rules-1];
			buf->n_syntax_rules--;
			buf->syntax_rules = realloc(buf->syntax_rules, sizeof(*rule) * buf->n_syntax_rules);
			if (!buf->syntax_rules)
				return -1;
			return 0;
		}
	}

	return 0;
}

void buffer_parser_rules_walk(Buffer *buf, syntax_rule_type_t type, size_t start, size_t end, void *arg,
			      int (*cb) (Buffer *buf, int type, size_t start, size_t end, void *data, void *arg))
{
	TSQueryCursor *syntax_cursor;;
	TSNode root_node;
	int i;

	if (!buf->parser)
		return;

	root_node = ts_tree_root_node(buf->parser_tree);
	syntax_cursor = buf->syntax_cursor;

	ts_query_cursor_set_byte_range(syntax_cursor, start, end);

	for (i = 0; i < buf->n_syntax_rules; i++) {
		SyntaxRule *rule = &buf->syntax_rules[i];

		if (rule->type == type) {
			TSQueryMatch match;

			ts_query_cursor_exec(syntax_cursor, rule->query, root_node);

			while (ts_query_cursor_next_match(syntax_cursor, &match)) {
				int cap_count = match.capture_count;
				int c;

				for (c = 0; c < cap_count; c++) {
					const TSQueryCapture *cap = &match.captures[c];
					uint32_t node_start = ts_node_start_byte(cap->node);
					uint32_t node_end = ts_node_end_byte(cap->node);

					cb(buf, type, node_start, node_end, rule->data, arg);
				}
			}
		}
	}
}
