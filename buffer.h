#ifndef BUFFER_H
#define BUFFER_H

#include <text/text.h>
#include <ui/style.h>

typedef struct SyntaxParser SyntaxParser;
typedef struct Buffer Buffer;
typedef struct KeyMap KeyMap;
typedef struct Process Process;

typedef void (*buffer_property_cb_t) (Buffer *buf, int type, size_t start, size_t end, void *data, void *arg);

typedef enum {
	PROPERTY_TYPE_TEXT_STYLE	= 1,
	PROPERTY_TYPE_TEXT_HIGHLIGHT	= 2,
	PROPERTY_TYPE_TEXT_KEYMAP	= 3,
	PROPERTY_TYPE_TEXT_SYMBOL	= 4,
	PROPERTY_TYPE_TEXT_DATA		= 5,

	PROPERTY_TYPE_ALL		= 10000,
} buffer_property_type_t;

typedef enum {
	SYNTAX_RULE_TYPE_STYLE = 1,
} syntax_rule_type_t;

Buffer *buffer_new(void);
bool buffer_del(Buffer *buf);
int buffer_file_open(Buffer *buf, const char *file);
char *buffer_filename_get(Buffer *buf);
void buffer_filename_set(Buffer *buf, const char *name);
bool buffer_save(Buffer *buf);
bool buffer_is_modified(Buffer *buf);
Buffer *buffer_first_get(void);
Buffer *buffer_next_get(Buffer *buf);
Text *buffer_text_get(Buffer *buf);
int buffer_id_get(Buffer *buf);
Buffer *buffer_by_id(int bid);
void buffer_cursor_set(Buffer *buf, size_t pos);
size_t buffer_cursor_get(Buffer *buf);
size_t buffer_line_num(Buffer *buf, size_t pos);
char *buffer_name_get(Buffer *buf);
void buffer_name_set(Buffer *buf, const char *name);
void buffer_keymap_set(Buffer *buf, char *name);
KeyMap *buffer_keymap_get(Buffer *buf);
size_t buffer_text_insert(Buffer *buf, size_t pos, const char *text);
size_t buffer_text_insert_len(Buffer *buf, size_t pos, const char *text, size_t len);
size_t buffer_text_insert_nl(Buffer *buf, size_t pos);
size_t buffer_text_delete(Buffer *buf, size_t start, size_t end);
char *buffer_text_extract(Buffer *buf, size_t pos, size_t len);
void buffer_proc_set(Buffer *buf, Process *proc);
Process *buffer_proc_get(Buffer *buf);
int buffer_property_add(Buffer *buf, int type, size_t start, size_t end, void *data, const char *pattern,
			char *name, void (*free_fn)(void *));
void buffer_properties_walk(Buffer *buf, int type, size_t start, size_t end, char *name, void *arg, buffer_property_cb_t cb);
bool buffer_property_remove(Buffer *buf, size_t type, size_t start, size_t end, const char *pattern, char *name);

void buffer_snapshot(Buffer *buf);
void buffer_undo(Buffer *buf);
void buffer_redo(Buffer *buf);

size_t buffer_search_regex(Buffer *buf, size_t pos, const char *pattern, int dir);

int buffer_parser_set(Buffer *buf, const char *lang);
int buffer_parser_parse(Buffer *buf);

int buffer_parser_rule_add(Buffer *buf, syntax_rule_type_t type, const char *match, void *data);

int buffer_parser_rule_remove(Buffer *buf, syntax_rule_type_t type, const char *match);

void buffer_syntax_rules_walk(Buffer *buf, syntax_rule_type_t type, size_t start, size_t end, void *arg,
			      int (*cb) (SyntaxParser *parser, int type, size_t start, size_t end, void *data, void *arg));

#endif /* BUFFER_H */
