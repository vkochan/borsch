#ifndef BUFFER_H
#define BUFFER_H

#include <text/text.h>
#include <ui/style.h>

typedef struct SyntaxParser SyntaxParser;
typedef struct Buffer Buffer;
typedef struct KeyMap KeyMap;
typedef struct Process Process;

typedef enum {
	PROPERTY_TYPE_TEXT_STYLE	= 1,
	PROPERTY_TYPE_TEXT_HIGHLIGHT	= 2,
	PROPERTY_TYPE_TEXT_KEYMAP	= 3,

	PROPERTY_TYPE_ALL		= 10000,
} buffer_property_t;

typedef enum {
	SYNTAX_RULE_TYPE_STYLE = 1,
} syntax_rule_type_t;

Buffer *buffer_new(const char *name);
bool buffer_del(Buffer *buf);
int buffer_file_open(Buffer *buf, const char *file);
void buffer_readonly_set(Buffer *buf, bool is_readonly);
bool buffer_is_readonly(Buffer *buf);
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
void buffer_name_lock(Buffer *buf, bool lock);
bool buffer_name_is_locked(Buffer *buf);
Buffer *buffer_by_name(const char *name);
void buffer_ref_get(Buffer *buf);
void buffer_ref_put(Buffer *buf);
int buffer_ref_count(Buffer *buf);
void buffer_keymap_set(Buffer *buf, char *name);
KeyMap *buffer_keymap_get(Buffer *buf);
size_t buffer_text_insert(Buffer *buf, size_t pos, const char *text);
size_t buffer_text_insert_len(Buffer *buf, size_t pos, const char *text, size_t len);
size_t buffer_text_insert_nl(Buffer *buf, size_t pos);
size_t buffer_text_delete(Buffer *buf, size_t start, size_t end);
void buffer_text_fg_set(Buffer *buf, short fg);
short buffer_text_fg_get(Buffer *buf);
void buffer_text_bg_set(Buffer *buf, short bg);
short  buffer_text_bg_get(Buffer *buf);
void buffer_text_style_set(Buffer *buf, ui_text_style_t style);
ui_text_style_t buffer_text_style_get(Buffer *buf);
char *buffer_text_extract(Buffer *buf, size_t pos, size_t len);
void buffer_text_input_enable(Buffer *buf, bool enable);
bool buffer_text_input_is_enabled(Buffer *buf);
bool buffer_is_dirty(Buffer *buf);
void buffer_dirty_set(Buffer *buf, bool dirty);
char *buffer_mode_name_get(Buffer *buf);
void buffer_mode_name_set(Buffer *buf, char *name);
char *buffer_state_name_get(Buffer *buf);
void buffer_state_name_set(Buffer *buf, char *name);
void buffer_mark_set(Buffer *buf, size_t pos);
bool buffer_is_mark_set(Buffer *buf);
void buffer_mark_clear(Buffer *buf);
size_t buffer_mark_get(Buffer *buf);
void buffer_proc_set(Buffer *buf, Process *proc);
Process *buffer_proc_get(Buffer *buf);
int buffer_property_add(Buffer *buf, int type, size_t start, size_t end, void *data);
void buffer_properties_walk(Buffer *buf, int type, size_t start, size_t end, void *arg,
		int (*cb) (Buffer *buf, int type, size_t start, size_t end, void *data, void *arg));
bool buffer_property_remove(Buffer *buf, size_t type, size_t start, size_t end);
bool buffer_property_remove_cb(Buffer *buf, size_t type, size_t start, size_t end, void *arg,
		void (*cb)(Buffer *buf, size_t type, size_t start, size_t end,
			void *data, void *arg));
void buffer_env_set(Buffer *buf, void *env);
void *buffer_env_get(Buffer *buf);

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
