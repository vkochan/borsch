#ifndef BUFFER_H
#define BUFFER_H

#include <text/text.h>
#include <ui/style.h>

typedef struct Buffer Buffer;
typedef struct KeyMap KeyMap;

Buffer *buffer_new(const char *name);
void buffer_del(Buffer *buf);
int buffer_file_open(Buffer *buf, const char *file);
bool buffer_save(Buffer *buf);
bool buffer_is_modified(Buffer *buf);
Buffer *buffer_first_get(void);
Buffer *buffer_next_get(Buffer *buf);
Text *buffer_text_get(Buffer *buf);
int buffer_id_get(Buffer *buf);
Buffer *buffer_by_id(int bid);
void buffer_cursor_set(Buffer *buf, size_t pos);
size_t buffer_cursor_get(Buffer *buf);
char *buffer_name_get(Buffer *buf);
void buffer_name_set(Buffer *buf, const char *name);
void buffer_name_lock(Buffer *buf, bool lock);
bool buffer_name_is_locked(Buffer *buf);
Buffer *buffer_by_name(const char *name);
void buffer_ref_get(Buffer *buf);
void buffer_ref_put(Buffer *buf);
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
char *buffer_mode_get(Buffer *buf);
void buffer_mode_set(Buffer *buf, char *name);
void buffer_mark_set(Buffer *buf, size_t pos);
void buffer_mark_clear(Buffer *buf);
size_t buffer_mark_get(Buffer *buf);

#endif /* BUFFER_H */
