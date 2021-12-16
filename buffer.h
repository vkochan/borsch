#ifndef BUFFER_H
#define BUFFER_H

#include <text/text.h>

typedef struct Buffer Buffer;
typedef struct KeyMap KeyMap;

Buffer *buffer_new(const char *name);
void buffer_del(Buffer *buf);
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
void buffer_keymap_set(Buffer *buf, KeyMap *kmap);
KeyMap *buffer_keymap_get(Buffer *buf);
size_t buffer_text_insert(Buffer *buf, size_t pos, const char *text);
size_t buffer_text_len_insert(Buffer *buf, size_t pos, const char *text, size_t len);
size_t buffer_text_delete(Buffer *buf, size_t start, size_t end);
bool buffer_is_dirty(Buffer *buf);
void buffer_dirty_set(Buffer *buf, bool dirty);

#endif /* BUFFER_H */
