#ifndef BUFFER_H
#define BUFFER_H

#include <text/text.h>

typedef struct Buffer Buffer;

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
bool buffer_is_name_locked(Buffer *buf);
Buffer *buffer_by_name(const char *name);

#endif /* BUFFER_H */
