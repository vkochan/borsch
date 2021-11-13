#ifndef BUFFER_H
#define BUFFER_H

#include <text/text.h>

typedef struct Buffer Buffer;

Buffer *buffer_new(void);
void buffer_del(Buffer *buf);
Text *buffer_text_get(Buffer *buf);
int buffer_id_get(Buffer *buf);
Buffer *buffer_by_id(int bid);
void buffer_cursor_set(Buffer *buf, size_t pos);
size_t buffer_cursor_get(Buffer *buf);

#endif /* BUFFER_H */
