#ifndef EVENT_H
#define EVENT_H

#include <stdbool.h>

void event_update_timeout_cb_set(double (*cb)(double timeout, void *arg));
void event_pre_handler_cb_set(bool (*cb)(void *arg));
int event_fd_handler_register(int fd, void (*fn)(int fd, void *arg), void *arg);
void event_fd_handler_unregister(int fd);
int event_process(void *);

#endif /* EVENT_H */
