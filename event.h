#ifndef EVENT_H
#define EVENT_H

int event_fd_handler_register(int fd, void (*fn)(int fd, void *arg), void *arg);
void event_fd_handler_unregister(int fd);
int event_process(struct timespec *tv);

#endif /* EVENT_H */
