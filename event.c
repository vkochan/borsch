#include <errno.h>
#include <signal.h>
#include <stdlib.h>
#include <sys/param.h>
#include <sys/select.h>

#include <stdio.h>

typedef struct event_fd {
	struct event_fd *prev, *next;
	void (*fn)(int fd, void *arg);
	void *arg;
	int fd;
} event_fd_t;

static event_fd_t event_fd_list;

static void event_fd_list_insert(event_fd_t *list, event_fd_t *evt)
{
	evt->next = list->next;
	evt->prev = list;

	if (list->next)
		list->next->prev = evt;

	list->next = evt;
}

static void event_fd_list_delete(event_fd_t *evt)
{
	evt->prev->next = evt->next;

	if (evt->next)
		evt->next->prev = evt->prev;
}

int event_fd_handler_register(int fd, void (*fn)(int fd, void *arg), void *arg)
{
	event_fd_t *evt;

	evt = malloc(sizeof(*evt));
	if (!evt)
		return -1;

	evt->arg = arg;
	evt->fn = fn;
	evt->fd = fd;

	event_fd_list_insert(&event_fd_list, evt);

	return 0;
}

void event_fd_handler_unregister(int fd)
{
	event_fd_t *evt = event_fd_list.next;;
	int i;

	while (evt) {
		if (evt->fd == fd) {
			event_fd_list_delete(evt);
			free(evt);
			return;
		}
		evt = evt->next;
	}
}

int event_process(struct timespec *tv)
{
	sigset_t emptyset;
	event_fd_t *evt;
	int r, nfds = 0;
	fd_set rd;

	FD_ZERO(&rd);

	sigemptyset(&emptyset);

	for (evt = event_fd_list.next; evt; evt = evt->next) {
		FD_SET(evt->fd, &rd);
		nfds = MAX(nfds, evt->fd);
	}

	r = pselect(nfds + 1, &rd, NULL, NULL, tv, &emptyset);
	if (r < 0) {
		if (errno == EINTR)
			return 0;
	}

	evt = event_fd_list.next;
	while (evt) {
		event_fd_t *next = evt->next;

		if (FD_ISSET(evt->fd, &rd) && evt->fn) {
			evt->fn(evt->fd, evt->arg);
		}
		evt = next;
	}

	return r;
}
